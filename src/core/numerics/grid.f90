module core_grid_mod
   use core_constants_mod, only: SP, N_GHOST, PI, R_EARTH, MPI_SP
   use core_comm_mod,      only: type_comm
   use core_crs_mod,       only: type_crs, CRS_GEOGRAPHIC
   use mpi_f08
   implicit none

   type, public :: type_loop_bounds
      integer :: ib, ie        ! interior x bounds in ghost-inclusive indexing (ib = N_GHOST+1)
      integer :: jb, je        ! interior y bounds
      integer :: mloc, nloc    ! ghost-inclusive local array dims
      integer :: kb  = 0       ! σ-layer start (ke < kb = 2D sentinel)
      integer :: ke  = -1      ! σ-layer end
      integer :: kloc = 0      ! ghost-inclusive σ-layer dim (0 = 2D)
      integer :: ti  = 32      ! tile size x (CPU OMP cache blocking)
      integer :: tj  = 32      ! tile size y
   end type type_loop_bounds

   type, public :: type_grid_2d
      integer :: M, N
      ! Domain decomposition
      integer :: nx_proc = 1, ny_proc = 1
      integer :: iproc = 0, jproc = 0
      ! Interior (compute) domain indices — no ghost cells
      integer :: ibegin, istop, jbegin, jstop
      integer :: local_nx, local_ny
      ! Full domain indices — includes ghost cells
      integer :: ig_begin, ig_stop, jg_begin, jg_stop
      ! Cartesian topology communicator — derived from the subset comm passed to setup().
      ! Owned by the grid; does not alias or mutate the caller's comm.
      ! MPI_COMM_NULL on ranks not participating in this grid (nested grid use).
      type(MPI_Comm) :: cart_comm
      ! MPI neighbor ranks (MPI_PROC_NULL if at domain boundary)
      integer :: back_rank, shore_rank, left_rank, right_rank
      ! Boundary flags
      logical :: is_back_boundary  = .false.
      logical :: is_shore_boundary = .false.
      logical :: is_left_boundary  = .false.
      logical :: is_right_boundary = .false.
      ! Loop bounds — derived from grid at setup(); safe for OMP target mapping (no allocatables)
      type(type_loop_bounds) :: lp
      ! Coordinate system
      logical         :: is_spherical = .false.
      type(type_crs)  :: crs
      ! Grid spacing — always 2D arrays (local_nx x local_ny, no ghost cells)
      real(SP) :: dx0 = 0.0_SP, dy0 = 0.0_SP     ! scalar when uniform (for reporting)
      real(SP), allocatable :: dx(:,:), dy(:,:)
      real(SP), allocatable :: inv_dx(:,:), inv_dy(:,:)  ! precomputed 1/dx, 1/dy
      real(SP), allocatable :: x(:,:), y(:,:)       ! physical coordinates (local metres)
   contains
      procedure, public :: decompose
      procedure, public :: setup
      procedure, public :: halo_exchange
      procedure, public :: init_spacing_uniform
      procedure, public :: init_spacing_variable
      procedure, public :: init_spacing_spherical
      generic,   public :: init_spacing => init_spacing_uniform, init_spacing_variable
      procedure, public :: finalize => grid_finalize
   end type type_grid_2d

contains

   subroutine setup(this, comm, create_partition)
      class(type_grid_2d), intent(inout) :: this
      type(type_comm),     intent(in)    :: comm
      logical,             intent(in)    :: create_partition

      integer, parameter :: n_dims = 2
      integer, dimension(n_dims) :: dims, coords
      logical, dimension(n_dims) :: periods
      integer :: ier

      if (create_partition) then
         call compute_optimal_grid_size(comm%size, this%M, this%N, this%nx_proc, this%ny_proc)
      end if

      dims    = [this%nx_proc, this%ny_proc]
      periods = [.false., .false.]

      ! Create Cart topology from the caller's comm without mutating it.
      ! reorder=.false. guarantees Cart ranks == caller ranks, so comm%rank_id
      ! is valid for MPI_Cart_coords without re-querying.
      call MPI_Cart_Create(comm%id, n_dims, dims, periods, .false., this%cart_comm, ier)

      call MPI_Cart_coords(this%cart_comm, comm%rank_id, n_dims, coords, ier)
      this%iproc = coords(1)
      this%jproc = coords(2)

      call MPI_Cart_shift(this%cart_comm, 0, 1, this%back_rank,  this%shore_rank, ier)
      call MPI_Cart_shift(this%cart_comm, 1, 1, this%right_rank, this%left_rank,  ier)

      this%is_back_boundary  = (this%back_rank  == MPI_PROC_NULL)
      this%is_shore_boundary = (this%shore_rank == MPI_PROC_NULL)
      this%is_left_boundary  = (this%left_rank  == MPI_PROC_NULL)
      this%is_right_boundary = (this%right_rank == MPI_PROC_NULL)

      call grid_range_per_procs(1, this%M, this%nx_proc, this%iproc, &
                                this%ibegin, this%istop, this%local_nx)
      call grid_range_per_procs(1, this%N, this%ny_proc, this%jproc, &
                                this%jbegin, this%jstop, this%local_ny)

      this%ig_begin = this%ibegin - N_GHOST
      this%ig_stop  = this%istop  + N_GHOST
      this%jg_begin = this%jbegin - N_GHOST
      this%jg_stop  = this%jstop  + N_GHOST

      this%lp%ib   = N_GHOST + 1
      this%lp%ie   = N_GHOST + this%local_nx
      this%lp%jb   = N_GHOST + 1
      this%lp%je   = N_GHOST + this%local_ny
      this%lp%mloc = this%local_nx + 2*N_GHOST
      this%lp%nloc = this%local_ny + 2*N_GHOST

   end subroutine setup

   ! Ghost-cell exchange for a ghost-inclusive field.
   ! field must be allocated as (local_nx + 2*N_GHOST, local_ny + 2*N_GHOST).
   ! Two-phase: x-direction first so corners are correct when y-strips are sent.
   subroutine halo_exchange(this, field)
      class(type_grid_2d), intent(in)    :: this
      real(SP),            intent(inout) :: field(:,:)

      integer :: nx, ny, ng, mloc_g, nloc_g
      integer :: nreq, ierr, i, j
      type(MPI_Request) :: req(4)
      type(MPI_Status)  :: stat(4)

      ! x-direction send/recv buffers: (nloc_g, ng) — contiguous in memory
      real(SP), allocatable :: sbuf_back(:,:), rbuf_back(:,:)
      real(SP), allocatable :: sbuf_shore(:,:), rbuf_shore(:,:)
      ! y-direction send/recv buffers: (mloc_g, ng)
      real(SP), allocatable :: sbuf_right(:,:), rbuf_right(:,:)
      real(SP), allocatable :: sbuf_left(:,:), rbuf_left(:,:)

      nx     = this%local_nx
      ny     = this%local_ny
      ng     = N_GHOST
      mloc_g = nx + 2*ng
      nloc_g = ny + 2*ng

      ! ---- Phase 1: x-direction (back / shore) ----
      allocate(sbuf_back (nloc_g, ng), rbuf_back (nloc_g, ng))
      allocate(sbuf_shore(nloc_g, ng), rbuf_shore(nloc_g, ng))

      ! Pack: low-x interior strip → send to back_rank
      do i = 1, ng
         do j = 1, nloc_g
            sbuf_back(j, i) = field(ng + i, j)
         end do
      end do
      ! Pack: high-x interior strip → send to shore_rank
      do i = 1, ng
         do j = 1, nloc_g
            sbuf_shore(j, i) = field(nx + i, j)
         end do
      end do

      nreq = 0
      if (this%back_rank /= MPI_PROC_NULL) then
         nreq = nreq + 1
         call MPI_Irecv(rbuf_back,  nloc_g*ng, MPI_SP, this%back_rank,  0, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_back,  nloc_g*ng, MPI_SP, this%back_rank,  1, this%cart_comm, req(nreq), ierr)
      end if
      if (this%shore_rank /= MPI_PROC_NULL) then
         nreq = nreq + 1
         call MPI_Irecv(rbuf_shore, nloc_g*ng, MPI_SP, this%shore_rank, 1, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_shore, nloc_g*ng, MPI_SP, this%shore_rank, 0, this%cart_comm, req(nreq), ierr)
      end if
      if (nreq > 0) call MPI_Waitall(nreq, req, stat, ierr)

      ! Unpack into ghost cells
      if (this%back_rank /= MPI_PROC_NULL) then
         do i = 1, ng
            do j = 1, nloc_g
               field(i, j) = rbuf_back(j, i)
            end do
         end do
      end if
      if (this%shore_rank /= MPI_PROC_NULL) then
         do i = 1, ng
            do j = 1, nloc_g
               field(nx + ng + i, j) = rbuf_shore(j, i)
            end do
         end do
      end if

      deallocate(sbuf_back, rbuf_back, sbuf_shore, rbuf_shore)

      ! ---- Phase 2: y-direction (right / left) ----
      ! After phase 1, x ghost cells are filled — y-sends include correct corner data.
      allocate(sbuf_right(mloc_g, ng), rbuf_right(mloc_g, ng))
      allocate(sbuf_left (mloc_g, ng), rbuf_left (mloc_g, ng))

      do j = 1, ng
         do i = 1, mloc_g
            sbuf_right(i, j) = field(i, ng + j)
            sbuf_left (i, j) = field(i, ny + j)
         end do
      end do

      nreq = 0
      if (this%right_rank /= MPI_PROC_NULL) then
         nreq = nreq + 1
         call MPI_Irecv(rbuf_right, mloc_g*ng, MPI_SP, this%right_rank, 2, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_right, mloc_g*ng, MPI_SP, this%right_rank, 3, this%cart_comm, req(nreq), ierr)
      end if
      if (this%left_rank /= MPI_PROC_NULL) then
         nreq = nreq + 1
         call MPI_Irecv(rbuf_left,  mloc_g*ng, MPI_SP, this%left_rank,  3, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_left,  mloc_g*ng, MPI_SP, this%left_rank,  2, this%cart_comm, req(nreq), ierr)
      end if
      if (nreq > 0) call MPI_Waitall(nreq, req, stat, ierr)

      if (this%right_rank /= MPI_PROC_NULL) then
         do j = 1, ng
            do i = 1, mloc_g
               field(i, j) = rbuf_right(i, j)
            end do
         end do
      end if
      if (this%left_rank /= MPI_PROC_NULL) then
         do j = 1, ng
            do i = 1, mloc_g
               field(i, ny + ng + j) = rbuf_left(i, j)
            end do
         end do
      end if

      deallocate(sbuf_right, rbuf_right, sbuf_left, rbuf_left)
   end subroutine halo_exchange

   subroutine decompose(this, nprocs)
      class(type_grid_2d), intent(inout) :: this
      integer, intent(in) :: nprocs
      call compute_optimal_grid_size(nprocs, this%M, this%N, this%nx_proc, this%ny_proc)
   end subroutine decompose

   subroutine grid_range_per_procs(i1_global, i2_global, n_procs, rank_id, i1, i2, local_n)
      integer, intent(in)  :: i1_global, i2_global, n_procs, rank_id
      integer, intent(out) :: i1, i2, local_n
      integer :: n_min, n_left

      n_min  = int((i2_global - i1_global + 1) / n_procs)
      n_left = mod(i2_global - i1_global + 1, n_procs)
      i1 = rank_id * n_min + i1_global + min(rank_id, n_left)
      i2 = i1 + n_min - 1
      if (n_left > rank_id) i2 = i2 + 1
      local_n = i2 - i1 + 1

   end subroutine grid_range_per_procs

   subroutine compute_optimal_grid_size(nproc, nx, ny, px, py)
      integer, intent(in)  :: nx, ny, nproc
      integer, intent(out) :: px, py
      integer, allocatable :: factors(:)
      integer :: nfactors, i, min_i
      real(SP) :: ratio, nx_loc, ny_loc, min_ratio

      call get_factors(nproc, factors, nfactors)
      min_ratio = 9e10_SP
      do i = 1, nfactors
         nx_loc = (1.0_SP*nx) / (1.0_SP*factors(i))
         ny_loc = (1.0_SP*ny) / ((1.0_SP*nproc) / (1.0_SP*factors(i)))
         if (nx_loc > ny_loc) then
            ratio = nx_loc / ny_loc
         else
            ratio = ny_loc / nx_loc
         end if
         if (ratio < min_ratio) then
            min_i = i
            min_ratio = ratio
         end if
      end do
      px = factors(min_i)
      py = nproc / factors(min_i)

   end subroutine compute_optimal_grid_size

   subroutine get_factors(n, factors, nfactors)
      integer, intent(in)  :: n
      integer, allocatable, intent(out) :: factors(:)
      integer, intent(out) :: nfactors
      integer :: i, limit, count
      integer, allocatable :: temp(:)

      if (n <= 0) then
         nfactors = 0
         allocate(factors(0))
         return
      end if

      limit = int(sqrt(real(n)))
      allocate(temp(2*limit))
      count = 0
      do i = 1, limit
         if (mod(n, i) == 0) then
            count = count + 1
            temp(count) = i
            if (i /= n/i) then
               count = count + 1
               temp(count) = n/i
            end if
         end if
      end do
      nfactors = count
      allocate(factors(nfactors))
      factors = temp(1:nfactors)
      deallocate(temp)

   end subroutine get_factors

   subroutine init_spacing_uniform(this, dx0, dy0, x0, y0)
      class(type_grid_2d), intent(inout) :: this
      real(SP), intent(in) :: dx0, dy0
      real(SP), intent(in), optional :: x0, y0
      integer :: i, j
      real(SP) :: ox, oy

      this%dx0 = dx0
      this%dy0 = dy0
      ox = 0.0_SP; if (present(x0)) ox = x0
      oy = 0.0_SP; if (present(y0)) oy = y0

      call grid_finalize(this)

      allocate(this%dx    (this%local_nx, this%local_ny), source=dx0)
      allocate(this%dy    (this%local_nx, this%local_ny), source=dy0)
      allocate(this%inv_dx(this%local_nx, this%local_ny), source=1.0_SP/dx0)
      allocate(this%inv_dy(this%local_nx, this%local_ny), source=1.0_SP/dy0)
      allocate(this%x(this%local_nx, this%local_ny))
      allocate(this%y(this%local_nx, this%local_ny))

      do j = 1, this%local_ny
         do i = 1, this%local_nx
            this%x(i,j) = ox + real(this%ibegin + i - 2, SP) * dx0
            this%y(i,j) = oy + real(this%jbegin + j - 2, SP) * dy0
         end do
      end do

   end subroutine init_spacing_uniform

   subroutine init_spacing_variable(this, dx, dy)
      class(type_grid_2d), intent(inout) :: this
      real(SP), intent(in) :: dx(:,:), dy(:,:)

      integer  :: i, j, ierr
      real(SP) :: x_offset, y_offset
      type(MPI_Comm) :: row_comm, col_comm
      logical :: remain(2)

      this%dx0 = 0.0_SP
      this%dy0 = 0.0_SP

      call grid_finalize(this)

      allocate(this%dx,     source=dx)
      allocate(this%dy,     source=dy)
      allocate(this%inv_dx(size(dx,1), size(dx,2)))
      allocate(this%inv_dy(size(dy,1), size(dy,2)))
      this%inv_dx = 1.0_SP / dx
      this%inv_dy = 1.0_SP / dy

      ! x-offset: prefix sum of each rank's local x-extent across the i-direction.
      ! remain=[T,F] creates sub-comms that vary in i, fixed j (row communicators).
      ! MPI_Exscan result is undefined for rank 0 in each sub-comm; x_offset=0 handles it.
      remain = [.true., .false.]
      call MPI_Cart_sub(this%cart_comm, remain, row_comm, ierr)
      x_offset = 0.0_SP
      call MPI_Exscan(sum(dx(:,1)), x_offset, 1, MPI_SP, MPI_SUM, row_comm, ierr)
      call MPI_Comm_free(row_comm, ierr)

      ! y-offset: prefix sum across the j-direction (column communicators).
      remain = [.false., .true.]
      call MPI_Cart_sub(this%cart_comm, remain, col_comm, ierr)
      y_offset = 0.0_SP
      call MPI_Exscan(sum(dy(1,:)), y_offset, 1, MPI_SP, MPI_SUM, col_comm, ierr)
      call MPI_Comm_free(col_comm, ierr)

      ! Build x/y as global physical coordinates.
      ! Assumes separable spacing: dx varies only in i, dy varies only in j.
      ! x(:,j) and y(i,:) are uniform across the other axis — column 1 / row 1 are representative.
      allocate(this%x(this%local_nx, this%local_ny))
      allocate(this%y(this%local_nx, this%local_ny))

      this%x(1,:) = x_offset
      do i = 2, this%local_nx
         this%x(i,:) = this%x(i-1,:) + dx(i-1, 1)
      end do

      this%y(:,1) = y_offset
      do j = 2, this%local_ny
         this%y(:,j) = this%y(:,j-1) + dy(1, j-1)
      end do

   end subroutine init_spacing_variable

   ! Spherical (lon/lat) grid spacing.
   ! dx varies with latitude; dy is constant.
   ! x/y stored in local metres using origin latitude as reference (flat-earth approx).
   ! This keeps grid%x(:,1) monotonic for interpolator bisection — error is O(cos(lat+span)/cos(lat)-1).
   subroutine init_spacing_spherical(this, dlon, dlat, lon0, lat0)
      class(type_grid_2d), intent(inout) :: this
      real(SP), intent(in) :: dlon, dlat   ! degrees per grid cell
      real(SP), intent(in) :: lon0, lat0   ! southwest-corner origin, degrees

      integer  :: i, j
      real(SP) :: dlon_r, dlat_r, lat_j_r, lat_ref_r, dx_ref, dy0_val

      this%dx0         = 0.0_SP
      this%dy0         = 0.0_SP
      this%is_spherical = .true.
      this%crs%mode    = CRS_GEOGRAPHIC
      this%crs%origin_x = lon0
      this%crs%origin_y = lat0
      this%crs%theta   = 0.0_SP

      call grid_finalize(this)

      dlon_r    = dlon * PI / 180.0_SP
      dlat_r    = dlat * PI / 180.0_SP
      lat_ref_r = lat0 * PI / 180.0_SP
      dx_ref    = R_EARTH * cos(lat_ref_r) * dlon_r
      dy0_val   = R_EARTH * dlat_r

      allocate(this%dx    (this%local_nx, this%local_ny))
      allocate(this%dy    (this%local_nx, this%local_ny))
      allocate(this%inv_dx(this%local_nx, this%local_ny))
      allocate(this%inv_dy(this%local_nx, this%local_ny))
      allocate(this%x(this%local_nx, this%local_ny))
      allocate(this%y(this%local_nx, this%local_ny))

      do j = 1, this%local_ny
         lat_j_r = lat_ref_r + real(this%jbegin + j - 2, SP) * dlat_r
         do i = 1, this%local_nx
            this%dx(i,j)     = R_EARTH * cos(lat_j_r) * dlon_r
            this%dy(i,j)     = dy0_val
            this%inv_dx(i,j) = 1.0_SP / this%dx(i,j)
            this%inv_dy(i,j) = 1.0_SP / dy0_val
            this%x(i,j)      = real(this%ibegin + i - 2, SP) * dx_ref
            this%y(i,j)      = real(this%jbegin + j - 2, SP) * dy0_val
         end do
      end do
   end subroutine init_spacing_spherical

   subroutine grid_finalize(this)
      class(type_grid_2d), intent(inout) :: this
      if (allocated(this%dx))     deallocate(this%dx)
      if (allocated(this%dy))     deallocate(this%dy)
      if (allocated(this%inv_dx)) deallocate(this%inv_dx)
      if (allocated(this%inv_dy)) deallocate(this%inv_dy)
      if (allocated(this%x))      deallocate(this%x)
      if (allocated(this%y))      deallocate(this%y)
   end subroutine grid_finalize


end module core_grid_mod
