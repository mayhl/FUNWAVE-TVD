module core_grid_mod
   use core_constants_mod, only: SP, N_GHOST
   use core_comm_mod, only: type_comm
   use core_grid_interface_mod, only: abstract_grid
   use mpi_f08
   implicit none

   type, extends(abstract_grid), public :: type_grid_2d
      integer :: M, N
      ! Domain decomposition
      integer :: nx_proc = 1, ny_proc = 1
      integer :: iproc = 0, jproc = 0
      ! Interior (compute) domain indices — no ghost cells
      integer :: ibegin, istop, jbegin, jstop
      integer :: local_nx, local_ny
      ! Full domain indices — includes ghost cells
      integer :: ig_begin, ig_stop, jg_begin, jg_stop
      ! MPI neighbor ranks (MPI_PROC_NULL if at domain boundary)
      integer :: back_rank, shore_rank, left_rank, right_rank
      ! Boundary flags
      logical :: is_back_boundary  = .false.
      logical :: is_shore_boundary = .false.
      logical :: is_left_boundary  = .false.
      logical :: is_right_boundary = .false.
      ! Grid spacing — always 2D arrays (local_nx x local_ny, no ghost cells)
      logical  :: is_uniform = .false.
      real(SP) :: dx0 = 0.0_SP, dy0 = 0.0_SP     ! scalar values when uniform (for reporting)
      real(SP), allocatable :: dx(:,:), dy(:,:)     ! grid spacing
      real(SP), allocatable :: inv_dx(:,:), inv_dy(:,:)  ! precomputed 1/dx, 1/dy
      real(SP), allocatable :: x(:,:), y(:,:)       ! physical coordinates
   contains
      procedure, public :: get_indices => get_indices_2d
      procedure, public :: decompose
      procedure, public :: setup
      procedure, public :: halo_exchange
      procedure, public :: init_spacing_uniform
      procedure, public :: init_spacing_variable
      generic,   public :: init_spacing => init_spacing_uniform, init_spacing_variable
      procedure, public :: finalize => grid_finalize
   end type type_grid_2d

contains

   subroutine setup(this, comm, create_partition)
      class(type_grid_2d), intent(inout) :: this
      type(type_comm), intent(inout) :: comm
      logical, intent(in) :: create_partition

      integer, parameter :: n_dims = 2
      integer, dimension(n_dims) :: dims, coords
      logical, dimension(n_dims) :: periods
      type(MPI_Comm) :: cart_comm
      integer :: ier

      if (create_partition) then
         call compute_optimal_grid_size(comm%size, this%M, this%N, this%nx_proc, this%ny_proc)
      end if

      dims    = [this%nx_proc, this%ny_proc]
      periods = [.false., .false.]

      call MPI_Cart_Create(comm%id, n_dims, dims, periods, .false., cart_comm, ier)
      comm%id = cart_comm

      call MPI_Comm_rank(comm%id, comm%rank_id, ier)
      call MPI_Cart_coords(comm%id, comm%rank_id, n_dims, coords, ier)
      this%iproc = coords(1)
      this%jproc = coords(2)

      call MPI_Cart_shift(comm%id, 0, 1, this%back_rank,  this%shore_rank, ier)
      call MPI_Cart_shift(comm%id, 1, 1, this%right_rank, this%left_rank,  ier)

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

   end subroutine setup

   subroutine halo_exchange(this, field, comm)
      class(type_grid_2d), intent(in) :: this
      real(SP), intent(inout) :: field(:,:)
      type(type_comm), intent(inout) :: comm
      ! TODO: ghost cell exchange using back/shore/left/right ranks
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

      this%is_uniform = .true.
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

      this%is_uniform = .false.
      this%dx0 = 0.0_SP
      this%dy0 = 0.0_SP

      call grid_finalize(this)

      allocate(this%dx,     source=dx)
      allocate(this%dy,     source=dy)
      allocate(this%inv_dx(size(dx,1), size(dx,2)))
      allocate(this%inv_dy(size(dy,1), size(dy,2)))
      this%inv_dx = 1.0_SP / dx
      this%inv_dy = 1.0_SP / dy
      ! TODO: x/y coordinates require global cumulative sum + broadcast

   end subroutine init_spacing_variable

   subroutine grid_finalize(this)
      class(type_grid_2d), intent(inout) :: this
      if (allocated(this%dx))     deallocate(this%dx)
      if (allocated(this%dy))     deallocate(this%dy)
      if (allocated(this%inv_dx)) deallocate(this%inv_dx)
      if (allocated(this%inv_dy)) deallocate(this%inv_dy)
      if (allocated(this%x))      deallocate(this%x)
      if (allocated(this%y))      deallocate(this%y)
   end subroutine grid_finalize

   subroutine get_indices_2d(this, indices)
      class(type_grid_2d), intent(inout) :: this
      integer, allocatable, intent(out) :: indices(:,:)
      integer :: i, j, k

      allocate(indices(2, this%M*this%N))
      k = 1
      do j = 1, this%N
         do i = 1, this%M
            indices(1, k) = i
            indices(2, k) = j
            k = k + 1
         end do
      end do
      this%n_points = k - 1

   end subroutine get_indices_2d

end module core_grid_mod
