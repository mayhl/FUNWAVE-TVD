module core_grid_mod
   use core_constants_mod, only: SP, N_GHOST, PI, R_EARTH, MPI_SP
   use core_comm_mod, only: type_comm
   use core_crs_mod, only: type_crs, CRS_GEOGRAPHIC
   use mpi_f08
   use, intrinsic :: iso_fortran_env, only: real64
   use core_comm_timers_mod, only: comm_t, comm_n, CT_HALO_X, CT_HALO_Y, &
                                   CT_HALO_ONE
   implicit none

   type, public :: type_loop_bounds
      integer :: ib, ie        ! interior x bounds in ghost-inclusive indexing (ib = N_GHOST+1)
      integer :: jb, je        ! interior y bounds
      integer :: mloc, nloc    ! ghost-inclusive local array dims
      integer :: kb = 0       ! σ-layer start (ke < kb = 2D sentinel)
      integer :: ke = -1      ! σ-layer end
      integer :: kloc = 0      ! ghost-inclusive σ-layer dim (0 = 2D)
      integer :: ti = 32      ! tile size x (CPU OMP cache blocking)
      integer :: tj = 32      ! tile size y
   end type type_loop_bounds

   ! Field descriptor for halo_exchange_batch: callers point each slot at a
   ! ghost-inclusive field and all slots ride one message per neighbor per
   ! phase.  Rank-2 by construction; the 3D grid gets a rank-3 sibling with
   ! interior-k packing (the message plan is flattened counts either way).
   type, public :: type_halo_field
      real(SP), pointer :: f(:, :) => null()
   end type type_halo_field

   ! Batch slots sized at setup; halo_exchange_batch chunks longer lists
   integer, parameter :: MAX_HALO_BATCH = 16

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
      ! Column sub-communicator (fixed iproc, every jproc) — the trid_y
      ! transpose path all-to-alls full y-lines inside it
      type(MPI_Comm) :: col_comm
      ! Row sub-communicator (fixed jproc, every iproc) — the periodic
      ! trid_x tail broadcasts beta inside it
      type(MPI_Comm) :: row_comm
      ! MPI neighbor ranks (MPI_PROC_NULL if at domain boundary)
      integer :: back_rank, shore_rank, left_rank, right_rank
      ! Boundary flags
      logical :: is_back_boundary = .false.
      logical :: is_shore_boundary = .false.
      logical :: is_left_boundary = .false.
      logical :: is_right_boundary = .false.
      ! Loop bounds — derived from grid at setup(); safe for OMP target mapping (no allocatables)
      type(type_loop_bounds) :: lp
      ! Coordinate system
      logical         :: is_spherical = .false.
      type(type_crs)  :: crs
      ! Grid spacing — always 2D arrays (local_nx x local_ny, no ghost cells)
      real(SP) :: dx0 = 0.0_SP, dy0 = 0.0_SP     ! scalar when uniform (for reporting)
      real(SP), allocatable :: dx(:, :), dy(:, :)
      real(SP), allocatable :: inv_dx(:, :), inv_dy(:, :)  ! precomputed 1/dx, 1/dy
      real(SP), allocatable :: x(:, :), y(:, :)       ! physical coordinates (local metres)
      ! Persistent halo strip buffers (allocated at setup) — halo_exchange
      ! runs tens of fields per stage, so per-call heap churn is measurable.
      ! Pointer (not allocatable) components so the intent(in) grid dummies
      ! of the exchange routines may define their targets.
      real(SP), pointer :: hx_sbuf_back(:, :) => null(), hx_rbuf_back(:, :) => null()
      real(SP), pointer :: hx_sbuf_shore(:, :) => null(), hx_rbuf_shore(:, :) => null()
      real(SP), pointer :: hx_sbuf_right(:, :) => null(), hx_rbuf_right(:, :) => null()
      real(SP), pointer :: hx_sbuf_left(:, :) => null(), hx_rbuf_left(:, :) => null()
      ! Batched variants: MAX_HALO_BATCH field slots per message, flat layout
      real(SP), pointer :: hb_sbuf_back(:) => null(), hb_rbuf_back(:) => null()
      real(SP), pointer :: hb_sbuf_shore(:) => null(), hb_rbuf_shore(:) => null()
      real(SP), pointer :: hb_sbuf_right(:) => null(), hb_rbuf_right(:) => null()
      real(SP), pointer :: hb_sbuf_left(:) => null(), hb_rbuf_left(:) => null()
      ! Persistent-request cache for halo_exchange_batch — counts and buffer
      ! addresses are fixed per batch length, so one Startall replaces the
      ! per-call Isend/Irecv construction and UCX keeps its registration warm.
      ! Column nb holds that length's x/y-phase request quads, built lazily on
      ! first use; PROC_NULL neighbors ride along as no-op requests.  Pointer
      ! components for the same intent(in) reason as the buffers.
      type(MPI_Request), pointer :: hb_reqx(:, :) => null()  ! (4, MAX_HALO_BATCH)
      type(MPI_Request), pointer :: hb_reqy(:, :) => null()
      logical, pointer :: hb_req_ready(:) => null()
   contains
      procedure, public :: decompose
      procedure, public :: setup
      procedure, public :: halo_exchange
      procedure, public :: halo_exchange_batch
      procedure, public :: halo_accumulate
      procedure, public :: init_spacing_uniform
      procedure, public :: init_spacing_variable
      procedure, public :: init_spacing_spherical
      generic, public :: init_spacing => init_spacing_uniform, init_spacing_variable
      procedure, public :: finalize => grid_finalize
   end type type_grid_2d

contains

   ! periodic_y: wrap the cart topology in y (legacy PERIODIC, south-north);
   ! periodic_x likewise for west-east.  halo_exchange then fills ghosts
   ! across the wrap and no rank reports a boundary on the wrapped axis,
   ! so physical-BC ghost fills skip those faces.
   subroutine setup(this, comm, create_partition, periodic_y, periodic_x)
      class(type_grid_2d), intent(inout) :: this
      type(type_comm), intent(in)    :: comm
      logical, intent(in)    :: create_partition
      logical, optional, intent(in)    :: periodic_y
      logical, optional, intent(in)    :: periodic_x

      integer, parameter :: n_dims = 2
      integer, dimension(n_dims) :: dims, coords
      logical, dimension(n_dims) :: periods
      integer :: ier, cart_rank
      logical :: wrap_y, wrap_x

      wrap_y = .false.
      if (present(periodic_y)) wrap_y = periodic_y
      wrap_x = .false.
      if (present(periodic_x)) wrap_x = periodic_x

      if (create_partition) then
         call compute_optimal_grid_size(comm%size, this%M, this%N, this%nx_proc, this%ny_proc)
      end if

      dims = [this%nx_proc, this%ny_proc]
      periods = [wrap_x, wrap_y]

      ! Create Cart topology from the caller's comm without mutating it.
      ! reorder=.true. lets the MPI place cart neighbours on nearby hardware
      ! (cray-mpich honors it); safe because every topology-derived value
      ! below comes from cart_comm's OWN rank, never comm%rank_id — the
      ! caller rank survives only for position-free duties (logging, IO root).
      call MPI_Cart_Create(comm%id, n_dims, dims, periods, .true., this%cart_comm, ier)

      call MPI_Comm_rank(this%cart_comm, cart_rank, ier)
      call MPI_Cart_coords(this%cart_comm, cart_rank, n_dims, coords, ier)
      this%iproc = coords(1)
      this%jproc = coords(2)

      call MPI_Cart_shift(this%cart_comm, 0, 1, this%back_rank, this%shore_rank, ier)
      call MPI_Cart_shift(this%cart_comm, 1, 1, this%right_rank, this%left_rank, ier)

      ! column sub-comm for the trid_y transpose path; sub-comm ranks
      ! follow jproc order (Cart_sub keeps retained-dimension ordering)
      call MPI_Cart_sub(this%cart_comm, [.false., .true.], this%col_comm, ier)
      ! row sub-comm for the periodic trid_x beta broadcast (iproc order)
      call MPI_Cart_sub(this%cart_comm, [.true., .false.], this%row_comm, ier)

      this%is_back_boundary = (this%back_rank == MPI_PROC_NULL)
      this%is_shore_boundary = (this%shore_rank == MPI_PROC_NULL)
      this%is_left_boundary = (this%left_rank == MPI_PROC_NULL)
      this%is_right_boundary = (this%right_rank == MPI_PROC_NULL)

      call grid_range_per_procs(1, this%M, this%nx_proc, this%iproc, &
                                this%ibegin, this%istop, this%local_nx)
      call grid_range_per_procs(1, this%N, this%ny_proc, this%jproc, &
                                this%jbegin, this%jstop, this%local_ny)

      this%ig_begin = this%ibegin - N_GHOST
      this%ig_stop = this%istop + N_GHOST
      this%jg_begin = this%jbegin - N_GHOST
      this%jg_stop = this%jstop + N_GHOST

      this%lp%ib = N_GHOST + 1
      this%lp%ie = N_GHOST + this%local_nx
      this%lp%jb = N_GHOST + 1
      this%lp%je = N_GHOST + this%local_ny
      this%lp%mloc = this%local_nx + 2*N_GHOST
      this%lp%nloc = this%local_ny + 2*N_GHOST

      ! halo strip buffers live for the grid's lifetime
      if (associated(this%hx_sbuf_back)) &
         deallocate (this%hx_sbuf_back, this%hx_rbuf_back, &
                     this%hx_sbuf_shore, this%hx_rbuf_shore, &
                     this%hx_sbuf_right, this%hx_rbuf_right, &
                     this%hx_sbuf_left, this%hx_rbuf_left)
      allocate (this%hx_sbuf_back(this%lp%nloc, N_GHOST), this%hx_rbuf_back(this%lp%nloc, N_GHOST))
      allocate (this%hx_sbuf_shore(this%lp%nloc, N_GHOST), this%hx_rbuf_shore(this%lp%nloc, N_GHOST))
      allocate (this%hx_sbuf_right(this%lp%mloc, N_GHOST), this%hx_rbuf_right(this%lp%mloc, N_GHOST))
      allocate (this%hx_sbuf_left(this%lp%mloc, N_GHOST), this%hx_rbuf_left(this%lp%mloc, N_GHOST))

      ! requests reference the buffers — retire them before the memory moves
      call free_batch_requests(this)
      if (associated(this%hb_sbuf_back)) &
         deallocate (this%hb_sbuf_back, this%hb_rbuf_back, &
                     this%hb_sbuf_shore, this%hb_rbuf_shore, &
                     this%hb_sbuf_right, this%hb_rbuf_right, &
                     this%hb_sbuf_left, this%hb_rbuf_left)
      allocate (this%hb_sbuf_back(this%lp%nloc*N_GHOST*MAX_HALO_BATCH), &
                this%hb_rbuf_back(this%lp%nloc*N_GHOST*MAX_HALO_BATCH))
      allocate (this%hb_sbuf_shore(this%lp%nloc*N_GHOST*MAX_HALO_BATCH), &
                this%hb_rbuf_shore(this%lp%nloc*N_GHOST*MAX_HALO_BATCH))
      allocate (this%hb_sbuf_right(this%lp%mloc*N_GHOST*MAX_HALO_BATCH), &
                this%hb_rbuf_right(this%lp%mloc*N_GHOST*MAX_HALO_BATCH))
      allocate (this%hb_sbuf_left(this%lp%mloc*N_GHOST*MAX_HALO_BATCH), &
                this%hb_rbuf_left(this%lp%mloc*N_GHOST*MAX_HALO_BATCH))

      if (.not. associated(this%hb_reqx)) &
         allocate (this%hb_reqx(4, MAX_HALO_BATCH), this%hb_reqy(4, MAX_HALO_BATCH), &
                   this%hb_req_ready(MAX_HALO_BATCH))
      this%hb_req_ready = .false.

   end subroutine setup

   ! Ghost-cell exchange for a ghost-inclusive field.
   ! field must be allocated as (local_nx + 2*N_GHOST, local_ny + 2*N_GHOST).
   ! Two-phase: x-direction first so corners are correct when y-strips are sent.
   subroutine halo_exchange(this, field)
      class(type_grid_2d), intent(in)    :: this
      real(SP), intent(inout) :: field(:, :)

      integer :: nx, ny, ng, mloc_g, nloc_g
      integer :: nreq, ierr, i, j
      real(real64) :: t0
      type(MPI_Request) :: req(4)
      type(MPI_Status)  :: stat(4)

      ! persistent strip buffers: x-phase (nloc_g, ng), y-phase (mloc_g, ng)
      real(SP), pointer :: sbuf_back(:, :), rbuf_back(:, :)
      real(SP), pointer :: sbuf_shore(:, :), rbuf_shore(:, :)
      real(SP), pointer :: sbuf_right(:, :), rbuf_right(:, :)
      real(SP), pointer :: sbuf_left(:, :), rbuf_left(:, :)

      nx = this%local_nx
      ny = this%local_ny
      ng = N_GHOST
      mloc_g = nx + 2*ng
      nloc_g = ny + 2*ng

      sbuf_back => this%hx_sbuf_back; rbuf_back => this%hx_rbuf_back
      sbuf_shore => this%hx_sbuf_shore; rbuf_shore => this%hx_rbuf_shore
      sbuf_right => this%hx_sbuf_right; rbuf_right => this%hx_rbuf_right
      sbuf_left => this%hx_sbuf_left; rbuf_left => this%hx_rbuf_left

      ! ---- Phase 1: x-direction (back / shore) ----

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
         call MPI_Irecv(rbuf_back, nloc_g*ng, MPI_SP, this%back_rank, 0, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_back, nloc_g*ng, MPI_SP, this%back_rank, 1, this%cart_comm, req(nreq), ierr)
      end if
      if (this%shore_rank /= MPI_PROC_NULL) then
         nreq = nreq + 1
         call MPI_Irecv(rbuf_shore, nloc_g*ng, MPI_SP, this%shore_rank, 1, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_shore, nloc_g*ng, MPI_SP, this%shore_rank, 0, this%cart_comm, req(nreq), ierr)
      end if
      t0 = MPI_Wtime()
      if (nreq > 0) call MPI_Waitall(nreq, req, stat, ierr)
      comm_t(CT_HALO_ONE) = comm_t(CT_HALO_ONE) + (MPI_Wtime() - t0)
      comm_n(CT_HALO_ONE) = comm_n(CT_HALO_ONE) + 1

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

      ! ---- Phase 2: y-direction (right / left) ----
      ! After phase 1, x ghost cells are filled — y-sends include correct corner data.

      do j = 1, ng
         do i = 1, mloc_g
            sbuf_right(i, j) = field(i, ng + j)
            sbuf_left(i, j) = field(i, ny + j)
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
         call MPI_Irecv(rbuf_left, mloc_g*ng, MPI_SP, this%left_rank, 3, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_left, mloc_g*ng, MPI_SP, this%left_rank, 2, this%cart_comm, req(nreq), ierr)
      end if
      t0 = MPI_Wtime()
      if (nreq > 0) call MPI_Waitall(nreq, req, stat, ierr)
      comm_t(CT_HALO_ONE) = comm_t(CT_HALO_ONE) + (MPI_Wtime() - t0)

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

   end subroutine halo_exchange

   ! Batched halo_exchange: every field in the list rides one packed
   ! message per neighbor per phase instead of its own exchange — the
   ! per-field version costs ~2 Waitall latency legs each, and the
   ! stepper exchanges tens of fields per stage.  Same two-phase
   ! x-then-y plan (fields are mutually independent during exchange,
   ! so values are bitwise those of N sequential halo_exchange calls);
   ! wall fills stay with the caller, per field, after both phases.
   ! Lists longer than MAX_HALO_BATCH are chunked; message posts ride the
   ! persistent-request cache (see batch_requests_init).
   subroutine halo_exchange_batch(this, fields)
      class(type_grid_2d), intent(in) :: this
      type(type_halo_field), intent(in) :: fields(:)

      integer :: nx, ny, ng, mloc_g, nloc_g, strip_x, strip_y
      integer :: nf, n0, nb, n, base
      integer :: ierr, i, j
      real(real64) :: t0
      type(MPI_Status) :: stat(4)

      nx = this%local_nx
      ny = this%local_ny
      ng = N_GHOST
      mloc_g = nx + 2*ng
      nloc_g = ny + 2*ng
      strip_x = nloc_g*ng
      strip_y = mloc_g*ng

      nf = size(fields)
      n0 = 0
      do while (n0 < nf)
         nb = min(nf - n0, MAX_HALO_BATCH)

         ! ---- Phase 1: x-direction (back / shore) ----
         ! pack/unpack threaded over the field index (disjoint slabs);
         ! MPI stays outside the regions (THREAD_FUNNELED)
         !$omp parallel do default(shared) schedule(static) private(base, i, j)
         do n = 1, nb
            base = (n - 1)*strip_x
            do i = 1, ng
               do j = 1, nloc_g
                  this%hb_sbuf_back(base + (i - 1)*nloc_g + j) = fields(n0 + n)%f(ng + i, j)
                  this%hb_sbuf_shore(base + (i - 1)*nloc_g + j) = fields(n0 + n)%f(nx + i, j)
               end do
            end do
         end do
         !$omp end parallel do

         if (.not. this%hb_req_ready(nb)) call batch_requests_init(this, nb)
         t0 = MPI_Wtime()
         call MPI_Startall(4, this%hb_reqx(:, nb), ierr)
         call MPI_Waitall(4, this%hb_reqx(:, nb), stat, ierr)
         comm_t(CT_HALO_X) = comm_t(CT_HALO_X) + (MPI_Wtime() - t0)
         comm_n(CT_HALO_X) = comm_n(CT_HALO_X) + 1

         if (this%back_rank /= MPI_PROC_NULL) then
            !$omp parallel do default(shared) schedule(static) private(base, i, j)
            do n = 1, nb
               base = (n - 1)*strip_x
               do i = 1, ng
                  do j = 1, nloc_g
                     fields(n0 + n)%f(i, j) = this%hb_rbuf_back(base + (i - 1)*nloc_g + j)
                  end do
               end do
            end do
            !$omp end parallel do
         end if
         if (this%shore_rank /= MPI_PROC_NULL) then
            !$omp parallel do default(shared) schedule(static) private(base, i, j)
            do n = 1, nb
               base = (n - 1)*strip_x
               do i = 1, ng
                  do j = 1, nloc_g
                     fields(n0 + n)%f(nx + ng + i, j) = this%hb_rbuf_shore(base + (i - 1)*nloc_g + j)
                  end do
               end do
            end do
            !$omp end parallel do
         end if

         ! ---- Phase 2: y-direction (right / left) ----
         !$omp parallel do default(shared) schedule(static) private(base, i, j)
         do n = 1, nb
            base = (n - 1)*strip_y
            do j = 1, ng
               do i = 1, mloc_g
                  this%hb_sbuf_right(base + (j - 1)*mloc_g + i) = fields(n0 + n)%f(i, ng + j)
                  this%hb_sbuf_left(base + (j - 1)*mloc_g + i) = fields(n0 + n)%f(i, ny + j)
               end do
            end do
         end do
         !$omp end parallel do

         t0 = MPI_Wtime()
         call MPI_Startall(4, this%hb_reqy(:, nb), ierr)
         call MPI_Waitall(4, this%hb_reqy(:, nb), stat, ierr)
         comm_t(CT_HALO_Y) = comm_t(CT_HALO_Y) + (MPI_Wtime() - t0)
         comm_n(CT_HALO_Y) = comm_n(CT_HALO_Y) + 1

         if (this%right_rank /= MPI_PROC_NULL) then
            !$omp parallel do default(shared) schedule(static) private(base, i, j)
            do n = 1, nb
               base = (n - 1)*strip_y
               do j = 1, ng
                  do i = 1, mloc_g
                     fields(n0 + n)%f(i, j) = this%hb_rbuf_right(base + (j - 1)*mloc_g + i)
                  end do
               end do
            end do
            !$omp end parallel do
         end if
         if (this%left_rank /= MPI_PROC_NULL) then
            !$omp parallel do default(shared) schedule(static) private(base, i, j)
            do n = 1, nb
               base = (n - 1)*strip_y
               do j = 1, ng
                  do i = 1, mloc_g
                     fields(n0 + n)%f(i, ny + ng + j) = this%hb_rbuf_left(base + (j - 1)*mloc_g + i)
                  end do
               end do
            end do
            !$omp end parallel do
         end if

         n0 = n0 + nb
      end do

   end subroutine halo_exchange_batch

   ! To build the persistent quads for batch length nb: counts are fixed per
   ! length and the buffers sit at fixed addresses from setup(), so the
   ! requests stay valid until the grid is re-setup or finalized.  Tag scheme
   ! matches the old per-call posts exactly.
   subroutine batch_requests_init(this, nb)
      class(type_grid_2d), intent(in) :: this
      integer, intent(in) :: nb
      integer :: strip_x, strip_y, ierr
      strip_x = this%lp%nloc*N_GHOST
      strip_y = this%lp%mloc*N_GHOST
      call MPI_Recv_init(this%hb_rbuf_back, nb*strip_x, MPI_SP, this%back_rank, 0, &
                         this%cart_comm, this%hb_reqx(1, nb), ierr)
      call MPI_Send_init(this%hb_sbuf_back, nb*strip_x, MPI_SP, this%back_rank, 1, &
                         this%cart_comm, this%hb_reqx(2, nb), ierr)
      call MPI_Recv_init(this%hb_rbuf_shore, nb*strip_x, MPI_SP, this%shore_rank, 1, &
                         this%cart_comm, this%hb_reqx(3, nb), ierr)
      call MPI_Send_init(this%hb_sbuf_shore, nb*strip_x, MPI_SP, this%shore_rank, 0, &
                         this%cart_comm, this%hb_reqx(4, nb), ierr)
      call MPI_Recv_init(this%hb_rbuf_right, nb*strip_y, MPI_SP, this%right_rank, 2, &
                         this%cart_comm, this%hb_reqy(1, nb), ierr)
      call MPI_Send_init(this%hb_sbuf_right, nb*strip_y, MPI_SP, this%right_rank, 3, &
                         this%cart_comm, this%hb_reqy(2, nb), ierr)
      call MPI_Recv_init(this%hb_rbuf_left, nb*strip_y, MPI_SP, this%left_rank, 3, &
                         this%cart_comm, this%hb_reqy(3, nb), ierr)
      call MPI_Send_init(this%hb_sbuf_left, nb*strip_y, MPI_SP, this%left_rank, 2, &
                         this%cart_comm, this%hb_reqy(4, nb), ierr)
      this%hb_req_ready(nb) = .true.
   end subroutine batch_requests_init

   subroutine free_batch_requests(this)
      class(type_grid_2d), intent(inout) :: this
      integer :: nb, k, ierr
      if (.not. associated(this%hb_req_ready)) return
      do nb = 1, MAX_HALO_BATCH
         if (.not. this%hb_req_ready(nb)) cycle
         do k = 1, 4
            call MPI_Request_free(this%hb_reqx(k, nb), ierr)
            call MPI_Request_free(this%hb_reqy(k, nb), ierr)
         end do
         this%hb_req_ready(nb) = .false.
      end do
   end subroutine free_batch_requests

   ! Reverse of halo_exchange: ship ghost-cell CONTRIBUTIONS back to the
   ! owning rank's interior and add them there (kernels that scatter across
   ! a subdomain edge, e.g. avalanche flux, write into ghosts they do not
   ! own).  Phase order is the exchange mirrored — y first, then x — so a
   ! corner contribution rides two hops: the y-strip carries the full
   ! ghost-inclusive width into the neighbour's x-ghost columns, and the
   ! x-phase then delivers it to the diagonal owner.  Sent ghost strips are
   ! zeroed after packing, so each contribution lands exactly once; ghosts
   ! at a physical (neighbourless) wall are left untouched for the caller.
   subroutine halo_accumulate(this, field)
      class(type_grid_2d), intent(in)    :: this
      real(SP), intent(inout) :: field(:, :)

      integer :: nx, ny, ng, mloc_g, nloc_g
      integer :: nreq, ierr, i, j
      type(MPI_Request) :: req(4)
      type(MPI_Status)  :: stat(4)

      ! persistent strip buffers: y-phase (mloc_g, ng), x-phase (nloc_g, ng)
      real(SP), pointer :: sbuf_right(:, :), rbuf_right(:, :)
      real(SP), pointer :: sbuf_left(:, :), rbuf_left(:, :)
      real(SP), pointer :: sbuf_back(:, :), rbuf_back(:, :)
      real(SP), pointer :: sbuf_shore(:, :), rbuf_shore(:, :)

      nx = this%local_nx
      ny = this%local_ny
      ng = N_GHOST
      mloc_g = nx + 2*ng
      nloc_g = ny + 2*ng

      sbuf_right => this%hx_sbuf_right; rbuf_right => this%hx_rbuf_right
      sbuf_left => this%hx_sbuf_left; rbuf_left => this%hx_rbuf_left
      sbuf_back => this%hx_sbuf_back; rbuf_back => this%hx_rbuf_back
      sbuf_shore => this%hx_sbuf_shore; rbuf_shore => this%hx_rbuf_shore

      ! ---- Phase 1: y-direction (right / left) ----

      ! Pack: low-y ghost rows → their owner (right_rank's high-y interior),
      ! high-y ghost rows → left_rank's low-y interior; full ghost-inclusive
      ! width so corner blocks travel with the strip
      do j = 1, ng
         do i = 1, mloc_g
            sbuf_right(i, j) = field(i, j)
            sbuf_left(i, j) = field(i, ny + ng + j)
         end do
      end do
      if (this%right_rank /= MPI_PROC_NULL) field(:, 1:ng) = 0.0_SP
      if (this%left_rank /= MPI_PROC_NULL) field(:, ny + ng + 1:nloc_g) = 0.0_SP

      nreq = 0
      if (this%left_rank /= MPI_PROC_NULL) then
         nreq = nreq + 1
         call MPI_Irecv(rbuf_left, mloc_g*ng, MPI_SP, this%left_rank, 4, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_left, mloc_g*ng, MPI_SP, this%left_rank, 5, this%cart_comm, req(nreq), ierr)
      end if
      if (this%right_rank /= MPI_PROC_NULL) then
         nreq = nreq + 1
         call MPI_Irecv(rbuf_right, mloc_g*ng, MPI_SP, this%right_rank, 5, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_right, mloc_g*ng, MPI_SP, this%right_rank, 4, this%cart_comm, req(nreq), ierr)
      end if
      if (nreq > 0) call MPI_Waitall(nreq, req, stat, ierr)

      ! Accumulate: left neighbour's low ghosts land in the high-y interior
      ! edge rows, right neighbour's high ghosts in the low-y edge rows
      if (this%left_rank /= MPI_PROC_NULL) then
         do j = 1, ng
            do i = 1, mloc_g
               field(i, ny + j) = field(i, ny + j) + rbuf_left(i, j)
            end do
         end do
      end if
      if (this%right_rank /= MPI_PROC_NULL) then
         do j = 1, ng
            do i = 1, mloc_g
               field(i, ng + j) = field(i, ng + j) + rbuf_right(i, j)
            end do
         end do
      end if

      ! ---- Phase 2: x-direction (back / shore) ----
      ! After phase 1, corner contributions received from y-neighbours sit
      ! in the x-ghost columns' interior rows and forward with the strip.

      do i = 1, ng
         do j = 1, nloc_g
            sbuf_back(j, i) = field(i, j)
            sbuf_shore(j, i) = field(nx + ng + i, j)
         end do
      end do
      if (this%back_rank /= MPI_PROC_NULL) field(1:ng, :) = 0.0_SP
      if (this%shore_rank /= MPI_PROC_NULL) field(nx + ng + 1:mloc_g, :) = 0.0_SP

      nreq = 0
      if (this%back_rank /= MPI_PROC_NULL) then
         nreq = nreq + 1
         call MPI_Irecv(rbuf_back, nloc_g*ng, MPI_SP, this%back_rank, 7, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_back, nloc_g*ng, MPI_SP, this%back_rank, 6, this%cart_comm, req(nreq), ierr)
      end if
      if (this%shore_rank /= MPI_PROC_NULL) then
         nreq = nreq + 1
         call MPI_Irecv(rbuf_shore, nloc_g*ng, MPI_SP, this%shore_rank, 6, this%cart_comm, req(nreq), ierr)
         nreq = nreq + 1
         call MPI_Isend(sbuf_shore, nloc_g*ng, MPI_SP, this%shore_rank, 7, this%cart_comm, req(nreq), ierr)
      end if
      if (nreq > 0) call MPI_Waitall(nreq, req, stat, ierr)

      if (this%shore_rank /= MPI_PROC_NULL) then
         do i = 1, ng
            do j = 1, nloc_g
               field(nx + i, j) = field(nx + i, j) + rbuf_shore(j, i)
            end do
         end do
      end if
      if (this%back_rank /= MPI_PROC_NULL) then
         do i = 1, ng
            do j = 1, nloc_g
               field(ng + i, j) = field(ng + i, j) + rbuf_back(j, i)
            end do
         end do
      end if

   end subroutine halo_accumulate

   subroutine decompose(this, nprocs)
      class(type_grid_2d), intent(inout) :: this
      integer, intent(in) :: nprocs
      call compute_optimal_grid_size(nprocs, this%M, this%N, this%nx_proc, this%ny_proc)
   end subroutine decompose

   subroutine grid_range_per_procs(i1_global, i2_global, n_procs, rank_id, i1, i2, local_n)
      integer, intent(in)  :: i1_global, i2_global, n_procs, rank_id
      integer, intent(out) :: i1, i2, local_n
      integer :: n_min, n_left

      n_min = int((i2_global - i1_global + 1)/n_procs)
      n_left = mod(i2_global - i1_global + 1, n_procs)
      i1 = rank_id*n_min + i1_global + min(rank_id, n_left)
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
         nx_loc = (1.0_SP*nx)/(1.0_SP*factors(i))
         ny_loc = (1.0_SP*ny)/((1.0_SP*nproc)/(1.0_SP*factors(i)))
         if (nx_loc > ny_loc) then
            ratio = nx_loc/ny_loc
         else
            ratio = ny_loc/nx_loc
         end if
         if (ratio < min_ratio) then
            min_i = i
            min_ratio = ratio
         end if
      end do
      px = factors(min_i)
      py = nproc/factors(min_i)

   end subroutine compute_optimal_grid_size

   subroutine get_factors(n, factors, nfactors)
      integer, intent(in)  :: n
      integer, allocatable, intent(out) :: factors(:)
      integer, intent(out) :: nfactors
      integer :: i, limit, count
      integer, allocatable :: temp(:)

      if (n <= 0) then
         nfactors = 0
         allocate (factors(0))
         return
      end if

      limit = int(sqrt(real(n)))
      allocate (temp(2*limit))
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
      allocate (factors(nfactors))
      factors = temp(1:nfactors)
      deallocate (temp)

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

      call clear_spacing(this)

      allocate (this%dx(this%local_nx, this%local_ny), source=dx0)
      allocate (this%dy(this%local_nx, this%local_ny), source=dy0)
      allocate (this%inv_dx(this%local_nx, this%local_ny), source=1.0_SP/dx0)
      allocate (this%inv_dy(this%local_nx, this%local_ny), source=1.0_SP/dy0)
      allocate (this%x(this%local_nx, this%local_ny))
      allocate (this%y(this%local_nx, this%local_ny))

      do j = 1, this%local_ny
         do i = 1, this%local_nx
            this%x(i, j) = ox + real(this%ibegin + i - 2, SP)*dx0
            this%y(i, j) = oy + real(this%jbegin + j - 2, SP)*dy0
         end do
      end do

   end subroutine init_spacing_uniform

   subroutine init_spacing_variable(this, dx, dy)
      class(type_grid_2d), intent(inout) :: this
      real(SP), intent(in) :: dx(:, :), dy(:, :)

      integer  :: i, j, ierr
      real(SP) :: x_offset, y_offset
      type(MPI_Comm) :: row_comm, col_comm
      logical :: remain(2)

      this%dx0 = 0.0_SP
      this%dy0 = 0.0_SP

      call clear_spacing(this)

      allocate (this%dx, source=dx)
      allocate (this%dy, source=dy)
      allocate (this%inv_dx(size(dx, 1), size(dx, 2)))
      allocate (this%inv_dy(size(dy, 1), size(dy, 2)))
      this%inv_dx = 1.0_SP/dx
      this%inv_dy = 1.0_SP/dy

      ! x-offset: prefix sum of each rank's local x-extent across the i-direction.
      ! remain=[T,F] creates sub-comms that vary in i, fixed j (row communicators).
      ! MPI_Exscan result is undefined for rank 0 in each sub-comm; x_offset=0 handles it.
      remain = [.true., .false.]
      call MPI_Cart_sub(this%cart_comm, remain, row_comm, ierr)
      x_offset = 0.0_SP
      call MPI_Exscan(sum(dx(:, 1)), x_offset, 1, MPI_SP, MPI_SUM, row_comm, ierr)
      call MPI_Comm_free(row_comm, ierr)

      ! y-offset: prefix sum across the j-direction (column communicators).
      remain = [.false., .true.]
      call MPI_Cart_sub(this%cart_comm, remain, col_comm, ierr)
      y_offset = 0.0_SP
      call MPI_Exscan(sum(dy(1, :)), y_offset, 1, MPI_SP, MPI_SUM, col_comm, ierr)
      call MPI_Comm_free(col_comm, ierr)

      ! Build x/y as global physical coordinates.
      ! Assumes separable spacing: dx varies only in i, dy varies only in j.
      ! x(:,j) and y(i,:) are uniform across the other axis — column 1 / row 1 are representative.
      allocate (this%x(this%local_nx, this%local_ny))
      allocate (this%y(this%local_nx, this%local_ny))

      this%x(1, :) = x_offset
      do i = 2, this%local_nx
         this%x(i, :) = this%x(i - 1, :) + dx(i - 1, 1)
      end do

      this%y(:, 1) = y_offset
      do j = 2, this%local_ny
         this%y(:, j) = this%y(:, j - 1) + dy(1, j - 1)
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

      this%dx0 = 0.0_SP
      this%dy0 = 0.0_SP
      this%is_spherical = .true.
      this%crs%mode = CRS_GEOGRAPHIC
      this%crs%origin_x = lon0
      this%crs%origin_y = lat0
      this%crs%theta = 0.0_SP

      call clear_spacing(this)

      dlon_r = dlon*PI/180.0_SP
      dlat_r = dlat*PI/180.0_SP
      lat_ref_r = lat0*PI/180.0_SP
      dx_ref = R_EARTH*cos(lat_ref_r)*dlon_r
      dy0_val = R_EARTH*dlat_r

      allocate (this%dx(this%local_nx, this%local_ny))
      allocate (this%dy(this%local_nx, this%local_ny))
      allocate (this%inv_dx(this%local_nx, this%local_ny))
      allocate (this%inv_dy(this%local_nx, this%local_ny))
      allocate (this%x(this%local_nx, this%local_ny))
      allocate (this%y(this%local_nx, this%local_ny))

      do j = 1, this%local_ny
         lat_j_r = lat_ref_r + real(this%jbegin + j - 2, SP)*dlat_r
         do i = 1, this%local_nx
            this%dx(i, j) = R_EARTH*cos(lat_j_r)*dlon_r
            this%dy(i, j) = dy0_val
            this%inv_dx(i, j) = 1.0_SP/this%dx(i, j)
            this%inv_dy(i, j) = 1.0_SP/dy0_val
            this%x(i, j) = real(this%ibegin + i - 2, SP)*dx_ref
            this%y(i, j) = real(this%jbegin + j - 2, SP)*dy0_val
         end do
      end do
   end subroutine init_spacing_spherical

   ! Spacing arrays only — init_spacing_* re-allocate after this, so the
   ! halo buffers (grid-lifetime, owned by setup) must survive
   subroutine clear_spacing(this)
      class(type_grid_2d), intent(inout) :: this
      if (allocated(this%dx)) deallocate (this%dx)
      if (allocated(this%dy)) deallocate (this%dy)
      if (allocated(this%inv_dx)) deallocate (this%inv_dx)
      if (allocated(this%inv_dy)) deallocate (this%inv_dy)
      if (allocated(this%x)) deallocate (this%x)
      if (allocated(this%y)) deallocate (this%y)
   end subroutine clear_spacing

   subroutine grid_finalize(this)
      class(type_grid_2d), intent(inout) :: this
      call clear_spacing(this)
      call free_batch_requests(this)
      if (associated(this%hb_reqx)) then
         deallocate (this%hb_reqx, this%hb_reqy, this%hb_req_ready)
         nullify (this%hb_reqx, this%hb_reqy, this%hb_req_ready)
      end if
      if (associated(this%hx_sbuf_back)) then
         deallocate (this%hx_sbuf_back, this%hx_rbuf_back, &
                     this%hx_sbuf_shore, this%hx_rbuf_shore, &
                     this%hx_sbuf_right, this%hx_rbuf_right, &
                     this%hx_sbuf_left, this%hx_rbuf_left)
         nullify (this%hx_sbuf_back, this%hx_rbuf_back, &
                  this%hx_sbuf_shore, this%hx_rbuf_shore, &
                  this%hx_sbuf_right, this%hx_rbuf_right, &
                  this%hx_sbuf_left, this%hx_rbuf_left)
      end if
   end subroutine grid_finalize

end module core_grid_mod
