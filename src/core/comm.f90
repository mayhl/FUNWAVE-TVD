!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Communicator
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module core_comm_mod

   use mpi_f08
   use core_constants_mod, only: n_ghost, LABEL_SIZE, SP, MPI_SP
   use core_log_io_mod, only: type_log_writer, new_log_writer

   implicit none(external)

   character(LABEL_SIZE), parameter :: log_key = "MPI"
   integer, parameter :: param_buff_max = 30

   private
   public :: type_comm, new_comm

   interface new_comm
      module procedure type_comm_initialize
   end interface new_comm

   type dummy

   end type dummy

   type type_comm

      private

      type(MPI_Comm), public :: id
      logical :: p_is_io_node
      integer :: io_node_id
      integer, public :: rank_id, size
      type(type_log_writer) :: log

      integer, dimension(:), allocatable :: blockcounts, types, offsets
      integer :: param_type
      integer, public :: param_size

      integer, public :: iproc, jproc
      integer, public :: nx_proc, ny_proc
      ! cross, public-shore (x-)direction.
      ! back , public=> shore <=> -x > + x
      integer, public :: back_rank_id, shore_rank_id
      ! along, publicshore (y-)direction.
      ! right, public => left <=> -y > + y
      integer, public :: left_rank_id, right_rank_id

      integer, public :: ibegin, istop
      integer, public :: jbegin, jstop
      integer, public :: nx, ny
      integer, public :: nx_global, ny_global
      logical, public :: is_left_boundry
      logical, public :: is_right_boundry
      logical, public :: is_shore_boundry
      logical, public :: is_back_boundry

   contains
      procedure, public :: get_logger
      procedure, public :: is_io_node
      procedure, public :: create_2d
      procedure, public :: bcast_integer
      procedure, public :: bcast_logical
      procedure, public :: bcast_real
      procedure, public :: bcast_string

      procedure, public :: finalize
      procedure, public :: barrier
   end type type_comm

contains

   function type_comm_initialize(io_rank_id, comm_id) result(this)

      use mpi_f08, only: MPI_COMM_WORLD, MPI_SUCCESS
      integer, intent(in) :: io_rank_id
      type(MPI_Comm), intent(in), optional :: comm_id
      type(type_comm) :: this

      integer :: ierr
      logical :: is_mpi_initialized

      if (present(comm_id)) then
         this%id = comm_id
      else
         this%id = MPI_COMM_WORLD
         call MPI_Initialized(is_mpi_initialized, ierr)
         if (ierr .ne. MPI_SUCCESS) then
            error stop "Failed to check if MPI is initialized."
         end if

         if (.not. is_mpi_initialized) then
            call MPI_Init(ierr)
            if (ierr .ne. MPI_SUCCESS) then
               error stop "Failed to initialize MPI."
            end if
         end if
      end if

      call MPI_Comm_rank(this%id, this%rank_id, ierr)
      if (ierr .ne. MPI_SUCCESS) then
         error stop "Failed to get MPI rank."
      end if

      call MPI_Comm_size(this%id, this%size, ierr)
      if (ierr .ne. MPI_SUCCESS) then
         error stop "Failed to get MPI size."
      end if

      this%p_is_io_node = this%rank_id .eq. io_rank_id
      this%io_node_id = io_rank_id

      this%log = new_log_writer(log_key, this%p_is_io_node)

   end function type_comm_initialize

   function get_logger(this, label) result(logger)

      class(type_comm), intent(inout) :: this
      character(LABEL_SIZE), intent(in) :: label
      type(type_log_writer) :: logger

      logger = new_log_writer(label, this%p_is_io_node)

   end function get_logger

   function is_io_node(this) result(flag)
      class(type_comm), intent(inout) :: this
      logical :: flag
      flag = this%p_is_io_node
   end function is_io_node

   subroutine create_2d(this, nx_proc, ny_proc, nx_global, ny_global, create_partition)

      class(type_comm), intent(inout) :: this
      integer, intent(in) :: nx_global, ny_global
      integer, intent(out) :: nx_proc, ny_proc
      logical, intent(in) :: create_partition
      integer, parameter  :: n_dims = 2
      integer, dimension(n_dims) :: dims, coords
      logical, dimension(n_dims)  :: periods
      type(MPI_Comm) :: new_comm_id, old_comm_id
      integer :: ier, old_rank_id
      logical :: reorder
      integer :: io

      if (create_partition) then
         call compute_optimal_grid_size(this%size, nx_global, ny_global, nx_proc, ny_proc)

         print *, this%size, nx_proc, ny_proc
      end if

      reorder = .true.
      coords = (/0, 0/)
      periods = (/.false., .false./)
      dims = (/nx_proc, ny_proc/)

      call MPI_Cart_Create(this%id, 2, dims, periods, reorder, new_comm_id, ier)
      ! NOTE: I don't I need to care about ID change with file/IO since flag in memory
      this%id = new_comm_id

      call MPI_Cart_coords(this%id, this%rank_id, n_dims, coords, ier)

      ! print *, 'ID', this%rank_id, 'WR ID', this%io_node_id
      this%iproc = coords(1)
      this%jproc = coords(2)

      ! TODO: Add rank id change check?

      call MPI_Cart_shift(this%id, 0, 1, this%back_rank_id, this%shore_rank_id, ier)
      call MPI_Cart_shift(this%id, 1, 1, this%right_rank_id, this%left_rank_id, ier)

      call grid_range_per_procs(1, nx_global, nx_proc, this%iproc, this%ibegin, this%istop, this%nx)
      call grid_range_per_procs(1, ny_global, ny_proc, this%jproc, this%jbegin, this%jstop, this%ny)

   end subroutine create_2d

   subroutine grid_range_per_procs(i1_global, i2_global, n_procs, rank_id, i1, i2, local_n)

      integer, intent(in) :: i1_global, i2_global, n_procs, rank_id
      integer, intent(out) :: i1, i2, local_n

      integer :: n_min, n_left

      ! Minimum about of points in each domain
      n_min = int((i2_global - i1_global + 1)/n_procs)
      ! Remainder
      n_left = mod(i2_global - i1_global + 1, n_procs)

      ! Adding additional point for first n_left slices
      i1 = rank_id*n_min + i1_global + min(rank_id, n_left)
      i2 = i1 + n_min - 1
      ! Offsetting remaining slices to account for additional point
      if (n_left > rank_id) i2 = i2 + 1
      local_n = (i2 - i1 + 1) + 2*n_ghost

   end subroutine grid_range_per_procs

   subroutine compute_optimal_grid_size(nproc, nx, ny, px, py)

      integer, intent(in) :: nx, ny, nproc
      integer, intent(out) :: px, py

      integer, allocatable :: factors(:)
      integer :: nfactors, i, min_i

      real(SP) :: ratio
      real(SP) :: nx_loc, ny_loc, min_ratio

      call get_factors(nproc, factors, nfactors)

      min_ratio = 9e10_sp

      do i = 1, nfactors

         nx_loc = (1.0_sp*nx)/(1.0_sp*factors(i))
         ny_loc = (1.0_sp*ny)/((1.0_sp*nproc)/(1.0_sp*factors(i)))

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
      integer, intent(in) :: n
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

      ! Temporary array (max possible size = 2*limit)
      allocate (temp(2*limit))
      count = 0

      do i = 1, limit
         if (mod(n, i) == 0) then
            count = count + 1
            temp(count) = i

            ! Adding other factor if not square root
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

   subroutine bcast_integer(this, val)
      class(type_comm), intent(inout) :: this
      integer, intent(inout) :: val

      call MPI_Bcast(val, 1, MPI_INTEGER, this%io_node_id, this%id)
      call this%barrier()

   end subroutine bcast_integer

   subroutine bcast_logical(this, val)
      class(type_comm), intent(inout) :: this
      logical, intent(inout) :: val

      call MPI_Bcast(val, 1, MPI_LOGICAL, this%io_node_id, this%id)
      call this%barrier()

   end subroutine bcast_logical

   subroutine bcast_real(this, val)
      class(type_comm), intent(inout) :: this
      real(SP), intent(inout) :: val

      call MPI_Bcast(val, 1, MPI_DOUBLE_PRECISION, this%io_node_id, this%id)
      call this%barrier()

   end subroutine bcast_real

   subroutine bcast_string(this, val)
      class(type_comm), intent(inout) :: this
      character(:), allocatable, intent(inout) :: val

      integer :: n

      n = len(val)
      call MPI_Bcast(n, 1, MPI_INTEGER, this%io_node_id, this%id)
      call this%barrier()

      if (.not. allocated(val)) then
         allocate (character(n) :: val)
      end if

      call MPI_Bcast(val, n, MPI_CHARACTER, this%io_node_id, this%id)
      call this%barrier()

   end subroutine bcast_string

   subroutine barrier(this)
      class(type_comm), intent(inout) :: this
      integer :: ierr

      call MPI_Barrier(this%id, ierr)

   end subroutine barrier

   subroutine finalize(this)
      class(type_comm), intent(inout) :: this
      integer :: ierr
      call MPI_Finalize(ierr)
   end subroutine finalize
end module core_comm_mod
