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

module comm_mod

   use MPI
   use Constants, only: n_ghost, LABEL_SIZE
   use log_io_mod, only: type_log_writer

   implicit none !(external)

   character(LABEL_SIZE), parameter :: log_key = "MPI"

   private
   public :: type_comm

   type type_comm

      private

      integer, public :: size
      integer, public :: comm_id, rank_id
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

      logical :: p_is_io_node
      integer :: io_node_id
      type(type_log_writer) :: log
   contains
      procedure, public :: create_2d
      procedure, public :: init
      procedure, public :: get_logger
      procedure, public :: is_io_node

      procedure, public :: finalize
      procedure, public :: barrier
   end type type_comm

contains

   subroutine init(this, io_rank_id, comm_id)

      class(type_comm), intent(inout) :: this
      integer, intent(in) :: io_rank_id
      integer, intent(in), optional :: comm_id

      integer :: ierr
      ! TODO: Add error checking
      !
      if (present(comm_id)) then
         this%comm_id = comm_id
      else
         this%comm_id = MPI_COMM_WORLD
         call MPI_Init(ierr)
      end if
      call MPI_Comm_rank(this%comm_id, this%rank_id, ierr)
      call MPI_Comm_size(this%comm_id, this%size, ierr)

      this%p_is_io_node = this%rank_id .eq. io_rank_id
      this%io_node_id = io_rank_id

      call this%log%get(log_key, this%comm_id, this%p_is_io_node)

   end subroutine init

   function get_logger(this, label) result(logger)

      class(type_comm), intent(inout) :: this
      character(LABEL_SIZE), intent(in) :: label
      type(type_log_writer) :: logger

      call logger%get(label, this%comm_id, this%p_is_io_node)

   end function get_logger

   function is_io_node(this) result(flag)
      class(type_comm), intent(inout) :: this
      logical :: flag
      flag = this%p_is_io_node
   end function is_io_node

   subroutine create_2d(this, nx_proc, ny_proc, nx_global, ny_global)

      class(type_comm), intent(inout) :: this
      integer, intent(in) :: nx_global, ny_global
      integer, intent(in) :: nx_proc, ny_proc
      integer, parameter  :: n_dims = 2
      integer, dimension(n_dims) :: dims, coords
      logical, dimension(n_dims)  :: periods
      integer :: new_comm_id, old_comm_id, ier, old_rank_id
      logical :: reorder
      integer :: io

      CHARACTER(*), PARAMETER :: tmp_file = '.new_reorder_comm_id'

      reorder = .true.
      coords = (/0, 0/)
      periods = (/.false., .false./)
      dims = (/nx_proc, ny_proc/)

      call MPI_Cart_Create(this%comm_id, n_dims, dims, periods, reorder, new_comm_id, ier)
      ! NOTE: I don't I need to care about ID change with file/IO since flag in memory
      this%comm_id = new_comm_id

      call MPI_Cart_coords(this%comm_id, this%rank_id, n_dims, coords, ier)

      ! print *, 'ID', this%rank_id, 'WR ID', this%io_node_id
      this%iproc = coords(1)
      this%jproc = coords(2)

      ! TODO: Add rank id change check?

      call MPI_Cart_shift(this%comm_id, 0, 1, this%back_rank_id, this%shore_rank_id, ier)
      call MPI_Cart_shift(this%comm_id, 1, 1, this%right_rank_id, this%left_rank_id, ier)

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

   subroutine barrier(this)
      class(type_comm), intent(inout) :: this
      integer :: ierr
      call MPI_Barrier(this%comm_id, ierr)
   end subroutine barrier

   subroutine finalize(this)
      class(type_comm), intent(inout) :: this
      integer :: ierr
      call MPI_Finalize(ierr)
   end subroutine finalize
end module comm_mod

