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
   use core_constants_mod, only: LABEL_SIZE, SP, MPI_SP, type_string
   use core_log_io_mod, only: type_log_writer, new_log_writer

   implicit none

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

   contains
      procedure, public :: get_logger
      procedure, public :: is_io_node
      procedure, public :: bcast_integer
      procedure, public :: bcast_logical
      procedure, public :: bcast_real
      procedure, public :: bcast_string => bcast_string
      procedure, public :: bcast_string_array => bcast_string_array
      procedure, public :: bcast_integer_array => bcast_integer_array
      procedure, public :: bcast_real_array => bcast_real_array

      generic, public :: bcast => bcast_integer, bcast_logical, bcast_real, &
         bcast_string, bcast_integer_array, bcast_real_array, bcast_string_array

      procedure, public :: finalize
      procedure, public :: get_io_rank

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

   function get_io_rank(this) result(r)
      class(type_comm), intent(in) :: this
      integer :: r
      r = this%io_node_id
   end function get_io_rank

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

      call MPI_Bcast(val, 1, MPI_SP, this%io_node_id, this%id)
      call this%barrier()

   end subroutine bcast_real

   subroutine bcast_string(this, val)
      class(type_comm), intent(inout) :: this
      character(:), allocatable, intent(inout) :: val

      integer :: n

      if (this%is_io_node()) then
         if (allocated(val)) then
            n = len(val)
         else
            n = -1
         end if
      end if
      call MPI_Bcast(n, 1, MPI_INTEGER, this%io_node_id, this%id)
      call this%barrier()

      if (n >= 0) then
         if (.not. this%is_io_node()) then
            if (allocated(val)) deallocate (val)
            allocate (character(n) :: val)
         end if
         call MPI_Bcast(val, n, MPI_CHARACTER, this%io_node_id, this%id)
      else
         if (allocated(val)) deallocate (val)
      end if
      call this%barrier()

   end subroutine bcast_string

   subroutine barrier(this)
      class(type_comm), intent(inout) :: this
      integer :: ierr

      call MPI_Barrier(this%id, ierr)

   end subroutine barrier

   subroutine finalize(this)
      ! MPI_Finalize is intentionally NOT called here — only main() may finalize MPI.
      class(type_comm), intent(inout) :: this
   end subroutine finalize

   subroutine bcast_integer_array(this, val)
      class(type_comm), intent(inout) :: this
      integer, allocatable, intent(inout), dimension(:) :: val
      integer :: n, ierr
      if (this%is_io_node()) n = size(val)
      call MPI_Bcast(n, 1, MPI_INTEGER, this%io_node_id, this%id, ierr)
      call this%barrier()
      if (.not. this%is_io_node()) then
         if (allocated(val)) deallocate (val)
         allocate (val(n))
      end if
      call MPI_Bcast(val, n, MPI_INTEGER, this%io_node_id, this%id, ierr)
      call this%barrier()
   end subroutine bcast_integer_array

   subroutine bcast_real_array(this, val)
      class(type_comm), intent(inout) :: this
      real(SP), allocatable, intent(inout), dimension(:) :: val
      integer :: n, ierr
      if (this%is_io_node()) n = size(val)
      call MPI_Bcast(n, 1, MPI_INTEGER, this%io_node_id, this%id, ierr)
      call this%barrier()
      if (.not. this%is_io_node()) then
         if (allocated(val)) deallocate (val)
         allocate (val(n))
      end if
      call MPI_Bcast(val, n, MPI_SP, this%io_node_id, this%id, ierr)
      call this%barrier()
   end subroutine bcast_real_array

   subroutine bcast_string_array(this, list)

      class(type_comm), intent(inout) :: this
      type(type_string), allocatable, intent(inout), dimension(:) :: list

      integer :: n, i, total_len, ierr
      integer, allocatable :: lengths(:)
      character(:), allocatable :: buffer

      if (this%is_io_node()) n = size(list)
      call this%bcast_integer(n)
      allocate (lengths(n))
      if (this%is_io_node()) then
         total_len = 0
         do i = 1, n
            lengths(i) = len(list(i)%s)
            total_len = total_len + lengths(i)
         end do
      end if

      call this%bcast_integer(total_len)
      call this%bcast_integer_array(lengths)

      allocate (character(len=total_len) :: buffer)

      ! Concatenating array of strings to single string
      if (this%is_io_node()) then
         total_len = 1
         do i = 1, n
            buffer(total_len:total_len + lengths(i) - 1) = list(i)%s
            total_len = total_len + lengths(i)
         end do
      end if

      call this%bcast_string(buffer)

      ! Reconstructing array of strings
      if (.not. this%is_io_node()) then
         if (allocated(list)) deallocate (list)
         allocate (list(n))

         total_len = 1
         do i = 1, n
            allocate (character(len=lengths(i)) :: list(i)%s)
            list(i)%s = buffer(total_len:total_len + lengths(i) - 1)
            total_len = total_len + lengths(i)
         end do

      end if

      deallocate (lengths, buffer)

   end subroutine bcast_string_array

end module core_comm_mod
