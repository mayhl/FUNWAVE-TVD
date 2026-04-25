
! Dummy module for pfUnit testing of errors

module core_throw_mod
   implicit none(external)
   private

   public :: throw_exception
   public :: set_throw_method
   public :: set_error_code

   abstract interface
      subroutine throw(filename, line_number, message)
         implicit none(external)
         character(len=*), intent(in) :: filename
         integer, intent(in) :: line_number
         character(len=*), optional, intent(in) :: message
      end subroutine throw
   end interface

   procedure(throw), pointer :: throw_method => null()
   logical, save :: initialized = .false.
   integer, save :: error_code = 1
contains

   subroutine set_throw_method(method)
      procedure(throw) :: method
      if (.not. initialized) call initialize()
      throw_method => method
   end subroutine set_throw_method

   subroutine initialize()
      throw_method => terminate
      initialized = .true.
   end subroutine initialize

   subroutine throw_exception(filename, line_number, message, errcode, comm_id)
      character(len=*), intent(in) :: filename
      integer, intent(in) :: line_number
      character(len=*), optional, intent(in) :: message
      integer, optional, intent(in) :: errcode, comm_id

      if (.not. initialized) then
         call initialize()
      end if

      call throw_method(filename, line_number, message=message)

   end subroutine throw_exception

   subroutine set_error_code(err_code)

      integer, INTENT(IN), OPTIONAL :: err_code

      if (PRESENT(err_code)) then
         error_code = err_code
      else
         error_code = 1
      end if

   end subroutine set_error_code

   subroutine terminate(filename, line, message)

      use MPI, only: MPI_Abort, MPI_COMM_WORLD
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      character(*), optional, intent(in) :: message
      integer :: ierr

      if (error_code .eq. 1) then
         call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
         error stop
      else
         call MPI_Abort(MPI_COMM_WORLD, error_code, ierr)
         stop error_code
      end if

   end subroutine terminate

end module core_throw_mod
