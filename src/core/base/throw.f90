
! Dummy module for pfUnit testing of errors

module core_throw_mod
   implicit none
   private

   public :: throw_exception
   public :: set_throw_method
   public :: set_error_code
   ! Exit-code contract (the wrapper's scheduler backends classify a run
   ! by this before reading any log; the JSON status event carries it as
   ! rc).  Anything the engine did not set -- a signal, a runtime crash,
   ! an MPI library error -- is the launcher's or the runtime's code.
   integer, parameter, public :: EXIT_OK = 0      ! completed, --validate passed, -v
   integer, parameter, public :: EXIT_ABORT = 1   ! unclassified abort (internal invariants)
   integer, parameter, public :: EXIT_BLOWUP = 3  ! numerical divergence detected
   integer, parameter, public :: EXIT_DIAG = 4    ! a diagnostics abort: threshold tripped
   integer, parameter, public :: EXIT_IO = 5      ! output I/O failure (netcdf, pnetcdf, a file)
   integer, parameter, public :: EXIT_COMM = 6    ! the comm layer's own MPI checks failed
   integer, parameter, public :: EXIT_USAGE = 64  ! bad flag or argument (sysexits EX_USAGE)
   integer, parameter, public :: EXIT_DECK = 65   ! the deck was refused (sysexits EX_DATAERR)

   abstract interface
      subroutine throw(filename, line_number, message)
         implicit none
         character(len=*), intent(in) :: filename
         integer, intent(in) :: line_number
         character(len=*), optional, intent(in) :: message
      end subroutine throw
   end interface

   procedure(throw), pointer :: throw_method => null()
   logical, save :: initialized = .false.
   integer, save :: error_code = 1
contains

   ! Entry point for PFUnit to intercept exception handling for testing
   subroutine set_throw_method(method)
      procedure(throw) :: method
      if (.not. initialized) call initialize()
      throw_method => method
   end subroutine set_throw_method

   ! Set exception handling to normal behavior
   subroutine initialize()
      throw_method => terminate
      initialized = .true.
   end subroutine initialize

   ! Wrapper method to switch between normal and PFUnit exception handling
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

   ! Hacky bypass to handle error_code while conforming
   ! to function signature of PFUnit
   subroutine set_error_code(err_code)

      integer, INTENT(IN), OPTIONAL :: err_code

      if (PRESENT(err_code)) then
         error_code = err_code
      else
         error_code = 1
      end if

   end subroutine set_error_code

   ! Common method to gracefully exit MPI and FORTRAN
   subroutine terminate(filename, line, message)
      use MPI, only: MPI_Abort, MPI_COMM_WORLD
      use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
      character(*), intent(in) :: filename
      integer, intent(in) :: line
      character(*), optional, intent(in) :: message
      integer :: ierr, fstat

      ! MPI_Abort tears every rank down with no unit finalization, so
      ! anything still buffered on stdout is discarded -- which silently
      ! ate direct writes from NON-IO ranks (the blow-up site line, lost
      ! under srun but not under mpiexec, which line-buffers through the
      ! launcher pipe).  iostat so the abort path itself cannot fault on
      ! an unconnected unit.
      flush (output_unit, iostat=fstat)
      flush (error_unit, iostat=fstat)

      if (error_code .eq. 1) then
         call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
         error stop
      else
         call MPI_Abort(MPI_COMM_WORLD, error_code, ierr)
         stop error_code
      end if

   end subroutine terminate

end module core_throw_mod
