!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Class for handling pretty logging
!  https://github.com/BoldingBruggeman/fortran-yaml
!
!  PURPOSE:
!   - interface with external library
!   - handle code termination on errors
!
!  Authors:
!   [mayhl] Michael-Angelo Y.-H. Lam
!
!  HISTORY:
!    11/23/2025 [mayhl] created initial class
!
!-------------------------------------------------

module LogIO

   use logger_mod, only: logger_init, logger => master_logger

   private
   public :: TypeLogWriter, constructor

   type TypeLogWriter

      private
      logical :: is_io_node
      integer :: world_comm
      character(:), allocatable :: name

   contains
      private
      procedure, nopass, public :: debug => debug
      procedure, nopass, public :: info
      procedure, nopass, public :: warning
      procedure, nopass, public :: errorAndTerminate
      procedure, nopass, public :: fatalAndTerminate

      procedure, nopass, public :: terminate => terminate

   end type TypeLogWriter

contains

   ! ---------------------------------
   ! Wrappers around flogging routines
   ! with termination on error/fatal
   ! ---------------------------------

   ! Log debugging info and continue run
   subroutine debug(self, message)

      type(TypeLogWriter), intent(inout) :: self
      character(len=*), intent(in) :: message

      if (self%is_io_node) then
         call logger%debug(self%name, message)
      end if
   end subroutine debug

   ! Log info and continue run
   subroutine info(self, message)

      type(TypeLogWriter), intent(inout) :: self
      character(len=*), intent(in) :: message

      if (self%is_io_node) then
         call logger%info(self%name, message)
      end if
   end subroutine info

   ! Log warning and continue run
   subroutine warning(self, message)

      type(TypeLogWriter), intent(inout) :: self
      character(len=*), intent(in) :: message

      if (self%is_io_node) then
         call logger%warning(self%name, message)
      end if

   end subroutine warning

   ! Log error and exit run
   subroutine errorAndTerminate(self, message, errcode)
      type(TypeLogWriter), intent(inout) :: self
      character(len=*), intent(in) :: message
      integer, intent(in) :: errcode

      if (self%is_io_node) then
         call logger%error(self%name, message)
      end if

      call terminate(self, errcode)

   end subroutine errorAndTerminate

   ! Log fatal error and exit run
   subroutine fatalAndTerminate(self, message, errcode)

      type(TypeLogWriter), intent(inout) :: self
      character(len=*), intent(in) :: message
      integer, intent(in) :: errcode

      if (self%is_io_node) then
         call logger%fatal(self%name, message)
      end if

      call terminate(self, errcode)

   end subroutine fatalAndTerminate

   ! Wrapper method of standardize code termination
   subroutine terminate(self, errcode)

      use MPI, only: MPI_Abort

      type(TypeLogWriter), intent(inout) :: self
      integer, intent(in) :: errcode
      integer :: ierr

      call MPI_Abort(self%world_comm, errcode, ierr)
      stop errcode

   end subroutine terminate

   function constructor(log, path, name, world_comm, is_io_node)

      type(TypeLogWriter), intent(inout) :: log
      character(len=*), intent(in) :: path, name
      logical, intent(in) :: is_io_node

      log%is_io_node = is_io_node

      call logger_init(path)

   end function constructor
end module LogIO
