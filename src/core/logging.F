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

module log_io_mod
   use Constants, only: MESSAGE_SIZE, STRING_SIZE, LABEL_SIZE
   use logger_mod, only: logger_init => logger_init, logger => master_logger
   use throw_mod, only: throw_exception, set_error_code

   implicit none(external)

   private

   public :: type_log_writer, initialize_logger

   type type_log_writer

      private
      character(MESSAGE_SIZE) :: path = ''
      CHARACTER(LABEL_SIZE) :: label
      !character(*), pointer :: path => null()
      integer :: world_comm = -1
      logical :: is_io_node = .false.
   contains
      private
      procedure, public :: debug
      procedure, public :: info
      procedure, public :: warning
      procedure, public :: exit_on_error
      procedure, public :: exit_on_fatal

      procedure, public:: init
      procedure, public:: get

   end type type_log_writer

contains

   subroutine initialize_logger(path, std_err_threshold, std_out_threshold, logfile_threshold)
      ! Wrapper method to to remove direct dependency on logger_mod
      character(:), allocatable, optional, intent(in) ::  path
      integer, optional, intent(in) :: std_err_threshold
      integer, optional, intent(in) :: std_out_threshold
      integer, optional, intent(in) :: logfile_threshold

      character(*), parameter:: default_log_path = "new.log"
      character(len=:), allocatable :: log_path

      if (.not. PRESENT(path)) then
         log_path = default_log_path
      else
         log_path = path
      end if

      call logger_init(trim(log_path), &
                       stderr_threshold=std_err_threshold, &
                       stdout_threshold=std_out_threshold, &
                       logfile_threshold=logfile_threshold)

   end subroutine initialize_logger

   subroutine init(this, label, is_io_node)
      class(type_log_writer), intent(inout) :: this
      character(LABEL_SIZE), intent(in) :: label
      !integer, intent(in) :: world_comm
      logical, intent(in) :: is_io_node

      !this%world_comm = world_comm
      this%is_io_node = is_io_node
      this%label = label

   end subroutine init

   subroutine get(this, label, is_io_node)
      class(type_log_writer), intent(inout) :: this
      character(LABEL_SIZE), intent(in) :: label
      !integer, intent(in) :: world_comm
      logical, intent(in) :: is_io_node

      !this%world_comm = world_comm
      this%is_io_node = is_io_node
      this%label = label

   end subroutine get

   ! Log debugging info and continue run
   subroutine debug(this, message)

      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message

      if (this%is_io_node) then
         call logger%debug(trim(this%label), trim(message))
      end if
   end subroutine debug

   ! Log trivia and continue run
   subroutine trivia(this, message)

      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message

      if (this%is_io_node) then
         call logger%trivia(trim(this%label), trim(message))
      end if
   end subroutine trivia

   ! Log info and continue run
   subroutine info(this, message)

      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message

      if (this%is_io_node) then
         call logger%info(trim(this%label), trim(message))
      end if
   end subroutine info

   ! Log warning and continue run
   subroutine warning(this, message)

      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message

      if (this%is_io_node) then
         call logger%warning(trim(this%label), trim(message))
      end if

   end subroutine warning

   ! Log error and exit run
   subroutine exit_on_error(this, message, errcode)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: errcode

      if (this%is_io_node) then
         call logger%error(trim(this%label), trim(message))
         call set_error_code(errcode)
         call throw_exception(__FILE__, __LINE__, message=message)
      end if

   end subroutine exit_on_error

   ! Log fatal error and exit run
   subroutine exit_on_fatal(this, message, errcode)

      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: errcode

      if (this%is_io_node) then
         call logger%fatal(trim(this%label), trim(message))
         call throw_exception(__FILE__, __LINE__, message=message)
      end if

   end subroutine exit_on_fatal

end module log_io_mod
