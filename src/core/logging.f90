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

module core_log_io_mod
   use core_constants_mod, only: MESSAGE_SIZE, STRING_SIZE, LABEL_SIZE
   use logger_mod, only: logger_init => logger_init, logger => master_logger
   use core_throw_mod, only: throw_exception, set_error_code

   implicit none(external)

   private

   public :: type_log_writer, new_log_writer, finalize_logger

   type type_log_writer
      private
      CHARACTER(LABEL_SIZE) :: label
      logical :: is_io_node = .false.
   contains
      private
      procedure, public :: debug
      procedure, public :: trivia
      procedure, public :: info
      procedure, public :: warning
      procedure, public :: exit_on_error
      procedure, public :: exit_on_fatal
      procedure, public :: finalize => log_writer_finalize
   end type type_log_writer

   interface new_log_writer
      module procedure type_log_writer_initialize
   end interface new_log_writer

contains

   function type_log_writer_initialize(label, is_io_node, path, std_err_threshold, std_out_threshold, logfile_threshold) result(this)
      character(*), intent(in) :: label
      logical, intent(in) :: is_io_node
      character(*), intent(in), optional ::  path
      integer, optional, intent(in) :: std_err_threshold
      integer, optional, intent(in) :: std_out_threshold
      integer, optional, intent(in) :: logfile_threshold

      type(type_log_writer) :: this

      character(*), parameter:: default_log_path = "new.log"
      character(len=:), allocatable :: log_path

      if (present(path)) then
         log_path = path
         call logger_init(trim(log_path), &
                          stderr_threshold=std_err_threshold, &
                          stdout_threshold=std_out_threshold, &
                          logfile_threshold=logfile_threshold)
      end if

      this%is_io_node = is_io_node
      this%label = label

   end function type_log_writer_initialize

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

   subroutine log_writer_finalize(this)
      class(type_log_writer), intent(inout) :: this
      call finalize_logger()
   end subroutine log_writer_finalize

   subroutine finalize_logger()
      call logger%destroy()
   end subroutine finalize_logger

end module core_log_io_mod
