!> @brief Module for platform-independent logging.
!! Replaces external flogging dependency with a native implementation.
module core_log_io_mod
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   use face, only: color_text, COLOR_RED, COLOR_YELLOW, COLOR_BLUE, COLOR_RESET
   implicit none
   private

   public :: type_log_writer, new_log_writer

   !> @brief Log levels
   integer, parameter, public :: log_level_debug = 1
   integer, parameter, public :: log_level_info = 2
   integer, parameter, public :: log_level_warn = 3
   integer, parameter, public :: log_level_error = 4
   integer, parameter, public :: log_level_fatal = 5

   type, public :: type_log_writer
      private
      character(len=20) :: label
      logical :: is_io_node = .false.
      integer :: min_stdout_level = log_level_info
      integer :: min_stderr_level = log_level_error
      integer :: file_unit = -1
   contains
      procedure, public :: debug, trivia, info, warning => warn, exit_on_error, exit_on_fatal, finalize => log_writer_finalize
      procedure, public :: set_levels => log_set_levels
      procedure, public :: set_file => log_set_file
      procedure, private :: write_log
   end type type_log_writer

   interface new_log_writer
      module procedure type_log_writer_initialize
   end interface new_log_writer

contains

  function type_log_writer_initialize(label, is_io_node, path, std_err_threshold, std_out_threshold, logfile_threshold) result(this)
      character(*), intent(in) :: label
      logical, intent(in) :: is_io_node
      character(*), intent(in), optional :: path
      integer, optional, intent(in) :: std_err_threshold, std_out_threshold, logfile_threshold
      type(type_log_writer) :: this

      this%label = label
      this%is_io_node = is_io_node
      if (present(std_out_threshold)) this%min_stdout_level = std_out_threshold
      if (present(std_err_threshold)) this%min_stderr_level = std_err_threshold
      if (present(path)) call this%set_file(path)
   end function type_log_writer_initialize

   subroutine log_set_levels(this, stdout_level, stderr_level)
      class(type_log_writer), intent(inout) :: this
      integer, intent(in), optional :: stdout_level, stderr_level
      if (present(stdout_level)) this%min_stdout_level = stdout_level
      if (present(stderr_level)) this%min_stderr_level = stderr_level
   end subroutine log_set_levels

   subroutine log_set_file(this, filename)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: filename
      integer :: stat
      if (this%file_unit /= -1) close (this%file_unit)
      open (newunit=this%file_unit, file=trim(filename), status='replace', action='write', iostat=stat)
   end subroutine log_set_file

   subroutine debug(this, message)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      call this%write_log(log_level_debug, "DEBUG", message)
   end subroutine debug

   subroutine trivia(this, message)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      call this%write_log(log_level_debug, "TRIVIA", message)
   end subroutine trivia

   subroutine info(this, message)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      call this%write_log(log_level_info, "INFO", message)
   end subroutine info

   subroutine warn(this, message)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      call this%write_log(log_level_warn, "WARN", message)
   end subroutine warn

   subroutine exit_on_error(this, message, errcode)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: errcode
      call this%write_log(log_level_error, "ERROR", message)
      stop 1
   end subroutine exit_on_error

   subroutine exit_on_fatal(this, message, errcode)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: errcode
      call this%write_log(log_level_fatal, "FATAL", message)
      stop 1
   end subroutine exit_on_fatal

   subroutine write_log(this, level, prefix, msg)
      class(type_log_writer), intent(in) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: prefix, msg

      character(len=20) :: date, time
      character(len=8)  :: zone
      character(len=100) :: colored_prefix

      if (.not. this%is_io_node) return

      ! Get timestamp
      call date_and_time(date, time, zone)

      ! Apply colors using FACE
      select case (level)
      case (log_level_debug); colored_prefix = color_text(prefix, COLOR_BLUE)
      case (log_level_warn); colored_prefix = color_text(prefix, COLOR_YELLOW)
      case (log_level_error, log_level_fatal); colored_prefix = color_text(prefix, COLOR_RED)
      case default; colored_prefix = prefix
      end select

      ! Formatting: [YYYY-MM-DD HH:MM:SS] [LABEL] [PREFIX] MSG
      write(output_unit, '(A)') "["//date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//time(1:2)//":"//time(3:4)//":"//time(5:6)//"] ["//trim(this%label)//"] ["//trim(colored_prefix)//"] "//trim(msg)
      if (this%file_unit /= -1) write(this%file_unit, *) "["//date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//time(1:2)//":"//time(3:4)//":"//time(5:6)//"] ["//trim(this%label)//"] ["//trim(prefix)//"] "//trim(msg)
   end subroutine write_log

   subroutine log_writer_finalize(this)
      class(type_log_writer), intent(inout) :: this
      if (this%file_unit /= -1) close (this%file_unit)
   end subroutine log_writer_finalize

end module core_log_io_mod
