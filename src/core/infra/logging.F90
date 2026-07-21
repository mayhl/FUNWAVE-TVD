!> @brief Module for platform-independent logging.
!! Replaces external flogging dependency with a native implementation.
module core_log_io_mod
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   use core_throw_mod, only: throw_exception, set_error_code
   implicit none
   private

   ! ANSI escape character for terminal coloring
   character(len=1), parameter :: ESC = achar(27)

   public :: new_log_writer, format_log_line

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
      procedure, public :: debug, info, warning => warn, exit_on_error, exit_on_fatal, finalize => log_writer_finalize
      procedure, public :: set_levels => log_set_levels
      procedure, public :: set_file => log_set_file
      procedure, private :: write_log
   end type type_log_writer

   interface new_log_writer
      module procedure type_log_writer_initialize
   end interface new_log_writer

contains

   function type_log_writer_initialize(label, is_io_node, path, std_err_threshold, &
                                       std_out_threshold, logfile_threshold) result(this)
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
      open (newunit=this%file_unit, file=trim(filename), status="replace", action="write", iostat=stat)
   end subroutine log_set_file

   subroutine debug(this, message)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      call this%write_log(log_level_debug, "DEBUG", message)
   end subroutine debug

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
      if (present(errcode)) call set_error_code(errcode)
      if (this%is_io_node) call throw_exception(__FILE__, __LINE__, message=message)
   end subroutine exit_on_error

   subroutine exit_on_fatal(this, message, errcode)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: errcode
      call this%write_log(log_level_fatal, "FATAL", message)
      if (present(errcode)) call set_error_code(errcode)
      if (this%is_io_node) call throw_exception(__FILE__, __LINE__, message=message)
   end subroutine exit_on_fatal

   subroutine write_log(this, level, prefix, msg)
      class(type_log_writer), intent(in) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: prefix, msg

      character(len=20) :: date, time
      character(len=8)  :: zone
      character(len=19) :: timestamp
      character(len=30) :: colored_prefix

      if (.not. this%is_io_node) return

      ! Get timestamp
      call date_and_time(date, time, zone)
      timestamp = date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//time(1:2)//":"//time(3:4)//":"//time(5:6)

      ! Apply ANSI colors
      select case (level)
      case (log_level_info); colored_prefix = colorize(prefix, color_fg="green")
      case (log_level_debug); colored_prefix = colorize(prefix, color_fg="blue")
      case (log_level_warn); colored_prefix = colorize(prefix, color_fg="yellow")
      case (log_level_error); colored_prefix = colorize(prefix, color_fg="red")
      case (log_level_fatal); colored_prefix = colorize(prefix, color_fg="red", style="inverse_on")
      case default; colored_prefix = prefix
      end select

      write (output_unit, "(A)") trim(format_log_line(this, timestamp, colored_prefix, msg))
      if (this%file_unit /= -1) write (this%file_unit, *) trim(format_log_line(this, timestamp, prefix, msg))
   end subroutine write_log

   !> @brief Wrap text in ANSI SGR escape codes (ECMA-48).
   !! Inline replacement for the external FACE dependency; supports only the
   !! foreground colors and inverse style used by write_log (codes: 31 red,
   !! 32 green, 33 yellow, 34 blue; 7 inverse; 0 reset).
   function colorize(string, color_fg, style) result(colorized)
      character(len=*), intent(in) :: string
      character(len=*), intent(in), optional :: color_fg, style
      character(len=:), allocatable :: colorized
      character(len=:), allocatable :: codes

      codes = ""
      if (present(color_fg)) then
         select case (color_fg)
         case ("red"); codes = "31"
         case ("green"); codes = "32"
         case ("yellow"); codes = "33"
         case ("blue"); codes = "34"
         end select
      end if
      if (present(style)) then
         if (style == "inverse_on") then
            if (len(codes) > 0) codes = codes//";"
            codes = codes//"7"
         end if
      end if

      if (len(codes) == 0) then
         colorized = string
      else
         colorized = ESC//"["//codes//"m"//string//ESC//"[0m"
      end if
   end function colorize

   !> @brief Format log line
  !! Format: YYYY-MM-DD HH:MM:SS [LABEL] PREFIX: MSG
   function format_log_line(this, timestamp, prefix, msg) result(formatted)
      class(type_log_writer), intent(in) :: this
      character(len=*), intent(in) :: timestamp, prefix, msg
      character(len=:), allocatable :: formatted
      formatted = trim(timestamp)//" ["//trim(this%label)//"] "//trim(prefix)//": "//trim(msg)
   end function format_log_line

   subroutine log_writer_finalize(this)
      class(type_log_writer), intent(inout) :: this
      if (this%file_unit /= -1) close (this%file_unit)
   end subroutine log_writer_finalize

end module core_log_io_mod
