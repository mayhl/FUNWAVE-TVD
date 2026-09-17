!> @brief Module for platform-independent logging.
!! Replaces external flogging dependency with a native implementation.
module core_log_io_mod
   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   use core_throw_mod, only: throw_exception, set_error_code, EXIT_ABORT, EXIT_DECK, EXIT_IO
   implicit none
   private

   ! ANSI escape character for terminal coloring
   character(len=1), parameter :: ESC = achar(27)

   public :: new_log_writer, format_log_line, format_json_line, set_default_log_levels
   public :: set_default_log_format, log_line, json_escape, fail, open_out

   !> @brief Log levels
   integer, parameter, public :: log_level_debug = 1
   integer, parameter, public :: log_level_info = 2
   integer, parameter, public :: log_level_warn = 3
   integer, parameter, public :: log_level_error = 4
   integer, parameter, public :: log_level_fatal = 5
   ! threshold above every level: a sink set to off never writes
   integer, parameter, public :: log_level_off = 6

   ! Console format: text (default) or JSON Lines -- one object per line
   ! {ts, kind, label, level, msg[, fields]}, the shape the funtools
   ! porcelain emitter speaks, so one consumer reads both.  Opt-in only
   ! (--log-format jsonl, or MU_WRAP=1 from the Go shim); the log FILE
   ! stays text either way -- it is the human record, the stream the
   ! machine one.  kinds: log (any message), phase (init/run/exit
   ! transitions), progress (the step line with step/t/dt as fields),
   ! status (the exit reason with rc).
   integer, parameter, public :: log_format_text = 1
   integer, parameter, public :: log_format_jsonl = 2

   ! Process-wide state new writers inherit: the CLI verbosity flags set the
   ! default levels once (before any writer exists), and the env's writer
   ! publishes its log file so later writers (e.g. the yaml [config] logger)
   ! share the sink instead of losing their lines
   integer, save :: default_stdout_level = log_level_info
   integer, save :: default_stderr_level = log_level_error
   integer, save :: default_file_level = log_level_info
   integer, save :: shared_file_unit = -1
   integer, save :: default_log_format = log_format_text

   type, public :: type_log_writer
      private
      character(len=20) :: label
      logical :: is_io_node = .false.
      integer :: min_stdout_level = log_level_info
      integer :: min_stderr_level = log_level_error
      integer :: min_file_level = log_level_info
      integer :: file_unit = -1
      ! an inherited (shared) unit is closed by its owner only
      logical :: owns_file = .false.
   contains
      procedure, public :: debug, info, warning => warn, exit_on_error, exit_on_fatal, finalize => log_writer_finalize
      procedure, public :: event
      procedure, public :: set_levels => log_set_levels
      procedure, public :: set_file => log_set_file
      procedure, private :: write_log, status_event
   end type type_log_writer

   interface new_log_writer
      module procedure type_log_writer_initialize
   end interface new_log_writer

contains

   function type_log_writer_initialize(label, is_io_node, path, std_err_threshold, &
                                       std_out_threshold, logfile_threshold, &
                                       share_file) result(this)
      character(*), intent(in) :: label
      logical, intent(in) :: is_io_node
      character(*), intent(in), optional :: path
      integer, optional, intent(in) :: std_err_threshold, std_out_threshold, logfile_threshold
      logical, intent(in), optional :: share_file
      type(type_log_writer) :: this

      this%label = label
      this%is_io_node = is_io_node
      this%min_stdout_level = default_stdout_level
      this%min_stderr_level = default_stderr_level
      this%min_file_level = default_file_level
      if (present(std_out_threshold)) this%min_stdout_level = std_out_threshold
      if (present(std_err_threshold)) this%min_stderr_level = std_err_threshold
      if (present(logfile_threshold)) this%min_file_level = logfile_threshold
      ! no file of its own -> write into the published process log (if any)
      this%file_unit = shared_file_unit
      if (present(path)) call this%set_file(path)
      if (present(share_file)) then
         if (share_file) shared_file_unit = this%file_unit
      end if
   end function type_log_writer_initialize

   ! Process-wide defaults for writers created AFTER this call; the CLI
   ! flags run this once before new_env creates the first writer
   subroutine set_default_log_format(fmt)
      integer, intent(in) :: fmt
      default_log_format = fmt
   end subroutine set_default_log_format

   subroutine set_default_log_levels(stdout_level, stderr_level, file_level)
      integer, intent(in), optional :: stdout_level, stderr_level, file_level
      if (present(stdout_level)) default_stdout_level = stdout_level
      if (present(stderr_level)) default_stderr_level = stderr_level
      if (present(file_level)) default_file_level = file_level
   end subroutine set_default_log_levels

   subroutine log_set_levels(this, stdout_level, stderr_level, file_level)
      class(type_log_writer), intent(inout) :: this
      integer, intent(in), optional :: stdout_level, stderr_level, file_level
      if (present(stdout_level)) this%min_stdout_level = stdout_level
      if (present(stderr_level)) this%min_stderr_level = stderr_level
      if (present(file_level)) this%min_file_level = file_level
   end subroutine log_set_levels

   subroutine log_set_file(this, filename)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: filename
      integer :: stat
      if (this%owns_file .and. this%file_unit /= -1) close (this%file_unit)
      open (newunit=this%file_unit, file=trim(filename), status="replace", action="write", iostat=stat)
      this%owns_file = .true.
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
      integer :: rc
      ! nearly every caller is a config-time refusal; the runtime sites
      ! (blow-up, diagnostics abort) pass their own code
      rc = EXIT_DECK
      if (present(errcode)) rc = errcode
      call this%write_log(log_level_error, "ERROR", message)
      call this%status_event(log_level_error, "ERROR", message, rc)
      ! the abort below skips normal unit finalization -- flush, or the log
      ! file ends empty exactly when it matters
      if (this%file_unit /= -1) flush (this%file_unit)
      call set_error_code(rc)
      if (this%is_io_node) call throw_exception(__FILE__, __LINE__, message=message)
   end subroutine exit_on_error

   subroutine exit_on_fatal(this, message, errcode)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: message
      integer, optional, intent(in) :: errcode
      integer :: rc
      rc = EXIT_ABORT
      if (present(errcode)) rc = errcode
      call this%write_log(log_level_fatal, "FATAL", message)
      call this%status_event(log_level_fatal, "FATAL", message, rc)
      if (this%file_unit /= -1) flush (this%file_unit)
      call set_error_code(rc)
      if (this%is_io_node) call throw_exception(__FILE__, __LINE__, message=message)
   end subroutine exit_on_fatal

   subroutine write_log(this, level, prefix, msg, kind, extra)
      class(type_log_writer), intent(in) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: prefix, msg
      character(len=*), intent(in), optional :: kind, extra

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

      ! Per-sink thresholds; a quiet console (stdout off) still surfaces
      ! errors on stderr so a failing batch run is never silent
      if (level >= this%min_stdout_level) then
         write (output_unit, "(A)") trim(console_line(this, timestamp, colored_prefix, prefix, msg, kind, extra))
      else if (level >= this%min_stderr_level) then
         write (error_unit, "(A)") trim(console_line(this, timestamp, colored_prefix, prefix, msg, kind, extra))
      end if
      if (this%file_unit /= -1 .and. level >= this%min_file_level) &
         write (this%file_unit, *) trim(format_log_line(this, timestamp, prefix, msg))
   end subroutine write_log

   ! the console line in the process format: coloured text, or one JSON object
   function console_line(this, timestamp, colored_prefix, prefix, msg, kind, extra) result(line)
      class(type_log_writer), intent(in) :: this
      character(len=*), intent(in) :: timestamp, colored_prefix, prefix, msg
      character(len=*), intent(in), optional :: kind, extra
      character(len=:), allocatable :: line
      if (default_log_format == log_format_jsonl) then
         line = format_json_line(this%label, timestamp, kind, prefix, msg, extra)
      else
         line = format_log_line(this, timestamp, colored_prefix, msg)
      end if
   end function console_line

   ! A typed event: kind (phase | progress | status | log), the message the
   ! text format prints unchanged, and an optional JSON fragment of extra
   ! fields ('"step":470,"t":20.0') the JSON format appends.  Text logs
   ! stay byte-identical to a plain info() call.
   subroutine event(this, kind, message, extra, level)
      class(type_log_writer), intent(inout) :: this
      character(len=*), intent(in) :: kind, message
      character(len=*), intent(in), optional :: extra
      integer, intent(in), optional :: level
      integer :: lvl
      lvl = log_level_info
      if (present(level)) lvl = level
      select case (lvl)
      case (log_level_debug); call this%write_log(lvl, "DEBUG", message, kind, extra)
      case (log_level_warn); call this%write_log(lvl, "WARN", message, kind, extra)
      case (log_level_error); call this%write_log(lvl, "ERROR", message, kind, extra)
      case default; call this%write_log(lvl, "INFO", message, kind, extra)
      end select
   end subroutine event

   ! JSON format only: the exit status event behind an error, rc = the
   ! process exit code
   subroutine status_event(this, level, prefix, message, rc)
      class(type_log_writer), intent(inout) :: this
      integer, intent(in) :: level
      character(len=*), intent(in) :: prefix, message
      integer, intent(in) :: rc
      character(len=32) :: rc_s
      if (default_log_format /= log_format_jsonl) return
      write (rc_s, '(a,i0)') '"rc":', rc
      call this%write_log(level, prefix, message, "status", trim(rc_s))
   end subroutine status_event

   ! Abort from a site without a writer with a contract code: the message
   ! as an ERROR line (and the status event on a JSON console), then the
   ! same MPI-wide termination exit_on_error takes.  Replaces the bare
   ! error stops, which exit 1 and skip the status event.
   subroutine fail(msg, code, label)
      character(len=*), intent(in) :: msg
      integer, intent(in) :: code
      character(len=*), intent(in), optional :: label
      character(len=20) :: date, time
      character(len=8)  :: zone
      character(len=19) :: timestamp
      character(len=32) :: rc_s
      character(len=:), allocatable :: lab
      lab = "funwave"
      if (present(label)) lab = label
      call log_line(msg, lab, "ERROR")
      if (default_log_format == log_format_jsonl) then
         call date_and_time(date, time, zone)
         timestamp = date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//time(1:2)//":"//time(3:4)//":"//time(5:6)
         write (rc_s, '(a,i0)') '"rc":', code
         write (output_unit, "(A)") format_json_line(lab, timestamp, "status", "ERROR", msg, trim(rc_s))
      end if
      flush (output_unit)
      call set_error_code(code)
      call throw_exception(__FILE__, __LINE__, message=msg)
   end subroutine fail

   ! A line from any rank without a writer (blow-up sites, netcdf failures
   ! before an error stop, the usage text): stdout in the process format.
   ! Bare write(*,*) calls would corrupt a JSON stream, so every module
   ! screen print goes through here.
   subroutine log_line(msg, label, level)
      character(len=*), intent(in) :: msg
      character(len=*), intent(in), optional :: label, level
      character(len=20) :: date, time
      character(len=8)  :: zone
      character(len=19) :: timestamp
      character(len=:), allocatable :: lab, lvl
      lab = "funwave"
      lvl = "INFO"
      if (present(label)) lab = label
      if (present(level)) lvl = level
      if (default_log_format == log_format_jsonl) then
         call date_and_time(date, time, zone)
         timestamp = date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "//time(1:2)//":"//time(3:4)//":"//time(5:6)
         write (output_unit, "(A)") format_json_line(lab, timestamp, "log", lvl, msg)
      else
         write (output_unit, "(A)") trim(msg)
      end if
   end subroutine log_line

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

   ! One JSON object: {"ts","kind","label","level","msg"[,extra fields]};
   ! the timestamp is written ISO-style (T separator); absent kind = log
   function format_json_line(label, timestamp, kind, level_name, msg, extra) result(line)
      character(len=*), intent(in) :: label, timestamp, level_name, msg
      character(len=*), intent(in), optional :: kind, extra
      character(len=:), allocatable :: line, ts, k
      ts = trim(timestamp)
      if (len(ts) >= 11) ts = ts(1:10)//"T"//ts(12:)
      k = "log"
      if (present(kind)) k = trim(kind)
      line = '{"ts":"'//ts//'","kind":"'//k//'","label":"'//trim(label)// &
             '","level":"'//trim(level_name)//'","msg":"'//json_escape(trim(msg))//'"'
      if (present(extra)) then
         if (len_trim(extra) > 0) line = line//","//trim(extra)
      end if
      line = line//"}"
   end function format_json_line

   ! Escape a string for a JSON literal: backslash, double quote, and the
   ! control characters as \uXXXX (tab and newline in their short forms)
   function json_escape(s) result(out)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: out
      character(len=6) :: u
      integer :: i, c
      out = ""
      do i = 1, len(s)
         c = iachar(s(i:i))
         select case (c)
         case (34); out = out//'\"'
         case (92); out = out//'\\'
         case (10); out = out//'\n'
         case (9); out = out//'\t'
         case (0:8, 11:12, 14:31)
            write (u, '(a2,z4.4)') '\u', c
            out = out//u
         case default; out = out//s(i:i)
         end select
      end do
   end function json_escape

   ! open an output file or fail with the I/O contract code: a permission
   ! or disk failure otherwise dies as a runtime error with the runtime's
   ! own exit code, outside the contract
   subroutine open_out(unit, file, status, action, access, form, position)
      integer, intent(out) :: unit
      character(len=*), intent(in) :: file
      character(len=*), intent(in), optional :: status, action, access, form, position
      character(len=:), allocatable :: st, ac, acc, fm, pos
      integer :: ios
      st = "unknown"; ac = "write"; acc = "sequential"; fm = "formatted"; pos = "asis"
      if (present(status)) st = status
      if (present(action)) ac = action
      if (present(access)) acc = access
      if (present(form)) fm = form
      if (present(position)) pos = position
      open (newunit=unit, file=trim(file), status=st, action=ac, access=acc, form=fm, position=pos, iostat=ios)
      if (ios /= 0) call fail("output: cannot open "//trim(file)//" for writing", EXIT_IO)
   end subroutine open_out

   subroutine log_writer_finalize(this)
      class(type_log_writer), intent(inout) :: this
      if (this%owns_file .and. this%file_unit /= -1) close (this%file_unit)
   end subroutine log_writer_finalize

end module core_log_io_mod
