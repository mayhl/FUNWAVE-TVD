!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Model launcher — dispatches to the 2D or 3D model at runtime based on
!  the number of dimensions in `grid size` list in the YAML:
!
!    grid size: [Nx, Ny]     ->  2D path  (type_model_main, YAML reader)
!    grid size: [Nx, Ny, Nz] ->  3D path  (type_model_3d, legacy pipeline)
!
!  A single env is created here from the CLI yaml_path and passed down so
!  that MPI and YAML are initialised exactly once.
!
!  The 3D path is compiled only when ENABLE_3D is defined (set by CMake
!  when HYPRE is found and funwave_3d shared library is built).  Passing a
!  3D yaml without ENABLE_3D prints an error and stops.
!
!  HISTORY :
!    05/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_launcher_mod
   use core_env_mod, only: type_env, new_env, get_sub_env
   use core_log_io_mod, only: log_level_debug, log_level_off, set_default_log_levels, &
                              set_default_log_format, log_format_jsonl, log_line, fail
   use core_throw_mod, only: EXIT_DECK
   use model_main_mod, only: type_model_main
   use core_version_mod, only: version_line, build_info_lines, n_build_info_lines, &
                               build_info_line_len
# if defined (ENABLE_3D)
   use model_3d_mod, only: type_model_3d
# endif

   implicit none
   private
   public :: launch

contains

   subroutine launch()
      character(2048)          :: yaml_path, log_path, arg
      character(build_info_line_len) :: info(n_build_info_lines)
      type(type_env)           :: env, grid_env
      integer, allocatable     :: dims(:)
      integer                  :: ndim, i, j
      logical                  :: missing, no_grid, quiet, dbg, want_log, have_deck, validate
      logical                  :: want_fmt, jsonl
      character(64)            :: envval
      integer                  :: envlen
      type(type_model_main)    :: model_2d
# if defined (ENABLE_3D)
      type(type_model_3d)      :: model_3d
# endif

      ! Exit codes (core_throw_mod): 0 completed / --validate passed / -v;
      ! 1 unclassified abort; 3 numerical blow-up; 4 diagnostics abort
      ! threshold; 5 output I/O failure; 6 the comm layer's MPI checks;
      ! 64 usage; 65 deck refused.  Signals, runtime crashes and MPI
      ! library errors keep the launcher's or runtime's code.
      ! CLI: flags anywhere, first non-flag argument is the deck
      ! (default input.yaml); -l redirects the log (default funwave.log);
      ! --validate runs the config read + setup + every module init_compute,
      ! then stops before output and the time loop (deck lint, no steps);
      ! -v/--version and --build-info print and stop before MPI comes up
      quiet = .false.
      dbg = .false.
      validate = .false.
      want_log = .false.
      want_fmt = .false.
      have_deck = .false.
      ! JSON Lines console: the flag wins over the MU_WRAP=1 environment the
      ! Go shim sets on every subprocess; text stays the default
      call get_environment_variable("MU_WRAP", envval, envlen)
      jsonl = envlen > 0 .and. trim(envval) == "1"
      yaml_path = "input.yaml"
      log_path = "funwave.log"
      do i = 1, command_argument_count()
         call get_command_argument(i, arg)
         if (want_log) then
            log_path = arg
            want_log = .false.
            cycle
         end if
         if (want_fmt) then
            select case (trim(arg))
            case ("text"); jsonl = .false.
            case ("jsonl"); jsonl = .true.
            case default; call usage_stop()
            end select
            want_fmt = .false.
            cycle
         end if
         select case (trim(arg))
         case ("-q", "--quiet"); quiet = .true.
         case ("-d", "--debug"); dbg = .true.
         case ("-l", "--log"); want_log = .true.
         case ("--log-format"); want_fmt = .true.
         case ("--validate"); validate = .true.
            ! return, not stop: cce prints " STOP " on a bare stop and that
            ! would trail the parsed output
         case ("-v", "--version")
            write (*, "(a)") version_line()
            return
         case ("--build-info")
            info = build_info_lines()
            do j = 1, n_build_info_lines
               write (*, "(a)") trim(info(j))
            end do
            return
         case default
            ! one deck argument; anything dash-led here is an unknown flag
            if (arg(1:1) == "-" .or. have_deck) call usage_stop()
            yaml_path = arg
            have_deck = .true.
         end select
      end do
      if (want_log .or. want_fmt) call usage_stop()

      ! -d opens both sinks to the debug config-resolution trace; -q turns
      ! the console off (errors fall through to stderr).  Defaults are set
      ! BEFORE new_env so every writer (incl. the yaml [config] logger)
      ! inherits them.
      if (dbg) call set_default_log_levels(stdout_level=log_level_debug, &
                                           file_level=log_level_debug)
      if (quiet) call set_default_log_levels(stdout_level=log_level_off)
      if (jsonl) call set_default_log_format(log_format_jsonl)

      ! Initialise environment once — owns MPI, YAML, and logging for this run.
      call new_env(env, label="funwave", yaml_path=trim(yaml_path), log_path=trim(log_path))
      ! --validate promotes the unread-key report from warning to abort
      env%yaml%unread_strict = validate

      ! Peek at grid_size to decide dimensionality: 2 elements → 2D path,
      ! 3 elements → 3D path.  The 2D schema owns grid:; the 3D schema
      ! still uses geometry: (review-gated separately), so fall back.
      grid_env = get_sub_env(env, "grid", no_grid)
      missing = .true.
      if (.not. no_grid) then
         call grid_env%yaml%read_integer_array("grid_size", val=dims, silent=missing)
      end if
      if (missing) then
         grid_env = get_sub_env(env, "geometry", no_grid)
         if (.not. no_grid) then
            call grid_env%yaml%read_integer_array("grid_size", val=dims, silent=missing)
         end if
      end if
      ndim = 0
      if (.not. missing .and. allocated(dims)) ndim = size(dims)

      if (ndim < 3) then
         call run_2d(model_2d, env, validate)
      else
# if defined (ENABLE_3D)
         call run_3d(model_3d, env, validate)
# else
         call fail("3D grid detected but HYPRE not linked -- rebuild with -DHYPRE_DIR=<path>.", EXIT_DECK)
# endif
      end if
   end subroutine launch

   subroutine usage_stop()
      call log_line("Usage: funwave [-q] [-d] [--validate] [-l <log path>]"// &
                    " [--log-format text|jsonl] [input.yaml]")
      call log_line("       funwave -v | --version | --build-info")
      ! before MPI is up: a plain stop with the usage code
      stop 64
   end subroutine usage_stop

   ! ── 2D path ────────────────────────────────────────────────────────────
   ! Uses init_from_env so that the env created above is reused rather than
   ! constructing a second MPI/YAML environment inside type_model_main%init.
   subroutine run_2d(model, env, validate)
      type(type_model_main), intent(inout) :: model
      type(type_env), intent(inout) :: env
      logical, intent(in) :: validate

      call model%init_from_env(env)
      ! --validate runs setup + every module init_compute (where init-time deck
      ! errors surface) and stops before output and the time loop
      call model%run(validate)
      call model%finalize()
   end subroutine run_2d

# if defined (ENABLE_3D)
   ! ── 3D path ────────────────────────────────────────────────────────────
   ! read_input delegates to legacy READ_INPUT (reads input.txt from CWD).
   ! run seeds MODULE GLOBAL via INIT_3D_GLOBAL then runs the full lifecycle.
   subroutine run_3d(model, env, validate)
      use mpi, only: MPI_FINALIZE
      type(type_model_3d), intent(inout)         :: model
      type(type_env), intent(inout), target :: env
      logical, intent(in)                        :: validate
      integer                                    :: ier

      call model%read_input(env)
      if (validate) then
         call env%log%info("validation complete -- deck OK")
      else
         call model%run(env)
      end if
      call MPI_FINALIZE(ier)
   end subroutine run_3d
# endif

end module model_launcher_mod
