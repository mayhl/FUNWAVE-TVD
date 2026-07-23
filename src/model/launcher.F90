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
   use core_log_io_mod, only: log_level_debug, log_level_off, &
                              set_default_log_levels
   use model_main_mod, only: type_model_main
# if defined (ENABLE_3D)
   use model_3d_mod, only: type_model_3d
# endif

   implicit none
   private
   public :: launch

contains

   subroutine launch()
      character(2048)          :: yaml_path, log_path, arg
      type(type_env)           :: env, grid_env
      integer, allocatable     :: dims(:)
      integer                  :: ndim, i
      logical                  :: missing, no_grid, quiet, dbg, want_log, have_deck, validate
      type(type_model_main)    :: model_2d
# if defined (ENABLE_3D)
      type(type_model_3d)      :: model_3d
# endif

      ! CLI: flags anywhere, first non-flag argument is the deck
      ! (default input.yaml); -l redirects the log (default funwave.log);
      ! --validate runs the full config read then stops before setup/run
      quiet = .false.
      dbg = .false.
      validate = .false.
      want_log = .false.
      have_deck = .false.
      yaml_path = "input.yaml"
      log_path = "funwave.log"
      do i = 1, command_argument_count()
         call get_command_argument(i, arg)
         if (want_log) then
            log_path = arg
            want_log = .false.
            cycle
         end if
         select case (trim(arg))
         case ("-q", "--quiet"); quiet = .true.
         case ("-d", "--debug"); dbg = .true.
         case ("-l", "--log"); want_log = .true.
         case ("--validate"); validate = .true.
         case default
            ! one deck argument; anything dash-led here is an unknown flag
            if (arg(1:1) == "-" .or. have_deck) then
               write (*, "(a)") "Usage: funwave [-q] [-d] [--validate] [-l <log path>] [input.yaml]"
               stop 1
            end if
            yaml_path = arg
            have_deck = .true.
         end select
      end do
      if (want_log) then
         write (*, "(a)") "Usage: funwave [-q] [-d] [--validate] [-l <log path>] [input.yaml]"
         stop 1
      end if

      ! -d opens both sinks to the debug config-resolution trace; -q turns
      ! the console off (errors fall through to stderr).  Defaults are set
      ! BEFORE new_env so every writer (incl. the yaml [config] logger)
      ! inherits them.
      if (dbg) call set_default_log_levels(stdout_level=log_level_debug, &
                                           file_level=log_level_debug)
      if (quiet) call set_default_log_levels(stdout_level=log_level_off)

      ! Initialise environment once — owns MPI, YAML, and logging for this run.
      call new_env(env, label="funwave", yaml_path=trim(yaml_path), log_path=trim(log_path))

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
         write (*, "(a)") "ERROR: 3D grid detected but HYPRE not linked — rebuild with -DHYPRE_DIR=<path>."
         stop 1
# endif
      end if
   end subroutine launch

   ! ── 2D path ────────────────────────────────────────────────────────────
   ! Uses init_from_env so that the env created above is reused rather than
   ! constructing a second MPI/YAML environment inside type_model_main%init.
   subroutine run_2d(model, env, validate)
      type(type_model_main), intent(inout) :: model
      type(type_env), intent(inout) :: env
      logical, intent(in) :: validate

      call model%init_from_env(env)
      ! --validate stops here: the full config read ran (schema, cross-rules,
      ! input file paths) but nothing is allocated and no output is created
      if (validate) then
         call env%log%info("validation complete -- deck OK")
      else
         call model%run()
      end if
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
