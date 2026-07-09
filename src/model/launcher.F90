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
   use core_env_mod,   only: type_env, new_env, get_sub_env
   use model_main_mod, only: type_model_main
# if defined (ENABLE_3D)
   use model_3d_mod,   only: type_model_3d
# endif

   implicit none
   private
   public :: launch

contains

   subroutine launch()
      character(2048)          :: yaml_path
      type(type_env)           :: env, grid_env
      integer, allocatable     :: dims(:)
      integer                  :: ndim
      logical                  :: missing
      type(type_model_main)    :: model_2d
# if defined (ENABLE_3D)
      type(type_model_3d)      :: model_3d
# endif

      call get_command_argument(1, yaml_path)
      if (len_trim(yaml_path) == 0) then
         write(*, "(a)") "Usage: funwave <input.yaml>"
         stop 1
      end if

      ! Initialise environment once — owns MPI, YAML, and logging for this run.
      call new_env(env, label="funwave", yaml_path=trim(yaml_path), log_path="funwave.log")

      ! Peek at grid_size under the geometry section to decide dimensionality.
      ! 2 elements → 2D path; 3 elements → 3D path.  Matches the 2D YAML layout.
      grid_env = get_sub_env(env, "geometry")
      call grid_env%yaml%read_integer_array("grid_size", val=dims, silent=missing)
      ndim = 0
      if (.not. missing .and. allocated(dims)) ndim = size(dims)

      if (ndim < 3) then
         call run_2d(model_2d, env)
      else
# if defined (ENABLE_3D)
         call run_3d(model_3d, env)
# else
         write(*, "(a)") "ERROR: 3D grid detected but HYPRE not linked — rebuild with -DHYPRE_DIR=<path>."
         stop 1
# endif
      end if
   end subroutine launch

   ! ── 2D path ────────────────────────────────────────────────────────────
   ! Uses init_from_env so that the env created above is reused rather than
   ! constructing a second MPI/YAML environment inside type_model_main%init.
   subroutine run_2d(model, env)
      type(type_model_main), intent(inout) :: model
      type(type_env),        intent(inout) :: env

      external :: run_legacy_2d  ! src/model/2d/old/legacy_runner.F

      call model%init_from_env(env)
      ! Bridge: run_legacy_2d calls READ_INPUT (which re-reads via model%init()
      ! internally) to populate MODULE GLOBALs, then runs the full simulation loop.
      ! TODO: replace with call model%run() once the time-loop is refactored out
      !       of src/model/2d/old/ into type_model_main.
      call run_legacy_2d()
      call model%finalize()
   end subroutine run_2d

# if defined (ENABLE_3D)
   ! ── 3D path ────────────────────────────────────────────────────────────
   ! read_input delegates to legacy READ_INPUT (reads input.txt from CWD).
   ! run seeds MODULE GLOBAL via INIT_3D_GLOBAL then runs the full lifecycle.
   subroutine run_3d(model, env)
      use mpi, only: MPI_FINALIZE
      type(type_model_3d), intent(inout)         :: model
      type(type_env),      intent(inout), target :: env
      integer                                    :: ier

      call model%read_input(env)
      call model%run(env)
      call MPI_FINALIZE(ier)
   end subroutine run_3d
# endif

end module model_launcher_mod
