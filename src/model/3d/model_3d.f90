!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Thin wrapper around the legacy 3D (fully-dispersive) model.
!
!  read_input() calls the legacy READ_INPUT() from src/model/3d/old/io.F,
!  which reads input.txt from the CWD.  The env argument is unused but
!  required by the type_model_base interface.
!
!  run() mirrors the PROGRAM MASTER lifecycle in src/model/3d/old/master.F:
!    INIT_3D_GLOBAL → PARALLEL_CARTESIAN → INDEX_LOCAL →
!    CALL_ALLOCATE_VARIABLES_3D → read_bathymetry →
!    CALL_INITIALIZATION_3D → generate_grid → SINGLE_GRID_LOOP
!  ALLOCATE_VARIABLES and INITIALIZATION are bridged because identically-named
!  2D symbols in the main binary preempt the dylib versions on macOS.
!  MPI_Init / MPI_Finalize are NOT called here; the unified binary's new_env /
!  finalize owns the MPI lifecycle.  INIT_3D_GLOBAL seeds myid/NumP in MODULE
!  GLOBAL from the already-initialised env communicator.
!
!  CMake note: when building the unified executable, exclude
!  src/model/3d/old/master.F (PROGRAM MASTER) from the source list and
!  include this file instead.  The standalone full_dispersion target
!  continues to compile master.F directly.
!
!  HISTORY :
!    05/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_mod
   use core_env_mod, only: type_env
   use model_base_mod, only: type_model_base

   implicit none
   private
   public :: type_model_3d

   type, extends(type_model_base) :: type_model_3d
   contains
      procedure :: read_input => model_3d_read_input
      procedure :: run => model_3d_run
   end type type_model_3d

contains

   ! Delegate to legacy READ_INPUT (reads input.txt from CWD).
   subroutine model_3d_read_input(this, env)
      class(type_model_3d), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      external :: CALL_READ_INPUT_3D, INIT_3D_GLOBAL
      ! Seed myid/NumP in MODULE GLOBAL before READ_INPUT so that MPI-conditional
      ! code inside READ_INPUT (broadcasts, rank-0 I/O) sees the correct values.
      call INIT_3D_GLOBAL(env%comm%rank_id, env%comm%size)
      call CALL_READ_INPUT_3D()
      this%is_activated = .true.
   end subroutine model_3d_read_input

   ! Run the full 3D lifecycle (mirrors PROGRAM MASTER).
   !
   ! MPI_Init / MPI_Finalize are intentionally absent here: in the unified
   ! binary MPI is already initialised by the caller's new_env / new_comm
   ! and will be finalised by type_model_main%finalize.  INIT_3D_GLOBAL is
   ! called in read_input (before READ_INPUT), so MODULE GLOBAL already has
   ! correct myid / NumP by the time run() is entered.
   subroutine model_3d_run(this, env)
      class(type_model_3d), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      ! ALLOCATE_VARIABLES and INITIALIZATION are also defined in the 2D legacy
      ! code; on macOS the main-binary symbols preempt the dylib versions.  Call
      ! through bridge wrappers (defined in bridge.F, internal to the dylib) so
      ! the 3D MODULE GLOBAL is used.
      external :: PARALLEL_CARTESIAN, INDEX_LOCAL, &
         CALL_ALLOCATE_VARIABLES_3D, read_bathymetry, &
         CALL_INITIALIZATION_3D, generate_grid, SINGLE_GRID_LOOP

      call PARALLEL_CARTESIAN()
      call INDEX_LOCAL(1)
      call CALL_ALLOCATE_VARIABLES_3D()
      call read_bathymetry()
      call CALL_INITIALIZATION_3D()
      call generate_grid()
      call SINGLE_GRID_LOOP()

   end subroutine model_3d_run

end module model_3d_mod
