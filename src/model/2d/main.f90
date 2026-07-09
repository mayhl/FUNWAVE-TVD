!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Top-level model orchestrator
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!    05/13/2026  Updated to new module layout; added output/physics components
!
!-------------------------------------------------

module model_main_mod

   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, new_env
   use core_grid_mod, only: type_grid_2d
   use core_field_registry_mod, only: type_field_registry
   use probe_mod, only: dump_state, reset_state

   use model_geometry_mod,   only: type_model_geometry
   use model_simulation_mod, only: type_model_simulation
   use model_hot_start_mod,  only: type_model_hot_start
   use model_wavemaker_mod,  only: type_model_wavemaker
   use model_sponge_mod,     only: type_model_sponge
   use model_obstacle_mod,   only: type_model_obstacle
   use model_friction_mod,   only: type_model_friction
   use model_numerics_mod,   only: type_model_numerics
   use model_breaking_mod,   only: type_model_breaking
   use model_output_mod,     only: type_model_output
   use model_physics_mod,    only: type_model_physics
   use model_coupling_mod,   only: type_model_coupling

   use model_fields_2d_mod,    only: type_fields_2d
   use model_kernel_masks_mod, only: update_mask9

   implicit none

   type, public :: type_model_main
      type(type_env) :: env

      type(type_model_geometry)   :: geometry
      type(type_model_simulation) :: simulation
      type(type_model_hot_start)  :: hot_start
      type(type_model_wavemaker)  :: wavemaker
      type(type_model_sponge)     :: sponge
      type(type_model_obstacle)   :: obstacle
      type(type_model_friction)   :: friction
      type(type_model_numerics)   :: numerics
      type(type_model_breaking)   :: breaking
      type(type_model_output)     :: output
      type(type_model_physics)    :: physics
      type(type_model_coupling)   :: coupling

      ! Distributed state — built by setup() after all read_input calls
      type(type_grid_2d)        :: grid
      type(type_fields_2d)      :: fields
      type(type_field_registry) :: registry
   contains
      procedure :: init
      procedure :: init_from_env => model_init_from_env
      procedure :: setup => model_setup
      procedure :: finalize => model_finalize
   end type type_model_main

contains

   subroutine init(this)
      class(type_model_main), intent(inout) :: this
      character(2048) :: yaml_path

      call reset_state()
      call dump_state(5.0d0, "main_init_test")
      call get_command_argument(1, yaml_path)

      ! Initialize environment (Comm, Log, YAML)
      call new_env(this%env, label="main", yaml_path=trim(yaml_path), log_path="test.log")

      ! Read component inputs using environment resources
      call this%env%comm%barrier()
      call this%geometry%read_input(this%env)
      call this%simulation%read_input(this%env)
      call this%hot_start%read_input(this%env)
      call this%wavemaker%read_input(this%env)
      call this%sponge%read_input(this%env)
      call this%obstacle%read_input(this%env)
      call this%friction%read_input(this%env)
      call this%numerics%read_input(this%env)
      call this%breaking%read_input(this%env)
      call this%output%read_input(this%env)
      call this%physics%read_input(this%env)
      call this%coupling%read_input(this%env)
      ! Finalize YAML after reading all inputs
      call this%env%yaml%finalize()

   end subroutine init

   ! Initialise from an already-created env (e.g. created by the launcher to
   ! peek at grid_size before dispatching).  All component read_input calls
   ! are performed; new_env is NOT called a second time.  The env pointer
   ! fields (comm, log) are shared by value-copy — both this%env and the
   ! caller's env point to the same comm/log heap objects.  Only one of them
   ! should call finalize.
   subroutine model_init_from_env(this, env)
      use core_env_mod, only: get_sub_env
      class(type_model_main), intent(inout) :: this
      type(type_env),         intent(inout) :: env

      call reset_state()
      call dump_state(5.0d0, "main_init_test")

      ! Adopt the caller's env (shallow copy; comm/log pointers are shared).
      ! Transfer yaml ownership: nullify env%yaml%file%root so that only
      ! this%env's YamlFile_final will free the tree when going out of scope.
      this%env = env
      call env%yaml%transfer_ownership()

      ! Read component inputs using environment resources
      call this%env%comm%barrier()
      call this%geometry%read_input(this%env)
      call this%simulation%read_input(this%env)
      call this%hot_start%read_input(this%env)
      call this%wavemaker%read_input(this%env)
      call this%sponge%read_input(this%env)
      call this%obstacle%read_input(this%env)
      call this%friction%read_input(this%env)
      call this%numerics%read_input(this%env)
      call this%breaking%read_input(this%env)
      call this%output%read_input(this%env)
      call this%physics%read_input(this%env)
      call this%coupling%read_input(this%env)
      ! Finalize YAML after reading all inputs
      call this%env%yaml%finalize()

   end subroutine model_init_from_env

   ! ----------------------------------------------------------------
   ! Build the distributed state from parsed config — Phase 6a.
   ! Ports the state-init portion of legacy INITIALIZATION (old/init.F):
   ! grid, bathymetry, initial condition, wet/dry masks, and conserved
   ! variables.  Call once, after init()/init_from_env().
   !
   ! Wet/dry from the IC (legacy "get Eta and H"):
   !   $$ \eta < -d \;\Rightarrow\; \text{dry:}\ m = 0,\
   !      \eta := -d_{min} - d $$
   ! Total depth and conserved variables:
   !   $$ H = \max(\gamma_3\,\eta + d,\ d_{frc}), \qquad
   !      p = H u, \quad q = H v $$
   ! The dispersion-corrected initial state
   ! $\bar U = Hu + \gamma_1 U_{1p} H$ needs cal_dispersion outputs and
   ! lands with the stepper's state-derivation step (Phase 6c); p = Hu
   ! is exact when $\gamma_1 = 0$ or $U_{1p}(t{=}0) = 0$.
   ! ----------------------------------------------------------------
   subroutine model_setup(this)
      class(type_model_main), intent(inout) :: this

      integer :: i, j

      call this%geometry%build_grid(this%env%comm, this%grid, &
                                    periodic_y=this%physics%periodic)
      call this%fields%alloc(this%grid)
      if (this%physics%viscosity_breaking) call this%fields%alloc_breaking(this%grid)

      call this%geometry%init_depth(this%grid, this%fields%depth, &
                                    this%fields%depth_x, this%fields%depth_y)
      call this%wavemaker%apply_ic(this%grid, this%fields%eta, &
                                   this%fields%u, this%fields%v)

      ! wet/dry mask from the initial condition (structure masks: Step 6+)
      this%fields%mask_struc = 1
      associate (f => this%fields, lp => this%grid%lp)
      do j = 1, lp%nloc
         do i = 1, lp%mloc
            if (f%eta(i, j) < -f%depth(i, j)) then
               f%mask(i, j) = 0
               f%eta(i, j)  = -this%numerics%MinDepth - f%depth(i, j)
            else
               f%mask(i, j) = 1
            end if
         end do
      end do
      f%mask = f%mask * f%mask_struc

      call update_mask9(lp, f%eta, f%depth, f%mask, f%mask9, &
                        this%numerics%MinDepthFrc, this%physics%SWE_ETA_DEP, &
                        this%physics%viscosity_breaking)

      f%h = max(this%physics%Gamma3 * f%eta + f%depth, this%numerics%MinDepthFrc)
      f%p = f%h * f%u
      f%q = f%h * f%v
      end associate

      call this%fields%register(this%registry)

   end subroutine model_setup

   subroutine model_finalize(this)
      use mpi_f08, only: MPI_Finalize
      class(type_model_main), intent(inout) :: this
      integer :: ierr
      call this%env%finalize()
      call MPI_Finalize(ierr)
   end subroutine model_finalize

end module model_main_mod
