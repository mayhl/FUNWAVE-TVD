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
   use core_comm_mod, only: type_comm
   use core_grid_mod, only: type_grid_2d
   use core_field_registry_mod, only: type_field_registry
   use core_output_manager_mod, only: type_output_manager
   use core_output_channel_mod, only: type_output_channel
   use core_stepper_engine_mod, only: type_stepper_engine, type_engine_monitor
   use core_path_mod, only: type_path
   use probe_mod, only: dump_state, reset_state

   use model_geometry_mod, only: type_model_geometry
   use model_simulation_mod, only: type_model_simulation
   use model_hot_start_mod, only: type_model_hot_start
   use model_wavemaker_mod, only: type_model_wavemaker
   use model_sponge_mod, only: type_model_sponge
   use model_obstacle_mod, only: type_model_obstacle
   use model_friction_mod, only: type_model_friction
   use model_numerics_mod, only: type_model_numerics
   use model_breaking_mod, only: type_model_breaking
   use model_output_mod, only: type_model_output
   use model_physics_mod, only: type_model_physics
   use model_coupling_mod, only: type_model_coupling

   use model_fields_2d_mod, only: type_fields_2d
   use model_stepper_2d_mod, only: type_model_stepper_2d

   implicit none

   ! Loop-top output call site for the stepper engine: wraps the
   ! output manager + registry + comm (the engine itself is
   ! output-free — libcore_output links libcore_engine).
   type, extends(type_engine_monitor) :: type_output_monitor
      type(type_output_manager), pointer :: mgr => null()
      type(type_field_registry), pointer :: registry => null()
      type(type_comm), pointer :: comm => null()
   contains
      procedure :: step => output_monitor_step
   end type type_output_monitor

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
      procedure :: run => model_run
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
      type(type_env), intent(inout) :: env

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
   ! $\bar U = Hu + \gamma_1 U_{1p} H$ needs cal_dispersion outputs;
   ! stepper%init applies it (p = Hu here is exact when $\gamma_1 = 0$
   ! or $U_{1p}(t{=}0) = 0$, and stands alone on the legacy path).
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
               f%eta(i, j) = -this%numerics%MinDepth - f%depth(i, j)
            else
               f%mask(i, j) = 1
            end if
         end do
      end do
      f%mask = f%mask*f%mask_struc

      ! initial MASK9 is the pure 3x3 product on the INTERIOR only
      ! (legacy init.F): neither the viscosity_breaking all-1 override
      ! nor the SWE_ETA_DEP zeroing of the in-loop update applies at
      ! t = 0, and every ghost — including the ring the stage-1 face
      ! reconstruction reads — is ZERO (legacy allocates zeroed and
      ! PHI_INT_EXCH fills MPI seams only, never walls).  Anything
      ! else kicks the stage-1 fluxes and seeds a persistent swash
      ! divergence (parity ledger 8c).
      f%mask9 = 0
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            f%mask9(i, j) = f%mask(i, j)*f%mask(i - 1, j)*f%mask(i + 1, j) &
                            *f%mask(i + 1, j + 1)*f%mask(i, j + 1)*f%mask(i - 1, j + 1) &
                            *f%mask(i + 1, j - 1)*f%mask(i, j - 1)*f%mask(i - 1, j - 1)
         end do
      end do
      ! MPI-seam ghosts (real-copy ride on the halo exchange); NOTE:
      ! under periodic-y this also wraps, where legacy PHI_INT_EXCH
      ! leaves 1-rank y-ghosts zeroed — revisit if a periodic case
      ! shows a step-1 ring deviation
      block
         real(SP), allocatable :: rmask(:, :)
         allocate (rmask, source=real(f%mask9, SP))
         call this%grid%halo_exchange(rmask)
         f%mask9 = nint(rmask)
      end block

      f%h = max(this%physics%Gamma3*f%eta + f%depth, this%numerics%MinDepthFrc)
      f%p = f%h*f%u
      f%q = f%h*f%v
      end associate

      call this%fields%register(this%registry)

   end subroutine model_setup

   ! ----------------------------------------------------------------
   ! Full modern-path simulation — Phase 6c.  Builds the distributed
   ! state, the 2D stepper, a field output channel bridged from the
   ! legacy-style output flags, and hands the loop to the engine.
   ! Hot start (TIME = HotStartTime) is not wired yet.
   ! ----------------------------------------------------------------
   subroutine model_run(this)
      class(type_model_main), intent(inout), target :: this

      type(type_model_stepper_2d) :: stepper
      type(type_stepper_engine) :: engine
      type(type_output_manager), target :: output_mgr
      type(type_output_monitor) :: monitor

      call this%setup()
      call this%friction%init_compute(this%grid)
      call this%sponge%init_compute(this%grid)
      ! NOTE: max-merge, where legacy adds the sponge drag on top of Cd
      ! — identical while Cd = 0 in the sponge zone (all current tests)
      call this%sponge%merge_friction(this%friction%Cd, this%fields%depth)
      call this%wavemaker%init_compute(this%grid, this%physics%periodic, &
                                       this%env, this%physics%Beta_ref)

      call stepper%init(this%env, this%grid, this%fields, this%physics, &
                        this%numerics, this%breaking, this%friction, &
                        this%simulation, this%output, this%wavemaker, &
                        this%sponge)

      call build_field_channel(this, output_mgr)
      monitor%mgr => output_mgr
      monitor%registry => this%registry
      monitor%comm => this%env%comm

      call engine%init(0.0_SP, this%simulation%total_time, &
                       this%simulation%screen_interval)
      call engine%run(stepper, monitor, this%env%log)

      call output_mgr%finalize()
      call stepper%free()

   end subroutine model_run

   subroutine output_monitor_step(this, t, dt)
      class(type_output_monitor), intent(inout) :: this
      real(SP), intent(in) :: t, dt
      call this%mgr%step(t, dt, this%registry, this%comm)
   end subroutine output_monitor_step

   ! ----------------------------------------------------------------
   ! Bridge the legacy-style output flags to one snapshot field
   ! channel (full output-block YAML: Step 7).  Only registry-backed
   ! variables map; MASK/MASK9 (integer) and the legacy P/Q interface
   ! fluxes are skipped with a warning.  Legacy file naming: the
   ! channel writes <registry_name>_NNNNN (h_max vs legacy hmax —
   ! reconcile at the 6e regression switchover).
   ! ----------------------------------------------------------------
   subroutine build_field_channel(this, mgr)
      class(type_model_main), intent(inout), target :: this
      type(type_output_manager), intent(inout) :: mgr

      character(len=16) :: vars(24)
      character(len=8) :: stats(1)
      character(:), allocatable :: folder, fmt
      real(SP) :: dummy_coord(1)
      type(type_path) :: outdir
      integer :: nv
      logical :: ok

      associate (out => this%output)

         nv = 0
         if (out%OUT_ETA) call add_var(vars, nv, "eta")
         if (out%OUT_U) call add_var(vars, nv, "u")
         if (out%OUT_V) call add_var(vars, nv, "v")
         if (out%OUT_Hmax) call add_var(vars, nv, "h_max")
         if (out%OUT_Hmin) call add_var(vars, nv, "h_min")
         if (out%OUT_Umax) call add_var(vars, nv, "u_max")
         if (out%OUT_MFmax) call add_var(vars, nv, "mf_max")
         if (out%OUT_VORmax) call add_var(vars, nv, "vort_max")
         if (this%numerics%OUT_Time) call add_var(vars, nv, "arr_time")
         if (out%OUT_NU) call add_var(vars, nv, "nu_break")

         if (out%OUT_MASK .or. out%OUT_MASK9) then
            call this%env%log%warning( &
               "output: MASK/MASK9 not yet available on the modern path")
         end if
         if (out%OUT_P .or. out%OUT_Q) then
            call this%env%log%warning( &
               "output: legacy P/Q (interface fluxes) not yet available "// &
               "on the modern path")
         end if

         folder = trim(out%result_folder)
         if (folder(len(folder):len(folder)) /= "/") folder = folder//"/"
         if (this%env%comm%is_io_node()) then
            outdir = type_path(folder)
            if (.not. outdir%is_dir()) ok = outdir%mkdir()
         end if
         call this%env%comm%barrier()

         select case (out%field_io_type(1:1))
         case ("B", "b")
            fmt = "binary"
         case default
            fmt = "ascii"
         end select

         stats(1) = " "
         dummy_coord(1) = 0.0_SP

         allocate (mgr%channels(1))
         mgr%n_channels = 1
         call mgr%channels(1)%init(id="field", geom_type="field", &
                                   variables=vars, n_vars=nv, &
                                   statistics=stats, n_stats=0, &
                                   snapshot=.true., &
                                   t_start=this%simulation%t_start, &
                                   interval=this%simulation%plot_intv, &
                                   result_folder=folder, format=fmt, &
                                   coords_x=dummy_coord, coords_y=dummy_coord, &
                                   n_coords=0, grid=this%grid, &
                                   comm=this%env%comm)

      end associate

   end subroutine build_field_channel

   subroutine add_var(vars, nv, name)
      character(len=*), intent(inout) :: vars(:)
      integer, intent(inout) :: nv
      character(len=*), intent(in) :: name
      nv = nv + 1
      vars(nv) = name
   end subroutine add_var

   subroutine model_finalize(this)
      use mpi_f08, only: MPI_Finalize
      class(type_model_main), intent(inout) :: this
      integer :: ierr
      call this%env%finalize()
      call MPI_Finalize(ierr)
   end subroutine model_finalize

end module model_main_mod
