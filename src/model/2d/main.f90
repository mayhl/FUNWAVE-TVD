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

   use core_constants_mod, only: SP, LARGE
   use core_env_mod, only: type_env, new_env
   use core_comm_mod, only: type_comm
   use core_grid_mod, only: type_grid_2d
   use core_field_registry_mod, only: type_field_registry
   use core_output_manager_mod, only: type_output_manager
   use core_output_channel_mod, only: type_output_channel, write_field_file
   use core_stepper_engine_mod, only: type_stepper_engine, type_engine_monitor
   use core_path_mod, only: type_path
   use probe_mod, only: dump_state, reset_state

   use model_geometry_mod, only: type_model_geometry, read_field_ascii, stagger_depth
   use model_simulation_mod, only: type_model_simulation
   use model_hot_start_mod, only: type_model_hot_start
   use model_initial_mod, only: type_model_initial
   use model_checkpoint_mod, only: write_checkpoint_core, read_checkpoint_core
   use model_wavemaker_mod, only: type_model_wavemaker
   use model_sponge_mod, only: type_model_sponge
   use model_boundaries_mod, only: boundaries_read_input
   use model_obstacle_mod, only: type_model_obstacle
   use model_friction_mod, only: type_model_friction
   use model_numerics_mod, only: type_model_numerics
   use model_breaking_mod, only: type_model_breaking
   use model_output_mod, only: type_model_output
   use model_physics_mod, only: type_model_physics
   use model_coupling_mod, only: type_model_coupling
   use model_tide_mod, only: type_model_tide
   use model_precipitation_mod, only: type_model_precipitation
   use model_subgrid_mod, only: type_model_subgrid
   use model_foam_mod, only: type_model_foam
   use model_tracer_mod, only: type_model_tracer
   use model_vessel_mod, only: type_model_vessel
   use model_sediment_mod, only: type_model_sediment
   use model_meteo_mod, only: type_model_meteo

   use model_fields_2d_mod, only: type_fields_2d
   use model_means_mod, only: type_model_means
   use model_stations_mod, only: type_model_stations
   use model_stepper_2d_mod, only: type_model_stepper_2d

   implicit none

   ! Loop-top output call site for the stepper engine: wraps the
   ! output manager + registry + comm (the engine itself is
   ! output-free — libcore_output links libcore_engine).
   type, extends(type_engine_monitor) :: type_output_monitor
      type(type_output_manager), pointer :: mgr => null()
      type(type_field_registry), pointer :: registry => null()
      type(type_comm), pointer :: comm => null()
      type(type_model_stations), pointer :: stations => null()
      type(type_model_tracer), pointer :: tracer => null()
      type(type_model_vessel), pointer :: vessel => null()
   contains
      procedure :: step => output_monitor_step
   end type type_output_monitor

   type, public :: type_model_main
      type(type_env) :: env

      type(type_model_geometry)   :: geometry
      type(type_model_simulation) :: simulation
      type(type_model_hot_start)  :: hot_start
      type(type_model_initial)    :: initial
      type(type_model_wavemaker)  :: wavemaker
      type(type_model_sponge)     :: sponge
      type(type_model_obstacle)   :: obstacle
      type(type_model_friction)   :: friction
      type(type_model_numerics)   :: numerics
      type(type_model_breaking)   :: breaking
      type(type_model_output)     :: output
      type(type_model_physics)    :: physics
      type(type_model_coupling)   :: coupling
      type(type_model_tide)       :: tide
      type(type_model_precipitation) :: precipitation
      type(type_model_subgrid) :: subgrid
      type(type_model_foam) :: foam
      type(type_model_tracer) :: tracer
      type(type_model_vessel) :: vessel
      type(type_model_sediment) :: sediment
      type(type_model_meteo) :: meteo

      ! Distributed state — built by setup() after all read_input calls
      type(type_grid_2d)        :: grid
      type(type_fields_2d)      :: fields
      type(type_field_registry) :: registry
      ! Checkpoint restart: the interface flux workspace (p_flux/q_flux) loaded
      ! from core.bin, staged here until register_output wires the registry
      real(SP), allocatable     :: chk_pflux(:, :), chk_qflux(:, :)
      ! Time-averaged statistics (legacy MIXING_STUFF port) — engine
      ! path only, initialised in run()
      type(type_model_means)    :: means
      ! Station time series (legacy STATIONS port) — engine path only
      type(type_model_stations) :: stations
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
      call this%initial%read_input(this%env)
      call this%wavemaker%read_input(this%env)
      call this%obstacle%read_input(this%env)
      call this%friction%read_input(this%env)
      call this%numerics%read_input(this%env)
      call this%breaking%read_input(this%env)
      call this%output%read_input(this%env)
      call this%physics%read_input(this%env)
      call boundaries_read_input(this%env, this%sponge, this%tide, this%physics, &
                                 this%wavemaker)
      call this%coupling%read_input(this%env)
      call this%precipitation%read_input(this%env)
      call this%subgrid%read_input(this%env)
      call this%foam%read_input(this%env)
      call this%tracer%read_input(this%env)
      call this%vessel%read_input(this%env)
      call this%sediment%read_input(this%env)
      call this%meteo%read_input(this%env)
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
      call this%initial%read_input(this%env)
      call this%wavemaker%read_input(this%env)
      call this%obstacle%read_input(this%env)
      call this%friction%read_input(this%env)
      call this%numerics%read_input(this%env)
      call this%breaking%read_input(this%env)
      call this%output%read_input(this%env)
      call this%physics%read_input(this%env)
      call boundaries_read_input(this%env, this%sponge, this%tide, this%physics, &
                                 this%wavemaker)
      call this%coupling%read_input(this%env)
      call this%precipitation%read_input(this%env)
      call this%subgrid%read_input(this%env)
      call this%foam%read_input(this%env)
      call this%tracer%read_input(this%env)
      call this%vessel%read_input(this%env)
      call this%sediment%read_input(this%env)
      call this%meteo%read_input(this%env)
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

      ! breaking.model selects the breaker mechanism (nee
      ! physics.viscosity_breaking); the stepper keeps reading the flag
      this%physics%viscosity_breaking = trim(this%breaking%model) == "eddy_viscosity"
      ! legacy io.F flag forcing (io.F:622-623): roller implies the
      ! breaking viscosity scheme, and breaking viscosity implies the
      ! show/display scheme — every downstream consumer (allocations,
      ! stepper dispatch, output gates) reads the forced values
      if (this%breaking%roller) this%physics%viscosity_breaking = .true.
      if (this%physics%viscosity_breaking) this%breaking%show_breaking = .true.

      call this%geometry%build_grid(this%env%comm, this%grid, &
                                    periodic_y=this%physics%periodic, &
                                    periodic_x=this%physics%periodic_x)
      call this%fields%alloc(this%grid)
      ! WAVEMAKER_VIS and the show-only display mode need nu_break/age
      ! too (legacy allocates the breaking arrays for all options since
      ! fyshi 01/15/2024)
      if (this%physics%viscosity_breaking .or. this%breaking%WAVEMAKER_VIS &
          .or. this%breaking%show_breaking) then
         call this%fields%alloc_breaking(this%grid)
      end if

      if (trim(this%geometry%bathy_type) == "file") then
         call read_field_ascii(this%env, this%geometry%bathy_file%root, &
                               this%grid, this%fields%depth)
      end if
      call this%geometry%init_depth(this%grid, this%fields%depth, &
                                    this%fields%depth_x, this%fields%depth_y)
      ! legacy SUBGRID_INITIAL sits here, off the ghost-filled but
      ! UNCORRECTED depth (subgrid.f90 header NOTE 5)
      if (this%subgrid%is_activated) then
         call this%subgrid%init_compute(this%grid, this%fields%depth, this%env)
         if (this%subgrid%out_porosity) call write_porosity(this)
      end if
      ! legacy order: correction sits between the ghost fill and the
      ! (re)staggering; WaterLevel (when wired) comes after correction
      if (this%geometry%bathy_correction) call apply_bathy_correction(this)
      ! apply_ic zeroes eta/u/v before its solitary branch, so the hot
      ! start loads AFTER it (legacy zeroes long before INI_UVZ; bed
      ! deformation never refreshes DepthX/DepthY).  Solitary IC plus
      ! hot start would resolve the other way in legacy — pathological,
      ! not supported here.
      call this%initial%apply_ic(this%grid, this%fields%eta, &
                                 this%fields%u, this%fields%v)
      if (this%initial%has_fields) call load_initial_fields(this)
      if (this%hot_start%use_checkpoint) then
         call load_checkpoint(this)   ! seeds eta,p,q,mask + hot_start%time
      else if (this%hot_start%is_activated) then
         call load_hot_start(this)    ! ASCII eta/u/v (u,v -> p=Hu below)
      end if

      ! wet/dry mask from the initial condition (structure masks: Step 6+);
      ! a hot-start mask file REPLACES this derivation (legacy NO_MASK_FILE
      ! guard on the "get Eta and H" block)
      this%fields%mask_struc = 1
      associate (f => this%fields, lp => this%grid%lp)
         ! a hot-start mask (ASCII mask_file OR checkpoint) REPLACES the derivation
         if (.not. ((this%hot_start%is_activated .and. &
                     .not. this%hot_start%no_mask_file) .or. &
                    this%hot_start%use_checkpoint)) then
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
         end if

         ! H and the conserved fluxes BEFORE the structure mask lands
         ! (legacy init.F "get Eta and H" precedes the obstacle block, so
         ! H at structure cells is built from the pre-obstacle depth and
         ! never refreshed at init)
         f%h = max(this%physics%Gamma3*f%eta + f%depth, this%numerics%MinDepthFrc)
         ! On a checkpoint restart p,q are the SAVED conserved dispersive flux; keep
         ! them (the stepper derives u,v from p,q on stage 1).  Otherwise seed the
         ! flux from the initial/loaded u,v as plain H*u; stepper_init then adds
         ! the initial-Ubar dispersion correction (parity ledger #1, now enabled).
         if (.not. this%hot_start%use_checkpoint) then
            f%p = f%h*f%u
            f%q = f%h*f%v
         end if

         ! permanent structures (legacy init.F obstacle block): mask from
         ! file, depth -> -LARGE at structure cells; the staggered faces
         ! are NOT rebuilt (legacy leaves DepthX/DepthY pre-obstacle)
         if (this%obstacle%obstacle) call load_obstacle(this)
         where (f%mask_struc == 0) f%depth = -LARGE

         f%mask = f%mask*f%mask_struc

         ! initial MASK9 is the pure 3x3 product on the INTERIOR only
         ! (legacy init.F): neither the viscosity_breaking all-1 override
         ! nor the SWE_ETA_DEP zeroing of the in-loop update applies at
         ! t = 0, and every ghost — including the ring the stage-1 face
         ! reconstruction reads — is ZERO (legacy allocates zeroed and
         ! PHI_INT_EXCH fills MPI seams only, never walls).  Anything
         ! else kicks the stage-1 fluxes and seeds a persistent swash
         ! divergence (parity ledger 8c).  A checkpoint restart carries the
         ! saved mask9 (with its SWE_ETA_DEP zeroing) verbatim — the pure
         ! product would drop that, so skip the derivation here.
         if (.not. this%hot_start%use_checkpoint) then
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
         end if
      end associate

      call this%fields%register(this%registry)

   end subroutine model_setup

   ! ----------------------------------------------------------------
   ! Bathy correction wrapper: run the slope-cap smoothing, rebuild the
   ! staggered faces from the corrected depth, and write the six legacy
   ! OUTPUT_CORRECTION diagnostics (always ascii, legacy PutFile).
   ! Setup-time, so the result folder may not exist yet — mkdir here
   ! mirrors build_field_channel.
   ! ----------------------------------------------------------------
   subroutine apply_bathy_correction(this)
      use core_output_gatherer_mod, only: type_output_gatherer
      class(type_model_main), intent(inout) :: this

      real(SP), allocatable :: depth_org(:, :), gradx0(:, :), grady0(:, :)
      real(SP), allocatable :: gradx(:, :), grady(:, :)
      type(type_output_gatherer) :: gatherer
      type(type_path) :: outdir
      character(:), allocatable :: folder
      character(len=6) :: fmt
      logical :: ok

      call this%geometry%correct_depth(this%env, this%grid, &
                                       this%numerics%MinDepthFrc, this%fields%depth, &
                                       depth_org, gradx0, grady0, gradx, grady)
      call stagger_depth(this%grid%lp, this%fields%depth, &
                         this%fields%depth_x, this%fields%depth_y)

      folder = trim(this%output%result_folder)
      if (folder(len(folder):len(folder)) /= "/") folder = folder//"/"
      if (this%env%comm%is_io_node()) then
         outdir = type_path(folder)
         if (.not. outdir%is_dir()) ok = outdir%mkdir()
      end if
      call this%env%comm%barrier()

      ! legacy PutFile honours FIELD_IO_TYPE for these too
      select case (this%output%field_io_type(1:1))
      case ("B", "b")
         fmt = "binary"
      case default
         fmt = "ascii"
      end select

      call gatherer%init_field(this%grid, this%env%comm)
      call gather_write(this, gatherer, depth_org, folder//"depth_org.txt", fmt)
      call gather_write(this, gatherer, depth_org - this%fields%depth, &
                        folder//"depth_change.txt", fmt)
      call gather_write(this, gatherer, gradx0, folder//"gradx0.txt", fmt)
      call gather_write(this, gatherer, grady0, folder//"grady0.txt", fmt)
      call gather_write(this, gatherer, gradx, folder//"gradx.txt", fmt)
      call gather_write(this, gatherer, grady, folder//"grady.txt", fmt)
      call gatherer%finalize()

   end subroutine apply_bathy_correction

   ! Legacy SUBGRID_INITIAL tail: PutFile the still-water porosity map
   ! (FIELD_IO_TYPE-aware, like the correction diagnostics).  Setup-time,
   ! so the result folder may not exist yet.
   subroutine write_porosity(this)
      use core_output_gatherer_mod, only: type_output_gatherer
      class(type_model_main), intent(inout) :: this

      type(type_output_gatherer) :: gatherer
      type(type_path) :: outdir
      character(:), allocatable :: folder
      character(len=6) :: fmt
      logical :: ok

      folder = trim(this%output%result_folder)
      if (folder(len(folder):len(folder)) /= "/") folder = folder//"/"
      if (this%env%comm%is_io_node()) then
         outdir = type_path(folder)
         if (.not. outdir%is_dir()) ok = outdir%mkdir()
      end if
      call this%env%comm%barrier()

      select case (this%output%field_io_type(1:1))
      case ("B", "b")
         fmt = "binary"
      case default
         fmt = "ascii"
      end select

      call gatherer%init_field(this%grid, this%env%comm)
      call gather_write(this, gatherer, this%subgrid%porosity, &
                        folder//"porosity.ini", fmt)
      call gatherer%finalize()

   end subroutine write_porosity

   ! Gather a raw (mloc,nloc) array and write it — the correction
   ! diagnostics aren't registry fields, so write_static_field can't serve
   subroutine gather_write(this, gatherer, arr, fname, fmt)
      use core_constants_mod, only: N_GHOST
      use core_output_gatherer_mod, only: type_output_gatherer
      class(type_model_main), intent(inout) :: this
      type(type_output_gatherer), intent(in) :: gatherer
      real(SP), intent(in) :: arr(:, :)
      character(*), intent(in) :: fname, fmt

      real(SP), allocatable :: glob(:, :)

      if (this%env%comm%is_io_node()) then
         allocate (glob(gatherer%M, gatherer%N))
      else
         allocate (glob(1, 1))
      end if
      associate (ng => N_GHOST, nx => this%grid%local_nx, ny => this%grid%local_ny)
         call gatherer%gather_field(arr(ng + 1:ng + nx, ng + 1:ng + ny), &
                                    glob, this%env%comm)
      end associate
      if (this%env%comm%is_io_node()) call write_field_file(fname, glob, fmt)
   end subroutine gather_write

   ! ----------------------------------------------------------------
   ! Obstacle structures (legacy init.F): permanent mask from file,
   ! INT-truncated like legacy MASK_STRUC = INT(VarGlob).  Ghosts follow
   ! the PARALLEL legacy GetFile (seam exchange + wall replication);
   ! NOTE: serial legacy reads the interior only and leaves wall ghosts
   ! at 1 — a structure touching the boundary diverges between the two
   ! legacy builds, and we reproduce the parallel one (punch-listed).
   ! ----------------------------------------------------------------
   subroutine load_obstacle(this)
      class(type_model_main), intent(inout) :: this

      real(SP), allocatable :: rstruc(:, :)

      associate (g => this%grid)
         allocate (rstruc(g%lp%mloc, g%lp%nloc), source=1.0_SP)
         call read_field_ascii(this%env, this%obstacle%obstacle_file%root, &
                               g, rstruc)
         call ghost_fill_replicate(this, rstruc)
         this%fields%mask_struc = int(rstruc)
      end associate

   end subroutine load_obstacle

   ! ----------------------------------------------------------------
   ! Hot start (legacy INITIAL_UVZ): load eta (+u/v, mask) from the
   ! configured files; ghosts follow legacy GetFile — MPI seams carry
   ! neighbour data, physical walls replicate the edge value.  Bed
   ! deformation subtracts eta from the still-water depth (cell
   ! centres only, matching legacy).
   ! ----------------------------------------------------------------
   ! ----------------------------------------------------------------
   ! t=0 fields from file (initial: fields, the IC-flavored
   ! INITIAL_UVZ): eta (+u/v) through the file_spec reader, ghosts
   ! replicated like the hot-start path.  No bed handling — a
   ! deformed bed is a grid.bathymetry concern.
   ! ----------------------------------------------------------------
   subroutine load_initial_fields(this)
      use model_field_input_mod, only: read_field
      class(type_model_main), intent(inout) :: this

      associate (ini => this%initial, f => this%fields, g => this%grid)

         call read_field(this%env, ini%eta_spec, g, f%eta)
         call ghost_fill_replicate(this, f%eta)
         if (.not. ini%fields_no_uv) then
            call read_field(this%env, ini%u_spec, g, f%u)
            call read_field(this%env, ini%v_spec, g, f%v)
            call ghost_fill_replicate(this, f%u)
            call ghost_fill_replicate(this, f%v)
         end if

      end associate

   end subroutine load_initial_fields

   subroutine load_hot_start(this)
      class(type_model_main), intent(inout) :: this

      real(SP), allocatable :: rmask(:, :)

      associate (hs => this%hot_start, f => this%fields, g => this%grid)

         call read_field_ascii(this%env, hs%eta_file%root, g, f%eta)
         call ghost_fill_replicate(this, f%eta)
         if (.not. hs%no_uv_file) then
            call read_field_ascii(this%env, hs%u_file%root, g, f%u)
            call read_field_ascii(this%env, hs%v_file%root, g, f%v)
            call ghost_fill_replicate(this, f%u)
            call ghost_fill_replicate(this, f%v)
         else
            f%u = 0.0_SP
            f%v = 0.0_SP
         end if

         if (.not. hs%no_mask_file) then
            allocate (rmask(g%lp%mloc, g%lp%nloc), source=1.0_SP)
            call read_field_ascii(this%env, hs%mask_file%root, g, rmask)
            call ghost_fill_replicate(this, rmask)
            f%mask = int(rmask)
         end if

         if (hs%bed_deformation) f%depth = f%depth - f%eta

      end associate

   end subroutine load_hot_start

   ! Restart from a checkpoint set: read core.bin into the live core fields,
   ! restore the saved time (drives engine%init).  Only eta gets a crude edge
   ! fill for the interim setup ops; stepper%restart_sync later refills every
   ! ghost with the parity exchange.  The per-module dispatch (later:
   ! wavemaker.bin, sediment.bin, ...) lands here; a missing module bin =>
   ! cold-init that module (design-hotstart mode-3).
   subroutine load_checkpoint(this)
      class(type_model_main), intent(inout) :: this

      character(:), allocatable :: dir
      real(SP) :: t

      associate (hs => this%hot_start, f => this%fields, g => this%grid)
         dir = trim(hs%checkpoint)
         if (len(dir) > 0 .and. dir(len(dir):len(dir)) /= "/") dir = dir//"/"

         allocate (this%chk_pflux(g%lp%mloc, g%lp%nloc), source=0.0_SP)
         allocate (this%chk_qflux(g%lp%mloc, g%lp%nloc), source=0.0_SP)
         call read_checkpoint_core(this%env, g, f, this%chk_pflux, this%chk_qflux, t, dir)
         hs%time = t

         call ghost_fill_replicate(this, f%eta)
      end associate

   end subroutine load_checkpoint

   ! Write the checkpoint set to output%checkpoint (mkdir + core.bin now; later
   ! per-module bins appended behind this dispatcher).
   subroutine write_checkpoint_set(this, time)
      class(type_model_main), intent(inout) :: this
      real(SP), intent(in) :: time

      type(type_path) :: cdir
      character(:), allocatable :: dir
      logical :: ok

      dir = trim(this%output%checkpoint)
      if (len(dir) == 0) return
      if (dir(len(dir):len(dir)) /= "/") dir = dir//"/"

      if (this%env%comm%is_io_node()) then
         cdir = type_path(dir)
         if (.not. cdir%is_dir()) ok = cdir%mkdir()
      end if
      call this%env%comm%barrier()

      call write_checkpoint_core(this%env, this%env%comm, this%grid, &
                                 this%fields, this%registry%get("p_flux"), &
                                 this%registry%get("q_flux"), time, dir)

   end subroutine write_checkpoint_set

   ! Halo exchange + edge replication at physical walls (legacy
   ! GetFile global ghost fill).
   subroutine ghost_fill_replicate(this, arr)
      use core_constants_mod, only: N_GHOST
      class(type_model_main), intent(inout) :: this
      real(SP), intent(inout) :: arr(:, :)

      integer :: k

      call this%grid%halo_exchange(arr)
      ! y walls first, then x walls over the full row range — corner
      ! ghosts land on the corner interior value like legacy
      associate (lp => this%grid%lp, g => this%grid)
         do k = 1, N_GHOST
            if (g%is_right_boundary) arr(:, lp%jb - k) = arr(:, lp%jb)
            if (g%is_left_boundary) arr(:, lp%je + k) = arr(:, lp%je)
         end do
         do k = 1, N_GHOST
            if (g%is_back_boundary) arr(lp%ib - k, :) = arr(lp%ib, :)
            if (g%is_shore_boundary) arr(lp%ie + k, :) = arr(lp%ie, :)
         end do
      end associate

   end subroutine ghost_fill_replicate

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
      real(SP), pointer :: pf(:, :), qf(:, :)

      call this%setup()
      call this%friction%init_compute(this%grid)
      call this%sponge%init_compute(this%grid)
      ! Sponge friction drag composes additively onto friction's constant base
      ! (ledger 8d: was a max-merge; identical while Cd = 0 in the sponge zone,
      ! as in all current tests); sync_base then hands it to the effective Cd
      call this%sponge%merge_friction(this%friction%cd_base, this%fields%depth)
      call this%friction%sync_base()
      ! legacy TIDE_INITIAL runs after INITIALIZATION — profiles, DATA
      ! series open, and the (inert-on-sponge) REMOVE_SPONGE disable
      call this%tide%init_compute(this%grid)
      ! GEN_ABS rides the ABS wavemaker relaxation (legacy sponge.F
      ! reads TIDE_MODULE state)
      this%wavemaker%tide => this%tide
      ! legacy PRECIPITATION_INITIAL runs after INITIALIZATION — index
      ! file open, first frame into the high bracket
      call this%precipitation%init_compute(this%grid)
      ! legacy init.F:850 derives the blow-up threshold from the bathymetry
      ! (100 * max|Depth|), so it must land after the depth is built
      call this%output%resolve_blowup(this%grid, this%fields%depth)

      ! legacy ALLOCATE_FOAM/INITIALIZATION_FOAM: zeroed state, no
      ! dependence on the bathymetry or any other component
      call this%foam%init_compute(this%grid)
      ! legacy TRACER_INITIAL: reads the tracker table and locates every
      ! tracker on the grid-point lattice, so the grid must be spaced
      call this%tracer%init_compute(this%grid, this%env, &
                                    this%output%result_folder, &
                                    this%simulation%t_start)
      ! resistance series wiring (nee OUT_VESSEL/PLOT_INTV_VESSEL): the
      ! output: vessel: block owns the request, the vessel model runs it
      if (this%output%vessel_series_on .and. .not. this%vessel%is_activated) &
         call this%env%log%exit_on_error( &
         "output: vessel: requested but there is no vessel: section")
      this%vessel%out_vessel = this%output%vessel_series_on
      this%vessel%plot_intv = this%output%vessel_interval
      ! legacy VESSEL_INITIAL: opens every vessel_NNNNN, reads its geometry and
      ! first track point, and builds the ghost-inclusive lattice the hull frame
      ! is evaluated on -- so the grid must already be spaced
      call this%vessel%init_compute(this%grid, this%env, &
                                    this%output%result_folder, &
                                    this%simulation%t_start)
      ! legacy SEDIMENT_INITIAL: zeroed transport state plus the grain
      ! parameters, which depend on config alone
      call this%sediment%init_compute(this%grid, this%env, this%fields%depth)
      ! legacy METEO_INITIAL: builds the ghost-inclusive pressure lattice and
      ! opens the storm-track file, so the grid must already be spaced
      call this%meteo%init_compute(this%grid)
      call this%wavemaker%init_compute(this%grid, this%physics%periodic, &
                                       this%env, this%physics%Beta_ref)
      call this%obstacle%init_compute(this%grid, this%geometry%dx, &
                                      this%geometry%dy, this%env)
      call this%means%init_compute(this%grid, this%env%comm, this%output)

      call stepper%init(this%env, this%grid, this%fields, this%physics, &
                        this%numerics, this%breaking, this%friction, &
                        this%simulation, this%output, this%wavemaker, &
                        this%sponge, this%obstacle, this%means, this%tide, &
                        this%precipitation, this%subgrid, this%foam, &
                        this%tracer, this%vessel, this%sediment, this%meteo, &
                        restart=this%hot_start%use_checkpoint)
      call stepper%register_output(this%registry)

      ! checkpoint restart: the loaded state is the full live core set.  Copy
      ! the staged interface flux (p_flux/q_flux) into the now-registered
      ! workspace, then refill the parity ghosts and rebuild H before the run.
      if (this%hot_start%use_checkpoint) then
         pf => this%registry%get("p_flux")
         qf => this%registry%get("q_flux")
         associate (lp => this%grid%lp)
            pf(lp%ib:lp%ie, lp%jb:lp%je) = this%chk_pflux(lp%ib:lp%ie, lp%jb:lp%je)
            qf(lp%ib:lp%ie, lp%jb:lp%je) = this%chk_qflux(lp%ib:lp%ie, lp%jb:lp%je)
         end associate
         call stepper%restart_sync()
      end if

      call build_field_channel(this, output_mgr)
      call this%stations%init_compute(this%grid, this%env, this%fields, &
                                      this%output%stations_on, &
                                      this%output%stations_file, &
                                      this%output%result_folder, &
                                      this%output%stations_interval, &
                                      this%output%stations_buffer, &
                                      this%simulation%total_time)
      monitor%mgr => output_mgr
      monitor%registry => this%registry
      monitor%comm => this%env%comm
      monitor%stations => this%stations
      monitor%tracer => this%tracer
      monitor%vessel => this%vessel

      call engine%init(merge(this%hot_start%time, 0.0_SP, &
                             this%hot_start%is_activated), &
                       this%simulation%total_time, &
                       this%simulation%screen_interval)
      call engine%run(stepper, monitor, this%env%log)

      ! checkpoint the final state (this slice: end-of-run only)
      if (this%output%write_checkpoint) &
         call write_checkpoint_set(this, engine%clock%current_time)

      ! legacy calls STATIONS once after the loop (residual flush)
      call this%stations%finish()
      call output_mgr%finalize()
      call stepper%free()
      call this%means%free()
      call this%stations%free()
      call this%tide%free()
      call this%precipitation%free()
      call this%subgrid%free()
      call this%foam%free()
      call this%tracer%free()
      call this%vessel%free()
      call this%sediment%free()
      call this%meteo%free()

   end subroutine model_run

   subroutine output_monitor_step(this, t, dt, force)
      class(type_output_monitor), intent(inout) :: this
      real(SP), intent(in) :: t, dt
      logical, intent(in), optional :: force

      integer :: unit
      logical :: forced

      forced = .false.
      if (present(force)) forced = force

      call this%mgr%step(t, dt, this%registry, this%comm, force=forced)
      ! the forced final flush covers field frames only: stations flush
      ! their residual via finish(), tracer/vessel keep their cadence
      if (.not. forced) then
         if (associated(this%stations)) call this%stations%update(t, dt)
         ! legacy OUTPUT_TRACKING: loop-top, on the PLOT_COUNT_TRACKING cadence
         if (associated(this%tracer)) call this%tracer%write_output(t, dt)
         ! legacy OUTPUT_VESSEL: resistance time series on the PLOT_COUNT_VESSEL cadence
         if (associated(this%vessel)) call this%vessel%write_output(t, dt)
      end if

      ! Legacy PREVIEW appends "time dt" to time_dt.out (run dir, not
      ! result_folder) at every field-frame flush; io rank only here.
      if (this%mgr%channels(1)%fired .and. this%comm%is_io_node()) then
         open (newunit=unit, file='time_dt.out', status='unknown', &
               position='append', action='write')
         write (unit, *) t, dt
         close (unit)
      end if
   end subroutine output_monitor_step

   ! ----------------------------------------------------------------
   ! Bridge the legacy-style output flags to one snapshot field
   ! channel (full output-block YAML: Step 7).  File naming and frame
   ! numbering follow legacy PREVIEW: <prefix>_NNNNN with the
   ! initial-condition frame at 00000 (icount_start=-1), plus dep.out
   ! and a truncated time_dt.out.  MASK/MASK9 and the legacy P/Q
   ! interface fluxes ride the stepper's register_output entries.
   ! ----------------------------------------------------------------
   subroutine build_field_channel(this, mgr)
      class(type_model_main), intent(inout), target :: this
      type(type_output_manager), intent(inout) :: mgr

      character(len=16) :: vars(40), prefs(40)
      character(len=8) :: stats(1)
      character(:), allocatable :: folder, fmt
      real(SP) :: dummy_coord(1)
      type(type_path) :: outdir
      integer :: nv, unit
      logical :: ok

      associate (out => this%output)

         nv = 0
         if (out%OUT_ETA) call add_var(vars, prefs, nv, "eta", "eta")
         if (out%OUT_U) call add_var(vars, prefs, nv, "u", "u")
         if (out%OUT_V) call add_var(vars, prefs, nv, "v", "v")
         if (out%OUT_Hmax) call add_var(vars, prefs, nv, "h_max", "hmax")
         if (out%OUT_Hmin) call add_var(vars, prefs, nv, "h_min", "hmin")
         if (out%OUT_Umax) call add_var(vars, prefs, nv, "u_max", "umax")
         if (out%OUT_MFmax) call add_var(vars, prefs, nv, "mf_max", "MFmax")
         if (out%OUT_VORmax) call add_var(vars, prefs, nv, "vort_max", "VORmax")
         if (this%output%out_arr_time) call add_var(vars, prefs, nv, "arr_time", "time")
         ! Legacy gates the nubrk write on VISCOSITY_BREAKING, not OUT_NU alone
         if (out%OUT_NU .and. this%physics%viscosity_breaking) &
            call add_var(vars, prefs, nv, "nu_break", "nubrk")
         ! Legacy gates the age write on SHOW_BREAKING (io.F:1442-1447);
         ! roller/undertow write whenever flagged (zero-filled files
         ! when no breaker runs, like legacy's unconditional arrays)
         if (out%OUT_AGE .and. this%breaking%show_breaking) &
            call add_var(vars, prefs, nv, "age_break", "age")
         if (out%OUT_ROLLER) call add_var(vars, prefs, nv, "roller_flux", "roller")
         if (out%OUT_UNDERTOW) then
            call add_var(vars, prefs, nv, "undertow_u", "U_undertow")
            call add_var(vars, prefs, nv, "undertow_v", "V_undertow")
         end if
         ! legacy PREVIEW writes FoamEta_ with no OUT_ gate — a -DFOAM
         ! build always dumps it
         if (this%foam%is_activated) &
            call add_var(vars, prefs, nv, "eta_foam", "FoamEta")
         ! Pves_/VesUp_/VesVp_ (nee legacy PREVIEW under OUT_VESSEL, now
         ! explicit variables: entries).  The jet pair is the ONLY observable
         ! the propeller has -- it feeds nothing back into the flow -- so
         ! without these a dead jet and a live one are indistinguishable.
         if ((out%OUT_Pves .or. out%OUT_VesUp .or. out%OUT_VesVp) &
             .and. .not. this%vessel%is_activated) &
            call this%env%log%exit_on_error( &
            "output: variables: Pves/VesUp/VesVp require the vessel: section")
         if ((out%OUT_VesUp .or. out%OUT_VesVp) &
             .and. .not. this%vessel%propeller) &
            call this%env%log%exit_on_error( &
            "output: variables: VesUp/VesVp require vessel: propeller: true")
         if (out%OUT_Pves) call add_var(vars, prefs, nv, "vessel_pressure", "Pves")
         if (out%OUT_VesUp) call add_var(vars, prefs, nv, "vessel_up", "VesUp")
         if (out%OUT_VesVp) call add_var(vars, prefs, nv, "vessel_vp", "VesVp")
         ! Pstorm_ (nee legacy OUTPUT_METEO under OUT_METEO, io.F:1709-1726)
         ! exists for every spatial pressure model; wind-only fields have no
         ! Pstorm.  Ustorm_/Vstorm_ are the Holland gradient wind.
         if (out%OUT_Pstorm) then
            if (.not. (this%meteo%is_activated &
                       .and. (this%meteo%meteo_gausian &
                              .or. this%meteo%wind_holland_model &
                              .or. this%meteo%slide_model))) &
               call this%env%log%exit_on_error( &
               "output: variables: Pstorm requires a meteo pressure model "// &
               "(gaussian/holland/slide)")
            call add_var(vars, prefs, nv, "meteo_pressure", "Pstorm")
         end if
         if (out%OUT_Ustorm .or. out%OUT_Vstorm) then
            if (.not. (this%meteo%is_activated .and. this%meteo%wind_holland_model)) &
               call this%env%log%exit_on_error( &
               "output: variables: Ustorm/Vstorm require meteo: holland:")
            if (out%OUT_Ustorm) call add_var(vars, prefs, nv, "meteo_wind_u", "Ustorm")
            if (out%OUT_Vstorm) call add_var(vars, prefs, nv, "meteo_wind_v", "Vstorm")
         end if
         ! legacy writes its sediment fields straight out of PREVIEW, ungated
         ! (OUTPUT_SEDIMENT, which PLOT_INTV_SEDIMENT gates, is an empty stub),
         ! so every one of these rides the ordinary plot cadence.  dep_ is the
         ! evolving bed — the only observable of the morphology.
         if (this%sediment%is_activated) then
            call add_var(vars, prefs, nv, "sediment_c", "C")
            call add_var(vars, prefs, nv, "sediment_pickup", "Pick")
            call add_var(vars, prefs, nv, "sediment_depo", "Depo")
            call add_var(vars, prefs, nv, "sediment_pavg", "Pavg")
            call add_var(vars, prefs, nv, "sediment_davg", "Davg")
            call add_var(vars, prefs, nv, "sediment_dchgs", "DchgS")
            call add_var(vars, prefs, nv, "sediment_dchgb", "DchgB")
            call add_var(vars, prefs, nv, "sediment_bedfx", "BedFx")
            call add_var(vars, prefs, nv, "sediment_bedfy", "BedFy")
            call add_var(vars, prefs, nv, "sediment_bedstr", "BedStr")
            call add_var(vars, prefs, nv, "sediment_aval", "Aval")
            call add_var(vars, prefs, nv, "sediment_avalac", "AvalAc")
            call add_var(vars, prefs, nv, "depth", "dep")
         end if
         if (out%OUT_MASK) call add_var(vars, prefs, nv, "mask", "mask")
         if (out%OUT_MASK9) call add_var(vars, prefs, nv, "mask9", "mask9")
         ! Legacy P/Q are the interface fluxes, not the registry p/q (Ubar)
         if (out%OUT_P) call add_var(vars, prefs, nv, "p_flux", "p")
         if (out%OUT_Q) call add_var(vars, prefs, nv, "q_flux", "q")

         folder = trim(out%result_folder)
         if (folder(len(folder):len(folder)) /= "/") folder = folder//"/"
         if (this%env%comm%is_io_node()) then
            outdir = type_path(folder)
            if (.not. outdir%is_dir()) ok = outdir%mkdir()
            ! Fresh time_dt.out per run (legacy leaves stale tails behind)
            open (newunit=unit, file='time_dt.out', status='replace', action='write')
            close (unit)
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
                                   interval=this%output%interval, &
                                   result_folder=folder, format=fmt, &
                                   coords_x=dummy_coord, coords_y=dummy_coord, &
                                   n_coords=0, grid=this%grid, &
                                   comm=this%env%comm, &
                                   file_prefixes=prefs, &
                                   icount_start=merge( &
                                   this%hot_start%output_start_number - 1, &
                                   -1, this%hot_start%is_activated))

         ! legacy PREVIEW first-frame block: OUT_DEPTH .OR. BREAKWATER
         ! writes BOTH dep.out and cd_breakwater.out (zeros when no
         ! breakwater)
         if (out%depth_out .or. this%obstacle%breakwater) then
            call write_static_field(this, mgr%channels(1), &
                                    "depth", folder//"dep.out", fmt)
            block
               real(SP), allocatable :: zeros(:, :)
               if (allocated(this%obstacle%cd_breakwater)) then
                  call gather_write(this, mgr%channels(1)%gatherer, &
                                    this%obstacle%cd_breakwater, &
                                    folder//"cd_breakwater.out", fmt)
               else
                  allocate (zeros(this%grid%lp%mloc, this%grid%lp%nloc), &
                            source=0.0_SP)
                  call gather_write(this, mgr%channels(1)%gatherer, zeros, &
                                    folder//"cd_breakwater.out", fmt)
               end if
            end block
         end if

      end associate

   end subroutine build_field_channel

   ! Gather one registry field and write it as a static (non-series)
   ! file — legacy dep.out.  Reuses the field channel's gatherer.
   subroutine write_static_field(this, ch, var, fname, fmt)
      use core_constants_mod, only: N_GHOST
      class(type_model_main), intent(inout), target :: this
      type(type_output_channel), intent(inout) :: ch
      character(*), intent(in) :: var, fname, fmt

      real(SP), pointer :: fld(:, :)
      real(SP), allocatable :: glob(:, :)

      fld => this%registry%get(var)
      if (this%env%comm%is_io_node()) then
         allocate (glob(ch%gatherer%M, ch%gatherer%N))
      else
         allocate (glob(1, 1))
      end if
      associate (ng => N_GHOST, nx => this%grid%local_nx, ny => this%grid%local_ny)
         call ch%gatherer%gather_field(fld(ng + 1:ng + nx, ng + 1:ng + ny), &
                                       glob, this%env%comm)
      end associate
      if (this%env%comm%is_io_node()) call write_field_file(fname, glob, fmt)
   end subroutine write_static_field

   subroutine add_var(vars, prefs, nv, name, prefix)
      character(len=*), intent(inout) :: vars(:), prefs(:)
      integer, intent(inout) :: nv
      character(len=*), intent(in) :: name, prefix
      nv = nv + 1
      vars(nv) = name
      prefs(nv) = prefix
   end subroutine add_var

   subroutine model_finalize(this)
      use mpi_f08, only: MPI_Finalize
      class(type_model_main), intent(inout) :: this
      integer :: ierr
      call this%env%finalize()
      call MPI_Finalize(ierr)
   end subroutine model_finalize

end module model_main_mod
