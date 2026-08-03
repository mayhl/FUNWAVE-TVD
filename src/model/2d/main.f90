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

   use core_constants_mod, only: SP, LARGE, DEG2RAD
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
   use model_wavemaker_mod, only: type_model_wavemaker, read_wavemakers
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
   use model_stepper_2d_mod, only: type_model_stepper_2d

   implicit none

   ! Loop-top output call site for the stepper engine: wraps the
   ! output manager + registry + comm (the engine itself is
   ! output-free — libcore_output links libcore_engine).
   type, extends(type_engine_monitor) :: type_output_monitor
      type(type_output_manager), pointer :: mgr => null()
      type(type_field_registry), pointer :: registry => null()
      type(type_comm), pointer :: comm => null()
      type(type_model_tracer), pointer :: tracer => null()
      type(type_model_vessel), pointer :: vessel => null()
      ! vector-derived scratch refresh hooks (null unless channels ask)
      type(type_fields_2d), pointer :: fields => null()
      real(SP), pointer :: vec_mag(:, :) => null()
      real(SP), pointer :: vec_dir(:, :) => null()
   contains
      procedure :: step => output_monitor_step
   end type type_output_monitor

   type, public :: type_model_main
      type(type_env) :: env

      type(type_model_geometry)   :: geometry
      type(type_model_simulation) :: simulation
      type(type_model_hot_start)  :: hot_start
      type(type_model_initial)    :: initial
      ! wavemaker: entries (mapping = one, sequence = many); at most one
      ! internal source until composition lands (read_wavemakers gates)
      type(type_model_wavemaker), allocatable :: wavemakers(:)
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
      ! Vector-derived output scratch (velocity.mag/.dir): registered when
      ! a channel requests them, refreshed before every manager step
      logical                   :: need_vec_mag = .false.
      logical                   :: need_vec_dir = .false.
      real(SP), allocatable     :: vec_mag(:, :), vec_dir(:, :)
      ! Time-averaged statistics (legacy MIXING_STUFF port) — engine
      ! path only, initialised in run()
      type(type_model_means)    :: means
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
      ! breaking before wavemakers: the zone-override keys variant-gate
      ! on breaking%model (same cross-component pattern as boundaries)
      call this%breaking%read_input(this%env)
      call read_wavemakers(this%env, this%wavemakers, this%breaking)
      call this%obstacle%read_input(this%env)
      call this%friction%read_input(this%env)
      call this%numerics%read_input(this%env)
      this%output%min_spacing = min(this%geometry%dx, this%geometry%dy)
      call this%output%read_input(this%env)
      call this%physics%read_input(this%env)
      call boundaries_read_input(this%env, this%sponge, this%tide, this%physics, &
                                 this%wavemakers)
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
      ! breaking before wavemakers: the zone-override keys variant-gate
      ! on breaking%model (same cross-component pattern as boundaries)
      call this%breaking%read_input(this%env)
      call read_wavemakers(this%env, this%wavemakers, this%breaking)
      call this%obstacle%read_input(this%env)
      call this%friction%read_input(this%env)
      call this%numerics%read_input(this%env)
      this%output%min_spacing = min(this%geometry%dx, this%geometry%dy)
      call this%output%read_input(this%env)
      call this%physics%read_input(this%env)
      call boundaries_read_input(this%env, this%sponge, this%tide, this%physics, &
                                 this%wavemakers)
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
      ! show_breaking is DERIVED (key retired): the breaker-diagnostics pass
      ! runs when it IS the physics (viscosity breaking, incl. the roller
      ! forcing above) or when a breaker field is requested for output —
      ! verified solution-neutral bitwise in show-only mode
      this%breaking%show_breaking = this%physics%viscosity_breaking &
                                    .or. this%output%OUT_AGE &
                                    .or. this%output%OUT_ROLLER &
                                    .or. this%output%OUT_UNDERTOW

      ! wavemaker-zone breaking overrides ride the wavemaker entry
      ! (source.breaking) but land in the global breaking fields until the
      ! coefficient-field assembler exists
      do i = 1, size(this%wavemakers)
         if (this%wavemakers(i)%has_breaking_override) then
            this%breaking%wavemaker_cbrk = this%wavemakers(i)%breaking_cbrk
            this%breaking%wavemaker_visbrk = this%wavemakers(i)%breaking_visbrk
         end if
      end do

      call this%geometry%build_grid(this%env%comm, this%grid, &
                                    periodic_y=this%physics%periodic, &
                                    periodic_x=this%physics%periodic_x)
      call this%fields%alloc(this%grid)
      ! WAVEMAKER_VIS and the show-only display mode need nu_break/age
      ! too (legacy allocates the breaking arrays for all options since
      ! fyshi 01/15/2024)
      if (this%physics%viscosity_breaking .or. this%breaking%wavemaker_vis &
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
      ! (re)staggering; the still-water offset comes after correction
      ! (init.F:799-810) and shifts the wavemaker reference depths too
      ! FUTURE: stamp water_level next to vertical_datum in the NetCDF
      ! output globals once the georef quartet lands
      if (this%geometry%bathy_correction) call apply_bathy_correction(this)
      if (this%geometry%water_level /= 0.0_SP) then
         this%fields%depth = this%fields%depth + this%geometry%water_level
         call stagger_depth(this%grid%lp, this%fields%depth, &
                            this%fields%depth_x, this%fields%depth_y)
         do i = 1, size(this%wavemakers)
            call this%wavemakers(i)%apply_water_level(this%geometry%water_level)
         end do
      end if
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
         ! t = 0 dispersion weight mirrors mask9 verbatim — no taper at
         ! init, matching the no-SWE-zeroing convention above (ledger 8c);
         ! the first in-loop update_swe_weight applies the ramp
         f%swe_w = real(f%mask9, SP)
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
      select case (this%output%format(1:1))
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

      select case (this%output%format(1:1))
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
      integer :: i

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
      do i = 1, size(this%wavemakers)
         this%wavemakers(i)%tide => this%tide
      end do
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
      do i = 1, size(this%wavemakers)
         call this%wavemakers(i)%init_compute(this%grid, this%physics%periodic, &
                                              this%env, this%physics%Beta_ref)
      end do
      call this%obstacle%init_compute(this%grid, this%geometry%dx, &
                                      this%geometry%dy, this%env)
      call this%means%init_compute(this%grid, this%env%comm, this%output)

      call stepper%init(this%env, this%grid, this%fields, this%physics, &
                        this%numerics, this%breaking, this%friction, &
                        this%simulation, this%output, this%wavemakers, &
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
      monitor%mgr => output_mgr
      monitor%registry => this%registry
      monitor%comm => this%env%comm
      monitor%tracer => this%tracer
      monitor%vessel => this%vessel
      monitor%fields => this%fields
      if (this%need_vec_mag) monitor%vec_mag => this%vec_mag
      if (this%need_vec_dir) monitor%vec_dir => this%vec_dir

      call engine%init(merge(this%hot_start%time, 0.0_SP, &
                             this%hot_start%is_activated), &
                       this%simulation%total_time, &
                       this%simulation%screen_interval)
      call engine%run(stepper, monitor, this%env%log)

      ! checkpoint the final state (this slice: end-of-run only)
      if (this%output%write_checkpoint) &
         call write_checkpoint_set(this, engine%clock%current_time)

      call output_mgr%finalize()
      call stepper%free()
      call this%means%free()
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

      ! vector-derived scratch refresh (flat, only when channels ask)
      if (associated(this%vec_mag)) &
         this%vec_mag = sqrt(this%fields%u**2 + this%fields%v**2)
      if (associated(this%vec_dir)) &
         this%vec_dir = atan2(this%fields%v, this%fields%u)/DEG2RAD

      call this%mgr%step(t, dt, this%registry, this%comm, force=forced)
      ! the forced final flush covers field frames only: tracer/vessel
      ! keep their cadence
      if (.not. forced) then
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
      use core_output_gatherer_mod, only: type_output_gatherer
      class(type_model_main), intent(inout), target :: this
      type(type_output_manager), intent(inout) :: mgr

      character(:), allocatable :: folder, fmt
      type(type_path) :: outdir
      integer :: unit
      logical :: ok

      associate (out => this%output)

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

         select case (out%format(1:1))
         case ("B", "b")
            fmt = "binary"
         case ("N", "n")
            fmt = "netcdf"
         case ("P", "p")
            fmt = "pnetcdf"
         case default
            fmt = "ascii"
         end select

         ! channels own every stream since the flags-list retirement;
         ! the manager array sizes to the deck's channel count
         allocate (mgr%channels(max(1, this%output%n_channels)))
         mgr%n_channels = 0

         ! legacy PREVIEW first-frame block: OUT_DEPTH .OR. BREAKWATER
         ! writes BOTH dep.out and cd_breakwater.out (zeros when no
         ! breakwater).  Statics ride their own gatherer since the
         ! channels took over the streams.
         if (out%depth_out .or. this%obstacle%breakwater) then
            block
               type(type_output_gatherer) :: statics
               real(SP), allocatable :: zeros(:, :)
               call statics%init_field(this%grid, this%env%comm)
               call write_static_field(this, statics, &
                                       "depth", folder//"dep.out", fmt)
               if (allocated(this%obstacle%cd_breakwater)) then
                  call gather_write(this, statics, &
                                    this%obstacle%cd_breakwater, &
                                    folder//"cd_breakwater.out", fmt)
               else
                  allocate (zeros(this%grid%lp%mloc, this%grid%lp%nloc), &
                            source=0.0_SP)
                  call gather_write(this, statics, zeros, &
                                    folder//"cd_breakwater.out", fmt)
               end if
            end block
         end if

      end associate

      call build_point_channels(this, mgr, folder)

   end subroutine build_field_channel

   ! ----------------------------------------------------------------
   ! Deck-defined point channels (output.channels: on geometries:).
   ! Variables are registry names validated against the field registry;
   ! a point falling in no rank's subdomain is a config error.
   ! ----------------------------------------------------------------
   subroutine build_point_channels(this, mgr, folder)
      use mpi_f08
      use core_output_channel_mod, only: type_var_meta, type_channel_derived
      use model_field_metadata_mod, only: field_meta
      use model_output_mod, only: VEC_DERIVED, PROD_DERIVED, PROD_SRC, &
                                  PROD_STAT, PROD_SCALE
      class(type_model_main), intent(inout), target :: this
      type(type_output_manager), intent(inout) :: mgr
      character(*), intent(in) :: folder

      type(type_var_meta), allocatable :: vmeta(:)
      type(type_channel_derived), allocatable :: dspecs(:)
      character(:), allocatable :: pfmt
      real(SP) :: cwin
      logical :: is_field
      integer :: k, iv, kc, n_owned, ierr, idn, ip, isrc, froot, ic0
      character(16) :: owned_str, total_str

      ! Vector-derived scratch: register once when any channel asks;
      ! refreshed each manager step (zero until the first step)
      do k = 1, this%output%n_channels
         do iv = 1, size(this%output%channels(k)%variables)
            select case (trim(this%output%channels(k)%variables(iv)))
            case ("velocity.mag")
               this%need_vec_mag = .true.
            case ("velocity.dir")
               this%need_vec_dir = .true.
            end select
         end do
      end do
      if (this%need_vec_mag) then
         allocate (this%vec_mag, mold=this%fields%u)
         this%vec_mag = 0.0_SP
         call this%registry%register("velocity.mag", this%vec_mag)
      end if
      if (this%need_vec_dir) then
         allocate (this%vec_dir, mold=this%fields%u)
         this%vec_dir = 0.0_SP
         call this%registry%register("velocity.dir", this%vec_dir)
      end if

      ! Shared root: created once when any POINT channel resolves to
      ! netcdf (per-channel format:, else the deck default).  Layout
      ! 'single' shares output.nc with the field stream.
      do k = 1, this%output%n_channels
         if (this%output%geometries(this%output%channels(k)%geom_idx)%geom_type &
             == "field") cycle
         if (point_format(this, k) == "netcdf") then
            if (this%output%layout == "single") then
               call mgr%open_diagnostics(folder, this%env%comm, fname="output.nc")
            else
               call mgr%open_diagnostics(folder, this%env%comm)
            end if
            exit
         end if
      end do

      do k = 1, this%output%n_channels
         associate (cfg => this%output%channels(k), &
                    geom => this%output%geometries(this%output%channels(k)%geom_idx))
            is_field = geom%geom_type == "field"

            do iv = 1, size(cfg%variables)
               if (.not. this%registry%has(trim(cfg%variables(iv)))) &
                  call this%env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                  "': '"//trim(cfg%variables(iv))// &
                                                  "' is not a registered output field")
            end do
            allocate (vmeta(size(cfg%variables)))
            do iv = 1, size(cfg%variables)
               vmeta(iv) = derived_aware_meta(trim(cfg%variables(iv)))
            end do

            ! catalogue requests -> runtime derived specs (source index
            ! resolved within this channel's variables list)
            allocate (dspecs(max(1, cfg%n_derived)))
            do idn = 1, cfg%n_derived
               do ip = 1, size(PROD_DERIVED)
                  if (cfg%derived(idn) == PROD_DERIVED(ip)) exit
               end do
               isrc = 0
               do iv = 1, size(cfg%variables)
                  if (trim(cfg%variables(iv)) == trim(PROD_SRC(ip))) isrc = iv
               end do
               dspecs(idn) = type_channel_derived(name=PROD_DERIVED(ip), iv=isrc, &
                                                  stat=PROD_STAT(ip), scale=PROD_SCALE(ip))
            end do

            ! field channels inherit the full deck format set; netcdf
            ! layouts wire per channel (nee the legacy stream's block):
            ! single = a group <id> in shared output.nc, chunked = a
            ! size-derived window, per_stream = one <id>.nc
            froot = -1
            cwin = 0.0_SP
            ic0 = 0
            if (is_field) then
               pfmt = trim(cfg%format)
               if (len_trim(pfmt) == 0) pfmt = trim(this%output%format)
               ! legacy frame-counter base: files start at _00000; a
               ! hotstart resumes the numbering where it left off
               ic0 = merge(this%hot_start%output_start_number - 1, -1, &
                           this%hot_start%is_activated)
               if (pfmt == "netcdf" .or. pfmt == "pnetcdf") then
                  call wire_field_netcdf(this, mgr, cfg, folder, pfmt, froot, cwin)
               end if
            else
               pfmt = point_format(this, k)
            end if

            kc = mgr%n_channels + 1
            call mgr%channels(kc)%init(id=cfg%name, geom_type=geom%geom_type, &
                                       variables=cfg%variables, &
                                       n_vars=size(cfg%variables), &
                                       statistics=cfg%statistics, &
                                       n_stats=cfg%n_stats, &
                                       snapshot=cfg%snapshot, &
                                       t_start=merge(cfg%t_start, &
                                                     this%simulation%t_start, &
                                                     cfg%has_t_start), &
                                       interval=cfg%interval, &
                                       result_folder=folder, format=pfmt, &
                                       coords_x=geom%x, coords_y=geom%y, &
                                       n_coords=size(geom%x), grid=this%grid, &
                                       comm=this%env%comm, var_meta=vmeta, &
                                       icount_start=ic0, &
                                       diag_ncid=merge(froot, mgr%diag_ncid, froot >= 0), &
                                       chunk_window=cwin, &
                                       hidden=cfg%hidden, &
                                       derived=dspecs, n_derived=cfg%n_derived)
            mgr%n_channels = kc
            deallocate (vmeta, dspecs)

            if (.not. is_field) then
               call MPI_Allreduce(mgr%channels(kc)%n_local, n_owned, 1, &
                                  MPI_INTEGER, MPI_SUM, this%env%comm%id, ierr)
               if (n_owned /= size(geom%x)) then
                  write (owned_str, '(I0)') n_owned
                  write (total_str, '(I0)') size(geom%x)
                  call this%env%log%exit_on_error("output: channels: '"//cfg%name// &
                                                  "': only "//trim(owned_str)//" of "// &
                                                  trim(total_str)//" points of geometry '"// &
                                                  geom%name//"' fall inside the domain")
               end if
            end if
         end associate
      end do

   end subroutine build_point_channels

   ! netcdf layout wiring for one field channel (nee the legacy field
   ! stream's block): resolves the shared root / chunk window and logs
   ! the size prediction against max_file_size
   subroutine wire_field_netcdf(this, mgr, cfg, folder, pfmt, froot, cwin)
      use model_output_mod, only: type_channel_config
      class(type_model_main), intent(inout) :: this
      type(type_output_manager), intent(inout) :: mgr
      type(type_channel_config), intent(in) :: cfg
      character(*), intent(in) :: folder, pfmt
      integer, intent(out) :: froot
      real(SP), intent(out) :: cwin

      character(len=160) :: msg
      real(SP) :: cap_bytes, rate, duration
      integer :: n_win, nv

      froot = -1
      cwin = 0.0_SP
      nv = count(.not. cfg%hidden)*(1 + cfg%n_stats) + cfg%n_derived
      cap_bytes = this%output%max_file_size*1024.0_SP**3
      rate = real(nv, SP)*real(this%grid%M, SP)*real(this%grid%N, SP) &
             *8.0_SP/cfg%interval
      duration = this%simulation%total_time - this%simulation%t_start

      select case (this%output%layout)
      case ("single")
         if (pfmt == "pnetcdf") call this%env%log%exit_on_error( &
            "output: layout: single needs netcdf groups -- "// &
            "PNETCDF supports per_stream or chunked")
         call mgr%open_diagnostics(folder, this%env%comm, fname="output.nc")
         froot = mgr%diag_ncid
      case ("chunked")
         n_win = int(min(cap_bytes/rate, duration)/cfg%interval)
         if (real(n_win, SP)*cfg%interval >= duration) n_win = n_win + 1
         cwin = real(max(1, n_win), SP)*cfg%interval
         write (msg, '(3a,F0.1,a,F0.3,a)') "output: channel '", trim(cfg%name), &
            "' chunked files span ", cwin, " s (~", &
            rate*cwin/1024.0_SP**3, " GB each)"
         call this%env%log%info(trim(msg))
      end select
      if (this%output%layout /= "chunked" .and. rate*duration > cap_bytes) then
         write (msg, '(3a,F0.1,a,F0.1,a)') "output: channel '", trim(cfg%name), &
            "' predicted stream size ", rate*duration/1024.0_SP**3, &
            " GB exceeds max_file_size ", this%output%max_file_size, &
            " GB -- consider layout: chunked"
         call this%env%log%warning(trim(msg))
      end if

   end subroutine wire_field_netcdf

   ! field_meta plus the vector-derived names it cannot know about
   function derived_aware_meta(name) result(m)
      use core_output_channel_mod, only: type_var_meta
      use model_field_metadata_mod, only: field_meta
      character(*), intent(in) :: name
      type(type_var_meta) :: m

      select case (trim(name))
      case ("velocity.mag")
         m%units = "m s-1"
         m%long_name = "depth-averaged speed"
      case ("velocity.dir")
         m%units = "degree"
         m%long_name = "depth-averaged velocity direction"
      case default
         m = field_meta(name)
      end select
   end function derived_aware_meta

   ! Point-channel format: the explicit format: key, else the deck
   ! default derived from the deck format.  pnetcdf also implies netcdf
   ! points — the parallel writer is field-only, points stay serial.
   function point_format(this, k) result(fmt)
      class(type_model_main), intent(in) :: this
      integer, intent(in) :: k
      character(:), allocatable :: fmt

      if (len_trim(this%output%channels(k)%format) > 0) then
         fmt = trim(this%output%channels(k)%format)
      else
         select case (this%output%format(1:1))
         case ("N", "n", "P", "p")
            fmt = "netcdf"
         case default
            fmt = "ascii"
         end select
      end if
   end function point_format

   ! Gather one registry field and write it as a static (non-series)
   ! file — legacy dep.out.  Reuses the field channel's gatherer.
   subroutine write_static_field(this, gatherer, var, fname, fmt)
      use core_constants_mod, only: N_GHOST
      use core_output_gatherer_mod, only: type_output_gatherer
      class(type_model_main), intent(inout), target :: this
      type(type_output_gatherer), intent(in) :: gatherer
      character(*), intent(in) :: var, fname, fmt

      real(SP), pointer :: fld(:, :)
      real(SP), allocatable :: glob(:, :)

      fld => this%registry%get(var)
      if (this%env%comm%is_io_node()) then
         allocate (glob(gatherer%M, gatherer%N))
      else
         allocate (glob(1, 1))
      end if
      associate (ng => N_GHOST, nx => this%grid%local_nx, ny => this%grid%local_ny)
         call gatherer%gather_field(fld(ng + 1:ng + nx, ng + 1:ng + ny), &
                                    glob, this%env%comm)
      end associate
      if (this%env%comm%is_io_node()) call write_field_file(fname, glob, fmt)
   end subroutine write_static_field

   subroutine model_finalize(this)
      use mpi_f08, only: MPI_Finalize, MPI_COMM_WORLD
      use core_comm_timers_mod, only: comm_timers_report
      class(type_model_main), intent(inout) :: this
      integer :: ierr
      call comm_timers_report(MPI_COMM_WORLD)
      call this%env%finalize()
      call MPI_Finalize(ierr)
   end subroutine model_finalize

end module model_main_mod
