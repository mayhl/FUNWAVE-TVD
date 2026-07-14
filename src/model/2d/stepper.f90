!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  2D stepper coordinator: implements the abstract type_stepper_model
!  hooks (pre_step / estimate_dt / stage / post_step) over the
!  explicit-arg kernels, porting the legacy RK3 loop body
!  (old/legacy_runner.F + old/etauv_solver.F).
!
!  Per-stage order (legacy):
!    dispersion -> fluxes -> wavemaker source -> sources -> RK update
!    -> H -> tridiagonal U/V solves -> mask/HU/HV/Froude ->
!    update_mask(9) -> breaking -> ghost exchange
!    [-> wavemaker BC (ABS/LEFT_BC_IRR): later 6d rung] -> sponge damping.
!
!  Not yet ported (deferred, with their features):
!    - radiation-stress diagnostics U_davg/V_davg (means.f90 covers
!      the compared P_center/Q_center sums only)
!    - Wsurf surface vertical velocity (foam / 3D coupling)
!    - sediment, foam, meteo, vessel, tracker hooks
!
!  Memory: all workspaces and per-step arrays are allocated once in
!  init() and reused every stage/step; migration onto the typed
!  scratch pool (scratch.f90) is a focused pass before GPU Step 8.
!
!  HISTORY :
!    07/09/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_stepper_2d_mod

   use core_constants_mod, only: SP, N_GHOST, MPI_SP
   use core_grid_mod, only: type_grid_2d
   use core_env_mod, only: type_env
   use core_stepper_engine_mod, only: type_stepper_model
   use core_solver_tridiag_mod, only: trid_x, trid_y, trid_y_periodic, &
                                      type_trid_workspace

   use model_fields_2d_mod, only: type_fields_2d
   use model_bc_mod, only: type_model_bc
   use model_physics_mod, only: type_model_physics
   use model_numerics_mod, only: type_model_numerics
   use model_breaking_mod, only: type_model_breaking
   use model_friction_mod, only: type_model_friction
   use model_simulation_mod, only: type_model_simulation
   use model_output_mod, only: type_model_output
   use model_wavemaker_mod, only: type_model_wavemaker
   use model_sponge_mod, only: type_model_sponge
   use model_obstacle_mod, only: type_model_obstacle
   use model_means_mod, only: type_model_means
   use model_tide_mod, only: type_model_tide
   use model_precipitation_mod, only: type_model_precipitation
   use model_subgrid_mod, only: type_model_subgrid
   use model_foam_mod, only: type_model_foam
   use model_tracer_mod, only: type_model_tracer
   use model_vessel_mod, only: type_model_vessel
   use model_sediment_mod, only: type_model_sediment

   use model_kernel_dispersion_mod, only: type_disp_workspace, &
                                          cal_dispersion_derivs, &
                                          cal_dispersion_assemble
   use model_kernel_fluxes_mod, only: type_flux_workspace, fluxes, &
                                      flux_wall_bc, flux_dry_bc
   use model_kernel_sources_mod, only: cal_sources
   use model_kernel_etauv_mod, only: type_etauv_workspace, cal_rk_update, &
                                     cal_etauv_assemble_x, cal_etauv_assemble_y, &
                                     cal_uv_no_dispersion, cal_etauv_update, &
                                     RK_ALPHA, RK_BETA
   use model_kernel_masks_mod, only: update_mask, update_mask9
   use model_kernel_breaker_mod, only: wave_breaking, viscosity_wmaker, &
                                       VIS_SCHEME_DEFAULT

   implicit none

   private
   public :: type_model_stepper_2d

   ! Legacy breaking-age threshold (old/init.F:1235: T_brk = 20 when
   ! SHOW_BREAKING).  The spectral-wavemaker T_brk assignments are DEAD
   ! in legacy: WAVEMAKER_INITIALIZATION (init.F:955) runs first and
   ! init.F:1235 overwrites unconditionally under the same
   ! SHOW_BREAKING gate, so 20 always wins (parity ledger 17d/e).
   real(SP), parameter :: T_BRK_LEGACY = 20.0_SP

   type, extends(type_stepper_model) :: type_model_stepper_2d

      ! Borrowed references — targets owned by type_model_main.
      type(type_env), pointer :: env => null()
      type(type_grid_2d), pointer :: grid => null()
      type(type_fields_2d), pointer :: fields => null()
      type(type_model_physics), pointer :: physics => null()
      type(type_model_numerics), pointer :: numerics => null()
      type(type_model_breaking), pointer :: breaking => null()
      type(type_model_friction), pointer :: friction => null()
      type(type_model_simulation), pointer :: simulation => null()
      type(type_model_output), pointer :: output => null()
      type(type_model_wavemaker), pointer :: wavemaker => null()
      type(type_model_sponge), pointer :: sponge => null()
      type(type_model_obstacle), pointer :: obstacle => null()
      type(type_model_means), pointer :: means => null()
      type(type_model_tide), pointer :: tide => null()
      type(type_model_precipitation), pointer :: precipitation => null()
      type(type_model_subgrid), pointer :: subgrid => null()
      type(type_model_foam), pointer :: foam => null()
      type(type_model_tracer), pointer :: tracer => null()
      type(type_model_vessel), pointer :: vessel => null()
      type(type_model_sediment), pointer :: sediment => null()

      type(type_model_bc) :: bc

      ! Dispersion coefficient set derived from Beta_ref (legacy
      ! init.F): $b_1 = \beta_{ref}^2$, $b_2 = \beta_{ref}$,
      ! $\beta_1 = \beta_{ref} + 1$, $\beta_2 = (1/5)^2$.
      real(SP) :: b1 = 0.0_SP, b2 = 0.0_SP
      real(SP) :: beta1 = 0.0_SP, beta2 = 0.0_SP

      ! Breaking-age threshold (legacy T_brk, always 20 — see above)
      real(SP) :: t_brk = T_BRK_LEGACY

      ! dt of the step in flight (estimate_dt -> post_step means/stats)
      real(SP) :: dt_step = 0.0_SP

      ! LEFT_BC_IRR west exemptions (ledger 11): skip the west
      ! cross-derivative zeroing and fold the known ghost U into the
      ! x-sweep RHS — both only on the west-boundary rank
      logical :: west_dirichlet = .false.

      ! Kernel workspaces — allocated once, reused every stage.
      type(type_flux_workspace) :: fws
      type(type_disp_workspace) :: dws
      type(type_etauv_workspace) :: ews
      type(type_trid_workspace) :: tws

      ! Ghost-inclusive spacing: kernels index spacing at cell indices,
      ! while grid%dx/dy are interior-only — ghosts replicate the edge.
      real(SP), allocatable :: dx(:, :), dy(:, :)
      real(SP), allocatable :: inv_dx(:, :), inv_dy(:, :)

      ! Working face-staggered depth (legacy DepthX/DepthY, Mloc1/Nloc1
      ! shapes): kernel_fluxes needs the high-edge face that the
      ! (mloc,nloc) fields%depth_x/y drop, and update_mask truncation
      ! mutates these per stage.  fields%depth_x/y keep the initial
      ! values (registry/output only).
      real(SP), allocatable :: depth_fx(:, :), depth_fy(:, :)

      ! Per-step state (legacy MODULE GLOBAL equivalents).
      real(SP), allocatable :: u0(:, :), v0(:, :)      ! U0/V0 at step start
      real(SP), allocatable :: etat(:, :), ut(:, :), vt(:, :)
      real(SP), allocatable :: etax(:, :), etay(:, :)
      real(SP), allocatable :: u4(:, :), v4(:, :)
      real(SP), allocatable :: u1p(:, :), v1p(:, :)
      real(SP), allocatable :: u1pp(:, :), v1pp(:, :)
      real(SP), allocatable :: u2(:, :), v2(:, :), u3(:, :), v3(:, :)
      real(SP), allocatable :: src_x(:, :), src_y(:, :)
      real(SP), allocatable :: zeros(:, :)             ! inactive-source stand-in
      real(SP), allocatable :: coriolis(:, :)          ! per-cell f (f-plane; CRS later)

      ! Breaking extras (allocated whenever the breaker runs — the
      ! show-only display mode included — or their OUT_ flags ask for
      ! the legacy zero-filled files).
      real(SP), allocatable :: roller_flux(:, :)
      real(SP), allocatable :: undertow_u(:, :), undertow_v(:, :)
      logical, allocatable :: in_wm_zone(:, :)         ! breaker's wavemaker-zone flags

      ! Breaker dispatch (legacy WAVE_BREAKING head): viscosity mode or
      ! the show-only display mode; WAVEMAKER_VIS keeps priority over
      ! show in modern (deliberate 19c deviation from the ykchoi trap)
      logical :: run_breaker = .false.

      ! Combined eddy viscosity, allocated only when more than one of
      ! nu_break / vessel deep-draft / nu_sponge is active (legacy nu_vis
      ! assembly in sources.F); single-source cases alias the source
      ! array in merge_nu_vis.
      real(SP), allocatable :: nu_vis(:, :)

      ! Output mirrors (registry is real(SP)-only): legacy Int2Flo
      ! casts of mask/mask9, refreshed at post_step; allocated only
      ! under their OUT_ flags.
      real(SP), allocatable :: mask_out(:, :), mask9_out(:, :)

   contains
      procedure :: init => stepper_init
      procedure :: register_output => stepper_register_output
      procedure :: free => stepper_free
      procedure :: pre_step => stepper_pre_step
      procedure :: estimate_dt => stepper_estimate_dt
      procedure :: stage => stepper_stage
      procedure :: post_step => stepper_post_step
   end type type_model_stepper_2d

contains

   ! ----------------------------------------------------------------
   ! Bind component references, allocate workspaces, and complete the
   ! initial state (HU/HV; see the initial-flux note at the end —
   ! the legacy dispersion correction of the initial Ubar is inert).
   ! ----------------------------------------------------------------
   subroutine stepper_init(this, env, grid, fields, physics, numerics, &
                           breaking, friction, simulation, output, &
                           wavemaker, sponge, obstacle, means, tide, &
                           precipitation, subgrid, foam, tracer, vessel, &
                           sediment)
      class(type_model_stepper_2d), intent(inout) :: this
      ! all component dummies are intent(inout) targets: they are
      ! captured as pointers on the stepper (intent(in) may not be a
      ! pointer-assignment target)
      type(type_env), intent(inout), target :: env
      type(type_grid_2d), intent(inout), target :: grid
      type(type_fields_2d), intent(inout), target :: fields
      type(type_model_physics), intent(inout), target :: physics
      type(type_model_numerics), intent(inout), target :: numerics
      type(type_model_breaking), intent(inout), target :: breaking
      type(type_model_friction), intent(inout), target :: friction
      type(type_model_simulation), intent(inout), target :: simulation
      type(type_model_output), intent(inout), target :: output
      ! wavemaker%init_compute must have run (source coefficients and
      ! zone box feed the mass source and the breaker zone flags);
      ! likewise sponge%init_compute (direct-sponge coeff)
      type(type_model_wavemaker), intent(inout), target :: wavemaker
      type(type_model_sponge), intent(inout), target :: sponge
      ! obstacle%init_compute must have run (breakwater drag map)
      type(type_model_obstacle), intent(inout), target :: obstacle
      ! means%init_compute must have run (the breaker reads etamean)
      type(type_model_means), intent(inout), target :: means
      ! tide%init_compute must have run (relaxation profiles, DATA
      ! series, and the REMOVE_SPONGE disable)
      type(type_model_tide), intent(inout), target :: tide
      ! precipitation%init_compute must have run (index file open,
      ! first frame loaded)
      type(type_model_precipitation), intent(inout), target :: precipitation
      ! subgrid%init_compute must have run (panels loaded, still-water
      ! porosity seeded — stage 1 divides by it before the first update)
      type(type_model_subgrid), intent(inout), target :: subgrid
      ! foam%init_compute must have run (state arrays allocated); foam is
      ! one-way, so nothing here depends on its values
      type(type_model_foam), intent(inout), target :: foam
      ! tracer%init_compute must have run (trackers located); like foam it
      ! is one-way — it only reads u/v/mask
      type(type_model_tracer), intent(inout), target :: tracer
      ! vessel%init_compute must have run (tracks open, first point read).
      ! Unlike foam/tracer this one is TWO-WAY: the pressure gradient and the
      ! slender-body flux feed the momentum and continuity RHS.
      type(type_model_vessel), intent(inout), target :: vessel
      ! sediment%init_compute must have run (transport state allocated, grain
      ! parameters folded).  One-way for now — it reads the flow and rebuilds
      ! H, but nothing feeds back into the momentum or continuity RHS until
      ! the morphology and source-term rungs land.
      type(type_model_sediment), intent(inout), target :: sediment

      integer :: i, j, ii, jj, mloc, nloc

      this%env => env
      this%grid => grid
      this%fields => fields
      this%physics => physics
      this%numerics => numerics
      this%breaking => breaking
      this%friction => friction
      this%simulation => simulation
      this%output => output
      this%wavemaker => wavemaker
      this%sponge => sponge
      this%obstacle => obstacle
      this%means => means
      this%tide => tide
      this%precipitation => precipitation
      this%subgrid => subgrid
      this%foam => foam
      this%tracer => tracer
      this%vessel => vessel
      this%sediment => sediment

      call this%bc%init(grid, wavemaker%wavemaker_type)

      ! legacy EXCHANGE ghost gates (old/bc.F:441-449): AGE_BREAKING
      ! travels only under VISCOSITY_BREAKING, nu_break also under
      ! WAVEMAKER_VIS — the show-only display mode exchanges NEITHER
      ! (its age ghosts stay locally-written/zero, seams included)
      this%bc%exch_age = physics%viscosity_breaking
      this%bc%exch_nu = physics%viscosity_breaking .or. breaking%WAVEMAKER_VIS

      ! wavemaker%T_brk is deliberately NOT consumed (dead in legacy;
      ! see the T_BRK_LEGACY note)
      this%t_brk = T_BRK_LEGACY

      this%west_dirichlet = grid%is_back_boundary &
                            .and. wavemaker%wavemaker_type == "LEFT_BC_IRR"

      this%b1 = physics%Beta_ref*physics%Beta_ref
      this%b2 = physics%Beta_ref
      this%beta1 = physics%Beta_ref + 1.0_SP
      this%beta2 = (1.0_SP/5.0_SP)**2

      mloc = grid%lp%mloc
      nloc = grid%lp%nloc

      ! ghost-inclusive spacing, edge-replicated into the ghosts
      allocate (this%dx(mloc, nloc), this%dy(mloc, nloc))
      allocate (this%inv_dx(mloc, nloc), this%inv_dy(mloc, nloc))
      do j = 1, nloc
         jj = min(max(j - N_GHOST, 1), grid%local_ny)
         do i = 1, mloc
            ii = min(max(i - N_GHOST, 1), grid%local_nx)
            this%dx(i, j) = grid%dx(ii, jj)
            this%dy(i, j) = grid%dy(ii, jj)
            this%inv_dx(i, j) = grid%inv_dx(ii, jj)
            this%inv_dy(i, j) = grid%inv_dy(ii, jj)
         end do
      end do

      ! face-staggered depth: interior faces from geometry, high edge
      ! extrapolated (legacy init.F):
      !   $$ d_{M+1/2} = \tfrac{1}{2}(3 d_M - d_{M-1}) $$
      allocate (this%depth_fx(mloc + 1, nloc), this%depth_fy(mloc, nloc + 1))
      associate (f => this%fields)
         this%depth_fx(1:mloc, :) = f%depth_x
         this%depth_fx(mloc + 1, :) = 0.5_SP*(3.0_SP*f%depth(mloc, :) &
                                              - f%depth(mloc - 1, :))
         this%depth_fy(:, 1:nloc) = f%depth_y
         this%depth_fy(:, nloc + 1) = 0.5_SP*(3.0_SP*f%depth(:, nloc) &
                                              - f%depth(:, nloc - 1))

         ! legacy init.F dry-cell face flattening: every initially-dry
         ! cell gets locally flat face depths (the in-loop update_mask
         ! only flattens on wet/dry TRANSITIONS, so the initial state
         ! must be pre-flattened or stage 1 sees a spurious depth
         ! gradient at the mask edge — parity ledger 8c)
         do j = 2, nloc - 1
            do i = 2, mloc - 1
               if (f%mask(i, j) < 1) then
                  this%depth_fx(i, j) = f%depth(i - 1, j)
                  this%depth_fx(i + 1, j) = f%depth(i + 1, j)
                  this%depth_fy(i, j) = f%depth(i, j - 1)
                  this%depth_fy(i, j + 1) = f%depth(i, j + 1)
               end if
            end do
         end do
      end associate

      call this%fws%alloc(mloc, nloc)
      ! interface fluxes persist across stages and are read by
      ! cal_dispersion before the first fluxes call (legacy P/Q = 0)
      this%fws%p = 0.0_SP; this%fws%q = 0.0_SP
      this%fws%fx = 0.0_SP; this%fws%fy = 0.0_SP
      this%fws%gx = 0.0_SP; this%fws%gy = 0.0_SP

      call this%ews%alloc(mloc, nloc)
      if (this%physics%dispersion) then
         call this%dws%alloc(mloc, nloc)
         if (this%physics%periodic) call this%tws%alloc(mloc, nloc)
      end if

      allocate (this%u0(mloc, nloc), source=0.0_SP)
      allocate (this%v0(mloc, nloc), source=0.0_SP)
      allocate (this%etat(mloc, nloc), source=0.0_SP)
      allocate (this%ut(mloc, nloc), source=0.0_SP)
      allocate (this%vt(mloc, nloc), source=0.0_SP)
      allocate (this%etax(mloc, nloc), source=0.0_SP)
      allocate (this%etay(mloc, nloc), source=0.0_SP)
      allocate (this%u4(mloc, nloc), source=0.0_SP)
      allocate (this%v4(mloc, nloc), source=0.0_SP)
      allocate (this%u1p(mloc, nloc), source=0.0_SP)
      allocate (this%v1p(mloc, nloc), source=0.0_SP)
      allocate (this%u1pp(mloc, nloc), source=0.0_SP)
      allocate (this%v1pp(mloc, nloc), source=0.0_SP)
      allocate (this%u2(mloc, nloc), source=0.0_SP)
      allocate (this%v2(mloc, nloc), source=0.0_SP)
      allocate (this%u3(mloc, nloc), source=0.0_SP)
      allocate (this%v3(mloc, nloc), source=0.0_SP)
      allocate (this%src_x(mloc, nloc), source=0.0_SP)
      allocate (this%src_y(mloc, nloc), source=0.0_SP)
      allocate (this%zeros(mloc, nloc), source=0.0_SP)

      ! per-cell f slot — the future CRS metric provider takes ownership
      ! of filling this ([[design-grid-crs]]); constant f-plane today
      if (this%physics%coriolis_on) then
         allocate (this%coriolis(mloc, nloc), source=this%physics%coriolis_f)
      end if

      ! legacy io.F refuses the combination (VISCOSITY_WMAKER replaces
      ! the breaking-age scheme, it does not stack on it)
      if (this%physics%viscosity_breaking .and. this%breaking%WAVEMAKER_VIS) then
         error stop "stepper: viscosity_breaking and WAVEMAKER_VIS are mutually exclusive"
      end if

      ! legacy WAVE_BREAKING dispatch: SHOW_BREAKING runs BREAKING
      ! (show_breaking is forced by viscosity_breaking in model_setup);
      ! in modern WAVEMAKER_VIS wins over show (19c deviation)
      this%run_breaker = this%physics%viscosity_breaking &
                         .or. (this%breaking%show_breaking &
                               .and. .not. this%breaking%WAVEMAKER_VIS)

      ! legacy allocates + zeroes ROLLER_FLUX/UNDERTOW unconditionally,
      ! so OUT_ROLLER/OUT_UNDERTOW without a running breaker still
      ! write zero-filled files
      if (this%run_breaker .or. this%output%OUT_ROLLER &
          .or. this%output%OUT_UNDERTOW) then
         allocate (this%roller_flux(mloc, nloc), source=0.0_SP)
         allocate (this%undertow_u(mloc, nloc), source=0.0_SP)
         allocate (this%undertow_v(mloc, nloc), source=0.0_SP)
      end if
      if (this%run_breaker .or. this%breaking%WAVEMAKER_VIS) then
         allocate (this%in_wm_zone(mloc, nloc))
         call wavemaker%fill_in_zone(this%in_wm_zone)
      end if
      ! Legacy assembles nu_vis as
      !     nu_vis = nu_break [+ VisVessel_2D]   under VISCOSITY_BREAKING/WAVEMAKER_VIS
      !     nu_vis = nu_vis + nu_sponge          under DIFFUSION_SPONGE
      ! so the vessel term rides INSIDE the breaking branch and is silently
      ! dropped when breaking viscosity is off (ledger 6g-5).  Allocate the
      ! combined array whenever more than one contributor is live; the
      ! single-source cases still alias in merge_nu_vis.
      if (((this%physics%viscosity_breaking .or. this%breaking%WAVEMAKER_VIS) &
           .and. this%sponge%diffusion_sponge) .or. ves_vis_on(this)) then
         allocate (this%nu_vis(mloc, nloc), source=0.0_SP)
      end if

      ! -- initial cell fluxes ---------------------------------------
      ! Legacy init.F writes Ubar = HU + Gamma1*U1p*H, but that
      ! correction is DEAD CODE: the init-time CAL_DISPERSION runs
      ! before MASK9 is first assigned (init.F:1168 vs :1036), so the
      ! MASK9-weighted derivatives — hence U1p — are identically zero
      ! and the legacy initial state is exactly Ubar = HU.  Parity
      ! therefore requires p = Hu (set in model_setup) with NO
      ! dispersion correction here.
      associate (f => this%fields)
         f%hu = f%h*f%u
         f%hv = f%h*f%v
      end associate

   end subroutine stepper_init

   ! ----------------------------------------------------------------
   ! Step head (legacy loop): save the step-start state, refresh
   ! ghosts, then snapshot U0/V0 *after* the exchange — their ghost
   ! values feed the cal_dispersion time-derivative stencils.
   ! ----------------------------------------------------------------
   subroutine stepper_pre_step(this)
      class(type_model_stepper_2d), intent(inout) :: this

      associate (f => this%fields)
         f%eta0 = f%eta
         f%p0 = f%p
         f%q0 = f%q

         ! the suspended load rides the same RK weights, so its step-start
         ! copy is taken here too (legacy CHH0 = CHH, alongside Eta0)
         call this%sediment%save_step0()

         call this%bc%exchange_state(this%grid, f)

         this%u0 = f%u
         this%v0 = f%v
      end associate

   end subroutine stepper_pre_step

   subroutine stepper_estimate_dt(this, dt)
      class(type_model_stepper_2d), intent(inout) :: this
      real(SP), intent(out) :: dt

      associate (f => this%fields)
         call this%numerics%estimate_dt(this%grid, f%u, f%v, f%h, &
                                        this%simulation%fixed_dt, &
                                        this%simulation%dt_fixed, dt)
      end associate
      this%dt_step = dt

   end subroutine stepper_estimate_dt

   ! ----------------------------------------------------------------
   ! One RK3 stage, legacy order.  time = t + dt (legacy TIME inside
   ! the stage loop — constant across the three stages).
   ! ----------------------------------------------------------------
   subroutine stepper_stage(this, istage, dt, time)
      class(type_model_stepper_2d), intent(inout) :: this
      integer, intent(in) :: istage
      real(SP), intent(in) :: dt, time

      associate (f => this%fields, lp => this%grid%lp, &
                 phy => this%physics, num => this%numerics)

         ! per-step tidal DATA refresh (legacy TIDE_DATA before the RK
         ! loop, at the already-advanced TIME); gated on read-time
         ! enablement, NOT tidal_bc_abs — the REMOVE_SPONGE disable
         ! stops TIDE_BC but legacy keeps streaming the files
         if (istage == 1 .and. this%tide%is_activated &
             .and. this%tide%data_mode()) then
            call this%tide%update_data(time, dt)
         end if

         ! per-step rainfall refresh (legacy PRECIPITATION_DISTRIBUTION
         ! before the RK loop, same already-advanced TIME)
         if (istage == 1 .and. this%precipitation%is_activated) then
            call this%precipitation%update(time)
         end if

         ! vessel tracks + pressure/flux fields (legacy VESSEL_FORCING,
         ! after ESTIMATE_DT and BEFORE the RK loop, so once per step at the
         ! already-advanced TIME).  It writes eta/eta0 on the first step only
         ! (MakeVesselDraft), so it must land ahead of the fluxes.
         if (istage == 1 .and. this%vessel%is_activated) then
            call this%vessel%update(this%bc, this%grid, time, dt, &
                                    f%eta, f%eta0, f%depth, f%h)
         end if

         if (phy%dispersion) call run_dispersion(this, dt)

         call fluxes(lp, num%high_order, num%construction, &
                     f%eta, f%u, f%v, f%hu, f%hv, this%u4, this%v4, &
                     this%depth_fx, this%depth_fy, this%dx, this%dy, &
                     this%inv_dx, this%inv_dy, f%mask, f%mask9, &
                     phy%Gamma1, phy%Gamma3, phy%dispersion, this%fws)

         call flux_wall_bc(lp, this%bc%fill_west, this%bc%fill_east, &
                           this%bc%fill_south, this%bc%fill_north, &
                           phy%Gamma3, this%depth_fx, this%depth_fy, this%fws)

         ! dry-cell faces after the wall fills (legacy BOUNDARY_CONDITION
         ! order); walls here are topological, not the bc fill flags
         call flux_dry_bc(lp, this%grid%is_back_boundary, &
                          this%grid%is_shore_boundary, &
                          this%grid%is_right_boundary, &
                          this%grid%is_left_boundary, &
                          phy%Gamma3, f%mask, this%depth_fx, this%depth_fy, &
                          this%fws)

         ! Manning drag from current H (legacy evaluates inside SourceTerms)
         call this%friction%update_cd(f%h, num%MinDepthFrc)

         ! wavemaker mass source at the stage TIME (legacy SourceTerms head)
         call this%wavemaker%update_source(time)

         ! combined eddy viscosity, assembled in legacy's order (sources.F
         ! head): nu_break, then the deep-draft hull, then the sponge
         if (allocated(this%nu_vis)) then
            this%nu_vis = 0.0_SP
            if (phy%viscosity_breaking .or. this%breaking%WAVEMAKER_VIS) then
               this%nu_vis = f%nu_break
               if (ves_vis_on(this)) then
                  this%nu_vis = this%nu_vis + this%vessel%vis_2d
               end if
            end if
            if (this%sponge%diffusion_sponge) then
               this%nu_vis = this%nu_vis + this%sponge%nu_sponge
            end if
         end if

         call cal_sources(lp, phy%Gamma1, phy%Gamma2, phy%dispersion, &
                          phy%coriolis_on, this%obstacle%breakwater, &
                          ves_drag_on(this), &
                          f%mask, f%mask9, this%inv_dx, this%inv_dy, &
                          f%depth, this%depth_fx, this%depth_fy, &
                          f%eta, f%h, f%u, f%v, &
                          this%fws%p, this%fws%q, f%hu, f%hv, &
                          this%u4, this%v4, this%u1p, this%v1p, &
                          this%u1pp, this%v1pp, this%u2, this%v2, &
                          this%u3, this%v3, &
                          wm_mass(this), &
                          this%friction%Cd, &
                          merge_nu_vis(this), &
                          cor_f(this), bw_cd(this), wm_cd(this), &
                          ves_cd(this), ves_px(this), ves_py(this), &
                          num%MinDepthFrc, this%src_x, this%src_y)

         call cal_rk_update(lp, RK_ALPHA(istage), RK_BETA(istage), dt, &
                            this%inv_dx, this%inv_dy, &
                            this%fws%p, this%fws%q, this%fws%fx, this%fws%fy, &
                            this%fws%gx, this%fws%gy, &
                            this%src_x, this%src_y, &
                            wm_mass(this), prec_rate(this), ves_flux(this), &
                            this%subgrid%is_activated, porosity(this), &
                            f%eta0, f%p0, f%q0, f%eta, f%p, f%q)

         ! legacy GET_Eta_U_V_HU_HV: whole-array H (unclamped; ghost eta
         ! is one exchange behind, exactly as legacy)
         f%h = phy%Gamma3*f%eta + f%depth

         ! sub-cell porosity/pixel average off the new eta, then the
         ! pixel-averaged column replaces H at subgrid cells; the
         ! porosity the NEXT stage divides by is the one left here
         if (this%subgrid%is_activated) then
            call this%subgrid%update(f%eta)
            call this%subgrid%apply_h(f%h)
         end if

         if (phy%dispersion) then
            call cal_etauv_assemble_x(lp, phy%Gamma1, num%MinDepthFrc, &
                                      this%b1, this%b2, this%inv_dx, &
                                      f%mask, f%mask9, f%depth, f%h, f%p, &
                                      this%dws%vxy, this%dws%dvxy, &
                                      this%west_dirichlet, f%u, this%ews)
            call trid_x(lp, this%grid, this%ews%a, this%ews%c, this%ews%d, &
                        this%ews%f)
            f%u(lp%ib:lp%ie, lp%jb:lp%je) = this%ews%f(lp%ib:lp%ie, lp%jb:lp%je)

            call cal_etauv_assemble_y(lp, phy%disp_time_left, phy%Gamma1, &
                                      phy%Gamma2, num%MinDepthFrc, &
                                      this%b1, this%b2, this%inv_dy, &
                                      f%mask, f%mask9, f%depth, f%h, f%eta, &
                                      f%q, this%dws%uxy, this%dws%duxy, &
                                      this%dws%ux, this%dws%dux, this%ews)
            if (phy%periodic) then
               call trid_y_periodic(lp, this%grid, this%ews%a, this%ews%c, &
                                    this%ews%d, this%tws, this%ews%f)
            else
               call trid_y(lp, this%grid, this%ews%a, this%ews%c, this%ews%d, &
                           this%ews%f)
            end if
            f%v(lp%ib:lp%ie, lp%jb:lp%je) = this%ews%f(lp%ib:lp%ie, lp%jb:lp%je)
         else
            call cal_uv_no_dispersion(lp, num%MinDepthFrc, f%h, f%p, f%q, &
                                      f%u, f%v)
         end if

         call cal_etauv_update(lp, num%FroudeCap, num%MinDepthFrc, f%mask, &
                               f%h, f%u, f%v, f%hu, f%hv, f%p, f%q)
         if (.not. phy%dispersion) then
            ! legacy: without dispersion the conserved flux IS the
            ! (Froude-capped, mask-zeroed) cell flux
            f%p = f%hu
            f%q = f%hv
         end if

         call update_mask(lp, f%eta, f%depth, f%mask_struc, f%mask, &
                          this%depth_fx, this%depth_fy, truncate_depth=.true.)
         call update_mask9(lp, f%eta, f%depth, f%mask, f%mask9, &
                           num%MinDepthFrc, phy%SWE_ETA_DEP, &
                           phy%viscosity_breaking)

         ! deep-draft hull blanks mask9 (legacy UPDATE_MASK:
         ! MASK9 = MASK9tmp*MaskVessel, before the ghost exchange)
         if (ves_mask_on(this)) then
            f%mask9 = f%mask9*this%vessel%mask_vessel
         end if

         ! tidal strip relaxation (legacy TIDE_BC between UPDATE_MASK
         ! and WAVE_BREAKING); hu/hv stay stale like legacy
         if (this%tide%tidal_bc_abs) then
            call this%tide%apply_bc(f%mask, f%eta, f%u, f%v)
         end if

         ! suspended load (legacy SEDIMENT_ADVECTION_DIFFUSION, between
         ! TIDE_BC and WAVE_BREAKING).  It REBUILDS f%h off the current eta,
         ! so the breaker and foam below see the sediment module's H, not the
         ! one the RK update left (sediment.f90 NOTE 3).  The undertow it
         ! advects on is the PREVIOUS stage's, since the breaker runs after.
         if (this%sediment%is_activated) then
            call this%sediment%update(this%bc, this%grid, RK_ALPHA(istage), &
                                      RK_BETA(istage), dt, phy%Gamma3, &
                                      num%MinDepth, this%inv_dx, this%inv_dy, &
                                      f%mask, f%eta, f%depth, f%u, f%v, &
                                      this%fws%p, this%fws%q, f%h, &
                                      this%breaking%roller, &
                                      this%undertow_u, this%undertow_v)
         end if

         if (this%run_breaker) then
            ! viscosity mode AND the legacy show-only display mode: the
            ! breaker always fills nu_break/age/roller here, but only
            ! viscosity_breaking feeds nu_break into the momentum
            ! sources (merge_nu_vis) — show-only leaves dynamics alone
            ! TODO(6e+): vis_scheme selection beyond DEFAULT
            call wave_breaking(lp, this%etax, this%etay, this%etat, &
                               f%eta, f%depth, f%h, f%u, f%v, this%means%etamean, &
                               this%dx, this%dy, dt, this%t_brk, &
                               num%MinDepthFrc, this%breaking%Cbrk1, &
                               this%breaking%Cbrk2, this%breaking%WAVEMAKER_Cbrk, &
                               this%breaking%nu_bkg, VIS_SCHEME_DEFAULT, &
                               phy%SWE_ETA_DEP, this%in_wm_zone, &
                               f%nu_break, f%age_break, this%roller_flux, &
                               this%undertow_u, this%undertow_v)
         elseif (this%breaking%WAVEMAKER_VIS) then
            ! legacy WAVE_BREAKING second branch: zone-only viscosity,
            ! no age tracking
            call viscosity_wmaker(lp, this%etat, f%eta, f%depth, f%h, &
                                  this%breaking%visbrk, &
                                  this%breaking%WAVEMAKER_visbrk, &
                                  this%breaking%nu_bkg, num%MinDepthFrc, &
                                  this%in_wm_zone, f%nu_break)
         end if

         ! foam rides the breaker output (legacy FOAM_* between
         ! WAVE_BREAKING and EXCHANGE, so it reads the pre-exchange
         ! nu_break/age ghosts).  One-way: nothing below reads it back,
         ! and it steps the FULL dt every stage (foam.f90 NOTE 1)
         if (this%foam%is_activated) then
            call this%foam%update(this%grid, dt, this%dx, this%dy, &
                                  this%inv_dx, this%inv_dy, &
                                  f%u, f%v, f%nu_break, f%age_break)
         end if

         call this%bc%exchange_state(this%grid, f)

         call this%wavemaker%apply_boundary(this%grid, istage, dt, time, &
                                            f%eta, f%u, f%v, f%hu, f%hv, &
                                            f%depth)

         call this%sponge%apply(f, this%grid)

      end associate

   end subroutine stepper_stage

   ! ----------------------------------------------------------------
   ! Register stepper-owned output arrays (after init): the legacy
   ! interface fluxes P/Q live in the flux workspace (loop-top state =
   ! last-stage fluxes, exactly what legacy PREVIEW writes), and the
   ! integer masks ride real mirrors.  fws%p/q are zeroed here so the
   ! initial-condition frame matches legacy's zero-initialised P/Q.
   ! ----------------------------------------------------------------
   subroutine stepper_register_output(this, registry)
      use core_field_registry_mod, only: type_field_registry
      class(type_model_stepper_2d), intent(inout), target :: this
      type(type_field_registry), intent(inout) :: registry

      this%fws%p = 0.0_SP
      this%fws%q = 0.0_SP
      call registry%register("p_flux", this%fws%p)
      call registry%register("q_flux", this%fws%q)

      if (allocated(this%roller_flux)) then
         call registry%register("roller_flux", this%roller_flux)
         call registry%register("undertow_u", this%undertow_u)
         call registry%register("undertow_v", this%undertow_v)
      end if

      if (this%foam%is_activated) then
         call registry%register("eta_foam", this%foam%eta_foam)
         call registry%register("eta_foam_max", this%foam%eta_foam_max)
      end if

      if (this%vessel%is_activated) then
         ! legacy PREVIEW writes Pves_ under OUT_VESSEL
         call registry%register("vessel_pressure", this%vessel%p_total)
         ! legacy VesUp_/VesVp_.  These matter more than they look: the
         ! propeller is ONE-WAY, so a dead jet is bitwise identical to a live
         ! one on eta/u/v and no hydrodynamic check can tell them apart.  The
         ! jet field is the only liveness evidence there is.
         if (this%vessel%propeller) then
            call registry%register("vessel_up", this%vessel%up_total)
            call registry%register("vessel_vp", this%vessel%vp_total)
         end if
      end if

      ! legacy OUTPUT_SEDIMENT writes C_/Pick_/Depo_ with no OUT_ gate.  Like
      ! the propeller jet these are the only observable the transport has until
      ! bed change lands, so without them a dead solver scores as parity.
      if (this%sediment%is_activated) then
         call registry%register("sediment_c", this%sediment%ch)
         call registry%register("sediment_pickup", this%sediment%pickup)
         call registry%register("sediment_depo", this%sediment%depo)
         call registry%register("sediment_bedfx", this%sediment%bed_flux_x)
         call registry%register("sediment_bedfy", this%sediment%bed_flux_y)
      end if

      associate (f => this%fields, lp => this%grid%lp)
         if (this%output%OUT_MASK) then
            allocate (this%mask_out(lp%mloc, lp%nloc))
            this%mask_out = real(f%mask, SP)
            call registry%register("mask", this%mask_out)
         end if
         if (this%output%OUT_MASK9) then
            allocate (this%mask9_out(lp%mloc, lp%nloc))
            this%mask9_out = real(f%mask9, SP)
            call registry%register("mask9", this%mask9_out)
         end if
      end associate
   end subroutine stepper_register_output

   ! ----------------------------------------------------------------
   ! Step tail (legacy): mixing (deferred), max/min envelopes, and the
   ! global blow-up check
   !   $$ \max_{i,j} |\eta| > \eta_{blow} \Rightarrow \text{abort} $$
   ! ----------------------------------------------------------------
   subroutine stepper_post_step(this, time, blowup)
      use mpi_f08, only: MPI_Allreduce, MPI_MAX, MPI_IN_PLACE
      class(type_model_stepper_2d), intent(inout) :: this
      real(SP), intent(in) :: time
      logical, intent(out) :: blowup

      real(SP) :: max_abs_eta
      integer :: ierr

      ! legacy MORPHOLOGICAL_CHANGE: outside the RK loop and ahead of
      ! MIXING_STUFF, so it evolves the bed on the last stage's bedload flux
      ! and the completed step's dt.  It REWRITES fields%depth (and restaggers
      ! depth_x/depth_y), which every kernel of the next step then reads
      if (this%sediment%is_activated) then
         call this%sediment%morphology(this%bc, this%grid, this%dt_step, &
                                       this%inv_dx, this%inv_dy, &
                                       this%fields%depth, this%fields%depth_x, &
                                       this%fields%depth_y)
      end if

      ! Legacy MIXING_STUFF: means accumulate on the completed step
      ! (last-stage interface fluxes feed the P_center/Q_center sums)
      call this%means%update(this%fields, this%fws%p, this%fws%q, &
                             this%numerics%MinDepthFrc, this%dt_step, time)

      ! legacy TRACK_XY: outside the RK loop, between MIXING_STUFF and
      ! MAX_MIN_PROPERTY, so the trackers advect on the completed step's
      ! u/v with the full dt (unlike foam, which sits inside the stages)
      if (this%tracer%is_activated) then
         call this%tracer%update(this%grid, time, this%dt_step, &
                                 this%fields%u, this%fields%v, this%fields%mask)
      end if

      call update_max_min(this, time)

      ! Refresh integer-mask output mirrors for the loop-top flush
      if (allocated(this%mask_out)) this%mask_out = real(this%fields%mask, SP)
      if (allocated(this%mask9_out)) this%mask9_out = real(this%fields%mask9, SP)

      associate (f => this%fields, lp => this%grid%lp)
         max_abs_eta = maxval(abs(f%eta(lp%ib:lp%ie, lp%jb:lp%je)))
      end associate
      call MPI_Allreduce(MPI_IN_PLACE, max_abs_eta, 1, MPI_SP, MPI_MAX, &
                         this%grid%cart_comm, ierr)
      blowup = max_abs_eta > this%output%EtaBlowVal

   end subroutine stepper_post_step

   ! ----------------------------------------------------------------
   ! Private: legacy MAX_MIN_PROPERTY (old/misc.F) — envelope fields
   ! over the whole (ghost-inclusive) array, wet cells only.  VORmax
   ! lives in cal_dispersion_assemble (legacy dispersion.F, Cartesian).
   ! ----------------------------------------------------------------
   subroutine update_max_min(this, time)
      class(type_model_stepper_2d), intent(inout) :: this
      real(SP), intent(in) :: time

      real(SP) :: maxv
      integer :: i, j

      associate (f => this%fields, lp => this%grid%lp, &
                 out => this%output, num => this%numerics)

         if (.not. (out%OUT_Hmax .or. out%OUT_Hmin .or. out%OUT_Umax &
                    .or. out%OUT_MFmax .or. num%OUT_Time)) return

         do j = 1, lp%nloc
            do i = 1, lp%mloc
               if (f%mask(i, j) < 1) cycle

               if (out%OUT_Hmax) then
                  if (f%eta(i, j) > f%h_max(i, j)) f%h_max(i, j) = f%eta(i, j)
               end if
               if (out%OUT_Hmin) then
                  if (f%eta(i, j) < f%h_min(i, j)) f%h_min(i, j) = f%eta(i, j)
               end if
               if (out%OUT_Umax) then
                  maxv = sqrt(f%u(i, j)**2 + f%v(i, j)**2)
                  if (maxv > f%u_max(i, j)) f%u_max(i, j) = maxv
               end if
               if (out%OUT_MFmax) then
                  maxv = (f%u(i, j)**2 + f%v(i, j)**2)*f%h(i, j)
                  if (maxv > f%mf_max(i, j)) f%mf_max(i, j) = maxv
               end if
               if (num%OUT_Time) then
                  if (f%arr_time(i, j) == 0.0_SP .and. &
                      abs(f%eta(i, j)) > num%ArrTimeMin) then
                     f%arr_time(i, j) = time
                  end if
               end if
            end do
         end do

      end associate

   end subroutine update_max_min

   ! ----------------------------------------------------------------
   ! Private: dispersion pass with the stepper's array wiring, in the
   ! legacy Cal_Dispersion order: derivatives -> component ghost
   ! exchange (legacy EXCHANGE_DISPERSION parities) -> ghost-inclusive
   ! u4/v4/u1p/v1p assembly.  Boundary flags are the cart-topology
   ! walls: back/shore = west/east, right/left = south/north; periodic
   ! wraps report no boundary.
   ! ----------------------------------------------------------------
   subroutine run_dispersion(this, dt)
      class(type_model_stepper_2d), intent(inout) :: this
      real(SP), intent(in) :: dt

      associate (f => this%fields, lp => this%grid%lp, g => this%grid, &
                 phy => this%physics, num => this%numerics)
         call cal_dispersion_derivs(lp, this%dws, f%eta, f%depth, f%u, f%v, &
                                    this%u0, this%v0, this%fws%p, this%fws%q, &
                                    f%mask9, this%inv_dx, this%inv_dy, dt, &
                                    num%MinDepthFrc, phy%Gamma2, &
                                    this%breaking%show_breaking, &
                                    g%is_back_boundary .and. .not. this%west_dirichlet, &
                                    g%is_shore_boundary, &
                                    g%is_right_boundary, g%is_left_boundary, &
                                    this%etat, this%ut, this%vt, this%etax, &
                                    this%etay)
         call this%bc%exchange_dispersion(g, phy%Gamma2, this%dws, this%ut, &
                                          this%vt, this%etax, this%etay)
         call cal_dispersion_assemble(lp, this%dws, f%eta, f%depth, f%u, f%v, &
                                      f%mask9, this%inv_dx, this%inv_dy, &
                                      this%beta1, this%beta2, phy%Gamma2, &
                                      this%etat, this%etax, this%etay, &
                                      this%u4, this%v4, this%u1p, this%v1p, &
                                      this%u1pp, this%v1pp, this%u2, this%v2, &
                                      this%u3, this%v3, &
                                      out_vormax=this%output%OUT_VORmax, &
                                      vort_max=f%vort_max)
      end associate

   end subroutine run_dispersion

   ! Wavemaker mass source for the eta/momentum RHS: the wavemaker's
   ! array when an internal source is active, zeros otherwise (the
   ! kernels add it unconditionally).
   function wm_mass(this) result(m)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: m(:, :)

      if (this%wavemaker%has_mass_source) then
         m => this%wavemaker%mass
      else
         m => this%zeros
      end if
   end function wm_mass

   ! Rainfall mass source for the eta RHS: the precipitation rate array
   ! when active, zeros otherwise (appended after wm_mass like legacy)
   function prec_rate(this) result(p)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: p(:, :)

      if (this%precipitation%is_activated) then
         p => this%precipitation%rate_model
      else
         p => this%zeros
      end if
   end function prec_rate

   ! Sub-cell porosity for the eta RHS divide: the subgrid module's
   ! array when active, zeros otherwise (unread — the kernel gates on
   ! subgrid_on — but the dummy must still be associated)
   function porosity(this) result(p)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: p(:, :)

      if (this%subgrid%is_activated) then
         p => this%subgrid%porosity
      else
         p => this%zeros
      end if
   end function porosity

   ! Per-cell Coriolis f for the momentum source: the stepper's array
   ! when active, zeros otherwise (the kernel also gates on coriolis_on)
   function cor_f(this) result(c)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: c(:, :)

      if (allocated(this%coriolis)) then
         c => this%coriolis
      else
         c => this%zeros
      end if
   end function cor_f

   ! Breakwater drag for the momentum source: the obstacle's map when
   ! computed, zeros otherwise (the kernel also gates on breakwater_on)
   function bw_cd(this) result(c)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: c(:, :)

      if (allocated(this%obstacle%cd_breakwater)) then
         c => this%obstacle%cd_breakwater
      else
         c => this%zeros
      end if
   end function bw_cd

   ! Wavemaker current-balance drag for the momentum source: the
   ! wavemaker's source-box map when the balance is on, zeros otherwise
   function wm_cd(this) result(c)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: c(:, :)

      if (allocated(this%wavemaker%cd_current)) then
         c => this%wavemaker%cd_current
      else
         c => this%zeros
      end if
   end function wm_cd

   ! Effective eddy viscosity for the momentum source (legacy nu_vis
   ! assembly at the SourceTerms head): nu_break under viscosity
   ! breaking or wavemaker viscosity, plus nu_sponge under the
   ! diffusion sponge.  Legacy zero-adds make the aliased single-source
   ! cases bitwise identical to the assembled sum.
   function merge_nu_vis(this) result(nu)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: nu(:, :)

      logical :: has_break

      has_break = this%physics%viscosity_breaking &
                  .or. this%breaking%WAVEMAKER_VIS
      if (allocated(this%nu_vis)) then
         nu => this%nu_vis
      elseif (has_break) then
         nu => this%fields%nu_break
      elseif (this%sponge%diffusion_sponge) then
         nu => this%sponge%nu_sponge
      else
         nu => this%zeros
      end if
   end function merge_nu_vis

   ! ── vessel accessors ───────────────────────────────────────────────
   ! Same zeros-stand-in contract as the other optional modules: an
   ! inactive vessel adds an exact zero, so a vessel-free run stays
   ! bitwise identical to one built with the module compiled in.

   ! Deep-draft hull drag is live only when the module, the deep-draft
   ! sub-feature, AND the friction method are all on.
   function ves_drag_on(this) result(on)
      class(type_model_stepper_2d), intent(in) :: this
      logical :: on

      on = this%vessel%is_activated .and. this%vessel%deep_draft &
           .and. this%vessel%friction_method
   end function ves_drag_on

   function ves_mask_on(this) result(on)
      class(type_model_stepper_2d), intent(in) :: this
      logical :: on

      on = this%vessel%is_activated .and. this%vessel%deep_draft &
           .and. this%vessel%mask_method
   end function ves_mask_on

   function ves_vis_on(this) result(on)
      class(type_model_stepper_2d), intent(in) :: this
      logical :: on

      on = this%vessel%is_activated .and. this%vessel%deep_draft &
           .and. this%vessel%viscosity_method
   end function ves_vis_on

   function ves_cd(this) result(c)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: c(:, :)

      if (ves_drag_on(this)) then
         c => this%vessel%cd_2d
      else
         c => this%zeros
      end if
   end function ves_cd

   ! Momentum source: -g H grad(P) from the moving pressure patch.
   function ves_px(this) result(p)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: p(:, :)

      if (this%vessel%is_activated) then
         p => this%vessel%p_x
      else
         p => this%zeros
      end if
   end function ves_px

   function ves_py(this) result(p)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: p(:, :)

      if (this%vessel%is_activated) then
         p => this%vessel%p_y
      else
         p => this%zeros
      end if
   end function ves_py

   ! Continuity source: the slender-body mass-flux dipole.
   function ves_flux(this) result(f)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: f(:, :)

      if (this%vessel%is_activated) then
         f => this%vessel%flux_grad
      else
         f => this%zeros
      end if
   end function ves_flux

   subroutine stepper_free(this)
      class(type_model_stepper_2d), intent(inout) :: this

      call this%fws%free()
      call this%ews%free()
      if (allocated(this%dws%du)) call this%dws%free()
      if (allocated(this%tws%a_loc)) call this%tws%free()

      if (allocated(this%dx)) deallocate (this%dx, this%dy, &
                                          this%inv_dx, this%inv_dy)
      if (allocated(this%depth_fx)) deallocate (this%depth_fx, this%depth_fy)
      if (allocated(this%u0)) deallocate (this%u0, this%v0, this%etat, &
                                          this%ut, this%vt, this%etax, this%etay, &
                                          this%u4, this%v4, this%u1p, this%v1p, &
                                          this%u1pp, this%v1pp, this%u2, this%v2, &
                                          this%u3, this%v3, this%src_x, this%src_y, &
                                          this%zeros)
      if (allocated(this%roller_flux)) deallocate (this%roller_flux, &
                                                   this%undertow_u, this%undertow_v)
      if (allocated(this%in_wm_zone)) deallocate (this%in_wm_zone)
      if (allocated(this%nu_vis)) deallocate (this%nu_vis)
      if (allocated(this%coriolis)) deallocate (this%coriolis)
      if (allocated(this%mask_out)) deallocate (this%mask_out)
      if (allocated(this%mask9_out)) deallocate (this%mask9_out)

      this%env => null()
      this%grid => null()
      this%fields => null()
      this%physics => null()
      this%numerics => null()
      this%breaking => null()
      this%friction => null()
      this%simulation => null()
      this%output => null()
      this%wavemaker => null()
      this%sponge => null()
      this%obstacle => null()
      this%means => null()
      this%tide => null()
      this%precipitation => null()
      this%subgrid => null()
      this%foam => null()
      this%tracer => null()
      this%vessel => null()

   end subroutine stepper_free

end module model_stepper_2d_mod
