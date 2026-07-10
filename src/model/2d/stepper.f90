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
!    dispersion -> fluxes -> sources -> RK update -> H -> tridiagonal
!    U/V solves -> mask/HU/HV/Froude -> update_mask(9) -> breaking ->
!    ghost exchange [-> wavemaker BC, sponge damping: Step 6d].
!
!  Not yet ported (deferred, with their features):
!    - MIXING_STUFF time-averaged statistics (post_step TODO)
!    - radiation-stress diagnostics P_center/Q_center/U_davg/V_davg
!    - Wsurf surface vertical velocity (foam / 3D coupling)
!    - VORmax envelope (legacy updates it inside dispersion.F)
!    - tidal BC, sediment, foam, meteo, vessel, tracker hooks
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

   use model_kernel_dispersion_mod, only: type_disp_workspace, cal_dispersion
   use model_kernel_fluxes_mod, only: type_flux_workspace, fluxes, flux_wall_bc
   use model_kernel_sources_mod, only: cal_sources
   use model_kernel_etauv_mod, only: type_etauv_workspace, cal_rk_update, &
                                     cal_etauv_assemble_x, cal_etauv_assemble_y, &
                                     cal_uv_no_dispersion, cal_etauv_update, &
                                     RK_ALPHA, RK_BETA
   use model_kernel_masks_mod, only: update_mask, update_mask9
   use model_kernel_breaker_mod, only: wave_breaking, VIS_SCHEME_DEFAULT

   implicit none

   private
   public :: type_model_stepper_2d

   ! Legacy breaking-age threshold (old/init.F: T_brk = 20 when
   ! SHOW_BREAKING); runtime-configurable form deferred.
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

      type(type_model_bc) :: bc

      ! Dispersion coefficient set derived from Beta_ref (legacy
      ! init.F): $b_1 = \beta_{ref}^2$, $b_2 = \beta_{ref}$,
      ! $\beta_1 = \beta_{ref} + 1$, $\beta_2 = (1/5)^2$.
      real(SP) :: b1 = 0.0_SP, b2 = 0.0_SP
      real(SP) :: beta1 = 0.0_SP, beta2 = 0.0_SP

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

      ! Breaking extras (allocated when viscosity_breaking).
      real(SP), allocatable :: etamean(:, :)           ! mixing port pending: 0
      real(SP), allocatable :: roller_flux(:, :)
      real(SP), allocatable :: undertow_u(:, :), undertow_v(:, :)
      logical, allocatable :: in_wm_zone(:, :)         ! wavemaker zone: Step 6d

   contains
      procedure :: init => stepper_init
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
                           wavemaker_type)
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
      character(*), intent(in) :: wavemaker_type

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

      call this%bc%init(grid, wavemaker_type)

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

      if (this%physics%viscosity_breaking) then
         allocate (this%etamean(mloc, nloc), source=0.0_SP)
         allocate (this%roller_flux(mloc, nloc), source=0.0_SP)
         allocate (this%undertow_u(mloc, nloc), source=0.0_SP)
         allocate (this%undertow_v(mloc, nloc), source=0.0_SP)
         allocate (this%in_wm_zone(mloc, nloc), source=.false.)
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

   end subroutine stepper_estimate_dt

   ! ----------------------------------------------------------------
   ! One RK3 stage, legacy order.  time (= t + dt, legacy TIME inside
   ! the stage loop) is unused until the wavemaker source lands (6d).
   ! ----------------------------------------------------------------
   subroutine stepper_stage(this, istage, dt, time)
      class(type_model_stepper_2d), intent(inout) :: this
      integer, intent(in) :: istage
      real(SP), intent(in) :: dt, time

      associate (f => this%fields, lp => this%grid%lp, &
                 phy => this%physics, num => this%numerics)

         if (phy%dispersion) call run_dispersion(this, dt)

         call fluxes(lp, num%high_order, num%construction, &
                     f%eta, f%u, f%v, f%hu, f%hv, this%u4, this%v4, &
                     this%depth_fx, this%depth_fy, this%dx, this%dy, &
                     this%inv_dx, this%inv_dy, f%mask, f%mask9, &
                     phy%Gamma1, phy%Gamma3, phy%dispersion, this%fws)

         call flux_wall_bc(lp, this%bc%fill_west, this%bc%fill_east, &
                           this%bc%fill_south, this%bc%fill_north, &
                           phy%Gamma3, this%depth_fx, this%depth_fy, this%fws)

         ! Manning drag from current H (legacy evaluates inside SourceTerms)
         call this%friction%update_cd(f%h, num%MinDepthFrc)

         call cal_sources(lp, phy%Gamma1, phy%Gamma2, phy%dispersion, &
                          f%mask, f%mask9, this%inv_dx, this%inv_dy, &
                          this%depth_fx, this%depth_fy, f%eta, f%h, f%u, f%v, &
                          this%fws%p, this%fws%q, &
                          this%u4, this%v4, this%u1p, this%v1p, &
                          this%u1pp, this%v1pp, this%u2, this%v2, &
                          this%u3, this%v3, &
                          this%zeros, &   ! wavemaker mass source: Step 6d
                          this%friction%Cd, &
                          merge_nu_vis(this), &
                          num%MinDepthFrc, this%src_x, this%src_y)

         call cal_rk_update(lp, RK_ALPHA(istage), RK_BETA(istage), dt, &
                            this%inv_dx, this%inv_dy, &
                            this%fws%p, this%fws%q, this%fws%fx, this%fws%fy, &
                            this%fws%gx, this%fws%gy, &
                            this%src_x, this%src_y, &
                            this%zeros, &   ! wavemaker mass source: Step 6d
                            f%eta0, f%p0, f%q0, f%eta, f%p, f%q)

         ! legacy GET_Eta_U_V_HU_HV: whole-array H (unclamped; ghost eta
         ! is one exchange behind, exactly as legacy)
         f%h = phy%Gamma3*f%eta + f%depth

         if (phy%dispersion) then
            call cal_etauv_assemble_x(lp, phy%Gamma1, num%MinDepthFrc, &
                                      this%b1, this%b2, this%inv_dx, &
                                      f%mask, f%mask9, f%depth, f%h, f%p, &
                                      this%dws%vxy, this%dws%dvxy, this%ews)
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
                               f%h, f%u, f%v, f%hu, f%hv)
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

         if (phy%viscosity_breaking) then
            ! TODO(6d/6e): vis_scheme selection, wavemaker zone flags, and
            ! ETAmean (mixing port) — untestable until the wavemaker rungs.
            call wave_breaking(lp, this%etax, this%etay, this%etat, &
                               f%eta, f%depth, f%h, f%u, f%v, this%etamean, &
                               this%dx, this%dy, dt, T_BRK_LEGACY, &
                               num%MinDepthFrc, this%breaking%Cbrk1, &
                               this%breaking%Cbrk2, this%breaking%WAVEMAKER_Cbrk, &
                               this%breaking%nu_bkg, VIS_SCHEME_DEFAULT, &
                               phy%SWE_ETA_DEP, this%in_wm_zone, &
                               f%nu_break, f%age_break, this%roller_flux, &
                               this%undertow_u, this%undertow_v)
         end if

         call this%bc%exchange_state(this%grid, f)

         ! wavemaker boundary injection (ABS / LEFT_BC_IRR) — Step 6d
         ! direct sponge damping — Step 6d

      end associate

   end subroutine stepper_stage

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

      ! TODO: MIXING_STUFF (time-averaged statistics) — deferred port

      call update_max_min(this, time)

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
   ! is not ported: legacy updates it inside dispersion.F (Cartesian).
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
   ! Private: cal_dispersion call with the stepper's array wiring.
   ! Boundary flags are the cart-topology walls: back/shore =
   ! west/east, right/left = south/north; periodic wraps report no
   ! boundary.  The u4/v4 ghost update afterwards is legacy
   ! EXCHANGE_DISPERSION: face reconstruction and the source-term
   ! gradients read u4/v4 in the ghosts.
   ! ----------------------------------------------------------------
   subroutine run_dispersion(this, dt)
      class(type_model_stepper_2d), intent(inout) :: this
      real(SP), intent(in) :: dt

      associate (f => this%fields, lp => this%grid%lp, g => this%grid, &
                 phy => this%physics, num => this%numerics)
         call cal_dispersion(lp, this%dws, f%eta, f%depth, f%u, f%v, &
                             this%u0, this%v0, this%fws%p, this%fws%q, &
                             f%mask9, this%inv_dx, this%inv_dy, dt, &
                             num%MinDepthFrc, this%beta1, this%beta2, &
                             phy%Gamma2, this%breaking%show_breaking, &
                             g%is_back_boundary, g%is_shore_boundary, &
                             g%is_right_boundary, g%is_left_boundary, &
                             this%etat, this%ut, this%vt, this%etax, &
                             this%etay, this%u4, this%v4, this%u1p, &
                             this%v1p, this%u1pp, this%v1pp, this%u2, &
                             this%v2, this%u3, this%v3)
         call this%bc%exchange_dispersion(g, this%u4, this%v4)
      end associate

   end subroutine run_dispersion

   ! Effective eddy viscosity for the momentum source: nu_break when
   ! breaking is active (diffusion-sponge contribution: Step 6d).
   function merge_nu_vis(this) result(nu)
      class(type_model_stepper_2d), intent(in), target :: this
      real(SP), pointer :: nu(:, :)

      if (this%physics%viscosity_breaking) then
         nu => this%fields%nu_break
      else
         nu => this%zeros
      end if
   end function merge_nu_vis

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
      if (allocated(this%etamean)) deallocate (this%etamean, this%roller_flux, &
                                               this%undertow_u, this%undertow_v, &
                                               this%in_wm_zone)

      this%env => null()
      this%grid => null()
      this%fields => null()
      this%physics => null()
      this%numerics => null()
      this%breaking => null()
      this%friction => null()
      this%simulation => null()
      this%output => null()

   end subroutine stepper_free

end module model_stepper_2d_mod
