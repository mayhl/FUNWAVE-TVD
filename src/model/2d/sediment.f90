!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Non-cohesive sediment transport and morphology — port of legacy MODULE
!  SEDIMENT_MODULE (old/mod_sediment.F, built under -DSEDIMENT).  Carried so
!  far: the suspended load (advection, diffusion, pickup, deposition of a
!  single grain size), the bedload flux, and the bed change those two drive.
!  Avalanching, cohesive sediment and the feedback into the hydrodynamics land
!  on later rungs.
!
!  With Bed_Change on, the module is two-way: it rewrites the still-water
!  depth every step, and every kernel downstream reads that depth.
!
!  The transported variable is the depth-integrated concentration $CH = c\,h$,
!  advanced on the same RK3 stage weights as the flow:
!
!    $$ (ch)^{(s)} = \alpha_s (ch)^{0}
!         + \beta_s \left[ (ch) + \Delta t \, R \right], \qquad
!       R = -\nabla\!\cdot\!\mathbf{F} + P - D $$
!
!  with the face flux $\mathbf{F}$ carrying advection by the volume flux
!  $(P, Q)$ (plus the roller undertow when the breaker supplies it) and a
!  Fickian diffusion whose coefficient rides the bed shear velocity
!
!    $$ u_* = \frac{0.4\,|\mathbf{u}|}{\ln(30 h_{po}/k_s) - 1}, \qquad
!       k = 5.93\,\bar{u_*}\,\bar{h}_{po}. $$
!
!  Pickup uses van Rijn's reference concentration and deposition Cao (2004):
!
!    $$ P = w_s\,\frac{r\,c_b\,D_{50}}{0.01\,h_{po}}, \qquad
!       c_b = 0.015\left(\frac{\tau - \tau_{cr}}{\tau_{cr}}\right)^{3/2}
!             D_*^{-0.3} $$
!    $$ D = \gamma\,c\,w_s\,(1 - \gamma c)^2, \qquad
!       \gamma = \min\!\left(2, \frac{1-n}{c}\right). $$
!
!  Bedload (Meyer-Peter–Muller form, laid along the current) rides on the same
!  bed shear, above its own threshold:
!
!    $$ q_b = \frac{8\,(\tau - \tau_{cr,b})^{3/2}}{g\,(s-1)}, \qquad
!       (q_{bx}, q_{by}) = q_b\,(\cos\theta, \sin\theta), \quad
!       \theta = \mathrm{atan2}(v, u). $$
!
!  Morphology (once per step, outside the RK loop) integrates the two loads
!  into a bed level and hands the result back as the still-water depth:
!
!    $$ \Sigma_s \mathrel{+}= (-\bar{P} + \bar{D})\,\Delta t, \qquad
!       \Sigma_b \mathrel{-}= (\nabla\!\cdot\!\mathbf{q}_b)\,\Delta t $$
!    $$ z_b = -\frac{\Sigma_s + \Sigma_b}{1-n}, \qquad
!       d = d_{ini} + z_b\,m_f $$
!
!  with $z_b$ positive for erosion, clamped at the hard bottom $z_s$, and the
!  rates the Morph_interval AVERAGES, not the instantaneous ones.
!
!  YAML block: sediment:           (top-level; omit to disable)
!    Sed_Scheme:         <str>     Upwinding | TVD,     default Upwinding
!    D50:                <real>    grain size (m),      default 0.0005
!    Sdensity:           <real>    specific gravity,    default 2.68
!    n_porosity:         <real>    bed porosity,        default 0.47
!    WS:                 <real>    settling velocity (m/s); ABSENT -> formula
!    Shields_cr:         <real>    critical Shields,    default 0.055
!    MinDepthPickup:     <real>    pickup cutoff (m),   default 0.1
!    PickupReduction:    <bool>    cap on c_b,          default YES
!    ReductionParameter: <real>    that cap,            default 0.65
!    C_limiter:          <real>    max concentration; ABSENT -> no limiter
!    Morph_interval:     <real>    averaging window (s); ABSENT -> SMALL
!    Bed_Change:         <bool>    evolve the bathymetry, default NO
!    BedLoad:            <bool>    add the bedload flux,  default NO
!    Shields_cr_bedload: <real>    bedload threshold; ABSENT -> Shields_cr
!    Morph_factor:       <int>     bed-change speed-up,   default 1
!    Hard_bottom:        <bool>    clamp erosion at z_s,  default NO
!    Hard_bottom_file:   <str>     z_s field (needs Hard_bottom)
!
!  Legacy quirks kept:
!    NOTE 1: the y-diffusion loop never recomputes ustar_c — it reads the
!            value the x-diffusion loop left in the module-SAVE scalar at
!            its last wet cell, so k4 mixes a local ustar_c4 with a bed
!            shear velocity from somewhere else entirely.  Reproduced by
!            keeping ustar_c a component (legacy SAVE), not a local: on the
!            first call, before any wet cell, it is the initialised zero.
!    NOTE 2: the deposition loop runs to Iend+1/Jend+1, one cell past the
!            interior every other loop in the file stops at.  The extra
!            column/row is never read back, but D carries it.
!    NOTE 3: H is recomputed here from the CURRENT eta, overwriting the
!            stepper's copy.  It is not a no-op: update_mask has since
!            truncated eta at drying cells, and (with subgrid on) H held
!            the pixel-averaged column, which this discards.  Everything
!            downstream in the stage — breaker, foam — sees this H.
!    NOTE 4: Sed_Scheme = 'TVD' is not TVD; it is a fixed 0.9/0.1 weighted
!            upwind.  Only the first three characters are tested, so any
!            string that is not 'Upw...' selects it.
!    NOTE 5: pickup and deposition are computed AFTER the CH solve, so the
!            P - D the residual carries is one stage old (and zero on the
!            very first stage of the run).  The call order below keeps that.
!    NOTE 6: the bed change integrates the AVERAGED rates over the FULL step
!            dt, but P_ave/D_ave only refresh when the Morph_interval window
!            closes.  Between closings the same averages are integrated again
!            every step — the bed keeps moving at the last window's rate.  A
!            Morph_interval below 3*dt (SMALL, i.e. the default) closes the
!            window on the first stage of every step, which is the only
!            setting where the two stay in step.
!    NOTE 7: the bedload divergence is a centred difference on the CELL-centred
!            flux, not a face difference — 2*dx wide, so it neither telescopes
!            nor sees the odd/even mode.  Kept as-is.
!    NOTE 8: the depth rewrite restaggers DepthX/DepthY but leaves DepthNode
!            stale at its t=0 values.  Legacy does the same; nothing in the 2D
!            path reads DepthNode, so it is inert here — but it is a live trap
!            for anything that starts to.
!    NOTE 9: the hard-bottom clamp in the pickup loop tests the PREVIOUS step's
!            z_b (morphology runs after all three stages), and it sits outside
!            the wet/deep test, so it fires on dry cells too.
!
!  Legacy config NOT ported: Kappa1 / Kappa2 are read, echoed to the log and
!  never used in any formula.  Mask_s is allocated and zeroed but never read —
!  the hard bottom is carried entirely by Zs.
!
!  HISTORY :
!    07/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_sediment_mod

   use core_constants_mod, only: SP, ZERO, SMALL, LARGE, GRAV
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d, type_loop_bounds
   use core_path_mod, only: type_path

   use model_base_mod, only: type_model_base
   use model_bc_mod, only: type_model_bc
   use model_geometry_mod, only: read_field_ascii, stagger_depth
   use model_config_defaults_mod, only: DEF_SEDIMENT_SED_SCHEME, DEF_SEDIMENT_D50, &
                                        DEF_SEDIMENT_SDENSITY, DEF_SEDIMENT_N_POROSITY, &
                                        DEF_SEDIMENT_SHIELDS_CR, &
                                        DEF_SEDIMENT_MINDEPTHPICKUP, &
                                        DEF_SEDIMENT_PICKUPREDUCTION, &
                                        DEF_SEDIMENT_REDUCTIONPARAMETER, &
                                        DEF_SEDIMENT_BED_CHANGE, DEF_SEDIMENT_BEDLOAD, &
                                        DEF_SEDIMENT_MORPH_FACTOR, &
                                        DEF_SEDIMENT_HARD_BOTTOM

   implicit none

   private
   public :: type_model_sediment

   ! von Karman constant and the log-law offset legacy hard-codes into every
   ! bed-shear expression (0.4 / (ln(30 h/k_s) - 1))
   real(SP), parameter :: KAPPA_VK = 0.4_SP
   ! diffusivity coefficient, legacy 5.93 * ubar_star * hbar
   real(SP), parameter :: K_DIFF = 5.93_SP
   ! van Rijn reference-concentration coefficients
   real(SP), parameter :: VR_COEF = 0.015_SP, VR_DSTAR_EXP = -0.3_SP
   ! Cao (2004) deposition cap on (1-n)/c
   real(SP), parameter :: CAO_GAMMA_MAX = 2.0_SP
   ! weighted-upwind ("TVD") stencil weights
   real(SP), parameter :: W_UP = 0.9_SP, W_DOWN = 0.1_SP
   ! kinematic viscosity of water, legacy non-cohesive value
   real(SP), parameter :: NU_WATER = 0.000001_SP
   ! Meyer-Peter-Muller bedload coefficient
   real(SP), parameter :: MPM_COEF = 8.0_SP
   ! slack legacy leaves on the hard-bottom test, so a bed sitting exactly on
   ! z_s still counts as exhausted
   real(SP), parameter :: HARD_BOTTOM_TOL = 0.001_SP

   type, extends(type_model_base) :: type_model_sediment

      ! ---- config
      character(:), allocatable :: sed_scheme
      logical  :: upwinding = .true.
      logical  :: pickup_reduction = .true.
      logical  :: use_climiter = .false.
      logical  :: ws_formula = .false.
      logical  :: bed_change = .false.
      logical  :: bedload = .false.
      logical  :: hard_bottom = .false.

      type(type_path) :: hard_bottom_file
      integer  :: morph_factor = 1

      real(SP) :: d50 = ZERO
      real(SP) :: sdensity = ZERO
      real(SP) :: n_porosity = ZERO
      real(SP) :: ws = ZERO
      real(SP) :: shields_cr = ZERO
      real(SP) :: shields_cr_bedload = ZERO
      real(SP) :: min_depth_pickup = ZERO
      real(SP) :: reduction_parameter = ZERO
      real(SP) :: c_limiter = ZERO
      real(SP) :: morph_interval = ZERO

      ! ---- derived at init
      real(SP) :: viscosity = NU_WATER
      real(SP) :: dstar = ZERO
      real(SP) :: tau_cr = ZERO
      real(SP) :: tau_cr_bedload = ZERO
      real(SP) :: k_s = ZERO

      ! ---- transported state
      real(SP), allocatable :: ch(:, :)      !< concentration c = CHH/h_po
      real(SP), allocatable :: chh(:, :)     !< depth-integrated c*h  (prognostic)
      real(SP), allocatable :: chh0(:, :)    !< step-start copy, for the RK weights
      real(SP), allocatable :: hpo(:, :)     !< max(h, MinDepth)
      real(SP), allocatable :: tau_xy(:, :)  !< bed shear stress / rho
      real(SP), allocatable :: pickup(:, :)  !< erosion rate P
      real(SP), allocatable :: depo(:, :)    !< deposition rate D  (legacy D)

      ! ---- Morph_interval averages (the morphology rung consumes them)
      real(SP), allocatable :: c_sum(:, :), p_sum(:, :), d_sum(:, :)
      real(SP), allocatable :: c_ave(:, :), p_ave(:, :), d_ave(:, :)
      real(SP) :: t_sum = ZERO

      ! ---- face fluxes (advection + diffusion), rebuilt every stage
      real(SP), allocatable :: scal_x(:, :), scal_y(:, :)

      ! ---- morphology.  bed_flux is CELL-centred (legacy BedFluxX/Y), not a
      ! face flux; zb is positive for erosion; zs is the hard bottom (LARGE
      ! where there is none, so the clamp never bites)
      real(SP), allocatable :: bed_flux_x(:, :), bed_flux_y(:, :)
      real(SP), allocatable :: zb(:, :), zs(:, :), depth_ini(:, :)
      real(SP), allocatable :: susp_load(:, :), bed_load(:, :)

      ! Legacy SAVE scalar, deliberately NOT a local: the y-diffusion loop
      ! reads it stale across loops and across calls (header NOTE 1)
      real(SP) :: ustar_c = ZERO

   contains
      procedure :: read_input => sediment_read_input
      procedure :: init_compute => sediment_init_compute
      procedure :: save_step0 => sediment_save_step0
      procedure :: update => sediment_update
      procedure :: morphology => sediment_morphology
      procedure :: free => sediment_free
   end type type_model_sediment

contains

   subroutine sediment_read_input(this, env)
      class(type_model_sediment), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk, no_key

      sub_env = get_sub_env(env, "sediment", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (no_blk) return

      call sub_env%yaml%read("Sed_Scheme", silent=no_key, val=this%sed_scheme, &
                             default=DEF_SEDIMENT_SED_SCHEME)
      ! legacy tests the first three characters only (header NOTE 4)
      this%upwinding = .true.
      if (len(this%sed_scheme) >= 3) then
         this%upwinding = this%sed_scheme(1:3) == "Upw"
      end if

      call sub_env%yaml%read("D50", silent=no_key, val=this%d50, &
                             default=DEF_SEDIMENT_D50)
      call sub_env%yaml%read("Sdensity", silent=no_key, val=this%sdensity, &
                             default=DEF_SEDIMENT_SDENSITY)
      call sub_env%yaml%read("n_porosity", silent=no_key, val=this%n_porosity, &
                             default=DEF_SEDIMENT_N_POROSITY)

      ! WS is presence-tested: absent means "use the settling formula", so it
      ! carries no default (see the yaml%read/silent contract)
      call sub_env%yaml%read("WS", silent=no_key, val=this%ws)
      this%ws_formula = no_key

      call sub_env%yaml%read("Shields_cr", silent=no_key, val=this%shields_cr, &
                             default=DEF_SEDIMENT_SHIELDS_CR)
      call sub_env%yaml%read("MinDepthPickup", silent=no_key, &
                             val=this%min_depth_pickup, &
                             default=DEF_SEDIMENT_MINDEPTHPICKUP)

      call sub_env%yaml%read("PickupReduction", silent=no_key, &
                             val=this%pickup_reduction, &
                             default=DEF_SEDIMENT_PICKUPREDUCTION)
      if (this%pickup_reduction) then
         call sub_env%yaml%read("ReductionParameter", silent=no_key, &
                                val=this%reduction_parameter, &
                                default=DEF_SEDIMENT_REDUCTIONPARAMETER)
      else
         ! legacy "any number": the reduction is 1 whatever this holds
         this%reduction_parameter = 1.0_SP
      end if

      ! C_limiter is presence-tested too — the key existing is the switch
      call sub_env%yaml%read("C_limiter", silent=no_key, val=this%c_limiter)
      this%use_climiter = .not. no_key

      call sub_env%yaml%read("Morph_interval", silent=no_key, val=this%morph_interval)
      if (no_key) this%morph_interval = SMALL

      ! ---- morphology
      call sub_env%yaml%read("Bed_Change", silent=no_key, val=this%bed_change, &
                             default=DEF_SEDIMENT_BED_CHANGE)
      call sub_env%yaml%read("BedLoad", silent=no_key, val=this%bedload, &
                             default=DEF_SEDIMENT_BEDLOAD)

      ! absent means "follow the suspended-load threshold", so no default
      call sub_env%yaml%read("Shields_cr_bedload", silent=no_key, &
                             val=this%shields_cr_bedload)
      if (no_key) this%shields_cr_bedload = this%shields_cr

      call sub_env%yaml%read("Morph_factor", silent=no_key, val=this%morph_factor, &
                             default=DEF_SEDIMENT_MORPH_FACTOR)

      call sub_env%yaml%read("Hard_bottom", silent=no_key, val=this%hard_bottom, &
                             default=DEF_SEDIMENT_HARD_BOTTOM)
      if (this%hard_bottom) then
         call sub_env%yaml%read_input_path("Hard_bottom_file", &
                                           val=this%hard_bottom_file)
      end if

   end subroutine sediment_read_input

   ! ----------------------------------------------------------------
   ! Legacy SEDIMENT_INITIAL, allocation half: every field starts at zero
   ! (a still sea carries no suspended load), and the grain parameters that
   ! depend only on config are folded once:
   !
   !   $$ D_* = D_{50}\left(\frac{(s-1)g}{\nu^2}\right)^{1/3}, \qquad
   !      \tau_{cr} = (s-1)\,g\,D_{50}\,\theta_{cr}, \qquad
   !      k_s = 2.5\,D_{50} $$
   !
   ! and, when WS was absent, the settling velocity itself
   !
   !   $$ w_s = \sqrt{(s-1)gD_{50}}
   !            \left(\sqrt{\tfrac{2}{3} + \tfrac{36\nu^2}{(s-1)gD_{50}^3}}
   !                - \sqrt{\tfrac{36\nu^2}{(s-1)gD_{50}^3}}\right). $$
   !
   ! `depth` is the CORRECTED still-water depth (this runs after the bathy
   ! correction, as legacy's SEDIMENT_INITIAL runs after INITIALIZATION), and
   ! it is the datum every later bed change is measured from.
   ! ----------------------------------------------------------------
   subroutine sediment_init_compute(this, grid, env, depth)
      class(type_model_sediment), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      type(type_env), intent(inout) :: env
      real(SP), intent(in) :: depth(:, :)

      real(SP) :: sgd, nu_term

      if (.not. this%is_activated) return

      associate (m => grid%lp%mloc, n => grid%lp%nloc)
         allocate (this%ch(m, n), source=ZERO)
         allocate (this%chh(m, n), source=ZERO)
         allocate (this%chh0(m, n), source=ZERO)
         allocate (this%hpo(m, n), source=ZERO)
         allocate (this%tau_xy(m, n), source=ZERO)
         allocate (this%pickup(m, n), source=ZERO)
         allocate (this%depo(m, n), source=ZERO)

         allocate (this%c_sum(m, n), source=ZERO)
         allocate (this%p_sum(m, n), source=ZERO)
         allocate (this%d_sum(m, n), source=ZERO)
         allocate (this%c_ave(m, n), source=ZERO)
         allocate (this%p_ave(m, n), source=ZERO)
         allocate (this%d_ave(m, n), source=ZERO)

         allocate (this%scal_x(m + 1, n), source=ZERO)
         allocate (this%scal_y(m, n + 1), source=ZERO)

         allocate (this%bed_flux_x(m, n), source=ZERO)
         allocate (this%bed_flux_y(m, n), source=ZERO)
         allocate (this%zb(m, n), source=ZERO)
         allocate (this%susp_load(m, n), source=ZERO)
         allocate (this%bed_load(m, n), source=ZERO)
         ! no hard bottom anywhere until a file says otherwise
         allocate (this%zs(m, n), source=LARGE)
      end associate

      this%depth_ini = depth

      ! legacy GetFile: interior only, so the ghosts keep their LARGE — which
      ! is what the (interior-only) clamp wants anyway
      if (this%hard_bottom) then
         call read_field_ascii(env, this%hard_bottom_file%root, grid, this%zs)
      end if

      this%viscosity = NU_WATER

      sgd = (this%sdensity - 1.0_SP)*GRAV*this%d50

      this%dstar = this%d50*((this%sdensity - 1.0_SP)*GRAV/this%viscosity**2.0_SP) &
                   **(1.0_SP/3.0_SP)
      this%tau_cr = sgd*this%shields_cr
      this%tau_cr_bedload = sgd*this%shields_cr_bedload
      this%k_s = 2.5_SP*this%d50

      if (this%ws_formula) then
         nu_term = 36.0_SP*this%viscosity**2/((this%sdensity - 1.0_SP)*GRAV*this%d50**3)
         this%ws = sqrt(sgd)*(sqrt(2.0_SP/3.0_SP + nu_term) - sqrt(nu_term))
      end if

   end subroutine sediment_init_compute

   ! Step-start copy for the RK weights (legacy CHH0 = CHH, alongside
   ! Eta0/Ubar0/Vbar0 at the head of the step)
   subroutine sediment_save_step0(this)
      class(type_model_sediment), intent(inout) :: this

      if (.not. this%is_activated) return
      this%chh0 = this%chh

   end subroutine sediment_save_step0

   ! ----------------------------------------------------------------
   ! One RK stage: legacy SEDIMENT_ADVECTION_DIFFUSION, called between
   ! TIDE_BC and WAVE_BREAKING.  `h` is intent(inout) because legacy
   ! rebuilds it here off the current eta (header NOTE 3).
   ! ----------------------------------------------------------------
   subroutine sediment_update(this, bc, grid, alpha, beta, dt, gamma3, min_depth, &
                              inv_dx, inv_dy, mask, eta, depth, u, v, &
                              p_face, q_face, h, roller, undertow_u, undertow_v)
      class(type_model_sediment), intent(inout) :: this
      type(type_model_bc), intent(in) :: bc
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: alpha, beta, dt, gamma3, min_depth
      real(SP), intent(in) :: inv_dx(:, :), inv_dy(:, :)
      integer, intent(in) :: mask(:, :)
      real(SP), intent(in) :: eta(:, :), depth(:, :)
      real(SP), intent(in) :: u(:, :), v(:, :)
      ! the face fluxes the stage's flux kernel built (legacy P/Q, shaped
      ! (mloc+1, nloc) / (mloc, nloc+1)) -- NOT the cell-centred Ubar/Vbar
      real(SP), intent(in) :: p_face(:, :), q_face(:, :)
      real(SP), intent(inout) :: h(:, :)
      logical, intent(in) :: roller
      real(SP), intent(in) :: undertow_u(:, :), undertow_v(:, :)

      if (.not. this%is_activated) return

      h = gamma3*eta + depth
      this%hpo = max(h, min_depth)

      call sediment_advect(this, grid%lp, mask, p_face, q_face, roller, &
                           undertow_u, undertow_v)
      call sediment_diffuse(this, grid%lp, inv_dx, inv_dy, mask, u, v)
      call sediment_flux_bc(this, grid, mask)
      call sediment_solve(this, grid%lp, alpha, beta, dt, inv_dx, inv_dy, mask)
      call bc%exchange_scalar(grid, this%ch)

      call sediment_pickup(this, grid%lp, mask, u, v, h)
      ! the morphology's divergence reads i+-1 / j+-1, so the cell-centred
      ! bedload flux has to carry its ghosts
      if (this%bedload) then
         call bc%exchange_scalar(grid, this%bed_flux_x)
         call bc%exchange_scalar(grid, this%bed_flux_y)
      end if

      call sediment_deposit(this, grid%lp, mask)
      call sediment_average(this, dt)
      call bc%exchange_scalar(grid, this%c_ave)

   end subroutine sediment_update

   ! ----------------------------------------------------------------
   ! Upwind (or 0.9/0.1 weighted upwind) advective face flux of CH by the
   ! volume flux, the roller undertow riding along when the breaker feeds
   ! it in:
   !   $$ F_{i} = \left(P_i + \tfrac{1}{2}(U^{ud}_{i-1} + U^{ud}_{i})\right)
   !              \, CH \big|_{\mathrm{upwind}} $$
   ! A dry donor cell contributes nothing.
   ! ----------------------------------------------------------------
   subroutine sediment_advect(this, lp, mask, p_face, q_face, roller, &
                              undertow_u, undertow_v)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in) :: mask(:, :)
      real(SP), intent(in) :: p_face(:, :), q_face(:, :)
      logical, intent(in) :: roller
      real(SP), intent(in) :: undertow_u(:, :), undertow_v(:, :)

      integer :: i, j
      real(SP) :: flx, wu, wd

      if (this%upwinding) then
         wu = 1.0_SP
         wd = ZERO
      else
         wu = W_UP
         wd = W_DOWN
      end if

      this%scal_x = ZERO
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie + 1
            flx = p_face(i, j)
            if (roller) flx = flx + 0.5_SP*(undertow_u(i - 1, j) + undertow_u(i, j))
            if (flx >= ZERO) then
               if (mask(i - 1, j) /= 0) &
                  this%scal_x(i, j) = flx*(wu*this%ch(i - 1, j) + wd*this%ch(i, j))
            else
               if (mask(i, j) /= 0) &
                  this%scal_x(i, j) = flx*(wu*this%ch(i, j) + wd*this%ch(i - 1, j))
            end if
         end do
      end do

      this%scal_y = ZERO
      do j = lp%jb, lp%je + 1
         do i = lp%ib, lp%ie
            flx = q_face(i, j)
            if (roller) flx = flx + 0.5_SP*(undertow_v(i, j - 1) + undertow_v(i, j))
            if (flx >= ZERO) then
               if (mask(i, j - 1) /= 0) &
                  this%scal_y(i, j) = flx*(wu*this%ch(i, j - 1) + wd*this%ch(i, j))
            else
               if (mask(i, j) /= 0) &
                  this%scal_y(i, j) = flx*(wu*this%ch(i, j) + wd*this%ch(i, j - 1))
            end if
         end do
      end do

   end subroutine sediment_advect

   ! ----------------------------------------------------------------
   ! Fickian diffusion added onto the same faces (the roller is left out,
   ! as in legacy).  The face diffusivity averages the two cells' shear
   ! velocities and depths:
   !   $$ k = \tfrac{5.93}{4}(u_{*,i-1} + u_{*,i})(h_{i-1} + h_{i}), \qquad
   !      F_i \mathrel{-}= k\,\frac{h_{i-1}+h_{i}}{2}\,
   !          \frac{CH_i - CH_{i-1}}{\Delta x} $$
   ! ustar_c is the module-SAVE scalar of header NOTE 1: computed in the x
   ! sweep, read (never rewritten) by the y sweep.
   ! ----------------------------------------------------------------
   subroutine sediment_diffuse(this, lp, inv_dx, inv_dy, mask, u, v)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in) :: inv_dx(:, :), inv_dy(:, :)
      integer, intent(in) :: mask(:, :)
      real(SP), intent(in) :: u(:, :), v(:, :)

      integer :: i, j
      real(SP) :: ustar_c2, ustar_c4, k2, k4

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie + 1
            if (mask(i, j) > 0) then
               this%ustar_c = shear_velocity(this, u(i, j), v(i, j), this%hpo(i, j))

               if (mask(i - 1, j) > 0) then
                  ustar_c2 = shear_velocity(this, u(i - 1, j), v(i - 1, j), &
                                            this%hpo(i - 1, j))
                  k2 = K_DIFF*(ustar_c2 + this%ustar_c) &
                       *(this%hpo(i - 1, j) + this%hpo(i, j))/4.0_SP
                  this%scal_x(i, j) = this%scal_x(i, j) &
                                      - k2*(this%hpo(i - 1, j) + this%hpo(i, j)) &
                                      *(this%ch(i, j) - this%ch(i - 1, j)) &
                                      *0.5_SP*inv_dx(i, j)
               end if
            end if
         end do
      end do

      do j = lp%jb, lp%je + 1
         do i = lp%ib, lp%ie
            if (mask(i, j) > 0) then
               if (mask(i, j - 1) > 0) then
                  ustar_c4 = shear_velocity(this, u(i, j - 1), v(i, j - 1), &
                                            this%hpo(i, j - 1))
                  ! NOTE 1: ustar_c is whatever the x sweep left behind
                  k4 = K_DIFF*(ustar_c4 + this%ustar_c) &
                       *(this%hpo(i, j) + this%hpo(i, j - 1))/4.0_SP
                  this%scal_y(i, j) = this%scal_y(i, j) &
                                      - k4*(this%hpo(i, j - 1) + this%hpo(i, j)) &
                                      *(this%ch(i, j) - this%ch(i, j - 1)) &
                                      *0.5_SP*inv_dy(i, j)
               end if
            end if
         end do
      end do

   end subroutine sediment_diffuse

   ! Log-law bed shear velocity
   !   $$ u_* = \frac{0.4\,\sqrt{u^2+v^2}}{\ln(30 h_{po}/k_s) - 1} $$
   pure function shear_velocity(this, uu, vv, hp) result(us)
      class(type_model_sediment), intent(in) :: this
      real(SP), intent(in) :: uu, vv, hp
      real(SP) :: us

      us = KAPPA_VK*sqrt(uu*uu + vv*vv) &
           /(-1.0_SP + log(30.0_SP*hp/this%k_s))

   end function shear_velocity

   ! ----------------------------------------------------------------
   ! Legacy FLUX_SCALAR_BC: no sediment crosses a physical wall, and a dry
   ! cell exchanges nothing with any of its four faces.  The dry sweep runs
   ! in index order and writes the i+1 / j+1 faces too, so a wet cell that
   ! follows a dry one has its shared face zeroed by its neighbour.
   ! ----------------------------------------------------------------
   subroutine sediment_flux_bc(this, grid, mask)
      class(type_model_sediment), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      integer, intent(in) :: mask(:, :)

      integer :: i, j

      associate (lp => grid%lp)
         if (grid%is_back_boundary) this%scal_x(lp%ib, lp%jb:lp%je) = ZERO
         if (grid%is_shore_boundary) this%scal_x(lp%ie + 1, lp%jb:lp%je) = ZERO
         if (grid%is_right_boundary) this%scal_y(lp%ib:lp%ie, lp%jb) = ZERO
         if (grid%is_left_boundary) this%scal_y(lp%ib:lp%ie, lp%je + 1) = ZERO

         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               if (mask(i, j) == 0) then
                  this%scal_x(i, j) = ZERO
                  this%scal_x(i + 1, j) = ZERO
                  this%scal_y(i, j) = ZERO
                  this%scal_y(i, j + 1) = ZERO
               end if
            end do
         end do
      end associate

   end subroutine sediment_flux_bc

   ! ----------------------------------------------------------------
   ! Flux divergence plus the exchange with the bed, on the stage weights:
   !   $$ (ch)^{(s)} = \alpha (ch)^0 + \beta\left[(ch)
   !        - \Delta t\left(\partial_x F + \partial_y G - P + D\right)\right] $$
   ! then the concentration itself, clipped at zero and (optionally) capped.
   ! ----------------------------------------------------------------
   subroutine sediment_solve(this, lp, alpha, beta, dt, inv_dx, inv_dy, mask)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in) :: alpha, beta, dt
      real(SP), intent(in) :: inv_dx(:, :), inv_dy(:, :)
      integer, intent(in) :: mask(:, :)

      integer :: i, j
      real(SP) :: r1

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            if (mask(i, j) > 0) then
               r1 = -((this%scal_x(i + 1, j) - this%scal_x(i, j))*inv_dx(i, j) &
                      + (this%scal_y(i, j + 1) - this%scal_y(i, j))*inv_dy(i, j)) &
                    + this%pickup(i, j) - this%depo(i, j)

               this%chh(i, j) = alpha*this%chh0(i, j) &
                                + beta*(this%chh(i, j) + dt*r1)

               if (this%chh(i, j) < ZERO) this%chh(i, j) = ZERO

               this%ch(i, j) = this%chh(i, j)/this%hpo(i, j)

               if (this%use_climiter) then
                  if (this%ch(i, j) > this%c_limiter) then
                     this%ch(i, j) = this%c_limiter
                     this%chh(i, j) = this%ch(i, j)*this%hpo(i, j)
                  end if
               end if
            end if
         end do
      end do

   end subroutine sediment_solve

   ! ----------------------------------------------------------------
   ! Bed shear stress (rho divided out, as it is out of tau_cr too) and the
   ! van Rijn pickup it drives:
   !   $$ \tau = \frac{0.16\,|\mathbf{u}|^2}{\left(1 + \ln(k_s/30h_{po})\right)^2} $$
   ! Below tau_cr, or too shallow to pick up, the rate is zero.
   !
   ! The bedload flux rides the same tau above its own threshold, and both it
   ! and the pickup are shut off where the bed has eroded down to the hard
   ! bottom (header NOTE 9).
   ! ----------------------------------------------------------------
   subroutine sediment_pickup(this, lp, mask, u, v, h)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in) :: mask(:, :)
      real(SP), intent(in) :: u(:, :), v(:, :), h(:, :)

      integer :: i, j
      real(SP) :: u_c, c_b, c_a, reduction, angle_cur, bedf

      if (this%bedload) then
         this%bed_flux_x = ZERO
         this%bed_flux_y = ZERO
      end if

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            if (mask(i, j) > 0 .and. h(i, j) > this%min_depth_pickup) then
               u_c = sqrt(u(i, j)*u(i, j) + v(i, j)*v(i, j))

               this%tau_xy(i, j) = 0.16_SP &
                                   /(1.0_SP + log(this%k_s/(30.0_SP*this%hpo(i, j))))**2 &
                                   *(u_c**2.0_SP)

               ! the Hpo test is redundant with the H test above, but legacy
               ! carries both and they differ at a cell clamped to MinDepth
               if (this%tau_xy(i, j) > this%tau_cr .and. &
                   this%hpo(i, j) >= this%min_depth_pickup) then

                  c_b = VR_COEF*(((this%tau_xy(i, j) - this%tau_cr)/this%tau_cr)**1.5_SP) &
                        *this%dstar**VR_DSTAR_EXP
                  if (this%pickup_reduction) then
                     reduction = min(1.0_SP, this%reduction_parameter/c_b)
                  else
                     reduction = 1.0_SP
                  end if
                  c_a = reduction*c_b*this%d50/(0.01_SP*this%hpo(i, j))
                  this%pickup(i, j) = max(ZERO, c_a*this%ws)
               else
                  this%pickup(i, j) = ZERO
               end if

               if (this%bedload) then
                  if (this%tau_xy(i, j) > this%tau_cr_bedload) then
                     angle_cur = atan2(v(i, j), u(i, j))
                     bedf = MPM_COEF &
                            *(this%tau_xy(i, j) - this%tau_cr_bedload)**1.5_SP &
                            /GRAV/(this%sdensity - 1.0_SP)
                     this%bed_flux_x(i, j) = bedf*cos(angle_cur)
                     this%bed_flux_y(i, j) = bedf*sin(angle_cur)
                  else
                     this%bed_flux_x(i, j) = ZERO
                     this%bed_flux_y(i, j) = ZERO
                  end if
               end if
            else
               this%pickup(i, j) = ZERO
            end if

            ! NOTE 9: outside the wet/deep test, so a dry cell over an
            ! exhausted bed lands here too, and zb is the previous step's
            if (this%hard_bottom) then
               if (this%zb(i, j) >= this%zs(i, j) - HARD_BOTTOM_TOL) then
                  this%pickup(i, j) = ZERO
                  if (this%bedload) then
                     this%bed_flux_x(i, j) = ZERO
                     this%bed_flux_y(i, j) = ZERO
                  end if
               end if
            end if
         end do
      end do

   end subroutine sediment_pickup

   ! ----------------------------------------------------------------
   ! Cao (2004) deposition, hindered by the sediment already in suspension:
   !   $$ D = \gamma\,c\,w_s\,(1 - \gamma c)^2, \qquad
   !      \gamma = \min\!\left(2,\ \frac{1-n}{c}\right) $$
   ! Loop bounds are legacy's, one cell past the interior (header NOTE 2).
   ! ----------------------------------------------------------------
   subroutine sediment_deposit(this, lp, mask)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in) :: mask(:, :)

      integer :: i, j
      real(SP) :: gamma_cao

      do j = lp%jb, lp%je + 1
         do i = lp%ib, lp%ie + 1
            if (mask(i, j) > 0) then
               gamma_cao = min(CAO_GAMMA_MAX, &
                               (1.0_SP - this%n_porosity)/max(SMALL, this%ch(i, j)))
               this%depo(i, j) = gamma_cao*this%ch(i, j)*this%ws &
                                 *(1.0_SP - gamma_cao*this%ch(i, j))**2.0_SP
            else
               this%depo(i, j) = ZERO
            end if
         end do
      end do

   end subroutine sediment_deposit

   ! ----------------------------------------------------------------
   ! Running means over Morph_interval, which the morphology rung integrates
   ! instead of the instantaneous rates.  The accumulators are added to on
   ! every stage (so a 3-stage step accumulates 3*dt of "time"), and the
   ! window closes on the first stage that carries t_sum past the interval.
   ! ----------------------------------------------------------------
   subroutine sediment_average(this, dt)
      class(type_model_sediment), intent(inout) :: this
      real(SP), intent(in) :: dt

      this%t_sum = this%t_sum + dt

      this%c_sum = this%c_sum + this%ch*dt
      this%p_sum = this%p_sum + this%pickup*dt
      this%d_sum = this%d_sum + this%depo*dt

      if (this%t_sum >= this%morph_interval) then
         this%p_ave = this%p_sum/this%t_sum
         this%d_ave = this%d_sum/this%t_sum
         this%c_ave = this%c_sum/this%t_sum

         this%t_sum = ZERO
         this%c_sum = ZERO
         this%p_sum = ZERO
         this%d_sum = ZERO
      end if

   end subroutine sediment_average

   ! ----------------------------------------------------------------
   ! Legacy MORPHOLOGICAL_CHANGE: once per step, OUTSIDE the RK loop (so it
   ! sees the last stage's fluxes and the completed step's dt), gated on
   ! Bed_Change.  Integrates the two loads into a bed level and hands it back
   ! as the still-water depth:
   !
   !   $$ \Sigma_s \mathrel{+}= (-\bar P + \bar D)\,\Delta t, \qquad
   !      \Sigma_b \mathrel{-}= \left(\frac{\partial q_{bx}}{\partial x}
   !          + \frac{\partial q_{by}}{\partial y}\right)\Delta t $$
   !   $$ z_b = -\frac{\Sigma_s + \Sigma_b}{1-n}
   !            \quad\text{(clamped at } z_s), \qquad
   !      d = d_{ini} + z_b\,m_f $$
   !
   ! The bed-level integration is the ONLY consumer of P_ave/D_ave, and the
   ! divergence is the 2*dx centred one legacy uses (header NOTE 7).  After the
   ! rewrite, depth needs its ghosts back and the face-staggered depths rebuilt
   ! — everything downstream reads those, not the cell values.
   ! ----------------------------------------------------------------
   subroutine sediment_morphology(this, bc, grid, dt, inv_dx, inv_dy, &
                                  depth, depth_x, depth_y)
      class(type_model_sediment), intent(inout) :: this
      type(type_model_bc), intent(in) :: bc
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: dt
      real(SP), intent(in) :: inv_dx(:, :), inv_dy(:, :)
      real(SP), intent(inout) :: depth(:, :), depth_x(:, :), depth_y(:, :)

      integer :: i, j

      if (.not. this%is_activated) return
      if (.not. this%bed_change) return

      associate (lp => grid%lp)
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               this%susp_load(i, j) = this%susp_load(i, j) &
                                      + (-this%p_ave(i, j) + this%d_ave(i, j))*dt

               this%bed_load(i, j) = this%bed_load(i, j) &
                                     - (this%bed_flux_x(i + 1, j) - this%bed_flux_x(i - 1, j)) &
                                     *0.5_SP*inv_dx(i, j)*dt &
                                     - (this%bed_flux_y(i, j + 1) - this%bed_flux_y(i, j - 1)) &
                                     *0.5_SP*inv_dy(i, j)*dt

               ! positive for erosion
               this%zb(i, j) = -(this%susp_load(i, j) + this%bed_load(i, j)) &
                               /(1.0_SP - this%n_porosity)

               if (this%zb(i, j) > this%zs(i, j)) this%zb(i, j) = this%zs(i, j)
            end do
         end do

         depth = this%depth_ini + this%zb*this%morph_factor
      end associate

      call bc%exchange_scalar(grid, depth)
      call stagger_depth(grid%lp, depth, depth_x, depth_y)

   end subroutine sediment_morphology

   subroutine sediment_free(this)
      class(type_model_sediment), intent(inout) :: this

      if (allocated(this%ch)) deallocate (this%ch)
      if (allocated(this%chh)) deallocate (this%chh)
      if (allocated(this%chh0)) deallocate (this%chh0)
      if (allocated(this%hpo)) deallocate (this%hpo)
      if (allocated(this%tau_xy)) deallocate (this%tau_xy)
      if (allocated(this%pickup)) deallocate (this%pickup)
      if (allocated(this%depo)) deallocate (this%depo)
      if (allocated(this%c_sum)) deallocate (this%c_sum)
      if (allocated(this%p_sum)) deallocate (this%p_sum)
      if (allocated(this%d_sum)) deallocate (this%d_sum)
      if (allocated(this%c_ave)) deallocate (this%c_ave)
      if (allocated(this%p_ave)) deallocate (this%p_ave)
      if (allocated(this%d_ave)) deallocate (this%d_ave)
      if (allocated(this%scal_x)) deallocate (this%scal_x)
      if (allocated(this%scal_y)) deallocate (this%scal_y)
      if (allocated(this%bed_flux_x)) deallocate (this%bed_flux_x)
      if (allocated(this%bed_flux_y)) deallocate (this%bed_flux_y)
      if (allocated(this%zb)) deallocate (this%zb)
      if (allocated(this%zs)) deallocate (this%zs)
      if (allocated(this%depth_ini)) deallocate (this%depth_ini)
      if (allocated(this%susp_load)) deallocate (this%susp_load)
      if (allocated(this%bed_load)) deallocate (this%bed_load)

   end subroutine sediment_free

end module model_sediment_mod
