!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Sediment transport and morphology — port of legacy MODULE SEDIMENT_MODULE
!  (old/mod_sediment.F, built under -DSEDIMENT).  Carried so far: the suspended
!  load (advection, diffusion, pickup, deposition of a single grain size), the
!  bedload flux, the bed change those two drive, the avalanching that relaxes it
!  back to the angle of repose, the cohesive alternative to the pickup and
!  settling laws, and the feedback of the load into the flow's own mass and
!  momentum equations.
!
!  With Bed_Change on, the module is two-way: it rewrites the still-water
!  depth every step, and every kernel downstream reads that depth.  With any of
!  the three feedback switches on it is two-way a second, faster way: the
!  stage residual of the flow carries a sediment term.
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
!  CohesiveSediment swaps the two constitutive laws above (and only those — the
!  transport, the bed change and the avalanching are untouched).  Pickup becomes
!  an erosion rate keyed on the excess shear, in one of two forms:
!
!    $$ P = E\,e^{\alpha\sqrt{|\tau - \tau_{cr}|}} \quad (\mathrm{SoftBed}),
!       \qquad
!       P = E\left(\frac{\tau}{\tau_{cr}} - 1\right) \quad (\mathrm{consolidated}) $$
!
!  and settling becomes a flocculation curve in the near-bed mass concentration
!  $c_b = c\,s\,\rho_w$, deposited only below a separate critical stress:
!
!    $$ w_s = \frac{a\,c_b^{\,n}}{(c_b^2 + b^2)^m}, \qquad
!       D = w_s\,c\,\left(1 - \frac{\tau}{\tau_{cr,d}}\right). $$
!
!  The feedback is three separately switchable terms.  The load exchanged with
!  the bed displaces water, which is a mass source; the concentration gradient
!  tilts the pressure the depth-averaged momentum feels; and the sediment
!  leaving or joining the column carries its momentum with it:
!
!    $$ S_{mass} = \frac{P - D}{1 - n} \qquad (\mathrm{SedimentMassSource}) $$
!    $$ (S^{DC}_x, S^{DC}_y) = -\frac{(s-1)\,g\,h_{po}^2}{1 + \bar{c}(s-1)}
!       \nabla \bar{c} \qquad (\mathrm{SedimentMomentDC}) $$
!    $$ (S^{EXG}_x, S^{EXG}_y) = -\frac{(s-1)\max(1 - n - \bar{c},\,0)}
!       {\left[1 + \bar{c}(s-1)\right](1-n)}\,(P - D)\,(u, v)
!       \qquad (\mathrm{SedimentMomentEXG}) $$
!
!  with $\bar{c}$ the Morph_interval average and $P - D$ the INSTANTANEOUS rates
!  (NOTE 19).  The stage residuals pick them up as $R_1 \mathrel{+}= S_{mass}$,
!  $R_{2,3} \mathrel{+}= S^{DC} + S^{EXG}$.
!
!  YAML block: sediment:           (top-level; omit to disable)
!    Sed_Scheme:         <str>     Upwinding | TVD,     default Upwinding
!    D50:                <real>    grain size (m); ABSENT -> 0.0005, or 5e-6
!                                  when CohesiveSediment (NOTE 13)
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
!    Avalanche:          <bool>    relax slopes past repose, default NO
!    Tan_phi:            <real>    angle of repose,       default 0.7
!    Aval_interval:      <real>    relaxation period (s); ABSENT -> SMALL
!    CohesiveSediment:   <bool>    mud laws instead of sand, default NO
!    SoftBed:            <bool>    unconsolidated pickup law, default YES
!    Tau_cr_coh:         <real>    critical pickup stress,  default 0.001
!    Tau_crd_coh:        <real>    critical deposition stress, default 0.001
!    E_coh:              <real>    erosion rate,          default 0.0001
!    alpha_coh:          <real>    SoftBed exponent,      default 1.0
!    a_coh / b_coh:      <real>    floc settling,         default 0.1 / 2.0
!    n_coh / m_coh:      <real>    floc settling exponents, default 0.5 / 1.5
!    k_coh:              <real>    inert (NOTE 16),       default 1e-6
!    SedimentMassSource: <bool>    (P-D) into the eta residual,  default NO
!    SedimentMomentDC:   <bool>    dc/dx into the momentum,      default NO
!    SedimentMomentEXG:  <bool>    exchanged momentum,           default NO
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
!    NOTE 10: avalanching writes its neighbour's share into zb_aval directly, so
!            a cell that has already been written as somebody's downhill
!            neighbour is OVERWRITTEN, not added to, when the sweep reaches it.
!            The relaxation is therefore i-then-j sweep-ordered and does not
!            conserve sediment.  Legacy accepts this to keep the scan from
!            chasing its own tail; kept.
!    NOTE 11: zb_aval is never halo-exchanged, and the neighbour share can land
!            in a ghost cell.  That share is then dropped — the depth rewrite's
!            exchange overwrites the ghosts.  Sediment leaks out of every
!            subdomain edge, so the avalanched bed is decomposition-dependent.
!    NOTE 12: the slope is measured on the depth from the PREVIOUS rewrite (it is
!            the last thing computed here), and that depth carries Morph_factor.
!            With Morph_factor > 1 the test therefore sees the amplified bed but
!            dh is subtracted from the un-amplified z_b, so repose is enforced
!            at the wrong angle.  Only Morph_factor = 1 is self-consistent.
!    NOTE 13: D50's fallback is conditional on CohesiveSediment — 0.5 mm sand or
!            5 nm mud — which the flat registry cannot express, so the key is
!            presence-tested here and the two live as parameters below.  The mud
!            value is NOT inert: it sets k_s = 2.5 D50, and k_s is in the bed
!            shear every cohesive cell is picked up by.
!    NOTE 14: cohesive silently retires the bedload.  BedFluxX/Y are zeroed at
!            the top of the pickup loop and the cohesive branch never refills
!            them, so BedLoad = YES with CohesiveSediment = YES gives no bedload
!            and no warning.  The bed then moves on the suspended load alone.
!    NOTE 15: cohesive deposition goes NEGATIVE above Tau_crd_coh — Pd = 1 -
!            tau/tau_crd is unbounded below — so D turns into a second erosion
!            term stacked on top of the pickup, and the residual's P - D adds
!            them instead of opposing them.  Legacy does not clamp it; kept.
!    NOTE 16: k_coh is inert.  It is documented as the diffusion coefficient but
!            legacy assigns it to the molecular viscosity, whose only consumers
!            (Dstar and the WS formula) are both non-cohesive-only.  Setting it
!            changes nothing.  The transport diffusivity is 5.93 ubar_star hbar,
!            same as sand.
!    NOTE 17: the cohesive pickup drops the Hpo >= MinDepthPickup test that the
!            sand branch applies (on top of the shared H > MinDepthPickup gate).
!            The two differ at a cell whose H has been clamped up to MinDepth,
!            so cohesive picks up from a few cells sand would not.
!    NOTE 18: the SoftBed law is DISCONTINUOUS at Tau_cr_coh.  It switches on at
!            E_coh, not at zero — exp(alpha*sqrt(0)) = 1 — so a cell crossing the
!            threshold steps the erosion rate by a full E_coh.  (van Rijn's sand
!            pickup rises as (tau - tau_cr)^1.5 and has no such cliff, and the
!            consolidated law below is likewise continuous.)  In practice the law
!            is close to a binary switch: at wave-scale stresses the exponent is
!            sqrt of a ~1e-5 number, so alpha_coh modulates E_coh by ~1%, and the
!            pickup field is bimodal — either 0 or E_coh, with nothing between.
!            Any perturbation of the flow therefore lands O(E_coh) differences in
!            the pickup of whichever cells straddle.  Kept; it is the law.
!    NOTE 19: the feedback mixes two timescales.  The DC term reads C_ave — the
!            Morph_interval average, which is a STAIRCASE (NOTE 6): it holds the
!            last closed window's value until the next one closes.  The MASS and
!            EXG terms read the instantaneous P and D of the current stage.  So
!            with a Morph_interval above ~3*dt the pressure-gradient feedback
!            lags the flow by up to a window while the other two track it.
!    NOTE 20: the five source arrays are written only where MASK > 0 and are
!            never zeroed.  A cell that dries keeps the term it carried when it
!            was last wet, and the residual loop in the flow solver has no mask
!            test — so the stale source is still added into R1/R2/R3 there.
!    NOTE 21: legacy fills all five arrays whenever ANY of the three switches is
!            on, and the flow solver then reads only the ones whose switch is
!            set.  Kept: it costs one array of arithmetic and keeps the branch
!            structure where legacy put it.
!    NOTE 22: the propeller jet feeds the sediment through the bed shear, at
!            three sites: it adds its shear velocity onto the log-law u_* used
!            in the diffusivity, adds its dynamic head onto tau in the pickup,
!            and swings the bedload onto the total (flow + jet) velocity:
!              $$ u_* \mathrel{+}= u_{*p}, \quad
!                 \tau \mathrel{+}= u_{*p}^2, \quad
!                 \phi = \mathrm{atan2}(v + v_p,\, u + u_p). $$
!            Legacy guards them with the PROPELLER && VESSEL compile flags; the
!            modern gate prop_on is true only when the vessel is active with the
!            propeller on, so a propeller-off run is bitwise the pre-coupling
!            path.  The jet velocities come from the vessel module (its upc/up/
!            vp totals), passed in by the stepper.
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

   use core_constants_mod, only: SP, ZERO, SMALL, LARGE, GRAV, RHO_WATER
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d, type_loop_bounds
   use core_path_mod, only: type_path

   use model_base_mod, only: type_model_base
   use model_bc_mod, only: type_model_bc
   use model_geometry_mod, only: read_field_ascii, stagger_depth
   use model_config_defaults_mod, only: DEF_SEDIMENT_SED_SCHEME, &
                                        DEF_SEDIMENT_SDENSITY, DEF_SEDIMENT_N_POROSITY, &
                                        DEF_SEDIMENT_SHIELDS_CR, &
                                        DEF_SEDIMENT_MINDEPTHPICKUP, &
                                        DEF_SEDIMENT_PICKUPREDUCTION, &
                                        DEF_SEDIMENT_REDUCTIONPARAMETER, &
                                        DEF_SEDIMENT_BED_CHANGE, DEF_SEDIMENT_BEDLOAD, &
                                        DEF_SEDIMENT_MORPH_FACTOR, &
                                        DEF_SEDIMENT_HARD_BOTTOM, &
                                        DEF_SEDIMENT_AVALANCHE, DEF_SEDIMENT_TAN_PHI, &
                                        DEF_SEDIMENT_COHESIVESEDIMENT, DEF_SEDIMENT_SOFTBED, &
                                        DEF_SEDIMENT_TAU_CR_COH, DEF_SEDIMENT_TAU_CRD_COH, &
                                        DEF_SEDIMENT_E_COH, DEF_SEDIMENT_ALPHA_COH, &
                                        DEF_SEDIMENT_A_COH, DEF_SEDIMENT_B_COH, &
                                        DEF_SEDIMENT_N_COH, DEF_SEDIMENT_M_COH, &
                                        DEF_SEDIMENT_K_COH, &
                                        DEF_SEDIMENT_SEDIMENTMASSSOURCE, &
                                        DEF_SEDIMENT_SEDIMENTMOMENTDC, &
                                        DEF_SEDIMENT_SEDIMENTMOMENTEXG

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
   ! the two D50 fallbacks legacy picks between on CohesiveSediment (NOTE 13)
   real(SP), parameter :: D50_SAND = 0.0005_SP, D50_MUD = 0.000005_SP
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
      logical  :: avalanche = .false.
      logical  :: cohesive = .false.
      logical  :: soft_bed = .true.
      logical  :: mass_source = .false.
      logical  :: moment_dc = .false.
      logical  :: moment_exg = .false.

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
      real(SP) :: tan_phi = ZERO
      real(SP) :: aval_interval = ZERO

      ! ---- cohesive.  k_coh is inert (NOTE 16) but carried so the log and the
      ! legacy bridge stay honest
      real(SP) :: tau_cr_coh = ZERO
      real(SP) :: tau_crd_coh = ZERO
      real(SP) :: e_coh = ZERO
      real(SP) :: alpha_coh = ZERO
      real(SP) :: a_coh = ZERO
      real(SP) :: b_coh = ZERO
      real(SP) :: n_coh = ZERO
      real(SP) :: m_coh = ZERO
      real(SP) :: k_coh = ZERO

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
      ! the two loads scaled by 1/(1-n) -- output-only (DchgS / DchgB); legacy
      ! divides at write time, we carry the divided copy so the registry can
      ! point a plain array at it
      real(SP), allocatable :: dchg_s(:, :), dchg_b(:, :)

      ! ---- feedback into the flow.  Filled whenever any of the three switches
      ! is on (NOTE 21), read by the flow's residual per switch
      real(SP), allocatable :: mass_sed(:, :)
      real(SP), allocatable :: dc_x(:, :), dc_y(:, :)
      real(SP), allocatable :: exg_x(:, :), exg_y(:, :)

      ! ---- avalanching.  zb_aval is the bed each relaxation moves (survives
      ! between relaxations, so the output holds the last one); aval_accum is
      ! its running total
      real(SP), allocatable :: zb_aval(:, :), aval_accum(:, :)
      real(SP) :: t_aval = ZERO

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

      ! ---- cohesive, read ahead of D50 because it selects D50's fallback
      call sub_env%yaml%read("CohesiveSediment", silent=no_key, val=this%cohesive, &
                             default=DEF_SEDIMENT_COHESIVESEDIMENT)
      call sub_env%yaml%read("SoftBed", silent=no_key, val=this%soft_bed, &
                             default=DEF_SEDIMENT_SOFTBED)
      call sub_env%yaml%read("Tau_cr_coh", silent=no_key, val=this%tau_cr_coh, &
                             default=DEF_SEDIMENT_TAU_CR_COH)
      call sub_env%yaml%read("Tau_crd_coh", silent=no_key, val=this%tau_crd_coh, &
                             default=DEF_SEDIMENT_TAU_CRD_COH)
      call sub_env%yaml%read("E_coh", silent=no_key, val=this%e_coh, &
                             default=DEF_SEDIMENT_E_COH)
      call sub_env%yaml%read("alpha_coh", silent=no_key, val=this%alpha_coh, &
                             default=DEF_SEDIMENT_ALPHA_COH)
      call sub_env%yaml%read("a_coh", silent=no_key, val=this%a_coh, &
                             default=DEF_SEDIMENT_A_COH)
      call sub_env%yaml%read("b_coh", silent=no_key, val=this%b_coh, &
                             default=DEF_SEDIMENT_B_COH)
      call sub_env%yaml%read("n_coh", silent=no_key, val=this%n_coh, &
                             default=DEF_SEDIMENT_N_COH)
      call sub_env%yaml%read("m_coh", silent=no_key, val=this%m_coh, &
                             default=DEF_SEDIMENT_M_COH)
      call sub_env%yaml%read("k_coh", silent=no_key, val=this%k_coh, &
                             default=DEF_SEDIMENT_K_COH)

      ! presence-tested: the fallback is mud or sand depending on the above,
      ! which no flat registry default can express (NOTE 13)
      call sub_env%yaml%read("D50", silent=no_key, val=this%d50)
      if (no_key) then
         if (this%cohesive) then
            this%d50 = D50_MUD
         else
            this%d50 = D50_SAND
         end if
      end if

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

      ! ---- avalanching
      call sub_env%yaml%read("Avalanche", silent=no_key, val=this%avalanche, &
                             default=DEF_SEDIMENT_AVALANCHE)
      call sub_env%yaml%read("Tan_phi", silent=no_key, val=this%tan_phi, &
                             default=DEF_SEDIMENT_TAN_PHI)

      ! absent means "relax every step", so no default
      call sub_env%yaml%read("Aval_interval", silent=no_key, val=this%aval_interval)
      if (no_key) this%aval_interval = SMALL

      ! ---- feedback into the flow
      call sub_env%yaml%read("SedimentMassSource", silent=no_key, &
                             val=this%mass_source, &
                             default=DEF_SEDIMENT_SEDIMENTMASSSOURCE)
      call sub_env%yaml%read("SedimentMomentDC", silent=no_key, &
                             val=this%moment_dc, &
                             default=DEF_SEDIMENT_SEDIMENTMOMENTDC)
      call sub_env%yaml%read("SedimentMomentEXG", silent=no_key, &
                             val=this%moment_exg, &
                             default=DEF_SEDIMENT_SEDIMENTMOMENTEXG)

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
         allocate (this%dchg_s(m, n), source=ZERO)
         allocate (this%dchg_b(m, n), source=ZERO)
         allocate (this%zb_aval(m, n), source=ZERO)
         allocate (this%aval_accum(m, n), source=ZERO)

         allocate (this%mass_sed(m, n), source=ZERO)
         allocate (this%dc_x(m, n), source=ZERO)
         allocate (this%dc_y(m, n), source=ZERO)
         allocate (this%exg_x(m, n), source=ZERO)
         allocate (this%exg_y(m, n), source=ZERO)
         ! no hard bottom anywhere until a file says otherwise
         allocate (this%zs(m, n), source=LARGE)
      end associate

      this%depth_ini = depth

      ! legacy GetFile: interior only, so the ghosts keep their LARGE — which
      ! is what the (interior-only) clamp wants anyway
      if (this%hard_bottom) then
         call read_field_ascii(env, this%hard_bottom_file%root, grid, this%zs)
      end if

      ! legacy overloads k_coh as the molecular viscosity here.  It is a dead
      ! assignment: both consumers below are non-cohesive-only (NOTE 16)
      if (this%cohesive) then
         this%viscosity = this%k_coh
      else
         this%viscosity = NU_WATER
      end if

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
                              p_face, q_face, h, roller, undertow_u, undertow_v, &
                              prop_on, upc, up, vp)
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
      ! NOTE 22: propeller jet bed-shear coupling -- prop_on true only when the
      ! vessel is active with the propeller on, upc/up/vp its jet velocities
      logical, intent(in) :: prop_on
      real(SP), intent(in) :: upc(:, :), up(:, :), vp(:, :)

      if (.not. this%is_activated) return

      h = gamma3*eta + depth
      this%hpo = max(h, min_depth)

      call sediment_advect(this, grid%lp, mask, p_face, q_face, roller, &
                           undertow_u, undertow_v)
      call sediment_diffuse(this, grid%lp, inv_dx, inv_dy, mask, u, v, prop_on, upc)
      call sediment_flux_bc(this, grid, mask)
      call sediment_solve(this, grid%lp, alpha, beta, dt, inv_dx, inv_dy, mask)
      call bc%exchange_scalar(grid, this%ch)

      call sediment_pickup(this, grid%lp, mask, u, v, h, prop_on, upc, up, vp)
      ! the morphology's divergence reads i+-1 / j+-1, so the cell-centred
      ! bedload flux has to carry its ghosts
      if (this%bedload) then
         call bc%exchange_scalar(grid, this%bed_flux_x)
         call bc%exchange_scalar(grid, this%bed_flux_y)
      end if

      call sediment_deposit(this, grid%lp, mask)
      call sediment_average(this, dt)
      ! the DC gradient is centred, so the average has to carry its ghosts
      call bc%exchange_scalar(grid, this%c_ave)

      if (this%mass_source .or. this%moment_dc .or. this%moment_exg) then
         call sediment_sources(this, grid%lp, inv_dx, inv_dy, mask, u, v)
      end if

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
   subroutine sediment_diffuse(this, lp, inv_dx, inv_dy, mask, u, v, prop_on, upc)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in) :: inv_dx(:, :), inv_dy(:, :)
      integer, intent(in) :: mask(:, :)
      real(SP), intent(in) :: u(:, :), v(:, :)
      ! NOTE 22: the propeller jet's bed-shear velocity, added onto the log-law
      ! u_* at each cell (legacy PROPELLER && VESSEL).  prop_on gates the add so
      ! propeller-off is bitwise the pre-e2 path — upc is zeros then anyway
      logical, intent(in) :: prop_on
      real(SP), intent(in) :: upc(:, :)

      integer :: i, j
      real(SP) :: ustar_c2, ustar_c4, k2, k4

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie + 1
            if (mask(i, j) > 0) then
               this%ustar_c = shear_velocity(this, u(i, j), v(i, j), this%hpo(i, j))
               if (prop_on) this%ustar_c = this%ustar_c + upc(i, j)

               if (mask(i - 1, j) > 0) then
                  ustar_c2 = shear_velocity(this, u(i - 1, j), v(i - 1, j), &
                                            this%hpo(i - 1, j))
                  if (prop_on) ustar_c2 = ustar_c2 + upc(i - 1, j)
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
                  if (prop_on) ustar_c4 = ustar_c4 + upc(i, j - 1)
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
   !
   ! Cohesive replaces the pickup law with an excess-shear erosion rate — the
   ! SoftBed root form for an unconsolidated bed, the linear form otherwise:
   !   $$ P = E\,e^{\alpha\sqrt{|\tau - \tau_{cr}|}}, \qquad
   !      P = E\left(\frac{\tau}{\tau_{cr}} - 1\right) $$
   ! and takes the bedload with it (header NOTE 14).
   ! ----------------------------------------------------------------
   subroutine sediment_pickup(this, lp, mask, u, v, h, prop_on, upc, up, vp)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in) :: mask(:, :)
      real(SP), intent(in) :: u(:, :), v(:, :), h(:, :)
      ! NOTE 22: the propeller jet raises tau by upc^2 in the bed-shear, and
      ! swings the bedload direction onto the total (flow + jet) velocity
      logical, intent(in) :: prop_on
      real(SP), intent(in) :: upc(:, :), up(:, :), vp(:, :)

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
               if (prop_on) this%tau_xy(i, j) = this%tau_xy(i, j) + upc(i, j)**2.0_SP

               if (this%cohesive) then

                  ! NOTE 17: no Hpo re-test here, unlike the sand branch below
                  if (this%tau_xy(i, j) > this%tau_cr_coh) then
                     if (this%soft_bed) then
                        ! the abs() is legacy's and is dead — the branch above
                        ! already guarantees the difference is positive.  NOTE 18:
                        ! this steps to E_coh at the threshold, it does not ramp
                        this%pickup(i, j) = this%e_coh &
                                            *exp(this%alpha_coh &
                                                 *sqrt(abs(this%tau_xy(i, j) &
                                                           - this%tau_cr_coh)))
                     else
                        this%pickup(i, j) = this%e_coh &
                                            *(this%tau_xy(i, j) &
                                              /max(this%tau_cr_coh, SMALL) - 1.0_SP)
                     end if
                  else
                     this%pickup(i, j) = ZERO
                  end if

                  ! NOTE 14: no bedload branch — mud leaves bed_flux at the zero
                  ! the loop head set, whatever BedLoad says

               else

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
                        if (prop_on) then
                           angle_cur = atan2(v(i, j) + vp(i, j), u(i, j) + up(i, j))
                        else
                           angle_cur = atan2(v(i, j), u(i, j))
                        end if
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
   !
   ! Cohesive swaps the constant settling velocity for a flocculation curve in
   ! the near-bed mass concentration, and hinders on the shear rather than on
   ! the concentration:
   !   $$ w_s = \frac{a\,c_b^{\,n}}{(c_b^2 + b^2)^m}, \qquad c_b = c\,s\,\rho_w $$
   !   $$ D = w_s\,c\,\left(1 - \frac{\tau}{\tau_{cr,d}}\right) $$
   ! which goes negative above tau_crd and erodes instead (header NOTE 15).
   !
   ! Loop bounds are legacy's, one cell past the interior (header NOTE 2).
   ! ----------------------------------------------------------------
   subroutine sediment_deposit(this, lp, mask)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in) :: mask(:, :)

      integer :: i, j
      real(SP) :: gamma_cao, c_b, ws_floc, p_d

      do j = lp%jb, lp%je + 1
         do i = lp%ib, lp%ie + 1
            if (mask(i, j) > 0) then

               if (this%cohesive) then
                  ! legacy recomputes the module-wide WS scalar per cell here.
                  ! A local is bit-identical: the cohesive pickup never reads it
                  ! back, and this loop overwrites it before every use
                  c_b = this%ch(i, j)*this%sdensity*RHO_WATER
                  ws_floc = this%a_coh*c_b**this%n_coh &
                            /max(SMALL, (c_b**2 + this%b_coh**2)**this%m_coh)
                  p_d = 1.0_SP - this%tau_xy(i, j)/max(this%tau_crd_coh, SMALL)
                  this%depo(i, j) = ws_floc*this%ch(i, j)*p_d
               else
                  gamma_cao = min(CAO_GAMMA_MAX, &
                                  (1.0_SP - this%n_porosity)/max(SMALL, this%ch(i, j)))
                  this%depo(i, j) = gamma_cao*this%ch(i, j)*this%ws &
                                    *(1.0_SP - gamma_cao*this%ch(i, j))**2.0_SP
               end if

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
   ! The load's feedback into the flow's own residual, all three terms built
   ! together whenever any one of them is switched on (NOTE 21):
   !
   !   $$ S_{mass} = \frac{P - D}{1 - n}, \qquad
   !      \mathbf{S}^{DC} = -\frac{(s-1) g h_{po}^2}{c_{sc}} \nabla \bar{c},
   !      \qquad
   !      \mathbf{S}^{EXG} = -\frac{s_{nc}}{c_{sc}(1-n)}(P - D)\,\mathbf{u} $$
   !
   ! with $c_{sc} = 1 + \bar{c}(s-1)$ the mixture's specific gravity and
   ! $s_{nc} = (s-1)\max(1 - n - \bar{c}, 0)$ the room the bed has left.  The
   ! gradient is centred on the Morph_interval average (staircased, NOTE 19);
   ! P and D are this stage's.  Nothing is zeroed outside the wet cells
   ! (NOTE 20).
   ! ----------------------------------------------------------------
   subroutine sediment_sources(this, lp, inv_dx, inv_dy, mask, u, v)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in) :: inv_dx(:, :), inv_dy(:, :)
      integer, intent(in) :: mask(:, :)
      real(SP), intent(in) :: u(:, :), v(:, :)

      integer :: i, j
      real(SP) :: csc, snc, p_d, grad_x, grad_y

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            if (mask(i, j) > 0) then

               grad_x = (this%c_ave(i + 1, j) - this%c_ave(i - 1, j)) &
                        /2.0_SP*inv_dx(i, j)
               grad_y = (this%c_ave(i, j + 1) - this%c_ave(i, j - 1)) &
                        /2.0_SP*inv_dy(i, j)

               csc = 1.0_SP + this%c_ave(i, j)*(this%sdensity - 1.0_SP)
               ! the bed cannot give up more than it holds, so 1 - n - c floors
               ! at zero
               snc = (this%sdensity - 1.0_SP) &
                     *max(1.0_SP - this%n_porosity - this%c_ave(i, j), ZERO)

               p_d = this%pickup(i, j) - this%depo(i, j)

               this%dc_x(i, j) = -(this%sdensity - 1.0_SP)*GRAV &
                                 *this%hpo(i, j)**2/csc*grad_x
               this%dc_y(i, j) = -(this%sdensity - 1.0_SP)*GRAV &
                                 *this%hpo(i, j)**2/csc*grad_y

               this%exg_x(i, j) = -snc/csc/(1.0_SP - this%n_porosity) &
                                  *p_d*u(i, j)
               this%exg_y(i, j) = -snc/csc/(1.0_SP - this%n_porosity) &
                                  *p_d*v(i, j)

               this%mass_sed(i, j) = p_d/(1.0_SP - this%n_porosity)
            end if
         end do
      end do

   end subroutine sediment_sources

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
   subroutine sediment_morphology(this, bc, grid, dt, dx, dy, inv_dx, inv_dy, &
                                  depth, depth_x, depth_y)
      class(type_model_sediment), intent(inout) :: this
      type(type_model_bc), intent(in) :: bc
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: dt
      real(SP), intent(in) :: dx(:, :), dy(:, :)
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

               ! output-only: each load carries a 1/(1-n) copy (DchgS / DchgB),
               ! which legacy forms at write time from the same accumulators
               this%dchg_s(i, j) = this%susp_load(i, j)/(1.0_SP - this%n_porosity)
               this%dchg_b(i, j) = this%bed_load(i, j)/(1.0_SP - this%n_porosity)

               if (this%zb(i, j) > this%zs(i, j)) this%zb(i, j) = this%zs(i, j)
            end do
         end do

         call sediment_avalanche(this, lp, dt, dx, dy, depth)

         depth = this%depth_ini + this%zb*this%morph_factor
      end associate

      call bc%exchange_scalar(grid, depth)
      call stagger_depth(grid%lp, depth, depth_x, depth_y)

   end subroutine sediment_morphology

   ! ----------------------------------------------------------------
   ! Legacy avalanching, inside MORPHOLOGICAL_CHANGE between the bed-level
   ! integration and the depth rewrite.  Every Aval_interval, any cell whose
   ! steepest downhill neighbour exceeds the angle of repose slides half the
   ! excess into that neighbour:
   !
   !   $$ s_k = \frac{d_{i,j} - d_k}{\Delta}, \qquad
   !      k^\ast = \arg\max_k s_k, \qquad
   !      s_{k^\ast} > \tan\phi $$
   !   $$ \delta = \tfrac{1}{2}\left(d_{i,j} - d_{k^\ast}\right)
   !               - \tfrac{1}{2}\tan\phi\,\Delta, \qquad
   !      z_b \mathrel{-}= \delta \ \text{here}, \quad
   !      z_b \mathrel{+}= \delta \ \text{at } k^\ast $$
   !
   ! d is the water depth, so the steep cell is the DEEP one and the slide
   ! fills it.  Only that one neighbour is relaxed per cell per interval —
   ! legacy's own comment says this is what keeps the scan from chasing its
   ! tail (header NOTE 10 for what it costs).
   ! ----------------------------------------------------------------
   subroutine sediment_avalanche(this, lp, dt, dx, dy, depth)
      class(type_model_sediment), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in) :: dt
      real(SP), intent(in) :: dx(:, :), dy(:, :)
      real(SP), intent(in) :: depth(:, :)

      integer :: i, j, i4, i4_record
      real(SP) :: slope_max, dh, slope4(4)

      if (.not. this%avalanche) return

      this%t_aval = this%t_aval + dt
      if (this%t_aval < this%aval_interval) return
      this%t_aval = ZERO

      this%zb_aval = ZERO

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            ! divided, not inv_dx-multiplied: the comparison below is a
            ! threshold, so a 1-ulp drift off legacy could flip a cell outright
            slope4(1) = (depth(i, j) - depth(i - 1, j))/dx(i, j)
            slope4(2) = (depth(i, j) - depth(i + 1, j))/dx(i, j)
            slope4(3) = (depth(i, j) - depth(i, j - 1))/dy(i, j)
            slope4(4) = (depth(i, j) - depth(i, j + 1))/dy(i, j)

            ! downhill only, and only the steepest of the four: a cell that is
            ! the shallow side of every face is left alone
            slope_max = ZERO
            i4_record = 0
            do i4 = 1, 4
               if (slope4(i4) > slope_max) then
                  slope_max = slope4(i4)
                  i4_record = i4
               end if
            end do

            if (slope_max <= this%tan_phi) cycle

            ! the hard bottom blocks the slide into this cell, not out of it
            if (this%zb(i, j) >= this%zs(i, j)) cycle

            select case (i4_record)
            case (1)
               dh = 0.5_SP*(depth(i, j) - depth(i - 1, j)) &
                    - 0.5_SP*this%tan_phi*dx(i, j)
               this%zb_aval(i, j) = dh
               this%zb_aval(i - 1, j) = -dh
            case (2)
               dh = 0.5_SP*(depth(i, j) - depth(i + 1, j)) &
                    - 0.5_SP*this%tan_phi*dx(i, j)
               this%zb_aval(i, j) = dh
               this%zb_aval(i + 1, j) = -dh
            case (3)
               dh = 0.5_SP*(depth(i, j) - depth(i, j - 1)) &
                    - 0.5_SP*this%tan_phi*dy(i, j)
               this%zb_aval(i, j) = dh
               this%zb_aval(i, j - 1) = -dh
            case (4)
               dh = 0.5_SP*(depth(i, j) - depth(i, j + 1)) &
                    - 0.5_SP*this%tan_phi*dy(i, j)
               this%zb_aval(i, j) = dh
               this%zb_aval(i, j + 1) = -dh
            end select
         end do
      end do

      this%zb = this%zb - this%zb_aval
      this%aval_accum = this%aval_accum + this%zb_aval

   end subroutine sediment_avalanche

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
      if (allocated(this%dchg_s)) deallocate (this%dchg_s)
      if (allocated(this%dchg_b)) deallocate (this%dchg_b)
      if (allocated(this%zb_aval)) deallocate (this%zb_aval)
      if (allocated(this%aval_accum)) deallocate (this%aval_accum)
      if (allocated(this%mass_sed)) deallocate (this%mass_sed)
      if (allocated(this%dc_x)) deallocate (this%dc_x)
      if (allocated(this%dc_y)) deallocate (this%dc_y)
      if (allocated(this%exg_x)) deallocate (this%exg_x)
      if (allocated(this%exg_y)) deallocate (this%exg_y)

   end subroutine sediment_free

end module model_sediment_mod
