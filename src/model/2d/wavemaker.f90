!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Wavemaker parameters YAML reader (bridge)
!
!  Wavemaker taxonomy — two distinct integration points in the engine
!  (the one-shot t=0 types moved to model_initial_mod, initial: section):
!    * internal source types (WK_REG, WK_IRR, TMA_1D/JON_1D/JON_2D,
!      WK_TIME, WK_NEW_*): continuous generation — init_compute derives
!      the generation coefficients, update_source refreshes the mass
!      array each stage (WK_REG + WK_IRR/TMA/JON live; WK_TIME and
!      WK_NEW_* pending).
!    * boundary types (ABS, LEFT_BC_IRR, LEF_SOL): own the west ghost
!      strip each step — the BC service must skip the wall mirror there
!      (fill_west=.false. in kernel_bc).  ABS = a spectrum-only entry
!      referenced by boundaries.west.forcing.wavemaker (the face reader
!      resolves the name and fills the strip/depth fields; config reorg
!      rung 3b); LEFT_BC_IRR/ABS_1D are deprecated pending the
!      characteristic BC track and LEF_SOL keeps its legacy type: escape.
!
!  YAML block: wavemaker:       (mapping or 1-element sequence; omit for
!                                no wavemaker — config reorg rung 3a/3b shape)
!    name: <string>             reference target for a boundaries face
!    spectrum:                  (required)
!      type: regular | jonswap | tma | spectrum_2d | components
!      --- regular:      amplitude, period, direction (nee AMP_WK/Tperiod/
!                        Theta_WK)
!      --- jonswap/tma:  hm0, gamma, freq: {peak, min, max} XOR
!                        period: {peak, min, max} (reciprocal, non-bitwise)
!      --- spectrum_2d:  file, format (nee WaveCompFile/WAVE_DATA_TYPE)
!      --- components:   n, period_peak, file
!      directional: {peak, spread}    presence = 2D spreading; spread REQUIRED
!      discretization: {freq_bins, theta_bins, equal_energy, method,
!                       coherence_percent}    theta_bins needs directional:
!    source:                    presence = Wei-Kirby internal source box
!      x_center, y_center, depth, delta, y_width, time_ramp, current_cd
!    limiter: {crest, trough}   presence = eta limiter (nee ETA_LIMITER)
!
!  A spectrum-only entry (no source: block) is a boundary feed: legal only
!  when a boundaries face references it by name (jonswap/tma/spectrum_2d
!  spectra; nee ABS).  The relaxation strip and series depth live with the
!  face (boundaries.west.sponge + forcing.depth), not here.
!
!  This is a flat bridge module — a redesigned wavemaker module will
!  replace it once the wavemaker refactor is complete.
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_wavemaker_mod
   use core_constants_mod, only: SP, PI, DEG2RAD
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base
   use model_tide_mod, only: type_model_tide

   use model_config_defaults_mod, only: DEF_WAVEMAKER_SOURCE_DELTA, &
                                        DEF_WAVEMAKER_SOURCE_DEPTH, &
                                        DEF_WAVEMAKER_SOURCE_TIME_RAMP, &
                                        DEF_WAVEMAKER_SOURCE_X_CENTER, &
                                        DEF_WAVEMAKER_SOURCE_Y_CENTER, &
                                        DEF_WAVEMAKER_SOURCE_Y_WIDTH, &
                                        DEF_WAVEMAKER_SPECTRUM_AMPLITUDE, &
                                        DEF_WAVEMAKER_SPECTRUM_DIRECTION, &
                                        DEF_WAVEMAKER_SPECTRUM_DIRECTIONAL_PEAK, &
                                        DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_COHERENCE_PERCENT, &
                                        DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_EQUAL_ENERGY, &
                                        DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_FREQ_BINS, &
                                        DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_METHOD, &
                                        DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_THETA_BINS, &
                                        DEF_WAVEMAKER_SPECTRUM_FORMAT, &
                                        DEF_WAVEMAKER_SPECTRUM_FREQ_MAX, &
                                        DEF_WAVEMAKER_SPECTRUM_FREQ_MIN, &
                                        DEF_WAVEMAKER_SPECTRUM_FREQ_PEAK, &
                                        DEF_WAVEMAKER_SPECTRUM_GAMMA, &
                                        DEF_WAVEMAKER_SPECTRUM_HM0, &
                                        DEF_WAVEMAKER_SPECTRUM_NORMALIZE, &
                                        DEF_WAVEMAKER_SPECTRUM_N, &
                                        DEF_WAVEMAKER_SPECTRUM_PERIOD, &
                                        DEF_WAVEMAKER_SPECTRUM_PERIOD_PEAK

   implicit none

   private
   public :: type_model_wavemaker
   public :: read_wavemakers
   public :: wk_regular_coefficients

   ! Default wavemaker phase-RNG seed — matches the legacy WAVE_COHERENCE
   ! fixed-seed convention (a fixed, nonzero value keeps runs reproducible).
   integer, parameter :: DEFAULT_WAVE_PHASE_SEED = 66

   type, extends(type_model_base) :: type_model_wavemaker

      character(:), allocatable :: wavemaker_type   ! YAML key: type
      character(:), allocatable :: name              ! YAML key: name (face reference target)
      character(:), allocatable :: WaveCompFile      ! YAML key: WaveCompFile
      character(:), allocatable :: WAVE_DATA_TYPE    ! YAML key: WAVE_DATA_TYPE

      ! Spectrum-only entry awaiting a boundaries face reference; the face
      ! reader resolves it to type ABS (unresolved = init_compute error)
      logical  :: boundary_candidate = .false.

      ! Shared position / depth / ramp
      real(SP) :: Xc_WK = 0.0_SP
      real(SP) :: Yc_WK = 0.0_SP
      real(SP) :: DEP_WK = 0.0_SP
      real(SP) :: Time_ramp = 0.0_SP
      real(SP) :: Delta_WK = 0.5_SP
      real(SP) :: Ywidth_WK = 999999.0_SP   ! LARGE in old code

      ! Solitary wave lag — LEF_SOL (the IC solitary fields live in
      ! model_initial_mod)
      real(SP) :: LAG_SOLI = 0.0_SP   ! YAML key: LAGTIME

      ! Regular wave — WK_REG
      real(SP) :: Tperiod = 0.0_SP
      real(SP) :: AMP_WK = 0.0_SP
      real(SP) :: Theta_WK = 0.0_SP

      ! Multi-component time series — WK_TIME
      integer  :: NumWaveComp = 1
      real(SP) :: PeakPeriod = 0.0_SP

      ! Spectral — WK_IRR, TMA_1D, JON_1D, JON_2D, WK_NEW_IRR, WK_NEW_DATA2D
      real(SP) :: FreqPeak = 0.0_SP
      real(SP) :: FreqMin = 0.0_SP
      real(SP) :: FreqMax = 0.0_SP
      real(SP) :: Hmo = 0.0_SP
      real(SP) :: GammaTMA = 3.3_SP
      integer  :: Nfreq = 45
      real(SP) :: ThetaPeak = 0.0_SP
      integer  :: Ntheta = 1
      real(SP) :: Sigma_Theta = 0.0_SP
      ! single-dir discretization (nee WK_NEW_*): one direction per
      ! frequency component; alpha_c coherence rides on its equal-df
      ! ladder (regime rules enforced in read_input)
      logical  :: single_dir = .false.
      real(SP) :: alpha_c = 0.0_SP
      ! normalize: total — Hm0 refers to the full spectrum and the
      ! [min, max] band carries only its natural energy share; default
      ! band renormalizes the band to the full Hm0 (legacy)
      logical  :: normalize_total = .false.

      ! Zero every component phase instead of the seeded draw — parity/
      ! debug knob (replaces the retired ZERO_PHASE build flag, so one
      ! binary serves production and A/B parity decks)
      logical  :: zero_phase = .false.
      ! RNG seed for the random phase realization — see seed_wave_phases.
      ! Deterministic by default so runs are reproducible AND a hotstart
      ! restart reproduces the same realization (the seed rides the shared deck).
      integer  :: seed = DEFAULT_WAVE_PHASE_SEED

      ! Eta limiter (type-independent)
      logical  :: ETA_LIMITER = .false.
      real(SP) :: CrestLimit = 0.0_SP
      real(SP) :: TroughLimit = 0.0_SP

      ! Absorbing-generating — ABS, LEFT_BC_IRR
      real(SP) :: DepthWaveMaker = 0.0_SP   ! DepthWaveMaker / DEP_WK fallback → DEP_Ser
      real(SP) :: WidthWaveMaker = 0.0_SP
      real(SP) :: R_sponge_wavemaker = 0.0_SP
      real(SP) :: A_sponge_wavemaker = 0.0_SP
      logical  :: EqualEnergy = .false.

      ! Wavemaker current balance — presence of WaveMakerCd enables balance
      logical  :: WaveMakerCurrentBalance = .false.
      real(SP) :: WaveMakerCd = 0.0_SP
      real(SP), allocatable :: cd_current(:, :)   ! WaveMakerCd over the source box

      ! Internal-source machinery (init_compute products)
      logical  :: has_mass_source = .false.
      real(SP) :: D_gen = 0.0_SP      ! source magnitude
      real(SP) :: rlamda = 0.0_SP     ! along-crest wavenumber k sin(theta)
      real(SP) :: Beta_gen = 0.0_SP   ! Gaussian shape factor
      real(SP) :: Width_WK = 0.0_SP   ! source half-width delta*L/2
      ! Wavemaker-frame coordinates (legacy xmk_wk/ymk_wk): x = 0 at the
      ! first interior cell of the global domain; ghost entries follow
      ! the same line (legacy recomputes them inline in breaker.F)
      real(SP), allocatable :: xmk_wk(:), ymk_wk(:)
      integer  :: ilo = 1, ihi = 0, jlo = 1, jhi = 0  ! interior cells inside the source box
      real(SP), allocatable :: mass(:, :)             ! eta-equation source (legacy WaveMaker_Mass)

      ! Spectral internal source (WK_IRR / TMA_1D / JON_1D / JON_2D):
      ! dense precomputed spatial modes, legacy (Mloc,Nloc,Nfreq) layout
      logical  :: spectral_source = .false.
      real(SP), allocatable :: Cm(:, :, :), Sm(:, :, :)
      real(SP), allocatable :: omgn_ir(:)             ! component frequencies 2 pi f

      ! Multi-component time-series internal source (WK_TIME): per-
      ! component (period, amplitude, phase) from WaveCompFile plus the
      ! per-component generation coefficients (legacy D_genS/Beta_genS)
      logical  :: time_series_source = .false.
      real(SP), allocatable :: wave_comp(:, :)        ! (NumWaveComp, 3)
      real(SP), allocatable :: D_genS(:), Beta_genS(:)

      ! Boundary wavemaker (ABS / LEFT_BC_IRR): dense eta/u/v series
      ! modes (legacy Cm_eta..Sm_v), component frequencies + phases,
      ! and the ABS relaxation sponge
      logical  :: left_bc_source = .false.            ! LEFT_BC_IRR ghost-strip fill
      logical  :: abs_source = .false.                ! ABS relaxation
      real(SP), allocatable :: Cm_eta(:, :, :), Sm_eta(:, :, :)
      real(SP), allocatable :: Cm_u(:, :, :), Sm_u(:, :, :)
      real(SP), allocatable :: Cm_v(:, :, :), Sm_v(:, :, :)
      real(SP), allocatable :: Segma_Ser(:), Phase_Ser(:)
      real(SP), allocatable :: sponge_maker(:, :)

      ! Tidal GEN_ABS hook (legacy ABSORBING_GENERATING_BC reads
      ! TIDE_MODULE state); set by main before init_compute
      type(type_model_tide), pointer :: tide => null()

   contains
      procedure :: read_input => wavemaker_read_input
      procedure :: init_compute => wavemaker_init_compute
      procedure :: update_source => wavemaker_update_source
      procedure :: apply_boundary => wavemaker_apply_boundary
      procedure :: fill_in_zone => wavemaker_fill_in_zone
      procedure :: free => wavemaker_free
   end type type_model_wavemaker

   ! Flat per-component seam between the per-type discretization paths
   ! and the shared Wei & Kirby solve (spectrum-pipeline rung 2).  Each
   ! path fills omgn/Tperiod with its own legacy float sequence
   ! (2 pi f and 2 pi / T do not round-trip bitwise).
   type :: type_component_set
      integer :: n = 0
      real(SP), allocatable :: freq(:)      ! Hz
      real(SP), allocatable :: omgn(:)      ! rad/s
      real(SP), allocatable :: Tperiod(:)   ! s (per-period solve form only)
      real(SP), allocatable :: theta(:)     ! rad (solve may snap in place)
      real(SP), allocatable :: amp(:)       ! half-amplitude a
      real(SP), allocatable :: phase(:)     ! rad
      integer, allocatable :: slot(:)       ! Cm/Sm frequency slot
   end type type_component_set

   ! wk_solve_components periodic-y snap selector
   integer, parameter :: SNAP_NONE = 0      ! non-periodic or pre-snapped
   integer, parameter :: SNAP_SPECTRAL = 1  ! scratch-carrying nearest-mode rule
   integer, parameter :: SNAP_NEW = 2       ! decrement-retry rule (WK_NEW)

   ! ── init-time spectrum/spreading class layer (pipeline rung 4) ────
   ! Polymorphic dispatch at INIT only (slow path, kept alive for the
   ! future AM/WW3 slow-cadence updates); the runtime kernels stay flat
   ! arrays + integer dispatch per the GPU ground rules.

   ! 1-D frequency density S(f); fm anchors the total-energy scan and
   ! the single-dir peak lookup
   type, abstract :: type_freq_spectrum
      real(SP) :: fm = 0.0_SP   ! peak frequency [Hz]
   contains
      procedure(spectrum_density_i), deferred :: density
   end type type_freq_spectrum

   ! JONSWAP density — the shared TMA kernel with depth factor phi = 1
   type, extends(type_freq_spectrum) :: type_spectrum_jonswap
      real(SP) :: h_gen = 0.0_SP        ! generation depth [m]
      real(SP) :: gamma_spec = 3.3_SP
   contains
      procedure :: density => jonswap_density
   end type type_spectrum_jonswap

   ! TMA = JONSWAP x the Kitaigorodskii depth factor, so it extends the
   ! JONSWAP fields and overrides only the density
   type, extends(type_spectrum_jonswap) :: type_spectrum_tma
   contains
      procedure :: density => tma_spectrum_density
   end type type_spectrum_tma

   ! directional spreading weight D(theta; f); f rides the interface for
   ! the future frequency-dependent models (WW3 dspr(f)); every model
   ! carries the mean/peak direction the discretizers center their bins on
   type, abstract :: type_dir_spreading
      real(SP) :: theta_peak = 0.0_SP   ! rad
   contains
      procedure(spreading_weight_i), deferred :: weight
   end type type_dir_spreading

   ! wrapped normal (Borgman 1984) — truncated cosine series
   type, extends(type_dir_spreading) :: type_spreading_wrapped_normal
      real(SP) :: sigma = 0.0_SP        ! rad
      integer  :: n_series = 0          ! int(20/sigma) truncation
   contains
      procedure :: weight => wrapped_normal_weight
   end type type_spreading_wrapped_normal

   ! discretizer — how a continuous density becomes N components.  Plain
   ! data: the builder branches on it, no dispatch
   type :: type_discretizer
      integer  :: nfreq = 0
      integer  :: ntheta = 1
      real(SP) :: fmin = 0.0_SP, fmax = 0.0_SP   ! generation band [Hz]
      logical  :: equal_energy = .false.         ! equal-df ladder otherwise
      ! one direction per frequency component (nee WK_NEW_*) instead of
      ! the (nfreq x ntheta) directional grid
      logical  :: single_dir_per_freq = .false.
      real(SP) :: alpha_c = 0.0_SP               ! coherence percent (single-dir only)
      logical  :: zero_phase = .false.           ! phase policy: zero | seeded draw
      integer  :: snap_mode = SNAP_NONE          ! periodic-y angle snap variant
   end type type_discretizer

   abstract interface
      function spectrum_density_i(this, f) result(s)
         import :: type_freq_spectrum, SP
         class(type_freq_spectrum), intent(in) :: this
         real(SP), intent(in) :: f
         real(SP) :: s
      end function spectrum_density_i

      function spreading_weight_i(this, theta, f) result(w)
         import :: type_dir_spreading, SP
         class(type_dir_spreading), intent(in) :: this
         real(SP), intent(in) :: theta, f
         real(SP) :: w
      end function spreading_weight_i
   end interface

contains

   subroutine component_set_alloc(cs, n)
      type(type_component_set), intent(inout) :: cs
      integer, intent(in) :: n

      cs%n = n
      allocate (cs%freq(n), cs%omgn(n), cs%Tperiod(n), cs%theta(n), &
                cs%amp(n), cs%phase(n), cs%slot(n))
   end subroutine component_set_alloc

   ! ----------------------------------------------------------------
   ! THE per-component Wei & Kirby source solve — every internal-source
   ! path funnels its component set through here.  With $\alpha =
   ! -0.39$, $\alpha_1 = \alpha + 1/3$: wavenumber from the Nwogu
   ! dispersion relation
   !   $$ (kh)^2 = \frac{t_c - \sqrt{t_c^2 - 4\alpha_1 t_b}}{2\alpha_1},
   !      \qquad t_b = \frac{\omega^2 h}{g},\ \ t_c = 1 + \alpha\,t_b $$
   ! then, with wavelength $L$ and source width parameter $\delta$:
   !   $$ \lambda = k\sin\theta, \qquad
   !      \beta = \frac{80}{\delta^2 L^2}, \qquad
   !      I = \sqrt{\pi/\beta}\;e^{-l^2/4\beta}, \qquad l = k\cos\theta $$
   !   $$ D = \frac{2 a \cos\theta\,(\omega^2 - \alpha_1 g k^4 h^3)}
   !               {\omega k I \left(1 - \alpha (kh)^2\right)} $$
   ! Emits D/lambda/beta per component; the source width comes from
   ! wk_peak_width — one peak-based width for every path (the legacy
   ! last-component width was ledger A7c, fixed at rung 3).
   ! Legacy float sequences are selected per path, not unified:
   !  * use_peak_cphase — wavelength through the peak frequency with the
   !    legacy wkn = 0 guard (analytic-spectrum family) vs through the
   !    component period with the legacy in-loop depth/period error stop
   !  * snap_mode — periodic-y angle snap INSIDE the loop where legacy
   !    does it (SNAP_SPECTRAL carries the theta = 0 reuse scratch;
   !    SNAP_NEW logs under snap_label); the DATA2D family snaps in a
   !    pre-pass and passes SNAP_NONE
   ! ----------------------------------------------------------------
   subroutine wk_solve_components(cs, h_gen, delta, fm, use_peak_cphase, &
                                  D_gen, rlamda, beta_gen, snap_mode, &
                                  dy, nglob, env, snap_label)
      use core_constants_mod, only: GRAV, SMALL
      type(type_component_set), intent(inout) :: cs
      real(SP), intent(in) :: h_gen, delta, fm
      logical, intent(in) :: use_peak_cphase
      real(SP), intent(out) :: D_gen(:), rlamda(:), beta_gen(:)
      integer, intent(in), optional :: snap_mode, nglob
      real(SP), intent(in), optional :: dy
      type(type_env), intent(inout), optional :: env
      character(*), intent(in), optional :: snap_label

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: alpha1, omgn, tb, tc, wkn, c_phase, wave_length
      real(SP) :: rl_gen, ri, snap_scratch, snapped
      character(96) :: msg
      integer :: c, snap

      snap = SNAP_NONE
      if (present(snap_mode)) snap = snap_mode

      alpha1 = alpha + 1.0_SP/3.0_SP
      ! legacy PARAM scratch is static: a theta = 0 component under
      ! periodic reuses the previous component's snapped value
      snap_scratch = 0.0_SP

      do c = 1, cs%n
         omgn = cs%omgn(c)

         if (.not. use_peak_cphase) then
            if (h_gen == 0.0_SP .or. cs%Tperiod(c) == 0.0_SP) &
               error stop "wavemaker: re-set depth, Tperiod for wavemaker"
         end if

         tb = omgn*omgn*h_gen/GRAV
         tc = 1.0_SP + tb*alpha
         wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                    /(2.0_SP*alpha1))/h_gen

         if (use_peak_cphase) then
            ! beta from the peak-frequency phase speed; wkn = 0 guard
            ! kept from legacy (02/08/2012 fix)
            if (wkn == 0.0_SP) then
               wkn = SMALL
               c_phase = sqrt(GRAV*h_gen)
               wave_length = c_phase/fm
            else
               c_phase = 1.0_SP/wkn*fm*2.0_SP*PI
               wave_length = c_phase/fm
            end if
         else
            c_phase = 1.0_SP/wkn/cs%Tperiod(c)*2.0_SP*PI
            wave_length = c_phase*cs%Tperiod(c)
         end if

         select case (snap)
         case (SNAP_SPECTRAL)
            call spectral_periodic_snap(cs%theta(c), snap_scratch, wkn, dy, &
                                        nglob, cs%freq(c), env)
         case (SNAP_NEW)
            call wk_new_periodic_snap(cs%theta(c), wkn, dy, nglob, snapped)
            write (msg, '(A,F8.3,A,F8.3,A,F8.3)') &
               snap_label//" periodic, freq: ", cs%freq(c), ", dir: ", &
               cs%theta(c)*180.0_SP/PI, " -> ", snapped*180.0_SP/PI
            call env%log%info(trim(msg))
            cs%theta(c) = snapped
         end select

         rlamda(c) = wkn*sin(cs%theta(c))
         beta_gen(c) = 80.0_SP/delta**2/wave_length**2
         rl_gen = wkn*cos(cs%theta(c))
         ri = sqrt(PI/beta_gen(c))*exp(-rl_gen**2/4.0_SP/beta_gen(c))

         D_gen(c) = 2.0_SP*cs%amp(c)*cos(cs%theta(c)) &
                    *(omgn**2 - alpha1*GRAV*wkn**4*h_gen**3) &
                    /(omgn*wkn*ri*(1.0_SP - alpha*(wkn*h_gen)**2))
      end do

   end subroutine wk_solve_components

   ! ----------------------------------------------------------------
   ! Private: dense modes from a solved component set (legacy
   ! CALCULATE_Cm_Sm / CALCULATE_NEW_Cm_Sm collapsed), ghost-inclusive:
   !   $$ C_m(x,y) = \sum_c D_c\,e^{-\beta_c(x - x_c)^2}
   !                 \cos\!\big(\lambda_c y + \phi_c\big), \qquad
   !      S_m(x,y) = \sum_c D_c\,e^{-\beta_c(x - x_c)^2}
   !                 \sin\!\big(\lambda_c y + \phi_c\big) $$
   ! Components accumulate into their slot in flat order; for the grid paths the
   ! slot is the frequency row and the flat order is theta-fastest, so
   ! each cell sees the same ktheta add sequence as the legacy
   ! kf/j/i/ktheta nest (bitwise-identical sums).  Merged WK_NEW
   ! slots: duplicates stay zero and contribute exact zeros to the
   ! stage sum, so update_source keeps its full-kf loop.
   ! ----------------------------------------------------------------
   subroutine calc_cm_sm(this, cs, D_gen, beta_gen, rlamda)
      class(type_model_wavemaker), intent(inout) :: this
      type(type_component_set), intent(in) :: cs
      real(SP), intent(in) :: D_gen(:), beta_gen(:), rlamda(:)

      integer :: i, j, c, kt

      this%Cm = 0.0_SP
      this%Sm = 0.0_SP
      do c = 1, cs%n
         kt = cs%slot(c)
         do j = 1, size(this%Cm, 2)
            do i = 1, size(this%Cm, 1)
               this%Cm(i, j, kt) = this%Cm(i, j, kt) + D_gen(c) &
                                   *exp(-beta_gen(c)*(this%xmk_wk(i) - this%Xc_WK)**2) &
                                   *cos(rlamda(c)*this%ymk_wk(j) + cs%phase(c))
               this%Sm(i, j, kt) = this%Sm(i, j, kt) + D_gen(c) &
                                   *exp(-beta_gen(c)*(this%xmk_wk(i) - this%Xc_WK)**2) &
                                   *sin(rlamda(c)*this%ymk_wk(j) + cs%phase(c))
            end do
         end do
      end do

   end subroutine calc_cm_sm

   ! ----------------------------------------------------------------
   ! Private: WK_NEW slot map (legacy CALCULATE_NEW_Cm_Sm head).
   ! Components sharing an ADJACENT equal frequency merge into the
   ! first slot of the run; non-adjacent duplicates are NOT merged
   ! (legacy compares neighbors only).
   ! ----------------------------------------------------------------
   subroutine wk_merge_slots(cs, env)
      type(type_component_set), intent(inout) :: cs
      type(type_env), intent(inout) :: env

      integer :: kf, ndistinct
      character(64) :: msg

      cs%slot(1) = 1
      ndistinct = 1
      do kf = 2, cs%n
         if (cs%freq(kf) /= cs%freq(kf - 1)) then
            ndistinct = ndistinct + 1
            cs%slot(kf) = kf
         else
            cs%slot(kf) = cs%slot(kf - 1)
         end if
      end do
      write (msg, '(A,I0)') "number of distinct freqs: ", ndistinct
      call env%log%info(trim(msg))
   end subroutine wk_merge_slots

   ! ----------------------------------------------------------------
   ! Private: source width $W = \delta L_p/2$ from the peak period —
   ! THE width for every internal-source path (legacy computed it from
   ! whatever component the solve loop ended on; ledger A7c).
   ! ----------------------------------------------------------------
   subroutine wk_peak_width(peak_period, h_gen, delta, width)
      use core_constants_mod, only: GRAV
      real(SP), intent(in) :: peak_period, h_gen, delta
      real(SP), intent(out) :: width

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: alpha1, omgn, tb, tc, wkn, c_phase, wave_length

      alpha1 = alpha + 1.0_SP/3.0_SP
      omgn = 2.0_SP*PI/peak_period
      tb = omgn*omgn*h_gen/GRAV
      tc = 1.0_SP + tb*alpha
      wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb))/(2.0_SP*alpha1))/h_gen
      c_phase = 1.0_SP/wkn/peak_period*2.0_SP*PI
      wave_length = c_phase*peak_period
      width = delta*wave_length/2.0_SP
   end subroutine wk_peak_width

   ! ----------------------------------------------------------------
   ! model_base contract — reads the FIRST wavemaker: entry only.  The
   ! engine owns an array and calls read_wavemakers instead; this stays
   ! for single-instance users (unit tests, legacy call shape).
   ! ----------------------------------------------------------------
   subroutine wavemaker_read_input(this, env)
      use core_yaml_file_mod, only: type_yaml_reader
      class(type_model_wavemaker), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_yaml_reader), allocatable :: entries(:)
      logical :: no_wm

      this%wavemaker_type = "nothing"
      entries = env%yaml%cast_dictionary_list("wavemaker", no_wm)
      this%is_activated = .not. no_wm
      if (no_wm) return

      if (size(entries) > 1) &
         call env%log%exit_on_error("wavemaker: the single-entry reader got"// &
                                    " a multi-entry deck (engine bug — use read_wavemakers)")
      call wavemaker_read_entry(this, env, entries(1), 1)

   end subroutine wavemaker_read_input

   ! ----------------------------------------------------------------
   ! Engine reader: every wavemaker: entry into an array (mapping = one
   ! entry, sequence = many).  Init gates that fall out of composition
   ! not being built yet: at most ONE internal source (boundary-feed
   ! entries are per-face, any number); names must be unique (faces
   ! resolve by name).
   ! ----------------------------------------------------------------
   subroutine read_wavemakers(env, wms)
      use core_yaml_file_mod, only: type_yaml_reader
      type(type_env), intent(inout), target :: env
      type(type_model_wavemaker), allocatable, intent(out) :: wms(:)

      type(type_yaml_reader), allocatable :: entries(:)
      logical :: no_wm
      integer :: k, kk, n_internal

      entries = env%yaml%cast_dictionary_list("wavemaker", no_wm)
      if (no_wm) then
         allocate (wms(0))
         return
      end if

      allocate (wms(size(entries)))
      n_internal = 0
      do k = 1, size(wms)
         wms(k)%wavemaker_type = "nothing"
         wms(k)%is_activated = .true.
         call wavemaker_read_entry(wms(k), env, entries(k), k)
         if (.not. wms(k)%boundary_candidate .and. &
             wms(k)%wavemaker_type /= "LEF_SOL") n_internal = n_internal + 1
      end do

      if (n_internal > 1) &
         call env%log%exit_on_error("wavemaker: multiple internal-source"// &
                                    " entries are pending source composition (one source:"// &
                                    " block max; boundary-feed entries are unlimited)")
      do k = 2, size(wms)
         do kk = 1, k - 1
            if (len(wms(k)%name) > 0 .and. wms(k)%name == wms(kk)%name) &
               call env%log%exit_on_error("wavemaker: duplicate entry name '"// &
                                          wms(k)%name//"' (faces resolve by name)")
         end do
      end do

   end subroutine read_wavemakers

   subroutine wavemaker_read_entry(this, env, wm, idx)
      use core_yaml_file_mod, only: type_yaml_reader
      class(type_model_wavemaker), intent(inout) :: this
      type(type_env), intent(inout), target :: env
      type(type_yaml_reader), intent(inout) :: wm
      integer, intent(in) :: idx

      type(type_yaml_reader) :: spec_yaml, blk
      character(:), allocatable :: stype, method, legacy_type, normalize
      character(8) :: def_bins
      logical :: no_key, has_dir
      logical :: no_spec, no_blk, no_freq, no_per
      real(SP) :: p_tmp

      call wm%read_string("name", silent=no_key, val=this%name)
      if (no_key) this%name = ""

      ! phase-RNG seed (rides the shared deck -> reproducible + restart-
      ! coherent); the default derives from the entry index so multiple
      ! entries never share a realization — entry 1 keeps the plain deck
      ! default (bitwise with the single-slot era)
      this%seed = DEFAULT_WAVE_PHASE_SEED + (idx - 1)
      call wm%read("seed", silent=no_key, val=this%seed)
      call wm%read("zero_phase", silent=no_key, val=this%zero_phase)

      ! legacy-shaped escape hatch: LEF_SOL keeps its old spelling until
      ! the characteristic BC track; the rest reject loudly
      call wm%read_string("type", silent=no_key, val=legacy_type)
      if (.not. no_key) then
         select case (trim(legacy_type))
         case ("LEF_SOL")
            this%wavemaker_type = "LEF_SOL"
            call wm%read("LAGTIME", silent=no_key, val=this%LAG_SOLI, default="0.0")
            return
         case ("ABS")
            call env%log%exit_on_error("wavemaker/type: schema renamed — ABS is a"// &
                                       " spectrum-only entry referenced by boundaries/west/"// &
                                       "forcing/wavemaker (registry has the mapping)")
         case ("ABS_1D", "LEFT_BC_IRR")
            call env%log%exit_on_error("wavemaker/type: "//trim(legacy_type)// &
                                       " is deprecated — pending the characteristic BC track")
         case default
            call env%log%exit_on_error("wavemaker/type: schema renamed — use"// &
                                       " spectrum:/source:/limiter: blocks (registry has the mapping)")
         end select
      end if

      ! ── spectrum ──────────────────────────────────────────────────
      spec_yaml = wm%cast_dictionary("spectrum", no_spec)
      if (no_spec) call env%log%exit_on_error("wavemaker: needs a spectrum: block")
      call spec_yaml%read_enum("type", [character(11) :: "regular", "jonswap", &
                                        "tma", "spectrum_2d", "components"], val=stype)

      ! directional: presence = 2D spreading (kills Ntheta/Sigma_Theta
      ! leaking into 1D configs).  spread is required -- the block's presence
      ! means spreading is wanted; a silent 0 (or legacy's hidden 10) is the
      ! degenerate-block trap
      blk = spec_yaml%cast_dictionary("directional", no_blk)
      has_dir = .not. no_blk
      if (has_dir) then
         call blk%read("peak", silent=no_key, val=this%ThetaPeak, &
                       default=DEF_WAVEMAKER_SPECTRUM_DIRECTIONAL_PEAK)
         call blk%read("spread", val=this%Sigma_Theta)
         call blk%read("n_bins", silent=no_key, val=this%Ntheta)
         if (.not. no_key) call env%log%exit_on_error( &
            "wavemaker/directional: n_bins moved -- set discretization: theta_bins")
      end if

      blk = spec_yaml%cast_dictionary("discretization", no_blk)
      if (.not. no_blk) then
         call blk%read("freq_bins", silent=no_key, val=this%Nfreq, &
                       default=DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_FREQ_BINS)
         ! theta_bins is the directional-axis resolution: defaulted only
         ! when directional: is present, meaningless without it
         call blk%read("theta_bins", silent=no_key, val=this%Ntheta, &
                       default=DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_THETA_BINS)
         if (.not. no_key .and. .not. has_dir) call env%log%exit_on_error( &
            "wavemaker/discretization: theta_bins requires a directional: block")
         call blk%read("equal_energy", val=this%EqualEnergy, &
                       default=DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_EQUAL_ENERGY)
         call blk%read_enum("method", [character(19) :: "grid", "single_dir_per_freq"], &
                            val=method, default=DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_METHOD)
         call blk%read("coherence_percent", silent=no_key, val=this%alpha_c, &
                       default=DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_COHERENCE_PERCENT)
         this%single_dir = method == "single_dir_per_freq"
         ! regime rules: coherence hosts live on the single-dir equal-df ladder
         if (this%alpha_c /= 0.0_SP .and. .not. this%single_dir) &
            call env%log%exit_on_error("wavemaker/discretization: coherence_percent"// &
                                       " requires method: single_dir_per_freq")
         if (this%single_dir .and. this%EqualEnergy) &
            call env%log%exit_on_error("wavemaker/discretization: single_dir_per_freq"// &
                                       " uses the equal-df ladder — equal_energy does not apply")
      else if (has_dir) then
         ! no discretization: block -- directional resolution falls to default
         def_bins = DEF_WAVEMAKER_SPECTRUM_DISCRETIZATION_THETA_BINS
         read (def_bins, *) this%Ntheta
      end if
      if (.not. has_dir) this%Ntheta = 1

      select case (stype)
      case ("regular")
         if (has_dir) call env%log%exit_on_error( &
            "wavemaker/spectrum: regular has no directional: block")
         call spec_yaml%read("amplitude", silent=no_key, val=this%AMP_WK, &
                             default=DEF_WAVEMAKER_SPECTRUM_AMPLITUDE)
         call spec_yaml%read("period", silent=no_key, val=this%Tperiod, &
                             default=DEF_WAVEMAKER_SPECTRUM_PERIOD)
         call spec_yaml%read("direction", silent=no_key, val=this%Theta_WK, &
                             default=DEF_WAVEMAKER_SPECTRUM_DIRECTION)
         this%wavemaker_type = "WK_REG"

      case ("jonswap", "tma")
         call spec_yaml%read("hm0", silent=no_key, val=this%Hmo, &
                             default=DEF_WAVEMAKER_SPECTRUM_HM0)
         call spec_yaml%read("gamma", silent=no_key, val=this%GammaTMA, &
                             default=DEF_WAVEMAKER_SPECTRUM_GAMMA)
         call spec_yaml%read_enum("normalize", [character(5) :: "band", "total"], &
                                  val=normalize, &
                                  default=DEF_WAVEMAKER_SPECTRUM_NORMALIZE)
         this%normalize_total = normalize == "total"
         ! freq {peak,min,max} = exact legacy path; period {peak,min,max} =
         ! hand-authoring alternative (reciprocal fill — 1/(1/x) is NOT
         ! bitwise x, acceptable off the legacy-parity path by design)
         blk = spec_yaml%cast_dictionary("freq", no_freq)
         if (.not. no_freq) then
            call blk%read("peak", silent=no_key, val=this%FreqPeak, &
                          default=DEF_WAVEMAKER_SPECTRUM_FREQ_PEAK)
            call blk%read("min", silent=no_key, val=this%FreqMin, &
                          default=DEF_WAVEMAKER_SPECTRUM_FREQ_MIN)
            call blk%read("max", silent=no_key, val=this%FreqMax, &
                          default=DEF_WAVEMAKER_SPECTRUM_FREQ_MAX)
         end if
         blk = spec_yaml%cast_dictionary("period", no_per)
         if (.not. no_freq .and. .not. no_per) &
            call env%log%exit_on_error("wavemaker/spectrum: freq: and period:"// &
                                       " are mutually exclusive")
         if (no_freq .and. no_per) &
            call env%log%exit_on_error("wavemaker/spectrum: "//stype// &
                                       " needs freq: {peak, min, max} or period: {peak, min, max}")
         if (.not. no_per) then
            ! period min <-> freq MAX (and vice versa)
            call blk%read("peak", val=p_tmp)
            this%FreqPeak = 1.0_SP/p_tmp
            call blk%read("min", val=p_tmp)
            this%FreqMax = 1.0_SP/p_tmp
            call blk%read("max", val=p_tmp)
            this%FreqMin = 1.0_SP/p_tmp
         end if
         if (stype == "tma") then
            this%wavemaker_type = merge("WK_IRR", "TMA_1D", has_dir)
         else
            this%wavemaker_type = merge("JON_2D", "JON_1D", has_dir)
         end if

      case ("components")
         call spec_yaml%read("n", silent=no_key, val=this%NumWaveComp, &
                             default=DEF_WAVEMAKER_SPECTRUM_N)
         call spec_yaml%read("period_peak", silent=no_key, val=this%PeakPeriod, &
                             default=DEF_WAVEMAKER_SPECTRUM_PERIOD_PEAK)
         call spec_yaml%read("file", silent=no_key, val=this%WaveCompFile)
         if (no_key) call env%log%exit_on_error( &
            "wavemaker/spectrum: components needs a file: (wave-component data)")
         this%wavemaker_type = "WK_TIME"

      case ("spectrum_2d")
         call spec_yaml%read("file", silent=no_key, val=this%WaveCompFile)
         if (no_key) call env%log%exit_on_error( &
            "wavemaker/spectrum: spectrum_2d needs a file: (2D-spectrum data)")
         call spec_yaml%read("format", val=this%WAVE_DATA_TYPE, &
                             default=DEF_WAVEMAKER_SPECTRUM_FORMAT)
         this%wavemaker_type = "WK_DATA2D"
      end select

      if (this%single_dir .and. stype /= "jonswap" .and. stype /= "tma") &
         call env%log%exit_on_error("wavemaker/discretization: single_dir_per_freq"// &
                                    " needs a density spectrum (jonswap or tma)")

      ! ── source — presence = Wei-Kirby internal source function;
      !    absence = boundary feed (nee ABS): a boundaries face must
      !    reference the entry by name, which resolves the type ────────
      blk = wm%cast_dictionary("source", no_blk)
      if (no_blk) then
         if (this%single_dir) call env%log%exit_on_error( &
            "wavemaker/discretization: single_dir_per_freq is internal-source"// &
            " only (the boundary series modes are grid-based)")
         ! legacy keys the JONSWAP/DATA switch + directionality off
         ! WAVE_DATA_TYPE (io.F ABS block); spectrum_2d read its format
         ! into WAVE_DATA_TYPE above
         select case (stype)
         case ("jonswap")
            this%WAVE_DATA_TYPE = merge("JON_2D", "JON_1D", has_dir)
         case ("tma")
            this%WAVE_DATA_TYPE = merge("TMA_2D", "TMA_1D", has_dir)
         case ("spectrum_2d")
            continue
         case default
            call env%log%exit_on_error("wavemaker: a "//stype//" spectrum cannot"// &
                                       " feed a boundary — needs a source: block")
         end select
         this%boundary_candidate = .true.
         this%wavemaker_type = "PENDING_BOUNDARY"
      else
         call blk%read("x_center", silent=no_key, val=this%Xc_WK, &
                       default=DEF_WAVEMAKER_SOURCE_X_CENTER)
         call blk%read("y_center", silent=no_key, val=this%Yc_WK, &
                       default=DEF_WAVEMAKER_SOURCE_Y_CENTER)
         call blk%read("depth", silent=no_key, val=this%DEP_WK, &
                       default=DEF_WAVEMAKER_SOURCE_DEPTH)
         call blk%read("delta", silent=no_key, val=this%Delta_WK, &
                       default=DEF_WAVEMAKER_SOURCE_DELTA)
         call blk%read("y_width", silent=no_key, val=this%Ywidth_WK, &
                       default=DEF_WAVEMAKER_SOURCE_Y_WIDTH)
         call blk%read("time_ramp", silent=no_key, val=this%Time_ramp, &
                       default=DEF_WAVEMAKER_SOURCE_TIME_RAMP)
         ! current_cd presence enables the current-balance drag
         call blk%read("current_cd", silent=no_key, val=this%WaveMakerCd)
         this%WaveMakerCurrentBalance = .not. no_key
         this%DepthWaveMaker = this%DEP_WK
      end if

      ! ── limiter — presence = eta limiter (nee ETA_LIMITER) ────────
      blk = wm%cast_dictionary("limiter", no_blk)
      this%ETA_LIMITER = .not. no_blk
      if (this%ETA_LIMITER) then
         call blk%read("crest", val=this%CrestLimit)
         call blk%read("trough", val=this%TroughLimit)
      end if

   end subroutine wavemaker_read_entry

   ! ----------------------------------------------------------------
   ! Internal-source wavemaker setup (legacy WAVEMAKER_INITIALIZATION,
   ! old/wavemaker.F + the xmk_wk/zone block of old/init.F).  WK_REG
   ! and the spectral family (WK_IRR/TMA_1D/JON_1D/JON_2D); remaining
   ! source types join at their 6d rungs.  No-op when no wavemaker: block
   ! is present.
   !
   ! Under periodic-y the wave angle must fit an integer number of
   ! along-crest wavelengths in the domain: snap $\theta$ to the
   ! nearest admissible $\sin\theta = m\,\frac{2\pi}{k\,\Delta y\,(N_{glob}-1)}$
   ! (legacy loop, kept verbatim including the $N_{glob}-1$ measure).
   ! The spectral family snaps per component inside the coefficient
   ! loop instead — different legacy algorithm, kept separate.
   ! ----------------------------------------------------------------
   subroutine wavemaker_init_compute(this, grid, periodic, env, beta_ref)
      use core_grid_mod, only: type_grid_2d
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env
      real(SP), intent(in) :: beta_ref

      integer :: i, j, mloc, nloc

      ! spectrum-only entry nobody claimed (boundaries reader resolves the
      ! reference to ABS) — a silent no-op here would drop the wavemaker
      if (this%wavemaker_type == "PENDING_BOUNDARY") &
         call env%log%exit_on_error("wavemaker: spectrum-only entry '"//this%name// &
                                    "' is not referenced by a boundaries face forcing/wavemaker")

      select case (this%wavemaker_type)
      case ("WK_REG")
         this%has_mass_source = .true.
      case ("WK_IRR", "TMA_1D", "JON_1D", "JON_2D", &
            "WK_DATA2D", "WK_NEW_DATA2D", "WK_NEW_IRR")
         this%has_mass_source = .true.
         this%spectral_source = .true.
      case ("WK_TIME")
         this%has_mass_source = .true.
         this%time_series_source = .true.
      case ("ABS")
         this%abs_source = .true.
      case ("LEFT_BC_IRR")
         this%left_bc_source = .true.
      case default
         return
      end select

      ! Seed the phase RNG once, before any coefficient routine draws phases.
      ! gfortran's default RANDOM_NUMBER randomizes per process (per run AND per
      ! rank), which desyncs the phase realization across ranks and breaks
      ! hotstart restart coherence; a fixed, rank-uniform seed fixes both.
      if (this%spectral_source .or. this%abs_source .or. this%left_bc_source) &
         call seed_wave_phases(this%seed)

      ! legacy uses the scalar spacing (DXg) throughout the wavemaker
      if (grid%dx0 <= 0.0_SP .or. grid%dy0 <= 0.0_SP) &
         error stop "wavemaker: internal source requires uniform grid spacing"

      mloc = grid%lp%mloc
      nloc = grid%lp%nloc
      allocate (this%xmk_wk(mloc), this%ymk_wk(nloc))
      ! two-term legacy form (I-Ibeg)*DXg + (iista-1)*DXg kept for parity
      do i = 1, mloc
         this%xmk_wk(i) = real(i - grid%lp%ib, SP)*grid%dx0 &
                          + real(grid%ibegin - 1, SP)*grid%dx0
      end do
      do j = 1, nloc
         this%ymk_wk(j) = real(j - grid%lp%jb, SP)*grid%dy0 &
                          + real(grid%jbegin - 1, SP)*grid%dy0
      end do

      ! boundary wavemakers build the series modes only — no mass
      ! source, zone box, or breaker zone
      if (this%abs_source .or. this%left_bc_source) then
         call boundary_init_compute(this, grid, periodic, env, beta_ref)
         return
      end if

      if (this%spectral_source) then
         select case (this%wavemaker_type)
         case ("WK_DATA2D")
            call data2d_init_compute(this, grid, periodic, env)
         case ("WK_NEW_DATA2D")
            call new_data2d_init_compute(this, grid, periodic, env)
         case default
            call parametric_init_compute(this, grid, periodic, env)
         end select
      else if (this%time_series_source) then
         call time_series_init_compute(this)
      else
         if (periodic .and. this%Theta_WK /= 0.0_SP) &
            call periodic_theta_snap(this, grid, env)
         call wk_regular_coefficients(this%Tperiod, this%AMP_WK, this%Theta_WK, &
                                      this%DEP_WK, this%Delta_WK, this%D_gen, &
                                      this%rlamda, this%Beta_gen, this%Width_WK)
      end if

      ! interior bounding box of the source region (legacy ilo/ihi/jlo/jhi)
      this%ilo = grid%lp%ie + 1
      this%ihi = grid%lp%ib - 1
      this%jlo = grid%lp%je + 1
      this%jhi = grid%lp%jb - 1
      do i = grid%lp%ib, grid%lp%ie
         if (abs(this%xmk_wk(i) - this%Xc_WK) < this%Width_WK) then
            this%ilo = min(this%ilo, i)
            this%ihi = max(this%ihi, i)
         end if
      end do
      do j = grid%lp%jb, grid%lp%je
         if (abs(this%ymk_wk(j) - this%Yc_WK) < this%Ywidth_WK/2.0_SP) then
            this%jlo = min(this%jlo, j)
            this%jhi = max(this%jhi, j)
         end if
      end do

      allocate (this%mass(mloc, nloc), source=0.0_SP)

      ! Current balance: extra bottom drag over the source box, to stop the
      ! wavemaker's momentum flux driving a longshore current (legacy
      ! sources.F, per-cell box test).  Baked into a map because the box is
      ! static; zero outside it, so the kernel's add is unconditional.
      ! Legacy tests the box for any wavemaker type, but Width_WK = 0 without
      ! a mass source makes it inert there — hence the gate above.
      if (this%WaveMakerCurrentBalance) then
         allocate (this%cd_current(mloc, nloc), source=0.0_SP)
         do j = 1, nloc
            do i = 1, mloc
               if (abs(this%xmk_wk(i) - this%Xc_WK) < this%Width_WK .and. &
                   abs(this%ymk_wk(j) - this%Yc_WK) < this%Ywidth_WK/2.0_SP) &
                  this%cd_current(i, j) = this%WaveMakerCd
            end do
         end do
      end if

   end subroutine wavemaker_init_compute

   ! Deterministic, rank-uniform seed for the wavemaker phase RNG.  Every rank
   ! seeds RANDOM_NUMBER with the same value so the random phase realization is
   ! (a) rank-consistent (the west-boundary wavemaker spans the y-decomposition;
   ! each strip must draw the SAME phases) and (b) reproduced on a hotstart
   ! re-init (the seed lives in the shared input deck, so no checkpoint scalar is
   ! needed).  Filling every seed word with the scalar matches the legacy
   ! WAVE_COHERENCE idiom.  Note: the single-dir coherence shuffle re-seeds the
   ! stream mid-init with the same deck seed, so its draws and the post-shuffle
   ! phases stay deterministic AND seed-varied.
   subroutine seed_wave_phases(seed_val)
      integer, intent(in) :: seed_val

      integer :: n
      integer, allocatable :: seed(:)

      call random_seed(size=n)
      allocate (seed(n), source=seed_val)
      call random_seed(put=seed)
   end subroutine seed_wave_phases

   ! ----------------------------------------------------------------
   ! Per-stage mass source refresh (legacy SourceTerms head,
   ! old/sources.F).  WK_REG:
   !   $$ M(x,y,t) = r(t)\,D\,e^{-\beta(x - x_c)^2}
   !                 \sin\!\big(\lambda y - \omega t\big), \qquad
   !      r(t) = \tanh\!\Big(\frac{\pi t}{\tau T}\Big) $$
   ! Spectral family, from the precomputed modes ($C_m\cos\omega_k t
   ! + S_m\sin\omega_k t$ collapses each component to
   ! $\cos(\lambda y + \phi - \omega_k t)$):
   !   $$ M(x,y,t) = r(t) \sum_k \big[C_{m,k}\cos(\omega_k t)
   !                 + S_{m,k}\sin(\omega_k t)\big], \qquad
   !      r(t) = \tanh\!\Big(\frac{\pi f_p\,t}{\tau}\Big) $$
   ! Cells outside the source box stay zero, which makes the
   ! whole-domain adds in cal_rk_update/cal_sources identical to the
   ! legacy per-cell zone tests.  time is constant across RK stages
   ! (legacy TIME advances in ESTIMATE_DT), so the stepper calls this
   ! once per step at istage 1 (legacy recomputed the same values
   ! per stage).
   ! ----------------------------------------------------------------
   subroutine wavemaker_update_source(this, time)
      class(type_model_wavemaker), intent(inout) :: this
      real(SP), intent(in) :: time

      real(SP) :: bb(this%Nfreq), cc(this%Nfreq)
      real(SP) :: bb1(this%NumWaveComp)
      real(SP) :: aa, ramp, omg, wk_source
      integer :: i, j, kf

      if (.not. this%has_mass_source) return

      ! legacy leans on IEEE tanh(inf) = 1 when Time_ramp = 0; guard
      ! gives the same value without the divide-by-zero
      ramp = 1.0_SP

      ! WK_TIME (legacy sources.F WK_TIME branch): per-component cosine
      ! with its own phase; no y-dependence (legacy hard-codes theta = 0)
      !   $$ M(x,t) = r(t) \sum_k D_k\,e^{-\beta_k (x - x_c)^2}
      !               \cos\!\big(\tfrac{2\pi}{T_k} t - \phi_k\big), \qquad
      !      r(t) = \tanh\!\Big(\frac{\pi t}{\tau T_p}\Big) $$
      if (this%time_series_source) then
         if (this%Time_ramp > 0.0_SP) &
            ramp = tanh(PI/(this%Time_ramp*this%PeakPeriod)*time)
         do kf = 1, this%NumWaveComp
            bb1(kf) = cos(2.0_SP*PI/this%wave_comp(kf, 1)*time &
                          - this%wave_comp(kf, 3))
         end do
         do j = this%jlo, this%jhi
            do i = this%ilo, this%ihi
               wk_source = 0.0_SP
               do kf = 1, this%NumWaveComp
                  wk_source = wk_source + ramp*this%D_genS(kf) &
                              *exp(-this%Beta_genS(kf) &
                                   *(this%xmk_wk(i) - this%Xc_WK)**2)*bb1(kf)
               end do
               this%mass(i, j) = wk_source
            end do
         end do
         return
      end if

      if (this%spectral_source) then
         if (this%Time_ramp > 0.0_SP) &
            ramp = tanh(PI/(this%Time_ramp/this%FreqPeak)*time)
         do kf = 1, this%Nfreq
            bb(kf) = cos(this%omgn_ir(kf)*time)
            cc(kf) = sin(this%omgn_ir(kf)*time)
         end do
         do j = this%jlo, this%jhi
            do i = this%ilo, this%ihi
               wk_source = 0.0_SP
               do kf = 1, this%Nfreq
                  wk_source = wk_source + ramp*(this%Cm(i, j, kf)*bb(kf) &
                                                + this%Sm(i, j, kf)*cc(kf))
               end do
               this%mass(i, j) = wk_source
            end do
         end do
         return
      end if

      if (this%Time_ramp > 0.0_SP) &
         ramp = tanh(PI/(this%Time_ramp*this%Tperiod)*time)
      aa = ramp*this%D_gen
      omg = 2.0_SP*PI/this%Tperiod

      do j = this%jlo, this%jhi
         do i = this%ilo, this%ihi
            this%mass(i, j) = aa*exp(-this%Beta_gen*(this%xmk_wk(i) - this%Xc_WK)**2) &
                              *sin(this%rlamda*this%ymk_wk(j) - omg*time)
         end do
      end do

   end subroutine wavemaker_update_source

   ! ----------------------------------------------------------------
   ! Boundary wavemaker state overwrite at stage end, after the ghost
   ! exchange and before the sponge (legacy call order).  No-op for
   ! non-boundary types.
   !
   ! LEFT_BC_IRR (legacy IRREGULAR_LEFT_BC): on the west-boundary rank
   ! only, overwrite the ghost strip $i \le N_{ghost}$ (all j, ghosts
   ! included) with the series state at the legacy stage time
   !   $$ t_s = t + (s - 1)\,\Delta t/3 $$
   !   $$ \eta = \sum_k C_{m,k}\cos(\sigma_k t_s + \phi_k)
   !             + S_{m,k}\sin(\sigma_k t_s + \phi_k) $$
   ! (u, v likewise), then hu = (d + eta) u, hv = (d + eta) v.
   !
   ! ABS (legacy ABSORBING_GENERATING_BC): relax eta toward the series
   ! over the whole domain through the wavemaker sponge,
   !   $$ \eta := \eta_{in} + (\eta - \eta_{in})/s(i), \qquad
   !      \eta_{in} = \sum_k C_{m,k}\cos(\tfrac{\pi}{2} + \sigma_k t + \phi_k)
   !                  + S_{m,k}\sin(\cdot) $$
   ! at the step time (no stage offset), u/v untouched (legacy comments
   ! them out), hu/hv rebuilt everywhere.  NOTE: without tidal flags
   ! this is the original SpongeMaker form — the vendored legacy's
   ! Salatin-2021 rewrite reads the TIDE module's SPONGE_TIDE_WEST,
   ! which is UNALLOCATED then (upstream bug; no legacy parity
   ! possible), and drops the phases from the time factors.  Under
   ! TIDAL_BC_GEN_ABS the tide module allocates it and the Salatin
   ! form becomes the well-defined legacy path, ported bug-for-bug
   ! (no phases, + TideWest_ETA, tide-sponge relaxation).
   ! ----------------------------------------------------------------
   subroutine wavemaker_apply_boundary(this, grid, istage, dt, time, &
                                       eta, u, v, hu, hv, depth)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: N_GHOST
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      integer, intent(in) :: istage
      real(SP), intent(in) :: dt, time
      real(SP), intent(inout) :: eta(:, :), u(:, :), v(:, :)
      real(SP), intent(inout) :: hu(:, :), hv(:, :)
      real(SP), intent(in) :: depth(:, :)

      real(SP) :: bb(this%Nfreq), cc(this%Nfreq)
      real(SP) :: rtime, ein, uin, vin
      integer :: i, j, kf
      logical :: gen_abs

      if (this%left_bc_source) then
         if (.not. grid%is_back_boundary) return
         rtime = time + real(istage - 1, SP)*dt/3.0_SP
         do kf = 1, this%Nfreq
            bb(kf) = cos(this%Segma_Ser(kf)*rtime + this%Phase_Ser(kf))
            cc(kf) = sin(this%Segma_Ser(kf)*rtime + this%Phase_Ser(kf))
         end do
         do j = 1, grid%lp%nloc
            do i = 1, N_GHOST
               ein = 0.0_SP; uin = 0.0_SP; vin = 0.0_SP
               do kf = 1, this%Nfreq
                  ein = ein + this%Cm_eta(i, j, kf)*bb(kf) + this%Sm_eta(i, j, kf)*cc(kf)
                  uin = uin + this%Cm_u(i, j, kf)*bb(kf) + this%Sm_u(i, j, kf)*cc(kf)
                  vin = vin + this%Cm_v(i, j, kf)*bb(kf) + this%Sm_v(i, j, kf)*cc(kf)
               end do
               eta(i, j) = ein
               u(i, j) = uin
               v(i, j) = vin
               hu(i, j) = (depth(i, j) + eta(i, j))*u(i, j)
               hv(i, j) = (depth(i, j) + eta(i, j))*v(i, j)
            end do
         end do
         return
      end if

      if (this%abs_source) then
         if (associated(this%tide)) then
            gen_abs = this%tide%tidal_bc_gen_abs
         else
            gen_abs = .false.
         end if
         if (gen_abs) then
            ! vendored Salatin-2021 form, well-defined once the tide
            ! module allocates SPONGE_TIDE_WEST: phases DROPPED from
            ! the time factors, the west tide added to the target, and
            ! the relaxation through the tide sponge (multiplication —
            ! the profile is stored inverted)
            do kf = 1, this%Nfreq
               bb(kf) = cos(PI/2.0_SP + this%Segma_Ser(kf)*time)
               cc(kf) = sin(PI/2.0_SP + this%Segma_Ser(kf)*time)
            end do
            do j = 1, grid%lp%nloc
               do i = 1, grid%lp%mloc
                  ein = 0.0_SP
                  do kf = 1, this%Nfreq
                     ein = ein + this%Cm_eta(i, j, kf)*bb(kf) + this%Sm_eta(i, j, kf)*cc(kf)
                  end do
                  ein = ein + this%tide%eta_west
                  eta(i, j) = ein + (eta(i, j) - ein)*this%tide%sponge_west(i, j)
                  hu(i, j) = (depth(i, j) + eta(i, j))*u(i, j)
                  hv(i, j) = (depth(i, j) + eta(i, j))*v(i, j)
               end do
            end do
            return
         end if
         do kf = 1, this%Nfreq
            bb(kf) = cos(PI/2.0_SP + this%Segma_Ser(kf)*time + this%Phase_Ser(kf))
            cc(kf) = sin(PI/2.0_SP + this%Segma_Ser(kf)*time + this%Phase_Ser(kf))
         end do
         do j = 1, grid%lp%nloc
            do i = 1, grid%lp%mloc
               ein = 0.0_SP
               do kf = 1, this%Nfreq
                  ein = ein + this%Cm_eta(i, j, kf)*bb(kf) + this%Sm_eta(i, j, kf)*cc(kf)
               end do
               eta(i, j) = ein + (eta(i, j) - ein)/this%sponge_maker(i, j)
               hu(i, j) = (depth(i, j) + eta(i, j))*u(i, j)
               hv(i, j) = (depth(i, j) + eta(i, j))*v(i, j)
            end do
         end do
      end if

   end subroutine wavemaker_apply_boundary

   ! ----------------------------------------------------------------
   ! Wavemaker-zone flags for the breaker (legacy per-cell box test in
   ! old/breaker.F; there WAVEMAKER_Cbrk replaces the breaking-age
   ! scheme).  All false for non-source wavemakers — legacy gets the
   ! same from Width_WK = 0.
   ! ----------------------------------------------------------------
   subroutine wavemaker_fill_in_zone(this, in_zone)
      class(type_model_wavemaker), intent(in) :: this
      logical, intent(out) :: in_zone(:, :)

      integer :: i, j

      in_zone = .false.
      if (.not. this%has_mass_source) return

      do j = 1, size(in_zone, 2)
         do i = 1, size(in_zone, 1)
            in_zone(i, j) = abs(this%xmk_wk(i) - this%Xc_WK) < this%Width_WK &
                            .and. abs(this%ymk_wk(j) - this%Yc_WK) < this%Ywidth_WK/2.0_SP
         end do
      end do

   end subroutine wavemaker_fill_in_zone

   subroutine wavemaker_free(this)
      class(type_model_wavemaker), intent(inout) :: this

      if (allocated(this%xmk_wk)) deallocate (this%xmk_wk, this%ymk_wk)
      if (allocated(this%mass)) deallocate (this%mass)
      if (allocated(this%cd_current)) deallocate (this%cd_current)
      if (allocated(this%Cm)) deallocate (this%Cm, this%Sm, this%omgn_ir)
      if (allocated(this%Cm_eta)) &
         deallocate (this%Cm_eta, this%Sm_eta, this%Cm_u, this%Sm_u, &
                     this%Cm_v, this%Sm_v, this%Segma_Ser, this%Phase_Ser)
      if (allocated(this%sponge_maker)) deallocate (this%sponge_maker)
      if (allocated(this%wave_comp)) &
         deallocate (this%wave_comp, this%D_genS, this%Beta_genS)
      this%has_mass_source = .false.
      this%spectral_source = .false.
      this%time_series_source = .false.
      this%left_bc_source = .false.
      this%abs_source = .false.

   end subroutine wavemaker_free

   ! ----------------------------------------------------------------
   ! Private: periodic-y wave-angle snap (legacy WAVEMAKER_INITIALIZATION
   ! WK_REG PERIODIC branch).  Walks admissible along-crest mode numbers
   ! m until the snapped angle passes the requested one; error-stops if
   ! the first admissible mode already exceeds the wavenumber (domain
   ! too narrow) or none is found within 1000 modes.
   ! ----------------------------------------------------------------
   subroutine periodic_theta_snap(this, grid, env)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: GRAV
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      type(type_env), intent(inout) :: env

      real(SP) :: alpha1, tb, tc, wkn, rlamda_m, theta
      integer :: m
      character(64) :: msg

      if (this%DEP_WK == 0.0_SP .or. this%Tperiod == 0.0_SP) &
         error stop "wavemaker: re-set depth, Tperiod for wavemaker"

      ! wave number from the same dispersion relation as the
      ! coefficient solve (legacy recomputes it inline here)
      alpha1 = -0.39_SP + 1.0_SP/3.0_SP
      tb = (2.0_SP*PI/this%Tperiod)**2*this%DEP_WK/GRAV
      tc = 1.0_SP + tb*(-0.39_SP)
      wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                 /(2.0_SP*alpha1))/this%DEP_WK

      ! |theta| < |Theta_WK| folds the two sign-mirrored legacy loops
      theta = 0.0_SP
      m = 0
      do while (abs(theta) < abs(this%Theta_WK))
         m = m + 1
         rlamda_m = real(m, SP)*2.0_SP*PI/grid%dy0/(real(grid%N, SP) - 1.0_SP)
         if (rlamda_m >= wkn) &
            error stop "wavemaker: should enlarge domain for periodic "// &
            "boundary with this wave angle"
         theta = sign(asin(rlamda_m/wkn)*180.0_SP/PI, this%Theta_WK)
         if (m > 1000) &
            error stop "wavemaker: could not find a wave angle for "// &
            "periodic boundary condition"
      end do

      write (msg, '(A,F8.3,A,F8.3)') "wave angle set: ", this%Theta_WK, &
         " -> periodic-adjusted: ", theta
      call env%log%info(trim(msg))
      this%Theta_WK = theta

   end subroutine periodic_theta_snap

   ! ----------------------------------------------------------------
   ! Private: parametric density-spectrum setup — ONE pipeline
   ! (density x spreading x discretizer -> component set -> shared
   ! solve -> dense-mode collapse) for both discretizer geometries:
   !  * directional grid (legacy WK_EQUAL_DFREQ_IRREGULAR_WAVE /
   !    WK_WAVEMAKER_IRREGULAR_WAVE for EqualEnergy): the
   !    (nfreq x ntheta) grid flattens theta-fastest with amplitude
   !    from the bin energy and normalized spreading weight
   !      $$ a_{f\theta} = \frac{4}{2\sqrt 2}
   !         \sqrt{\alpha_s\,E_f\,G_\theta}, \qquad
   !         \alpha_s = \frac{H_{m0}^2}{16\,E} $$
   !  * single direction per frequency (legacy WK_NEW_EQUAL_DFREQ_
   !    IRREGULAR_WAVE, Salatin 2021): equal-df ladder, directions
   !    sign-alternating about the peak, optional coherence shuffle
   ! then the shared solve (full-pi rI, peak-frequency phase speed,
   ! per-mode periodic snap) and one seeded phase draw.
   ! ----------------------------------------------------------------
   subroutine parametric_init_compute(this, grid, periodic, env)
      use core_grid_mod, only: type_grid_2d
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env

      type(type_component_set) :: cs
      class(type_freq_spectrum), allocatable :: spec
      type(type_spreading_wrapped_normal) :: spread
      type(type_discretizer) :: disc
      real(SP), allocatable :: D_gen(:), rlamda(:), beta_gen(:)
      real(SP), allocatable :: phase2(:, :)
      integer :: kf, ktheta, c, nt_draw

      if (this%DEP_WK == 0.0_SP .or. this%FreqPeak == 0.0_SP .or. &
          this%FreqMax == 0.0_SP) &
         error stop "wavemaker: re-set depth, FreqPeak, FreqMax for wavemaker"

      allocate (this%omgn_ir(this%Nfreq), &
                this%Cm(grid%lp%mloc, grid%lp%nloc, this%Nfreq), &
                this%Sm(grid%lp%mloc, grid%lp%nloc, this%Nfreq))

      ! legacy TMA phi factor drops for the pure JONSWAP types
      call new_parametric_spectrum(this%wavemaker_type(1:3) == "JON", &
                                   this%FreqPeak, this%DEP_WK, this%GammaTMA, &
                                   spec)
      spread = new_wrapped_normal(this%ThetaPeak, this%Sigma_Theta)

      disc%nfreq = this%Nfreq
      disc%ntheta = this%Ntheta
      disc%fmin = this%FreqMin
      disc%fmax = this%FreqMax
      disc%equal_energy = this%EqualEnergy
      disc%single_dir_per_freq = this%wavemaker_type == "WK_NEW_IRR" &
                                 .or. this%single_dir
      disc%alpha_c = this%alpha_c
      disc%zero_phase = this%zero_phase
      if (periodic) disc%snap_mode = &
         merge(SNAP_NEW, SNAP_SPECTRAL, disc%single_dir_per_freq)

      call wk_build_component_set(this, spec, spread, disc, env, cs)

      allocate (D_gen(cs%n), rlamda(cs%n), beta_gen(cs%n))
      call wk_solve_components(cs, this%DEP_WK, this%Delta_WK, &
                               this%FreqPeak, .true., D_gen, rlamda, beta_gen, &
                               snap_mode=disc%snap_mode, &
                               dy=grid%dy0, nglob=grid%N, env=env, &
                               snap_label=this%wavemaker_type)
      call wk_peak_width(1.0_SP/this%FreqPeak, this%DEP_WK, this%Delta_WK, &
                         this%Width_WK)

      ! phase policy: parity builds fix all phases to zero; the seeded
      ! draw fills the legacy (nfreq, ntheta) shape column-major (kf
      ! fastest) then flattens theta-fastest, preserving the legacy
      ! realization; single-dir is the (nfreq, 1) degenerate case = the
      ! legacy 1-D draw
      nt_draw = disc%ntheta
      if (disc%single_dir_per_freq) nt_draw = 1
      allocate (phase2(disc%nfreq, nt_draw))
      if (disc%zero_phase) then
         phase2 = 0.0_SP
      else
         call random_number(phase2)
         phase2 = phase2*2.0_SP*PI
      end if
      c = 0
      do kf = 1, disc%nfreq
         do ktheta = 1, nt_draw
            c = c + 1
            cs%phase(c) = phase2(kf, ktheta)
         end do
      end do

      if (disc%single_dir_per_freq) call wk_merge_slots(cs, env)
      call calc_cm_sm(this, cs, D_gen, beta_gen, rlamda)

   end subroutine parametric_init_compute

   ! ----------------------------------------------------------------
   ! Private: THE component-set builder — evaluates the discretizer
   ! against the density and spreading models and fills the flat
   ! component set.  Phases are drawn by the caller after the solve,
   ! at the legacy position in the seeded stream.
   ! ----------------------------------------------------------------
   subroutine wk_build_component_set(this, spec, spread, disc, env, cs)
      class(type_model_wavemaker), intent(inout) :: this
      class(type_freq_spectrum), intent(in) :: spec
      class(type_dir_spreading), intent(in) :: spread
      type(type_discretizer), intent(in) :: disc
      type(type_env), intent(inout) :: env
      type(type_component_set), intent(inout) :: cs

      real(SP) :: freq(disc%nfreq), energy_bin(disc%nfreq)
      real(SP) :: theta_arr(disc%nfreq), agf(disc%nfreq), ag(disc%ntheta)
      real(SP) :: Ef, alpha_spec, theta, df
      real(SP) :: ktheta_temp, sign_kf, alpha_c, correction_coeff
      real(SP) :: w_sum, theta_mean
      logical :: valid(disc%nfreq)
      character(96) :: msg
      integer :: kf, ktheta, c, idx_theta, displace(1)

      if (disc%single_dir_per_freq) then

         df = (disc%fmax - disc%fmin)/(real(disc%nfreq, SP) - 1.0_SP)
         do kf = 1, disc%nfreq
            freq(kf) = disc%fmin + real(kf - 1, SP)*df
         end do

         idx_theta = 0
         valid = .true.
         if (disc%ntheta == 1) then
            ! legacy fills theta(1)/AG(1) only and reads the rest
            ! uninitialized — UB; the peak angle everywhere is the
            ! sensible 1D limit
            theta_arr = spread%theta_peak
            agf = 1.0_SP
         else
            displace = minloc(abs(freq - spec%fm))
            idx_theta = mod(displace(1), disc%ntheta)
            do kf = 1, disc%nfreq
               ktheta_temp = real(mod(kf - idx_theta, disc%ntheta), SP)
               if (ktheta_temp <= 0.0_SP) &
                  ktheta_temp = ktheta_temp + real(disc%ntheta, SP)
               if (mod(kf, 2) == 0) then
                  sign_kf = 1.0_SP
               else
                  sign_kf = -1.0_SP
               end if
               theta_arr(kf) = sign_kf*(-PI/2.0_SP &
                                        + PI*real(floor(ktheta_temp/2.0_SP - 0.5_SP), SP) &
                                        /(real(disc%ntheta, SP) - 1.0_SP))
               theta_arr(kf) = theta_arr(kf) + spread%theta_peak
               ! components beyond +-90 deg are dropped (zero weight, angle
               ! clamped for the solve); the energy calibration below
               ! renormalizes over the survivors
               valid(kf) = abs(theta_arr(kf)) <= 0.5_SP*PI
               if (theta_arr(kf) > 0.5_SP*PI) theta_arr(kf) = 0.5_SP*PI
               if (theta_arr(kf) < -0.5_SP*PI) theta_arr(kf) = -0.5_SP*PI
               agf(kf) = spread%weight(theta_arr(kf), freq(kf))
            end do
            agf = abs(agf)
            if (.not. any(valid)) call env%log%exit_on_error( &
               "wavemaker: every single-dir component lies beyond +-90 deg (check peak)")
            if (.not. all(valid)) then
               where (.not. valid) agf = 0.0_SP
               write (msg, '(A,I0,A)') "wavemaker: ", count(.not. valid), &
                  " single-dir components beyond +-90 deg dropped; energy renormalized"
               call env%log%warning(trim(msg))
            end if
         end if

         ! coherence shuffle: move components onto host frequencies until
         ! alpha_c percent share a frequency (Salatin 2021)
         alpha_c = disc%alpha_c
         if (alpha_c > 100.0_SP) alpha_c = 100.0_SP
         if (alpha_c < 0.0_SP) alpha_c = 0.0_SP
         if (alpha_c > 0.0_SP) &
            call wave_coherence(alpha_c, freq, disc%nfreq, disc%ntheta, &
                                idx_theta, this%seed, env)

         ! densities on the (possibly moved) frequencies; spreading
         ! weights renormalize against the band energy instead of the
         ! grid path's bin sum
         Ef = 0.0_SP
         do kf = 1, disc%nfreq
            energy_bin(kf) = spec%density(freq(kf))*df
            Ef = Ef + energy_bin(kf)
         end do

         alpha_spec = wk_alpha_spec(this, spec, Ef)
         correction_coeff = Ef/dot_product(agf, energy_bin)

         call component_set_alloc(cs, disc%nfreq)
         do kf = 1, disc%nfreq
            agf(kf) = agf(kf)*correction_coeff
            this%omgn_ir(kf) = 2.0_SP*PI*freq(kf)
            cs%freq(kf) = freq(kf)
            cs%omgn(kf) = this%omgn_ir(kf)
            cs%theta(kf) = theta_arr(kf)
            ! legacy folds the Hmo -> Hrms conversion into the half
            ! amplitude: a = H_each / (2 sqrt 2)
            cs%amp(kf) = 4.0_SP*sqrt(alpha_spec*energy_bin(kf)*agf(kf)) &
                         /sqrt(2.0_SP)/2.0_SP
         end do

      else

         if (disc%equal_energy) then
            call freq_bins_equal_energy(spec, disc%nfreq, disc%fmax, &
                                        disc%fmin, freq, energy_bin, Ef)
         else
            call freq_bins_equal_dfreq(spec, disc%nfreq, disc%fmax, &
                                       disc%fmin, freq, energy_bin, Ef)
         end if

         call directional_spreading(disc%ntheta, spread, ag, env)

         alpha_spec = wk_alpha_spec(this, spec, Ef)

         ! flat component set, theta-fastest — matches both the legacy
         ! kf-outer/ktheta-inner solve order (snap scratch carry) and
         ! the ktheta-inner accumulation order
         call component_set_alloc(cs, disc%nfreq*disc%ntheta)
         c = 0
         do kf = 1, disc%nfreq
            this%omgn_ir(kf) = 2.0_SP*PI*freq(kf)
            do ktheta = 1, disc%ntheta

               ! legacy folds the Hmo -> Hrms conversion into the half
               ! amplitude: a = H_each / (2 sqrt 2)
               if (disc%ntheta == 1) then
                  theta = spread%theta_peak
               else
                  theta = -PI/3.0_SP + spread%theta_peak &
                          + 2.0_SP/3.0_SP*PI/(real(disc%ntheta, SP) - 1.0_SP) &
                          *(real(ktheta, SP) - 1.0_SP)
                  if (theta > 0.5_SP*PI) theta = 0.5_SP*PI
                  if (theta < -0.5_SP*PI) theta = -0.5_SP*PI
               end if

               c = c + 1
               cs%freq(c) = freq(kf)
               cs%omgn(c) = this%omgn_ir(kf)
               cs%theta(c) = theta
               cs%amp(c) = 4.0_SP*sqrt(alpha_spec*energy_bin(kf)*ag(ktheta)) &
                           /sqrt(2.0_SP)/2.0_SP
               cs%slot(c) = kf
            end do
         end do

      end if

      ! init invariants (info only): the discrete realization vs the
      ! spectral targets.  sum(a^2)/2 is the component variance; under
      ! normalize: band it must reproduce the deck Hm0 up to dropped
      ! bins, under normalize: total the band's natural share.  The
      ! spread is the amp^2-weighted directional std, pre-snap.
      write (msg, '(A,F8.4,A,F8.4,A)') "wavemaker: realized Hm0 ", &
         4.0_SP*sqrt(0.5_SP*sum(cs%amp**2)), " m (deck ", this%Hmo, " m)"
      call env%log%info(trim(msg))
      if (disc%ntheta > 1) then
         w_sum = sum(cs%amp**2)
         theta_mean = dot_product(cs%amp**2, cs%theta)/w_sum
         write (msg, '(A,F7.2,A,F7.2,A)') "wavemaker: realized spread ", &
            sqrt(dot_product(cs%amp**2, (cs%theta - theta_mean)**2)/w_sum) &
            *180.0_SP/PI, " deg (deck ", this%Sigma_Theta, " deg)"
         call env%log%info(trim(msg))
      end if
      if (.not. disc%equal_energy) then
         df = (disc%fmax - disc%fmin)/(real(disc%nfreq, SP) - 1.0_SP)
         write (msg, '(A,F8.1,A)') "wavemaker: equal-df repeat period ", &
            1.0_SP/df, " s"
         call env%log%info(trim(msg))
      end if

   end subroutine wk_build_component_set

   ! ----------------------------------------------------------------
   ! Private: WK_TIME multi-component setup (legacy WK_TIME block of
   ! WAVEMAKER_INITIALIZATION + WK_WAVEMAKER_TIME_SERIES).  Reads
   ! NumWaveComp rows of (period, amplitude, phase) from WaveCompFile,
   ! solves the per-component Wei & Kirby source magnitude, and takes
   ! the shared width from PeakPeriod.
   ! ----------------------------------------------------------------
   subroutine time_series_init_compute(this)
      class(type_model_wavemaker), intent(inout) :: this

      type(type_component_set) :: cs
      real(SP), allocatable :: rlamda(:)
      integer :: kf, i, unit, ios

      allocate (this%wave_comp(this%NumWaveComp, 3), &
                this%D_genS(this%NumWaveComp), &
                this%Beta_genS(this%NumWaveComp), &
                rlamda(this%NumWaveComp))

      open (newunit=unit, file=trim(this%WaveCompFile), status="old", &
            action="read", iostat=ios)
      if (ios /= 0) error stop "wavemaker: cannot open WaveCompFile"
      do kf = 1, this%NumWaveComp
         read (unit, *, iostat=ios) (this%wave_comp(kf, i), i=1, 3)
         if (ios /= 0) error stop "wavemaker: WaveCompFile short read"
      end do
      close (unit)

      if (this%PeakPeriod == 0.0_SP) &
         error stop "wavemaker: re-set PeakPeriod for wavemaker"

      ! theta = 0 hard-coded (legacy "assume zero because no or few
      ! cases include directions"); the shared solve's cos(0) = 1
      ! factors are float-exact no-ops, rlamda comes back all zero
      call component_set_alloc(cs, this%NumWaveComp)
      do kf = 1, this%NumWaveComp
         cs%Tperiod(kf) = this%wave_comp(kf, 1)
         cs%omgn(kf) = 2.0_SP*PI/this%wave_comp(kf, 1)
         cs%theta(kf) = 0.0_SP
         cs%amp(kf) = this%wave_comp(kf, 2)
      end do

      call wk_solve_components(cs, this%DEP_WK, this%Delta_WK, &
                               0.0_SP, .false., this%D_genS, rlamda, &
                               this%Beta_genS)
      call wk_peak_width(this%PeakPeriod, this%DEP_WK, this%Delta_WK, &
                         this%Width_WK)

   end subroutine time_series_init_compute

   ! ----------------------------------------------------------------
   ! Private: WK_DATA2D setup (legacy WK_DATA2D block of WAVEMAKER_
   ! INITIALIZATION + WK_WAVEMAKER_2D_SPECTRAL_DATA + CALCULATE_Cm_Sm).
   ! WaveCompFile layout:
   !   NumFreq NumDir / PeakPeriod / freq rows / dir rows /
   !   amp(1:NumFreq) per dir / [phase(1:NumFreq) per dir, degrees]
   ! Directions with |dir| >= 60 deg are dropped (SWAN-conversion
   ! guard); the filter runs on (dir, amp, phase) tuples, so an input
   ! phase column stays paired with its own direction (legacy never
   ! compacted Phase2D — the ledger A7d mis-pair, fixed here).  The
   ! input phase converts degrees -> radians via DEG2RAD (legacy used a
   ! truncated 0.005555555555556*pi literal).
   ! FreqPeak (ramp scale) = 1/PeakPeriod from the file.
   ! ----------------------------------------------------------------
   subroutine data2d_init_compute(this, grid, periodic, env)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: GRAV, SMALL
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env

      real(SP), parameter :: alpha = -0.39_SP
      type(type_component_set) :: cs
      real(SP), allocatable :: freq(:), dire(:), amp(:, :), phase(:, :)
      real(SP), allocatable :: dire_flt(:), amp_flt(:, :), phase_flt(:, :)
      real(SP), allocatable :: dire_rad(:), dir2d(:, :)
      real(SP), allocatable :: D_gen(:), rlamda(:), beta_gen(:)
      real(SP) :: alpha1, omgn, tb, tc, wkn_snap
      logical :: input_phase
      integer :: nfreq, ndir_in, ndir, unit, ios, i, j, kf, kt, c, nfre
      integer :: mloc, nloc
      character(96) :: msg

      open (newunit=unit, file=trim(this%WaveCompFile), status="old", &
            action="read", iostat=ios)
      if (ios /= 0) error stop "wavemaker: cannot open WaveCompFile"
      read (unit, *) nfreq, ndir_in
      allocate (freq(nfreq), dire(ndir_in), amp(nfreq, ndir_in), &
                phase(nfreq, ndir_in))
      read (unit, *) this%PeakPeriod
      do j = 1, nfreq
         read (unit, *) freq(j)
      end do
      do i = 1, ndir_in
         read (unit, *) dire(i)
      end do
      do i = 1, ndir_in
         read (unit, *) (amp(j, i), j=1, nfreq)
      end do
      ! optional phase block (legacy READ ... END=): a missing/short
      ! block means generated phases
      input_phase = .true.
      do i = 1, ndir_in
         read (unit, *, iostat=ios) (phase(j, i), j=1, nfreq)
         if (ios /= 0) then
            input_phase = .false.
            exit
         end if
      end do
      close (unit)

      ! drop out-of-range directions, order-preserving (|dir| < 60);
      ! (dir, amp, phase) move as a tuple
      allocate (dire_flt(ndir_in), amp_flt(nfreq, ndir_in), &
                phase_flt(nfreq, ndir_in))
      ndir = 0
      do i = 1, ndir_in
         if (abs(dire(i)) < 60.0_SP) then
            ndir = ndir + 1
            dire_flt(ndir) = dire(i)
            amp_flt(:, ndir) = amp(:, i)
            phase_flt(:, ndir) = phase(:, i)
         end if
      end do

      write (msg, '(A,I0,A,I0)') "WK_DATA2D: NumFreq ", nfreq, &
         ", NumDir ", ndir
      call env%log%info(trim(msg))

      if (input_phase) then
         call env%log%info("WK_DATA2D: using input phase info")
         ! degrees -> radians (legacy used a truncated 0.00555*pi literal)
         do j = 1, nfreq
            do i = 1, ndir
               phase_flt(j, i) = phase_flt(j, i)*DEG2RAD
            end do
         end do
      else
         if (this%zero_phase) then
            phase_flt(:, 1:ndir) = 0.0_SP
         else
            call random_number(phase_flt(:, 1:ndir))
            phase_flt(:, 1:ndir) = phase_flt(:, 1:ndir)*2.0_SP*PI
         end if
      end if

      ! periodic pre-snap (legacy PERIODIC block of WK_WAVEMAKER_2D_
      ! SPECTRAL_DATA): per (freq, dir) nearest-of-two-modes rule with
      ! the MAX(SMALL, h) wavenumber guard; calc_periodic_theta's
      ! |theta| >= 90 error stop is unreachable (|dir| < 60 prefiltered)
      allocate (dir2d(nfreq, ndir))
      dire_rad = dire_flt(1:ndir)*DEG2RAD
      alpha1 = alpha + 1.0_SP/3.0_SP
      if (periodic) then
         do nfre = 1, nfreq
            omgn = 2.0_SP*PI*freq(nfre)
            tb = omgn*omgn*this%DEP_WK/GRAV
            tc = 1.0_SP + tb*alpha
            wkn_snap = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                            /(2.0_SP*alpha1))/max(SMALL, this%DEP_WK)
            do kt = 1, ndir
               if (dire_rad(kt) /= 0.0_SP) then
                  call calc_periodic_theta(wkn_snap, dire_rad(kt), grid%dy0, &
                                           grid%N, dir2d(nfre, kt))
                  write (msg, '(A,F8.3,A,F8.3,A,F8.3)') &
                     "WK_DATA2D periodic, freq: ", freq(nfre), ", dir: ", &
                     dire_rad(kt)*180.0_SP/PI, " -> ", &
                     dir2d(nfre, kt)*180.0_SP/PI
                  call env%log%info(trim(msg))
               else
                  dir2d(nfre, kt) = 0.0_SP
               end if
            end do
         end do
      else
         do kt = 1, ndir
            do nfre = 1, nfreq
               dir2d(nfre, kt) = dire_rad(kt)
            end do
         end do
      end if

      ! flat component set, theta-fastest
      call component_set_alloc(cs, nfreq*ndir)
      c = 0
      do kf = 1, nfreq
         do kt = 1, ndir
            c = c + 1
            cs%freq(c) = freq(kf)
            cs%omgn(c) = 2.0_SP*PI*freq(kf)
            cs%Tperiod(c) = 1.0_SP/freq(kf)
            cs%theta(c) = dir2d(kf, kt)
            cs%amp(c) = amp_flt(kf, kt)
            cs%phase(c) = phase_flt(kf, kt)
            cs%slot(c) = kf
         end do
      end do

      allocate (D_gen(cs%n), rlamda(cs%n), beta_gen(cs%n))
      call wk_solve_components(cs, this%DEP_WK, this%Delta_WK, &
                               0.0_SP, .false., D_gen, rlamda, beta_gen)
      call wk_peak_width(this%PeakPeriod, this%DEP_WK, this%Delta_WK, &
                         this%Width_WK)

      this%Nfreq = nfreq
      this%FreqPeak = 1.0_SP/this%PeakPeriod
      mloc = grid%lp%mloc
      nloc = grid%lp%nloc
      allocate (this%omgn_ir(nfreq), this%Cm(mloc, nloc, nfreq), &
                this%Sm(mloc, nloc, nfreq))
      do j = 1, nfreq
         this%omgn_ir(j) = 2.0_SP*PI*freq(j)
      end do

      call calc_cm_sm(this, cs, D_gen, beta_gen, rlamda)

   end subroutine data2d_init_compute

   ! ----------------------------------------------------------------
   ! Private: WK_NEW_DATA2D setup (legacy Salatin 2021 block of
   ! WAVEMAKER_INITIALIZATION + WK_NEW_WAVEMAKER_2D_SPECTRAL_DATA).
   ! Component-list WaveCompFile layout (one direction per component):
   !   NumFreq / PeakPeriod / freqs / dirs / amps / [phases, degrees]
   ! Components with |dir| > 90 deg are dropped; here the phase list
   ! IS compacted with its component (unlike WK_DATA2D).  FreqPeak =
   ! 1/PeakPeriod from the file.
   ! ----------------------------------------------------------------
   subroutine new_data2d_init_compute(this, grid, periodic, env)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: GRAV, SMALL
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env

      real(SP), parameter :: alpha = -0.39_SP
      type(type_component_set) :: cs
      real(SP), allocatable :: freq(:), dire(:), amp(:), phase(:)
      real(SP), allocatable :: freq_flt(:), dire_flt(:), amp_flt(:), phase_flt(:)
      real(SP), allocatable :: d_gen(:), rlamda(:), beta_gen(:)
      real(SP) :: alpha1, omgn, tb, tc, wkn_snap, snapped
      logical :: input_phase
      integer :: nfreq_in, nfreq, unit, ios, i, j
      character(96) :: msg

      open (newunit=unit, file=trim(this%WaveCompFile), status="old", &
            action="read", iostat=ios)
      if (ios /= 0) error stop "wavemaker: cannot open WaveCompFile"
      read (unit, *) nfreq_in
      allocate (freq(nfreq_in), dire(nfreq_in), amp(nfreq_in), phase(nfreq_in))
      read (unit, *) this%PeakPeriod
      do i = 1, nfreq_in
         read (unit, *) freq(i)
      end do
      do i = 1, nfreq_in
         read (unit, *) dire(i)
      end do
      do i = 1, nfreq_in
         read (unit, *) amp(i)
      end do
      input_phase = .true.
      do i = 1, nfreq_in
         read (unit, *, iostat=ios) phase(i)
         if (ios /= 0) then
            input_phase = .false.
            exit
         end if
      end do
      close (unit)

      ! drop out-of-range components, order-preserving (|dir| <= 90)
      allocate (freq_flt(nfreq_in), dire_flt(nfreq_in), amp_flt(nfreq_in), &
                phase_flt(nfreq_in))
      nfreq = 0
      do i = 1, nfreq_in
         if (abs(dire(i)) <= 90.0_SP) then
            nfreq = nfreq + 1
            freq_flt(nfreq) = freq(i)
            dire_flt(nfreq) = dire(i)
            amp_flt(nfreq) = amp(i)
            phase_flt(nfreq) = phase(i)
         end if
      end do

      write (msg, '(A,I0,A)') "WK_NEW_DATA2D: using ", nfreq, &
         " wave components"
      call env%log%info(trim(msg))

      if (input_phase) then
         call env%log%info("WK_NEW_DATA2D: using input phase info")
         do i = 1, nfreq
            phase_flt(i) = phase_flt(i)*PI/180.0_SP
         end do
      else
         if (this%zero_phase) then
            phase_flt(1:nfreq) = 0.0_SP
         else
            call random_number(phase_flt(1:nfreq))
            phase_flt(1:nfreq) = phase_flt(1:nfreq)*2.0_SP*PI
         end if
      end if

      ! periodic pre-snap (legacy PERIODIC block of WK_NEW_WAVEMAKER_2D_
      ! SPECTRAL_DATA): per-component decrement-retry rule with the
      ! MAX(SMALL, h) wavenumber guard; zero angles pass through
      dire_flt(1:nfreq) = dire_flt(1:nfreq)*DEG2RAD
      alpha1 = alpha + 1.0_SP/3.0_SP
      if (periodic) then
         do i = 1, nfreq
            omgn = 2.0_SP*PI*freq_flt(i)
            tb = omgn*omgn*this%DEP_WK/GRAV
            tc = 1.0_SP + tb*alpha
            wkn_snap = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                            /(2.0_SP*alpha1))/max(SMALL, this%DEP_WK)
            call wk_new_periodic_snap(dire_flt(i), wkn_snap, grid%dy0, &
                                      grid%N, snapped)
            write (msg, '(A,F8.3,A,F8.3,A,F8.3)') &
               "WK_NEW_DATA2D periodic, freq: ", freq_flt(i), ", dir: ", &
               dire_flt(i)*180.0_SP/PI, " -> ", snapped*180.0_SP/PI
            call env%log%info(trim(msg))
            dire_flt(i) = snapped
         end do
      end if

      call component_set_alloc(cs, nfreq)
      do i = 1, nfreq
         cs%freq(i) = freq_flt(i)
         cs%omgn(i) = 2.0_SP*PI*freq_flt(i)
         cs%Tperiod(i) = 1.0_SP/freq_flt(i)
         cs%theta(i) = dire_flt(i)
         cs%amp(i) = amp_flt(i)
         cs%phase(i) = phase_flt(i)
      end do

      allocate (d_gen(nfreq), rlamda(nfreq), beta_gen(nfreq))
      call wk_solve_components(cs, this%DEP_WK, this%Delta_WK, &
                               0.0_SP, .false., d_gen, rlamda, beta_gen)
      call wk_peak_width(this%PeakPeriod, this%DEP_WK, this%Delta_WK, &
                         this%Width_WK)

      this%Nfreq = nfreq
      this%FreqPeak = 1.0_SP/this%PeakPeriod
      allocate (this%omgn_ir(nfreq), &
                this%Cm(grid%lp%mloc, grid%lp%nloc, nfreq), &
                this%Sm(grid%lp%mloc, grid%lp%nloc, nfreq))
      do j = 1, nfreq
         this%omgn_ir(j) = 2.0_SP*PI*freq_flt(j)
      end do

      call wk_merge_slots(cs, env)
      call calc_cm_sm(this, cs, d_gen, beta_gen, rlamda)

   end subroutine new_data2d_init_compute

   ! ----------------------------------------------------------------
   ! Private: boundary wavemaker setup (legacy ABS / LEFT_BC_IRR block
   ! of WAVEMAKER_INITIALIZATION + init.F CALCULATE_SPONGE_MAKER).
   ! Builds the six dense series modes at the linear-theory reference
   ! level $z = |1 + \beta_{ref}|\,h_s$ (legacy CALCULATE_TMA_Cm_Sm[_
   ! EQUAL_DFREQ]); ABS additionally builds the relaxation sponge.
   ! WAVE_DATA_TYPE = DATA reads a 2D (freq x dir) spectrum from
   ! WaveCompFile instead (legacy io.F block + CALCULATE_DATA2D_Cm_Sm);
   ! the file header then overrides Nfreq.
   ! Legacy adds WaterLevel to Dep_Ser for LEFT_BC_IRR (init.F:807) —
   ! WaterLevel is not in the YAML schema yet (assumed 0).
   ! ----------------------------------------------------------------
   subroutine boundary_init_compute(this, grid, periodic, env, beta_ref)
      use core_grid_mod, only: type_grid_2d
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env
      real(SP), intent(in) :: beta_ref

      logical :: is_jonswap, is_data
      integer :: mloc, nloc, num_dir
      real(SP), allocatable :: per_ser(:), theta_ser(:)
      real(SP), allocatable :: amp_ser(:, :), phase_left(:, :)

      is_data = .false.
      if (len(this%WAVE_DATA_TYPE) >= 4) &
         is_data = this%WAVE_DATA_TYPE(1:4) == "DATA"

      ! the DATA file header sets the series length (legacy io.F reads
      ! the spectrum before WAVEMAKER_INITIALIZATION)
      if (is_data) then
         call read_boundary_2d_spectrum(this, num_dir, per_ser, theta_ser, &
                                        amp_ser, phase_left)
      end if

      mloc = grid%lp%mloc
      nloc = grid%lp%nloc
      allocate (this%Cm_eta(mloc, nloc, this%Nfreq), &
                this%Sm_eta(mloc, nloc, this%Nfreq), &
                this%Cm_u(mloc, nloc, this%Nfreq), &
                this%Sm_u(mloc, nloc, this%Nfreq), &
                this%Cm_v(mloc, nloc, this%Nfreq), &
                this%Sm_v(mloc, nloc, this%Nfreq), &
                this%Segma_Ser(this%Nfreq), this%Phase_Ser(this%Nfreq))

      if (is_data) then
         call data_series_coefficients(this, grid, periodic, beta_ref, &
                                       num_dir, per_ser, theta_ser, &
                                       amp_ser, phase_left)
      else
         ! legacy keys the JONSWAP switch off WAVE_DATA_TYPE here, not
         ! the wavemaker name
         is_jonswap = .false.
         if (len(this%WAVE_DATA_TYPE) >= 3) &
            is_jonswap = this%WAVE_DATA_TYPE(1:3) == "JON"

         call tma_series_coefficients(this, grid, periodic, env, is_jonswap, &
                                      beta_ref)
      end if

      if (this%abs_source) then
         allocate (this%sponge_maker(mloc, nloc), source=1.0_SP)
         call fill_sponge_maker(grid, this%WidthWaveMaker, &
                                this%R_sponge_wavemaker, &
                                this%A_sponge_wavemaker, this%sponge_maker)
      end if

   end subroutine boundary_init_compute

   ! ----------------------------------------------------------------
   ! Private: eta/u/v series modes for the boundary wavemakers (legacy
   ! CALCULATE_TMA_Cm_Sm_EQUAL_DFREQ, default, and CALCULATE_TMA_Cm_Sm
   ! for EqualEnergy).  Frequency bins and spreading share the WK_IRR
   ! helpers; the component amplitude is used directly (no Wei & Kirby
   ! source solve):
   !   $$ \eta:\ a\cos(k(x\cos\theta + y\sin\theta)), \qquad
   !      u,v:\ a\,\sigma\,\frac{\cosh(k z_{lev})}{\sinh(k h_s)}
   !            \{\cos,\sin\}\theta\,\cos(\cdot) $$
   ! with the sin-mode partners for the time expansion.  Under
   ! periodic-y each component snaps via the nearest-mode rule
   ! (calc_periodic_theta — a THIRD legacy snap algorithm).  Phases:
   ! zero for parity builds, RANDOM_NUMBER otherwise (legacy
   ! rand()*2*3.1415926 is compiler-specific).
   ! ----------------------------------------------------------------
   subroutine tma_series_coefficients(this, grid, periodic, env, is_jonswap, &
                                      beta_ref)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: GRAV, SMALL
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic, is_jonswap
      type(type_env), intent(inout) :: env
      real(SP), intent(in) :: beta_ref

      real(SP), parameter :: alpha = -0.39_SP
      class(type_freq_spectrum), allocatable :: spec
      type(type_spreading_wrapped_normal) :: spread
      real(SP) :: freq(this%Nfreq), energy_bin(this%Nfreq)
      real(SP) :: wkn(this%Nfreq), theta(this%Ntheta), ag(this%Ntheta)
      real(SP) :: amp(this%Nfreq, this%Ntheta)
      real(SP) :: Ef, alpha_spec, alpha1, tb, tc, h_ser, zlev
      real(SP) :: theta_per, transfer, arg
      integer :: kf, ktheta, i, j

      h_ser = this%DepthWaveMaker
      if (h_ser == 0.0_SP .or. this%FreqPeak == 0.0_SP .or. this%FreqMax == 0.0_SP) &
         error stop "wavemaker: re-set DepthWaveMaker, FreqPeak, FreqMax for wavemaker"

      if (this%zero_phase) then
         this%Phase_Ser = 0.0_SP
      else
         call random_number(this%Phase_Ser)
         this%Phase_Ser = this%Phase_Ser*2.0_SP*PI
      end if

      call new_parametric_spectrum(is_jonswap, this%FreqPeak, h_ser, &
                                   this%GammaTMA, spec)
      spread = new_wrapped_normal(this%ThetaPeak, this%Sigma_Theta)

      if (this%EqualEnergy) then
         call freq_bins_equal_energy(spec, this%Nfreq, this%FreqMax, &
                                     this%FreqMin, freq, energy_bin, Ef)
      else
         call freq_bins_equal_dfreq(spec, this%Nfreq, this%FreqMax, &
                                    this%FreqMin, freq, energy_bin, Ef)
      end if

      call directional_spreading(this%Ntheta, spread, ag, env)

      alpha_spec = wk_alpha_spec(this, spec, Ef)
      alpha1 = alpha + 1.0_SP/3.0_SP

      do ktheta = 1, this%Ntheta
         if (this%Ntheta == 1) then
            theta(ktheta) = this%ThetaPeak*PI/180.0_SP
         else
            theta(ktheta) = -PI/3.0_SP + this%ThetaPeak*PI/180.0_SP &
                            + 2.0_SP/3.0_SP*PI/(real(this%Ntheta, SP) - 1.0_SP) &
                            *(real(ktheta, SP) - 1.0_SP)
            if (theta(ktheta) > 0.5_SP*PI) theta(ktheta) = 0.5_SP*PI
            if (theta(ktheta) < -0.5_SP*PI) theta(ktheta) = -0.5_SP*PI
         end if
      end do

      do kf = 1, this%Nfreq
         this%Segma_Ser(kf) = 2.0_SP*PI*freq(kf)
         tb = this%Segma_Ser(kf)**2*h_ser/GRAV
         tc = 1.0_SP + tb*alpha
         wkn(kf) = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                        /(2.0_SP*alpha1))/h_ser
         if (wkn(kf) == 0.0_SP) wkn(kf) = SMALL
         do ktheta = 1, this%Ntheta
            amp(kf, ktheta) = 4.0_SP*sqrt(alpha_spec*energy_bin(kf)*ag(ktheta)) &
                              /sqrt(2.0_SP)/2.0_SP
         end do
      end do

      ! linear-theory velocity reference level (legacy Zlev)
      zlev = abs(1.0_SP + beta_ref)*h_ser

      this%Cm_eta = 0.0_SP; this%Sm_eta = 0.0_SP
      this%Cm_u = 0.0_SP; this%Sm_u = 0.0_SP
      this%Cm_v = 0.0_SP; this%Sm_v = 0.0_SP

      do kf = 1, this%Nfreq
         do ktheta = 1, this%Ntheta
            if (periodic) then
               call calc_periodic_theta(wkn(kf), theta(ktheta), grid%dy0, &
                                        grid%N, theta_per)
            else
               theta_per = theta(ktheta)
            end if
            transfer = this%Segma_Ser(kf)*cosh(wkn(kf)*zlev)/sinh(wkn(kf)*h_ser)
            do j = 1, grid%lp%nloc
               do i = 1, grid%lp%mloc
                  arg = wkn(kf)*sin(theta_per)*this%ymk_wk(j) &
                        + wkn(kf)*cos(theta_per)*this%xmk_wk(i)
                  this%Cm_eta(i, j, kf) = this%Cm_eta(i, j, kf) &
                                          + amp(kf, ktheta)*cos(arg)
                  this%Sm_eta(i, j, kf) = this%Sm_eta(i, j, kf) &
                                          + amp(kf, ktheta)*sin(arg)
                  this%Cm_u(i, j, kf) = this%Cm_u(i, j, kf) &
                                        + amp(kf, ktheta)*transfer*cos(theta_per)*cos(arg)
                  this%Sm_u(i, j, kf) = this%Sm_u(i, j, kf) &
                                        + amp(kf, ktheta)*transfer*cos(theta_per)*sin(arg)
                  this%Cm_v(i, j, kf) = this%Cm_v(i, j, kf) &
                                        + amp(kf, ktheta)*transfer*sin(theta_per)*cos(arg)
                  this%Sm_v(i, j, kf) = this%Sm_v(i, j, kf) &
                                        + amp(kf, ktheta)*transfer*sin(theta_per)*sin(arg)
               end do
            end do
         end do
      end do

   end subroutine tma_series_coefficients

   ! ----------------------------------------------------------------
   ! Private: read the boundary 2D spectrum from WaveCompFile (legacy
   ! io.F WAVE_DATA_TYPE DATA block): NumFreq NumDir / PeakPeriod
   ! (unused) / NumFreq frequencies / NumDir directions (degrees) /
   ! NumDir rows of NumFreq amplitudes / optional NumDir rows of
   ! NumFreq phases (degrees).  Frequencies invert to periods (legacy
   ! bare STOP on zero); directions convert via DEG2RAD; input
   ! phases via the truncated-pi literal.
   ! Missing phases: zero for parity builds, RANDOM_NUMBER otherwise
   ! (legacy rand()-based phase is compiler-specific).  Overrides Nfreq from
   ! the file header.
   ! ----------------------------------------------------------------
   subroutine read_boundary_2d_spectrum(this, num_dir, per_ser, theta_ser, &
                                        amp_ser, phase_left)
      class(type_model_wavemaker), intent(inout) :: this
      integer, intent(out) :: num_dir
      real(SP), allocatable, intent(out) :: per_ser(:), theta_ser(:)
      real(SP), allocatable, intent(out) :: amp_ser(:, :), phase_left(:, :)

      integer :: unit, ios, i, j, num_freq
      logical :: input_phase
      real(SP) :: peak_period

      open (newunit=unit, file=trim(this%WaveCompFile), status="old", &
            action="read", iostat=ios)
      if (ios /= 0) error stop "wavemaker: cannot open WaveCompFile"
      read (unit, *, iostat=ios) num_freq, num_dir
      if (ios /= 0) error stop "wavemaker: WaveCompFile short read"
      allocate (per_ser(num_freq), theta_ser(num_dir))
      allocate (amp_ser(num_freq, num_dir), phase_left(num_freq, num_dir))
      read (unit, *, iostat=ios) peak_period ! kept for format consistency
      do j = 1, num_freq
         read (unit, *, iostat=ios) per_ser(j) ! read in as frequency
      end do
      do i = 1, num_dir
         read (unit, *, iostat=ios) theta_ser(i)
      end do
      do i = 1, num_dir
         read (unit, *, iostat=ios) (amp_ser(j, i), j=1, num_freq)
      end do
      if (ios /= 0) error stop "wavemaker: WaveCompFile short read"
      ! phases are optional: EOF leaves input_phase false (legacy END= jump)
      input_phase = .true.
      do i = 1, num_dir
         read (unit, *, iostat=ios) (phase_left(j, i), j=1, num_freq)
         if (ios /= 0) then
            input_phase = .false.
            exit
         end if
      end do
      close (unit)

      if (input_phase) then
         phase_left = phase_left*DEG2RAD
      elseif (this%zero_phase) then
         phase_left = 0.0_SP
      else
         call random_number(phase_left)
         phase_left = phase_left*2.0_SP*PI
      end if

      do j = 1, num_freq
         if (per_ser(j) == 0.0_SP) &
            error stop "wavemaker: zero frequency in WaveCompFile"
         per_ser(j) = 1.0_SP/per_ser(j)
      end do
      theta_ser = theta_ser*DEG2RAD

      this%Nfreq = num_freq

   end subroutine read_boundary_2d_spectrum

   ! ----------------------------------------------------------------
   ! Private: series modes from the WaveCompFile 2D spectrum (legacy
   ! CALCULATE_DATA2D_Cm_Sm): component amplitudes enter directly and
   ! the wave number comes from a Newton solve of the full dispersion
   ! relation seeded with the shallow-water guess (tol 1e-8, 1000
   ! iterations — NOT the TMA closed form),
   !   $$ \sigma = 2\pi/T, \qquad F(k) = g\,k\tanh(k h_s) - \sigma^2 . $$
   ! The modes are phase-free like the TMA path; the input phases
   ! enter only through the per-frequency Phase_Ser = column-1 phase
   ! (legacy collapses the direction axis "to make consistent with cm
   ! and sm").
   ! ----------------------------------------------------------------
   subroutine data_series_coefficients(this, grid, periodic, beta_ref, &
                                       num_dir, per_ser, theta_ser, &
                                       amp_ser, phase_left)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: GRAV
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      real(SP), intent(in) :: beta_ref
      integer, intent(in) :: num_dir
      real(SP), intent(in) :: per_ser(:), theta_ser(:)
      real(SP), intent(in) :: amp_ser(:, :), phase_left(:, :)

      real(SP) :: wkn(this%Nfreq)
      real(SP) :: h_ser, zlev, celerity, fk, fkdif
      real(SP) :: theta_per, transfer, arg
      integer :: kf, kdir, i, j, iter

      h_ser = this%DepthWaveMaker
      if (h_ser == 0.0_SP) &
         error stop "wavemaker: re-set DepthWaveMaker for wavemaker"

      do kf = 1, this%Nfreq
         this%Segma_Ser(kf) = 2.0*PI/per_ser(kf)
         this%Phase_Ser(kf) = phase_left(kf, 1)
         ! Newton from the shallow-water guess (legacy literals)
         celerity = sqrt(GRAV*h_ser)
         wkn(kf) = 2.0*PI/(celerity*per_ser(kf))
         iter = 0
         do
            fk = GRAV*wkn(kf)*tanh(wkn(kf)*h_ser) - this%Segma_Ser(kf)**2
            if (abs(fk) <= 1.0e-8 .or. iter > 1000) exit
            fkdif = GRAV*wkn(kf)*h_ser*(1.0 - tanh(wkn(kf)*h_ser)**2) &
                    + GRAV*tanh(wkn(kf)*h_ser)
            wkn(kf) = wkn(kf) - fk/fkdif
            iter = iter + 1
         end do
      end do

      ! linear-theory velocity reference level (legacy Zlev)
      zlev = abs(1.0_SP + beta_ref)*h_ser

      this%Cm_eta = 0.0_SP; this%Sm_eta = 0.0_SP
      this%Cm_u = 0.0_SP; this%Sm_u = 0.0_SP
      this%Cm_v = 0.0_SP; this%Sm_v = 0.0_SP

      do kf = 1, this%Nfreq
         do kdir = 1, num_dir
            if (periodic) then
               call calc_periodic_theta(wkn(kf), theta_ser(kdir), grid%dy0, &
                                        grid%N, theta_per)
            else
               theta_per = theta_ser(kdir)
            end if
            transfer = this%Segma_Ser(kf)*cosh(wkn(kf)*zlev)/sinh(wkn(kf)*h_ser)
            do j = 1, grid%lp%nloc
               do i = 1, grid%lp%mloc
                  arg = wkn(kf)*sin(theta_per)*this%ymk_wk(j) &
                        + wkn(kf)*cos(theta_per)*this%xmk_wk(i)
                  this%Cm_eta(i, j, kf) = this%Cm_eta(i, j, kf) &
                                          + amp_ser(kf, kdir)*cos(arg)
                  this%Sm_eta(i, j, kf) = this%Sm_eta(i, j, kf) &
                                          + amp_ser(kf, kdir)*sin(arg)
                  this%Cm_u(i, j, kf) = this%Cm_u(i, j, kf) &
                                        + amp_ser(kf, kdir)*transfer*cos(theta_per)*cos(arg)
                  this%Sm_u(i, j, kf) = this%Sm_u(i, j, kf) &
                                        + amp_ser(kf, kdir)*transfer*cos(theta_per)*sin(arg)
                  this%Cm_v(i, j, kf) = this%Cm_v(i, j, kf) &
                                        + amp_ser(kf, kdir)*transfer*sin(theta_per)*cos(arg)
                  this%Sm_v(i, j, kf) = this%Sm_v(i, j, kf) &
                                        + amp_ser(kf, kdir)*transfer*sin(theta_per)*sin(arg)
               end do
            end do
         end do
      end do

   end subroutine data_series_coefficients

   ! ----------------------------------------------------------------
   ! Private: ABS relaxation sponge (legacy CALCULATE_SPONGE_MAKER,
   ! old/sponge.F): west strip of global width $W/\Delta x + N_{ghost}$,
   !   $$ s(i) = \max\!\big(A^{\,r}, 1\big), \qquad
   !      r = R^{\lfloor 50 (i_g - 1) / (I_w - 1) \rfloor} $$
   ! (both exponent divisions are truncating integer arithmetic).
   ! Legacy loops i = 1..Iwidth on every rank regardless of the local
   ! extent — clamped to mloc here (identical values in range).
   ! ----------------------------------------------------------------
   subroutine fill_sponge_maker(grid, width, r_sponge, a_sponge, sponge)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: N_GHOST
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: width, r_sponge, a_sponge
      real(SP), intent(inout) :: sponge(:, :)

      real(SP) :: ri, lim
      integer :: i, j, iwidth

      iwidth = int(width/grid%dx0) + N_GHOST
      do j = 1, size(sponge, 2)
         do i = 1, min(iwidth, size(sponge, 1))
            lim = 1.0_SP
            if (sponge(i, j) > 1.0_SP) lim = sponge(i, j)
            ri = r_sponge**((50*(i + grid%ibegin - 2))/(iwidth - 1))
            sponge(i, j) = max(a_sponge**ri, lim)
         end do
      end do

   end subroutine fill_sponge_maker

   ! ----------------------------------------------------------------
   ! Private: nearest-mode periodic wave-angle snap for the boundary
   ! wavemakers (legacy CalcPeriodicTheta — a THIRD snap algorithm):
   ! walk modes up past the target, then pick whichever of the modes
   ! m-1, m is CLOSER; caps at $\pi/2 - \epsilon$; error-stops for
   ! $|\theta| \ge 90°$.  theta = 0 passes through (legacy SMALL gate).
   ! ----------------------------------------------------------------
   subroutine calc_periodic_theta(wkn, theta_in, dy, nglob, theta_out)
      use core_constants_mod, only: SMALL
      real(SP), intent(in) :: wkn, theta_in, dy
      integer, intent(in) :: nglob
      real(SP), intent(out) :: theta_out

      real(SP) :: walked, rlamda_m, nearest
      integer :: m

      if (theta_in*180.0_SP/PI >= 90.0_SP .or. theta_in*180.0_SP/PI <= -90.0_SP) &
         error stop "wavemaker: input angle out of range of -90 -> 90"

      theta_out = theta_in
      if (abs(theta_in) <= SMALL) return

      if (theta_in > 0.0_SP) then
         walked = 0.0_SP
         m = 0
         do while (walked < theta_in)
            m = m + 1
            rlamda_m = real(m, SP)*2.0_SP*PI/dy/(real(nglob, SP) - 1.0_SP)
            if (rlamda_m >= wkn) then
               walked = PI*0.5_SP - SMALL
            else
               walked = asin(rlamda_m/wkn)
            end if
            if (m > 1000) walked = PI*0.5_SP - SMALL
         end do
         nearest = asin(real(m - 1, SP)*2.0_SP*PI/dy &
                        /(real(nglob, SP) - 1.0_SP)/wkn)
         if (abs(nearest - theta_in) < abs(theta_in - walked)) then
            theta_out = nearest
         else
            theta_out = walked
         end if
      else
         walked = 0.0_SP
         m = 0
         do while (walked > theta_in)
            m = m + 1
            rlamda_m = real(m, SP)*2.0_SP*PI/dy/(real(nglob, SP) - 1.0_SP)
            if (rlamda_m >= wkn) then
               walked = -PI*0.5_SP + SMALL
            else
               walked = -asin(rlamda_m/wkn)
            end if
            if (m > 1000) walked = -PI*0.5_SP + SMALL
         end do
         nearest = -asin(real(m - 1, SP)*2.0_SP*PI/dy &
                         /(real(nglob, SP) - 1.0_SP)/wkn)
         if (abs(nearest - theta_in) < abs(theta_in - walked)) then
            theta_out = nearest
         else
            theta_out = walked
         end if
      end if

   end subroutine calc_periodic_theta

   ! ----------------------------------------------------------------
   ! Wei & Kirby internal-source coefficients for a regular wave
   ! (legacy WK_WAVEMAKER_REGULAR_WAVE, old/wavemaker.F): an n = 1
   ! component set through the shared solve — see wk_solve_components
   ! for the math.
   ! ----------------------------------------------------------------
   subroutine wk_regular_coefficients(Tperiod, amp, theta_deg, h_gen, delta, &
                                      D_gen, rlamda, beta_gen, width)
      real(SP), intent(in)  :: Tperiod, amp, theta_deg, h_gen, delta
      real(SP), intent(out) :: D_gen, rlamda, beta_gen, width

      type(type_component_set) :: cs
      real(SP) :: D1(1), rl1(1), b1(1)

      if (h_gen == 0.0_SP .or. Tperiod == 0.0_SP) &
         error stop "wavemaker: re-set depth, Tperiod for wavemaker"

      call component_set_alloc(cs, 1)
      cs%Tperiod(1) = Tperiod
      cs%omgn(1) = 2.0_SP*PI/Tperiod
      cs%theta(1) = theta_deg*PI/180.0_SP
      cs%amp(1) = amp

      call wk_solve_components(cs, h_gen, delta, 0.0_SP, .false., &
                               D1, rl1, b1)
      call wk_peak_width(Tperiod, h_gen, delta, width)
      D_gen = D1(1)
      rlamda = rl1(1)
      beta_gen = b1(1)

   end subroutine wk_regular_coefficients

   ! ----------------------------------------------------------------
   ! Private: coherence shuffle (legacy WAVE_COHERENCE, Salatin 2021).
   ! Host frequencies sit every ntheta-th component (anchored at the
   ! peak-frequency index, last component always a host); randomly
   ! drawn non-host components move UP to the nearest host frequency
   ! until alpha_c percent of components share a frequency.  Legacy
   ! draws with C rand() at its default seed (deterministic per libc,
   ! rank-consistent, even under ZERO_PHASE) — reproduced by
   ! re-seeding RANDOM_SEED with the deck seed (default 66, the legacy
   ! convention) so runs stay bitwise reproducible, rank-consistent,
   ! and seed-varied; the legacy draw sequence itself is
   ! compiler-specific, so alpha_c > 0 has no legacy parity.
   ! ----------------------------------------------------------------
   subroutine wave_coherence(alpha_c, freq, nfreq, ntheta, idx_theta, seed_val, &
                             env)
      real(SP), intent(in) :: alpha_c
      integer, intent(in) :: nfreq, ntheta, idx_theta, seed_val
      real(SP), intent(inout) :: freq(nfreq)
      type(type_env), intent(inout) :: env

      real(SP) :: freq_temp(nfreq), host_freqs(nfreq)
      real(SP) :: pool(nfreq), r, cand, host_freq
      integer :: repetitions(nfreq), host_idx(nfreq)
      integer, allocatable :: seed(:)
      integer :: num_coherent, num_coherent_temp, nhost, npool
      integer :: kf, jj, cand_idx, hi, host_whole, seed_n

      call random_seed(size=seed_n)
      allocate (seed(seed_n), source=seed_val)
      call random_seed(put=seed)

      repetitions = 1
      num_coherent = int(alpha_c/100.0_SP*real(nfreq, SP))

      ! host indices: every ntheta-th component from the peak anchor;
      ! the last component is always a host (legacy append)
      nhost = 0
      kf = idx_theta
      if (idx_theta == 0) kf = idx_theta + ntheta
      do while (kf <= nfreq)
         nhost = nhost + 1
         host_idx(nhost) = kf
         kf = kf + ntheta
      end do
      if (host_idx(nhost) /= nfreq) then
         nhost = nhost + 1
         host_idx(nhost) = nfreq
      end if
      host_freqs(1:nhost) = freq(host_idx(1:nhost))

      ! candidate pool = all non-host components
      npool = 0
      jj = 1
      do kf = 1, nfreq
         if (jj <= nhost .and. kf == host_idx(min(jj, nhost))) then
            jj = jj + 1
         else
            npool = npool + 1
            pool(npool) = freq(kf)
         end if
      end do

      freq_temp = freq
      num_coherent_temp = 0
      do while (num_coherent_temp < num_coherent)
         if (npool == 0) then
            call env%log%warning("wave_coherence: candidate pool exhausted "// &
                                 "before reaching alpha_c")
            exit
         end if
         call random_number(r)
         cand_idx = max(1, ceiling(r*real(npool, SP)))
         cand = pool(cand_idx)
         ! remove the drawn component from the pool
         pool(cand_idx:npool - 1) = pool(cand_idx + 1:npool)
         npool = npool - 1
         ! nearest host frequency ABOVE the candidate
         hi = 0
         do jj = 1, nhost
            if (host_freqs(jj) - cand > 0.0_SP) then
               if (hi == 0) then
                  hi = jj
               else if (host_freqs(jj) - cand < host_freqs(hi) - cand) then
                  hi = jj
               end if
            end if
         end do
         if (hi == 0) cycle   ! nothing above (legacy would index 0 — UB)
         host_freq = host_freqs(hi)
         host_whole = host_idx(hi)
         do kf = 1, nfreq
            if (freq_temp(kf) == cand) freq_temp(kf) = host_freq
         end do
         repetitions(host_whole) = repetitions(host_whole) + 1
         do kf = 1, nfreq
            if (freq_temp(kf) == host_freq) &
               repetitions(kf) = repetitions(host_whole)
         end do
         num_coherent_temp = count(repetitions > 1)
      end do
      freq = freq_temp

   end subroutine wave_coherence

   ! ----------------------------------------------------------------
   ! Private: periodic wave-angle snap for the WK_NEW family (legacy
   ! goto-1000 blocks — a FOURTH snap variant).  Walks along-crest
   ! modes up past the requested angle then snaps DOWN one mode; when
   ! no mode fits below the wavenumber the ANGLE is decremented toward
   ! zero by 0.001 rad and the walk restarts (no pi/2 cap, no error
   ! stop); zero passes through.
   ! ----------------------------------------------------------------
   subroutine wk_new_periodic_snap(theta_in, wkn, dy, nglob, theta_out)
      real(SP), intent(in) :: theta_in, wkn, dy
      integer, intent(in) :: nglob
      real(SP), intent(out) :: theta_out

      real(SP) :: theta_temp, walked, rlamda_m
      integer :: m

      theta_temp = theta_in
      retry: do
         if (theta_temp > 0.0_SP) then
            walked = 0.0_SP
            m = 0
            do while (walked < theta_temp)
               m = m + 1
               rlamda_m = real(m, SP)*2.0_SP*PI/dy/(real(nglob, SP) - 1.0_SP)
               if (rlamda_m >= wkn) then
                  theta_temp = theta_temp - 0.001_SP
                  if (theta_temp <= 0.0_SP) then
                     theta_out = 0.0_SP
                     return
                  end if
                  cycle retry
               end if
               walked = asin(rlamda_m/wkn)
            end do
            if (rlamda_m < wkn) &
               walked = asin(real(m - 1, SP)*2.0_SP*PI/dy &
                             /(real(nglob, SP) - 1.0_SP)/wkn)
            theta_out = walked
            return
         else if (theta_temp < 0.0_SP) then
            walked = 0.0_SP
            m = 0
            do while (walked > theta_temp)
               m = m + 1
               rlamda_m = real(m, SP)*2.0_SP*PI/dy/(real(nglob, SP) - 1.0_SP)
               if (rlamda_m >= wkn) then
                  theta_temp = theta_temp + 0.001_SP
                  if (theta_temp >= 0.0_SP) then
                     theta_out = 0.0_SP
                     return
                  end if
                  cycle retry
               end if
               walked = -asin(rlamda_m/wkn)
            end do
            if (rlamda_m < wkn) &
               walked = -asin(real(m - 1, SP)*2.0_SP*PI/dy &
                              /(real(nglob, SP) - 1.0_SP)/wkn)
            theta_out = walked
            return
         else
            theta_out = theta_temp
            return
         end if
      end do retry

   end subroutine wk_new_periodic_snap

   ! ----------------------------------------------------------------
   ! Private: uniform frequency bins (legacy WK_EQUAL_DFREQ_IRREGULAR_
   ! WAVE head): $f_k = f_{min} + (k-1)\,df$, $df = \frac{f_{max}-f_{min}}
   ! {N_f - 1}$, bin energy $E_k = S(f_k)\,df$.
   ! ----------------------------------------------------------------
   subroutine freq_bins_equal_dfreq(spec, nfreq, fmax, fmin, freq, energy_bin, Ef)
      class(type_freq_spectrum), intent(in) :: spec
      integer, intent(in)  :: nfreq
      real(SP), intent(in) :: fmax, fmin
      real(SP), intent(out) :: freq(nfreq), energy_bin(nfreq), Ef

      real(SP) :: df
      integer :: kff

      df = (fmax - fmin)/(real(nfreq, SP) - 1.0_SP)
      Ef = 0.0_SP
      do kff = 1, nfreq
         freq(kff) = fmin + real(kff - 1, SP)*df
         energy_bin(kff) = spec%density(freq(kff))*df
         Ef = Ef + energy_bin(kff)
      end do

   end subroutine freq_bins_equal_dfreq

   ! ----------------------------------------------------------------
   ! Private: equal-energy frequency bins (legacy WK_WAVEMAKER_
   ! IRREGULAR_WAVE head): scan the spectrum on 10000 points, split
   ! into bins of energy $E/(N_f+1)$, put each component at the bin
   ! midpoint.  All components carry the same bin energy.  Legacy
   ! zero-frequency and non-monotone tail guards kept (the kff = 1
   ! zero guard additionally bounds-checks — legacy would index 0).
   ! ----------------------------------------------------------------
   subroutine freq_bins_equal_energy(spec, nfreq, fmax, fmin, freq, energy_bin, Ef)
      integer, parameter :: NSCAN = 10000
      class(type_freq_spectrum), intent(in) :: spec
      integer, intent(in)  :: nfreq
      real(SP), intent(in) :: fmax, fmin
      real(SP), intent(out) :: freq(nfreq), energy_bin(nfreq), Ef

      real(SP) :: ef_scan(NSCAN)
      real(SP) :: fre, ef_bin, ef_add
      integer :: k, kf, kff, kb

      Ef = 0.0_SP
      do kf = 1, NSCAN
         fre = fmin + (fmax - fmin)/real(NSCAN, SP)*(real(kf, SP) - 1.0_SP)
         ef_scan(kf) = spec%density(fre)
         Ef = Ef + ef_scan(kf)*(fmax - fmin)/real(NSCAN, SP)
      end do

      ef_bin = Ef/real(nfreq + 1, SP)

      kb = 0
      do kff = 1, nfreq
         freq(kff) = 0.0_SP
         ef_add = 0.0_SP
         do k = kb + 1, NSCAN
            ef_add = ef_add + ef_scan(k)*(fmax - fmin)/real(NSCAN, SP)
            if (ef_add >= ef_bin .or. k == NSCAN) then
               ! (k - kb)/2 is a truncating integer division in legacy
               freq(kff) = fmin + (fmax - fmin)/real(NSCAN, SP) &
                           *real(k - (k - kb)/2, SP)
               kb = k
               exit
            end if
         end do
         if (freq(kff) == 0.0_SP .and. kff > 1) freq(kff) = freq(kff - 1)
      end do
      if (nfreq >= 2) then
         if (freq(nfreq) < freq(nfreq - 1)) freq(nfreq) = freq(nfreq - 1)
      end if

      energy_bin = ef_bin

   end subroutine freq_bins_equal_energy

   ! ----------------------------------------------------------------
   ! Private: TMA spectral density (legacy inline block, both spectrum
   ! variants).  JONSWAP with the Kitaigorodskii depth factor
   ! $\phi(\bar\omega)$, $\bar\omega = 2\pi f\sqrt{h/g}$:
   !   $$ S(f) = \frac{g^2 \phi}{(2\pi)^4 f^5}
   !             \exp\!\Big[-\frac54\Big(\frac{f}{f_p}\Big)^{-4}\Big]\,
   !             \gamma^{\exp\left[-\frac{(f/f_p - 1)^2}{2\sigma^2}\right]},
   !      \qquad \sigma = \begin{cases}0.07 & f \le f_p\\
   !                                   0.09 & f > f_p\end{cases} $$
   ! $\phi = 1$ for the pure JONSWAP types.
   ! ----------------------------------------------------------------
   function tma_density(is_jonswap, fre, fm, h_gen, gamma_spec) result(etma)
      use core_constants_mod, only: GRAV
      logical, intent(in)  :: is_jonswap
      real(SP), intent(in) :: fre, fm, h_gen, gamma_spec
      real(SP) :: etma

      real(SP) :: omiga_spec, phi, sigma_spec

      omiga_spec = 2.0_SP*PI*fre*sqrt(h_gen/GRAV)
      phi = 1.0_SP - 0.5_SP*(2.0_SP - omiga_spec)**2
      if (omiga_spec <= 1.0_SP) phi = 0.5_SP*omiga_spec**2
      if (omiga_spec >= 2.0_SP) phi = 1.0_SP
      if (is_jonswap) phi = 1.0_SP

      sigma_spec = 0.07_SP
      if (fre > fm) sigma_spec = 0.09_SP

      etma = GRAV**2*fre**(-5)*(2.0_SP*PI)**(-4)*phi &
             *exp(-5.0_SP/4.0_SP*(fre/fm)**(-4)) &
             *gamma_spec**(exp(-(fre/fm - 1.0_SP)**2/(2.0_SP*sigma_spec**2)))

   end function tma_density

   function jonswap_density(this, f) result(s)
      class(type_spectrum_jonswap), intent(in) :: this
      real(SP), intent(in) :: f
      real(SP) :: s

      s = tma_density(.true., f, this%fm, this%h_gen, this%gamma_spec)
   end function jonswap_density

   function tma_spectrum_density(this, f) result(s)
      class(type_spectrum_tma), intent(in) :: this
      real(SP), intent(in) :: f
      real(SP) :: s

      s = tma_density(.false., f, this%fm, this%h_gen, this%gamma_spec)
   end function tma_spectrum_density

   ! legacy path selector: the pure JONSWAP types drop the TMA phi factor
   subroutine new_parametric_spectrum(is_jonswap, fm, h_gen, gamma_spec, spec)
      logical, intent(in) :: is_jonswap
      real(SP), intent(in) :: fm, h_gen, gamma_spec
      class(type_freq_spectrum), allocatable, intent(out) :: spec

      if (is_jonswap) then
         spec = type_spectrum_jonswap(fm=fm, h_gen=h_gen, gamma_spec=gamma_spec)
      else
         spec = type_spectrum_tma(fm=fm, h_gen=h_gen, gamma_spec=gamma_spec)
      end if
   end subroutine new_parametric_spectrum

   ! sigma = 0 (1D configs) never evaluates the series — n_series stays 0
   function new_wrapped_normal(theta_peak_deg, sigma_deg) result(spread)
      real(SP), intent(in) :: theta_peak_deg, sigma_deg
      type(type_spreading_wrapped_normal) :: spread

      spread%theta_peak = theta_peak_deg*PI/180.0_SP
      spread%sigma = sigma_deg*PI/180.0_SP
      if (spread%sigma > 0.0_SP) spread%n_series = int(20.0_SP/spread%sigma)
   end function new_wrapped_normal

   ! $$ G(\theta) = \frac{1}{2\pi} + \frac{1}{\pi}\sum_{n=1}^{N}
   !    e^{-\frac{(n\sigma_\theta)^2}{2}}\cos\!\big(n(\theta-\theta_p)\big),
   !    \qquad N = \lfloor 20/\sigma_\theta \rfloor $$
   function wrapped_normal_weight(this, theta, f) result(w)
      class(type_spreading_wrapped_normal), intent(in) :: this
      real(SP), intent(in) :: theta, f
      real(SP) :: w

      integer :: k_n

      ! f is interface-carried only — wrapped normal is frequency-uniform
      w = 1.0_SP/(2.0_SP*PI)
      do k_n = 1, this%n_series
         w = w + (1.0_SP/PI)*exp(-0.5_SP*(real(k_n, SP)*this%sigma)**2) &
             *cos(real(k_n, SP)*(theta - this%theta_peak))
      end do
   end function wrapped_normal_weight

   ! ----------------------------------------------------------------
   ! Private: the spectral scale $\alpha_s = H_{m0}^2/(16 E)$.  Under
   ! normalize: band (legacy default) $E$ is the truncated [min, max]
   ! band integral, so the band is inflated to carry the full Hm0;
   ! under normalize: total $E$ is the full-spectrum integral and the
   ! band keeps its natural energy share.
   ! ----------------------------------------------------------------
   function wk_alpha_spec(this, spec, Ef) result(alpha_spec)
      class(type_model_wavemaker), intent(in) :: this
      class(type_freq_spectrum), intent(in) :: spec
      real(SP), intent(in) :: Ef
      real(SP) :: alpha_spec

      if (this%normalize_total) then
         alpha_spec = this%Hmo**2/16.0_SP/spectrum_total_energy(spec)
      else
         alpha_spec = this%Hmo**2/16.0_SP/Ef
      end if
   end function wk_alpha_spec

   ! ----------------------------------------------------------------
   ! Private: full-spectrum energy for normalize: total — trapezoid of
   ! the density over $[f_p/10,\ 10 f_p]$; the tails beyond contribute
   ! < 1e-4 of the integral (double-exponential low side, $f^{-5}$
   ! high side).
   ! ----------------------------------------------------------------
   function spectrum_total_energy(spec) result(E_total)
      class(type_freq_spectrum), intent(in) :: spec
      real(SP) :: E_total

      integer, parameter :: n_scan = 10000
      real(SP) :: f_lo, df_scan, weight
      integer :: k

      f_lo = spec%fm/10.0_SP
      df_scan = (10.0_SP*spec%fm - f_lo)/real(n_scan, SP)
      E_total = 0.0_SP
      do k = 0, n_scan
         weight = 1.0_SP
         if (k == 0 .or. k == n_scan) weight = 0.5_SP
         E_total = E_total + weight*spec%density(f_lo + real(k, SP)*df_scan)
      end do
      E_total = E_total*df_scan
   end function spectrum_total_energy

   ! ----------------------------------------------------------------
   ! Private: directional-grid bin weights (legacy ykchoi 11/07/2016
   ! block).  Bins span $\theta_p \pm \pi/3$; the weight at each bin
   ! angle comes from the spreading model, normalized by the (signed)
   ! sum then made positive (legacy ABS).
   ! Bins landing beyond $\pm\pi/2$ are DROPPED (zero weight) and the
   ! normalization runs over the survivors, so the excluded weight
   ! redistributes instead of piling at the clamp where the source is
   ! inert (legacy clamped the angle and silently lost the energy);
   ! the excluded fraction is warned.
   ! ----------------------------------------------------------------
   subroutine directional_spreading(ntheta, spread, ag, env)
      integer, intent(in)  :: ntheta
      class(type_dir_spreading), intent(in) :: spread
      real(SP), intent(out) :: ag(ntheta)
      type(type_env), intent(inout) :: env

      real(SP) :: theta, sum_ag, sum_all
      logical :: valid(ntheta)
      integer :: ktheta
      character(96) :: msg

      if (ntheta == 1) then
         ag(1) = 1.0_SP
         return
      end if

      sum_ag = 0.0_SP
      sum_all = 0.0_SP
      do ktheta = 1, ntheta
         theta = -PI/3.0_SP + spread%theta_peak &
                 + 2.0_SP/3.0_SP*PI/(real(ntheta, SP) - 1.0_SP) &
                 *(real(ktheta, SP) - 1.0_SP)
         valid(ktheta) = abs(theta) <= 0.5_SP*PI

         ! frequency-uniform evaluation — a dspr(f) spreading model needs
         ! per-(kf, ktheta) weights and restructures this normalization
         ag(ktheta) = spread%weight(theta, 0.0_SP)
         sum_all = sum_all + ag(ktheta)
         if (valid(ktheta)) sum_ag = sum_ag + ag(ktheta)
      end do

      if (.not. any(valid)) call env%log%exit_on_error( &
         "wavemaker: every directional bin lies beyond +-90 deg (check peak)")
      if (.not. all(valid)) then
         where (.not. valid) ag = 0.0_SP
         write (msg, '(A,I0,A,F5.1,A)') "wavemaker: ", count(.not. valid), &
            " directional bins beyond +-90 deg dropped (", &
            (sum_all - sum_ag)/sum_all*100.0_SP, &
            " % of spread weight); renormalized"
         call env%log%warning(trim(msg))
      end if
      ! small bins can go negative; integral of G must be 1
      ag = abs(ag/sum_ag)

   end subroutine directional_spreading

   ! ----------------------------------------------------------------
   ! Private: per-component periodic wave-angle snap (legacy PERIODIC
   ! block inside both spectrum variants).  Walks along-crest mode
   ! numbers up past the requested angle, then snaps DOWN one mode;
   ! caps at $\pm\pi/2$ when no mode fits (no error stop — unlike the
   ! WK_REG snap).  scratch is caller-saved across components: a
   ! theta = 0 component keeps the previous snapped value (legacy
   ! PARAM static-scratch semantics).
   ! ----------------------------------------------------------------
   subroutine spectral_periodic_snap(theta, scratch, wkn, dy, nglob, fre, env)
      real(SP), intent(inout) :: theta, scratch
      real(SP), intent(in) :: wkn, dy, fre
      integer, intent(in) :: nglob
      type(type_env), intent(inout) :: env

      real(SP) :: rlamda_m
      integer :: m
      character(80) :: msg

      rlamda_m = 0.0_SP
      if (theta > 0.0_SP) then
         scratch = 0.0_SP
         m = 0
         do while (scratch < theta)
            m = m + 1
            rlamda_m = real(m, SP)*2.0_SP*PI/dy/(real(nglob, SP) - 1.0_SP)
            if (rlamda_m >= wkn) then
               scratch = PI/2.0_SP
            else
               scratch = asin(rlamda_m/wkn)
            end if
         end do
         if (rlamda_m < wkn) &
            scratch = asin(real(m - 1, SP)*2.0_SP*PI/dy &
                           /(real(nglob, SP) - 1.0_SP)/wkn)
      else if (theta < 0.0_SP) then
         scratch = 0.0_SP
         m = 0
         do while (scratch > theta)
            m = m + 1
            rlamda_m = real(m, SP)*2.0_SP*PI/dy/(real(nglob, SP) - 1.0_SP)
            if (rlamda_m >= wkn) then
               scratch = -PI/2.0_SP
            else
               scratch = -asin(rlamda_m/wkn)
            end if
         end do
         if (rlamda_m < wkn) &
            scratch = -asin(real(m - 1, SP)*2.0_SP*PI/dy &
                            /(real(nglob, SP) - 1.0_SP)/wkn)
      end if

      write (msg, '(A,F8.3,A,F8.3,A,F8.3)') "periodic bc, freq: ", fre, &
         ", dir: ", theta*180.0_SP/PI, " -> ", scratch*180.0_SP/PI
      call env%log%info(trim(msg))
      theta = scratch

   end subroutine spectral_periodic_snap

end module model_wavemaker_mod
