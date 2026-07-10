!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Wavemaker parameters YAML reader (bridge)
!
!  Wavemaker taxonomy — three distinct integration points in the engine:
!    * initial-condition types (INI_SOLITARY, INI_REC/GAU/DIP, N_WAVE):
!      one-shot state fill at t=0 via apply_ic(); no per-step work.
!    * internal source types (WK_REG, WK_IRR, TMA_1D/JON_1D/JON_2D,
!      WK_TIME, WK_NEW_*): continuous generation — init_compute derives
!      the generation coefficients, update_source refreshes the mass
!      array each stage (WK_REG live; spectral types pending).
!    * boundary types (ABS, LEFT_BC_IRR, LEF_SOL): own the west ghost
!      strip each step — the BC service must skip the wall mirror there
!      (fill_west=.false. in kernel_bc).
!
!  YAML block: wavemaker:       (top-level; omit for no wavemaker)
!    type: <string>             default 'nothing'
!    --- shared position/ramp ---
!    Xc_WK: <length>
!    Yc_WK: <length>            default 0
!    DEP_WK: <length>
!    Time_ramp: <time>          default 0
!    Delta_WK: <length>         default 0.5
!    Ywidth_WK: <length>        default 999999 (= no limit)
!    --- solitary / initial IC ---
!    AMP: <length>              AMP_SOLI
!    DEP: <length>              DEP_SOLI
!    LAGTIME: <time>            LAG_SOLI, default 0
!    XWAVEMAKER: <length>
!    SolitaryPositiveDirection: <bool>  default YES
!    Xc: <length>
!    Yc: <length>               default 0
!    WID: <length>
!    --- N-wave ---
!    x1_Nwave, x2_Nwave, a0_Nwave, gamma_Nwave, dep_Nwave
!    --- regular (WK_REG) ---
!    Tperiod: <time>
!    AMP_WK: <length>
!    Theta_WK: <angle>          default 0
!    --- multi-component (WK_TIME) ---
!    NumWaveComp: <int>
!    PeakPeriod: <time>
!    WaveCompFile: <path>
!    --- spectral ---
!    FreqPeak, FreqMin, FreqMax, Hmo, GammaTMA (default 3.3),
!    Nfreq (default 45), ThetaPeak, Ntheta (default 1),
!    Sigma_Theta, alpha_c (WK_NEW_IRR)
!    --- eta limiter (type-independent) ---
!    ETA_LIMITER: <bool>        default NO
!    CrestLimit, TroughLimit    required if ETA_LIMITER: YES
!    --- absorbing-generating (ABS / LEFT_BC_IRR) ---
!    WAVE_DATA_TYPE: <string>   default DATA_1D
!    DepthWaveMaker: <length>   fallback: DEP_WK
!    WidthWaveMaker: <length>
!    R_sponge_wavemaker, A_sponge_wavemaker
!    EqualEnergy: <bool>        default NO
!
!  All parameters are optional; the reader silently ignores absent keys.
!  This is a flat bridge module — a redesigned wavemaker module will
!  replace it once the wavemaker refactor is complete.
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_wavemaker_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_WAVEMAKER_A0_NWAVE, DEF_WAVEMAKER_ALPHA_C, &
                                        DEF_WAVEMAKER_AMP, DEF_WAVEMAKER_AMP_WK, &
                                        DEF_WAVEMAKER_A_SPONGE_WAVEMAKER, &
                                        DEF_WAVEMAKER_DELTA_WK, DEF_WAVEMAKER_DEP, &
                                        DEF_WAVEMAKER_DEPTHWAVEMAKER, &
                                        DEF_WAVEMAKER_DEP_NWAVE, DEF_WAVEMAKER_DEP_WK, &
                                        DEF_WAVEMAKER_EQUALENERGY, &
                                        DEF_WAVEMAKER_ETA_LIMITER, DEF_WAVEMAKER_FREQMAX, &
                                        DEF_WAVEMAKER_FREQMIN, DEF_WAVEMAKER_FREQPEAK, &
                                        DEF_WAVEMAKER_GAMMATMA, DEF_WAVEMAKER_GAMMA_NWAVE, &
                                        DEF_WAVEMAKER_HMO, DEF_WAVEMAKER_LAGTIME, &
                                        DEF_WAVEMAKER_NFREQ, DEF_WAVEMAKER_NTHETA, &
                                        DEF_WAVEMAKER_NUMWAVECOMP, DEF_WAVEMAKER_PEAKPERIOD, &
                                        DEF_WAVEMAKER_R_SPONGE_WAVEMAKER, &
                                        DEF_WAVEMAKER_SIGMA_THETA, &
                                        DEF_WAVEMAKER_SOLITARYPOSITIVEDIRECTION, &
                                        DEF_WAVEMAKER_THETAPEAK, DEF_WAVEMAKER_THETA_WK, &
                                        DEF_WAVEMAKER_TIME_RAMP, DEF_WAVEMAKER_TPERIOD, &
                                        DEF_WAVEMAKER_TYPE, DEF_WAVEMAKER_WAVEMAKERCD, &
                                        DEF_WAVEMAKER_WAVE_DATA_TYPE, DEF_WAVEMAKER_WID, &
                                        DEF_WAVEMAKER_WIDTHWAVEMAKER, &
                                        DEF_WAVEMAKER_X1_NWAVE, DEF_WAVEMAKER_X2_NWAVE, &
                                        DEF_WAVEMAKER_XC, DEF_WAVEMAKER_XC_WK, &
                                        DEF_WAVEMAKER_XWAVEMAKER, DEF_WAVEMAKER_YC, &
                                        DEF_WAVEMAKER_YC_WK, DEF_WAVEMAKER_YWIDTH_WK

   implicit none

   private
   public :: type_model_wavemaker
   public :: solitary_coefficients
   public :: wk_regular_coefficients

   type, extends(type_model_base) :: type_model_wavemaker

      character(:), allocatable :: wavemaker_type   ! YAML key: type
      character(:), allocatable :: WaveCompFile      ! YAML key: WaveCompFile
      character(:), allocatable :: WAVE_DATA_TYPE    ! YAML key: WAVE_DATA_TYPE

      ! Shared position / depth / ramp
      real(SP) :: Xc_WK = 0.0_SP
      real(SP) :: Yc_WK = 0.0_SP
      real(SP) :: DEP_WK = 0.0_SP
      real(SP) :: Time_ramp = 0.0_SP
      real(SP) :: Delta_WK = 0.5_SP
      real(SP) :: Ywidth_WK = 999999.0_SP   ! LARGE in old code

      ! Solitary wave — LEF_SOL, INI_SOL
      real(SP) :: AMP_SOLI = 0.0_SP   ! YAML key: AMP
      real(SP) :: DEP_SOLI = 0.0_SP   ! YAML key: DEP
      real(SP) :: LAG_SOLI = 0.0_SP   ! YAML key: LAGTIME
      real(SP) :: XWAVEMAKER = 0.0_SP
      logical  :: SolitaryPositiveDirection = .true.

      ! Initial condition wavemakers — INI_REC, INI_GAU, INI_DIP
      real(SP) :: Xc = 0.0_SP
      real(SP) :: Yc = 0.0_SP
      real(SP) :: WID = 0.0_SP

      ! N-wave — N_WAVE
      real(SP) :: x1_Nwave = 0.0_SP
      real(SP) :: x2_Nwave = 0.0_SP
      real(SP) :: a0_Nwave = 0.0_SP
      real(SP) :: gamma_Nwave = 0.0_SP
      real(SP) :: dep_Nwave = 0.0_SP

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
      real(SP) :: alpha_c = 0.0_SP   ! WK_NEW_IRR only

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

      ! Internal-source machinery (init_compute products; WK_REG so far)
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

   contains
      procedure :: read_input => wavemaker_read_input
      procedure :: init_compute => wavemaker_init_compute
      procedure :: apply_ic => wavemaker_apply_ic
      procedure :: update_source => wavemaker_update_source
      procedure :: fill_in_zone => wavemaker_fill_in_zone
      procedure :: free => wavemaker_free
   end type type_model_wavemaker

contains

   subroutine wavemaker_read_input(this, env)
      class(type_model_wavemaker), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_wm, no_key

      this%wavemaker_type = "nothing"
      sub_env = get_sub_env(env, "wavemaker", is_empty=no_wm)
      this%is_activated = .not. no_wm
      if (.not. this%is_activated) return

      call sub_env%yaml%read("type", val=this%wavemaker_type, default=DEF_WAVEMAKER_TYPE)

      ! Shared position / depth / ramp
      call sub_env%yaml%read("Xc_WK", silent=no_key, val=this%Xc_WK, default=DEF_WAVEMAKER_XC_WK)
      call sub_env%yaml%read("Yc_WK", silent=no_key, val=this%Yc_WK, default=DEF_WAVEMAKER_YC_WK)
      call sub_env%yaml%read("DEP_WK", silent=no_key, val=this%DEP_WK, default=DEF_WAVEMAKER_DEP_WK)
      call sub_env%yaml%read("Time_ramp", silent=no_key, val=this%Time_ramp, default=DEF_WAVEMAKER_TIME_RAMP)
      call sub_env%yaml%read("Delta_WK", silent=no_key, val=this%Delta_WK, default=DEF_WAVEMAKER_DELTA_WK)
      call sub_env%yaml%read("Ywidth_WK", silent=no_key, val=this%Ywidth_WK, default=DEF_WAVEMAKER_YWIDTH_WK)

      ! Solitary
      call sub_env%yaml%read("AMP", silent=no_key, val=this%AMP_SOLI, default=DEF_WAVEMAKER_AMP)
      call sub_env%yaml%read("DEP", silent=no_key, val=this%DEP_SOLI, default=DEF_WAVEMAKER_DEP)
      call sub_env%yaml%read("LAGTIME", silent=no_key, val=this%LAG_SOLI, default=DEF_WAVEMAKER_LAGTIME)
      call sub_env%yaml%read("XWAVEMAKER", silent=no_key, val=this%XWAVEMAKER, default=DEF_WAVEMAKER_XWAVEMAKER)
      call sub_env%yaml%read("SolitaryPositiveDirection", silent=no_key, &
                             val=this%SolitaryPositiveDirection, default=DEF_WAVEMAKER_SOLITARYPOSITIVEDIRECTION)

      ! Initial condition
      call sub_env%yaml%read("Xc", silent=no_key, val=this%Xc, default=DEF_WAVEMAKER_XC)
      call sub_env%yaml%read("Yc", silent=no_key, val=this%Yc, default=DEF_WAVEMAKER_YC)
      call sub_env%yaml%read("WID", silent=no_key, val=this%WID, default=DEF_WAVEMAKER_WID)

      ! N-wave
      call sub_env%yaml%read("x1_Nwave", silent=no_key, val=this%x1_Nwave, default=DEF_WAVEMAKER_X1_NWAVE)
      call sub_env%yaml%read("x2_Nwave", silent=no_key, val=this%x2_Nwave, default=DEF_WAVEMAKER_X2_NWAVE)
      call sub_env%yaml%read("a0_Nwave", silent=no_key, val=this%a0_Nwave, default=DEF_WAVEMAKER_A0_NWAVE)
      call sub_env%yaml%read("gamma_Nwave", silent=no_key, val=this%gamma_Nwave, default=DEF_WAVEMAKER_GAMMA_NWAVE)
      call sub_env%yaml%read("dep_Nwave", silent=no_key, val=this%dep_Nwave, default=DEF_WAVEMAKER_DEP_NWAVE)

      ! Regular wave
      call sub_env%yaml%read("Tperiod", silent=no_key, val=this%Tperiod, default=DEF_WAVEMAKER_TPERIOD)
      call sub_env%yaml%read("AMP_WK", silent=no_key, val=this%AMP_WK, default=DEF_WAVEMAKER_AMP_WK)
      call sub_env%yaml%read("Theta_WK", silent=no_key, val=this%Theta_WK, default=DEF_WAVEMAKER_THETA_WK)

      ! Multi-component time series
      call sub_env%yaml%read("NumWaveComp", silent=no_key, val=this%NumWaveComp, default=DEF_WAVEMAKER_NUMWAVECOMP)
      call sub_env%yaml%read("PeakPeriod", silent=no_key, val=this%PeakPeriod, default=DEF_WAVEMAKER_PEAKPERIOD)
      call sub_env%yaml%read("WaveCompFile", silent=no_key, val=this%WaveCompFile)

      ! Spectral
      call sub_env%yaml%read("FreqPeak", silent=no_key, val=this%FreqPeak, default=DEF_WAVEMAKER_FREQPEAK)
      call sub_env%yaml%read("FreqMin", silent=no_key, val=this%FreqMin, default=DEF_WAVEMAKER_FREQMIN)
      call sub_env%yaml%read("FreqMax", silent=no_key, val=this%FreqMax, default=DEF_WAVEMAKER_FREQMAX)
      call sub_env%yaml%read("Hmo", silent=no_key, val=this%Hmo, default=DEF_WAVEMAKER_HMO)
      call sub_env%yaml%read("GammaTMA", silent=no_key, val=this%GammaTMA, default=DEF_WAVEMAKER_GAMMATMA)
      call sub_env%yaml%read("Nfreq", silent=no_key, val=this%Nfreq, default=DEF_WAVEMAKER_NFREQ)
      call sub_env%yaml%read("ThetaPeak", silent=no_key, val=this%ThetaPeak, default=DEF_WAVEMAKER_THETAPEAK)
      call sub_env%yaml%read("Ntheta", silent=no_key, val=this%Ntheta, default=DEF_WAVEMAKER_NTHETA)
      call sub_env%yaml%read("Sigma_Theta", silent=no_key, val=this%Sigma_Theta, default=DEF_WAVEMAKER_SIGMA_THETA)
      call sub_env%yaml%read("alpha_c", silent=no_key, val=this%alpha_c, default=DEF_WAVEMAKER_ALPHA_C)

      ! Eta limiter
      call sub_env%yaml%read("ETA_LIMITER", val=this%ETA_LIMITER, default=DEF_WAVEMAKER_ETA_LIMITER)
      if (this%ETA_LIMITER) then
         call sub_env%yaml%read("CrestLimit", val=this%CrestLimit)
         call sub_env%yaml%read("TroughLimit", val=this%TroughLimit)
      end if

      ! Absorbing-generating
      call sub_env%yaml%read("WAVE_DATA_TYPE", val=this%WAVE_DATA_TYPE, default=DEF_WAVEMAKER_WAVE_DATA_TYPE)
      call sub_env%yaml%read("DepthWaveMaker", silent=no_key, val=this%DepthWaveMaker, default=DEF_WAVEMAKER_DEPTHWAVEMAKER)
      if (no_key) &
         call sub_env%yaml%read("DEP_WK", silent=no_key, val=this%DepthWaveMaker, default=DEF_WAVEMAKER_DEP_WK)
      call sub_env%yaml%read("WidthWaveMaker", silent=no_key, val=this%WidthWaveMaker, default=DEF_WAVEMAKER_WIDTHWAVEMAKER)
  call sub_env%yaml%read("R_sponge_wavemaker", silent=no_key, val=this%R_sponge_wavemaker, default=DEF_WAVEMAKER_R_SPONGE_WAVEMAKER)
  call sub_env%yaml%read("A_sponge_wavemaker", silent=no_key, val=this%A_sponge_wavemaker, default=DEF_WAVEMAKER_A_SPONGE_WAVEMAKER)
      call sub_env%yaml%read("EqualEnergy", val=this%EqualEnergy, default=DEF_WAVEMAKER_EQUALENERGY)

      ! WaveMakerCd presence enables WaveMakerCurrentBalance
      call sub_env%yaml%read("WaveMakerCd", silent=no_key, val=this%WaveMakerCd, default=DEF_WAVEMAKER_WAVEMAKERCD)
      this%WaveMakerCurrentBalance = .not. no_key

   end subroutine wavemaker_read_input

   ! ----------------------------------------------------------------
   ! Internal-source wavemaker setup (legacy WAVEMAKER_INITIALIZATION,
   ! old/wavemaker.F + the xmk_wk/zone block of old/init.F).  WK_REG
   ! only so far; spectral types join at their 6d rungs.  No-op for
   ! IC/boundary wavemaker types.
   !
   ! Under periodic-y the wave angle must fit an integer number of
   ! along-crest wavelengths in the domain: snap $\theta$ to the
   ! nearest admissible $\sin\theta = m\,\frac{2\pi}{k\,\Delta y\,(N_{glob}-1)}$
   ! (legacy loop, kept verbatim including the $N_{glob}-1$ measure).
   ! ----------------------------------------------------------------
   subroutine wavemaker_init_compute(this, grid, periodic, env)
      use core_grid_mod, only: type_grid_2d
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env

      integer :: i, j, mloc, nloc

      if (this%wavemaker_type /= "WK_REG") return
      this%has_mass_source = .true.

      ! legacy uses the scalar spacing (DXg) throughout the wavemaker
      if (grid%dx0 <= 0.0_SP .or. grid%dy0 <= 0.0_SP) &
         error stop "wavemaker: WK_REG requires uniform grid spacing"

      if (periodic .and. this%Theta_WK /= 0.0_SP) &
         call periodic_theta_snap(this, grid, env)

      call wk_regular_coefficients(this%Tperiod, this%AMP_WK, this%Theta_WK, &
                                   this%DEP_WK, this%Delta_WK, this%D_gen, &
                                   this%rlamda, this%Beta_gen, this%Width_WK)

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

   end subroutine wavemaker_init_compute

   ! ----------------------------------------------------------------
   ! Per-stage mass source refresh (legacy SourceTerms head,
   ! old/sources.F).  WK_REG:
   !   $$ M(x,y,t) = r(t)\,D\,e^{-\beta(x - x_c)^2}
   !                 \sin\!\big(\lambda y - \omega t\big), \qquad
   !      r(t) = \tanh\!\Big(\frac{\pi t}{\tau T}\Big) $$
   ! Cells outside the source box stay zero, which makes the
   ! unconditional adds in cal_rk_update/cal_sources identical to the
   ! legacy per-cell zone tests.  time is constant across RK stages
   ! (legacy TIME advances in ESTIMATE_DT), so the per-stage call
   ! recomputes the same values — kept legacy-shaped.
   ! FUTURE: hoist to once per step
   ! ----------------------------------------------------------------
   subroutine wavemaker_update_source(this, time)
      use core_constants_mod, only: PI
      class(type_model_wavemaker), intent(inout) :: this
      real(SP), intent(in) :: time

      real(SP) :: aa, ramp, omg
      integer :: i, j

      if (.not. this%has_mass_source) return

      ! legacy leans on IEEE tanh(inf) = 1 when Time_ramp = 0; guard
      ! gives the same value without the divide-by-zero
      ramp = 1.0_SP
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
      this%has_mass_source = .false.

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
      use core_constants_mod, only: PI, GRAV
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
   ! Initial-condition wavemakers: fill eta/u/v at t=0.
   ! Currently INI_SOLITARY only (legacy INITIAL_SOLITARY_WAVE,
   ! old/samples.F); INI_REC/INI_GAU/INI_DIP/N_WAVE to follow.
   ! No-op (still water) for source/BC wavemaker types.
   !
   ! WKN-B solitary solution (Wei & Kirby Boussinesq, Nwogu form):
   !   $$ \eta(\xi) = a_1\,\mathrm{sech}^2(B\xi) + a_2\,\mathrm{sech}^4(B\xi) $$
   !   $$ u(\xi)    = \pm a_u\,\mathrm{sech}^2(B\xi), \qquad v = 0 $$
   ! with $\xi$ measured from the crest at $x_{wm}$ (XWAVEMAKER).
   ! The legacy index form is preserved exactly: for ghost-inclusive
   ! local index i,
   !   $$ \xi = \big[(i_{beg}-1) + i - x_{wm}/\Delta x - 1\big]\Delta x $$
   ! which lands the crest at global ghost-inclusive index
   ! $x_{wm}/\Delta x + 1$, i.e. N_GHOST cells shoreward of interior
   ! $x = x_{wm}$ — kept for legacy parity.
   ! ----------------------------------------------------------------
   subroutine wavemaker_apply_ic(this, grid, eta, u, v)
      use core_grid_mod, only: type_grid_2d
      class(type_model_wavemaker), intent(in)  :: this
      type(type_grid_2d), intent(in)  :: grid
      real(SP), intent(out) :: eta(:, :), u(:, :), v(:, :)

      real(SP) :: c_ph, b, a1, a2, au, sc, usign
      integer  :: i, j

      eta = 0.0_SP
      u = 0.0_SP
      v = 0.0_SP

      if (this%wavemaker_type /= "INI_SOLITARY") return

      call solitary_coefficients(this%AMP_SOLI, this%DEP_SOLI, c_ph, b, a1, a2, au)

      usign = 1.0_SP
      if (.not. this%SolitaryPositiveDirection) usign = -1.0_SP

      do j = 1, grid%lp%nloc
         do i = 1, grid%lp%mloc
            sc = 1.0_SP/cosh(b*(real(grid%ibegin - 1 + i, SP) &
                                - this%XWAVEMAKER/grid%dx0 - 1.0_SP)*grid%dx0)
            eta(i, j) = a1*sc*sc + a2*sc*sc*sc*sc
            u(i, j) = usign*au*sc*sc
         end do
      end do

   end subroutine wavemaker_apply_ic

   ! ----------------------------------------------------------------
   ! Solitary-wave coefficients (legacy SUB_SLTRY, old/samples.F).
   ! For amplitude $a_0$, depth $h$, Nwogu reference-level parameter
   ! $\alpha$ ($\alpha_2 = \alpha + 1/3$, $\epsilon = a_0/h$), solve
   !   $$ x^3 + p x^2 + q x + r = 0, \qquad x > 1 $$
   !   $$ p = -\frac{\alpha_2 + 2\alpha(1+\epsilon)}{2\alpha}, \quad
   !      q = \frac{\epsilon\,\alpha_2}{\alpha}, \quad
   !      r = \frac{\alpha_2}{2\alpha} $$
   ! by Newton iteration from $x = 1.2$.  Then, with $c = \sqrt{gh}$:
   !   $$ C_{ph} = c\sqrt{x}, \qquad
   !      a_u = \frac{(x-1)\,c}{\sqrt{x}}, \qquad
   !      B = \frac{1}{h}\sqrt{\frac{x-1}{4(\alpha_2 - \alpha x)}} $$
   !   $$ a_1 = \frac{(x-1)}{3\epsilon\,(\alpha_2 - \alpha x)}\,a_0, \qquad
   !      a_2 = -\frac{(x-1)^2\,(2\alpha x + \alpha_2)}
   !                  {2\epsilon\,x\,(\alpha_2 - \alpha x)}\,a_0 $$
   ! alpha is fixed at -0.39: legacy notes that the analytic
   ! $\alpha = \beta^2/2 + \beta$ with $\beta = -0.531$ mismatches the
   ! wave shape and keeps -0.39 empirically.
   ! ----------------------------------------------------------------
   subroutine solitary_coefficients(amp, dep, c_ph, b, a1, a2, au)
      use core_constants_mod, only: GRAV
      real(SP), intent(in)  :: amp, dep
      real(SP), intent(out) :: c_ph, b, a1, a2, au

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: alp2, eps, p, q, r, x, fx, fpx, rx, cph
      integer  :: ite

      alp2 = alpha + 1.0_SP/3.0_SP
      eps = amp/dep

      p = -(alp2 + 2.0_SP*alpha*(1.0_SP + eps))/(2.0_SP*alpha)
      q = eps*alp2/alpha
      r = alp2/(2.0_SP*alpha)

      x = 1.2_SP
      do ite = 1, 10
         fx = r + x*(q + x*(p + x))
         fpx = q + x*(2.0_SP*p + 3.0_SP*x)
         x = x - fx/fpx
         if (abs(fx) < 1e-5_SP) exit
      end do
      if (abs(fx) >= 1e-5_SP) then
         error stop "wavemaker: no solitary wave solution (check eps = AMP/DEP)"
      end if

      rx = sqrt(x)
      cph = sqrt(GRAV*dep)
      c_ph = rx*cph

      au = (x - 1.0_SP)/(eps*rx)*cph*eps
      b = sqrt((x - 1.0_SP)/(4.0_SP*(alp2 - alpha*x)))/dep
      a1 = (x - 1.0_SP)/(eps*3.0_SP*(alp2 - alpha*x))*amp
      a2 = -(x - 1.0_SP)/(2.0_SP*eps)*(x - 1.0_SP)*(2.0_SP*alpha*x + alp2) &
           /(x*(alp2 - alpha*x))*amp

   end subroutine solitary_coefficients

   ! ----------------------------------------------------------------
   ! Wei & Kirby internal-source coefficients for a regular wave
   ! (legacy WK_WAVEMAKER_REGULAR_WAVE, old/wavemaker.F).  With
   ! $\alpha = -0.39$, $\alpha_1 = \alpha + 1/3$, $\omega = 2\pi/T$:
   ! wavenumber from the Nwogu dispersion relation
   !   $$ (kh)^2 = \frac{t_c - \sqrt{t_c^2 - 4\alpha_1 t_b}}{2\alpha_1},
   !      \qquad t_b = \frac{\omega^2 h}{g},\ \ t_c = 1 + \alpha\,t_b $$
   ! then, with wavelength $L = C_p T$ and source width parameter
   ! $\delta$:
   !   $$ \lambda = k\sin\theta, \qquad W = \frac{\delta L}{2}, \qquad
   !      \beta = \frac{80}{\delta^2 L^2} $$
   !   $$ I = \sqrt{\pi/\beta}\;e^{-l^2/4\beta}, \qquad l = k\cos\theta $$
   !   $$ D = \frac{2 a \cos\theta\,(\omega^2 - \alpha_1 g k^4 h^3)}
   !               {\omega k I \left(1 - \alpha (kh)^2\right)} $$
   ! ----------------------------------------------------------------
   subroutine wk_regular_coefficients(Tperiod, amp, theta_deg, h_gen, delta, &
                                      D_gen, rlamda, beta_gen, width)
      use core_constants_mod, only: GRAV, PI
      real(SP), intent(in)  :: Tperiod, amp, theta_deg, h_gen, delta
      real(SP), intent(out) :: D_gen, rlamda, beta_gen, width

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: alpha1, theta, omgn, tb, tc, wkn, c_phase, wave_length
      real(SP) :: rl_gen, ri

      if (h_gen == 0.0_SP .or. Tperiod == 0.0_SP) &
         error stop "wavemaker: re-set depth, Tperiod for wavemaker"

      alpha1 = alpha + 1.0_SP/3.0_SP
      theta = theta_deg*PI/180.0_SP
      omgn = 2.0_SP*PI/Tperiod

      tb = omgn*omgn*h_gen/GRAV
      tc = 1.0_SP + tb*alpha
      wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                 /(2.0_SP*alpha1))/h_gen
      c_phase = 1.0_SP/wkn/Tperiod*2.0_SP*PI
      wave_length = c_phase*Tperiod

      rlamda = wkn*sin(theta)
      width = delta*wave_length/2.0_SP
      beta_gen = 80.0_SP/delta**2/wave_length**2
      rl_gen = wkn*cos(theta)
      ! legacy uses the truncated literal 3.14159 here (not pi) — kept
      ri = sqrt(3.14159_SP/beta_gen)*exp(-rl_gen**2/4.0_SP/beta_gen)

      D_gen = 2.0_SP*amp &
              *cos(theta)*(omgn**2 - alpha1*GRAV*wkn**4*h_gen**3) &
              /(omgn*wkn*ri*(1.0_SP - alpha*(wkn*h_gen)**2))

   end subroutine wk_regular_coefficients

end module model_wavemaker_mod
