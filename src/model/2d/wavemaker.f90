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
!      array each stage (WK_REG + WK_IRR/TMA/JON live; WK_TIME and
!      WK_NEW_* pending).
!    * boundary types (ABS, LEFT_BC_IRR, LEF_SOL): own the west ghost
!      strip each step — the BC service must skip the wall mirror there
!      (fill_west=.false. in kernel_bc).  ABS/LEFT_BC_IRR with the TMA/
!      JON spectrum live (apply_boundary per stage); the DATA
!      (WaveCompFile) spectrum and LEF_SOL are pending.
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
      real(SP) :: T_brk = 0.0_SP                      ! breaking-age override 1/FreqMax; 0 = none

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

   contains
      procedure :: read_input => wavemaker_read_input
      procedure :: init_compute => wavemaker_init_compute
      procedure :: apply_ic => wavemaker_apply_ic
      procedure :: update_source => wavemaker_update_source
      procedure :: apply_boundary => wavemaker_apply_boundary
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
   ! and the spectral family (WK_IRR/TMA_1D/JON_1D/JON_2D); remaining
   ! source types join at their 6d rungs.  No-op for IC/boundary
   ! wavemaker types.
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

      select case (this%wavemaker_type)
      case ("WK_REG")
         this%has_mass_source = .true.
      case ("WK_IRR", "TMA_1D", "JON_1D", "JON_2D")
         this%has_mass_source = .true.
         this%spectral_source = .true.
      case ("ABS")
         this%abs_source = .true.
      case ("LEFT_BC_IRR")
         this%left_bc_source = .true.
      case default
         return
      end select

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
         call spectral_init_compute(this, grid, periodic, env)
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

   end subroutine wavemaker_init_compute

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

      real(SP) :: bb(this%Nfreq), cc(this%Nfreq)
      real(SP) :: aa, ramp, omg, wk_source
      integer :: i, j, kf

      if (.not. this%has_mass_source) return

      ! legacy leans on IEEE tanh(inf) = 1 when Time_ramp = 0; guard
      ! gives the same value without the divide-by-zero
      ramp = 1.0_SP

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
   ! them out), hu/hv rebuilt everywhere.  NOTE: this is the original
   ! SpongeMaker form — the vendored legacy's Salatin-2021 rewrite
   ! reads the TIDE module's SPONGE_TIDE_WEST, which is UNALLOCATED
   ! without tidal BC flags (upstream bug; no legacy parity possible),
   ! and drops the phases from the time factors.
   ! ----------------------------------------------------------------
   subroutine wavemaker_apply_boundary(this, grid, istage, dt, time, &
                                       eta, u, v, hu, hv, depth)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: PI, N_GHOST
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
      if (allocated(this%Cm)) deallocate (this%Cm, this%Sm, this%omgn_ir)
      if (allocated(this%Cm_eta)) &
         deallocate (this%Cm_eta, this%Sm_eta, this%Cm_u, this%Sm_u, &
                     this%Cm_v, this%Sm_v, this%Segma_Ser, this%Phase_Ser)
      if (allocated(this%sponge_maker)) deallocate (this%sponge_maker)
      this%has_mass_source = .false.
      this%spectral_source = .false.
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
   ! Private: spectral internal-source setup (legacy WK_IRR block of
   ! WAVEMAKER_INITIALIZATION, old/wavemaker.F).  Builds per-component
   ! generation parameters, collapses them into the dense spatial modes
   !   $$ C_m(x,y) = \sum_\theta D\,e^{-\beta(x - x_c)^2}
   !                 \cos\!\big(\lambda y + \phi\big), \qquad
   !      S_m(x,y) = \sum_\theta D\,e^{-\beta(x - x_c)^2}
   !                 \sin\!\big(\lambda y + \phi\big) $$
   ! (legacy CALCULATE_Cm_Sm, ghost-inclusive), and frees the
   ! per-component temporaries.  T_brk override 1/FreqMax matches the
   ! legacy SHOW_BREAKING assignment.
   ! ----------------------------------------------------------------
   subroutine spectral_init_compute(this, grid, periodic, env)
      use core_grid_mod, only: type_grid_2d
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env

      real(SP), allocatable :: D_gen_ir(:, :), rlamda_ir(:, :), phase_ir(:, :)
      real(SP), allocatable :: beta_gen_ir(:)
      logical :: is_jonswap
      integer :: i, j, kf, ktheta, mloc, nloc

      mloc = grid%lp%mloc
      nloc = grid%lp%nloc
      allocate (D_gen_ir(this%Nfreq, this%Ntheta), &
                rlamda_ir(this%Nfreq, this%Ntheta), &
                phase_ir(this%Nfreq, this%Ntheta), &
                beta_gen_ir(this%Nfreq), this%omgn_ir(this%Nfreq), &
                this%Cm(mloc, nloc, this%Nfreq), this%Sm(mloc, nloc, this%Nfreq))

      ! legacy TMA phi factor drops for the pure JONSWAP types
      is_jonswap = this%wavemaker_type(1:3) == "JON"

      call wk_irregular_coefficients(this%EqualEnergy, is_jonswap, this%Nfreq, &
                                     this%Ntheta, this%Delta_WK, this%DEP_WK, &
                                     this%FreqPeak, this%FreqMax, this%FreqMin, &
                                     this%GammaTMA, this%Hmo, this%ThetaPeak, &
                                     this%Sigma_Theta, periodic, grid%dy0, grid%N, &
                                     env, rlamda_ir, beta_gen_ir, D_gen_ir, &
                                     phase_ir, this%Width_WK, this%omgn_ir)

      ! per-element accumulation over ktheta matches the legacy
      ! summation order; x/y enter via the shared wavemaker frame
      this%Cm = 0.0_SP
      this%Sm = 0.0_SP
      do kf = 1, this%Nfreq
         do j = 1, nloc
            do i = 1, mloc
               do ktheta = 1, this%Ntheta
                  this%Cm(i, j, kf) = this%Cm(i, j, kf) + D_gen_ir(kf, ktheta) &
                                      *exp(-beta_gen_ir(kf)*(this%xmk_wk(i) - this%Xc_WK)**2) &
                                      *cos(rlamda_ir(kf, ktheta)*this%ymk_wk(j) &
                                           + phase_ir(kf, ktheta))
                  this%Sm(i, j, kf) = this%Sm(i, j, kf) + D_gen_ir(kf, ktheta) &
                                      *exp(-beta_gen_ir(kf)*(this%xmk_wk(i) - this%Xc_WK)**2) &
                                      *sin(rlamda_ir(kf, ktheta)*this%ymk_wk(j) &
                                           + phase_ir(kf, ktheta))
               end do
            end do
         end do
      end do

      this%T_brk = 1.0_SP/this%FreqMax

   end subroutine spectral_init_compute

   ! ----------------------------------------------------------------
   ! Private: boundary wavemaker setup (legacy ABS / LEFT_BC_IRR block
   ! of WAVEMAKER_INITIALIZATION + init.F CALCULATE_SPONGE_MAKER).
   ! Builds the six dense series modes at the linear-theory reference
   ! level $z = |1 + \beta_{ref}|\,h_s$ (legacy CALCULATE_TMA_Cm_Sm[_
   ! EQUAL_DFREQ]); ABS additionally builds the relaxation sponge.
   ! WAVE_DATA_TYPE = DATA (WaveCompFile 2D spectrum) is a later rung.
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

      logical :: is_jonswap
      integer :: mloc, nloc

      if (len(this%WAVE_DATA_TYPE) >= 4) then
         if (this%WAVE_DATA_TYPE(1:4) == "DATA") &
            error stop "wavemaker: WAVE_DATA_TYPE DATA (WaveCompFile) not yet ported"
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

      ! legacy keys the JONSWAP switch off WAVE_DATA_TYPE here, not
      ! the wavemaker name
      is_jonswap = .false.
      if (len(this%WAVE_DATA_TYPE) >= 3) &
         is_jonswap = this%WAVE_DATA_TYPE(1:3) == "JON"

      call tma_series_coefficients(this, grid, periodic, env, is_jonswap, &
                                   beta_ref)

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
      use core_constants_mod, only: GRAV, PI, SMALL
      use core_build_config_mod, only: BUILD_ZERO_PHASE
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic, is_jonswap
      type(type_env), intent(inout) :: env
      real(SP), intent(in) :: beta_ref

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: freq(this%Nfreq), energy_bin(this%Nfreq)
      real(SP) :: wkn(this%Nfreq), theta(this%Ntheta), ag(this%Ntheta)
      real(SP) :: amp(this%Nfreq, this%Ntheta)
      real(SP) :: Ef, alpha_spec, alpha1, tb, tc, h_ser, zlev
      real(SP) :: theta_per, transfer, arg
      integer :: kf, ktheta, i, j

      h_ser = this%DepthWaveMaker
      if (h_ser == 0.0_SP .or. this%FreqPeak == 0.0_SP .or. this%FreqMax == 0.0_SP) &
         error stop "wavemaker: re-set DepthWaveMaker, FreqPeak, FreqMax for wavemaker"

      if (BUILD_ZERO_PHASE) then
         this%Phase_Ser = 0.0_SP
      else
         call random_number(this%Phase_Ser)
         this%Phase_Ser = this%Phase_Ser*2.0_SP*PI
      end if

      if (this%EqualEnergy) then
         call freq_bins_equal_energy(is_jonswap, this%Nfreq, h_ser, &
                                     this%FreqPeak, this%FreqMax, this%FreqMin, &
                                     this%GammaTMA, freq, energy_bin, Ef)
      else
         call freq_bins_equal_dfreq(is_jonswap, this%Nfreq, h_ser, &
                                    this%FreqPeak, this%FreqMax, this%FreqMin, &
                                    this%GammaTMA, freq, energy_bin, Ef)
      end if

      call directional_spreading(this%Ntheta, this%ThetaPeak, this%Sigma_Theta, ag)

      alpha_spec = this%Hmo**2/16.0_SP/Ef
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
      use core_constants_mod, only: PI, SMALL
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

   ! ----------------------------------------------------------------
   ! Wei & Kirby internal-source coefficients for a TMA/JONSWAP
   ! spectrum (legacy WK_EQUAL_DFREQ_IRREGULAR_WAVE, default, and
   ! WK_WAVEMAKER_IRREGULAR_WAVE for EqualEnergy, old/wavemaker.F).
   ! The two variants differ only in the frequency bins and per-bin
   ! energy; the directional spreading and the per-component
   ! generation solve are shared, element-identical to legacy.
   ! Component amplitude from the bin energy and spreading weight:
   !   $$ a_{f\theta} = \frac{4}{2\sqrt 2}
   !      \sqrt{\alpha_s\,E_f\,G_\theta}, \qquad
   !      \alpha_s = \frac{H_{m0}^2}{16\,E} $$
   ! then per component the same Nwogu dispersion solve and source
   ! magnitude $D$ as the regular wave, with $\beta$ built from the
   ! PEAK-frequency wavelength (Wei & Kirby 1999 suggestion).
   ! Legacy quirks kept: width uses the LAST component's wave_length
   ! (parity-ledger candidate), and the periodic snap caps at pi/2
   ! with a snap-DOWN fallback and no error stop.
   ! ----------------------------------------------------------------
   subroutine wk_irregular_coefficients(equal_energy, is_jonswap, nfreq, ntheta, &
                                        delta, h_gen, fm, fmax, fmin, gamma_spec, &
                                        Hmo, theta_peak, sigma_theta_deg, periodic, &
                                        dy, nglob, env, rlamda, beta_gen, D_gen, &
                                        phi1, width, omgn)
      use core_constants_mod, only: GRAV, PI, SMALL
      use core_build_config_mod, only: BUILD_ZERO_PHASE
      logical, intent(in)  :: equal_energy, is_jonswap, periodic
      integer, intent(in)  :: nfreq, ntheta, nglob
      real(SP), intent(in) :: delta, h_gen, fm, fmax, fmin, gamma_spec, Hmo, &
                              theta_peak, sigma_theta_deg, dy
      type(type_env), intent(inout) :: env
      real(SP), intent(out) :: rlamda(nfreq, ntheta), beta_gen(nfreq)
      real(SP), intent(out) :: D_gen(nfreq, ntheta), phi1(nfreq, ntheta)
      real(SP), intent(out) :: width, omgn(nfreq)

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: freq(nfreq), energy_bin(nfreq), ag(ntheta)
      real(SP) :: Ef, alpha_spec, ap, theta, alpha1, tb, tc, wkn
      real(SP) :: c_phase, wave_length, rl_gen, ri, snap_scratch
      integer :: kf, ktheta

      if (h_gen == 0.0_SP .or. fm == 0.0_SP .or. fmax == 0.0_SP) &
         error stop "wavemaker: re-set depth, FreqPeak, FreqMax for wavemaker"

      if (equal_energy) then
         call freq_bins_equal_energy(is_jonswap, nfreq, h_gen, fm, fmax, fmin, &
                                     gamma_spec, freq, energy_bin, Ef)
      else
         call freq_bins_equal_dfreq(is_jonswap, nfreq, h_gen, fm, fmax, fmin, &
                                    gamma_spec, freq, energy_bin, Ef)
      end if

      call directional_spreading(ntheta, theta_peak, sigma_theta_deg, ag)

      alpha_spec = Hmo**2/16.0_SP/Ef
      alpha1 = alpha + 1.0_SP/3.0_SP
      wave_length = 0.0_SP

      ! legacy PARAM scratch is static: a theta = 0 component under
      ! periodic reuses the previous component's snapped value
      snap_scratch = 0.0_SP

      do kf = 1, nfreq
         do ktheta = 1, ntheta

            ! legacy folds the Hmo -> Hrms conversion into the half
            ! amplitude: a = H_each / (2 sqrt 2)
            ap = 4.0_SP*sqrt(alpha_spec*energy_bin(kf)*ag(ktheta)) &
                 /sqrt(2.0_SP)/2.0_SP

            if (ntheta == 1) then
               theta = theta_peak*PI/180.0_SP
            else
               theta = -PI/3.0_SP + theta_peak*PI/180.0_SP &
                       + 2.0_SP/3.0_SP*PI/(real(ntheta, SP) - 1.0_SP) &
                       *(real(ktheta, SP) - 1.0_SP)
               if (theta > 0.5_SP*PI) theta = 0.5_SP*PI
               if (theta < -0.5_SP*PI) theta = -0.5_SP*PI
            end if

            omgn(kf) = 2.0_SP*PI*freq(kf)
            tb = omgn(kf)*omgn(kf)*h_gen/GRAV
            tc = 1.0_SP + tb*alpha
            wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                       /(2.0_SP*alpha1))/h_gen

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

            if (periodic) &
               call spectral_periodic_snap(theta, snap_scratch, wkn, dy, &
                                           nglob, freq(kf), env)

            rlamda(kf, ktheta) = wkn*sin(theta)
            beta_gen(kf) = 80.0_SP/delta**2/wave_length**2
            rl_gen = wkn*cos(theta)
            ri = sqrt(PI/beta_gen(kf))*exp(-rl_gen**2/4.0_SP/beta_gen(kf))

            D_gen(kf, ktheta) = 2.0_SP*ap*cos(theta) &
                                *(omgn(kf)**2 - alpha1*GRAV*wkn**4*h_gen**3) &
                                /(omgn(kf)*wkn*ri*(1.0_SP - alpha*(wkn*h_gen)**2))

         end do
      end do

      ! legacy recomputes the peak wavenumber here but the width still
      ! uses the last component's wave_length — kept bug-for-bug
      width = delta*wave_length/2.0_SP

      ! parity builds fix all phases to zero; the random path uses the
      ! standard generator (legacy rand()/rand(0) is compiler-specific
      ! and never reproducible anyway)
      if (BUILD_ZERO_PHASE) then
         phi1 = 0.0_SP
      else
         call random_number(phi1)
         phi1 = phi1*2.0_SP*PI
      end if

   end subroutine wk_irregular_coefficients

   ! ----------------------------------------------------------------
   ! Private: uniform frequency bins (legacy WK_EQUAL_DFREQ_IRREGULAR_
   ! WAVE head): $f_k = f_{min} + (k-1)\,df$, $df = \frac{f_{max}-f_{min}}
   ! {N_f - 1}$, bin energy $E_k = S(f_k)\,df$.
   ! ----------------------------------------------------------------
   subroutine freq_bins_equal_dfreq(is_jonswap, nfreq, h_gen, fm, fmax, fmin, &
                                    gamma_spec, freq, energy_bin, Ef)
      logical, intent(in)  :: is_jonswap
      integer, intent(in)  :: nfreq
      real(SP), intent(in) :: h_gen, fm, fmax, fmin, gamma_spec
      real(SP), intent(out) :: freq(nfreq), energy_bin(nfreq), Ef

      real(SP) :: df
      integer :: kff

      df = (fmax - fmin)/(real(nfreq, SP) - 1.0_SP)
      Ef = 0.0_SP
      do kff = 1, nfreq
         freq(kff) = fmin + real(kff - 1, SP)*df
         energy_bin(kff) = tma_density(is_jonswap, freq(kff), fm, h_gen, &
                                       gamma_spec)*df
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
   subroutine freq_bins_equal_energy(is_jonswap, nfreq, h_gen, fm, fmax, fmin, &
                                     gamma_spec, freq, energy_bin, Ef)
      integer, parameter :: NSCAN = 10000
      logical, intent(in)  :: is_jonswap
      integer, intent(in)  :: nfreq
      real(SP), intent(in) :: h_gen, fm, fmax, fmin, gamma_spec
      real(SP), intent(out) :: freq(nfreq), energy_bin(nfreq), Ef

      real(SP) :: ef_scan(NSCAN)
      real(SP) :: fre, ef_bin, ef_add
      integer :: k, kf, kff, kb

      Ef = 0.0_SP
      do kf = 1, NSCAN
         fre = fmin + (fmax - fmin)/real(NSCAN, SP)*(real(kf, SP) - 1.0_SP)
         ef_scan(kf) = tma_density(is_jonswap, fre, fm, h_gen, gamma_spec)
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
      use core_constants_mod, only: GRAV, PI
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

   ! ----------------------------------------------------------------
   ! Private: wrapped-normal directional spreading (Borgman 1984;
   ! legacy ykchoi 11/07/2016 block).  Bins span $\theta_p \pm \pi/3$,
   ! clamped to $\pm\pi/2$:
   !   $$ G(\theta) = \frac{1}{2\pi} + \frac{1}{\pi}\sum_{n=1}^{N}
   !      e^{-\frac{(n\sigma_\theta)^2}{2}}\cos\!\big(n(\theta-\theta_p)\big),
   !      \qquad N = \lfloor 20/\sigma_\theta \rfloor $$
   ! normalized by the (signed) sum then made positive (legacy ABS).
   ! N is computed inside the ntheta > 1 branch — legacy evaluates
   ! 20/sigma before its 1D branch, dividing by zero when sigma = 0;
   ! the value is unused there, so the guard is unobservable.
   ! ----------------------------------------------------------------
   subroutine directional_spreading(ntheta, theta_peak, sigma_theta_deg, ag)
      use core_constants_mod, only: PI
      integer, intent(in)  :: ntheta
      real(SP), intent(in) :: theta_peak, sigma_theta_deg
      real(SP), intent(out) :: ag(ntheta)

      real(SP) :: sigma_theta, theta, sum_ag
      integer :: ktheta, k_n, n_spec

      if (ntheta == 1) then
         ag(1) = 1.0_SP
         return
      end if

      sigma_theta = sigma_theta_deg*PI/180.0_SP
      n_spec = int(20.0_SP/sigma_theta)

      sum_ag = 0.0_SP
      do ktheta = 1, ntheta
         theta = -PI/3.0_SP + theta_peak*PI/180.0_SP &
                 + 2.0_SP/3.0_SP*PI/(real(ntheta, SP) - 1.0_SP) &
                 *(real(ktheta, SP) - 1.0_SP)
         if (theta > 0.5_SP*PI) theta = 0.5_SP*PI
         if (theta < -0.5_SP*PI) theta = -0.5_SP*PI

         ag(ktheta) = 1.0_SP/(2.0_SP*PI)
         do k_n = 1, n_spec
            ag(ktheta) = ag(ktheta) &
                         + (1.0_SP/PI)*exp(-0.5_SP*(real(k_n, SP)*sigma_theta)**2) &
                         *cos(real(k_n, SP)*(theta - theta_peak*PI/180.0_SP))
         end do
         sum_ag = sum_ag + ag(ktheta)
      end do
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
      use core_constants_mod, only: PI
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
