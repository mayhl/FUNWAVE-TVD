!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Wavemaker parameters YAML reader (bridge)
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

   type, extends(type_model_base) :: type_model_wavemaker

      character(:), allocatable :: wavemaker_type   ! YAML key: type
      character(:), allocatable :: WaveCompFile      ! YAML key: WaveCompFile
      character(:), allocatable :: WAVE_DATA_TYPE    ! YAML key: WAVE_DATA_TYPE

      ! Shared position / depth / ramp
      real(SP) :: Xc_WK     = 0.0_SP
      real(SP) :: Yc_WK     = 0.0_SP
      real(SP) :: DEP_WK    = 0.0_SP
      real(SP) :: Time_ramp = 0.0_SP
      real(SP) :: Delta_WK  = 0.5_SP
      real(SP) :: Ywidth_WK = 999999.0_SP   ! LARGE in old code

      ! Solitary wave — LEF_SOL, INI_SOL
      real(SP) :: AMP_SOLI   = 0.0_SP   ! YAML key: AMP
      real(SP) :: DEP_SOLI   = 0.0_SP   ! YAML key: DEP
      real(SP) :: LAG_SOLI   = 0.0_SP   ! YAML key: LAGTIME
      real(SP) :: XWAVEMAKER = 0.0_SP
      logical  :: SolitaryPositiveDirection = .true.

      ! Initial condition wavemakers — INI_REC, INI_GAU, INI_DIP
      real(SP) :: Xc  = 0.0_SP
      real(SP) :: Yc  = 0.0_SP
      real(SP) :: WID = 0.0_SP

      ! N-wave — N_WAVE
      real(SP) :: x1_Nwave    = 0.0_SP
      real(SP) :: x2_Nwave    = 0.0_SP
      real(SP) :: a0_Nwave    = 0.0_SP
      real(SP) :: gamma_Nwave = 0.0_SP
      real(SP) :: dep_Nwave   = 0.0_SP

      ! Regular wave — WK_REG
      real(SP) :: Tperiod  = 0.0_SP
      real(SP) :: AMP_WK   = 0.0_SP
      real(SP) :: Theta_WK = 0.0_SP

      ! Multi-component time series — WK_TIME
      integer  :: NumWaveComp = 1
      real(SP) :: PeakPeriod  = 0.0_SP

      ! Spectral — WK_IRR, TMA_1D, JON_1D, JON_2D, WK_NEW_IRR, WK_NEW_DATA2D
      real(SP) :: FreqPeak    = 0.0_SP
      real(SP) :: FreqMin     = 0.0_SP
      real(SP) :: FreqMax     = 0.0_SP
      real(SP) :: Hmo         = 0.0_SP
      real(SP) :: GammaTMA    = 3.3_SP
      integer  :: Nfreq       = 45
      real(SP) :: ThetaPeak   = 0.0_SP
      integer  :: Ntheta      = 1
      real(SP) :: Sigma_Theta = 0.0_SP
      real(SP) :: alpha_c     = 0.0_SP   ! WK_NEW_IRR only

      ! Eta limiter (type-independent)
      logical  :: ETA_LIMITER  = .false.
      real(SP) :: CrestLimit   = 0.0_SP
      real(SP) :: TroughLimit  = 0.0_SP

      ! Absorbing-generating — ABS, LEFT_BC_IRR
      real(SP) :: DepthWaveMaker    = 0.0_SP   ! DepthWaveMaker / DEP_WK fallback → DEP_Ser
      real(SP) :: WidthWaveMaker    = 0.0_SP
      real(SP) :: R_sponge_wavemaker = 0.0_SP
      real(SP) :: A_sponge_wavemaker = 0.0_SP
      logical  :: EqualEnergy        = .false.

      ! Wavemaker current balance — presence of WaveMakerCd enables balance
      logical  :: WaveMakerCurrentBalance = .false.
      real(SP) :: WaveMakerCd             = 0.0_SP

   contains
      procedure :: read_input => wavemaker_read_input
   end type type_model_wavemaker

contains

   subroutine wavemaker_read_input(this, env)
      class(type_model_wavemaker), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_wm, no_key

      this%wavemaker_type = 'nothing'
      sub_env = get_sub_env(env, 'wavemaker', is_empty=no_wm)
      this%is_activated = .not. no_wm
      if (.not. this%is_activated) return

      call sub_env%yaml%read('type', val=this%wavemaker_type, default=DEF_WAVEMAKER_TYPE)

      ! Shared position / depth / ramp
      call sub_env%yaml%read('Xc_WK',     silent=no_key, val=this%Xc_WK,     default=DEF_WAVEMAKER_XC_WK)
      call sub_env%yaml%read('Yc_WK',     silent=no_key, val=this%Yc_WK,     default=DEF_WAVEMAKER_YC_WK)
      call sub_env%yaml%read('DEP_WK',    silent=no_key, val=this%DEP_WK,    default=DEF_WAVEMAKER_DEP_WK)
      call sub_env%yaml%read('Time_ramp', silent=no_key, val=this%Time_ramp,  default=DEF_WAVEMAKER_TIME_RAMP)
      call sub_env%yaml%read('Delta_WK',  silent=no_key, val=this%Delta_WK,   default=DEF_WAVEMAKER_DELTA_WK)
      call sub_env%yaml%read('Ywidth_WK', silent=no_key, val=this%Ywidth_WK,  default=DEF_WAVEMAKER_YWIDTH_WK)

      ! Solitary
      call sub_env%yaml%read('AMP',                       silent=no_key, val=this%AMP_SOLI,  default=DEF_WAVEMAKER_AMP)
      call sub_env%yaml%read('DEP',                       silent=no_key, val=this%DEP_SOLI,  default=DEF_WAVEMAKER_DEP)
      call sub_env%yaml%read('LAGTIME',                   silent=no_key, val=this%LAG_SOLI,  default=DEF_WAVEMAKER_LAGTIME)
      call sub_env%yaml%read('XWAVEMAKER',                silent=no_key, val=this%XWAVEMAKER, default=DEF_WAVEMAKER_XWAVEMAKER)
      call sub_env%yaml%read('SolitaryPositiveDirection', silent=no_key, &
                              val=this%SolitaryPositiveDirection, default=DEF_WAVEMAKER_SOLITARYPOSITIVEDIRECTION)

      ! Initial condition
      call sub_env%yaml%read('Xc',  silent=no_key, val=this%Xc,  default=DEF_WAVEMAKER_XC)
      call sub_env%yaml%read('Yc',  silent=no_key, val=this%Yc,  default=DEF_WAVEMAKER_YC)
      call sub_env%yaml%read('WID', silent=no_key, val=this%WID, default=DEF_WAVEMAKER_WID)

      ! N-wave
      call sub_env%yaml%read('x1_Nwave',    silent=no_key, val=this%x1_Nwave,    default=DEF_WAVEMAKER_X1_NWAVE)
      call sub_env%yaml%read('x2_Nwave',    silent=no_key, val=this%x2_Nwave,    default=DEF_WAVEMAKER_X2_NWAVE)
      call sub_env%yaml%read('a0_Nwave',    silent=no_key, val=this%a0_Nwave,    default=DEF_WAVEMAKER_A0_NWAVE)
      call sub_env%yaml%read('gamma_Nwave', silent=no_key, val=this%gamma_Nwave, default=DEF_WAVEMAKER_GAMMA_NWAVE)
      call sub_env%yaml%read('dep_Nwave',   silent=no_key, val=this%dep_Nwave,   default=DEF_WAVEMAKER_DEP_NWAVE)

      ! Regular wave
      call sub_env%yaml%read('Tperiod',  silent=no_key, val=this%Tperiod,  default=DEF_WAVEMAKER_TPERIOD)
      call sub_env%yaml%read('AMP_WK',   silent=no_key, val=this%AMP_WK,   default=DEF_WAVEMAKER_AMP_WK)
      call sub_env%yaml%read('Theta_WK', silent=no_key, val=this%Theta_WK, default=DEF_WAVEMAKER_THETA_WK)

      ! Multi-component time series
      call sub_env%yaml%read('NumWaveComp',  silent=no_key, val=this%NumWaveComp, default=DEF_WAVEMAKER_NUMWAVECOMP)
      call sub_env%yaml%read('PeakPeriod',   silent=no_key, val=this%PeakPeriod,  default=DEF_WAVEMAKER_PEAKPERIOD)
      call sub_env%yaml%read('WaveCompFile', silent=no_key, val=this%WaveCompFile)

      ! Spectral
      call sub_env%yaml%read('FreqPeak',    silent=no_key, val=this%FreqPeak,    default=DEF_WAVEMAKER_FREQPEAK)
      call sub_env%yaml%read('FreqMin',     silent=no_key, val=this%FreqMin,     default=DEF_WAVEMAKER_FREQMIN)
      call sub_env%yaml%read('FreqMax',     silent=no_key, val=this%FreqMax,     default=DEF_WAVEMAKER_FREQMAX)
      call sub_env%yaml%read('Hmo',         silent=no_key, val=this%Hmo,         default=DEF_WAVEMAKER_HMO)
      call sub_env%yaml%read('GammaTMA',    silent=no_key, val=this%GammaTMA,    default=DEF_WAVEMAKER_GAMMATMA)
      call sub_env%yaml%read('Nfreq',       silent=no_key, val=this%Nfreq,       default=DEF_WAVEMAKER_NFREQ)
      call sub_env%yaml%read('ThetaPeak',   silent=no_key, val=this%ThetaPeak,   default=DEF_WAVEMAKER_THETAPEAK)
      call sub_env%yaml%read('Ntheta',      silent=no_key, val=this%Ntheta,      default=DEF_WAVEMAKER_NTHETA)
      call sub_env%yaml%read('Sigma_Theta', silent=no_key, val=this%Sigma_Theta, default=DEF_WAVEMAKER_SIGMA_THETA)
      call sub_env%yaml%read('alpha_c',     silent=no_key, val=this%alpha_c,     default=DEF_WAVEMAKER_ALPHA_C)

      ! Eta limiter
      call sub_env%yaml%read('ETA_LIMITER', val=this%ETA_LIMITER, default=DEF_WAVEMAKER_ETA_LIMITER)
      if (this%ETA_LIMITER) then
         call sub_env%yaml%read('CrestLimit',  val=this%CrestLimit)
         call sub_env%yaml%read('TroughLimit', val=this%TroughLimit)
      end if

      ! Absorbing-generating
      call sub_env%yaml%read('WAVE_DATA_TYPE', val=this%WAVE_DATA_TYPE, default=DEF_WAVEMAKER_WAVE_DATA_TYPE)
      call sub_env%yaml%read('DepthWaveMaker', silent=no_key, val=this%DepthWaveMaker, default=DEF_WAVEMAKER_DEPTHWAVEMAKER)
      if (no_key) &
         call sub_env%yaml%read('DEP_WK', silent=no_key, val=this%DepthWaveMaker, default=DEF_WAVEMAKER_DEP_WK)
      call sub_env%yaml%read('WidthWaveMaker',     silent=no_key, val=this%WidthWaveMaker,     default=DEF_WAVEMAKER_WIDTHWAVEMAKER)
      call sub_env%yaml%read('R_sponge_wavemaker', silent=no_key, val=this%R_sponge_wavemaker, default=DEF_WAVEMAKER_R_SPONGE_WAVEMAKER)
      call sub_env%yaml%read('A_sponge_wavemaker', silent=no_key, val=this%A_sponge_wavemaker, default=DEF_WAVEMAKER_A_SPONGE_WAVEMAKER)
      call sub_env%yaml%read('EqualEnergy',        val=this%EqualEnergy, default=DEF_WAVEMAKER_EQUALENERGY)

      ! WaveMakerCd presence enables WaveMakerCurrentBalance
      call sub_env%yaml%read('WaveMakerCd', silent=no_key, val=this%WaveMakerCd, default=DEF_WAVEMAKER_WAVEMAKERCD)
      this%WaveMakerCurrentBalance = .not. no_key

   end subroutine wavemaker_read_input

end module model_wavemaker_mod
