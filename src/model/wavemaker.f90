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

   implicit none(external)

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

      call sub_env%yaml%read('type', val=this%wavemaker_type, default='nothing')

      ! Shared position / depth / ramp
      call sub_env%yaml%read('Xc_WK',     silent=no_key, val=this%Xc_WK,     default='0.0')
      call sub_env%yaml%read('Yc_WK',     silent=no_key, val=this%Yc_WK,     default='0.0')
      call sub_env%yaml%read('DEP_WK',    silent=no_key, val=this%DEP_WK,    default='0.0')
      call sub_env%yaml%read('Time_ramp', silent=no_key, val=this%Time_ramp,  default='0.0')
      call sub_env%yaml%read('Delta_WK',  silent=no_key, val=this%Delta_WK,   default='0.5')
      call sub_env%yaml%read('Ywidth_WK', silent=no_key, val=this%Ywidth_WK,  default='999999.0')

      ! Solitary
      call sub_env%yaml%read('AMP',                       silent=no_key, val=this%AMP_SOLI,  default='0.0')
      call sub_env%yaml%read('DEP',                       silent=no_key, val=this%DEP_SOLI,  default='0.0')
      call sub_env%yaml%read('LAGTIME',                   silent=no_key, val=this%LAG_SOLI,  default='0.0')
      call sub_env%yaml%read('XWAVEMAKER',                silent=no_key, val=this%XWAVEMAKER, default='0.0')
      call sub_env%yaml%read('SolitaryPositiveDirection', silent=no_key, &
                              val=this%SolitaryPositiveDirection, default='YES')

      ! Initial condition
      call sub_env%yaml%read('Xc',  silent=no_key, val=this%Xc,  default='0.0')
      call sub_env%yaml%read('Yc',  silent=no_key, val=this%Yc,  default='0.0')
      call sub_env%yaml%read('WID', silent=no_key, val=this%WID, default='0.0')

      ! N-wave
      call sub_env%yaml%read('x1_Nwave',    silent=no_key, val=this%x1_Nwave,    default='0.0')
      call sub_env%yaml%read('x2_Nwave',    silent=no_key, val=this%x2_Nwave,    default='0.0')
      call sub_env%yaml%read('a0_Nwave',    silent=no_key, val=this%a0_Nwave,    default='0.0')
      call sub_env%yaml%read('gamma_Nwave', silent=no_key, val=this%gamma_Nwave, default='0.0')
      call sub_env%yaml%read('dep_Nwave',   silent=no_key, val=this%dep_Nwave,   default='0.0')

      ! Regular wave
      call sub_env%yaml%read('Tperiod',  silent=no_key, val=this%Tperiod,  default='0.0')
      call sub_env%yaml%read('AMP_WK',   silent=no_key, val=this%AMP_WK,   default='0.0')
      call sub_env%yaml%read('Theta_WK', silent=no_key, val=this%Theta_WK, default='0.0')

      ! Multi-component time series
      call sub_env%yaml%read('NumWaveComp',  silent=no_key, val=this%NumWaveComp, default='1')
      call sub_env%yaml%read('PeakPeriod',   silent=no_key, val=this%PeakPeriod,  default='0.0')
      call sub_env%yaml%read('WaveCompFile', silent=no_key, val=this%WaveCompFile)

      ! Spectral
      call sub_env%yaml%read('FreqPeak',    silent=no_key, val=this%FreqPeak,    default='0.0')
      call sub_env%yaml%read('FreqMin',     silent=no_key, val=this%FreqMin,     default='0.0')
      call sub_env%yaml%read('FreqMax',     silent=no_key, val=this%FreqMax,     default='0.0')
      call sub_env%yaml%read('Hmo',         silent=no_key, val=this%Hmo,         default='0.0')
      call sub_env%yaml%read('GammaTMA',    silent=no_key, val=this%GammaTMA,    default='3.3')
      call sub_env%yaml%read('Nfreq',       silent=no_key, val=this%Nfreq,       default='45')
      call sub_env%yaml%read('ThetaPeak',   silent=no_key, val=this%ThetaPeak,   default='0.0')
      call sub_env%yaml%read('Ntheta',      silent=no_key, val=this%Ntheta,      default='1')
      call sub_env%yaml%read('Sigma_Theta', silent=no_key, val=this%Sigma_Theta, default='0.0')
      call sub_env%yaml%read('alpha_c',     silent=no_key, val=this%alpha_c,     default='0.0')

      ! Eta limiter
      call sub_env%yaml%read('ETA_LIMITER', val=this%ETA_LIMITER, default='NO')
      if (this%ETA_LIMITER) then
         call sub_env%yaml%read('CrestLimit',  val=this%CrestLimit)
         call sub_env%yaml%read('TroughLimit', val=this%TroughLimit)
      end if

      ! Absorbing-generating
      call sub_env%yaml%read('WAVE_DATA_TYPE', val=this%WAVE_DATA_TYPE, default='DATA_1D')
      call sub_env%yaml%read('DepthWaveMaker', silent=no_key, val=this%DepthWaveMaker, default='0.0')
      if (no_key) &
         call sub_env%yaml%read('DEP_WK', silent=no_key, val=this%DepthWaveMaker, default='0.0')
      call sub_env%yaml%read('WidthWaveMaker',     silent=no_key, val=this%WidthWaveMaker,     default='0.0')
      call sub_env%yaml%read('R_sponge_wavemaker', silent=no_key, val=this%R_sponge_wavemaker, default='0.0')
      call sub_env%yaml%read('A_sponge_wavemaker', silent=no_key, val=this%A_sponge_wavemaker, default='0.0')
      call sub_env%yaml%read('EqualEnergy',        val=this%EqualEnergy, default='NO')

      ! WaveMakerCd presence enables WaveMakerCurrentBalance
      call sub_env%yaml%read('WaveMakerCd', silent=no_key, val=this%WaveMakerCd, default='0.0')
      this%WaveMakerCurrentBalance = .not. no_key

   end subroutine wavemaker_read_input

end module model_wavemaker_mod
