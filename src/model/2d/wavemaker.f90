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
!      Configured via the initial: section, not wavemaker.type.
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
!  YAML block: initial:         (initial-condition types, nee INI_*/N_WAVE;
!                                block presence selects the type)
!    water_level: <length>      default 0 — PORT GAP, non-zero gates pending
!    solitary:  {amplitude, depth, x_center, direction: +x|-x}
!    sine_mode: {amplitude, depth, mode_x (default 1), mode_y (default 0)}
!    hump:      pending (INI_REC/GAU/DIP not in apply_ic yet)
!    n_wave:    pending
!
!  YAML block: wavemaker:       (top-level; omit for no wavemaker;
!                                cannot combine with initial: yet)
!    type: <string>             default 'nothing'
!    --- shared position/ramp ---
!    Xc_WK: <length>
!    Yc_WK: <length>            default 0
!    DEP_WK: <length>
!    Time_ramp: <time>          default 0
!    Delta_WK: <length>         default 0.5
!    Ywidth_WK: <length>        default 999999 (= no limit)
!    --- boundary solitary (LEF_SOL) ---
!    LAGTIME: <time>            LAG_SOLI, default 0
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
   use core_constants_mod, only: SP, PI, DEG2RAD
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base
   use model_tide_mod, only: type_model_tide

   use model_config_defaults_mod, only: DEF_INITIAL_SINE_MODE_AMPLITUDE, &
                                        DEF_INITIAL_SINE_MODE_DEPTH, &
                                        DEF_INITIAL_SINE_MODE_MODE_X, &
                                        DEF_INITIAL_SINE_MODE_MODE_Y, &
                                        DEF_INITIAL_SOLITARY_AMPLITUDE, &
                                        DEF_INITIAL_SOLITARY_DEPTH, &
                                        DEF_INITIAL_SOLITARY_DIRECTION, &
                                        DEF_INITIAL_SOLITARY_X_CENTER, &
                                        DEF_INITIAL_WATER_LEVEL, &
                                        DEF_WAVEMAKER_ALPHA_C, &
                                        DEF_WAVEMAKER_AMP_WK, &
                                        DEF_WAVEMAKER_A_SPONGE_WAVEMAKER, &
                                        DEF_WAVEMAKER_DELTA_WK, DEF_WAVEMAKER_DEP_WK, &
                                        DEF_WAVEMAKER_EQUALENERGY, &
                                        DEF_WAVEMAKER_ETA_LIMITER, DEF_WAVEMAKER_FREQMAX, &
                                        DEF_WAVEMAKER_FREQMIN, DEF_WAVEMAKER_FREQPEAK, &
                                        DEF_WAVEMAKER_GAMMATMA, &
                                        DEF_WAVEMAKER_HMO, DEF_WAVEMAKER_LAGTIME, &
                                        DEF_WAVEMAKER_NFREQ, DEF_WAVEMAKER_NTHETA, &
                                        DEF_WAVEMAKER_NUMWAVECOMP, DEF_WAVEMAKER_PEAKPERIOD, &
                                        DEF_WAVEMAKER_R_SPONGE_WAVEMAKER, &
                                        DEF_WAVEMAKER_SIGMA_THETA, &
                                        DEF_WAVEMAKER_THETAPEAK, DEF_WAVEMAKER_THETA_WK, &
                                        DEF_WAVEMAKER_TIME_RAMP, DEF_WAVEMAKER_TPERIOD, &
                                        DEF_WAVEMAKER_TYPE, &
                                        DEF_WAVEMAKER_WAVE_DATA_TYPE, &
                                        DEF_WAVEMAKER_WIDTHWAVEMAKER, &
                                        DEF_WAVEMAKER_XC_WK, &
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

      ! Standing wave — INI_SINE.  Basin-mode numbers along x and y; the
      ! default (1, 0) is the 1D fundamental seiche, mode_y >= 1 gives an
      ! oblique (diagonal) 2D standing wave for the isotropy check.
      integer  :: MODE_X = 1   ! YAML key: mode_x
      integer  :: MODE_Y = 0   ! YAML key: mode_y

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
      real(SP) :: T_brk = 0.0_SP                      ! breaking-age override 1/FreqMax; 0 = none

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
      logical :: no_wm, no_key, has_initial

      this%wavemaker_type = "nothing"
      call wavemaker_read_initial(this, env, has_initial)

      sub_env = get_sub_env(env, "wavemaker", is_empty=no_wm)
      this%is_activated = has_initial .or. .not. no_wm
      if (no_wm) return

      if (has_initial) then
         ! single-slot machinery: the IC fills wavemaker_type, so a
         ! wavemaker: section cannot coexist until the wavemaker refactor
         call env%log%exit_on_error( &
            "wavemaker: cannot combine with an initial: condition yet")
      end if

      call sub_env%yaml%read("type", val=this%wavemaker_type, default=DEF_WAVEMAKER_TYPE)
      select case (trim(this%wavemaker_type))
      case ("INI_SOLITARY", "INI_SOL", "INI_SINE", "INI_REC", "INI_GAU", &
            "INI_DIP", "N_WAVE")
         call env%log%exit_on_error("wavemaker/type: initial-condition types"// &
                                    " moved to the initial: section")
      end select

      ! Shared position / depth / ramp
      call sub_env%yaml%read("Xc_WK", silent=no_key, val=this%Xc_WK, default=DEF_WAVEMAKER_XC_WK)
      call sub_env%yaml%read("Yc_WK", silent=no_key, val=this%Yc_WK, default=DEF_WAVEMAKER_YC_WK)
      call sub_env%yaml%read("DEP_WK", silent=no_key, val=this%DEP_WK, default=DEF_WAVEMAKER_DEP_WK)
      call sub_env%yaml%read("Time_ramp", silent=no_key, val=this%Time_ramp, default=DEF_WAVEMAKER_TIME_RAMP)
      call sub_env%yaml%read("Delta_WK", silent=no_key, val=this%Delta_WK, default=DEF_WAVEMAKER_DELTA_WK)
      call sub_env%yaml%read("Ywidth_WK", silent=no_key, val=this%Ywidth_WK, default=DEF_WAVEMAKER_YWIDTH_WK)

      ! Boundary-solitary lag (LEF_SOL)
      call sub_env%yaml%read("LAGTIME", silent=no_key, val=this%LAG_SOLI, default=DEF_WAVEMAKER_LAGTIME)

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
      ! no `default=` on either presence-tested read below: yaml%read only
      ! assigns `silent` when `default` is ABSENT, so asking for both hands
      ! back an unwritten flag.  Absent key -> the component initialiser
      ! stands and the fallback resolves it.
      call sub_env%yaml%read("DepthWaveMaker", silent=no_key, val=this%DepthWaveMaker)
      if (no_key) this%DepthWaveMaker = this%DEP_WK
      call sub_env%yaml%read("WidthWaveMaker", silent=no_key, val=this%WidthWaveMaker, default=DEF_WAVEMAKER_WIDTHWAVEMAKER)
  call sub_env%yaml%read("R_sponge_wavemaker", silent=no_key, val=this%R_sponge_wavemaker, default=DEF_WAVEMAKER_R_SPONGE_WAVEMAKER)
  call sub_env%yaml%read("A_sponge_wavemaker", silent=no_key, val=this%A_sponge_wavemaker, default=DEF_WAVEMAKER_A_SPONGE_WAVEMAKER)
      call sub_env%yaml%read("EqualEnergy", val=this%EqualEnergy, default=DEF_WAVEMAKER_EQUALENERGY)

      ! WaveMakerCd presence enables WaveMakerCurrentBalance
      call sub_env%yaml%read("WaveMakerCd", silent=no_key, val=this%WaveMakerCd)
      this%WaveMakerCurrentBalance = .not. no_key

   end subroutine wavemaker_read_input

   ! ----------------------------------------------------------------
   ! initial: section (nee wavemaker INI_*/N_WAVE) — block presence
   ! selects the IC type; the blocks fill the same single-slot
   ! wavemaker machinery until the wavemaker refactor.  solitary +
   ! sine_mode are live in apply_ic; hump + n_wave gate pending.
   ! water_level is a PORT GAP: legacy added it to Depth/DEP_WK/Dep_Ser
   ! (old init.F:807-810) and the modern engine never wired it.
   ! ----------------------------------------------------------------
   subroutine wavemaker_read_initial(this, env, has_initial)
      use core_yaml_file_mod, only: type_yaml_reader
      class(type_model_wavemaker), intent(inout) :: this
      type(type_env), intent(inout), target :: env
      logical, intent(out) :: has_initial

      type(type_env) :: ini_env
      type(type_yaml_reader) :: blk_yaml
      character(:), allocatable :: direction
      real(SP) :: water_level
      logical :: no_ini, no_blk, no_key

      has_initial = .false.
      ini_env = get_sub_env(env, "initial", no_ini)
      if (no_ini) return

      call ini_env%yaml%read("water_level", silent=no_key, val=water_level, &
                             default=DEF_INITIAL_WATER_LEVEL)
      if (water_level /= 0.0_SP) then
         call env%log%exit_on_error("initial/water_level: pending — the"// &
                                    " legacy still-water offset is not wired yet")
      end if

      blk_yaml = ini_env%yaml%cast_dictionary("solitary", no_blk)
      if (.not. no_blk) then
         has_initial = .true.
         this%wavemaker_type = "INI_SOLITARY"
         call blk_yaml%read("amplitude", silent=no_key, val=this%AMP_SOLI, &
                            default=DEF_INITIAL_SOLITARY_AMPLITUDE)
         call blk_yaml%read("depth", silent=no_key, val=this%DEP_SOLI, &
                            default=DEF_INITIAL_SOLITARY_DEPTH)
         call blk_yaml%read("x_center", silent=no_key, val=this%XWAVEMAKER, &
                            default=DEF_INITIAL_SOLITARY_X_CENTER)
         call blk_yaml%read("direction", silent=no_key, val=direction, &
                            default=DEF_INITIAL_SOLITARY_DIRECTION)
         select case (trim(direction))
         case ("+x")
            this%SolitaryPositiveDirection = .true.
         case ("-x")
            this%SolitaryPositiveDirection = .false.
         case default
            call env%log%exit_on_error( &
               "initial/solitary/direction: expected +x or -x")
         end select
      end if

      blk_yaml = ini_env%yaml%cast_dictionary("sine_mode", no_blk)
      if (.not. no_blk) then
         if (has_initial) call env%log%exit_on_error( &
            "initial: only one initial-condition block allowed")
         has_initial = .true.
         this%wavemaker_type = "INI_SINE"
         call blk_yaml%read("amplitude", silent=no_key, val=this%AMP_SOLI, &
                            default=DEF_INITIAL_SINE_MODE_AMPLITUDE)
         call blk_yaml%read("depth", silent=no_key, val=this%DEP_SOLI, &
                            default=DEF_INITIAL_SINE_MODE_DEPTH)
         call blk_yaml%read("mode_x", silent=no_key, val=this%MODE_X, &
                            default=DEF_INITIAL_SINE_MODE_MODE_X)
         call blk_yaml%read("mode_y", silent=no_key, val=this%MODE_Y, &
                            default=DEF_INITIAL_SINE_MODE_MODE_Y)
      end if

      blk_yaml = ini_env%yaml%cast_dictionary("hump", no_blk)
      if (.not. no_blk) call env%log%exit_on_error( &
         "initial/hump: pending — INI_REC/GAU/DIP are not ported to apply_ic yet")
      blk_yaml = ini_env%yaml%cast_dictionary("n_wave", no_blk)
      if (.not. no_blk) call env%log%exit_on_error( &
         "initial/n_wave: pending — N_WAVE is not ported to apply_ic yet")

   end subroutine wavemaker_read_initial

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
         case ("WK_NEW_IRR")
            call new_irr_init_compute(this, grid, periodic, env)
         case default
            call spectral_init_compute(this, grid, periodic, env)
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
   ! Private: WK_TIME multi-component setup (legacy WK_TIME block of
   ! WAVEMAKER_INITIALIZATION + WK_WAVEMAKER_TIME_SERIES).  Reads
   ! NumWaveComp rows of (period, amplitude, phase) from WaveCompFile,
   ! solves the per-component Wei & Kirby source magnitude, and takes
   ! the shared width from PeakPeriod.  T_brk override is the LAST
   ! component's period (legacy SHOW_BREAKING assignment).
   ! ----------------------------------------------------------------
   subroutine time_series_init_compute(this)
      class(type_model_wavemaker), intent(inout) :: this

      integer :: kf, i, unit, ios

      allocate (this%wave_comp(this%NumWaveComp, 3), &
                this%D_genS(this%NumWaveComp), &
                this%Beta_genS(this%NumWaveComp))

      open (newunit=unit, file=trim(this%WaveCompFile), status="old", &
            action="read", iostat=ios)
      if (ios /= 0) error stop "wavemaker: cannot open WaveCompFile"
      do kf = 1, this%NumWaveComp
         read (unit, *, iostat=ios) (this%wave_comp(kf, i), i=1, 3)
         if (ios /= 0) error stop "wavemaker: WaveCompFile short read"
      end do
      close (unit)

      call wk_time_series_coefficients(this%NumWaveComp, this%wave_comp, &
                                       this%PeakPeriod, this%DEP_WK, &
                                       this%Delta_WK, this%D_genS, &
                                       this%Beta_genS, this%Width_WK)

      this%T_brk = this%wave_comp(this%NumWaveComp, 1)

   end subroutine time_series_init_compute

   ! ----------------------------------------------------------------
   ! Private: WK_DATA2D setup (legacy WK_DATA2D block of WAVEMAKER_
   ! INITIALIZATION + WK_WAVEMAKER_2D_SPECTRAL_DATA + CALCULATE_Cm_Sm).
   ! WaveCompFile layout:
   !   NumFreq NumDir / PeakPeriod / freq rows / dir rows /
   !   amp(1:NumFreq) per dir / [phase(1:NumFreq) per dir, degrees]
   ! Directions with |dir| >= 60 deg are dropped (SWAN-conversion
   ! guard).  Legacy quirks kept: input phase columns are NOT
   ! remapped through the direction filter (legacy Phase2D is never
   ! compacted — column k pairs with the k-th SURVIVING direction
   ! only when nothing before it was dropped); the input phase unit
   ! conversion is 0.005555555555556*pi.  Legacy SHOW_BREAKING reads
   ! WAVE_COMP(NumWaveComp,1) with NumWaveComp belonging to WK_TIME —
   ! never set on this path (uninitialized) — so T_brk stays at the
   ! stepper default here.  FreqPeak (ramp scale) = 1/PeakPeriod from
   ! the file.
   ! ----------------------------------------------------------------
   subroutine data2d_init_compute(this, grid, periodic, env)
      use core_grid_mod, only: type_grid_2d
      use core_build_config_mod, only: BUILD_ZERO_PHASE
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env

      real(SP), allocatable :: freq(:), dire(:), amp(:, :), phase(:, :)
      real(SP), allocatable :: dire_flt(:), amp_flt(:, :)
      real(SP), allocatable :: d_gen2d(:, :), rlamda2d(:, :), beta_gen2d(:)
      logical :: input_phase
      integer :: nfreq, ndir_in, ndir, unit, ios, i, j, kf, ktheta
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

      ! drop out-of-range directions, order-preserving (|dir| < 60)
      allocate (dire_flt(ndir_in), amp_flt(nfreq, ndir_in))
      ndir = 0
      do i = 1, ndir_in
         if (abs(dire(i)) < 60.0_SP) then
            ndir = ndir + 1
            dire_flt(ndir) = dire(i)
            amp_flt(:, ndir) = amp(:, i)
         end if
      end do

      write (msg, '(A,I0,A,I0)') "WK_DATA2D: NumFreq ", nfreq, &
         ", NumDir ", ndir
      call env%log%info(trim(msg))

      if (input_phase) then
         call env%log%info("WK_DATA2D: using input phase info")
         ! legacy unit conversion literal (0.00555... * pi, not /180)
         do j = 1, nfreq
            do i = 1, ndir
               phase(j, i) = phase(j, i)*0.005555555555556_SP*PI
            end do
         end do
      else
         if (BUILD_ZERO_PHASE) then
            phase(:, 1:ndir) = 0.0_SP
         else
            call random_number(phase(:, 1:ndir))
            phase(:, 1:ndir) = phase(:, 1:ndir)*2.0_SP*PI
         end if
      end if

      allocate (d_gen2d(nfreq, ndir), rlamda2d(nfreq, ndir), &
                beta_gen2d(nfreq))
      call wk_data2d_coefficients(grid, periodic, env, nfreq, ndir, freq, &
                                  dire_flt(1:ndir), amp_flt(:, 1:ndir), &
                                  this%PeakPeriod, this%DEP_WK, this%Delta_WK, &
                                  d_gen2d, beta_gen2d, rlamda2d, this%Width_WK)

      this%Nfreq = nfreq
      this%FreqPeak = 1.0_SP/this%PeakPeriod
      mloc = grid%lp%mloc
      nloc = grid%lp%nloc
      allocate (this%omgn_ir(nfreq), this%Cm(mloc, nloc, nfreq), &
                this%Sm(mloc, nloc, nfreq))
      do j = 1, nfreq
         this%omgn_ir(j) = 2.0_SP*PI*freq(j)
      end do

      ! dense modes (legacy CALCULATE_Cm_Sm; beta enters as a (mfreq)
      ! dummy via sequence association — column 1, identical across
      ! directions since the wavelength is frequency-only)
      this%Cm = 0.0_SP
      this%Sm = 0.0_SP
      do kf = 1, nfreq
         do j = 1, nloc
            do i = 1, mloc
               do ktheta = 1, ndir
                  this%Cm(i, j, kf) = this%Cm(i, j, kf) + d_gen2d(kf, ktheta) &
                                      *exp(-beta_gen2d(kf)*(this%xmk_wk(i) - this%Xc_WK)**2) &
                                      *cos(rlamda2d(kf, ktheta)*this%ymk_wk(j) &
                                           + phase(kf, ktheta))
                  this%Sm(i, j, kf) = this%Sm(i, j, kf) + d_gen2d(kf, ktheta) &
                                      *exp(-beta_gen2d(kf)*(this%xmk_wk(i) - this%Xc_WK)**2) &
                                      *sin(rlamda2d(kf, ktheta)*this%ymk_wk(j) &
                                           + phase(kf, ktheta))
               end do
            end do
         end do
      end do

   end subroutine data2d_init_compute

   ! ----------------------------------------------------------------
   ! Private: WK_NEW_DATA2D setup (legacy Salatin 2021 block of
   ! WAVEMAKER_INITIALIZATION + WK_NEW_WAVEMAKER_2D_SPECTRAL_DATA).
   ! Component-list WaveCompFile layout (one direction per component):
   !   NumFreq / PeakPeriod / freqs / dirs / amps / [phases, degrees]
   ! Components with |dir| > 90 deg are dropped; here the phase list
   ! IS compacted with its component (unlike WK_DATA2D).  T_brk quirk
   ! kept bug-for-bug: legacy assigns the last component's FREQUENCY
   ! (not period).  FreqPeak = 1/PeakPeriod from the file.
   ! ----------------------------------------------------------------
   subroutine new_data2d_init_compute(this, grid, periodic, env)
      use core_grid_mod, only: type_grid_2d
      use core_build_config_mod, only: BUILD_ZERO_PHASE
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env

      real(SP), allocatable :: freq(:), dire(:), amp(:), phase(:)
      real(SP), allocatable :: freq_flt(:), dire_flt(:), amp_flt(:), phase_flt(:)
      real(SP), allocatable :: d_gen(:), rlamda(:), beta_gen(:)
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
         if (BUILD_ZERO_PHASE) then
            phase_flt(1:nfreq) = 0.0_SP
         else
            call random_number(phase_flt(1:nfreq))
            phase_flt(1:nfreq) = phase_flt(1:nfreq)*2.0_SP*PI
         end if
      end if

      allocate (d_gen(nfreq), rlamda(nfreq), beta_gen(nfreq))
      call wk_new_data2d_coefficients(grid, periodic, env, nfreq, &
                                      freq_flt, dire_flt, amp_flt, &
                                      this%PeakPeriod, this%DEP_WK, &
                                      this%Delta_WK, d_gen, beta_gen, &
                                      rlamda, this%Width_WK)

      this%Nfreq = nfreq
      this%FreqPeak = 1.0_SP/this%PeakPeriod
      allocate (this%omgn_ir(nfreq), &
                this%Cm(grid%lp%mloc, grid%lp%nloc, nfreq), &
                this%Sm(grid%lp%mloc, grid%lp%nloc, nfreq))
      do j = 1, nfreq
         this%omgn_ir(j) = 2.0_SP*PI*freq_flt(j)
      end do

      call calc_new_cm_sm(this, env, freq_flt(1:nfreq), d_gen, &
                          phase_flt(1:nfreq), rlamda, beta_gen)

      ! legacy: T_brk = Freq(FreqCount) — the last FREQUENCY, not a
      ! period; kept bug-for-bug (parity ledger)
      this%T_brk = freq_flt(nfreq)

   end subroutine new_data2d_init_compute

   ! ----------------------------------------------------------------
   ! Private: WK_NEW_IRR setup (legacy Salatin 2021 analytic-spectrum
   ! block: WK_NEW_EQUAL_DFREQ_IRREGULAR_WAVE + CALCULATE_NEW_Cm_Sm).
   ! Equal-dfreq TMA bins with ONE direction per frequency component
   ! (interleaved across the spread), optional coherence shuffle
   ! (alpha_c), then the same per-component source solve.
   ! ----------------------------------------------------------------
   subroutine new_irr_init_compute(this, grid, periodic, env)
      use core_grid_mod, only: type_grid_2d
      class(type_model_wavemaker), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env

      real(SP), allocatable :: freq(:), d_gen(:), rlamda(:), phi1(:), beta_gen(:)

      allocate (freq(this%Nfreq), d_gen(this%Nfreq), rlamda(this%Nfreq), &
                phi1(this%Nfreq), beta_gen(this%Nfreq), &
                this%omgn_ir(this%Nfreq), &
                this%Cm(grid%lp%mloc, grid%lp%nloc, this%Nfreq), &
                this%Sm(grid%lp%mloc, grid%lp%nloc, this%Nfreq))

      call wk_new_irr_coefficients(this%Nfreq, this%Ntheta, this%Delta_WK, &
                                   this%DEP_WK, this%FreqPeak, this%FreqMax, &
                                   this%FreqMin, this%GammaTMA, this%Hmo, &
                                   this%ThetaPeak, this%Sigma_Theta, &
                                   this%alpha_c, periodic, grid%dy0, grid%N, &
                                   env, freq, rlamda, beta_gen, d_gen, phi1, &
                                   this%Width_WK, this%omgn_ir)

      call calc_new_cm_sm(this, env, freq, d_gen, phi1, rlamda, beta_gen)

      this%T_brk = 1.0_SP/this%FreqMax

   end subroutine new_irr_init_compute

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
   ! Private: read the boundary 2D spectrum from WaveCompFile (legacy
   ! io.F WAVE_DATA_TYPE DATA block): NumFreq NumDir / PeakPeriod
   ! (unused) / NumFreq frequencies / NumDir directions (degrees) /
   ! NumDir rows of NumFreq amplitudes / optional NumDir rows of
   ! NumFreq phases (degrees).  Frequencies invert to periods (legacy
   ! bare STOP on zero); directions convert via DEG2RAD; input
   ! phases via the truncated-pi literal.
   ! Missing phases: zero for parity builds, RANDOM_NUMBER otherwise
   ! (legacy random2() is compiler-specific).  Overrides Nfreq from
   ! the file header.
   ! ----------------------------------------------------------------
   subroutine read_boundary_2d_spectrum(this, num_dir, per_ser, theta_ser, &
                                        amp_ser, phase_left)
      use core_build_config_mod, only: BUILD_ZERO_PHASE
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
         phase_left = phase_left*3.1415926/180.0_SP
      elseif (BUILD_ZERO_PHASE) then
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
   ! Initial-condition wavemakers: fill eta/u/v at t=0.
   ! Currently INI_SOLITARY (legacy INITIAL_SOLITARY_WAVE, old/samples.F)
   ! and INI_SINE; INI_REC/INI_GAU/INI_DIP/N_WAVE to follow.
   ! No-op (still water) for source/BC wavemaker types.
   !
   ! INI_SINE — standing wave of a closed flat basin, for the linear-
   ! dispersion validation case.  With reflective walls on all sides (the
   ! default when no wavemaker/sponge drives a face), the basin modes are
   !   $$ \eta(x,y) = a\,\cos(k_x x)\,\cos(k_y y), \quad u = v = 0 $$
   !   $$ k_x = n_x \pi/(M\,\Delta x), \quad k_y = n_y \pi/(N\,\Delta y) $$
   ! with amplitude $a$ = AMP, mode numbers $(n_x, n_y)$ = (mode_x, mode_y),
   ! and $x, y$ measured from the left/back walls so the antinodes sit at the
   ! walls (zero normal velocity, matching no-flux).  The default $(1, 0)$ is
   ! the 1D fundamental seiche ($k_y = 0$, a half wavelength across the domain);
   ! $n_y \ge 1$ tilts it into an oblique diagonal mode with $|k| =
   ! \sqrt{k_x^2 + k_y^2}$ at angle $\theta = \arctan(k_y/k_x)$, so a fixed-$|k|$
   ! sweep of $(n_x, n_y)$ on one grid probes dispersion ISOTROPY.
   ! A still-water start with this cosine surface oscillates at the model's
   ! $\omega(|k|)$; the measured period, checked against the Nwogu linear
   ! dispersion relation, probes the dispersive terms directly.  Periodic BCs
   ! are deliberately avoided here — periodic-x is not yet implemented, and the
   ! closed basin needs only the default walls.
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

      real(SP) :: c_ph, b, a1, a2, au, sc, usign, kx, ky, xloc, yloc
      integer  :: i, j

      eta = 0.0_SP
      u = 0.0_SP
      v = 0.0_SP

      ! Closed-basin standing wave — half wavelengths across the reflective
      ! walls; x, y run from the left/back walls so the cos() antinodes sit at
      ! the ends (zero normal velocity, matching no-flux).  mode_y = 0 leaves
      ! ky = 0 so cos(ky y) = 1 and this is the 1D fundamental seiche; mode_y
      ! >= 1 makes it an oblique diagonal mode for the isotropy check.
      if (this%wavemaker_type == "INI_SINE") then
         kx = real(this%MODE_X, SP)*PI/(real(grid%M, SP)*grid%dx0)
         ky = real(this%MODE_Y, SP)*PI/(real(grid%N, SP)*grid%dy0)
         do j = 1, grid%lp%nloc
            do i = 1, grid%lp%mloc
               xloc = (real(grid%ibegin + i - grid%lp%ib, SP) - 0.5_SP)*grid%dx0
               yloc = (real(grid%jbegin + j - grid%lp%jb, SP) - 0.5_SP)*grid%dy0
               eta(i, j) = this%AMP_SOLI*cos(kx*xloc)*cos(ky*yloc)
            end do
         end do
         return
      end if

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
      use core_constants_mod, only: GRAV
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
      use core_constants_mod, only: GRAV, SMALL
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
   ! Private: per-component Wei & Kirby solve for WK_TIME (legacy
   ! WK_WAVEMAKER_TIME_SERIES).  The regular-wave formulas with
   ! theta = 0 hard-coded (legacy "assume zero because no or few cases
   ! include directions" — the exact cos(0) = 1 factors are dropped),
   ! one D/beta pair per component, shared width from PeakPeriod.
   ! rI keeps the legacy truncated literal 3.14159.
   ! ----------------------------------------------------------------
   subroutine wk_time_series_coefficients(nc, wave_comp, peak_period, h_gen, &
                                          delta, D_gen, beta_gen, width)
      use core_constants_mod, only: GRAV
      integer, intent(in) :: nc
      real(SP), intent(in) :: wave_comp(nc, 3), peak_period, h_gen, delta
      real(SP), intent(out) :: D_gen(nc), beta_gen(nc), width

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: alpha1, omgn, Tperiod, amp, tb, tc, wkn, c_phase
      real(SP) :: wave_length, rl_gen, ri
      integer :: kf

      if (peak_period == 0.0_SP) &
         error stop "wavemaker: re-set PeakPeriod for wavemaker"

      alpha1 = alpha + 1.0_SP/3.0_SP

      do kf = 1, nc
         omgn = 2.0_SP*PI/wave_comp(kf, 1)
         Tperiod = wave_comp(kf, 1)
         amp = wave_comp(kf, 2)

         if (h_gen == 0.0_SP .or. Tperiod == 0.0_SP) &
            error stop "wavemaker: re-set depth, Tperiod for wavemaker"

         tb = omgn*omgn*h_gen/GRAV
         tc = 1.0_SP + tb*alpha
         wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                    /(2.0_SP*alpha1))/h_gen
         c_phase = 1.0_SP/wkn/Tperiod*2.0_SP*PI
         wave_length = c_phase*Tperiod

         beta_gen(kf) = 80.0_SP/delta**2/wave_length**2
         rl_gen = wkn
         ri = sqrt(3.14159_SP/beta_gen(kf))*exp(-rl_gen**2/4.0_SP/beta_gen(kf))

         D_gen(kf) = 2.0_SP*amp &
                     *(omgn**2 - alpha1*GRAV*wkn**4*h_gen**3) &
                     /(omgn*wkn*ri*(1.0_SP - alpha*(wkn*h_gen)**2))
      end do

      ! shared width from the peak period (legacy tail block)
      omgn = 2.0_SP*PI/peak_period
      tb = omgn*omgn*h_gen/GRAV
      tc = 1.0_SP + tb*alpha
      wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb))/(2.0_SP*alpha1))/h_gen
      c_phase = 1.0_SP/wkn/peak_period*2.0_SP*PI
      wave_length = c_phase*peak_period
      width = delta*wave_length/2.0_SP

   end subroutine wk_time_series_coefficients

   ! ----------------------------------------------------------------
   ! Private: per-component solve for a measured 2D spectrum (legacy
   ! WK_WAVEMAKER_2D_SPECTRAL_DATA).  Directions convert through
   ! DEG2RAD; under periodic-y each (freq, dir) pair
   ! snaps via the nearest-of-two-modes rule — identical arithmetic to
   ! calc_periodic_theta, whose |theta| >= 90 error stop is
   ! unreachable here (|dir| < 60 prefiltered).  rI keeps the
   ! truncated 3.14159; beta is frequency-only (legacy stores it per
   ! direction but consumes column 1 via sequence association).
   ! ----------------------------------------------------------------
   subroutine wk_data2d_coefficients(grid, periodic, env, nfreq, ndir, freq, &
                                     dire_deg, amp, peak_period, h_gen, delta, &
                                     D_gen, beta_gen, rlamda, width)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: GRAV, SMALL
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env
      integer, intent(in) :: nfreq, ndir
      real(SP), intent(in) :: freq(:), dire_deg(:), amp(:, :)
      real(SP), intent(in) :: peak_period, h_gen, delta
      real(SP), intent(out) :: D_gen(nfreq, ndir), beta_gen(nfreq)
      real(SP), intent(out) :: rlamda(nfreq, ndir), width

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: dire(ndir), dir2d(nfreq, ndir)
      real(SP) :: alpha1, omgn, Tperiod, amp_wk, tb, tc, wkn, wkn_snap
      real(SP) :: c_phase, wave_length, rl_gen, ri, theta
      integer :: nfre, kdir
      character(96) :: msg

      dire = dire_deg*DEG2RAD
      alpha1 = alpha + 1.0_SP/3.0_SP

      if (periodic) then
         do nfre = 1, nfreq
            ! legacy snap wavenumber chain (MAX(SMALL, h) guard)
            omgn = 2.0_SP*PI*freq(nfre)
            tb = omgn*omgn*h_gen/GRAV
            tc = 1.0_SP + tb*alpha
            wkn_snap = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                            /(2.0_SP*alpha1))/max(SMALL, h_gen)
            do kdir = 1, ndir
               if (dire(kdir) /= 0.0_SP) then
                  call calc_periodic_theta(wkn_snap, dire(kdir), grid%dy0, &
                                           grid%N, dir2d(nfre, kdir))
                  write (msg, '(A,F8.3,A,F8.3,A,F8.3)') &
                     "WK_DATA2D periodic, freq: ", freq(nfre), ", dir: ", &
                     dire(kdir)*180.0_SP/PI, " -> ", &
                     dir2d(nfre, kdir)*180.0_SP/PI
                  call env%log%info(trim(msg))
               else
                  dir2d(nfre, kdir) = 0.0_SP
               end if
            end do
         end do
      else
         do kdir = 1, ndir
            do nfre = 1, nfreq
               dir2d(nfre, kdir) = dire(kdir)
            end do
         end do
      end if

      do kdir = 1, ndir
         do nfre = 1, nfreq
            theta = dir2d(nfre, kdir)
            omgn = 2.0_SP*PI*freq(nfre)
            Tperiod = 1.0_SP/freq(nfre)
            amp_wk = amp(nfre, kdir)

            if (h_gen == 0.0_SP .or. Tperiod == 0.0_SP) &
               error stop "wavemaker: re-set depth, Tperiod for wavemaker"

            tb = omgn*omgn*h_gen/GRAV
            tc = 1.0_SP + tb*alpha
            wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                       /(2.0_SP*alpha1))/h_gen
            c_phase = 1.0_SP/wkn/Tperiod*2.0_SP*PI
            wave_length = c_phase*Tperiod

            rlamda(nfre, kdir) = wkn*sin(theta)
            beta_gen(nfre) = 80.0_SP/delta**2/wave_length**2
            rl_gen = wkn*cos(theta)
            ri = sqrt(3.14159_SP/beta_gen(nfre)) &
                 *exp(-rl_gen**2/4.0_SP/beta_gen(nfre))

            D_gen(nfre, kdir) = 2.0_SP*amp_wk &
                                *cos(theta)*(omgn**2 - alpha1*GRAV*wkn**4*h_gen**3) &
                                /(omgn*wkn*ri*(1.0_SP - alpha*(wkn*h_gen)**2))
         end do
      end do

      ! width from the peak period (legacy tail block)
      omgn = 2.0_SP*PI/peak_period
      tb = omgn*omgn*h_gen/GRAV
      tc = 1.0_SP + tb*alpha
      wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb))/(2.0_SP*alpha1))/h_gen
      c_phase = 1.0_SP/wkn/peak_period*2.0_SP*PI
      wave_length = c_phase*peak_period
      width = delta*wave_length/2.0_SP

   end subroutine wk_data2d_coefficients

   ! ----------------------------------------------------------------
   ! Private: per-component solve for a measured component list
   ! (legacy WK_NEW_WAVEMAKER_2D_SPECTRAL_DATA, Salatin 2021).  One
   ! direction per component; under periodic-y the angle snaps via the
   ! decrement-retry rule (wk_new_periodic_snap).  rI keeps the
   ! truncated 3.14159.
   ! ----------------------------------------------------------------
   subroutine wk_new_data2d_coefficients(grid, periodic, env, nfreq, freq, &
                                         dire_deg, amp, peak_period, h_gen, &
                                         delta, D_gen, beta_gen, rlamda, width)
      use core_grid_mod, only: type_grid_2d
      use core_constants_mod, only: GRAV, SMALL
      type(type_grid_2d), intent(in) :: grid
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env
      integer, intent(in) :: nfreq
      real(SP), intent(in) :: freq(:), dire_deg(:), amp(:)
      real(SP), intent(in) :: peak_period, h_gen, delta
      real(SP), intent(out) :: D_gen(nfreq), beta_gen(nfreq)
      real(SP), intent(out) :: rlamda(nfreq), width

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: dire(nfreq)
      real(SP) :: alpha1, omgn, Tperiod, amp_wk, tb, tc, wkn, wkn_snap
      real(SP) :: c_phase, wave_length, rl_gen, ri, snapped
      integer :: nfre
      character(96) :: msg

      dire = dire_deg(1:nfreq)*DEG2RAD
      alpha1 = alpha + 1.0_SP/3.0_SP

      if (periodic) then
         do nfre = 1, nfreq
            omgn = 2.0_SP*PI*freq(nfre)
            tb = omgn*omgn*h_gen/GRAV
            tc = 1.0_SP + tb*alpha
            wkn_snap = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                            /(2.0_SP*alpha1))/max(SMALL, h_gen)
            call wk_new_periodic_snap(dire(nfre), wkn_snap, grid%dy0, &
                                      grid%N, snapped)
            write (msg, '(A,F8.3,A,F8.3,A,F8.3)') &
               "WK_NEW_DATA2D periodic, freq: ", freq(nfre), ", dir: ", &
               dire(nfre)*180.0_SP/PI, " -> ", snapped*180.0_SP/PI
            call env%log%info(trim(msg))
            dire(nfre) = snapped
         end do
      end if

      do nfre = 1, nfreq
         omgn = 2.0_SP*PI*freq(nfre)
         Tperiod = 1.0_SP/freq(nfre)
         amp_wk = amp(nfre)

         if (h_gen == 0.0_SP .or. Tperiod == 0.0_SP) &
            error stop "wavemaker: re-set depth, Tperiod for wavemaker"

         tb = omgn*omgn*h_gen/GRAV
         tc = 1.0_SP + tb*alpha
         wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb)) &
                    /(2.0_SP*alpha1))/h_gen
         c_phase = 1.0_SP/wkn/Tperiod*2.0_SP*PI
         wave_length = c_phase*Tperiod

         rlamda(nfre) = wkn*sin(dire(nfre))
         beta_gen(nfre) = 80.0_SP/delta**2/wave_length**2
         rl_gen = wkn*cos(dire(nfre))
         ri = sqrt(3.14159_SP/beta_gen(nfre)) &
              *exp(-rl_gen**2/4.0_SP/beta_gen(nfre))

         D_gen(nfre) = 2.0_SP*amp_wk &
                       *cos(dire(nfre))*(omgn**2 - alpha1*GRAV*wkn**4*h_gen**3) &
                       /(omgn*wkn*ri*(1.0_SP - alpha*(wkn*h_gen)**2))
      end do

      ! width from the peak period (legacy tail block)
      omgn = 2.0_SP*PI/peak_period
      tb = omgn*omgn*h_gen/GRAV
      tc = 1.0_SP + tb*alpha
      wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb))/(2.0_SP*alpha1))/h_gen
      c_phase = 1.0_SP/wkn/peak_period*2.0_SP*PI
      wave_length = c_phase*peak_period
      width = delta*wave_length/2.0_SP

   end subroutine wk_new_data2d_coefficients

   ! ----------------------------------------------------------------
   ! Private: WK_NEW_IRR analytic-spectrum solve (legacy WK_NEW_EQUAL_
   ! DFREQ_IRREGULAR_WAVE, Salatin 2021 — equal-dfreq only).  ONE
   ! direction per frequency component, interleaved across the
   ! +-pi/3 spread around ThetaPeak with the wrapped-normal weight
   ! evaluated per component; the weights are calibrated so the
   ! weighted bin energy reproduces the full spectral energy
   !   $$ A_k \leftarrow A_k \frac{E}{\sum_k A_k E_k}, \qquad
   !      H_{m0,k} = 4\sqrt{\alpha_s E_k A_k} $$
   ! then the same Nwogu source solve per component (rI uses full pi
   ! here — the truncated literal is the DATA2D/REG family only).
   ! Legacy quirks kept: the phase speed uses the PEAK frequency
   ! (wave_length = 2 pi / k evaluated through fm), the width uses the
   ! LAST component's wave_length, and the coherence shuffle runs
   ! before the spectral densities so moved components pick up the
   ! host frequency's TMA density.  Legacy assigns phases only under
   ! PERIODIC (uninitialized otherwise — UB); assigned unconditionally
   ! here.  Legacy ntheta = 1 reads theta(2:)/AG(2:) uninitialized
   ! (UB); here the peak angle and unit weight fill the whole array.
   ! ----------------------------------------------------------------
   subroutine wk_new_irr_coefficients(nfreq, ntheta, delta, h_gen, fm, fmax, &
                                      fmin, gamma_spec, Hmo, theta_peak_deg, &
                                      sigma_theta_deg, alpha_c_in, periodic, &
                                      dy, nglob, env, freq, rlamda, beta_gen, &
                                      D_gen, phi1, width, omgn)
      use core_constants_mod, only: GRAV, SMALL
      use core_build_config_mod, only: BUILD_ZERO_PHASE
      integer, intent(in) :: nfreq, ntheta, nglob
      real(SP), intent(in) :: delta, h_gen, fm, fmax, fmin, gamma_spec, Hmo
      real(SP), intent(in) :: theta_peak_deg, sigma_theta_deg, alpha_c_in, dy
      logical, intent(in) :: periodic
      type(type_env), intent(inout) :: env
      real(SP), intent(out) :: freq(nfreq), rlamda(nfreq), beta_gen(nfreq)
      real(SP), intent(out) :: D_gen(nfreq), phi1(nfreq), width, omgn(nfreq)

      real(SP), parameter :: alpha = -0.39_SP
      real(SP) :: theta(nfreq), ag(nfreq), energy_bin(nfreq), hmo_each(nfreq)
      real(SP) :: df, sigma_theta, ktheta_temp, sign_kf, alpha_c
      real(SP) :: Ef, alpha_spec, correction_coeff, alpha1, ap, tb, tc, wkn
      real(SP) :: c_phase, wave_length, rl_gen, ri, snapped
      integer :: kf, k_n, n_spec, idx_theta, displace(1)
      character(96) :: msg

      if (h_gen == 0.0_SP .or. fm == 0.0_SP .or. fmax == 0.0_SP) &
         error stop "wavemaker: re-set depth, FreqPeak, FreqMax for wavemaker"

      df = (fmax - fmin)/(real(nfreq, SP) - 1.0_SP)
      do kf = 1, nfreq
         freq(kf) = fmin + real(kf - 1, SP)*df
      end do

      sigma_theta = sigma_theta_deg*PI/180.0_SP
      idx_theta = 0

      if (ntheta == 1) then
         ! legacy fills theta(1)/AG(1) only and reads the rest
         ! uninitialized — UB; the peak angle everywhere is the
         ! sensible 1D limit
         theta = theta_peak_deg*PI/180.0_SP
         ag = 1.0_SP
      else
         ! N is computed here, not before the branch (legacy divides
         ! 20/sigma ahead of its 1D branch — unobservable there)
         n_spec = int(20.0_SP/sigma_theta)
         displace = minloc(abs(freq - fm))
         idx_theta = mod(displace(1), ntheta)
         do kf = 1, nfreq
            ktheta_temp = real(mod(kf - idx_theta, ntheta), SP)
            if (ktheta_temp <= 0.0_SP) &
               ktheta_temp = ktheta_temp + real(ntheta, SP)
            if (mod(kf, 2) == 0) then
               sign_kf = 1.0_SP
            else
               sign_kf = -1.0_SP
            end if
            theta(kf) = sign_kf*(-PI/2.0_SP &
                                 + PI*real(floor(ktheta_temp/2.0_SP - 0.5_SP), SP) &
                                 /(real(ntheta, SP) - 1.0_SP))
            theta(kf) = theta(kf) + theta_peak_deg*PI/180.0_SP
            if (theta(kf) > 0.5_SP*PI) theta(kf) = 0.5_SP*PI
            if (theta(kf) < -0.5_SP*PI) theta(kf) = -0.5_SP*PI
            ag(kf) = 1.0_SP/(2.0_SP*PI)
            do k_n = 1, n_spec
               ag(kf) = ag(kf) &
                        + (1.0_SP/PI)*exp(-0.5_SP*(real(k_n, SP)*sigma_theta)**2) &
                        *cos(real(k_n, SP)*(theta(kf) - theta_peak_deg*PI/180.0_SP))
            end do
         end do
         ag = abs(ag)
      end if

      ! coherence shuffle: move components onto host frequencies until
      ! alpha_c percent share a frequency (Salatin 2021)
      alpha_c = alpha_c_in
      if (alpha_c > 100.0_SP) alpha_c = 100.0_SP
      if (alpha_c < 0.0_SP) alpha_c = 0.0_SP
      if (alpha_c > 0.0_SP) &
         call wave_coherence(alpha_c, freq, nfreq, ntheta, idx_theta, env)

      ! TMA densities on the (possibly moved) frequencies
      Ef = 0.0_SP
      do kf = 1, nfreq
         energy_bin(kf) = tma_density(.false., freq(kf), fm, h_gen, &
                                      gamma_spec)*df
         Ef = Ef + energy_bin(kf)
      end do

      alpha_spec = Hmo**2/16.0_SP/Ef
      correction_coeff = Ef/dot_product(ag, energy_bin)
      do kf = 1, nfreq
         ag(kf) = ag(kf)*correction_coeff
         hmo_each(kf) = 4.0_SP*sqrt((alpha_spec*energy_bin(kf)*ag(kf)))
      end do

      alpha1 = alpha + 1.0_SP/3.0_SP
      wave_length = 0.0_SP

      do kf = 1, nfreq
         ap = hmo_each(kf)/sqrt(2.0_SP)/2.0_SP
         omgn(kf) = 2.0_SP*PI*freq(kf)
         tb = omgn(kf)*omgn(kf)*h_gen/GRAV
         tc = 1.0_SP + tb*alpha
         wkn = sqrt((tc - sqrt(tc*tc - 4.0_SP*alpha1*tb))/(2.0_SP*alpha1))/h_gen

         if (wkn == 0.0_SP) then
            wkn = SMALL
            c_phase = sqrt(GRAV*h_gen)
            wave_length = c_phase/fm
         else
            ! legacy evaluates the phase speed through the PEAK
            ! frequency (fm cancels only in exact arithmetic)
            c_phase = 1.0_SP/wkn*fm*2.0_SP*PI
            wave_length = c_phase/fm
         end if

         if (periodic) then
            call wk_new_periodic_snap(theta(kf), wkn, dy, nglob, snapped)
            write (msg, '(A,F8.3,A,F8.3,A,F8.3)') &
               "WK_NEW_IRR periodic, freq: ", freq(kf), ", dir: ", &
               theta(kf)*180.0_SP/PI, " -> ", snapped*180.0_SP/PI
            call env%log%info(trim(msg))
            theta(kf) = snapped
         end if

         rlamda(kf) = wkn*sin(theta(kf))
         beta_gen(kf) = 80.0_SP/delta**2/wave_length**2
         rl_gen = wkn*cos(theta(kf))
         ri = sqrt(PI/beta_gen(kf))*exp(-rl_gen**2/4.0_SP/beta_gen(kf))
         D_gen(kf) = 2.0_SP*ap*cos(theta(kf)) &
                     *(omgn(kf)**2 - alpha1*GRAV*wkn**4*h_gen**3) &
                     /(omgn(kf)*wkn*ri*(1.0_SP - alpha*(wkn*h_gen)**2))
      end do

      ! legacy width recomputes the peak wavenumber but still uses the
      ! last component's wave_length — kept bug-for-bug
      width = delta*wave_length/2.0_SP

      if (BUILD_ZERO_PHASE) then
         phi1 = 0.0_SP
      else
         call random_number(phi1)
         phi1 = phi1*2.0_SP*PI
      end if

   end subroutine wk_new_irr_coefficients

   ! ----------------------------------------------------------------
   ! Private: coherence shuffle (legacy WAVE_COHERENCE, Salatin 2021).
   ! Host frequencies sit every ntheta-th component (anchored at the
   ! peak-frequency index, last component always a host); randomly
   ! drawn non-host components move UP to the nearest host frequency
   ! until alpha_c percent of components share a frequency.  Legacy
   ! draws with C rand() at its default seed (deterministic per libc,
   ! rank-consistent, even under ZERO_PHASE) — reproduced with a
   ! fixed-seed RANDOM_SEED so runs stay bitwise reproducible and
   ! rank-consistent; the legacy draw sequence itself is
   ! compiler-specific, so alpha_c > 0 has no legacy parity.
   ! ----------------------------------------------------------------
   subroutine wave_coherence(alpha_c, freq, nfreq, ntheta, idx_theta, env)
      real(SP), intent(in) :: alpha_c
      integer, intent(in) :: nfreq, ntheta, idx_theta
      real(SP), intent(inout) :: freq(nfreq)
      type(type_env), intent(inout) :: env

      real(SP) :: freq_temp(nfreq), host_freqs(nfreq)
      real(SP) :: pool(nfreq), r, cand, host_freq
      integer :: repetitions(nfreq), host_idx(nfreq)
      integer, allocatable :: seed(:)
      integer :: num_coherent, num_coherent_temp, nhost, npool
      integer :: kf, jj, cand_idx, hi, host_whole, seed_n

      call random_seed(size=seed_n)
      allocate (seed(seed_n), source=66)
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
   ! Private: dense modes for the WK_NEW family (legacy CALCULATE_NEW_
   ! Cm_Sm).  Components sharing an (adjacent) frequency accumulate
   ! into the FIRST slot of the run; duplicate slots stay zero and
   ! contribute exact zeros to the stage sum, so update_source keeps
   ! its full-kf loop (legacy iterates loop_index — numerically
   ! identical).  Non-adjacent duplicates are NOT merged (legacy
   ! compares neighbors only).
   ! ----------------------------------------------------------------
   subroutine calc_new_cm_sm(this, env, freq, D_gen, phase, rlamda, beta_gen)
      class(type_model_wavemaker), intent(inout) :: this
      type(type_env), intent(inout) :: env
      real(SP), intent(in) :: freq(:), D_gen(:), phase(:), rlamda(:), beta_gen(:)

      integer :: target_kf(this%Nfreq)
      integer :: i, j, kf, kt, ndistinct
      character(64) :: msg

      target_kf(1) = 1
      ndistinct = 1
      do kf = 2, this%Nfreq
         if (freq(kf) /= freq(kf - 1)) then
            ndistinct = ndistinct + 1
            target_kf(kf) = kf
         else
            target_kf(kf) = target_kf(kf - 1)
         end if
      end do
      write (msg, '(A,I0)') "number of distinct freqs: ", ndistinct
      call env%log%info(trim(msg))

      this%Cm = 0.0_SP
      this%Sm = 0.0_SP
      do kf = 1, this%Nfreq
         kt = target_kf(kf)
         do j = 1, size(this%Cm, 2)
            do i = 1, size(this%Cm, 1)
               this%Cm(i, j, kt) = this%Cm(i, j, kt) + D_gen(kf) &
                                   *exp(-beta_gen(kf)*(this%xmk_wk(i) - this%Xc_WK)**2) &
                                   *cos(rlamda(kf)*this%ymk_wk(j) + phase(kf))
               this%Sm(i, j, kt) = this%Sm(i, j, kt) + D_gen(kf) &
                                   *exp(-beta_gen(kf)*(this%xmk_wk(i) - this%Xc_WK)**2) &
                                   *sin(rlamda(kf)*this%ymk_wk(j) + phase(kf))
            end do
         end do
      end do

   end subroutine calc_new_cm_sm

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
