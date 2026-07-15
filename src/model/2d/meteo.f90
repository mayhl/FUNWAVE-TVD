!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Atmospheric forcing (legacy mod_meteo.F, METEO_MODULE)
!
!  Legacy METEO is four independent switched sub-models (dispatcher
!  METEO_FORCING): MeteoGausian (a moving Gaussian air-pressure pulse),
!  WindConstantField (a spatially uniform time-series wind), the Holland
!  hurricane (pressure + gradient wind), and a Slide/landslide source.  This
!  module ports them one at a time.
!    RUNG a: MeteoGausian     -- the moving pressure pulse (simple_cases case)
!    RUNG b: WindConstantField -- uniform wind stress from a time series
!    RUNG c: WindHollandModel  -- (pending)
!    RUNG d: SlideModel        -- (pending)
!
!  YAML block: meteo:                 (top-level; omit for no atmospheric forcing)
!    MeteoGausian:        <bool>   default NO   -- the moving pressure pulse
!    METEO_GAUSIAN_FILE:  <path>   storm track; required when MeteoGausian is on
!    WindConstantField:   <bool>   default NO   -- uniform time-series wind
!    CONSTANT_WIND_FILE:  <path>   wind series; required when WindConstantField
!    WindForce:           <bool>   default = WindConstantField (legacy fallback)
!    AirPressure:         <bool>   default NO   -- add the pressure gradient
!    WindWaveInteraction: <bool>   default NO   -- adjust wind by wave celerity
!    Cdw:                 <real>   default 0.002 -- wind drag coefficient
!    WindCrestPercent:    <real>   default LARGE -- crest-only wind mask cutoff
!    OUT_METEO:           <bool>   default YES  -- write the pressure field
!
!  Two coupling paths into the flow, both reproduced from sources.F:
!    1. air-pressure gradient  S += -g H grad(P), a whole-array add AFTER the
!       source loop (sources.F:529-533), gated on AirPressure;
!    2. wind stress  S += mask_wind * (rho_air/rho_water) * Cdw * W |W|, added
!       INSIDE the source loop after the breakwater term (sources.F:348-357),
!       gated on WindForce.
!
!  MeteoGausian: a storm-track file streams records (time, x, y, dP, SigmaX,
!  SigmaY, Theta); the two bracketing records are linearly interpolated in time
!  to a rotated 2D Gaussian air-pressure field, whose gradient forces the flow:
!    $$ P(x,y) = \Delta P\,\exp\!\big[-(a\,\Delta x^2 + 2b\,\Delta x\,\Delta y
!               + c\,\Delta y^2)\big]/100, $$
!    $$ a = \tfrac{\cos^2\theta}{2\sigma_x^2}+\tfrac{\sin^2\theta}{2\sigma_y^2},\;
!       b = \tfrac{\sin 2\theta}{4}\!\big(\tfrac1{\sigma_y^2}-\tfrac1{\sigma_x^2}\big),\;
!       c = \tfrac{\sin^2\theta}{2\sigma_x^2}+\tfrac{\cos^2\theta}{2\sigma_y^2}. $$
!
!  WindConstantField: a time series (time, WU, WV) is linearly interpolated to a
!  spatially uniform wind (WindU2D, WindV2D); optionally adjusted by the local
!  wave celerity (WindWaveInteraction, Chen et al. 2004) and masked to wave
!  crests (WindCrestPercent).  The stress then feeds the momentum source.
!
!  Bug-for-bug notes vs legacy:
!    NOTE 1: the pressure record advance shifts ONLY (t, x, y) into the low
!            bracket (mod_meteo.F:1061-1063 for MeteoGausian, and identically in
!            Holland_Model_Forcing); dP / SigmaX / SigmaY / Theta (and Holland's
!            Pn/Pc/A/B) are NOT shifted, so after the first advance they stay
!            frozen at the FIRST record's setup values.  A LEGACY BUG: latent
!            for a 2-record file (one interval), live for any 3+ record storm.
!            Reproduced.
!    NOTE 2: params are ZERO for TIME <= the first record time (both weights
!            stay 0), which drives SigmaX to 0 and legacy STOPs.  It does not
!            bite in practice because the first forcing call runs at an
!            already-advanced TIME > 0 that triggers the record advance first.
!    NOTE 3: dP is scaled by 1/100 ("cm to metre" per legacy) -- the file's mb
!            label is not honoured as a pressure unit; the number is used raw.
!    NOTE 4: the Gaussian is evaluated over the ghost cells too (full local
!            lattice); the gradient is a Cartesian centred difference off the
!            scalar grid%dx(1,1) -- ported Cartesian-only (spherical METEO is a
!            post-strip concern).
!    NOTE 5: the storm-file EOF freezes the bracket at its last record (legacy
!            END=120 leaves the slot-2 values in place); reproduced as the eof
!            flag -- no further advance, so the last field holds.
!    NOTE 6: the degrees->radians is legacy's `*PI/180.0` on the interpolated
!            angle (not a precomputed DEG2RAD parameter), kept for the identical
!            round-off.  The eta parity floor is the legacy single-precision PI
!            (mod_param.F), same class as the vessel port -- a ~2.8e-8 rel bias
!            that accumulates in the driven flow but hides below the ASCII output
!            floor of the pressure field itself.
!    NOTE 7: WindWaveInteraction and the WindCrestPercent crest mask couple the
!            wind to the RUNNING wave envelope (etat/etax/etay, etamean, h_max).
!            They are ported for completeness but no legacy case exercises them;
!            the crest mask additionally needs h_max, which the modern engine
!            only tracks under OUT_Hmax.  The parity cases keep
!            WindWaveInteraction off and WindCrestPercent = LARGE (mask == 1).
!            At istage 1 the derivative fields hold the previous step's values,
!            which is the legacy cadence (METEO_FORCING before the RK loop).
!    NOTE 8: the wind time-series index advances by at most one record per call
!            (a single IF, not a loop), and WindU2D/WindV2D stay ZERO until TIME
!            clears the first series time -- reproduced.
!
!  HISTORY :
!    07/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_meteo_mod
   use core_constants_mod, only: SP, ZERO, SMALL, LARGE, PI, GRAV, RHO_AW, RHO_AIR
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_METEO_METEOGAUSIAN, &
                                        DEF_METEO_OUT_METEO, &
                                        DEF_METEO_WINDCONSTANTFIELD, &
                                        DEF_METEO_WINDHOLLANDMODEL, &
                                        DEF_METEO_SLIDEMODEL, &
                                        DEF_METEO_WINDWAVEINTERACTION, &
                                        DEF_METEO_CDW

   implicit none

   private
   public :: type_model_meteo

   type, extends(type_model_base) :: type_model_meteo

      ! sub-model switches (legacy METEO_FORCING dispatcher)
      logical :: meteo_gausian = .false.
      logical :: wind_constant_field = .false.
      logical :: wind_holland_model = .false.
      logical :: slide_model = .false.

      ! shared knobs
      logical :: air_pressure = .false.          ! gate on the pressure add
      logical :: wind_force = .false.            ! gate on the wind stress add
      logical :: wind_wave_interaction = .false.
      real(SP) :: cdw = 0.002_SP
      real(SP) :: wind_crest_percent = LARGE
      logical :: out_meteo = .true.

      type(type_path) :: gausian_file
      type(type_path) :: constant_wind_file
      type(type_path) :: storm_file        ! Holland Pn/Pc/A/B track
      type(type_path) :: slide_file        ! landslide geometry + X/Y track

      ! slide geometry (legacy LengthSlide/WidthSlide/AlphaSlide/BetaSlide/
      ! PSlide) and the sech-shape parameter epsilon; first_call seeds eta once
      real(SP) :: length_slide = ZERO, width_slide = ZERO
      real(SP) :: alpha_slide = ZERO, beta_slide = ZERO, p_slide = ZERO
      real(SP) :: epsilon = ZERO
      logical :: first_call = .true.

      ! two-record storm-track bracket (legacy TimeStorm1/2, Xstorm1/2, ...).
      ! NOTE 1: only t/x/y advance into the low slot; the shape params do not
      real(SP) :: t1 = ZERO, t2 = ZERO
      real(SP) :: x1 = ZERO, x2 = ZERO, y1 = ZERO, y2 = ZERO
      real(SP) :: dp1 = ZERO, dp2 = ZERO
      real(SP) :: sigx1 = ZERO, sigx2 = ZERO, sigy1 = ZERO, sigy2 = ZERO
      real(SP) :: th1 = ZERO, th2 = ZERO
      ! Holland shape bracket (Pn/Pc/A/B); frozen after the first advance too
      real(SP) :: pn1 = ZERO, pn2 = ZERO, pc1 = ZERO, pc2 = ZERO
      real(SP) :: ast1 = ZERO, ast2 = ZERO, bst1 = ZERO, bst2 = ZERO
      integer :: unit_track = -1        ! -1 marks never-opened
      logical :: eof = .false.

      ! constant-wind time series (legacy TimeWind/WU/WV)
      real(SP), allocatable :: time_wind(:), wu(:), wv(:)
      integer :: num_time_wind = 0
      integer :: icount_wind = 1

      ! ghost-inclusive grid-point coordinates (legacy Xco/Yco)
      real(SP), allocatable :: xco(:), yco(:)
      real(SP) :: dx0 = ZERO, dy0 = ZERO

      ! the pressure field and its gradient forcing (StormPressureTotal/X/Y)
      real(SP), allocatable :: p_total(:, :), p_x(:, :), p_y(:, :)

      ! the wind field, its crest mask, and the precomputed stress source
      ! (legacy WindU2D/WindV2D/MASK_WIND and the sources.F inline stress)
      real(SP), allocatable :: wind_u(:, :), wind_v(:, :)
      integer, allocatable :: mask_wind(:, :)
      real(SP), allocatable :: wind_sx(:, :), wind_sy(:, :)

      ! cached interior window
      integer :: ib = 0, ie = 0, jb = 0, je = 0
      integer :: mloc = 0, nloc = 0

   contains
      procedure :: read_input => meteo_read_input
      procedure :: init_compute => meteo_init_compute
      procedure :: update => meteo_update
      procedure :: free => meteo_free
   end type type_model_meteo

contains

   subroutine meteo_read_input(this, env)
      class(type_model_meteo), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk, no_key

      sub_env = get_sub_env(env, "meteo", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (no_blk) return

      ! sub-model switches (legacy reads all four up front, defaults NO)
      call sub_env%yaml%read("WindConstantField", silent=no_key, &
                             val=this%wind_constant_field, &
                             default=DEF_METEO_WINDCONSTANTFIELD)
      call sub_env%yaml%read("WindHollandModel", silent=no_key, &
                             val=this%wind_holland_model, &
                             default=DEF_METEO_WINDHOLLANDMODEL)
      call sub_env%yaml%read("MeteoGausian", silent=no_key, &
                             val=this%meteo_gausian, &
                             default=DEF_METEO_METEOGAUSIAN)
      call sub_env%yaml%read("SlideModel", silent=no_key, &
                             val=this%slide_model, &
                             default=DEF_METEO_SLIDEMODEL)

      ! MeteoGausian and SlideModel force the pressure path on (mod_meteo.F:206,
      ! and Slide_Model_Setup sets AirPressure = .TRUE.)
      if (this%meteo_gausian) this%air_pressure = .true.
      if (this%slide_model) this%air_pressure = .true.

      ! wind + pressure knobs, read only when a wind model is on (legacy gate)
      if (this%wind_holland_model .or. this%wind_constant_field) then
         call sub_env%yaml%read("WindWaveInteraction", silent=no_key, &
                                val=this%wind_wave_interaction, &
                                default=DEF_METEO_WINDWAVEINTERACTION)
         ! AirPressure: user switch (default off); consumed silent so an absent
         ! key leaves the initializer, matching the legacy ierr default
         call sub_env%yaml%read("AirPressure", silent=no_key, &
                                val=this%air_pressure)
         if (no_key) this%air_pressure = .false.
         ! WindForce: absent -> TRUE for a constant wind field, else FALSE
         call sub_env%yaml%read("WindForce", silent=no_key, val=this%wind_force)
         if (no_key) this%wind_force = this%wind_constant_field
         if (this%wind_force) then
            call sub_env%yaml%read("Cdw", silent=no_key, val=this%cdw, &
                                   default=DEF_METEO_CDW)
            ! WindCrestPercent: absent OR interaction-off -> LARGE (mask == 1)
            call sub_env%yaml%read("WindCrestPercent", silent=no_key, &
                                   val=this%wind_crest_percent)
            if (no_key .or. .not. this%wind_wave_interaction) &
               this%wind_crest_percent = LARGE
         end if
      end if

      call sub_env%yaml%read("OUT_METEO", silent=no_key, &
                             val=this%out_meteo, &
                             default=DEF_METEO_OUT_METEO)

      ! per-model input files
      if (this%meteo_gausian) then
         call sub_env%yaml%read_input_path("METEO_GAUSIAN_FILE", silent=no_key, &
                                           val=this%gausian_file)
         if (no_key) error stop &
            "meteo: METEO_GAUSIAN_FILE is required when MeteoGausian is on"
      end if
      if (this%wind_constant_field) then
         call sub_env%yaml%read_input_path("CONSTANT_WIND_FILE", silent=no_key, &
                                           val=this%constant_wind_file)
         if (no_key) error stop &
            "meteo: CONSTANT_WIND_FILE is required when WindConstantField is on"
      end if
      if (this%wind_holland_model) then
         call sub_env%yaml%read_input_path("STORM_FILE", silent=no_key, &
                                           val=this%storm_file)
         if (no_key) error stop &
            "meteo: STORM_FILE is required when WindHollandModel is on"
      end if
      if (this%slide_model) then
         call sub_env%yaml%read_input_path("SLIDE_FILE", silent=no_key, &
                                           val=this%slide_file)
         if (no_key) error stop &
            "meteo: SLIDE_FILE is required when SlideModel is on"
      end if

   end subroutine meteo_read_input

   ! ----------------------------------------------------------------
   ! Legacy METEO_INITIAL: build the ghost-inclusive Xco/Yco lattice (any
   ! spatial pressure model), allocate the coupling fields, and run the
   ! per-model setups (open files, seed the first record / read the series).
   ! ----------------------------------------------------------------
   subroutine meteo_init_compute(this, grid)
      class(type_model_meteo), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      integer :: i, j
      logical :: need_lattice, need_pressure, need_wind

      if (.not. this%is_activated) return

      need_pressure = this%meteo_gausian .or. this%wind_holland_model &
                      .or. this%slide_model
      need_wind = this%wind_constant_field .or. this%wind_holland_model
      ! Xco/Yco are used by the spatial pressure models; a uniform wind does not
      need_lattice = need_pressure

      associate (lp => grid%lp)
         this%ib = lp%ib
         this%ie = lp%ie
         this%jb = lp%jb
         this%je = lp%je
         this%mloc = lp%mloc
         this%nloc = lp%nloc
         this%dx0 = grid%dx(1, 1)
         this%dy0 = grid%dy(1, 1)

         if (need_lattice) then
            ! ghost-inclusive grid-point lattice; grid%x/grid%dx are interior-only
            allocate (this%xco(lp%mloc), this%yco(lp%nloc))
            this%xco(lp%ib) = real(grid%ibegin - 1, SP)*this%dx0
            do i = lp%ib + 1, lp%mloc
               this%xco(i) = this%xco(i - 1) + this%dx0
            end do
            do i = lp%ib - 1, 1, -1
               this%xco(i) = this%xco(i + 1) - this%dx0
            end do
            this%yco(lp%jb) = real(grid%jbegin - 1, SP)*this%dy0
            do j = lp%jb + 1, lp%nloc
               this%yco(j) = this%yco(j - 1) + this%dy0
            end do
            do j = lp%jb - 1, 1, -1
               this%yco(j) = this%yco(j + 1) - this%dy0
            end do
         end if

         if (need_pressure) then
            allocate (this%p_total(lp%mloc, lp%nloc), source=ZERO)
            allocate (this%p_x(lp%mloc, lp%nloc), source=ZERO)
            allocate (this%p_y(lp%mloc, lp%nloc), source=ZERO)
         end if

         if (need_wind) then
            allocate (this%wind_u(lp%mloc, lp%nloc), source=ZERO)
            allocate (this%wind_v(lp%mloc, lp%nloc), source=ZERO)
            allocate (this%mask_wind(lp%mloc, lp%nloc), source=0)
            allocate (this%wind_sx(lp%mloc, lp%nloc), source=ZERO)
            allocate (this%wind_sy(lp%mloc, lp%nloc), source=ZERO)
         end if
      end associate

      if (this%meteo_gausian) call gausian_setup(this)
      if (this%wind_constant_field) call constant_wind_setup(this)
      if (this%wind_holland_model) call holland_setup(this)
      if (this%slide_model) call slide_setup(this)

   end subroutine meteo_init_compute

   ! Open the storm track, skip its three banner lines, read the first record
   ! into slot2, then copy ALL seven fields to slot1 (the only place dp/sigx/
   ! sigy/th ever reach the low slot -- NOTE 1).
   subroutine gausian_setup(this)
      class(type_model_meteo), intent(inout) :: this
      character(len=80) :: header

      open (newunit=this%unit_track, file=this%gausian_file%root, &
            status='old', action='read')
      read (this%unit_track, *) header                  ! title
      read (this%unit_track, *) header                  ! storm name
      read (this%unit_track, *) header                  ! column banner
      read (this%unit_track, *) this%t2, this%x2, this%y2, &
         this%dp2, this%sigx2, this%sigy2, this%th2

      this%t1 = this%t2
      this%x1 = this%x2
      this%y1 = this%y2
      this%dp1 = this%dp2
      this%sigx1 = this%sigx2
      this%sigy1 = this%sigy2
      this%th1 = this%th2
   end subroutine gausian_setup

   ! Open the Holland track, skip its three banner lines, read the first record
   ! (Time, X, Y, Pn, Pc, A, B) into slot2, then copy ALL seven to slot1 (the
   ! only place Pn/Pc/A/B ever reach the low slot -- NOTE 1).
   subroutine holland_setup(this)
      class(type_model_meteo), intent(inout) :: this
      character(len=80) :: header

      open (newunit=this%unit_track, file=this%storm_file%root, &
            status='old', action='read')
      read (this%unit_track, *) header                  ! title
      read (this%unit_track, *) header                  ! storm name
      read (this%unit_track, *) header                  ! column banner
      read (this%unit_track, *) this%t2, this%x2, this%y2, &
         this%pn2, this%pc2, this%ast2, this%bst2

      this%t1 = this%t2
      this%x1 = this%x2
      this%y1 = this%y2
      this%pn1 = this%pn2
      this%pc1 = this%pc2
      this%ast1 = this%ast2
      this%bst1 = this%bst2
   end subroutine holland_setup

   ! Legacy Slide_Model_Setup: epsilon = 0.717, open the slide file, read its
   ! geometry (L/W/Alpha/Beta/P) and first (time, x, y) into slot2, copy t/x/y
   ! to slot1.  AirPressure is already forced on in read_input.
   subroutine slide_setup(this)
      class(type_model_meteo), intent(inout) :: this
      character(len=80) :: header

      this%epsilon = 0.717_SP

      open (newunit=this%unit_track, file=this%slide_file%root, &
            status='old', action='read')
      read (this%unit_track, *) header                  ! title
      read (this%unit_track, *) header                  ! slide name
      read (this%unit_track, *) header                  ! geometry banner
      read (this%unit_track, *) this%length_slide, this%width_slide, &
         this%alpha_slide, this%beta_slide, this%p_slide
      read (this%unit_track, *) header                  ! t,x,y banner
      read (this%unit_track, *) this%t2, this%x2, this%y2

      this%t1 = this%t2
      this%x1 = this%x2
      this%y1 = this%y2
   end subroutine slide_setup

   ! Open the wind file, read the record count and the (time, WU, WV) series.
   subroutine constant_wind_setup(this)
      class(type_model_meteo), intent(inout) :: this
      character(len=80) :: header
      integer :: i, ios

      open (newunit=this%unit_track, file=this%constant_wind_file%root, &
            status='old', action='read')
      read (this%unit_track, *) header                  ! title
      read (this%unit_track, *) this%num_time_wind
      allocate (this%time_wind(this%num_time_wind), source=ZERO)
      allocate (this%wu(this%num_time_wind), source=ZERO)
      allocate (this%wv(this%num_time_wind), source=ZERO)
      do i = 1, this%num_time_wind
         read (this%unit_track, *, iostat=ios) this%time_wind(i), &
            this%wu(i), this%wv(i)
         if (ios /= 0) exit                             ! legacy END=111
      end do
      close (this%unit_track)
      this%unit_track = -1
   end subroutine constant_wind_setup

   ! ----------------------------------------------------------------
   ! Legacy METEO_FORCING dispatcher at the already-advanced TIME.  The wave
   ! fields (eta/etax/etay/etat/etamean/h_max) feed only the wind-wave and
   ! crest-mask refinements (NOTE 7); MeteoGausian ignores them.
   ! ----------------------------------------------------------------
   subroutine meteo_update(this, time, h, eta, eta0, etax, etay, etat, &
                           etamean, h_max)
      class(type_model_meteo), intent(inout) :: this
      real(SP), intent(in) :: time
      real(SP), intent(in) :: h(:, :)
      ! eta/eta0 are inout: the slide seeds the initial surface on its first
      ! call (legacy Eta = Eta0 = -StormPressureTotal); the wind models only read
      real(SP), intent(inout) :: eta(:, :), eta0(:, :)
      real(SP), intent(in) :: etax(:, :), etay(:, :), etat(:, :)
      real(SP), intent(in) :: etamean(:, :), h_max(:, :)

      if (.not. this%is_activated) return

      if (this%meteo_gausian) call gausian_forcing(this, time, h)
      if (this%wind_constant_field) &
         call constant_wind_forcing(this, time, h, eta, etax, etay, etat, &
                                    etamean, h_max)
      if (this%wind_holland_model) &
         call holland_forcing(this, time, h, eta, etax, etay, etat, &
                              etamean, h_max)
      if (this%slide_model) call slide_forcing(this, time, h, eta, eta0)

   end subroutine meteo_update

   ! Legacy MeteoGausian_Forcing: one optional record advance (t/x/y only,
   ! NOTE 1), linear-in-time blend, the rotated Gaussian pressure, then the
   ! -g*H*grad forcing.
   subroutine gausian_forcing(this, time, h)
      class(type_model_meteo), intent(inout) :: this
      real(SP), intent(in) :: time, h(:, :)

      real(SP) :: w1, w2, xs, ys, dps, sigx, sigy, theta, a, b, c
      integer :: i, j, ios

      this%p_total = ZERO

      if (.not. this%eof) then
         if (time > this%t1 .and. time > this%t2) then
            ! NOTE 1: only t/x/y move into the low slot; dp/sigx/sigy/th do not
            this%t1 = this%t2
            this%x1 = this%x2
            this%y1 = this%y2
            read (this%unit_track, *, iostat=ios) this%t2, this%x2, this%y2, &
               this%dp2, this%sigx2, this%sigy2, this%th2
            if (ios /= 0) this%eof = .true.
         end if
      end if

      w2 = ZERO
      w1 = ZERO
      if (time > this%t1) then
         if (this%t1 == this%t2) then
            w2 = ZERO
            w1 = ZERO
         else
            w2 = (this%t2 - time)/max(SMALL, abs(this%t2 - this%t1))
            w1 = 1.0_SP - w2
         end if
      end if

      xs = this%x2*w1 + this%x1*w2
      ys = this%y2*w1 + this%y1*w2
      dps = this%dp2*w1 + this%dp1*w2
      sigx = this%sigx2*w1 + this%sigx1*w2
      sigy = this%sigy2*w1 + this%sigy1*w2
      theta = (this%th2*w1 + this%th1*w2)*PI/180.0_SP     ! NOTE 6

      if (sigx == ZERO .or. sigy == ZERO) &
         error stop "meteo: SigmaX or SigmaY is zero"

      a = (cos(theta))**2/2.0_SP/sigx**2 &
          + (sin(theta))**2/2.0_SP/sigy**2
      b = -sin(2.0_SP*theta)/4.0_SP/sigx**2 &
          + sin(2.0_SP*theta)/4.0_SP/sigy**2
      c = (sin(theta))**2/2.0_SP/sigx**2 &
          + (cos(theta))**2/2.0_SP/sigy**2

      ! full local lattice, ghost cells included (NOTE 4)
      do j = 1, this%nloc
         do i = 1, this%mloc
            this%p_total(i, j) = dps*exp(-(a*(this%xco(i) - xs)**2 &
                                           + 2.0_SP*b*(this%xco(i) - xs)*(this%yco(j) - ys) &
                                           + c*(this%yco(j) - ys)**2))/100.0_SP
         end do
      end do

      call pressure_gradient(this, h)

   end subroutine gausian_forcing

   ! Legacy Constant_Wind_Forcing: advance the series index by one, blend WU/WV
   ! into a uniform wind, optionally adjust by the wave celerity, build the
   ! crest mask, then precompute the wind stress source.
   subroutine constant_wind_forcing(this, time, h, eta, etax, etay, etat, &
                                    etamean, h_max)
      class(type_model_meteo), intent(inout) :: this
      real(SP), intent(in) :: time, h(:, :), eta(:, :)
      real(SP), intent(in) :: etax(:, :), etay(:, :), etat(:, :)
      real(SP), intent(in) :: etamean(:, :), h_max(:, :)

      real(SP) :: w2, t1, celerity, angle
      integer :: i, j

      if (.not. this%wind_force) return

      ! NOTE 8: advance by at most one record; wind stays ZERO before series
      if (time > this%time_wind(this%icount_wind) .and. &
          this%icount_wind < this%num_time_wind) &
         this%icount_wind = this%icount_wind + 1

      if (this%icount_wind > 1) then
         if (time > this%time_wind(this%icount_wind)) then
            w2 = ZERO
         else
            w2 = (this%time_wind(this%icount_wind) - time) &
                 /(this%time_wind(this%icount_wind) &
                   - this%time_wind(this%icount_wind - 1))
         end if
         this%wind_u = this%wu(this%icount_wind)*(1.0_SP - w2) &
                       + this%wu(this%icount_wind - 1)*w2
         this%wind_v = this%wv(this%icount_wind)*(1.0_SP - w2) &
                       + this%wv(this%icount_wind - 1)*w2
      end if

      ! wave-celerity adjustment, Chen et al. 2004 (NOTE 7)
      if (this%wind_wave_interaction) then
         do j = 1, this%nloc
            do i = 1, this%mloc
               t1 = max(sqrt(etax(i, j)*etax(i, j) + etay(i, j)*etay(i, j)), SMALL)
               celerity = min(abs(etat(i, j))/t1, sqrt(GRAV*abs(h(i, j))))
               angle = atan2(etay(i, j), etax(i, j))
               this%wind_u(i, j) = this%wind_u(i, j) - celerity*cos(angle)
               this%wind_v(i, j) = this%wind_v(i, j) - celerity*sin(angle)
            end do
         end do
      end if

      call crest_mask(this, eta, h_max, etamean)
      call wind_stress(this)

   end subroutine constant_wind_forcing

   ! Legacy Holland_Model_Forcing: one optional record advance (t/x/y only,
   ! NOTE 1), linear-in-time blend of the Pn/Pc/A/B shape, then the radial
   ! Holland pressure Pw and gradient wind Vw over the lattice.  Feeds both the
   ! pressure path (if AirPressure) and the wind path (if WindForce).
   subroutine holland_forcing(this, time, h, eta, etax, etay, etat, &
                              etamean, h_max)
      class(type_model_meteo), intent(inout) :: this
      real(SP), intent(in) :: time, h(:, :), eta(:, :)
      real(SP), intent(in) :: etax(:, :), etay(:, :), etat(:, :)
      real(SP), intent(in) :: etamean(:, :), h_max(:, :)

      real(SP) :: w1, w2, xs, ys, pn, pc, ast, bst
      real(SP) :: rdis, expt, pw, vw, angle, t1, celerity, waveangle
      integer :: i, j, ios

      this%p_total = ZERO

      if (.not. this%eof) then
         if (time > this%t1 .and. time > this%t2) then
            ! NOTE 1: only t/x/y move into the low slot; Pn/Pc/A/B do not
            this%t1 = this%t2
            this%x1 = this%x2
            this%y1 = this%y2
            read (this%unit_track, *, iostat=ios) this%t2, this%x2, this%y2, &
               this%pn2, this%pc2, this%ast2, this%bst2
            if (ios /= 0) this%eof = .true.
         end if
      end if

      w2 = ZERO
      w1 = ZERO
      if (time > this%t1) then
         if (this%t1 == this%t2) then
            w2 = ZERO
            w1 = ZERO
         else
            w2 = (this%t2 - time)/max(SMALL, abs(this%t2 - this%t1))
            w1 = 1.0_SP - w2
         end if
      end if

      xs = this%x2*w1 + this%x1*w2
      ys = this%y2*w1 + this%y1*w2
      pn = this%pn2*w1 + this%pn1*w2
      pc = this%pc2*w1 + this%pc1*w2
      ast = this%ast2*w1 + this%ast1*w2
      bst = this%bst2*w1 + this%bst1*w2

      ! Holland radial pressure (mb) and gradient wind (m/s), full lattice
      do j = 1, this%nloc
         do i = 1, this%mloc
            rdis = sqrt((this%xco(i) - xs)**2 + (this%yco(j) - ys)**2)/1000.0_SP
            rdis = max(SMALL, rdis)                        ! km
            expt = exp(-ast/rdis**bst)
            pw = pc + (pn - pc)*expt
            if (this%air_pressure) this%p_total(i, j) = pw/100.0_SP   ! NOTE 3
            if (this%wind_force) then
               vw = sqrt(ast*bst*100.0_SP*abs(pn - pc)*expt/RHO_AIR/rdis**bst)
               angle = atan2(this%xco(i) - xs, this%yco(j) - ys)
               if (this%wind_wave_interaction) then
                  t1 = max(sqrt(etax(i, j)*etax(i, j) + etay(i, j)*etay(i, j)), SMALL)
                  celerity = min(abs(etat(i, j))/t1, sqrt(GRAV*abs(h(i, j))))
                  waveangle = atan2(etay(i, j), etax(i, j))
                  this%wind_u(i, j) = -vw*cos(angle) - celerity*cos(waveangle)
                  this%wind_v(i, j) = vw*sin(angle) - celerity*sin(waveangle)
               else
                  this%wind_u(i, j) = -vw*cos(angle)
                  this%wind_v(i, j) = vw*sin(angle)
               end if
            end if
         end do
      end do

      if (this%wind_force) then
         call crest_mask(this, eta, h_max, etamean)
         call wind_stress(this)
      end if
      if (this%air_pressure) call pressure_gradient(this, h)

   end subroutine holland_forcing

   ! Legacy Slide_Model_Forcing: advance the (x,y) track (t/x/y only, NOTE 1),
   ! build the moving sech^2 pressure bump, seed the initial surface on the
   ! first call (Eta = Eta0 = -P), then the -g*H*grad forcing.
   subroutine slide_forcing(this, time, h, eta, eta0)
      class(type_model_meteo), intent(inout) :: this
      real(SP), intent(in) :: time, h(:, :)
      real(SP), intent(inout) :: eta(:, :), eta0(:, :)

      real(SP) :: w1, w2, xs, ys, cc, kb, kw, sech1, sech2
      integer :: i, j, ios

      this%p_total = ZERO

      if (.not. this%eof) then
         if (time > this%t1 .and. time > this%t2) then
            this%t1 = this%t2
            this%x1 = this%x2
            this%y1 = this%y2
            read (this%unit_track, *, iostat=ios) this%t2, this%x2, this%y2
            if (ios /= 0) this%eof = .true.
         end if
      end if

      w2 = ZERO
      w1 = ZERO
      if (time > this%t1) then
         if (this%t1 == this%t2) then
            w2 = ZERO
            w1 = ZERO
         else
            w2 = (this%t2 - time)/max(SMALL, abs(this%t2 - this%t1))
            w1 = 1.0_SP - w2
         end if
      end if

      xs = this%x2*w1 + this%x1*w2
      ys = this%y2*w1 + this%y1*w2

      cc = acosh(1.0_SP/this%epsilon)
      kb = 2.0_SP*cc/max(SMALL, this%width_slide)
      kw = 2.0_SP*cc/max(SMALL, this%length_slide)

      do i = 1, this%mloc
         sech1 = kw*(this%xco(i) - xs)
         do j = 1, this%nloc
            sech2 = kb*(this%yco(j) - ys)
            this%p_total(i, j) = (this%p_slide/(1.0_SP - this%epsilon)) &
                                 *((1.0_SP/cosh(sech1))*(1.0_SP/cosh(sech2)) &
                                   - this%epsilon)
            if (this%p_total(i, j) < 0.0_SP) this%p_total(i, j) = 0.0_SP
         end do
      end do

      if (this%first_call) then
         eta = -this%p_total
         eta0 = -this%p_total
         this%first_call = .false.
      end if

      call pressure_gradient(this, h)

   end subroutine slide_forcing

   ! Crest-only wind mask (NOTE 7): everywhere on when WindCrestPercent == LARGE,
   ! else off wherever the surface sits below the crest cutoff.
   subroutine crest_mask(this, eta, h_max, etamean)
      class(type_model_meteo), intent(inout) :: this
      real(SP), intent(in) :: eta(:, :), h_max(:, :), etamean(:, :)
      integer :: i, j

      this%mask_wind = 1
      if (this%wind_crest_percent /= LARGE) then
         do j = 1, this%nloc
            do i = 1, this%mloc
               if (eta(i, j) < h_max(i, j)*(1.0_SP - this%wind_crest_percent) &
                   + etamean(i, j)) this%mask_wind(i, j) = 0
            end do
         end do
      end if
   end subroutine crest_mask

   ! -g H grad(P) into the pressure source, centred on the scalar dx/dy.
   subroutine pressure_gradient(this, h)
      class(type_model_meteo), intent(inout) :: this
      real(SP), intent(in) :: h(:, :)
      integer :: i, j

      do j = this%jb, this%je
         do i = this%ib, this%ie
            this%p_x(i, j) = -GRAV*h(i, j) &
                             *(this%p_total(i + 1, j) - this%p_total(i - 1, j)) &
                             /2.0_SP/this%dx0
            this%p_y(i, j) = -GRAV*h(i, j) &
                             *(this%p_total(i, j + 1) - this%p_total(i, j - 1)) &
                             /2.0_SP/this%dy0
         end do
      end do
   end subroutine pressure_gradient

   ! Precompute the wind stress the source loop adds (sources.F:350-355):
   !   mask * (rho_air/rho_water) * Cdw * W |W|, in legacy's operand order.
   subroutine wind_stress(this)
      class(type_model_meteo), intent(inout) :: this
      real(SP) :: spd
      integer :: i, j

      do j = this%jb, this%je
         do i = this%ib, this%ie
            spd = sqrt(this%wind_u(i, j)*this%wind_u(i, j) &
                       + this%wind_v(i, j)*this%wind_v(i, j))
            this%wind_sx(i, j) = this%mask_wind(i, j)*RHO_AW*this%cdw &
                                 *this%wind_u(i, j)*spd
            this%wind_sy(i, j) = this%mask_wind(i, j)*RHO_AW*this%cdw &
                                 *this%wind_v(i, j)*spd
         end do
      end do
   end subroutine wind_stress

   subroutine meteo_free(this)
      class(type_model_meteo), intent(inout) :: this

      if (this%unit_track /= -1) close (this%unit_track)
      this%unit_track = -1
      if (allocated(this%xco)) deallocate (this%xco, this%yco)
      if (allocated(this%p_total)) deallocate (this%p_total, this%p_x, this%p_y)
      if (allocated(this%wind_u)) deallocate (this%wind_u, this%wind_v, &
                                              this%mask_wind, this%wind_sx, &
                                              this%wind_sy)
      if (allocated(this%time_wind)) deallocate (this%time_wind, this%wu, this%wv)
   end subroutine meteo_free

end module model_meteo_mod
