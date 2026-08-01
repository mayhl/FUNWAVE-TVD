!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Initial-condition parameters YAML reader + t=0 state fill
!
!  Split out of the wavemaker (nee INI_*/N_WAVE types): a one-shot
!  eta/u/v fill at t=0 via apply_ic(); no per-step work.  Coexists
!  with a wavemaker: section — the sections are independent.
!
!  YAML block: initial:         (block presence selects the type)
!    solitary:  {amplitude, depth, x_center, direction: +x|-x,
!                angle, y_center}
!    sine_mode: {amplitude, depth, mode_x (default 1), mode_y (default 0)}
!    fields:    {eta, u, v, format} — t=0 fields from file (file_spec
!               refs; the IC-flavored INITIAL_UVZ, the restart-flavored
!               twin stays in hot_start:).  A deformed bed belongs to
!               grid.bathymetry, not here.
!    hump:      pending (INI_REC/GAU/DIP not in apply_ic yet)
!    n_wave:    pending
!  The still-water offset lives in grid.water_level (it survives
!  hotstart, so it is not an IC); a water_level key here errors.
!
!  HISTORY :
!    07/21/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_initial_mod
   use core_constants_mod, only: SP, PI, DEG2RAD
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base
   use model_field_input_mod, only: type_file_spec, parse_file_spec

   use model_config_defaults_mod, only: DEF_INITIAL_SINE_MODE_AMPLITUDE, &
                                        DEF_INITIAL_SINE_MODE_DEPTH, &
                                        DEF_INITIAL_SINE_MODE_MODE_X, &
                                        DEF_INITIAL_SINE_MODE_MODE_Y, &
                                        DEF_INITIAL_SOLITARY_AMPLITUDE, &
                                        DEF_INITIAL_SOLITARY_DEPTH, &
                                        DEF_INITIAL_SOLITARY_DIRECTION, &
                                        DEF_INITIAL_SOLITARY_X_CENTER, &
                                        DEF_INITIAL_SOLITARY_Y_CENTER

   implicit none

   private
   public :: type_model_initial
   public :: solitary_coefficients

   type, extends(type_model_base) :: type_model_initial

      ! Selected IC (internal strings, nee wavemaker types)
      character(:), allocatable :: ic_type

      ! Solitary wave — INI_SOLITARY
      real(SP) :: AMP_SOLI = 0.0_SP   ! YAML key: amplitude
      real(SP) :: DEP_SOLI = 0.0_SP   ! YAML key: depth
      real(SP) :: XWAVEMAKER = 0.0_SP
      logical  :: SolitaryPositiveDirection = .true.
      ! Oblique solitary (angle: present): crest angle from +x and the crest
      ! line's y anchor.  The doubly-periodic tiled-train IC evaluates the
      ! profile at the wrapped phase, so the box must tile: Lx cos = p*P,
      ! Ly sin = q*P (checked at apply_ic).  angle < 0 sentinel = straight
      ! legacy path.
      real(SP) :: solitary_angle = -1.0_SP  ! degrees
      real(SP) :: solitary_yc = 0.0_SP

      ! Standing wave — INI_SINE.  Basin-mode numbers along x and y; the
      ! default (1, 0) is the 1D fundamental seiche, mode_y >= 1 gives an
      ! oblique (diagonal) 2D standing wave for the isotropy check.
      integer  :: MODE_X = 1   ! YAML key: mode_x
      integer  :: MODE_Y = 0   ! YAML key: mode_y

      ! Hump ICs — INI_REC, INI_GAU, INI_DIP (pending apply_ic port)
      real(SP) :: Xc = 0.0_SP
      real(SP) :: Yc = 0.0_SP
      real(SP) :: WID = 0.0_SP

      ! N-wave — N_WAVE (pending apply_ic port)
      real(SP) :: x1_Nwave = 0.0_SP
      real(SP) :: x2_Nwave = 0.0_SP
      real(SP) :: a0_Nwave = 0.0_SP
      real(SP) :: gamma_Nwave = 0.0_SP
      real(SP) :: dep_Nwave = 0.0_SP

      ! t=0 fields from file — INI_FIELDS.  main loads them after
      ! apply_ic's zeroing (apply_ic itself stays analytic-only)
      logical :: has_fields = .false.
      logical :: fields_no_uv = .true.
      type(type_file_spec) :: eta_spec
      type(type_file_spec) :: u_spec
      type(type_file_spec) :: v_spec

   contains
      procedure :: read_input => initial_read_input
      procedure :: apply_ic => initial_apply_ic
   end type type_model_initial

contains

   ! ----------------------------------------------------------------
   ! initial: section — block presence selects the IC type.  solitary
   ! + sine_mode are live in apply_ic; hump + n_wave gate pending.
   ! water_level moved to grid: — a key here gets a redirect error, not
   ! the silent-ignore the YAML layer would otherwise give a dead key.
   ! ----------------------------------------------------------------
   subroutine initial_read_input(this, env)
      use core_yaml_file_mod, only: type_yaml_reader
      class(type_model_initial), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: ini_env
      type(type_yaml_reader) :: blk_yaml
      character(:), allocatable :: direction
      real(SP) :: water_level
      logical :: no_ini, no_blk, no_key

      this%ic_type = "none"
      this%is_activated = .false.
      ini_env = get_sub_env(env, "initial", no_ini)
      if (no_ini) return

      call ini_env%yaml%read("water_level", silent=no_key, val=water_level, &
                             default="0.0")
      if (.not. no_key) then
         call env%log%exit_on_error("initial/water_level: moved — set"// &
                                    " grid.water_level instead")
      end if

      blk_yaml = ini_env%yaml%cast_dictionary("solitary", no_blk)
      if (.not. no_blk) then
         this%is_activated = .true.
         this%ic_type = "INI_SOLITARY"
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
         ! oblique tiled-train variant — presence-selected; read into a
         ! temp so a missing key preserves the -1 straight-path sentinel
         block
            real(SP) :: tmp_ang
            call blk_yaml%read("angle", silent=no_key, val=tmp_ang)
            if (.not. no_key) then
               if (tmp_ang <= 0.0_SP .or. tmp_ang >= 90.0_SP) &
                  call env%log%exit_on_error( &
                  "initial/solitary/angle: expected 0 < angle < 90 deg")
               this%solitary_angle = tmp_ang
               call blk_yaml%read("y_center", silent=no_key, &
                                  val=this%solitary_yc, &
                                  default=DEF_INITIAL_SOLITARY_Y_CENTER)
            end if
         end block
      end if

      blk_yaml = ini_env%yaml%cast_dictionary("sine_mode", no_blk)
      if (.not. no_blk) then
         if (this%is_activated) call env%log%exit_on_error( &
            "initial: only one initial-condition block allowed")
         this%is_activated = .true.
         this%ic_type = "INI_SINE"
         call blk_yaml%read("amplitude", silent=no_key, val=this%AMP_SOLI, &
                            default=DEF_INITIAL_SINE_MODE_AMPLITUDE)
         call blk_yaml%read("depth", silent=no_key, val=this%DEP_SOLI, &
                            default=DEF_INITIAL_SINE_MODE_DEPTH)
         call blk_yaml%read("mode_x", silent=no_key, val=this%MODE_X, &
                            default=DEF_INITIAL_SINE_MODE_MODE_X)
         call blk_yaml%read("mode_y", silent=no_key, val=this%MODE_Y, &
                            default=DEF_INITIAL_SINE_MODE_MODE_Y)
      end if

      blk_yaml = ini_env%yaml%cast_dictionary("fields", no_blk)
      if (.not. no_blk) then
         if (this%is_activated) call env%log%exit_on_error( &
            "initial: only one initial-condition block allowed")
         this%is_activated = .true.
         this%ic_type = "INI_FIELDS"
         this%has_fields = .true.
         call read_fields_block(this, env, blk_yaml)
      end if

      blk_yaml = ini_env%yaml%cast_dictionary("hump", no_blk)
      if (.not. no_blk) call env%log%exit_on_error( &
         "initial/hump: pending — INI_REC/GAU/DIP are not ported to apply_ic yet")
      blk_yaml = ini_env%yaml%cast_dictionary("n_wave", no_blk)
      if (.not. no_blk) call env%log%exit_on_error( &
         "initial/n_wave: pending — N_WAVE is not ported to apply_ic yet")

   end subroutine initial_read_input

   ! ----------------------------------------------------------------
   ! initial: fields: — eta required, u/v both-or-neither (absent =
   ! still), format override optional (else per-extension).  Refs go
   ! through the file_spec grammar.
   ! ----------------------------------------------------------------
   subroutine read_fields_block(this, env, blk_yaml)
      use core_yaml_file_mod, only: type_yaml_reader
      class(type_model_initial), intent(inout) :: this
      type(type_env), intent(inout) :: env
      type(type_yaml_reader), intent(inout) :: blk_yaml

      character(:), allocatable :: ref, ref_u, ref_v, fmt
      logical :: no_key, no_fmt, no_u, no_v

      call blk_yaml%read_string("format", silent=no_fmt, val=fmt)

      call blk_yaml%read_string("eta", silent=no_key, val=ref)
      if (no_key) call env%log%exit_on_error("initial/fields: needs eta")
      call parse_ref(env, "initial/fields/eta", ref, this%eta_spec)

      call blk_yaml%read_string("u", silent=no_u, val=ref_u)
      call blk_yaml%read_string("v", silent=no_v, val=ref_v)
      if (no_u .neqv. no_v) call env%log%exit_on_error( &
         "initial/fields: u and v come together (both or neither)")
      this%fields_no_uv = no_u
      if (.not. no_u) then
         call parse_ref(env, "initial/fields/u", ref_u, this%u_spec)
         call parse_ref(env, "initial/fields/v", ref_v, this%v_spec)
      end if

   contains

      subroutine parse_ref(env_, key, ref_, spec)
         type(type_env), intent(inout) :: env_
         character(*), intent(in) :: key, ref_
         type(type_file_spec), intent(out) :: spec
         if (no_fmt) then
            call parse_file_spec(env_, key, ref_, spec)
         else
            call parse_file_spec(env_, key, ref_, spec, format_override=fmt)
         end if
      end subroutine parse_ref

   end subroutine read_fields_block

   ! ----------------------------------------------------------------
   ! Initial conditions: fill eta/u/v at t=0.
   ! Currently INI_SOLITARY (legacy INITIAL_SOLITARY_WAVE, old/samples.F)
   ! and INI_SINE; INI_REC/INI_GAU/INI_DIP/N_WAVE to follow.
   ! No-op (still water) when no initial: block is present.
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
   subroutine initial_apply_ic(this, grid, eta, u, v)
      use core_grid_mod, only: type_grid_2d
      class(type_model_initial), intent(in)  :: this
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
      if (this%ic_type == "INI_SINE") then
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

      if (this%ic_type /= "INI_SOLITARY") return

      call solitary_coefficients(this%AMP_SOLI, this%DEP_SOLI, c_ph, b, a1, a2, au)

      ! Oblique tiled train (angle: present): the profile is evaluated at the
      ! wrapped phase $\xi \bmod P$ along $\hat{k} = (\cos\theta, \sin\theta)$,
      ! so the doubly-periodic box holds an exact train of parallel crests
      ! with spacing $P$.  Well-posed only when the box tiles the crest line:
      !   $$ L_x \cos\theta = p\,P, \qquad L_y \sin\theta = q\,P $$
      ! (integers p, q >= 1) — e.g. a square box at 45 deg (p = q = 1).
      ! Physical cell-centre coordinates here (the legacy index form is a
      ! straight-crest quirk kept on the angle-absent path only).
      if (this%solitary_angle > 0.0_SP) then
         block
            real(SP) :: cth, sth, per, qreal, xi, xg, yg
            cth = cos(this%solitary_angle*DEG2RAD)
            sth = sin(this%solitary_angle*DEG2RAD)
            per = real(grid%M, SP)*grid%dx0*cth
            qreal = real(grid%N, SP)*grid%dy0*sth/per
            if (abs(qreal - real(nint(qreal), SP)) > 1.0e-4_SP .or. &
                nint(qreal) < 1) then
               error stop "initial/solitary/angle: box does not tile the"// &
                  " oblique crest — need Lx*cos(angle) = p*P and"// &
                  " Ly*sin(angle) = q*P (e.g. a square box at 45 deg)"
            end if
            do j = 1, grid%lp%nloc
               do i = 1, grid%lp%mloc
                  xg = (real(grid%ibegin + i - grid%lp%ib, SP) - 0.5_SP)*grid%dx0
                  yg = (real(grid%jbegin + j - grid%lp%jb, SP) - 0.5_SP)*grid%dy0
                  xi = (xg - this%XWAVEMAKER)*cth + (yg - this%solitary_yc)*sth
                  xi = modulo(xi + 0.5_SP*per, per) - 0.5_SP*per
                  sc = 1.0_SP/cosh(b*xi)
                  eta(i, j) = a1*sc*sc + a2*sc*sc*sc*sc
                  u(i, j) = au*sc*sc*cth
                  v(i, j) = au*sc*sc*sth
               end do
            end do
         end block
         return
      end if

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

   end subroutine initial_apply_ic

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
         error stop "initial: no solitary wave solution (check eps = amplitude/depth)"
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

end module model_initial_mod
