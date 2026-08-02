!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  boundaries: section reader — face-first, presence-derived BCs
!  (config reorg rung 2; owns what the sponge:/tide: sections and the
!  physics periodic stop-gap used to read).
!
!  YAML block: boundaries:       (top-level; omit for all-wall)
!    periodic: [y]               axis-level list, x and/or y
!    west: / east: / south: / north:
!    sponge:                     OPTIONAL shared coefficient defaults every
!      direct: {r, a}            face sponge inherits PER-KEY (a face
!      friction: {cd}            sub-block overrides only the keys it
!      diffusion: {nu}           states); a shared mechanism block turns
!                                that mechanism on for every face sponge.
!                                A face mechanism also takes a BOOLEAN:
!                                on = enable with inherited/default
!                                coefficients, off = opt out of the
!                                shared template (mapping-only here)
!    <face>:
!      sponge:
!        width: <m>              strip width (required, > 0)
!        direct:    {r: 0.85, a: 5.0}    Larsen-Dancy damping (nee R/A_sponge)
!        friction:  {cd: 0.0}            momentum drag strip (nee CDsponge)
!        diffusion: {nu: 0.1}            lateral viscosity strip (nee Csp)
!      forcing:                  relaxation target (nee tide: CONSTANT/DATA)
!        eta: <m>  u: <m/s>  v: <m/s>    constant targets, or
!        file: <path>                    time-series targets, or
!        wavemaker: <name>               spectrum-only wavemaker entry
!        depth: <m>                      series reference depth (with
!                                        wavemaker; nee DepthWaveMaker)
!      type: <string>            OPTIONAL assertion: wall | sponge |
!                                relaxation | characteristic — errors at
!                                init if it disagrees with the derivation
!
!  Face BC derivation (design-config-reorg; presence = on):
!    nothing                  -> wall
!    sponge only              -> sponge     (wall + absorbing strip)
!    sponge.direct + forcing  -> relaxation (nee TIDAL_BC_ABS)
!    forcing, no direct       -> characteristic — PENDING (char BC track)
!    forcing: {wavemaker: ..} -> relaxation to the wavemaker signal (nee
!                                ABS; west only) — the face sponge block
!                                routes to the wavemaker's strip (nee
!                                WidthWaveMaker/R_,A_sponge_wavemaker),
!                                NOT the sponge model; + eta/file target
!                                = generating-absorbing (nee GEN_ABS) —
!                                the face sponge {width, direct} then
!                                sizes the tide relaxation profile
!                                (rung 11; nee WaveMakerPointNum cells +
!                                hardcoded r 0.85 / a 10)
!
!  The tide/gen-abs relaxation strip takes its geometry AND coefficients
!  from the face sponge block: width (m) -> cells at init, direct.{r, a}
!  per face with the shared-template inheritance.  relaxation_cells is
!  retired.
!
!  NOTE 1: a sub-block is "present" only as a YAML mapping (direct: {} is
!    on with defaults; a bare `direct:` null reads as absent).
!  NOTE 2: forcing targets must be constants on every forced face or files
!    on every forced face — the engine's DATA/CONSTANT mode is global, a
!    mix is rejected "pending".
!  NOTE 3: corner overlap between two face strips keeps legacy
!    last-write/max-combine; a warning flags it, blending is deferred
!    (validation track).
!
!  HISTORY :
!    07/16/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_boundaries_mod
   use core_constants_mod, only: SP, type_string
   use core_env_mod, only: type_env, get_sub_env
   use core_yaml_file_mod, only: type_yaml_reader
   use core_path_mod, only: type_path
   use model_sponge_mod, only: type_model_sponge, FACE_W, FACE_E, FACE_S, FACE_N
   use model_tide_mod, only: type_model_tide
   use model_physics_mod, only: type_model_physics
   use model_wavemaker_mod, only: type_model_wavemaker
   use model_config_defaults_mod, only: DEF_BOUNDARIES_WEST_SPONGE_WIDTH, &
                                        DEF_BOUNDARIES_WEST_SPONGE_DIRECT_R, &
                                        DEF_BOUNDARIES_WEST_SPONGE_DIRECT_A, &
                                        DEF_BOUNDARIES_WEST_SPONGE_FRICTION_CD, &
                                        DEF_BOUNDARIES_WEST_SPONGE_DIFFUSION_NU

   implicit none

   private
   public :: boundaries_read_input

   ! the per-face DEF_BOUNDARIES_<FACE>_* generated constants are identical
   ! across faces; west's stand in for all four
   character(*), parameter :: D_WIDTH = DEF_BOUNDARIES_WEST_SPONGE_WIDTH
   character(*), parameter :: D_R = DEF_BOUNDARIES_WEST_SPONGE_DIRECT_R
   character(*), parameter :: D_A = DEF_BOUNDARIES_WEST_SPONGE_DIRECT_A
   character(*), parameter :: D_CD = DEF_BOUNDARIES_WEST_SPONGE_FRICTION_CD
   character(*), parameter :: D_NU = DEF_BOUNDARIES_WEST_SPONGE_DIFFUSION_NU

   character(5), parameter :: FACE_KEY(4) = ['west ', 'east ', 'south', 'north']

   ! derived BC per face (indexes DERIVED_NAME)
   integer, parameter :: BC_WALL = 1, BC_SPONGE = 2, BC_RELAX = 3
   character(14), parameter :: DERIVED_NAME(3) = &
                               ['wall          ', 'sponge        ', 'relaxation    ']

   ! shared sponge coefficients (boundaries.sponge): seeds mirror the
   ! per-face registry defaults; a present shared sub-block flips the
   ! mechanism on for every face sponge and replaces the seed values
   character(len=9), parameter :: MECH_KEY(3) = &
                                  [character(len=9) :: "direct", "friction", "diffusion"]

   type :: type_sponge_defaults
      logical  :: direct = .false., friction = .false., diffusion = .false.
      real(SP) :: r = 0.85_SP, a = 5.0_SP, cd = 0.0_SP, nu = 0.1_SP
   end type type_sponge_defaults

contains

   subroutine boundaries_read_input(env, sponge, tide, physics, wavemakers)
      type(type_env), intent(inout), target :: env
      type(type_model_sponge), intent(inout) :: sponge
      type(type_model_tide), intent(inout) :: tide
      type(type_model_physics), intent(inout) :: physics
      type(type_model_wavemaker), intent(inout) :: wavemakers(:)

      type(type_env) :: bnd_env
      type(type_yaml_reader) :: face_yaml
      type(type_sponge_defaults) :: defs
      type(type_string), allocatable :: axes(:)
      character(:), allocatable :: assert_val
      logical :: no_bnd, no_face, no_key
      logical :: forced(4), wm_forced(4), has_file(4), has_const(4)
      integer :: derived(4), wm_idx(4)
      integer :: f, i, itmp

      derived = BC_WALL
      forced = .false.
      wm_forced = .false.
      has_file = .false.
      has_const = .false.
      wm_idx = 0

      bnd_env = get_sub_env(env, "boundaries", is_empty=no_bnd)
      if (no_bnd) return

      ! ── periodic — axis-level list (a face is never "periodic"; the
      !    axis identifies the pair) ─────────────────────────────────────
      call bnd_env%yaml%read_string_array("periodic", silent=no_key, val=axes)
      if (.not. no_key) then
         do i = 1, size(axes)
            select case (trim(axes(i)%s))
            case ("y")
               physics%periodic = .true.
            case ("x")
               physics%periodic_x = .true.
            case default
               call env%log%exit_on_error( &
                  "boundaries/periodic: expected axis labels x and/or y")
            end select
         end do
      end if

      ! relaxation_cells: retired (rung 11) — the face sponge width sizes
      ! the strip in metres, resolution-invariantly
      call bnd_env%yaml%read("relaxation_cells", silent=no_key, val=itmp)
      if (.not. no_key) call env%log%exit_on_error( &
         "boundaries: relaxation_cells retired -- the face sponge width (m)"// &
         " sizes the relaxation strip (nee 30 cells = width: 30*dx)")

      ! ── shared sponge coefficients (optional; faces inherit per-key) ──
      call read_shared_sponge(env, bnd_env%yaml, defs)

      ! ── per-face blocks ────────────────────────────────────────────────
      do f = FACE_W, FACE_N
         face_yaml = bnd_env%yaml%cast_dictionary(trim(FACE_KEY(f)), no_face)
         if (no_face) cycle

         if ((physics%periodic .and. (f == FACE_S .or. f == FACE_N)) .or. &
             (physics%periodic_x .and. (f == FACE_W .or. f == FACE_E))) &
            call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                       ": face block on a periodic axis")

         call read_face_forcing(env, face_yaml, tide, wavemakers, f, &
                                forced(f), wm_forced(f), wm_idx(f), &
                                has_file(f), has_const(f))

         if (wm_forced(f)) then
            ! wavemaker-fed face: the sponge block IS the relaxation strip
            ! (nee WidthWaveMaker/R_,A_sponge_wavemaker; gen-abs routes it
            ! to the tide profile instead) — NOT the sponge model
            call read_wavemaker_strip(env, face_yaml, wavemakers(wm_idx(f)), f, &
                                      tide, defs)
            derived(f) = BC_RELAX
         else
            call read_face_sponge(env, face_yaml, sponge, f, defs)

            ! derivation table (design-config-reorg)
            if (forced(f)) then
               if (.not. sponge%direct_on(f)) &
                  call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                             ": forcing without sponge.direct derives a characteristic"// &
                                             " face — pending (add sponge: {width, direct} for relaxation)")
               derived(f) = BC_RELAX
               ! the tide strip inherits the face sponge geometry +
               ! direct coefficients (rung 11)
               tide%width_m(f) = sponge%width(f)
               tide%r_face(f) = sponge%r_direct(f)
               tide%a_face(f) = sponge%a_direct(f)
            else if (sponge%width(f) > 0.0_SP) then
               derived(f) = BC_SPONGE
            end if
         end if

         ! optional type: assertion — errors when it disagrees, otherwise inert
         call face_yaml%read_enum("type", &
                                  [character(14) :: "wall", "sponge", "relaxation", &
                                   "characteristic"], silent=no_key, val=assert_val)
         if (.not. no_key) then
            if (assert_val /= trim(DERIVED_NAME(derived(f)))) &
               call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                          ": type asserts '"//assert_val//"' but blocks derive '"// &
                                          trim(DERIVED_NAME(derived(f)))//"'")
         end if

         if (wm_forced(f)) then
            call env%log%info("boundaries: "//trim(FACE_KEY(f))//" = relaxation"// &
                              " (wavemaker '"//wavemakers(wm_idx(f))%name//"')")
         else
            call env%log%info("boundaries: "//trim(FACE_KEY(f))//" = "// &
                              trim(DERIVED_NAME(derived(f))))
         end if
      end do

      ! ── fold into the engine models ────────────────────────────────────
      sponge%is_activated = any(sponge%direct_on) .or. any(sponge%friction_on) &
                            .or. any(sponge%diffusion_on)

      ! a generating-absorbing west face (nee GEN_ABS) streams/holds its
      ! target through the same west tide slot, without the TIDE_BC strip
      tide%tide_west = forced(FACE_W) .or. tide%tidal_bc_gen_abs
      tide%tide_east = forced(FACE_E)
      tide%tide_south = forced(FACE_S)
      tide%tide_north = forced(FACE_N)
      tide%tidal_bc_abs = any(forced)
      tide%is_activated = tide%tidal_bc_abs .or. tide%tidal_bc_gen_abs
      if (any(has_file) .and. any(has_const)) &
         call env%log%exit_on_error("boundaries: forcing targets must be all"// &
                                    " constants or all files — mixing is pending")
      if (any(has_file)) then
         tide%tide_bc_type = 'DATA'
      else if (any(has_const)) then
         tide%tide_bc_type = 'CONSTANT'
      end if

      ! NOTE 3: corner strips keep legacy last-write/max-combine
      if ((sponge%width(FACE_W) > 0.0_SP .or. sponge%width(FACE_E) > 0.0_SP) .and. &
          (sponge%width(FACE_S) > 0.0_SP .or. sponge%width(FACE_N) > 0.0_SP)) &
         call env%log%warning("boundaries: overlapping corner sponge strips"// &
                              " keep legacy last-write combine; blending is a future policy")

      ! strip-width advisory (--validate only): every absorbing/relaxation
      ! strip should span the longest energetic wavelength any wavemaker
      ! puts in the domain, or it re-reflects the low band (rung 11)
      if (env%yaml%unread_strict) &
         call check_strip_widths(env, sponge, tide, wavemakers)

   end subroutine boundaries_read_input

   ! ── λ_low width advisory: domain-level longest energetic wavelength
   !    (max over wavemakers of the 2%-quantile low-side wavelength at the
   !    generation depth) vs every configured strip width.  Warn, never
   !    error: sizing is the authoring side's job — this only keeps the
   !    legacy silent-under-absorption mode from coming back ──────────────
   subroutine check_strip_widths(env, sponge, tide, wavemakers)
      use model_wavemaker_mod, only: wavemaker_lambda_low
      type(type_env), intent(inout) :: env
      type(type_model_sponge), intent(in) :: sponge
      type(type_model_tide), intent(in) :: tide
      type(type_model_wavemaker), intent(in) :: wavemakers(:)

      character(160) :: msg
      real(SP) :: lam, w
      integer :: i, f

      lam = 0.0_SP
      do i = 1, size(wavemakers)
         lam = max(lam, wavemaker_lambda_low(wavemakers(i)))
      end do
      if (lam <= 0.0_SP) return

      do f = FACE_W, FACE_N
         w = max(sponge%width(f), tide%width_m(f))
         if (w > 0.0_SP .and. w < lam) then
            write (msg, '(3a,f0.1,a,f0.1,a)') "boundaries/", trim(FACE_KEY(f)), &
               "/sponge: width ", w, " m < the 2%-quantile low-side wavelength ", &
               lam, " m -- the strip under-absorbs the longest waves"
            call env%log%warning(trim(msg))
         end if
      end do

      do i = 1, size(wavemakers)
         w = wavemakers(i)%WidthWaveMaker
         if (w > 0.0_SP .and. w < lam) then
            write (msg, '(3a,f0.1,a,f0.1,a)') "wavemaker '", wavemakers(i)%name, &
               "': strip width ", w, " m < the 2%-quantile low-side wavelength ", &
               lam, " m -- the strip under-absorbs the longest waves"
            call env%log%warning(trim(msg))
         end if
      end do

   end subroutine check_strip_widths

   ! ── face sub-block readers ──────────────────────────────────────────────

   subroutine read_face_sponge(env, face_yaml, sponge, f, defs)
      type(type_env), intent(inout) :: env
      type(type_yaml_reader), intent(inout) :: face_yaml
      type(type_model_sponge), intent(inout) :: sponge
      integer, intent(in) :: f
      type(type_sponge_defaults), intent(in) :: defs

      type(type_yaml_reader) :: sp_yaml, sub_yaml
      real(SP) :: tmp
      logical :: no_sp, no_key, on, has_blk

      sp_yaml = face_yaml%cast_dictionary("sponge", no_sp)
      if (no_sp) return

      call sp_yaml%read("width", silent=no_key, val=sponge%width(f), default=D_WIDTH)
      if (sponge%width(f) <= 0.0_SP) &
         call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                    "/sponge: needs width > 0")

      ! each mechanism: a face mapping = on + per-key coefficient override;
      ! a face boolean = on (inherited/default coefficients) or off (opt
      ! out of the shared template); absent = the shared state.  Seeds come
      ! from the shared defaults (tmp guards the intent(out) wipe)
      call mech_gate(sp_yaml, "direct", defs%direct, on, sub_yaml, has_blk)
      if (on) then
         sponge%direct_on(f) = .true.
         sponge%r_direct(f) = defs%r
         sponge%a_direct(f) = defs%a
         if (has_blk) then
            call sub_yaml%read("r", silent=no_key, val=tmp)
            if (.not. no_key) sponge%r_direct(f) = tmp
            call sub_yaml%read("a", silent=no_key, val=tmp)
            if (.not. no_key) sponge%a_direct(f) = tmp
         end if
      end if

      call mech_gate(sp_yaml, "friction", defs%friction, on, sub_yaml, has_blk)
      if (on) then
         sponge%friction_on(f) = .true.
         sponge%cd_fric(f) = defs%cd
         if (has_blk) then
            call sub_yaml%read("cd", silent=no_key, val=tmp)
            if (.not. no_key) sponge%cd_fric(f) = tmp
         end if
      end if

      call mech_gate(sp_yaml, "diffusion", defs%diffusion, on, sub_yaml, has_blk)
      if (on) then
         sponge%diffusion_on(f) = .true.
         sponge%nu_diff(f) = defs%nu
         if (has_blk) then
            call sub_yaml%read("nu", silent=no_key, val=tmp)
            if (.not. no_key) sponge%nu_diff(f) = tmp
         end if
      end if

      if (.not. (sponge%direct_on(f) .or. sponge%friction_on(f) &
                 .or. sponge%diffusion_on(f))) &
         call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                    "/sponge: needs at least one of direct/friction/diffusion")

   end subroutine read_face_sponge

   ! ----------------------------------------------------------------
   ! Resolve one face mechanism key: mapping = on (+ has_blk for the
   ! coefficient overrides), boolean = forced on/off, absent = the
   ! shared template state.  cast_dictionary flags a non-mapping value
   ! silently, so the boolean read only fires when the cast missed.
   ! ----------------------------------------------------------------
   subroutine mech_gate(sp_yaml, key, shared_on, on, sub_yaml, has_blk)
      type(type_yaml_reader), intent(inout) :: sp_yaml
      character(*), intent(in) :: key
      logical, intent(in) :: shared_on
      logical, intent(out) :: on, has_blk
      type(type_yaml_reader), intent(out) :: sub_yaml

      logical :: no_blk, no_key, flag

      sub_yaml = sp_yaml%cast_dictionary(key, no_blk)
      has_blk = .not. no_blk
      if (has_blk) then
         on = .true.
         return
      end if
      call sp_yaml%read(key, silent=no_key, val=flag)
      if (no_key) then
         on = shared_on
      else
         on = flag
      end if

   end subroutine mech_gate

   ! ----------------------------------------------------------------
   ! boundaries.sponge — the OPTIONAL shared coefficient block every
   ! face sponge inherits per-key.  Presence of a mechanism sub-block
   ! here turns that mechanism on for every face sponge; a face opts
   ! out with <mechanism>: off.  Mapping-only at this level.
   ! ----------------------------------------------------------------
   subroutine read_shared_sponge(env, yaml, defs)
      type(type_env), intent(inout) :: env
      type(type_yaml_reader), intent(inout) :: yaml
      type(type_sponge_defaults), intent(out) :: defs

      type(type_yaml_reader) :: sp_yaml, sub_yaml
      real(SP) :: tmp
      logical :: no_sp, no_blk, no_key
      integer :: i

      sp_yaml = yaml%cast_dictionary("sponge", no_sp)
      if (no_sp) return

      ! booleans are a FACE grammar; here absent already means off
      do i = 1, size(MECH_KEY)
         sub_yaml = sp_yaml%cast_dictionary(trim(MECH_KEY(i)), no_blk)
         if (no_blk .and. sp_yaml%has_key(trim(MECH_KEY(i)))) then
            call env%log%exit_on_error("boundaries/sponge/"// &
                                       trim(MECH_KEY(i))// &
                                       ": takes a mapping (absent = off)")
         end if
      end do

      sub_yaml = sp_yaml%cast_dictionary("direct", no_blk)
      if (.not. no_blk) then
         defs%direct = .true.
         call sub_yaml%read("r", silent=no_key, val=tmp)
         if (.not. no_key) defs%r = tmp
         call sub_yaml%read("a", silent=no_key, val=tmp)
         if (.not. no_key) defs%a = tmp
      end if

      sub_yaml = sp_yaml%cast_dictionary("friction", no_blk)
      if (.not. no_blk) then
         defs%friction = .true.
         call sub_yaml%read("cd", silent=no_key, val=tmp)
         if (.not. no_key) defs%cd = tmp
      end if

      sub_yaml = sp_yaml%cast_dictionary("diffusion", no_blk)
      if (.not. no_blk) then
         defs%diffusion = .true.
         call sub_yaml%read("nu", silent=no_key, val=tmp)
         if (.not. no_key) defs%nu = tmp
      end if

      if (.not. (defs%direct .or. defs%friction .or. defs%diffusion)) &
         call env%log%exit_on_error( &
         "boundaries/sponge: needs at least one of direct/friction/diffusion")

   end subroutine read_shared_sponge

   subroutine read_face_forcing(env, face_yaml, tide, wavemakers, f, &
                                forced, wm_forced, wm_idx, has_file, has_const)
      type(type_env), intent(inout) :: env
      type(type_yaml_reader), intent(inout) :: face_yaml
      type(type_model_tide), intent(inout) :: tide
      type(type_model_wavemaker), intent(inout) :: wavemakers(:)
      integer, intent(in) :: f
      logical, intent(out) :: forced, wm_forced, has_file, has_const
      integer, intent(out) :: wm_idx

      type(type_yaml_reader) :: frc_yaml
      type(type_path) :: file
      real(SP) :: eta, u, v
      logical :: no_frc, no_eta, no_u, no_v, no_file, no_key
      character(:), allocatable :: wm_name
      integer :: k

      forced = .false.
      wm_forced = .false.
      has_file = .false.
      has_const = .false.
      wm_idx = 0

      frc_yaml = face_yaml%cast_dictionary("forcing", no_frc)
      if (no_frc) return

      ! named-wavemaker reference: the face consumes the spectrum-only
      ! wavemaker entry as its relaxation signal (nee ABS / GEN_ABS)
      call frc_yaml%read_string("wavemaker", silent=no_key, val=wm_name)
      if (.not. no_key) then
         wm_forced = .true.
         do k = 1, size(wavemakers)
            if (wavemakers(k)%boundary_candidate .and. &
                len(wavemakers(k)%name) > 0) then
               if (wavemakers(k)%name == wm_name) wm_idx = k
            end if
         end do
         if (wm_idx == 0) &
            call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                       "/forcing: wavemaker '"//wm_name//"' does not name a"// &
                                       " spectrum-only wavemaker entry (check name:)")
         call bind_face_wavemaker(env, frc_yaml, tide, wavemakers(wm_idx), f, &
                                  wm_name, has_file, has_const)
         return
      end if
      forced = .true.

      ! yaml read val is intent(out) — a silent-miss WIPES the passed
      ! component, so read into temps and assign only when present
      call frc_yaml%read_input_path("file", silent=no_file, val=file)
      call frc_yaml%read("eta", silent=no_eta, val=eta)
      call frc_yaml%read("u", silent=no_u, val=u)
      call frc_yaml%read("v", silent=no_v, val=v)

      if (no_file .eqv. no_eta) &
         call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                    "/forcing: needs exactly one of eta (constant) or file (series)")
      if (.not. no_file .and. .not. (no_u .and. no_v)) &
         call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                    "/forcing: u/v constants conflict with file")

      has_file = .not. no_file
      has_const = .not. no_eta

      select case (f)
      case (FACE_W)
         if (has_file) tide%file_west = file
         if (has_const) then
            tide%eta_west = eta
            if (.not. no_u) tide%u_west = u
            if (.not. no_v) tide%v_west = v
         end if
      case (FACE_E)
         if (has_file) tide%file_east = file
         if (has_const) then
            tide%eta_east = eta
            if (.not. no_u) tide%u_east = u
            if (.not. no_v) tide%v_east = v
         end if
      case (FACE_S)
         if (has_file) tide%file_south = file
         if (has_const) then
            tide%eta_south = eta
            if (.not. no_u) tide%u_south = u
            if (.not. no_v) tide%v_south = v
         end if
      case (FACE_N)
         if (has_file) tide%file_north = file
         if (has_const) then
            tide%eta_north = eta
            if (.not. no_u) tide%u_north = u
            if (.not. no_v) tide%v_north = v
         end if
      end select

   end subroutine read_face_forcing

   ! ── wavemaker-fed face (nee ABS): resolve the named spectrum-only
   !    entry, read the series reference depth, and detect the optional
   !    tide target (nee GEN_ABS) ────────────────────────────────────────
   subroutine bind_face_wavemaker(env, frc_yaml, tide, wavemaker, f, wm_name, &
                                  has_file, has_const)
      type(type_env), intent(inout) :: env
      type(type_yaml_reader), intent(inout) :: frc_yaml
      type(type_model_tide), intent(inout) :: tide
      type(type_model_wavemaker), intent(inout) :: wavemaker
      integer, intent(in) :: f
      character(*), intent(in) :: wm_name
      logical, intent(out) :: has_file, has_const

      type(type_path) :: file
      real(SP) :: eta, u, v
      logical :: no_eta, no_u, no_v, no_file

      has_file = .false.
      has_const = .false.

      if (f /= FACE_W) &
         call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                    "/forcing: wavemaker-fed faces other than west are"// &
                                    " pending (legacy ABS relaxes a west strip)")
      if (.not. wavemaker%boundary_candidate) &
         call env%log%exit_on_error("boundaries/west/forcing: wavemaker '"//wm_name// &
                                    "' does not name a spectrum-only wavemaker entry")
      if (len(wavemaker%name) == 0) &
         call env%log%exit_on_error("boundaries/west/forcing: the wavemaker entry"// &
                                    " needs a name: key to be referenced")
      if (wavemaker%name /= wm_name) &
         call env%log%exit_on_error("boundaries/west/forcing: wavemaker '"//wm_name// &
                                    "' does not match the entry name '"//wavemaker%name//"'")

      ! series reference depth (nee DepthWaveMaker; no DEP_WK fallback —
      ! a boundary entry has no source box)
      call frc_yaml%read("depth", val=wavemaker%DepthWaveMaker)
      wavemaker%wavemaker_type = "ABS"

      ! optional tide target on the same face = generating-absorbing (nee
      ! GEN_ABS): eta constant XOR file series; u/v targets are unused by
      ! the legacy form
      call frc_yaml%read_input_path("file", silent=no_file, val=file)
      call frc_yaml%read("eta", silent=no_eta, val=eta)
      call frc_yaml%read("u", silent=no_u, val=u)
      call frc_yaml%read("v", silent=no_v, val=v)
      if (.not. (no_u .and. no_v)) &
         call env%log%exit_on_error("boundaries/west/forcing: u/v targets are"// &
                                    " unused by the wavemaker generating-absorbing form")
      if (.not. no_eta .and. .not. no_file) &
         call env%log%exit_on_error("boundaries/west/forcing: needs at most one of"// &
                                    " eta (constant) or file (series) with wavemaker")
      if (.not. no_eta) then
         tide%eta_west = eta
         has_const = .true.
      end if
      if (.not. no_file) then
         tide%file_west = file
         has_file = .true.
      end if
      tide%tidal_bc_gen_abs = .not. (no_eta .and. no_file)

   end subroutine bind_face_wavemaker

   ! ── relaxation strip of a wavemaker-fed face (nee CALCULATE_SPONGE_MAKER
   !    inputs WidthWaveMaker/R_,A_sponge_wavemaker) — abs form: all keys
   !    required, legacy leaves them UNDEFINED when omitted so there is no
   !    default to honour.  The generating-absorbing form routes the same
   !    block to the tide profile (rung 11; nee WaveMakerPointNum cells +
   !    hardcoded r 0.85 / a 10) with per-key shared-template inheritance —
   !    legacy had no keys there at all ────────────────────────────────────
   subroutine read_wavemaker_strip(env, face_yaml, wavemaker, f, tide, defs)
      type(type_env), intent(inout) :: env
      type(type_yaml_reader), intent(inout) :: face_yaml
      type(type_model_wavemaker), intent(inout) :: wavemaker
      integer, intent(in) :: f
      type(type_model_tide), intent(inout) :: tide
      type(type_sponge_defaults), intent(in) :: defs

      type(type_yaml_reader) :: sp_yaml, sub_yaml
      real(SP) :: tmp
      logical :: no_sp, no_blk, no_key

      no_blk = .true.
      sp_yaml = face_yaml%cast_dictionary("sponge", no_sp)
      if (.not. no_sp) sub_yaml = sp_yaml%cast_dictionary("direct", no_blk)
      if (no_sp .or. no_blk) &
         call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                    ": wavemaker forcing without sponge.direct derives a"// &
                                    " characteristic face — pending (add sponge: {width,"// &
                                    " direct: {r, a}} for relaxation)")

      if (tide%tidal_bc_gen_abs) then
         call sp_yaml%read("width", val=tide%width_m(f))
         if (tide%width_m(f) <= 0.0_SP) &
            call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                       "/sponge: needs width > 0")
         ! per-key inheritance: face key > shared template > registry
         ! default (defs seeds carry the shared/registry resolution)
         tide%r_face(f) = defs%r
         tide%a_face(f) = defs%a
         call sub_yaml%read("r", silent=no_key, val=tmp)
         if (.not. no_key) tide%r_face(f) = tmp
         call sub_yaml%read("a", silent=no_key, val=tmp)
         if (.not. no_key) tide%a_face(f) = tmp
      else
         call sp_yaml%read("width", val=wavemaker%WidthWaveMaker)
         if (wavemaker%WidthWaveMaker <= 0.0_SP) &
            call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                       "/sponge: needs width > 0")
         call sub_yaml%read("r", val=wavemaker%R_sponge_wavemaker)
         call sub_yaml%read("a", val=wavemaker%A_sponge_wavemaker)
      end if

      sub_yaml = sp_yaml%cast_dictionary("friction", no_blk)
      if (no_blk) sub_yaml = sp_yaml%cast_dictionary("diffusion", no_blk)
      if (.not. no_blk) &
         call env%log%exit_on_error("boundaries/"//trim(FACE_KEY(f))// &
                                    "/sponge: friction/diffusion strips on a wavemaker-fed"// &
                                    " face are not supported")

   end subroutine read_wavemaker_strip

end module model_boundaries_mod
