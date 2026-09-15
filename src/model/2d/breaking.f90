!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Wave breaking parameters YAML reader
!
!  YAML block: breaking:       (top-level; omit to use defaults)
!    model:         <enum>   eddy_viscosity | shock_capturing |
!                            wavemaker_viscosity (nee WAVEMAKER_VIS:
!                            shock-capturing globally + zone viscosity)
!    roller:        <bool>   enable the surface roller (nee ROLLER;
!                            forces eddy_viscosity, as legacy), default NO
!    show_breaking: <bool>   enable breaking detection,          default YES
!    cbrk1:         <real>   onset breaking threshold,           default 0.55
!    cbrk2:         <real>   cessation breaking threshold,       default 0.35
!    visbrk:        <real>   breaking viscosity,                 default 0.0
!    nu_bkg:        <real>   background viscosity floor,         default 0.0
!    swe_eta_dep:   <real>   bore-regime eta/h threshold,        default 0.8
!    swe_gate:      <bool>   SWE gate under eddy_viscosity,      default NO
!    swe_eta_ramp:  <real>   SWE-gate smoothstep taper width,    default 0.1
!    wetdry_disp_ramp: <real> wet/dry dispersion taper (x min_depth), default 3
!    solver:        <enum>   explicit | split_implicit | stage_split,
!                            default split_implicit
!
!  Variant keys are read CONDITIONALLY so the unread-key detector flags
!  inapplicable knobs: cbrk1/cbrk2 need the breaker kernel (eddy_viscosity
!  or show_breaking), swe_eta_ramp needs the SWE gate (not eddy_viscosity —
!  mask9 is forced 1 there), visbrk is the wavemaker_viscosity threshold.
!  swe_eta_dep reads always (gate threshold AND the viscous breaker's
!  extra onset criterion).
!
!  The wavemaker-zone overrides (nee WAVEMAKER_Cbrk/WAVEMAKER_visbrk) moved
!  to wavemaker.source.breaking; main bridges them into the fields here
!  until the coefficient-field assembler lands.  The enum makes the legacy
!  WAVEMAKER_VIS x VISCOSITY_BREAKING exclusion structural.
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_breaking_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_breaking

   ! none = no breaker and no SWE gate: dispersion everywhere, nothing
   ! dissipates a bore -- analytic cases and debugging, never a beach
   character(len=20), parameter :: BREAKING_MODELS(4) = &
                                   [character(len=20) :: "eddy_viscosity", "shock_capturing", &
                                                          "wavemaker_viscosity", "none"]
   character(len=12), parameter :: VISC_SCHEMES(5) = &
                                   [character(len=12) :: "constant", "kennedy", "kennedy_orig", &
                                                          "static_trans", "depth_ratio"]
   character(len=20), parameter :: VISC_SOLVERS(3) = &
                                   [character(len=20) :: "explicit", "split_implicit", &
                                                          "stage_split"]

   type, extends(type_model_base) :: type_model_breaking

      ! Breaker mechanism (nee physics.viscosity_breaking): eddy_viscosity
      ! runs the Kennedy-style breaker; shock_capturing leaves dissipation
      ! to the TVD scheme + SWE transition.  Default applies with NO
      ! breaking: section — breaking is core physics, not presence-gated.
      character(:), allocatable :: model

      logical  :: roller = .false.
      ! DERIVED, not a deck key: main sets it from viscosity_breaking +
      ! the AGE/ROLLER/UNDERTOW output requests (show-only pass is
      ! solution-neutral; off by default = skip the diagnostics cost)
      logical  :: show_breaking = .false.

      real(SP) :: cbrk1 = 0.55_SP
      real(SP) :: cbrk2 = 0.35_SP
      real(SP) :: wavemaker_cbrk = 1.0_SP

      ! breaking-event age threshold (legacy hard-coded 20 s; the
      ! per-wavemaker assignments were dead code — parity ledger 17d/e)
      real(SP) :: t_brk = 20.0_SP
      ! viscosity magnitude multiplier: every scheme scales nu with
      ! cbrk2*sqrt(gh)*depth, so cbrk2 carried cessation AND magnitude;
      ! nu_scale separates them (1 = as calibrated)
      real(SP) :: nu_scale = 1.0_SP
      ! viscosity scheme (kernel forms; constant = the legacy default)
      character(:), allocatable :: scheme
      ! legacy accrues breaker age every RK stage (3x wall-clock); the
      ! cbrk defaults were calibrated with it.  false = once per step
      logical  :: age_per_stage = .true.

      logical  :: wavemaker_vis = .false.
      real(SP) :: visbrk = 0.0_SP
      real(SP) :: wavemaker_visbrk = 0.0_SP

      real(SP) :: nu_bkg = 0.0_SP   ! background kinematic viscosity floor for nu_break

      ! SWE-transition gate (nee physics.dispersion keys): the gate IS the
      ! shock-capturing breaking mechanism; dep doubles as the viscous
      ! breaker's onset criterion.  Initializers must track the registry
      ! defaults -- block-less decks land here
      real(SP) :: swe_eta_dep = 0.8_SP
      real(SP) :: swe_eta_ramp = 0.1_SP
      ! wet/dry-proximity dispersion taper (multiples of min_depth); 0 = off.
      ! Mode-independent -- the viscous path has no SWE gate, so swash-edge
      ! mask flips otherwise radiate through the dispersive terms
      real(SP) :: wetdry_disp_ramp = 3.0_SP
      ! apply the SWE gate under eddy_viscosity as a dispersion amplitude
      ! cap at steep bores; false = legacy (dispersion never gated -- the
      ! open-water surf blow-up mechanism)
      logical  :: swe_gate = .false.
      ! clamp nu_break at this fraction of the explicit-diffusion stability
      ! bound (the Laplacian rides the advective-CFL dt); 0 = off (legacy)
      real(SP) :: nu_cap = 0.0_SP
      ! breaker-viscosity integrator: explicit source (legacy), the
      ! once-per-step operator-split ADI solve on Hu/Hv, or the same solve
      ! inside every RK stage -- both splits unconditionally stable, so
      ! nu_cap never engages there
      character(:), allocatable :: solver
      logical  :: split_implicit = .true.
      logical  :: per_stage = .false.
      ! implicitness weight of the split solve: 1 = backward Euler,
      ! 0.5 = Crank-Nicolson (A-stable for theta >= 0.5)
      real(SP) :: theta = 1.0_SP

   contains
      procedure :: read_input => breaking_read_input
   end type type_model_breaking

contains

   subroutine breaking_read_input(this, env)
      class(type_model_breaking), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk, no_key

      this%model = "eddy_viscosity"
      this%solver = "split_implicit"
      this%scheme = "constant"

      sub_env = get_sub_env(env, "breaking", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (.not. this%is_activated) return

      call sub_env%yaml%read_enum("model", BREAKING_MODELS, val=this%model, &
                                  default="eddy_viscosity")
      call sub_env%yaml%read("roller", val=this%roller, default="NO")
      ! eddy_viscosity only: the split solve is not wired for the
      ! wavemaker_viscosity zone term (it stays an explicit source), so
      ! leaving solver unread there lets the detector flag it
      if (trim(this%model) == "eddy_viscosity") then
         call sub_env%yaml%read("nu_scale", silent=no_key, val=this%nu_scale, &
                                default="1.0")
         call sub_env%yaml%read_enum("scheme", VISC_SCHEMES, silent=no_key, &
                                     val=this%scheme, default="constant")
         call sub_env%yaml%read_enum("solver", VISC_SOLVERS, silent=no_key, &
                                     val=this%solver, default="split_implicit")
         this%split_implicit = trim(this%solver) /= "explicit"
         this%per_stage = trim(this%solver) == "stage_split"
         if (this%split_implicit) then
            call sub_env%yaml%read("theta", silent=no_key, val=this%theta, &
                                   default="1.0")
            if (this%theta < 0.5_SP .or. this%theta > 1.0_SP) &
               call env%log%exit_on_error("breaking/theta: must be in [0.5, 1]")
         end if
      else
         ! the type initializers carry the eddy_viscosity defaults; the
         ! zone term feeds the explicit source, so force that path
         this%solver = "explicit"
         this%split_implicit = .false.
         this%per_stage = .false.
      end if
      if (sub_env%yaml%has_key("show_breaking")) then
         call env%log%exit_on_error("breaking/show_breaking: retired — the"// &
                                    " breaker-diagnostics pass is derived from the model and"// &
                                    " the AGE/ROLLER/UNDERTOW output requests")
      end if

      call sub_env%yaml%read("nu_bkg", silent=no_key, val=this%nu_bkg, default="0.0")
      call sub_env%yaml%read("swe_eta_dep", silent=no_key, val=this%swe_eta_dep, &
                             default="0.8")
      call sub_env%yaml%read("wetdry_disp_ramp", silent=no_key, val=this%wetdry_disp_ramp, &
                             default="3.0")

      ! variant keys — conditionally read so the unread-key detector flags
      ! knobs inapplicable to the selected model.  cbrk1/cbrk2 stay live
      ! under shock_capturing too: the display breaker may run (derived
      ! show_breaking, known only after output reads) and uses them
      if (trim(this%model) /= "wavemaker_viscosity") then
         call sub_env%yaml%read("cbrk1", silent=no_key, val=this%cbrk1, default="0.55")
         call sub_env%yaml%read("cbrk2", silent=no_key, val=this%cbrk2, default="0.35")
         call sub_env%yaml%read("t_brk", silent=no_key, val=this%t_brk, default="20.0")
         call sub_env%yaml%read("age_per_stage", silent=no_key, val=this%age_per_stage, &
                                default="YES")
         ! inapplicable under the implicit solve -- leaving it unread lets
         ! the unread-key detector flag a stale nu_cap in the deck
         if (.not. this%split_implicit) then
            call sub_env%yaml%read("nu_cap", silent=no_key, val=this%nu_cap, &
                                   default="0.0")
         end if
      end if
      ! the gate is structural under the other models — the knob only means
      ! something where legacy forces mask9 to 1
      if (trim(this%model) == "eddy_viscosity") then
         call sub_env%yaml%read("swe_gate", silent=no_key, val=this%swe_gate, &
                                default="NO")
      end if
      if (trim(this%model) == "shock_capturing" .or. trim(this%model) == "wavemaker_viscosity" &
          .or. this%swe_gate) then
         call sub_env%yaml%read("swe_eta_ramp", silent=no_key, val=this%swe_eta_ramp, &
                                default="0.1")
      end if
      if (trim(this%model) == "wavemaker_viscosity") then
         call sub_env%yaml%read("visbrk", silent=no_key, val=this%visbrk, default="0.0")
      end if

      ! the enum IS the legacy WAVEMAKER_VIS x VISCOSITY_BREAKING exclusion
      this%wavemaker_vis = trim(this%model) == "wavemaker_viscosity"

   end subroutine breaking_read_input

end module model_breaking_mod
