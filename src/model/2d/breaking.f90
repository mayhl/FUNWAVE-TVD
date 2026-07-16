!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Wave breaking parameters YAML reader
!
!  YAML block: breaking:       (top-level; omit to use defaults)
!    roller_effect:    <bool>   enable roller effect,             default NO
!    show_breaking:    <bool>   enable breaking detection,        default YES
!    Cbrk1:            <real>   onset breaking threshold,         default 0.65
!    Cbrk2:            <real>   cessation breaking threshold,     default 0.35
!    WAVEMAKER_Cbrk:   <real>   breaking threshold near wavemaker, default 1.0
!    WAVEMAKER_VIS:    <bool>   wavemaker viscosity (deprecated), default NO
!    visbrk:           <real>   breaking viscosity,               default 0.0
!    WAVEMAKER_visbrk: <real>   wavemaker breaking viscosity,     default 0.0
!
!  Note: WAVEMAKER_VIS and viscosity_breaking (physics:) are mutually exclusive.
!        The conflict check is enforced in io.F.
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_breaking_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_BREAKING_CBRK1, DEF_BREAKING_CBRK2, &
                                        DEF_BREAKING_MODEL, &
                                        DEF_BREAKING_NU_BKG, DEF_BREAKING_ROLLER_EFFECT, &
                                        DEF_BREAKING_SHOW_BREAKING, DEF_BREAKING_VISBRK, &
                                        DEF_BREAKING_WAVEMAKER_CBRK, &
                                        DEF_BREAKING_WAVEMAKER_VIS, &
                                        DEF_BREAKING_WAVEMAKER_VISBRK

   implicit none

   private
   public :: type_model_breaking

   character(len=16), parameter :: BREAKING_MODELS(2) = &
                                   [character(len=16) :: "eddy_viscosity", "shock_capturing"]

   type, extends(type_model_base) :: type_model_breaking

      ! Breaker mechanism (nee physics.viscosity_breaking): eddy_viscosity
      ! runs the Kennedy-style breaker; shock_capturing leaves dissipation
      ! to the TVD scheme + SWE transition.  Default applies with NO
      ! breaking: section — breaking is core physics, not presence-gated.
      character(:), allocatable :: model

      logical  :: roller = .false.
      logical  :: show_breaking = .true.

      real(SP) :: Cbrk1 = 0.65_SP
      real(SP) :: Cbrk2 = 0.35_SP
      real(SP) :: WAVEMAKER_Cbrk = 1.0_SP

      logical  :: WAVEMAKER_VIS = .false.
      real(SP) :: visbrk = 0.0_SP
      real(SP) :: WAVEMAKER_visbrk = 0.0_SP

      real(SP) :: nu_bkg = 0.0_SP   ! background kinematic viscosity floor for nu_break

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

      sub_env = get_sub_env(env, "breaking", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (.not. this%is_activated) return

      call sub_env%yaml%read_enum("model", BREAKING_MODELS, val=this%model, &
                                  default=DEF_BREAKING_MODEL)
      call sub_env%yaml%read("roller_effect", val=this%roller, default=DEF_BREAKING_ROLLER_EFFECT)
      call sub_env%yaml%read("show_breaking", val=this%show_breaking, default=DEF_BREAKING_SHOW_BREAKING)

      call sub_env%yaml%read("Cbrk1", silent=no_key, val=this%Cbrk1, default=DEF_BREAKING_CBRK1)
      call sub_env%yaml%read("Cbrk2", silent=no_key, val=this%Cbrk2, default=DEF_BREAKING_CBRK2)
      call sub_env%yaml%read("WAVEMAKER_Cbrk", silent=no_key, val=this%WAVEMAKER_Cbrk, default=DEF_BREAKING_WAVEMAKER_CBRK)

      call sub_env%yaml%read("WAVEMAKER_VIS", val=this%WAVEMAKER_VIS, default=DEF_BREAKING_WAVEMAKER_VIS)
      call sub_env%yaml%read("visbrk", silent=no_key, val=this%visbrk, default=DEF_BREAKING_VISBRK)
      call sub_env%yaml%read("WAVEMAKER_visbrk", silent=no_key, val=this%WAVEMAKER_visbrk, default=DEF_BREAKING_WAVEMAKER_VISBRK)
      call sub_env%yaml%read("nu_bkg", silent=no_key, val=this%nu_bkg, default=DEF_BREAKING_NU_BKG)

   end subroutine breaking_read_input

end module model_breaking_mod
