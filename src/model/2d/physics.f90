!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Physics parameters YAML reader
!
!  YAML block: physics:
!    water_level: <length>   optional, default 0 (still-water offset added to bathymetry)
!    periodic:   <bool>      Cartesian only; south-north periodic BC, default NO
!    dispersion: <bool>      default YES
!    Gamma1:     <real>      dispersion coefficient,           default 1.0
!    Gamma2:     <real>      nonlinearity coefficient (CART),  default 1.0
!    Beta_ref:   <real>      reference level (CART/ZALPHA),    default -0.531
!    Gamma3:     <real>      linearity switch coefficient,     default 1.0
!    viscosity_breaking: <bool>   default YES
!    SWE_ETA_DEP: <real>    SWE depth fraction,               default 0.7
!    breaking: <bool>
!    wavemaker: <bool>
!    sediment: <bool>
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_physics_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_PHYSICS_BETA_REF, DEF_PHYSICS_BREAKING, &
                                          DEF_PHYSICS_C_SMG, DEF_PHYSICS_DISPERSION, &
                                          DEF_PHYSICS_DISP_TIME_LEFT, DEF_PHYSICS_GAMMA1, &
                                          DEF_PHYSICS_GAMMA2, DEF_PHYSICS_GAMMA3, &
                                          DEF_PHYSICS_PERIODIC, DEF_PHYSICS_SEDIMENT, &
                                          DEF_PHYSICS_SWE_ETA_DEP, &
                                          DEF_PHYSICS_VISCOSITY_BREAKING, &
                                          DEF_PHYSICS_WATER_LEVEL, DEF_PHYSICS_WAVEMAKER

   implicit none

   private
   public :: type_model_physics

   type, extends(type_model_base) :: type_model_physics

      real(SP) :: water_level = 0.0_SP
      logical  :: periodic   = .false.
      logical  :: dispersion = .true.
      real(SP) :: Gamma1     = 1.0_SP
      real(SP) :: Gamma2     = 1.0_SP
      logical  :: disp_time_left = .false.   ! semi-implicit Gamma2 LHS correction; deferred post-refactor
      real(SP) :: Beta_ref   = -0.531_SP
      real(SP) :: Gamma3     = 1.0_SP
      logical  :: viscosity_breaking = .true.
      real(SP) :: SWE_ETA_DEP = 0.70_SP
      real(SP) :: C_smg       = 0.0_SP   ! Smagorinsky sub-grid viscosity coefficient
      logical  :: breaking   = .false.
      logical  :: wavemaker  = .false.
      logical  :: sediment   = .false.

   contains
      procedure :: read_input => physics_read_input
   end type type_model_physics

contains

   subroutine physics_read_input(this, env)
      class(type_model_physics), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: is_empty, no_key

      sub_env = get_sub_env(env, "physics", is_empty)
      this%is_activated = .not. is_empty
      if (is_empty) return

      call sub_env%yaml%read("water_level", val=this%water_level, default=DEF_PHYSICS_WATER_LEVEL)
      call sub_env%yaml%read("periodic",    val=this%periodic,    default=DEF_PHYSICS_PERIODIC)
      call sub_env%yaml%read("dispersion",  val=this%dispersion,  default=DEF_PHYSICS_DISPERSION)
      ! TODO: add mode enum (e.g. mode: boussinesq_full / boussinesq_linear / nswe /
      !       weakly_nonlinear) that sets Gamma1/Gamma2/Gamma3 automatically, so
      !       users never need to specify raw Gamma values directly in YAML.
      !   boussinesq_full    -> Gamma1=1, Gamma2=1, Gamma3=1  (default)
      !   boussinesq_linear  -> Gamma1=1, Gamma2=0, Gamma3=1
      !   weakly_nonlinear   -> Gamma1=1, Gamma2=1, Gamma3=0
      !   nswe               -> Gamma1=0, Gamma2=0, Gamma3=1
      call sub_env%yaml%read("Gamma1",    silent=no_key, val=this%Gamma1,    default=DEF_PHYSICS_GAMMA1)
      call sub_env%yaml%read("Gamma2",          silent=no_key, val=this%Gamma2,          default=DEF_PHYSICS_GAMMA2)
      call sub_env%yaml%read("disp_time_left", silent=no_key, val=this%disp_time_left, default=DEF_PHYSICS_DISP_TIME_LEFT)
      call sub_env%yaml%read("Beta_ref",  silent=no_key, val=this%Beta_ref,  default=DEF_PHYSICS_BETA_REF)
      call sub_env%yaml%read("Gamma3",    silent=no_key, val=this%Gamma3,    default=DEF_PHYSICS_GAMMA3)
      call sub_env%yaml%read("viscosity_breaking", val=this%viscosity_breaking, default=DEF_PHYSICS_VISCOSITY_BREAKING)
      ! 0.7 matches the legacy default (old/mod_global.F); the earlier 0.8 here
      ! was unintentional drift.
      call sub_env%yaml%read("SWE_ETA_DEP", silent=no_key, val=this%SWE_ETA_DEP, default=DEF_PHYSICS_SWE_ETA_DEP)
      call sub_env%yaml%read("C_smg",      silent=no_key, val=this%C_smg,       default=DEF_PHYSICS_C_SMG)
      call sub_env%yaml%read("breaking",   val=this%breaking,  default=DEF_PHYSICS_BREAKING)
      call sub_env%yaml%read("wavemaker", val=this%wavemaker, default=DEF_PHYSICS_WAVEMAKER)
      call sub_env%yaml%read("sediment",  val=this%sediment,  default=DEF_PHYSICS_SEDIMENT)

   end subroutine physics_read_input

end module model_physics_mod
