!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D turbulence / viscosity parameters YAML reader
!
!  YAML block: turbulence:
!    viscous_flow: <bool>      default NO
!    ivturb: <int>             vertical turbulence model, default 0
!    ihturb: <int>             horizontal turbulence model, default 0
!    visc: <real>              kinematic viscosity (VISCOSITY), default 1e-6
!    schmidt: <real>           Schmidt number, default 1.0
!    cvs: <real>               vertical Schmidt coeff, default 0.0
!    chs: <real>               horizontal Schmidt coeff, default 0.0
!    viscous_number: <real>    VISCOUS_NUMBER, default 0.0
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_turbulence_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_turbulence

   type, extends(type_model_base) :: type_model_3d_turbulence

      logical  :: viscous_flow = .false.
      integer  :: ivturb = 0
      integer  :: ihturb = 0
      real(SP) :: visc = 1.0e-6_SP
      real(SP) :: schmidt = 1.0_SP
      real(SP) :: cvs = 0.0_SP
      real(SP) :: chs = 0.0_SP
      real(SP) :: viscous_number = 0.0_SP

   contains
      procedure :: read_input => turbulence_3d_read_input
   end type type_model_3d_turbulence

contains

   subroutine turbulence_3d_read_input(this, env)
      class(type_model_3d_turbulence), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_key

      sub_env = get_sub_env(env, "turbulence")
      this%is_activated = .true.

      call sub_env%yaml%read("viscous_flow", val=this%viscous_flow, default="NO")
      call sub_env%yaml%read("ivturb", val=this%ivturb, default="0")
      call sub_env%yaml%read("ihturb", val=this%ihturb, default="0")
      call sub_env%yaml%read("visc", silent=no_key, val=this%visc, default="1.0e-6")
      call sub_env%yaml%read("schmidt", silent=no_key, val=this%schmidt, default="1.0")
      call sub_env%yaml%read("cvs", silent=no_key, val=this%cvs, default="0.0")
      call sub_env%yaml%read("chs", silent=no_key, val=this%chs, default="0.0")
      call sub_env%yaml%read("viscous_number", silent=no_key, val=this%viscous_number, default="0.0")

   end subroutine turbulence_3d_read_input

end module model_3d_turbulence_mod
