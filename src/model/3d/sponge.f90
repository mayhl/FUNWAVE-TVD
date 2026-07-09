!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D sponge layer parameters YAML reader
!
!  YAML block: sponge:           (optional; omit for no sponge)
!    west_width:  <real>         Sponge_West_Width,  default 0.0
!    east_width:  <real>         Sponge_East_Width,  default 0.0
!    south_width: <real>         Sponge_South_Width, default 0.0
!    north_width: <real>         Sponge_North_Width, default 0.0
!    r_sponge:    <real>         default 0.85
!    a_sponge:    <real>         default 5.0
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_sponge_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_sponge

   type, extends(type_model_base) :: type_model_3d_sponge

      real(SP) :: west_width  = 0.0_SP
      real(SP) :: east_width  = 0.0_SP
      real(SP) :: south_width = 0.0_SP
      real(SP) :: north_width = 0.0_SP
      real(SP) :: r_sponge    = 0.85_SP
      real(SP) :: a_sponge    = 5.0_SP

   contains
      procedure :: read_input => sponge_3d_read_input
   end type type_model_3d_sponge

contains

   subroutine sponge_3d_read_input(this, env)
      class(type_model_3d_sponge), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: is_empty, no_key

      sub_env = get_sub_env(env, "sponge", is_empty)
      this%is_activated = .not. is_empty
      if (.not. this%is_activated) return

      call sub_env%yaml%read("west_width",  silent=no_key, val=this%west_width,  default="0.0")
      call sub_env%yaml%read("east_width",  silent=no_key, val=this%east_width,  default="0.0")
      call sub_env%yaml%read("south_width", silent=no_key, val=this%south_width, default="0.0")
      call sub_env%yaml%read("north_width", silent=no_key, val=this%north_width, default="0.0")
      call sub_env%yaml%read("r_sponge",    silent=no_key, val=this%r_sponge,    default="0.85")
      call sub_env%yaml%read("a_sponge",    silent=no_key, val=this%a_sponge,    default="5.0")

   end subroutine sponge_3d_read_input

end module model_3d_sponge_mod
