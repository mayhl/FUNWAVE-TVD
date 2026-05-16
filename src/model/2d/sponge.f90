!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Sponge layer parameters YAML reader
!
!  YAML block: sponge:       (top-level; omit for no sponge)
!    diffusion_sponge: <bool>   default NO
!    direct_sponge:    <bool>   default NO
!    friction_sponge:  <bool>   default NO
!    Csp:              <real>   diffusion coefficient,       default 0.1
!    CDsponge:         <real>   friction drag coefficient,   default 5.0
!    Sponge_west_width:  <length>   default 0
!    Sponge_east_width:  <length>   default 0
!    Sponge_south_width: <length>   default 0
!    Sponge_north_width: <length>   default 0
!    R_sponge:  <real>   sponge relaxation rate,  default 0.85
!    A_sponge:  <real>   sponge amplitude factor, default 5.0
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_sponge_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_sponge

   type, extends(type_model_base) :: type_model_sponge

      logical  :: diffusion_sponge = .false.
      logical  :: direct_sponge    = .false.
      logical  :: friction_sponge  = .false.

      real(SP) :: Csp      = 0.1_SP
      real(SP) :: CDsponge = 5.0_SP

      real(SP) :: Sponge_west_width  = 0.0_SP
      real(SP) :: Sponge_east_width  = 0.0_SP
      real(SP) :: Sponge_south_width = 0.0_SP
      real(SP) :: Sponge_north_width = 0.0_SP

      real(SP) :: R_sponge = 0.85_SP
      real(SP) :: A_sponge = 5.0_SP

   contains
      procedure :: read_input => sponge_read_input
   end type type_model_sponge

contains

   subroutine sponge_read_input(this, env)
      class(type_model_sponge), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_sp, no_key

      sub_env = get_sub_env(env, 'sponge', is_empty=no_sp)
      this%is_activated = .not. no_sp
      if (.not. this%is_activated) return

      call sub_env%yaml%read('diffusion_sponge', val=this%diffusion_sponge, default='NO')
      call sub_env%yaml%read('direct_sponge',    val=this%direct_sponge,    default='NO')
      call sub_env%yaml%read('friction_sponge',  val=this%friction_sponge,  default='NO')

      call sub_env%yaml%read('Csp',      silent=no_key, val=this%Csp,      default='0.1')
      call sub_env%yaml%read('CDsponge', silent=no_key, val=this%CDsponge, default='5.0')

      call sub_env%yaml%read('Sponge_west_width',  silent=no_key, val=this%Sponge_west_width,  default='0.0')
      call sub_env%yaml%read('Sponge_east_width',  silent=no_key, val=this%Sponge_east_width,  default='0.0')
      call sub_env%yaml%read('Sponge_south_width', silent=no_key, val=this%Sponge_south_width, default='0.0')
      call sub_env%yaml%read('Sponge_north_width', silent=no_key, val=this%Sponge_north_width, default='0.0')

      call sub_env%yaml%read('R_sponge', silent=no_key, val=this%R_sponge, default='0.85')
      call sub_env%yaml%read('A_sponge', silent=no_key, val=this%A_sponge, default='5.0')

   end subroutine sponge_read_input

end module model_sponge_mod
