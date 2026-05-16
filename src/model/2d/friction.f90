!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Bottom friction parameters YAML reader
!
!  YAML block: friction:       (top-level; omit for no friction)
!    friction_matrix: <bool>   use spatially varying Cd file, default NO
!    friction_file:   <path>   required when friction_matrix: YES
!    Cd:              <real>   constant drag coefficient, default 0.0
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_friction_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_friction

   type, extends(type_model_base) :: type_model_friction

      logical  :: friction_matrix = .false.
      logical  :: no_cd_file      = .true.
      type(type_path) :: cd_file

      real(SP) :: Cd_fixed = 0.0_SP

   contains
      procedure :: read_input => friction_read_input
   end type type_model_friction

contains

   subroutine friction_read_input(this, env)
      class(type_model_friction), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_fr, no_key

      sub_env = get_sub_env(env, 'friction', is_empty=no_fr)
      this%is_activated = .not. no_fr
      if (.not. this%is_activated) return

      call sub_env%yaml%read('friction_matrix', val=this%friction_matrix, default='NO')
      call sub_env%yaml%read_input_path('friction_file', silent=this%no_cd_file, val=this%cd_file)
      call sub_env%yaml%read('Cd', silent=no_key, val=this%Cd_fixed, default='0.0')

   end subroutine friction_read_input

end module model_friction_mod
