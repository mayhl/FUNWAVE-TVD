!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Physics toggles YAML reader (stub)
!
!  YAML block: physics:
!    dispersion: <bool>
!    breaking: <bool>
!    sponge: <bool>
!    wavemaker: <bool>
!    sediment: <bool>
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_physics_mod
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none(external)

   private
   public :: type_model_physics

   type, extends(type_model_base) :: type_model_physics

      logical :: dispersion = .true.
      logical :: breaking = .false.
      logical :: sponge = .false.
      logical :: wavemaker = .false.
      logical :: sediment = .false.

   contains
      procedure :: read_input => physics_read_input
   end type type_model_physics

contains

   subroutine physics_read_input(this, env)
      class(type_model_physics), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: is_empty

      sub_env = get_sub_env(env, 'physics', is_empty)
      this%is_activated = .not. is_empty
      if (is_empty) return

      call sub_env%yaml%read('dispersion', val=this%dispersion, default='YES')
      call sub_env%yaml%read('breaking',   val=this%breaking,   default='NO')
      call sub_env%yaml%read('sponge',     val=this%sponge,     default='NO')
      call sub_env%yaml%read('wavemaker',  val=this%wavemaker,  default='NO')
      call sub_env%yaml%read('sediment',   val=this%sediment,   default='NO')

   end subroutine physics_read_input

end module model_physics_mod
