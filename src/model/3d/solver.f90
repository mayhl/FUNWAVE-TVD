!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D Poisson solver parameters YAML reader
!
!  YAML block: solver:
!    solver_type: <int>    isolver, default 1
!    max_iter: <int>       itmax,   default 500
!    tolerance: <real>     tol,     default 1.0e-6
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_solver_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_solver

   type, extends(type_model_base) :: type_model_3d_solver

      integer  :: solver_type = 1
      integer  :: max_iter    = 500
      real(SP) :: tolerance   = 1.0e-6_SP

   contains
      procedure :: read_input => solver_3d_read_input
   end type type_model_3d_solver

contains

   subroutine solver_3d_read_input(this, env)
      class(type_model_3d_solver), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_key

      sub_env = get_sub_env(env, "solver")
      this%is_activated = .true.

      call sub_env%yaml%read("solver_type", silent=no_key, val=this%solver_type, default="1")
      call sub_env%yaml%read("max_iter",    silent=no_key, val=this%max_iter,    default="500")
      call sub_env%yaml%read("tolerance",   silent=no_key, val=this%tolerance,   default="1.0e-6")

   end subroutine solver_3d_read_input

end module model_3d_solver_mod
