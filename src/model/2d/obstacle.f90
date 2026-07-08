!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Obstacle and breakwater parameters YAML reader
!
!  YAML block: obstacle:       (top-level; omit for no obstacle or breakwater)
!    obstacle_file:         <path>   optional; presence enables obstacle
!    breakwater_file:       <path>   optional; presence enables breakwater
!    BreakWaterAbsorbCoef:  <real>   default 10.0
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_obstacle_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_OBSTACLE_BREAKWATERABSORBCOEF

   implicit none

   private
   public :: type_model_obstacle

   type, extends(type_model_base) :: type_model_obstacle

      type(type_path) :: obstacle_file
      type(type_path) :: breakwater_file

      logical  :: obstacle   = .false.
      logical  :: breakwater = .false.

      real(SP) :: BreakWaterAbsorbCoef = 10.0_SP

   contains
      procedure :: read_input => obstacle_read_input
   end type type_model_obstacle

contains

   subroutine obstacle_read_input(this, env)
      class(type_model_obstacle), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk, no_obs, no_bw, no_key

      sub_env = get_sub_env(env, 'obstacle', is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (.not. this%is_activated) return

      call sub_env%yaml%read_input_path('obstacle_file',   silent=no_obs, val=this%obstacle_file)
      call sub_env%yaml%read_input_path('breakwater_file', silent=no_bw,  val=this%breakwater_file)

      this%obstacle   = .not. no_obs
      this%breakwater = .not. no_bw

      call sub_env%yaml%read('BreakWaterAbsorbCoef', silent=no_key, &
                              val=this%BreakWaterAbsorbCoef, default=DEF_OBSTACLE_BREAKWATERABSORBCOEF)

   end subroutine obstacle_read_input

end module model_obstacle_mod
