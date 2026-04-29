!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Component
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------
module model_time_mod
   use mpi_f08
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_interface_mod, only: type_model_interface

   implicit none(external)

   private

   public type_model_time

   type, extends(type_model_interface) :: type_model_time

      real(SP) :: total
      real(SP) :: start
      real(SP) :: plot_dt
      real(SP) :: log_dt
      integer :: start_index

   contains
      procedure :: read_input => read_input
   end type type_model_time

contains

   subroutine read_input(this, env)

      class(type_model_time), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      this%env = get_sub_env(env, 'time')
      this%is_activated = .true.

      call this%env%comm%barrier()

      call this%env%yaml%read('total', val=this%total)
      call this%env%yaml%read('start', default="0.0", val=this%start)
      call this%env%yaml%read('plot', val=this%plot_dt)
      call this%env%yaml%read('log', val=this%log_dt)
      call this%env%yaml%read_positive('start index', val=this%start_index)

      call this%env%comm%barrier()

   end subroutine read_input

end module model_time_mod

