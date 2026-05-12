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

module model_stations_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_interface_mod, only: type_model_interface

   implicit none(external)

   private

   public type_model_stations

   type, extends(type_model_interface) :: type_model_stations

      real(SP) :: dt
      character(:), allocatable :: path
      integer :: n
      integer :: buffer_size

   contains
      procedure :: read_input => read_input

   end type type_model_stations

contains

   subroutine read_input(this, env)

      class(type_model_stations), intent(inout) :: this
      type(type_env), intent(inout), target :: env
      logical :: is_empty

      this%env = get_sub_env(env, 'stations', is_empty)
      this%is_activated = .not. is_empty

      call this%env%comm%barrier()
      if (is_empty) return

      call this%env%yaml%read_positive('dt', val=this%dt)
      call this%env%yaml%read('file path', val=this%path)
      call this%env%yaml%read_positive('number', val=this%n)
      call this%env%yaml%read_positive('buffer size', val=this%buffer_size, default="1000")

      call this%env%comm%barrier()

   end subroutine read_input

end module model_stations_mod

