!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Station output YAML reader
!
!  NOTE: This module is deprecated. Stations are being absorbed into
!  type_model_output as output channels with geometry='station'.
!  Retained temporarily for legacy compatibility.
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!    05/13/2026  Fixed env storage UB; env passed as argument only
!
!-------------------------------------------------

module model_stations_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use model_base_mod, only: type_model_base

   implicit none(external)

   private
   public :: type_model_stations

   type, extends(type_model_base) :: type_model_stations

      real(SP) :: dt = 0.0_SP
      character(:), allocatable :: path
      integer :: n = 0
      integer :: buffer_size = 1000

   contains
      procedure :: read_input => stations_read_input

   end type type_model_stations

contains

   subroutine stations_read_input(this, env)
      class(type_model_stations), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: is_empty

      sub_env = get_sub_env(env, 'stations', is_empty)
      this%is_activated = .not. is_empty
      if (is_empty) return

      call sub_env%yaml%read_positive('dt', val=this%dt)
      call sub_env%yaml%read('file path', val=this%path)
      call sub_env%yaml%read_positive('number', val=this%n)
      call sub_env%yaml%read_positive('buffer size', val=this%buffer_size, default="1000")

   end subroutine stations_read_input

end module model_stations_mod
