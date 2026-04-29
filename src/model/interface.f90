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

module model_interface_mod

   use core_env_mod, only: type_env
   implicit none(external)

   type, abstract, public :: type_model_interface

      type(type_env), pointer :: env => null()
      logical :: is_activated = .false.

   contains

      procedure(model_read_input), deferred :: read_input

   end type type_model_interface

   abstract interface
      subroutine model_read_input(this, env)
         import :: type_model_interface, type_env
         implicit none(external)
         class(type_model_interface), intent(inout) :: this
         type(type_env), intent(inout), target :: env
      end subroutine model_read_input
   end interface

contains

end module model_interface_mod
