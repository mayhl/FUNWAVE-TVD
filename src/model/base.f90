!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Abstract base for model components
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!    05/13/2026  Dropped env member; env passed as argument only
!
!-------------------------------------------------

module model_base_mod

   use core_env_mod, only: type_env
   implicit none

   type, abstract, public :: type_model_base

      logical :: is_activated = .false.

   contains

      procedure(model_read_input), deferred :: read_input

   end type type_model_base

   abstract interface
      subroutine model_read_input(this, env)
         import :: type_model_base, type_env
         implicit none
         class(type_model_base), intent(inout) :: this
         type(type_env), intent(inout), target :: env
      end subroutine model_read_input
   end interface

contains

end module model_base_mod
