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

   use core_comm_mod, only: type_comm
   use core_yaml_file_mod, only: type_yaml_reader
   use core_log_io_mod, only: type_log_writer
   use core_env_mod, only: type_env

   implicit none(external)

   type, abstract :: type_model_interface

      type(type_yaml_reader) :: yaml
      type(type_log_writer), pointer :: log => null()
      logical :: is_activated = .false.

   contains

      procedure(model_read_input), deferred :: read_input

   end type

   interface
      subroutine model_read_input(this, env)
         import :: type_model_interface
         import :: type_env
         implicit none(external)
         class(type_model_interface), intent(inout) :: this
         type(type_env), intent(inout), target :: env
      end subroutine model_read_input
   end interface
contains

end module model_interface_mod
