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

   use comm_mod, only: type_comm
   use yaml_file_mod, only: type_yaml_reader
   use log_io_mod, only: type_log_writer

   implicit none(external)

   type, abstract :: type_model_interface

      type(type_yaml_reader):: yaml
      type(type_log_writer), pointer :: log
      logical :: is_activated = .false.

   contains

      procedure(model_read_input), deferred :: read_input

   end type

   interface
      subroutine model_read_input(this, comm, yaml, log)
         import :: type_model_interface
         import :: type_comm
         import :: type_yaml_reader
         import :: type_log_writer
         implicit none(external)
         class(type_model_interface), intent(inout) :: this
         class(type_comm), intent(inout) :: comm
         type(type_yaml_reader), intent(inout), target :: yaml
         type(type_log_writer), intent(inout), target :: log
      end subroutine model_read_input
   end interface
contains

end module model_interface_mod
