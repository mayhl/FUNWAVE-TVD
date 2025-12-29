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

   use yaml_file_mod, only: type_yaml_reader
   use log_io_mod, only: type_log_writer

   type type_model_interface

      type(type_yaml_reader):: yaml
      type(type_log_writer), pointer :: log

   contains

      procedure :: validate_yaml

   end type

contains

   subroutine validate_yaml(this, yaml, log)

      class(type_model_interface), intent(inout) :: this
      type(type_yaml_reader), intent(inout), target :: yaml
      type(type_log_writer), intent(inout), target :: log

      call log%exit_on_fatal("Called abstact model interface validate_yaml method.", 1)

   end subroutine validate_yaml

end module model_interface_mod
