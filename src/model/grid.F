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

module model_grid_mod

   use yaml_file_mod, only: type_yaml_reader
   use log_io_mod, only: type_log_writer
   use model_interface_mod, only: type_model_interface

   type, extends(type_model_interface) :: type_model_grid

      integer :: nx, ny
      integer :: nx_proc, ny_procs

   contains
      procedure :: validate_yaml => model_validate_yaml

   end type type_model_grid

contains

   subroutine model_validate_yaml(this, yaml, log)

      class(type_model_grid), intent(inout) :: this
      type(type_yaml_reader), intent(inout), target :: yaml
      type(type_log_writer), intent(inout), target :: log

      logical :: is_px_empty, is_py_empty, is_p_empty

      integer :: np, nxp, nyp

      call yaml%cast_dictionary('grid', this%yaml)
      this%log => log

      call this%yaml%read_integer('n_procs', val=np, is_empty=is_p_empty)
      call this%yaml%read_integer('nx_proc', val=nxp, is_empty=is_px_empty)
      call this%yaml%read_integer('ny_proc', val=nyp, is_empty=is_py_empty)

      call this%yaml%read_integer('dummy', val=nyp, default_val='5')

      print *, is_p_empty, is_px_empty, is_py_empty

   end subroutine model_validate_yaml

end module model_grid_mod

