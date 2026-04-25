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
   use core_constants_mod, only: sp
   use core_comm_mod, only: type_comm
   use core_yaml_file_mod, only: type_yaml_reader, type_path
   use core_log_io_mod, only: type_log_writer
   use core_env_mod, only: type_env
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

      character(:), allocatable :: msg, submsg
      logical:: is_empty

      call env%yaml%cast_dictionary('stations', this%yaml, is_empty)
      call env%comm%barrier()
      this%log => env%log
      this%is_activated = .not. is_empty
      if (is_empty) return

   end subroutine read_input

end module model_stations_mod

