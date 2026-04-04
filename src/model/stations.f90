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
   ! NOTE: Remove in future
   use mpi_f08

   use constants_mod, only: sp
   use comm_mod, only: type_comm
   use yaml_file_mod, only: type_yaml_reader, type_path
   use log_io_mod, only: type_log_writer
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

   subroutine read_input(this, comm, yaml, log)

      class(type_model_stations), intent(inout) :: this
      class(type_comm), intent(inout) :: comm
      type(type_yaml_reader), intent(inout), target :: yaml
      type(type_log_writer), intent(inout), target :: log

      character(:), allocatable :: msg, submsg
      logical:: is_empty

      call yaml%cast_dictionary('stations', this%yaml, is_empty)
      call this%yaml%comm%barrier()
      this%log => log
      this%is_activated = .not. is_empty
      if (is_empty) return

   end subroutine read_input

end module model_stations_mod

