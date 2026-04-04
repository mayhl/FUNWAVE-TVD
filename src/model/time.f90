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

module model_time_mod
   ! NOTE: Remove in future
   use mpi_f08

   use constants_mod, only: SP
   use comm_mod, only: type_comm
   use yaml_file_mod, only: type_yaml_reader, type_path
   use log_io_mod, only: type_log_writer
   use model_interface_mod, only: type_model_interface

   implicit none(external)

   private

   public type_model_time

   type, extends(type_model_interface) :: type_model_time

      real(SP) :: total
      real(SP) :: start
      real(SP) :: plot_dt
      real(SP) :: log_dt
      integer :: start_index

   contains
      procedure :: read_input => read_input
   end type type_model_time

contains

   subroutine read_input(this, comm, yaml, log)

      class(type_model_time), intent(inout) :: this
      class(type_comm), intent(inout) :: comm
      type(type_yaml_reader), intent(inout), target :: yaml
      type(type_log_writer), intent(inout), target :: log

      character(:), allocatable :: msg, submsg

      call yaml%cast_dictionary('time', this%yaml)

      call this%yaml%comm%barrier()

      call this%yaml%read_time('total', val=this%total)
      call this%yaml%read_time('start', default="0.0", val=this%start)
      call this%yaml%read_time('plot', val=this%plot_dt)
      call this%yaml%read_time('log', val=this%log_dt)
      call this%yaml%read_positive_integer('start index', val=this%start_index)

      this%log => log

   end subroutine read_input

end module model_time_mod

