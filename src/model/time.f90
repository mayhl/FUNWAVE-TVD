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
   use core_constants_mod, only: SP
   use core_comm_mod, only: type_comm
   use core_yaml_file_mod, only: type_yaml_reader, type_path
   use core_log_io_mod, only: type_log_writer
   use core_env_mod, only: type_env
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

   subroutine read_input(this, env)

      class(type_model_time), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      character(:), allocatable :: msg, submsg

      call env%yaml%cast_dictionary('time', this%yaml)

      call env%comm%barrier()

      call this%yaml%read('total', val=this%total)
      call this%yaml%read('start', default="0.0", val=this%start)
      call this%yaml%read('plot', val=this%plot_dt)
      call this%yaml%read('log', val=this%log_dt)
      call this%yaml%read_positive('start index', val=this%start_index)

      this%log => env%log

   end subroutine read_input

end module model_time_mod

