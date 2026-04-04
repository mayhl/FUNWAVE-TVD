
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

module model_main_mod

   use mpi_f08

   use constants_mod, only: LABEL_SIZE
   use log_io_mod, only: initialize_logger
   use comm_mod, only: type_comm
   use yaml_file_mod, only: type_yaml_reader
   use log_io_mod, only: type_log_writer

   use model_grid_mod, only: type_model_grid
   use model_time_mod, only: type_model_time
   use model_stations_mod, only: type_model_stations

   implicit none(external)

   type type_model_main

      type(type_log_writer) :: log
      type(type_comm) :: comm

      type(type_model_grid) :: grid
      type(type_model_time) :: time
      type(type_model_stations) :: stations
   contains

      procedure :: init
      procedure :: read_input

   end type type_model_main

contains

   subroutine init(this)
      class(type_model_main), intent(inout) :: this
      character(:), allocatable :: log_fpath
      !CHARACTER(*), ALLOCATABLE :: err_msg
      logical :: is_io_node

      log_fpath = 'test.log'
      call initialize_logger(log_fpath, 0, 0, 100)
      call this%comm%init(io_rank_id=0)

      deallocate (log_fpath)
   end subroutine init

   subroutine read_input(this)

      class(type_model_main), intent(inout) :: this

      character(LABEL_SIZE) :: log_label = 'config'
      character(2048) :: path

      type(type_yaml_reader):: yaml
      character(:), allocatable :: yaml_path
      integer(MPI_ADDRESS_KIND):: address

      call getarg(1, path)

      this%log = this%comm%get_logger(log_label)

      yaml_path = path
      call yaml%init(yaml_path, this%comm)

      call this%comm%barrier()
      call this%grid%read_input(this%comm, yaml, this%log)
      call this%time%read_input(this%comm, yaml, this%log)
      call this%stations%read_input(this%comm, yaml, this%log)

   end subroutine read_input

end module model_main_mod
