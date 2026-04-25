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

   use core_constants_mod, only: LABEL_SIZE
   use core_env_mod, only: type_env, new_env

   use model_grid_mod, only: type_model_grid
   use model_time_mod, only: type_model_time
   use model_stations_mod, only: type_model_stations

   implicit none(external)

   type type_model_main
      type(type_env) :: env

      type(type_model_grid) :: grid
      type(type_model_time) :: time
      type(type_model_stations) :: stations
   contains
      procedure :: init
      procedure :: finalize => model_finalize
   end type type_model_main

contains

   subroutine init(this)
      class(type_model_main), intent(inout) :: this
      character(2048) :: yaml_path

      call getarg(1, yaml_path)

      ! Initialize environment (Comm, Log, YAML)
      this%env = new_env(label='main', yaml_path=trim(yaml_path), log_path='test.log')

      ! Read component inputs using environment resources
      call this%env%comm%barrier()
      call this%grid%read_input(this%env%comm, this%env%yaml, this%env%log)
      call this%time%read_input(this%env%comm, this%env%yaml, this%env%log)
      call this%stations%read_input(this%env%comm, this%env%yaml, this%env%log)

      ! Finalize YAML after reading all inputs
      call this%env%yaml%finalize()

   end subroutine init

   subroutine model_finalize(this)
      class(type_model_main), intent(inout) :: this
      call this%env%finalize()
   end subroutine model_finalize

end module model_main_mod
