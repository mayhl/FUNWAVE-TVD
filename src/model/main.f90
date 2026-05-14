!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Top-level model orchestrator
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!    05/13/2026  Updated to new module layout; added output/physics components
!
!-------------------------------------------------

module model_main_mod

   use core_env_mod, only: type_env, new_env
   use probe_mod, only: dump_state, reset_state

   use model_geometry_mod,   only: type_model_geometry
   use model_simulation_mod, only: type_model_simulation
   use model_hot_start_mod,  only: type_model_hot_start
   use model_wavemaker_mod,  only: type_model_wavemaker
   use model_sponge_mod,     only: type_model_sponge
   use model_obstacle_mod,   only: type_model_obstacle
   use model_friction_mod,   only: type_model_friction
   use model_numerics_mod,   only: type_model_numerics
   use model_breaking_mod,   only: type_model_breaking
   use model_output_mod,     only: type_model_output
   use model_physics_mod,    only: type_model_physics
   use model_coupling_mod,   only: type_model_coupling

   implicit none(external)

   type, public :: type_model_main
      type(type_env) :: env

      type(type_model_geometry)   :: geometry
      type(type_model_simulation) :: simulation
      type(type_model_hot_start)  :: hot_start
      type(type_model_wavemaker)  :: wavemaker
      type(type_model_sponge)     :: sponge
      type(type_model_obstacle)   :: obstacle
      type(type_model_friction)   :: friction
      type(type_model_numerics)   :: numerics
      type(type_model_breaking)   :: breaking
      type(type_model_output)     :: output
      type(type_model_physics)    :: physics
      type(type_model_coupling)   :: coupling
   contains
      procedure :: init
      procedure :: finalize => model_finalize
   end type type_model_main

contains

   subroutine init(this)
      class(type_model_main), intent(inout) :: this
      character(2048) :: yaml_path

      call reset_state()
      call dump_state(5.0d0, "main_init_test")
      call getarg(1, yaml_path)

      ! Initialize environment (Comm, Log, YAML)
      call new_env(this%env, label='main', yaml_path=trim(yaml_path), log_path='test.log')

      ! Read component inputs using environment resources
      call this%env%comm%barrier()
      call this%geometry%read_input(this%env)
      call this%simulation%read_input(this%env)
      call this%hot_start%read_input(this%env)
      call this%wavemaker%read_input(this%env)
      call this%sponge%read_input(this%env)
      call this%obstacle%read_input(this%env)
      call this%friction%read_input(this%env)
      call this%numerics%read_input(this%env)
      call this%breaking%read_input(this%env)
      call this%output%read_input(this%env)
      call this%physics%read_input(this%env)
      call this%coupling%read_input(this%env)
      ! Finalize YAML after reading all inputs
      call this%env%yaml%finalize()

   end subroutine init

   subroutine model_finalize(this)
      use mpi_f08, only: MPI_Finalize
      class(type_model_main), intent(inout) :: this
      integer :: ierr
      call this%env%finalize()
      call MPI_Finalize(ierr)
   end subroutine model_finalize

end module model_main_mod
