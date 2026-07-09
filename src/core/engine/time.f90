module core_simulation_time_mod
   use core_constants_mod, only: SP
   use core_yaml_file_mod, only: type_yaml_reader
   implicit none

   type, public :: type_simulation_control
      ! --- Simulation State ---
      real(SP) :: t_start = 0.0
      real(SP) :: t_end = 0.0
      real(SP) :: current_time = 0.0
      integer  :: step = 0

   contains
      procedure :: init_from_yaml
      procedure :: advance
      procedure :: is_finished
   end type type_simulation_control

contains

   subroutine init_from_yaml(this, reader)
      class(type_simulation_control), intent(inout) :: this
      type(type_yaml_reader), intent(inout) :: reader

      type(type_yaml_reader) :: time_reader

      if (reader%is_dictionary("time")) then
         time_reader = reader%cast_dictionary("time")
         call time_reader%read("start", default="0.0", val=this%t_start)
         call time_reader%read("total_time", val=this%t_end)
      else
         call reader%log%exit_on_error("Missing 'time' configuration block")
      end if

      this%current_time = this%t_start
      this%step = 0
   end subroutine init_from_yaml

   subroutine advance(this, dt_in)
      class(type_simulation_control), intent(inout) :: this
      real(SP), intent(in) :: dt_in

      this%current_time = this%current_time + dt_in
      this%step = this%step + 1
   end subroutine advance

   function is_finished(this) result(finished)
      class(type_simulation_control), intent(in) :: this
      logical :: finished
      finished = (this%current_time >= this%t_end)
   end function is_finished

end module core_simulation_time_mod
