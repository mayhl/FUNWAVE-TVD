module core_simulation_time_mod
   use core_constants_mod, only: SP
   use core_yaml_file_mod, only: type_yaml_reader
   use core_accumulators_mod, only: type_accumulator
   implicit none

   type, public :: type_simulation_control
      ! --- Simulation State ---
      real(SP) :: t_start = 0.0
      real(SP) :: t_end = 0.0
      real(SP) :: current_time = 0.0
      integer  :: step = 0

      ! --- Telemetry ---
      type(type_accumulator) :: stats

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
         call time_reader%read("start", default="0.0", val=this%t_start, dim="time")
         call time_reader%read("end", val=this%t_end, dim="time")
      else
         call reader%log%exit_on_error("Missing 'time' configuration block")
      end if

      this%current_time = this%t_start
      this%step = 0

      ! Initialize accumulator for DT tracking
      call this%stats%init(1, 1, "dt")
      call this%stats%allocate_stat("min")
      call this%stats%allocate_stat("max")
   end subroutine init_from_yaml

   subroutine advance(this, dt_in)
      class(type_simulation_control), intent(inout) :: this
      real(SP), intent(in) :: dt_in
      real(SP), dimension(1, 1) :: dt_arr

      this%current_time = this%current_time + dt_in
      this%step = this%step + 1

      ! Track dt statistics
      dt_arr(1, 1) = dt_in
      call this%stats%accumulate(dt_arr, dt_in)
   end subroutine advance

   function is_finished(this) result(finished)
      class(type_simulation_control), intent(in) :: this
      logical :: finished
      finished = (this%current_time >= this%t_end)
   end function is_finished

end module core_simulation_time_mod
