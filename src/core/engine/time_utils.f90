module core_time_utils_mod
   use core_constants_mod, only: SP, type_string
   implicit none

   ! Abstract interface for the callback
   abstract interface
      subroutine callback_interface()
      end subroutine callback_interface
   end interface

   type, public :: type_timing_control
      ! Standard timing parameters
      character(:), allocatable :: id
      real(SP)      :: interval
      real(SP)      :: t_start = 0.0
      real(SP)      :: t_end = 0.0
      real(SP)      :: dt = 0.0
      real(SP)      :: current_time = 0.0
      real(SP)      :: last_triggered = -1.0
      real(SP)      :: accum = 0.0   ! legacy PLOT_COUNT (dt-accumulator mode)
      integer       :: step = 0

      ! Dynamic list of operations
      type(type_string), allocatable :: ops(:)

      procedure(callback_interface), pointer, nopass :: callback => null()
   contains
      procedure, public :: should_trigger
      procedure, public :: init_from_yaml
      procedure, public :: advance
      procedure, public :: is_finished
   end type type_timing_control
contains

   ! When dt is passed (the per-step output path), the trigger runs in
   ! dt-accumulator mode — the exact legacy PLOT_COUNT arithmetic
   ! (PLOT_COUNT += DT; fire and subtract PLOT_INTV on overflow), so
   ! frame times are bit-compatible with the legacy loop given the
   ! same dt sequence.  Without dt, marker mode: fire when an interval
   ! has elapsed since the last ideal fire time (residual carries so
   ! the per-fire overshoot never accumulates as lateness).
   function should_trigger(this, current_time, dt) result(trigger)
      class(type_timing_control), intent(inout) :: this
      real(SP), intent(in) :: current_time
      real(SP), intent(in), optional :: dt
      logical :: trigger

      ! Check if we have passed the spin-up time
      if (current_time < this%t_start) then
         trigger = .false.
         return
      end if

      if (this%last_triggered < 0.0) then
         ! first call at/after t_start always fires (legacy writes the
         ! initial condition as frame one)
         trigger = .true.
         this%last_triggered = current_time
         return
      end if

      if (present(dt)) then
         this%accum = this%accum + dt
         trigger = this%accum >= this%interval
         if (trigger) this%accum = this%accum - this%interval
      else
         trigger = current_time - this%last_triggered >= this%interval
         if (trigger) this%last_triggered = this%last_triggered + this%interval
      end if
   end function should_trigger

   subroutine advance(this)
      class(type_timing_control), intent(inout) :: this
      this%current_time = this%current_time + this%dt
      this%step = this%step + 1
   end subroutine advance

   function is_finished(this) result(finished)
      class(type_timing_control), intent(in) :: this
      logical :: finished
      finished = (this%current_time >= this%t_end)
   end function is_finished

   subroutine init_from_yaml(this, yaml_reader)
      use core_yaml_file_mod, only: type_yaml_reader
      class(type_timing_control), intent(inout) :: this
      type(type_yaml_reader), intent(inout) :: yaml_reader

      call yaml_reader%read("id", val=this%id)
      call yaml_reader%read("interval", val=this%interval)

      ! Optional fields
      call yaml_reader%read("t_start", default="0.0", val=this%t_start)

      ! Dynamically load ops list if present
      if (yaml_reader%has_key("ops")) then
         call yaml_reader%read_string_array("ops", val=this%ops)
      end if
   end subroutine init_from_yaml

end module core_time_utils_mod
