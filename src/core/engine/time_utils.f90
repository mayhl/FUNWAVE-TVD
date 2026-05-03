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

   function should_trigger(this, current_time) result(trigger)
      class(type_timing_control), intent(inout) :: this
      real(SP), intent(in) :: current_time
      logical :: trigger

      ! Check if we have passed the spin-up time
      if (current_time < this%t_start) then
         trigger = .false.
         return
      end if

      ! Check if interval has elapsed
      if (this%last_triggered < 0.0 .or. (current_time - this%last_triggered >= this%interval)) then
         trigger = .true.
         this%last_triggered = current_time
      else
         trigger = .false.
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
