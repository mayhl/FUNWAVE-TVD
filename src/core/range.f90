!> @brief Module for parsing integer and real range strings.
!!
!! Provides types and parsing functionality to convert string ranges (e.g., "1:10")
!! into structured integer or real range objects.
!!
!! @note This module uses base type extension for implementation.
module core_range_parse_mod

   use core_constants_mod, only: MESSAGE_SIZE, STRING_SIZE, LABEL_SIZE, SP
   use core_log_io_mod, only: type_log_writer
   use core_comm_mod, only: type_comm
   use core_misc_mod, only: str2int, str2real, count_char
   use filesystem, only: type_path => path_t

   implicit none(external)

   private
!> @brief Base type for range logic
   type, public, abstract :: type_base_range
      character(len=:), allocatable :: lower_str, upper_str
      logical :: is_set = .false.
      logical :: p_is_valid = .false.
      logical :: p_is_inc_low = .false.
      logical :: p_is_inc_upp = .false.
      logical :: is_lower_set = .false.
      logical :: is_upper_set = .false.
   contains
      procedure, public :: initialize => base_range_initialize
      procedure, public :: is_valid => base_range_is_valid
      procedure, public :: is_inc_low => base_range_is_inc_low
      procedure, public :: is_inc_upp => base_range_is_inc_upp
      procedure, public :: get_lower => base_range_get_lower
      procedure, public :: get_upper => base_range_get_upper
      procedure, public :: parse => base_range_parse
   end type type_base_range

!> @brief Integer range type
   type, extends(type_base_range), public :: type_integer_range
      integer :: lower, upper
   contains
      procedure, public :: in_range => in_range_integer
      procedure, public :: cast_range => cast_integer_range
   end type type_integer_range

!> @brief Real range type
   type, extends(type_base_range), public :: type_real_range
      real(SP) :: lower, upper
   contains
      procedure, public :: in_range => in_range_real
      procedure, public :: cast_range => cast_real_range
   end type type_real_range

   !> @brief Constructor interface for integer ranges
   interface type_integer_range
      module procedure new_integer_range
   end interface type_integer_range

   !> @brief Constructor interface for real ranges
   interface type_real_range
      module procedure new_real_range
   end interface type_real_range

contains

   ! Base class procedures
   subroutine base_range_initialize(this)
      class(type_base_range), intent(inout) :: this
      this%is_set = .false.
      this%p_is_valid = .false.
      if (allocated(this%lower_str)) deallocate (this%lower_str)
      if (allocated(this%upper_str)) deallocate (this%upper_str)
   end subroutine base_range_initialize

   !> @brief Constructor function for integer range
   function new_integer_range(range_str) result(this)
      type(type_integer_range) :: this
      character(len=*), intent(in) :: range_str
      call this%initialize()
      call this%parse(range_str)
      call this%cast_range()
   end function new_integer_range

   !> @brief Constructor function for real range
   function new_real_range(range_str) result(this)
      type(type_real_range) :: this
      character(len=*), intent(in) :: range_str
      call this%initialize()
      call this%parse(range_str)
      call this%cast_range()
   end function new_real_range

   ! Base class procedures
   function base_range_is_valid(this) result(val)
      class(type_base_range), intent(in) :: this
      logical :: val
      val = this%p_is_valid
   end function base_range_is_valid

   function base_range_get_upper(this) result(val)
      class(type_base_range), intent(in) :: this
      character(len=:), allocatable :: val
      val = this%upper_str
   end function base_range_get_upper

   function base_range_get_lower(this) result(val)
      class(type_base_range), intent(in) :: this
      character(len=:), allocatable :: val
      val = this%lower_str
   end function base_range_get_lower

   function base_range_is_inc_low(this) result(val)
      class(type_base_range), intent(in) :: this
      logical :: val
      val = this%p_is_inc_low
   end function base_range_is_inc_low

   function base_range_is_inc_upp(this) result(val)
      class(type_base_range), intent(in) :: this
      logical :: val
      val = this%p_is_inc_upp
   end function base_range_is_inc_upp

   subroutine base_range_parse(this, range_str)
      class(type_base_range), intent(inout) :: this
      character(len=*), intent(in) :: range_str
      character(1) :: lbound, ubound
      integer :: count, pos

      this%is_set = .true.
      this%p_is_valid = .true.

      ! Count commas to check for basic split
      count = count_char(range_str, ',')
      if (count /= 1) then
         this%p_is_valid = .false.
         return
      end if

      pos = index(range_str, ",")
      count = len_trim(range_str)
      this%lower_str = trim(adjustl(range_str(1:pos - 1)))
      this%upper_str = trim(adjustl(range_str(pos + 1:count)))

      ! Extract bounds characters
      count = len_trim(this%upper_str)
      lbound = this%lower_str(1:1)
      ubound = this%upper_str(count:count)

      ! Determine inclusivity
      select case (lbound)
      case ("("); this%p_is_inc_low = .false.
      case ("["); this%p_is_inc_low = .true.
      case default
         this%p_is_valid = .false.
         return
      end select

      select case (ubound)
      case (")"); this%p_is_inc_upp = .false.
      case ("]"); this%p_is_inc_upp = .true.
      case default
         this%p_is_valid = .false.
         return
      end select

      ! Strip boundary characters
      this%lower_str = trim(adjustl(this%lower_str(2:len_trim(this%lower_str))))
      this%upper_str = trim(adjustl(this%upper_str(1:len_trim(this%upper_str) - 1)))

      this%is_lower_set = (len_trim(this%lower_str) > 0)
      this%is_upper_set = (len_trim(this%upper_str) > 0)

   end subroutine base_range_parse

   ! Integer Implementations

   function in_range_integer(this, val) result(is_in)
      class(type_integer_range), intent(in) :: this
      integer, intent(in) :: val
      logical :: is_in, lower_ok, upper_ok

      if (.not. this%p_is_valid) then; is_in = .false.; return; end if

      ! Lower bound check
      if (this%is_lower_set) then
         if (this%p_is_inc_low) then
            lower_ok = (val >= this%lower)
         else
            lower_ok = (val > this%lower)
         end if
      else
         lower_ok = .true.
      end if

      ! Upper bound check
      if (this%is_upper_set) then
         if (this%p_is_inc_upp) then
            upper_ok = (val <= this%upper)
         else
            upper_ok = (val < this%upper)
         end if
      else
         upper_ok = .true.
      end if

      is_in = lower_ok .and. upper_ok
   end function in_range_integer

   subroutine cast_integer_range(this)
      class(type_integer_range), intent(inout) :: this
      logical :: is_lower_ok, is_upper_ok
      integer :: stat

      if (.not. this%p_is_valid) return

      if (this%is_lower_set) then
         read (this%lower_str, '(I20)', iostat=stat) this%lower
         is_lower_ok = (stat == 0)
      else
         is_lower_ok = .true.
      end if

      if (this%is_upper_set) then
         read (this%upper_str, '(I20)', iostat=stat) this%upper
         is_upper_ok = (stat == 0)
      else
         is_upper_ok = .true.
      end if

      if (.not. (is_lower_ok .and. is_upper_ok)) then
         this%p_is_valid = .false.
      else
         if (this%is_lower_set .and. this%is_upper_set) then
            this%p_is_valid = (this%lower < this%upper)
         else
            this%p_is_valid = .true.
         end if
      end if

   end subroutine cast_integer_range

   ! Real Implementations
   function in_range_real(this, val) result(is_in)
      class(type_real_range), intent(in) :: this
      real(SP), intent(in) :: val
      logical :: is_in, lower_ok, upper_ok

      if (.not. this%p_is_valid) then; is_in = .false.; return; end if

      ! Lower bound check
      if (this%is_lower_set) then
         if (this%p_is_inc_low) then
            lower_ok = (val >= this%lower)
         else
            lower_ok = (val > this%lower)
         end if
      else
         lower_ok = .true.
      end if

      ! Upper bound check
      if (this%is_upper_set) then
         if (this%p_is_inc_upp) then
            upper_ok = (val <= this%upper)
         else
            upper_ok = (val < this%upper)
         end if
      else
         upper_ok = .true.
      end if

      is_in = lower_ok .and. upper_ok
   end function in_range_real

   subroutine cast_real_range(this)
      class(type_real_range), intent(inout) :: this
      integer :: stat
      logical :: is_lower_ok, is_upper_ok

      if (.not. this%p_is_valid) return

      if (this%is_lower_set) then
         read (this%lower_str, *, iostat=stat) this%lower
         is_lower_ok = (stat == 0)
      else
         is_lower_ok = .true.
      end if

      if (this%is_upper_set) then
         read (this%upper_str, *, iostat=stat) this%upper
         is_upper_ok = (stat == 0)
      else
         is_upper_ok = .true.
      end if

      if (.not. (is_lower_ok .and. is_upper_ok)) then
         this%p_is_valid = .false.
      else
         if (this%is_lower_set .and. this%is_upper_set) then
            this%p_is_valid = (this%lower < this%upper)
         else
            this%p_is_valid = .true.
         end if
      end if
   end subroutine cast_real_range

end module core_range_parse_mod
