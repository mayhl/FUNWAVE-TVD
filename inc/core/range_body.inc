#include "prep.inc"

#define _IN_RANGE      _PASTE(_RANGE, _in_range)
#define _CAST_RANGE    _PASTE(_RANGE, _cast_range)

#define _GET_UPPER     _PASTE(_RANGE, _get_upper)
#define _GET_LOWER     _PASTE(_RANGE, _get_lower)
#define _IS_VALID      _PASTE(_RANGE, _is_valid)
#define _IS_INC_LOW    _PASTE(_RANGE, _is_inc_low)
#define _IS_INC_UP     _PASTE(_RANGE, _is_inc_upp)
#define _RANGE_PARSE   _PASTE(_RANGE, _range_parse)

   function _GET_UPPER(this) result(val)
      class(_RANGE), intent(inout) :: this
      type(_CLASS) :: val
      val = this%upper
   end function _GET_UPPER

   function _GET_LOWER(this) result(val)
      class(_RANGE), intent(inout) :: this
      type(_CLASS) :: val
      val = this%lower
   end function _GET_LOWER

   function _IS_VALID(this) result(val)
      class(_RANGE), intent(inout) :: this
      logical:: val
      val = this%p_IS_VALID
   end function _IS_VALID

   logical function _IS_INC_LOW(this)
      class(_RANGE), intent(inout) :: this
      _IS_INC_LOW = this%p_is_inc_low
   end function _IS_INC_LOW

   logical function _IS_INC_UPP(this)
      class(_RANGE), intent(inout) :: this
      _IS_INC_UPP = this%p_is_inc_upp
   end function _IS_INC_UPP

   subroutine _RANGE_PARSE(this, range)

      class(_RANGE), intent(inout) :: this
      character(len=*), intent(in), optional :: range
      character(1) :: lbound, ubound
      integer :: count, pos

      if (present(range)) then
         this%is_set = .true.
      else
         this%is_set = .false.
         this%p_is_valid = .true.
         return
      end if

      count = count_char(range, ',')

      this%p_is_valid = count == 1
      if (.not. this%p_is_valid) return

      pos = index(range, ",")
      count = len_trim(range)
      this%lower_str = trim(adjustl(range(1:pos - 1)))
      this%upper_str = trim(adjustl(range(pos + 1:count)))

      count = len_trim(this%upper_str)
      lbound = this%lower_str(1:1)
      ubound = this%upper_str(count:count)

      select case (lbound)
      case ("(")
         this%p_is_inc_low = .false.
      case ("[")
         this%p_is_inc_low = .true.
      case default
         this%p_is_valid = .false.
         return
      end select

      count = len_trim(this%lower_str)
      this%lower_str = trim(adjustl(this%lower_str(2:count)))

      if (len_trim(this%lower_str) == 0) then
         this%p_is_valid = .false.
         return
      end if

      select case (ubound)
      case (")")
         this%p_is_inc_upp = .false.
      case ("]")
         this%p_is_inc_upp = .true.
      case default
         this%p_is_valid = .false.
         return
      end select

      count = len_trim(this%upper_str)
      this%upper_str = trim(adjustl(this%upper_str(1:count - 1)))

      if (len_trim(this%upper_str) == 0) then
         this%p_is_valid = .false.
         return
      end if

      this%p_is_valid = .true.
      call this%cast_range()

   end subroutine _RANGE_PARSE

   function _IN_RANGE(this, x) result(val)
      class(_RANGE), intent(inout) :: this
      logical:: val
      type(_CLASS), intent(in) :: x
      val = .true.
      this%lower = x

   end function _IN_RANGE

   subroutine _CAST_RANGE(this)
      class(_RANGE), intent(inout) :: this
      integer :: stat

      if (.not. this%p_is_valid) return

      read (this%lower_str, _FMT, iostat=stat) this%lower
      if (stat /= 0) then
         this%p_is_valid = .false.
         return
      end if

      read (this%upper_str, _FMT, iostat=stat) this%upper
      if (stat /= 0) then
         this%p_is_valid = .false.
         return
      end if

      if (this%lower >= this%upper) then
         this%p_is_valid = .false.
         return
      end if

   end subroutine _CAST_RANGE

#undef _RANGE
#undef _CLASS
#undef _FMT
