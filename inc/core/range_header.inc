#include "prep.inc"

!#define _NEW           _PASTE(_RANGE, _new_obj)
#define _IN_RANGE      _PASTE(_RANGE, _in_range)
#define _CAST_RANGE    _PASTE(_RANGE, _cast_range)

#define _GET_UPPER     _PASTE(_RANGE, _get_upper)
#define _GET_LOWER     _PASTE(_RANGE, _get_lower)
#define _IS_VALID      _PASTE(_RANGE, _is_valid)
#define _IS_INC_LOW    _PASTE(_RANGE, _is_inc_low)
#define _IS_INC_UPP    _PASTE(_RANGE, _is_inc_upp)
#define _RANGE_PARSE   _PASTE(_RANGE, _range_parse)

! allow(E001)
   type _RANGE
      private

      character(len=:), allocatable, public:: lower_str, upper_str
      logical :: p_is_inc_low, p_is_inc_upp
      logical :: p_is_valid, is_set
      logical :: is_range_err, is_zero

      _CLASS :: lower, upper

   contains
      procedure, public :: get_lower => _GET_LOWER
      procedure, public :: get_upper => _GET_UPPER
      procedure, public :: in_range => _IN_RANGE
      procedure, public :: is_inc_low => _IS_INC_LOW
      procedure, public :: is_inc_upp => _IS_INC_UPP
      procedure, public :: is_valid => _IS_VALID
      procedure :: cast_range => _CAST_RANGE
      procedure :: parse => _RANGE_PARSE
   end type _RANGE

   ! interface _RANGE
   !    procedure :: new_obj => _NEW
   ! end interface _RANGE

#undef _RANGE
#undef _CLASS
#undef _FMT
