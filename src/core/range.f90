module core_range_parse_mod

   use core_constants_mod, only: MESSAGE_SIZE, STRING_SIZE, LABEL_SIZE, SP
   use core_log_io_mod, only: type_log_writer
   use core_comm_mod, only: type_comm
   use core_misc_mod, only: str2int, str2real, count_char
   use filesystem, only: type_path => path_t

   implicit none(external)

   private

   public type_integer_range, type_real_range

#define _RANGE type_integer_range
#define _CLASS integer
#include "core/range_header.inc"

#define _RANGE type_real_range
#define _CLASS real(SP)
#include "core/range_header.inc"

contains

#define _RANGE type_integer_range
#define _CLASS integer
#define _FMT "(I20)"
#include "core/range_body.inc"
   !
#define _RANGE type_real_range
#define _CLASS real(SP)
#define _FMT *
#include "core/range_body.inc"

end module core_range_parse_mod
