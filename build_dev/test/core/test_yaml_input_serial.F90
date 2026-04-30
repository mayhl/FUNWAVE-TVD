
! TODO: Add dict tests

! allow(E001)
module test_yaml_input_serial

   use pFUnit

   use core_comm_mod, only: type_comm, new_comm
   use core_constants_mod, only: SP, MESSAGE_SIZE
   use core_log_io_mod, only: type_log_writer, new_log_writer
   use core_misc_mod, only: str2int
   use core_path_mod, only: type_path
   use core_yaml_file_mod, only: type_yaml_reader
   use iso_fortran_env, only: error_unit

   implicit none(external)

contains

   subroutine dummy_fix_lint()
      ! Dummy function to fix linter, seems to reset @ errors
   end subroutine dummy_fix_lint

   subroutine open_yaml(reader, comm)

      type(type_yaml_reader), intent(inout) :: reader
      type(type_comm), intent(inout) :: comm
      character(:), allocatable :: yaml_path
      character(:), allocatable :: log_path
      type(type_log_writer) :: log_dummy

      log_path = "./new.log"
      yaml_path = "test.yaml"

      log_dummy = new_log_writer('test', .true., log_path, 0, 0, 100)

      comm = new_comm(io_rank_id=0)
      call reader%init(yaml_path, comm)

   end subroutine open_yaml
   subroutine test_integer()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      integer :: val

      call open_yaml(reader, comm)

      val = 10
      call reader%read('integer', val=val)
#line 52 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(1, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 52) )
  if (anyExceptions()) return
#line 53 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      val = 1
      call reader%read('dummy_int', default="10", val=val)
#line 56 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(10, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 56) )
  if (anyExceptions()) return
#line 57 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      ! TODO: Fix error
      val = 1
      call reader%read('dummy_real', default="10.5", val=val)
#line 61 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('Default value for /dummy_real is set to "10.5", which cannot be interpreted as an integer.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 61) )
#line 62 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      val = 1
      call reader%read('real', val=val)
#line 65 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/real is set to "0.1", which cannot be interpreted as an integer.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 65) )
#line 66 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      val = 1
      call reader%read('string1', val=val)
#line 69 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/string1 is set to "test string 1", which cannot be interpreted as an integer.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 69) )
#line 70 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()
   end subroutine test_integer

   !@test
   subroutine test_range_integer()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      integer :: val

      call open_yaml(reader, comm)

      call reader%read('integer', &
                       required_range="[0, 1)", &
                       val=val)

#line 87 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/integer is set to "1", which is out of the required range: [0, 1)', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 87) )
#line 88 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('integer', &
                       required_range="[0, 1]", &
                       val=val)
#line 92 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(1, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 92) )
  if (anyExceptions()) return
#line 93 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('integer', &
                       required_range="[0, 1]", &
                       recommend_range="[0, 1)", &
                       val=val)
#line 98 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(1, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 98) )
  if (anyExceptions()) return
#line 99 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
      ! Note: Warning is logged but doesn't raise exception in pFUnit unless we check logs

      call reader%read('integer', &
                       required_range="[0, 2]", &
                       val=val)
#line 104 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(1, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 104) )
  if (anyExceptions()) return
#line 105 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_range_integer

   !@test
   subroutine test_positive_integer()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      integer :: val

      call open_yaml(reader, comm)

      call reader%read_positive('positive_integer', val=val)
#line 120 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(5, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 120) )
  if (anyExceptions()) return
#line 121 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_positive('negative_integer', val=val)
#line 123 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/negative_integer is set to "-3", which must be positive.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 123) )
#line 124 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_positive('zero_integer', val=val)
#line 126 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/zero_integer is set to "0", which must be positive.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 126) )
#line 127 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_positive_integer

   !@test
   subroutine test_negative_integer()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      integer :: val

      call open_yaml(reader, comm)

      call reader%read_negative('negative_integer', val=val)
#line 142 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(-3, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 142) )
  if (anyExceptions()) return
#line 143 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_negative('positive_integer', val=val)
#line 145 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/positive_integer is set to "5", which must be negative.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 145) )
#line 146 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_negative('zero_integer', val=val)
#line 148 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/zero_integer is set to "0", which must be negative.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 148) )
#line 149 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_negative_integer

   !@test
   subroutine test_nonnegative_integer()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      integer :: val

      call open_yaml(reader, comm)

      call reader%read_nonnegative('positive_integer', val=val)
#line 164 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(5, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 164) )
  if (anyExceptions()) return
#line 165 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_nonnegative('zero_integer', val=val)
#line 167 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 167) )
  if (anyExceptions()) return
#line 168 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_nonnegative('negative_integer', val=val)
#line 170 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/negative_integer is set to "-3", which must be non-negative.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 170) )
#line 171 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_nonnegative_integer

   !@test
   subroutine test_nonpositive_integer()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      integer :: val

      call open_yaml(reader, comm)

      call reader%read_nonpositive('negative_integer', val=val)
#line 186 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(-3, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 186) )
  if (anyExceptions()) return
#line 187 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_nonpositive('zero_integer', val=val)
#line 189 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 189) )
  if (anyExceptions()) return
#line 190 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_nonpositive('positive_integer', val=val)
#line 192 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/positive_integer is set to "5", which must be non-positive.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 192) )
#line 193 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_nonpositive_integer

   !@test
   subroutine test_real()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      real(SP) :: val

      call open_yaml(reader, comm)

      val = 10.0_SP
      call reader%read('integer', val=val)
#line 209 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(1.0_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 209) )
  if (anyExceptions()) return
#line 210 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      val = 10.0_SP
      call reader%read('real', val=val)
#line 213 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0.1_sp, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 213) )
  if (anyExceptions()) return
#line 214 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      val = 1.0_SP
      call reader%read('dummy_int', default="10", val=val)
#line 217 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(10.0_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 217) )
  if (anyExceptions()) return
#line 218 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      val = 1.0_SP
      call reader%read('dummy_real', default="10.5", val=val)
#line 221 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(10.5_sp, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 221) )
  if (anyExceptions()) return
#line 222 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      val = 1.0_SP
      call reader%read('string1', val=val)
#line 225 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/string1 is set to "test string 1", which cannot be interpreted as a real number.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 225) )
#line 226 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()
   end subroutine test_real

   !@test
   subroutine test_range_real()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      real(SP) :: val

      call open_yaml(reader, comm)

      call reader%read('real', &
                       required_range="[0, 0.1)", &
                       val=val)

#line 243 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/real is set to "0.1", which is out of the required range: [0, 0.1)', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 243) )
#line 244 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('real', &
                       required_range="[0, 0.1]", &
                       val=val)
#line 248 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0.1_sp, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 248) )
  if (anyExceptions()) return
#line 249 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('real', &
                       required_range="[0, 0.1]", &
                       recommend_range="[0, 0.1)", &
                       val=val)
#line 254 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0.1_sp, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 254) )
  if (anyExceptions()) return
#line 255 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_range_real

   !@test
   subroutine test_positive_real()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      real(SP) :: val

      call open_yaml(reader, comm)

      call reader%read_positive('positive_real', val=val)
#line 270 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0.5, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 270) )
  if (anyExceptions()) return
#line 271 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_positive('negative_real', val=val)
#line 273 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/negative_real is set to "-0.3", which must be positive.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 273) )
#line 274 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_positive('zero_real', val=val)
#line 276 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/zero_real is set to "0.0", which must be positive.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 276) )
#line 277 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_positive_real

   !@test
   subroutine test_negative_real()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      real(SP) :: val

      call open_yaml(reader, comm)

      call reader%read_negative('negative_real', val=val)
#line 292 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(-0.3_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 292) )
  if (anyExceptions()) return
#line 293 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_negative('positive_real', val=val)
#line 295 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/positive_real is set to "0.5", which must be negative.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 295) )
#line 296 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_negative('zero_real', val=val)
#line 298 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/zero_real is set to "0.0", which must be negative.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 298) )
#line 299 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_negative_real

   !@test
   subroutine test_nonnegative_real()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      real(SP) :: val

      call open_yaml(reader, comm)

      call reader%read_nonnegative('positive_real', val=val)
#line 314 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0.5_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 314) )
  if (anyExceptions()) return
#line 315 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_nonnegative('zero_real', val=val)
#line 317 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0.0_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 317) )
  if (anyExceptions()) return
#line 318 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_nonnegative('negative_real', val=val)
#line 320 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/negative_real is set to "-0.3", which must be non-negative.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 320) )
#line 321 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_nonnegative_real

   !@test
   subroutine test_nonpositive_real()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      real(SP) :: val

      call open_yaml(reader, comm)

      call reader%read_nonpositive('negative_real', val=val)
#line 336 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(-0.3_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 336) )
  if (anyExceptions()) return
#line 337 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_nonpositive('zero_real', val=val)
#line 339 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0.0_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 339) )
  if (anyExceptions()) return
#line 340 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_nonpositive('positive_real', val=val)
#line 342 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/positive_real is set to "0.5", which must be non-positive.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 342) )
#line 343 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_nonpositive_real

   !@test
   subroutine test_time()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      real(SP) :: val

      call open_yaml(reader, comm)

      call reader%read_time('time_simple', val=val)
#line 358 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(100.0_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 358) )
  if (anyExceptions()) return
#line 359 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
      !
      call reader%read_time('time_mins', val=val)
#line 361 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(60.0_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 361) )
  if (anyExceptions()) return
#line 362 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_time('time_hrs', val=val)
#line 364 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(3600.0_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 364) )
  if (anyExceptions()) return
#line 365 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read_time('time_hertz', val=val)
#line 367 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0.1_SP, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 367) )
  if (anyExceptions()) return
#line 368 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_time

   !@test
   subroutine test_logical()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      logical :: val

      call open_yaml(reader, comm)

      call reader%read('logical_true', val=val)
#line 383 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 383) )
  if (anyExceptions()) return
#line 384 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_True', val=val)
#line 386 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 386) )
  if (anyExceptions()) return
#line 387 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_TRUE', val=val)
#line 389 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 389) )
  if (anyExceptions()) return
#line 390 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_on', val=val)
#line 392 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 392) )
  if (anyExceptions()) return
#line 393 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_On', val=val)
#line 395 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 395) )
  if (anyExceptions()) return
#line 396 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_ON', val=val)
#line 398 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 398) )
  if (anyExceptions()) return
#line 399 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_y', val=val)
#line 401 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 401) )
  if (anyExceptions()) return
#line 402 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_Y', val=val)
#line 404 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 404) )
  if (anyExceptions()) return
#line 405 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_yes', val=val)
#line 407 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 407) )
  if (anyExceptions()) return
#line 408 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_Yes', val=val)
#line 410 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 410) )
  if (anyExceptions()) return
#line 411 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_YES', val=val)
#line 413 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 413) )
  if (anyExceptions()) return
#line 414 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      ! False Tests
      call reader%read('logical_false', val=val)
#line 417 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 417) )
  if (anyExceptions()) return
#line 418 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_False', val=val)
#line 420 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 420) )
  if (anyExceptions()) return
#line 421 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_False', val=val)
#line 423 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 423) )
  if (anyExceptions()) return
#line 424 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_off', val=val)
#line 426 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 426) )
  if (anyExceptions()) return
#line 427 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_Off', val=val)
#line 429 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 429) )
  if (anyExceptions()) return
#line 430 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_OFF', val=val)
#line 432 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 432) )
  if (anyExceptions()) return
#line 433 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_n', val=val)
#line 435 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 435) )
  if (anyExceptions()) return
#line 436 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_N', val=val)
#line 438 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 438) )
  if (anyExceptions()) return
#line 439 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_no', val=val)
#line 441 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 441) )
  if (anyExceptions()) return
#line 442 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_No', val=val)
#line 444 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 444) )
  if (anyExceptions()) return
#line 445 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_NO', val=val)
#line 447 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(.false., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 447) )
  if (anyExceptions()) return
#line 448 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_t', val=val)
#line 450 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/logical_t is set to "t", which cannot be interpreted as a Boolean value.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 450) )
#line 451 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_T', val=val)
#line 453 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/logical_T is set to "T", which cannot be interpreted as a Boolean value.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 453) )
#line 454 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_f', val=val)
#line 456 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/logical_f is set to "f", which cannot be interpreted as a Boolean value.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 456) )
#line 457 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('logical_F', val=val)
#line 459 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/logical_F is set to "F", which cannot be interpreted as a Boolean value.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 459) )
#line 460 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_logical

   !@test
   subroutine test_string()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      character(:), ALLOCATABLE :: val, val2

      call open_yaml(reader, comm)

      call reader%read('string1', val=val)
#line 475 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual('test string 1', val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 475) )
  if (anyExceptions()) return
#line 476 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('dummy_string', default="dummy string", val=val)
#line 478 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual('dummy string', val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 478) )
  if (anyExceptions()) return
#line 479 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('string2', val=val)
#line 481 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual('test string 2', val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 481) )
  if (anyExceptions()) return
#line 482 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_string

   !@test
   subroutine test_enum()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      character(:), allocatable :: val
      character(len=6), dimension(3) :: words

      data words/'item1', 'item2', 'item3'/
      call open_yaml(reader, comm)

      call reader%read('enum1', words, val=val)
#line 499 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual('item1', val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 499) )
  if (anyExceptions()) return
#line 500 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('dummy_enum', words, default='item3', val=val)
#line 502 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual('item3', val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 502) )
  if (anyExceptions()) return
#line 503 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('enum2', words, val=val)
#line 505 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual('item2', val, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 505) )
  if (anyExceptions()) return
#line 506 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call reader%read('enum_error', words, val=val)
#line 508 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/enum_error is set to "item5", which is not in the allowable list of values. Valid values: item1, item2, & item3.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 508) )
#line 509 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_enum

   !@test
   subroutine test_path()

      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      character(:), allocatable :: test !, file_path
      type(type_path):: file_path, val
      integer :: unit, iostat
      call open_yaml(reader, comm)

      call reader%read('invalid_file_path', val=val)
#line 525 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised('/invalid_file_path is set to "invalid_file_path.txt", which is not a valid file path.', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 525) )
#line 526 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      file_path = type_path('valid_file_path.txt')
      call file_path%touch()
      call reader%read('valid_file_path', val=val)
#line 530 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(file_path%root, val%root, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 530) )
  if (anyExceptions()) return
#line 531 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
      call file_path%remove()

      call dummy_fix_lint()

   end subroutine test_path

   !@test
   subroutine test_dict()

      type(type_yaml_reader) :: reader, child
      type(type_comm) :: comm
      integer :: val_int
      real(SP) :: val_real
      logical :: is_empty

      call open_yaml(reader, comm)

      ! Test existing dictionary
      child = reader%cast_dictionary('dict')
      call child%read('positive_integer', val=val_int)
#line 551 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(5, val_int, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 551) )
  if (anyExceptions()) return
#line 552 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
      call child%read('positive_real', val=val_real)
#line 553 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertEqual(0.5_SP, val_real, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 553) )
  if (anyExceptions()) return
#line 554 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      ! Test missing dictionary (optional)
      child = reader%cast_dictionary('missing_dict', is_empty=is_empty)
#line 557 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertTrue(is_empty, &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 557) )
  if (anyExceptions()) return
#line 558 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      ! Test missing dictionary (required)
      child = reader%cast_dictionary('missing_dict')
#line 561 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"
  call assertExceptionRaised(' does not contain key "missing_dict".', &
 & location=SourceLocation( &
 & 'test_yaml_input_serial.pf', &
 & 561) )
#line 562 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_serial.pf"

      call dummy_fix_lint()

   end subroutine test_dict

end module test_yaml_input_serial

module Wraptest_yaml_input_serial
   use FUnit
   use test_yaml_input_serial
   implicit none
   private

contains


end module Wraptest_yaml_input_serial

function test_yaml_input_serial_suite() result(suite)
   use FUnit
   use test_yaml_input_serial
   use Wraptest_yaml_input_serial
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   suite = TestSuite('test_yaml_input_serial_suite')

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_range_integer', &
      test_range_integer))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_positive_integer', &
      test_positive_integer))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_negative_integer', &
      test_negative_integer))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_nonnegative_integer', &
      test_nonnegative_integer))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_nonpositive_integer', &
      test_nonpositive_integer))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_real', &
      test_real))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_range_real', &
      test_range_real))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_positive_real', &
      test_positive_real))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_negative_real', &
      test_negative_real))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_nonnegative_real', &
      test_nonnegative_real))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_nonpositive_real', &
      test_nonpositive_real))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_time', &
      test_time))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_logical', &
      test_logical))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_string', &
      test_string))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_enum', &
      test_enum))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_path', &
      test_path))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_dict', &
      test_dict))
   call suite%addTest(t)


end function test_yaml_input_serial_suite

