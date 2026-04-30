! allow(E001)
module test_range_parser

   use pFUnit
   use core_constants_mod, only: SP
   use core_range_parse_mod
   implicit none(external)

contains

   !@test
   subroutine test_integer_range_parser()
      class(type_integer_range), allocatable :: range
      character(len=:), allocatable :: str

      str = "[2,3,1]"
      range = type_integer_range(str)
#line 18 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 18) )
  if (anyExceptions()) return
#line 19 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "[,]"
      range = type_integer_range(str)
#line 22 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 22) )
  if (anyExceptions()) return
#line 23 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 23 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 23) )
  if (anyExceptions()) return
#line 24 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 24 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 24) )
  if (anyExceptions()) return
#line 25 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "[,1]"
      range = type_integer_range(str)
#line 28 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 28) )
  if (anyExceptions()) return
#line 29 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 29 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 29) )
  if (anyExceptions()) return
#line 30 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 30 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 30) )
  if (anyExceptions()) return
#line 31 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 31 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("1", range%get_upper(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 31) )
  if (anyExceptions()) return
#line 32 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 32 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(1, range%upper, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 32) )
  if (anyExceptions()) return
#line 33 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "[3,]"
      range = type_integer_range(str)
#line 36 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 36) )
  if (anyExceptions()) return
#line 37 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 37 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 37) )
  if (anyExceptions()) return
#line 38 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 38 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 38) )
  if (anyExceptions()) return
#line 39 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 39 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("3", range%get_lower(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 39) )
  if (anyExceptions()) return
#line 40 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 40 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(3, range%lower, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 40) )
  if (anyExceptions()) return
#line 41 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "[2,3]"
      range = type_integer_range(str)
#line 44 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 44) )
  if (anyExceptions()) return
#line 45 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 45 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 45) )
  if (anyExceptions()) return
#line 46 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 46 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 46) )
  if (anyExceptions()) return
#line 47 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 47 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("2", range%get_lower(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 47) )
  if (anyExceptions()) return
#line 48 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 48 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(2, range%lower, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 48) )
  if (anyExceptions()) return
#line 49 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 49 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("3", range%get_upper(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 49) )
  if (anyExceptions()) return
#line 50 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 50 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(3, range%upper, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 50) )
  if (anyExceptions()) return
#line 51 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "   (   -5   ,   -1   ]    "
      range = type_integer_range(str)
#line 54 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 54) )
  if (anyExceptions()) return
#line 55 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 55 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 55) )
  if (anyExceptions()) return
#line 56 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 56 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 56) )
  if (anyExceptions()) return
#line 57 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 57 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("-5", range%get_lower(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 57) )
  if (anyExceptions()) return
#line 58 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 58 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(-5, range%lower, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 58) )
  if (anyExceptions()) return
#line 59 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 59 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("-1", range%get_upper(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 59) )
  if (anyExceptions()) return
#line 60 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 60 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(-1, range%upper, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 60) )
  if (anyExceptions()) return
#line 61 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "[1, 10.11)"
      range = type_integer_range(str)
#line 64 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 64) )
  if (anyExceptions()) return
#line 65 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "(a, 5.1)"
      range = type_integer_range(str)
#line 68 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 68) )
  if (anyExceptions()) return
#line 69 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "(-1,-5)"
      range = type_integer_range(str)
#line 72 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 72) )
  if (anyExceptions()) return
#line 73 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
   end subroutine test_integer_range_parser

   !@test
   subroutine test_real_range_parser()
      class(type_real_range), allocatable :: range
      character(len=:), allocatable :: str

      str = "[2.2,0.3,1.1]"
      range = type_real_range(str)
#line 82 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 82) )
  if (anyExceptions()) return
#line 83 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "[,]"
      range = type_real_range(str)
#line 86 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 86) )
  if (anyExceptions()) return
#line 87 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 87 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 87) )
  if (anyExceptions()) return
#line 88 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 88 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 88) )
  if (anyExceptions()) return
#line 89 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "[,12.2]"
      range = type_real_range(str)
#line 92 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 92) )
  if (anyExceptions()) return
#line 93 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 93 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 93) )
  if (anyExceptions()) return
#line 94 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 94 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 94) )
  if (anyExceptions()) return
#line 95 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 95 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("12.2", range%get_upper(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 95) )
  if (anyExceptions()) return
#line 96 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 96 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(12.2_SP, range%upper, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 96) )
  if (anyExceptions()) return
#line 97 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "[3.2,]"
      range = type_real_range(str)
#line 100 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 100) )
  if (anyExceptions()) return
#line 101 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 101 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 101) )
  if (anyExceptions()) return
#line 102 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 102 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 102) )
  if (anyExceptions()) return
#line 103 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 103 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("3.2", range%get_lower(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 103) )
  if (anyExceptions()) return
#line 104 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 104 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(3.2_SP, range%lower, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 104) )
  if (anyExceptions()) return
#line 105 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "[1.2,3.33]"
      range = type_real_range(str)
#line 108 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 108) )
  if (anyExceptions()) return
#line 109 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 109 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 109) )
  if (anyExceptions()) return
#line 110 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 110 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 110) )
  if (anyExceptions()) return
#line 111 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 111 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("1.2", range%get_lower(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 111) )
  if (anyExceptions()) return
#line 112 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 112 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(1.2_SP, range%lower, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 112) )
  if (anyExceptions()) return
#line 113 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 113 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("3.33", range%get_upper(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 113) )
  if (anyExceptions()) return
#line 114 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 114 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(3.33_SP, range%upper, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 114) )
  if (anyExceptions()) return
#line 115 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "   (   -4   ,   -1.2   ]    "
      range = type_real_range(str)
#line 118 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 118) )
  if (anyExceptions()) return
#line 119 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 119 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 119) )
  if (anyExceptions()) return
#line 120 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 120 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 120) )
  if (anyExceptions()) return
#line 121 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 121 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("-4", range%get_lower(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 121) )
  if (anyExceptions()) return
#line 122 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 122 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(-4.0_SP, range%lower, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 122) )
  if (anyExceptions()) return
#line 123 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 123 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("-1.2", range%get_upper(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 123) )
  if (anyExceptions()) return
#line 124 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 124 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(-1.2_SP, range%upper, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 124) )
  if (anyExceptions()) return
#line 125 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "(a, 5.1)"
      range = type_real_range(str)
#line 128 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 128) )
  if (anyExceptions()) return
#line 129 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "(-1, -4.0)"
      range = type_real_range(str)
#line 132 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 132) )
  if (anyExceptions()) return
#line 133 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 133 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 133) )
  if (anyExceptions()) return
#line 134 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 134 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 134) )
  if (anyExceptions()) return
#line 135 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      str = "(-1,)"
      range = type_real_range(str)
#line 138 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(range%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 138) )
  if (anyExceptions()) return
#line 139 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 139 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_inc_low(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 139) )
  if (anyExceptions()) return
#line 140 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 140 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(range%is_inc_upp(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 140) )
  if (anyExceptions()) return
#line 141 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 141 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual("-1", range%get_lower(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 141) )
  if (anyExceptions()) return
#line 142 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 142 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertEqual(-1.0_SP, range%lower, &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 142) )
  if (anyExceptions()) return
#line 143 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
   end subroutine test_real_range_parser

   !@test
   subroutine test_range_in_range()
      type(type_integer_range) :: irange
      type(type_real_range) :: rrange

      ! Integer range [2, 5)
      irange = type_integer_range("[2, 5)")
#line 152 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 152) )
  if (anyExceptions()) return
#line 153 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 153 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(irange%in_range(1), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 153) )
  if (anyExceptions()) return
#line 154 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 154 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%in_range(2), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 154) )
  if (anyExceptions()) return
#line 155 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 155 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%in_range(3), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 155) )
  if (anyExceptions()) return
#line 156 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 156 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%in_range(4), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 156) )
  if (anyExceptions()) return
#line 157 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 157 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(irange%in_range(5), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 157) )
  if (anyExceptions()) return
#line 158 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      ! Integer range (2, 5]
      irange = type_integer_range("(2, 5]")
#line 161 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 161) )
  if (anyExceptions()) return
#line 162 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 162 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(irange%in_range(2), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 162) )
  if (anyExceptions()) return
#line 163 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 163 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%in_range(3), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 163) )
  if (anyExceptions()) return
#line 164 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 164 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%in_range(5), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 164) )
  if (anyExceptions()) return
#line 165 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 165 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(irange%in_range(6), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 165) )
  if (anyExceptions()) return
#line 166 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      ! Real range [1.0, 2.0]
      rrange = type_real_range("[1.0, 2.0]")
#line 169 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 169) )
  if (anyExceptions()) return
#line 170 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 170 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(rrange%in_range(0.9_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 170) )
  if (anyExceptions()) return
#line 171 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 171 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%in_range(1.0_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 171) )
  if (anyExceptions()) return
#line 172 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 172 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%in_range(1.5_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 172) )
  if (anyExceptions()) return
#line 173 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 173 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%in_range(2.0_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 173) )
  if (anyExceptions()) return
#line 174 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 174 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(rrange%in_range(2.1_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 174) )
  if (anyExceptions()) return
#line 175 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      ! Real range (1.0, 2.0)
      rrange = type_real_range("(1.0, 2.0)")
#line 178 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 178) )
  if (anyExceptions()) return
#line 179 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 179 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(rrange%in_range(1.0_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 179) )
  if (anyExceptions()) return
#line 180 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 180 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%in_range(1.1_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 180) )
  if (anyExceptions()) return
#line 181 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 181 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%in_range(1.9_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 181) )
  if (anyExceptions()) return
#line 182 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 182 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(rrange%in_range(2.0_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 182) )
  if (anyExceptions()) return
#line 183 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      ! Open-ended range [0, )
      irange = type_integer_range("[0, )")
#line 186 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 186) )
  if (anyExceptions()) return
#line 187 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 187 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(irange%in_range(-1), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 187) )
  if (anyExceptions()) return
#line 188 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 188 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%in_range(0), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 188) )
  if (anyExceptions()) return
#line 189 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 189 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(irange%in_range(1000), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 189) )
  if (anyExceptions()) return
#line 190 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"

      ! Open-ended range ( , 0]
      rrange = type_real_range("( , 0.0]")
#line 193 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%is_valid(), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 193) )
  if (anyExceptions()) return
#line 194 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 194 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%in_range(-100.0_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 194) )
  if (anyExceptions()) return
#line 195 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 195 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertTrue(rrange%in_range(0.0_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 195) )
  if (anyExceptions()) return
#line 196 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
#line 196 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
  call assertFalse(rrange%in_range(0.0001_SP), &
 & location=SourceLocation( &
 & 'test_range_parser.pf', &
 & 196) )
  if (anyExceptions()) return
#line 197 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_range_parser.pf"
   end subroutine test_range_in_range

end module test_range_parser

module Wraptest_range_parser
   use FUnit
   use test_range_parser
   implicit none
   private

contains


end module Wraptest_range_parser

function test_range_parser_suite() result(suite)
   use FUnit
   use test_range_parser
   use Wraptest_range_parser
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   suite = TestSuite('test_range_parser_suite')

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_integer_range_parser', &
      test_integer_range_parser))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_real_range_parser', &
      test_real_range_parser))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_range_in_range', &
      test_range_in_range))
   call suite%addTest(t)


end function test_range_parser_suite

