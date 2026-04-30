! allow(E001)
module test_path_mod

   use pFUnit
   use core_path_mod
   implicit none(external)

contains

   subroutine dummy_fix_lint()
      ! Dummy function to fix linter, seems to reset @ errors
   end subroutine dummy_fix_lint

   !@test
   subroutine test_path_lifecycle()
      type(type_path) :: sandbox
      type(type_path) :: file_in_sandbox
      integer :: stat

      sandbox = type_path("sandbox_dir")

      ! 1. Force Clean Setup: Ensure it does not exist using system command for robust cleanup
      call execute_command_line("rm -rf sandbox_dir", exitstat=stat)
#line 24 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertFalse(sandbox%exists(), "Sandbox should not exist at start", &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 24) )
  if (anyExceptions()) return
#line 25 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"

      ! 2. Create directory
#line 27 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertTrue(sandbox%mkdir(), "mkdir failed", &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 27) )
  if (anyExceptions()) return
#line 28 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
#line 28 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertTrue(sandbox%exists(), "Dir should exist after mkdir", &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 28) )
  if (anyExceptions()) return
#line 29 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
#line 29 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertTrue(sandbox%is_dir(), "Dir path should report is_dir=true", &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 29) )
  if (anyExceptions()) return
#line 30 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"

      ! 3. Create file inside
      file_in_sandbox = sandbox%join("test.txt")
      call file_in_sandbox%touch()
#line 34 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertTrue(file_in_sandbox%exists(), "File should exist after touch", &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 34) )
  if (anyExceptions()) return
#line 35 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
#line 35 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertTrue(file_in_sandbox%is_file(), "File path should report is_file=true", &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 35) )
  if (anyExceptions()) return
#line 36 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"

      ! 4. Final Cleanup: Verify teardown
      call file_in_sandbox%remove()
      call sandbox%remove()

      ! Verify final state
#line 42 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertFalse(sandbox%exists(), "Sandbox should be gone after remove", &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 42) )
  if (anyExceptions()) return
#line 43 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"

   end subroutine test_path_lifecycle

   !@test
   subroutine test_path_utils()
      type(type_path) :: p
      type(type_path) :: joined
      p = type_path("dir")
      joined = p%join("file.txt")
#line 52 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertEqual("dir/file.txt", trim(joined%root), &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 52) )
  if (anyExceptions()) return
#line 53 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"

      p = type_path("/path/to/file.txt")
#line 55 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertEqual("file.txt", trim(p%get_filename()), &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 55) )
  if (anyExceptions()) return
#line 56 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
#line 56 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertEqual("/path/to", trim(p%get_parent()), &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 56) )
  if (anyExceptions()) return
#line 57 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"

      p = type_path("data.yaml")
#line 59 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertEqual("yaml", trim(p%get_suffix()), &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 59) )
  if (anyExceptions()) return
#line 60 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
#line 60 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertTrue(p%has_suffix("yaml"), &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 60) )
  if (anyExceptions()) return
#line 61 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
#line 61 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertFalse(p%has_suffix("txt"), &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 61) )
  if (anyExceptions()) return
#line 62 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"

      p = type_path("data")
      call p%add_suffix('yaml')
#line 65 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
  call assertEqual("data.yaml", trim(p%root), &
 & location=SourceLocation( &
 & 'test_path_mod.pf', &
 & 65) )
  if (anyExceptions()) return
#line 66 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_path_mod.pf"
   end subroutine test_path_utils

end module test_path_mod

module Wraptest_path_mod
   use FUnit
   use test_path_mod
   implicit none
   private

contains


end module Wraptest_path_mod

function test_path_mod_suite() result(suite)
   use FUnit
   use test_path_mod
   use Wraptest_path_mod
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   suite = TestSuite('test_path_mod_suite')

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_path_lifecycle', &
      test_path_lifecycle))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_path_utils', &
      test_path_utils))
   call suite%addTest(t)


end function test_path_mod_suite

