! allow(E001)
module test_logging

   use pFUnit
   use core_log_io_mod
   use core_constants_mod, only: LABEL_SIZE
   implicit none(external)

contains

   !> Helper to validate that a file contains a specific message
   subroutine assert_file_contains(log_path, expected_text)
      character(*), intent(in) :: log_path, expected_text
      character(len=256) :: line
      integer :: u, iostatus
      logical :: found

      open (newunit=u, file=trim(log_path), status='old', action='read', iostat=iostatus)
      found = .false.
      do
         read (u, '(A)', iostat=iostatus) line
         if (iostatus /= 0) exit
         if (index(line, trim(expected_text)) > 0) then
            found = .true.
            exit
         end if
      end do
      close (u)
#line 29 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
  call assertTrue(found, "Could not find '"//trim(expected_text)//"' in "//trim(log_path), &
 & location=SourceLocation( &
 & 'test_logging.pf', &
 & 29) )
  if (anyExceptions()) return
#line 30 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
   end subroutine assert_file_contains

   !@test
   subroutine test_constructor()
      type(type_log_writer) :: log
      log = new_log_writer(label='TEST', is_io_node=.true.)
#line 36 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
  call assertTrue(.true., &
 & location=SourceLocation( &
 & 'test_logging.pf', &
 & 36) )
  if (anyExceptions()) return
#line 37 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
   end subroutine test_constructor

   !@test
   subroutine test_constructor_with_init()
      type(type_log_writer) :: log
      character(len=20) :: log_path
      log_path = "test_init.log"
      log = new_log_writer(label='INIT', is_io_node=.true., path=log_path, &
                           std_err_threshold=0, std_out_threshold=0, logfile_threshold=0)
#line 46 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
  call assertTrue(.true., &
 & location=SourceLocation( &
 & 'test_logging.pf', &
 & 46) )
  if (anyExceptions()) return
#line 47 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
   end subroutine test_constructor_with_init

   !@test
   subroutine test_logging_methods()
      type(type_log_writer) :: log
      character(len=20) :: log_path

      log_path = "test_methods.log"
      log = new_log_writer(label='LOG', is_io_node=.true., path=log_path, logfile_threshold=0)

      call log%debug("Debug message")
      call log%info("Info message")
      call log%warning("Warning message")
      call log%finalize()

      call assert_file_contains(log_path, "DEBUG")
      call assert_file_contains(log_path, "INFO")
      call assert_file_contains(log_path, "WARN")
   end subroutine test_logging_methods

   !@test
   subroutine test_exit_on_error()
      type(type_log_writer) :: log
      log = new_log_writer(label='ERR', is_io_node=.true.)
      call log%exit_on_error("Testing exit on error")
#line 72 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
  call assertExceptionRaised("Testing exit on error", &
 & location=SourceLocation( &
 & 'test_logging.pf', &
 & 72) )
#line 73 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
   end subroutine test_exit_on_error

   !@test
   subroutine test_exit_on_fatal()
      type(type_log_writer) :: log
      log = new_log_writer(label='FATAL', is_io_node=.true.)
      call log%exit_on_fatal("Testing exit on fatal")
#line 80 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
  call assertExceptionRaised("Testing exit on fatal", &
 & location=SourceLocation( &
 & 'test_logging.pf', &
 & 80) )
#line 81 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
   end subroutine test_exit_on_fatal

   !@test
   subroutine test_non_io_node()
      type(type_log_writer) :: log
      log = new_log_writer(label='SILENT', is_io_node=.false.)
      call log%debug("Should be silent")
      call log%exit_on_error("Should NOT throw")
      call log%exit_on_fatal("Should NOT throw")
#line 90 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
  call assertTrue(.true., &
 & location=SourceLocation( &
 & 'test_logging.pf', &
 & 90) )
  if (anyExceptions()) return
#line 91 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
   end subroutine test_non_io_node

   !@test
   subroutine test_format_log_line()
      type(type_log_writer) :: log
      character(len=256) :: formatted
      character(len=19) :: timestamp

      log = new_log_writer(label='TEST', is_io_node=.true.)
      timestamp = "2026-04-29 10:00:00"

      ! Use correct function call syntax for a module procedure
      formatted = format_log_line(log, timestamp, "INFO", "Message")

      ! Verify exact format: YYYY-MM-DD HH:MM:SS [LABEL] PREFIX: MSG
#line 106 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
  call assertEqual("2026-04-29 10:00:00 [TEST] INFO: Message", trim(formatted), &
 & location=SourceLocation( &
 & 'test_logging.pf', &
 & 106) )
  if (anyExceptions()) return
#line 107 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_logging.pf"
   end subroutine test_format_log_line

end module test_logging

module Wraptest_logging
   use FUnit
   use test_logging
   implicit none
   private

contains


end module Wraptest_logging

function test_logging_suite() result(suite)
   use FUnit
   use test_logging
   use Wraptest_logging
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   suite = TestSuite('test_logging_suite')

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_constructor', &
      test_constructor))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_constructor_with_init', &
      test_constructor_with_init))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_logging_methods', &
      test_logging_methods))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_exit_on_error', &
      test_exit_on_error))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_exit_on_fatal', &
      test_exit_on_fatal))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_non_io_node', &
      test_non_io_node))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=TestMethod('test_format_log_line', &
      test_format_log_line))
   call suite%addTest(t)


end function test_logging_suite

