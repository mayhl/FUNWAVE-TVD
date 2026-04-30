
! allow(E001)
module test_yaml_input_parallel

   use mpi_f08
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

   subroutine open_yaml(this, reader, comm)

      class(MpiTestMethod), intent(inout) :: this
      type(type_yaml_reader), intent(inout) :: reader
      type(type_comm), intent(inout) :: comm
      type(MPI_Comm) :: mcomm
      character(:), allocatable :: yaml_path
      character(:), allocatable :: log_path
      type(type_log_writer) :: log_dummy

      log_path = "./parallel.log"
      yaml_path = "test.yaml"

      mcomm = this%getMpiCommunicator()
      comm = new_comm(io_rank_id=0, comm_id=mcomm)

      log_dummy = new_log_writer('test', comm%is_io_node(), log_path, 0, 0, 100)

      call reader%init(yaml_path, comm)

   end subroutine open_yaml

   !@test(npes=[2])
   subroutine test_integer_broadcast(this)

      class(MpiTestMethod), intent(inout) :: this
      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      integer :: val

      call open_yaml(this, reader, comm)

      call reader%read_integer('integer', val=val)
#line 57 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_parallel.pf"
  call assertEqual(1, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_parallel.pf', &
 & 57) )
  if (anyExceptions()) return
#line 58 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_parallel.pf"

      call dummy_fix_lint()
   end subroutine test_integer_broadcast

   !@test(npes=[2])
   subroutine test_real_broadcast(this)

      class(MpiTestMethod), intent(inout) :: this
      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      real(SP) :: val

      call open_yaml(this, reader, comm)

      call reader%read_real('real', val=val)
#line 73 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_parallel.pf"
  call assertEqual(0.1_sp, val, &
 & location=SourceLocation( &
 & 'test_yaml_input_parallel.pf', &
 & 73) )
  if (anyExceptions()) return
#line 74 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_parallel.pf"

      call dummy_fix_lint()
   end subroutine test_real_broadcast

   !@test(npes=[2])
   subroutine test_logical_broadcast(this)

      class(MpiTestMethod), intent(inout) :: this
      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      logical :: val

      call open_yaml(this, reader, comm)

      call reader%read_logical('logical_true', val=val)
#line 89 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_parallel.pf"
  call assertEqual(.true., val, &
 & location=SourceLocation( &
 & 'test_yaml_input_parallel.pf', &
 & 89) )
  if (anyExceptions()) return
#line 90 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_parallel.pf"

      call dummy_fix_lint()
   end subroutine test_logical_broadcast

   !@test(npes=[2])
   subroutine test_string_broadcast(this)

      class(MpiTestMethod), intent(inout) :: this
      type(type_yaml_reader) :: reader
      type(type_comm) :: comm
      character(:), allocatable :: val

      call open_yaml(this, reader, comm)

      call reader%read_string('string1', val=val)
#line 105 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_parallel.pf"
  call assertEqual('test string 1', val, &
 & location=SourceLocation( &
 & 'test_yaml_input_parallel.pf', &
 & 105) )
  if (anyExceptions()) return
#line 106 "/Users/rdchlmyl/repos/mayhlFUNWAVE/test/core/test_yaml_input_parallel.pf"

      call dummy_fix_lint()
   end subroutine test_string_broadcast

end module test_yaml_input_parallel

module Wraptest_yaml_input_parallel
   use FUnit
   use test_yaml_input_parallel
   implicit none
   private

contains


end module Wraptest_yaml_input_parallel

function test_yaml_input_parallel_suite() result(suite)
   use FUnit
   use test_yaml_input_parallel
   use Wraptest_yaml_input_parallel
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   suite = TestSuite('test_yaml_input_parallel_suite')

   if(allocated(t)) deallocate(t)
   allocate(t, source=MpiTestMethod('test_integer_broadcast', &
      test_integer_broadcast, 2))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=MpiTestMethod('test_real_broadcast', &
      test_real_broadcast, 2))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=MpiTestMethod('test_logical_broadcast', &
      test_logical_broadcast, 2))
   call suite%addTest(t)

   if(allocated(t)) deallocate(t)
   allocate(t, source=MpiTestMethod('test_string_broadcast', &
      test_string_broadcast, 2))
   call suite%addTest(t)


end function test_yaml_input_parallel_suite

