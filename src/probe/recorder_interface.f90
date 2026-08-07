module probe_mod
   use iso_fortran_env, only: int32, real32, real64
   implicit none

   interface dump_state
      module procedure dump_state_r4, dump_state_r8, dump_state_i4
      module procedure dump_state_r4_a1, dump_state_r8_a1, dump_state_i4_a1
      module procedure dump_state_r4_a2, dump_state_r8_a2, dump_state_i4_a2
   end interface

   interface reset_state
      module procedure reset_state_impl
   end interface

contains

   subroutine reset_state_impl()
      integer :: unit_num = 99
      open (unit=unit_num, file="dump.bin", status="replace")
      close (unit_num)
   end subroutine reset_state_impl

   subroutine dump_state_r4(var, name)
      real(real32), intent(in) :: var
      character(len=*), intent(in) :: name
      call write_data_scalar(var, name)
   end subroutine dump_state_r4

   subroutine dump_state_r8(var, name)
      real(real64), intent(in) :: var
      character(len=*), intent(in) :: name
      call write_data_scalar(var, name)
   end subroutine dump_state_r8

   subroutine dump_state_i4(var, name)
      integer(int32), intent(in) :: var
      character(len=*), intent(in) :: name
      call write_data_scalar(var, name)
   end subroutine dump_state_i4

   subroutine dump_state_r4_a1(var, name)
      real(real32), intent(in) :: var(:)
      character(len=*), intent(in) :: name
      call write_data_arr1d(var, name)
   end subroutine dump_state_r4_a1

   subroutine dump_state_r8_a1(var, name)
      real(real64), intent(in) :: var(:)
      character(len=*), intent(in) :: name
      call write_data_arr1d(var, name)
   end subroutine dump_state_r8_a1

   subroutine dump_state_i4_a1(var, name)
      integer(int32), intent(in) :: var(:)
      character(len=*), intent(in) :: name
      call write_data_arr1d(var, name)
   end subroutine dump_state_i4_a1

   subroutine dump_state_r4_a2(var, name)
      real(real32), intent(in) :: var(:, :)
      character(len=*), intent(in) :: name
      call write_data_arr2d(var, name)
   end subroutine dump_state_r4_a2

   subroutine dump_state_r8_a2(var, name)
      real(real64), intent(in) :: var(:, :)
      character(len=*), intent(in) :: name
      call write_data_arr2d(var, name)
   end subroutine dump_state_r8_a2

   subroutine dump_state_i4_a2(var, name)
      integer(int32), intent(in) :: var(:, :)
      character(len=*), intent(in) :: name
      call write_data_arr2d(var, name)
   end subroutine dump_state_i4_a2

   subroutine write_data_scalar(var, name)
      class(*), intent(in) :: var
      character(len=*), intent(in) :: name
      integer :: unit_num = 99
      open (unit=unit_num, file="dump.bin", status="unknown", position="append", form="unformatted")
      write (unit_num) name
      select type (var)
      type is (real(real32)); write (unit_num) var
      type is (real(real64)); write (unit_num) var
      type is (integer(int32)); write (unit_num) var
      end select
      close (unit_num)
   end subroutine write_data_scalar

   subroutine write_data_arr1d(var, name)
      class(*), intent(in) :: var(:)
      character(len=*), intent(in) :: name
      integer :: unit_num = 99
      open (unit=unit_num, file="dump.bin", status="unknown", position="append", form="unformatted")
      write (unit_num) name
      select type (var)
      type is (real(real32)); write (unit_num) var
      type is (real(real64)); write (unit_num) var
      type is (integer(int32)); write (unit_num) var
      end select
      close (unit_num)
   end subroutine write_data_arr1d

   subroutine write_data_arr2d(var, name)
      class(*), intent(in) :: var(:, :)
      character(len=*), intent(in) :: name
      integer :: unit_num = 99
      open (unit=unit_num, file="dump.bin", status="unknown", position="append", form="unformatted")
      write (unit_num) name
      select type (var)
      type is (real(real32)); write (unit_num) var
      type is (real(real64)); write (unit_num) var
      type is (integer(int32)); write (unit_num) var
      end select
      close (unit_num)
   end subroutine write_data_arr2d

end module probe_mod
