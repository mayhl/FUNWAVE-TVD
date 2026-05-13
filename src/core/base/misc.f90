module core_misc_mod

   use core_constants_mod, only: SP

   implicit none(external)
   public random2
contains

   function random2() result(val)
      real(SP) :: val

! # if defined (INTEL)
!       val = rand()
! # elif defined (CRAY)
!       val = rand()
! # else
      call random_number(val)
      !# endif

   end function random2

   elemental subroutine str2int(str, int, stat, fmt, is_empty)
      character(len=*), intent(in) :: str
      integer, intent(out) :: int
      integer, intent(out) :: stat
      character(*), intent(in), optional :: fmt
      logical, intent(out), optional :: is_empty

      character(:), allocatable :: fmt_

      if (present(fmt)) then
         fmt_ = fmt
      else
         fmt_ = "(I20)"
      end if

      if (present(is_empty)) then
         is_empty = len(trim(str)) == 0
      end if

      ! Reject strings with non-integer characters before reading; ifx's I20
      ! silently accepts "10.5" as 10 (partial parse) rather than setting iostat.
      if (verify(trim(adjustl(str)), ' +-0123456789') > 0) then
         stat = 1
         return
      end if

      read (str, fmt_, iostat=stat) int

   end subroutine str2int

   elemental subroutine str2real(str, flt, stat, fmt, is_empty)
      character(len=*), intent(in) :: str
      real(sp), intent(out) :: flt
      integer, intent(out) :: stat
      character(*), intent(in), optional :: fmt
      logical, intent(out), optional :: is_empty

      if (present(is_empty)) then
         is_empty = len(trim(str)) == 0
      end if

      if (present(fmt)) then
         read (str, fmt, iostat=stat) flt
      else
         read (str, *, iostat=stat) flt
      end if

   end subroutine str2real

   function count_char(string, char) result(count)

      character(len=*), intent(in) :: string
      character(len=1), intent(in) :: char
      integer :: count, pos, start_pos

      count = 0
      start_pos = 1
      do
         pos = index(string(start_pos:), char)
         if (pos == 0) exit
         count = count + 1
         start_pos = start_pos + pos

      end do
   end function count_char
end module core_misc_mod
