
module misc_mod
   use Constants, only: SP

contains

   function random() result(val)
      real(SP) :: val

# if defined (INTEL)
      val = rand()
# elif defined (CRAY)
      val = rand()
# else
      call random_number(val)
# endif

   end function random

   elemental subroutine str2int(str, int, stat, fmt, is_empty)
      implicit none(external)
      character(len=*), intent(in) :: str
      integer, intent(out) :: int
      integer, intent(out) :: stat
      character(*), intent(in), optional :: fmt
      logical, intent(out), optional :: is_empty

      character(:), allocatable :: fmt_

      if (present(fmt)) then
         fmt_ = fmt
      else
         fmt_ = "(I8)"
      end if

      if (present(is_empty)) then
         is_empty = len(trim(str)) == 0
      end if

      read (str, fmt_, iostat=stat) int

   end subroutine str2int

   elemental subroutine str2real(str, flt, stat, fmt, is_empty)
      implicit none(external)
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

end module misc_mod
