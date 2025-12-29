
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

end module misc_mod
