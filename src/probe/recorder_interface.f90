module probe_mod
   implicit none
   interface
      subroutine reset_state()
      end subroutine reset_state

      subroutine dump_state(var, name)
         real(8), intent(in) :: var
         character(len=*), intent(in) :: name
      end subroutine dump_state
   end interface
end module probe_mod
