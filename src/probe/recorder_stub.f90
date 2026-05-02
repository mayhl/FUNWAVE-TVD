module probe_recorder_stub
   implicit none
contains

   subroutine reset_state()
      ! no-op
   end subroutine reset_state

   subroutine dump_state(var, name)
      real(8), intent(in) :: var
      character(len=*), intent(in) :: name
   end subroutine dump_state

end module probe_recorder_stub
