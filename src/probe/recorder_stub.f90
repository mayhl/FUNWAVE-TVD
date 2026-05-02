module probe_recorder
   implicit none
contains

   subroutine reset_state()
   end subroutine reset_state

   ! Scalars
   subroutine dump_state_r4(var, name)
      real(4), intent(in) :: var
      character(len=*), intent(in) :: name
   end subroutine dump_state_r4

   subroutine dump_state_r8(var, name)
      real(8), intent(in) :: var
      character(len=*), intent(in) :: name
   end subroutine dump_state_r8

   subroutine dump_state_i4(var, name)
      integer(4), intent(in) :: var
      character(len=*), intent(in) :: name
   end subroutine dump_state_i4

   ! 1D Arrays
   subroutine dump_state_r4_a1(var, name)
      real(4), intent(in) :: var(:)
      character(len=*), intent(in) :: name
   end subroutine dump_state_r4_a1

   subroutine dump_state_r8_a1(var, name)
      real(8), intent(in) :: var(:)
      character(len=*), intent(in) :: name
   end subroutine dump_state_r8_a1

   subroutine dump_state_i4_a1(var, name)
      integer(4), intent(in) :: var(:)
      character(len=*), intent(in) :: name
   end subroutine dump_state_i4_a1

   ! 2D Arrays
   subroutine dump_state_r4_a2(var, name)
      real(4), intent(in) :: var(:, :)
      character(len=*), intent(in) :: name
   end subroutine dump_state_r4_a2

   subroutine dump_state_r8_a2(var, name)
      real(8), intent(in) :: var(:, :)
      character(len=*), intent(in) :: name
   end subroutine dump_state_r8_a2

   subroutine dump_state_i4_a2(var, name)
      integer(4), intent(in) :: var(:, :)
      character(len=*), intent(in) :: name
   end subroutine dump_state_i4_a2

end module probe_recorder
