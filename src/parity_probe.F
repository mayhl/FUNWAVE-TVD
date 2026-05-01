module parity_probe_mod
  implicit none

contains

  subroutine reset_state()
    integer :: unit_num = 99
    open(unit=unit_num, file="dump.bin", status="replace")
    close(unit_num)
  end subroutine reset_state

  subroutine dump_state(var, name)
    real(8), intent(in) :: var
    character(len=*), intent(in) :: name
    integer :: unit_num = 99
    open(unit=unit_num, file="dump.bin", status="unknown", access="append", form="unformatted")
    write(unit_num) name
    write(unit_num) var
    close(unit_num)
  end subroutine dump_state

end module parity_probe_mod
