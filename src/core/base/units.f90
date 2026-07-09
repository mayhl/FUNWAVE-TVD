module core_units_mod
   use core_constants_mod, only: SP
   implicit none

   type, public :: type_unit
      character(16) :: name
      character(16) :: dimension ! 'time', 'length', 'mass'
      real(SP)      :: factor    ! Conversion to base unit
   end type type_unit

   ! Global registry of supported units
   type(type_unit), parameter, public :: &
      UNIT_REGISTRY(*) = [ &
      type_unit("sec", "time", 1.0_SP), &
      type_unit("min", "time", 60.0_SP), &
      type_unit("hour", "time", 3600.0_SP), &
      type_unit("hertz", "time", 1.0_SP), &
      type_unit("m", "length", 1.0_SP), &
      type_unit("cm", "length", 0.01_SP), &
      type_unit("km", "length", 1000.0_SP)]

contains

   function get_factor(name, dim) result(factor)
      character(*), intent(in) :: name, dim
      real(SP) :: factor
      integer :: i

      factor = -1.0_SP
      do i = 1, size(UNIT_REGISTRY)
         if (trim(name) == trim(UNIT_REGISTRY(i)%name) .and. &
             trim(dim) == trim(UNIT_REGISTRY(i)%dimension)) then
            factor = UNIT_REGISTRY(i)%factor
            exit
         end if
      end do
   end function get_factor

   subroutine get_units_by_dim(dim, units, err)
      character(*), intent(in) :: dim
      character(16), allocatable, intent(out) :: units(:)
      character(:), allocatable, intent(out) :: err
      integer :: i, count

      count = 0
      do i = 1, size(UNIT_REGISTRY)
         if (trim(dim) == trim(UNIT_REGISTRY(i)%dimension)) count = count + 1
      end do

      if (count == 0) then
         err = " read method has unknown dimension '"//trim(dim)//"'."
         return
      end if

      if (allocated(err)) deallocate (err)
      allocate (units(count))
      count = 0
      do i = 1, size(UNIT_REGISTRY)
         if (trim(dim) == trim(UNIT_REGISTRY(i)%dimension)) then
            count = count + 1
            units(count) = UNIT_REGISTRY(i)%name
         end if
      end do
   end subroutine get_units_by_dim

   subroutine apply_unit_conversion(val, unit_str, dim, converted_val, err)
      real(SP), intent(in) :: val
      character(*), intent(in) :: unit_str, dim
      real(SP), intent(out) :: converted_val
      character(:), allocatable, intent(out) :: err
      real(SP) :: factor

      factor = get_factor(unit_str, dim)
      if (factor < 0.0_SP) then
         err = " read method has unknown unit '"//trim(unit_str)//"' for dimension '"//trim(dim)//"'."
         converted_val = 0.0_SP
         return
      end if

      if (allocated(err)) deallocate (err)

      if (trim(unit_str) == "hertz") then
         converted_val = 1.0_SP/val
      else
         converted_val = val*factor
      end if
   end subroutine apply_unit_conversion

end module core_units_mod
