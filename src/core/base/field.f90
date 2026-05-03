module core_field_mod
   use iso_c_binding
   use core_constants_mod, only: SP
   use core_memory_mod, only: core_alloc_aligned, core_dealloc_aligned
   use core_grid_interface_mod, only: abstract_grid
   implicit none

   type, public :: type_field
      character(len=32) :: name
      integer :: rank = 0
      integer, allocatable :: shape(:)
      type(c_ptr) :: host_ptr = c_null_ptr
      class(abstract_grid), pointer :: grid => null()

   contains
      procedure :: allocate => field_allocate
      procedure :: allocate_on_grid => field_allocate_on_grid
      procedure :: finalize => field_finalize
      procedure :: access_1d => field_access_1d
      procedure :: access_2d => field_access_2d
      procedure :: access_3d => field_access_3d
   end type type_field

   contains

   subroutine field_allocate_on_grid(this, name, rank, shape, grid)
      class(type_field), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: rank
      integer, intent(in) :: shape(:)
      class(abstract_grid), target, intent(in) :: grid

      this%grid => grid
      call this%allocate(name, rank, shape)
   end subroutine field_allocate_on_grid
   subroutine field_allocate(this, name, rank, shape)
      class(type_field), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, intent(in) :: rank
      integer, intent(in) :: shape(:)
      integer(c_size_t) :: total_size
      integer :: i

      this%name = name
      this%rank = rank
      this%shape = shape
      
      ! Calculate total size in bytes (assuming SP = 4 bytes)
      total_size = 4
      do i = 1, rank
         total_size = total_size * shape(i)
      end do

      call core_alloc_aligned(this%host_ptr, total_size, 64_c_size_t)
   end subroutine field_allocate

   subroutine field_finalize(this)
      class(type_field), intent(inout) :: this
      if (c_associated(this%host_ptr)) then
         call core_dealloc_aligned(this%host_ptr)
         this%host_ptr = c_null_ptr
      end if
   end subroutine field_finalize

   subroutine field_access_1d(this, ptr)
      class(type_field), intent(in) :: this
      real(SP), pointer, intent(out) :: ptr(:)
      call c_f_pointer(this%host_ptr, ptr, [this%shape(1)])
   end subroutine field_access_1d

   subroutine field_access_2d(this, ptr)
      class(type_field), intent(in) :: this
      real(SP), pointer, intent(out) :: ptr(:, :)
      call c_f_pointer(this%host_ptr, ptr, [this%shape(1), this%shape(2)])
   end subroutine field_access_2d

   subroutine field_access_3d(this, ptr)
      class(type_field), intent(in) :: this
      real(SP), pointer, intent(out) :: ptr(:, :, :)
      call c_f_pointer(this%host_ptr, ptr, [this%shape(1), this%shape(2), this%shape(3)])
   end subroutine field_access_3d

end module core_field_mod
