module core_memory_mod
   use iso_c_binding
   implicit none

   private
   public :: core_alloc_aligned, core_dealloc_aligned

   interface
      function c_aligned_alloc(alignment, size) bind(c, name="aligned_alloc")
         import
         type(c_ptr) :: c_aligned_alloc
         integer(c_size_t), value :: alignment
         integer(c_size_t), value :: size
      end function c_aligned_alloc

      subroutine c_free(ptr) bind(c, name="free")
         import
         type(c_ptr), value :: ptr
      end subroutine c_free
   end interface

contains

   subroutine core_alloc_aligned(ptr, size, alignment)
      type(c_ptr), intent(out) :: ptr
      integer(c_size_t), intent(in) :: size
      integer(c_size_t), intent(in) :: alignment
      integer(c_size_t) :: rounded_size

      ! aligned_alloc requires size to be a multiple of alignment
      rounded_size = ((size + alignment - 1_c_size_t)/alignment)*alignment
      ptr = c_aligned_alloc(alignment, rounded_size)
   end subroutine core_alloc_aligned

   subroutine core_dealloc_aligned(ptr)
      type(c_ptr), intent(in) :: ptr
      call c_free(ptr)
   end subroutine core_dealloc_aligned

end module core_memory_mod
