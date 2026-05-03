module core_memory_mod
   use iso_c_binding
   implicit none

   private
   public :: core_alloc_aligned, core_dealloc_aligned

   interface
      function c_malloc(size) bind(c, name="malloc")
         import
         type(c_ptr) :: c_malloc
         integer(c_size_t), value :: size
      end function c_malloc

      subroutine c_free(ptr) bind(c, name="free")
         import
         type(c_ptr), value :: ptr
      end subroutine c_free
   end interface

contains

   ! Simple wrapper for aligned allocation
   subroutine core_alloc_aligned(ptr, size, alignment)
      type(c_ptr), intent(out) :: ptr
      integer(c_size_t), intent(in) :: size
      integer(c_size_t), intent(in) :: alignment
      
      ptr = c_malloc(size)
   end subroutine core_alloc_aligned

   subroutine core_dealloc_aligned(ptr)
      type(c_ptr), intent(in) :: ptr
      call c_free(ptr)
   end subroutine core_dealloc_aligned

end module core_memory_mod
