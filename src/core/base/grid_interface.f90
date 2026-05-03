module core_grid_interface_mod
   implicit none
   type, abstract, public :: abstract_grid
      integer :: n_points = 0
   contains
      procedure(get_indices_interface), deferred, public :: get_indices
   end type abstract_grid

   abstract interface
      subroutine get_indices_interface(this, indices)
         import :: abstract_grid
         class(abstract_grid), intent(inout) :: this
         integer, allocatable, intent(out) :: indices(:, :)
      end subroutine get_indices_interface
   end interface
end module core_grid_interface_mod
