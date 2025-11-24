!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Component
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module ModelComponent

   type, abstract :: TypeInterfaceComp

   contains
      ! procedure :: validateInput
      ! procedure :: allocateMemory

   end type TypeInterfaceComp

   type, extends(TypeInterfaceComp) :: TypeGridComp

      public
      integer :: p_procs, n_procs, m_procs
      integer :: n_loc, m_loc, n_glob, m_glob

   end type TypeGridComp

contains

   function validateInput() result(ierror)

      integer :: ierror

   end function validateInput

   subroutine constructor(grid)
      type(TypeGridComp), INTENT(INOUT) :: grid

   end subroutine constructor

end module ModelComponent

