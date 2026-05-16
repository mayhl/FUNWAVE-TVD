!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Field registry: name → real(SP)(:,:) pointer map.
!
!  Registered at model init_compute (before first timestep).
!  Used by output_manager to read field values at output time.
!  Host-only — never referenced inside GPU kernel regions.
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module core_field_registry_mod
   use core_constants_mod, only: SP
   implicit none

   private
   public :: type_field_registry

   integer, parameter :: REGISTRY_MAX  = 64
   integer, parameter :: FIELD_NAME_LEN = 32

   type :: type_field_entry
      character(FIELD_NAME_LEN) :: name = ''
      real(SP), pointer :: data(:,:) => null()
   end type type_field_entry

   type :: type_field_registry
      type(type_field_entry) :: entries(REGISTRY_MAX)
      integer :: n = 0
   contains
      procedure :: register
      procedure :: get
      procedure :: has
      procedure :: finalize => registry_finalize
   end type type_field_registry

contains

   subroutine register(this, name, ptr)
      class(type_field_registry), intent(inout) :: this
      character(*),               intent(in)    :: name
      real(SP), target,           intent(in)    :: ptr(:,:)
      integer :: k

      do k = 1, this%n
         if (trim(this%entries(k)%name) == trim(name)) then
            this%entries(k)%data => ptr
            return
         end if
      end do

      if (this%n >= REGISTRY_MAX) &
         error stop 'type_field_registry: exceeded maximum registered fields (' // &
                    trim(adjustl(transfer(REGISTRY_MAX, ' '))) // ')'
      this%n = this%n + 1
      this%entries(this%n)%name = name
      this%entries(this%n)%data => ptr
   end subroutine register

   function get(this, name) result(ptr)
      class(type_field_registry), intent(in) :: this
      character(*),               intent(in) :: name
      real(SP), pointer :: ptr(:,:)
      integer :: k

      do k = 1, this%n
         if (trim(this%entries(k)%name) == trim(name)) then
            ptr => this%entries(k)%data
            return
         end if
      end do
      ptr => null()
      error stop 'type_field_registry: field not found: ' // trim(name)
   end function get

   logical function has(this, name)
      class(type_field_registry), intent(in) :: this
      character(*),               intent(in) :: name
      integer :: k

      has = .false.
      do k = 1, this%n
         if (trim(this%entries(k)%name) == trim(name)) then
            has = .true.
            return
         end if
      end do
   end function has

   subroutine registry_finalize(this)
      class(type_field_registry), intent(inout) :: this
      integer :: k
      do k = 1, this%n
         nullify(this%entries(k)%data)
         this%entries(k)%name = ''
      end do
      this%n = 0
   end subroutine registry_finalize

end module core_field_registry_mod
