!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Parser clase for reading YAML files using:
!  https://github.com/BoldingBruggeman/fortran-yaml
!
!  PURPOSE:
!   - error handling
!     - optional default handling
!   - casting raw inputs
!
!
!  HISTORY:
!    11/23/2025  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module YamlFile

   use yaml
   use yaml_types
!   use, intrinsic :: iso_fortran_env
   !  private
   public :: YamlReader, constructor

   type YamlReader

      !   private
      logical :: is_io_node = .False.
      character(256):: path = ""
      class(type_node), pointer :: root => Null()

   contains

      !  private
      ! NOTE: constructor is not a class method
      procedure, public :: parseFile => parseFile

   end type YamlReader

contains

   function constructor(this, is_io_node)

      class(YamlReader), intent(inout) :: this
      logical, intent(in):: is_io_node

      this%is_io_node = is_io_node
      !this%root => parse(path, unit=100, error=err)

   end function constructor
   function parseFile(this, path, unit) result(message)

      class(YamlReader), intent(inout) :: this
      character(256), intent(in) :: path
      integer, intent(in) :: unit
      character(:), allocatable :: message

      this%root => parse(path, unit=unit, error=message)

   end function parseFile

end module YamlFile
