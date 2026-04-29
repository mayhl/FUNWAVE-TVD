!> @brief Module for platform-independent path manipulation.
module core_path_mod
   use, intrinsic :: iso_c_binding
   implicit none
   private

   ! Determine path separator based on OS
#ifdef _WIN32
   character(len=1), parameter :: SEP = '\'
#else
   character(len=1), parameter :: SEP = '/'
#endif

   interface
      function is_directory(path) bind(c, name="is_directory") result(res)
         import :: c_char, c_bool
         character(kind=c_char), intent(in) :: path(*)
         logical(c_bool) :: res
      end function is_directory

      function is_regular_file(path) bind(c, name="is_regular_file") result(res)
         import :: c_char, c_bool
         character(kind=c_char), intent(in) :: path(*)
         logical(c_bool) :: res
      end function is_regular_file
   end interface

   !> @brief Utility type for path manipulation
   type, public :: type_path
      character(len=:), allocatable :: root
   contains
      procedure, public :: exists => path_exists
      procedure, public :: is_file => path_is_file
      procedure, public :: is_dir => path_is_dir
      procedure, public :: join => path_join
      procedure, public :: file_size => path_file_size
      procedure, public :: touch => path_touch
      procedure, public :: remove => path_remove
      procedure, public :: get_filename => path_get_filename
      procedure, public :: get_parent => path_get_parent
      procedure, public :: get_suffix => path_get_suffix
      procedure, public :: has_suffix => path_has_suffix
      procedure, public :: add_suffix => path_add_suffix
   end type type_path

   !> @brief Constructor interface for path utilities
   interface type_path
      module procedure new_path
   end interface type_path

contains

   !> @brief Constructor function
   function new_path(path_str) result(this)
      type(type_path) :: this
      character(len=*), intent(in) :: path_str
      this%root = trim(path_str)
   end function new_path

   !> Check if file or directory exists
   function path_exists(this) result(exists)
      class(type_path), intent(in) :: this
      logical :: exists
      inquire (file=trim(this%root), exist=exists)
   end function path_exists

   !> Check if path is a file
   function path_is_file(this) result(is_file)
      class(type_path), intent(in) :: this
      logical :: is_file
      is_file = is_regular_file(trim(this%root)//c_null_char)
   end function path_is_file

   !> Check if path is a directory
   function path_is_dir(this) result(is_dir)
      class(type_path), intent(in) :: this
      logical :: is_dir
      is_dir = is_directory(trim(this%root)//c_null_char)
   end function path_is_dir

   !> Join current path with a new component
   function path_join(this, subpath) result(new_path)
      class(type_path), intent(in) :: this
      character(len=*), intent(in) :: subpath
      type(type_path) :: new_path
      new_path%root = trim(this%root)//SEP//trim(subpath)
   end function path_join

   !> Get file size
   function path_file_size(this) result(size)
      class(type_path), intent(in) :: this
      integer(8) :: size
      inquire (file=trim(this%root), size=size)
   end function path_file_size

   !> Updates the file timestamp or creates an empty file
   subroutine path_touch(this)
      class(type_path), intent(in) :: this
      integer :: unit, stat
      open (newunit=unit, file=trim(this%root), access='append', action='write', iostat=stat)
      if (stat == 0) close (unit)
   end subroutine path_touch

   !> Removes the file
   subroutine path_remove(this)
      class(type_path), intent(in) :: this
      integer :: unit, stat
      open (newunit=unit, file=trim(this%root), status='old', iostat=stat)
      if (stat == 0) close (unit, status='delete')
   end subroutine path_remove

   !> Get filename from path
   function path_get_filename(this, path_in) result(name)
      class(type_path), intent(in) :: this
      character(len=*), intent(in) :: path_in
      character(len=len(path_in)) :: name
      integer :: i
      i = scan(path_in, "/\", back=.true.)
      name = path_in(i + 1:)
   end function path_get_filename

   !> Get parent directory path
   function path_get_parent(this, path_in) result(parent)
      class(type_path), intent(in) :: this
      character(len=*), intent(in) :: path_in
      character(len=len(path_in)) :: parent
      integer :: i
      i = scan(path_in, "/\", back=.true.)
      if (i > 0) then
         parent = path_in(1:i - 1)
      else
         parent = "."
      end if
   end function path_get_parent

   !> Extracts the file suffix
   function path_get_suffix(this, path_in) result(ext)
      class(type_path), intent(in) :: this
      character(len=*), intent(in) :: path_in
      character(len=len(path_in)) :: ext
      integer :: i
      i = scan(path_in, ".", back=.true.)
      if (i > 0) then
         ext = path_in(i + 1:)
      else
         ext = ""
      end if
   end function path_get_suffix

   !> Check if the file has a specific suffix
   function path_has_suffix(this, path_in, ext) result(has_ext)
      class(type_path), intent(in) :: this
      character(len=*), intent(in) :: path_in, ext
      logical :: has_ext
      has_ext = (trim(this%get_suffix(path_in)) == trim(ext))
   end function path_has_suffix

   !> Adds a suffix to a path
   function path_add_suffix(this, path_in, ext) result(new_path)
      class(type_path), intent(in) :: this
      character(len=*), intent(in) :: path_in, ext
      character(len=len(path_in) + len(ext) + 1) :: new_path
      new_path = trim(path_in)//"."//trim(ext)
   end function path_add_suffix

end module core_path_mod
