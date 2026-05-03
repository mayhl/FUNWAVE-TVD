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

      function mkdir_wrapper(path) bind(c, name="mkdir_wrapper") result(res)
         import :: c_char, c_bool
         character(kind=c_char), intent(in) :: path(*)
         logical(c_bool) :: res
      end function mkdir_wrapper

      function rmdir_wrapper(path) bind(c, name="rmdir_wrapper") result(res)
         import :: c_char, c_bool
         character(kind=c_char), intent(in) :: path(*)
         logical(c_bool) :: res
      end function rmdir_wrapper
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
      procedure, public :: mkdir => path_mkdir
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

   function path_exists(this) result(exists)
      class(type_path), intent(in) :: this
      logical :: exists
      inquire (file=trim(this%root), exist=exists)
   end function path_exists

   function path_is_file(this) result(is_file)
      class(type_path), intent(in) :: this
      logical :: is_file
      is_file = is_regular_file(trim(this%root)//c_null_char)
   end function path_is_file

   function path_is_dir(this) result(is_dir)
      class(type_path), intent(in) :: this
      logical :: is_dir
      is_dir = is_directory(trim(this%root)//c_null_char)
   end function path_is_dir

   function path_mkdir(this) result(success)
      class(type_path), intent(in) :: this
      logical :: success
      success = mkdir_wrapper(trim(this%root)//c_null_char)
   end function path_mkdir

   function path_join(this, subpath) result(new_path)
      class(type_path), intent(in) :: this
      character(len=*), intent(in) :: subpath
      type(type_path) :: new_path
      new_path%root = trim(this%root)//SEP//trim(subpath)
   end function path_join

   function path_file_size(this) result(size)
      class(type_path), intent(in) :: this
      integer(8) :: size
      inquire (file=trim(this%root), size=size)
   end function path_file_size

   subroutine path_touch(this)
      class(type_path), intent(in) :: this
      integer :: unit, stat
      open (newunit=unit, file=trim(this%root), access='append', action='write', iostat=stat)
      if (stat == 0) close (unit)
   end subroutine path_touch

   !> Removes the file or directory
   subroutine path_remove(this)
      class(type_path), intent(in) :: this
      integer :: unit, stat
      logical :: success

      if (this%is_dir()) then
         success = rmdir_wrapper(trim(this%root)//c_null_char)
      else
         open (newunit=unit, file=trim(this%root), status='old', iostat=stat)
         if (stat == 0) close (unit, status='delete')
      end if
   end subroutine path_remove

   function path_get_filename(this) result(name)
      class(type_path), intent(in) :: this
      character(len=len(this%root)) :: name
      integer :: i
      i = scan(this%root, "/\", back=.true.)
      name = this%root(i + 1:)
   end function path_get_filename

   function path_get_parent(this) result(parent)
      class(type_path), intent(in) :: this
      character(len=len(this%root)) :: parent
      integer :: i
      i = scan(this%root, "/\", back=.true.)
      if (i > 0) then
         parent = this%root(1:i - 1)
      else
         parent = "."
      end if
   end function path_get_parent

   function path_get_suffix(this) result(ext)
      class(type_path), intent(in) :: this
      character(len=len(this%root)) :: ext
      integer :: i
      i = scan(this%root, ".", back=.true.)
      if (i > 0) then
         ext = this%root(i + 1:)
      else
         ext = ""
      end if
   end function path_get_suffix

   function path_has_suffix(this, ext) result(has_ext)
      class(type_path), intent(in) :: this
      character(len=*), intent(in) :: ext
      logical :: has_ext
      has_ext = (trim(this%get_suffix()) == trim(ext))
   end function path_has_suffix

   subroutine path_add_suffix(this, ext)
      class(type_path), intent(inout) :: this
      character(len=*), intent(in) :: ext
      this%root = trim(this%root)//"."//trim(ext)
   end subroutine path_add_suffix

end module core_path_mod
