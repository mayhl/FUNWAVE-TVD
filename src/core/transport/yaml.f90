module core_yaml_file_mod
   use core_comm_mod, only: type_comm
   use core_constants_mod, only: MESSAGE_SIZE, STRING_SIZE, LABEL_SIZE, SP, type_string
   use core_log_io_mod, only: type_log_writer
   use core_misc_mod, only: str2int, str2real
   use core_path_mod, only: type_path
   use core_range_parse_mod, only: type_integer_range, type_real_range
   use core_units_mod, only: apply_unit_conversion, get_units_by_dim

   use fortran_yaml_c, only: YamlFile, dp, &
                             type_node, type_dictionary, type_error, &
                             type_list, type_list_item, type_scalar
   implicit none(external)

   private
   public :: type_yaml_reader

   type(type_log_writer), TARGET :: log_buff
   character(LABEL_SIZE), parameter :: log_label = "config"

   type type_yaml_reader
      logical :: is_io_node = .False.
      character(MESSAGE_SIZE):: path = ""
      type(type_log_writer), pointer, public :: log
      type(type_comm), pointer, public :: comm
      class(type_dictionary), pointer :: root => null()
      type(YamlFile):: file

   contains

      procedure, public :: init
      procedure, public :: has_key
      procedure, public :: finalize
      procedure, public :: clone

      procedure :: parse_error_message
      procedure :: prep_msg
      procedure :: prep_msg_val
      procedure :: prep_extern_msg
      procedure :: sanitize_path

      procedure, public :: cast_dictionary
      procedure, public :: is_dictionary
      procedure, public :: is_dictionary_node
      procedure, public :: read_time

      ! Integers
      procedure, public :: read_integer
      procedure, public :: read_integer_node
      procedure, public :: read_positive_integer
      procedure, public :: read_negative_integer
      procedure, public :: read_nonnegative_integer
      procedure, public :: read_nonpositive_integer

      ! Real
      procedure, public :: read_real
      procedure, public :: read_real_node
      procedure, public :: read_positive_real
      procedure, public :: read_negative_real
      procedure, public :: read_nonnegative_real
      procedure, public :: read_nonpositive_real

      ! Bool/Logical
      procedure, public :: read_logical
      procedure, public :: read_logical_node

      ! Strings
      procedure, public :: read_string
      procedure, public :: read_string_node
      procedure, public :: read_string_array
   procedure, public :: read_integer_array
   procedure, public :: read_real_array
      procedure, public :: read_enum
      procedure, public :: read_enum_node
      procedure, public :: read_input_path

      ! Interface overloads
      generic, public :: read => read_integer, read_real, read_logical, &
         read_string, read_enum, read_input_path, read_string_array, &
         read_integer_array, read_real_array

      generic, public :: read_positive => read_positive_integer, read_positive_real
      generic, public :: read_negative => read_negative_integer, read_negative_real
      generic, public :: read_nonnegative => read_nonnegative_integer, read_nonnegative_real
      generic, public :: read_nonpositive => read_nonpositive_integer, read_nonpositive_real

   end type type_yaml_reader

contains

   subroutine init(this, path_str, comm)
      class(type_yaml_reader), intent(inout) :: this
      character(:), allocatable, intent(in) :: path_str
      type(type_comm), target, intent(inout):: comm

      character(:), allocatable :: err
      class(type_node), pointer :: root
      type(type_path) :: path
      character(MESSAGE_SIZE) :: message
      logical :: file_exist

      log_buff = comm%get_logger(log_label)
      this%log => log_buff
      this%comm => comm

      path = type_path(path_str)

      if (.not. path%is_file()) then
         message = "Input file does not exists, got: "//trim(path_str)
         call this%log%exit_on_error(message)
      end if

      if (path%file_size() .eq. 0) then
         message = "Input file is empty: "//trim(path_str)
         call this%log%exit_on_error(message)
      end if

      call this%file%parse(path%root, err)

      if (allocated(err)) then
         call this%log%exit_on_error(err)
      end if

      root => this%file%root
      select type (root)
      class is (type_dictionary)
         this%root => root

      class default
         call this%log%exit_on_error('Input file does not appear to be a valid YAML file.')
      end select

   end subroutine init

   function clone(this, node) result(reader)
      class(type_yaml_reader), intent(in) :: this
      class(type_dictionary), target, intent(in) :: node
      type(type_yaml_reader) :: reader

      reader%log => this%log
      reader%comm => this%comm
      reader%root => node
   end function clone

   function is_dictionary_node(this, key, silent) result(val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      logical, optional, intent(out) :: silent
      logical :: val

      class(type_node), pointer :: node
      character(:), allocatable :: buff

      node => this%root%get(key)

      if (associated(node)) then
         select type (node)
         class is (type_dictionary)
            val = .true.
         class default
            val = .false.
         end select
         if (present(silent)) silent = .false.
      else
         if (present(silent)) then
            silent = .true.
            val = .false.
         else
            buff = trim(this%root%path)//' does not contain key "'//trim(key)//'".'
            call this%log%exit_on_error(buff)
         end if
      end if
   end function is_dictionary_node

   function is_dictionary(this, key, is_empty) result(val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      logical, optional, intent(out) :: is_empty
      logical :: val
      val = this%is_dictionary_node(key, is_empty)
   end function is_dictionary

   function has_key(this, key, is_empty) result(val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      logical, optional, intent(out) :: is_empty
      logical :: val
      val = associated(this%root%get(key))
   end function has_key

   function cast_dictionary(this, key, is_empty) result(child)
      class(type_yaml_reader), intent(in) :: this
      character(*), intent(in) :: key
      logical, optional, intent(out) :: is_empty
      type(type_yaml_reader) :: child
      type(type_dictionary), pointer :: node
      type(type_error), allocatable :: io_err

      logical, parameter :: is_required = .true.

      node => this%root%get_dictionary(key, is_required, io_err)

      if (allocated(io_err)) then
         if (present(is_empty)) then
            is_empty = .true.
         else
            call this%log%exit_on_error(io_err%message)
         end if
      else
         if (present(is_empty)) is_empty = .false.
      end if

      if (associated(node)) then
         child = this%clone(node)
      end if

   end function cast_dictionary

   ! ... (rest of the file unchanged)
#include "core/prep.inc"
#define _NAME integer
#define _CLASS integer
#define _GET get_integer
#define _RANGE type_integer_range
#define _HAS_RANGE 1
#define _READ _PASTE(read_,_NAME)
#define _READ_NODE _PASTE(_READ,_node)
#define _READ_POSITIVE _PASTE(read_positive_,_NAME)
#define _READ_NEGATIVE _PASTE(read_negative_,_NAME)
#define _READ_NONNEGATIVE _PASTE(read_nonnegative_,_NAME)
#define _READ_NONPOSITIVE _PASTE(read_nonpositive_,_NAME)
#include "core/yaml_body.inc"
#undef _READ
#undef _READ_NODE
#undef _READ_POSITIVE
#undef _READ_NEGATIVE
#undef _READ_NONNEGATIVE
#undef _READ_NONPOSITIVE
#undef _HAS_RANGE

#define _NAME real
#define _CLASS real(SP)
#define _GET get_real
#define _RANGE type_real_range
#define _HAS_RANGE 1
#define _HAS_UNITS 1
#define _READ _PASTE(read_,_NAME)
#define _READ_NODE _PASTE(_READ,_node)
#define _READ_POSITIVE _PASTE(read_positive_,_NAME)
#define _READ_NEGATIVE _PASTE(read_negative_,_NAME)
#define _READ_NONNEGATIVE _PASTE(read_nonnegative_,_NAME)
#define _READ_NONPOSITIVE _PASTE(read_nonpositive_,_NAME)
#include "core/yaml_body.inc"
#undef _READ
#undef _READ_NODE
#undef _READ_POSITIVE
#undef _READ_NEGATIVE
#undef _READ_NONNEGATIVE
#undef _READ_NONPOSITIVE
#undef _HAS_RANGE
#undef _HAS_UNITS

#define _NAME logical
#define _CLASS logical
#define _GET get_logical
#define _READ _PASTE(read_,_NAME)
#define _READ_NODE _PASTE(_READ,_node)
#include "core/yaml_body.inc"
#undef _READ
#undef _READ_NODE

#define _NAME string
#define _CLASS character(:), allocatable
#define _GET get_string
#define _READ _PASTE(read_,_NAME)
#define _READ_NODE _PASTE(_READ,_node)
#include "core/yaml_body.inc"
#undef _READ
#undef _READ_NODE

   subroutine read_time(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      real(SP), intent(out) :: val
      type(type_yaml_reader) :: child
      character(:), allocatable :: unit
      character(len=5), dimension(4) :: utypes
      data utypes/'sec', 'min', 'hour', 'hertz'/
      if (this%is_dictionary(key)) then
         child = this%cast_dictionary(key)
         call child%read_enum("units", utypes, val=unit)
         call child%read_positive_real("value", default=default, silent=silent, val=val)
         select case (unit)
         case ('min')
            val = val*60_SP
         case ('hour')
            val = val*3600_SP
         case ('hertz')
            val = 1.0_SP/val
         end select
         if (allocated(unit)) deallocate (unit)
      else
         call this%read_positive_real(key, default=default, silent=silent, val=val)
      end if
   end subroutine read_time

   subroutine read_enum_node(this, key, values, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(len=*), dimension(:), intent(in) :: values
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      character(:), allocatable, intent(inout) :: val
      integer :: len, i
      logical :: is_found
      character(len=:), allocatable :: msg
      call this%read_string_node(key, default, silent, val)
      len = size(values)
      is_found = .false.
      do i = 1, len
         if (val .eq. values(i)) then
            is_found = .true.
            exit
         end if
      end do
      if (.not. is_found) then
         msg = "which is not in the allowable list of values. Valid values: "
         msg = msg//trim(values(1))
         do i = 2, len - 1
            msg = msg//', '//trim(values(i))
         end do
         msg = msg//', & '//trim(values(len))//"."
         msg = this%prep_msg_val(key, msg)
         call this%log%exit_on_error(msg)
      end if
   end subroutine read_enum_node

   subroutine read_enum(this, key, values, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(len=*), dimension(:), intent(in) :: values
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      character(:), allocatable, intent(inout) :: val
      integer :: n, i
      logical :: is_found, p_silent
      character(len=:), allocatable :: msg

      p_silent = .false.
      call this%read_string(key, default=default, silent=p_silent, val=val)
      if (present(silent)) silent = p_silent
      if (.not. p_silent) then
         n = size(values)
         is_found = .false.
         do i = 1, n
            if (val .eq. values(i)) then
               is_found = .true.
               exit
            end if
         end do
         if (.not. is_found) then
            msg = "which is not in the allowable list of values. Valid values: "
            msg = msg//trim(values(1))
            do i = 2, n - 1
               msg = msg//', '//trim(values(i))
            end do
            msg = msg//', & '//trim(values(n))//"."
            msg = this%prep_msg_val(key, msg)
            call this%log%exit_on_error(msg)
         end if
      end if
   end subroutine read_enum

   subroutine read_input_path(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      type(type_path), intent(inout) :: val
      character(:), allocatable :: val_buff
      character(len=:), allocatable :: msg
      logical :: p_silent

      p_silent = .false.
      call this%read_string(key, default=default, silent=p_silent, val=val_buff)
      if (present(silent)) silent = p_silent
      if (.not. p_silent) then
         val = type_path(val_buff)
         if (.not. val%is_file()) then
            msg = "which is not a valid file path."
            msg = this%prep_msg_val(key, msg)
            call this%log%exit_on_error(msg)
         end if
      end if
   end subroutine read_input_path

   function parse_error_message(this, key, io_err, default, silent) result(is_default)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in)::key
      type(type_error), allocatable, intent(inout) :: io_err
      character(*), optional, intent(in):: default
      logical, optional, intent(out) ::silent
      logical :: is_default
      class(type_node), pointer:: node
      character(STRING_SIZE) :: buff
      integer :: err_id
      if (.not. allocated(io_err)) then
         is_default = .false.
         buff = " read value '"//this%root%get_string(key, error=io_err)//"'."
         call this%log%debug(this%prep_msg(key, buff))
         return
      end if
      if (present(default)) then
         if (is_no_key_err(io_err)) then
            is_default = .true.
            buff = " not found, using default value "//trim(default)//'.'
            call this%log%info(this%prep_msg(key, buff))
            call this%root%set_string(key, default)
            node => this%root%get(key)
            node%path = this%root%path//"/"//key
         else
            is_default = .false.
            buff = this%prep_extern_msg(key, io_err%message)
            call this%log%exit_on_error(buff)
         end if
      else if (present(silent)) then
         silent = .true.
         is_default = .false.
         if (.not. is_no_key_err(io_err)) then
            buff = this%prep_extern_msg(key, io_err%message)
            call this%log%exit_on_error(buff)
         end if
      else
         is_default = .false.
         buff = this%prep_extern_msg(key, io_err%message)
         call this%log%exit_on_error(buff)
      end if
      deallocate (io_err)
   end function parse_error_message

   function sanitize_path(this, key) result(new_path)
      class(type_yaml_reader), intent(in) :: this
      character(*), intent(in) :: key
      character(len=:), allocatable :: new_path
      integer :: i, n
      new_path = adjustl(this%root%path)
      n = len_trim(new_path)
      do i = 1, n
         if (new_path(i:i) == "/") then
            new_path(i:i) = "."
         end if
      end do
      if (n > 0) then
         if (new_path(1:1) == '.') then
            new_path = new_path(2:n)
         end if
         new_path = new_path//"."//key
      else
         new_path = key
      end if
   end function sanitize_path

   function prep_msg(this, key, msg) result(new_msg)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in)::key, msg
      character(len=STRING_SIZE) :: new_msg
      new_msg = this%sanitize_path(key)//msg
   end function prep_msg

   function prep_msg_val(this, key, msg) result(new_msg)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in)::key, msg
      character(len=STRING_SIZE) :: new_msg
      character(:), allocatable :: val
      type(type_error), allocatable :: io_err
      val = this%root%get_string(key, error=io_err)
      new_msg = this%sanitize_path(key)//" is set to """//val//""", "//msg
   end function prep_msg_val

   function prep_extern_msg(this, key, msg) result(new_msg)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in)::key, msg
      character(len=STRING_SIZE) :: new_msg
      integer :: i0, i1
      i0 = len_trim(this%root%path) + len_trim(key) + 2
      i1 = len_trim(msg)
      new_msg = this%sanitize_path(key)//msg(i0:i1)
   end function prep_extern_msg

   pure function is_no_key_err(io_err) result(val)
      type(type_error), allocatable, intent(in) :: io_err
      character(*), parameter:: NO_KEY_ERR = "does not contain key"
      logical :: val
      val = (index(io_err%message, NO_KEY_ERR) .gt. 0)
   end function is_no_key_err

   subroutine read_string_array(this, key, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      type(type_string), allocatable, intent(inout), dimension(:) :: val
      logical, optional, intent(out) :: silent
      class(type_node), pointer :: node
      class(type_list), pointer :: list_node
      type(type_list_item), pointer :: item
      class(type_scalar), pointer :: item_scalar
      integer :: n, i
      logical :: p_silent

      p_silent = .false.
      if (this%comm%is_io_node()) then
         node => this%root%get(key)
         if (.not. associated(node)) then
            if (present(silent)) then
               p_silent = .true.
            else
               call this%log%exit_on_error(trim(this%root%path)//' does not contain key "'//trim(key)//'".')
            end if
         else
            select type (node)
            class is (type_list)
               list_node => node
               n = list_node%size()
               if (allocated(val)) deallocate (val)
               allocate (val(n))
               i = 1
               item => list_node%first
               do while (associated(item))
                  select type (node_item => item%node)
                  class is (type_scalar)
                     item_scalar => node_item
                     val(i)%s = item_scalar%string
                  class default
                     call this%log%exit_on_error("List item at index "//key//" is not a scalar.")
                  end select
                  i = i + 1
                  item => item%next
               end do
            class default
               call this%log%exit_on_error("Key '"//trim(key)//"' is not a list.")
            end select
         end if
      end if
      if (present(silent)) then
         call this%comm%bcast(p_silent)
         silent = p_silent
      end if
      if (.not. p_silent) call this%comm%bcast(val)
   end subroutine read_string_array

   subroutine read_integer_array(this, key, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      integer, allocatable, intent(inout), dimension(:) :: val
      logical, optional, intent(out) :: silent
      class(type_node), pointer :: node
      class(type_list), pointer :: list_node
      type(type_list_item), pointer :: item
      class(type_scalar), pointer :: item_scalar
      integer :: n, i, stat
      logical :: p_silent

      p_silent = .false.
      if (this%comm%is_io_node()) then
         node => this%root%get(key)
         if (.not. associated(node)) then
            if (present(silent)) then
               p_silent = .true.
            else
               call this%log%exit_on_error(trim(this%root%path)//' does not contain key "'//trim(key)//'".')
            end if
         else
            select type (node)
            class is (type_list)
               list_node => node
               n = list_node%size()
               if (allocated(val)) deallocate (val)
               allocate (val(n))
               i = 1
               item => list_node%first
               do while (associated(item))
                  select type (node_item => item%node)
                  class is (type_scalar)
                     item_scalar => node_item
                     call str2int(item_scalar%string, val(i), stat)
                     if (stat /= 0) call this%log%exit_on_error("Value '"//trim(item_scalar%string)//"' is not a valid integer.")
                  class default
                     call this%log%exit_on_error("List item at index "//key//" is not a scalar.")
                  end select
                  i = i + 1
                  item => item%next
               end do
            class default
               call this%log%exit_on_error("Key '"//trim(key)//"' is not a list.")
            end select
         end if
      end if
      if (present(silent)) then
         call this%comm%bcast(p_silent)
         silent = p_silent
      end if
      if (.not. p_silent) call this%comm%bcast(val)
   end subroutine read_integer_array

   subroutine read_real_array(this, key, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      real(SP), allocatable, intent(inout), dimension(:) :: val
      logical, optional, intent(out) :: silent
      class(type_node), pointer :: node
      class(type_list), pointer :: list_node
      type(type_list_item), pointer :: item
      class(type_scalar), pointer :: item_scalar
      integer :: n, i, stat
      logical :: p_silent

      p_silent = .false.
      if (this%comm%is_io_node()) then
         node => this%root%get(key)
         if (.not. associated(node)) then
            if (present(silent)) then
               p_silent = .true.
            else
               call this%log%exit_on_error(trim(this%root%path)//' does not contain key "'//trim(key)//'".')
            end if
         else
            select type (node)
            class is (type_list)
               list_node => node
               n = list_node%size()
               if (allocated(val)) deallocate (val)
               allocate (val(n))
               i = 1
               item => list_node%first
               do while (associated(item))
                  select type (node_item => item%node)
                  class is (type_scalar)
                     item_scalar => node_item
                     call str2real(item_scalar%string, val(i), stat)
                     if (stat /= 0) call this%log%exit_on_error("Value '"//trim(item_scalar%string)//"' is not a valid real.")
                  class default
                     call this%log%exit_on_error("List item at index "//key//" is not a scalar.")
                  end select
                  i = i + 1
                  item => item%next
               end do
            class default
               call this%log%exit_on_error("Key '"//trim(key)//"' is not a list.")
            end select
         end if
      end if
      if (present(silent)) then
         call this%comm%bcast(p_silent)
         silent = p_silent
      end if
      if (.not. p_silent) call this%comm%bcast(val)
   end subroutine read_real_array

   subroutine finalize(this)
      class(type_yaml_reader), intent(inout) :: this
      if (associated(this%root)) then
         call this%root%finalize()
         nullify (this%root)
      end if
   end subroutine finalize

end module core_yaml_file_mod
