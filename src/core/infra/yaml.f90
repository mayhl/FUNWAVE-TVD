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
                             type_list, type_list_item, type_scalar, &
                             type_key_value_pair
   implicit none

   private
   public :: type_yaml_reader

   type(type_log_writer), TARGET :: log_buff
   character(LABEL_SIZE), parameter :: log_label = "config"

   ! Unread-key detection (decided 2026-07-24): every consuming access marks
   ! the node's parse path in a set shared by all clones of one parsed file;
   ! finalize walks the tree leaves against it.  No key list to maintain --
   ! the readers ARE the schema.  Entries ending "/*" reserve a subtree
   ! (schema-known but intentionally unconsumed, e.g. grid.crs).
   type type_visited_set
      integer :: n = 0
      character(MESSAGE_SIZE), allocatable :: paths(:)
   end type type_visited_set

   type type_yaml_reader
      logical :: is_io_node = .False.
      ! --validate aborts on unread keys; runtime only warns
      logical :: unread_strict = .false.
      character(MESSAGE_SIZE):: path = ""
      type(type_log_writer), pointer, public :: log
      type(type_comm), pointer, public :: comm
      class(type_dictionary), pointer :: root => null()
      type(type_visited_set), pointer :: visited => null()
      type(YamlFile):: file

   contains

      procedure, public :: init
      procedure, public :: has_key
      procedure, public :: finalize
      procedure, public :: transfer_ownership
      procedure, public :: clone
      procedure, public :: mark_reserved
      procedure :: mark_read
      procedure :: report_unread

      procedure :: parse_error_message
      procedure :: prep_msg
      procedure :: prep_msg_val
      procedure :: prep_extern_msg
      procedure :: sanitize_path

      procedure, public :: cast_dictionary
      procedure, public :: cast_dictionary_list
      procedure, public :: is_dictionary
      procedure, public :: is_dictionary_node
      procedure, public :: read_time

      ! Integers (the *_node names alias the same implementations: reads
      ! became rank-local at the serial-read change, so the split is gone)
      procedure, public :: read_integer
      procedure, public :: read_integer_node => read_integer
      procedure, public :: read_positive_integer
      procedure, public :: read_negative_integer
      procedure, public :: read_nonnegative_integer
      procedure, public :: read_nonpositive_integer

      ! Real
      procedure, public :: read_real
      procedure, public :: read_real_node => read_real
      procedure, public :: read_positive_real
      procedure, public :: read_negative_real
      procedure, public :: read_nonnegative_real
      procedure, public :: read_nonpositive_real

      ! Bool/Logical
      procedure, public :: read_logical
      procedure, public :: read_logical_node => read_logical

      ! Strings
      procedure, public :: read_string
      procedure, public :: read_string_node => read_string
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

      character(:), allocatable :: err, content
      class(type_node), pointer :: root
      type(type_path) :: path
      character(MESSAGE_SIZE) :: message
      integer :: unit_, n

      log_buff = comm%get_logger(log_label)
      this%log => log_buff
      this%comm => comm

      ! Serial read + bcast of the RAW BYTES, then every rank parses its own
      ! tree straight from the buffer.  One shared-filesystem read instead of
      ! N (the metadata storm at O(10^3+) ranks), and every rank owns a full
      ! tree -- parsing on the io node alone left null roots on the other
      ! ranks, and any structural query segfaulted.
      if (comm%is_io_node()) then
         path = type_path(path_str)

         if (.not. path%is_file()) then
            message = "Input file does not exists, got: "//trim(path_str)
            call this%log%exit_on_error(message)
         end if

         if (path%file_size() .eq. 0) then
            message = "Input file is empty: "//trim(path_str)
            call this%log%exit_on_error(message)
         end if

         n = path%file_size()
         allocate (character(n) :: content)
         open (newunit=unit_, file=path_str, access="stream", form="unformatted", status="old")
         read (unit_) content
         close (unit_)
      end if
      call comm%bcast(content)

      call this%file%parse_string(content, err)

      if (allocated(err)) then
         call this%log%exit_on_error(err)
      end if

      root => this%file%root
      select type (root)
      class is (type_dictionary)
         this%root => root

      class default
         call this%log%exit_on_error("Input file does not appear to be a valid YAML file.")
      end select

      allocate (this%visited)
      allocate (this%visited%paths(64))

   end subroutine init

   function clone(this, node) result(reader)
      class(type_yaml_reader), intent(in) :: this
      class(type_dictionary), target, intent(in) :: node
      type(type_yaml_reader) :: reader

      reader%log => this%log
      reader%comm => this%comm
      reader%root => node
      reader%visited => this%visited
      reader%unread_strict = this%unread_strict
   end function clone

   function is_dictionary_node(this, key, silent) result(val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      logical, optional, intent(out) :: silent
      logical :: val

      class(type_node), pointer :: node
      character(:), allocatable :: buff

      call this%mark_read(key)
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
      call this%mark_read(key)
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

      call this%mark_read(key)
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

   !> Cast key to a list of dictionary readers: a mapping yields one child,
   !> a sequence of mappings one child per item — list-shaped sections
   !> (e.g. wavemaker:) accept both spellings transparently.
   !
   ! A subroutine, not a function returning the array: for an ALLOCATABLE
   ! array result of a finalizable type, gfortran emits the deallocation of
   ! the assignment temporary BEFORE its finalizer (verified in
   ! -fdump-tree-original, gfortran 15.2 and 16.1 alike), so
   ! "entries = reader%cast_dictionary_list(...)" ran YamlFile_final over
   ! freed memory -- a heap-use-after-free AddressSanitizer flags on macOS
   ! and the Windows heap turns into a SIGSEGV.  Returning through an
   ! intent(out) dummy builds no temporary and so has nothing to mis-order.
   subroutine cast_dictionary_list(this, key, is_empty, children)
      class(type_yaml_reader), intent(in) :: this
      character(*), intent(in) :: key
      logical, intent(out) :: is_empty
      type(type_yaml_reader), allocatable, intent(out) :: children(:)

      class(type_node), pointer :: node
      class(type_list), pointer :: list_node
      type(type_list_item), pointer :: item
      integer :: i

      call this%mark_read(key)
      node => this%root%get(key)
      is_empty = .not. associated(node)
      if (is_empty) then
         allocate (children(0))
         return
      end if

      select type (node)
      class is (type_dictionary)
         allocate (children(1))
         children(1) = this%clone(node)
      class is (type_list)
         list_node => node
         allocate (children(list_node%size()))
         i = 1
         item => list_node%first
         do while (associated(item))
            select type (node_item => item%node)
            class is (type_dictionary)
               children(i) = this%clone(node_item)
            class default
               call this%log%exit_on_error('Key "'//trim(key)// &
                                           '" list items must be mappings.')
            end select
            i = i + 1
            item => item%next
         end do
      class default
         call this%log%exit_on_error('Key "'//trim(key)// &
                                     '" must be a mapping or a sequence of mappings.')
      end select
   end subroutine cast_dictionary_list

   !----------------------------------------------------------------------
   ! Typed read implementations, expanded from the retired yaml_body.inc
   ! template: one body per type, sign-constraint wrappers for the numeric
   ! types; the *_node names bind to the same implementations
   !----------------------------------------------------------------------

   subroutine read_integer(this, key, default, silent, required_range, recommend_range, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      character(*), optional, intent(in) :: required_range, recommend_range
      ! NOTE: intent(inout), never intent(out) -- a silent miss must leave the
      ! caller's initializer untouched (intent(out) wiping it is the trap that
      ! bit slope_cap, nx_proc, and the gamma overrides)
      integer, intent(inout) :: val
      logical, optional, intent(out) :: silent

      type(type_integer_range) :: rq_range, rd_range
      type(type_error), allocatable :: io_err
      logical :: is_default
      character(len=:), allocatable :: msg

      if (present(required_range)) then
         rq_range = type_integer_range(required_range)
         if (.not. rq_range%is_valid()) then
            msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'required_range': "//required_range//"."
            call this%log%exit_on_fatal(msg)
         end if
      end if

      if (present(recommend_range)) then
         rd_range = type_integer_range(recommend_range)
         if (.not. rd_range%is_valid()) then
            msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'recommend_range': "//recommend_range//"."
            call this%log%exit_on_fatal(msg)
         end if
      end if

      ! Pre-check key existence before the typed getter: it assigns its result
      ! even on a missing key (wiping val with garbage), and on ifx the
      ! allocatable error dummy may stay unallocated through the
      ! fortran-yaml-c call chain
      call this%mark_read(key)
      if (associated(this%root%get(key))) then
         val = this%root%get_integer(key, error=io_err)
      else
         allocate (io_err)
         io_err%message = trim(this%root%path)//' does not contain key "'//trim(key)//'".'
      end if
      is_default = this%parse_error_message(key, io_err, default, silent)

      if (is_default) then
         ! Validate the default string with str2int directly: both gfortran
         ! and ifx silently partially-parse "10.5" into an integer (ios=0),
         ! so the library getter cannot be relied upon here
         block
            integer :: ios_dflt
            call str2int(default, val, ios_dflt)
            if (ios_dflt /= 0) then
               msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'default': "//default//"."
               call this%log%exit_on_fatal(msg)
            end if
         end block
      end if

      if (rq_range%is_set) then
         if (.not. rq_range%in_range(val)) then
            msg = "which is out of the required range: "//required_range//"."
            call this%log%exit_on_error(this%prep_msg_val(key, msg))
         end if
      end if

      if (rd_range%is_set) then
         if (.not. rd_range%in_range(val)) then
            msg = "which is out of the recommended range: "//recommend_range//"."
            call this%log%warning(this%prep_msg_val(key, msg))
         end if
      end if

   end subroutine read_integer

   subroutine read_positive_integer(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      integer, intent(inout) :: val

      call this%read_integer(key, default=default, silent=silent, val=val)

      ! A silent miss with no default leaves val untouched -- nothing to check
      if (present(silent) .and. .not. present(default)) then
         if (silent) return
      end if

      if (.not. (val > 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be positive."))
      end if

   end subroutine read_positive_integer

   subroutine read_negative_integer(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      integer, intent(inout) :: val

      call this%read_integer(key, default=default, silent=silent, val=val)

      if (present(silent) .and. .not. present(default)) then
         if (silent) return
      end if

      if (.not. (val < 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be negative."))
      end if

   end subroutine read_negative_integer

   subroutine read_nonnegative_integer(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      integer, intent(inout) :: val

      call this%read_integer(key, default=default, silent=silent, val=val)

      if (present(silent) .and. .not. present(default)) then
         if (silent) return
      end if

      if (.not. (val >= 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be non-negative."))
      end if

   end subroutine read_nonnegative_integer

   subroutine read_nonpositive_integer(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      integer, intent(inout) :: val

      call this%read_integer(key, default=default, silent=silent, val=val)

      if (present(silent) .and. .not. present(default)) then
         if (silent) return
      end if

      if (.not. (val <= 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be non-positive."))
      end if

   end subroutine read_nonpositive_integer

   subroutine read_real(this, key, default, silent, required_range, recommend_range, dim, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      character(*), optional, intent(in) :: required_range, recommend_range
      character(*), optional, intent(in) :: dim
      real(SP), intent(inout) :: val
      logical, optional, intent(out) :: silent

      type(type_real_range) :: rq_range, rd_range
      type(type_error), allocatable :: io_err
      logical :: is_default
      character(len=:), allocatable :: msg
      character(:), allocatable :: unit, err
      type(type_yaml_reader) :: child
      character(16), allocatable :: units(:)
      logical :: has_dict, no_node

      if (present(required_range)) then
         rq_range = type_real_range(required_range)
         if (.not. rq_range%is_valid()) then
            msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'required_range': "//required_range//"."
            call this%log%exit_on_fatal(msg)
         end if
      end if

      if (present(recommend_range)) then
         rd_range = type_real_range(recommend_range)
         if (.not. rd_range%is_valid()) then
            msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'recommend_range': "//recommend_range//"."
            call this%log%exit_on_fatal(msg)
         end if
      end if

      has_dict = .false.
      if (present(dim)) has_dict = this%is_dictionary_node(key, silent=no_node)
      if (has_dict) then

         child = this%cast_dictionary(key)

         call get_units_by_dim(dim, units, err)

         if (allocated(err)) then
            call this%log%exit_on_fatal(this%prep_msg(key, err))
            return ! Testing bypass
         end if

         call child%read_enum_node("units", units, val=unit)
         if (.not. associated(child%root%get("value"))) then
            call this%log%exit_on_error(this%prep_msg(key, " unit dictionary requires a 'value' key."))
         end if
         ! raw temp: apply_unit_conversion's in/out args must not alias (the
         ! intent(out) undefines val before the intent(in) copy is read --
         ! latent in the old template, expressed by the flag reorg)
         block
            real(SP) :: raw
            raw = child%root%get_real("value", error=io_err)
            call apply_unit_conversion(raw, unit, dim, val, err)
         end block

         if (allocated(err)) then
            ! NOTE: Bypass for testing
            ! 'get_units_by_dim' guards 'apply_unit_conversion'
            return
         end if

         if (allocated(unit)) deallocate (unit)

         is_default = .false.
         if (present(silent)) silent = .false.
      else
         call this%mark_read(key)
         if (associated(this%root%get(key))) then
            val = this%root%get_real(key, error=io_err)
         else
            allocate (io_err)
            io_err%message = trim(this%root%path)//' does not contain key "'//trim(key)//'".'
         end if
         is_default = this%parse_error_message(key, io_err, default, silent)
      end if

      if (is_default) then
         block
            integer :: ios_dflt
            call str2real(default, val, ios_dflt)
            if (ios_dflt /= 0) then
               msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'default': "//default//"."
               call this%log%exit_on_fatal(msg)
            end if
         end block
      end if

      if (rq_range%is_set) then
         if (.not. rq_range%in_range(val)) then
            msg = "which is out of the required range: "//required_range//"."
            call this%log%exit_on_error(this%prep_msg_val(key, msg))
         end if
      end if

      if (rd_range%is_set) then
         if (.not. rd_range%in_range(val)) then
            msg = "which is out of the recommended range: "//recommend_range//"."
            call this%log%warning(this%prep_msg_val(key, msg))
         end if
      end if

   end subroutine read_real

   subroutine read_positive_real(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      real(SP), intent(inout) :: val

      call this%read_real(key, default=default, silent=silent, val=val)

      if (present(silent) .and. .not. present(default)) then
         if (silent) return
      end if

      if (.not. (val > 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be positive."))
      end if

   end subroutine read_positive_real

   subroutine read_negative_real(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      real(SP), intent(inout) :: val

      call this%read_real(key, default=default, silent=silent, val=val)

      if (present(silent) .and. .not. present(default)) then
         if (silent) return
      end if

      if (.not. (val < 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be negative."))
      end if

   end subroutine read_negative_real

   subroutine read_nonnegative_real(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      real(SP), intent(inout) :: val

      call this%read_real(key, default=default, silent=silent, val=val)

      if (present(silent) .and. .not. present(default)) then
         if (silent) return
      end if

      if (.not. (val >= 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be non-negative."))
      end if

   end subroutine read_nonnegative_real

   subroutine read_nonpositive_real(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      real(SP), intent(inout) :: val

      call this%read_real(key, default=default, silent=silent, val=val)

      if (present(silent) .and. .not. present(default)) then
         if (silent) return
      end if

      if (.not. (val <= 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be non-positive."))
      end if

   end subroutine read_nonpositive_real

   subroutine read_logical(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, intent(inout) :: val
      logical, optional, intent(out) :: silent

      type(type_error), allocatable :: io_err
      logical :: is_default
      character(len=:), allocatable :: msg

      call this%mark_read(key)
      if (associated(this%root%get(key))) then
         val = this%root%get_logical(key, error=io_err)
      else
         allocate (io_err)
         io_err%message = trim(this%root%path)//' does not contain key "'//trim(key)//'".'
      end if
      is_default = this%parse_error_message(key, io_err, default, silent)

      if (is_default) then
         ! parse_error_message wrote the default into the tree; re-read it
         ! through the typed getter so it is validated the same way
         val = this%root%get_logical(key, error=io_err)
         if (allocated(io_err)) then
            msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'default': "//default//"."
            call this%log%exit_on_fatal(msg)
         end if
      end if

   end subroutine read_logical

   subroutine read_string(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      character(:), allocatable, intent(inout) :: val
      logical, optional, intent(out) :: silent

      type(type_error), allocatable :: io_err
      logical :: is_default
      character(len=:), allocatable :: msg

      call this%mark_read(key)
      if (associated(this%root%get(key))) then
         val = this%root%get_string(key, error=io_err)
      else
         allocate (io_err)
         io_err%message = trim(this%root%path)//' does not contain key "'//trim(key)//'".'
      end if
      is_default = this%parse_error_message(key, io_err, default, silent)

      if (is_default) then
         val = this%root%get_string(key, error=io_err)
         if (allocated(io_err)) then
            msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'default': "//default//"."
            call this%log%exit_on_fatal(msg)
         end if
      end if

   end subroutine read_string

   subroutine read_time(this, key, default, silent, val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      real(SP), intent(inout) :: val
      type(type_yaml_reader) :: child
      character(:), allocatable :: unit
      character(len=5), dimension(4) :: utypes
      logical :: missed
      data utypes/"sec", "min", "hour", "hertz"/
      if (this%is_dictionary(key)) then
         child = this%cast_dictionary(key)
         call child%read_enum("units", utypes, val=unit)
         call child%read_positive_real("value", default=default, silent=silent, val=val)
         ! Convert only a value actually read -- a silent miss with no default
         ! leaves val as the caller's initializer, which must not be rescaled
         missed = .false.
         if (present(silent) .and. .not. present(default)) missed = silent
         if (.not. missed) then
            select case (unit)
            case ("min")
               val = val*60_SP
            case ("hour")
               val = val*3600_SP
            case ("hertz")
               val = 1.0_SP/val
            end select
         end if
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
      logical :: is_found, missed
      character(len=:), allocatable :: msg
      call this%read_string_node(key, default, silent, val)
      ! A silent miss with no default leaves val untouched -- nothing to check
      missed = .false.
      if (present(silent) .and. .not. present(default)) missed = silent
      if (missed) return
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
            msg = msg//", "//trim(values(i))
         end do
         msg = msg//", & "//trim(values(len))//"."
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

      ! Forward `silent` only when the caller passed it: always handing our
      ! local to read_string made every missing enum non-fatal, required or not
      p_silent = .false.
      if (present(silent)) then
         call this%read_string(key, default=default, silent=silent, val=val)
         p_silent = silent .and. .not. present(default)
      else
         call this%read_string(key, default=default, val=val)
      end if
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
               msg = msg//", "//trim(values(i))
            end do
            msg = msg//", & "//trim(values(n))//"."
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
      if (present(silent)) then
         call this%read_string(key, default=default, silent=silent, val=val_buff)
         p_silent = silent .and. .not. present(default)
      else
         call this%read_string(key, default=default, val=val_buff)
      end if
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
      ! `silent` reports key absence in EVERY branch, orthogonal to whether a
      ! default filled the value -- leaving it unwritten when default is also
      ! present was the bug that deadened the flag at ~90 combo call sites
      if (present(silent)) silent = .false.
      if (.not. allocated(io_err)) then
         is_default = .false.
         buff = " read value '"//this%root%get_string(key, error=io_err)//"'."
         call this%log%debug(this%prep_msg(key, buff))
         return
      end if
      if (present(default)) then
         if (is_no_key_err(io_err)) then
            is_default = .true.
            if (present(silent)) silent = .true.
            buff = " not found, using default value "//trim(default)//"."
            call this%log%info(this%prep_msg(key, buff))
            call this%root%set_string(key, default)
            node => this%root%get(key)
            node%path = this%root%path//"/"//key
            ! the injected default is a new tree node -- mark it or every
            ! defaulted key walks as a false-positive unread leaf
            call this%mark_read(key)
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
         if (is_no_key_err(io_err)) then
            ! pre-check no-key messages lack the /key prefix prep_extern_msg
            ! strips (it mangled them) -- a required key reads better anyway
            buff = this%sanitize_path(key)//" is required."
         else
            buff = this%prep_extern_msg(key, io_err%message)
         end if
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
         if (new_path(1:1) == ".") then
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

      call this%mark_read(key)
      node => this%root%get(key)
      if (.not. associated(node)) then
         if (present(silent)) then
            silent = .true.
         else
            call this%log%exit_on_error(trim(this%root%path)//' does not contain key "'//trim(key)//'".')
         end if
         return
      end if
      if (present(silent)) silent = .false.
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

      call this%mark_read(key)
      node => this%root%get(key)
      if (.not. associated(node)) then
         if (present(silent)) then
            silent = .true.
         else
            call this%log%exit_on_error(trim(this%root%path)//' does not contain key "'//trim(key)//'".')
         end if
         return
      end if
      if (present(silent)) silent = .false.
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

      call this%mark_read(key)
      node => this%root%get(key)
      if (.not. associated(node)) then
         if (present(silent)) then
            silent = .true.
         else
            call this%log%exit_on_error(trim(this%root%path)//' does not contain key "'//trim(key)//'".')
         end if
         return
      end if
      if (present(silent)) silent = .false.
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
   end subroutine read_real_array

   !----------------------------------------------------------------------
   ! Unread-key detection machinery.  mark_read stamps the fetched node's
   ! parse path into the shared visited set; mark_reserved stamps a "/*"
   ! wildcard covering a schema-known but intentionally-unconsumed subtree;
   ! report_unread (finalize) walks the tree leaves and reports the rest:
   ! runtime = io-node warning block, unread_strict (--validate) = abort.
   !----------------------------------------------------------------------

   subroutine visited_add(set, path)
      type(type_visited_set), intent(inout) :: set
      character(*), intent(in) :: path
      character(MESSAGE_SIZE), allocatable :: tmp(:)
      integer :: i

      do i = 1, set%n
         if (trim(set%paths(i)) == path) return
      end do
      if (set%n == size(set%paths)) then
         allocate (tmp(2*set%n))
         tmp(1:set%n) = set%paths
         call move_alloc(tmp, set%paths)
      end if
      set%n = set%n + 1
      set%paths(set%n) = path
   end subroutine visited_add

   function visited_covers(set, path) result(covered)
      type(type_visited_set), intent(in) :: set
      character(*), intent(in) :: path
      logical :: covered
      integer :: i, n

      covered = .true.
      do i = 1, set%n
         if (trim(set%paths(i)) == path) return
         n = len_trim(set%paths(i))
         if (n >= 2) then
            if (set%paths(i) (n - 1:n) == "/*" .and. len(path) >= n - 1) then
               if (path(1:n - 1) == set%paths(i) (1:n - 1)) return
            end if
         end if
      end do
      covered = .false.
   end function visited_covers

   subroutine mark_read(this, key)
      class(type_yaml_reader), intent(in) :: this
      character(*), intent(in) :: key
      class(type_node), pointer :: node

      if (.not. associated(this%visited)) return
      if (.not. associated(this%root)) return
      node => this%root%get(key)
      if (.not. associated(node)) return
      if (.not. allocated(node%path)) return
      call visited_add(this%visited, node%path)
   end subroutine mark_read

   subroutine mark_reserved(this, key)
      class(type_yaml_reader), intent(in) :: this
      character(*), intent(in) :: key
      class(type_node), pointer :: node

      if (.not. associated(this%visited)) return
      if (.not. associated(this%root)) return
      node => this%root%get(key)
      if (.not. associated(node)) return
      if (.not. allocated(node%path)) return
      call visited_add(this%visited, trim(node%path)//"/*")
   end subroutine mark_reserved

   recursive subroutine collect_unread(node, set, unread)
      class(type_node), intent(in) :: node
      type(type_visited_set), intent(in) :: set
      type(type_visited_set), intent(inout) :: unread

      type(type_key_value_pair), pointer :: pair
      type(type_list_item), pointer :: item
      logical :: all_scalar

      select type (node)
      class is (type_dictionary)
         pair => node%first
         do while (associated(pair))
            call collect_unread(pair%value, set, unread)
            pair => pair%next
         end do
      class is (type_list)
         ! a scalar-only list is one leaf at the list's own path (array
         ! reads mark the list node, not its items)
         all_scalar = .true.
         item => node%first
         do while (associated(item))
            select type (n_ => item%node)
            class is (type_scalar)
            class default
               all_scalar = .false.
            end select
            item => item%next
         end do
         if (all_scalar) then
            if (allocated(node%path)) then
               if (.not. visited_covers(set, node%path)) &
                  call visited_add(unread, node%path)
            end if
         else
            item => node%first
            do while (associated(item))
               call collect_unread(item%node, set, unread)
               item => item%next
            end do
         end if
      class default
         if (allocated(node%path)) then
            if (.not. visited_covers(set, node%path)) &
               call visited_add(unread, node%path)
         end if
      end select
   end subroutine collect_unread

   subroutine report_unread(this)
      class(type_yaml_reader), intent(inout) :: this
      type(type_visited_set) :: unread
      character(MESSAGE_SIZE) :: msg
      integer :: i

      if (.not. associated(this%visited)) return
      if (.not. associated(this%root)) return
      allocate (unread%paths(16))
      call collect_unread(this%root, this%visited, unread)
      if (unread%n == 0) return

      if (this%comm%is_io_node()) then
         call this%log%warning("config keys read by no component -- misplaced,"// &
                               " misspelled, or inapplicable to this configuration:")
         do i = 1, unread%n
            call this%log%warning("  "//trim(unread%paths(i)))
         end do
      end if
      if (this%unread_strict) then
         write (msg, "(a,i0,a)") "validation failed -- ", unread%n, &
            " config key(s) read by no component (list above)"
         call this%log%exit_on_error(trim(msg))
      end if
   end subroutine report_unread

   subroutine finalize(this)
      class(type_yaml_reader), intent(inout) :: this
      if (associated(this%root)) then
         call this%report_unread()
         call this%root%finalize()
         nullify (this%root)
      end if
      if (associated(this%visited)) deallocate (this%visited)
   end subroutine finalize

   ! Transfer ownership of the YAML tree to another yaml_reader that was
   ! created by value-copying this one (e.g. this%env = env).  Nullifies
   ! this%file%root so that YamlFile_final won't double-free the root node
   ! when both copies eventually go out of scope.  Call immediately after
   ! the value copy, on the SOURCE object.
   subroutine transfer_ownership(this)
      class(type_yaml_reader), intent(inout) :: this
      nullify (this%file%root)
   end subroutine transfer_ownership

end module core_yaml_file_mod
