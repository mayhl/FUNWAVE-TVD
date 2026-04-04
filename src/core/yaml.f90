!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Class for integrating external YAML reader with logging
!  and error handling.
!
!  PURPOSE:
!   - validating input
!   - error handling
!     - optional default handling
!   - casting raw inputs
!
!  HISTORY:
!    11/23/2025  Michael-Angelo Y.H. Lam
!
!  MAIN METHODS:
!   type_yaml_reader
!
!     init - initialize class and open YAML file
!     cast_dictionary - read dictionary in YAML file and cast
!                       to current class with log/error handling
!
!     read_integer - read integer by key name with basic type checking
!     read_real - read real number by key name with basic type checking
!     read_logical - read logical by key name with basic type checking
!     read_string - read string by key name with basic type checking
!     read_enum - read string by key name and validate against list
!     read_file - read path like string and validate file
!     read_folder read path like string and validate folder
!
!
!     Arguments
!
!     read_real/real_integer/read_logical/read_string/read_enum
!
!     Required
!       key - Key of value to read in current dictionary
!       val - Variable to read YAML value into
!             Note: Use key explicitly, e.g.,
!                     call read_real('key', val=val)
!
!     Optional
!       default - Default value as string.
!       is_empty - Silence no key error and return if key is found
!
!     read_real/read_integer
!
!     Extra Optional
!       range - range of valid values, throw error if out of bounds
!       inclusive - logical to include range ends in check
!                   Default: [.true., .true.]
!       recommended_range - same as range, but replaces error with warning
!       recommended_inclusive - same as inclusive, but for recommended_range
!                               Default: [.true., .true.]
!
!     read_enum
!
!     Extra Argument
!       values - list of value string values for enumeration
!--------------------------------------------------
module yaml_file_mod

   use constants_mod, only: MESSAGE_SIZE, STRING_SIZE, LABEL_SIZE, SP
   use log_io_mod, only: type_log_writer
   use comm_mod, only: type_comm
   use misc_mod, only: str2int, str2real
   use filesystem, only: type_path => path_t

   !use Constants, only: STRING_SIZE
   use fortran_yaml_c, only: YamlFile, dp, &
                             type_node, type_dictionary, type_error, &
                             type_list, type_list_item, type_scalar
   implicit none(external)

   private
   public :: type_yaml_reader, err_key, ERR_NONE, ERR_TYPE, ERR_RANGE, type_path

   character(*), parameter:: NO_KEY_ERR = "does not contain key"
   integer, parameter :: ERR_NONE = 0
   integer, parameter :: err_key = 1
   integer, parameter :: ERR_TYPE = 2
   integer, parameter :: ERR_RANGE = 3
   type(type_log_writer), TARGET :: log_buff

   character(LABEL_SIZE), parameter :: log_label = "config"

   type type_yaml_reader
      private
      logical :: is_io_node = .False.
      character(MESSAGE_SIZE):: path = ""
      type(type_log_writer), pointer, public :: log
      type(type_comm), pointer, public :: comm
      class(type_dictionary), pointer :: root => null()
      type(YamlFile):: file

   contains

      private
      procedure, public :: init
      procedure, public :: cast_dictionary
      procedure, public :: is_dictionary
      procedure, public :: has_key
      procedure, public :: read_enum
      procedure, public :: read_input_path
      procedure, public :: read_integer
      procedure, public :: read_logical
      procedure, public :: read_positive_integer
      procedure, public :: read_positive_real
      procedure, public :: read_real
      procedure, public :: read_time
      procedure, public :: read_string
      procedure :: copy_node
      procedure :: parse_error_message
      procedure :: prep_msg
      procedure :: prep_msg_val
      procedure :: read_integer_node
      procedure :: read_logical_node
      procedure :: read_real_node
      procedure :: read_string_node

   end type type_yaml_reader

   type type_range

      private
      character(:), allocatable :: key
      character(:), allocatable :: prefix
      logical :: is_recommended, is_skip_checks
      logical, dimension(2) :: is_inclusive
      logical, dimension(2) :: is_out_bound
      logical, dimension(2) :: is_empty
      character(24), dimension(2) :: range_str(2)
      integer, dimension(2) :: iostat
      type(type_yaml_reader), pointer :: reader

      logical :: is_range_err, is_zero
   contains
      private
      procedure, public :: initialize => range_initialize
      procedure :: finalize => range_finalize
      procedure :: range2str => range_range2str

   end type type_range

   type, extends(type_range) :: type_range_integer
      private
      integer, dimension(2) :: range
   contains
      private
      procedure, public :: parse => range_integer_parse
      procedure, public :: intersects => range_integer_intersects

   end type type_range_integer

   type, extends(type_range) :: type_range_real
      private
      real(SP), dimension(2) :: range
   contains
      private
      procedure, public :: parse => range_real_parse
      procedure, public :: intersects => range_real_intersects

   end type type_range_real
contains
   function range_initialize(this, reader, key, is_recommended, range, inclusive) result(is_error_exit)
      !----------------------------------------------------------

      class(type_range), intent(inout) :: this
      class(type_yaml_reader), intent(inout), target :: reader
      character(*), intent(in) :: key
      logical, intent(in) :: is_recommended
      character(*), dimension(2), optional, intent(in) :: range
      logical, dimension(2), optional, intent(in) :: inclusive

      logical :: is_error_exit
      character(:), allocatable :: msg, prefix

      this%range_str = range
      this%is_recommended = is_recommended
      this%key = key
      this%reader => reader

      if (is_recommended) then
         this%prefix = "recommended_"
      else
         this%prefix = ""
      end if

      if (.not. present(range)) then
         if (present(inclusive)) then
            msg = " can not specify "//this%prefix//"inclusive without also specifying "//this%prefix//"range."
            call reader%log%exit_on_fatal(reader%prep_msg(key, msg))
            is_error_exit = .true.
         else
            is_error_exit = .false.
         end if

         this%is_skip_checks = .true.
         return
      end if

      if (present(inclusive)) then
         this%is_inclusive = inclusive
      else
         this%is_inclusive = [.true., .true.]
      end if
      this%is_skip_checks = .false.
      is_error_exit = .false.

   end function range_initialize

   function range_range2str(this) result(range)
      !----------------------------------------------------------
      class(type_range), intent(inout) :: this
      character :: left, right

      character(:), allocatable :: range

      ! Custom messages for lower and upper bound
      ! Using () brackets for exclusive bounds &
      !       [] brackets for inclusive bounds
      if (this%is_inclusive(1)) then
         left = '['
      else
         left = '('
      end if

      if (this%is_inclusive(2)) then
         right = ']'
      else
         right = ')'
      end if

      range = left//trim(this%range_str(1))//", "//trim(this%range_str(2))//right

   end function range_range2str

   subroutine range_finalize(this, type_noun)
      !----------------------------------------------------------

      class(type_range), intent(inout) :: this
      character(*), intent(in) :: type_noun

      character(:), allocatable :: msg, prefix
      character :: left, right

      if (this%is_range_err) then
         msg = " "//this%range2str()//" is not a valid "//this%prefix//"range."
      end if

      if ((.not. this%is_empty(1)) .and. &
          (this%iostat(1) /= 0)) then
         msg = " lower value of "//this%prefix//"range, '"//trim(this%range_str(1))//"' can not be interperted as "//type_noun//"."
      end if

      if ((.not. this%is_empty(2)) .and. &
          (this%iostat(2) /= 0)) then
         msg = " upper value of "//this%prefix//"range, '"//trim(this%range_str(2))//"' can not be interperted as "//type_noun//"."
      end if

      if (allocated(msg)) then
         msg = this%reader%prep_msg(this%key, msg)
         call this%reader%log%exit_on_fatal(msg)
         deallocate (msg)
         return
      end if

      ! Exiting on no out of bounds errors
      if (.not. (this%is_out_bound(1) .or. this%is_out_bound(2))) then
         return
      end if

      if (this%is_recommended) then
         this%prefix = 'recommmend'
      else
         this%prefix = 'required'
      end if

      if ((.not. this%is_empty(1)) .and. (.not. this%is_empty(2))) then

         msg = "which is not in the "//this%prefix//" range "//this%range2str()//"."

      else if (.not. this%is_empty(1)) then
         ! Custom messages for only lower bound
         if (this%is_inclusive(1)) then
            if (this%is_zero) then
               msg = "zero"
            else
               msg = this%range_str(1)
            end if
            msg = "which is "//this%prefix//" to be greater than or equal to "//msg//"."
         else
            if (this%is_zero) then
               msg = "which is "//this%prefix//" to be a positive number."
            else
               msg = "which is "//this%prefix//" to be greater than "//this%range_str(1)//"."
            end if
         end if

      else if (.not. this%is_empty(2)) then
         ! Custom messages for only upper bound
         if (this%is_inclusive(2)) then
            if (this%is_zero) then
               msg = "zero"
            else
               msg = this%range_str(2)
            end if
            msg = " is "//this%prefix//" to be less than or equal to "//msg//"."
         else
            if (this%is_zero) then
               msg = "which is "//this%prefix//" to be a negative number."
            else
               msg = "which is "//this%prefix//" to be less than "//this%range_str(2)//"."
            end if
         end if

      else
         ! Safety check, code should not be reached
         msg = " in an unexepected state."
         msg = this%reader%prep_msg(this%key, msg)
         call this%reader%log%exit_on_fatal(msg)

      end if

      msg = this%reader%prep_msg_val(this%key, msg)
      if (this%is_recommended) then
         call this%reader%log%warning(msg)
      else
         call this%reader%log%exit_on_error(msg)
      end if

   end subroutine range_finalize

   subroutine range_integer_parse(this, val)
      !----------------------------------------------------------
      class(type_range_integer), intent(inout) :: this
      integer, intent(in) :: val

      if (this%is_skip_checks) return

      call str2int(this%range_str, this%range, this%iostat, is_empty=this%is_empty)

      if (this%is_empty(1)) then
         this%is_out_bound(1) = .false.
      else
         if (this%iostat(1) == 0) then
            if (this%is_inclusive(1)) then
               this%is_out_bound(1) = val < this%range(1)
            else
               this%is_out_bound(1) = val <= this%range(1)
            end if
         else
            this%is_out_bound(1) = .true.
         end if
      end if

      if (this%is_empty(2)) then
         this%is_out_bound(2) = .false.
      else
         if (this%iostat(2) == 0) then
            if (this%is_inclusive(2)) then
               this%is_out_bound(2) = val > this%range(2)
            else
               this%is_out_bound(2) = val >= this%range(2)
            end if
         else
            this%is_out_bound(2) = .true.
         end if
      end if

      if ((.not. this%is_empty(1)) .and. &
          (.not. this%is_empty(2))) then

         this%is_range_err = this%range(2) <= this%range(1)
      else
         this%is_range_err = .false.
      end if
      ! Flag for custom error messages
      this%is_zero = (this%range(1) == 0) .or. (this%range(2) == 0)

      call this%finalize("an integer")

   end subroutine range_integer_parse

   subroutine range_integer_intersects(this, other)
      !----------------------------------------------------------
      class(type_range_integer), intent(inout) :: this
      class(type_range_integer), intent(inout) :: other

      character(:), allocatable :: msg
      logical, dimension(2) :: is_error

      is_error = [.false., .false.]
      if (this%is_skip_checks) return
      if (other%is_skip_checks) return

      if ((.not. this%is_empty(1)) .and. &
          (.not. other%is_empty(1))) then

         if (this%is_inclusive(1) .eqv. other%is_inclusive(1)) then
            is_error(1) = other%range(1) < this%range(1)
         else

            if (this%is_inclusive(1)) then
               is_error(1) = other%range(1) < this%range(1)
            else
               is_error(1) = other%range(1) <= this%range(1)
            end if

         end if

      end if

      if ((.not. this%is_empty(2)) .and. &
          (.not. other%is_empty(2))) then

         if (this%is_inclusive(2) .eqv. other%is_inclusive(2)) then
            is_error(2) = other%range(2) > this%range(2)
         else

            if (this%is_inclusive(2)) then
               is_error(2) = other%range(2) > this%range(2)
            else
               is_error(2) = other%range(2) >= this%range(2)
            end if

         end if

      end if

      if (is_error(1) .or. is_error(2)) then
         msg = " recommended_range, "//other%range2str()//", is not a subinterval of range, "//this%range2str()//"."
         msg = this%reader%prep_msg(this%key, msg)
         call this%reader%log%exit_on_fatal(msg)
      end if

   end subroutine range_integer_intersects

   subroutine range_real_parse(this, val)
      !----------------------------------------------------------
      class(type_range_real), intent(inout) :: this
      real(SP), intent(in) :: val

      if (this%is_skip_checks) return

      call str2real(this%range_str, this%range, this%iostat, is_empty=this%is_empty)

      if (this%is_empty(1)) then
         this%is_out_bound(1) = .false.
      else
         if (this%iostat(1) == 0) then
            if (this%is_inclusive(1)) then
               this%is_out_bound(1) = val < this%range(1)
            else
               this%is_out_bound(1) = val <= this%range(1)
            end if
         else
            this%is_out_bound(1) = .true.
         end if
      end if

      if (this%is_empty(2)) then
         this%is_out_bound(2) = .false.
      else
         if (this%iostat(2) == 0) then
            if (this%is_inclusive(2)) then
               this%is_out_bound(2) = val > this%range(2)
            else
               this%is_out_bound(2) = val >= this%range(2)
            end if
         else
            this%is_out_bound(2) = .true.
         end if
      end if

      if ((.not. this%is_empty(1)) .and. &
          (.not. this%is_empty(2))) then

         this%is_range_err = this%range(2) <= this%range(1)
      else
         this%is_range_err = .false.
      end if
      ! Flag for custom error messages
      this%is_zero = (this%range(1) == 0) .or. (this%range(2) == 0)

      call this%finalize("a real number")

   end subroutine range_real_parse

   subroutine range_real_intersects(this, other)
      !----------------------------------------------------------
      class(type_range_real), intent(inout) :: this
      class(type_range_real), intent(inout) :: other

      character(:), allocatable :: msg
      logical, dimension(2) :: is_error

      is_error = [.false., .false.]
      if (this%is_skip_checks) return
      if (other%is_skip_checks) return

      if ((.not. this%is_empty(1)) .and. &
          (.not. other%is_empty(1))) then

         if (this%is_inclusive(1) .eqv. other%is_inclusive(1)) then
            is_error(1) = other%range(1) < this%range(1)
         else

            if (this%is_inclusive(1)) then
               is_error(1) = other%range(1) < this%range(1)
            else
               is_error(1) = other%range(1) <= this%range(1)
            end if

         end if

      end if

      if ((.not. this%is_empty(2)) .and. &
          (.not. other%is_empty(2))) then

         if (this%is_inclusive(2) .eqv. other%is_inclusive(2)) then
            is_error(2) = other%range(2) > this%range(2)
         else

            if (this%is_inclusive(2)) then
               is_error(2) = other%range(2) > this%range(2)
            else
               is_error(2) = other%range(2) >= this%range(2)
            end if

         end if

      end if

      if (is_error(1) .or. is_error(2)) then
         msg = " recommended_range, "//other%range2str()//", is not a subinterval of range, "//this%range2str()//"."
         msg = this%reader%prep_msg(this%key, msg)
         call this%reader%log%exit_on_fatal(msg)
      end if

   end subroutine range_real_intersects

   subroutine init(this, path_str, comm)
      !----------------------------------------------------------
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

      if (.not. comm%is_io_node()) then
         this%root => null()
         return
      end if

      if (.not. path%is_file()) then
         message = "Input file does not exists, got: "//trim(path_str)
         call this%log%exit_on_error(message)
      end if

      if (path%file_size() .eq. 0) then
         message = "Input file is empty: "//trim(path_str)
         call this%log%exit_on_error(message)
      end if

      call this%file%parse(path%path(), err)

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

   function is_dictionary(this, key, is_empty) result(val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      logical, optional, intent(out) :: is_empty
      logical :: val

      class(type_node), pointer :: node
      character(:), allocatable :: buff

      if (this%comm%is_io_node()) then

         node => this%root%get(key)

         if (associated(node)) then
            select type (node)
            class is (type_dictionary)
               val = .true.
            class default
               val = .false.
            end select
            if (present(is_empty)) is_empty = .false.
         else
            if (present(is_empty)) then
               is_empty = .true.
               val = .false.
            else
               ! buff = trim(this%root%path)//' does not contain key "'//trim(key)//'".'
               call this%log%exit_on_error(key)
            end if

         end if
      end if

      call this%comm%bcast_logical(val)
      if (present(is_empty)) call this%comm%bcast_logical(is_empty)

   end function is_dictionary

   function has_key(this, key, is_empty) result(val)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      logical, optional, intent(out) :: is_empty
      logical :: val

      class(type_node), pointer :: node
      character(:), allocatable :: buff

      val = associated(this%root%get(key))

   end function has_key

   subroutine cast_dictionary(this, key, child, is_empty)
      !----------------------------------------------------------
      !
      ! Subroutine type casting child dictionary to current class
      !
      ! Note: Want to wrap logging & error checking but to lazy
      ! to code garbage handling and reimplement the yaml code
      !
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      type(type_yaml_reader), intent(inout) :: child
      logical, optional, intent(out) :: is_empty
      type(type_yaml_reader), target :: dummy
      type(type_dictionary), pointer :: node
      type(type_error), allocatable :: io_err

      logical, parameter :: is_required = .true.
      logical :: flag

      if (this%comm%is_io_node()) then
         node => this%root%get_dictionary(key, is_required, io_err)
      else
         node => null()
      end if

      if (.not. allocated(io_err)) then
         if (present(is_empty)) is_empty = .false.
      else
         if (present(is_empty)) then
            flag = .not. is_no_key_err(io_err)
            is_empty = .true.
         else
            flag = .true.
         end if
         if (flag) call this%log%exit_on_error(io_err%message)
      end if

      call child%copy_node(node, this%comm)

   end subroutine cast_dictionary

   subroutine copy_node(this, node, comm)
      !----------------------------------------------------------
      ! Mediator routine for sharing private variables
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      type(type_dictionary), target, intent(inout):: node
      type(type_comm), target, intent(inout):: comm
      this%log => log_buff
      this%root => node
      this%comm => comm
   end subroutine copy_node

   subroutine read_real_node(this, key, default, is_empty, &
                             range, inclusive, &
                             recommended_range, recommended_inclusive, &
                             val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      character(*), dimension(2), optional, intent(in) :: range, recommended_range
      logical, dimension(2), optional, intent(in) :: inclusive, recommended_inclusive
      real(SP), intent(out) :: val

      type(type_range_real) :: req_range, rec_range
      type(type_error), allocatable :: io_err
      logical :: is_default
      logical, dimension(2) :: is_err_exit

      val = this%root%get_real(key, error=io_err)
      is_default = this%parse_error_message(key, io_err, default, is_empty)

      ! Parsing default value string
      if (is_default) then
         val = this%root%get_real(key, error=io_err)
         ! Safety check
         if (allocated(io_err)) then
            call this%log%exit_on_fatal("Default value for "//io_err%message)
         end if
      end if

      is_err_exit(1) = req_range%initialize( &
                       this, &
                       key, &
                       is_recommended=.false., &
                       range=range, &
                       inclusive=inclusive)

      is_err_exit(2) = rec_range%initialize( &
                       this, &
                       key, &
                       is_recommended=.true., &
                       range=recommended_range, &
                       inclusive=recommended_inclusive)

      if (is_err_exit(1)) return
      call req_range%parse(val)
      if (is_err_exit(2)) return
      call rec_range%parse(val)

      call req_range%intersects(rec_range)
   end subroutine read_real_node

   subroutine read_real(this, key, default, is_empty, &
                        range, inclusive, &
                        recommended_range, recommended_inclusive, &
                        val)
      !----------------------------------------------------------
      ! Wrapper method around read_real_node for single node
      ! MPI I/O and broadcasting to other nodes
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      character(*), dimension(2), optional, intent(in) :: range, recommended_range
      logical, dimension(2), optional, intent(in) :: inclusive, recommended_inclusive
      real(SP), intent(out) :: val

      if (this%comm%is_io_node()) then
         call this%read_real_node(key, default, is_empty, &
                                  range, inclusive, &
                                  recommended_range, recommended_inclusive, &
                                  val)
      end if

      call this%comm%bcast_real(val)

   end subroutine read_real

   subroutine read_positive_real(this, key, default, is_empty, include_zero, &
                                 recommended_range, recommended_inclusive, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      logical, optional, intent(in) :: include_zero
      character(*), dimension(2), optional, intent(in) :: recommended_range
      logical, dimension(2), optional, intent(in) :: recommended_inclusive
      real(SP), intent(out) :: val

      logical :: is_error, is_include_zero
      character(len=:), allocatable :: message
      character(1), parameter, dimension(2) :: range = ["0", " "]
      logical, dimension(2) :: inclusive

      if (this%comm%is_io_node()) then

         ! Note: 2nd boolean is a dummy value
         if (present(include_zero)) then
            inclusive = [include_zero, .true.]
         else
            inclusive = [.false., .true.]
         end if

         call this%read_real_node(key, default, is_empty, &
                                  range, inclusive, &
                                  recommended_range, recommended_inclusive, &
                                  val)

      end if

      call this%comm%bcast_real(val)

   end subroutine read_positive_real

   subroutine read_time(this, key, default, is_empty, include_zero, &
                        recommended_range, recommended_inclusive, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      logical, optional, intent(in) :: include_zero
      character(*), dimension(2), optional, intent(in) :: recommended_range
      logical, dimension(2), optional, intent(in) :: recommended_inclusive
      real(SP), intent(out) :: val

      logical :: is_dict
      type(type_yaml_reader) :: child
      character(len=5), dimension(4) ::  utypes
      character(:), allocatable :: unit, ckey

      data utypes/'sec', 'min', 'hour', 'hertz'/

      is_dict = this%is_dictionary(key)
      if (is_dict) then
         unit = ""
         print *, "HERE"
         print *, unit
         call this%cast_dictionary(key, child)
         print *, "HERE"
         print *, unit
         call child%read_enum("units", utypes, val=unit)
         print *, "HERE"
         print *, unit
         ckey = 'value'
      else
         child = this
         ckey = key
      end if

      call child%read_positive_real(ckey, default, &
                                    is_empty, include_zero, &
                                    recommended_range, &
                                    recommended_inclusive, val)

      if (is_dict) then
         select case (unit)
         case ('min')
            val = val*60_SP
         case ('hour')
            val = val*3600_SP
         case ('hert')
            val = 1.0_SP/val
         end select

         deallocate (unit)
      end if

   end subroutine read_time

   subroutine read_integer_node(this, key, default, is_empty, &
                                range, inclusive, &
                                recommended_range, recommended_inclusive, &
                                val)
      !----------------------------------------------------------
      ! Read key from from root node and parse as integer
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      character(*), dimension(2), optional, intent(in) :: range, recommended_range
      logical, dimension(2), optional, intent(in) :: inclusive, recommended_inclusive
      integer, intent(out) :: val

      type(type_range_integer) :: req_range, rec_range
      type(type_error), allocatable :: io_err
      logical :: is_default
      logical, dimension(2) :: is_err_exit

      val = this%root%get_integer(key, error=io_err)
      is_default = this%parse_error_message(key, io_err, default, is_empty)

      ! Parsing default value string
      if (is_default) then

         val = this%root%get_integer(key, error=io_err)

         ! Safety check
         if (allocated(io_err)) then
            call this%log%exit_on_fatal("Default value for "//io_err%message, 1)
         end if
      end if

      is_err_exit(1) = req_range%initialize( &
                       this, &
                       key, &
                       is_recommended=.false., &
                       range=range, &
                       inclusive=inclusive)

      is_err_exit(2) = rec_range%initialize( &
                       this, &
                       key, &
                       is_recommended=.true., &
                       range=recommended_range, &
                       inclusive=recommended_inclusive)

      if (is_err_exit(1)) return
      call req_range%parse(val)
      if (is_err_exit(2)) return
      call rec_range%parse(val)

      call req_range%intersects(rec_range)

   end subroutine read_integer_node

   subroutine read_integer(this, key, default, is_empty, &
                           range, inclusive, &
                           recommended_range, recommended_inclusive, &
                           val)
      !----------------------------------------------------------
      !
      ! Wrapper subroutine around read_real_node for single node
      ! MPI I/O and broadcasting to other nodes
      !
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      character(*), dimension(2), optional, intent(in) :: range, recommended_range
      logical, dimension(2), optional, intent(in) :: inclusive, recommended_inclusive
      integer, intent(out) :: val

      if (this%comm%is_io_node()) then
         call this%read_integer_node(key, default, is_empty, &
                                     range, inclusive, &
                                     recommended_range, recommended_inclusive, &
                                     val)
      end if

      call this%comm%bcast_integer(val)

   end subroutine read_integer

   subroutine read_positive_integer(this, key, default, is_empty, include_zero, &
                                    recommended_range, recommended_inclusive, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      logical, optional, intent(in) :: include_zero
      character(*), dimension(2), optional, intent(in) :: recommended_range
      logical, dimension(2), optional, intent(in) :: recommended_inclusive
      integer, intent(out) :: val
      logical :: is_error, is_include_zero

      character(1), parameter, dimension(2) :: range = ["0", " "]
      logical, dimension(2) :: inclusive

      if (this%comm%is_io_node()) then

         ! Note: 2nd boolean is a dummy value
         if (present(include_zero)) then
            inclusive = [include_zero, .true.]
         else
            inclusive = [.false., .true.]
         end if

         call this%read_integer_node(key, default, is_empty, &
                                     range, inclusive, &
                                     recommended_range, recommended_inclusive, &
                                     val)

      end if

      call this%comm%bcast_integer(val)

   end subroutine read_positive_integer

   subroutine read_logical_node(this, key, default, is_empty, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      logical, intent(out) :: val

      type(type_error), allocatable :: io_err
      logical :: is_default

      val = this%root%get_logical(key, error=io_err)
      is_default = this%parse_error_message(key, io_err, default, is_empty)

      ! Parsing default value string
      if (is_default) then
         val = this%root%get_logical(key, error=io_err)

         ! Safety check
         if (allocated(io_err)) then
            call this%log%exit_on_fatal("Default value for /"//key//io_err%message, 1)
         end if
      end if

   end subroutine read_logical_node

   subroutine read_logical(this, key, default, is_empty, val)
      !----------------------------------------------------------
      !
      ! Wrapper subroutine around read_real_node for single node
      ! MPI I/O and broadcasting to other nodes
      !
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      logical, intent(out) :: val

      if (this%comm%is_io_node()) then
         call this%read_logical_node(key, default, is_empty, val)
      end if

      call this%comm%bcast_logical(val)

   end subroutine read_logical

   subroutine read_string_node(this, key, default, is_empty, val)
      !----------------------------------------------------------
      ! Read key from from root node and parse as integer
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      character(:), allocatable, intent(inout) :: val
      !character(:), allocatable:: val

      class(type_node), pointer :: node
      type(type_error), allocatable :: io_err
      logical :: is_default

      node => this%root%get(key)

      ! NOTE: get_string does not trigger no key error
      if (associated(node)) then
         val = trim(this%root%get_string(key, error=io_err))
      else
         allocate (io_err)
         io_err%message = trim(this%path)//' does not contain key "'//trim(key)//'".'
      end if

      is_default = this%parse_error_message(key, io_err, default, is_empty)

      ! Parsing default value string
      if (is_default) then
         !val_buff = TRIM(default)
         val = trim(this%root%get_string(key, error=io_err))
         ! Safety check
         if (allocated(io_err)) then
            call this%log%exit_on_fatal("Default value for "//io_err%message, 1)
         end if
      end if

   end subroutine read_string_node

   subroutine read_string(this, key, default, is_empty, val)
      !----------------------------------------------------------
      ! Read key from from root node and parse as integer
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      character(:), allocatable, intent(inout) :: val

      if (this%comm%is_io_node()) then
         call this%read_string_node(key, default=default, is_empty=is_empty, val=val)
         val = trim(val)
      end if

      call this%comm%bcast_string(val)
   end subroutine read_string

   subroutine read_enum(this, key, values, default, is_empty, val)
      !----------------------------------------------------------

      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(len=*), dimension(:), intent(in) :: values
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      character(:), allocatable, intent(inout) :: val

      integer :: len, i
      logical :: is_found
      character(len=:), allocatable :: msg

      if (this%comm%is_io_node()) then

         call this%read_string_node(key, default, is_empty, val)

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

      end if

      call this%comm%bcast_string(val)

   end subroutine read_enum

   subroutine read_input_path(this, key, default, is_empty, val)
      !----------------------------------------------------------
      ! Read key from from root node and parse as integer
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      type(type_path), intent(inout) :: val

      character(:), allocatable :: val_buff
      logical :: file_exists
      character(len=:), allocatable :: msg

      if (this%comm%is_io_node()) then
         call this%read_string_node(key, default=default, is_empty=is_empty, val=val_buff)

         val = type_path(val_buff)

         if (.not. val%is_file()) then
            msg = "which is not a valid file path."
            msg = this%prep_msg_val(key, msg)
            call this%log%exit_on_error(msg)
         end if

      end if

      call this%comm%bcast_string(val_buff)
      val = type_path(val_buff)
   end subroutine read_input_path

   function parse_error_message(this, key, io_err, default, is_empty) result(is_default)
      ! Parses fortran-yaml-c error message and filters no key error from
      ! terminating program when optional arguments are provided

      class(type_yaml_reader), intent(inout) :: this

      character(*), intent(in)::key
      type(type_error), allocatable, intent(inout) :: io_err
      character(*), optional, intent(in):: default
      logical, optional, intent(out) ::is_empty
      logical :: is_default
      class(type_node), pointer:: node
      character(STRING_SIZE) :: buff
      integer :: err_id

      if (.not. allocated(io_err)) then
         is_default = .false.
         buff = " read value "//this%root%get_string(key, error=io_err)
         call this%log%debug(this%prep_msg(key, buff))
         return
      end if

      if (present(default)) then

         ! Ignoring no key error from error checking if optional arguments
         if (is_no_key_err(io_err)) then
            is_default = .true.
            buff = " not found, using default value "//trim(default)//'.'
            call this%log%info(this%prep_msg(key, buff))

            ! Updating YAML with defaults for outputting full config, and
            ! parse string default values.
            call this%root%set_string(key, default)

            node => this%root%get(key)
            node%path = this%root%path//"/"//key

         else
            is_default = .false.
            call this%log%exit_on_error(io_err%message)
         end if

      else if (present(is_empty)) then
         ! Ignoring no key error if bypass value is given
         is_empty = .true.
         is_default = .false.
         if (.not. is_no_key_err(io_err)) then
            call this%log%exit_on_error(io_err%message)
         end if

      else
         ! Default behavior, exiting on normally on YAML error
         is_default = .false.
         call this%log%exit_on_error(io_err%message)

      end if

      deallocate (io_err)

   end function parse_error_message

   function prep_msg(this, key, msg) result(new_msg)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in)::key, msg
      character(len=STRING_SIZE) :: new_msg
      new_msg = this%root%path//"/"//key//msg
   end function prep_msg

   function prep_msg_val(this, key, msg) result(new_msg)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in)::key, msg
      character(len=STRING_SIZE) :: new_msg
      character(:), allocatable :: val
      type(type_error), allocatable :: io_err

      val = this%root%get_string(key, error=io_err)

      new_msg = this%root%path//"/"//key//" is set to """//val//""", "//msg
   end function prep_msg_val

   pure function is_no_key_err(io_err) result(val)

      type(type_error), allocatable, intent(in) :: io_err
      character(*), parameter:: NO_KEY_ERR = "does not contain key"
      logical :: val

      val = (index(io_err%message, NO_KEY_ERR) .gt. 0)

   end function is_no_key_err
end module yaml_file_mod
