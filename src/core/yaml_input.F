!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Class for integrating external YAML reader with logging
!  and error handling.
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
!--------------------------------------------------
module yaml_file_mod

   use Constants, only: MESSAGE_SIZE, STRING_SIZE, LABEL_SIZE, sp
   use log_io_mod, only: type_log_writer
   use comm_mod, only: type_comm

   !use Constants, only: STRING_SIZE
   use fortran_yaml_c, only: YamlFile, dp, &
                             type_node, type_dictionary, type_error, &
                             type_list, type_list_item, type_scalar
   implicit none(external)

   private
   public :: type_yaml_reader, err_key, err_none, err_type, err_range

   integer, parameter :: err_none = 0
   integer, parameter :: err_key = 1
   integer, parameter :: err_type = 2
   integer, parameter :: err_range = 3
   type(type_log_writer), TARGET :: log_buff

   character(LABEL_SIZE), parameter :: log_label = "config"

   type type_yaml_reader
      private
      logical :: is_io_node = .False.
      character(MESSAGE_SIZE):: path = ""
      type(type_log_writer), pointer, public :: log
      type(type_comm), pointer :: comm
      class(type_dictionary), pointer :: root => null()
      type(YamlFile):: file

   contains

      private
      procedure, public :: init
      procedure, public :: read_real
      procedure, public :: read_integer
      procedure, public :: read_positive_integer
      ! procedure, public :: read_string
      !  procedure, public :: read_enum
      procedure, public :: cast_dictionary
      procedure :: copy_node
      procedure :: parse_error_message
      procedure :: prep_msg
      procedure :: prep_msg_val
      procedure:: read_integer_node
      procedure:: read_real_node
      ! procedure:: read_string_node

   end type type_yaml_reader

contains

   subroutine init(this, path, comm)

      class(type_yaml_reader), intent(inout) :: this
      character(:), allocatable, intent(in) :: path
      type(type_comm), target, intent(inout):: comm

      character(:), allocatable :: err
      class(type_node), pointer :: root
      character(MESSAGE_SIZE) :: message
      logical :: file_exist

      if (.not. comm%is_io_node()) return

      log_buff = comm%get_logger(log_label)
      this%log => log_buff
      this%comm => comm

      inquire (file=path, exist=file_exist)

      if (.not. file_exist) then
         message = "Input file does not exists, got: "//trim(path)
         call this%log%exit_on_error(message)
      end if

      call this%file%parse(trim(path), err)

      if (allocated(err)) then
         call this%log%exit_on_error(err)
      end if

      root => this%file%root

      select type (root)
      class is (type_dictionary)
         this%root => root

      class default
         call this%log%exit_on_error('Input file does not appear to be a dictionary')
      end select

   end subroutine init

   subroutine cast_dictionary(this, key, child)
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
      type(type_dictionary), pointer :: node
      type(type_error), allocatable :: io_err

      logical, parameter :: is_required = .true.

      node => this%root%get_dictionary(key, is_required, io_err)

      if (allocated(io_err)) then
         call this%log%exit_on_error(io_err%message)
      end if

      call child%copy_node(node)

   end subroutine cast_dictionary

   subroutine copy_node(this, node)
      !----------------------------------------------------------
      ! Mediator routine for sharing private variables
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      type(type_dictionary), target, intent(inout):: node
      this%log => log_buff
      this%root => node

   end subroutine copy_node

   subroutine read_real_node(this, key, default_val, is_empty, val)
      !----------------------------------------------------------
      !
      ! Read key from from root node and parse as real
      !
      ! Arguments
      !   key - keyword in yaml dictionary
      !   default_val - Optional default value as string
      !   is_empty    - Optional flag indicating if key was found
      !                 Note: Bypasses logging
      !   val         - Buffer for value to be read
      !
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      real(sp), intent(out) :: val
      character(*), optional, intent(in) :: default_val
      logical, optional, intent(out) :: is_empty
      type(type_error), allocatable :: io_err
      logical :: is_default

      val = this%root%get_real(key, error=io_err)
      is_default = this%parse_error_message(key, io_err, default_val, is_empty)

      ! Parsing default value string
      if (is_default) then
         val = this%root%get_real(key, error=io_err)
         ! Safety check
         if (allocated(io_err)) then
            call this%log%exit_on_fatal("Default value for "//io_err%message)
         end if
      end if

   end subroutine read_real_node

   subroutine read_real(this, key, default_val, is_empty, val)
      !----------------------------------------------------------
      ! Wrapper method around read_real_node for single node
      ! MPI I/O and broadcasting to other nodes
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      real(sp), intent(out) :: val
      character(*), optional, intent(in) :: default_val
      logical, optional, intent(out) :: is_empty

      if (this%comm%is_io_node()) then
         call this%read_real_node(key, default_val, is_empty, val)
      end if

   end subroutine read_real

   subroutine read_integer_node(this, key, default_val, is_empty, val)
      !----------------------------------------------------------
      ! Read key from from root node and parse as integer
      !
      ! Arguments
      !   key         - keyword in yaml dictionary
      !   default_val - Optional default value as string
      !   is_empty    - Optional flag indicating if key was found
      !                 Note: Bypasses logging
      !   val         - Buffer for value to be read
      !
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default_val
      logical, optional, intent(out) :: is_empty
      integer, intent(out) :: val

      type(type_error), allocatable :: io_err
      logical :: is_default

      val = this%root%get_integer(key, error=io_err)
      is_default = this%parse_error_message(key, io_err, default_val, is_empty)

      ! Parsing default value string
      if (is_default) then
         val = this%root%get_integer(key, error=io_err)
         ! Safety check
         if (allocated(io_err)) then
            call this%log%exit_on_fatal("Default value for /"//key//io_err%message, 1)
         end if
      end if

   end subroutine read_integer_node

   subroutine read_integer(this, key, default_val, is_empty, val)
      !----------------------------------------------------------
      !
      ! Wrapper subroutine around read_real_node for single node
      ! MPI I/O and broadcasting to other nodes
      !
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default_val
      logical, optional, intent(out) :: is_empty
      integer, intent(out) :: val

      if (this%comm%is_io_node()) then
         call this%read_integer_node(key, default_val, is_empty, val)
      end if

   end subroutine read_integer

   subroutine read_positive_integer(this, key, default_val, is_empty, include_zero, val)

      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default_val
      logical, optional, intent(out) :: is_empty
      logical, optional, intent(in) :: include_zero
      integer, intent(out) :: val
      logical :: is_error, is_include_zero
      character(len=:), allocatable :: message

      if (present(include_zero)) then
         is_include_zero = include_zero
      else
         is_include_zero = .false.
      end if

      if (this%comm%is_io_node()) then

         call this%read_integer_node(key, default_val, is_empty, val)

         if (is_include_zero) then
            if (val .lt. 0) then
               message = this%prep_msg_val(key, "which is not an integer greater than or equal to zero.")
               call this%log%exit_on_error(message)
            end if
         else
            if (val .le. 0) then
               message = this%prep_msg_val(key, "which is not a positive integer.")
               call this%log%exit_on_error(message)
            end if
         end if

      end if

   end subroutine read_positive_integer

   ! subroutine read_string_node(this, key, default_val, is_empty, val)
   !
   !    !----------------------------------------------------------
   !    ! Read key from from root node and parse as integer
   !    !
   !    ! Arguments
   !    !   key         - keyword in yaml dictionary
   !    !   default_val - Optional default value as string
   !    !   is_empty    - Optional flag indicating if key was found
   !    !                 Note: Bypasses logging
   !    !   val         - Buffer for value to be read
   !    !
   !    !----------------------------------------------------------
   !    class(type_yaml_reader), intent(inout) :: this
   !    character(*), intent(in) :: key
   !    character(*), optional, intent(in) :: default_val
   !    logical, optional, intent(out) :: is_empty
   !    character(:), allocatable, intent(inout) :: val
   !
   !    type(type_error), allocatable :: io_err
   !    logical :: is_default
   !
   !    if (allocated(val)) then
   !       deallocate (val)
   !    end if
   !
   !    val = this%root%get_string(key, error=io_err)
   !    is_default = this%parse_error_message(key, io_err, default_val, is_empty)
   !
   !    ! Parsing default value string
   !    if (is_default) then
   !       print *, "SETTING DEFAULT"
   !       val = this%root%get_string(key, error=io_err)
   !       print *, "DEFAULT: "//val
   !       ! Safety check
   !       if (allocated(io_err)) then
   !          call this%log%exit_on_fatal("Default value for /"//key//io_err%message, 1)
   !       end if
   !       print *, "HERE"
   !    end if
   !
   !    print *, "HERE"
   ! end subroutine read_string_node
   !
   ! subroutine read_string(this, key, default_val, is_empty, val)
   !
   !    !----------------------------------------------------------
   !    ! Read key from from root node and parse as integer
   !    !
   !    ! Arguments
   !    !   key         - keyword in yaml dictionary
   !    !   default_val - Optional default value as string
   !    !   is_empty    - Optional flag indicating if key was found
   !    !                 Note: Bypasses logging
   !    !   val         - Buffer for value to be read
   !    !
   !    !----------------------------------------------------------
   !    class(type_yaml_reader), intent(inout) :: this
   !    character(*), intent(in) :: key
   !    character(*), optional, intent(in) :: default_val
   !    logical, optional, intent(out) :: is_empty
   !    character(:), allocatable, intent(inout) :: val
   !
   !    if (this%comm%is_io_node()) then
   !       call this%read_string_node(key, default_val, is_empty, val)
   !    end if
   !
   ! end subroutine read_string
   !
   ! subroutine read_enum(this, key, values, default_val, is_empty, val)
   !
   !    class(type_yaml_reader), intent(inout) :: this
   !    character(*), intent(in) :: key
   !    character(len=*), dimension(:), intent(in) :: values
   !    character(*), optional, intent(in) :: default_val
   !    logical, optional, intent(out) :: is_empty
   !    character(:), allocatable, intent(inout) :: val
   !
   !    integer :: len, i
   !    logical :: is_found
   !    CHARACTER(len=:), ALLOCATABLE :: message
   !
   !    if (this%comm%is_io_node()) then
   !       is_found = .false.
   !
   !       !!default_val2 = default_val
   !       call this%read_string_node(key, default_val, is_empty, val)
   !
   !       PRINT *, "HERE2"
   !       len = size(values)
   !       do i = 1, len
   !          if (val .eq. values(i)) then
   !             is_found = .true.
   !             exit
   !          end if
   !       end do
   !
   !       if (.not. is_found) then
   !
   !          message = "which is not in the allowable list of values. Valid values: "
   !
   !          message = message//trim(values(1))
   !          do i = 2, len - 1
   !             message = message//', '//trim(values(i))
   !          end do
   !          message = message//', & '//trim(values(len))//"."
   !
   !          message = this%prep_msg_val(key, message)
   !          call this%log%exit_on_error(message)
   !       end if
   !
   !    end if
   ! end subroutine read_enum

   function parse_error_message(this, key, io_err, default_val, is_empty) result(is_default)
      ! Parses fortran-yaml-c error message and filters no key error from
      ! terminating program when optional arguments are provided

      class(type_yaml_reader), intent(inout) :: this

      character(*), intent(in)::key
      type(type_error), allocatable, intent(inout) :: io_err
      character(*), optional, intent(in):: default_val
      logical, optional, intent(out) ::is_empty
      logical :: is_default

      character(STRING_SIZE) :: buff
      character(*), parameter:: nokey_err_msg = "does not contain key"
      integer :: err_id

      if (.not. allocated(io_err)) then
         is_default = .false.
         return
      end if

      ! TODO: Check for other error types
      if (index(io_err%message, nokey_err_msg) .gt. 0) then
         err_id = err_key
      else
         err_id = err_type
      end if

      ! Removing  no key error from error checking if optional arguments
      if (present(default_val) .or. present(is_empty)) then

         if (err_id .eq. err_key) then
            is_default = .true.
            buff = " not found, using default value "//trim(default_val)//'.'
            call this%log%warning(this%prep_msg(key, buff))

            ! Updating YAML with defaults for outputting full config, and
            ! parse string default values.
            call this%root%set_string(key, default_val)
         else
            is_default = .false.
            call this%log%exit_on_error(io_err%message)
         end if

      else
         is_default = .false.
         call this%log%exit_on_error(io_err%message)
      end if

      deallocate (io_err)

   end function parse_error_message

   function prep_msg(this, key, msg) result(new_msg)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in)::key, msg
      character(len=STRING_SIZE) :: new_msg
      new_msg = this%root%path//"/"//key//msg
   end function prep_msg

   function prep_msg_val(this, key, msg) result(new_msg)
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in)::key, msg
      character(len=STRING_SIZE) :: new_msg
      character(:), ALLOCATABLE :: val
      type(type_error), ALLOCATABLE :: io_err

      val = this%root%get_string(key, error=io_err)

      new_msg = this%root%path//"/"//key//" is set to """//val//""", "//msg
   end function prep_msg_val

end module yaml_file_mod
