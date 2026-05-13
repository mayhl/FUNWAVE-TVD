   subroutine _READ_NODE(this, key, default, silent, &
#ifdef _HAS_RANGE
                         required_range, recommend_range, &
#endif
#ifdef _HAS_UNITS
                         dim, &
#endif
                         val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this

      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
#ifdef _HAS_RANGE
      character(*), optional, intent(in) :: required_range, recommend_range
#endif
#ifdef _HAS_UNITS
      character(*), optional, intent(in) :: dim
#endif
      _CLASS, intent(out) :: val
      logical, optional, intent(out) :: silent
#ifdef _HAS_RANGE
      type(_RANGE) :: rq_range, rd_range
#endif
      type(type_error), allocatable :: io_err
      logical :: is_default
      character(len=:), allocatable :: msg
#ifdef _HAS_UNITS
      character(:), allocatable :: unit, err
      type(type_yaml_reader) :: child
      character(16), allocatable :: units(:)
#endif

#ifdef _HAS_RANGE
      if (present(required_range)) then
         rq_range = _RANGE(required_range)
         if (.not. rq_range%is_valid()) then
            msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'required_range': "//required_range//"."
            call this%log%exit_on_fatal(msg)
         end if
      end if

      if (present(recommend_range)) then
         rd_range = _RANGE(recommend_range)
         if (.not. rd_range%is_valid()) then
            msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'recommend_range': "//recommend_range//"."
            call this%log%exit_on_fatal(msg)
         end if
      end if
#endif

#ifdef _HAS_UNITS
      if (present(dim)) then

         if (this%is_dictionary_node(key)) then

            child = this%cast_dictionary(key)

            call get_units_by_dim(dim, units, err)

            if (allocated(err)) then
               call this%log%exit_on_fatal(this%prep_msg(key, err))
               return ! Testing bypass
            end if

            call child%read_enum_node("units", units, val=unit)
            val = child%root%_GET("value", error=io_err)
            call apply_unit_conversion(val, unit, dim, val, err)

            if (allocated(err)) then
               ! NOTE: Bypass for testing
               ! 'get_units_by_dim' guards 'apply_unit_conversion'
               return
            end if

            if (allocated(unit)) deallocate (unit)

            is_default = .false.
         else
            val = this%root%_GET(key, error=io_err)
            is_default = this%parse_error_message(key, io_err, default, silent)
         end if

      else
         val = this%root%_GET(key, error=io_err)
         is_default = this%parse_error_message(key, io_err, default, silent)
      end if

#else
      ! Pre-check key existence before calling _GET. On ifx, allocatable
      ! error dummy args without explicit intent do not propagate back through
      ! the fortran-yaml-c call chain, so io_err may stay unallocated for a
      ! missing key. For character(:) return types this causes a crash when
      ! parse_error_message tries to concatenate the unallocated result.
      if (associated(this%root%get(key))) then
         val = this%root%_GET(key, error=io_err)
      else
         allocate(io_err)
         io_err%message = trim(this%root%path)//' does not contain key "'//trim(key)//'".'
      end if
      is_default = this%parse_error_message(key, io_err, default, silent)
#endif

! Parsing default value string
      if (is_default) then
#ifdef _STR2VAL
         ! For numeric types (integer, real), validate the default string directly
         ! using str2int / str2real rather than going through get_integer / get_real.
         ! Both gfortran and ifx silently partially-parse "10.5" into an integer
         ! with I20 or * format (ios=0), so the library cannot be relied upon here.
         ! str2int uses verify() to reject non-integer characters before reading.
         block
            integer :: ios_dflt
            call _STR2VAL(default, val, ios_dflt)
            if (ios_dflt /= 0) then
               msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'default': "//default//"."
               call this%log%exit_on_fatal(msg)
            end if
         end block
#else
         val = this%root%_GET(key, error=io_err)
         if (allocated(io_err)) then
            msg = "Read method for '"//this%sanitize_path(key)//"' has an invalid 'default': "//default//"."
            call this%log%exit_on_fatal(msg)
         end if
#endif
      end if

#ifdef _HAS_RANGE
      if (rq_range%is_set) then
         if (.not. rq_range%in_range(val)) then
            msg = "which is out of the required range: "//required_range//'.'
            call this%log%exit_on_error(this%prep_msg_val(key, msg))
         end if
      end if

      if (rd_range%is_set) then
         if (.not. rd_range%in_range(val)) then
            msg = "which is out of the recommended range: "//recommend_range//'.'
            call this%log%warning(this%prep_msg_val(key, msg))
         end if
      end if
#endif

   end subroutine _READ_NODE

   subroutine _READ(this, key, default, silent, &
#ifdef _HAS_RANGE
                    required_range, recommend_range, &
#endif
#ifdef _HAS_UNITS
                    dim, &
#endif
                    val)
      !----------------------------------------------------------
      ! IO node reads from YAML; all nodes receive via bcast.
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
#ifdef _HAS_RANGE
      character(*), optional, intent(in) :: required_range, recommend_range
#endif
#ifdef _HAS_UNITS
      character(*), optional, intent(in) :: dim
#endif
      _CLASS, intent(out) :: val
      logical, optional, intent(out) :: silent
      logical :: p_silent

      p_silent = .false.
      if (this%comm%is_io_node()) then
         if (present(silent)) then
            call this%_READ_NODE(key, default=default, &
                                 silent=p_silent, &
#ifdef _HAS_RANGE
                                 required_range=required_range, &
                                 recommend_range=recommend_range, &
#endif
#ifdef _HAS_UNITS
                                 dim=dim, &
#endif
                                 val=val)
         else
            call this%_READ_NODE(key, default=default, &
#ifdef _HAS_RANGE
                                 required_range=required_range, &
                                 recommend_range=recommend_range, &
#endif
#ifdef _HAS_UNITS
                                 dim=dim, &
#endif
                                 val=val)
         end if
      end if
      call this%comm%bcast(val)
      if (present(silent)) then
         call this%comm%bcast(p_silent)
         silent = p_silent
      end if

   end subroutine _READ

#ifdef _HAS_RANGE
   subroutine _READ_POSITIVE(this, key, default, silent, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      _CLASS, intent(out) :: val

      call this%_READ(key, default=default, silent=silent, val=val)

      if (.not. (val > 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be positive."))
      end if

   end subroutine _READ_POSITIVE

   subroutine _READ_NONNEGATIVE(this, key, default, silent, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      _CLASS, intent(out) :: val

      call this%_READ(key, default=default, silent=silent, val=val)

      if (.not. (val >= 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be non-negative."))
      end if

   end subroutine _READ_NONNEGATIVE

   subroutine _READ_NEGATIVE(this, key, default, silent, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      _CLASS, intent(out) :: val

      call this%_READ(key, default=default, silent=silent, val=val)

      if (.not. (val < 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be negative."))
      end if

   end subroutine _READ_NEGATIVE

   subroutine _READ_NONPOSITIVE(this, key, default, silent, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: silent
      _CLASS, intent(out) :: val

      call this%_READ(key, default=default, silent=silent, val=val)

      if (.not. (val <= 0)) then
         call this%log%exit_on_error(this%prep_msg_val(key, "which must be non-positive."))
      end if

   end subroutine _READ_NONPOSITIVE
#endif

#undef _NAME
#undef _CLASS
#undef _GET
#undef _BCAST
#undef _RANGE
#undef _HAS_RANGE
