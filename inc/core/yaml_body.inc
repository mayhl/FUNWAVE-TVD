   subroutine _READ_NODE(this, key, default, is_empty, &
#ifdef _HAS_RANGE
                         required_range, recommend_range, &
#endif
                         val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this

      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
#ifdef _HAS_RANGE
      character(*), optional, intent(in) :: required_range, recommend_range
#endif
      _CLASS, intent(out) :: val
      logical, optional, intent(out) :: is_empty
#ifdef _HAS_RANGE
      type(_RANGE) :: rq_range, rd_range
#endif
      type(type_error), allocatable :: io_err
      logical :: is_default
      character(len=:), allocatable :: msg

#ifdef _HAS_RANGE
      if (present(required_range)) then
         rq_range = _RANGE(required_range)
         if (.not. rq_range%is_valid()) then
            call this%log%exit_on_fatal("Invalid required range: "//required_range)
         end if
      end if

      if (present(recommend_range)) then
         rd_range = _RANGE(recommend_range)
         if (.not. rd_range%is_valid()) then
            call this%log%exit_on_fatal("Invalid recommended range: "//recommend_range)
         end if
      end if
#endif

      val = _GET(key, error=io_err)
      is_default = this%parse_error_message(key, io_err, default, is_empty)

      ! Parsing default value string
      if (is_default) then
         val = _GET(key, error=io_err)
         ! Safety check
         if (allocated(io_err)) then
            call this%log%exit_on_fatal("Default value for "//io_err%message)
         end if
      end if

#ifdef _HAS_RANGE
      if (rq_range%is_set) then
         if (.not. rq_range%in_range(val)) then
            msg = "which is out of the required range: "//required_range
            call this%log%exit_on_error(this%prep_msg_val(key, msg))
         end if
      end if

      if (rd_range%is_set) then
         if (.not. rd_range%in_range(val)) then
            msg = "which is out of the recommended range: "//recommend_range
            call this%log%warning(this%prep_msg_val(key, msg))
         end if
      end if
#endif

   end subroutine _READ_NODE

   subroutine _READ(this, key, default, is_empty, &
#ifdef _HAS_RANGE
                    required_range, recommend_range, &
#endif
                    val)
      !----------------------------------------------------------
      ! Wrapper method around _READ_NODE for single node
      ! MPI I/O and broadcasting to other nodes
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
#ifdef _HAS_RANGE
      character(*), optional, intent(in) :: required_range, recommend_range
#endif
      _CLASS, intent(out) :: val
      logical, optional, intent(out) :: is_empty

      if (this%comm%is_io_node()) then

         call this%_READ_NODE(key, default=default, &
                              is_empty=is_empty, &
#ifdef _HAS_RANGE
                              required_range=required_range, &
                              recommend_range=recommend_range, &
#endif
                              val=val)

      end if

      _BCAST(val)

   end subroutine _READ

#ifdef _HAS_RANGE
   subroutine _READ_POSITIVE(this, key, default, is_empty, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      _CLASS, intent(out) :: val

      call this%_READ(key, default=default, is_empty=is_empty, val=val)

      if (this%comm%is_io_node()) then
         if (.not. (val > 0)) then
            call this%log%exit_on_error(this%prep_msg_val(key, "which must be positive."))
         end if
      end if

   end subroutine _READ_POSITIVE

   subroutine _READ_NONNEGATIVE(this, key, default, is_empty, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      _CLASS, intent(out) :: val

      call this%_READ(key, default=default, is_empty=is_empty, val=val)

      if (this%comm%is_io_node()) then
         if (.not. (val >= 0)) then
            call this%log%exit_on_error(this%prep_msg_val(key, "which must be non-negative."))
         end if
      end if

   end subroutine _READ_NONNEGATIVE

   subroutine _READ_NEGATIVE(this, key, default, is_empty, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      _CLASS, intent(out) :: val

      call this%_READ(key, default=default, is_empty=is_empty, val=val)

      if (this%comm%is_io_node()) then
         if (.not. (val < 0)) then
            call this%log%exit_on_error(this%prep_msg_val(key, "which must be negative."))
         end if
      end if

   end subroutine _READ_NEGATIVE

   subroutine _READ_NONPOSITIVE(this, key, default, is_empty, val)
      !----------------------------------------------------------
      class(type_yaml_reader), intent(inout) :: this
      character(*), intent(in) :: key
      character(*), optional, intent(in) :: default
      logical, optional, intent(out) :: is_empty
      _CLASS, intent(out) :: val

      call this%_READ(key, default=default, is_empty=is_empty, val=val)

      if (this%comm%is_io_node()) then
         if (.not. (val <= 0)) then
            call this%log%exit_on_error(this%prep_msg_val(key, "which must be non-positive."))
         end if
      end if

   end subroutine _READ_NONPOSITIVE
#endif

#undef _NAME
#undef _CLASS
#undef _GET
#undef _BCAST
#undef _RANGE
#undef _HAS_RANGE
