!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Static-field input seam: one reference grammar + one dispatching
!  reader for 2D snapshot fields.
!
!  Reference grammar (mirrors the planned NetCDF container design):
!    depth.txt                  loose file, format from the extension
!    root#/bathymetry/depth     master container (input_container:) path
!    other.nc#/group/var        explicit container, overrides the master
!  A fragment-only "#/path" is rejected — unquoted it parses as an
!  empty YAML value anyway (comment), so the container token is
!  mandatory before '#'.
!
!  Formats: ascii (legacy GetFile rows) and binary (the same row
!  contract as a real(SP) stream) are live; netcdf (and with it any
!  container ref) gates pending the NetCDF bringup — output-first.
!
!  HISTORY :
!    07/21/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_field_input_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use model_geometry_mod, only: read_field_ascii

   implicit none

   private
   public :: type_file_spec
   public :: parse_file_spec
   public :: read_field

   ! Parsed field-file reference — where the data lives and how to
   ! decode it.  container = "" for a loose file; "root" resolves
   ! against the deck-level input_container: key (at read time).
   type :: type_file_spec
      character(:), allocatable :: path         ! loose-file path
      character(:), allocatable :: container    ! container file or "root"
      character(:), allocatable :: group_path   ! /group/var inside the container
      character(:), allocatable :: format       ! ascii | binary | netcdf
   end type type_file_spec

contains

   ! ----------------------------------------------------------------
   ! Split "container#/group/var" | "path" into a spec; format from
   ! the explicit override when given, else the extension (.nc ->
   ! netcdf, .bin -> binary, default ascii).  Grammar errors report
   ! through env%log with the offending key for context.
   ! ----------------------------------------------------------------
   subroutine parse_file_spec(env, key, ref, spec, format_override)
      type(type_env), intent(inout) :: env
      character(*), intent(in) :: key   ! YAML key, error context only
      character(*), intent(in) :: ref
      type(type_file_spec), intent(out) :: spec
      character(*), intent(in), optional :: format_override

      type(type_path) :: p
      integer :: ihash

      spec%path = ""
      spec%container = ""
      spec%group_path = ""

      if (len_trim(ref) == 0) call env%log%exit_on_error(trim(key)// &
                                                         ": empty file reference (an unquoted '#...' fragment reads as"// &
                                                         " an empty YAML value — use 'root#/...' or quote)")

      ihash = index(ref, "#")
      if (ihash > 0) then
         if (ihash == 1) call env%log%exit_on_error(trim(key)// &
                                                    ": missing container before '#' — use root#/... for the"// &
                                                    " input_container: file")
         if (ihash == len_trim(ref)) call env%log%exit_on_error(trim(key)// &
                                                                ": empty group path after '#'")
         spec%container = trim(ref(1:ihash - 1))
         spec%group_path = trim(ref(ihash + 1:))
         spec%format = "netcdf"
      else
         spec%path = trim(ref)
         p = type_path(spec%path)
         if (p%has_suffix("nc")) then
            spec%format = "netcdf"
         else if (p%has_suffix("bin")) then
            spec%format = "binary"
         else
            spec%format = "ascii"
         end if
      end if

      if (present(format_override)) spec%format = trim(format_override)

   end subroutine parse_file_spec

   ! ----------------------------------------------------------------
   ! Dispatching snapshot-field reader.  Ghosts are the caller's
   ! concern (same contract as read_field_ascii).
   ! ----------------------------------------------------------------
   subroutine read_field(env, spec, grid, arr)
      type(type_env), intent(inout) :: env
      type(type_file_spec), intent(in) :: spec
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: arr(:, :)

      if (len(spec%container) > 0) then
         call env%log%exit_on_error("field input: container refs are pending"// &
                                    " the NetCDF bringup ('"//spec%container//"#"// &
                                    spec%group_path//"')")
      end if

      select case (spec%format)
      case ("ascii")
         call read_field_ascii(env, spec%path, grid, arr)
      case ("binary")
         call read_field_binary(env, spec%path, grid, arr)
      case ("netcdf")
         call env%log%exit_on_error("field input: netcdf is pending the"// &
                                    " NetCDF bringup ('"//spec%path//"')")
      case default
         call env%log%exit_on_error("field input: unknown format '"// &
                                    spec%format//"'")
      end select

   end subroutine read_field

   ! ----------------------------------------------------------------
   ! Binary twin of read_field_ascii: the same global row layout (one
   ! row of Mglob values per global J) as a plain real(SP) stream, no
   ! header.  Every rank reads the file — init-time only, no scatter.
   ! ----------------------------------------------------------------
   subroutine read_field_binary(env, fname, grid, arr)
      type(type_env), intent(inout) :: env
      character(*), intent(in) :: fname
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: arr(:, :)

      real(SP), allocatable :: row(:)
      logical :: exists
      integer :: gj, unit

      inquire (file=trim(fname), exist=exists)
      if (.not. exists) then
         call env%log%exit_on_error( &
            "read_field_binary: cannot find "//trim(fname))
      end if

      allocate (row(grid%M))
      open (newunit=unit, file=trim(fname), status="old", action="read", &
            access="stream", form="unformatted")
      do gj = 1, grid%N
         read (unit) row
         if (gj >= grid%jbegin .and. gj <= grid%jstop) then
            arr(grid%lp%ib:grid%lp%ie, grid%lp%jb + gj - grid%jbegin) = &
               row(grid%ibegin:grid%istop)
         end if
      end do
      close (unit)

   end subroutine read_field_binary

end module model_field_input_mod
