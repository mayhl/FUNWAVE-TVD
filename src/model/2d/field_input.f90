!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Static-field input seam: one reference grammar + one dispatching
!  reader for 2D snapshot fields.
!
!  Reference grammar (mirrors the NetCDF container design):
!    depth.txt                  loose file, format from the extension
!    other.nc#/group/var        one variable inside a container file
!    root#/bathymetry/depth     master container (input_container:)
!  A fragment-only "#/path" is rejected — unquoted it parses as an
!  empty YAML value anyway (comment), so the container token is
!  mandatory before '#'.
!
!  Formats: ascii (legacy GetFile rows, one record per global J, values
!  separated by blanks, tabs or commas — a .csv reads as is); binary
!  (the same rows as a real(SP) stream, no header, so the deck carries
!  the dimensions); netcdf (a 2-D variable on (x, y) — a loose .nc
!  holds exactly one 2-D variable, a container ref names it).
!  The master container waits on the input_container: key.
!
!  Windows: a present grid.n_cells is an origin-anchored subset of a
!  larger ascii or netcdf file; binary has no record structure to
!  skip, so its dimensions must equal the deck's (check_binary_size).
!
!  HISTORY :
!    07/21/2026  Michael-Angelo Y.H. Lam
!    09/16/2026  netcdf reader; ascii reader + dims scan moved from geometry;
!                read_field_global for the breakwater width
!
!-------------------------------------------------

module model_field_input_mod
   use netcdf
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path

   implicit none

   private
   public :: type_file_spec
   public :: parse_file_spec
   public :: read_field
   public :: read_field_global
   public :: scan_field_dims
   public :: check_binary_size

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
   ! Dispatching snapshot-field reader: this rank's interior window of
   ! the file's leading M x N block.  Ghosts are the caller's concern.
   ! ----------------------------------------------------------------
   subroutine read_field(env, spec, grid, arr)
      type(type_env), intent(inout) :: env
      type(type_file_spec), intent(in) :: spec
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: arr(:, :)

      call read_window(env, spec, grid%M, grid%N, grid%ibegin, grid%istop, &
                       grid%jbegin, grid%jstop, &
                       arr(grid%lp%ib:grid%lp%ie, grid%lp%jb:grid%lp%je))

   end subroutine read_field

   ! ----------------------------------------------------------------
   ! The whole nx x ny block on every rank — for the init-time fields a
   ! global compute consumes before slicing (breakwater drag).
   ! ----------------------------------------------------------------
   subroutine read_field_global(env, spec, nx, ny, arr)
      type(type_env), intent(inout) :: env
      type(type_file_spec), intent(in) :: spec
      integer, intent(in) :: nx, ny
      real(SP), intent(inout) :: arr(:, :)

      call read_window(env, spec, nx, ny, 1, nx, 1, ny, arr)

   end subroutine read_field_global

   ! the [i0:i1, j0:j1] window of the file's leading nx x ny block
   subroutine read_window(env, spec, nx, ny, i0, i1, j0, j1, win)
      type(type_env), intent(inout) :: env
      type(type_file_spec), intent(in) :: spec
      integer, intent(in) :: nx, ny, i0, i1, j0, j1
      real(SP), intent(inout) :: win(:, :)

      call check_container(env, spec)

      select case (spec%format)
      case ("ascii")
         call read_rows(env, spec%path, .false., nx, ny, i0, i1, j0, j1, win)
      case ("binary")
         call read_rows(env, spec%path, .true., nx, ny, i0, i1, j0, j1, win)
      case ("netcdf")
         call read_window_netcdf(env, spec, nx, ny, i0, i1, j0, j1, win)
      case default
         call env%log%exit_on_error("field input: unknown format '"// &
                                    spec%format//"'")
      end select

   end subroutine read_window

   ! ----------------------------------------------------------------
   ! File dimensions for the self-describing formats (n_cells
   ! inference and the --validate window check); binary carries none.
   ! ----------------------------------------------------------------
   subroutine scan_field_dims(env, spec, nx, ny)
      type(type_env), intent(inout) :: env
      type(type_file_spec), intent(in) :: spec
      integer, intent(out) :: nx, ny

      integer :: ncid, grp, varid

      call check_container(env, spec)

      select case (spec%format)
      case ("ascii")
         call scan_ascii_dims(env, spec%path, nx, ny)
      case ("netcdf")
         call open_field_netcdf(env, spec, ncid, grp, varid, nx, ny)
         call nc_check(env, nf90_close(ncid), "close "//nc_name(spec))
      case ("binary")
         call env%log%exit_on_error("field input: a binary stream carries no"// &
                                    " dimensions — set grid/n_cells ('"//spec%path//"')")
      case default
         call env%log%exit_on_error("field input: unknown format '"// &
                                    spec%format//"'")
      end select

   end subroutine scan_field_dims

   ! ----------------------------------------------------------------
   ! A binary field is exactly nx*ny values of the working precision;
   ! anything else is the wrong file, precision or dimensions.
   ! ----------------------------------------------------------------
   subroutine check_binary_size(env, spec, nx, ny)
      type(type_env), intent(inout) :: env
      type(type_file_spec), intent(in) :: spec
      integer, intent(in) :: nx, ny

      integer(8) :: nbytes, want
      character(32) :: have_s, want_s

      inquire (file=spec%path, size=nbytes)
      want = int(nx, 8)*int(ny, 8)*int(storage_size(0.0_SP)/8, 8)
      if (nbytes /= want) then
         write (have_s, '(I0)') nbytes
         write (want_s, '(I0)') want
         call env%log%exit_on_error("field input: "//spec%path//" holds "// &
                                    trim(have_s)//" bytes, grid/n_cells at the working"// &
                                    " precision wants "//trim(want_s))
      end if

   end subroutine check_binary_size

   ! ----------------------------------------------------------------
   ! Row files: one record of nx values per global J (legacy GetFile),
   ! as text or as a real(SP) stream with no header.  Every rank reads
   ! the file — init-time only, no scatter.  A list-directed read drops
   ! the rest of a longer record, which is what makes a present n_cells
   ! an origin-anchored window; it also takes an empty field (",,") as
   ! a null that leaves the element untouched, so the row is
   ! sentinel-filled and checked.  A short record silently continues
   ! into the next one — only the dims scan (n_cells absent, or
   ! --validate) catches those.
   ! ----------------------------------------------------------------
   subroutine read_rows(env, fname, binary, nx, ny, i0, i1, j0, j1, win)
      type(type_env), intent(inout) :: env
      character(*), intent(in) :: fname
      logical, intent(in) :: binary
      integer, intent(in) :: nx, ny, i0, i1, j0, j1
      real(SP), intent(inout) :: win(:, :)

      real(SP), parameter :: NULL_FIELD = huge(1.0_SP)
      real(SP), allocatable :: row(:)
      character(16) :: row_s
      logical :: exists, has_null
      integer :: gj, unit, ios

      inquire (file=trim(fname), exist=exists)
      if (.not. exists) then
         call env%log%exit_on_error("field input: cannot find "//trim(fname))
      end if

      allocate (row(nx))
      if (binary) then
         open (newunit=unit, file=trim(fname), status="old", action="read", &
               access="stream", form="unformatted")
      else
         open (newunit=unit, file=trim(fname), status="old", action="read")
      end if
      do gj = 1, ny
         has_null = .false.
         if (binary) then
            read (unit, iostat=ios) row
         else
            row = NULL_FIELD
            read (unit, *, iostat=ios) row
            has_null = ios == 0 .and. any(row == NULL_FIELD)
         end if
         if (ios /= 0 .or. has_null) then
            write (row_s, '(I0)') gj
            if (has_null) then
               call env%log%exit_on_error("field input: "//trim(fname)// &
                                          " row "//trim(row_s)//": empty field")
            else if (ios < 0) then
               call env%log%exit_on_error("field input: "//trim(fname)// &
                                          " ends at row "//trim(row_s)//", fewer rows than the grid")
            else
               call env%log%exit_on_error("field input: "//trim(fname)// &
                                          " row "//trim(row_s)//": not a numeric value (header line?)")
            end if
         end if
         if (gj >= j0 .and. gj <= j1) win(:, gj - j0 + 1) = row(i0:i1)
      end do
      close (unit)

   end subroutine read_rows

   ! ----------------------------------------------------------------
   ! NetCDF: the [i0:i1, j0:j1] window straight from the variable —
   ! the library does the row skipping the row readers pay for by
   ! reading.  The file may be larger than nx x ny (window), never
   ! smaller.
   ! ----------------------------------------------------------------
   subroutine read_window_netcdf(env, spec, nx, ny, i0, i1, j0, j1, win)
      type(type_env), intent(inout) :: env
      type(type_file_spec), intent(in) :: spec
      integer, intent(in) :: nx, ny, i0, i1, j0, j1
      real(SP), intent(inout) :: win(:, :)

      integer :: ncid, grp, varid, fx, fy
      character(32) :: dims_s

      call open_field_netcdf(env, spec, ncid, grp, varid, fx, fy)
      if (fx < nx .or. fy < ny) then
         write (dims_s, '(I0,A,I0)') fx, " x ", fy
         call env%log%exit_on_error("field input: "//nc_name(spec)//" is "// &
                                    trim(dims_s)//", smaller than the grid")
      end if
      call nc_check(env, nf90_get_var(grp, varid, win, start=[i0, j0], &
                                      count=[i1 - i0 + 1, j1 - j0 + 1]), &
                    "read "//nc_name(spec))
      call nc_check(env, nf90_close(ncid), "close "//nc_name(spec))

   end subroutine read_window_netcdf

   ! ----------------------------------------------------------------
   ! Open the file and locate the field: the named variable under the
   ! container's group path, or the sole 2-D variable of a loose file.
   ! Returns the open ids and the variable's (x, y) extents.
   ! ----------------------------------------------------------------
   subroutine open_field_netcdf(env, spec, ncid, grp, varid, nx, ny)
      type(type_env), intent(inout) :: env
      type(type_file_spec), intent(in) :: spec
      integer, intent(out) :: ncid, grp, varid, nx, ny

      character(NF90_MAX_NAME) :: vname
      character(:), allocatable :: fname, names
      logical :: exists
      integer :: islash, nvars, iv, ndims, n2d, dimids(2)

      fname = nc_name(spec)
      inquire (file=fname, exist=exists)
      if (.not. exists) then
         call env%log%exit_on_error("read_field_netcdf: cannot find "//fname)
      end if
      call nc_check(env, nf90_open(fname, NF90_NOWRITE, ncid), "open "//fname)

      if (len(spec%group_path) > 0) then
         islash = index(spec%group_path, "/", back=.true.)
         if (islash == len(spec%group_path)) then
            call env%log%exit_on_error("field input: no variable name in '"// &
                                       spec%container//"#"//spec%group_path//"'")
         end if
         grp = ncid
         if (islash > 1) then
            call nc_check(env, nf90_inq_grp_full_ncid(ncid, spec%group_path(1:islash - 1), grp), &
                          "group "//spec%group_path(1:islash - 1)//" in "//fname)
         end if
         call nc_check(env, nf90_inq_varid(grp, spec%group_path(islash + 1:), varid), &
                       "variable "//spec%group_path(islash + 1:)//" in "//fname)
      else
         ! loose file: exactly one 2-D variable, or the deck must name it
         grp = ncid
         call nc_check(env, nf90_inquire(ncid, nVariables=nvars), "inquire "//fname)
         n2d = 0
         names = ""
         do iv = 1, nvars
            call nc_check(env, nf90_inquire_variable(ncid, iv, name=vname, ndims=ndims), &
                          "inquire variable in "//fname)
            if (ndims /= 2) cycle
            n2d = n2d + 1
            varid = iv
            names = names//" "//trim(vname)
         end do
         if (n2d == 0) then
            call env%log%exit_on_error("field input: no 2-D variable in "//fname)
         else if (n2d > 1) then
            call env%log%exit_on_error("field input: "//fname//" holds several 2-D"// &
                                       " variables ("//trim(names)//") — name one as "// &
                                       fname//"#/<var>")
         end if
      end if

      call nc_check(env, nf90_inquire_variable(grp, varid, ndims=ndims), &
                    "inquire "//fname)
      if (ndims /= 2) then
         call env%log%exit_on_error("field input: '"//spec%group_path// &
                                    "' in "//fname//" is not a 2-D variable")
      end if
      call nc_check(env, nf90_inquire_variable(grp, varid, dimids=dimids), &
                    "inquire "//fname)
      call nc_check(env, nf90_inquire_dimension(grp, dimids(1), len=nx), "x dim of "//fname)
      call nc_check(env, nf90_inquire_dimension(grp, dimids(2), len=ny), "y dim of "//fname)

   end subroutine open_field_netcdf

   ! the file a netcdf spec names: its container, else its loose path
   function nc_name(spec) result(fname)
      type(type_file_spec), intent(in) :: spec
      character(:), allocatable :: fname
      if (len(spec%container) > 0) then
         fname = spec%container
      else
         fname = spec%path
      end if
   end function nc_name

   ! the master container has no deck key yet
   subroutine check_container(env, spec)
      type(type_env), intent(inout) :: env
      type(type_file_spec), intent(in) :: spec
      if (spec%container == "root") then
         call env%log%exit_on_error("field input: input_container: is pending ('root#"// &
                                    spec%group_path//"')")
      end if
   end subroutine check_container

   subroutine nc_check(env, status, what)
      type(type_env), intent(inout) :: env
      integer, intent(in) :: status
      character(*), intent(in) :: what
      if (status /= NF90_NOERR) then
         call env%log%exit_on_error("field input/netcdf: "//what//": "// &
                                    trim(nf90_strerror(status)))
      end if
   end subroutine nc_check

   ! ----------------------------------------------------------------
   ! Dimension scan of a headerless ASCII grid: nx = token count per
   ! record (rectangularity enforced), ny = record count.  Chunked
   ! non-advancing reads, so no line-length assumption; blanks, tabs,
   ! CR and commas separate tokens; blank records are skipped (trailing
   ! newline tolerance).  Each record's first token must parse as a
   ! number (a header line is the usual offender) and no comma may
   ! stand without a token before it (an empty field reads as a null).
   ! One pass over the bytes -- config-time cost, paid only when
   ! n_cells is absent (inference) or under --validate (window fit).
   ! ----------------------------------------------------------------
   subroutine scan_ascii_dims(env, fname, nx, ny)
      use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
      type(type_env), intent(inout) :: env
      character(*), intent(in) :: fname
      integer, intent(out) :: nx, ny

      character(4096) :: chunk
      character(64) :: first_tok
      character(16) :: row_s
      character(1) :: c
      logical :: exists, in_tok
      integer :: unit, ios, sz, i, count, at_comma, nfirst
      real(SP) :: probe

      inquire (file=trim(fname), exist=exists)
      if (.not. exists) then
         call env%log%exit_on_error( &
            "scan_ascii_dims: cannot find "//trim(fname))
      end if

      nx = 0
      ny = 0
      open (newunit=unit, file=trim(fname), status="old", action="read")
      record: do
         count = 0
         at_comma = 0        ! token count when the last comma was seen
         nfirst = 0
         first_tok = ""
         in_tok = .false.
         do
            read (unit, '(A)', advance="no", size=sz, iostat=ios) chunk
            do i = 1, sz
               c = chunk(i:i)
               if (c == ",") then
                  in_tok = .false.
                  if (count == at_comma) call fail("empty field")
                  at_comma = count
               else if (c == " " .or. c == char(9) .or. c == char(13)) then
                  in_tok = .false.
               else
                  if (.not. in_tok) then
                     in_tok = .true.
                     count = count + 1
                  end if
                  if (count == 1 .and. nfirst < len(first_tok)) then
                     nfirst = nfirst + 1
                     first_tok(nfirst:nfirst) = c
                  end if
               end if
            end do
            if (ios == iostat_eor) exit
            if (ios == iostat_end) then
               if (count > 0) call check_row()
               exit record
            end if
         end do
         call check_row()
      end do record
      close (unit)

      if (nx == 0 .or. ny == 0) then
         call env%log%exit_on_error( &
            "scan_ascii_dims: no data rows in "//trim(fname))
      end if

   contains

      subroutine fail(what)
         character(*), intent(in) :: what
         write (row_s, '(I0)') ny + 1
         call env%log%exit_on_error("scan_ascii_dims: "//trim(fname)//" row "// &
                                    trim(row_s)//": "//what)
      end subroutine fail

      subroutine check_row()
         integer :: ios_tok
         if (count == 0) return  ! blank record
         if (count > 0 .and. count == at_comma) call fail("empty field")
         read (first_tok, *, iostat=ios_tok) probe
         if (ios_tok /= 0) call fail("not a numeric value (header line?)")
         ny = ny + 1
         if (nx == 0) then
            nx = count
         else if (count /= nx) then
            call env%log%exit_on_error( &
               "scan_ascii_dims: ragged row in "//trim(fname)// &
               " (file corrupt or not a rectangular grid)")
         end if
      end subroutine check_row

   end subroutine scan_ascii_dims

end module model_field_input_mod
