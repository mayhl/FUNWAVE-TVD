!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Output channel: one logical output stream.
!
!  Geometry types:
!   'field'    — full 2D subdomain per rank, MPI_Gatherv to assemble
!   'station'  — n_points scattered locations, bilinear interpolation
!   'transect' — n_points along a line, same as station
!
!  Per-variable accumulation: one type_accumulator per variable.
!  Each accumulator can hold any subset of {min, max, mean, rms}.
!  Snapshot (instantaneous write) reads directly from field_registry,
!  bypassing the accumulator.
!
!  File layout (all files under result_folder, which must exist and
!  include a trailing path separator; <var> is the registry name or the
!  per-variable file_prefixes override, counter base icount_start+1):
!   field snapshot   <var>_NNNNN            (legacy PREVIEW naming, 5-digit
!   field statistic  <var>_<stat>_NNNNN      flush counter starting at 1)
!   point snapshot   <id>_<var>.dat          one row per flush: t, v(1..n)
!   point statistic  <id>_<var>_<stat>.dat   in point order
!  A statistic value covers the window ending at its stamped t.  The
!  first flush (at t_start) closes a degenerate single-step window and
!  is dropped: stat output starts at the second flush (fields: _00002).
!  Field format follows the 'format' setting: 'ascii' gathers to the IO
!  rank and writes one row of M E16.6 values per J (legacy PutFileASCII
!  layout); 'binary' is a collective MPI-IO write — every rank puts its
!  interior tile at its global subarray offset in one shared file (legacy
!  PutFileBinary, Gropp lecture-33 pattern), no gather.  Both produce the
!  same bytes: the raw real(SP) global interior array in Fortran order.
!  'netcdf' gathers like ascii but appends every variable to a netcdf
!  stream (x, y, time-unlimited) whose file topology the caller picks
!  at init: one data.nc per channel (default), time-chunked
!  <id>_<t0>-<t1>.nc files spanning chunk_window seconds each (frames
!  land in the chunk covering [t0, t1)), or a group named <id> inside
!  a shared root file (diag_ncid; layout 'single').
!  Point channels default to ASCII .dat files (truncated at init);
!  format 'netcdf' instead writes a group named <id> inside the shared
!  diagnostics.nc (root handle created by the output manager, passed in
!  as diag_ncid) — per-group point/time dims keep channel cadences
!  independent in one file.  Snapshot variables carry cell_methods
!  "time: point"; statistic variables their reduction; windowed groups
!  a time_bnds pair spanning each closed window (start, end].
!
!  Call order:
!   1. init(config, grid, comm)   — after grid%setup()
!   2. step(t, dt, registry)      — every timestep from output_manager
!   3. finalize()                 — at simulation end
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module core_output_channel_mod
   use core_constants_mod, only: SP, MPI_SP, N_GHOST
   use core_comm_mod, only: type_comm
   use core_grid_mod, only: type_grid_2d
   use core_accumulators_mod, only: type_accumulator
   use core_interpolation_mod, only: type_interpolator
   use core_time_utils_mod, only: type_timing_control
   use core_field_registry_mod, only: type_field_registry
   use core_output_gatherer_mod, only: type_output_gatherer
   use netcdf
   use mpi_f08
   implicit none

   private
   public :: type_output_channel, type_var_meta, write_field_file
   public :: open_diagnostics_file, close_diagnostics_file

   integer, parameter :: VARNAME_LEN = 32
   integer, parameter :: STATNAME_LEN = 8
   integer, parameter :: ID_LEN = 64
   integer, parameter :: VARS_MAX = 32
   integer, parameter :: STATS_MAX = 4
   integer, parameter :: META_LEN = 64

   ! CF attributes for one variable; a blank component writes no attr.
   ! Filled by the model from the registry catalog (field_metadata.f90)
   type :: type_var_meta
      character(META_LEN) :: units = ''
      character(META_LEN) :: long_name = ''
      character(META_LEN) :: standard_name = ''
   end type type_var_meta

   ! Serial NetCDF backend state: one data.nc per channel, every channel
   ! variable as <var>(x, y, time) — C order (time, y, x) per the CF
   ! output design; time is the unlimited record dimension.
   type :: type_netcdf_field_writer
      integer :: ncid = -1
      integer :: time_varid = -1
      integer :: nrec = 0
      integer :: n_vars = 0
      character(VARNAME_LEN + STATNAME_LEN + 1), allocatable :: names(:)
      integer, allocatable :: varids(:)
      logical :: is_open = .false.
      ! .false. when the stream is a group in a shared root file
      ! (layout 'single'): close() only forgets, the manager closes
      logical :: owns_file = .true.
   contains
      procedure :: create => nc_create
      procedure :: begin_frame => nc_begin_frame
      procedure :: put => nc_put
      procedure :: close => nc_close
   end type type_netcdf_field_writer

   ! Serial NetCDF point backend: one group per channel inside the
   ! shared diagnostics.nc (root handle owned by the output manager —
   ! the channel only defines and fills its own group, never closes)
   type :: type_netcdf_point_writer
      integer :: grpid = -1
      integer :: time_varid = -1
      integer :: bnds_varid = -1
      integer :: nrec = 0
      integer :: n_vars = 0
      logical :: windowed = .false.
      character(VARNAME_LEN + STATNAME_LEN + 1), allocatable :: names(:)
      integer, allocatable :: varids(:)
      logical :: is_open = .false.
   contains
      procedure :: create_group => ncp_create_group
      procedure :: begin_frame => ncp_begin_frame
      procedure :: put => ncp_put
      procedure :: reset => ncp_reset
   end type type_netcdf_point_writer

   type :: type_output_channel
      character(ID_LEN)              :: id = ''
      character(ID_LEN)              :: geom_type = ''  ! 'field', 'station', 'transect'
      character(8)                   :: format = 'ascii'
      character(VARNAME_LEN)         :: variables(VARS_MAX) = ''
      ! Per-variable file-name overrides (legacy bridge: registry h_max
      ! writes legacy hmax_NNNNN); default to the registry names.
      character(VARNAME_LEN)         :: prefixes(VARS_MAX) = ''
      character(STATNAME_LEN)        :: statistics(STATS_MAX) = ''
      type(type_var_meta)            :: meta(VARS_MAX)
      integer                        :: n_vars = 0
      integer                        :: n_stats = 0
      logical                        :: snapshot = .true.
      real(SP)                       :: t_start = 0.0_SP
      real(SP)                       :: interval = 0.0_SP

      ! Output destination and flush counter
      character(:), allocatable :: result_folder
      integer                   :: icount = 0

      ! Whether the last step() call flushed (loop-top time_dt.out hook)
      logical                   :: fired = .false.

      ! Whether a flush has closed a full window yet: the first flush
      ! fires at t_start with a single accumulated step, so its
      ! statistics window is degenerate and is dropped, not written
      logical                   :: stats_primed = .false.

      ! Timing
      type(type_timing_control) :: trigger

      ! Interpolation (station/transect only)
      type(type_interpolator)   :: interp

      ! Accumulators: one per variable; each holds all requested stats
      type(type_accumulator), allocatable :: accum(:)

      ! MPI gather helper
      type(type_output_gatherer) :: gatherer

      ! NetCDF backend (field geometry, format='netcdf'; IO rank only)
      type(type_netcdf_field_writer) :: nc

      ! NetCDF point backend (station/transect, format='netcdf')
      type(type_netcdf_point_writer) :: ncp

      ! Previous flush time = the open window's start (time_bnds)
      real(SP) :: t_last_flush = 0.0_SP

      ! Chunked field layout: roll to a fresh time-aligned file when a
      ! frame crosses the window's right edge (0 = no chunking).  The
      ! defined variable set is saved for re-creation at each roll-over.
      real(SP) :: chunk_window = 0.0_SP
      real(SP) :: t_chunk0 = 0.0_SP, t_chunk1 = 0.0_SP
      real(SP) :: dx0 = 0.0_SP, dy0 = 0.0_SP
      character(VARNAME_LEN + STATNAME_LEN + 1), allocatable :: nc_names(:)
      type(type_var_meta), allocatable :: nc_meta(:)
      integer :: nc_n = 0

      ! Grid geometry (set at init for use in step/flush)
      integer :: local_nx = 0, local_ny = 0
      integer :: n_local = 0   ! local interp points (station/transect)
      integer :: n_global = 0   ! total global output points
      ! 0-based global start of this rank's interior tile (MPI-IO subarray)
      integer :: i0 = 0, j0 = 0

   contains
      procedure :: init => channel_init
      procedure :: step => channel_step
      procedure :: finalize => channel_finalize
   end type type_output_channel

contains

   subroutine channel_init(this, id, geom_type, variables, n_vars, &
                           statistics, n_stats, snapshot, t_start, interval, &
                           result_folder, format, &
                           coords_x, coords_y, n_coords, grid, comm, &
                           file_prefixes, icount_start, var_meta, diag_ncid, &
                           chunk_window)
      class(type_output_channel), intent(inout) :: this
      character(*), intent(in) :: id, geom_type
      character(*), intent(in) :: variables(*)
      integer, intent(in) :: n_vars
      character(*), intent(in) :: statistics(*)
      integer, intent(in) :: n_stats
      logical, intent(in) :: snapshot
      real(SP), intent(in) :: t_start, interval
      character(*), intent(in) :: result_folder  ! must include trailing separator
      character(*), intent(in) :: format         ! field: ascii/binary/netcdf; points: ascii/netcdf
      real(SP), intent(in) :: coords_x(*), coords_y(*)  ! global query coords
      integer, intent(in) :: n_coords   ! n_stations or n_transect_points (0 for field)
      type(type_grid_2d), intent(in)    :: grid
      type(type_comm), intent(inout) :: comm
      character(*), intent(in), optional :: file_prefixes(*)  ! per-var name overrides
      integer, intent(in), optional :: icount_start  ! pre-increment counter base
      type(type_var_meta), intent(in), optional :: var_meta(*)  ! per-var CF attrs
      ! Shared root file handle: netcdf point channels always; the field
      ! channel only under layout 'single' (stream becomes a group)
      integer, intent(in), optional :: diag_ncid
      ! Field netcdf layout 'chunked': time span per file (s)
      real(SP), intent(in), optional :: chunk_window

      integer :: iv, is
      integer, allocatable :: pids(:)

      this%id = id
      this%geom_type = geom_type
      this%format = format
      this%snapshot = snapshot
      this%t_start = t_start
      this%interval = interval
      this%n_vars = n_vars
      this%n_stats = n_stats
      this%local_nx = grid%local_nx
      this%local_ny = grid%local_ny
      this%result_folder = trim(result_folder)
      this%icount = 0
      if (present(icount_start)) this%icount = icount_start

      ! loud guard: a release build without bounds checking would silently
      ! corrupt neighbouring components instead
      if (n_vars > VARS_MAX) error stop "output_channel: variable list exceeds VARS_MAX"
      if (n_stats > STATS_MAX) error stop "output_channel: statistics list exceeds STATS_MAX"

      this%variables(1:n_vars) = variables(1:n_vars)
      if (present(file_prefixes)) then
         this%prefixes(1:n_vars) = file_prefixes(1:n_vars)
      else
         this%prefixes(1:n_vars) = variables(1:n_vars)
      end if
      this%statistics(1:n_stats) = statistics(1:n_stats)
      if (present(var_meta)) this%meta(1:n_vars) = var_meta(1:n_vars)

      ! Timing control
      this%trigger%t_start = t_start
      this%trigger%interval = interval
      this%trigger%last_triggered = -1.0_SP

      ! Geometry-specific setup
      select case (trim(geom_type))
      case ('station', 'transect')
         ! Bilinear interpolation from global query coords
         call this%interp%init(coords_x(1:n_coords), coords_y(1:n_coords), grid)
         this%n_local = this%interp%n_points
         this%n_global = n_coords
         if (this%n_local > 0) then
            pids = this%interp%point_id
         else
            allocate (pids(0))
         end if
         call this%gatherer%init_points(n_coords, this%n_local, comm, local_ids=pids)

         if (trim(format) == 'netcdf') then
            this%t_last_flush = t_start
            if (comm%is_io_node()) then
               if (.not. present(diag_ncid)) &
                  error stop 'output_channel: netcdf point channel needs diag_ncid'
               call init_netcdf_points(this, diag_ncid, &
                                       coords_x(1:n_coords), coords_y(1:n_coords))
            end if
         else
            ! Point files append per flush; start each run from empty files.
            if (comm%is_io_node()) call truncate_point_files(this)
         end if

         ! Accumulators: (n_local, 1)
         allocate (this%accum(n_vars))
         do iv = 1, n_vars
            call this%accum(iv)%init(this%n_local, 1, trim(variables(iv)))
            do is = 1, n_stats
               call this%accum(iv)%allocate_stat(trim(statistics(is)))
            end do
         end do

      case ('field')
         this%n_local = grid%local_nx*grid%local_ny
         this%n_global = grid%M*grid%N
         this%i0 = grid%ibegin - 1
         this%j0 = grid%jbegin - 1
         call this%gatherer%init_field(grid, comm)

         ! Accumulators: (local_nx, local_ny)
         allocate (this%accum(n_vars))
         do iv = 1, n_vars
            call this%accum(iv)%init(grid%local_nx, grid%local_ny, trim(variables(iv)))
            do is = 1, n_stats
               call this%accum(iv)%allocate_stat(trim(statistics(is)))
            end do
         end do

         ! NetCDF backend: every snapshot + statistic variable defined
         ! up front (names fixed at init).  Layout: a shared-root group
         ! (diag_ncid), time-chunked files (chunk_window), or one
         ! data.nc per channel.
         if (trim(format) == 'netcdf') then
            this%dx0 = grid%dx0
            this%dy0 = grid%dy0
            if (present(chunk_window)) this%chunk_window = chunk_window
            this%t_chunk0 = t_start
            this%t_chunk1 = t_start + this%chunk_window
            if (comm%is_io_node()) call init_netcdf_backend(this, grid, diag_ncid)
         end if

      case default
         error stop 'type_output_channel: unknown geometry type: '//trim(geom_type)
      end select

   end subroutine channel_init

   ! Called every timestep. Accumulates from registry; flushes when triggered.
   ! force=.true. (after-loop final flush) fires unconditionally once the
   ! channel has started.
   subroutine channel_step(this, t, dt, registry, comm, force)
      class(type_output_channel), intent(inout) :: this
      real(SP), intent(in)    :: t, dt
      type(type_field_registry), intent(in)    :: registry
      type(type_comm), intent(inout) :: comm
      logical, intent(in), optional :: force

      integer  :: iv
      real(SP), pointer :: fld(:, :)
      real(SP), allocatable :: interp_vals(:), interp_2d(:, :)
      logical :: do_flush

      this%fired = .false.
      if (t < this%t_start) return

      ! dt-accumulator mode: legacy PLOT_COUNT frame cadence
      do_flush = this%trigger%should_trigger(t, dt)
      if (present(force)) do_flush = do_flush .or. force
      if (do_flush) this%icount = this%icount + 1
      this%fired = do_flush

      ! Chunked field stream: a frame at or past the window's right
      ! edge rolls to the next time-aligned file first (chunks cover
      ! [t0, t1); a frame at exactly t1 opens the next chunk)
      if (do_flush .and. this%chunk_window > 0.0_SP .and. this%nc%is_open) then
         if (t >= this%t_chunk1) then
            call this%nc%close()
            do while (t >= this%t_chunk1)
               this%t_chunk0 = this%t_chunk1
               this%t_chunk1 = this%t_chunk1 + this%chunk_window
            end do
            call create_chunk_file(this)
         end if
      end if

      ! One record per flush: stamp the time value before any variable
      ! lands (snapshot and statistics share the frame)
      if (do_flush .and. this%nc%is_open) call this%nc%begin_frame(t)
      ! Point groups: a windowed channel's first flush writes nothing
      ! (degenerate window) — advance the record only once primed
      if (do_flush .and. this%ncp%is_open .and. &
          (this%snapshot .or. this%stats_primed)) &
         call this%ncp%begin_frame(t, this%t_last_flush)

      ! --- Snapshot: write current field directly from registry ---
      if (do_flush .and. this%snapshot) then
         do iv = 1, this%n_vars
            fld => registry%get(trim(this%variables(iv)))
            call channel_write_snapshot(this, iv, fld, t, comm)
         end do
      end if

      ! --- Accumulate for statistics ---
      if (this%n_stats > 0) then
         select case (trim(this%geom_type))
         case ('station', 'transect')
            allocate (interp_vals(this%n_local), interp_2d(this%n_local, 1))
            do iv = 1, this%n_vars
               fld => registry%get(trim(this%variables(iv)))
               call this%interp%gather(fld, interp_vals)
               interp_2d(:, 1) = interp_vals
               call this%accum(iv)%accumulate(interp_2d, dt)
            end do
            deallocate (interp_vals, interp_2d)

         case ('field')
            do iv = 1, this%n_vars
               fld => registry%get(trim(this%variables(iv)))
               ! Slice interior (ghost-inclusive field → interior only)
               associate (ng => N_GHOST, nx => this%local_nx, ny => this%local_ny)
                  call this%accum(iv)%accumulate( &
                     fld(ng + 1:ng + nx, ng + 1:ng + ny), dt)
               end associate
            end do
         end select
      end if

      ! --- Flush statistics at interval (first flush closes a
      !     degenerate single-step window: reset without writing) ---
      if (do_flush .and. this%n_stats > 0) then
         do iv = 1, this%n_vars
            if (this%stats_primed) call channel_write_stats(this, iv, t, comm)
            call this%accum(iv)%reset()
         end do
      end if
      if (do_flush) this%stats_primed = .true.
      if (do_flush) this%t_last_flush = t

   end subroutine channel_step

   ! Write a snapshot of one variable. Host-only I/O on IO rank.
   subroutine channel_write_snapshot(this, iv, fld, t, comm)
      class(type_output_channel), intent(inout) :: this
      integer, intent(in) :: iv
      real(SP), pointer, intent(in) :: fld(:, :)
      real(SP), intent(in) :: t
      type(type_comm), intent(inout) :: comm

      real(SP), allocatable :: local_vals(:)

      select case (trim(this%geom_type))
      case ('field')
         associate (ng => N_GHOST, nx => this%local_nx, ny => this%local_ny)
            call channel_flush_field(this, fld(ng + 1:ng + nx, ng + 1:ng + ny), &
                                     trim(this%prefixes(iv)), comm)
         end associate
      case ('station', 'transect')
         allocate (local_vals(this%n_local))
         call this%interp%gather(fld, local_vals)
         call channel_flush_points(this, local_vals, &
                                   trim(this%prefixes(iv)), t, comm)
      end select
   end subroutine channel_write_snapshot

   ! Write all accumulated statistics for variable iv. Host-only I/O on IO rank.
   subroutine channel_write_stats(this, iv, t, comm)
      class(type_output_channel), intent(inout) :: this
      integer, intent(in)    :: iv
      real(SP), intent(in)    :: t
      type(type_comm), intent(inout) :: comm

      integer :: is
      character(:), allocatable :: name
      real(SP), allocatable :: stat_vals(:, :)

      do is = 1, this%n_stats
         stat_vals = this%accum(iv)%get_stat(trim(this%statistics(is)))
         name = trim(this%prefixes(iv))//'_'//trim(this%statistics(is))
         select case (trim(this%geom_type))
         case ('field')
            call channel_flush_field(this, stat_vals, name, comm)
         case ('station', 'transect')
            call channel_flush_points(this, stat_vals(:, 1), name, t, comm)
         end select
      end do
   end subroutine channel_write_stats

   ! Write one field-geometry interior array as <name>_NNNNN.
   ! binary: collective MPI-IO, every rank writes its tile in place;
   ! ascii: gather to the IO rank, serial formatted write.
   subroutine channel_flush_field(this, vals, name, comm)
      class(type_output_channel), intent(inout) :: this
      real(SP), intent(in)    :: vals(:, :)   ! (local_nx, local_ny)
      character(*), intent(in)    :: name
      type(type_comm), intent(inout) :: comm

      real(SP), allocatable :: glob(:, :)
      character(5) :: cnt

      write (cnt, '(I5.5)') this%icount

      if (trim(this%format) == 'binary') then
         call write_field_file_mpiio(this%result_folder//name//'_'//cnt, &
                                     vals, this%gatherer%M, this%gatherer%N, &
                                     this%i0, this%j0, comm)
         return
      end if

      if (comm%is_io_node()) then
         allocate (glob(this%gatherer%M, this%gatherer%N))
      else
         allocate (glob(1, 1))
      end if
      call this%gatherer%gather_field(vals, glob, comm)

      if (comm%is_io_node()) then
         if (trim(this%format) == 'netcdf') then
            call this%nc%put(name, glob)
         else
            call write_field_file(this%result_folder//name//'_'//cnt, &
                                  glob, trim(this%format))
         end if
      end if
   end subroutine channel_flush_field

   ! Define the channel's field stream: snapshot variables under their
   ! file prefixes plus every <prefix>_<stat> combination.  The set is
   ! saved on the channel so chunked roll-overs can re-create it.
   ! IO rank only.
   subroutine init_netcdf_backend(this, grid, diag_ncid)
      class(type_output_channel), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      integer, intent(in), optional :: diag_ncid

      integer :: iv, is, n, root

      root = -1
      if (present(diag_ncid)) root = diag_ncid

      ! statistic variables inherit the base variable's attrs
      allocate (this%nc_names(this%n_vars*(1 + this%n_stats)))
      allocate (this%nc_meta(this%n_vars*(1 + this%n_stats)))
      n = 0
      do iv = 1, this%n_vars
         if (this%snapshot) then
            n = n + 1
            this%nc_names(n) = trim(this%prefixes(iv))
            this%nc_meta(n) = this%meta(iv)
         end if
         do is = 1, this%n_stats
            n = n + 1
            this%nc_names(n) = trim(this%prefixes(iv))//'_'//trim(this%statistics(is))
            this%nc_meta(n) = this%meta(iv)
         end do
      end do
      this%nc_n = n

      if (root >= 0) then
         ! layout 'single': the stream is a group in the shared root
         call this%nc%create(trim(this%id), grid%M, grid%N, &
                             grid%dx0, grid%dy0, this%nc_names, &
                             this%nc_meta, n, root=root)
      else if (this%chunk_window > 0.0_SP) then
         call create_chunk_file(this)
      else
         call this%nc%create(this%result_folder//'data.nc', grid%M, grid%N, &
                             grid%dx0, grid%dy0, this%nc_names, this%nc_meta, n)
      end if
   end subroutine init_netcdf_backend

   ! Open the chunk covering [t_chunk0, t_chunk1); the name carries the
   ! window bounds (zero-padded seconds, deterministic and sortable)
   subroutine create_chunk_file(this)
      class(type_output_channel), intent(inout) :: this
      call this%nc%create(this%result_folder//trim(this%id)//'_'// &
                          chunk_stamp(this%t_chunk0)//'-'// &
                          chunk_stamp(this%t_chunk1)//'.nc', &
                          this%gatherer%M, this%gatherer%N, &
                          this%dx0, this%dy0, this%nc_names, &
                          this%nc_meta, this%nc_n)
   end subroutine create_chunk_file

   ! Zero-padded seconds (F edit descriptors cannot zero-fill)
   pure function chunk_stamp(t) result(s)
      real(SP), intent(in) :: t
      character(10) :: s
      integer :: i
      write (s, '(F10.1)') t
      do i = 1, len(s)
         if (s(i:i) == ' ') s(i:i) = '0'
      end do
   end function chunk_stamp

   ! Define the channel's group in the shared diagnostics.nc: snapshot
   ! variables plus every <prefix>_<stat>, each tagged with its CF
   ! cell_methods.  IO rank only.
   subroutine init_netcdf_points(this, diag_ncid, x, y)
      class(type_output_channel), intent(inout) :: this
      integer, intent(in) :: diag_ncid
      real(SP), intent(in) :: x(:), y(:)

      character(VARNAME_LEN + STATNAME_LEN + 1), allocatable :: names(:)
      character(32), allocatable :: methods(:)
      type(type_var_meta), allocatable :: vmeta(:)
      integer :: iv, is, n

      ! statistic variables inherit the base variable's attrs
      allocate (names(this%n_vars*(1 + this%n_stats)))
      allocate (methods(this%n_vars*(1 + this%n_stats)))
      allocate (vmeta(this%n_vars*(1 + this%n_stats)))
      n = 0
      do iv = 1, this%n_vars
         if (this%snapshot) then
            n = n + 1
            names(n) = trim(this%prefixes(iv))
            methods(n) = 'time: point'
            vmeta(n) = this%meta(iv)
         end if
         do is = 1, this%n_stats
            n = n + 1
            names(n) = trim(this%prefixes(iv))//'_'//trim(this%statistics(is))
            methods(n) = stat_cell_method(trim(this%statistics(is)))
            vmeta(n) = this%meta(iv)
         end do
      end do

      call this%ncp%create_group(diag_ncid, trim(this%id), x, y, &
                                 names, vmeta, methods, n, this%n_stats > 0)
   end subroutine init_netcdf_points

   ! CF cell_methods label for one accumulator statistic
   pure function stat_cell_method(stat) result(cm)
      character(*), intent(in) :: stat
      character(:), allocatable :: cm
      select case (stat)
      case ('min')
         cm = 'time: minimum'
      case ('max')
         cm = 'time: maximum'
      case ('mean')
         cm = 'time: mean'
      case default   ! 'rms'
         cm = 'time: root_mean_square'
      end select
   end function stat_cell_method

   ! Collective MPI-IO twin of write_field_file's binary branch (legacy
   ! PutFileBinary, after Gropp lecture 33): the file view maps each
   ! rank's (local_nx, local_ny) interior tile to its 0-based (i0, j0)
   ! subarray offset in the global (M, N) array, then one write_all puts
   ! every tile concurrently — byte-identical to the gathered stream.
   subroutine write_field_file_mpiio(fname, vals, M, N, i0, j0, comm)
      character(*), intent(in) :: fname
      real(SP), intent(in) :: vals(:, :)   ! interior tile, ghost-free
      integer, intent(in) :: M, N, i0, j0
      type(type_comm), intent(inout) :: comm

      type(MPI_Datatype) :: ftype
      type(MPI_File) :: fh
      integer(MPI_OFFSET_KIND) :: zero_off
      integer :: ierr

      call MPI_Type_create_subarray(2, [M, N], shape(vals), [i0, j0], &
                                    MPI_ORDER_FORTRAN, MPI_SP, ftype, ierr)
      call MPI_Type_commit(ftype, ierr)

      call MPI_File_open(comm%id, fname, MPI_MODE_WRONLY + MPI_MODE_CREATE, &
                         MPI_INFO_NULL, fh, ierr)
      ! MPI_MODE_CREATE does not truncate: a rerun over a larger stale
      ! file (e.g. prior ASCII output) would keep a garbage tail
      zero_off = 0
      call MPI_File_set_size(fh, zero_off, ierr)
      call MPI_Barrier(comm%id, ierr)
      call MPI_File_set_view(fh, zero_off, MPI_SP, ftype, 'native', &
                             MPI_INFO_NULL, ierr)
      call MPI_File_write_all(fh, vals, size(vals), MPI_SP, &
                              MPI_STATUS_IGNORE, ierr)
      call MPI_File_close(fh, ierr)
      call MPI_Type_free(ftype, ierr)
   end subroutine write_field_file_mpiio

   ! Gather one point-geometry value set and append a "t, v(1..n)" row
   ! (in point order) to <id>_<name>.dat on the IO rank.
   subroutine channel_flush_points(this, local_vals, name, t, comm)
      class(type_output_channel), intent(inout) :: this
      real(SP), intent(in)    :: local_vals(:)   ! (n_local)
      character(*), intent(in)    :: name
      real(SP), intent(in)    :: t
      type(type_comm), intent(inout) :: comm

      real(SP), allocatable :: gathered(:), sorted(:)
      integer :: k, unit

      allocate (gathered(merge(this%n_global, 1, comm%is_io_node())))
      call this%gatherer%gather_vals(local_vals, gathered, comm)

      if (comm%is_io_node()) then
         ! Restore point order: gathered is rank-ordered.
         allocate (sorted(this%n_global), source=0.0_SP)
         do k = 1, this%n_global
            sorted(this%gatherer%point_ids(k)) = gathered(k)
         end do
         if (this%ncp%is_open) then
            ! Time already stamped by the frame the step opened
            call this%ncp%put(name, sorted)
         else
            open (newunit=unit, file=point_file_name(this, name), &
                  status='unknown', position='append', action='write')
            write (unit, '(*(E16.6))') t, sorted
            close (unit)
         end if
      end if
   end subroutine channel_flush_points

   function point_file_name(this, name) result(fname)
      class(type_output_channel), intent(in) :: this
      character(*), intent(in) :: name
      character(:), allocatable :: fname
      fname = this%result_folder//trim(this%id)//'_'//trim(name)//'.dat'
   end function point_file_name

   ! Truncate all point files this channel will append to (IO rank only).
   subroutine truncate_point_files(this)
      class(type_output_channel), intent(in) :: this
      integer :: iv, is, unit

      do iv = 1, this%n_vars
         if (this%snapshot) then
            open (newunit=unit, file=point_file_name(this, trim(this%prefixes(iv))), &
                  status='replace', action='write')
            close (unit)
         end if
         do is = 1, this%n_stats
            open (newunit=unit, file=point_file_name(this, &
                                                     trim(this%prefixes(iv))//'_'//trim(this%statistics(is))), &
                  status='replace', action='write')
            close (unit)
         end do
      end do
   end subroutine truncate_point_files

   ! Write one global interior array. ascii: one row of M E16.6 values per J
   ! (legacy PutFileASCII layout); binary: raw real(SP) stream, Fortran order.
   subroutine write_field_file(fname, g, format)
      character(*), intent(in) :: fname
      real(SP), intent(in) :: g(:, :)
      character(*), intent(in) :: format

      integer :: j, unit
      character(20) :: row_fmt

      select case (format)
      case ('binary')
         open (newunit=unit, file=fname, access='stream', &
               form='unformatted', status='replace', action='write')
         write (unit) g
         close (unit)
      case default   ! 'ascii'
         write (row_fmt, '(A,I0,A)') '(', size(g, 1), 'E16.6)'
         open (newunit=unit, file=fname, status='replace', action='write')
         do j = 1, size(g, 2)
            write (unit, row_fmt) g(:, j)
         end do
         close (unit)
      end select
   end subroutine write_field_file

   ! ---- NetCDF backend (serial: caller gathers, IO rank writes) ----

   subroutine nc_check(status, what)
      integer, intent(in) :: status
      character(*), intent(in) :: what
      if (status /= NF90_NOERR) then
         write (*, '(A)') 'output_channel/netcdf: '//what//': '// &
            trim(nf90_strerror(status))
         error stop 'output_channel: fatal NetCDF error'
      end if
   end subroutine nc_check

   ! Define the file: dims (x, y, time-unlimited), center coordinates,
   ! one SP-kind variable per name with its CF attrs.  Clobbers any
   ! existing file.  With root, fname names a GROUP defined in that
   ! shared file instead (layout 'single'; the root owner closes).
   subroutine nc_create(this, fname, M, N, dx, dy, names, meta, n_names, root)
      class(type_netcdf_field_writer), intent(inout) :: this
      character(*), intent(in) :: fname
      integer, intent(in) :: M, N
      real(SP), intent(in) :: dx, dy
      character(*), intent(in) :: names(:)
      type(type_var_meta), intent(in) :: meta(:)
      integer, intent(in) :: n_names
      integer, intent(in), optional :: root

      integer :: x_dim, y_dim, t_dim, x_var, y_var
      integer :: i
      real(SP), allocatable :: coord(:)

      this%owns_file = .not. present(root)
      if (this%owns_file) then
         call nc_check(nf90_create(fname, ior(NF90_CLOBBER, NF90_NETCDF4), &
                                   this%ncid), 'create '//fname)
      else
         call nc_check(nf90_def_grp(root, fname, this%ncid), &
                       'def group '//fname)
      end if

      call nc_check(nf90_def_dim(this%ncid, 'x', M, x_dim), 'def x')
      call nc_check(nf90_def_dim(this%ncid, 'y', N, y_dim), 'def y')
      call nc_check(nf90_def_dim(this%ncid, 'time', NF90_UNLIMITED, t_dim), &
                    'def time')

      call nc_check(nf90_def_var(this%ncid, 'x', NF90_DOUBLE, [x_dim], &
                                 x_var), 'def var x')
      call nc_check(nf90_put_att(this%ncid, x_var, 'units', 'm'), &
                    'att x units')
      call nc_check(nf90_def_var(this%ncid, 'y', NF90_DOUBLE, [y_dim], &
                                 y_var), 'def var y')
      call nc_check(nf90_put_att(this%ncid, y_var, 'units', 'm'), &
                    'att y units')
      call nc_check(nf90_def_var(this%ncid, 'time', NF90_DOUBLE, [t_dim], &
                                 this%time_varid), 'def var time')
      call nc_check(nf90_put_att(this%ncid, this%time_varid, 'units', &
                                 'seconds since start'), 'att time units')

      this%n_vars = n_names
      allocate (this%names(n_names), this%varids(n_names))
      do i = 1, n_names
         this%names(i) = names(i)
         call nc_check(nf90_def_var(this%ncid, trim(names(i)), NF90_DOUBLE, &
                                    [x_dim, y_dim, t_dim], this%varids(i)), &
                       'def var '//trim(names(i)))
         if (len_trim(meta(i)%units) > 0) &
            call nc_check(nf90_put_att(this%ncid, this%varids(i), 'units', &
                                       trim(meta(i)%units)), &
                          'att units '//trim(names(i)))
         if (len_trim(meta(i)%long_name) > 0) &
            call nc_check(nf90_put_att(this%ncid, this%varids(i), 'long_name', &
                                       trim(meta(i)%long_name)), &
                          'att long_name '//trim(names(i)))
         if (len_trim(meta(i)%standard_name) > 0) &
            call nc_check(nf90_put_att(this%ncid, this%varids(i), 'standard_name', &
                                       trim(meta(i)%standard_name)), &
                          'att standard_name '//trim(names(i)))
      end do

      ! group mode: the root already carries the global attrs, and a
      ! NETCDF4 root needs no define/data mode juggling
      if (this%owns_file) then
         call nc_check(nf90_put_att(this%ncid, NF90_GLOBAL, 'Conventions', &
                                    'CF-1.8'), 'att Conventions')
         call nc_check(nf90_put_att(this%ncid, NF90_GLOBAL, 'source', &
                                    'FUNWAVE-TVD'), 'att source')
         call nc_check(nf90_enddef(this%ncid), 'enddef')
      end if

      ! cell-center coordinates on the uniform spacing
      allocate (coord(max(M, N)))
      do i = 1, M
         coord(i) = real(i - 1, SP)*dx
      end do
      call nc_check(nf90_put_var(this%ncid, x_var, coord(1:M)), 'put x')
      do i = 1, N
         coord(i) = real(i - 1, SP)*dy
      end do
      call nc_check(nf90_put_var(this%ncid, y_var, coord(1:N)), 'put y')

      this%nrec = 0
      this%is_open = .true.
   end subroutine nc_create

   ! Advance the record dimension and stamp its time value.
   subroutine nc_begin_frame(this, t)
      class(type_netcdf_field_writer), intent(inout) :: this
      real(SP), intent(in) :: t
      this%nrec = this%nrec + 1
      call nc_check(nf90_put_var(this%ncid, this%time_varid, [t], &
                                 start=[this%nrec]), 'put time')
   end subroutine nc_begin_frame

   ! Write one variable's global interior array at the current record.
   subroutine nc_put(this, name, glob)
      class(type_netcdf_field_writer), intent(inout) :: this
      character(*), intent(in) :: name
      real(SP), intent(in) :: glob(:, :)

      integer :: i, id

      id = -1
      do i = 1, this%n_vars
         if (trim(this%names(i)) == trim(name)) then
            id = this%varids(i)
            exit
         end if
      end do
      if (id < 0) &
         error stop 'output_channel: netcdf put of undefined variable '//name

      call nc_check(nf90_put_var(this%ncid, id, glob, &
                                 start=[1, 1, this%nrec]), 'put '//trim(name))
   end subroutine nc_put

   subroutine nc_close(this)
      class(type_netcdf_field_writer), intent(inout) :: this
      if (this%is_open .and. this%owns_file) &
         call nc_check(nf90_close(this%ncid), 'close')
      this%is_open = .false.
      this%owns_file = .true.
      this%ncid = -1
      this%nrec = 0
      this%n_vars = 0
      if (allocated(this%names)) deallocate (this%names)
      if (allocated(this%varids)) deallocate (this%varids)
   end subroutine nc_close

   ! Create/close the shared diagnostics.nc root (owned by the output
   ! manager; each netcdf point channel defines one group).  IO rank only.
   function open_diagnostics_file(fname) result(ncid)
      character(*), intent(in) :: fname
      integer :: ncid
      call nc_check(nf90_create(fname, ior(NF90_CLOBBER, NF90_NETCDF4), &
                                ncid), 'create '//fname)
      call nc_check(nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', &
                                 'CF-1.8'), 'att Conventions')
      call nc_check(nf90_put_att(ncid, NF90_GLOBAL, 'source', &
                                 'FUNWAVE-TVD'), 'att source')
   end function open_diagnostics_file

   subroutine close_diagnostics_file(ncid)
      integer, intent(in) :: ncid
      call nc_check(nf90_close(ncid), 'close diagnostics.nc')
   end subroutine close_diagnostics_file

   ! Define the group: dims (point, time-unlimited), the resolved query
   ! coords once, one variable per name with its CF attrs; windowed
   ! groups add time_bnds(bnds, time) spanning each closed window.
   ! NETCDF4 files need no define/data mode juggling across groups.
   subroutine ncp_create_group(this, root, id, x, y, names, meta, &
                               methods, n_names, windowed)
      class(type_netcdf_point_writer), intent(inout) :: this
      integer, intent(in) :: root
      character(*), intent(in) :: id
      real(SP), intent(in) :: x(:), y(:)
      character(*), intent(in) :: names(:)
      type(type_var_meta), intent(in) :: meta(:)
      character(*), intent(in) :: methods(:)
      integer, intent(in) :: n_names
      logical, intent(in) :: windowed

      integer :: p_dim, t_dim, b_dim, x_var, y_var
      integer :: i

      call nc_check(nf90_def_grp(root, id, this%grpid), 'def group '//id)

      call nc_check(nf90_def_dim(this%grpid, 'point', size(x), p_dim), &
                    'def point '//id)
      call nc_check(nf90_def_dim(this%grpid, 'time', NF90_UNLIMITED, &
                                 t_dim), 'def time '//id)

      call nc_check(nf90_def_var(this%grpid, 'x', NF90_DOUBLE, [p_dim], &
                                 x_var), 'def var x '//id)
      call nc_check(nf90_put_att(this%grpid, x_var, 'units', 'm'), &
                    'att x units '//id)
      call nc_check(nf90_def_var(this%grpid, 'y', NF90_DOUBLE, [p_dim], &
                                 y_var), 'def var y '//id)
      call nc_check(nf90_put_att(this%grpid, y_var, 'units', 'm'), &
                    'att y units '//id)
      call nc_check(nf90_def_var(this%grpid, 'time', NF90_DOUBLE, [t_dim], &
                                 this%time_varid), 'def var time '//id)
      call nc_check(nf90_put_att(this%grpid, this%time_varid, 'units', &
                                 'seconds since start'), 'att time units '//id)

      this%windowed = windowed
      if (windowed) then
         call nc_check(nf90_def_dim(this%grpid, 'bnds', 2, b_dim), &
                       'def bnds '//id)
         call nc_check(nf90_def_var(this%grpid, 'time_bnds', NF90_DOUBLE, &
                                    [b_dim, t_dim], this%bnds_varid), &
                       'def var time_bnds '//id)
         call nc_check(nf90_put_att(this%grpid, this%time_varid, 'bounds', &
                                    'time_bnds'), 'att time bounds '//id)
      end if

      this%n_vars = n_names
      allocate (this%names(n_names), this%varids(n_names))
      do i = 1, n_names
         this%names(i) = names(i)
         call nc_check(nf90_def_var(this%grpid, trim(names(i)), NF90_DOUBLE, &
                                    [p_dim, t_dim], this%varids(i)), &
                       'def var '//trim(names(i)))
         call nc_check(nf90_put_att(this%grpid, this%varids(i), &
                                    'cell_methods', trim(methods(i))), &
                       'att cell_methods '//trim(names(i)))
         if (len_trim(meta(i)%units) > 0) &
            call nc_check(nf90_put_att(this%grpid, this%varids(i), 'units', &
                                       trim(meta(i)%units)), &
                          'att units '//trim(names(i)))
         if (len_trim(meta(i)%long_name) > 0) &
            call nc_check(nf90_put_att(this%grpid, this%varids(i), 'long_name', &
                                       trim(meta(i)%long_name)), &
                          'att long_name '//trim(names(i)))
         if (len_trim(meta(i)%standard_name) > 0) &
            call nc_check(nf90_put_att(this%grpid, this%varids(i), 'standard_name', &
                                       trim(meta(i)%standard_name)), &
                          'att standard_name '//trim(names(i)))
      end do

      call nc_check(nf90_put_var(this%grpid, x_var, x), 'put x '//id)
      call nc_check(nf90_put_var(this%grpid, y_var, y), 'put y '//id)

      this%nrec = 0
      this%is_open = .true.
   end subroutine ncp_create_group

   ! Advance the group's record, stamp its time and, for windowed
   ! channels, the closed window (t0, t]
   subroutine ncp_begin_frame(this, t, t0)
      class(type_netcdf_point_writer), intent(inout) :: this
      real(SP), intent(in) :: t, t0
      this%nrec = this%nrec + 1
      call nc_check(nf90_put_var(this%grpid, this%time_varid, [t], &
                                 start=[this%nrec]), 'put point time')
      if (this%windowed) &
         call nc_check(nf90_put_var(this%grpid, this%bnds_varid, &
                                    reshape([t0, t], [2, 1]), &
                                    start=[1, this%nrec]), 'put time_bnds')
   end subroutine ncp_begin_frame

   ! Write one variable's point-ordered values at the current record.
   subroutine ncp_put(this, name, vals)
      class(type_netcdf_point_writer), intent(inout) :: this
      character(*), intent(in) :: name
      real(SP), intent(in) :: vals(:)

      integer :: i, id

      id = -1
      do i = 1, this%n_vars
         if (trim(this%names(i)) == trim(name)) then
            id = this%varids(i)
            exit
         end if
      end do
      if (id < 0) &
         error stop 'output_channel: netcdf put of undefined variable '//name

      call nc_check(nf90_put_var(this%grpid, id, vals, &
                                 start=[1, this%nrec]), 'put '//trim(name))
   end subroutine ncp_put

   ! Forget the group; the manager owns and closes the root file
   subroutine ncp_reset(this)
      class(type_netcdf_point_writer), intent(inout) :: this
      this%is_open = .false.
      this%grpid = -1
      this%time_varid = -1
      this%bnds_varid = -1
      this%nrec = 0
      this%n_vars = 0
      this%windowed = .false.
      if (allocated(this%names)) deallocate (this%names)
      if (allocated(this%varids)) deallocate (this%varids)
   end subroutine ncp_reset

   subroutine channel_finalize(this)
      class(type_output_channel), intent(inout) :: this
      integer :: iv
      call this%interp%finalize()
      call this%gatherer%finalize()
      call this%nc%close()
      call this%ncp%reset()
      if (allocated(this%accum)) then
         do iv = 1, size(this%accum)
            call this%accum(iv)%finalize()
         end do
         deallocate (this%accum)
      end if
      if (allocated(this%result_folder)) deallocate (this%result_folder)
      if (allocated(this%nc_names)) deallocate (this%nc_names)
      if (allocated(this%nc_meta)) deallocate (this%nc_meta)
      this%nc_n = 0
      this%chunk_window = 0.0_SP
      this%n_vars = 0
      this%n_stats = 0
      this%icount = 0
   end subroutine channel_finalize

end module core_output_channel_mod
