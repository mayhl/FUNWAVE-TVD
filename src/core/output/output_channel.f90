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
!  a shared root file (diag_ncid; layout 'single').  'pnetcdf' writes
!  the same field stream as classic CDF-5 with every rank putting its
!  interior tile collectively (no gather) — no groups, so points stay
!  serial and layout 'single' is rejected upstream.
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
   use core_path_mod, only: type_path
   use core_output_gatherer_mod, only: type_output_gatherer
   use netcdf
   ! shared NF90_* constants come from the netcdf module (PnetCDF's F90
   ! layer mirrors the same values); only the CDF-5 cmode is PnetCDF-own
   use pnetcdf, only: nf90mpi_create, nf90mpi_def_dim, nf90mpi_def_var, &
                      nf90mpi_put_att, nf90mpi_enddef, nf90mpi_put_var_all, &
                      nf90mpi_close, nf90mpi_strerror, &
                      PNC_64BIT_DATA => NF90_64BIT_DATA
   use mpi_f08
   implicit none

   private
   public :: type_output_channel, type_var_meta, write_field_file
   public :: open_diagnostics_file, close_diagnostics_file

   integer, parameter :: VARNAME_LEN = 32
   integer, parameter :: STATNAME_LEN = 12
   integer, parameter :: THRTAG_LEN = 16
   ! the event class: per-threshold accumulators, wet samples only
   character(len=STATNAME_LEN), parameter :: EVENT_STATS(5) = &
                                             [character(len=STATNAME_LEN) :: "first_time", "last_time", &
                                                                              "duration", "duration_max", "count"]
   integer, parameter :: ID_LEN = 64
   integer, parameter :: VARS_MAX = 32
   integer, parameter :: STATS_MAX = 5
   integer, parameter :: META_LEN = 64
   integer, parameter :: FLAGS_MAX = 4
   integer, parameter :: COMMENT_LEN = 160

   ! CF attributes for one variable; a blank component writes no attr.
   ! Filled by the model from the registry catalog (field_metadata.f90).
   ! funwave_name is the in-house CF-style name of a variable with no
   ! standard_name (its own attribute, never standard_name -- checkers
   ! validate that against the CF table); flag_values/flag_meanings are
   ! the CF 3.5 coded-value pair, n_flags = 0 for a plain quantity.  A
   ! statistic of a flag is not a flag: the writers drop the pair there.
   ! comment is CF 2.6.2 free text (semantics, method, caveats).
   type :: type_var_meta
      character(META_LEN) :: units = ''
      character(META_LEN) :: long_name = ''
      character(META_LEN) :: standard_name = ''
      character(META_LEN) :: funwave_name = ''
      character(COMMENT_LEN) :: comment = ''
      ! CF `coordinates`: the scalar coordinate variable(s) of a statistic
      ! (the event threshold), space separated
      character(META_LEN) :: coordinates = ''
      ! `_FillValue` (the registry fill): written when has_fill, in the
      ! variable's own type, before enddef
      logical :: has_fill = .false.
      real(SP) :: fill_value = 0.0_SP
      character(META_LEN) :: flag_meanings = ''
      integer :: n_flags = 0
      real(SP) :: flag_values(FLAGS_MAX) = 0.0_SP
   end type type_var_meta

   ! A scalar coordinate variable (CF 5.7): the event threshold beside
   ! the statistics that name it in `coordinates`; defined once per file
   ! with the base variable's units and standard_name, value written at
   ! create
   type, public :: type_scalar_coord
      character(VARNAME_LEN + THRTAG_LEN + 12) :: name = ''
      real(SP) :: value = 0.0_SP
      type(type_var_meta) :: meta
   end type type_scalar_coord

   ! Serial NetCDF backend state: one data.nc per channel, every channel
   ! variable as <var>(x, y, time) — C order (time, y, x) per the CF
   ! output design; time is the unlimited record dimension.
   type :: type_netcdf_field_writer
      integer :: ncid = -1
      integer :: time_varid = -1
      integer :: nrec = 0
      integer :: n_vars = 0
      character(VARNAME_LEN + STATNAME_LEN + THRTAG_LEN + 1), allocatable :: names(:)
      integer, allocatable :: varids(:)
      logical :: is_open = .false.
      ! data vars defined NF90_FLOAT when the channel saves single
      logical :: single = .false.
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
      character(VARNAME_LEN + STATNAME_LEN + THRTAG_LEN + 1), allocatable :: names(:)
      integer, allocatable :: varids(:)
      logical :: is_open = .false.
   contains
      procedure :: create_group => ncp_create_group
      procedure :: begin_frame => ncp_begin_frame
      procedure :: put => ncp_put
      procedure :: reset => ncp_reset
   end type type_netcdf_point_writer

   ! Parallel PnetCDF field backend (classic CDF-5): every rank writes
   ! its interior tile at its global subarray offset collectively — no
   ! gather.  Field streams only (the classic format has no groups, so
   ! points stay in the serial diagnostics.nc and layout 'single' is
   ! rejected upstream).  All methods are COLLECTIVE over comm.
   type :: type_pnetcdf_field_writer
      integer :: ncid = -1
      integer :: time_varid = -1
      integer :: nrec = 0
      integer :: n_vars = 0
      character(VARNAME_LEN + STATNAME_LEN + THRTAG_LEN + 1), allocatable :: names(:)
      integer, allocatable :: varids(:)
      logical :: is_open = .false.
      ! data vars defined NF90_FLOAT when the channel saves single
      logical :: single = .false.
   contains
      procedure :: create => pnc_create
      procedure :: begin_frame => pnc_begin_frame
      procedure :: put => pnc_put
      procedure :: close => pnc_close
   end type type_pnetcdf_field_writer

   ! Product-derived output: a flush-time formula over one accumulator
   ! product — out = scale * get_stat(stat) of variable iv (e.g. hsig =
   ! 4.004 * std(eta)).  The source variable may be hidden (accumulated
   ! but not itself written).
   type, public :: type_channel_derived
      character(VARNAME_LEN)  :: name = ''
      integer                 :: iv = 0
      character(STATNAME_LEN) :: stat = ''
      real(SP)                :: scale = 1.0_SP
   end type type_channel_derived

   integer, parameter :: DERIVED_MAX = 8
   ! event log: rows kept per rank and per accumulator between flushes
   ! (the buffer grows on demand up to it, 48 MB of rows at the cap);
   ! beyond it rows are dropped and counted.  100k lost 1790 rows on the
   ! TK spilling flume at accumulate total (130k events on one rank)
   integer, parameter :: LOG_CAP = 1000000
   integer, parameter :: LOG_NVARS = 8   ! t_on t_off duration peak x y i j

   type :: type_output_channel
      character(ID_LEN)              :: id = ''
      character(ID_LEN)              :: geom_type = ''  ! 'field', 'station', 'transect'
      character(8)                   :: format = 'ascii'
      character(VARNAME_LEN)         :: variables(VARS_MAX) = ''
      ! Per-variable file-name overrides (legacy bridge: registry h_max
      ! writes legacy hmax_NNNNN); default to the registry names.
      character(VARNAME_LEN)         :: prefixes(VARS_MAX) = ''
      character(STATNAME_LEN)        :: statistics(STATS_MAX) = ''
      ! event thresholds (one accumulator column per value), direction
      ! (THR_ABOVE/THR_BELOW), filters, the wet-sample depth floor and the
      ! per-value name suffix ('' for a single value)
      integer :: n_thr = 0
      real(SP), allocatable :: thr(:)
      integer :: thr_dir = 0
      real(SP) :: gap = 0.0_SP
      real(SP) :: min_duration = 0.0_SP
      real(SP) :: wet_floor = 0.0_SP
      character(THRTAG_LEN), allocatable :: thr_tag(:)
      logical :: has_events = .false.
      ! accumulate: window (reset per interval) | running (since t_start,
      ! written each interval) | total (one write at the end)
      character(8) :: accum_mode = 'window'
      ! event log (log: true): every committed event of accum(iv, it) as a
      ! row of events_<prefix><tag>.dat, gathered per flush; on a netcdf
      ! root also the group <id>_events_<prefix><tag> (CF point features)
      logical :: log_events = .false.
      integer :: log_ncid = -1
      integer, allocatable :: ev_grp(:, :), ev_nrec(:, :), ev_var(:, :, :)
      integer :: log_dropped = 0          ! IO rank: cumulative over ranks
      logical :: log_warned = .false.
      logical :: log_seam_pending = .false.   ! hot start: seam line at the first step
      real(SP), allocatable :: log_px(:), log_py(:)   ! point geometry coords
      type(type_var_meta)            :: meta(VARS_MAX)
      integer                        :: n_vars = 0
      integer                        :: n_stats = 0
      ! hidden variables accumulate (derived sources) but never write
      logical                        :: hidden(VARS_MAX) = .false.
      ! single-precision save (binary casts, netcdf/pnetcdf NF90_FLOAT
      ! vars; ascii text unchanged) -- halves high-cadence field storage
      logical                        :: single_prec = .false.
      integer                        :: n_derived = 0
      type(type_channel_derived)     :: derived(DERIVED_MAX)
      logical                        :: snapshot = .true.
      real(SP)                       :: t_start = 0.0_SP
      !> Upper time bound; huge() = unbounded (the default).  Lets a
      !! high-cadence channel cover part of a long record without the
      !! tail running to the end of the run.
      real(SP)                       :: t_end = huge(1.0_SP)
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
      type(type_accumulator), allocatable :: accum(:, :)

      ! MPI gather helper
      type(type_output_gatherer) :: gatherer

      ! NetCDF backend (field geometry, format='netcdf'; IO rank only)
      type(type_netcdf_field_writer) :: nc

      ! NetCDF point backend (station/transect, format='netcdf')
      type(type_netcdf_point_writer) :: ncp

      ! Parallel PnetCDF field backend (format='pnetcdf'; all ranks)
      type(type_pnetcdf_field_writer) :: pnc

      ! Previous flush time = the open window's start (time_bnds)
      real(SP) :: t_last_flush = 0.0_SP

      ! Chunked field layout: roll to a fresh time-aligned file when a
      ! frame crosses the window's right edge (0 = no chunking).  The
      ! defined variable set is saved for re-creation at each roll-over.
      real(SP) :: chunk_window = 0.0_SP
      real(SP) :: t_chunk0 = 0.0_SP, t_chunk1 = 0.0_SP
      real(SP) :: dx0 = 0.0_SP, dy0 = 0.0_SP
      character(VARNAME_LEN + STATNAME_LEN + THRTAG_LEN + 1), allocatable :: nc_names(:)
      type(type_scalar_coord), allocatable :: nc_scalars(:)
      integer :: nc_nsc = 0
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
                           statistics, n_stats, snapshot, t_start, t_end, interval, &
                           result_folder, format, &
                           coords_x, coords_y, n_coords, grid, comm, &
                           file_prefixes, icount_start, var_meta, diag_ncid, &
                           chunk_window, hidden, derived, n_derived, single_prec, &
                           thresholds, thr_dir, gap, min_duration, wet_floor, accum_mode, &
                           log_events, restart, log_ncid)
      class(type_output_channel), intent(inout) :: this
      character(*), intent(in) :: id, geom_type
      character(*), intent(in) :: variables(*)
      integer, intent(in) :: n_vars
      character(*), intent(in) :: statistics(*)
      integer, intent(in) :: n_stats
      logical, intent(in) :: snapshot
      real(SP), intent(in) :: t_start, interval
      real(SP), intent(in), optional :: t_end   ! absent = unbounded
      character(*), intent(in) :: result_folder  ! must include trailing separator
      character(*), intent(in) :: format  ! field: ascii/binary/netcdf/pnetcdf; points: ascii/netcdf
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
      ! Hidden mask (accumulate only) + product-derived output specs
      logical, intent(in), optional :: hidden(*)
      type(type_channel_derived), intent(in), optional :: derived(*)
      integer, intent(in), optional :: n_derived
      logical, intent(in), optional :: single_prec
      ! event thresholds (values, direction THR_ABOVE/THR_BELOW), the
      ! filters, the wet-sample depth floor and the accumulate mode
      real(SP), intent(in), optional :: thresholds(:)
      integer, intent(in), optional :: thr_dir
      real(SP), intent(in), optional :: gap, min_duration, wet_floor
      character(*), intent(in), optional :: accum_mode
      ! event log: rows per committed event; restart appends behind a seam
      ! line; log_ncid = a netcdf root for the event groups (IO rank)
      logical, intent(in), optional :: log_events, restart
      integer, intent(in), optional :: log_ncid

      type(type_path) :: chan_dir
      logical :: dir_ok
      integer :: iv, is, it, id_, gunit
      integer, allocatable :: pids(:)

      this%id = id
      this%geom_type = geom_type
      this%format = format
      this%snapshot = snapshot
      this%t_start = t_start
      this%interval = interval
      this%n_vars = n_vars
      this%n_stats = n_stats
      this%hidden(1:n_vars) = .false.
      if (present(hidden)) this%hidden(1:n_vars) = hidden(1:n_vars)
      this%n_derived = 0
      if (present(n_derived)) then
         if (n_derived > DERIVED_MAX) &
            error stop "output_channel: derived list exceeds DERIVED_MAX"
         this%n_derived = n_derived
         this%derived(1:n_derived) = derived(1:n_derived)
      end if
      if (present(single_prec)) this%single_prec = single_prec
      this%nc%single = this%single_prec
      this%pnc%single = this%single_prec
      this%local_nx = grid%local_nx
      this%local_ny = grid%local_ny
      ! every channel owns a subfolder (design_output_io group layout):
      ! result_folder/<id>/ holds the frames, the per-channel t.out index,
      ! and grid.txt for field channels
      this%result_folder = trim(result_folder)//trim(id)//'/'
      if (comm%is_io_node()) then
         chan_dir = type_path(this%result_folder)
         if (.not. chan_dir%is_dir()) dir_ok = chan_dir%mkdir()
      end if
      call comm%barrier()
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

      ! events: one accumulator column per threshold value
      this%has_events = .false.
      do is = 1, n_stats
         if (any(EVENT_STATS == statistics(is))) this%has_events = .true.
      end do
      if (present(thresholds)) then
         this%n_thr = size(thresholds)
         this%thr = thresholds
         if (present(thr_dir)) this%thr_dir = thr_dir
         allocate (this%thr_tag(this%n_thr))
         do it = 1, this%n_thr
            this%thr_tag(it) = ''
            if (this%n_thr > 1) this%thr_tag(it) = '_'//threshold_tag(thresholds(it))
         end do
      end if
      if (this%has_events .and. this%n_thr == 0) &
         error stop 'output_channel: event statistics need a threshold:'
      if (present(gap)) this%gap = gap
      if (present(min_duration)) this%min_duration = min_duration
      if (present(wet_floor)) this%wet_floor = wet_floor
      if (present(accum_mode)) this%accum_mode = accum_mode
      if (present(log_events)) this%log_events = log_events
      if (this%log_events .and. .not. this%has_events) &
         error stop 'output_channel: log: needs an event statistic'
      if (present(log_ncid)) this%log_ncid = log_ncid

      ! Timing control
      this%trigger%t_start = t_start
      if (present(t_end)) this%t_end = t_end
      this%trigger%interval = interval
      ! total: the end-of-run forced flush is the only write
      if (this%accum_mode == 'total') this%trigger%interval = huge(1.0_SP)
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
            if (comm%is_io_node()) then
               call truncate_point_files(this)
               call write_metadata_yaml(this)
            end if
         end if

         ! Accumulators: (n_local, 1)
         call init_accumulators(this, this%n_local, 1)
         if (this%log_events) then
            this%log_px = coords_x(1:n_coords)
            this%log_py = coords_y(1:n_coords)
         end if

      case ('field')
         this%n_local = grid%local_nx*grid%local_ny
         this%n_global = grid%M*grid%N
         this%i0 = grid%ibegin - 1
         this%j0 = grid%jbegin - 1
         call this%gatherer%init_field(grid, comm)

         ! per-channel grid descriptor (design_output_io group layout)
         if (comm%is_io_node()) then
            open (newunit=gunit, file=this%result_folder//'grid.txt', &
                  status='replace', action='write')
            write (gunit, '(2I8, 2E16.8)') grid%M, grid%N, grid%dx0, grid%dy0
            close (gunit)
         end if

         ! Accumulators: (local_nx, local_ny)
         call init_accumulators(this, grid%local_nx, grid%local_ny)
         this%dx0 = grid%dx0
         this%dy0 = grid%dy0

         ! NetCDF backends: every snapshot + statistic variable defined
         ! up front (names fixed at init).  Layout: a shared-root group
         ! (diag_ncid), time-chunked files (chunk_window), or one
         ! data.nc per channel.  'netcdf' is serial on the IO rank;
         ! 'pnetcdf' creates collectively on every rank.
         if (trim(format) == 'netcdf' .or. trim(format) == 'pnetcdf') then
            this%dx0 = grid%dx0
            this%dy0 = grid%dy0
            if (present(chunk_window)) this%chunk_window = chunk_window
            this%t_chunk0 = t_start
            this%t_chunk1 = t_start + this%chunk_window
         end if
         if (trim(format) == 'netcdf') then
            if (comm%is_io_node()) call init_netcdf_backend(this, grid, diag_ncid)
         else if (trim(format) == 'ascii' .or. trim(format) == 'binary') then
            ! no attributes in the frames: the netcdf header rides beside them
            call build_nc_varlist(this)
            if (comm%is_io_node()) call write_metadata_yaml(this, grid%M, grid%N)
         else if (trim(format) == 'pnetcdf') then
            call build_nc_varlist(this)
            if (this%chunk_window > 0.0_SP) then
               call create_chunk_file(this, comm)
            else
               call this%pnc%create(this%result_folder//trim(this%id)//'.nc', &
                                    grid%M, grid%N, grid%dx0, grid%dy0, &
                                    this%nc_names, this%nc_meta, this%nc_n, comm, &
                                    scalars=this%nc_scalars(1:this%nc_nsc))
            end if
         end if

      case default
         error stop 'type_output_channel: unknown geometry type: '//trim(geom_type)
      end select

      ! product-derived sources: ensure the required statistic storage
      ! exists on the source accumulator (its variable may be hidden)
      do id_ = 1, this%n_derived
         call this%accum(this%derived(id_)%iv, 1)%allocate_stat(trim(this%derived(id_)%stat))
      end do

      if (this%log_events) call init_event_log(this, comm, restart)

   end subroutine channel_init

   ! Event log files (IO rank): a cold start truncates and writes the
   ! header, a hot start appends behind a seam line so the pre-restart
   ! rows survive (open events at the restart are lost either way: the
   ! state arrays are not checkpointed).  Every rank arms the buffers.
   subroutine init_event_log(this, comm, restart)
      class(type_output_channel), intent(inout) :: this
      type(type_comm), intent(inout) :: comm
      logical, intent(in), optional :: restart

      integer :: iv, it, unit
      logical :: appending, exists

      do iv = 1, this%n_vars
         do it = 1, max(1, this%n_thr)
            call this%accum(iv, it)%enable_log(LOG_CAP)
         end do
      end do
      if (.not. comm%is_io_node()) return

      appending = .false.
      if (present(restart)) appending = restart
      this%log_seam_pending = appending
      allocate (this%ev_grp(this%n_vars, max(1, this%n_thr)), &
                this%ev_nrec(this%n_vars, max(1, this%n_thr)), &
                this%ev_var(LOG_NVARS, this%n_vars, max(1, this%n_thr)))
      this%ev_grp = -1
      this%ev_nrec = 0
      do iv = 1, this%n_vars
         if (this%hidden(iv)) cycle
         do it = 1, max(1, this%n_thr)
            exists = .false.
            if (appending) inquire (file=event_file_name(this, iv, it), exist=exists)
            if (exists) then
               open (newunit=unit, file=event_file_name(this, iv, it), status='old', &
                     position='append', action='write')
            else
               open (newunit=unit, file=event_file_name(this, iv, it), status='replace', &
                     action='write')
               write (unit, '(a)') '#'//repeat(' ', 12)//'t_on'//repeat(' ', 12)//'t_off'// &
                  repeat(' ', 9)//'duration'//repeat(' ', 13)//'peak'//repeat(' ', 16)//'x'// &
                  repeat(' ', 16)//'y'//repeat(' ', 9)//'i'//repeat(' ', 9)//'j'
            end if
            close (unit)
            if (this%log_ncid >= 0) call create_event_group(this, iv, it)
         end do
      end do
   end subroutine init_event_log

   subroutine write_event_seam(this, t)
      class(type_output_channel), intent(in) :: this
      real(SP), intent(in) :: t
      integer :: iv, it, unit
      do iv = 1, this%n_vars
         if (this%hidden(iv)) cycle
         do it = 1, max(1, this%n_thr)
            open (newunit=unit, file=event_file_name(this, iv, it), status='old', &
                  position='append', action='write')
            write (unit, '(a,es17.8)') '# restart t_start=', t
            close (unit)
         end do
      end do
   end subroutine write_event_seam

   function event_file_name(this, iv, it) result(fname)
      class(type_output_channel), intent(in) :: this
      integer, intent(in) :: iv, it
      character(:), allocatable :: fname
      fname = this%result_folder//'events_'//trim(this%prefixes(iv))
      if (this%n_thr > 0) fname = fname//trim(this%thr_tag(it))
      fname = fname//'.dat'
   end function event_file_name

   ! CF discrete-sampling-geometry point features: one element per event
   ! on an unlimited dimension, time/x/y the coordinates of the peak
   subroutine create_event_group(this, iv, it)
      class(type_output_channel), intent(inout) :: this
      integer, intent(in) :: iv, it

      character(len=*), parameter :: VNAME(LOG_NVARS) = &
                                     [character(len=8) :: 'time', 't_off', 'duration', 'peak', &
                                                           'x', 'y', 'i', 'j']
      character(:), allocatable :: gname, base
      integer :: e_dim, k, grp

      gname = trim(this%id)//'_events_'//trim(this%prefixes(iv))
      if (this%n_thr > 0) gname = gname//trim(this%thr_tag(it))
      call nc_check(nf90_def_grp(this%log_ncid, gname, grp), 'def group '//gname)
      call nc_check(nf90_put_att(grp, NF90_GLOBAL, 'featureType', 'point'), 'att featureType')
      call nc_check(nf90_def_dim(grp, 'event', NF90_UNLIMITED, e_dim), 'def event '//gname)
      do k = 1, LOG_NVARS
         if (k >= 7) then
            call nc_check(nf90_def_var(grp, VNAME(k), NF90_INT, [e_dim], &
                                       this%ev_var(k, iv, it)), 'def var '//VNAME(k))
         else
            call nc_check(nf90_def_var(grp, VNAME(k), NF90_DOUBLE, [e_dim], &
                                       this%ev_var(k, iv, it)), 'def var '//VNAME(k))
         end if
      end do
      base = trim(this%meta(iv)%long_name)
      if (len(base) == 0) base = trim(this%variables(iv))
      call nc_check(nf90_put_att(grp, this%ev_var(1, iv, it), 'units', 'seconds since start'), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(1, iv, it), 'long_name', 'event onset'), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(2, iv, it), 'units', 's'), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(2, iv, it), 'long_name', &
                                 'first sample failing the condition'), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(3, iv, it), 'units', 's'), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(3, iv, it), 'long_name', &
                                 'time meeting the condition'), 'att')
      if (len_trim(this%meta(iv)%units) > 0) &
         call nc_check(nf90_put_att(grp, this%ev_var(4, iv, it), 'units', &
                                    trim(this%meta(iv)%units)), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(4, iv, it), 'long_name', &
                                 base//' furthest past the threshold '//dir_text(this%thr_dir)// &
                                 ' '//threshold_text(this%thr(it))), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(4, iv, it), 'coordinates', 'time x y'), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(5, iv, it), 'units', 'm'), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(6, iv, it), 'units', 'm'), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(7, iv, it), 'long_name', &
                                 'global cell i (point index on a point geometry)'), 'att')
      call nc_check(nf90_put_att(grp, this%ev_var(8, iv, it), 'long_name', &
                                 'global cell j (1 on a point geometry)'), 'att')
      this%ev_grp(iv, it) = grp
   end subroutine create_event_group

   ! Drain every rank's event rows to the IO rank, order them by onset
   ! (then cell, so the file is rank-layout independent) and append them;
   ! rows over the per-rank cap are counted, warned once and marked in
   ! the file at the flush that lost them (rows resume after every flush)
   subroutine flush_event_log(this, comm, final)
      class(type_output_channel), intent(inout) :: this
      type(type_comm), intent(inout) :: comm
      logical, intent(in) :: final

      integer :: iv, it, n_loc, n_tot, k, r, ierr, io, unit, dropped, dropped_tot
      integer, allocatable :: counts(:), displs(:), gi(:), gj(:), perm(:)
      real(SP), allocatable :: t_on(:), t_off(:), dur(:), peak(:), x(:), y(:)
      real(SP) :: rowbuf(4)

      io = comm%get_io_rank()
      allocate (counts(comm%size), displs(comm%size))
      do iv = 1, this%n_vars
         do it = 1, max(1, this%n_thr)
            associate (acc => this%accum(iv, it))
               n_loc = acc%n_events
               if (this%hidden(iv)) n_loc = 0
               call MPI_Gather(n_loc, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, io, comm%id, ierr)
               n_tot = 0
               if (comm%is_io_node()) then
                  displs(1) = 0
                  do r = 2, comm%size
                     displs(r) = displs(r - 1) + counts(r - 1)
                  end do
                  n_tot = sum(counts)
               end if
               allocate (t_on(max(1, n_tot)), t_off(max(1, n_tot)), dur(max(1, n_tot)), &
                         peak(max(1, n_tot)), gi(max(1, n_tot)), gj(max(1, n_tot)))
               call MPI_Gatherv(acc%ev_t_on, n_loc, MPI_SP, t_on, counts, displs, MPI_SP, &
                                io, comm%id, ierr)
               call MPI_Gatherv(acc%ev_t_off, n_loc, MPI_SP, t_off, counts, displs, MPI_SP, &
                                io, comm%id, ierr)
               call MPI_Gatherv(acc%ev_dur, n_loc, MPI_SP, dur, counts, displs, MPI_SP, &
                                io, comm%id, ierr)
               call MPI_Gatherv(acc%ev_peak, n_loc, MPI_SP, peak, counts, displs, MPI_SP, &
                                io, comm%id, ierr)
               ! cells to global indices before the gather
               call MPI_Gatherv(global_i(this, acc%ev_i(1:n_loc), n_loc), n_loc, MPI_INTEGER, &
                                gi, counts, displs, MPI_INTEGER, io, comm%id, ierr)
               call MPI_Gatherv(global_j(this, acc%ev_j(1:n_loc), n_loc), n_loc, MPI_INTEGER, &
                                gj, counts, displs, MPI_INTEGER, io, comm%id, ierr)
               call acc%clear_events()

               if (comm%is_io_node() .and. n_tot > 0) then
                  allocate (x(n_tot), y(n_tot))
                  if (trim(this%geom_type) == 'field') then
                     x = real(gi(1:n_tot) - 1, SP)*this%dx0
                     y = real(gj(1:n_tot) - 1, SP)*this%dy0
                  else
                     x = this%log_px(gi(1:n_tot))
                     y = this%log_py(gi(1:n_tot))
                  end if
                  call sort_events(n_tot, t_on, gi, gj, perm)
                  open (newunit=unit, file=event_file_name(this, iv, it), status='old', &
                        position='append', action='write')
                  do k = 1, n_tot
                     rowbuf = [t_on(perm(k)), t_off(perm(k)), dur(perm(k)), peak(perm(k))]
                     write (unit, '(6es17.8,2i10)') rowbuf, x(perm(k)), y(perm(k)), &
                        gi(perm(k)), gj(perm(k))
                  end do
                  close (unit)
                  if (this%ev_grp(iv, it) >= 0) &
                     call put_event_rows(this, iv, it, n_tot, t_on(perm), t_off(perm), &
                                         dur(perm), peak(perm), x(perm), y(perm), gi(perm), gj(perm))
                  deallocate (x, y, perm)
               end if
               deallocate (t_on, t_off, dur, peak, gi, gj)
            end associate
         end do
      end do

      ! rows over the cap: one warning, and the total in a trailer line
      dropped = 0
      do iv = 1, this%n_vars
         do it = 1, max(1, this%n_thr)
            dropped = dropped + this%accum(iv, it)%n_dropped
         end do
      end do
      call MPI_Reduce(dropped, dropped_tot, 1, MPI_INTEGER, MPI_SUM, io, comm%id, ierr)
      if (comm%is_io_node()) then
         ! the marker sits where the loss happened (a streaming reader
         ! learns early that rows /= count from here on); the buffers
         ! refill after every flush, so rows resume
         if (dropped_tot > this%log_dropped) then
            do iv = 1, this%n_vars
               if (this%hidden(iv)) cycle
               do it = 1, max(1, this%n_thr)
                  open (newunit=unit, file=event_file_name(this, iv, it), status='old', &
                        position='append', action='write')
                  write (unit, '(a,i0,a,i0,a)') '# dropped ', dropped_tot - this%log_dropped, &
                     ' rows over the per-rank cap at this flush (', dropped_tot, &
                     ' so far, channel total)'
                  close (unit)
               end do
            end do
            if (.not. this%log_warned) then
               write (*, '(a,i0,a)') 'output_channel: '//trim(this%id)//': event log dropped ', &
                  dropped_tot, ' rows over the per-rank cap (shorten the interval)'
               this%log_warned = .true.
            end if
            this%log_dropped = dropped_tot
         end if
      end if
      if (final) call write_open_events(this, comm)
   end subroutine flush_event_log

   ! End of a leg: the events still open (the long ones a restart would
   ! lose) as comment rows -- onset, peak so far, cell -- so a reader can
   ! report the truncation or stitch across the seam
   subroutine write_open_events(this, comm)
      class(type_output_channel), intent(inout) :: this
      type(type_comm), intent(inout) :: comm

      integer :: iv, it, n_loc, n_tot, k, r, ierr, io, unit
      integer, allocatable :: counts(:), displs(:), gi(:), gj(:), perm(:), li(:), lj(:)
      real(SP), allocatable :: t_on(:), peak(:), lt(:), lp(:)

      io = comm%get_io_rank()
      allocate (counts(comm%size), displs(comm%size))
      do iv = 1, this%n_vars
         do it = 1, max(1, this%n_thr)
            call this%accum(iv, it)%open_events(n_loc, lt, lp, li, lj)
            if (this%hidden(iv)) n_loc = 0
            call MPI_Gather(n_loc, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, io, comm%id, ierr)
            n_tot = 0
            if (comm%is_io_node()) then
               displs(1) = 0
               do r = 2, comm%size
                  displs(r) = displs(r - 1) + counts(r - 1)
               end do
               n_tot = sum(counts)
            end if
            allocate (t_on(max(1, n_tot)), peak(max(1, n_tot)), gi(max(1, n_tot)), gj(max(1, n_tot)))
            call MPI_Gatherv(lt, n_loc, MPI_SP, t_on, counts, displs, MPI_SP, io, comm%id, ierr)
            call MPI_Gatherv(lp, n_loc, MPI_SP, peak, counts, displs, MPI_SP, io, comm%id, ierr)
            call MPI_Gatherv(global_i(this, li(1:n_loc), n_loc), n_loc, MPI_INTEGER, &
                             gi, counts, displs, MPI_INTEGER, io, comm%id, ierr)
            call MPI_Gatherv(global_j(this, lj(1:n_loc), n_loc), n_loc, MPI_INTEGER, &
                             gj, counts, displs, MPI_INTEGER, io, comm%id, ierr)
            if (comm%is_io_node() .and. n_tot > 0) then
               call sort_events(n_tot, t_on, gi, gj, perm)
               open (newunit=unit, file=event_file_name(this, iv, it), status='old', &
                     position='append', action='write')
               do k = 1, n_tot
                  write (unit, '(a,es17.8,a,es17.8,a,i0,a,i0)') '# open t_on=', t_on(perm(k)), &
                     ' peak=', peak(perm(k)), ' i=', gi(perm(k)), ' j=', gj(perm(k))
               end do
               close (unit)
               deallocate (perm)
            end if
            deallocate (t_on, peak, gi, gj, lt, lp, li, lj)
         end do
      end do
   end subroutine write_open_events

   function global_i(this, i_loc, n) result(ig)
      class(type_output_channel), intent(in) :: this
      integer, intent(in) :: n, i_loc(n)
      integer :: ig(max(1, n))
      ig = 0
      if (n == 0) return
      if (trim(this%geom_type) == 'field') then
         ig(1:n) = this%i0 + i_loc
      else
         ig(1:n) = this%interp%point_id(i_loc)
      end if
   end function global_i

   function global_j(this, j_loc, n) result(jg)
      class(type_output_channel), intent(in) :: this
      integer, intent(in) :: n, j_loc(n)
      integer :: jg(max(1, n))
      jg = 0
      if (n == 0) return
      if (trim(this%geom_type) == 'field') then
         jg(1:n) = this%j0 + j_loc
      else
         jg(1:n) = 1
      end if
   end function global_j

   ! Permutation ordering the rows by (onset, j, i); bottom-up merge
   ! sort, stable, n log n for the 1e5-row flushes
   subroutine sort_events(n, t_on, gi, gj, perm)
      integer, intent(in) :: n
      real(SP), intent(in) :: t_on(:)
      integer, intent(in) :: gi(:), gj(:)
      integer, allocatable, intent(out) :: perm(:)

      integer, allocatable :: tmp(:)
      integer :: width, lo, mid, hi, a, b, k

      allocate (perm(n), tmp(n))
      perm = [(k, k=1, n)]
      width = 1
      do while (width < n)
         lo = 1
         do while (lo <= n)
            mid = min(lo + width - 1, n)
            hi = min(lo + 2*width - 1, n)
            a = lo; b = mid + 1; k = lo
            do while (a <= mid .and. b <= hi)
               if (before(perm(b), perm(a))) then
                  tmp(k) = perm(b); b = b + 1
               else
                  tmp(k) = perm(a); a = a + 1
               end if
               k = k + 1
            end do
            do while (a <= mid)
               tmp(k) = perm(a); a = a + 1; k = k + 1
            end do
            do while (b <= hi)
               tmp(k) = perm(b); b = b + 1; k = k + 1
            end do
            lo = lo + 2*width
         end do
         perm = tmp
         width = 2*width
      end do
   contains
      logical function before(p, q)
         integer, intent(in) :: p, q
         if (t_on(p) /= t_on(q)) then
            before = t_on(p) < t_on(q)
         else if (gj(p) /= gj(q)) then
            before = gj(p) < gj(q)
         else
            before = gi(p) < gi(q)
         end if
      end function before
   end subroutine sort_events

   subroutine put_event_rows(this, iv, it, n, t_on, t_off, dur, peak, x, y, gi, gj)
      class(type_output_channel), intent(inout) :: this
      integer, intent(in) :: iv, it, n
      real(SP), intent(in) :: t_on(:), t_off(:), dur(:), peak(:), x(:), y(:)
      integer, intent(in) :: gi(:), gj(:)
      integer :: grp, s0
      grp = this%ev_grp(iv, it)
      s0 = this%ev_nrec(iv, it) + 1
      call nc_check(nf90_put_var(grp, this%ev_var(1, iv, it), t_on(1:n), start=[s0]), 'put events')
      call nc_check(nf90_put_var(grp, this%ev_var(2, iv, it), t_off(1:n), start=[s0]), 'put events')
      call nc_check(nf90_put_var(grp, this%ev_var(3, iv, it), dur(1:n), start=[s0]), 'put events')
      call nc_check(nf90_put_var(grp, this%ev_var(4, iv, it), peak(1:n), start=[s0]), 'put events')
      call nc_check(nf90_put_var(grp, this%ev_var(5, iv, it), x(1:n), start=[s0]), 'put events')
      call nc_check(nf90_put_var(grp, this%ev_var(6, iv, it), y(1:n), start=[s0]), 'put events')
      call nc_check(nf90_put_var(grp, this%ev_var(7, iv, it), gi(1:n), start=[s0]), 'put events')
      call nc_check(nf90_put_var(grp, this%ev_var(8, iv, it), gj(1:n), start=[s0]), 'put events')
      this%ev_nrec(iv, it) = this%ev_nrec(iv, it) + n
      call nc_check(nf90_sync(this%log_ncid), 'sync events')
   end subroutine put_event_rows

   ! accum(iv, it): column 1 carries every statistic, further columns
   ! (one per extra threshold) the event class only
   subroutine init_accumulators(this, d1, d2)
      class(type_output_channel), intent(inout) :: this
      integer, intent(in) :: d1, d2

      integer :: iv, is, it

      allocate (this%accum(this%n_vars, max(1, this%n_thr)))
      do iv = 1, this%n_vars
         do it = 1, max(1, this%n_thr)
            call this%accum(iv, it)%init(d1, d2, trim(this%variables(iv)))
            do is = 1, this%n_stats
               if (it > 1 .and. .not. any(EVENT_STATS == this%statistics(is))) cycle
               call this%accum(iv, it)%allocate_stat(trim(this%statistics(is)))
            end do
            if (this%n_thr > 0) call this%accum(iv, it)%set_threshold( &
               this%thr(it), this%thr_dir, gap=this%gap, min_duration=this%min_duration)
         end do
      end do
   end subroutine init_accumulators

   ! One threshold value as the shortest plain decimal (0.05, 2, -0.5,
   ! 1.25); outside [1e-3, 1e6) the exponent form (2.5e-4)
   function threshold_text(v) result(txt)
      real(SP), intent(in) :: v
      character(:), allocatable :: txt

      character(24) :: buf
      character(:), allocatable :: mant, expo
      integer :: i, e

      if (v == 0.0_SP) then
         txt = '0'
         return
      end if
      if (abs(v) >= 1.0e-3_SP .and. abs(v) < 1.0e6_SP) then
         write (buf, '(F0.6)') v
         mant = trim(adjustl(buf))
         expo = ''
      else
         write (buf, '(ES12.5E2)') v
         mant = trim(adjustl(buf))
         e = index(mant, 'E')
         ! e-004 -> e-4
         i = e + 2
         do while (i < len(mant) .and. mant(i:i) == '0')
            i = i + 1
         end do
         expo = 'e'//merge('-', '+', mant(e + 1:e + 1) == '-')//mant(i:)
         if (expo(2:2) == '+') expo = 'e'//expo(3:)
         mant = mant(1:e - 1)
      end if
      if (mant(1:1) == '.') mant = '0'//mant
      if (mant(1:2) == '-.') mant = '-0'//mant(2:)
      ! strip trailing zeros of the mantissa, then a bare point
      i = len(mant)
      if (index(mant, '.') > 0) then
         do while (i > 1 .and. mant(i:i) == '0')
            i = i - 1
         end do
         if (mant(i:i) == '.') i = i - 1
      end if
      txt = mant(1:i)//expo
   end function threshold_text

   ! Name suffix of one threshold value: the plain text with 'p' for the
   ! point and 'm' for a minus (0.1 -> 0p1, 2 -> 2, -0.5 -> m0p5, 2.5e-4 -> 2p5em4)
   function threshold_tag(v) result(tag)
      real(SP), intent(in) :: v
      character(:), allocatable :: tag

      integer :: i

      tag = threshold_text(v)
      do i = 1, len(tag)
         select case (tag(i:i))
         case ('.'); tag(i:i) = 'p'
         case ('-'); tag(i:i) = 'm'
         end select
      end do
   end function threshold_tag

   ! Called every timestep. Accumulates from registry; flushes when triggered.
   ! force=.true. (after-loop final flush) fires unconditionally once the
   ! channel has started.
   subroutine channel_step(this, t, dt, registry, comm, force)
      class(type_output_channel), intent(inout) :: this
      real(SP), intent(in)    :: t, dt
      type(type_field_registry), intent(in)    :: registry
      type(type_comm), intent(inout) :: comm
      logical, intent(in), optional :: force

      integer  :: iv, it, tunit
      real(SP), pointer :: fld(:, :), mask(:, :), h(:, :)
      real(SP), allocatable :: interp_vals(:), interp_2d(:, :), mask_i(:), h_i(:)
      logical, allocatable :: wet(:, :), wet_ev(:, :)
      logical :: do_flush

      this%fired = .false.
      if (t < this%t_start) return
      if (t > this%t_end) return
      ! hot start: the event logs mark the seam with the restart time
      if (this%log_seam_pending .and. comm%is_io_node()) call write_event_seam(this, t)
      this%log_seam_pending = .false.

      ! dt-accumulator mode: legacy PLOT_COUNT frame cadence
      do_flush = this%trigger%should_trigger(t, dt)
      if (present(force)) do_flush = do_flush .or. force
      if (do_flush) this%icount = this%icount + 1
      this%fired = do_flush

      ! per-channel frame index (nee the global time_dt.out): one line
      ! per flush -- frame counter, time, dt
      if (do_flush .and. comm%is_io_node()) then
         open (newunit=tunit, file=this%result_folder//'t.out', &
               status='unknown', position='append', action='write')
         write (tunit, '(I6, 2E16.6)') this%icount, t, dt
         close (tunit)
      end if

      ! Chunked field stream: a frame at or past the window's right
      ! edge rolls to the next time-aligned file first (chunks cover
      ! [t0, t1); a frame at exactly t1 opens the next chunk).  The
      ! serial writer is open on the IO rank alone; the pnetcdf writer
      ! on every rank, so its roll-over stays collective.
      if (do_flush .and. this%chunk_window > 0.0_SP .and. &
          (this%nc%is_open .or. this%pnc%is_open)) then
         if (t >= this%t_chunk1) then
            call this%nc%close()
            call this%pnc%close()
            do while (t >= this%t_chunk1)
               this%t_chunk0 = this%t_chunk1
               this%t_chunk1 = this%t_chunk1 + this%chunk_window
            end do
            call create_chunk_file(this, comm)
         end if
      end if

      ! One record per flush: stamp the time value before any variable
      ! lands (snapshot and statistics share the frame)
      if (do_flush .and. this%nc%is_open) call this%nc%begin_frame(t)
      if (do_flush .and. this%pnc%is_open) call this%pnc%begin_frame(t)
      ! Point groups: a windowed channel's first flush writes nothing
      ! (degenerate window) — advance the record only once primed
      if (do_flush .and. this%ncp%is_open .and. &
          (this%snapshot .or. this%stats_primed)) &
         call this%ncp%begin_frame(t, this%t_last_flush)

      ! --- Snapshot: write current field directly from registry ---
      if (do_flush .and. this%snapshot) then
         do iv = 1, this%n_vars
            if (this%hidden(iv)) cycle
            fld => registry%get(trim(this%variables(iv)))
            call channel_write_snapshot(this, iv, fld, t, comm)
         end do
      end if

      ! --- Accumulate for statistics (derived sources included) ---
      ! Extremes and events sample wet cells only: the instantaneous mask
      ! (the model raised its need); events additionally sit above the
      ! swash-edge depth floor.  A point is wet when its whole stencil is
      ! (interpolated mask exactly 1).  Moments take every sample.
      if (this%n_stats > 0 .or. this%n_derived > 0) then
         select case (trim(this%geom_type))
         case ('station', 'transect')
            allocate (interp_vals(this%n_local), interp_2d(this%n_local, 1))
            if (this%n_stats > 0) then
               allocate (mask_i(this%n_local), h_i(this%n_local), &
                         wet(this%n_local, 1), wet_ev(this%n_local, 1))
               if (registry%has('mask') .and. registry%has('h')) then
                  mask => registry%get('mask')
                  h => registry%get('h')
                  call this%interp%gather(mask, mask_i)
                  call this%interp%gather(h, h_i)
                  wet(:, 1) = mask_i == 1.0_SP
                  wet_ev(:, 1) = wet(:, 1) .and. h_i > this%wet_floor
               else
                  ! no model behind the registry (unit tests): every sample wet
                  wet = .true.
                  wet_ev = .true.
               end if
            end if
            do iv = 1, this%n_vars
               fld => registry%get(trim(this%variables(iv)))
               call this%interp%gather(fld, interp_vals)
               interp_2d(:, 1) = interp_vals
               do it = 1, max(1, this%n_thr)
                  if (this%n_stats > 0) then
                     call this%accum(iv, it)%accumulate(interp_2d, dt, t=t, wet=wet, &
                                                        wet_event=wet_ev)
                  else
                     call this%accum(iv, it)%accumulate(interp_2d, dt, t=t)
                  end if
               end do
            end do
            deallocate (interp_vals, interp_2d)

         case ('field')
            associate (ng => N_GHOST, nx => this%local_nx, ny => this%local_ny)
               if (this%n_stats > 0) then
                  if (registry%has('mask') .and. registry%has('h')) then
                     mask => registry%get('mask')
                     h => registry%get('h')
                     wet = mask(ng + 1:ng + nx, ng + 1:ng + ny) > 0.5_SP
                     wet_ev = wet .and. h(ng + 1:ng + nx, ng + 1:ng + ny) > this%wet_floor
                  else
                     allocate (wet(nx, ny), wet_ev(nx, ny))
                     wet = .true.
                     wet_ev = .true.
                  end if
               end if
               do iv = 1, this%n_vars
                  fld => registry%get(trim(this%variables(iv)))
                  ! Slice interior (ghost-inclusive field → interior only)
                  do it = 1, max(1, this%n_thr)
                     if (this%n_stats > 0) then
                        call this%accum(iv, it)%accumulate( &
                           fld(ng + 1:ng + nx, ng + 1:ng + ny), dt, t=t, wet=wet, &
                           wet_event=wet_ev)
                     else
                        call this%accum(iv, it)%accumulate( &
                           fld(ng + 1:ng + nx, ng + 1:ng + ny), dt, t=t)
                     end if
                  end do
               end do
            end associate
         end select
      end if

      ! --- Flush statistics at interval (first flush closes a
      !     degenerate single-step window: reset without writing).
      !     Derived products read the accumulators, so resets come last ---
      if (do_flush .and. (this%n_stats > 0 .or. this%n_derived > 0)) then
         if (this%stats_primed) then
            do iv = 1, this%n_vars
               if (this%hidden(iv)) cycle
               call channel_write_stats(this, iv, t, comm)
            end do
            do iv = 1, this%n_derived
               call channel_write_derived(this, iv, t, comm)
            end do
         end if
         ! running/total never reset: statistics since t_start
         if (this%accum_mode == 'window') then
            do iv = 1, this%n_vars
               do it = 1, max(1, this%n_thr)
                  call this%accum(iv, it)%reset()
               end do
            end do
         end if
      end if
      ! event log: every rank drains its committed rows to the IO rank
      if (do_flush .and. this%log_events) then
         if (present(force)) then
            call flush_event_log(this, comm, force)
         else
            call flush_event_log(this, comm, .false.)
         end if
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

      integer :: is, it, n_it
      character(:), allocatable :: name
      real(SP), allocatable :: stat_vals(:, :)

      do is = 1, this%n_stats
         ! event statistics write once per threshold, the rest once
         n_it = 1
         if (any(EVENT_STATS == this%statistics(is))) n_it = max(1, this%n_thr)
         do it = 1, n_it
            stat_vals = this%accum(iv, it)%get_stat(trim(this%statistics(is)))
            name = stat_name(this, iv, is, it)
            select case (trim(this%geom_type))
            case ('field')
               call channel_flush_field(this, stat_vals, name, comm)
            case ('station', 'transect')
               call channel_flush_points(this, stat_vals(:, 1), name, t, comm)
            end select
         end do
      end do
   end subroutine channel_write_stats

   ! <prefix>_threshold[_<threshold tag>]: the scalar coordinate of
   ! variable iv at threshold it
   function scalar_name(this, iv, it) result(name)
      class(type_output_channel), intent(in) :: this
      integer, intent(in) :: iv, it
      character(:), allocatable :: name
      name = trim(this%prefixes(iv))//'_threshold'//trim(this%thr_tag(it))
   end function scalar_name

   ! The scalar coordinates of a channel with events: one per variable
   ! and threshold, shared by every statistic on that pair
   subroutine build_scalar_coords(this, scalars, n)
      class(type_output_channel), intent(in) :: this
      type(type_scalar_coord), allocatable, intent(out) :: scalars(:)
      integer, intent(out) :: n

      integer :: iv, it
      character(:), allocatable :: base

      n = 0
      if (.not. this%has_events) then
         allocate (scalars(0))
         return
      end if
      allocate (scalars(this%n_vars*this%n_thr))
      do iv = 1, this%n_vars
         if (this%hidden(iv)) cycle
         base = trim(this%meta(iv)%long_name)
         if (len(base) == 0) base = trim(this%variables(iv))
         do it = 1, this%n_thr
            n = n + 1
            scalars(n)%name = scalar_name(this, iv, it)
            scalars(n)%value = this%thr(it)
            scalars(n)%meta%units = this%meta(iv)%units
            scalars(n)%meta%standard_name = this%meta(iv)%standard_name
            scalars(n)%meta%long_name = base//' threshold'
            scalars(n)%meta%comment = 'event condition: '//dir_text(this%thr_dir)// &
                                      ' this value on wet samples'
         end do
      end do
   end subroutine build_scalar_coords

   ! <prefix>_<stat>[_<threshold tag>]: the tag only on the event class
   ! with more than one threshold
   function stat_name(this, iv, is, it) result(name)
      class(type_output_channel), intent(in) :: this
      integer, intent(in) :: iv, is, it
      character(:), allocatable :: name
      name = trim(this%prefixes(iv))//'_'//trim(this%statistics(is))
      if (any(EVENT_STATS == this%statistics(is)) .and. this%n_thr > 0) &
         name = name//trim(this%thr_tag(it))
   end function stat_name

   ! Attributes of one statistic variable: moments and extremes inherit
   ! the base variable's (flag pair dropped); the time-valued and event
   ! statistics are new quantities in seconds or counts, described from
   ! the base long_name and the threshold
   function stat_meta(this, iv, is, it) result(m)
      class(type_output_channel), intent(in) :: this
      integer, intent(in) :: iv, is, it
      type(type_var_meta) :: m

      character(:), allocatable :: cond, base

      m = this%meta(iv)
      m%n_flags = 0
      base = trim(m%long_name)
      if (len(base) == 0) base = trim(this%variables(iv))
      select case (trim(this%statistics(is)))
      case ('max_time')
         m = type_var_meta()
         m%has_fill = this%meta(iv)%has_fill
         m%fill_value = this%meta(iv)%fill_value
         m%units = 's'
         m%long_name = 'time of the maximum of '//base
         m%comment = 'time the running maximum was last raised; fill where no sample'
      case ('first_time', 'last_time', 'duration', 'duration_max', 'count')
         cond = ' '//dir_text(this%thr_dir)//' '// &
                threshold_text(this%thr(it))//' '//trim(this%meta(iv)%units)
         m = type_var_meta()
         m%has_fill = this%meta(iv)%has_fill
         m%fill_value = this%meta(iv)%fill_value
         m%units = 's'
         select case (trim(this%statistics(is)))
         case ('first_time')
            m%long_name = 'onset of the first event of '//base//cond
         case ('last_time')
            m%long_name = 'onset of the last event of '//base//cond
         case ('duration')
            m%long_name = 'duration of '//base//cond
         case ('duration_max')
            m%long_name = 'longest event of '//base//cond
         case ('count')
            m%units = '1'
            m%long_name = 'number of events of '//base//cond
         end select
         m%comment = 'events on wet samples'//cond//', committed at close;'// &
                     ' time values fill where none triggered'
         m%coordinates = scalar_name(this, iv, it)
      end select
   end function stat_meta

   ! Write one product-derived output: scale * get_stat(stat) of the
   ! source accumulator, under the derived name (e.g. hsig_NNNNN).
   subroutine channel_write_derived(this, id, t, comm)
      class(type_output_channel), intent(inout) :: this
      integer, intent(in)    :: id
      real(SP), intent(in)    :: t
      type(type_comm), intent(inout) :: comm

      real(SP), allocatable :: stat_vals(:, :)

      associate (d => this%derived(id))
         stat_vals = d%scale*this%accum(d%iv, 1)%get_stat(trim(d%stat))
         select case (trim(this%geom_type))
         case ('field')
            call channel_flush_field(this, stat_vals, trim(d%name), comm)
         case ('station', 'transect')
            call channel_flush_points(this, stat_vals(:, 1), trim(d%name), t, comm)
         end select
      end associate
   end subroutine channel_write_derived

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
                                     this%i0, this%j0, comm, this%single_prec)
         return
      end if

      if (trim(this%format) == 'pnetcdf') then
         call this%pnc%put(name, vals, this%i0, this%j0)
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

   ! Build the stream's variable set: snapshot variables under their
   ! file prefixes plus every <prefix>_<stat> combination.  Saved on
   ! the channel so chunked roll-overs can re-create it.
   subroutine build_nc_varlist(this)
      class(type_output_channel), intent(inout) :: this

      integer :: iv, is, it, n, n_it

      ! statistic variables inherit the base variable's attrs; hidden
      ! variables (derived sources) define nothing; derived outputs
      ! define under their own name with the source variable's attrs
      allocate (this%nc_names(this%n_vars*(1 + this%n_stats*max(1, this%n_thr)) + this%n_derived))
      allocate (this%nc_meta(this%n_vars*(1 + this%n_stats*max(1, this%n_thr)) + this%n_derived))
      n = 0
      do iv = 1, this%n_vars
         if (this%hidden(iv)) cycle
         if (this%snapshot) then
            n = n + 1
            this%nc_names(n) = trim(this%prefixes(iv))
            this%nc_meta(n) = this%meta(iv)
         end if
         do is = 1, this%n_stats
            n_it = 1
            if (any(EVENT_STATS == this%statistics(is))) n_it = max(1, this%n_thr)
            do it = 1, n_it
               n = n + 1
               this%nc_names(n) = stat_name(this, iv, is, it)
               this%nc_meta(n) = stat_meta(this, iv, is, it)
            end do
         end do
      end do
      do is = 1, this%n_derived
         n = n + 1
         this%nc_names(n) = trim(this%derived(is)%name)
         this%nc_meta(n) = this%meta(this%derived(is)%iv)
         this%nc_meta(n)%n_flags = 0
      end do
      this%nc_n = n
      call build_scalar_coords(this, this%nc_scalars, this%nc_nsc)
   end subroutine build_nc_varlist

   ! Define the serial field stream (IO rank only): a shared-root
   ! group, the first chunk, or one data.nc per channel.
   subroutine init_netcdf_backend(this, grid, diag_ncid)
      class(type_output_channel), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      integer, intent(in), optional :: diag_ncid

      integer :: root

      root = -1
      if (present(diag_ncid)) root = diag_ncid

      call build_nc_varlist(this)

      if (root >= 0) then
         ! layout 'single': the stream is a group in the shared root
         call this%nc%create(trim(this%id), grid%M, grid%N, &
                             grid%dx0, grid%dy0, this%nc_names, &
                             this%nc_meta, this%nc_n, root=root, &
                             scalars=this%nc_scalars(1:this%nc_nsc))
      else if (this%chunk_window > 0.0_SP) then
         call create_chunk_file(this)
      else
         call this%nc%create(this%result_folder//trim(this%id)//'.nc', grid%M, grid%N, &
                             grid%dx0, grid%dy0, this%nc_names, this%nc_meta, &
                             this%nc_n, &
                             scalars=this%nc_scalars(1:this%nc_nsc))
      end if
   end subroutine init_netcdf_backend

   ! Open the chunk covering [t_chunk0, t_chunk1); the name carries the
   ! window bounds (zero-padded seconds, deterministic and sortable).
   ! Serial branch runs on the IO rank alone (comm not needed); the
   ! pnetcdf branch is collective and requires it.
   subroutine create_chunk_file(this, comm)
      class(type_output_channel), intent(inout) :: this
      type(type_comm), intent(inout), optional :: comm

      character(:), allocatable :: fname

      fname = this%result_folder//trim(this%id)//'_'// &
              chunk_stamp(this%t_chunk0)//'-'// &
              chunk_stamp(this%t_chunk1)//'.nc'
      if (trim(this%format) == 'pnetcdf') then
         call this%pnc%create(fname, this%gatherer%M, this%gatherer%N, &
                              this%dx0, this%dy0, this%nc_names, &
                              this%nc_meta, this%nc_n, comm, &
                              scalars=this%nc_scalars(1:this%nc_nsc))
      else
         call this%nc%create(fname, this%gatherer%M, this%gatherer%N, &
                             this%dx0, this%dy0, this%nc_names, &
                             this%nc_meta, this%nc_n, &
                             scalars=this%nc_scalars(1:this%nc_nsc))
      end if
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

      character(VARNAME_LEN + STATNAME_LEN + THRTAG_LEN + 1), allocatable :: names(:)
      character(32), allocatable :: methods(:)
      type(type_var_meta), allocatable :: vmeta(:)
      type(type_scalar_coord), allocatable :: scalars(:)
      integer :: n, nsc

      call build_point_varlist(this, names, methods, vmeta, n)
      call build_scalar_coords(this, scalars, nsc)
      call this%ncp%create_group(diag_ncid, trim(this%id), x, y, &
                                 names, vmeta, methods, n, this%n_stats > 0, &
                                 scalars(1:nsc))
   end subroutine init_netcdf_points

   ! Point-channel variable list: snapshot variables plus every
   ! <prefix>_<stat> with its CF cell_methods; statistic variables inherit
   ! the base variable's attrs minus the flag pair
   subroutine build_point_varlist(this, names, methods, vmeta, n)
      class(type_output_channel), intent(in) :: this
      character(VARNAME_LEN + STATNAME_LEN + THRTAG_LEN + 1), allocatable, intent(out) :: names(:)
      character(32), allocatable, intent(out) :: methods(:)
      type(type_var_meta), allocatable, intent(out) :: vmeta(:)
      integer, intent(out) :: n

      integer :: iv, is, it, n_it

      allocate (names(this%n_vars*(1 + this%n_stats*max(1, this%n_thr))))
      allocate (methods(this%n_vars*(1 + this%n_stats*max(1, this%n_thr))))
      allocate (vmeta(this%n_vars*(1 + this%n_stats*max(1, this%n_thr))))
      n = 0
      do iv = 1, this%n_vars
         if (this%snapshot) then
            n = n + 1
            names(n) = trim(this%prefixes(iv))
            methods(n) = 'time: point'
            vmeta(n) = this%meta(iv)
         end if
         do is = 1, this%n_stats
            n_it = 1
            if (any(EVENT_STATS == this%statistics(is))) n_it = max(1, this%n_thr)
            do it = 1, n_it
               n = n + 1
               names(n) = stat_name(this, iv, is, it)
               methods(n) = stat_cell_method(trim(this%statistics(is)))
               vmeta(n) = stat_meta(this, iv, is, it)
            end do
         end do
      end do
   end subroutine build_point_varlist

   ! ----------------------------------------------------------------
   ! metadata.yaml for an ascii/binary channel: the header the netcdf
   ! backend would write (globals, dimensions, every variable with its
   ! CF attrs), key for key, so a reader sees the same metadata from
   ! either source, plus a frames: block (file pattern, element type,
   ! byte order, layout) so the folder reads standalone without the
   ! deck.  Built as a fortran-yaml-c node tree and dumped by the
   ! library, the same emitter the reader round-trips.  Static, written
   ! once at init on the IO rank.  netcdf/pnetcdf channels carry it
   ! in-file and write none.
   ! ----------------------------------------------------------------
   subroutine write_metadata_yaml(this, m, n)
      use, intrinsic :: iso_fortran_env, only: int8, int32
      use fortran_yaml_c, only: type_dictionary
      class(type_output_channel), intent(in) :: this
      integer, intent(in), optional :: m, n   ! field: global grid size

      character(VARNAME_LEN + STATNAME_LEN + THRTAG_LEN + 1), allocatable :: names(:)
      character(32), allocatable :: methods(:)
      type(type_var_meta), allocatable :: vmeta(:)
      type(type_scalar_coord), allocatable :: scalars(:)
      type(type_dictionary), pointer :: root, blk, vars, var
      integer(int8) :: probe(4)
      character(8) :: bits
      integer :: unit, i, nv, nsc
      logical :: field

      field = present(m)
      allocate (root)

      blk => yaml_child(root, 'frames')
      if (field) then
         call blk%set_string('pattern', yaml_quoted('<variable>_NNNNN'))
         call blk%set_string('counter_digits', '5')
         if (trim(this%format) == 'binary') then
            write (bits, '(I0)') storage_size(0.0_SP)
            probe = transfer(1_int32, probe)
            call blk%set_string('dtype', yaml_quoted('float'//trim(bits)))
            call blk%set_string('byte_order', &
                                yaml_quoted(trim(merge('little', 'big   ', probe(1) == 1_int8))))
            call blk%set_string('layout', &
                                yaml_quoted('raw stream of the (x, y) array, x fastest, no header'))
         else
            call blk%set_string('dtype', yaml_quoted('text'))
            call blk%set_string('layout', yaml_quoted('one line per y, x values across'))
         end if
      else
         call blk%set_string('pattern', yaml_quoted('<variable>.dat'))
         call blk%set_string('dtype', yaml_quoted('text'))
         call blk%set_string('layout', &
                             yaml_quoted('one line per flush: time, then the point values in order'))
      end if

      if (this%log_events) then
         ! the event log beside the frames: one file per variable and
         ! threshold, fixed columns, the peak in the variable's units
         blk => yaml_child(root, 'events')
         call blk%set_string('pattern', yaml_quoted('events_<variable>[_<threshold tag>].dat'))
         call blk%set_string('columns', yaml_quoted('t_on t_off duration peak x y i j'))
         call blk%set_string('units', yaml_quoted('s s s <variable> m m 1 1'))
         call blk%set_string('order', yaml_quoted('by onset, then cell; one row per committed event'))
         call blk%set_string('invariant', yaml_quoted('rows == the count statistic summed over cells,'// &
                                                      ' unless a # dropped line is present'))
         call blk%set_string('peak', yaml_quoted('signed sample furthest past the threshold;'// &
                                                 ' a magnitude threshold selects on |value|'))
         call blk%set_string('coordinates', yaml_quoted('model metres, x = (i-1) dx, y = (j-1) dy'// &
                                                        ' from cell (1,1): NOT georeferenced'))
         call blk%set_string('restart', yaml_quoted('appends behind a # restart t_start=<t> line;'// &
                                                    ' # open rows at the end of a leg list the events'// &
                                                    ' still open (lost across the restart)'))
         call blk%set_string('cell', yaml_quoted('i j global 1-based; on a point geometry i = point index, j = 1'))
      end if

      call root%set_string('Conventions', yaml_quoted('CF-1.8'))
      call root%set_string('source', yaml_quoted('FUNWAVE-TVD'))

      blk => yaml_child(root, 'dimensions')
      if (field) then
         write (bits, '(I0)') m
         call blk%set_string('x', trim(bits))
         write (bits, '(I0)') n
         call blk%set_string('y', trim(bits))
      else
         write (bits, '(I0)') this%n_global
         call blk%set_string('point', trim(bits))
         if (this%n_stats > 0) call blk%set_string('bnds', '2')
      end if
      call blk%set_string('time', 'unlimited')

      vars => yaml_child(root, 'variables')
      var => yaml_child(vars, 'x')
      call var%set_string('units', yaml_quoted('m'))
      var => yaml_child(vars, 'y')
      call var%set_string('units', yaml_quoted('m'))
      var => yaml_child(vars, 'time')
      call var%set_string('units', yaml_quoted('seconds since start'))
      if (.not. field .and. this%n_stats > 0) then
         call var%set_string('bounds', yaml_quoted('time_bnds'))
         var => yaml_child(vars, 'time_bnds')
      end if
      call build_scalar_coords(this, scalars, nsc)
      do i = 1, nsc
         var => yaml_child(vars, trim(scalars(i)%name))
         call var%set_string('value', threshold_text(scalars(i)%value))
         call yaml_var_atts(var, scalars(i)%meta)
      end do
      if (field) then
         do i = 1, this%nc_n
            var => yaml_child(vars, trim(this%nc_names(i)))
            call yaml_var_atts(var, this%nc_meta(i))
         end do
      else
         call build_point_varlist(this, names, methods, vmeta, nv)
         do i = 1, nv
            var => yaml_child(vars, trim(names(i)))
            call var%set_string('cell_methods', yaml_quoted(trim(methods(i))))
            call yaml_var_atts(var, vmeta(i))
         end do
      end if

      open (newunit=unit, file=this%result_folder//'metadata.yaml', &
            status='replace', action='write')
      write (unit, '(A)') '# netcdf header of this channel (CF attributes for the '// &
         trim(this%format)//' frames) and how the frames are laid out'
      call root%dump(unit, 0)
      close (unit)
      call root%finalize()
      deallocate (root)
   end subroutine write_metadata_yaml

   ! One variable's attrs onto its node, the nc_put_var_atts set in its order
   subroutine yaml_var_atts(var, meta)
      use fortran_yaml_c, only: type_dictionary, type_list, type_scalar
      type(type_dictionary), intent(inout) :: var
      type(type_var_meta), intent(in) :: meta

      type(type_list), pointer :: vals
      type(type_scalar), pointer :: val
      character(24) :: buf
      integer :: k

      if (len_trim(meta%units) > 0) &
         call var%set_string('units', yaml_quoted(trim(meta%units)))
      if (len_trim(meta%long_name) > 0) &
         call var%set_string('long_name', yaml_quoted(trim(meta%long_name)))
      if (len_trim(meta%standard_name) > 0) &
         call var%set_string('standard_name', yaml_quoted(trim(meta%standard_name)))
      if (len_trim(meta%funwave_name) > 0) &
         call var%set_string('funwave_name', yaml_quoted(trim(meta%funwave_name)))
      if (len_trim(meta%comment) > 0) &
         call var%set_string('comment', yaml_quoted(trim(meta%comment)))
      if (len_trim(meta%coordinates) > 0) &
         call var%set_string('coordinates', yaml_quoted(trim(meta%coordinates)))
      if (meta%has_fill) then
         write (buf, '(G0.6)') meta%fill_value
         call var%set_string('_FillValue', trim(adjustl(buf)))
      end if
      if (meta%n_flags > 0) then
         allocate (vals)
         do k = 1, meta%n_flags
            allocate (val)
            write (buf, '(G0.6)') meta%flag_values(k)
            val%string = trim(adjustl(buf))
            call vals%append(val)
         end do
         call yaml_set_node(var, 'flag_values', vals)
         call var%set_string('flag_meanings', yaml_quoted(trim(meta%flag_meanings)))
      end if
   end subroutine yaml_var_atts

   ! New empty dictionary under parent%key, returned for filling
   function yaml_child(parent, key) result(child)
      use fortran_yaml_c, only: type_dictionary
      type(type_dictionary), intent(inout) :: parent
      character(*), intent(in) :: key
      type(type_dictionary), pointer :: child

      allocate (child)
      call yaml_set_node(parent, key, child)
   end function yaml_child

   ! dictionary%set takes a class(type_node) pointer; this does the upcast
   subroutine yaml_set_node(parent, key, node)
      use fortran_yaml_c, only: type_dictionary, type_node
      type(type_dictionary), intent(inout) :: parent
      character(*), intent(in) :: key
      class(type_node), target, intent(in) :: node

      class(type_node), pointer :: p

      p => node
      call parent%set(key, p)
   end subroutine yaml_set_node

   ! The event direction in words (THR_ABOVE / THR_BELOW / THR_ABS)
   pure function dir_text(dir) result(txt)
      integer, intent(in) :: dir
      character(:), allocatable :: txt
      select case (dir)
      case (1)
         txt = 'above'
      case (-1)
         txt = 'below'
      case default
         txt = 'in magnitude above'
      end select
   end function dir_text

   ! Scalars dump verbatim, so a string value carries its own quotes
   pure function yaml_quoted(s) result(q)
      character(*), intent(in) :: s
      character(:), allocatable :: q
      q = '"'//s//'"'
   end function yaml_quoted

   ! CF cell_methods label for one accumulator statistic
   pure function stat_cell_method(stat) result(cm)
      character(*), intent(in) :: stat
      character(:), allocatable :: cm
      select case (stat)
      case ('min')
         cm = 'time: minimum'
      case ('max')
         cm = 'time: maximum'
      case ('duration_max')
         cm = 'time: maximum'
      case ('duration', 'count')
         cm = 'time: sum'
      case ('max_time', 'first_time', 'last_time')
         cm = 'time: point'
      case ('mean')
         cm = 'time: mean'
      case ('std')
         cm = 'time: standard_deviation'
      case default   ! 'rms'
         cm = 'time: root_mean_square'
      end select
   end function stat_cell_method

   ! Collective MPI-IO twin of write_field_file's binary branch (legacy
   ! PutFileBinary, after Gropp lecture 33): the file view maps each
   ! rank's (local_nx, local_ny) interior tile to its 0-based (i0, j0)
   ! subarray offset in the global (M, N) array, then one write_all puts
   ! every tile concurrently — byte-identical to the gathered stream.
   subroutine write_field_file_mpiio(fname, vals, M, N, i0, j0, comm, single)
      use, intrinsic :: iso_fortran_env, only: real32
      character(*), intent(in) :: fname
      real(SP), intent(in) :: vals(:, :)   ! interior tile, ghost-free
      integer, intent(in) :: M, N, i0, j0
      type(type_comm), intent(inout) :: comm
      logical, intent(in), optional :: single

      type(MPI_Datatype) :: etype, ftype
      type(MPI_File) :: fh
      real(real32), allocatable :: vals32(:, :)
      integer(MPI_OFFSET_KIND) :: zero_off
      logical :: to32
      integer :: ierr

      to32 = .false.
      if (present(single)) to32 = single
      etype = MPI_SP
      if (to32) etype = MPI_REAL4

      call MPI_Type_create_subarray(2, [M, N], shape(vals), [i0, j0], &
                                    MPI_ORDER_FORTRAN, etype, ftype, ierr)
      call MPI_Type_commit(ftype, ierr)

      call MPI_File_open(comm%id, fname, MPI_MODE_WRONLY + MPI_MODE_CREATE, &
                         MPI_INFO_NULL, fh, ierr)
      ! MPI_MODE_CREATE does not truncate: a rerun over a larger stale
      ! file (e.g. prior ASCII output) would keep a garbage tail
      zero_off = 0
      call MPI_File_set_size(fh, zero_off, ierr)
      call MPI_Barrier(comm%id, ierr)
      call MPI_File_set_view(fh, zero_off, etype, ftype, 'native', &
                             MPI_INFO_NULL, ierr)
      if (to32) then
         vals32 = real(vals, real32)
         call MPI_File_write_all(fh, vals32, size(vals), MPI_REAL4, &
                                 MPI_STATUS_IGNORE, ierr)
      else
         call MPI_File_write_all(fh, vals, size(vals), MPI_SP, &
                                 MPI_STATUS_IGNORE, ierr)
      end if
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
      integer :: k, unit, n_got

      allocate (gathered(merge(this%n_global, 1, comm%is_io_node())))
      call this%gatherer%gather_vals(local_vals, gathered, comm)

      if (comm%is_io_node()) then
         ! Restore point order: gathered is rank-ordered.  Gatherv fills only
         ! sum(recv_counts) entries -- stations outside EVERY rank's subdomain
         ! are dropped by the interpolator and contribute nothing -- so the
         ! loop must stop there.  Running to n_global read past the gathered
         ! data and, because point_ids is zero-filled beyond it, wrote to
         ! sorted(0): an out-of-bounds store of an uninitialised value.
         !
         ! Unresolved slots keep the 0.0 initialiser rather than a sentinel.
         ! Station VALIDITY is reported out-of-band by requesting `mask` on
         ! the same channel: it rides the same interpolation, so an
         ! unresolved station reads mask 0 alongside its 0.0 value, and --
         ! more importantly -- a station that is INSIDE the domain but dry
         ! or inside a structure reads mask 0 while its field value is a
         ! meaningless interpolation of dry cells.  A sentinel here would
         ! catch only the first case and would be static; the mask is
         ! per-step, which is what a moving wet/dry line needs.
         n_got = sum(this%gatherer%recv_counts)
         allocate (sorted(this%n_global), source=0.0_SP)
         do k = 1, n_got
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
      ! the channel folder carries the identity; no <id>_ prefix
      fname = this%result_folder//trim(name)//'.dat'
   end function point_file_name

   ! Truncate all point files this channel will append to (IO rank only).
   subroutine truncate_point_files(this)
      class(type_output_channel), intent(in) :: this
      integer :: iv, is, it, n_it, unit

      do iv = 1, this%n_vars
         if (this%snapshot) then
            open (newunit=unit, file=point_file_name(this, trim(this%prefixes(iv))), &
                  status='replace', action='write')
            close (unit)
         end if
         do is = 1, this%n_stats
            n_it = 1
            if (any(EVENT_STATS == this%statistics(is))) n_it = max(1, this%n_thr)
            do it = 1, n_it
               open (newunit=unit, file=point_file_name(this, stat_name(this, iv, is, it)), &
                     status='replace', action='write')
               close (unit)
            end do
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

   ! Per-variable CF attributes; a blank component writes nothing.  The
   ! flag_values kind follows the variable's own type, as CF requires
   subroutine nc_put_var_atts(ncid, varid, name, meta, single)
      integer, intent(in) :: ncid, varid
      character(*), intent(in) :: name
      type(type_var_meta), intent(in) :: meta
      logical, intent(in) :: single

      if (len_trim(meta%units) > 0) &
         call nc_check(nf90_put_att(ncid, varid, 'units', trim(meta%units)), &
                       'att units '//trim(name))
      if (len_trim(meta%long_name) > 0) &
         call nc_check(nf90_put_att(ncid, varid, 'long_name', trim(meta%long_name)), &
                       'att long_name '//trim(name))
      if (len_trim(meta%standard_name) > 0) &
         call nc_check(nf90_put_att(ncid, varid, 'standard_name', &
                                    trim(meta%standard_name)), &
                       'att standard_name '//trim(name))
      if (len_trim(meta%funwave_name) > 0) &
         call nc_check(nf90_put_att(ncid, varid, 'funwave_name', &
                                    trim(meta%funwave_name)), &
                       'att funwave_name '//trim(name))
      if (len_trim(meta%comment) > 0) &
         call nc_check(nf90_put_att(ncid, varid, 'comment', trim(meta%comment)), &
                       'att comment '//trim(name))
      if (len_trim(meta%coordinates) > 0) &
         call nc_check(nf90_put_att(ncid, varid, 'coordinates', trim(meta%coordinates)), &
                       'att coordinates '//trim(name))
      if (meta%has_fill) then
         if (single) then
            call nc_check(nf90_put_att(ncid, varid, '_FillValue', [real(meta%fill_value, 4)]), &
                          'att _FillValue '//trim(name))
         else
            call nc_check(nf90_put_att(ncid, varid, '_FillValue', [real(meta%fill_value, 8)]), &
                          'att _FillValue '//trim(name))
         end if
      end if
      if (meta%n_flags > 0) then
         if (single) then
            call nc_check(nf90_put_att(ncid, varid, 'flag_values', &
                                       real(meta%flag_values(1:meta%n_flags), 4)), &
                          'att flag_values '//trim(name))
         else
            call nc_check(nf90_put_att(ncid, varid, 'flag_values', &
                                       real(meta%flag_values(1:meta%n_flags), 8)), &
                          'att flag_values '//trim(name))
         end if
         call nc_check(nf90_put_att(ncid, varid, 'flag_meanings', &
                                    trim(meta%flag_meanings)), &
                       'att flag_meanings '//trim(name))
      end if
   end subroutine nc_put_var_atts

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
   subroutine nc_create(this, fname, M, N, dx, dy, names, meta, n_names, root, scalars)
      class(type_netcdf_field_writer), intent(inout) :: this
      character(*), intent(in) :: fname
      integer, intent(in) :: M, N
      real(SP), intent(in) :: dx, dy
      character(*), intent(in) :: names(:)
      type(type_var_meta), intent(in) :: meta(:)
      integer, intent(in) :: n_names
      integer, intent(in), optional :: root
      type(type_scalar_coord), intent(in), optional :: scalars(:)

      integer :: x_dim, y_dim, t_dim, x_var, y_var
      integer :: i
      integer, allocatable :: sc_var(:)
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
         call nc_check(nf90_def_var(this%ncid, trim(names(i)), &
                                    merge(NF90_FLOAT, NF90_DOUBLE, this%single), &
                                    [x_dim, y_dim, t_dim], this%varids(i)), &
                       'def var '//trim(names(i)))
         call nc_put_var_atts(this%ncid, this%varids(i), names(i), meta(i), this%single)
      end do
      call nc_def_scalars(this%ncid, scalars, sc_var)

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
      call nc_put_scalars(this%ncid, scalars, sc_var)

      this%nrec = 0
      this%is_open = .true.
   end subroutine nc_create

   ! Scalar coordinate variables (no dimensions): define with their attrs
   ! before enddef, put the values after -- sc_var carries the ids across
   subroutine nc_def_scalars(ncid, scalars, sc_var)
      integer, intent(in) :: ncid
      type(type_scalar_coord), intent(in), optional :: scalars(:)
      integer, allocatable, intent(out) :: sc_var(:)

      integer :: no_dims(0)
      integer :: i

      if (.not. present(scalars)) then
         allocate (sc_var(0))
         return
      end if
      allocate (sc_var(size(scalars)))
      do i = 1, size(scalars)
         call nc_check(nf90_def_var(ncid, trim(scalars(i)%name), NF90_DOUBLE, &
                                    no_dims, sc_var(i)), &
                       'def scalar '//trim(scalars(i)%name))
         call nc_put_var_atts(ncid, sc_var(i), scalars(i)%name, scalars(i)%meta, .false.)
      end do
   end subroutine nc_def_scalars

   subroutine nc_put_scalars(ncid, scalars, sc_var)
      integer, intent(in) :: ncid
      type(type_scalar_coord), intent(in), optional :: scalars(:)
      integer, intent(in) :: sc_var(:)

      integer :: i

      if (.not. present(scalars)) return
      do i = 1, size(scalars)
         call nc_check(nf90_put_var(ncid, sc_var(i), real(scalars(i)%value, 8)), &
                       'put scalar '//trim(scalars(i)%name))
      end do
   end subroutine nc_put_scalars

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
                               methods, n_names, windowed, scalars)
      class(type_netcdf_point_writer), intent(inout) :: this
      integer, intent(in) :: root
      character(*), intent(in) :: id
      real(SP), intent(in) :: x(:), y(:)
      character(*), intent(in) :: names(:)
      type(type_var_meta), intent(in) :: meta(:)
      character(*), intent(in) :: methods(:)
      integer, intent(in) :: n_names
      logical, intent(in) :: windowed
      type(type_scalar_coord), intent(in), optional :: scalars(:)

      integer :: p_dim, t_dim, b_dim, x_var, y_var
      integer :: i
      integer, allocatable :: sc_var(:)

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
         call nc_put_var_atts(this%grpid, this%varids(i), names(i), meta(i), .false.)
      end do
      call nc_def_scalars(this%grpid, scalars, sc_var)

      call nc_check(nf90_put_var(this%grpid, x_var, x), 'put x '//id)
      call nc_check(nf90_put_var(this%grpid, y_var, y), 'put y '//id)
      call nc_put_scalars(this%grpid, scalars, sc_var)

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

   ! ---- PnetCDF backend (parallel CDF-5; every method collective) ----

   ! pnetcdf twin of nc_put_var_atts
   subroutine pnc_put_var_atts(ncid, varid, name, meta, single)
      integer, intent(in) :: ncid, varid
      character(*), intent(in) :: name
      type(type_var_meta), intent(in) :: meta
      logical, intent(in) :: single

      if (len_trim(meta%units) > 0) &
         call pnc_check(nf90mpi_put_att(ncid, varid, 'units', trim(meta%units)), &
                        'att units '//trim(name))
      if (len_trim(meta%long_name) > 0) &
         call pnc_check(nf90mpi_put_att(ncid, varid, 'long_name', &
                                        trim(meta%long_name)), &
                        'att long_name '//trim(name))
      if (len_trim(meta%standard_name) > 0) &
         call pnc_check(nf90mpi_put_att(ncid, varid, 'standard_name', &
                                        trim(meta%standard_name)), &
                        'att standard_name '//trim(name))
      if (len_trim(meta%funwave_name) > 0) &
         call pnc_check(nf90mpi_put_att(ncid, varid, 'funwave_name', &
                                        trim(meta%funwave_name)), &
                        'att funwave_name '//trim(name))
      if (len_trim(meta%comment) > 0) &
         call pnc_check(nf90mpi_put_att(ncid, varid, 'comment', trim(meta%comment)), &
                        'att comment '//trim(name))
      if (len_trim(meta%coordinates) > 0) &
         call pnc_check(nf90mpi_put_att(ncid, varid, 'coordinates', trim(meta%coordinates)), &
                        'att coordinates '//trim(name))
      if (meta%has_fill) then
         if (single) then
            call pnc_check(nf90mpi_put_att(ncid, varid, '_FillValue', [real(meta%fill_value, 4)]), &
                           'att _FillValue '//trim(name))
         else
            call pnc_check(nf90mpi_put_att(ncid, varid, '_FillValue', [real(meta%fill_value, 8)]), &
                           'att _FillValue '//trim(name))
         end if
      end if
      if (meta%n_flags > 0) then
         if (single) then
            call pnc_check(nf90mpi_put_att(ncid, varid, 'flag_values', &
                                           real(meta%flag_values(1:meta%n_flags), 4)), &
                           'att flag_values '//trim(name))
         else
            call pnc_check(nf90mpi_put_att(ncid, varid, 'flag_values', &
                                           real(meta%flag_values(1:meta%n_flags), 8)), &
                           'att flag_values '//trim(name))
         end if
         call pnc_check(nf90mpi_put_att(ncid, varid, 'flag_meanings', &
                                        trim(meta%flag_meanings)), &
                        'att flag_meanings '//trim(name))
      end if
   end subroutine pnc_put_var_atts

   subroutine pnc_check(status, what)
      integer, intent(in) :: status
      character(*), intent(in) :: what
      if (status /= NF90_NOERR) then
         write (*, '(A)') 'output_channel/pnetcdf: '//what//': '// &
            trim(nf90mpi_strerror(status))
         error stop 'output_channel: fatal PnetCDF error'
      end if
   end subroutine pnc_check

   ! Define the file (CDF-5): same dims/vars/attrs as the serial
   ! writer.  Static coords and per-frame times use the count-0
   ! collective pattern — every rank participates, only the IO rank
   ! contributes elements.
   subroutine pnc_create(this, fname, M, N, dx, dy, names, meta, n_names, comm, scalars)
      class(type_pnetcdf_field_writer), intent(inout) :: this
      character(*), intent(in) :: fname
      integer, intent(in) :: M, N
      real(SP), intent(in) :: dx, dy
      character(*), intent(in) :: names(:)
      type(type_var_meta), intent(in) :: meta(:)
      integer, intent(in) :: n_names
      type(type_comm), intent(inout) :: comm
      type(type_scalar_coord), intent(in), optional :: scalars(:)

      integer :: x_dim, y_dim, t_dim, x_var, y_var
      integer :: i, nsc
      integer :: no_dims(0)
      integer, allocatable :: sc_var(:)
      integer(kind=MPI_OFFSET_KIND) :: dlen
      real(SP), allocatable :: coord(:)

      call pnc_check(nf90mpi_create(comm%id%mpi_val, fname, &
                                    ior(NF90_CLOBBER, PNC_64BIT_DATA), &
                                    MPI_INFO_NULL%mpi_val, this%ncid), &
                     'create '//fname)

      dlen = int(M, MPI_OFFSET_KIND)
      call pnc_check(nf90mpi_def_dim(this%ncid, 'x', dlen, x_dim), 'def x')
      dlen = int(N, MPI_OFFSET_KIND)
      call pnc_check(nf90mpi_def_dim(this%ncid, 'y', dlen, y_dim), 'def y')
      dlen = int(NF90_UNLIMITED, MPI_OFFSET_KIND)
      call pnc_check(nf90mpi_def_dim(this%ncid, 'time', dlen, t_dim), &
                     'def time')

      call pnc_check(nf90mpi_def_var(this%ncid, 'x', NF90_DOUBLE, [x_dim], &
                                     x_var), 'def var x')
      call pnc_check(nf90mpi_put_att(this%ncid, x_var, 'units', 'm'), &
                     'att x units')
      call pnc_check(nf90mpi_def_var(this%ncid, 'y', NF90_DOUBLE, [y_dim], &
                                     y_var), 'def var y')
      call pnc_check(nf90mpi_put_att(this%ncid, y_var, 'units', 'm'), &
                     'att y units')
      call pnc_check(nf90mpi_def_var(this%ncid, 'time', NF90_DOUBLE, [t_dim], &
                                     this%time_varid), 'def var time')
      call pnc_check(nf90mpi_put_att(this%ncid, this%time_varid, 'units', &
                                     'seconds since start'), 'att time units')

      this%n_vars = n_names
      allocate (this%names(n_names), this%varids(n_names))
      do i = 1, n_names
         this%names(i) = names(i)
         call pnc_check(nf90mpi_def_var(this%ncid, trim(names(i)), &
                                        merge(NF90_FLOAT, NF90_DOUBLE, this%single), &
                                        [x_dim, y_dim, t_dim], &
                                        this%varids(i)), &
                        'def var '//trim(names(i)))
         call pnc_put_var_atts(this%ncid, this%varids(i), names(i), meta(i), this%single)
      end do
      ! scalar coordinates (the event thresholds): no dimensions
      nsc = 0
      if (present(scalars)) nsc = size(scalars)
      allocate (sc_var(nsc))
      do i = 1, nsc
         call pnc_check(nf90mpi_def_var(this%ncid, trim(scalars(i)%name), NF90_DOUBLE, &
                                        no_dims, sc_var(i)), &
                        'def scalar '//trim(scalars(i)%name))
         call pnc_put_var_atts(this%ncid, sc_var(i), scalars(i)%name, scalars(i)%meta, .false.)
      end do

      call pnc_check(nf90mpi_put_att(this%ncid, NF90_GLOBAL, 'Conventions', &
                                     'CF-1.8'), 'att Conventions')
      call pnc_check(nf90mpi_put_att(this%ncid, NF90_GLOBAL, 'source', &
                                     'FUNWAVE-TVD'), 'att source')
      call pnc_check(nf90mpi_enddef(this%ncid), 'enddef')

      ! cell-center coordinates on the uniform spacing
      allocate (coord(max(M, N)))
      do i = 1, M
         coord(i) = real(i - 1, SP)*dx
      end do
      call pnc_check(pnc_put_replicated(this%ncid, x_var, coord(1:M), 1), &
                     'put x')
      do i = 1, N
         coord(i) = real(i - 1, SP)*dy
      end do
      call pnc_check(pnc_put_replicated(this%ncid, y_var, coord(1:N), 1), &
                     'put y')
      do i = 1, nsc
         call pnc_check(pnc_put_replicated(this%ncid, sc_var(i), [scalars(i)%value], 1), &
                        'put scalar '//trim(scalars(i)%name))
      end do

      this%nrec = 0
      this%is_open = .true.
   end subroutine pnc_create

   ! Collective 1-D write of rank-identical data: every rank puts the
   ! full range with the same bytes (deterministic despite the formal
   ! overlapping-write caveat).  NOT the count-0 participation pattern
   ! — PnetCDF 1.15 record-variable puts diverge on it (the numrecs
   ! sync collective mismatches -> MPI_ERR_TRUNCATE or a hang).  The
   ! buffer is copied: PnetCDF's F90 interfaces omit intent(in) on
   ! values, so an intent(in) actual fails generic resolution.
   integer function pnc_put_replicated(ncid, varid, vals, start1) &
      result(status)
      integer, intent(in) :: ncid, varid, start1
      real(SP), intent(in) :: vals(:)

      integer(kind=MPI_OFFSET_KIND) :: start(1), count(1)
      real(SP), allocatable :: buf(:)

      buf = vals
      start = int(start1, MPI_OFFSET_KIND)
      count = int(size(vals), MPI_OFFSET_KIND)
      status = nf90mpi_put_var_all(ncid, varid, buf, start=start, count=count)
   end function pnc_put_replicated

   ! Advance the record dimension and stamp its time value.
   subroutine pnc_begin_frame(this, t)
      class(type_pnetcdf_field_writer), intent(inout) :: this
      real(SP), intent(in) :: t
      this%nrec = this%nrec + 1
      call pnc_check(pnc_put_replicated(this%ncid, this%time_varid, [t], &
                                        this%nrec), 'put time')
   end subroutine pnc_begin_frame

   ! Write this rank's interior tile at its global subarray offset in
   ! the current record.  Collective.
   subroutine pnc_put(this, name, tile, i0, j0)
      class(type_pnetcdf_field_writer), intent(inout) :: this
      character(*), intent(in) :: name
      real(SP), intent(in) :: tile(:, :)
      integer, intent(in) :: i0, j0

      integer :: i, id
      integer(kind=MPI_OFFSET_KIND) :: start(3), count(3)
      real(SP), allocatable :: buf(:, :)

      id = -1
      do i = 1, this%n_vars
         if (trim(this%names(i)) == trim(name)) then
            id = this%varids(i)
            exit
         end if
      end do
      if (id < 0) &
         error stop 'output_channel: pnetcdf put of undefined variable '//name

      ! definable copy (PnetCDF F90 values args carry no intent)
      buf = tile
      start = int([i0 + 1, j0 + 1, this%nrec], MPI_OFFSET_KIND)
      count = int([size(tile, 1), size(tile, 2), 1], MPI_OFFSET_KIND)
      call pnc_check(nf90mpi_put_var_all(this%ncid, id, buf, &
                                         start=start, count=count), &
                     'put '//trim(name))
   end subroutine pnc_put

   subroutine pnc_close(this)
      class(type_pnetcdf_field_writer), intent(inout) :: this
      if (this%is_open) call pnc_check(nf90mpi_close(this%ncid), 'close')
      this%is_open = .false.
      this%ncid = -1
      this%nrec = 0
      this%n_vars = 0
      if (allocated(this%names)) deallocate (this%names)
      if (allocated(this%varids)) deallocate (this%varids)
   end subroutine pnc_close

   subroutine channel_finalize(this)
      class(type_output_channel), intent(inout) :: this
      integer :: iv, it
      call this%interp%finalize()
      call this%gatherer%finalize()
      call this%nc%close()
      call this%ncp%reset()
      call this%pnc%close()
      if (allocated(this%accum)) then
         do iv = 1, size(this%accum, 1)
            do it = 1, size(this%accum, 2)
               call this%accum(iv, it)%finalize()
            end do
         end do
         deallocate (this%accum)
      end if
      if (allocated(this%result_folder)) deallocate (this%result_folder)
      if (allocated(this%ev_grp)) deallocate (this%ev_grp, this%ev_nrec, this%ev_var)
      if (allocated(this%log_px)) deallocate (this%log_px, this%log_py)
      this%log_events = .false.
      this%log_ncid = -1
      this%log_dropped = 0
      this%log_warned = .false.
      this%log_seam_pending = .false.
      if (allocated(this%nc_names)) deallocate (this%nc_names)
      if (allocated(this%nc_meta)) deallocate (this%nc_meta)
      this%nc_n = 0
      this%chunk_window = 0.0_SP
      this%n_vars = 0
      this%n_stats = 0
      this%icount = 0
   end subroutine channel_finalize

end module core_output_channel_mod
