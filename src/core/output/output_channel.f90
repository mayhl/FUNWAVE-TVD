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
!  Field format follows the 'format' setting: 'ascii' gathers to the IO
!  rank and writes one row of M E16.6 values per J (legacy PutFileASCII
!  layout); 'binary' is a collective MPI-IO write — every rank puts its
!  interior tile at its global subarray offset in one shared file (legacy
!  PutFileBinary, Gropp lecture-33 pattern), no gather.  Both produce the
!  same bytes: the raw real(SP) global interior array in Fortran order.
!  'netcdf' gathers like ascii but appends every variable to one
!  data.nc per channel (x, y, time-unlimited; core_netcdf_writer_mod).
!  Point files are always ASCII and are truncated at init.
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
   contains
      procedure :: create => nc_create
      procedure :: begin_frame => nc_begin_frame
      procedure :: put => nc_put
      procedure :: close => nc_close
   end type type_netcdf_field_writer

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
                           file_prefixes, icount_start, var_meta)
      class(type_output_channel), intent(inout) :: this
      character(*), intent(in) :: id, geom_type
      character(*), intent(in) :: variables(*)
      integer, intent(in) :: n_vars
      character(*), intent(in) :: statistics(*)
      integer, intent(in) :: n_stats
      logical, intent(in) :: snapshot
      real(SP), intent(in) :: t_start, interval
      character(*), intent(in) :: result_folder  ! must include trailing separator
      character(*), intent(in) :: format         ! 'ascii' or 'binary' (field only)
      real(SP), intent(in) :: coords_x(*), coords_y(*)  ! global query coords
      integer, intent(in) :: n_coords   ! n_stations or n_transect_points (0 for field)
      type(type_grid_2d), intent(in)    :: grid
      type(type_comm), intent(inout) :: comm
      character(*), intent(in), optional :: file_prefixes(*)  ! per-var name overrides
      integer, intent(in), optional :: icount_start  ! pre-increment counter base
      type(type_var_meta), intent(in), optional :: var_meta(*)  ! per-var CF attrs

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

         ! Point files append per flush; start each run from empty files.
         if (comm%is_io_node()) call truncate_point_files(this)

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

         ! NetCDF backend: one data.nc per channel, every snapshot +
         ! statistic variable defined up front (names fixed at init)
         if (trim(format) == 'netcdf' .and. comm%is_io_node()) &
            call init_netcdf_backend(this, grid)

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

      ! One record per flush: stamp the time value before any variable
      ! lands (snapshot and statistics share the frame)
      if (do_flush .and. this%nc%is_open) call this%nc%begin_frame(t)

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

      ! --- Flush statistics at interval ---
      if (do_flush .and. this%n_stats > 0) then
         do iv = 1, this%n_vars
            call channel_write_stats(this, iv, t, comm)
            call this%accum(iv)%reset()
         end do
      end if

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

   ! Define the channel's data.nc: snapshot variables under their file
   ! prefixes plus every <prefix>_<stat> combination.  IO rank only.
   subroutine init_netcdf_backend(this, grid)
      class(type_output_channel), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      character(VARNAME_LEN + STATNAME_LEN + 1), allocatable :: names(:)
      type(type_var_meta), allocatable :: vmeta(:)
      integer :: iv, is, n

      ! statistic variables inherit the base variable's attrs
      allocate (names(this%n_vars*(1 + this%n_stats)))
      allocate (vmeta(this%n_vars*(1 + this%n_stats)))
      n = 0
      do iv = 1, this%n_vars
         if (this%snapshot) then
            n = n + 1
            names(n) = trim(this%prefixes(iv))
            vmeta(n) = this%meta(iv)
         end if
         do is = 1, this%n_stats
            n = n + 1
            names(n) = trim(this%prefixes(iv))//'_'//trim(this%statistics(is))
            vmeta(n) = this%meta(iv)
         end do
      end do

      call this%nc%create(this%result_folder//'data.nc', grid%M, grid%N, &
                          grid%dx0, grid%dy0, names, vmeta, n)
   end subroutine init_netcdf_backend

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
         open (newunit=unit, file=point_file_name(this, name), &
               status='unknown', position='append', action='write')
         write (unit, '(*(E16.6))') t, sorted
         close (unit)
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
   ! existing file.
   subroutine nc_create(this, fname, M, N, dx, dy, names, meta, n_names)
      class(type_netcdf_field_writer), intent(inout) :: this
      character(*), intent(in) :: fname
      integer, intent(in) :: M, N
      real(SP), intent(in) :: dx, dy
      character(*), intent(in) :: names(:)
      type(type_var_meta), intent(in) :: meta(:)
      integer, intent(in) :: n_names

      integer :: x_dim, y_dim, t_dim, x_var, y_var
      integer :: i
      real(SP), allocatable :: coord(:)

      call nc_check(nf90_create(fname, ior(NF90_CLOBBER, NF90_NETCDF4), &
                                this%ncid), 'create '//fname)

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

      call nc_check(nf90_put_att(this%ncid, NF90_GLOBAL, 'Conventions', &
                                 'CF-1.8'), 'att Conventions')
      call nc_check(nf90_put_att(this%ncid, NF90_GLOBAL, 'source', &
                                 'FUNWAVE-TVD'), 'att source')
      call nc_check(nf90_enddef(this%ncid), 'enddef')

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
      if (this%is_open) call nc_check(nf90_close(this%ncid), 'close')
      this%is_open = .false.
      this%ncid = -1
      this%nrec = 0
      this%n_vars = 0
      if (allocated(this%names)) deallocate (this%names)
      if (allocated(this%varids)) deallocate (this%varids)
   end subroutine nc_close

   subroutine channel_finalize(this)
      class(type_output_channel), intent(inout) :: this
      integer :: iv
      call this%interp%finalize()
      call this%gatherer%finalize()
      call this%nc%close()
      if (allocated(this%accum)) then
         do iv = 1, size(this%accum)
            call this%accum(iv)%finalize()
         end do
         deallocate (this%accum)
      end if
      if (allocated(this%result_folder)) deallocate (this%result_folder)
      this%n_vars = 0
      this%n_stats = 0
      this%icount = 0
   end subroutine channel_finalize

end module core_output_channel_mod
