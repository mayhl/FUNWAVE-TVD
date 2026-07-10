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
!  include a trailing path separator):
!   field snapshot   <var>_NNNNN            (legacy PREVIEW naming, 5-digit
!   field statistic  <var>_<stat>_NNNNN      flush counter starting at 1)
!   point snapshot   <id>_<var>.dat          one row per flush: t, v(1..n)
!   point statistic  <id>_<var>_<stat>.dat   in point order
!  Field format follows the 'format' setting: 'ascii' writes one row of
!  M E16.6 values per J (legacy PutFileASCII layout); 'binary' writes the
!  raw real(SP) global interior array as a stream (Fortran order).
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
   use core_constants_mod, only: SP, N_GHOST
   use core_comm_mod, only: type_comm
   use core_grid_mod, only: type_grid_2d
   use core_accumulators_mod, only: type_accumulator
   use core_interpolation_mod, only: type_interpolator
   use core_time_utils_mod, only: type_timing_control
   use core_field_registry_mod, only: type_field_registry
   use core_output_gatherer_mod, only: type_output_gatherer
   implicit none

   private
   public :: type_output_channel

   integer, parameter :: VARNAME_LEN = 32
   integer, parameter :: STATNAME_LEN = 8
   integer, parameter :: ID_LEN = 64
   integer, parameter :: VARS_MAX = 16
   integer, parameter :: STATS_MAX = 4

   type :: type_output_channel
      character(ID_LEN)              :: id = ''
      character(ID_LEN)              :: geom_type = ''  ! 'field', 'station', 'transect'
      character(8)                   :: format = 'ascii'
      character(VARNAME_LEN)         :: variables(VARS_MAX) = ''
      character(STATNAME_LEN)        :: statistics(STATS_MAX) = ''
      integer                        :: n_vars = 0
      integer                        :: n_stats = 0
      logical                        :: snapshot = .true.
      real(SP)                       :: t_start = 0.0_SP
      real(SP)                       :: interval = 0.0_SP

      ! Output destination and flush counter
      character(:), allocatable :: result_folder
      integer                   :: icount = 0

      ! Timing
      type(type_timing_control) :: trigger

      ! Interpolation (station/transect only)
      type(type_interpolator)   :: interp

      ! Accumulators: one per variable; each holds all requested stats
      type(type_accumulator), allocatable :: accum(:)

      ! MPI gather helper
      type(type_output_gatherer) :: gatherer

      ! Grid geometry (set at init for use in step/flush)
      integer :: local_nx = 0, local_ny = 0
      integer :: n_local = 0   ! local interp points (station/transect)
      integer :: n_global = 0   ! total global output points

   contains
      procedure :: init => channel_init
      procedure :: step => channel_step
      procedure :: finalize => channel_finalize
   end type type_output_channel

contains

   subroutine channel_init(this, id, geom_type, variables, n_vars, &
                           statistics, n_stats, snapshot, t_start, interval, &
                           result_folder, format, &
                           coords_x, coords_y, n_coords, grid, comm)
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

      this%variables(1:n_vars) = variables(1:n_vars)
      this%statistics(1:n_stats) = statistics(1:n_stats)

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
         call this%gatherer%init_field(grid, comm)

         ! Accumulators: (local_nx, local_ny)
         allocate (this%accum(n_vars))
         do iv = 1, n_vars
            call this%accum(iv)%init(grid%local_nx, grid%local_ny, trim(variables(iv)))
            do is = 1, n_stats
               call this%accum(iv)%allocate_stat(trim(statistics(is)))
            end do
         end do

      case default
         error stop 'type_output_channel: unknown geometry type: '//trim(geom_type)
      end select

   end subroutine channel_init

   ! Called every timestep. Accumulates from registry; flushes when triggered.
   subroutine channel_step(this, t, dt, registry, comm)
      class(type_output_channel), intent(inout) :: this
      real(SP), intent(in)    :: t, dt
      type(type_field_registry), intent(in)    :: registry
      type(type_comm), intent(inout) :: comm

      integer  :: iv
      real(SP), pointer :: fld(:, :)
      real(SP), allocatable :: interp_vals(:), interp_2d(:, :)
      logical :: do_flush

      if (t < this%t_start) return

      ! dt-accumulator mode: legacy PLOT_COUNT frame cadence
      do_flush = this%trigger%should_trigger(t, dt)
      if (do_flush) this%icount = this%icount + 1

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
                                     trim(this%variables(iv)), comm)
         end associate
      case ('station', 'transect')
         allocate (local_vals(this%n_local))
         call this%interp%gather(fld, local_vals)
         call channel_flush_points(this, local_vals, &
                                   trim(this%variables(iv)), t, comm)
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
         name = trim(this%variables(iv))//'_'//trim(this%statistics(is))
         select case (trim(this%geom_type))
         case ('field')
            call channel_flush_field(this, stat_vals, name, comm)
         case ('station', 'transect')
            call channel_flush_points(this, stat_vals(:, 1), name, t, comm)
         end select
      end do
   end subroutine channel_write_stats

   ! Gather one field-geometry interior array and write <name>_NNNNN on IO rank.
   subroutine channel_flush_field(this, vals, name, comm)
      class(type_output_channel), intent(inout) :: this
      real(SP), intent(in)    :: vals(:, :)   ! (local_nx, local_ny)
      character(*), intent(in)    :: name
      type(type_comm), intent(inout) :: comm

      real(SP), allocatable :: glob(:, :)
      character(5) :: cnt

      if (comm%is_io_node()) then
         allocate (glob(this%gatherer%M, this%gatherer%N))
      else
         allocate (glob(1, 1))
      end if
      call this%gatherer%gather_field(vals, glob, comm)

      if (comm%is_io_node()) then
         write (cnt, '(I5.5)') this%icount
         call write_field_file(this%result_folder//name//'_'//cnt, &
                               glob, trim(this%format))
      end if
   end subroutine channel_flush_field

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
            open (newunit=unit, file=point_file_name(this, trim(this%variables(iv))), &
                  status='replace', action='write')
            close (unit)
         end if
         do is = 1, this%n_stats
            open (newunit=unit, file=point_file_name(this, &
                                                     trim(this%variables(iv))//'_'//trim(this%statistics(is))), &
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

   subroutine channel_finalize(this)
      class(type_output_channel), intent(inout) :: this
      integer :: iv
      call this%interp%finalize()
      call this%gatherer%finalize()
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
