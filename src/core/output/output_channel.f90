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
   use core_constants_mod,       only: SP, N_GHOST
   use core_comm_mod,            only: type_comm
   use core_grid_mod,            only: type_grid_2d
   use core_accumulators_mod,    only: type_accumulator
   use core_interpolation_mod,   only: type_interpolator
   use core_time_utils_mod,      only: type_timing_control
   use core_field_registry_mod,  only: type_field_registry
   use core_output_gatherer_mod, only: type_output_gatherer
   implicit none(external)

   private
   public :: type_output_channel

   integer, parameter :: VARNAME_LEN  = 32
   integer, parameter :: STATNAME_LEN = 8
   integer, parameter :: ID_LEN       = 64
   integer, parameter :: VARS_MAX     = 16
   integer, parameter :: STATS_MAX    = 4

   type, public :: type_output_channel
      character(ID_LEN)              :: id       = ''
      character(ID_LEN)              :: geom_type = ''  ! 'field', 'station', 'transect'
      character(8)                   :: format   = 'ascii'
      character(VARNAME_LEN)         :: variables(VARS_MAX) = ''
      character(STATNAME_LEN)        :: statistics(STATS_MAX) = ''
      integer                        :: n_vars  = 0
      integer                        :: n_stats = 0
      logical                        :: snapshot = .true.
      real(SP)                       :: t_start  = 0.0_SP
      real(SP)                       :: interval = 0.0_SP

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
      integer :: n_local  = 0   ! local interp points (station/transect)
      integer :: n_global = 0   ! total global output points

   contains
      procedure :: init    => channel_init
      procedure :: step    => channel_step
      procedure :: finalize => channel_finalize
   end type type_output_channel

contains

   subroutine channel_init(this, id, geom_type, variables, n_vars, &
                           statistics, n_stats, snapshot, t_start, interval, &
                           coords_x, coords_y, n_coords, grid, comm)
      class(type_output_channel), intent(inout) :: this
      character(*),    intent(in) :: id, geom_type
      character(*),    intent(in) :: variables(*)
      integer,         intent(in) :: n_vars
      character(*),    intent(in) :: statistics(*)
      integer,         intent(in) :: n_stats
      logical,         intent(in) :: snapshot
      real(SP),        intent(in) :: t_start, interval
      real(SP),        intent(in) :: coords_x(*), coords_y(*)  ! global query coords
      integer,         intent(in) :: n_coords   ! n_stations or n_transect_points (0 for field)
      type(type_grid_2d), intent(in)    :: grid
      type(type_comm),    intent(inout) :: comm

      integer :: iv, is

      this%id        = id
      this%geom_type = geom_type
      this%snapshot  = snapshot
      this%t_start   = t_start
      this%interval  = interval
      this%n_vars    = n_vars
      this%n_stats   = n_stats
      this%local_nx  = grid%local_nx
      this%local_ny  = grid%local_ny

      this%variables(1:n_vars) = variables(1:n_vars)
      this%statistics(1:n_stats) = statistics(1:n_stats)

      ! Timing control
      this%trigger%t_start       = t_start
      this%trigger%interval      = interval
      this%trigger%last_triggered = -1.0_SP

      ! Geometry-specific setup
      select case (trim(geom_type))
      case ('station', 'transect')
         ! Bilinear interpolation from global query coords
         call this%interp%init(coords_x(1:n_coords), coords_y(1:n_coords), grid)
         this%n_local  = this%interp%n_points
         this%n_global = n_coords
         call this%gatherer%init_points(n_coords, this%n_local, comm)

         ! Accumulators: (n_local, 1)
         allocate(this%accum(n_vars))
         do iv = 1, n_vars
            call this%accum(iv)%init(this%n_local, 1, trim(variables(iv)))
            do is = 1, n_stats
               call this%accum(iv)%allocate_stat(trim(statistics(is)))
            end do
         end do

      case ('field')
         this%n_local  = grid%local_nx * grid%local_ny
         this%n_global = grid%M * grid%N

         ! Accumulators: (local_nx, local_ny)
         allocate(this%accum(n_vars))
         do iv = 1, n_vars
            call this%accum(iv)%init(grid%local_nx, grid%local_ny, trim(variables(iv)))
            do is = 1, n_stats
               call this%accum(iv)%allocate_stat(trim(statistics(is)))
            end do
         end do

      case default
         error stop 'type_output_channel: unknown geometry type: ' // trim(geom_type)
      end select

   end subroutine channel_init

   ! Called every timestep. Accumulates from registry; flushes when triggered.
   subroutine channel_step(this, t, dt, registry, comm)
      class(type_output_channel), intent(inout) :: this
      real(SP),                   intent(in)    :: t, dt
      type(type_field_registry),  intent(in)    :: registry
      type(type_comm),            intent(inout) :: comm

      integer  :: iv
      real(SP), pointer :: fld(:,:)
      real(SP), allocatable :: interp_vals(:), interp_2d(:,:)
      logical :: do_flush

      if (t < this%t_start) return

      do_flush = this%trigger%should_trigger(t)

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
            allocate(interp_vals(this%n_local), interp_2d(this%n_local, 1))
            do iv = 1, this%n_vars
               fld => registry%get(trim(this%variables(iv)))
               call this%interp%gather(fld, interp_vals)
               interp_2d(:, 1) = interp_vals
               call this%accum(iv)%accumulate(interp_2d, dt)
            end do
            deallocate(interp_vals, interp_2d)

         case ('field')
            do iv = 1, this%n_vars
               fld => registry%get(trim(this%variables(iv)))
               ! Slice interior (ghost-inclusive field → interior only)
               associate(ng => N_GHOST, nx => this%local_nx, ny => this%local_ny)
                  call this%accum(iv)%accumulate( &
                     fld(ng+1:ng+nx, ng+1:ng+ny), dt)
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
      integer,         intent(in) :: iv
      real(SP), pointer,intent(in) :: fld(:,:)
      real(SP),        intent(in) :: t
      type(type_comm), intent(inout) :: comm
      ! TODO: implement ascii write via output_gatherer
      !   field:           MPI_Gatherv subdomains → global 2D write
      !   station/transect: gatherer_gather_vals → sorted write
   end subroutine channel_write_snapshot

   ! Write one accumulated statistic for variable iv. Host-only I/O on IO rank.
   subroutine channel_write_stats(this, iv, t, comm)
      class(type_output_channel), intent(inout) :: this
      integer,         intent(in)    :: iv
      real(SP),        intent(in)    :: t
      type(type_comm), intent(inout) :: comm

      integer :: is
      real(SP), allocatable :: stat_vals(:,:), global_vals(:)
      ! TODO: implement ascii write
      !   For each stat, call accum%get_stat, then gatherer/write
      do is = 1, this%n_stats
         stat_vals = this%accum(iv)%get_stat(trim(this%statistics(is)))
         ! TODO: channel_write_field or channel_write_points depending on geom_type
      end do
   end subroutine channel_write_stats

   subroutine channel_finalize(this)
      class(type_output_channel), intent(inout) :: this
      integer :: iv
      call this%interp%finalize()
      call this%gatherer%finalize()
      if (allocated(this%accum)) then
         do iv = 1, size(this%accum)
            call this%accum(iv)%finalize()
         end do
         deallocate(this%accum)
      end if
      this%n_vars = 0
      this%n_stats = 0
   end subroutine channel_finalize

end module core_output_channel_mod
