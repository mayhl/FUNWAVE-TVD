!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Diagnostics channel: the modern form of legacy STATISTICS.  One scalar
!  row per sample (t, dt, step, n_wet, metrics), every metric a domain-wide
!  reduction of a field-registry variable or a named derived quantity:
!
!    <var>.max | .min | .sum | .mean                        (any variable)
!    <var>.count           cells where the gate is ACTIVE
!    <var>.fraction        .count / wet cells
!    <var>.area            sum of dx dy over the active cells        [m^2]
!    mass                  sum(eta dx dy)                            [m^3]
!    energy                sum((g h^2 + h |u|^2)/2 dx dy)            [J/rho]
!    speed.max             max |u|                                   [m/s]
!    froude.max            max |u| / sqrt(g max(h, h_frc))
!    flooded_area          wet area above the still-water shoreline  [m^2]
!
!  Every reduction runs over WET interior cells (mask = 1): a dry cell
!  holds the stale bed-level eta and would own any min.  n_wet is a fixed
!  column so a mean or fraction over no wet cells (written 0, never NaN)
!  reads as undefined rather than as a physical zero.  ACTIVE for
!  count/fraction/area: value >= 1/2 (a 0/1 flag), except the two weight
!  gates froude_scale and disp_gate, active below 1.
!
!  Cadence: interval > 0 samples on a clock like a channel; interval = 0
!  samples every N steps with N set at the first step from the screen
!  interval and dt0, so a dt collapse densifies the record on its own.
!  A collapse (dt under a fraction of the running median of the sampled
!  dt) forces a sample.  Warnings fire ONCE each: a guard (froude_scale,
!  nu_capped, disp_gate count/fraction/area) first active beyond its
!  t_start baseline (t, cell, from the rank holding the most), dt
!  collapse, a dt jump above a factor, |u| above a few sqrt(g h_max).  A non-finite metric
!  aborts (legacy CHECK_STATISTICS); an `abort:` threshold aborts above
!  its value.
!
!  Files under the result folder: diagnostics.dat (header + one row per
!  sample; a hot start APPENDS behind a `# restart t_start=<t>` seam line
!  so the pre-restart history survives, a cold start truncates), the
!  sidecar diagnostics.latest (header + the last rows, rewritten through
!  a rename so a reader never sees a partial file) and
!  diagnostics.metadata.yaml (units and long names per column, the
!  channel metadata.yaml shape).  With a netcdf root handle (a
!  netcdf/pnetcdf deck) the same rows also land as the group
!  `diagnostics` in diagnostics.nc: time (unlimited), dt, step, n_wet and
!  one scalar variable per metric; the root is recreated on a hot start
!  like every netcdf output.  Reductions are interior loops + allreduce;
!  no kernel changes.
!
!  HISTORY :
!    09/16/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------
module core_diagnostics_mod
   use mpi_f08
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use, intrinsic :: iso_fortran_env, only: output_unit
   use core_constants_mod, only: SP, MPI_SP, GRAV
   use core_grid_mod, only: type_grid_2d
   use core_field_registry_mod, only: type_field_registry
   use core_log_io_mod, only: type_log_writer, log_line
   use core_path_mod, only: rename_file
   use netcdf
   implicit none
   private
   public :: type_diagnostics, type_diag_metric, parse_metric, NAME_LEN

   integer, parameter :: NAME_LEN = 40
   integer, parameter :: RING_ROWS = 5
   integer, parameter :: DT_HIST = 32
   ! Warning thresholds; parameters, not deck keys, until a case argues
   real(SP), parameter :: COLLAPSE_RATIO = 0.1_SP   ! dt under this x median
   real(SP), parameter :: JUMP_FACTOR = 10.0_SP     ! dt over this x previous
   real(SP), parameter :: SPEED_FACTOR = 3.0_SP     ! |u| over this x sqrt(g h_max)

   ! reducer kinds
   integer, parameter, public :: RED_MAX = 1, RED_MIN = 2, RED_SUM = 3, RED_MEAN = 4, &
                                 RED_COUNT = 5, RED_FRACTION = 6, RED_AREA = 7, &
                                 RED_MASS = 8, RED_ENERGY = 9, RED_SPEED_MAX = 10, &
                                 RED_FROUDE_MAX = 11, RED_FLOODED_AREA = 12

   type :: type_diag_metric
      character(NAME_LEN) :: name = ''   ! as written in the deck
      character(NAME_LEN) :: var = ''    ! registry variable, '' for derived
      integer :: kind = 0
      ! CF units of the value; max/min/sum/mean carry the variable's own,
      ! filled by the caller from the field catalog (not visible here)
      character(64) :: units = ''
      logical :: below_one = .false.     ! active test for the weight gates
      real(SP), pointer :: data(:, :) => null()
      real(SP) :: abort_above = huge(1.0_SP)
      logical :: has_abort = .false.
      logical :: guard = .false.         ! a stability guard: first-active warning
      logical :: warned = .false.        ! first-active latch
      real(SP) :: baseline = -1.0_SP     ! count at the first sample (< 0 = unset)
      real(SP) :: value = 0.0_SP
   end type type_diag_metric

   type :: type_diagnostics
      logical :: is_activated = .false.
      type(type_diag_metric), allocatable :: metrics(:)
      integer :: n = 0
      real(SP) :: interval = 0.0_SP
      real(SP) :: screen_interval = 1.0_SP
      real(SP) :: t_next = 0.0_SP        ! clock cadence, set from the first t
      logical :: started = .false.
      integer :: stride = 0              ! step stride, 0 until dt0 is known
      integer :: n_steps = 0             ! completed steps seen
      real(SP) :: h_frc = 0.0_SP         ! numerics MinDepthFrc
      ! dt history at the samples + per-step watch
      real(SP) :: dt_hist(DT_HIST) = 0.0_SP
      integer :: n_hist = 0
      real(SP) :: dt_prev = 0.0_SP
      real(SP) :: n_wet = 0.0_SP         ! wet cells at the last sample
      logical :: warned_collapse = .false., warned_jump = .false., warned_speed = .false.
      ! plumbing
      type(type_grid_2d), pointer :: grid => null()
      type(type_field_registry), pointer :: registry => null()
      type(type_log_writer), pointer :: log => null()
      real(SP), pointer :: eta(:, :) => null(), u(:, :) => null(), v(:, :) => null()
      real(SP), pointer :: h(:, :) => null(), depth(:, :) => null(), mask(:, :) => null()
      ! files (IO rank only)
      logical :: is_io = .false.
      logical :: write_files = .false.
      character(:), allocatable :: folder
      integer :: unit = -1
      logical :: seam_pending = .false.  ! hot start: seam line before the first row
      real(SP), allocatable :: ring(:, :)   ! (4 + n, RING_ROWS), newest last
      integer :: n_ring = 0
      ! netcdf group (IO rank only; -1 = none)
      integer :: grpid = -1, time_varid = -1, dt_varid = -1, step_varid = -1, nwet_varid = -1
      integer, allocatable :: varids(:)
      integer :: nrec = 0
   contains
      procedure :: init => diag_init
      procedure :: step => diag_step
      procedure :: finalize => diag_finalize
   end type type_diagnostics

contains

   ! Parse "<var>.<reducer>" or a derived name; ok = .false. on an unknown
   ! form (the caller names the deck key in its error)
   subroutine parse_metric(name, m, ok)
      character(*), intent(in) :: name
      type(type_diag_metric), intent(out) :: m
      logical, intent(out) :: ok
      integer :: idot

      ok = .true.
      m%name = name
      select case (trim(name))
      case ("mass"); m%kind = RED_MASS; m%units = "m3"
      case ("energy"); m%kind = RED_ENERGY; m%units = "m5 s-2"
      case ("speed.max"); m%kind = RED_SPEED_MAX; m%units = "m s-1"
      case ("froude.max"); m%kind = RED_FROUDE_MAX; m%units = "1"
      case ("flooded_area"); m%kind = RED_FLOODED_AREA; m%units = "m2"
      case default
         idot = index(name, ".", back=.true.)
         if (idot <= 1 .or. idot == len_trim(name)) then
            ok = .false.
            return
         end if
         m%var = name(1:idot - 1)
         select case (name(idot + 1:len_trim(name)))
         case ("max"); m%kind = RED_MAX
         case ("min"); m%kind = RED_MIN
         case ("sum"); m%kind = RED_SUM
         case ("mean"); m%kind = RED_MEAN
         case ("count"); m%kind = RED_COUNT; m%units = "1"
         case ("fraction"); m%kind = RED_FRACTION; m%units = "1"
         case ("area"); m%kind = RED_AREA; m%units = "m2"
         case default; ok = .false.
         end select
         m%below_one = trim(m%var) == "froude_scale" .or. trim(m%var) == "disp_gate"
         ! the guards whose first engagement is worth one log line; the
         ! dispersion gate holds a static shoreline taper at rest, so every
         ! guard warns when its count first exceeds the t_start baseline
         m%guard = (m%kind == RED_COUNT .or. m%kind == RED_FRACTION .or. m%kind == RED_AREA) &
                   .and. (m%below_one .or. trim(m%var) == "nu_capped")
      end select
   end subroutine parse_metric

   ! folder absent = no files (unit tests); every registry name in the
   ! metrics must already be registered (the caller raised the needs)
   ! diag_ncid = the shared diagnostics.nc root (IO rank), absent = ascii only
   ! restart = a hot start: append to an existing table behind a seam line
   subroutine diag_init(this, metrics, interval, screen_interval, h_frc, grid, registry, &
                        log, is_io, folder, diag_ncid, restart)
      class(type_diagnostics), intent(inout) :: this
      type(type_diag_metric), intent(in) :: metrics(:)
      real(SP), intent(in) :: interval, screen_interval, h_frc
      type(type_grid_2d), intent(in), target :: grid
      type(type_field_registry), intent(in), target :: registry
      type(type_log_writer), intent(in), target :: log
      logical, intent(in) :: is_io
      character(*), intent(in), optional :: folder
      integer, intent(in), optional :: diag_ncid
      logical, intent(in), optional :: restart

      integer :: k
      logical :: appending

      this%n = size(metrics)
      allocate (this%metrics(this%n))
      this%metrics = metrics
      this%interval = interval
      this%screen_interval = screen_interval
      this%h_frc = h_frc
      this%grid => grid
      this%registry => registry
      this%log => log
      this%is_io = is_io
      this%is_activated = .true.

      this%eta => registry%get("eta")
      this%u => registry%get("u")
      this%v => registry%get("v")
      this%h => registry%get("h")
      this%depth => registry%get("depth")
      this%mask => registry%get("mask")
      do k = 1, this%n
         if (len_trim(this%metrics(k)%var) > 0) &
            this%metrics(k)%data => registry%get(trim(this%metrics(k)%var))
      end do

      allocate (this%ring(4 + this%n, RING_ROWS))
      this%ring = 0.0_SP
      this%write_files = present(folder) .and. is_io
      if (this%write_files) then
         this%folder = folder
         if (present(restart)) this%seam_pending = restart
         appending = .false.
         if (this%seam_pending) inquire (file=this%folder//"diagnostics.dat", exist=appending)
         if (appending) then
            ! the pre-restart rows are the history a restart is diagnosed from
            open (newunit=this%unit, file=this%folder//"diagnostics.dat", &
                  status="old", position="append", action="write")
         else
            open (newunit=this%unit, file=this%folder//"diagnostics.dat", &
                  status="replace", action="write")
            write (this%unit, "(a)") trim(header_line(this))
         end if
         call write_metadata(this)
      end if
      if (present(diag_ncid) .and. is_io) call create_group(this, diag_ncid)
   end subroutine diag_init

   subroutine create_group(this, root)
      class(type_diagnostics), intent(inout) :: this
      integer, intent(in) :: root
      integer :: t_dim, k
      character(NAME_LEN) :: vname

      call nc_check(nf90_def_grp(root, "diagnostics", this%grpid), "def group diagnostics")
      call nc_check(nf90_def_dim(this%grpid, "time", NF90_UNLIMITED, t_dim), "def time")
      call nc_check(nf90_def_var(this%grpid, "time", NF90_DOUBLE, [t_dim], this%time_varid), &
                    "def var time")
      call nc_check(nf90_put_att(this%grpid, this%time_varid, "units", "seconds since start"), &
                    "att time units")
      call nc_check(nf90_def_var(this%grpid, "dt", NF90_DOUBLE, [t_dim], this%dt_varid), &
                    "def var dt")
      call nc_check(nf90_put_att(this%grpid, this%dt_varid, "units", "s"), "att dt units")
      call nc_check(nf90_put_att(this%grpid, this%dt_varid, "long_name", &
                                 "time step just completed"), "att dt long_name")
      call nc_check(nf90_def_var(this%grpid, "step", NF90_INT, [t_dim], this%step_varid), &
                    "def var step")
      call nc_check(nf90_put_att(this%grpid, this%step_varid, "long_name", &
                                 "completed steps"), "att step long_name")
      call nc_check(nf90_def_var(this%grpid, "n_wet", NF90_INT, [t_dim], this%nwet_varid), &
                    "def var n_wet")
      call nc_check(nf90_put_att(this%grpid, this%nwet_varid, "long_name", &
                                 "wet cells in the reductions"), "att n_wet long_name")
      allocate (this%varids(this%n))
      this%varids = -1
      do k = 1, this%n
         ! variable names keep the deck spelling with the dot as an underscore
         vname = this%metrics(k)%name
         call replace_dots(vname)
         call nc_check(nf90_def_var(this%grpid, trim(vname), NF90_DOUBLE, [t_dim], &
                                    this%varids(k)), "def var "//trim(vname))
         call nc_check(nf90_put_att(this%grpid, this%varids(k), "long_name", &
                                    trim(this%metrics(k)%name)//" over wet cells"), &
                       "att long_name "//trim(vname))
         call nc_check(nf90_put_att(this%grpid, this%varids(k), "cell_methods", &
                                    "area: "//trim(reducer_name(this%metrics(k)%kind))), &
                       "att cell_methods "//trim(vname))
         if (len_trim(this%metrics(k)%units) > 0) &
            call nc_check(nf90_put_att(this%grpid, this%varids(k), "units", &
                                       trim(this%metrics(k)%units)), "att units "//trim(vname))
      end do
      ! NETCDF4 root: define/data mode switches on its own at the first put
   end subroutine create_group

   subroutine replace_dots(name)
      character(*), intent(inout) :: name
      integer :: i
      do i = 1, len_trim(name)
         if (name(i:i) == ".") name(i:i) = "_"
      end do
   end subroutine replace_dots

   function reducer_name(kind) result(name)
      integer, intent(in) :: kind
      character(:), allocatable :: name
      select case (kind)
      case (RED_MAX, RED_SPEED_MAX, RED_FROUDE_MAX); name = "maximum"
      case (RED_MIN); name = "minimum"
      case (RED_SUM, RED_MASS, RED_ENERGY, RED_AREA, RED_FLOODED_AREA); name = "sum"
      case (RED_MEAN); name = "mean"
      case (RED_COUNT, RED_FRACTION); name = "point"
      case default; name = "point"
      end select
   end function reducer_name

   subroutine nc_check(status, what)
      integer, intent(in) :: status
      character(*), intent(in) :: what
      if (status /= NF90_NOERR) then
         call log_line("diagnostics netcdf: "//what//": "//trim(nf90_strerror(status)), level="ERROR")
         error stop "diagnostics: netcdf failure"
      end if
   end subroutine nc_check

   subroutine put_record(this, t, dt)
      class(type_diagnostics), intent(inout) :: this
      real(SP), intent(in) :: t, dt
      integer :: k
      this%nrec = this%nrec + 1
      call nc_check(nf90_put_var(this%grpid, this%time_varid, t, start=[this%nrec]), "put time")
      call nc_check(nf90_put_var(this%grpid, this%dt_varid, dt, start=[this%nrec]), "put dt")
      call nc_check(nf90_put_var(this%grpid, this%step_varid, this%n_steps, start=[this%nrec]), &
                    "put step")
      call nc_check(nf90_put_var(this%grpid, this%nwet_varid, nint(this%n_wet), start=[this%nrec]), &
                    "put n_wet")
      do k = 1, this%n
         if (this%varids(k) < 0) cycle
         call nc_check(nf90_put_var(this%grpid, this%varids(k), this%metrics(k)%value, &
                                    start=[this%nrec]), "put "//trim(this%metrics(k)%name))
      end do
      call nc_check(nf90_sync(this%grpid), "sync diagnostics")
   end subroutine put_record

   function header_line(this) result(line)
      class(type_diagnostics), intent(in) :: this
      character(len=32 + 17*(4 + this%n)) :: line
      integer :: k
      line = "#"//repeat(" ", 15)//"t"//repeat(" ", 15)//"dt"//repeat(" ", 6)//"step"// &
             repeat(" ", 5)//"n_wet"
      do k = 1, this%n
         line = trim(line)//repeat(" ", max(1, 17 - len_trim(this%metrics(k)%name)))// &
                trim(this%metrics(k)%name)
      end do
   end function header_line

   ! Loop-top call, once per step (dt = the step just completed, 0 at the
   ! initial call); force = the final flush
   subroutine diag_step(this, t, dt, force)
      class(type_diagnostics), intent(inout) :: this
      real(SP), intent(in) :: t, dt
      logical, intent(in), optional :: force

      logical :: due, forced, collapse
      real(SP) :: median

      forced = .false.
      if (present(force)) forced = force
      if (dt > 0.0_SP) this%n_steps = this%n_steps + 1

      ! per-step dt watch: collapse against the sampled median, jump
      ! against the previous step
      collapse = .false.
      if (dt > 0.0_SP .and. this%n_hist >= 4) then
         median = median_dt(this)
         if (dt < COLLAPSE_RATIO*median) then
            collapse = .true.
            if (.not. this%warned_collapse) then
               call warn(this, "dt collapse", t, dt, median)
               this%warned_collapse = .true.
            end if
         end if
      end if
      if (dt > 0.0_SP .and. this%dt_prev > 0.0_SP .and. dt > JUMP_FACTOR*this%dt_prev &
          .and. .not. this%warned_jump) then
         call warn(this, "dt jump", t, dt, this%dt_prev)
         this%warned_jump = .true.
      end if
      if (dt > 0.0_SP) this%dt_prev = dt

      ! cadence; the clock starts at the first t so a hot start does not
      ! replay every missed slot from zero
      if (.not. this%started) this%t_next = t
      this%started = .true.
      if (this%interval > 0.0_SP) then
         due = t >= this%t_next
         if (due) this%t_next = this%t_next + this%interval
      else
         if (this%stride == 0 .and. dt > 0.0_SP) &
            this%stride = max(1, nint(this%screen_interval/dt))
         due = this%stride == 0 .or. mod(this%n_steps, max(this%stride, 1)) == 0
      end if
      if (.not. (due .or. forced .or. collapse)) return

      call sample(this, t, dt)
   end subroutine diag_step

   subroutine sample(this, t, dt)
      class(type_diagnostics), intent(inout) :: this
      real(SP), intent(in) :: t, dt

      real(SP), allocatable :: sums(:), maxs(:), mins(:), counts(:)
      real(SP) :: wet_count, speed_max, h_max, spd, fr, a
      integer :: i, j, k, ierr, ib, ie, jb, je
      logical :: active

      ib = this%grid%lp%ib; ie = this%grid%lp%ie
      jb = this%grid%lp%jb; je = this%grid%lp%je
      allocate (sums(this%n), maxs(this%n), mins(this%n), counts(this%n))
      sums = 0.0_SP; maxs = -huge(1.0_SP); mins = huge(1.0_SP); counts = 0.0_SP
      wet_count = 0.0_SP; speed_max = 0.0_SP; h_max = 0.0_SP

      do j = jb, je
         do i = ib, ie
            if (this%mask(i, j) < 0.5_SP) cycle
            ! grid spacing is interior-indexed (no ghosts)
            a = this%grid%dx(i - ib + 1, j - jb + 1)*this%grid%dy(i - ib + 1, j - jb + 1)
            wet_count = wet_count + 1.0_SP
            spd = sqrt(this%u(i, j)**2 + this%v(i, j)**2)
            speed_max = max(speed_max, spd)
            h_max = max(h_max, this%h(i, j))
            do k = 1, this%n
               associate (m => this%metrics(k))
                  select case (m%kind)
                  case (RED_MAX); maxs(k) = max(maxs(k), m%data(i, j))
                  case (RED_MIN); mins(k) = min(mins(k), m%data(i, j))
                  case (RED_SUM, RED_MEAN); sums(k) = sums(k) + m%data(i, j)
                  case (RED_COUNT, RED_FRACTION, RED_AREA)
                     if (m%below_one) then
                        active = m%data(i, j) < 1.0_SP
                     else
                        active = m%data(i, j) >= 0.5_SP
                     end if
                     if (active) then
                        counts(k) = counts(k) + 1.0_SP
                        sums(k) = sums(k) + a
                     end if
                  case (RED_MASS); sums(k) = sums(k) + this%eta(i, j)*a
                  case (RED_ENERGY)
                     sums(k) = sums(k) + 0.5_SP*a*(GRAV*this%h(i, j)**2 + &
                                                   this%h(i, j)*(this%u(i, j)**2 + this%v(i, j)**2))
                  case (RED_SPEED_MAX); maxs(k) = max(maxs(k), spd)
                  case (RED_FROUDE_MAX)
                     fr = spd/sqrt(GRAV*max(this%h(i, j), this%h_frc))
                     maxs(k) = max(maxs(k), fr)
                  case (RED_FLOODED_AREA)
                     if (this%depth(i, j) < 0.0_SP) sums(k) = sums(k) + a
                  end select
               end associate
            end do
         end do
      end do

      call MPI_Allreduce(MPI_IN_PLACE, sums, this%n, MPI_SP, MPI_SUM, this%grid%cart_comm, ierr)
      call MPI_Allreduce(MPI_IN_PLACE, maxs, this%n, MPI_SP, MPI_MAX, this%grid%cart_comm, ierr)
      call MPI_Allreduce(MPI_IN_PLACE, mins, this%n, MPI_SP, MPI_MIN, this%grid%cart_comm, ierr)
      call MPI_Allreduce(MPI_IN_PLACE, wet_count, 1, MPI_SP, MPI_SUM, this%grid%cart_comm, ierr)
      call MPI_Allreduce(MPI_IN_PLACE, speed_max, 1, MPI_SP, MPI_MAX, this%grid%cart_comm, ierr)
      call MPI_Allreduce(MPI_IN_PLACE, h_max, 1, MPI_SP, MPI_MAX, this%grid%cart_comm, ierr)
      this%n_wet = wet_count

      do k = 1, this%n
         associate (m => this%metrics(k))
            select case (m%kind)
            case (RED_MAX, RED_SPEED_MAX, RED_FROUDE_MAX); m%value = maxs(k)
            case (RED_MIN); m%value = mins(k)
            case (RED_SUM, RED_MASS, RED_ENERGY, RED_FLOODED_AREA, RED_AREA); m%value = sums(k)
            case (RED_MEAN); m%value = sums(k)/max(wet_count, 1.0_SP)
            case (RED_COUNT); m%value = sum_counts(this, counts(k))
            case (RED_FRACTION); m%value = sum_counts(this, counts(k))/max(wet_count, 1.0_SP)
            end select
            ! first engagement of a guard beyond its baseline, from the rank
            ! holding the most active cells
            if (m%guard) then
               if (m%baseline < 0.0_SP) then
                  m%baseline = m%value
               else if (.not. m%warned .and. m%value > m%baseline) then
                  call first_active_site(this, k, counts(k), t)
                  m%warned = .true.
               end if
            end if
         end associate
      end do

      ! speed plausibility, once
      if (.not. this%warned_speed .and. h_max > 0.0_SP .and. &
          speed_max > SPEED_FACTOR*sqrt(GRAV*h_max)) then
         call warn(this, "speed", t, speed_max, SPEED_FACTOR*sqrt(GRAV*h_max))
         this%warned_speed = .true.
      end if

      call push_dt(this, dt)
      call write_row(this, t, dt)
      call check_abort(this, t)
   end subroutine sample

   ! counts are summed separately per call because the area ride in sums
   real(SP) function sum_counts(this, local) result(total)
      class(type_diagnostics), intent(in) :: this
      real(SP), intent(in) :: local
      integer :: ierr
      total = local
      call MPI_Allreduce(MPI_IN_PLACE, total, 1, MPI_SP, MPI_SUM, this%grid%cart_comm, ierr)
   end function sum_counts

   ! The rank with the most active cells names its first one (global
   ! cell, x, y); direct write + flush like the blow-up site, since the
   ! logger drops non-IO ranks
   subroutine first_active_site(this, k, local_count, t)
      class(type_diagnostics), intent(inout) :: this
      integer, intent(in) :: k
      real(SP), intent(in) :: local_count, t

      real(SP) :: pair(2)
      integer :: ierr, i, j, rank
      character(200) :: msg
      logical :: active

      call MPI_Comm_rank(this%grid%cart_comm, rank, ierr)
      pair = [local_count, real(rank, SP)]
      call MPI_Allreduce(MPI_IN_PLACE, pair, 1, MPI_2DOUBLE_PRECISION, MPI_MAXLOC, &
                         this%grid%cart_comm, ierr)
      if (nint(pair(2)) /= rank) return

      associate (m => this%metrics(k), lp => this%grid%lp, g => this%grid)
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               if (this%mask(i, j) < 0.5_SP) cycle
               if (m%below_one) then
                  active = m%data(i, j) < 1.0_SP
               else
                  active = m%data(i, j) >= 0.5_SP
               end if
               if (active) then
                  write (msg, '(a,a,a,es12.5,a,i0,a,i0,a,f0.1,a,f0.1,a,i0,a)') &
                     "diagnostics: ", trim(m%var), " first active beyond its start baseline at t = ", t, &
                     ", global cell (", g%ibegin + i - lp%ib + 1, ", ", &
                     g%jbegin + j - lp%jb + 1, "), x = ", &
                     (real(g%ibegin + i - lp%ib, SP) + 0.5_SP)*g%dx0, " y = ", &
                     (real(g%jbegin + j - lp%jb, SP) + 0.5_SP)*g%dy0, &
                     " (", nint(local_count), " cells on this rank)"
                  call log_line(trim(msg), level="WARN")
                  flush (output_unit)
                  return
               end if
            end do
         end do
      end associate
   end subroutine first_active_site

   subroutine warn(this, what, t, value, ref)
      class(type_diagnostics), intent(in) :: this
      character(*), intent(in) :: what
      real(SP), intent(in) :: t, value, ref
      character(160) :: msg
      select case (what)
      case ("dt collapse")
         write (msg, '(a,es12.5,a,es10.3,a,es10.3,a)') "diagnostics: dt collapse at t = ", t, &
            ": dt = ", value, " under 0.1 x the sampled median ", ref, " s"
      case ("dt jump")
         write (msg, '(a,es12.5,a,es10.3,a,es10.3,a)') "diagnostics: dt jump at t = ", t, &
            ": dt = ", value, " from ", ref, " s (speeds lost?)"
      case ("speed")
         write (msg, '(a,es12.5,a,f0.2,a,f0.2,a)') "diagnostics: implausible speed at t = ", t, &
            ": max |u| = ", value, " m/s over 3 sqrt(g h_max) = ", ref, " m/s"
      end select
      call this%log%warning(trim(msg))
   end subroutine warn

   subroutine push_dt(this, dt)
      class(type_diagnostics), intent(inout) :: this
      real(SP), intent(in) :: dt
      if (dt <= 0.0_SP) return
      if (this%n_hist < DT_HIST) then
         this%n_hist = this%n_hist + 1
         this%dt_hist(this%n_hist) = dt
      else
         this%dt_hist(1:DT_HIST - 1) = this%dt_hist(2:DT_HIST)
         this%dt_hist(DT_HIST) = dt
      end if
   end subroutine push_dt

   real(SP) function median_dt(this) result(med)
      class(type_diagnostics), intent(in) :: this
      real(SP) :: s(DT_HIST), tmp
      integer :: i, j, n
      n = this%n_hist
      s(1:n) = this%dt_hist(1:n)
      do i = 2, n            ! insertion sort, n <= 32
         tmp = s(i)
         j = i - 1
         do while (j >= 1)
            if (s(j) <= tmp) exit
            s(j + 1) = s(j)
            j = j - 1
         end do
         s(j + 1) = tmp
      end do
      if (mod(n, 2) == 1) then
         med = s((n + 1)/2)
      else
         med = 0.5_SP*(s(n/2) + s(n/2 + 1))
      end if
   end function median_dt

   subroutine write_row(this, t, dt)
      class(type_diagnostics), intent(inout) :: this
      real(SP), intent(in) :: t, dt
      character(len=64 + (NAME_LEN + 16)*this%n) :: line
      character(NAME_LEN + 16) :: item
      integer :: k, r, unit

      ! screen-log line at every sample (the sample cadence IS the screen
      ! cadence by construction)
      write (line, '(a,es11.4,a,es9.2)') "diag t = ", t, "  dt = ", dt
      do k = 1, this%n
         write (item, '(a,a,es10.3)') "  ", trim(this%metrics(k)%name)//" = ", this%metrics(k)%value
         line = trim(line)//trim(item)
      end do
      call this%log%info(trim(line))

      ! ring of the last rows (every rank keeps it; only IO writes)
      if (this%n_ring < RING_ROWS) then
         this%n_ring = this%n_ring + 1
      else
         this%ring(:, 1:RING_ROWS - 1) = this%ring(:, 2:RING_ROWS)
      end if
      this%ring(1, this%n_ring) = t
      this%ring(2, this%n_ring) = dt
      this%ring(3, this%n_ring) = real(this%n_steps, SP)
      this%ring(4, this%n_ring) = this%n_wet
      this%ring(5:, this%n_ring) = this%metrics(:)%value

      if (this%grpid >= 0) call put_record(this, t, dt)
      if (.not. this%write_files) return
      if (this%seam_pending) then
         write (this%unit, '(a,es17.8)') "# restart t_start=", t
         this%seam_pending = .false.
      end if
      call put_row(this%unit, this%ring(:, this%n_ring), this%n)
      flush (this%unit)
      ! sidecar: whole file rewritten, then renamed into place
      open (newunit=unit, file=this%folder//"diagnostics.latest.tmp", &
            status="replace", action="write")
      write (unit, "(a)") trim(header_line(this))
      do r = 1, this%n_ring
         call put_row(unit, this%ring(:, r), this%n)
      end do
      close (unit)
      if (.not. rename_file(this%folder//"diagnostics.latest.tmp", &
                            this%folder//"diagnostics.latest")) &
         call this%log%warning("diagnostics: could not replace diagnostics.latest")
   end subroutine write_row

   subroutine put_row(unit, row, n)
      integer, intent(in) :: unit, n
      real(SP), intent(in) :: row(:)
      write (unit, '(2es17.8,2i10,*(es17.8))') row(1), row(2), nint(row(3)), nint(row(4)), &
         row(5:4 + n)
   end subroutine put_row

   ! diagnostics.metadata.yaml: units and long names per column in the
   ! channel metadata.yaml shape, so a reader takes units from the file
   ! rather than from a table of its own; flat enough to write by hand
   subroutine write_metadata(this)
      class(type_diagnostics), intent(in) :: this
      integer :: unit, k
      open (newunit=unit, file=this%folder//"diagnostics.metadata.yaml", &
            status="replace", action="write")
      write (unit, "(a)") "# columns of diagnostics.dat and diagnostics.latest, in order"
      write (unit, "(a)") "Conventions: 'CF-1.8'"
      write (unit, "(a)") "source: 'FUNWAVE-TVD'"
      write (unit, "(a)") "restart: 'a hot start appends behind a # restart t_start=<t> line'"
      write (unit, "(a)") "undefined: 'a mean or fraction with n_wet = 0 is written as 0'"
      write (unit, "(a)") "variables:"
      write (unit, "(a)") "  t: {units: 's', long_name: 'seconds since start'}"
      write (unit, "(a)") "  dt: {units: 's', long_name: 'time step just completed'}"
      write (unit, "(a)") "  step: {units: '1', long_name: 'completed steps'}"
      write (unit, "(a)") "  n_wet: {units: '1', long_name: 'wet cells in the reductions'}"
      do k = 1, this%n
         write (unit, "(a)") "  "//trim(this%metrics(k)%name)//": {units: '"// &
            trim(this%metrics(k)%units)//"', long_name: '"//trim(this%metrics(k)%name)// &
            " over wet cells', cell_methods: 'area: "// &
            trim(reducer_name(this%metrics(k)%kind))//"'}"
      end do
      close (unit)
   end subroutine write_metadata

   subroutine check_abort(this, t)
      class(type_diagnostics), intent(in) :: this
      real(SP), intent(in) :: t
      character(200) :: msg
      integer :: k
      do k = 1, this%n
         associate (m => this%metrics(k))
            if (.not. ieee_is_finite(m%value)) then
               write (msg, '(a,a,a,es12.5)') "diagnostics: ", trim(m%name), &
                  " is not finite at t = ", t
               call this%log%exit_on_error(trim(msg))
            end if
            if (m%has_abort .and. m%value > m%abort_above) then
               write (msg, '(a,a,a,es12.5,a,es12.5,a,es12.5)') "diagnostics: ", trim(m%name), &
                  " = ", m%value, " exceeds abort ", m%abort_above, " at t = ", t
               call this%log%exit_on_error(trim(msg))
            end if
         end associate
      end do
   end subroutine check_abort

   subroutine diag_finalize(this)
      class(type_diagnostics), intent(inout) :: this
      if (this%write_files .and. this%unit >= 0) close (this%unit)
      this%unit = -1
      if (allocated(this%metrics)) deallocate (this%metrics)
      if (allocated(this%ring)) deallocate (this%ring)
      if (allocated(this%varids)) deallocate (this%varids)
      this%grpid = -1   ! the root file is closed by the output manager
      this%is_activated = .false.
   end subroutine diag_finalize

end module core_diagnostics_mod
