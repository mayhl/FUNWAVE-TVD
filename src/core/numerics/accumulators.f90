!> @file accumulators.f90
!> @brief Time-weighted statistical accumulators for field quantities.

!> Accumulates field statistics over a simulation time window using
!! time-step weights \f$\Delta t_n\f$.
!!
!! Supported statistics are time-weighted mean, root-mean-square (RMS),
!! point-wise minimum, and point-wise maximum.  Multiple statistics can
!! be active simultaneously on the same accumulator by calling
!! `allocate_stat` once per statistic before the accumulation loop.
!!
!! ### Time-weighted formulas
!!
!! Let \f$f^{(n)}\f$ denote the field at time step \f$n\f$ with time
!! increment \f$\Delta t_n\f$.  Define the total accumulated time
!! \f$T = \sum_n \Delta t_n\f$.
!!
!! **Mean**
!! \f[
!!   \bar{f} = \frac{1}{T} \sum_n f^{(n)} \Delta t_n
!! \f]
!!
!! **RMS**
!! \f[
!!   f_{\mathrm{rms}} = \sqrt{\frac{1}{T} \sum_n \bigl(f^{(n)}\bigr)^2 \Delta t_n}
!! \f]
!!
!! **Min / Max** — point-wise extrema over all accumulated steps,
!! unweighted.
!!
!! **Std** — time-weighted standard deviation about the window mean,
!! \f[
!!   f_{\mathrm{std}} = \sqrt{\frac{1}{T} \sum_n \bigl(f^{(n)}\bigr)^2 \Delta t_n
!!                      - \bar{f}^2}
!! \f]
!! computed with SHIFTED moments (the first accumulated sample is
!! subtracted from every term) so the \f$E[f^2]-E[f]^2\f$ cancellation
!! stays benign in single precision even when \f$|\bar f| \gg
!! f_{\mathrm{std}}\f$.  The shift changes neither the variance nor the
!! reconstructed mean/RMS; shifted accumulation engages only when "std"
!! is requested, so channels without it keep the historical bit pattern.
!!
!! **Extremes with time** — `max_time` rides `max`: the sample time at
!! which the running maximum was last raised.  With a wet mask the
!! extremes sample wet cells only (a dry cell's surface is its bed).
!!
!! **Events** — a per-cell state machine on a threshold condition
!! (`set_threshold`: value and direction, `above`, `below` or `abs` =
!! |sample| above), tested on wet samples only when a wet mask is given
!! (`wet_event` when the event test carries its own, stricter mask).  An event opens at the
!! first sample meeting the condition and closes at the first sample
!! failing it; it is COMMITTED at close (never while open), so a window
!! flush never sees a partial event and a discarded one leaves no trace:
!! `count` (events closed), `first_time` and `last_time` (onset of the
!! first and last committed event), `duration` (sum of the time meeting
!! the condition) and `duration_max`.  Filters, off at zero: `gap`
!! re-opens the same event when the condition returns within that many
!! seconds (the gap itself is not counted as duration); `min_duration`
!! discards a shorter event at close.  `reset` clears only the committed
!! values, so an event straddling a window boundary lands whole in the
!! window it closes in and adjacent windows sum exactly.  Time-valued
!! results, and the extremes of a cell never sampled, hold FILL_VALUE;
!! counts and
!! durations hold 0.
!!
!! **Event log** — `enable_log(cap)` records every committed event as a
!! row (onset, end, duration, peak, cell) in a per-accumulator buffer
!! the owner drains (`n_events`, the `ev_*` arrays, `clear_events`);
!! `peak` is the SIGNED sample furthest past the threshold (a magnitude
!! threshold selects on |value|).  Rows beyond `cap` between drains are
!! dropped and counted in `n_dropped`; `open_events` lists the events
!! still open for the end-of-leg record.
module core_accumulators_mod
   use core_constants_mod, only: SP, FILL_VALUE
   implicit none

   !> Never-triggered marker of the time-valued statistics: the one output
   !! fill (core_constants_mod), re-exported for the tests
   public :: FILL_VALUE
   integer, parameter, public :: THR_NONE = 0, THR_ABOVE = 1, THR_BELOW = -1, THR_ABS = 2

   !> Accumulates time-weighted statistics for a 2-D field array.
   !!
   !! Call `init` to set dimensions and the quantity label, then
   !! `allocate_stat` for each required statistic, then `accumulate`
   !! inside the time-stepping loop, and finally `get_stat` to retrieve
   !! results.  Call `reset` to restart without reallocation; `finalize`
   !! to free all memory.
   type, public :: type_accumulator
      !> Human-readable label for the accumulated quantity (e.g. `"eta"`).
      character(32) :: quantity
      !> First spatial dimension of the field arrays.
      integer  :: dim1 = 0
      !> Second spatial dimension of the field arrays.
      integer  :: dim2 = 0
      !> Total accumulated time \f$T = \sum_n \Delta t_n\f$,
      !! used as the normalisation denominator for mean and RMS.
      real(SP) :: total_dt = 0.0_SP
      !> Running sum \f$\sum_n f^{(n)} \Delta t_n\f$ (allocated for "mean" and "rms").
      real(SP), allocatable :: val_sum(:, :)
      !> Running sum of squares \f$\sum_n (f^{(n)})^2 \Delta t_n\f$ (allocated for "rms").
      real(SP), allocatable :: val_sum_sq(:, :)
      !> Point-wise maximum over all accumulated steps (allocated for "max").
      real(SP), allocatable :: val_max(:, :)
      !> Point-wise minimum over all accumulated steps (allocated for "min").
      real(SP), allocatable :: val_min(:, :)
      !> Shifted accumulation engaged ("std" requested): sums accumulate
      !! \f$f - f_{\mathrm{shift}}\f$; mean/RMS reconstruct through the shift.
      logical  :: shifted = .false.
      !> Shift captured for the current window (first sample after reset).
      logical  :: have_shift = .false.
      !> Per-cell shift \f$f_{\mathrm{shift}}\f$ (allocated for "std").
      real(SP), allocatable :: shift(:, :)
      !> Sample time of the running maximum (allocated for "max_time").
      real(SP), allocatable :: t_max(:, :)
      !> Event threshold value and direction (THR_ABOVE / THR_BELOW).
      real(SP) :: thr = 0.0_SP
      integer  :: thr_dir = THR_NONE
      !> Event filters in seconds; 0 = off.
      real(SP) :: gap = 0.0_SP
      real(SP) :: min_duration = 0.0_SP
      !> Open-event state (allocated by any event statistic; survives reset)
      logical, allocatable :: in_event(:, :)   !< condition met at the last sample
      logical, allocatable :: closing(:, :)    !< condition lost, gap not yet elapsed
      real(SP), allocatable :: t_on(:, :)       !< onset of the open event
      real(SP), allocatable :: t_off(:, :)      !< first sample failing the condition
      real(SP), allocatable :: pend_dur(:, :)   !< duration of the open event so far
      real(SP), allocatable :: pend_peak(:, :)  !< sample furthest past the threshold (log only)
      !> Committed event values (reset per window)
      real(SP), allocatable :: first_time(:, :)
      real(SP), allocatable :: last_time(:, :)
      real(SP), allocatable :: dur_sum(:, :)
      real(SP), allocatable :: dur_max(:, :)
      real(SP), allocatable :: count(:, :)
      !> Event log: committed events since the last drain (enable_log)
      logical :: log_events = .false.
      integer :: ev_cap = 0
      integer :: n_events = 0
      integer :: n_dropped = 0                 !< cumulative rows over the cap
      real(SP), allocatable :: ev_t_on(:), ev_t_off(:), ev_dur(:), ev_peak(:)
      integer, allocatable :: ev_i(:), ev_j(:)
   contains
      procedure, public :: init
      procedure, public :: allocate_stat
      procedure, public :: set_threshold
      procedure, public :: enable_log
      procedure, public :: clear_events
      procedure, public :: open_events
      procedure, public :: accumulate
      procedure, public :: reset
      procedure, public :: finalize
      procedure, public :: get_stat
   end type type_accumulator

contains

   !> Initialise the accumulator dimensions and quantity label.
   !!
   !! Calls `finalize` first so it is safe to re-initialise an existing
   !! accumulator.
   !!
   !! @param[in]  d1        First spatial dimension.
   !! @param[in]  d2        Second spatial dimension.
   !! @param[in]  quantity  Label for the accumulated field (max 32 chars).
   subroutine init(this, d1, d2, quantity)
      class(type_accumulator), intent(inout) :: this
      integer, intent(in) :: d1, d2
      character(*), intent(in) :: quantity
      call this%finalize()
      this%dim1 = d1
      this%dim2 = d2
      this%quantity = quantity
      this%total_dt = 0.0_SP
   end subroutine init

   !> Allocate storage for one statistic.
   !!
   !! Must be called after `init` and before the first `accumulate`.
   !! Safe to call multiple times for the same `op`; subsequent calls
   !! are no-ops if storage is already allocated.
   !!
   !! Valid values of `op`:
   !! | `op`    | Storage allocated              |
   !! |---------|-------------------------------|
   !! | `"min"` | `val_min`                     |
   !! | `"max"` | `val_max`                     |
   !! | `"mean"`| `val_sum`                     |
   !! | `"rms"` | `val_sum`, `val_sum_sq`       |
   !!
   !! @param[in]  op  Statistic name: `"min"`, `"max"`, `"mean"`, or `"rms"`.
   subroutine allocate_stat(this, op)
      class(type_accumulator), intent(inout) :: this
      character(*), intent(in) :: op
      select case (trim(op))
      case ("min")
         if (.not. allocated(this%val_min)) &
            allocate (this%val_min(this%dim1, this%dim2), source=huge(1.0_SP))
      case ("max")
         if (.not. allocated(this%val_max)) &
            allocate (this%val_max(this%dim1, this%dim2), source=-huge(1.0_SP))
      case ("mean")
         if (.not. allocated(this%val_sum)) &
            allocate (this%val_sum(this%dim1, this%dim2), source=0.0_SP)
      case ("rms")
         if (.not. allocated(this%val_sum)) &
            allocate (this%val_sum(this%dim1, this%dim2), source=0.0_SP)
         if (.not. allocated(this%val_sum_sq)) &
            allocate (this%val_sum_sq(this%dim1, this%dim2), source=0.0_SP)
      case ("std")
         if (.not. allocated(this%val_sum)) &
            allocate (this%val_sum(this%dim1, this%dim2), source=0.0_SP)
         if (.not. allocated(this%val_sum_sq)) &
            allocate (this%val_sum_sq(this%dim1, this%dim2), source=0.0_SP)
         if (.not. allocated(this%shift)) &
            allocate (this%shift(this%dim1, this%dim2), source=0.0_SP)
         this%shifted = .true.
      case ("max_time")
         if (.not. allocated(this%val_max)) &
            allocate (this%val_max(this%dim1, this%dim2), source=-huge(1.0_SP))
         if (.not. allocated(this%t_max)) &
            allocate (this%t_max(this%dim1, this%dim2), source=FILL_VALUE)
      case ("first_time", "last_time", "duration", "duration_max", "count")
         if (.not. allocated(this%in_event)) then
            allocate (this%in_event(this%dim1, this%dim2), source=.false.)
            allocate (this%closing(this%dim1, this%dim2), source=.false.)
            allocate (this%t_on(this%dim1, this%dim2), source=FILL_VALUE)
            allocate (this%t_off(this%dim1, this%dim2), source=FILL_VALUE)
            allocate (this%pend_dur(this%dim1, this%dim2), source=0.0_SP)
            allocate (this%first_time(this%dim1, this%dim2), source=FILL_VALUE)
            allocate (this%last_time(this%dim1, this%dim2), source=FILL_VALUE)
            allocate (this%dur_sum(this%dim1, this%dim2), source=0.0_SP)
            allocate (this%dur_max(this%dim1, this%dim2), source=0.0_SP)
            allocate (this%count(this%dim1, this%dim2), source=0.0_SP)
         end if
      case default
         error stop "Unknown statistic operation: "//trim(op)
      end select
   end subroutine allocate_stat

   !> Set the event condition: value and direction (THR_ABOVE: sample >
   !! value; THR_BELOW: sample < value; THR_ABS: |sample| > value) and the
   !! two filters in seconds.
   subroutine set_threshold(this, value, direction, gap, min_duration)
      class(type_accumulator), intent(inout) :: this
      real(SP), intent(in) :: value
      integer, intent(in) :: direction
      real(SP), intent(in), optional :: gap, min_duration
      this%thr = value
      this%thr_dir = direction
      if (present(gap)) this%gap = gap
      if (present(min_duration)) this%min_duration = min_duration
   end subroutine set_threshold

   !> Ingest one time step of field data.
   !!
   !! Updates all allocated running sums:
   !! - `val_sum`    \f$\mathrel{+}= f \cdot \Delta t\f$
   !! - `val_sum_sq` \f$\mathrel{+}= f^2 \cdot \Delta t\f$
   !! - `val_min`    \f$= \min(\texttt{val\_min},\, f)\f$
   !! - `val_max`    \f$= \max(\texttt{val\_max},\, f)\f$
   !! - `total_dt`   \f$\mathrel{+}= \Delta t\f$
   !!
   !! @param[in]  value  Field snapshot at the current time step,
   !!                    shape `(dim1, dim2)`.
   !! @param[in]  dt     Time-step size \f$\Delta t > 0\f$.
   !! @param[in]  t          Sample time; required by max_time and the events.
   !! @param[in]  wet        Wet mask; extremes and events sample only where
   !!                        true (absent = every cell).
   !! @param[in]  wet_event  Stricter mask for the event test alone (the
   !!                        swash-edge depth floor); absent = wet.
   subroutine accumulate(this, value, dt, t, wet, wet_event)
      class(type_accumulator), intent(inout) :: this
      real(SP), intent(in) :: value(:, :)
      real(SP), intent(in) :: dt
      real(SP), intent(in), optional :: t
      logical, intent(in), optional :: wet(:, :), wet_event(:, :)

      this%total_dt = this%total_dt + dt
      if (present(wet)) then
         if (allocated(this%val_min)) &
            where (wet) this%val_min = min(this%val_min, value)
         if (allocated(this%t_max) .and. present(t)) &
            where (wet .and. value > this%val_max) this%t_max = t
         if (allocated(this%val_max)) &
            where (wet) this%val_max = max(this%val_max, value)
      else
         if (allocated(this%val_min)) this%val_min = min(this%val_min, value)
         if (allocated(this%t_max) .and. present(t)) &
            where (value > this%val_max) this%t_max = t
         if (allocated(this%val_max)) this%val_max = max(this%val_max, value)
      end if
      if (allocated(this%in_event) .and. present(t)) then
         if (present(wet_event)) then
            call step_events(this, value, dt, t, wet_event)
         else
            call step_events(this, value, dt, t, wet)
         end if
      end if
      if (this%shifted) then
         if (.not. this%have_shift) then
            this%shift = value
            this%have_shift = .true.
         end if
         this%val_sum = this%val_sum + (value - this%shift)*dt
         this%val_sum_sq = this%val_sum_sq + (value - this%shift)**2*dt
      else
         if (allocated(this%val_sum)) this%val_sum = this%val_sum + value*dt
         if (allocated(this%val_sum_sq)) this%val_sum_sq = this%val_sum_sq + value**2*dt
      end if
   end subroutine accumulate

   !> Log every committed event as a row; cap = rows kept between drains
   !! (the buffer starts small and doubles up to it).  Call after the
   !! event statistics are allocated.
   subroutine enable_log(this, cap)
      class(type_accumulator), intent(inout) :: this
      integer, intent(in) :: cap
      integer :: n0
      this%log_events = .true.
      this%ev_cap = cap
      this%n_events = 0
      this%n_dropped = 0
      if (allocated(this%ev_t_on)) deallocate (this%ev_t_on, this%ev_t_off, this%ev_dur, &
                                               this%ev_peak, this%ev_i, this%ev_j)
      n0 = min(cap, 1024)
      allocate (this%ev_t_on(n0), this%ev_t_off(n0), this%ev_dur(n0), this%ev_peak(n0), &
                this%ev_i(n0), this%ev_j(n0))
      if (.not. allocated(this%pend_peak)) &
         allocate (this%pend_peak(this%dim1, this%dim2), source=0.0_SP)
   end subroutine enable_log

   !> Double the row buffer, up to the cap; .false. when full
   logical function grow_log(this) result(ok)
      class(type_accumulator), intent(inout) :: this
      real(SP), allocatable :: r(:)
      integer, allocatable :: k(:)
      integer :: n, m
      n = size(this%ev_t_on)
      ok = n < this%ev_cap
      if (.not. ok) return
      m = min(this%ev_cap, 2*n)
      allocate (r(m)); r(1:n) = this%ev_t_on; call move_alloc(r, this%ev_t_on)
      allocate (r(m)); r(1:n) = this%ev_t_off; call move_alloc(r, this%ev_t_off)
      allocate (r(m)); r(1:n) = this%ev_dur; call move_alloc(r, this%ev_dur)
      allocate (r(m)); r(1:n) = this%ev_peak; call move_alloc(r, this%ev_peak)
      allocate (k(m)); k(1:n) = this%ev_i; call move_alloc(k, this%ev_i)
      allocate (k(m)); k(1:n) = this%ev_j; call move_alloc(k, this%ev_j)
   end function grow_log

   !> Forget the drained rows (the dropped count is cumulative)
   subroutine clear_events(this)
      class(type_accumulator), intent(inout) :: this
      this%n_events = 0
   end subroutine clear_events

   !> The events still open (met, or within the gap): onset, peak so far
   !! and cell, for the end-of-leg record
   subroutine open_events(this, n, t_on, peak, i_cell, j_cell)
      class(type_accumulator), intent(in) :: this
      integer, intent(out) :: n
      real(SP), allocatable, intent(out) :: t_on(:), peak(:)
      integer, allocatable, intent(out) :: i_cell(:), j_cell(:)
      integer :: i, j
      n = 0
      if (allocated(this%in_event)) n = count(this%in_event .or. this%closing)
      allocate (t_on(max(1, n)), peak(max(1, n)), i_cell(max(1, n)), j_cell(max(1, n)))
      if (n == 0) return
      n = 0
      do j = 1, this%dim2
         do i = 1, this%dim1
            if (.not. (this%in_event(i, j) .or. this%closing(i, j))) cycle
            n = n + 1
            t_on(n) = this%t_on(i, j)
            peak(n) = 0.0_SP
            if (allocated(this%pend_peak)) peak(n) = this%pend_peak(i, j)
            i_cell(n) = i
            j_cell(n) = j
         end do
      end do
   end subroutine open_events

   !> Advance the per-cell event state machine by one sample.
   subroutine step_events(this, value, dt, t, wet)
      class(type_accumulator), intent(inout) :: this
      real(SP), intent(in) :: value(:, :)
      real(SP), intent(in) :: dt, t
      logical, intent(in), optional :: wet(:, :)

      logical :: met
      integer :: i, j

      do j = 1, this%dim2
         do i = 1, this%dim1
            select case (this%thr_dir)
            case (THR_ABOVE)
               met = value(i, j) > this%thr
            case (THR_BELOW)
               met = value(i, j) < this%thr
            case default
               met = abs(value(i, j)) > this%thr
            end select
            if (present(wet)) met = met .and. wet(i, j)

            if (met) then
               if (.not. this%in_event(i, j)) then
                  if (this%closing(i, j)) then
                     ! back within the gap: the same event continues
                     this%closing(i, j) = .false.
                  else
                     this%t_on(i, j) = t
                     this%pend_dur(i, j) = 0.0_SP
                     if (this%log_events) this%pend_peak(i, j) = value(i, j)
                  end if
                  this%in_event(i, j) = .true.
               end if
               this%pend_dur(i, j) = this%pend_dur(i, j) + dt
               if (this%log_events) then
                  ! furthest past the threshold: the min for a below event
                  if (this%thr_dir == THR_BELOW) then
                     this%pend_peak(i, j) = min(this%pend_peak(i, j), value(i, j))
                  else if (this%thr_dir == THR_ABOVE) then
                     this%pend_peak(i, j) = max(this%pend_peak(i, j), value(i, j))
                  else if (abs(value(i, j)) > abs(this%pend_peak(i, j))) then
                     this%pend_peak(i, j) = value(i, j)
                  end if
               end if
            else if (this%in_event(i, j)) then
               this%in_event(i, j) = .false.
               this%t_off(i, j) = t
               if (this%gap > 0.0_SP) then
                  this%closing(i, j) = .true.
               else
                  call commit_event(this, i, j)
               end if
            end if
            ! the gap has run out: close for good
            if (this%closing(i, j) .and. .not. this%in_event(i, j)) then
               if (t - this%t_off(i, j) > this%gap) call commit_event(this, i, j)
            end if
         end do
      end do
   end subroutine step_events

   !> Commit the closed event of one cell, or discard it under min_duration.
   subroutine commit_event(this, i, j)
      class(type_accumulator), intent(inout) :: this
      integer, intent(in) :: i, j
      logical :: room

      this%closing(i, j) = .false.
      if (this%pend_dur(i, j) >= this%min_duration) then
         this%count(i, j) = this%count(i, j) + 1.0_SP
         if (this%first_time(i, j) == FILL_VALUE) this%first_time(i, j) = this%t_on(i, j)
         this%last_time(i, j) = this%t_on(i, j)
         this%dur_sum(i, j) = this%dur_sum(i, j) + this%pend_dur(i, j)
         this%dur_max(i, j) = max(this%dur_max(i, j), this%pend_dur(i, j))
         if (this%log_events) then
            room = this%n_events < size(this%ev_t_on)
            if (.not. room) room = grow_log(this)
            if (room) then
               this%n_events = this%n_events + 1
               this%ev_t_on(this%n_events) = this%t_on(i, j)
               this%ev_t_off(this%n_events) = this%t_off(i, j)
               this%ev_dur(this%n_events) = this%pend_dur(i, j)
               this%ev_peak(this%n_events) = this%pend_peak(i, j)
               this%ev_i(this%n_events) = i
               this%ev_j(this%n_events) = j
            else
               this%n_dropped = this%n_dropped + 1
            end if
         end if
      end if
      this%pend_dur(i, j) = 0.0_SP
   end subroutine commit_event

   !> Retrieve the final statistic as a 2-D array.
   !!
   !! Applies the normalisation formula for the requested `op`:
   !!
   !! | `op`     | Formula |
   !! |----------|---------|
   !! | `"min"`  | \f$\min\f$ (already stored) |
   !! | `"max"`  | \f$\max\f$ (already stored) |
   !! | `"mean"` | \f$\bar{f} = \sum f\,\Delta t \;/\; T\f$ |
   !! | `"rms"`  | \f$\sqrt{\sum f^2\,\Delta t \;/\; T}\f$ |
   !!
   !! Returns a zero-filled array if the required storage is not
   !! allocated or if `total_dt <= 0`.
   !!
   !! @param[in]  op    Statistic name (same values as `allocate_stat`).
   !! @return           Array of shape `(dim1, dim2)` with the statistic value.
   function get_stat(this, op) result(stat)
      class(type_accumulator), intent(in) :: this
      character(*), intent(in) :: op
      real(SP), allocatable :: stat(:, :)

      allocate (stat(this%dim1, this%dim2), source=0.0_SP)

      select case (trim(op))
      case ("min")
         ! a cell never sampled (dry throughout, under a wet mask) holds the fill
         if (allocated(this%val_min)) then
            stat = this%val_min
            where (stat == huge(1.0_SP)) stat = FILL_VALUE
         end if
      case ("max")
         if (allocated(this%val_max)) then
            stat = this%val_max
            where (stat == -huge(1.0_SP)) stat = FILL_VALUE
         end if
      case ("mean")
         if (allocated(this%val_sum) .and. this%total_dt > 0.0_SP) then
            stat = this%val_sum/this%total_dt
            if (this%shifted) stat = stat + this%shift
         end if
      case ("rms")
         if (allocated(this%val_sum_sq) .and. this%total_dt > 0.0_SP) then
            if (this%shifted) then
               ! E[f^2] = E[s^2] + 2 shift E[s] + shift^2 with s = f - shift
               stat = sqrt(max(0.0_SP, this%val_sum_sq/this%total_dt &
                               + 2.0_SP*this%shift*(this%val_sum/this%total_dt) &
                               + this%shift**2))
            else
               stat = sqrt(max(0.0_SP, this%val_sum_sq/this%total_dt))
            end if
         end if
      case ("std")
         ! shift cancels in the variance identity
         if (allocated(this%val_sum_sq) .and. this%total_dt > 0.0_SP) &
            stat = sqrt(max(0.0_SP, this%val_sum_sq/this%total_dt &
                            - (this%val_sum/this%total_dt)**2))
      case ("max_time")
         if (allocated(this%t_max)) stat = this%t_max
      case ("first_time")
         if (allocated(this%first_time)) stat = this%first_time
      case ("last_time")
         if (allocated(this%last_time)) stat = this%last_time
      case ("duration")
         if (allocated(this%dur_sum)) stat = this%dur_sum
      case ("duration_max")
         if (allocated(this%dur_max)) stat = this%dur_max
      case ("count")
         if (allocated(this%count)) stat = this%count
      case default
         error stop "Unknown statistic operation: "//trim(op)
      end select
   end function get_stat

   !> Reset all running sums to zero without freeing memory.
   !!
   !! Use this to start a new accumulation window after calling `get_stat`,
   !! avoiding the overhead of reallocation.
   subroutine reset(this)
      class(type_accumulator), intent(inout) :: this
      this%total_dt = 0.0_SP
      if (allocated(this%val_sum)) this%val_sum = 0.0_SP
      if (allocated(this%val_sum_sq)) this%val_sum_sq = 0.0_SP
      if (allocated(this%val_max)) this%val_max = -huge(1.0_SP)
      if (allocated(this%val_min)) this%val_min = huge(1.0_SP)
      if (allocated(this%t_max)) this%t_max = FILL_VALUE
      ! committed events only: an open event carries into the next window
      if (allocated(this%first_time)) then
         this%first_time = FILL_VALUE
         this%last_time = FILL_VALUE
         this%dur_sum = 0.0_SP
         this%dur_max = 0.0_SP
         this%count = 0.0_SP
      end if
      ! re-capture the shift each window (tracks a drifting mean)
      this%have_shift = .false.
   end subroutine reset

   !> Free all allocated arrays and reset dimensions to zero.
   subroutine finalize(this)
      class(type_accumulator), intent(inout) :: this
      if (allocated(this%val_min)) deallocate (this%val_min)
      if (allocated(this%val_max)) deallocate (this%val_max)
      if (allocated(this%val_sum)) deallocate (this%val_sum)
      if (allocated(this%val_sum_sq)) deallocate (this%val_sum_sq)
      if (allocated(this%shift)) deallocate (this%shift)
      if (allocated(this%t_max)) deallocate (this%t_max)
      if (allocated(this%in_event)) deallocate (this%in_event, this%closing, &
                                                this%t_on, this%t_off, this%pend_dur, &
                                                this%first_time, this%last_time, &
                                                this%dur_sum, this%dur_max, this%count)
      if (allocated(this%pend_peak)) deallocate (this%pend_peak)
      if (allocated(this%ev_t_on)) deallocate (this%ev_t_on, this%ev_t_off, this%ev_dur, &
                                               this%ev_peak, this%ev_i, this%ev_j)
      this%log_events = .false.
      this%n_events = 0
      this%n_dropped = 0
      this%thr_dir = THR_NONE
      this%gap = 0.0_SP
      this%min_duration = 0.0_SP
      this%shifted = .false.
      this%have_shift = .false.
      this%dim1 = 0
      this%dim2 = 0
      this%total_dt = 0.0_SP
   end subroutine finalize

end module core_accumulators_mod
