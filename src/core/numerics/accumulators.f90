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
module core_accumulators_mod
   use core_constants_mod, only: SP
   implicit none

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
   contains
      procedure, public :: init
      procedure, public :: allocate_stat
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
      case default
         error stop "Unknown statistic operation: "//trim(op)
      end select
   end subroutine allocate_stat

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
   subroutine accumulate(this, value, dt)
      class(type_accumulator), intent(inout) :: this
      real(SP), intent(in) :: value(:, :)
      real(SP), intent(in) :: dt

      this%total_dt = this%total_dt + dt
      if (allocated(this%val_min)) this%val_min = min(this%val_min, value)
      if (allocated(this%val_max)) this%val_max = max(this%val_max, value)
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
         if (allocated(this%val_min)) stat = this%val_min
      case ("max")
         if (allocated(this%val_max)) stat = this%val_max
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
      this%shifted = .false.
      this%have_shift = .false.
      this%dim1 = 0
      this%dim2 = 0
      this%total_dt = 0.0_SP
   end subroutine finalize

end module core_accumulators_mod
