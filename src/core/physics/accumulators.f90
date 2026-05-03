module core_accumulators_mod
   use core_constants_mod, only: SP
   implicit none

   type, public :: type_accumulator
      character(32) :: quantity
      integer :: dim1 = 0, dim2 = 0
      
      real(SP), allocatable :: val_min(:,:)
      real(SP), allocatable :: val_max(:,:)
      real(SP), allocatable :: val_sum(:,:)
      real(SP), allocatable :: val_sum_sq(:,:)
      real(SP), allocatable :: val_ewma(:,:)
      real(SP)              :: total_dt = 0.0_SP
      real(SP)              :: alpha = -1.0_SP ! EWMA factor (0 < alpha <= 1)
      integer               :: count = 0
   contains
      procedure, public :: init
      procedure, public :: allocate_stat
      procedure, public :: accumulate
      procedure, public :: reset
      procedure, public :: finalize
      procedure, public :: get_stat
   end type type_accumulator

contains

   subroutine init(this, d1, d2, quantity)
      class(type_accumulator), intent(inout) :: this
      integer, intent(in) :: d1, d2
      character(*), intent(in) :: quantity
      call this%finalize()
      this%dim1 = d1
      this%dim2 = d2
      this%quantity = quantity
      this%count = 0
      this%total_dt = 0.0_SP
      this%alpha = -1.0_SP
   end subroutine init

   subroutine allocate_stat(this, op)
      class(type_accumulator), intent(inout) :: this
      character(*), intent(in) :: op
      
      select case (trim(op))
      case ("min")
         if (.not. allocated(this%val_min)) allocate(this%val_min(this%dim1, this%dim2), source=1.0e30_SP)
      case ("max")
         if (.not. allocated(this%val_max)) allocate(this%val_max(this%dim1, this%dim2), source=-1.0e30_SP)
      case ("mean", "rms")
         if (.not. allocated(this%val_sum)) allocate(this%val_sum(this%dim1, this%dim2), source=0.0_SP)
         if (trim(op) == "rms" .and. .not. allocated(this%val_sum_sq)) &
            allocate(this%val_sum_sq(this%dim1, this%dim2), source=0.0_SP)
      case ("ewma")
         if (.not. allocated(this%val_ewma)) allocate(this%val_ewma(this%dim1, this%dim2), source=0.0_SP)
      case default
         error stop "Unknown statistic operation: " // op
      end select
   end subroutine allocate_stat

   !> @brief Accumulate data point with time-weighting.
   !>
   !> For standard mode (\f$\alpha \le 0\f$):
   !> Performs time-weighted integration: 
   !> \f$ \bar{X} = \frac{\int X(t) dt}{\int dt} \approx \frac{\sum (X_i \cdot \Delta t_i)}{\sum \Delta t_i} \f$
   !>
   !> For EWMA mode (\f$\alpha > 0\f$):
   !> Performs exponential moving average: 
   !> \f$ S_n = \alpha \cdot X_n + (1 - \alpha) \cdot S_{n-1} \f$
   subroutine accumulate(this, value, dt)
      class(type_accumulator), intent(inout) :: this
      real(SP), intent(in) :: value(:,:)
      real(SP), intent(in) :: dt

      if (this%alpha > 0.0_SP) then
         if (.not. allocated(this%val_ewma)) allocate(this%val_ewma(this%dim1, this%dim2), source=value)
         this%val_ewma = (1.0_SP - this%alpha) * this%val_ewma + this%alpha * value
      else
         if (allocated(this%val_min)) this%val_min = min(this%val_min, value)
         if (allocated(this%val_max)) this%val_max = max(this%val_max, value)
         if (allocated(this%val_sum)) then
            this%val_sum = this%val_sum + (value * dt)
            this%total_dt = this%total_dt + dt
         end if
         if (allocated(this%val_sum_sq)) this%val_sum_sq = this%val_sum_sq + (value**2 * dt)
      end if
      this%count = this%count + 1
   end subroutine accumulate


   function get_stat(this, op) result(stat)
      class(type_accumulator), intent(in) :: this
      character(*), intent(in) :: op
      real(SP), allocatable :: stat(:,:)
      
      allocate(stat(this%dim1, this%dim2))
      
      select case (trim(op))
      case ("min")
         if (allocated(this%val_min)) stat = this%val_min
      case ("max")
         if (allocated(this%val_max)) stat = this%val_max
      case ("mean")
         if (this%alpha > 0.0_SP) then
            if (allocated(this%val_ewma)) stat = this%val_ewma
         else if (allocated(this%val_sum) .and. this%total_dt > 0.0_SP) then
            stat = this%val_sum / this%total_dt
         end if
      case ("rms")
         if (allocated(this%val_sum_sq) .and. this%total_dt > 0.0_SP) then
            stat = sqrt(max(0.0_SP, this%val_sum_sq / this%total_dt))
         end if
      case ("ewma")
         if (allocated(this%val_ewma)) stat = this%val_ewma
      case default
         error stop "Unknown statistic operation: " // op
      end select
   end function get_stat

   subroutine reset(this)
      class(type_accumulator), intent(inout) :: this
      if (allocated(this%val_min)) this%val_min = 1.0e30_SP
      if (allocated(this%val_max)) this%val_max = -1.0e30_SP
      if (allocated(this%val_sum)) this%val_sum = 0.0_SP
      if (allocated(this%val_sum_sq)) this%val_sum_sq = 0.0_SP
      this%count = 0
      this%total_dt = 0.0_SP
   end subroutine reset

   subroutine finalize(this)
      class(type_accumulator), intent(inout) :: this
      if (allocated(this%val_min)) deallocate(this%val_min)
      if (allocated(this%val_max)) deallocate(this%val_max)
      if (allocated(this%val_sum)) deallocate(this%val_sum)
      if (allocated(this%val_sum_sq)) deallocate(this%val_sum_sq)
      if (allocated(this%val_ewma)) deallocate(this%val_ewma)
      this%dim1 = 0
      this%dim2 = 0
   end subroutine finalize

end module core_accumulators_mod
