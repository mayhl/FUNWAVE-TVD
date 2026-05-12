module core_accumulators_mod
   use core_constants_mod, only: SP
   implicit none

   type, public :: type_accumulator
      character(32) :: quantity
      integer  :: dim1 = 0, dim2 = 0
      real(SP) :: total_dt = 0.0_SP        ! Σ dt — normalisation for all time-averaged ops
      real(SP), allocatable :: val_sum(:,:)    ! Σ (x · dt)
      real(SP), allocatable :: val_sum_sq(:,:) ! Σ (x² · dt)
      real(SP), allocatable :: val_max(:,:)
      real(SP), allocatable :: val_min(:,:)
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
      integer,      intent(in) :: d1, d2
      character(*), intent(in) :: quantity
      call this%finalize()
      this%dim1     = d1
      this%dim2     = d2
      this%quantity = quantity
      this%total_dt = 0.0_SP
   end subroutine init

   subroutine allocate_stat(this, op)
      class(type_accumulator), intent(inout) :: this
      character(*), intent(in) :: op
      select case (trim(op))
      case ("min")
         if (.not. allocated(this%val_min)) &
            allocate(this%val_min(this%dim1, this%dim2), source=huge(1.0_SP))
      case ("max")
         if (.not. allocated(this%val_max)) &
            allocate(this%val_max(this%dim1, this%dim2), source=-huge(1.0_SP))
      case ("mean")
         if (.not. allocated(this%val_sum)) &
            allocate(this%val_sum(this%dim1, this%dim2), source=0.0_SP)
      case ("rms", "var", "hsig")
         if (.not. allocated(this%val_sum)) &
            allocate(this%val_sum(this%dim1, this%dim2), source=0.0_SP)
         if (.not. allocated(this%val_sum_sq)) &
            allocate(this%val_sum_sq(this%dim1, this%dim2), source=0.0_SP)
      case default
         error stop "Unknown statistic operation: " // trim(op)
      end select
   end subroutine allocate_stat

   subroutine accumulate(this, value, dt)
      class(type_accumulator), intent(inout) :: this
      real(SP), intent(in) :: value(:,:)
      real(SP), intent(in) :: dt

      this%total_dt = this%total_dt + dt
      if (allocated(this%val_min))    this%val_min    = min(this%val_min, value)
      if (allocated(this%val_max))    this%val_max    = max(this%val_max, value)
      if (allocated(this%val_sum))    this%val_sum    = this%val_sum    + value      * dt
      if (allocated(this%val_sum_sq)) this%val_sum_sq = this%val_sum_sq + value**2  * dt
   end subroutine accumulate

   function get_stat(this, op) result(stat)
      class(type_accumulator), intent(in) :: this
      character(*), intent(in) :: op
      real(SP), allocatable :: stat(:,:)
      real(SP), allocatable :: mean_sq(:,:), sq_mean(:,:)

      allocate(stat(this%dim1, this%dim2), source=0.0_SP)

      select case (trim(op))
      case ("min")
         if (allocated(this%val_min)) stat = this%val_min
      case ("max")
         if (allocated(this%val_max)) stat = this%val_max
      case ("mean")
         if (allocated(this%val_sum) .and. this%total_dt > 0.0_SP) &
            stat = this%val_sum / this%total_dt
      case ("rms")
         if (allocated(this%val_sum_sq) .and. this%total_dt > 0.0_SP) &
            stat = sqrt(max(0.0_SP, this%val_sum_sq / this%total_dt))
      case ("var")
         if (allocated(this%val_sum_sq) .and. allocated(this%val_sum) &
               .and. this%total_dt > 0.0_SP) then
            mean_sq = (this%val_sum / this%total_dt)**2
            sq_mean = this%val_sum_sq / this%total_dt
            stat = max(0.0_SP, sq_mean - mean_sq)
         end if
      case ("hsig")
         if (allocated(this%val_sum_sq) .and. allocated(this%val_sum) &
               .and. this%total_dt > 0.0_SP) then
            mean_sq = (this%val_sum / this%total_dt)**2
            sq_mean = this%val_sum_sq / this%total_dt
            stat = 4.0_SP * sqrt(max(0.0_SP, sq_mean - mean_sq))
         end if
      case default
         error stop "Unknown statistic operation: " // trim(op)
      end select
   end function get_stat

   subroutine reset(this)
      class(type_accumulator), intent(inout) :: this
      this%total_dt = 0.0_SP
      if (allocated(this%val_sum))    this%val_sum    = 0.0_SP
      if (allocated(this%val_sum_sq)) this%val_sum_sq = 0.0_SP
      if (allocated(this%val_max))    this%val_max    = -huge(1.0_SP)
      if (allocated(this%val_min))    this%val_min    =  huge(1.0_SP)
   end subroutine reset

   subroutine finalize(this)
      class(type_accumulator), intent(inout) :: this
      if (allocated(this%val_min))    deallocate(this%val_min)
      if (allocated(this%val_max))    deallocate(this%val_max)
      if (allocated(this%val_sum))    deallocate(this%val_sum)
      if (allocated(this%val_sum_sq)) deallocate(this%val_sum_sq)
      this%dim1     = 0
      this%dim2     = 0
      this%total_dt = 0.0_SP
   end subroutine finalize

end module core_accumulators_mod
