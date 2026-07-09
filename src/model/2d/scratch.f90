module model_scratch_mod
   use core_constants_mod, only: SP
   implicit none

   !> Generic pre-allocated slot pool for intra-timestep temporary 2-D arrays.
   !>
   !> Lifecycle:
   !>   1. Each physics module calls reserve(n)/unreserve(n) during init_compute,
   !>      mirroring the nesting of acquire/release at runtime.
   !>      Nested reserves accumulate; peak tracks the high-water mark.
   !>   2. Call finalize(dim1, dim2) once to allocate slots(:,:,1:peak).
   !>   3. At runtime, acquire() returns the first free slot index; release(k) frees it.
   !>
   !> GPU note: after finalize, add
   !>   !$omp target enter data map(alloc: this%slots)
   !> Acquire/release operate on host-only in_use(:) and are never called inside kernels.
   type, public :: type_scratch_pool
      real(SP), allocatable :: slots(:,:,:)  !< device-resident scratch storage
      logical,  allocatable :: in_use(:)     !< host-only acquire/release bookkeeping
      integer :: peak  = 0  !< high-water slot count (set by reserve/unreserve)
      integer :: depth = 0  !< running nesting depth during reservation phase
      integer :: dim1  = 0
      integer :: dim2  = 0
   contains
      procedure :: reserve   => pool_reserve
      procedure :: unreserve => pool_unreserve
      procedure :: finalize  => pool_finalize
      procedure :: acquire   => pool_acquire
      procedure :: release   => pool_release
      procedure :: free      => pool_free
   end type type_scratch_pool

   !> Model-level scratch wrapper.  Holds a field pool shaped to the local grid
   !> (local_nx + 2*N_GHOST, local_ny + 2*N_GHOST).  spec/bc pools are added when
   !> the wavemaker and nesting modules reach init_compute.
   type, public :: type_model_scratch
      type(type_scratch_pool) :: field
   contains
      procedure :: finalize => scratch_finalize
      procedure :: free     => scratch_free
   end type type_model_scratch

contains

   ! ── reservation helpers ────────────────────────────────────────────────────

   subroutine pool_reserve(this, n)
      class(type_scratch_pool), intent(inout) :: this
      integer,                  intent(in)    :: n
      this%depth = this%depth + n
      if (this%depth > this%peak) this%peak = this%depth
   end subroutine pool_reserve

   subroutine pool_unreserve(this, n)
      class(type_scratch_pool), intent(inout) :: this
      integer,                  intent(in)    :: n
      this%depth = this%depth - n
   end subroutine pool_unreserve

   ! ── allocation ─────────────────────────────────────────────────────────────

   subroutine pool_finalize(this, dim1, dim2)
      class(type_scratch_pool), intent(inout) :: this
      integer,                  intent(in)    :: dim1, dim2

      if (allocated(this%slots))  deallocate(this%slots)
      if (allocated(this%in_use)) deallocate(this%in_use)
      this%dim1 = dim1
      this%dim2 = dim2
      if (this%peak > 0) then
         allocate(this%slots (dim1, dim2, this%peak), source=0.0_SP)
         allocate(this%in_use(this%peak),              source=.false.)
      end if
   end subroutine pool_finalize

   ! ── runtime acquire / release ──────────────────────────────────────────────

   function pool_acquire(this) result(k)
      class(type_scratch_pool), intent(inout) :: this
      integer :: k, i

      do i = 1, size(this%in_use)
         if (.not. this%in_use(i)) then
            this%in_use(i) = .true.
            k = i
            return
         end if
      end do
      error stop "type_scratch_pool: acquire failed — all slots in use"
   end function pool_acquire

   subroutine pool_release(this, k)
      class(type_scratch_pool), intent(inout) :: this
      integer,                  intent(in)    :: k
      this%in_use(k) = .false.
   end subroutine pool_release

   ! ── teardown ───────────────────────────────────────────────────────────────

   subroutine pool_free(this)
      class(type_scratch_pool), intent(inout) :: this
      if (allocated(this%slots))  deallocate(this%slots)
      if (allocated(this%in_use)) deallocate(this%in_use)
      this%peak  = 0
      this%depth = 0
      this%dim1  = 0
      this%dim2  = 0
   end subroutine pool_free

   ! ── type_model_scratch ─────────────────────────────────────────────────────

   subroutine scratch_finalize(this, dim1, dim2)
      class(type_model_scratch), intent(inout) :: this
      integer,                   intent(in)    :: dim1, dim2
      call this%field%finalize(dim1, dim2)
   end subroutine scratch_finalize

   subroutine scratch_free(this)
      class(type_model_scratch), intent(inout) :: this
      call this%field%free()
   end subroutine scratch_free

end module model_scratch_mod
