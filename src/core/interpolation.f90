module core_interpolation_mod
   use core_constants_mod, only: SP
   implicit none

   ! Base class for spatial/temporal mapping
   type, public :: type_interpolator
      ! NOTE: Needs connection to core_comm_mod to synchronize boundaries/ghost-cells
      ! for parallel interpolation across subdomains.
      character(16) :: method = "linear"
      integer :: dim_in(2) = 0, dim_out(2) = 0
      
      ! Weights and indices for GPU-friendly 'gather' operations
      ! Pre-allocated during initialization phase
      real(SP), allocatable :: weights(:,:)
      integer, allocatable :: indices(:,:)
      
   contains
      procedure, public :: init
      procedure, public :: map
      procedure, public :: finalize
   end type type_interpolator

contains

   subroutine init(this, method, d_in, d_out)
      class(type_interpolator), intent(inout) :: this
      character(*), intent(in) :: method
      integer, intent(in) :: d_in(2), d_out(2)
      
      this%method = method
      this%dim_in = d_in
      this%dim_out = d_out
      ! Allocation of weights/indices would happen here
      ! For now, we stub them to ensure framework connectivity
      allocate(this%weights(d_out(1), d_out(2)))
      allocate(this%indices(d_out(1), d_out(2)))
   end subroutine init

   subroutine map(this, input_field, output_buffer)
      class(type_interpolator), intent(in) :: this
      real(SP), intent(in) :: input_field(:,:)
      real(SP), intent(out) :: output_buffer(:,:)
      
      ! This is the hot loop "kernel".
      ! It is designed to be easily offloaded to GPU via OpenACC/OpenMP
      !$acc parallel loop collapse(2)
      do j = 1, this%dim_out(2)
         do i = 1, this%dim_out(1)
             ! Abstracted gather operation
             ! output_buffer(i,j) = interpolate(input_field, this%indices(i,j), this%weights(i,j))
             output_buffer(i,j) = input_field(1,1) ! Placeholder for mapping logic
         end do
      end do
      !$acc end parallel loop
   end subroutine map

   subroutine finalize(this)
      class(type_interpolator), intent(inout) :: this
      if (allocated(this%weights)) deallocate(this%weights)
      if (allocated(this%indices)) deallocate(this%indices)
   end subroutine finalize

end module core_interpolation_mod
