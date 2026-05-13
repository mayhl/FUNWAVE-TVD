!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Output manager: owns all output channels and drives the
!  per-timestep output loop.
!
!  Call order:
!   1. init(...)       — after grid%setup(), reads channel configs
!   2. step(t, dt)     — every timestep from stepper_engine/main
!   3. finalize()      — at simulation end
!
!  stepper_engine has NO dependency on this module — the engine
!  exposes a callback hook or the driver calls step() explicitly.
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module core_output_manager_mod
   use core_constants_mod,      only: SP
   use core_comm_mod,           only: type_comm
   use core_grid_mod,           only: type_grid_2d
   use core_field_registry_mod, only: type_field_registry
   use core_output_channel_mod, only: type_output_channel
   implicit none(external)

   private
   public :: type_output_manager

   type :: type_output_manager
      type(type_output_channel), allocatable :: channels(:)
      integer :: n_channels = 0
   contains
      procedure :: step    => manager_step
      procedure :: finalize => manager_finalize
   end type type_output_manager

contains

   ! Called every timestep. Dispatches to each active channel.
   subroutine manager_step(this, t, dt, registry, comm)
      class(type_output_manager), intent(inout) :: this
      real(SP),                   intent(in)    :: t, dt
      type(type_field_registry),  intent(in)    :: registry
      type(type_comm),            intent(inout) :: comm

      integer :: k
      do k = 1, this%n_channels
         call this%channels(k)%step(t, dt, registry, comm)
      end do
   end subroutine manager_step

   subroutine manager_finalize(this)
      class(type_output_manager), intent(inout) :: this
      integer :: k
      if (allocated(this%channels)) then
         do k = 1, this%n_channels
            call this%channels(k)%finalize()
         end do
         deallocate(this%channels)
      end if
      this%n_channels = 0
   end subroutine manager_finalize

end module core_output_manager_mod
