!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Boundary-condition service: per-step ghost updates for the model
!  state — MPI halo exchange, physical wall fills, and dry-cell
!  velocity masking.  Ports the state subset of legacy EXCHANGE
!  (old/bc.F): eta (VTYPE=1), u/hu (VTYPE=2), v/hv (VTYPE=3), and the
!  wet/dry mask (VTYPE=1 via a real copy).
!
!  Periodicity is owned by the grid cart topology (periodic_y at
!  grid%setup): halo_exchange wraps the y-ghosts and no rank reports a
!  y-boundary, so the wall fills skip those faces without special
!  casing here.
!
!  Wavemaker-owned west boundary (legacy ABS / LEFT_BC_IRR): the wall
!  mirror is skipped on the west face; the wavemaker BC writes those
!  ghosts (Step 6d).
!
!  Feature fields exchanged by legacy EXCHANGE (Wsurf, P_center,
!  nu_break, sediment) join when their physics modules are ported.
!
!  HISTORY :
!    07/09/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_bc_mod
   use core_constants_mod, only: SP
   use core_grid_mod, only: type_grid_2d
   use model_fields_2d_mod, only: type_fields_2d
   use model_kernel_bc_mod, only: fill_ghost_wall, SIGN_MIRROR, SIGN_ANTI
   implicit none

   private
   public :: type_model_bc

   type :: type_model_bc
      ! Physical wall fills per face: grid boundary flag AND not
      ! wavemaker-owned.  Interior ranks and periodic-wrap faces are
      ! .false. via the grid flags.
      logical :: fill_west = .false.
      logical :: fill_east = .false.
      logical :: fill_south = .false.
      logical :: fill_north = .false.
   contains
      procedure :: init => bc_init
      procedure :: exchange_state => bc_exchange_state
      procedure :: exchange_dispersion => bc_exchange_dispersion
   end type type_model_bc

contains

   subroutine bc_init(this, grid, wavemaker_type)
      class(type_model_bc), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      character(*), intent(in) :: wavemaker_type

      logical :: west_owned_by_wavemaker

      ! legacy: WaveMaker(1:3)=='ABS' .or. WaveMaker(1:11)=='LEFT_BC_IRR'
      west_owned_by_wavemaker = .false.
      if (len(wavemaker_type) >= 3) then
         if (wavemaker_type(1:3) == "ABS") west_owned_by_wavemaker = .true.
      end if
      if (len(wavemaker_type) >= 11) then
         if (wavemaker_type(1:11) == "LEFT_BC_IRR") west_owned_by_wavemaker = .true.
      end if

      this%fill_west = grid%is_back_boundary .and. .not. west_owned_by_wavemaker
      this%fill_east = grid%is_shore_boundary
      this%fill_south = grid%is_right_boundary
      this%fill_north = grid%is_left_boundary

   end subroutine bc_init

   ! ----------------------------------------------------------------
   ! Ghost update for the advanced state, in legacy EXCHANGE order:
   ! scalars mirror on every wall, normal velocities reflect
   ! antisymmetrically ($u = 0$ at x-walls, $v = 0$ at y-walls):
   !   $$ \eta:\ (+,+) \qquad u, p, hu:\ (-,+) \qquad v, q, hv:\ (+,-) $$
   ! The mask travels as a real copy with scalar mirror semantics.
   ! Breaking fields (nu_break, age_break) mirror as scalars when
   ! allocated (legacy VISCOSITY_BREAKING branch).
   ! Dry-cell velocities are then zeroed (legacy U = U*MASK):
   !   $$ u := u\,m, \quad v := v\,m, \quad hu := hu\,m, \quad hv := hv\,m $$
   ! Ubar/Vbar are exchanged (their ghosts are unread in legacy —
   ! harmless) but NOT masked: legacy keeps a freshly-dried cell's
   ! Ubar until the etauv dry zeroing of the next stage.  Masking
   ! them here was a ~2e-2 runup u deviation on flume_1d_wk_reg.
   ! ----------------------------------------------------------------
   subroutine bc_exchange_state(this, grid, fields)
      class(type_model_bc), intent(in) :: this
      type(type_grid_2d), intent(in) :: grid
      type(type_fields_2d), intent(inout) :: fields

      real(SP), allocatable :: rmask(:, :)

      call exchange_one(this, grid, fields%eta, SIGN_MIRROR, SIGN_MIRROR)

      allocate (rmask, source=real(fields%mask, SP))
      call exchange_one(this, grid, rmask, SIGN_MIRROR, SIGN_MIRROR)
      fields%mask = nint(rmask)
      deallocate (rmask)

      call exchange_one(this, grid, fields%u, SIGN_ANTI, SIGN_MIRROR)
      call exchange_one(this, grid, fields%p, SIGN_ANTI, SIGN_MIRROR)
      call exchange_one(this, grid, fields%hu, SIGN_ANTI, SIGN_MIRROR)
      call exchange_one(this, grid, fields%v, SIGN_MIRROR, SIGN_ANTI)
      call exchange_one(this, grid, fields%q, SIGN_MIRROR, SIGN_ANTI)
      call exchange_one(this, grid, fields%hv, SIGN_MIRROR, SIGN_ANTI)

      if (allocated(fields%nu_break)) then
         call exchange_one(this, grid, fields%nu_break, SIGN_MIRROR, SIGN_MIRROR)
         call exchange_one(this, grid, fields%age_break, SIGN_MIRROR, SIGN_MIRROR)
      end if

      fields%u = fields%u*fields%mask
      fields%v = fields%v*fields%mask
      fields%hu = fields%hu*fields%mask
      fields%hv = fields%hv*fields%mask

   end subroutine bc_exchange_state

   ! ----------------------------------------------------------------
   ! Ghost update for the dispersion velocity corrections (legacy
   ! EXCHANGE_DISPERSION): $u_4$ reflects like $u$ (anti in x), $v_4$
   ! like $v$ (anti in y).  Their ghosts feed the 4th-order face
   ! reconstruction and the centred $\nabla u_4$ source stencils.
   ! Walls fill at every physical boundary — no wavemaker exemption,
   ! since no later BC rewrites dispersion ghosts.  Legacy also
   ! mirrors the raw derivative intermediates and, under breaking/
   ! Gamma2 flags, etat/Ut chains — those join with their consumers.
   ! ----------------------------------------------------------------
   subroutine bc_exchange_dispersion(this, grid, u4, v4)
      class(type_model_bc), intent(in) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: u4(:, :), v4(:, :)

      call grid%halo_exchange(u4)
      call fill_ghost_wall(grid%lp, grid%is_back_boundary, grid%is_shore_boundary, &
                           grid%is_right_boundary, grid%is_left_boundary, &
                           SIGN_ANTI, SIGN_MIRROR, u4)
      call grid%halo_exchange(v4)
      call fill_ghost_wall(grid%lp, grid%is_back_boundary, grid%is_shore_boundary, &
                           grid%is_right_boundary, grid%is_left_boundary, &
                           SIGN_MIRROR, SIGN_ANTI, v4)

   end subroutine bc_exchange_dispersion

   subroutine exchange_one(this, grid, f, sign_x, sign_y)
      class(type_model_bc), intent(in) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: f(:, :)
      real(SP), intent(in) :: sign_x, sign_y

      call grid%halo_exchange(f)
      call fill_ghost_wall(grid%lp, this%fill_west, this%fill_east, &
                           this%fill_south, this%fill_north, sign_x, sign_y, f)

   end subroutine exchange_one

end module model_bc_mod
