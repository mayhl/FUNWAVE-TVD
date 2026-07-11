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
      ! Legacy EXCHANGE feature gates (old/bc.F:441-449): AGE_BREAKING
      ! under VISCOSITY_BREAKING, nu_break also under WAVEMAKER_VIS.
      ! The show-only display mode exchanges neither (stepper sets).
      logical :: exch_age = .false.
      logical :: exch_nu = .false.
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
   ! Breaking fields (age_break, nu_break) mirror as scalars under
   ! their legacy gates (exch_age/exch_nu, legacy order age first).
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

      if (this%exch_age) then
         call exchange_one(this, grid, fields%age_break, SIGN_MIRROR, SIGN_MIRROR)
      end if
      if (this%exch_nu) then
         call exchange_one(this, grid, fields%nu_break, SIGN_MIRROR, SIGN_MIRROR)
      end if

      fields%u = fields%u*fields%mask
      fields%v = fields%v*fields%mask
      fields%hu = fields%hu*fields%mask
      fields%hv = fields%hv*fields%mask

   end subroutine bc_exchange_state

   ! ----------------------------------------------------------------
   ! Ghost update for the dispersion COMPONENT arrays (legacy
   ! EXCHANGE_DISPERSION, same array order): each raw derivative
   ! mirrors with its own parity — u-like (anti-x), v-like (anti-y),
   ! or scalar-like (mirror both) — and u4/v4 are then ASSEMBLED
   ! ghost-inclusive by cal_dispersion_assemble.  Mirroring u4/v4
   ! directly is wrong wherever $V_{xy} \ne 0$ (the components mix
   ! parities) and flips MUSCL limiter branches through the sign of
   ! zero even on quiescent fields (parity ledger 8c/8e/9 root).
   ! Wavemaker-owned west face: fills skipped via fill_west (legacy
   ! PHI_COLL exemption) — the workspace ghosts stay zeroed, which
   ! reproduces legacy's never-written ghosts there.
   ! Only the disp_time_left = .false. gamma2 chain is ported (the
   ! legacy DISP_TIME_LEFT path is compile-time dead; see
   ! design_kernel_etauv notes).
   ! ----------------------------------------------------------------
   subroutine bc_exchange_dispersion(this, grid, gamma2, ws, ut, vt, etax, etay)
      use model_kernel_dispersion_mod, only: type_disp_workspace
      class(type_model_bc), intent(in) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: gamma2
      type(type_disp_workspace), intent(inout) :: ws
      real(SP), intent(inout) :: ut(:, :), vt(:, :)
      real(SP), intent(inout) :: etax(:, :), etay(:, :)

      call exchange_one(this, grid, ws%uxx, SIGN_ANTI, SIGN_MIRROR)
      call exchange_one(this, grid, ws%duxx, SIGN_ANTI, SIGN_MIRROR)
      call exchange_one(this, grid, ws%vyy, SIGN_MIRROR, SIGN_ANTI)
      call exchange_one(this, grid, ws%dvyy, SIGN_MIRROR, SIGN_ANTI)

      call exchange_one(this, grid, ws%uxy, SIGN_MIRROR, SIGN_MIRROR)
      call exchange_one(this, grid, ws%duxy, SIGN_MIRROR, SIGN_MIRROR)
      call exchange_one(this, grid, ws%vxy, SIGN_MIRROR, SIGN_MIRROR)
      call exchange_one(this, grid, ws%dvxy, SIGN_MIRROR, SIGN_MIRROR)

      if (gamma2 > 0.0_SP) then
         call exchange_one(this, grid, ut, SIGN_ANTI, SIGN_MIRROR)
         call exchange_one(this, grid, vt, SIGN_MIRROR, SIGN_ANTI)

         call exchange_one(this, grid, ws%utx, SIGN_MIRROR, SIGN_MIRROR)
         call exchange_one(this, grid, ws%vty, SIGN_MIRROR, SIGN_MIRROR)

         call exchange_one(this, grid, ws%utxx, SIGN_ANTI, SIGN_MIRROR)
         call exchange_one(this, grid, ws%vtyy, SIGN_MIRROR, SIGN_ANTI)

         call exchange_one(this, grid, ws%utxy, SIGN_MIRROR, SIGN_MIRROR)
         call exchange_one(this, grid, ws%vtxy, SIGN_MIRROR, SIGN_MIRROR)

         call exchange_one(this, grid, ws%dutxx, SIGN_ANTI, SIGN_MIRROR)
         call exchange_one(this, grid, ws%dvtyy, SIGN_MIRROR, SIGN_ANTI)

         call exchange_one(this, grid, ws%dutxy, SIGN_MIRROR, SIGN_MIRROR)
         call exchange_one(this, grid, ws%dvtxy, SIGN_MIRROR, SIGN_MIRROR)

         call exchange_one(this, grid, ws%ux, SIGN_MIRROR, SIGN_MIRROR)
         call exchange_one(this, grid, ws%dux, SIGN_MIRROR, SIGN_MIRROR)
         call exchange_one(this, grid, ws%vy, SIGN_MIRROR, SIGN_MIRROR)
         call exchange_one(this, grid, ws%dvy, SIGN_MIRROR, SIGN_MIRROR)

         ! legacy also exchanges DUy/DVx — no modern counterpart is
         ! computed or consumed (u2/u3 use dux/dvy only)
         call exchange_one(this, grid, ws%uy, SIGN_MIRROR, SIGN_ANTI)
         call exchange_one(this, grid, ws%vx, SIGN_ANTI, SIGN_MIRROR)

         call exchange_one(this, grid, etax, SIGN_ANTI, SIGN_MIRROR)
         call exchange_one(this, grid, etay, SIGN_MIRROR, SIGN_ANTI)
      end if

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
