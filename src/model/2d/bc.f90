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
   use core_constants_mod, only: SP, N_GHOST
   use core_grid_mod, only: type_grid_2d, type_halo_field
   use model_fields_2d_mod, only: type_fields_2d
   use model_kernel_bc_mod, only: fill_ghost_wall, SIGN_MIRROR, SIGN_ANTI
   implicit none

   private
   public :: type_model_bc, type_halo_batch

   ! Growable exchange list: fields append via add() and all ride one
   ! packed message per neighbor per phase at exchange_batch().  Owner
   ! modules (foam, sediment, tracer) append to bc%batch alongside the
   ! state fields instead of issuing their own per-field exchanges.
   ! add() stores a POINTER to the field — actuals must carry the
   ! target attribute and outlive the flush.
   type :: type_halo_batch
      type(type_halo_field), allocatable :: fld(:)
      real(SP), allocatable :: sx(:), sy(:)
      integer :: n = 0
   contains
      procedure :: add => batch_add
      procedure :: reset => batch_reset
   end type type_halo_batch

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
      ! Shared exchange list — exchange_batch() flushes and resets it
      type(type_halo_batch) :: batch
      ! Persistent real mirror of the wet/dry mask (integer fields
      ! can't ride the SP batch).  Only the 2*N_GHOST frame is kept
      ! current — the exchange never reads deeper, so the bulk sits
      ! stale by design
      real(SP), allocatable :: rmask(:, :)
   contains
      procedure :: init => bc_init
      procedure :: exchange_state => bc_exchange_state
      procedure :: exchange_dispersion => bc_exchange_dispersion
      procedure :: exchange_scalar => bc_exchange_scalar
      procedure :: exchange_batch => bc_exchange_batch
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

      if (allocated(this%rmask)) deallocate (this%rmask)
      allocate (this%rmask(grid%lp%mloc, grid%lp%nloc), source=0.0_SP)

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
      class(type_model_bc), intent(inout), target :: this
      type(type_grid_2d), intent(in) :: grid
      type(type_fields_2d), intent(inout), target :: fields

      integer :: ml, nl, ng

      ml = grid%lp%mloc
      nl = grid%lp%nloc
      ng = N_GHOST

      ! Refreshing the 2*ng frame of the mask mirror: the pack strips
      ! are ghost-inclusive, so the frame is exactly the read set —
      ! whole-array int->real 4x/step was pure overhead
      this%rmask(1:2*ng, :) = real(fields%mask(1:2*ng, :), SP)
      this%rmask(ml - 2*ng + 1:ml, :) = real(fields%mask(ml - 2*ng + 1:ml, :), SP)
      this%rmask(:, 1:2*ng) = real(fields%mask(:, 1:2*ng), SP)
      this%rmask(:, nl - 2*ng + 1:nl) = real(fields%mask(:, nl - 2*ng + 1:nl), SP)

      ! One packed message per neighbor per phase for the whole state
      ! (fields are independent during exchange — values identical to
      ! the per-field sequence); wall fills stay per-field in the flush
      call this%batch%add(fields%eta, SIGN_MIRROR, SIGN_MIRROR)
      call this%batch%add(this%rmask, SIGN_MIRROR, SIGN_MIRROR)
      call this%batch%add(fields%u, SIGN_ANTI, SIGN_MIRROR)
      call this%batch%add(fields%p, SIGN_ANTI, SIGN_MIRROR)
      call this%batch%add(fields%hu, SIGN_ANTI, SIGN_MIRROR)
      call this%batch%add(fields%v, SIGN_MIRROR, SIGN_ANTI)
      call this%batch%add(fields%q, SIGN_MIRROR, SIGN_ANTI)
      call this%batch%add(fields%hv, SIGN_MIRROR, SIGN_ANTI)
      if (this%exch_age) call this%batch%add(fields%age_break, SIGN_MIRROR, SIGN_MIRROR)
      if (this%exch_nu) call this%batch%add(fields%nu_break, SIGN_MIRROR, SIGN_MIRROR)

      call this%exchange_batch(grid)

      ! Exchange and wall fills write ghosts only, so only the ghost
      ! bands carry news; a face nothing wrote (wavemaker-owned west)
      ! writes back its own refresh = identity, as the whole-array
      ! nint did
      fields%mask(1:ng, :) = nint(this%rmask(1:ng, :))
      fields%mask(ml - ng + 1:ml, :) = nint(this%rmask(ml - ng + 1:ml, :))
      fields%mask(:, 1:ng) = nint(this%rmask(:, 1:ng))
      fields%mask(:, nl - ng + 1:nl) = nint(this%rmask(:, nl - ng + 1:nl))

      fields%u = fields%u*fields%mask
      fields%v = fields%v*fields%mask
      fields%hu = fields%hu*fields%mask
      fields%hv = fields%hv*fields%mask

   end subroutine bc_exchange_state

   ! ----------------------------------------------------------------
   ! Ghost update for the dispersion COMPONENT arrays (legacy
   ! EXCHANGE_DISPERSION): each raw derivative mirrors with its own
   ! parity — u-like (anti-x), v-like (anti-y), or scalar-like (mirror
   ! both) — and u4/v4 are then ASSEMBLED ghost-inclusive by
   ! cal_dispersion_assemble.  Mirroring u4/v4 directly is wrong
   ! wherever $V_{xy} \ne 0$ (the components mix parities) and flips
   ! MUSCL limiter branches through the sign of zero even on quiescent
   ! fields (parity ledger 8c/8e/9 root).
   ! Legacy exchanges 26 arrays here; only 10 have readable ghosts —
   ! the 8 second-derivative components (assemble reads them over the
   ! full array and at (i±1, j±1) in the gamma2 stencil) and etax/etay.
   ! The 16 gamma2 workspace derivatives plus Ut/Vt are consumed at
   ! (i, j) interior-only everywhere (assemble, etauv tridiag
   ! coefficients), so their exchanges are dropped — ghost values
   ! unread, output bitwise-identical (perf audit item 1).
   ! Wavemaker-owned west face: fills skipped via fill_west (legacy
   ! PHI_COLL exemption) — the workspace ghosts stay zeroed, which
   ! reproduces legacy's never-written ghosts there.
   ! Only the disp_time_left = .false. gamma2 chain is ported (the
   ! legacy DISP_TIME_LEFT path is compile-time dead; see
   ! design_kernel_etauv notes).
   ! ----------------------------------------------------------------
   subroutine bc_exchange_dispersion(this, grid, gamma2, ws, etax, etay)
      use model_kernel_dispersion_mod, only: type_disp_workspace
      class(type_model_bc), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: gamma2
      type(type_disp_workspace), intent(inout), target :: ws
      real(SP), intent(inout), target :: etax(:, :), etay(:, :)

      call this%batch%add(ws%uxx, SIGN_ANTI, SIGN_MIRROR)
      call this%batch%add(ws%duxx, SIGN_ANTI, SIGN_MIRROR)
      call this%batch%add(ws%vyy, SIGN_MIRROR, SIGN_ANTI)
      call this%batch%add(ws%dvyy, SIGN_MIRROR, SIGN_ANTI)
      call this%batch%add(ws%uxy, SIGN_MIRROR, SIGN_MIRROR)
      call this%batch%add(ws%duxy, SIGN_MIRROR, SIGN_MIRROR)
      call this%batch%add(ws%vxy, SIGN_MIRROR, SIGN_MIRROR)
      call this%batch%add(ws%dvxy, SIGN_MIRROR, SIGN_MIRROR)
      if (gamma2 > 0.0_SP) then
         call this%batch%add(etax, SIGN_ANTI, SIGN_MIRROR)
         call this%batch%add(etay, SIGN_MIRROR, SIGN_ANTI)
      end if

      call this%exchange_batch(grid)

   end subroutine bc_exchange_dispersion

   ! Halo + scalar mirror for a field an optional module owns (legacy PHI_COLL
   ! with VTYPE=1); the vessel propeller jet is the first caller.
   subroutine bc_exchange_scalar(this, grid, f)
      class(type_model_bc), intent(in) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: f(:, :)

      call exchange_one(this, grid, f, SIGN_MIRROR, SIGN_MIRROR)

   end subroutine bc_exchange_scalar

   ! Flush the shared exchange list: one batched halo exchange, then
   ! the per-field wall fills, then reset for the next builder.
   subroutine bc_exchange_batch(this, grid)
      class(type_model_bc), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      integer :: n

      if (this%batch%n == 0) return
      call grid%halo_exchange_batch(this%batch%fld(1:this%batch%n))
      do n = 1, this%batch%n
         call wall_one(this, grid, this%batch%fld(n)%f, &
                       this%batch%sx(n), this%batch%sy(n))
      end do
      call this%batch%reset()

   end subroutine bc_exchange_batch

   subroutine batch_add(this, f, sign_x, sign_y)
      class(type_halo_batch), intent(inout) :: this
      real(SP), intent(inout), target :: f(:, :)
      real(SP), intent(in) :: sign_x, sign_y

      type(type_halo_field), allocatable :: tf(:)
      real(SP), allocatable :: ts(:)
      integer :: cap

      if (.not. allocated(this%fld)) then
         allocate (this%fld(16), this%sx(16), this%sy(16))
      else if (this%n == size(this%fld)) then
         cap = 2*size(this%fld)
         allocate (tf(cap)); tf(1:this%n) = this%fld(1:this%n)
         call move_alloc(tf, this%fld)
         allocate (ts(cap)); ts(1:this%n) = this%sx(1:this%n)
         call move_alloc(ts, this%sx)
         allocate (ts(cap)); ts(1:this%n) = this%sy(1:this%n)
         call move_alloc(ts, this%sy)
      end if

      this%n = this%n + 1
      this%fld(this%n)%f => f
      this%sx(this%n) = sign_x
      this%sy(this%n) = sign_y

   end subroutine batch_add

   subroutine batch_reset(this)
      class(type_halo_batch), intent(inout) :: this

      integer :: n

      ! Drop the field pointers (stale targets must not linger); keep
      ! the capacity
      do n = 1, this%n
         this%fld(n)%f => null()
      end do
      this%n = 0

   end subroutine batch_reset

   subroutine exchange_one(this, grid, f, sign_x, sign_y)
      class(type_model_bc), intent(in) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: f(:, :)
      real(SP), intent(in) :: sign_x, sign_y

      call grid%halo_exchange(f)
      call wall_one(this, grid, f, sign_x, sign_y)

   end subroutine exchange_one

   ! Physical wall fills + corner repair for one exchanged field — the
   ! local half of exchange_one, shared by the batched paths
   subroutine wall_one(this, grid, f, sign_x, sign_y)
      class(type_model_bc), intent(in) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: f(:, :)
      real(SP), intent(in) :: sign_x, sign_y

      integer :: j, k, ng

      call fill_ghost_wall(grid%lp, this%fill_west, this%fill_east, &
                           this%fill_south, this%fill_north, sign_x, sign_y, f)

      ! Corner repair (ledger 18b/18c class): the wall mirror sweeps
      ! interior rows only, while halo phase 2 shipped the PRE-mirror
      ! x-ghost columns into the y-ghost rows — so the corner blocks
      ! held one-exchange-behind history.  Re-mirroring x-walls over
      ! ALL rows rewrites interior rows bitwise-identical and leaves
      ! corners = mirror(wrap(interior)), a pure function of the
      ! interior (checkpoint-restart reproducible).
      associate (lp => grid%lp)
         ng = lp%ib - 1
         if (this%fill_west) then
            do j = 1, lp%nloc
               do k = 1, ng
                  f(k, j) = sign_x*f(2*ng + 1 - k, j)
               end do
            end do
         end if
         if (this%fill_east) then
            do j = 1, lp%nloc
               do k = 1, ng
                  f(lp%ie + k, j) = sign_x*f(lp%ie - k + 1, j)
               end do
            end do
         end if
      end associate

   end subroutine wall_one

end module model_bc_mod
