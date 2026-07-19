!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Sponge layer physics compute (per-face configuration).
!
!  Three sponge types, now independently per face:
!    direct     -- Larsen-Dancy (1983) post-step damping of eta/p/q.
!    friction   -- cd-based momentum drag term in the RHS (sources.f90).
!    diffusion  -- nu-based lateral viscosity term in the RHS (sources.f90).
!
!  Configuration arrives from the boundaries: section reader
!  (model_boundaries_mod) — this module owns no YAML read since the
!  config reorg (rung 2).  Faces are indexed FACE_W/E/S/N.
!
!  NOTE 1: corner overlap between two face strips keeps the legacy
!    last-write/max-combine behaviour; a deliberate blending policy is
!    deferred (validation track).
!  NOTE 2: the direct-sponge decay floor is per-face; at a corner the
!    winning face's floor applies (identical to legacy when all faces
!    share coefficients).
!
!  Only the direct sponge has a standalone apply() here.
!  friction/diffusion coefficient arrays are initialised in init_compute() so
!  sources.f90 can reference them; their apply is deferred to that refactor.
!
!  HISTORY :
!    05/13/2026  Michael-Angelo Y.H. Lam  (read_input)
!    05/28/2026  Michael-Angelo Y.H. Lam  (init_compute, merge_friction, apply)
!    07/16/2026  Michael-Angelo Y.H. Lam  (per-face config, reader moved to
!                                          boundaries)
!
!-------------------------------------------------

module model_sponge_mod
   use core_constants_mod, only: SP, N_GHOST
   use core_env_mod, only: type_env
   use core_grid_mod, only: type_grid_2d
   use model_base_mod, only: type_model_base
   use model_fields_2d_mod, only: type_fields_2d

   implicit none

   private
   public :: type_model_sponge
   public :: FACE_W, FACE_E, FACE_S, FACE_N

   ! face indices shared with the boundaries reader
   integer, parameter :: FACE_W = 1, FACE_E = 2, FACE_S = 3, FACE_N = 4

   type, extends(type_model_base) :: type_model_sponge

      ! ── Per-face configuration (set by model_boundaries_mod) ──────
      ! width(f) > 0 with no type flag set is rejected at read time,
      ! so the flags alone gate the compute passes.
      real(SP) :: width(4) = 0.0_SP

      logical  :: direct_on(4) = .false.
      logical  :: friction_on(4) = .false.
      logical  :: diffusion_on(4) = .false.

      real(SP) :: r_direct(4) = 0.85_SP    ! nee R_sponge
      real(SP) :: a_direct(4) = 5.0_SP     ! nee A_sponge
      real(SP) :: cd_fric(4) = 0.0_SP      ! nee CDsponge
      real(SP) :: nu_diff(4) = 0.1_SP      ! nee Csp

      ! ── Computed state ────────────────────────────────────────────
      ! Ghost-inclusive arrays: (local_nx+2*N_GHOST, local_ny+2*N_GHOST).
      ! coeff:     direct sponge damping ratio (>= 1; 1.0 = no damping).
      ! cd_sponge: friction sponge drag coefficient (>= 0).
      ! nu_sponge: diffusion sponge kinematic viscosity (>= 0).
      real(SP), allocatable :: coeff(:, :)
      real(SP), allocatable :: cd_sponge(:, :)
      real(SP), allocatable :: nu_sponge(:, :)

   contains
      procedure :: read_input => sponge_read_input
      procedure :: any_direct => sponge_any_direct
      procedure :: any_friction => sponge_any_friction
      procedure :: any_diffusion => sponge_any_diffusion
      procedure :: init_compute => sponge_init_compute
      procedure :: merge_friction => sponge_merge_friction
      procedure :: apply => sponge_apply
      procedure :: free => sponge_free
   end type type_model_sponge

contains

   !> No-op satisfying the deferred base binding — configuration arrives
   !> from the boundaries: reader (model_boundaries_mod)
   subroutine sponge_read_input(this, env)
      class(type_model_sponge), intent(inout) :: this
      type(type_env), intent(inout), target :: env
   end subroutine sponge_read_input

   ! ── Type queries (per-face flags folded for the engine gates) ─────────────

   pure logical function sponge_any_direct(this)
      class(type_model_sponge), intent(in) :: this
      sponge_any_direct = any(this%direct_on)
   end function sponge_any_direct

   pure logical function sponge_any_friction(this)
      class(type_model_sponge), intent(in) :: this
      sponge_any_friction = any(this%friction_on)
   end function sponge_any_friction

   pure logical function sponge_any_diffusion(this)
      class(type_model_sponge), intent(in) :: this
      sponge_any_diffusion = any(this%diffusion_on)
   end function sponge_any_diffusion

   ! ── Initialisation ────────────────────────────────────────────────────────

   !> Allocate and compute sponge coefficient arrays.
   !>
   !> Direct sponge (coeff):    Larsen-Dancy exponential damping ratio.
   !> Friction sponge (cd):     tanh-profile drag coefficient; allocates cd_sponge.
   !> Diffusion sponge (nu):    tanh-profile lateral viscosity.
   !>
   !> All arrays ghost-inclusive.  Called once at init_compute time, before
   !> the first timestep and after grid%setup() + grid%init_spacing() have run.
   !> To max-merge friction sponge into an external Cd array instead, call
   !> merge_friction() after init_compute().
   subroutine sponge_init_compute(this, grid)
      class(type_model_sponge), intent(inout) :: this
      type(type_grid_2d), intent(in)    :: grid

      integer  :: ng, nx, ny, mloc_g, nloc_g
      real(SP) :: ref_dx, ref_dy
      real(SP) :: w(4)

      if (.not. this%is_activated) return

      ng = N_GHOST
      nx = grid%local_nx
      ny = grid%local_ny
      mloc_g = nx + 2*ng
      nloc_g = ny + 2*ng

      ref_dx = grid%dx0
      ref_dy = grid%dy0

      call this%free()

      if (this%any_direct()) then
         w = merge(this%width, [0.0_SP, 0.0_SP, 0.0_SP, 0.0_SP], this%direct_on)
         allocate (this%coeff(mloc_g, nloc_g), source=1.0_SP)
         call compute_direct_coeff(this%coeff, mloc_g, nloc_g, ng, ref_dx, ref_dy, &
                                   w, this%r_direct, this%a_direct, &
                                   grid%ibegin, grid%iproc, grid%nx_proc, nx, &
                                   grid%jbegin, grid%jproc, grid%ny_proc, ny)
      end if

      if (this%any_friction()) then
         w = merge(this%width, [0.0_SP, 0.0_SP, 0.0_SP, 0.0_SP], this%friction_on)
         allocate (this%cd_sponge(mloc_g, nloc_g), source=0.0_SP)
         call compute_ramp_coeff(this%cd_sponge, mloc_g, nloc_g, ng, ref_dx, ref_dy, &
                                 w, this%cd_fric, &
                                 grid%ibegin, grid%iproc, grid%nx_proc, nx, &
                                 grid%jbegin, grid%jproc, grid%ny_proc, ny)
      end if

      if (this%any_diffusion()) then
         w = merge(this%width, [0.0_SP, 0.0_SP, 0.0_SP, 0.0_SP], this%diffusion_on)
         allocate (this%nu_sponge(mloc_g, nloc_g), source=0.0_SP)
         call compute_ramp_coeff(this%nu_sponge, mloc_g, nloc_g, ng, ref_dx, ref_dy, &
                                 w, this%nu_diff, &
                                 grid%ibegin, grid%iproc, grid%nx_proc, nx, &
                                 grid%jbegin, grid%jproc, grid%ny_proc, ny)
      end if

   end subroutine sponge_init_compute

   !> Add the friction sponge profile into an external drag-base array.
   !>
   !> Must be called after init_compute().  When a friction sponge is active,
   !> adds cd_sponge*depth into cd_inout and then deallocates cd_sponge.
   !> No-op when no friction sponge is active or not allocated.
   !>
   !> The *depth factor converts the flux-based sponge drag to the same units
   !> as the velocity-based friction term in cal_sources:
   !>   legacy: -CD_4_SPONGE * U * |UV| * Depth  (Wei et al. flux form)
   !>   merged: -(CD_4_SPONGE * Depth) * U * |UV|  (same as Cd * U * |UV|)
   !> Composition with a constant/Manning drag is additive (drag terms sum),
   !> superseding the earlier max()-merge (legacy ledger 8d).
   !>
   !> Merge into friction's constant base so both survive the per-step Manning
   !> rebuild, then sync_base pushes it into the effective Cd:
   !>   call sponge%init_compute(grid)
   !>   call sponge%merge_friction(friction%cd_base, fields%depth)
   !>   call friction%sync_base()
   subroutine sponge_merge_friction(this, cd_inout, depth)
      class(type_model_sponge), intent(inout) :: this
      real(SP), intent(inout) :: cd_inout(:, :)
      real(SP), intent(in)    :: depth(:, :)

      integer :: i, j

      if (.not. this%any_friction()) return
      if (.not. allocated(this%cd_sponge)) return

      do j = 1, size(cd_inout, 2)
         do i = 1, size(cd_inout, 1)
            cd_inout(i, j) = cd_inout(i, j) + this%cd_sponge(i, j)*depth(i, j)
         end do
      end do

      deallocate (this%cd_sponge)

   end subroutine sponge_merge_friction

   ! ── Apply (direct sponge only) ────────────────────────────────────────────

   !> Apply Larsen-Dancy (1983) per-stage damping to the state
   !> (legacy SPONGE_DAMPING, old/sponge.F).
   !>
   !> Divides eta/u/v by coeff(i,j) at every ghost-inclusive cell.
   !> eta is only damped at wet cells (mask > 0); u/v always.  Legacy
   !> damps the velocities, NOT the conserved Ubar/Vbar — the damped
   !> u/v feed the next stage's fluxes/sources while the tridiagonal
   !> solves rebuild u/v from the undamped Ubar/Vbar.  Kept as is.
   !>
   !> Friction and diffusion sponge terms are RHS source contributions;
   !> their apply is in sources.f90 (refactor TODO).
   subroutine sponge_apply(this, fields, grid)
      class(type_model_sponge), intent(in)    :: this
      type(type_fields_2d), intent(inout) :: fields
      type(type_grid_2d), intent(in)    :: grid

      integer :: i, j, mloc_g, nloc_g, ng

      if (.not. this%is_activated) return
      if (.not. this%any_direct()) return

      ng = N_GHOST
      mloc_g = grid%local_nx + 2*ng
      nloc_g = grid%local_ny + 2*ng

      do j = 1, nloc_g
         do i = 1, mloc_g
            if (fields%mask(i, j) > 0) &
               fields%eta(i, j) = fields%eta(i, j)/this%coeff(i, j)
            fields%u(i, j) = fields%u(i, j)/this%coeff(i, j)
            fields%v(i, j) = fields%v(i, j)/this%coeff(i, j)
         end do
      end do

   end subroutine sponge_apply

   ! ── Teardown ──────────────────────────────────────────────────────────────

   subroutine sponge_free(this)
      class(type_model_sponge), intent(inout) :: this
      if (allocated(this%coeff)) deallocate (this%coeff)
      if (allocated(this%cd_sponge)) deallocate (this%cd_sponge)
      if (allocated(this%nu_sponge)) deallocate (this%nu_sponge)
   end subroutine sponge_free

   ! ── Private coefficient computation ───────────────────────────────────────

   !> Direct (Larsen-Dancy) sponge coefficient, per-face amplitudes.
   !> Exponential profile: coeff >= 1; 1 = no damping.
   !> dim1 = local_nx + 2*N_GHOST, dim2 = local_ny + 2*N_GHOST.
   !> Values below the winning face's decay floor a**(r**50) are reset to 1
   !> (NOTE 2 in the module header).
   !>
   !> Bug-fix vs. legacy: each axis is combined independently so that a west-only
   !> sponge is not silently zeroed by the empty south/north combine pass.
   subroutine compute_direct_coeff(coeff, dim1, dim2, ng, ref_dx, ref_dy, &
                                   width, r_sp, a_sp, &
                                   ibegin, iproc, nx_proc, local_nx, &
                                   jbegin, jproc, ny_proc, local_ny)
      integer, intent(in)    :: dim1, dim2, ng
      real(SP), intent(inout) :: coeff(dim1, dim2)
      real(SP), intent(in)    :: ref_dx, ref_dy
      real(SP), intent(in)    :: width(4), r_sp(4), a_sp(4)
      integer, intent(in)    :: ibegin, iproc, nx_proc, local_nx
      integer, intent(in)    :: jbegin, jproc, ny_proc, local_ny

      real(SP), allocatable :: tmp1(:, :), tmp2(:, :)
      real(SP) :: ri, lim, floor1, floor2, floor_here
      integer  :: i, j, iwidth

      allocate (tmp1(dim1, dim2), tmp2(dim1, dim2))

      ! ── west / east — combine only when at least one side is active ──
      if (width(FACE_W) > 0.0_SP .or. width(FACE_E) > 0.0_SP) then
         tmp1 = 0.0_SP
         tmp2 = 0.0_SP
         floor1 = a_sp(FACE_W)**(r_sp(FACE_W)**50)
         floor2 = a_sp(FACE_E)**(r_sp(FACE_E)**50)

         if (width(FACE_W) > 0.0_SP) then
            iwidth = int(width(FACE_W)/ref_dx) + ng
            do j = 1, dim2
               do i = 1, dim1
                  lim = max(coeff(i, j), 1.0_SP)
                  ri = r_sp(FACE_W)**(50.0_SP*real(i + ibegin - 2, SP)/real(iwidth - 1, SP))
                  tmp1(i, j) = max(a_sp(FACE_W)**ri, lim)
               end do
            end do
         end if

         if (width(FACE_E) > 0.0_SP) then
            iwidth = int(width(FACE_E)/ref_dx) + ng
            do j = 1, dim2
               do i = 1, dim1
                  lim = max(coeff(i, j), 1.0_SP)
                  ri = r_sp(FACE_E)**(50.0_SP*real(dim1 - i + (nx_proc - iproc - 1)*local_nx, SP) &
                                      /real(iwidth - 1, SP))
                  tmp2(i, j) = max(a_sp(FACE_E)**ri, lim)
               end do
            end do
         end if

         do j = 1, dim2
            do i = 1, dim1
               floor_here = merge(floor1, floor2, tmp1(i, j) >= tmp2(i, j))
               coeff(i, j) = max(tmp1(i, j), tmp2(i, j))
               if (coeff(i, j) < floor_here) coeff(i, j) = 1.0_SP
            end do
         end do
      end if

      ! ── south / north — combine only when at least one side is active ──
      if (width(FACE_S) > 0.0_SP .or. width(FACE_N) > 0.0_SP) then
         tmp1 = 0.0_SP
         tmp2 = 0.0_SP
         floor1 = a_sp(FACE_S)**(r_sp(FACE_S)**50)
         floor2 = a_sp(FACE_N)**(r_sp(FACE_N)**50)

         if (width(FACE_S) > 0.0_SP) then
            iwidth = int(width(FACE_S)/ref_dy) + ng
            do i = 1, dim1
               do j = 1, dim2
                  lim = max(coeff(i, j), 1.0_SP)
                  ri = r_sp(FACE_S)**(50.0_SP*real(j + jbegin - 2, SP)/real(iwidth - 1, SP))
                  tmp1(i, j) = max(a_sp(FACE_S)**ri, lim)
               end do
            end do
         end if

         if (width(FACE_N) > 0.0_SP) then
            iwidth = int(width(FACE_N)/ref_dy) + ng
            do i = 1, dim1
               do j = 1, dim2
                  lim = max(coeff(i, j), 1.0_SP)
                  ri = r_sp(FACE_N)**(50.0_SP*real(dim2 - j + (ny_proc - jproc - 1)*local_ny, SP) &
                                      /real(iwidth - 1, SP))
                  tmp2(i, j) = max(a_sp(FACE_N)**ri, lim)
               end do
            end do
         end if

         do j = 1, dim2
            do i = 1, dim1
               floor_here = merge(floor1, floor2, tmp1(i, j) >= tmp2(i, j))
               coeff(i, j) = max(tmp1(i, j), tmp2(i, j))
               if (coeff(i, j) < floor_here) coeff(i, j) = 1.0_SP
            end do
         end do
      end if

      deallocate (tmp1, tmp2)
   end subroutine compute_direct_coeff

   !> Friction/diffusion sponge ramp coefficient (>= 0), per-face amplitudes.
   !> Linear ramp via tanh profile.  No floor reset — zero outside sponge is
   !> correct.  Serves both cd (amp = cd_fric) and nu (amp = nu_diff); the two
   !> profiles were always identical (nee compute_friction/diffusion_coeff).
   subroutine compute_ramp_coeff(cd, dim1, dim2, ng, ref_dx, ref_dy, &
                                 width, amp, &
                                 ibegin, iproc, nx_proc, local_nx, &
                                 jbegin, jproc, ny_proc, local_ny)
      integer, intent(in)    :: dim1, dim2, ng
      real(SP), intent(inout) :: cd(dim1, dim2)
      real(SP), intent(in)    :: ref_dx, ref_dy
      real(SP), intent(in)    :: width(4), amp(4)
      integer, intent(in)    :: ibegin, iproc, nx_proc, local_nx
      integer, intent(in)    :: jbegin, jproc, ny_proc, local_ny

      real(SP), allocatable :: tmp1(:, :), tmp2(:, :)
      real(SP) :: ri, lim
      integer  :: i, j, iwidth

      allocate (tmp1(dim1, dim2), tmp2(dim1, dim2))

      if (width(FACE_W) > 0.0_SP .or. width(FACE_E) > 0.0_SP) then
         tmp1 = 0.0_SP
         tmp2 = 0.0_SP

         if (width(FACE_W) > 0.0_SP) then
            iwidth = int(width(FACE_W)/ref_dx) + ng
            do j = 1, dim2
               do i = 1, dim1
                  lim = max(cd(i, j), 0.0_SP)
                  ri = max(0.0_SP, real(iwidth - i - (ibegin - 1), SP))
                  tmp1(i, j) = max(amp(FACE_W)*tanh(ri/10.0_SP), lim)
               end do
            end do
         end if

         if (width(FACE_E) > 0.0_SP) then
            iwidth = int(width(FACE_E)/ref_dx) + ng
            do j = 1, dim2
               do i = 1, dim1
                  lim = max(cd(i, j), 0.0_SP)
                  ri = max(0.0_SP, real(iwidth - dim1 + i - (nx_proc - iproc - 1)*local_nx, SP))
                  tmp2(i, j) = max(amp(FACE_E)*tanh(ri/10.0_SP), lim)
               end do
            end do
         end if

         do j = 1, dim2
            do i = 1, dim1
               cd(i, j) = max(tmp1(i, j), tmp2(i, j))
            end do
         end do
      end if

      if (width(FACE_S) > 0.0_SP .or. width(FACE_N) > 0.0_SP) then
         tmp1 = 0.0_SP
         tmp2 = 0.0_SP

         if (width(FACE_S) > 0.0_SP) then
            iwidth = int(width(FACE_S)/ref_dy) + ng
            do i = 1, dim1
               do j = 1, dim2
                  lim = max(cd(i, j), 0.0_SP)
                  ri = max(0.0_SP, real(iwidth - j - (jbegin - 1), SP))
                  tmp1(i, j) = max(amp(FACE_S)*tanh(ri/10.0_SP), lim)
               end do
            end do
         end if

         if (width(FACE_N) > 0.0_SP) then
            iwidth = int(width(FACE_N)/ref_dy) + ng
            do i = 1, dim1
               do j = 1, dim2
                  lim = max(cd(i, j), 0.0_SP)
                  ri = max(0.0_SP, real(iwidth - dim2 + j - (ny_proc - jproc - 1)*local_ny, SP))
                  tmp2(i, j) = max(amp(FACE_N)*tanh(ri/10.0_SP), lim)
               end do
            end do
         end if

         do j = 1, dim2
            do i = 1, dim1
               cd(i, j) = max(tmp1(i, j), tmp2(i, j))
            end do
         end do
      end if

      deallocate (tmp1, tmp2)
   end subroutine compute_ramp_coeff

end module model_sponge_mod
