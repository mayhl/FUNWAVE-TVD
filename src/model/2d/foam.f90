!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Surface foam transport (legacy mod_foam.F, MOD_FOAM)
!
!  A passive surface-foam layer driven by the breaker: whitecap mass is
!  injected where the breaking eddy viscosity is nonzero, decays on a
!  burst time scale set by the breaking age, and is advected by its own
!  depth-averaged foam velocity, which is dragged towards the surface
!  water velocity.  Reul and Chapron (2003) source/sink form.
!
!  ONE-WAY: foam reads eta/u/v/nu_break/age_break and writes only its own
!  state.  Nothing in the hydrodynamics reads it back, so a foam-enabled
!  build must reproduce a foam-free run BITWISE (the parity check below).
!
!  YAML block: foam:                 (top-level; omit for no foam)
!    source_coef:    <real>  default 0.05   whitecap injection coefficient
!                                           (nee f_source)
!    time_scale:     <real>  default 3.8    burst/decay time scale   [s]
!                                           (nee FoamTimeScale)
!    burst_time_non_breaking: <real> default 1.0  age used off the
!                                           breakers [s] (nee BurstTimeNonBreaking)
!    min_thickness:  <real>  default 0.01   drag thickness floor      [m]
!                                           (nee MinThick)
!    cd:             <real>  default 0.5    foam-water drag coefficient
!                                           (nee CdFoam)
!
!  Legacy call shape: ALLOCATE_FOAM + INITIALIZATION_FOAM from init;
!  FOAM_FLUX -> FOAM_UPDATE -> FOAM_BC every RK stage, between
!  WAVE_BREAKING and EXCHANGE.
!
!  Bug-for-bug notes vs legacy:
!    1. FIXED (cord cut): foam is a one-way diagnostic, so the stepper
!       calls update() ONCE per timestep from its post_step hook (beside
!       the trackers) with the full step dt.  Legacy called it every RK
!       stage with the full dt, advancing 3 dt per timestep — the foam
!       clock ran 3x fast against the wave clock
!    2. NOTE: the flux stage reads the ghost eta_foam/u_foam/v_foam left
!       by the PREVIOUS stage's bc (halo + wall), so foam is always one
!       stage behind at the halo, exactly as legacy
!    3. NOTE: the non-breaking test is an exact float equality against
!       zero, nu_break == 0.  It works only because nu_bkg defaults to 0;
!       any nonzero nu_bkg makes the breaker write nu_bkg into EVERY cell
!       and this branch goes dead, silently.  Ported verbatim
!    4. NOTE: MaskFoam is allocated, set to 1 and never changed —
!       UPDATE_FOAM_MASK is dead code, legacy's own comment says "useless
!       so far".  Folded away (it multiplied the source by 1)
!    5. NOTE: legacy PLOT_INTV_FOAM drives OUTPUT_FOAM, whose body is an
!       empty stub ("time series here") — the key is DROPPED, rejected
!       loudly.  The real foam output is the FoamEta_ field dump in
!       PREVIEW, which is unconditional under -DFOAM
!    6. NOTE: BurstRate/TransferRate and the whole "old approach" branch
!       sit behind USE_BURSTRATE, which no build system defines.  Not
!       ported.  Usurf1/2, Vsurf1/2, VFsurf1/2 and DepthFoam are
!       allocated, zeroed and never read.  Not ported
!    7. NOTE: the wall zeroing of mx_foam/my_foam is outside the update
!       stencil (the residual reads faces ib..ie+1 only), so it is inert;
!       the eta_foam wall zeroing is the one that bites.  Both ported, to
!       keep the boundary block legible against legacy FOAM_BC
!
!  HISTORY :
!    07/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_foam_mod
   use core_constants_mod, only: SP, LARGE, ZERO
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d, type_loop_bounds
   use model_base_mod, only: type_model_base
   use model_kernel_fluxes_mod, only: delx_fun, dely_fun, construct_x, construct_y

   use model_config_defaults_mod, only: DEF_FOAM_SOURCE_COEF, DEF_FOAM_TIME_SCALE, &
                                        DEF_FOAM_BURST_TIME_NON_BREAKING, &
                                        DEF_FOAM_MIN_THICKNESS, DEF_FOAM_CD

   implicit none

   private
   ! flux/advance are plain lp-driven kernels (no grid, no MPI): the stage
   ! order lives in update(), and the unit test drives them directly
   public :: type_model_foam, foam_flux, foam_advance

   type, extends(type_model_base) :: type_model_foam

      real(SP) :: f_source = 0.05_SP
      real(SP) :: time_scale = 3.8_SP    ! FoamTimeScale
      real(SP) :: burst_time_nb = 1.0_SP ! BurstTimeNonBreaking
      real(SP) :: min_thick = 0.01_SP
      real(SP) :: cd_foam = 0.5_SP

      ! foam state (ghost-inclusive); eta_foam_max is a running envelope
      real(SP), allocatable :: eta_foam(:, :), eta_foam_max(:, :)
      real(SP), allocatable :: u_foam(:, :), v_foam(:, :)

      ! interface fluxes, source/sink, and the reconstruction scratch
      real(SP), allocatable :: mx(:, :), my(:, :)
      real(SP), allocatable :: sink(:, :), source(:, :)
      real(SP), allocatable :: del(:, :), vxl(:, :), vxr(:, :), vyl(:, :), vyr(:, :)

   contains
      procedure :: read_input => foam_read_input
      procedure :: init_compute => foam_init_compute
      procedure :: update => foam_update
      procedure :: free => foam_free
   end type type_model_foam

contains

   subroutine foam_read_input(this, env)
      class(type_model_foam), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk, no_key
      real(SP) :: tmp_r

      sub_env = get_sub_env(env, "foam", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (no_blk) return

      ! dead legacy knob (NOTE 5): dropped, not parked
      call sub_env%yaml%read("PLOT_INTV_FOAM", silent=no_key, val=tmp_r)
      if (.not. no_key) call env%log%exit_on_error( &
         "foam: PLOT_INTV_FOAM dropped -- it drove an empty legacy stub writer")

      call sub_env%yaml%read("source_coef", silent=no_key, val=this%f_source, &
                             default=DEF_FOAM_SOURCE_COEF)
      call sub_env%yaml%read("time_scale", silent=no_key, val=this%time_scale, &
                             default=DEF_FOAM_TIME_SCALE)
      call sub_env%yaml%read("burst_time_non_breaking", silent=no_key, &
                             val=this%burst_time_nb, &
                             default=DEF_FOAM_BURST_TIME_NON_BREAKING)
      call sub_env%yaml%read("min_thickness", silent=no_key, val=this%min_thick, &
                             default=DEF_FOAM_MIN_THICKNESS)
      call sub_env%yaml%read("cd", silent=no_key, val=this%cd_foam, &
                             default=DEF_FOAM_CD)

   end subroutine foam_read_input

   ! Legacy ALLOCATE_FOAM + INITIALIZATION_FOAM: every foam array starts
   ! at zero (a still sea has no foam).
   subroutine foam_init_compute(this, grid)
      class(type_model_foam), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      if (.not. this%is_activated) return

      associate (m => grid%lp%mloc, n => grid%lp%nloc)
         allocate (this%eta_foam(m, n), source=ZERO)
         allocate (this%eta_foam_max(m, n), source=ZERO)
         allocate (this%u_foam(m, n), source=ZERO)
         allocate (this%v_foam(m, n), source=ZERO)

         allocate (this%mx(m + 1, n), source=ZERO)
         allocate (this%my(m, n + 1), source=ZERO)

         allocate (this%sink(m, n), source=ZERO)
         allocate (this%source(m, n), source=ZERO)

         allocate (this%del(m, n), source=ZERO)
         allocate (this%vxl(m + 1, n), source=ZERO)
         allocate (this%vxr(m + 1, n), source=ZERO)
         allocate (this%vyl(m, n + 1), source=ZERO)
         allocate (this%vyr(m, n + 1), source=ZERO)
      end associate

   end subroutine foam_init_compute

   ! ----------------------------------------------------------------
   ! One foam step: FOAM_FLUX -> FOAM_UPDATE -> FOAM_BC, in that order.
   ! Called ONCE per timestep from the stepper's post_step hook with the
   ! full step dt (header NOTE 1 — was per RK stage in legacy).
   ! ----------------------------------------------------------------
   ! dx/dy/inv_dx/inv_dy are the GHOST-INCLUSIVE spacing arrays the stepper
   ! owns (grid%dx is interior-only), same as the breaker takes.
   subroutine foam_update(this, grid, dt, dx, dy, inv_dx, inv_dy, &
                          u, v, nu_break, age_break)
      class(type_model_foam), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: dt
      real(SP), intent(in) :: dx(:, :), dy(:, :), inv_dx(:, :), inv_dy(:, :)
      real(SP), intent(in) :: u(:, :), v(:, :), nu_break(:, :), age_break(:, :)

      if (.not. this%is_activated) return

      call foam_flux(this, grid%lp, dx, dy, inv_dx, inv_dy)
      call foam_advance(this, grid%lp, dt, inv_dx, inv_dy, u, v, nu_break, age_break)
      call foam_bc(this, grid)

   end subroutine foam_update

   ! ----------------------------------------------------------------
   ! Legacy FOAM_FLUX: upwind foam mass flux, van Leer reconstructed.
   ! Both the transported quantity and the transporting velocity are
   ! reconstructed, and each is picked by the sign of the face-averaged
   ! foam velocity:
   !   $$ M_x = \tilde{u}_f \, \tilde{\eta}_f
   !      \Big|_{\mathrm{upwind}\ \mathrm{sgn}
   !      \left(\tfrac{1}{2}(u_{f,i-1}+u_{f,i})\right)} $$
   ! Legacy runs the x sweep over the full j range and the y sweep over
   ! the full i range (its own comment flags the asymmetry) — both are
   ! wider than the residual ever reads, so the shape is kept.
   ! ----------------------------------------------------------------
   subroutine foam_flux(this, lp, dx, dy, inv_dx, inv_dy)
      class(type_model_foam), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in) :: dx(:, :), dy(:, :), inv_dx(:, :), inv_dy(:, :)

      integer :: i, j

      associate (ef => this%eta_foam, &
                 uf => this%u_foam, vf => this%v_foam)

         call delx_fun(inv_dx, ef, this%del)
         call construct_x(dx, ef, this%del, this%vxl, this%vxr)
         do j = 1, lp%nloc
            do i = 2, lp%mloc
               if (0.5_SP*(uf(i - 1, j) + uf(i, j)) > ZERO) then
                  this%mx(i, j) = this%vxl(i, j)
               else
                  this%mx(i, j) = this%vxr(i, j)
               end if
            end do
         end do

         call delx_fun(inv_dx, uf, this%del)
         call construct_x(dx, uf, this%del, this%vxl, this%vxr)
         do j = 1, lp%nloc
            do i = 2, lp%mloc
               if (0.5_SP*(uf(i - 1, j) + uf(i, j)) > ZERO) then
                  this%mx(i, j) = this%vxl(i, j)*this%mx(i, j)
               else
                  this%mx(i, j) = this%vxr(i, j)*this%mx(i, j)
               end if
            end do
         end do

         call dely_fun(inv_dy, ef, this%del)
         call construct_y(dy, ef, this%del, this%vyl, this%vyr)
         do j = 2, lp%nloc
            do i = 1, lp%mloc
               if (0.5_SP*(vf(i, j - 1) + vf(i, j)) > ZERO) then
                  this%my(i, j) = this%vyl(i, j)
               else
                  this%my(i, j) = this%vyr(i, j)
               end if
            end do
         end do

         call dely_fun(inv_dy, vf, this%del)
         call construct_y(dy, vf, this%del, this%vyl, this%vyr)
         do j = 2, lp%nloc
            do i = 1, lp%mloc
               if (0.5_SP*(vf(i, j - 1) + vf(i, j)) > ZERO) then
                  this%my(i, j) = this%vyl(i, j)*this%my(i, j)
               else
                  this%my(i, j) = this%vyr(i, j)*this%my(i, j)
               end if
            end do
         end do

      end associate

   end subroutine foam_flux

   ! ----------------------------------------------------------------
   ! Legacy FOAM_UPDATE, Reul and Chapron (2003) branch.  A cell whose
   ! breaking event is younger than 2 dt restarts its envelope and is
   ! given an infinite age (no decay yet); a cell with no breaking
   ! viscosity is aged to BurstTimeNonBreaking so its foam bursts:
   !   $$ S^- = \frac{\eta_f}{\tau_f}\,e^{-a/\tau_f}, \qquad
   !      S^+ = f_{\mathrm{source}}\,\nu_{\mathrm{brk}} $$
   !   $$ \eta_f \mathrel{-}= \Delta t\left[
   !      \partial_x M_x + \partial_y M_y + S^- - S^+ \right] $$
   ! clipped at zero.  The foam velocity is then dragged towards the
   ! surface water velocity, with the increment capped so a stage can
   ! never overshoot past it:
   !   $$ \Delta u_f = C_{d,f}\,\frac{|\Delta \mathbf{u}|\,\Delta u}
   !      {\max(\eta_f, \eta_{\min})}\,\Delta t $$
   !
   ! The source/sink sweep covers the ghosts (legacy 1..Mloc) and the
   ! residual only the interior, so the ghost ring contributes nothing but
   ! its eta_foam_max envelope.  Kept as two sweeps like legacy.
   ! ----------------------------------------------------------------
   subroutine foam_advance(this, lp, dt, inv_dx, inv_dy, u, v, nu_break, age_break)
      class(type_model_foam), intent(inout) :: this
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in) :: dt
      real(SP), intent(in) :: inv_dx(:, :), inv_dy(:, :)
      real(SP), intent(in) :: u(:, :), v(:, :), nu_break(:, :), age_break(:, :)

      real(SP) :: age, uabs, uadd, vadd, du, dv
      integer  :: i, j

      associate (ef => this%eta_foam, &
                 uf => this%u_foam, vf => this%v_foam)

         do j = 1, lp%nloc
            do i = 1, lp%mloc

               age = age_break(i, j)
               if (age < 2.0_SP*dt) then
                  age = LARGE
                  this%eta_foam_max(i, j) = ZERO
               end if
               ! NOTE 3: exact equality, meaningful only while nu_bkg = 0
               if (nu_break(i, j) == ZERO) age = this%burst_time_nb

               if (ef(i, j) > this%eta_foam_max(i, j)) this%eta_foam_max(i, j) = ef(i, j)

               this%sink(i, j) = ef(i, j)/this%time_scale*exp(-age/this%time_scale)
               this%source(i, j) = this%f_source*nu_break(i, j)

            end do
         end do

         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie

               ef(i, j) = ef(i, j) &
                          - dt*(this%mx(i + 1, j) - this%mx(i, j))*inv_dx(i, j) &
                          - dt*(this%my(i, j + 1) - this%my(i, j))*inv_dy(i, j) &
                          - this%sink(i, j)*dt &
                          + this%source(i, j)*dt

               if (ef(i, j) < ZERO) ef(i, j) = ZERO

            end do
         end do

         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie

               if (ef(i, j) > ZERO) then
                  du = u(i, j) - uf(i, j)
                  dv = v(i, j) - vf(i, j)
                  uabs = sqrt(du*du + dv*dv)
                  uadd = uabs*du*this%cd_foam/max(ef(i, j), this%min_thick)*dt
                  vadd = uabs*dv*this%cd_foam/max(ef(i, j), this%min_thick)*dt

                  ! legacy overshoot guard: note it snaps to the WATER
                  ! velocity, not to the increment that would reach it
                  if (abs(uadd) > abs(du)) uadd = u(i, j)
                  if (abs(vadd) > abs(dv)) vadd = v(i, j)

                  uf(i, j) = uf(i, j) + uadd
                  vf(i, j) = vf(i, j) + vadd
               else
                  uf(i, j) = ZERO
                  vf(i, j) = ZERO
               end if

            end do
         end do

      end associate

   end subroutine foam_advance

   ! ----------------------------------------------------------------
   ! Legacy FOAM_BC: zero foam on the physical walls, then a plain halo
   ! exchange (legacy phi_exch — no wall mirror, no mask multiply).
   ! Periodic faces report no boundary, so they wrap instead.
   ! ----------------------------------------------------------------
   subroutine foam_bc(this, grid)
      class(type_model_foam), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      integer :: i, j

      associate (lp => grid%lp)

         if (grid%is_back_boundary) then
            do j = 1, lp%nloc
               this%mx(1, j) = ZERO
               this%eta_foam(1, j) = ZERO
            end do
         end if

         if (grid%is_shore_boundary) then
            do j = 1, lp%nloc
               this%mx(lp%mloc + 1, j) = ZERO
               this%eta_foam(lp%mloc, j) = ZERO
            end do
         end if

         if (grid%is_right_boundary) then
            do i = 1, lp%mloc
               this%my(i, 1) = ZERO
               this%eta_foam(i, 1) = ZERO
            end do
         end if

         if (grid%is_left_boundary) then
            do i = 1, lp%mloc
               this%my(i, lp%nloc + 1) = ZERO
               this%eta_foam(i, lp%nloc) = ZERO
            end do
         end if

      end associate

      call grid%halo_exchange(this%eta_foam)
      call grid%halo_exchange(this%u_foam)
      call grid%halo_exchange(this%v_foam)

   end subroutine foam_bc

   subroutine foam_free(this)
      class(type_model_foam), intent(inout) :: this

      if (allocated(this%eta_foam)) deallocate (this%eta_foam)
      if (allocated(this%eta_foam_max)) deallocate (this%eta_foam_max)
      if (allocated(this%u_foam)) deallocate (this%u_foam)
      if (allocated(this%v_foam)) deallocate (this%v_foam)
      if (allocated(this%mx)) deallocate (this%mx)
      if (allocated(this%my)) deallocate (this%my)
      if (allocated(this%sink)) deallocate (this%sink)
      if (allocated(this%source)) deallocate (this%source)
      if (allocated(this%del)) deallocate (this%del)
      if (allocated(this%vxl)) deallocate (this%vxl)
      if (allocated(this%vxr)) deallocate (this%vxr)
      if (allocated(this%vyl)) deallocate (this%vyl)
      if (allocated(this%vyr)) deallocate (this%vyr)
   end subroutine foam_free

end module model_foam_mod
