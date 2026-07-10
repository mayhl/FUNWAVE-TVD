! allow(E001)
module model_kernel_etauv_mod
   use core_constants_mod, only: SP, GRAV
   use core_grid_mod, only: type_loop_bounds
   implicit none
   private

   public :: type_etauv_workspace, update_h
   public :: cal_rk_update
   public :: cal_etauv_assemble_x, cal_etauv_assemble_y
   public :: cal_uv_no_dispersion, cal_etauv_update
   public :: RK_ALPHA, RK_BETA

   ! SSP-RK3 stage coefficients (legacy mod_global.F alpha/beta):
   !   $$ \phi^{(k)} = \alpha_k \phi^n + \beta_k\left(\phi^{(k-1)}
   !      + \Delta t\, R^{(k-1)}\right) $$
   real(SP), parameter :: RK_ALPHA(3) = &
                          [0.0_SP, 3.0_SP/4.0_SP, 1.0_SP/3.0_SP]
   real(SP), parameter :: RK_BETA(3) = &
                          [1.0_SP, 1.0_SP/4.0_SP, 2.0_SP/3.0_SP]

   ! ----------------------------------------------------------------
   ! Dispersive U/V update — pure assemble/update kernels around the
   ! tridiagonal solves in core_solver_tridiag_mod (kernels never call
   ! MPI; the caller owns the solver).  Caller sequence per stage:
   !
   !   dispersion path:
   !     call cal_etauv_assemble_x(...)                 ! fills ws%a/c/d
   !     call trid_x(lp, grid, ws%a, ws%c, ws%d, ws%f)  ! solver
   !     u(lp%ib:lp%ie, lp%jb:lp%je) = ws%f(lp%ib:lp%ie, lp%jb:lp%je)
   !     call cal_etauv_assemble_y(...)
   !     call trid_y[_periodic](lp, grid, ws%a, ws%c, ws%d, ws%f)
   !     v(lp%ib:lp%ie, lp%jb:lp%je) = ws%f(lp%ib:lp%ie, lp%jb:lp%je)
   !     call cal_etauv_update(...)                     ! mask, HU/HV, Froude cap
   !
   !   no-dispersion path:
   !     call cal_uv_no_dispersion(...)                 ! U=Ubar/H, V=Vbar/H
   !     call cal_etauv_update(...)
   !
   ! Coefficients: Shi et al. 2012 Cartesian, fully nonlinear.
   !   x-sweep (U): Gamma1 terms only — no Gamma2 correction in x.
   !   y-sweep (V): Gamma1 + Gamma2 terms (Gamma2>0 = DISP_TIME_LEFT).
   !   vxy/dvxy/uxy/duxy from cal_dispersion workspace (must precede).
   !   ux/dux also from dispersion workspace (Gamma2 path only).
   !   ETAy computed inline from eta and inv_dy.
   ! ----------------------------------------------------------------

   ! Scratch arrays for the two tridiagonal solves.  Allocated once
   ! at model startup (mloc×nloc), reused every stage/step.
   type, public :: type_etauv_workspace
      integer :: m = 0, n = 0
      real(SP), allocatable :: a(:, :), c(:, :), d(:, :), f(:, :)
   contains
      procedure :: alloc => ews_alloc
      procedure :: free => ews_free
   end type type_etauv_workspace

contains

   subroutine ews_alloc(ws, m, n)
      class(type_etauv_workspace), intent(inout) :: ws
      integer, intent(in) :: m, n
      ws%m = m; ws%n = n
      allocate (ws%a(m, n), ws%c(m, n), ws%d(m, n), ws%f(m, n))
   end subroutine ews_alloc

   subroutine ews_free(ws)
      class(type_etauv_workspace), intent(inout) :: ws
      ws%m = 0; ws%n = 0
      deallocate (ws%a, ws%c, ws%d, ws%f)
   end subroutine ews_free

   ! ----------------------------------------------------------------
   ! update_h — total water depth H = Gamma3*Eta + Depth.
   ! ----------------------------------------------------------------
   pure subroutine update_h(lp, gamma3, eta, depth, h)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: gamma3
      real(SP), intent(in)  :: eta(:, :), depth(:, :)
      real(SP), intent(out) :: h(:, :)
      integer :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            h(i, j) = eta(i, j)*gamma3 + depth(i, j)
         end do
      end do
   end subroutine update_h

   ! ----------------------------------------------------------------
   ! cal_rk_update — one SSP-RK3 stage combination for the conserved
   ! state (first half of legacy ESTIMATE_HUV, old/etauv_solver.F).
   ! Flux divergence plus sources form the stage residual:
   !   $$ R_1 = -\nabla\cdot(P, Q) + S_{wm}, \quad
   !      R_2 = -\nabla\cdot(F_x, F_y) + S_x, \quad
   !      R_3 = -\nabla\cdot(G_x, G_y) + S_y $$
   !   $$ \eta \leftarrow \alpha\,\eta^n + \beta(\eta + \Delta t R_1),
   !      \quad \bar U \leftarrow \alpha\,\bar U^n
   !      + \beta(\bar U + \Delta t R_2), \quad
   !      \bar V \leftarrow \alpha\,\bar V^n + \beta(\bar V + \Delta t R_3) $$
   ! pflx/qflx/fx/fy/gx/gy are the interface fluxes from
   ! type_flux_workspace, face-aligned with cell index i (face i =
   ! low side of cell i).  wavemaker_mass is the WK_* mass source
   ! (zero array when no wavemaker).  Legacy ETA_LIMITER (default
   ! off) is not ported.
   ! ----------------------------------------------------------------
   pure subroutine cal_rk_update(lp, alpha, beta, dt, inv_dx, inv_dy, &
                                 pflx, qflx, fx, fy, gx, gy, &
                                 src_x, src_y, wavemaker_mass, &
                                 eta0, ubar0, vbar0, eta, ubar, vbar)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in) :: alpha, beta, dt
      real(SP), intent(in) :: inv_dx(:, :), inv_dy(:, :)
      real(SP), intent(in) :: pflx(:, :), qflx(:, :)
      real(SP), intent(in) :: fx(:, :), fy(:, :), gx(:, :), gy(:, :)
      real(SP), intent(in) :: src_x(:, :), src_y(:, :), wavemaker_mass(:, :)
      real(SP), intent(in) :: eta0(:, :), ubar0(:, :), vbar0(:, :)
      real(SP), intent(inout) :: eta(:, :), ubar(:, :), vbar(:, :)

      real(SP) :: r1, r2, r3
      integer :: i, j

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            r1 = -(pflx(i + 1, j) - pflx(i, j))*inv_dx(i, j) &
                 - (qflx(i, j + 1) - qflx(i, j))*inv_dy(i, j) &
                 + wavemaker_mass(i, j)
            eta(i, j) = alpha*eta0(i, j) + beta*(eta(i, j) + dt*r1)

            r2 = -(fx(i + 1, j) - fx(i, j))*inv_dx(i, j) &
                 - (fy(i, j + 1) - fy(i, j))*inv_dy(i, j) &
                 + src_x(i, j)
            ubar(i, j) = alpha*ubar0(i, j) + beta*(ubar(i, j) + dt*r2)

            r3 = -(gx(i + 1, j) - gx(i, j))*inv_dx(i, j) &
                 - (gy(i, j + 1) - gy(i, j))*inv_dy(i, j) &
                 + src_y(i, j)
            vbar(i, j) = alpha*vbar0(i, j) + beta*(vbar(i, j) + dt*r3)
         end do
      end do

   end subroutine cal_rk_update

   ! ----------------------------------------------------------------
   ! cal_etauv_assemble_x — x-sweep coefficients for U (Gamma1 only).
   ! Fills ws%a/c/d (zeroed here); solve with trid_x into ws%f.
   ! ----------------------------------------------------------------
   pure subroutine cal_etauv_assemble_x(lp, gamma1, min_depth, b1, b2, &
                                        inv_dx, mask, mask9, depth, h, &
                                        ubar, vxy, dvxy, ws)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: gamma1, min_depth, b1, b2
      real(SP), intent(in)  :: inv_dx(:, :)
      integer, intent(in)  :: mask(:, :), mask9(:, :)
      real(SP), intent(in)  :: depth(:, :), h(:, :)
      real(SP), intent(in)  :: ubar(:, :), vxy(:, :), dvxy(:, :)
      type(type_etauv_workspace), intent(inout) :: ws

      real(SP) :: dep, depl, depr, tmp1, tmp2, tmp3, tmp4
      real(SP) :: idxsq, heff
      integer  :: i, j

      ws%a = 0.0_SP; ws%c = 0.0_SP; ws%d = 0.0_SP

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            dep = max(depth(i, j), min_depth)
            depl = max(depth(i - 1, j), min_depth)
            depr = max(depth(i + 1, j), min_depth)
            idxsq = inv_dx(i, j)*inv_dx(i, j)
            heff = max(h(i, j), min_depth)

            tmp1 = gamma1*mask9(i, j)*(b1*0.5_SP*idxsq*dep*dep + b2*idxsq*depl*dep)
            tmp2 = 1.0_SP + gamma1*mask9(i, j)*(-b1*idxsq*dep*dep - 2.0_SP*b2*idxsq*dep*dep)
            tmp3 = gamma1*mask9(i, j)*(b1*0.5_SP*idxsq*dep*dep + b2*idxsq*dep*depr)
            tmp4 = ubar(i, j)*mask(i, j)/heff &
                   + gamma1*mask9(i, j)*(-b1*0.5_SP*dep*dep*vxy(i, j) - b2*dep*dvxy(i, j))

            if (tmp2 /= 0.0_SP) then
               ws%a(i, j) = tmp1/tmp2
               ws%c(i, j) = tmp3/tmp2
               ws%d(i, j) = tmp4/tmp2
            end if
         end do
      end do

   end subroutine cal_etauv_assemble_x

   ! ----------------------------------------------------------------
   ! cal_etauv_assemble_y — y-sweep coefficients for V
   ! (Gamma1 + optional Gamma2 when disp_time_left).
   ! Fills ws%a/c/d (zeroed here); solve with trid_y[_periodic] into ws%f.
   ! ----------------------------------------------------------------
   pure subroutine cal_etauv_assemble_y(lp, disp_time_left, gamma1, gamma2, &
                                        min_depth, b1, b2, inv_dy, mask, mask9, &
                                        depth, h, eta, vbar, uxy, duxy, ux, dux, ws)
      type(type_loop_bounds), intent(in) :: lp
      logical, intent(in)  :: disp_time_left
      real(SP), intent(in)  :: gamma1, gamma2, min_depth, b1, b2
      real(SP), intent(in)  :: inv_dy(:, :)
      integer, intent(in)  :: mask(:, :), mask9(:, :)
      real(SP), intent(in)  :: depth(:, :), h(:, :), eta(:, :)
      real(SP), intent(in)  :: vbar(:, :), uxy(:, :), duxy(:, :)
      real(SP), intent(in)  :: ux(:, :), dux(:, :)
      type(type_etauv_workspace), intent(inout) :: ws

      real(SP) :: dep, depl, depr, tmp1, tmp2, tmp3, tmp4
      real(SP) :: idysq, heff
      real(SP) :: reta, retal, retar, etay_ij
      integer  :: i, j

      ws%a = 0.0_SP; ws%c = 0.0_SP; ws%d = 0.0_SP

      if (disp_time_left) then

         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               dep = max(depth(i, j), min_depth)
               depl = max(depth(i, j - 1), min_depth)
               depr = max(depth(i, j + 1), min_depth)
               idysq = inv_dy(i, j)*inv_dy(i, j)
               heff = max(h(i, j), min_depth)
               reta = eta(i, j)
               retal = eta(i, j - 1)
               retar = eta(i, j + 1)
               etay_ij = (retar - retal)*0.5_SP*inv_dy(i, j)

               tmp1 = gamma1*mask9(i, j)*(b1*0.5_SP*idysq*dep*dep + b2*idysq*depl*dep) &
                      - gamma2*mask9(i, j)*idysq*((reta + retal)*depl*0.5_SP &
                                                  + (retal + reta)**2*0.125_SP)
               tmp2 = 1.0_SP + gamma1*mask9(i, j)*(-b1*idysq*dep*dep - 2.0_SP*b2*idysq*dep*dep) &
                      + gamma2*mask9(i, j)*idysq*((retar + retal + 2.0_SP*reta)*0.5_SP &
                                                  + ((retar + reta)**2 + (retal + reta)**2)*0.125_SP)
               tmp3 = gamma1*mask9(i, j)*(b1*0.5_SP*idysq*dep*dep + b2*idysq*dep*depr) &
                      - gamma2*mask9(i, j)*idysq*((reta + retar)*depr*0.5_SP &
                                                  + (retar + reta)**2*0.125_SP)
               tmp4 = vbar(i, j)*mask(i, j)/heff &
                      + gamma1*mask9(i, j)*(-b1*0.5_SP*dep*dep*uxy(i, j) - b2*dep*duxy(i, j)) &
                      + gamma2*mask9(i, j)*(reta**2*0.5_SP*uxy(i, j) + reta*duxy(i, j) &
                                            + etay_ij*(reta*ux(i, j) + dux(i, j)))

               if (tmp2 /= 0.0_SP) then
                  ws%a(i, j) = tmp1/tmp2
                  ws%c(i, j) = tmp3/tmp2
                  ws%d(i, j) = tmp4/tmp2
               end if
            end do
         end do

      else

         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               dep = max(depth(i, j), min_depth)
               depl = max(depth(i, j - 1), min_depth)
               depr = max(depth(i, j + 1), min_depth)
               idysq = inv_dy(i, j)*inv_dy(i, j)
               heff = max(h(i, j), min_depth)

               tmp1 = gamma1*mask9(i, j)*(b1*0.5_SP*idysq*dep*dep + b2*idysq*depl*dep)
               tmp2 = 1.0_SP + gamma1*mask9(i, j)*(-b1*idysq*dep*dep - 2.0_SP*b2*idysq*dep*dep)
               tmp3 = gamma1*mask9(i, j)*(b1*0.5_SP*idysq*dep*dep + b2*idysq*dep*depr)
               tmp4 = vbar(i, j)*mask(i, j)/heff &
                      + gamma1*mask9(i, j)*(-b1*0.5_SP*dep*dep*uxy(i, j) - b2*dep*duxy(i, j))

               if (tmp2 /= 0.0_SP) then
                  ws%a(i, j) = tmp1/tmp2
                  ws%c(i, j) = tmp3/tmp2
                  ws%d(i, j) = tmp4/tmp2
               end if
            end do
         end do

      end if

   end subroutine cal_etauv_assemble_y

   ! ----------------------------------------------------------------
   ! cal_uv_no_dispersion — depth-average U = Ubar/H, V = Vbar/H.
   ! ----------------------------------------------------------------
   pure subroutine cal_uv_no_dispersion(lp, min_depth, h, ubar, vbar, u, v)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: min_depth
      real(SP), intent(in)  :: h(:, :), ubar(:, :), vbar(:, :)
      real(SP), intent(out) :: u(:, :), v(:, :)

      real(SP) :: heff
      integer  :: i, j

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            heff = max(h(i, j), min_depth)
            u(i, j) = ubar(i, j)/heff
            v(i, j) = vbar(i, j)/heff
         end do
      end do

   end subroutine cal_uv_no_dispersion

   ! ----------------------------------------------------------------
   ! cal_etauv_update — mask zeroing, HU/HV assembly, Froude cap.
   ! Runs after U and V are final for the stage (either path).
   ! ----------------------------------------------------------------
   pure subroutine cal_etauv_update(lp, froude_cap, min_depth, mask, h, &
                                    u, v, hu, hv, ubar, vbar)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)    :: froude_cap, min_depth
      integer, intent(in)    :: mask(:, :)
      real(SP), intent(in)    :: h(:, :)
      real(SP), intent(inout) :: u(:, :), v(:, :)
      real(SP), intent(out)   :: hu(:, :), hv(:, :)
      real(SP), intent(inout) :: ubar(:, :), vbar(:, :)

      real(SP) :: heff, utotal, fr_speed, utheta
      integer  :: i, j

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            if (mask(i, j) < 1) then
               ! legacy zeroes the conserved Ubar/Vbar here (with the
               ! stage's pre-UPDATE_MASK mask), not at the exchange
               ubar(i, j) = 0.0_SP; vbar(i, j) = 0.0_SP
               u(i, j) = 0.0_SP; v(i, j) = 0.0_SP
               hu(i, j) = 0.0_SP; hv(i, j) = 0.0_SP
            else
               heff = max(h(i, j), min_depth)
               hu(i, j) = heff*u(i, j)
               hv(i, j) = heff*v(i, j)
               utotal = sqrt(u(i, j)**2 + v(i, j)**2)
               fr_speed = sqrt(GRAV*heff)
               if (utotal > froude_cap*fr_speed) then
                  utheta = atan2(v(i, j), u(i, j))
                  u(i, j) = froude_cap*fr_speed*cos(utheta)
                  v(i, j) = froude_cap*fr_speed*sin(utheta)
                  hu(i, j) = u(i, j)*heff
                  hv(i, j) = v(i, j)*heff
               end if
            end if
         end do
      end do

   end subroutine cal_etauv_update

end module model_kernel_etauv_mod
