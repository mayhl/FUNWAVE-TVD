! allow(E001)
module model_kernel_dispersion_mod
   use core_constants_mod, only: SP
   use core_grid_mod, only: type_loop_bounds
   implicit none
   private

   public :: cal_dispersion_derivs, cal_dispersion_assemble

   ! ----------------------------------------------------------------
   ! Workspace for intermediate arrays in cal_dispersion.  Allocated
   ! once at model startup and reused every time step.
   ! ----------------------------------------------------------------
   type, public :: type_disp_workspace
      integer :: m = 0, n = 0
      ! depth-scaled velocity products
      real(SP), allocatable :: du(:, :), dv(:, :)
      real(SP), allocatable :: dut(:, :), dvt(:, :)
      ! second-order derivatives of u / v
      real(SP), allocatable :: uxx(:, :), uxy(:, :), vxy(:, :), vyy(:, :)
      ! second-order derivatives of (h*u) / (h*v)
      real(SP), allocatable :: duxx(:, :), duxy(:, :), dvxy(:, :), dvyy(:, :)
      ! first-order derivatives of u / v  (gamma2 path)
      real(SP), allocatable :: ux(:, :), vx(:, :), uy(:, :), vy(:, :)
      ! selected first-order derivatives of (h*u)/(h*v)  (gamma2 path)
      real(SP), allocatable :: dux(:, :), dvy(:, :)
      ! time-derivative intermediates  (gamma2 path)
      real(SP), allocatable :: utx(:, :), vty(:, :)
      real(SP), allocatable :: utxx(:, :), vtyy(:, :), utxy(:, :), vtxy(:, :)
      real(SP), allocatable :: dutx(:, :), dvty(:, :)
      real(SP), allocatable :: dutxx(:, :), dvtyy(:, :), dutxy(:, :), dvtxy(:, :)
   contains
      procedure :: alloc => dws_alloc
      procedure :: free => dws_free
   end type type_disp_workspace

contains

   subroutine dws_alloc(ws, m, n)
      class(type_disp_workspace), intent(inout) :: ws
      integer, intent(in) :: m, n
      ws%m = m; ws%n = n
      ! zero-fill ONCE: the per-stage written set is topology-fixed
      ! (deriv interiors, exchange seams, wall mirrors), so a cell
      ! outside it keeps this zero forever — re-zeroing each stage
      ! was pure overhead (perf audit item 3)
      allocate (ws%du(m, n), ws%dv(m, n), source=0.0_SP)
      allocate (ws%dut(m, n), ws%dvt(m, n), source=0.0_SP)
      allocate (ws%uxx(m, n), ws%uxy(m, n), ws%vxy(m, n), ws%vyy(m, n), source=0.0_SP)
      allocate (ws%duxx(m, n), ws%duxy(m, n), ws%dvxy(m, n), ws%dvyy(m, n), source=0.0_SP)
      allocate (ws%ux(m, n), ws%vx(m, n), ws%uy(m, n), ws%vy(m, n), source=0.0_SP)
      allocate (ws%dux(m, n), ws%dvy(m, n), source=0.0_SP)
      allocate (ws%utx(m, n), ws%vty(m, n), source=0.0_SP)
      allocate (ws%utxx(m, n), ws%vtyy(m, n), ws%utxy(m, n), ws%vtxy(m, n), source=0.0_SP)
      allocate (ws%dutx(m, n), ws%dvty(m, n), source=0.0_SP)
      allocate (ws%dutxx(m, n), ws%dvtyy(m, n), ws%dutxy(m, n), ws%dvtxy(m, n), source=0.0_SP)
   end subroutine dws_alloc

   subroutine dws_free(ws)
      class(type_disp_workspace), intent(inout) :: ws
      ws%m = 0; ws%n = 0
      deallocate (ws%du, ws%dv, ws%dut, ws%dvt)
      deallocate (ws%uxx, ws%uxy, ws%vxy, ws%vyy)
      deallocate (ws%duxx, ws%duxy, ws%dvxy, ws%dvyy)
      deallocate (ws%ux, ws%vx, ws%uy, ws%vy)
      deallocate (ws%dux, ws%dvy)
      deallocate (ws%utx, ws%vty)
      deallocate (ws%utxx, ws%vtyy, ws%utxy, ws%vtxy)
      deallocate (ws%dutx, ws%dvty)
      deallocate (ws%dutxx, ws%dvtyy, ws%dutxy, ws%dvtxy)
   end subroutine dws_free

   ! ----------------------------------------------------------------
   ! Point-stencil helpers for the fused derivative sweeps.  Same
   ! expressions as the kernel_derivatives array kernels; kept in this
   ! module so ifx/gfortran inline them at -O2 (a cross-module call
   ! per point would need IPO to disappear).
   ! ----------------------------------------------------------------
   pure function pt_dx(f, i, j, idx, m9) result(d)
      real(SP), intent(in) :: f(:, :), idx
      integer, intent(in)  :: i, j
      real(SP), intent(in) :: m9
      real(SP) :: d
      d = (f(i + 1, j) - f(i - 1, j))*0.5_SP*idx*m9
   end function pt_dx

   pure function pt_dy(f, i, j, idy, m9) result(d)
      real(SP), intent(in) :: f(:, :), idy
      integer, intent(in)  :: i, j
      real(SP), intent(in) :: m9
      real(SP) :: d
      d = (f(i, j + 1) - f(i, j - 1))*0.5_SP*idy*m9
   end function pt_dy

   pure function pt_dxx(f, i, j, idx, m9) result(d)
      real(SP), intent(in) :: f(:, :), idx
      integer, intent(in)  :: i, j
      real(SP), intent(in) :: m9
      real(SP) :: d
      d = (f(i + 1, j) - 2.0_SP*f(i, j) + f(i - 1, j))*idx*idx*m9
   end function pt_dxx

   pure function pt_dyy(f, i, j, idy, m9) result(d)
      real(SP), intent(in) :: f(:, :), idy
      integer, intent(in)  :: i, j
      real(SP), intent(in) :: m9
      real(SP) :: d
      d = (f(i, j + 1) - 2.0_SP*f(i, j) + f(i, j - 1))*idy*idy*m9
   end function pt_dyy

   pure function pt_dxy(f, i, j, idx, idy, m9) result(d)
      real(SP), intent(in) :: f(:, :), idx, idy
      integer, intent(in)  :: i, j
      real(SP), intent(in) :: m9
      real(SP) :: t1, t2, d
      t1 = (f(i + 1, j + 1) - f(i + 1, j - 1))*0.5_SP*idy
      t2 = (f(i - 1, j + 1) - f(i - 1, j - 1))*0.5_SP*idy
      d = (t1 - t2)*0.5_SP*idx*m9
   end function pt_dxy

   ! ----------------------------------------------------------------
   ! Boussinesq dispersion source terms (Shi et al. 2012, Cartesian).
   !
   ! Always computed: etat, u4/v4, u1p/v1p.
   ! gamma2 > 0 additionally: ut/vt, u1pp/v1pp, u2/v2, u3/v3.
   ! show_breaking (and gamma2 > 0): etax/etay.
   !
   ! west/east/south/north_bdy flag marks domain-edge ranks; cross-
   ! derivatives at those faces are zeroed to suppress stencil errors.
   ! Ghost-cell exchanges remain the caller's responsibility.
   ! ----------------------------------------------------------------
   subroutine cal_dispersion_derivs(lp, ws, eta, depth, u, v, u0, v0, p, q, &
                                    mask9, inv_dx, inv_dy, dt, min_depth_frc, &
                                    gamma2, show_breaking, &
                                    west_bdy, east_bdy, south_bdy, north_bdy, &
                                    etat, ut, vt, etax, etay)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_disp_workspace), intent(inout) :: ws
      real(SP), intent(in)    :: eta(:, :), depth(:, :)
      real(SP), intent(in)    :: u(:, :), v(:, :), u0(:, :), v0(:, :)
      real(SP), intent(in)    :: p(:, :), q(:, :)
      real(SP), intent(in)    :: mask9(:, :)
      real(SP), intent(in)    :: inv_dx(:, :), inv_dy(:, :)
      real(SP), intent(in)    :: dt, min_depth_frc, gamma2
      logical, intent(in)    :: show_breaking
      logical, intent(in)    :: west_bdy, east_bdy, south_bdy, north_bdy
      real(SP), intent(out)   :: etat(:, :)
      real(SP), intent(inout) :: ut(:, :), vt(:, :)
      real(SP), intent(inout) :: etax(:, :), etay(:, :)

      integer  :: i, j
      real(SP) :: inv_dt

      ! Ghost-cell neighbours of the computed region read 0 via the
      ! one-time alloc zero-fill (dws_alloc) — nothing here writes
      ! outside the fixed per-stage set, so those cells never change

      ! ---- second-order derivatives of u / v --------------------------
      ! one fused sweep instead of four deriv_* calls — the split sweeps
      ! re-streamed u and v once per output (perf audit, np20 cachegrind)
      !$omp parallel do default(shared) schedule(static) private(i)
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            ws%uxx(i, j) = pt_dxx(u, i, j, inv_dx(i, j), mask9(i, j))
            ws%uxy(i, j) = pt_dxy(u, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))
            ws%vxy(i, j) = pt_dxy(v, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))
            ws%vyy(i, j) = pt_dyy(v, i, j, inv_dy(i, j), mask9(i, j))
         end do
      end do

      ! ---- first-order derivatives (gamma2 or show_breaking) ----------
      if (gamma2 > 0.0_SP) then
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               ws%ux(i, j) = pt_dx(u, i, j, inv_dx(i, j), mask9(i, j))
               ws%vx(i, j) = pt_dx(v, i, j, inv_dx(i, j), mask9(i, j))
               ws%uy(i, j) = pt_dy(u, i, j, inv_dy(i, j), mask9(i, j))
               ws%vy(i, j) = pt_dy(v, i, j, inv_dy(i, j), mask9(i, j))
               etax(i, j) = pt_dx(eta, i, j, inv_dx(i, j), mask9(i, j))
               etay(i, j) = pt_dy(eta, i, j, inv_dy(i, j), mask9(i, j))
            end do
         end do
      else if (show_breaking) then
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               etax(i, j) = pt_dx(eta, i, j, inv_dx(i, j), mask9(i, j))
               etay(i, j) = pt_dy(eta, i, j, inv_dy(i, j), mask9(i, j))
            end do
         end do
      end if

      ! ---- DU, DV, ETAT -----------------------------------------------
      !$omp parallel do default(shared) schedule(static) private(i)
      do j = 1, lp%nloc - 1
         do i = 1, lp%mloc - 1
            ws%du(i, j) = max(depth(i, j), min_depth_frc)*u(i, j)
            ws%dv(i, j) = max(depth(i, j), min_depth_frc)*v(i, j)
            etat(i, j) = -(p(i + 1, j) - p(i, j))*inv_dx(i, j) &
                         - (q(i, j + 1) - q(i, j))*inv_dy(i, j)
         end do
      end do

      ! ---- Ut, Vt and their depth-scaled forms (gamma2 only) ----------
      if (gamma2 > 0.0_SP) then
         inv_dt = 1.0_SP/dt
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = 1, lp%nloc
            do i = 1, lp%mloc
               ut(i, j) = (u(i, j) - u0(i, j))*inv_dt
               vt(i, j) = (v(i, j) - v0(i, j))*inv_dt
               ws%dut(i, j) = max(depth(i, j), min_depth_frc)*ut(i, j)
               ws%dvt(i, j) = max(depth(i, j), min_depth_frc)*vt(i, j)
            end do
         end do
      end if

      ! ---- second-order derivatives of (h*u) / (h*v) -----------------
      ! same fusion as the u/v group; under gamma2 the t-derivative
      ! family rides the sweep too — 18 separate deriv_* calls
      ! otherwise stream du/dv/ut/vt/dut/dvt four times each
      if (gamma2 > 0.0_SP) then
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               ws%duxx(i, j) = pt_dxx(ws%du, i, j, inv_dx(i, j), mask9(i, j))
               ws%duxy(i, j) = pt_dxy(ws%du, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))
               ws%dvxy(i, j) = pt_dxy(ws%dv, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))
               ws%dvyy(i, j) = pt_dyy(ws%dv, i, j, inv_dy(i, j), mask9(i, j))

               ws%dux(i, j) = pt_dx(ws%du, i, j, inv_dx(i, j), mask9(i, j))
               ws%dvy(i, j) = pt_dy(ws%dv, i, j, inv_dy(i, j), mask9(i, j))

               ws%utx(i, j) = pt_dx(ut, i, j, inv_dx(i, j), mask9(i, j))
               ws%vty(i, j) = pt_dy(vt, i, j, inv_dy(i, j), mask9(i, j))
               ws%utxx(i, j) = pt_dxx(ut, i, j, inv_dx(i, j), mask9(i, j))
               ws%vtyy(i, j) = pt_dyy(vt, i, j, inv_dy(i, j), mask9(i, j))
               ws%utxy(i, j) = pt_dxy(ut, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))
               ws%vtxy(i, j) = pt_dxy(vt, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))

               ws%dutx(i, j) = pt_dx(ws%dut, i, j, inv_dx(i, j), mask9(i, j))
               ws%dvty(i, j) = pt_dy(ws%dvt, i, j, inv_dy(i, j), mask9(i, j))
               ws%dutxx(i, j) = pt_dxx(ws%dut, i, j, inv_dx(i, j), mask9(i, j))
               ws%dvtyy(i, j) = pt_dyy(ws%dvt, i, j, inv_dy(i, j), mask9(i, j))
               ws%dutxy(i, j) = pt_dxy(ws%dut, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))
               ws%dvtxy(i, j) = pt_dxy(ws%dvt, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))
            end do
         end do
      else
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               ws%duxx(i, j) = pt_dxx(ws%du, i, j, inv_dx(i, j), mask9(i, j))
               ws%duxy(i, j) = pt_dxy(ws%du, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))
               ws%dvxy(i, j) = pt_dxy(ws%dv, i, j, inv_dx(i, j), inv_dy(i, j), mask9(i, j))
               ws%dvyy(i, j) = pt_dyy(ws%dv, i, j, inv_dy(i, j), mask9(i, j))
            end do
         end do
      end if

      ! zero cross-derivatives at domain faces (legacy dispersion.F
      ! boundary conditions — unconditional, NOT gamma2-gated; the
      ! t-derivative arrays are zeroed only when gamma2 computed them)
      if (west_bdy) then
         ws%uxy(lp%ib, :) = 0.0_SP; ws%vxy(lp%ib, :) = 0.0_SP
         ws%duxy(lp%ib, :) = 0.0_SP; ws%dvxy(lp%ib, :) = 0.0_SP
      end if
      if (east_bdy) then
         ws%uxy(lp%ie, :) = 0.0_SP; ws%vxy(lp%ie, :) = 0.0_SP
         ws%duxy(lp%ie, :) = 0.0_SP; ws%dvxy(lp%ie, :) = 0.0_SP
      end if
      if (south_bdy) then
         ws%uxy(:, lp%jb) = 0.0_SP; ws%vxy(:, lp%jb) = 0.0_SP
         ws%duxy(:, lp%jb) = 0.0_SP; ws%dvxy(:, lp%jb) = 0.0_SP
      end if
      if (north_bdy) then
         ws%uxy(:, lp%je) = 0.0_SP; ws%vxy(:, lp%je) = 0.0_SP
         ws%duxy(:, lp%je) = 0.0_SP; ws%dvxy(:, lp%je) = 0.0_SP
      end if
      if (gamma2 > 0.0_SP) then
         if (west_bdy) then
            ws%utxy(lp%ib, :) = 0.0_SP; ws%vtxy(lp%ib, :) = 0.0_SP
            ws%dutxy(lp%ib, :) = 0.0_SP; ws%dvtxy(lp%ib, :) = 0.0_SP
         end if
         if (east_bdy) then
            ws%utxy(lp%ie, :) = 0.0_SP; ws%vtxy(lp%ie, :) = 0.0_SP
            ws%dutxy(lp%ie, :) = 0.0_SP; ws%dvtxy(lp%ie, :) = 0.0_SP
         end if
         if (south_bdy) then
            ws%utxy(:, lp%jb) = 0.0_SP; ws%vtxy(:, lp%jb) = 0.0_SP
            ws%dutxy(:, lp%jb) = 0.0_SP; ws%dvtxy(:, lp%jb) = 0.0_SP
         end if
         if (north_bdy) then
            ws%utxy(:, lp%je) = 0.0_SP; ws%vtxy(:, lp%je) = 0.0_SP
            ws%dutxy(:, lp%je) = 0.0_SP; ws%dvtxy(:, lp%je) = 0.0_SP
         end if
      end if

   end subroutine cal_dispersion_derivs

   ! ----------------------------------------------------------------
   ! Dispersion assembly (legacy Cal_Dispersion after
   ! EXCHANGE_DISPERSION): the caller must exchange the workspace
   ! component arrays first (bc%exchange_dispersion) — legacy fills
   ! their ghosts with parity mirrors and then assembles u4/v4 and
   ! u1p/v1p over the FULL ghost-inclusive array, so the flux face
   ! reconstruction and source stencils read assembled ghosts, never
   ! mirrored u4/v4 (the mirror shortcut is wrong wherever
   ! $V_{xy} \ne 0$ and flips limiter branches via the sign of zero
   ! at zero fields).  The gamma2 stencil terms (u1pp/u2/u3) stay
   ! interior-only, exactly as legacy.
   ! ----------------------------------------------------------------
   subroutine cal_dispersion_assemble(lp, ws, eta, depth, u, v, mask9, &
                                      inv_dx, inv_dy, beta1, beta2, gamma2, &
                                      etat, etax, etay, &
                                      u4, v4, u1p, v1p, u1pp, v1pp, &
                                      u2, v2, u3, v3, out_vormax, vort_max)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_disp_workspace), intent(in) :: ws
      real(SP), intent(in)    :: eta(:, :), depth(:, :)
      real(SP), intent(in)    :: u(:, :), v(:, :)
      real(SP), intent(in)    :: mask9(:, :)
      real(SP), intent(in)    :: inv_dx(:, :), inv_dy(:, :)
      real(SP), intent(in)    :: beta1, beta2, gamma2
      real(SP), intent(in)    :: etat(:, :), etax(:, :), etay(:, :)
      real(SP), intent(out)   :: u4(:, :), v4(:, :), u1p(:, :), v1p(:, :)
      real(SP), intent(inout) :: u1pp(:, :), v1pp(:, :)
      real(SP), intent(inout) :: u2(:, :), v2(:, :), u3(:, :), v3(:, :)
      ! VORmax envelope (legacy updates it here, per stage): signed
      ! store under an |omega| test, exactly as dispersion.F
      logical, intent(in), optional :: out_vormax
      real(SP), intent(inout), optional :: vort_max(:, :)

      integer  :: i, j
      logical  :: do_vormax
      real(SP) :: rh, rhx, rhy, reta
      real(SP) :: uxxvxy, uxyvyy, huxxhvxy, huxyhvyy
      real(SP) :: uxxvxy_x, uxxvxy_y, uxyvyy_x, uxyvyy_y
      real(SP) :: huxxhvxy_x, huxxhvxy_y, huxyhvyy_x, huxyhvyy_y
      real(SP) :: ken1, ken2, ken3, ken4, ken5
      real(SP) :: omega_0, omega_1
      real(SP) :: coeff_a, coeff_b, coeff_1p

      do_vormax = .false.
      if (present(out_vormax)) do_vormax = out_vormax .and. present(vort_max)

      ! ---- linear dispersion: u4/v4, u1p/v1p -------------------------
      coeff_a = 1.0_SP/3.0_SP - beta1 + 0.5_SP*beta1*beta1
      coeff_b = beta1 - 0.5_SP
      coeff_1p = 0.5_SP*(1.0_SP - beta1)*(1.0_SP - beta1)

      if (gamma2 <= 0.0_SP) then
         !$omp parallel do default(shared) schedule(static) &
         !$omp& private(i, uxxvxy, uxyvyy, huxxhvxy, huxyhvyy, rh)
         do j = 1, lp%nloc
            do i = 1, lp%mloc
               uxxvxy = ws%uxx(i, j) + ws%vxy(i, j)
               uxyvyy = ws%uxy(i, j) + ws%vyy(i, j)
               huxxhvxy = ws%duxx(i, j) + ws%dvxy(i, j)
               huxyhvyy = ws%duxy(i, j) + ws%dvyy(i, j)
               rh = depth(i, j)

               u4(i, j) = coeff_a*rh*rh*uxxvxy + coeff_b*rh*huxxhvxy
               v4(i, j) = coeff_a*rh*rh*uxyvyy + coeff_b*rh*huxyhvyy
               u1p(i, j) = coeff_1p*rh*rh*uxxvxy + (beta1 - 1.0_SP)*rh*huxxhvxy
               v1p(i, j) = coeff_1p*rh*rh*uxyvyy + (beta1 - 1.0_SP)*rh*huxyhvyy
            end do
         end do
         return
      end if

      ! ---- gamma2: linear + nonlinear passes fused per row ------------
      ! the two passes share the eight exchanged stencil arrays, so a
      ! second full sweep re-streamed them from DRAM at large tiles;
      ! the nonlinear body only reads workspace arrays, never the
      ! linear pass outputs, so the row fusion is order-exact
      !$omp parallel do default(shared) schedule(static) &
      !$omp& private(i, uxxvxy, uxyvyy, huxxhvxy, huxyhvyy, rh, rhx, rhy, reta, &
      !$omp&         uxxvxy_x, uxxvxy_y, uxyvyy_x, uxyvyy_y, huxxhvxy_x, huxxhvxy_y, &
      !$omp&         huxyhvyy_x, huxyhvyy_y, ken1, ken2, ken3, ken4, ken5, omega_0, omega_1)
      do j = 1, lp%nloc
         do i = 1, lp%mloc
            uxxvxy = ws%uxx(i, j) + ws%vxy(i, j)
            uxyvyy = ws%uxy(i, j) + ws%vyy(i, j)
            huxxhvxy = ws%duxx(i, j) + ws%dvxy(i, j)
            huxyhvyy = ws%duxy(i, j) + ws%dvyy(i, j)
            rh = depth(i, j)

            u4(i, j) = coeff_a*rh*rh*uxxvxy + coeff_b*rh*huxxhvxy
            v4(i, j) = coeff_a*rh*rh*uxyvyy + coeff_b*rh*huxyhvyy
            u1p(i, j) = coeff_1p*rh*rh*uxxvxy + (beta1 - 1.0_SP)*rh*huxxhvxy
            v1p(i, j) = coeff_1p*rh*rh*uxyvyy + (beta1 - 1.0_SP)*rh*huxyhvyy

            ! gamma2 nonlinear addition to u4/v4
            reta = eta(i, j)
            ken1 = (1.0_SP/6.0_SP - beta1 + beta1*beta1)*rh*reta*beta2 &
                   + (0.5_SP*beta1*beta1 - 1.0_SP/6.0_SP)*reta*reta*beta2*beta2
            ken2 = (beta1 - 0.5_SP)*reta*beta2
            u4(i, j) = u4(i, j) + gamma2*mask9(i, j)*(ken1*uxxvxy + ken2*huxxhvxy)
            v4(i, j) = v4(i, j) + gamma2*mask9(i, j)*(ken1*uxyvyy + ken2*huxyhvyy)
         end do

         if (j < lp%jb .or. j > lp%je) cycle
         do i = lp%ib, lp%ie
            uxxvxy = ws%uxx(i, j) + ws%vxy(i, j)
            uxyvyy = ws%uxy(i, j) + ws%vyy(i, j)
            huxxhvxy = ws%duxx(i, j) + ws%dvxy(i, j)
            huxyhvyy = ws%duxy(i, j) + ws%dvyy(i, j)

            uxxvxy_x = (ws%uxx(i + 1, j) + ws%vxy(i + 1, j) - ws%uxx(i - 1, j) - ws%vxy(i - 1, j)) &
                       *0.5_SP*inv_dx(i, j)
            uxxvxy_y = (ws%uxx(i, j + 1) + ws%vxy(i, j + 1) - ws%uxx(i, j - 1) - ws%vxy(i, j - 1)) &
                       *0.5_SP*inv_dy(i, j)
            uxyvyy_x = (ws%uxy(i + 1, j) + ws%vyy(i + 1, j) - ws%uxy(i - 1, j) - ws%vyy(i - 1, j)) &
                       *0.5_SP*inv_dx(i, j)
            uxyvyy_y = (ws%uxy(i, j + 1) + ws%vyy(i, j + 1) - ws%uxy(i, j - 1) - ws%vyy(i, j - 1)) &
                       *0.5_SP*inv_dy(i, j)
            huxxhvxy_x = (ws%duxx(i + 1, j) + ws%dvxy(i + 1, j) - ws%duxx(i - 1, j) - ws%dvxy(i - 1, j)) &
                         *0.5_SP*inv_dx(i, j)
            huxxhvxy_y = (ws%duxx(i, j + 1) + ws%dvxy(i, j + 1) - ws%duxx(i, j - 1) - ws%dvxy(i, j - 1)) &
                         *0.5_SP*inv_dy(i, j)
            huxyhvyy_x = (ws%duxy(i + 1, j) + ws%dvyy(i + 1, j) - ws%duxy(i - 1, j) - ws%dvyy(i - 1, j)) &
                         *0.5_SP*inv_dx(i, j)
            huxyhvyy_y = (ws%duxy(i, j + 1) + ws%dvyy(i, j + 1) - ws%duxy(i, j - 1) - ws%dvyy(i, j - 1)) &
                         *0.5_SP*inv_dy(i, j)

            rh = depth(i, j)
            rhx = (depth(i + 1, j) - depth(i - 1, j))*0.5_SP*inv_dx(i, j)
            rhy = (depth(i, j + 1) - depth(i, j - 1))*0.5_SP*inv_dy(i, j)
            reta = eta(i, j)

            ! ---- U1pp / V1pp  (time-derivative correction) -----------
            u1pp(i, j) = -reta*beta2*etax(i, j)*beta2*(ws%utx(i, j) + ws%vty(i, j)) &
                         - 0.5_SP*reta*reta*beta2*beta2*(ws%utxx(i, j) + ws%vtxy(i, j)) &
                         - etax(i, j)*beta2*(ws%dutx(i, j) + ws%dvty(i, j)) &
                         - reta*beta2*(ws%dutxx(i, j) + ws%dvtxy(i, j))

            v1pp(i, j) = -reta*beta2*etay(i, j)*beta2*(ws%utx(i, j) + ws%vty(i, j)) &
                         - 0.5_SP*reta*reta*beta2*beta2*(ws%utxy(i, j) + ws%vtyy(i, j)) &
                         - etay(i, j)*beta2*(ws%dutx(i, j) + ws%dvty(i, j)) &
                         - reta*beta2*(ws%dutxy(i, j) + ws%dvtyy(i, j))

            ken1 = beta1*(1.0_SP - beta1)*rh*etat(i, j)*beta2 &
                   - beta1*beta1*reta*beta2*etat(i, j)*beta2
            ken2 = beta1*(1.0_SP - beta1)*rh*reta*beta2 &
                   - 0.5_SP*beta1*beta1*reta*reta*beta2*beta2
            ken3 = beta1*etat(i, j)*beta2
            ken4 = beta1*reta*beta2

            u1pp(i, j) = u1pp(i, j) - ken1*uxxvxy &
                         - ken2*(ws%utxx(i, j) + ws%vtxy(i, j)) &
                         + ken3*huxxhvxy + ken4*(ws%dutxx(i, j) + ws%dvtxy(i, j))
            v1pp(i, j) = v1pp(i, j) - ken1*uxyvyy &
                         - ken2*(ws%utxy(i, j) + ws%vtyy(i, j)) &
                         + ken3*huxyhvyy + ken4*(ws%dutxy(i, j) + ws%dvtyy(i, j))

            ! ---- U2 / V2  (nonlinear advection-type dispersion) ------
            ken1 = (beta1 - 1.0_SP)*(rhx + etax(i, j))*beta2
            ken2 = (beta1 - 1.0_SP)*(rh + reta)*beta2
            ken3 = (1.0_SP - beta1)*(1.0_SP - beta1)*rh*rhx*beta2*beta2 &
                   - beta1*(1.0_SP - beta1)*(rhx*reta + rh*etax(i, j))*beta2 &
                   + (beta1*beta1 - 1.0_SP)*reta*etax(i, j)*beta2*beta2
            ken4 = 0.5_SP*(1.0_SP - beta1)*(1.0_SP - beta1)*rh*rh*beta2*beta2 &
                   - beta1*(1.0_SP - beta1)*rh*reta*beta2 &
                   + 0.5_SP*(beta1*beta1 - 1.0_SP)*reta*reta*beta2*beta2
            ken5 = (1.0_SP - beta1)*(1.0_SP - beta1)*rh*rhy*beta2*beta2 &
                   - beta1*(1.0_SP - beta1)*(rhy*reta + rh*etay(i, j))*beta2 &
                   + (beta1*beta1 - 1.0_SP)*reta*etay(i, j)*beta2*beta2

            u2(i, j) = ken1*(u(i, j)*huxxhvxy + v(i, j)*huxyhvyy) &
                       + ken2*(ws%ux(i, j)*huxxhvxy + u(i, j)*huxxhvxy_x &
                               + ws%vx(i, j)*huxyhvyy + v(i, j)*huxyhvyy_x) &
                       + ken3*(u(i, j)*uxxvxy + v(i, j)*uxyvyy) &
                       + ken4*(ws%ux(i, j)*uxxvxy + u(i, j)*uxxvxy_x &
                               + ws%vx(i, j)*uxyvyy + v(i, j)*uxyvyy_x) &
                       + beta2*beta2*(ws%dux(i, j) + ws%dvy(i, j) &
                                      + reta*beta2*(ws%ux(i, j) + ws%vy(i, j))) &
                       *(huxxhvxy + etax(i, j)*beta2*(ws%ux(i, j) + ws%vy(i, j)) &
                         + reta*beta2*uxxvxy)

            ken1 = (beta1 - 1.0_SP)*(rhy + etay(i, j))*beta2

            v2(i, j) = ken1*(u(i, j)*huxxhvxy + v(i, j)*huxyhvyy) &
                       + ken2*(ws%uy(i, j)*huxxhvxy + u(i, j)*huxxhvxy_y &
                               + ws%vy(i, j)*huxyhvyy + v(i, j)*huxyhvyy_y) &
                       + ken5*(u(i, j)*uxxvxy + v(i, j)*uxyvyy) &
                       + ken4*(ws%uy(i, j)*uxxvxy + u(i, j)*uxxvxy_y &
                               + ws%vy(i, j)*uxyvyy + v(i, j)*uxyvyy_y) &
                       + beta2*beta2*(ws%dux(i, j) + ws%dvy(i, j) &
                                      + reta*beta2*(ws%ux(i, j) + ws%vy(i, j))) &
                       *(huxyhvyy + etay(i, j)*beta2*(ws%ux(i, j) + ws%vy(i, j)) &
                         + reta*beta2*uxyvyy)

            ! ---- U3 / V3  (vorticity-type dispersion) ----------------
            omega_0 = ws%vx(i, j) - ws%uy(i, j)
            omega_1 = ((beta1 - 1.0_SP)*rhx + beta1*etax(i, j))*beta2 &
                      *(huxyhvyy + ((beta1 - 1.0_SP)*rh + beta1*reta)*beta2*uxyvyy) &
                      - ((beta1 - 1.0_SP)*rhy + beta1*etay(i, j))*beta2 &
                      *(huxxhvxy + ((beta1 - 1.0_SP)*rh + beta1*reta)*beta2*uxxvxy)

            if (do_vormax) then
               if (abs(omega_0 + omega_1) > vort_max(i, j)) &
                  vort_max(i, j) = omega_0 + omega_1
            end if

            ken1 = (beta1 - 0.5_SP)*(reta + rh)*beta2
            ken2 = (1.0_SP/3.0_SP - beta1 + 0.5_SP*beta1*beta1)*rh*rh*beta2*beta2 &
                   + (1.0_SP/6.0_SP - beta1 + beta1*beta1)*rh*reta*beta2 &
                   + (0.5_SP*beta1*beta1 - 1.0_SP/6.0_SP)*reta*reta*beta2*beta2

            u3(i, j) = -v(i, j)*omega_1 - omega_0*(ken1*huxyhvyy + ken2*uxyvyy)
            v3(i, j) = u(i, j)*omega_1 + omega_0*(ken1*huxxhvxy + ken2*uxxvxy)
         end do
      end do

   end subroutine cal_dispersion_assemble

end module model_kernel_dispersion_mod
