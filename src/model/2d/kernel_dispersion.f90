! allow(E001)
module model_kernel_dispersion_mod
   use core_constants_mod,           only: SP
   use core_grid_mod,                only: type_loop_bounds
   use model_kernel_derivatives_mod, only: deriv_x, deriv_y, &
                                           deriv_xx, deriv_yy, deriv_xy
   implicit none
   private

   public :: type_disp_workspace, cal_dispersion

   ! ----------------------------------------------------------------
   ! Workspace for intermediate arrays in cal_dispersion.  Allocated
   ! once at model startup and reused every time step.
   ! ----------------------------------------------------------------
   type, public :: type_disp_workspace
      integer :: m = 0, n = 0
      ! depth-scaled velocity products
      real(SP), allocatable :: du(:,:), dv(:,:)
      real(SP), allocatable :: dut(:,:), dvt(:,:)
      ! second-order derivatives of u / v
      real(SP), allocatable :: uxx(:,:), uxy(:,:), vxy(:,:), vyy(:,:)
      ! second-order derivatives of (h*u) / (h*v)
      real(SP), allocatable :: duxx(:,:), duxy(:,:), dvxy(:,:), dvyy(:,:)
      ! first-order derivatives of u / v  (gamma2 path)
      real(SP), allocatable :: ux(:,:), vx(:,:), uy(:,:), vy(:,:)
      ! selected first-order derivatives of (h*u)/(h*v)  (gamma2 path)
      real(SP), allocatable :: dux(:,:), dvy(:,:)
      ! time-derivative intermediates  (gamma2 path)
      real(SP), allocatable :: utx(:,:), vty(:,:)
      real(SP), allocatable :: utxx(:,:), vtyy(:,:), utxy(:,:), vtxy(:,:)
      real(SP), allocatable :: dutx(:,:), dvty(:,:)
      real(SP), allocatable :: dutxx(:,:), dvtyy(:,:), dutxy(:,:), dvtxy(:,:)
   contains
      procedure :: alloc => dws_alloc
      procedure :: free  => dws_free
   end type type_disp_workspace

contains

   subroutine dws_alloc(ws, m, n)
      class(type_disp_workspace), intent(inout) :: ws
      integer, intent(in) :: m, n
      ws%m = m;  ws%n = n
      allocate(ws%du(m,n),    ws%dv(m,n))
      allocate(ws%dut(m,n),   ws%dvt(m,n))
      allocate(ws%uxx(m,n),   ws%uxy(m,n),   ws%vxy(m,n),   ws%vyy(m,n))
      allocate(ws%duxx(m,n),  ws%duxy(m,n),  ws%dvxy(m,n),  ws%dvyy(m,n))
      allocate(ws%ux(m,n),    ws%vx(m,n),    ws%uy(m,n),    ws%vy(m,n))
      allocate(ws%dux(m,n),   ws%dvy(m,n))
      allocate(ws%utx(m,n),   ws%vty(m,n))
      allocate(ws%utxx(m,n),  ws%vtyy(m,n),  ws%utxy(m,n),  ws%vtxy(m,n))
      allocate(ws%dutx(m,n),  ws%dvty(m,n))
      allocate(ws%dutxx(m,n), ws%dvtyy(m,n), ws%dutxy(m,n), ws%dvtxy(m,n))
   end subroutine dws_alloc

   subroutine dws_free(ws)
      class(type_disp_workspace), intent(inout) :: ws
      ws%m = 0;  ws%n = 0
      deallocate(ws%du,    ws%dv,    ws%dut,   ws%dvt)
      deallocate(ws%uxx,   ws%uxy,   ws%vxy,   ws%vyy)
      deallocate(ws%duxx,  ws%duxy,  ws%dvxy,  ws%dvyy)
      deallocate(ws%ux,    ws%vx,    ws%uy,    ws%vy)
      deallocate(ws%dux,   ws%dvy)
      deallocate(ws%utx,   ws%vty)
      deallocate(ws%utxx,  ws%vtyy,  ws%utxy,  ws%vtxy)
      deallocate(ws%dutx,  ws%dvty)
      deallocate(ws%dutxx, ws%dvtyy, ws%dutxy, ws%dvtxy)
   end subroutine dws_free

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
   subroutine cal_dispersion(lp, ws, eta, depth, u, v, u0, v0, p, q, mask9, &
                              inv_dx, inv_dy, dt, min_depth_frc, &
                              beta1, beta2, gamma2, show_breaking, &
                              west_bdy, east_bdy, south_bdy, north_bdy, &
                              etat, ut, vt, etax, etay, &
                              u4, v4, u1p, v1p, u1pp, v1pp, u2, v2, u3, v3)
      type(type_loop_bounds),    intent(in)    :: lp
      type(type_disp_workspace), intent(inout) :: ws
      real(SP), intent(in)    :: eta(:,:), depth(:,:)
      real(SP), intent(in)    :: u(:,:), v(:,:), u0(:,:), v0(:,:)
      real(SP), intent(in)    :: p(:,:), q(:,:)
      integer,  intent(in)    :: mask9(:,:)
      real(SP), intent(in)    :: inv_dx(:,:), inv_dy(:,:)
      real(SP), intent(in)    :: dt, min_depth_frc, beta1, beta2, gamma2
      logical,  intent(in)    :: show_breaking
      logical,  intent(in)    :: west_bdy, east_bdy, south_bdy, north_bdy
      real(SP), intent(out)   :: etat(:,:)
      real(SP), intent(inout) :: ut(:,:), vt(:,:)
      real(SP), intent(inout) :: etax(:,:), etay(:,:)
      real(SP), intent(out)   :: u4(:,:), v4(:,:), u1p(:,:), v1p(:,:)
      real(SP), intent(inout) :: u1pp(:,:), v1pp(:,:)
      real(SP), intent(inout) :: u2(:,:), v2(:,:), u3(:,:), v3(:,:)

      integer  :: i, j
      real(SP) :: rh, rhx, rhy, reta
      real(SP) :: uxxvxy, uxyvyy, huxxhvxy, huxyhvyy
      real(SP) :: inv_dt
      real(SP) :: uxxvxy_x, uxxvxy_y, uxyvyy_x, uxyvyy_y
      real(SP) :: huxxhvxy_x, huxxhvxy_y, huxyhvyy_x, huxyhvyy_y
      real(SP) :: ken1, ken2, ken3, ken4, ken5
      real(SP) :: omega_0, omega_1
      real(SP) :: coeff_a, coeff_b, coeff_1p

      ! zero workspace so ghost-cell neighbours of computed region are 0
      ws%du    = 0.0_SP;  ws%dv    = 0.0_SP
      ws%dut   = 0.0_SP;  ws%dvt   = 0.0_SP
      ws%uxx   = 0.0_SP;  ws%uxy   = 0.0_SP
      ws%vxy   = 0.0_SP;  ws%vyy   = 0.0_SP
      ws%duxx  = 0.0_SP;  ws%duxy  = 0.0_SP
      ws%dvxy  = 0.0_SP;  ws%dvyy  = 0.0_SP
      ws%ux    = 0.0_SP;  ws%vx    = 0.0_SP
      ws%uy    = 0.0_SP;  ws%vy    = 0.0_SP
      ws%dux   = 0.0_SP;  ws%dvy   = 0.0_SP
      ws%utx   = 0.0_SP;  ws%vty   = 0.0_SP
      ws%utxx  = 0.0_SP;  ws%vtyy  = 0.0_SP
      ws%utxy  = 0.0_SP;  ws%vtxy  = 0.0_SP
      ws%dutx  = 0.0_SP;  ws%dvty  = 0.0_SP
      ws%dutxx = 0.0_SP;  ws%dvtyy = 0.0_SP
      ws%dutxy = 0.0_SP;  ws%dvtxy = 0.0_SP

      ! ---- second-order derivatives of u / v --------------------------
      call deriv_xx(lp, inv_dx, mask9, u, ws%uxx)
      call deriv_xy(lp, inv_dx, inv_dy, mask9, u, ws%uxy)
      call deriv_xy(lp, inv_dx, inv_dy, mask9, v, ws%vxy)
      call deriv_yy(lp, inv_dy, mask9, v, ws%vyy)

      ! ---- first-order derivatives (gamma2 or show_breaking) ----------
      if (gamma2 > 0.0_SP) then
         call deriv_x(lp, inv_dx, mask9, u,   ws%ux)
         call deriv_x(lp, inv_dx, mask9, v,   ws%vx)
         call deriv_y(lp, inv_dy, mask9, u,   ws%uy)
         call deriv_y(lp, inv_dy, mask9, v,   ws%vy)
         call deriv_x(lp, inv_dx, mask9, eta, etax)
         call deriv_y(lp, inv_dy, mask9, eta, etay)
      else if (show_breaking) then
         call deriv_x(lp, inv_dx, mask9, eta, etax)
         call deriv_y(lp, inv_dy, mask9, eta, etay)
      end if

      ! ---- DU, DV, ETAT -----------------------------------------------
      do j = 1, lp%nloc - 1
         do i = 1, lp%mloc - 1
            ws%du(i,j) = max(depth(i,j), min_depth_frc) * u(i,j)
            ws%dv(i,j) = max(depth(i,j), min_depth_frc) * v(i,j)
            etat(i,j)  = -(p(i+1,j) - p(i,j)) * inv_dx(i,j) &
                         - (q(i,j+1) - q(i,j)) * inv_dy(i,j)
         end do
      end do

      ! ---- Ut, Vt and their depth-scaled forms (gamma2 only) ----------
      if (gamma2 > 0.0_SP) then
         inv_dt = 1.0_SP/dt
         do j = 1, lp%nloc
            do i = 1, lp%mloc
               ut(i,j)      = (u(i,j) - u0(i,j)) * inv_dt
               vt(i,j)      = (v(i,j) - v0(i,j)) * inv_dt
               ws%dut(i,j)  = max(depth(i,j), min_depth_frc) * ut(i,j)
               ws%dvt(i,j)  = max(depth(i,j), min_depth_frc) * vt(i,j)
            end do
         end do
      end if

      ! ---- second-order derivatives of (h*u) / (h*v) -----------------
      call deriv_xx(lp, inv_dx, mask9, ws%du, ws%duxx)
      call deriv_xy(lp, inv_dx, inv_dy, mask9, ws%du, ws%duxy)
      call deriv_xy(lp, inv_dx, inv_dy, mask9, ws%dv, ws%dvxy)
      call deriv_yy(lp, inv_dy, mask9, ws%dv, ws%dvyy)

      ! ---- additional derivatives for gamma2 terms --------------------
      if (gamma2 > 0.0_SP) then
         call deriv_x(lp, inv_dx, mask9, ws%du,  ws%dux)
         call deriv_y(lp, inv_dy, mask9, ws%dv,  ws%dvy)
         call deriv_x(lp, inv_dx, mask9, ut,     ws%utx)
         call deriv_y(lp, inv_dy, mask9, vt,     ws%vty)
         call deriv_xx(lp, inv_dx, mask9, ut,    ws%utxx)
         call deriv_yy(lp, inv_dy, mask9, vt,    ws%vtyy)
         call deriv_xy(lp, inv_dx, inv_dy, mask9, ut, ws%utxy)
         call deriv_xy(lp, inv_dx, inv_dy, mask9, vt, ws%vtxy)
         call deriv_x(lp, inv_dx, mask9, ws%dut, ws%dutx)
         call deriv_y(lp, inv_dy, mask9, ws%dvt, ws%dvty)
         call deriv_xx(lp, inv_dx, mask9, ws%dut, ws%dutxx)
         call deriv_yy(lp, inv_dy, mask9, ws%dvt, ws%dvtyy)
         call deriv_xy(lp, inv_dx, inv_dy, mask9, ws%dut, ws%dutxy)
         call deriv_xy(lp, inv_dx, inv_dy, mask9, ws%dvt, ws%dvtxy)

         ! zero cross-derivatives at domain faces (Neumann-type BC)
         if (west_bdy) then
            ws%uxy(lp%ib,:)  = 0.0_SP;  ws%vxy(lp%ib,:)  = 0.0_SP
            ws%duxy(lp%ib,:) = 0.0_SP;  ws%dvxy(lp%ib,:) = 0.0_SP
            ws%utxy(lp%ib,:) = 0.0_SP;  ws%vtxy(lp%ib,:) = 0.0_SP
            ws%dutxy(lp%ib,:) = 0.0_SP; ws%dvtxy(lp%ib,:) = 0.0_SP
         end if
         if (east_bdy) then
            ws%uxy(lp%ie,:)  = 0.0_SP;  ws%vxy(lp%ie,:)  = 0.0_SP
            ws%duxy(lp%ie,:) = 0.0_SP;  ws%dvxy(lp%ie,:) = 0.0_SP
            ws%utxy(lp%ie,:) = 0.0_SP;  ws%vtxy(lp%ie,:) = 0.0_SP
            ws%dutxy(lp%ie,:) = 0.0_SP; ws%dvtxy(lp%ie,:) = 0.0_SP
         end if
         if (south_bdy) then
            ws%uxy(:,lp%jb)  = 0.0_SP;  ws%vxy(:,lp%jb)  = 0.0_SP
            ws%duxy(:,lp%jb) = 0.0_SP;  ws%dvxy(:,lp%jb) = 0.0_SP
            ws%utxy(:,lp%jb) = 0.0_SP;  ws%vtxy(:,lp%jb) = 0.0_SP
            ws%dutxy(:,lp%jb) = 0.0_SP; ws%dvtxy(:,lp%jb) = 0.0_SP
         end if
         if (north_bdy) then
            ws%uxy(:,lp%je)  = 0.0_SP;  ws%vxy(:,lp%je)  = 0.0_SP
            ws%duxy(:,lp%je) = 0.0_SP;  ws%dvxy(:,lp%je) = 0.0_SP
            ws%utxy(:,lp%je) = 0.0_SP;  ws%vtxy(:,lp%je) = 0.0_SP
            ws%dutxy(:,lp%je) = 0.0_SP; ws%dvtxy(:,lp%je) = 0.0_SP
         end if
      end if

      ! ---- linear dispersion: u4/v4, u1p/v1p -------------------------
      coeff_a  = 1.0_SP/3.0_SP - beta1 + 0.5_SP*beta1*beta1
      coeff_b  = beta1 - 0.5_SP
      coeff_1p = 0.5_SP*(1.0_SP - beta1)*(1.0_SP - beta1)

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uxxvxy   = ws%uxx(i,j)  + ws%vxy(i,j)
            uxyvyy   = ws%uxy(i,j)  + ws%vyy(i,j)
            huxxhvxy = ws%duxx(i,j) + ws%dvxy(i,j)
            huxyhvyy = ws%duxy(i,j) + ws%dvyy(i,j)
            rh = depth(i,j)

            u4(i,j)  = coeff_a*rh*rh*uxxvxy  + coeff_b*rh*huxxhvxy
            v4(i,j)  = coeff_a*rh*rh*uxyvyy  + coeff_b*rh*huxyhvyy
            u1p(i,j) = coeff_1p*rh*rh*uxxvxy + (beta1 - 1.0_SP)*rh*huxxhvxy
            v1p(i,j) = coeff_1p*rh*rh*uxyvyy + (beta1 - 1.0_SP)*rh*huxyhvyy

            ! gamma2 nonlinear addition to u4/v4
            if (gamma2 > 0.0_SP) then
               reta = eta(i,j)
               ken1 = (1.0_SP/6.0_SP - beta1 + beta1*beta1)*rh*reta*beta2 &
                      + (0.5_SP*beta1*beta1 - 1.0_SP/6.0_SP)*reta*reta*beta2*beta2
               ken2 = (beta1 - 0.5_SP)*reta*beta2
               u4(i,j) = u4(i,j) + gamma2*mask9(i,j)*(ken1*uxxvxy  + ken2*huxxhvxy)
               v4(i,j) = v4(i,j) + gamma2*mask9(i,j)*(ken1*uxyvyy  + ken2*huxyhvyy)
            end if
         end do
      end do

      ! ---- nonlinear dispersion terms (gamma2 > 0 only) ---------------
      if (gamma2 <= 0.0_SP) return

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uxxvxy   = ws%uxx(i,j)  + ws%vxy(i,j)
            uxyvyy   = ws%uxy(i,j)  + ws%vyy(i,j)
            huxxhvxy = ws%duxx(i,j) + ws%dvxy(i,j)
            huxyhvyy = ws%duxy(i,j) + ws%dvyy(i,j)

            uxxvxy_x  = (ws%uxx(i+1,j)  + ws%vxy(i+1,j)  - ws%uxx(i-1,j)  - ws%vxy(i-1,j)) &
                        * 0.5_SP * inv_dx(i,j)
            uxxvxy_y  = (ws%uxx(i,j+1)  + ws%vxy(i,j+1)  - ws%uxx(i,j-1)  - ws%vxy(i,j-1)) &
                        * 0.5_SP * inv_dy(i,j)
            uxyvyy_x  = (ws%uxy(i+1,j)  + ws%vyy(i+1,j)  - ws%uxy(i-1,j)  - ws%vyy(i-1,j)) &
                        * 0.5_SP * inv_dx(i,j)
            uxyvyy_y  = (ws%uxy(i,j+1)  + ws%vyy(i,j+1)  - ws%uxy(i,j-1)  - ws%vyy(i,j-1)) &
                        * 0.5_SP * inv_dy(i,j)
            huxxhvxy_x = (ws%duxx(i+1,j) + ws%dvxy(i+1,j) - ws%duxx(i-1,j) - ws%dvxy(i-1,j)) &
                         * 0.5_SP * inv_dx(i,j)
            huxxhvxy_y = (ws%duxx(i,j+1) + ws%dvxy(i,j+1) - ws%duxx(i,j-1) - ws%dvxy(i,j-1)) &
                         * 0.5_SP * inv_dy(i,j)
            huxyhvyy_x = (ws%duxy(i+1,j) + ws%dvyy(i+1,j) - ws%duxy(i-1,j) - ws%dvyy(i-1,j)) &
                         * 0.5_SP * inv_dx(i,j)
            huxyhvyy_y = (ws%duxy(i,j+1) + ws%dvyy(i,j+1) - ws%duxy(i,j-1) - ws%dvyy(i,j-1)) &
                         * 0.5_SP * inv_dy(i,j)

            rh   = depth(i,j)
            rhx  = (depth(i+1,j) - depth(i-1,j)) * 0.5_SP * inv_dx(i,j)
            rhy  = (depth(i,j+1) - depth(i,j-1)) * 0.5_SP * inv_dy(i,j)
            reta = eta(i,j)

            ! ---- U1pp / V1pp  (time-derivative correction) -----------
            u1pp(i,j) = -reta*beta2*etax(i,j)*beta2*(ws%utx(i,j) + ws%vty(i,j))   &
                        - 0.5_SP*reta*reta*beta2*beta2*(ws%utxx(i,j) + ws%vtxy(i,j)) &
                        - etax(i,j)*beta2*(ws%dutx(i,j) + ws%dvty(i,j))              &
                        - reta*beta2*(ws%dutxx(i,j) + ws%dvtxy(i,j))

            v1pp(i,j) = -reta*beta2*etay(i,j)*beta2*(ws%utx(i,j) + ws%vty(i,j))   &
                        - 0.5_SP*reta*reta*beta2*beta2*(ws%utxy(i,j) + ws%vtyy(i,j)) &
                        - etay(i,j)*beta2*(ws%dutx(i,j) + ws%dvty(i,j))              &
                        - reta*beta2*(ws%dutxy(i,j) + ws%dvtyy(i,j))

            ken1 = beta1*(1.0_SP - beta1)*rh*etat(i,j)*beta2 &
                   - beta1*beta1*reta*beta2*etat(i,j)*beta2
            ken2 = beta1*(1.0_SP - beta1)*rh*reta*beta2 &
                   - 0.5_SP*beta1*beta1*reta*reta*beta2*beta2
            ken3 = beta1*etat(i,j)*beta2
            ken4 = beta1*reta*beta2

            u1pp(i,j) = u1pp(i,j) - ken1*uxxvxy                              &
                        - ken2*(ws%utxx(i,j) + ws%vtxy(i,j))                  &
                        + ken3*huxxhvxy + ken4*(ws%dutxx(i,j) + ws%dvtxy(i,j))
            v1pp(i,j) = v1pp(i,j) - ken1*uxyvyy                              &
                        - ken2*(ws%utxy(i,j) + ws%vtyy(i,j))                  &
                        + ken3*huxyhvyy + ken4*(ws%dutxy(i,j) + ws%dvtyy(i,j))

            ! ---- U2 / V2  (nonlinear advection-type dispersion) ------
            ken1 = (beta1 - 1.0_SP)*(rhx + etax(i,j))*beta2
            ken2 = (beta1 - 1.0_SP)*(rh  + reta)*beta2
            ken3 = (1.0_SP - beta1)*(1.0_SP - beta1)*rh*rhx*beta2*beta2             &
                   - beta1*(1.0_SP - beta1)*(rhx*reta + rh*etax(i,j))*beta2         &
                   + (beta1*beta1 - 1.0_SP)*reta*etax(i,j)*beta2*beta2
            ken4 = 0.5_SP*(1.0_SP - beta1)*(1.0_SP - beta1)*rh*rh*beta2*beta2      &
                   - beta1*(1.0_SP - beta1)*rh*reta*beta2                            &
                   + 0.5_SP*(beta1*beta1 - 1.0_SP)*reta*reta*beta2*beta2
            ken5 = (1.0_SP - beta1)*(1.0_SP - beta1)*rh*rhy*beta2*beta2             &
                   - beta1*(1.0_SP - beta1)*(rhy*reta + rh*etay(i,j))*beta2         &
                   + (beta1*beta1 - 1.0_SP)*reta*etay(i,j)*beta2*beta2

            u2(i,j) = ken1*(u(i,j)*huxxhvxy + v(i,j)*huxyhvyy)                     &
                      + ken2*(ws%ux(i,j)*huxxhvxy + u(i,j)*huxxhvxy_x               &
                              + ws%vx(i,j)*huxyhvyy + v(i,j)*huxyhvyy_x)            &
                      + ken3*(u(i,j)*uxxvxy + v(i,j)*uxyvyy)                        &
                      + ken4*(ws%ux(i,j)*uxxvxy  + u(i,j)*uxxvxy_x                  &
                              + ws%vx(i,j)*uxyvyy + v(i,j)*uxyvyy_x)                &
                      + beta2*beta2*(ws%dux(i,j) + ws%dvy(i,j)                      &
                                     + reta*beta2*(ws%ux(i,j) + ws%vy(i,j)))        &
                      * (huxxhvxy + etax(i,j)*beta2*(ws%ux(i,j) + ws%vy(i,j))       &
                         + reta*beta2*uxxvxy)

            ken1 = (beta1 - 1.0_SP)*(rhy + etay(i,j))*beta2

            v2(i,j) = ken1*(u(i,j)*huxxhvxy + v(i,j)*huxyhvyy)                     &
                      + ken2*(ws%uy(i,j)*huxxhvxy + u(i,j)*huxxhvxy_y               &
                              + ws%vy(i,j)*huxyhvyy + v(i,j)*huxyhvyy_y)            &
                      + ken5*(u(i,j)*uxxvxy + v(i,j)*uxyvyy)                        &
                      + ken4*(ws%uy(i,j)*uxxvxy  + u(i,j)*uxxvxy_y                  &
                              + ws%vy(i,j)*uxyvyy + v(i,j)*uxyvyy_y)                &
                      + beta2*beta2*(ws%dux(i,j) + ws%dvy(i,j)                      &
                                     + reta*beta2*(ws%ux(i,j) + ws%vy(i,j)))        &
                      * (huxyhvyy + etay(i,j)*beta2*(ws%ux(i,j) + ws%vy(i,j))       &
                         + reta*beta2*uxyvyy)

            ! ---- U3 / V3  (vorticity-type dispersion) ----------------
            omega_0 = ws%vx(i,j) - ws%uy(i,j)
            omega_1 = ((beta1-1.0_SP)*rhx + beta1*etax(i,j))*beta2                  &
                      * (huxyhvyy + ((beta1-1.0_SP)*rh + beta1*reta)*beta2*uxyvyy)  &
                      - ((beta1-1.0_SP)*rhy + beta1*etay(i,j))*beta2                &
                      * (huxxhvxy + ((beta1-1.0_SP)*rh + beta1*reta)*beta2*uxxvxy)

            ken1 = (beta1 - 0.5_SP)*(reta + rh)*beta2
            ken2 = (1.0_SP/3.0_SP - beta1 + 0.5_SP*beta1*beta1)*rh*rh*beta2*beta2  &
                   + (1.0_SP/6.0_SP - beta1 + beta1*beta1)*rh*reta*beta2            &
                   + (0.5_SP*beta1*beta1 - 1.0_SP/6.0_SP)*reta*reta*beta2*beta2

            u3(i,j) = -v(i,j)*omega_1 - omega_0*(ken1*huxyhvyy + ken2*uxyvyy)
            v3(i,j) =  u(i,j)*omega_1 + omega_0*(ken1*huxxhvxy + ken2*uxxvxy)
         end do
      end do

   end subroutine cal_dispersion

end module model_kernel_dispersion_mod
