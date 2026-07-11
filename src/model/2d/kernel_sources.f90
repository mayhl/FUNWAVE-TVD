! allow(E001)
module model_kernel_sources_mod
   use core_constants_mod, only: SP, GRAV
   use core_grid_mod, only: type_loop_bounds
   implicit none
   private

   public :: cal_sources

contains

   ! ----------------------------------------------------------------
   ! cal_sources — Cartesian momentum source terms (Shi et al. 2012).
   !
   ! Contributes to the RHS of d(HU)/dt and d(HV)/dt:
   !
   !   1. Bathymetric slope:  g*eta*(depth_x(i+1)-depth_x(i))/dx
   !      depth_x(i,j) = x-face staggered depth  (Depthx in legacy).
   !
   !   2. Bottom friction:  -Cd * u * |UV|
   !      Cd is the effective drag (Manning formula pre-applied by caller
   !      via friction%update_cd when manning=YES).
   !
   !   3. Dispersive source (gamma1 > 0, dispersion=.true.):
   !        H*(u∇u4 + u4∇u − gamma2*(u1pp+u2+u3)) + div(p,q)*(u4−u1p)
   !      u1pp / u2 / u3 from cal_dispersion gamma2 path; pass zero
   !      arrays when gamma2 = 0.
   !
   !   4. Wavemaker mass injection:  wavemaker_mass * u / v
   !      Pass zero array when no wavemaker.
   !
   !   5. Eddy viscosity (BreakSourceX/Y, Shi 2012 §3):
   !        d/dx[(νa+νb)*dHU/dx] + d/dy[(νa+νb)*dHU/dy]
   !      nu_vis = nu_break + nu_sponge assembled by caller.
   !      Pass zero array when eddy viscosity is inactive.
   !      hu/hv are the CELL-CENTRED fluxes (legacy HU/HV) — the
   !      interface p/q feed div(p,q) only.
   !
   !   6. Coriolis (coriolis_on; f-plane today, CRS per-cell f later):
   !        $$ S_x \mathrel{+}= f\,\tfrac12(q_{i,j} + q_{i,j+1}), \qquad
   !           S_y \mathrel{-}= f\,\tfrac12(p_{i,j} + p_{i+1,j}) $$
   !      exact legacy spherical-branch face-average form (sources.F);
   !      gated, not zero-added — legacy Cartesian has no such term.
   !
   !   7. Breakwater friction (breakwater_on):
   !        $$ S \mathrel{-}= c_{d,bw}\,u\,|UV|\,d $$
   !      legacy multiplies by the STILL-WATER depth d, not H ("we used
   !      flux, so need multiply D"); added last, matching the legacy
   !      += order after every other term.
   ! ----------------------------------------------------------------
   subroutine cal_sources(lp, gamma1, gamma2, dispersion, coriolis_on, &
                          breakwater_on, mask, mask9, inv_dx, inv_dy, &
                          depth, depth_x, depth_y, eta, h, u, v, p, q, hu, hv, &
                          u4, v4, u1p, v1p, u1pp, v1pp, u2, v2, u3, v3, &
                          wavemaker_mass, cd, nu_vis, coriolis, &
                          cd_breakwater, min_depth_frc, src_x, src_y)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: gamma1, gamma2
      logical, intent(in)  :: dispersion, coriolis_on, breakwater_on
      integer, intent(in)  :: mask(:, :), mask9(:, :)
      real(SP), intent(in)  :: inv_dx(:, :), inv_dy(:, :)
      real(SP), intent(in)  :: depth(:, :), depth_x(:, :), depth_y(:, :)
      real(SP), intent(in)  :: eta(:, :), h(:, :), u(:, :), v(:, :)
      real(SP), intent(in)  :: p(:, :), q(:, :), hu(:, :), hv(:, :)
      real(SP), intent(in)  :: u4(:, :), v4(:, :), u1p(:, :), v1p(:, :)
      real(SP), intent(in)  :: u1pp(:, :), v1pp(:, :)
      real(SP), intent(in)  :: u2(:, :), v2(:, :), u3(:, :), v3(:, :)
      real(SP), intent(in)  :: wavemaker_mass(:, :), cd(:, :), nu_vis(:, :)
      real(SP), intent(in)  :: coriolis(:, :), cd_breakwater(:, :)
      real(SP), intent(in)  :: min_depth_frc
      real(SP), intent(out) :: src_x(:, :), src_y(:, :)

      integer  :: i, j
      real(SP) :: spd, heff
      real(SP) :: u4x, u4y, v4x, v4y, ux, uy, vx, vy, div_pq

      src_x = 0.0_SP; src_y = 0.0_SP

      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            spd = sqrt(u(i, j)**2 + v(i, j)**2)

            ! bathy slope
            src_x(i, j) = GRAV*eta(i, j)*(depth_x(i + 1, j) - depth_x(i, j)) &
                          *inv_dx(i, j)*mask(i, j)
            src_y(i, j) = GRAV*eta(i, j)*(depth_y(i, j + 1) - depth_y(i, j)) &
                          *inv_dy(i, j)*mask(i, j)

            ! friction (Cd already effective: linear drag or Manning pre-applied)
            src_x(i, j) = src_x(i, j) - cd(i, j)*u(i, j)*spd
            src_y(i, j) = src_y(i, j) - cd(i, j)*v(i, j)*spd

            ! wavemaker mass injection
            src_x(i, j) = src_x(i, j) + wavemaker_mass(i, j)*u(i, j)
            src_y(i, j) = src_y(i, j) + wavemaker_mass(i, j)*v(i, j)

            ! Coriolis on the face-averaged fluxes
            if (coriolis_on) then
               src_x(i, j) = src_x(i, j) &
                             + coriolis(i, j)*0.5_SP*(q(i, j) + q(i, j + 1))
               src_y(i, j) = src_y(i, j) &
                             - coriolis(i, j)*0.5_SP*(p(i, j) + p(i + 1, j))
            end if

            if (dispersion) then
               heff = max(h(i, j), min_depth_frc)
               ! centred gradients of u4, v4, u, v
               u4x = (u4(i + 1, j) - u4(i - 1, j))*0.5_SP*inv_dx(i, j)
               u4y = (u4(i, j + 1) - u4(i, j - 1))*0.5_SP*inv_dy(i, j)
               v4x = (v4(i + 1, j) - v4(i - 1, j))*0.5_SP*inv_dx(i, j)
               v4y = (v4(i, j + 1) - v4(i, j - 1))*0.5_SP*inv_dy(i, j)
               ux = (u(i + 1, j) - u(i - 1, j))*0.5_SP*inv_dx(i, j)
               uy = (u(i, j + 1) - u(i, j - 1))*0.5_SP*inv_dy(i, j)
               vx = (v(i + 1, j) - v(i - 1, j))*0.5_SP*inv_dx(i, j)
               vy = (v(i, j + 1) - v(i, j - 1))*0.5_SP*inv_dy(i, j)

               ! div(P,Q) = d(HU)/dx + d(HV)/dy  (forward differenced, matches etat sign)
               div_pq = (p(i + 1, j) - p(i, j))*inv_dx(i, j) &
                        + (q(i, j + 1) - q(i, j))*inv_dy(i, j)

               src_x(i, j) = src_x(i, j) &
                             + gamma1*mask9(i, j)*( &
                             heff*(u(i, j)*u4x + v(i, j)*u4y + u4(i, j)*ux + v4(i, j)*uy &
                                   - gamma2*mask9(i, j)*(u1pp(i, j) + u2(i, j) + u3(i, j))) &
                             + div_pq*(u4(i, j) - u1p(i, j)))

               src_y(i, j) = src_y(i, j) &
                             + gamma1*mask9(i, j)*( &
                             heff*(u(i, j)*v4x + v(i, j)*v4y + u4(i, j)*vx + v4(i, j)*vy &
                                   - gamma2*mask9(i, j)*(v1pp(i, j) + v2(i, j) + v3(i, j))) &
                             + div_pq*(v4(i, j) - v1p(i, j)))
            end if

            ! breakwater friction on the still-water depth
            if (breakwater_on) then
               src_x(i, j) = src_x(i, j) &
                             - cd_breakwater(i, j)*u(i, j)*spd*depth(i, j)
               src_y(i, j) = src_y(i, j) &
                             - cd_breakwater(i, j)*v(i, j)*spd*depth(i, j)
            end if

         end do
      end do

      ! eddy viscosity: d/dx[(ν_r+ν_l)*dHU/dx] + d/dy[(ν_u+ν_d)*dHU/dy]
      ! Cell-centred hu/hv (legacy BreakSource PQ_scheme=.FALSE. form).
      ! Active when nu_vis > 0 anywhere.
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            src_x(i, j) = src_x(i, j) &
                          + 0.5_SP*inv_dx(i, j)*( &
                          (nu_vis(i + 1, j) + nu_vis(i, j))*inv_dx(i, j)*(hu(i + 1, j) - hu(i, j)) &
                          - (nu_vis(i - 1, j) + nu_vis(i, j))*inv_dx(i, j)*(hu(i, j) - hu(i - 1, j))) &
                          + 0.5_SP*inv_dy(i, j)*( &
                          (nu_vis(i, j + 1) + nu_vis(i, j))*inv_dy(i, j)*(hu(i, j + 1) - hu(i, j)) &
                          - (nu_vis(i, j - 1) + nu_vis(i, j))*inv_dy(i, j)*(hu(i, j) - hu(i, j - 1)))
            src_y(i, j) = src_y(i, j) &
                          + 0.5_SP*inv_dx(i, j)*( &
                          (nu_vis(i + 1, j) + nu_vis(i, j))*inv_dx(i, j)*(hv(i + 1, j) - hv(i, j)) &
                          - (nu_vis(i - 1, j) + nu_vis(i, j))*inv_dx(i, j)*(hv(i, j) - hv(i - 1, j))) &
                          + 0.5_SP*inv_dy(i, j)*( &
                          (nu_vis(i, j + 1) + nu_vis(i, j))*inv_dy(i, j)*(hv(i, j + 1) - hv(i, j)) &
                          - (nu_vis(i, j - 1) + nu_vis(i, j))*inv_dy(i, j)*(hv(i, j) - hv(i, j - 1)))
         end do
      end do

   end subroutine cal_sources

end module model_kernel_sources_mod
