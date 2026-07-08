! allow(E001)
module model_kernel_breaker_mod
   use core_constants_mod, only: SP, GRAV, DEG2RAD, RAD2DEG
   use core_grid_mod,      only: type_loop_bounds
   implicit none
   private

   public :: wave_breaking
   public :: VIS_SCHEME_DEFAULT, VIS_SCHEME_KENNEDY, VIS_SCHEME_KENNEDY_ORIG
   public :: VIS_SCHEME_STATIC_TRANS, VIS_SCHEME_DEPTH_RATIO

   integer, parameter :: VIS_SCHEME_DEFAULT      = 0
   integer, parameter :: VIS_SCHEME_KENNEDY       = 1
   integer, parameter :: VIS_SCHEME_KENNEDY_ORIG  = 2
   integer, parameter :: VIS_SCHEME_STATIC_TRANS  = 3
   integer, parameter :: VIS_SCHEME_DEPTH_RATIO   = 4

   real(SP), parameter :: SMALL = 1.0e-6_SP

   ! Empirical roller coefficients (values carried over from legacy breaker.F).
   real(SP), parameter :: ROLLER_COEF  = 0.45_SP    !< r = |ROLLER_COEF*etat/c|
   real(SP), parameter :: ROLLER_R_MAX = 0.1638_SP  !< cap on roller ratio r

contains

   ! ----------------------------------------------------------------
   ! Eddy-viscosity wave-breaking: updates nu_break and age, and
   ! computes roller flux / undertow velocities where breaking is active.
   !
   ! vis_scheme selects the viscosity formula:
   !   VIS_SCHEME_DEFAULT      -- nu = cap1*cbrk2*c + nu_bkg
   !   VIS_SCHEME_KENNEDY      -- modified Kennedy (no etat_star)
   !   VIS_SCHEME_KENNEDY_ORIG -- original Kennedy et al. with etat_star
   !   VIS_SCHEME_STATIC_TRANS -- static age-weighted transition
   !   VIS_SCHEME_DEPTH_RATIO  -- ratio-based detection (overrides age logic)
   !
   ! in_wm_zone: .true. for cells inside the wavemaker exclusion region.
   !   Computed once by the caller; removes MPI-rank-specific coordinate
   !   arithmetic from the kernel.
   !
   ! Loop runs over lp%jb-1..lp%je+1 x lp%ib-1..lp%ie+1 so that
   ! ghost-cell nu_break values are available to the flux kernel.
   ! Age ghost cells must be exchanged by the caller before this call.
   ! ----------------------------------------------------------------
   subroutine wave_breaking(lp, etax, etay, etat, eta, depth, h, u, v, etamean, &
                             dx, dy, dt, t_brk, min_depth_frc, &
                             cbrk1, cbrk2, wavemaker_cbrk, nu_bkg, &
                             vis_scheme, swe_eta_dep, in_wm_zone, &
                             nu_break, age, roller_flux, undertow_u, undertow_v)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: etax(:,:), etay(:,:), etat(:,:)
      real(SP), intent(in)  :: eta(:,:), depth(:,:), h(:,:)
      real(SP), intent(in)  :: u(:,:), v(:,:), etamean(:,:)
      real(SP), intent(in)  :: dx(:,:), dy(:,:)
      real(SP), intent(in)  :: dt, t_brk, min_depth_frc
      real(SP), intent(in)  :: cbrk1, cbrk2, wavemaker_cbrk, nu_bkg
      integer,  intent(in)  :: vis_scheme
      real(SP), intent(in)  :: swe_eta_dep
      logical,  intent(in)  :: in_wm_zone(:,:)
      real(SP), intent(inout) :: nu_break(:,:), age(:,:)
      real(SP), intent(inout) :: roller_flux(:,:), undertow_u(:,:), undertow_v(:,:)

      integer  :: i, j
      real(SP) :: c_shallow, thr1, thr2, cap1
      real(SP) :: angle, c, c1, r, b
      real(SP) :: age1, age2, age3
      real(SP) :: propx, propy, propxy
      real(SP) :: etat_star, t_star
      real(SP) :: dxg, dyg, slope_mag

      do j = lp%jb - 1, lp%je + 1
         do i = lp%ib - 1, lp%ie + 1

            c_shallow = sqrt(GRAV * max(min_depth_frc, h(i,j)))
            thr1 = cbrk1 * c_shallow
            thr2 = cbrk2 * c_shallow
            dxg  = dx(i,j)
            dyg  = dy(i,j)

            angle = atan2(-etay(i,j), -etax(i,j)) * RAD2DEG

            ! ---- VIS_DEPTH_RATIO: ratio-based detection, no age -----
            if (vis_scheme == VIS_SCHEME_DEPTH_RATIO) then
               if (abs(eta(i,j)) / max(depth(i,j), min_depth_frc) > swe_eta_dep) then
                  cap1 = max(depth(i,j), min_depth_frc) + eta(i,j)
                  b = 0.0_SP
                  if (etat(i,j) > thr1 .and. etat(i,j) <= 2.0_SP*thr1) then
                     b = etat(i,j)/thr1 - 1.0_SP
                  else if (etat(i,j) > 2.0_SP*thr1) then
                     b = 1.0_SP
                  end if
                  nu_break(i,j) = cap1*thr2*(1.0_SP + b) + nu_bkg
               end if
               cycle
            end if

            ! ---- age-based detection --------------------------------
            slope_mag = max(sqrt(etax(i,j)*etax(i,j) + etay(i,j)*etay(i,j)), SMALL)

            if (etat(i,j) >= thr1 .and. &
                (age(i,j) == 0.0_SP .or. age(i,j) > t_brk)) then
               age(i,j) = dt
            else
               if (age(i,j) > 0.0_SP) then
                  age(i,j) = age(i,j) + dt
               else
                  c = min(abs(etat(i,j)) / slope_mag, sqrt(GRAV * abs(h(i,j))))
                  propxy = sqrt(dxg*dxg + dyg*dyg) / max(c, SMALL)
                  propx  = dxg / max(c, SMALL)
                  propy  = dyg / max(c, SMALL)

                  if (etat(i,j) >= thr2) then
                     if (angle >= 0.0_SP .and. angle < 90.0_SP) then
                        age1 = age(i-1,j);  age2 = age(i-1,j-1);  age3 = age(i,j-1)
                        if ((age1 >= dt .and. age1 > propx)  .or. &
                            (age2 >= dt .and. age2 > propxy) .or. &
                            (age3 >= dt .and. age3 > propy))  age(i,j) = dt
                     else if (angle >= 90.0_SP .and. angle < 180.0_SP) then
                        age1 = age(i+1,j);  age2 = age(i+1,j-1);  age3 = age(i,j-1)
                        if ((age1 >= dt .and. age1 > propx)  .or. &
                            (age2 >= dt .and. age2 > propxy) .or. &
                            (age3 >= dt .and. age3 > propy))  age(i,j) = dt
                     else if (angle >= -180.0_SP .and. angle < -90.0_SP) then
                        age1 = age(i+1,j);  age2 = age(i+1,j+1);  age3 = age(i,j+1)
                        if ((age1 >= dt .and. age1 > propx)  .or. &
                            (age2 >= dt .and. age2 > propxy) .or. &
                            (age3 >= dt .and. age3 > propy))  age(i,j) = dt
                     else if (angle >= -90.0_SP .and. angle < 0.0_SP) then
                        age1 = age(i,j+1);  age2 = age(i-1,j+1);  age3 = age(i-1,j)
                        if ((age1 >= dt .and. age1 > propy)  .or. &
                            (age2 >= dt .and. age2 > propxy) .or. &
                            (age3 >= dt .and. age3 > propx))  age(i,j) = dt
                     end if
                  end if
               end if
            end if

            ! ---- set viscosity --------------------------------------
            if (in_wm_zone(i,j)) then
               if (etat(i,j) > min(thr2, wavemaker_cbrk*c_shallow)) then
                  cap1 = max(depth(i,j), min_depth_frc) + eta(i,j)
                  nu_break(i,j) = cap1*wavemaker_cbrk*c_shallow + nu_bkg
               else
                  nu_break(i,j) = nu_bkg
               end if
            else
               if (age(i,j) > 0.0_SP .and. age(i,j) < t_brk .and. &
                   etat(i,j) > thr2) then
                  cap1 = max(depth(i,j), min_depth_frc) + eta(i,j)

                  select case (vis_scheme)
                  case (VIS_SCHEME_KENNEDY_ORIG)
                     etat_star = thr2
                     t_star = 5.0_SP * sqrt(max(depth(i,j), min_depth_frc) / GRAV)
                     if (age(i,j) >= 0.0_SP .and. age(i,j) < t_star) &
                        etat_star = thr1 + age(i,j)/t_star*(thr2 - thr1)
                     b = 0.0_SP
                     if (etat(i,j) > etat_star .and. etat(i,j) <= 2.0_SP*etat_star) then
                        b = etat(i,j)/etat_star - 1.0_SP
                     else if (etat(i,j) > 2.0_SP*etat_star) then
                        b = 1.0_SP
                     end if
                     nu_break(i,j) = cap1*abs(etat(i,j))*b + nu_bkg

                  case (VIS_SCHEME_KENNEDY)
                     b = 0.0_SP
                     if (etat(i,j) > thr1 .and. etat(i,j) <= 2.0_SP*thr1) then
                        b = etat(i,j)/thr1 - 1.0_SP
                     else if (etat(i,j) > 2.0_SP*thr1) then
                        b = 1.0_SP
                     end if
                     nu_break(i,j) = cap1*thr2*(1.0_SP + b) + nu_bkg

                  case (VIS_SCHEME_STATIC_TRANS)
                     nu_break(i,j) = cap1*c_shallow &
                                     *(cbrk2 + (cbrk1 - cbrk2)*(t_brk - age(i,j))/t_brk) &
                                     + nu_bkg

                  case default
                     nu_break(i,j) = cap1*thr2 + nu_bkg
                  end select

                  ! ---- roller flux and undertow --------------------
                  c1 = max(abs(etat(i,j))/slope_mag, sqrt(GRAV*abs(h(i,j))))
                  r  = abs(ROLLER_COEF * etat(i,j) / max(c1, SMALL))
                  r  = min(r, ROLLER_R_MAX)

                  roller_flux(i,j) = abs(c1 - sqrt(u(i,j)*u(i,j) + v(i,j)*v(i,j))) &
                                     * r * (eta(i,j) - etamean(i,j))
                  undertow_u(i,j)  = -roller_flux(i,j) * cos(angle * DEG2RAD)
                  undertow_v(i,j)  = -roller_flux(i,j) * sin(angle * DEG2RAD)
               else
                  nu_break(i,j) = nu_bkg
               end if
            end if

         end do
      end do

   end subroutine wave_breaking

end module model_kernel_breaker_mod
