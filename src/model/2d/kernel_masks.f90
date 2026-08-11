! allow(E001)
module model_kernel_masks_mod
   use core_constants_mod, only: SP
   use core_grid_mod, only: type_loop_bounds
   implicit none
   private

   public :: update_mask, update_mask9, update_swe_weight

contains

   ! ----------------------------------------------------------------
   ! Flood/dry update of MASK and optional depth-gradient truncation.
   !
   ! Flooding: a dry cell becomes wet when a wetter wet neighbour
   ! exists (or when eta > -depth for the rainfall case).
   ! Drying:   a wet cell with eta < -depth becomes dry.
   !
   ! truncate_depth: when .true., depthx/depthy at newly dry cell
   !   boundaries are reset to the single-sided depth value (prevents
   !   large numerical depth gradients that destabilise the scheme).
   !   Set .false. to match the -DIGNORE_BIG_SLOPE compile-time flag.
   !
   ! MPI ghost-cell exchange is NOT performed here; the caller must
   ! exchange MASK and MASK9 before the next kernel step.
   ! NOT OMP-threaded: a newly-dry cell writes its neighbours' faces
   ! (depthx(i+1,j)/depthy(i,j+1)) with values that differ from what the
   ! neighbour's own iteration writes — order-dependent under threads.
   ! The face-owned gather that fixes it reproduces legacy bitwise (dry
   ! cell wins its own face) — parked for the GPU pass.
   ! ----------------------------------------------------------------
   subroutine update_mask(lp, eta, depth, mask_struc, mask, &
                          depthx, depthy, truncate_depth)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)    :: eta(:, :), depth(:, :)
      integer, intent(in)    :: mask_struc(:, :)
      integer, intent(inout) :: mask(:, :)
      real(SP), intent(inout) :: depthx(:, :), depthy(:, :)
      logical, intent(in)    :: truncate_depth

      integer :: masktmp(lp%mloc, lp%nloc)
      integer :: i, j

      masktmp = mask

      do j = lp%jb - 2, lp%je + 2
         do i = lp%ib - 2, lp%ie + 2
            if (mask_struc(i, j) /= 1) cycle

            if (mask(i, j) < 1) then
               ! flooding: dry cell may become wet
               if (eta(i, j) > -depth(i, j)) masktmp(i, j) = 1
               if (mask(i - 1, j) == 1 .and. eta(i - 1, j) > eta(i, j)) masktmp(i, j) = 1
               if (mask(i + 1, j) == 1 .and. eta(i + 1, j) > eta(i, j)) masktmp(i, j) = 1
               if (mask(i, j - 1) == 1 .and. eta(i, j - 1) > eta(i, j)) masktmp(i, j) = 1
               if (mask(i, j + 1) == 1 .and. eta(i, j + 1) > eta(i, j)) masktmp(i, j) = 1
            else
               ! drying: wet cell may become dry
               if (eta(i, j) < -depth(i, j)) masktmp(i, j) = 0
            end if

            if (truncate_depth) then
               if (mask(i, j) < 1) then
                  depthx(i, j) = depth(i - 1, j)
                  depthx(i + 1, j) = depth(i + 1, j)
                  depthy(i, j) = depth(i, j - 1)
                  depthy(i, j + 1) = depth(i, j + 1)
               end if
            end if
         end do
      end do

      mask = masktmp
   end subroutine update_mask

   ! ----------------------------------------------------------------
   ! Update MASK9: 3×3 product of MASK neighbours, zeroed where the
   ! local wave is too nonlinear (abs(eta)/depth > swe_eta_dep) to
   ! use the Boussinesq correction.
   !
   ! viscosity_breaking: when .true., MASK9 is forced to 1 everywhere
   !   (Boussinesq dispersion disabled in favour of viscosity breaking).
   ! ----------------------------------------------------------------
   subroutine update_mask9(lp, eta, depth, mask, mask9, &
                           min_depth_frc, swe_eta_dep, viscosity_breaking)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: eta(:, :), depth(:, :)
      integer, intent(in)  :: mask(:, :)
      integer, intent(out) :: mask9(:, :)
      real(SP), intent(in)  :: min_depth_frc, swe_eta_dep
      logical, intent(in)  :: viscosity_breaking

      integer :: i, j

      !$omp parallel do default(shared) schedule(static) private(i)
      do j = lp%jb - 1, lp%je + 1
         do i = lp%ib - 1, lp%ie + 1
            if (viscosity_breaking) then
               mask9(i, j) = 1
            else
               mask9(i, j) = mask(i, j)*mask(i - 1, j)*mask(i + 1, j) &
                             *mask(i + 1, j + 1)*mask(i, j + 1)*mask(i - 1, j + 1) &
                             *mask(i + 1, j - 1)*mask(i, j - 1)*mask(i - 1, j - 1)
               if (abs(eta(i, j))/max(depth(i, j), min_depth_frc) > swe_eta_dep) then
                  mask9(i, j) = 0
               end if
            end if
         end do
      end do
   end subroutine update_mask9

   ! ----------------------------------------------------------------
   ! Real dispersion-gate weight consumed by the kernels in place of
   ! MASK9: swe_w = mask9 * smoothstep taper over eta/depth in
   ! [swe_eta_dep - swe_eta_ramp, swe_eta_dep].  Ramp 0 (or viscosity
   ! breaking) reduces to real(mask9) — bitwise the old behaviour.
   ! Pointwise over the FULL local array: mask9/eta carry valid halos
   ! here (caller runs it after the mask9 ring exchange), so no new
   ! exchange is needed.  Rationale: the hard switch converts last-bit
   ! eta differences into O(1) residual flips at threshold cells — the
   ! blow-up injector of the 2026-07 damping mission.
   ! ----------------------------------------------------------------
   subroutine update_swe_weight(eta, depth, mask9, min_depth_frc, &
                                swe_eta_dep, swe_eta_ramp, &
                                viscosity_breaking, swe_w, &
                                min_depth, swe_wetdry_ramp)
      real(SP), intent(in)  :: eta(:, :), depth(:, :)
      integer, intent(in)  :: mask9(:, :)
      real(SP), intent(in)  :: min_depth_frc, swe_eta_dep, swe_eta_ramp
      logical, intent(in)  :: viscosity_breaking
      real(SP), intent(out) :: swe_w(:, :)
      real(SP), intent(in)  :: min_depth, swe_wetdry_ramp

      real(SP) :: t, s
      integer :: i, j
      logical :: gate_on, wd_on

      ! wet/dry-proximity taper: smoothstep on the water column over
      ! [min_depth, (1+ramp)*min_depth], mode-independent -- kills the
      ! dispersive weight AT the swash-edge flip cells so raw mask flips
      ! stop seeding the dispersion-ringing artifact (viscous path has
      ! no SWE gate to do it for free)
      gate_on = swe_eta_ramp > 0.0_SP .and. .not. viscosity_breaking
      wd_on = swe_wetdry_ramp > 0.0_SP

      if (.not. (gate_on .or. wd_on)) then
         swe_w = real(mask9, SP)
         return
      end if

      !$omp parallel do default(shared) schedule(static) private(i, t, s)
      do j = 1, size(mask9, 2)
         do i = 1, size(mask9, 1)
            swe_w(i, j) = real(mask9(i, j), SP)
            if (gate_on) then
               t = (swe_eta_dep - abs(eta(i, j)) &
                    /max(depth(i, j), min_depth_frc))/swe_eta_ramp
               t = min(1.0_SP, max(0.0_SP, t))
               swe_w(i, j) = swe_w(i, j)*t*t*(3.0_SP - 2.0_SP*t)
            end if
            if (wd_on) then
               s = (eta(i, j) + depth(i, j) - min_depth) &
                   /(swe_wetdry_ramp*min_depth)
               s = min(1.0_SP, max(0.0_SP, s))
               swe_w(i, j) = swe_w(i, j)*s*s*(3.0_SP - 2.0_SP*s)
            end if
         end do
      end do
   end subroutine update_swe_weight

end module model_kernel_masks_mod
