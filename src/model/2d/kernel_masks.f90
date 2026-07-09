! allow(E001)
module model_kernel_masks_mod
   use core_constants_mod, only: SP
   use core_grid_mod, only: type_loop_bounds
   implicit none
   private

   public :: update_mask, update_mask9

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

end module model_kernel_masks_mod
