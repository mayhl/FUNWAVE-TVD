module model_kernel_derivatives_mod
   use core_constants_mod, only: SP
   use core_grid_mod, only: type_loop_bounds
   implicit none
   private

   public :: deriv_x, deriv_y
   public :: deriv_xx, deriv_yy, deriv_xy
   public :: deriv_x_high, deriv_y_high
   public :: deriv_xx_high, deriv_yy_high, deriv_xy_high

contains

   ! First derivative d/dx — central 2-point stencil.
   pure subroutine deriv_x(lp, inv_dx, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dx(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uout(i, j) = (uin(i + 1, j) - uin(i - 1, j))*0.5_SP*inv_dx(i, j)*mask(i, j)
         end do
      end do
   end subroutine deriv_x

   ! First derivative d/dy — central 2-point stencil.
   pure subroutine deriv_y(lp, inv_dy, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dy(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uout(i, j) = (uin(i, j + 1) - uin(i, j - 1))*0.5_SP*inv_dy(i, j)*mask(i, j)
         end do
      end do
   end subroutine deriv_y

   ! Second derivative d²/dx² — standard 3-point stencil.
   pure subroutine deriv_xx(lp, inv_dx, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dx(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uout(i, j) = (uin(i + 1, j) - 2.0_SP*uin(i, j) + uin(i - 1, j)) &
                         *inv_dx(i, j)*inv_dx(i, j)*mask(i, j)
         end do
      end do
   end subroutine deriv_xx

   ! Second derivative d²/dy² — standard 3-point stencil.
   pure subroutine deriv_yy(lp, inv_dy, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dy(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uout(i, j) = (uin(i, j + 1) - 2.0_SP*uin(i, j) + uin(i, j - 1)) &
                         *inv_dy(i, j)*inv_dy(i, j)*mask(i, j)
         end do
      end do
   end subroutine deriv_yy

   ! Cross derivative d²/dxdy — central 4-point stencil.
   pure subroutine deriv_xy(lp, inv_dx, inv_dy, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dx(:, :), inv_dy(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer :: i, j
      real(SP) :: t1, t2
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            t1 = (uin(i + 1, j + 1) - uin(i + 1, j - 1))*0.5_SP*inv_dy(i, j)
            t2 = (uin(i - 1, j + 1) - uin(i - 1, j - 1))*0.5_SP*inv_dy(i, j)
            uout(i, j) = (t1 - t2)*0.5_SP*inv_dx(i, j)*mask(i, j)
         end do
      end do
   end subroutine deriv_xy

   ! First derivative d/dx — higher-order 4-point stencil.
   pure subroutine deriv_x_high(lp, inv_dx, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dx(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uout(i, j) = (uin(i + 2, j) + 2.0_SP*uin(i + 1, j) - uin(i - 2, j) - 2.0_SP*uin(i - 1, j)) &
                         *inv_dx(i, j)/8.0_SP*mask(i, j)
         end do
      end do
   end subroutine deriv_x_high

   ! First derivative d/dy — higher-order 4-point stencil.
   pure subroutine deriv_y_high(lp, inv_dy, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dy(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uout(i, j) = (uin(i, j + 2) + 2.0_SP*uin(i, j + 1) - uin(i, j - 2) - 2.0_SP*uin(i, j - 1)) &
                         *inv_dy(i, j)/8.0_SP*mask(i, j)
         end do
      end do
   end subroutine deriv_y_high

   ! Second derivative d²/dx² — higher-order 5-point stencil.
   pure subroutine deriv_xx_high(lp, inv_dx, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dx(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uout(i, j) = mask(i, j)*inv_dx(i, j)*inv_dx(i, j)/12.0_SP &
                         *(-uin(i - 2, j) + 16.0_SP*uin(i - 1, j) - 30.0_SP*uin(i, j) &
                           + 16.0_SP*uin(i + 1, j) - uin(i + 2, j))
         end do
      end do
   end subroutine deriv_xx_high

   ! Second derivative d²/dy² — higher-order 5-point stencil.
   pure subroutine deriv_yy_high(lp, inv_dy, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dy(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            uout(i, j) = mask(i, j)*inv_dy(i, j)*inv_dy(i, j)/12.0_SP &
                         *(-uin(i, j - 2) + 16.0_SP*uin(i, j - 1) - 30.0_SP*uin(i, j) &
                           + 16.0_SP*uin(i, j + 1) - uin(i, j + 2))
         end do
      end do
   end subroutine deriv_yy_high

   ! Cross derivative d²/dxdy — higher-order 4×4-point stencil.
   ! Uses locally constant dx/dy for symmetry on non-uniform grids.
   pure subroutine deriv_xy_high(lp, inv_dx, inv_dy, mask, uin, uout)
      type(type_loop_bounds), intent(in)  :: lp
      real(SP), intent(in)  :: inv_dx(:, :), inv_dy(:, :), uin(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(out) :: uout(:, :)
      integer  :: i, j
      real(SP) :: t1, t2, t3, t4
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            t1 = inv_dy(i, j)/12.0_SP*(uin(i - 2, j - 2) - 8.0_SP*uin(i - 2, j - 1) &
                                       + 8.0_SP*uin(i - 2, j + 1) - uin(i - 2, j + 2))
            t2 = inv_dy(i, j)/12.0_SP*(uin(i - 1, j - 2) - 8.0_SP*uin(i - 1, j - 1) &
                                       + 8.0_SP*uin(i - 1, j + 1) - uin(i - 1, j + 2))
            t3 = inv_dy(i, j)/12.0_SP*(uin(i + 1, j - 2) - 8.0_SP*uin(i + 1, j - 1) &
                                       + 8.0_SP*uin(i + 1, j + 1) - uin(i + 1, j + 2))
            t4 = inv_dy(i, j)/12.0_SP*(uin(i + 2, j - 2) - 8.0_SP*uin(i + 2, j - 1) &
                                       + 8.0_SP*uin(i + 2, j + 1) - uin(i + 2, j + 2))
            uout(i, j) = mask(i, j)*inv_dx(i, j)/12.0_SP &
                         *(t1 - 8.0_SP*t2 + 8.0_SP*t3 - t4)
         end do
      end do
   end subroutine deriv_xy_high

end module model_kernel_derivatives_mod
