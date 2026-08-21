! allow(E001)
module model_kernel_visc_mod
   use core_constants_mod, only: SP
   use core_grid_mod, only: type_loop_bounds
   implicit none
   private

   public :: cal_visc_assemble_x, cal_visc_assemble_y

contains

   ! ----------------------------------------------------------------
   ! cal_visc_assemble_x — x-sweep bands for the split implicit
   ! breaker-viscosity solve, theta-weighted:
   !   (I - theta dt d/dx nu d/dx) q_new = (I + (1-theta) dt ...) q
   ! on the cell-centred flux q (hu or hv); theta 1 = backward Euler,
   ! 0.5 = Crank-Nicolson.  Face viscosity is the two-cell average,
   ! matching the explicit BreakSource form.  A dry cell is an identity
   ! row but its faces stay ACTIVE — the explicit stencil diffuses
   ! against the dry cell's hu unmasked (a swash-front momentum sink),
   ! and the identity row makes that a Dirichlet value here, closing
   ! the operator identically.  Only the global wall zeroes a face.
   ! Diagonal normalized to 1 for trid_x.
   ! ----------------------------------------------------------------
   subroutine cal_visc_assemble_x(lp, dt, theta, inv_dx, mask, nu, q, &
                                  edge_w, edge_e, a, c, d)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: dt, theta
      real(SP), intent(in)  :: inv_dx(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(in)  :: nu(:, :), q(:, :)
      logical, intent(in)  :: edge_w, edge_e
      real(SP), intent(out) :: a(:, :), c(:, :), d(:, :)

      real(SP) :: w, e, diag, expl
      integer  :: i, j

      !$omp parallel do default(shared) schedule(static) private(i, w, e, diag, expl)
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            if (mask(i, j) == 0) then
               a(i, j) = 0.0_SP
               c(i, j) = 0.0_SP
               d(i, j) = q(i, j)
               cycle
            end if
            w = 0.5_SP*dt*inv_dx(i, j)*inv_dx(i, j)*(nu(i - 1, j) + nu(i, j))
            e = 0.5_SP*dt*inv_dx(i, j)*inv_dx(i, j)*(nu(i + 1, j) + nu(i, j))
            if (edge_w .and. i == lp%ib) w = 0.0_SP
            if (edge_e .and. i == lp%ie) e = 0.0_SP
            diag = 1.0_SP + theta*(w + e)
            expl = (1.0_SP - theta)*(w*(q(i - 1, j) - q(i, j)) &
                                     + e*(q(i + 1, j) - q(i, j)))
            a(i, j) = -theta*w/diag
            c(i, j) = -theta*e/diag
            d(i, j) = (q(i, j) + expl)/diag
         end do
      end do

   end subroutine cal_visc_assemble_x

   ! ----------------------------------------------------------------
   ! cal_visc_assemble_y — y-sweep bands, same closure rules.  Under
   ! theta < 1 the explicit part reads q(i, j+-1): the caller must
   ! refresh the ghost rows of q first (the x-solve fills interior only).
   ! ----------------------------------------------------------------
   subroutine cal_visc_assemble_y(lp, dt, theta, inv_dy, mask, nu, q, &
                                  edge_s, edge_n, a, c, d)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: dt, theta
      real(SP), intent(in)  :: inv_dy(:, :)
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(in)  :: nu(:, :), q(:, :)
      logical, intent(in)  :: edge_s, edge_n
      real(SP), intent(out) :: a(:, :), c(:, :), d(:, :)

      real(SP) :: s, n, diag, expl
      integer  :: i, j

      !$omp parallel do default(shared) schedule(static) private(i, s, n, diag, expl)
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie
            if (mask(i, j) == 0) then
               a(i, j) = 0.0_SP
               c(i, j) = 0.0_SP
               d(i, j) = q(i, j)
               cycle
            end if
            s = 0.5_SP*dt*inv_dy(i, j)*inv_dy(i, j)*(nu(i, j - 1) + nu(i, j))
            n = 0.5_SP*dt*inv_dy(i, j)*inv_dy(i, j)*(nu(i, j + 1) + nu(i, j))
            if (edge_s .and. j == lp%jb) s = 0.0_SP
            if (edge_n .and. j == lp%je) n = 0.0_SP
            diag = 1.0_SP + theta*(s + n)
            expl = (1.0_SP - theta)*(s*(q(i, j - 1) - q(i, j)) &
                                     + n*(q(i, j + 1) - q(i, j)))
            a(i, j) = -theta*s/diag
            c(i, j) = -theta*n/diag
            d(i, j) = (q(i, j) + expl)/diag
         end do
      end do

   end subroutine cal_visc_assemble_y

end module model_kernel_visc_mod
