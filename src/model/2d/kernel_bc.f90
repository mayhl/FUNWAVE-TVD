! allow(E001)
module model_kernel_bc_mod
   use core_constants_mod, only: SP
   use core_grid_mod, only: type_loop_bounds
   implicit none
   private

   public :: fill_ghost_wall
   public :: SIGN_MIRROR, SIGN_ANTI

   real(SP), parameter :: SIGN_MIRROR = 1.0_SP   ! symmetric (scalar) reflection
   real(SP), parameter :: SIGN_ANTI = -1.0_SP  ! antisymmetric (normal velocity) reflection

contains

   ! ----------------------------------------------------------------
   ! Wall (mirror) ghost fill at physical domain boundaries.
   ! Ghost cell k (of $n_g$) reflects the interior across the wall face,
   ! e.g. on the west side
   !   $$ f_{k,j} = \sigma\, f_{2 n_g + 1 - k,\, j}, \qquad k = 1..n_g $$
   ! with $\sigma = +1$ (mirror) enforcing $\partial_n f = 0$ and
   ! $\sigma = -1$ (antisymmetric) enforcing $f = 0$ at the wall face.
   ! Port of legacy PHI_COLL ghost logic (old/bc.F, Cartesian branch):
   !   VTYPE=1 (eta, scalars): sign_x = SIGN_MIRROR, sign_y = SIGN_MIRROR
   !   VTYPE=2 (u, hu)       : sign_x = SIGN_ANTI,   sign_y = SIGN_MIRROR
   !   VTYPE=3 (v, hv)       : sign_x = SIGN_MIRROR, sign_y = SIGN_ANTI
   !
   ! Fill order matters and is preserved from legacy: x-faces fill
   ! interior rows (jb..je) only; y-faces then sweep the full i range
   ! (1..mloc) so corner ghosts inherit the already-mirrored x-ghost
   ! columns.
   !
   ! fill_* flags: pass grid%is_*_boundary. Under a periodic cart
   ! topology no rank reports a boundary on the wrapped axis, so the
   ! flag is .false. and halo_exchange has already filled the ghosts.
   ! Wavemaker-exempt faces (legacy ABS / LEFT_BC_IRR west) also pass
   ! .false. — the wavemaker BC owns those ghosts.
   !
   ! MPI halo exchange is NOT performed here; the caller exchanges
   ! first, then fills physical boundaries.
   ! ----------------------------------------------------------------
   pure subroutine fill_ghost_wall(lp, fill_west, fill_east, fill_south, fill_north, &
                                   sign_x, sign_y, f)
      type(type_loop_bounds), intent(in)    :: lp
      logical, intent(in)    :: fill_west, fill_east, fill_south, fill_north
      real(SP), intent(in)    :: sign_x, sign_y
      real(SP), intent(inout) :: f(:, :)

      integer :: i, j, k, ng

      ng = lp%ib - 1

      if (fill_west) then
         do j = lp%jb, lp%je
            do k = 1, ng
               f(k, j) = sign_x*f(2*ng + 1 - k, j)
            end do
         end do
      end if

      if (fill_east) then
         do j = lp%jb, lp%je
            do k = 1, ng
               f(lp%ie + k, j) = sign_x*f(lp%ie - k + 1, j)
            end do
         end do
      end if

      if (fill_south) then
         do k = 1, ng
            do i = 1, lp%mloc
               f(i, k) = sign_y*f(i, 2*ng + 1 - k)
            end do
         end do
      end if

      if (fill_north) then
         do k = 1, ng
            do i = 1, lp%mloc
               f(i, lp%je + k) = sign_y*f(i, lp%je - k + 1)
            end do
         end do
      end if

   end subroutine fill_ghost_wall

end module model_kernel_bc_mod
