! allow(E001)
module model_kernel_fluxes_mod
   use core_constants_mod, only: SP, N_GHOST, GRAV
   use core_grid_mod, only: type_loop_bounds
   implicit none
   private

   real(SP), parameter :: SMALL = 1.0e-6_SP

   ! j-strip height for the blocked fourth-order construction pass:
   ! sized so a strip of the ~20 construct/assemble arrays stays
   ! L2-resident on production tile widths (perf audit, cache rung)
   integer, parameter :: J_BLOCK = 32

   ! Interface arrays for one time-step; caller allocates once and reuses.
   type, public :: type_flux_workspace
      integer :: m = 0, n = 0
      ! x-interface: (m+1) x n
      real(SP), allocatable :: uxl(:, :), uxr(:, :)
      real(SP), allocatable :: vxl(:, :), vxr(:, :)
      real(SP), allocatable :: huxl(:, :), huxr(:, :)
      real(SP), allocatable :: hvxl(:, :), hvxr(:, :)
      real(SP), allocatable :: etarxl(:, :), etarxr(:, :)
      real(SP), allocatable :: hxl(:, :), hxr(:, :)
      real(SP), allocatable :: u4xl(:, :), u4xr(:, :)
      real(SP), allocatable :: v4xl(:, :), v4xr(:, :)
      real(SP), allocatable :: pl(:, :), pr(:, :)
      real(SP), allocatable :: fxl(:, :), fxr(:, :)
      real(SP), allocatable :: gxl(:, :), gxr(:, :)
      real(SP), allocatable :: sxl(:, :), sxr(:, :)
      ! y-interface: m x (n+1)
      real(SP), allocatable :: uyl(:, :), uyr(:, :)
      real(SP), allocatable :: vyl(:, :), vyr(:, :)
      real(SP), allocatable :: huyl(:, :), huyr(:, :)
      real(SP), allocatable :: hvyl(:, :), hvyr(:, :)
      real(SP), allocatable :: etaryl(:, :), etaryr(:, :)
      real(SP), allocatable :: hyl(:, :), hyr(:, :)
      real(SP), allocatable :: u4yl(:, :), u4yr(:, :)
      real(SP), allocatable :: v4yl(:, :), v4yr(:, :)
      real(SP), allocatable :: ql(:, :), qr(:, :)
      real(SP), allocatable :: fyl(:, :), fyr(:, :)
      real(SP), allocatable :: gyl(:, :), gyr(:, :)
      real(SP), allocatable :: syl(:, :), syr(:, :)
      ! output fluxes: (m+1)xn and mx(n+1)
      real(SP), allocatable :: p(:, :), fx(:, :), gx(:, :)
      real(SP), allocatable :: q(:, :), fy(:, :), gy(:, :)
      ! slope scratch for construction: m x n
      real(SP), allocatable :: sl(:, :)
   contains
      procedure :: alloc => fws_alloc
      procedure :: free => fws_free
   end type type_flux_workspace

   public :: delx_fun, dely_fun
   public :: construct_x, construct_y
   public :: construct_ho_x, construct_ho_y
   public :: construct_ho_x_minmod, construct_ho_y_minmod
   public :: construct_ho_x_mlp, construct_ho_y_mlp
   public :: weno_construct_x, weno_construct_y
   public :: wave_speed, hll3
   public :: flux_at_interface, flux_at_interface_hll
   public :: construction, fluxes_ho_blocked
   public :: construction_ho_minmod, construction_ho_mlp
   public :: construction_weno
   public :: fluxes, flux_wall_bc, flux_dry_bc

contains

   ! ----------------------------------------------------------------
   ! Workspace alloc / free
   ! ----------------------------------------------------------------
   subroutine fws_alloc(ws, m, n)
      class(type_flux_workspace), intent(inout) :: ws
      integer, intent(in) :: m, n
      integer :: m1, n1
      ws%m = m; ws%n = n
      m1 = m + 1; n1 = n + 1
      allocate (ws%uxl(m1, n), ws%uxr(m1, n))
      allocate (ws%vxl(m1, n), ws%vxr(m1, n))
      allocate (ws%huxl(m1, n), ws%huxr(m1, n))
      allocate (ws%hvxl(m1, n), ws%hvxr(m1, n))
      allocate (ws%etarxl(m1, n), ws%etarxr(m1, n))
      allocate (ws%hxl(m1, n), ws%hxr(m1, n))
      allocate (ws%u4xl(m1, n), ws%u4xr(m1, n))
      allocate (ws%v4xl(m1, n), ws%v4xr(m1, n))
      allocate (ws%pl(m1, n), ws%pr(m1, n))
      allocate (ws%fxl(m1, n), ws%fxr(m1, n))
      allocate (ws%gxl(m1, n), ws%gxr(m1, n))
      allocate (ws%sxl(m1, n), ws%sxr(m1, n))
      allocate (ws%uyl(m, n1), ws%uyr(m, n1))
      allocate (ws%vyl(m, n1), ws%vyr(m, n1))
      allocate (ws%huyl(m, n1), ws%huyr(m, n1))
      allocate (ws%hvyl(m, n1), ws%hvyr(m, n1))
      allocate (ws%etaryl(m, n1), ws%etaryr(m, n1))
      allocate (ws%hyl(m, n1), ws%hyr(m, n1))
      allocate (ws%u4yl(m, n1), ws%u4yr(m, n1))
      allocate (ws%v4yl(m, n1), ws%v4yr(m, n1))
      allocate (ws%ql(m, n1), ws%qr(m, n1))
      allocate (ws%fyl(m, n1), ws%fyr(m, n1))
      allocate (ws%gyl(m, n1), ws%gyr(m, n1))
      allocate (ws%syl(m, n1), ws%syr(m, n1))
      allocate (ws%p(m1, n), ws%fx(m1, n), ws%gx(m1, n))
      allocate (ws%q(m, n1), ws%fy(m, n1), ws%gy(m, n1))
      allocate (ws%sl(m, n))
   end subroutine fws_alloc

   subroutine fws_free(ws)
      class(type_flux_workspace), intent(inout) :: ws
      if (.not. allocated(ws%p)) return
      deallocate (ws%uxl, ws%uxr, ws%vxl, ws%vxr)
      deallocate (ws%huxl, ws%huxr, ws%hvxl, ws%hvxr)
      deallocate (ws%etarxl, ws%etarxr, ws%hxl, ws%hxr)
      deallocate (ws%u4xl, ws%u4xr, ws%v4xl, ws%v4xr)
      deallocate (ws%pl, ws%pr, ws%fxl, ws%fxr)
      deallocate (ws%gxl, ws%gxr, ws%sxl, ws%sxr)
      deallocate (ws%uyl, ws%uyr, ws%vyl, ws%vyr)
      deallocate (ws%huyl, ws%huyr, ws%hvyl, ws%hvyr)
      deallocate (ws%etaryl, ws%etaryr, ws%hyl, ws%hyr)
      deallocate (ws%u4yl, ws%u4yr, ws%v4yl, ws%v4yr)
      deallocate (ws%ql, ws%qr, ws%fyl, ws%fyr)
      deallocate (ws%gyl, ws%gyr, ws%syl, ws%syr)
      deallocate (ws%p, ws%fx, ws%gx)
      deallocate (ws%q, ws%fy, ws%gy)
      deallocate (ws%sl)
      ws%m = 0; ws%n = 0
   end subroutine fws_free

   ! ----------------------------------------------------------------
   ! Van Leer limited slope in x.  Takes inv_dx (no division by grid
   ! spacing); the limiter's own ratio division is data-dependent and
   ! intrinsic to the scheme.  Deliberately full-array (size-based, not
   ! lp): interface arrays are staggered and ghost slopes feed the
   ! boundary reconstruction, matching legacy DelxFun.
   ! ----------------------------------------------------------------
   pure subroutine delx_fun(inv_dx, din, dout)
      real(SP), intent(in)  :: inv_dx(:, :), din(:, :)
      real(SP), intent(out) :: dout(:, :)
      integer  :: i, j, m, n
      real(SP) :: tmp1, tmp2
      m = size(din, 1); n = size(din, 2)
      do j = 1, n
         do i = 2, m - 1
            tmp1 = (din(i + 1, j) - din(i, j))*inv_dx(i, j)
            tmp2 = (din(i, j) - din(i - 1, j))*inv_dx(i - 1, j)
            if (abs(tmp1) + abs(tmp2) < SMALL) then
               dout(i, j) = 0.0_SP
            else
               dout(i, j) = (tmp1*abs(tmp2) + abs(tmp1)*tmp2)/(abs(tmp1) + abs(tmp2))
            end if
         end do
         dout(1, j) = (din(2, j) - din(1, j))*inv_dx(1, j)
         dout(m, j) = (din(m, j) - din(m - 1, j))*inv_dx(m, j)
      end do
   end subroutine delx_fun

   ! ----------------------------------------------------------------
   ! Van Leer limited slope in y (takes inv_dy; see delx_fun notes).
   ! ----------------------------------------------------------------
   pure subroutine dely_fun(inv_dy, din, dout)
      real(SP), intent(in)  :: inv_dy(:, :), din(:, :)
      real(SP), intent(out) :: dout(:, :)
      integer  :: i, j, m, n
      real(SP) :: tmp1, tmp2
      m = size(din, 1); n = size(din, 2)
      ! j-outer keeps the inner loop stride-1 (perf audit item 2)
      do j = 2, n - 1
         do i = 1, m
            tmp1 = (din(i, j + 1) - din(i, j))*inv_dy(i, j)
            tmp2 = (din(i, j) - din(i, j - 1))*inv_dy(i, j - 1)
            if (abs(tmp1) + abs(tmp2) < SMALL) then
               dout(i, j) = 0.0_SP
            else
               dout(i, j) = (tmp1*abs(tmp2) + abs(tmp1)*tmp2)/(abs(tmp1) + abs(tmp2))
            end if
         end do
      end do
      do i = 1, m
         dout(i, 1) = (din(i, 2) - din(i, 1))*inv_dy(i, 1)
         dout(i, n) = (din(i, n) - din(i, n - 1))*inv_dy(i, n)
      end do
   end subroutine dely_fun

   ! ----------------------------------------------------------------
   ! First-order van Leer reconstruction in x (Zhou et al. 2001).
   ! kappa removed (Choi 2016: 2nd/3rd order identical).
   ! ----------------------------------------------------------------
   pure subroutine construct_x(dx, vin, din, outl, outr)
      real(SP), intent(in)  :: dx(:, :), vin(:, :), din(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      integer :: i, j, m, n
      m = size(vin, 1); n = size(vin, 2)
      do j = 1, n
         do i = 2, m
            outl(i, j) = vin(i - 1, j) + 0.5_SP*dx(i - 1, j)*din(i - 1, j)
            outr(i, j) = vin(i, j) - 0.5_SP*dx(i, j)*din(i, j)
         end do
         outl(m + 1, j) = vin(m, j) + 0.5_SP*dx(m, j)*din(m, j)
         outr(1, j) = vin(1, j) - 0.5_SP*dx(1, j)*din(1, j)
         outl(1, j) = outr(1, j)
         outr(m + 1, j) = outl(m + 1, j)
      end do
   end subroutine construct_x

   ! ----------------------------------------------------------------
   ! First-order van Leer reconstruction in y.
   ! ----------------------------------------------------------------
   pure subroutine construct_y(dy, vin, din, outl, outr)
      real(SP), intent(in)  :: dy(:, :), vin(:, :), din(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      integer :: i, j, m, n
      m = size(vin, 1); n = size(vin, 2)
      ! j-outer keeps the inner loop stride-1 (perf audit item 2)
      do j = 2, n
         do i = 1, m
            outl(i, j) = vin(i, j - 1) + 0.5_SP*dy(i, j - 1)*din(i, j - 1)
            outr(i, j) = vin(i, j) - 0.5_SP*dy(i, j)*din(i, j)
         end do
      end do
      do i = 1, m
         outl(i, n + 1) = vin(i, n) + 0.5_SP*dy(i, n)*din(i, n)
         outr(i, 1) = vin(i, 1) - 0.5_SP*dy(i, 1)*din(i, 1)
         outl(i, 1) = outr(i, 1)
         outr(i, n + 1) = outl(i, n + 1)
      end do
   end subroutine construct_y

   ! ----------------------------------------------------------------
   ! 4th-order MUSCL-TVD in x: van Leer (3rd) + minmod (4th) limiter.
   ! Erduran et al. (2005), default 'FOU' path.
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_x(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(in)  :: vin(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      real(SP) :: din(lp%mloc, lp%nloc)
      real(SP) :: txp1, txp2, txp3, dvp1, dvp2, dvp3
      real(SP) :: van1, van2, rat, tmp1, tmp2
      integer  :: i, j
      ! no din zero-fill: every read row below is written first
      do j = lp%jb, lp%je
         do i = lp%ib - 1, lp%ie + 2
            txp1 = vin(i - 1, j) - vin(i - 2, j)
            txp2 = vin(i, j) - vin(i - 1, j)
            txp3 = vin(i + 1, j) - vin(i, j)
            dvp1 = minmod3(txp1, 2.0_SP*txp2, 2.0_SP*txp3)
            dvp2 = minmod3(txp2, 2.0_SP*txp3, 2.0_SP*txp1)
            dvp3 = minmod3(txp3, 2.0_SP*txp1, 2.0_SP*txp2)
            if (mask(i - 2, j) == 0 .or. mask(i + 1, j) == 0) then
               txp2 = vin(i, j) - vin(i - 1, j)
               txp1 = txp2; txp3 = txp2
               dvp1 = minmod3(txp1, 2.0_SP*txp2, 2.0_SP*txp3)
               dvp2 = minmod3(txp2, 2.0_SP*txp3, 2.0_SP*txp1)
               dvp3 = minmod3(txp3, 2.0_SP*txp1, 2.0_SP*txp2)
            end if
            if (mask(i - 1, j) == 0 .or. mask(i, j) == 0) then
               dvp1 = 0.0_SP; dvp2 = 0.0_SP; dvp3 = 0.0_SP
            end if
            din(i, j) = txp2 - (1.0_SP/6.0_SP)*(dvp3 - 2.0_SP*dvp2 + dvp1)
         end do
         do i = lp%ib, lp%ie + 1
            tmp1 = din(i - 1, j); tmp2 = din(i, j)
            if (abs(tmp1) <= SMALL) tmp1 = SMALL*sign(1.0_SP, tmp1)
            if (abs(tmp2) <= SMALL) tmp2 = SMALL*sign(1.0_SP, tmp2)
            rat = tmp2/tmp1
            van1 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van1 = (rat + abs(rat))/(1.0_SP + rat)
            rat = tmp1/tmp2
            van2 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van2 = (rat + abs(rat))/(1.0_SP + rat)
            outl(i, j) = vin(i - 1, j) + (1.0_SP/6.0_SP)*(van1*tmp1 + 2.0_SP*van2*tmp2)
            tmp1 = din(i, j); tmp2 = din(i + 1, j)
            if (abs(tmp1) <= SMALL) tmp1 = SMALL*sign(1.0_SP, tmp1)
            if (abs(tmp2) <= SMALL) tmp2 = SMALL*sign(1.0_SP, tmp2)
            rat = tmp2/tmp1
            van1 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van1 = (rat + abs(rat))/(1.0_SP + rat)
            rat = tmp1/tmp2
            van2 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van2 = (rat + abs(rat))/(1.0_SP + rat)
            outr(i, j) = vin(i, j) - (1.0_SP/6.0_SP)*(2.0_SP*van1*tmp1 + van2*tmp2)
         end do
      end do
   end subroutine construct_ho_x

   ! ----------------------------------------------------------------
   ! 4th-order MUSCL-TVD in y: van Leer (3rd) + minmod (4th).
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_y(lp, mask, vin, outl, outr, js, je)
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(in)  :: vin(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      integer, intent(in)  :: js, je   ! face rows to write (full sweep = jb..je+1)
      real(SP) :: din(lp%mloc, lp%nloc)
      real(SP) :: typ1, typ2, typ3, dvp1, dvp2, dvp3
      real(SP) :: van1, van2, rat, tmp1, tmp2
      integer  :: i, j
      ! no din zero-fill: every read row below is written first
      ! two j-outer nests like the minmod sibling — the fused per-i
      ! form walked both inner loops at stride mloc (perf audit item 2)
      do j = js - 1, je + 1
         do i = lp%ib, lp%ie
            typ1 = vin(i, j - 1) - vin(i, j - 2)
            typ2 = vin(i, j) - vin(i, j - 1)
            typ3 = vin(i, j + 1) - vin(i, j)
            dvp1 = minmod3(typ1, 2.0_SP*typ2, 2.0_SP*typ3)
            dvp2 = minmod3(typ2, 2.0_SP*typ3, 2.0_SP*typ1)
            dvp3 = minmod3(typ3, 2.0_SP*typ1, 2.0_SP*typ2)
            if (mask(i, j - 2) == 0 .or. mask(i, j + 1) == 0) then
               typ2 = vin(i, j) - vin(i, j - 1)
               typ1 = typ2; typ3 = typ2
               dvp1 = minmod3(typ1, 2.0_SP*typ2, 2.0_SP*typ3)
               dvp2 = minmod3(typ2, 2.0_SP*typ3, 2.0_SP*typ1)
               dvp3 = minmod3(typ3, 2.0_SP*typ1, 2.0_SP*typ2)
            end if
            if (mask(i, j - 1) == 0 .or. mask(i, j) == 0) then
               dvp1 = 0.0_SP; dvp2 = 0.0_SP; dvp3 = 0.0_SP
            end if
            din(i, j) = typ2 - (1.0_SP/6.0_SP)*(dvp3 - 2.0_SP*dvp2 + dvp1)
         end do
      end do
      do j = js, je
         do i = lp%ib, lp%ie
            tmp1 = din(i, j - 1); tmp2 = din(i, j)
            if (abs(tmp1) <= SMALL) tmp1 = SMALL*sign(1.0_SP, tmp1)
            if (abs(tmp2) <= SMALL) tmp2 = SMALL*sign(1.0_SP, tmp2)
            rat = tmp2/tmp1
            van1 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van1 = (rat + abs(rat))/(1.0_SP + rat)
            rat = tmp1/tmp2
            van2 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van2 = (rat + abs(rat))/(1.0_SP + rat)
            outl(i, j) = vin(i, j - 1) + (1.0_SP/6.0_SP)*(van1*tmp1 + 2.0_SP*van2*tmp2)
            tmp1 = din(i, j); tmp2 = din(i, j + 1)
            if (abs(tmp1) <= SMALL) tmp1 = SMALL*sign(1.0_SP, tmp1)
            if (abs(tmp2) <= SMALL) tmp2 = SMALL*sign(1.0_SP, tmp2)
            rat = tmp2/tmp1
            van1 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van1 = (rat + abs(rat))/(1.0_SP + rat)
            rat = tmp1/tmp2
            van2 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van2 = (rat + abs(rat))/(1.0_SP + rat)
            outr(i, j) = vin(i, j) - (1.0_SP/6.0_SP)*(2.0_SP*van1*tmp1 + van2*tmp2)
         end do
      end do
   end subroutine construct_ho_y

   ! ----------------------------------------------------------------
   ! 4th-order MUSCL-TVD in x: minmod-only variant ('FMI').
   ! DX argument removed (unused in original, Choi 2016).
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_x_minmod(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(in)  :: vin(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      real(SP) :: din(lp%mloc, lp%nloc)
      real(SP) :: txp1, txp2, txp3, txp4, dvp1, dvp2, dvp3
      integer  :: i, j
      ! no din zero-fill: every read row below is written first
      do j = lp%jb, lp%je
         do i = lp%ib - 1, lp%ie + 2
            txp1 = vin(i - 1, j) - vin(i - 2, j)
            txp2 = vin(i, j) - vin(i - 1, j)
            txp3 = vin(i + 1, j) - vin(i, j)
            dvp1 = minmod3(txp1, 2.0_SP*txp2, 2.0_SP*txp3)
            dvp2 = minmod3(txp2, 2.0_SP*txp3, 2.0_SP*txp1)
            dvp3 = minmod3(txp3, 2.0_SP*txp1, 2.0_SP*txp2)
            if (mask(i - 2, j) == 0 .or. mask(i + 1, j) == 0) then
               txp2 = vin(i, j) - vin(i - 1, j)
               txp1 = txp2; txp3 = txp2
               dvp1 = minmod3(txp1, 2.0_SP*txp2, 2.0_SP*txp3)
               dvp2 = minmod3(txp2, 2.0_SP*txp3, 2.0_SP*txp1)
               dvp3 = minmod3(txp3, 2.0_SP*txp1, 2.0_SP*txp2)
            end if
            if (mask(i - 1, j) == 0 .or. mask(i, j) == 0) then
               dvp1 = 0.0_SP; dvp2 = 0.0_SP; dvp3 = 0.0_SP
            end if
            din(i, j) = txp2 - (1.0_SP/6.0_SP)*(dvp3 - 2.0_SP*dvp2 + dvp1)
         end do
         do i = lp%ib, lp%ie + 1
            if (din(i - 1, j) >= 0.0_SP) then
               txp1 = max(0.0_SP, min(din(i - 1, j), 4.0_SP*din(i, j)))
            else
               txp1 = min(0.0_SP, max(din(i - 1, j), 4.0_SP*din(i, j)))
            end if
            if (din(i, j) >= 0.0_SP) then
               txp2 = max(0.0_SP, min(din(i, j), 4.0_SP*din(i - 1, j)))
            else
               txp2 = min(0.0_SP, max(din(i, j), 4.0_SP*din(i - 1, j)))
            end if
            if (din(i, j) >= 0.0_SP) then
               txp4 = max(0.0_SP, min(din(i, j), 4.0_SP*din(i + 1, j)))
            else
               txp4 = min(0.0_SP, max(din(i, j), 4.0_SP*din(i + 1, j)))
            end if
            if (din(i + 1, j) >= 0.0_SP) then
               txp3 = max(0.0_SP, min(din(i + 1, j), 4.0_SP*din(i, j)))
            else
               txp3 = min(0.0_SP, max(din(i + 1, j), 4.0_SP*din(i, j)))
            end if
            outl(i, j) = vin(i - 1, j) + (1.0_SP/6.0_SP)*(txp1 + 2.0_SP*txp2)
            outr(i, j) = vin(i, j) - (1.0_SP/6.0_SP)*(txp3 + 2.0_SP*txp4)
         end do
      end do
   end subroutine construct_ho_x_minmod

   ! ----------------------------------------------------------------
   ! 4th-order MUSCL-TVD in y: minmod-only.
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_y_minmod(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(in)  :: vin(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      real(SP) :: din(lp%mloc, lp%nloc)
      real(SP) :: typ1, typ2, typ3, typ4, dvp1, dvp2, dvp3
      integer  :: i, j
      ! no din zero-fill: every read row below is written first
      do j = lp%jb - 1, lp%je + 2
         do i = lp%ib, lp%ie
            typ1 = vin(i, j - 1) - vin(i, j - 2)
            typ2 = vin(i, j) - vin(i, j - 1)
            typ3 = vin(i, j + 1) - vin(i, j)
            dvp1 = minmod3(typ1, 2.0_SP*typ2, 2.0_SP*typ3)
            dvp2 = minmod3(typ2, 2.0_SP*typ3, 2.0_SP*typ1)
            dvp3 = minmod3(typ3, 2.0_SP*typ1, 2.0_SP*typ2)
            if (mask(i, j - 2) == 0 .or. mask(i, j + 1) == 0) then
               typ2 = vin(i, j) - vin(i, j - 1)
               typ1 = typ2; typ3 = typ2
               dvp1 = minmod3(typ1, 2.0_SP*typ2, 2.0_SP*typ3)
               dvp2 = minmod3(typ2, 2.0_SP*typ3, 2.0_SP*typ1)
               dvp3 = minmod3(typ3, 2.0_SP*typ1, 2.0_SP*typ2)
            end if
            if (mask(i, j - 1) == 0 .or. mask(i, j) == 0) then
               dvp1 = 0.0_SP; dvp2 = 0.0_SP; dvp3 = 0.0_SP
            end if
            din(i, j) = typ2 - (1.0_SP/6.0_SP)*(dvp3 - 2.0_SP*dvp2 + dvp1)
         end do
      end do
      do j = lp%jb, lp%je + 1
         do i = lp%ib, lp%ie
            if (din(i, j - 1) >= 0.0_SP) then
               typ1 = max(0.0_SP, min(din(i, j - 1), 4.0_SP*din(i, j)))
            else
               typ1 = min(0.0_SP, max(din(i, j - 1), 4.0_SP*din(i, j)))
            end if
            if (din(i, j) >= 0.0_SP) then
               typ2 = max(0.0_SP, min(din(i, j), 4.0_SP*din(i, j - 1)))
            else
               typ2 = min(0.0_SP, max(din(i, j), 4.0_SP*din(i, j - 1)))
            end if
            if (din(i, j) >= 0.0_SP) then
               typ4 = max(0.0_SP, min(din(i, j), 4.0_SP*din(i, j + 1)))
            else
               typ4 = min(0.0_SP, max(din(i, j), 4.0_SP*din(i, j + 1)))
            end if
            if (din(i, j + 1) >= 0.0_SP) then
               typ3 = max(0.0_SP, min(din(i, j + 1), 4.0_SP*din(i, j)))
            else
               typ3 = min(0.0_SP, max(din(i, j + 1), 4.0_SP*din(i, j)))
            end if
            outl(i, j) = vin(i, j - 1) + (1.0_SP/6.0_SP)*(typ1 + 2.0_SP*typ2)
            outr(i, j) = vin(i, j) - (1.0_SP/6.0_SP)*(typ3 + 2.0_SP*typ4)
         end do
      end do
   end subroutine construct_ho_y_minmod

   ! ----------------------------------------------------------------
   ! MLP reconstruction in x.
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_x_mlp(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(in)  :: vin(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      real(SP), parameter   :: SV = 1.0e-10_SP
      real(SP) :: txp1, txp2, txp3
      real(SP) :: gaml, gamr, betal, betar
      real(SP) :: delsol, tanth1, tanth2, gamrth2, gamlth1
      real(SP) :: alphin, alph, slope
      integer  :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib - 1, lp%ie + 2
            txp1 = vin(i - 1, j) - vin(i - 2, j)
            txp2 = vin(i, j) - vin(i - 1, j)
            txp3 = vin(i + 1, j) - vin(i, j)
            gaml = txp2/txp1; if (abs(txp1) < SV) gaml = 0.0_SP
            betal = (1.0_SP + 2.0_SP*gaml)/3.0_SP
            gamr = txp2/txp3; if (abs(txp3) < SV) gamr = 0.0_SP
            betar = (1.0_SP + 2.0_SP*gamr)/3.0_SP
            delsol = vin(i, j) - vin(i - 2, j)
            tanth1 = abs((vin(i - 1, j + 1) - vin(i - 1, j - 1))/delsol)
            if (abs(delsol) < SV) tanth1 = 0.0_SP
            delsol = vin(i + 1, j) - vin(i - 1, j)
            tanth2 = abs((vin(i, j + 1) - vin(i, j - 1))/delsol)
            if (abs(delsol) < SV) tanth2 = 0.0_SP
            gamrth2 = tanth2/gamr; if (abs(gamr) < SV) gamrth2 = 0.0_SP
            alphin = 2.0_SP*max(1.0_SP, gaml)*(1.0_SP + max(0.0_SP, gamrth2))/(1.0_SP + tanth1)
            alph = max(1.0_SP, min(2.0_SP, alphin))
            slope = max(0.0_SP, min(alph*gaml, min(alph, betal)))
            outl(i, j) = vin(i - 1, j) + 0.5_SP*slope*txp1
            gamlth1 = tanth1/gaml; if (abs(gaml) < SV) gamlth1 = 0.0_SP
            alphin = 2.0_SP*max(1.0_SP, gamr)*(1.0_SP + max(0.0_SP, gamlth1))/(1.0_SP + tanth2)
            alph = max(1.0_SP, min(2.0_SP, alphin))
            slope = max(0.0_SP, min(alph*gamr, min(alph, betar)))
            outr(i, j) = vin(i, j) - 0.5_SP*slope*txp3
         end do
      end do
   end subroutine construct_ho_x_mlp

   ! ----------------------------------------------------------------
   ! MLP reconstruction in y.
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_y_mlp(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in)  :: mask(:, :)
      real(SP), intent(in)  :: vin(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      real(SP), parameter   :: SV = 1.0e-10_SP
      real(SP) :: typ1, typ2, typ3
      real(SP) :: gaml, gamr, betal, betar
      real(SP) :: delsol, tanth1, tanth2, gamrth2, gamlth1
      real(SP) :: alphin, alph, slope
      integer  :: i, j
      ! j-outer keeps the inner loop stride-1 (perf audit item 2)
      do j = lp%jb - 1, lp%je + 2
         do i = lp%ib, lp%ie
            typ1 = vin(i, j - 1) - vin(i, j - 2)
            typ2 = vin(i, j) - vin(i, j - 1)
            typ3 = vin(i, j + 1) - vin(i, j)
            gaml = typ2/typ1; if (abs(typ1) < SV) gaml = 0.0_SP
            betal = (1.0_SP + 2.0_SP*gaml)/3.0_SP
            gamr = typ2/typ3; if (abs(typ3) < SV) gamr = 0.0_SP
            betar = (1.0_SP + 2.0_SP*gamr)/3.0_SP
            delsol = vin(i, j) - vin(i, j - 2)
            tanth1 = abs((vin(i + 1, j - 1) - vin(i - 1, j - 1))/delsol)
            if (abs(delsol) < SV) tanth1 = 0.0_SP
            delsol = vin(i, j + 1) - vin(i, j - 1)
            tanth2 = abs((vin(i + 1, j) - vin(i - 1, j))/delsol)
            if (abs(delsol) < SV) tanth2 = 0.0_SP
            gamrth2 = tanth2/gamr; if (abs(gamr) < SV) gamrth2 = 0.0_SP
            alphin = 2.0_SP*max(1.0_SP, gaml)*(1.0_SP + max(0.0_SP, gamrth2))/(1.0_SP + tanth1)
            alph = max(1.0_SP, min(2.0_SP, alphin))
            slope = max(0.0_SP, min(alph*gaml, min(alph, betal)))
            outl(i, j) = vin(i, j - 1) + 0.5_SP*slope*typ1
            gamlth1 = tanth1/gaml; if (abs(gaml) < SV) gamlth1 = 0.0_SP
            alphin = 2.0_SP*max(1.0_SP, gamr)*(1.0_SP + max(0.0_SP, gamlth1))/(1.0_SP + tanth2)
            alph = max(1.0_SP, min(2.0_SP, alphin))
            slope = max(0.0_SP, min(alph*gamr, min(alph, betar)))
            outr(i, j) = vin(i, j) - 0.5_SP*slope*typ3
         end do
      end do
   end subroutine construct_ho_y_mlp

   ! ----------------------------------------------------------------
   ! 5th-order WENO reconstruction in x (Qiu & Shu 2005).
   ! Constant dx assumed (Cartesian only).
   ! ----------------------------------------------------------------
   pure subroutine weno_construct_x(lp, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: vin(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      real(SP), parameter :: WNEPS = 1.0e-6_SP
      real(SP), parameter :: R0R = 0.3_SP, R1R = 0.6_SP, R2R = 0.1_SP
      real(SP), parameter :: R0L = 0.1_SP, R1L = 0.6_SP, R2L = 0.3_SP
      real(SP), parameter :: BC1 = 13.0_SP/12.0_SP, BC2 = 0.25_SP
      real(SP) :: wb0, wb1, wb2, tx1, tx2, tx3, wnw0, wnw1, wnw2
      real(SP) :: wp0, wp1, wp2
      integer  :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie + 1
            tx1 = vin(i - 2, j) - 2.0_SP*vin(i - 1, j) + vin(i, j)
            tx2 = vin(i - 2, j) - 4.0_SP*vin(i - 1, j) + 3.0_SP*vin(i, j)
            wb0 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx1 = vin(i - 1, j) - 2.0_SP*vin(i, j) + vin(i + 1, j)
            tx2 = vin(i - 1, j) - vin(i + 1, j)
            wb1 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx1 = vin(i, j) - 2.0_SP*vin(i + 1, j) + vin(i + 2, j)
            tx2 = 3.0_SP*vin(i, j) - 4.0_SP*vin(i + 1, j) + vin(i + 2, j)
            wb2 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx3 = R0R/(WNEPS + wb0)**2 + R1R/(WNEPS + wb1)**2 + R2R/(WNEPS + wb2)**2
            wnw0 = R0R/((WNEPS + wb0)**2*tx3)
            wnw1 = R1R/((WNEPS + wb1)**2*tx3)
            wnw2 = R2R/((WNEPS + wb2)**2*tx3)
            wp0 = -(1.0_SP/6.0_SP)*vin(i - 2, j) + (5.0_SP/6.0_SP)*vin(i - 1, j) + (1.0_SP/3.0_SP)*vin(i, j)
            wp1 = (1.0_SP/3.0_SP)*vin(i - 1, j) + (5.0_SP/6.0_SP)*vin(i, j) - (1.0_SP/6.0_SP)*vin(i + 1, j)
            wp2 = (11.0_SP/6.0_SP)*vin(i, j) - (7.0_SP/6.0_SP)*vin(i + 1, j) + (1.0_SP/3.0_SP)*vin(i + 2, j)
            outr(i, j) = wnw0*wp0 + wnw1*wp1 + wnw2*wp2
            tx1 = vin(i - 3, j) - 2.0_SP*vin(i - 2, j) + vin(i - 1, j)
            tx2 = vin(i - 3, j) - 4.0_SP*vin(i - 2, j) + 3.0_SP*vin(i - 1, j)
            wb0 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx1 = vin(i - 2, j) - 2.0_SP*vin(i - 1, j) + vin(i, j)
            tx2 = vin(i - 2, j) - vin(i, j)
            wb1 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx1 = vin(i - 1, j) - 2.0_SP*vin(i, j) + vin(i + 1, j)
            tx2 = 3.0_SP*vin(i - 1, j) - 4.0_SP*vin(i, j) + vin(i + 1, j)
            wb2 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx3 = R0L/(WNEPS + wb0)**2 + R1L/(WNEPS + wb1)**2 + R2L/(WNEPS + wb2)**2
            wnw0 = R0L/((WNEPS + wb0)**2*tx3)
            wnw1 = R1L/((WNEPS + wb1)**2*tx3)
            wnw2 = R2L/((WNEPS + wb2)**2*tx3)
            wp0 = (1.0_SP/3.0_SP)*vin(i - 3, j) - (7.0_SP/6.0_SP)*vin(i - 2, j) + (11.0_SP/6.0_SP)*vin(i - 1, j)
            wp1 = -(1.0_SP/6.0_SP)*vin(i - 2, j) + (5.0_SP/6.0_SP)*vin(i - 1, j) + (1.0_SP/3.0_SP)*vin(i, j)
            wp2 = (1.0_SP/3.0_SP)*vin(i - 1, j) + (5.0_SP/6.0_SP)*vin(i, j) - (1.0_SP/6.0_SP)*vin(i + 1, j)
            outl(i, j) = wnw0*wp0 + wnw1*wp1 + wnw2*wp2
         end do
      end do
   end subroutine weno_construct_x

   ! ----------------------------------------------------------------
   ! 5th-order WENO reconstruction in y.
   ! ----------------------------------------------------------------
   pure subroutine weno_construct_y(lp, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: vin(:, :)
      real(SP), intent(out) :: outl(:, :), outr(:, :)
      real(SP), parameter :: WNEPS = 1.0e-6_SP
      real(SP), parameter :: R0R = 0.3_SP, R1R = 0.6_SP, R2R = 0.1_SP
      real(SP), parameter :: R0L = 0.1_SP, R1L = 0.6_SP, R2L = 0.3_SP
      real(SP), parameter :: BC1 = 13.0_SP/12.0_SP, BC2 = 0.25_SP
      real(SP) :: wb0, wb1, wb2, ty1, ty2, ty3, wnw0, wnw1, wnw2
      real(SP) :: wp0, wp1, wp2
      integer  :: i, j
      do j = lp%jb, lp%je + 1
         do i = lp%ib, lp%ie
            ty1 = vin(i, j - 2) - 2.0_SP*vin(i, j - 1) + vin(i, j)
            ty2 = vin(i, j - 2) - 4.0_SP*vin(i, j - 1) + 3.0_SP*vin(i, j)
            wb0 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty1 = vin(i, j - 1) - 2.0_SP*vin(i, j) + vin(i, j + 1)
            ty2 = vin(i, j - 1) - vin(i, j + 1)
            wb1 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty1 = vin(i, j) - 2.0_SP*vin(i, j + 1) + vin(i, j + 2)
            ty2 = 3.0_SP*vin(i, j) - 4.0_SP*vin(i, j + 1) + vin(i, j + 2)
            wb2 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty3 = R0R/(WNEPS + wb0)**2 + R1R/(WNEPS + wb1)**2 + R2R/(WNEPS + wb2)**2
            wnw0 = R0R/((WNEPS + wb0)**2*ty3)
            wnw1 = R1R/((WNEPS + wb1)**2*ty3)
            wnw2 = R2R/((WNEPS + wb2)**2*ty3)
            wp0 = -(1.0_SP/6.0_SP)*vin(i, j - 2) + (5.0_SP/6.0_SP)*vin(i, j - 1) + (1.0_SP/3.0_SP)*vin(i, j)
            wp1 = (1.0_SP/3.0_SP)*vin(i, j - 1) + (5.0_SP/6.0_SP)*vin(i, j) - (1.0_SP/6.0_SP)*vin(i, j + 1)
            wp2 = (11.0_SP/6.0_SP)*vin(i, j) - (7.0_SP/6.0_SP)*vin(i, j + 1) + (1.0_SP/3.0_SP)*vin(i, j + 2)
            outr(i, j) = wnw0*wp0 + wnw1*wp1 + wnw2*wp2
            ty1 = vin(i, j - 3) - 2.0_SP*vin(i, j - 2) + vin(i, j - 1)
            ty2 = vin(i, j - 3) - 4.0_SP*vin(i, j - 2) + 3.0_SP*vin(i, j - 1)
            wb0 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty1 = vin(i, j - 2) - 2.0_SP*vin(i, j - 1) + vin(i, j)
            ty2 = vin(i, j - 2) - vin(i, j)
            wb1 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty1 = vin(i, j - 1) - 2.0_SP*vin(i, j) + vin(i, j + 1)
            ty2 = 3.0_SP*vin(i, j - 1) - 4.0_SP*vin(i, j) + vin(i, j + 1)
            wb2 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty3 = R0L/(WNEPS + wb0)**2 + R1L/(WNEPS + wb1)**2 + R2L/(WNEPS + wb2)**2
            wnw0 = R0L/((WNEPS + wb0)**2*ty3)
            wnw1 = R1L/((WNEPS + wb1)**2*ty3)
            wnw2 = R2L/((WNEPS + wb2)**2*ty3)
            wp0 = (1.0_SP/3.0_SP)*vin(i, j - 3) - (7.0_SP/6.0_SP)*vin(i, j - 2) + (11.0_SP/6.0_SP)*vin(i, j - 1)
            wp1 = -(1.0_SP/6.0_SP)*vin(i, j - 2) + (5.0_SP/6.0_SP)*vin(i, j - 1) + (1.0_SP/3.0_SP)*vin(i, j)
            wp2 = (1.0_SP/3.0_SP)*vin(i, j - 1) + (5.0_SP/6.0_SP)*vin(i, j) - (1.0_SP/6.0_SP)*vin(i, j + 1)
            outl(i, j) = wnw0*wp0 + wnw1*wp1 + wnw2*wp2
         end do
      end do
   end subroutine weno_construct_y

   ! ----------------------------------------------------------------
   ! Roe wave speeds (Zhou et al. 2001).
   ! ----------------------------------------------------------------
   pure subroutine wave_speed(lp, uxl, uxr, vyl, vyr, hxl, hxr, hyl, hyr, &
                              sxl, sxr, syl, syr)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: uxl(:, :), uxr(:, :), hxl(:, :), hxr(:, :)
      real(SP), intent(in)  :: vyl(:, :), vyr(:, :), hyl(:, :), hyr(:, :)
      real(SP), intent(out) :: sxl(:, :), sxr(:, :)
      real(SP), intent(out) :: syl(:, :), syr(:, :)
      call wave_speed_strip(lp, 1, lp%nloc + 1, uxl, uxr, vyl, vyr, &
                            hxl, hxr, hyl, hyr, sxl, sxr, syl, syr)
      call wave_speed_bands(lp, sxl, sxr, syl, syr)
   end subroutine wave_speed

   ! ----------------------------------------------------------------
   ! Interior wave-speed compute over a row strip (ghost bands are
   ! mirror fills of these values — wave_speed_bands, post-pass).
   ! ----------------------------------------------------------------
   pure subroutine wave_speed_strip(lp, js, je, uxl, uxr, vyl, vyr, &
                                    hxl, hxr, hyl, hyr, sxl, sxr, syl, syr)
      type(type_loop_bounds), intent(in) :: lp
      integer, intent(in) :: js, je
      real(SP), intent(in)  :: uxl(:, :), uxr(:, :), hxl(:, :), hxr(:, :)
      real(SP), intent(in)  :: vyl(:, :), vyr(:, :), hyl(:, :), hyr(:, :)
      real(SP), intent(inout) :: sxl(:, :), sxr(:, :)
      real(SP), intent(inout) :: syl(:, :), syr(:, :)
      integer  :: i, j, m, n, m1, n1
      real(SP) :: spl, spr, sps, us
      m = lp%mloc; n = lp%nloc; m1 = m + 1; n1 = n + 1
      do j = max(js, 1 + N_GHOST), min(je, n - N_GHOST)
         do i = 1 + N_GHOST, m1 - N_GHOST
            spl = sqrt(GRAV*abs(hxl(i, j))); spr = sqrt(GRAV*abs(hxr(i, j)))
            sps = 0.5_SP*(spl + spr) + 0.25_SP*(uxl(i, j) - uxr(i, j))
            us = 0.5_SP*(uxl(i, j) + uxr(i, j)) + spl - spr
            sxl(i, j) = min(uxl(i, j) - spl, us - sps)
            sxr(i, j) = max(uxr(i, j) + spr, us + sps)
         end do
      end do
      do j = max(js, 1 + N_GHOST), min(je, n1 - N_GHOST)
         do i = 1 + N_GHOST, m - N_GHOST
            spl = sqrt(GRAV*abs(hyl(i, j))); spr = sqrt(GRAV*abs(hyr(i, j)))
            sps = 0.5_SP*(spl + spr) + 0.25_SP*(vyl(i, j) - vyr(i, j))
            us = 0.5_SP*(vyl(i, j) + vyr(i, j)) + spl - spr
            syl(i, j) = min(vyl(i, j) - spl, us - sps)
            syr(i, j) = max(vyr(i, j) + spr, us + sps)
         end do
      end do
   end subroutine wave_speed_strip

   ! ----------------------------------------------------------------
   ! Ghost-band mirror fills for the wave speeds.  Reads only interior
   ! values (or bands filled earlier in this routine), so it must run
   ! after ALL interior strips — order of the four groups preserved
   ! from the original fused form.
   ! ----------------------------------------------------------------
   pure subroutine wave_speed_bands(lp, sxl, sxr, syl, syr)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(inout) :: sxl(:, :), sxr(:, :)
      real(SP), intent(inout) :: syl(:, :), syr(:, :)
      integer  :: i, j, m, n, m1, n1
      m = lp%mloc; n = lp%nloc; m1 = m + 1; n1 = n + 1
      do j = 1 + N_GHOST, n - N_GHOST
         do i = 1, N_GHOST
            sxl(i, j) = sxl(N_GHOST + 1, j); sxr(i, j) = sxr(N_GHOST + 1, j)
         end do
         do i = m1 - N_GHOST + 1, m1
            sxl(i, j) = sxl(m1 - N_GHOST, j); sxr(i, j) = sxr(m1 - N_GHOST, j)
         end do
      end do
      do i = 1, m1
         do j = 1, N_GHOST
            sxl(i, j) = sxl(i, N_GHOST + 1); sxr(i, j) = sxr(i, N_GHOST + 1)
         end do
         do j = n - N_GHOST + 1, n
            sxl(i, j) = sxl(i, n - N_GHOST); sxr(i, j) = sxr(i, n - N_GHOST)
         end do
      end do
      do i = 1 + N_GHOST, m - N_GHOST
         do j = 1, N_GHOST
            syl(i, j) = syl(i, N_GHOST + 1); syr(i, j) = syr(i, N_GHOST + 1)
         end do
         do j = n1 - N_GHOST + 1, n1
            syl(i, j) = syl(i, n1 - N_GHOST); syr(i, j) = syr(i, n1 - N_GHOST)
         end do
      end do
      do j = 1, n1
         do i = 1, N_GHOST
            syl(i, j) = syl(N_GHOST + 1, j); syr(i, j) = syr(N_GHOST + 1, j)
         end do
         do i = m - N_GHOST + 1, m
            syl(i, j) = syl(m - N_GHOST, j); syr(i, j) = syr(m - N_GHOST, j)
         end do
      end do
   end subroutine wave_speed_bands

   ! ----------------------------------------------------------------
   ! HLL flux kernel over the three interface fluxes of one direction,
   ! fused so the wave-speed strips stream once instead of once per
   ! flux (perf audit, cache rung); branch and expression per flux
   ! match the former single-flux kernel exactly.
   ! ----------------------------------------------------------------
   pure subroutine hll3(sl, sr, f1l, f1r, u1l, u1r, f1out, &
                        f2l, f2r, u2l, u2r, f2out, &
                        f3l, f3r, u3l, u3r, f3out, is, ie, js, je)
      real(SP), intent(in)  :: sl(:, :), sr(:, :)
      real(SP), intent(in)  :: f1l(:, :), f1r(:, :), u1l(:, :), u1r(:, :)
      real(SP), intent(in)  :: f2l(:, :), f2r(:, :), u2l(:, :), u2r(:, :)
      real(SP), intent(in)  :: f3l(:, :), f3r(:, :), u3l(:, :), u3r(:, :)
      real(SP), intent(inout) :: f1out(:, :), f2out(:, :), f3out(:, :)
      integer, intent(in)   :: is, ie, js, je
      real(SP) :: rsl, rsr, denom
      integer  :: i, j
      do j = js, je
         do i = is, ie
            rsl = sl(i, j); rsr = sr(i, j)
            if (rsl >= 0.0_SP) then
               f1out(i, j) = f1l(i, j)
               f2out(i, j) = f2l(i, j)
               f3out(i, j) = f3l(i, j)
            else if (rsr <= 0.0_SP) then
               f1out(i, j) = f1r(i, j)
               f2out(i, j) = f2r(i, j)
               f3out(i, j) = f3r(i, j)
            else
               denom = rsr - rsl
               if (abs(denom) < SMALL) denom = SMALL
               f1out(i, j) = (rsr*f1l(i, j) - rsl*f1r(i, j) &
                              + rsl*rsr*(u1r(i, j) - u1l(i, j)))/denom
               f2out(i, j) = (rsr*f2l(i, j) - rsl*f2r(i, j) &
                              + rsl*rsr*(u2r(i, j) - u2l(i, j)))/denom
               f3out(i, j) = (rsr*f3l(i, j) - rsl*f3r(i, j) &
                              + rsl*rsr*(u3r(i, j) - u3l(i, j)))/denom
            end if
         end do
      end do
   end subroutine hll3

   ! ----------------------------------------------------------------
   ! Average-based flux (predictor / averaging approach).
   ! ----------------------------------------------------------------
   subroutine flux_at_interface(ws)
      type(type_flux_workspace), intent(inout) :: ws
      call flux_interface_avg_x(ws, 1, ws%m + 1, 1, ws%n)
      call flux_interface_avg_y(ws, 1, ws%m, 1, ws%n + 1)
   end subroutine flux_at_interface

   subroutine flux_interface_avg_x(ws, is, ie, js, je)
      type(type_flux_workspace), intent(inout) :: ws
      integer, intent(in) :: is, ie, js, je
      ws%p(is:ie, js:je) = 0.5_SP*(ws%pr(is:ie, js:je) + ws%pl(is:ie, js:je))
      ws%fx(is:ie, js:je) = 0.5_SP*(ws%fxr(is:ie, js:je) + ws%fxl(is:ie, js:je))
      ws%gx(is:ie, js:je) = 0.5_SP*(ws%gxr(is:ie, js:je) + ws%gxl(is:ie, js:je))
   end subroutine flux_interface_avg_x

   subroutine flux_interface_avg_y(ws, is, ie, js, je)
      type(type_flux_workspace), intent(inout) :: ws
      integer, intent(in) :: is, ie, js, je
      ws%q(is:ie, js:je) = 0.5_SP*(ws%qr(is:ie, js:je) + ws%ql(is:ie, js:je))
      ws%fy(is:ie, js:je) = 0.5_SP*(ws%fyr(is:ie, js:je) + ws%fyl(is:ie, js:je))
      ws%gy(is:ie, js:je) = 0.5_SP*(ws%gyr(is:ie, js:je) + ws%gyl(is:ie, js:je))
   end subroutine flux_interface_avg_y

   ! ----------------------------------------------------------------
   ! HLL-based flux.
   ! ----------------------------------------------------------------
   subroutine flux_at_interface_hll(ws)
      type(type_flux_workspace), intent(inout) :: ws
      call flux_interface_hll_x(ws, 1, ws%m + 1, 1, ws%n)
      call flux_interface_hll_y(ws, 1, ws%m, 1, ws%n + 1)
   end subroutine flux_at_interface_hll

   subroutine flux_interface_hll_x(ws, is, ie, js, je)
      type(type_flux_workspace), intent(inout) :: ws
      integer, intent(in) :: is, ie, js, je
      call hll3(ws%sxl, ws%sxr, ws%pl, ws%pr, ws%etarxl, ws%etarxr, ws%p, &
                ws%fxl, ws%fxr, ws%huxl, ws%huxr, ws%fx, &
                ws%gxl, ws%gxr, ws%hvxl, ws%hvxr, ws%gx, is, ie, js, je)
   end subroutine flux_interface_hll_x

   subroutine flux_interface_hll_y(ws, is, ie, js, je)
      type(type_flux_workspace), intent(inout) :: ws
      integer, intent(in) :: is, ie, js, je
      call hll3(ws%syl, ws%syr, ws%ql, ws%qr, ws%etaryl, ws%etaryr, ws%q, &
                ws%fyl, ws%fyr, ws%huyl, ws%huyr, ws%fy, &
                ws%gyl, ws%gyr, ws%hvyl, ws%hvyr, ws%gy, is, ie, js, je)
   end subroutine flux_interface_hll_y

   ! ----------------------------------------------------------------
   ! Private helper: assemble P/Fx/Gx from x-interface arrays.
   ! ----------------------------------------------------------------
   subroutine assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion, js, je)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_flux_workspace), intent(inout) :: ws
      real(SP), intent(in) :: depthx(:, :), gamma1, gamma3
      integer, intent(in) :: mask9(:, :)
      logical, intent(in) :: dispersion
      integer, intent(in) :: js, je   ! row strip, ghosts included (1..n)
      integer  :: i, j, ii
      real(SP) :: u4l, u4r, v4l, v4r
      ws%hxl(:, js:je) = ws%etarxl(:, js:je) + depthx(:, js:je)
      ws%hxr(:, js:je) = ws%etarxr(:, js:je) + depthx(:, js:je)
      if (dispersion) then
         do j = js, je
            do i = 1, ws%m + 1
               ii = min(i, ws%m)
               u4l = gamma1*mask9(ii, j)*ws%u4xl(i, j)
               u4r = gamma1*mask9(ii, j)*ws%u4xr(i, j)
               v4l = gamma1*mask9(ii, j)*ws%v4xl(i, j)
               v4r = gamma1*mask9(ii, j)*ws%v4xr(i, j)
               ws%pl(i, j) = ws%huxl(i, j) + ws%hxl(i, j)*u4l
               ws%pr(i, j) = ws%huxr(i, j) + ws%hxr(i, j)*u4r
               ws%fxl(i, j) = gamma3*ws%pl(i, j)*(ws%uxl(i, j) + u4l) &
                              + 0.5_SP*GRAV*(gamma3*ws%etarxl(i, j)**2 + 2.0_SP*ws%etarxl(i, j)*depthx(i, j))
               ws%fxr(i, j) = gamma3*ws%pr(i, j)*(ws%uxr(i, j) + u4r) &
                              + 0.5_SP*GRAV*(gamma3*ws%etarxr(i, j)**2 + 2.0_SP*ws%etarxr(i, j)*depthx(i, j))
               ws%gxl(i, j) = gamma3*ws%hxl(i, j)*(ws%uxl(i, j) + u4l)*(ws%vxl(i, j) + v4l)
               ws%gxr(i, j) = gamma3*ws%hxr(i, j)*(ws%uxr(i, j) + u4r)*(ws%vxr(i, j) + v4r)
            end do
         end do
      else
         do j = js, je
            do i = 1, ws%m + 1
               ws%pl(i, j) = ws%huxl(i, j)
               ws%pr(i, j) = ws%huxr(i, j)
               ws%fxl(i, j) = gamma3*ws%pl(i, j)*ws%uxl(i, j) &
                              + 0.5_SP*GRAV*(gamma3*ws%etarxl(i, j)**2 + 2.0_SP*ws%etarxl(i, j)*depthx(i, j))
               ws%fxr(i, j) = gamma3*ws%pr(i, j)*ws%uxr(i, j) &
                              + 0.5_SP*GRAV*(gamma3*ws%etarxr(i, j)**2 + 2.0_SP*ws%etarxr(i, j)*depthx(i, j))
               ws%gxl(i, j) = gamma3*ws%hxl(i, j)*ws%uxl(i, j)*ws%vxl(i, j)
               ws%gxr(i, j) = gamma3*ws%hxr(i, j)*ws%uxr(i, j)*ws%vxr(i, j)
            end do
         end do
      end if
   end subroutine assemble_x

   ! ----------------------------------------------------------------
   ! Private helper: assemble Q/Fy/Gy from y-interface arrays.
   ! ----------------------------------------------------------------
   subroutine assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion, js, je)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_flux_workspace), intent(inout) :: ws
      real(SP), intent(in) :: depthy(:, :), gamma1, gamma3
      integer, intent(in) :: mask9(:, :)
      logical, intent(in) :: dispersion
      integer, intent(in) :: js, je   ! row strip, ghosts included (1..n+1)
      integer  :: i, j, jj
      real(SP) :: u4l, u4r, v4l, v4r
      ws%hyl(:, js:je) = ws%etaryl(:, js:je) + depthy(:, js:je)
      ws%hyr(:, js:je) = ws%etaryr(:, js:je) + depthy(:, js:je)
      if (dispersion) then
         do j = js, je
            jj = min(j, ws%n)
            do i = 1, ws%m
               v4l = gamma1*mask9(i, jj)*ws%v4yl(i, j)
               v4r = gamma1*mask9(i, jj)*ws%v4yr(i, j)
               u4l = gamma1*mask9(i, jj)*ws%u4yl(i, j)
               u4r = gamma1*mask9(i, jj)*ws%u4yr(i, j)
               ws%ql(i, j) = ws%hvyl(i, j) + ws%hyl(i, j)*v4l
               ws%qr(i, j) = ws%hvyr(i, j) + ws%hyr(i, j)*v4r
               ws%gyl(i, j) = gamma3*ws%ql(i, j)*(ws%vyl(i, j) + v4l) &
                              + 0.5_SP*GRAV*(gamma3*ws%etaryl(i, j)**2 + 2.0_SP*ws%etaryl(i, j)*depthy(i, j))
               ws%gyr(i, j) = gamma3*ws%qr(i, j)*(ws%vyr(i, j) + v4r) &
                              + 0.5_SP*GRAV*(gamma3*ws%etaryr(i, j)**2 + 2.0_SP*ws%etaryr(i, j)*depthy(i, j))
               ws%fyl(i, j) = gamma3*ws%hyl(i, j)*(ws%uyl(i, j) + u4l)*(ws%vyl(i, j) + v4l)
               ws%fyr(i, j) = gamma3*ws%hyr(i, j)*(ws%uyr(i, j) + u4r)*(ws%vyr(i, j) + v4r)
            end do
         end do
      else
         do j = js, je
            do i = 1, ws%m
               ws%ql(i, j) = ws%hvyl(i, j)
               ws%qr(i, j) = ws%hvyr(i, j)
               ws%gyl(i, j) = gamma3*ws%ql(i, j)*ws%vyl(i, j) &
                              + 0.5_SP*GRAV*(gamma3*ws%etaryl(i, j)**2 + 2.0_SP*ws%etaryl(i, j)*depthy(i, j))
               ws%gyr(i, j) = gamma3*ws%qr(i, j)*ws%vyr(i, j) &
                              + 0.5_SP*GRAV*(gamma3*ws%etaryr(i, j)**2 + 2.0_SP*ws%etaryr(i, j)*depthy(i, j))
               ws%fyl(i, j) = gamma3*ws%hyl(i, j)*ws%uyl(i, j)*ws%vyl(i, j)
               ws%fyr(i, j) = gamma3*ws%hyr(i, j)*ws%uyr(i, j)*ws%vyr(i, j)
            end do
         end do
      end if
   end subroutine assemble_y

   ! ----------------------------------------------------------------
   ! Basic (1st-order) CONSTRUCTION: van Leer slopes + construct_x/y.
   ! ----------------------------------------------------------------
   subroutine construction(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                           dx, dy, inv_dx, inv_dy, mask, mask9, &
                           gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)  :: eta(:, :), u(:, :), v(:, :), hu(:, :), hv(:, :)
      real(SP), intent(in)  :: u4(:, :), v4(:, :)
      real(SP), intent(in)  :: depthx(:, :), depthy(:, :)
      real(SP), intent(in)  :: dx(:, :), dy(:, :), inv_dx(:, :), inv_dy(:, :)
      integer, intent(in)  :: mask(:, :), mask9(:, :)
      real(SP), intent(in)  :: gamma1, gamma3
      logical, intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      call delx_fun(inv_dx, eta, ws%sl); call construct_x(dx, eta, ws%sl, ws%etarxl, ws%etarxr)
      call delx_fun(inv_dx, u, ws%sl); call construct_x(dx, u, ws%sl, ws%uxl, ws%uxr)
      call delx_fun(inv_dx, v, ws%sl); call construct_x(dx, v, ws%sl, ws%vxl, ws%vxr)
      call delx_fun(inv_dx, hu, ws%sl); call construct_x(dx, hu, ws%sl, ws%huxl, ws%huxr)
      call delx_fun(inv_dx, hv, ws%sl); call construct_x(dx, hv, ws%sl, ws%hvxl, ws%hvxr)
      if (dispersion) then
         call delx_fun(inv_dx, u4, ws%sl); call construct_x(dx, u4, ws%sl, ws%u4xl, ws%u4xr)
         call delx_fun(inv_dx, v4, ws%sl); call construct_x(dx, v4, ws%sl, ws%v4xl, ws%v4xr)
      end if
      call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion, 1, lp%nloc)
      call dely_fun(inv_dy, eta, ws%sl); call construct_y(dy, eta, ws%sl, ws%etaryl, ws%etaryr)
      call dely_fun(inv_dy, u, ws%sl); call construct_y(dy, u, ws%sl, ws%uyl, ws%uyr)
      call dely_fun(inv_dy, v, ws%sl); call construct_y(dy, v, ws%sl, ws%vyl, ws%vyr)
      call dely_fun(inv_dy, hv, ws%sl); call construct_y(dy, hv, ws%sl, ws%hvyl, ws%hvyr)
      call dely_fun(inv_dy, hu, ws%sl); call construct_y(dy, hu, ws%sl, ws%huyl, ws%huyr)
      if (dispersion) then
         call dely_fun(inv_dy, v4, ws%sl); call construct_y(dy, v4, ws%sl, ws%v4yl, ws%v4yr)
         call dely_fun(inv_dy, u4, ws%sl); call construct_y(dy, u4, ws%sl, ws%u4yl, ws%u4yr)
      end if
      call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion, 1, lp%nloc + 1)
   end subroutine construction

   ! ----------------------------------------------------------------
   ! High-order 'FOU' path, j-strip blocked end to end: 4th-order
   ! van Leer+minmod construction, wave speeds, and interface fluxes.
   ! ----------------------------------------------------------------
   subroutine fluxes_ho_blocked(lp, constr, eta, u, v, hu, hv, u4, v4, &
                                depthx, depthy, mask, mask9, gamma1, gamma3, &
                                dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      character(len=*), intent(in)    :: constr
      real(SP), intent(in)  :: eta(:, :), u(:, :), v(:, :), hu(:, :), hv(:, :)
      real(SP), intent(in)  :: u4(:, :), v4(:, :)
      real(SP), intent(in)  :: depthx(:, :), depthy(:, :)
      integer, intent(in)  :: mask(:, :), mask9(:, :)
      real(SP), intent(in)  :: gamma1, gamma3
      logical, intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      type(type_loop_bounds) :: lps
      integer :: j0, j1, jsx, jex, jsy, jey, jsf, jef, ng, m, n, m1, n1
      logical :: use_hll
      use_hll = constr(1:3) == "HLL"
      ng = N_GHOST; m = lp%mloc; n = lp%nloc; m1 = m + 1; n1 = n + 1
      ! j-strip blocking: run every sweep — constructs, assembles,
      ! wave speeds, interface fluxes — over one strip of rows before
      ! moving north, so each stage's outputs are still cache-resident
      ! when the next re-reads them.  Pure visit-order change — the
      ! kernels are pointwise or read-only-stencil in j, so per-cell
      ! arithmetic is untouched (bitwise).  Every write is strip-owned:
      ! the y-face rows are partitioned jsf..jef (a strip writes only
      ! faces inside its own rows, no je+1 seam spill), so strips are
      ! fully independent and the loop threads directly; static schedule
      ! + identical per-strip arithmetic keeps any thread count bitwise.
      ! Interface fluxes cover only the interior-complete block in-strip;
      ! ghost rows and cols wait on the wave-speed mirror fills
      ! (post-pass below, serial).
      ! FUTURE: extend to the other reconstruction variants if the
      ! FOU win holds
      lps = lp
      !$omp parallel do default(shared) schedule(static) firstprivate(lps) &
      !$omp& private(j1, jsx, jex, jsy, jey, jsf, jef)
      do j0 = 1, n1, J_BLOCK
         j1 = min(j0 + J_BLOCK - 1, n1)
         lps%jb = max(lp%jb, j0)
         lps%je = min(lp%je, j1)
         if (lps%jb <= lps%je) then
            call construct_ho_x(lps, mask, eta, ws%etarxl, ws%etarxr)
            call construct_ho_x(lps, mask, u, ws%uxl, ws%uxr)
            call construct_ho_x(lps, mask, v, ws%vxl, ws%vxr)
            call construct_ho_x(lps, mask, hu, ws%huxl, ws%huxr)
            call construct_ho_x(lps, mask, hv, ws%hvxl, ws%hvxr)
            if (dispersion) then
               call construct_ho_x(lps, mask, u4, ws%u4xl, ws%u4xr)
               call construct_ho_x(lps, mask, v4, ws%v4xl, ws%v4xr)
            end if
         end if
         if (j0 <= n) then
            call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion, &
                            j0, min(j1, n))
         end if
         jsf = max(lp%jb, j0); jef = min(lp%je + 1, j1)
         if (jsf <= jef) then
            call construct_ho_y(lp, mask, eta, ws%etaryl, ws%etaryr, jsf, jef)
            call construct_ho_y(lp, mask, u, ws%uyl, ws%uyr, jsf, jef)
            call construct_ho_y(lp, mask, v, ws%vyl, ws%vyr, jsf, jef)
            call construct_ho_y(lp, mask, hv, ws%hvyl, ws%hvyr, jsf, jef)
            call construct_ho_y(lp, mask, hu, ws%huyl, ws%huyr, jsf, jef)
            if (dispersion) then
               call construct_ho_y(lp, mask, v4, ws%v4yl, ws%v4yr, jsf, jef)
               call construct_ho_y(lp, mask, u4, ws%u4yl, ws%u4yr, jsf, jef)
            end if
         end if
         call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion, j0, j1)
         call wave_speed_strip(lp, j0, j1, ws%uxl, ws%uxr, ws%vyl, ws%vyr, &
                               ws%hxl, ws%hxr, ws%hyl, ws%hyr, &
                               ws%sxl, ws%sxr, ws%syl, ws%syr)
         jsx = max(j0, 1 + ng); jex = min(j1, n - ng)
         jsy = max(j0, 1 + ng); jey = min(j1, n1 - ng)
         if (use_hll) then
            if (jsx <= jex) call flux_interface_hll_x(ws, 1 + ng, m1 - ng, jsx, jex)
            if (jsy <= jey) call flux_interface_hll_y(ws, 1 + ng, m - ng, jsy, jey)
         else
            if (jsx <= jex) call flux_interface_avg_x(ws, 1 + ng, m1 - ng, jsx, jex)
            if (jsy <= jey) call flux_interface_avg_y(ws, 1 + ng, m - ng, jsy, jey)
         end if
      end do
      !$omp end parallel do
      call wave_speed_bands(lp, ws%sxl, ws%sxr, ws%syl, ws%syr)
      ! ghost-frame interface fluxes: south/north rows full-width,
      ! west/east cols over the interior rows
      if (use_hll) then
         call flux_interface_hll_x(ws, 1, m1, 1, ng)
         call flux_interface_hll_x(ws, 1, m1, n - ng + 1, n)
         call flux_interface_hll_x(ws, 1, ng, ng + 1, n - ng)
         call flux_interface_hll_x(ws, m1 - ng + 1, m1, ng + 1, n - ng)
         call flux_interface_hll_y(ws, 1, m, 1, ng)
         call flux_interface_hll_y(ws, 1, m, n1 - ng + 1, n1)
         call flux_interface_hll_y(ws, 1, ng, ng + 1, n1 - ng)
         call flux_interface_hll_y(ws, m - ng + 1, m, ng + 1, n1 - ng)
      else
         call flux_interface_avg_x(ws, 1, m1, 1, ng)
         call flux_interface_avg_x(ws, 1, m1, n - ng + 1, n)
         call flux_interface_avg_x(ws, 1, ng, ng + 1, n - ng)
         call flux_interface_avg_x(ws, m1 - ng + 1, m1, ng + 1, n - ng)
         call flux_interface_avg_y(ws, 1, m, 1, ng)
         call flux_interface_avg_y(ws, 1, m, n1 - ng + 1, n1)
         call flux_interface_avg_y(ws, 1, ng, ng + 1, n1 - ng)
         call flux_interface_avg_y(ws, m - ng + 1, m, ng + 1, n1 - ng)
      end if
   end subroutine fluxes_ho_blocked

   ! ----------------------------------------------------------------
   ! 'FMI': 4th-order minmod-only.
   ! ----------------------------------------------------------------
   subroutine construction_ho_minmod(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                     mask, mask9, gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)  :: eta(:, :), u(:, :), v(:, :), hu(:, :), hv(:, :)
      real(SP), intent(in)  :: u4(:, :), v4(:, :)
      real(SP), intent(in)  :: depthx(:, :), depthy(:, :)
      integer, intent(in)  :: mask(:, :), mask9(:, :)
      real(SP), intent(in)  :: gamma1, gamma3
      logical, intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      call construct_ho_x_minmod(lp, mask, eta, ws%etarxl, ws%etarxr)
      call construct_ho_x_minmod(lp, mask, u, ws%uxl, ws%uxr)
      call construct_ho_x_minmod(lp, mask, v, ws%vxl, ws%vxr)
      call construct_ho_x_minmod(lp, mask, hu, ws%huxl, ws%huxr)
      call construct_ho_x_minmod(lp, mask, hv, ws%hvxl, ws%hvxr)
      if (dispersion) then
         call construct_ho_x_minmod(lp, mask, u4, ws%u4xl, ws%u4xr)
         call construct_ho_x_minmod(lp, mask, v4, ws%v4xl, ws%v4xr)
      end if
      call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion, 1, lp%nloc)
      call construct_ho_y_minmod(lp, mask, eta, ws%etaryl, ws%etaryr)
      call construct_ho_y_minmod(lp, mask, u, ws%uyl, ws%uyr)
      call construct_ho_y_minmod(lp, mask, v, ws%vyl, ws%vyr)
      call construct_ho_y_minmod(lp, mask, hv, ws%hvyl, ws%hvyr)
      call construct_ho_y_minmod(lp, mask, hu, ws%huyl, ws%huyr)
      if (dispersion) then
         call construct_ho_y_minmod(lp, mask, v4, ws%v4yl, ws%v4yr)
         call construct_ho_y_minmod(lp, mask, u4, ws%u4yl, ws%u4yr)
      end if
      call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion, 1, lp%nloc + 1)
   end subroutine construction_ho_minmod

   ! ----------------------------------------------------------------
   ! 'MLP': MLP reconstruction.
   ! ----------------------------------------------------------------
   subroutine construction_ho_mlp(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                  mask, mask9, gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)  :: eta(:, :), u(:, :), v(:, :), hu(:, :), hv(:, :)
      real(SP), intent(in)  :: u4(:, :), v4(:, :)
      real(SP), intent(in)  :: depthx(:, :), depthy(:, :)
      integer, intent(in)  :: mask(:, :), mask9(:, :)
      real(SP), intent(in)  :: gamma1, gamma3
      logical, intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      call construct_ho_x_mlp(lp, mask, eta, ws%etarxl, ws%etarxr)
      call construct_ho_x_mlp(lp, mask, u, ws%uxl, ws%uxr)
      call construct_ho_x_mlp(lp, mask, v, ws%vxl, ws%vxr)
      call construct_ho_x_mlp(lp, mask, hu, ws%huxl, ws%huxr)
      call construct_ho_x_mlp(lp, mask, hv, ws%hvxl, ws%hvxr)
      if (dispersion) then
         call construct_ho_x_mlp(lp, mask, u4, ws%u4xl, ws%u4xr)
         call construct_ho_x_mlp(lp, mask, v4, ws%v4xl, ws%v4xr)
      end if
      call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion, 1, lp%nloc)
      call construct_ho_y_mlp(lp, mask, eta, ws%etaryl, ws%etaryr)
      call construct_ho_y_mlp(lp, mask, u, ws%uyl, ws%uyr)
      call construct_ho_y_mlp(lp, mask, v, ws%vyl, ws%vyr)
      call construct_ho_y_mlp(lp, mask, hv, ws%hvyl, ws%hvyr)
      call construct_ho_y_mlp(lp, mask, hu, ws%huyl, ws%huyr)
      if (dispersion) then
         call construct_ho_y_mlp(lp, mask, v4, ws%v4yl, ws%v4yr)
         call construct_ho_y_mlp(lp, mask, u4, ws%u4yl, ws%u4yr)
      end if
      call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion, 1, lp%nloc + 1)
   end subroutine construction_ho_mlp

   ! ----------------------------------------------------------------
   ! 'WEN': WENO5 reconstruction (Cartesian, constant dx).
   ! ----------------------------------------------------------------
   subroutine construction_weno(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                mask9, gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)  :: eta(:, :), u(:, :), v(:, :), hu(:, :), hv(:, :)
      real(SP), intent(in)  :: u4(:, :), v4(:, :)
      real(SP), intent(in)  :: depthx(:, :), depthy(:, :)
      integer, intent(in)  :: mask9(:, :)
      real(SP), intent(in)  :: gamma1, gamma3
      logical, intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      call weno_construct_x(lp, eta, ws%etarxl, ws%etarxr)
      call weno_construct_x(lp, u, ws%uxl, ws%uxr)
      call weno_construct_x(lp, v, ws%vxl, ws%vxr)
      call weno_construct_x(lp, hu, ws%huxl, ws%huxr)
      call weno_construct_x(lp, hv, ws%hvxl, ws%hvxr)
      if (dispersion) then
         call weno_construct_x(lp, u4, ws%u4xl, ws%u4xr)
         call weno_construct_x(lp, v4, ws%v4xl, ws%v4xr)
      end if
      call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion, 1, lp%nloc)
      call weno_construct_y(lp, eta, ws%etaryl, ws%etaryr)
      call weno_construct_y(lp, u, ws%uyl, ws%uyr)
      call weno_construct_y(lp, v, ws%vyl, ws%vyr)
      call weno_construct_y(lp, hv, ws%hvyl, ws%hvyr)
      call weno_construct_y(lp, hu, ws%huyl, ws%huyr)
      if (dispersion) then
         call weno_construct_y(lp, v4, ws%v4yl, ws%v4yr)
         call weno_construct_y(lp, u4, ws%u4yl, ws%u4yr)
      end if
      call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion, 1, lp%nloc + 1)
   end subroutine construction_weno

   ! ----------------------------------------------------------------
   ! Top-level dispatcher.  Does NOT call boundary_condition —
   ! the caller is responsible for ghost-cell exchange.
   ! ----------------------------------------------------------------
   subroutine fluxes(lp, high_order, constr, &
                     eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                     dx, dy, inv_dx, inv_dy, mask, mask9, gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      character(len=*), intent(in)    :: high_order, constr
      real(SP), intent(in)  :: eta(:, :), u(:, :), v(:, :), hu(:, :), hv(:, :)
      real(SP), intent(in)  :: u4(:, :), v4(:, :)
      real(SP), intent(in)  :: depthx(:, :), depthy(:, :)
      real(SP), intent(in)  :: dx(:, :), dy(:, :), inv_dx(:, :), inv_dy(:, :)
      integer, intent(in)  :: mask(:, :), mask9(:, :)
      real(SP), intent(in)  :: gamma1, gamma3
      logical, intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws

      ! the FOU default path is j-strip blocked through the whole
      ! chain; the other variants keep the full-sweep structure
      if (high_order(1:3) == "FOU") then
         call fluxes_ho_blocked(lp, constr, eta, u, v, hu, hv, u4, v4, &
                                depthx, depthy, mask, mask9, gamma1, gamma3, &
                                dispersion, ws)
         return
      end if

      select case (high_order(1:3))
      case ("FMI")
         call construction_ho_minmod(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                     mask, mask9, gamma1, gamma3, dispersion, ws)
      case ("WEN")
         call construction_weno(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                mask9, gamma1, gamma3, dispersion, ws)
      case ("MLP")
         call construction_ho_mlp(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                  mask, mask9, gamma1, gamma3, dispersion, ws)
      case default
         call construction(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                           dx, dy, inv_dx, inv_dy, mask, mask9, gamma1, gamma3, dispersion, ws)
      end select

      call wave_speed(lp, ws%uxl, ws%uxr, ws%vyl, ws%vyr, &
                      ws%hxl, ws%hxr, ws%hyl, ws%hyr, &
                      ws%sxl, ws%sxr, ws%syl, ws%syr)

      if (constr(1:3) == "HLL") then
         call flux_at_interface_hll(ws)
      else
         call flux_at_interface(ws)
      end if
   end subroutine fluxes

   ! ----------------------------------------------------------------
   ! flux_wall_bc — wall-face flux enforcement (legacy
   ! BOUNDARY_CONDITION, old/bc.F): zero normal mass/advective flux
   ! through closed walls, hydrostatic pressure only in the normal
   ! momentum flux,
   !   $$ F_{wall} = \tfrac{1}{2} g\,(\gamma_3\,\xi^2 + 2\,\xi\,d) $$
   ! with $\xi$ the interior-side face reconstruction of $\eta$.
   ! Callers pass the bc fill flags: wavemaker-owned west and
   ! periodic-wrapped faces are excluded there, exactly as legacy.
   ! ----------------------------------------------------------------
   subroutine flux_wall_bc(lp, fill_west, fill_east, fill_south, fill_north, &
                           gamma3, depthx, depthy, ws)
      type(type_loop_bounds), intent(in) :: lp
      logical, intent(in) :: fill_west, fill_east, fill_south, fill_north
      real(SP), intent(in) :: gamma3
      real(SP), intent(in) :: depthx(:, :), depthy(:, :)
      type(type_flux_workspace), intent(inout) :: ws

      real(SP) :: xi
      integer :: i, j

      if (fill_west) then
         do j = lp%jb, lp%je
            xi = ws%etarxr(lp%ib, j)
            ws%p(lp%ib, j) = 0.0_SP
            ws%fx(lp%ib, j) = 0.5_SP*GRAV*(xi*xi*gamma3 + 2.0_SP*xi*depthx(lp%ib, j))
            ws%gx(lp%ib, j) = 0.0_SP
         end do
      end if

      if (fill_east) then
         do j = lp%jb, lp%je
            xi = ws%etarxl(lp%ie + 1, j)
            ws%p(lp%ie + 1, j) = 0.0_SP
            ws%fx(lp%ie + 1, j) = 0.5_SP*GRAV*(xi*xi*gamma3 + 2.0_SP*xi*depthx(lp%ie + 1, j))
            ws%gx(lp%ie + 1, j) = 0.0_SP
         end do
      end if

      if (fill_south) then
         do i = lp%ib, lp%ie
            xi = ws%etaryr(i, lp%jb)
            ws%q(i, lp%jb) = 0.0_SP
            ws%fy(i, lp%jb) = 0.0_SP
            ws%gy(i, lp%jb) = 0.5_SP*GRAV*(xi*xi*gamma3 + 2.0_SP*xi*depthy(i, lp%jb))
         end do
      end if

      if (fill_north) then
         do i = lp%ib, lp%ie
            xi = ws%etaryl(i, lp%je + 1)
            ws%q(i, lp%je + 1) = 0.0_SP
            ws%fy(i, lp%je + 1) = 0.0_SP
            ws%gy(i, lp%je + 1) = 0.5_SP*GRAV*(xi*xi*gamma3 + 2.0_SP*xi*depthy(i, lp%je + 1))
         end do
      end if

   end subroutine flux_wall_bc

   ! ----------------------------------------------------------------
   ! flux_dry_bc — dry-cell face flux enforcement (the "mask points"
   ! section of legacy BOUNDARY_CONDITION, old/bc.F).  Every face of a
   ! dry cell carries zero mass and cross flux; the normal momentum
   ! flux keeps hydrostatic pressure only,
   !   $$ F = \tfrac{1}{2} g\,(\gamma_3\,\xi^2 + 2\,\xi\,d)\,m_{nb} $$
   ! with $\xi$ the NEIGHBOUR-side face reconstruction of $\eta$ and
   ! $m_{nb}$ the neighbour's mask (zero when it is dry too).  Faces
   ! on a physical domain edge are zeroed outright.  Zero mass flux
   ! on all faces freezes dry-cell eta exactly, matching legacy.
   ! Walls are purely topological here (MPI_PROC_NULL in legacy) — no
   ! wavemaker exemption, unlike the flux_wall_bc fill flags.
   ! Runs after flux_wall_bc; dry-cell overrides win at dry wall cells.
   ! ----------------------------------------------------------------
   subroutine flux_dry_bc(lp, west_wall, east_wall, south_wall, north_wall, &
                          gamma3, mask, depthx, depthy, ws)
      type(type_loop_bounds), intent(in) :: lp
      logical, intent(in) :: west_wall, east_wall, south_wall, north_wall
      real(SP), intent(in) :: gamma3
      integer, intent(in) :: mask(:, :)
      real(SP), intent(in) :: depthx(:, :), depthy(:, :)
      type(type_flux_workspace), intent(inout) :: ws

      real(SP) :: xi
      integer :: i, j

      do j = lp%jb - 1, lp%je + 1
         do i = lp%ib - 1, lp%ie + 1
            if (mask(i, j) >= 1) cycle

            ws%p(i, j) = 0.0_SP
            if (i == lp%ib .and. west_wall) then
               ws%fx(i, j) = 0.0_SP
            else
               xi = ws%etarxl(i, j)
               ws%fx(i, j) = 0.5_SP*GRAV*(xi*xi*gamma3 &
                                          + 2.0_SP*xi*depthx(i, j))*mask(i - 1, j)
            end if
            ws%gx(i, j) = 0.0_SP

            ws%p(i + 1, j) = 0.0_SP
            if (i == lp%ie .and. east_wall) then
               ws%fx(i + 1, j) = 0.0_SP
            else
               xi = ws%etarxr(i + 1, j)
               ws%fx(i + 1, j) = 0.5_SP*GRAV*(xi*xi*gamma3 &
                                              + 2.0_SP*xi*depthx(i + 1, j))*mask(i + 1, j)
            end if
            ws%gx(i + 1, j) = 0.0_SP

            ws%q(i, j) = 0.0_SP
            ws%fy(i, j) = 0.0_SP
            if (j == lp%jb .and. south_wall) then
               ws%gy(i, j) = 0.0_SP
            else
               xi = ws%etaryl(i, j)
               ws%gy(i, j) = 0.5_SP*GRAV*(xi*xi*gamma3 &
                                          + 2.0_SP*xi*depthy(i, j))*mask(i, j - 1)
            end if

            ws%q(i, j + 1) = 0.0_SP
            ws%fy(i, j + 1) = 0.0_SP
            if (j == lp%je .and. north_wall) then
               ws%gy(i, j + 1) = 0.0_SP
            else
               xi = ws%etaryr(i, j + 1)
               ws%gy(i, j + 1) = 0.5_SP*GRAV*(xi*xi*gamma3 &
                                              + 2.0_SP*xi*depthy(i, j + 1))*mask(i, j + 1)
            end if
         end do
      end do

   end subroutine flux_dry_bc

   ! ----------------------------------------------------------------
   ! Private: minmod of three values (preserving sign of A).
   ! ----------------------------------------------------------------
   pure function minmod3(a, b, c) result(r)
      real(SP), intent(in) :: a, b, c
      real(SP) :: r
      if (a >= 0.0_SP) then
         r = max(0.0_SP, min(a, b, c))
      else
         r = min(0.0_SP, max(a, b, c))
      end if
   end function minmod3

end module model_kernel_fluxes_mod
