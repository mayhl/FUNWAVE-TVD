! allow(E001)
module model_kernel_fluxes_mod
   use core_constants_mod, only: SP, N_GHOST, GRAV
   use core_grid_mod,      only: type_loop_bounds
   implicit none
   private

   real(SP), parameter :: SMALL = 1.0e-6_SP

   ! Interface arrays for one time-step; caller allocates once and reuses.
   type, public :: type_flux_workspace
      integer :: m = 0, n = 0
      ! x-interface: (m+1) x n
      real(SP), allocatable :: uxl(:,:),    uxr(:,:)
      real(SP), allocatable :: vxl(:,:),    vxr(:,:)
      real(SP), allocatable :: huxl(:,:),   huxr(:,:)
      real(SP), allocatable :: hvxl(:,:),   hvxr(:,:)
      real(SP), allocatable :: etarxl(:,:), etarxr(:,:)
      real(SP), allocatable :: hxl(:,:),    hxr(:,:)
      real(SP), allocatable :: u4xl(:,:),   u4xr(:,:)
      real(SP), allocatable :: v4xl(:,:),   v4xr(:,:)
      real(SP), allocatable :: pl(:,:),     pr(:,:)
      real(SP), allocatable :: fxl(:,:),    fxr(:,:)
      real(SP), allocatable :: gxl(:,:),    gxr(:,:)
      real(SP), allocatable :: sxl(:,:),    sxr(:,:)
      ! y-interface: m x (n+1)
      real(SP), allocatable :: uyl(:,:),    uyr(:,:)
      real(SP), allocatable :: vyl(:,:),    vyr(:,:)
      real(SP), allocatable :: huyl(:,:),   huyr(:,:)
      real(SP), allocatable :: hvyl(:,:),   hvyr(:,:)
      real(SP), allocatable :: etaryl(:,:), etaryr(:,:)
      real(SP), allocatable :: hyl(:,:),    hyr(:,:)
      real(SP), allocatable :: u4yl(:,:),   u4yr(:,:)
      real(SP), allocatable :: v4yl(:,:),   v4yr(:,:)
      real(SP), allocatable :: ql(:,:),     qr(:,:)
      real(SP), allocatable :: fyl(:,:),    fyr(:,:)
      real(SP), allocatable :: gyl(:,:),    gyr(:,:)
      real(SP), allocatable :: syl(:,:),    syr(:,:)
      ! output fluxes: (m+1)xn and mx(n+1)
      real(SP), allocatable :: p(:,:),  fx(:,:),  gx(:,:)
      real(SP), allocatable :: q(:,:),  fy(:,:),  gy(:,:)
      ! slope scratch for construction: m x n
      real(SP), allocatable :: sl(:,:)
   contains
      procedure :: alloc => fws_alloc
      procedure :: free  => fws_free
   end type type_flux_workspace

   public :: delx_fun, dely_fun
   public :: construct_x, construct_y
   public :: construct_ho_x, construct_ho_y
   public :: construct_ho_x_minmod, construct_ho_y_minmod
   public :: construct_ho_x_mlp, construct_ho_y_mlp
   public :: weno_construct_x, weno_construct_y
   public :: wave_speed, hll
   public :: flux_at_interface, flux_at_interface_hll
   public :: construction, construction_ho
   public :: construction_ho_minmod, construction_ho_mlp
   public :: construction_weno
   public :: fluxes

contains

   ! ----------------------------------------------------------------
   ! Workspace alloc / free
   ! ----------------------------------------------------------------
   subroutine fws_alloc(ws, m, n)
      class(type_flux_workspace), intent(inout) :: ws
      integer, intent(in) :: m, n
      integer :: m1, n1
      ws%m = m;  ws%n = n
      m1 = m + 1;  n1 = n + 1
      allocate(ws%uxl(m1,n),    ws%uxr(m1,n))
      allocate(ws%vxl(m1,n),    ws%vxr(m1,n))
      allocate(ws%huxl(m1,n),   ws%huxr(m1,n))
      allocate(ws%hvxl(m1,n),   ws%hvxr(m1,n))
      allocate(ws%etarxl(m1,n), ws%etarxr(m1,n))
      allocate(ws%hxl(m1,n),    ws%hxr(m1,n))
      allocate(ws%u4xl(m1,n),   ws%u4xr(m1,n))
      allocate(ws%v4xl(m1,n),   ws%v4xr(m1,n))
      allocate(ws%pl(m1,n),     ws%pr(m1,n))
      allocate(ws%fxl(m1,n),    ws%fxr(m1,n))
      allocate(ws%gxl(m1,n),    ws%gxr(m1,n))
      allocate(ws%sxl(m1,n),    ws%sxr(m1,n))
      allocate(ws%uyl(m,n1),    ws%uyr(m,n1))
      allocate(ws%vyl(m,n1),    ws%vyr(m,n1))
      allocate(ws%huyl(m,n1),   ws%huyr(m,n1))
      allocate(ws%hvyl(m,n1),   ws%hvyr(m,n1))
      allocate(ws%etaryl(m,n1), ws%etaryr(m,n1))
      allocate(ws%hyl(m,n1),    ws%hyr(m,n1))
      allocate(ws%u4yl(m,n1),   ws%u4yr(m,n1))
      allocate(ws%v4yl(m,n1),   ws%v4yr(m,n1))
      allocate(ws%ql(m,n1),     ws%qr(m,n1))
      allocate(ws%fyl(m,n1),    ws%fyr(m,n1))
      allocate(ws%gyl(m,n1),    ws%gyr(m,n1))
      allocate(ws%syl(m,n1),    ws%syr(m,n1))
      allocate(ws%p(m1,n),  ws%fx(m1,n),  ws%gx(m1,n))
      allocate(ws%q(m,n1),  ws%fy(m,n1),  ws%gy(m,n1))
      allocate(ws%sl(m,n))
   end subroutine fws_alloc

   subroutine fws_free(ws)
      class(type_flux_workspace), intent(inout) :: ws
      if (.not. allocated(ws%p)) return
      deallocate(ws%uxl,    ws%uxr,    ws%vxl,    ws%vxr)
      deallocate(ws%huxl,   ws%huxr,   ws%hvxl,   ws%hvxr)
      deallocate(ws%etarxl, ws%etarxr, ws%hxl,    ws%hxr)
      deallocate(ws%u4xl,   ws%u4xr,   ws%v4xl,   ws%v4xr)
      deallocate(ws%pl,     ws%pr,     ws%fxl,    ws%fxr)
      deallocate(ws%gxl,    ws%gxr,    ws%sxl,    ws%sxr)
      deallocate(ws%uyl,    ws%uyr,    ws%vyl,    ws%vyr)
      deallocate(ws%huyl,   ws%huyr,   ws%hvyl,   ws%hvyr)
      deallocate(ws%etaryl, ws%etaryr, ws%hyl,    ws%hyr)
      deallocate(ws%u4yl,   ws%u4yr,   ws%v4yl,   ws%v4yr)
      deallocate(ws%ql,     ws%qr,     ws%fyl,    ws%fyr)
      deallocate(ws%gyl,    ws%gyr,    ws%syl,    ws%syr)
      deallocate(ws%p,  ws%fx,  ws%gx)
      deallocate(ws%q,  ws%fy,  ws%gy)
      deallocate(ws%sl)
      ws%m = 0;  ws%n = 0
   end subroutine fws_free

   ! ----------------------------------------------------------------
   ! Van Leer limited slope in x.  Takes inv_dx (no division by grid
   ! spacing); the limiter's own ratio division is data-dependent and
   ! intrinsic to the scheme.  Deliberately full-array (size-based, not
   ! lp): interface arrays are staggered and ghost slopes feed the
   ! boundary reconstruction, matching legacy DelxFun.
   ! ----------------------------------------------------------------
   pure subroutine delx_fun(inv_dx, din, dout)
      real(SP), intent(in)  :: inv_dx(:,:), din(:,:)
      real(SP), intent(out) :: dout(:,:)
      integer  :: i, j, m, n
      real(SP) :: tmp1, tmp2
      m = size(din, 1);  n = size(din, 2)
      do j = 1, n
         do i = 2, m - 1
            tmp1 = (din(i+1,j) - din(i,j))   * inv_dx(i,j)
            tmp2 = (din(i,j)   - din(i-1,j)) * inv_dx(i-1,j)
            if (abs(tmp1) + abs(tmp2) < SMALL) then
               dout(i,j) = 0.0_SP
            else
               dout(i,j) = (tmp1*abs(tmp2) + abs(tmp1)*tmp2) / (abs(tmp1) + abs(tmp2))
            end if
         end do
         dout(1,j) = (din(2,j)   - din(1,j))   * inv_dx(1,j)
         dout(m,j) = (din(m,j)   - din(m-1,j)) * inv_dx(m,j)
      end do
   end subroutine delx_fun

   ! ----------------------------------------------------------------
   ! Van Leer limited slope in y (takes inv_dy; see delx_fun notes).
   ! ----------------------------------------------------------------
   pure subroutine dely_fun(inv_dy, din, dout)
      real(SP), intent(in)  :: inv_dy(:,:), din(:,:)
      real(SP), intent(out) :: dout(:,:)
      integer  :: i, j, m, n
      real(SP) :: tmp1, tmp2
      m = size(din, 1);  n = size(din, 2)
      do i = 1, m
         do j = 2, n - 1
            tmp1 = (din(i,j+1) - din(i,j))   * inv_dy(i,j)
            tmp2 = (din(i,j)   - din(i,j-1)) * inv_dy(i,j-1)
            if (abs(tmp1) + abs(tmp2) < SMALL) then
               dout(i,j) = 0.0_SP
            else
               dout(i,j) = (tmp1*abs(tmp2) + abs(tmp1)*tmp2) / (abs(tmp1) + abs(tmp2))
            end if
         end do
         dout(i,1) = (din(i,2) - din(i,1))   * inv_dy(i,1)
         dout(i,n) = (din(i,n) - din(i,n-1)) * inv_dy(i,n)
      end do
   end subroutine dely_fun

   ! ----------------------------------------------------------------
   ! First-order van Leer reconstruction in x (Zhou et al. 2001).
   ! kappa removed (Choi 2016: 2nd/3rd order identical).
   ! ----------------------------------------------------------------
   pure subroutine construct_x(dx, vin, din, outl, outr)
      real(SP), intent(in)  :: dx(:,:), vin(:,:), din(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      integer :: i, j, m, n
      m = size(vin, 1);  n = size(vin, 2)
      do j = 1, n
         do i = 2, m
            outl(i,j) = vin(i-1,j) + 0.5_SP*dx(i-1,j)*din(i-1,j)
            outr(i,j) = vin(i,j)   - 0.5_SP*dx(i,j)  *din(i,j)
         end do
         outl(m+1,j) = vin(m,j) + 0.5_SP*dx(m,j)*din(m,j)
         outr(1,j)   = vin(1,j) - 0.5_SP*dx(1,j)*din(1,j)
         outl(1,j)   = outr(1,j)
         outr(m+1,j) = outl(m+1,j)
      end do
   end subroutine construct_x

   ! ----------------------------------------------------------------
   ! First-order van Leer reconstruction in y.
   ! ----------------------------------------------------------------
   pure subroutine construct_y(dy, vin, din, outl, outr)
      real(SP), intent(in)  :: dy(:,:), vin(:,:), din(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      integer :: i, j, m, n
      m = size(vin, 1);  n = size(vin, 2)
      do i = 1, m
         do j = 2, n
            outl(i,j) = vin(i,j-1) + 0.5_SP*dy(i,j-1)*din(i,j-1)
            outr(i,j) = vin(i,j)   - 0.5_SP*dy(i,j)  *din(i,j)
         end do
         outl(i,n+1) = vin(i,n) + 0.5_SP*dy(i,n)*din(i,n)
         outr(i,1)   = vin(i,1) - 0.5_SP*dy(i,1)*din(i,1)
         outl(i,1)   = outr(i,1)
         outr(i,n+1) = outl(i,n+1)
      end do
   end subroutine construct_y

   ! ----------------------------------------------------------------
   ! 4th-order MUSCL-TVD in x: van Leer (3rd) + minmod (4th) limiter.
   ! Erduran et al. (2005), default 'FOU' path.
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_x(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer,  intent(in)  :: mask(:,:)
      real(SP), intent(in)  :: vin(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      real(SP) :: din(lp%mloc, lp%nloc)
      real(SP) :: txp1, txp2, txp3, dvp1, dvp2, dvp3
      real(SP) :: van1, van2, rat, tmp1, tmp2
      integer  :: i, j
      din = 0.0_SP
      do j = lp%jb, lp%je
         do i = lp%ib - 1, lp%ie + 2
            txp1 = vin(i-1,j) - vin(i-2,j)
            txp2 = vin(i,j)   - vin(i-1,j)
            txp3 = vin(i+1,j) - vin(i,j)
            dvp1 = minmod3(txp1, 2.0_SP*txp2, 2.0_SP*txp3)
            dvp2 = minmod3(txp2, 2.0_SP*txp3, 2.0_SP*txp1)
            dvp3 = minmod3(txp3, 2.0_SP*txp1, 2.0_SP*txp2)
            if (mask(i-2,j) == 0 .or. mask(i+1,j) == 0) then
               txp2 = vin(i,j) - vin(i-1,j)
               txp1 = txp2;  txp3 = txp2
               dvp1 = minmod3(txp1, 2.0_SP*txp2, 2.0_SP*txp3)
               dvp2 = minmod3(txp2, 2.0_SP*txp3, 2.0_SP*txp1)
               dvp3 = minmod3(txp3, 2.0_SP*txp1, 2.0_SP*txp2)
            end if
            if (mask(i-1,j) == 0 .or. mask(i,j) == 0) then
               dvp1 = 0.0_SP;  dvp2 = 0.0_SP;  dvp3 = 0.0_SP
            end if
            din(i,j) = txp2 - (dvp3 - 2.0_SP*dvp2 + dvp1)/6.0_SP
         end do
         do i = lp%ib, lp%ie + 1
            tmp1 = din(i-1,j);  tmp2 = din(i,j)
            if (abs(tmp1) <= SMALL) tmp1 = SMALL*sign(1.0_SP, tmp1)
            if (abs(tmp2) <= SMALL) tmp2 = SMALL*sign(1.0_SP, tmp2)
            rat = tmp2/tmp1
            van1 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van1 = (rat + abs(rat))/(1.0_SP + rat)
            rat = tmp1/tmp2
            van2 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van2 = (rat + abs(rat))/(1.0_SP + rat)
            outl(i,j) = vin(i-1,j) + (van1*tmp1 + 2.0_SP*van2*tmp2)/6.0_SP
            tmp1 = din(i,j);  tmp2 = din(i+1,j)
            if (abs(tmp1) <= SMALL) tmp1 = SMALL*sign(1.0_SP, tmp1)
            if (abs(tmp2) <= SMALL) tmp2 = SMALL*sign(1.0_SP, tmp2)
            rat = tmp2/tmp1
            van1 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van1 = (rat + abs(rat))/(1.0_SP + rat)
            rat = tmp1/tmp2
            van2 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van2 = (rat + abs(rat))/(1.0_SP + rat)
            outr(i,j) = vin(i,j) - (2.0_SP*van1*tmp1 + van2*tmp2)/6.0_SP
         end do
      end do
   end subroutine construct_ho_x

   ! ----------------------------------------------------------------
   ! 4th-order MUSCL-TVD in y: van Leer (3rd) + minmod (4th).
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_y(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer,  intent(in)  :: mask(:,:)
      real(SP), intent(in)  :: vin(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      real(SP) :: din(lp%mloc, lp%nloc)
      real(SP) :: typ1, typ2, typ3, dvp1, dvp2, dvp3
      real(SP) :: van1, van2, rat, tmp1, tmp2
      integer  :: i, j
      din = 0.0_SP
      do i = lp%ib, lp%ie
         do j = lp%jb - 1, lp%je + 2
            typ1 = vin(i,j-1) - vin(i,j-2)
            typ2 = vin(i,j)   - vin(i,j-1)
            typ3 = vin(i,j+1) - vin(i,j)
            dvp1 = minmod3(typ1, 2.0_SP*typ2, 2.0_SP*typ3)
            dvp2 = minmod3(typ2, 2.0_SP*typ3, 2.0_SP*typ1)
            dvp3 = minmod3(typ3, 2.0_SP*typ1, 2.0_SP*typ2)
            if (mask(i,j-2) == 0 .or. mask(i,j+1) == 0) then
               typ2 = vin(i,j) - vin(i,j-1)
               typ1 = typ2;  typ3 = typ2
               dvp1 = minmod3(typ1, 2.0_SP*typ2, 2.0_SP*typ3)
               dvp2 = minmod3(typ2, 2.0_SP*typ3, 2.0_SP*typ1)
               dvp3 = minmod3(typ3, 2.0_SP*typ1, 2.0_SP*typ2)
            end if
            if (mask(i,j-1) == 0 .or. mask(i,j) == 0) then
               dvp1 = 0.0_SP;  dvp2 = 0.0_SP;  dvp3 = 0.0_SP
            end if
            din(i,j) = typ2 - (dvp3 - 2.0_SP*dvp2 + dvp1)/6.0_SP
         end do
         do j = lp%jb, lp%je + 1
            tmp1 = din(i,j-1);  tmp2 = din(i,j)
            if (abs(tmp1) <= SMALL) tmp1 = SMALL*sign(1.0_SP, tmp1)
            if (abs(tmp2) <= SMALL) tmp2 = SMALL*sign(1.0_SP, tmp2)
            rat = tmp2/tmp1
            van1 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van1 = (rat + abs(rat))/(1.0_SP + rat)
            rat = tmp1/tmp2
            van2 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van2 = (rat + abs(rat))/(1.0_SP + rat)
            outl(i,j) = vin(i,j-1) + (van1*tmp1 + 2.0_SP*van2*tmp2)/6.0_SP
            tmp1 = din(i,j);  tmp2 = din(i,j+1)
            if (abs(tmp1) <= SMALL) tmp1 = SMALL*sign(1.0_SP, tmp1)
            if (abs(tmp2) <= SMALL) tmp2 = SMALL*sign(1.0_SP, tmp2)
            rat = tmp2/tmp1
            van1 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van1 = (rat + abs(rat))/(1.0_SP + rat)
            rat = tmp1/tmp2
            van2 = 0.0_SP
            if (abs(1.0_SP + rat) > SMALL) van2 = (rat + abs(rat))/(1.0_SP + rat)
            outr(i,j) = vin(i,j) - (2.0_SP*van1*tmp1 + van2*tmp2)/6.0_SP
         end do
      end do
   end subroutine construct_ho_y

   ! ----------------------------------------------------------------
   ! 4th-order MUSCL-TVD in x: minmod-only variant ('FMI').
   ! DX argument removed (unused in original, Choi 2016).
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_x_minmod(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer,  intent(in)  :: mask(:,:)
      real(SP), intent(in)  :: vin(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      real(SP) :: din(lp%mloc, lp%nloc)
      real(SP) :: txp1, txp2, txp3, txp4, dvp1, dvp2, dvp3
      integer  :: i, j
      din = 0.0_SP
      do j = lp%jb, lp%je
         do i = lp%ib - 1, lp%ie + 2
            txp1 = vin(i-1,j) - vin(i-2,j)
            txp2 = vin(i,j)   - vin(i-1,j)
            txp3 = vin(i+1,j) - vin(i,j)
            dvp1 = minmod3(txp1, 2.0_SP*txp2, 2.0_SP*txp3)
            dvp2 = minmod3(txp2, 2.0_SP*txp3, 2.0_SP*txp1)
            dvp3 = minmod3(txp3, 2.0_SP*txp1, 2.0_SP*txp2)
            if (mask(i-2,j) == 0 .or. mask(i+1,j) == 0) then
               txp2 = vin(i,j) - vin(i-1,j)
               txp1 = txp2;  txp3 = txp2
               dvp1 = minmod3(txp1, 2.0_SP*txp2, 2.0_SP*txp3)
               dvp2 = minmod3(txp2, 2.0_SP*txp3, 2.0_SP*txp1)
               dvp3 = minmod3(txp3, 2.0_SP*txp1, 2.0_SP*txp2)
            end if
            if (mask(i-1,j) == 0 .or. mask(i,j) == 0) then
               dvp1 = 0.0_SP;  dvp2 = 0.0_SP;  dvp3 = 0.0_SP
            end if
            din(i,j) = txp2 - (dvp3 - 2.0_SP*dvp2 + dvp1)/6.0_SP
         end do
         do i = lp%ib, lp%ie + 1
            if (din(i-1,j) >= 0.0_SP) then
               txp1 = max(0.0_SP, min(din(i-1,j), 4.0_SP*din(i,j)))
            else
               txp1 = min(0.0_SP, max(din(i-1,j), 4.0_SP*din(i,j)))
            end if
            if (din(i,j) >= 0.0_SP) then
               txp2 = max(0.0_SP, min(din(i,j), 4.0_SP*din(i-1,j)))
            else
               txp2 = min(0.0_SP, max(din(i,j), 4.0_SP*din(i-1,j)))
            end if
            if (din(i,j) >= 0.0_SP) then
               txp4 = max(0.0_SP, min(din(i,j), 4.0_SP*din(i+1,j)))
            else
               txp4 = min(0.0_SP, max(din(i,j), 4.0_SP*din(i+1,j)))
            end if
            if (din(i+1,j) >= 0.0_SP) then
               txp3 = max(0.0_SP, min(din(i+1,j), 4.0_SP*din(i,j)))
            else
               txp3 = min(0.0_SP, max(din(i+1,j), 4.0_SP*din(i,j)))
            end if
            outl(i,j) = vin(i-1,j) + (txp1 + 2.0_SP*txp2)/6.0_SP
            outr(i,j) = vin(i,j)   - (txp3 + 2.0_SP*txp4)/6.0_SP
         end do
      end do
   end subroutine construct_ho_x_minmod

   ! ----------------------------------------------------------------
   ! 4th-order MUSCL-TVD in y: minmod-only.
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_y_minmod(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer,  intent(in)  :: mask(:,:)
      real(SP), intent(in)  :: vin(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      real(SP) :: din(lp%mloc, lp%nloc)
      real(SP) :: typ1, typ2, typ3, typ4, dvp1, dvp2, dvp3
      integer  :: i, j
      din = 0.0_SP
      do j = lp%jb - 1, lp%je + 2
         do i = lp%ib, lp%ie
            typ1 = vin(i,j-1) - vin(i,j-2)
            typ2 = vin(i,j)   - vin(i,j-1)
            typ3 = vin(i,j+1) - vin(i,j)
            dvp1 = minmod3(typ1, 2.0_SP*typ2, 2.0_SP*typ3)
            dvp2 = minmod3(typ2, 2.0_SP*typ3, 2.0_SP*typ1)
            dvp3 = minmod3(typ3, 2.0_SP*typ1, 2.0_SP*typ2)
            if (mask(i,j-2) == 0 .or. mask(i,j+1) == 0) then
               typ2 = vin(i,j) - vin(i,j-1)
               typ1 = typ2;  typ3 = typ2
               dvp1 = minmod3(typ1, 2.0_SP*typ2, 2.0_SP*typ3)
               dvp2 = minmod3(typ2, 2.0_SP*typ3, 2.0_SP*typ1)
               dvp3 = minmod3(typ3, 2.0_SP*typ1, 2.0_SP*typ2)
            end if
            if (mask(i,j-1) == 0 .or. mask(i,j) == 0) then
               dvp1 = 0.0_SP;  dvp2 = 0.0_SP;  dvp3 = 0.0_SP
            end if
            din(i,j) = typ2 - (dvp3 - 2.0_SP*dvp2 + dvp1)/6.0_SP
         end do
      end do
      do j = lp%jb, lp%je + 1
         do i = lp%ib, lp%ie
            if (din(i,j-1) >= 0.0_SP) then
               typ1 = max(0.0_SP, min(din(i,j-1), 4.0_SP*din(i,j)))
            else
               typ1 = min(0.0_SP, max(din(i,j-1), 4.0_SP*din(i,j)))
            end if
            if (din(i,j) >= 0.0_SP) then
               typ2 = max(0.0_SP, min(din(i,j), 4.0_SP*din(i,j-1)))
            else
               typ2 = min(0.0_SP, max(din(i,j), 4.0_SP*din(i,j-1)))
            end if
            if (din(i,j) >= 0.0_SP) then
               typ4 = max(0.0_SP, min(din(i,j), 4.0_SP*din(i,j+1)))
            else
               typ4 = min(0.0_SP, max(din(i,j), 4.0_SP*din(i,j+1)))
            end if
            if (din(i,j+1) >= 0.0_SP) then
               typ3 = max(0.0_SP, min(din(i,j+1), 4.0_SP*din(i,j)))
            else
               typ3 = min(0.0_SP, max(din(i,j+1), 4.0_SP*din(i,j)))
            end if
            outl(i,j) = vin(i,j-1) + (typ1 + 2.0_SP*typ2)/6.0_SP
            outr(i,j) = vin(i,j)   - (typ3 + 2.0_SP*typ4)/6.0_SP
         end do
      end do
   end subroutine construct_ho_y_minmod

   ! ----------------------------------------------------------------
   ! MLP reconstruction in x.
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_x_mlp(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer,  intent(in)  :: mask(:,:)
      real(SP), intent(in)  :: vin(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      real(SP), parameter   :: SV = 1.0e-10_SP
      real(SP) :: txp1, txp2, txp3
      real(SP) :: gaml, gamr, betal, betar
      real(SP) :: delsol, tanth1, tanth2, gamrth2, gamlth1
      real(SP) :: alphin, alph, slope
      integer  :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib - 1, lp%ie + 2
            txp1 = vin(i-1,j) - vin(i-2,j)
            txp2 = vin(i,j)   - vin(i-1,j)
            txp3 = vin(i+1,j) - vin(i,j)
            gaml = txp2/txp1;  if (abs(txp1) < SV) gaml = 0.0_SP
            betal = (1.0_SP + 2.0_SP*gaml)/3.0_SP
            gamr = txp2/txp3;  if (abs(txp3) < SV) gamr = 0.0_SP
            betar = (1.0_SP + 2.0_SP*gamr)/3.0_SP
            delsol = vin(i,j) - vin(i-2,j)
            tanth1 = abs((vin(i-1,j+1) - vin(i-1,j-1))/delsol)
            if (abs(delsol) < SV) tanth1 = 0.0_SP
            delsol = vin(i+1,j) - vin(i-1,j)
            tanth2 = abs((vin(i,j+1) - vin(i,j-1))/delsol)
            if (abs(delsol) < SV) tanth2 = 0.0_SP
            gamrth2 = tanth2/gamr;  if (abs(gamr) < SV) gamrth2 = 0.0_SP
            alphin = 2.0_SP*max(1.0_SP,gaml)*(1.0_SP+max(0.0_SP,gamrth2))/(1.0_SP+tanth1)
            alph = max(1.0_SP, min(2.0_SP, alphin))
            slope = max(0.0_SP, min(alph*gaml, min(alph, betal)))
            outl(i,j) = vin(i-1,j) + 0.5_SP*slope*txp1
            gamlth1 = tanth1/gaml;  if (abs(gaml) < SV) gamlth1 = 0.0_SP
            alphin = 2.0_SP*max(1.0_SP,gamr)*(1.0_SP+max(0.0_SP,gamlth1))/(1.0_SP+tanth2)
            alph = max(1.0_SP, min(2.0_SP, alphin))
            slope = max(0.0_SP, min(alph*gamr, min(alph, betar)))
            outr(i,j) = vin(i,j) - 0.5_SP*slope*txp3
         end do
      end do
   end subroutine construct_ho_x_mlp

   ! ----------------------------------------------------------------
   ! MLP reconstruction in y.
   ! ----------------------------------------------------------------
   pure subroutine construct_ho_y_mlp(lp, mask, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      integer,  intent(in)  :: mask(:,:)
      real(SP), intent(in)  :: vin(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      real(SP), parameter   :: SV = 1.0e-10_SP
      real(SP) :: typ1, typ2, typ3
      real(SP) :: gaml, gamr, betal, betar
      real(SP) :: delsol, tanth1, tanth2, gamrth2, gamlth1
      real(SP) :: alphin, alph, slope
      integer  :: i, j
      do i = lp%ib, lp%ie
         do j = lp%jb - 1, lp%je + 2
            typ1 = vin(i,j-1) - vin(i,j-2)
            typ2 = vin(i,j)   - vin(i,j-1)
            typ3 = vin(i,j+1) - vin(i,j)
            gaml = typ2/typ1;  if (abs(typ1) < SV) gaml = 0.0_SP
            betal = (1.0_SP + 2.0_SP*gaml)/3.0_SP
            gamr = typ2/typ3;  if (abs(typ3) < SV) gamr = 0.0_SP
            betar = (1.0_SP + 2.0_SP*gamr)/3.0_SP
            delsol = vin(i,j) - vin(i,j-2)
            tanth1 = abs((vin(i+1,j-1) - vin(i-1,j-1))/delsol)
            if (abs(delsol) < SV) tanth1 = 0.0_SP
            delsol = vin(i,j+1) - vin(i,j-1)
            tanth2 = abs((vin(i+1,j) - vin(i-1,j))/delsol)
            if (abs(delsol) < SV) tanth2 = 0.0_SP
            gamrth2 = tanth2/gamr;  if (abs(gamr) < SV) gamrth2 = 0.0_SP
            alphin = 2.0_SP*max(1.0_SP,gaml)*(1.0_SP+max(0.0_SP,gamrth2))/(1.0_SP+tanth1)
            alph = max(1.0_SP, min(2.0_SP, alphin))
            slope = max(0.0_SP, min(alph*gaml, min(alph, betal)))
            outl(i,j) = vin(i,j-1) + 0.5_SP*slope*typ1
            gamlth1 = tanth1/gaml;  if (abs(gaml) < SV) gamlth1 = 0.0_SP
            alphin = 2.0_SP*max(1.0_SP,gamr)*(1.0_SP+max(0.0_SP,gamlth1))/(1.0_SP+tanth2)
            alph = max(1.0_SP, min(2.0_SP, alphin))
            slope = max(0.0_SP, min(alph*gamr, min(alph, betar)))
            outr(i,j) = vin(i,j) - 0.5_SP*slope*typ3
         end do
      end do
   end subroutine construct_ho_y_mlp

   ! ----------------------------------------------------------------
   ! 5th-order WENO reconstruction in x (Qiu & Shu 2005).
   ! Constant dx assumed (Cartesian only).
   ! ----------------------------------------------------------------
   pure subroutine weno_construct_x(lp, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: vin(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      real(SP), parameter :: WNEPS = 1.0e-6_SP
      real(SP), parameter :: R0R = 0.3_SP, R1R = 0.6_SP, R2R = 0.1_SP
      real(SP), parameter :: R0L = 0.1_SP, R1L = 0.6_SP, R2L = 0.3_SP
      real(SP), parameter :: BC1 = 13.0_SP/12.0_SP, BC2 = 0.25_SP
      real(SP) :: wb0, wb1, wb2, tx1, tx2, tx3, wnw0, wnw1, wnw2
      real(SP) :: wp0, wp1, wp2
      integer  :: i, j
      do j = lp%jb, lp%je
         do i = lp%ib, lp%ie + 1
            tx1 = vin(i-2,j) - 2.0_SP*vin(i-1,j) + vin(i,j)
            tx2 = vin(i-2,j) - 4.0_SP*vin(i-1,j) + 3.0_SP*vin(i,j)
            wb0 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx1 = vin(i-1,j) - 2.0_SP*vin(i,j)   + vin(i+1,j)
            tx2 = vin(i-1,j) - vin(i+1,j)
            wb1 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx1 = vin(i,j)   - 2.0_SP*vin(i+1,j) + vin(i+2,j)
            tx2 = 3.0_SP*vin(i,j) - 4.0_SP*vin(i+1,j) + vin(i+2,j)
            wb2 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx3 = R0R/(WNEPS+wb0)**2 + R1R/(WNEPS+wb1)**2 + R2R/(WNEPS+wb2)**2
            wnw0 = R0R/((WNEPS+wb0)**2*tx3)
            wnw1 = R1R/((WNEPS+wb1)**2*tx3)
            wnw2 = R2R/((WNEPS+wb2)**2*tx3)
            wp0 = -(1.0_SP/6.0_SP)*vin(i-2,j) + (5.0_SP/6.0_SP)*vin(i-1,j) + (1.0_SP/3.0_SP)*vin(i,j)
            wp1 =  (1.0_SP/3.0_SP)*vin(i-1,j) + (5.0_SP/6.0_SP)*vin(i,j)   - (1.0_SP/6.0_SP)*vin(i+1,j)
            wp2 = (11.0_SP/6.0_SP)*vin(i,j)   - (7.0_SP/6.0_SP)*vin(i+1,j) + (1.0_SP/3.0_SP)*vin(i+2,j)
            outr(i,j) = wnw0*wp0 + wnw1*wp1 + wnw2*wp2
            tx1 = vin(i-3,j) - 2.0_SP*vin(i-2,j) + vin(i-1,j)
            tx2 = vin(i-3,j) - 4.0_SP*vin(i-2,j) + 3.0_SP*vin(i-1,j)
            wb0 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx1 = vin(i-2,j) - 2.0_SP*vin(i-1,j) + vin(i,j)
            tx2 = vin(i-2,j) - vin(i,j)
            wb1 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx1 = vin(i-1,j) - 2.0_SP*vin(i,j)   + vin(i+1,j)
            tx2 = 3.0_SP*vin(i-1,j) - 4.0_SP*vin(i,j) + vin(i+1,j)
            wb2 = BC1*tx1*tx1 + BC2*tx2*tx2
            tx3 = R0L/(WNEPS+wb0)**2 + R1L/(WNEPS+wb1)**2 + R2L/(WNEPS+wb2)**2
            wnw0 = R0L/((WNEPS+wb0)**2*tx3)
            wnw1 = R1L/((WNEPS+wb1)**2*tx3)
            wnw2 = R2L/((WNEPS+wb2)**2*tx3)
            wp0 =  (1.0_SP/3.0_SP)*vin(i-3,j) - (7.0_SP/6.0_SP)*vin(i-2,j) + (11.0_SP/6.0_SP)*vin(i-1,j)
            wp1 = -(1.0_SP/6.0_SP)*vin(i-2,j) + (5.0_SP/6.0_SP)*vin(i-1,j) +  (1.0_SP/3.0_SP)*vin(i,j)
            wp2 =  (1.0_SP/3.0_SP)*vin(i-1,j) + (5.0_SP/6.0_SP)*vin(i,j)   -  (1.0_SP/6.0_SP)*vin(i+1,j)
            outl(i,j) = wnw0*wp0 + wnw1*wp1 + wnw2*wp2
         end do
      end do
   end subroutine weno_construct_x

   ! ----------------------------------------------------------------
   ! 5th-order WENO reconstruction in y.
   ! ----------------------------------------------------------------
   pure subroutine weno_construct_y(lp, vin, outl, outr)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: vin(:,:)
      real(SP), intent(out) :: outl(:,:), outr(:,:)
      real(SP), parameter :: WNEPS = 1.0e-6_SP
      real(SP), parameter :: R0R = 0.3_SP, R1R = 0.6_SP, R2R = 0.1_SP
      real(SP), parameter :: R0L = 0.1_SP, R1L = 0.6_SP, R2L = 0.3_SP
      real(SP), parameter :: BC1 = 13.0_SP/12.0_SP, BC2 = 0.25_SP
      real(SP) :: wb0, wb1, wb2, ty1, ty2, ty3, wnw0, wnw1, wnw2
      real(SP) :: wp0, wp1, wp2
      integer  :: i, j
      do j = lp%jb, lp%je + 1
         do i = lp%ib, lp%ie
            ty1 = vin(i,j-2) - 2.0_SP*vin(i,j-1) + vin(i,j)
            ty2 = vin(i,j-2) - 4.0_SP*vin(i,j-1) + 3.0_SP*vin(i,j)
            wb0 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty1 = vin(i,j-1) - 2.0_SP*vin(i,j)   + vin(i,j+1)
            ty2 = vin(i,j-1) - vin(i,j+1)
            wb1 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty1 = vin(i,j)   - 2.0_SP*vin(i,j+1) + vin(i,j+2)
            ty2 = 3.0_SP*vin(i,j) - 4.0_SP*vin(i,j+1) + vin(i,j+2)
            wb2 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty3 = R0R/(WNEPS+wb0)**2 + R1R/(WNEPS+wb1)**2 + R2R/(WNEPS+wb2)**2
            wnw0 = R0R/((WNEPS+wb0)**2*ty3)
            wnw1 = R1R/((WNEPS+wb1)**2*ty3)
            wnw2 = R2R/((WNEPS+wb2)**2*ty3)
            wp0 = -(1.0_SP/6.0_SP)*vin(i,j-2) + (5.0_SP/6.0_SP)*vin(i,j-1) + (1.0_SP/3.0_SP)*vin(i,j)
            wp1 =  (1.0_SP/3.0_SP)*vin(i,j-1) + (5.0_SP/6.0_SP)*vin(i,j)   - (1.0_SP/6.0_SP)*vin(i,j+1)
            wp2 = (11.0_SP/6.0_SP)*vin(i,j)   - (7.0_SP/6.0_SP)*vin(i,j+1) + (1.0_SP/3.0_SP)*vin(i,j+2)
            outr(i,j) = wnw0*wp0 + wnw1*wp1 + wnw2*wp2
            ty1 = vin(i,j-3) - 2.0_SP*vin(i,j-2) + vin(i,j-1)
            ty2 = vin(i,j-3) - 4.0_SP*vin(i,j-2) + 3.0_SP*vin(i,j-1)
            wb0 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty1 = vin(i,j-2) - 2.0_SP*vin(i,j-1) + vin(i,j)
            ty2 = vin(i,j-2) - vin(i,j)
            wb1 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty1 = vin(i,j-1) - 2.0_SP*vin(i,j)   + vin(i,j+1)
            ty2 = 3.0_SP*vin(i,j-1) - 4.0_SP*vin(i,j) + vin(i,j+1)
            wb2 = BC1*ty1*ty1 + BC2*ty2*ty2
            ty3 = R0L/(WNEPS+wb0)**2 + R1L/(WNEPS+wb1)**2 + R2L/(WNEPS+wb2)**2
            wnw0 = R0L/((WNEPS+wb0)**2*ty3)
            wnw1 = R1L/((WNEPS+wb1)**2*ty3)
            wnw2 = R2L/((WNEPS+wb2)**2*ty3)
            wp0 =  (1.0_SP/3.0_SP)*vin(i,j-3) - (7.0_SP/6.0_SP)*vin(i,j-2) + (11.0_SP/6.0_SP)*vin(i,j-1)
            wp1 = -(1.0_SP/6.0_SP)*vin(i,j-2) + (5.0_SP/6.0_SP)*vin(i,j-1) +  (1.0_SP/3.0_SP)*vin(i,j)
            wp2 =  (1.0_SP/3.0_SP)*vin(i,j-1) + (5.0_SP/6.0_SP)*vin(i,j)   -  (1.0_SP/6.0_SP)*vin(i,j+1)
            outl(i,j) = wnw0*wp0 + wnw1*wp1 + wnw2*wp2
         end do
      end do
   end subroutine weno_construct_y

   ! ----------------------------------------------------------------
   ! Roe wave speeds (Zhou et al. 2001).
   ! ----------------------------------------------------------------
   pure subroutine wave_speed(lp, uxl, uxr, vyl, vyr, hxl, hxr, hyl, hyr, &
                               sxl, sxr, syl, syr)
      type(type_loop_bounds), intent(in) :: lp
      real(SP), intent(in)  :: uxl(:,:), uxr(:,:), hxl(:,:), hxr(:,:)
      real(SP), intent(in)  :: vyl(:,:), vyr(:,:), hyl(:,:), hyr(:,:)
      real(SP), intent(out) :: sxl(:,:), sxr(:,:)
      real(SP), intent(out) :: syl(:,:), syr(:,:)
      integer  :: i, j, m, n, m1, n1
      real(SP) :: spl, spr, sps, us
      m = lp%mloc;  n = lp%nloc;  m1 = m + 1;  n1 = n + 1
      do j = 1+N_GHOST, n-N_GHOST
         do i = 1+N_GHOST, m1-N_GHOST
            spl = sqrt(GRAV*abs(hxl(i,j)));  spr = sqrt(GRAV*abs(hxr(i,j)))
            sps = 0.5_SP*(spl+spr) + 0.25_SP*(uxl(i,j)-uxr(i,j))
            us  = 0.5_SP*(uxl(i,j)+uxr(i,j)) + spl - spr
            sxl(i,j) = min(uxl(i,j)-spl, us-sps)
            sxr(i,j) = max(uxr(i,j)+spr, us+sps)
         end do
      end do
      do j = 1+N_GHOST, n-N_GHOST
         do i = 1, N_GHOST
            sxl(i,j) = sxl(N_GHOST+1,j);  sxr(i,j) = sxr(N_GHOST+1,j)
         end do
         do i = m1-N_GHOST+1, m1
            sxl(i,j) = sxl(m1-N_GHOST,j);  sxr(i,j) = sxr(m1-N_GHOST,j)
         end do
      end do
      do i = 1, m1
         do j = 1, N_GHOST
            sxl(i,j) = sxl(i,N_GHOST+1);  sxr(i,j) = sxr(i,N_GHOST+1)
         end do
         do j = n-N_GHOST+1, n
            sxl(i,j) = sxl(i,n-N_GHOST);  sxr(i,j) = sxr(i,n-N_GHOST)
         end do
      end do
      do j = 1+N_GHOST, n1-N_GHOST
         do i = 1+N_GHOST, m-N_GHOST
            spl = sqrt(GRAV*abs(hyl(i,j)));  spr = sqrt(GRAV*abs(hyr(i,j)))
            sps = 0.5_SP*(spl+spr) + 0.25_SP*(vyl(i,j)-vyr(i,j))
            us  = 0.5_SP*(vyl(i,j)+vyr(i,j)) + spl - spr
            syl(i,j) = min(vyl(i,j)-spl, us-sps)
            syr(i,j) = max(vyr(i,j)+spr, us+sps)
         end do
      end do
      do i = 1+N_GHOST, m-N_GHOST
         do j = 1, N_GHOST
            syl(i,j) = syl(i,N_GHOST+1);  syr(i,j) = syr(i,N_GHOST+1)
         end do
         do j = n1-N_GHOST+1, n1
            syl(i,j) = syl(i,n1-N_GHOST);  syr(i,j) = syr(i,n1-N_GHOST)
         end do
      end do
      do j = 1, n1
         do i = 1, N_GHOST
            syl(i,j) = syl(N_GHOST+1,j);  syr(i,j) = syr(N_GHOST+1,j)
         end do
         do i = m-N_GHOST+1, m
            syl(i,j) = syl(m-N_GHOST,j);  syr(i,j) = syr(m-N_GHOST,j)
         end do
      end do
   end subroutine wave_speed

   ! ----------------------------------------------------------------
   ! HLL flux kernel.
   ! ----------------------------------------------------------------
   pure subroutine hll(sl, sr, fl, fr, ul, ur, fout)
      real(SP), intent(in)  :: sl(:,:), sr(:,:)
      real(SP), intent(in)  :: fl(:,:), fr(:,:), ul(:,:), ur(:,:)
      real(SP), intent(out) :: fout(:,:)
      real(SP) :: denom
      integer  :: i, j
      do j = 1, size(fout,2)
         do i = 1, size(fout,1)
            if (sl(i,j) >= 0.0_SP) then
               fout(i,j) = fl(i,j)
            else if (sr(i,j) <= 0.0_SP) then
               fout(i,j) = fr(i,j)
            else
               denom = sr(i,j) - sl(i,j)
               if (abs(denom) < SMALL) denom = SMALL
               fout(i,j) = (sr(i,j)*fl(i,j) - sl(i,j)*fr(i,j) &
                            + sl(i,j)*sr(i,j)*(ur(i,j) - ul(i,j))) / denom
            end if
         end do
      end do
   end subroutine hll

   ! ----------------------------------------------------------------
   ! Average-based flux (predictor / averaging approach).
   ! ----------------------------------------------------------------
   subroutine flux_at_interface(ws)
      type(type_flux_workspace), intent(inout) :: ws
      ws%p  = 0.5_SP*(ws%pr  + ws%pl)
      ws%fx = 0.5_SP*(ws%fxr + ws%fxl)
      ws%gx = 0.5_SP*(ws%gxr + ws%gxl)
      ws%q  = 0.5_SP*(ws%qr  + ws%ql)
      ws%fy = 0.5_SP*(ws%fyr + ws%fyl)
      ws%gy = 0.5_SP*(ws%gyr + ws%gyl)
   end subroutine flux_at_interface

   ! ----------------------------------------------------------------
   ! HLL-based flux.
   ! ----------------------------------------------------------------
   subroutine flux_at_interface_hll(ws)
      type(type_flux_workspace), intent(inout) :: ws
      call hll(ws%sxl, ws%sxr, ws%pl,   ws%pr,   ws%etarxl, ws%etarxr, ws%p)
      call hll(ws%syl, ws%syr, ws%ql,   ws%qr,   ws%etaryl, ws%etaryr, ws%q)
      call hll(ws%sxl, ws%sxr, ws%fxl,  ws%fxr,  ws%huxl,   ws%huxr,   ws%fx)
      call hll(ws%syl, ws%syr, ws%fyl,  ws%fyr,  ws%huyl,   ws%huyr,   ws%fy)
      call hll(ws%sxl, ws%sxr, ws%gxl,  ws%gxr,  ws%hvxl,   ws%hvxr,   ws%gx)
      call hll(ws%syl, ws%syr, ws%gyl,  ws%gyr,  ws%hvyl,   ws%hvyr,   ws%gy)
   end subroutine flux_at_interface_hll

   ! ----------------------------------------------------------------
   ! Private helper: assemble P/Fx/Gx from x-interface arrays.
   ! ----------------------------------------------------------------
   subroutine assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_flux_workspace), intent(inout) :: ws
      real(SP), intent(in) :: depthx(:,:), gamma1, gamma3
      integer,  intent(in) :: mask9(:,:)
      logical,  intent(in) :: dispersion
      integer  :: i, j, ii
      real(SP) :: u4l, u4r, v4l, v4r
      ws%hxl = ws%etarxl + depthx
      ws%hxr = ws%etarxr + depthx
      if (dispersion) then
         do j = 1, ws%n
            do i = 1, ws%m + 1
               ii = min(i, ws%m)
               u4l = gamma1 * mask9(ii,j) * ws%u4xl(i,j)
               u4r = gamma1 * mask9(ii,j) * ws%u4xr(i,j)
               v4l = gamma1 * mask9(ii,j) * ws%v4xl(i,j)
               v4r = gamma1 * mask9(ii,j) * ws%v4xr(i,j)
               ws%pl(i,j) = ws%huxl(i,j) + ws%hxl(i,j)*u4l
               ws%pr(i,j) = ws%huxr(i,j) + ws%hxr(i,j)*u4r
               ws%fxl(i,j) = gamma3*ws%pl(i,j)*(ws%uxl(i,j)+u4l) &
                    + 0.5_SP*GRAV*(gamma3*ws%etarxl(i,j)**2 + 2.0_SP*ws%etarxl(i,j)*depthx(i,j))
               ws%fxr(i,j) = gamma3*ws%pr(i,j)*(ws%uxr(i,j)+u4r) &
                    + 0.5_SP*GRAV*(gamma3*ws%etarxr(i,j)**2 + 2.0_SP*ws%etarxr(i,j)*depthx(i,j))
               ws%gxl(i,j) = gamma3*ws%hxl(i,j)*(ws%uxl(i,j)+u4l)*(ws%vxl(i,j)+v4l)
               ws%gxr(i,j) = gamma3*ws%hxr(i,j)*(ws%uxr(i,j)+u4r)*(ws%vxr(i,j)+v4r)
            end do
         end do
      else
         ws%pl  = ws%huxl
         ws%pr  = ws%huxr
         ws%fxl = gamma3*ws%pl*ws%uxl &
                  + 0.5_SP*GRAV*(gamma3*ws%etarxl**2 + 2.0_SP*ws%etarxl*depthx)
         ws%fxr = gamma3*ws%pr*ws%uxr &
                  + 0.5_SP*GRAV*(gamma3*ws%etarxr**2 + 2.0_SP*ws%etarxr*depthx)
         ws%gxl = gamma3*ws%hxl*ws%uxl*ws%vxl
         ws%gxr = gamma3*ws%hxr*ws%uxr*ws%vxr
      end if
   end subroutine assemble_x

   ! ----------------------------------------------------------------
   ! Private helper: assemble Q/Fy/Gy from y-interface arrays.
   ! ----------------------------------------------------------------
   subroutine assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_flux_workspace), intent(inout) :: ws
      real(SP), intent(in) :: depthy(:,:), gamma1, gamma3
      integer,  intent(in) :: mask9(:,:)
      logical,  intent(in) :: dispersion
      integer  :: i, j, jj
      real(SP) :: u4l, u4r, v4l, v4r
      ws%hyl = ws%etaryl + depthy
      ws%hyr = ws%etaryr + depthy
      if (dispersion) then
         do j = 1, ws%n + 1
            jj = min(j, ws%n)
            do i = 1, ws%m
               v4l = gamma1 * mask9(i,jj) * ws%v4yl(i,j)
               v4r = gamma1 * mask9(i,jj) * ws%v4yr(i,j)
               u4l = gamma1 * mask9(i,jj) * ws%u4yl(i,j)
               u4r = gamma1 * mask9(i,jj) * ws%u4yr(i,j)
               ws%ql(i,j) = ws%hvyl(i,j) + ws%hyl(i,j)*v4l
               ws%qr(i,j) = ws%hvyr(i,j) + ws%hyr(i,j)*v4r
               ws%gyl(i,j) = gamma3*ws%ql(i,j)*(ws%vyl(i,j)+v4l) &
                    + 0.5_SP*GRAV*(gamma3*ws%etaryl(i,j)**2 + 2.0_SP*ws%etaryl(i,j)*depthy(i,j))
               ws%gyr(i,j) = gamma3*ws%qr(i,j)*(ws%vyr(i,j)+v4r) &
                    + 0.5_SP*GRAV*(gamma3*ws%etaryr(i,j)**2 + 2.0_SP*ws%etaryr(i,j)*depthy(i,j))
               ws%fyl(i,j) = gamma3*ws%hyl(i,j)*(ws%uyl(i,j)+u4l)*(ws%vyl(i,j)+v4l)
               ws%fyr(i,j) = gamma3*ws%hyr(i,j)*(ws%uyr(i,j)+u4r)*(ws%vyr(i,j)+v4r)
            end do
         end do
      else
         ws%ql  = ws%hvyl
         ws%qr  = ws%hvyr
         ws%gyl = gamma3*ws%ql*ws%vyl &
                  + 0.5_SP*GRAV*(gamma3*ws%etaryl**2 + 2.0_SP*ws%etaryl*depthy)
         ws%gyr = gamma3*ws%qr*ws%vyr &
                  + 0.5_SP*GRAV*(gamma3*ws%etaryr**2 + 2.0_SP*ws%etaryr*depthy)
         ws%fyl = gamma3*ws%hyl*ws%uyl*ws%vyl
         ws%fyr = gamma3*ws%hyr*ws%uyr*ws%vyr
      end if
   end subroutine assemble_y

   ! ----------------------------------------------------------------
   ! Basic (1st-order) CONSTRUCTION: van Leer slopes + construct_x/y.
   ! ----------------------------------------------------------------
   subroutine construction(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                            dx, dy, inv_dx, inv_dy, mask, mask9, &
                            gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)  :: eta(:,:), u(:,:), v(:,:), hu(:,:), hv(:,:)
      real(SP), intent(in)  :: u4(:,:), v4(:,:)
      real(SP), intent(in)  :: depthx(:,:), depthy(:,:)
      real(SP), intent(in)  :: dx(:,:), dy(:,:), inv_dx(:,:), inv_dy(:,:)
      integer,  intent(in)  :: mask(:,:), mask9(:,:)
      real(SP), intent(in)  :: gamma1, gamma3
      logical,  intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      call delx_fun(inv_dx, eta, ws%sl);  call construct_x(dx, eta, ws%sl, ws%etarxl, ws%etarxr)
      call delx_fun(inv_dx, u,   ws%sl);  call construct_x(dx, u,   ws%sl, ws%uxl,    ws%uxr)
      call delx_fun(inv_dx, v,   ws%sl);  call construct_x(dx, v,   ws%sl, ws%vxl,    ws%vxr)
      call delx_fun(inv_dx, hu,  ws%sl);  call construct_x(dx, hu,  ws%sl, ws%huxl,   ws%huxr)
      call delx_fun(inv_dx, hv,  ws%sl);  call construct_x(dx, hv,  ws%sl, ws%hvxl,   ws%hvxr)
      if (dispersion) then
         call delx_fun(inv_dx, u4, ws%sl); call construct_x(dx, u4, ws%sl, ws%u4xl, ws%u4xr)
         call delx_fun(inv_dx, v4, ws%sl); call construct_x(dx, v4, ws%sl, ws%v4xl, ws%v4xr)
      end if
      call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion)
      call dely_fun(inv_dy, eta, ws%sl);  call construct_y(dy, eta, ws%sl, ws%etaryl, ws%etaryr)
      call dely_fun(inv_dy, u,   ws%sl);  call construct_y(dy, u,   ws%sl, ws%uyl,    ws%uyr)
      call dely_fun(inv_dy, v,   ws%sl);  call construct_y(dy, v,   ws%sl, ws%vyl,    ws%vyr)
      call dely_fun(inv_dy, hv,  ws%sl);  call construct_y(dy, hv,  ws%sl, ws%hvyl,   ws%hvyr)
      call dely_fun(inv_dy, hu,  ws%sl);  call construct_y(dy, hu,  ws%sl, ws%huyl,   ws%huyr)
      if (dispersion) then
         call dely_fun(inv_dy, v4, ws%sl); call construct_y(dy, v4, ws%sl, ws%v4yl, ws%v4yr)
         call dely_fun(inv_dy, u4, ws%sl); call construct_y(dy, u4, ws%sl, ws%u4yl, ws%u4yr)
      end if
      call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion)
   end subroutine construction

   ! ----------------------------------------------------------------
   ! High-order CONSTRUCTION ('FOU'): 4th-order van Leer+minmod.
   ! ----------------------------------------------------------------
   subroutine construction_ho(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                               mask, mask9, gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)  :: eta(:,:), u(:,:), v(:,:), hu(:,:), hv(:,:)
      real(SP), intent(in)  :: u4(:,:), v4(:,:)
      real(SP), intent(in)  :: depthx(:,:), depthy(:,:)
      integer,  intent(in)  :: mask(:,:), mask9(:,:)
      real(SP), intent(in)  :: gamma1, gamma3
      logical,  intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      call construct_ho_x(lp, mask, eta, ws%etarxl, ws%etarxr)
      call construct_ho_x(lp, mask, u,   ws%uxl,    ws%uxr)
      call construct_ho_x(lp, mask, v,   ws%vxl,    ws%vxr)
      call construct_ho_x(lp, mask, hu,  ws%huxl,   ws%huxr)
      call construct_ho_x(lp, mask, hv,  ws%hvxl,   ws%hvxr)
      if (dispersion) then
         call construct_ho_x(lp, mask, u4, ws%u4xl, ws%u4xr)
         call construct_ho_x(lp, mask, v4, ws%v4xl, ws%v4xr)
      end if
      call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion)
      call construct_ho_y(lp, mask, eta, ws%etaryl, ws%etaryr)
      call construct_ho_y(lp, mask, u,   ws%uyl,    ws%uyr)
      call construct_ho_y(lp, mask, v,   ws%vyl,    ws%vyr)
      call construct_ho_y(lp, mask, hv,  ws%hvyl,   ws%hvyr)
      call construct_ho_y(lp, mask, hu,  ws%huyl,   ws%huyr)
      if (dispersion) then
         call construct_ho_y(lp, mask, v4, ws%v4yl, ws%v4yr)
         call construct_ho_y(lp, mask, u4, ws%u4yl, ws%u4yr)
      end if
      call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion)
   end subroutine construction_ho

   ! ----------------------------------------------------------------
   ! 'FMI': 4th-order minmod-only.
   ! ----------------------------------------------------------------
   subroutine construction_ho_minmod(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                      mask, mask9, gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)  :: eta(:,:), u(:,:), v(:,:), hu(:,:), hv(:,:)
      real(SP), intent(in)  :: u4(:,:), v4(:,:)
      real(SP), intent(in)  :: depthx(:,:), depthy(:,:)
      integer,  intent(in)  :: mask(:,:), mask9(:,:)
      real(SP), intent(in)  :: gamma1, gamma3
      logical,  intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      call construct_ho_x_minmod(lp, mask, eta, ws%etarxl, ws%etarxr)
      call construct_ho_x_minmod(lp, mask, u,   ws%uxl,    ws%uxr)
      call construct_ho_x_minmod(lp, mask, v,   ws%vxl,    ws%vxr)
      call construct_ho_x_minmod(lp, mask, hu,  ws%huxl,   ws%huxr)
      call construct_ho_x_minmod(lp, mask, hv,  ws%hvxl,   ws%hvxr)
      if (dispersion) then
         call construct_ho_x_minmod(lp, mask, u4, ws%u4xl, ws%u4xr)
         call construct_ho_x_minmod(lp, mask, v4, ws%v4xl, ws%v4xr)
      end if
      call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion)
      call construct_ho_y_minmod(lp, mask, eta, ws%etaryl, ws%etaryr)
      call construct_ho_y_minmod(lp, mask, u,   ws%uyl,    ws%uyr)
      call construct_ho_y_minmod(lp, mask, v,   ws%vyl,    ws%vyr)
      call construct_ho_y_minmod(lp, mask, hv,  ws%hvyl,   ws%hvyr)
      call construct_ho_y_minmod(lp, mask, hu,  ws%huyl,   ws%huyr)
      if (dispersion) then
         call construct_ho_y_minmod(lp, mask, v4, ws%v4yl, ws%v4yr)
         call construct_ho_y_minmod(lp, mask, u4, ws%u4yl, ws%u4yr)
      end if
      call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion)
   end subroutine construction_ho_minmod

   ! ----------------------------------------------------------------
   ! 'MLP': MLP reconstruction.
   ! ----------------------------------------------------------------
   subroutine construction_ho_mlp(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                   mask, mask9, gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)  :: eta(:,:), u(:,:), v(:,:), hu(:,:), hv(:,:)
      real(SP), intent(in)  :: u4(:,:), v4(:,:)
      real(SP), intent(in)  :: depthx(:,:), depthy(:,:)
      integer,  intent(in)  :: mask(:,:), mask9(:,:)
      real(SP), intent(in)  :: gamma1, gamma3
      logical,  intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      call construct_ho_x_mlp(lp, mask, eta, ws%etarxl, ws%etarxr)
      call construct_ho_x_mlp(lp, mask, u,   ws%uxl,    ws%uxr)
      call construct_ho_x_mlp(lp, mask, v,   ws%vxl,    ws%vxr)
      call construct_ho_x_mlp(lp, mask, hu,  ws%huxl,   ws%huxr)
      call construct_ho_x_mlp(lp, mask, hv,  ws%hvxl,   ws%hvxr)
      if (dispersion) then
         call construct_ho_x_mlp(lp, mask, u4, ws%u4xl, ws%u4xr)
         call construct_ho_x_mlp(lp, mask, v4, ws%v4xl, ws%v4xr)
      end if
      call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion)
      call construct_ho_y_mlp(lp, mask, eta, ws%etaryl, ws%etaryr)
      call construct_ho_y_mlp(lp, mask, u,   ws%uyl,    ws%uyr)
      call construct_ho_y_mlp(lp, mask, v,   ws%vyl,    ws%vyr)
      call construct_ho_y_mlp(lp, mask, hv,  ws%hvyl,   ws%hvyr)
      call construct_ho_y_mlp(lp, mask, hu,  ws%huyl,   ws%huyr)
      if (dispersion) then
         call construct_ho_y_mlp(lp, mask, v4, ws%v4yl, ws%v4yr)
         call construct_ho_y_mlp(lp, mask, u4, ws%u4yl, ws%u4yr)
      end if
      call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion)
   end subroutine construction_ho_mlp

   ! ----------------------------------------------------------------
   ! 'WEN': WENO5 reconstruction (Cartesian, constant dx).
   ! ----------------------------------------------------------------
   subroutine construction_weno(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                 mask9, gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)  :: eta(:,:), u(:,:), v(:,:), hu(:,:), hv(:,:)
      real(SP), intent(in)  :: u4(:,:), v4(:,:)
      real(SP), intent(in)  :: depthx(:,:), depthy(:,:)
      integer,  intent(in)  :: mask9(:,:)
      real(SP), intent(in)  :: gamma1, gamma3
      logical,  intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws
      call weno_construct_x(lp, eta, ws%etarxl, ws%etarxr)
      call weno_construct_x(lp, u,   ws%uxl,    ws%uxr)
      call weno_construct_x(lp, v,   ws%vxl,    ws%vxr)
      call weno_construct_x(lp, hu,  ws%huxl,   ws%huxr)
      call weno_construct_x(lp, hv,  ws%hvxl,   ws%hvxr)
      if (dispersion) then
         call weno_construct_x(lp, u4, ws%u4xl, ws%u4xr)
         call weno_construct_x(lp, v4, ws%v4xl, ws%v4xr)
      end if
      call assemble_x(lp, ws, depthx, mask9, gamma1, gamma3, dispersion)
      call weno_construct_y(lp, eta, ws%etaryl, ws%etaryr)
      call weno_construct_y(lp, u,   ws%uyl,    ws%uyr)
      call weno_construct_y(lp, v,   ws%vyl,    ws%vyr)
      call weno_construct_y(lp, hv,  ws%hvyl,   ws%hvyr)
      call weno_construct_y(lp, hu,  ws%huyl,   ws%huyr)
      if (dispersion) then
         call weno_construct_y(lp, v4, ws%v4yl, ws%v4yr)
         call weno_construct_y(lp, u4, ws%u4yl, ws%u4yr)
      end if
      call assemble_y(lp, ws, depthy, mask9, gamma1, gamma3, dispersion)
   end subroutine construction_weno

   ! ----------------------------------------------------------------
   ! Top-level dispatcher.  Does NOT call boundary_condition —
   ! the caller is responsible for ghost-cell exchange.
   ! ----------------------------------------------------------------
   subroutine fluxes(lp, high_order, constr, &
                     eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                     dx, dy, inv_dx, inv_dy, mask, mask9, gamma1, gamma3, dispersion, ws)
      type(type_loop_bounds), intent(in)    :: lp
      character(len=*),       intent(in)    :: high_order, constr
      real(SP), intent(in)  :: eta(:,:), u(:,:), v(:,:), hu(:,:), hv(:,:)
      real(SP), intent(in)  :: u4(:,:), v4(:,:)
      real(SP), intent(in)  :: depthx(:,:), depthy(:,:)
      real(SP), intent(in)  :: dx(:,:), dy(:,:), inv_dx(:,:), inv_dy(:,:)
      integer,  intent(in)  :: mask(:,:), mask9(:,:)
      real(SP), intent(in)  :: gamma1, gamma3
      logical,  intent(in)  :: dispersion
      type(type_flux_workspace), intent(inout) :: ws

      select case (high_order(1:3))
      case ('FOU')
         call construction_ho(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                               mask, mask9, gamma1, gamma3, dispersion, ws)
      case ('FMI')
         call construction_ho_minmod(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                     mask, mask9, gamma1, gamma3, dispersion, ws)
      case ('WEN')
         call construction_weno(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                 mask9, gamma1, gamma3, dispersion, ws)
      case ('MLP')
         call construction_ho_mlp(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                                   mask, mask9, gamma1, gamma3, dispersion, ws)
      case default
         call construction(lp, eta, u, v, hu, hv, u4, v4, depthx, depthy, &
                           dx, dy, inv_dx, inv_dy, mask, mask9, gamma1, gamma3, dispersion, ws)
      end select

      call wave_speed(lp, ws%uxl, ws%uxr, ws%vyl, ws%vyr, &
                      ws%hxl, ws%hxr, ws%hyl, ws%hyr, &
                      ws%sxl, ws%sxr, ws%syl, ws%syr)

      if (constr(1:3) == 'HLL') then
         call flux_at_interface_hll(ws)
      else
         call flux_at_interface(ws)
      end if
   end subroutine fluxes

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
