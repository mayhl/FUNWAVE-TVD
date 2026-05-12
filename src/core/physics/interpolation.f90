module core_interpolation_mod
   use core_constants_mod, only: SP
   use core_grid_mod,      only: type_grid_2d
   implicit none(external)

   type, public :: type_interpolator
      integer :: n_points = 0
      ! Lower-left cell index in local grid coords (1-based); src_i+1 is always valid
      integer,  allocatable :: src_i(:), src_j(:)
      ! Bilinear weights in [0,1]: alpha in x-dir, beta in y-dir
      real(SP), allocatable :: alpha(:), beta(:)
      ! Global query-point index for output assembly across ranks
      integer,  allocatable :: point_id(:)
   contains
      procedure :: init     => interp_init
      procedure :: gather   => interp_gather
      procedure :: finalize => interp_finalize
   end type type_interpolator

contains

   ! Initialise from a set of global query coordinates.
   ! Each MPI rank retains only the points that fall in its local subdomain.
   ! grid%x and grid%y must be allocated before calling (call grid%init_spacing first).
   subroutine interp_init(this, xq, yq, grid)
      class(type_interpolator), intent(inout) :: this
      real(SP),           intent(in) :: xq(:), yq(:)
      type(type_grid_2d), intent(in) :: grid

      integer :: n_global, k, cnt
      integer,  allocatable :: tmp_si(:), tmp_sj(:), tmp_pid(:)
      real(SP), allocatable :: tmp_a(:), tmp_b(:)
      integer  :: li, lj
      real(SP) :: a, b

      n_global = size(xq)
      allocate(tmp_si(n_global), tmp_sj(n_global), tmp_pid(n_global))
      allocate(tmp_a(n_global),  tmp_b(n_global))

      cnt = 0
      do k = 1, n_global
         if (.not. point_is_local(xq(k), yq(k), grid)) cycle
         call find_bilinear_cell(xq(k), yq(k), grid, li, lj, a, b)
         cnt = cnt + 1
         tmp_si(cnt)  = li;  tmp_sj(cnt)  = lj
         tmp_a(cnt)   = a;   tmp_b(cnt)   = b
         tmp_pid(cnt) = k
      end do

      call this%finalize()
      this%n_points = cnt
      if (cnt > 0) then
         this%src_i    = tmp_si(1:cnt)
         this%src_j    = tmp_sj(1:cnt)
         this%alpha    = tmp_a(1:cnt)
         this%beta     = tmp_b(1:cnt)
         this%point_id = tmp_pid(1:cnt)
      end if
   end subroutine interp_init

   ! Bilinear gather — hot loop. No polymorphic dispatch; contiguous arrays.
   ! out must be sized to at least n_points.
   subroutine interp_gather(this, field, out)
      class(type_interpolator), intent(in)  :: this
      real(SP), intent(in)  :: field(:,:)
      real(SP), intent(out) :: out(:)
      integer  :: k, i, j
      real(SP) :: a, b, oma, omb

      !$omp parallel do private(i, j, a, b, oma, omb)
      do k = 1, this%n_points
         i = this%src_i(k);  j = this%src_j(k)
         a = this%alpha(k);  b = this%beta(k)
         oma = 1.0_SP - a;   omb = 1.0_SP - b
         out(k) = oma*omb * field(i,   j  ) &
                +   a*omb * field(i+1, j  ) &
                + oma*  b * field(i,   j+1) &
                +   a*  b * field(i+1, j+1)
      end do
      !$omp end parallel do
   end subroutine interp_gather

   subroutine interp_finalize(this)
      class(type_interpolator), intent(inout) :: this
      this%n_points = 0
      if (allocated(this%src_i))    deallocate(this%src_i)
      if (allocated(this%src_j))    deallocate(this%src_j)
      if (allocated(this%alpha))    deallocate(this%alpha)
      if (allocated(this%beta))     deallocate(this%beta)
      if (allocated(this%point_id)) deallocate(this%point_id)
   end subroutine interp_finalize

   ! Returns true if (xq,yq) is owned by this rank.
   ! Interior ranks use exclusive upper bound to avoid double-counting with the next rank.
   ! Terminal ranks (shore / left) use inclusive upper bound.
   logical function point_is_local(xq, yq, grid)
      real(SP),           intent(in) :: xq, yq
      type(type_grid_2d), intent(in) :: grid
      real(SP) :: x_lo, x_hi, y_lo, y_hi
      logical  :: in_x, in_y

      x_lo = grid%x(1,             1)
      x_hi = grid%x(grid%local_nx, 1)
      y_lo = grid%y(1, 1)
      y_hi = grid%y(1, grid%local_ny)

      if (grid%is_shore_boundary) then
         in_x = (xq >= x_lo) .and. (xq <= x_hi)
      else
         in_x = (xq >= x_lo) .and. (xq <  x_hi)
      end if

      if (grid%is_left_boundary) then
         in_y = (yq >= y_lo) .and. (yq <= y_hi)
      else
         in_y = (yq >= y_lo) .and. (yq <  y_hi)
      end if

      point_is_local = in_x .and. in_y
   end function point_is_local

   ! Find the lower-left bilinear cell (li, lj) in local coords and weights.
   ! For uniform grids the cell index is O(1); for variable it uses bisection.
   subroutine find_bilinear_cell(xq, yq, grid, li, lj, alpha, beta)
      real(SP),           intent(in)  :: xq, yq
      type(type_grid_2d), intent(in)  :: grid
      integer,            intent(out) :: li, lj
      real(SP),           intent(out) :: alpha, beta

      if (grid%is_uniform) then
         li = int((xq - grid%x(1, 1)) / grid%dx0) + 1
         lj = int((yq - grid%y(1, 1)) / grid%dy0) + 1
      else
         li = bisect(grid%x(:, 1), xq)
         lj = bisect(grid%y(1, :), yq)
      end if

      li = max(1, min(grid%local_nx - 1, li))
      lj = max(1, min(grid%local_ny - 1, lj))

      alpha = (xq - grid%x(li,   1)) / (grid%x(li+1, 1) - grid%x(li,   1))
      beta  = (yq - grid%y(1,  lj)) / (grid%y(1, lj+1) - grid%y(1,  lj))
      alpha = max(0.0_SP, min(1.0_SP, alpha))
      beta  = max(0.0_SP, min(1.0_SP, beta))
   end subroutine find_bilinear_cell

   ! Left-biased bisection: largest idx such that arr(idx) <= val.
   ! Assumes arr is strictly increasing, size >= 2.
   integer function bisect(arr, val) result(idx)
      real(SP), intent(in) :: arr(:), val
      integer :: lo, hi, mid

      lo = 1
      hi = size(arr) - 1
      do while (lo < hi)
         mid = (lo + hi + 1) / 2
         if (arr(mid) <= val) then
            lo = mid
         else
            hi = mid - 1
         end if
      end do
      idx = lo
   end function bisect

end module core_interpolation_mod
