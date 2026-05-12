module core_interpolation_mod
   use core_constants_mod, only: SP, N_GHOST
   use core_grid_mod,      only: type_grid_2d
   implicit none(external)

   type, public :: type_interpolator
      integer :: n_points = 0
      ! Ghost-inclusive local index of the lower-left bilinear cell.
      ! src_i = interior_i + N_GHOST so gather indexes directly into ghost-inclusive fields.
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
   ! grid%x, grid%y, grid%dx, grid%dy must be allocated (call grid%init_spacing first).
   ! field arrays passed to gather must be ghost-inclusive: size (local_nx+2*N_GHOST, ...).
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
         tmp_si(cnt)  = li + N_GHOST   ! shift to ghost-inclusive index
         tmp_sj(cnt)  = lj + N_GHOST
         tmp_a(cnt)   = a
         tmp_b(cnt)   = b
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
   ! field must be ghost-inclusive: first dimension size = local_nx + 2*N_GHOST.
   ! Call halo_exchange before gather so ghost cells contain valid neighbour data.
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

   ! Returns true if (xq,yq) falls in a bilinear cell owned by this rank.
   ! A rank owns cells where the lower-left node is in [1, local_nx/ny].
   ! Cross-boundary cells (lower-left here, upper-right in ghost) are valid for
   ! non-terminal ranks after halo_exchange.  Terminal (shore/left) ranks cap at
   ! their last grid node; non-terminal ranks cap at that node + one cell width.
   logical function point_is_local(xq, yq, grid)
      real(SP),           intent(in) :: xq, yq
      type(type_grid_2d), intent(in) :: grid
      integer :: li, lj
      logical :: in_x, in_y

      li = bisect(grid%x(:,1), xq)
      lj = bisect(grid%y(1,:), yq)

      in_x = (li >= 1) .and. (li <= grid%local_nx)
      in_y = (lj >= 1) .and. (lj <= grid%local_ny)

      if (grid%is_shore_boundary) then
         in_x = in_x .and. (xq <= grid%x(grid%local_nx, 1))
      else
         in_x = in_x .and. (xq < grid%x(grid%local_nx, 1) + grid%dx(grid%local_nx, 1))
      end if
      if (grid%is_left_boundary) then
         in_y = in_y .and. (yq <= grid%y(1, grid%local_ny))
      else
         in_y = in_y .and. (yq < grid%y(1, grid%local_ny) + grid%dy(1, grid%local_ny))
      end if

      point_is_local = in_x .and. in_y
   end function point_is_local

   ! Find the lower-left bilinear cell (li, lj) in interior local coords [1, local_nx/ny]
   ! and bilinear weights. Alpha/beta use dx(li)/dy(lj) so li=local_nx is valid
   ! (its upper neighbour is a ghost cell, populated by halo_exchange).
   subroutine find_bilinear_cell(xq, yq, grid, li, lj, alpha, beta)
      real(SP),           intent(in)  :: xq, yq
      type(type_grid_2d), intent(in)  :: grid
      integer,            intent(out) :: li, lj
      real(SP),           intent(out) :: alpha, beta

      li = bisect(grid%x(:, 1), xq)
      lj = bisect(grid%y(1, :), yq)

      li = max(1, min(grid%local_nx, li))
      lj = max(1, min(grid%local_ny, lj))

      ! Use dx(li)/dy(lj) rather than x(li+1)-x(li) so li=local_nx stays in bounds.
      alpha = (xq - grid%x(li, 1)) / grid%dx(li, 1)
      beta  = (yq - grid%y(1, lj)) / grid%dy(1, lj)
      alpha = max(0.0_SP, min(1.0_SP, alpha))
      beta  = max(0.0_SP, min(1.0_SP, beta))
   end subroutine find_bilinear_cell

   ! Left-biased bisection: largest idx such that arr(idx) <= val.
   ! Returns 0 if val < arr(1) (below range); point_is_local treats 0 as not owned.
   ! Returns size(arr) if val >= arr(size(arr)) (above range, caller applies upper-bound check).
   ! Assumes arr is strictly increasing, size >= 2.
   integer function bisect(arr, val) result(idx)
      real(SP), intent(in) :: arr(:), val
      integer :: lo, hi, mid

      if (val < arr(1)) then
         idx = 0
         return
      end if

      lo = 1
      hi = size(arr)
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
