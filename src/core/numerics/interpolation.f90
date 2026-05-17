!> @file interpolation.f90
!> @brief MPI-distributed bilinear interpolation on structured grids.

!> Bilinear interpolation of 2-D field values at arbitrary query points
!! distributed across an MPI-decomposed structured grid.
!!
!! Each MPI rank owns a contiguous subdomain of the global grid.  On
!! initialisation (`type_interpolator%init`) every rank retains only the
!! query points that fall within its local subdomain; all other points
!! are discarded.  The gather step (`type_interpolator%gather`) evaluates
!! the bilinear formula locally, with no inter-rank communication required
!! provided the caller has already performed a halo exchange.
!!
!! @note Ghost cells (N_GHOST layers) must be populated by a halo exchange
!!       before calling gather.  Internal indices are stored in
!!       ghost-inclusive form so that the hot loop avoids index arithmetic.
!!
!! @see core_grid_mod::type_grid_2d
!! @see core_constants_mod::N_GHOST
module core_interpolation_mod
   use core_constants_mod, only: SP, N_GHOST
   use core_grid_mod,      only: type_grid_2d
   implicit none

   !> Stores pre-computed bilinear stencils for a set of query points
   !! that are local to the calling MPI rank.
   !!
   !! Members are allocated by `init` and freed by `finalize`.
   !! All index arrays are in *ghost-inclusive* local coordinates so that
   !! `gather` can index directly into ghost-padded field arrays.
   type, public :: type_interpolator
      !> Number of query points owned by this rank.
      integer :: n_points = 0
      !> Ghost-inclusive local i-index of the lower-left bilinear cell,
      !! i.e. interior index + N_GHOST.
      integer,  allocatable :: src_i(:)
      !> Ghost-inclusive local j-index of the lower-left bilinear cell.
      integer,  allocatable :: src_j(:)
      !> Normalised x-weight \f$\alpha \in [0,1]\f$.
      real(SP), allocatable :: alpha(:)
      !> Normalised y-weight \f$\beta \in [0,1]\f$.
      real(SP), allocatable :: beta(:)
      !> Global (1-based) index of each point, for output assembly across ranks.
      integer,  allocatable :: point_id(:)
   contains
      procedure :: init     => interp_init
      procedure :: gather   => interp_gather
      procedure :: finalize => interp_finalize
   end type type_interpolator

contains

   !> Initialise from a set of global query coordinates.
   !!
   !! Iterates over all global query points and retains only those that
   !! fall within the local subdomain owned by the calling MPI rank.
   !! For each retained point the bilinear cell and weights are computed
   !! by `find_bilinear_cell`.
   !!
   !! @pre  `grid%x`, `grid%y`, `grid%dx`, `grid%dy` must be allocated
   !!       (call `grid%init_spacing` first).
   !! @pre  Field arrays passed to `gather` must be ghost-inclusive:
   !!       first dimension size = `local_nx + 2*N_GHOST`.
   !!
   !! @param[in]  xq    Global x-coordinates of query points (size N_global).
   !! @param[in]  yq    Global y-coordinates of query points (size N_global).
   !! @param[in]  grid  Local subdomain grid descriptor.
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

   !> Evaluate bilinear field values at the pre-computed query points.
   !!
   !! For each locally owned point \f$k\f$ with lower-left cell index
   !! \f$(i,j)\f$ and normalised weights \f$\alpha, \beta \in [0,1]\f$,
   !! the interpolated value is
   !!
   !! \f[
   !!   f_k = (1-\alpha)(1-\beta)\,f_{i,\,j}
   !!       + \alpha(1-\beta)\,f_{i+1,\,j}
   !!       + (1-\alpha)\beta\,f_{i,\,j+1}
   !!       + \alpha\,\beta\,f_{i+1,\,j+1}
   !! \f]
   !!
   !! where the weights are defined as
   !! \f[
   !!   \alpha = \frac{x_k - x_i}{\Delta x_i}, \qquad
   !!   \beta  = \frac{y_k - y_j}{\Delta y_j}.
   !! \f]
   !!
   !! See \cite press2007numerical, §3.6, for the bilinear form on a
   !! non-uniform rectilinear grid.
   !!
   !! @note The loop is parallelised with OpenMP (`!$omp parallel do`).
   !!       Call a halo exchange before gather so ghost cells contain
   !!       valid neighbour data.
   !!
   !! @param[in]  field  Ghost-inclusive 2-D field array; first dimension
   !!                    size must equal `local_nx + 2*N_GHOST`.
   !! @param[out] out    Interpolated values at the `n_points` locally
   !!                    owned query points.
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

   !> Release all allocated arrays and reset the point count.
   subroutine interp_finalize(this)
      class(type_interpolator), intent(inout) :: this
      this%n_points = 0
      if (allocated(this%src_i))    deallocate(this%src_i)
      if (allocated(this%src_j))    deallocate(this%src_j)
      if (allocated(this%alpha))    deallocate(this%alpha)
      if (allocated(this%beta))     deallocate(this%beta)
      if (allocated(this%point_id)) deallocate(this%point_id)
   end subroutine interp_finalize

   !> Test whether a query point falls within the local subdomain.
   !!
   !! A rank owns bilinear cells whose lower-left node lies in the
   !! interior index range \f$[1, N_{\rm loc}]\f$.  At terminal
   !! (shore/left) boundaries the domain is closed; elsewhere it is
   !! half-open to avoid double-counting with the adjacent rank.
   !!
   !! @param[in]  xq    x-coordinate of the query point.
   !! @param[in]  yq    y-coordinate of the query point.
   !! @param[in]  grid  Local subdomain grid descriptor.
   !! @return           `.true.` if the point is owned by this rank.
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

   !> Find the lower-left bilinear cell and compute normalised weights.
   !!
   !! Returns interior local indices \f$(l_i, l_j) \in [1, N_{\rm loc}]\f$
   !! and weights
   !! \f[
   !!   \alpha = \frac{x_q - x_{l_i}}{\Delta x_{l_i}}, \qquad
   !!   \beta  = \frac{y_q - y_{l_j}}{\Delta y_{l_j}},
   !! \f]
   !! both clamped to \f$[0,1]\f$.  Using the cell-local spacing
   !! \f$\Delta x_{l_i}\f$ rather than \f$x_{l_i+1}-x_{l_i}\f$ keeps
   !! \f$l_i = N_{\rm loc}\f$ in-bounds, with the upper node in the ghost
   !! layer (populated by halo exchange).
   !!
   !! @param[in]  xq     x-coordinate of the query point.
   !! @param[in]  yq     y-coordinate of the query point.
   !! @param[in]  grid   Local subdomain grid descriptor.
   !! @param[out] li     Interior i-index of lower-left cell node.
   !! @param[out] lj     Interior j-index of lower-left cell node.
   !! @param[out] alpha  Normalised x-weight \f$\in [0,1]\f$.
   !! @param[out] beta   Normalised y-weight \f$\in [0,1]\f$.
   subroutine find_bilinear_cell(xq, yq, grid, li, lj, alpha, beta)
      real(SP),           intent(in)  :: xq, yq
      type(type_grid_2d), intent(in)  :: grid
      integer,            intent(out) :: li, lj
      real(SP),           intent(out) :: alpha, beta

      li = bisect(grid%x(:, 1), xq)
      lj = bisect(grid%y(1, :), yq)

      li = max(1, min(grid%local_nx, li))
      lj = max(1, min(grid%local_ny, lj))

      alpha = (xq - grid%x(li, lj)) / grid%dx(li, lj)
      beta  = (yq - grid%y(li, lj)) / grid%dy(li, lj)
      alpha = max(0.0_SP, min(1.0_SP, alpha))
      beta  = max(0.0_SP, min(1.0_SP, beta))
   end subroutine find_bilinear_cell

   !> Left-biased binary search on a strictly increasing 1-D array.
   !!
   !! Returns the largest index \f$k\f$ such that
   !! \f$\texttt{arr}(k) \le \texttt{val}\f$.
   !! Special cases:
   !! - Returns 0 if \f$\texttt{val} < \texttt{arr}(1)\f$.
   !! - Returns \f$N\f$ (the array size) if
   !!   \f$\texttt{val} \ge \texttt{arr}(N)\f$; the caller is responsible
   !!   for the upper-boundary check.
   !!
   !! Complexity: \f$\mathcal{O}(\log N)\f$. \cite press2007numerical
   !!
   !! @param[in]  arr  Strictly increasing array, size \f$N \ge 2\f$.
   !! @param[in]  val  Query value.
   !! @return          Largest index \f$k\f$ with \f$\texttt{arr}(k) \le \texttt{val}\f$.
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
