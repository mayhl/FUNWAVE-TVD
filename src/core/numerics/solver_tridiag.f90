! allow(E001)
!> @file solver_tridiag.f90
!> @brief Tridiagonal solvers — serial Thomas primitive and MPI-distributed sweeps.

!> Solver-class module: unlike `kernel_*` modules (pure compute, no MPI),
!! `solver_*` modules may hold a `type_grid_2d` reference and communicate.
!! Callers assemble coefficients with a kernel, invoke a solver here, then
!! consume the solution with another kernel (see model_kernel_etauv_mod).
!!
!! Buffers may be device-resident (device-aware MPI); do not add host-side
!! staging assumptions to the message paths.
module core_solver_tridiag_mod
   use core_constants_mod, only: SP, MPI_SP
   use core_grid_mod, only: type_loop_bounds, type_grid_2d
   use mpi_f08
   implicit none
   private

   public :: trid_thomas_1d
   public :: trid_x, trid_y
   public :: trid_x_periodic, trid_y_periodic
   public :: trid_configure
   ! public for the unit-test bitwise pair vs the pipelined sweeps
   public :: trid_y_alltoall

   ! Scratch for the periodic (Sherman-Morrison) solves: one coefficient
   ! copy plus the two RHS/solution pairs.  Both solves share a single
   ! eliminated c (the c recurrence never reads d).  Allocated once by
   ! the caller (mloc×nloc), reused every stage/step — no per-call heap
   ! traffic.
   type, public :: type_trid_workspace
      integer :: m = 0, n = 0
      real(SP), allocatable :: a_loc(:, :)
      real(SP), allocatable :: c1(:, :)
      real(SP), allocatable :: d1(:, :), d2(:, :)
      real(SP), allocatable :: y1(:, :), y2(:, :)
   contains
      procedure :: alloc => tws_alloc
      procedure :: free => tws_free
   end type type_trid_workspace

   ! Chunk width of the pipelined sweeps and the transpose threshold —
   ! deck-tunable (numerics: tridiag:), defaults measured on wheat:
   ! chunk from the 312446 sweep (flat 16-64, 96 worse), threshold
   ! from the 313297 A/B (transpose wins at PY=80 and PY=40 on 8
   ! nodes, neutral at PY=40 on 4, loses at PY<=10).  Both are
   ! bitwise-neutral and system-flavored — retune per fabric via the
   ! benchmark harness.
   integer :: trid_chunk = 48
   integer :: trid_transpose_min_py = 40

   ! Transpose-path persistent buffers — grow-only, sized on first
   ! solve (grid extents are run-constant); avoids per-stage heap
   ! churn.  Message buffers are shared by both all-to-all phases.
   real(SP), allocatable :: ts_sbuf(:), ts_rbuf(:)
   real(SP), allocatable :: ts_la(:, :), ts_lc(:, :), ts_l1(:, :), ts_l2(:, :)
   ! Transpose-path decomposition metadata — geometry-only (py, mx, ny),
   ! so cached across calls; the (py, mx, ny) key guards re-derivation
   ! (and its Allgather) if a differently-shaped grid ever calls in
   integer, allocatable :: ts_wq(:), ts_x0(:), ts_nyp(:), ts_yoff(:)
   integer :: ts_key_py = -1, ts_key_mx = -1, ts_key_ny = -1
   integer :: ts_nyg = 0, ts_wme = 0

contains

   subroutine tws_alloc(ws, m, n)
      class(type_trid_workspace), intent(inout) :: ws
      integer, intent(in) :: m, n
      ws%m = m; ws%n = n
      allocate (ws%a_loc(m, n), ws%c1(m, n), &
                ws%d1(m, n), ws%d2(m, n), &
                ws%y1(m, n), ws%y2(m, n))
   end subroutine tws_alloc

   subroutine tws_free(ws)
      class(type_trid_workspace), intent(inout) :: ws
      ws%m = 0; ws%n = 0
      deallocate (ws%a_loc, ws%c1, ws%d1, ws%d2, ws%y1, ws%y2)
   end subroutine tws_free

   ! ----------------------------------------------------------------
   ! trid_configure — deck overrides for the tunables above (numerics
   ! reader calls this; non-positive values keep the defaults)
   ! ----------------------------------------------------------------
   subroutine trid_configure(chunk, transpose_min_py)
      integer, intent(in) :: chunk, transpose_min_py
      if (chunk > 0) trid_chunk = chunk
      if (transpose_min_py > 0) trid_transpose_min_py = transpose_min_py
   end subroutine trid_configure

   ! ----------------------------------------------------------------
   ! trid_thomas_1d — serial 1D Thomas along a single line.
   ! Diagonal assumed 1 (caller normalises). a, c, d overwritten
   ! in-place during elimination. Skip row if a(i)=0 (masked).
   ! Local per-column solves (e.g. 3D sigma viscosity) call this;
   ! the distributed sweeps below inline the same recurrence because
   ! the pipeline splits each line across ranks.
   ! ----------------------------------------------------------------
   pure subroutine trid_thomas_1d(n, a, c, d, z)
      integer, intent(in)    :: n
      real(SP), intent(inout) :: a(n), c(n), d(n)
      real(SP), intent(out)   :: z(n)
      integer :: i
      do i = 2, n
         if (a(i) /= 0.0_SP) then
            c(i) = c(i)/a(i)/(1.0_SP/a(i) - c(i - 1))
            d(i) = (d(i)/a(i) - d(i - 1))/(1.0_SP/a(i) - c(i - 1))
         end if
      end do
      z(n) = d(n)
      do i = n - 1, 1, -1
         z(i) = d(i) - c(i)*z(i + 1)
      end do
   end subroutine trid_thomas_1d

   ! ----------------------------------------------------------------
   ! trid_x — MPI chunk-pipelined Thomas in x.
   ! Forward sweep west→east; back-sub east→west.
   ! west neighbor = grid%back_rank, east = grid%shore_rank.
   ! Chain endpoints come from the cart POSITION (iproc), never from
   ! neighbor nullity: under a periodic cart topology the wrap makes
   ! every rank have neighbors, but the sweep is still linear.
   ! The transverse j-extent is swept in chunks so downstream ranks
   ! start eliminating while this rank is still mid-block; same-tag
   ! chunks match FIFO (recvs pre-posted and sends issued in chunk
   ! order).  PX=1 keeps the plain full-width path.
   ! c and d are overwritten during elimination; a is read-only.
   ! ----------------------------------------------------------------
   subroutine trid_x(lp, grid, a, c, d, f)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: a(:, :)
      real(SP), intent(inout) :: c(:, :), d(:, :)
      real(SP), intent(out)   :: f(:, :)

      real(SP)          :: smsg(2, lp%nloc), rmsg(2, lp%nloc)
      real(SP)          :: sbk(lp%nloc), rbk(lp%nloc)
      type(MPI_Request) :: rreq(lp%nloc), sreq(lp%nloc)
      type(MPI_Status)  :: stat
      integer           :: i, j, ierr, nt, w, nc, ic, j0, j1

      if (grid%nx_proc == 1) then
         ! single-rank chain: plain full-width Thomas, no messages
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = lp%jb, lp%je
            do i = lp%ib + 1, lp%ie
               if (a(i, j) /= 0.0_SP) then
                  c(i, j) = c(i, j)/a(i, j)/(1.0_SP/a(i, j) - c(i - 1, j))
                  d(i, j) = (d(i, j)/a(i, j) - d(i - 1, j))/(1.0_SP/a(i, j) - c(i - 1, j))
               end if
            end do
         end do
         !$omp end parallel do
         do j = lp%jb, lp%je
            f(lp%ie, j) = d(lp%ie, j)
         end do
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = lp%jb, lp%je
            do i = lp%ie - 1, lp%ib, -1
               f(i, j) = d(i, j) - c(i, j)*f(i + 1, j)
            end do
         end do
         !$omp end parallel do
         return
      end if

      nt = lp%je - lp%jb + 1
      w = min(trid_chunk, nt)
      nc = (nt + w - 1)/w

      ! --- forward sweep, chunk-pipelined ---
      if (grid%iproc > 0) then
         do ic = 1, nc
            j0 = lp%jb + (ic - 1)*w
            j1 = min(j0 + w - 1, lp%je)
            call MPI_Irecv(rmsg(:, j0:j1), 2*(j1 - j0 + 1), MPI_SP, grid%back_rank, 0, &
                           grid%cart_comm, rreq(ic), ierr)
         end do
      end if

      do ic = 1, nc
         j0 = lp%jb + (ic - 1)*w
         j1 = min(j0 + w - 1, lp%je)
         if (grid%iproc > 0) then
            call MPI_Wait(rreq(ic), stat, ierr)
            do j = j0, j1
               if (a(lp%ib, j) /= 0.0_SP) then
                  c(lp%ib, j) = c(lp%ib, j)/a(lp%ib, j) &
                                /(1.0_SP/a(lp%ib, j) - rmsg(2, j))
                  d(lp%ib, j) = (d(lp%ib, j)/a(lp%ib, j) - rmsg(1, j)) &
                                /(1.0_SP/a(lp%ib, j) - rmsg(2, j))
               end if
            end do
         end if
         ! recurrence runs along i, rows independent — thread over the chunk
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = j0, j1
            do i = lp%ib + 1, lp%ie
               if (a(i, j) /= 0.0_SP) then
                  c(i, j) = c(i, j)/a(i, j)/(1.0_SP/a(i, j) - c(i - 1, j))
                  d(i, j) = (d(i, j)/a(i, j) - d(i - 1, j))/(1.0_SP/a(i, j) - c(i - 1, j))
               end if
            end do
         end do
         !$omp end parallel do
         if (grid%iproc < grid%nx_proc - 1) then
            do j = j0, j1
               smsg(1, j) = d(lp%ie, j)
               smsg(2, j) = c(lp%ie, j)
            end do
            call MPI_Isend(smsg(:, j0:j1), 2*(j1 - j0 + 1), MPI_SP, grid%shore_rank, 0, &
                           grid%cart_comm, sreq(ic), ierr)
         end if
      end do
      if (grid%iproc < grid%nx_proc - 1) &
         call MPI_Waitall(nc, sreq(1:nc), MPI_STATUSES_IGNORE, ierr)

      ! --- back substitution, chunk-pipelined ---
      if (grid%iproc < grid%nx_proc - 1) then
         do ic = 1, nc
            j0 = lp%jb + (ic - 1)*w
            j1 = min(j0 + w - 1, lp%je)
            call MPI_Irecv(rbk(j0:j1), j1 - j0 + 1, MPI_SP, grid%shore_rank, 1, &
                           grid%cart_comm, rreq(ic), ierr)
         end do
      end if

      do ic = 1, nc
         j0 = lp%jb + (ic - 1)*w
         j1 = min(j0 + w - 1, lp%je)
         if (grid%iproc < grid%nx_proc - 1) then
            call MPI_Wait(rreq(ic), stat, ierr)
            do j = j0, j1
               f(lp%ie, j) = d(lp%ie, j) - c(lp%ie, j)*rbk(j)
            end do
         else
            do j = j0, j1
               f(lp%ie, j) = d(lp%ie, j)
            end do
         end if
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = j0, j1
            do i = lp%ie - 1, lp%ib, -1
               f(i, j) = d(i, j) - c(i, j)*f(i + 1, j)
            end do
         end do
         !$omp end parallel do
         if (grid%iproc > 0) then
            do j = j0, j1
               sbk(j) = f(lp%ib, j)
            end do
            call MPI_Isend(sbk(j0:j1), j1 - j0 + 1, MPI_SP, grid%back_rank, 1, &
                           grid%cart_comm, sreq(ic), ierr)
         end if
      end do
      if (grid%iproc > 0) &
         call MPI_Waitall(nc, sreq(1:nc), MPI_STATUSES_IGNORE, ierr)

   end subroutine trid_x

   ! ----------------------------------------------------------------
   ! trid_x2 — two-RHS chunk-pipelined Thomas in x, one shared
   ! elimination.  The c recurrence never reads d, so both RHS ride a
   ! single sweep: per-RHS arithmetic matches two trid_x calls bitwise,
   ! but the rank pipeline is traversed once instead of twice.
   ! Chunking as in trid_x.  Sherman-Morrison callers only (both
   ! solves must share a and the pre-sweep c).
   ! ----------------------------------------------------------------
   subroutine trid_x2(lp, grid, a, c, d1, d2, f1, f2)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: a(:, :)
      real(SP), intent(inout) :: c(:, :), d1(:, :), d2(:, :)
      real(SP), intent(out)   :: f1(:, :), f2(:, :)

      real(SP)          :: smsg(3, lp%nloc), rmsg(3, lp%nloc)
      real(SP)          :: sbk(2, lp%nloc), rbk(2, lp%nloc)
      type(MPI_Request) :: rreq(lp%nloc), sreq(lp%nloc)
      type(MPI_Status)  :: stat
      integer           :: i, j, ierr, nt, w, nc, ic, j0, j1

      if (grid%nx_proc == 1) then
         ! single-rank chain: plain full-width Thomas, no messages
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = lp%jb, lp%je
            do i = lp%ib + 1, lp%ie
               if (a(i, j) /= 0.0_SP) then
                  c(i, j) = c(i, j)/a(i, j)/(1.0_SP/a(i, j) - c(i - 1, j))
                  d1(i, j) = (d1(i, j)/a(i, j) - d1(i - 1, j))/(1.0_SP/a(i, j) - c(i - 1, j))
                  d2(i, j) = (d2(i, j)/a(i, j) - d2(i - 1, j))/(1.0_SP/a(i, j) - c(i - 1, j))
               end if
            end do
         end do
         !$omp end parallel do
         do j = lp%jb, lp%je
            f1(lp%ie, j) = d1(lp%ie, j)
            f2(lp%ie, j) = d2(lp%ie, j)
         end do
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = lp%jb, lp%je
            do i = lp%ie - 1, lp%ib, -1
               f1(i, j) = d1(i, j) - c(i, j)*f1(i + 1, j)
               f2(i, j) = d2(i, j) - c(i, j)*f2(i + 1, j)
            end do
         end do
         !$omp end parallel do
         return
      end if

      nt = lp%je - lp%jb + 1
      w = min(trid_chunk, nt)
      nc = (nt + w - 1)/w

      ! --- forward sweep, chunk-pipelined ---
      if (grid%iproc > 0) then
         do ic = 1, nc
            j0 = lp%jb + (ic - 1)*w
            j1 = min(j0 + w - 1, lp%je)
            call MPI_Irecv(rmsg(:, j0:j1), 3*(j1 - j0 + 1), MPI_SP, grid%back_rank, 0, &
                           grid%cart_comm, rreq(ic), ierr)
         end do
      end if

      do ic = 1, nc
         j0 = lp%jb + (ic - 1)*w
         j1 = min(j0 + w - 1, lp%je)
         if (grid%iproc > 0) then
            call MPI_Wait(rreq(ic), stat, ierr)
            do j = j0, j1
               if (a(lp%ib, j) /= 0.0_SP) then
                  c(lp%ib, j) = c(lp%ib, j)/a(lp%ib, j) &
                                /(1.0_SP/a(lp%ib, j) - rmsg(3, j))
                  d1(lp%ib, j) = (d1(lp%ib, j)/a(lp%ib, j) - rmsg(1, j)) &
                                 /(1.0_SP/a(lp%ib, j) - rmsg(3, j))
                  d2(lp%ib, j) = (d2(lp%ib, j)/a(lp%ib, j) - rmsg(2, j)) &
                                 /(1.0_SP/a(lp%ib, j) - rmsg(3, j))
               end if
            end do
         end if
         ! recurrence runs along i, rows independent — thread over the chunk
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = j0, j1
            do i = lp%ib + 1, lp%ie
               if (a(i, j) /= 0.0_SP) then
                  c(i, j) = c(i, j)/a(i, j)/(1.0_SP/a(i, j) - c(i - 1, j))
                  d1(i, j) = (d1(i, j)/a(i, j) - d1(i - 1, j))/(1.0_SP/a(i, j) - c(i - 1, j))
                  d2(i, j) = (d2(i, j)/a(i, j) - d2(i - 1, j))/(1.0_SP/a(i, j) - c(i - 1, j))
               end if
            end do
         end do
         !$omp end parallel do
         if (grid%iproc < grid%nx_proc - 1) then
            do j = j0, j1
               smsg(1, j) = d1(lp%ie, j)
               smsg(2, j) = d2(lp%ie, j)
               smsg(3, j) = c(lp%ie, j)
            end do
            call MPI_Isend(smsg(:, j0:j1), 3*(j1 - j0 + 1), MPI_SP, grid%shore_rank, 0, &
                           grid%cart_comm, sreq(ic), ierr)
         end if
      end do
      if (grid%iproc < grid%nx_proc - 1) &
         call MPI_Waitall(nc, sreq(1:nc), MPI_STATUSES_IGNORE, ierr)

      ! --- back substitution, chunk-pipelined ---
      if (grid%iproc < grid%nx_proc - 1) then
         do ic = 1, nc
            j0 = lp%jb + (ic - 1)*w
            j1 = min(j0 + w - 1, lp%je)
            call MPI_Irecv(rbk(:, j0:j1), 2*(j1 - j0 + 1), MPI_SP, grid%shore_rank, 1, &
                           grid%cart_comm, rreq(ic), ierr)
         end do
      end if

      do ic = 1, nc
         j0 = lp%jb + (ic - 1)*w
         j1 = min(j0 + w - 1, lp%je)
         if (grid%iproc < grid%nx_proc - 1) then
            call MPI_Wait(rreq(ic), stat, ierr)
            do j = j0, j1
               f1(lp%ie, j) = d1(lp%ie, j) - c(lp%ie, j)*rbk(1, j)
               f2(lp%ie, j) = d2(lp%ie, j) - c(lp%ie, j)*rbk(2, j)
            end do
         else
            do j = j0, j1
               f1(lp%ie, j) = d1(lp%ie, j)
               f2(lp%ie, j) = d2(lp%ie, j)
            end do
         end if
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = j0, j1
            do i = lp%ie - 1, lp%ib, -1
               f1(i, j) = d1(i, j) - c(i, j)*f1(i + 1, j)
               f2(i, j) = d2(i, j) - c(i, j)*f2(i + 1, j)
            end do
         end do
         !$omp end parallel do
         if (grid%iproc > 0) then
            do j = j0, j1
               sbk(1, j) = f1(lp%ib, j)
               sbk(2, j) = f2(lp%ib, j)
            end do
            call MPI_Isend(sbk(:, j0:j1), 2*(j1 - j0 + 1), MPI_SP, grid%back_rank, 1, &
                           grid%cart_comm, sreq(ic), ierr)
         end if
      end do
      if (grid%iproc > 0) &
         call MPI_Waitall(nc, sreq(1:nc), MPI_STATUSES_IGNORE, ierr)

   end subroutine trid_x2

   ! ----------------------------------------------------------------
   ! trid_y — MPI chunk-pipelined Thomas in y.
   ! Forward sweep south→north; back-sub north→south.
   ! south neighbor = grid%right_rank, north = grid%left_rank.
   ! Chain endpoints from the cart position (jproc), as in trid_x —
   ! required for periodic-y cart topologies (Sherman-Morrison callers).
   ! The transverse i-extent is swept in chunks (see trid_x); PY=1
   ! keeps the plain full-width path.
   ! ----------------------------------------------------------------
   subroutine trid_y(lp, grid, a, c, d, f)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: a(:, :)
      real(SP), intent(inout) :: c(:, :), d(:, :)
      real(SP), intent(out)   :: f(:, :)

      real(SP)          :: smsg(2, lp%mloc), rmsg(2, lp%mloc)
      real(SP)          :: sbk(lp%mloc), rbk(lp%mloc)
      type(MPI_Request) :: rreq(lp%mloc), sreq(lp%mloc)
      type(MPI_Status)  :: stat
      integer           :: i, j, ierr, nt, w, nc, ic, i0, i1

      if (grid%ny_proc == 1) then
         ! single-rank chain: plain full-width Thomas, no messages.
         ! NOT OMP-threaded: the j recurrence bars the sweep loop, and
         ! the i-slab variant (each thread sweeping its own column
         ! range) cost ~5% serial under ifx — code-shape regression,
         ! wheat A/B 310901 vs 310916; columns stay a GPU-pass target
         do j = lp%jb + 1, lp%je
            do i = lp%ib, lp%ie
               if (a(i, j) /= 0.0_SP) then
                  c(i, j) = c(i, j)/a(i, j)/(1.0_SP/a(i, j) - c(i, j - 1))
                  d(i, j) = (d(i, j)/a(i, j) - d(i, j - 1))/(1.0_SP/a(i, j) - c(i, j - 1))
               end if
            end do
         end do
         do i = lp%ib, lp%ie
            f(i, lp%je) = d(i, lp%je)
         end do
         do j = lp%je - 1, lp%jb, -1
            do i = lp%ib, lp%ie
               f(i, j) = d(i, j) - c(i, j)*f(i, j + 1)
            end do
         end do
         return
      end if

      if (grid%ny_proc >= trid_transpose_min_py) then
         call trid_y_alltoall(lp, grid, a, c, d, f)
         return
      end if

      nt = lp%ie - lp%ib + 1
      w = min(trid_chunk, nt)
      nc = (nt + w - 1)/w

      ! --- forward sweep, chunk-pipelined ---
      if (grid%jproc > 0) then
         do ic = 1, nc
            i0 = lp%ib + (ic - 1)*w
            i1 = min(i0 + w - 1, lp%ie)
            call MPI_Irecv(rmsg(:, i0:i1), 2*(i1 - i0 + 1), MPI_SP, grid%right_rank, 0, &
                           grid%cart_comm, rreq(ic), ierr)
         end do
      end if

      do ic = 1, nc
         i0 = lp%ib + (ic - 1)*w
         i1 = min(i0 + w - 1, lp%ie)
         if (grid%jproc > 0) then
            call MPI_Wait(rreq(ic), stat, ierr)
            do i = i0, i1
               if (a(i, lp%jb) /= 0.0_SP) then
                  c(i, lp%jb) = c(i, lp%jb)/a(i, lp%jb) &
                                /(1.0_SP/a(i, lp%jb) - rmsg(2, i))
                  d(i, lp%jb) = (d(i, lp%jb)/a(i, lp%jb) - rmsg(1, i)) &
                                /(1.0_SP/a(i, lp%jb) - rmsg(2, i))
               end if
            end do
         end if
         ! unthreaded sweep — see the code-shape note on the PY=1 path
         do j = lp%jb + 1, lp%je
            do i = i0, i1
               if (a(i, j) /= 0.0_SP) then
                  c(i, j) = c(i, j)/a(i, j)/(1.0_SP/a(i, j) - c(i, j - 1))
                  d(i, j) = (d(i, j)/a(i, j) - d(i, j - 1))/(1.0_SP/a(i, j) - c(i, j - 1))
               end if
            end do
         end do
         if (grid%jproc < grid%ny_proc - 1) then
            do i = i0, i1
               smsg(1, i) = d(i, lp%je)
               smsg(2, i) = c(i, lp%je)
            end do
            call MPI_Isend(smsg(:, i0:i1), 2*(i1 - i0 + 1), MPI_SP, grid%left_rank, 0, &
                           grid%cart_comm, sreq(ic), ierr)
         end if
      end do
      if (grid%jproc < grid%ny_proc - 1) &
         call MPI_Waitall(nc, sreq(1:nc), MPI_STATUSES_IGNORE, ierr)

      ! --- back substitution, chunk-pipelined ---
      if (grid%jproc < grid%ny_proc - 1) then
         do ic = 1, nc
            i0 = lp%ib + (ic - 1)*w
            i1 = min(i0 + w - 1, lp%ie)
            call MPI_Irecv(rbk(i0:i1), i1 - i0 + 1, MPI_SP, grid%left_rank, 1, &
                           grid%cart_comm, rreq(ic), ierr)
         end do
      end if

      do ic = 1, nc
         i0 = lp%ib + (ic - 1)*w
         i1 = min(i0 + w - 1, lp%ie)
         if (grid%jproc < grid%ny_proc - 1) then
            call MPI_Wait(rreq(ic), stat, ierr)
            do i = i0, i1
               f(i, lp%je) = d(i, lp%je) - c(i, lp%je)*rbk(i)
            end do
         else
            do i = i0, i1
               f(i, lp%je) = d(i, lp%je)
            end do
         end if
         do j = lp%je - 1, lp%jb, -1
            do i = i0, i1
               f(i, j) = d(i, j) - c(i, j)*f(i, j + 1)
            end do
         end do
         if (grid%jproc > 0) then
            do i = i0, i1
               sbk(i) = f(i, lp%jb)
            end do
            call MPI_Isend(sbk(i0:i1), i1 - i0 + 1, MPI_SP, grid%right_rank, 1, &
                           grid%cart_comm, sreq(ic), ierr)
         end if
      end do
      if (grid%jproc > 0) &
         call MPI_Waitall(nc, sreq(1:nc), MPI_STATUSES_IGNORE, ierr)

   end subroutine trid_y

   ! ----------------------------------------------------------------
   ! trid_y2 — two-RHS chunk-pipelined Thomas in y, one shared
   ! elimination.  y analogue of trid_x2; sweeps unthreaded as in
   ! trid_y (see the code-shape note there).
   ! ----------------------------------------------------------------
   subroutine trid_y2(lp, grid, a, c, d1, d2, f1, f2)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: a(:, :)
      real(SP), intent(inout) :: c(:, :), d1(:, :), d2(:, :)
      real(SP), intent(out)   :: f1(:, :), f2(:, :)

      real(SP)          :: smsg(3, lp%mloc), rmsg(3, lp%mloc)
      real(SP)          :: sbk(2, lp%mloc), rbk(2, lp%mloc)
      type(MPI_Request) :: rreq(lp%mloc), sreq(lp%mloc)
      type(MPI_Status)  :: stat
      integer           :: i, j, ierr, nt, w, nc, ic, i0, i1

      if (grid%ny_proc == 1) then
         ! single-rank chain: plain full-width Thomas, no messages
         do j = lp%jb + 1, lp%je
            do i = lp%ib, lp%ie
               if (a(i, j) /= 0.0_SP) then
                  c(i, j) = c(i, j)/a(i, j)/(1.0_SP/a(i, j) - c(i, j - 1))
                  d1(i, j) = (d1(i, j)/a(i, j) - d1(i, j - 1))/(1.0_SP/a(i, j) - c(i, j - 1))
                  d2(i, j) = (d2(i, j)/a(i, j) - d2(i, j - 1))/(1.0_SP/a(i, j) - c(i, j - 1))
               end if
            end do
         end do
         do i = lp%ib, lp%ie
            f1(i, lp%je) = d1(i, lp%je)
            f2(i, lp%je) = d2(i, lp%je)
         end do
         do j = lp%je - 1, lp%jb, -1
            do i = lp%ib, lp%ie
               f1(i, j) = d1(i, j) - c(i, j)*f1(i, j + 1)
               f2(i, j) = d2(i, j) - c(i, j)*f2(i, j + 1)
            end do
         end do
         return
      end if

      if (grid%ny_proc >= trid_transpose_min_py) then
         call trid_y_alltoall(lp, grid, a, c, d1, f1, d2, f2)
         return
      end if

      nt = lp%ie - lp%ib + 1
      w = min(trid_chunk, nt)
      nc = (nt + w - 1)/w

      ! --- forward sweep, chunk-pipelined ---
      if (grid%jproc > 0) then
         do ic = 1, nc
            i0 = lp%ib + (ic - 1)*w
            i1 = min(i0 + w - 1, lp%ie)
            call MPI_Irecv(rmsg(:, i0:i1), 3*(i1 - i0 + 1), MPI_SP, grid%right_rank, 0, &
                           grid%cart_comm, rreq(ic), ierr)
         end do
      end if

      do ic = 1, nc
         i0 = lp%ib + (ic - 1)*w
         i1 = min(i0 + w - 1, lp%ie)
         if (grid%jproc > 0) then
            call MPI_Wait(rreq(ic), stat, ierr)
            do i = i0, i1
               if (a(i, lp%jb) /= 0.0_SP) then
                  c(i, lp%jb) = c(i, lp%jb)/a(i, lp%jb) &
                                /(1.0_SP/a(i, lp%jb) - rmsg(3, i))
                  d1(i, lp%jb) = (d1(i, lp%jb)/a(i, lp%jb) - rmsg(1, i)) &
                                 /(1.0_SP/a(i, lp%jb) - rmsg(3, i))
                  d2(i, lp%jb) = (d2(i, lp%jb)/a(i, lp%jb) - rmsg(2, i)) &
                                 /(1.0_SP/a(i, lp%jb) - rmsg(3, i))
               end if
            end do
         end if
         ! unthreaded sweep — see the code-shape note in trid_y
         do j = lp%jb + 1, lp%je
            do i = i0, i1
               if (a(i, j) /= 0.0_SP) then
                  c(i, j) = c(i, j)/a(i, j)/(1.0_SP/a(i, j) - c(i, j - 1))
                  d1(i, j) = (d1(i, j)/a(i, j) - d1(i, j - 1))/(1.0_SP/a(i, j) - c(i, j - 1))
                  d2(i, j) = (d2(i, j)/a(i, j) - d2(i, j - 1))/(1.0_SP/a(i, j) - c(i, j - 1))
               end if
            end do
         end do
         if (grid%jproc < grid%ny_proc - 1) then
            do i = i0, i1
               smsg(1, i) = d1(i, lp%je)
               smsg(2, i) = d2(i, lp%je)
               smsg(3, i) = c(i, lp%je)
            end do
            call MPI_Isend(smsg(:, i0:i1), 3*(i1 - i0 + 1), MPI_SP, grid%left_rank, 0, &
                           grid%cart_comm, sreq(ic), ierr)
         end if
      end do
      if (grid%jproc < grid%ny_proc - 1) &
         call MPI_Waitall(nc, sreq(1:nc), MPI_STATUSES_IGNORE, ierr)

      ! --- back substitution, chunk-pipelined ---
      if (grid%jproc < grid%ny_proc - 1) then
         do ic = 1, nc
            i0 = lp%ib + (ic - 1)*w
            i1 = min(i0 + w - 1, lp%ie)
            call MPI_Irecv(rbk(:, i0:i1), 2*(i1 - i0 + 1), MPI_SP, grid%left_rank, 1, &
                           grid%cart_comm, rreq(ic), ierr)
         end do
      end if

      do ic = 1, nc
         i0 = lp%ib + (ic - 1)*w
         i1 = min(i0 + w - 1, lp%ie)
         if (grid%jproc < grid%ny_proc - 1) then
            call MPI_Wait(rreq(ic), stat, ierr)
            do i = i0, i1
               f1(i, lp%je) = d1(i, lp%je) - c(i, lp%je)*rbk(1, i)
               f2(i, lp%je) = d2(i, lp%je) - c(i, lp%je)*rbk(2, i)
            end do
         else
            do i = i0, i1
               f1(i, lp%je) = d1(i, lp%je)
               f2(i, lp%je) = d2(i, lp%je)
            end do
         end if
         do j = lp%je - 1, lp%jb, -1
            do i = i0, i1
               f1(i, j) = d1(i, j) - c(i, j)*f1(i, j + 1)
               f2(i, j) = d2(i, j) - c(i, j)*f2(i, j + 1)
            end do
         end do
         if (grid%jproc > 0) then
            do i = i0, i1
               sbk(1, i) = f1(i, lp%jb)
               sbk(2, i) = f2(i, lp%jb)
            end do
            call MPI_Isend(sbk(:, i0:i1), 2*(i1 - i0 + 1), MPI_SP, grid%right_rank, 1, &
                           grid%cart_comm, sreq(ic), ierr)
         end if
      end do
      if (grid%jproc > 0) &
         call MPI_Waitall(nc, sreq(1:nc), MPI_STATUSES_IGNORE, ierr)

   end subroutine trid_y2

   ! ----------------------------------------------------------------
   ! trid_y_alltoall — transpose form of the distributed y solve.
   ! Column all-to-all gathers full y-lines (each rank takes a near-
   ! even share of the x-columns), one serial Thomas per line, second
   ! all-to-all scatters the solutions back.  Trades the alpha*PY
   ! latency ladder of the pipelined chain for pure bandwidth — the
   ! 8n trid_y datum (ctband 312020).  Per-line float sequence matches
   ! the pipelined recurrence exactly (rank seams just split rows), so
   ! the path is bitwise vs trid_y/trid_y2.  The pack loops ARE the
   ! transpose: send side packs y-fastest per destination, unpack
   ! lands y-contiguous line storage — no standalone transpose pass.
   ! Line solves unthreaded (ifx code-shape lesson, trid_y note).
   ! d2/f2 present = the two-RHS Sherman-Morrison batch.
   ! FUTURE: persist the buffers on type_trid_workspace.
   ! ----------------------------------------------------------------
   subroutine trid_y_alltoall(lp, grid, a, c, d1, f1, d2, f2)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: a(:, :)
      real(SP), intent(inout) :: c(:, :), d1(:, :)
      real(SP), intent(out)   :: f1(:, :)
      real(SP), intent(inout), optional :: d2(:, :)
      real(SP), intent(out), optional :: f2(:, :)

      integer, allocatable :: scnt(:), sdsp(:), rcnt(:), rdsp(:)
      integer :: py, me, mx, ny, nyg, nrhs, nfin, w_me
      integer :: r, x, jj, pos, base, rem, ierr, sbn, rbn

      py = grid%ny_proc
      me = grid%jproc
      mx = lp%ie - lp%ib + 1
      ny = lp%je - lp%jb + 1
      nrhs = 1
      if (present(d2)) nrhs = 2
      nfin = 2 + nrhs

      ! x-column shares (near-even) and per-rank y extents — geometry
      ! only, so derived once and cached (skips the per-call Allgather)
      if (py /= ts_key_py .or. mx /= ts_key_mx .or. ny /= ts_key_ny) then
         if (allocated(ts_wq)) deallocate (ts_wq, ts_x0, ts_nyp, ts_yoff)
         allocate (ts_wq(0:py - 1), ts_x0(0:py - 1), ts_nyp(0:py - 1), ts_yoff(0:py - 1))
         base = mx/py
         rem = mod(mx, py)
         do r = 0, py - 1
            ts_wq(r) = base
            if (r < rem) ts_wq(r) = ts_wq(r) + 1
         end do
         ts_x0(0) = 0
         do r = 1, py - 1
            ts_x0(r) = ts_x0(r - 1) + ts_wq(r - 1)
         end do
         call MPI_Allgather(ny, 1, MPI_INTEGER, ts_nyp, 1, MPI_INTEGER, &
                            grid%col_comm, ierr)
         ts_yoff(0) = 0
         do r = 1, py - 1
            ts_yoff(r) = ts_yoff(r - 1) + ts_nyp(r - 1)
         end do
         ts_nyg = ts_yoff(py - 1) + ts_nyp(py - 1)
         ts_wme = ts_wq(me)
         ts_key_py = py; ts_key_mx = mx; ts_key_ny = ny
      end if
      nyg = ts_nyg
      w_me = ts_wme

      allocate (scnt(0:py - 1), sdsp(0:py - 1), rcnt(0:py - 1), rdsp(0:py - 1))

      ! --- forward all-to-all: a, c, d1[, d2] slabs -> full lines ---
      do r = 0, py - 1
         scnt(r) = nfin*ts_wq(r)*ny
         rcnt(r) = nfin*w_me*ts_nyp(r)
      end do
      sdsp(0) = 0; rdsp(0) = 0
      do r = 1, py - 1
         sdsp(r) = sdsp(r - 1) + scnt(r - 1)
         rdsp(r) = rdsp(r - 1) + rcnt(r - 1)
      end do

      sbn = max(nfin*mx*ny, nrhs*w_me*nyg)
      rbn = max(nfin*w_me*nyg, nrhs*mx*ny)
      if (.not. allocated(ts_sbuf) .or. size(ts_sbuf) < sbn) then
         if (allocated(ts_sbuf)) deallocate (ts_sbuf)
         allocate (ts_sbuf(sbn))
      end if
      if (.not. allocated(ts_rbuf) .or. size(ts_rbuf) < rbn) then
         if (allocated(ts_rbuf)) deallocate (ts_rbuf)
         allocate (ts_rbuf(rbn))
      end if
      if (.not. allocated(ts_la) .or. size(ts_la, 1) < nyg .or. size(ts_la, 2) < w_me) then
         if (allocated(ts_la)) deallocate (ts_la, ts_lc, ts_l1)
         allocate (ts_la(nyg, w_me), ts_lc(nyg, w_me), ts_l1(nyg, w_me))
      end if
      if (nrhs == 2 .and. (.not. allocated(ts_l2) .or. size(ts_l2, 1) < nyg &
                           .or. size(ts_l2, 2) < w_me)) then
         if (allocated(ts_l2)) deallocate (ts_l2)
         allocate (ts_l2(nyg, w_me))
      end if
      pos = 0
      do r = 0, py - 1
         call pack_slab(a, ts_x0(r), ts_wq(r))
         call pack_slab(c, ts_x0(r), ts_wq(r))
         call pack_slab(d1, ts_x0(r), ts_wq(r))
         if (nrhs == 2) call pack_slab(d2, ts_x0(r), ts_wq(r))
      end do
      call MPI_Alltoallv(ts_sbuf, scnt, sdsp, MPI_SP, ts_rbuf, rcnt, rdsp, MPI_SP, &
                         grid%col_comm, ierr)

      pos = 0
      do r = 0, py - 1
         call unpack_lines(ts_la, ts_yoff(r), ts_nyp(r))
         call unpack_lines(ts_lc, ts_yoff(r), ts_nyp(r))
         call unpack_lines(ts_l1, ts_yoff(r), ts_nyp(r))
         if (nrhs == 2) call unpack_lines(ts_l2, ts_yoff(r), ts_nyp(r))
      end do

      ! --- one serial Thomas per line; back-sub in place (l -> f).
      ! Same fused sweep as trid_y/trid_y2: within a row the d updates
      ! read the PREVIOUS row's eliminated c, so both RHS share one
      ! elimination bitwise ---
      if (nrhs == 2) then
         do x = 1, w_me
            do jj = 2, nyg
               if (ts_la(jj, x) /= 0.0_SP) then
                  ts_lc(jj, x) = ts_lc(jj, x)/ts_la(jj, x)/(1.0_SP/ts_la(jj, x) - ts_lc(jj - 1, x))
                  ts_l1(jj, x) = (ts_l1(jj, x)/ts_la(jj, x) - ts_l1(jj - 1, x))/(1.0_SP/ts_la(jj, x) - ts_lc(jj - 1, x))
                  ts_l2(jj, x) = (ts_l2(jj, x)/ts_la(jj, x) - ts_l2(jj - 1, x))/(1.0_SP/ts_la(jj, x) - ts_lc(jj - 1, x))
               end if
            end do
            do jj = nyg - 1, 1, -1
               ts_l1(jj, x) = ts_l1(jj, x) - ts_lc(jj, x)*ts_l1(jj + 1, x)
               ts_l2(jj, x) = ts_l2(jj, x) - ts_lc(jj, x)*ts_l2(jj + 1, x)
            end do
         end do
      else
         do x = 1, w_me
            do jj = 2, nyg
               if (ts_la(jj, x) /= 0.0_SP) then
                  ts_lc(jj, x) = ts_lc(jj, x)/ts_la(jj, x)/(1.0_SP/ts_la(jj, x) - ts_lc(jj - 1, x))
                  ts_l1(jj, x) = (ts_l1(jj, x)/ts_la(jj, x) - ts_l1(jj - 1, x))/(1.0_SP/ts_la(jj, x) - ts_lc(jj - 1, x))
               end if
            end do
            do jj = nyg - 1, 1, -1
               ts_l1(jj, x) = ts_l1(jj, x) - ts_lc(jj, x)*ts_l1(jj + 1, x)
            end do
         end do
      end if

      ! --- return all-to-all: solved lines -> owner slabs ---
      do r = 0, py - 1
         scnt(r) = nrhs*w_me*ts_nyp(r)
         rcnt(r) = nrhs*ts_wq(r)*ny
      end do
      sdsp(0) = 0; rdsp(0) = 0
      do r = 1, py - 1
         sdsp(r) = sdsp(r - 1) + scnt(r - 1)
         rdsp(r) = rdsp(r - 1) + rcnt(r - 1)
      end do
      pos = 0
      do r = 0, py - 1
         call pack_lines(ts_l1, ts_yoff(r), ts_nyp(r))
         if (nrhs == 2) call pack_lines(ts_l2, ts_yoff(r), ts_nyp(r))
      end do
      call MPI_Alltoallv(ts_sbuf, scnt, sdsp, MPI_SP, ts_rbuf, rcnt, rdsp, MPI_SP, &
                         grid%col_comm, ierr)
      pos = 0
      do r = 0, py - 1
         call unpack_slab(f1, ts_x0(r), ts_wq(r))
         if (nrhs == 2) call unpack_slab(f2, ts_x0(r), ts_wq(r))
      end do

   contains

      subroutine pack_slab(fld, xoff, w)
         real(SP), intent(in) :: fld(:, :)
         integer, intent(in) :: xoff, w
         integer :: xx, j2
         do xx = 1, w
            do j2 = 1, ny
               pos = pos + 1
               ts_sbuf(pos) = fld(lp%ib - 1 + xoff + xx, lp%jb - 1 + j2)
            end do
         end do
      end subroutine pack_slab

      subroutine unpack_lines(lf, yo, nyr)
         real(SP), intent(inout) :: lf(:, :)
         integer, intent(in) :: yo, nyr
         integer :: xx, j2
         do xx = 1, w_me
            do j2 = 1, nyr
               pos = pos + 1
               lf(yo + j2, xx) = ts_rbuf(pos)
            end do
         end do
      end subroutine unpack_lines

      subroutine pack_lines(lf, yo, nyr)
         real(SP), intent(in) :: lf(:, :)
         integer, intent(in) :: yo, nyr
         integer :: xx, j2
         do xx = 1, w_me
            do j2 = 1, nyr
               pos = pos + 1
               ts_sbuf(pos) = lf(yo + j2, xx)
            end do
         end do
      end subroutine pack_lines

      subroutine unpack_slab(fld, xoff, w)
         real(SP), intent(inout) :: fld(:, :)
         integer, intent(in) :: xoff, w
         integer :: xx, j2
         do xx = 1, w
            do j2 = 1, ny
               pos = pos + 1
               fld(lp%ib - 1 + xoff + xx, lp%jb - 1 + j2) = ts_rbuf(pos)
            end do
         end do
      end subroutine unpack_slab

   end subroutine trid_y_alltoall

   ! ----------------------------------------------------------------
   ! trid_x_periodic — Sherman-Morrison for x-periodic BC.
   ! Off-diagonal corners: a(ib,j) couples last→first row (west wrap);
   !                       c(ie,j) couples first→last row (east wrap).
   ! Based on Thomas (1995) Sec. 5.6.1, x-direction analogue of trid_y_periodic.
   ! ----------------------------------------------------------------
   subroutine trid_x_periodic(lp, grid, a, c, d, ws, f)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: a(:, :)
      real(SP), intent(inout) :: c(:, :), d(:, :)
      type(type_trid_workspace), intent(inout) :: ws
      real(SP), intent(out)   :: f(:, :)

      real(SP) :: a_beg(lp%nloc), c_end(lp%nloc)
      real(SP) :: y1_end(lp%nloc), y2_end(lp%nloc), beta(lp%nloc)
      integer  :: west_rank, east_rank
      integer  :: j, k, ierr
      type(MPI_Status) :: stat

      associate (a_loc => ws%a_loc, c1 => ws%c1, &
                 d1 => ws%d1, d2 => ws%d2, y1 => ws%y1, y2 => ws%y2)

         ! fused workspace fill: one threaded pass instead of four serial
         ! whole-array sweeps (3 copies + the Step-3 d2 zero) — pure
         ! bandwidth, hit every stage on periodic decks
         !$omp parallel do default(shared) schedule(static) private(k)
         do j = 1, ws%n
            do k = 1, ws%m
               a_loc(k, j) = a(k, j)
               c1(k, j) = c(k, j)
               d1(k, j) = d(k, j)
               d2(k, j) = 0.0_SP
            end do
         end do
         !$omp end parallel do

         ! --- Step 1: exchange boundary off-diagonal values ---
         call MPI_Cart_rank(grid%cart_comm, [0, grid%jproc], west_rank, ierr)
         call MPI_Cart_rank(grid%cart_comm, [grid%nx_proc - 1, grid%jproc], east_rank, ierr)

         if (grid%nx_proc == 1) then
            a_beg = a_loc(lp%ib, :)
            c_end = c1(lp%ie, :)
         else
            ! Sendrecv — see the deadlock note in trid_y_periodic
            if (grid%iproc == 0) then
               a_beg = a_loc(lp%ib, :)
               call MPI_Sendrecv(a_beg, lp%nloc, MPI_SP, east_rank, 20, &
                                 c_end, lp%nloc, MPI_SP, east_rank, 21, &
                                 grid%cart_comm, stat, ierr)
            end if
            if (grid%iproc == grid%nx_proc - 1) then
               c_end = c1(lp%ie, :)
               call MPI_Sendrecv(c_end, lp%nloc, MPI_SP, west_rank, 21, &
                                 a_beg, lp%nloc, MPI_SP, west_rank, 20, &
                                 grid%cart_comm, stat, ierr)
            end if
         end if

         ! --- Step 2: normalise and save ---
         if (grid%iproc == 0) then
            do j = lp%jb, lp%je
               c1(lp%ib, j) = c1(lp%ib, j)/(1.0_SP + c_end(j))
               d1(lp%ib, j) = d1(lp%ib, j)/(1.0_SP + c_end(j))
            end do
         end if
         if (grid%iproc == grid%nx_proc - 1) then
            do j = lp%jb, lp%je
               a_loc(lp%ie, j) = a_loc(lp%ie, j)/(1.0_SP + a_beg(j))
               d1(lp%ie, j) = d1(lp%ie, j)/(1.0_SP + a_beg(j))
            end do
         end if

         ! --- Step 3: boundary RHS for the correction solve (d2 zeroed above) ---
         if (grid%iproc == 0) then
            do j = lp%jb, lp%je
               d2(lp%ib, j) = 1.0_SP/(1.0_SP + c_end(j))
            end do
         end if
         if (grid%iproc == grid%nx_proc - 1) then
            do j = lp%jb, lp%je
               d2(lp%ie, j) = -1.0_SP/(1.0_SP + a_beg(j))
            end do
         end if

         ! --- Step 4: batched solve B*y1 = d1, B*y2 = d2 ---
         call trid_x2(lp, grid, a_loc, c1, d1, d2, y1, y2)

         ! --- Step 5: gather y1(ie,j) and y2(ie,j) to west rank ---
         if (grid%iproc == grid%nx_proc - 1 .and. grid%nx_proc > 1) then
            y1_end = y1(lp%ie, :)
            y2_end = y2(lp%ie, :)
            call MPI_Send(y1_end, lp%nloc, MPI_SP, west_rank, 22, grid%cart_comm, ierr)
            call MPI_Send(y2_end, lp%nloc, MPI_SP, west_rank, 23, grid%cart_comm, ierr)
         end if
         if (grid%iproc == 0 .and. grid%nx_proc > 1) then
            call MPI_Recv(y1_end, lp%nloc, MPI_SP, east_rank, 22, grid%cart_comm, stat, ierr)
            call MPI_Recv(y2_end, lp%nloc, MPI_SP, east_rank, 23, grid%cart_comm, stat, ierr)
         end if
         if (grid%nx_proc == 1) then
            y1_end = y1(lp%ie, :)
            y2_end = y2(lp%ie, :)
         end if

         ! --- Step 6: west rank computes beta ---
         beta = 0.0_SP
         if (grid%iproc == 0) then
            do j = lp%jb, lp%je
               beta(j) = (c_end(j)*y1(lp%ib, j) - a_beg(j)*y1_end(j)) &
                         /(1.0_SP - (c_end(j)*y2(lp%ib, j) - a_beg(j)*y2_end(j)))
            end do
         end if

         ! --- Step 7: broadcast beta along x-row (same jproc) ---
         ! row_comm root 0 = iproc 0 (Cart_sub keeps iproc ordering);
         ! O(log px) vs the previous serial send loop from the chain end
         if (grid%nx_proc > 1) then
            call MPI_Bcast(beta, lp%nloc, MPI_SP, 0, grid%row_comm, ierr)
         end if

         ! --- Step 8: combine ---
         !$omp parallel do default(shared) schedule(static) private(k)
         do j = lp%jb, lp%je
            do k = lp%ib, lp%ie
               f(k, j) = y1(k, j) + beta(j)*y2(k, j)
            end do
         end do
         !$omp end parallel do

      end associate

   end subroutine trid_x_periodic

   ! ----------------------------------------------------------------
   ! trid_y_periodic — Sherman-Morrison for y-periodic BC.
   ! Off-diagonal corners: a(i,jb) couples last→first row (south wrap);
   !                       c(i,je) couples first→last row (north wrap).
   ! south chain-end rank: jproc == 0; north: jproc == ny_proc - 1
   ! (cart position, valid under the periodic-y cart topology).
   ! ----------------------------------------------------------------
   subroutine trid_y_periodic(lp, grid, a, c, d, ws, f)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: a(:, :)
      real(SP), intent(inout) :: c(:, :), d(:, :)
      type(type_trid_workspace), intent(inout) :: ws
      real(SP), intent(out)   :: f(:, :)

      real(SP) :: a_beg(lp%mloc), c_end(lp%mloc)
      real(SP) :: y1_end(lp%mloc), y2_end(lp%mloc), beta(lp%mloc)
      integer  :: south_rank, north_rank
      integer  :: i, j, k, ierr
      type(MPI_Status) :: stat

      associate (a_loc => ws%a_loc, c1 => ws%c1, &
                 d1 => ws%d1, d2 => ws%d2, y1 => ws%y1, y2 => ws%y2)

         ! fused workspace fill — see the note in trid_x_periodic
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = 1, ws%n
            do i = 1, ws%m
               a_loc(i, j) = a(i, j)
               c1(i, j) = c(i, j)
               d1(i, j) = d(i, j)
               d2(i, j) = 0.0_SP
            end do
         end do
         !$omp end parallel do

         ! --- Step 1: exchange boundary off-diagonal values ---
         call MPI_Cart_rank(grid%cart_comm, [grid%iproc, 0], south_rank, ierr)
         call MPI_Cart_rank(grid%cart_comm, [grid%iproc, grid%ny_proc - 1], north_rank, ierr)

         if (grid%ny_proc == 1) then
            a_beg = a_loc(:, lp%jb)
            c_end = c1(:, lp%je)
         else
            ! Sendrecv: the send-then-recv ordering on both chain ends
            ! deadlocks once mloc exceeds the eager threshold (latent
            ! in legacy too — small tiles never rendezvous)
            if (grid%jproc == 0) then   ! southernmost (jproc=0)
               a_beg = a_loc(:, lp%jb)
               call MPI_Sendrecv(a_beg, lp%mloc, MPI_SP, north_rank, 30, &
                                 c_end, lp%mloc, MPI_SP, north_rank, 31, &
                                 grid%cart_comm, stat, ierr)
            end if
            if (grid%jproc == grid%ny_proc - 1) then    ! northernmost (jproc=PY-1)
               c_end = c1(:, lp%je)
               call MPI_Sendrecv(c_end, lp%mloc, MPI_SP, south_rank, 31, &
                                 a_beg, lp%mloc, MPI_SP, south_rank, 30, &
                                 grid%cart_comm, stat, ierr)
            end if
         end if

         ! --- Step 2: normalise and save ---
         if (grid%jproc == 0) then
            do i = lp%ib, lp%ie
               c1(i, lp%jb) = c1(i, lp%jb)/(1.0_SP + c_end(i))
               d1(i, lp%jb) = d1(i, lp%jb)/(1.0_SP + c_end(i))
            end do
         end if
         if (grid%jproc == grid%ny_proc - 1) then
            do i = lp%ib, lp%ie
               a_loc(i, lp%je) = a_loc(i, lp%je)/(1.0_SP + a_beg(i))
               d1(i, lp%je) = d1(i, lp%je)/(1.0_SP + a_beg(i))
            end do
         end if

         ! --- Step 3: boundary RHS for the correction solve (d2 zeroed above) ---
         if (grid%jproc == 0) then
            do i = lp%ib, lp%ie
               d2(i, lp%jb) = 1.0_SP/(1.0_SP + c_end(i))
            end do
         end if
         if (grid%jproc == grid%ny_proc - 1) then
            do i = lp%ib, lp%ie
               d2(i, lp%je) = -1.0_SP/(1.0_SP + a_beg(i))
            end do
         end if

         ! --- Step 4: batched solve B*y1 = d1, B*y2 = d2 ---
         call trid_y2(lp, grid, a_loc, c1, d1, d2, y1, y2)

         ! --- Step 5: gather y1(i,je) and y2(i,je) to south rank ---
         if (grid%jproc == grid%ny_proc - 1 .and. grid%ny_proc > 1) then
            y1_end = y1(:, lp%je)
            y2_end = y2(:, lp%je)
            call MPI_Send(y1_end, lp%mloc, MPI_SP, south_rank, 32, grid%cart_comm, ierr)
            call MPI_Send(y2_end, lp%mloc, MPI_SP, south_rank, 33, grid%cart_comm, ierr)
         end if
         if (grid%jproc == 0 .and. grid%ny_proc > 1) then
            call MPI_Recv(y1_end, lp%mloc, MPI_SP, north_rank, 32, grid%cart_comm, stat, ierr)
            call MPI_Recv(y2_end, lp%mloc, MPI_SP, north_rank, 33, grid%cart_comm, stat, ierr)
         end if
         if (grid%ny_proc == 1) then
            y1_end = y1(:, lp%je)
            y2_end = y2(:, lp%je)
         end if

         ! --- Step 6: south rank computes beta ---
         beta = 0.0_SP
         if (grid%jproc == 0) then
            do i = lp%ib, lp%ie
               beta(i) = (c_end(i)*y1(i, lp%jb) - a_beg(i)*y1_end(i)) &
                         /(1.0_SP - (c_end(i)*y2(i, lp%jb) - a_beg(i)*y2_end(i)))
            end do
         end if

         ! --- Step 7: broadcast beta along y-column (same iproc) ---
         ! col_comm root 0 = jproc 0 (Cart_sub keeps jproc ordering);
         ! O(log py) vs the previous serial send loop from the chain end
         if (grid%ny_proc > 1) then
            call MPI_Bcast(beta, lp%mloc, MPI_SP, 0, grid%col_comm, ierr)
         end if

         ! --- Step 8: combine ---
         !$omp parallel do default(shared) schedule(static) private(i)
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               f(i, j) = y1(i, j) + beta(i)*y2(i, j)
            end do
         end do
         !$omp end parallel do

      end associate

   end subroutine trid_y_periodic

end module core_solver_tridiag_mod
