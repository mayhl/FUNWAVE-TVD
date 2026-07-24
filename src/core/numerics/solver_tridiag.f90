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

   ! Scratch for the periodic (Sherman-Morrison) solves: two coefficient
   ! copies plus the two auxiliary solutions.  Allocated once by the caller
   ! (mloc×nloc), reused every stage/step — no per-call heap traffic.
   type, public :: type_trid_workspace
      integer :: m = 0, n = 0
      real(SP), allocatable :: a_loc(:, :)
      real(SP), allocatable :: c1(:, :), c2(:, :)
      real(SP), allocatable :: d1(:, :), d2(:, :)
      real(SP), allocatable :: y1(:, :), y2(:, :)
   contains
      procedure :: alloc => tws_alloc
      procedure :: free => tws_free
   end type type_trid_workspace

contains

   subroutine tws_alloc(ws, m, n)
      class(type_trid_workspace), intent(inout) :: ws
      integer, intent(in) :: m, n
      ws%m = m; ws%n = n
      allocate (ws%a_loc(m, n), &
                ws%c1(m, n), ws%c2(m, n), &
                ws%d1(m, n), ws%d2(m, n), &
                ws%y1(m, n), ws%y2(m, n))
   end subroutine tws_alloc

   subroutine tws_free(ws)
      class(type_trid_workspace), intent(inout) :: ws
      ws%m = 0; ws%n = 0
      deallocate (ws%a_loc, ws%c1, ws%c2, ws%d1, ws%d2, ws%y1, ws%y2)
   end subroutine tws_free

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
   ! trid_x — MPI pipeline Thomas in x.
   ! Forward sweep west→east; back-sub east→west.
   ! west neighbor = grid%back_rank, east = grid%shore_rank.
   ! Chain endpoints come from the cart POSITION (iproc), never from
   ! neighbor nullity: under a periodic cart topology the wrap makes
   ! every rank have neighbors, but the sweep is still linear.
   ! Works for PX=1 (single x-rank, exchanges skipped).
   ! c and d are overwritten during elimination; a is read-only.
   ! ----------------------------------------------------------------
   subroutine trid_x(lp, grid, a, c, d, f)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: a(:, :)
      real(SP), intent(inout) :: c(:, :), d(:, :)
      real(SP), intent(out)   :: f(:, :)

      real(SP)          :: smsg(lp%nloc, 2), rmsg(lp%nloc, 2)
      type(MPI_Request) :: req
      type(MPI_Status)  :: stat
      integer           :: i, j, ierr

      ! --- forward sweep ---
      if (grid%iproc > 0) then
         call MPI_Irecv(rmsg, 2*lp%nloc, MPI_SP, grid%back_rank, 0, grid%cart_comm, req, ierr)
         call MPI_Wait(req, stat, ierr)
         do j = lp%jb, lp%je
            if (a(lp%ib, j) /= 0.0_SP) then
               c(lp%ib, j) = c(lp%ib, j)/a(lp%ib, j) &
                             /(1.0_SP/a(lp%ib, j) - rmsg(j, 2))
               d(lp%ib, j) = (d(lp%ib, j)/a(lp%ib, j) - rmsg(j, 1)) &
                             /(1.0_SP/a(lp%ib, j) - rmsg(j, 2))
            end if
         end do
      end if

      ! recurrence runs along i, rows independent — thread over j
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

      if (grid%iproc < grid%nx_proc - 1) then
         do j = lp%jb, lp%je
            smsg(j, 1) = d(lp%ie, j)
            smsg(j, 2) = c(lp%ie, j)
         end do
         call MPI_Isend(smsg, 2*lp%nloc, MPI_SP, grid%shore_rank, 0, grid%cart_comm, req, ierr)
         call MPI_Wait(req, stat, ierr)
      end if

      ! --- back substitution ---
      if (grid%iproc < grid%nx_proc - 1) then
         call MPI_Irecv(rmsg, 2*lp%nloc, MPI_SP, grid%shore_rank, 1, grid%cart_comm, req, ierr)
         call MPI_Wait(req, stat, ierr)
         do j = lp%jb, lp%je
            f(lp%ie, j) = d(lp%ie, j) - c(lp%ie, j)*rmsg(j, 1)
         end do
      else
         do j = lp%jb, lp%je
            f(lp%ie, j) = d(lp%ie, j)
         end do
      end if

      !$omp parallel do default(shared) schedule(static) private(i)
      do j = lp%jb, lp%je
         do i = lp%ie - 1, lp%ib, -1
            f(i, j) = d(i, j) - c(i, j)*f(i + 1, j)
         end do
      end do
      !$omp end parallel do

      if (grid%iproc > 0) then
         do j = lp%jb, lp%je
            smsg(j, 1) = f(lp%ib, j)
         end do
         call MPI_Isend(smsg, 2*lp%nloc, MPI_SP, grid%back_rank, 1, grid%cart_comm, req, ierr)
         call MPI_Wait(req, stat, ierr)
      end if

   end subroutine trid_x

   ! ----------------------------------------------------------------
   ! trid_y — MPI pipeline Thomas in y.
   ! Forward sweep south→north; back-sub north→south.
   ! south neighbor = grid%right_rank, north = grid%left_rank.
   ! Chain endpoints from the cart position (jproc), as in trid_x —
   ! required for periodic-y cart topologies (Sherman-Morrison callers).
   ! ----------------------------------------------------------------
   subroutine trid_y(lp, grid, a, c, d, f)
      type(type_loop_bounds), intent(in)    :: lp
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: a(:, :)
      real(SP), intent(inout) :: c(:, :), d(:, :)
      real(SP), intent(out)   :: f(:, :)

      real(SP)          :: smsg(lp%mloc, 2), rmsg(lp%mloc, 2)
      type(MPI_Request) :: req
      type(MPI_Status)  :: stat
      integer           :: i, j, ierr

      ! --- forward sweep ---
      if (grid%jproc > 0) then
         call MPI_Irecv(rmsg, 2*lp%mloc, MPI_SP, grid%right_rank, 0, grid%cart_comm, req, ierr)
         call MPI_Wait(req, stat, ierr)
         do i = lp%ib, lp%ie
            if (a(i, lp%jb) /= 0.0_SP) then
               c(i, lp%jb) = c(i, lp%jb)/a(i, lp%jb) &
                             /(1.0_SP/a(i, lp%jb) - rmsg(i, 2))
               d(i, lp%jb) = (d(i, lp%jb)/a(i, lp%jb) - rmsg(i, 1)) &
                             /(1.0_SP/a(i, lp%jb) - rmsg(i, 2))
            end if
         end do
      end if

      ! NOT OMP-threaded: the j recurrence bars the sweep loop, and the
      ! i-slab variant (each thread sweeping its own column range) cost
      ! ~5% serial under ifx — code-shape regression, wheat A/B 310901
      ! vs 310916; columns stay a GPU-pass target
      do j = lp%jb + 1, lp%je
         do i = lp%ib, lp%ie
            if (a(i, j) /= 0.0_SP) then
               c(i, j) = c(i, j)/a(i, j)/(1.0_SP/a(i, j) - c(i, j - 1))
               d(i, j) = (d(i, j)/a(i, j) - d(i, j - 1))/(1.0_SP/a(i, j) - c(i, j - 1))
            end if
         end do
      end do

      if (grid%jproc < grid%ny_proc - 1) then
         do i = lp%ib, lp%ie
            smsg(i, 1) = d(i, lp%je)
            smsg(i, 2) = c(i, lp%je)
         end do
         call MPI_Isend(smsg, 2*lp%mloc, MPI_SP, grid%left_rank, 0, grid%cart_comm, req, ierr)
         call MPI_Wait(req, stat, ierr)
      end if

      ! --- back substitution ---
      if (grid%jproc < grid%ny_proc - 1) then
         call MPI_Irecv(rmsg, 2*lp%mloc, MPI_SP, grid%left_rank, 1, grid%cart_comm, req, ierr)
         call MPI_Wait(req, stat, ierr)
         do i = lp%ib, lp%ie
            f(i, lp%je) = d(i, lp%je) - c(i, lp%je)*rmsg(i, 1)
         end do
      else
         do i = lp%ib, lp%ie
            f(i, lp%je) = d(i, lp%je)
         end do
      end if

      do j = lp%je - 1, lp%jb, -1
         do i = lp%ib, lp%ie
            f(i, j) = d(i, j) - c(i, j)*f(i, j + 1)
         end do
      end do

      if (grid%jproc > 0) then
         do i = lp%ib, lp%ie
            smsg(i, 1) = f(i, lp%jb)
         end do
         call MPI_Isend(smsg, 2*lp%mloc, MPI_SP, grid%right_rank, 1, grid%cart_comm, req, ierr)
         call MPI_Wait(req, stat, ierr)
      end if

   end subroutine trid_y

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
      integer  :: west_rank, east_rank, dest_rank
      integer  :: j, k, ierr
      type(MPI_Status) :: stat

      associate (a_loc => ws%a_loc, c1 => ws%c1, c2 => ws%c2, &
                 d1 => ws%d1, d2 => ws%d2, y1 => ws%y1, y2 => ws%y2)

         a_loc = a
         c1 = c
         d1 = d

         ! --- Step 1: exchange boundary off-diagonal values ---
         call MPI_Cart_rank(grid%cart_comm, [0, grid%jproc], west_rank, ierr)
         call MPI_Cart_rank(grid%cart_comm, [grid%nx_proc - 1, grid%jproc], east_rank, ierr)

         if (grid%nx_proc == 1) then
            a_beg = a_loc(lp%ib, :)
            c_end = c1(lp%ie, :)
         else
            if (grid%iproc == 0) then
               a_beg = a_loc(lp%ib, :)
               call MPI_Send(a_beg, lp%nloc, MPI_SP, east_rank, 20, grid%cart_comm, ierr)
               call MPI_Recv(c_end, lp%nloc, MPI_SP, east_rank, 21, grid%cart_comm, stat, ierr)
            end if
            if (grid%iproc == grid%nx_proc - 1) then
               c_end = c1(lp%ie, :)
               call MPI_Send(c_end, lp%nloc, MPI_SP, west_rank, 21, grid%cart_comm, ierr)
               call MPI_Recv(a_beg, lp%nloc, MPI_SP, west_rank, 20, grid%cart_comm, stat, ierr)
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

         c2 = c1   ! both solves use the same normalised c

         ! --- Step 3: first solve B*y1 = d1 ---
         call trid_x(lp, grid, a_loc, c1, d1, y1)

         ! --- Step 4: build RHS for second solve ---
         d2 = 0.0_SP
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

         ! --- Step 5: second solve B*y2 = d2 ---
         call trid_x(lp, grid, a_loc, c2, d2, y2)

         ! --- Step 6: gather y1(ie,j) and y2(ie,j) to west rank ---
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

         ! --- Step 7: west rank computes beta ---
         beta = 0.0_SP
         if (grid%iproc == 0) then
            do j = lp%jb, lp%je
               beta(j) = (c_end(j)*y1(lp%ib, j) - a_beg(j)*y1_end(j)) &
                         /(1.0_SP - (c_end(j)*y2(lp%ib, j) - a_beg(j)*y2_end(j)))
            end do
         end if

         ! --- Step 8: broadcast beta along x-row (same jproc) ---
         if (grid%nx_proc > 1) then
            if (grid%iproc == 0) then
               do k = 1, grid%nx_proc - 1
                  call MPI_Cart_rank(grid%cart_comm, [k, grid%jproc], dest_rank, ierr)
                  call MPI_Send(beta, lp%nloc, MPI_SP, dest_rank, 24, grid%cart_comm, ierr)
               end do
            else
               call MPI_Recv(beta, lp%nloc, MPI_SP, west_rank, 24, grid%cart_comm, stat, ierr)
            end if
         end if

         ! --- Step 9: combine ---
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
      integer  :: south_rank, north_rank, dest_rank
      integer  :: i, j, k, ierr
      type(MPI_Status) :: stat

      associate (a_loc => ws%a_loc, c1 => ws%c1, c2 => ws%c2, &
                 d1 => ws%d1, d2 => ws%d2, y1 => ws%y1, y2 => ws%y2)

         a_loc = a
         c1 = c
         d1 = d

         ! --- Step 1: exchange boundary off-diagonal values ---
         call MPI_Cart_rank(grid%cart_comm, [grid%iproc, 0], south_rank, ierr)
         call MPI_Cart_rank(grid%cart_comm, [grid%iproc, grid%ny_proc - 1], north_rank, ierr)

         if (grid%ny_proc == 1) then
            a_beg = a_loc(:, lp%jb)
            c_end = c1(:, lp%je)
         else
            if (grid%jproc == 0) then   ! southernmost (jproc=0)
               a_beg = a_loc(:, lp%jb)
               call MPI_Send(a_beg, lp%mloc, MPI_SP, north_rank, 30, grid%cart_comm, ierr)
               call MPI_Recv(c_end, lp%mloc, MPI_SP, north_rank, 31, grid%cart_comm, stat, ierr)
            end if
            if (grid%jproc == grid%ny_proc - 1) then    ! northernmost (jproc=PY-1)
               c_end = c1(:, lp%je)
               call MPI_Send(c_end, lp%mloc, MPI_SP, south_rank, 31, grid%cart_comm, ierr)
               call MPI_Recv(a_beg, lp%mloc, MPI_SP, south_rank, 30, grid%cart_comm, stat, ierr)
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

         c2 = c1

         ! --- Step 3: first solve B*y1 = d1 ---
         call trid_y(lp, grid, a_loc, c1, d1, y1)

         ! --- Step 4: build RHS for second solve ---
         d2 = 0.0_SP
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

         ! --- Step 5: second solve B*y2 = d2 ---
         call trid_y(lp, grid, a_loc, c2, d2, y2)

         ! --- Step 6: gather y1(i,je) and y2(i,je) to south rank ---
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

         ! --- Step 7: south rank computes beta ---
         beta = 0.0_SP
         if (grid%jproc == 0) then
            do i = lp%ib, lp%ie
               beta(i) = (c_end(i)*y1(i, lp%jb) - a_beg(i)*y1_end(i)) &
                         /(1.0_SP - (c_end(i)*y2(i, lp%jb) - a_beg(i)*y2_end(i)))
            end do
         end if

         ! --- Step 8: broadcast beta along y-column (same iproc) ---
         if (grid%ny_proc > 1) then
            if (grid%jproc == 0) then
               do k = 1, grid%ny_proc - 1
                  call MPI_Cart_rank(grid%cart_comm, [grid%iproc, k], dest_rank, ierr)
                  call MPI_Send(beta, lp%mloc, MPI_SP, dest_rank, 34, grid%cart_comm, ierr)
               end do
            else
               call MPI_Recv(beta, lp%mloc, MPI_SP, south_rank, 34, grid%cart_comm, stat, ierr)
            end if
         end if

         ! --- Step 9: combine ---
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
