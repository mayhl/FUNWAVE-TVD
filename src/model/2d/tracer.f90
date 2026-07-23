!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Lagrangian particle tracking — port of legacy MODULE TRACER
!  (old/mod_tracer.F, built under -DTRACKING).
!
!  Each tracker is located inside a triangle of the grid-point lattice,
!  its velocity is interpolated with barycentric area weights, and it is
!  advanced with a forward-Euler step
!
!    $$ \mathbf{x}^{n+1} = \mathbf{x}^{n} + \mathbf{u}(\mathbf{x}^{n})\,\Delta t $$
!
!  with $ \mathbf{u} = (S_1\mathbf{u}_1 + S_2\mathbf{u}_2 + S_3\mathbf{u}_3)/S_c $,
!  the $S_k$ being the signed sub-triangle areas opposite each vertex.
!  Trackers are one-way: nothing in the hydrodynamics reads them back.
!
!  YAML block: tracer:            (top-level; omit to disable)
!    file: <path>                 required (nee TRACER_FILE)
!
!  The tracker table itself stays in TRACER_FILE, in the legacy layout, so
!  both engines eat the identical file:
!
!    <header line, ignored>
!    <NumTracker>
!    <plot interval, s>
!    <x> <y> <t_start> <layer>     x NumTracker
!
!  Legacy quirks kept:
!    NOTE 1: FIXED (cord cut) — the wet-cell revert now sums all three
!            triangle vertices.  Legacy counted vertex 2 TWICE and vertex 3
!            never (a copy-paste typo in the MASK sum), holding a tracker
!            back only when vertices 1 and 2 were both dry.
!    NOTE 2: a tracker lost by the search is re-searched exactly once, over
!            identical state, so the retry always fails; it then sets
!            stop_search and freezes at its last position for the rest of
!            the run, while still being written out every frame.
!    NOTE 3: velocities are sampled at grid POINTS (legacy Xco/Yco), so the
!            interpolation lattice is the cell-centre lattice — consistent,
!            but it means the last interior point row/column is only covered
!            when a neighbour rank supplies the far vertex.
!    NOTE 4: the output cadence is the PLOT_COUNT dt-accumulator seeded at
!            zero, so — unlike the field preview — the tracker
!            files carry NO initial-condition row.
!    NOTE 5: legacy builds Xco/Yco from DX(1,1) alone, so on a variable-
!            spacing grid the tracker lattice is wrong.  Reproduced: xco/yco
!            below are built from the same corner spacing.
!
!  Legacy quirk NOT kept (see the parity ledger): layers 1 (surface) and 2
!  (bottom) advect on Usurf/Vsurf/Ubott/Vbott, which legacy allocates and
!  never assigns — the tracker rides uninitialised heap.  There is no
!  behaviour there to reproduce, so those layers are rejected at read time.
!  Only layer 0 (depth-averaged u, v) is supported.
!
!  HISTORY :
!    07/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_tracer_mod

   use mpi_f08, only: MPI_Allgather, MPI_Allreduce, MPI_SUM, MPI_INTEGER, &
                      MPI_LOGICAL

   use core_constants_mod, only: SP, ZERO, MPI_SP
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_time_utils_mod, only: type_timing_control

   use model_base_mod, only: type_model_base

   implicit none

   private
   ! locate/move are driven directly by the unit test: the stage order lives
   ! in update(), and tri_area is the geometric primitive both rest on
   public :: type_model_tracer, tracer_locate, tracer_move, tri_area

   type, extends(type_model_base) :: type_model_tracer

      character(:), allocatable :: tracer_file
      character(:), allocatable :: result_folder

      integer  :: n_tracker = 0
      real(SP) :: plot_intv = ZERO

      type(type_timing_control) :: trigger

      ! tracker state
      real(SP), allocatable :: x_track(:), y_track(:)
      real(SP), allocatable :: t_start(:)
      integer, allocatable :: layer(:)

      ! enclosing triangle: vertex grid-point indices and sub-triangle areas
      integer, allocatable :: nx1(:), ny1(:), nx2(:), ny2(:), nx3(:), ny3(:)
      real(SP), allocatable :: sc(:), s1(:), s2(:), s3(:)

      ! previous-step copies, for the dry-cell revert
      real(SP), allocatable :: x_pre(:), y_pre(:)
      integer, allocatable :: nx1_pre(:), ny1_pre(:), nx2_pre(:), ny2_pre(:), &
                              nx3_pre(:), ny3_pre(:)
      real(SP), allocatable :: sc_pre(:), s1_pre(:), s2_pre(:), s3_pre(:)

      logical, allocatable :: in_cell(:), stop_search(:)
      integer, allocatable :: found(:)

      ! ghost-inclusive grid-point coordinates (legacy Xco/Yco)
      real(SP), allocatable :: xco(:), yco(:)

      integer :: unit_x = -1, unit_y = -1
      logical :: is_io_rank = .false.
      logical :: opened = .false.

   contains
      procedure :: read_input => tracer_read_input
      procedure :: init_compute => tracer_init_compute
      procedure :: update => tracer_update
      procedure :: write_output => tracer_write_output
      procedure :: free => tracer_free
   end type type_model_tracer

contains

   subroutine tracer_read_input(this, env)
      class(type_model_tracer), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk

      sub_env = get_sub_env(env, "tracer", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (no_blk) return

      call sub_env%yaml%read("file", val=this%tracer_file, default="")
      if (len_trim(this%tracer_file) == 0) then
         call env%log%exit_on_error( &
            "tracer: the tracer: block requires file")
      end if

   end subroutine tracer_read_input

   ! Legacy TRACER_INITIAL + GET_XY_POSITION: read the tracker table, build
   ! the ghost-inclusive grid-point lattice, and locate every tracker once.
   subroutine tracer_init_compute(this, grid, env, result_folder, t_start_plot)
      class(type_model_tracer), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      type(type_env), intent(inout) :: env
      character(*), intent(in) :: result_folder
      real(SP), intent(in) :: t_start_plot

      character(len=256) :: header
      character(:), allocatable :: folder
      logical :: file_exist
      integer :: i, funit, n
      real(SP) :: dx0, dy0

      if (.not. this%is_activated) return

      inquire (file=trim(this%tracer_file), exist=file_exist)
      if (.not. file_exist) then
         call env%log%exit_on_error( &
            "tracer: file cannot be found: "//trim(this%tracer_file))
      end if

      open (newunit=funit, file=trim(this%tracer_file), action="read")
      read (funit, *) header
      read (funit, *) n
      read (funit, *) this%plot_intv

      if (n <= 0) then
         close (funit)
         call env%log%exit_on_error("tracer: the tracker file declares no trackers")
      end if
      this%n_tracker = n

      allocate (this%x_track(n), this%y_track(n), this%t_start(n), this%layer(n))
      do i = 1, n
         read (funit, *) this%x_track(i), this%y_track(i), &
            this%t_start(i), this%layer(i)
      end do
      close (funit)

      ! Layers 1/2 ride uninitialised memory in legacy — refuse rather than
      ! launder garbage into a parity number (see the header)
      do i = 1, n
         if (this%layer(i) /= 0) then
            call env%log%exit_on_error( &
               "tracer: only layer 0 (depth-averaged) is supported; layers 1 "// &
               "(surface) and 2 (bottom) advect on uninitialised velocities in "// &
               "the legacy code and are not ported")
         end if
      end do

      allocate (this%nx1(n), this%ny1(n), this%nx2(n), this%ny2(n), &
                this%nx3(n), this%ny3(n))
      allocate (this%sc(n), this%s1(n), this%s2(n), this%s3(n))
      allocate (this%x_pre(n), this%y_pre(n))
      allocate (this%nx1_pre(n), this%ny1_pre(n), this%nx2_pre(n), &
                this%ny2_pre(n), this%nx3_pre(n), this%ny3_pre(n))
      allocate (this%sc_pre(n), this%s1_pre(n), this%s2_pre(n), this%s3_pre(n))
      allocate (this%in_cell(n), this%stop_search(n), this%found(n))

      this%nx1 = 0; this%ny1 = 0; this%nx2 = 0; this%ny2 = 0
      this%nx3 = 0; this%ny3 = 0
      this%sc = ZERO; this%s1 = ZERO; this%s2 = ZERO; this%s3 = ZERO
      this%in_cell = .false.
      this%stop_search = .false.
      this%found = 0

      ! Grid-point coordinates over the ghost-inclusive index range: grid%x is
      ! interior-only, but the search needs the far vertex xco(ie+1) whenever a
      ! neighbour rank owns the next point.  Legacy uses the corner spacing for
      ! the whole ramp (NOTE 5).
      associate (lp => grid%lp)
         dx0 = grid%dx(1, 1)
         dy0 = grid%dy(1, 1)

         allocate (this%xco(lp%mloc), this%yco(lp%nloc))

         this%xco(lp%ib) = real(grid%ibegin - 1, SP)*dx0
         do i = lp%ib + 1, lp%mloc
            this%xco(i) = this%xco(i - 1) + dx0
         end do
         do i = lp%ib - 1, 1, -1
            this%xco(i) = this%xco(i + 1) - dx0
         end do

         this%yco(lp%jb) = real(grid%jbegin - 1, SP)*dy0
         do i = lp%jb + 1, lp%nloc
            this%yco(i) = this%yco(i - 1) + dy0
         end do
         do i = lp%jb - 1, 1, -1
            this%yco(i) = this%yco(i + 1) - dy0
         end do
      end associate

      folder = trim(result_folder)
      if (folder(len(folder):len(folder)) /= "/") folder = folder//"/"
      this%result_folder = folder

      this%is_io_rank = env%comm%is_io_node()

      ! Legacy seeds PLOT_COUNT_TRACKING = 0 and gates on PLOT_START_TIME;
      ! last_triggered >= 0 keeps the trigger out of its fire-on-first-call
      ! branch, so there is no initial-condition row (NOTE 4)
      this%trigger%t_start = t_start_plot
      this%trigger%interval = this%plot_intv
      this%trigger%last_triggered = ZERO
      this%trigger%accum = ZERO

      call tracer_locate(this, grid)

   end subroutine tracer_init_compute

   ! Legacy GET_XY_POSITION: for each tracker, sweep candidate grid-point
   ! quads and keep the first triangle whose three sub-areas are all
   ! non-negative (i.e. the tracker is inside it).
   subroutine tracer_locate(this, grid)
      class(type_model_tracer), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      real(SP) :: x1, y1, x2, y2, x3, y3, area1, area2, area3
      integer  :: i, ii, jj, i_start, i_end, j_start, j_end
      integer  :: search_count, total, ierr
      logical  :: hit

      associate (lp => grid%lp)
         do i = 1, this%n_tracker

            search_count = 0
            do while (.true.)
               if (this%stop_search(i)) exit
               search_count = search_count + 1

               x1 = this%x_track(i)
               y1 = this%y_track(i)

               if (this%in_cell(i)) then
                  ! local re-search around the last known quad
                  i_start = max(this%nx1(i) - 2, lp%ib)
                  i_end = min(this%nx1(i) + 2, lp%ie)
                  j_start = max(this%ny2(i) - 2, lp%jb)
                  j_end = min(this%ny2(i) + 2, lp%je)
               else if (this%found(i) < 1) then
                  ! full local sweep.  The far vertex is xco(ii+1), so the last
                  ! interior point may only be used as a quad origin when a
                  ! neighbour rank owns the point beyond it (NOTE 3)
                  i_start = lp%ib
                  i_end = merge(lp%ie - 1, lp%ie, grid%is_shore_boundary)
                  j_start = lp%jb
                  j_end = merge(lp%je - 1, lp%je, grid%is_left_boundary)
               else
                  ! another rank owns it — probe a single cell and move on
                  i_start = lp%ib
                  i_end = lp%ib
                  j_start = lp%jb
                  j_end = lp%jb
               end if

               hit = .false.
               sweep: do jj = j_start, j_end
                  do ii = i_start, i_end

                     ! lower-left triangle: (ii,jj) (ii+1,jj) (ii,jj+1)
                     area1 = tri_area(x1, y1, this%xco(ii + 1), this%yco(jj), &
                                      this%xco(ii), this%yco(jj + 1))
                     area2 = tri_area(x1, y1, this%xco(ii), this%yco(jj + 1), &
                                      this%xco(ii), this%yco(jj))
                     area3 = tri_area(x1, y1, this%xco(ii), this%yco(jj), &
                                      this%xco(ii + 1), this%yco(jj))

                     if (area1 >= ZERO .and. area2 >= ZERO .and. area3 >= ZERO) then
                        call set_triangle(this, i, ii, jj, ii + 1, jj, ii, jj + 1, &
                                          area1, area2, area3)
                        hit = .true.
                        exit sweep
                     end if

                     ! upper-right triangle: (ii,jj+1) (ii+1,jj) (ii+1,jj+1)
                     area1 = tri_area(x1, y1, this%xco(ii + 1), this%yco(jj), &
                                      this%xco(ii + 1), this%yco(jj + 1))
                     area2 = tri_area(x1, y1, this%xco(ii + 1), this%yco(jj + 1), &
                                      this%xco(ii), this%yco(jj + 1))
                     area3 = tri_area(x1, y1, this%xco(ii), this%yco(jj + 1), &
                                      this%xco(ii + 1), this%yco(jj))

                     if (area1 >= ZERO .and. area2 >= ZERO .and. area3 >= ZERO) then
                        call set_triangle(this, i, ii, jj + 1, ii + 1, jj, &
                                          ii + 1, jj + 1, area1, area2, area3)
                        hit = .true.
                        exit sweep
                     end if

                  end do
               end do sweep

               if (.not. hit) then
                  this%in_cell(i) = .false.
                  this%found(i) = 0
               end if

               ! legacy sums FOUND_IN_DOMAIN across ranks, so every rank agrees
               ! on whether a tracker is lost and the retry stays collective
               if (grid%nx_proc*grid%ny_proc > 1) then
                  call MPI_Allreduce(this%found(i), total, 1, MPI_INTEGER, &
                                     MPI_SUM, grid%cart_comm, ierr)
                  this%found(i) = total
               end if

               if (this%found(i) >= 1) exit

               ! Lost.  Legacy re-searches once over identical state, so the
               ! retry cannot succeed; the tracker then freezes (NOTE 2)
               if (search_count > 1) then
                  this%stop_search(i) = .true.
                  exit
               end if
            end do

         end do
      end associate

   end subroutine tracer_locate

   ! Signed area of the triangle (x1,y1) (x2,y2) (x3,y3); negative when the
   ! vertices run clockwise
   pure function tri_area(x1, y1, x2, y2, x3, y3) result(a)
      real(SP), intent(in) :: x1, y1, x2, y2, x3, y3
      real(SP) :: a
      a = 0.5_SP*(x1*y2 - x2*y1 + x2*y3 - x3*y2 + x3*y1 - x1*y3)
   end function tri_area

   subroutine set_triangle(this, i, i1, j1, i2, j2, i3, j3, a1, a2, a3)
      class(type_model_tracer), intent(inout) :: this
      integer, intent(in) :: i, i1, j1, i2, j2, i3, j3
      real(SP), intent(in) :: a1, a2, a3

      this%in_cell(i) = .true.
      this%found(i) = 1
      this%nx1(i) = i1; this%ny1(i) = j1
      this%nx2(i) = i2; this%ny2(i) = j2
      this%nx3(i) = i3; this%ny3(i) = j3
      this%s1(i) = a1; this%s2(i) = a2; this%s3(i) = a3

      this%sc(i) = tri_area(this%xco(i1), this%yco(j1), &
                            this%xco(i2), this%yco(j2), &
                            this%xco(i3), this%yco(j3))

   end subroutine set_triangle

   ! Legacy TRACK_XY: save the current triangle, advect, re-locate, then
   ! revert any tracker that landed on dry ground.
   subroutine tracer_update(this, grid, t, dt, u, v, mask)
      class(type_model_tracer), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: t, dt
      real(SP), intent(in) :: u(:, :), v(:, :)
      integer, intent(in) :: mask(:, :)

      integer :: i

      if (.not. this%is_activated) return

      this%x_pre = this%x_track
      this%y_pre = this%y_track
      this%nx1_pre = this%nx1; this%ny1_pre = this%ny1
      this%nx2_pre = this%nx2; this%ny2_pre = this%ny2
      this%nx3_pre = this%nx3; this%ny3_pre = this%ny3
      this%s1_pre = this%s1; this%s2_pre = this%s2; this%s3_pre = this%s3
      this%sc_pre = this%sc

      call tracer_move(this, t, dt, u, v)
      call tracer_sync(this, grid)
      call tracer_locate(this, grid)

      do i = 1, this%n_tracker
         if (.not. this%in_cell(i)) cycle

         ! NOTE 1: revert only when all three triangle vertices are dry
         ! (legacy typo double-counted vertex 2 and dropped vertex 3)
         if (mask(this%nx1(i), this%ny1(i)) &
             + mask(this%nx2(i), this%ny2(i)) &
             + mask(this%nx3(i), this%ny3(i)) < 1) then
            this%nx1(i) = this%nx1_pre(i); this%ny1(i) = this%ny1_pre(i)
            this%nx2(i) = this%nx2_pre(i); this%ny2(i) = this%ny2_pre(i)
            this%nx3(i) = this%nx3_pre(i); this%ny3(i) = this%ny3_pre(i)
            this%s1(i) = this%s1_pre(i)
            this%s2(i) = this%s2_pre(i)
            this%s3(i) = this%s3_pre(i)
            this%sc(i) = this%sc_pre(i)
            this%x_track(i) = this%x_pre(i)
            this%y_track(i) = this%y_pre(i)
         end if
      end do

   end subroutine tracer_update

   ! Legacy MOVE_TRACER: barycentric velocity, forward-Euler step.  Only
   ! layer 0 exists (layers 1/2 are rejected at read time).
   subroutine tracer_move(this, t, dt, u, v)
      class(type_model_tracer), intent(inout) :: this
      real(SP), intent(in) :: t, dt
      real(SP), intent(in) :: u(:, :), v(:, :)

      real(SP) :: u_tr, v_tr
      integer  :: i

      do i = 1, this%n_tracker
         if (.not. this%in_cell(i)) cycle
         if (t <= this%t_start(i)) cycle
         if (this%sc(i) == ZERO) cycle   ! legacy prints and skips

         u_tr = (this%s1(i)*u(this%nx1(i), this%ny1(i)) &
                 + this%s2(i)*u(this%nx2(i), this%ny2(i)) &
                 + this%s3(i)*u(this%nx3(i), this%ny3(i)))/this%sc(i)
         v_tr = (this%s1(i)*v(this%nx1(i), this%ny1(i)) &
                 + this%s2(i)*v(this%nx2(i), this%ny2(i)) &
                 + this%s3(i)*v(this%nx3(i), this%ny3(i)))/this%sc(i)

         this%x_track(i) = this%x_track(i) + u_tr*dt
         this%y_track(i) = this%y_track(i) + v_tr*dt
      end do

   end subroutine tracer_move

   ! Legacy BROADCAST_XY: the rank that owns a tracker publishes its position
   ! to every rank.  Legacy gathers to rank 0, takes the LAST owner in rank
   ! order, then scatters back; an allgather is the same result in one step
   ! (and skips legacy's uninitialised read on the non-root ranks).
   subroutine tracer_sync(this, grid)
      class(type_model_tracer), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      real(SP), allocatable :: x_all(:, :), y_all(:, :)
      logical, allocatable :: own_all(:, :)
      integer :: i, l, nproc, ierr

      nproc = grid%nx_proc*grid%ny_proc
      if (nproc <= 1) return

      allocate (x_all(this%n_tracker, nproc), y_all(this%n_tracker, nproc), &
                own_all(this%n_tracker, nproc))

      call MPI_Allgather(this%x_track, this%n_tracker, MPI_SP, &
                         x_all, this%n_tracker, MPI_SP, &
                         grid%cart_comm, ierr)
      call MPI_Allgather(this%y_track, this%n_tracker, MPI_SP, &
                         y_all, this%n_tracker, MPI_SP, &
                         grid%cart_comm, ierr)
      call MPI_Allgather(this%in_cell, this%n_tracker, MPI_LOGICAL, &
                         own_all, this%n_tracker, MPI_LOGICAL, &
                         grid%cart_comm, ierr)

      do i = 1, this%n_tracker
         do l = 1, nproc
            if (own_all(i, l)) then
               this%x_track(i) = x_all(i, l)
               this%y_track(i) = y_all(i, l)
            end if
         end do
      end do

   end subroutine tracer_sync

   ! Legacy OUTPUT_TRACKING: one row per frame, "time  x1 x2 ..." into
   ! tk_x.txt and the same for y, io rank only.
   subroutine tracer_write_output(this, t, dt)
      class(type_model_tracer), intent(inout) :: this
      real(SP), intent(in) :: t, dt

      character(len=32) :: fmt
      integer :: i

      if (.not. this%is_activated) return
      if (.not. this%trigger%should_trigger(t, dt)) return
      if (.not. this%is_io_rank) return

      if (.not. this%opened) then
         this%opened = .true.
         open (newunit=this%unit_x, file=this%result_folder//"tk_x.txt", &
               status="replace", action="write")
         open (newunit=this%unit_y, file=this%result_folder//"tk_y.txt", &
               status="replace", action="write")
      end if

      write (fmt, '("(",I0,"E16.6)")') this%n_tracker + 1
      write (this%unit_x, fmt) t, (this%x_track(i), i=1, this%n_tracker)
      write (this%unit_y, fmt) t, (this%y_track(i), i=1, this%n_tracker)

   end subroutine tracer_write_output

   subroutine tracer_free(this)
      class(type_model_tracer), intent(inout) :: this

      if (this%opened) then
         close (this%unit_x)
         close (this%unit_y)
         this%opened = .false.
      end if
      if (allocated(this%x_track)) then
         deallocate (this%x_track, this%y_track, this%t_start, this%layer, &
                     this%nx1, this%ny1, this%nx2, this%ny2, this%nx3, this%ny3, &
                     this%sc, this%s1, this%s2, this%s3, &
                     this%x_pre, this%y_pre, &
                     this%nx1_pre, this%ny1_pre, this%nx2_pre, this%ny2_pre, &
                     this%nx3_pre, this%ny3_pre, &
                     this%sc_pre, this%s1_pre, this%s2_pre, this%s3_pre, &
                     this%in_cell, this%stop_search, this%found)
      end if
      if (allocated(this%xco)) deallocate (this%xco, this%yco)
      this%n_tracker = 0
      this%is_activated = .false.

   end subroutine tracer_free

end module model_tracer_mod
