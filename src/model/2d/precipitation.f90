!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Rainfall source (legacy mod_precipitation.F, PRECIPITATION_MODULE)
!
!  YAML block: precipitation:       (top-level; omit for no rainfall)
!    file:                <path>   index file: dims header + (time, frame
!                                  file) records; required when the block
!                                  is present (nee RAINFALL_FILE; legacy
!                                  STOPs without it)
!
!  RainWaveInteraction and OUT_PRECIPITATION are dropped, not renamed:
!  legacy reads them and consumes them nowhere, so there is nothing to port.
!
!  Legacy call shape: PRECIPITATION_DISTRIBUTION once per step before the
!  RK loop (at the already-advanced TIME); the rate enters the eta RHS in
!  every stage (etauv_solver.F:264, R1 += PrecRateModel after the
!  wavemaker mass term).
!
!  Bug-for-bug notes vs legacy:
!    1. NOTE: rain is ZERO until TIME passes the first record time (both
!       interpolation weights stay 0), not the first frame's values
!    2. NOTE: the bracket advances at most ONE record per step (IF, not a
!       while loop) — when record spacing < dt the weights extrapolate
!       ($w_2 < 0$) until the bracket catches up
!    3. NOTE: the interpolation stops one cell short and copies the local
!       Iend/Jend column/row from its neighbor on EVERY rank (OOB guard at
!       the data grid's far edge) — the rain field is decomposition-
!       dependent at rank seams
!    4. NOTE: index-file EOF freezes the blended field at its last values;
!       legacy fatals one step later (post-EOF READ falls through to a
!       no-IOSTAT READ), modern freezes forever (punch-listed deviation,
!       same class as the tide DATA EOF)
!    5. NOTE: streamed frame names are list-directed reads — a '/' in the
!       name truncates it (legacy quirk), so frames after the first must
!       sit in the run directory; the initial name is a full-line read
!    6. NOTE: the mm/hr -> m/s constant is legacy's unpromoted float32
!       literal, kept unsuffixed here for the identical promoted value
!
!  HISTORY :
!    07/11/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_precipitation_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_precipitation

   ! legacy conversion literal (mod_precipitation.F:292), deliberately
   ! default-real: float32 value promoted exactly like legacy
   real(SP), parameter :: MMHR_TO_MS = 0.000000277778
   real(SP), parameter :: SMALL = 0.000001_SP

   type, extends(type_model_base) :: type_model_precipitation

      type(type_path) :: rainfall_file

      ! rainfall data grid (M_PrecDim x N_PrecDim) and time bracket
      integer :: m_dim = 0, n_dim = 0
      real(SP) :: t1 = 0.0_SP, t2 = 0.0_SP
      character(len=80) :: name1 = ' ', name2 = ' '
      integer :: unit_index = -1        ! -1 marks never-opened
      logical :: eof = .false.

      ! bracket frames, blended field (data units then m/s), and the
      ! model-grid rate; legacy leaves data1 and rate_model unallocated
      ! garbage until first use — zero-init here is inert (they only
      ! ever multiply a zero weight / fill ghost cells)
      real(SP), allocatable :: data1(:, :), data2(:, :), blend(:, :)
      real(SP), allocatable :: rate_model(:, :)

      ! cached local window + global index offsets (legacy iXco/iYco)
      integer :: ib = 0, ie = 0, jb = 0, je = 0
      integer :: ig0 = 0, jg0 = 0        ! global = local + offset
      integer :: mglob = 0, nglob = 0

   contains
      procedure :: read_input => prec_read_input
      procedure :: init_compute => prec_init_compute
      procedure :: update => prec_update
      procedure :: free => prec_free
   end type type_model_precipitation

contains

   subroutine prec_read_input(this, env)
      class(type_model_precipitation), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk, no_key, tmp_l

      sub_env = get_sub_env(env, "precipitation", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (no_blk) return

      ! dead legacy knob: dropped, not parked (no writer ever consumed it)
      call sub_env%yaml%read("OUT_PRECIPITATION", silent=no_key, val=tmp_l)
      if (.not. no_key) call env%log%exit_on_error( &
         "precipitation: OUT_PRECIPITATION dropped -- legacy never had a writer for it")

      call sub_env%yaml%read_input_path("file", silent=no_key, &
                                        val=this%rainfall_file)
      if (no_key) then
         ! legacy PRECIPITATION builds refuse to run without the file
         call env%log%exit_on_error("precipitation: file is required")
      end if

   end subroutine prec_read_input

   ! ----------------------------------------------------------------
   ! Legacy PRECIPITATION_INITIAL tail: open the index file, read dims
   ! and the first (time, frame) record, load the first frame into the
   ! HIGH bracket only — data1 holds nothing until the first advance
   ! copies data2 down (header NOTE 1).
   ! ----------------------------------------------------------------
   subroutine prec_init_compute(this, grid)
      class(type_model_precipitation), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      character(len=80) :: header

      if (.not. this%is_activated) return

      this%ib = grid%lp%ib
      this%ie = grid%lp%ie
      this%jb = grid%lp%jb
      this%je = grid%lp%je
      this%ig0 = grid%ibegin - grid%lp%ib
      this%jg0 = grid%jbegin - grid%lp%jb
      this%mglob = grid%M
      this%nglob = grid%N

      open (newunit=this%unit_index, file=this%rainfall_file%root, &
            status='old', action='read')
      read (this%unit_index, *) header                  ! title
      read (this%unit_index, *) this%m_dim, this%n_dim
      read (this%unit_index, *) header                  ! t / name banner
      read (this%unit_index, *) this%t2
      read (this%unit_index, '(A80)') this%name2

      this%t1 = this%t2
      this%name1 = this%name2

      allocate (this%data1(this%m_dim, this%n_dim), source=0.0_SP)
      allocate (this%data2(this%m_dim, this%n_dim), source=0.0_SP)
      allocate (this%blend(this%m_dim, this%n_dim), source=0.0_SP)
      allocate (this%rate_model(grid%lp%mloc, grid%lp%nloc), source=0.0_SP)

      call read_frame(this%name2, this%m_dim, this%n_dim, this%data2)

   end subroutine prec_init_compute

   subroutine read_frame(fname, m_dim, n_dim, frame)
      character(*), intent(in) :: fname
      integer, intent(in) :: m_dim, n_dim
      real(SP), intent(out) :: frame(:, :)

      integer :: unit, i, j

      open (newunit=unit, file=trim(fname), status='old', action='read')
      do j = 1, n_dim
         read (unit, *) (frame(i, j), i=1, m_dim)
      end do
      close (unit)
   end subroutine read_frame

   ! ----------------------------------------------------------------
   ! Legacy PRECIPITATION_DISTRIBUTION at the already-advanced TIME:
   ! one optional record advance, blend + unit conversion
   !   $$ w_2 = (t_2 - t)/\max(\epsilon, |t_2 - t_1|), \quad
   !      w_1 = 1 - w_2, \qquad
   !      P = (P_2 w_1 + P_1 w_2)\cdot 2.77778\times 10^{-7} $$
   ! then bilinear interpolation of the data grid onto interior cells
   ! by GLOBAL index, one short of the local far edges (header NOTE 3).
   ! ----------------------------------------------------------------
   subroutine prec_update(this, time)
      class(type_model_precipitation), intent(inout) :: this
      real(SP), intent(in) :: time

      real(SP) :: w1, w2, rii, rjj, tmp1, tmp2
      integer :: i, j, ii, jj, ios

      ! frozen after EOF: the blend (and so the model rate) can never
      ! change again — legacy instead fatals on its next READ
      if (this%eof) return

      if (time > this%t1 .and. time > this%t2) then
         this%t1 = this%t2
         this%name1 = this%name2
         this%data1 = this%data2

         read (this%unit_index, *, iostat=ios) this%t2
         if (ios /= 0) then
            this%eof = .true.
            return
         end if
         ! list-directed name read — the '/' truncation quirk lives here
         read (this%unit_index, *) this%name2

         call read_frame(this%name2, this%m_dim, this%n_dim, this%data2)
      end if

      w2 = 0.0_SP
      w1 = 0.0_SP
      if (time > this%t1) then
         if (this%t1 == this%t2) then
            ! no more data (single record, or duplicate times)
            w2 = 0.0_SP
            w1 = 0.0_SP
         else
            w2 = (this%t2 - time)/max(SMALL, abs(this%t2 - this%t1))
            w1 = 1.0_SP - w2
         end if
      end if

      this%blend = this%data2*w1 + this%data1*w2
      this%blend = this%blend*MMHR_TO_MS

      ! default-kind real() of the global index like legacy REAL(iXco)
      do j = this%jb, this%je - 1
         do i = this%ib, this%ie - 1
            rii = (real(this%ig0 + i) - 1.0_SP)/(real(this%mglob) - 1.0_SP) &
                  *(real(this%m_dim) - 1.0_SP) + 1.0
            rjj = (real(this%jg0 + j) - 1.0_SP)/(real(this%nglob) - 1.0_SP) &
                  *(real(this%n_dim) - 1.0_SP) + 1.0
            ii = floor(rii)
            jj = floor(rjj)
            tmp1 = (1.0_SP - rii + ii)*this%blend(ii, jj) &
                   + (rii - ii)*this%blend(ii + 1, jj)
            tmp2 = (1.0_SP - rii + ii)*this%blend(ii, jj + 1) &
                   + (rii - ii)*this%blend(ii + 1, jj + 1)
            this%rate_model(i, j) = (1.0 - rjj + jj)*tmp1 + (rjj - jj)*tmp2
         end do
      end do

      do j = this%jb, this%je - 1
         this%rate_model(this%ie, j) = this%rate_model(this%ie - 1, j)
      end do
      do i = this%ib, this%ie - 1
         this%rate_model(i, this%je) = this%rate_model(i, this%je - 1)
      end do
      this%rate_model(this%ie, this%je) = this%rate_model(this%ie - 1, this%je - 1)

   end subroutine prec_update

   subroutine prec_free(this)
      class(type_model_precipitation), intent(inout) :: this

      if (this%unit_index /= -1) close (this%unit_index)
      this%unit_index = -1
      if (allocated(this%data1)) deallocate (this%data1, this%data2, this%blend)
      if (allocated(this%rate_model)) deallocate (this%rate_model)
   end subroutine prec_free

end module model_precipitation_mod
