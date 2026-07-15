!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Atmospheric forcing (legacy mod_meteo.F, METEO_MODULE)
!
!  Legacy METEO is four independent switched sub-models (dispatcher
!  METEO_FORCING): MeteoGausian (a moving Gaussian air-pressure pulse),
!  WindConstantField, the Holland hurricane wind, and a Slide/landslide source.
!  This module ports them one at a time.  RUNG a: MeteoGausian only — the sole
!  sub-model any legacy case exercises (simple_cases/meteo_tsunami).
!
!  YAML block: meteo:                 (top-level; omit for no atmospheric forcing)
!    MeteoGausian:        <bool>   default NO   — the moving pressure pulse
!    METEO_GAUSIAN_FILE:  <path>   storm track; required when MeteoGausian is on
!    OUT_METEO:           <bool>   default YES  — write the pressure field
!
!  MeteoGausian: a storm-track file streams records (time, x, y, dP, SigmaX,
!  SigmaY, Theta); the two bracketing records are linearly interpolated in time
!  to a rotated 2D Gaussian air-pressure field, whose gradient forces the flow:
!    $$ P(x,y) = \Delta P\,\exp\!\big[-(a\,\Delta x^2 + 2b\,\Delta x\,\Delta y
!               + c\,\Delta y^2)\big]/100, $$
!    $$ a = \tfrac{\cos^2\theta}{2\sigma_x^2}+\tfrac{\sin^2\theta}{2\sigma_y^2},\;
!       b = \tfrac{\sin 2\theta}{4}\!\big(\tfrac1{\sigma_y^2}-\tfrac1{\sigma_x^2}\big),\;
!       c = \tfrac{\sin^2\theta}{2\sigma_x^2}+\tfrac{\cos^2\theta}{2\sigma_y^2}, $$
!    $$ S_x = -g\,H\,\partial_x P, \qquad S_y = -g\,H\,\partial_y P, $$
!  added into SourceX/SourceY (legacy gates this on AirPressure, which
!  MeteoGausian forces .TRUE. at mod_meteo.F:207 — so the pressure always reaches
!  the flow; there is no separate switch to miss).
!
!  Bug-for-bug notes vs legacy:
!    NOTE 1: the record advance shifts ONLY (t, x, y) into the low bracket
!            (mod_meteo.F:1061-1063); dP / SigmaX / SigmaY / Theta are NOT
!            shifted, so after the first advance they stay frozen at the FIRST
!            record's setup values for the rest of the run.  A LEGACY BUG:
!            latent for a 2-record file (one interval, so slot-1 already holds
!            the first record correctly), live for any 3+ record storm, where
!            those four interpolate against a stale first-record endpoint while
!            x/y interpolate against the true previous record.  Reproduced.
!    NOTE 2: params are ZERO for TIME <= the first record time (both weights
!            stay 0), which drives SigmaX to 0 and legacy STOPs.  It does not
!            bite in practice because the first forcing call runs at an
!            already-advanced TIME > 0 that triggers the record advance first.
!    NOTE 3: dP is scaled by 1/100 ("cm to metre" per legacy) — the file's mb
!            label is not honoured as a pressure unit; the number is used raw.
!    NOTE 4: the Gaussian is evaluated over the ghost cells too (full local
!            lattice); the gradient is a Cartesian centred difference off the
!            scalar grid%dx(1,1) — ported Cartesian-only (spherical METEO is a
!            post-strip concern).
!    NOTE 5: the storm-file EOF freezes the bracket at its last record (legacy
!            END=120 leaves the slot-2 values in place); reproduced as the eof
!            flag — no further advance, so the last field holds.
!    NOTE 6: the degrees->radians is legacy's `*PI/180.0` on the interpolated
!            angle (not a precomputed DEG2RAD parameter), kept for the identical
!            round-off.
!
!  HISTORY :
!    07/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_meteo_mod
   use core_constants_mod, only: SP, ZERO, SMALL, PI, GRAV
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_METEO_METEOGAUSIAN, &
                                        DEF_METEO_OUT_METEO

   implicit none

   private
   public :: type_model_meteo

   type, extends(type_model_base) :: type_model_meteo

      logical :: meteo_gausian = .false.
      logical :: out_meteo = .true.

      type(type_path) :: gausian_file

      ! two-record storm-track bracket (legacy TimeStorm1/2, Xstorm1/2, ...).
      ! NOTE 1: only t/x/y advance into the low slot; dp/sigx/sigy/th do not
      real(SP) :: t1 = ZERO, t2 = ZERO
      real(SP) :: x1 = ZERO, x2 = ZERO, y1 = ZERO, y2 = ZERO
      real(SP) :: dp1 = ZERO, dp2 = ZERO
      real(SP) :: sigx1 = ZERO, sigx2 = ZERO, sigy1 = ZERO, sigy2 = ZERO
      real(SP) :: th1 = ZERO, th2 = ZERO
      integer :: unit_track = -1        ! -1 marks never-opened
      logical :: eof = .false.

      ! ghost-inclusive grid-point coordinates (legacy Xco/Yco)
      real(SP), allocatable :: xco(:), yco(:)
      real(SP) :: dx0 = ZERO, dy0 = ZERO

      ! the pressure field and its gradient forcing (StormPressureTotal/X/Y)
      real(SP), allocatable :: p_total(:, :), p_x(:, :), p_y(:, :)

      ! cached interior window
      integer :: ib = 0, ie = 0, jb = 0, je = 0
      integer :: mloc = 0, nloc = 0

   contains
      procedure :: read_input => meteo_read_input
      procedure :: init_compute => meteo_init_compute
      procedure :: update => meteo_update
      procedure :: free => meteo_free
   end type type_model_meteo

contains

   subroutine meteo_read_input(this, env)
      class(type_model_meteo), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk, no_key

      sub_env = get_sub_env(env, "meteo", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (no_blk) return

      call sub_env%yaml%read("MeteoGausian", silent=no_key, &
                             val=this%meteo_gausian, &
                             default=DEF_METEO_METEOGAUSIAN)
      call sub_env%yaml%read("OUT_METEO", silent=no_key, &
                             val=this%out_meteo, &
                             default=DEF_METEO_OUT_METEO)

      if (this%meteo_gausian) then
         call sub_env%yaml%read_input_path("METEO_GAUSIAN_FILE", silent=no_key, &
                                           val=this%gausian_file)
         if (no_key) then
            error stop "meteo: METEO_GAUSIAN_FILE is required when MeteoGausian is on"
         end if
      end if

   end subroutine meteo_read_input

   ! ----------------------------------------------------------------
   ! Legacy MeteoGausian_Setup: build the ghost-inclusive Xco/Yco lattice off
   ! the scalar dx/dy, open the storm file, skip its two banner lines, and read
   ! the first record into BOTH bracket slots (so the low slot holds the whole
   ! first record — the only place dp/sigx/sigy/th ever reach it, NOTE 1).
   ! ----------------------------------------------------------------
   subroutine meteo_init_compute(this, grid)
      class(type_model_meteo), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      integer :: i, j
      character(len=80) :: header

      if (.not. this%is_activated) return
      if (.not. this%meteo_gausian) return

      associate (lp => grid%lp)
         this%ib = lp%ib
         this%ie = lp%ie
         this%jb = lp%jb
         this%je = lp%je
         this%mloc = lp%mloc
         this%nloc = lp%nloc
         this%dx0 = grid%dx(1, 1)
         this%dy0 = grid%dy(1, 1)

         ! ghost-inclusive grid-point lattice; grid%x/grid%dx are interior-only
         allocate (this%xco(lp%mloc), this%yco(lp%nloc))
         this%xco(lp%ib) = real(grid%ibegin - 1, SP)*this%dx0
         do i = lp%ib + 1, lp%mloc
            this%xco(i) = this%xco(i - 1) + this%dx0
         end do
         do i = lp%ib - 1, 1, -1
            this%xco(i) = this%xco(i + 1) - this%dx0
         end do
         this%yco(lp%jb) = real(grid%jbegin - 1, SP)*this%dy0
         do j = lp%jb + 1, lp%nloc
            this%yco(j) = this%yco(j - 1) + this%dy0
         end do
         do j = lp%jb - 1, 1, -1
            this%yco(j) = this%yco(j + 1) - this%dy0
         end do

         allocate (this%p_total(lp%mloc, lp%nloc), source=ZERO)
         allocate (this%p_x(lp%mloc, lp%nloc), source=ZERO)
         allocate (this%p_y(lp%mloc, lp%nloc), source=ZERO)
      end associate

      open (newunit=this%unit_track, file=this%gausian_file%root, &
            status='old', action='read')
      read (this%unit_track, *) header                  ! title
      read (this%unit_track, *) header                  ! storm name
      read (this%unit_track, *) header                  ! column banner
      read (this%unit_track, *) this%t2, this%x2, this%y2, &
         this%dp2, this%sigx2, this%sigy2, this%th2

      ! whole first record into the low slot (setup copies ALL seven)
      this%t1 = this%t2
      this%x1 = this%x2
      this%y1 = this%y2
      this%dp1 = this%dp2
      this%sigx1 = this%sigx2
      this%sigy1 = this%sigy2
      this%th1 = this%th2

   end subroutine meteo_init_compute

   ! ----------------------------------------------------------------
   ! Legacy MeteoGausian_Forcing at the already-advanced TIME: one optional
   ! record advance (t/x/y only, NOTE 1), linear-in-time blend of the storm
   ! params, the rotated Gaussian pressure field, then its -g*H*grad forcing.
   ! ----------------------------------------------------------------
   subroutine meteo_update(this, time, h)
      class(type_model_meteo), intent(inout) :: this
      real(SP), intent(in) :: time
      real(SP), intent(in) :: h(:, :)

      real(SP) :: w1, w2
      real(SP) :: xs, ys, dps, sigx, sigy, theta, a, b, c
      integer :: i, j, ios

      if (.not. this%is_activated) return
      if (.not. this%meteo_gausian) return

      this%p_total = ZERO

      ! advance the bracket by ONE record when TIME clears both endpoints
      if (.not. this%eof) then
         if (time > this%t1 .and. time > this%t2) then
            ! NOTE 1: only t/x/y move into the low slot; dp/sigx/sigy/th do not
            this%t1 = this%t2
            this%x1 = this%x2
            this%y1 = this%y2
            read (this%unit_track, *, iostat=ios) this%t2, this%x2, this%y2, &
               this%dp2, this%sigx2, this%sigy2, this%th2
            if (ios /= 0) this%eof = .true.
         end if
      end if

      ! linear-in-time weights (NOTE 2: both zero until TIME clears t1)
      w2 = ZERO
      w1 = ZERO
      if (time > this%t1) then
         if (this%t1 == this%t2) then
            w2 = ZERO
            w1 = ZERO
         else
            w2 = (this%t2 - time)/max(SMALL, abs(this%t2 - this%t1))
            w1 = 1.0_SP - w2
         end if
      end if

      xs = this%x2*w1 + this%x1*w2
      ys = this%y2*w1 + this%y1*w2
      dps = this%dp2*w1 + this%dp1*w2
      sigx = this%sigx2*w1 + this%sigx1*w2
      sigy = this%sigy2*w1 + this%sigy1*w2
      theta = (this%th2*w1 + this%th1*w2)*PI/180.0_SP     ! NOTE 6

      ! legacy fatals on a degenerate Gaussian rather than divide by zero
      if (sigx == ZERO .or. sigy == ZERO) then
         error stop "meteo: SigmaX or SigmaY is zero"
      end if

      a = (cos(theta))**2/2.0_SP/sigx**2 &
          + (sin(theta))**2/2.0_SP/sigy**2
      b = -sin(2.0_SP*theta)/4.0_SP/sigx**2 &
          + sin(2.0_SP*theta)/4.0_SP/sigy**2
      c = (sin(theta))**2/2.0_SP/sigx**2 &
          + (cos(theta))**2/2.0_SP/sigy**2

      ! full local lattice, ghost cells included (NOTE 4)
      do j = 1, this%nloc
         do i = 1, this%mloc
            this%p_total(i, j) = dps*exp(-(a*(this%xco(i) - xs)**2 &
                                           + 2.0_SP*b*(this%xco(i) - xs)*(this%yco(j) - ys) &
                                           + c*(this%yco(j) - ys)**2))/100.0_SP
         end do
      end do

      ! -g*H*grad(P) into the momentum source, centred on the scalar dx/dy
      do j = this%jb, this%je
         do i = this%ib, this%ie
            this%p_x(i, j) = -GRAV*h(i, j) &
                             *(this%p_total(i + 1, j) - this%p_total(i - 1, j)) &
                             /2.0_SP/this%dx0
            this%p_y(i, j) = -GRAV*h(i, j) &
                             *(this%p_total(i, j + 1) - this%p_total(i, j - 1)) &
                             /2.0_SP/this%dy0
         end do
      end do

   end subroutine meteo_update

   subroutine meteo_free(this)
      class(type_model_meteo), intent(inout) :: this

      if (this%unit_track /= -1) close (this%unit_track)
      this%unit_track = -1
      if (allocated(this%xco)) deallocate (this%xco, this%yco)
      if (allocated(this%p_total)) deallocate (this%p_total, this%p_x, this%p_y)
   end subroutine meteo_free

end module model_meteo_mod
