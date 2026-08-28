!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Tide/surge boundary conditions (legacy mod_tide.F, TIDE_MODULE)
!
!  Configuration arrives from the boundaries: section reader
!  (model_boundaries_mod) — this module owns no YAML read since the config
!  reorg (rung 2): per-face forcing {eta,u,v} constants or file series map
!  onto the CONSTANT/DATA targets below, and tidal_bc_abs derives from
!  forcing + sponge.direct presence.  The strip geometry/coefficients come
!  from the face sponge block (rung 11): width in metres -> iwidth cells at
!  init, direct r/a per face (nee global WaveMakerPointNum + the hardcoded
!  R=0.85/A=10 pair).
!  GEN_ABS = a wavemaker-fed west face with an eta/file target
!  (boundaries.west.forcing {wavemaker, eta|file}, reorg rung 3b): the
!  west tide slot holds/streams the target without the TIDE_BC strip.
!
!  Legacy call shape: TIDE_DATA once per step after ESTIMATE_DT (so at the
!  already-advanced TIME), TIDE_BC per RK stage between UPDATE_MASK and
!  WAVE_BREAKING, GEN_ABS inside ABSORBING_GENERATING_BC (wavemaker.f90).
!
!  Bug-for-bug notes vs legacy:
!    1. NOTE: REMOVE_SPONGE is inert on the sponge itself — TIDE_INITIAL runs
!       after INITIALIZATION, so zeroing Sponge_*_width post-dates the sponge
!       coefficient build and "Sponge_west is removed" never happens; only
!       the no-BC disable survives, and its test reads TideEast three times
!       (mod_tide.F:635), so south/north-only tide turns TIDAL_BC_ABS off
!    2. NOTE: TIDE_BC strips are LOCAL indices on every rank — interior ranks
!       relax their own subdomain edges through factors within 1e-15 of 1
!    3. DIVERGES from legacy: the shared time-series reader
!       (core_time_series_mod) freezes each boundary at its OWN last record
!       past EOF, not all boundaries on the first EOF (legacy END=120)
!    4. DIVERGES from legacy: DATA targets hold the FIRST record's values
!       before TIME reaches it (clamped bracket), not zero (legacy left both
!       interpolation weights at 0 until TIME passed the first record)
!    5. NOTE: eta/u/v are relaxed but P/Q/HU/HV are not rebuilt — the
!       inconsistency rides into the next stage's fluxes like legacy
!
!  HISTORY :
!    07/11/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_tide_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use core_time_series_mod, only: type_time_series
   use model_base_mod, only: type_model_base
   use model_sponge_mod, only: FACE_W, FACE_E, FACE_S, FACE_N

   implicit none

   private
   public :: type_model_tide

   ! legacy TIDE_SPONGE profile floor (mod_tide.F:249) and PARAM SMALL
   real(SP), parameter :: LIM_TIDE = 1.0_SP
   real(SP), parameter :: SMALL = 0.000001_SP

   type, extends(type_model_base) :: type_model_tide

      ! is_activated = read-time abs .or. gen_abs; never flipped, and it
      ! keeps gating the per-step DATA reads after the no-BC disable
      ! turns tidal_bc_abs off (legacy TIDE_DATA gates on TideBcType,
      ! which stays set)
      logical :: tidal_bc_abs = .false.
      logical :: tidal_bc_gen_abs = .false.

      character(len=80) :: tide_bc_type = 'CONSTANT'

      ! per-face strip geometry + coefficients from the face sponge block
      ! (rung 11); defaults mirror the legacy hardcodes for never-forced
      ! faces (their profiles build but never apply).  iwidth derives from
      ! width_m at init (dx for W/E, dy for S/N)
      real(SP) :: width_m(4) = 0.0_SP
      real(SP) :: r_face(4) = 0.85_SP
      real(SP) :: a_face(4) = 10.0_SP
      integer  :: iwidth(4) = 30

      ! per-boundary enables (legacy default .TRUE., knocked out by a
      ! missing TideX_ETA / TideXFileName)
      logical :: tide_west = .true., tide_east = .true.
      logical :: tide_south = .true., tide_north = .true.

      ! Flather radiation faces (forcing without sponge.direct): the target
      ! rides the boundary-normal flux (kernel flux_flather_bc), NOT this
      ! relaxation strip, so apply_bc skips them
      logical :: flather(4) = .false.

      ! Flather external-target arrays along each face (west/east indexed
      ! in j over nloc, south/north in i over mloc; sized (max(mloc,nloc),
      ! 4)).  Rebuilt per step by build_flather_target: the tide scalar
      ! broadcast uniform, then any boundary wavemaker ADDS its incident
      ! series on top, so a tide and a wave superpose on one face
      real(SP), allocatable :: eta_ext(:, :), u_ext(:, :), v_ext(:, :)

      ! current relaxation targets — CONSTANT values, or the DATA
      ! interpolants refreshed by update_data
      real(SP) :: eta_west = 0.0_SP, u_west = 0.0_SP, v_west = 0.0_SP
      real(SP) :: eta_east = 0.0_SP, u_east = 0.0_SP, v_east = 0.0_SP
      real(SP) :: eta_south = 0.0_SP, u_south = 0.0_SP, v_south = 0.0_SP
      real(SP) :: eta_north = 0.0_SP, u_north = 0.0_SP, v_north = 0.0_SP

      type(type_path) :: file_west, file_east, file_south, file_north

      ! DATA streaming state: one time-series reader per boundary
      ! (FACE_W..FACE_N), opened lazily when the tide is DATA-driven
      type(type_time_series) :: series(4)

      ! inverted relaxation profiles (legacy SPONGE_TIDE_*), local
      ! ghost-inclusive windows
      real(SP), allocatable :: sponge_west(:, :), sponge_east(:, :)
      real(SP), allocatable :: sponge_south(:, :), sponge_north(:, :)

   contains
      procedure :: read_input => tide_read_input
      procedure :: init_compute => tide_init_compute
      procedure :: update_data => tide_update_data
      procedure :: build_flather_target => tide_build_flather_target
      procedure :: apply_bc => tide_apply_bc
      procedure :: data_mode => tide_data_mode
      procedure :: free => tide_free
   end type type_model_tide

contains

   !> No-op satisfying the deferred base binding — configuration arrives
   !> from the boundaries: reader (model_boundaries_mod)
   subroutine tide_read_input(this, env)
      class(type_model_tide), intent(inout) :: this
      type(type_env), intent(inout), target :: env
   end subroutine tide_read_input

   ! ----------------------------------------------------------------
   ! Relaxation profiles + DATA series open (legacy TIDE_INITIAL tail).
   ! The west profile in local index i on cart rank npx of px:
   !   $$ r_i = R^{\lfloor 50\,(i + n_{px} M_{glob}/p_x - 1)
   !            /(I_w - 1) \rfloor}, \qquad
   !      s(i) = 1/\max(A^{r_i},\ 1) $$
   ! with per-face R/A/I_w from the face sponge block (rung 11; the
   ! legacy pair was R = 0.85, A = 10 with I_w global) — the exponent is
   ! INTEGER division like legacy, and every rank fills its whole window
   ! (the global offset keeps the profile continuous across seams).
   ! NOTE: the serial legacy build has a north typo (Nloc - i,
   ! mod_tide.F:295); the vendored legacy is a PARALLEL build, so the j
   ! form is the one reproduced here.
   ! ----------------------------------------------------------------
   subroutine tide_init_compute(this, grid)
      class(type_model_tide), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      real(SP) :: ri, d
      integer :: i, j, f, mloc, nloc

      if (.not. this%is_activated) return

      ! width (m) -> strip cells; the profile divides by iwidth-1, so a
      ! sub-2-cell strip is floored (never applied narrower than legacy
      ! could express)
      do f = FACE_W, FACE_N
         if (this%width_m(f) > 0.0_SP) then
            if (f == FACE_W .or. f == FACE_E) then
               d = grid%dx(1, 1)
            else
               d = grid%dy(1, 1)
            end if
            this%iwidth(f) = max(2, nint(this%width_m(f)/d))
         end if
      end do

      mloc = grid%lp%mloc
      nloc = grid%lp%nloc
      allocate (this%sponge_west(mloc, nloc), this%sponge_east(mloc, nloc), &
                this%sponge_south(mloc, nloc), this%sponge_north(mloc, nloc))

      ! Flather external-target arrays (see the type declaration): a single
      ! (max(mloc,nloc), 4) block indexed per face, zero until build_flather
      if (any(this%flather)) &
         allocate (this%eta_ext(max(mloc, nloc), 4), &
                   this%u_ext(max(mloc, nloc), 4), &
                   this%v_ext(max(mloc, nloc), 4), source=0.0_SP)

      associate (Mglob => grid%M, Nglob => grid%N, &
                 px => grid%nx_proc, py => grid%ny_proc, &
                 npx => grid%iproc, npy => grid%jproc, &
                 iw => this%iwidth, r => this%r_face, a => this%a_face)
         do j = 1, nloc
            do i = 1, mloc
               ri = r(FACE_W)**(50*(i + npx*Mglob/px - 1)/(iw(FACE_W) - 1))
               this%sponge_west(i, j) = max(a(FACE_W)**ri, LIM_TIDE)
               ri = r(FACE_E)**(50*(mloc - i + (px - npx - 1)*Mglob/px)/(iw(FACE_E) - 1))
               this%sponge_east(i, j) = max(a(FACE_E)**ri, LIM_TIDE)
               ri = r(FACE_S)**(50*(j + npy*Nglob/py - 1)/(iw(FACE_S) - 1))
               this%sponge_south(i, j) = max(a(FACE_S)**ri, LIM_TIDE)
               ri = r(FACE_N)**(50*(nloc - j + (py - npy - 1)*Nglob/py)/(iw(FACE_N) - 1))
               this%sponge_north(i, j) = max(a(FACE_N)**ri, LIM_TIDE)
            end do
         end do
      end associate

      this%sponge_west = 1.0_SP/this%sponge_west
      this%sponge_east = 1.0_SP/this%sponge_east
      this%sponge_south = 1.0_SP/this%sponge_south
      this%sponge_north = 1.0_SP/this%sponge_north

      ! DATA series: every rank opens and streams its own copy (legacy
      ! units 201-204); first line is a header, first record seeds both
      ! bracket points
      if (this%data_mode()) then
         if (this%tide_west) &
            call this%series(FACE_W)%open(this%file_west%root, 3, .true.)
         if (this%tide_east) &
            call this%series(FACE_E)%open(this%file_east%root, 3, .true.)
         if (this%tide_south) &
            call this%series(FACE_S)%open(this%file_south%root, 3, .true.)
         if (this%tide_north) &
            call this%series(FACE_N)%open(this%file_north%root, 3, .true.)
      end if

      ! legacy REMOVE_SPONGE (TIDAL_BC_ABS only): the width zeroing is
      ! inert (see header NOTE 1); only the no-BC disable acts, and its
      ! broken conjunction checks west and east alone
      if (this%tidal_bc_abs) then
         if ((.not. this%tide_west) .and. (.not. this%tide_east)) then
            this%tidal_bc_abs = .false.
         end if
      end if

   end subroutine tide_init_compute

   logical function tide_data_mode(this)
      class(type_model_tide), intent(in) :: this
      tide_data_mode = this%tide_bc_type(1:4) == 'DATA'
   end function tide_data_mode

   ! ----------------------------------------------------------------
   ! Per-step DATA refresh (legacy TIDE_DATA).  time is the legacy
   ! already-advanced TIME; the bracket advances while
   ! $t_2 < t + \Delta t$, then the targets interpolate to
   !   $$ w_2 = (t_2 - t)/\max(\epsilon, |t_2 - t_1|), \quad
   !      w_1 = 1 - w_2, \qquad
   !      \eta_{tgt} = w_1 \eta_2 + w_2 \eta_1 $$
   ! (weights zero before the first record — see header NOTE 4).
   ! EOF anywhere aborts the whole call (header NOTE 3).
   ! ----------------------------------------------------------------
   ! Refresh the DATA relaxation targets from the per-boundary series at the
   ! current model time (dt is unused: the shared reader brackets the query
   ! time itself and each boundary freezes independently past its own EOF).
   subroutine tide_update_data(this, time, dt)
      class(type_model_tide), intent(inout) :: this
      real(SP), intent(in) :: time, dt

      real(SP) :: out(3)

      if (this%tide_west) then
         call this%series(FACE_W)%sample(time, out)
         this%eta_west = out(1); this%u_west = out(2); this%v_west = out(3)
      end if
      if (this%tide_east) then
         call this%series(FACE_E)%sample(time, out)
         this%eta_east = out(1); this%u_east = out(2); this%v_east = out(3)
      end if
      if (this%tide_south) then
         call this%series(FACE_S)%sample(time, out)
         this%eta_south = out(1); this%u_south = out(2); this%v_south = out(3)
      end if
      if (this%tide_north) then
         call this%series(FACE_N)%sample(time, out)
         this%eta_north = out(1); this%u_north = out(2); this%v_north = out(3)
      end if

   end subroutine tide_update_data

   ! ----------------------------------------------------------------
   ! Rebuild the Flather external-target arrays for the current step: the
   ! (constant or streamed) tide scalar broadcast uniform along each
   ! flather face.  A boundary wavemaker then ADDS its incident series on
   ! top (wavemaker_add_flather_target), so the two superpose on one face;
   ! the kernel flux_flather_bc reads these arrays every RK stage.
   ! ----------------------------------------------------------------
   subroutine tide_build_flather_target(this)
      class(type_model_tide), intent(inout) :: this

      if (.not. allocated(this%eta_ext)) return

      this%eta_ext = 0.0_SP
      this%u_ext = 0.0_SP
      this%v_ext = 0.0_SP

      if (this%flather(FACE_W)) then
         this%eta_ext(:, FACE_W) = this%eta_west
         this%u_ext(:, FACE_W) = this%u_west
         this%v_ext(:, FACE_W) = this%v_west
      end if
      if (this%flather(FACE_E)) then
         this%eta_ext(:, FACE_E) = this%eta_east
         this%u_ext(:, FACE_E) = this%u_east
         this%v_ext(:, FACE_E) = this%v_east
      end if
      if (this%flather(FACE_S)) then
         this%eta_ext(:, FACE_S) = this%eta_south
         this%u_ext(:, FACE_S) = this%u_south
         this%v_ext(:, FACE_S) = this%v_south
      end if
      if (this%flather(FACE_N)) then
         this%eta_ext(:, FACE_N) = this%eta_north
         this%u_ext(:, FACE_N) = this%u_north
         this%v_ext(:, FACE_N) = this%v_north
      end if

   end subroutine tide_build_flather_target

   ! ----------------------------------------------------------------
   ! Per-stage boundary relaxation (legacy TIDE_BC), wet cells only:
   !   $$ \eta := \eta_{tide} + (\eta - \eta_{tide})\,s(i,j) $$
   ! (u, v likewise), over LOCAL strips on every rank in legacy order
   ! west/east/south/north — the corner cells of two active adjacent
   ! boundaries relax twice, second pass on the first's result.
   ! NOTE: legacy loops run 1..Iwidth unclamped and index out of
   ! bounds when Iwidth exceeds the local extent; the clamp here is
   ! the punch-listed deviation (legacy reads/writes heap garbage).
   ! ----------------------------------------------------------------
   subroutine tide_apply_bc(this, mask, eta, u, v)
      class(type_model_tide), intent(in) :: this
      integer, intent(in) :: mask(:, :)
      real(SP), intent(inout) :: eta(:, :), u(:, :), v(:, :)

      integer :: i, j, mloc, nloc

      mloc = size(eta, 1)
      nloc = size(eta, 2)

      if (this%tide_west .and. .not. this%flather(FACE_W)) then
         do j = 1, nloc
            do i = 1, min(this%iwidth(FACE_W), mloc)
               if (mask(i, j) == 1) then
                  eta(i, j) = this%eta_west + (eta(i, j) - this%eta_west)*this%sponge_west(i, j)
                  u(i, j) = this%u_west + (u(i, j) - this%u_west)*this%sponge_west(i, j)
                  v(i, j) = this%v_west + (v(i, j) - this%v_west)*this%sponge_west(i, j)
               end if
            end do
         end do
      end if

      if (this%tide_east .and. .not. this%flather(FACE_E)) then
         do j = 1, nloc
            do i = max(1, mloc - this%iwidth(FACE_E) + 1), mloc
               if (mask(i, j) == 1) then
                  eta(i, j) = this%eta_east + (eta(i, j) - this%eta_east)*this%sponge_east(i, j)
                  u(i, j) = this%u_east + (u(i, j) - this%u_east)*this%sponge_east(i, j)
                  v(i, j) = this%v_east + (v(i, j) - this%v_east)*this%sponge_east(i, j)
               end if
            end do
         end do
      end if

      if (this%tide_south .and. .not. this%flather(FACE_S)) then
         do j = 1, min(this%iwidth(FACE_S), nloc)
            do i = 1, mloc
               if (mask(i, j) == 1) then
                  eta(i, j) = this%eta_south + (eta(i, j) - this%eta_south)*this%sponge_south(i, j)
                  u(i, j) = this%u_south + (u(i, j) - this%u_south)*this%sponge_south(i, j)
                  v(i, j) = this%v_south + (v(i, j) - this%v_south)*this%sponge_south(i, j)
               end if
            end do
         end do
      end if

      if (this%tide_north .and. .not. this%flather(FACE_N)) then
         do j = max(1, nloc - this%iwidth(FACE_N) + 1), nloc
            do i = 1, mloc
               if (mask(i, j) == 1) then
                  eta(i, j) = this%eta_north + (eta(i, j) - this%eta_north)*this%sponge_north(i, j)
                  u(i, j) = this%u_north + (u(i, j) - this%u_north)*this%sponge_north(i, j)
                  v(i, j) = this%v_north + (v(i, j) - this%v_north)*this%sponge_north(i, j)
               end if
            end do
         end do
      end if

   end subroutine tide_apply_bc

   subroutine tide_free(this)
      class(type_model_tide), intent(inout) :: this

      integer :: f

      if (allocated(this%sponge_west)) deallocate (this%sponge_west)
      if (allocated(this%sponge_east)) deallocate (this%sponge_east)
      if (allocated(this%sponge_south)) deallocate (this%sponge_south)
      if (allocated(this%sponge_north)) deallocate (this%sponge_north)

      do f = FACE_W, FACE_N
         call this%series(f)%close()
      end do

   end subroutine tide_free

end module model_tide_mod
