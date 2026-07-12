!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Tide/surge boundary conditions (legacy mod_tide.F, TIDE_MODULE)
!
!  YAML block: tide:       (top-level; omit for no tidal BC)
!    TIDAL_BC_ABS:      <bool>   absorbing-only mode, default NO
!    TIDAL_BC_GEN_ABS:  <bool>   generating-absorbing mode (rides the ABS
!                                wavemaker relaxation), default NO
!    TideBcType:        CONSTANT | DATA   default CONSTANT
!    WaveMakerPointNum: <int>    relaxation-strip width in cells, default 30
!    TideWest_ETA/U/V:  <real>   CONSTANT targets; ETA presence enables the
!                                boundary, U/V default 0 (all four boundaries)
!    TideWestFileName:  <path>   DATA series; presence enables the boundary
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
!    3. NOTE: DATA EOF on any boundary skips the remaining boundaries AND the
!       current interpolation (END=120 jumps to the subroutine tail), so all
!       targets freeze at their last computed values
!    4. NOTE: DATA targets are ZERO until TIME passes the first record (both
!       interpolation weights stay 0), not the first record's values
!    5. NOTE: eta/u/v are relaxed but P/Q/HU/HV are not rebuilt — the
!       inconsistency rides into the next stage's fluxes like legacy
!
!  HISTORY :
!    07/11/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_tide_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_TIDE_TIDAL_BC_ABS, &
                                        DEF_TIDE_TIDAL_BC_GEN_ABS, &
                                        DEF_TIDE_TIDEBCTYPE, &
                                        DEF_TIDE_WAVEMAKERPOINTNUM

   implicit none

   private
   public :: type_model_tide

   ! legacy TIDE_SPONGE constants (mod_tide.F:247-249) and PARAM SMALL
   real(SP), parameter :: R_SP_TIDE = 0.85_SP
   real(SP), parameter :: A_SP_TIDE = 10.0_SP
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
      integer :: iwidth = 30                     ! WaveMakerPointNum

      ! per-boundary enables (legacy default .TRUE., knocked out by a
      ! missing TideX_ETA / TideXFileName)
      logical :: tide_west = .true., tide_east = .true.
      logical :: tide_south = .true., tide_north = .true.

      ! current relaxation targets — CONSTANT values, or the DATA
      ! interpolants refreshed by update_data
      real(SP) :: eta_west = 0.0_SP, u_west = 0.0_SP, v_west = 0.0_SP
      real(SP) :: eta_east = 0.0_SP, u_east = 0.0_SP, v_east = 0.0_SP
      real(SP) :: eta_south = 0.0_SP, u_south = 0.0_SP, v_south = 0.0_SP
      real(SP) :: eta_north = 0.0_SP, u_north = 0.0_SP, v_north = 0.0_SP

      type(type_path) :: file_west, file_east, file_south, file_north

      ! DATA streaming state: open unit + bracketing records per boundary
      ! newunit units are negative; -1 marks never-opened
      integer :: unit_west = -1, unit_east = -1
      integer :: unit_south = -1, unit_north = -1
      real(SP) :: t1_w = 0.0_SP, e1_w = 0.0_SP, su1_w = 0.0_SP, sv1_w = 0.0_SP
      real(SP) :: t2_w = 0.0_SP, e2_w = 0.0_SP, su2_w = 0.0_SP, sv2_w = 0.0_SP
      real(SP) :: t1_e = 0.0_SP, e1_e = 0.0_SP, su1_e = 0.0_SP, sv1_e = 0.0_SP
      real(SP) :: t2_e = 0.0_SP, e2_e = 0.0_SP, su2_e = 0.0_SP, sv2_e = 0.0_SP
      real(SP) :: t1_s = 0.0_SP, e1_s = 0.0_SP, su1_s = 0.0_SP, sv1_s = 0.0_SP
      real(SP) :: t2_s = 0.0_SP, e2_s = 0.0_SP, su2_s = 0.0_SP, sv2_s = 0.0_SP
      real(SP) :: t1_n = 0.0_SP, e1_n = 0.0_SP, su1_n = 0.0_SP, sv1_n = 0.0_SP
      real(SP) :: t2_n = 0.0_SP, e2_n = 0.0_SP, su2_n = 0.0_SP, sv2_n = 0.0_SP

      ! inverted relaxation profiles (legacy SPONGE_TIDE_*), local
      ! ghost-inclusive windows
      real(SP), allocatable :: sponge_west(:, :), sponge_east(:, :)
      real(SP), allocatable :: sponge_south(:, :), sponge_north(:, :)

   contains
      procedure :: read_input => tide_read_input
      procedure :: init_compute => tide_init_compute
      procedure :: update_data => tide_update_data
      procedure :: apply_bc => tide_apply_bc
      procedure :: data_mode => tide_data_mode
      procedure :: free => tide_free
   end type type_model_tide

contains

   subroutine tide_read_input(this, env)
      class(type_model_tide), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      character(:), allocatable :: bc_type
      logical :: no_blk, no_key

      sub_env = get_sub_env(env, "tide", is_empty=no_blk)
      if (no_blk) return

      call sub_env%yaml%read("WaveMakerPointNum", silent=no_key, &
                             val=this%iwidth, default=DEF_TIDE_WAVEMAKERPOINTNUM)
      call sub_env%yaml%read("TIDAL_BC_ABS", silent=no_key, &
                             val=this%tidal_bc_abs, default=DEF_TIDE_TIDAL_BC_ABS)
      call sub_env%yaml%read("TIDAL_BC_GEN_ABS", silent=no_key, &
                             val=this%tidal_bc_gen_abs, default=DEF_TIDE_TIDAL_BC_GEN_ABS)

      this%is_activated = this%tidal_bc_abs .or. this%tidal_bc_gen_abs
      if (.not. this%is_activated) return

      call sub_env%yaml%read("TideBcType", silent=no_key, &
                             val=bc_type, default=DEF_TIDE_TIDEBCTYPE)
      this%tide_bc_type = bc_type   ! fixed-len copy pads short values

      if (this%tide_bc_type(1:4) == 'CONS') then
         ! legacy Tide_READ_CONSTANT: ETA presence enables the boundary,
         ! U/V default to zero independently
         call sub_env%yaml%read("TideWest_ETA", silent=no_key, val=this%eta_west)
         if (no_key) this%tide_west = .false.
         call sub_env%yaml%read("TideWest_U", silent=no_key, val=this%u_west)
         call sub_env%yaml%read("TideWest_V", silent=no_key, val=this%v_west)

         call sub_env%yaml%read("TideEast_ETA", silent=no_key, val=this%eta_east)
         if (no_key) this%tide_east = .false.
         call sub_env%yaml%read("TideEast_U", silent=no_key, val=this%u_east)
         call sub_env%yaml%read("TideEast_V", silent=no_key, val=this%v_east)

         call sub_env%yaml%read("TideSouth_ETA", silent=no_key, val=this%eta_south)
         if (no_key) this%tide_south = .false.
         call sub_env%yaml%read("TideSouth_U", silent=no_key, val=this%u_south)
         call sub_env%yaml%read("TideSouth_V", silent=no_key, val=this%v_south)

         call sub_env%yaml%read("TideNorth_ETA", silent=no_key, val=this%eta_north)
         if (no_key) this%tide_north = .false.
         call sub_env%yaml%read("TideNorth_U", silent=no_key, val=this%u_north)
         call sub_env%yaml%read("TideNorth_V", silent=no_key, val=this%v_north)
      end if

      if (this%tide_bc_type(1:4) == 'DATA') then
         call sub_env%yaml%read_input_path("TideWestFileName", silent=no_key, &
                                           val=this%file_west)
         if (no_key) this%tide_west = .false.
         call sub_env%yaml%read_input_path("TideEastFileName", silent=no_key, &
                                           val=this%file_east)
         if (no_key) this%tide_east = .false.
         call sub_env%yaml%read_input_path("TideSouthFileName", silent=no_key, &
                                           val=this%file_south)
         if (no_key) this%tide_south = .false.
         call sub_env%yaml%read_input_path("TideNorthFileName", silent=no_key, &
                                           val=this%file_north)
         if (no_key) this%tide_north = .false.
      end if

   end subroutine tide_read_input

   ! ----------------------------------------------------------------
   ! Relaxation profiles + DATA series open (legacy TIDE_INITIAL tail).
   ! The west profile in local index i on cart rank npx of px:
   !   $$ r_i = R^{\lfloor 50\,(i + n_{px} M_{glob}/p_x - 1)
   !            /(I_w - 1) \rfloor}, \qquad
   !      s(i) = 1/\max(A^{r_i},\ 1) $$
   ! with R = 0.85, A = 10 — the exponent is INTEGER division like
   ! legacy, and every rank fills its whole window (the global offset
   ! keeps the profile continuous across seams).  NOTE: the serial
   ! legacy build has a north typo (Nloc - i, mod_tide.F:295); the
   ! vendored legacy is a PARALLEL build, so the j form is the one
   ! reproduced here.
   ! ----------------------------------------------------------------
   subroutine tide_init_compute(this, grid)
      class(type_model_tide), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid

      real(SP) :: ri
      integer :: i, j, mloc, nloc

      if (.not. this%is_activated) return

      mloc = grid%lp%mloc
      nloc = grid%lp%nloc
      allocate (this%sponge_west(mloc, nloc), this%sponge_east(mloc, nloc), &
                this%sponge_south(mloc, nloc), this%sponge_north(mloc, nloc))

      associate (Mglob => grid%M, Nglob => grid%N, &
                 px => grid%nx_proc, py => grid%ny_proc, &
                 npx => grid%iproc, npy => grid%jproc, iw => this%iwidth)
         do j = 1, nloc
            do i = 1, mloc
               ri = R_SP_TIDE**(50*(i + npx*Mglob/px - 1)/(iw - 1))
               this%sponge_west(i, j) = max(A_SP_TIDE**ri, LIM_TIDE)
               ri = R_SP_TIDE**(50*(mloc - i + (px - npx - 1)*Mglob/px)/(iw - 1))
               this%sponge_east(i, j) = max(A_SP_TIDE**ri, LIM_TIDE)
               ri = R_SP_TIDE**(50*(j + npy*Nglob/py - 1)/(iw - 1))
               this%sponge_south(i, j) = max(A_SP_TIDE**ri, LIM_TIDE)
               ri = R_SP_TIDE**(50*(nloc - j + (py - npy - 1)*Nglob/py)/(iw - 1))
               this%sponge_north(i, j) = max(A_SP_TIDE**ri, LIM_TIDE)
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
         if (this%tide_west) call series_open(this%file_west%root, this%unit_west, &
                                              this%t1_w, this%e1_w, this%su1_w, this%sv1_w, &
                                              this%t2_w, this%e2_w, this%su2_w, this%sv2_w)
         if (this%tide_east) call series_open(this%file_east%root, this%unit_east, &
                                              this%t1_e, this%e1_e, this%su1_e, this%sv1_e, &
                                              this%t2_e, this%e2_e, this%su2_e, this%sv2_e)
         if (this%tide_south) call series_open(this%file_south%root, this%unit_south, &
                                               this%t1_s, this%e1_s, this%su1_s, this%sv1_s, &
                                               this%t2_s, this%e2_s, this%su2_s, this%sv2_s)
         if (this%tide_north) call series_open(this%file_north%root, this%unit_north, &
                                               this%t1_n, this%e1_n, this%su1_n, this%sv1_n, &
                                               this%t2_n, this%e2_n, this%su2_n, this%sv2_n)
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

   subroutine series_open(fname, unit, t1, e1, su1, sv1, t2, e2, su2, sv2)
      character(*), intent(in) :: fname
      integer, intent(out) :: unit
      real(SP), intent(out) :: t1, e1, su1, sv1, t2, e2, su2, sv2

      character(len=80) :: header

      open (newunit=unit, file=fname, status='old', action='read')
      read (unit, '(A80)') header
      read (unit, *) t2, e2, su2, sv2
      t1 = t2; e1 = e2; su1 = su2; sv1 = sv2
   end subroutine series_open

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
   subroutine tide_update_data(this, time, dt)
      class(type_model_tide), intent(inout) :: this
      real(SP), intent(in) :: time, dt

      logical :: hit_eof

      if (this%tide_west) then
         call series_advance(this%unit_west, time, dt, &
                             this%t1_w, this%e1_w, this%su1_w, this%sv1_w, &
                             this%t2_w, this%e2_w, this%su2_w, this%sv2_w, &
                             this%eta_west, this%u_west, this%v_west, hit_eof)
         if (hit_eof) return
      end if
      if (this%tide_east) then
         call series_advance(this%unit_east, time, dt, &
                             this%t1_e, this%e1_e, this%su1_e, this%sv1_e, &
                             this%t2_e, this%e2_e, this%su2_e, this%sv2_e, &
                             this%eta_east, this%u_east, this%v_east, hit_eof)
         if (hit_eof) return
      end if
      if (this%tide_south) then
         call series_advance(this%unit_south, time, dt, &
                             this%t1_s, this%e1_s, this%su1_s, this%sv1_s, &
                             this%t2_s, this%e2_s, this%su2_s, this%sv2_s, &
                             this%eta_south, this%u_south, this%v_south, hit_eof)
         if (hit_eof) return
      end if
      if (this%tide_north) then
         call series_advance(this%unit_north, time, dt, &
                             this%t1_n, this%e1_n, this%su1_n, this%sv1_n, &
                             this%t2_n, this%e2_n, this%su2_n, this%sv2_n, &
                             this%eta_north, this%u_north, this%v_north, hit_eof)
         if (hit_eof) return
      end if

   end subroutine tide_update_data

   subroutine series_advance(unit, time, dt, t1, e1, su1, sv1, &
                             t2, e2, su2, sv2, tgt_eta, tgt_u, tgt_v, hit_eof)
      integer, intent(in) :: unit
      real(SP), intent(in) :: time, dt
      real(SP), intent(inout) :: t1, e1, su1, sv1, t2, e2, su2, sv2
      real(SP), intent(inout) :: tgt_eta, tgt_u, tgt_v
      logical, intent(out) :: hit_eof

      real(SP) :: w1, w2
      integer :: ios

      hit_eof = .false.

      if (time > t1 .and. time > t2) then
         t1 = t2; e1 = e2; su1 = su2; sv1 = sv2
         do while (t2 < time + dt)
            ! EOF leaves t2/e2/su2/sv2 at the last read record, and the
            ! caller abandons the rest of the update (legacy END=120)
            read (unit, *, iostat=ios) t2, e2, su2, sv2
            if (ios /= 0) then
               hit_eof = .true.
               return
            end if
         end do
      end if

      w2 = 0.0_SP
      w1 = 0.0_SP
      if (time > t1) then
         ! exact-equality bracket collapse like legacy (single record,
         ! or duplicate times)
         if (t1 == t2) then
            w2 = 0.0_SP
            w1 = 0.0_SP
         else
            w2 = (t2 - time)/max(SMALL, abs(t2 - t1))
            w1 = 1.0_SP - w2
         end if
      end if

      tgt_u = su2*w1 + su1*w2
      tgt_v = sv2*w1 + sv1*w2
      tgt_eta = e2*w1 + e1*w2

   end subroutine series_advance

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

      if (this%tide_west) then
         do j = 1, nloc
            do i = 1, min(this%iwidth, mloc)
               if (mask(i, j) == 1) then
                  eta(i, j) = this%eta_west + (eta(i, j) - this%eta_west)*this%sponge_west(i, j)
                  u(i, j) = this%u_west + (u(i, j) - this%u_west)*this%sponge_west(i, j)
                  v(i, j) = this%v_west + (v(i, j) - this%v_west)*this%sponge_west(i, j)
               end if
            end do
         end do
      end if

      if (this%tide_east) then
         do j = 1, nloc
            do i = max(1, mloc - this%iwidth + 1), mloc
               if (mask(i, j) == 1) then
                  eta(i, j) = this%eta_east + (eta(i, j) - this%eta_east)*this%sponge_east(i, j)
                  u(i, j) = this%u_east + (u(i, j) - this%u_east)*this%sponge_east(i, j)
                  v(i, j) = this%v_east + (v(i, j) - this%v_east)*this%sponge_east(i, j)
               end if
            end do
         end do
      end if

      if (this%tide_south) then
         do j = 1, min(this%iwidth, nloc)
            do i = 1, mloc
               if (mask(i, j) == 1) then
                  eta(i, j) = this%eta_south + (eta(i, j) - this%eta_south)*this%sponge_south(i, j)
                  u(i, j) = this%u_south + (u(i, j) - this%u_south)*this%sponge_south(i, j)
                  v(i, j) = this%v_south + (v(i, j) - this%v_south)*this%sponge_south(i, j)
               end if
            end do
         end do
      end if

      if (this%tide_north) then
         do j = max(1, nloc - this%iwidth + 1), nloc
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

      logical :: opened

      if (allocated(this%sponge_west)) deallocate (this%sponge_west)
      if (allocated(this%sponge_east)) deallocate (this%sponge_east)
      if (allocated(this%sponge_south)) deallocate (this%sponge_south)
      if (allocated(this%sponge_north)) deallocate (this%sponge_north)

      if (this%unit_west /= -1) then
         inquire (unit=this%unit_west, opened=opened)
         if (opened) close (this%unit_west)
      end if
      if (this%unit_east /= -1) then
         inquire (unit=this%unit_east, opened=opened)
         if (opened) close (this%unit_east)
      end if
      if (this%unit_south /= -1) then
         inquire (unit=this%unit_south, opened=opened)
         if (opened) close (this%unit_south)
      end if
      if (this%unit_north /= -1) then
         inquire (unit=this%unit_north, opened=opened)
         if (opened) close (this%unit_north)
      end if

   end subroutine tide_free

end module model_tide_mod
