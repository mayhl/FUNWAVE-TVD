!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Moving vessels / ship wakes (legacy mod_vessel.F, VESSEL_MODULE)
!
!  Each hull is a moving forcing region that tracks a prescribed path.  Three
!  source types exist in legacy; two are reachable (see NOTE 4):
!
!    PRESSURE ('PR')  a moving pressure patch on the free surface (Ertekin et
!                     al., JFM 1986).  Feeds the momentum source as
!                     -g H grad(P), and imprints its own initial draft on eta.
!    SLENDER  ('SL')  a moving mass-flux dipole (Tanimoto et al. 2000), added
!                     straight into the continuity RHS.
!
!  Optional sub-features, cpp flags in legacy (-DPROPELLER, -DDEEP_DRAFT_VESSEL)
!  and runtime switches here, because the modern layer is not preprocessed:
!
!    propeller   a submerged jet trailing the stern.  Writes up/vp/upc ONLY;
!                the hydrodynamics never read them, so propeller-on must
!                reproduce propeller-off BITWISE.  upc is the bed-shear
!                contribution the sediment module consumes (6g-6).
!    deep_draft  a hull whose draft clears the bed by less than `clearance`:
!                blanks mask9, and adds a drag and/or an eddy viscosity.
!                This one IS two-way.
!
!  TWO-WAY: the pressure gradient and the slender-body flux gradient both feed
!  the hydrodynamics, so a vessel run cannot be checked against a vessel-free
!  one.  Only the propeller sub-feature has the bitwise ON==OFF property.
!
!  Cartesian only, and legacy is explicit about it: the module divides by a
!  bare scalar DX/DY, and etauv_solver adds the flux gradient only inside
!  `#if defined (CARTESIAN)`.  See NOTE 6.
!
!  YAML block: vessel:                 (top-level; omit for no vessel)
!    folder:    <path>  required; holds vessel_00001, ... (nee VESSEL_FOLDER)
!    count:     <int>   default 1  (nee NumVessel)
!    propeller: <bool>  default F  (nee PROPELLER; per-hull jet specs stay in
!                       the vessel_NNNNN files)
!    deep_draft:        presence = near-bed hull handling (nee DEEP_DRAFT)
!      clearance: <real>  draft-to-bed gap threshold [m], required
!                         (nee CLEARANCE; anchors the block)
!      mask:      <bool>  default T  blank mask9 (nee MaskMethod)
!      cd:        <real>  presence = hull drag (nee FrictionMethod +
!                         CdDeepDraft; NOTE: legacy defaulted the drag ON at
!                         0.1 — a block without cd runs drag-free)
!      nu:        <real>  presence = hull eddy viscosity [m^2/s]
!                         (nee ViscosityMethod + VisDeepDraft)
!    OUT_VESSEL:       <bool>  default T     write the resistance time series
!    PLOT_INTV_VESSEL: <real>  default SMALL resistance output interval  [s]
!                      (both legacy-spelled until the rung-5 output move)
!
!  Legacy call shape: VESSEL_INITIAL from init; VESSEL_FORCING once per step,
!  after ESTIMATE_DT (so TIME is already advanced) and BEFORE the RK loop.
!
!  Bug-for-bug notes vs legacy:
!    1. NOTE: a vessel VANISHES when its track file runs out, rather than
!       stopping.  The `READ(...,END=120)` jumps past the source calls, and
!       p_total/flux_grad were zeroed at the top of the step, so the hull
!       simply stops forcing.  The `! no more data` branch that sets
!       tmp1=tmp2=0 is unreachable dead code.  Ported as-is (`cycle`)
!    2. NOTE: a track that starts at t > 0 is REJECTED (init_compute), not
!       ported.  Legacy leaves ThetaVessel uninitialised until the first path
!       point is read, and meanwhile tmp1=tmp2=0 makes Xves=Yves=0 -- so the
!       hull is imprinted at the ORIGIN, at a garbage heading, and
!       MakeVesselDraft bakes that crater into eta/eta0.  It reads uninitialised
!       memory, so there is no behaviour to reproduce.  Every stock case starts
!       its track at t=0, which is why this has never bitten.  Deferred vessel
!       arrival is punch-listed as the real feature
!    3. NOTE: resistance is accumulated with `res_x = res_pos_x + res_neg_x`
!       INSIDE the i/j loop, so it is recomputed every cell.  Harmless (the
!       final value is the one that lands) but kept, to read against legacy
!    4. NOTE: source type 'PA' (panel / Green function) is REJECTED.  Legacy
!       reads the geometry line only under 'PR' or 'SL', so a 'PA' hull leaves
!       length/width/alpha/beta/p uninitialised AND misaligns the track read by
!       one line.  It has never been exercised -- every stock vessel file is
!       PRESSURE.  Its one compilable branch is also a byte-for-byte duplicate
!       of SLENDER type 1; the REALISTIC_VESSEL_BODY branch it guards does not
!       compile at all (declarations after executables, unbalanced parens, a
!       missing operator, and rank-1 Xco indexed as rank-2)
!    5. NOTE: the deep-draft stamp writes a (2*N_GHOST+1)^2 block around every
!       cell it trips on, reaching i-N_GHOST..i+N_GHOST.  On the interior loop
!       bounds that runs off the end of the local array at the edges -- legacy's
!       own comment calls it "mask9 with 25 points rather than 9".  Ported with
!       the same bounds, clamped only to stay in-array
!    6. NOTE: legacy divides by a bare scalar DX/DY here, so the module is
!       Cartesian-only; under -DSPHERICAL the flux gradient is silently dropped
!       by etauv_solver (`#if defined (CARTESIAN)`) while the pressure gradient
!       is still applied.  Ported Cartesian-only, off grid%dx0
!    7. NOTE: upc_total is halo-exchanged with a scalar mirror (legacy
!       PHI_COLL, VTYPE=1).  The jet is evaluated analytically at every local
!       point INCLUDING the ghosts, so the exchange overwrites good values with
!       mirrored ones at a physical wall -- but sediment reads upc at i-1/j-1,
!       so the mirror is load-bearing.  Ported
!
!  HISTORY :
!    07/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_vessel_mod
   use core_constants_mod, only: SP, ZERO, SMALL, PI, GRAV, RHO_WATER, N_GHOST, MPI_SP
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_time_utils_mod, only: type_timing_control
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base
   use model_bc_mod, only: type_model_bc
   use model_config_defaults_mod, only: DEF_VESSEL_COUNT, DEF_VESSEL_OUT_VESSEL, &
                                        DEF_VESSEL_PROPELLER, &
                                        DEF_VESSEL_DEEP_DRAFT_MASK
   use mpi_f08

   implicit none

   private
   ! the three source kernels are lp-driven and take the hull index, so the
   ! unit test can drive one hull without a step
   public :: type_model_vessel, vessel_pressure_source, vessel_slender_source

   type, extends(type_model_base) :: type_model_vessel

      ! ── config ────────────────────────────────────────────────────────────
      character(:), allocatable :: vessel_folder
      character(:), allocatable :: result_folder
      integer  :: n_vessel = 0
      logical  :: out_vessel = .true.
      real(SP) :: plot_intv = ZERO
      logical  :: propeller = .false.
      logical  :: deep_draft = .false.

      ! deep-draft knobs
      logical  :: mask_method = .true.
      logical  :: friction_method = .true.
      logical  :: viscosity_method = .false.
      real(SP) :: clearance = 1.0_SP
      real(SP) :: cd_deep_draft = 0.1_SP
      real(SP) :: vis_deep_draft = 0.1_SP

      ! ── per-hull table (from vessel_NNNNN) ────────────────────────────────
      character(80), allocatable :: source_type(:)
      integer, allocatable  :: vessel_type(:)
      real(SP), allocatable :: length(:), width(:)
      real(SP), allocatable :: alpha1(:), alpha2(:), beta(:), p_ves(:)

      ! path state: segment endpoints 1 (behind) and 2 (ahead)
      real(SP), allocatable :: t1(:), x1(:), y1(:)
      real(SP), allocatable :: t2(:), x2(:), y2(:)
      real(SP), allocatable :: theta(:), u_vel(:), v_vel(:)
      integer, allocatable  :: unit_track(:)

      ! resistance, per hull
      real(SP), allocatable :: res_x(:), res_y(:)
      real(SP), allocatable :: res_pos_x(:), res_neg_x(:)
      real(SP), allocatable :: res_pos_y(:), res_neg_y(:)

      ! ── fields ────────────────────────────────────────────────────────────
      real(SP), allocatable :: p_total(:, :), p_each(:, :)
      real(SP), allocatable :: p_x(:, :), p_y(:, :)
      real(SP), allocatable :: flux_grad(:, :), flux_grad_each(:, :)

      ! deep draft
      real(SP), allocatable :: cd_2d(:, :), vis_2d(:, :)
      integer, allocatable  :: mask_vessel(:, :)

      ! propeller: derived constants, then the jet
      real(SP), allocatable :: n_rev(:), d_prop(:), c_thrust(:), d_hub(:)
      real(SP), allocatable :: bar(:), h_prop(:), c_duct(:)
      real(SP), allocatable :: veff(:), cf(:), izef(:)
      real(SP), allocatable :: up(:, :), vp(:, :)
      real(SP), allocatable :: up_total(:, :), vp_total(:, :)
      real(SP), allocatable :: upc(:, :), upc_total(:, :)

      ! ── grid-point lattice, ghost-inclusive (grid%x is interior-only) ─────
      real(SP), allocatable :: xco(:), yco(:)
      real(SP) :: dx0 = ZERO, dy0 = ZERO

      ! legacy MakeVesselDraft: imprint the hull on eta once, on the first step
      logical :: make_draft = .true.

      ! output
      type(type_timing_control) :: trigger
      integer :: unit_res = -1
      logical :: is_io_rank = .false.
      logical :: opened = .false.

   contains
      procedure, public :: read_input => vessel_read_input
      procedure, public :: init_compute => vessel_init_compute
      procedure, public :: update => vessel_update
      procedure, public :: write_output => vessel_write_output
      procedure, public :: free => vessel_free
   end type type_model_vessel

contains

   subroutine vessel_read_input(this, env)
      class(type_model_vessel), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: blk
      logical :: no_blk, no_key
      real(SP) :: tmp_r

      sub_env = get_sub_env(env, "vessel", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (no_blk) return

      call sub_env%yaml%read("folder", silent=no_key, val=this%vessel_folder)
      if (no_key) call env%log%exit_on_error( &
         "vessel: folder is required (it holds vessel_00001, ...)")

      call sub_env%yaml%read("count", silent=no_key, val=this%n_vessel, &
                             default=DEF_VESSEL_COUNT)
      call sub_env%yaml%read("OUT_VESSEL", silent=no_key, val=this%out_vessel, &
                             default=DEF_VESSEL_OUT_VESSEL)
      ! legacy "PLOT_INTV_VESSEL not specified, use SMALL" -- SMALL, not the
      ! main output interval, so an absent key writes resistance every step
      call sub_env%yaml%read("PLOT_INTV_VESSEL", silent=no_key, val=this%plot_intv)
      if (no_key) this%plot_intv = SMALL

      call sub_env%yaml%read("propeller", silent=no_key, val=this%propeller, &
                             default=DEF_VESSEL_PROPELLER)

      ! deep_draft block presence = near-bed hull handling (nee the DEEP_DRAFT
      ! bool + the three Method bools)
      blk = sub_env%yaml%cast_dictionary("deep_draft", no_key)
      this%deep_draft = .not. no_key
      if (this%deep_draft) then
         ! clearance anchors the block (an empty block reads as absent)
         call blk%read("clearance", silent=no_key, val=tmp_r)
         if (no_key) call env%log%exit_on_error( &
            "vessel: deep_draft requires clearance (the draft-to-bed gap threshold)")
         this%clearance = tmp_r
         call blk%read("mask", silent=no_key, val=this%mask_method, &
                       default=DEF_VESSEL_DEEP_DRAFT_MASK)
         ! cd/nu presence derives the drag and eddy-viscosity switches
         call blk%read("cd", silent=no_key, val=tmp_r)
         this%friction_method = .not. no_key
         if (this%friction_method) this%cd_deep_draft = tmp_r
         call blk%read("nu", silent=no_key, val=tmp_r)
         this%viscosity_method = .not. no_key
         if (this%viscosity_method) this%vis_deep_draft = tmp_r
      end if

   end subroutine vessel_read_input

   ! Legacy VESSEL_INITIAL: open one vessel_NNNNN per hull, read its geometry
   ! and first track point, and leave the unit open -- VESSEL_FORCING streams
   ! the rest of the track from it, one segment at a time.
   subroutine vessel_init_compute(this, grid, env, result_folder, t_start)
      class(type_model_vessel), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      type(type_env), intent(inout) :: env
      character(*), intent(in) :: result_folder
      real(SP), intent(in) :: t_start

      integer :: k, i, j, ios, u
      character(len=256) :: fname
      character(len=80)  :: hull_name
      logical :: found
      real(SP) :: e0

      if (.not. this%is_activated) return

      this%result_folder = result_folder
      this%is_io_rank = env%comm%is_io_node()

      if (this%n_vessel < 1) call env%log%exit_on_error( &
         "vessel: NumVessel must be >= 1")

      associate (n => this%n_vessel, lp => grid%lp)

         allocate (this%source_type(n), this%vessel_type(n))
         allocate (this%length(n), this%width(n))
         allocate (this%alpha1(n), this%alpha2(n), this%beta(n), this%p_ves(n))
         allocate (this%t1(n), this%x1(n), this%y1(n))
         allocate (this%t2(n), this%x2(n), this%y2(n))
         allocate (this%theta(n), this%u_vel(n), this%v_vel(n))
         allocate (this%unit_track(n))
         allocate (this%res_x(n), this%res_y(n))
         allocate (this%res_pos_x(n), this%res_neg_x(n))
         allocate (this%res_pos_y(n), this%res_neg_y(n))

         ! legacy zeroes Uvel/Vvel but NOT ThetaVessel; we zero all three, and
         ! reject the only config that could read theta before it is set (NOTE 2)
         this%theta = ZERO
         this%u_vel = ZERO
         this%v_vel = ZERO
         this%res_x = ZERO
         this%res_y = ZERO
         this%res_pos_x = ZERO
         this%res_neg_x = ZERO
         this%res_pos_y = ZERO
         this%res_neg_y = ZERO

         allocate (this%p_total(lp%mloc, lp%nloc), this%p_each(lp%mloc, lp%nloc))
         allocate (this%p_x(lp%mloc, lp%nloc), this%p_y(lp%mloc, lp%nloc))
         allocate (this%flux_grad(lp%mloc, lp%nloc))
         allocate (this%flux_grad_each(lp%mloc, lp%nloc))
         this%p_total = ZERO
         this%p_each = ZERO
         this%p_x = ZERO
         this%p_y = ZERO
         this%flux_grad = ZERO
         this%flux_grad_each = ZERO

         if (this%deep_draft) then
            allocate (this%cd_2d(lp%mloc, lp%nloc), this%vis_2d(lp%mloc, lp%nloc))
            allocate (this%mask_vessel(lp%mloc, lp%nloc))
            this%cd_2d = ZERO
            this%vis_2d = ZERO
            this%mask_vessel = 1
         end if

         if (this%propeller) then
            allocate (this%n_rev(n), this%d_prop(n), this%c_thrust(n), this%d_hub(n))
            allocate (this%bar(n), this%h_prop(n), this%c_duct(n))
            allocate (this%veff(n), this%cf(n), this%izef(n))
            allocate (this%up(lp%mloc, lp%nloc), this%vp(lp%mloc, lp%nloc))
            allocate (this%up_total(lp%mloc, lp%nloc), this%vp_total(lp%mloc, lp%nloc))
            allocate (this%upc(lp%mloc, lp%nloc), this%upc_total(lp%mloc, lp%nloc))
            this%up = ZERO
            this%vp = ZERO
            this%up_total = ZERO
            this%vp_total = ZERO
            this%upc = ZERO
            this%upc_total = ZERO
         end if

         ! ghost-inclusive grid-point lattice; grid%x/grid%dx are interior-only
         this%dx0 = grid%dx(1, 1)
         this%dy0 = grid%dy(1, 1)
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

         do k = 1, n
            write (fname, '(A,"vessel_",I5.5)') trim(this%vessel_folder), k

            inquire (file=trim(fname), exist=found)
            if (.not. found) call env%log%exit_on_error( &
               "vessel: "//trim(fname)//" is listed by NumVessel but cannot be found")

            open (newunit=u, file=trim(fname), status="old", action="read", iostat=ios)
            if (ios /= 0) call env%log%exit_on_error( &
               "vessel: cannot open "//trim(fname))
            this%unit_track(k) = u

            read (u, '(A80)', iostat=ios) hull_name
            read (u, *, iostat=ios) this%source_type(k), this%vessel_type(k)
            if (ios /= 0) call env%log%exit_on_error( &
               "vessel: "//trim(fname)//": cannot parse the source-type line")

            read (u, *, iostat=ios)   ! geometry header

            select case (this%source_type(k) (1:2))
            case ("PR", "SL")
               read (u, *, iostat=ios) this%length(k), this%width(k), &
                  this%alpha1(k), this%alpha2(k), this%beta(k), this%p_ves(k)
               if (ios /= 0) call env%log%exit_on_error( &
                  "vessel: "//trim(fname)//": cannot parse the geometry line")
            case default
               ! 'PA' and anything else: legacy reads no geometry at all and
               ! then misaligns the track read (NOTE 4)
               call env%log%exit_on_error( &
                  "vessel: "//trim(fname)//": source type '"// &
                  trim(this%source_type(k))//"' is not supported; legacy never "// &
                  "reads its geometry line, leaving length/width/alpha/beta/P "// &
                  "uninitialised and the track read off by one line.  Use "// &
                  "PRESSURE or SLENDER")
            end select

            ! legacy clamps only type 1, and only alpha1/beta, to (SMALL, 1]
            if (this%source_type(k) (1:2) == "PR" .and. this%vessel_type(k) == 1) then
               this%alpha1(k) = max(SMALL, this%alpha1(k))
               this%beta(k) = max(SMALL, this%beta(k))
               this%alpha1(k) = min(1.0_SP, this%alpha1(k))
               this%beta(k) = min(1.0_SP, this%beta(k))
            end if

            if (this%propeller .and. this%source_type(k) (1:2) == "PR") then
               read (u, *, iostat=ios)   ! propeller header
               read (u, *, iostat=ios) this%n_rev(k), this%d_prop(k), this%c_thrust(k), &
                  this%d_hub(k), this%bar(k), this%h_prop(k), this%c_duct(k)
               if (ios /= 0) call env%log%exit_on_error( &
                  "vessel: "//trim(fname)//": propeller is on but the propeller "// &
                  "line (N_revolution, D_prop, C_thrust, D_hub, BAR, H_prop, "// &
                  "C_duct) is missing or unparseable")

               e0 = (this%d_prop(k)/this%d_hub(k))**(-0.403_SP) &
                    *this%c_thrust(k)**(-1.79_SP)*this%bar(k)**(0.744_SP)
               this%veff(k) = e0*this%n_rev(k)*this%d_prop(k)*sqrt(this%c_thrust(k))
               this%cf(k) = 0.01_SP*(this%d_prop(k)/this%h_prop(k))
               this%izef(k) = this%d_prop(k)/2.0_SP/this%c_duct(k)
            end if

            read (u, *, iostat=ios)   ! track header
            read (u, *, iostat=ios) this%t2(k), this%x2(k), this%y2(k)
            if (ios /= 0) call env%log%exit_on_error( &
               "vessel: "//trim(fname)//": cannot parse the first track point")

            ! legacy seeds segment 1 == segment 2, so the hull sits still until
            ! the first VESSEL_FORCING advances the track
            this%t1(k) = this%t2(k)
            this%x1(k) = this%x2(k)
            this%y1(k) = this%y2(k)

            if (this%t2(k) > t_start) call env%log%exit_on_error( &
               "vessel: "//trim(fname)//": the track starts after the simulation "// &
               "does.  Legacy would imprint this hull at the ORIGIN, at an "// &
               "uninitialised heading, until its start time -- there is no "// &
               "behaviour to reproduce.  Start the track at or before the "// &
               "simulation start time")
         end do

         this%trigger%t_start = t_start
         this%trigger%interval = this%plot_intv
         this%trigger%last_triggered = ZERO
         this%trigger%accum = ZERO

      end associate

   end subroutine vessel_init_compute

   ! Legacy VESSEL_FORCING: advance every hull along its track, rebuild the
   ! pressure / flux-gradient fields, and (deep draft) restamp the hull mask.
   ! Called once per step at the already-advanced TIME, before the RK loop.
   subroutine vessel_update(this, bc, grid, time, dt, eta, eta0, depth, h)
      class(type_model_vessel), intent(inout) :: this
      type(type_model_bc), intent(in) :: bc
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: time, dt
      real(SP), intent(inout) :: eta(:, :), eta0(:, :)
      real(SP), intent(in) :: depth(:, :), h(:, :)

      integer :: k, i, j, ii, jj, ios
      real(SP) :: tmp1, tmp2, xves, yves

      if (.not. this%is_activated) return

      associate (lp => grid%lp)

         this%flux_grad = ZERO
         this%p_total = ZERO
         if (this%propeller) then
            this%up_total = ZERO
            this%vp_total = ZERO
            this%upc_total = ZERO
         end if

         hull: do k = 1, this%n_vessel

            if (time > this%t1(k) .and. time > this%t2(k)) then

               this%t1(k) = this%t2(k)
               this%x1(k) = this%x2(k)
               this%y1(k) = this%y2(k)

               ! legacy walks the track until the far endpoint is past the step,
               ! so a dt larger than the track interval cannot stall the hull
               do while (this%t2(k) < time + dt)
                  read (this%unit_track(k), *, iostat=ios) &
                     this%t2(k), this%x2(k), this%y2(k)
                  ! track exhausted: legacy's END= jumps past every source call
                  ! below, and p_total/flux_grad are already zero -- so the hull
                  ! silently VANISHES rather than stopping (NOTE 1)
                  if (ios /= 0) cycle hull
               end do

               this%theta(k) = atan2(this%y2(k) - this%y1(k), this%x2(k) - this%x1(k))

               if ((this%t2(k) - this%t1(k)) > ZERO) then
                  this%u_vel(k) = (this%x2(k) - this%x1(k))/(this%t2(k) - this%t1(k))
                  this%v_vel(k) = (this%y2(k) - this%y1(k))/(this%t2(k) - this%t1(k))
               end if

            end if

            ! linear interpolation along the current segment
            tmp1 = ZERO
            tmp2 = ZERO
            if (time > this%t1(k)) then
               if (this%t1(k) == this%t2(k)) then
                  ! unreachable: the EOF cycle above takes this path out (NOTE 1)
                  tmp1 = ZERO
                  tmp2 = ZERO
               else
                  tmp2 = (this%t2(k) - time) &
                         /max(SMALL, abs(this%t2(k) - this%t1(k)))
                  tmp1 = 1.0_SP - tmp2
               end if
            end if

            xves = this%x2(k)*tmp1 + this%x1(k)*tmp2
            yves = this%y2(k)*tmp1 + this%y1(k)*tmp2

            select case (this%source_type(k) (1:2))
            case ("PR")
               call vessel_pressure_source(this, grid, k, xves, yves, eta)
               this%p_total = this%p_total + this%p_each
               if (this%propeller) then
                  this%up_total = this%up_total + this%up
                  this%vp_total = this%vp_total + this%vp
                  this%upc_total = this%upc_total + this%upc
               end if
            case ("SL")
               call vessel_slender_source(this, grid, k, xves, yves)
               this%flux_grad = this%flux_grad + this%flux_grad_each
            end select

         end do hull

         if (this%deep_draft) then
            this%cd_2d = ZERO
            this%vis_2d = ZERO
            this%mask_vessel = 1
         end if

         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie

               ! legacy flipped the sign here in 2016: -g H grad(P)
               this%p_x(i, j) = -GRAV*h(i, j) &
                                *(this%p_total(i + 1, j) - this%p_total(i - 1, j)) &
                                /2.0_SP/this%dx0
               this%p_y(i, j) = -GRAV*h(i, j) &
                                *(this%p_total(i, j + 1) - this%p_total(i, j - 1)) &
                                /2.0_SP/this%dy0

               if (this%deep_draft) then
                  if (this%p_total(i, j) > ZERO) then
                     if (depth(i, j) - this%p_total(i, j) < this%clearance) then
                        ! (2*N_GHOST+1)^2 stamp, clamped to the array (NOTE 5)
                        do jj = max(1, j - N_GHOST), min(lp%nloc, j + N_GHOST)
                           do ii = max(1, i - N_GHOST), min(lp%mloc, i + N_GHOST)
                              if (this%mask_method) this%mask_vessel(ii, jj) = 0
                              if (this%friction_method) &
                                 this%cd_2d(ii, jj) = this%cd_deep_draft
                              if (this%viscosity_method) &
                                 this%vis_2d(ii, jj) = this%vis_deep_draft
                           end do
                        end do
                     end if
                  end if
               end if

            end do
         end do

         ! legacy MakeVesselDraft: on the first step only, sink the free surface
         ! to the hull's own pressure signature so it starts in its own hole
         if (this%make_draft) then
            this%make_draft = .false.
            do j = 1, lp%nloc
               do i = 1, lp%mloc
                  if (abs(this%p_total(i, j)) > SMALL) then
                     eta(i, j) = -this%p_total(i, j)
                     eta0(i, j) = eta(i, j)
                  end if
               end do
            end do
         end if

         ! scalar mirror + halo (legacy PHI_COLL, VTYPE=1) -- load-bearing for
         ! sediment, which reads upc at i-1/j-1 (NOTE 7)
         if (this%propeller .and. grid%nx_proc*grid%ny_proc > 1) then
            call bc%exchange_scalar(grid, this%upc_total)
         end if

      end associate

   end subroutine vessel_update

   ! Legacy PRESSURE_SOURCE: a moving pressure patch, plus (propeller) the
   ! stern jet, plus the wave-resistance integral over the patch.
   subroutine vessel_pressure_source(this, grid, k, xves, yves, eta)
      class(type_model_vessel), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      integer, intent(in) :: k
      real(SP), intent(in) :: xves, yves
      real(SP), intent(in) :: eta(:, :)

      integer :: i, j, ierr
      real(SP) :: lves, wves, p_x, p_y
      real(SP) :: xrear, yrear, lrear, wrear, uup
      real(SP) :: detax, detay, myvar
      real(SP) :: cth, sth

      associate (lp => grid%lp, len => this%length(k), wid => this%width(k), &
                 a1 => this%alpha1(k), a2 => this%alpha2(k), bt => this%beta(k))

         this%p_each = ZERO
         this%res_x(k) = ZERO
         this%res_y(k) = ZERO
         this%res_pos_x(k) = ZERO
         this%res_neg_x(k) = ZERO
         this%res_pos_y(k) = ZERO
         this%res_neg_y(k) = ZERO

         cth = cos(this%theta(k))
         sth = sin(this%theta(k))

         if (this%propeller) then
            xrear = xves - len*cth
            yrear = yves - len*sth
         end if

         do j = 1, lp%nloc
            do i = 1, lp%mloc

               ! hull frame: lves along the keel, wves across the beam
               lves = (this%xco(i) - xves)*cth + (this%yco(j) - yves)*sth
               wves = -(this%xco(i) - xves)*sth + (this%yco(j) - yves)*cth

               if (abs(lves) <= 0.5_SP*len .and. abs(wves) <= 0.5_SP*wid) then

                  ! Ertekin et al. JFM 1986: flat-topped cosine-tapered patch
                  if (this%vessel_type(k) == 1) then
                     p_x = ZERO
                     p_y = ZERO

                     if (lves > 0.5_SP*len*a1 .and. lves < 0.5_SP*len) then
                        p_x = cos(PI*(lves - 0.5_SP*a1*len)/((1.0_SP - a1)*len))**2
                     else if (lves < -0.5_SP*len*a2 .and. lves > -0.5_SP*len) then
                        p_x = cos(PI*(abs(lves) - 0.5_SP*a2*len)/((1.0_SP - a2)*len))**2
                     else if (lves <= 0.5_SP*len*a1 .and. lves >= -0.5_SP*len*a2) then
                        p_x = 1.0_SP
                     end if

                     if (abs(wves) > 0.5_SP*wid*bt .and. abs(wves) < 0.5_SP*wid) then
                        p_y = cos(PI*(abs(wves) - 0.5_SP*bt*wid)/((1.0_SP - bt)*wid))**2
                     else if (abs(wves) <= 0.5_SP*wid*bt) then
                        p_y = 1.0_SP
                     end if

                     this%p_each(i, j) = this%p_ves(k)*p_x*p_y
                  end if

                  ! Divid & Volker 2017: alpha1=cl, alpha2=cb, beta=a
                  if (this%vessel_type(k) == 2) then
                     this%p_each(i, j) = this%p_ves(k) &
                                         *(1.0_SP - a1*(lves/len)**4) &
                                         *(1.0_SP - a2*(wves/wid)**2) &
                                         *exp(-bt*(wves/wid)**2)
                  end if

               end if

               if (this%propeller) then
                  lrear = (this%xco(i) - xrear)*cth + (this%yco(j) - yrear)*sth
                  wrear = -(this%xco(i) - xrear)*sth + (this%yco(j) - yrear)*cth

                  ! jet only astern; a near-field plateau inside the duct
                  ! length, a spreading Gaussian beyond it
                  if (lrear < ZERO) then
                     if (lrear > -this%izef(k)) then
                        uup = this%veff(k) &
                              *exp(-2.0_SP*(wrear**2 + this%h_prop(k)**2) &
                                   /this%d_prop(k)**2)
                     else
                        uup = this%veff(k)*this%d_prop(k)/2.0_SP/this%c_thrust(k) &
                              /abs(lrear) &
                              *exp(-(wrear**2 + this%h_prop(k)**2)/2.0_SP &
                                   /this%c_thrust(k)**2/lrear**2)
                     end if
                  else
                     uup = ZERO
                  end if

                  this%up(i, j) = -uup*cth
                  this%vp(i, j) = -uup*sth
                  this%upc(i, j) = uup*sqrt(this%cf(k)/2.0_SP)
               end if

            end do
         end do

         ! Wave resistance: integrate P dEta/dx over the patch.  Interior only --
         ! legacy used to run this inside the grid loop above and double-counted
         ! the ghosts (Jeff Harris).  Split pos/neg so the sign breakdown is
         ! reportable.
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie

               detax = (eta(i + 1, j) - eta(i - 1, j))/2.0_SP/this%dx0
               detay = (eta(i, j + 1) - eta(i, j - 1))/2.0_SP/this%dy0

               if (detax >= ZERO) then
                  this%res_pos_x(k) = this%res_pos_x(k) &
                                      + this%p_each(i, j)*RHO_WATER*GRAV*detax*this%dy0
               else
                  this%res_neg_x(k) = this%res_neg_x(k) &
                                      + this%p_each(i, j)*RHO_WATER*GRAV*detax*this%dy0
               end if

               if (detay >= ZERO) then
                  this%res_pos_y(k) = this%res_pos_y(k) &
                                      + this%p_each(i, j)*RHO_WATER*GRAV*detay*this%dx0
               else
                  this%res_neg_y(k) = this%res_neg_y(k) &
                                      + this%p_each(i, j)*RHO_WATER*GRAV*detay*this%dx0
               end if

               ! recomputed every cell, exactly as legacy (NOTE 3)
               this%res_x(k) = this%res_pos_x(k) + this%res_neg_x(k)
               this%res_y(k) = this%res_pos_y(k) + this%res_neg_y(k)

            end do
         end do

         if (grid%nx_proc*grid%ny_proc > 1) then
            call MPI_Allreduce(this%res_x(k), myvar, 1, MPI_SP, MPI_SUM, &
                               grid%cart_comm, ierr)
            this%res_x(k) = myvar
            call MPI_Allreduce(this%res_y(k), myvar, 1, MPI_SP, MPI_SUM, &
                               grid%cart_comm, ierr)
            this%res_y(k) = myvar
            call MPI_Allreduce(this%res_pos_x(k), myvar, 1, MPI_SP, MPI_SUM, &
                               grid%cart_comm, ierr)
            this%res_pos_x(k) = myvar
            call MPI_Allreduce(this%res_pos_y(k), myvar, 1, MPI_SP, MPI_SUM, &
                               grid%cart_comm, ierr)
            this%res_pos_y(k) = myvar
            call MPI_Allreduce(this%res_neg_x(k), myvar, 1, MPI_SP, MPI_SUM, &
                               grid%cart_comm, ierr)
            this%res_neg_x(k) = myvar
            call MPI_Allreduce(this%res_neg_y(k), myvar, 1, MPI_SP, MPI_SUM, &
                               grid%cart_comm, ierr)
            this%res_neg_y(k) = myvar
         end if

      end associate

   end subroutine vessel_pressure_source

   ! Legacy SLENDER_BODY_SOURCE: a mass-flux dipole along the keel (Tanimoto
   ! et al. 2000) -- a source ahead of midships, a sink behind, so the hull
   ! displaces water without a pressure patch.
   subroutine vessel_slender_source(this, grid, k, xves, yves)
      class(type_model_vessel), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      integer, intent(in) :: k
      real(SP), intent(in) :: xves, yves

      integer :: i, j
      real(SP) :: lves, wves, cth, sth
      real(SP) :: x_right_1, x_right_2, x_left_1, x_left_2

      associate (lp => grid%lp, len => this%length(k), wid => this%width(k), &
                 a1 => this%alpha1(k), a2 => this%alpha2(k))

         this%flux_grad_each = ZERO

         cth = cos(this%theta(k))
         sth = sin(this%theta(k))

         do j = 1, lp%nloc
            do i = 1, lp%mloc

               lves = (this%xco(i) - xves)*cth + (this%yco(j) - yves)*sth
               wves = -(this%xco(i) - xves)*sth + (this%yco(j) - yves)*cth

               if (abs(lves) <= 0.5_SP*len .and. abs(wves) <= 0.5_SP*wid) then

                  ! one full sine along the keel: +ve bow, -ve stern
                  if (this%vessel_type(k) == 1) then
                     this%flux_grad_each(i, j) = this%p_ves(k) &
                                                 *sin(2.0*PI*lves/len) &
                                                 *cos(PI*wves/wid)**2
                  end if

                  ! type 2: flat-bottomed parallel midbody, taper only at the
                  ! ends, so the dipole is confined to bow and stern
                  if (this%vessel_type(k) == 2) then
                     x_right_1 = 0.5_SP*len*a1
                     x_right_2 = 0.5_SP*len
                     x_left_1 = -0.5_SP*len*a2
                     x_left_2 = -0.5_SP*len

                     if (lves > x_right_1 .and. lves < x_right_2) then
                        this%flux_grad_each(i, j) = this%p_ves(k) &
                                                    *sin(PI*(lves - x_right_1)/(x_right_2 - x_right_1)) &
                                                    *cos(PI*wves/wid)**2
                     else if (lves < x_left_1 .and. lves > x_left_2) then
                        this%flux_grad_each(i, j) = this%p_ves(k) &
                                                    *sin(PI*(lves - x_left_1)/(x_left_1 - x_left_2)) &
                                                    *cos(PI*wves/wid)**2
                     else if (lves <= x_right_1 .and. lves >= x_left_1) then
                        this%flux_grad_each(i, j) = ZERO
                     end if
                  end if

               end if

            end do
         end do

      end associate

   end subroutine vessel_slender_source

   ! Legacy OUTPUT_VESSEL: the resistance time series, one row per trigger.
   ! (Its header and terminator were lost upstream -- see the io.F repair.)
   subroutine vessel_write_output(this, t, dt)
      class(type_model_vessel), intent(inout) :: this
      real(SP), intent(in) :: t, dt

      integer :: k

      if (.not. this%is_activated) return
      if (.not. this%out_vessel) return
      if (.not. this%is_io_rank) return
      if (.not. this%trigger%should_trigger(t, dt)) return

      if (.not. this%opened) then
         this%opened = .true.
         open (newunit=this%unit_res, file=this%result_folder//"Resis.txt", &
               status="replace", action="write")
      end if

      write (this%unit_res, '(60E16.5)') t, &
         (this%res_x(k), k=1, this%n_vessel), &
         (this%res_pos_x(k), k=1, this%n_vessel), &
         (this%res_neg_x(k), k=1, this%n_vessel), &
         (this%res_y(k), k=1, this%n_vessel), &
         (this%res_pos_y(k), k=1, this%n_vessel), &
         (this%res_neg_y(k), k=1, this%n_vessel)

   end subroutine vessel_write_output

   subroutine vessel_free(this)
      class(type_model_vessel), intent(inout) :: this

      integer :: k
      logical :: is_open

      if (allocated(this%unit_track)) then
         do k = 1, size(this%unit_track)
            inquire (unit=this%unit_track(k), opened=is_open)
            if (is_open) close (this%unit_track(k))
         end do
      end if

      if (this%opened) then
         close (this%unit_res)
         this%opened = .false.
      end if

      if (allocated(this%vessel_folder)) deallocate (this%vessel_folder)
      if (allocated(this%result_folder)) deallocate (this%result_folder)
      if (allocated(this%source_type)) deallocate (this%source_type)
      if (allocated(this%vessel_type)) deallocate (this%vessel_type)
      if (allocated(this%length)) deallocate (this%length)
      if (allocated(this%width)) deallocate (this%width)
      if (allocated(this%alpha1)) deallocate (this%alpha1)
      if (allocated(this%alpha2)) deallocate (this%alpha2)
      if (allocated(this%beta)) deallocate (this%beta)
      if (allocated(this%p_ves)) deallocate (this%p_ves)
      if (allocated(this%t1)) deallocate (this%t1)
      if (allocated(this%x1)) deallocate (this%x1)
      if (allocated(this%y1)) deallocate (this%y1)
      if (allocated(this%t2)) deallocate (this%t2)
      if (allocated(this%x2)) deallocate (this%x2)
      if (allocated(this%y2)) deallocate (this%y2)
      if (allocated(this%theta)) deallocate (this%theta)
      if (allocated(this%u_vel)) deallocate (this%u_vel)
      if (allocated(this%v_vel)) deallocate (this%v_vel)
      if (allocated(this%unit_track)) deallocate (this%unit_track)
      if (allocated(this%res_x)) deallocate (this%res_x)
      if (allocated(this%res_y)) deallocate (this%res_y)
      if (allocated(this%res_pos_x)) deallocate (this%res_pos_x)
      if (allocated(this%res_neg_x)) deallocate (this%res_neg_x)
      if (allocated(this%res_pos_y)) deallocate (this%res_pos_y)
      if (allocated(this%res_neg_y)) deallocate (this%res_neg_y)
      if (allocated(this%p_total)) deallocate (this%p_total)
      if (allocated(this%p_each)) deallocate (this%p_each)
      if (allocated(this%p_x)) deallocate (this%p_x)
      if (allocated(this%p_y)) deallocate (this%p_y)
      if (allocated(this%flux_grad)) deallocate (this%flux_grad)
      if (allocated(this%flux_grad_each)) deallocate (this%flux_grad_each)
      if (allocated(this%cd_2d)) deallocate (this%cd_2d)
      if (allocated(this%vis_2d)) deallocate (this%vis_2d)
      if (allocated(this%mask_vessel)) deallocate (this%mask_vessel)
      if (allocated(this%n_rev)) deallocate (this%n_rev)
      if (allocated(this%d_prop)) deallocate (this%d_prop)
      if (allocated(this%c_thrust)) deallocate (this%c_thrust)
      if (allocated(this%d_hub)) deallocate (this%d_hub)
      if (allocated(this%bar)) deallocate (this%bar)
      if (allocated(this%h_prop)) deallocate (this%h_prop)
      if (allocated(this%c_duct)) deallocate (this%c_duct)
      if (allocated(this%veff)) deallocate (this%veff)
      if (allocated(this%cf)) deallocate (this%cf)
      if (allocated(this%izef)) deallocate (this%izef)
      if (allocated(this%up)) deallocate (this%up)
      if (allocated(this%vp)) deallocate (this%vp)
      if (allocated(this%up_total)) deallocate (this%up_total)
      if (allocated(this%vp_total)) deallocate (this%vp_total)
      if (allocated(this%upc)) deallocate (this%upc)
      if (allocated(this%upc_total)) deallocate (this%upc_total)
      if (allocated(this%xco)) deallocate (this%xco)
      if (allocated(this%yco)) deallocate (this%yco)

      this%is_activated = .false.

   end subroutine vessel_free

end module model_vessel_mod
