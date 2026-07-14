!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Subgrid porosity/depth (legacy mod_subgrid.F, SUBGRID_MODULE)
!
!  Sub-cell blocks (buildings, jetties, urban obstructions) that are too
!  small to resolve on the main grid.  Each main cell carries a
!  SubMainGridRatio x SubMainGridRatio panel of pixel depths; the wet
!  fraction becomes a porosity that scales the mass residual, and the
!  pixel-averaged water column replaces H.
!
!  YAML block: subgrid:            (top-level; omit for no subgrid)
!    SubMainGridRatio:   <int>     default 1   pixels per main-cell side
!    DEPTH_SUBGRID_FILE: <path>    sparse panel file (required in practice)
!    Porosity:           <bool>    default NO  write porosity.ini at init
!
!  Panel file: one record per subgrid main cell,
!    Ix  Iy  d(1,1) d(2,1) ... d(r,r)      (column-major within the panel)
!  with Ix/Iy GLOBAL 1-based interior indices.  Listed cells get
!  MaskSubgrid = 1; every other cell is filled with the main-grid depth.
!
!  Legacy call shape: SUBGRID_INITIAL from init.F before the bathymetry
!  correction; UPDATE_SUBGRID at the head of GET_Eta_U_V_HU_HV (every RK
!  stage), so the porosity a stage divides by is the one the PREVIOUS
!  stage left behind.  Coupling is exactly two terms:
!    etauv_solver.F:249   R1 := R1 / Porosity      (after the wavemaker
!                                                   mass, before rainfall)
!    etauv_solver.F:380   H  := DepAvgSubgrid      (where MaskSubgrid = 1)
!
!  Bug-for-bug notes vs legacy:
!    1. NOTE: porosity and the pixel average are rebuilt over the WHOLE
!       array including ghosts, from eta that is one halo exchange behind
!       — the ghost ring is stale by a stage, exactly as legacy
!    2. NOTE: the panel scatter replicates depth into the ghost ring
!       (parallel GetFile_Subgrid -> GLOB_to_LOC); the serial legacy build
!       instead leaves ghost MaskSubgrid at 0.  We port the parallel path,
!       as with the obstacle mask (punch-listed deviation from serial)
!    3. NOTE: a zero-porosity cell is impossible — the < SMALL clamp turns
!       dry cells into porosity 1 — so legacy's "porosity = 0" warning
!       branch in the eta residual is DEAD.  Not ported
!    4. NOTE: DepMaxSubgrid and rMASKS are computed/allocated by legacy and
!       never read.  Not ported
!    5. NOTE: the panel depths are the RAW file values — legacy builds them
!       before the bathymetry correction runs, so slope-cap smoothing of
!       the main depth never reaches the subgrid panels (nor the porosity
!       of the non-subgrid cells, which is 1 either way)
!    6. NOTE: legacy re-reads DEPTH_FILE inside SUBGRID_INITIAL, so a
!       SUBGRID build silently overrides a FLAT/SLOPE bathymetry with the
!       depth file.  The modern reader keeps geometry's depth (identical
!       whenever DEPTH_TYPE = DATA, the only configuration legacy supports)
!
!  HISTORY :
!    07/13/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_subgrid_mod
   use core_constants_mod, only: SP, N_GHOST, SMALL, LARGE, ZERO
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use model_base_mod, only: type_model_base

   use model_config_defaults_mod, only: DEF_SUBGRID_SUBMAINGRIDRATIO, &
                                        DEF_SUBGRID_POROSITY

   implicit none

   private
   public :: type_model_subgrid

   type, extends(type_model_base) :: type_model_subgrid

      integer :: ratio = 1              ! SubMainGridRatio
      integer :: num_pixel = 1          ! ratio**2
      logical :: out_porosity = .false.

      type(type_path) :: depth_subgrid_file

      ! local ghost-inclusive windows; dep_sub is the pixel panel
      real(SP), allocatable :: porosity(:, :)
      real(SP), allocatable :: dep_avg(:, :)
      real(SP), allocatable :: dep_sub(:, :, :, :)
      integer, allocatable :: mask_sub(:, :)

   contains
      procedure :: read_input => subgrid_read_input
      procedure :: init_compute => subgrid_init_compute
      procedure :: update => subgrid_update
      procedure :: apply_h => subgrid_apply_h
      procedure :: free => subgrid_free
   end type type_model_subgrid

contains

   subroutine subgrid_read_input(this, env)
      class(type_model_subgrid), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      logical :: no_blk, no_key

      sub_env = get_sub_env(env, "subgrid", is_empty=no_blk)
      this%is_activated = .not. no_blk
      if (no_blk) return

      call sub_env%yaml%read("SubMainGridRatio", silent=no_key, &
                             val=this%ratio, &
                             default=DEF_SUBGRID_SUBMAINGRIDRATIO)
      call sub_env%yaml%read("Porosity", silent=no_key, &
                             val=this%out_porosity, &
                             default=DEF_SUBGRID_POROSITY)

      call sub_env%yaml%read_input_path("DEPTH_SUBGRID_FILE", silent=no_key, &
                                        val=this%depth_subgrid_file)
      if (no_key) then
         call env%log%exit_on_error( &
            "subgrid: DEPTH_SUBGRID_FILE is required")
      end if

      this%num_pixel = this%ratio*this%ratio

   end subroutine subgrid_read_input

   ! ----------------------------------------------------------------
   ! Legacy SUBGRID_INITIAL: load the panels, then seed the porosity and
   ! pixel average from the STILL-WATER column (eta = 0 implied):
   !   $$ \phi = \frac{n_{wet}}{r^2}, \qquad
   !      \bar{d} = \frac{1}{r^2}\sum_{wet} d_{ij} $$
   ! A dry cell (no wet pixel) gets $\bar{d} = -LARGE$ and porosity 0,
   ! which the clamp below lifts to 1 so the eta residual never divides
   ! by zero.  Non-subgrid cells hold the main depth in every pixel, so
   ! their porosity is 1 (wet) or clamped to 1 (dry) — and UPDATE never
   ! revisits them.
   !
   ! `depth` must be the ghost-filled, PRE-correction depth (header
   ! NOTE 5).  Every rank reads the panel file and builds the global
   ! array, then slices its own window: legacy allocates the same global
   ! on every rank anyway, and this keeps init MPI-free (obstacle
   ! precedent).
   ! ----------------------------------------------------------------
   subroutine subgrid_init_compute(this, grid, depth, env)
      class(type_model_subgrid), intent(inout) :: this
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: depth(:, :)
      type(type_env), intent(inout) :: env

      real(SP), allocatable :: glob(:, :, :, :), mask_glob(:, :)
      real(SP), allocatable :: plane(:, :), tmp_read(:)
      integer :: i, j, ii, jj, ix, iy, unit, ios, rec
      logical :: exists

      if (.not. this%is_activated) return

      associate (lp => grid%lp, m => grid%M, n => grid%N, r => this%ratio)

         allocate (this%porosity(lp%mloc, lp%nloc), source=ZERO)
         allocate (this%dep_avg(lp%mloc, lp%nloc), source=ZERO)
         allocate (this%mask_sub(lp%mloc, lp%nloc), source=0)
         allocate (this%dep_sub(lp%mloc, lp%nloc, r, r), source=ZERO)

         inquire (file=this%depth_subgrid_file%root, exist=exists)
         if (.not. exists) then
            call env%log%exit_on_error( &
               "subgrid: cannot find "//this%depth_subgrid_file%root)
         end if

         allocate (glob(m, n, r, r), source=ZERO)
         allocate (mask_glob(m, n), source=ZERO)
         allocate (tmp_read(this%num_pixel))

         open (newunit=unit, file=this%depth_subgrid_file%root, &
               status="old", action="read")
         do rec = 1, m*n
            read (unit, *, iostat=ios) ix, iy, tmp_read
            if (ios /= 0) exit
            mask_glob(ix, iy) = 1.0_SP
            do jj = 1, r
               do ii = 1, r
                  glob(ix, iy, ii, jj) = tmp_read((jj - 1)*r + ii)
               end do
            end do
         end do
         close (unit)

         ! scatter panel by panel, then the listed-cell mask
         allocate (plane(lp%mloc, lp%nloc))
         do jj = 1, r
            do ii = 1, r
               call scatter_global(grid, glob(:, :, ii, jj), plane)
               this%dep_sub(:, :, ii, jj) = plane
            end do
         end do
         call scatter_global(grid, mask_glob, plane)
         this%mask_sub = int(plane)

         ! cells the file never listed are flat: the main depth everywhere
         do j = 1, lp%nloc
            do i = 1, lp%mloc
               if (this%mask_sub(i, j) < 1) then
                  this%dep_sub(i, j, :, :) = depth(i, j)
               end if
            end do
         end do

         call rebuild(this, lp%mloc, lp%nloc, all_cells=.true.)

      end associate

   end subroutine subgrid_init_compute

   ! ----------------------------------------------------------------
   ! Pad a global interior field into the ghost ring by edge replication
   ! (legacy GLOB_to_LOC: y walls over the interior columns first, then x
   ! walls over every row, so corners land on edge values), then hand back
   ! this rank's ghost-inclusive window.
   ! ----------------------------------------------------------------
   subroutine scatter_global(grid, glob, loc)
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(in) :: glob(:, :)
      real(SP), intent(out) :: loc(:, :)

      real(SP), allocatable :: pad(:, :)
      integer :: i, j, mp, np

      associate (m => grid%M, n => grid%N, ng => N_GHOST, lp => grid%lp)
         mp = m + 2*ng
         np = n + 2*ng
         allocate (pad(mp, np), source=ZERO)
         pad(ng + 1:m + ng, ng + 1:n + ng) = glob

         do i = ng + 1, m + ng
            do j = 1, ng
               pad(i, j) = pad(i, ng + 1)
            end do
            do j = n + ng + 1, np
               pad(i, j) = pad(i, n + ng)
            end do
         end do
         do j = 1, np
            do i = 1, ng
               pad(i, j) = pad(ng + 1, j)
            end do
            do i = m + ng + 1, mp
               pad(i, j) = pad(m + ng, j)
            end do
         end do

         do j = 1, lp%nloc
            do i = 1, lp%mloc
               loc(i, j) = pad(grid%ibegin - 1 + i, grid%jbegin - 1 + j)
            end do
         end do
      end associate

   end subroutine scatter_global

   ! ----------------------------------------------------------------
   ! Legacy UPDATE_SUBGRID: rebuild porosity and the pixel average from
   ! the current free surface,
   !   $$ \phi = \frac{1}{r^2}\left|\{\, \eta + d_{ij} > 0 \,\}\right|,
   !      \qquad \bar{d} = \frac{1}{r^2}
   !      \sum_{\eta + d_{ij} > 0} (\eta + d_{ij}) $$
   ! over the subgrid cells only.  Ghost cells read stale eta (header
   ! NOTE 1).
   ! ----------------------------------------------------------------
   subroutine subgrid_update(this, eta)
      class(type_model_subgrid), intent(inout) :: this
      real(SP), intent(in) :: eta(:, :)

      call rebuild(this, size(this%porosity, 1), size(this%porosity, 2), &
                   all_cells=.false., eta=eta)

   end subroutine subgrid_update

   ! Shared core of SUBGRID_INITIAL and UPDATE_SUBGRID.  all_cells sweeps
   ! every cell off the still-water column; otherwise only the subgrid
   ! cells, off eta.  Both end with the same dry-cell porosity clamp over
   ! the whole array.
   subroutine rebuild(this, mloc, nloc, all_cells, eta)
      class(type_model_subgrid), intent(inout) :: this
      integer, intent(in) :: mloc, nloc
      logical, intent(in) :: all_cells
      real(SP), intent(in), optional :: eta(:, :)

      real(SP) :: col, wet_sum
      integer :: i, j, ii, jj, pcount

      do j = 1, nloc
         do i = 1, mloc
            if (.not. all_cells) then
               if (this%mask_sub(i, j) /= 1) cycle
            end if

            wet_sum = ZERO
            pcount = 0
            this%porosity(i, j) = ZERO

            do jj = 1, this%ratio
               do ii = 1, this%ratio
                  col = this%dep_sub(i, j, ii, jj)
                  if (present(eta)) col = eta(i, j) + col
                  if (col > ZERO) then
                     wet_sum = wet_sum + col
                     pcount = pcount + 1
                  end if
               end do
            end do

            if (pcount == 0) then
               this%dep_avg(i, j) = -LARGE
            else
               this%dep_avg(i, j) = wet_sum/real(this%num_pixel, SP)
               this%porosity(i, j) = real(pcount, SP)/real(this%num_pixel, SP)
            end if
         end do
      end do

      ! a dry cell divides the eta residual by 1, not by 0
      do j = 1, nloc
         do i = 1, mloc
            if (this%porosity(i, j) < SMALL) this%porosity(i, j) = 1.0_SP
         end do
      end do

   end subroutine rebuild

   ! Legacy GET_Eta_U_V_HU_HV: the pixel-averaged water column replaces
   ! the main-grid H at subgrid cells (unclamped, whole array).
   subroutine subgrid_apply_h(this, h)
      class(type_model_subgrid), intent(inout) :: this
      real(SP), intent(inout) :: h(:, :)

      integer :: i, j

      do j = 1, size(h, 2)
         do i = 1, size(h, 1)
            if (this%mask_sub(i, j) == 1) h(i, j) = this%dep_avg(i, j)
         end do
      end do

   end subroutine subgrid_apply_h

   subroutine subgrid_free(this)
      class(type_model_subgrid), intent(inout) :: this

      if (allocated(this%porosity)) deallocate (this%porosity)
      if (allocated(this%dep_avg)) deallocate (this%dep_avg)
      if (allocated(this%dep_sub)) deallocate (this%dep_sub)
      if (allocated(this%mask_sub)) deallocate (this%mask_sub)
   end subroutine subgrid_free

end module model_subgrid_mod
