!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Model geometry / bathymetry YAML reader
!
!  YAML block: grid:          (nee geometry:; the 3-D model keeps geometry:)
!    cell_size: [dx, dy]        OR dx_file/dy_file for variable spacing
!    n_cells: [nx, ny]          global domain size (nee grid_size; bathymetry
!                               nx/ny). Required for flat/slope; for file
!                               bathymetry absent = inferred from the file
!                               (ASCII dimension scan) and present = an
!                               origin-anchored subset window (fit checked
!                               under --validate)
!    origin: [x0, y0]           optional, default [0, 0]
!    water_level: <real>        optional, default 0 — still-water offset
!                               above the bathy datum (nee WaterLevel);
!                               applied by main after bathy correction
!    n_procs: [px, py]          optional, absent = auto-decompose
!                               (nee decomposition nx_proc/ny_proc)
!    bathymetry:
!      type: flat | file | slope
!      depth: <real>            flat and slope
!      slope: <real>            slope only
!      x0: <real>               slope only, default 0
!      file: <path>             file only
!      file_type: ascii         file only, default ascii
!      correction: <bool>       file only, default false
!      smooth_below_depth: <real>  correction only, default -LARGE (off)
!      slope_cap: <real>        correction only, default 1.0
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!    05/13/2026  Renamed from grid.f90; new YAML schema
!
!-------------------------------------------------

module model_geometry_mod
   use core_constants_mod, only: SP, LARGE
   use core_comm_mod, only: type_comm
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d, type_loop_bounds
   use core_path_mod, only: type_path
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base
   use model_config_defaults_mod, only: DEF_GRID_WATER_LEVEL
   use model_kernel_bc_mod, only: fill_ghost_wall, SIGN_MIRROR

   implicit none

   private
   public :: type_model_geometry, read_field_ascii, stagger_depth

   character(len=8), parameter :: BATHY_TYPES(3) = &
                                  [character(len=8) :: "file", "flat", "slope"]
   character(len=8), parameter :: FILE_TYPES(1) = &
                                  [character(len=8) :: "ascii"]

   type, extends(type_model_base) :: type_model_geometry

      ! Grid spacing (uniform)
      real(SP) :: dx = 0.0_SP, dy = 0.0_SP
      ! Grid spacing (variable — file paths, variable type only)
      character(:), allocatable :: dx_file, dy_file

      ! Grid origin
      real(SP) :: x0 = 0.0_SP, y0 = 0.0_SP

      ! Still-water level above the bathy datum (nee WaterLevel); main
      ! adds it to depth + wavemaker reference depths after correction
      real(SP) :: water_level = 0.0_SP

      ! Integer grid dimensions (required for flat/slope; derived otherwise)
      integer :: grid_nx = 0, grid_ny = 0

      ! MPI decomposition (0 = auto)
      integer :: nx_proc = 0, ny_proc = 0

      ! Bathymetry
      character(:), allocatable :: bathy_type
      character(:), allocatable :: bathy_ftype
      type(type_path) :: bathy_file
      real(SP) :: bathy_depth = 0.0_SP
      real(SP) :: bathy_slope = 0.0_SP
      real(SP) :: bathy_slope_x0 = 0.0_SP
      logical :: bathy_correction = .false.
      real(SP) :: smooth_below_depth = -LARGE
      real(SP) :: slope_cap = 1.0_SP

   contains
      procedure :: read_input => geometry_read_input
      procedure :: build_grid => geometry_build_grid
      procedure :: init_depth => geometry_init_depth
      procedure :: correct_depth => geometry_correct_depth
   end type type_model_geometry

contains

   subroutine geometry_read_input(this, env)
      class(type_model_geometry), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: bathy_yaml
      real(SP), allocatable :: cell_size(:), origin(:)
      integer, allocatable :: n_cells(:), n_procs(:)
      integer :: file_nx, file_ny
      logical :: no_cell_size, no_origin, no_decomp, no_ncells
      logical :: no_dx_file, no_dy_file

      sub_env = get_sub_env(env, "grid")
      this%is_activated = .true.

      ! --- Spacing ---
      call sub_env%yaml%read("cell_size", silent=no_cell_size, val=cell_size)
      if (.not. no_cell_size) then
         this%dx = cell_size(1)
         this%dy = cell_size(2)
      else
         call sub_env%yaml%read("dx_file", silent=no_dx_file, val=this%dx_file)
         call sub_env%yaml%read("dy_file", silent=no_dy_file, val=this%dy_file)
         if (no_dx_file .and. no_dy_file) then
            call sub_env%log%exit_on_error( &
               "geometry: cell_size or dx_file/dy_file required")
         end if
         if (no_dx_file .neqv. no_dy_file) then
            call sub_env%log%exit_on_error( &
               "geometry: dx_file and dy_file must both be specified")
         end if
      end if

      ! --- Origin (optional) ---
      call sub_env%yaml%read("origin", silent=no_origin, val=origin)
      if (.not. no_origin) then
         this%x0 = origin(1)
         this%y0 = origin(2)
      end if

      ! --- Still-water level (optional) ---
      call sub_env%yaml%read("water_level", val=this%water_level, &
                             default=DEF_GRID_WATER_LEVEL)

      ! crs is metadata-only until the output stamp lands -- reserve it so
      ! unread-key detection stays quiet on georeferenced decks
      call sub_env%yaml%mark_reserved("crs")

      ! --- Decomposition (optional) ---
      ! a bare pair (nee decomposition nx_proc/ny_proc) — either present
      ! and valid or absent (auto), no half-states to guard
      call sub_env%yaml%read("n_procs", silent=no_decomp, val=n_procs)
      if (.not. no_decomp) then
         if (n_procs(1) <= 0 .or. n_procs(2) <= 0) then
            call sub_env%log%exit_on_error( &
               "geometry/n_procs: process counts must be positive")
         end if
         this%nx_proc = n_procs(1)
         this%ny_proc = n_procs(2)
      end if

      ! --- Domain size ---
      ! the single global-size key for every bathymetry type (nee grid_size;
      ! bathymetry nx/ny). flat/slope require it; file bathymetry infers an
      ! absent pair from the file itself and treats a present pair as an
      ! origin-anchored subset window
      call sub_env%yaml%read("n_cells", silent=no_ncells, val=n_cells)
      if (.not. no_ncells) then
         if (n_cells(1) <= 0 .or. n_cells(2) <= 0) then
            call sub_env%log%exit_on_error( &
               "grid/n_cells: cell counts must be positive")
         end if
         this%grid_nx = n_cells(1)
         this%grid_ny = n_cells(2)
      end if

      ! --- Bathymetry ---
      bathy_yaml = sub_env%yaml%cast_dictionary("bathymetry")
      call bathy_yaml%read_enum("type", BATHY_TYPES, val=this%bathy_type, default="file")

      select case (trim(this%bathy_type))
      case ("file")
         call bathy_yaml%read_enum("file_type", FILE_TYPES, val=this%bathy_ftype, default="ascii")
         call bathy_yaml%read_input_path("file", val=this%bathy_file)
         call bathy_yaml%read("correction", val=this%bathy_correction, default="NO")
         ! NOTE: 1. the vendored legacy still READ_FLOATs these two with the
         !          flat-txt parser, so under the YAML bridge it always lands
         !          on the defaults — pin defaults in any parity config
         !       2. defaults must ride the read call — yaml val is intent(out),
         !          a silent miss wipes the type initializer
         call bathy_yaml%read("smooth_below_depth", val=this%smooth_below_depth, &
                              default="-999999.0")
         call bathy_yaml%read("slope_cap", val=this%slope_cap, default="1.0")
         ! absent n_cells = infer the domain from the file; present = subset
         ! window, whose fit only --validate pays the scan to prove (a
         ! production run trusts the deck; an oversized window fails the
         ! row parse at init_depth)
         if (no_ncells .or. sub_env%yaml%unread_strict) then
            call scan_ascii_dims(sub_env, this%bathy_file%root, file_nx, file_ny)
         end if
         if (no_ncells) then
            this%grid_nx = file_nx
            this%grid_ny = file_ny
         else if (sub_env%yaml%unread_strict) then
            if (this%grid_nx > file_nx .or. this%grid_ny > file_ny) then
               call sub_env%log%exit_on_error( &
                  "grid/n_cells: window exceeds the bathymetry file dimensions")
            end if
         end if

      case ("flat")
         call bathy_yaml%read_positive("depth", val=this%bathy_depth)
         if (no_ncells) then
            call sub_env%log%exit_on_error( &
               "grid/n_cells: required for flat bathymetry")
         end if

      case ("slope")
         call bathy_yaml%read_positive("depth", val=this%bathy_depth)
         call bathy_yaml%read("slope", val=this%bathy_slope)
         call bathy_yaml%read("x0", val=this%bathy_slope_x0, default="0.0")
         if (no_ncells) then
            call sub_env%log%exit_on_error( &
               "grid/n_cells: required for slope bathymetry")
         end if
      end select

   end subroutine geometry_read_input

   ! ----------------------------------------------------------------
   ! Build the distributed grid from the parsed geometry config:
   ! decomposition (explicit or auto), cart topology, uniform spacing.
   ! periodic_y comes from physics%periodic (legacy PERIODIC, y only).
   ! Config validity is checked at read_input; the guards here are
   ! unimplemented-feature stops, not user-error handling.
   ! ----------------------------------------------------------------
   subroutine geometry_build_grid(this, comm, grid, periodic_y, periodic_x)
      class(type_model_geometry), intent(in)    :: this
      type(type_comm), intent(in)    :: comm
      type(type_grid_2d), intent(inout) :: grid
      logical, intent(in)    :: periodic_y
      logical, intent(in), optional :: periodic_x

      logical :: create_partition

      if (allocated(this%dx_file)) then
         error stop "geometry: variable spacing not yet implemented in new path"
      end if

      ! n_cells is unified: explicit, or inferred from the file at read_input
      grid%M = this%grid_nx
      grid%N = this%grid_ny

      create_partition = (this%nx_proc <= 0)
      if (.not. create_partition) then
         if (this%nx_proc*this%ny_proc /= comm%size) then
            error stop "geometry/decomposition: nx_proc*ny_proc must equal MPI size"
         end if
         grid%nx_proc = this%nx_proc
         grid%ny_proc = this%ny_proc
      end if

      block
         logical :: wrap_x
         wrap_x = .false.
         if (present(periodic_x)) wrap_x = periodic_x
         call grid%setup(comm, create_partition, periodic_y=periodic_y, &
                         periodic_x=wrap_x)
      end block
      call grid%init_spacing(this%dx, this%dy, this%x0, this%y0)

   end subroutine geometry_build_grid

   ! ----------------------------------------------------------------
   ! Fill still-water depth and face-staggered depths (ghost-inclusive
   ! arrays, fields_alloc layout).  Ports legacy init.F:
   !   interior:  flat  $d_i = d_0$, or slope (legacy io.F SLO branch)
   !              $$ d_i = d_0 - s\,(i - i_{slp})\,\Delta x, \quad
   !                 i \ge i_{slp} = \lfloor x_{slp}/\Delta x \rfloor + 1 $$
   !              with $d_i = d_0$ shoreward of $i_{slp}$ (global index);
   !   ghosts:    MPI halo exchange, then wall-mirror at physical
   !              boundaries (PHI_COLL VTYPE=1); y wraps when the cart
   !              topology is periodic;
   !   staggering: centred faces
   !              $$ d_{i-1/2} = \tfrac{1}{2}(d_{i-1} + d_i) $$
   !              one-sided extrapolation at the low array edge
   !              $$ d_{1/2} = \tfrac{1}{2}(3 d_1 - d_2) $$
   ! depth_x/depth_y are (mloc,nloc): the legacy Mloc1/Nloc1 high-edge
   ! face is dropped — kernels read faces up to ie+1/je+1 <= mloc/nloc.
   ! water_level lands later — main adds it after bathy correction and
   ! re-staggers, keeping the legacy init.F order.
   ! ----------------------------------------------------------------
   subroutine geometry_init_depth(this, grid, depth, depth_x, depth_y)
      class(type_model_geometry), intent(in)    :: this
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(inout) :: depth(:, :), depth_x(:, :), depth_y(:, :)

      integer :: i, j, k, ng, gi, i_slp

      associate (lp => grid%lp)

         select case (trim(this%bathy_type))
         case ("file")
            ! interior pre-loaded by the caller via read_field_ascii
            ! (keeps this routine env-free); ghosts/faces below
         case ("flat")
            depth = this%bathy_depth
         case ("slope")
            i_slp = int(this%bathy_slope_x0/this%dx) + 1
            do j = lp%jb, lp%je
               do i = lp%ib, lp%ie
                  gi = grid%ibegin + (i - lp%ib)
                  if (gi >= i_slp) then
                     depth(i, j) = this%bathy_depth - this%bathy_slope*real(gi - i_slp, SP)*this%dx
                  else
                     depth(i, j) = this%bathy_depth
                  end if
               end do
            end do
         case default
            error stop "geometry: unknown bathy type"
         end select

         call grid%halo_exchange(depth)
         call fill_ghost_wall(lp, grid%is_back_boundary, grid%is_shore_boundary, &
                              grid%is_right_boundary, grid%is_left_boundary, &
                              SIGN_MIRROR, SIGN_MIRROR, depth)

         ! Re-mirroring x-walls over ALL rows (ledger 18b corner hygiene):
         ! the wall fill above sweeps interior rows only, and halo_exchange
         ! phase 2 shipped the then-zero x-ghost columns into the y-seam/
         ! periodic corner blocks -- interior rows rewrite bitwise-identical,
         ! the corner blocks become exchange-then-mirror well-defined.
         ! Init-only: per-step BC calls keep the legacy PHI_COLL order.
         ng = lp%ib - 1
         if (grid%is_back_boundary) then
            do j = 1, lp%nloc
               do k = 1, ng
                  depth(k, j) = depth(2*ng + 1 - k, j)
               end do
            end do
         end if
         if (grid%is_shore_boundary) then
            do j = 1, lp%nloc
               do k = 1, ng
                  depth(lp%ie + k, j) = depth(lp%ie - k + 1, j)
               end do
            end do
         end if

         call stagger_depth(lp, depth, depth_x, depth_y)

      end associate

   end subroutine geometry_init_depth

   ! ----------------------------------------------------------------
   ! Face-staggered depths from cell centres (legacy init.F
   ! "re-construct Depth"): centred faces with one-sided extrapolation
   ! at the low array edge — split out so bathy correction can rebuild
   ! them after rewriting depth.
   ! ----------------------------------------------------------------
   subroutine stagger_depth(lp, depth, depth_x, depth_y)
      type(type_loop_bounds), intent(in)    :: lp
      real(SP), intent(in)    :: depth(:, :)
      real(SP), intent(inout) :: depth_x(:, :), depth_y(:, :)

      integer :: i, j

      do j = 1, lp%nloc
         do i = 2, lp%mloc
            depth_x(i, j) = 0.5_SP*(depth(i - 1, j) + depth(i, j))
         end do
         depth_x(1, j) = 0.5_SP*(3.0_SP*depth(1, j) - depth(2, j))
      end do

      do j = 2, lp%nloc
         do i = 1, lp%mloc
            depth_y(i, j) = 0.5_SP*(depth(i, j - 1) + depth(i, j))
         end do
      end do
      depth_y(:, 1) = 0.5_SP*(3.0_SP*depth(:, 1) - depth(:, 2))

   end subroutine stagger_depth

   ! ----------------------------------------------------------------
   ! Bathymetry correction (legacy mod_bathy_correction.F CORRECTION):
   ! iterative smoothing of cells whose slope exceeds slope_cap,
   ! skipping the 5-point neighbourhood of anything shallower than
   ! smooth_below_depth.  Capped cells relax by
   !   $$ d^{n+1}_{ij} = 0.4\,d^n_{ij} + 0.15\,(d^n_{i+1,j} + d^n_{i-1,j}
   !                     + d^n_{i,j+1} + d^n_{i,j-1}) $$
   ! until the max relative change
   !   $$ \max_{ij} \frac{|d^{n+1}_{ij} - d^n_{ij}|}
   !                     {\max(10\,d_{frc},\ d^n_{ij})} \le 0.05 $$
   ! or 1001 sweeps.  Slopes are centred one-sided-free:
   !   $$ |\partial_x d| \approx |d_{i+1,j} - d_{i-1,j}|/\Delta x $$
   ! Bug-for-bug notes vs legacy:
   !   1. NOTE: wall ghosts are NOT re-mirrored during or after the
   !      iteration (legacy only phi_exch's MPI seams) — faces later
   !      staggered from corrected interior + stale ghost mirrors.
   !   2. NOTE: mid-iteration halo exchange wraps under periodic-y here;
   !      legacy PHI_EXCH/PHI_INT_EXCH never wrap (punch-listed).  Inert
   !      for the usual non-periodic correction configs.
   !   3. NOTE: legacy leaves gradx/grady uninitialised where mask0=0
   !      (allocate garbage in the diag files); zeroed here.
   ! Diagnostic arrays are returned for the caller to gather/write
   ! (legacy OUTPUT_CORRECTION files).
   ! ----------------------------------------------------------------
   subroutine geometry_correct_depth(this, env, grid, min_depth_frc, depth, &
                                     depth_org, gradx0, grady0, gradx, grady)
      use core_constants_mod, only: MPI_SP
      use mpi_f08, only: MPI_Allreduce, MPI_MAX, MPI_IN_PLACE
      class(type_model_geometry), intent(in)    :: this
      type(type_env), intent(inout) :: env
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(in)    :: min_depth_frc
      real(SP), intent(inout) :: depth(:, :)
      real(SP), intent(out), allocatable :: depth_org(:, :)
      real(SP), intent(out), allocatable :: gradx0(:, :), grady0(:, :)
      real(SP), intent(out), allocatable :: gradx(:, :), grady(:, :)

      real(SP), allocatable :: depth0(:, :), depth1(:, :), rmask(:, :)
      integer, allocatable :: mask0(:, :)
      real(SP) :: change, tmp
      integer :: i, j, iter, ierr
      character(len=80) :: msg

      call env%log%info("Bathymetry correction ...")

      associate (lp => grid%lp)

         allocate (depth_org, source=depth)
         allocate (depth0, source=depth)
         allocate (depth1, source=depth)
         allocate (mask0(lp%mloc, lp%nloc), source=1)
         allocate (gradx0(lp%mloc, lp%nloc), source=0.0_SP)
         allocate (grady0(lp%mloc, lp%nloc), source=0.0_SP)
         allocate (gradx(lp%mloc, lp%nloc), source=0.0_SP)
         allocate (grady(lp%mloc, lp%nloc), source=0.0_SP)

         ! mask off the smooth area + its 4-neighbours (ring-1 sweep
         ! writes into ring 2, legacy loop bounds)
         do j = lp%jb - 1, lp%je + 1
            do i = lp%ib - 1, lp%ie + 1
               if (depth0(i, j) < this%smooth_below_depth) then
                  mask0(i, j) = 0
                  mask0(i + 1, j) = 0
                  mask0(i - 1, j) = 0
                  mask0(i, j + 1) = 0
                  mask0(i, j - 1) = 0
               end if
            end do
         end do
         ! int halo rides a real copy (legacy phi_int_exch)
         allocate (rmask, source=real(mask0, SP))
         call grid%halo_exchange(rmask)
         mask0 = nint(rmask)

         ! initial slope, diag only
         do j = lp%jb, lp%je
            do i = lp%ib, lp%ie
               if (mask0(i, j) == 1) then
                  gradx0(i, j) = abs(depth0(i + 1, j) - depth0(i - 1, j))/this%dx
                  grady0(i, j) = abs(depth0(i, j + 1) - depth0(i, j - 1))/this%dy
               end if
            end do
         end do

         change = 1.0_SP
         iter = 0
         do while (change > 0.05_SP .and. iter <= 1000)
            write (msg, '(A,I4,A,F6.2)') "iteration: ", iter, &
               "  convergence percentage: ", change
            call env%log%info(trim(msg))

            change = 0.0_SP
            do j = lp%jb, lp%je
               do i = lp%ib, lp%ie
                  if (mask0(i, j) == 1) then
                     gradx(i, j) = abs(depth0(i + 1, j) - depth0(i - 1, j))/this%dx
                     grady(i, j) = abs(depth0(i, j + 1) - depth0(i, j - 1))/this%dy
                     if (max(gradx(i, j), grady(i, j)) > this%slope_cap) then
                        depth1(i, j) = 0.4_SP*depth0(i, j) &
                                       + 0.15_SP*(depth0(i + 1, j) + depth0(i - 1, j) &
                                                  + depth0(i, j + 1) + depth0(i, j - 1))
                        tmp = abs(depth0(i, j) - depth1(i, j)) &
                              /max(min_depth_frc*10.0_SP, depth0(i, j))
                        if (tmp > change) change = tmp
                     end if
                  end if
               end do
            end do

            call grid%halo_exchange(depth1)
            depth0 = depth1
            iter = iter + 1
            call MPI_Allreduce(MPI_IN_PLACE, change, 1, MPI_SP, MPI_MAX, &
                               grid%cart_comm, ierr)
         end do

         write (msg, '(A,I10)') "total iteration: ", iter
         call env%log%info(trim(msg))

         depth = depth0
         call grid%halo_exchange(depth)
         call grid%halo_exchange(gradx)
         call grid%halo_exchange(grady)
         call grid%halo_exchange(gradx0)
         call grid%halo_exchange(grady0)

      end associate

   end subroutine geometry_correct_depth

   ! ----------------------------------------------------------------
   ! Read a global-interior ASCII field (legacy GetFile row layout:
   ! one row of Mglob values per global J) and slice this rank's
   ! interior into arr.  Every rank reads the file — init-time only,
   ! no scatter.  Ghosts are the caller's concern.
   ! ----------------------------------------------------------------
   ! ----------------------------------------------------------------
   ! Dimension scan of a headerless whitespace ASCII grid: nx = token
   ! count per record (rectangularity enforced), ny = record count.
   ! Chunked non-advancing reads, so no line-length assumption; tabs and
   ! CR count as whitespace; blank records are skipped (trailing newline
   ! tolerance). One pass over the bytes -- config-time cost, paid only
   ! when n_cells is absent (inference) or under --validate (window fit).
   ! ----------------------------------------------------------------
   subroutine scan_ascii_dims(env, fname, nx, ny)
      use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
      type(type_env), intent(inout) :: env
      character(*), intent(in) :: fname
      integer, intent(out) :: nx, ny

      character(4096) :: chunk
      character(1) :: c
      logical :: exists, in_tok
      integer :: unit, ios, sz, i, count

      inquire (file=trim(fname), exist=exists)
      if (.not. exists) then
         call env%log%exit_on_error( &
            "scan_ascii_dims: cannot find "//trim(fname))
      end if

      nx = 0
      ny = 0
      open (newunit=unit, file=trim(fname), status="old", action="read")
      record: do
         count = 0
         in_tok = .false.
         do
            read (unit, '(A)', advance="no", size=sz, iostat=ios) chunk
            do i = 1, sz
               c = chunk(i:i)
               if (c == " " .or. c == char(9) .or. c == char(13)) then
                  in_tok = .false.
               else if (.not. in_tok) then
                  in_tok = .true.
                  count = count + 1
               end if
            end do
            if (ios == iostat_eor) exit
            if (ios == iostat_end) then
               if (count > 0) call check_row()
               exit record
            end if
         end do
         call check_row()
      end do record
      close (unit)

      if (nx == 0 .or. ny == 0) then
         call env%log%exit_on_error( &
            "scan_ascii_dims: no data rows in "//trim(fname))
      end if

   contains

      subroutine check_row()
         if (count == 0) return  ! blank record
         ny = ny + 1
         if (nx == 0) then
            nx = count
         else if (count /= nx) then
            call env%log%exit_on_error( &
               "scan_ascii_dims: ragged row in "//trim(fname)// &
               " (file corrupt or not a rectangular grid)")
         end if
      end subroutine check_row

   end subroutine scan_ascii_dims

   subroutine read_field_ascii(env, fname, grid, arr)
      type(type_env), intent(inout) :: env
      character(*), intent(in) :: fname
      type(type_grid_2d), intent(in) :: grid
      real(SP), intent(inout) :: arr(:, :)

      real(SP), allocatable :: row(:)
      logical :: exists
      integer :: gj, unit

      inquire (file=trim(fname), exist=exists)
      if (.not. exists) then
         call env%log%exit_on_error( &
            "read_field_ascii: cannot find "//trim(fname))
      end if

      allocate (row(grid%M))
      open (newunit=unit, file=trim(fname), status="old", action="read")
      do gj = 1, grid%N
         read (unit, *) row
         if (gj >= grid%jbegin .and. gj <= grid%jstop) then
            arr(grid%lp%ib:grid%lp%ie, grid%lp%jb + gj - grid%jbegin) = &
               row(grid%ibegin:grid%istop)
         end if
      end do
      close (unit)

   end subroutine read_field_ascii

end module model_geometry_mod
