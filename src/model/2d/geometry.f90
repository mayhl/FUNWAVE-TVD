!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Model geometry / bathymetry YAML reader
!
!  YAML block: geometry:
!    cell_size: [dx, dy]        OR dx_file/dy_file for variable spacing
!    grid_size: [nx, ny]        required for flat and slope bathymetry types
!    origin: [x0, y0]           optional, default [0, 0]
!    decomposition:
!      nx_proc: <int>
!      ny_proc: <int>
!    bathymetry:
!      type: flat | file | slope
!      depth: <real>            flat and slope
!      slope: <real>            slope only
!      x0: <real>               slope only, default 0
!      file: <path>             file only
!      file_type: ascii         file only, default ascii
!      correction: <bool>       file only, default false
!      nx: <int>                file only, headerless ASCII
!      ny: <int>                file only, headerless ASCII
!
!  HISTORY :
!    11/23/2025  Michael-Angelo Y.H. Lam
!    05/13/2026  Renamed from grid.f90; new YAML schema
!
!-------------------------------------------------

module model_geometry_mod
   use core_constants_mod, only: SP
   use core_comm_mod, only: type_comm
   use core_env_mod, only: type_env, get_sub_env
   use core_grid_mod, only: type_grid_2d
   use core_path_mod, only: type_path
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base
   use model_kernel_bc_mod, only: fill_ghost_wall, SIGN_MIRROR

   implicit none

   private
   public :: type_model_geometry, read_field_ascii

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
      integer :: bathy_nx = 0, bathy_ny = 0  ! headerless ASCII only

   contains
      procedure :: read_input => geometry_read_input
      procedure :: build_grid => geometry_build_grid
      procedure :: init_depth => geometry_init_depth
   end type type_model_geometry

contains

   subroutine geometry_read_input(this, env)
      class(type_model_geometry), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: bathy_yaml, decomp_yaml
      real(SP), allocatable :: cell_size(:), origin(:)
      integer, allocatable :: grid_size(:)
      logical :: no_cell_size, no_origin, no_decomp
      logical :: no_nx_proc, no_ny_proc, no_dx_file, no_dy_file
      logical :: no_bathy_nx, no_bathy_ny

      sub_env = get_sub_env(env, "geometry")
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

      ! --- Decomposition (optional) ---
      decomp_yaml = sub_env%yaml%cast_dictionary("decomposition", no_decomp)
      if (.not. no_decomp) then
         call decomp_yaml%read_positive("nx_proc", silent=no_nx_proc, val=this%nx_proc)
         call decomp_yaml%read_positive("ny_proc", silent=no_ny_proc, val=this%ny_proc)
         if (no_nx_proc .neqv. no_ny_proc) then
            call sub_env%log%exit_on_error( &
               "geometry/decomposition: nx_proc and ny_proc must both be specified")
         end if
      end if

      ! --- Bathymetry ---
      bathy_yaml = sub_env%yaml%cast_dictionary("bathymetry")
      call bathy_yaml%read_enum("type", BATHY_TYPES, val=this%bathy_type, default="file")

      select case (trim(this%bathy_type))
      case ("file")
         call bathy_yaml%read_enum("file_type", FILE_TYPES, val=this%bathy_ftype, default="ascii")
         call bathy_yaml%read_input_path("file", val=this%bathy_file)
         call bathy_yaml%read("correction", val=this%bathy_correction, default="NO")
         call bathy_yaml%read_positive("nx", silent=no_bathy_nx, val=this%bathy_nx)
         call bathy_yaml%read_positive("ny", silent=no_bathy_ny, val=this%bathy_ny)
         ! headerless ASCII: dimensions must come from the bathymetry block
         if (no_bathy_nx .or. no_bathy_ny) then
            call sub_env%log%exit_on_error( &
               "geometry/bathymetry: file type needs nx and ny")
         end if

      case ("flat")
         call bathy_yaml%read_positive("depth", val=this%bathy_depth)
         call sub_env%yaml%read("grid_size", val=grid_size)
         this%grid_nx = grid_size(1)
         this%grid_ny = grid_size(2)

      case ("slope")
         call bathy_yaml%read_positive("depth", val=this%bathy_depth)
         call bathy_yaml%read("slope", val=this%bathy_slope)
         call bathy_yaml%read("x0", val=this%bathy_slope_x0, default="0.0")
         call sub_env%yaml%read("grid_size", val=grid_size)
         this%grid_nx = grid_size(1)
         this%grid_ny = grid_size(2)
      end select

   end subroutine geometry_read_input

   ! ----------------------------------------------------------------
   ! Build the distributed grid from the parsed geometry config:
   ! decomposition (explicit or auto), cart topology, uniform spacing.
   ! periodic_y comes from physics%periodic (legacy PERIODIC, y only).
   ! Config validity is checked at read_input; the guards here are
   ! unimplemented-feature stops, not user-error handling.
   ! ----------------------------------------------------------------
   subroutine geometry_build_grid(this, comm, grid, periodic_y)
      class(type_model_geometry), intent(in)    :: this
      type(type_comm), intent(in)    :: comm
      type(type_grid_2d), intent(inout) :: grid
      logical, intent(in)    :: periodic_y

      logical :: create_partition

      if (allocated(this%dx_file)) then
         error stop "geometry: variable spacing not yet implemented in new path"
      end if

      if (trim(this%bathy_type) == "file") then
         ! dimensions validated at read_input (headerless ASCII)
         grid%M = this%bathy_nx
         grid%N = this%bathy_ny
      else
         grid%M = this%grid_nx
         grid%N = this%grid_ny
      end if

      create_partition = (this%nx_proc <= 0)
      if (.not. create_partition) then
         if (this%nx_proc*this%ny_proc /= comm%size) then
            error stop "geometry/decomposition: nx_proc*ny_proc must equal MPI size"
         end if
         grid%nx_proc = this%nx_proc
         grid%ny_proc = this%ny_proc
      end if

      call grid%setup(comm, create_partition, periodic_y=periodic_y)
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
   ! Legacy WaterLevel offset is not yet in the YAML schema (assumed 0).
   ! ----------------------------------------------------------------
   subroutine geometry_init_depth(this, grid, depth, depth_x, depth_y)
      class(type_model_geometry), intent(in)    :: this
      type(type_grid_2d), intent(in)    :: grid
      real(SP), intent(inout) :: depth(:, :), depth_x(:, :), depth_y(:, :)

      integer :: i, j, gi, i_slp

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

      end associate

   end subroutine geometry_init_depth

   ! ----------------------------------------------------------------
   ! Read a global-interior ASCII field (legacy GetFile row layout:
   ! one row of Mglob values per global J) and slice this rank's
   ! interior into arr.  Every rank reads the file — init-time only,
   ! no scatter.  Ghosts are the caller's concern.
   ! ----------------------------------------------------------------
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
