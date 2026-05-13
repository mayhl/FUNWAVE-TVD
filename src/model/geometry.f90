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
   use core_env_mod, only: type_env, get_sub_env
   use core_path_mod, only: type_path
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base

   implicit none(external)

   private
   public :: type_model_geometry

   character(len=8), parameter :: BATHY_TYPES(3) = &
      [character(len=8) :: 'file', 'flat', 'slope']
   character(len=8), parameter :: FILE_TYPES(1) = &
      [character(len=8) :: 'ascii']

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
   end type type_model_geometry

contains

   subroutine geometry_read_input(this, env)
      class(type_model_geometry), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: bathy_yaml, decomp_yaml
      real(SP), allocatable :: cell_size(:), origin(:)
      integer,  allocatable :: grid_size(:)
      logical :: no_cell_size, no_origin, no_decomp
      logical :: no_nx_proc, no_ny_proc, no_dx_file, no_dy_file
      logical :: no_bathy_nx, no_bathy_ny

      sub_env = get_sub_env(env, 'geometry')
      this%is_activated = .true.

      ! --- Spacing ---
      call sub_env%yaml%read('cell_size', silent=no_cell_size, val=cell_size)
      if (.not. no_cell_size) then
         this%dx = cell_size(1)
         this%dy = cell_size(2)
      else
         call sub_env%yaml%read('dx_file', silent=no_dx_file, val=this%dx_file)
         call sub_env%yaml%read('dy_file', silent=no_dy_file, val=this%dy_file)
         if (no_dx_file .and. no_dy_file) then
            call sub_env%log%exit_on_error( &
               'geometry: cell_size or dx_file/dy_file required')
         end if
         if (no_dx_file .neqv. no_dy_file) then
            call sub_env%log%exit_on_error( &
               'geometry: dx_file and dy_file must both be specified')
         end if
      end if

      ! --- Origin (optional) ---
      call sub_env%yaml%read('origin', silent=no_origin, val=origin)
      if (.not. no_origin) then
         this%x0 = origin(1)
         this%y0 = origin(2)
      end if

      ! --- Decomposition (optional) ---
      decomp_yaml = sub_env%yaml%cast_dictionary('decomposition', no_decomp)
      if (.not. no_decomp) then
         call decomp_yaml%read_positive('nx_proc', silent=no_nx_proc, val=this%nx_proc)
         call decomp_yaml%read_positive('ny_proc', silent=no_ny_proc, val=this%ny_proc)
         if (no_nx_proc .neqv. no_ny_proc) then
            call sub_env%log%exit_on_error( &
               'geometry/decomposition: nx_proc and ny_proc must both be specified')
         end if
      end if

      ! --- Bathymetry ---
      bathy_yaml = sub_env%yaml%cast_dictionary('bathymetry')
      call bathy_yaml%read_enum('type', BATHY_TYPES, val=this%bathy_type, default='file')

      select case (trim(this%bathy_type))
      case ('file')
         call bathy_yaml%read_enum('file_type', FILE_TYPES, val=this%bathy_ftype, default='ascii')
         call bathy_yaml%read_input_path('file', val=this%bathy_file)
         call bathy_yaml%read('correction', val=this%bathy_correction, default='NO')
         call bathy_yaml%read_positive('nx', silent=no_bathy_nx, val=this%bathy_nx)
         call bathy_yaml%read_positive('ny', silent=no_bathy_ny, val=this%bathy_ny)

      case ('flat')
         call bathy_yaml%read_positive('depth', val=this%bathy_depth)
         call sub_env%yaml%read('grid_size', val=grid_size)
         this%grid_nx = grid_size(1)
         this%grid_ny = grid_size(2)

      case ('slope')
         call bathy_yaml%read_positive('depth', val=this%bathy_depth)
         call bathy_yaml%read('slope', val=this%bathy_slope)
         call bathy_yaml%read('x0', val=this%bathy_slope_x0, default='0.0')
         call sub_env%yaml%read('grid_size', val=grid_size)
         this%grid_nx = grid_size(1)
         this%grid_ny = grid_size(2)
      end select

   end subroutine geometry_read_input

end module model_geometry_mod
