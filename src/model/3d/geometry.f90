!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  3D model geometry / bathymetry YAML reader
!
!  YAML block: geometry:
!    grid_size: [nx, ny, nz]    required
!    cell_size: [dx, dy]        required
!    ivgrd: <int>               vertical grid option, default 1
!    grd_r: <real>              grid ratio (required when ivgrd == 2)
!    decomposition:
!      nx_proc: <int>           default 0 (auto)
!      ny_proc: <int>           default 0 (auto)
!    bathymetry:
!      type: flat | slope | data  default data
!      file: <path>             (type==data)
!      analytic: <bool>         default NO
!    bottom:
!      roughness_type: <int>    Ibot, default 1
!      cd: <real>               Cd0, default 0.0
!      zob: <real>              Zob, default 0.0
!      min_depth: <real>        MinDep, default 0.001
!
!  HISTORY :
!    05/15/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_3d_geometry_mod
   use core_constants_mod, only: SP
   use core_env_mod, only: type_env, get_sub_env
   use core_yaml_file_mod, only: type_yaml_reader
   use model_base_mod, only: type_model_base

   implicit none

   private
   public :: type_model_3d_geometry

   type, extends(type_model_base) :: type_model_3d_geometry

      integer  :: nx = 0
      integer  :: ny = 0
      integer  :: nz = 0
      real(SP) :: dx = 0.0_SP
      real(SP) :: dy = 0.0_SP
      integer  :: ivgrd = 1
      real(SP) :: grd_r = 1.0_SP
      integer  :: nx_proc = 0
      integer  :: ny_proc = 0

      character(:), allocatable :: bathy_type
      character(:), allocatable :: depth_file
      logical  :: ana_bathy = .false.

      integer  :: roughness_type = 1
      real(SP) :: cd = 0.0_SP
      real(SP) :: zob = 0.0_SP
      real(SP) :: min_depth = 0.001_SP

   contains
      procedure :: read_input => geometry_3d_read_input
   end type type_model_3d_geometry

contains

   subroutine geometry_3d_read_input(this, env)
      class(type_model_3d_geometry), intent(inout) :: this
      type(type_env), intent(inout), target :: env

      type(type_env) :: sub_env
      type(type_yaml_reader) :: decomp_yaml, bathy_yaml, bot_yaml
      integer, allocatable :: grid_size(:)
      real(SP), allocatable :: cell_size(:)
      logical :: no_decomp, no_grd_r, no_nx_proc, no_ny_proc

      sub_env = get_sub_env(env, "geometry")
      this%is_activated = .true.

      call sub_env%yaml%read("grid_size", val=grid_size)
      this%nx = grid_size(1)
      this%ny = grid_size(2)
      this%nz = grid_size(3)

      call sub_env%yaml%read("cell_size", val=cell_size)
      this%dx = cell_size(1)
      this%dy = cell_size(2)

      call sub_env%yaml%read("ivgrd", val=this%ivgrd, default="1")
      call sub_env%yaml%read("grd_r", silent=no_grd_r, val=this%grd_r, default="1.0")

      ! nx_proc/ny_proc keep their 0 sentinel when the decomposition block is
      ! omitted -> auto-decomposition resolved from the MPI rank count in
      ! READ_INPUT (src/model/3d/old/io.F).  When the block is present both keys
      ! must be given (mirrors the 2D reader, model/2d/geometry.f90).
      decomp_yaml = sub_env%yaml%cast_dictionary("decomposition", no_decomp)
      if (.not. no_decomp) then
         call decomp_yaml%read_positive("nx_proc", silent=no_nx_proc, val=this%nx_proc)
         call decomp_yaml%read_positive("ny_proc", silent=no_ny_proc, val=this%ny_proc)
         if (no_nx_proc .neqv. no_ny_proc) then
            call sub_env%log%exit_on_error( &
               "geometry/decomposition: nx_proc and ny_proc must both be specified")
         end if
      end if

      bathy_yaml = sub_env%yaml%cast_dictionary("bathymetry")
      call bathy_yaml%read("type", val=this%bathy_type, default="data")
      call bathy_yaml%read("file", silent=no_grd_r, val=this%depth_file, default="")
      call bathy_yaml%read("analytic", val=this%ana_bathy, default="NO")

      bot_yaml = sub_env%yaml%cast_dictionary("bottom")
      call bot_yaml%read("roughness_type", val=this%roughness_type, default="1")
      call bot_yaml%read("cd", val=this%cd, default="0.0")
      call bot_yaml%read("zob", val=this%zob, default="0.0")
      call bot_yaml%read("min_depth", val=this%min_depth, default="0.001")

   end subroutine geometry_3d_read_input

end module model_3d_geometry_mod
