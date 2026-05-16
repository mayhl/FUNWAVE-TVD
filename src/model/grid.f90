!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Shared horizontal+vertical grid reader for 1D/2D/3D models.
!
!  YAML block: grid:
!    grid size: [nx]            1D  — ny=1, nk=0
!    grid size: [nx, ny]        2D  — nk=0
!    grid size: [nx, ny, nk]    3D  — nk sigma layers
!    dx: <real>                 cell size in x
!    dy: <real>                 cell size in y  (omit or same as dx for 1D)
!    x0: <real>                 origin x (default 0)
!    y0: <real>                 origin y (default 0)
!    bathymetry type: file | flat | slope
!    bathymetry file: <path>    (file only)
!    bathymetry depth: <real>   (flat / slope)
!    bathymetry slope: <real>   (slope only)
!    vertical type: uniform | exponential | hyperbolic  (3D only; default uniform)
!    stretch ratio: <real>      (exponential; default 1.1)
!    hyper alpha: <real>        (hyperbolic; default 0.5)
!    c upper / c lower: <real>  (hyperbolic s-coordinate)
!    a upper / a lower: <real>  (hyperbolic s-coordinate)
!
!  HISTORY :
!    05/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

module model_grid_nd_mod
   use core_constants_mod, only: SP
   use core_env_mod,       only: type_env, get_sub_env
   use core_path_mod,      only: type_path
   use model_base_mod,     only: type_model_base

   implicit none
   private
   public :: type_model_grid_nd, NDIM_1D, NDIM_2D, NDIM_3D

   integer, parameter :: NDIM_1D = 1
   integer, parameter :: NDIM_2D = 2
   integer, parameter :: NDIM_3D = 3

   character(len=12), parameter :: BATHY_TYPES(3) = &
      [character(len=12) :: 'file', 'flat', 'slope']
   character(len=12), parameter :: VERT_TYPES(3) = &
      [character(len=12) :: 'uniform', 'exponential', 'hyperbolic']

   type, extends(type_model_base) :: type_model_grid_nd

      integer :: ndim = NDIM_2D

      ! Horizontal (all dimensions)
      integer  :: nx = 0, ny = 1
      real(SP) :: dx = 0.0_SP, dy = 0.0_SP
      real(SP) :: x0 = 0.0_SP, y0 = 0.0_SP

      ! Bathymetry
      character(:), allocatable :: bathy_type
      type(type_path)           :: bathy_file
      real(SP)                  :: bathy_depth = 0.0_SP
      real(SP)                  :: bathy_slope = 0.0_SP

      ! Vertical sigma layers (3D only; nk == 0 implies 2D/1D)
      integer               :: nk = 0
      character(:), allocatable :: vert_type
      real(SP) :: grd_r       = 1.1_SP   ! exponential stretch ratio
      real(SP) :: hyper_alpha = 0.5_SP   ! hyperbolic
      real(SP) :: c_upper     = 1.0_SP   ! s-coordinate
      real(SP) :: c_lower     = 0.0_SP
      real(SP) :: a_upper     = 1.0_SP
      real(SP) :: a_lower     = 0.0_SP

   contains
      procedure :: read_input => grid_nd_read_input
   end type type_model_grid_nd

contains

   subroutine grid_nd_read_input(this, env)
      class(type_model_grid_nd), intent(inout) :: this
      type(type_env),            intent(inout), target :: env

      type(type_env)             :: sub_env
      integer, allocatable       :: dims(:)
      logical                    :: no_dy

      sub_env = get_sub_env(env, 'grid')
      this%is_activated = .true.

      ! grid size: [nx] | [nx,ny] | [nx,ny,nk]  — length sets ndim
      call sub_env%yaml%read_integer_array('grid size', val=dims)
      select case (size(dims))
      case (1)
         this%ndim = NDIM_1D
         this%nx   = dims(1)
         this%ny   = 1
         this%nk   = 0
      case (2)
         this%ndim = NDIM_2D
         this%nx   = dims(1)
         this%ny   = dims(2)
         this%nk   = 0
      case (3)
         this%ndim = NDIM_3D
         this%nx   = dims(1)
         this%ny   = dims(2)
         this%nk   = dims(3)
      case default
         call sub_env%log%exit_on_error( &
            'grid size must have 1, 2, or 3 elements [nx], [nx,ny], or [nx,ny,nk].')
      end select

      call sub_env%yaml%read_real('dx', val=this%dx)
      call sub_env%yaml%read_real('dy', val=this%dy, silent=no_dy)
      if (no_dy) this%dy = this%dx
      call sub_env%yaml%read_real('x0', val=this%x0, default='0.0')
      call sub_env%yaml%read_real('y0', val=this%y0, default='0.0')

      ! Bathymetry
      call sub_env%yaml%read_enum('bathymetry type', BATHY_TYPES, &
                                   val=this%bathy_type, default='file')
      select case (this%bathy_type)
      case ('file')
         call sub_env%yaml%read_input_path('bathymetry file', val=this%bathy_file)
      case ('flat')
         call sub_env%yaml%read_positive('bathymetry depth', val=this%bathy_depth)
      case ('slope')
         call sub_env%yaml%read_positive('bathymetry depth', val=this%bathy_depth)
         call sub_env%yaml%read_real('bathymetry slope', val=this%bathy_slope)
      end select

      ! Vertical sigma layers (3D only)
      if (this%ndim == NDIM_3D) then
         call sub_env%yaml%read_enum('vertical type', VERT_TYPES, &
                                      val=this%vert_type, default='uniform')
         select case (this%vert_type)
         case ('exponential')
            call sub_env%yaml%read_real('stretch ratio', val=this%grd_r, default='1.1')
         case ('hyperbolic')
            call sub_env%yaml%read_real('hyper alpha',  val=this%hyper_alpha, default='0.5')
            call sub_env%yaml%read_real('c upper',      val=this%c_upper,     default='1.0')
            call sub_env%yaml%read_real('c lower',      val=this%c_lower,     default='0.0')
            call sub_env%yaml%read_real('a upper',      val=this%a_upper,     default='1.0')
            call sub_env%yaml%read_real('a lower',      val=this%a_lower,     default='0.0')
         end select
      end if

      call sub_env%comm%barrier()
   end subroutine grid_nd_read_input

end module model_grid_nd_mod
