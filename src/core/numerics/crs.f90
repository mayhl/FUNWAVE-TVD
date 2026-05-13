module core_crs_mod
   use core_constants_mod, only: SP, PI, R_EARTH
   implicit none(external)

   ! Coordinate mode constants — used on type_grid_2d%is_spherical
   ! and to select to_local path in type_crs
   integer, parameter, public :: CRS_LOCAL = 0   ! ungeoreferenced; identity transform
   integer, parameter, public :: CRS_PROJECTED = 1   ! UTM / state plane (metres)
   integer, parameter, public :: CRS_GEOGRAPHIC = 2   ! lon/lat degrees; flat-earth approx

   type, public :: type_crs
      integer  :: mode = CRS_LOCAL
      integer  :: epsg = 0
      real(SP) :: origin_x = 0.0_SP   ! grid (0,0) in external CRS (metres or degrees)
      real(SP) :: origin_y = 0.0_SP
      real(SP) :: theta = 0.0_SP   ! grid rotation, radians CCW from east
      character(:), allocatable :: wkt
   contains
      procedure :: to_local
      procedure :: to_external
      ! write_cf belongs in output layer (requires NetCDF); not in physics/
   end type type_crs

contains

   ! Convert external CRS coordinates to local grid metres.
   ! For CRS_GEOGRAPHIC: flat-earth approximation valid for domains < ~200 km.
   ! Reference latitude is origin_y — consistent with init_spacing_spherical.
   subroutine to_local(this, x_ext, y_ext, x_loc, y_loc)
      class(type_crs), intent(in) :: this
      real(SP), intent(in)  :: x_ext(:), y_ext(:)
      real(SP), intent(out) :: x_loc(:), y_loc(:)
      real(SP) :: dx, dy, cos_t, sin_t, scale_x
      integer  :: k

      cos_t = cos(this%theta)
      sin_t = sin(this%theta)
      scale_x = crs_scale_x(this)

      do k = 1, size(x_ext)
         call unproject(this, scale_x, x_ext(k), y_ext(k), dx, dy)
         x_loc(k) = cos_t*dx + sin_t*dy
         y_loc(k) = -sin_t*dx + cos_t*dy
      end do
   end subroutine to_local

   subroutine to_external(this, x_loc, y_loc, x_ext, y_ext)
      class(type_crs), intent(in) :: this
      real(SP), intent(in)  :: x_loc(:), y_loc(:)
      real(SP), intent(out) :: x_ext(:), y_ext(:)
      real(SP) :: dx, dy, cos_t, sin_t, scale_x
      integer  :: k

      cos_t = cos(this%theta)
      sin_t = sin(this%theta)
      scale_x = crs_scale_x(this)

      do k = 1, size(x_loc)
         ! inverse rotation (transpose of rotation matrix)
         dx = cos_t*x_loc(k) - sin_t*y_loc(k)
         dy = sin_t*x_loc(k) + cos_t*y_loc(k)
         call reproject(this, scale_x, dx, dy, x_ext(k), y_ext(k))
      end do
   end subroutine to_external

   ! ---- private helpers ----

   ! Precompute x scale factor (cost hoisted out of the point loop)
   real(SP) function crs_scale_x(this)
      class(type_crs), intent(in) :: this
      if (this%mode == CRS_GEOGRAPHIC) then
         crs_scale_x = R_EARTH*cos(this%origin_y*PI/180.0_SP)*(PI/180.0_SP)
      else
         crs_scale_x = 1.0_SP
      end if
   end function crs_scale_x

   subroutine unproject(this, scale_x, xe, ye, dx, dy)
      class(type_crs), intent(in) :: this
      real(SP), intent(in)  :: scale_x, xe, ye
      real(SP), intent(out) :: dx, dy
      select case (this%mode)
      case (CRS_LOCAL)
         dx = xe
         dy = ye
      case (CRS_PROJECTED)
         dx = xe - this%origin_x
         dy = ye - this%origin_y
      case (CRS_GEOGRAPHIC)
         dx = scale_x*(xe - this%origin_x)
         dy = R_EARTH*(PI/180.0_SP)*(ye - this%origin_y)
      end select
   end subroutine unproject

   subroutine reproject(this, scale_x, dx, dy, xe, ye)
      class(type_crs), intent(in) :: this
      real(SP), intent(in)  :: scale_x, dx, dy
      real(SP), intent(out) :: xe, ye
      select case (this%mode)
      case (CRS_LOCAL)
         xe = dx
         ye = dy
      case (CRS_PROJECTED)
         xe = dx + this%origin_x
         ye = dy + this%origin_y
      case (CRS_GEOGRAPHIC)
         xe = dx/scale_x + this%origin_x
         ye = dy/(R_EARTH*PI/180.0_SP) + this%origin_y
      end select
   end subroutine reproject

end module core_crs_mod
