!> @file crs.f90
!> @brief Coordinate Reference System (CRS) transforms for the model grid.

!> Provides forward and inverse coordinate transforms between an external
!! CRS (geographic or projected) and the model's local Cartesian metre grid.
!!
!! Three modes are supported:
!!
!! | Mode constant       | Description |
!! |---------------------|-------------|
!! | `CRS_LOCAL`         | Ungeoreferenced; identity transform. |
!! | `CRS_PROJECTED`     | UTM or state-plane (metres); pure translation by origin. |
!! | `CRS_GEOGRAPHIC`    | Longitude/latitude (degrees); flat-Earth approximation. |
!!
!! ### Coordinate transform
!!
!! The local grid is related to the external CRS by a rotation and
!! translation.  Given external coordinates \f$(x_e, y_e)\f$, the
!! displacement from the grid origin \f$(\lambda_0, \phi_0)\f$ is first
!! converted to metres \f$(\delta x, \delta y)\f$ (see below), then
!! rotated by the grid azimuth \f$\theta\f$ (counter-clockwise from east):
!!
!! \f[
!!   \begin{pmatrix} x_{\rm loc} \\ y_{\rm loc} \end{pmatrix}
!!   = \underbrace{\begin{pmatrix}
!!       \cos\theta & \sin\theta \\
!!      -\sin\theta & \cos\theta
!!     \end{pmatrix}}_{R(\theta)}
!!   \begin{pmatrix} \delta x \\ \delta y \end{pmatrix}
!! \f]
!!
!! The inverse is \f$({\delta x}, {\delta y}) = R^T (x_{\rm loc}, y_{\rm loc})\f$.
!!
!! ### Flat-Earth geographic projection
!!
!! For `CRS_GEOGRAPHIC`, angular displacements \f$(\Delta\lambda, \Delta\phi)\f$
!! from the origin are converted to metres using a first-order
!! flat-Earth (equidistant) approximation valid for domains smaller than
!! approximately 200 km \cite torge2012geodesy:
!!
!! \f[
!!   \delta x = R_\oplus \cos\phi_0 \;\frac{\pi}{180}\; \Delta\lambda, \qquad
!!   \delta y = R_\oplus \;\frac{\pi}{180}\; \Delta\phi,
!! \f]
!!
!! where \f$R_\oplus\f$ is the mean Earth radius and \f$\phi_0\f$ is the
!! reference (origin) latitude.
!!
!! @see core_constants_mod::R_EARTH
module core_crs_mod
   use core_constants_mod, only: SP, PI, R_EARTH
   implicit none

   !> Ungeoreferenced local grid — identity transform.
   integer, parameter, public :: CRS_LOCAL      = 0
   !> Projected CRS (UTM, state-plane, metres) — translation only.
   integer, parameter, public :: CRS_PROJECTED  = 1
   !> Geographic CRS (lon/lat degrees) — flat-Earth approximation.
   integer, parameter, public :: CRS_GEOGRAPHIC = 2

   !> Coordinate Reference System descriptor for the model grid.
   !!
   !! Encapsulates the mode, origin, and rotation angle required to
   !! transform between the external CRS and the local metre grid.
   type, public :: type_crs
      !> Transform mode: one of `CRS_LOCAL`, `CRS_PROJECTED`, `CRS_GEOGRAPHIC`.
      integer  :: mode = CRS_LOCAL
      !> EPSG code of the external CRS (0 = unset).
      integer  :: epsg = 0
      !> x-coordinate of the grid origin in the external CRS (metres or degrees).
      real(SP) :: origin_x = 0.0_SP
      !> y-coordinate of the grid origin in the external CRS (metres or degrees).
      real(SP) :: origin_y = 0.0_SP
      !> Grid rotation angle \f$\theta\f$ in radians, counter-clockwise from east.
      real(SP) :: theta = 0.0_SP
      !> Well-Known Text (WKT) representation of the external CRS (optional).
      character(:), allocatable :: wkt
   contains
      procedure :: to_local
      procedure :: to_external
   end type type_crs

contains

   !> Transform external CRS coordinates to local grid metres.
   !!
   !! Applies the displacement + rotation formula described in the module
   !! documentation.  For `CRS_GEOGRAPHIC`, the flat-Earth approximation
   !! is used.
   !!
   !! @param[in]  x_ext  External x-coordinates (longitude or easting), size N.
   !! @param[in]  y_ext  External y-coordinates (latitude or northing), size N.
   !! @param[out] x_loc  Local grid x-coordinates in metres, size N.
   !! @param[out] y_loc  Local grid y-coordinates in metres, size N.
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

   !> Transform local grid metres back to external CRS coordinates.
   !!
   !! Applies the inverse rotation \f$R^T\f$ followed by re-projection.
   !!
   !! @param[in]  x_loc  Local grid x-coordinates in metres, size N.
   !! @param[in]  y_loc  Local grid y-coordinates in metres, size N.
   !! @param[out] x_ext  External x-coordinates (longitude or easting), size N.
   !! @param[out] y_ext  External y-coordinates (latitude or northing), size N.
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

   !> Precompute the x-scale factor for the flat-Earth projection.
   !!
   !! For `CRS_GEOGRAPHIC`:
   !! \f$ s_x = R_\oplus \cos\phi_0 \;\dfrac{\pi}{180} \f$
   !!
   !! Hoisted out of the point loop to avoid recomputation per point.
   real(SP) function crs_scale_x(this)
      class(type_crs), intent(in) :: this
      if (this%mode == CRS_GEOGRAPHIC) then
         crs_scale_x = R_EARTH*cos(this%origin_y*PI/180.0_SP)*(PI/180.0_SP)
      else
         crs_scale_x = 1.0_SP
      end if
   end function crs_scale_x

   !> Convert external coordinates to a displacement in metres from the origin.
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

   !> Convert a metre displacement from the origin back to external coordinates.
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
