!> @file constants.f90
!> @brief Global numerical, mathematical, and physical constants.

!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!   Authors:
!     [mayhl] Michael-Angelo Y.-H. Lam
!     [fengyanshi] Fengyan Shi
!   HISTORY:
!     11/23/2025 [mayhl]
!          adapted mod_param.F
!     05/01/2010 [fengyanshi]
!          the module is updated corresponding to modifications in subroutines
!-------------------------------------------------

!> Project-wide compile-time constants grouped by category.
!!
!! All parameters are `public` by default.  The working precision `SP`
!! is set to `real64` (double precision) throughout the model;
!! the corresponding MPI datatype `MPI_SP` must match.
!!
!! @see core_grid_mod
!! @see core_units_mod
module core_constants_mod
   use mpi_f08, only: MPI_DOUBLE_PRECISION, MPI_Datatype

   use, intrinsic :: iso_fortran_env, only: real64

   implicit none

   public

   ! -------------------
   ! Numerical Constants
   ! -------------------

   !> Working floating-point precision kind (double, 64-bit).
   integer, parameter :: SP = real64

   !> MPI datatype matching `SP`; must equal `MPI_DOUBLE_PRECISION` when `SP = real64`.
   type(MPI_Datatype), parameter :: MPI_SP = MPI_DOUBLE_PRECISION

   !> Small positive threshold used as a near-zero guard in physics kernels.
   real(SP), parameter :: SMALL = 0.000001_SP

   !> Large sentinel value used to initialise min-search accumulators.
   real(SP), parameter :: LARGE = 999999.0_SP

   !> Exact zero in working precision.
   real(SP), parameter :: ZERO = 0.0_SP

   !> Maximum length of a general string (YAML keys, labels, etc.).
   integer, parameter :: STRING_SIZE = 256

   !> Maximum length of a log or error message.
   integer, parameter :: MESSAGE_SIZE = 2048

   !> Maximum length of a file-system path.
   integer, parameter :: PATH_SIZE = 512

   !> Maximum length of a short identifier label.
   integer, parameter :: LABEL_SIZE = 10

   ! ----------------------
   ! Mathematical Constants
   ! ----------------------

   !> \f$\pi\f$ to full real64 precision.
   real(SP), parameter :: PI = 3.14159265358979323846_SP

   !> Number of ghost layers shared across MPI subdomain boundaries.
   !!
   !! Ghost cells are populated by halo exchange before any stencil
   !! operation that reads neighbour values.  A value of 3 supports
   !! up to 5th-order spatial stencils.
   integer, parameter :: N_GHOST = 3

   ! ------------------
   ! Physical Constants
   ! ------------------

   !> Mean radius of the Earth \f$R_\oplus\f$ (metres).
   !! Used for the flat-Earth geographic projection in `core_crs_mod`.
   real(SP), parameter :: R_EARTH = 6371000.0_SP

   !> Gravitational acceleration \f$g\f$ (m s\f$^{-2}\f$).
   real(SP), parameter :: GRAV = 9.81_SP

   !> Air-to-water density ratio \f$\rho_{\rm air}/\rho_{\rm water}\f$ (dimensionless).
   real(SP), parameter :: RHO_AW = 0.0012041_SP

   !> Absolute air density \f$\rho_{\rm air}\f$ (kg m\f$^{-3}\f$).
   real(SP), parameter :: RHO_AIR = 1.15_SP

   !> Fresh-water density \f$\rho_{\rm water}\f$ (kg m\f$^{-3}\f$).
   real(SP), parameter :: RHO_WATER = 1000.0_SP

   !> Degrees-to-radians conversion factor \f$\pi/180\f$.
   real(SP), parameter :: DEG2RAD = PI/180.0_SP

   !> Radians-to-degrees conversion factor \f$180/\pi\f$.
   real(SP), parameter :: RAD2DEG = 180.0_SP/PI

   ! -----------
   ! Error Codes
   ! -----------

   !> Error code for a file-permission failure.
   integer, parameter :: err_file_permission = 9

   !> Error code for a missing file.
   integer, parameter :: err_no_file = 29

   !> Wrapper for a deferred-length allocatable string,
   !! used where a fixed-length `character` array would be inconvenient.
   type, public :: type_string
      !> The string content.
      character(:), allocatable :: s
   end type type_string

end module core_constants_mod
