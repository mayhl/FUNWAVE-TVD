!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!   Store commonly used constant values
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

module constants_mod
   use mpi_f08, only: MPI_DOUBLE_PRECISION, MPI_Datatype

   implicit none(external)

   ! TODO: Switch data types to standard
   ! use, intrinsic :: iso_fortran_env

   public
   ! -------------------
   ! Numerical Constants
   ! -------------------
   integer, parameter::SP = 8
   type(MPI_Datatype), parameter::MPI_SP = MPI_DOUBLE_PRECISION
   real(SP), parameter::SMALL = 0.000001_SP
   real(SP), parameter::LARGE = 999999.0_SP
   real(SP), parameter:: ZERO = 0.0_SP
   integer, parameter :: STRING_SIZE = 256
   integer, parameter :: MESSAGE_SIZE = 2048
   integer, parameter  :: PATH_SIZE = 512
   integer, parameter  :: LABEL_SIZE = 10

   ! ----------------------
   ! Mathematical Constants
   ! ----------------------
   real(SP), parameter::PI = 3.141592653_SP
   ! Number of ghost points to share boundary data with sub-grids
   integer, parameter :: N_GHOST = 3

   ! ------------------
   ! Physical Constants
   ! ------------------
   real(SP), parameter::R_EARTH = 6371000.0_SP
   real(SP), parameter:: GRAV = 9.81_SP
   real(SP), parameter:: RHO_AW = 0.0012041_SP  ! Relative to water
   real(SP), parameter:: RHO_AIR = 1.15_SP  ! Absolute value
   real(SP), parameter:: RHO_WATER = 1000.0_SP
   real(SP), parameter:: DEG2RAD = 0.0175_SP

   ! -----------
   ! Error Codes
   ! ----------
   integer, parameter :: err_file_permission = 9
   integer, parameter :: err_no_file = 29

   !integer, parameter ::
end module constants_mod

