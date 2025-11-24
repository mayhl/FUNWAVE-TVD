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

module Constants

   !implict none
   use MPI

   ! -------------------
   ! Numerical Constants
   ! -------------------
   integer, parameter::SP = 8
   integer, parameter::MPI_SP = MPI_DOUBLE_PRECISION
   real(SP), parameter::SMALL = 0.000001_SP
   real(SP), parameter::LARGE = 999999.0_SP
   real(SP), parameter:: ZERO = 0.0_SP
   integer, parameter :: STRING_SIZE = 256

   ! ----------------------
   ! Mathematical Constants
   ! ----------------------
   real(SP), parameter::PI = 3.141592653_SP

   ! ------------------
   ! Physical Constants
   ! ------------------
   real(SP), parameter::R_EARTH = 6371000.0_SP
   real(SP), parameter:: GRAV = 9.81_SP
   real(SP), parameter:: RHO_AW = 0.0012041_SP  ! relative to water
   real(SP), parameter:: RHO_AIR = 1.15_SP  ! absolute value
   real(SP), parameter:: RHO_WATER = 1000.0_SP
   real(SP), parameter:: DEG2RAD = 0.0175_SP

end module Constants

