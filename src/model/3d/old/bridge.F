!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Transitional scaffolding — remove when MODULE GLOBAL is replaced by
!  the new type_model_3d framework and the shared-library isolation is no
!  longer needed.
!
!  Seeds myid and NumP in MODULE GLOBAL from values already established
!  by the caller's MPI communicator (new_env / new_comm), so the 3D
!  legacy routines that read those module variables are consistent with
!  the MPI state of the unified binary.
!
!  HISTORY :
!    05/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

subroutine INIT_3D_GLOBAL(rank, size_in)
   USE GLOBAL, ONLY: myid, NumP
   integer, intent(in) :: rank, size_in
   myid = rank
   NumP = size_in
end subroutine INIT_3D_GLOBAL

! The following subroutines shadow identically-named routines in the 2D old
! code (src/model/2d/old/).  On macOS the main-binary symbol preempts the dylib
! version, so model_3d.f90 calls these wrappers which resolve internally inside
! the dylib and see the 3D MODULE GLOBAL.

subroutine CALL_READ_INPUT_3D()
   call READ_INPUT()
end subroutine CALL_READ_INPUT_3D

subroutine CALL_ALLOCATE_VARIABLES_3D()
   call allocate_variables()
end subroutine CALL_ALLOCATE_VARIABLES_3D

subroutine CALL_INITIALIZATION_3D()
   call INITIALIZATION()
end subroutine CALL_INITIALIZATION_3D
