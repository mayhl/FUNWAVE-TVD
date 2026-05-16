!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!  Unified entry point.  Dispatches to the 2D or 3D model based on the
!  CLI argument file extension:
!
!    funwave input.yaml   →  2D/1D path  (type_model_main, YAML reader)
!    funwave input.txt    →  3D path     (legacy full-dispersion pipeline)
!
!  The full 2D/1D program body lives in src/model/2d/old/main.F.
!  The full 3D  program body lives in src/model/3d/old/master.F.
!  Both are excluded from the unified build; this file is the sole PROGRAM.
!
!  HISTORY :
!    05/14/2026  Michael-Angelo Y.H. Lam
!
!-------------------------------------------------

program main
   use model_launcher_mod, only: launch
   implicit none
   call launch()
end program main
