!> @file version.F90
!> @brief Build identity: version, git state, compiler and build configuration.

!--------------------------------------------------
!   This file is part of the FUNWAVE-TVD
!   program under the Simplified BSD license
!--------------------------------------------------
!
!   Authors:
!     [mayhl] Michael-Angelo Y.-H. Lam
!   HISTORY:
!     09/15/2026 [mayhl]
!          created
!-------------------------------------------------

! Defines stamped by cmake/internal/version_header.cmake on every build into
! build/generated/ -- git describe is a build-time fact, not a configure-time one
#include "funwave_version.h"

!> Build identity as string constants plus the two CLI renderings.
!!
!! `version_line()` is the one-line `--version` form and `build_info_lines()`
!! the `--build-info` block, which also heads every log.
module core_version_mod
   implicit none
   private

   character(*), parameter, public :: program_name = "funwave"
   character(*), parameter, public :: package_name = "FUNWAVE-TVD"
   character(*), parameter, public :: version = FUNWAVE_VERSION
   character(*), parameter, public :: git_describe = FUNWAVE_GIT_DESCRIBE
   character(*), parameter, public :: compiler = FUNWAVE_COMPILER
   character(*), parameter, public :: build_type = FUNWAVE_BUILD_TYPE
   character(*), parameter, public :: precision = FUNWAVE_PRECISION
   character(*), parameter, public :: has_mpi = FUNWAVE_MPI
   character(*), parameter, public :: has_openmp = FUNWAVE_OPENMP
   character(*), parameter, public :: has_netcdf = FUNWAVE_NETCDF
   character(*), parameter, public :: has_pnetcdf = FUNWAVE_PNETCDF
   character(*), parameter, public :: has_hypre = FUNWAVE_HYPRE

   integer, parameter, public :: n_build_info_lines = 10
   integer, parameter, public :: build_info_line_len = 128

   public :: version_line, build_info_lines

contains

   !> "FUNWAVE-TVD 4.0.0 (7874061d-dirty)"
   pure function version_line() result(line)
      character(:), allocatable :: line
      line = package_name//" "//version//" ("//git_describe//")"
   end function version_line

   !> The `--build-info` block: the GNU-style name line, then one "key: value"
   !> fact per line
   pure function build_info_lines() result(lines)
      character(build_info_line_len) :: lines(n_build_info_lines)
      lines(1) = program_name//" ("//package_name//") "//version
      lines(2) = "  git:       "//git_describe
      lines(3) = "  compiler:  "//compiler
      lines(4) = "  build:     "//build_type
      lines(5) = "  precision: "//precision
      lines(6) = "  mpi:       "//has_mpi
      lines(7) = "  openmp:    "//has_openmp
      lines(8) = "  netcdf:    "//has_netcdf
      lines(9) = "  pnetcdf:   "//has_pnetcdf
      lines(10) = "  hypre:     "//has_hypre
   end function build_info_lines

end module core_version_mod
