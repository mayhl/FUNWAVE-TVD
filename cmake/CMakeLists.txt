# ==============================================================================
# 1. COMPILER & BUILD ENVIRONMENT CONFIGURATION
# ==============================================================================

# Set default build type
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE
      "RelWithDebInfo"
      CACHE STRING "Build type to be used." FORCE)
  message(
    STATUS "Setting build type to '${CMAKE_BUILD_TYPE}' as none was specified.")
endif()

# Specify module installation directory
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/modules/"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")

# ==============================================================================
# 1. COMPILER-SPECIFIC FLAGS
# ==============================================================================
# One file per compiler ID (pFUnit-style).  Each file sets funwave_base_flags
# (dialect/portability, every build) and the per-build-type sets:
#
# * Release: portable optimized (baseline ISA, reproducible anywhere)
# * RelWithDebInfo: default; the refs/boards build
# * Debug: runtime checks + FPE traps + warnings
# * Coverage: GNU-only lcov diagnostic (empty elsewhere -> guarded)
# * Benchmark: this-machine peak + symbols for timing/profiling
set(fc_config
    "${CMAKE_CURRENT_SOURCE_DIR}/compilers/${CMAKE_Fortran_COMPILER_ID}.cmake")
if(NOT EXISTS "${fc_config}")
  message(
    FATAL_ERROR "Unsupported FORTRAN compiler ID: ${CMAKE_Fortran_COMPILER_ID}")
endif()
include("${fc_config}")

if(CMAKE_BUILD_TYPE STREQUAL "Coverage" AND funwave_flags_coverage STREQUAL "")
  message(
    FATAL_ERROR "Coverage build not configured for ${CMAKE_Fortran_COMPILER_ID}"
  )
endif()

# Propagated to PARENT_SCOPE so the flags apply to every target defined in
# the root CMakeLists.txt
set(CMAKE_Fortran_FLAGS
    "${CMAKE_Fortran_FLAGS} ${funwave_base_flags}"
    PARENT_SCOPE)
set(CMAKE_Fortran_FLAGS_RELEASE
    "${funwave_flags_release}"
    PARENT_SCOPE)
set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO
    "${funwave_flags_relwithdebinfo}"
    PARENT_SCOPE)
set(CMAKE_Fortran_FLAGS_DEBUG
    "${funwave_flags_debug}"
    PARENT_SCOPE)
set(CMAKE_Fortran_FLAGS_COVERAGE
    "${funwave_flags_coverage}"
    PARENT_SCOPE)
set(CMAKE_Fortran_FLAGS_BENCHMARK
    "${funwave_flags_benchmark}"
    PARENT_SCOPE)
