# ==============================================================================
# 1. COMPILER & BUILD ENVIRONMENT CONFIGURATION
# ==============================================================================

# Set default build type
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Build type to be used." FORCE)
  message(STATUS "Setting build type to '${CMAKE_BUILD_TYPE}' as none was specified.")
endif()

# Specify module installation directory
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/modules/"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")

# ==============================================================================
# 2. MAC OS MPI OVERRIDES
# ==============================================================================
# Toolchain usage: cmake -DCMAKE_TOOLCHAIN_FILE=macos_mpi.cmake .


# ==============================================================================
# 3. COMPILER-SPECIFIC FLAGS
# ==============================================================================
# We define compiler-specific standard flags and diagnostic "traps" (FPE) to
# ensure code portability and early error detection during development.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^GNU$")
  # Strict flags for modern Fortran development
  set(standard_compile_flags "-ffree-form -fimplicit-none -fcheck=all -fbacktrace -cpp")
  set(warning_flags "-Wall -Wextra -Wpedantic -Wsurprising -Waliasing -Wampersand -Warray-bounds -Wcharacter-truncation -Wconversion -Wline-truncation -Wintrinsics-std -Wno-tabs -Wunderflow -Wunused-parameter -Wintrinsic-shadow -Wno-align-commons -Wsurprising")
  set(fpe "-frange-check -ffpe-trap=invalid,zero,overflow")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel$")
  set(standard_compile_flags "-traceback")
  set(fpe "-check all -fpe0")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^IntelLLVM$")
  set(standard_compile_flags "-axAVX2 -traceback -fpp -free -stand")
  set(fpe "-check all -fpe0 -traceback -fpp -free -stand -O0")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^NVHPC$")
  set(standard_compile_flags "-traceback")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^LLVMFlang$")
  set(standard_compile_flags "")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Cray$")
  set(standard_compile_flags "-f free")
else()
  message(FATAL_ERROR "Unsupported FORTRAN compiler ID: ${CMAKE_Fortran_COMPILER_ID}")
endif()

# We propagate these flags to the PARENT_SCOPE so that they apply globally
# across all project targets defined in the root CMakeLists.txt.
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${fpe}" PARENT_SCOPE)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${standard_compile_flags}" PARENT_SCOPE)

# Optional Coverage build type
set(CMAKE_Fortran_FLAGS_COVERAGE "-O0 -g -coverage -fprofile-arcs -ftest-coverage")
if(CMAKE_BUILD_TYPE STREQUAL "Coverage")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_COVERAGE}" PARENT_SCOPE)
endif()
