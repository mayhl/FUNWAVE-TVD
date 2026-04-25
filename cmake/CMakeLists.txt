if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE
      "RelWithDebInfo"
      CACHE STRING "Build type to be used." FORCE)
  message(
    STATUS "Setting build type to '${CMAKE_BUILD_TYPE}' as none was specified.")
endif()

# specify module installation directory
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/modules/"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")

# Hack fix for MacOS MPI
if(APPLE)
  set(CMAKE_Fortran_COMPILER "mpif90" CACHE FILEPATH "MPI Fortran Compiler" FORCE)
  set(CMAKE_C_COMPILER "mpicc" CACHE FILEPATH "MPI C Compiler" FORCE)
  set(CMAKE_CXX_COMPILER "mpic++" CACHE FILEPATH "MPI C++ Compiler" FORCE)
  message(STATUS "macOS detected: Forcing MPI compiler wrappers")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^GNU$")

  # set(standard_compile_flags "-ffree-line-length-none -fbacktrace -free -stand
  # -fpp")

  set(standard_compile_flags
      "-ffree-form -fimplicit-none -fcheck=all -fbacktrace -cpp")
  set(warning_flags
      "-Wall -Wextra -Wpedantic -Wsurprising -Waliasing -Wampersand -Warray-bounds -Wcharacter-truncation -Wconversion -Wline-truncation -Wintrinsics-std -Wno-tabs -Wunderflow -Wunused-parameter -Wintrinsic-shadow -Wno-align-commons -Wsurprising"
  )
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
  message(
    FATAL_ERROR "Unsupported FORTRAN compiler ID: ${CMAKE_Fortran_COMPILER_ID}")
endif()

# Customize compiler flags
set(CMAKE_Fortran_FLAGS_DEBUG
    "${CMAKE_Fortran_FLAGS_DEBUG} ${fpe}"
    PARENT_SCOPE)
set(CMAKE_Fortran_FLAGS
    "${CMAKE_Fortran_FLAGS} ${standard_compile_flags}"
    PARENT_SCOPE)

set(CMAKE_Fortran_FLAGS_COVERAGE
    "-O0 -g -coverage -fprofile-arcs -ftest-coverage")
if(CMAKE_BUILD_TYPE STREQUAL "Coverage")
  set(CMAKE_Fortran_FLAGS
      "${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_COVERAGE}"
      PARENT_SCOPE)
endif()
