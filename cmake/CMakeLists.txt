set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                             "RelWithDebInfo Coverage")

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Build type to be used." FORCE)
  message(
    STATUS "Setting build type to '${CMAKE_BUILD_TYPE}' as none was specified.")
endif()

# specify module installation directory
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/modules/"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")

# Compiler-specific configurations
if(CMAKE_Fortran_COMPILER_ID MATCHES "^GNU$")

  # set(standard_compile_flags "-ffree-line-length-none -fbacktrace -free -stand
  # -fpp")

  set(standard_compile_flags
      "-ffree-form -fimplicit-none -fcheck=all -fbacktrace -cpp -fallow-argument-mismatch -w"
  )
  set(warning_flags
      "-Wall -Wextra -Wpedantic -Wsurprising -Waliasing -Wampersand -Warray-bounds -Wcharacter-truncation -Wconversion -Wline-truncation -Wintrinsics-std -Wno-tabs -Wunderflow -Wunused-parameter -Wintrinsic-shadow -Wno-align-commons -Wsurprising"
  )
  set(fpe "-frange-check -ffpe-trap=invalid,zero,overflow")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel$")
  set(standard_compile_flags "-traceback")
  set(fpe "-check all -fpe0")
  list(APPEND module_flags "INTEL")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^IntelLLVM$")
  set(standard_compile_flags "-axAVX2 -traceback -fpp -free -stand -w")
  set(fpe "-check all -fpe0 -traceback -fpp -free -stand -O0")
  list(APPEND module_flags "INTEL")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^NVHPC$")
  set(standard_compile_flags "-traceback")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^LLVMFlang$")
  set(standard_compile_flags "")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Cray$")
  set(standard_compile_flags "-f free -ccp -w")
  list(APPEND module_flags "CRAY")
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

# Compile Options
option(USE_DOUBLE "Build with double precision" ON)
option(USE_SPHERICAL "Build with spherical coordinate module" OFF)
option(USE_MPI "Build with MPI parallel processing" ON)

# Modules
option(USE_FOAM "Build with foam module" OFF)
option(USE_METEO "Build with meteo module" OFF)
option(USE_PERCIPITATION "Build with percipitation module" OFF)
option(USE_SEDIMENT "Build with sediment module" OFF)
option(USE_SUBGRID "Build with subgrid module" OFF)
option(USE_TRACKING "Build with tracking module" OFF)
option(USE_VESSEL "Build FUNWAVE with vessel module" OFF)

if(USE_MPI)
  list(APPEND module_flags "PARALLEL")
endif()

if(USE_DOUBLE)
  list(APPEND module_flags "DOUBLE_PRECISION")
endif()

if(NOT USE_SPHERICAL)
  list(APPEND module_flags "CARTESIAN")
endif()

if(USE_VESSEL)
  list(APPEND module_flags "VESSEL")
endif()

if(USE_METEO)
  list(APPEND module_flags "METEO")
endif()

if(USE_SEDIMENT)
  list(APPEND module_flags "SEDIMENT")
endif()

if(USE_FOAM)
  list(APPEND module_flags "FOAM")
endif()

if(USE_TRACKING)
  list(APPEND module_flags "TRACKING")
endif()

if(USE_SUBGRID)
  list(APPEND module_flags "SUBGRID")
endif()

if(USE_PERCIPITATION)
  list(APPEND module_flags "PERCIPITATION")
endif()

set(module_flags
    ${module_flags}
    PARENT_SCOPE)
# include(CMakeDependentOption)

# cmake_dependent_option( USE_FOO "Use Foo" ON "USE_A AND (USE_B OR USE_C)" OFF
# )

# mark_as_advanced(MY_VAR)
