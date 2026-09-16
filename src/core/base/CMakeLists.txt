# libcore_base: Foundation utilities

add_library(
  libcore_base STATIC
  constants.f90
  misc.f90
  path.F90
  path_utils.c
  units.f90
  throw.f90
  memory.f90
  version.F90)

# Build identity header for version.F90: stamped on every build (git describe is
# a build-time fact), rewritten only on change.  HYPRE is decided in
# src/model/3d, after this directory -- hence the generate-time target test.
set(version_header "${CMAKE_BINARY_DIR}/generated/funwave_version.h")
add_custom_target(
  funwave_version_header ALL
  COMMAND
    ${CMAKE_COMMAND} -DGIT_EXECUTABLE=${GIT_EXECUTABLE}
    -DSOURCE_DIR=${PROJECT_SOURCE_DIR} -DOUT=${version_header}
    -DVERSION=${PROJECT_VERSION}
    "-DCOMPILER=${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}"
    -DBUILD_TYPE=${CMAKE_BUILD_TYPE} -DDOUBLE=${USE_DOUBLE} -DMPI=${USE_MPI}
    -DOPENMP=${USE_OPENMP} -DNETCDF=${FUNWAVE_HAS_NETCDF}
    -DPNETCDF=${FUNWAVE_HAS_PNETCDF} -DHYPRE=$<TARGET_EXISTS:funwave_3d> -P
    ${PROJECT_SOURCE_DIR}/cmake/internal/version_header.cmake
  BYPRODUCTS ${version_header}
  COMMENT "Stamping the build identity header"
  VERBATIM)
add_dependencies(libcore_base funwave_version_header)
set_source_files_properties(version.F90 PROPERTIES OBJECT_DEPENDS
                                                   ${version_header})

set_target_properties(
  libcore_base PROPERTIES Fortran_MODULE_DIRECTORY
                          "${CMAKE_BINARY_DIR}/modules/core")

target_include_directories(
  libcore_base
  PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
         "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/inc>"
         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>"
  PRIVATE "${CMAKE_BINARY_DIR}/generated")
