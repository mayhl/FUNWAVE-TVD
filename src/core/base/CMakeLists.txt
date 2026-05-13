# libcore_base: Foundation utilities
add_library(
  libcore_base STATIC
  constants.f90 misc.f90 path.F90 path_utils.c units.f90
  throw.f90 memory.f90)

set_target_properties(
  libcore_base PROPERTIES Fortran_MODULE_DIRECTORY
                          "${CMAKE_BINARY_DIR}/modules/core")

target_include_directories(
  libcore_base
  PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
         "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/inc>"
         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")
