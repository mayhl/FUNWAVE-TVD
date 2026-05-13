# libcore_numerics: Numerics — grid decomposition, accumulators, interpolation, CRS
add_library(
  libcore_numerics STATIC
  accumulators.f90 crs.f90 grid.f90 interpolation.f90)

set_target_properties(
  libcore_numerics PROPERTIES Fortran_MODULE_DIRECTORY
                              "${CMAKE_BINARY_DIR}/modules/core")

target_link_libraries(libcore_numerics PUBLIC libcore_engine libcore_infra
                                              libcore_base)

target_include_directories(
  libcore_numerics PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
                          "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")
