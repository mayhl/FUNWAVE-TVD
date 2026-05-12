# libcore_physics: Simulation Context
add_library(
  libcore_physics STATIC
  accumulators.f90 crs.f90 grid.f90 interpolation.f90
)

set_target_properties(
  libcore_physics PROPERTIES Fortran_MODULE_DIRECTORY
                             "${CMAKE_BINARY_DIR}/modules/core")

target_link_libraries(libcore_physics PUBLIC libcore_engine libcore_transport
                                             libcore_base)

target_include_directories(
  libcore_physics PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
                         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")
