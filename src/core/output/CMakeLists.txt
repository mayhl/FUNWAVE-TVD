add_library(
  libcore_output STATIC
  field_registry.f90
  output_gatherer.f90
  output_channel.f90
  output_manager.f90)

set_target_properties(
  libcore_output PROPERTIES Fortran_MODULE_DIRECTORY
                             "${CMAKE_BINARY_DIR}/modules/core")

target_include_directories(
  libcore_output PUBLIC
  "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")

target_link_libraries(libcore_output PUBLIC libcore_numerics libcore_engine)
