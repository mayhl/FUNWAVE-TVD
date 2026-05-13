# libcore_physics: Simulation Context
add_library(
  libcore_engine STATIC
  # stepper_engine.f90
  time.f90 time_utils.f90)

set_target_properties(
  libcore_engine PROPERTIES Fortran_MODULE_DIRECTORY
                             "${CMAKE_BINARY_DIR}/modules/core")

target_link_libraries(libcore_engine PUBLIC libcore_transport libcore_base)

target_include_directories(
  libcore_engine
  PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>"
         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules>")
