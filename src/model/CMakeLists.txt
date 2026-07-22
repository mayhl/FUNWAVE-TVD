target_sources(${main_lib}_model PRIVATE base.f90 config_defaults.f90
                                        field_metadata.f90 launcher.F90)

add_subdirectory(2d)
add_subdirectory(3d)

set_target_properties(
  ${main_lib}_model PROPERTIES Fortran_MODULE_DIRECTORY
                               "${CMAKE_BINARY_DIR}/modules/model")

# Expose model modules to dependents (the exe and unit tests)
target_include_directories(
  ${main_lib}_model
  PUBLIC "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/model>"
         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")
