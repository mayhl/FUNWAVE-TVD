target_sources(${main_lib}_model PRIVATE grid.f90 interface.f90 main.f90
                                         stations.f90 time.f90)

# Expose model's module directory to legacy code
target_include_directories(
  ${main_lib}_model PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>")
