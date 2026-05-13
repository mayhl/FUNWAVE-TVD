target_sources(${main_lib}_model PRIVATE
   base.f90
   geometry.f90
   simulation.f90
   hotstart.f90
   wavemaker.f90
   sponge.f90
   obstacle.f90
   friction.f90
   numerics.f90
   breaking.f90
   output.f90
   physics.f90
   coupling.f90
   grid.f90
   main.f90)

set_target_properties(
  ${main_lib}_model PROPERTIES Fortran_MODULE_DIRECTORY
                               "${CMAKE_BINARY_DIR}/modules/model")

# Expose model modules to dependents (legacy old/ code)
target_include_directories(
  ${main_lib}_model PUBLIC
  "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/model>"
  "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")
