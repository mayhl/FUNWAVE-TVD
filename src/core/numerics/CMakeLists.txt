# libcore_numerics: Numerics — grid decomposition, accumulators, interpolation,
# CRS
add_library(libcore_numerics STATIC accumulators.f90 crs.f90 grid.f90
                                    interpolation.f90 solver_tridiag.f90)

set_target_properties(
  libcore_numerics PROPERTIES Fortran_MODULE_DIRECTORY
                              "${CMAKE_BINARY_DIR}/modules/core")

# only the OMP-threaded solvers pay the OpenMP compile flag
if(USE_OPENMP)
  set_source_files_properties(
    interpolation.f90 solver_tridiag.f90 PROPERTIES COMPILE_OPTIONS
                                                    "${OpenMP_Fortran_FLAGS}")
endif()

target_link_libraries(libcore_numerics PUBLIC libcore_engine libcore_infra
                                              libcore_base)

target_include_directories(
  libcore_numerics PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
                          "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")
