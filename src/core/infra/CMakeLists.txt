# libcore_infra: Infrastructure — MPI comm, logging, YAML, env, range
add_library(libcore_infra STATIC comm.f90 env.f90 logging.F90 range.f90
                                 yaml.F90)

set_target_properties(
  libcore_infra PROPERTIES Fortran_MODULE_DIRECTORY
                           "${CMAKE_BINARY_DIR}/modules/core")

target_link_libraries(libcore_infra PUBLIC libcore_base)

target_link_libraries(libcore_infra PUBLIC ${ext_libs})

target_include_directories(
  libcore_infra
  PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules>"
         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")
