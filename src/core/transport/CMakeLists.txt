# libcore_transport: Communication & Logging
add_library(libcore_transport STATIC comm.f90 env.f90 logging.f90 range.f90
                                     yaml.f90)

# Set the module directory so dependents can find it
set_target_properties(
  libcore_transport PROPERTIES Fortran_MODULE_DIRECTORY
                               "${CMAKE_BINARY_DIR}/modules/core")

target_link_libraries(libcore_transport PUBLIC libcore_base)

target_link_libraries(libcore_transport PRIVATE ${ext_libs})

target_include_directories(
  libcore_transport
  PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules>"
         "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")
