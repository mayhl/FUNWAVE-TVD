add_library(libcore_output STATIC field_registry.f90 output_gatherer.f90
                                  output_channel.f90 output_manager.f90)

if(FUNWAVE_HAS_NETCDF)
  # PUBLIC include so test read-backs can `use netcdf` too; BUILD_INTERFACE
  # because the extern/ install lives inside the source tree, which the
  # export/install generate step otherwise refuses
  foreach(_ncdir IN LISTS NETCDF_F_INCLUDE_DIRS)
    target_include_directories(libcore_output
                               PUBLIC "$<BUILD_INTERFACE:${_ncdir}>")
  endforeach()
  target_link_libraries(libcore_output PUBLIC ${NETCDF_F_LINK})
else()
  # dummy `netcdf` module: nf90 calls error out at first use
  target_sources(libcore_output PRIVATE netcdf_stub.f90)
endif()

set_target_properties(
  libcore_output PROPERTIES Fortran_MODULE_DIRECTORY
                            "${CMAKE_BINARY_DIR}/modules/core")

target_include_directories(
  libcore_output PUBLIC "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules/core>")

target_link_libraries(libcore_output PUBLIC libcore_numerics libcore_engine)
