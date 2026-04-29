add_library(
  old_lib STATIC
  mod_param.F
  mod_global.F
  mod_input.F
  mod_bathy_correction.F
  mod_parallel_field_io.F
  mod_tide.F
  mod_time_spectra.F
  bc.F
  fluxes.F
  init.F
  io.F
  tridiagnal.F
  breaker.F
  derivatives.F
  dispersion.F
  etauv_solver.F
  sponge.F
  sources.F
  samples.F
  masks.F
  parallel.F
  statistics.F
  wavemaker.F
  mixing.F
  nesting.F
  misc.F)

set_target_properties(old_lib PROPERTIES Fortran_MODULE_DIRECTORY
                                         "${CMAKE_BINARY_DIR}/modules/old")
target_include_directories(old_lib PUBLIC "${CMAKE_BINARY_DIR}/modules/old")

target_compile_definitions(old_lib PRIVATE ${_module_flags})

message(STATUS "===========================")
message(STATUS ${_module_flags})
message(STATUS "===========================")
if(USE_VESSEL)
  target_sources(old_lib PRIVATE vessel.F)
endif()

if(USE_METEO)
  target_sources(old_lib PRIVATE mod_meteo.F)
endif()

if(USE_SEDIMENT)
  target_sources(old_lib PRIVATE mod_sediment.F)
endif()

if(USE_FOAM)
  target_sources(old_lib PRIVATE mod_foam.F)
endif()

if(USE_TRACKING)
  target_sources(old_lib PRIVATE mod_tracer.F)
endif()

if(USE_SUBGRID)
  target_sources(old_lib PRIVATE mod_subgrid.F)
endif()

if(USE_PERCIPITATION)
  target_sources(old_lib PRIVATE mod_percipitation.F)
endif()
