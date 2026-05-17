add_library(
  ${main_lib}_old STATIC
  legacy_runner.F
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

# Set module directory for old library
set_target_properties(
  ${main_lib}_old PROPERTIES Fortran_MODULE_DIRECTORY
                             "${CMAKE_BINARY_DIR}/modules/old")
target_include_directories(
  ${main_lib}_old PRIVATE "${CMAKE_BINARY_DIR}/modules/old"
                          "${CMAKE_BINARY_DIR}/src")

# Apply compiler definitions
target_compile_definitions(${main_lib}_old PRIVATE ${MODULE_FLAGS})

# Inherit module include paths from model and core (linking handled by parent CMakeLists.txt)
target_include_directories(${main_lib}_old PRIVATE
    $<TARGET_PROPERTY:${main_lib}_model,Fortran_MODULE_DIRECTORY>
    $<TARGET_PROPERTY:${main_lib}_core,Fortran_MODULE_DIRECTORY>)
if(USE_VESSEL)
  target_sources(${main_lib}_old PRIVATE vessel.F)
endif()

if(USE_METEO)
  target_sources(${main_lib}_old PRIVATE mod_meteo.F)
endif()

if(USE_SEDIMENT)
  target_sources(${main_lib}_old PRIVATE mod_sediment.F)
endif()

if(USE_FOAM)
  target_sources(${main_lib}_old PRIVATE mod_foam.F)
endif()

if(USE_TRACKING)
  target_sources(${main_lib}_old PRIVATE mod_tracer.F)
endif()

if(USE_SUBGRID)
  target_sources(${main_lib}_old PRIVATE mod_subgrid.F)
endif()

if(USE_PERCIPITATION)
  target_sources(${main_lib}_old PRIVATE mod_percipitation.F)
endif()
