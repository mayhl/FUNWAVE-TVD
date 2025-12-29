# ----------------------------
# FUNWAVE Central Dependencies
# ----------------------------

# Central Modules
add_library(param mod_param.F)
# target_compile_definitions(param PUBLIC ${_module_flags})

add_library(global mod_global.F)
# target_compile_definitions(global PUBLIC ${_module_flags})

target_link_libraries(global param core)

add_library(input mod_input.F)
target_link_libraries(input param global central)
# target_compile_definitions(input PUBLIC ${_module_flags})

add_library(bathy_correction mod_bathy_correction.F)
target_link_libraries(bathy_correction param global input)
# target_compile_definitions(bathy_correction PUBLIC ${_module_flags})

add_library(parallel_field_io mod_parallel_field_io.F)
# target_link_libraries(parallel_field_io param global input)
# target_compile_definitions(parallel_field_io PUBLIC ${_module_flags})

add_library(tide mod_tide.F)
target_link_libraries(tide param global input)

# target_compile_definitions(tide PUBLIC ${_module_flags})

add_library(time_spectra mod_time_spectra.F)
target_link_libraries(time_spectra param global input)

# target_compile_definitions(time_spectra PUBLIC ${_module_flags})

list(
  APPEND
  _modules
  # Old Code
  param
  global
  input
  bathy_correction
  parallel_field_io
  tide
  time_spectra)

# Optional Modules

# Cmake doesn't produce .mod files with empty modules causing error in build
# chain.

if("VESSEL" IN_LIST _module_flags)
  add_library(vessel mod_vessel.F)
  list(APPEND _modules vessel)
endif()

if("METEO" IN_LIST _module_flags)
  add_library(meteo mod_meteo.F)
  list(APPEND _modules meteo)
endif()

if("SEDIMENT" IN_LIST _module_flags)
  add_library(sediment mod_sediment.F)
  list(APPEND _modules sediment)
endif()

if("FOAM" IN_LIST _module_flags)
  add_library(foam mod_foam.F)
  list(APPEND _modules foam)
endif()

if("TRACKING" IN_LIST _module_flags)
  add_library(tracer mod_tracer.F)
  list(APPEND _modules tracer)
endif()

if("SUBGRID" IN_LIST _module_flags)
  add_library(subgrid mod_subgrid.F)
  list(APPEND _modules subgrid)
endif()

if("PERCIPITATION" IN_LIST _module_flags)
  add_library(percipitation mod_meteo.F)
  list(APPEND _modules percipitation)
endif()

add_executable(
  exe
  main.F
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

add_compile_definitions(${_module_flags})

# target_compile_options(exe PRIVATE -std=f95)

target_link_libraries(exe ${_modules} ${MPI_Fortran_LIBRARIES})
