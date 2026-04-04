# ----------------------------
# FUNWAVE Central Dependencies
# ----------------------------
#
list(APPEND _module_flags "PARALLEL" "DOUBLE_PRECISION" "CARTESIAN" "INTEL")

# add_compile_definitions(DEBUG ${_module_flags})

# set_target_properties(${main_lib} PROPERTIES COMPILE_DEFINITIONS
# ${_module_flags})

target_compile_definitions(${main_lib} PRIVATE ${_module_flags})
list(APPEND CMAKE_Fortran_FLAGS_DEBUG "PARALLEL" "DOUBLE_PRECISION" "CARTESIAN"
     "INTEL")
target_sources(${main_lib} PRIVATE mod_param.F)
target_sources(${main_lib} PRIVATE mod_global.F)
target_sources(${main_lib} PRIVATE mod_input.F)
target_sources(${main_lib} PRIVATE mod_bathy_correction.F)
target_sources(${main_lib} PRIVATE mod_parallel_field_io.f90)
target_sources(${main_lib} PRIVATE mod_tide.F)
target_sources(${main_lib} PRIVATE mod_time_spectra.F)

if("VESSEL" IN_LIST _module_flags)
  target_sources(${main_lib} PRIVATE vessel.F)
endif()

if("METEO" IN_LIST _module_flags)
  target_sources(${main_lib} PRIVATE mod_meteo.F)
endif()

if("SEDIMENT" IN_LIST _module_flags)
  target_sources(${main_lib} PRIVATE mod_sediment.F)
endif()

if("FOAM" IN_LIST _module_flags)
  target_sources(${main_lib} PRIVATE mod_foam.F)
endif()

if("TRACKING" IN_LIST _module_flags)
  target_sources(${main_lib} PRIVATE mod_tracer.F)
endif()

if("SUBGRID" IN_LIST _module_flags)
  target_sources(${main_lib} PRIVATE mod_subgrid.F)
endif()

if("PERCIPITATION" IN_LIST _module_flags)
  target_sources(${main_lib} PRIVATE mod_percipitation.F)
  add_library(percipitation mod_meteo.F)
  list(APPEND _modules percipitation)
endif()

target_sources(${main_lib} PRIVATE bc.F)
target_sources(${main_lib} PRIVATE fluxes.F)
target_sources(${main_lib} PRIVATE init.F)
target_sources(${main_lib} PRIVATE io.F)
target_sources(${main_lib} PRIVATE tridiagnal.F)
target_sources(${main_lib} PRIVATE breaker.F)
target_sources(${main_lib} PRIVATE derivatives.F)
target_sources(${main_lib} PRIVATE dispersion.F)
target_sources(${main_lib} PRIVATE etauv_solver.F)
target_sources(${main_lib} PRIVATE sponge.F)
target_sources(${main_lib} PRIVATE sources.F)
target_sources(${main_lib} PRIVATE samples.F)
target_sources(${main_lib} PRIVATE masks.F)
target_sources(${main_lib} PRIVATE parallel.F)
target_sources(${main_lib} PRIVATE statistics.F)
target_sources(${main_lib} PRIVATE wavemaker.F)
target_sources(${main_lib} PRIVATE mixing.F)
target_sources(${main_lib} PRIVATE nesting.F)
target_sources(${main_lib} PRIVATE misc.F)
