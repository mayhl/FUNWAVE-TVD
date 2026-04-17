# ----------------------------
# FUNWAVE Central Dependencies
# ----------------------------

target_sources(${main_lib} PRIVATE mod_param.F)
target_sources(${main_lib} PRIVATE mod_global.F)
target_sources(${main_lib} PRIVATE mod_input.F)
target_sources(${main_lib} PRIVATE mod_bathy_correction.F)
target_sources(${main_lib} PRIVATE mod_parallel_field_io.F)
target_sources(${main_lib} PRIVATE mod_tide.F)
target_sources(${main_lib} PRIVATE mod_time_spectra.F)

if(USE_VESSEL)
  target_sources(${main_lib} PRIVATE mod_vessel.F)
endif()

if(USE_METEO)
  target_sources(${main_lib} PRIVATE mod_meteo.F)
endif()

if(USE_SEDIMENT)
  target_sources(${main_lib} PRIVATE mod_sediment.F)
endif()

if(USE_FOAM)
  target_sources(${main_lib} PRIVATE mod_foam.F)
endif()

if(USE_TRACKING)
  target_sources(${main_lib} PRIVATE mod_tracer.F)
endif()

if(USE_SUBGRID)
  target_sources(${main_lib} PRIVATE mod_subgrid.F)
endif()

if(USE_PERCIPITATION)
  target_sources(${main_lib} PRIVATE mod_percipitation.F)
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
