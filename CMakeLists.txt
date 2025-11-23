cmake_minimum_required(VERSION 3.5)
project(
  my_project
  VERSION 4.0.0
  DESCRIPTION "FUNWAVE-TVD"
  LANGUAGES Fortran)

enable_language(Fortran)

# Set Fortran standard (e.g., F95)
set(FVERSION "-std=f95")

# module directory is set before building
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

# Setting MPI linkage find_package(MPI)
find_package(MPI REQUIRED)

list(APPEND _module_flags "PARALLEL" "DOUBLE_PRECISION" "CARTESIAN")

set(CMAKE_Fortran_FLAGS "-ffree-form")

set_source_files_properties(./src/mod_vessel.F PROPERTIES Fortran_PREPROCESS ON)
message(STATUS "MPI_Fortran_COMPILER: ${MPI_Fortran_COMPILER}")
# Add source files add_subdirectory(src)

add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})

add_compile_definitions(${_module_flags})

# Central Modules
add_library(param ./src/mod_param.F)

# target_compile_definitions(param PUBLIC DUMMY)
add_library(global ./src/mod_global.F)
add_library(input ./src/mod_input.F)
add_library(bathy_correction ./src/mod_bathy_correction.F)
add_library(parallel_field_io ./src/mod_parallel_field_io.F)
add_library(tide ./src/mod_tide.F)
add_library(time_spectra ./src/mod_time_spectra.F)

list(
  APPEND
  _modules
  "param"
  "global"
  input
  bathy_correction
  parallel_field_io
  tide
  time_spectra)

# Optional Modules

# Cmake doesn't produce .mod files with empty modules causing error in build
# chain.

if("VESSEL" IN_LIST _module_flags)
  add_library(vessel ./src/mod_vessel.F)
  list(APPEND _modules vessel)
endif()

if("METEO" IN_LIST _module_flags)
  add_library(meteo ./src/mod_meteo.F)
  list(APPEND _modules meteo)
endif()

if("SEDIMENT" IN_LIST _module_flags)
  add_library(sediment ./src/mod_sediment.F)
  list(APPEND _modules sediment)
endif()

if("FOAM" IN_LIST _module_flags)
  add_library(foam ./src/mod_foam.F)
  list(APPEND _modules foam)
endif()

if("TRACKING" IN_LIST _module_flags)
  add_library(tracer ./src/mod_tracer.F)
  list(APPEND _modules tracer)
endif()

if("SUBGRID" IN_LIST _module_flags)
  add_library(subgrid ./src/mod_subgrid.F)
  list(APPEND _modules subgrid)
endif()

if("PERCIPITATION" IN_LIST _module_flags)
  add_library(percipitation ./src/mod_meteo.F)
  list(APPEND _modules percipitation)
endif()

add_executable(
  exe
  ./src/main.F
  ./src/bc.F
  ./src/fluxes.F
  ./src/init.F
  ./src/io.F
  ./src/tridiagnal.F
  ./src/breaker.F
  ./src/derivatives.F
  ./src/dispersion.F
  ./src/etauv_solver.F
  ./src/sponge.F
  ./src/sources.F
  ./src/masks.F
  ./src/parallel.F
  ./src/statistics.F
  ./src/wavemaker.F
  ./src/mixing.F
  ./src/nesting.F
  ./src/misc.F
  ./src/samples.F)

target_link_libraries(exe ${_modules} ${MPI_Fortran_LIBRARIES})
