cmake_minimum_required(VERSION 3.5)

# ------------
# Basic Config
# ------------
project(
  my_project
  VERSION 4.0.0
  DESCRIPTION "FUNWAVE-TVD"
  LANGUAGES Fortran)

enable_language(Fortran)

# ------------------
# Build Requirements
# ------------------
find_package(MPI REQUIRED)
find_package(Git REQUIRED)
include(FetchContent)

# -------------------
# External Libraries
# -------------------

# YAML reader for input file
set(YAML_SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ext/fortran-yaml")
FetchContent_Declare(
  yamlio
  GIT_REPOSITORY https://github.com/BoldingBruggeman/fortran-yaml.git
  GIT_TAG d10f56bfd6339c203d8ccc74d522596e076d480c
  SOURCE_DIR ${YAML_SRC_DIR})

# Prettify logging
set(FLOG_SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ext/flogging")
FetchContent_Declare(
  floggingio
  GIT_REPOSITORY https://github.com/cmacmackin/flogging.git
  GIT_TAG 2429bae8080bce33771fb888bf47395ce518d5b2
  SOURCE_DIR ${FLOG_SRC_DIR})

# Ansi Colors for flogging
set(FACE_SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ext/FACE")
FetchContent_Declare(
  faceio
  GIT_REPOSITORY https://github.com/szaghi/FACE.git
  GIT_TAG c0b9d95b1fa27e9a6b75025a55ceea8b86b644ba
  SOURCE_DIR ${FACE_SRC_DIR})

# -----------------------
# Setting Compiler Flags
# -----------------------

# TODO::
# cmake-format: off
#   [x] Cray/Intel HPC support
#   [] Windows support
#   [] Preprocessor flag selection
# cmake-format: on

# Platform Specfic Flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(dialect "-ffree-form -std=f2008 -fimplicit-none")
  set(bounds "-fbounds-check")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(dialect "-stand f08 -free -implicitnone")
  list(APPEND _module_flags "INTEL")
  # set(bounds "-check bounds")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  set(dialect "-f free")
  list(APPEND _module_flags "CRAY")
endif()

# set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

# NOTE:: Temporaarily hard-coded
list(APPEND _module_flags "PARALLEL" "DOUBLE_PRECISION" "CARTESIAN")

# set(CMAKE_EXE_LINKER_FLAGS "-static-libgcc -static-libstdc++")

add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})

# module directory is set before building
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

# --------------------------------
# External Libaries Build Recipes
# ---------------------------------

# NOTE: Temporary folder for transitioning original code
set(OLD_SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src/old")
# FUNWAVE source code directory path
set(SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")

# Make external dependency available for build
FetchContent_MakeAvailable(yamlio floggingio faceio)

# flogging dependencies
add_library(fface ${FACE_SRC_DIR}/src/lib/face.F90)
add_library(flogging ${FLOG_SRC_DIR}/src/logging.f90)
target_link_libraries(flogging fface)

# yaml dependencies
add_library(fyaml ${YAML_SRC_DIR}/yaml_types.F90 ${YAML_SRC_DIR}/yaml.F90)
# ${YAML_SRC_DIR}/yaml_settings.F90

# ----------------------------------------------
# FUNWAVE build recipe linking external libaries
# ----------------------------------------------

# FUNWAVE YAML Input Reader
add_library(fwyaml ${SRC_DIR}/io/yaml.F)
# target_compile_definitions(fwyaml PUBLIC ${_module_flags})
target_link_libraries(fwyaml fyaml)

# FUNWAVE Pretty Logging
add_library(fwlogging ${SRC_DIR}/io/logging.F)
# target_compile_definitions(fwlogging PUBLIC ${_module_flags})
target_link_libraries(fwlogging flogging)

add_compile_definitions(${_module_flags})

# ----------------------------
# FUNWAVE Central Dependencies
# ----------------------------

# Central Modules
add_library(param ${OLD_SRC_DIR}/mod_param.F)
# target_compile_definitions(param PUBLIC ${_module_flags})

add_library(global ${OLD_SRC_DIR}/mod_global.F)
# target_compile_definitions(global PUBLIC ${_module_flags})

target_link_libraries(global param fwyaml fwlogging)

add_library(input ${OLD_SRC_DIR}/mod_input.F)
target_link_libraries(input param global)
# target_compile_definitions(input PUBLIC ${_module_flags})

add_library(bathy_correction ${OLD_SRC_DIR}/mod_bathy_correction.F)
target_link_libraries(bathy_correction param global input)
# target_compile_definitions(bathy_correction PUBLIC ${_module_flags})

add_library(parallel_field_io ${OLD_SRC_DIR}/mod_parallel_field_io.F)
# target_link_libraries(parallel_field_io param global input)
# target_compile_definitions(parallel_field_io PUBLIC ${_module_flags})

add_library(tide ${OLD_SRC_DIR}/mod_tide.F)
target_link_libraries(tide param global input)

# target_compile_definitions(tide PUBLIC ${_module_flags})

add_library(time_spectra ${OLD_SRC_DIR}/mod_time_spectra.F)
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
  add_library(vessel ${OLD_SRC_DIR}/mod_vessel.F)
  list(APPEND _modules vessel)
endif()

if("METEO" IN_LIST _module_flags)
  add_library(meteo ${OLD_SRC_DIR}/mod_meteo.F)
  list(APPEND _modules meteo)
endif()

if("SEDIMENT" IN_LIST _module_flags)
  add_library(sediment ${OLD_SRC_DIR}/mod_sediment.F)
  list(APPEND _modules sediment)
endif()

if("FOAM" IN_LIST _module_flags)
  add_library(foam ${OLD_SRC_DIR}/mod_foam.F)
  list(APPEND _modules foam)
endif()

if("TRACKING" IN_LIST _module_flags)
  add_library(tracer ${OLD_SRC_DIR}/mod_tracer.F)
  list(APPEND _modules tracer)
endif()

if("SUBGRID" IN_LIST _module_flags)
  add_library(subgrid ${OLD_SRC_DIR}/mod_subgrid.F)
  list(APPEND _modules subgrid)
endif()

if("PERCIPITATION" IN_LIST _module_flags)
  add_library(percipitation ${OLD_SRC_DIR}/mod_meteo.F)
  list(APPEND _modules percipitation)
endif()

add_executable(
  exe
  ${OLD_SRC_DIR}/main.F
  ${OLD_SRC_DIR}/bc.F
  ${OLD_SRC_DIR}/fluxes.F
  ${OLD_SRC_DIR}/init.F
  ${OLD_SRC_DIR}/io.F
  ${OLD_SRC_DIR}/tridiagnal.F
  ${OLD_SRC_DIR}/breaker.F
  ${OLD_SRC_DIR}/derivatives.F
  ${OLD_SRC_DIR}/dispersion.F
  ${OLD_SRC_DIR}/etauv_solver.F
  ${OLD_SRC_DIR}/sponge.F
  ${OLD_SRC_DIR}/sources.F
  ${OLD_SRC_DIR}/samples.F
  ${OLD_SRC_DIR}/masks.F
  ${OLD_SRC_DIR}/parallel.F
  ${OLD_SRC_DIR}/statistics.F
  ${OLD_SRC_DIR}/wavemaker.F
  ${OLD_SRC_DIR}/mixing.F
  ${OLD_SRC_DIR}/nesting.F
  ${OLD_SRC_DIR}/misc.F)

# target_compile_definitions(exe PUBLIC ${_module_flags})
target_link_libraries(exe ${_modules} ${MPI_Fortran_LIBRARIES})
