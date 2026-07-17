# ==============================================================================
# 3D (fully-dispersive) model — optional standalone executable, requires HYPRE.
# Enabled when HYPRE_DIR is set or HYPRE is found on the default path.
#
# Produces: full_dispersion  (PROGRAM MASTER in master.F)
#
# NOTE: the 3D old sources share module names (GLOBAL, PARAM, INPUT_READ) with
# the 2D old sources, so they cannot be linked into the same binary.  The true
# unified executable (funwave dispatching to both) requires namespace isolation
# and is deferred until the new type_model_3d framework absorbs the 3D physics.
# For now, full_dispersion is the standalone 3D binary used by regression tests.
# ==============================================================================

find_package(HYPRE QUIET PATHS "${HYPRE_DIR}" NO_DEFAULT_PATH)
if(NOT HYPRE_FOUND AND NOT HYPRE_DIR)
  find_package(HYPRE QUIET)
endif()

if(NOT HYPRE_FOUND)
  message(
    STATUS
      "HYPRE not found — full_dispersion (3D) target disabled (set HYPRE_DIR to enable)"
  )
  return()
endif()

message(STATUS "HYPRE found at ${HYPRE_DIR} — building full_dispersion (3D)")

# Ensure the shared module directory exists before any Fortran target tries to
# write .mod files into it (avoids a race when both targets build in parallel).
file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/modules/3d_old")

# New typed 3D component modules — compiled into libfunwave_model so their .mod
# files land in modules/model, where funwave_3d and full_dispersion can find
# them (via target_include_directories below).
target_sources(
  ${main_lib}_model
  PRIVATE geometry.f90
          simulation.f90
          physics.f90
          turbulence.f90
          solver.f90
          wavemaker.f90
          bc.f90
          sponge.f90
          hotstart.f90
          baroclinic.f90
          output.f90
          config.f90
          model_3d.f90)
target_compile_definitions(${main_lib}_model PRIVATE ENABLE_3D)

add_executable(
  full_dispersion
  old/mod_param.F
  old/mod_global.F
  old/mod_input.F
  old/io.F
  old/init.F
  old/kernel.F
  old/coupling.F
  old/master.F)

# ==============================================================================
# TRANSITIONAL SCAFFOLDING — funwave_3d shared library
# ---------------------------------------------------------------------------
# The 3D legacy sources (old/*.F) define MODULE GLOBAL, PARAM, INPUT_READ which
# conflict with identically-named modules in the 2D old sources.  To allow both
# to coexist in the unified "funwave" binary we build the 3D old sources as a
# hidden shared library that exposes only the 9 entry-point symbols the wrapper
# (model_3d.f90) needs to call.
#
# Remove this target (and bridge.F, exports_macos.txt, exports_linux.map) once
# MODULE GLOBAL is replaced by the new type_model_3d typed framework and the 3D
# physics no longer rely on global module state.
# ==============================================================================
add_library(
  funwave_3d SHARED
  old/mod_param.F
  old/mod_global.F
  old/mod_input.F
  old/io.F
  old/init.F
  old/kernel.F
  old/coupling.F
  old/master.F # PROGRAM MASTER excluded via #ifndef UNIFIED_BUILD; subroutines
               # (SINGLE_GRID_LOOP etc.) compiled
  old/bridge.F)

target_compile_definitions(funwave_3d PRIVATE ${MODULE_FLAGS} UNIFIED_BUILD)

set_target_properties(
  funwave_3d
  PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules/3d_old"
             LINKER_LANGUAGE Fortran)

target_include_directories(
  funwave_3d
  PRIVATE "${CMAKE_BINARY_DIR}/modules/3d_old"
          $<TARGET_PROPERTY:${main_lib}_model,Fortran_MODULE_DIRECTORY>
          $<TARGET_PROPERTY:${main_lib}_core,Fortran_MODULE_DIRECTORY>)

target_link_libraries(funwave_3d PRIVATE HYPRE::HYPRE MPI::MPI_Fortran
                                         ${main_lib}_model ${main_lib}_core)

# Hide all symbols by default; expose only the 9 entry points via platform
# export lists.  (Transitional scaffolding — see comment block above.)
if(APPLE)
  target_link_options(
    funwave_3d PRIVATE
    "-Wl,-exported_symbols_list,${CMAKE_CURRENT_SOURCE_DIR}/exports_macos.txt"
    "-fvisibility=hidden")
else()
  target_link_options(
    funwave_3d PRIVATE
    "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/exports_linux.map")
  target_compile_options(funwave_3d PRIVATE -fvisibility=hidden)
endif()

if(USE_VESSEL)
  target_sources(full_dispersion PRIVATE old/mod_vessel.F)
endif()

# funwave_3d shares the same sources and module dir — serialise to avoid a
# parallel-build race on param.mod and other module files. Also depend on
# ${main_lib}_model so the 3D component .mod files are ready before io.F (which
# uses model_3d_config_mod) is compiled.
add_dependencies(funwave_3d full_dispersion ${main_lib}_model)
add_dependencies(full_dispersion ${main_lib}_model)

set_target_properties(
  full_dispersion
  PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules/3d_old"
             LINKER_LANGUAGE Fortran)

target_compile_definitions(full_dispersion PRIVATE ${MODULE_FLAGS})

target_include_directories(
  full_dispersion
  PRIVATE "${CMAKE_BINARY_DIR}/modules/3d_old"
          $<TARGET_PROPERTY:${main_lib}_model,Fortran_MODULE_DIRECTORY>
          $<TARGET_PROPERTY:${main_lib}_core,Fortran_MODULE_DIRECTORY>)

target_link_libraries(
  full_dispersion PRIVATE HYPRE::HYPRE MPI::MPI_Fortran ${main_lib}_model
                          ${main_lib}_core)
