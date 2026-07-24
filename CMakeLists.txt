# ==============================================================================
# 1. PROJECT INITIALIZATION
# ==============================================================================

set(CMAKE_POLICY_VERSION_MINIMUM 3.5)
cmake_minimum_required(VERSION 3.22)

# TODO::
# cmake-format: off
#   [] Cray pfUnit Support 
#   [?] Windows Support: Issues linking Microsoft MPI library
#   [?] Type enforce a standard
#          Note 2: Current FUNWAVE code not compatible with 2023
#          Note 3: While only 2008 standards are necessary, 2023 is what
#                  works for external libraries.  
# cmake-format: on

# ------------
# Basic Setup
# ------------
project(
  funwave
  VERSION 4.0.0
  DESCRIPTION "FUNWAVE-TVD"
  LANGUAGES Fortran C CXX)

set(project_name funwave)
set(main_lib lib${project_name})
set(exe_name ${project_name})
set(project_targets ${main_lib})
set(ext_libs "")
set(ext_targets "")

# ==============================================================================
# 1. BUILD OPTIONS
# ==============================================================================
option(USE_MPI "Enable MPI support" ON)
option(USE_DOUBLE "Enable double precision" ON)
option(USE_SPHERICAL "Enable spherical coordinates" OFF)
# Per-module 2D features are now runtime YAML switches (one binary does all);
# USE_SEDIMENT is retained only because the 3D path still gates on -DSEDIMENT.
option(USE_SEDIMENT "Enable sediment module (3D path only)" OFF)
option(USE_NETCDF "Enable NetCDF output when netcdf-fortran is found" ON)
option(USE_PNETCDF "Enable parallel PnetCDF field output when PnetCDF is found"
       ON)
option(USE_OPENMP "Enable OpenMP threading (hybrid MPI x OpenMP)" OFF)

set(MODULE_FLAGS "")
if(USE_MPI)
  list(APPEND MODULE_FLAGS "PARALLEL")
endif()
if(USE_DOUBLE)
  list(APPEND MODULE_FLAGS "DOUBLE_PRECISION")
endif()
if(NOT USE_SPHERICAL)
  list(APPEND MODULE_FLAGS "CARTESIAN")
endif()
if(USE_SEDIMENT)
  list(APPEND MODULE_FLAGS "SEDIMENT")
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
include(GNUInstallDirs)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules"
     "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/internal/intro_banner.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/internal/utils.cmake)
add_subdirectory(cmake)
# Fortran comes after cmake for MacOS override
enable_language(Fortran)

# ==============================================================================
# 1. DEPENDENCIES
# ==============================================================================

find_package(MPI REQUIRED COMPONENTS Fortran)
find_package(Git REQUIRED)
find_package("fortran-yaml-c" REQUIRED)

# OpenMP compiles are scoped per source file (each directive-carrying CMakeLists
# opts its files in) -- the global flag taxed every kernel ~10% under ifx (wheat
# A/B 311001 vs 310901); only the link step is global so every exe -- tests
# included -- pulls the runtime
if(USE_OPENMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
  set(CMAKE_EXE_LINKER_FLAGS
      "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif()

# netcdf-fortran (optional): pkg-config first (brew + most HPC modules ship the
# .pc), nf-config fallback (older module trees).  Absent -> the dummy `netcdf`
# module compiles in and format: netcdf dies with a clear message at output
# init; everything else builds and runs.
set(FUNWAVE_HAS_NETCDF FALSE)
if(USE_NETCDF)
  find_package(PkgConfig QUIET)
  if(PKG_CONFIG_FOUND)
    pkg_check_modules(NETCDF_F QUIET netcdf-fortran)
  endif()
  if(NETCDF_F_FOUND)
    set(NETCDF_F_LINK ${NETCDF_F_LDFLAGS})
    set(FUNWAVE_HAS_NETCDF TRUE)
  else()
    # HINTS: the install_deps.sh extern build (pFUnit-style prefix)
    find_program(
      NF_CONFIG_EXECUTABLE nf-config
      HINTS "${CMAKE_CURRENT_SOURCE_DIR}/extern/netcdf-fortran/installed/bin")
    if(NF_CONFIG_EXECUTABLE)
      execute_process(
        COMMAND ${NF_CONFIG_EXECUTABLE} --includedir
        OUTPUT_VARIABLE NETCDF_F_INCLUDE_DIRS
        OUTPUT_STRIP_TRAILING_WHITESPACE)
      execute_process(
        COMMAND ${NF_CONFIG_EXECUTABLE} --flibs
        OUTPUT_VARIABLE _nf_flibs
        OUTPUT_STRIP_TRAILING_WHITESPACE)
      separate_arguments(_nf_flibs UNIX_COMMAND "${_nf_flibs}")
      set(NETCDF_F_LINK ${_nf_flibs})
      set(FUNWAVE_HAS_NETCDF TRUE)
    endif()
  endif()
endif()
if(FUNWAVE_HAS_NETCDF)
  message(STATUS "netcdf-fortran found -- NetCDF output enabled")
else()
  message(STATUS "netcdf-fortran not found -- NetCDF output stubbed out")
endif()

# PnetCDF (optional): parallel CDF-5 field writes.  pnetcdf-config probe
# (pkg-config .pc files are not shipped by every build); absent -> the dummy
# `pnetcdf` module compiles in and field_io_type: PNETCDF dies with a clear
# message at output init.
set(FUNWAVE_HAS_PNETCDF FALSE)
if(USE_PNETCDF)
  find_program(PNETCDF_CONFIG_EXECUTABLE pnetcdf-config
               HINTS "${CMAKE_CURRENT_SOURCE_DIR}/extern/pnetcdf/installed/bin")
  if(PNETCDF_CONFIG_EXECUTABLE)
    execute_process(
      COMMAND ${PNETCDF_CONFIG_EXECUTABLE} --includedir
      OUTPUT_VARIABLE PNETCDF_INCLUDE_DIRS
      OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(
      COMMAND ${PNETCDF_CONFIG_EXECUTABLE} --libdir
      OUTPUT_VARIABLE _pnc_libdir
      OUTPUT_STRIP_TRAILING_WHITESPACE)
    set(PNETCDF_LINK "-L${_pnc_libdir}" "-lpnetcdf")
    set(FUNWAVE_HAS_PNETCDF TRUE)
  endif()
endif()
if(FUNWAVE_HAS_PNETCDF)
  message(STATUS "PnetCDF found -- parallel field output enabled")
else()
  message(STATUS "PnetCDF not found -- parallel field output stubbed out")
endif()

set(all_targets ${project_targets} ${ext_targets} probe)

set(libraries_to_link)

# ==============================================================================
# 1. BUILD TARGETS
# ==============================================================================

# Enable position-independent code for shared-library linking support
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_subdirectory(src)

if(NOT ENABLE_DEV_MODE)
  add_executable(${exe_name} ${PROJECT_SOURCE_DIR}/app/main.f90)
  target_include_directories(
    ${exe_name} PRIVATE "${CMAKE_BINARY_DIR}/modules"
                        "${CMAKE_BINARY_DIR}/modules/model")
  target_compile_definitions(${exe_name} PRIVATE ${MODULE_FLAGS})
  # Intel needs linker_language Fortran else error "undefined reference to
  # `main'"
  set_property(TARGET ${exe_name} PROPERTY LINKER_LANGUAGE Fortran)
  # Link the 3D static library when available (built by src/model/3d/CMakeLists
  # .txt when HYPRE is found).  funwave_3d and the model lib are mutually
  # dependent — model_3d.f90 calls funwave_3d's bridge entries, funwave_3d calls
  # back into model/core — so a rescan group (--start-group/--end-group) lets ld
  # close the static cycle.  Transitional scaffolding, retires with funwave_3d.
  if(TARGET funwave_3d)
    # Apple ld64 rejects LINK_GROUP:RESCAN for Fortran but resolves archive
    # cycles without grouping; plain listing suffices there
    if(CMAKE_Fortran_LINK_GROUP_USING_RESCAN_SUPPORTED
       OR CMAKE_LINK_GROUP_USING_RESCAN_SUPPORTED)
      target_link_libraries(
        ${exe_name}
        PRIVATE
          "$<LINK_GROUP:RESCAN,${main_lib}_model,${main_lib}_core,funwave_3d>")
    else()
      target_link_libraries(${exe_name} PRIVATE ${main_lib}_model
                                                ${main_lib}_core funwave_3d)
    endif()
    target_include_directories(${exe_name}
                               PRIVATE "${CMAKE_BINARY_DIR}/modules/3d_old")
  else()
    target_link_libraries(${exe_name} PRIVATE ${main_lib})
  endif()
endif()

# ==============================================================================
# 1. TESTING CONFIGURATION
# ==============================================================================

option(ENABLE_UNIT_TESTING "Enable unit (pFUnit) testing" OFF)
option(ENABLE_DEV_MODE "Enable development mode (skip executable)" OFF)

# Enable CTest when unit testing is on
if(ENABLE_UNIT_TESTING)
  enable_testing()
  # The output-channel round-trip test reads data.nc back through nf90 -- dev
  # builds need the real library, only production builds may stub it
  if(NOT FUNWAVE_HAS_NETCDF)
    message(FATAL_ERROR "unit testing requires netcdf-fortran"
                        " (brew install netcdf-fortran / module load)")
  endif()
endif()

# Only require PFUNIT for unit testing
if(ENABLE_UNIT_TESTING)
  if(NOT PFUNIT_DIR)
    file(GLOB PFUNIT_DIRS
         "${CMAKE_CURRENT_SOURCE_DIR}/extern/pfunit/installed/PFUNIT-*")

    list(LENGTH PFUNIT_DIRS PFUNIT_COUNT)
    if(PFUNIT_COUNT EQUAL 0)
      message(
        FATAL_ERROR
          "No pFUnit installation found in 'extern/pfunit/installed/'. Run 'install_deps.sh' first."
      )
    elseif(PFUNIT_COUNT GREATER 1)
      list(SORT PFUNIT_DIRS)
      list(REVERSE PFUNIT_DIRS)
      list(GET PFUNIT_DIRS 0 LATEST_PFUNIT)
      message(
        WARNING "Multiple pFUnit versions found. Using latest: ${LATEST_PFUNIT}"
      )
      set(PFUNIT_DIR "${LATEST_PFUNIT}/cmake")
    else()
      set(PFUNIT_DIR "${PFUNIT_DIRS}/cmake")
    endif()
  endif()
  find_package(PFUNIT REQUIRED PATHS "${PFUNIT_DIR}" NO_DEFAULT_PATH)
endif()

# Only include test subdirectory if unit testing is enabled
if(ENABLE_UNIT_TESTING)
  add_subdirectory(test)
endif()

# ==============================================================================
# 1. INSTALLATION
# ==============================================================================

message(STATUS ${all_targets})
foreach(tgt ${all_targets})
  install(
    TARGETS ${tgt}
    EXPORT ${project_name}Targets
    ARCHIVE DESTINATION lib
    INCLUDES
    DESTINATION include/${project_name})
endforeach()

install(
  DIRECTORY ${CMAKE_BINARY_DIR}/modules/
  DESTINATION include/${project_name}
  FILES_MATCHING
  PATTERN "*.mod")

install(
  EXPORT ${project_name}Targets
  FILE ${project_name}Targets.cmake
  NAMESPACE ${project_name}::
  DESTINATION lib/cmake/${project_name})

include(CMakePackageConfigHelpers)

configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/${project_name}Config.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/${project_name}Config.cmake"
  INSTALL_DESTINATION lib/cmake/${project_name})

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/${project_name}ConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${project_name}Config.cmake"
              "${CMAKE_CURRENT_BINARY_DIR}/${project_name}ConfigVersion.cmake"
        DESTINATION lib/cmake/${project_name})

install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/${project_name}Config.cmake"
  DESTINATION lib/cmake/${project_name}
  RENAME ${project_name}-config.cmake)

# ==============================================================================
# 1. DOXYGEN DOCUMENTATION
# ==============================================================================

option(ENABLE_DOCS "Enable Doxygen documentation generation" OFF)

if(ENABLE_DOCS)
  find_package(Doxygen)

  if(DOXYGEN_FOUND)
    include(FetchContent)
    FetchContent_Declare(
      doxygen-awesome-css
      GIT_REPOSITORY https://github.com/jothepro/doxygen-awesome-css.git
      GIT_TAG v2.3.0)
    FetchContent_MakeAvailable(doxygen-awesome-css)

    FetchContent_GetProperties(doxygen-awesome-css SOURCE_DIR AWESOME_CSS_DIR)

    set(DOXYFILE_IN ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in)
    set(DOXYFILE_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
    set(DOC_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/html)
    set(DOC_INDEX ${DOC_OUTPUT_DIR}/index.html)

    configure_file(${DOXYFILE_IN} ${DOXYFILE_OUT} @ONLY)

    add_custom_command(
      OUTPUT ${DOC_INDEX}
      COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYFILE_OUT}
      DEPENDS ${DOXYFILE_OUT} ${AWESOME_CSS_DIR}/doxygen-awesome.css
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT "Generating HTML documentation with Doxygen"
      VERBATIM)

    add_custom_target(doc ALL DEPENDS ${DOC_INDEX})
  endif()
endif()

print_summary()

if(CMAKE_BUILD_TYPE STREQUAL "Coverage")
  add_custom_target(
    coverage
    COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
    COMMAND ${CMAKE_COMMAND} -E remove -f coverage.info
    COMMAND lcov --directory . --capture --output-file coverage.info
    COMMAND lcov --ignore-errors unused --remove coverage.info '/usr/*'
            '*/build/*' --output-file coverage_filtered.info
    COMMAND genhtml coverage_filtered.info --output-directory coverage_report
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Generating code coverage report")
endif()
