# ==============================================================================
# Helper Macros for Project Configuration
# ==============================================================================

# Macro: my_fetch_package Purpose: Wraps FetchContent to clone, register, and
# link external dependencies like 'fortran-yaml-c' into the project build
# system.
macro("my_fetch_package" package url rev)

  string(TOLOWER "${package}" _pkg_lc)
  string(TOUPPER "${package}" _pkg_uc)

  # fetch from url case
  message(STATUS "Retrieving ${package} from ${url}")
  include(FetchContent) # module for fetching from repo
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(FETCHCONTENT_QUIET FALSE)
  endif()
  # Offline hosts (HPC compute nodes have no egress): FUNWAVE_DEPS_DIR points at
  # pre-cloned sources, one subdir per package, populated on a login node --
  # FetchContent then skips the network entirely
  if(DEFINED ENV{FUNWAVE_DEPS_DIR})
    if(EXISTS "$ENV{FUNWAVE_DEPS_DIR}/${_pkg_lc}")
      set(FETCHCONTENT_SOURCE_DIR_${_pkg_uc}
          "$ENV{FUNWAVE_DEPS_DIR}/${_pkg_lc}")
    endif()
  endif()
  FetchContent_Declare(
    "${_pkg_lc}"
    GIT_REPOSITORY "${url}"
    GIT_TAG "${rev}")
  FetchContent_MakeAvailable("${_pkg_lc}")

  set(_extra_args ${ARGN})
  list(LENGTH _extra_args _extra_count)
  if(${_extra_count} GREATER 0)

    list(GET _extra_args 0 _src_path)
    add_library("${package}" "${${_pkg_lc}_SOURCE_DIR}/${_src_path}")

    list(APPEND ext_libs "${package}")
    list(APPEND ext_targets "${package}")

    target_link_libraries("${package}" PRIVATE)
  else()
    add_library("${package}::${package}" INTERFACE IMPORTED)

    list(APPEND ext_libs "${package}::${package}")
    list(APPEND ext_targets "${package}")

    target_link_libraries("${package}::${package}" INTERFACE "${package}")
  endif()

  if(NOT EXISTS "${${_pkg_lc}_BINARY_DIR}/include")
    file(MAKE_DIRECTORY "${${_pkg_lc}_BINARY_DIR}/include")
  endif()

  unset(_pkg_lc)
  unset(_pkg_uc)

  # sanity check
  if(NOT TARGET "${package}")
    message(FATAL_ERROR "Could not find dependency ${package}")
  endif()
endmacro()

# Macro: qadd_pfunit_ctest Purpose: Simplifies pFUnit test registration by
# linking against the core library and configuring the required Fortran module
# search paths.
#
# A suite given a rank count needs the PARALLEL pFUnit umbrella: `pfunit.mod` at
# compile time for the MpiTestMethod suites, and libpfunit's own `funit_main` --
# the one that wraps MPI_Init -- at run time for the rest.  Upstream forces
# SKIP_MPI on MinGW (pFUnit `CMakeLists.txt`), so a Windows install ships only
# libfunit/funit.mod and every such suite would die on a missing module, taking
# the whole build down with it.  Those registrations are therefore skipped when
# the located install is serial-only, and reported by qreport_pfunit_skips.
macro("qadd_pfunit_ctest" name)

  set(_extra_args ${ARGN})
  list(LENGTH _extra_args _extra_count)

  set(_other_sources ${CMAKE_SOURCE_DIR}/test/throw_with_pfunit.F90)
  set(_extra_use throw_with_pfunit_mod)
  set(_extra_init initialize_throw)
  set(_max_pes "")

  if(${_extra_count} GREATER 0)
    list(GET _extra_args 0 _first_arg)
    if(_first_arg STREQUAL "NO_THROW")
      set(_other_sources "")
      set(_extra_use "")
      set(_extra_init "")
      list(REMOVE_AT _extra_args 0)
      list(LENGTH _extra_args _extra_count)
    endif()
  endif()

  if(${_extra_count} GREATER 0)
    list(GET _extra_args 0 _max_pes)
    set(_extra_args MAX_PES ${_max_pes})
  endif()

  if(_max_pes AND NOT PFUNIT_MPI_FOUND)

    set_property(GLOBAL APPEND PROPERTY funwave_skipped_mpi_suites ${name})

  else()

    add_pfunit_ctest(
      ${name}
      TEST_SOURCES
      ${name}.pf
      OTHER_SOURCES
      ${_other_sources}
      LINK_LIBRARIES
      ${main_lib}_core
      EXTRA_USE
      ${_extra_use}
      EXTRA_INITIALIZE
      ${_extra_init}
      ${_extra_args})

    target_include_directories(
      ${name}
      PRIVATE
        "$<TARGET_PROPERTY:${main_lib}_core,INTERFACE_INCLUDE_DIRECTORIES>"
        "${CMAKE_BINARY_DIR}/src" "${CMAKE_BINARY_DIR}/src/core/engine")

    set_tests_properties(${name} PROPERTIES LABELS "unit")
    set_target_properties(
      ${name} PROPERTIES Fortran_MODULE_DIRECTORY
                         ${CMAKE_CURRENT_BINARY_DIR}/mod/${name})
    # Intel needs linker_language Fortran else error "undefined reference to
    # `main'"
    set_property(TARGET ${name} PROPERTY LINKER_LANGUAGE Fortran)

  endif()

  unset(_extra_args)
  unset(_max_pes)
endmacro()

# Macro: qlink_pfunit_ctest Purpose: Adds extra libraries to a suite, tolerating
# the suite having been skipped -- a bare target_link_libraries on a name
# qadd_pfunit_ctest declined to register is a hard configure error.
macro("qlink_pfunit_ctest" name)
  if(TARGET ${name})
    target_link_libraries(${name} ${ARGN})
  endif()
endmacro()

# Macro: qreport_pfunit_skips Purpose: Names the suites qadd_pfunit_ctest
# dropped, so a truncated unit gate announces itself at configure time instead
# of passing quietly with a third of the coverage missing.
macro("qreport_pfunit_skips")
  get_property(_skipped GLOBAL PROPERTY funwave_skipped_mpi_suites)
  list(LENGTH _skipped _skipped_count)
  if(${_skipped_count} GREATER 0)
    string(REPLACE ";" " " _skipped_names "${_skipped}")
    message(
      STATUS "pFUnit install is serial-only (no parallel umbrella) -- skipping "
             "${_skipped_count} MPI unit suites: ${_skipped_names}")
  endif()
  unset(_skipped)
  unset(_skipped_count)
  unset(_skipped_names)
endmacro()
