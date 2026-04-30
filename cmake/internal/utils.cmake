# ==============================================================================
# Helper Macros for Project Configuration
# ==============================================================================

# Macro: my_fetch_package Purpose: Wraps FetchContent to clone, register, and
# link external dependencies like 'face' or 'fortran-yaml-c' into the project
# build system.
macro("my_fetch_package" package url rev)

  string(TOLOWER "${package}" _pkg_lc)
  string(TOUPPER "${package}" _pkg_uc)

  # fetch from url case
  message(STATUS "Retrieving ${package} from ${url}")
  include(FetchContent) # module for fetching from repo
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(FETCHCONTENT_QUIET FALSE)
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
macro("qadd_pfunit_ctest" name)

  set(_extra_args ${ARGN})
  list(LENGTH _extra_args _extra_count)

  if(${_extra_count} GREATER 0)
    list(GET _extra_args 0 _max_pes)
    set(_extra_args MAX_PES ${_max_pes})
  endif()

  add_pfunit_ctest(
    ${name}
    TEST_SOURCES
    ${name}.pf
    OTHER_SOURCES
    ../throw_with_pfunit.F90
    LINK_LIBRARIES
    ${main_lib}_core
    EXTRA_USE
    throw_with_pfunit_mod
    EXTRA_INITIALIZE
    initialize_throw
    ${_extra_args})

  target_include_directories(
    ${name}
    PRIVATE "$<TARGET_PROPERTY:${main_lib}_core,INTERFACE_INCLUDE_DIRECTORIES>"
            "${CMAKE_BINARY_DIR}/src")

  set_target_properties(
    ${name} PROPERTIES Fortran_MODULE_DIRECTORY
                       ${CMAKE_CURRENT_BINARY_DIR}/mod/${name})
  # Intel needs linker_language Fortran else error "undefined reference to
  # `main'"
  set_property(TARGET ${name} PROPERTY LINKER_LANGUAGE Fortran)

  unset(_extra_args)
endmacro()
