cmake_minimum_required(VERSION 3.10)

# TODO::
# cmake-format: off
#   [x] Cray/Intel HPC support
#   [?] Windows Support: Issues linking Microsoft MPI library
#   []  Preprocessor flag selection
#   []  Figure out why YAML tests' Makefile not be called by CMake.
#   []  Try to get fortran-yaml-c tests working 
#       make test works if tests' Makefile is called manually
#   []  Add backtrace? https://github.com/SINTEF/fortran-error-handling
#   [?]  Fix newunit issue => use 2023 standards. 
#          Note 1: Temporary hack hard-coding unit in flogging.
#          Note 2: Current FUNWAVE code not compatible with 2023
#          Note 3: While only 2008 standards are necessary, 2023 is what
#                  works for external libraries.  
# cmake-format: on

# ------------
# Basic Setup
# ------------
project(
  my_project
  VERSION 4.0.0
  DESCRIPTION "FUNWAVE-TVD"
  LANGUAGES Fortran C CXX)
enable_language(Fortran)

# ------------------
# Build Requirements
# ------------------

find_package(MPI REQUIRED)
find_package(Git REQUIRED)
# find_package(PFUNIT REQUIRED)

include(FetchContent)
include(CTest)

# Turn off yaml-lib testing

# message(STATUS ${BUILD_TESTING}) set(BUILD_TESTING OFF) message(STATUS
# ${BUILD_TESTING})
# -----------------------
# Setting Compiler Flags
# -----------------------
# Platform Specific Flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(dialect "-ffree-form -fimplicit-none -fcheck=all")
  set(bounds "-fbounds-check")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(dialect "-stand -free") # -implicitnone")
  list(APPEND _module_flags "INTEL")
  # set(bounds "-check bounds")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  set(dialect "-f free")
  list(APPEND _module_flags "CRAY")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

# Linking MPI library
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
set(CMAKE_EXE_LINKER_FLAGS "-static-libgcc -static-libstdc++")

# module directory is set before building

# set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

# set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# Path for external libraries
set(EXTERNAL_DIR ${PROJECT_SOURCE_DIR}/ext)

# Adding main FUNWAVE code
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/src)

# Adding tests
if(BUILD_TESTING)
  add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/tests)
endif()
