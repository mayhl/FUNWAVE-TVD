cmake_minimum_required(VERSION 3.18)
project(FullyDispersive LANGUAGES Fortran)

# -------------------------------------------------------------------
# MPI
# -------------------------------------------------------------------
find_package(MPI REQUIRED COMPONENTS Fortran)

# -------------------------------------------------------------------
# HYPRE  (set -DHYPRE_DIR=/path/to/hypre/install)
# -------------------------------------------------------------------
set(HYPRE_DIR "" CACHE PATH "HYPRE install root containing lib/ and include/")
if(NOT HYPRE_DIR)
  message(FATAL_ERROR "HYPRE_DIR is required. Pass -DHYPRE_DIR=/path/to/hypre")
endif()

find_library(HYPRE_LIB HYPRE HINTS "${HYPRE_DIR}/lib" REQUIRED)

# -------------------------------------------------------------------
# Preprocessor flags  (mirror Makefile defaults)
# -------------------------------------------------------------------
set(FD_FLAGS DOUBLE_PRECISION PARALLEL VESSEL)

# -------------------------------------------------------------------
# Executable
# -------------------------------------------------------------------
add_executable(full_dispersion
  src_FD/mod_param.F
  src_FD/mod_global.F
  src_FD/mod_input.F
  src_FD/mod_vessel.F
  src_FD/master.F
  src_FD/kernel.F
  src_FD/io.F
  src_FD/init.F
  src_FD/coupling.F)

# .F extension → gfortran assumes fixed-form; these files are free-form
set_target_properties(full_dispersion PROPERTIES Fortran_FORMAT FREE)

target_compile_definitions(full_dispersion PRIVATE ${FD_FLAGS})
target_link_libraries(full_dispersion PRIVATE MPI::MPI_Fortran ${HYPRE_LIB})
target_include_directories(full_dispersion PRIVATE "${HYPRE_DIR}/include")
