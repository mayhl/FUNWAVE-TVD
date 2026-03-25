# -------------------------------
# External Libraries Repositories
# -------------------------------

# set(BUILD_SHARED_LIBS off)

# Prettify logging
set(FLOG_SRC_DIR "${EXTERNAL_DIR}/flogging")
FetchContent_Declare(
  floggingio
  # GIT_REPOSITORY https://github.com/cmacmackin/flogging.git
  GIT_REPOSITORY https://github.com/mayhl/flogging.git
  # GIT_TAG v1.0.1
  SOURCE_DIR ${FLOG_SRC_DIR})

# ANSI Colors for flogging
set(FACE_SRC_DIR "${EXTERNAL_DIR}/FACE")
FetchContent_Declare(
  faceio
  GIT_REPOSITORY https://github.com/szaghi/FACE.git
  GIT_TAG v1.1.3
  SOURCE_DIR ${FACE_SRC_DIR})

# FORTRAN interface to YAML-C reader/writer
set(FYAML_SRC_DIR "${EXTERNAL_DIR}/fortran-yaml-c")
FetchContent_Declare(
  fortran-yaml-c
  GIT_REPOSITORY https://github.com/Nicholaswogan/fortran-yaml-c.git
  GIT_TAG v0.2.6
  SOURCE_DIR ${FYAML_SRC_DIR})

# Disable external library tests due to bug with yaml-c tests not building
set(TEMP_BUILD_TEST ${BUILD_TESTING})
if(BUILD_TESTING)
  set(BUILD_TESTING OFF)
endif()

FetchContent_MakeAvailable(fortran-yaml-c floggingio faceio)

# NOTE: Using PRIVATE to apply only target

# Building flogging
add_library(fface ${FACE_SRC_DIR}/src/lib/face.F90)
target_compile_options(fface PRIVATE)

add_library(flogging ${FLOG_SRC_DIR}/src/logging.f90)
target_compile_options(flogging PRIVATE) #

# -std=f2023)

target_link_libraries(flogging PRIVATE fface)
#
# target_compile_options(fortran-yaml-c PRIVATE -std=f2023)

# Re-enabling tests for main project
if(TEMP_BUILD_TEST)
  set(BUILD_TESTING ON)
endif()

# -------------------------------------
# Building core FUNWAVE class structure
# -------------------------------------

# add_library(constants constants.F)
add_compile_definitions(${_module_flags})

add_library(core constants.F comm.F throw.F logging.F yaml.F misc.F)

# Linking external libraries to FUNWAVE core
target_link_libraries(core PRIVATE fortran-yaml-c flogging)

target_compile_options(core PRIVATE) # -s:qtd=f2023)

# TODO: Properly encapsulate core lib, i.e., No direct dependence on flogging
# and fortran-yaml-c

foreach(LIB IN ITEMS core fortran-yaml-c)

  set_target_properties(${LIB} PROPERTIES Fortran_MODULE_DIRECTORY
                                          ${CMAKE_CURRENT_BINARY_DIR})
  target_include_directories(${LIB} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

endforeach()
