set(PFUNIT_SRC_DIR ${EXTERNAL_DIR}/pfunit)

# include(ExternalProject)

# ExternalProject_Add( # FetchContent_Declare( pfunit_repo SOURCE_DIR
# ${PFUNIT_SRC_DIR} GIT_REPOSITORY
# https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git GIT_TAG v4.15.0) #
# SOURCE_DIR ${PFUNIT_SRC_DIR})
#
# ExternalProject_Get_Property(pfunit_repo source_dir) set(PFUNIT_DIR
# ${source_dir}-build CACHE INTERNAL "Path to include folder for Catch")

# FetchContent_MakeAvailable(pfunit_repo)

find_package(PFUNIT REQUIRED)

file(COPY test.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

# cmake-format: off

add_pfunit_ctest(test_yaml_input
  TEST_SOURCES test_yaml.pf
  OTHER_SOURCES throw_with_pfunit.F90
  LINK_LIBRARIES core
  EXTRA_USE throw_with_pfunit_mod
  EXTRA_INITIALIZE initialize_throw
  MAX_PES 1
)


#set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
# cmake-format: on

# add_executable(test_yaml_input test_yaml.F)
# target_link_libraries(test_yaml_input core ${MPI_Fortran_LIBRARIES})
# target_compile_options(test_yaml_input PRIVATE -std=f2023)

# add_test( NAME test_yaml_input WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
# COMMAND echo $(pwd); mpirun -n 4
# ./${CMAKE_CURRENT_SOURCE_DIR}/test_yaml_input)
