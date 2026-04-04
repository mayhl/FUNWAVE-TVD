cmake_minimum_required(VERSION 3.22)

# TODO::
# cmake-format: off
#   [] Cray pfUnit Support 
#   [?] Windows Support: Issues linking Microsoft MPI library
#   []  Preprocessor flag selection
#   []  Figure out why YAML tests' Makefile not be called by CMake.
#   []  Try to get fortran-yaml-c tests working 
#       make test works if tests' Makefile is called manually
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
  funwave
  VERSION 4.0.0
  DESCRIPTION "FUNWAVE-TVD"
  LANGUAGES Fortran C CXX)
enable_language(Fortran)

set(project_name funwave)
set(main_lib ${project_name})
set(exe_name exe_${project_name})
set(project_targets ${main_lib} ${exe_name})
set(ext_libs "")
set(ext_targets "")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
include(GNUInstallDirs)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

include(intro_banner)

option(ENABLE_TESTING "Enable testing for ${project_name}" ON)

add_subdirectory(cmake)
# file(DOWNLOAD
# https://github.com/cpm-cmake/CPM.cmake/releases/download/v0.38.3/CPM.cmake
# ${CMAKE_CURRENT_BINARY_DIR}/cmake/CPM.cmake) include(cmake/CPM.cmake)

find_package(MPI REQUIRED COMPONENTS Fortran)
find_package(Git REQUIRED)
# include(FetchContent) include(CTest)

# Note: string point  to cmake/modules/Findxyz.cmake files
message(STATUS "${CMAKE_MODULE_PATH}")
find_package("face" REQUIRED)
find_package("flogging" REQUIRED)
find_package("ffilesystem" REQUIRED)
find_package("fortran-yaml-c" REQUIRED)

set(all_targets ${project_targets} ${ext_targets})

set(libraries_to_link)
# if(NOT TARGET test-drive::test-drive AND ENABLE_TESTING)
if(ENABLE_TESTING)
  enable_testing()
  # find_package("test-drive" REQUIRED)
  find_package(PFUNIT REQUIRED)
endif()

add_library(${main_lib} STATIC)

# Enable position-independent code for shared-library linking support
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_subdirectory(src)

if(ENABLE_TESTING)
  add_subdirectory(test)
endif()

set_target_properties(${main_lib} PROPERTIES Fortran_MODULE_DIRECTORY
                                             ${CMAKE_BINARY_DIR}/modules)

target_link_libraries(flogging PRIVATE face)
target_link_libraries(${main_lib} PRIVATE ${ext_libs})
target_include_directories(${main_lib}
                           PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/inc")

add_executable(${exe_name} ${PROJECT_SOURCE_DIR}/app/main.f90)
target_include_directories(${exe_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")

target_link_libraries(${exe_name} PRIVATE ${main_lib})

# Intel needs linker_language Fortran else error "undefined reference to `main'"
set_property(TARGET ${exe_name} PROPERTY LINKER_LANGUAGE Fortran)

foreach(tgt ${all_targets})
  message(STATUS "${tgt}")
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
