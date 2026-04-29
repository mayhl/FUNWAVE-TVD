
# Define a target for regression testing
# This script assumes 'python3' and compares output files
# Define a target for fetching and building the regression reference
include(ExternalProject)
ExternalProject_Add(
    funwave_reference
    GIT_REPOSITORY "https://github.com/mayhl/FUNWAVE-TVD.git"
    GIT_TAG "feat/cmake"
    PREFIX "${CMAKE_CURRENT_BINARY_DIR}/regression_ref"
    CMAKE_ARGS 
        -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_BINARY_DIR}/regression_ref/install
    BUILD_COMMAND ${CMAKE_COMMAND} --build .
    INSTALL_COMMAND ""
)

set(REGRESSION_BRANCH "master" CACHE STRING "Branch to compare against")

# Define a target for regression testing
add_custom_target(
    regression_tests
    DEPENDS funwave_reference
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/run_test.sh ${REGRESSION_BRANCH}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Running regression tests against reference output from ${REGRESSION_BRANCH}"
    VERBATIM
)

# Optional: Make it part of the default test suite if needed
if(ENABLE_REGRESSION_TESTING)
    add_test(NAME regression_tests COMMAND ${CMAKE_COMMAND} --build . --target regression_tests)
endif()
