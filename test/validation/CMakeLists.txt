
# Define a target for validation testing
# This target provides a structure for validating specific features/physics modules
add_custom_target(
    validation_tests
    COMMAND ${CMAKE_COMMAND} -E echo "Running validation tests..."
    COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_SOURCE_DIR} ./run_test.sh
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Running validation tests"
    VERBATIM
)

# Optional: Make it part of the default test suite
if(ENABLE_TESTING)
    add_test(NAME validation_tests COMMAND ${CMAKE_COMMAND} --build . --target validation_tests)
endif()
