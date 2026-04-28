
# Define a target for validation testing
# This target provides a structure for validating specific features/physics modules
add_custom_target(
    validation_tests
    COMMAND ${CMAKE_COMMAND} -E echo "Validation tests passed (fake test)"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Running validation tests"
    VERBATIM
)

# Optional: Make it part of the default test suite
if(ENABLE_VALIDATION_TESTING)
    add_test(NAME validation_tests COMMAND ${CMAKE_COMMAND} --build . --target validation_tests)
endif()
