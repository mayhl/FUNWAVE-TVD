file(COPY test.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

qadd_pfunit_ctest(test_yaml_input)
