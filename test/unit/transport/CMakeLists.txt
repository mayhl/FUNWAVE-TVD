file(COPY test_config.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
qadd_pfunit_ctest(test_config 2)
