file(COPY test.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

qadd_pfunit_ctest(test_path_mod)
qadd_pfunit_ctest(test_range_parser)
qadd_pfunit_ctest(test_logging)
qadd_pfunit_ctest(test_yaml_input_serial)
qadd_pfunit_ctest(test_yaml_input_parallel 2)
