file(COPY test.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

qadd_pfunit_ctest(test_comm 2)
qadd_pfunit_ctest(test_path)
qadd_pfunit_ctest(test_range_parser)
qadd_pfunit_ctest(test_logging)
qadd_pfunit_ctest(test_yaml_serial)
qadd_pfunit_ctest(test_yaml_parallel 2)
qadd_pfunit_ctest(test_time_utils)
qadd_pfunit_ctest(test_field)
qadd_pfunit_ctest(test_memory)

# qadd_pfunit_ctest(test_accumulators)
