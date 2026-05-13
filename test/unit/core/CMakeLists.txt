file(COPY test.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

qadd_pfunit_ctest(test_comm 2)
qadd_pfunit_ctest(test_grid 4)
qadd_pfunit_ctest(test_path)
qadd_pfunit_ctest(test_range_parser)
qadd_pfunit_ctest(test_logging)
qadd_pfunit_ctest(test_yaml_serial)
qadd_pfunit_ctest(test_yaml_parallel 2)
qadd_pfunit_ctest(test_time_utils)
qadd_pfunit_ctest(test_memory)
qadd_pfunit_ctest(test_interpolation 2)

qadd_pfunit_ctest(test_accumulators)
qadd_pfunit_ctest(test_crs)

qadd_pfunit_ctest(test_misc)
qadd_pfunit_ctest(test_units)

qadd_pfunit_ctest(test_time)
target_link_libraries(test_time libcore_engine libcore_infra libcore_base)

qadd_pfunit_ctest(test_field_registry)
qadd_pfunit_ctest(test_gatherer 2)
