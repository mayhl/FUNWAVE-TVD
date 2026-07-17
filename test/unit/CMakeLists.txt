add_subdirectory(core)
add_subdirectory(model)

if(ENABLE_UNIT_TESTING AND ENABLE_INSTRUMENTATION)
  qadd_pfunit_ctest(test_probe NO_THROW)
endif()
