add_subdirectory(core)

if(ENABLE_TESTING AND ENABLE_INSTRUMENTATION)
  qadd_pfunit_ctest(test_probe NO_THROW)
endif()
