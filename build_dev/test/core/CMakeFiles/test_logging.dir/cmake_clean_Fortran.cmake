# Remove fortran modules provided by this target.
FILE(REMOVE
  "mod/test_logging/loader.mod"
  "mod/test_logging/LOADER.mod"
  "CMakeFiles/test_logging.dir/loader.mod.stamp"

  "mod/test_logging/test_logging.mod"
  "mod/test_logging/TEST_LOGGING.mod"
  "CMakeFiles/test_logging.dir/test_logging.mod.stamp"

  "mod/test_logging/throw_with_pfunit_mod.mod"
  "mod/test_logging/THROW_WITH_PFUNIT_MOD.mod"
  "CMakeFiles/test_logging.dir/throw_with_pfunit_mod.mod.stamp"

  "mod/test_logging/wraptest_logging.mod"
  "mod/test_logging/WRAPTEST_LOGGING.mod"
  "CMakeFiles/test_logging.dir/wraptest_logging.mod.stamp"
  )
