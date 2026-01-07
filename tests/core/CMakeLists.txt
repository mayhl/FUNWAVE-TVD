file(COPY test.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_pfunit_ctest(
  test_yaml_input
  TEST_SOURCES
  test_yaml.pf
  OTHER_SOURCES
  ../throw_with_pfunit.F90
  LINK_LIBRARIES
  core
  EXTRA_USE
  throw_with_pfunit_mod
  EXTRA_INITIALIZE
  initialize_throw
  MAX_PES
  1)
