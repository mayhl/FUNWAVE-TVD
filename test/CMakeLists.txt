option(ENABLE_TESTING "Enable testing for ${project_name}" ON)
option(ENABLE_REGRESSION_TESTING
       "Enable regression testing for ${project_name}" OFF)
option(ENABLE_VALIDATION_TESTING
       "Enable validation testing for ${project_name}" OFF)

if(ENABLE_TESTING)
  add_subdirectory(unit)
endif()

if(ENABLE_REGRESSION_TESTING)
  add_subdirectory(regression)
endif()

if(ENABLE_VALIDATION_TESTING)
  add_subdirectory(validation)
endif()
add_subdirectory(unit/transport)
