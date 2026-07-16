option(ENABLE_TESTING "Enable testing for ${project_name}" ON)
option(ENABLE_REGRESSION_TESTING
       "Enable regression testing for ${project_name}" OFF)

if(ENABLE_TESTING)
  add_subdirectory(unit)
endif()

if(ENABLE_REGRESSION_TESTING)
  add_subdirectory(regression)
endif()
