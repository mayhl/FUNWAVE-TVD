# Define libraries
add_library(${main_lib}_core STATIC)
add_library(${main_lib}_model STATIC)

# Link model to core
target_link_libraries(${main_lib}_model PRIVATE ${main_lib}_core)

set(_new_libs ${main_lib}_model ${main_lib}_core)

set(_suffixes core model)
set(_new_libs)

foreach(_suffix IN LISTS _suffixes)
  list(APPEND _new_libs "${main_lib}_${_suffix}")
  add_subdirectory(${_suffix})
endforeach()

# Create an interface library to aggregate dependencies
add_library(${main_lib} INTERFACE)

list(APPEND all_targets ${_new_libs})
if(ENABLE_DEV_MODE)
  target_link_libraries(${main_lib} INTERFACE ${_new_libs})
else()
  add_subdirectory(old)
  target_link_libraries(${main_lib}_old PRIVATE ${_new_libs})
  list(APPEND all_targets ${main_lib}_old)
endif()

set(all_targets
    "${all_targets}"
    PARENT_SCOPE)
