# ----------------------------
# Main library
# ----------------------------

add_library(${main_lib}_core STATIC)
add_subdirectory(core)

add_library(${main_lib}_model STATIC)
add_subdirectory(model)
target_link_libraries(${main_lib}_model PUBLIC ${main_lib}_core)

set(_new_libs ${main_lib}_core ${main_lib}_model)

# Create an interface library to aggregate dependencies
add_library(${main_lib} INTERFACE)
list(APPEND all_targets ${_new_libs})

# NOTE: Temporary hack to not compile old code
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

# ----------------------------
# Probe (Instrumentation)
# ----------------------------

add_library(probe_interface OBJECT probe/recorder_interface.f90)
target_sources(${main_lib}_core PRIVATE $<TARGET_OBJECTS:probe_interface>)

# Swap dummy implementation (empty) with probe
if(ENABLE_INSTRUMENTATION)
  add_library(probe STATIC probe/recorder.f90)
else()
  add_library(probe STATIC probe/recorder_stub.f90)
endif()

add_dependencies(probe probe_interface)
target_link_libraries(${main_lib}_core INTERFACE probe)
