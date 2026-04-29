# Standard libraries add_library(${main_lib} STATIC)

# Add subdirectories
add_subdirectory(core)
add_subdirectory(model)

if(NOT ENABLE_DEV_MODE)
  add_subdirectory(old)
  target_link_libraries(old_lib PRIVATE ${main_lib})
  list(APPEND all_targets old_lib)
endif()
