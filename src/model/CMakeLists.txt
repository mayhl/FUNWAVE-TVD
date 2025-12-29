add_library(central interface.F grid.F)
target_link_libraries(central PRIVATE core)

# set_target_properties(central PROPERTIES Fortran_MODULE_DIRECTORY
# ${CMAKE_CURRENT_BINARY_DIR})
