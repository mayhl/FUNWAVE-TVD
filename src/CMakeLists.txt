# NOTE:: Temporarily hard-coded
list(APPEND _module_flags "PARALLEL" "DOUBLE_PRECISION" "CARTESIAN")

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/core)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/model)
# add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/old)
