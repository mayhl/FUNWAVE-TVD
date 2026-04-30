set(_lib "face")
set(_pkg "face")
set(_url "https://github.com/szaghi/FACE")
set(_rev "v1.1.3")
set(_files "src/lib/face.F90")

include("${CMAKE_CURRENT_LIST_DIR}/utils.cmake")
my_fetch_package("${_lib}" "${_url}" "${_rev}" "${_files}")

# Explicitly add the modules directory as an include path for the imported target
set_target_properties(face PROPERTIES 
    INTERFACE_INCLUDE_DIRECTORIES "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}>"
)

unset(_files)
unset(_lib)
unset(_pkg)
unset(_url)
unset(_rev)
