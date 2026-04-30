#----------------------------------------------------------------
# Generated CMake target import file for configuration "RelWithDebInfo".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "FACE::FACE" for configuration "RelWithDebInfo"
set_property(TARGET FACE::FACE APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(FACE::FACE PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "Fortran"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libFACE.a"
  )

list(APPEND _cmake_import_check_targets FACE::FACE )
list(APPEND _cmake_import_check_files_for_FACE::FACE "${_IMPORT_PREFIX}/lib/libFACE.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
