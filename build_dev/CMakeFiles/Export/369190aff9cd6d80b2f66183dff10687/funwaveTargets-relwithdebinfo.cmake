#----------------------------------------------------------------
# Generated CMake target import file for configuration "RelWithDebInfo".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "funwave::face" for configuration "RelWithDebInfo"
set_property(TARGET funwave::face APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(funwave::face PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "Fortran"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libface.a"
  )

list(APPEND _cmake_import_check_targets funwave::face )
list(APPEND _cmake_import_check_files_for_funwave::face "${_IMPORT_PREFIX}/lib/libface.a" )

# Import target "funwave::libyaml_interface" for configuration "RelWithDebInfo"
set_property(TARGET funwave::libyaml_interface APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(funwave::libyaml_interface PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "C"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/liblibyaml_interface.a"
  )

list(APPEND _cmake_import_check_targets funwave::libyaml_interface )
list(APPEND _cmake_import_check_files_for_funwave::libyaml_interface "${_IMPORT_PREFIX}/lib/liblibyaml_interface.a" )

# Import target "funwave::fortran-yaml-c" for configuration "RelWithDebInfo"
set_property(TARGET funwave::fortran-yaml-c APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(funwave::fortran-yaml-c PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "Fortran"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libfortran-yaml-c.a"
  )

list(APPEND _cmake_import_check_targets funwave::fortran-yaml-c )
list(APPEND _cmake_import_check_files_for_funwave::fortran-yaml-c "${_IMPORT_PREFIX}/lib/libfortran-yaml-c.a" )

# Import target "funwave::funwave_core" for configuration "RelWithDebInfo"
set_property(TARGET funwave::funwave_core APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(funwave::funwave_core PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "C;Fortran"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libfunwave_core.a"
  )

list(APPEND _cmake_import_check_targets funwave::funwave_core )
list(APPEND _cmake_import_check_files_for_funwave::funwave_core "${_IMPORT_PREFIX}/lib/libfunwave_core.a" )

# Import target "funwave::funwave_model" for configuration "RelWithDebInfo"
set_property(TARGET funwave::funwave_model APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(funwave::funwave_model PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "Fortran"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libfunwave_model.a"
  )

list(APPEND _cmake_import_check_targets funwave::funwave_model )
list(APPEND _cmake_import_check_files_for_funwave::funwave_model "${_IMPORT_PREFIX}/lib/libfunwave_model.a" )

# Import target "funwave::funwave_old" for configuration "RelWithDebInfo"
set_property(TARGET funwave::funwave_old APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(funwave::funwave_old PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "Fortran"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libfunwave_old.a"
  )

list(APPEND _cmake_import_check_targets funwave::funwave_old )
list(APPEND _cmake_import_check_files_for_funwave::funwave_old "${_IMPORT_PREFIX}/lib/libfunwave_old.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
