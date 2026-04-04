set(_lib "fortran-yaml-c")
set(_pkg "fortran-yaml-c")
set(_url "https://github.com/mayhl/fortran-yaml-c")
set(_rev "HEAD")

include("${CMAKE_CURRENT_LIST_DIR}/utils.cmake")
list(APPEND ext_targets "libyaml_interface")
my_fetch_package("${_lib}" "${_url}" "${_rev}")

unset(_lib)
unset(_pkg)
unset(_url)
unset(_rev)
