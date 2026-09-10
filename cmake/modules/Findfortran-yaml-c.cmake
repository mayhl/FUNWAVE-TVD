set(_lib "fortran-yaml-c")
set(_pkg "fortran-yaml-c")
set(_url "https://github.com/mayhl/fortran-yaml-c")
# Pinned: HEAD left the bitwise regression tier resting on a moving reference.
# This rev carries the ifx integer/real character validation (c3ee6b6) and
# parse_string; bump deliberately when a fork fix lands.
set(_rev "20d0d3fa8ac16132d9d95ce4f166597ab69f56fb")

include("${CMAKE_CURRENT_LIST_DIR}/../internal/utils.cmake")
list(APPEND ext_targets "libyaml_interface")
my_fetch_package("${_lib}" "${_url}" "${_rev}")

unset(_lib)
unset(_pkg)
unset(_url)
unset(_rev)
