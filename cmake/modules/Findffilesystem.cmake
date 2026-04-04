set(_lib "ffilesystem")
set(_pkg "ffilesystem")
set(_url "https://github.com/scivision/ffilesystem")
set(_rev "v6.4.0")

include("${CMAKE_CURRENT_LIST_DIR}/utils.cmake")
my_fetch_package("${_lib}" "${_url}" "${_rev}")

unset(_lib)
unset(_pkg)
unset(_url)
unset(_rev)
