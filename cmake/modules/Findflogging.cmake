set(_lib "flogging")
set(_pkg "flogging")
set(_url "https://github.com/mayhl/flogging.git")
set(_rev "HEAD")
set(_files "src/logging.f90")

include("${CMAKE_CURRENT_LIST_DIR}/utils.cmake")
my_fetch_package("${_lib}" "${_url}" "${_rev}" "${_files}")

unset(_files)
unset(_lib)
unset(_pkg)
unset(_url)
unset(_rev)
