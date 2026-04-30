# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file LICENSE.rst or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION ${CMAKE_VERSION}) # this file comes with cmake

# If CMAKE_DISABLE_SOURCE_CHANGES is set to true and the source directory is an
# existing directory in our source tree, calling file(MAKE_DIRECTORY) on it
# would cause a fatal error, even though it would be a no-op.
if(NOT EXISTS "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-src")
  file(MAKE_DIRECTORY "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-src")
endif()
file(MAKE_DIRECTORY
  "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-build"
  "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-subbuild/face-populate-prefix"
  "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-subbuild/face-populate-prefix/tmp"
  "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-subbuild/face-populate-prefix/src/face-populate-stamp"
  "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-subbuild/face-populate-prefix/src"
  "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-subbuild/face-populate-prefix/src/face-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-subbuild/face-populate-prefix/src/face-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "/Users/rdchlmyl/repos/mayhlFUNWAVE/build_dev/_deps/face-subbuild/face-populate-prefix/src/face-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
