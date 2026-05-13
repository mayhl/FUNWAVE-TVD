#!/usr/bin/env bash
# install_deps.sh — builds pFUnit from source into extern/pfunit/installed/

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJ_ROOT="$(dirname "$SCRIPT_DIR")"

PFUNIT_SRC="${PROJ_ROOT}/extern/pfunit/src"
PFUNIT_BUILD="${PROJ_ROOT}/extern/pfunit/build"
PFUNIT_INSTALL="${PROJ_ROOT}/extern/pfunit/installed"

echo "pFUnit source:  ${PFUNIT_SRC}"
echo "pFUnit install: ${PFUNIT_INSTALL}"

if [ ! -d "${PFUNIT_SRC}/.git" ]; then
    echo "Cloning pFUnit..."
    git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git "${PFUNIT_SRC}"
fi

echo "Configuring pFUnit..."
cmake -S "${PFUNIT_SRC}" -B "${PFUNIT_BUILD}" \
    -DCMAKE_INSTALL_PREFIX="${PFUNIT_INSTALL}" \
    -DENABLE_MPI=YES \
    -DENABLE_MPI_F08=YES \
    -DSKIP_OPENMP=YES \
    -DCMAKE_BUILD_TYPE=Release

echo "Building and installing pFUnit..."
cmake --build "${PFUNIT_BUILD}" -j"${NPROC:-$(nproc 2>/dev/null || sysctl -n hw.logicalcpu)}"
cmake --install "${PFUNIT_BUILD}"

echo "----------------------------------------------------"
echo "pFUnit installed to: ${PFUNIT_INSTALL}"
echo "----------------------------------------------------"
