#!/bin/bash
# install_deps.sh
# Automates pFUnit installation for development

set -e

# Define installation path
PFUNIT_ROOT="$(pwd)/extern/pfunit"
PFUNIT_BUILD_DIR="${PFUNIT_ROOT}/build"
PFUNIT_INSTALL_DIR="${PFUNIT_ROOT}/installed"

echo "Building pFUnit into: ${PFUNIT_INSTALL_DIR}"

# Clone pFUnit if it does not exist
if [ ! -d "${PFUNIT_ROOT}" ]; then
    echo "Cloning pFUnit..."
    git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git "${PFUNIT_ROOT}"
fi

# Build pFUnit
mkdir -p "${PFUNIT_BUILD_DIR}"
cd "${PFUNIT_BUILD_DIR}"

echo "Configuring pFUnit..."
cmake .. \
    -DCMAKE_INSTALL_PREFIX="${PFUNIT_INSTALL_DIR}" \
    -DENABLE_MPI_F08=YES \
    -DSKIP_OPENMP=YES \
    -DCMAKE_BUILD_TYPE=Release

echo "Building and installing pFUnit..."
make -j4
make install

echo "----------------------------------------------------"
echo "pFUnit installed successfully!"
echo "To use this in your project, set the following:"
echo "export PFUNIT_DIR=${PFUNIT_INSTALL_DIR}/PFUNIT-4.18/cmake"
echo "----------------------------------------------------"
