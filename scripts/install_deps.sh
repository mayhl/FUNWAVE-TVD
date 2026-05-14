#!/usr/bin/env bash
# install_deps.sh — builds HYPRE from source into extern/hypre/installed/

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJ_ROOT="$(dirname "$SCRIPT_DIR")"

HYPRE_SRC="${PROJ_ROOT}/extern/hypre/src"
HYPRE_BUILD="${PROJ_ROOT}/extern/hypre/build"
HYPRE_INSTALL="${PROJ_ROOT}/extern/hypre/installed"

echo "HYPRE source:  ${HYPRE_SRC}"
echo "HYPRE install: ${HYPRE_INSTALL}"

if [ ! -d "${HYPRE_SRC}/.git" ]; then
    echo "Cloning HYPRE..."
    git clone https://github.com/hypre-space/hypre.git "${HYPRE_SRC}"
fi

echo "Configuring HYPRE..."
cmake -S "${HYPRE_SRC}/src" -B "${HYPRE_BUILD}" \
    -DCMAKE_INSTALL_PREFIX="${HYPRE_INSTALL}" \
    -DHYPRE_ENABLE_SHARED=OFF \
    -DHYPRE_WITH_MPI=ON \
    -DCMAKE_BUILD_TYPE=Release

echo "Building and installing HYPRE..."
cmake --build "${HYPRE_BUILD}" -j"${NPROC:-$(nproc 2>/dev/null || sysctl -n hw.logicalcpu)}"
cmake --install "${HYPRE_BUILD}"

echo "----------------------------------------------------"
echo "HYPRE installed to: ${HYPRE_INSTALL}"
echo "Pass to CMake: -DHYPRE_DIR=${HYPRE_INSTALL}"
echo "----------------------------------------------------"
