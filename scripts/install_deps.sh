#!/usr/bin/env bash
# install_deps.sh — builds pFUnit and HYPRE from source

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJ_ROOT="$(dirname "$SCRIPT_DIR")"

# ── pFUnit ────────────────────────────────────────────────────────────────────
# SKIP_PFUNIT=1 skips it: compile-only images (nvfortran ICEs on the pFUnit
# generated drivers; GPU validation will use regression runs, not units)

if [ -n "${SKIP_PFUNIT:-}" ]; then
	echo "SKIP_PFUNIT set — skipping pFUnit build."
else

	PFUNIT_SRC="${PROJ_ROOT}/extern/pfunit/src"
	PFUNIT_BUILD="${PROJ_ROOT}/extern/pfunit/build"
	PFUNIT_INSTALL="${PROJ_ROOT}/extern/pfunit/installed"

	echo "pFUnit source:  ${PFUNIT_SRC}"
	echo "pFUnit install: ${PFUNIT_INSTALL}"

	if [ ! -d "${PFUNIT_SRC}/.git" ]; then
		echo "Cloning pFUnit..."
		git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git "${PFUNIT_SRC}"
	fi

	PFUNIT_MPI_F08="${PFUNIT_ENABLE_MPI_F08:-YES}"

	echo "Configuring pFUnit..."
	cmake -S "${PFUNIT_SRC}" -B "${PFUNIT_BUILD}" \
		-DCMAKE_INSTALL_PREFIX="${PFUNIT_INSTALL}" \
		-DENABLE_MPI=YES \
		-DENABLE_MPI_F08="${PFUNIT_MPI_F08}" \
		-DSKIP_OPENMP=YES \
		-DCMAKE_BUILD_TYPE=Release

	echo "Building and installing pFUnit..."
	cmake --build "${PFUNIT_BUILD}" -j"${NPROC:-$(nproc 2>/dev/null || sysctl -n hw.logicalcpu)}"
	cmake --install "${PFUNIT_BUILD}"

	echo "----------------------------------------------------"
	echo "pFUnit installed to: ${PFUNIT_INSTALL}"
	echo "----------------------------------------------------"

fi

# ── HYPRE ─────────────────────────────────────────────────────────────────────

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
