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

# ── netcdf-fortran ────────────────────────────────────────────────────────────
# Fortran bindings over an existing netcdf-C (system nc-config, or point
# NETCDF_C_ROOT at a prefix — e.g. an HPC /app install).  Skips itself when
# nf-config is already on PATH (brew/module case); SKIP_NETCDF=1 forces skip.

if [ -n "${SKIP_NETCDF:-}" ]; then
	echo "SKIP_NETCDF set — skipping netcdf-fortran build."
elif command -v nf-config >/dev/null 2>&1; then
	echo "System netcdf-fortran found ($(command -v nf-config)) — skipping build."
else

	NF_SRC="${PROJ_ROOT}/extern/netcdf-fortran/src"
	NF_BUILD="${PROJ_ROOT}/extern/netcdf-fortran/build"
	NF_INSTALL="${PROJ_ROOT}/extern/netcdf-fortran/installed"

	if [ -n "${NETCDF_C_ROOT:-}" ]; then
		NC_PREFIX="${NETCDF_C_ROOT}"
	elif command -v nc-config >/dev/null 2>&1; then
		NC_PREFIX="$(nc-config --prefix)"
	else
		echo "ERROR: netcdf-fortran needs a netcdf-C install — put nc-config on PATH or set NETCDF_C_ROOT."
		exit 1
	fi
	echo "netcdf-C prefix: ${NC_PREFIX}"
	echo "netcdf-fortran install: ${NF_INSTALL}"

	if [ ! -d "${NF_SRC}/.git" ]; then
		echo "Cloning netcdf-fortran..."
		git clone https://github.com/Unidata/netcdf-fortran.git "${NF_SRC}"
	fi

	echo "Configuring netcdf-fortran..."
	cmake -S "${NF_SRC}" -B "${NF_BUILD}" \
		-DCMAKE_INSTALL_PREFIX="${NF_INSTALL}" \
		-DCMAKE_PREFIX_PATH="${NC_PREFIX}" \
		-DBUILD_SHARED_LIBS=OFF \
		-DNETCDF_ENABLE_TESTS=OFF \
		-DCMAKE_BUILD_TYPE=Release

	echo "Building and installing netcdf-fortran..."
	cmake --build "${NF_BUILD}" -j"${NPROC:-$(nproc 2>/dev/null || sysctl -n hw.logicalcpu)}"
	cmake --install "${NF_BUILD}"

	echo "----------------------------------------------------"
	echo "netcdf-fortran installed to: ${NF_INSTALL}"
	echo "(the FUNWAVE configure probes this prefix automatically)"
	echo "----------------------------------------------------"

fi

# ── PnetCDF ───────────────────────────────────────────────────────────────────
# Self-contained parallel I/O library (MPI + Fortran, no netcdf-C/HDF5
# dependency) — the pragmatic parallel-write path on clusters whose netcdf-C
# lacks parallel support.  No FUNWAVE consumer yet; installed ahead of the
# parallel NetCDF writer.  Release tarball (git needs autoreconf).

PNETCDF_VERSION="${PNETCDF_VERSION:-1.14.0}"

if [ -n "${SKIP_PNETCDF:-}" ]; then
	echo "SKIP_PNETCDF set — skipping PnetCDF build."
elif command -v pnetcdf-config >/dev/null 2>&1; then
	echo "System PnetCDF found ($(command -v pnetcdf-config)) — skipping build."
else

	PN_ROOT="${PROJ_ROOT}/extern/pnetcdf"
	PN_SRC="${PN_ROOT}/pnetcdf-${PNETCDF_VERSION}"
	PN_INSTALL="${PN_ROOT}/installed"

	echo "PnetCDF install: ${PN_INSTALL}"

	mkdir -p "${PN_ROOT}"
	if [ ! -d "${PN_SRC}" ]; then
		echo "Fetching PnetCDF ${PNETCDF_VERSION}..."
		curl -fL "https://parallel-netcdf.github.io/Release/pnetcdf-${PNETCDF_VERSION}.tar.gz" |
			tar -xz -C "${PN_ROOT}"
	fi

	echo "Configuring PnetCDF..."
	(cd "${PN_SRC}" && ./configure --prefix="${PN_INSTALL}" \
		MPIF90="${MPIF90:-mpifort}" MPICC="${MPICC:-mpicc}" --disable-shared)

	echo "Building and installing PnetCDF..."
	make -C "${PN_SRC}" -j"${NPROC:-$(nproc 2>/dev/null || sysctl -n hw.logicalcpu)}"
	make -C "${PN_SRC}" install

	echo "----------------------------------------------------"
	echo "PnetCDF installed to: ${PN_INSTALL}"
	echo "----------------------------------------------------"

fi

# ── HYPRE ─────────────────────────────────────────────────────────────────────

if [ -n "${SKIP_HYPRE:-}" ]; then
	echo "SKIP_HYPRE set — skipping HYPRE build."
	exit 0
fi

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
