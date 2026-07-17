FROM nvcr.io/nvidia/nvhpc:24.11-devel-cuda12.6-ubuntu22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        git python3 python3-pip curl ca-certificates make m4 ccache \
    && pip3 install cmake \
    && rm -rf /var/lib/apt/lists/*

RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/root/.local/bin:/root/.cargo/bin:$PATH"

# NVIDIA HPC SDK ships its own OpenMPI; add MPI wrappers to PATH
ENV PATH="/opt/nvidia/hpc_sdk/Linux_x86_64/24.11/comm_libs/mpi/bin:${PATH}" \
    LD_LIBRARY_PATH="/opt/nvidia/hpc_sdk/Linux_x86_64/24.11/comm_libs/mpi/lib:${LD_LIBRARY_PATH}"

ENV FC=mpifort CC=mpicc CXX=mpicxx
ENV CMAKE_Fortran_COMPILER_LAUNCHER=ccache \
    CMAKE_C_COMPILER_LAUNCHER=ccache \
    CMAKE_CXX_COMPILER_LAUNCHER=ccache \
    CCACHE_DIR=/ccache
# Allow MPI to run as root inside Docker containers
ENV OMPI_ALLOW_RUN_AS_ROOT=1 OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1
# Widest unit test wants 4 ranks; CI runners expose only 2 cores, so let
# Open MPI oversubscribe (PRTE var covers Open MPI 5)
ENV OMPI_MCA_rmaps_base_oversubscribe=1 \
    PRTE_MCA_rmaps_default_mapping_policy=:oversubscribe

WORKDIR /workspace

# pFUnit layer — rebuilds only when install_deps.sh changes
COPY scripts/install_deps.sh scripts/install_deps.sh
RUN SKIP_PFUNIT=1 bash scripts/install_deps.sh

# Python deps layer — rebuilds only when lockfile changes
COPY pyproject.toml uv.lock ./
RUN uv sync -q

COPY . .

ENV CI=true
CMD ["./bin/fun-dev", "unit", "--mode", "ci", "--compile-only"]
