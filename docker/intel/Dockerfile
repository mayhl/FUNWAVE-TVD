FROM intel/oneapi-hpckit:latest

SHELL ["/bin/bash", "-c"]

RUN apt-get update && apt-get install -y --no-install-recommends \
        git python3 python3-pip curl ca-certificates make m4 ccache \
    && pip3 install cmake --break-system-packages \
    && rm -rf /var/lib/apt/lists/*

RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/root/.local/bin:/root/.cargo/bin:$PATH"

# Convenience wrapper: source oneAPI env then exec arguments
RUN printf '#!/bin/bash\n. /opt/intel/oneapi/setvars.sh --force >/dev/null 2>&1\nexec "$@"\n' \
        > /usr/local/bin/irun && chmod +x /usr/local/bin/irun

ENV FC=mpiifx CC=mpiicx CXX=mpiicpx
ENV CMAKE_Fortran_COMPILER_LAUNCHER=ccache \
    CMAKE_C_COMPILER_LAUNCHER=ccache \
    CMAKE_CXX_COMPILER_LAUNCHER=ccache \
    CCACHE_DIR=/ccache

WORKDIR /workspace

# pFUnit layer — rebuilds only when install_deps.sh changes
COPY scripts/install_deps.sh scripts/install_deps.sh
RUN irun bash scripts/install_deps.sh

# Python deps layer — rebuilds only when lockfile changes
COPY pyproject.toml uv.lock ./
RUN uv sync -q

COPY . .

# Patch rdtscp → rdtsc+nop in Intel MPI's libmpi.so at runtime only.
# Docker Desktop on Mac doesn't virtualise rdtscp even though CPUID reports it
# available, causing SIGILL in MPL_wtime_init. Script self-skips on CI Linux
# where rdtscp works natively.
RUN irun python3 scripts/patch_intel_mpi.py

ENV CI=true
CMD ["irun", "./bin/fun-dev", "unit", "--mode", "ci"]
