FROM gmao/llvm-flang-openmpi:latest

RUN apt-get update && apt-get install -y --no-install-recommends ccache \
    && rm -rf /var/lib/apt/lists/*

RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/root/.local/bin:/root/.cargo/bin:$PATH"
ENV FC=mpifort CC=mpicc CXX=mpicxx
ENV CMAKE_Fortran_COMPILER_LAUNCHER=ccache \
    CMAKE_C_COMPILER_LAUNCHER=ccache \
    CMAKE_CXX_COMPILER_LAUNCHER=ccache \
    CCACHE_DIR=/ccache
ENV OMPI_ALLOW_RUN_AS_ROOT=1 OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1

WORKDIR /workspace

# pFUnit layer — rebuilds only when install_deps.sh changes
COPY scripts/install_deps.sh scripts/install_deps.sh
RUN bash scripts/install_deps.sh

# Python deps layer — rebuilds only when lockfile changes
COPY pyproject.toml uv.lock ./
RUN uv sync -q

COPY . .

ENV CI=true
ENTRYPOINT []
CMD ["./bin/fun-dev", "unit", "--mode", "ci"]
