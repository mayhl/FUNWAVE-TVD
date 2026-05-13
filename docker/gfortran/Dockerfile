FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
        gfortran gcc g++ make m4 openmpi-bin libopenmpi-dev \
        git python3 python3-pip curl ca-certificates \
    && pip3 install cmake \
    && rm -rf /var/lib/apt/lists/*

RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/root/.local/bin:/root/.cargo/bin:$PATH"

ENV FC=mpifort CC=mpicc CXX=mpicxx
# Allow MPI to run as root inside Docker containers
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
CMD ["./bin/fun", "unit", "--mode", "ci"]
