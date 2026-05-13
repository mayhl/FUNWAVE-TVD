FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# LLVM apt repo for flang-new
RUN apt-get update && apt-get install -y --no-install-recommends \
        wget gnupg ca-certificates lsb-release software-properties-common \
    && wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
    && add-apt-repository "deb http://apt.llvm.org/jammy/ llvm-toolchain-jammy-18 main" \
    && apt-get update && apt-get install -y --no-install-recommends \
        flang-18 clang-18 \
        openmpi-bin libopenmpi-dev \
        cmake git python3 curl \
    && ln -sf /usr/bin/flang-new-18 /usr/local/bin/flang-new \
    && rm -rf /var/lib/apt/lists/*

RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/root/.local/bin:/root/.cargo/bin:$PATH"
ENV FC=flang-new CC=clang-18 CXX=clang++-18

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
