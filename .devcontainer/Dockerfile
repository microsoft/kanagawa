# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.
#
# Dockerfile for the Kanagawa build environment.
# Matches the Ubuntu 24.04 environment used by CI (see BUILDING.md for details).
#
# Used as:
#   - A VS Code / GitHub Codespaces devcontainer (via devcontainer.json)
#   - A GitHub Copilot coding agent build environment

FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

# ── System packages ──────────────────────────────────────────────────────────
# Includes prerequisites for CMake (from Kitware repo), Verilator, GHCup,
# Boost, Rust, Node.js, and general build tooling.
RUN apt-get update && apt-get install -y --no-install-recommends \
        # Core build tools
        build-essential \
        git \
        curl \
        wget \
        ca-certificates \
        gnupg \
        apt-transport-https \
        software-properties-common \
        # Parallel build / caching
        ninja-build \
        ccache \
        # Python (required by CIRCT/LLVM and various scripts)
        python3 \
        python3-pip \
        # Verilator build prerequisites
        autoconf \
        flex \
        bison \
        help2man \
        perl \
        libfl2 \
        libfl-dev \
        zlib1g-dev \
        # GHCup / GHC prerequisites
        libgmp-dev \
        libffi-dev \
        libtinfo-dev \
        # Misc utilities
        unzip \
        xz-utils \
    && rm -rf /var/lib/apt/lists/*

# ── CMake 3.30+ (pre-built binary from GitHub releases) ──────────────────────
# Ubuntu 24.04 ships CMake 3.28, but the project requires 3.30+.
ARG CMAKE_VERSION=3.31.7
RUN wget -q \
        "https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}-linux-x86_64.tar.gz" \
        -O /tmp/cmake.tar.gz \
    && tar xzf /tmp/cmake.tar.gz -C /usr/local --strip-components=1 \
    && rm /tmp/cmake.tar.gz

# ── Boost 1.88.0 (built from source) ─────────────────────────────────────────
ARG BOOST_VERSION=1.88.0
RUN wget -q \
        "https://github.com/boostorg/boost/releases/download/boost-${BOOST_VERSION}/boost-${BOOST_VERSION}-b2-nodocs.tar.gz" \
        -O /tmp/boost.tar.gz \
    && tar xzf /tmp/boost.tar.gz -C /tmp \
    && cd "/tmp/boost-${BOOST_VERSION}" \
    && ./bootstrap.sh --prefix=/usr/local \
    && ./b2 install -j"$(nproc)" \
    && rm -rf /tmp/boost.tar.gz "/tmp/boost-${BOOST_VERSION}"

# ── Verilator 5.040 (built from source) ──────────────────────────────────────
# Ubuntu 24.04 packages an older version; the project requires exactly 5.040.
ARG VERILATOR_VERSION=5.040
RUN git clone --depth 1 --branch "v${VERILATOR_VERSION}" \
        https://github.com/verilator/verilator /tmp/verilator \
    && cd /tmp/verilator \
    && autoconf \
    && ./configure \
    && make -j"$(nproc)" \
    && make install \
    && rm -rf /tmp/verilator

# ── GHCup, GHC 9.6.7, and Cabal 3.12.1.0 ────────────────────────────────────
ARG GHC_VERSION=9.6.7
ARG CABAL_VERSION=3.12.1.0
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC_VERSION} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=${CABAL_VERSION} \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    GHCUP_INSTALL_BASE_PREFIX=/opt \
    CABAL_DIR=/opt/.cabal
ENV PATH="/opt/.ghcup/bin:${PATH}"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
    && cabal update

# ── RISC-V toolchain (xpack-riscv-none-elf-gcc v15.2.0-1) ───────────────────
ARG RISCV_VERSION=15.2.0-1
ENV RISCV64_GCC=/opt/riscv-none-elf-gcc
RUN ARCHIVE="xpack-riscv-none-elf-gcc-${RISCV_VERSION}-linux-x64.tar.gz" \
    && wget -q \
        "https://github.com/xpack-dev-tools/riscv-none-elf-gcc-xpack/releases/download/v${RISCV_VERSION}/${ARCHIVE}" \
        -O /tmp/riscv.tar.gz \
    && mkdir -p "${RISCV64_GCC}" \
    && tar xzf /tmp/riscv.tar.gz -C "${RISCV64_GCC}" --strip-components=1 \
    && rm /tmp/riscv.tar.gz
ENV PATH="${RISCV64_GCC}/bin:${PATH}"

# ── Rust and svgbob_cli (for Sandcastle) ─────────────────────────────────────
ENV CARGO_HOME=/opt/cargo \
    RUSTUP_HOME=/opt/rustup
ENV PATH="/opt/cargo/bin:${PATH}"
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
        | sh -s -- -y --default-toolchain stable --no-modify-path \
    && cargo install svgbob_cli --version 0.7.6

# ── Node.js and yarn (for Sandcastle) ────────────────────────────────────────
# Ubuntu 24.04 ships Node.js 18.x (≥ the 16.20.2 minimum required).
RUN apt-get update \
    && apt-get install -y --no-install-recommends nodejs npm \
    && npm install -g yarn \
    && rm -rf /var/lib/apt/lists/*

# ── pandoc (for documentation checks) ────────────────────────────────────────
ARG PANDOC_VERSION=3.5
RUN wget -q \
        "https://github.com/jgm/pandoc/releases/download/${PANDOC_VERSION}/pandoc-${PANDOC_VERSION}-1-amd64.deb" \
        -O /tmp/pandoc.deb \
    && dpkg -i /tmp/pandoc.deb \
    && rm /tmp/pandoc.deb

# ── Environment variables used by CMake ──────────────────────────────────────
ENV BOOST_DIR=/usr/local/lib/cmake/Boost-1.88.0 \
    GHCUP_DIR=/opt/.ghcup/bin \
    VERILATOR_EXE=/usr/local/bin/verilator

WORKDIR /workspace

CMD ["/bin/bash"]
