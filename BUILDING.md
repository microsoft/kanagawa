TODO: Flesh this out - just collecting notes here for now.

__Third Party Libraries and Tools__

Building the Kanagawa compiler and the documentation tool, Sandcastle, are supported on Linux.
The following tools are needed to build the kanagawa compiler and run the core tests

- GCC 11.4.0 or later
- Haskell 8.6.5 or later (see https://www.haskell.org/ghcup/install/#linux-ubuntu)
- Boost C++ library version 1.83.0 or later
- CMake version 3.26 or later
- Verilator version 5.036. See the [Verilator documentation](https://veripool.org/guide/latest/install.html#git-quick-install) for detailed instructions.

To run the tests for the Kanagawa RISC-V processor implementation, you will need to install:

- The RISC-V toolchain. It is recommended to use the
[SiFive riscv64 toolchain, version  10.1.0](https://static.dev.sifive.com/dev-tools/freedom-tools/v2020.08/riscv64-unknown-elf-gcc-10.1.0-2020.08.2-x86_64-linux-ubuntu14.tar.gz)

To build Sandcastle, you will need to install:
- Rust cargo version 1.88.0 or later (see https://doc.rust-lang.org/cargo/getting-started/installation.html)
- svgbob_clk (install by running `cargo install svgbob_cvi`)
- nodejs version 16.20.2 or later (see https://github.com/nodesource/distributions)
- yarn version 1.22.22 or later (see https://classic.yarnpkg.com/en/docs/install#linux-stable)

__Building Kanagawa__

- Check out the repository, for example:
`git clone https://github.com/microsoft/kanagawa.git`

- Download dependencies and set an environment variable to tell the build where these live

- Change into the newly cloned directory
`cd kanagawa`

- Initialize git submodules
`git submodule update --init --recursive`

