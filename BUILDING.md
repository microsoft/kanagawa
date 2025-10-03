TODO: Flesh this out - just collecting notes here for now.

__Third Party Libraries and Tools__

Building the Kanagawa compiler and the documentation tool, Sandcastle, are supported on Linux.
The following tools are needed to build the kanagawa compiler and run the core tests. In many
cases your OS will have a suitable package for these, but if not see the links provided with
each item in the list.

- GCC 11.4.0 or later
- ghcup (Haskell 9.6.7 and cabal 3.12.1.0) (see https://www.haskell.org/ghcup/install/#linux-ubuntu)
- Boost C++ library version 1.83.0 or later (see https://www.boost.org/)
- CMake version 3.26 or later (see https://cmake.org/)
- Ninja. It's not required to use Ninja; you can use any build tool supported by CMake, but we recommend Ninja for its speed and simplicity (see https://ninja-build.org/)
- Verilator version 5.036. See the [Verilator documentation](https://veripool.org/guide/latest/install.html#git-quick-install) for detailed instructions. Note that version 5.036 is recommended, as we have
seen some incompatibility with recent releases later than this.
- Python version 3.x

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

- Install the dependencies listed above.

- Change into the newly cloned directory
`cd kanagawa`

- Initialize git submodules
`git submodule update --init --recursive`

- Make a directory to build into. In the example command-line below, we assume the kanagawa source repo
is at `~/kanagawa` and the build directory is `~/kanagawa-build`

- Run cmake generate. In the example command-line below, explicit paths for each dependency are provided.
```
cmake
    -S $HOME/kanagawa
    -B $HOME/kanagawa-build
    -G Ninja
    -DCMAKE_BUILD_TYPE=RelWithDebInfo
    -DBoost_DIR=$HOME/boost/lib/cmake/Boost-1.83.0
    -DGHCUP_DIR=/home/user/.ghcup/bin
    -DVERILATOR_EXE=/opt/verilator/bin/verilator
```

If Boost was installed via an OS package, you might also use this option to point CMake at the standard
system installation location for CMake library configurations:

```
-DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu/cmake
```