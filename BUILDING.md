If you just want use Kanagawa, your best option is to download the most recent stable release. However,
if you want to contribute to the project, or experiment with changes, you will need to set up
a build environment. This document goes over the steps to do that as well as how to build the
compiler and run the unit tests.

## Setting up your build environment

At this time, building the Kanagawa compiler and the documentation tool, Sandcastle, is supported on Linux.
If you are using Windows, you can use Windows Subsystem for Linux (WSL). At this time, the team is building
and testing the project on Ubuntu 22, and Ubuntu 24 in a WSL session.

## Building the Kanagawa Compiler

The steps to build the compiler are:

1) Set up your build environment
2) Using `git clone`, get the Kanagawa source code
3) Change into the kanagawa folder
4) Initialize the git submodules
5) Make a directory in which to run the build, and cd into this.
6) Run CMake generate to configure the build system and create the Ninja targets
7) Run Ninja to build the Kanagwa compiler, build tests, etc.

Each of these steps is discussed in more detail in the subsequent sections.

### Set up your build environment

Set the section below on the various third-party tools you will need. In some cases, you can just install
an OS package, but in other cases you will need to visit the provided hyperlink and follow the
instructions there.

### Checking out and initializing the repository

Check out the repository:

`git clone https://github.com/microsoft/kanagawa.git`

Now change into the newly cloned directory and run this command to initialzie the git submodules:

`git submodule update --init --recursive`

### Configuring the build

Make a directory to build into. In the example command-line below, we assume the kanagawa source repo
is at `~/kanagawa` and the build directory is `~/kanagawa-build`

Now run cmake generate to initialize the build system. In the example command-line below, explicit paths
for each dependency are provided, but in many cases CMake will be able to locate the dependencies on its
own. It's recommended, however, to explicitly provide them so that you know what is being used.

```
cmake \
    -S $HOME/kanagawa \
    -B $HOME/kanagawa-build \
    -G Ninja \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DBoost_DIR=$HOME/boost/lib/cmake/Boost-1.83.0 \
    -DGHCUP_DIR=/home/user/.ghcup/bin \
    -DVERILATOR_EXE=/usr/local/bin/verilator
    -DRISCV64_GCC=$HOME/riscv64-unknown-elf-gcc/riscv64-unknown-elf-gcc-10.1.0-2020.08.2-x86_64-linux-ubuntu14
```

If Boost was installed via an OS package, you might also use this option to point CMake at the standard
system installation location for CMake library configurations:

```
-DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu/cmake
```

### Building the compiler, unit tests, etc.

To build individual targets, use ninja:

```
# Build Kanagawa
ninja -j $(nproc) kanagawa_runtime

# Build the library tests
ninja -j $(nproc) library_tests
```

## Running unit tests

To run tests, we use ctest. The various test types use a prefix in the test name to allow a group of related tests
to be run separately.

```
  # Run library tests
  ctest --verbose -R "^library\\."

  # Run syntax (front-end) tests
  ctest --verbose -R "^syntax\\."

  # Run a single test
  ctest --verbose -R "^library\\.processor_risc_v_1_hart"
```

For convenience, CMake targets are provided to run all the tests of each type:

```
ninja -j $(nproc) run_library_tests
ninja -j $(nproc) run_syntax_tests
```

## Third Party Tools

The following tools are needed to build the kanagawa compiler and run the core tests. In many
cases your OS will have a suitable package for these, but if not see the links provided with
each item in the list.

- GCC 11.4.0 or later
- ghcup (Haskell 9.6.7 and cabal 3.12.1.0) (see https://www.haskell.org/ghcup/install/#linux-ubuntu)
- Boost C++ library version 1.83.0 or later (see https://www.boost.org/)
- CMake version 3.26 or later (see https://cmake.org/)
- Ninja. It's not required to use Ninja; you can use any build tool supported by CMake, but we recommend Ninja for its speed and simplicity (see https://ninja-build.org/)
- Verilator version 5.036. See the [Verilator documentation](https://veripool.org/guide/latest/install.html#git-quick-install) for detailed instructions. Note that version 5.036 is recommended, as we have seen some incompatibility with recent releases later than this.
- Python version 3.x

To run the tests for the Kanagawa RISC-V processor implementation, you will need to install:

- The RISC-V toolchain. It is recommended to use the
[SiFive riscv64 toolchain, version  10.1.0](https://static.dev.sifive.com/dev-tools/freedom-tools/v2020.08/riscv64-unknown-elf-gcc-10.1.0-2020.08.2-x86_64-linux-ubuntu14.tar.gz)

To build Sandcastle, you will need to install:
- Rust cargo version 1.88.0 or later (see https://doc.rust-lang.org/cargo/getting-started/installation.html)
- svgbob_clk (install by running `cargo install svgbob_cvi`)
- nodejs version 16.20.2 or later (see https://github.com/nodesource/distributions)
- yarn version 1.22.22 or later (see https://classic.yarnpkg.com/en/docs/install#linux-stable)

