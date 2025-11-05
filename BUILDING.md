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
    -DBoost_DIR=$HOME/boost/lib/cmake/Boost-1.88.0 \
    -DGHCUP_DIR=/home/user/.ghcup/bin \
    -DVERILATOR_EXE=/usr/local/bin/verilator
    -DRISCV64_GCC=$HOME/riscv64-unknown-elf-gcc/riscv64-unknown-elf-gcc-10.1.0-2020.08.2-x86_64-linux-ubuntu14
```

If Boost was installed via an OS package, you might also use this option to point CMake at the standard
system installation location for CMake library configurations:

```
-DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu/cmake
```

If building in WSL or some memory constrained environment, you may wish to add this option:

```
-DKANAGAWA_HEAVY_PARALLEL_JOBS=2
```

This restricts the number of parallel jobs, for memory intensive tasks such as
Kanagawa or Verilator compilation, to 2 - even if you forget to specify a `-j2` argument to CMake or Ninja.

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

Note that in WSL environments, processes will fail if the total memory footprint exceeds available memory and swap space.
Because of limited memory and swapfile in WSL sessions, it is easy to run into this when running the unit tests. We
recommend limiting the number of concurrent processes to 2 in these environments (with -j2 option to ninja or ctest).

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
ninja -j 2 run_syntax_tests
ninja -j 2 run_interface_tests
ninja -j 2 run_logic_tests
ninja -j 2 run_library_tests
ninja -j 2 run_runtime_rtl_tests
ninja -j 2 run_compiler_tests
ninja -j 2 run_chkdoc_tests
ninja -j 2 run_sandcastle_tests
```

These convenience targets should cause a build of any dependencies.

The following table lists the different test types and the relevant CMake targets and sample ctest command line.

// ...existing code...

The following table lists the different test types and the relevant CMake targets and sample ctest command line.

| Test Type | Description | Build Target | Run Target | CTest Command |
|-----------|-------------|--------------|------------|---------------|
| Syntax | Front-end parser and syntax validation tests | syntax_tests | run_syntax_tests | `ctest --verbose -R "^syntax\\."` |
| Interface | Interface and API tests | interface_tests | run_interface_tests | `ctest --verbose -R "^interface\\."` |
| Logic | Logic and behavioral tests | logic_tests | run_logic_tests | `ctest --verbose -R "^logic\\."` |
| Library | Standard library functionality tests | library_tests | run_library_tests | `ctest --verbose -R "^library\\."` |
| Runtime RTL | Runtime and RTL (Register Transfer Level) tests | runtime_rtl_tests | run_runtime_rtl_tests | `ctest --verbose -R "^runtime\\."` |
| Compiler | Compiler functionality and code generation tests | compiler_tests | run_compiler_tests | `ctest --verbose -R "^compiler\\."` |
| Chkdoc | Documentation checking and validation tests | chkdoc_tests | run_chkdoc_tests | `ctest --verbose -R "^chkdoc\\."` |
| Sandcastle | Documentation generation tool tests | N/A | run_sandcastle_tests | `ctest --verbose -R "^sandcastle\\."` |

## Third Party Tools

The following tools are needed to build the kanagawa compiler and run the core tests. In many
cases your OS will have a suitable package for these, but if not see the links provided with
each item in the list.

- GCC 11.4.0 or later
- ghcup (Haskell 9.6.7 and cabal 3.12.1.0) (see https://www.haskell.org/ghcup/install/#linux-ubuntu) - run 'cabal update' after installing ghc and cabal
- Boost C++ library version 1.88.0 or later (see https://www.boost.org/)
- CMake version 3.30 or later (see https://cmake.org/)
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


## Building on Windows

At this time, the unit tests are only supported on Linux. However, you can build and run the compiler and
related tools on Windows. Here are the dependencies that you must install:

- C/C++ compiler. CMake should auto-detect it. Visual Studio 2022 Community Edition is a good option.
- CMake version 3.30 or later (see https://cmake.org/)
- Ninja. It's not required to use Ninja; you can use any build tool supported by CMake, but we recommend Ninja for its speed and simplicity (see https://ninja-build.org/)
- [ghcup](https://www.haskell.org/ghcup/). After installation, close and re-open your Powershell session and then run `ghcup tui` to launch the interactive version of ghcup. Install Haskell 9.6.7 and cabal 3.12.1.0. Run `cabal update` after installation.
- Boost C++ library version 1.88.0 or later (see https://www.boost.org/). After extracting the archive, you will need to change into the extracted directory and run `.\bootstrap.bat` and then `.\b2.exe install --prefix=target_directory` (substitute `target_directory` with where you want Boost installed). The proper value to pass as Boost_DIR to CMake will be something like `...\boost_1_88_0\stage\lib\cmake\Boost-1.88.0`.
To tell CMake about Boost so that the `find_package` command can find it, add `<boost-install-dir>\lib\cmake` to `CMAKE_PREFIX_PATH`. For example: `-DCMAKE_PREFIX_PATH=D:\boost.1.88.0\lib\cmake`

Run cmake generate to initialize the build system. Here's an example command line - replace the paths with values appropriate for your set-up:

```
cmake -S kanagawa -B kanagawa-build -G Ninja -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_PREFIX_PATH=D:\boost.1.88.0\lib\cmake -DGHCUP_DIR=D:/ghcup
```

## Preparing a release

There is a GitHub workflow (Nightly prerelease) that runs nightly at 08:00 UTC. If there have been any changes checked into main since it last run, it will prepare and publish a release.
These releases are marked as pre-release, and they are named based on the date.

For official releases, the process is as follows:

- Update CHANGELOG.MD with a summary of changes since the last official release. You can
  check the commit history, or the summary in the nightly release description to help
  with this task.
- Update VERSION to increment the version number - major, minor, or patch as appropriate.
- Prepare a PR with the aforementioned changes and merge it to main.
- Once the PR is merged, run the Release workflow (release.yml). This will build
  and publish a release, and if that is successful, assign a tag.
