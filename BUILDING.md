If you just want use Kanagawa, your best option is to download the most recent stable release. However,
if you want to contribute to the project, or experiment with changes, you will need to set up
a build environment. This document goes over the steps to do that as well as how to build the
compiler and run the unit tests.

## Setting up your build environment

At this time, the full Kanagawa build and test environment is supported on Linux (or Linux in WSL on Windows). However, building the compiler and related tools is supported on both Windows and MacOS, and all the unit tests, save those requiring the RISC-V cross-compiler,
have been verified on MacOS.

The CI checks that are run when a pull request is submitted run on Ubuntu 24, Windows 2019, and MacOS. The unit tests are only run on Ubuntu 24.

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
    -DGHCUP_DIR=$HOME/.ghcup/bin \
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
- Verilator version 5.040. See the [Verilator documentation](https://veripool.org/guide/latest/install.html#git-quick-install) for detailed instructions.
- Python version 3.x

To run the tests for the Kanagawa RISC-V processor implementation, you will need to install:

- The RISC-V toolchain. It is recommended to use the
[xPack RISC-V embedded toolchain, version  15.2.0-1](https://github.com/xpack-dev-tools/riscv-none-elf-gcc-xpack/releases/download/v15.2.0-1/xpack-riscv-none-elf-gcc-15.2.0-1-linux-x64.tar.gz)

To build Sandcastle, you will need to install:
- Rust cargo version 1.88.0 or later (see https://doc.rust-lang.org/cargo/getting-started/installation.html)
- svgbob_cli (install by running `cargo install svgbob_cli`)
- nodejs version 16.20.2 or later (see https://github.com/nodesource/distributions)
- yarn version 1.22.22 or later (see https://classic.yarnpkg.com/en/docs/install#linux-stable)

## Building on MacOS

You need to have `homebrew` installed to use the method described here. If you don't already have it installed,
you can install it by running this command in a terminal session:

```zsh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Once `homebrew` is installed, setting up a build environment that supports building the compiler and most of the 
unit tests is as simple as running these commands in a terminal:

```zsh
brew install cmake ninja boost verilator
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

To build Sandcastle and run the sandcastle and documentation check tests, run these commands to install the prerequisites:

```zsh
brew install nodejs yarn pandoc
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install svgbob_cli
```

The above set-up will let you build the compiler, sandcastle, and run all the unit tests save those that require
the RISC-V GCC cross-compiler. To install the cross-compiler, use these commands:

```zsh
brew tap riscv-software-src/riscv
brew install riscv-tools
```

Alternatively, you can grab the darwin-arm64 release of RISC-V GCC from here:

https://github.com/xpack-dev-tools/riscv-none-elf-gcc-xpack/releases/tag/v15.2.0-1/

This is the same version we use for testing on Linux, so it's a good choice.

Then follow the instructions above to clone the repo, initialize the submodules, etc. Here is an example CMake command (assuming RISCV-64 GCC installed via homebrew):

```zsh
cmake \
    -S $HOME/kanagawa \
    -B $HOME/kanagawa-build \
    -G Ninja \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DBoost_DIR=/opt/homebrew/opt/boost/lib/cmake \
    -DGHCUP_DIR=$HOME/.ghcup/bin \
    -DVERILATOR_EXE=/opt/homebrew/bin/verilator \
    -DRISCV64_GCC=/opt/homebrew/Cellar/riscv-gnu-toolchain/main
```

## Building on Windows

At this time, the unit tests are only supported on Linux. However, you can build and run the compiler and
related tools on Windows. The following step-by-step guide assumes a PowerShell session and Visual
Studio 2022 (any edition with the "Desktop development with C++" workload, including Community).
Adjust paths as appropriate for your environment.

### 1. Install Visual Studio 2022

Install Visual Studio 2022 with the "Desktop development with C++" workload. This provides the MSVC
compiler, CMake, and Ninja.

To make `cl.exe`, `cmake`, and `ninja` available in your PowerShell session, enter the
"Developer PowerShell for VS 2022" environment. 

Verify the tools are on your `PATH`:

```powershell
cmake --version
ninja --version
```

### 2. Install GHC and cabal via ghcup

Run the official ghcup bootstrap script in a non-interactive form so it can be scripted. The
arguments below skip the interactive prompts and install ghcup but not GHC, cabal, stack, or HLS
(we install specific versions of GHC and cabal in the next step):

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol =
    [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
Invoke-Command -ScriptBlock ([ScriptBlock]::Create(
    (Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) `
    -ArgumentList $false,$true,$true,$false,$false,$false,$false,"","","",""
```

This installs ghcup at `C:\ghcup`. Add it to `PATH` for the current session, then install the
required versions of GHC and cabal, set them as the default, and update the Hackage index:

```powershell
$env:Path = "C:\ghcup\bin;" + $env:Path
ghcup install ghc 9.6.7
ghcup install cabal 3.12.1.0
ghcup set ghc 9.6.7
ghcup set cabal 3.12.1.0
cabal update
```

Optionally, redirect the cabal package store to a different drive (useful if your system drive is
small):

```powershell
cabal user-config update -a "store-dir: E:\cache\cabal"
```

### 3. Install Boost 1.88.0

Download and extract the Boost source archive. The extraction step takes a few minutes because
Boost contains tens of thousands of files:

```powershell
New-Item -ItemType Directory -Path E:\cache\boost -Force | Out-Null
Invoke-WebRequest `
    -Uri "https://archives.boost.io/release/1.88.0/source/boost_1_88_0.zip" `
    -OutFile "E:\cache\boost_1_88_0.zip" -UseBasicParsing
Expand-Archive -Path "E:\cache\boost_1_88_0.zip" -DestinationPath "E:\cache\boost" -Force
```

Bootstrap and install Boost. Kanagawa only needs the Boost headers, so a headers-only install is
sufficient. The recommended approach is to use `b2` to generate the CMake config files, then use
`robocopy` to copy the headers in bulk (much faster than `b2`'s file-by-file install on Windows):

```powershell
cd E:\cache\boost\boost_1_88_0
.\bootstrap.bat
# Start the install to generate the CMake config files in the install prefix.
# You can cancel (Ctrl+C) once you see headers being copied -- the CMake config
# files are written early.
.\b2.exe install --prefix=E:\cache\boost\install --with-headers
# Bulk-copy all headers to the install prefix using robocopy
robocopy "E:\cache\boost\boost_1_88_0\boost" `
         "E:\cache\boost\install\include\boost-1_88\boost" /E /NFL /NDL /NJH /NP
```

Alternatively, you can let `b2 install` run to completion (slower, but simpler).

### 4. Configure the build with CMake

From the repository root, run CMake generate. The example below uses
`E:\cache\boost\install` for Boost and `C:\ghcup\bin` for ghcup; adjust paths as needed:

```powershell
cmake -S E:\git\kanagawa -B E:\git\kanagawa-build -G Ninja `
    -DCMAKE_BUILD_TYPE=RelWithDebInfo `
    -DBoost_DIR=E:\cache\boost\install\lib\cmake\Boost-1.88.0 `
    -DGHCUP_DIR=C:\ghcup\bin
```

The configure step takes a couple of minutes (CMake configures the bundled LLVM/CIRCT submodule
in addition to Kanagawa itself). When it finishes, you should see `Build files have been written
to: E:/git/kanagawa-build`.

### 5. Build the compiler

Build the `kanagawa_runtime` target. This compiles LLVM, MLIR, CIRCT, and the Kanagawa compiler,
and is a long-running build (tens of minutes to over an hour depending on your machine):

```powershell
ninja -C E:\git\kanagawa-build kanagawa_runtime
```

The resulting `kanagawa.exe` and `kanagawa-backend.dll` are staged in
`E:\git\kanagawa-build\dist\bin`.

### Notes

- Verilator and the RISC-V GCC cross-compiler are not available on Windows out of the box, so
  the corresponding tests are disabled automatically.
- If you re-open your PowerShell session, you must re-enter the VS dev environment and re-add ghcup to `PATH` (`$env:Path = "C:\ghcup\bin;" + $env:Path`)
  before running `cmake` or `ninja`.

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
