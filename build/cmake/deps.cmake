
cmake_policy(SET CMP0167 NEW)

####
# Boost version 1.74.0 or later
find_package(Boost 1.74 CONFIG REQUIRED)

# Pick the header target name that exists:
if (TARGET Boost::headers)
  set(_boost_hdr_target Boost::headers)
elseif (TARGET Boost::boost)
  set(_boost_hdr_target Boost::boost)
else()
  message(FATAL_ERROR "No Boost header target found in this Boost package.")
endif()

get_target_property(BOOST_INCLUDE_DIR ${_boost_hdr_target} INTERFACE_INCLUDE_DIRECTORIES)
message(STATUS "Dependency: Boost version ${Boost_VERSION} at ${BOOST_INCLUDE_DIR}/boost")

####
# Verilator

# Users can override via: -DVERILATOR_EXE=/opt/verilator/bin/verilator
find_program(VERILATOR_EXE
  NAMES verilator
  HINTS
    "$ENV{VERILATOR_ROOT}/bin"
    "$ENV{HOME}/verilator/bin"
    /usr/local/bin
    /usr/bin
)
if (VERILATOR_EXE)
  message(STATUS "Dependency: Verilator at ${VERILATOR_EXE}")
else()
  message(WARNING "Verilator executable not found. Verilator-based tests will be disabled.")
endif()

####
# CIRCT

if (NOT EXISTS ${CMAKE_SOURCE_DIR}/thirdparty/circt/CMakeLists.txt)
  message(FATAL_ERROR "Cannot find CIRCT submodule. Run `git submodule update --init`")
endif()
if (NOT EXISTS ${CMAKE_SOURCE_DIR}/thirdparty/circt/llvm/llvm/CMakeLists.txt)
  message(FATAL_ERROR "Cannot find CIRCT's LLVM submodule. Run `git submodule update --init --recursive` or `git submodule update --init` in `thirdparty/circt/llvm`")
endif()

# CIRCT configuration
set(LLVM_ENABLE_PROJECTS "mlir" CACHE STRING "Enable MLIR project")
set(BUILD_SHARED_LIBS "OFF" CACHE STRING "Kanagawa doesn't use shared libs")
set(LLVM_TARGETS_TO_BUILD "X86" CACHE STRING "Part of CIRCT needs the X86 target")
set(STANDALONE_INSTALL "OFF" CACHE STRING "Kanagawa only uses libs from CIRCT")
set(LLVM_EXTERNAL_PROJECTS "circt" CACHE STRING "Enable the CIRCT project")
set(LLVM_EXTERNAL_CIRCT_SOURCE_DIR "${CMAKE_SOURCE_DIR}/thirdparty/circt" CACHE STRING "Location of CIRCT")
set(LLVM_ENABLE_TERMINFO "OFF" CACHE STRING "Don't use terminfo library")

# Disable CIRCT features which we don't need. (Yet.)
set(CLANG_TIDY_DISABLE "ON")
set(SYSTEMC_DISABLE "ON")
set(YOSYS_DISABLE "ON")
set(IVERILOG_DISABLE "ON")
set(CAPNP_DISABLE "ON")
set(OR_TOOLS_DISABLE "ON")
set(CIRCT_LLHD_SIM_ENABLED "OFF")
set(CIRCT_LEC_DISABLE "ON")
set(CIRCT_BINDINGS_PYTHON_ENABLED "OFF")
set(CIRCT_BINDINGS_TCL_ENABLED "OFF")

add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/circt/llvm/llvm)
