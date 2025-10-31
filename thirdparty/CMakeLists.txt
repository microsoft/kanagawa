# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# CIRCT

if (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/circt/CMakeLists.txt)
  message(FATAL_ERROR "Cannot find CIRCT submodule. Run `git submodule update --init`")
endif()
if (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/circt/llvm/llvm/CMakeLists.txt)
  message(FATAL_ERROR "Cannot find CIRCT's LLVM submodule. Run `git submodule update --init --recursive` or `git submodule update --init` in `thirdparty/circt/llvm`")
endif()

# Required to link a dynamic library with a static library
if(UNIX)
    set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

include(${CMAKE_SOURCE_DIR}/build/cmake/msvc.cmake)

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

if (MSVC)
  set(LLVM_ENABLE_PDB "ON" CACHE STRING "Generate PDB files along with libraries")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /wd4996")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4996")
endif()

# Add CIRCT to the build
add_subdirectory(circt/llvm/llvm)