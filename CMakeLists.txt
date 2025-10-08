# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

cmake_minimum_required(VERSION 3.30)

project(kanagawa LANGUAGES CXX)

enable_testing()

# Set the C++ standard required
set(CMAKE_CXX_STANDARD 17)
set(LIKER_USES_LIBRT "rt")
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Required to link a dynamic library with a static library
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set(KANAGAWA_EXE_DIR "${CMAKE_BINARY_DIR}/kanagawa")
set(KANAGAWA_EXE_NAME "kanagawa")
# This can not be the same as KANAGAWA_EXE_NAME (CMake does not like it)
set(KANAGAWA_SHARED_LIBRARY_NAME "kanagawa-backend")

# Find all dependent libraries, headers, and tools.
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/deps.cmake)

# Set CIRCT-specific variables
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/circt.cmake)
# Add CIRCT to the build
add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/circt/llvm/llvm)

# Add build of the Kanagawa compiler
add_subdirectory(compiler)

# Include helper functions for building unit tests
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/test_helper.cmake)

# Include RISC-V GCC wrapper and risc-v-elf2mem
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/risc-v-gcc.cmake)

# Unit tests
add_subdirectory(test)

# Tools (sandcastle, chkdoc, ...)
add_subdirectory(tools)
