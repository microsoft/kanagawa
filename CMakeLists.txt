# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

cmake_minimum_required(VERSION 3.30)

project(kanagawa LANGUAGES CXX)

enable_testing()

# Set the C++ standard required
set(CMAKE_CXX_STANDARD 17)
set(LIKER_USES_LIBRT "rt")
set(CMAKE_CXX_STANDARD_REQUIRED True)

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

# Add helper function for running the Kanagawa compiler to generate RTL
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/kanagawa.cmake)

# Unit tests
add_subdirectory(test)
