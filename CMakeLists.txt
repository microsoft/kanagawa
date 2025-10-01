# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

cmake_minimum_required(VERSION 3.30)

project(kanagawa LANGUAGES CXX)

set(KANAGAWA_DIST_DIR "${CMAKE_BINARY_DIR}/dist")
set(KANAGAWA_BIN_DIR "${KANAGAWA_DIST_DIR}/bin")

# Find all dependent libraries, headers, and tools.
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/deps.cmake)

# Set CIRCT-specific variables
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/circt.cmake)
# Add CIRCT to the build
add_subdirectory(${CMAKE_SOURCE_DIR}/thirdparty/circt/llvm/llvm)

# Add build of the Kanagawa compiler
add_subdirectory(compiler)