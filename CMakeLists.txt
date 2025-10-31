# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

cmake_minimum_required(VERSION 3.30)


file(READ "${CMAKE_SOURCE_DIR}/VERSION" VERSION_RAW)
string(STRIP "${VERSION_RAW}" VERSION)
message(STATUS "Project version: ${VERSION}")

project(kanagawa VERSION ${VERSION} LANGUAGES CXX)

enable_testing()

# Set the C++ standard required
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# This is where we stage binaries and generated docs during the build, so they are in a consistent location that is easy to locate
set(BUILD_OUTPUT_DIR "${CMAKE_BINARY_DIR}/dist")
set(BUILD_OUTPUT_BIN_DIR "${BUILD_OUTPUT_DIR}/bin")

set(KANAGAWA_EXE_NAME "kanagawa")
# This can not be the same as KANAGAWA_EXE_NAME (CMake does not like it)
set(KANAGAWA_SHARED_LIBRARY_NAME "kanagawa-backend")

# List of targets which comprise kanagawa_runtime
set(KANAGAWA_RUNTIME_TARGETS
    kanagawa_lib
    kanagawa_exe
)

# Find all dependent libraries, headers, and tools.
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/deps.cmake)

# Adds CIRCT to the build, but keeps any CIRCT-specific settings in a separate scope
add_subdirectory(thirdparty)

# Add build of the Kanagawa compiler
add_subdirectory(compiler)

# Configure heavy job concurrency (limit for memory-constrained environments)
# Both Kanagawa compilation and Verilator can be memory-intensive, so they share a job pool
set(KANAGAWA_HEAVY_PARALLEL_JOBS "" CACHE STRING
    "Maximum number of concurrent Kanagawa compiler and Verilator jobs. Leave empty for unlimited (default). Set to 2 for memory-constrained CI environments.")
if(KANAGAWA_HEAVY_PARALLEL_JOBS)
  if(NOT CMAKE_GENERATOR MATCHES "Ninja")
    message(WARNING "Heavy job pooling is only available with Ninja generators.")
  else()
    set_property(GLOBAL APPEND PROPERTY JOB_POOLS heavy_job_pool=${KANAGAWA_HEAVY_PARALLEL_JOBS})
    message(STATUS "Heavy job pool enabled with ${KANAGAWA_HEAVY_PARALLEL_JOBS} parallel jobs (Kanagawa compiler + Verilator)")
  endif()
endif()

# Include helper functions for building unit tests
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/test_helper.cmake)

# Include RISC-V GCC wrapper and risc-v-elf2mem
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/risc-v-gcc.cmake)

# Unit tests
add_subdirectory(test)

# Tools (sandcastle, chkdoc, ...)
add_subdirectory(tools)

# Include helpers for building a release
include(${CMAKE_CURRENT_SOURCE_DIR}/build/cmake/release.cmake)