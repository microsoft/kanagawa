# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# This file configures the build for the sandcastle code documentation tool

# Set the directory where cabal will be store intermediate files
set(CABAL_WORKING_DIR ${CMAKE_BINARY_DIR}/sandcastletemp)
file(MAKE_DIRECTORY ${CABAL_WORKING_DIR})

#
# Build the sandcastle exe using cabal

# Where cabal will output the executable (a stable path)
set(SANDCASTLE_EXE_DEST "${KANAGAWA_EXE_DIR}/sandcastle${CMAKE_EXECUTABLE_SUFFIX}")

set(CABAL_OPTIONS
  "--with-compiler=${GHC_EXE}"
  "--builddir=${CABAL_WORKING_DIR}/dist"
  "--logs-dir=${CABAL_WORKING_DIR}/logs"
  "v2-build"
  "--enable-tests"
)

set(CABAL_OPTIONS_TEST
  "--with-compiler=${GHC_EXE}"
  "--builddir=${CABAL_WORKING_DIR}/dist"
  "--logs-dir=${CABAL_WORKING_DIR}/logs"
  "--enable-tests"
  "v2-run"
  "test"
  "--"
  "--no-create"
)

# Tell CMake about the source file dependencies of the cabal build

file(GLOB_RECURSE SANDCASTLE_HS_SOURCES CONFIGURE_DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/*.hs"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.lhs"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.hsc"
)

file(GLOB SANDCASTLE_CABAL_FILES CONFIGURE_DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}*.cabal"
    "${CMAKE_CURRENT_SOURCE_DIR}/cabal.project"
    "${CMAKE_CURRENT_SOURCE_DIR}/cabal.project.local"
    "${CMAKE_CURRENT_SOURCE_DIR}/cabal.project.freeze"
)

list(APPEND SANDCASTLE_HS_SOURCES ${SANDCASTLE_CABAL_FILES})

# Build the sandcastle executable and copy it to the dist/bin directory
add_custom_command(
  OUTPUT ${SANDCASTLE_EXE_DEST}
  COMMAND ${CMAKE_COMMAND} -E echo "Building sandcastle exe with Cabal"
  COMMAND ${CMAKE_COMMAND} -E env
    TMP=${CABAL_WORKING_DIR}
    TEMP=${CABAL_WORKING_DIR}
    ${CABAL_EXE} ${CABAL_OPTIONS}
  COMMAND ${CMAKE_COMMAND} -E make_directory "${KANAGAWA_EXE_DIR}"
  COMMAND
    ${CMAKE_COMMAND} -E copy_if_different
    "${CABAL_WORKING_DIR}/dist/build/x86_64-linux/${GHC_NAME}/sandcastle-1.0.0/x/sandcastle/build/sandcastle/sandcastle${CMAKE_EXECUTABLE_SUFFIX}"
    "${SANDCASTLE_EXE_DEST}"
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/tools/sandcastle
  DEPENDS ${SANDCASTLE_HS_SOURCES}
  VERBATIM
  USES_TERMINAL
)

# Add an internal test for sandcastle
add_test(
  NAME sandcastle.internal
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND
    "${CABAL_EXE}" ${CABAL_OPTIONS_TEST}
)

# Phony target representing presence of the exe
add_custom_target(sandcastle_exe DEPENDS "${SANDCASTLE_EXE_DEST}")

# IMPORTED executable for convenient references. You can get the path to the exe with
add_executable(sandcastle::exe IMPORTED GLOBAL)
set_target_properties(sandcastle::exe PROPERTIES
    IMPORTED_LOCATION "${SANDCASTLE_EXE_DEST}"
)

# Make the imported exe depend on the build of the real exe
add_dependencies(sandcastle::exe sandcastle_exe)
