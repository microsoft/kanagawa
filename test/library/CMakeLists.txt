# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# This file contains a set of unit tests to validate correct functionality
# of the Kanagawa standard library. The tests are run within the Verilator
# RTL simulator.


# Create a CMake target to build a library unit test. This function also
# creates a CTest target that can be used to run the test.
#
# Usage:
#   add_library_test(<test_name> <test_source>
#       [OPTIONS <opt1> <opt2> ...]
#       [TESTBENCH <path-to-systemverilog-file>]
#   )
#
#   test_name - the name of the ctest that will be created. Note that the
#               actual ctest name will be prefixed with 'library.'
#               (i.e. library.test_name).
#   test_source - a path to a Kanagawa source file that contains the test code
#   OPTIONS (optional) - A list of options to pass to the Kanagawa compiler
#   TESTBENCH (optional) - A custom SystemVerilog testbench (see note below).
#
# If no value is provided for TESTBENCH, a default testbench in library/test/testbench.sv
# will be used. The default testbench is designed to work with a kanagawa test runner
# defined in library/test/runner.pd.
#
# If the test requires a custom testbench the path to the SystemVerilog file containing
# that testbench must be provided via the TESTBENCH argument. The top-level (testbench)
# module in that file must be named "Testbench".
#
# The function creates a CMake target named library_test.${test_name} which you can use
# to build the test (both Kanagawa code generation and Verilator compilation).
#
function(add_library_test test_name test_source)
  set(_opts)
  set(_one TESTBENCH)
  set(_multi OPTIONS)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT test_name)
    message(FATAL_ERROR "add_library_test: missing <test_name> name as first argument.")
  endif()

  if (NOT test_source)
    message(FATAL_ERROR "add_library_test: missing <test_source> name as second argument.")
  endif()

  set(FULL_TEST_NAME, "library.${test_name}")

  set(TARGET_NAME "library_test.${test_name}")

  set(OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${test_name}")

  set(KANAGAWA_OPTIONS
    "--Wall"
    "--Werror"
    "--check-narrowing-assignment"
    "--parse-docs"
    "--max-block-ram=10000000"
    "--frequency=250"
    "--reset-cycles=5"
    "--register-ratio=3"
    "--output=${OUTPUT_DIR}/"
    "--import-dir=${CMAKE_SOURCE_DIR}/library"
    "--resource-usage=${OUTPUT_DIR}/resources.rpt"
    "--define=verbose#true"
  )

  if (_ARG_OPTIONS)
    list(APPEND KANAGAWA_OPTIONS ${_ARG_OPTIONS})
  endif()

  add_kanagawa(${TARGET_NAME}
    CLEAN_OUTPUT_DIR
    OUTPUT_DIR ${OUTPUT_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    SOURCES ${test_source}
    OPTIONS ${KANAGAWA_OPTIONS}
  )

endfunction()

add_library_test(control_async "control/async.k")