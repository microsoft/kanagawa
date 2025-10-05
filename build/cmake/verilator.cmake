# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# Helper function for running Verilator to compile SystemVerilog into
# a C++ simulation executable. Note that the built-in support for CMake that
# is part of Verilator is not used here, as it does the verilate step during
# the CMake generation/configure step, which is not desirable.
#
# Usage:
#   add_verilator(<verilate_target> <test_name>
#     [OUTPUT_DIR <dir>]
#     [WORKING_DIRECTORY <dir>]
#     [DEPENDS <target-or-file> ...]]
#     TESTBENCH <testbench-module-name>
#     SOURCES <src1> [<src2> ...]
#     [OPTIONS <opt1> <opt2> ...]
#   )
#
# Downstream can depend on <target> to ensure the that Verilation has run.
# The target will produce a C++ executable that can be run to execute the
# simulation. The path to the executable is available via the target property
# VERILATOR_OUTPUT.
#
function(add_verilator verilate_target test_name)
  set(_opts)
  set(_one  OUTPUT_DIR WORKING_DIRECTORY TESTBENCH)
  set(_multi SOURCES OPTIONS DEPENDS)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT VERILATOR_EXE)
    message(FATAL_ERROR "add_verilator: VERILATOR_EXE is not set. Please ensure Verilator is installed and VERILATOR_EXE is set to the path to the Verilator executable.")
  endif()

  if(NOT verilate_target)
    message(FATAL_ERROR "add_verilator: missing <verilate_target> name as first argument.")
  endif()

  if(NOT _ARG_TESTBENCH)
    message(FATAL_ERROR "add_verilator(${verilate_target}): TESTBENCH is required.")
  endif()

  if(NOT _ARG_SOURCES)
    message(FATAL_ERROR "add_verilator(${verilate_target}): SOURCES is required.")
  endif()

  # Default OUTPUT_DIR if not provided
  if(NOT _ARG_OUTPUT_DIR)
    set(_ARG_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}")
  endif()

  if(NOT _ARG_WORKING_DIRECTORY)
    set(_ARG_WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
  endif()

  set(VERILATOR_ARGS
    --binary
    --Mdir "${_ARG_OUTPUT_DIR}"
    --top-module "${_ARG_TESTBENCH}"
    --assert
    -CFLAGS -fcoroutines
    -DSIMULATION
  )

  set(SUPPRESSIONS
      TIMESCALEMOD
      WIDTHTRUNC
      SELRANGE
      WIDTHEXPAND
      CONSTRAINTIGN
      UNSIGNED
      INITIALDLY
      UNOPTFLAT
      ASCRANGE
      BLKANDNBLK
      CASEINCOMPLETE
      MULTIDRIVEN
      SHORTREAL
      MISINDENT
      WIDTHCONCAT
  )

  foreach(_suppr IN LISTS SUPPRESSIONS)
    list(APPEND VERILATOR_ARGS "-Wno-${_suppr}")
  endforeach()

  # Add search paths so Verilator can find the file lists
  list(APPEND VERILATOR_ARGS
    -y "${CMAKE_SOURCE_DIR}/runtime/rtl/hal/mock"
    -y "${CMAKE_SOURCE_DIR}/runtime/rtl"
    -y "${CMAKE_SOURCE_DIR}/runtime/rtl/sim"
  )

  # Add support files
  list(APPEND VERILATOR_ARGS
    -f "${CMAKE_SOURCE_DIR}/runtime/rtl/hal/mock/files.f"
    -f "${CMAKE_SOURCE_DIR}/runtime/rtl/files.f"
    -f "${CMAKE_SOURCE_DIR}/runtime/rtl/sim/files.f"
  )

  set(PRE_COMMANDS
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_ARG_OUTPUT_DIR}"
  )

  # Run Verilator to generate C++ from SystemVerilog
  add_custom_command(
    OUTPUT "${_ARG_OUTPUT_DIR}/V${_ARG_TESTBENCH}"
    ${PRE_COMMANDS}
    COMMAND ${CMAKE_COMMAND} -E echo "Running Verilator to generate C++ from SystemVerilog for target '${verilate_target}'"
    COMMAND ${VERILATOR_EXE} ${VERILATOR_ARGS} ${_ARG_SOURCES}
    DEPENDS ${_ARG_DEPENDS}
    WORKING_DIRECTORY "${_ARG_WORKING_DIRECTORY}"
    COMMENT "Verilating sources for target '${verilate_target}'"
    VERBATIM
  )

  # Create a phoney target that downstream can depend on
  add_custom_target(${verilate_target}
    DEPENDS "${_ARG_OUTPUT_DIR}/V${_ARG_TESTBENCH}"
  )

  add_test(NAME ${test_name}
    COMMAND "${_ARG_OUTPUT_DIR}/V${_ARG_TESTBENCH}"
    WORKING_DIRECTORY "${_ARG_OUTPUT_DIR}"
  )

endfunction()