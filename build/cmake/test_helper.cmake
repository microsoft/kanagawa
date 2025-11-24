# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

#
# This file contains CMake macros to facilitate creating unit tests
#


# Helper function for running the Kanagawa compiler to generate RTL
#
# Usage:
#   add_kanagawa(<target>
#     [OUTPUT_DIR <dir>]
#     [OUTPUT_PREFIX <prefix>]
#     [CLEAN_OUTPUT_DIR]
#     [WORKING_DIRECTORY <dir>]
#     SOURCES <src1> [<src2> ...]
#     [OPTIONS <opt1> <opt2> ...]
#   )
#
# Downstream can depend on <target> to ensure the HLS step has run:
#   add_dependencies(my_rtl_consumer <target>)
#
function(add_kanagawa target)
  set(_opts CLEAN_OUTPUT_DIR)
  set(_one  OUTPUT_DIR WORKING_DIRECTORY OUTPUT_PREFIX)
  set(_multi SOURCES OPTIONS)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if(NOT target)
    message(FATAL_ERROR "add_kanagawa: missing <target> name as first argument.")
  endif()

  # Default OUTPUT_DIR if not provided
  if(NOT _ARG_OUTPUT_DIR)
    set(_ARG_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}")
  endif()

  if(NOT _ARG_SOURCES)
    message(FATAL_ERROR "add_kanagawa(${target}): SOURCES is required.")
  endif()

  if(NOT _ARG_WORKING_DIRECTORY)
    set(_ARG_WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
  endif()

  # Ensure the output directory exists at configure time
  file(MAKE_DIRECTORY "${_ARG_OUTPUT_DIR}")

  # Normalize paths for reproducible content
  file(TO_CMAKE_PATH "${_ARG_OUTPUT_DIR}" _out_dir_norm)
  file(TO_CMAKE_PATH "${_ARG_WORKING_DIRECTORY}" _work_dir_norm)

  # Stable inputs summary (lives in binary dir; triggers rebuilds when changed)
  set(_inputs_file "${CMAKE_CURRENT_BINARY_DIR}/${target}.kanagawa.inputs")
  set(_inputs_text "OUTPUT_DIR=${_out_dir_norm}\nWORKING_DIRECTORY=${_work_dir_norm}\nSOURCES=\n")
  foreach(_s IN LISTS _ARG_SOURCES)
    file(TO_CMAKE_PATH "${_s}" _s_norm)
    string(APPEND _inputs_text "  ${_s_norm}\n")
  endforeach()
  string(APPEND _inputs_text "OPTIONS=\n")
  foreach(_o IN LISTS _ARG_OPTIONS)
    string(APPEND _inputs_text "  ${_o}\n")
  endforeach()
  file(GENERATE OUTPUT "${_inputs_file}" CONTENT "${_inputs_text}")

  # These files that live inside OUTPUT_DIR and are used to help CMake / Ninja understand when something changed
  set(_args_in_outdir "${_ARG_OUTPUT_DIR}/${target}.kanagawa.args")
  set(_stamp         "${_ARG_OUTPUT_DIR}/${target}.kanagawa.stamp")

  # Pre-commands (optional clean, ensure dir, copy inputs -> args in OUTPUT_DIR)
  set(_pre_commands)
  if(_ARG_CLEAN_OUTPUT_DIR)
    list(APPEND _pre_commands
      COMMAND ${CMAKE_COMMAND} -E rm -f "${_ARG_OUTPUT_DIR}/*"
    )
  endif()
  list(APPEND _pre_commands
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_ARG_OUTPUT_DIR}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${_inputs_file}" "${_args_in_outdir}"
  )

  set(KANAGAWA_OUTPUT "${_ARG_OUTPUT_DIR}/${_ARG_OUTPUT_PREFIX}")

  add_custom_command(
    OUTPUT "${_stamp}"
    BYPRODUCTS
      "${_args_in_outdir}"
    ${_pre_commands}
    COMMAND "$<TARGET_FILE:kanagawa::exe>"
            ${_ARG_OPTIONS}
            --output "${KANAGAWA_OUTPUT}"
            ${_ARG_SOURCES}
    COMMAND ${CMAKE_COMMAND} -E echo "Kanagawa HLS completed for '${target}'" > "${_stamp}"
    DEPENDS
      ${KANAGAWA_RUNTIME_TARGETS}
      ${_ARG_SOURCES}
      "${_inputs_file}"
    WORKING_DIRECTORY "${_ARG_WORKING_DIRECTORY}"
    VERBATIM
    COMMAND_EXPAND_LISTS
  )

  # Phony target others can depend on
  add_custom_target(${target} DEPENDS "${_stamp}")
  add_dependencies(${target} kanagawa_runtime)

  # If heavy job pool is configured, assign it to this target (Kanagawa compilation can be memory-intensive)
  if(KANAGAWA_HEAVY_PARALLEL_JOBS)
    set_property(TARGET ${target} PROPERTY JOB_POOL_COMPILE heavy_job_pool)
  endif()

  # Expose OUTPUT_DIR to caller
  set(${target}_OUTPUT_DIR "${_ARG_OUTPUT_DIR}" PARENT_SCOPE)

  # Expose stamp file path to caller
  set(${target}_STAMP_FILE "${_stamp}" PARENT_SCOPE)

  # Optional: include OUTPUT_DIR in 'clean'
  set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${_ARG_OUTPUT_DIR}")
endfunction()

# Helper function for running Verilator to compile SystemVerilog into
# a C++ simulation executable. Note that the built-in support for CMake that
# is part of Verilator is not used here, as it does the verilate step during
# the CMake generation/configure step, which is not desirable.
#
# Usage:
#   add_verilator(<verilate_target>
#     [TEST_NAME test_name]
#     [OUTPUT_DIR <dir>]
#     [WORKING_DIRECTORY <dir>]
#     [SIM_EXE_OUT_VAR <variable-name>]
#     [CLEAN_BYPRODUCTS]
#     [DEPENDS <target-or-file> ...]
#     TESTBENCH_MODULE <testbench-module-name>
#     SOURCES <src1> [<src2> ...]
#     [OPTIONS <opt1> <opt2> ...]
#   )
#
# Downstream can depend on <target> to ensure the that Verilation has run.
# The target will produce a C++ executable that can be run to execute the
# simulation. The path to the executable is available via the target property
# VERILATOR_OUTPUT.
#
function(add_verilator verilate_target)
  set(_opts CLEAN_BYPRODUCTS)
  set(_one TEST_NAME OUTPUT_DIR WORKING_DIRECTORY TESTBENCH_MODULE SIM_EXE_OUT_VAR)
  set(_multi SOURCES OPTIONS DEPENDS)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT VERILATOR_EXE)
    message(FATAL_ERROR "add_verilator: VERILATOR_EXE is not set. Please ensure Verilator is installed and VERILATOR_EXE is set to the path to the Verilator executable.")
  endif()

  if(NOT verilate_target)
    message(FATAL_ERROR "add_verilator: missing <verilate_target> name as first argument.")
  endif()

  if(NOT _ARG_TESTBENCH_MODULE)
    message(FATAL_ERROR "add_verilator(${verilate_target}): _ARG_TESTBENCH_MODULE is required.")
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
    --top-module "${_ARG_TESTBENCH_MODULE}"
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
    -f "${CMAKE_SOURCE_DIR}/runtime/rtl/files.f"
    -f "${CMAKE_SOURCE_DIR}/runtime/rtl/sim/files.f"
    -f "${CMAKE_SOURCE_DIR}/runtime/rtl/hal/mock/files.f"
  )

  set(PRE_COMMANDS
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_ARG_OUTPUT_DIR}"
  )

  set(SIMULATION_EXE "${_ARG_OUTPUT_DIR}/V${_ARG_TESTBENCH_MODULE}")

  # Optional post-build cleanup command
  set(POST_COMMANDS)
  if(_ARG_CLEAN_BYPRODUCTS)
    list(APPEND POST_COMMANDS
      COMMAND ${CMAKE_COMMAND} -E echo "Cleaning Verilator byproduct files for target '${verilate_target}'"
      COMMAND ${CMAKE_COMMAND} -E rm -f "${_ARG_OUTPUT_DIR}/*.cpp"
      COMMAND ${CMAKE_COMMAND} -E rm -f "${_ARG_OUTPUT_DIR}/*.h"
      COMMAND ${CMAKE_COMMAND} -E rm -f "${_ARG_OUTPUT_DIR}/*.o"
      COMMAND ${CMAKE_COMMAND} -E rm -f "${_ARG_OUTPUT_DIR}/*.a"
      COMMAND ${CMAKE_COMMAND} -E rm -f "${_ARG_OUTPUT_DIR}/*.d"
      COMMAND ${CMAKE_COMMAND} -E rm -f "${_ARG_OUTPUT_DIR}/*.mk"
      COMMAND ${CMAKE_COMMAND} -E rm -f "${_ARG_OUTPUT_DIR}/*.dat"
    )
  endif()

  # Run Verilator to generate C++ from SystemVerilog
  add_custom_command(
    OUTPUT ${SIMULATION_EXE}
    ${PRE_COMMANDS}
    COMMAND ${CMAKE_COMMAND} -E echo "Running Verilator to generate C++ from SystemVerilog for target '${verilate_target}'"
    COMMAND ${VERILATOR_EXE} ${VERILATOR_ARGS} ${_ARG_SOURCES}
    ${POST_COMMANDS}
    DEPENDS ${_ARG_DEPENDS}
    WORKING_DIRECTORY "${_ARG_WORKING_DIRECTORY}"
    COMMENT "Verilating sources for target '${verilate_target}'"
    VERBATIM
  )

  # Optionally pass path to Verilator-generated simulation executable to caller
  if (_ARG_SIM_EXE_OUT_VAR)
    set(${_ARG_SIM_EXE_OUT_VAR} "${SIMULATION_EXE}" PARENT_SCOPE)
  endif()

  # Create a phoney target that downstream can depend on
  add_custom_target(${verilate_target}
    DEPENDS ${SIMULATION_EXE}
  )

  # If heavy job pool is configured, assign it to this target (Verilator can be memory-intensive)
  if(KANAGAWA_HEAVY_PARALLEL_JOBS)
    set_property(TARGET ${verilate_target} PROPERTY JOB_POOL_COMPILE heavy_job_pool)
  endif()

  if (_ARG_TEST_NAME)
    add_test(NAME ${_ARG_TEST_NAME}
      COMMAND ${SIMULATION_EXE}
      WORKING_DIRECTORY "${_ARG_OUTPUT_DIR}"
    )
  endif()

endfunction()

# Helper function that uses Kanagawa to compile sources into RTL, runs
# Verilator to build a simulation executable from that RTL, and creates
# a ctest that will run that executable/simulation.
# Usage:
#   add_kanagawa_verilator_test(<name>
#     [SCOPE <test-scope>]
#     SOURCES <kanagawa_source1> <kanagawa_source2> ...
#     [OPTIONS <opt1> <opt2> ...]
#     [GENERATED_RTL <rtl-file>]
#     [EXTRA_RTL <rtl-file]
#     [TESTBENCH <testbench-file>]
#     [TESTBENCH_MODULE <systemverilog-module-name>]
#     [OUTPUT_DIR <dir>]
#     [AGGREGATE_TARGET <aggregate-target-name>]
#     [CLEAN_VERILATOR_BYPRODUCTS]
#
#   SCOPE (optional)
#     The type/scope of the test, for example "library", "syntax", "logic", etc.
#     this value will be used to scope the test name and targets so that tests
#     of different types with the same name do not cause naming conflicts.
#     For example, a test with a test_name of "foo" and a "test_scope" of library,
#     will result in a CTest name of "library.foo" and a build target of
#     "library_test.foo".
#
#   name
#     A name used in the name of a target that will be created. Along with <test_scope>,
#     this will be used to name the build target, ctest name, and the output directory.
#
#   NO_CTEST
#     If set, no ctest will be created.
#
#   SOURCES
#     One or more Kanagawa source files.
#
#   OPTIONS (optional)
#     Options to pass to the Kanagawa compiler.
#
#   GENERATED_RTL (optional)
#     Allows you to specify the names of the RTL files generated by the Kanagawa
#     compiler. If not specified, the default value that is generated for the
#     test runner will be used (_test_runner_main.sv). Note that these files
#     will be relative to the output directory where the Kanagawa compiler writes
#     the generated RTL.
#
#   EXTRA_RTL
#     Additional RTL files to add to the simulation build. Unlike GENERATED_RTL,
#     these files are not relative to any directory so the caller must provide
#     an absolute path.
#
#   TESTBENCH (optional)
#     A SystemVerilog file containing a test bench to be added to
#     the Verilator simulation. If not provided, a default testbench that works
#     with the Kanagawa test runner will be provided (library/test/testbench.sv).
#     Note that this should be an absolute, not relative path.
#
#   TESTBENCH_MODULE (optional)
#     The name of the SystemVerilog testbench module. This is the top module
#     in the Verilator simulation. Default: Testbench.
#
#   CLEAN_VERILATOR_BYPRODUCTS (optional)
#     If set, Verilator intermediate files (C++ sources, object files, etc.)
#     will be deleted after the simulation executable is built, saving disk space.
#
#   AGGREGATE_TARGET (optional)
#     If provided, the test will be added as a dependency to the target with the
#     specified name. This is useful to have a target to run all tests of a
#     particular type.
#
#   Notes:
#     The generated RTL will be written to ${CMAKE_CURRENT_BINARY_DIR}/${target}
#     The Verilator simulation will be placed in a subfolder named 'verilator'
#
function(add_kanagawa_verilator_test name)
  set(_opts NO_CTEST CLEAN_VERILATOR_BYPRODUCTS)
  set(_one SCOPE TESTBENCH TESTBENCH_MODULE AGGREGATE_TARGET)
  set(_multi OPTIONS SOURCES GENERATED_RTL EXTRA_RTL)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT name)
    message(FATAL_ERROR "add_kanagawa_verilator_test: missing <name> as first argument.")
  endif()

  if (NOT _ARG_SOURCES)
    message(FATAL_ERROR "add_kanagawa_verilator_test: missing SOURCES.")
  endif()

  if (_ARG_SCOPE)
    set(SCOPED_TEST_NAME "${_ARG_SCOPE}.${name}")
    set(TARGET_NAME "${_ARG_SCOPE}_test.${name}")
  else()
    set(SCOPED_TEST_NAME "${name}")
    set(TARGET_NAME "${name}")
  endif()

  if (NO_CTEST)
    set(SCOPED_TEST_NAME)
  endif()

  set(OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${name}")

  set(KANAGAWA_OPTIONS
    "--Wall"
    "--Werror"
    "--check-narrowing-assignment"
    "--parse-docs"
    "--max-block-ram=10000000"
    "--frequency=250"
    "--reset-cycles=5"
    "--register-ratio=3"
    "--backend=sv"
    "--import-dir=${CMAKE_SOURCE_DIR}/library"
    "--define=verbose#true"
  )

  if (_ARG_OPTIONS)
    list(APPEND KANAGAWA_OPTIONS ${_ARG_OPTIONS})
  endif()

  add_custom_target(${TARGET_NAME}
    COMMENT "Building test: ${TARGET_NAME}"
  )

  set(KANAGAWA_TARGET_NAME "${TARGET_NAME}.kanagawa")

  # Add Kanagawa RTL generation
  add_kanagawa(${KANAGAWA_TARGET_NAME}
    CLEAN_OUTPUT_DIR
    OUTPUT_DIR ${OUTPUT_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    SOURCES ${_ARG_SOURCES}
    OPTIONS ${KANAGAWA_OPTIONS}
  )

  add_dependencies(${TARGET_NAME} ${KANAGAWA_TARGET_NAME})

  # Add verilate (Verilator build) if Verilator is available
  if (VERILATOR_EXE)
    # By convention Verilator generated executables are named this way Vxxxx
    set(VERILATOR_TARGET_NAME "${TARGET_NAME}.verilator")

    if (_ARG_TESTBENCH_MODULE)
      set(TESTBENCH_MODULE ${_ARG_TESTBENCH_MODULE})
    else()
      set(TESTBENCH_MODULE "Testbench")
    endif()

    if (_ARG_GENERATED_RTL)
      set(RTL_FILES)
      foreach(_rtl IN LISTS _ARG_GENERATED_RTL)
        # Prepend ${OUTPUT_DIR} to each file
        list(APPEND RTL_FILES "${OUTPUT_DIR}/${_rtl}")
      endforeach()
    else()
      set(RTL_FILES
        "${OUTPUT_DIR}/_test_runner_main.sv"
      )
    endif()

    if (_ARG_EXTRA_RTL)
      list(APPEND RTL_FILES ${_ARG_EXTRA_RTL})
    endif()

    if (_ARG_TESTBENCH)
      list(APPEND RTL_FILES ${_ARG_TESTBENCH})
    else()
      list(APPEND RTL_FILES "${CMAKE_SOURCE_DIR}/library/test/testbench.sv")
    endif()

    set(VERILATOR_OUTPUT_DIR
      "${OUTPUT_DIR}/verilator"
    )

    set(SIM_EXE)

    set(VERILATOR_OPTIONS)
    if(_ARG_CLEAN_VERILATOR_BYPRODUCTS)
      list(APPEND VERILATOR_OPTIONS CLEAN_BYPRODUCTS)
    endif()

    add_verilator(${VERILATOR_TARGET_NAME}
      TEST_NAME ${SCOPED_TEST_NAME}
      SIM_EXE_OUT_VAR "SIM_EXE"
      OUTPUT_DIR ${VERILATOR_OUTPUT_DIR}
      WORKING_DIRECTORY ${VERILATOR_OUTPUT_DIR}
      TESTBENCH_MODULE ${TESTBENCH_MODULE}
      SOURCES ${RTL_FILES}
      DEPENDS ${${KANAGAWA_TARGET_NAME}_STAMP_FILE}
      ${VERILATOR_OPTIONS}
    )

    set_target_properties(${TARGET_NAME} PROPERTIES
      SIM_EXE "${SIM_EXE}"
    )

    add_dependencies(${TARGET_NAME} ${VERILATOR_TARGET_NAME})
  else()
    message(WARNING "Verilator executable not found. Skipping generation of Verilator simulation for test ${SCOPED_TEST_NAME}")
  endif()

  if (_ARG_AGGREGATE_TARGET)
    add_dependencies(${_ARG_AGGREGATE_TARGET} ${TARGET_NAME})
  endif()
endfunction()

