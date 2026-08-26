# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# CLI-level tests that exercise specific kanagawa command-line options
# by inspecting the files produced by the compiler.

# Helper function for adding a CLI-level CTest. The test invokes the Kanagawa
# compiler with the supplied OPTIONS and then runs the supplied verification
# command(s) (TEST). Each verification command is its own CTest, chained
# behind the compile step via CTest fixtures so that they only run after a
# successful compile. Each command's exit code determines pass/fail (0 ==
# pass, non-zero == fail).
#
# Usage:
#   add_cli_test(<test_name>
#     SOURCES <src1> [<src2> ...]
#     [OPTIONS <opt1> <opt2> ...]
#     [OUTPUT_PREFIX <prefix>]    # default: test
#     [TEST <command-with-args> ...]
#   )
#
# A separate output directory is created for each test under the current
# binary dir, and the compiler is invoked with --output=<outdir>/<prefix>.
# CTest names are formed by prefixing the test name with 'cli.' (e.g.
# cli.<test_name>.compile, cli.<test_name>.test1, cli.<test_name>.test2).
function(add_cli_test test_name)
  set(_one OUTPUT_PREFIX)
  set(_multi SOURCES OPTIONS TEST)
  cmake_parse_arguments(_ARG "" "${_one}" "${_multi}" ${ARGN})

  if(NOT test_name)
    message(FATAL_ERROR "add_cli_test: missing <test_name>.")
  endif()
  if(NOT _ARG_SOURCES)
    message(FATAL_ERROR "add_cli_test(${test_name}): SOURCES is required.")
  endif()
  if(NOT _ARG_OUTPUT_PREFIX)
    set(_ARG_OUTPUT_PREFIX "test")
  endif()

  set(_outdir "${CMAKE_CURRENT_BINARY_DIR}/${test_name}")
  set(_fixture "cli_${test_name}")

  # Compile step: clean the output dir, recreate it, then run the compiler.
  # add_test only allows one COMMAND, so drive the prep + compile via
  # `cmake -E env --` which simply forwards to the kanagawa exe after the
  # output dir exists. The dir prep is done as a separate (cheap) test that
  # the compile depends on via fixtures.
  add_test(
    NAME cli.${test_name}.prepare
    COMMAND ${CMAKE_COMMAND} -E rm -rf "${_outdir}"
  )
  set_tests_properties(cli.${test_name}.prepare PROPERTIES
    FIXTURES_SETUP "${_fixture}_prepared"
  )

  add_test(
    NAME cli.${test_name}.mkdir
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_outdir}"
  )
  set_tests_properties(cli.${test_name}.mkdir PROPERTIES
    FIXTURES_REQUIRED "${_fixture}_prepared"
    FIXTURES_SETUP    "${_fixture}_dir"
  )

  add_test(
    NAME cli.${test_name}.compile
    COMMAND $<TARGET_FILE:kanagawa::exe>
              ${_ARG_OPTIONS}
              --output=${_outdir}/${_ARG_OUTPUT_PREFIX}
              ${_ARG_SOURCES}
  )
  set_tests_properties(cli.${test_name}.compile PROPERTIES
    FIXTURES_REQUIRED "${_fixture}_dir"
    FIXTURES_SETUP    "${_fixture}_compiled"
  )

  # Each TEST command runs as its own ctest, gated on a successful compile.
  set(_idx 0)
  foreach(_cmd IN LISTS _ARG_TEST)
    math(EXPR _idx "${_idx} + 1")
    # Split the command string on spaces into a list so the first token is
    # the program and the remainder are arguments.
    separate_arguments(_cmd_list UNIX_COMMAND "${_cmd}")
    add_test(
      NAME cli.${test_name}.test${_idx}
      COMMAND ${_cmd_list}
    )
    set_tests_properties(cli.${test_name}.test${_idx} PROPERTIES
      FIXTURES_REQUIRED "${_fixture}_compiled"
    )
  endforeach()
endfunction()


add_cli_test(skip_circt_lowering
  SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/skip_circt_lowering.k
  OPTIONS
    --backend=sv
    --skip-circt-lowering
    --base-library=${CMAKE_SOURCE_DIR}/library/mini-base.k
    --import-dir=${CMAKE_SOURCE_DIR}/library
    --place-iterations=1
  TEST
    "python3 ${CMAKE_CURRENT_SOURCE_DIR}/check_skip_circt_lowering.py ${CMAKE_CURRENT_BINARY_DIR}/skip_circt_lowering"
)

# `list-deps` resolves imports without running the frontend or codegen, so it
# does not fit add_cli_test's compile-and-inspect shape.
add_golden_test(cli.list_deps
  COMMAND $<TARGET_FILE:kanagawa::exe> list-deps
            --base-library=${CMAKE_SOURCE_DIR}/library/mini-base.k
            --import-dir=${CMAKE_SOURCE_DIR}/library
            --file-list=${CMAKE_CURRENT_BINARY_DIR}/list_deps.txt
            ${CMAKE_CURRENT_SOURCE_DIR}/list_deps.k
  ACTUAL ${CMAKE_CURRENT_BINARY_DIR}/list_deps.txt
  GOLDEN list_deps.golden.in
)
