# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# CLI-level tests that exercise specific kanagawa command-line options
# by inspecting the files produced by the compiler.

# The verifier scripts are in Python; resolve the interpreter once via
# CMake's bundled finder (CIRCT already calls `find_package(Python3)`,
# which sets Python3_EXECUTABLE on every platform). Falling back to a bare
# `python3` only works on POSIX-like systems where it is on PATH.
if(NOT Python3_EXECUTABLE)
  find_package(Python3 COMPONENTS Interpreter REQUIRED)
endif()
set(_cli_python "${Python3_EXECUTABLE}")

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
#     [SUBCOMMAND <name>]         # e.g. list-deps; default: omit (auto compile)
#     [NO_AUTO_OUTPUT]            # do not pass --output=<outdir>/<prefix>
#     [TEST <command-with-args> ...]
#   )
#
# A separate output directory is created for each test under the current
# binary dir, and (unless NO_AUTO_OUTPUT is set) the compiler is invoked with
# --output=<outdir>/<prefix>. CTest names are formed by prefixing the test
# name with 'cli.' (e.g. cli.<test_name>.compile, cli.<test_name>.test1,
# cli.<test_name>.test2). Test commands can reference @OUTDIR@ to substitute
# the absolute path of the per-test output directory.
function(add_cli_test test_name)
  set(_opt NO_AUTO_OUTPUT)
  set(_one OUTPUT_PREFIX SUBCOMMAND)
  set(_multi SOURCES OPTIONS TEST)
  cmake_parse_arguments(_ARG "${_opt}" "${_one}" "${_multi}" ${ARGN})

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

  # Build the kanagawa command line, optionally prefixed by a sub-command
  # and optionally suppressing the auto --output flag. @OUTDIR@ in any
  # OPTIONS or SOURCES entry is substituted with the per-test output dir.
  set(_compile_cmd $<TARGET_FILE:kanagawa::exe>)
  if(_ARG_SUBCOMMAND)
    list(APPEND _compile_cmd ${_ARG_SUBCOMMAND})
  endif()
  set(_subst_options "")
  foreach(_opt IN LISTS _ARG_OPTIONS)
    string(REPLACE "@OUTDIR@" "${_outdir}" _opt "${_opt}")
    list(APPEND _subst_options "${_opt}")
  endforeach()
  list(APPEND _compile_cmd ${_subst_options})
  if(NOT _ARG_NO_AUTO_OUTPUT)
    list(APPEND _compile_cmd --output=${_outdir}/${_ARG_OUTPUT_PREFIX})
  endif()
  set(_subst_sources "")
  foreach(_src IN LISTS _ARG_SOURCES)
    string(REPLACE "@OUTDIR@" "${_outdir}" _src "${_src}")
    list(APPEND _subst_sources "${_src}")
  endforeach()
  list(APPEND _compile_cmd ${_subst_sources})

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
    COMMAND ${_compile_cmd}
  )
  set_tests_properties(cli.${test_name}.compile PROPERTIES
    FIXTURES_REQUIRED "${_fixture}_dir"
    FIXTURES_SETUP    "${_fixture}_compiled"
  )

  # Each TEST command runs as its own ctest, gated on a successful compile.
  set(_idx 0)
  foreach(_cmd IN LISTS _ARG_TEST)
    math(EXPR _idx "${_idx} + 1")
    # @OUTDIR@ is a convenience placeholder for the per-test output dir.
    string(REPLACE "@OUTDIR@" "${_outdir}" _cmd "${_cmd}")
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
    "${_cli_python} ${CMAKE_CURRENT_SOURCE_DIR}/check_skip_circt_lowering.py ${CMAKE_CURRENT_BINARY_DIR}/skip_circt_lowering"
)

# `list-deps` sub-command: parse + import resolution only, fast enough to run
# during cmake configure for staleness detection. The compile step here is
# the list-deps invocation itself; the verifier asserts the manifest is
# present, well-formed, and that no codegen artifacts were produced.
add_cli_test(list_deps_plain
  SUBCOMMAND list-deps
  NO_AUTO_OUTPUT
  SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/list_deps.k
  OPTIONS
    --base-library=${CMAKE_SOURCE_DIR}/library/mini-base.k
    --import-dir=${CMAKE_SOURCE_DIR}/library
    --file-list=@OUTDIR@/deps.txt
  TEST
    "${_cli_python} ${CMAKE_CURRENT_SOURCE_DIR}/check_list_deps.py --source ${CMAKE_CURRENT_SOURCE_DIR}/list_deps.k --plain @OUTDIR@/deps.txt --no-codegen-dir @OUTDIR@"
)

