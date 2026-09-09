# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# Tests that compile a Kanagawa source through the front-/middle-end with
# --skip-circt-lowering and run a python script to inspect the produced
# CIRCT MLIR. Useful for asserting on the shape of the EsiWrapper interface.
function(add_circt_mlir_test test_name)
  set(_one OUTPUT_PREFIX)
  set(_multi SOURCES OPTIONS TEST)
  cmake_parse_arguments(_ARG "" "${_one}" "${_multi}" ${ARGN})

  if(NOT _ARG_OUTPUT_PREFIX)
    set(_ARG_OUTPUT_PREFIX "test")
  endif()

  set(_outdir "${CMAKE_CURRENT_BINARY_DIR}/${test_name}")
  set(_fixture "interface_circt_${test_name}")

  add_test(
    NAME interface.circt.${test_name}.prepare
    COMMAND ${CMAKE_COMMAND} -E rm -rf "${_outdir}"
  )
  set_tests_properties(interface.circt.${test_name}.prepare PROPERTIES
    FIXTURES_SETUP "${_fixture}_prepared"
  )

  add_test(
    NAME interface.circt.${test_name}.mkdir
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_outdir}"
  )
  set_tests_properties(interface.circt.${test_name}.mkdir PROPERTIES
    FIXTURES_REQUIRED "${_fixture}_prepared"
    FIXTURES_SETUP    "${_fixture}_dir"
  )

  add_test(
    NAME interface.circt.${test_name}.compile
    COMMAND $<TARGET_FILE:kanagawa::exe>
              ${_ARG_OPTIONS}
              --output=${_outdir}/${_ARG_OUTPUT_PREFIX}
              ${_ARG_SOURCES}
  )
  set_tests_properties(interface.circt.${test_name}.compile PROPERTIES
    FIXTURES_REQUIRED "${_fixture}_dir"
    FIXTURES_SETUP    "${_fixture}_compiled"
  )

  set(_idx 0)
  foreach(_cmd IN LISTS _ARG_TEST)
    math(EXPR _idx "${_idx} + 1")
    separate_arguments(_cmd_list UNIX_COMMAND "${_cmd}")
    add_test(
      NAME interface.circt.${test_name}.test${_idx}
      COMMAND ${_cmd_list}
    )
    set_tests_properties(interface.circt.${test_name}.test${_idx} PROPERTIES
      FIXTURES_REQUIRED "${_fixture}_compiled"
    )
  endforeach()
endfunction()

add_circt_mlir_test(esi_wrapper_ports
  SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/esi_wrapper_ports.k
  OPTIONS
    --backend=sv
    --skip-circt-lowering
    --base-library=${CMAKE_SOURCE_DIR}/library/base.k
    --import-dir=${CMAKE_SOURCE_DIR}/library
    --place-iterations=1
  TEST
    "python3 ${CMAKE_CURRENT_SOURCE_DIR}/check_esi_wrapper_ports.py ${CMAKE_CURRENT_BINARY_DIR}/esi_wrapper_ports"
)

add_circt_mlir_test(named_type_aliases
  SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/named_type_aliases.k
  OPTIONS
    --backend=sv
    --skip-circt-lowering
    --base-library=${CMAKE_SOURCE_DIR}/library/base.k
    --import-dir=${CMAKE_SOURCE_DIR}/library
    --place-iterations=1
  TEST
    "python3 ${CMAKE_CURRENT_SOURCE_DIR}/check_named_type_aliases.py ${CMAKE_CURRENT_BINARY_DIR}/named_type_aliases"
)
