# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# This file defines logic tests. These are unit tests that verify that
# the Kanagawa generated RTL is logically correct.

# This is the set of tests that are run for all the different option configurations
set(LOGIC_TEST_SOURCES
    array_test_cases.k
    atomic_do.k
    auto_inline.k
    bit_constant_prop_test_cases.k
    call_arbiter.k
    callback_test_cases.k
    circt_test_cases_1.k
    common_subexpression.k
    compare_operand_width.k
    concat.k
    conditional_update.k
    const.k
    context_saver_depth.k
    context_saver_full.k
    data_prop_test_cases.k
    decompose_sub.k
    designated_initializer.k
    device_memory_tests.k
    dsp_test_cases.k
    fifo_depth.k
    function_call.k
    gft_constant_prop_repro.k
    initializer_list.k
    lambda_test_cases.k
    last.k
    long_class_name.k
    loop_test_cases_1.k
    loop_test_cases_2.k
    memory_test_cases_1.k
    memory_test_cases_2.k
    memory_test_cases_3.k
    memory_test_cases_4.k
    nested_atomic.k
    optimization_test_cases.k
    pipelined_test_cases.k
    remove_enqueue.k
    reorder.k
    rtl_naming.k
    scope_test_cases.k
    sign_extension.k
    single_thread.k
    stall.k
    static_if.k
    static_local.k
    string.k
    switch.k
    test_cases_1.k
    test_cases_10.k
    test_cases_11.k
    test_cases_12.k
    test_cases_13.k
    test_cases_14.k
    test_cases_15.k
    test_cases_16.k
    test_cases_17.k
    test_cases_18.k
    test_cases_19.k
    test_cases_2.k
    test_cases_20.k
    test_cases_21.k
    test_cases_22.k
    test_cases_23.k
    test_cases_24.k
    test_cases_25.k
    test_cases_26.k
    test_cases_27.k
    test_cases_28.k
    test_cases_29.k
    test_cases_3.k
    test_cases_30.k
    test_cases_31.k
    test_cases_32.k
    test_cases_33.k
    test_cases_35.k
    test_cases_36.k
    test_cases_37.k
    test_cases_38.k
    test_cases_39.k
    test_cases_4.k
    test_cases_40.k
    test_cases_41.k
    test_cases_42.k
    test_cases_43.k
    test_cases_44.k
    test_cases_45.k
    test_cases_46.k
    test_cases_47.k
    test_cases_48.k
    test_cases_49.k
    test_cases_5.k
    test_cases_50.k
    test_cases_51.k
    test_cases_6.k
    test_cases_7.k
    test_cases_8.k
    test_cases_9.k
    test_cases_slow.k
)

function(add_logic_test_suite suite_name)
  set(_opts)
  set(_one)
  set(_multi OPTIONS TEST_SOURCES)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT _ARG_TEST_SOURCES)
    set(_ARG_TEST_SOURCES ${LOGIC_TEST_SOURCES})
  endif()

  set(OPTIONS
    "--Wno-initializer-overrides"
    "--Wno-shadow"
    "--Wno-transaction-size"
    "--define=ECC_SUPPORT#true"
    "--using=COMPILE_PARAM_1#uint8"
    "--define=COMPILE_PARAM_2#9"
    "--define=tag#0"
    "--import-dir=${CMAKE_CURRENT_SOURCE_DIR}"
  )

  if (_ARG_OPTIONS)
    list(APPEND OPTIONS ${_ARG_OPTIONS})
  endif()

  foreach(_srcfile IN LISTS _ARG_TEST_SOURCES)
    get_filename_component(BASE_NAME "${_srcfile}" NAME_WE)
    set(TEST_NAME "${suite_name}.${BASE_NAME}")

    add_kanagawa_verilator_test(${TEST_NAME}
      SCOPE logic
      SOURCES ${_srcfile}
      OPTIONS ${OPTIONS}
      AGGREGATE_TARGET logic_tests
    )
  endforeach()

endfunction()

# Add a target to build all logic tests
add_custom_target(logic_tests)

# Add a target to run all logic tests
add_custom_target(run_logic_tests
    COMMAND ${CMAKE_CTEST_COMMAND} --test-dir ${CMAKE_CURRENT_BINARY_DIR} -R "^logic\\." --output-on-failure --verbose
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS logic_tests
    COMMENT "Run all logic tests"
    USES_TERMINAL
)

# All of the tests in the LOGIC_TEST_SOURCES list above are run for each of these test suite definitions
# A test suite is a set of test run against a set of compiler options.
add_logic_test_suite(default)
add_logic_test_suite(O0 OPTIONS "-O0")
add_logic_test_suite(O1 OPTIONS "-O1")
add_logic_test_suite(STALL2 OPTIONS "--debug" "--stall=2")
add_logic_test_suite(MEM2REG OPTIONS "--mem-to-reg=100000")
add_logic_test_suite(MEM_TO_ARRAY OPTIONS "--force-mem-to-array=64")
add_logic_test_suite(RMWR1 OPTIONS "--rmw-memory-read-delay=1")