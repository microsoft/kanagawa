# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# This file defines a set of unit tests for the RTL files that
# support the Kangawa generated RTL.

# Helper function for creating Verilator tests for the runtime RTL modules.
#
# Usage:
#   add_runtime_rtl_test(<test_name> <testbench> <testbench_module>
#     [OPTIONS <opt1> <opt2> ...]
#   )
#
function(add_runtime_rtl_test test_name testbench testbench_module)
  set(_opts)
  set(_one)
  set(_multi OPTIONS)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT test_name)
    message(FATAL_ERROR "add_runtime_rtl_test: missing <test_name> as first argument.")
  endif()

  if (NOT testbench)
    message(FATAL_ERROR "add_runtime_rtl_test: missing <testbench> as second argument.")
  endif()

  if (NOT testbench_module)
    message(FATAL_ERROR "add_runtime_rtl_test: missing <testbench_module> as third argument.")
  endif()

  set(TARGET_NAME "runtime_rtl_test.${test_name}")
  set(SCOPED_TEST_NAME "runtime_rtl.${test_name}")

  if (VERILATOR_EXE)
      add_verilator(${TARGET_NAME}
        TEST_NAME ${SCOPED_TEST_NAME}
        OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${test_name}"
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${test_name}"
        TESTBENCH_MODULE ${testbench_module}
        SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/${testbench}"
        DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${testbench}"
        OPTIONS ${_ARG_OPTIONS}
      )

      add_dependencies(runtime_rtl_tests ${TARGET_NAME})
  else()
    message(WARNING "Verilator executable not found. Skipping generation of Verilator simulation for test ${SCOPED_TEST_NAME}")
  endif()
endfunction()

add_custom_target(runtime_rtl_tests)

# Add a target to run all runtime rtl tests
add_custom_target(run_runtime_rtl_tests
    COMMAND ${CMAKE_CTEST_COMMAND} --test-dir ${CMAKE_CURRENT_BINARY_DIR} -R "^runtime_rtl\\." --output-on-failure --verbose
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS runtime_rtl_tests
    COMMENT "Run all runtime RTL tests"
    USES_TERMINAL
)

add_runtime_rtl_test(hal_dsp_mock_umul27 hal_dsp_mock_tb.sv KanagawaHALDSP_mock_umul27_tb)

add_runtime_rtl_test(hal_dsp_mock_imul27 hal_dsp_mock_tb.sv KanagawaHALDSP_mock_imul27_tb)

add_runtime_rtl_test(hal_dsp_mock_fmul32 hal_dsp_mock_tb.sv KanagawaHALDSP_mock_fmul32_tb)

add_runtime_rtl_test(hal_dsp_mock_fadd32 hal_dsp_mock_tb.sv KanagawaHALDSP_mock_fadd32_tb)

add_runtime_rtl_test(hal_dsp_mock_fsub32 hal_dsp_mock_tb.sv KanagawaHALDSP_mock_fsub32_tb)

add_runtime_rtl_test(hal_dsp_mock_fmac32 hal_dsp_mock_tb.sv KanagawaHALDSP_mock_fmac32_tb)

add_runtime_rtl_test(hal_dsp_mock_fmad32 hal_dsp_mock_tb.sv KanagawaHALDSP_mock_fmad32_tb)

add_runtime_rtl_test(string_table string_table_tb.sv KanagawaStringTable_tb)

# Crashes in pipeline
#
# add_runtime_rtl_test(logic_ram logic_ram_tb.sv KanagawaLogicRam_tb)

add_runtime_rtl_test(fixed_delay_fifo_delay_2 fixed_delay_fifo_tb.sv KanagawaFixedDelayFifo_tb
    OPTIONS
        "-GTB_WIDTH=32"
        "-GTB_DELAY=2"
)

add_runtime_rtl_test(fixed_delay_fifo_delay_3 fixed_delay_fifo_tb.sv KanagawaFixedDelayFifo_tb
    OPTIONS
        "-GTB_WIDTH=32"
        "-GTB_DELAY=3"
)

add_runtime_rtl_test(fixed_delay_fifo_delay_4 fixed_delay_fifo_tb.sv KanagawaFixedDelayFifo_tb
    OPTIONS
        "-GTB_WIDTH=32"
        "-GTB_DELAY=4"
)

add_runtime_rtl_test(fixed_delay_fifo_delay_5 fixed_delay_fifo_tb.sv KanagawaFixedDelayFifo_tb
    OPTIONS
        "-GTB_WIDTH=32"
        "-GTB_DELAY=5"
)

add_runtime_rtl_test(fifo_ptrs fifo_ptrs_tb.sv KanagawaFifoPtrs_tb)

add_runtime_rtl_test(fifo_ptrs_ex_pow2 fifo_ptrs_ex_tb.sv KanagawaFifoPtrsEx_tb
    OPTIONS
        "-GTB_DEPTH=32"
        "-GTB_ALMOST_FULL_MARGIN=4"
        "-GTB_ALMOST_EMPTY_MARGIN=3"
)

add_runtime_rtl_test(fifo_ptrs_ex_non_pow2 fifo_ptrs_ex_tb.sv KanagawaFifoPtrsEx_tb
    OPTIONS
        "-GTB_DEPTH=27"
        "-GTB_ALMOST_FULL_MARGIN=4"
        "-GTB_ALMOST_EMPTY_MARGIN=3"
)

add_runtime_rtl_test(show_ahead_register_fifo_directed_nonpow2_mux show_ahead_register_fifo_directed_tb.sv KanagawaShowAheadRegisterFifo_Directed_tb
    OPTIONS
        "-GTB_DEPTH=7"
        "-GTB_MUX_ON_READ=1"
)

add_runtime_rtl_test(show_ahead_register_fifo_directed_pow2_mux show_ahead_register_fifo_directed_tb.sv KanagawaShowAheadRegisterFifo_Directed_tb
    OPTIONS
        "-GTB_DEPTH=8"
        "-GTB_MUX_ON_READ=1"
)

add_runtime_rtl_test(show_ahead_register_fifo_directed_nonpow2_nomux show_ahead_register_fifo_directed_tb.sv KanagawaShowAheadRegisterFifo_Directed_tb
    OPTIONS
        "-GTB_DEPTH=7"
        "-GTB_MUX_ON_READ=0"
)

add_runtime_rtl_test(show_ahead_register_fifo_directed_pow2_nomux show_ahead_register_fifo_directed_tb.sv KanagawaShowAheadRegisterFifo_Directed_tb
    OPTIONS
        "-GTB_DEPTH=8"
        "-GTB_MUX_ON_READ=0"
)

add_runtime_rtl_test(sim_random_latency_chain_passthrough sim_random_latency_chain_tb.sv KanagawaSimRandomLatencyChain_tb
    OPTIONS
        "-GTB_DEPTH=0"
        "-GTB_MAX_DEPTH=0"
)

add_runtime_rtl_test(sim_random_latency_chain_fixed sim_random_latency_chain_tb.sv KanagawaSimRandomLatencyChain_tb
    OPTIONS
        "-GTB_DEPTH=2"
        "-GTB_MAX_DEPTH=2"
)

add_runtime_rtl_test(sim_random_latency_chain_random_0_8 sim_random_latency_chain_tb.sv KanagawaSimRandomLatencyChain_tb
    OPTIONS
        "-GTB_DEPTH=0"
        "-GTB_MAX_DEPTH=8"
)

add_runtime_rtl_test(sim_random_latency_chain_random_2_10 sim_random_latency_chain_tb.sv KanagawaSimRandomLatencyChain_tb
    OPTIONS
        "-GTB_DEPTH=2"
        "-GTB_MAX_DEPTH=10"
)

add_runtime_rtl_test(fifo_debug_single_clock fifo_debug_tb.sv KanagawaSingleClockFifoDebug_tb)

add_runtime_rtl_test(fifo_debug_dual_clock fifo_debug_tb.sv KanagawaDualClockFifoDebug_tb)

add_runtime_rtl_test(write_delay_fifo_directed write_delay_fifo_directed_tb.sv KanagawaWriteDelayFifoDirected_tb)

add_runtime_rtl_test(write_delay_fifo_ae4 write_delay_fifo_tb.sv KanagawaWriteDelayFifo_tb
    OPTIONS
        "-GALMOSTEMPTY_VAL=4"
)

add_runtime_rtl_test(write_delay_fifo_ae0 write_delay_fifo_tb.sv KanagawaWriteDelayFifo_tb
    OPTIONS
        "-GALMOSTEMPTY_VAL=0"
)

add_runtime_rtl_test(hal_simple_dual_port_ram_d40_a8_lr0_or1 hal_simple_dual_port_ram_tb.sv KanagawaHALSimpleDualPortRAM_tb
    OPTIONS
        "-GTB_DATA_WIDTH=40"
        "-GTB_ADDR_WIDTH=8"
        "-GTB_USE_LUTRAM=0"
        "-GTB_USE_BRAM=1"
        "-GTB_USE_OUTPUT_REG=1"
        "-GTB_DEVICE_FAMILY='\"mock\""
)

add_runtime_rtl_test(hal_simple_dual_port_ram_d40_a8_lr0_or0 hal_simple_dual_port_ram_tb.sv KanagawaHALSimpleDualPortRAM_tb
    OPTIONS
        "-GTB_DATA_WIDTH=40"
        "-GTB_ADDR_WIDTH=8"
        "-GTB_USE_LUTRAM=0"
        "-GTB_USE_BRAM=1"
        "-GTB_USE_OUTPUT_REG=0"
        "-GTB_DEVICE_FAMILY='\"mock\""
)

add_runtime_rtl_test(hal_simple_dual_port_ram_d40_a1_lr0_or1 hal_simple_dual_port_ram_tb.sv KanagawaHALSimpleDualPortRAM_tb
    OPTIONS
        "-GTB_DATA_WIDTH=40"
        "-GTB_ADDR_WIDTH=1"
        "-GTB_USE_LUTRAM=0"
        "-GTB_USE_BRAM=1"
        "-GTB_USE_OUTPUT_REG=1"
        "-GTB_DEVICE_FAMILY='\"mock\""
)

add_runtime_rtl_test(hal_simple_dual_port_ram_d1_a8_lr0_or1 hal_simple_dual_port_ram_tb.sv KanagawaHALSimpleDualPortRAM_tb
    OPTIONS
        "-GTB_DATA_WIDTH=1"
        "-GTB_ADDR_WIDTH=8"
        "-GTB_USE_LUTRAM=0"
        "-GTB_USE_BRAM=1"
        "-GTB_USE_OUTPUT_REG=1"
        "-GTB_DEVICE_FAMILY='\"mock\""
)

add_runtime_rtl_test(hal_simple_dual_port_ram_d39_a7_lr0_or1 hal_simple_dual_port_ram_tb.sv KanagawaHALSimpleDualPortRAM_tb
    OPTIONS
        "-GTB_DATA_WIDTH=39"
        "-GTB_ADDR_WIDTH=7"
        "-GTB_USE_LUTRAM=0"
        "-GTB_USE_BRAM=1"
        "-GTB_USE_OUTPUT_REG=1"
        "-GTB_DEVICE_FAMILY='\"mock\""
)

add_runtime_rtl_test(hal_simple_dual_port_ram_d81_a6_lr0_or1 hal_simple_dual_port_ram_tb.sv KanagawaHALSimpleDualPortRAM_tb
    OPTIONS
        "-GTB_DATA_WIDTH=81"
        "-GTB_ADDR_WIDTH=6"
        "-GTB_USE_LUTRAM=0"
        "-GTB_USE_BRAM=1"
        "-GTB_USE_OUTPUT_REG=1"
        "-GTB_DEVICE_FAMILY='\"mock\""
)

add_runtime_rtl_test(hal_simple_dual_port_ram_non_pow2_depth_s10 hal_simple_dual_port_ram_tb.sv KanagawaHALSimpleDualPortRAM_tb
    OPTIONS
        "-GTB_DATA_WIDTH=40"
        "-GTB_ADDR_WIDTH=8"
        "-GTB_DEPTH=192"
        "-GTB_USE_LUTRAM=0"
        "-GTB_USE_BRAM=1"
        "-GTB_USE_OUTPUT_REG=1"
        "-GTB_DEVICE_FAMILY='\"mock\""
)

add_runtime_rtl_test(loop_generator loop_generator_tb.sv KanagawaLoopGenerator_tb)

add_runtime_rtl_test(semaphore semaphore_tb.sv KanagawaSemaphore_tb)

add_runtime_rtl_test(extern_return_router_switch_node extern_return_router_switch_node_tb.sv KanagawaExternReturnRouter_switch_node_tb)

add_runtime_rtl_test(extern_return_router_dc0 extern_return_router_tb.sv KanagawaExternReturnRouter_tb
    OPTIONS
        "-GDUAL_CLOCK=0"
)

add_runtime_rtl_test(extern_return_router_dc1 extern_return_router_tb.sv KanagawaExternReturnRouter_tb
    OPTIONS
        "-GDUAL_CLOCK=1"
)

add_runtime_rtl_test(thread_counter thread_counter_tb.sv KanagawaThreadCounter_tb)

add_runtime_rtl_test(reorder_buffer reorder_buffer_tb.sv KanagawaReorderBuffer_tb)

add_runtime_rtl_test(context_saver_pilelined_filtering_buffer_fifo_optimization_enabled "context_saver_tb.sv" KanagawaContextSaverPipelinedFiltering_tb
    OPTIONS
        "-GTB_BUFFER_FIFO_OPTIMIZATION=1"
)

add_runtime_rtl_test(context_saver_pilelined_filtering_buffer_fifo_optimization_disabled context_saver_tb.sv KanagawaContextSaverPipelinedFiltering_tb
    OPTIONS
        "-GTB_BUFFER_FIFO_OPTIMIZATION=0"
)

add_runtime_rtl_test(reset_control_fan_out_levels_0 reset_control_tb.sv KanagawaResetControl_tb
    OPTIONS
        "-GTB_FAN_OUT_LEVELS=0"
)

add_runtime_rtl_test(reset_control_fan_out_levels_1 reset_control_tb.sv KanagawaResetControl_tb
    OPTIONS
        "-GTB_FAN_OUT_LEVELS=1"
)

add_runtime_rtl_test(reset_control_fan_out_levels_2 reset_control_tb.sv KanagawaResetControl_tb
    OPTIONS
        "-GTB_FAN_OUT_LEVELS=2"
)

add_runtime_rtl_test(race_counter race_counter_tb.sv KanagawaRaceCounter_tb)

add_runtime_rtl_test(hal_reset_synchronizer hal_reset_synchronizer_tb.sv KanagawaHALResetSynchronizer_tb)

add_runtime_rtl_test(memory_bypass_1_0_0 memory_bypass_tb.sv KanagawaMemoryBypass_tb
    OPTIONS
        "-GNUM_BYPASS_SLOTS=1"
        "-GADDRESS_EARLY=0"
        "-GDATA_EARLY=0"
)

add_runtime_rtl_test(memory_bypass_1_1_0 memory_bypass_tb.sv KanagawaMemoryBypass_tb
    OPTIONS
        "-GNUM_BYPASS_SLOTS=1"
        "-GADDRESS_EARLY=1"
        "-GDATA_EARLY=0"
)

add_runtime_rtl_test(memory_bypass_1_0_1 memory_bypass_tb.sv KanagawaMemoryBypass_tb
    OPTIONS
        "-GNUM_BYPASS_SLOTS=1"
        "-GADDRESS_EARLY=0"
        "-GDATA_EARLY=1"
)

add_runtime_rtl_test(memory_bypass_2_0_0 memory_bypass_tb.sv KanagawaMemoryBypass_tb
    OPTIONS
        "-GNUM_BYPASS_SLOTS=2"
        "-GADDRESS_EARLY=0"
        "-GDATA_EARLY=0"
)

add_runtime_rtl_test(memory_bypass_2_1_0 memory_bypass_tb.sv KanagawaMemoryBypass_tb
    OPTIONS
        "-GNUM_BYPASS_SLOTS=2"
        "-GADDRESS_EARLY=1"
        "-GDATA_EARLY=0"
)

add_runtime_rtl_test(memory_bypass_2_0_1 memory_bypass_tb.sv KanagawaMemoryBypass_tb
    OPTIONS
        "-GNUM_BYPASS_SLOTS=2"
        "-GADDRESS_EARLY=0"
        "-GDATA_EARLY=1"
)

add_runtime_rtl_test(memory_bypass_2_1_1 memory_bypass_tb.sv KanagawaMemoryBypass_tb
    OPTIONS
        "-GNUM_BYPASS_SLOTS=2"
        "-GADDRESS_EARLY=1"
        "-GDATA_EARLY=1"
)

add_runtime_rtl_test(memory_bypass_3_0_0 memory_bypass_tb.sv KanagawaMemoryBypass_tb
    OPTIONS
        "-GNUM_BYPASS_SLOTS=3"
        "-GADDRESS_EARLY=0"
        "-GDATA_EARLY=0"
)
