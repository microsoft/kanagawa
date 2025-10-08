# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# Uses Verilator to build a simulation executable that can be re-used for multiple RISC-V tests.
function(add_riscv_simulation_exe name)
  set(_opts ENABLE_M)
  set(_one HARTS TESTBENCH TESTBENCH_MODULE)
  set(_multi SOURCES OPTIONS)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT name)
    message(FATAL_ERROR "add_riscv_simulation_exe: missing <name> as first argument.")
  endif()

  if (NOT _ARG_HARTS)
    set(_ARG_HARTS 1)
  endif()

  set(OPTIONS
    "--register-ratio=3"
    "--reset-cycles=2"
    "--define=HARTS#${_ARG_HARTS}"
    --rmw-memory-write-delay=0
    --rmw-hardened-write-delay=0
  )

  if (_ARG_ENABLE_M)
    list(APPEND OPTIONS "--define=ENABLE_M#true")
  else()
    list(APPEND OPTIONS "--define=ENABLE_M#false")
  endif()

  list(APPEND OPTIONS ${_ARG_OPTIONS})

  if (NOT _ARG_SOURCES)
    set(_ARG_SOURCES "${CMAKE_CURRENT_FUNCTION_LIST_DIR}/testbench.k")
  endif()

  if (NOT _ARG_TESTBENCH_MODULE)
    set(_ARG_TESTBENCH_MODULE "risc_v_testbench")
  endif()

  if (NOT _ARG_TESTBENCH)
    set(_ARG_TESTBENCH "${CMAKE_CURRENT_FUNCTION_LIST_DIR}/testbench.sv")
  endif()

  # Add Kanagawa codegen and verilation, but do not generate a ctest
  # Multiple consumers of the Verilated executable will generate their
  # own ctest with different command-line arguments. This allows one
  # verilated executable to be re-used across many tests.
  add_kanagawa_verilator_test(${name}
    SOURCES ${_ARG_SOURCES}
    OPTIONS ${OPTIONS}
    TESTBENCH_MODULE ${_ARG_TESTBENCH_MODULE}
    TESTBENCH ${_ARG_TESTBENCH}
    GENERATED_RTL
      risc_v_wrapper.sv
      risc_v_wrapper_types.sv
  )
endfunction()

# Using the pre-compiled simulation executables and pre-compiled RISC-V memory images,
# this function creates a test that runs a program on a simulated RISC-V processor
function(add_riscv_simulation_test test_name)
  set(_opts ENABLE_INSTRUCTION_TRACE)
  set(_one SIM_EXE_TARGET ELF_TARGET START_PC_OFFSET TIMEOUT_CYCLES DMEM_RESULTS_FILE DMEM_RESULTS_OFFSET CHECK_RESULTS_FILE CHECK_RESULTS_STDOUT)
  set(_multi)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT test_name)
    message(FATAL_ERROR "add_riscv_simulation_test: missing <test_name> as first argument.")
  endif()

  if (NOT _ARG_SIM_EXE_TARGET)
    message(FATAL_ERROR "add_riscv_simulation_test: missing SIM_EXE_TARGET.")
  endif()

  if (NOT _ARG_ELF_TARGET)
    message(FATAL_ERROR "add_riscv_simulation_test: missing ELF_TARGET.")
  endif()

  if (NOT _ARG_START_PC_OFFSET)
    set(_ARG_START_PC_OFFSET 0)
  endif()

  if (NOT _ARG_TIMEOUT_CYCLES)
    set(_ARG_TIMEOUT_CYCLES 100000)
  endif()

  if (NOT _ARG_DMEM_RESULTS_OFFSET)
    set(_ARG_DMEM_RESULTS_OFFSET 0)
  endif()

  # Get the path to the ELF file
  get_target_property(ELF_FILE ${_ARG_ELF_TARGET} ELF_FILE)

  # Get the path to the simulation EXE
  get_target_property(SIM_EXE ${_ARG_SIM_EXE_TARGET} SIM_EXE)

  # Generate IMEM and DMEM memory files from the ELF
  add_riscv_elf_to_mem(${ELF_FILE}
    IMEM_OUT_VAR "IMEM_FILE"
    DMEM_OUT_VAR "DMEM_FILE"
  )

  set(SCOPED_TARGET "library_test.riscv.${test_name}")

  add_custom_target(${SCOPED_TARGET}
    DEPENDS
      ${_ARG_SIM_EXE_TARGET}
      ${IMEM_FILE}
      ${DMEM_FILE}
      ${ELF_FILE}
  )

  set(SCOPED_TEST_NAME "library.riscv.${test_name}")

  set(SIM_ARGS
    "+IMEM_FILE=${IMEM_FILE}"
    "+DMEM_FILE=${DMEM_FILE}"
    "+START_PC_OFFSET=${_ARG_START_PC_OFFSET}"
    "+TIMEOUT_CYCLES=${_ARG_TIMEOUT_CYCLES}"
  )

  if (_ARG_DMEM_RESULTS_FILE)
    list(APPEND SIM_ARGS "+DMEM_RESULTS_FILE=${_ARG_DMEM_RESULTS_FILE}")
    list(APPEND SIM_ARGS "+DMEM_RESULTS_OFFSET=${_ARG_DMEM_RESULTS_OFFSET}")
  endif()

  if (_ARG_CHECK_RESULTS_FILE)
    list(APPEND SIM_ARGS "+CHECK_RESULTS_FILE=${_ARG_CHECK_RESULTS_FILE}")
  endif()

  if (_ARG_CHECK_RESULTS_STDOUT)
    list(APPEND SIM_ARGS "+CHECK_RESULTS_STDOUT=${_ARG_CHECK_RESULTS_STDOUT}")
  endif()

  # ENABLE_INSTRUCTION_TRACE
  if (_ARG_ENABLE_INSTRUCTION_TRACE)
    list(APPEND SIM_ARGS "+ENABLE_INSTRUCTION_TRACE=1")
  else()
    list(APPEND SIM_ARGS "+ENABLE_INSTRUCTION_TRACE=0")
  endif()

  # Add a ctest that runs the simulation
  add_test(
    NAME ${SCOPED_TEST_NAME}
    COMMAND ${SIM_EXE} ${SIM_ARGS}
  )

  add_dependencies(library_tests ${SCOPED_TARGET})
endfunction()


add_riscv_simulation_exe(
  "riscv_sim.1hart"
  HARTS 1
)
add_riscv_simulation_exe(
  "riscv_sim.2hart"
  HARTS 2
)
add_riscv_simulation_exe(
  "riscv_sim.4hart"
  HARTS 4
)
add_riscv_simulation_exe(
  "riscv_sim.8hart"
  HARTS 8
)

add_riscv_simulation_exe(
  "riscv_sim.1hart_m"
  HARTS 1
  ENABLE_M
)
add_riscv_simulation_exe(
  "riscv_sim.2hart_m"
  HARTS 2
  ENABLE_M
)
add_riscv_simulation_exe(
  "riscv_sim.4hart_m"
  HARTS 4
  ENABLE_M
)
add_riscv_simulation_exe(
  "riscv_sim.8hart_m"
  HARTS 8
  ENABLE_M
)

# Using one of the simulation executables created above, and an IMEM and DMEM file created
# from an .elf file created elsewhere, load and execute a RISC-V test program

add_subdirectory(access_fault)
# add_subdirectory(compliance)
# add_subdirectory(coremark)
# add_subdirectory(coremark-M)
add_subdirectory(csr)
# add_subdirectory(illegal)
# add_subdirectory(invalid_instruction_address)
# add_subdirectory(microarchitecture)
# add_subdirectory(nullptr)
# add_subdirectory(unaligned_branch_address)