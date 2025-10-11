# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

function(add_riscv_coremark_executable target)
  set(_opts)
  set(_one ELF ARCH)
  set(_multi)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT target)
    message(FATAL_ERROR "add_riscv_coremark_executable: missing <target> as first argument.")
  endif()

  if (NOT _ARG_ELF)
    message(FATAL_ERROR "add_riscv_coremark_executable: missing ELF.")
  endif()

  if (NOT _ARG_ARCH)
    set(_ARG_ARCH "rv32i")
  endif()

  set(COREMARK_DIR ${CMAKE_SOURCE_DIR}/thirdparty/coremark)

  set(OPTIMIZATION "-O3")

  add_riscv_executable(${target}
    ELF
      ${_ARG_ELF}
    ARCH
      ${_ARG_ARCH}
    SOURCES
      ${COREMARK_DIR}/core_list_join.c
      ${COREMARK_DIR}/core_main.c
      ${COREMARK_DIR}/core_matrix.c
      ${COREMARK_DIR}/core_state.c
      ${COREMARK_DIR}/core_util.c
      ${COREMARK_DIR}/barebones/core_portme.c
    OPTIONS
      "${OPTIMIZATION}"
      "-funroll-loops"
      "-finline-functions"
      "-funroll-all-loops"
      "-finline-limit=1000"
      "-fno-tree-loop-distribute-patterns"
      "-fselective-scheduling"
      "-fno-strict-overflow"
      "--param"
      "fsm-scale-path-stmts=3"
    DEFINITIONS
      "FLAGS_STR=\\\"${OPTIMIZATION}\\\""
      "CLOCKS_PER_SEC=1000000"
  )
endfunction()

add_riscv_coremark_executable(risc_v_coremark
  ELF coremark.elf
  ARCH rv32i
)

add_riscv_coremark_executable(risc_v_coremark_m
  ELF coremark_m.elf
  ARCH rv32im
)

add_riscv_simulation_test(coremark_1hart
    SIM_EXE_TARGET
        "riscv_sim.1hart"
    ELF_TARGET
        "risc_v_coremark"
    CHECK_RESULTS_STDOUT
      "CoreMark 1.0 : 1.06"
    TIMEOUT_CYCLES
      "50000000"
)

add_riscv_simulation_test(coremark_2hart
    SIM_EXE_TARGET
        "riscv_sim.2hart"
    ELF_TARGET
        "risc_v_coremark"
    CHECK_RESULTS_STDOUT
      "CoreMark 1.0 : 0.618"
    TIMEOUT_CYCLES
      "50000000"
)

add_riscv_simulation_test(coremark_4hart
    SIM_EXE_TARGET
        "riscv_sim.4hart"
    ELF_TARGET
        "risc_v_coremark"
    CHECK_RESULTS_STDOUT
      "CoreMark 1.0 : 0.333"
    TIMEOUT_CYCLES
      "100000000"
)

add_riscv_simulation_test(coremark_1hart_m
    SIM_EXE_TARGET
        "riscv_sim.1hart_m"
    ELF_TARGET
        "risc_v_coremark_m"
    CHECK_RESULTS_STDOUT
      "CoreMark 1.0 : 2.62"
    TIMEOUT_CYCLES
      "50000000"
)

add_riscv_simulation_test(coremark_2hart_m
    SIM_EXE_TARGET
        "riscv_sim.2hart_m"
    ELF_TARGET
        "risc_v_coremark_m"
    CHECK_RESULTS_STDOUT
      "CoreMark 1.0 : 1.70"
    TIMEOUT_CYCLES
      "50000000"
)

add_riscv_simulation_test(coremark_4hart_m
    SIM_EXE_TARGET
        "riscv_sim.4hart_m"
    ELF_TARGET
        "risc_v_coremark_m"
    CHECK_RESULTS_STDOUT
      "CoreMark 1.0 : 0.968"
    TIMEOUT_CYCLES
      "100000000"
)
