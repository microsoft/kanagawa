# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# Support this toolchain on any supported platform, for example:
#  - riscv-none-elf-gcc (xPack GNU RISC-V Embedded GCC x86_64) 15.2.0
#  - riscv-none-elf-gcc (xPack GNU RISC-V Embedded GCC arm64) 15.2.0
set(RISCV_COREMARK_SUPPORTED_COMPILER_REGEX "riscv-none-elf-gcc \\(xPack GNU RISC-V Embedded GCC [A-Za-z0-9_]+\\) 15\\.2\\.0")
set(RISCV_COREMARK_SUPPORTED_COMPILER_MSG "riscv-none-elf-gcc (xPack GNU RISC-V Embedded GCC <platform>) 15.2.0")

if ("${RISCV_GCC_NAME_VERSION}" MATCHES "^${RISCV_COREMARK_SUPPORTED_COMPILER_REGEX}$")
  set(RISCV_COREMARK_SCORE_ENABLED 1)
else()
  message(STATUS "RISC-V CoreMark score check disabled. Unsupported compiler version: ${RISCV_GCC_NAME_VERSION}. Supported versions: ${RISCV_COREMARK_SUPPORTED_COMPILER_MSG}")
endif()

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
    set(_ARG_ARCH "rv32i_zicsr")
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
  ARCH rv32i_zicsr
)

add_riscv_coremark_executable(risc_v_coremark_m
  ELF coremark_m.elf
  ARCH rv32im_zicsr
)

function(add_riscv_coremark_simulation_test test_name)
  set(_opts ENABLE_M)
  set(_one NUM_HARTS EXPECTED_COREMARK_SCORE)
  set(_multi)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT _ARG_NUM_HARTS)
    message(FATAL_ERROR "add_riscv_coremark_simulation_test: missing NUM_HARTS.")
  endif()

  if (_ARG_ENABLE_M)
    set(_elf_target "risc_v_coremark_m")
    set(_sim_exe_target "riscv_sim.${_ARG_NUM_HARTS}hart_m")
  else()
    set(_elf_target "risc_v_coremark")
    set(_sim_exe_target "riscv_sim.${_ARG_NUM_HARTS}hart")
  endif()

  if (NOT _ARG_EXPECTED_COREMARK_SCORE)
    message(FATAL_ERROR "add_riscv_coremark_simulation_test: missing EXPECTED_COREMARK_SCORE.")
  endif()

  if (RISCV_COREMARK_SCORE_ENABLED)
    set(_expected_results_stdout "CoreMark 1.0 : ${_ARG_EXPECTED_COREMARK_SCORE}")
  else()
    message(STATUS "RISC-V CoreMark score checking for ${test_name} is disabled.")
    set(_expected_results_stdout "Correct operation validated.")
  endif()

  add_riscv_simulation_test(${test_name}
    SIM_EXE_TARGET
      ${_sim_exe_target}
    ELF_TARGET
      ${_elf_target}
    CHECK_RESULTS_STDOUT
      ${_expected_results_stdout}
    TIMEOUT_CYCLES
      100000000
  )
endfunction()

add_riscv_coremark_simulation_test(coremark_1hart
    NUM_HARTS 1
    EXPECTED_COREMARK_SCORE
      "1.079"
)

add_riscv_coremark_simulation_test(coremark_2hart
    NUM_HARTS 2
    EXPECTED_COREMARK_SCORE
      "0.623"
)

add_riscv_coremark_simulation_test(coremark_4hart
    NUM_HARTS 4
    EXPECTED_COREMARK_SCORE
      "0.336"
)

add_riscv_coremark_simulation_test(coremark_1hart_m
    NUM_HARTS 1
    ENABLE_M
    EXPECTED_COREMARK_SCORE
      "2.125"
)

add_riscv_coremark_simulation_test(coremark_2hart_m
    NUM_HARTS 2
    ENABLE_M
    EXPECTED_COREMARK_SCORE
      "1.468"
)

add_riscv_coremark_simulation_test(coremark_4hart_m
    NUM_HARTS 4
    ENABLE_M
    EXPECTED_COREMARK_SCORE
      "1.367"
)
