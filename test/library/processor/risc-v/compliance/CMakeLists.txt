# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

function(add_riscv_simulation_compliance_test extension)
  set(_opts)
  set(_one)
  set(_multi INSTRUCTIONS)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT extension)
    message(FATAL_ERROR "add_riscv_simulation_compliance_test: missing <extension> as first argument.")
  endif()

  if (NOT _ARG_INSTRUCTIONS)
    message(FATAL_ERROR "add_riscv_simulation_compliance_test: missing INSTRUCTIONS list.")
  endif()

  set(RISCV_ARCH_TEST_DIR "${CMAKE_SOURCE_DIR}/thirdparty/riscv-arch-test")

  foreach(_i IN LISTS _ARG_INSTRUCTIONS)
    set(TEST_FILE
      ${RISCV_ARCH_TEST_DIR}/riscv-test-suite/rv32i_m/${extension}/src/${_i}-01.S
    )
    set(SIGNATURE_FILE
      ${RISCV_ARCH_TEST_DIR}/riscv-test-suite/rv32i_m/${extension}/references/${_i}-01.reference_output
    )

    set(ELF_TARGET "risc_v_compliance_${_i}")

    add_riscv_executable(${ELF_TARGET}
      ELF "compliance_${_i}.elf"
      SOURCES ${TEST_FILE}
      ARCH "rv32im"
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/model_test.h
      LD_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/model-ld-script"
      OPTIONS
        "-static"
        "-mcmodel=medany"
        "-fvisibility=hidden"
        "-nostdlib"
        "-nostartfiles"
      DEFINITIONS "XLEN=32"
      INCLUDE_DIRS
          "${RISCV_ARCH_TEST_DIR}/riscv-test-suite/env"
          "${CMAKE_CURRENT_SOURCE_DIR}"
    )

    if (NOT "${extension}" STREQUAL "M")
      add_riscv_simulation_test(compliance_1hart_${_i}
        SIM_EXE_TARGET
          riscv_sim.1hart
        ELF_TARGET
          ${ELF_TARGET}
        DMEM_RESULTS_FILE
          ${SIGNATURE_FILE}
        DMEM_RESULTS_OFFSET
          16
      )

      add_riscv_simulation_test(compliance_2hart_${_i}
        SIM_EXE_TARGET
          riscv_sim.2hart
        ELF_TARGET
          ${ELF_TARGET}
        DMEM_RESULTS_FILE
          ${SIGNATURE_FILE}
        DMEM_RESULTS_OFFSET
          16
      )

      add_riscv_simulation_test(compliance_4hart_${_i}
        SIM_EXE_TARGET
          riscv_sim.4hart
        ELF_TARGET
          ${ELF_TARGET}
        DMEM_RESULTS_FILE
          ${SIGNATURE_FILE}
        DMEM_RESULTS_OFFSET
          16
      )
    endif()

    add_riscv_simulation_test(compliance_1hart_m_${_i}
      SIM_EXE_TARGET
        riscv_sim.1hart_m
      ELF_TARGET
        ${ELF_TARGET}
      DMEM_RESULTS_FILE
        ${SIGNATURE_FILE}
      DMEM_RESULTS_OFFSET
        16
    )

    add_riscv_simulation_test(compliance_2hart_m_${_i}
      SIM_EXE_TARGET
        riscv_sim.2hart_m
      ELF_TARGET
        ${ELF_TARGET}
      DMEM_RESULTS_FILE
        ${SIGNATURE_FILE}
      DMEM_RESULTS_OFFSET
        16
    )

    add_riscv_simulation_test(compliance_4hart_m_${_i}
      SIM_EXE_TARGET
        riscv_sim.4hart_m
      ELF_TARGET
        ${ELF_TARGET}
      DMEM_RESULTS_FILE
        ${SIGNATURE_FILE}
      DMEM_RESULTS_OFFSET
        16
    )

  endforeach()

endfunction()

add_riscv_simulation_compliance_test("I"
  INSTRUCTIONS
    "add"
    "addi"
    "and"
    "andi"
    "auipc"
    "beq"
    "bge"
    "bgeu"
    "blt"
    "bltu"
    "bne"
    "fence"
    "jal"
    "jalr"
    "lb-align"
    "lbu-align"
    "lh-align"
    "lhu-align"
    "lui"
    "lw-align"
    "or"
    "ori"
    "sb-align"
    "sh-align"
    "sll"
    "slli"
    "slt"
    "slti"
    "sltiu"
    "sltu"
    "sra"
    "srai"
    "srl"
    "srli"
    "sub"
    "sw-align"
    "xor"
    "xori"
  )

add_riscv_simulation_compliance_test("M"
  INSTRUCTIONS
    "mul"
    "mulh"
    "mulhsu"
    "mulhu"
)

