# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

add_riscv_executable(risc_v_microarchitecture
    ELF microarchitecture.elf
    SOURCES microarchitecture.c
)

add_riscv_simulation_test(microarchitecture_1hart
    SIM_EXE_TARGET
        riscv_sim.1hart
    ELF_TARGET
        risc_v_microarchitecture
    DMEM_RESULTS_FILE
        ${CMAKE_CURRENT_SOURCE_DIR}/harts-1-expected.mem
)

add_riscv_simulation_test(microarchitecture_2hart
    SIM_EXE_TARGET
        riscv_sim.2hart
    ELF_TARGET
        risc_v_microarchitecture
    DMEM_RESULTS_FILE
        ${CMAKE_CURRENT_SOURCE_DIR}/harts-2-expected.mem
)

add_riscv_simulation_test(microarchitecture_4hart
    SIM_EXE_TARGET
        riscv_sim.4hart
    ELF_TARGET
        risc_v_microarchitecture
    DMEM_RESULTS_FILE
        ${CMAKE_CURRENT_SOURCE_DIR}/harts-4-expected.mem
)

add_riscv_simulation_test(microarchitecture_8hart
    SIM_EXE_TARGET
        riscv_sim.8hart
    ELF_TARGET
        risc_v_microarchitecture
    DMEM_RESULTS_FILE
        ${CMAKE_CURRENT_SOURCE_DIR}/harts-8-expected.mem
)
