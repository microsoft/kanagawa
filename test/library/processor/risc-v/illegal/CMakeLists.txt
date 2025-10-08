# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

add_riscv_executable(risc_v_illegal
    ELF illegal.elf
    SOURCES illegal.c
)

add_riscv_simulation_test(illegal_1hart
    SIM_EXE_TARGET
        riscv_sim.1hart
    ELF_TARGET
        risc_v_illegal
)