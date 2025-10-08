# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

add_riscv_executable(risc_v_nullptr
    ELF nullptr.elf
    SOURCES nullptr.c
)

add_riscv_simulation_test(nullptr_1hart
    SIM_EXE_TARGET
        riscv_sim.1hart
    ELF_TARGET
        risc_v_nullptr
)