# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

add_riscv_executable(risc_v_invalid_instruction_address
    ELF invalid_instruction_address.elf
    SOURCES invalid_instruction_address.c
)

add_riscv_simulation_test(invalid_instruction_address_1hart
    SIM_EXE_TARGET
        riscv_sim.1hart
    ELF_TARGET
        risc_v_invalid_instruction_address
)