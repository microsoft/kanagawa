# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

add_riscv_executable(risc_v_unaligned_branch_address
    ELF unaligned_branch_address.elf
    SOURCES unaligned_branch_address.c
)

add_riscv_simulation_test(unaligned_branch_address_1hart
    SIM_EXE_TARGET
        riscv_sim.1hart
    ELF_TARGET
        risc_v_unaligned_branch_address
)