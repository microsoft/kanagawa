# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

add_riscv_executable(risc_v_access_fault
    ELF access_fault.elf
    SOURCES access_fault.c
)

add_riscv_simulation_test(access_fault_1hart
    SIM_EXE_TARGET
        riscv_sim.1hart
    ELF_TARGET
        risc_v_access_fault
)