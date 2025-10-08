# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

add_riscv_executable(risc_v_csr
    ELF csr.elf
    SOURCES csr.c
)

add_riscv_simulation_test(csr_1hart
    SIM_EXE_TARGET
        riscv_sim.1hart
    ELF_TARGET
        risc_v_csr
)