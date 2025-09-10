# RISC-V Core Implementation Details

RISC-V Core can be configured into multiple different variants that make
different size/fmax/performance trade-offs. This document describes details of
micro-architecture common to all variant followed by specific characteristics of
each configuration.

## Branch Prediction

RISC-V core performs dynamic branch prediction using 2-bit Branch Target Buffer.

## Pipeline Stalls

Execution pipeline can stall only as result of user provided Memory Mapped IO
Store handler or System Trap instruction handler stalling to handle
backpressure. There are no late result/data hazard stalls.

## 1-hart configuration

The 1-hart configuration is designed to balance of single threaded execution
performance and small core size.

### Instruction Performance

    Instruction                                         Cycles  Penalty
    --------------------------------------------------------------------------
    Branch predicted (taken or not taken)               1
    Branch mispredicted                                 4       Pipeline flush
    LOAD                                                2
    ECALL                                               1+      Pipeline flush
    EBREAK                                              1+      Pipeline flush
    MUL, MULH, MULHSU, MULHU                            5 (**)
    DIV, DIVU, REM, REMU                                n/a
    All other instructions                              1

    (*) Wiht optional "M" Standard Extension

## 2-hart configuration

The 2-hart configuration provides 2 hardware execution threads and is designed
to balance high IPC (instructions per cycles) and single thread execution
performance.

### Instruction Performance

    Instruction                                         Cycles  Penalty
    --------------------------------------------------------------------------
    Branch predicted (taken or not taken)               1
    Branch mispredicted                                 3       Pipeline flush
    LOAD                                                1
    ECALL                                               1+      Pipeline flush
    EBREAK                                              1+      Pipeline flush
    MUL, MULH, MULHSU, MULHU                            4 (*)
    DIV, DIVU, REM, REMU                                n/a
    All other instructions                              1

    (*) Wiht optional "M" Standard Extension

## 4-hart configuration

The 4-hart configuration provides 4 hardware execution threads and supports
1-cycle execution of all instruction, include instruction from "M" extension.

### Instruction Performance

    Instruction                                         Cycles  Penalty
    --------------------------------------------------------------------------
    Branch predicted (taken or not taken)               1
    Branch mispredicted                                 2/3 (*) Pipeline flush
    LOAD                                                1
    ECALL                                               1+      Pipeline flush
    EBREAK                                              1+      Pipeline flush
    MUL, MULH, MULHSU, MULHU                            1 (**)
    DIV, DIVU, REM, REMU                                n/a
    All other instructions                              1

     (*) Area optimized configuration has 1 cycle branch misprediction penalty,
         Fmax optimized configured has 2 cycles penalty.
    (**) With optional "M" Standard Extension.

## 8-hart configuration

The 8-hart configuration provides 8 hardware execution threads and average
performance of 1 instructions per cycles. This configuration is designed for
highly parallel workloads that need maximum aggregate performance but can
tolerate lower per-thread throughput.

### Instruction Performance

    Instruction                                         Cycles  Penalty
    --------------------------------------------------------------------------
    Branch predicted (taken or not taken)               1
    Branch mispredicted                                 1
    LOAD                                                1
    ECALL                                               1+      Pipeline flush
    EBREAK                                              1+      Pipeline flush
    MUL, MULH, MULHSU, MULHU                            1 (*)
    DIV, DIVU, REM, REMU                                n/a
    All other instructions                              1

    (*) Wiht optional "M" Standard Extension
