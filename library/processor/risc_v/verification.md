# RISC-V Verification

## Compliance

The compliance tests confirm the basic operation is in accordance with the
specification. They don't not exhaustively test all functional aspects of
a processor – only confirm that it implements the ISA specifications.
A processor by definition is a complex state machine with dynamic interrupts and
multiple modes of operation and privilege levels that present many scenarios
that are not included as part of the scope of the compliance tests.

The RISC-V International technical committee working group for Compliance
(Compliance Working Group – CWG) provides and maintains the test framework and
the compliance [test suites](https://github.com/riscv/riscv-arch-test). The
tests are grouped by ISA extension/subset. The current implementation of the
core is covered by the following subset of the tests:

 - RV32I

        add
        addi
        and
        andi
        auipc
        beq
        bge
        bgeu
        blt
        bltu
        bne
        fence
        jal
        jalr
        lb-align
        lbu-align
        lh-align
        lhu-align
        lui
        lw-align
        or
        ori
        sb-align
        sh-align
        sll
        slli
        slt
        slti
        sltiu
        sltu
        sra
        srai
        srl
        srli
        sub
        sw-align
        xor
        xori  

 - RV32M

        mul
        mulh
        mulhsu
        mulhu

### Test harness

The compliance tests are implemented as assembly source files and together with
reference signatures to verify test results. The assembler source files are
compiled with `riscv64-unknown-elf-gcc` tool-set and executed on RISC-V core
using custom `RVBench` test harness which executes compiled RISC-V executable
and then downloads signatures from the core memory region designated by the test
framework and compares them against reference signatures. The subset of tests
listed above comprises 15,228 individual signatures/test cases.

### Test matrix

The compliance tests can be run using Kanagawa emulator and RTL simulator via
CoSim. The configurations of the core covered by the test matrix include:

                                "I" Base ISA        "M" extension            
    --------------------------------------------------------------
    1-hart                       +                   +
    1-hart with External Fetch   +                   +
    2-hart                       +                   n/a
    4-hart                       +                   +

## Directed tests

Directed tests that cover specific aspects of the micro-architecture are
implemented in Kanagawa plus C programs compiled to RISC-V target. The tests
cover the following features/areas (partially overlapping with the official
compliance tests):

 - Instruction decoder
 - ALU operations
 - Branch conditions
 - Micro-op fetch
 - External fetch (fetch from arbitrary latency "memory")
 - CSR (insret, clock, time)
 - Branch prediction performance (verify expected number of mis-predictions)
 - Branch Target Buffer poisoning (verify curing of BTB "poisoned" by previous
   program)
 - Multi-hart execution
 - MMIO
 - Multiplication instructions from "M" extension
 - Interaction of pipeline stalls due to external fetch/MMIO/branch mis-prediction
 - Custom instruction interface
 - TCM interface


The directed tests are executed in Kanagawa emulator and RTL simulator.

## Verification with Instruction Stream Generation (ISG)

In order to cover extreme corner cases and rare or unusual conditions the core
is tested using random instruction stream generator with traces compared against
reference simulator model.

The Google Open Source Project: RISC-V DV with Instruction Stream Generation is
used to generated large number of constrained instruction streams. The Spike ISA
Simulator is used as the reference model.

ISG tests are run under Kanagawa emulator and on real hardware on FPGA.

*TODO: document constraints used to configure ISG* 

## Stall injection

In addition to normal production configurations, the directed tests and ISG
tests are also run with random stalling logic injected by Kanagawa compiler.

## Coverage metrics

Branch coverage metrics at Kanagawa source code level are obtained for
compliance tests and directed tests. The goal is 100% coverage for tested
configurations.

## References

[RISC-V Architecture Test](https://github.com/riscv/riscv-arch-test)
[RISC-V DV with Instruction Stream Generation (ISG)](https://github.com/google/riscv-dv)
[Spike RISC-V ISA Simulator](https://github.com/riscv/riscv-isa-sim)

