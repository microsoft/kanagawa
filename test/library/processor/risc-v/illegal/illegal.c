#include "../test.h"

int main()
{
    int value = 0xdeadbeef;

    ENABLE_TRAP_CAPTURE(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn r CUSTOM_1, 0, 0, %[rd], x0, x0"
            : [rd] "=r" (value));
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn r AMO, 2, 2, %[rd], x0, x0"
            : [rd] "=r" (value));
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile ("csrr %[rd], 0xF11"
            : [rd] "=r" (value));
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile ("csrrs %[rd], 0xC00, %[rs1]"
            : [rd] "=r" (value)
            : [rs1] "r" (value));
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    // illegal FENCE
    asm volatile (".insn i MISC_MEM, 0, %[rd], x0, 2"
            : [rd] "=r" (value));
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i MISC_MEM, 0, %[rd], x1, 2"
            : [rd] "=r" (value));
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i MISC_MEM, 0, x0, x1, 2");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    // .insn i opcode, func3, rd, rs1, simm12
    asm volatile (".insn i SYSTEM, 0, x0, x0, 2");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i SYSTEM, 0, x0, x0, 2");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    // .insn i opcode, func3, rd, rs1, simm12
    asm volatile (".insn i SYSTEM, 1, x0, x0, 0");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i SYSTEM, 0, x1, x0, 0");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i SYSTEM, 0, x0, x1, 0");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i SYSTEM, 1, x0, x0, 1");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i SYSTEM, 0, x1, x0, 1");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i SYSTEM, 0, x0, x1, 1");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i OP_IMM, 1, x0, x1, 32");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn i OP_IMM, 5, x0, x1, 32");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    // .insn r opcode, funct3, funct7, rd, rs1, rs2
    asm volatile (".insn r OP, 1, 32, x0, x1, x0");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    asm volatile (".insn r OP, 0, 2, x0, x1, x0");
    ASSERT_TRAP_OCCURRED(TRAP_ILLEGAL_INSTRUCTION)

    CLEAR_TRAP_CAPTURE()

    return 0;
}
