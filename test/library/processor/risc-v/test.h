#pragma once

#include "sim_assert.h"

#define IMEM_ORIGIN 0x80000000
#define MMIO_TRAP_MASK_ADDR 0x80220008
#define MMIO_TRAP_CAPTURED_ADDR 0x8022000C

#define TRAP_INVALID_INSTRUCTION_ADDRESS 0
#define TRAP_ACCESS_FAULT 1
#define TRAP_ILLEGAL_INSTRUCTION 2
#define TRAP_ECALL 4
#define TRAP_EBREAK 5

#define ENABLE_TRAP_CAPTURE(TRAP) \
        *((volatile unsigned*)(MMIO_TRAP_MASK_ADDR)) = (1 << (TRAP));

#define CLEAR_TRAP_CAPTURE() \
    *((volatile unsigned*)(MMIO_TRAP_MASK_ADDR)) = 0;

#define ASSERT_TRAP_OCCURRED(TRAP) \
    sim_assert((1 << (TRAP)) == *((volatile unsigned*)(MMIO_TRAP_CAPTURED_ADDR))); \
    sim_assert(0 == *((volatile unsigned*)(MMIO_TRAP_CAPTURED_ADDR)));
