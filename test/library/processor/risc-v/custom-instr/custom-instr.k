// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import processor.risc_v

const auto IMEM_LENGTH = 0x500;
const auto IMEM_ORIGIN = 0;
const auto DMEM_LENGTH = 0x2000;
const auto DMEM_ORIGIN = 0x00200000;
const auto MMIO_LENGTH = 0x100;
const auto MMIO_ORIGIN = 0x10000000;
const auto HARTS = 2;
const auto STACK_START = DMEM_ORIGIN + DMEM_LENGTH - 0x10;
const auto STACK_SIZE  = 0x100;

using RV = RISC_V<HARTS, IMEM_LENGTH, DMEM_LENGTH, MMIO_LENGTH, IMEM_ORIGIN, DMEM_ORIGIN, MMIO_ORIGIN>;
using int_t = RV::int_t;
using uint_t = RV::uint_t;
using hart_index_t = RV::hart_index_t;
using imem_addr_t = RV::imem_addr_t;

export RVG;

class RISCVCustomInstrWrapper
{
private:
    RV core = {
        .mmio_store = CheckResult,
        .custom_decode = CustomDecode,
        .custom_execute = CustomExecute
    };

    bool[HARTS] finished;
    
    [[latency(1)]] (hart_index_t hid, RVG major_opcode, Funct3 minor_opcode, int_t op1, int_t op2, int_t imm, Funct7 funct7)->int_t CustomInstr;

    inline auto CheckResult(hart_index_t hid, uint_t addr, MemorySize size, int_t value)
    {
        assert(value == 45);
        finished[hid] = true;
        return true;
    }

    inline auto CustomDecode (RVG major_opcode)
    {
        return major_opcode == RVG::custom_0 ? Format::R : Format::Invalid;
    }

    inline auto CustomExecute(hart_index_t hid, RVG major_opcode, Funct3 minor_opcode, int_t op1, int_t op2, int_t imm, Funct7 funct7)
    {
        return CustomInstr(hid, major_opcode, minor_opcode, op1, op2, imm, funct7);
    }

public:
    void StartRISCV()
    {
        /*
            volatile unsigned* io_start = (unsigned*)0x10000000;

            int x = 0;

            asm volatile (".insn r CUSTOM_0, 0, 0, x0, %[rs1], x0"
                : : [rs1] "r" (x));

            for (int i = 0; i < 10; ++i)
            {
                asm volatile (".insn r CUSTOM_0, 1, 0, x0, x0, %[rs2]"
                    : : [rs2] "r" (i));
            }

            asm volatile (".insn r CUSTOM_0, 2, 0, %[rd], x0, x0"
                : [rd] "=r" (x) :);

            *io_start = x;

        00000000 <main>:
        0:   fe010113                addi    sp,sp,-32
        4:   00812e23                sw      s0,28(sp)
        8:   02010413                addi    s0,sp,32
        c:   fe042423                sw      zero,-24(s0)
        10:   fe842783                lw      a5,-24(s0)
        14:   0007800b                0x7800b
        18:   fe042623                sw      zero,-20(s0)
        1c:   0180006f                j       34 <main+0x34>
        20:   fec42783                lw      a5,-20(s0)
        24:   00f0100b                0xf0100b
        28:   fec42783                lw      a5,-20(s0)
        2c:   00178793                addi    a5,a5,1
        30:   fef42623                sw      a5,-20(s0)
        34:   fec42703                lw      a4,-20(s0)
        38:   00900793                li      a5,9
        3c:   fee7d2e3                bge     a5,a4,20 <main+0x20>
        40:   0000278b                0x278b
        44:   fef42423                sw      a5,-24(s0)
        48:   fe842783                lw      a5,-24(s0)
        4c:   00078513                mv      a0,a5
        50:   01c12403                lw      s0,28(sp)
        54:   02010113                addi    sp,sp,32
        58:   10000737                lui     a4,0x10000
        5c:   00f72023                sw      a5,0(a4)
        60:   0000006f                j       60

        */

        uint_t[25] custom_state = {
            /* 00000000 */ 0xfe010113, 0x00812e23, 0x02010413, 0xfe042423,
            /* 00000010 */ 0xfe842783, 0x0007800b, 0xfe042623, 0x0180006f,
            /* 00000020 */ 0xfec42783, 0x00f0100b, 0xfec42783, 0x00178793,
            /* 00000030 */ 0xfef42623, 0xfec42703, 0x00900793, 0xfee7d2e3,
            /* 00000040 */ 0x0000278b, 0xfef42423, 0xfe842783, 0x00078513,
            /* 00000050 */ 0x01c12403, 0x02010113, 0x10000737, 0x00f72023,
            /* 00000060 */ 0x0000006f};

        pipelined_for(25, [custom_state](index_t<25> i)
        {
            core.imem_write(i, custom_state[i]);
        });

        finished = {};

        core.init_stack_pointers(STACK_START, STACK_SIZE);
        core.start({0, 0});

        wait([]()
        {
            return reinterpret_cast<uint<HARTS>>(finished) == cast<uint<HARTS>>(-1);
        });
    }
}

export RISCVCustomInstrWrapper;
