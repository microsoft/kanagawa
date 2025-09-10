// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import processor.risc_v

const auto IMEM_LENGTH = 0x400;
const auto IMEM_ORIGIN = 0;
const auto DMEM_LENGTH = 0x2000;
const auto DMEM_ORIGIN = 0x00200000;
const auto MMIO_LENGTH = 0x1000;
const auto MMIO_ORIGIN = 0x10000000;
const auto STACK_START = DMEM_ORIGIN + DMEM_LENGTH - 0x10;
const auto STACK_SIZE  = 0x100;

using RV = RISC_V<HARTS, IMEM_LENGTH, DMEM_LENGTH, MMIO_LENGTH, IMEM_ORIGIN, DMEM_ORIGIN, MMIO_ORIGIN, IMEM_LENGTH, memory_norep, Extension::None, Optimize::Area, Base::RV32I, 1024, memory>;
using int_t = RV::int_t;
using uint_t = RV::uint_t;
using hart_index_t = RV::hart_index_t;
using imem_addr_t = RV::imem_addr_t;

export RVG;

class RISCVMMIOWrapper
{
private:
    RV core = {
        .system_trap = finish_on_ecall,
        .trace = core.print_trace,
        .mmio_access = mmio_access
    };

    count_t<HARTS> finished;

    memory<int_t, 64> mem;

    [[latency(1)]] (uint_t addr)->int_t LoadLatency1;

    (uint_t addr)->int_t LoadBlocking;

    [[async]] (uint_t addr, int_t value)->void StoreBlocking;

    [[latency(1)]] (uint_t addr, int_t value)->void StoreLatency1;
    
    inline optional<imem_addr_t> finish_on_ecall(hart_index_t hid, Trap trap, imem_addr_t addr)
    {
        if (trap == Trap::ECALL)
        {
            sleep(16);
            finished++;
        }
        else
        {
            assert(false);
        }
        return make_optional(true, addr);
    }

    inline optional<int_t> mmio_load(hart_index_t hid, uint_t addr, MemorySize size, bool sign)
    {
        optional<int_t> result = {};

        if(addr < MMIO_ORIGIN + 64)
        {
            // coherent memory load
            static if (HARTS == 1)
            {
                // implemented in Kanagawa for 1-hart config
                result = make_optional(true, mem[addr]);
            }
            else
            {
                // implemented in Verilog for multiple-hart config
                result = make_optional(true, LoadLatency1(addr));
            }
        }
        else
        {
            async_exec([hid, addr]
            {
                // potentially blocking, asynchronous load
                core.mmio_load_result(hid, LoadBlocking(addr));
            });
        }

        return result;
    }

    inline bool mmio_store(hart_index_t hid, uint_t addr, MemorySize size, int_t value)
    {
        if(addr < MMIO_ORIGIN + 64)
        {
            // coherent memory store
            static if (HARTS == 1)
            {
                // implemented in Kanagawa for 1-hart config
                mem[addr] = value;
            }
            else
            {
                // implemented in Verilog for multiple-hart config
                StoreLatency1(addr, value);
            }
        }
        else
        {
            // potentially blocking [[async]] extern
            StoreBlocking(addr, value);
        }

        return true;
    }

    inline optional<int_t> mmio_access(hart_index_t hid, uint_t load_addr, uint_t store_addr, MemorySize size, bool sign_extend, optional<int_t> value)
    {
        optional<int_t> result;

        if (!value.is_valid)
        {
            result = mmio_load(hid, load_addr, size, sign_extend);
        }
        else
        {
            result.is_valid = mmio_store(hid, store_addr, size, value.value);
        }

        return result;
    }

    inline void sleep(auto delay)
    {
        const auto start = cycles();
        wait([delay, start]()
        {
            return cycles() - start >= delay;
        });
    }

public:
    void bogus()
    {
        static if (HARTS == 1)
        {
            // Make sure that StoreLatency1 ports are included even in HART == 1 config
            StoreLatency1(0, 0);
            LoadLatency1(0);
        }
    }

    void StartRISCV()
    {
        /*
            int main()
            {
                const unsigned MMIO_ORIGIN = 0x10000000;

                volatile unsigned* p1 = (unsigned*)(MMIO_ORIGIN);
                volatile unsigned* p2 = (unsigned*)(MMIO_ORIGIN + 64);

                int x = 0;

                for (int i = 1; i< 10; ++i)
                {
                    p1[i] = i;
                    x += p1[i];
                }

                x += p2[64];

                *p2 = x;

                asm volatile ("ecall");
            }

            00000000 <main>:
            0:   100007b7                lui     a5,0x10000
            4:   00100713                li      a4,1
            8:   00e7a223                sw      a4,4(a5) # 10000004 <__global_pointer$+0xffef804>
            c:   00200693                li      a3,2
            10:   0047a703                lw      a4,4(a5)
            14:   00300613                li      a2,3
            18:   00d7a423                sw      a3,8(a5)
            1c:   0087a683                lw      a3,8(a5)
            20:   00c7a623                sw      a2,12(a5)
            24:   00400613                li      a2,4
            28:   00c7a503                lw      a0,12(a5)
            2c:   00c7a823                sw      a2,16(a5)
            30:   00500613                li      a2,5
            34:   0107a583                lw      a1,16(a5)
            38:   00c7aa23                sw      a2,20(a5)
            3c:   0147a603                lw      a2,20(a5)
            40:   00600813                li      a6,6
            44:   00d70733                add     a4,a4,a3
            48:   0107ac23                sw      a6,24(a5)
            4c:   00a70733                add     a4,a4,a0
            50:   00700813                li      a6,7
            54:   0187a683                lw      a3,24(a5)
            58:   00b70733                add     a4,a4,a1
            5c:   0107ae23                sw      a6,28(a5)
            60:   00800813                li      a6,8
            64:   01c7a503                lw      a0,28(a5)
            68:   00c70733                add     a4,a4,a2
            6c:   0307a023                sw      a6,32(a5)
            70:   00900613                li      a2,9
            74:   0207a583                lw      a1,32(a5)
            78:   02c7a223                sw      a2,36(a5)
            7c:   0247a603                lw      a2,36(a5)
            80:   00d70733                add     a4,a4,a3
            84:   00a70733                add     a4,a4,a0
            88:   1407a683                lw      a3,320(a5)
            8c:   00b70733                add     a4,a4,a1
            90:   00c70733                add     a4,a4,a2
            94:   00d70733                add     a4,a4,a3
            98:   04e7a023                sw      a4,64(a5)
            9c:   00000513                li      a0,0
            a0:   00000073                ecall
        */

        uint_t[44] mmio = {
            /* 00000000 */ 0x100007b7, 0x00100713, 0x00e7a223, 0x00200693,
            /* 00000010 */ 0x0047a703, 0x00300613, 0x00d7a423, 0x0087a683,
            /* 00000020 */ 0x00c7a623, 0x00400613, 0x00c7a503, 0x00c7a823,
            /* 00000030 */ 0x00500613, 0x0107a583, 0x00c7aa23, 0x0147a603,
            /* 00000040 */ 0x00600813, 0x00d70733, 0x0107ac23, 0x00a70733,
            /* 00000050 */ 0x00700813, 0x0187a683, 0x00b70733, 0x0107ae23,
            /* 00000060 */ 0x00800813, 0x01c7a503, 0x00c70733, 0x0307a023,
            /* 00000070 */ 0x00900613, 0x0207a583, 0x02c7a223, 0x0247a603,
            /* 00000080 */ 0x00d70733, 0x00a70733, 0x1407a683, 0x00b70733,
            /* 00000090 */ 0x00c70733, 0x00d70733, 0x04e7a023, 0x00000513,
            /* 000000a0 */ 0x00000073, 0x00000000, 0x00000000, 0x00000000};

        const auto length = bitsizeof(mmio)/bitsizeof(mmio[0]);

        pipelined_for(length, [mmio](index_t<length> i)
        {
            core.imem_write(i, mmio[i]);
        });

        finished = 0;

        core.print_memory_map();
        core.start({0});

        wait([]()
        {
            return finished == HARTS;
        });
    }
}

export RISCVMMIOWrapper;
