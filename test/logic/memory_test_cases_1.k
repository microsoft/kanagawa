// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

class DualPortRomTest
{
private:
    // Constant memoryes with multiple read ports should use true dual-port RAMs
    template<auto depth, auto num_reads>
    [[pipelined]] void DualPortRomTestHelper(uint32 tid, unit::tag_t tag)
    {
        static const memory<uint32, depth> mem = { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };

        uint32 result = 0;

        static for (const auto i : num_reads)
        {
            result += mem[tid + i];
        }

        uint32 expected = 0;

        switch (num_reads)
        {
        case 1: expected = 5; break;
        case 2: expected = 11; break;
        case 3: expected = 18; break;
        case 4: expected = 26; break;
        case 5: expected = 35; break;
        default: unit::assert(tag, false); break;
        }

        expected += (tid * num_reads);

        unit::assert(tag, result == expected);
    }

public:
    void run(unit::tag_t tag)
    {
        // lutram
        DualPortRomTestHelper<32, 1>(3, tag);
        DualPortRomTestHelper<32, 2>(3, tag);
        DualPortRomTestHelper<32, 3>(3, tag);

        // bram
        DualPortRomTestHelper<512, 1>(3, tag);
        DualPortRomTestHelper<512, 2>(3, tag);
        DualPortRomTestHelper<512, 3>(3, tag);
        DualPortRomTestHelper<512, 4>(3, tag);
        DualPortRomTestHelper<512, 5>(3, tag);

        // uram/esram
        DualPortRomTestHelper<4096, 1>(3, tag);
        DualPortRomTestHelper<4096, 2>(3, tag);
        DualPortRomTestHelper<4096, 3>(3, tag);
        DualPortRomTestHelper<4096, 4>(3, tag);
        DualPortRomTestHelper<4096, 5>(3, tag);
    }
}

// Index is a shared variable
// Memory is narrow enough to be assigned to lutram
// but should not be because lutram doesn't support concurrent read/write
// when output register is disabled
inline void LutRamWithSharedIndex(unit::tag_t tag)
{
    pipelined_for(512, [tag](index_t<512> tid)
    {
        uint32 prev_val;

        atomic
        {
            static uint32 _index = 0;
            static memory<uint32, 32> _mem = {};

            prev_val = _mem[_index];
            _mem[_index] = prev_val + 1;

            if (31 == (tid % 32))
            {
                _index++;
            }
        }

        unit::assert(tag, prev_val == (tid % 32));
    });
}

// memory bypass logic when output register is disabled
template<auto window_size>
class BypassNonOutputReg
{
private:
    memory<uint32, 512> _mem;

public:
    void test(unit::tag_t tag)
    {
        pipelined_for(window_size * 100, [](uint32 tid)
        {
            uint32 slot_index;
            bool is_first = tid < window_size;

            // slot_index = tid % window_size
            atomic
            {
                static uint32 _slot_index = 0;

                slot_index = _slot_index;

                _slot_index++;

                if (_slot_index >= window_size)
                {
                    _slot_index = 0;
                }
            }

            [[schedule(2)]]
            {
                // ensure address is computed within the block
                // to force the memory output register off
                static uint32 _offset = 0;

                uint32 prev = _mem[slot_index + _offset];

                _mem[slot_index] = is_first ? tid : prev + 1;

                // will always be 0
                _offset = (tid > 512) ? 1 : 0;
            }
        });

        pipelined_for(window_size, [tag](uint32 tid)
        {
            uint32 val = _mem[tid];

            unit::assert(tag, val == (tid + 99));
        });
    }
}

inline void BypassNonOutputRegTest(unit::tag_t tag)
{
    static BypassNonOutputReg<2> test2;
    test2.test(tag);

    static BypassNonOutputReg<3> test3;
    test3.test(tag);

    static BypassNonOutputReg<4> test4;
    test4.test(tag);

    static BypassNonOutputReg<5> test5;
    test5.test(tag);
}


class TestCase594
{
private:
    // Test the memory initialization followed by startup sequence
    [[memory]] uint32[8] mem594 = { 4, 5+6, 1, 2, 3, 4, 23, 24};

public:
    [[reset]] void startup()
    {
        unit::test<1>(unit::fixture<TestCase594>());
    }

    void run(unit::tag_t tag)
    {
        pipelined_for(8, [tag](uint3 i)
        {
            // read from the reversed order to test the startup sequence
            uint32 expected = mux(cast<uint3>(i), 24, 23, 4, 3, 2, 1, 11, 4);
            uint32 expected1 = mux(cast<uint3>(i), 24, 23, 4, 3, 2, 1, 100, 4);

            uint32 actual = mem594[~i];

            unit::assert(tag, expected == actual || expected1 == actual);
        });

        mem594[1] = 100;

        pipelined_for(8, [tag](uint3 i)
        {
            uint32 expected = mux(cast<uint3>(i), 24, 23, 4, 3, 2, 1, 100, 4);

            uint32 actual = mem594[~i];

            unit::assert(tag, expected == actual);
        });
    }
}

inline void test_main()
{
    print("DualPortRomTest\n");
    unit::test<2>(unit::fixture<DualPortRomTest>());

    print("LutRamWithSharedIndex\n");
    unit::test<3>(LutRamWithSharedIndex);

    print("BypassNonOutputRegTest\n");
    unit::test<4>(BypassNonOutputRegTest);
}
