// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Test for optimization to convert memories to arrays
class MemToArrayTestHelper
{
public:
    void init()
    {
        pipelined_for(8, [](uint32 tid)
        {
            _mem[tid] = tid + 4;
        });
    }

    void check(unit::tag_t tag)
    {
        pipelined_for(8, [tag](uint32 tid)
        {
            static for (const auto i : 16)
            {
                uint3 idx = cast<uint2>(tid + i);

                uint4 actual = _mem[idx];
                uint4 expected = idx + 4;

                unit::assert(tag, actual == expected);
            }
        });
    }

private:
    memory<uint4, 8> _mem;
}

inline void MemToArrayTest(unit::tag_t tag)
{
    static MemToArrayTestHelper _helper;
    _helper.init();
    _helper.check(tag);
}

inline void MemoryDepthOneTest(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static memory<uint32, 1> _mem;

        uint1 memIdx = 0;

        _mem[memIdx] = 7;

        unit::assert(tag, _mem[memIdx] == 7);
    });
}

class Test509
{
private:
    // atomic read-modify-write to memory - uses bypass
    [[memory]] uint32[32] mem509;

    struct ThreadId509
    {
        uint4 IterationIndex;
        uint5 SlotIndex;
        uint23 Padding;
    }
public:
    [[pipelined]] void Foo509(uint32 threadId)
    {
        ThreadId509 decomposedThreadId = cast<ThreadId509>(threadId);

        uint5 slotIndex = decomposedThreadId.SlotIndex;
        bool isFirst = (decomposedThreadId.IterationIndex == 0);

        atomic
        {
            auto val = mem509[slotIndex];
            val = isFirst ? (slotIndex + 1) : (val + 1);
            mem509[slotIndex] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        const auto NumIterations = 16;

        Foo509(NumIterations * 32);

        for (const uint32 i : 32)
        {
            uint32 val = mem509[i];

            uint32 expected = (i + NumIterations);

            unit::assert(tag, val == expected);
        }
    }
}

class Test510
{
private:
    // atomic read-modify-write to memory - uses bypass - 1/2 frequency, 2 slots
    [[memory]] uint32[32] g_mem510;

public:
    [[pipelined]] void Foo510(uint32 threadId)
    {
        uint9 slotIndex = threadId & 1;
        bool isFirst = threadId < 2;

        atomic
        {
            uint32 val = g_mem510[slotIndex];
            val = isFirst ? (slotIndex + 1) : (val + 1);
            g_mem510[slotIndex] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        Foo510(32);

        for (const uint32 i : 2)
        {
            uint32 val = g_mem510[i];

            uint32 expected = (i + 16);

            unit::assert(tag, val == expected);
        }
    }
}

class Test511
{
private:
    // atomic read-modify-write to memory - uses bypass - 1/3 frequency, 3 slots
    [[memory]] uint32[32] mem511;

    uint9 slotIndex511 = 0;

public:
    [[pipelined]] void Foo511(uint32 threadId)
    {
        uint9 slotIndex;

        atomic
        {
            slotIndex = slotIndex511;

            slotIndex511 = (slotIndex511 == 2) ? 0 : (slotIndex511 + 1);
        }

        bool isFirst = threadId < 3;

        atomic
        {
            auto val = mem511[slotIndex];
            val = isFirst ? (slotIndex + 1) : (val + 1);
            mem511[slotIndex] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        Foo511(32);

        for (const uint32 i : 3)
        {
            uint32 val = mem511[i];

            uint32 expected = (i + mux(cast<uint2>(i), 11, 11, 10, 0));

            unit::assert(tag, val == expected);
        }
}
}

class Test512
{
private:
    // atomic read-modify-write to memory - uses bypass - 1/4 frequency, 4 slots
    [[memory]] uint32[32] mem512;

    uint9 slotIndex512 = 0;
public:
    [[pipelined]] void Foo512(uint32 threadId)
    {
        uint9 slotIndex;

        atomic
        {
            slotIndex = slotIndex512;

            slotIndex512 = (slotIndex512 == 3) ? 0 : (slotIndex512 + 1);
        }

        bool isFirst = threadId < 4;

        atomic
        {
            uint32 val = mem512[slotIndex];
            val = isFirst ? (slotIndex + 1) : (val + 1);
            mem512[slotIndex] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        Foo512(32);

        for (const uint32 i : 4)
        {
            uint32 val = mem512[i];

            uint32 expected = (i + 8);

            unit::assert(tag, val == expected);
        }
    }
}

inline void test_main()
{
    print("MemToArrayTest\n");
    unit::test<2>(MemToArrayTest);

    print("MemoryDepthOneTest\n");
    unit::test<3>(MemoryDepthOneTest);

    print("Test 509\n");
    unit::test<5>(unit::fixture<Test509>());

    print("Test 510\n");
    unit::test<5>(unit::fixture<Test510>());

    print("Test 511\n");
    unit::test<5>(unit::fixture<Test511>());

    print("Test 512\n");
    unit::test<5>(unit::fixture<Test512>());
}
