// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Inserts a loop with a loop-carried dependency
// to force a delay
template<auto num_iterations, typename T>
inline T delay(T input)
{
    // This is only a NOP if the delay is a multiple of 2
    static assert(0 == (num_iterations % 2));

    T result = input;

    static for (const auto i : num_iterations)
    {
        result = ~result;
    }

    return result;
}

// Global data propgation for a variable defined in 2 separate basic blocks
inline void TwoDefinitionTest(unit::tag_t tag)
{
    pipelined_for(128, [tag](uint32 id)
    {
        uint32 tripCount = (id % 4) + 1;
        uint32 i = 0; // i is written in this basic block

        do
        {
            // This basic block reads and writes i
            // it must handle the fact that there are 2 blocks that write to i
            i++;
        } while(i < tripCount);

        unit::assert(tag, i == tripCount);
    });
}

// Global data propagation for 2 basic blocks separated by an atomic do
inline void WaitForTest(unit::tag_t tag)
{
    pipelined_for(128, [tag](uint32 id)
    {
        static uint32 _x;

        // y is defined in this basic block
        uint32 y = static_cast(id + 4);

        atomic
        {
            _x = id;
        }

        atomic do; while(_x < id);

        // y is used in this basic block
        unit::assert(tag, y == (id + 4));
    });
}

// Definition before a synchronous call, use after
class SyncCallTest
{
private:
    noinline uint32 SyncCallHelper(uint32 x)
    {
        return x + 3;
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(128, [tag](uint32 id)
        {
            uint32 x = static_cast(id + 4);

            uint32 result = 0;

            result = SyncCallHelper(x);

            unit::assert(tag, result == (x + 3));
            unit::assert(tag, result == (id + 7));
        });
    }
}

// Synchronous call, under predication
class PredicatedSyncCallTest
{
private:
    noinline uint32 SyncCallHelper(uint32 x)
    {
        return x + 3;
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(128, [tag](uint32 id)
        {
            uint32 x = static_cast(id + 4);

            uint32 result = 0;

            bool b =  (1 == (id % 2));

            if (b)
            {
                result = SyncCallHelper(x);
            }

            if (1 == (id % 2))
            {
                unit::assert(tag, result == (x + 3));
                unit::assert(tag, result == (id + 7));
            }
            else
            {
                unit::assert(tag, result == 0);
            }
        });
    }
}

// Global data propgation optimization with calls to pipelined functions with return values
// The return values are handled specially during liveness analysis
inline void SyncCallToPipelinedTest(unit::tag_t tag)
{
    pipelined_for(128, [tag](uint32 id)
    {
        uint32 x = static_cast(id + 4);

        uint32[4] result = pipelined_map<4>(4, [x](index_t<4> i) -> uint32
        {
            return i + x + 5;
        });

        static for (const auto i : 4)
        {
            unit::assert(tag, result[i] == (id + i + 9));
        }
    });
}

// Global data propgation - variable accessed in an atomic blocks
inline void AtomicAccessTest(unit::tag_t tag)
{
    pipelined_for(128, [tag](uint32 id)
    {
        uint32 tripCount = (id % 4) + 1;
        uint32 i = 0; // i is written in this basic block

        atomic
        {
            tripCount++;
        }

        do
        {
            // This basic block reads and writes i
            // it must handle the fact that there are 2 blocks that write to i
            i++;
        } while(i < tripCount);

        uint32 snappedTripCount;

        atomic
        {
            snappedTripCount = tripCount;
        }

        unit::assert(tag, i == snappedTripCount);
        unit::assert(tag, i == tripCount);
    });
}

// Global data propagation should be disabled for unordered functions
// Thread ID allocation scheme does not support unordered functions
class UnorderedTest
{
private:
    [[unordered, max_threads(4)]] uint32 UnorderedHelper(uint32 x)
    {
        uint32 i = 0;
        uint32 tripCount = (x % 8) + 1;

        [[unordered]]
        do
        {
            i++;
        }
        while(i < tripCount);

        return i;
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(128, [tag](uint32 id)
        {
            reorder
            {
                uint32 result = UnorderedHelper(id);

                unit::assert(tag, result == ((id % 8) + 1));
            }
        });
    }
}

// Live-in variable is used in an atomic do - a memory load cannot be inserted before the atomic do
// The only reference to "id" in the basic block containing the atomic do is inside of the atomic do
inline void WaitInputVarForTest(unit::tag_t tag)
{
    pipelined_for(128, [tag](uint32 id)
    {
        static uint32 _i = 0;

        uint32 id_plus_one = static_cast(id + 1);
        uint32 id_plus_two = static_cast(id + 2);

        _i = id;

        do
        {
            id = delay<64>(id);
        } while (id_plus_one > 0x00ffffff);

        atomic do; while(_i < id);

        // No access to "id" here
        unit::assert(tag, (id_plus_one + 1) == id_plus_two);
    });
}

class AtomicRmwTestCase
{
private:
    // Atomic block that reads and writes a memory
    // The address is data propagation candidate
    memory<uint32, 32> atomicRmwMem;

public:

    void run(unit::tag_t tag)
    {
        pipelined_for(32, [](uint32 x)
        {
            atomicRmwMem[x] = 0;
        });

        pipelined_for(128, [tag](uint32 x)
        {
            index_t<32> addr = x % 2;

            x = delay<32>(x);

            // x modifications here are only there to ensure the loop isn't optimized away
            do
            {
                x++;
            } while (x > 1000);

            unit::assert(tag, x <= 128);

            atomic
            {
                uint32 val = atomicRmwMem[addr];
                val++;
                atomicRmwMem[addr] = val;
            }
        });

        pipelined_for(32, [tag](uint32 x)
        {
            uint32 actual = atomicRmwMem[x];
            uint32 expected = (x < 2) ? 64 : 0;

            unit::assert(tag, actual == expected);
        });
    }
}

class DataPropEnqueueAfterStartConditionTest
{
private:
    [[pipelined]] void DataPropEnqueueAfterStartCondition(index_t<512> tid)
    {
        // Prior to fixing the compiler, data propagation FIFOs would enqueue data
        // (including a thread's ReorderSlotId) before the basic block's start condition
        // was evaluated. Enqueuing an incorrect ReorderSlotId then leads to deadlock in
        // the ReorderBuffer.
        bool foo;
        do
        {
            atomic do; while(cycles() % 0x100 < 0xFF);
            stages<5>();
        }
        while(foo);
    }

public:
    void run(unit::tag_t tag)
    {
        DataPropEnqueueAfterStartCondition(512);
    }
}

inline void OverlappingRangeTest(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 x = tid + 1;

        uint32 a = static_cast(x + 2);
        uint32 b = static_cast(x * 7);
        uint32 c = static_cast((x * 9) + 45);

        stages<4>();

        uint32 sum = static_cast(a + b + c);

        const uint32[8] expected_array =
        {
            0x00000040,
            0x00000051,
            0x00000062,
            0x00000073,
            0x00000084,
            0x00000095,
            0x000000a6,
            0x000000b7
        };

        const uint32 expected = expected_array[tid];
        const uint32 actual = sum;

        unit::assert(tag, actual == expected);
    });
}

inline void WrittenInAtomicDo(unit::tag_t tag)
{
    uint32 x = 0;

    atomic do
    {
        x++;
    } while (x < 10);

    do
    {
        x++;
        stages<4>();
    } while (x < 20);

    unit::assert_equal(tag, 20, x);
}

inline void test_main()
{
    print("TwoDefinitionTest\n");
    unit::test<1>(TwoDefinitionTest);

    print("SyncCallTest\n");
    unit::test<2>(unit::fixture<SyncCallTest>());

    print("PredicatedSyncCallTest\n");
    unit::test<3>(unit::fixture<PredicatedSyncCallTest>());

    print("SyncCallToPipelinedTest\n");
    unit::test<4>(SyncCallToPipelinedTest);

    print("AtomicAccessTest\n");
    unit::test<5>(AtomicAccessTest);

    print("UnorderedTest\n");
    unit::test<6>(unit::fixture<UnorderedTest>());

    print("WaitForTest\n");
    unit::test<7>(WaitForTest);

    print("WaitInputVarForTest\n");
    unit::test<8>(WaitInputVarForTest);

    print("AtomicRmw\n");
    unit::test<9>(unit::fixture<AtomicRmwTestCase>());

    // This test has an atomic do that only fires on specific cycles
    print("DataPropEnqueueAfterStartConditionTest\n");
    unit::test<10>(unit::fixture<DataPropEnqueueAfterStartConditionTest>());

    print("OverlappingRangeTest\n");
    unit::test<11>(OverlappingRangeTest);

    print("WrittenInAtomicDo\n");
    unit::test<12>(WrittenInAtomicDo);
}
