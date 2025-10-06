// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Return x * 3
inline uint32 MultBy3Loop(uint32 x)
{
    uint32 result = 0;

    for (const uint32 i : x)
    {
        result += 3;
    }

    return result;
}

// Loop that contains 1 basic block.  Backward link should be a register (no fifo need on the backward link)
inline void SimpleLoop(unit::tag_t tag)
{
    // Helper will not be inlined, which prevents compiler optimizations that compute the result of MultBy3Loop at compile time
    inline uint32 Helper(uint32 count)
    {
        return MultBy3Loop(count);
    }

    {
        uint32 actual = Helper(3);

        unit::assert(tag, actual == 9);
    }

    {
        uint32 actual = Helper(6);

        unit::assert(tag, actual == 18);
    }
}

// Loop executed by many threads
// No fifo needed on backward link
inline void SimpleMultithreadedLoop(unit::tag_t tag)
{
    pipelined_for(512, [tag](uint9 count)
    {
        uint32 result = MultBy3Loop(count);

        unit::assert(tag, result == (count * 3));
    });
}

class SimpleLoopWaitForCase
{
private:
    // Loop with a wait-for integrated in the only basic block
    // The backward link should be a real fifo
    bool waitCondition = false;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(512, [tag](uint9 x)
        {
            uint32 result = 0;

            uint32 i = 0;

            do
            {
                // First thread goes through, after it computes a result
                // then it allows other threads through
                atomic do; while(!((x == 0) || waitCondition));
                result += 3;
                i++;
            } while (i < x);

            if (x == 0)
            {
                // One iteration through the loop occured, because it is a do/while loop
                unit::assert(tag, result == 3);
            }
            else
            {
                unit::assert(tag, result == (x * 3));
            }

            atomic
            {
                if (x == 0)
                {
                    waitCondition = true;
                }
            }
        });
    }
}

class SimpleLoopThreadRate
{
private:
    // Simple loop with thread_rate
    // The backward link should be a real fifo
    [[thread_rate(2)]] uint32 MultBy3LoopThreadRate(uint32 x)
    {
        return MultBy3Loop(x);
    }

public:
    inline void run(unit::tag_t tag)
    {
        pipelined_for(512, [tag](uint9 count)
        {
            uint32 result = MultBy3LoopThreadRate(count);

            unit::assert(tag, result == (count * 3));
        });
    }
}

inline void RangeForKnownBound(unit::tag_t tag)
{
    uint32 x = 0;

    for (const auto i : 9)
    {
        x += 3;
    }

    unit::assert(tag, x == 27);
}

// range-for, bound is known to be 0 at compile time
inline void RangeForKnownBoundZero(unit::tag_t tag)
{
    uint32 x = 5;

    for (const auto i : 0)
    {
        x += 3;
    }

    unit::assert(tag, x == 5);
}

// Range-for loop with zero bound
// bound is not known at copmile time
inline void RangeForZeroBoundNotKnown(unit::tag_t tag)
{
    pipelined_for(16, [tag](uint9 count)
    {
        uint32 y = 4;

        uint32 result = MultBy3Loop(count);

        unit::assert(tag, result == (count * 3));
        unit::assert(tag, y == 4);
    });
}

// Induction variable with the same name as another local variable
inline void RangeForShadowing(unit::tag_t tag)
{
    uint32 x = 5;
    uint32 y = 0;

    for (const auto x : 10)
    {
        y += 3;
    }

    unit::assert(tag, x == 5);
    unit::assert(tag, y == 30);
}

inline void UnorderedRangeFor(unit::tag_t tag)
{
    pipelined_for(512, [tag](uint9 count)
    {
        uint32 result = 0;

        reorder
        {
            [[unordered]] for (const uint32 i : count)
            {
                result += 3;
            }
        }

        unit::assert(tag, result == (count * 3));
    });
}

inline void UnrolledForTypeInference(unit::tag_t tag)
{
    uint32 result = 0;

    static for (const auto i : 4)
    {
        result += 5;
    }

    unit::assert(tag, result == 20);
}

inline void NestedUnrolledFor(unit::tag_t tag)
{
    uint32 a = 0;
    uint32 b = 0;
    uint32 c = 0;

    static for (const auto i : 4)
    {
        a = a + 1;
        static for (const auto j : i)
        {
            b = b + 1;
            static for (const auto k : j)
            {
                c = c + 1;
            }
        }
    }

    unit::assert(tag, a == 4);
    unit::assert(tag, b == 6);
    unit::assert(tag, c == 4);
}

inline void RangeForSameVariableName(unit::tag_t tag)
{
    uint32 result = 0;

    for (const auto i : 10)
    {
        result += 2;
    }

    for (const auto i : 5)
    {
        result += 3;
    }

    unit::assert(tag, result == 35);
}

inline void UnrolledForSameVariableName(unit::tag_t tag)
{
    uint32 result = 0;

    static for (const auto i : 10)
    {
        result += 2;
    }

    static for (const auto i : 5)
    {
        result += 3;
    }

    unit::assert(tag, result == 35);
}

// Range-for in a predciate, the predication means that the loop bound
// is either the compile-time known value or 0
inline void PredicatedRangeFor(unit::tag_t tag)
{
    pipelined_for(512, [tag](uint9 tid)
    {
        uint32 result = 0;

        bool b = 0 == (tid % 2);

        if (b)
        {
            for (const auto i : 3)
            {
                result += 5;
            }
        }

        if (b)
        {
            unit::assert(tag, result == 15);
        }
        else
        {
            unit::assert(tag, result == 0);
        }

        atomic
        {
            static uint9 _expected_tid = 0;

            unit::assert(tag, tid == _expected_tid);

            _expected_tid = static_cast(_expected_tid + 1); // Wrap OK
        }
    });
}

inline void test_main()
{
    print("RangeForKnownBoundZero\n");
    unit::test<1>(RangeForKnownBoundZero);

    print("RangeForKnownBound\n");
    unit::test<2>(RangeForKnownBound);

    print("SimpleLoop\n");
    unit::test<3>(SimpleLoop);

    print("SimpleMultithreadedLoop\n");
    unit::test<4>(SimpleMultithreadedLoop);

    print("SimpleLoopWaitFor\n");
    unit::test<5>(unit::fixture<SimpleLoopWaitForCase>());

    print("SimpleLoopThreadRate\n");
    unit::test<6>(unit::fixture<SimpleLoopThreadRate>());

    print("RangeForZeroBoundNotKnown\n");
    unit::test<7>(RangeForZeroBoundNotKnown);

    print("RangeForShadowing\n");
    unit::test<8>(RangeForShadowing);

    print("UnorderedRangeFor\n");
    unit::test<9>(UnorderedRangeFor);

    print("NestedUnrolledFor\n");
    unit::test<10>(NestedUnrolledFor);

    print("UnrolledForTypeInference\n");
    unit::test<11>(UnrolledForTypeInference);

    print("RangeForSameVariableName\n");
    unit::test<12>(RangeForSameVariableName);

    print("UnrolledForSameVariableName\n");
    unit::test<13>(UnrolledForSameVariableName);

    print("PredicatedRangeFor\n");
    unit::test<14>(PredicatedRangeFor);
}
