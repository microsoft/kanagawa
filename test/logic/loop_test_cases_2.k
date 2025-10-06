// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Range-for, bound is known at compile time
// Reorder buffer is removed, fifo between loop
// and successor to loop is made into passthrough
inline void TransitiveControlFlow(unit::tag_t tag)
{
    pipelined_for(512, [tag](uint9 tid)
    {
        uint32 result = tid;

        for (const auto i : 3)
        {
            result += 5;
        }

        unit::assert(tag, result == (tid + 15));
    });
}

// almost_full signal from loop successor should be routed
// to the loop start, and almost_full_threshold adjusted accordingly
template<auto LoopIterations, auto LoopPipelineDepth, auto SuccesorPipelineDepth>
inline void TransitiveControlFlowBackPressure(unit::tag_t tag)
{
    pipelined_for(4096, [tag](uint32 tid)
    {
        uint32 result = tid;

        for (const auto i : LoopIterations)
        {
            stages<LoopPipelineDepth>();

            result += 5;
        }

        stages<SuccesorPipelineDepth>();

        // random backpressure
        uint32 stallIterations = (tid * 7654321) % 64;

        uint32 expected = static_cast(tid + (LoopIterations * 5));

        for (const auto i : stallIterations)
        {
            // Something so that the loop is not optimized out
            unit::assert(tag, result == expected);
        }

        unit::assert(tag, result == expected);
    });
}

inline void TransitiveControlFlowBackPressure_3_2_2(unit::tag_t tag)  {TransitiveControlFlowBackPressure<3, 2, 2>(tag);}
inline void TransitiveControlFlowBackPressure_1_2_2(unit::tag_t tag)  {TransitiveControlFlowBackPressure<1, 2, 2>(tag);}
inline void TransitiveControlFlowBackPressure_3_2_10(unit::tag_t tag) {TransitiveControlFlowBackPressure<3, 2, 10>(tag);}

// Transitive control flow optimization should be disabled
// if successor block can introduce backpressure via atomic do
inline void TransitiveControlFlowWaitFor(unit::tag_t tag)
{
    pipelined_for(512, [tag](uint9 tid)
    {
        static bool _b = true;

        uint32 result = tid;

        for (const auto i : 3)
        {
            result += 5;
        }

        atomic do; while(!_b);

        unit::assert(tag, result == (tid + 15));
    });
}

class TransitiveControlFlowThreadRate
{
private:
    // Transitive control flow optimization should be disabled
    // if successor block has a thread rate != 1
    [[pipelined, thread_rate(2)]] void TransitiveControlFlowThreadRateHelper(uint9 tid, unit::tag_t tag)
    {
        uint32 result = tid;

        for (const auto i : 3)
        {
            result += 5;
        }

        unit::assert(tag, result == (tid + 15));
    }

public:
    void run(unit::tag_t tag)
    {
        TransitiveControlFlowThreadRateHelper(512, tag);
    }
}

class UnorderedBackwardLinkTest
{
private:
    // Verify that almost_full value from backward link fifo
    // does not cause a deadlock
    [[unordered, max_threads(4)]] uint32 UnorderedBackwardLinkHelper(uint32 x)
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
        pipelined_for(2048, [tag](uint32 id)
        {
            reorder
            {
                uint32 result = UnorderedBackwardLinkHelper(id);

                unit::assert(tag, result == ((id % 8) + 1));
            }
        });
    }
}

inline void test_main()
{
    print("TransitiveControlFlow\n");
    unit::test<1>(TransitiveControlFlow);

    print("TransitiveControlFlowWaitFor\n");
    unit::test<2>(TransitiveControlFlowWaitFor);

    print("TransitiveControlFlowThreadRate\n");
    unit::test<3>(unit::fixture<TransitiveControlFlowThreadRate>());

    print("TransitiveControlFlowBackPressure-3-2-2\n");
    unit::test<4>(TransitiveControlFlowBackPressure_3_2_2);

    print("TransitiveControlFlowBackPressure-1-2-2\n");
    unit::test<5>(TransitiveControlFlowBackPressure_1_2_2);

    print("TransitiveControlFlowBackPressure-3-2-10\n");
    unit::test<6>(TransitiveControlFlowBackPressure_3_2_10);

    print("UnorderedBackwardLinkTest\n");
    unit::test<7>(unit::fixture<UnorderedBackwardLinkTest>());
}
