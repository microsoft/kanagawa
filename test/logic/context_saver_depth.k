// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Test for sizing of context savers

import .options as opt
import test.unit as unit
import test.runner

// Context saver for a function with a single basic block with 64 stages in it
class BasicTest
{
private:
    noinline uint32 Helper(uint32 x)
    {
        stages<64>();
        return x + 1;
    }

public:
    void run(unit::tag_t tag)
    {
        uint64 start = cycles();

        pipelined_for(1024, [tag](uint32 tid)
        {
            uint32 result = Helper(tid);
            unit::assert(tag, result == (tid + 1));
        });

        uint64 elapsed = static_cast(cycles() - start);
        unit::assert(tag, elapsed < 1200);
    }
}

// Context saver for a function that contains a loop
// 2 basic blocks, each are long
class LoopTest
{
private:
    noinline uint32 LoopHelper(uint32 x)
    {
        uint32 result = x;

        do
        {
            stages<32>();
            result++;
        } while (x > 100000);

        stages<32>();
        result++;

        return result;
    }

public:
    void run(unit::tag_t tag)
    {
        uint64 start = cycles();

        pipelined_for(1024, [tag](uint32 tid)
        {
            uint32 result = LoopHelper(tid);
            unit::assert(tag, result == (tid + 2));
        });

        uint64 elapsed = static_cast(cycles() - start);
        unit::assert(tag, elapsed < 1200);
    }
}

// Context saver for a function that calls another function
class NestedTest
{
private:
    noinline uint32 Inner(uint32 x)
    {
        stages<32>();
        return x + 1;
    }

    noinline uint32 Outer(uint32 x)
    {
        stages<32>();
        return Inner(x) + 1;
    }

public:
    void run(unit::tag_t tag)
    {
        uint64 start = cycles();

        pipelined_for(1024, [tag](uint32 tid)
        {
            uint32 result = Outer(tid);
            unit::assert(tag, result == (tid + 2));
        });

        uint64 elapsed = static_cast(cycles() - start);
        unit::assert(tag, elapsed < 1200);
    }
}

// Ensures that context saver backpressure signals are connected
class BackpressureTest
{
private:
    noinline uint32 LoopHelper(uint32 x)
    {
        uint32 result = x;

        // Avoid filling up the entry fifo to this function
        // with a maximumally out-of-order loop
        // The pipeline here will not fill up because most threads
        // just go through one time.
        // The context saver will full up because thread0 blocks the head of the line.
        uint32 num_iterations = (x == 0) ? 1024 : 1;

        for (const auto i : num_iterations)
        {
            result++;
        }

        return result;
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(1024, [tag](uint32 tid)
        {
            uint32 result = LoopHelper(tid);

            uint32 expected = static_cast((tid == 0) ? (tid + 1024) : (tid + 1));

            unit::assert(tag, result == expected);
        });
    }
}

// Context saver sizing for a function call that contains atomic do
class WaitTestCase
{
private:
    uint32 _waitTestCount = 0;

public:
    void run(unit::tag_t tag)
    {
        uint64 startCycles = cycles();

        pipelined_for(1024, [](uint32 tid)
        {
            pipelined_for(2, [tid](uint32 tid2)
            {
                async_exec([]()
                {
                    // long latency here motivates a deep fifo for the atomic do
                    stages<200>();

                    atomic
                    {
                        _waitTestCount++;
                    }
                });

                atomic do; while(_waitTestCount < (tid * 2 + tid2));
            });
        });

        uint64 elapsedCycles = static_cast(cycles() - startCycles);

        unit::assert(tag, elapsedCycles < 2700);
    }
}

inline void test_main()
{
    if (opt::stall == 0)
    {
        print("BasicTest\n");
        unit::test<1>(unit::fixture<BasicTest>());

        print("LoopTest\n");
        unit::test<2>(unit::fixture<LoopTest>());

        print("NestedTest\n");
        unit::test<3>(unit::fixture<NestedTest>());

        print("BackpressureTest\n");
        unit::test<4>(unit::fixture<BackpressureTest>());

        print("WaitTestCase\n");
        unit::test<5>(unit::fixture<WaitTestCase>());
    }
    else
    {
        print("skipped due to stall {opt::stall}\n");
    }
}
