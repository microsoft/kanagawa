// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.random.lfsr
import .options as opt

import test.unit as unit
import test.runner

// Uncalled pipelined function with a reference parameter
class UnreachablePipelinedHelper
{
public:
    void Test()
    {
        pipelined_for(8, [](uint32 tid)
        {
            println(tid);
        });
    }

    void run(unit::tag_t tag)
    {
        // do nothing
    }
}

// Inline function with multiple call sites
// that calls a function with reference parameters
class InlineCallsPipelinedForHelper
{
public:
    inline void load()
    {
        pipelined_for(64, [](uint6 i)
        {
        });
    }
}

inline void InlineCallsPipelinedFor(unit::tag_t tag)
{
    static InlineCallsPipelinedForHelper _helper;
    _helper.load();
    _helper.load();
}

// Calling pipeliend_last from within a method
class PipelinedLastWithinMethodHelper
{
private:
    uint32 _result;

public:
    void Test(unit::tag_t tag)
    {
        _result = 0;

        uint32 localResult = pipelined_last(8, [](uint32 tid)
        {
            _result = tid;

            return tid;
        });

        unit::assert(tag, _result == localResult);
        unit::assert(tag, localResult == 7);
    }
}

inline void PipelinedLastWithinMethod(unit::tag_t tag)
{
    static PipelinedLastWithinMethodHelper testObj;
    testObj.Test(tag);
}

// Test transformation that removes context saver predicate
// when thread count is a compile-time-known positive value
class LiteralThreadCount
{
private:
    uint32 _literalThreadCount = 0;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(16, [](index_t<16> id)
        {
            atomic
            {
                _literalThreadCount++;
            }
        });

        unit::assert(tag, _literalThreadCount == 16);
    }
}

// pipelined_with some thread counts = 0
// and some thread counts = N
// The context saver should be able to consume inner threads
// while dispatching outer threads with count = 0
inline void ThreadCountZeroTest(unit::tag_t tag)
{
    uint64 start = cycles();

    pipelined_for(256, [](uint32 tid)
    {
        uint32 thread_count = (0 == (tid % 8)) ? 8 : 0;

        pipelined_for(thread_count, [](uint32 inner_tid)
        {
            stages<32>();
        });
    });

    uint64 elapsed = cycles() - start;

    if ((opt::optimize > 0) && (opt::stall == 0))
    {
        unit::assert(tag, elapsed < 390);
    }
}

inline void PipelinedMapTest(unit::tag_t tag)
{
    uint64 start = cycles();

    pipelined_for(256, [tag](uint32 tid)
    {
        uint32[4] result = pipelined_map<4>(4, [tid](index_t<4> inner_tid)
        {
            return cast<uint32>(inner_tid + tid);
        });

        static for (const auto i : 4)
        {
            unit::assert(tag, result[i] == (tid + i));
        }
    });

    uint64 elapsed = cycles() - start;

    // Verify that pipelined_map returns 1 result every 4 cycles
    if ((opt::optimize > 0) && (opt::stall == 0))
    {
        unit::assert(tag, elapsed < 1100);
    }
}

class PredicatedPipelinedTest
{
private:
    uint32 _predicatedPipelinedTestCounter = 0;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(2, [](index_t<2> outer)
        {
            if (outer == 1)
            {
                pipelined_for(4, [](index_t<4> inner)
                {
                    atomic
                    {
                        _predicatedPipelinedTestCounter++;
                    }
                });
            }
        });

        unit::assert(tag, _predicatedPipelinedTestCounter == 4);
    }
}

inline void PipelinedMapVaryingThreadCountTest(unit::tag_t tag)
{
    pipelined_for(256, [tag](uint32 tid)
    {
        // Generate a random nunber that is <= 4
        static lfsr_random<3> lfsr;
        count_t<4> thread_count = lfsr.next({});

        if (thread_count > 4)
        {
            thread_count = thread_count - 4;
        }
        unit::assert(tag, thread_count <= 4);

        uint32[4] result = pipelined_map<4>(thread_count, [tid](index_t<4> inner_tid)
        {
            return cast<uint32>(inner_tid + tid);
        });

        static for (const auto i : 4)
        {
            if (i < thread_count)
            {
                unit::assert(tag, result[i] == (tid + i));
            }
        }
    });
}

class WideLoopCount
{
private:
    uint64 _wideLoopCountIterations = 0;

public:
    void run(unit::tag_t tag)
    {
        uint64 loopCount = 8;

        pipelined_for(loopCount, [](index_t<8> i)
        {
            atomic
            {
                _wideLoopCountIterations++;
            }
        });

        unit::assert(tag, _wideLoopCountIterations == 8);
    }
}

// Test for the pending count register inside of a context saver overflowing
class PendingCountOverflow
{
private:
    uint64 _pendingCountOverflowHelperTime = -1;

public:
    [[max_threads(16)]] void PendingCountOverflowHelper(uint5 outerTid)
    {
        bool isLastOuter = (outerTid == 31);

        uint4 threadCount = 15;

        // The context saver for this call must be able to represent 15*16=240 pending returns
        // that have finished executing the lambda but have not left the context saver
        pipelined_for(threadCount, [isLastOuter, threadCount, outerTid](index_t<16> tid)
        {
            print("{outerTid} {tid}\n");
        });

        // Stall for a while to create backpressure
        atomic do; while(cycles() < _pendingCountOverflowHelperTime);
        print("end waiting\n");
    }

    void run(unit::tag_t tag)
    {
        // Allow threads to exit the atomic do in 1000 cycles
        _pendingCountOverflowHelperTime = cycles() + 1000;

        pipelined_for(16, [](index_t<16> tid)
        {
            PendingCountOverflowHelper(tid);
        });
    }
}

inline void test_main()
{
    print("InlineCallsPipelinedFor\n");
    unit::test<1>(InlineCallsPipelinedFor);

    print("PipelinedLastWithinMethod\n");
    unit::test<2>(PipelinedLastWithinMethod);

    print("LiteralThreadCount\n");
    unit::test<3>(unit::fixture<LiteralThreadCount>());

    print("ThreadCountZeroTest\n");
    unit::test<4>(ThreadCountZeroTest);

    print("PipelinedMapTest\n");
    unit::test<5>(PipelinedMapTest);

    print("PredicatedPipelinedTest\n");
    unit::test<6>(unit::fixture<PredicatedPipelinedTest>());

    print("PipelinedMapVaryingThreadCountTest\n");
    unit::test<7>(PipelinedMapVaryingThreadCountTest);

    print("WideLoopCount\n");
    unit::test<8>(unit::fixture<WideLoopCount>());

    print("PendingCountOverflow\n");
    unit::test<9>(unit::fixture<PendingCountOverflow>());

    print("UnreachablePipelinedHelper\n");
    unit::test<10>(unit::fixture<UnreachablePipelinedHelper>());
}
