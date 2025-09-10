// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Test for large UpdateRate causing bypass circuits to be disabled
template<uint3 UpdateRate>
class BypassHelper662
{
private:
    [[memory]] uint32[512] _data;
    uint32 _ctr;

public:
    [[pipelined]] void Clear(uint32 tid)
    {
        _ctr = 0;
        _data[tid] = 0;
    }

    uint32 Get(uint32 idx)
    {
        return _data[idx];
    }

    [[pipelined]] void Run(uint32 tid, uint32 modulus)
    {
        // Determine which index to use
        uint32 index;

        atomic
        {
            index = _ctr;

            _ctr = (_ctr == (modulus - 1)) ? 0 : (_ctr + 1);
        }

        [[schedule( UpdateRate)]]
        {
            auto val = _data[index];
            val++;
            _data[index] = val;
        }
    }
}

class TestCase662
{
private:
    BypassHelper662<1> g_helper662_1;
    BypassHelper662<2> g_helper662_2;
    BypassHelper662<3> g_helper662_3;
    BypassHelper662<4> g_helper662_4;

public:
    void run(unit::tag_t tag)
    {
        // Update Rate = 1
        {
            // Each thread updates the same slot
            g_helper662_1.Clear(512);
            g_helper662_1.Run(2048, 1);
            unit::assert(tag, 2048 == g_helper662_1.Get(0));

            // Index alternates between 2 different slots
            g_helper662_1.Clear(512);
            g_helper662_1.Run(2048, 2);
            unit::assert(tag, 1024 == g_helper662_1.Get(0));
            unit::assert(tag, 1024 == g_helper662_1.Get(1));

            // Index alternates between 3 different slots
            g_helper662_1.Clear(512);
            g_helper662_1.Run(2048, 3);
            unit::assert(tag, 683 == g_helper662_1.Get(0));
            unit::assert(tag, 683 == g_helper662_1.Get(1));
            unit::assert(tag, 682 == g_helper662_1.Get(2));

            // Index alternates between 4 different slots
            g_helper662_1.Clear(512);
            g_helper662_1.Run(2048, 4);
            unit::assert(tag, 512 == g_helper662_1.Get(0));
            unit::assert(tag, 512 == g_helper662_1.Get(1));
            unit::assert(tag, 512 == g_helper662_1.Get(2));
            unit::assert(tag, 512 == g_helper662_1.Get(3));
        }

        // Update Rate = 2
        {
            // Index alternates between 2 different slots
            g_helper662_2.Clear(512);
            g_helper662_2.Run(2048, 2);
            unit::assert(tag, 1024 == g_helper662_2.Get(0));
            unit::assert(tag, 1024 == g_helper662_2.Get(1));

            // Index alternates between 3 different slots
            g_helper662_2.Clear(512);
            g_helper662_2.Run(2048, 3);
            unit::assert(tag, 683 == g_helper662_2.Get(0));
            unit::assert(tag, 683 == g_helper662_2.Get(1));
            unit::assert(tag, 682 == g_helper662_2.Get(2));

            // Index alternates between 4 different slots
            g_helper662_2.Clear(512);
            g_helper662_2.Run(2048, 4);
            unit::assert(tag, 512 == g_helper662_2.Get(0));
            unit::assert(tag, 512 == g_helper662_2.Get(1));
            unit::assert(tag, 512 == g_helper662_2.Get(2));
            unit::assert(tag, 512 == g_helper662_2.Get(3));
        }

        // Update Rate = 3
        {
            // Index alternates between 3 different slots
            g_helper662_3.Clear(512);
            g_helper662_3.Run(2048, 3);
            unit::assert(tag, 683 == g_helper662_3.Get(0));
            unit::assert(tag, 683 == g_helper662_3.Get(1));
            unit::assert(tag, 682 == g_helper662_3.Get(2));

            // Index alternates between 4 different slots
            g_helper662_3.Clear(512);
            g_helper662_3.Run(2048, 4);
            unit::assert(tag, 512 == g_helper662_3.Get(0));
            unit::assert(tag, 512 == g_helper662_3.Get(1));
            unit::assert(tag, 512 == g_helper662_3.Get(2));
            unit::assert(tag, 512 == g_helper662_3.Get(3));
        }

        // Update Rate = 4
        {
            // Index alternates between 4 different slots
            g_helper662_4.Clear(512);
            g_helper662_4.Run(2048, 4);
            unit::assert(tag, 512 == g_helper662_4.Get(0));
            unit::assert(tag, 512 == g_helper662_4.Get(1));
            unit::assert(tag, 512 == g_helper662_4.Get(2));
            unit::assert(tag, 512 == g_helper662_4.Get(3));
        }
    }
}

// repro of a bug related to shifting large values
inline uint32[8] Helper664(uint256 a)
{
    uint32[8] b;

    static for(const auto i: 8)
    {
        b[i] = a >> (i * 32);
    }

    return b;
}

inline void TestCase664(unit::tag_t tag)
{
    uint256 a = 0x98456123deadcafeabcd1234;

    uint32[8] b = Helper664(a);

    unit::assert(tag, b[0] == 0xabcd1234);
    unit::assert(tag, b[1] == 0xdeadcafe);
    unit::assert(tag, b[2] == 0x98456123);
}

// Pipelined call with return value
class TestCase666
{
private:
    uint32 g_numHelper666InnerCalls = 0;

    [[pipelined]] uint32 Helper666Inner(uint16 tid)
    {
        // Count the number of calls, to ensure it is not affected by the return array size
        atomic
        {
            g_numHelper666InnerCalls++;
        }

        return tid + 3;
    }

    void Helper666(uint16 threadCount, unit::tag_t tag)
    {
        g_numHelper666InnerCalls = 0;

        uint32[4] result = Helper666Inner(threadCount);

        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper666InnerCalls == threadCount);

        // Verify returned values
        static for(const auto i : 4)
        {
            uint32 expected;

            if (i < threadCount)
            {
                unit::assert(tag, (i + 3) == result[i]);
            }
        }
    }

public:
    void run(unit::tag_t tag)
    {
        for (const auto threadCount : 5)
        {
            Helper666(threadCount, tag);
        }
    }
}

// Pipelined call with return value that is ignored
class TestCase667
{
private:
    uint32 g_numHelper667InnerCalls = 0;

    [[pipelined]] uint32 Helper667Inner(uint16 tid)
    {
        // Count the number of calls, to ensure it is not affected by the return array size
        atomic
        {
            g_numHelper667InnerCalls++;
        }

        return tid + 5;
    }

    void Helper667(uint16 threadCount, unit::tag_t tag)
    {
        g_numHelper667InnerCalls = 0;

        Helper667Inner(threadCount);

        unit::assert(tag, g_numHelper667InnerCalls == threadCount);
    }

public:
    void run(unit::tag_t tag)
    {
        for (const uint16 threadCount : 8)
        {
            Helper667(threadCount, tag);
        }
    }
}

// Pipelined call with return value that is ignored - thread count known at compile time
class TestCase668
{
private:
    uint32 g_numHelper668InnerCalls = 0;

    [[pipelined]] uint32 Helper668Inner(uint16 tid)
    {
        // Count the number of calls, to ensure it is not affected by the return array size
        atomic
        {
            g_numHelper668InnerCalls++;
        }

        return tid + 5;
    }

public:
    void run(unit::tag_t tag)
    {
        g_numHelper668InnerCalls = 0;

        Helper668Inner(8);

        unit::assert(tag, g_numHelper668InnerCalls == 8);
    }
}

// Pipelined function with return value that is called from multiple call sites
// some call sites have known thread counts, others do not
// some call sites read the return value, others do not
class TestCase669
{
private:
    uint32 g_numHelper669InnerCalls = 0;

    [[pipelined]] uint32 Helper669Inner(uint16 tid)
    {
        // Count the number of calls, to ensure it is not affected by the return array size
        atomic
        {
            g_numHelper669InnerCalls++;
        }

        return tid + 3;
    }

    // Thread count not known at compile time, return values used
    void Helper669_1(uint16 threadCount, unit::tag_t tag)
    {
        g_numHelper669InnerCalls = 0;

        uint32[4] result = Helper669Inner(threadCount);

        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper669InnerCalls == threadCount);

        // Verify returned values
        static for(const auto i : 4)
        {
            uint32 expected;

            if (i < threadCount)
            {
                unit::assert(tag, (i + 3) == result[i]);
            }
        }
    }

    // Thread count not known at compile time, return values ignored
    void Helper669_2(uint16 threadCount, unit::tag_t tag)
    {
        g_numHelper669InnerCalls = 0;

        Helper669Inner(threadCount);

        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper669InnerCalls == threadCount);
    }

    // Thread count (large) not known at compile time, return values used
    void Helper669_3(unit::tag_t tag)
    {
        uint16 threadCount = 4;

        g_numHelper669InnerCalls = 0;

        uint32[4] result = Helper669Inner(threadCount);

        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper669InnerCalls == threadCount);

        // Verify returned values
        static for(const auto i : 4)
        {
            uint32 expected;

            if (i < threadCount)
            {
                unit::assert(tag, (i + 3) == result[i]);
            }
        }
    }

    // Thread count (small) not known at compile time, return values used
    void Helper669_4(unit::tag_t tag)
    {
        uint16 threadCount = 2;

        g_numHelper669InnerCalls = 0;

        uint32[4] result = Helper669Inner(threadCount);

        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper669InnerCalls == threadCount);

        // Verify returned values
        static for(const auto i : 4)
        {
            uint32 expected;

            if (i < threadCount)
            {
                unit::assert(tag, (i + 3) == result[i]);
            }
        }
    }

public:
    void run(unit::tag_t tag)
    {
        for (const auto threadCount : 5)
        {
            Helper669_1(threadCount, tag);
            Helper669_2(threadCount, tag);
        }

        Helper669_3(tag);
        Helper669_4(tag);
    }
}

// Pipelined call with return value - called in a cast expression
class TestCase670
{
private:
    uint32 g_numHelper670InnerCalls = 0;

    [[pipelined]] uint32 Helper670Inner(uint16 tid)
    {
        // Count the number of calls, to ensure it is not affected by the return array size
        atomic
        {
            g_numHelper670InnerCalls++;
        }

        return tid + 3;
    }

    void Helper670(uint16 threadCount, unit::tag_t tag)
    {
        g_numHelper670InnerCalls = 0;

        uint32[4] result = cast<uint32[4]>(Helper670Inner(threadCount));

        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper670InnerCalls == threadCount);

        // Verify returned values
        static for(const auto i : 4)
        {
            uint32 expected;

            if (i < threadCount)
            {
                unit::assert(tag, (i + 3) == result[i]);
            }
        }
    }

public:
    void run(unit::tag_t tag)
    {
        for (const auto threadCount : 5)
        {
            Helper670(threadCount, tag);
        }
    }
}

// Pipelined call with return value - used in a function call
class TestCase671
{
private:
    uint32 g_numHelper671InnerCalls = 0;

    [[pipelined]] uint32 Helper671Inner(uint16 tid)
    {
        // Count the number of calls, to ensure it is not affected by the return array size
        atomic
        {
            g_numHelper671InnerCalls++;
        }

        return tid + 3;
    }

    void CheckResult671(uint16 threadCount, uint32[4] result, unit::tag_t tag)
    {
        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper671InnerCalls == threadCount);

        // Verify returned values
        static for(const auto i : 4)
        {
            uint32 expected;

            if (i < threadCount)
            {
                unit::assert(tag, (i + 3) == result[i]);
            }
        }
    }

    void Helper671(uint16 threadCount, unit::tag_t tag)
    {
        g_numHelper671InnerCalls = 0;

        CheckResult671(threadCount, Helper671Inner(threadCount), tag);
    }

public:
    void run(unit::tag_t tag)
    {
        for (const auto threadCount : 5)
        {
            Helper671(threadCount, tag);
        }
    }
}

// Pipelined call with return value - used in an inline function call
class TestCase672
{
private:
    uint32 g_numHelper672InnerCalls = 0;

    [[pipelined]] uint32 Helper672Inner(uint16 tid)
    {
        // Count the number of calls, to ensure it is not affected by the return array size
        atomic
        {
            g_numHelper672InnerCalls++;
        }

        return tid + 3;
    }

    inline void CheckResult672(uint16 threadCount, uint32[4] result, unit::tag_t tag)
    {
        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper672InnerCalls == threadCount);

        // Verify returned values
        static for(const auto i : 4)
        {
            uint32 expected;

            if (i < threadCount)
            {
                unit::assert(tag, (i + 3) == result[i]);
            }
        }
    }

    void Helper672(uint16 threadCount, unit::tag_t tag)
    {
        g_numHelper672InnerCalls = 0;

        CheckResult672(threadCount, Helper672Inner(threadCount), tag);
    }

public:
    void run(unit::tag_t tag)
    {
        for (const auto threadCount : 5)
        {
            Helper672(threadCount, tag);
        }
    }
}


// Pipelined call with return value
// The call to the pipelined function is inside a return expression
class TestCase673
{
private:
    uint32 g_numHelper673InnerCalls = 0;

    [[pipelined]] uint32 Helper673Inner(uint16 tid)
    {
        // Count the number of calls, to ensure it is not affected by the return array size
        atomic
        {
            g_numHelper673InnerCalls++;
        }

        return tid + 3;
    }

    uint32[4] Helper673_2(uint16 threadCount)
    {
        return Helper673Inner(threadCount);
    }

    void Helper673(uint16 threadCount, unit::tag_t tag)
    {
        g_numHelper673InnerCalls = 0;

        uint32[4] result = Helper673_2(threadCount);

        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper673InnerCalls == threadCount);

        // Verify returned values
        static for(const auto i : 4)
        {
            uint32 expected;

            if (i < threadCount)
            {
                unit::assert(tag, (i + 3) == result[i]);
            }
        }
    }

public:
    void run(unit::tag_t tag)
    {
        for (const auto threadCount : 5)
        {
            Helper673(threadCount, tag);
        }
    }
}

// Pipelined call with return value - used in a cast
class TestCase674
{
private:
    uint32 g_numHelper674InnerCalls = 0;

    [[pipelined]] uint32 Helper674Inner(uint16 tid)
    {
        // Count the number of calls, to ensure it is not affected by the return array size
        atomic
        {
            g_numHelper674InnerCalls++;
        }

        return tid + 3;
    }

public:
    void run(unit::tag_t tag)
    {
        g_numHelper674InnerCalls = 0;

        const auto threadCount = 4;

        uint64[2] resultWide = cast<uint64[2]>(Helper674Inner(threadCount));

        uint32[4] result = cast<uint32[4]>(resultWide);

        // Verify the correct number of threads ran
        unit::assert(tag, g_numHelper674InnerCalls == threadCount);

        // Verify returned values
        static for(const auto i : 4)
        {
            uint32 expected;

            if (i < threadCount)
            {
                unit::assert(tag, (i + 3) == result[i]);
            }
        }
    }
}

// Testing async pipelined calls
class TestCase663
{
    uint32 g_numThreads663;

    [[pipelined, async]] void Helper663(uint32 tid)
    {
        atomic
        {
            g_numThreads663++;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        for (const uint32 threadCount : 10)
        {
            g_numThreads663 = 0;

            Helper663(threadCount);

            atomic do; while(!(g_numThreads663 == threadCount));
        }
    }
}


// Loop followed by wait for
// Repro of a bug that would cause the reorder buffer to be disabled
class TestCase665
{
    uint32 g_numLoopIterations665 = 0;

    uint32 g_numThreadsCompleted665 = 0;

    [[pipelined]] void Helper665(uint32 tid)
    {
        // Loop in such a way that threads will run out of order
        uint32 bound = 128 - tid;

        for (const uint32 i : bound)
        {
            // Future-proofing, to ensure the loop isn't optimized out
            atomic
            {
                g_numLoopIterations665++;
            }
        }

        atomic do; while(!(g_numLoopIterations665 > 0));

        uint32 expectedTid;

        atomic
        {
            expectedTid = g_numThreadsCompleted665;

            g_numThreadsCompleted665++;
        }

        assert(expectedTid == tid);
    }

public:
    void run(unit::tag_t tag)
    {
        Helper665(64);
    }
}

inline void test_main()
{
    unit::test<662>(unit::fixture<TestCase662>());
    unit::test<663>(unit::fixture<TestCase663>());
    unit::test<664>(TestCase664);
    unit::test<665>(unit::fixture<TestCase665>());
    unit::test<666>(unit::fixture<TestCase666>());
    unit::test<667>(unit::fixture<TestCase667>());
    unit::test<668>(unit::fixture<TestCase668>());
    unit::test<669>(unit::fixture<TestCase669>());
    unit::test<670>(unit::fixture<TestCase670>());
    unit::test<671>(unit::fixture<TestCase671>());
    unit::test<672>(unit::fixture<TestCase672>());
    unit::test<673>(unit::fixture<TestCase673>());
    unit::test<674>(unit::fixture<TestCase674>());
}
