// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner
import .options as opt

// pipelined function call with thread count = (1 << threadcountwidth)
class TestCase643
{
private:
    uint32 g_sum643 = 0;

    [[pipelined]] void Helper643(uint8 tid)
    {
        atomic
        {
            g_sum643++;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        Helper643(256);
        unit::assert(tag, g_sum643 == 256);
    }
}

// memory update with [[schedule(N)]]
class TestCase645
{
private:
    [[memory]] uint32[2] g_mem645;

    [[pipelined]] void Init645(uint32 tid)
    {
        g_mem645[tid] = tid;
    }

    [[pipelined]] void Validate645(uint32 tid, uint32 totalThreadCount, unit::tag_t tag)
    {
        uint32 actual = g_mem645[tid];
        uint32 expected = tid + totalThreadCount;
        unit::assert(tag, actual == expected);
    }

    [[pipelined]] void Helper645(uint32 tid)
    {
        uint32 idx = tid % 2;

        [[schedule( 2)]]
        {
            uint32 val = g_mem645[idx];
            val++;
            g_mem645[idx] = val;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        Init645(2);

        Helper645(512);

        Validate645(2, 512 / 2, tag);
    }
}

// memory update with [[schedule(N)]]
class TestCase646
{
private:
    memory<uint32, 512>[4] g_mem646;

    [[pipelined]] void Init646(uint32 tid)
    {
        static for(const auto i : 4)
        {
            g_mem646[i][tid] = tid + i;
        }
    }

    [[pipelined]] void Validate646(uint32 tid, uint32 totalThreadCount, unit::tag_t tag)
    {
        static for(const auto i : 4)
        {
            uint32 actual = g_mem646[i][tid];
            uint32 expected = tid + i + totalThreadCount;
            unit::assert(tag, actual == expected);
        }
    }

    [[pipelined]] void Helper646(uint32 tid)
    {
        uint32 idx = tid % 2;

        uint32[4] indices;

        static for(const auto i : 4)
        {
            indices[i] = idx;
        }

        [[schedule(2)]]
        {
            uint32[4] values;
            static for(const auto i : 4)
            {
                values[i] = g_mem646[i][indices[i]];
            }
            static for(const auto i : 4)
            {
                values[i]++;
            }
            static for(const auto i : 4)
            {
                g_mem646[i][indices[i]] = values[i];
            }
        }
    }

public:
    void run(unit::tag_t tag)
    {
        Init646(2);

        Helper646(512);

        Validate646(2, 512 / 2, tag);
    }
}


// BAD [[schedule(N)]], testing for incorrect value
class TestCase647
{
private:
    [[memory]] uint32[2] g_mem647;

    [[pipelined]] void Init647(uint32 tid)
    {
        g_mem647[tid] = tid;
    }

    [[pipelined]] void Validate647(uint32 tid, uint32 totalThreadCount, unit::tag_t tag)
    {
        uint32 actual = g_mem647[tid];
        uint32 expected = tid + totalThreadCount*4; // this would be *8 if the update rate was correct
        unit::assert(tag, actual == expected);
    }

    [[pipelined]] void Helper647(uint32 tid)
    {
        uint32 idx = tid % 2;

        [[schedule( 4)]]
        {
            uint32 val = g_mem647[idx];
            static for(const auto i : 8)
            {
                val++;
            }
            g_mem647[idx] = val;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        Init647(2);

        Helper647(512);

        Validate647(2, 512 / 2, tag);
    }
}

// Decomposing additions/subtractions (unsigned, mismatching widths)
inline void Helper653(uint64 a, uint9 b, uint64 expectedSum, uint64 expectedDiff, unit::tag_t tag)
{
    uint64 sum = a + b;
    uint64 diff = a - b;

    unit::assert(tag, expectedSum == sum);
    unit::assert(tag, expectedDiff == diff);
}

inline void TestCase653(unit::tag_t tag)
{
    Helper653(0xDEADBEEF, 37, 0xDEADBF14, 0xDEADBECA, tag);
    Helper653(36, 37, 73, 0xFFFFFFFFFFFFFFFF, tag);
}

// Decomposing additions/subtractions (signed, mismatching widths)
inline void Helper654(int64 a, int9 b, int64 expectedSum, int64 expectedDiff, unit::tag_t tag)
{
    int64 sum = a + b;
    int64 diff = a - b;

    unit::assert(tag, expectedSum == sum);
    unit::assert(tag, expectedDiff == diff);
}

inline void TestCase654(unit::tag_t tag)
{
    Helper654(123456789, 37, 123456826, 123456752, tag);
    Helper654(123456789, -37, 123456752, 123456826, tag);
}

// 6-bit wide equality comparison (to test reduction tree that is naturally aligned)
inline void Helper655(uint6 a, uint6 b, bool expected, unit::tag_t tag)
{
    bool eq = (a == b);
    bool ne = (a != b);

    unit::assert(tag, expected == eq);
    unit::assert(tag, expected == !ne);
}

inline void TestCase655(unit::tag_t tag)
{
    for (const uint32 i : 100)
    {
        Helper655(i, i, true, tag);
        Helper655(i, i - 1, false, tag);
        Helper655(i, i - 2, false, tag);
        Helper655(i, i + 1, false, tag);
        Helper655(i, i + 2, false, tag);
    }
}

inline void test_main()
{
    unit::test<643>(unit::fixture<TestCase643>());
    unit::test<645>(unit::fixture<TestCase645>());
    unit::test<646>(unit::fixture<TestCase646>());

    if (opt::stall == 0)
    {
        // This test reads a shared variable every 2 cycles but updates it
        // every 4 cycles -- the expected value is only correct if there are no stalls
        unit::test<647>(unit::fixture<TestCase647>());
    }
    else
    {
        print("  Skipped due to --stall > 0\n");
    }

    unit::test<653>(TestCase653);
    unit::test<654>(TestCase654);
    unit::test<655>(TestCase655);
}
