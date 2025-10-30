// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner
import .options as opt

// Tests that run slowly in other files are placed here
// This file is kept small to avoid per-clock-cycle costs in the simulator that grow with the size of the IR
// transactional function

class Test638
{
private:
    uint32 g_currCallSite = 0;
    uint32 g_currTid = 0;

    bool[2] g_done638;
    uint32[2] g_dummy638 = {};

public:
    // [[last]] is explicitly not the last parameter, to test this scenario
    void Helper638(uint32 callSite, [[last]] bool eot, uint32 tid, unit::tag_t tag)
    {
        uint32 snappedCallSite;
        uint32 snappedTid;

        atomic
        {
            snappedCallSite = g_currCallSite;
            snappedTid = g_currTid;

            g_currTid = tid;
            g_currCallSite = callSite;
        }

        if (tid > 0)
        {
            unit::assert(tag, snappedCallSite == callSite);
            unit::assert(tag, tid == (snappedTid + 1));
        }
    }

    [[pipelined, async]] void CallSite1_638(uint32 tid, uint32 threadCount, unit::tag_t tag)
    {
        // Randomly stutter - to force interleaving
        for (const uint32 i : ((tid + 1) % 16))
        {
            atomic
            {
                g_dummy638[0]++;
            }
        }

        bool done = (tid == (threadCount - 1));

        [[transaction_size(opt::max_threads_limit)]] Helper638(1, done, tid, tag);

        if (done)
        {
            g_done638[0] = true;
        }
    }

    [[pipelined, async]] void CallSite2_638(uint32 tid, uint32 threadCount, unit::tag_t tag)
    {
        // Randomly stutter - to force interleaving
        for (const uint32 i : (tid % 16))
        {
            atomic
            {
                g_dummy638[1]++;
            }
        }

        bool done = (tid == (threadCount - 1));

        [[transaction_size(opt::max_threads_limit)]] Helper638(2, done, tid, tag);

        if (done)
        {
            g_done638[1] = true;
        }
    }

    void run(unit::tag_t tag)
    {
        static for(const auto i : 2)
        {
            g_done638[i] = false;
        }

        CallSite1_638(256, 256, tag);
        CallSite2_638(256, 256, tag);

        // Wait for all threads to finish
        static for(const auto i : 2)
        {
            atomic do; while(!g_done638[i]);
        }
    }
}

class TestCase651
{
private:
    // Decomposing comparisons (unsigned)
    [[pipelined]] void Helper651(uint8 tid, uint8 baseA, uint8 b, uint8 shift, unit::tag_t tag)
    {
        uint9 a = baseA + tid;

        // Some comparisons are converted to subtractions
        // These are wide enough that the carry chain will be decomposed
        uint64 shiftedA = static_cast(a << shift);
        uint64 shiftedB = static_cast(b << shift);

        bool a_lt_b = shiftedA < shiftedB;
        bool a_le_b = shiftedA <= shiftedB;

        bool a_gt_b = shiftedA > shiftedB;
        bool a_ge_b = shiftedA >= shiftedB;

        bool a_eq_b = shiftedA == shiftedB;
        bool a_ne_b = shiftedA != shiftedB;

        // (a < b) implies (a <= b)
        if (a_lt_b)
        {
            unit::assert(tag, a_le_b);
        }

        // (a > b) implies (a >= b)
        if (a_gt_b)
        {
            unit::assert(tag, a_ge_b);
        }

        // (a == b) implies !((a < b) || (a > b))
        // (a == b) implies (a <= b) and (a >= b)
        if (a_eq_b)
        {
            unit::assert(tag, !a_lt_b);
            unit::assert(tag, !a_gt_b);

            unit::assert(tag, a_le_b);
            unit::assert(tag, a_ge_b);
        }

        // (a != b) implies ((a < b) || (a > b))
        if (a_ne_b)
        {
            unit::assert(tag, a_lt_b || a_gt_b);
        }
    }

public:
    void run(unit::tag_t tag)
    {
        range_for (0, 200, 55, [tag](uint8 baseA)
        {
            range_for (0, 200, 45, [baseA, tag](uint8 b)
            {
                range_for (0, 64, 3, [baseA, b, tag](uint8 shift)
                {
                    Helper651(256, baseA, b, shift, tag);
                });
            });
        });
    }
}

class TestCase652
{
private:
    // Decomposing comparisons (signed)
    [[pipelined]] void Helper652(uint8 tid, int8 baseA, int8 b, uint8 shift, unit::tag_t tag)
    {
        int9 a = static_cast(baseA + tid);

        // Some comparisons are converted to subtractions
        // These are wide enough that the carry chain will be decomposed
        int64 shiftedA = static_cast(a << shift);
        int64 shiftedB = static_cast(b << shift);

        bool a_lt_b = shiftedA < shiftedB;
        bool a_le_b = shiftedA <= shiftedB;

        bool a_gt_b = shiftedA > shiftedB;
        bool a_ge_b = shiftedA >= shiftedB;

        bool a_eq_b = shiftedA == shiftedB;
        bool a_ne_b = shiftedA != shiftedB;

        // (a < b) implies (a <= b)
        if (a_lt_b)
        {
            unit::assert(tag, a_le_b);
        }

        // (a > b) implies (a >= b)
        if (a_gt_b)
        {
            unit::assert(tag, a_ge_b);
        }

        // (a == b) implies !((a < b) || (a > b))
        // (a == b) implies (a <= b) and (a >= b)
        if (a_eq_b)
        {
            unit::assert(tag, !a_lt_b);
            unit::assert(tag, !a_gt_b);

            unit::assert(tag, a_le_b);
            unit::assert(tag, a_ge_b);
        }

        // (a != b) implies ((a < b) || (a > b))
        if (a_ne_b)
        {
            unit::assert(tag, a_lt_b || a_gt_b);
        }
    }

public:
    void run(unit::tag_t tag)
    {
        range_for (-100, 100, 27, [tag](int8 baseA)
        {
            range_for (-100, 100, 24, [baseA, tag](int8 b)
            {
                range_for (0, 64, 3, [baseA, b, tag](uint8 shift)
                {
                    Helper652(256, baseA, b, shift, tag);
                });
            });
        });
    }
}

inline void test_main()
{
    unit::test<638>(unit::fixture<Test638>());
    unit::test<651>(unit::fixture<TestCase651>());
    unit::test<652>(unit::fixture<TestCase652>());
}
