// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

class TestAtomicWait
{
public:
    uint32 g_var;

    void run(unit::tag_t tag)
    {
        g_var = 0;

        async_exec([]()
        {
            pipelined_for(32, [](uint32 tid)
            {
                atomic
                {
                    g_var++;
                }
            });
        });

        wait([]()
        {
            return g_var == 32;
        });

        unit::assert(tag, g_var == 32);
    }
}

class TestAtomicWaitResult
{
public:
    uint32 g_var;

    void run(unit::tag_t tag)
    {
        g_var = 0;

        async_exec([]()
        {
            pipelined_for(32, [](uint32 tid)
            {
                atomic
                {
                    g_var++;
                }
            });
        });

        uint32 result = wait([]()
        {
            return make_optional(g_var == 32, g_var);
        });

        unit::assert_equal(tag, 32, result);
        unit::assert(tag, g_var == 32);
    }
}

// Optimization that avoids inserting a new basic block for `wait` if
// all operations before the `wait` can be folded in to the `wait`
inline void TestMerge(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint5 tid)
    {
        static uint32 _sum = 0;

        // Zero-cost operation should not cause a new basic block
        uint32 wide_tid = tid;

        wait([wide_tid, _sum]()
        {
            return _sum <= wide_tid;
        });

        atomic
        {
            unit::assert_equal(tag, tid, _sum);
            _sum++;
        }

        unit::assert_equal(tag, tid, wide_tid);
    });
}

// Wait for with a preceding atomic block
inline void TestMergeWithAtomic(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint5 tid)
    {
        static uint32 _sum = 0;

        uint32 snapped_sum;

        atomic
        {
            snapped_sum = _sum;
        }

        wait([snapped_sum, _sum]()
        {
            return snapped_sum <= _sum;
        });

        atomic
        {
            unit::assert(tag, snapped_sum <= _sum);
            unit::assert_equal(tag, tid, _sum);
            _sum++;
        }
    });
}

class TestSideEffect
{
public:
    bool g_done;
    uint32 g_side_effect_counter;

    void run(unit::tag_t tag)
    {
        g_done = false;
        g_side_effect_counter = 0;

        async_exec([]()
        {
            const auto N = 4096;
            pipelined_for(N, [](uint32 tid)
            {
                if (tid == N - 1)
                {
                    g_done = true;
                }
            });
        });

        // atomic do condition with global side effect
        inline bool helper()
        {
            g_side_effect_counter++;
            return g_done;
        }

        atomic do {} while (!helper());

        unit::assert(tag, g_side_effect_counter > 1);
    }
}

class TestLoop
{
public:
    uint32 g_side_effect_counter;

    void run(unit::tag_t tag)
    {
        g_side_effect_counter = 0;

        for (const auto i : 4)
        {
            // Test atomic do at start of loop. Start of loop has multiple FIFOs which
            // generates Verilog differently than case with single FIFO.
            atomic do
            {
                g_side_effect_counter++;
            } while (g_side_effect_counter < 100 * (i + 1));

            unit::assert_equal(tag, 100 * (i + 1), g_side_effect_counter);
        }
    }
}

class TestPipelinedAtomicDo
{
public:
    uint32 g_wait_gate;
    uint32 g_wait_finished_count;

    // Test back-to-back pipelined threads using `atomic do`. This tests that
    // side effects are correct, even in the situation of compiler-inserted stalls.
    void run(unit::tag_t tag)
    {
        g_wait_gate = 0;
        g_wait_finished_count = 0;

        async_exec([]()
        {
            for (const auto i : 512)
            {
                atomic
                {
                    g_wait_gate++;
                }
            }
        });

        pipelined_for(512, [tag](uint32 tid)
        {
            bool result;
            atomic do
            {
                result = tid < g_wait_gate;

                if (result)
                {
                    unit::assert_equal(tag, tid, g_wait_finished_count);
                    g_wait_finished_count = g_wait_finished_count + 1;
                }
            } while (!result)
        });

        unit::assert_equal(tag, 512, g_wait_finished_count);
    }
}

inline void TestLocals(unit::tag_t tag)
{
    // Test multiple threads to check that each thread intializes locals
    // to the correct value when starting the atomic do loop
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 w = 0;
        uint32 x = 0;
        // Test that initial values of local variable are correctly used
        uint32 y = 15;
        uint32 z = tid;

        atomic do
        {
            // Increments to local variables should be stateful across loop
            // iterations
            w++;
            if (w % 2 == 0)
            {
                x++;
            }
            y++;
            z++;
        } while (w < 10)

        unit::assert_equal(tag, 10, w);
        unit::assert_equal(tag, 5, x);
        unit::assert_equal(tag, 25, y);
        unit::assert_equal(tag, tid + 10, z);
    });
}

// Regression set for atomic loop which modifies local variations
// immediately after a non-inline call
class TestAtomicDoAfterNonInlineCall
{
private:
    noinline uint32 Foo(uint32 x)
    {
        return x + 1;
    }
public:
    void run(unit::tag_t tag)
    {
        uint32 a = Foo(4);

        atomic do
        {
            a--;
        } while(a > 1);

        unit::assert_equal(tag, 1, a);
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<TestAtomicWait>());
    unit::test<2>(unit::fixture<TestAtomicWaitResult>());
    unit::test<3>(TestMerge);
    unit::test<4>(TestMergeWithAtomic);
    unit::test<5>(unit::fixture<TestSideEffect>());
    unit::test<6>(unit::fixture<TestLoop>());
    unit::test<7>(unit::fixture<TestPipelinedAtomicDo>());
    unit::test<8>(TestLocals);
    unit::test<9>(unit::fixture<TestAtomicDoAfterNonInlineCall>());
}
