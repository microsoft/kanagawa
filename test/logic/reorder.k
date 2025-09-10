// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import control.loop
import numeric.int
import numeric.int.operator
import sync.atomic
import test.unit as unit
import test.runner

const auto PIPELINED_ITERATIONS = 16;
const auto LOOP_ITERATIONS = 8;

class test_reorder_by_looping_conditional_loop
{
private:
    uint32[PIPELINED_ITERATIONS] g_counters;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(PIPELINED_ITERATIONS, [tag](index_t<PIPELINED_ITERATIONS> tid)
        {
            g_counters[tid] = 0;

            if (even(tid))
            {
                uint32 x = 0;
                [[reorder_by_looping]]
                do
                {
                    g_counters[tid]++;
                    x++;
                } while (x < LOOP_ITERATIONS);

                // Check value of local
                unit::assert_equal(tag, LOOP_ITERATIONS, x);
            }

            // Check value of global
            unit::assert_equal(tag, even(tid) ? LOOP_ITERATIONS : 0, g_counters[tid]);

            // Check that threads are exiting in order
            uint32 expected_tid = first(atomically<uint32, 0>(increment));
            unit::assert_equal(tag, expected_tid, tid);
        });
    }
}

// Test for the case where loop executions that are predicated off
// would always compute a true loop condition (indicating that the backward link should always be taken)
// Loop conditions should be ignored for executions which are predicated off.
inline void test_reorder_by_looping_conditional_loop_ignore_predicated_locals(unit::tag_t tag)
{
    pipelined_for(PIPELINED_ITERATIONS, [tag](index_t<PIPELINED_ITERATIONS> tid)
    {
        if (even(tid))
        {
            uint32 x = 0;
            [[reorder_by_looping]]
            do
            {
                x++;
            } while ((x < LOOP_ITERATIONS) || odd(tid));

            // Check value of local
            unit::assert_equal(tag, LOOP_ITERATIONS, x);
        }

        // Check that threads are exiting in order
        uint32 expected_tid = first(atomically<uint32, 0>(increment));
        unit::assert_equal(tag, expected_tid, tid);
    });
}


inline void test_main()
{
    print("test_reorder_by_looping_conditional_loop\n");
    unit::test<1>(unit::fixture<test_reorder_by_looping_conditional_loop>());

    print("test_reorder_by_looping_conditional_loop_ignore_predicated_locals\n");
    unit::test<2>(test_reorder_by_looping_conditional_loop_ignore_predicated_locals);
}
