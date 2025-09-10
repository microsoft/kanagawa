// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import control.loop
import test.unit as unit
import test.runner

template<auto OuterUpdateRate, auto InnerUpdateRate>
class NestedAtomic
{
private:
    const auto ARRAY_SIZE = 4;
    uint32[ARRAY_SIZE] g_array;

public:
    void run(unit::tag_t tag)
    {
        static assert(OuterUpdateRate <= ARRAY_SIZE);

        // Initialize array to 0
        pipelined_for(ARRAY_SIZE, [](index_t<ARRAY_SIZE> tid)
        {
            g_array[tid] = 0;
        });

        pipelined_for(256, [tag](index_t<256> tid)
        {
            index_t<ARRAY_SIZE> array_index = tid % OuterUpdateRate;

            uint32 x;
            [[schedule(OuterUpdateRate)]]
            {
                [[schedule(InnerUpdateRate)]]
                {
                    // Read-modify-write of array. schedule(OuterUpdateRate)
                    // prevents collisions.
                    x = g_array[array_index];
                    x++;
                    g_array[array_index] = x;
                }
            }
            unit::assert_equal(tag, (tid / OuterUpdateRate) + 1, x);
        });
    }
}

// Test accesses to shared variable in non-leaf atomic block. Check that
// shared access rewriting works correctly so that read after read-modify-write
// gets the value after the write.
class NonLeafSharedAccess
{
private:
    uint32 g_var = 15;

public:
    void run(unit::tag_t tag)
    {
        uint32[5] x;
        [[schedule(5)]]
        {
            g_var++;
            x[0] = g_var;

            atomic
            {
                g_var++;
                x[1] = g_var;
            }

            g_var++;
            x[2] = g_var;

            atomic
            {
                g_var++;
                x[3] = g_var;
            }

            g_var++;
            x[4] = g_var;
        }
        unit::assert_equal(tag, 16, x[0]);
        unit::assert_equal(tag, 17, x[1]);
        unit::assert_equal(tag, 18, x[2]);
        unit::assert_equal(tag, 19, x[3]);
        unit::assert_equal(tag, 20, x[4]);
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<NestedAtomic<1, 1>>());
    unit::test<2>(unit::fixture<NestedAtomic<2, 3>>());
    unit::test<3>(unit::fixture<NestedAtomic<4, 2>>());
    unit::test<4>(unit::fixture<NonLeafSharedAccess>());
}
