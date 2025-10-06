// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import .options as opt

import test.unit as unit
import test.runner

inline void Test(unit::tag_t tag)
{
    if (opt::stall != 0)
    {
        print("StallTest\n");
        const auto StallThreads = 128;
        pipelined_for(StallThreads,
            [StallThreads, tag](index_t<StallThreads> tid)
            {
                atomic
                {
                    static uint64 t0;
                    auto c = cycles();
                    if (tid == 0)
                    {
                        t0 = c;
                    }
                    if (tid == StallThreads - 1)
                    {
                        // Assert that there was at least one stall
                        unit::assert(tag, c - t0 >= StallThreads);
                    }
                }
            }
        );
    }
    else
    {
        print("StallTest skipped because opt:stall == 0\n");
    }
}

inline void test_main()
{
    unit::test<1>(Test);
}
