// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

inline void StaticInUnrolledFor(unit::tag_t tag)
{
    pipelined_for (8, [tag](index_t<8> tid)
    {
        static for (const auto i : 4)
        {
            uint32 before;
            uint32 after;

            atomic
            {
                static uint32 _x = 0;

                before = _x;

                _x += i;

                after = _x;
            }
        
            unit::assert_equal(tag, tid == 0 ? 0 : tid * i, before);
            unit::assert_equal(tag, (tid + 1) * i, after);
        }
    });
}

inline void test_main()
{
    unit::test<1>(StaticInUnrolledFor);
}
