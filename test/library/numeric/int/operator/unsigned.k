// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import numeric.int.operator.unsigned as unsigned
import test.unit as unit
import test.runner

inline void TestSub(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> a)
    {
        pipelined_for(32, [tag, a](index_t<32> b)
        {
            if (b <= a)
            {
                index_t<32> result = unsigned::sub(a, b);

                unit::assert_equal(tag, a - b, result);
            }
        });
    });
}

inline void TestDecrement(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> a)
    {
        if (a > 0)
        {
            index_t<32> result = unsigned::decrement(a);

            unit::assert_equal(tag, a - 1, result);
        }
    });
}

inline void TestDecrementIf(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> a)
    {
        pipelined_for(2, [tag, a](index_t<2> b)
        {
            if (a > 0)
            {
                index_t<32> result = unsigned::decrement_if(a, cast<bool>(b));

                unit::assert_equal(tag, a - b, result);
            }
        });
    });
}

inline void test_main()
{
    unit::test<1>(TestSub);
    unit::test<2>(TestDecrement);
    unit::test<3>(TestDecrementIf);
}
