// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import numeric.int.operator.modular as modular
import test.unit as unit
import test.runner

inline void TestAdd(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> a)
    {
        pipelined_for(32, [tag, a](index_t<32> b)
        {
            // power of 2
            unit::assert_equal(tag, cast<index_t<32>>(a + b), modular::add(a, b));

            // non power of 2
            if ((a < 17) && (b < 17))
            {
                index_t<17> actual = modular::add<index_t<32>, index_t<32>, 17>(a, b);

                auto expected = a + b;

                if (expected >= 17)
                {
                    expected -= 17;

                    assert(expected < 17);
                }

                unit::assert_equal(tag, expected, actual);
            }
        });
    });
}

inline void TestIncrement(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> a)
    {
        // power of 2
        unit::assert_equal(tag, cast<index_t<32>>(a + 1), modular::increment(a));

        // non power of 2
        if (a < 23)
        {
            index_t<23> actual = modular::increment<index_t<32>, 23>(a);

            auto expected = a + 1;

            if (expected >= 23)
            {
                expected -= 23;

                assert(expected < 23);
            }

            unit::assert_equal(tag, expected, actual);
        }
    });
}

inline void TestIncrementIf(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> a)
    {
        bool b = ((a >> 1) & 1) == 1;

        unit::assert_equal(tag, cast<index_t<32>>(a + cast<uint1>(b)), modular::increment_if(a, b));
    });
}

inline void TestSub(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> a)
    {
        pipelined_for(32, [tag, a](index_t<32> b)
        {
            // power of 2
            unit::assert_equal(tag, cast<index_t<32>>(a - b), modular::sub(a, b));

            // non power of 2
            if ((a < 17) && (b < 17))
            {
                index_t<17> actual = modular::sub<index_t<32>, index_t<32>, 17>(a, b);

                auto expected = a - b;

                if (expected < 0)
                {
                    expected += 17;

                    assert(expected >= 0);
                }

                unit::assert_equal(tag, expected, actual);
            }
        });
    });
}

inline void TestDecrement(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> a)
    {
        // power of 2
        unit::assert_equal(tag, cast<index_t<32>>(a - 1), modular::decrement(a));

        // non power of 2
        if (a < 23)
        {
            index_t<23> actual = modular::decrement<index_t<32>, 23>(a);

            auto expected = a - 1;

            if (expected < 0)
            {
                expected += 23;

                assert(expected >= 0);
            }

            unit::assert_equal(tag, expected, actual);
        }
    });
}

inline void TestDecrementIf(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> a)
    {
        bool b = ((a >> 2) & 1) == 1;

        unit::assert_equal(tag, cast<index_t<32>>(a - cast<uint1>(b)), modular::decrement_if(a, b));
    });
}

inline void test_main()
{
    unit::test<1>(TestAdd);
    unit::test<2>(TestIncrement);
    unit::test<3>(TestIncrementIf);
    unit::test<4>(TestSub);
    unit::test<5>(TestDecrement);
    unit::test<6>(TestDecrementIf);
}
