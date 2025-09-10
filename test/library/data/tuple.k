// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.tuple
import test.unit as unit
import test.runner

inline bool test_swap()
{
    bool result = true;

    tuple2<bool, uint8> t1 = {false, 1};
    tuple2<uint8, bool> t1_swapped = {1, false};
    result &&= equal(t1_swapped, swap(t1));

    // Test property swap(swap(x)) === id(x)
    pair<bool, uint3> t2 = {true, 2};
    result &&= equal(t2, swap(swap(t2)));

    return result;
}

inline void test_pair(unit::tag_t tag)
{
    unit::assert_equal(tag, 10, first(first(make_pair(make_pair(10, 99), (make_pair(true, 123))))));

    const auto x = make_pair(make_pair(10, 99), (make_pair(true, 123)));
    auto y = make_pair(second(x), first(x));

    unit::assert_equal(tag, 123, second(second(x)));
    unit::assert_equal(tag, true, first(second(x)));
    unit::assert_equal(tag, 99, second(first(x)));
    unit::assert_equal(tag, 99, second(second((y))));
    unit::assert_equal(tag, true, first(first((y))));
}

inline void test_main()
{
    print("data.tuple test start\n");

    unit::check<1>(test_swap);
    unit::test<2>(test_pair);

    print("data.tuple test complete\n");
}
