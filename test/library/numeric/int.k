// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Copyright (c), Microsoft Corporation. All rights reserved.

import numeric.int
import test.unit as unit
import test.runner

inline void test_even(unit::tag_t tag)
{
    unit::assert(tag,  even(0));
    unit::assert(tag, !even(1));
    unit::assert(tag,  even(2));
}

inline void test_odd(unit::tag_t tag)
{
    unit::assert(tag, !odd(0));
    unit::assert(tag,  odd(1));
    unit::assert(tag, !odd(2));
}

inline void test_main()
{
    print("numeric.int test start\n");
    unit::test<1>(test_even);
    unit::test<2>(test_odd);
    print("numeric.int.test complete\n");
}
