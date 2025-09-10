// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import numeric.int.limits
import test.runner
import test.unit as unit

template<auto BitWidth, auto UnsignedMaxValue, auto SignedMaxValue, auto SignedMinValue>
inline void TestLimits(unit::tag_t tag)
{
    unit::assert_equal(tag, limits<uint<BitWidth>>::max, UnsignedMaxValue);
    unit::assert_equal(tag, limits<uint<BitWidth>>::min, 0);
    unit::assert_equal(tag, limits<int<BitWidth>>::max, SignedMaxValue);
    unit::assert_equal(tag, limits<int<BitWidth>>::min, SignedMinValue);
}

inline void test_main()
{
    unit::test<1>(TestLimits<2, 3, 1, -2>);
    unit::test<2>(TestLimits<8, 255, 127, -128>);
    unit::test<3>(TestLimits<32, 0xFFFFFFFF, 0x7FFFFFFF, -2147483648>);
}
