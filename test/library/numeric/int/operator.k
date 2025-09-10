// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.function
import numeric.int.operator
import test.unit as unit
import test.runner

inline void TestDivMod(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint32 tid)
    {
        auto result0 = div_mod(tid, 4);

        unit::assert_equal(tag, tid / 4, result0.first);
        unit::assert_equal(tag, tid % 4, result0.second);
        unit::assert_equal(tag, div(tid, 4), result0.first);
        unit::assert_equal(tag, mod(tid, 4), result0.second);

        auto result1 = div_mod<1>(tid, 4);

        unit::assert_equal(tag, tid / 4, result1.first);
        unit::assert_equal(tag, (tid % 4) == 0 ? 4 : tid % 4, result1.second);
        unit::assert_equal(tag, div(tid, 4), result1.first);
        unit::assert_equal(tag, mod<1>(tid, 4), result1.second);

        auto result2 = div_mod<2>(tid, 4);

        unit::assert_equal(tag, tid / 4, result2.first);
        unit::assert_equal(tag, mux(cast<uint2>(tid % 4), 4, 5, 2, 3), result2.second);
        unit::assert_equal(tag, div(tid, 4), result2.first);
        unit::assert_equal(tag, mod<2>(tid, 4), result2.second);
    });
}

inline void TestCeilDivMod(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint32 tid)
    {
        auto result0 = ceil_div_mod(tid, 4);

        unit::assert_equal(tag, 32, bitsizeof(result0.first));
        unit::assert_equal(tag, (tid + 3) / 4, result0.first);
        unit::assert_equal(tag, tid % 4, result0.second);
        unit::assert_equal(tag, ceil_div(tid, 4), result0.first);
        unit::assert_equal(tag, mod(tid, 4), result0.second);

        auto result1 = ceil_div_mod<1>(tid, 4);

        unit::assert_equal(tag, 32, bitsizeof(result0.first));
        unit::assert_equal(tag, (tid + 3) / 4, result1.first);
        unit::assert_equal(tag, (tid % 4) == 0 ? 4 : tid % 4, result1.second);
        unit::assert_equal(tag, ceil_div(tid, 4), result1.first);
        unit::assert_equal(tag, mod<1>(tid, 4), result1.second);

        auto result2 = ceil_div_mod<2>(tid, 4);

        unit::assert_equal(tag, 32, bitsizeof(result0.first));
        unit::assert_equal(tag, (tid + 3) / 4, result2.first);
        unit::assert_equal(tag, mux(cast<uint2>(tid % 4), 4, 5, 2, 3), result2.second);
        unit::assert_equal(tag, ceil_div(tid, 4), result2.first);
        unit::assert_equal(tag, mod<2>(tid, 4), result2.second);
    });
}

// Test for a case where the numerator passed to
// ceil_div is narrow, internal computation should not overflow
inline void TestCeilDivNarrow(unit::tag_t tag)
{
    pipelined_for(32, [tag](index_t<32> tid)
    {
        auto result = ceil_div(tid, 2);

        index_t<32> expected = (tid / 2) + cast<uint1>(tid % 2);

        unit::assert_equal(tag, expected, result);
    });
}

const auto xor_and_increment = compose2(increment, xor);

inline void TestOps(unit::tag_t tag)
{
    const auto x = 45325;
    const auto y = 87241;

    unit::assert_equal(tag, (x ^ y) + 1, xor_and_increment(x, y));

    const auto cipher = bind2nd(xor, 0xC0DE);

    pipelined_for(256, [tag, cipher](index_t<256> i)
    {
        unit::assert_equal(tag, i, cipher(cipher(i)));
    });
}

inline void TestAbs(unit::tag_t tag)
{
    pipelined_for(256, [tag](index_t<256> i)
    {
        auto src = cast<int8>(i);

        uint8 actual = abs(src);

        auto expected = src < 0 ? -src : src;

        unit::assert_equal(tag, expected, actual);
    });
}

inline void test_main()
{
    unit::test<1>(TestDivMod);
    unit::test<2>(TestCeilDivMod);
    unit::test<3>(TestCeilDivNarrow);
    unit::test<4>(TestOps);
    unit::test<5>(TestAbs);
}
