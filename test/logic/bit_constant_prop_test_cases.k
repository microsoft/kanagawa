// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Test constant analysis for add operations where MSBs of operands are known to be constant
inline void TestAddZeroMsb(unit::tag_t tag)
{
    pipelined_for (4, [tag](index_t<4> tid)
    {
        uint4 a = mux(tid, 3, 4, 15, 15);
        uint4 b = mux(tid, 1, 6, 12, 15);

        uint8 actual = static_cast(a + cast<uint8>(b));

        uint8 expected = mux(tid, 4, 10, 27, 30);

        unit::assert_equal(tag, expected, actual);

        // To prevent the register width optimization
        // from narrowing `actual`
        assert(actual > 0);
    });
}

// Test constant analysis for add operations where MSBs of operands are known to be constant
// With 1 of the add operands being a literal
inline void TestAddLiteralRhsZeroMsb(unit::tag_t tag)
{
    pipelined_for (4, [tag](index_t<4> tid)
    {
        uint4 a = mux(tid, 3, 4, 15, 0);

        uint8 actual = static_cast(cast<uint8>(a) + 6);

        uint8 expected = mux(tid, 9, 10, 21, 6);

        unit::assert_equal(tag, expected, actual);

        // To prevent the register width optimization
        // from narrowing `actual`
        assert(actual > 0);
    });
}

inline void TestAddLiteralLhsZeroMsb(unit::tag_t tag)
{
    pipelined_for (4, [tag](index_t<4> tid)
    {
        uint4 a = mux(tid, 3, 4, 15, 0);

        uint8 actual = static_cast(7 + cast<uint8>(a));

        uint8 expected = mux(tid, 10, 11, 22, 7);

        unit::assert_equal(tag, expected, actual);

        // To prevent the register width optimization
        // from narrowing `actual`
        assert(actual > 0);
    });
}

// Test constant analysis for add operations where MSBs of operands are known to be constant
// signed operands
inline void TestAddSignedZeroMsb(unit::tag_t tag)
{
    pipelined_for (4, [tag](index_t<4> tid)
    {
        int4 a = static_cast(mux(tid, 3,  -4, 7,  -8));
        int4 b = static_cast(mux(tid, -1,  6, -8, -8));

        int8 actual = static_cast(cast<uint8>(a) + cast<uint8>(b));

        int8 expected = mux(tid, 2, 2, -1, -16);

        unit::assert_equal(tag, expected, actual);

        // To prevent the register width optimization
        // from narrowing `actual`
        assert(actual > -255);
    });
}

inline void test_main()
{
    unit::test<1>(TestAddZeroMsb);
    unit::test<2>(TestAddLiteralRhsZeroMsb);
    unit::test<3>(TestAddLiteralLhsZeroMsb);
    unit::test<4>(TestAddSignedZeroMsb);
}
