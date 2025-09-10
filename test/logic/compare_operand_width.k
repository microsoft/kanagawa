// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Same signed comparison - different widths dw(a) < dw(b)
inline void CompareOpsInt4Int8(int4 a, int8 b, bool expectedLT, bool expectedLE, bool expectedEQ, bool expectedNE, bool expectedGT, bool expectedGE, unit::tag_t tag)
{
    bool lt = (a < b);
    bool le = (a <= b);

    bool eq = (a == b);
    bool ne = (a != b);

    bool gt = (a > b);
    bool ge = (a >= b);

    unit::assert_equal(tag, expectedLT, lt);
    unit::assert_equal(tag, expectedLE, le);

    unit::assert_equal(tag, expectedEQ, eq);
    unit::assert_equal(tag, expectedNE, ne);

    unit::assert_equal(tag, expectedGT, gt);
    unit::assert_equal(tag, expectedGE, ge);
}

inline void TestFunctionCompareOpsInt4Int8(unit::tag_t tag)
{
    CompareOpsInt4Int8(0, 0, false, true, true, false, false, true, tag);

    CompareOpsInt4Int8(1, 0, false, false, false, true, true, true, tag);

    CompareOpsInt4Int8(7, -15, false, false, false, true, true, true, tag);

    CompareOpsInt4Int8(7,  1, false, false, false, true, true, true, tag);

    CompareOpsInt4Int8(-8, 0, true, true, false, true, false, false, tag);

    CompareOpsInt4Int8(-1, 1, true, true, false, true, false, false, tag);

    CompareOpsInt4Int8(-8, 127, true, true, false, true, false, false, tag);

    CompareOpsInt4Int8(-8, -128, false, false, false, true, true, true, tag);
}

// mixed signed/unsigned comparison - different widths dw(a) < dw(b)
inline void CompareOpsInt4Uint8(int4 a, uint8 b, bool expectedLT, bool expectedLE, bool expectedEQ, bool expectedNE, bool expectedGT, bool expectedGE, unit::tag_t tag)
{
    bool lt = (a < b);
    bool le = (a <= b);

    bool eq = (a == b);
    bool ne = (a != b);

    bool gt = (a > b);
    bool ge = (a >= b);

    unit::assert_equal(tag, expectedLT, lt);
    unit::assert_equal(tag, expectedLE, le);

    unit::assert_equal(tag, expectedEQ, eq);
    unit::assert_equal(tag, expectedNE, ne);

    unit::assert_equal(tag, expectedGT, gt);
    unit::assert_equal(tag, expectedGE, ge);
}

inline void TestFunctionCompareOpsInt4Uint8(unit::tag_t tag)
{
    CompareOpsInt4Uint8(0, 0, false, true, true, false, false, true, tag);

    CompareOpsInt4Uint8(1, 0, false, false, false, true, true, true, tag);

    CompareOpsInt4Uint8(7, 15, true, true, false, true, false, false, tag);

    CompareOpsInt4Uint8(7,  1, false, false, false, true, true, true, tag);

    CompareOpsInt4Uint8(-8, 0, true, true, false, true, false, false, tag);

    CompareOpsInt4Uint8(-1, 1, true, true, false, true, false, false, tag);

    CompareOpsInt4Uint8(-8, 255, true, true, false, true, false, false, tag);

    CompareOpsInt4Uint8(-8, 127, true, true, false, true, false, false, tag);
}

// mixed signed/unsigned comparison - different widths dw(a) > dw(b)
inline void CompareOpsInt8Uint4(int8 a, uint4 b, bool expectedLT, bool expectedLE, bool expectedEQ, bool expectedNE, bool expectedGT, bool expectedGE, unit::tag_t tag)
{
    bool lt = (a < b);
    bool le = (a <= b);

    bool eq = (a == b);
    bool ne = (a != b);

    bool gt = (a > b);
    bool ge = (a >= b);

    unit::assert_equal(tag, expectedLT, lt);
    unit::assert_equal(tag, expectedLE, le);

    unit::assert_equal(tag, expectedEQ, eq);
    unit::assert_equal(tag, expectedNE, ne);

    unit::assert_equal(tag, expectedGT, gt);
    unit::assert_equal(tag, expectedGE, ge);
}

inline void TestFunctionCompareOpsInt8Uint4(unit::tag_t tag)
{
    CompareOpsInt8Uint4(0, 0, false, true, true, false, false, true, tag);

    CompareOpsInt8Uint4(1, 0, false, false, false, true, true, true, tag);

    CompareOpsInt8Uint4(7, 15, true, true, false, true, false, false, tag);

    CompareOpsInt8Uint4(7,  1, false, false, false, true, true, true, tag);

    CompareOpsInt8Uint4(-8, 0, true, true, false, true, false, false, tag);

    CompareOpsInt8Uint4(-128, 1, true, true, false, true, false, false, tag);

    CompareOpsInt8Uint4(-8, 15, true, true, false, true, false, false, tag);

    CompareOpsInt8Uint4(-7, 12, true, true, false, true, false, false, tag);
}

inline void test_main()
{
    print("Test CompareOpsInt4Int8\n");
    unit::test<1>(TestFunctionCompareOpsInt4Int8);

    print("Test CompareOpsInt4Uint8\n");
    unit::test<2>(TestFunctionCompareOpsInt4Uint8);

    print("Test CompareOpsInt8Uint4\n");
    unit::test<3>(TestFunctionCompareOpsInt8Uint4);
}
