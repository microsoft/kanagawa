// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Mixed signed/unsigned comparison - unsigned value is wider
inline void Helper660(int8 a, uint16 b, bool expectedLT, bool expectedLE, bool expectedEQ, bool expectedNE, bool expectedGT, bool expectedGE, unit::tag_t tag)
{
    bool lt = (a < b);
    bool le = (a <= b);

    bool eq = (a == b);
    bool ne = (a != b);

    bool gt = (a > b);
    bool ge = (a >= b);

    assert(expectedLT == lt);
    assert(expectedLE == le);

    assert(expectedEQ == eq);
    assert(expectedNE == ne);

    assert(expectedGT == gt);
    assert(expectedGE == ge);
}

inline void TestCase660(unit::tag_t tag)
{
    Helper660(0, 0, false, true, true, false, false, true, tag);

    Helper660(1, 0, false, false, false, true, true, true, tag);

    Helper660(-1, 0, true, true, false, true, false, false, tag);

    Helper660(0, 1, true, true, false, true, false, false, tag);

    Helper660(1, 1, false, true, true, false, false, true, tag);

    Helper660(-1, 1, true, true, false, true, false, false, tag);

    Helper660(-128, 0, true, true, false, true, false, false, tag);

    Helper660(-127, 0, true, true, false, true, false, false, tag);

    Helper660(-128, 1, true, true, false, true, false, false, tag);

    Helper660(-127, 1, true, true, false, true, false, false, tag);

    Helper660(-128, 1, true, true, false, true, false, false, tag);

    Helper660(-128, 65535, true, true, false, true, false, false, tag);

    Helper660(127, 65535, true, true, false, true, false, false, tag);

    Helper660(127, 128, true, true, false, true, false, false, tag);

    Helper660(127, 127, false, true, true, false, false, true, tag);

    Helper660(127, 126, false, false, false, true, true, true, tag);
}

// Mixed signed/unsigned comparison - unsigned value is narrower
inline void Helper661(int16 a, uint8 b, bool expectedLT, bool expectedLE, bool expectedEQ, bool expectedNE, bool expectedGT, bool expectedGE, unit::tag_t tag)
{
    bool lt = (a < b);
    bool le = (a <= b);

    bool eq = (a == b);
    bool ne = (a != b);

    bool gt = (a > b);
    bool ge = (a >= b);

    assert(expectedLT == lt);
    assert(expectedLE == le);

    assert(expectedEQ == eq);
    assert(expectedNE == ne);

    assert(expectedGT == gt);
    assert(expectedGE == ge);
}

inline void TestCase661(unit::tag_t tag)
{
    Helper661(0, 0, false, true, true, false, false, true, tag);

    Helper661(1, 0, false, false, false, true, true, true, tag);

    Helper661(1, 1, false, true, true, false, false, true, tag);

    Helper661(254, 255, true, true, false, true, false, false, tag);

    Helper661(255, 255, false, true, true, false, false, true, tag);

    Helper661(255, 254, false, false, false, true, true, true, tag);

    Helper661(32767, 0, false, false, false, true, true, true, tag);

    Helper661(-32768, 0, true, true, false, true, false, false, tag);

    Helper661(-32767, 0, true, true, false, true, false, false, tag);

    Helper661(-2, 0, true, true, false, true, false, false, tag);

    Helper661(-1, 0, true, true, false, true, false, false, tag);
}


inline void test_main()
{
    unit::test<660>(TestCase660);
    unit::test<661>(TestCase661);
}
