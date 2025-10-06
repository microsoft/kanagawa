// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// 7-bit wide equality comparison (to test reduction tree that is 1 bit wider than naturally aligned)
inline void Helper656(uint7 a, uint7 b, bool expected, unit::tag_t tag)
{
    bool eq = (a == b);
    bool ne = (a != b);

    unit::assert(tag, expected == eq);
    unit::assert(tag, expected == !ne);
}

inline void TestCase656(unit::tag_t tag)
{
    for (const uint7 i : 100)
    {
        Helper656(i, i, true, tag);
        Helper656(i, static_cast(i - 1), false, tag);
        Helper656(i, static_cast(i - 2), false, tag);
        Helper656(i, static_cast(i + 1), false, tag);
        Helper656(i, static_cast(i + 2), false, tag);
    }
}

// 8-bit wide equality comparison (to test reduction tree that is 2 bits wider than naturally aligned)
inline void Helper657(uint8 a, uint8 b, bool expected, unit::tag_t tag)
{
    bool eq = (a == b);
    bool ne = (a != b);

    unit::assert(tag, expected == eq);
    unit::assert(tag, expected == !ne);
}

inline void TestCase657(unit::tag_t tag)
{
    for (const uint8 i : 100)
    {
        Helper657(i, static_cast(i), true, tag);
        Helper657(i, static_cast(i - 1), false, tag);
        Helper657(i, static_cast(i - 2), false, tag);
        Helper657(i, static_cast(i + 1), false, tag);
        Helper657(i, static_cast(i + 2), false, tag);
    }
}

// Mixed unsigned/signed comparison - equal widths
class TestCase658
{
private:
    void Helper658(uint8 a, int8 b, bool expectedLT, bool expectedLE, bool expectedEQ, bool expectedNE, bool expectedGT, bool expectedGE, unit::tag_t tag)
    {
        bool lt = (a < b);
        bool le = (a <= b);

        bool eq = (a == b);
        bool ne = (a != b);

        bool gt = (a > b);
        bool ge = (a >= b);

        unit::assert(tag, expectedLT == lt);
        unit::assert(tag, expectedLE == le);

        unit::assert(tag, expectedEQ == eq);
        unit::assert(tag, expectedNE == ne);

        unit::assert(tag, expectedGT == gt);
        unit::assert(tag, expectedGE == ge);
    }

public:
    void run(unit::tag_t tag)
    {
        Helper658(0, 0, false, true, true, false, false, true, tag);

        Helper658(1, 0, false, false, false, true, true, true, tag);

        Helper658(255, 0, false, false, false, true, true, true, tag);

        Helper658(255, 1, false, false, false, true, true, true, tag);

        Helper658(255, 127, false, false, false, true, true, true, tag);

        Helper658(1, -128, false, false, false, true, true, true, tag);

        Helper658(255, -127, false, false, false, true, true, true, tag);

        Helper658(255, -1, false, false, false, true, true, true, tag);

        Helper658(0, 1, true, true, false, true, false, false, tag);

        Helper658(0, 127, true, true, false, true, false, false, tag);

        Helper658(1, 1, false, true, true, false, false, true, tag);

        Helper658(127, 127, false, true, true, false, false, true, tag);
    }
}

// Mixed signed/unsigned comparison - equal widths
class TestCase659
{
private:
    void Helper659(int8 a, uint8 b, bool expectedLT, bool expectedLE, bool expectedEQ, bool expectedNE, bool expectedGT, bool expectedGE, unit::tag_t tag)
    {
        bool lt = (a < b);
        bool le = (a <= b);

        bool eq = (a == b);
        bool ne = (a != b);

        bool gt = (a > b);
        bool ge = (a >= b);

        unit::assert(tag, expectedLT == lt);
        unit::assert(tag, expectedLE == le);

        unit::assert(tag, expectedEQ == eq);
        unit::assert(tag, expectedNE == ne);

        unit::assert(tag, expectedGT == gt);
        unit::assert(tag, expectedGE == ge);
    }

public:
    void run(unit::tag_t tag)
    {
        Helper659(0, 0, false, true, true, false, false, true, tag);

        Helper659(1, 0, false, false, false, true, true, true, tag);

        Helper659(127, 0, false, false, false, true, true, true, tag);

        Helper659(127, 1, false, false, false, true, true, true, tag);

        Helper659(-128, 0, true, true, false, true, false, false, tag);

        Helper659(-127, 0, true, true, false, true, false, false, tag);

        Helper659(-1, 0, true, true, false, true, false, false, tag);

        Helper659(-128, 1, true, true, false, true, false, false, tag);

        Helper659(-127, 1, true, true, false, true, false, false, tag);

        Helper659(-1, 1, true, true, false, true, false, false, tag);

        Helper659(-128, 255, true, true, false, true, false, false, tag);

        Helper659(-127, 255, true, true, false, true, false, false, tag);

        Helper659(-1, 255, true, true, false, true, false, false, tag);
    }
}

inline void test_main()
{
    unit::test<656>(TestCase656);
    unit::test<657>(TestCase657);
    unit::test<658>(unit::fixture<TestCase658>());
    unit::test<659>(unit::fixture<TestCase659>());
}
