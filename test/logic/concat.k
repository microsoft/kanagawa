// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Check that concat honors typed literal width
inline void TypedLiterals(unit::tag_t tag)
{
    auto result = concat(2u2, 123u16, 456u10);

    unit::assert_equal(tag, 0x801EDC8, result);
}

// Check that concat honors type system special cases
// where one operand of a binary operation is known at compile time
inline void BinaryOpSpecialCases(unit::tag_t tag)
{
    uint8 a = 123;
    uint8 b = 98;

    auto result = concat(b % 4, a >> 2);

    unit::assert_equal(tag, 0x9E, result);
}

inline void EnumMember(unit::tag_t tag)
{
    enum E : uint8
    {
        one = 1
    }

    auto result = concat(3u8, E::one);
    unit::assert_equal(tag, 16, bitsizeof result);
    unit::assert_equal(tag, 0x0301, result);
}

inline void test_main()
{
    unit::test<1>(TypedLiterals);
    unit::test<2>(BinaryOpSpecialCases);
    unit::test<3>(EnumMember);
}
