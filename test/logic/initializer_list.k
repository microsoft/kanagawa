// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Initializer list used to initialize one member of a union
inline void InitializeOneUnionMember(unit::tag_t tag)
{
    union U
    {
        uint16 u16;
        uint32 u32;
        uint8  u8;
    }

    U u = { 0x12ba };

    unit::assert_equal(tag, 0x12ba, u.u16);
    unit::assert_equal(tag, 0x000012ba, u.u32);
    unit::assert_equal(tag, 0xba, u.u8);

    // Initializer u16 to a 32-bit value
    u = { 0xabcd1234 };

    unit::assert_equal(tag, 0x1234, u.u16);
    unit::assert_equal(tag, 0x00001234, u.u32);
    unit::assert_equal(tag, 0x34, u.u8);
}

inline void test_main()
{
    unit::test<1>(InitializeOneUnionMember);
}
