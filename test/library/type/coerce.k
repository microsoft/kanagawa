// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import type.coerce
import test.runner
import test.unit as unit

inline bool TestSafeCast()
{
    bool result = true;

    // Test some values that can safely be cast to a narrower integer.
    optional<uint8> uint8result = safe_cast(0xff_u32);
    if (!uint8result.is_valid || uint8result.value != 0xff)
    {
        print("safe_cast<uint8>(0xff_u32) expected 0xff, actual result:  {uint8result.value}  valid:  {uint8result.is_valid}\n");
        result = false;
    }

    auto uint3result = safe_cast<uint3>(7_u32);
    if (!uint3result.is_valid || uint3result.value != 7)
    {
        print("safe_cast<uint3>(7_u32) expected 7, actual result:  {uint3result.value}  valid:  {uint3result.is_valid}\n");
        result = false;
    }

    // Test a few values that can NOT safely be cast to a narrower integer.
    uint8result = safe_cast<uint8>(0x101_u32);
    if (uint8result.is_valid)
    {
        print("safe_cast<uint8>(0x101_u32) returned valid, but cast was unsafe\n");
        result = false;
    }

    uint3result = safe_cast<uint3>(17_u32);
    if (uint3result.is_valid)
    {
        print("safe_cast<uint3>(17_u32) returned valid, but cast was unsafe\n");
        result = false;
    }

    // Testing signed integers that can/can't be narrowed.
    auto int8result = safe_cast<int8>(127_i32);
    if (!int8result.is_valid || int8result.value != 127)
    {
        print("safe_cast<int8>(127_i32) expected 127, actual result:  {int8result.value}  valid:  {int8result.is_valid}\n");
        result = false;
    }

    int8result = safe_cast<int8>(129_i32);
    if (int8result.is_valid)
    {
        print("safe_cast<int8>(129_i32) returned valid, but cast was unsafe\n");
        result = false;
    }

    // Test a few negative values also.
    int8result = safe_cast<int8>(-128_i32);
    if (!int8result.is_valid || int8result.value != -128)
    {
        print("safe_cast<int8>(-128_i32) expected -128, actual result:  {int8result.value}  valid:  {int8result.is_valid}\n");
        result = false;
    }

    int8result = safe_cast<int8>(-129_i32);
    if (int8result.is_valid)
    {
        print("safe_cast<int8>(-129_i32) returned valid, but cast was unsafe\n");
        result = false;
    }

    // Sanity check that casting up to wider values is recognized as safe.
    uint8result = safe_cast<uint8>(15u4);
    if (!uint8result.is_valid || uint8result.value != 15)
    {
        print("safe_cast<uint8>(15u4) expected 15, actual result:  {uint8result.value}  valid:  {uint8result.is_valid}\n");
        result = false;
    }

    return result;
}

inline void TestStaticCast(unit::tag_t tag)
{
    uint8 x = static_cast(1024);
    unit::assert_equal(tag, 0, x);

    auto y = static_cast<uint8>(1024);
    unit::assert_equal(tag, 0, y);
}

inline void test_main()
{
    unit::check<1>(TestSafeCast);
    unit::test<2>(TestStaticCast);
}

