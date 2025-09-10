// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.bits
import data.representation
import test.runner
import test.unit as unit

inline bool TestConvertToOneHot()
{
    bool result = true;

    for (const auto i : 32)
    {
        auto actualOneHot = binary_to_one_hot<32>(i);
        auto actualBitCount = pop_count(actualOneHot);
        auto actualHighestOne = highest_one(actualOneHot);
        // The expected bitcount for a onehot is one, hence the name.
        if (actualBitCount != 1)
        {
            print("Error: binary_to_one_hot<32>( {i} ): actual bit count  {actualBitCount}\n");
            result = false;
        }

        // The expected highest bit set on iteration i is i.
        if (actualHighestOne.value != i)
        {
            print("Error: binary_to_one_hot<32>( {i} ): actual first bit set  {actualHighestOne}\n");
            result = false;
        }
    }

    // Uncomment to test that the assert detects an invalid call like this:
    // auto testSimAssert = binary_to_one_hot<32>(37); // Can't set the 37th bool in a 32 bool array

    return result;
}

inline bool TestOnehotMux()
{
    bool result = true;

    const auto selection_size = 32;
    uint16[selection_size] int_selection;

    static for (const auto i : selection_size)
    {
        int_selection[i] = i * 10;
    }

    for (const auto i : selection_size)
    {
        uint16 expected_value = i * 10;
        uint<selection_size> onehot = 1 << i;

        auto mux_result = onehot_mux(onehot, int_selection);

        if (mux_result != expected_value)
        {
            print("Error: onehot_mux for index {i} expected {expected_value} actual {mux_result}\n");
            result = false;
        }
    }

    return result;
}

struct onehot_test
{
    uint16 times_two;
    uint16 times_three;
}

inline bool TestOnehotMuxWithStruct()
{
    bool result = true;

    const auto selection_size = 16;
    onehot_test[selection_size] struct_selection;

    static for (const auto i : selection_size)
    {
        struct_selection[i].times_two = i * 2;
        struct_selection[i].times_three = i * 3;
    }

    for (const auto i : selection_size)
    {
        onehot_test expected_value;
        expected_value.times_two = i * 2;
        expected_value.times_three = i * 3;

        uint<selection_size> onehot = 1 << i;

        auto mux_result = onehot_mux(onehot, struct_selection);

        if (mux_result.times_two != expected_value.times_two ||
            mux_result.times_three != expected_value.times_three)
        {
            print("Error: onehot_mux for index {i} expected {expected_value} actual {mux_result}\n");
            result = false;
        }
    }

    return result;
}

inline bool TestEndianChange()
{
    bool result = true;

    auto uint8result = endian_change<uint8>(0x12);
    if (uint8result != 0x12)
    {
        print("endian_change<uint8>(0x12) expected 0x12, actual result:  {uint8result}\n");
        result = false;
    }

    auto uint16result = endian_change<uint16>(0x1234);
    if (uint16result != 0x3412)
    {
        print("endian_change<uint16>(0x1234) expected 0x3412, actual result:  {uint16result}\n");
        result = false;
    }

    uint16result = endian_change<uint16>(0x12);
    if (uint16result != 0x1200)
    {
        print("endian_change<uint16>(0x12) expected 0x1200, actual result:  {uint16result}\n");
        result = false;
    }

    // Testing with an odd number of bytes.
    auto uint24result = endian_change<uint24>(0x123456);
    if (uint24result != 0x563412)
    {
        print("endian_change<uint24>(0x123456) expected 0x563412, actual result:  {uint24result}\n");
        result = false;
    }

    auto uint32result = endian_change<uint32>(0x12345678);
    if (uint32result != 0x78563412)
    {
        print("endian_change<uint32>(0x12345678) expected 0x78563412, actual result:  {uint32result}\n");
        result = false;
    }

    auto uint64result = endian_change<uint64>(0x1234567811223344);
    if (uint64result != 0x4433221178563412)
    {
        print("endian_change<uint64>(0x1234567811223344) expected 0x4433221178563412, actual result:  {uint64result}\n");
        result = false;
    }

    // Some tests with signed integers
    auto int32result = endian_change<int32>(0x12345678);
    if (int32result != 0x78563412)
    {
        print("endian_change<int32>(0x12345678) expected 0x78563412, actual result:  {int32result}\n");
        result = false;
    }

    auto int64result = endian_change<int64>(0x1234567811223344);
    if (int64result != 0x4433221178563412)
    {
        print("endian_change<int64>(0x1234567811223344) expected 0x4433221178563412, actual result:  {int64result}\n");
        result = false;
    }

    // Uncomment to test that the compiler detects an invalid call like this,
    // where the input type isn't a multiple of 8 bits wide.
    // auto uint13result = endian_change<uint13>(0x123);

    return result;
}

inline void test_main()
{
    uint32 result = 0;

    unit::check<1>(TestConvertToOneHot);
    unit::check<2>(TestOnehotMux);
    unit::check<3>(TestOnehotMuxWithStruct);
    unit::check<4>(TestEndianChange);
}

