// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.bits
import test.unit as unit
import test.runner

inline bool TestRotl()
{
    bool result = true;

    for (const auto i : 3)
    {
        uint4 actual = rotl(0x9, i);
        uint4 expected;
        switch (i)
        {
            case 0:
                expected = 0x9;
                break;
            case 1:
                expected = 0x3;
                break;
            case 2:
                expected = 0x6;
                break;
            case 3:
                expected = 0xC;
                break;
        }
        if (actual != expected)
        {
            print("Error: rotl(0x9, {i} ): expected {expected}  but returned {actual}\n");
            result = false;
        }
    }

    return result;
}

inline bool TestRotr()
{
    bool result = true;

    for (const auto i : 3)
    {
        uint4 actual = rotr(0x9, i);
        uint4 expected;
        switch (i)
        {
            case 0:
                expected = 0x9;
                break;
            case 1:
                expected = 0xC;
                break;
            case 2:
                expected = 0x6;
                break;
            case 3:
                expected = 0x3;
                break;
        }
        if (actual != expected)
        {
            print("Error: rotr(0x9, {i}): expected {expected}  but returned {actual}\n");
            result = false;
        }
    }

    return result;
}

inline bool TestHighestOne()
{
    const auto N = 13;
    bool result = true;

    for (const auto i : N)
    {
        auto inp = cast<uint<N>>(1 << i);
        auto expected = highest_one(inp);
        if (inp == 0)
        {
            if (expected.is_valid == true)
            {
                print("Error: expected({inp}): expected no bit set\n");
                result = false;
            }
        }
        else
        {
            if (expected.is_valid != true || expected.value != i)
            {
                print("Error: highest_one({inp}): expected {make_pair<bool, uint<N>>(true, i)} but returned {expected}\n");
                result = false;
            }
        }
        inp |= 1 << (N - 1);
        expected = highest_one(inp);
        if (expected.is_valid != true || expected.value != N - 1)
        {
            print("Error: highest_one({inp}): expected {make_pair<bool, uint<N>>(true, N - 1)} but returned {expected}\n");
            result = false;
        }
    }

    return result;
}

inline bool TestLowestOne()
{
    const auto N = 13;
    bool result = true;

    for (const auto i : N)
    {
        auto inp = cast<uint<N>>(1 << i);
        auto expected = lowest_one(inp);
        if (inp == 0)
        {
            if (expected.is_valid == true)
            {
                print("Error: expected( {inp} ): expected no bit set\n");
                result = false;
            }
        }
        else
        {
            if (expected.is_valid != true || expected.value != i)
            {
                print("Error: lowest_one( {inp} ): expected {make_pair<bool, uint<N>>(true, i)} but returned {expected}\n");
                result = false;
            }
        }
        inp |= 1;
        expected = lowest_one(inp);
        if (expected.is_valid != true || expected.value != 0)
        {
            print("Error: lowest_one( {inp} ): expected {make_pair<bool, uint<N>>(true, N - 1)} but returned {expected}\n");
            result = false;
        }
    }

    return result;
}

inline bool TestPopCount()
{
    bool result = true;

    for (const uint5 i : 16)
    {
        uint5 expected;
        switch(i)
        {
            case 0:
                expected = 0; break;
            case 1:
                expected = 1; break;
            case 2:
                expected = 1; break;
            case 3:
                expected = 2; break;
            case 4:
                expected = 1; break;
            case 5:
                expected = 2; break;
            case 6:
                expected = 2; break;
            case 7:
                expected = 3; break;
            case 8:
                expected = 1; break;
            case 9:
                expected = 2; break;
            case 10:
                expected = 2; break;
            case 11:
                expected = 3; break;
            case 12:
                expected = 2; break;
            case 13:
                expected = 3; break;
            case 14:
                expected = 3; break;
            case 15:
                expected = 4; break;
        }

        uint5 actual = pop_count(i);
        if (actual != expected)
        {
            print("Error: pop_count<4>( {i} ): expected {expected}  but returned {actual}\n");
            result = false;
        }
    }

    return result;
}

inline bool TestPopCountOddNumberOfBits()
{
    bool result = true;

    for (const uint6 i : 32)
    {
        uint6 expected;
        switch(i)
        {
            case 0:
                expected = 0; break;
            case 1:
                expected = 1; break;
            case 2:
                expected = 1; break;
            case 3:
                expected = 2; break;
            case 4:
                expected = 1; break;
            case 5:
                expected = 2; break;
            case 6:
                expected = 2; break;
            case 7:
                expected = 3; break;
            case 8:
                expected = 1; break;
            case 9:
                expected = 2; break;
            case 10:
                expected = 2; break;
            case 11:
                expected = 3; break;
            case 12:
                expected = 2; break;
            case 13:
                expected = 3; break;
            case 14:
                expected = 3; break;
            case 15:
                expected = 4; break;
            case 16:
                expected = 1; break;
            case 17:
                expected = 2; break;
            case 18:
                expected = 2; break;
            case 19:
                expected = 3; break;
            case 20:
                expected = 2; break;
            case 21:
                expected = 3; break;
            case 22:
                expected = 3; break;
            case 23:
                expected = 4; break;
            case 24:
                expected = 2; break;
            case 25:
                expected = 3; break;
            case 26:
                expected = 3; break;
            case 27:
                expected = 4; break;
            case 28:
                expected = 3; break;
            case 29:
                expected = 4; break;
            case 30:
                expected = 4; break;
            case 31:
                expected = 5; break;
        }

        uint6 actual = pop_count(i);
        if (actual != expected)
        {
            print("Error: pop_count<5>( {i} ): expected {expected}  but returned {actual}\n");
            result = false;
        }
    }

    return result;
}


template<typename InputType, typename OutputType>
inline bool TestRoundupToPow2Helper(InputType input, OutputType expected)
{
    bool result = true;

    auto roundupResult = roundup_to_pow2<InputType, OutputType>(input);
    if (roundupResult.is_valid != true)
    {
        print("roundup_to_pow2 for {input} returned invalid, but should have been valid.\n");
        result = false;
    }
    else if (roundupResult.value != expected)
    {
        print("roundup_to_pow2 for {input} expected {expected} actual {roundupResult.value}\n");
        result = false;
    }

    return result;
}

inline bool TestRoundupToPow2()
{
    bool result = true;

    auto roundupResult = roundup_to_pow2<uint15, uint16>(0);

    if (roundupResult.is_valid)
    {
        print("roundup_to_pow2 for 0 returned valid, but should have returned invalid.\n");
        result = false;
    }

    if (!TestRoundupToPow2Helper<int7, int8>(0x1f, 0x20))
    {
        result = false;
    }

    if (!TestRoundupToPow2Helper<uint7, uint8>(0x7f, 0x80))
    {
        result = false;
    }

    if (!TestRoundupToPow2Helper<uint7, uint8>(0x20, 0x20))
    {
        result = false;
    }

    if (!TestRoundupToPow2Helper<uint15, uint16>(0x21, 0x40))
    {
        result = false;
    }

    if (!TestRoundupToPow2Helper<int15, int16>(0x701, 0x800))
    {
        result = false;
    }

    return result;
}

template<typename DividendType, typename DivisorType>
inline bool TestDivideAndRoundupHelper(DividendType dividend, const DivisorType divisor, DividendType expected)
{
    bool result = true;

    auto divResult = divide_and_roundup<DividendType, DivisorType>(dividend, divisor);
    if (divResult != expected)
    {
        print("divide_and_roundup for {dividend} , {divisor} expected {expected} actual {divResult}\n");
        result = false;
    }

    return result;
}

inline bool TestDivideAndRoundup()
{
    bool result = true;

    // Note that a few different operand types are used below
    // to cover larger/smaller types and signed/unsigned.
    // Note that the divisor must be signed.

    if (!TestDivideAndRoundupHelper<uint16, uint12>(0, 0x20, 0))
    {
        result = false;
    }

    if (!TestDivideAndRoundupHelper<uint16, uint12>(1, 0x20, 1))
    {
        result = false;
    }

    if (!TestDivideAndRoundupHelper<int8, uint8>(0x20, 0x20, 1))
    {
        result = false;
    }

    if (!TestDivideAndRoundupHelper<uint16, uint16>(0x21, 0x20, 2))
    {
        result = false;
    }

    if (!TestDivideAndRoundupHelper<int12, uint12>(0x3f, 0x20, 2))
    {
        result = false;
    }

    if (!TestDivideAndRoundupHelper<uint16, uint16>(0x40, 0x20, 2))
    {
        result = false;
    }

    if (!TestDivideAndRoundupHelper<uint16, uint16>(0x800, 0x40, 0x20))
    {
        result = false;
    }

    if (!TestDivideAndRoundupHelper<int13, uint13>(0x81f, 0x40, 0x21))
    {
        result = false;
    }

    if (!TestDivideAndRoundupHelper<uint16, uint16>(0x820, 0x40, 0x21))
    {
        result = false;
    }

    return result;
}

inline bool TestCountAndIndex()
{
    bool result = true;

    index_t<7> x7;
    if (bitsizeof(x7) != 3)
    {
        print("index_t<7> expected bitsize 3, actual {bitsizeof(x7)}\n");
        result = false;
    }

    index_t<7> x8;
    if (bitsizeof(x8) != 3)
    {
        print("index_t<8> expected bitsize 3, actual {bitsizeof(x8)}\n");
        result = false;
    }

    index_t<9> x9;
    if (bitsizeof(x9) != 4)
    {
        print("index_t<9> expected bitsize 4, actual {bitsizeof(x9)}\n");
        result = false;
    }

    count_t<7> y7;
    if (bitsizeof(y7) != 3)
    {
        print("count_t<7> expected bitsize 3, actual {bitsizeof(y7)}\n");
        result = false;
    }

    count_t<8> y8;
    if (bitsizeof(y8) != 4)
    {
        print("count_t<8> expected bitsize 4, actual {bitsizeof(y8)}\n");
        result = false;
    }

    count_t<9> y9;
    if (bitsizeof(y9) != 4)
    {
        print("count_t<9> expected bitsize 4, actual {bitsizeof(y9)}\n");
        result = false;
    }

    return result;
}

inline void TestMask(unit::tag_t tag)
{
    print("mask test start\n");

    bool[12] errors;
    const auto N = 8;

    {
        auto zero = mask_greater_equal<N, index_t<N>>(0);
        static for(const auto i : N)
        {
            auto golden = (i >= 0);
            errors[0] ||= (zero[i] != golden);
            unit::assert(tag, !errors[0]);
        }

        auto five = mask_greater_equal<N, count_t<5>>(5);
        static for(const auto i : N)
        {
            auto golden = (i >= 5);
            errors[1] ||= (five[i] != golden);
            unit::assert(tag, !errors[1]);
        }

        auto largest = mask_greater_equal<N, count_t<N>>(N);
        static for(const auto i : N)
        {
            auto golden = (i >= 8);
            errors[2] ||= (largest[i] != golden);
            unit::assert(tag, !errors[2]);
        }
    }

    {
        auto zero = mask_less_than<N, index_t<N>>(0);
        static for(const auto i : N)
        {
            auto golden = (i < 0);
            errors[3] ||= (zero[i] != golden);
            unit::assert(tag, !errors[3]);
        }

        auto five = mask_less_than<N, count_t<5>>(5);
        static for(const auto i : N)
        {
            auto golden = (i < 5);
            errors[4] ||= (five[i] != golden);
            unit::assert(tag, !errors[4]);
        }

        auto largest = mask_less_than<N, count_t<N>>(N);
        static for(const auto i : N)
        {
            auto golden = (i < 8);
            errors[5] ||= (largest[i] != golden);
            unit::assert(tag, !errors[5]);
        }
    }

    {
        auto zero = mask_greater_than<N, index_t<N>>(0);
        static for(const auto i : N)
        {
            auto golden = (i > 0);
            errors[6] ||= (zero[i] != golden);
            unit::assert(tag, !errors[6]);
        }

        auto five = mask_greater_than<N, count_t<5>>(5);
        static for(const auto i : N)
        {
            auto golden = (i > 5);
            errors[7] ||= (five[i] != golden);
            unit::assert(tag, !errors[7]);
        }

        auto largest = mask_greater_than<N, count_t<N>>(N);
        static for(const auto i : N)
        {
            auto golden = (i > 8);
            errors[8] ||= (largest[i] != golden);
            unit::assert(tag, !errors[8]);
        }
    }

    {
        auto zero = mask_less_equal<N, index_t<N>>(0);
        static for(const auto i : N)
        {
            auto golden = (i <= 0);
            errors[9] ||= (zero[i] != golden);
            unit::assert(tag, !errors[9]);
        }

        auto five = mask_less_equal<N, count_t<5>>(5);
        static for(const auto i : N)
        {
            auto golden = (i <= 5);
            errors[10] ||= (five[i] != golden);
            unit::assert(tag, !errors[10]);
        }

        auto largest = mask_less_equal<N, count_t<N>>(N);
        static for(const auto i : N)
        {
            auto golden = (i <= 8);
            errors[11] ||= (largest[i] != golden);
            unit::assert(tag, !errors[11]);
        }
    }

    print("mask test complete\n");
}

inline void TestReductionOr(unit::tag_t tag)
{
    print("OR reduction test start\n");

    const auto N = 8;

    const uint<N> all_zeroes = 0;
    const uint<N> all_ones = cast<uint<N>>(-1);

    unit::assert_equal(tag, false, reduction_or<uint<N>>(all_zeroes));
    unit::assert_equal(tag, true, reduction_or(all_ones));
    static for(const auto i : N)
    {
        uint<N> x = (1 << i);
        unit::assert_equal(tag, true, reduction_or(x));
    }

    print("OR reduction test complete\n");
}

inline void TestReductionAnd(unit::tag_t tag)
{
    print("AND reduction test start\n");

    const auto N = 8;

    const uint<N> all_zeroes = 0;
    const uint<N> all_ones = cast<uint<N>>(-1);

    unit::assert_equal(tag, false, reduction_and<uint<N>>(all_zeroes));
    unit::assert_equal(tag, true, reduction_and(all_ones));
    static for(const auto i : N)
    {
        uint<N> x = (1 << i);
        unit::assert_equal(tag, false, reduction_and(x));
    }

    print("AND reduction test complete\n");
}

inline void TestReductionXor(unit::tag_t tag)
{
    print("XOR reduction test start\n");

    const auto N = 8;

    const uint<N> all_zeroes = 0;

    unit::assert_equal(tag, false, reduction_xor<uint<N>>(all_zeroes));
    static for(const auto i : N)
    {
        auto x = (1 << (i+1)) - 1;
        bool odd_number_of_ones = cast<uint1>(pop_count(x)) == 1;
        unit::assert_equal(tag, odd_number_of_ones, reduction_xor(x));
    }

    print("XOR reduction test complete\n");
}

inline void TestReductionXnor(unit::tag_t tag)
{
    print("XNOR reduction test start\n");

    const auto N = 8;

    const uint<N> all_zeroes = 0;

    unit::assert_equal(tag, true, reduction_xnor<uint<N>>(all_zeroes));
    static for(const auto i : N)
    {
        uint<N> x = checked_cast((1 << (i+1)) - 1);
        bool odd_number_of_ones = cast<uint1>(pop_count(x)) == 1;
        unit::assert_equal(tag, !odd_number_of_ones, reduction_xnor(x));
    }

    print("XNOR reduction test complete\n");
}

inline void TestReductionNor(unit::tag_t tag)
{
    print("NOR reduction test start\n");

    const auto N = 8;

    const uint<N> all_zeroes = 0;
    const uint<N> all_ones = cast<uint<N>>(-1);

    unit::assert_equal(tag, true, reduction_nor<uint<N>>(all_zeroes));
    unit::assert_equal(tag, false, reduction_nor(all_ones));
    static for(const auto i : N)
    {
        uint<N> x = (1 << i);
        print("{i=} {x=} {reduction_nor(x)=}\n"); barrier;
        unit::assert_equal(tag, false, reduction_nor(x));
    }

    print("NOR reduction test complete\n");
}

inline void TestReductionNand(unit::tag_t tag)
{
    print("NAND reduction test start\n");

    const auto N = 8;

    const uint<N> all_zeroes = 0;
    const uint<N> all_ones = cast<uint<N>>(-1);

    unit::assert_equal(tag, true, reduction_nand<uint<N>>(all_zeroes));
    unit::assert_equal(tag, false, reduction_nand(all_ones));
    static for(const auto i : N)
    {
        uint<N> x = (1 << i);
        unit::assert_equal(tag, true, reduction_nand(x));
    }

    print("NAND reduction test complete\n");
}

// 4-bit wide struct to test casting functionality of bitwise functions
struct FourBitStruct
{
    uint1[3] ary;
    uint1 z;
}

inline void TestBitwiseOp(unit::tag_t tag, (uint4, uint4)->uint4 oracle, (FourBitStruct, FourBitStruct)->FourBitStruct test)
{
    // Test all combinations of 4-bit inputs
    pipelined_for(256, [tag, oracle, test](uint8 i)
    {
        uint4 a = cast<uint4>(i);
        uint4 b = cast<uint4>(i >> 4);

        uint4 expected = oracle(a, b);

        uint4 actual = cast<uint4>(test(cast<FourBitStruct>(a), cast<FourBitStruct>(b)));

        unit::assert_equal(tag, expected, actual);
    });
}

inline void TestBitwiseAnd(unit::tag_t tag)
{
    TestBitwiseOp(tag, [](uint4 x, uint4 y) { return x & y;}, bitwise_and<FourBitStruct>);
}

inline void TestBitwiseOr(unit::tag_t tag)
{
    TestBitwiseOp(tag, [](uint4 x, uint4 y) { return x | y;}, bitwise_or<FourBitStruct>);
}

inline void TestBitwiseXor(unit::tag_t tag)
{
    TestBitwiseOp(tag, [](uint4 x, uint4 y) { return x ^ y;}, bitwise_xor<FourBitStruct>);
}

inline void TestBitwiseNand(unit::tag_t tag)
{
    TestBitwiseOp(tag, [](uint4 x, uint4 y) { return ~(x & y);}, bitwise_nand<FourBitStruct>);
}

inline void TestBitwiseNor(unit::tag_t tag)
{
    TestBitwiseOp(tag, [](uint4 x, uint4 y) { return ~(x | y);}, bitwise_nor<FourBitStruct>);
}

inline void TestBitwiseXNor(unit::tag_t tag)
{
    TestBitwiseOp(tag, [](uint4 x, uint4 y) { return ~(x ^ y);}, bitwise_xnor<FourBitStruct>);
}

inline void test_main()
{
    print("data.bits test start\n");

    unit::check<1>(TestRotl);
    unit::check<2>(TestRotr);
    unit::check<3>(TestHighestOne);
    unit::check<4>(TestLowestOne);
    unit::check<5>(TestPopCount);
    unit::check<6>(TestPopCountOddNumberOfBits);
    unit::check<7>(TestRoundupToPow2);
    unit::check<8>(TestDivideAndRoundup);
    unit::check<9>(TestCountAndIndex);
    unit::test<10>(TestMask);
    unit::test<11>(TestReductionOr);
    unit::test<12>(TestReductionAnd);
    unit::test<13>(TestReductionXor);
    unit::test<14>(TestReductionXnor);
    unit::test<15>(TestReductionNor);
    unit::test<16>(TestReductionNand);
    unit::test<17>(TestBitwiseAnd);
    unit::test<18>(TestBitwiseOr);
    unit::test<19>(TestBitwiseXor);
    unit::test<20>(TestBitwiseNand);
    unit::test<11>(TestBitwiseNor);
    unit::test<12>(TestBitwiseXNor);

    print("data.bits test complete\n");
}
