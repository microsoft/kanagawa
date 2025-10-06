// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.random.lfsr
import test.unit as unit
import test.runner

// Tests for decomposing wide subtract operations

inline bool ExtractBit(auto value, auto bitIndex)
{
    bool result = reinterpret_cast<bool>(value >> bitIndex);

    if (bitIndex >= bitsizeof(value))
    {
        // Sign-extend if necessary
        result = (value < 0) ? true : false;
    }

    return result;
}

// bit-blasted subtract
// Used to avoid the decomposition being tested
// from being used to computed expected values
template<typename ResultType, typename LhsType, typename RhsType>
inline ResultType BitwiseSub(LhsType lhs, RhsType rhs)
{
    bool[bitsizeof ResultType] resultArray;

    bool borrow = false;

    static for (const auto i : bitsizeof ResultType)
    {
        int2 a = cast<uint1>(ExtractBit(lhs, i)) - cast<uint1>(borrow);
        uint1 b = cast<uint1>(ExtractBit(rhs, i));

        int2 diff = static_cast(a - b);

        resultArray[i] = ExtractBit(diff, 0);
        borrow = ExtractBit(diff, 1);
    }

    return cast<ResultType>(resultArray);
}

template<typename ResultType, typename LhsType, typename RhsType>
inline void RandomTest(unit::tag_t tag)
{
    pipelined_for(1024, [tag](index_t<1024> i)
    {
        static lfsr_random<32> lhsLfsr;
        static lfsr_random<32> rhsLfsr;

        LhsType lhs = static_cast(lhsLfsr.next(make_optional(i == 0, 0xcafecafe)));
        RhsType rhs = static_cast(lhsLfsr.next(make_optional<uint32>(i == 0, 0x12345678)));

        ResultType actual = static_cast(lhs - rhs);

        ResultType expected = static_cast(BitwiseSub<ResultType>(lhs, rhs)); //lhs + (~rhs + 1));

        unit::assert_equal(tag, expected, actual);
    });
}

inline void test_main()
{
    unit::test<1>(RandomTest<uint32, uint32, uint32>);
    unit::test<2>(RandomTest<int32, int32, int32>);
    unit::test<3>(RandomTest<int32, int8, uint32>);
    unit::test<4>(RandomTest<int33, int32, uint8>);
    unit::test<5>(RandomTest<int28, int27, int25>);
    unit::test<6>(RandomTest<uint28, uint27, uint25>);
    unit::test<7>(RandomTest<uint16, uint16, uint16>);
}
