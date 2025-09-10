// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.array
import data.array as A
import data.function
import data.optional
import data.optional as O
import data.order
import data.tuple as P
import numeric.int.operator
import test.unit as unit
import test.runner

// Function: TestPartialSort
//
// Tests partial_sort
inline bool TestPartialSort()
{
    bool result = true;

    const auto N = 10;
    uint32[N] a;

    static for(const auto i : N)
    {
        a[i] = i + 3;
    }

    // Test min partial_sort with function
    result = pipelined_last(N, [a, N, result](index_t<N> tid)
    {
        using T = decltype(a[0]);
        bool last = (tid == N-1);

        optional<uint32>[3] m = partial_sort<3>(a[tid], [](T lhs, T rhs){ return lhs < rhs; }, last);
        if (tid == 0)
        {
            result &&= (m[0].is_valid && !m[1].is_valid && !m[2].is_valid);
            result &&= (m[0].value == 3);
            assert(result);
        }
        else if (tid == 1)
        {
            result &&= (m[0].is_valid && m[1].is_valid && !m[2].is_valid);
            result &&= (m[0].value == 3 && m[1].value == 4);
            assert(result);
        }
        else if (tid >= 2)
        {
            result &&= (m[0].is_valid && m[1].is_valid && m[2].is_valid);
            result &&= (m[0].value == 3 && m[1].value == 4 && m[2].value == 5);
            assert(result);
        }
        return result;
    });

    // Test max partial_sort with lambda
    result = pipelined_last(N, [a, N, result](index_t<N> tid)
    {
        using T = decltype(a[0]);
        bool last = (tid == N-1);
        auto m = partial_sort<2>(a[tid], [](T lhs, T rhs){ return lhs > rhs; }, last);
        if (tid == 0)
        {
            result &&= (m[0].is_valid && !m[1].is_valid);
            result &&= (m[0].value == 3);
            assert(result);
        }
        else if (tid == 1)
        {
            result &&= (m[0].is_valid && m[1].is_valid);
            result &&= (m[0].value == 4 && m[1].value == 3);
            assert(result);
        }
        else if (tid == 2)
        {
            result &&= (m[0].is_valid && m[1].is_valid);
            result &&= (m[0].value == 5 && m[1].value == 4);
            assert(result);
        }
        else if (tid == 3)
        {
            result &&= (m[0].is_valid && m[1].is_valid);
            result &&= (m[0].value == 6 && m[1].value == 5);
            assert(result);
        }
        else if (last)
        {
            result &&= (m[0].is_valid && m[1].is_valid);
            result &&= (m[0].value == 12 && m[1].value == 11);
            assert(result);
        }
        return result;
    });

    return result;
}

template<typename T, auto N>
inline bool AssertArraysEqual(T[N] lhs, T[N] rhs)
{
    bool[N] eq;
    static for(const auto i : N)
        eq[i] = (lhs[i] == rhs[i]);
    bool result = and(eq);
    if (!result)
        print("{lhs} != {rhs}\n");
    return result;
}

inline bool TestShiftArray()
{
    bool result = true;

    using T = uint8;
    T[32] data32;
    static for(const auto i : 32)
        data32[i] = i;

    T[5] data5;
    static for(const auto i : 5)
        data5[i] = i;

    // Shift right
    {
        // Shift 32 right by 0
        T[32] out = shift_array_right(data32, 0);
        T[32] expected = { 0, 1, 2, 3, 4, 5, 6, 7,
                           8, 9, 10, 11, 12, 13, 14, 15,
                           16, 17, 18, 19, 20, 21, 22, 23,
                           24, 25, 26, 27, 28, 29, 30, 31 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 32 right by 8
        auto out = shift_array_right<32>(data32, 8);
        T[32] expected = { 8, 9, 10, 11, 12, 13, 14, 15,
                           16, 17, 18, 19, 20, 21, 22, 23,
                           24, 25, 26, 27, 28, 29, 30, 31,
                           0, 0, 0, 0, 0, 0, 0, 0 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 32 right by 3 (returning 7)
        T[7] out = shift_array_right(data32, 3);
        T[7] expected = { 3, 4, 5, 6, 7, 8, 9 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 32 right by 29 (returning 7)
        T[7] out = shift_array_right(data32, 29);
        T[7] expected = { 29, 30, 31, 0, 0, 0, 0 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 32 right by 61 (returning 61)
        T[61] out = shift_array_right(data32, 61);
        T[61] expected = {};
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 5 right by 0
        T[5] out = shift_array_right(data5, 0);
        T[5] expected = { 0, 1, 2, 3, 4 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 5 right by 2 (returning 4)
        T[4] out = shift_array_right(data5, 2);
        T[4] expected = { 2, 3, 4, 0 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 5 right by 10 (returning 4)
        T[4] out = shift_array_right(data5, 10);
        T[4] expected = { 0, 0, 0, 0 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }

    // Shift left
    {
        // Shift 32 left by 0
        auto out = shift_array_left<32>(data32, 0);
        T[32] expected = { 0, 1, 2, 3, 4, 5, 6, 7,
                           8, 9, 10, 11, 12, 13, 14, 15,
                           16, 17, 18, 19, 20, 21, 22, 23,
                           24, 25, 26, 27, 28, 29, 30, 31 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 32 left by 8
        auto out = shift_array_left<32>(data32, 8);
        T[32] expected = { 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 1, 2, 3, 4, 5, 6, 7,
                           8, 9, 10, 11, 12, 13, 14, 15,
                           16, 17, 18, 19, 20, 21, 22, 23 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 32 left by 3 (returning 7)
        auto out = shift_array_left<7>(data32, 3);
        T[7] expected = { 0, 0, 0, 0, 1, 2, 3 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 32 left by 7 (returning 7)
        T[7] out = shift_array_left(data32, 7);
        T[7] expected = { 0, 0, 0, 0, 0, 0, 0 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 32 left by 61 (returning 61)
        T[61] out = shift_array_left(data32, 61);
        T[61] expected = {};
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 5 left by 0
        T[5] out = shift_array_left(data5, 0);
        T[5] expected = { 0, 1, 2, 3, 4 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 5 left by 2 (returning 4)
        T[4] out = shift_array_left(data5, 2);
        T[4] expected = { 0, 0, 0, 1 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Shift 5 left by 10 (returning 4)
        T[4] out = shift_array_left(data5, 10);
        T[4] expected = { 0, 0, 0, 0 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }

    return result;
}

inline bool TestRotateArray()
{
    bool result = true;

    using T = uint8;
    const auto N = 32;
    T[N] data32;
    static for(const auto i : N)
        data32[i] = i;

    T[5] data5;
    static for(const auto i : 5)
        data5[i] = i;

    // Rotate right
    {
        // Rotate 32 right by 0
        auto out = rotate_array_right<32>(data32, 0);
        T[32] expected = { 0, 1, 2, 3, 4, 5, 6, 7,
                           8, 9, 10, 11, 12, 13, 14, 15,
                           16, 17, 18, 19, 20, 21, 22, 23,
                           24, 25, 26, 27, 28, 29, 30, 31 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 32 right by 8
        auto out = rotate_array_right<32>(data32, 8);
        T[32] expected = { 8, 9, 10, 11, 12, 13, 14, 15,
                           16, 17, 18, 19, 20, 21, 22, 23,
                           24, 25, 26, 27, 28, 29, 30, 31,
                           0, 1, 2, 3, 4, 5, 6, 7 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 32 right by 3 (returning 7)
        T[7] out = rotate_array_right(data32, 3);
        T[7] expected = { 3, 4, 5, 6, 7, 8, 9 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 32 right by 29 (returning 7)
        T[7] out = rotate_array_right(data32, 29);
        T[7] expected = { 29, 30, 31, 0, 1, 2, 3 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 32 right by 30
        T[32] out = rotate_array_right(data32, 30);
        T[32] expected = { 30, 31, 0, 1, 2, 3, 4, 5,
                           6, 7, 8, 9, 10, 11, 12, 13,
                           14, 15, 16, 17, 18, 19, 20, 21,
                           22, 23, 24, 25, 26, 27, 28, 29 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 5 right by 0
        T[5] out = rotate_array_right(data5, 0);
        T[5] expected = { 0, 1, 2, 3, 4 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 5 right by 2 (returning 4)
        T[4] out = rotate_array_right(data5, 2);
        T[4] expected = { 2, 3, 4, 0 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }

    // Rotate left
    {
        // Rotate 32 left by 0
        auto out = rotate_array_left<32>(data32, 0);
        T[32] expected = { 0, 1, 2, 3, 4, 5, 6, 7,
                           8, 9, 10, 11, 12, 13, 14, 15,
                           16, 17, 18, 19, 20, 21, 22, 23,
                           24, 25, 26, 27, 28, 29, 30, 31 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 32 left by 8
        auto out = rotate_array_left<32>(data32, 8);
        T[32] expected = { 24, 25, 26, 27, 28, 29, 30, 31,
                           0, 1, 2, 3, 4, 5, 6, 7,
                           8, 9, 10, 11, 12, 13, 14, 15,
                           16, 17, 18, 19, 20, 21, 22, 23 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 32 left by 3 (returning 7)
        T[7] out = rotate_array_left(data32, 3);
        T[7] expected = { 29, 30, 31, 0, 1, 2, 3 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 32 left by 7 (returning 7)
        T[7] out = rotate_array_left(data32, 7);
        T[7] expected = { 25, 26, 27, 28, 29, 30, 31 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 32 left by 29
        T[32] out = rotate_array_left(data32, 29);
        T[32] expected = { 3, 4, 5, 6, 7, 8, 9, 10,
                           11, 12, 13, 14, 15, 16, 17, 18,
                           19, 20, 21, 22, 23, 24, 25, 26,
                           27, 28, 29, 30, 31, 0, 1, 2 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 5 left by 0
        auto out = rotate_array_left<5>(data5, 0);
        T[5] expected = { 0, 1, 2, 3, 4 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }
    {
        // Rotate 5 right by 2 (returning 4)
        T[4] out = rotate_array_left(data5, 2);
        T[4] expected = { 3, 4, 0, 1 };
        result &&= AssertArraysEqual(out, expected);
        assert(result);
    }

    return result;
}

//
// Reduce Test
//
inline auto plus(auto a, auto b)
{
    return a + b;
}

template <auto N>
inline uint8 acc_sum(uint8[N] ns)
{
    uint8 acc = 0;
    static for(const auto i : N)
        acc += ns[i];
    return acc;
}

template <auto N>
inline bool TestReduceArray()
{
    bool result = true;

    static if (N > 1)
    {
        result = TestReduceArray<N - 1>();
    }

    // 1,2,3...N
    uint8[N] nats;
    static for(const auto i : N)
        nats[i] = i+1;

    uint8 expected = acc_sum(nats);
    uint8 actual = reduce(plus, nats);
    if (expected != actual)
    {
        print("Error: array reduce returned wrong value for sum of values. Expecting: {expected} but actual was: {actual}\n");
        result = false;
    }
    return result;
}

inline bool TestReduceStruct()
{
    bool result = true;

    struct Foo
    {
        uint8 x;
    }

    inline Foo ReduceSumFoo(Foo x, Foo y)
    {
        return {x.x + y.x};
    }

    Foo[7] data = {{1}, {2}, {3}, {4}, {5}, {6}, {7}};

    const uint8 expected = 1+2+3+4+5+6+7; // 0x1C

    auto actual = reduce(ReduceSumFoo, data);
    if (expected != actual.x)
    {
        print("Error: struct reduce returned wrong value for sum of non-power of two values. Expecting: {expected} but actual was: {actual}\n");
        result = false;
    }

    return result;
}

inline bool TestReduce()
{
    bool result = true;

    print("reduce test start\n");
    print("reduce array test start\n");

    result &&= TestReduceArray<8>();

    print("reduce array test complete\n");
    print("reduce struct test start\n");

    result &&= TestReduceStruct();

    print("reduce struct test complete\n");
    print("reduce test complete\n");

    return result;
}

inline bool TestFunctional()
{
    print("functional test start\n");
    uint32[10] a;

    static for(const auto i : 10)
    {
        a[i] = i + 3;
    }

    // Is any a[i] less than 5?
    assert(true == any(bind2nd(less_than, 5), a));

    // Is 5 less than any a[i]?
    assert(true == any(bind1st(less_than, 5), a));

    // Is any a[i] less than 3?
    assert(false == any(bind2nd(less_than, 3), a));

    // Is 3 less than any a[i]?
    assert(true == any(bind1st(less_than, 3), a));

    // Is 2 less than all a[i]?
    assert(true == all(bind2nd(flip(less_than), 2), a));

    // Are all a[i] not less than 3?
    assert(true == all(bind2nd(not2(less_than), 3), a));

    bool[10] b;

    static for(const auto i : 10)
    {
        b[i] = i > 4;
    }

    assert(false == and(b));

    auto x = zip(b, a);

    auto y = zip(b, indices<10, uint32>());

    assert(first(x[3]) == b[3]);
    assert(second(x[3]) == a[3]);
    assert(second(y[3]) == 3);

    int7[7] z = indices();

    static for (const auto i : 7)
    {
        assert(z[i] == i);
    }

    print("functional test complete\n");
    return true;
}

// Function: TestMinMaxSum
//
// Tests minimum, maximum, and sum functions.
inline bool TestMinMaxSum()
{
    assert(-1 == minimum({-1}));
    assert(-10 == minimum({-1, -2, -3, -4, -5, -6, -7, -8, -9, -10}));
    assert(-1 == maximum({-1}));
    assert(-1 == maximum({-1, -2, -3, -4, -5, -6, -7, -8, -9, -10}));

    bool result = true;

    uint32[10] a;

    static for(const auto i : 10)
    {
        a[i] = i + 3;
    }

    assert(3 == minimum(a));

    assert(12 == maximum(a));

    assert(75 == sum<uint32>(a));

    // Test sum for InputType != OutputType
    assert(75 == sum<uint7>(a));
    assert(75 == sum<uint64>(a));

    int32[10] b;

    static for(const auto i : 10)
    {
        b[i] = i - 3;
    }

    assert(-3 == minimum(b));

    assert(6 == maximum(b));

    assert(15 == sum<int32>(b));

    // Test sum for InputType != OutputType
    assert(15 == sum<int5>(b));
    assert(15 == sum<int64>(b));

    return result;
}

// Function: NaivePrefixSum
//
// Naive implementation of prefix sum to generate known good results to
// compare with the efficient implementation.
template <typename R, typename T, auto N>
inline R[N] NaivePrefixSum(T[N] input)
{
    R[N] result;

    result[0] = input[0];

    for (const auto i : N - 1)
    {
        result[i + 1] = result[i] + input[i + 1];
    }

    return result;
}

template<auto array_size>
inline bool TestPrefixSumHelper()
{
    bool result = true;

    auto naturals = indices<array_size>();
    uint32[array_size] expected = NaivePrefixSum(naturals);
    uint32[array_size] actual = prefix_sum(naturals);

    for (const auto i : array_size)
    {
        if (expected[i] != actual[i])
        {
            print("Incorrect value at index {i} expected {expected[i]} actual {actual[i]} array_size {array_size}\n");
            result = false;
        }
    }

    return result;
}

inline bool TestPrefixSum()
{
    bool result = true;

    // Test a few different array sizes.
    // The implementation of prefix_sum switches between a pair
    // of intermediate result arrays, and it's important to test
    // with both odd and even array sizes, and also values that
    // are/aren't powers of 2.
    if (!TestPrefixSumHelper<4>())
    {
        result = false;
    }

    if (!TestPrefixSumHelper<7>())
    {
        result = false;
    }

    if (!TestPrefixSumHelper<16>())
    {
        result = false;
    }

    if (!TestPrefixSumHelper<17>())
    {
        result = false;
    }

    if (!TestPrefixSumHelper<23>())
    {
        result = false;
    }

    if (!TestPrefixSumHelper<24>())
    {
        result = false;
    }

    return result;
}

inline bool TestCopyArray()
{
    bool result = true;

    const auto source_size = 16;
    const auto destination_size = 40;
    index_t<source_size> source_start_index = 2;
    const auto destination_start_index = 6;
    count_t<source_size> elements_to_copy = 5;

    uint16[source_size] source;
    uint16[destination_size] destination;

    // Fill source and destination with multiples of 7 and 19 respectively.
    // Since they're both primes there aren't any overlapping members in
    // the range we're testing with.
    static for (const auto i : source_size)
    {
        source[i] = (i + 1) * 7;
    }

    static for (const auto i : destination_size)
    {
        destination[i] = (i + 1) * 19;
    }

    // Copy 5 elements from source, starting at source index 2.
    // Source is;        7, 14, 21, 28, 35,  42,  49,  56,  63, etc.
    // Destination is:  19, 38, 57, 76, 95, 114, 133, 152, 171, 190, 209, 228, 247, etc.
    // Expected result: 19, 38, 57, 76, 95, 114,  21,  28,  35,  42,  49, 228, 247, etc.
    destination = copy_array(source, source_start_index, destination, destination_start_index, elements_to_copy);
    for (const auto i : destination_size)
    {
        uint16 expected_value;

        if (i < destination_start_index || i >= destination_start_index + elements_to_copy)
        {
            // The entries at the beginning and end should have been unmodified by
            // copy_array and should still be multiples of 19.
            expected_value = (i + 1) * 19;
        }
        else
        {
            // The values in the middle should have been overwritten with multiples
            // of 7 from the source array.  Note the adjustment to i because the
            // source and destination indices were different.
            expected_value = (i + 1 + source_start_index - destination_start_index) * 7;
        }

        if (destination[i] != expected_value)
        {
            print("Wrong value at index {i} expected {expected_value} actual {destination[i]}\n");
            result = false;
        }
    }

    // Refill destination with multiples of 19.
    static for (const auto i : destination_size)
    {
        destination[i] = (i + 1) * 19;
    }

    // Test copying the entire source array this time.
    source_start_index = 0;
    elements_to_copy = source_size;
    destination = copy_array(source, source_start_index, destination, destination_start_index, elements_to_copy);
    for (const auto i : destination_size)
    {
        uint16 expected_value;

        if (i < destination_start_index || i >= destination_start_index + elements_to_copy)
        {
            // The entries at the beginning and end should have been unmodified by
            // copy_array and should still be multiples of 19.
            expected_value = (i + 1) * 19;
        }
        else
        {
            // The values in the middle should have been overwritten with multiples
            // of 7 from the source array.  Note the adjustment to i because the
            // source and destination indices were different.
            expected_value = (i + 1 + source_start_index - destination_start_index) * 7;
        }

        if (destination[i] != expected_value)
        {
            print("Wrong value at index {i} expected {expected_value} actual (full array copy) {destination[i]}\n");
            result = false;
        }
    }

    return result;
}

template<typename T, auto N>
inline bool TestOptionalHelper(optional<T>[N] x, bool valid, T first_value, T last_value)
{
    bool result = true;

    // Get first and last optional
    optional<T> first = first_valid<T, N>(x);
    optional<T> last = last_valid<T, N>(x);

    // Check valid
    if (first.is_valid != valid)
    {
        result = false;
    }
    if (last.is_valid != valid)
    {
        result = false;
    }

    // Check value
    if (valid && (first.value != first_value))
    {
        result = false;
    }
    if (valid && (last.value != last_value))
    {
        result = false;
    }

    return result;
}

// Function: TestOptional
//
// Test functions for reducing array of optionals
template <auto N>
inline bool TestOptional()
{
    bool result = true;

    using T = index_t<N>;

    // Create array of optionals
    optional<T>[N] x;
    static for (const auto i : N)
    {
        x[i].value = i;
    }

    // No valid entries
    result &&= TestOptionalHelper<T, N>(x, false, 0, 0);

    // Single valid entry
    x[2].is_valid = true;
    result &&= TestOptionalHelper<T, N>(x, true, 2, 2);

    // Multiple valid entries
    x[1].is_valid = true;
    x[5].is_valid = true;
    result &&= TestOptionalHelper<T, N>(x, true, 1, 5);

    x[0].is_valid = true;
    x[N - 1].is_valid = true;
    result &&= TestOptionalHelper<T, N>(x, true, 0, N - 1);

    using U = bool;
    // Create array of bool optionals
    optional<U>[N] y;

    // Single valid entry
    y[2] = {true, true};
    result &&= TestOptionalHelper<U, N>(y, true, true, true);

    static for (const auto k : N)
    {
        static for (const auto i : N)
        {
            x[i].is_valid = i >= k;
        }
        result &&= TestOptionalHelper<T, N>(x, true, k, N - 1);

        static for (const auto i : N)
        {
            x[i].is_valid = i <= k;
        }
        result &&= TestOptionalHelper<T, N>(x, true, 0, k);
    }

    return result;
}

inline void test_unique_by(unit::tag_t tag)
{
    unit::assert_equal
        ( tag
        , {{true, {true, 0xFF}}}
        , unique_by(P::equal<bool, uint8>, {{true, 0xFF}})
        );
    unit::assert_equal
        ( tag
        , {{true, {true, 0xFF}}, {false, {true, 0xFF}}, {true, {true, 0xAB}}}
        , unique_by(P::equal<bool, uint8>, {{true, 0xFF}, {true, 0xFF}, {true, 0xAB}})
        );

    unit::assert_equal
        ( tag
        , {{true, {true, 0xFF}}, {false, {true, 0xFF}}}
        , unique_by(O::equal<uint8>, {{true, 0xFF}, {true, 0xFF}})
        );
    unit::assert_equal
        ( tag
        , {{true, {true, 0xFF}}, {true, {false, 0xFF}}}
        , unique_by(O::equal<uint8>, {{true, 0xFF}, {false, 0xFF}})
        );
    unit::assert_equal
        ( tag
        , {{true, {false, 0xFF}}, {false, {false, 0xAB}}}
        , unique_by(O::equal<uint8>, {{false, 0xFF}, {false, 0xAB}})
        );
}

inline void test_unique(unit::tag_t tag)
{
    unit::assert_equal
        ( tag
        , {{true, 0}}
        , unique<uint32>({0})
        );
    unit::assert_equal
        ( tag
        , {{true, 0}, {true, 1}}
        , unique<uint32>({0, 1})
        );
    unit::assert_equal
        ( tag
        , {{true, 0}, {true, 1}, {false, 0}}
        , unique<uint32>({0, 1, 0})
        );
    unit::assert_equal
        ( tag
        , {{true, 0}, {false, 0}, {true, 1}, {false, 1}}
        , unique<uint32>({0, 0, 1, 1})
        );

    unit::assert_equal
        ( tag
        , {{true, true}}
        , unique({true})
        );
    unit::assert_equal
        ( tag
        , {{true, true}, {true, false}}
        , unique({true, false})
        );
    unit::assert_equal
        ( tag
        , {{true, true}, {false, true}}
        , unique({true, true})
        );
    unit::assert_equal
        ( tag
        , {{true, false}, {true, true}, {false, false}}
        , unique({false, true, false})
        );
}

template <typename L, typename R, auto N>
inline bool test_case_unzip(pair<L, R>[N] xs, pair<L[N], R[N]> expected)
{
    pair<L[N], R[N]> unzipped = unzip(xs);
    return A::equal(unzipped.first, expected.first) && A::equal(unzipped.second, expected.second);
}

template <typename L, typename R, typename T, auto N>
inline bool test_case_unzip_with((T) -> pair<L, R> fn, T[N] xs, pair<L[N], R[N]> expected)
{
    pair<L[N], R[N]> unzipped = unzip_with(fn, xs);
    return A::equal(unzipped.first, expected.first) && A::equal(unzipped.second, expected.second);
}

// Test unzip and unzip_with functions.
inline bool test_unzip()
{
    bool result = true;

    result &&= test_case_unzip
        ( {make_pair(true, 0xff), make_pair(false, 0xbe), make_pair(false, 0xef)}
        , make_pair<bool[3], uint8[3]>({true, false, false}, {0xff, 0xbe, 0xef})
        );

    // unzip_with optionals
    result &&= test_case_unzip_with
        ( optional_to_pair<uint32>
        , {{true, 0xcafe}, {false, 0xbeef}, {true, 0x1111}}
        , make_pair<bool[3], uint32[3]>({true, false, true}, {0xcafe, 0xbeef, 0x1111})
        );

    return result;
}

inline void test_zip_with(unit::tag_t tag)
{
    bool [5] xs = {true, false, false, true, false};
    uint32[5] ys = {1, 3, 8534, 21, 9};

    auto result = unzip(zip_with(make_pair<bool, uint32>, xs, ys));

    unit::assert(tag, A::equal(xs, result.first));
    unit::assert(tag, A::equal(ys, result.second));
}

inline bool test_repeat()
{
    bool result = true;

    uint32[4] actual = repeat(5);

    uint32[4] expected = { 5, 5, 5, 5 };

    result &&= A::equal(actual, expected);

    return result;
}

// Exhaustively test all 0-1 sequences of length N. By the 0-1 principle,
// if a network sorts all sequences of 0's and 1's then it also sorts any
// sequence of arbitrary values.
template<auto N>
inline void test_bitonic_sort_helper(unit::tag_t tag)
{
    pipelined_for(1 << N, [tag](uint<N> i)
    {
        // Convert to bit array
        uint1[N] unsorted = cast<uint1[N]>(i);
        // Sort
        uint1[N] sorted = bitonic_sort(unsorted, less_than);

        // Check result
        for (const auto j : N)
        {
            if (j < N - 1)
            {
                unit::assert(tag, sorted[j] <= sorted[j + 1]);
            }
        }
    });
}

inline void test_bitonic_sort(unit::tag_t tag)
{
    // Test sorter with different number of inputs
    test_bitonic_sort_helper<2>(tag);
    test_bitonic_sort_helper<4>(tag);
    test_bitonic_sort_helper<8>(tag);
    test_bitonic_sort_helper<16>(tag);
}

inline void test_iterate(unit::tag_t tag)
{
    bool[4] actual = iterate([](bool x){ return !x; }, true);
    unit::assert_equal(tag, {true, false, true, false}, actual);

    auto a = iterate<3, uint8>(increment, 0);
    unit::assert_equal(tag, {0, 1, 2}, a);

    auto b = iterate<3, optional<int8>>(id, {true, 12});
    unit::assert_equal(tag, {{true, 12}, {true, 12}, {true, 12}}, b);

    auto grow = [](optional<uint8> x)
                {
                    x.value++;
                    return make_optional(x.value < 2, x.value);
                };
    optional<uint8>[4] c = iterate(grow, {true, 0});
    unit::assert_equal(tag, {{true, 0}, {true, 1}, {false, 2}, {false, 3}}, c);
}

inline void test_at(unit::tag_t tag)
{
    unit::assert(tag, 2 == at({0, 1, 2, 3}, 2));
}

inline void test_front(unit::tag_t tag)
{
    unit::assert(tag, 0 == front({0, 1, 2}));
}

inline void test_back(unit::tag_t tag)
{
    unit::assert(tag, 2 == back({0, 1, 2}));
}

inline void test_zip_with_indices(unit::tag_t tag)
{
    uint8[4] a  = {0x00, 0xAB, 0xBC, 0xDE};
    uint8[4] aa = zip_with_indices( [](index_t<4> i, uint8 b){ return i % 2 == 0 ? 0xFF : b; }
                                  , a
                                  );
    unit::assert_equal(tag, {0xFF, 0xAB, 0xFF, 0xDE}, aa);
}

inline void test_map_indices(unit::tag_t tag)
{
    unit::assert_equal(tag, {0, 1, 2}, map_indices<3>(id));
}

inline void test_transpose(unit::tag_t tag)
{
    uint32[4][3] src = {{0, 1, 2}, {3, 4, 5}, {6, 7, 8}, {9, 10, 11}};

    uint32[3][4] result = transpose(src);

    static for (const auto i : 4)
    {
        static for (const auto j : 3)
        {
            unit::assert_equal(tag, src[i][j], result[j][i]);
        }
    }
}

inline void test_folds(unit::tag_t tag)
{
    uint32[5] a = {0, 1, 2, 3, 4};

    unit::assert_equal(tag, reduce(plus, a), foldr(plus, a));
    unit::assert_equal(tag, reduce(plus, a), foldl(plus, a));
    unit::assert_equal(tag, 5, foldr(plus, singleton(5)));
    unit::assert_equal(tag, 5, foldl(plus, singleton(5)));

    unit::assert_equal(tag, 43210, foldr([](uint32 x, uint32 y) -> uint32
             {
                return x + 10 * y;
             }, a));

    unit::assert_equal(tag, 1234, foldl([](uint32 x, uint32 y) -> uint32
             {
                return 10 * x + y;
             }, a));
}

inline void test_drop(unit::tag_t tag)
{
    uint32[5] a = {0, 1, 2, 3, 4};

    unit::assert_equal(tag, a, drop<0>(a));
    unit::assert_equal(tag, {13}, drop<0>(singleton(13)));
    unit::assert_equal(tag, {1, 2, 3, 4}, drop<1>(a));
    unit::assert_equal(tag, {4}, drop<4>(a));
}

inline void test_take(unit::tag_t tag)
{
    uint32[5] a = {0, 1, 2, 3, 4};

    unit::assert_equal(tag, a, take<5>(a));
    unit::assert_equal(tag, {0}, take<1>(a));
    unit::assert_equal(tag, {14}, take<1>(singleton(14)));
    unit::assert_equal(tag, {0, 1, 2, 3}, take<4>(a));

    uint32[3] b = take(a);
    unit::assert_equal(tag, {0, 1, 2}, b);
}

inline void test_tail(unit::tag_t tag)
{
    uint32[5] a = {0, 1, 2, 3, 4};

    unit::assert_equal(tag, {1, 2, 3, 4}, tail(a));
}

inline void test_init(unit::tag_t tag)
{
    uint32[5] a = {0, 1, 2, 3, 4};

    unit::assert_equal(tag, {0, 1, 2, 3}, init(a));
}

inline void test_intersperse(unit::tag_t tag)
{
    int32[5] a = {0, 1, 2, 3, 4};

    unit::assert_equal(tag, {0, -1, 1, -1, 2, -1, 3, -1, 4}, intersperse(-1, a));

    unit::assert_equal(tag, {12}, intersperse(1, singleton(12)));
}

inline void test_main()
{
    print("data.array test start\n");

    unit::check<1>(TestPartialSort);
    unit::test<2>(test_bitonic_sort);
    unit::check<4>(TestShiftArray);
    unit::check<5>(TestRotateArray);
    unit::check<6>(TestReduce);
    unit::check<7>(TestFunctional);
    unit::check<8>(TestMinMaxSum);
    unit::check<9>(TestPrefixSum);
    unit::check<10>(TestCopyArray);
    unit::check<11, 1>(TestOptional<10>);
    unit::check<11, 2>(TestOptional<11>);
    unit::check<11, 3>(TestOptional<16>);
    unit::test<12>(test_unique_by);
    unit::test<13>(test_unique);
    unit::check<14>(test_unzip);
    unit::check<15>(test_repeat);
    unit::test<16>(test_zip_with);
    unit::test<17>(test_iterate);
    unit::test<18>(test_at);
    unit::test<19>(test_front);
    unit::test<20>(test_back);
    unit::test<21>(test_zip_with_indices);
    unit::test<22>(test_map_indices);
    unit::test<23>(test_transpose);
    unit::test<24>(test_folds);
    unit::test<25>(test_drop);
    unit::test<26>(test_take);
    unit::test<27>(test_tail);
    unit::test<28>(test_init);
    unit::test<29>(test_intersperse);

    print("data.array test complete\n");
}
