// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright: (c) Microsoft Corporation. All rights reserved.
//
// Integer operators.
module numeric.int.operator
    { abs
    , add
    , div
    , ceil_div
    , mod
    , div_mod
    , ceil_div_mod
    , increment
    , xor
    }

import data.tuple

//| Compute the absolute value of `a`.
// The return type is an unsigned integer of the same width as `a`.
template<typename T>
inline uint<bitsizeof T> abs(T a)
{
    // To avoid 2:1 muxes for each output bit
    // the result is computed as `(a ^ mask) + addend`
    // When `a` is positive, `mask = 0` and `addend = 0`
    // and so the logic simply returns `a`.
    // When `a` is negative, `mask = -1` and `addend = 1`
    // which has the effect of flipping all bits and then adding one
    // which is what the unary negate (`-`) operator does.
    bool is_neg = a < 0;

    bool[bitsizeof T] mask_bits;
    static for (const auto i : bitsizeof T)
    {
        mask_bits[i] = is_neg;
    }

    T mask = cast<T>(mask_bits);

    uint1 addend = cast<uint1>(is_neg);

    return cast<uint<bitsizeof T>>((a ^ mask) + addend);
}

//| Add two values of the input type `T` and return the result as the output type `R`,
// which might be larger.  e.g. 3 fits in a `uint2`, but 3 + 3 does not.
//
// #### __Example__
//
//     >>> add<uint3>(3, 3);
//     6
template <typename R, typename T>
inline R add(T a, T b)
{
    return a + b;
}

//| Returns the floor of the quotient when dividing `numerator` by `denominator`.
inline auto div(auto numerator, auto denominator)
{
    assert(denominator > 0);
    return numerator / denominator;
}

//| Returns the ceiling of the quotient when dividing `numerator` by `denominator`.
inline auto ceil_div(auto numerator, auto denominator)
{
    assert(denominator > 0);

    auto denominator_minus_one = cast<decltype(denominator)>(denominator - 1);

    return cast<decltype(numerator)>((numerator + denominator_minus_one) / denominator);
}

//| Returns the remainder when dividing `numerator` by `denominator`.
// `Offset` is used to to adjust the range of repeated values.
// For example, if `mod<4, 0>` is applied to the array: `[0, 1, 2, 3, 4, 5]`
// the result is: `[0, 1, 2, 3, 0, 1]`.
// If `mod<4, 1>` is applied to the same array, the result is:
// `[4, 1, 2, 3, 4, 1]`

template<auto Offset = 0>
inline auto mod(auto numerator, auto denominator)
{
    static if(Offset == 0)
    {
        return numerator % denominator;
    }
    else
    {
        return (cast<decltype(numerator)>(numerator - Offset) % denominator) + Offset;
    }
}

//| Returns the floor of the quotient and remainder when dividing `numerator` by `denominator`.
// The quotient is returned in `first`.
// The remainder is returned in `second`.
// `Offset` is used to to adjust the range of repeated values.
// For example, if `mod<4, 0>` is applied to the array: `[0, 1, 2, 3, 4, 5]`
// the result is: `[0, 1, 2, 3, 0, 1]`.
// If `mod<4, 1>` is applied to the same array, the result is:
// `[4, 1, 2, 3, 4, 1]`
template<auto Offset = 0>
inline auto div_mod(auto numerator, auto denominator)
{
    return make_pair(div(numerator, denominator), mod<Offset>(numerator, denominator));
}

//| Returns the ceiling of the quotient and remainder when dividing `numerator` by `denominator`.
// The quotient is returned in `first`.
// The remainder is returned in `second`.
// `Offset` is used to to adjust the range of repeated values.
// For example, if `mod<4, 0>` is applied to the array: `[0, 1, 2, 3, 4, 5]`
// the result is: `[0, 1, 2, 3, 0, 1]`.
// If `mod<4, 1>` is applied to the same array, the result is:
// `[4, 1, 2, 3, 4, 1]`
template<auto Offset = 0>
inline auto ceil_div_mod(auto numerator, auto denominator)
{
    return make_pair(ceil_div(numerator, denominator), mod<Offset>(numerator, denominator));
}

//| Increment the input by 1.
//
//     prop> increment(a) == a + 1
inline auto increment(auto a)
{
    return a + 1;
}

//| Return xor of inputs.
//
// #### __Example__
//
//     >>> xor(2, 3);
//     1
inline auto xor(auto a, auto b)
{
    return a ^ b;
}