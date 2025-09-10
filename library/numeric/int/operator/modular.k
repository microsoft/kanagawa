// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright: (c) Microsoft Corporation. All rights reserved.
//
// Modular arithmetic operators.
module numeric.int.operator.modular
    { add
    , increment
    , increment_if
    , sub
    , decrement
    , decrement_if
    }

import numeric.int.limits
import type.stdtype

template<typename T>
inline bool sign_bit(T input)
{
    auto input_bits = cast<bool[bitsizeof input]>(input);

    return input_bits[(bitsizeof input) - 1];
}

//| Returns `(a + b) mod M`.
// `a` and `b` must be less than `M`.
template
    < typename T                    //< Type of the first operand.  Must be unsigned.
    , typename U                    //< Type of the second operand.  Must be unsigned.
    , auto M = (1 << bitsizeof(T))  //< Modulus value.
    >
inline auto add(T a, U b)
{
    static assert(!limits<T>::is_signed);
    static assert(!limits<U>::is_signed);

    // Inputs must be less than M
    assert(a < M);
    assert(b < M);

    static if (((M & (M - 1)) == 0))
    {
        // M is a power of 2, simply throw away upper bits
        return cast<index_t<M>>(a + b);
    }
    else
    {
        auto result = a + b;

        // Check if the addition overflowed, substract the modulus value if it did.
        // This is done with a subtraction (expensive operation) followed by checking the sign bit (cheap operation)
        // rather than comparison (expensive) followed by subtraction (expensive)
        auto diff = result - M;

        if (!sign_bit(diff))
        {
            // A subtraction of M is sufficient to restore result to [0, M)
            // because both inputs are < M
            result = cast<decltype(result)>(diff);
        }

        assert(result < M);

        return cast<index_t<M>>(result);
    }
}

//| Returns `(a + 1) mod M`.
// `a` must be less than `M`.
template
    < typename T                    //< Type of the operand.  Must be unsigned.
    , auto M = (1 << bitsizeof(T))  //< Modulus value.
    >
inline auto increment(T a)
{
    return add<T, uint1, M>(a, 1);
}

//| If `b` is true, then returns `(a + 1) mod M`.
// returns `a` otherwise.
// `a` must be less than `M`.
template
    < typename T                    // Type of a.  Must be unsigned.
    , auto M = (1 << bitsizeof(T))  // Modulus value.
    >
inline auto increment_if(T a, bool b)
{
    return add<T, uint1, M>(a, cast<uint1>(b));
}

//| Returns `(a - b) mod M`.
// `a` and `b` must be less than `M`.
template
    < typename T                    //< Type of the first operand.  Must be unsigned.
    , typename U                    //< Type of the second operand.  Must be unsigned.
    , auto M = (1 << bitsizeof(T))  //< Modulus value.
    >
inline auto sub(T a, U b)
{
    static assert(!limits<T>::is_signed);
    static assert(!limits<U>::is_signed);

    // Inputs must be less than M
    assert(a < M);
    assert(b < M);

    static if (((M & (M - 1)) == 0))
    {
        // M is a power of 2, simply throw away upper bits
        return cast<index_t<M>>(a - b);
    }
    else
    {
        auto result = a - b;

        // Check if the subtraction underflowed.  Add the modulus value if it did.
        // Check for underflow is implemented by checking the sign bit.
        if (sign_bit(result))
        {
            // A single addition of M is sufficient to restore result to [0, M)
            // because both inputs are < M
            result = result + M;

            assert(result >= 0);
        }

        assert(result < M);

        return cast<index_t<M>>(result);
    }
}

//| Returns `(a - 1) mod M`.
// `a` must be less than `M`.
template
    < typename T                    //< Type of the operand.  Must be unsigned.
    , auto M = (1 << bitsizeof(T))  //< Modulus value.
    >
inline auto decrement(T a)
{
    return sub<T, uint1, M>(a, 1);
}

//| If `b` is true, then returns `(a - 1) mod M`.
// returns `a` otherwise.
// `a` must be less than `M`.
template
    < typename T                    // Type of a.  Must be unsigned.
    , auto M = (1 << bitsizeof(T))  // Modulus value.
    >
inline auto decrement_if(T a, bool b)
{
    return sub<T, uint1, M>(a, cast<uint1>(b));
}