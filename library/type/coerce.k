// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module type.coerce
    { reinterpret_cast
    , static_cast
    , checked_cast
    , safe_cast
    , optional
    }

import data.optional

//| Forcibly convert the input type to the output type `T`.
// This overrides any compile-time check from the built-in `cast` operator
// by casting the input to an intermediate `uint` type before casting to `T`.
// If `T` is wider than the input, then due to the intermediate trip through `uint`
// the result is zero extended.
// If `T` is narrower than the input, truncate the most-significant bits.
template <typename T /*< Output*/>
inline T reinterpret_cast(auto x)
{
    return cast<T>(cast<uint<bitsizeof T>>(cast<uint<bitsizeof x>>(x)));
}

//| A wrapper around the built-in `cast` operator, allowing it to be used
// as a function
template <typename T>
inline T static_cast(auto x)
{
    return cast<T>(x);
}

//| `cast` then `assert` that the output equals the input.
template <typename T>
inline T checked_cast(auto x)
{
    auto y = cast<T>(x);

    assert(y == x);

    return y;
}

//| Cast a value from type `From` to type `To` and return
// whether the cast was done without losing information.
// For example, the `uint32` value `255` can be cast to a `uint8`
// without losing information, but casting the `uint32` value
// `257` to a `uint8` is lossy.
template<typename To, typename From>
inline optional<To> safe_cast(From input)
{
    To result = cast<To>(input);

    bool castWasSafe = (cast<From>(result) == input);

    return make_optional<To>(castWasSafe, result);
}