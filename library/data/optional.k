// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// The optional type and associated operations.
module data.optional
    { optional
    , is_valid
    , make_optional
    , just
    , equal
    , optional_to_pair
    , from_optional
    , join
    , equal_by
    , lift_optional
    , lift2_optional
    }

import data.order as O
import data.tuple as P

template <typename T>
struct optional
{
    bool is_valid;
    T value;
}

template <typename T>
inline bool is_valid(optional<T> x)
{
    return x.is_valid;
}

template <typename T>
inline optional<T> make_optional(bool is_valid, T value)
{
    return {is_valid, value};
}

//| Create optional with `is_valid` set to true.
inline auto just(auto value)
{
    return make_optional(true, value);
}

//| Compare optionals for equality. Return true when both arguments
// are invalid or both arguments are valid and have equal values.
//
// #### __Examples__
//
//     >>> equal({false, 0x0}, {false, 0x1});
//     true
//
//     >>> equal({true, 0x4}, {false, 0x4});
//     false
//
//     >>> equal({true, 0x4}, {true, 0x4});
//     true
template <typename T>
inline bool equal(optional<T> x, optional<T> y)
{
    return equal_by(O::equal, x, y);
}

//| Convert from `optional<T>` to `pair<bool, T>`.
template <typename T>
inline P::pair<bool, T> optional_to_pair(optional<T> x)
{
    return P::make_pair(x.is_valid, x.value);
}

//| Convert from `optional<T>` to `T`, using a default value if necessary.
//
// #### __Examples__
//
//     >>> from_optional(0x7, {false, 0x2});
//     0x7
//
//     >>> from_optional(0x7, {true, 0x2});
//     0x2
template <typename T>
inline auto from_optional(T default_value, optional<T> x)
{
    return x.is_valid ? x.value : default_value;
}

//| Remove one layer of `optional`, projecting `T` into the outer layer.
//
// #### __Examples__
//
//     >>> join({false, {false, 0xFF}})
//     {false, 0xFF}
//
//     >>> join({false, {true, 0xFF}})
//     {false, 0xFF}
//
//     >>> join({true, {false, 0xFF}})
//     {false, 0xFF}
//
//     >>> join({true, {true, 0xFF}})
//     {true, 0xFF}
template<typename T>
inline optional<T> join(optional<optional<T>> x)
{
    return make_optional(x.is_valid && x.value.is_valid, x.value.value);
}

//| Equality comparison of optionals using the specified function.
//
// #### __Examples__
//
//     >>> equal_by([](uint8 x, uint8 y){ return even(x) && even(y); }, {true, 0}, {false, 2})
//     false
//
//     >>> equal_by([](uint8 x, uint8 y){ return even(x) && even(y); }, {true, 0}, {true, 2})
//     true
//
//     >>> equal_by([](uint8 x, uint8 y){ return even(x) && even(y); }, {false, 0}, {false, 3})
//     true
template <typename T>
inline bool equal_by((T, T) -> bool equality_fn, optional<T> x, optional<T> y)
{
    return (!x.is_valid && !y.is_valid) || (x.is_valid && y.is_valid && equality_fn(x.value, y.value));
}

//| Returns a function with the signature: `(optional<A>)->optional<decltype(fn(A))>`.
// The result is valid if and only if the input is valid.
// `fn` is called to compute the resulting value.
// If the result is invalid, then `fn` is not called, and the result value is undefined.
inline auto lift_optional(
    (auto) -> auto fn //< Function used to convert values.
    )
{
    return [fn](auto x)
    {
        optional<decltype(fn(x))> result = {};

        // If statement to ensure that if `fn` has side effects
        // then it is not called when `x.is_valid` is false
        if (x.is_valid)
        {
            result = make_optional(true, fn(x.value));
        }

        return result;
    };
}

//| Returns a function with the signature: `(optional<A>, optional<B>)->optional<decltype(fn(A, B))>`.
// The result is valid if and only if both inputs are valid.
// `fn` is called to compute the resulting value.
// If the result is invalid, then `fn` is not called, and the result value is undefined.
inline auto lift2_optional(
    (auto, auto) -> auto fn //< Function used to convert values.
    )
{
    return [fn](auto x, auto y)
    {
        optional<decltype(fn(x, y))> result = {};

        // If statement to ensure that if `fn` has side effects
        // then it is not called when either `x.is_valid` or `y.is_valid` are false
        if (x.is_valid && y.is_valid)
        {
            result = make_optional(true, fn(x.value, y.value));
        }

        return result;
    };
}