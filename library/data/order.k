// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Orderings
module data.order
    { min
    , max
    , less_than
    , greater_than
    , equal
    }

//| Return the smaller of two values.
inline auto min(auto a, auto b)
{
    return (a < b) ? a : b;
}

//| Return the larger of two values.
inline auto max(auto a, auto b)
{
    return (a > b) ? a : b;
}

//| Return true if the first argument is less than the second.
inline bool less_than(auto x, auto y)
{
    return x < y;
}

//| Return true if the first argument is greater than the second.
inline bool greater_than(auto x, auto y)
{
    return x > y;
}

//| Return equality of inputs.
inline bool equal(auto x, auto y)
{
    return x == y;
}