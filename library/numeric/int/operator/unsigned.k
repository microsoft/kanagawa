// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright: (c) Microsoft Corporation. All rights reserved.
//
// Unsigned arithmetic operators.
module numeric.int.operator.unsigned
    { sub
    , decrement
    , decrement_if
    }

import numeric.int.limits
import type.coerce

//| Returns `t - u`.
// The result type is the same (unsigned) type as `t`.
// `u` must be no greater than `t` (to ensure the result is non-negative).
template
    < typename T
    , typename U
    >
inline T sub(T t, U u)
{
    static assert(!limits<T>::is_signed);
    static assert(!limits<U>::is_signed);

    assert(u <= t);

    return static_cast<T>(t - u);
}

//| Returns `t - 1`.
// The result type is the same (unsigned) type as `t`.
// `t` must be positive (to ensure the result is positive)
template
    < typename T
    >
inline T decrement(T t)
{
    return sub(t, 1);
}

//| If `b` is true, then returns `t - 1`.
// Returns `t` otherwise.
// The result type is the same (unsigned) type as `t`.
// The caller must ensure that the decrement would not result in a negative
// number.
template
    < typename T
    >
inline T decrement_if(T t, bool b)
{
    return sub(t, cast<uint1>(b));
}