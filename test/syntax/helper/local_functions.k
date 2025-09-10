// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.local_functions
    { Foo
    }

template <typename T, typename U, (T, U) -> U Fn>
inline U Foo(T x, U y)
{
    return Fn(x, y);
}

