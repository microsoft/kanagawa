// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.enum
    { Bar
    , Fn1
    , Fn2
    , C
    }

enum Bar : int32
{
    B1,
    B2 = 3,
    B3 = B1 + B2,
    B4
}

template <typename T, (T) -> T F>
inline T Fn1(T x)
{
    return F(x);
}

template <typename T>
inline T Fn2(T x)
{
    return x;
}

template <typename T>
class C
{
    T x = T::B2;
}
