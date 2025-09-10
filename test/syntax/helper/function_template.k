// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.function_template
    { Bar
    , Bar1
    }

template <typename T, T N>
inline T Bar(T x)
{
    T y;
    static assert(bitsizeof x == N);
    static assert(bitsizeof y == N);
    static assert(bitsizeof T == N);
    return x;
}

template <typename T>
inline void Bar1(uint32 x)
{
    T[bitsizeof T] a;
    
    a[3] = 0;
}

