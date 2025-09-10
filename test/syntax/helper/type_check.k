// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.type_check
    { foo
    , Bar
    , Foo
    , NFoo
    , T1
    , T2
    , T3
    }

using foo=uint5;

template <typename T, T X>
class Bar
{
    const T x = X;
}

template <typename T, T X>
class Foo
{
    Bar<T, X> bar;
}

template <auto N, int<N> X>
class NFoo
{
    int<N> x = X;
    static assert(bitsizeof x == N);
}

template <typename T>
class T1
{
    T x;
}

template <(uint32) -> int32 F>
class T2
{
}

template <foo N>
class T3
{
}

