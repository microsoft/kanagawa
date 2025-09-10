// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.nested_templates
    { Foo
    , Id
    , Bar
    }

template <typename T1>
class Foo
{
public:
    template <typename T2>
    struct Bar
    {
        T1 x;
        T2 y;
    }

    template <T1 N>
    struct S
    {
        T1[N] x;
    }

    Bar<uint7> b;

    static assert(bitsizeof(b) == bitsizeof(T1) + 7);
    static assert(bitsizeof(Bar<uint7>) == bitsizeof(T1) + 7);
}


template <typename T>
using Id = T;

template <typename T>
class Bar
{
public:
    template <Id<T> N>
    struct S
    {
        uint<N> x;
    }

    inline void Do()
    {
    }

    template <typename U>
    U Go(U x)
    {
        return x;
    }

    T x;
}

