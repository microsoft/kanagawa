// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

enum E : uint2
{
    A,
    B,
    C
}

struct S
{
    E e;
    uint6 x;
}

template <typename T, auto N>
union U
{
    T x;
    int<N> y;
}

using U8 = U<uint8, 8>;

union Foo
{
    uint8 x;
    uint4[2] a;
    S s;
    U8 u;
}

struct Bar
{
    S s;
    Foo f;
    U8[2] a;
}

inline auto Id(Bar x)
{
    auto y = x;
    return y;
}

using BarT = decltype(Id({}));

class Test
{
private:
    (BarT, Foo, E)->BarT callback;

public:
    BarT Run(BarT x, Foo y, E z)
    {
        auto result = callback(x, y, z);
        return Id(result);
    }
}

export Test;