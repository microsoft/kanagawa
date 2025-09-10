// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.constexpr
    { T1
    , T2
    , T3
    , T4
    , T5
    , T6
    , T7
    , S1
    , S2
    , S1_t
    , S1_tt
    , S3
    , S4
    , A1
    , A2
    , S3_size
    , A1_size
    , E5
    , S5
    , C
    , C_t
    }

template <uint8 N>
class T1
{
    using t=int<N>;
    t x;
}

template <uint8 N>
class T2
{
    T1<N> t1;
}

template <bool B>
class T3
{
    static assert(B);
}

template <auto N, uint32 M>
class T4
{
    static assert(bitsizeof N == M);
}

template <typename T, T X, auto Y>
class T5
{
    static assert(X == Y);
}

template <auto N, uint<N> X, auto Y>
class T6
{
    static assert(X == Y);
}

template <auto N, int<N> X, auto Y>
class T7
{
    static assert(X == Y);
}

struct S1
{
    int3 x;
}

struct S2
{
    S1 x;
    bool b;
}

using S1_t=S1;

using S1_tt=S1_t;

struct S3
{
    S1_t x;
    S1_tt y;
    S2 z;
    int<16 - bitsizeof(S1_t) - bitsizeof(S1_tt) - bitsizeof(S2)> q;
}

template <typename T>
struct S4
{
    T     x;
    T[10] y;
}

using A1=S3[bytesizeof(S3)];

using A2=uint32[10][bytesizeof(A1)];

const auto S3_size = bitsizeof S3;

const auto A1_size = bitsizeof A1;

enum E5 : uint5
{
    X,
    Count
}

struct S5
{
    E5 e
}

template <auto N>
class C
{
    public:
        const uint32 y = 16;
        const uint32 x = 2 * y;
        const auto z = N;
        const auto q = x * z;
}

using C_t = C<2>;
