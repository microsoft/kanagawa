// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.literal_operand
    { AndLiteral
    , MulLiteral
    , DivLiteral
    , ModLiteral
    , BinaryOpsLiteral
    , Num
    }

enum Num : uint4
{
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Siz,
    Seven,
    Eight,
    Nine
}


const uint5 u = 0;
const int5 s = 0;

template <auto x, auto N>
inline void AndLiteral()
{
    const auto iu = u & N;
    const auto is = s & N;

    using u_t = decltype(iu);
    using s_t = decltype(is);

    const auto sx = x - 16;
    const u_t a = x & N;
    const s_t b = sx & N;

    static assert(a == (x & N));
    static assert(b == (sx & N));
}

template <auto x, auto N>
inline void MulLiteral()
{
    const auto iu = u * N;
    const auto is = s * N;

    using u_t = decltype(iu);
    using s_t = decltype(is);

    const auto sx = x - 16;
    const u_t a = x * N;
    const s_t b = sx * N;

    static assert(a == (x * N));
    static assert(b == (sx * N));
}

template <auto x, auto N>
inline void DivLiteral()
{
    static assert(N > 0);

    const auto iu = u / N;
    const auto is = s / N;

    using u_t = decltype(iu);
    using s_t = decltype(is);

    const auto sx = x - 16;
    const u_t a = x / N;
    const s_t b = sx / N;

    static assert(a == (x / N));
    static assert(b == (sx / N));
}

template <auto x, auto N>
inline void ModLiteral()
{
    static assert(N > 0);

    const auto iu = u % N;
    const auto is = s % N;

    using u_t = decltype(iu);
    using s_t = decltype(is);

    const auto sx = x - 16;
    const u_t a = x % N;
    const s_t b = sx % N;

    static assert(a == (x % N));
    static assert(b == (sx % N));
}

template <auto N>
inline void BinaryOpsLiteral()
{
    static for (const auto x : 32)
    {
        AndLiteral<x, N>();
        MulLiteral<x, N>();

        static if (N > 0 && ((N -1) & N) == 0)
        {
            DivLiteral<x, N>();
            ModLiteral<x, N>();
        }
    }
}
