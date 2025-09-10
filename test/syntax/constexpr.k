// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

const auto X = X;

inline void main()
{
    static T1<X> foo;
}

expected:1

inline uint32 Foo()
{
    return 0;
}
inline void main()
{
    static T1<Foo()> foo;
}

expected:1

inline void main()
{
uint32 x = 10;
    static T1<x> foo;
}

expected:1

const auto x = lutmul(2, 3);

inline void main()
{
    static T1<x> foo;
}

expected:0

inline void main()
{
    static T1<lutmul(2, 3)> foo;
}

expected:0

inline void main()
{
    static T1<mux(1, 2, 3)> foo;
}

expected:0

inline void main()
{
    static T1<2 ? 3 : 5> foo;
}

expected:1

inline void main()
{
    static T1<clog2(0)> foo;
}

expected:1

inline void main()
{
    static T1<bitsizeof int32> foo;
}

expected:0

inline void main()
{
    static T1<bitsizeof int3> foo;
}

expected:0

inline void main()
{
    static T1<bytesizeof int32> foo;
}

expected:0

inline void main()
{
    static T1<bytesizeof int3> foo;
}

expected:1

inline void main()
{
    static T1<bitsizeof(int32) + bitsizeof(uint32)> foo;
}

expected:0

inline void main()
{
    static T1<bitsizeof S1> foo;
}

expected:0

inline void main()
{
    static T1<bitsizeof S1_t> foo;
}

expected:0

inline void main()
{
    static T1<bitsizeof S1_tt> foo;
}

expected:0

inline void main()
{
    static T1<bitsizeof S2> foo;
}

expected:0

inline void main()
{
    static T1<bitsizeof S3> foo;
}

expected:0

inline void main()
{
    static T2<bitsizeof int32> foo;
}

expected:0

inline void main()
{
    static T1<bitsizeof(int<bitsizeof S3 + bitsizeof S1>)> foo;
}

expected:0

inline void main()
{
    static T1<bitsizeof int<bitsizeof(uint32)>> foo;
}

expected:0

inline void main()
{
    static T2<bytesizeof S3> foo;
}

expected:0

inline void main()
{
    static T2<clog2(bytesizeof S3) + bitsizeof S1_tt> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof S1_t == bitsizeof S1_tt> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof S1_t != bitsizeof S1_tt> foo;
}

expected:1

inline void main()
{
    static T3<bitsizeof S3 == (bytesizeof S3) * 8> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof S2 == bitsizeof S1 + bitsizeof bool> foo;
}

expected:0

inline void main()
{
    static T3<(bitsizeof S3 > bitsizeof S1)> foo;
}

expected:0

inline void main()
{
    static T3<(bitsizeof S3 < bitsizeof S1)> foo;
}

expected:1

inline void main()
{
    static T3<(bitsizeof S1 >= bitsizeof S1_t && bitsizeof S1 <= bitsizeof S1_t)> foo;
}

expected:0

inline void main()
{
    static T3<(bitsizeof S3 >= bitsizeof S1_t || bitsizeof S3 <= bitsizeof S1_t)> foo;
}

expected:0

inline void main()
{
    static T3<(S3_size / 8  == S3_size >> 3)> foo;
}

expected:0

inline void main()
{
    static T3<(S3_size / 8  == bytesizeof S3)> foo;
}

expected:0

inline void main()
{
    static T3<(((S3_size + S3_size) * S3_size >> 8) & 7) == (~cast<uint2>(1) & 3)> foo;
}

expected:0

inline void main()
{
    static T1<bitsizeof A1> foo;
}

expected:0

inline void main()
{
    static T1<bytesizeof A1> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof A1 == A1_size> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof A2 == 10 * bitsizeof(uint32) * A1_size / 8> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof bool == 1> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof int<312> == 312> foo;
}

expected:0

const uint312 x = 10;

inline void main()
{
    static T3<bitsizeof x == 312> foo;
}

expected:0

const auto x = 10;

inline void main()
{
    static T3<bitsizeof x == 4> foo;
}

expected:0

const uint13 y = 10;

const auto x = y;

inline void main()
{
    static T3<bitsizeof x == 13> foo;
}

expected:0

const uint13 y = 10;

inline void main()
{
    static T3<bitsizeof(uint13[bitsizeof y]) == 13 * 13> foo;
}

expected:0

inline void main()
{
    uint13 y;
    static T3<bitsizeof(uint13[bitsizeof y]) == 13 * 13> foo;
}

expected:0

inline void main()
{
    uint4 x;
    static T3<bitsizeof x == 4> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof float32 == 32> foo;
}

expected:0

import .cmdargs

inline void main()
{
    static T3<bitsizeof int<FOO_CONST> == FOO_CONST> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof S4<int<4>> == 4 + 4 * 10> foo;
}

expected:0

template <typename T>
using A4 = S4<T>;

inline void main()
{
    static T3<bitsizeof S4<A4<int<5>>> == bitsizeof A4<S4<int5>>> foo1;
    static T3<bitsizeof S4<A4<int<5>>> == 11 * bitsizeof S4<int5>> foo2;
    static T3<bitsizeof S4<A4<int<5>>> == 11 * (5 + 5 * 10)> foo3;
}

expected:0

const auto x = bitsizeof S4<S4<int<5>>>;

inline void main()
{
    static T3<x == 11 * (5 + 5 * 10)> foo3;
}

expected:0

template <typename T>
inline T F4()
{
    return 3;
}

const auto x = F4<4>();

inline void main()
{
    static T3<x == 11 * (5 + 5 * 10)> foo3;
}

expected:1

inline void main()
{
    static T4<7, 3> foo;
}

expected:0

inline void main()
{
    static T4<8, 4> foo;
}

expected:0

inline void main()
{
    static T4<8, 5> foo;
}

expected:1

inline void main()
{
    static T3<clog2(1) == 0> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof 0 == 1> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof 1 == 1> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof 2 == 2> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof 3 == 2> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof 4 == 3> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof 255 == 8> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof 256 == 9> foo;
}

expected:0

const auto x = bitsizeof -1;
inline void main()
{
    static T3<bitsizeof -1 == x> foo;
}

expected:0

const auto x = bitsizeof -10;
inline void main()
{
    static T3<bitsizeof -10 == x> foo;
}

expected:0

inline void main()
{
    uint<bitsizeof 4> x;

    static assert(bitsizeof x == 3);
}

expected:0

inline void main()
{
    uint<40> x;

    static assert(bitsizeof x == 40);
}

expected:0

inline void main()
{
    int<40> x;

    static assert(bitsizeof x == 40);
}

expected:0

inline void main()
{
    static T3<((1>2) ? 0 : 1) == 1> foo;
}

expected:0

inline void main()
{
    int32 x;
    static T1<x == 0 ? 1 : 0> foo;
}

expected:1

inline void main()
{
    int32 x;
    static T1<1 == 1 ? x : 0> foo;
}

expected:1

inline void main()
{
    int32 x;
    static T1<2 == 0 ? 1 : x> foo;
}

expected:1

inline void main()
{
    int32 x;
    static T1<1 == 0 ? x : 1> foo;
}

expected:0

inline void main()
{
    int32 x;
    static T1<2 == 2 ? 1 : x> foo;
}

expected:0

const uint2 n = 1;
const auto x = mux(n, 0, 1, 2, 3);
const auto a = x > 0 ? 10 : 11;
const auto b = 0;
const auto y = mux(x, 0, a, b, 0);

inline void main()
{
    static T3<y == 10> foo;
    static assert(y == 10);
}

expected:0

inline void main()
{
    static T3<mux(4, 0, 1, 2, 3)> foo;
}

expected:1

const auto x = 10;
using foo = int<(x < 1) ? 1 : clog2(x)>;

inline void main()
{
    static T3<bitsizeof foo == 4> bar;
}

expected:0

inline void main()
{
    static T3<bitsizeof 3.14 == 32> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof true == 1> foo;
}

expected:0

inline void main()
{
    static T3<bitsizeof false == 1> foo;
}

expected:0

inline void main()
{
    static T5<uint5, 78, 14> foo;
}

expected:0

inline void main()
{
    static T5<uint<5>, 78, 14> foo;
}

expected:0

const uint<5> x = 78;
inline void main()
{
    static T6<bitsizeof x, 78, x> foo;
}

expected:0

const uint<7> x = 78;
inline void main()
{
    static T6<bitsizeof x, 78, x> foo;
}

expected:0

const uint<7> x = -1;
inline void main()
{
    static T6<bitsizeof x, -1, x> foo;
}

expected:0

inline void main()
{
    static T5<uint7, 78, 14> foo;
}

expected:1

inline void main()
{
    static T5<uint<7>, 78, 14> foo;
}

expected:1

const auto n = 0;
inline void main()
{
    static T4<cast<float32>(n), 32> foo;
}

expected:0

const int3 x = -1;
inline void main()
{
    static T7<bitsizeof(int3), -1, x> foo;
}

expected:0

const int3 x = -3;
inline void main()
{
    static T7<bitsizeof(int3), -3, x> foo;
}

expected:0

const int3 x = -10;
inline void main()
{
    static T7<bitsizeof(int3), -10, x> foo;
}

expected:0

    const auto a = 8;

    class Main
    {
        [[max_threads(a)]]
        [[thread_rate(a + 1)]]
        uint32 Bar(uint32 x)
        {
            return x;
        }
    }

    export Main;

expected:0

    const auto a = 8;

    class Main
    {
        [[max_threads(a)]]
        [[thread_rate(a > 1)]]
        uint32 Bar(uint32 x)
        {
            return x;
        }
    }

expected:1

    inline void main()
    {
        static T3<bitsizeof(E5) == 5> t3;
    }

expected:0

    const E5 e = E5::X;

    inline void main()
    {
        static T3<bitsizeof(e) == 5> t3;
    }

expected:0

    inline void main()
    {
        static T3<bitsizeof(S5) == 5> t3;
    }

expected:0

    static assert(bitsizeof S5 == 5);

expected:0

    inline void main()
    {
        static S5 s5;

        static assert(bitsizeof s5 == 5);
    }

expected:0

    static assert(bitoffsetof(S1, x) == bitoffsetof(S1_tt, x));

expected:0

    static assert(bitoffsetof(S4<uint3>, x) == 0);

expected:0

    static assert(bitoffsetof(S4<uint3>, y) == 3);

expected:0

    static assert(bitoffsetof(S3, y) == bitsizeof(S1_t));

expected:0

    static assert(bitoffsetof(S4<S3>, y) == bitsizeof(S3));

expected:0

    static assert(byteoffsetof(S4<S3>, y) == bytesizeof(S3));

expected:0

    static assert(bitoffsetof(S4<S3>, y) == 8 * bytesizeof(S3));

expected:0

    inline void main()
    {
        static T3<byteoffsetof(S4<S3>, y) == bytesizeof(S3)> t3;
    }

expected:0

    inline void main()
    {
        int<bitoffsetof(S2, b)>[byteoffsetof(S4<S3>, y)] a;

        static assert(bitsizeof a == bitsizeof S1 * (bitsizeof S3) / 8);
    }

expected:0

    inline void main()
    {
        static T3<cast<uint8>(255) == 255> foo;
    }

expected:0

    inline void main()
    {
        static T3<cast<uint8>(-1) == 255> foo;
    }

expected:0

    inline void main()
    {
        static T3<cast<int8>(255) == -1> foo;
    }

expected:0

    inline void main()
    {
        static T3<cast<int8>(127) == 127> foo;
    }

expected:0

    inline void main()
    {
        static T3<cast<int8>(128) == -128> foo;
    }

expected:0

    inline void main()
    {
        static T3<cast<int8>(129) == -127> foo;
    }

expected:0

    const auto N = Undefined;

    inline void main()
    {
        static T1<N> t1;
    }

expected:1

    inline void main()
    {
        uint<E5::Count> x;
    }

expected:0

    inline void main()
    {
        int<E5::Count> x;
    }

expected:0

    import type.stdtype

    inline void main()
    {
        count_t<E5::Count> x;
        uint<bitsizeof x> y;
    }

expected:0

    inline void main()
    {
        static T3<-E5::Count == -1> x;
    }

expected:0

    inline void main()
    {
        static T3<~E5::Count == ~cast<uint5>(1)> x;
    }

expected:0

    inline void main()
    {
        static T3<clog2(E5::Count) == 0> x;
    }

expected:0

    inline void main()
    {
        static T3<mux(E5::Count, 1, 2) == 2> x;
    }

expected:0

    template <auto N>
    using Foo = bool[N];

    inline void main()
    {
        Foo<E5::Count> x;

        const auto y = x;
    }

expected:0

    inline void main()
    {
        uint<1 && 1 && 1> x;
    }

expected:1

    inline void main()
    {
        uint<true - false + true> x;
    }

expected:1

    inline void main()
    {
        uint<2 * bytesizeof 1> x;
    }

expected:1

    inline void main()
    {
        uint<false >= true == true> x;
    }

expected:1

    class Main
    {
        [[max_threads(C_t::x)]]
        void main()
        {
        }
    }

expected:0

    class Main
    {
        [[max_threads(C_t::y)]]
        void main()
        {
        }
    }

expected:0

    class Main
    {
        [[max_threads(C_t::z)]]
        void main()
        {
        }
    }

expected:0

    class Main
    {
        [[max_threads(C_t::q)]]
        void main()
        {
        }
    }

expected:0

    import type.stdtype

    const auto iter = 100;

    inline void test()
    {
        inline void foo (index_t<iter> x, int32 y)
        {
            const auto iter = y;
        };
    }

expected:0

    import type.stdtype

    const auto iter = 100;

    inline void test()
    {
        const auto fn = [iter](index_t<iter> x)
        {
        };
    }

expected:0

    import type.stdtype

    const auto N = 100;

    inline void test()
    {
        const auto fn = [N]()
        {
            index_t<N> x;
        };
    }

expected:1

    const auto a = 2 + 2;

    const auto b = 2 - 1;

    const auto c = true ? b : a;

    static assert(c == 1);

expected:0

    const int8 a = -1;

    const int8 b = -2;

    const auto c = false ? a : b;

    static assert(c == -2);

expected:0

    const int8 a = -1;

    const int8 b = -2;

    static assert(a * b == 2);

expected:0

    const int8 a = -4;

    const int8 b = 5;

    static assert(a * b == -20);

expected:0

    // x should be truncated to 0
    const uint6 x = 64;

    const auto b = 4 / x;

expected:1

    inline void main()
    {
        int32 x;

        if (clog2(x) == 2)
        {}
    }

expected:1

    inline void Run(uint32 i)
    {
        const bool b = false;

        // b is constexpr (false)
        // therefore b && anything is constexpr
        static assert(!(b && (i == 4)));
    }

expected:0

    inline void Run(uint32 i)
    {
        const bool b = false;

        // b is constexpr (false)
        // therefore b && anything is constexpr
        static assert(!((i == 4) && b));
    }

expected:0

    inline void Run(uint32 i)
    {
        const bool b = true;

        // b is constexpr (true)
        // therefore b || anything is constexpr
        static assert(b || (i == 4));
    }

expected:0

    inline void Run(uint32 i)
    {
        const bool b = true;

        // b is constexpr (true)
        // therefore b || anything is constexpr
        static assert((i == 4) || b);
    }

expected:0

    inline void Run(uint32 i)
    {
        const uint32 x = 0;

        // 0 & (anything <= bit width) = constexpr 0
        static assert((i & x) == 0);
    }

expected:0

    inline void Run(uint16 i)
    {
        const uint32 x = 0;

        // 0 & (anything <= bit width) = constexpr 0
        static assert((i & x) == 0);
    }

expected:0

    inline void Run(uint64 i)
    {
        const uint32 x = 0;

        // 0 & (anything > bit width) is not constexpr
        static assert((i & x) == 0);
    }

expected:1

    inline void Run(uint32 i)
    {
        const uint32 x = 1;

        // x is not 0, this is not a constexpr
        static assert((i & x) == 0);
    }

expected:1

    inline void Run(uint32 i)
    {
        const uint32 x = 0xffffffff;

        // 1 | (anything <= bit width) = constexpr 1
        static assert((i | x) == 0xffffffff);
    }

expected:0

    inline void Run(uint16 i)
    {
        const uint32 x = 0xffffffff;

        // 1 | (anything <= bit width) = constexpr 1
        static assert((i | x) == x);
    }

expected:0

    inline void Run(uint32 src)
    {
        uint28 i = src;

        const uint32 x = 0x2fffffff;

        // x is all 1s for bit width of i, this is a constexpr x
        static assert((i | x) == x);
    }

expected:0

    inline void Run(uint32 src)
    {
        uint29 i = src;

        const uint32 x = 0x2fffffff;

        // x is not all 1s for bit width of i, this is not a constexpr x
        static assert((i | x) == x);
    }

expected:1

    inline void Run(uint64 i)
    {
        const uint32 x = 0xffffffff;

        // 1 | (anything > bit width) is not constexpr
        static assert((i | x) == x);
    }

expected:1

    inline void Run(uint32 i)
    {
        const uint32 x = 0xefffffff;

        // x is not all 1s, this is not a constexpr
        static assert((i | x) == x);
    }

expected:1

    inline void Run(uint32 i)
    {
        const uint32 x = 0;

        // x is not all 1s, this is not a constexpr
        static assert((i | x) == x);
    }

expected:1

    inline void main()
    {
        static if (bitsizeof void){}
    }

expected:1

    inline void main()
    {
        static if (bytesizeof void){}
    }

expected:1

    uint32 y;

    inline void main()
    {
        static if (bitoffsetof(uint32, y)){}
    }

expected:1

    uint32 y;

    inline void main()
    {
        static if (byteoffsetof(bool, y)){}
    }

expected:1

    struct Foo
    {
        uint32 x;
    }

    uint32 y;

    inline void main()
    {
        static if (bitoffsetof(Foo, y)){}
    }

expected:1

    struct Foo
    {
        uint32 x;
    }

    uint32 y;

    inline void main()
    {
        static if (byteoffsetof(Foo, y)){}
    }

expected:1

    const int<FOO> x = 0;

expected:1

    const uint<FOO> x = 0;

expected:1

    const uint<FOO> x = 0;

const auto FOO = 32;

expected:0

    template <uint32 N, uint32 M>
    inline void Foo()
    {
        const uint32 _N = N;
        const auto _M = M << 1;
        const uint32 O = _M | _N;
        const uint32 _O = O << _M;

        uint<O> x;
        uint<_O> y;
    }

    inline void main()
    {
        Foo<10, 2>();
    }

expected:0

    class Foo
    {
    private:
        uint<Width>[Size] a;
    public:
        const auto Size = 32;
        const auto Width = 8;
    }

expected:0

    class Foo
    {
    private:
        uint<Width>[Size] a;
    public:
        const auto Size = 32;
        auto Width = 8;
    }

expected:1

    class Foo
    {
    private:
        uint<Width>[Size] a;
    public:
        auto Size = 32;
        const auto Width = 8;
    }

expected:1

    class Foo
    {
    private:
        uint<Width>[Size] a;
    public:
        auto Size = 32;
        auto Width = 8;
    }

expected:1

    const auto x = 10;
    const auto y = 20;

    const int<x-y> a = 0;

expected:1

    const auto x = 10;
    const auto y = 20;

    const uint<x-y> a = 9;

expected:1

    inline void main()
    {
        uint32 x = 10;

        uint32 y = 1 << x;
    }

expected:1

    inline void main()
    {
        const uint32 x = 10;

        uint32 y = 1 << x;
    }

expected:0

    const auto x = clog2(1024);

    static assert(x == 10);

expected:0

    const auto x = clog2(513);

    static assert(x == 10);

expected:0

    const int8 a = 127;
    const auto x = clog2(a);

    static assert(x == 7);

expected:0

    const uint7 a = 127;
    const auto x = clog2(a);

    static assert(x == 7);

expected:0

    const float32 x = 2.5;
    const float32 y = cast<float32>(0x40200000);

    static assert(cast<uint32>(x) == cast<uint32>(y));

expected:0

    enum E : uint32
    {
        One
    }

    inline void main()
    {
        auto x = ~E::One;
        E y = x;
    }

expected:1

    static assert(4 == bitsizeof(mux(0, 10, undefined)));

expected:0

    static assert(4 == bitsizeof(mux(1, 0, undefined)));

expected:1
