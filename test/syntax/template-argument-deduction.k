// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline uint32 Run()
    {
        return Fn_T<uint32>(10);
    }

expected:0

    inline uint4 Run()
    {
        return Fn_T(10);
    }

expected:0

    template <auto N>
    inline uint<clog2(N)> Go()
    {
        const auto x = N;
        return Fn_T(x);
    }

    inline uint4 Run()
    {
        return Go<10>();
    }

expected:0

    inline Enum Run()
    {
        return Fn_T(Enum::Zero);
    }

expected:0

    inline Enum Run()
    {
        Enum x = Enum::Zero;
        return Fn_T(x);
    }

expected:0

    inline Struct Run()
    {
        Struct x;
        return Fn_T(x);
    }

expected:0

    inline Union Run()
    {
        Union x;
        return Fn_T(x);
    }

expected:0

    inline bool Run()
    {
        return Fn_T(Enum::Zero);
    }

expected:1

    inline void main()
    {
        return Fn_T(true);
    }

expected:1

    inline float32 Run()
    {
        return Fn_T(cast<float32>(cast<uint32>(10)));
    }

expected:0

    inline uint4 Run()
    {
        return Fn_T(cast<float32>(cast<uint32>(10)));
    }

expected:1

    inline int3 Run()
    {
        return Fn_T(-3);
    }

expected:0

    inline bool Run()
    {
        return Fn_T(false);
    }

expected:0

    inline bool Run()
    {
        auto x = false;

        return Fn_T(x);
    }

expected:0

    inline bool Run()
    {
        struct Foo
        {
            bool x;
        }

        static Foo foo;

        return Fn_T(foo.x);
    }

expected:0

    inline uint32 Run()
    {
        auto x = false;

        return Fn_T(x);
    }

expected:1

    template <typename T>
    inline T Do()
    {
        T x;

        return Fn_T(x);
    }

    inline uint32 Run()
    {
        return Do<uint32>();
    }

expected:0

    inline uint4 Run()
    {
        return Fn_T_T(10, 10);
    }

expected:0

    inline Enum Run()
    {
        return Fn_T_T(Enum::Zero, Enum::Zero);
    }

expected:0

    inline Struct Run()
    {
        Struct x;
        Struct y;

        return Fn_T_T(x, y);
    }

expected:0

    inline Union Run()
    {
        Union x;
        Union y;

        return Fn_T_T(x, y);
    }

expected:0

    inline Enum Run()
    {
        return Fn_T_T(Enum::Zero, 10);
    }

expected:1

    inline uint5 Run()
    {
        return Fn_T_T(10, 20);
    }

expected:0

    inline uint5 Run()
    {
        return Fn_T_T(10, 20.0);
    }

expected:1

    inline float32 Run()
    {
        return Fn_T_T(cast<float32>(cast<uint32>(10)), 20.0);
    }

expected:0

    inline bool Run()
    {
        return Fn_T_T(false, 10);
    }

expected:1

    inline bool Run()
    {
        return Fn_T_T(false, true);
    }

expected:0

    inline int5 Run()
    {
        return Fn_T_T(-10, 10);
    }

expected:0

    inline int5 Run()
    {
        return Fn_T_T(Enum::Zero, 10);
    }

expected:0

    inline uint4[10] Run()
    {
        uint4[10] x;

        return Fn_T(x);
    }

expected:0

    template <auto N>
    inline uint4[N] Go()
    {
        uint4[N] x;
        return Fn_T(x);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:0

    template <auto N>
    inline uint4[N] Go()
    {
        uint4[N] x;
        return Fn_T(x);
    }

    inline uint4[11] Run()
    {
        return Go<10>();
    }

expected:1

    inline void main()
    {
        uint7 x;
        int5 y;

        const auto r = Fn_T_U(x, y);

        static assert(bitsizeof r == 9);
    }

expected:0

    inline void main()
    {
        uint7 x;
        int5 y;

        const auto r = Fn_T_U<uint2>(x, y);

        static assert(bitsizeof r == 6);
    }

expected:0

    inline void main()
    {
        uint7 x;
        int5 y;

        const auto r = Fn_T_U<uint2, int5>(x, y);

        static assert(bitsizeof r == 6);
    }

expected:0

    inline void main()
    {
        uint7 x;
        int5 y;

        const auto r = Fn_T_U<uint2, int2>(x, y);

        static assert(bitsizeof r == 4);
    }

expected:0

    template <auto N>
    inline uint4[N] Go()
    {
        uint4[N] x;
        return Fn_T_T(x, x);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:0

    template <auto N>
    inline uint4[N] Go()
    {
        uint4[N] x;
        uint3[N] y;
        return Fn_T_T(x, y);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:1

    template <auto N>
    inline uint4[N] Go()
    {
        uint4[N] x;
        uint4[N+1] y;
        return Fn_T_T(x, y);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:1

    template <auto N>
    inline uint4[N] Go()
    {
        const uint4[N] x = {0};
        uint4[N] y;
        return Fn_T_T(x, y);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:0

    template <auto N>
    inline uint4[N] Go()
    {
        const uint4[N] x = {0};
        uint4[N] y;
        return Fn_T_T(y, x);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:0

    import type.stdtype

    inline void main()
    {
        index_t<10> x;
        uint<2> y;

        auto r = Fn_T_T(x, y);
        static assert(bitsizeof x > bitsizeof y ? bitsizeof r == bitsizeof x : bitsizeof r == bitsizeof y);
    }

expected:0

    import type.stdtype

    inline void main()
    {
        uint<2> x;
        index_t<10> y;

        auto r = Fn_T_T(x, y);
        static assert(bitsizeof x > bitsizeof y ? bitsizeof r == bitsizeof x : bitsizeof r == bitsizeof y);
    }

expected:0

    inline void main()
    {
        auto x = Fn_T(10);
        auto y = 2;
        auto r = Fn_T_T(x, y);
        static assert(bitsizeof x > bitsizeof y ? bitsizeof r == bitsizeof x : bitsizeof r == bitsizeof y);
    }

expected:0

    inline void main()
    {
        auto x = 2;
        auto y = Fn_T(10);
        auto r = Fn_T_T(x, y);
        static assert(bitsizeof x > bitsizeof y ? bitsizeof r == bitsizeof x : bitsizeof r == bitsizeof y);
    }

expected:0

    inline void main()
    {
        struct Foo { bool x; };
        Foo x;
        auto r = Fn_T_T(x, {});
        static assert(bitsizeof r == bitsizeof x);
    }

expected:0

    inline void main()
    {
        struct Foo { bool x; };
        Foo x;
        auto r = Fn_T_T({}, x);
        static assert(bitsizeof r == bitsizeof x);
    }

expected:0

    inline uint4[4] Run()
    {
        uint4[10][4] x;

        return Fn_T_10(x);
    }

expected:0

    using t = uint4[4];

    inline uint4[4] Run()
    {
        t[10] x;

        return Fn_T_10(x);
    }

expected:0

    import data.array

    inline uint4[4] Run()
    {
        array<array<uint4, 4>, 10> x;

        return Fn_T_10(x);
    }

expected:0

    inline uint4[4] Run()
    {
        uint4[9][4] x;

        return Fn_T_10(x);
    }

expected:1

    inline void main()
    {
        Fn_T_10(10);
    }

expected:1

    inline uint4[11] Run()
    {
        uint4[10] x;

        return Fn_T_N(x);
    }

expected:0

    inline uint4[11][3] Run()
    {
        uint4[10][3] x;

        return Fn_T_N(x);
    }

expected:0

    inline Enum[11] Run()
    {
        Enum[10] es;

        return Fn_T_T_N(Enum::Zero, es);
    }

expected:0

    inline Enum[10] Run()
    {
        Enum[10] es;

        return Fn_T_T_N(Enum::Zero, es);
    }

expected:1

    inline Enum[5][10] Run()
    {
        Enum[10] es;
        Enum[4][10] fs;

        return Fn_T_T_N(es, fs);
    }

expected:0

    inline Struct[5][10] Run()
    {
        Struct[10] es;
        Struct[4][10] fs;

        return Fn_T_T_N(es, fs);
    }

expected:0

    inline Union[5][10] Run()
    {
        Union[10] es;
        Union[4][10] fs;

        return Fn_T_T_N(es, fs);
    }

expected:0

    inline Union[5][10] Run()
    {
        Struct[10] es;
        Union[4][10] fs;

        return Fn_T_T_N(es, fs);
    }

expected:1

    inline Enum[5][10] Run()
    {
        Enum[10] es;
        Enum[4][10] fs;

        return Fn_T_T_N<Enum[4]>(es, fs);
    }

expected:1

    inline void main()
    {
        Enum[10] es;
        Enum[4][10] fs;

        Fn_T_T_N<Enum[10]>(es, fs);
    }

expected:0

    inline void main()
    {
        bool[10] fs;

        Fn_T_T_N<bool>(false, fs);
    }

expected:0

    inline void main()
    {
        bool[10] fs;

        Fn_T_T_N<bool>(10, fs);
    }

expected:1

    inline bool Run()
    {
        return Fn_R_T(10);
    }

expected:0

    inline bool Run()
    {
        return Fn_R_T<bool>(10);
    }

expected:0

    inline
    uint32 Go((uint32) -> uint32 Do)
    {
        static decltype(Do) fn;
        uint32 x;

        fn = Do;

        return Fn_T_F(fn, x);
    }

    inline uint32 Run()
    {
        bool b;

        return Go([b](uint32 x) -> uint32 {return x;});
    }

expected:0

    inline uint4 Run()
    {
        uint4[8] a;

        return Fn_T_10(
                    Fn_T_T_N(
                        Fn_T(10),
                        Fn_T_N(a)));
    }

expected:0

    template <typename T, typename N>
    inline void Foo(T[N] x)
    {
    }

    inline void main()
    {
        uint3[10] x;

        Foo(x);
    }

expected:1

    template <auto N>
    inline void Foo(bool[N] x, uint32[N] y)
    {
    }

    inline void main()
    {
        bool[3] x;
        uint32[3] y;

        Foo(x, y);
    }

expected:0

    template <auto N>
    inline void Foo(bool[N] x, uint32[N] y)
    {
    }

    inline void main()
    {
        bool[3] x;
        uint32[2] y;

        Foo(x, y);
    }

expected:1

    inline void main()
    {
        Fn_T_T(1, -1);
    }

expected:0

    inline void main()
    {
        uint3[10] x;
        uint3[10] y;

        Fn_T_N_T_N(x, y);
    }

expected:0

    import data.array

    inline void main()
    {
        array<uint3, 10> x;
        uint3[10] y;

        Fn_T_N_T_N(x, y);
    }

expected:0

    inline void main()
    {
        uint3[10] x;

        Fn_T_N_T_N(x, {});
    }

expected:0

    inline void main()
    {
        uint3[10] x;

        Fn_T_N_T_N({}, x);
    }

expected:0

    inline void main()
    {
        uint3[10] x;
        uint2[10] y;

        Fn_T_N_T_N(x, y);
    }

expected:1

    inline uint5 Run()
    {
        return Fn_uint_N(10);
    }

expected:0

    inline uint5 Run()
    {
        return Fn_uint_N(-10);
    }

expected:1

    inline int5 Run()
    {
        return Fn_int_N(7);
    }

expected:0

    inline uint5[3] Run()
    {
        uint4[2] x;

        return Fn_uint_N_M(x);
    }

expected:0

    inline uint32 Run()
    {
        static Class o;
        return o.Fn_T(10);
    }

expected:0

    inline
    uint32 Go((uint32) -> uint32 Do)
    {
        static Class o;
        static decltype(Do) fn;
        uint32 x;

        fn = Do;

        return o.Fn_T_F(fn, x);
    }

    inline uint32 Run()
    {
        bool b;

        return Go([b](uint32 x) -> uint32 {return x;});
    }

expected:0

    inline Enum Run()
    {
        static Class o;
        return o.Fn_T(Enum::Zero);
    }

expected:0

    inline Enum Run()
    {
        static Class o;
        Enum x = Enum::Zero;
        return o.Fn_T(x);
    }

expected:0

    inline Struct Run()
    {
        static Class o;
        Struct x;
        return o.Fn_T(x);
    }

expected:0

    inline Union Run()
    {
        static Class o;
        Union x;
        return o.Fn_T(x);
    }

expected:0

    inline bool Run()
    {
        static Class o;
        return o.Fn_T(Enum::Zero);
    }

expected:1

    inline void main()
    {
        static Class o;
        return o.Fn_T(true);
    }

expected:1

    inline float32 Run()
    {
        static Class o;
        return o.Fn_T(cast<float32>(cast<uint32>(10)));
    }

expected:0

    inline uint4 Run()
    {
        static Class o;
        return o.Fn_T(cast<float32>(cast<uint32>(10)));
    }

expected:1

    inline int3 Run()
    {
        static Class o;
        return o.Fn_T(-3);
    }

expected:0

    inline bool Run()
    {
        static Class o;
        return o.Fn_T(false);
    }

expected:0

    inline bool Run()
    {
        static Class o;
        auto x = false;

        return o.Fn_T(x);
    }

expected:0

    inline bool Run()
    {
        static Class o;
        struct Foo
        {
            bool x;
        }

        static Foo foo;

        return o.Fn_T(foo.x);
    }

expected:0

    inline uint32 Run()
    {
        static Class o;
        auto x = false;

        return o.Fn_T(x);
    }

expected:1

    template <typename T>
    inline T Do()
    {
        static Class o;
        T x;

        return o.Fn_T(x);
    }

    inline uint32 Run()
    {
        return Do<uint32>();
    }

expected:0

    inline uint4 Run()
    {
        static Class o;
        return o.Fn_T_T(10, 10);
    }

expected:0

    inline Enum Run()
    {
        static Class o;
        return o.Fn_T_T(Enum::Zero, Enum::Zero);
    }

expected:0

    inline Struct Run()
    {
        Struct y;

        static Class o;
        Struct x;
        return o.Fn_T_T(x, y);
    }

expected:0

    inline Union Run()
    {
        static Class o;
        Union x;
        Union y;

        return o.Fn_T_T(x, y);
    }

expected:0

    inline Enum Run()
    {
        static Class o;
        return o.Fn_T_T(Enum::Zero, 10);
    }

expected:1

    inline uint5 Run()
    {
        static Class o;
        return o.Fn_T_T(10, 20);
    }

expected:0

    inline uint5 Run()
    {
        static Class o;
        return o.Fn_T_T(10, 20.0);
    }

expected:1

    inline float32 Run()
    {
        static Class o;
        return o.Fn_T_T(cast<float32>(cast<uint32>(10)), 20.0);
    }

expected:0

    inline bool Run()
    {
        static Class o;
        return o.Fn_T_T(false, 10);
    }

expected:1

    inline bool Run()
    {
        static Class o;
        return o.Fn_T_T(false, true);
    }

expected:0

    inline int5 Run()
    {
        static Class o;
        return o.Fn_T_T(-10, 10);
    }

expected:0

    inline int5 Run()
    {
        static Class o;
        return o.Fn_T_T(Enum::Zero, 10);
    }

expected:0

    inline uint4[10] Run()
    {
        static Class o;
        uint4[10] x;

        return o.Fn_T(x);
    }

expected:0

    template <auto N>
    inline uint4[N] Go()
    {
        static Class o;
        uint4[N] x;
        return o.Fn_T(x);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:0

    template <auto N>
    inline uint4[N] Go()
    {
        static Class o;
        uint4[N] x;
        return o.Fn_T(x);
    }

    inline uint4[11] Run()
    {
        return Go<10>();
    }

expected:1

    template <auto N>
    inline uint4[N] Go()
    {
        static Class o;
        uint4[N] x;
        return o.Fn_T_T(x, x);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:0

    template <auto N>
    inline uint4[N] Go()
    {
        static Class o;
        uint4[N] x;
        uint3[N] y;
        return o.Fn_T_T(x, y);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:1

    template <auto N>
    inline uint4[N] Go()
    {
        static Class o;
        uint4[N] x;
        uint4[N+1] y;
        return o.Fn_T_T(x, y);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:1

    template <auto N>
    inline uint4[N] Go()
    {
        static Class o;
        const uint4[N] x = {0};
        uint4[N] y;
        return o.Fn_T_T(x, y);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:0

    template <auto N>
    inline uint4[N] Go()
    {
        static Class o;
        const uint4[N] x = {0};
        uint4[N] y;
        return o.Fn_T_T(y, x);
    }

    inline uint4[10] Run()
    {
        return Go<10>();
    }

expected:0

    inline uint4[4] Run()
    {
        static Class o;
        uint4[10][4] x;

        return o.Fn_T_10(x);
    }

expected:0

    import data.array

    inline uint4[4] Run()
    {
        static Class o;
        array<array<uint4, 4>, 10> x;

        return o.Fn_T_10(x);
    }

expected:0

    inline uint4[4] Run()
    {
        static Class o;
        uint4[9][4] x;

        return o.Fn_T_10(x);
    }

expected:1

    inline void main()
    {
        static Class o;
        o.Fn_T_10(10);
    }

expected:1

    inline uint4[11] Run()
    {
        static Class o;
        uint4[10] x;

        return o.Fn_T_N(x);
    }

expected:0

    inline uint4[11][3] Run()
    {
        static Class o;
        uint4[10][3] x;

        return o.Fn_T_N(x);
    }

expected:0

    inline Enum[11] Run()
    {
        static Class o;
        Enum[10] es;

        return o.Fn_T_T_N(Enum::Zero, es);
    }

expected:0

    inline Enum[10] Run()
    {
        static Class o;
        Enum[10] es;

        return o.Fn_T_T_N(Enum::Zero, es);
    }

expected:1

    inline Enum[5][10] Run()
    {
        static Class o;
        Enum[10] es;
        Enum[4][10] fs;

        return o.Fn_T_T_N(es, fs);
    }

expected:0

    inline Struct[5][10] Run()
    {
        static Class o;
        Struct[10] es;
        Struct[4][10] fs;

        return o.Fn_T_T_N(es, fs);
    }

expected:0

    inline Union[5][10] Run()
    {
        static Class o;
        Union[10] es;
        Union[4][10] fs;

        return o.Fn_T_T_N(es, fs);
    }

expected:0

    inline Union[5][10] Run()
    {
        static Class o;
        Struct[10] es;
        Union[4][10] fs;

        return o.Fn_T_T_N(es, fs);
    }

expected:1

    inline Enum[5][10] Run()
    {
        static Class o;
        Enum[10] es;
        Enum[4][10] fs;

        return o.Fn_T_T_N<Enum[4]>(es, fs);
    }

expected:1

    inline void main()
    {
        static Class o;
        Enum[10] es;
        Enum[4][10] fs;

        o.Fn_T_T_N<Enum[10]>(es, fs);
    }

expected:0

    inline void main()
    {
        static Class o;
        bool[10] fs;

        o.Fn_T_T_N<bool>(false, fs);
    }

expected:0

    inline void main()
    {
        static Class o;
        bool[10] fs;

        o.Fn_T_T_N<bool>(10, fs);
    }

expected:1

    inline bool Run()
    {
        static Class o;
        return o.Fn_R_T(10);
    }

expected:0

    inline bool Run()
    {
        static Class o;
        return o.Fn_R_T<bool>(10);
    }

expected:0

    inline
    uint32 Go((uint32) -> uint32 Do)
    {
        static Class o;
        static decltype(Do) fn;
        uint32 x;

        fn = Do;

        return o.Fn_T_F(fn, x);
    }

    inline uint32 Run()
    {
        bool b;

        return Go([b](uint32 x) -> uint32 {return x;});
    }

expected:0

    inline uint4 Run()
    {
        static Class o;
        uint4[8] a;

        return o.Fn_T_10(
                    o.Fn_T_T_N(
                        o.Fn_T(10),
                        o.Fn_T_N(a)));
    }

expected:0

    inline void main()
    {
        static Class o;
        o.Fn_T_T(1, -1);
    }

expected:0

    inline void main()
    {
        static Class o;
        uint3[10] x;
        uint2[10] y;

        o.Fn_T_N_T_N(x, y);
    }

expected:1

    inline uint5 Run()
    {
        static Class o;
        return o.Fn_uint_N(10);
    }

expected:0

    inline uint5 Run()
    {
        static Class o;
        return o.Fn_uint_N(-10);
    }

expected:1

    inline int5 Run()
    {
        static Class o;
        return o.Fn_int_N(7);
    }

expected:0

    inline uint5 Run()
    {
        static Template_T<bool> o;
        return o.Fn_uint_N(10);
    }

expected:0

    inline uint5 Run()
    {
        static Template_T<bool> o;
        return o.Fn_uint_N(-10);
    }

expected:1

    inline int5 Run()
    {
        static Template_T<bool> o;
        return o.Fn_int_N(7);
    }

expected:0

    inline uint5[3] Run()
    {
        static Class o;
        uint4[2] x;

        return o.Fn_uint_N_M(x);
    }

expected:0

    inline uint5[3] Run()
    {
        static Template_N<4> o;
        uint4[2] x;

        return o.Fn_uint_N_M(x);
    }

expected:0

    inline uint5[3] Run()
    {
        static Template_N<5> o;
        uint4[2] x;

        return o.Fn_uint_N_M(x);
    }

expected:1

    inline uint5[3] Run()
    {
        static Template_N<2> o;
        uint4[2] x;

        return o.Fn_uint_M_N(x);
    }

expected:0

    inline uint4[11] Run()
    {
        static Template_T<uint4> o;
        uint4[10] x;

        return o.Fn_T_N(x);
    }

expected:0

    inline uint4[11][3] Run()
    {
        static Template_T<uint4[10]> o;
        uint4[10][3] x;

        return Fn_T_N(x);
    }

expected:0

    inline uint4[11] Run()
    {
        static Template_N<10> o;
        uint4[10] x;

        return o.Fn_T_N(x);
    }

expected:0

    inline uint4[11][3] Run()
    {
        static Template_N<10> o;
        uint4[10][3] x;

        return Fn_T_N(x);
    }

expected:0

    inline uint4 Run()
    {
        return Fn_Fn1([](uint32 x) -> uint4 {return 0;});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_Fn1([](uint32 x) {return 10;});
    }

expected:0

    inline uint3 Foo(int5 x)
    {
        return 0;
    }

    inline uint3 Run()
    {
        return Fn_Fn1(Foo);
    }

expected:0

    inline auto Foo(int5 x)
    {
        return 10;
    }

    inline uint3 Run()
    {
        return Fn_Fn1(Foo);
    }

expected:0

    template <typename T, typename R>
    inline R Foo(T x)
    {
        return 0;
    }

    inline uint3 Run()
    {
        return Fn_Fn1(Foo<uint5, uint3>);
    }

expected:0

    inline uint4 Run()
    {
        return Fn_Fn3([](uint32 x, int3 y, uint1 z) -> uint4 {return 0;});
    }

expected:0

    inline uint3 Foo(int5 x, uint32 y, uint64 z)
    {
        return 0;
    }

    inline uint3 Run()
    {
        return Fn_Fn3(Foo);
    }

expected:0

    inline uint4 Run()
    {
        return Fn_FnA1<6>([](uint32[6] x) -> uint4 {return 0;});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_FnA1([](uint32[6] x) -> uint4 {return 0;});
    }

expected:0

    inline uint4[7] Run()
    {
        return Fn_Fn2A<7>([](uint32 x) -> uint4 {return x;});
    }

expected:0

    inline uint4 Foo(uint4 i)
    {
        return i;
    }

    inline uint4[7] Run()
    {
        return Fn_Fn2A<7>(Foo);
    }

expected:0

    inline uint4 Run()
    {
        return Fn_T_Fn1(10, [](uint32 x) -> uint4 {return 0;});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_U_Fn1(10, [](uint32 x) -> uint4 {return 0;});
    }

expected:0

    inline void main()
    {
        return Fn_Fn([](){});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_T_Fn1(10, [](uint3 x) -> uint4 {return 0;});
    }

expected:1

    inline uint4 Run()
    {
        return Fn_Fn1_Fn1(
            [](uint32 x) -> uint4 {return 0;},
            [](uint32 x) -> uint4 {return 0;});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_Fn1_Fn1(
            [](uint32 x) -> uint4 {return 0;},
            [](uint2 x) -> uint4 {return 0;});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_Fn1_Fn1(
            [](uint32 x) -> uint4 {return 0;},
            [](uint32 x) -> uint5 {return 0;});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_Fn1_Fn1(
            [](uint32 x) -> float32 {return 0;},
            [](uint32 x) -> uint5 {return 0;});
    }

expected:1

    inline uint4 Run()
    {
        return Fn_Fn1_Fn2(
            [](uint32 x) -> uint4 {return 0;},
            [](uint32 x) -> uint4 {return 0;});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_Fn1_Fn2(
            [](uint32 x) -> uint4 {return 0;},
            [](uint2 x) -> uint4 {return 0;});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_Fn1_Fn2(
            [](uint32 x) -> uint4 {return 0;},
            [](uint32 x) -> uint5 {return 0;});
    }

expected:0

    inline uint4 Run()
    {
        return Fn_Fn1_Fn2(
            [](uint32 x) -> float32 {return 0;},
            [](uint32 x) -> uint5 {return 0;});
    }

expected:1

    inline uint4 Run()
    {
        static Class o;
        return o.Fn_Fn1_Fn2(
            [](uint32 x) -> uint4 {return 0;},
            [](uint2 x) -> uint4 {return 0;});
    }

expected:0

    inline uint4 Foo(bool b)
    {
        return 0;
    }

    inline uint4 Run()
    {
        static Class o;
        return o.Fn_Fn1_Fn2(
            [](uint32 x) -> uint4 {return 0;}, Foo);
    }

expected:0

    template <typename T>
    inline T Foo(const T x)
    {
        return 100 / x;
    }

    inline void main()
    {
        Foo(10);
    }

expected:0

    template <auto M>
    using X = uint<M>;

    class Foo
    {
    public:
        X<10> x;

        template <auto N>
        inline void bar(uint<N> data) {}
    };


    inline void main()
    {
        static Foo foo;

        uint15 a;
        foo.bar(a);
    }

expected:0

    enum E : uint4
    {
        Nine = 9,
        Ten
    }

    inline void main()
    {
        uint32[E::Ten] x;

        Fn_T_10(x);
    }

expected:0

    inline void main()
    {
        Foo(10, true);
    }

    template <typename T, typename U>
    inline void Foo(T x, U y)
    {
    }

expected:0

    inline void Foo(auto n)
    {
    }

    inline void main()
    {
        Foo(10);
    }

expected:0

    inline auto Foo(auto n)
    {
        return n;
    }

    inline void main()
    {
        uint4 x = Foo(10);
    }

expected:0

    template <typename T>
    inline void Foo(T x, auto n)
    {
    }

    inline void main()
    {
        Foo<bool>(true, 10);
        Foo(true, 10);
    }

expected:0

    inline void Foo(auto x, auto n)
    {
    }

    inline void main()
    {
        Foo(true, 10);
    }

expected:0

    class Bar
    {
    public:
        inline void Foo(auto x, auto n)
        {
        }
    }

    inline void main()
    {
        static Bar bar;

        bar.Foo(true, 10);
    }

expected:0

    class Bar
    {
    public:
        template <typename T>
        inline void Foo(T x, auto n)
        {
        }
    }

    inline void main()
    {
        static Bar bar;

        bar.Foo<bool>(true, 10);
        bar.Foo(true, 10);
    }

expected:0

    template <typename T>
    inline void Foo(auto x, (T) -> void Do)
    {
    }

    inline void main()
    {
        Foo(10, [](uint32 i){});
    }

expected:0

    template <typename T>
    inline void Foo((T) -> void Do, auto x)
    {
    }

    inline void main()
    {
        Foo([](uint32 i){}, 10);
    }

expected:0

    inline void main()
    {
        Fn_Fn_uint32<Fn_T>();
    }

expected:0

    inline void main()
    {
        Fn_Fn_uint32_Fn_bool<Fn_T, Fn_T>();
    }

expected:0

    template <typename T>
    inline void Do(T x)
    {
        auto y = bitsizeof(T);
    }

    inline void Go()
    {
        uint32[10] x;

        Do(x);
    }

expected:0

    inline void main()
    {
        auto x = Fn_T_10({1, 2, 3});

        check_type<uint2>(x);
    }

expected:0

    inline void main()
    {
        auto x = Fn_T_10({1, 2, 3});

        check_type<int2>(x);
    }

expected:14

    inline void main()
    {
        Fn_T_10({1, 2, 3, 0, 2, 2, 2, 2, 2, 2, 2});
    }

expected:1

    inline void main()
    {
        auto x = Fn_T_10({1, 2, -3});

        check_type<int3>(x);
    }

expected:0

    inline void main()
    {
        auto x = Fn_T_10({false, true});

        check_type<bool>(x);
    }

expected:0

    inline void main()
    {
        Fn_T_10({false, 2, -3});
    }

expected:1

    inline void main()
    {
        auto x = Fn_T_N({1, 2, -3});

        check_type<int3[4]>(x);
    }

expected:0

    inline void main()
    {
        auto x = Fn_T_N<int32, 8>({1, 2, -3});

        check_type<int32[9]>(x);
    }

expected:0

    inline void main()
    {
        auto x = Fn_T_N_M({{1, 2, -3}});

        check_type<int3[1][3]>(x);
    }

expected:0

    inline void main()
    {
        auto x = Fn_T_N_M({{1, 2, -3}, {2, -1, 2}});

        check_type<int3[2][3]>(x);
    }

expected:0

    inline void main()
    {
        auto x = Fn_T_N_M({{1, 2, -3}, {2}});
    }

expected:1

    inline void main()
    {
        int3[3] a;

        auto x = Fn_T_N_T_N({1, 2, -3}, a);

        check_type<int3[4]>(x);
    }

expected:0

    inline void main()
    {
        int32[3] a;

        auto x = Fn_T_N_T_N({1, 2, -3}, a);

        check_type<int32[4]>(x);
    }

expected:0

    inline void main()
    {
        int32[3] a;

        auto x = Fn_T_N_T_N({false, true, true}, a);

        check_type<int32[4]>(x);
    }

expected:1

    inline void Go((uint32) -> void Do)
    {
    }

    inline void Foo(auto x)
    {
    }

    class C
    {
    public:
        inline void Run()
        {
            const auto fn = []()
            {
                Go(Foo);
            };
        }
    }

expected:0

    template <typename T, T x = {}>
    inline void Foo(T x)
    {
    }

    inline void main()
    {
        Foo(10);

        Struct s;

        Foo(s);
    }

expected:0

    template <typename T, typename U, T x = 0>
    inline void Foo(T x, (U) -> U fn)
    {
    }

    inline void main()
    {
        Foo(10, [](bool x) -> bool {return x;});
    }

expected:0

    inline void Foo()
    {
    }

    inline void main()
    {
        static C_Fn_Go<Foo> bar;
        bar.Go(10);
    }

expected:0

    inline void Go(auto x)
    {
    }

    inline void Foo()
    {
    }

    inline void main()
    {
        static C_Fn_Go<Foo> bar;
        inline void Go(auto x)
        {
        }

        bar.Go(10);
    }

expected:0

    inline void Go(uint4 x)
    {
    }

    inline void Foo()
    {
    }

    inline void main()
    {
        static C_Fn_Go<Foo> bar;
        inline void Go(uint4 x)
        {
        }

        bar.Go(10);
    }

expected:0

    template <auto N>
    inline void Go(uint<N> x)
    {
    }

    inline void Foo()
    {
    }

    inline void main()
    {
        template <auto N>
        inline void Go(uint<N> x)
        {
        }

        static C_Fn_Go<Foo> bar;
        bar.Go(10);
    }

expected:0

    template <typename T, typename R,  auto N>
    inline void Foo((T) -> R fn, T[N] a)
    {
    }

    inline void main()
    {
        uint4[5] a = {0, 1, 2, 3, 4};

        Foo([](uint32 x){return x;}, a);
    }

expected:0

    template <typename T, typename R,  auto N>
    inline void Foo(T[N] a, (T) -> R fn)
    {
    }

    inline void main()
    {
        uint4[5] a = {0, 1, 2, 3, 4};

        Foo(a, [](uint32 x){return x;});
    }
expected:0

    template <typename T, auto N>
    inline void Foo(T[N] a)
    {
    }

    struct S
    {
        bool x;
        uint4 y;
    }

    inline void main()
    {
        Foo({{true, 2}, {false, 200}});
    }

expected:1

    template <typename T, auto N>
    inline void Foo(T[N] a, T x)
    {
    }

    struct S
    {
        bool x;
        uint4 y;
    }

    inline void main()
    {
        S s = {false, 300};

        Foo({{true, 2}, {false, 200}}, s);
    }

expected:0

    template <typename T, T fn>
    struct Fun
    {
    }

    template <(uint32) -> void fn>
    inline void Go(Fun<(uint32) -> void, fn> f)
    {
        fn(10);
    }

    inline void Do(uint32 x)
    {
    }

    inline void main()
    {
        Fun<(uint32) -> void, Do> fun;

        Go(fun);
    }

expected:0

    template <(uint32) -> void fn>
    struct Fun
    {
    }

    template <(uint32) -> void fn>
    inline void Go(Fun<fn> f)
    {
        fn(10);
    }

    inline void Do(uint32 x)
    {
    }

    inline void main()
    {
        Fun<Do> fun;

        Go(fun);
    }

expected:0

    template <typename T, typename U>
    struct Foo
    {
        T x;
        U y;
    }

    template <typename T, typename U>
    inline void Go(Foo<T, U> foo)
    {
    }

    inline void main()
    {
        Go({10, 1000});
    }

expected:0
