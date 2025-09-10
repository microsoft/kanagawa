// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    using foo = uint32;

expected:0

    using foo = uint<32>;

expected:0

    using bar = foo;
    inline void main()
    {
        static bar x;
    }

expected:1

    using foo=uint2;
    using bar = foo;
    inline void main()
    {
        static bar x;
    }

expected:0

    using bar = int2;
    using foo=bar;
    inline void main()
    {
        static foo x;
    }

expected:0

    using foo32 = Bar<uint32>;

expected:0

    using foo32 = Bar;

expected:1

    const MyInt<2> x = 0;
    static assert(bitsizeof x == bitsizeof int<2>);

expected:0

    static assert(bitsizeof MyInt<4> == bitsizeof int<4>);

expected:0

    static assert(bitsizeof MyInt<4> != bitsizeof int<4>);

expected:1

    import data.array

    inline void main()
    {
        array<int8, 10> a8;

        int8[10] b8;

        static assert(bitsizeof a8 == bitsizeof b8);
    }

expected:0

    import data.array

    inline void main()
    {
        array<int8, 10> a8;

        int7[10] b8;

        static assert(bitsizeof a8 == bitsizeof b8);
    }

expected:1

    import data.array

    inline void main()
    {
        array<MyInt<8>, 10> a8;

        int8[10] b8;

        static assert(bitsizeof a8 == bitsizeof b8);
    }

expected:0

    import data.array

    inline void main()
    {
        array<array<MyInt<7>, 4>, 5> x;
        using a = int<7>[4];

        a[5] y;

        static assert(bitsizeof x == bitsizeof y);
    }

expected:0

    template <uint32 N, uint16 N>
    using x = int<N>;

expected:1

    inline void main()
    {
        MyInt<> x;
    }

expected:1

    inline void main()
    {
        MyInt<10, 10> x;
    }

expected:1

    import data.array

    inline void main()
    {
        array<int32> x;
    }

expected:1

    import data.array

    inline void main()
    {
        array<32> x;
    }

expected:1

    import data.array

    inline void main()
    {
        array<int32, 10, 20> x;
    }

expected:1

    template <uint32 Foo>
    using Foo = uint<Foo>;

    inline void main()
    {
        Foo x;
    }

expected:1

    template <uint32 Foo>
    using Foo = uint<Foo>;

    inline void main()
    {
        Foo<10> x;

        static assert(bitsizeof x == 10);
    }

expected:0

    import data.array

    template <uint32 N>
    using int_array = array<int<N>, N>;

    inline void main()
    {
        int_array<10> x;

        int<10>[10] y;

        static assert(bitsizeof x == bitsizeof y);
    }

expected:0

    template <typename T>
    using Foo = T;

    inline void main()
    {
        T x;
    }

expected:1

    template <typename T, uint8 N>
    class A
    {
    public:
        template <typename T>
        using Foo = T[2];

        Foo<T> x;
        T[2] y;

        static assert(bitsizeof x == bitsizeof y);
        static assert(bitsizeof x == 2 * N);
    }

    inline void main()
    {
        static A<uint16, 16> a;
    }

expected:0

    class A
    {
    public:
        template <typename T>
        using Foo = T;
    }

    inline void main()
    {
        static A::Foo<uint32> x;

        static assert(bitsizeof x == bitsizeof uint32);
    }

expected:0

    import data.array

    inline void main()
    {
        static Bar<array<MyInt<12>, 10>> bar;
    }

expected:0

    import data.array

    inline void main()
    {
        array<MyInt<10>, bitsizeof(array<int<2>, 2>)> x;

        static assert(bitsizeof x == 10 * 2 * 2);
    }

expected:0

    inline uint32 Foo()
    {
        using Return = uint32;

        Return x;

        return x;
    }

expected:0

    inline Return Foo()
    {
        using Return = uint32;

        Return x;

        return x;
    }

expected:1

    template <typename T>
    inline T Foo()
    {
        using Return = T;

        Return x;

        return x;
    }

expected:0

    inline uint32 Foo()
    {
        using Return=uint32;

        Return x;

        return x;
    }

expected:0

    inline uint32 Foo()
    {
        template <auto N>
        using Return = uint<N>;

        Return<32> x;

        return x;
    }

expected:0

    inline uint32 Foo()
    {
        using Return = bool;

        Return x;

        return x;
    }

expected:1

    inline uint32 Foo()
    {
        using Return = uint32;

        {
            using Return = bool;

            Return x;
        }

        Return x;

        return x;
    }

expected:0

    inline void Foo(uint32 x)
    {
        switch (x)
        {
            case 0:
                using Foo = uint32;
                Foo y = x;
                break;
        }
    }

expected:0

    template <auto N>
    class Foo
    {
    public:
        using A = uint<clog2(N)>;

        struct S
        {
            A x;
        }

        [[memory]] S[N] m;
    }

    inline void main()
    {
        static Foo<16> foo;
    }

expected:0

    template <auto N>
    using Foo = uint<N>;

    class Bar
    {
        using Foo = Foo<10>;
    }

expected:1

    template <auto N>
    using Foo = uint<N>;

    class Bar
    {
        inline void Go()
        {
            Foo_t x;
        }
        
        using Foo_t = Foo<10>;
    }

expected:0

    template <typename T>
    class Foo
    {
        public:
            using foo_t = T;
    }

    inline void main()
    {
        using bar_t = Foo<uint32>;

        inline void Do()
        {
            bar_t::foo_t x;
        }

        Do();
    }

expected:0
