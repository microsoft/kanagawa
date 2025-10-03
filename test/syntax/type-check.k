// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        int<bitsizeof (foo)> bar;
    }

expected:0

    inline void main()
    {
        int<bitsizeof (foo + 1)> bar;
    }

expected:1

    inline void main()
    {
        int<bitsizeof (1 * foo)> bar;
    }

expected:1

    inline void main()
    {
        int<bitsizeof (-foo)> bar;
    }

expected:1

    inline void main()
    {
        int<bitsizeof (clog2(foo))> bar;
    }

expected:1

    const int32 x = foo(2, 3, int32);

expected:1

    const int32 x = 2 ? bool : 3;

expected:1

    const int32 x = 2 ? 2 : bool;

expected:1

    const int32 x = int<bitsizeof int3> ? 2 : 5;

expected:1

    static assert(int32);

expected:1

    inline void main()
    {
        int32[10][int32] a;
    }

expected:1

    inline void main()
    {
        int32[3.14] a;
    }

expected:1

    inline void Bar()
    {
        int<4>;
        foo;
    }

expected:1

    template <typename T>
    class C
    {
        const int32 x = T;
    }

expected:1

    inline void main()
    {
        int<int32> x;
    }

expected:1

    inline void main()
    {
        uint<int<foo>> x;
    }

expected:1

    inline void main()
    {
        uint32 x;
        uint32[4] f = fan_out<uint32>(x);
    }

expected:1

    [[max_threads(int32)]] void Go()
    {
        return;
    }

expected:1

    inline void main()
    {
        uint32 x = umul27(uint16, 4);
    }

expected:1

    inline void main()
    {
        static Foo<10, 10> x;
    }

expected:1

    inline void main()
    {
        static Foo<int32, int32> x;
    }

expected:1

    inline void main()
    {
        static Foo<int32, 10> x;
    }

expected:0

    inline void main()
    {
        static Foo<int32, -10> x;
    }

expected:0

    inline void main()
    {
        static Foo<int<32>, 10+8> x;
    }

expected:0

    inline void main()
    {
        static Foo<int32, false> x;
    }

expected:1

    inline void main()
    {
        static Foo<int32, 10.0> x;
    }

expected:1

    inline void main()
    {
        static Foo<int<32>, 10==10> x;
    }

expected:1

    inline void main()
    {
        static Foo<bool, 10> x;
    }

expected:1

    inline void main()
    {
        static Foo<bool, 10.1> x;
    }

expected:1

    inline void main()
    {
        static Foo<bool, false> x;
    }

expected:0

    inline void main()
    {
        static Foo<bool, true> x;
    }

expected:0

    inline void main()
    {
        static Foo<bool, true==false> x;
    }

expected:0

    inline void main()
    {
        static Foo<bool, (10>11)> x;
    }

expected:0

    inline void main()
    {
        static Foo<float32, 10> x;
    }

expected:1

    inline void main()
    {
        static Foo<float32, -5> x;
    }

expected:1

    inline void main()
    {
        static Foo<float32, 1.0> x;
    }

expected:0

    inline void main()
    {
        static Foo<float32, -1.10> x;
    }

expected:0

    inline void main()
    {
        static Foo<float32, false> x;
    }

expected:1

    inline void main()
    {
        static Foo<auto, false> x;
    }

expected:1

    inline void main()
    {
        static NFoo<10, 10> x;
    }

expected:0

    inline void main()
    {
        static NFoo<10, 1.0> x;
    }

expected:1

    inline void main()
    {
        static T1<T1<int32>> t;
    }

expected:0

    inline void main()
    {
        static Foo<T1<int32>, 10> t;
    }

expected:1

    template <T1<bool> X>
    class C
    {
    }

    inline void main()
    {
        static C<0> c;
    }

expected:1

    inline void main()
    {
        static T2<int32> t2;
    }

expected:1

    inline void main()
    {
        static T2<32> t2;
    }

expected:1

    inline void main()
    {
        static T1<uint32[3]> t1;
    }

expected:0

    inline void main()
    {
        static T1<T2<32>> t2;
    }

expected:1

    inline void main()
    {
        static T3<10> t2;
    }

expected:0

    inline void main()
    {
        static T3<10*2> t2;
    }

expected:0

    inline void main()
    {
        static T3<10!=2> t2;
    }

expected:1

    inline void main()
    {
        static T3<false> t2;
    }

expected:1

    inline void main()
    {
        static T3<1.0> t2;
    }

expected:1

    inline void main()
    {
        static Foo<int32, cast<int32[2]>(1000)> x;
    }

expected:1

    inline void main()
    {
        static Foo<int32[2], cast<int32[2]>(1000)> x;
    }

expected:1

    template <auto N>
    class C
    {
    }

    inline void main()
    {
        static C<min(10, 12)> c;
    }

expected:1

    class C
    {
        public:
            inline void Go(uint8[4] x)
            {
            }
    }

    class B
    {
        public:
            inline void Go(uint32 x)
            {
                c.Go(cast<uint8[4]>(x));
            }

        C c;
    }

    inline void main()
    {
        static B b;

        b.Go(10);
    }

expected:0

    class C
    {
        public:
            inline void Go(uint8[5] x)
            {
            }
    }

    class B
    {
        public:
            inline void Go(uint32 x)
            {
                c.Go(cast<uint8[4]>(x));
            }

        C c;
    }

    inline void main()
    {
        static B b;

        b.Go(10);
    }

expected:1

    // Check for overflow when computing type of shift expression
    inline uint32 main(uint64 z)
    {
        uint32 x = 1 << z;

        return x;
    }

expected:1

    struct S
    {
        uint32 x;
        uint32 y;
    }

    inline void main()
    {
        bool c;

        {
            auto x = c;

            static assert(bitsizeof c == bitsizeof bool);

            S c;

            static assert(bitsizeof c == bitsizeof S);

            auto y = c;

            static assert(bitsizeof x == bitsizeof bool);
            static assert(bitsizeof y == bitsizeof S);
        }
    }

expected:0

    class C
    {
        public:
            template <typename T>
            inline void Do()
            {
            }
    }

    class D
    {
        public:
            template <auto N>
            inline void Do()
            {
                c.Do<bool>();
            }

        C c;
    }

    inline void main()
    {
        static D d;

        d.Do<10>();
    }

expected:0

    class C
    {
        public:
            template <typename T>
            inline void Do()
            {
            }
    }

    class Z
    {
        public:
            template <auto N>
            inline void Do()
            {
            }
    }

    inline void Go()
    {
        static C c;
        {
            c.Do<bool>();

            static Z c;

            c.Do<10>();
        }
    }

expected:0
