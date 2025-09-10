// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        const auto x = Foo<bool>(true);

        static assert (bitsizeof x == 6);
    }

expected:0

    inline void main()
    {
        const auto x = Foo(true);

        static assert (bitsizeof x == 6);
    }

expected:0

    inline void main()
    {
        const auto x = Foo(10);

        static assert (bitsizeof x == 9);
    }

expected:0

    inline void main()
    {
        const auto x = Foo<bool, uint4>(true);

        static assert (bitsizeof x == 7);
    }

expected:0

    inline void main()
    {
        const auto x = Foo<bool, uint4, 8>(true);

        static assert (bitsizeof x == 8);
    }

expected:0

    template <(bool) -> int6 Fn>
    inline void Check()
    {
    }

    inline void main()
    {
        Check<Foo<bool>>();
    }

expected:0

    template <(bool) -> int4 Fn>
    inline void Check()
    {
    }

    inline void main()
    {
        Check<Foo<bool, bool>>();
    }

expected:0

    inline void Check((bool) -> int4 Fn)
    {
    }

    inline void main()
    {
        Check(Foo<bool, bool>);
    }

expected:0

    inline void Check((bool) -> int6 Fn)
    {
    }

    inline void main()
    {
        const auto fn = Foo<bool>;

        Check(fn);
    }

expected:0

    inline void Check((bool) -> void Fn)
    {
    }

    template <typename T = bool>
    inline void F(T x)
    {
    }

    inline void main()
    {
        Check(F);
    }

expected:0

    inline void Check((bool) -> void Fn)
    {
    }

    template <typename T = bool>
    inline void F(T x)
    {
    }

    inline void main()
    {
        Check(F<uint32>);
    }

expected:1

    inline void main()
    {
        const auto x = Bar<uint14, uint13>(10);

        static assert (bitsizeof x == 13);
    }

expected:0

    inline void main()
    {
        const auto x = Bar<uint14, uint14>(10);

        static assert (bitsizeof x == 14);
    }

expected:0

    inline void main()
    {
        const auto x = Bar<uint14, uint12>(10);
    }

expected:1

    inline void main()
    {
        const auto x = Bar<uint15, uint13>(10);
    }

expected:1

    inline void main()
    {
        const auto x = Bar<uint13, uint13>(10);
    }

expected:0

    inline bool F2(bool x)
    {
        return x;
    }

    inline void main()
    {
        const auto x = Bar<bool, bool, F2>(true);

        static assert (bitsizeof x == 1);
    }

expected:0

    inline uint32 F2(uint32 x)
    {
        return x;
    }

    inline void main()
    {
        const auto x = Bar<uint32, uint32, F2>(10);

        static assert (bitsizeof x == 32);
    }

expected:0

    inline uint32 F2(uint32 x)
    {
        return x;
    }

    inline void main()
    {
        const auto x = Bar<uint32, int33, F2>(10);

        static assert (bitsizeof x == 33);
    }

expected:0

    inline uint32 F2(uint33 x)
    {
        return x;
    }

    inline void main()
    {
        const auto x = Bar<uint32, int33, F2>(10);

        static assert (bitsizeof x == 33);
    }

expected:0

    inline uint32 F2(uint31 x)
    {
        return x;
    }

    inline void main()
    {
        const auto x = Bar<uint32, int33, F2>(10);

        static assert (bitsizeof x == 33);
    }

expected:1

    inline void main()
    {
        static C1<uint32> c1;
        static assert(bitsizeof c1.x == 32);
        static assert(bitsizeof c1.y == 64);
    }

expected:0

    inline void main()
    {
        static C1<uint32, 10> c1;
        static assert(bitsizeof c1.x == 10);
        static assert(bitsizeof c1.y == 20);
    }

expected:0

    inline void main()
    {
        static C1<uint32, 10, 3> c1;
        static assert(bitsizeof c1.x == 10);
        static assert(bitsizeof c1.y == 3);
    }

expected:0

    inline void main()
    {
        static C2<uint32> c2;
        static assert(bitsizeof c2.x == 32);
        static assert(bitsizeof c2.y == 64);
    }

expected:0

    inline void main()
    {
        static C2<uint32, 10> c2;
        static assert(bitsizeof c2.x == 10);
        static assert(bitsizeof c2.y == 42);
    }

expected:0

    inline void main()
    {
        static C2<uint32, 10, 3> c2;
        static assert(bitsizeof c2.x == 10);
        static assert(bitsizeof c2.y == 3);
    }

expected:0

    inline void main()
    {
        static S1<> s1;
        static assert(bitsizeof s1.x == 32);
        static assert(bitsizeof s1.y == 1);
    }

expected:0

    inline void main()
    {
        static S1<bool> s1;
        static assert(bitsizeof s1.x == 1);
        static assert(bitsizeof s1.y == 1);
    }

expected:0

    inline void main()
    {
        static A1_t<uint32> x;
        static assert(bitsizeof x == bitsizeof S1<uint32>);
    }

expected:0

    inline void main()
    {
        static A1_t<uint32, bool> x;
        static assert(bitsizeof x == bitsizeof bool);
    }

expected:0

    inline void main()
    {
        static A2_t<uint7> x;
        static assert(bitsizeof x.x == 10 * 7);
        static assert(bitsizeof x.y == clog2(10));
    }

expected:0

    inline void main()
    {
        static A2_t<uint7, 20> x;
        static assert(bitsizeof x.x == 20 * 7);
        static assert(bitsizeof x.y == clog2(20));
    }

expected:0

    inline void main()
    {
        static A2_t<uint7, 20, bool> x;
        static assert(bitsizeof x.x == 20 * 7);
        static assert(bitsizeof x.y == 1);
    }

expected:0

    template <typename T = uint32>
    inline T Fun(T x)
    {
        return x;
    }

    inline void main()
    {
        const auto x = Fun(false);

        static assert (bitsizeof x == 1);
    }

expected:0

    template <typename T = 10>
    struct X
    {
        T x;
    }

    inline void main()
    {
        static X x;
    }

expected:1

    template <auto N = uint32>
    struct X
    {
        uint<N> x;
    }

    inline void main()
    {
        static X x;
    }

expected:1

    template <uint4 N = true>
    struct X
    {
        uint<N> x;
    }

    inline void main()
    {
        static X x;
    }

expected:1

    template <typename T>
    using A = T;

    template <typename T, template <typename> typename TT = A>
    class C
    {
        public:
            using addr = TT<T>;
    }

    inline void main()
    {
        static C<uint32>::addr bar;
    }

expected:0

    class X
    {
        public:
            template <typename T, auto N = bitsizeof T>
            int<N> Do(int<N> x)
            {
                return x;
            }
    }

    class Y
    {
        (int32) -> int32 callback;
    }

    inline void main()
    {
        static X x;
        static Y y = {.callback = x.Do<uint32>};
    }

expected:0
