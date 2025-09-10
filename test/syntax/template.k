// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    template <typename T>
    class Foo
    {
        T x;
    }

    template <typename T>
    class Bar
    {
    private:
        const auto N = bitsizeof(T);

        Foo<uint<N>> _foo;
    }

    inline void main()
    {
        static Bar<int32> bar;
    }

expected:0

    template <typename T, T N>
    class Foo
    {
        const T x = N;
    }

expected:0

    template <uint8 N, uint<N> M>
    class Foo
    {
        const uint<N> x = M;
    }

expected:0

    template <uint8 N, uint32 M, uint<N>[M] A>
    class Foo
    {
        const uint<N> x = M;
    }

    inline void main()
    {
        static Foo<1, 1, {1}> foo;
    }

expected:0

    template <uint8 N, uint<N>[M] A, uint32 M>
    class Foo
    {
        const uint<N> x = M;
    }

    inline void main()
    {
        static Foo<1, {1}, 1> foo;
    }

expected:1

    template <uint<bytesizeof int4> N>
    class Foo
    {
    }

expected:1

    template <uint<bytesizeof int8> N>
    class Foo
    {
    }

expected:0

    import type.stdtype

    template <auto N, index_t<N> M>
    class Foo
    {
    }

    inline void main()
    {
        static Foo<10, 0> foo;
    }

expected:0

    template <auto N, uint<clog2(N)> M>
    class Foo
    {
    }

    inline void main()
    {
        static Foo<10, 0> foo;
    }

expected:0

    template <typename T, typename R, (T, T) -> R F>
    class Foo
    {
        inline void Bar()
        {
            F();
        }
    }

expected:0

    template <typename T, typename R, (T, T) -> R F>
    class Foo
    {
        F x;
    }

expected:1

    template <typename T>
    class Foo
    {
        inline void Bar()
        {
            T++;
        }
    }

expected:1

    template <typename T>
    class Foo
    {
        inline void Bar()
        {
            T = 10;
        }
    }

expected:1

    template <typename T>
    class Foo
    {
        inline void Bar()
        {
            T.x;
        }
    }

expected:1

    template <typename T>
    class Foo
    {
        inline void Bar()
        {
            T();
        }
    }

expected:1

    template <uint32 N>
    class Foo
    {
        N();
    }

expected:1

    template <uint32 N>
    class Foo
    {
        N x;
    }

expected:1

    template <uint32 N>
    class Foo
    {
        N Bar()
        {
            return 0;
        }
    }

expected:1

    template <typename T, T N>
    class Foo
    {
        const N x;
    }

expected:1

    template <typename T, typename R, (S, T) -> R F>
    class Foo
    {
        inline void Bar()
        {
            F();
        }
    }

    inline uint32 Do(uint32 x, uint32 y)
    {
        return 0;
    }

    inline void main()
    {
        static Foo<uint32, uint32, Do> foo;
    }

expected:1

    template <uint32 N, typename R, (N, N) -> R F>
    class Foo
    {
        inline void Bar()
        {
            F();
        }
    }

expected:1

    template <typename T, typename R, (T, T) -> R F, F F1>
    class Foo
    {
        inline void Bar()
        {
            F();
        }
    }

expected:1

    template <typename T, typename R, (T, T) -> R F, (F) -> F F1>
    class Foo
    {
        inline void Bar()
        {
            F();
        }
    }

expected:1

    template <typename T, uint<bitsizeof T> N>
    class Foo
    {
    }

    inline void main()
    {
        static Foo<uint8, 0> foo;
    }

expected:0

    template <typename T, (uint<bitsizeof T>) -> void F>
    class Foo
    {
    }

    inline void Fn(uint8 x)
    {
    }

    inline void main()
    {
        static Foo<uint8, Fn> foo;
    }

expected:0

    template <typename T, (int<clog2(bytesizeof (uint<bitsizeof T>)) + 1>) -> void F>
    class Foo
    {
    }

    inline void Fn(int1 x)
    {
    }

    inline void main()
    {
        static Foo<uint8, Fn> foo;
    }

expected:0

    template <typename T, (int<clog2(bytesizeof (uint<bitsizeof T>)) + 1>) -> void F>
    class Foo
    {
    }

    inline void Fn(int2 x)
    {
    }

    inline void main()
    {
        static Foo<uint8, Fn> foo;
    }

expected:0

    template <typename T, (int<clog2(bytesizeof (uint<bitsizeof T>)) + 1>) -> void F>
    class Foo
    {
    }

    inline void Fn(int2 x)
    {
    }

    inline void main()
    {
        static Foo<uint32, Fn> foo;
    }

expected:1

    inline void main()
    {
        uint32 i;

        auto result = (i < 0 && i > 0);
    }

expected:0

    inline void fn(() -> void callback)
    {
        callback();
    }

    class Foo
    {
    public:
        template <auto N>
        class Bar
        {
            public:
                template <typename T>
                inline void Do(T x)
                {
                    fn([](){});
                }
        }

        Bar<0> bar1;

        inline void Do()
        {
            bar1.Do(10);
        }
    }

    inline void main()
    {
        static Foo foo;
    }

expected:0

    template <typename T>
    inline void Go(T x)
    {
        template <auto I>
        inline void Do()
        {
            bool[I - 3] x;
        }

        Do<3>();
    }

    inline void main()
    {
        Go(10);
    }

expected:1

    template <typename T>
    class Foo
    {
        public:
            template <auto N>
            uint<N> Go()
            {
                return 0;
            }
    }

    inline void main()
    {
        static Foo<uint32> foo;

        auto x = foo.Go<32>();

        static Foo<decltype(x)> foo2;

        foo2.Go<20>();
    }

expected:0

    import type.stdtype

    template <typename T, index_t<bitsizeof(T)> N>
    class Foo
    {
        static assert(N == cast<uint2>(100));
    }

    inline void main()
    {
        static Foo<uint3, 100> foo; 
    }

expected:0

    import type.stdtype

    template <typename T, index_t<bitsizeof(T)> N>
    using Foo = index_t<N>;

    template <typename T, Foo<T, 100> N>
    class Bar
    {
        static assert(N == cast<uint2>(101));
    }

    inline void main()
    {
        static Bar<uint6, 101> bar; 
    }

expected:0    

    import type.stdtype

    template <typename T, index_t<bitsizeof(index_t<bitsizeof T>)> N>
    class Foo
    {
        static assert(N == cast<uint1>(3));
    }

    inline void main()
    {
        static Foo<uint3, 3> foo;
    }

expected:0

    import type.stdtype

    template <typename T, index_t<bitsizeof(index_t<bitsizeof T>)> N, count_t<N + 4> M>
    class Foo
    {
        static assert(cast<count_t<5>>(5) == M);
    }

    inline void main()
    {
        static Foo<uint3, 3, 5> foo; 
    }

expected:0

    template <auto N>
    class Foo
    {
    public:    
        using t = int32;
    }

    class Bar
    {
        using F = Foo<BAR>;    

        inline void Run()
        {
            auto fn = [](F::t x)
            {
                cast<uint32>(0xdeadbeef);
            };
        }
    }

    inline void main()
    {
        static Bar bar;
    }

expected:1

    template <auto M>
    class Bar
    {
    public:
        template <auto N>
        inline void Go()
        {
            uint32[N / M] x;
        }
    }

    template <auto N, auto M>
    class Foo
    {
    public:
        Bar<M> bar;

        inline void Go()
        {
            bar.Go<N>();
        }
    }

    inline void main()
    {
        static Foo<1, 1> foo;
        static Foo<4, 2> foo1;
    }

expected:0    

    template <typename T>
    class C
    {
        T::X x;
    }

    class Foo
    {
    public:
        using X = uint32;
    }

    inline void main()
    {
        static C<Foo> c;
    }

expected:0

    template <typename T>
    class C
    {
        T::Foo::X x;
    }

    class Bar
    {
    public:
        class Foo
        {
        public:
            using X = uint32;
        }
    }

    inline void main()
    {
        static C<Bar> c;
    }

expected:0

    template <typename T>
    class C
    {
        T::X x;
    }

    template <typename T>
    class D
    {
        C<typename T::Foo> c;
    }

    class Bar
    {
    public:
        class Foo
        {
        public:
            using X = uint32;
        }
    }

    inline void main()
    {
        static D<Bar> c;
    }

expected:0

    template <typename T>
    class C
    {
        T::X x;
    }

    template <typename T>
    class D
    {
        C<T::Foo> c;
    }

    class Bar
    {
    public:
        class Foo
        {
        public:
            using X = uint32;
        }
    }

    inline void main()
    {
        static D<Bar> c;
    }

expected:1

    template <typename T, T::Foo::X N>
    class C
    {
        static assert(N == cast<uint3>(10));
    }

    class Bar
    {
    public:
        class Foo
        {
        public:
            using X = uint3;
        }
    }

    inline void main()
    {
        static C<Bar, 10> c;
    }

expected:0

    template <typename T, typename U = T::Foo::X>
    class C
    {
        U x;
        static assert(bitsizeof x == bitsizeof typename T::Foo::X);
    }

    class Bar
    {
    public:
        class Foo
        {
        public:
            using X = uint7;
        }
    }

    inline void main()
    {
        static C<Bar> c;
    }

expected:0

    template <typename T, typename U = T::Foo::X>
    class C
    {
        U x;
        static assert(bitsizeof x == bitsizeof typename T::Foo::X);
    }

    class Bar
    {
    public:
        class Foo
        {
        public:
            struct X
            {
               uint7 x;
            }
        }
    }

    inline void main()
    {
        static C<Bar> c;
    }

expected:0

    template <typename T, uint32 N = T::Foo::Three>
    class C
    {
        static assert(N == 3);
    }

    class Bar
    {
    public:
        enum Foo : uint3
        {
            Three = 3
        }
    }

    inline void main()
    {
        static C<Bar> c;
    }

expected:0

    template <Foo N>
    class Bar
    {}

expected:1
