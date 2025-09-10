// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline uint32 Do(uint32 x)
    {
        return Bar<uint32, 32>(x);
    }

expected:0

    inline uint32 Do(uint32 x)
    {
        uint32 y;
        auto a = Bar<uint<32>, bitsizeof(uint<32>)>(y);
        return a;
    }

expected:0

    inline uint32 Do(uint32 x)
    {
        return Bar<uint16, 32>(x);
    }

expected:1

    inline uint32 Do(uint32 x)
    {
        return Bar<uint32>(x);
    }

expected:1

    inline uint32 Do(uint32 x)
    {
        return Bar<uint32, 32, 0>(x);
    }

expected:1

    template <typename T>
    inline T Foo(uint32 x)
    {
        return cast<T>(x);
    }

    inline uint32 Do(uint32 x)
    {
        return Foo<uint2>(x);
    }

expected:0

    class Main
    {
    public:
        void Do(uint32 x)
        {
            Bar1<uint4>(x);
        }
    }

    export Main;

expected:0

    class Main
    {
    public:
        void Do(uint32 x)
        {
            Bar1<uint2>(x);
        }
    }

    export Main;

expected:9

    inline void Foo()
    {
    }

    inline void Do()
    {
        Foo<uint2>();
    }

expected:1

    template <typename T>
    inline void Foo()
    {
    }

    inline void Do()
    {
        Foo();
    }

expected:1

    template <typename T>
    inline T Foo()
    {
        const T x = 0;
        return x;
    }

    inline uint32 Do()
    {
        return Foo<uint32>();
    }

expected:0

    template <typename T>
    class Foo
    {
    public:
        T Do(T x)
        {
            return Bar<T, bitsizeof T>(x);
        }
    }

    inline void main()
    {
        static Foo<uint32> foo;
    }

expected:0

    template <typename T>
    class Foo
    {
    public:
        T Do(T x)
        {
            return Bar<T, 1 + bitsizeof T>(x);
        }
    }

    inline uint32 main()
    {
        static Foo<uint32> foo;

        return foo.Do(3);
    }

expected:1

    template <typename T, uint8 N>
    inline void Foo(T x)
    {
        static [[memory]] uint32[512] a;

        const auto i = N;
        atomic
        {
            T y = a[i];
            y = x;
            a[i] = y;
        }
    }

    inline void Do(uint32 x)
    {
        Foo<uint32, 10>(x);
    }

expected:0

    template <typename T, uint8 N>
    inline void Foo(T x)
    {
        static for(const auto i : N)
        {
            x = x + i;
        }
    }

    inline void Do(uint32 x)
    {
        Foo<uint32, 10>(x);
    }

expected:0

    import hardware.dsp

    template <uint8 N>
    inline int<(N<<1)> Foo(int<N> x, int<N> y)
    {
        return x * y;
    }

    inline int32 Do(int16 x)
    {
        return Foo<16>(x, 20);
    }

expected:0

    template <typename T, uint8 N>
    inline T Foo(T[N] x)
    {
        return x[2];
    }

    inline uint32 Do()
    {
        uint32[16] x;
        return Foo<uint32, 16>(x);
    }

expected:0

    inline uint32 Do()
    {
        return Foo<uint32>(10);
    }

    template <typename T>
    inline T Foo(T x)
    {
        return x;
    }

expected:1

    inline void main()
    {
        Bar<uint32, 0> x;
    }

expected:1

    template <typename T>
    using Bar32 = Bar<T, 0>;

expected:1

    template <typename T>
    class Foo
    {
        const T x;
    }

    inline void Do(uint32 x)
    {
        Foo<uint32>();
    }

expected:1
