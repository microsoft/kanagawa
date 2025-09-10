// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    const auto foo = Enum4::One;
    static assert(bitsizeof(foo) == bitsizeof(Enum4));

expected:0

    inline void main()
    {
        for (const auto x : 8)
        {
            static assert(bitsizeof(x) == 3);
        }
    }

expected:0

    inline void main()
    {
        for (const auto x : 10)
        {
            static assert(bitsizeof(x) == 4);
        }
    }

expected:0

    inline void Run(uint32 y)
    {
        for (const auto x : y)
        {
            static assert(bitsizeof(x) == bitsizeof(y));
        }
    }

expected:0

    const uint32 y = 8;

    inline void main()
    {
        for (const auto x : y)
        {
            static assert(bitsizeof(x) == 3);
        }
    }

expected:0

    const uint32 y = 8;

    inline void main()
    {
        for (const auto x : y + 1)
        {
            static assert(bitsizeof(x) == 4);
        }
    }

expected:0

    inline void main()
    {
        static for (const auto x : 8)
        {
            static assert(bitsizeof(x) == 3);
        }
    }

expected:0

    inline void Run(uint32 y)
    {
        static for (const auto x : y)
        {
            static assert(bitsizeof(x) == bitsizeof(y));
        }
    }

expected:1

    const uint32 y = 10;

    inline void main()
    {
        static for (const auto x : y)
        {
            static assert(bitsizeof(x) == 4);
        }
    }

expected:0

    const int8 x = -128;

    static assert(bitsizeof(x) == 8);

    const auto y = -x;

    static assert(bitsizeof(y) == 9);

    static assert(x != y);
    static assert(x == -y);

expected:0

    const uint8 x = 255;

    static assert(bitsizeof(x) == 8);

    const auto y = -x;

    static assert(bitsizeof(y) == 9);

    static assert(x != y);
    static assert(x == -y);

expected:0

    inline auto Foo(() -> bool Fn)
    {
        return Fn();
    }

    inline void main()
    {
        auto x = Foo([](){return true;});
    }

expected:0

    inline auto Foo(() -> bool Fn)
    {
        decltype(Fn) f = Fn;

        return f();
    }

    inline void main()
    {
        auto x = Foo([](){return true;});
    }

expected:0

    inline auto Foo(() -> bool Fn)
    {
        decltype(Fn) f = Fn;

        auto x = f();

        return x;
    }

    inline void main()
    {
        auto x = Foo([](){return true;});
    }

expected:0

    inline auto Foo(() -> bool Fn)
    {
        decltype(Fn) f = Fn;

        auto x = f();

        return x;
    }

    inline void main()
    {
        bool b = false;
        auto x = Foo([b](){return b;});
    }

expected:0

    inline void main()
    {
        static for(auto x : 10)
        {
        }
    }

expected:1

    template <typename T>
    struct S
    {
        T x;
    }

    inline void main()
    {
        S<bool> s;

        auto x = s.x;
    }

expected:0

    template <typename T>
    inline void Foo()
    {
        struct Bar {};

        Bar bar;

        decltype(bar) bar1;
    }

    inline void main()
    {
        Foo<bool>();
    }

expected:0

    import data.memory

    template <typename T>
    class Foo
    {
        public:
            using t = T;
    }

    class Bar
    {
        using tt = Foo<uint32>::t;

        struct S { tt x; }

        memory<S, 16> _a1;

        inline void Do(bool b)
        {
            auto x = b ? _a1[1] : _a2[1];
        }

        const S[2] _a2 = {{}, {}};
    }

    inline void main()
    {
        static Bar bar;
    }

expected:0

    template <typename T>
    class Foo
    {
        struct t {}
    }

    template <typename T>
    class Bar
    {
        using X = Foo<T>;

        inline void Run(X::t x)
        {
            auto z = x;
        }

    }

    inline void main()
    {
        static Bar<uint10> bar;
    }

expected:0

    inline const auto Foo()
    {
        return 10;
    }

expected:1

    inline auto Run()
    {
        return println(0);
    }

expected:0

    inline auto Run()
    {
        return print(0);
    }

expected:0

    const auto x = 10 * 0;
    static assert(decltype(x) == uint1);

expected:0

    const auto x = 10;
    const auto y = x * 0;
    static assert(decltype(y) == uint1);

expected:0

    inline void main()
    {
        static uint32 x;
        static int32 y;
        static uint3 z;
        static int3 q;

        {
            auto a = x % 8;
            static assert (decltype(a) == uint3);
        }

        {
            auto a = x % 1;
            static assert (decltype(a) == uint1);
        }

        {
            auto a = y % 256;
            static assert (decltype(a) == int9);
        }

        {
            auto a = y % 1;
            static assert (decltype(a) == int2);
        }

        {
            auto a = z % 256;
            static assert (decltype(a) == uint3);
        }

        {
            auto a = z % 256;
            static assert (decltype(a) == uint3);
        }

        {
            auto a = q % 256;
            static assert (decltype(a) == int3);
        }
    }

expected:0

    inline void main()
    {
        static uint32 x;
        static int32 y;
        static uint3 z;
        static int3 q;
        const auto five = 5;

        {
            auto a = x % (3 + five);
            static assert (decltype(a) == uint3);
        }

        {
            auto a = y % (206 + five * 10);
            static assert (decltype(a) == int9);
        }

        {
            auto a = (five % 2);
            static assert (decltype(a) == uint1);
        }

        {
            auto a = z % (five * 10);
            static assert (decltype(a) == uint3);
        }

        {
            auto a = q % (1 << five);
            static assert (decltype(a) == int3);
        }
    }

expected:0

    inline void main()
    {
        static uint32 x;
        static int32 y;
        static uint3 z;
        static int3 q;

        {
            auto a = x & 8;
            static assert (decltype(a) == uint4);
        }

        {
            auto a = y & 256;
            static assert (decltype(a) == int10);
        }

        {
            auto a = y & 1;
            static assert (decltype(a) == int2);
        }

        {
            auto a = z & 256;
            static assert (decltype(a) == uint3);
        }

        {
            auto a = q & 256;
            static assert (decltype(a) == int10);
        }
    }

expected:0

    inline void main()
    {
        static uint32 x;
        static int32 y;
        static uint3 z;
        static int3 q;
        const auto five = 5;

        {
            auto a = x & (3 + five);
            static assert (decltype(a) == uint4);
        }

        {
            auto a = y & (206 + five * 10);
            static assert (decltype(a) == int10);
        }

        {
            auto a = y & (five % 2);
            static assert (decltype(a) == int2);
        }

        {
            auto a = z & (five * 10);
            static assert (decltype(a) == uint3);
        }

        {
            auto a = q & (1 << five);
            static assert (decltype(a) == int7);
        }
    }

expected:0

    inline void main()
    {
        static uint32 x;
        static int32 y;

        {
            auto a = x * 8;
            static assert (decltype(a) == uint35);
        }

        {
            auto a = x * -8;
            static assert (decltype(a) == int36);
        }

        {
            auto a = x * 0;
            static assert (decltype(a) == uint1);
        }

        {
            auto a = y * 2;
            static assert (decltype(a) == int33);
        }

        {
            auto a = y * -2;
            static assert (decltype(a) == int34);
        }

        {
            auto a = y * 1;
            static assert (decltype(a) == int32);
        }

        {
            auto a = y * 0;
            static assert (decltype(a) == int2);
        }
    }


expected:0

    inline void main()
    {
        static uint32 x;
        static int32 y;
        const auto n = 5;

        {
            auto a = x << n;
            static assert (decltype(a) == uint37);
        }

        {
            auto a = x >> n;
            static assert (decltype(a) == uint27);
        }

        {
            auto a = x >> 32;
            static assert (decltype(a) == uint1);
        }

        {
            auto a = y >> 32;
            static assert (decltype(a) == int2);
        }
    }

expected:0

    inline void main()
    {
        static uint32 x;
        const auto y = x / 0;
    }

expected:1

    inline void main()
    {
        static uint32 x;
        const auto y = x % 0;
    }

expected:1

    inline void main()
    {
        static uint32 x;
        const auto y = x / -1;
    }

expected:1

    inline void main()
    {
        static uint32 x;
        const auto y = x % -1;
    }

expected:1

    inline void main()
    {
        uint3 a;
        const auto x = 1 << a;

        static assert(decltype(x) == uint8);
    }

expected:0
