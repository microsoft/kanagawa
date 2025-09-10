// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void Go(() -> void Do)
    {
        decltype(Do) x = Do;

        x();
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    inline void Go(() -> void Do)
    {
        decltype(Do) x = 10;
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:1

    inline void Go(uint32 Do)
    {
        decltype(Do) x;
    }

    inline void main()
    {
        Go(10);
    }

expected:0

    inline void Go(uint32 Do)
    {
        decltype(Go) x;
    }

    inline void main()
    {
        Go(10);
    }

expected:1

    inline void Go(()->void Do)
    {
        decltype(Do) x;

        x = Do;

        x();
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    inline void Go(()->void Do)
    {
        using DoClosure = decltype(Do);

        DoClosure x = Do;

        x();
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    inline void Go(()->void Do)
    {
        static decltype(Do)[10] a;
        static uint32 x;

        a[x] = Do;
        x++;
        auto f = a[x-1];
        f();
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    import data.array

    inline void Go(()->void Do)
    {
        static array<decltype(Do), 10> a;
        static uint32 x;

        a[x] = Do;
        x++;
        auto f = a[x-1];
        f();
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    inline void Go(() -> void Do1, () -> void Do2)
    {
        decltype(Do1) c1 = Do1;
        decltype(Do2) c2 = Do2;
        c1();
        c2();
    }

    inline void main()
    {
        uint32 x;
        bool b;

        Go([b]() -> void {},
           [x]() -> void {});
    }

expected:0

    inline void Go(() -> void Do1, () -> void Do2)
    {
        decltype(Do1) c1 = Do2;
        decltype(Do2) c2 = Do1;
    }

    inline void main()
    {
        Go([]() -> void {},
           []() -> void {});
    }

expected:1

    template <typename T>
    inline void Go1(() -> void Do)
    {
        T x = Do;
        x();
    }

    inline void Go(() -> void Do)
    {
        Go1<decltype(Do)>(Do);
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    template <typename T>
    inline void Go1(T Do)
    {
        T x = Do;
        x();
    }

    inline void Go(() -> void Do)
    {
        Go1<decltype(Do)>(Do);
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    template <typename T>
    inline void Go1(T Do)
    {
        T x = Do;
    }

    inline void Go(() -> void Do1, () -> void Do2)
    {
        Go1<decltype(Do1)>(Do2);
    }

    inline void main()
    {
        Go([]() -> void {},
           []() -> void {});
    }

expected:1

    template <typename T>
    inline void Go1(() -> void Do)
    {
        T x = Do;
    }

    inline void Go(() -> void Do1, () -> void Do2)
    {
        Go1<decltype(Do1)>(Do2);
    }

    inline void main()
    {
        Go([]() -> void {},
           []() -> void {});
    }

expected:1

    template <typename T, typename R>
    inline void Go((T) -> R Do)
    {
        decltype(Do) x = Do;
        bool b = x(10);
    }

    inline void main()
    {
        Go<uint32, bool>([](uint32 x) -> bool {return x == 0;});
    }

expected:0

    template <typename T, typename R>
    inline void Go((T) -> R Do)
    {
        decltype(Do) x = Do;
        uint3 b = x(10);
    }

    inline void main()
    {
        Go<uint32, bool>([](uint32 x) -> bool {return x == 0;});
    }

expected:1

    inline void Go(() -> void Do)
    {
        struct Foo
        {
            decltype(Do) x;
        }

        Foo foo;

        foo.x = Do;

        auto f = foo.x;

        f();
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    inline void Go(() -> void Do)
    {
        struct Foo
        {
            decltype(Do) x;
        }

        Foo foo;

        foo.x = 10;
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:14

    template <typename Fn>
    inline void Go1(Fn fn)
    {
        fn();
    }

    inline void Go(() -> void Do)
    {
        Go1<decltype(Do)>(Do);
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    template <typename Fn, typename T>
    inline void Go1(() -> void fn1, Fn fn, (T) -> T fn2)
    {
        fn();
        T x = fn2(10);
    }

    inline void Go(() -> void Do)
    {
        Go1<decltype(Do)>(Do, Do, [](uint32 x){return x;});
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    inline void Go(() -> void Do)
    {
        using ClosureDo = decltype(Do);

        struct Foo
        {
            ClosureDo[10] a;
        }

        static Foo[10] foo;

        foo[2].a[3] = Do;

        auto f = foo[2].a[3];

        f();
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    inline void Go(() -> void Do)
    {
        using ClosureDo = decltype(Do);

        inline ClosureDo Fn(ClosureDo fn)
        {
            return fn;
        }

        auto fn = Fn(Do);

        fn();
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    inline void Go(() -> void Do)
    {
        using ClosureDo = decltype(Do);

        inline ClosureDo Fn(ClosureDo fn, () -> void fn2)
        {
            fn2();
            return fn;
        }

        auto fn = Fn(Do, Do);

        fn();
    }

    inline void main()
    {
        Go([]() -> void {});
    }

expected:0

    inline uint32 Do()
    {
        return 10;
    }

    inline void main()
    {
        const auto callback = [](uint32 x){return Do();};
    }

expected:0

    inline void Go(() -> uint32 Do)
    {
        Do();
    }

    inline void main()
    {
        uint32 a;
        uint32 b;
        uint32 c;
        uint32 d;
        uint32 e;
        uint32 f;
        uint32 g;
        uint32 h;
        uint32 i;
        uint32 j;
        uint32 k;
        uint32 l;
        uint32 m;
        uint32 n;
        uint32 o;
        uint32 p;
        uint32 r;
        uint32 s;
        uint32 t;
        uint32 u;
        uint32 v;

        Go([]() -> uint32{return 0;});
        Go([a]() -> uint32{return a;});
        Go([a, b]() -> uint32{return a + b;});
        Go([a, b, c]() -> uint32{return a + b + c;});
        Go([a, b, c, d]() -> uint32{return a + b + c + d;});
        Go([a, b, c, d, e]() -> uint32{return a + b + c + d + e;});
        Go([a, b, c, d, e, f]() -> uint32{return a + b + c + d + e + f;});
        Go([a, b, c, d, e, f, g]() -> uint32{return a + b + c + d + e + f + g;});
        Go([a, b, c, d, e, f, g, h]() -> uint32{return a + b + c + d + e + f + g + h;});
        Go([a, b, c, d, e, f, g, h, i]() -> uint32{return a + b + c + d + e + f + g + h + i;});
        Go([a, b, c, d, e, f, g, h, i, j]() -> uint32{return a + b + c + d + e + f + g + h + i + j;});
        Go([a, b, c, d, e, f, g, h, i, j, k]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l, m]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l + m;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l, m, n]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l + m + n;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, r]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + r;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, r, s]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + r;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, r, s, t]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + r + t;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, r, s, t, u]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + r + t + u;});
        Go([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, r, s, t, u, v]() -> uint32{return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + r + t + u + v;});
    }

expected:0

    template <auto N>
    class Foo
    {
    public:
        using t = uint32;

        inline void Go((t) -> void fn)
        {
        }
    }

    inline void Do(auto x)
    {
    }

    inline void main()
    {
        static Foo<32> foo;

        foo.Go(Do);
    }

expected:0

    template <auto N>
    class Bar
    {
    public:
        using s = uint32;
    }

    template <auto N>
    class Foo
    {
    public:
        using t = Bar<N>::s;

        inline void Go( (t) -> void fn)
        {
        }
    }

    inline void Do(auto x)
    {
    }

    inline void main()
    {
        static Foo<32> foo;

        foo.Go([](auto x){});
    }

expected:0

    inline auto Foo(uint32 y)
    {
        return [y](uint32 x)
        {
            return x + y;
        };
    }

    inline void main()
    {
        auto fn = Foo(10);

        fn(20);
    }

expected:0

    template <typename T>
    inline auto Go((T) -> bool Do, T x)
    {
        return Do(x);
    }

    inline void main()
    {
        Go([](auto x){return true;}, 10);
    }

expected:0

    template <typename T, typename R>
    inline void Go((T) -> R fn, T x)
    {
    }

    inline bool Do(auto x)
    {
    }

    inline void main()
    {
        Go(Do, 10);
    }

expected:0

    template <typename T, typename R>
    inline void Go((T) -> R fn, T x)
    {
    }

    template <typename T>
    inline T Do(auto x)
    {
    }

    inline void main()
    {
        Go(Do<bool>, 10);
    }

expected:0

    template <typename T, typename R>
    inline void Go((T) -> R fn, T x)
    {
    }

    class Foo
    {
        public:
            (uint32 x) -> bool callback;

            inline void Run()
            {
                Go(callback, 10);
            }
    }

    inline void main()
    {
        static Foo foo;

        foo.Run();
    }

expected:0

    inline void Go(() -> void Do)
    {
        Do();
    }

    template <auto N>
    struct Bar
    {
        uint<N> x;
    }

    inline void main()
    {
        uint32 x;

        Bar<2> bar;

        static for (const auto i : bitsizeof bar)
        {
            Go([x](){uint32 y = x;});

            uint<i+1> z;
        }

        Go([](){});
    }

expected:0
