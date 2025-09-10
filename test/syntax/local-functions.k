// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go(uint32 x)
        {
            return x;
        }

        return Go(x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go(uint32 y)
        {
            return x;
        }

        return Go(x);
    }

expected:1

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go(uint32 y)
        {
            inline uint32 Do(uint32 z)
            {
                return z;
            }

            return Do(y);
        }

        return Go(x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go(uint32 y)
        {
            inline uint32 Do1(uint32 z)
            {
                return z;
            }

            inline uint32 Do2(uint32 z)
            {
                return z;
            }

            return Do1(y) + Do2(y);
        }

        return Go(x);
    }

expected:0

    template <typename T>
    inline T Go(T x)
    {
        inline T Do(T y)
        {
            return y;
        }

        return Do(x);
    }

    inline void main()
    {
        uint32 x = Go<uint32>(10);
        bool b = Go<bool>(true);
    }

expected:0

    inline void Go()
    {
        struct Capture
        {
            uint32 x;
        }

        inline bool Do(Capture x, bool y)
        {
            return y ? x.x > 0 : x.x < 0;
        }

        Capture capture;

        capture.x = 10;

        auto x = Foo<Capture, bool, Do>(capture, true);
    }

expected:0

    inline void Go()
    {
        struct Capture
        {
            uint32 x;
        }

        inline bool Do(Capture x, bool y)
        {
            inline bool _Do(uint32 x, bool y)
            {
                return y ? x > 0 : x < 0;
            }

            return _Do(x.x, y);
        }

        Capture capture;

        capture.x = 10;

        auto x = Foo<Capture, bool, Do>(capture, true);
    }

expected:0

    inline void Go()
    {
        struct Capture
        {
            uint32 x;
        }

        inline bool Do(Capture x, bool y)
        {
            return y ? x.x > 0 : x.x < 0;
        }

        Capture capture;

        capture.x = 10;

        auto x = Foo<Capture, uint32, Do>(capture, true);
    }

expected:1

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go(uint32 x)
        {
            return x;
        }

        const auto fn = Go;

        return fn(x);
    }

expected:0

    class C
    {
    public:
        inline uint32 Run(uint32 x)
        {
            inline uint32 Go(uint32 x)
            {
                return x;
            }

            return Go(x);
        }
    }

    inline void main()
    {
        static C c;

        c.Run(10);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go(uint32 x)
        {
            return x;
        }

        inline uint32 Do(uint32 x)
        {
            return Go(x);
        }

        return Do(x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        const auto size = 32;
        template <auto N>
        using UInt = uint<N>;

        inline uint32 Go(uint32 x)
        {
            UInt<size> i = x;
            return i;
        }

        return Go(x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        template <typename T>
        inline uint32 Go(T x)
        {
            return x;
        }

        return Go<uint32>(x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        [[async]] inline void Go(uint3 x)
        {
            return;
        }
    }

expected:18

    template <() -> void fn>
    inline void Fn()
    {
        fn();
    }

    template <typename T>
    inline void Go()
    {
        inline void Do()
        {
        }

        Do();
        Fn<Do>();
    }

    inline void main()
    {
        Go<bool>();
    }

expected:0

    inline void Go()
    {
        struct S{};
        S s;
        inline void Fn(S x)
        {
            struct S{};
        }
        Fn(s);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        template <typename T>
        inline auto Go(T x)
        {
            return x;
        }

        return Go(10);
    }

expected:0

    template <typename T>
    inline uint32 Go(T x)
    {
        return x;
    }

    inline uint32 Run(uint32 x)
    {
        template <typename T, typename U>
        inline U Go(T x)
        {
            return x;
        }

        return Go<uint32, uint32>(10);
    }

expected:0

    template <typename T>
    inline uint32 Go(T x)
    {
        return x;
    }

    inline uint32 Run(uint32 x)
    {
        template <typename T, typename U>
        inline U Go(T x)
        {
            return x;
        }

        return Go<uint32>(10);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        inline auto Go(uint32 x)
        {
            template <typename T>
            inline auto Do(T x)
            {
                return x;
            }

            return Do(x);
        }

        return Go(10);
    }

expected:0

    inline void main()
    {
        inline void Foo()
        {
            assert(false;
        }
    }

expected:1

    inline void Go(() -> void Do)
    {
    }

    inline void main()
    {
        template <typename T>
        inline void Foo()
        {
            Go([](){});
        }

        const auto fn = [](){};

        Foo<bool>();

        fn();
    } 

expected:0

    inline void Go(() -> void Do)
    {
    }

    inline void main()
    {
        inline void Foo()
        {
            Go([](){});
        }

        const auto fn = [](){};

        Foo();

        fn();
    } 

expected:0

    inline void main()
    {
        inline void Foo()
        {
            const auto fn = [](){};
            fn();
        }

        const auto fn = [](){};

        fn();

        Foo();
    } 

expected:0

    inline void Go(() -> void Do)
    {
    }

    inline uint32 Run(uint32 x)
    {
        inline auto Foo(uint32 x)
        {
            template <typename T>
            inline auto Do(T x)
            {
                Go([](){});
                return x;
            }

            const auto fn = [](){};

            fn();

            return Do(x);
        }

        return Foo(10);
    }

expected:0

    inline void Go((uint32) -> void Do)
    {
    }

    inline void main()
    {
        inline void Foo()
        {
            Go([](auto x){});
        }

        const auto fn = [](){};

        Foo();
    } 

expected:0

    inline void Go(() -> void Do)
    {
    }

    inline void Fn()
    {
    }

    inline void main()
    {
        template <() -> void Do>
        inline void Foo()
        {
            Do();
        }

        Foo<Fn>();

        Go(Foo<Fn>);
    }

expected:0

    inline void Do(auto x)
    {
        inline void Foo(decltype(x) y)
        {
        }
    }

    inline void main()
    {
        Do(10);
    }

expected:0

    inline void Go(() -> void Do)
    {
        decltype(Do) a;

        a = Do;

        auto f = a;
    }

    inline void main()
    {
        inline void Test()
        {
            Go([](){});
        }

        Test();
    }

expected:0

    import type.stdtype

    template <typename T>
    inline void Go((T) -> void Do)
    {
    }

    inline void main()
    {
        const auto N = 10;

        const count_t<N> i = 10;

        Go([](index_t<i> x)
        {
        });
    }

expected:0

    template <typename T>
    inline void Go((T) -> void Do)
    {
    }

    inline void main()
    {
        const auto N = 10;

        Go([N](uint32 x)
        {
            if (x == N)
            {
            }
        });
    }

expected:0

    inline void main()
    {
        do
        {
            inline void Do()
            {
            }

            Do();
        }
        while(false);
    }

expected:0

    inline void main()
    {
        do
        {
            template <typename T>
            inline void Do(T x)
            {
            }

            Do(10);
        }
        while(false);
    }

expected:0

    inline void main()
    {
        do
        {
            static for (const auto i : 10)
            {
                inline void Do(uint<i + 1> x)
                {
                }

                Do(10);
            }
        }
        while(false);
    }

expected:0

    inline void Go(()->void Do)
    {
        Do();
    }

    class C
    {
        void Run()
        {
            inline auto Bar()
            {}

            Go(Bar);
        }
    }

    export C;

expected:0
