// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void Go()
    {
        const auto fn = [](uint32 x) -> uint32
            {
                return x;
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        const auto fn = [](uint32 x)
            {
                return x;
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        const auto fn = [](auto x)
            {
                return x;
            };

        auto x = fn(10);
    }

expected:0

    inline void Go()
    {
        []() -> uint32
        {
            return x;
        };
    }

expected:1

    inline void Go()
    {
        const auto fn = [](uint32 x) -> uint32
            {
                return x;
            };

        fn(false);
    }

expected:1

    inline void Go()
    {
        const auto fn = [](uint32 x)
            {
                return;
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        const auto fn = [](uint32 x)
            {
                return {0};
            };

        fn(10);
    }

expected:1

    inline void Go()
    {
        const auto fn = [](uint32 x) -> void
            {
                return;
            };

        fn(10);
    }

expected:0

    inline void Do()
    {
    }

    inline void Go()
    {
        const auto fn = []() -> void
            {
                Do();
            };

        fn();
    }

expected:0

    inline void Go()
    {
        uint32 a = 10;

        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        uint32 a = 10;

        const auto fn = [a](uint32 x) -> uint32
            {
                return a ? 10 : 11;
            };

        fn(10);
    }

expected:26

    inline void Go(uint32 a)
    {
        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        uint32 a = 10;

        const auto fn = [](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

expected:32

    inline void Go()
    {
        auto a;

        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

expected:1

    inline void Go()
    {
        auto a = 10;

        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        auto a = 10;

        const auto fn = [a](auto x)
            {
                return x - a;
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        auto a = clog2(10);

        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        auto b = 123;
        auto a = bitsizeof(b);

        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

expected:0

    template <typename T>
    inline void Go(T x)
    {
        auto a = x;

        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

    inline void main()
    {
        Go<uint13>(10);
    }

expected:0

    template <typename T>
    inline void Go(T x)
    {
        T a = x;

        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

    inline void main()
    {
        Go<uint13>(10);
    }

expected:0

    inline void Go()
    {
        auto a = 10;

        const auto fn = [a](uint32 x) -> uint32
            {
                const auto fn = [a](uint32 x) -> uint32
                    {
                        return x - a;
                    };

                return fn(x);
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

expected:1

    inline void Go()
    {
        using a = uint32;

        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a;
            };

        fn(10);
    }

expected:1

    inline void Go()
    {
        uint32 a = 10;
        bool b;

        const auto fn = [a, b](uint32 x) -> uint32
            {
                return b ? x - a : x + a;
            };

        fn(10);
    }

expected:0

    inline void Go()
    {
        uint32 a = 10;
        bool b;

        const auto fn = [a](uint32 x) -> uint32
            {
                return b ? x - a : x + a;
            };

        fn(10);
    }

expected:32

    template <typename T>
    inline void Do(T a)
    {
        const auto fn = [a](T x) -> T
            {
                return x - a;
            };

        fn(10);
    }

    inline void Go()
    {
        Do<uint32>(10);
        Do<uint16>(10);
    }

expected:0

    inline void Go()
    {
        uint32 a = 10;
        bool b;

        const auto fn1 = [a, b](uint32 x) -> uint32
            {
                return b ? x - a : x + a;
            };

        const auto fn2 = [b, a](uint16 x) -> uint16
            {
                return b ? x - a : x + a;
            };

        fn1(10);
        fn2(11);
    }

expected:0

    struct Foo
    {
        uint32 x;
    }

    inline void Go()
    {
        Foo a = {10};

        const auto fn = [a](uint32 x) -> uint32
            {
                return x - a.x;
            };

        fn(10);
    }

expected:0

    const auto fn = [](uint32 x) -> uint32
        {
            return x;
        };

expected:0

    inline void Go()
    {
        inline uint32 Do(uint32 x)
        {
            const auto fn = [](uint32 x) -> uint32
                {
                    return x;
                };

            return fn(x);
        }

        Do(10);
    }

expected:0

    inline void Go()
    {
        const auto fn = [](uint32 x) -> uint32
            {
                const auto fn = [](uint32 x) -> uint32
                    {
                        return x;
                    };

                return fn(x);
            };

        fn(10);
    }

expected:0

    inline void Go(uint32 x)
    {
        const auto fn = [x](bool y) -> uint32
            {
                const auto fn = [y](uint32 z) -> uint32
                    {
                        return y ? z : 0;
                    };

                return fn(y ? x : 0);
            };

        fn(true);
    }

expected:0

    class C
    {
    public:
        inline void Go()
        {
            const auto fn = [](uint32 x) -> uint32
                {
                    return x;
                };

            fn(10);
        }
    }

    inline void main()
    {
        static C c;

        c.Go();
    }

expected:0

    class C
    {
    public:
        inline void Go()
        {
            const auto fn = [](auto x) -> uint32
                {
                    return x;
                };

            auto x = fn(10);
        }
    }

    inline void main()
    {
        static C c;

        c.Go();
    }

expected:0

    inline void Go()
    {
        {
            bool b;
            using Uint = uint32;

            const auto fn = [b](Uint x) -> Uint
                {
                    return b ? 0 : x;
                };

            fn(10);
        }

        bool b;
        const auto fn = [b](uint32 x) -> uint32
            {
                return b ? x : 0;
            };

        fn(11);
    }

expected:0

    inline void Go()
    {
        for (const uint32 i : 10)
        {
            const auto fn = [i](uint32 x) -> uint32
                {
                    return i + x;
                };

            fn(10);
        }

        for (const uint32 i : 10)
        {
            const auto fn = [i](uint32 x) -> uint32
                {
                    return i + x;
                };

            fn(10);
        }
    }

expected:0

    inline void main()
    {
        {
            struct S
            {
                uint32 x;
            }

            const auto fn = [](S x) -> S
            {
                return x;
            };

            fn({12});
        }

        {
            struct S
            {
                bool x;
            }

            const auto fn = [](S x) -> S
            {
                return x;
            };

            fn({false});
        }
    }

expected:0

    inline void main()
    {
        bool b = true;

        const auto fn = [b](uint32 x) -> uint32
        {
            return b ? x : 0;
        };

        {
            uint32 b;
        }

        fn(12);
    }

expected:0

    inline void main()
    {
        bool b = true;

        const auto fn = [b](uint32 x) -> uint32
        {
            return b ? x : 0;
        };

        inline void Do(uint32 b)
        {
        }

        fn(12);
    }

expected:0

    inline void Do()
    {
    }

    inline void main()
    {
        inline void Do()
        {
            bool b = true;

            const auto fn = [b](uint32 x) -> uint32
            {
                return b ? x : 0;
            };

            fn(12);
        }
    }

expected:0

    inline void Go()
    {
        const auto fn1 = []()
        {
            const auto fn2 = []()
            {
                const auto fn3 = [](){};
            };

            fn2();
        };
    }

expected:0

    inline void Go()
    {
        const auto fn1 = []()
        {
            const auto fn2 = []()
            {
                const auto fn3 = [](){};

                fn3();
            };

            fn2();
        };

        fn1();
    }

expected:0

    inline void Go()
    {
        const auto fn = []()
        {
            const auto fn = []()
            {
                const auto fn = [](){};

                fn();
            };

            fn();
        };

        fn();
    }

expected:0

    inline void Go()
    {
        const auto fn = []()
        {
            const auto fn = []()
            {
                const auto fn = []()
                {
                    const auto fn = []()
                    {
                        const auto fn = []()
                        {
                        };
                        fn();
                    };
                    fn();
                };
                fn();
            };
            fn();
        };
        fn();
    }

expected:0

    inline void Do((uint32 x) -> void fn)
    {
        fn(100);
    }

    inline void Go(auto x)
    {
        Do([x](auto n)
        {
            Do([n, x](uint32 y)
            {
                auto z = x + n + y;
            });
        });
    }

    inline void main()
    {
        Go(10);
    }

expected:0

    inline void ReturnReferenceFromFunction()
    {
        template <typename T>
        inline void Do(T x)
        {
        }

        inline auto Go(() -> void fn)
        {
            decltype(fn) x = fn;

            return fn;
        }

        class Foo
        {
        public:
            inline void Run()
            {
                Do(Go([](){}));
            }
        }

        static Foo _foo;

        _foo.Run();
    }

expected:0

    inline void main()
    {
        auto fn = [](){};

        fn();
    }

expected:0

    inline void main()
    {
        auto fn = []{};

        fn();
    }

expected:0

    inline void main()
    {
        auto fn = [] -> void {};

        fn();
    }

expected:0

    inline void main()
    {
        auto fn = [] -> uint32 {return 0;};

        fn();
    }

expected:0

    const auto inc = [](uint32 x)
    {
        return x + 1;
    };

    inline void main()
    {
        auto x = inc(10);
    }

expected:0

    const uint32 y = 0;

    const auto inc = [y](uint32 x)
    {
        return x + 1;
    };

expected:1
