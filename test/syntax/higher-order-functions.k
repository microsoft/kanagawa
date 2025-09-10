// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline uint32 Run(uint32 x)
    {
        const auto fn = [](uint32 x) -> uint32
        {
            return 2 * x;
        };

        return Go(fn, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        const auto fn = [](auto x) -> uint32
        {
            return 2 * x;
        };

        return Go(fn, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        const auto fn = [a](uint32 x) -> uint32
        {
            return a + x;
        };

        return Go(fn, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        const auto fn = [a](uint32 x)
        {
            return a;
        };

        return Go(fn, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        const auto fn = [](uint32 x) -> void
        {
            return 2 * x;
        };

        return Go(fn, x);
    }

expected:1

    inline uint32 Run(uint32 x)
    {
        const auto fn = [](uint32 x) -> uint32
        {
            return 2 * x;
        };

        return Go(x, fn);
    }

expected:1

    inline uint32 Run(uint32 x)
    {
        return Go(x);
    }

expected:1

    inline uint32 Run(uint32 x)
    {
        uint32 a = 5;
        bool b = true;

        const auto fn1 = [a](uint32 x) -> uint32
        {
            return a + x;
        };

        const auto fn2 = [b](uint32 x) -> uint32
        {
            return b ? x : 2 * x;
        };

        return Go2(fn1, fn2, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        uint32 a = 5;
        bool b = true;

        const auto fn1 = [a](uint32 x) -> uint32
        {
            return a + x;
        };

        const auto fn2 = [b](uint16 x) -> uint32
        {
            return b ? x : 2 * x;
        };

        return Go2(fn1, fn2, x);
    }

expected:1

    inline void Do(() -> void Callback)
    {
        Callback();
    }

    inline void main()
    {
        const auto fn = []() -> void {};

        Do(fn);
    }

expected:0

    inline void Do(() -> void Callback)
    {
        Callback();
    }

    inline void main()
    {
        const auto fn = [](){};

        Do(fn);
    }

expected:0

    inline void Do(() -> void Callback1, () -> void Callback2)
    {
        Callback1();
        Callback2();
    }

    inline void main()
    {
        const auto fn = []() -> void {};

        Do(fn, fn);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        inline uint32 Foo(uint32 x)
        {
            return 2 * x;
        }

        const auto fn = [](uint32 x) -> uint32
        {
            return Foo(x);
        };

        return Go(fn, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        bool b;

        const auto fn = [b](uint32 x) -> uint32
        {
            return b ? x : 2 * x;
        };

        return Go3<uint32>(fn, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        bool b;

        const auto fn = [b](uint32 x) -> uint32
        {
            return b ? x : 2 * x;
        };

        return Go3(fn, x);
    }

expected:0

    inline bool Run(uint32 x)
    {
        bool b;

        const auto fn = [b](uint32 x, uint32 y) -> bool
        {
            return b ? x > y : x <= y;
        };

        return Go4<uint32, bool>(fn, x, x);
    }

expected:0

    inline bool Run(uint32 x)
    {
        bool b;

        const auto fn = [b](uint32 x, uint32 y)
        {
            return b ? x > y : x <= y;
        };

        return Go4<uint32, bool>(fn, x, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        bool b;

        const auto fn = [b](uint32 x) -> uint32
        {
            return b ? x : 2 * x;
        };

        return Go3<bool>(fn, x);
    }

expected:1

    inline bool Run(uint32 x)
    {
        bool b;

        const auto fn = [b](uint32 x, uint32 y) -> bool
        {
            return b ? x > y : x <= y;
        };

        return Go4<uint32, bool>(fn, x);
    }

expected:1

    inline bool Run(uint32 x)
    {
        bool b;

        const auto fn = [b](uint32 x) -> bool
        {
            return b && x > y;
        };

        return Go4<uint32, bool>(fn, x);
    }

expected:1

    template <(bool) -> uint32 Do>
    inline uint32 DoDo((uint32) -> uint32 Do)
    {
        return Do(10);
    }

    inline void main()
    {
        const auto fn = [](uint32 x) -> uint32 {return x;};

        inline uint32 Do(bool x)
        {
            return x ? 10 : 11;
        }

        DoDo<Do>(fn);
    }

expected:0

    template <(bool) -> uint32 Do1>
    inline uint32 DoDo((uint32) -> uint32 Do)
    {
        return Do(10) + Do1(true);
    }

    inline void main()
    {
        const auto fn = [](uint32 x) -> uint32 {return x;};

        inline uint32 Do(bool x)
        {
            return x ? 10 : 11;
        }

        DoDo<Do>(fn);
    }

expected:0

    using Callback = (uint32) -> uint32;

    inline uint32 Do(Callback callback, uint32 x)
    {
        return callback(x);
    }

    inline uint32 Run(uint32 x)
    {
        const auto fn = [](uint32 x) -> uint32
        {
            return 2 * x;
        };

        return Do(fn, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        return Go(
            [](uint32 x) -> uint32
            {
                return 2 * x;
            },
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        return Go(
            [](auto x) -> uint32
            {
                return 2 * x;
            },
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        uint32 a = 5;
        bool b = true;

        return Go2(
            [a](uint32 x) -> uint32
            {
                return a + x;
            },
            [b](uint32 x) -> uint32
            {
                return b ? x : 2 * x;
            },
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        uint32 a = 5;
        bool b = true;

        return Go2(
            [a](auto x) -> uint32
            {
                return a + x;
            },
            [b](auto x) -> uint32
            {
                return b ? x : 2 * x;
            },
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        bool b;

        return Go3<uint32>(
            [b](uint32 x) -> uint32
            {
                return b ? x : 2 * x;
            },
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        bool b;

        return Go3<uint32>(
            [b](uint33 x) -> uint32
            {
                return b ? x : 2 * x;
            },
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        bool b;

        return Go3<uint32>(
            [b](uint33 x) -> uint31
            {
                return b ? x : 2 * x;
            },
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        bool b;

        return Go3<uint32>(
            [b](uint33 x) -> int31
            {
                return b ? x : 2 * x;
            },
            x
        );
    }

expected:1

    inline uint32 Run(uint32 x)
    {
        bool b;

        return Go3<int32>(
            [b](int33 x) -> uint31
            {
                return b ? x : 2 * x;
            },
            x
        );
    }

expected:0

    inline bool Run(uint32 x)
    {
        bool b;

        return Go4<uint32, bool>(
            [b](uint32 x, uint32 y) -> bool
            {
                return b ? x > y : x <= y;
            },
            x,
            x
        );
    }

expected:0

    import helper.higher_order_functions as N

    inline bool Run(uint32 x)
    {
        bool b;

        return N::Go4<uint32, bool>(
            [b](uint32 x, uint32 y) -> bool
            {
                return b ? x > y : x <= y;
            },
            x,
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        uint32 a = 5;
        bool b = true;

        return Go(
            [](uint32 x) -> uint32
            {
                return 2 * x;
            },
            Go2([a](uint32 x) -> uint32
                {
                    return a + x;
                },
                [b](uint32 x) -> uint32
                {
                    return b ? x : 2 * x;
                },
                x
            )
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        uint32 a = 5;
        bool b = true;

        return Go(
            [](auto x) -> uint32
            {
                return 2 * x;
            },
            Go2([a](auto x) -> uint32
                {
                    return a + x;
                },
                [b](auto x) -> uint32
                {
                    return b ? x : 2 * x;
                },
                x
            )
        );
    }

expected:0

    inline void Run(uint32 x)
    {
        for (const uint32 i : 10)
        {
            Go([i](uint32 x) -> uint32
               {
                   return i + x;
               },
               x
            );
        }

        for (const uint32 i : 10)
        {
            Go([i](uint32 x) -> uint32
               {
                   return i + x;
               },
               x
            );
        }
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        static C c;
        bool b;

        return c.Go(
            [b](uint32 x) -> uint32
            {
                return b ? 2 * x : x;
            },
            x
        );
    }

expected:0

    inline bool Run(uint32 x)
    {
        static C c;
        bool b;

        return c.Go4<uint32, bool>(
            [b](uint32 x, uint32 y) -> bool
            {
                return b ? x > y : x <= y;
            },
            x,
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        static C2<uint32> c;
        bool b;

        return c.Go(
            [b](uint32 x) -> uint32
            {
                return b ? 2 * x : x;
            },
            x
        );
    }

expected:0

    inline bool Run(uint32 x)
    {
        static C2<uint32> c;
        bool b;

        return c.Go4<bool>(
            [b](uint32 x, uint32 y) -> bool
            {
                return b ? x > y : x <= y;
            },
            x,
            x
        );
    }

expected:0

    class A
    {
    public:
        inline uint32 Go((uint32) -> uint32 Do, uint32 x)
        {
            return Do(x);
        }

        uint32 Run(uint32 x)
        {
            bool b;

            return Go([b](uint32 x) -> uint32
                {
                    return Go([b](uint32 x) -> uint32
                        {
                            return b ? x : 0;
                        },
                        x
                    );
                },
                x
            );
        }
    }

    inline void main()
    {
        static A a;
        a.Run(10);
    }

expected:0

    class A
    {
    public:
        inline uint32 Go((uint32) -> uint32 Do, uint32 x)
        {
            return Do(x);
        }

        uint32 Run(uint32 x)
        {
            bool b;

            return Go([b](uint32 x) -> uint32
                {
                    return b ? x : 0;
                },
                x
            );
        }
    }

    inline void main()
    {
        static A a;
        a.Run(10);
    }

expected:0

    inline void main()
    {
        Go(DoIt, 10);
    }

expected:0

    inline void main()
    {
        Go(DoIt, Go(DoIt, 10));
    }

expected:0

    inline void main()
    {
        Go(DoIt, 10);
        Go(DoIt, 10);
        Go(DoIt, 10);
    }

expected:0

    inline void main()
    {
        Go2(DoIt, DoIt, 10);
    }

expected:0

    inline void main()
    {
        Go3<uint32>(DoIt, 10);
    }

expected:0

    inline void Do(() -> void This)
    {
        This();
    }

    inline void main()
    {
        inline void This()
        {
        }

        Do(This);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        static C c;
        return c.Go(
            DoIt,
            x
        );
    }

expected:0

    inline bool Do(uint32 x, uint32 y)
    {
        return x > y;
    }

    inline bool Run(uint32 x)
    {
        static C2<uint32> c;
        return c.Go4<bool>(Do, x, x);
    }

expected:0

    inline bool Fun(uint32 x, uint32 y)
    {
        return x > y;
    }

    inline bool Run(uint32 x)
    {
        const auto fn = [](uint32 Fun) -> void
        {
        };

        inline bool Do(uint32 x)
        {
            static C2<uint32> c;
            return c.Go4<bool>(Fun, x, x);
        }

        return Do(x);
    }

expected:0

    template <typename T>
    inline void Do(T x)
    {
        Go([](uint32 x) -> uint32 {return x;}, x);
    }

    inline void main()
    {
        Do<uint32>(10);
    }

expected:0

    template <typename T>
    inline T Do(T x)
    {
        return x;
    }

    inline void main()
    {
        Go2(Do<uint32>, Do<uint32>, 10);
    }

expected:0

    template <typename T>
    class Do
    {
    }

    inline void main()
    {
        Go2(DoIt, Do<uint32>, 10);
    }

expected:1

    template <typename T>
    inline T Do(T x)
    {
        return x;
    }

    inline void main()
    {
        Go2(DoIt, Do<uint32>, 10);
    }

expected:0

    template <typename T>
    inline T Do(T x)
    {
        return x;
    }

    inline void main()
    {
        Go2(Do<uint32>, DoIt, 10);
    }

expected:0

    template <typename T>
    inline T Do(T x)
    {
        return x;
    }

    template <typename T>
    inline void Run()
    {
        Go2(Do<T>, DoIt, 10);
    }

    inline void main()
    {
        Run<uint32>();
    }

expected:0

    template <typename T, auto N>
    inline T[N] Do(T[N] x)
    {
        return x;
    }

    template <typename T>
    inline void Run()
    {
        Go3<uint32[3]>(Do<T, 3>, {1, 2, 3});
    }

    inline void main()
    {
        Run<uint32>();
    }

expected:0

    template <typename T, (T, T) -> bool Predicate>
    inline T mux2(T x, T y)
    {
        return Predicate(x, y) ? x : y;
    }

    template <typename T>
    inline bool greater_than(T x, T y)
    {
        return x > y;
    }

    template <typename T, (T, T) -> T Fn, T N>
    inline T bind1st(T x)
    {
        return Fn(N, x);
    }

    inline void main()
    {
        Go3<uint32>(bind1st<uint32, mux2<uint32, greater_than<uint32>>, 13>, 10);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        return Go(Do, x);
    }

    inline uint32 Run(uint32 x)
    {
        const auto fn = [](uint32 x) -> uint32
        {
            return 2 * x;
        };

        return Foo(fn, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        return Go2(Do, Do, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        const auto fn = [a](uint32 x) -> uint32
        {
            return a + x;
        };

        return Foo(fn, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        return Go(Do, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        return Foo([a](uint32 x) -> uint32
            {
                return a + x;
            }, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        static C2<uint32> c;
        return c.Go(Do, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        return Foo([a](uint32 x) -> uint32
            {
                return a + x;
            }, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        return Go2(Do, Do, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        return Foo([a](uint32 x) -> uint32
            {
                return a + x;
            }, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        bool b;

        return Go2([b](uint32 x) -> uint32
            {
                return b ? x : 0;
            },
            Do, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        return Foo([a](uint32 x) -> uint32
            {
                return a + x;
            }, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        bool b;

        return Go2(Do,
            [b](uint32 x) -> uint32
            {
                return b ? x : 0;
            },
            x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        return Foo([a](uint32 x) -> uint32
            {
                return a + x;
            }, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        return Go2(Do, DoIt, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        return Foo([a](uint32 x) -> uint32
            {
                return a + x;
            }, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        return Go2(DoIt, Do, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        return Foo([a](uint32 x) -> uint32
            {
                return a + x;
            }, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        return Go3<uint32>(Do, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        return Foo([a](uint32 x) -> uint32
            {
                return a + x;
            }, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do1, (uint32) -> uint32 Do2, uint32 x)
    {
        return Go2(Do1, Do2, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        bool b;

        return Foo(
            [a](uint32 x) -> uint32
            {
                return a + x;
            },
            [b](uint32 x) -> uint32
            {
                return b ? x : 0;
            },
            x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do1, (uint32) -> uint32 Do2, uint32 x)
    {
        return Go2(Do2, Do1, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        bool b;

        return Foo(
            [a](uint32 x) -> uint32
            {
                return a + x;
            },
            [b](uint32 x) -> uint32
            {
                return b ? x : 0;
            },
            x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do, uint32 x)
    {
        uint32 y;

        return Go3<uint32>([x, y, Do](uint32 z) -> uint32
            {
                return Do(y) + x + z;
            },
            x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        return Foo([a](uint32 x) -> uint32
            {
                return a + x;
            }, x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do1, (uint32) -> uint32 Do2, uint32 x)
    {
        return Go2(
            [Do2](uint32 y)-> uint32
            {
                return Do2(y);
            },
            Do1, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        bool b;

        return Foo(
            [a](uint32 x) -> uint32
            {
                return a + x;
            },
            [b](uint32 x) -> uint32
            {
                return b ? x : 0;
            },
            x);
    }

expected:0

    inline uint32 Foo((uint32) -> uint32 Do1, (uint32) -> uint32 Do2, uint32 x)
    {
        return Go2(
            [Do2, Do1, x](uint32 y)-> uint32
            {
                return Do2(y) + Do1(x);
            },
            Do1, x);
    }

    inline uint32 Run(uint32 x)
    {
        uint32 a;

        a = 10;

        bool b;

        return Foo(
            [a](uint32 x) -> uint32
            {
                return a + x;
            },
            [b](uint32 x) -> uint32
            {
                return b ? x : 0;
            },
            x);
    }

expected:0

    class Foo
    {
    public:
        uint32 Run(uint32 x)
        {
            const auto fn = [](uint32 x) -> uint32
            {
                return 2 * x;
            };

            return Go(fn, x);
        }
    }

    inline void main()
    {
        static Foo foo;

        foo.Run(10);
    }

expected:0

    class Foo
    {
    public:
        class Bar
        {
        public:
            uint32 Run(uint32 x)
            {
                const auto fn = [](uint32 x) -> uint32
                {
                    return 2 * x;
                };

                return Go(fn, x);
            }
        }

        uint32 Run(uint32 x)
        {
            return bar.Run(x);
        }

        Bar bar;
    }

    inline void main()
    {
        static Foo foo;
        foo.Run(10);
    }

expected:0

    class Foo
    {
    public:
        uint32 Run(uint32 x)
        {
            uint32 a = 5;
            bool b = true;

            const auto fn1 = [a](uint32 x) -> uint32
            {
                return a + x;
            };

            const auto fn2 = [b](uint32 x) -> uint32
            {
                return b ? x : 2 * x;
            };

            return Go2(fn1, fn2, x);
        }
    }

    inline void main()
    {
        static Foo foo;
        foo.Run(10);
    }

expected:0

    inline uint32 Fn((uint32) -> uint32 Do, uint32 x)
    {
        uint32 y;

        return Go3<uint32>([x, y, Do](uint32 z) -> uint32
            {
                return Do(y) + x + z;
            },
            x);
    }

    class Foo
    {
    public:
        uint32 Run(uint32 x)
        {
            uint32 a;

            a = 10;

            return Fn([a](uint32 x) -> uint32
                {
                    return a + x;
                }, x);
        }
    }

    inline void main()
    {
        static Foo foo;
        foo.Run(10);
    }

expected:0

    class Foo
    {
    public:
        C2<uint32> c;

        bool Run(uint32 x)
        {
            bool b;

            return c.Go4<bool>(
                [b](uint32 x, uint32 y) -> bool
                {
                    return b ? x > y : x <= y;
                },
                x,
                x
            );
        }
    }

    inline void main()
    {
        static Foo foo;
        foo.Run(10);
    }

expected:0

    class Foo
    {
    public:
        bool Run(uint32 x)
        {
            bool b;

            return Go4<uint32, bool>(
                [b](uint32 x, uint32 y) -> bool
                {
                    return b ? x > y : x <= y;
                },
                x,
                x
            );
        }
    }

    inline void main()
    {
        static Foo foo;
        foo.Run(10);
    }

expected:0

    template <typename T1, typename T2>
    class Foo
    {
    public:
        bool Run(T1 x)
        {
            bool b;

            return Go4<T1, T2>(
                [b](T1 x, T1 y) -> bool
                {
                    return b ? x > y : x <= y;
                },
                x,
                x
            );
        }
    }

    inline void main()
    {
        static Foo<uint32, bool> foo;

        foo.Run(10);
    }

expected:0

    class Foo
    {
    public:
        template <typename T>
        uint32 Run(T x)
        {
            return Go(
                [](T x) -> T
                {
                    return 2 * x;
                },
                x
            );
        }
    }

    inline void main()
    {
        static Foo foo;
        foo.Run<uint32>(10);
    }

expected:0

    inline uint32 Fn((uint32) -> uint32 Do, uint32 x)
    {
        return Go2(Do, DoIt, x);
    }

    class Foo
    {
    public:
        uint32 Run(uint32 x)
        {
            uint32 a;

            a = 10;

            return Fn([a](uint32 x) -> uint32
                {
                    return a + x;
                }, x);
        }
    }

    inline void main()
    {
        static Foo foo;
        foo.Run(10);
    }

expected:0

    import helper.higher_order_functions as H

    inline uint32 Fn((uint32) -> uint32 Do, uint32 x)
    {
        return H::Go2(Do, H::DoIt, x);
    }

    class Foo
    {
    public:
        uint32 Run(uint32 x)
        {
            uint32 a;

            a = 10;

            return Fn([a](uint32 x) -> uint32
                {
                    return a + x;
                }, x);
        }
    }

    inline void main()
    {
        static Foo foo;
        foo.Run(10);
    }

expected:0

    template <typename T>
    inline auto Foo((uint32) -> T Bar, uint32 x)
    {
        return Bar(x);
    }

    inline auto Run()
    {
        uint32 x = Foo<uint32>([](uint32 x){return x;}, 10);

        return Foo<void>([](uint32 x){}, 10);
    }

expected:0

    template <auto N>
    inline void Foo(uint<N> i, (uint<N>) -> void body)
    {
    }

    inline void main()
    {
        Foo(10, [](uint4 x)
                          {
                              println(x);
                          });
    }

expected:0

    template <auto N, typename T>
    inline void Foo(T[N] i, (T) -> uint<N> body)
    {
    }

    inline void main()
    {
        bool[4] b;

        Foo(b, [](bool x)
               {
                   uint4 y;
                   return y;
               });
    }

expected:0

    inline void main()
    {
        auto x = closure<void, uint32>([]() -> uint32 {return 0;});

        x();
    }

expected:0

    inline void main()
    {
        auto x = closure<bool, uint32>([](bool b) -> uint32 {return 0;});

        x(true);
    }

expected:0

    inline void main()
    {
        auto x = closure<bool, uint32>([]() -> uint32 {return 0;});

        x(true);
    }

expected:1

    inline void main()
    {
        auto x = closure<void, uint32>([](bool b) -> uint32 {return 0;});

        x();
    }

expected:1

    inline void main()
    {
        inline uint32 Do(uint32 x)
        {
            return x;
        }

        Go([](uint32 x) -> uint32
            {
                return Go(Do, x);
            },
            10);
    }

expected:0

    template <typename T>
    inline T Do(T x)
    {
        return x;
    }

    inline void main()
    {
        Go([](uint32 x) -> uint32
            {
                return Go(Do<uint32>, x);
            },
            10);
    }

expected:0

    inline void main()
    {
        Go([](uint32 x) -> uint32
            {
                return Go(DoIt, x);
            },
            10);
    }

expected:0

    inline void main()
    {
        Go([](uint32 x) -> uint32
            {
                return Go([](uint32 x) -> uint32
                    {
                        return Go(DoIt, x);
                    },
                    x);
            },
            10);
    }

expected:0

    class Foo
    {
    public:
        inline uint32 Do(uint32 x)
        {
            return x;
        }

        inline void Run()
        {
            Go([](uint32 x) -> uint32
                {
                    return Go(Do, x);
                },
                10);
        }
    }

    inline void main()
    {
        static Foo foo;
        foo.Run();
    }

expected:0

    template <typename T>
    class Foo
    {
    public:
        T Do(uint32 x)
        {
            return x;
        }

        inline void Run()
        {
            Go([](uint32 x) -> uint32
                    {
                        return Do(x);
                    },
                    10);
        }
    }

    inline void main()
    {
        static Foo<uint32> foo;
    }

expected:0

    template <(uint32) -> uint32 Fn>
    class Foo
    {
        public:
            inline void Do()
            {
                Go([](uint32 i)
                {
                    return Fn(i);
                },
                10);
            }
    }

    inline void main()
    {
        static Foo<DoIt> foo;
        foo.Do();
    }

expected:0

    template <typename T, T value>
    inline T Foo((T) -> T Do)
    {
        return Do(value);
    }

    inline void main()
    {
        Foo<uint32, 10>([](uint32 x) { return x; });
    }

expected:0

    template <(uint32 x) -> uint32 Do>
    inline uint32 Foo(uint32 x)
    {
        return Go(Do, x);
    }

    inline uint32 Bar(uint32 x)
    {
        return x + x;
    }

    inline void main()
    {
        Foo<Bar>(10);
    }

expected:0

    template <(bool b) -> uint32 Do>
    inline uint32 Foo(uint32 x)
    {
        return Go(Do, x);
    }

    inline uint32 Bar(bool x)
    {
        return x ? 10 : 11;
    }

    inline void main()
    {
        Foo<Bar>(10);
    }

expected:1

    template <typename T, (T) -> T Do>
    inline uint32 Foo(uint32 x)
    {
        return Go(Do, x);
    }

    inline uint32 Bar(uint32 x)
    {
        return x + x;
    }

    inline void main()
    {
        Foo<uint32, Bar>(10);
    }

expected:0

    template <typename T, (T) -> T Do>
    inline uint32 Foo(uint32 x)
    {
        return Go(Do, x);
    }

    template <typename T>
    inline T Bar(T x)
    {
        return x + x;
    }

    inline void main()
    {
        Foo<uint32, Bar<uint32>>(10);
    }

expected:0

    template <typename T, (T) -> T Do>
    inline uint32 Foo(uint32 x)
    {
        return Go(Do, x);
    }

    template <typename T>
    inline T Bar(T x)
    {
        return x + x;
    }

    inline void main()
    {
        Foo<uint32, Bar<bool>>(10);
    }

expected:1

    template <typename T, (T x) -> T Do1, (T y) -> T Do2>
    inline uint32 Foo(uint32 x)
    {
        return Go2(Do1, Do2, x);
    }

    inline uint32 Bar(uint32 x)
    {
        return x + x;
    }

    inline void main()
    {
        Foo<uint32, Bar, Bar>(10);
    }

expected:0

    template <typename T, (T) -> T Do>
    inline uint32 Foo(uint32 x)
    {
        static C c;
        return c.Go(Do, x);
    }

    template <typename T>
    inline T Bar(T x)
    {
        return x + x;
    }

    inline void main()
    {
        Foo<uint32, Bar<uint32>>(10);
    }

expected:0

    template <typename T, typename R, (T, T) -> R Do>
    inline uint32 Foo(uint32 x, uint32 y)
    {
        static C c;
        return c.Go4(Do, x, y);
    }

    template <typename T>
    inline T Bar(T x, T y)
    {
        return x + x;
    }

    inline void main()
    {
        Foo<uint32, uint32, Bar<uint32>>(10, 11);
    }

expected:0

    template <typename T, typename R, (T, T) -> R Do>
    inline R Foo(uint32 x, uint32 y)
    {
        static C c;
        return c.Go4(Do, x, y);
    }

    template <typename T>
    inline T Bar(T x, T y)
    {
        return x + x;
    }

    inline void main()
    {
        Foo<uint32, bool, Bar<uint32>>(10, 11);
    }

expected:1

    template <typename T, typename R, (T, T) -> R Do>
    inline R Foo(uint32 x, uint32 y)
    {
        static C c;

        return c.Go4(Do, x, y);
    }

    template <typename T, typename R>
    inline R Bar(T x, T y)
    {
        return x > y;
    }

    inline void main()
    {
        Foo<uint32, bool, Bar<uint32, bool>>(10, 11);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go1((uint32) -> uint32 Do, uint32 x)
        {
            return Do(x) + Do(x + 1);
        }

        const auto fn = [](uint32 x) -> uint32
        {
            return 2 * x;
        };

        return Go1(fn, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go1((uint32) -> uint32 Do, uint32 x)
        {
            return Do(x) + Do(x + 1);
        }

        return Go1([](uint32 x) -> uint32
            {
                return 2 * x;
            },
            x
        );
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go1((uint32) -> uint32 Do, uint32 x)
        {
            return Do(x) + Do(x + 1);
        }

        return Go1(DoIt, x);
    }

expected:0

    inline uint32 Run(uint32 x)
    {
        inline uint32 Go1((uint32) -> uint32 Do, uint32 x)
        {
            return Do(x) + Do(x + 1);
        }

        inline uint32 DoIt1(uint32 x)
        {
            return x;
        }

        return Go1(DoIt1, x);
    }

expected:0

    template <typename T, (T) -> T Fn>
    inline uint32 Test(uint32 x)
    {
        inline uint32 Go1((T) -> T Do, uint32 x)
        {
            return Do(x) + Do(x + 1);
        }

        return Go1(Fn, x);
    }

    inline void main()
    {
        Test<uint32, DoIt>(10);
    }

expected:0

    template <typename T, (T) -> T Fn>
    inline uint32 Test(uint32 x)
    {
        inline uint32 Go1((T) -> uint64 Do, uint32 x)
        {
            return Do(x) + Do(x + 1);
        }

        return Go1(Fn, x);
    }

    inline void main()
    {
        Test<uint32, DoIt>(10);
    }

expected:0

    template <typename T, (T) -> T Fn>
    inline uint32 Test(uint32 x)
    {
        inline uint32 Go1((T) -> uint16 Do, uint32 x)
        {
            return Do(x) + Do(x + 1);
        }

        return Go1(Fn, x);
    }

    inline void main()
    {
        Test<uint32, DoIt>(10);
    }

expected:1

    inline uint32 Test((uint32) -> uint32 Fn, uint32 x)
    {
        inline uint32 Go1((uint32) -> uint32 Do, uint32 x)
        {
            return Do(x) + Do(x + 1);
        }

        return Go1(Fn, x);
    }

    inline void main()
    {
        Test(DoIt, 10);
    }

expected:0

    class Foo
    {
    public:
        inline void Go()
        {
            Do([](){});
        }

        inline void Do(() -> void fn)
        {
            fn();
        }

    }

    inline void main()
    {
        static Foo foo;
        foo.Go();
    }

expected:0

    class Foo
    {
    public:
        inline void Go(() -> void fn)
        {
            Do(fn);
        }

        inline void Do(() -> void fn)
        {
            fn();
        }

    }

    inline void main()
    {
        static Foo foo;
        foo.Go([](){});
    }

expected:0

    inline void foo(() -> void fn)
    {
        inline void exec()
        {
        }
    }

    template <auto N>
    class Foo
    {
        public:
            inline void run(auto iter)
            {
                foo([]()
                {
                });
            }
    }

    inline void main()
    {
        static Foo<1> test;

        test.run(0);
    }

expected:0

    inline auto Do(auto x)
    {
        return x;
    }

    inline void main()
    {
        Go(Do, 10);
    }

expected:0

    inline auto Do(auto x)
    {
        return x;
    }

    inline void main()
    {
        uint32 x;

        const auto fn = [x](){};

        Go(Do, x);
    }

expected:0

    inline auto Do(auto x)
    {
        return x;
    }

    class Foo
    {
        public:
            inline void Run()
            {
                Go(Do, 10);
            }
    }

expected:0

    inline void Run((uint32) -> uint32 Do)
    {
        const auto fn = [Do]()
        {
            Go(Do, 10);
        };
    }

    inline void Start()
    {
        Run(DoIt);
    }

expected:0

    inline void Run((uint32) -> uint32 Do)
    {
        const auto fn = [Do]()
        {
            Go([Do](uint32 x)
            {
                return Do(x);
            },
            10);
        };
    }

    inline void Start()
    {
        Run(DoIt);
    }

expected:0

    inline void Run((uint32) -> uint32 Do)
    {
        Go([Do](uint32 x)
        {
            return Go([Do](uint32 x)
            {
                return Go(Do, x);
            },
            x);
        },
        10);
    }

    inline void Start()
    {
        Run(DoIt);
    }

expected:0

    inline void Run((uint32) -> uint32 Do)
    {
        Go([Do](uint32 x)
        {
            return Go([Do](uint32 x)
            {
                return Do(x);
            },
            x);
        },
        10);
    }

    inline void Start()
    {
        Run(DoIt);
    }

expected:0

    const uint32 a = 10;

    inline void Run((uint32 x) -> uint32 Do)
    {
        Go3([Do](uint32 x)
        {
            return Go3([Do](uint32 x)
            {
                return Do(x);
            },
            x);
        },
        a);
    }

    inline void Start()
    {
        Run(DoIt);
    }

expected:0

    class Foo
    {
    public:
        inline void init((uint9)->uint32 fn)
        {
        }

        inline void run()
        {
        }
    }

    inline void run_tutorials()
    {
        static Foo foo;
        foo.init([](auto i){return i;});
    }

expected:0

    class Foo
    {
    public:
        inline void init((uint33)->uint32 fn)
        {
        }

        inline void run()
        {
        }
    }

    inline void run_tutorials()
    {
        static Foo foo;
        foo.init([](auto i){return i;});
    }

expected:0

    inline void Start()
    {
        Go5([](S<uint32> s)
        {
            return s.x;
        });
    }

expected:0

    struct S1
    {
        uint32 x;
    };

    inline void Start()
    {
        Go5([](S1 s)
        {
            return s.x;
        });
    }

expected:1

    inline void Start()
    {
        Go5([](uint32 s)
        {
            return s;
        });
    }

expected:1

    inline void Start()
    {
        Go6<10>(
            [](U<10> s)
            {
                return s;
            }
        );
    }

expected:0

    inline void Go((uint32, uint32 x) -> uint32 fn)
    {
    }

expected:0

    inline void Go((uint32 x, uint32) -> uint32 fn)
    {
    }

expected:0

    inline void Go((uint32 x, uint32 y) -> uint32 fn)
    {
    }

expected:0

    inline void Go((uint32 x) -> uint32 fn1, (uint32 x) -> uint32 fn2)
    {
    }

expected:0

    inline void Go((uint32 x, uint32 x) -> uint32 fn)
    {
    }

    inline void main()
    {
        Go([](uint32 a, uint32 b){return a;});
    }

expected:1

    class Foo
    {
    public:
        uint32 Run(uint32 x)
        {
            bool b;

            return c.Go(
                [b](uint32 x) -> uint32
                {
                    return b ? 2 * x : x;
                },
                x
            );
        }

    private:
        C c;
    }

expected:0

    class Foo
    {
    public:
        uint32 Run(uint32 x)
        {
            bool b;

            return c[1].Go(
                [b](uint32 x) -> uint32
                {
                    return b ? 2 * x : x;
                },
                x
            );
        }

    private:
        C[2] c;
    }

expected:0

    class Bar
    {
    public:
        C[2] c;
    }

    class Foo
    {
    public:
        uint32 Run(uint32 x)
        {
            bool b;

            return bar[1].c[1].Go(
                [b](uint32 x) -> uint32
                {
                    return b ? 2 * x : x;
                },
                x
            );
        }

    private:
        Bar[2] bar;
    }

expected:0

    class Bar
    {
    public:
        C[2] c;
    }

    inline uint32 Run(uint32 x)
    {
        static Bar[2] bar;

        bool b;

        return bar[1].c[1].Go(
            [b](uint32 x) -> uint32
            {
                return b ? 2 * x : x;
            },
            x
        );
    }

expected:0

    template <(bool) -> uint32 Do>
    class Bux
    {
    public:
        inline uint32 DoDo((uint32) -> uint32 Do)
        {
            return Do(10);
        }
    }

    inline void main()
    {
        const auto fn = [](uint32 x) -> uint32 {return x;};

        inline uint32 Do(bool x)
        {
            return x ? 10 : 11;
        }

        static Bux<Do> bux;

        bux.DoDo(fn);
    }

expected:0

    template <(bool) -> uint32 Do1>
    class Bux
    {
    public:
        inline uint32 DoDo((uint32) -> uint32 Do)
        {
            return Do(10) + Do1(true);
        }
    }

    inline void main()
    {
        const auto fn = [](uint32 x) -> uint32 {return x;};

        inline uint32 Do(bool x)
        {
            return x ? 10 : 11;
        }

        static Bux<Do> bux;

        bux.DoDo(fn);
    }

expected:0

    template <(uint32 x) -> uint32 Do>
    class Bux
    {
    public:
        inline uint32 Foo(uint32 x)
        {
            return Go(Do, x);
        }
    }

    inline uint32 Bar(uint32 x)
    {
        return x + x;
    }

    inline void main()
    {
        static Bux<Bar> bux;
        bux.Foo(10);
    }

expected:0

    template <(bool b) -> uint32 Do>
    class Bux
    {
    public:
        inline uint32 Foo(uint32 x)
        {
            return Go(Do, x);
        }
    }

    inline uint32 Bar(bool x)
    {
        return x ? 10 : 11;
    }

    inline void main()
    {
        static Bux<Bar> bux;
        bux.Foo(10);
    }

expected:1

    template <typename T, (T) -> T Do>
    class Bux
    {
    public:
        inline uint32 Foo(uint32 x)
        {
            return Go(Do, x);
        }
    }

    inline uint32 Bar(uint32 x)
    {
        return x + x;
    }

    inline void main()
    {
        static Bux<uint32, Bar> bux;
        bux.Foo(10);
    }

expected:0

    template <typename T, (T) -> T Do>
    class Bux
    {
    public:
        inline uint32 Foo(uint32 x)
        {
            return Go(Do, x);
        }
    }

    template <typename T>
    inline T Bar(T x)
    {
        return x + x;
    }

    inline void main()
    {
        static Bux<uint32, Bar<uint32>> bux;
        bux.Foo(10);
    }

expected:0

    template <typename T, (T) -> T Do>
    class Bux
    {
    public:
        inline uint32 Foo(uint32 x)
        {
            return Go(Do, x);
        }
    }

    template <typename T>
    inline T Bar(T x)
    {
        return x + x;
    }

    inline void main()
    {
        static Bux<uint32, Bar<bool>> bux;
        bux.Foo(10);
    }

expected:1

    template <typename T, (T x) -> T Do1, (T y) -> T Do2>
    class Bux
    {
    public:
        inline uint32 Foo(uint32 x)
        {
            return Go2(Do1, Do2, x);
        }
    }

    inline uint32 Bar(uint32 x)
    {
        return x + x;
    }

    inline void main()
    {
        static Bux<uint32, Bar, Bar> bux;
        bux.Foo(10);
    }

expected:0

    template <typename T, (T) -> T Do>
    class Bux
    {
        C c;

    public:
        inline uint32 Foo(uint32 x)
        {
            return c.Go(Do, x);
        }
    }

    template <typename T>
    inline T Bar(T x)
    {
        return x + x;
    }

    inline void main()
    {
        static Bux<uint32, Bar<uint32>> bux;
        bux.Foo(10);
    }

expected:0

    template <typename T, typename R, (T, T) -> R Do>
    class Bux
    {
        C c;

    public:
        inline uint32 Foo(uint32 x, uint32 y)
        {
            return c.Go4(Do, x, y);
        }
    }

    template <typename T>
    inline T Bar(T x, T y)
    {
        return x + x;
    }

    inline void main()
    {
        static Bux<uint32, uint32, Bar<uint32>> bux;
        bux.Foo(10, 11);
    }

expected:0

    template <typename T, typename R, (T, T) -> R Do>
    class Bux
    {
        C c;

    public:
        inline R Foo(uint32 x, uint32 y)
        {
            return c.Go4(Do, x, y);
        }
    }

    template <typename T>
    inline T Bar(T x, T y)
    {
        return x + x;
    }

    inline void main()
    {
        static Bux<uint32, bool, Bar<uint32>> bux;
        bux.Foo(10, 11);
    }

expected:1

    template <typename T, typename R, (T, T) -> R Do>
    class Bux
    {
        C c;

    public:
        inline R Foo(uint32 x, uint32 y)
        {
            return c.Go4(Do, x, y);
        }
    }

    template <typename T, typename R>
    inline R Bar(T x, T y)
    {
        return x > y;
    }

    inline void main()
    {
        static Bux<uint32, bool, Bar<uint32, bool>> bux;
        bux.Foo(10, 11);
    }

expected:0

    class Foo
    {
    public:
        inline void Run()
        {
            Go([](uint32 x)
            {
                return callback(x);
            }, 10);
        }

    private:
        (uint32 x) -> uint32 callback = DoIt;
    }

    inline void main()
    {
        static Foo foo;
        foo.Run();
    }

expected:0

    class Foo
    {
    public:
        inline void Run()
        {
            Go(callback, 10);
        }

    private:
        (uint32 x) -> uint32 callback = DoIt;
    }

    inline void main()
    {
        static Foo foo;
        foo.Run();
    }

expected:0

    class Foo
    {
    public:
        inline void Run()
        {
            Go(callback, 10);
        }

    private:
        (uint32 x) -> uint32 callback;
    }

    inline void main()
    {
        static Foo foo = {.callback = DoIt};
        foo.Run();
    }

expected:0

    class Foo
    {
    public:
        inline void Run()
        {
            Go3(callback, 10);
        }

    private:
        (uint32 x) -> uint32 callback;
    }

    inline void main()
    {
        static Foo foo = {.callback = DoIt};
        foo.Run();
    }

expected:0

    class Foo
    {
    public:
        inline void Run()
        {
            Go4(callback, 10, 20);
        }

    private:
        (uint32 x, uint32 y) -> bool  callback;
    }

    inline bool Bar(uint32 x, uint32 y)
    {
        return x < y;
    }

    inline void main()
    {
        static Foo foo = {.callback = Bar};
        foo.Run();
    }

expected:0

    const (uint32 x) -> uint32 callback = DoIt;

    inline void main()
    {
        Go(callback, 10);
    }

expected:0

    inline bool Do(uint32 x)
    {
        return x != 10;
    }

    inline bool main()
    {
        return Go7(Do, 10);
    }

expected:0

    inline uint32 main()
    {
        return Go7(DoIt, 10);
    }

expected:0

    inline bool Do(uint32 x)
    {
        return x != 10;
    }

    inline bool[10] Run()
    {
        return Go8(Do);
    }

expected:0

    inline uint32[10] Run()
    {
        return Go8(DoIt);
    }

expected:0

    inline auto Do(auto x)
    {
        return x;
    }

    inline uint4 Run()
    {
        return Go7(Do, 10);
    }

expected:0

    inline auto Do(auto x)
    {
        return x;
    }

    inline bool main()
    {
        return Go7(Do, false);
    }

expected:0

    inline uint33 Run(uint32 z)
    {
        return Go7([z](auto x)
                {
                    return x + z;
                },
                10);
    }

expected:0

    inline auto Do(auto x)
    {
        return x + x;
    }

    inline uint2[10] Run()
    {
        return Go8(Do);
    }

expected:0

    inline bool main()
    {
        return Go3<bool>([](auto x){return x;}, false);
    }

expected:0

    inline bool main()
    {
        return Go3<bool>([](bool x){return x;}, true);
    }

expected:0

    inline bool main()
    {
        return Go4([](uint32 x, uint32 y) -> bool
            {
                return x > y;
            },
            10,
            20);

    }

expected:0

    inline bool main()
    {
        return Go4([](uint32 x, uint32 y)
            {
                return x > y;
            },
            10,
            20);

    }

expected:0

    inline bool main()
    {
        return Go4([](auto x, auto y)
            {
                return x > y;
            },
            10,
            20);

    }

expected:0

    inline void DoIt1(bool b)
    {
    }

    inline void Bar((bool) -> void Do1, () -> void Do2)
    {
        Do1(true);
        Do2();
    }

    template<(bool) -> void cb>
    class Foo
    {
    private:
        inline void DoIt2()
        {
        }

    public:
        inline void start()
        {
            Bar(cb, DoIt2);
        }

    }

    inline void main()
    {
        static Foo<DoIt1> foo;
    }

expected:0

    inline void Bar((uint32) -> uint32 Do1, (bool) -> void Do2)
    {
        Do1(10);
        Do2(true);
    }

    template<(uint32) -> uint32 cb>
    class Foo
    {
        inline void DoIt(bool b)
        {
        }

    public:
        inline void start()
        {
            Bar(cb, DoIt);
        }
    }

    inline void main()
    {
        static Foo<DoIt> foo;
    }

expected:0

    inline void DoIt1()
    {
    }

    inline void Bar(() -> void Do1, (uint32) -> uint32 Do2)
    {
        Do1();
        Do2(0);
    }

    template<() -> void cb>
    inline void Test()
    {
        Bar(cb, DoIt);
    }

    inline void main()
    {
        Test<DoIt1>();
    }

expected:0

    inline void DoIt1()
    {
    }

    inline void Bar((bool) -> void Do1, () -> void Do2)
    {
        Do1(true);
        Do2();
    }

    template<(bool) -> void cb>
    inline void Test()
    {
        Bar(cb, DoIt1);
    }

    inline void main()
    {
        inline void DoIt1(bool b)
        {
        }

        Test<DoIt1>();
    }

expected:0

    template <typename T>
    inline void DoIt1(T x, auto y)
    {
    }

    inline void Bar((bool) -> void Do1, (uint32) -> void Do2)
    {
        Do1(true, 100);
        Do2(0, 100);
    }

    inline void Test()
    {
        Bar(DoIt1<bool>, DoIt1<uint32>);
    }

    inline void main()
    {
        Test();
    }

expected:0

    template <typename T>
    inline void DoIt1(T x, auto y)
    {
    }

    inline void Bar((bool, auto) -> void Do1, (uint32, auto) -> void Do2)
    {
        Do1(true, 100);
        Do2(0, 100);
    }

    inline void Test()
    {
        Bar(DoIt1<bool>, DoIt1<uint32>);
    }

    inline void main()
    {
        Test();
    }

expected:0

    template <typename T>
    inline T Bar(auto x)
    {
        return {};
    }

    inline uint32 Fn2((uint4) -> uint32 fn)
    {
        return {};
    }

    template <typename R>
    inline R Fn1()
    {
        return Fn2(Bar<R>);
    }


    template <auto N>
    class Foo
    {
    public:
        inline void Test()
        {
            Fn1<uint32>();
        }
    }

    inline void main()
    {
        static Foo<10> foo;

        Bar<uint32>(10);

        foo.Test();
    }

expected:0

    inline void Fn(() -> void Do)
    {
        Do();
    }

    class Foo
    {
        public:
            template <auto N>
            inline void Do()
            {
            }

            inline void Test()
            {
                const auto fn = []() -> void
                {
                    Fn(Do<0>);
                };

                fn();
            }
    }

expected:0

    inline uint32 add(uint32 x, uint32 y)
    {
        return x + y;
    }

    inline void main()
    {
        Go(Go9(add, 10), 20);
    }

expected:0

    inline auto add(auto x, auto y)
    {
        return x + y;
    }

    inline void main()
    {
        Go(Go9(add, 10), 20);
    }

expected:0

    inline uint32 add(uint32 x, uint32 y)
    {
        return x + y;
    }

    inline void main()
    {
        Go(Go9(add, false), 20);
    }

expected:1

    template<([[last]] bool) -> void Fn>
    class Foo
    {
    public:
        inline void Run()
        {
            Go_last(Fn);
        }
    }

    inline void main()
    {
        static Foo<DoEndTransacation> foo;

        foo.Run();
    }

expected:0

    inline void main()
    {
        Go_last(DoEndTransacation);
    }

expected:0

    template <typename T>
    inline void Do([[last]] T end)
    {
    }

    inline void main()
    {
        Go_last(Do<bool>);
    }

expected:0

    template <typename T>
    inline void Do(T end)
    {
    }

    inline void main()
    {
        Go_last(Do<bool>);
    }

expected:1

    template <typename T>
    inline T Do([[last]] auto end)
    {
        return {};
    }

    inline void main()
    {
        Go_last(Do<uint32>);
    }

expected:0

    inline void Bar(() -> void task)
    {
    }

    inline void Foo(() -> void task)
    {
        Bar([](){});
    }

    inline void test(uint32 tag)
    {
        bool x;

        Foo([tag, x]()
        {
            Foo([tag, x]()
            {
            });
        });
    }

expected:0

    import data.function

    inline void Bux((bool) -> void fn)
    {
        return fn(true);
    }

    inline void Foo(uint4 i, bool b)
    {
    }

    inline void Bar()
    {
        return Bux(bind1st(Foo, 10));
    }

expected:0

    class Foo
    {
    public:
        (type_t) -> void callback;

        inline void Test()
        {
            Go([](uint32 x)
            {
                Go10(callback);
                return x;
            }, 0);
        }
    }

    inline void main()
    {
        static Foo foo = {.callback = Do10};
        foo.Test();
    }

expected:0

    inline void invalid(type_t x)
    {
        // trigger middle end compiler error if function called
        invalid(x);
    }

    class Foo
    {
    public:
        const (type_t) -> void callback = invalid;

        inline void Test()
        {
            Go([](uint32 x)
            {
                Go10(callback);
                return x;
            }, 0);
        }
    }

    inline void main()
    {
        static Foo foo = {.callback = Do10};
        foo.Test();
    }

expected:0

    import data.function
    import numeric.int.operator

    const auto cipher = bind2nd(xor, 0xc0de);

expected:1

    import data.function
    import numeric.int.operator

    inline void main()
    {
        const auto cipher = bind2nd(xor, 0xc0de);

        auto x = cipher(cipher(123));
    }

expected:0

    import data.function
    import numeric.int.operator

    const auto xor_and_increment = compose2(increment, xor);

    inline void main()
    {
        auto x = xor_and_increment(14321, 423);
    }

expected:0

    inline void main()
    {
        auto x = add_and_increment(14321, 423);
    }

expected:0

    inline void main()
    {
        GoVar([]{});
    }

expected:0

    inline void Fn()
    {
    }

    inline void main()
    {
        GoVar(Fn);
    }

expected:0

    inline void main()
    {
        GoVar([](uint32 x){});
    }

expected:0

    inline void Fn(uint16 x)
    {
    }

    inline void main()
    {
        GoVar(Fn);
    }

expected:0

    inline void main()
    {
        GoVar([](uint32 x, uint16 y){});
    }

expected:0

    inline void main()
    {
        GoVar([](uint32 x, uint16 y, uint4 z){});
    }

expected:0

    inline void main()
    {
        GoVar([](uint32 x, uint16 y, uint4 z, int6 q){});
    }

expected:0

    inline void main()
    {
        GoVar([](uint32 x, uint16 y, uint4 z, int6 q, uint32 a){});
    }

expected:1

    inline void main()
    {
        GoVar<void, void, void, void>([]{});
    }

expected:0

    inline void main()
    {
        GoVar<uint32, uint32, uint32, void>([](uint32 x, uint32 y, uint32 z){});
    }

expected:0

    inline void main()
    {
        GoVar<void, uint32, uint32, uint32>([](uint32 x, uint32 y, uint32 z){});
    }

expected:1

    inline void Foo(([[last]] is_last) -> void Do)
    {
    }

    inline void Bar([[last]] bool is_last)
    {
    }

    inline void main()
    {
        Foo(Bar);
    }    

expected:1
