// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        static for (const auto i : 2)
        {
            uint32[i+1] a;
        }
    }

expected:0

    inline void main()
    {
        static for (const auto i : 2)
        {
            uint32[3][i+1][2] a;
        }
    }

expected:0

    inline void main()
    {
        static for (const auto i : 2)
        {
            uint<i+1> a;
        }
    }

expected:0

    inline void main()
    {
        const auto a = 10;
        const auto b = a + 4;
        const auto c = b * a;
        const auto d = clog2(c + a + b);

        static for (const auto i : 2)
        {
            uint<a + b * (c * (d - i))> x;
        }
    }

expected:0

    inline void main()
    {
        static for (const auto i : 2)
        {
            int<i+1> a;
        }
    }

expected:0

    import type.stdtype

    inline void main()
    {
        static for (const auto i : 2)
        {
            index_t<i> a;
        }
    }

expected:0

    inline void main()
    {
        static for (const auto i : 2)
        {
            static if (i == 1)
            {
            }
        }
    }

expected:0

    inline void main()
    {
        static for (const auto i : 2)
        {
            static for (const auto j : i)
            {
                int<i + 1> a;
            }
        }
    }

expected:0

    inline void main()
    {
        static for (const auto i : 2)
        {
            static for (const auto j : i)
            {
                int<j + 1> a;
            }
        }
    }

expected:0

    inline void main()
    {
        static for (const auto i : 2)
        {
            static for (const auto j : 2)
            {
                int<j + 1> a;
            }
        }
    }

expected:0

    inline void main()
    {
        static for (const auto i : 2)
        {
            static for (const auto j : i)
            {
                const auto a = j + 1;

                uint32[a] b;
            }
        }
    }

expected:1

    template <typename T>
    inline T Go((T) -> T Do, T x)
    {
        return Do(x);
    }

    inline void main()
    {
        static for (const auto i : 2)
        {
            const auto a = Go([](uint<i+2> x)
                    {
                        return x;
                    },
                    0);

            static assert(bitsizeof(a) == i +2);
        }
    }

expected:0

    template <typename T>
    inline T Go((T) -> T Do, T x)
    {
        return Do(x);
    }

    inline void main()
    {
        static for (const auto i : 2)
        {
            const auto a = Go([i](uint<2> x)
                    {
                        return x;
                    },
                    0);
        }
    }

expected:0

    template <typename T>
    inline void Go((T) -> void fn)
    {
    }

    class Foo
    {
    public:
        inline void Run()
        {
            static for(const auto i : 2)
                static for(const auto j : 2)
                {
                    struct Bar
                    {
                        uint<j + i + 1> x;
                    }

                    Go<Bar>([](Bar bar){});
                }
        }
    }

    inline void main()
    {
        static Foo foo;
        foo.Run();
    }

expected:0

    inline void Go((uint32) -> uint32 Do)
    {
        static for (const auto i : 2)
        {
            Do(i);

            static for (const auto j : i)
            {
                Do(j + i);
            }
        }
    }

    template <auto N>
    class Foo
    {
    public:
        inline void Run()
        {
            static for (const auto i : 1)
            {
                Go([](uint32 x) -> uint32
                {
                    return x;
                });
            }
        }
    }

    inline void main()
    {
        static Foo<2> foo;

        foo.Run();
    }

expected:0

    import type.stdtype

    inline void Go(() -> void fn)
    {
    }

    class Foo
    {
        public:
            inline void Do(index_t<10> x)
            {
                bool ctx;

                static for (const auto i : 2)
                {
                    Go([ctx](){!ctx;});
                }
            }
    }

    inline void main()
    {
        static Foo foo;

        foo.Do(0);
    }

expected:0

    import type.stdtype

    inline void Go((bool) -> void fn)
    {
        fn(false);
    }

    inline void main()
    {
        class Foo
        {
            public:
                inline void test(bool b)
                {
                }

                inline void tests()
                {
                    static for (const auto i : 2)
                    {
                        index_t<i> x;
                        Go(test);
                    }
                }
        }


        static Foo foo;

        foo.tests();
    }

expected:0

    inline void Go((bool) -> void fn)
    {
        fn(false);
    }

    inline void main()
    {
        class Foo
        {
            public:
                inline void test(bool b)
                {
                }

                inline void tests()
                {
                    static for (const auto i : 3)
                    {
                        Go(test);
                    }
                }
        }


        static Foo foo;

        foo.tests();
    }

expected:0

    inline void Go((bool) -> void fn)
    {
        fn(false);
    }

    inline void main()
    {
        class Foo
        {
            public:
                inline void test(bool b)
                {
                }

                inline void tests()
                {
                    const auto y = clog2(10 - 3);
                    const auto x = bitsizeof y;
                    static for (const auto i : 3 + x)
                    {
                        Go(test);
                    }
                }
        }


        static Foo foo;

        foo.tests();
    }

expected:0

    inline void main()
    {
        static for (const auto i : 2)
        {
            const auto fn = []()
            {
                auto x = static(i);
            };

            fn();
        }
    }

expected:0

    inline auto Do(auto x)
    {
        return x;
    }

    inline void main()
    {
        static for (const auto i : 2)
        {
            const auto fn = []()
            {
                auto x = Do(1 + static(i));
            };

            fn();
        }
    }

expected:0
