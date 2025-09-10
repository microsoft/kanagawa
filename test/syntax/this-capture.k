// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    import type.stdtype

    class Foo
    {
    public:
        inline void Run()
        {
            const auto fn = []() -> void
            {
                auto x = _x;
            };

            fn();
        }

        index_t<10> _x;
    }

    inline void main()
    {
        static Foo foo;

        foo.Run();
    }

expected:0

    import type.stdtype

    class Foo
    {
    public:
        inline void Run()
        {
            const auto fn = []() -> void
            {
                auto x = _x;
            };

            fn();
        }

        static if (true)
        {
            index_t<10> _x;
        }
    }

    inline void main()
    {
        static Foo foo;

        foo.Run();
    }

expected:0

    import type.stdtype

    inline void main()
    {
        class Foo
        {
        public:
            inline void Run()
            {
                const auto fn = []() -> void
                {
                    auto x = _x;
                };

                fn();
            }

            index_t<10> _x;
        }

        static Foo foo;

        foo.Run();
    }

expected:0

    import type.stdtype

    inline void main()
    {
        class Foo
        {
        public:
            inline void Run()
            {
                const auto fn = []() -> void
                {
                    auto x = _x;
                };

                fn();
            }

            static if (true)
            {
                index_t<10> _x;
            }
        }
    }

expected:0

    inline void Go(() -> void Do)
    {
        Do();
    }

    class Foo
    {
        public:
            inline void Fn(() -> void Do)
            {
                Do();
            }

        bool x;
    }

    template <auto N>
    class Bar
    {
        Foo foo;

        public:
            inline void Run()
            {
                Go([]()
                {
                    foo.Fn([]()
                    {
                        foo.x;
                    });
                });
            }

    }

    inline void main()
    {
        static Bar<10> bar;
    }

expected:0

    inline void Go(() -> void Do)
    {
        Do();
    }

    class Foo
    {
        public:
            inline void Fn(() -> void Do)
            {
                Do();
            }

        bool x;
    }

    inline void main()
    {
        class Bar
        {
            Foo foo;

            public:
                inline void Run()
                {
                    Go([]()
                    {
                        foo.Fn([]()
                        {
                            foo.x;
                        });
                    });
                }

        }

        static Bar bar;
    }

expected:0

    inline void Go(() -> uint32 Do)
    {
    }

    class Foo
    {
    public:
        const uint32 bar = 10;

        inline void Run()
        {
            Go([](){return bar;});
        }
    }

    inline void main()
    {
        static Foo foo;

        foo.Run();
    }

expected:0

    inline void Go(() -> uint32 Do)
    {
    }

    class Foo
    {
    public:
        static if (true)
        {
            const uint32 bar = 10;
        }

        inline void Run()
        {
            Go([](){return bar;});
        }
    }

    inline void main()
    {
        static Foo foo;

        foo.Run();
    }

expected:0

    inline void Go(() -> uint32 Do)
    {
    }

    template <auto N>
    inline uint32 Do()
    {
        return N;
    }

    class Foo
    {
    public:
        const uint32 bar = 10;

        inline void Run()
        {
            Go([](){return Do<bar>();});
        }
    }

    inline void main()
    {
        static Foo foo;

        foo.Run();
    }

expected:0

    inline void Foo(() -> void fn)
    {
    }

    class C
    {
    public:
        inline void f()
        {
        }

        inline void Run()
        {
            Foo([]()
            {
                Foo(f);
            });
        }

    }

    inline void main()
    {
        static C x;

        x.Run();
    }

expected:0

    class C
    {
    private:
        uint32[8] a = {0, 1, 2, 3, 4, 5, 6, 7};
    public:
        inline void Run()
        {
            const auto fn = ([]()
            {
                auto data = a[0];
            });

            fn();
        }
    }

    inline void main()
    {
        static C x;

        x.Run();
    }

expected:0

    class C
    {
    private:
        static if (true)
        {
            uint32[8] a = {0, 1, 2, 3, 4, 5, 6, 7};
        }
    public:
        inline void Run()
        {
            const auto fn = ([]()
            {
                auto data = a[0];
            });

            fn();
        }
    }

    inline void main()
    {
        static C x;

        x.Run();
    }

expected:0

    class C
    {
    private:
        const uint32[8] a = {0, 1, 2, 3, 4, 5, 6, 7};
    public:
        inline void Run()
        {
            const auto fn = ([]()
            {
                auto data = a[0];
            });

            fn();
        }
    }

    inline void main()
    {
        static C x;

        x.Run();
    }

expected:0

    class C
    {
    private:
        static if (true)
        {
            const uint32[8] a = {0, 1, 2, 3, 4, 5, 6, 7};
        }
    public:
        inline void Run()
        {
            const auto fn = ([]()
            {
                auto data = a[0];
            });

            fn();
        }
    }

    inline void main()
    {
        static C x;

        x.Run();
    }

expected:0

    class Foo
    {
    public:
        inline void Run()
        {
            const auto fn = []() -> void
            {
                auto x = Do();
            };

            fn();
        }

        static if (true)
        {
            uint32 Do()
            {
                return 10;
            }
        }
    }

expected:0

    class Foo
    {
    public:
        inline void Run()
        {
            const auto fn = []() -> void
            {
                auto x = template Do<10>();
            };

            fn();
        }

        static if (true)
        {
            template <auto N>
            uint32 Do()
            {
                return N;
            }
        }
    }

    inline void main()
    {
        static Foo foo;

        foo.Run();
    }

expected:0

    inline uint32 Do()
    {
        return 10;
    }

    class Foo
    {
    public:
        inline void Run()
        {
            const auto fn = []() -> void
            {
                auto x = callback();
            };

            fn();
        }

        static if (true)
        {
            () -> uint32 callback = Do;
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

    class Foo
    {
    public:
        inline void Run()
        {
            Go([]()
            {
                Do();
            });
        }

    private:
        static if (true)
        {
            inline void Do()
            {
            }
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
    }

    template <typename T>
    class Foo
    {
        () -> void callback;

    public:
        inline void Run()
        {
            Go([]()
            {
                Go(callback);
            });
        }
    }

    inline void main()
    {
        static Foo<uint32> foo;

        foo.Run();
    }

expected:0
