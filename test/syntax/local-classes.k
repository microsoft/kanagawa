// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        class Foo
        {
        public:
            uint32 Do()
            {
                return x;
            }
            uint32 x;
        }

        static Foo foo;

        foo.x = 10;

        foo.Do();
    }

expected:0

    class Foo
    {
    public:
        bool x;
    }

    inline void main()
    {
        class Foo
        {
        public:
            uint32 x;
        }

        static Foo foo;

        foo.x = 10;
    }

expected:0

    inline void main()
    {
        template <typename T>
        class Foo
        {
        public:
            T x;
        }
    }

expected:0

    template <auto N>
    inline bool[N] Go(uint<N> x)
    {
        template <typename T>
        class Foo
        {
        public:
            T[N] x;
        }

        static Foo<bool> foo;

        return foo.x;
    }


    inline void main()
    {
        Go(10);
    }

expected:0

    inline void main()
    {
        class Foo
        {
        public:
            uint32 x;
        }

        class Foo
        {
        }

        static Foo foo;

        foo.x = 10;
    }

expected:1

    inline void main()
    {
        class Foo
        {
        public:
            uint32 x;
        }

        {
            class Foo
            {
            public:
                bool x;
            }

            static Foo foo;

            foo.x = 10;
        }
    }

expected:14

    inline void main()
    {
        class Foo
        {
        public:
            uint32 x;
        }

        {
            class Foo
            {
            public:
                bool x;
            }

            static Foo foo;

            foo.x = true;
        }
    }

expected:0

    inline void main()
    {
        inline void Bar()
        {
            class Foo
            {
            public:
                uint32 Do()
                {
                    return x;
                }
                uint32 x;
            }

            static Foo foo;

            foo.x = 10;

            foo.Do();
        }

        Bar();
    }

expected:0

    template <typename T>
    inline void Foo()
    {
        static T x;
    }

    class Bar
    {
    public:
        inline void Do()
        {
            class C
            {
            }

            Foo<C>();
        }

    };

expected:0

    inline void main()
    {
        class Bar
        {
        public:
            inline void Do()
            {
                const auto fn = [](){};

                fn();
            }
        }

        static Bar bar;

        bar.Do();
    }

expected:0

    inline void main()
    {
        class Bar
        {
        public:
            inline void Do()
            {
                const auto fn = []()
                {
                    const auto fn = []()
                    {
                    };

                    fn();
                };

                fn();
            }
        }

        static Bar bar;

        bar.Do();
    }

expected:0

    inline void main()
    {
        class Bar
        {
        public:
            struct S1{};

            inline void Do(S1 x)
            {
            }
        }

        static Bar bar;
        Bar::S1 s1;

        bar.Do(s1);
    }

expected:0

    inline void main()
    {
        class Bar
        {
        public:
            inline void Do(() -> void Fn)
            {
                Fn();
            }
        }

        static Bar bar;

        bar.Do([](){});
    }

expected:0

    inline void main()
    {
        class Bar
        {
        public:
            auto Do()
            {
                const auto fn = [](){return 10;};

                return fn();
            }
        }

        static Bar bar;

        auto x = bar.Do();
    }

expected:0

    import data.fifo

    template <typename T>
    inline auto Foo(() -> T Fn)
    {
        using Fn_t = decltype(Fn);

        class Helper
        {
        public:
            [[async]] void exec(Fn_t fn)
            {
                result.enqueue(fn());
            }

            inline auto get_result()
            {
                return result.dequeue();
            }

        private:
            FIFO<T, 32, true, true> result;
        }

        static Helper helper;

        helper.exec(Fn);

        return helper.get_result();
    }

    inline void main()
    {
        auto r = Foo<uint32>([]() -> uint32
                    {
                        return 0;
                    });
    }

expected:0

    inline void Go(() -> void Do)
    {
    }

    inline void main()
    {
        class Foo
        {
            public:
                template <typename T>
                inline void Run()
                {
                    Go([]()
                    {
                        x += 2;
                    });
                }

                uint32 x;
        }

        static Foo foo;

        foo.Run<bool>();
    }

expected:0

    inline void Go(() -> void Do)
    {
    }

    inline void Run(bool b)
    {
        if (b)
        {
            class Foo
            {
                public:
                    template <typename T>
                    inline void Run()
                    {
                        Go([]()
                        {
                            x += 2;
                        });
                    }

                    uint32 x;
            }

            static Foo foo;

            foo.Run<bool>();
        }
    }

expected:0
