// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline bool Foo()
    {
        static if (true)
            return true;
        else
            return false;
    }

expected:0

    inline bool Run()
    {
        return auto_Fn<true>();
    }

expected:0

    inline uint4 Run()
    {
        return auto_Fn<false>();
    }

expected:0

    inline bool Run()
    {
        return auto_Fn<false>();
    }

expected:1

    inline uint4 Run()
    {
        return auto_Fn<false>();
    }

expected:0

    template <auto Cond>
    inline auto auto_Fn()
    {
        static if (Cond)
            return auto_Fn();
        else
            return 0;
    }

expected:1

    template <bool Test>
    class Testable
    {
        static if (Test) Testable x;
    }

expected:1

    inline auto Run()
    {
        return factorial<4>();
    }

expected:0

    inline uint29 Run()
    {
        return factorial<10>();
    }

expected:0

    inline auto Run()
    {
        uint32[8] a = {0, 9, 1, 5, 2, 2, 3, 7};

        return reduce(a, [](uint32 x, uint32 y) { return x > y ? x : y; });
    }

expected:0

    inline auto Run()
    {
        {
            float32 x;
        }

        static if (true)
        {
            uint32 x;

            inline auto foo()
            {
                return 10;
            }

            x = foo();
        }
        else
        {
            bool x;

            x = true;
        }

        return x;
    }

expected:0

    inline bool Run()
    {
        static if (true)
            static if (false)
            {
            }
            else
            {
                return true;
            }
    }

expected:0

    inline void main()
    {
        static if (true)
            static if (true)
            {
            }
            else
            {
                return true;
            }
    }

expected:0

    inline void Foo(uint32 i)
    {
    }

    inline void main()
    {
        unroll_for<4>([](uint5 i)
        {
            Foo(i);
        });
    }

expected:0

    inline void main()
    {
        static if (true)
        {
            struct Foo
            {
                uint2 x;
            }

            Foo foo1;
        }
        else
        {
            union Foo
            {
                uint2 x;
                bool y;
            }

            Foo foo1;
        }
    }

expected:0

    inline void main()
    {
        static if (true)
        {
            template <typename T>
            struct Foo
            {
                T x;
            }

            Foo<bool> foo1;
        }
    }

expected:0

    inline void Foo()
    {
    }

    inline void main()
    {
        static if (true)
        {
            atomic
            {
                Foo();
            }
        }
        else
        {
            Foo();
        }
    }

expected:0

    inline void Run(uint32 x)
    {
        if (x > 10)
        {
            static if (true)
            {
                if (x > 20)
                {
                    x++;
                }
            }
            else
            {
                Foo();
            }
        }
        else
        {
        }
    }

expected:0

    inline uint32 Run()
    {
        uint32 x;

        static if (true)
        {
            uint32 x;
        }

        return x;
    }

expected:33

    inline uint32 Run()
    {
        uint32 x;

        static if (true)
        {
            {
                uint32 x;
            }
        }

        return x;
    }

expected:0

    inline void run()
    {
        static if (true)
        {
            static uint32 x;
        }

        x++;
    }

expected:0

    template <auto N>
    inline void Foo()
    {
        static if (N > 1)
            uint<clog2(N)> x;
    }

    inline void main()
    {
        Foo<1>();
        Foo<2>();
    }

expected:0

    inline void run()
    {
        {
            struct Foo
            {
            }

            Foo foo;
        }

        static if (true)
        {
            {
                struct Foo
                {
                    bool x;
                }

                Foo foo = {true};
            }

            {
                struct Foo
                {
                    int32 x;
                }

                Foo foo = {10};
            }
        }
        else
        {
            {
                struct Foo
                {
                    bool x;
                    bool y;
                }

                Foo foo = {true, false};
            }

            {
                struct Foo
                {
                    int32 x;
                    int32 y;
                }

                Foo foo = {10, 11};
            }
        }
    }

expected:0

    inline void main()
    {
        bool Foo = true;
    }

expected:0

    inline void Foo(bool Foo)
    {
        bool x = Foo;
    }

expected:0

    class Bar
    {
        inline void Foo(bool Foo)
        {
            bool x = Foo;
        }
    }

expected:0

    template <typename T>
    inline void Foo(const T x)
    {
        static if (x != 0)
        {
            Foo(x - 1);
        }
    }


    inline void main()
    {
        Foo(10);
    }

expected:1

    inline void main()
    {
        static if (True)
        {
            uint32 x;
        }
        else
        {
            bool x;
        }

        x = 10;
    }

expected:0

    inline void main()
    {
        static if (False)
        {
            uint32 x;
        }
        else
        {
            bool x;
        }

        x = true;
    }

expected:0

    inline void main()
    {
        static if (False)
        {
            uint32 x;
        }
        else
        {
            bool x;
        }

        x = 10;
    }

expected:14

    static if (Ten == 10)
    {
        const uint32 x = 0;

        inline void Do(uint32 y)
        {
            y = x;
        }
    }

    inline void main()
    {
        Do(10);
    }

expected:0

    static if (Ten != 10)
    {
        const uint32 x = 0;

        inline void Do(uint32 y)
        {
            y = x;
        }
    }

    inline void main()
    {
        Do(10);
    }

expected:1

    static if (Ten == 10 && True)
    class C
    {
    public:
        uint32 x;
    }

    inline void main()
    {
        static C c;

        c.x = 10;
    }

expected:0

    static if (Ten == 10 && False)
    class C
    {
    public:
        uint32 x;
    }

    inline void main()
    {
        static C c;

        c.x = 10;
    }

expected:1

    static if (Ten == 10 && True)
    template <auto N>
    class C
    {
    public:
        static if (N > 0)
        {
            uint32[N] x;
        }

        static if (N == 0) bool foo;
    }

    inline void main()
    {
        static C<0> c0;
        static C<2> c2;

        c2.x[0] = 10;

        c0.foo = true;
    }

expected:0

    inline void main()
    {
        static Testable<true> t;

        t.foo = true;
    }

expected:0

    inline void main()
    {
        static Testable<false> t;

        t.foo = true;
    }

expected:29

    static if (!False && True)
    template <typename T>
    inline void Do(T x)
    {
    }

    inline void main()
    {
        Do(10);
        Do(false);
    }

expected:0

    static if (Ten > 9)
    {
        const uint32 x = 0;
    }

expected:0

    static if (Ten)
    {
        const uint32 x = 0;
    }

expected:1

    template <auto N>
    class Worker
    {
        public:
           inline void Do()
           {
           }
    }

    inline void main()
    {
        static Tree<6, Worker> workers;

        workers.Do();
    }

expected:0

    inline void main()
    {
        static Method<true> obj;
        obj.Do();
    }

expected:0

    inline void main()
    {
        static Method<false> obj;
        obj.Do();
    }

expected:1

    inline void main()
    {
        static Storage<uint32, 8> s8;
        static Storage<bool, 16> s16;
        static Storage<bool, 17> s17;
    }

expected:0

    inline void main()
    {
        static if (true)
        {
            Go([](uint32 x) -> uint32
            {
                return 2 * x;
            },
            10);
        }
        else
        {
            Go([](uint16 x) -> uint16
            {
                return x - 10;
            },
            11);
        }
    }

expected:0

    inline void main()
    {
        static if (true)
            static if (true)
            {
                Go([](uint32 x) -> uint32
                {
                    return 2 * x;
                },
                10);
            }
            else static if (Ten == 10)
            {
                Go([](uint16 x) -> uint16
                {
                    return x - 10;
                },
                10);
            }
    }

expected:0

    template <typename T>
    class Foo
    {
    public:
        inline auto Run()
        {
            static if (bitsizeof T > 7)
            {
                return Go([](uint32 x) -> uint32
                {
                    return x;
                },
                19);
            }
            else
            {
                return true;
            }
        }
    }

    inline void main()
    {
        static Foo<uint3> foo3;
        static Foo<uint8> foo8;

        bool b = foo3.Run();
        uint32 x = foo8.Run();
    }

expected:0

    inline void bar(() -> void fn)
    {
    }

    inline void foo(() -> void fn)
    {
        bar([fn]()
        {
            fn();
        });
    }


    inline void Run(uint32 hid)
    {
        static if(true)
        {
            foo([](){});
        }
    }

expected:0

    template <auto N>
    class Bar
    {
    public:
        template <typename T>
        T Do(T x)
        {
            static assert(bitsizeof T == N);
            return x;
        }
    }

    class Mock
    {
    public:
        uint32 Do(uint32 x)
        {
            return x;
        }
    }

    template <typename T>
    class Foo
    {
    public:
        static if (bitsizeof(T) == 32)
        {
            Mock x;
        }
        else static if (bitsizeof(T) == 16)
        {
            Bar<16> x;
        }
        else
        {
            Bar<1> x;
        }

        inline auto Go()
        {
            T y;
            return x.Do(y);
        }
    }

    inline void main()
    {
        static Foo<uint32> foo32;
        static Foo<uint16> foo16;
        static Foo<bool> foo_bool;
        uint32 x = foo32.Go();
        uint16 y = foo16.Go();
        bool z = foo_bool.Go();
    }

expected:0

    const auto a = false;

    static if (a)
    {
        const uint32 y = 10;
    }


    inline void Run(uint32 x)
    {
        switch(x)
        {
            case 0: x+=1; break;
            case 2: x+=1; break;
        static if (a)
        {
            case 3: x+=y; break;
        }
            case 5: x+=1; break;
        }
    }

expected:0

    const auto a = true;

    static if (a)
    {
        const uint32 y = 10;
    }


    inline void Run(uint32 x)
    {
        switch(x)
        {
            case 0: x+=1; break;
            case 2: x+=1; break;
        static if (a)
        {
            case 3: x+=y; break;
        }
            case 5: x+=1; break;
        }
    }

expected:0

    const auto a = false;

    static if (a)
    {
        const uint32 y = 10;
    }


    inline void Run(uint32 x)
    {
        switch(x)
        {
            case 0: x+=1; break;
            case 2: x+=1; break;
        static if (a)
        {
            default: x+=y; break;
        }
        }
    }

expected:0

    const auto a = true;

    static if (a)
    {
        const uint32 y = 10;
    }


    inline void Run(uint32 x)
    {
        switch(x)
        {
            case 0: x+=1; break;
            case 2: x+=1; break;
        static if (a)
        {
            default: x+=y; break;
        }
        }
    }

expected:0

    const auto a = true;

    inline void Run(uint32 x)
    {
        switch(x)
        {
            case 0: x+=1; break;
        static if (a)
        {
            default: x+=1; break;
        }
        }
    }

expected:0

    const auto a = false;

    inline void Run(uint32 x)
    {
        switch(x)
        {
        static if (a)
        {
            default: x+=1; break;
        }
        }
    }

expected:0

    const auto a = true;
    const auto b = false;

    inline void Run(uint32 x)
    {
        switch(x)
        {
            case 0: x+=1; break;
        static if (a)
        {
            default: x+=1; break;
        }
        static if (b)
        {
            default: x+=1; break;
        }
        }
    }

expected:0

    const auto a = false;
    const auto b = true;

    inline void Run(uint32 x)
    {
        switch(x)
        {
            case 0: x+=1; break;
        static if (a)
        {
            default: x+=1; break;
        }
        static if (b)
        {
            default: x+=1; break;
        }
        }
    }

expected:0

    const auto a = true;
    const auto b = true;

    inline void Run(uint32 x)
    {
        switch(x)
        {
            case 0: x+=1; break;
        static if (a)
        {
            default: x+=1; break;
        }
        static if (b)
        {
            default: x+=1; break;
        }
        }
    }

expected:13

    const auto a = true;
    const auto b = true;

    inline void Run(uint32 x)
    {
        switch(x)
        {
        static if (a)
        {
            case 0: x+=1; break;
        }
        static if (b)
        {
            case 0: x+=1; break;
        }
        }
    }

expected:13

    const auto a = true;
    const auto b = false;

    inline void Run(uint32 x)
    {
        switch(x)
        {
        static if (a)
        {
            case 0: x+=1; break;
        }
        static if (b)
        {
            case 0: x+=1; break;
        }
        }
    }

expected:0

    template <auto N>
    class Foo
    {
        static if (N > 10)
        {
            uint32 x;
        }

        inline void Run()
        {
            const auto fn = [x](){return x + 1;};

            fn();
        }
    }

    inline void main()
    {
        static Foo<20> foo;
    }

expected:0

    template <auto N>
    class Foo
    {
        static if (N > 10)
        {
            uint32 x;
        }

        inline void Run()
        {
            const auto fn = [x](){return x + 1;};

            fn();
        }
    }

    inline void main()
    {
        static Foo<2> foo;
    }

expected:1

    template <auto N>
    class Foo{}

    class C
    {
        static if(true)
        {
            const auto Y = 5;
        }

        static if(true)
        {
            Foo<Y> g;
        }
    }

    inline void main()
    {
        static C c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        inline void Run()
        {
            {
                static if (True)
                {
                    Exec([](){});
                }
            }
        }
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        inline void Run()
        {
            do
            {
                static if (True)
                {
                    Exec([](){});
                }
            }
            while(false);
        }
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        inline void Run()
        {
            for (const auto i : 10)
            {
                static if (True)
                {
                    Exec([](){});
                }
            }
        }
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        inline void Run()
        {
            if (True)
            {
                static if (True)
                {
                    Exec([](){});
                }
            }
            else
            {
                static if (True)
                {
                    Exec([](){});
                }
            }
        }
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        inline void Run()
        {
            if (True)
            {
            }
            else
            {
                static if (True)
                {
                    Exec([](){});
                }
            }
        }
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        inline void Run()
        {
            if (True)
            {
                static if (True)
                {
                    Exec([](){});
                }
            }
            else
            {
            }
        }
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        inline void Run()
        {
            if (True)
            {
                static if (True)
                {
                    Exec([](){});
                }
            }
        }
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        inline void Run()
        {
            for (const auto i : 10)
            {
                do
                {
                    if (True)
                    {
                        static if (True)
                        {
                            Exec([](){});
                        }
                    }
                    else
                    {
                        static if (True)
                        {
                            Exec([](){});
                        }
                    }
                }
                while(false);
            }
        }
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:0 

    const bool Cond = true;

    static if (Cond)
    {
        using T = uint32;
    }
    else
    {
        struct T
        {
            bool x;
        }
    }

    const T x = 10;

expected:0

    const bool Cond = false;

    static if (Cond)
    {
        using T = uint32;
    }
    else
    {
        struct T
        {
            bool x;
        }
    }

    const T x = 10;

expected:1

    const bool Cond = false;

    static if (Cond)
    {
        using T = uint32;
    }
    else
    {
        struct T
        {
            bool x;
        }
    }

    const T x = {false};

expected:0
