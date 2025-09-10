// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        static C<true> c;
    }

expected:0

    inline void main()
    {
        static C<false> c;
    }

expected:1

    template <auto Cond>
    class C
    {
        inline void Run()
        {
            d.Go<uint32>();
        }

        static if (Cond)
        {
            B d;
        }
    }

expected:1

    inline void main()
    {
        static D<B> d;
    }

expected:0

    template <typename T>
    class D
    {
    public:
        inline void Go()
        {
            obj.Go<uint32>();
        }

        T obj;
    }

expected:1

    class Foo
    {
        inline void Go()
        {
        }
    }

    inline void main()
    {
        static D<Foo> d;
    }

expected:1

    static if (true)
    {
        template <typename T>
        inline void Go()
        {
            T x;
        }
    }

    inline void main()
    {
        template Go<bool>();
    }

expected:0

    static if (true)
    {
        template <typename T>
        inline void Go()
        {
            T x;
        }
    }

    inline void main()
    {
        Go<bool>();
    }

expected:1

    static if (false)
    {
        template <typename T>
        inline void Go()
        {
            T x;
        }
    }

    inline void main()
    {
        template Go<bool>();
    }

expected:1

    template <auto Cond>
    class Foo
    {
    public:
        () -> uint32 callback = obj.template Go<uint32>;

    private:
        static if (Cond)
        {
            B obj;
        }
    }

    inline void main()
    {
        static Foo<true> foo;
    }

expected:0


    inline void main()
    {
        static if (true)
        {
            static B obj;
        }
        static E e = {.callback = obj.template Go<uint32>};
    }

expected:0

    inline void main()
    {
        static if (true)
        {
            static B obj;
        }
        static E e = {.callback = obj.Go<uint32>};
    }

expected:1

    inline void main()
    {
        static if (true)
        {
            static B obj;
        }
        static E e = {.callback = obj.template Go<uint33>};
    }

expected:1

    static if (true)
    {
        template <typename T>
        inline T Go()
        {
            return {};
        }
    }

    inline void main()
    {
        static E e = {.callback = Go<uint32>};
    }

expected:1

    static if (true)
    {
        template <typename T>
        inline T Go()
        {
            return {};
        }
    }

    inline void main()
    {
        static E e = {.callback = template Go<uint32>};
    }

expected:0

    static if (true)
    {
        template <typename T>
        inline T Go()
        {
            return {};
        }
    }

    inline void main()
    {
        static E e = {.callback = template Go<uint33>};
    }

expected:1

    inline uint32 Run1()
    {
        static AmbiguosSyntax<uint32, true, false> obj1;
        return obj1.Go();
    }

    inline bool Run2()
    {
        static AmbiguosSyntax<uint3, 13, 14> obj2;
        return obj2.Go();
    }

expected:0
