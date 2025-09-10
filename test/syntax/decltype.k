// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void Run(uint32 x)
    {
        for (const decltype(x) i : x)
        {
        }
    }

expected:0

    inline void Run(uint32 x)
    {
        decltype(++x) i;
    }

expected:1

    inline void main()
    {
        auto x = 10;

        decltype(bitsizeof(decltype(x))) y;

        static assert(bitsizeof x == 4);
        static assert(bitsizeof y == 3);
    }

expected:0

    inline void Run(uint4 x, uint3 y)
    {
        decltype(x + y)[10] a;
        
        a[0] = x + y;
    }

expected:0    

    import data.array

    inline void Run(uint4 x, uint3 y)
    {
        array<decltype(x + y), 10> a;
        
        a[0] = x + y;
    }

expected:0    

    import data.array

    inline void Run(uint4 x, uint3 y)
    {
        array<decltype(x + y), 10> a;
        
        decltype(a[0]) b;
        
        b = x + y;
    }

expected:0    

    const auto g = 10;

    template <(decltype(g)) -> decltype(g) Do>
    inline auto Go(decltype(g) x)
    {
        return Do(x);
    }

    inline auto Run()
    {
        inline uint4 Fn4(uint4 x)
        {
            return x;
        }

        return Go<Fn4>(g);
    }

expected:0

    inline void Run(bool f)
    {
        decltype(f) g;
        
        g = f;
    }

expected:0

    inline void Run(bool f)
    {
        const decltype(f) g = f;
    }

expected:0

    inline void Run(bool f)
    {
        const decltype(f) g = f;

        g = f;
    }

expected:1

    import data.array
    import hardware.dsp

    inline void Run(uint5 x)
    {
        using x_t = decltype(x);
        using xx_t = decltype(x * x);

        array<x_t, 10> a;

        inline xx_t Fn(x_t i)
        {
            return i * i;
        }

        a[0] = Fn(x);
    }

expected:0

    inline void main()
    {
        decltype(10) x;
    }

expected:0

    inline void main()
    {
        decltype(10 + 1) x;
    }

expected:0

    const uint32 y = 0;

    inline void main()
    {
        decltype(y) x;
    }

expected:54

    const uint32 y = 0;

    inline void main()
    {
        decltype(y + 3) x;
    }

expected:0

    const uint32 y = 0;

    inline void main()
    {
        decltype(y) x = 0;
    }

expected:0
          
    template <auto N>
    inline void foo()
    {
        decltype(N) x;

        x = 0;
    }

    inline void main()
    {
        foo<10>();
    }

expected:0

    template <typename R>
    inline auto closure(() -> R fn)
    {
        decltype(fn) x = fn;
        return x;
    }

    template <typename T>
    inline void Foo()
    {
        auto x = closure<T>([]() -> T
        {
            return 10;
        });
    }

    inline void main()
    {
        Foo<uint32>();
    }

expected:0

    inline uint32 Do()
    {
        return 10;
    }

    inline void main()
    {
        decltype(Do()) x;
    }

expected:0

    inline auto Do(auto x)
    {
        return 2 * x;
    }

    inline void Go(decltype(Do(10)) x)
    {
    }

    inline void main()
    {
        Go(20);
    }

expected:0

    inline auto Do(auto x)
    {
        return 2 * x;
    }

    template <decltype(Do(10)) X>
    inline void Go()
    {
    }

    inline void main()
    {
        Go<20>();
    }  

expected:0
