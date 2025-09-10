// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    
    inline uint32 Do(uint32 x)
    {
        static C1 c;

        return c.Foo<uint32>(x) + c.Bar<uint32, 12>();
    }

expected:0

    inline uint32 Do(uint32 x)
    {
        static C1[4][4] c;
        
        return c[2][2].Foo<uint32>(x) + c[0][1].Bar<uint32, 12>();
    }

expected:0

    inline uint32 Do(uint32 x)
    {
        static C1 c;
        
        return c.Foo<bool>(x);
    }

expected:1

    inline uint32 Do(uint32 x)
    {
        static C1 c;
        
        return c.Bar<bool, false>();
    }

expected:1

    inline uint32 Do(uint32 x)
    {
        static C1 c;
        
        return c.Bar<bool>();
    }

expected:1

    inline uint8 Do(uint32 x)
    {
        static C1 c;
        
        return c.Bar<uint7, 2>();
    }

expected:0

    inline uint16 Do(uint16 x)
    {
        static T1<uint16> c;
        
        return c.Foo<uint16>(x) + c.Bar<uint16, 12>(9);
    }

expected:0

    inline uint16 Do(uint16 x)
    {
        static T1<uint16> c;
        
        return c.Bar<bool, 12>(true);
    }

expected:0

    inline uint16 Do(uint16 x)
    {
        static T1<bool> c;
        
        return c.Foo<uint16>(x);
    }

expected:1

    inline uint16 Do(uint16 x)
    {
        static T1<bool> c;
        
        return c.Bar<uint16, 6>(true);
    }

expected:1

    inline uint16 Do(uint16 x)
    {
        static T1<bool> c;
        
        return c.Foo<bool>(true) ? 1 : 2;
    }

expected:0

    inline uint32 Go(uint32 x)
    {
        return 2 * x;
    }

    inline uint32 Do(uint32 x)
    {
        static T2<uint32> c;
        
        return c.Bar<uint32, Go>(x);
    }

expected:0

    inline uint32 Go(uint32 x)
    {
        return 2 * x;
    }

    inline uint32 Do(uint32 x)
    {
        static T2<uint32> c;
        
        return c.Bar<bool, Go>(false);
    }

expected:1

    inline uint32 Go(bool x)
    {
        return x ? 1 : 2;
    }

    inline uint32 Do(uint32 x)
    {
        static T2<uint32> c;
        
        return c.Bar<bool, Go>(x);
    }

expected:1

    inline uint32 Go(bool x)
    {
        return x ? 1 : 2;
    }

    inline uint32 Do(uint32 x)
    {
        static T2<uint32> c;
        
        return c.Bar<bool, Go>(false);
    }

expected:0

    inline uint32 Go(uint32 x)
    {
        return 2 * x;
    }

    inline uint32 Do(uint32 x)
    {
        static T2<uint32> c;
        
        return c.Foo<Go>(x);
    }

expected:0

    template <typename U>
    inline U Go(uint32 x)
    {
        return 2 * x;
    }

    inline uint32 Do(uint32 x)
    {
        static C1 c;
        
        return c.Buz<uint32, Go<uint32>>(x);
    }

expected:0

    template <typename U>
    inline U Go(uint32 x)
    {
        return 2 * x;
    }

    inline uint32 Do(uint32 x)
    {
        static C1 c;
        
        return c.Buz<uint16, Go<uint32>>(x);
    }

expected:1

    inline uint32 Do(uint32 x)
    {
        static C2 c;
        
        return c.c1.Foo<uint32>(x);
    }

expected:0

    inline uint32 Do(uint32 x)
    {
        static C2 c;
        
        return c.c1s[0].Foo<uint32>(x);
    }

expected:0

    inline uint32 Do(uint32 x)
    {
        static C2 c;
        
        return c.c1s.Foo<uint32>(x);
    }

expected:1

    inline uint32 Do(uint32 x)
    {
        static C2[4] c2s;
        
        return c2s[1].c1s[0].Foo<uint32>(x);
    }

expected:0

    inline uint32 Do(uint32 x)
    {
        static C2[4] c2s;
        
        return c2s[1].c[0].Foo<uint32>(x);
    }

expected:1

    template <auto N>
    class C
    {
    public:
        T1<uint16>[N] c;
    }

    inline uint16 Do(uint16 x)
    {
        static C<4>[4] c;
        return c[3].c[3].Bar<bool, 12>(true);
    }

expected:0

    inline uint4 Go(bool x, uint32 y)
    {
        return x ? 2 * y : y;
    }

    inline uint4 Do(bool x, uint32 y)
    {
        static T3<uint4>::T4<bool> c;
        return c.Bar<uint32, Go>(x, y);
    }

expected:0

    inline uint4 Go(bool x, uint32 y)
    {
        return x ? 2 * y : y;
    }

    inline uint4 Do(bool x, uint32 y)
    {
        static T3<uint4>::T4<bool> c;
        return c.Bar<uint3, Go>(x, y);
    }

expected:0

    T3<uint4>::T4<bool> c;

    inline uint4 Go(bool x, uint32 y)
    {
        return x ? 2 * y : y;
    }

    inline uint4 Do(bool x, uint32 y)
    {
        return c.Bar<uint33, Go>(x, y);
    }

expected:1

    inline uint4 Go(bool x, uint32 y)
    {
        return x ? 2 * y : y;
    }

    inline uint4 Do(bool x)
    {
        static T3<uint4>::T4<bool> c;
        return c.Bar<bool, Go>(x, x);
    }

expected:1

    inline uint5 Go(bool x, uint32 y)
    {
        return x ? 2 * y : y;
    }

    inline uint4 Do(bool x, uint32 y)
    {
        static T3<uint4>::T4<bool> c;
        return c.Bar<uint32, Go>(x, y);
    }

expected:1

    inline uint4 Go(uint32 x, uint32 y)
    {
        return x ? 2 * y : y;
    }

    inline uint4 Do(bool x, uint32 y)
    {
        static T3<uint4>::T4<bool> c;
        return c.Bar<uint32, Go>(x, y);
    }

expected:1

    class Foo
    {
    public:
        inline void Go()
        {
            auto x = Give<uint32>();
        }

    private:
        template <typename T>
        T Give()
        {
            return {};
        }
    }

expected:0

    template <auto N>
    class Foo
    {
    public:
        inline void Go()
        {
            auto x = Give<uint32>();
        }

    private:
        template <typename T>
        T[N] Give()
        {
            return {};
        }
    }

    inline void main()
    {
        static Foo<10> foo;

        foo.Go();
    }

expected:0

    class Bar
    {
    public:
        inline void Go()
        {
            foo.Go();
        }

        template <auto N>
        class Foo
        {
        public:
            inline void Go()
            {
                auto x = Give<uint32>();
            }

        private:
            template <typename T>
            T[N] Give()
            {
                return {};
            }
        }

        Foo<5> foo;
    }

expected:0

    using t1_t = T1<uint32>;

    inline void main()
    {
        static t1_t t1;
        t1.Foo<uint32>(10);
    }

expected:0

    class A
    {
    public:
        inline void Go(auto x)
        {
        }
    }

    inline void main()
    {
        static A a;
        a.Go();
    }

expected:1
