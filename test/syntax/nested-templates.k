// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    template <typename T>
    class A
    {
        static assert(bitsizeof T == 5);

        template <typename T>
        class B
        {
            static assert(bitsizeof T == 3);
        }

        B<uint3> b;
    }

    inline void main()
    {
        static A<int5> a;
    }

expected:0

    template <typename T, auto N>
    class A
    {
        static assert(bitsizeof T == N);

        template <auto N, typename T>
        class B
        {
            static assert(bitsizeof T == N);
        }

        B<3, uint3> b;
    }

    inline void main()
    {
        static A<int5, 5> a;
    }

expected:0

    template <typename T, auto N>
    class A
    {
        template <typename S>
        class B
        {
            static assert(N == bitsizeof S);
        }

        B<uint<N>> b;
    }

    inline void main()
    {
        static A<int5, 10> a;
    }

expected:0

    template <typename T, typename S>
    class A
    {
        template <typename U>
        class B
        {
            static assert(bitsizeof U == bitsizeof S);
        }

        B<S> b;
    }

    inline void main()
    {
        static A<int5, uint13> a;
    }

expected:0

    template <typename T1, typename T2, typename T3>
    class A
    {
        template <typename S1, typename S2>
        class B
        {
            static assert(bitsizeof S1 == bitsizeof T1);
            static assert(bitsizeof S2 == bitsizeof T2);

            template <typename U>
            class C
            {
                static assert(bitsizeof U == bitsizeof T1);
                static assert(bitsizeof U == bitsizeof S1);
                static assert(bitsizeof T3 == 1);
            }

            C<S1> c;
        }

        B<T1, T2> b;
    }

    inline void main()
    {
        static A<int5, uint13, bool> a;
    }

expected:0

    inline void main()
    {
        static Foo<uint3> a;

        static assert(bitsizeof a.b == 10);
    }

    inline void main2()
    {
        static Foo<uint6>::Bar<uint9> b;

        static assert(bitsizeof b == 15); 
    } 

expected:0

    inline Foo<int4>::Bar<uint3> Go()
    {
        static Foo<uint4>::Bar<uint3> x;
        return x;
    }

expected:1

    inline Foo<uint4>::Bar<uint3> Go()
    {
        static Foo<uint4>::Bar<uint3> x;
        return x;
    }

expected:0

    inline void main()
    {
        static Foo<bool>::S<4> foo;
    }

expected:1

    inline void main()
    {
        static Foo<bool>::S<false> bar;
    }

expected:1

    inline void main()
    {
        static Foo<uint32>::S<4> foo;
    }

expected:0

    inline void main()
    {
        static Bar<uint32>::S<5> x;
    }

expected:0    

    inline void main()
    {
        static Bar<bool>::S<5> x;
    }

expected:1

    template <typename T>
    class C
    {
    public:
        template <typename T>
        using X = T;

        template <typename S>
        using Y = T;
    }

    inline void main()
    {
        static C<uint32>::X<bool> x = false;
        static C<bool>::Y<uint32> y = false;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        template <typename T>
        using X = T;
    }

    inline void main()
    {
        static C<bool>::X<uint32> x = false;
    }

expected:1

    template <typename T>
    class C
    {
    public:
        struct Foo
        {
            uint32 x;
        }

        Bar<Foo> bar;
    }

    inline void main()
    {
        static C<bool> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        Bar<T> bar;
    }

    inline void main()
    {
        static C<bool> c;
        c.bar.Do();
        c.bar.x = c.bar.Go<bool>(true);

    }

expected:0

    template <typename T>
    class C
    {
    public:
        Bar<T> bar;
    }

    template <typename T>
    class D
    {
    public:
        C<T> c;
    }

    inline void main()
    {
        static D<bool> d;
        static D<uint32> d32;
        d.c.bar.Do();
        d.c.bar.x = d.c.bar.Go<bool>(true);
        d32.c.bar.x = d32.c.bar.Go<uint32>(10);
    }

expected:0

    template <typename T>
    class C
    {
        inline void Go()
        {
            S1<5> s;
            s.x[4] = 0;
        }

        template <auto N>
        struct S1
        {
            T[N] x;
        }
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:0
