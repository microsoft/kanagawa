// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    enum Foo : int8
    {
    }

expected:1

    enum Foo
    {
        F
    }

expected:1

    enum Foo : int32
    {
        Foo
    }

    static assert(Foo::Foo == 0);

expected:0

    enum Foo : int32
    {
        F = 0
    }

expected:0

    enum Foo : int32
    {
        F = 0,
        G = F
    }

expected:0

    enum Foo : int32
    {
        F = 0,
        G = 2 * F
    }

expected:0

    enum Foo : int32
    {
        G = 2 * F,
        F = 0
    }

expected:0

    enum Foo : int32
    {
        G = F + 1,
        F = G + 1
    }

expected:1

    enum Foo : int32
    {
        F = Bar::B1,
        G = F
    }

expected:0

    enum Foo : int32
    {
        F = Bar::B1 + Bar::B2,
        G = F
    }

expected:0

    enum E : uint32
    {
        E1,
        E2,
        E3 = 4,
        E4,
        E5 = E1 + E2 + E3,
        E6 = Bar::B4

    }

    static assert(E::E1 == 0);
    static assert(E::E2 == 1);
    static assert(E::E3 == 4);
    static assert(E::E4 == 5);
    static assert(E::E5 == 5);
    static assert(E::E5 == 5);
    static assert(E::E6 == 4);

expected:0

    static assert(Bar::B1 != 0);

expected:1

    static assert(Bar::Undefined != 0);

expected:1

    inline Bar Do(Bar x)
    {
        return x;
    }

expected:0

    template <Bar B>
    class B
    {
        Bar b = B;
    }

    inline void main()
    {
        static B<Bar::B1> b;
    }

expected:0

    using BarEnum = Bar;

    template <BarEnum B>
    class B
    {
        Bar b = B;
    }

    inline void main()
    {
        static B<Bar::B1> b;
    }

expected:0

    template <Bar B>
    class B
    {
        Bar b = B;
    }

    inline void main()
    {
        static B<0> b;
    }

expected:1

    template <Bar B>
    class B
    {
        Bar b = B;
    }

    enum Foo : int32
    {
        B1
    }

    inline void main()
    {
        static B<Foo::B1> b;
    }

expected:1

    const auto x = Bar::B1 * 2;

expected:0

    template <typename T>
    inline T Foo(T x)
    {
        return x;
    }

    inline void Do()
    {
        Foo<Bar>(Bar::B4);
    }

expected:0

    inline void Do()
    {
        Fn1<Bar, Fn2<Bar>>(Bar::B3);
    }

expected:0

    inline void Do()
    {
        Fn1<Bar, Fn2<int32>>(Bar::B3);
    }

expected:1

    inline Bar Fn(Bar x)
    {
        return x;
    }

    inline void Do()
    {
        Fn1<Bar, Fn>(Bar::B3);
    }

expected:0

    inline int32 Fn(int32 x)
    {
        return x;
    }

    inline void Do()
    {
        Fn1<Bar, Fn>(Bar::B3);
    }

expected:1

    using DWORD = uint32;

    enum Foo : DWORD
    {
       X
    }

    const Foo x = Foo::X;

expected:0

    using DWORD = uint<32>;

    enum Foo : DWORD
    {
       X
    }

    const Foo x = Foo::X;

expected:0

    using DWORD = bool;

    enum Foo : DWORD
    {
       X
    }

    const Foo x = Foo::X;

expected:1

    enum Foo : int8
    {
        E1 = 0,
        E2 = -128,
        E3 = 127
    }

expected:0

    enum Foo : int8
    {
        E1 = -129
    }

expected:1

    enum Foo : int8
    {
        E1 = 128
    }

expected:1

    enum Foo : uint8
    {
        E1 = 0,
        E2 = 255
    }

expected:0

    enum Foo : uint8
    {
        E1 = -1
    }

expected:1

    enum Foo : int8
    {
        E1 = 256
    }

expected:1

    enum Foo : int1
    {
        E1 = -1,
        E2
    }

expected:0

    enum Foo : uint1
    {
        E1 = 0,
        E2
    }

expected:0

    enum Foo : int1
    {
        E1 = -1,
        E2,
        E3
    }

expected:1

    enum Foo : uint1
    {
        E1 = 0,
        E2,
        E3
    }

expected:1

    enum Foo : int1
    {
        E1 = -1,
        E2,
        E3 = E2
    }

expected:0

    enum Foo : uint1
    {
        E1 = 0,
        E2 = E1 + 2
    }

expected:1

    inline void main()
    {
        bool[Bar::B4] x;

    }

expected:0

    enum Foo : int8
    {
        A = -2,
        B
    }

    inline void main()
    {
        bool[Foo::B] x;
    }

expected:1

    enum Foo : uint8
    {
        count
    }

    inline void main()
    {
        bool[Foo::count] x;
    }

expected:1

    const uint8 count = Bar::B4;

    inline void main()
    {
        bool[count] x;
    }

expected:0

    enum Foo : int8
    {
        A = -2,
        B
    }

    const int8 count = Foo::B;

    inline void main()
    {
        bool[count] x;
    }

expected:1

    enum Foo : int8
    {
        A = -2,
        B
    }

    const uint8 count = Foo::B;

    inline void main()
    {
        bool[count] x;
    }

expected:0

    class Foo
    {
    public:
        enum Bar : uint32
        {
            A,
            B
        }

        Bar x = Foo::Bar::A;
    }

    inline void main()
    {
        Foo::Bar x = Foo::Bar::B;
    }

expected:0

    template <typename T>
    class Foo
    {
    public:
        enum Bar : T
        {
            A = 10
        }
    }

    inline void main()
    {
        static Foo<uint3> foo;
    }

expected:1

    template <typename T>
    class Foo
    {
    public:
        enum Bar : T
        {
            A = 10
        }
    }

    inline void main()
    {
        static Foo<bool> foo;
    }

expected:1

    template <typename T>
    class Foo
    {
    public:
        enum Bar : T
        {
            A = 10
        }
    }

    inline void main()
    {
        static Foo<uint4> foo;
    }

expected:0

    template <auto N>
    class Foo
    {
    public:
        enum Bar : uint5
        {
            A = N
        }
    }

    inline void main()
    {
        static Foo<100> foo;
    }

expected:1

    template <auto N>
    class Foo
    {
    public:
        enum Bar : uint5
        {
            A = N
        }
    }

    inline void main()
    {
        static Foo<10> foo;
    }

expected:0

    template <typename T>
    class Foo
    {
    public:
        enum Bar : uint1
        {
            Zero
        }

        inline void Run()
        {
            Bar b = Bar::Zero;
        }
    }

    inline void main()
    {
        static Foo<uint32> foo;

        static Foo<uint32>::Bar bar = Foo<uint32>::Bar::Zero;
    }

expected:0

    inline uint1 Run()
    {
        enum Foo : uint1
        {
            On, Off
        }

        const auto x = Foo::On;

        return Foo::Off;
    }

expected:0

    inline auto Run()
    {
        enum Foo : uint1
        {
            On, Off
        }

        const auto x = Foo::On;

        return x;
    }

expected:0

    template <typename T>
    inline void Foo()
    {
        enum X : T
        {
            Zero, One, Two
        }

        X x;
    }

    inline void main()
    {
        Foo<uint2>();
    }

expected:0

    template <typename T>
    inline void Foo()
    {
        enum X : T
        {
            Zero, One, Two
        }
    }

    inline void main()
    {
        Foo<uint1>();
    }

expected:1

    inline void main()
    {
        static C<Bar> c;
    }

expected:0

    enum E : uint8
    {
        B2
    }

    inline void main()
    {
        static C<E> c;
    }

expected:0

    enum E : uint8
    {
        B1
    }

    inline void main()
    {
        static C<E> c;
    }

expected:1
