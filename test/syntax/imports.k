// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        static a1<10> x;
    }

expected:1

    import missing

expected:1

    import binary

expected:1

    import a1.a1

    inline void main()
    {
        static a1<10> a;
    }

expected:0

    import a1.a1

    inline void main()
    {
        static b1<10> b;
    }

expected:1

    import a1.a1
    import a1.b1.b1

    inline void main()
    {
        static a1<10> a;
        static b1<10> b;
    }

expected:0

    import a1.b1.b1

    inline void main()
    {
        static b1<10> b;
    }

expected:0

    import a1.b1.c1.c1

    inline void main()
    {
        static c1<10> c;
    }

expected:0

    import a1.b2.c1.c1

    inline void main()
    {
        static c1<10> c;
    }

expected:0

    import a1.b2.c1.c1
    struct c1
    {
        uint32 x;
    }

    inline void main()
    {
        static c1 c;
    }

expected:1

    import a1.a1

    inline void main()
    {
        static Aa<10> x;
    }

expected:0

    import a1.a1

    template <auto N>
    struct Aa
    {
        uint<N> x;
    }

    inline void main()
    {
        static Aa<10> x;
    }

expected:1

    import a1.a1

    template <auto N, auto M>
    struct Aa
    {
        uint<N> x;
        uint<M> y;
    }

    inline void main()
    {
        static Aa<10, 11> x;
    }

expected:1

    import a1.b2.c1.c1
    class Foo
    {
        struct c1
        {
            uint32 x;
        }
        c1 c;
    }

expected:0

    import a1.b1.c1.c1
    import a1.b2.c1.c1

expected:0

    import a1.b2.b2

    inline void main()
    {
        static c1<10> c;
    }

expected:1

    import a1.b1.c1.c1
    import a1.b2.c1.c1

    inline void main()
    {
        static c1<10> c;
    }

expected:1

    import a1.b1.b1
    import a1.b2.b2

    inline void main()
    {
        static b1<10> b;
    }

expected:0

    import a1.b1.b1
    import a1.b2.b2

    inline void main()
    {
        static b2<10> b;
    }

expected:0

    import a1.b1.b1
    import a1.b2.b2

    inline void main()
    {
        static b1<10> b;
        static b2<10> bb;
    }

expected:0

    import a1.a1

    inline void main()
    {
        a1<10> a = {};
        b1<10> b = a.x;
    }

expected:1

    import a1.a1

    inline void main()
    {
        a1<10> a = {};
        auto b = a.x;
    }

expected:0

    import a1.a1
    import a1.b1.b1

    inline void main()
    {
        a1<10> a = {};
        b1<10> b = a.x;
    }

expected:0

    import a1.a1
    import a1.b1.b1

    inline void main()
    {
        static a1<bb1> a;
    }

expected:0

    import a1.a1

    inline void main()
    {
        static a1<bb1> a;
    }

expected:1

    inline void main()
    {
        b1_fn();
    }

expected:1

    import a1.a1

    inline void main()
    {
        b1_fn();
    }

expected:1

    import a1.b1.b1

    inline void main()
    {
        b1_fn();
    }

expected:0

    import a1.a2

expected:1

    import a1.a3

    inline void main()
    {
        static a1<10> x;
    }

    expected:0

    import a1.a4

    inline void main()
    {
        static a1<10> x;
    }

    expected:0

    import a1.a4

    inline void main()
    {
        static Aa<10> x;
    }

    expected:1

    import a1.a1
    import a1.a4

    inline void main()
    {
        static a1<10> x;
        static Aa<10> y;
    }

expected:0

    import a1.a1
    import a1.a3
    import a1.a4

    inline void main()
    {
        static a1<10> x;
        static Aa<10> y;
    }

expected:0

    import a1.a3
    import a1.a4

    inline void main()
    {
        static a1<10> x;
    }

expected:0

    import a1.a1
    import a1.a5

    inline void main()
    {
        static a1<bool> x;
    }

expected:1

    import a1.a1
    import a1.a5 as A5

    inline void main()
    {
        static A5::a1<bool> x;
        static a1<5> y;
    }

expected:0

    import a1.a1 as A1
    import a1.a5 as A5

    inline void main()
    {
        static A5::a1<bool> x;
        static A1::a1<5> y;
    }

expected:0

    import a1.a1 as A1
    import a1.a5 as A5

    inline void main()
    {
        static a1<bool> x;
        static A1::a1<5> y;
    }

expected:1

    import a1.a1 as N
    import a1.a5

    inline void main()
    {
        static N::Aa<1> x;
    }

expected:1


    import a1.a1 as M
    import a1.a5

    inline void main()
    {
        static M::Aa<1> x;
        static N::Aa y;
    }

expected:0

    import function

    inline void X()
    {
        struct Foo
        {
            uint32 x;
        }

        Foo foo;
    }

expected:0

    import undefined

expected:1

    import a1.a6

    inline void main()
    {
        static a1 x;
    }

expected:0

    import a1.a1

warning:redundant-import

    import a1.a1

    inline void main()
    {
        static a1<10> x;
    }

warning:

    module foo { a1 }
    import a1.a1

warning:

    import a1.a3

warning:redundant-import

    import a1.a3

    inline void main()
    {
        static a1<10> x;
    }

warning:

    import a1.a6

    inline void main()
    {
        static a1 x;
    }

warning:redundant-import

    import a1.a7

    inline void main()
    {
        static a1<10> x;
    }

warning:redundant-import

    import a1.a3
    import a1.a4

    inline void main()
    {
        static a1<10> x;
    }

warning:redundant-import

    import a1.a3

    inline void main()
    {
        static a1<10> x;
    }

warning:

    import a1.a4

    inline void main()
    {
        static a1<10> x;
    }

warning:

    import a1.a1
    import a1.a8

    inline void main()
    {
        static a1<10> x;
        static a8 y;
    }

warning:redundant-import

    import a1.a8
    import a1.a1

    inline void main()
    {
        static a1<10> x;
        static a8 y;
    }

warning:

    import a1.a9

expected:1

    import a1.a10

expected:1

    import a1.a11

expected:1

    import a1.a12

expected:1

    import a1.a13

    using foo = bar;

    inline void main()
    {
        static foo x;
    }

expected:0

    import a1.a14 as A14

    using foo = A14::bar;

    inline void main()
    {
        static foo x;
        static A14::bar y;
    }

expected:0

    import a1.a13 as A13

    using foo = A13::bar;

    inline void main()
    {
        static foo x;
        static A13::bar y;
    }

expected:0

    import a1.a15

expected:1

    module foo
    {
       x
    }

expected:1

    module foo
    {
       x
    }

    inline void main()
    {
        static uint32 x;
    }

expected:1

    module foo
    {
       x
    }

    const uint32 x = 10;

expected:0

    import a1.a16

    inline void main()
    {
        static Aa<10> aa;
        static a1<10> a;
    }

expected:0

    import a1.a19

    inline void main()
    {
        static Aa<10> aa;
        static a1<10> a;
    }

expected:0

    import a1.a17

    inline void main()
    {
        static Aa<10> a;
    }

expected:0

    import a1.a17

    inline void main()
    {
        static a1<10> a;
    }

expected:1

    import a1.a18

    inline void main()
    {
        static Aa<10> a;
    }

expected:0

    import a1.a18

    inline void main()
    {
        static a1<10> a;
    }

expected:1

    import a1.a20

expected:1

    import a1.a21

expected:1

    import a1.a22

expected:0

    import a1.a25

    inline void main()
    {
        static Aa<10> aa;
        static a1<10> a;
        static C c;
        static D d;
    }

expected:0

   import data.bits

   inline void main()
   {
       static optional<bool> x;
   }

warning:

    import a1.a26

warning:redundant-import

    import a1.a26

    inline void main()
    {
        static Foo foo = Foo::Bar;
    }

expected:0

    inline void main()
    {
        static Foo foo = Foo::Bar;
    }

expected:1

    import a1.a26

    inline void main()
    {
        static Aa<10> a;
    }

expected:1

    import a1.a26
    import a1.a1

    inline void main()
    {
        static Aa<10> a;
    }

warning:redundant-import

    import a1.a27

    inline void main()
    {
        static Foo foo = Foo::Bar;
        static Aa<10> a;
    }

expected:0

    import a1.a28

    inline void main()
    {
        static Foo foo = Foo::Bar;
        static Aa<10> a;
    }

expected:0

    import a1.a30

    inline void main()
    {
        static Foo foo = Foo::Bar;
        static Aa<10> a;
    }

expected:0

    import a1.a29

    inline void main()
    {
        static Foo foo = Foo::Bar;
        static Aa<10> a;
    }

expected:1

    import a1.a29

    inline void main()
    {
        static Foo foo = Foo::Bar;
    }

expected:0

    import a1.a26
    import a1.a1

    inline void main()
    {
        static Foo foo = Foo::Bar;
        static Aa<10> a;
    }

expected:0

    import a1.a31

    inline void main()
    {
        static A31 a31;
    }

expected:0

    import a1.a31

    inline void main()
    {
        static Foo foo;
    }

expected:1

    import a1.a32

    inline void main()
    {
        static Foo foo;
    }

expected:0

    import a1.a1
    import a1.a8

warning:redundant-import

    import a1.a5 as A5

    class C
    {
    public:
        A5::N foo;

        inline void Run()
        {
            foo.Go<10>();
        }
    }

expected:0

    import a1.a1

    const auto x = g_a1;

expected:1

    import a1.a16

    const auto x = g_a1;

expected:1

    import a1.a26

    const auto x = a;

expected:1

    import diff.foo_self

    static assert(self == 99);
    static assert(foo == false);
    static assert(x == 10);
    static assert(y == false);
    static assert(bitsizeof(t) == 32);

warning:0

    import diff.self_foo

    static assert(self == 99);
    static assert(foo == 3);
    static assert(x == 10);
    static assert(y == false);
    static assert(bitsizeof(t) == 32);

warning:0

    import diff.foo_bar

    static assert(foo == 3);
    static assert(bar == 2);
    static assert(x == 11);
    static assert(y == true);
    static assert(bitsizeof(t) == 20);

warning:0

    import diff.foo_minus_bar

    static assert(foo == 3);

    inline void main()
    {
        static foo_t x;
    }

warning:0

    import diff.foo_minus_bar

    static assert(x == 11);

expected:1

    import diff.self_missing

expected:1

    import export

warning:0

    import reset

warning:0

    import a1.a1 as Foo

    inline void main()
    {
        static Foo::a1<10> a;
    }

expected:0

    import a1.a1 as Foo

    class Foo
    {
    }

    inline void main()
    {
        static Foo::a1<10> a;
    }

expected:1

    import a1.a1 as Foo

    class Bar
    {
    public:
        class Foo
        {
            using t = uint32;
        }
    }

    inline void main()
    {
        static Foo::a1<10> a;
        static Bar::Foo::t b;
    }

expected:0

    import a1.a1 as Foo

    class Bar
    {
    public:
        class Foo
        {
            using t = uint32;
        }

        Foo::a1<10> a;
    }

expected:1

    import a1.a1 as Foo

    class Bar
    {
    public:
        class Foo
        {
            using t = uint32;
        }

        Foo a;
    }

expected:1

    import a1.a1 as Bar

    class Bar
    {
    public:
        class Foo
        {
            using t = uint32;
        }
    }

    inline void main()
    {
        static Bar::Foo::t a;
        static Bar::Foo b;
    }

expected:1

    import a1.a1 as N
    import a1.a5

    inline void main()
    {
        static N a;
    }

expected:1

    import a1.a1 as N
    import a1.a5

    inline void main()
    {
        static N::a1<10> a;
    }

expected:1

    import a1.a1 as N
    import a1.a5 as M

    inline void main()
    {
        static M::N a;
    }

expected:0

    import a1.a1 as N
    import a1.a5 as M

    inline void main()
    {
        static N::a1<10> a;
    }

expected:0

    import with-dash

    static assert(x == 10);

expected:0

    import with-dash.with-dash

    static assert(x == 11);

expected:0

    import a1 .a1

expected:1

    import a1. a1

expected:1

    import a1 . a1

expected:1
