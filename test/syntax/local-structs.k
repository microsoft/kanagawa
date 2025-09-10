// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        struct Foo
        {
            uint32 x;
        }

        Foo foo;

        foo.x = 10;
    }

expected:0

    inline void main()
    {
        union Foo
        {
            uint32 x;
            bool y;
        }

        Foo foo;

        foo.x = 10;
        foo.y = true;
    }

expected:0

    inline uint32 Run()
    {
        struct Foo
        {
            uint32 x;
        }

        Foo foo = {};

        return foo.x;
    }

expected:0

    inline uint32 Run()
    {
        struct Foo
        {
            bool x;
        }

        Foo foo = {};

        return foo.x;
    }

expected:1

    inline uint32 Run()
    {
        template <typename T>
        struct Foo
        {
            T x;
        }

        Foo<uint32> foo = {};

        return foo.x;
    }

expected:0

    inline uint32 Run()
    {
        template <typename T>
        struct Foo
        {
            T x;
        }

        Foo<bool> foo = {};

        return foo.x;
    }

expected:1

    inline void main()
    {
        struct Foo
        {
            uint32 x;
        }

        Foo foo;

        F1<Foo>(foo);
    }

expected:0

    inline void main()
    {
        struct Foo
        {
            uint32 x;
        }

        Foo foo;

        F2<Foo, F1<Foo>>(foo);
    }

expected:0

    inline void main()
    {
        struct Foo
        {
            uint32 x;
        }

        Foo foo;

        using Foo1 = Foo;

        F2<Foo, F1<Foo1>>(foo);
    }

expected:0

    inline void main()
    {
        struct Foo
        {
            uint32 x;
        }

        Foo foo;

        struct Bar
        {
            uint32 x;
        }

        F2<Foo, F1<Bar>>(foo);
    }

expected:1

    inline void main()
    {
        struct Foo
        {
            uint32 x;
        }

        Foo foo;

        struct Bar
        {
            uint32 x;
        }

        Bar bar;

        F2<Foo, F1<Foo>>(bar);
    }

expected:1

    inline void main()
    {
        struct Foo
        {
            uint32 x;
        }

        struct Bar
        {
            Foo foo;
        }

        Bar bar;

        bar.foo.x = 10;
    }

expected:0

    template <typename T>
    inline void Do(T x)
    {
        struct Foo
        {
            T x;
        }

        Foo foo;

        foo.x = x;
    }

    inline void main()
    {
        Do<bool>(false);
    }

expected:0

    template <typename T>
    inline void Do(T x)
    {
        template <auto N>
        struct Foo
        {
            T x;
            T[N] y;
        }

        Foo<10> foo;

        foo.x = x;
        foo.y[0] = x;
    }

    inline void main()
    {
        Do<bool>(false);
    }

expected:0
