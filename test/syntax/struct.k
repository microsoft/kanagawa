// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    class Foo
    {
    public:
        struct Bar
        {
            uint4 x;
            bool y;
        }

        Bar b;
    }

    inline void main()
    {
        static Foo::Bar b;
    }

expected:0

    class Foo
    {
    public:
        template <typename T, auto N>
        struct Bar
        {
            T[N] x;
        }
    }

    inline void main()
    {
        static Foo::Bar<uint3, 10> b10;
        static Foo::Bar<uint4, 11> b11;

        static assert(bitsizeof(b10) == 3 * 10);
        static assert(bitsizeof(b11) == 4 * 11);
    }

expected:0

    inline Foo2::Bar<uint3, 10> Go()
    {
        static Foo2 foo;
        return foo.Do<uint3, 10>();
    }

expected:0

    inline Foo2::Bar<uint3, 10> Go()
    {
        static Foo2 foo;
        return foo.Do<uint3, 11>();
    }

expected:1

    inline Foo1<uint4>::Bar Go()
    {
        static Foo1<uint4> foo;
        return foo.Do();
    }

expected:0

    inline Foo1<uint4>::Bar Go()
    {
        static Foo1<uint5> foo;
        return foo.Do();
    }

expected:1

    struct Foo
    {
        struct Bar
        {
            uint4 x;
            bool y;
        }
    }

expected:1

    union Foo
    {
        struct Bar
        {
            uint4 x;
            bool y;
        }
    }

expected:1

    inline void main()
    {
        uint32[32] var;
        return var.val[0]; // access to non-existant field
    }

expected:1

    inline void main()
    {
        uint32[32][3] var;
        return var[0].val[3]; // access to non-existant field
    }

expected:1

    struct Foo
    {
        uint32 x;
    }

    inline void main()
    {
        static Foo foo;

        foo.y = 10;
    }

expected:1
