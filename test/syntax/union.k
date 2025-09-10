// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    class Foo
    {
    public:
        union Bar
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

    template <typename T>
    class Foo
    {
    public:
        union Bar
        {
            T x;
            bool y;
        }

        Bar b;
    }

    inline void main()
    {
        static Foo<uint4>::Bar b;
    }

expected:0

    struct Foo
    {
        union Bar
        {
            uint4 x;
            bool y;
        }
    }

expected:1

    union Foo
    {
        union Bar
        {
            uint4 x;
            bool y;
        }
    }

expected:1
