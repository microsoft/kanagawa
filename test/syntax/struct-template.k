// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    template <typename T, uint32 N>
    struct Foo
    {
        T         a;
        T[N]      b;
        Bar<T>    c;
        uint<N>   e;
    }

expected:0

    template <typename T>
    inline Bar<T> Foo(Bar<T> x)
    {
        Bar<T> bar;
        bar.x = x.x;

        return bar;
    }

expected:0

    template <typename T>
    class Foo
    {
        T Do()
        {
            return bar.x;
        }

        Bar<T>      bar;
        Bar<Bar<T>> barbar;
    }

    inline void main()
    {
        static Foo<int4> foo;
    }

expected:0

    inline void main()
    {
        static Bar<uint32, 10> bar;
    }

expected:1

    inline void main()
    {
        static Bar bar;
    }

expected:1

    template <typename T>
    struct Foo
    {
        uint<bitsizeof T> x;
    }

    inline void main()
    {
        static Foo<Bar<uint4>> foo;
    }

expected:0
