// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.structs
    { Foo1
    , Foo2
    }

template <typename T>
class Foo1
{
public:
    struct Bar
    {
        T x;
    }

    Bar Do()
    {
        Bar x;
        return x;
    }
}


class Foo2
{
public:
    template <typename T, auto N>
    struct Bar
    {
        T[N] x;
    }

    template <typename T, auto N>
    Bar<T, N> Do()
    {
        Bar<T, N> b;

        return b;
    }
}

