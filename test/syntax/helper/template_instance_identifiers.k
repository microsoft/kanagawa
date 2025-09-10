// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.template_instance_identifiers
    { Foo
    , A
    , B
    }

template <typename T, typename S>
struct Foo
{
    T x;
    S y;
}

template <typename T>
class A
{
public:
    using A1_t = T;

    template <auto N>
    using A2_t = T[N];
}

template <typename T>
class B
{
public:
    template <typename S>
    class A
    {
    public:
        using A1_t = S;

        template <auto N>
        using A2_t = S[N];

        using A3_t = Foo<T, S>;
    }
}

