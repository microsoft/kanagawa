// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.member_function_template
    { C1
    , C2
    , T1
    , T2
    , T3
    }

class C1
{
public:
    template <typename T>
    T Foo(T x)
    {
        return x;
    }

    template <typename T, T x>
    inline T Bar()
    {
        return x;
    }

    template <typename T, (T) -> T Fn>
    T Buz(T x)
    {
        return Fn(x);
    }
}

class C2
{
public:
    C1 c1;
    C1[4] c1s;
}

template <typename T>
class T1
{
public:
    template <typename S>
    S Foo(T x)
    {
        return x;
    }

    template <typename S, T x>
    T Bar(S s)
    {
        return x;
    }
}

template <typename T>
class T2
{
public:
    template <(T) -> T Fn>
    T Foo(T x)
    {
        return Fn(x);
    }
    
    template <typename S, (S) -> T Fn>
    inline T Bar(S x)
    {
        return Fn(x);
    }
}

template <typename T>
class T3
{
public:
    template <typename S>
    class T4
    {
    public:
        template <typename U, (S, U) -> T Fn>
        inline T Bar(S x, U y)
        {
            return Fn(x, y);
        }
    }
}

