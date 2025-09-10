// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.function_template_param
    { UInt
    , UInt3
    , T1
    , T2
    , T3
    , Foo
    , T_to_R
    , S
    , FromS
    , From
    , ToS
    , To
    }

template <auto N>
using UInt = uint<N>;

using UInt3 = uint3;

template <typename R, typename P, (P) -> R F>
class T3
{
    R Foo(P x)
    {
        return F(x);
    }
}

template <(uint3) -> bool F>
class T1
{
    bool Foo(uint3 x)
    {
        return F(x);
    }

    T3<bool, uint3, F> t3;
}

template <(int3, int2, int1) -> bool F>
class T2
{
    bool Foo(int1 x)
    {
        return F(x, x , x);
    }
}

template <(uint32) -> uint32 F>
inline uint32 Foo(uint32 x)
{
    return F(x);
}

template <typename R, typename T, R N>
inline R T_to_R(T x)
{
    return N;
}

template <typename T>
struct S
{
    T x;
}

template <typename T>
inline void FromS(S<T> x)
{
}

template <typename T, (T) -> void Fn>
inline void From(T x)
{
    Fn(x);
}

template <typename T>
inline S<T> ToS()
{
    S<T> s;
    return s;
}

template <typename T, () -> T Fn>
inline T To()
{
    return Fn();
}
