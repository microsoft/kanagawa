// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.higher_order_functions
    { Go
    , DoIt
    , Go_last
    , DoEndTransacation
    , Go2
    , Go3
    , Go4
    , Go5
    , Go6
    , Go7
    , Go8
    , Go9
    , Go10
    , GoVar
    , Do10
    , U
    , S
    , C
    , C2
    , closure
    , type_t
    , add_and_increment
    }

import type.stdtype
import data.function
import numeric.int.operator

inline auto add(auto x, auto y)
{
    return x + y;
}

const auto add_and_increment = compose2(increment, add);

using type_t = count_t<bytesizeof uint32>;

inline uint32 Go((uint32) -> uint32 Do, uint32 x)
{
    return Do(x) + Do(x + 1);
}

inline uint32 DoIt(uint32 x)
{
    return x;
}

inline void Go_last(([[last]] bool) -> void Do)
{
    Do(true);
}

inline void DoEndTransacation([[last]] bool end)
{
}

inline uint32 Go2((uint32) -> uint32 Do1, (uint32) -> uint32 Do2, uint32 x)
{
    return Do1(x) + Do2(x + 1);
}

template <typename T>
inline T Go3((T) -> T Do, T x)
{
    T y = x;

    return Do(y);
}

template <typename T, typename R>
inline R Go4((T, T) -> R Do, T x, T y)
{
    T z = x;

    return Do(y, z);
}


template <typename T>
struct S
{
    T x;
}

template <typename T>
inline T Go5((S<T>) -> T Do)
{
    S<T> s;
    return Do(s);
}

template <auto N>
using U = uint<clog2(N)>;

template <auto N>
inline U<N> Go6((U<N>) -> U<N> Do)
{
    U<N> x;
    return Do(x);
}

template <typename T>
inline auto Go7((T) -> auto Do, T x)
{
    return Do(x);
}

inline auto Go8((uint32) -> auto Do)
{
    using t = decltype(Do(0));

    t[10] result;

    result[0] = Do(10);

    return result;
}

template <typename T>
inline auto Go9((T, auto) -> auto fn, T x)
{
    return [fn, x](auto y)
        {
            return fn(x, y);
        };
}

inline void Do10(type_t x)
{
}

inline void Go10((type_t) -> void Do)
{
    Do(0);
}

template <typename T1, typename T2, typename T3, typename T4>
inline auto GoVar((T1, T2, T3, T4) -> auto Do)
{
    static if (T1 == void)
    {
        return Do();
    }
    else static if (T2 == void)
    {
        return Do(10);
    }
    else static if (T3 == void)
    {
        return Do(10, 11);
    }
    else static if (T4 == void)
    {
        return Do(10, 11, 12);
    }
    else
    {
        return Do(10, 11, 12, 13);
    }
}

class C
{
public:
    inline uint32 Go((uint32) -> uint32 Do, uint32 x)
    {
        return Do(x) + Do(x + 1) + _x;
    }

    template <typename T, typename R>
    inline R Go4((T, T) -> R Do, T x, T y)
    {
        T z = x;

        return Do(y, z);
    }

    uint32 _x;
}

template <typename T>
class C2
{
public:
    inline T Go((T) -> T Do, T x)
    {
        return Do(x) + Do(x + 1) + _x;
    }

    template <typename R>
    inline R Go4((T, T) -> R Do, T x, T y)
    {
        T z = x;

        return Do(y, z);
    }

    T _x;
}

template <typename T, typename R>
inline auto closure((T) -> R fn)
{
    decltype(fn) x = fn;
    return x;
}

