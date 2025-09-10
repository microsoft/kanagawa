// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.template_argument_deduction_from_instance
    { Tuple2
    , Pair
    , Duo
    , MakeTuple
    , MakePair
    , MakeDuo
    , First
    , Second
    , Go
    , Go1    
    }

template <typename T1, typename T2>
struct Tuple2
{
    T1 first;
    T2 second;
}

template <typename T1, typename T2>
using Pair = Tuple2<T1, T2>;

template <typename T>
using Duo = Pair<T, T>;

template <typename T1, typename T2>
inline Pair<T1, T2> MakePair(T1 x, T2 y)
{
    return {x, y};
}

template <typename T1, typename T2>
inline Tuple2<T1, T2> MakeTuple(T1 x, T2 y)
{
    return {x, y};
}

template <typename T>
inline Duo<T> MakeDuo(T x, T y)
{
    return MakePair(x, y);
}

template <typename T1, typename T2>
inline T1 First(Tuple2<T1, T2> x)
{
    return x.first;
} 

template <typename T1, typename T2>
inline T2 Second(Tuple2<T1, T2> x)
{
    return x.second;
} 

template <typename T1, typename T2>
inline void Go((Tuple2<T1, T2>) -> T1 fn)
{
}

template <typename T1, typename T2>
inline void Go1((Tuple2<T1, T2>) -> T1 fn, Tuple2<T1, T2> p)
{
}
