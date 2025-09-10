// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.tuple
    { tuple2
    , tuple3
    , tuple4
    , pair
    , make_pair
    , first
    , second
    , swap
    , equal
    }

template <typename T1, typename T2>
struct tuple2
{
    T1 first;
    T2 second;
}

template <typename T1, typename T2, typename T3>
struct tuple3
{
    T1 first;
    T2 second;
    T3 third;
}

template <typename T1, typename T2, typename T3, typename T4>
struct tuple4
{
    T1 first;
    T2 second;
    T3 third;
    T4 fourth;
}

//| For improved type inference, consume `tuple2` and produce `pair`.
// For example, the compiler is able to infer the template of `first` in
// `first(make_pair(1, 2))`.
template <typename T1, typename T2>
using pair = tuple2<T1, T2>;


//| Create a pair of values.
template <typename T1, typename T2>
inline pair<T1, T2> make_pair(T1 x, T2 y)
{
    return {x, y};
}


//| Return the first value of a pair.
//
// #### __Example__
//
//     >>> first(make_pair(1, 2));
//     1
template <typename T1, typename T2>
inline auto first(tuple2<T1, T2> x)
{
    return x.first;
}


//| Return the second value of a pair.
//
// #### __Example__
//
//     >>> second(make_pair(1, 2));
//     2
template <typename T1, typename T2>
inline auto second(tuple2<T1, T2> x)
{
    return x.second;
}


//| Swap the components of a `pair`.
//
//     prop> swap(swap(x)) == x
template <typename T1, typename T2>
inline pair<T2, T1> swap(tuple2<T1, T2> p)
{
    return {p.second, p.first};
}

//| Compare pairs for equality.
template <typename T1, typename T2>
inline bool equal(tuple2<T1, T2> x, tuple2<T1, T2> y)
{
    return x.first == y.first && x.second == y.second;
}