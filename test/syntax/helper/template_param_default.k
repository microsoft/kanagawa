// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.template_param_default
  { Foo
  , Bar
  , C1
  , C2
  , S1
  , A1_t
  , A2_t
  }

import type.stdtype

template <typename T, typename U = uint<bitsizeof T + 2>, auto N = bitsizeof U + 3>
inline int<N> Foo(T x)
{
    return 0;
}

inline uint13 F1(uint14 x)
{
    return x;
}

template <typename T, typename U, (T) -> U Fn = F1>
inline U Bar(T x)
{
    return Fn(x);
}

template <typename T, auto N = bitsizeof T, auto M = N * 2>
class C1
{
    public:
        uint<N> x;
        uint<M> y;
}

template <typename T, auto N = bitsizeof T, auto M = N + bitsizeof T>
class C2
{
    public:
        uint<N> x;
        uint<M> y;
}

template <typename T = uint32, typename U = bool>
struct S1
{
    T x;
    U y;
}

template <typename T, typename U = S1<T>>
using A1_t = U;

template <typename T, auto N = 10, typename U = index_t<N>>
using A2_t = S1<T[N], U>;
