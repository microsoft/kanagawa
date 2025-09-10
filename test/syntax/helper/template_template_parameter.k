// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.template_template_parameter
  { T1
  , T2
  , T3
  , T4
  , S1
  , S2
  , S3
  , Fn1
  , FnFn
  }

template <typename T>
struct S1
{
    T x;
}

template <typename T>
struct S2
{
    T x;
}

template <typename T, auto N = 10, typename U = bool>
struct S3
{
    T[N] x;
    U y;
}

template <typename T>
using A1 = S1<T>;

template <template <typename> typename TT = S1>
class T1
{
public:
    TT<uint32> x;
}

template <template <typename> typename TT = S1>
inline void Fn1()
{
    static TT<uint32> x;
}

template <typename T, template <typename> typename TT = A1>
class T2
{
public:
    TT<T> x;
}

template <auto N, template <typename, auto> typename TT>
class T3
{
public:
    TT<uint32, N> x;
}

template <typename T, template <T> typename TT>
class T4
{
public:
    TT<false> x;
}

template <typename T, template <typename> typename TT, () -> TT<T> fn>
inline void FnFn()
{
}
