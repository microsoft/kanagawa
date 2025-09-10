// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.function_constants
    { Foo
    , FooB
    , Bar
    , T1
    , C1
    , C2
    , F3
    , T_to_R
    , T_to_R_t
    , max_threads_N_T_to_R_t
    }

inline uint32 Foo(bool x)
{
    return x ? 10 : 20;
}

inline bool FooB(uint32 x)
{
    return x > 10;
}

template <typename T>
inline T Bar(T x)
{
    return x;
}

template <(uint32) -> uint32 F>
inline uint32 T1(uint32 x)
{
    return F(x);
}

template <(bool) -> uint32 Fn>
class C1
{
public:
    const auto fn = Fn;

    inline void Run()
    {
        fn(false);
    }
}

template <typename T, typename U, (T) -> U Fn>
class C2
{
public:
    const auto fn = Fn;

    inline void Run()
    {
        fn(false);
    }
}

template <(bool) -> uint32 Fn>
inline void F3()
{
    const auto fn = Fn;
    fn(false);
}

template <typename T, typename R>
inline R T_to_R(T x)
{
   return x;
}

template <auto N, typename T, typename R>
using max_threads_N_T_to_R_t = [[max_threads(N)]] (T x) -> R;

template <typename T, typename R>
using T_to_R_t = (T x) -> R;

