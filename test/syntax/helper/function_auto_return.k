// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.function_auto_return
    { Fn_void
    , Fn_return_void
    , Fn_uint4
    , Fn_T
    , Fn_Tx2
    , UINT
    , Go
    }

inline auto Fn_void()
{
}

inline auto Fn_return_void()
{
    return;
}

inline auto Fn_uint4()
{
    return 10;
}

template <typename T>
inline auto Fn_T(T x)
{
    return x;
}

template <typename T>
inline auto Fn_Tx2(T x)
{
    auto y = x * 2;
    return y;
}

enum Enum : uint1
{
    Zero
}

template <auto N>
struct UINT
{
    uint<N> x;
}

inline uint4 Go(() -> uint4 Do)
{
    return Do();
}
