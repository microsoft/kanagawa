// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.initializer_list
    { Bar
    , Bux
    , Bump
    , FnT
    }

struct Bar
{
    uint32 x;
    uint32 y;
}

template <typename T, typename S>
struct Bux
{
    T x;
    S y;
}

union Bump
{
    uint32 x;
    int16 y;
}

template <typename T>
inline void FnT(T x)
{
}
