// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.struct_constexpr
    { Foo
    , Go
    }
    
struct Foo
{
    uint32 x;
    bool b;
}

template <typename T, T x>
inline T Go()
{
    T[2] y = {x, x};
    
    return x;
}
