// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.local_structs
    { F1
    , F2
    }

template <typename T>
inline uint32 F1(T x)
{
    return bitsizeof x;
}

template <typename T, (T) -> uint32 Fn>
inline uint32 F2(T x)
{
    return Fn(x);
}

