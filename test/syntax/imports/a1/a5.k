// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module a1.a5
    { a1
    , N
    }

template <typename T>
struct a1
{
    T x;
}

class N
{
public:
    using Aa = bool;

    template <auto X>
    inline void Go()
    {
    }
}
