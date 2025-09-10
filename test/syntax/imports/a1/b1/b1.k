// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module a1.b1.b1
    { b1
    , bb1
    , b1_fn
    }

import a1.b1.c1.c1

template <auto N>
struct b1
{
    c1<N> x;
}

const auto bb1 = 10;

inline void b1_fn()
{
    static b1<1> g_b1;
}

