// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module a1.a1
    { a1
    , Aa
    }

import a1.b1.b1

template <auto N>
struct a1
{
    b1<N> x;
}

template <auto N>
struct Aa
{
    uint<N> x;
}
