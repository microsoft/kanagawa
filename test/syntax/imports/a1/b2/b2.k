// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module a1.b2.b2
    { b2
    }

import a1.b2.c1.c1

template <auto N>
struct b2
{
    c1<N> x;
}

