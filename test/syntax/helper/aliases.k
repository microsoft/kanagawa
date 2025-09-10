// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.aliases
    { Bar
    , MyInt
    }

template <typename T>
class Bar
{
    const T x = {};
}

template <uint8 N>
using MyInt = int<N>;
