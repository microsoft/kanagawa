// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module type.stdtype
    { index_t
    , count_t
    , bitindex_t
    , bitcount_t
    }

//| Type of index for a collection of N elements
template <auto N>
using index_t = uint<(N > 1) ? clog2(N) : 1>;

template <typename T>
using bitindex_t = index_t<bitsizeof(T)>;

//| Type to express a count of N elements
template <auto N>
using count_t = uint<clog2(N + 1)>;

template <typename T>
using bitcount_t = count_t<bitsizeof(T)>;