// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.memory
  { check_return_type
  , check_scalar_return_type
  , memory
  , memory_ecc
  , memory_ecc_strict
  , read_mem
  , ecc
  , optional
  }

import data.memory
import data.optional

template <typename T, template <typename> typename S, () -> S<T> fn>
inline void check_return_type()
{
}

template <typename T, () -> T fn>
inline void check_scalar_return_type()
{
}

template <typename T, template <typename, auto> typename Memory>
inline auto read_mem()
{
    static Memory<T, 16> mem;

    return mem[0];
}
