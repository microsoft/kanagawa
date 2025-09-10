// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module debug.sim_assert
    { sim_assert
    }

//| Alias for `assert` intrinsic. Useful when defining an `assert` function in
// a module that is meant to be imported qualified.
inline void sim_assert(bool cond)
{
    assert(cond);
}
