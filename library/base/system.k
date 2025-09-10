// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module base.system
    { cycles
    , stages
    }

//| Return cycle count.
inline uint64 cycles()
{
    return __cycles();
}

//| Inject `N` stages into the generated hardware pipeline.
template <auto N>
inline void stages()
{
    static assert(N >= 0);

    static for (const auto i : N) barrier;
}