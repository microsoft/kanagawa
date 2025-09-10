// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.closure.core
    { Closure
    , make_capture0
    }

template <typename FunctionType, FunctionType fn, typename Capture>
using Closure = Capture;

//| Capture 0 values
inline auto make_capture0
    (
    )
{
    struct Capture
    {
    }

    Capture capture;

    return capture;
}