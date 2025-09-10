// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright (C) Microsoft Corporation. All rights reserved.
module hardware.dsp
    { umul18
    , imul18
    }

import compiler.hal as hal
import compiler.config as config

inline uint36 umul18(uint18 x, uint18 y)
{
    static hal::umul18<config::hal_device_family, 2> dsp;
    return dsp.op(x, y);
}

inline int36 imul18(int18 x, int18 y)
{
    static hal::imul18<config::hal_device_family, 2> dsp;
    return dsp.op(x, y);
}