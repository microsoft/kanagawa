// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright (C) Microsoft Corporation. All rights reserved.
module hardware.dsp
    { fadd32
    , fsub32
    , facc32
    , addbf16x3
    }

import compiler.hal as hal
import compiler.config as config

inline float32 fadd32(float32 x, float32 y)
{
    static hal::fadd32<config::hal_device_family, 3> dsp;
    return dsp.op(x, y);
}

inline float32 fsub32(float32 x, float32 y)
{
    static hal::fsub32<config::hal_device_family, 3> dsp;
    return dsp.op(x, y);
}

inline float32 facc32(float32 x, bool accumulate)
{
    static hal::facc32<config::hal_device_family, 3> dsp;
    return dsp.op(x, accumulate);
}

inline uint16[3] addbf16x3(uint16[3] x, uint16[3] y)
{
    static hal::addbf16x3<config::hal_device_family, 3> dsp;
    return cast<uint16[3]>(dsp.op(cast<uint48>(x), cast<uint48>(y)));
}