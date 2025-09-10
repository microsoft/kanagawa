// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright (C) Microsoft Corporation. All rights reserved.
module hardware.dsp
    { fmul32
    , fadd32
    , fsub32
    , fmac32
    , fmad32
    , umul27
    , imul27
    }

import compiler.hal as hal
import compiler.config as config

inline float32 fmul32(float32 x, float32 y)
{
    static hal::fmul32<config::hal_device_family, 3> dsp;
    return dsp.op(x, y);
}

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

inline float32 fmac32(float32 x, float32 y, bool accumulate)
{
    static hal::fmac32<config::hal_device_family, 4> dsp;
    return dsp.op(x, y, accumulate);
}

inline float32 fmad32(float32 x, float32 y, float32 z)
{
    static hal::fmad32<config::hal_device_family, 4> dsp;
    return dsp.op(x, y, z);
}

inline uint54 umul27(uint27 x, uint27 y)
{
    static hal::umul27<config::hal_device_family, 2> dsp;
    return dsp.op(x, y);
}

inline int54 imul27(int27 x, int27 y, bool negate)
{
    static hal::imul27<config::hal_device_family, 2> dsp;
    return dsp.op(x, y, negate);
}