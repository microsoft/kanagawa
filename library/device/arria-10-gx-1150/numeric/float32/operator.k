// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright (C) Microsoft Corporation. All rights reserved.
module numeric.float32.operator
    { mul
    , add
    , sub
    , mad
    }

import hardware.dsp

//| Floating-point multiplication.
inline float32 mul(float32 x, float32 y)
{
    return fmul32(x, y);
}

//| Floating-point addition.
inline float32 add(float32 x, float32 y)
{
    return fadd32(x, y);
}

//| Floating-point subtraction.
inline float32 sub(float32 x, float32 y)
{
    return fsub32(x, y);
}

//| Floating-point multiply-accumulate.
inline float32 mad(float32 x, float32 y, float32 z)
{
    return fmad32(x, y, z);
}