// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright (C) Microsoft Corporation. All rights reserved.
module numeric.float32.operator
    { mul
    , add
    , sub
    , mad
    }

import numeric.float32.internal.operator as F32

//| Floating-point multiplication.
inline float32 mul(float32 x, float32 y)
{
    return F32::float32_mul_denormoff(x, y);
}

//| Floating-point addition.
inline float32 add(float32 x, float32 y)
{
    return F32::float32_add_denormoff(x, y);
}

//| Floating-point subtraction.
inline float32 sub(float32 x, float32 y)
{
    return F32::float32_sub_denormoff(x, y);
}

//| Floating-point multiply-accumulate.
inline float32 mad(float32 x, float32 y, float32 z)
{
    return F32::float32_add_denormoff(x, F32::float32_mul_denormoff(y, z));
}