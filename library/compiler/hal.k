// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module compiler.hal
    { module compiler.hal
    }

template <string DEVICE_FAMILY, auto LATENCY>
class fmul32
{
public:
    [[latency(LATENCY)]]
    float32 op(float32 x, float32 y);
}

[[name("_hardware_dsp__fmul32")]]
extern fmul32;

template <string DEVICE_FAMILY, auto LATENCY>
class fadd32
{
public:
    [[latency(LATENCY)]]
    float32 op(float32 x, float32 y);
}

[[name("_hardware_dsp__fadd32")]]
extern fadd32;

template <string DEVICE_FAMILY, auto LATENCY>
class fsub32
{
public:
    [[latency(LATENCY)]]
    float32 op(float32 x, float32 y);
}

[[name("_hardware_dsp__fsub32")]]
extern fsub32;

template <string DEVICE_FAMILY, auto LATENCY>
class fmac32
{
public:
    [[latency(LATENCY)]]
    float32 op(float32 x, float32 y, bool accumulate);
}

[[name("_hardware_dsp__fmac32")]]
extern fmac32;

template <string DEVICE_FAMILY, auto LATENCY>
class fmad32
{
public:
    [[latency(LATENCY)]]
    float32 op(float32 x, float32 y, float32 z);
}

[[name("_hardware_dsp__fmad32")]]
extern fmad32;

template <string DEVICE_FAMILY, auto LATENCY>
class umul27
{
public:
    [[latency(LATENCY)]]
    uint54 op(uint27 x, uint27 y);
}

[[name("_hardware_dsp__umul27")]]
extern umul27;

template <string DEVICE_FAMILY, auto LATENCY>
class imul27
{
public:
    [[latency(LATENCY)]]
    int54 op(int27 x, int27 y, bool negate);
}

[[name("_hardware_dsp__imul27")]]
extern imul27;

template <string DEVICE_FAMILY, auto LATENCY>
class umul18x2
{
public:
    [[latency(LATENCY)]]
    uint72 op(uint36 x, uint36 y);
}

[[name("_hardware_dsp__umul18x2")]]
extern umul18x2;

template <string DEVICE_FAMILY, auto LATENCY>
class umul32
{
public:
    [[latency(LATENCY)]]
    uint64 op(uint32 x, uint32 y);
}

[[name("_hardware_dsp__umul32")]]
extern umul32;

template <string DEVICE_FAMILY, auto LATENCY>
class imul32
{
public:
    [[latency(LATENCY)]]
    int64 op(int32 x, int32 y, bool negate);
}

[[name("_hardware_dsp__imul32")]]
extern imul32;

template <string DEVICE_FAMILY, auto LATENCY>
class facc32
{
public:
    [[latency(LATENCY)]]
    float32 op(float32 x, bool accumulate);
}

[[name("_hardware_dsp__facc32")]]
extern facc32;

template <string DEVICE_FAMILY, auto LATENCY>
class addbf16x3
{
public:
    [[latency(LATENCY)]]
    uint48 op(uint48 x, uint48 y);
}

[[name("_hardware_dsp__addbf16x3")]]
extern addbf16x3;

template <string DEVICE_FAMILY, auto LATENCY>
class umul18
{
public:
    [[latency(LATENCY)]]
    uint36 op(uint18 x, uint18 y);
}

[[name("_hardware_dsp__umul18")]]
extern umul18;

template <string DEVICE_FAMILY, auto LATENCY>
class imul18
{
public:
    [[latency(LATENCY)]]
    int36 op(int18 x, int18 y);
}

[[name("_hardware_dsp__imul18")]]
extern imul18;
