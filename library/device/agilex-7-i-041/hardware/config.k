// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Copyright (C) Microsoft Corporation. All rights reserved.

Device configuration for Intel Agilex 7 I 041 FPGA
*/
module hardware.config
    { module intel.agilex-7.config \ hardware.config
    , module hardware.config
    }

import intel.agilex-7.config

//| Version of device configuration schema
const auto version = 1;

//| Unambiguous name of the target device
const string device_name = "agilex-7-i-041";

// TODO: remove before OSS
const bool trade_area_for_speed = true;

//
// DSP
//

// Note: This device does not have DSPs

//| Width of source operands for an unsigned integer multiplication using a device DSP
const auto integer_mul_src_width = 0;

//| Name of the intrinsic operation for 1 unsigned integer multiplication using a device DSP
const string unsigned_integer_mul_name = "";

//| Name of the intrinsic operation for 1 signed integer multiplication using a device DSP
const string signed_integer_mul_name = "";

//
// Memory
//

//| Specifies the number of each type of memory resource that is available on the device.
const MemoryResource[2] memory_resources =
{
    {
        .type_id = MemoryType::LUT,
        .num_available = 67808 / 5 // MLABs / 5
    },
    {
        .type_id = MemoryType::Block,
        .num_available = 17136
    }
};
