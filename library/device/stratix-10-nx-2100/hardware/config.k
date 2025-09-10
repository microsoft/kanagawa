// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Copyright (C) Microsoft Corporation. All rights reserved.

Device configuration for Intel Stratix 10 NX 2100 FPGA
*/
module hardware.config
    { module intel.stratix-10.config \ hardware.config
    , module hardware.config
    }

import intel.stratix-10.config

//| Version of device configuration schema
const auto version = 1;

//| Name of the device family that is significant to the Vendor RTL (or simulation libraries).
// This value is passed into HAL modules as a parameter, and in some cases passed down into
// the Vendor modules or used to enable or disable some conditional behavior.
const string hal_device_family = "Stratix10NX";

//| Unambiguous name of the target device
const string device_name = "stratix-10-nx-2100";

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
// FIFO
//

//| true if basic block thread rates should be used when computing fifo depths.
// When set to false, a thread rate of 1 is assumed for sizing. This feature is
// used to work around an EDA tool bug for certain Intel devices.
const bool use_thread_rate_in_fifo_sizing = false;

//
// Memory
//

//| Specifies the number of each type of memory resource that is available on the device.
const MemoryResource[2] memory_resources =
{
    {
        .type_id = MemoryType::LUT,
        .num_available = 18074 / 5 // MLABs / 5
    },
    {
        .type_id = MemoryType::Block,
        .num_available = 6847
    }
};
