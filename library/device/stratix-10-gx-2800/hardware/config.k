// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Copyright (C) Microsoft Corporation. All rights reserved.

Device configuration for Intel Stratix 10 GX 2800 FPGA
*/
module hardware.config
    { module intel.stratix-10.config \ hardware.config
    , module hardware.config
    }

import intel.stratix-10.config

//| Version of device configuration schema
const auto version = 1;

//| Unambiguous name of the target device
const string device_name = "stratix-10-gx-2800";

// TODO: remove before OSS
const bool trade_area_for_speed = true;

//
// Memory
//

//| Specifies the number of each type of memory resource that is available on the device.
const MemoryResource[2] memory_resources =
{
    {
        .type_id = MemoryType::LUT,
        .num_available = 11556 / 5 // MLABs / 5
    },
    {
        .type_id = MemoryType::Block,
        .num_available = 11721
    }
};
