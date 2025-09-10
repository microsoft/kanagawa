// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Copyright (C) Microsoft Corporation. All rights reserved.

Device configuration for Intel Agilex 7 F-023 FPGA
*/
module hardware.config
    { module intel.agilex-7.config \ hardware.config
    , module hardware.config
    }

import intel.agilex-7.config

//| Version of device configuration schema
const auto version = 1;


//| Unambiguous name of the target device
const string device_name = "agilex-7-f-023";

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
        .num_available = 39120 / 5 // MLABs / 5
    },
    {
        .type_id = MemoryType::Block,
        .num_available = 10464
    }
};

