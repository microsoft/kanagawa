// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Copyright (C) Microsoft Corporation. All rights reserved.

Device configuration for Intel Arria 10 GX 1150 FPGA
*/
module hardware.config
    { module intel.arria-10.config \ hardware.config
    , module hardware.config
    }

import intel.arria-10.config

//| Version of device configuration schema
const auto version = 1;

//| Unambiguous name of the target device
const string device_name = "arria-10-gx-1150";

//
// Memory
//

//| Specifies the number of each type of memory resource that is available on the device.
const MemoryResource[2] memory_resources =
{
    {
        .type_id = MemoryType::LUT,
        .num_available = 20774 / 5 // MLABs / 5
    },
    {
        .type_id = MemoryType::Block,
        .num_available = 2713
    }
};
