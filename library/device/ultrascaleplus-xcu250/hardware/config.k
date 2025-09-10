// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Copyright (C) Microsoft Corporation. All rights reserved.

Device configuration for the Xilinx UltraScale+ XCU250‑L2FIGD2104E.
This is the device in the Xilinx U250 board.
*/
module hardware.config
    { module xilinx.ultrascaleplus.config \ hardware.config
    , module hardware.config
    }

import xilinx.ultrascaleplus.config

// Device configuration for the Xilinx UltraScale+ XCU250‑L2FIGD2104E.
// This is the device in the Xilinx U250 board.

//| Version of device configuration schema
const auto version = 1;

//| Unambiguous name of the target device
const string device_name = "ultrascaleplus-xcu250";

//
// Memory
//

//| Specifies the number of each type of memory resource that is available on the device.
const MemoryResource[3] memory_resources =
{
    {
        .type_id = MemoryType::LUT,
        .num_available = 98880 / 5 // SLICEMs / 5
    },
    {
        .type_id = MemoryType::Block,
        .num_available = 5376
    },
    {
        .type_id = MemoryType::Deep,
        .num_available = 1280
    }
};
