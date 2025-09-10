// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Default values for device specific compiler configuration parameters.
*/
module compiler.device.config
    { module compiler.device.config \ hardware.config
    , module hardware.config
    }
import hardware.config

//| `true` if FIFO depths should be round up to a power of 2
const bool fifo_depth_pow2 = true;

//| true if the platform supports a feature where the EDA tool chooses the number of
// additional pipeline registers to add as needed to improve placement and routing
const bool supports_auto_pipelining = false;

//| On platforms that support auto-pipelining, there is a mininum fixed depth of
// pipeline registers required.
const auto min_auto_pipeline_depth = 0;

//| true if the device supports LUTs (should be true for all FPGAs)
const bool supports_luts = true;

// TODO: remove before OSS
const bool trade_area_for_speed = false;