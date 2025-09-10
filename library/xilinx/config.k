// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*!
Copyright (c), Microsoft Corporation. All rights reserved.

Device configuration with defaults for Xilinx FPGAs
*/
module xilinx.config
    { module xilinx.config
    , module compiler.device.schema
    }

import compiler.device.schema

//| Name of the vendor that manufactures the target device, for example 'Intel' or 'Xilinx'
const string vendor = "Xilinx";

//| The format to use for memory initialization files
const MemoryInitFileType memory_init_file_type = MemoryInitFileType::Mem;

//
// Memory
//

//| true if the device supports block RAMs in a simple quad-port configuration
const bool block_ram_supports_quad_port = false;

//| This value is multiplied by the number of muxes necessary to implement an array when estimating.
// the relative cost of synthesizing a memory structure as an array (logic) versus using a RAM primitive.
const float32 array_mux_cost_factor = 0.125;

//| This value is multiplied by the number of registers necessary to implement an array when estimating.
// the relative cost of synthesizing a memory structure as an array (logic) versus using a RAM primitive.
const float32 array_register_cost_factor = 0.0;

//| This value is multiplied by the number of write ports necessary to implement an array when estimating.
// the relative cost of synthesizing a memory structure as an array (logic) versus using a RAM primitive.
const float32 array_write_port_cost_factor = 0.0625;

