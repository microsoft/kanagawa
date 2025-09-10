// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*!
Copyright (c), Microsoft Corporation. All rights reserved.

Device configuration with defaults for Intel FPGAs
*/
module intel.config
    { module intel.config
    , module compiler.device.schema
    }

import compiler.device.schema

//| Name of the vendor that manufactures the target device, for example 'Intel' or 'Xilinx'
const string vendor = "Intel";

//| The format to use for memory initialization files
const MemoryInitFileType memory_init_file_type = MemoryInitFileType::Mif;

//
// Miscellaneous
//

//| The Verilog pragma used to tell the synthesis tool not to merge a register with others
const string verilog_dont_merge_pragma_name = "dont_merge";

//
// Memory
//

//| true if the device supports block RAMs in a simple quad-port configuration
const bool block_ram_supports_quad_port = true;

//| This value is multiplied by the number of muxes necessary to implement an array when estimating.
// the relative cost of synthesizing a memory structure as an array (logic) versus using a RAM primitive.
const float32 array_mux_cost_factor = 0.5;

//| This value is multiplied by the number of registers necessary to implement an array when estimating.
// the relative cost of synthesizing a memory structure as an array (logic) versus using a RAM primitive.
const float32 array_register_cost_factor = 0.0;

//| This value is multiplied by the number of write ports necessary to implement an array when estimating.
// the relative cost of synthesizing a memory structure as an array (logic) versus using a RAM primitive.
const float32 array_write_port_cost_factor = 0.25;

