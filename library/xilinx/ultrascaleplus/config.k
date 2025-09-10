// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*!
Copyright (c), Microsoft Corporation. All rights reserved.

Device configuration with defaults for Xilinx UltraScale+ FPGAs
*/
module xilinx.ultrascaleplus.config
    { module xilinx.config \ xilinx.ultrascaleplus.config
    , module xilinx.ultrascaleplus.config
    }

import xilinx.config

//| Name of the device family into which the target device belongs, for example 'Agilex 7'
const string device_family = "ultrascaleplus";

//| Name of the device family that is significant to the Vendor RTL (or simulation libraries).
// This value is passed into HAL modules as a parameter, and in some cases passed down into
// the Vendor modules or used to enable or disable some conditional behavior.
const string hal_device_family = "ultrascaleplus";


//
// DSP
//

//| Width of source operands for an unsigned integer multiplication using a device DSP
const auto integer_mul_src_width = 18;

//| Name of the intrinsic operation for 1 unsigned integer multiplication using a device DSP
const string unsigned_integer_mul_name = "umul18";

//| Name of the intrinsic operation for 1 signed integer multiplication using a device DSP
const string signed_integer_mul_name = "imul18";

//
// FIFO
//

//| Minimum depth for a single clock fifo
const auto min_fifo_depth = 32;

//| Minimum depth for a dual clock fifo
const auto min_dual_clock_fifo_depth = 32;

//| Minimum almost_full value for a fifo. This is the minimum value needed to overcome fifo
// read->almost_full latency
const auto min_almost_full_depth = 5;

//| Minimum value for the almost empty threshold for a single-clock FIFO
const auto almost_empty_depth = 5;

//| Minimum width of LUTram based FIFO structures below which there are no area savings.
// Used in calculations for the local data propagation optimization
const auto fifo_width_alignment = 8;

//| Minimum depth of LUTram based FIFO structures below which there are no area savings.
// Used in calculations for the local data propagation optimization
const auto fifo_depth_alignment = 32;

//| Fixed cost of a FIFO used in calculations for the local data propagation optimization
// Used in calculations for the local data propagation optimization
const auto fifo_fixed_cost = 128;

//| The number of bits in an implicit fifo that has the same area cost as 1 pipeline register.
// Used in calculations for the local data propagation optimization
const auto fifo_bits_per_register = 16;

//| `true` if it makes sense to use KanagawaInternalBufferFifo for basic blockswith
// start conditions as an Fmax optimization
const bool use_internal_buffer_fifo_optimization = true;

//| true if basic block thread rates should be used when computing fifo depths.
// When set to false, a thread rate of 1 is assumed for sizing. This feature is
// used to work around an EDA tool bug for certain Intel devices.
const bool use_thread_rate_in_fifo_sizing = true;

//
// Miscellaneous
//

//| The Verilog pragma used to tell the synthesis tool not to merge a register with others
const string verilog_dont_merge_pragma_name = "dont_touch";

//| //| Multiple LUTs of this size can fit into 1 resource (ALM, CLB)
const auto small_lut_size = 6;

//| true if the platform supports a RAM-based shift register.
// Used in calculations for the local data propagation optimization
const bool lut_based_shift_register_available = true;

//| true if control signals should have power-on value specified
const bool registers_require_power_on_initial_value = false;

//
// Memory
//

//| If the block RAMs in the device support pipeline bypass (aka memory forwarding) for at least 2 stages
const bool block_ram_supports_hardened_bypass = false;

//| true if the device supports ECC for block RAMs
const bool block_ram_supports_ecc = true;

//| true if the device supports block RAMs in a true dual-port configuration (i.e. two independent read/write ports).
const bool block_ram_supports_true_dual_port = true;

//| For data propagation memories, the minimum width of 1 memory
const auto global_data_propagation_ram_alignment = 36;

//| Valid memory configurations
const MemoryConfiguration[12] memory_configurations =
{
    {
        .type_id = MemoryType::LUT,
        .cost = 2.0,
        .width = 8,
        .depth = 64
    },
    {
        .type_id = MemoryType::LUT,
        .cost = 2.0,
        .width = 16,
        .depth = 32
    },
    {
        .type_id = MemoryType::Block,
        .cost = 36.0,
        .width = 1,
        .depth = 16384
    },
    {
        .type_id = MemoryType::Block,
        .cost = 36.0,
        .width = 2,
        .depth = 8192
    },
    {
        .type_id = MemoryType::Block,
        .cost = 36.0,
        .width = 4,
        .depth = 4096
    },
    {
        .type_id = MemoryType::Block,
        .cost = 36.0,
        .width = 9,
        .depth = 2048
    },
    {
        .type_id = MemoryType::Block,
        .cost = 36.0,
        .width = 18,
        .depth = 1024
    },
    {
        .type_id = MemoryType::Block,
        .cost = 36.0,
        .width = 36,
        .depth = 512
    },
    {
        .type_id = MemoryType::Deep,
        .cost = 576.0,
        .width = 9,
        .depth = 32768
    },
    {
        .type_id = MemoryType::Deep,
        .cost = 576.0,
        .width = 18,
        .depth = 16384
    },
    {
        .type_id = MemoryType::Deep,
        .cost = 576.0,
        .width = 36,
        .depth = 8192
    },
    {
        .type_id = MemoryType::Deep,
        .cost = 576.0,
        .width = 72,
        .depth = 4096
    }
};
