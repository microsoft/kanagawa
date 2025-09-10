// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Compiler configuration parameters.

The Compiler uses constants defined in this module as configuration parameters
for code generation.

Values of the parameters are imported from compiler.config.device, which
combines default parameter values with device-specific values defined in
in a device-specific module hardware.device.

Your device, either directly or indirectly, MUST define values for each
of these constants.
*/
module compiler.config
    { module compiler.config
    }

import compiler.device.schema
import compiler.device.config as device

//| Version of device configuration schema
const auto version = device::version;

//| Unambiguous name of the target device
const string device_name = device::device_name;

//| Name of the vendor that manufactures the target device, for example 'Intel' or 'Xilinx'
const string vendor = device::vendor;

//| Name of the device family into which the target device belongs, for example 'Agilex 7'
const string device_family = device::device_family;

//| Name of the device family that is significant to the Vendor RTL (or simulation libraries).
// This value is passed into HAL modules as a parameter, and in some cases passed down into
// the Vendor modules or used to enable or disable some conditional behavior.
const string hal_device_family = device::hal_device_family;

// TODO: Remove before OSS
const bool trade_area_for_speed = device::trade_area_for_speed;

//
// DSP
//
const bool supports_dsp = (device::integer_mul_src_width != 0);

//| Width of source operands for an unsigned integer multiplication using a device DSP
const auto integer_mul_src_width = device::integer_mul_src_width;

//| Name of the intrinsic operation for 1 unsigned integer multiplication using a device DSP
const string unsigned_integer_mul_name = device::unsigned_integer_mul_name;

//| Name of the intrinsic operation for 1 signed integer multiplication using a device DSP
const string signed_integer_mul_name = device::signed_integer_mul_name;


//
// FIFO
//

//| Minimum depth for a single clock fifo
const auto min_fifo_depth = device::min_fifo_depth;

//| Minimum depth for a dual clock fifo
const auto min_dual_clock_fifo_depth = device::min_dual_clock_fifo_depth;

//| Minimum almost_full value for a fifo. This is the minimum value needed to overcome fifo
// read->almost_full latency
const auto min_almost_full_depth = device::min_almost_full_depth;

//| Minimum value for the almost empty threshold for a single-clock FIFO
const auto almost_empty_depth = device::almost_empty_depth;

//| Minimum width of LUTram based FIFO structures below which there are no area savings.
// Used in calculations for the local data propagation optimization
const auto fifo_width_alignment = device::fifo_width_alignment;

//| Minimum depth of LUTram based FIFO structures below which there are no area savings.
// Used in calculations for the local data propagation optimization
const auto fifo_depth_alignment = device::fifo_depth_alignment;

//| Fixed cost of a FIFO used in calculations for the local data propagation optimization
// Used in calculations for the local data propagation optimization
const auto fifo_fixed_cost = device::fifo_fixed_cost;

//| The number of bits in an implicit fifo that has the same area cost as 1 pipeline register.
// Used in calculations for the local data propagation optimization
const auto fifo_bits_per_register = device::fifo_bits_per_register;

//| `true` if it makes sense to use KanagawaInternalBufferFifo for basic blockswith
// start conditions as an Fmax optimization
const bool use_internal_buffer_fifo_optimization = device::use_internal_buffer_fifo_optimization;

//| `true` if FIFO depths should be round up to a power of 2
const bool fifo_depth_pow2 = device::fifo_depth_pow2;

//| true if basic block thread rates should be used when computing fifo depths.
// When set to false, a thread rate of 1 is assumed for sizing. This feature is
// used to work around an EDA tool bug for certain Intel devices.
const bool use_thread_rate_in_fifo_sizing = device::use_thread_rate_in_fifo_sizing;

//
// EDA tool auto-pipelining
//

//| true if the platform supports a feature where the EDA tool chooses the number of
// additional pipeline registers to add as needed to improve placement and routing
const bool supports_auto_pipelining = device::supports_auto_pipelining;

//| On platforms that support auto-pipelining, there is a mininum fixed depth of
// pipeline registers required.
const auto min_auto_pipeline_depth = device::min_auto_pipeline_depth;

//
// Miscellaneous
//

//| The Verilog pragma used to tell the synthesis tool not to merge a register with others
const string verilog_dont_merge_pragma_name = device::verilog_dont_merge_pragma_name;

//| true if the device supports LUTs (should be true for all FPGAs)
const bool supports_luts = device::supports_luts;

//| //| Multiple LUTs of this size can fit into 1 resource (ALM, CLB)
const auto small_lut_size = device::small_lut_size;

//| true if the platform supports a RAM-based shift register.
// Used in calculations for the local data propagation optimization
const bool lut_based_shift_register_available = device::lut_based_shift_register_available;

//| true if control signals should have power-on value specified
const bool registers_require_power_on_initial_value = device::registers_require_power_on_initial_value;

//
// Memory
//

//| If the block RAMs in the device support pipeline bypass (aka memory forwarding) for at least 2 stages
const bool block_ram_supports_hardened_bypass = device::block_ram_supports_hardened_bypass;

//| true if the device supports ECC for block RAMs
const bool block_ram_supports_ecc = device::block_ram_supports_ecc;

//| true if the device supports block RAMs in a true dual-port configuration (i.e. two independent read/write ports).
const bool block_ram_supports_true_dual_port = device::block_ram_supports_true_dual_port;

//| true if the device supports block RAMs in a simple quad-port configuration
const bool block_ram_supports_quad_port = device::block_ram_supports_quad_port;

//| For data propagation memories, the minimum width of 1 memory
const auto global_data_propagation_ram_alignment = device::global_data_propagation_ram_alignment;

//| The format to use for memory initialization files
const MemoryInitFileType memory_init_file_type = device::memory_init_file_type;

//| Valid memory configurations
const auto memory_configurations = device::memory_configurations;

//| Specifies the number of each type of memory resource that is available on the device.
const auto memory_resources = device::memory_resources;

//| This value is multiplied by the number of muxes necessary to implement an array when estimating.
// the relative cost of synthesizing a memory structure as an array (logic) versus using a RAM primitive.
const float32 array_mux_cost_factor = device::array_mux_cost_factor;

//| This value is multiplied by the number of registers necessary to implement an array when estimating.
// the relative cost of synthesizing a memory structure as an array (logic) versus using a RAM primitive.
const float32 array_register_cost_factor = device::array_register_cost_factor;

//| This value is multiplied by the number of write ports necessary to implement an array when estimating.
// the relative cost of synthesizing a memory structure as an array (logic) versus using a RAM primitive.
const float32 array_write_port_cost_factor = device::array_write_port_cost_factor;

const auto default_clock_frequency_mhz = 200;
