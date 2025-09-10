// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Compiler data types for device configuration.
*/
module compiler.device.schema
    { module compiler.device.schema
    }

const auto schema_version = 1;

//| Device memory types
enum MemoryType : uint3
{
    //| LUTram / MLAB RAM
    LUT = 0,

    //| Block RAMs, for example M20K on Intel FPGA
    Block = 1,

    //| Deep RAMs, for example URAM on a Xilinx FPGA
    Deep = 2
}

//| Specifies the number of each type of RAM provide by a device
struct MemoryResource
{
    //| Type of memory resource
    MemoryType type_id;

    //| Number of the memory resources available. 0 here means compiler will not enforce a limit.
    uint32 num_available;
}

//| RAM types and configurations supported by the device
struct MemoryConfiguration
{
    //| Type of memory resource
    MemoryType type_id;

    //| Cost of the memory resource. Used in a comparison with array cost to determine
    // whether to use an array (soft logic) or a physical memory
    float32 cost;

    //| Width of the memory resource
    uint16 width;

    //| Depth of the memory resource
    uint16 depth;
}

//| Formats for the memory initialization file
enum MemoryInitFileType : uint2
{
    //| Memory Initilization File format. Used with Intel/Altera FPGAs.
    Mif = 0,

    //| Memory Data File format. Used with Xilinx FPGAs.
    Mem = 1
}
