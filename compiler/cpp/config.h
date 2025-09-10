// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#include "options.h"

enum class MemoryInitFileType
{
    Mif,
    Mem,
    Asic
};

// Configurations that are board properties
struct CodeGenDeviceConfig
{
    std::string _deviceName;            // Unambiguous name of the target device
    std::string _vendor;                // Name of the vendor that manufactures the target device, for example 'Intel' or 'Xilinx'
    std::string _deviceFamily;          // Name of the device family into which the target device belongs, for example 'Agilex 7'
    std::string _halDeviceFamily;       // Name of the device family that is significant to the Vendor RTL (or simulation libraries).
    size_t _integerMulSrcWidth;          // width of source operands for 1 unsigned integer multiplication
    size_t _almostEmptyDepth;            // minimum value for fifo almost_empty
    std::string _unsignedIntegerMulName; // Name of the intrinsic DSP operation for 1 unsigned integer multiplication
    std::string _signedIntegerMulName;   // Name of the intrinsic DSP operation for 1 signed integer multiplication
    bool _largeAndFast;                  // trade area for speed
    std::string _verilogDontMergePragmaName;
    MemoryInitFileType _memoryInitFileType;
    bool _isDeepRamAvailable;
    size_t _smallLutSize; // multiple luts of this size can fit into 1 resource (ALM, CLB)
    bool _isLutBasedShiftRegisterAvailable;
    bool _supportsRmwBram;                    // true if bram have hardened forwarding hardware
    size_t _globalDataPropgationRamAlignment; // For data propagation memories, the minimum width of 1 memory
    size_t _minFifoDepth;                     // Minimum depth for a single clock fifo
    size_t _minDualClockFifoDepth;            // Minimum depth for a dual clock fifo
    size_t _minAlmostFullDepth; // Minimum almost_full value for a fifo. This is the minimum value need to overcome fifo
                                // read->almost_full latency.
    bool _supportsAutoPipelining;  // True if the platform supports a feature where the EDA tool chooses the number of
                                   // additional pipeline registers to add as needed to improve placement and routing.
    size_t _minAutoPipelineDepth;  // On platforms that support Auto-pipelining, there is a mininum fixed depth of
                                   // pipeline registers required.
    bool _supportsLuts;            // true if the device supports LUTs (true for FPGAs)
    bool _supportsTrueDualPortRam; // true if true dual-port RAM is supported
    bool _useInternalBufferFifoOptimization; // true if it makes sense to use KanagawaInternalBufferFifo for basic
                                             // blocks with start conditions as an Fmax optimization
    bool _fifoDepthPow2;                     // true FIFO depths should be round up to a power of 2
    bool _eccMemorySupport;                  // true if ECC is supported on memories
    bool _quadPortRamSupport;                // true if quad-port memories are supported
    bool _requirePowerOnReset;               // true if control signals should have power-on value specified
    bool _useThreadRateInFifoSizing; // true if basic block thread rates should be used when computing fifo depths

    size_t EstimatePipelineRegisterCost(size_t width, size_t distance) const;

    // To compute costs associated with local data propagation
    size_t _fifoWidthAlignment;
    size_t _fifoDepthAlignment;
    size_t _fifoFixedCost;
    size_t _fifoBitsPerRegister;

    // These values are used to calculate the cost of using an array relative to that of using memory
    float _arrayMuxCostFactor;
    float _arrayRegisterCostFactor;
    float _arrayWritePortCostFactor;

    size_t _defaultFreqMhz[MaxClockCount];

    struct MemoryConfig
    {
        struct width_depth_cost_t
        {
            width_depth_cost_t(size_t width = 0, size_t depth = 0, float cost = 0)
                : width(width), depth(depth), cost(cost)
            {
            }
            size_t width = 0;
            size_t depth = 0;
            float cost = 0;
        };
        size_t _numLutRam;
        std::array<width_depth_cost_t, 10> _lutRamConfigs;
        size_t _numBlockRam;
        std::array<width_depth_cost_t, 10> _blockRamConfigs;
        size_t _numDeepRam;
        std::array<width_depth_cost_t, 10> _deepRamConfigs;
    } _memory;

    bool _isInitialized;
};

struct CodeGenConfig : CodeGenOptions
{
    bool _isInitialized;

    size_t _frequency[MaxClockCount];

    // Number of bits that each ALM can store when used as memory
    size_t _bitsPerAlm;

    // Command line arguments used
    std::string _cmdArgs;

    // Name of the FPGA to target
    std::string _deviceName;

    // Name of the output module
    std::string _moduleName;

    std::vector<std::string> _fileNames;

    Placement::Parameters _placementConfig;

    bool PipelineClockGatingEnabled() const;

    bool ControlClockGatingEnabled() const;

    bool ClockGateBelowThreshold(const size_t numGatedBits) const;

    size_t MaximumWorkListSize() const;

    bool PathLengthReportEnabled() const;

    size_t GetMaxBypassSlots() const;

    size_t GetMinCrossRegionFifoWriteDelay() const;

    size_t GetInvocationIndexSize() const;

    size_t GetMaxSelectIndexOptRecursion() const;
};

void SetupCodeGenConfig(const Options& options);
void SetCodeGenDeviceConfig(const CodeGenDeviceConfig& deviceConfig);
void SetTestCodeGenDeviceConfig();
void AdjustCodeGenConfigForLargeAndFast();

const CodeGenConfig& GetCodeGenConfig();
const CodeGenDeviceConfig& GetCodeGenDeviceConfig();


