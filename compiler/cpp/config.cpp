// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

CodeGenConfig g_codeGenConfig = {};
CodeGenDeviceConfig g_codeGenDeviceConfig = {};

// This is a dummy device configuration that is used for internal compiler testing in situations
// where the externally defined device configuration is not available.
CodeGenDeviceConfig g_TestDeviceConfig = {
    "TestDevice", // _deviceName
    "Microsoft",  // _vendor
    "TestFamily", // _deviceFamily
    "TestFamily", // _halDeviceFamily
    0,      // _integerMulSrcWidth
    4,      // _almostEmptyDepth
    "",     // _unsignedIntegerMulName
    "",     // _signedIntegerMulName
    false,  // _largeAndFast
    "dont_merge",     // _verilogDontMergePragmaName
    MemoryInitFileType::Mif,  // _memoryInitFileType
    false,  // _isDeepRamAvailable
    4,      // _smallLutSize
    false,  // _isLutBasedShiftRegisterAvailable
    true,   // _supportsRmwBram
    40,     // _globalDataPropgationRamAlignment
    32,     // _minFifoDepth
    32,     // _minDualClockFifoDepth
    4,      // _minAlmostFullDepth
    false,  // _supportsAutoPipelining
    0,      // _minAutoPipelineDepth
    true,   // _supportsLuts
    true,   // _supportsTrueDualPortRam
    true,   // _useInternalBufferFifoOptimization
    true,   // _fifoDepthPow2
    true,   // _eccMemorySupport
    true,   // _quadPortRamSupport
    true,   // _requirePowerOnReset
    true,   // _useThreadRateInFifoSizing
    20,     // _fifoWidthAlignment
    32,     // _fifoDepthAlignment
    128,    // _fifoFixedCost
    16,     // _fifoBitsPerRegister
    0.5f,   // _arrayMuxCostFactor
    0.0f,   // _arrayRegisterCostFactor
    0.25f,  // _arrayWritePortCostFactor
    {200},  // _defaultFreqMhz[MaxClockCount]
    {
        // Stratix 10 GX 1100 values
        11556 / 5,
        {{{20, 32, 10.0f}}},
        11721,
        {{
        {10, 2048, 640.0f},
        {20, 1024, 320.0f},
        {40, 512, 320.0f}}}, // _blockRamConfigs
        0,                     // _numDeepRam
        {},                    // _deepRamConfigs
    },
    true   // _isInitialized
};


size_t CodeGenDeviceConfig::EstimatePipelineRegisterCost(size_t width, size_t distance) const
{
    return width * distance / 4;
}

bool CodeGenConfig::PipelineClockGatingEnabled() const { return _clockGatingLevel > 0; }

bool CodeGenConfig::ControlClockGatingEnabled() const { return _clockGatingLevel > 1; }

bool CodeGenConfig::ClockGateBelowThreshold(const size_t numGatedBits) const
{
    return numGatedBits <= _clockGatingThreshold;
}

bool CodeGenConfig::PathLengthReportEnabled() const { return !_debug; }

size_t CodeGenConfig::GetMaxBypassSlots() const
{
    // 2 slot the fundamental read-modify-write loop
    // N for read delay
    // M for write delay
    return 2 + _rmwMemoryReadDelay + std::max(_rmwMemoryWriteDelay, _rmwHardenedMemoryWriteDelay);
}

size_t CodeGenConfig::GetMinCrossRegionFifoWriteDelay() const
{
    const auto& deviceConfig = GetCodeGenDeviceConfig();
    return deviceConfig._supportsAutoPipelining ? deviceConfig._minAutoPipelineDepth : _autoPipelineCrossRegion;
}

size_t CodeGenConfig::MaximumWorkListSize() const
{
    size_t logResult = _logWorkListSize;

    if (0 == logResult)
    {
        // Select default value based on optimization level
        if (_optimize < 2)
        {
            logResult = 16;
        }
        else
        {
            logResult = 20;
        }
    }

    if (logResult >= 64)
    {
        throw std::runtime_error("Work list size is too large");
    }

    return 1ull << logResult;
}

size_t CodeGenConfig::GetInvocationIndexSize() const { return Log2(_maxThreadsLimit); }

size_t CodeGenConfig::GetMaxSelectIndexOptRecursion() const { return _optimize > 1 ? 32 : 4; }

void SetCodeGenDeviceConfig(const CodeGenDeviceConfig& deviceConfig)
{
    g_codeGenDeviceConfig = deviceConfig;
}

void SetTestCodeGenDeviceConfig()
{
    SetCodeGenDeviceConfig(g_TestDeviceConfig);
}

void SetupCodeGenConfig(const Options& options)
{
    *static_cast<CodeGenOptions*>(&g_codeGenConfig) = options._codegenOptions;

    g_codeGenConfig._isInitialized = true;

    // Inspectables external  interface currently disabled for FOSS
    g_codeGenConfig._inspection = false;
    g_codeGenConfig._controlInspection = false;
    g_codeGenConfig._detectRaces = false;

    g_codeGenConfig._cmdArgs = options._cmdArgs;
    g_codeGenConfig._frequency[0] = options._frequency;
    g_codeGenConfig._deviceName = options._deviceName;
    g_codeGenConfig._moduleName = "KanagawaCore";
    g_codeGenConfig._placementConfig = options._placementOptions;
    g_codeGenConfig._bitsPerAlm = 64; // each MLAB holds 640 bits, and consumes 10 ALMs

    for (const char** p = options._fileNames; *p; ++p)
        g_codeGenConfig._fileNames.push_back(*p);

    // CodeGenDeviceConfig comes from Kanagawa sources and is now set later in the compiler processing of ParseTreeNode

    if (g_codeGenConfig._verbosity > 1)
    {
        std::cout << "Device name: " << g_codeGenConfig._deviceName << "\n";
    }

    if (g_codeGenConfig._resetFanOutCycles > g_codeGenConfig._resetCycles)
    {
        throw std::runtime_error("--reset-fan-out-cycles must be no greater than --reset-cycles");
    }

    if (g_codeGenConfig._logicRegisterRatio < 1)
    {
        throw std::runtime_error("Logic to register ratio must be at least 1");
    }

    if (g_codeGenConfig._fileNames.empty())
    {
        throw std::runtime_error("No input files specified");
    }

    // No clock gating is allowed before we implement it in CIRCT
    if (g_codeGenConfig.PipelineClockGatingEnabled())
    {
        throw std::runtime_error("Invalid -- clock gating level must be equal to 0");
    }
    // No stallable pipeline is allowed before we implement it in CIRCT
    if (g_codeGenConfig._stallablePipelines)
    {
        throw std::runtime_error("Invalid -- stallable pipeline feature must be disabled");
    }

    // Speed up compile time by using random placement when the optimization level is less than 2
    if (g_codeGenConfig._optimize < 2)
    {
        g_codeGenConfig._placementConfig._numIterations = 1;
    }

    if (g_codeGenConfig._stall > 4)
        throw std::runtime_error("Invalid --stall value not in [0,4]");

    if (g_codeGenConfig._maxSelectInputs < 2)
        throw std::runtime_error("--max-mux-sources must be at least 2");

    if (!IsPow2(g_codeGenConfig._maxSelectInputs))
        throw std::runtime_error("--max-mux-sources must be a power of 2");

    if (!IsPow2(g_codeGenConfig._maxThreadsDefault))
        throw std::runtime_error("--max-threads-default must be a power of 2");

    if (!IsPow2(g_codeGenConfig._maxThreadsLimit))
        throw std::runtime_error("--max-threads-limit must be a power of 2");
}

const CodeGenConfig& GetCodeGenConfig()
{
    // Verify SetupCodeGenConfig has been called
    assert(g_codeGenConfig._isInitialized);

    return g_codeGenConfig;
}

const CodeGenDeviceConfig& GetCodeGenDeviceConfig()
{
    // Verify device configuration has been extracted
    assert(g_codeGenDeviceConfig._isInitialized);

    return g_codeGenDeviceConfig;
}

void AdjustCodeGenConfigForLargeAndFast()
{
    // Add 1 more cycle of latency to all fifo writes, to ease timing
    g_codeGenConfig._fifoWriteDelay++;

    // Add 1 more cycle of latency to reset network, to ease timing
    g_codeGenConfig._resetCycles++;

    // Decompose add/sub operations more aggressively
    g_codeGenConfig._carryChainWidthPerLogicLevel /= 2;

    // Add more pipeline stages for almost_full signals
    g_codeGenConfig._additionalLatency++;
}

VerbosityLevel Verbosity() { return static_cast<VerbosityLevel>(g_codeGenConfig._verbosity); }
