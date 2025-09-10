// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif

    enum VerbosityLevel
    {
        Quiet,
        Normal,
        Verbose
    };

    typedef unsigned BOOL;

    typedef struct
    {
        // The number of iterations to run for
        size_t _numIterations;

        // Random number seed
        size_t _seed;

        // Coefficient of linear force that pulls connected nodes together
        float _pullStrength;

        // Coefficient of inverse square force that pushes nodes away from others
        float _pushStrength;

        // Coefficient of inverse cube force that pushes nodes away from walls
        float _wallStrength;

        // Maximum length of gradient vectors
        float _updateRate;

        // True to animate the placement process
        BOOL _display;
    } PlacementOptions;

    typedef struct
    {
        // True to ensure that generated code runs in the same order as the source code
        // For debugging
        BOOL _debug;

        // True to generate DebugSymbols with stripped file names
        BOOL _stripDebugSymbols;

        // Optimization level
        size_t _optimize;

        // True to add assertions to the IR to verify that narrowing conversions do not lose data
        BOOL _assertNarrowingConversion;

        // True to dump IR before/after optimization
        BOOL _dumpIR;

        // True to dump IR after each optimization step
        BOOL _dumpOpt;

        // True to dump IR to json after all optimizations
        BOOL _serializeIRPostOpt;

        // True to dump IR to json after all optimizations
        BOOL _serializeCIRCTIR;

        // true to generate hardware to detect race conditions
        BOOL _detectRaces;

        // true to generate hardware to inspect control state
        BOOL _controlInspection;

        // true to generate hardware to inspect
        BOOL _inspection;

        // true to enable assertions on release builds of the compiler
        BOOL _releaseAssert;

        // true to allow fifos to stutter when they are almost empty (to improve timing)
        BOOL _allowFifoStutter;

        // true to generate pipelines where individual stages may be stalled (to reduce area)
        BOOL _stallablePipelines;

        // Level of clock gating to apply (0 disables, 1 is per-stage, 2 is full clock gating)
        size_t _clockGatingLevel;

        // Only instantiate a clock gate if it applies to at more than this many register bits
        size_t _clockGatingThreshold;

        // For memory reads that are conditional, try and make the memory read enable signal conditional as well.
        // This can lower power consumption, but in some cases may negatively impact the maximum clock frequency.
        BOOL _conditionalMemoryReads;

        // Controls the number of pipeline registers inserted in the path of the input signals that drive the semaphore
        // module used to limit threads within a pipeline. This is a trade off between clock frequency and power
        // consumption (lower values mean less extra registers and hence less power and less area).
        size_t _semaphoreDelay;

        // true to generate hardware for code coverage
        BOOL _codeCoverage;

        // If code coverage is enabled, only generate code coverage hardware for muxes with less than this number of
        // cases A value of 0 means no limit.
        size_t _codeCoverageMuxThreshold;

        // true to generate metadata files for memories and fifos
        BOOL _dumpMemoryMetadata;

        // Maximum number of each RAM resource allowed
        size_t _maxBlockRam;
        size_t _maxDeepRam;

        // Minimum utilization of RAM resources
        // 0 means 0%, 100 means 100%
        size_t _minBlockRamUtilization;
        size_t _minDeepRamUtilization;

        // Decompose mux with more than this many inputs
        size_t _maxSelectInputs;

        // Decompose multiplication operations with operands wider than this
        size_t _maxMulSrcWidth;

        // Max thread count for functions when max count is not specified
        size_t _maxThreadsDefault;

        // Max thread count for any function (compilation fails if a function could have a thread count greater than
        // this)
        size_t _maxThreadsLimit;

        // Limit on the count of instructions to be scheduled
        size_t _scheduleLimit;

        // Extra latency added to to ease routing
        size_t _additionalLatency;

        // Additional clock cycles to add to the reset sequence
        // Hyper-retiming can require additional cycles be added
        size_t _additionalResetCycles;

        // A maximum number of additional pipeline registers that can be added to function entry points.
        // On platforms that support it, the synthesis tool is allowed to choose the number of registers.
        // On platforms a fixed number of registers will be used.
        //    0 means no auto-pipelining
        //    2 means that each register can be turned into 2 if necessary
        size_t _autoPipelineScale;

        // A maximum number of additional pipeline registers that can be added to cross-region function entry points.
        // On platforms that support it, the synthesis tool is allowed to choose the number of registers.
        // On platforms a fixed number of registers will be used.
        //    0 means no auto-pipelining
        //    2 means that each register can be turned into 2 if necessary
        size_t _autoPipelineCrossRegion;

        // 1 causes pipeline registers to be inserted after each level of combinational logic
        // 4 causes pipeline registers to be inserted after every 4 levels of combinational logic
        size_t _logicRegisterRatio;

        // Atomic blocks can relax scheduling up to this register ratio
        size_t _maxLogicRegisterRatio;

        // Number of cycles to propagate the reset signal
        size_t _resetCycles;

        // Subset of _resetCycles to spend in a fan-out tree
        size_t _resetFanOutCycles;

        // Number of cycles to add to fifo writes
        size_t _fifoWriteDelay;

        // Max distance between enqueue operations when merging fifos during local data propagation
        size_t _fifoMergeDistance;

        // The number of output bits in a carry-chain operation (add/sub) that is considered to be 1 level of logic
        size_t _carryChainWidthPerLogicLevel;

        // Write delay to add to memories that are accessed via read-modify-write
        size_t _rmwMemoryWriteDelay;

        // Read delay to add to memories that are accessed via read-modify-write
        size_t _rmwMemoryReadDelay;

        // Write delay to add to memories that are accessed via read-modify-write
        // In cases where hardened read-modify-write logic can be used
        size_t _rmwHardenedMemoryWriteDelay;

        // In cases where hardened BRAM read-modify-write logic could be used,
        // don't use the hardened logic when the width is > _rmwHardenedMemoryMaxWidth
        // AND height is <= _rmwHardenedMemoryMaxHeight to lessen
        // BRAM requirement/underutilization.
        size_t _rmwHardenedMemoryMaxWidth;
        size_t _rmwHardenedMemoryMaxHeight;

        // Optimize read-modify-write memory access within atomic block for area.
        // By default optimize for Fmax.
        BOOL _rmwMemoryFavorArea;

        // Require that if a memory read and write (access the same address) occur
        // at the same time, that the read will see the old data
        // If FALSE, then let the synthesis tool decide
        BOOL _strictMemoryReadWrite;

        // Delay to account for routing data to/from DSP/BRAM input/output registers (measured in levels of logic)
        size_t _inputOutputRegisterDelay;

        // Threshold for moving variables from registers into memories
        // Measured in memory bits per register bit
        // A value of 0 means variables should always stay in registers
        // A value of N means that a single register should be moved to a memory if
        // the number of memory bits is < N
        float _memToRegThreshold;

        // Threshold for replacing local data propagation FIFO with DSP
        // Measured in number of pipeline stages that FIFO replaces
        // A value of N means that FIFOs for number of pipeline stages <= N should
        // be replaced with DSPs
        size_t _fifoToDspThreshold;

        size_t _verbosity;

        // For limiting string length in generated code
        size_t _maxStringLength;

        // Randomly force each basic block to stall
        size_t _stall;

        // Convert all memories to arrays (ignoring area costs)
        size_t _forceMemToArray;

        // Don't emit debug view - currently used for code coverage tests
        BOOL _suppressDebugView;

        // Maximum optimizer work list size
        // defaults to 0
        size_t _logWorkListSize;

        // Extra pipeline stages to add functions with multiple return sites
        size_t _returnStages;

        // Maximum depth of call stack to write into generated RTL
        size_t _maxStackDepth;

        // The reset fanout is enabled
        BOOL _resetFanout;

        // Remove unnecessary/constant bits from registers
        BOOL _sparseRegOpt;

        // Reduce pipeline length with replicated hardware that performs speculative computations
        BOOL _carrySelect;

        // Don't output the informational header on top of generated Verilog files
        BOOL _noVerilogHeader;

        // True to enable warnings about missing [[transaction_size]]
        BOOL _enableTransactionSizeWarning;

        // True to enable warnings about deprecated features
        BOOL _enableDeprecationWarnings;

        // True to treat warnings as errors
        BOOL _warningsAsErrors;

    } CodeGenOptions;

    typedef struct
    {
        // Role clock frequency (in MHz) (clock 0)
        size_t _frequency;

        // Name of the FPGA to target
        const char* _deviceName;

        // Command line arguments used
        const char* _cmdArgs;

        CodeGenOptions _codegenOptions;

        PlacementOptions _placementOptions;

        const char** _fileNames;

    } Options;

#ifdef __cplusplus
};
#endif
