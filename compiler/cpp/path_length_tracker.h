// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

// Register local register index to number of levels of logic required to compute the value of that register
typedef std::vector<uint32_t> RegPathLengthMap;

static void InitializeRegPathLengthMap(RegPathLengthMap& regPathLengthMap, const size_t registerCount)
{
    assert(regPathLengthMap.empty());

    // One entry per register
    regPathLengthMap.resize(registerCount);

    // All paths are initially empty
    std::fill(regPathLengthMap.begin(), regPathLengthMap.end(), 0);
}

// Computes the path length to execute a given operation taking into account the path length for all source operands
struct OperationPathLength
{
    size_t _maxInputLength;
    size_t _inputPlusOperationLength;
};

static OperationPathLength ComputeOperationPathLength(const RegPathLengthMap& regPathLength,
                                                      const std::vector<size_t>& srcRegisterIndices,
                                                      const size_t opPathLength)
{
    // Find the maximum path length over all inputs
    size_t maxSrcPathLength = 0;

    for (const size_t registerIndex : srcRegisterIndices)
    {
        const size_t srcPathLength = regPathLength[registerIndex];

        maxSrcPathLength = std::max(maxSrcPathLength, srcPathLength);
    }

    OperationPathLength result = {};

    result._maxInputLength = maxSrcPathLength;
    result._inputPlusOperationLength = maxSrcPathLength + opPathLength;

    return result;
}

// Updates a RegPathLengthMap to take into account the fact that 1 new operation executed
static void UpdateRegPathLength(RegPathLengthMap& regPathLength, const std::vector<size_t>& dstRegisterIndices,
                                const size_t newPathLength)
{
    assert(newPathLength < std::numeric_limits<uint32_t>::max());

    for (const size_t registerIndex : dstRegisterIndices)
    {
        regPathLength[registerIndex] = static_cast<uint32_t>(newPathLength);
    }
}

// Used to track path lengths through a basic block
class PathLengthTracker
{
  public:
    PathLengthTracker(const Program& program)
        : _program(program), _carryChainWidthPerLogicLevel(GetCodeGenConfig()._carryChainWidthPerLogicLevel),
          _totalDepth(0), _stagesDepth(0)

    {
        InitializeRegPathLengthMap(_pathLengthMap, program._registerTable.size());
    }

    size_t GetTotalPathLength() { return _totalDepth + _stagesDepth; }

    // Update path length information with 1 operation
    // Returns the path length leaving that operation
    size_t UpdateForward(const Operation& op)
    {
        std::vector<size_t> registersRead;
        std::vector<size_t> registersWritten;

        GetAccessedRegisters(op, registersRead, registersWritten);

        return Update(op, registersRead, registersWritten);
    }

    // Use this version when traversing a operation list in reverse order
    size_t UpdateBackward(const Operation& op)
    {
        std::vector<size_t> registersRead;
        std::vector<size_t> registersWritten;

        GetAccessedRegisters(op, registersRead, registersWritten);

        // Note that registers are passed in reverse order here
        return Update(op, registersWritten, registersRead);
    }

    size_t GetCarryChainWidth() { return _carryChainWidthPerLogicLevel; }

  private:
    size_t Update(const Operation& op, const std::vector<size_t>& inputSet, const std::vector<size_t>& outputSet)
    {
        const size_t opPathLength = GetOpPathLength(_program, op, _carryChainWidthPerLogicLevel);

        if (op._opcode == Opcode::Stage)
        {
            _stagesDepth += opPathLength * GetCodeGenConfig()._logicRegisterRatio;
        }

        const OperationPathLength opl = ComputeOperationPathLength(_pathLengthMap, inputSet, opPathLength);

        UpdateRegPathLength(_pathLengthMap, outputSet, opl._inputPlusOperationLength);

        _totalDepth = std::max(_totalDepth, opl._inputPlusOperationLength);

        return opl._inputPlusOperationLength + _stagesDepth;
    }

    const Program& _program;
    RegPathLengthMap _pathLengthMap;
    size_t _carryChainWidthPerLogicLevel;
    size_t _totalDepth;
    size_t _stagesDepth; // depth added by stage operations
};
