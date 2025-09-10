// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

#include <condition_variable>
#include <mutex>
#include <thread>

struct DestinationRegister
{
    size_t _stageSequence;
    size_t _registerIndex;

    bool operator<(const DestinationRegister& dst) const { return _stageSequence < dst._stageSequence; }
};

// Maps original register to set of registers used after the first propagation
using RenamedRegisterMap = std::map<size_t, std::set<DestinationRegister>>;

void GetDataPropagationAtUse(const Program& program, const size_t pipelineStageIndex, const size_t registerIndex,
                             std::map<size_t, size_t>& recentAccessStages, Propagations& propagations)
{
    auto it = recentAccessStages.find(registerIndex);
    assert(it != recentAccessStages.end());

    const size_t previousAccessStage = it->second;

    if ((previousAccessStage + 2) < pipelineStageIndex)
    {
        const RegisterDescription& regDesc = program._registerTable[registerIndex];
        const size_t width = regDesc._width;

        const PropagationRange range(previousAccessStage, pipelineStageIndex - 1);

        assert(range.second > range.first);

        propagations[range].insert(registerIndex);
    }

    // Update recent access stage
    // It is possible that both def and use are in the same stage.
    // std::max prevents overwriting the restriction imposed by def
    it->second = std::max(it->second, pipelineStageIndex);
}

// Find data propagation within a basic block
Propagations GetDataPropagations(const Program& program, BasicBlock& basicBlock, RegisterSetMap& liveInMap,
                                 const std::unordered_set<const BasicBlock*>& returnSites)
{
    Propagations propagations;

    // Maps register index to pipeline stage index of the most recent def or use
    std::map<size_t, size_t> recentAccessStages;

    // find all local variables that are live-in to the basic block
    for (const size_t r : liveInMap[&basicBlock])
    {
        assert(RegisterType::Local == program._registerTable[r]._type);

        // Basic blocks without a start condition will have a pipeline stage
        // added to read fifo outputs for timing reasons (see AddPipelineRegisters())
        // thus any enqueueregisters in stage 0 will be pushed out to stage 1.
        // For BBs with a start condition, require that the earliest stage to
        // contain enqueueregisters to be stage 1 directly.
        // This treats both BBs consistently
        recentAccessStages[r] = (basicBlock.HasStartCondition() ? 1 : 0);
    }

    std::vector<PipelineStage> pipelineStages = GetPipelineStages(basicBlock);

    for (size_t pipelineStageIndex = 0; pipelineStageIndex < pipelineStages.size(); ++pipelineStageIndex)
    {
        RegisterSet liveInToSuccessor;

        for (Stage* const subStage : pipelineStages[pipelineStageIndex])
        {
            for (const Operation& op : subStage->_operations)
            {
                for (const DestinationOperand& dstOp : op._dst)
                {
                    if (DestinationOperandType::Register == dstOp.Type())
                    {
                        const AccessedRegister& reg = dstOp.GetAccessedRegister();

                        recentAccessStages[reg._registerIndex] = pipelineStageIndex + 1;
                    }
                }

                // Enqueue operation - Save variables that are live-in to successor
                if (op._getSuccessorBlock)
                {
                    GetLiveInToSuccessor(op, basicBlock, liveInMap, returnSites, liveInToSuccessor);
                }
                else
                {
                    // Check source operands for possible propagations
                    for (const SourceOperand& srcOp : op._src)
                    {
                        if (srcOp.Type() == SourceOperandType::Register)
                        {
                            const AccessedRegister& reg = srcOp.GetAccessedRegister();
                            const RegisterDescription& regDesc = program._registerTable[reg._registerIndex];

                            if (regDesc._type == RegisterType::Local)
                            {
                                GetDataPropagationAtUse(program, pipelineStageIndex, reg._registerIndex,
                                                        recentAccessStages, propagations);
                            }
                        }
                    }
                }
            }

            // Check for possible fifo propagations for variables that are live-in to successors of enqueue operations
            for (const size_t registerIndex : liveInToSuccessor)
            {
                GetDataPropagationAtUse(program, pipelineStageIndex, registerIndex, recentAccessStages, propagations);
            }
        }
    }

    return propagations;
}

size_t PropagationRangeDepth(const PropagationRange& range)
{
    assert(range.second >= range.first);

    return range.second - range.first;
}

FifoPropagations OptimizePropagations(const Propagations& inputPropagations, const RegToWidth& regToWidth)
{
    // (PropagationRange, std::set<size_t>)
    using RangeAndRegisters = Propagations::value_type;

    // Flatten input propagations into a vector
    const std::vector<RangeAndRegisters> flatInputPropagations(inputPropagations.begin(), inputPropagations.end());

    const CodeGenDeviceConfig& config = GetCodeGenDeviceConfig();

    const auto getRegisterCost = [&](const size_t depth, const size_t width) -> size_t
    { return depth * width * config._fifoBitsPerRegister; };

    const auto getTrailingRegisterCost = [&](const PropagationRange& registerRange, const size_t fifoDepth,
                                             const size_t width) -> size_t
    {
        const size_t registerRangeDepth = PropagationRangeDepth(registerRange);

        assert(registerRangeDepth >= fifoDepth);

        return getRegisterCost(registerRangeDepth - fifoDepth, width);
    };

    const auto getFifoCost = [&](const size_t depth, const size_t width) -> size_t
    {
        return (AlignNonPow2(width, config._fifoWidthAlignment) * AlignNonPow2(depth, config._fifoDepthAlignment)) +
               config._fifoFixedCost;
    };

    const auto getUnalignedFifoCost = [&](const size_t depth, const size_t width) -> size_t { return width * depth; };

    // Returns the sum of all register widths in the set of registers
    // associated with a propagation
    const auto getRangeWidth = [&](const RangeAndRegisters& rar)
    {
        size_t result = 0;

        for (const size_t registerIndex : rar.second)
        {
            result += regToWidth(registerIndex);
        }

        return result;
    };

    // Each element of flatInputPropagations corresponds to an array of possible PropagationDesc
    // The optimization process chooses 1 PropagationDesc for each element of flatInputPropagations
    struct PropagationDesc
    {
        // True if some range of the input propagation is in a FIFO
        bool _inFifo;

        // Key to group propagations
        AnchorAndDepth _anchorAndDepth;
    };

    // Enumerate all choices
    using PropagationDescVector = std::vector<PropagationDesc>;

    std::vector<size_t> parameterRanges(flatInputPropagations.size());
    std::vector<PropagationDescVector> propagationDescs(flatInputPropagations.size());

    const size_t maxMergeDistance = GetCodeGenConfig()._fifoMergeDistance;

    for (size_t i = 0; i < flatInputPropagations.size(); i++)
    {
        const PropagationRange& inputRange = flatInputPropagations[i].first;

        const size_t width = getRangeWidth(flatInputPropagations[i]);

        const size_t inputDepth = PropagationRangeDepth(inputRange);

        const size_t allRegisterCost = getRegisterCost(inputDepth, width);

        const size_t allFifoCost = getFifoCost(inputDepth, width);

        const size_t costUpperBound = std::min(allRegisterCost, allFifoCost);

        PropagationDescVector& pdv = propagationDescs[i];
        assert(pdv.empty());

        // 1 option is to implement the whole propagation with registers
        {
            const PropagationDesc noFifoPd = {false};
            pdv.push_back(noFifoPd);
        }

        // Other options put a subset of the propagation into FIFOs
        const size_t firstAnchorStage = inputRange.first >= maxMergeDistance ? inputRange.first - maxMergeDistance : 0;
        const size_t lastAnchorStage = inputRange.first + maxMergeDistance;

        for (size_t anchorStage = firstAnchorStage; anchorStage <= lastAnchorStage; anchorStage++)
        {
            // Lower depth values cannot be handled by the fixed delay fifo
            for (size_t depth = 3; depth <= inputDepth; depth++)
            {
                // Don't consider configurations where the cost of the trailing registers
                // are more than the cost of just putting the input range in a FIFO by itself
                const size_t leadingTrailingCost = getTrailingRegisterCost(inputRange, depth, width);

                if (leadingTrailingCost < costUpperBound)
                {
                    const PropagationDesc pd = {true, {anchorStage, depth}};
                    pdv.push_back(pd);
                }
            }
        }

        parameterRanges[i] = pdv.size();
    }

    // Determine bounds for each propagation
    // these bounds are used when the corresponding parameter is undefined
    std::vector<ObjectiveFunctionBounds> precomputedBounds(flatInputPropagations.size());

    const size_t freeLoadWidth = config._fifoWidthAlignment - 1;

    for (size_t i = 0; i < flatInputPropagations.size(); i++)
    {
        const PropagationRange& inputRange = flatInputPropagations[i].first;

        const size_t width = getRangeWidth(flatInputPropagations[i]);

        const PropagationDescVector& pdv = propagationDescs[i];

        ObjectiveFunctionBounds& bounds = precomputedBounds[i];

        const size_t registerCost = getRegisterCost(PropagationRangeDepth(inputRange), width);

        const size_t fifoCost = getFifoCost(PropagationRangeDepth(inputRange), width);

        const size_t freeLoadWidth = config._fifoWidthAlignment - 1;

        if (width <= freeLoadWidth)
        {
            // In the best case, the input propagation will only exist in unused space
            // in FIFOs required by other input propagations
            bounds._lowerBound = 0;
        }
        else
        {
            // In the best case, "config._fifoWidthAlignment - 1" bits will be exist in unused space
            // in FIFOs required by other input propagations
            const size_t narrowFifoCost =
                getUnalignedFifoCost(PropagationRangeDepth(inputRange), width - freeLoadWidth);

            bounds._lowerBound = std::min(registerCost, narrowFifoCost);
        }

        // In the worst case, the input propagation will not share a FIFO with any other
        // input propagation
        bounds._upperBound = std::min(registerCost, fifoCost);

        assert(bounds._upperBound >= bounds._lowerBound);
    }

    FifoPropagations finalResult;

    const auto boundCallback = [&](const IntegerParameterVector& paramVec,
                                   const bool isOptimalPoint) -> ObjectiveFunctionBounds
    {
        assert(paramVec.GetParameterCount() == flatInputPropagations.size());
        assert(finalResult.empty());

        ObjectiveFunctionBounds result = {};

        // Maps anchor stage to width
        std::map<AnchorAndDepth, size_t> fifoWidths;

        for (size_t i = 0; i < flatInputPropagations.size(); i++)
        {
            const RangeAndRegisters& rar = flatInputPropagations[i];

            const PropagationRange& inputRange = rar.first;

            const size_t inputRangeWidth = getRangeWidth(rar);

            const size_t inputDepth = PropagationRangeDepth(inputRange);

            const PropagationDescVector& pdv = propagationDescs[i];

            if (paramVec.IsParameterDefined(i))
            {
                const size_t paramValue = paramVec.GetParameter(i);

                assert(paramValue < pdv.size());
                const PropagationDesc& pd = pdv[paramValue];

                size_t registerCost = 0;

                if (pd._inFifo)
                {
                    // Record all of the registers assigned to the fifo
                    fifoWidths[pd._anchorAndDepth] += inputRangeWidth;

                    if (isOptimalPoint)
                    {
                        // Save register assignment
                        DropPoints& dropPoints = finalResult[pd._anchorAndDepth];

                        Union(dropPoints[rar.first.first], rar.second);
                    }

                    registerCost += getTrailingRegisterCost(inputRange, pd._anchorAndDepth.second, inputRangeWidth);
                }
                else
                {
                    // All registers
                    registerCost += getRegisterCost(PropagationRangeDepth(inputRange), inputRangeWidth);
                }

                result._lowerBound += registerCost;
                result._upperBound += registerCost;
            }
            else
            {
                assert(!isOptimalPoint);

                // Parameters are not yet defined

                // Lower-bound is precomputed
                const ObjectiveFunctionBounds& precomputed = precomputedBounds[i];

                result._lowerBound += precomputed._lowerBound;
                result._upperBound += precomputed._upperBound;
            }
        }

        // Add FIFO costs
        for (const auto& p : fifoWidths)
        {
            const AnchorAndDepth anchorAndDepth = p.first;
            const size_t width = p.second;

            const size_t fifoCost = getFifoCost(anchorAndDepth.second, width);

            result._lowerBound += fifoCost;
            result._upperBound += fifoCost;
        }

        return result;
    };

    // Determine which parameters should be optimized jointly
    // If 2 propagations could share a FIFO, then then should be optimized jointly
    ParameterInteractionGraph interactions;

    {
        std::map<AnchorAndDepth, std::set<size_t>> fifoMap;

        for (size_t i = 0; i < flatInputPropagations.size(); i++)
        {
            const PropagationDescVector& pdv = propagationDescs[i];

            for (const PropagationDesc& pd : pdv)
            {
                if (pd._inFifo)
                {
                    fifoMap[pd._anchorAndDepth].insert(i);
                }
            }
        }

        for (size_t i = 0; i < flatInputPropagations.size(); i++)
        {
            const PropagationDescVector& pdv = propagationDescs[i];

            for (const PropagationDesc& pd : pdv)
            {
                if (pd._inFifo)
                {
                    const std::set<size_t>& other = fifoMap[pd._anchorAndDepth];

                    for (const size_t otherRangeIndex : other)
                    {
                        if (otherRangeIndex != i)
                        {
                            interactions[i].insert(otherRangeIndex);
                        }
                    }
                }
            }
        }
    }

    const size_t maxWorkListSize = GetCodeGenConfig().MaximumWorkListSize();

    IntegerBranchAndBoundSparse(parameterRanges, interactions, maxWorkListSize, boundCallback);

    return finalResult;
}

RenamedRegisterMap InsertLocalDataPropagationFifos(Program& program, BasicBlock& basicBlock,
                                                   const FifoPropagations& fifoPropagations)
{
    std::vector<PipelineStage> pipelineStages = GetPipelineStages(basicBlock);

    RenamedRegisterMap destinationRegisters;

    for (const auto& p : fifoPropagations)
    {
        const AnchorAndDepth& anchorAndDepth = p.first;
        const DropPoints& dropPoints = p.second;

        const size_t depth = anchorAndDepth.second;

        size_t fifoWidth = 0;

        for (const auto& p2 : dropPoints)
        {
            for (const size_t regIndex : p2.second)
            {
                const size_t width = program._registerTable[regIndex]._width;

                fifoWidth += width;
            }
        }

        const size_t fifo = AllocateRegister(&program, fifoWidth, RegisterType::Fifo, "local_data_prop");

        RegisterDescription& fifoDesc = program._registerTable[fifo];

        assert(fifoDesc._type == RegisterType::Fifo);

        fifoDesc.Fifo().SetLocation(basicBlock._location, basicBlock._callStackIndex);

        // depth is: endStage - startStage
        // startStage is the stage where the value is computed, the memory store occurs on stageStage + 1
        // endState is the stage where the value is read out
        // The number of memory slots needed in the fixed delay fifo
        // is one less than depth
        assert(depth > 0);
        fifoDesc.Fifo()._depth = depth - 1;

        // The fixed delay fifo requires this
        assert(fifoDesc.Fifo()._depth >= 2);

        fifoDesc.Fifo()._type = FifoType::FixedDelay;
        fifoDesc.Fifo()._useDsp = fifoDesc.Fifo()._depth < GetCodeGenConfig()._fifoToDspThreshold;

        size_t offset = 0;

        for (const auto& p2 : dropPoints)
        {
            const size_t startPipelineStage = p2.first;

            const size_t endPipelineStage = startPipelineStage + depth;

            Operation opEnqueue = {};
            Operation opDequeue = {};

            // There are no reasonable locations for these operations
            // Because they combine many separate variables into 1 fifo
            opEnqueue._expectNoSourceLocation = true;
            opDequeue._expectNoSourceLocation = true;

            opEnqueue._opcode = Opcode::EnqueueRegisters;
            opDequeue._opcode = Opcode::DequeueRegisters;

            opEnqueue._flags._queueRegisters._fifoIndex = fifo;
            opDequeue._flags._queueRegisters._fifoIndex = fifo;

            // Separate offsets tables to allow future transformations
            // to modify them separately
            opEnqueue._flags._queueRegisters._offsets = g_compiler->Create<std::vector<size_t>>();
            opDequeue._flags._queueRegisters._offsets = g_compiler->Create<std::vector<size_t>>();

            for (const size_t regIndex : p2.second)
            {
                opEnqueue._flags._queueRegisters._offsets->push_back(offset);
                opDequeue._flags._queueRegisters._offsets->push_back(offset);

                const size_t width = program._registerTable[regIndex]._width;

                const size_t r =
                    AllocateRegister(&program, width, RegisterType::Local, program._registerTable[regIndex]._name);

                std::set<DestinationRegister>& destinations = destinationRegisters[regIndex];

                // If the source register is placed into multiple fifos
                // and this is not the first enqueue, then regIndex is the incorrect register
                // instead one from destinationRegisters should be used.  That will be fixed up later
                // in the code that changes source operand registers for all operations
                opEnqueue._src.emplace_back(AccessedRegister{regIndex});

                opDequeue._dst.emplace_back(AccessedRegister{r});

                destinations.insert(DestinationRegister{endPipelineStage + 1u, r});

                offset += width;
            }

            pipelineStages[startPipelineStage][0]->_operations.push_back(opEnqueue);

            pipelineStages[endPipelineStage][0]->_operations.push_back(opDequeue);
        }
    }

    return destinationRegisters;
}

boost::optional<size_t> FindLatestPropagation(const std::set<DestinationRegister>& propagatedRegisters,
                                              const size_t stageSequence)
{
    boost::optional<size_t> result = {};

    for (const auto& it : propagatedRegisters)
    {
        if (it._stageSequence <= stageSequence)
        {
            result = {it._registerIndex};
        }
    }

    return result;
}

void RenameFifoPropagatedRegisters(Program& program, BasicBlock& basicBlock, const RenamedRegisterMap& propagations,
                                   RegisterSetMap& liveInMap, const std::unordered_set<const BasicBlock*>& returnSites)
{
    // Update renaming tables for enqueue operations
    for (auto& stage : basicBlock._stages)
    {
        const size_t stageSequence = stage._atomicSequence;

        for (auto& op : stage._operations)
        {
            // Enqueue operation - update rename table
            if (op._getSuccessorBlock)
            {
                for (const auto& ppgReg : propagations)
                {
                    // Find latest propagation destination register that is still less than the current stage
                    auto propagatedReg = FindLatestPropagation(ppgReg.second, stageSequence);

                    if (propagatedReg)
                    {
                        const size_t propagatedRegIndex = propagatedReg.value();
                        auto revRegIt = op._reverseRenamingTable.find(ppgReg.first);
                        // Propagation register found in reverse renaming table
                        if (revRegIt != op._reverseRenamingTable.end())
                        {
                            const size_t originalRegIndex = revRegIt->second;
                            op._renamingTable[originalRegIndex] = propagatedRegIndex;
                            op._reverseRenamingTable.erase(revRegIt);
                            SafeInsert(op._reverseRenamingTable, propagatedRegIndex, originalRegIndex);
                        }
                        // Not found
                        else
                        {
                            // Add an entry for registers that are live-in to successor
                            const size_t originalRegIndex = ppgReg.first;
                            RegisterSet liveInToSuccessor;
                            GetLiveInToSuccessor(op, basicBlock, liveInMap, returnSites, liveInToSuccessor);
                            auto it = liveInToSuccessor.find(originalRegIndex);
                            if (it != liveInToSuccessor.end())
                            {
                                SafeInsert(op._renamingTable, originalRegIndex, propagatedRegIndex);
                                SafeInsert(op._reverseRenamingTable, propagatedRegIndex, originalRegIndex);
                            }
                        }
                    }
                }
            }

            // Update source operands
            for (SourceOperand& srcOp : op._src)
            {
                if (srcOp.Type() == SourceOperandType::Register)
                {
                    AccessedRegister& reg = srcOp.GetAccessedRegister();
                    // Find src reg in propagations
                    auto regIt = propagations.find(reg._registerIndex);
                    if (regIt != propagations.end())
                    {
                        // Find latest propagation destination register that is still less than the current stage
                        auto propagatedReg = FindLatestPropagation(regIt->second, stageSequence);
                        if (propagatedReg)
                        {
                            reg._registerIndex = propagatedReg.value();
                        }
                    }
                }
            }
        }
    }
}

void LocalDataPropagation(Program& program, BasicBlock& basicBlock, RegisterSetMap& liveInMap,
                          const std::unordered_set<const BasicBlock*>& returnSites)
{
    if (basicBlock._function->_functionNode->GetModifiers() & ParseTreeFunctionModifierNoBackPressure)
    {
        // Don't insert fifos for data propagation, because in the current implementation
        // the basic blocks check the data propagation fifo almost_full signal to control thread dispatch
        return;
    }

    if (GetCodeGenDeviceConfig()._isLutBasedShiftRegisterAvailable)
    {
        // Don't insert FIFOs for those architectures that support RAM-based shift registers; using them is
        // expected to be more efficient than transforming into LUTRAM or BRAM with accompanying address logic
        return;
    }

    if (GetCodeGenConfig()._stallablePipelines)
    {
        // Fixed delay fifos are incompatible with stallable pipelines
        return;
    }

    const Propagations propagationCandidates = GetDataPropagations(program, basicBlock, liveInMap, returnSites);

    const auto regToWidth = [&](const size_t registerIndex)
    {
        assert(registerIndex < program._registerTable.size());

        const auto& rd = program._registerTable[registerIndex];

        assert(rd._type == RegisterType::Local);

        return rd._width;
    };

    const FifoPropagations fifos = OptimizePropagations(propagationCandidates, regToWidth);

    const RenamedRegisterMap renamedRegisterMap = InsertLocalDataPropagationFifos(program, basicBlock, fifos);

    RenameFifoPropagatedRegisters(program, basicBlock, renamedRegisterMap, liveInMap, returnSites);
}
