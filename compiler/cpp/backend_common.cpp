// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

size_t BitsToBytes(const size_t bits) { return Align(bits, 8) / 8; }

std::string GetBasicBlockName(const BasicBlock& basicBlock)
{
    std::ostringstream str;

    str << basicBlock._function->_uniqueName << "_BasicBlock_";

    // Get the index of the basic block within the containing function
    size_t basicBlockIndex = 0;

    bool foundBasicBlock = false;

    for (const BasicBlock& iterBb : basicBlock._function->_basicBlocks)
    {
        if (&iterBb == &basicBlock)
        {
            str << basicBlockIndex;
            foundBasicBlock = true;
            break;
        }

        basicBlockIndex++;
    }

    assert(foundBasicBlock);

    // Some tools (quartus) have length limits on module names
    return FixupString(g_compiler->ClampStringLength(str.str()));
}

std::string GetExternalClassInstanceInterfaceName(const ExternalClassInstance& externModule)
{
    std::ostringstream str;

    str << FlattenScopeAndAppendName(externModule._scope, externModule._name) << "Interface";

    return FixupString(str.str());
}

std::string GetExternalClassInstanceTypeName(const ExternalClassInstance& externModule)
{
    std::string str = FlattenScopeAndAppendName(externModule._scope, externModule._name);

    return FixupString(str);
}

bool ContainsAssert(const BasicBlock& basicBlock)
{
    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::Assert)
            {
                return true;
            }
        }
    }

    return false;
}

// This is a high-runner for the debug symbols writer.
std::string GetRegisterName(const size_t registerIndex, const RegisterDescription& regDesc)
{
    const std::string indexStr = std::to_string(registerIndex);

    switch (regDesc._type)
    {
    case RegisterType::Global:
        return "global" + indexStr + "_" + regDesc._name;

    case RegisterType::GlobalView:
        return "global_view" + indexStr + "_" + regDesc._name;

    case RegisterType::Memory:
        return "memory" + indexStr;

    case RegisterType::Pipeline:
        return "reg" + indexStr + "_" + regDesc._name;

    case RegisterType::Wire:
        return "wire" + indexStr + "_" + regDesc._name;

    default:
        assert(false);
    }

    return "";
}

std::string GlobalViewTempName(const Program& program, const size_t globalViewReg, const size_t localReg)
{
    std::ostringstream str;
    str << "global_view_temp_" << globalViewReg << "_" << localReg << "_" << program._registerTable[localReg]._name;
    return str.str();
}

// If a global variable is referenced the GlobalView,this global variable will have the bypassed "_next" port
std::set<size_t> GetRefByGlobalViews(const Program& program)
{
    std::set<size_t> result;

    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];
        if (regDesc._type == RegisterType::GlobalView)
        {
            const OperationList& operations =
                SafeLookup(program._globalViewFunctions, regDesc.GlobalView()._globalViewFunctionIndex);
            Union(result, GetGlobalRegisterSources(program, operations));
        }
    }

    return result;
}

// Addition condition of having _next on global variable
// In the IR, a global variable requires bypassed "_next" port if all three conditions are met:
// (1)registerType::Global , (2)source operand, (3)specific opcode in IR(Opcode::LoadMemory)
std::set<size_t> GetGlobalRequireNext(const Program& program)
{
    std::set<size_t> result;

    result = GetRefByGlobalViews(program);
    // iteration through all operators
    for (const Function& function : program._functions)
    {
        if (function.IsExtern())
        {
        }
        else
        {
            for (const BasicBlock& basicBlock : function._basicBlocks)
            {
                for (const Stage& stage : basicBlock._stages)
                {
                    for (const Operation& op : stage._operations)
                    {
                        if (DetectOpsUseNextValue(op))
                        {
                            for (const SourceOperand& sourceOp : op._src)
                            {
                                if (sourceOp.Type() == SourceOperandType::Register)
                                {
                                    const size_t registerIndex = sourceOp.GetAccessedRegister()._registerIndex;

                                    if (RegisterType::Global == program._registerTable[registerIndex]._type)
                                    {
                                        result.insert(registerIndex);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return result;
}

// Detect if an operand uses the bypassing _next output to check assertion
bool DetectOpsUseNextValue(const Operation& op)
{
    bool foundOp = false;

    if ((op._opcode == Opcode::LoadMemory) || (op._opcode == Opcode::StoreMemory) ||
        (op._opcode == Opcode::InlineExternalModule && OperationHasRegisteredInput(op)) ||
        (op._opcode == Opcode::BypassMemory))
    {
        for (const SourceOperand& sourceOp : op._src)
        {
            if (sourceOp.Type() == SourceOperandType::Register)
            {
                foundOp = true;
                break;
            }
        }
    }

    return foundOp;
}

// Returns the set of global variables that are read by a basic block
std::set<size_t> GetGlobalsRead(const Program& program, const BasicBlock& basicBlock)
{
    std::set<size_t> result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            for (const SourceOperand& sourceOp : op._src)
            {
                if (sourceOp.Type() == SourceOperandType::Register)
                {
                    const size_t registerIndex = sourceOp.GetAccessedRegister()._registerIndex;

                    if (RegisterType::Global == program._registerTable[registerIndex]._type)
                    {
                        result.insert(registerIndex);
                    }
                }
            }
        }
    }
    return result;
}

std::set<size_t> GetGlobalsReadWithoutNext(const Program& program, const BasicBlock& basicBlock,
                                           const std::set<size_t>& globalsRequiringNext)
{
    const std::set<size_t> globals = GetGlobalsRead(program, basicBlock);
    std::set<size_t> result;

    for (const size_t i : globals)
    {
        if (!Contains(globalsRequiringNext, i))
        {
            result.insert(i);
        }
    }

    return result;
}

// Returns the set of global variables' next value that are read by a basic block
// If a register is found in the referredByGlobalViews set,it will have the _next port
std::set<size_t> GetGlobalsReadWithNext(const Program& program, const BasicBlock& basicBlock,
                                        const std::set<size_t>& globalsRequiringNext)
{
    const std::set<size_t> globals = GetGlobalsRead(program, basicBlock);
    const std::set<size_t> result = Intersection(globals, globalsRequiringNext);

    return result;
}

// Returns the set of global variables that are written by a basic block
// The returned pairs contain { registerIndex, writeIndex }
std::set<std::pair<size_t, size_t>> GetGlobalsWritten(const Program& program, const BasicBlock& basicBlock)
{
    std::set<std::pair<size_t, size_t>> result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            for (const DestinationOperand& dstOp : op._dst)
            {
                if (DestinationOperandType::Register == dstOp.Type())
                {
                    const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

                    if (RegisterType::Global == program._registerTable[registerIndex]._type)
                    {
                        const size_t writeIndex = dstOp.GetWriteIndex();

                        result.insert(std::pair<size_t, size_t>(registerIndex, writeIndex));
                    }
                }
            }
        }
    }

    return result;
}

// Returns the set of global views that are read by a basic block
std::set<size_t> GetGlobalViewsRead(const Program& program, const BasicBlock& basicBlock)
{
    std::set<size_t> result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            for (const SourceOperand& sourceOp : op._src)
            {
                if (sourceOp.Type() == SourceOperandType::Register)
                {
                    const size_t registerIndex = sourceOp.GetAccessedRegister()._registerIndex;

                    if (RegisterType::GlobalView == program._registerTable[registerIndex]._type)
                    {
                        result.insert(registerIndex);
                    }
                }
            }
        }
    }

    return result;
}

// Returns the set of global registers that are read by a set of operations
std::set<size_t> GetGlobalRegisterSources(const Program& program, const OperationList& operations)
{
    std::set<size_t> result;

    for (const Operation& op : operations)
    {
        for (const SourceOperand& srcOp : op._src)
        {
            if (SourceOperandType::Register == srcOp.Type())
            {
                const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                if (RegisterType::Global == program._registerTable[registerIndex]._type)
                {
                    result.insert(registerIndex);
                }
            }
        }
    }

    return result;
}

// Returns the set of local registers accessed by a set of operations
std::set<size_t> GetLocalRegisters(const Program& program, const OperationList& operations)
{
    std::set<size_t> result;

    for (const Operation& op : operations)
    {
        // it is sufficient to only look at destination registers
        // as all sources must be defined before use
        for (const DestinationOperand& dstOp : op._dst)
        {
            if (DestinationOperandType::Register == dstOp.Type())
            {
                const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

                if (RegisterType::Local == program._registerTable[registerIndex]._type)
                {
                    result.insert(registerIndex);
                }
            }
        }
    }

    return result;
}

std::set<size_t> GetPropagationFifos(const Program& program, const BasicBlock& basicBlock)
{
    std::set<size_t> result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::DequeueRegisters)
            {
                result.insert(op._flags._enqueue._successorFifo);
            }
        }
    }

    return result;
}

// Returns a map which maps propagation fifo index to
//   a map which maps bit offset to bit width
PropagationFifoSlices GetPropagationFifoSlices(const Program& program, const BasicBlock& basicBlock)
{
    PropagationFifoSlices result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::EnqueueRegisters)
            {
                for (size_t i = 0; i < op._src.size(); i++)
                {
                    const size_t offset = op._flags._queueRegisters._offsets->at(i);

                    const size_t width = op._src[i].Width(program);

                    SafeInsert(result[op._flags._enqueue._successorFifo], offset, width);
                }
            }
        }
    }

    return result;
}

const std::string StageFunctionName(const BasicBlock& basicBlock, const size_t stageIndex)
{
    std::ostringstream str;

    str << GetBasicBlockName(basicBlock) << "_" << stageIndex;

    return FixupString(str.str());
}

bool IsStageEmpty(const BasicBlock& basicBlock, const size_t stageIndex,
                  const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap)
{
    bool result = true;

    auto it = allBlocksEmptyStagesMap.find(basicBlock._inputFifoIndices[0]);

    if (it != allBlocksEmptyStagesMap.end())
    {
        const std::vector<bool>& stageIsEmpty = it->second;

        result = stageIsEmpty[stageIndex];
    }
    // else, this was an externWrapper. It has an input Fifo, but it is not a traditional BasicBlock

    return result;
}

// Returns non-empty stages within a pipeline and their corresponding stageIndex
const std::vector<std::pair<const Stage*, size_t>>
GetStagesInPipeline(const BasicBlock& basicBlock, const size_t atomicSequence,
                    const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap)
{
    std::vector<std::pair<const Stage*, size_t>> result;

    size_t stageIndex = 0;

    for (const Stage& stage : basicBlock._stages)
    {
        size_t index = stageIndex++;

        // Find the pipeline stage in question
        if (stage._atomicSequence < atomicSequence)
        {
            continue;
        }

        if (stage._atomicSequence > atomicSequence)
        {
            break;
        }

        if (!IsStageEmpty(basicBlock, index, allBlocksEmptyStagesMap))
        {
            // Stages in the current pipeline stage
            result.push_back(std::pair<const Stage*, size_t>(&stage, index));
        }
    }

    return result;
}

const std::set<FileAndLineNumber>
GetLocationsPerPipelineStage(const BasicBlock& basicBlock, const size_t atomicSequence,
                             const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap)
{
    // At this point all line numbers have been propagated from Opcode::LineNumber to Stages (GetCodeGenConfig()._debug)
    // or from operation metadata to Stages (optimized case). In both cases it is safe to retrieve line numbers
    // from basicBlock.stages

    std::set<FileAndLineNumber> result;

    const std::vector<std::pair<const Stage*, size_t>> stages =
        GetStagesInPipeline(basicBlock, atomicSequence, allBlocksEmptyStagesMap);

    for (const auto stage : stages)
    {
        const auto& locations = stage.first->_fileAndLineNumbers;

        result.insert(locations.begin(), locations.end());
    }

    return result;
}

template <class T, class Y>
void PrioritizeRegistersWithinStage(const Program& program, const Operation& op, std::set<size_t>& sourcesAccessed,
                                    std::list<std::pair<size_t, RegisterAccessType>>& result,
                                    std::list<std::pair<size_t, RegisterAccessType>>& temporaries,
                                    const std::vector<T>& operands, const RegisterAccessType accessType)
{
    for (const auto& operand : operands)
    {
        if (Y::Register == operand.Type())
        {
            const size_t registerIndex = operand.GetAccessedRegister()._registerIndex;

            RegisterType type = program._registerTable[registerIndex]._type;

            RegisterSourceType sourceType = program._registerTable[registerIndex]._sourceType;

            // Only capture accesses that stem from a source variable
            if (program._registerTable[registerIndex]._isDirectSource)
            {
                if (RegisterType::Global == type || RegisterType::Memory == type || RegisterType::Wire == type)
                {
                    // Globals and Memories are handled elsewhere
                    // Do not include wires since wires only have values during a pipeline stage, not before/after
                    continue;
                }
                else if (RegisterSourceType::Predicate == sourceType && !op._isPredicated)
                {
                    // The operand is a predicate register, but the operation itself is not predicated
                    // i.e. a Mov operation that just moving a predicated register down the pipeline
                    // until it is used. Export it as a temporary.
                    temporaries.push_back(std::pair<size_t, RegisterAccessType>(registerIndex, accessType));
                    continue;
                }
                else
                {
                    // Find the RegisterType::Local index that this pipeline register stems from (if any)
                    const auto it = program._pipelineToSource.find(registerIndex);

                    if (it != program._pipelineToSource.end())
                    {
                        const size_t source = it->second;

                        // It is possible for multiple registers within a pipeline stage to map to the same local source
                        // variable Prioritize which pipeline register to expose to the debugger by selecting the one
                        // accessed last per local source variable
                        if (sourcesAccessed.find(source) == sourcesAccessed.end())
                        {
                            result.push_back(
                                std::pair<size_t, RegisterAccessType>(registerIndex, accessType));

                            sourcesAccessed.insert(source);
                        }
                    }
                }
            }
            else
            {
                if (type == RegisterType::Pipeline)
                {
                    temporaries.push_back(std::pair<size_t, RegisterAccessType>(registerIndex, accessType));
                }
            }
        }
    }
}

const std::list<std::pair<size_t, RegisterAccessType>>
GetRegistersAccessedByStage(const Program& program, const BasicBlock& basicBlock, const size_t atomicSequence,
                            const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap,
                            std::list<std::pair<size_t, RegisterAccessType>>& temporaries)
{
    std::list<std::pair<size_t, RegisterAccessType>> result;

    std::set<size_t> sourcesAccessed;

    const std::vector<std::pair<const Stage*, size_t>> stages =
        GetStagesInPipeline(basicBlock, atomicSequence, allBlocksEmptyStagesMap);

    // Reverse iterate through stages and their operations in order to prioritize
    // the final result of an operation
    for (auto rit = stages.rbegin(); rit != stages.rend(); ++rit)
    {
        const Stage* stage = (*rit).first;

        // Current pipeline stage
        for (auto oit = stage->_operations.rbegin(); oit != stage->_operations.rend(); ++oit)
        {
            const Operation& op = *oit;

            // Get live registers for this pipeline stage
            PrioritizeRegistersWithinStage<SourceOperand, SourceOperandType>(
                program, op, sourcesAccessed, result, temporaries, op._src, RegisterAccessType::Read);
        }
    }

    return result;
}

// Returns the set of fifos that the specified basic block should check almost_full values for
std::set<size_t> GetBackpressureFifos(const Program& program, const BasicBlock& basicBlock)
{
    std::set<size_t> result;

    const auto addFifosToResult = [&](const BasicBlock& bb)
    {
        for (const Stage& stage : bb._stages)
        {
            for (const Operation& op : stage._operations)
            {
                if (op._opcode == Opcode::Enqueue)
                {
                    const RegisterDescription& regDesc = program._registerTable[op._flags._enqueue._successorFifo];
                    assert(RegisterType::Fifo == regDesc._type);

                    const auto& fifoDesc = regDesc.Fifo();

                    if (fifoDesc._maxDepth > 0)
                    {
                        assert(fifoDesc._depth <= fifoDesc._maxDepth);
                        if (fifoDesc._depth == fifoDesc._maxDepth)
                        {
                            // FIFO overflow is impossible because the depth = max depth
                            continue;
                        }
                    }

                    switch (regDesc.Fifo()._type)
                    {
                    case FifoType::Default:
                    case FifoType::ReorderBuffer:
                        result.insert(op._flags._enqueue._successorFifo);
                        break;

                    case FifoType::ContextSaverCaller:
                    {
                        if (regDesc.Fifo()._contextSaverCaller._ordered)
                        {
                            // Ordered context savers can backpressure
                            result.insert(op._flags._enqueue._successorFifo);
                        }
                        else
                        {
                            // These FIFOs will never overflow because
                            // they are large enough for all possible threads
                            const size_t depth = regDesc.Fifo()._depth;

                            assert(depth >= basicBlock._function->_maxThreadCountInsideFunction);
                        }
                    }
                    break;

                    // Passthrough fifos can never backpressure
                    case FifoType::Passthrough:
                    case FifoType::PassthroughRegistered:
                    case FifoType::PassthroughUnregistered:
                        break;

                    default:
                        assert(false);
                    }
                }
            }
        }
    };

    // Backpressure should not flow transitively more than once
    assert(!basicBlock._transitiveBackpressurePredecessor || !basicBlock._transitiveBackpressureSucessor);

    // if _transitiveBackpressurePredecessor != null
    // then almost_full will be checked in the predecessor basic block
    if (!basicBlock._transitiveBackpressurePredecessor)
    {
        addFifosToResult(basicBlock);
    }

    // Also check almost_full values for fifos written by _transitiveBackpressureSucessor
    if (basicBlock._transitiveBackpressureSucessor)
    {
        assert(basicBlock._transitiveBackpressureSucessor->_transitiveBackpressurePredecessor == &basicBlock);
        addFifosToResult(*basicBlock._transitiveBackpressureSucessor);
    }

    return result;
}

std::set<size_t> GetWrittenFifos(const BasicBlock& basicBlock)
{
    std::set<size_t> result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::Enqueue)
            {
                result.insert(op._flags._enqueue._successorFifo);
            }
        }
    }

    return result;
}

// Gets the set of FIFOs which are written by a basic block
// and/or can exert backpressure on the basic block
std::set<size_t> GetWrittenAndBackpressureFifos(const Program& program, const BasicBlock& basicBlock)
{
    std::set<size_t> result = GetWrittenFifos(basicBlock);

    Union(result, GetBackpressureFifos(program, basicBlock));

    return result;
}

std::set<MemoryAccessRecord> GetMemoriesWritten(const Program& program, const BasicBlock& basicBlock)
{
    std::set<MemoryAccessRecord> result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::StoreMemory)
            {
                MemoryAccessRecord record = {};

                record._memoryIndex = op._dst[0].GetAccessedRegister()._registerIndex;
                record._portIndex = op._flags._storeMemory._writePort;

                result.insert(record);
            }
        }
    }

    return result;
}

bool GetMemoryStructureHelperRecursive(const Program& program, const std::string memoryDataType, size_t& offset,
                                       MemoryStructure& memStructure)
{
    const auto it = std::find_if(program._sourceContainers.begin(), program._sourceContainers.end(),
                                 [memoryDataType](const SourceContainer container)
                                 { return container._containerName == memoryDataType; });

    // Check if memory type is a source container (i.e. struct)
    // Save offset and width information for each source
    if (it != program._sourceContainers.end())
    {
        const SourceContainer& container = *it;

        for (const auto member : container._members)
        {
            if (!GetMemoryStructureHelperRecursive(program, member._type, offset, memStructure))
            {
                const size_t width = member._width;

                memStructure._members.push_back({member._name, member._type, offset, width});

                offset += width;
            }
        }

        return true;
    }

    return false;
}

void GetMemoryStructures(const Program& program, std::map<std::string, MemoryStructure>& memoryStructures)
{
    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];

        if (regDesc._type == RegisterType::Memory)
        {
            const std::string& memoryDataType = regDesc.Memory()._sourceDataType;

            size_t offset = 0;
            MemoryStructure memStructure = {};

            // Get the memory structure if data is not flat
            if (GetMemoryStructureHelperRecursive(program, memoryDataType, offset, memStructure))
            {
                const std::string& memoryName = GetRegisterName(i, regDesc);

                memoryStructures.insert({memoryName, memStructure});
            }
        }
    }
}

// Returns the set of ExternalModuleCall that match a given ExternalModuleCallType
std::set<size_t> GetExternalClassInstanceCallsOfType(const Program& program, const BasicBlock& basicBlock,
                                                     const ExternalModuleCallType type)
{
    std::set<size_t> result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::InlineExternalModule)
            {
                const size_t externalModuleIndex = op._flags._callInlineExternalModule._externalModuleIndex;

                const ExternalModuleCall& externalModuleCall = program._externalModuleCalls[externalModuleIndex];

                if (externalModuleCall._type == type)
                {
                    result.insert(externalModuleIndex);
                }
            }
        }
    }

    return result;
}

// Returns the set of ExternalModuleCall calls within a basic block
// which require ports on the basic block
std::set<size_t> GetCrossBBExternalClassInstanceCalls(const Program& program, const BasicBlock& basicBlock)
{
    std::set<size_t> result;

    const ExternalModuleCallType externalModuleCallTypes[] = {ExternalModuleCallType::ExternallyInstantiated,
                                                              ExternalModuleCallType::ExternClassMethod};

    for (const ExternalModuleCallType callType : externalModuleCallTypes)
    {
        const std::set<size_t> callsOfThisType = GetExternalClassInstanceCallsOfType(program, basicBlock, callType);

        Union(result, callsOfThisType);
    }

    return result;
}

bool operator<(const MemoryAccessRecord& lhs, const MemoryAccessRecord& rhs)
{
    if (lhs._memoryIndex < rhs._memoryIndex)
    {
        return true;
    }
    else if (lhs._memoryIndex > rhs._memoryIndex)
    {
        return false;
    }
    else
    {
        return lhs._portIndex < rhs._portIndex;
    }
}

std::set<MemoryAccessRecord> GetMemoriesRead(const Program& program, const BasicBlock& basicBlock)
{
    std::set<MemoryAccessRecord> result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::LoadMemory)
            {
                MemoryAccessRecord record = {};

                record._memoryIndex = op._src[0].GetAccessedRegister()._registerIndex;
                record._portIndex = op._flags._loadMemory._readPort;

                result.insert(record);
            }
        }
    }

    return result;
}

std::vector<std::pair<const Operation&, const BasicBlock*>> GetSuccessorBasicBlocks(const Program& program,
                                                                                    const BasicBlock& basicBlock)
{
    std::vector<std::pair<const Operation&, const BasicBlock*>> result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::Enqueue)
            {
                // Need to check this because an enqueue in an export function does not have a successor.
                if (op._getSuccessorBlock)
                {
                    result.push_back(std::pair<const Operation&, const BasicBlock*>(op, op._getSuccessorBlock()));
                }
            }
        }
    }

    return result;
}

// Returns a unique string identifying a function
// For member functions, combines object name with function name to get a unique string
std::string GetFunctionCombinedName(const Function* const function)
{
    std::ostringstream str;

    // GetBackendName prepends the object name for extern modules
    if ((function->_objectName != g_globalObjectName) && !function->_externClassInstance)
    {
        str << function->_objectName << "_";
    }

    str << function->GetBackendName();

    return FixupString(str.str());
}

// Generates a unique opcode (integer) for each function in the program
FunctionOpcodeMap GetFunctionOpcodeMap(const Program& program)
{
    FunctionOpcodeMap result;

    // don't start at 0, just for debugging purposes
    size_t opcode = 1;

    // CPU->FPGA calls
    for (const EntryPoint* const entryPoint : program._callableEntryPoints)
    {
        for (const Function* const function : entryPoint->_instances)
        {
            result[function] = opcode++;
        }
    }

    return result;
}

FifoResourceUsage GetFifoResourceUsage(const Program& program, const size_t index)
{
    const RegisterDescription& regDesc = program._registerTable[index];

    assert(RegisterType::Fifo == regDesc._type);

    FifoResourceUsage result = {};

    const size_t depth = regDesc.Fifo()._depth;

    const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();

    if (FifoType::Passthrough == regDesc.Fifo()._type)
    {
        // Passthrough fifos do not consume resources
    }
    else if (FifoType::PassthroughUnregistered == regDesc.Fifo()._type)
    {
        // Passthrough fifos do not consume resources
    }
    else if (FifoType::PassthroughRegistered == regDesc.Fifo()._type)
    {
        // Passthrough registered fifos consume one register per bit (ignoring depth)
        const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();
        result._alms = deviceConfig.EstimatePipelineRegisterCost(regDesc._width, 1 /* distance */);
    }
    else if (ShouldFifoUseLutRam(program, index))
    {
        result._alms =
            GetMemoryResourceUsage(deviceConfig._memory._lutRamConfigs, regDesc._width, depth, 1 /* replicaCount */)
                .first;
    }
    else
    {
        result._brams =
            GetMemoryResourceUsage(deviceConfig._memory._blockRamConfigs, regDesc._width, depth, 1 /* replicaCount */)
                .first;
    }

    return result;
}

bool ShouldUseRamType(const RegisterDescription& regDesc, const RAM_TYPE ramType)
{
    assert(RegisterType::Memory == regDesc._type);

    return (ramType == RAM_TYPE::LOGIC)   ? regDesc.Memory()._useLogicRam
           : (ramType == RAM_TYPE::LUT)   ? regDesc.Memory()._useLutRam
           : (ramType == RAM_TYPE::BLOCK) ? regDesc.Memory()._useBlockRam
           : (ramType == RAM_TYPE::DEEP)  ? !regDesc.Memory()._useLutRam && !regDesc.Memory()._useBlockRam
                                          : false;
}

// Implements the policy of determining when a FIFO should use LUT ram vs BRAM
bool ShouldFifoUseLutRam(const Program& program, const size_t registerIndex)
{
    const RegisterDescription& regDesc = program._registerTable[registerIndex];

    assert(RegisterType::Fifo == regDesc._type);

    return regDesc.Fifo()._useLutRam;
}

// Returns a set so that deadlock between semaphores can be avoided by acquiring in low-to-high order
static std::set<size_t> GetModifiedSemaphores(const BasicBlock& basicBlock, const Opcode opcode)
{
    std::set<size_t> result;

    for (const Operation& op : basicBlock._operations)
    {
        if (op._opcode == opcode)
        {
            result.insert(op._flags._semaphoreIndex);
        }
    }

    return result;
}

std::set<size_t> GetAcquiredSemaphores(const BasicBlock& basicBlock)
{
    return GetModifiedSemaphores(basicBlock, Opcode::AcquireSemaphore);
}

std::set<size_t> GetReleasedSemaphores(const BasicBlock& basicBlock)
{
    return GetModifiedSemaphores(basicBlock, Opcode::ReleaseSemaphore);
}

// Determines if a function should fail to compile non-ordered constructs in order to ensure that threads flow through
// the function in FIFO manner
bool DoesFunctionHaveOrderedRestrictions(const ParseTreeFunctionModifier modifiers, const size_t maxThreadCount)
{
    bool result = true;

    if (modifiers & ParseTreeFunctionModifierAsync)
    {
        // Async functions are never considered to be ordered
        // because threads never leave async functions, so the FO in FIFO is meaningless
        result = false;
    }
    else if (maxThreadCount == 1)
    {
        // If there can only be 1 thread in a function at a time
        // then all control-flow will preserve ordering among threads
        // because there is only 1 thread
        result = false;
    }
    else if (modifiers & ParseTreeFunctionModifierUnordered)
    {
        // The function is declared to not preserve order, callers must handle this
        result = false;
    }

    return result;
}

bool BasicBlockNeedsCycleCounter(const BasicBlock& basicBlock)
{
    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::CycleCounter)
            {
                return true;
            }
        }
    }

    return false;
}

// Returns true if unsigned operands in a binary operation should be promoted to signed before the operation occurs
bool ShouldPromoteUnsignedToSigned(const Operation& op)
{
    assert(Opcode::BinaryOp == op._opcode);

    // The unsigned to signed promotion of relational operators is handled by UpdateCompareConditionList() in Lower.cpp.
    // There is no need to promote them here, and it only returns true when performing LutMul.
    const bool promoteIfEitherSigned = op._flags._binaryOpType == ParseTreeBinaryOpTypeLutMul;
    bool promoteToSigned = false;

    // If either operand is signed or if we are doing subtraction, then treat both as signed.
    // The reason we need to special-case subtraction is that DC does not do what we intend
    // in the following common pattern:
    //      logic [3:0] x;
    //      logic [4:0] result;
    //      logic negative_bit;
    //      assign result = 5'(x - 4'hA);
    //      assign negative_bit = result[4];
    // Questa and Vitis sign-extend the result of the unsigned subtraction, but DC truncates.
    if ((promoteIfEitherSigned && op._signExtendSourceMask) || op._flags._binaryOpType == ParseTreeBinaryOpTypeSub)
    {
        promoteToSigned = true;
    }

    return promoteToSigned;
}

const std::string FifoNamer::CrossRegionFifoSuffix = "_cross_region_fifo";

FifoNamer::FifoNamer(const Program& program) : _program(program)
{
    for (const Function& function : program._functions)
    {
        const std::string functionName = FixupString(function._name);

        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            // Names for basic block inputs
            for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
            {
                const size_t inputFifoIndex = basicBlock._inputFifoIndices[i];

                std::ostringstream str;

                str << functionName << "_";

                if (i != 0)
                {
                    str << "BackwardLink";
                }
                else if (&basicBlock == function._start)
                {
                    str << "Entry";
                }
                else
                {
                    str << "Internal";
                }

                SafeInsert(_nameMap, inputFifoIndex, str.str());
            }

            // Names for propagation FIFOs
            const std::set<size_t> propagationFifos = GetPropagationFifos(program, basicBlock);
            for (size_t propagationFifo : propagationFifos)
            {
                SafeInsert(_nameMap, propagationFifo, functionName + "_Propagation");
            }
        }

        // Name for return FIFO
        const size_t returnFifo = function._returnFifoRegisterIndex;
        if (returnFifo != c_invalidAccessedRegisterIndex)
        {
            SafeInsert(_nameMap, returnFifo, functionName + "_Return");
        }
    }
    // Names for FIFOs after a function call (context saver and return from callee)
    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        const std::string calleeName = FixupString(contextSaver._callee->GetName());
        const size_t fromCallerFifo = contextSaver._fromCallerFifoIndex;
        const size_t fromCalleeFifo = contextSaver._fromCalleeFifoIndex;
        SafeInsert(_nameMap, fromCalleeFifo, calleeName + "_FromCallee");
        SafeInsert(_nameMap, fromCallerFifo, calleeName + "_ContextSaver");
    }

    // Construct the FIFO name map and index map
    size_t fifoCount = 0;

    for (size_t i = 0; i < _program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = _program._registerTable[i];

        if (regDesc._type == RegisterType::Fifo)
        {
            SafeInsert(_indexMap, i, fifoCount++);
        }
    }
}

std::string FifoNamer::GetFifoName(const size_t registerIndex) const
{
    const RegisterDescription& regDesc = _program._registerTable[registerIndex];

    auto fifoType = regDesc.Fifo()._type;

    std::ostringstream str;

    std::string fifoPrefix;
    switch (fifoType)
    {
    case FifoType::Default:
        fifoPrefix = "fifo";
        break;

    case FifoType::Passthrough:
    case FifoType::PassthroughRegistered:
    case FifoType::PassthroughUnregistered:
        fifoPrefix = "passthrough";
        break;

    case FifoType::ReorderBuffer:
        fifoPrefix = "reorderbuffer";
        break;

    case FifoType::FixedDelay:
        fifoPrefix = "delay_fifo";
        break;

    case FifoType::ContextSaverCaller:
        fifoPrefix = "context_saver";
        break;

    default:
        assert(false);
    }

    str << fifoPrefix << "_" << GetNormFifoIndex(registerIndex);

    const auto it = _nameMap.find(registerIndex);

    if (it != _nameMap.end())
    {
        str << "_" << it->second;
    }

    // Enable floor planning scripts to find cross-region FIFOs by name
    if (regDesc.Fifo()._implementation == FifoImplementation::CrossRegion)
    {
        str << CrossRegionFifoSuffix;
    }

    return str.str();
}

size_t FifoNamer::GetNormFifoIndex(const size_t registerIndex) const
{
    const size_t normIndex = SafeLookup(_indexMap, registerIndex);

    return normIndex;
}

// Counts the number of bits in all of the registers represented by an inspectable variable
size_t GetInspectableBitWidth(const Program& program, const InspectableVariable& inspectableVariable)
{
    size_t totalBitWidth = 0;

    if (InspectableVariableType::Default == inspectableVariable._inspectionType)
    {
        const bool isMemory = (RegisterType::Memory == inspectableVariable._registerType);

        if (isMemory)
        {
            assert(1 == inspectableVariable._registers.size());
            totalBitWidth = program._registerTable[inspectableVariable._registers[0]].Memory()._elementWidth;
        }
        else
        {
            for (const size_t registerIndex : inspectableVariable._registers)
            {
                const RegisterDescription& regDesc = program._registerTable[registerIndex];

                totalBitWidth += regDesc._width;
            }
        }
    }
    else if (InspectableVariableType::RaceCount == inspectableVariable._inspectionType)
    {
        assert(c_raceCountWidth == inspectableVariable._type->GetBitWidth());

        totalBitWidth = inspectableVariable._type->GetBitWidth();
    }
    else if (InspectableVariableType::BasicBlockControlState == inspectableVariable._inspectionType)
    {
        assert(c_basicBlockControlWidth == inspectableVariable._type->GetBitWidth());

        totalBitWidth = inspectableVariable._type->GetBitWidth();
    }
    else if (InspectableVariableType::CodeCoverage == inspectableVariable._inspectionType)
    {
        assert(1 == inspectableVariable._type->GetBitWidth());

        totalBitWidth = inspectableVariable._type->GetBitWidth();
    }
    else if (InspectableVariableType::SymbolHash == inspectableVariable._inspectionType)
    {
        totalBitWidth = 64;
    }
    else
    {
        assert(false);
    }

    return totalBitWidth;
}

// Replaces characters that are not valid identifier characters with "_" to avoid compilation errors in generated code
std::string FixupString(const std::string& src)
{
    std::string result = src;

    for (size_t i = 0; i < result.size(); i++)
    {
        bool validCharacter = true;

        char& c = result.at(i);

        if (i == 0)
        {
            // First char cannot be a number
            validCharacter = std::isalpha(c);
        }
        else
        {
            validCharacter = std::isalnum(c);
        }

        // Note that if c == '_' then validCharacter = false
        // But actually underscore is valid, that is OK because an underscore is inserted
        if (!validCharacter)
        {
            result.at(i) = '_';
        }
    }

    return result;
}

// Replaces "," with "_" to avoid accidentally creating new entries in CSV files
std::string FixupCommas(const std::string& src)
{
    std::string result = src;

    for (size_t i = 0; i < result.size(); i++)
    {
        if (result.at(i) == ',')
        {
            result.at(i) = '_';
        }
    }

    return result;
}

// Returns true if the specified register needs an extra bit to track race conditions
bool RegisterTracksRaces(const RegisterDescription& regDesc)
{
    bool result = false;

    if (regDesc._type == RegisterType::Global)
    {
        result = (regDesc.Global()._writeCount > 1);
    }
    else if (regDesc._type == RegisterType::Memory)
    {
        if (regDesc.Memory()._replicate)
        {
            // Only writes can cause races
            result = regDesc.Memory()._writePortCount > 1;
        }
        else if (regDesc.Memory()._quadPort)
        {
            // quad port memories can never have races
            // because the number of read/write locations in the program is limited
            result = false;
        }
        else
        {
            // Reads and writes can cause races
            result = (regDesc.Memory()._writePortCount > 1) || (regDesc.Memory()._readPortCount > 1);
        }
    }

    return result;
}

// Replaces characters in a class name so that the resulting string can be used in generated code
std::string FixupClassName(const ClassType* const classType) { return FixupString(classType->GetName()); }

// Returns a string that contains containing object name (if present)
// extern module scope and extern module name
std::string GetExternalClassInstanceBaseName(const ExternalClassInstance& externModule)
{
    std::ostringstream str;

    if (externModule._objectName != g_globalObjectName)
    {
        str << externModule._objectName << "_";
    }

    str << FlattenScopeAndAppendName(externModule._scope, externModule._name);

    return FixupString(str.str());
}

std::string GetExternalClassInstanceName(const ExternalClassInstance& externModule)
{
    std::ostringstream str;

    str << "ExternalClassInstance_";

    str << GetExternalClassInstanceBaseName(externModule);

    return FixupString(str.str());
}

std::string CombineObjectNameAndExternalClassInstanceName(const std::string& objectName,
                                                          const std::string& externModuleName)
{
    std::string result = objectName;
    result += "::";
    result += externModuleName;

    return result;
}

BypassMaps ComputeBypassMaps(const Program& program, const CreateBypassFunction& createBypass)
{
    BypassMaps maps = {};

    size_t count = 0;

    // Maps pipeline stage index to last Stage* associated with that pipeline stage
    std::map<size_t, const Stage*> atomicSequenceToLastStage;

    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            const BypassStoreMap bypassStoreMap = ComputeBypassStoreMap(basicBlock);

            for (const Stage& stage : basicBlock._stages)
            {
                atomicSequenceToLastStage[stage._atomicSequence] = &stage;

                for (const Operation& op : stage._operations)
                {
                    if ((Opcode::LoadMemory == op._opcode) && op._flags._loadMemory._bypass)
                    {
                        const BypassStoreGroupRecord& bsgr =
                            SafeLookup(bypassStoreMap, op._flags._loadMemory._bypassGroupIndex);

                        const size_t id = count++;

                        SafeInsert(maps._loadMemoryOpToBypassId, &op, id);

                        assert(bsgr._loadStage <= bsgr._bypassStage);
                        const size_t depth = (bsgr._bypassStage - bsgr._loadStage) + 1;

                        createBypass(depth, id);
                    }
                    else if (Opcode::StoreMemory == op._opcode)
                    {
                        assert(stage._atomicSequence > 0);

                        const Stage* const previousStage =
                            SafeLookup(atomicSequenceToLastStage, stage._atomicSequence - 1);

                        maps._stageToStoreMemory[previousStage].push_back(&op);

                        if (op._flags._storeMemory._bypass)
                        {
                            const BypassStoreGroupRecord& bsgr =
                                SafeLookup(bypassStoreMap, op._flags._storeMemory._bypassGroupIndex);

                            std::set<size_t> idSet;

                            for (const auto& p : bsgr._loadMemoryMap)
                            {
                                idSet.insert(SafeLookup(maps._loadMemoryOpToBypassId, p.second));
                            }

                            SafeInsert(maps._storeMemoryOpToBypassIds, &op, idSet);
                        }
                    }
                }
            }
        }
    }

    return maps;
}

bool IsAStartConditionPlaceholder(const Operation& op) { return (op._opcode == Opcode::StartCondition); }

bool IsASemaphorePlaceholder(const Operation& op)
{
    return (op._opcode == Opcode::ReleaseSemaphore) || (op._opcode == Opcode::AcquireSemaphore);
}

bool IsAllSemaphores(const Stage& stage)
{
    bool all = true;
    for (const Operation& op : stage._operations)
    {
        all &= IsASemaphorePlaceholder(op);
    }
    return all;
}

BasicBlockDebugMaps ComputeBasicBlockDebugMaps(const Program& program)
{
    BasicBlockDebugMaps maps;

    // We need this map now and later, avoid computing it twice and return it.
    maps._allBlocksBypassMaps = ComputeBypassMaps(program,
                                                  [&](const size_t depth, const size_t id) {
                                                      maps._bypassObjects.push_back({depth, id});
                                                  });
    const BypassMaps& bypassMaps = maps._allBlocksBypassMaps;

    for (const Function& function : program._functions)
    {
        if (function.IsExtern())
        {
            // No need to emit code
            continue;
        }

        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            const size_t localsSuffix = basicBlock._inputFifoIndices[0];

            // First the empty stages.
            //
            std::vector<bool> stageIsEmpty;
            for (const Stage& stage : basicBlock._stages)
            {
                const bool functionIsEmpty = FunctionStageIsEmpty(stage, bypassMaps);

                stageIsEmpty.push_back(functionIsEmpty);
            }
            maps._allBlocksEmptyStagesMap[localsSuffix] = std::move(stageIsEmpty);

            // Next the locals map
            //
            std::map<std::string, uint32_t> localsMap;
            uint32_t structOffset = 0;

            const std::set<size_t> moveLatencyWires = GetMovLatencyWires(basicBlock, program);

            // NB: Must be first: DeclarePipelineValidBits
            localsMap["validBits"] = structOffset;
            localsMap["isWaitConditionMet"] = structOffset + offsetof(LocalsHeader, isWaitConditionMet);
            structOffset += sizeof(LocalsHeader);

            // DeclarePipelineRegisters
            for (const size_t inputRegisterIndex : basicBlock._intraStageRegisters)
            {
                const RegisterDescription inputRegDesc = program._registerTable[inputRegisterIndex];

                if (inputRegDesc._type == RegisterType::Pipeline)
                {
                    uint32_t fieldSize = sizeof(mp_int);
                    const std::string fieldName = GetRegisterName(inputRegisterIndex, inputRegDesc);

                    localsMap[fieldName] = structOffset;
                    structOffset += fieldSize;

                    structOffset += fieldSize;
                }
            }

            // DeclareWires(basicBlock, structOffset);
            for (const size_t inputRegisterIndex : basicBlock._intraStageRegisters)
            {
                const RegisterDescription inputRegDesc = program._registerTable[inputRegisterIndex];

                if (inputRegDesc._type == RegisterType::Wire)
                {
                    std::string wireName = GetRegisterName(inputRegisterIndex, inputRegDesc);
                    uint32_t fieldSize = sizeof(mp_int);

                    localsMap[wireName] = structOffset;
                    structOffset += fieldSize;

                    if (moveLatencyWires.end() != moveLatencyWires.find(inputRegisterIndex))
                    {
                        structOffset += fieldSize;
                    }
                }
            }

            // DeclareGlobalViewTemps
            const std::set<size_t> globalViews = GetGlobalViewsRead(program, basicBlock);

            for (const size_t r : globalViews)
            {
                const RegisterDescription& rd = program._registerTable[r];
                assert(RegisterType::GlobalView == rd._type);

                const OperationList& operations =
                    SafeLookup(program._globalViewFunctions, rd.GlobalView()._globalViewFunctionIndex);

                const std::set<size_t> localRegisters = GetLocalRegisters(program, operations);

                for (const size_t localReg : localRegisters)
                {
                    const RegisterDescription& inputRegDesc = program._registerTable[localReg];

                    const std::string name = GlobalViewTempName(program, r, localReg);

                    uint32_t fieldSize = sizeof(mp_int);

                    localsMap[name] = structOffset;
                    structOffset += fieldSize;
                }

                {
                    // one more variable that holds the final value of the global view
                    const std::string name = GetRegisterName(r, rd);

                    uint32_t fieldSize = sizeof(mp_int);

                    localsMap[name] = structOffset;
                    structOffset += fieldSize;
                }
            }

            // and done with these locals
            maps._allBlocksLocalsMap[localsSuffix] = std::move(localsMap);
        }
    }

    return maps;
}

CommaSeparatedOutputHelper::CommaSeparatedOutputHelper() : _empty(true) {}

void CommaSeparatedOutputHelper::Append(const std::string& str)
{
    if (!_empty)
    {
        _str << ", ";
    }

    _str << str;

    _empty = false;
}

const std::string CommaSeparatedOutputHelper::Str() const { return _str.str(); }

bool FifoSupportsEncoding(const RegisterDescription& regDesc)
{
    assert(RegisterType::Fifo == regDesc._type);

    // Fifo encoding optimizations do not occur at optimization level 0
    if (GetCodeGenConfig()._optimize == 0)
    {
        return false;
    }

    // Codes are only initialized for fifos written by basic blocks
    if (!regDesc.Fifo()._code._initialized)
    {
        return false;
    }

    // Context savers and reorder buffers do not support fifo encoding
    // because they interpret internal data
    // Passthrough* fifos do not have internal memories
    // so the optimization is not useful (synthesis tools can optimize
    // duplicates/constant values in registers)
    return FifoType::Default == regDesc.Fifo()._type;
}

size_t GetLogicalFifoWidth(const RegisterDescription& regDesc)
{
    assert(RegisterType::Fifo == regDesc._type);

    return regDesc._width;
}

size_t GetPhysicalFifoWidth(const RegisterDescription& regDesc)
{
    assert(RegisterType::Fifo == regDesc._type);

    size_t result = regDesc._width;

    if (FifoSupportsEncoding(regDesc))
    {
        const FifoCode& fifoCode = regDesc.Fifo()._code;

        assert(fifoCode._encodedWidth <= regDesc._width);

        result = fifoCode._encodedWidth;
    }

    return result;
}

std::string EscapeSpecialChars(const std::string& str)
{
    std::string result;

    for (const auto ch : str)
    {
        switch (ch)
        {
        case '\n':
            result.append("\\n");
            break;
        case '\t':
            result.append("\\t");
            break;
        case '\\':
            result.append("\\\\");
            break;
        case '"':
            result.append("\\\"");
            break;
        default:
            result.append(1, ch);
            break;
        }
    }

    return result;
}

// Given a memory that has been configured to use a certain RAM type
// Return the depth of each individual physical memory that will create the logical memory
size_t GetMemoryAtomDepth(const Program& program, const size_t registerIndex)
{
    size_t result = 0;

    const RegisterDescription& rd = program._registerTable[registerIndex];
    assert(rd._type == RegisterType::Memory);

    MemoryComposition mc = {};

    const size_t width = rd.Memory()._elementWidth;
    const size_t depth = rd.Memory()._elementCount;
    const size_t replicaCount = rd.Memory().ReplicaCount();

    const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();

    if (rd.Memory()._useLutRam)
    {
        mc = GetMemoryComposition(deviceConfig._memory._lutRamConfigs, width, depth, replicaCount);
    }
    else if (rd.Memory()._useBlockRam)
    {
        mc = GetMemoryComposition(deviceConfig._memory._blockRamConfigs, width, depth, replicaCount);
    }
    else
    {
        mc = GetMemoryComposition(deviceConfig._memory._deepRamConfigs, width, depth, replicaCount);
    }

    assert(mc._atomCount != 0);
    return mc._atomDepth;
}

// Returns the set of wires that are sources or destinations of MovLatencyPlaceholder operations
std::set<size_t> GetMovLatencyWires(const BasicBlock& bb, const Program& program)
{
    std::set<size_t> result;

    const auto handleRegister = [&](const size_t registerIndex)
    {
        if (program._registerTable[registerIndex]._type == RegisterType::Wire)
        {
            result.insert(registerIndex);
        }
    };

    const auto callback = [&](Operation& op)
    {
        if (op._opcode == Opcode::MovLatencyPlaceholder)
        {
            assert(1 == op._dst.size());
            assert(1 == op._src.size());

            if (SourceOperandType::Register == op._src[0].Type())
            {
                handleRegister(op._src[0].GetAccessedRegister()._registerIndex);
            }

            if (DestinationOperandType::Register == op._dst[0].Type())
            {
                handleRegister(op._dst[0].GetAccessedRegister()._registerIndex);
            }
        }
    };

    ForEachOperationForward(const_cast<BasicBlock&>(bb), callback);

    return result;
}

RandomStallRateThresholdGenerator::RandomStallRateThresholdGenerator()
    : RandomStallRateThresholdGenerator(GetCodeGenConfig()._stall)
{
}

RandomStallRateThresholdGenerator::RandomStallRateThresholdGenerator(size_t stallRate)
    : _rng(std::mt19937::default_seed)
{
    constexpr float c_stallProbability[5] = {
        0.00f, // stall == 0
        0.01f, // stall == 1
        0.11f, // stall == 2
        0.50f, // stall == 3
        0.85f  // stall == 4
    };
    assert(stallRate < sizeof(c_stallProbability) / sizeof(float));

    if (stallRate > 0)
    {
        const float mean = c_stallLfsrPeriod * c_stallProbability[stallRate];
        const float stddev = mean * 0.1f;

        _threshold = std::make_unique<std::normal_distribution<float>>(mean, stddev);
    }
}

size_t RandomStallRateThresholdGenerator::Next()
{
    size_t result = 0;

    if (_threshold)
    {
        result = static_cast<size_t>(std::round((*_threshold)(_rng)));
    }

    return result;
}

AutoSection::AutoSection(SourceWriter& sourceWriter, bool indent, std::string openingString, std::string closingString)
    : _sourceWriter(sourceWriter)
{
    _section._indent = indent;
    _section._isActive = false;
    _section._openingString = openingString;
    _section._closingString = closingString;
}

AutoSection::~AutoSection()
{
    if (_section._isActive)
    {
        if (_section._indent)
            _sourceWriter.Unindent();

        if (!_section._closingString.empty())
            _sourceWriter.Printer() << _section._closingString;
    }
}

void AutoSection::Activate()
{
    if (!_section._isActive)
    {
        _section._isActive = true;
        _sourceWriter.Printer() << _section._openingString;
        if (_section._indent)
        {
            _sourceWriter.Indent();
        }
    }
}

bool AutoSection::isActive() { return _section._isActive; }

bool AutoSection::emptyClosingString() { return (_section._closingString.empty()); }

AutoSectionRAII::AutoSectionRAII(SourceWriter& sourceWriter, bool indent, std::string openingString,
                                 std::string closingString)
    : _sourceWriter(sourceWriter)
{
    _sourceWriter.PushAutoSection(AutoSection(sourceWriter, indent, openingString, closingString));
}

AutoSectionRAII::~AutoSectionRAII() { _sourceWriter.PopAutoSection(); }

PopExistingAutoSectionAndRestart::PopExistingAutoSectionAndRestart(SourceWriter& sourceWriter)
    : _sourceWriter(sourceWriter)
{
    _section = *(_sourceWriter.getTopNonEmptySection());
    _sourceWriter.PopAutoSection();
}

PopExistingAutoSectionAndRestart::~PopExistingAutoSectionAndRestart()
{
    _sourceWriter.PushAutoSection(
        AutoSection(_sourceWriter, _section._indent, _section._openingString, _section._closingString));
}

size_t CalculateComparisonWidth(const Program& program, const Operation& op)
{
    // This helper function is used by both CompileBinaryOp and CompileAssert,which may have up to 3 source operands
    assert(op._src.size() == 2 || op._src.size() == 3);
    const size_t lhsWidth = op._src[0].Width(program);
    const size_t rhsWidth = op._src[1].Width(program);
    const size_t maxSrcWidth = std::max(lhsWidth, rhsWidth);
    const bool lhsSigned = op.ShouldSignExtend(0);
    const bool rhsSigned = op.ShouldSignExtend(1);
    const size_t desiredWidth = maxSrcWidth + ((lhsSigned ^ rhsSigned) ? 1 : 0);

    return desiredWidth;
}

size_t CalculateBinaryOpDesiredSourceOperandWidths(const Program& program, const Operation& op)
{
    size_t desiredWidth = 0;

    switch (op._flags._binaryOpType)
    {
    case ParseTreeBinaryOpTypeAdd:
    case ParseTreeBinaryOpTypeSub:
    case ParseTreeBinaryOpTypeLutMul:
    case ParseTreeBinaryOpTypeAnd:
    case ParseTreeBinaryOpTypeOr:
    case ParseTreeBinaryOpTypeXor:
        desiredWidth = op._dst[0].Width(program);
        break;

    case ParseTreeBinaryOpTypeShl:
    case ParseTreeBinaryOpTypeShr:
        desiredWidth = std::max(op._dst[0].Width(program), op._src[0].Width(program));
        break;

    case ParseTreeBinaryOpTypeEQ:
    case ParseTreeBinaryOpTypeNE:
    case ParseTreeBinaryOpTypeGT:
    case ParseTreeBinaryOpTypeGE:
    case ParseTreeBinaryOpTypeLT:
    case ParseTreeBinaryOpTypeLE:
        desiredWidth = CalculateComparisonWidth(program, op);
        break;

    default:
        assert(false);
    }

    return desiredWidth;
}

BypassStoreMap ComputeBypassStoreMap(const BasicBlock& basicBlock)
{
    BypassStoreMap result;

    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if ((op._opcode == Opcode::LoadMemory) && op._flags._loadMemory._bypass)
            {
                BypassStoreGroupRecord& record = result[op._flags._loadMemory._bypassGroupIndex];

                assert((record._loadStage == 0) || (record._loadStage == stage._atomicSequence));
                record._loadStage = stage._atomicSequence;

                const size_t memoryIndex = op._src[0].GetAccessedRegister()._registerIndex;
                assert((record._memoryIndex == 0) || (record._memoryIndex == memoryIndex));
                record._memoryIndex = memoryIndex;

                SafeInsert(record._loadMemoryMap, op._flags._loadMemory._loadMemoryKey, &op);
            }
            else if (op._opcode == Opcode::BypassMemory)
            {
                BypassStoreGroupRecord& record = result[op._flags._bypassMemory._bypassGroupIndex];

                assert((record._bypassStage == 0) || (record._bypassStage == stage._atomicSequence));
                record._bypassStage = stage._atomicSequence;
            }
            else if ((op._opcode == Opcode::StoreMemory) && op._flags._storeMemory._bypass)
            {
                BypassStoreGroupRecord& record = result[op._flags._storeMemory._bypassGroupIndex];

                // There can be only 1 write
                assert(record._storeStage == 0);
                record._storeStage = stage._atomicSequence;

                record._writePort = op._flags._storeMemory._writePort;

                const size_t memoryIndex = op._dst[0].GetAccessedRegister()._registerIndex;
                assert((record._memoryIndex == 0) || (record._memoryIndex == memoryIndex));
                record._memoryIndex = memoryIndex;
            }
        }
    }

    return result;
}

// Given a call into a extern module method
// Lookup the called extern module
const ExternalClassInstance& LookupContainingExternalClassInstance(const Program& program,
                                                                   const ExternalModuleCall& call)
{
    assert(call._type == ExternalModuleCallType::ExternClassMethod);

    const ExternalClassInstanceName& key = *call._externClassInstanceName;

    for (const ExternalClassInstance& externModule : program._externClassInstances)
    {
        if ((externModule._name == key._className) && (externModule._objectName == key._objectName))
        {
            return externModule;
        }
    }

    throw std::runtime_error("Failed to lookup extern module: " + key._className + " " + key._objectName);
}

bool BasicBlockUsesCycleCounter(const BasicBlock& bb, const OperationEnumerationMode mode)
{
    bool result = false;

    ForEachOperationForward(
        const_cast<BasicBlock&>(bb),
        [&](Operation& op)
        {
            if (Opcode::CycleCounter == op._opcode)
            {
                result = true;
            }
        },
        mode);

    return result;
}

LatencyMap GetExtraLatencyMap(const BasicBlock& basicBlock, const OperationEnumerationMode mode, const Program& program)
{
    LatencyMap result;

    std::map<size_t, size_t> chainMap;

    ForEachOperationForward(
        const_cast<BasicBlock&>(basicBlock),
        [&](Operation& op)
        {
            if (Opcode::MovLatencyPlaceholder == op._opcode)
            {
                assert(1 == op._src.size());
                assert(1 == op._dst.size());

                const size_t srcReg = op._src[0].GetAccessedRegister()._registerIndex;
                const size_t dstReg = op._dst[0].GetAccessedRegister()._registerIndex;

                // All stages except the final will have 1 MovLatencyPlaceholder, writing to a wire
                // The final stage will have 2 MovLatencyPlaceholder
                // The first writes to a wire, the second writes to a local/pipeline.
                // The second does not increase latency
                const size_t latencyIncrement = (RegisterType::Wire == program._registerTable[dstReg]._type) ? 1 : 0;

                size_t chainStart = srcReg;

                const auto it = chainMap.find(srcReg);
                if (it != chainMap.end())
                {
                    chainStart = it->second;
                }

                const auto it2 = result.find(chainStart);
                if (it2 == result.end())
                {
                    assert(latencyIncrement == 1);
                    SafeInsert(result, chainStart, LatencyRecord{dstReg, 1});
                }
                else
                {
                    LatencyRecord& lr = it2->second;

                    lr._registerIndex = dstReg;
                    lr._extraLatency += latencyIncrement;
                }

                chainMap[dstReg] = chainStart;
            }
        },
        mode);

    return result;
}

double ArrayCostFunction(const CodeGenDeviceConfig& deviceConfig, const size_t width, const size_t depth,
                         const size_t numReadPorts, const size_t numWritePorts)
{
    assert(depth > 0);

    const size_t registerCount = width * depth;

    const size_t muxCount = (depth - 1) * width * numReadPorts;

    double cost = static_cast<double>(registerCount) * deviceConfig._arrayRegisterCostFactor;

    cost += static_cast<double>(muxCount) * deviceConfig._arrayMuxCostFactor;

    if (numWritePorts > 0)
    {
        cost += static_cast<double>(registerCount) * deviceConfig._arrayWritePortCostFactor;
    }

    return cost;
}

template <typename T>
double GetMemoryCompositionCost(const T& configs, const size_t width, const size_t depth, const size_t replicaCount)
{
    auto memoryComposition = GetMemoryComposition(configs, width, depth, replicaCount);

    return memoryComposition._atomCost * memoryComposition._atomCount;
}

double MemoryCostFunction(const CodeGenDeviceConfig& deviceConfig, const size_t width, const size_t depth,
                          const size_t numReadPorts, const size_t numWritePorts)
{
    // Call GetMemoryCompositionCost for each memory type
    // and return the minimum cost
    assert(depth > 0);

    double costLutRam = GetMemoryCompositionCost(deviceConfig._memory._lutRamConfigs, width, depth, numReadPorts);
    double costBlockRam = GetMemoryCompositionCost(deviceConfig._memory._blockRamConfigs, width, depth, numReadPorts);
    double costDeepRam = GetMemoryCompositionCost(deviceConfig._memory._deepRamConfigs, width, depth, numReadPorts);
    double cost = std::min({costLutRam, costBlockRam, costDeepRam});

    return cost;
}
