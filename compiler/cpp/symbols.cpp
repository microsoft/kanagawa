// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

void AppendLocationAndStack(const Program& program, SymbolWriter& writer, const Location& location,
                            const size_t callStackIndex)
{
    writer.Append("file", g_compiler->GetSourceFileNameWithoutLeadingPath(location._fileIndex));

    writer.Append("line", location._beginLine);

    // a single CSV field with the call stack
    std::string callStackString;

    CallStack callStack = program.GetCallstackFromIndex(callStackIndex);

    while (!callStack.empty())
    {
        const StackFrame& stackFrame = callStack.top();

        if (!callStackString.empty())
        {
            callStackString += "<-";
        }

        callStackString += g_compiler->GetSourceFileNameWithoutLeadingPath(stackFrame._fileIndex);
        callStackString += ":" + std::to_string(stackFrame._lineNumber);

        callStack.pop();
    }

    // Remove commas from the call stack string, to ensure output call stack string is a single CSV field
    for (char& c : callStackString)
    {
        if (c == ',')
        {
            c = ' ';
        }
    }
    writer.Append("stack", callStackString);
}

// Returns either the first or last pipeline stage within a BasicBlock that has source information
// Needs to match the backends which remove empty stages
const std::pair<bool, size_t> GetStageWithSourceLocation(const BasicBlock& basicBlock,
                                                         const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap,
                                                         bool first = true)
{
    std::pair<bool, size_t> result(false, 0);

    size_t atomicSequence = (first) ? 0 : basicBlock._stages.back()._atomicSequence;

    while (true)
    {
        size_t sequence = (first) ? atomicSequence++ : atomicSequence--;

        if (sequence > basicBlock._stages.back()._atomicSequence || (!first && sequence == 0))
        {
            break;
        }

        const auto stages = GetStagesInPipeline(basicBlock, sequence, allBlocksEmptyStagesMap);

        // Need to check that the last non-empty stage function in this pipeline stage
        // has source location information
        if (!stages.empty())
        {
            std::pair<const Stage*, size_t> stage = stages.back();

            if (stage.first->_fileAndLineNumbers.size() > 0)
            {
                result = {true, sequence};

                break;
            }
        }
    }

    return result;
}

void AddSuccessorInformation(const Program& program, DebugSymbols& symbols,
                             const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap)
{
    for (const Function& function : program._functions)
    {
        if (function.IsExtern())
        {
            continue;
        }

        // BasicBlock pipeline stage to source location
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            if (basicBlock._stages.empty())
            {
                continue;
            }

            const auto bbIndex = basicBlock._inputFifoIndices[0];

            // Get last source location in current BasicBlock - returns {valid, atomicSequence}
            const auto lastStageWithSource =
                GetStageWithSourceLocation(basicBlock, allBlocksEmptyStagesMap, false /* last */);

            // 1. First take care of basicBlock boundaries
            std::vector<std::pair<const Operation&, const BasicBlock*>> successors =
                GetSuccessorBasicBlocks(program, basicBlock);

            if (lastStageWithSource.first)
            {
                for (auto successor : successors)
                {
                    const auto successorStage =
                        GetStageWithSourceLocation((*successor.second), allBlocksEmptyStagesMap, true /* first */);

                    const Operation& op = successor.first;

                    if (successorStage.first)
                    {
                        if (op._flags._enqueue._type == EnqueueType::FunctionCall)
                        {
                            symbols.AddStepInto(bbIndex, lastStageWithSource.second,
                                                {{successor.second->_inputFifoIndices[0], successorStage.second}});
                        }
                        else if (op._flags._enqueue._type == EnqueueType::ContextSaverCallee)
                        {
                            symbols.AddStepOut(bbIndex, lastStageWithSource.second,
                                               {{successor.second->_inputFifoIndices[0], successorStage.second}});

                            bool foundCaller = false;

                            // If type is ContextSaverCallee, then we can use this to find the ContextSaver,
                            // which provides function call and callIndex information required for debug symbols.
                            const size_t contextSaverIndex = op._flags._enqueue._successorFifo;

                            for (const ContextSaver& contextSaver : program._contextSavers)
                            {
                                if (contextSaverIndex == contextSaver._fromCalleeFifoIndex)
                                {
                                    const size_t callIndex = contextSaver._callSiteIndex;

                                    // The "afterCall" basicBlock of this callee should belong to the function caller.
                                    const std::string functionCallerName = contextSaver._afterCall->_function->_name;

                                    // Get last source location in current BasicBlock
                                    const auto callerPipelineStage = GetStageWithSourceLocation(
                                        *contextSaver._beforeCall, allBlocksEmptyStagesMap, false /* last */);

                                    symbols.AddFunctionCallerToFunction(function._name, functionCallerName,
                                                                        contextSaver._beforeCall->_inputFifoIndices[0],
                                                                        callerPipelineStage.second, callIndex);

                                    foundCaller = true;

                                    break;
                                }
                            }

                            // Error if we have not found the caller for this callee function
                            assert(foundCaller);
                        }
                        else
                        {
                            symbols.AddStepOver(bbIndex, lastStageWithSource.second,
                                                {{successor.second->_inputFifoIndices[0], successorStage.second}});
                        }
                    }
                }
            }

            // 2. Now add successor information within a BasicBlock
            // For a given pipelineStage with source information, its successor is the next pipeline stage with source
            // information
            size_t currentAtomicSequence = 0;
            size_t nextAtomicSequence = 1;
            std::set<FileAndLineNumber> curLocations =
                GetLocationsPerPipelineStage(basicBlock, currentAtomicSequence, allBlocksEmptyStagesMap);

            while (nextAtomicSequence <= basicBlock._stages.back()._atomicSequence)
            {
                std::set<FileAndLineNumber> nextLocations =
                    GetLocationsPerPipelineStage(basicBlock, nextAtomicSequence, allBlocksEmptyStagesMap);
                while (nextLocations.size() == 0 && nextAtomicSequence < basicBlock._stages.back()._atomicSequence)
                {
                    // The next stage could be empty (i.e. barrier or empty stage in schedule block)
                    // Find the next pipeline stage with source information if any
                    nextLocations =
                        GetLocationsPerPipelineStage(basicBlock, ++nextAtomicSequence, allBlocksEmptyStagesMap);
                }

                if (curLocations.size() > 0 && nextLocations.size() > 0)
                {
                    symbols.AddStepOver(bbIndex, currentAtomicSequence, {{bbIndex, nextAtomicSequence}});
                }

                currentAtomicSequence = nextAtomicSequence++;

                curLocations = nextLocations;
            }
        }
    }
}

uint64_t CreateDebugSymbols(const std::string& fileName, const Program& program,
                            const BasicBlockDebugMaps& allBlocksData, const uint64_t symbolHash)
{
    DebugSymbols symbols;

    // Filenames
    for (size_t i = 0; i < GetCodeGenConfig()._fileNames.size(); i++)
    {
        std::string fileName = g_compiler->GetSourceFileName(i);
        if (GetCodeGenConfig()._stripDebugSymbols)
        {
            fileName = StripLeadingPath(fileName);
        }

        symbols.AddFilePath(i, fileName);
    }

    for (const Function& function : program._functions)
    {
        if (function.IsExtern())
        {
            continue;
        }

        // BasicBlock pipeline stage to source location
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            if (basicBlock._stages.empty())
            {
                continue;
            }

            const auto bbIndex = basicBlock._inputFifoIndices[0];

            const std::map<std::string, uint32_t>& localsMap = SafeLookup(allBlocksData._allBlocksLocalsMap, bbIndex);

            // Get Memories accessed by this BasicBlock
            std::set<size_t> memoriesAccessed;
            const std::set<MemoryAccessRecord> writtenMemories = GetMemoriesWritten(program, basicBlock);
            const std::set<MemoryAccessRecord> readMemories = GetMemoriesRead(program, basicBlock);

            std::for_each(readMemories.begin(), readMemories.end(),
                          [&](const MemoryAccessRecord& m) { memoriesAccessed.insert(m._memoryIndex); });

            std::for_each(writtenMemories.begin(), writtenMemories.end(),
                          [&](const MemoryAccessRecord& m) { memoriesAccessed.insert(m._memoryIndex); });

            // Get Globals accessed by this BasicBlock
            std::set<size_t> globalsAccessed = GetGlobalsRead(program, basicBlock);
            const std::set<std::pair<size_t, size_t>> writtenGlobals = GetGlobalsWritten(program, basicBlock);

            // Merge the read/written sets
            std::for_each(writtenGlobals.begin(), writtenGlobals.end(),
                          [&](const std::pair<size_t, size_t>& g) { globalsAccessed.insert(g.first); });

            // Member functions of an object should display "this" globals
            for (const auto& objectName : basicBlock._objectNames)
            {
                assert(objectName != g_globalObjectName);

                const auto it = program._objectNameToGlobals.find(objectName);

                if (it != program._objectNameToGlobals.end())
                {
                    const auto& globals = it->second;

                    for (const auto global : globals)
                    {
                        const RegisterDescription& regDesc = program._registerTable[global];

                        assert(regDesc._isDirectSource);

                        if (regDesc._type == RegisterType::Memory)
                        {
                            memoriesAccessed.insert(global);
                        }
                        else
                        {
                            assert(regDesc._type == RegisterType::Global);
                            globalsAccessed.insert(global);
                        }
                    }
                }
            }

            for (const auto& mem : memoriesAccessed)
            {
                const RegisterDescription& regDesc = program._registerTable[mem];

                assert(regDesc._type == RegisterType::Memory);

                // Only capture accesses that stem from a source variable
                if (program._registerTable[mem]._isDirectSource)
                {
                    symbols.AddGlobalRegisterAccessedByBasicBlock(bbIndex, "memory", GetRegisterName(mem, regDesc));
                }
            }

            for (const auto& global : globalsAccessed)
            {
                const RegisterDescription& regDesc = program._registerTable[global];

                assert(regDesc._type == RegisterType::Global);

                // Only capture accesses that stem from a source variable
                if (program._registerTable[global]._isDirectSource)
                {
                    symbols.AddGlobalRegisterAccessedByBasicBlock(bbIndex, "global", GetRegisterName(global, regDesc));
                }
            }

            // Pipeline stage specific symbols
            for (size_t currentAtomicSequence = 0; currentAtomicSequence <= basicBlock._stages.back()._atomicSequence;
                 currentAtomicSequence++)
            {
                // Only add source locations to the symbols for non-empty stage functions. Empty stage
                // functions are not run by the emulator backends.
                const std::set<FileAndLineNumber> locations = GetLocationsPerPipelineStage(
                    basicBlock, currentAtomicSequence, allBlocksData._allBlocksEmptyStagesMap);

                // Verify uniqueness of source locations, for the debugger.
                std::map<size_t, std::list<size_t>> locationsMap;

                for (const auto& loc : locations)
                {
                    symbols.AddSourceLocationToPipelineStage(loc._fileIndex, loc._lineNumber, loc._columnNumber,
                                                             bbIndex, currentAtomicSequence);

                    locationsMap[loc._fileIndex].push_back(loc._lineNumber);
                }

                // If we have multiple files this is trouble for the debugger,
                // it won't be able to map back from stage to source
                if (locationsMap.size() > 1)
                {
                    // There are two un-avoidable cases:
                    // 1) A function was inlined in a wait_for statement (stage 0)
                    // 2) A function was inlined in an atomic statement (stage > 0)
                    // Anything else is an error.

                    if ((basicBlock._hasInlinedAtomicDoFunctions && (0 == currentAtomicSequence)) ||
                        (basicBlock._hasInlinedAtomicFunctions && (0 != currentAtomicSequence)))
                    {
                    }
                    else
                    {
                        if (GetCodeGenConfig()._debug && (GetCodeGenConfig()._optimize == 0))
                        {
                            Location loc = {0, 0, 0, false};
                            std::ostringstream err;

                            err << "Basic block index " << bbIndex << " stage " << currentAtomicSequence
                                << " has multiple file locations:\n";
                            for (const auto& dup : locationsMap)
                            {
                                err << "\tfile " << g_compiler->GetSourceFileName(dup.first) << " fileIndex "
                                    << dup.first << " line numbers:";
                                for (const auto l : dup.second)
                                {
                                    err << " " << l;

                                    // Charge the first file location with the error
                                    if (!loc._valid)
                                    {
                                        loc._beginLine = l;
                                        loc._endLine = l;
                                        loc._fileIndex = dup.first;
                                        loc._valid = true;
                                    }
                                }
                                err << std::endl;
                            }

                            g_compiler->ErrorStream(loc, CompileError::AmbiguousDebugSymbols) << err.str();
                        }
                    }
                }

                // BasicBlock pipeline stage to live registers (local) accessed by this pipeline stage
                std::list<std::pair<size_t, RegisterAccessType>> temporariesAccessed;
                const std::list<std::pair<size_t, RegisterAccessType>> registersAccessed =
                    GetRegistersAccessedByStage(program, basicBlock, currentAtomicSequence,
                                                allBlocksData._allBlocksEmptyStagesMap, temporariesAccessed);

                // For locals, need to identify read/write for read/writeIndex potentially
                for (const auto& reg : registersAccessed)
                {
                    const RegisterDescription& regDesc = program._registerTable[reg.first];

                    switch (regDesc._type)
                    {
                    case RegisterType::Pipeline:
                    case RegisterType::Wire:
                    {
                        const std::string regName = GetRegisterName(reg.first, regDesc);
                        const size_t fieldOffset = SafeLookup(localsMap, regName);
                        symbols.AddLocalRegisterAccessedByPipelineStage(bbIndex, currentAtomicSequence, regName,
                                                                        fieldOffset, reg.second, regDesc._sourceType);
                        break;
                    }

                    default:
                        assert(false);
                        break;
                    }
                }

                // For temporaries ...
                for (const auto& reg : temporariesAccessed)
                {
                    const RegisterDescription& regDesc = program._registerTable[reg.first];

                    switch (regDesc._type)
                    {
                    case RegisterType::Pipeline:
                    {
                        const std::string regName = GetRegisterName(reg.first, regDesc);
                        const size_t fieldOffset = SafeLookup(localsMap, regName);
                        symbols.AddLocalRegisterAccessedByPipelineStage(bbIndex, currentAtomicSequence, regName,
                                                                        fieldOffset, reg.second,
                                                                        RegisterSourceType::Intermediate);
                        break;
                    }

                    default:
                        assert(false);
                        break;
                    }
                }
            }

            symbols.AddBasicBlockToFunction(function._name, bbIndex);
        }
    }

    // Get memory layout information for memories that store structs
    std::map<std::string, MemoryStructure> memoryStructures;
    GetMemoryStructures(program, memoryStructures);
    symbols.AddMemoryStructure(memoryStructures);

    // Needs to run after all BasicBlock sources have been added to Symbols
    AddSuccessorInformation(program, symbols, allBlocksData._allBlocksEmptyStagesMap);

    // Num globals and memories
    size_t numGlobals = 0;
    size_t numMemories = 0;
    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];

        switch (regDesc._type)
        {
        case RegisterType::Global:
            numGlobals++;
            break;
        case RegisterType::Memory:
            numMemories++;
            break;
        default:
            break;
        }
    }

    symbols.SetNumGlobalsAndMems(numGlobals, numMemories);

    // Source variables to Registers/Globals/Memories
    for (const auto& it : program._sourceToPipelineRegisterMap)
    {
        const size_t localReg = it.first;

        const std::set<size_t>& pipelineRegisters = it.second;
        const RegisterDescription& regDesc = program._registerTable[localReg];

        if (regDesc._type == RegisterType::Local && pipelineRegisters.empty())
        {
            // At this point, any Locals without pipelined registers
            // are variables that have been optimized away
            // i.e. unused source code
            continue;
        }

        // Get register names
        std::vector<std::string> pipelineRegisterNames;
        for (auto reg : pipelineRegisters)
        {
            const RegisterDescription& regDesc = program._registerTable[reg];

            // Do not include wires for now
            if (regDesc._type == RegisterType::Wire)
            {
                continue;
            }

            pipelineRegisterNames.push_back(GetRegisterName(reg, regDesc));
        }

        const std::string& name = regDesc._sourceVariable._name;
        const std::string& scope = regDesc._sourceVariable._scope;
        const std::string& dataType = regDesc._sourceVariable._dataType;
        const size_t bitSize = regDesc._sourceVariable._bitSize;
        const SourceLocation& declaredLocation = regDesc._sourceVariable._declaredLocation;

        const std::string& containingObjectName =
            (regDesc._sourceVariable._container) ? regDesc._sourceVariable._container->_scope : "";

        if (pipelineRegisters.empty())
        {
            // This is the case for globals and memories
            symbols.AddSourceVariable(name, scope, dataType, bitSize, declaredLocation, containingObjectName,
                                      {GetRegisterName(localReg, regDesc)});
        }
        else
        {
            symbols.AddSourceVariable(name, scope, dataType, bitSize, declaredLocation, containingObjectName,
                                      pipelineRegisterNames);
        }
    }

    // Capture object hierarchy
    symbols.AddSourceObjects(program._sourceContainers);

    // Add version to the DebugSymbols file
    symbols.AddDetailLevel();

    const size_t hash = WriteDebugSymbols(symbols, fileName, symbolHash);

    if (g_compiler->HadCompileError())
    {
        throw std::runtime_error("Debug symbols generation failed");
    }

    return hash;
}
