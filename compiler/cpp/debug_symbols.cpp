// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

void DebugSymbols::AddFilePath(const size_t index, const std::string& filePath)
{
    _filePathToIndexMap[filePath] = index;
    _indexToFilePathMap[index] = filePath;
}

void DebugSymbols::AddSourceHashes(const uint64_t symbolHash) { _symbolHash = symbolHash; }

int DebugSymbols::GetIndexFromFile(const std::string& fileName)
{
    int result = -1;

    const auto it = _filePathToIndexMap.find(fileName);

    if (it != _filePathToIndexMap.end())
    {
        result = static_cast<int>(it->second);
    }

    return result;
}

const std::string DebugSymbols::GetFileFromIndex(const size_t index)
{
    const auto it = _indexToFilePathMap.find(index);

    if (it != _indexToFilePathMap.end())
    {
        return it->second;
    }

    return "";
}

const file_path_index_map_t& DebugSymbols::GetProgramSourceFiles() { return _filePathToIndexMap; }

const DebugSymbols::FunctionInfo* DebugSymbols::LookupFunctionInfo(const std::string& functionName)
{
    const auto it = _functionSymbols.find(functionName);

    if (it == _functionSymbols.end())
    {
        return nullptr;
    }

    return &it->second;
}

DebugSymbols::FunctionInfo& DebugSymbols::GetOrCreateFunctionInfo(const std::string& functionName)
{
    const auto it = _functionSymbols.find(functionName);

    if (it != _functionSymbols.end())
    {
        return it->second;
    }

    FunctionInfo info;
    auto result = _functionSymbols.insert(std::pair<std::string, FunctionInfo>(functionName, info));
    return result.first->second;
}

void DebugSymbols::AddBasicBlockToFunction(const std::string& functionName, const size_t basicBlock)
{
    GetOrCreateFunctionInfo(functionName)._basicBlocks.push_back(basicBlock);
}

void DebugSymbols::AddFunctionCallerToFunction(const std::string& functionName, const std::string& functionCaller,
                                               const size_t bbIndex, const size_t atomicSequence,
                                               const size_t callIndex)
{
    LocationId pdKey(bbIndex, atomicSequence);

    GetOrCreateFunctionInfo(functionName)
        ._functionCallerInfo.push_back({{functionCaller, pdKey.Serialize()}, callIndex});
}

const std::string DebugSymbols::GetFunctionFromPipelineStage(const size_t pdLocationKey)
{
    // Get BasicBlock index
    LocationId id(pdLocationKey);

    std::string functionName = "";

    for (const auto& f : _functionSymbols)
    {
        for (const auto& bb : f.second._basicBlocks)
        {
            if (id._index == bb)
            {
                // This is the function we are looking for
                functionName = f.first;

                break;
            }
        }
    }

    return functionName;
}

bool DebugSymbols::FunctionHasCallers(const std::string& functionName)
{
    const auto info = LookupFunctionInfo(functionName);

    if (info)
    {
        return info->_functionCallerInfo.size() > 0;
    }

    return false;
}

const std::pair<std::string, size_t> DebugSymbols::GetFunctionCaller(const std::string& functionName,
                                                                     const size_t callIndex)
{
    const auto info = LookupFunctionInfo(functionName);

    // returns <function caller name, caller pipeline stage>
    std::pair<std::string, size_t> result = {"", 0};

    if (info)
    {
        for (const auto& callers : info->_functionCallerInfo)
        {
            if (callIndex == callers.second)
            {
                result = callers.first;

                break;
            }
        }
    }

    return result;
}

void DebugSymbols::SetNumGlobalsAndMems(const size_t numGlobals, const size_t numMemories)
{
    _numGlobals = numGlobals;

    _numMemories = numMemories;
}

void DebugSymbols::AddSourceLocationToPipelineStage(const size_t fileIndex, const size_t lineNumber,
                                                    const size_t columnNumber, const size_t bbIndex,
                                                    const size_t atomicSequence)
{
    SourceLocation srcLoc(fileIndex, lineNumber, columnNumber);

    size_t pdKey = LocationId(bbIndex, atomicSequence).Serialize();

    _sourceLocationToPipelineStagesMap[srcLoc].insert(pdKey);

    // Reverse lookup map
    GetOrCreatePipelineStageInfo(pdKey)._sourceLocations.push_back(srcLoc);
}

const std::set<size_t> DebugSymbols::GetKanagawaLocationFromSource(const size_t fileIndex, const size_t lineNumber,
                                                                   const std::optional<size_t> column)
{
    std::set<std::size_t> result;
    const size_t columnNumber = column.value_or(0);
    const auto loc = SourceLocation(fileIndex, lineNumber, columnNumber);

    auto it = _sourceLocationToPipelineStagesMap.find(loc);
    if (it != _sourceLocationToPipelineStagesMap.end())
    {
        return it->second;
    }
    if (!column.has_value())
    {
        // Tests and interactive use are loose on the column number, they use zero most of the time.
        // If the columnNumber is not specified take it to mean 'any column in that line'
        it = _sourceLocationToPipelineStagesMap.upper_bound(loc);
        if ((it != _sourceLocationToPipelineStagesMap.end()) && (it->first._fileIndex == fileIndex) &&
            (it->first._lineNumber == lineNumber))
        {
            return it->second;
        }
    }

    return result;
}

const std::pair<SourceLocation, SourceLocation>
DebugSymbols::GetSourceRangeFromKanagawaLocation(const size_t pdLocationKey)
{
    // Returns a pair of source locations to describe the start and end range of a Kanagawa location
    // If there is no range, the second location is just {0, 0}
    // SourceLocation contains <fileIndex, lineNumber>
    std::pair<SourceLocation, SourceLocation> result = {{0, 0, 0}, {0, 0, 0}};

    const auto info = LookupPipelineStage(pdLocationKey);

    if (info)
    {
        // This list of <SourceLocation> is sorted by fileIndex, then lineNumbers
        const auto& sourceLocations = info->_sourceLocations;

        if (!sourceLocations.empty())
        {
            // For now return the smallest contiguous line number range that belongs to one file
            // A pipeline stage can map to several non-contiguous source line ranges from different files
            // Until a smarter selection process is implemented, return one valid range
            std::map<size_t, std::list<size_t>> locationsMap;

            for (const auto& src : sourceLocations)
            {
                locationsMap[src._fileIndex].push_back(src._lineNumber);
            }

            assert(!locationsMap.empty());
            size_t fileIndex = locationsMap.begin()->first;
            std::list<size_t> lineNumbers = locationsMap.begin()->second;

            for (const auto& it : locationsMap)
            {
                if (lineNumbers.size() > it.second.size())
                {
                    fileIndex = it.first;
                    lineNumbers = it.second;
                }
            }

            result.first._fileIndex = fileIndex;
            result.first._lineNumber = lineNumbers.front();

            if (lineNumbers.size() > 0)
            {
                result.second._fileIndex = fileIndex;
                result.second._lineNumber = lineNumbers.back();
            }
        }
        // else this DebugSymbols::PipelineStage info does not have source locations
        // This can happen for stage 0, where globals and memories are stored for a
        // particular basic block, but which may not correspond to any source.
    }

    return result;
}

void DebugSymbols::AddStepInto(const size_t bbIndex, const size_t atomicSequence,
                               const std::list<std::pair<size_t, size_t>>& successorStages)
{
    const auto bbKey = LocationId(bbIndex, atomicSequence).Serialize();
    auto& stepInto = GetOrCreatePipelineStageInfo(bbKey)._stepInto;

    for (const auto& successor : successorStages)
    {
        const auto succKey = LocationId(successor.first, successor.second).Serialize();
        stepInto.push_back(succKey);

        // Reverse direction
        auto& stepBack = GetOrCreatePipelineStageInfo(succKey)._stepBack;
        stepBack.push_back(bbKey);
    }
}

void DebugSymbols::AddStepOver(const size_t bbIndex, const size_t atomicSequence,
                               const std::list<std::pair<size_t, size_t>>& successorStages)
{
    const auto bbKey = LocationId(bbIndex, atomicSequence).Serialize();
    auto& stepOver = GetOrCreatePipelineStageInfo(bbKey)._stepOver;

    for (const auto& successor : successorStages)
    {
        const auto succKey = LocationId(successor.first, successor.second).Serialize();
        stepOver.push_back(succKey);

        // Reverse direction
        auto& stepBack = GetOrCreatePipelineStageInfo(succKey)._stepBack;
        stepBack.push_back(bbKey);
    }
}

void DebugSymbols::AddStepOut(const size_t bbIndex, const size_t lastAtomicSequence,
                              const std::list<std::pair<size_t, size_t>>& successorStages)
{
    // Can step out from anywhere within a BasicBlock, so just store
    // step information in stage 0 of BasicBlock.
    const auto bbKey = LocationId(bbIndex, 0).Serialize();
    auto& stepOut = GetOrCreatePipelineStageInfo(bbKey)._stepOut;

    // For the reversed direction we need to preserve the lastStage info,
    // which we insert at the front of the list. GetStepOut knows to skip it.
    // We need that info because we will need to serialize the list later.
    const auto lastKey = LocationId(bbIndex, lastAtomicSequence).Serialize();

    for (const auto& successor : successorStages)
    {
        const auto succKey = LocationId(successor.first, successor.second).Serialize();
        if (stepOut.empty())
        {
            stepOut.push_back(lastKey);
        }
        stepOut.push_back(succKey);

        // Reversed direction
        auto& stepBack = GetOrCreatePipelineStageInfo(succKey)._stepBack;
        stepBack.push_back(lastKey);
    }
}

const std::list<size_t> DebugSymbols::GetStepOut(const size_t bbIndex)
{
    std::list<size_t> result;

    const auto info = LookupPipelineStage(LocationId(bbIndex, 0).Serialize());

    if (info)
    {
        // Skip the first element (see above)
        result = std::list<size_t>(++info->_stepOut.begin(), info->_stepOut.end());
    }

    return result;
}

void DebugSymbols::AddGlobalRegisterAccessedByBasicBlock(const size_t bbIndex, const std::string& type,
                                                         const std::string& name)
{
    GetOrCreatePipelineStageInfo(LocationId(bbIndex, 0).Serialize())
        ._globals.push_back(std::pair<std::string, std::string>(type, name));
    // Collect all global symbol names into a map
    _globalRegisterSymbolNames[name] = type;
}

void DebugSymbols::AddMemoryStructure(const std::map<std::string, MemoryStructure>& memoryStructureMap)
{
    _memoryStructures.insert(memoryStructureMap.begin(), memoryStructureMap.end());
}

const MemoryStructure* DebugSymbols::GetMemoryStructure(const std::string& memoryRegisterName) const
{
    const MemoryStructure* result = nullptr;

    const auto it = _memoryStructures.find(memoryRegisterName);

    if (it != _memoryStructures.end())
    {
        result = &(it->second);
    }

    return result;
}

void DebugSymbols::AddLocalRegisterAccessedByPipelineStage(const size_t bbIndex, const size_t atomicSequence,
                                                           const std::string& regName, const size_t offset,
                                                           const RegisterAccessType access,
                                                           const RegisterSourceType sourceType)
{
    PipelineStageInfo& info = GetOrCreatePipelineStageInfo(LocationId(bbIndex, atomicSequence).Serialize());

    size_t registerKey = LocationId(bbIndex, offset).Serialize();

    std::pair<size_t, RegisterAccessType> registerOffsetAndAccess = {registerKey, access};

    std::pair<size_t, std::string> registerOffsetAndName = {registerKey, regName};

    switch (sourceType)
    {
    case RegisterSourceType::Local:

        info._localsOffset.push_back(registerOffsetAndAccess);

        _localsOffsetToRegName.insert(registerOffsetAndName);

        break;

    case RegisterSourceType::Intermediate:

        info._intermediatesOffset.push_back(registerOffsetAndAccess);

        _intermediateOffsetToRegName.insert(registerOffsetAndName);

        break;

    case RegisterSourceType::Hidden:

        info._callIndexOffset = registerOffsetAndAccess;

        _callIndexOffsetToRegName.insert(registerOffsetAndName);

        break;

    case RegisterSourceType::Predicate:

        info._predicateOffset = registerOffsetAndAccess;

        _predicateOffsetToRegName.insert(registerOffsetAndName);

        break;

    default:
        assert(false);
    }

    // Insert in the _registerNameToStage map and check no dups.
    const auto it = _registerNameToStage.find(regName);
    if (it == _registerNameToStage.end())
    {
        _registerNameToStage[regName] = atomicSequence;
    }
    else
    {
        assert(atomicSequence == it->second);
    }
}

// Used by trace report writer
size_t DebugSymbols::GetStageForRegister(const std::string& regName)
{
    const auto it = _registerNameToStage.find(regName);

    if (it == _registerNameToStage.end())
    {
        return std::numeric_limits<size_t>::max();
    }

    return it->second;
}

// Used by DebugCore in DebugAdapter
const DebugSymbols::PipelineStageInfo* DebugSymbols::LookupPipelineStage(const size_t pdLocationKey)
{
    const auto it = _pipelineStageSymbols.find(pdLocationKey);

    if (it == _pipelineStageSymbols.end())
    {
        return nullptr;
    }

    return &it->second;
}

// Used by Kanagawa compiler when generating symbols
// Gets struct from map if exists, or creates for new key
DebugSymbols::PipelineStageInfo& DebugSymbols::GetOrCreatePipelineStageInfo(const size_t pdLocationKey)
{
    const auto it = _pipelineStageSymbols.find(pdLocationKey);

    if (it != _pipelineStageSymbols.end())
    {
        return it->second;
    }

    PipelineStageInfo info = {};
    auto result = _pipelineStageSymbols.insert(std::pair<size_t, PipelineStageInfo>(pdLocationKey, info));
    return result.first->second;
}

const std::string DebugSymbols::GetRegisterNameFromLocalsOffset(const size_t key)
{
    std::string result;

    auto it = _localsOffsetToRegName.find(key);

    if (it != _localsOffsetToRegName.end())
    {
        result = it->second;
    }

    return result;
}

const std::string DebugSymbols::GetRegisterNameFromCallIndexOffset(const size_t key)
{
    std::string result;

    auto it = _callIndexOffsetToRegName.find(key);

    if (it != _callIndexOffsetToRegName.end())
    {
        result = it->second;
    }

    return result;
}

const SourceLocation* DebugSymbols::LookupBasicBlockSourceLocation(size_t bbIndex) const
{
    // Likely stage 1, but use the first source info we can find.
    LocationId pdKeyStage0(bbIndex, 0);
    const auto it = _pipelineStageSymbols.upper_bound(pdKeyStage0.Serialize());

    if (it != _pipelineStageSymbols.end())
    {
        LocationId pdKey(it->first);
        if (pdKey._index == bbIndex)
        {
            if (!it->second._sourceLocations.empty())
            {
                // Pick the first, but it will not always be right
                return &it->second._sourceLocations.front();
            }
        }
    }
    return nullptr;
}

// This is a high-runner for the debug symbols writer.
void DebugSymbols::AddSourceVariable(const std::string& name, const std::string& scope, const std::string& dataType,
                                     const size_t bitSize, const SourceLocation& declaredLocation,
                                     const std::string& containingObjectName,
                                     const std::vector<std::string>& pipelinedRegisters)
{
    SourceVariable var(name, scope, dataType, bitSize, declaredLocation, containingObjectName, pipelinedRegisters);

    _sourceVariables.push_back(std::move(var));

    SourceVariable* newVar = &_sourceVariables.back();
    for (const auto& reg : pipelinedRegisters)
    {
        _registerNameToSourceVariable.insert(std::move(std::pair<std::string, SourceVariable*>(reg, newVar)));
    }
}

const SourceVariable* DebugSymbols::GetSourceVariable(const std::string& regName) const
{
    SourceVariable* result = nullptr;

    auto it = _registerNameToSourceVariable.find(regName);

    if (it != _registerNameToSourceVariable.end())
    {
        result = it->second;
    }

    return result;
}

void DebugSymbols::AddSourceObjects(const std::list<SourceContainer>& objects)
{
    for (const auto& object : objects)
    {
        if (!object._scope.empty())
        {
            _sourceObjects.insert(std::pair<std::string, SourceContainer>(object._scope, object));
        }
    }
}

void DebugSymbols::MapMemberVariablesToContainers()
{
    // This function should only be called after both sourceObjects
    // and sourceVariables lists have been populated during ReadDebugSymbols
    assert(!_sourceVariables.empty());

    // Map source variables to parent container
    for (SourceVariable& source : _sourceVariables)
    {
        if (!source._parentInstanceName.empty())
        {
            auto it = _sourceObjects.find(source._parentInstanceName);

            assert(it != _sourceObjects.end());

            SourceContainer& container = it->second;

            source._container = &container;
        }
    }

    // Map containers to parent containers
    for (auto& it : _sourceObjects)
    {
        SourceContainer& container = it.second;

        if (!container._parentName.empty())
        {
            // This container has a parent.
            auto itp = _sourceObjects.find(container._parentName);

            assert(itp != _sourceObjects.end());

            // Check that there is no circular reference
            assert(&itp->second != &container);

            container._parent = &itp->second;
        }
    }
}

void DebugSymbols::AddDetailLevel()
{
    // Determines the number of sections that are filled in the DebugSymbols.
    // The DebugAdapter has an expected level of detail that it requires for Debugger functionality
    // If these two differ, the DebugAdapter will throw a useful error to the user
    _detailLevel = 0;

    if (!_filePathToIndexMap.empty())
    {
        ++_detailLevel;
    }
    if (!_functionSymbols.empty())
    {
        ++_detailLevel;
    }
    if (!_sourceLocationToPipelineStagesMap.empty())
    {
        ++_detailLevel;
    }
    if (!_pipelineStageSymbols.empty())
    {
        ++_detailLevel;
    }
    if (!_sourceVariables.empty())
    {
        ++_detailLevel;
    }
}

size_t WriteDebugSymbols(const DebugSymbols& symbols, const std::string& fileName, const uint64_t symbolHash)
{
    DebugSymbolWriter writer;

    writer.Begin("--- SOURCE HASHES ---");

    writer.End();

    writer.Begin("hash");

    writer.Append("symbolHash", symbolHash);

    writer.End();

    writer.Begin("--- DEBUG SYMBOLS LEVEL OF DETAIL ---");

    writer.End();

    writer.Begin("detail");

    writer.Append("debugSymbolsDetail", symbols._detailLevel);

    writer.Append("debugSymbolsVersion", DEBUG_ADAPTER_SYMBOLS_VERSION);

    writer.End();

    writer.Begin("--- FILE INDEX TO FILENAME ---");

    writer.End();

    for (const auto it : symbols._filePathToIndexMap)
    {
        writer.Begin("file");

        writer.Append("fileName", it.first);

        writer.Append("fileIndex", it.second);

        writer.End();
    }

    writer.Begin("--- NUMBER OF GLOBALS ---");

    writer.End();

    writer.Begin("globals");

    writer.Append("numGlobals", symbols._numGlobals);

    writer.End();

    writer.Begin("--- NUMBER OF MEMORIES ---");

    writer.End();

    writer.Begin("memories");

    writer.Append("numMemories", symbols._numMemories);

    writer.End();

    writer.Begin("--- FUNCTION NAME, BASIC BLOCKS ---");

    writer.End();

    for (const auto& it : symbols._functionSymbols)
    {
        writer.Begin("function");

        writer.Append("name", it.first);

        for (const auto& bb : it.second._basicBlocks)
        {
            writer.Append("basicBlock", bb);
        }

        writer.End();
    }

    writer.Begin("--- FUNCTION CALLEE, CALLERS ---");

    writer.End();

    for (const auto& it : symbols._functionSymbols)
    {
        if (it.second._functionCallerInfo.size() > 0)
        {
            writer.Begin("function");

            writer.Append("name", it.first);

            for (const auto& callers : it.second._functionCallerInfo)
            {
                writer.Append("caller", callers.first.first);

                LocationId id(callers.first.second);

                writer.Append("bbIndex", id._index);

                writer.Append("stage", id._offset);

                writer.Append("callIndexValue", callers.second);
            }

            writer.End();
        }
    }

    // NEW SYMBOLS
    writer.Begin("--- SOURCE LOCATION, KANAGAWA LOCATION ---");

    writer.End();

    for (const auto it : symbols._sourceLocationToPipelineStagesMap)
    {
        writer.Begin("location");

        writer.Append("fileIndex", it.first._fileIndex);

        writer.Append("lineNumber", it.first._lineNumber);

        writer.Append("columnNumber", it.first._columnNumber);

        for (const auto stageName : it.second)
        {
            LocationId id(stageName);

            writer.Append("bbIndex", id._index);

            writer.Append("stage", id._offset);
        }

        writer.End();
    }

    writer.Begin("--- PIPELINE STAGE, DESTINATION STEPINTO ---");

    writer.End();

    for (const auto it : symbols._pipelineStageSymbols)
    {
        if (!it.second._stepInto.empty())
        {
            writer.Begin("stepInto");

            LocationId id(it.first);

            writer.Append("bbIndex", id._index);

            writer.Append("stage", id._offset);
        }
        else
        {
            continue;
        }

        for (const auto successor : it.second._stepInto)
        {
            LocationId id(successor);

            writer.Append("bbIndex", id._index);

            writer.Append("stage", id._offset);
        }

        writer.End();
    }

    writer.Begin("--- PIPELINE STAGE, DESTINATION STEPOVER ---");

    writer.End();

    for (const auto it : symbols._pipelineStageSymbols)
    {
        if (!it.second._stepOver.empty())
        {
            writer.Begin("stepOver");

            LocationId id(it.first);

            writer.Append("bbIndex", id._index);

            writer.Append("stage", id._offset);
        }
        else
        {
            continue;
        }

        for (const auto successor : it.second._stepOver)
        {
            LocationId id(successor);

            writer.Append("bbIndex", id._index);

            writer.Append("stage", id._offset);
        }

        writer.End();
    }

    writer.Begin("--- PIPELINE STAGE, DESTINATION STEPOUT ---");

    writer.End();

    for (const auto it : symbols._pipelineStageSymbols)
    {
        size_t bbIndex = 0;
        if (!it.second._stepOut.empty())
        {
            writer.Begin("stepOut");

            LocationId id(it.first);
            bbIndex = id._index;

            writer.Append("bbIndex", bbIndex);
        }
        else
        {
            continue;
        }

        for (const auto successor : it.second._stepOut)
        {
            LocationId id(successor);

            // The first entry in the list is used to provide the last stage info
            if (bbIndex == id._index)
            {
                writer.Append("lastStage", id._offset);
            }
            else
            {
                writer.Append("bbIndex", id._index);

                writer.Append("stage", id._offset);
            }
        }

        writer.End();
    }

    writer.Begin("--- PIPELINE STAGE, ACCESSED GLOBALS ---");

    writer.End();

    for (const auto it : symbols._pipelineStageSymbols)
    {
        if (it.second._globals.empty())
        {
            continue;
        }

        writer.Begin("registers");

        LocationId id(it.first);

        writer.Append("bbIndex", id._index);

        for (const auto reg : it.second._globals)
        {
            // first: type "memory" or "global"
            // second: reg name
            writer.Append(reg.first, reg.second);
        }

        writer.End();
    }

    writer.Begin("--- MEMORY STRUCTURE ---");

    writer.End();

    for (const auto it : symbols._memoryStructures)
    {
        writer.Begin("structure");

        writer.Append("memory", it.first);

        for (const auto member : it.second._members)
        {
            writer.Append("name", member._name);

            writer.Append("type", member._type);

            writer.Append("offset", member._offset);

            writer.Append("width", member._width);
        }

        writer.End();
    }

    writer.Begin("--- PIPELINE STAGE, ACCESSED LOCALS ---");

    writer.End();

    for (const auto it : symbols._pipelineStageSymbols)
    {
        if (it.second._localsOffset.empty() && !it.second._callIndexOffset.first && !it.second._predicateOffset.first &&
            it.second._intermediatesOffset.empty())
        {
            continue;
        }
        else
        {
            writer.Begin("registers");

            LocationId id(it.first);

            writer.Append("bbIndex", id._index);

            writer.Append("stage", id._offset);
        }

        for (const auto reg : it.second._localsOffset)
        {
            const std::string regName = symbols._localsOffsetToRegName.at(reg.first);

            LocationId id(reg.first);

            writer.Append("offset", id._offset);

            writer.Append("local", regName);

            std::string access;
            if (reg.second == RegisterAccessType::Read)
            {
                access = "Read";
            }
            else if (reg.second == RegisterAccessType::Write)
            {
                access = "Write";
            }
            else
            {
                access = "Wire";
            }

            writer.Append("access", access);
        }

        for (const auto reg : it.second._intermediatesOffset)
        {
            const std::string regName = symbols._intermediateOffsetToRegName.at(reg.first);

            LocationId id(reg.first);

            writer.Append("offset", id._offset);

            writer.Append("intermediate", regName);

            std::string access;
            if (reg.second == RegisterAccessType::Read)
            {
                access = "Read";
            }
            else if (reg.second == RegisterAccessType::Write)
            {
                access = "Write";
            }
            else
            {
                access = "Wire";
            }

            writer.Append("access", access);
        }

        const auto regc = it.second._callIndexOffset;

        const size_t keyc = regc.first;

        if (symbols._callIndexOffsetToRegName.find(keyc) != symbols._callIndexOffsetToRegName.end())
        {
            const std::string regName = symbols._callIndexOffsetToRegName.at(keyc);

            LocationId id(keyc);

            writer.Append("offset", id._offset);

            writer.Append("hidden", regName);

            std::string access;
            if (regc.second == RegisterAccessType::Read)
            {
                access = "Read";
            }
            else if (regc.second == RegisterAccessType::Write)
            {
                access = "Write";
            }
            else
            {
                access = "Wire";
            }

            writer.Append("access", access);
        }

        const auto regp = it.second._predicateOffset;

        const size_t keyp = regp.first;

        if (symbols._predicateOffsetToRegName.find(keyp) != symbols._predicateOffsetToRegName.end())
        {
            const std::string regName = symbols._predicateOffsetToRegName.at(keyp);

            LocationId id(keyp);

            writer.Append("offset", id._offset);

            writer.Append("predicate", regName);

            std::string access;
            if (regp.second == RegisterAccessType::Read)
            {
                access = "Read";
            }
            else if (regp.second == RegisterAccessType::Write)
            {
                access = "Write";
            }
            else
            {
                access = "Wire";
            }

            writer.Append("access", access);
        }

        writer.End();
    }

    writer.Begin("--- SOURCE VARIABLE, SCOPE, DECLARED LOCATION, REGISTERS ---");

    writer.End();

    for (const SourceVariable& src : symbols._sourceVariables)
    {

        if (src._name.empty())
        {
            // Hidden registers like callIndex do not have source variable information
            continue;
        }

        writer.Begin("source");

        writer.Append("name", src._name);

        writer.Append("scope", src._scope);

        writer.Append("type", src._dataType);

        writer.Append("bitsize", src._bitSize);

        writer.Append("file", src._declaredLocation._fileIndex);

        writer.Append("line", src._declaredLocation._lineNumber);

        for (const auto reg : src._pipelinedRegisters)
        {
            writer.Append("register", reg);
        }

        if (!src._parentInstanceName.empty())
        {
            writer.Append("parent", src._parentInstanceName);
        }

        writer.End();
    }

    writer.Begin("--- SOURCE CONTAINERS ---");

    writer.End();

    for (const auto it : symbols._sourceObjects)
    {
        writer.Begin("object");

        writer.Append("scope", it.first);

        writer.Append("name", it.second._instanceName);

        writer.Append("containerName", it.second._containerName);

        for (const auto member : it.second._members)
        {
            writer.Append("member", member._name);

            writer.Append("type", member._type);
        }

        if (it.second._parent != nullptr && !it.second._parent->_scope.empty())
        {
            writer.Append("parent", it.second._parent->_scope);
        }

        writer.End();
    }

    size_t hash = writer.Finalize(fileName);

    return hash;
}

bool ReadDebugSymbols(DebugSymbols& symbols, const std::string& fileName)
{
    DebugSymbolReader reader;

    if (!reader.OpenFile(fileName))
    {
        std::cout << "Error reading Debug Symbols file\n";

        return false;
    }

    if (!reader.SkipToToken("--- SOURCE HASHES ---"))
    {
        std::cout << "Debug Symbols missing source hashes section\n";

        return false;
    }
    else
    {
        std::string str;

        if (!reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            // hash information missing
            return false;
        }

        std::vector<std::string> tokens = reader.SplitCsvLine(str);

        uint64_t symbolHash;

        for (size_t i = 1; i < tokens.size(); i += 2)
        {
            if (tokens[i] == "symbolHash")
            {
                std::istringstream iss(tokens[i + 1]);
                iss >> symbolHash;
            }
        }

        symbols.AddSourceHashes(symbolHash);
    }

    if (!reader.SkipToToken("--- DEBUG SYMBOLS LEVEL OF DETAIL ---"))
    {
        std::cout << "Debug Symbols missing level of detail section\n";

        return false;
    }
    else
    {
        std::string str;

        if (!reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            // detail level information missing
            return false;
        }

        std::vector<std::string> tokens = reader.SplitCsvLine(str);

        size_t detailLevel = std::stoull(tokens[2]);

        if (detailLevel != DEBUG_ADAPTER_LEVEL_OF_DETAIL)
        {
            // detail number does not match DebugAdapter detail level
            return false;
        }

        symbols._detailLevel = detailLevel;

        // Version control
        if ((tokens.size() >= 5) && (tokens[3] == "debugSymbolsVersion"))
        {
            size_t version = std::stoull(tokens[4]);
            if (version != DEBUG_ADAPTER_SYMBOLS_VERSION)
            {
                std::cout << "Debug Symbols version is " << version << " was expecting "
                          << DEBUG_ADAPTER_SYMBOLS_VERSION << std::endl;

                return false;
            }
        }
    }

    if (!reader.SkipToToken("--- FILE INDEX TO FILENAME ---"))
    {
        std::cout << "--- FILE INDEX TO FILENAME --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            std::string filePath;
            size_t fileIndex;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "fileName")
                {
                    filePath = tokens[i + 1];
                }
                else if (tokens[i] == "fileIndex")
                {
                    fileIndex = std::stoull(tokens[i + 1]);
                }
            }

            symbols.AddFilePath(fileIndex, filePath);
        }
    }

    if (!reader.SkipToToken("--- FUNCTION NAME, BASIC BLOCKS ---"))
    {
        std::cout << "--- FUNCTION NAME, BASIC BLOCKS --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            std::string function;

            std::list<size_t> basicBlocks;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "name")
                {
                    function = tokens[i + 1];
                }
                else if (tokens[i] == "basicBlock")
                {
                    basicBlocks.push_back(std::stoull(tokens[i + 1]));
                }
            }

            for (const auto& bb : basicBlocks)
            {
                symbols.AddBasicBlockToFunction(function, bb);
            }
        }
    }

    if (!reader.SkipToToken("--- FUNCTION CALLEE, CALLERS ---"))
    {
        std::cout << "--- FUNCTION CALLEE, CALLERS --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            std::string function, functionCaller;

            size_t callIndexValue, bbIndex, stage;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "name")
                {
                    function = tokens[i + 1];
                }
                else if (tokens[i] == "caller")
                {
                    functionCaller = tokens[i + 1];
                }
                else if (tokens[i] == "bbIndex")
                {
                    bbIndex = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "stage")
                {
                    stage = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "callIndexValue")
                {
                    callIndexValue = std::stoull(tokens[i + 1]);

                    symbols.AddFunctionCallerToFunction(function, functionCaller, bbIndex, stage, callIndexValue);
                }
            }
        }
    }

    if (!reader.SkipToToken("--- SOURCE LOCATION, KANAGAWA LOCATION ---"))
    {
        std::cout << "--- SOURCE LOCATION, KANAGAWA LOCATION --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            size_t fileIndex;
            size_t lineNumber;
            size_t columnNumber = 0;
            size_t bbIndex;
            size_t stage;
            std::list<std::pair<size_t, size_t>> pipelineStages;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "fileIndex")
                {
                    fileIndex = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "lineNumber")
                {
                    lineNumber = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "columnNumber")
                {
                    columnNumber = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "bbIndex")
                {
                    bbIndex = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "stage")
                {
                    stage = std::stoull(tokens[i + 1]);

                    symbols.AddSourceLocationToPipelineStage(fileIndex, lineNumber, columnNumber, bbIndex, stage);
                }
            }
        }
    }

    if (!reader.SkipToToken("--- PIPELINE STAGE, DESTINATION STEPINTO ---"))
    {
        std::cout << "--- PIPELINE STAGE, DESTINATION STEPINTO --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            size_t bbIndex;
            size_t stage;
            std::list<std::pair<size_t, size_t>> stepIntoStages;
            std::pair<size_t, size_t> stepIntoStage;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "bbIndex" && i == 1)
                {
                    bbIndex = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "stage" && i == 3)
                {
                    stage = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "bbIndex")
                {
                    stepIntoStage.first = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "stage")
                {
                    stepIntoStage.second = std::stoull(tokens[i + 1]);

                    stepIntoStages.push_back(stepIntoStage);
                }
            }

            symbols.AddStepInto(bbIndex, stage, stepIntoStages);
        }
    }

    if (!reader.SkipToToken("--- PIPELINE STAGE, DESTINATION STEPOVER ---"))
    {
        std::cout << "--- PIPELINE STAGE, DESTINATION STEPOVER --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            size_t bbIndex;
            size_t stage;
            std::list<std::pair<size_t, size_t>> stepOverStages;
            std::pair<size_t, size_t> stepOverStage;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "bbIndex" && i == 1)
                {
                    bbIndex = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "stage" && i == 3)
                {
                    stage = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "bbIndex")
                {
                    stepOverStage.first = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "stage")
                {
                    stepOverStage.second = std::stoull(tokens[i + 1]);

                    stepOverStages.push_back(stepOverStage);
                }
            }

            symbols.AddStepOver(bbIndex, stage, stepOverStages);
        }
    }

    if (!reader.SkipToToken("--- PIPELINE STAGE, DESTINATION STEPOUT ---"))
    {
        std::cout << "--- PIPELINE STAGE, DESTINATION STEPOUT --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            size_t bbIndex;
            size_t lastStage = 0;
            std::list<std::pair<size_t, size_t>> stepOutStages;
            std::pair<size_t, size_t> stepOutStage;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "bbIndex" && i == 1)
                {
                    bbIndex = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "lastStage")
                {
                    lastStage = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "bbIndex")
                {
                    stepOutStage.first = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "stage")
                {
                    stepOutStage.second = std::stoull(tokens[i + 1]);

                    stepOutStages.push_back(stepOutStage);
                }
            }

            symbols.AddStepOut(bbIndex, lastStage, stepOutStages);
        }
    }

    if (!reader.SkipToToken("--- PIPELINE STAGE, ACCESSED GLOBALS ---"))
    {
        std::cout << "--- PIPELINE STAGE, ACCESSED GLOBALS --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            size_t bbIndex;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "bbIndex")
                {
                    bbIndex = std::stoull(tokens[i + 1]);
                }
                else
                {
                    std::string type = tokens[i];
                    std::string name = tokens[i + 1];
                    symbols.AddGlobalRegisterAccessedByBasicBlock(bbIndex, type, name);
                }
            }
        }
    }

    if (!reader.SkipToToken("--- MEMORY STRUCTURE ---"))
    {
        std::cout << "--- MEMORY STRUCTURE --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        std::map<std::string, MemoryStructure> memoryStructuresMap;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            std::string memoryName, name, type;
            MemoryStructure memStructure;
            size_t offset, width;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "memory")
                {
                    memoryName = tokens[i + 1];
                }
                else if (tokens[i] == "name")
                {
                    name = tokens[i + 1];
                }
                else if (tokens[i] == "type")
                {
                    type = tokens[i + 1];
                }
                else if (tokens[i] == "offset")
                {
                    offset = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "width")
                {
                    width = std::stoull(tokens[i + 1]);
                    memStructure._members.push_back({name, type, offset, width});
                }
            }
            memoryStructuresMap.insert({memoryName, memStructure});
        }
        symbols.AddMemoryStructure(memoryStructuresMap);
    }

    if (!reader.SkipToToken("--- PIPELINE STAGE, ACCESSED LOCALS ---"))
    {
        std::cout << "--- PIPELINE STAGE, ACCESSED LOCALS --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);

            size_t stage;
            size_t bbIndex;
            size_t offset;
            std::string regName;
            RegisterAccessType accessType;
            RegisterSourceType sourceType;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "bbIndex")
                {
                    bbIndex = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "stage")
                {
                    stage = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "offset")
                {
                    offset = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "local")
                {
                    regName = tokens[i + 1];

                    sourceType = RegisterSourceType::Local;
                }
                else if (tokens[i] == "hidden")
                {
                    regName = tokens[i + 1];

                    sourceType = RegisterSourceType::Hidden;
                }
                else if (tokens[i] == "predicate")
                {
                    regName = tokens[i + 1];

                    sourceType = RegisterSourceType::Predicate;
                }
                else if (tokens[i] == "intermediate")
                {
                    regName = tokens[i + 1];

                    sourceType = RegisterSourceType::Intermediate;
                }
                else if (tokens[i] == "access")
                {
                    if (tokens[i + 1] == "Write")
                    {
                        accessType = RegisterAccessType::Write;
                    }
                    else if (tokens[i + 1] == "Read")
                    {
                        accessType = RegisterAccessType::Read;
                    }
                    else
                    {
                        accessType = RegisterAccessType::Wire;
                    }
                    symbols.AddLocalRegisterAccessedByPipelineStage(bbIndex, stage, regName, offset, accessType,
                                                                    sourceType);
                }
            }
        }
    }

    if (!reader.SkipToToken("--- SOURCE VARIABLE, SCOPE, DECLARED LOCATION, REGISTERS ---"))
    {
        std::cout << "--- SOURCE VARIABLE, SCOPE, DECLARED LOCATION, REGISTERS --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);
            std::string name, scope, type, containingObjectName = "";
            size_t bitSize = 0;
            SourceLocation declaredLocation = {0, 0, 0};
            std::vector<std::string> pipelineRegisterNames;

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "name")
                {
                    name = tokens[i + 1];
                }
                else if (tokens[i] == "scope")
                {
                    scope = tokens[i + 1];
                }
                else if (tokens[i] == "type")
                {
                    type = tokens[i + 1];
                }
                else if (tokens[i] == "bitsize")
                {
                    bitSize = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "file")
                {
                    declaredLocation._fileIndex = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "line")
                {
                    declaredLocation._lineNumber = std::stoull(tokens[i + 1]);
                }
                else if (tokens[i] == "register")
                {
                    pipelineRegisterNames.push_back(tokens[i + 1]);
                }
                else if (tokens[i] == "parent")
                {
                    containingObjectName = tokens[i + 1];
                }
            }
            symbols.AddSourceVariable(name, scope, type, bitSize, declaredLocation, containingObjectName,
                                      pipelineRegisterNames);
        }
    }

    if (!reader.SkipToToken("--- SOURCE CONTAINERS ---"))
    {
        std::cout << "--- SOURCE CONTAINERS --- section does not exist\n";

        return false;
    }
    else
    {
        std::string str;
        std::list<SourceContainer> sourceObjects;
        std::list<DataSubsetInfo> members;

        // This automatically stops when end of section has been reached
        while (reader.GetNextLine(str, true /* checkForEndOfSection */))
        {
            std::vector<std::string> tokens = reader.SplitCsvLine(str);
            std::string scope, instanceName, containerName, parentName = "";
            std::string memberName, memberType = "";
            members.clear();

            for (size_t i = 1; i < tokens.size(); i += 2)
            {
                if (tokens[i] == "scope")
                {
                    scope = tokens[i + 1];
                }
                else if (tokens[i] == "name")
                {
                    instanceName = tokens[i + 1];
                }
                else if (tokens[i] == "containerName")
                {
                    containerName = tokens[i + 1];
                }
                else if (tokens[i] == "member")
                {
                    memberName = tokens[i + 1];
                }
                else if (tokens[i] == "type")
                {
                    memberType = tokens[i + 1];
                    members.push_back({memberName, memberType, 0, 0});
                }
                else if (tokens[i] == "parent")
                {
                    parentName = tokens[i + 1];
                }
            }
            sourceObjects.push_back(SourceContainer(containerName, instanceName, scope));
            SourceContainer* source = &sourceObjects.back();
            source->_parentName = parentName;
            source->_members = members;
        }
        symbols.AddSourceObjects(sourceObjects);
        symbols.MapMemberVariablesToContainers();
    }

    // This needs two sections read: "FUNCTION NAME, BASIC BLOCKS" and "SOURCE LOCATION, KANAGAWA LOCATION"
    for (auto& it : symbols._functionSymbols)
    {
        const std::string functionName = it.first;
        auto& functionInfo = it.second;

        // Set the value of _firstSourcePipelineStage to the earliest source line for this function.
        size_t bbAndStage = 0; // Invalid since bbIndexes start at 1

        const auto bbIndex = functionInfo._basicBlocks.front();

        // Likely stage 1, but use the first source info we can find.
        LocationId pdKeyStage0(bbIndex, 0);
        const auto it2 = symbols._pipelineStageSymbols.upper_bound(pdKeyStage0.Serialize());

        if (it2 != symbols._pipelineStageSymbols.end())
        {
            LocationId pdKey(it2->first);
            if (pdKey._index == bbIndex)
            {
                for (const auto& it3 : it2->second._sourceLocations)
                {
                    bbAndStage = pdKey.Serialize();
                    break;
                }
            }
        }
        functionInfo._firstSourcePipelineStage = bbAndStage;
    }

    return true;
}

// Opens a file for writing
// throws an exception on failure (for example, the file is locked)
// This prevents Kanagawa from silently failing when it cannot write to an output file
void OpenOutputFileStream(std::ofstream& str, const char* const fileName, bool binaryMode)
{
    str.exceptions(std::ofstream::failbit | std::ofstream::badbit);

    try
    {
        str.open(fileName, (binaryMode) ? std::ios_base::out | std::ios_base::binary : std::ios_base::out);
    }
    catch (...)
    {
        std::cout << "Failed to open output file (is it in use by another process?): " << fileName << "\n";
        throw;
    }
}

// Read the contents of a file into a filestream
bool OpenInputFileStream(std::ifstream& str, const char* const fileName)
{
    // Do not throw on failure, return false so that DebugAdapter can gracefully notify the user
    str.open(fileName);

    return str.is_open();
}
