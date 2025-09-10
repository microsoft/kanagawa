// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#define DEBUG_ADAPTER_LEVEL_OF_DETAIL 5
#define DEBUG_ADAPTER_SYMBOLS_VERSION 1

#ifdef _WIN32
struct comp
{
    // Use case-insensitive file path compare
    bool operator()(const std::string& lhs, const std::string& rhs) const
    {
#ifdef _MSC_VER
        return _stricmp(lhs.c_str(), rhs.c_str()) < 0;
#else
        return strcasecmp(lhs.c_str(), rhs.c_str()) < 0;
#endif
    }
};
typedef std::map<std::string, size_t, comp> file_path_index_map_t;
#else
typedef std::map<std::string, size_t> file_path_index_map_t;
#endif

struct SourceLocation
{
    SourceLocation(const size_t fileIndex, const size_t lineNumber, const size_t columnNumber)
        : _fileIndex(fileIndex), _lineNumber(lineNumber), _columnNumber(columnNumber)
    {
    }

    SourceLocation(const SourceLocation& mE) = default;
    SourceLocation& operator=(const SourceLocation& mE) = default;
    SourceLocation(SourceLocation&& mE) noexcept = default;
    SourceLocation& operator=(SourceLocation&& mE) noexcept = default;

    size_t _fileIndex;

    size_t _lineNumber;

    size_t _columnNumber;

    bool operator<(const SourceLocation& rhs) const
    {
        return std::tie(_fileIndex, _lineNumber, _columnNumber) <
               std::tie(rhs._fileIndex, rhs._lineNumber, rhs._columnNumber);
    }

    bool operator==(const SourceLocation& rhs) const
    {
        return std::tie(_fileIndex, _lineNumber, _columnNumber) ==
               std::tie(rhs._fileIndex, rhs._lineNumber, rhs._columnNumber);
    }

    bool operator!=(const SourceLocation& rhs) const
    {
        return std::tie(_fileIndex, _lineNumber, _columnNumber) !=
               std::tie(rhs._fileIndex, rhs._lineNumber, rhs._columnNumber);
    }
};

struct DataSubsetInfo
{
    DataSubsetInfo(const DataSubsetInfo& mE) = default;
    DataSubsetInfo& operator=(const DataSubsetInfo& mE) = default;
    DataSubsetInfo(DataSubsetInfo&& mE) noexcept = default;
    DataSubsetInfo& operator=(DataSubsetInfo&& mE) noexcept = default;

    std::string _name;

    std::string _type;

    size_t _offset;

    size_t _width;
};

struct MemoryStructure
{
    std::vector<DataSubsetInfo> _members;
};

struct SourceContainer
{
    SourceContainer(const std::string& containerName, const std::string& instanceName, const std::string& scope)
        : _containerName(containerName), _instanceName(instanceName), _scope(scope), _members({}), _parentName(""),
          _parent(nullptr)
    {
    }

    SourceContainer(const SourceContainer& mE) = default;
    SourceContainer& operator=(const SourceContainer& mE) = default;
    SourceContainer(SourceContainer&& mE) noexcept = default;
    SourceContainer& operator=(SourceContainer&& mE) noexcept = default;

    // Class/struct type name
    std::string _containerName;

    std::string _instanceName;

    std::string _scope;

    // Member name, member type, data width
    std::list<DataSubsetInfo> _members;

    std::string _parentName;

    struct SourceContainer* _parent;
};

struct SourceVariable
{

    SourceVariable() : _bitSize(0), _declaredLocation({0, 0, 0}), _container(nullptr) {}

    SourceVariable(const std::string& name, const std::string& scope, const std::string& dataType, const size_t bitSize,
                   const SourceLocation& declaredLocation, const std::string& containingObjectName = {})
        : _name(name), _scope(scope), _dataType(dataType), _bitSize(bitSize), _declaredLocation(declaredLocation),
          _parentInstanceName(containingObjectName), _container(nullptr)
    {
    }

    SourceVariable(const std::string& name, const std::string& scope, const std::string& dataType, const size_t bitSize,
                   const SourceLocation& declaredLocation, const std::string& containingObjectName,
                   const std::vector<std::string>& pipelinedRegisters)
        : _name(name), _scope(scope), _dataType(dataType), _bitSize(bitSize), _declaredLocation(declaredLocation),
          _pipelinedRegisters(pipelinedRegisters), _parentInstanceName(containingObjectName), _container(nullptr)
    {
    }

    SourceVariable(const SourceVariable& mE) = default;
    SourceVariable& operator=(const SourceVariable& mE) = default;
    SourceVariable(SourceVariable&& mE) noexcept = default;
    SourceVariable& operator=(SourceVariable&& mE) noexcept = default;

    std::string _name;

    std::string _scope;

    std::string _dataType;

    size_t _bitSize;

    SourceLocation _declaredLocation;

    std::string _parentInstanceName;

    std::vector<std::string> _pipelinedRegisters;

    SourceContainer* _container;
};

class DebugSymbols
{
  public:
    // Symbols
    void AddFilePath(const size_t index, const std::string& filePath);

    void AddSourceHashes(const uint64_t symbolHash);

    void AddBasicBlockToFunction(const std::string& functionName, const size_t basicBlock);

    void AddFunctionCallerToFunction(const std::string& functionName, const std::string& functionCaller,
                                     const size_t bbIndex, const size_t atomicSequence, const size_t callIndex);

    void SetNumGlobalsAndMems(const size_t numGlobals, const size_t numMemories);

    void AddSourceLocationToPipelineStage(const size_t fileIndex, const size_t lineNumber, const size_t columnNumber,
                                          const size_t bbIndex, const size_t atomicSequence);

    void AddStepInto(const size_t bbIndex, const size_t atomicSequence,
                     const std::list<std::pair<size_t, size_t>>& successorStages);

    void AddStepOver(const size_t bbIndex, const size_t atomicSequence,
                     const std::list<std::pair<size_t, size_t>>& successorStages);

    void AddStepOut(const size_t bbIndex, const size_t lastAtomicSequence,
                    const std::list<std::pair<size_t, size_t>>& successorStages);

    void AddGlobalRegisterAccessedByBasicBlock(const size_t bbIndex, const std::string& type, const std::string& name);

    void AddLocalRegisterAccessedByPipelineStage(const size_t bbIndex, const size_t atomicSequence,
                                                 const std::string& name, const size_t offset,
                                                 const RegisterAccessType access, const RegisterSourceType sourceType);

    void AddMemoryStructure(const std::map<std::string, MemoryStructure>& memoryStructureMap);

    void AddSourceVariable(const std::string& name, const std::string& scope, const std::string& dataType,
                           const size_t bitSize, const SourceLocation& declaredLocation,
                           const std::string& containingObjectName, const std::vector<std::string>& pipelinedRegisters);

    void AddSourceObjects(const std::list<SourceContainer>& objects);

    void MapMemberVariablesToContainers();

    void AddDetailLevel();

    // Debug Core
    int GetIndexFromFile(const std::string& fileName);

    const std::string GetFileFromIndex(const size_t index);

    const file_path_index_map_t& GetProgramSourceFiles();

    const std::string GetFunctionFromPipelineStage(const size_t pdLocationKey);

    bool FunctionHasCallers(const std::string& functionName);

    const std::pair<std::string, size_t> GetFunctionCaller(const std::string& functionName, const size_t callIndex);

    const std::set<size_t> GetKanagawaLocationFromSource(const size_t fileIndex, const size_t lineNumber,
                                                         const std::optional<size_t> columnNumber);

    const std::pair<SourceLocation, SourceLocation> GetSourceRangeFromKanagawaLocation(const size_t pdLocationKey);

    const std::string GetRegisterNameFromLocalsOffset(const size_t key);

    const std::string GetRegisterNameFromCallIndexOffset(const size_t key);

    const SourceVariable* GetSourceVariable(const std::string& regName) const;

    const std::list<size_t> GetStepOut(const size_t bbIndex);

    const MemoryStructure* GetMemoryStructure(const std::string& memoryRegisterName) const;

    typedef std::pair<size_t, RegisterAccessType> RegisterAccessOffset;

    struct PipelineStageInfo
    {
        // Multiple source lines can map to one pipeline stage
        // I.e. lines within an atomic block
        std::list<SourceLocation> _sourceLocations;
        std::list<size_t> _stepInto;

        std::list<size_t> _stepOver;

        std::list<size_t> _stepOut;

        std::list<size_t> _stepBack;

        // "global" or "memory", name
        std::list<std::pair<std::string, std::string>> _globals;

        // <localsXX << 32 | offset, registerAccessType>
        std::list<RegisterAccessOffset> _localsOffset;

        std::list<RegisterAccessOffset> _intermediatesOffset;

        RegisterAccessOffset _callIndexOffset;

        RegisterAccessOffset _predicateOffset;
    };

    struct FunctionInfo
    {
        std::list<size_t> _basicBlocks;

        // Function's source start (for SetFunctionBreakpoints' use)
        size_t _firstSourcePipelineStage;

        // List of <<function caller, caller pipeline stage>, callIndexValue>
        std::list<std::pair<std::pair<std::string, size_t>, size_t>> _functionCallerInfo;
    };

    const PipelineStageInfo* LookupPipelineStage(const size_t pdLocationKey);

    PipelineStageInfo& GetOrCreatePipelineStageInfo(const size_t pdLocationKey);

    const FunctionInfo* LookupFunctionInfo(const std::string& functionName);

    FunctionInfo& GetOrCreateFunctionInfo(const std::string& functionName);

    size_t GetStageForRegister(const std::string& regName);

    const SourceLocation* LookupBasicBlockSourceLocation(size_t bbIndex) const;

    std::map<size_t, PipelineStageInfo> _pipelineStageSymbols;

    std::map<std::string, size_t> _registerNameToStage;

    std::map<std::string, FunctionInfo> _functionSymbols;

    file_path_index_map_t _filePathToIndexMap;

    std::map<size_t, std::string> _indexToFilePathMap;

    std::map<SourceLocation, std::set<size_t>> _sourceLocationToPipelineStagesMap;

    std::map<size_t, std::string> _localsOffsetToRegName;

    std::map<size_t, std::string> _callIndexOffsetToRegName;

    std::map<size_t, std::string> _predicateOffsetToRegName;

    std::map<size_t, std::string> _intermediateOffsetToRegName;

    std::map<std::string, SourceVariable*> _registerNameToSourceVariable;

    std::list<SourceVariable> _sourceVariables;

    std::map<std::string, SourceContainer> _sourceObjects;

    /* memory HW name, structure info if not flat data */
    std::map<std::string, MemoryStructure> _memoryStructures;

    size_t _numGlobals;

    size_t _numMemories;

    std::map<std::string, std::string> _globalRegisterSymbolNames;

    // Level of detail in the debug symbols file
    // Currently, debug symbols have the most level of detail
    // when Kanagawa source compiled with --debug -O0
    // This level of detail is what is required by the Debugger
    size_t _detailLevel;

    uint64_t _symbolHash;
};

size_t WriteDebugSymbols(const DebugSymbols& symbols, const std::string& fileName, const uint64_t symbolHash);

void OpenOutputFileStream(std::ofstream& str, const char* const fileName, bool binaryMode = false);

bool ReadDebugSymbols(DebugSymbols& symbols, const std::string& fileName);

bool OpenInputFileStream(std::ifstream& str, const char* const fileName);

//
//  Specs for the Fowler-Noll-Vo hash functions:
//  https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
//
//  For our uses we need a hash function that minimizes the collisions, e.g. the chance that
//  different sources will hash to the same value.
//  The following post has some discussion on the subject, with FNV-1 best at collisions on text:
//  https://softwareengineering.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed
//

template <size_t const c_numBits> uint64_t Fnv1Hash(const std::string& src)
{
    const uint64_t FNV_offset_basis = 14695981039346656037ull;
    const uint64_t FNV_prime = 1099511628211ull;

    static_assert(c_numBits == 64, "Fnv1Hash only implemented for 64bits");

    uint64_t hash = FNV_offset_basis;
    for (unsigned char c : src)
    {
        hash = hash * FNV_prime;
        hash = hash ^ c;
    }
    return hash;
}

class DebugSymbolWriter
{
  public:
    void Begin(const std::string& title) { _str << title; }

    template <typename T> void Append(const std::string& key, const T& value) { _str << "," << key << "," << value; }

    void End() { _str << std::endl; }

    size_t Finalize(const std::string& fileName)
    {
        const std::string contents = _str.str();

        // Compute hash of symbol file contents
        const size_t hash = Fnv1Hash<64>(contents);

        // Write to the file
        std::ofstream outputStream;
        OpenOutputFileStream(outputStream, fileName.c_str());

        // prepend hash
        outputStream << "hash," << std::hex << hash << std::dec << std::endl;

        outputStream << contents;

        outputStream.close();

        return hash;
    }

  private:
    std::ostringstream _str;
};

class DebugSymbolReader
{
  public:
    bool OpenFile(const std::string& fileName) { return OpenInputFileStream(_inputStream, fileName.c_str()); }

    std::vector<std::string> SplitCsvLine(const std::string& str)
    {
        std::vector<std::string> result;

        std::istringstream iss(str);

        std::string token;

        while (getline(iss, token, ','))
        {
            result.push_back(token);
        }

        return result;
    }

    bool SkipToToken(const std::string& token)
    {
        bool result = false;

        std::string str;
        while (GetNextLine(str))
        {
            if (str.find(token) != std::string::npos)
            {
                result = true;

                // Go to the beginning of the line
                // _inputStream.seekg(_pos, std::ios_base::beg);

                break;
            }
        }

        return result;
    }

    bool GetNextLine(std::string& str, bool checkForEndOfSection = false)
    {
        if (_inputStream.eof())
        {
            return false;
        }

        bool result = true;

        _pos = _inputStream.tellg();

        getline(_inputStream, str);

        // Check if next section has been reached
        if (checkForEndOfSection && str.find("---") != std::string::npos)
        {
            // Go to the beginning of the line
            _inputStream.seekg(_pos, std::ios_base::beg);

            result = false;
        }

        return result;
    }

    ~DebugSymbolReader() { _inputStream.close(); }

  private:
    std::ifstream _inputStream;

    size_t _pos;
};
