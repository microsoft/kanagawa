// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

typedef std::map<size_t, std::map<std::string, uint32_t>> BasicBlockToLocalsMap;

typedef std::map<size_t, std::vector<bool>> BasicBlockToEmptyStagesMap;

std::string GetBasicBlockName(const BasicBlock& basicBlock);

std::string GetExternalClassInstanceInterfaceName(const ExternalClassInstance& externModule);

std::string GetExternalClassInstanceTypeName(const ExternalClassInstance& externModule);

std::string GetRegisterName(const size_t registerIndex, const RegisterDescription& regDesc);

std::string GlobalViewTempName(const Program& program, const size_t globalViewReg, const size_t localReg);

struct Section
{
    bool _indent;
    bool _isActive;
    std::string _openingString;
    std::string _closingString;
};

class SourceWriter;
class PopExistingAutoSectionAndRestart;

// This is a class for an arbitrary Verilog section (e.g. foo begin -> end) that must be activated
// before they are emitted.  This allows empty sections to be squashed before getting written out.
// This is not intended for RAII, but rather the execution-time creation and deletion of items on
// SourceWriter._autoSectionActivationStack.  For RAII, use AutoSectionRAII.
class AutoSection
{
  public:
    AutoSection(SourceWriter& sourceWriter, bool indent, std::string openingString, std::string closingString);
    ~AutoSection();
    void Activate();
    bool isActive();
    bool emptyClosingString();

  private:
    friend class SourceWriter;

    SourceWriter& _sourceWriter;
    Section _section;
};

// This is a RAII class for an arbitrary Verilog section (e.g. foo begin -> end) that must be activated
// before they are emitted.
class AutoSectionRAII
{
  public:
    AutoSectionRAII(SourceWriter& sourceWriter, bool indent, std::string openingString, std::string closingString);
    ~AutoSectionRAII();

  private:
    SourceWriter& _sourceWriter;
};

// This is a RAII class for closing the top-most Verilog section on the SourceWriter stack and then restarting it later
class PopExistingAutoSectionAndRestart
{
  public:
    PopExistingAutoSectionAndRestart(SourceWriter& sourceWriter);
    ~PopExistingAutoSectionAndRestart();

  private:
    SourceWriter& _sourceWriter;
    Section _section;
};

// Generate interface to an object that can write output source
class SourceWriter
{
    friend class RedirectableSourceWriter;

  public:
    SourceWriter() : _indent(0), _lineNumber(0) { Reset(); }

    std::ostream& Str()
    {
        std::ostream& str = GetStream();

        // Try to activate everything in the stack, starting from the bottom
        for (AutoSection& p : _autoSectionActivationStack)
        {
            p.Activate();
        }

        // Add newline and indenting
        str << "\n";

        ++_lineNumber;

        for (size_t i = 0; i < (_indent * c_spacesPerTab); ++i)
        {
            str << " ";
        }

        return str;
    }

    void Indent() { _indent++; }

    void Unindent() { _indent--; }

    size_t GetLineNumber() const { return _lineNumber; }

    void PushAutoSection(AutoSection section) { _autoSectionActivationStack.push_back(section); }

    // Pop sections off the stack until we pop a section with a non-empty closing string
    void PopAutoSection()
    {
        bool done = false;
        while (!done)
        {
            assert(!_autoSectionActivationStack.empty());
            if (!_autoSectionActivationStack.back().emptyClosingString())
            {
                done = true;
            }
            _autoSectionActivationStack.pop_back();
        }
    }

    // Pop sections off the stack until we see a section with a non-empty closing string.
    // Leave that section on the stack
    void PopAutoSectionUntilNonEmptyClose()
    {
        bool done = false;
        while (!done)
        {
            assert(!_autoSectionActivationStack.empty());
            if (!_autoSectionActivationStack.back().emptyClosingString())
            {
                done = true;
            }
            else
            {
                _autoSectionActivationStack.pop_back();
            }
        }
    }

    // Return the top-most section with a non-empty closing string
    Section* getTopNonEmptySection()
    {
        for (auto it = _autoSectionActivationStack.rbegin(); it != _autoSectionActivationStack.rend(); it++)
        {
            if (!(it->emptyClosingString()))
            {
                return &(it->_section);
            }
        }
        assert(false);
        return nullptr;
    }

    // Unlike Str(), this won't check _autoSectionActivationStack.
    // It should only be used by AutoSection
    std::ostream& Printer()
    {
        std::ostream& str = GetStream();

        // Add newline and indenting
        str << "\n";

        ++_lineNumber;

        for (size_t i = 0; i < (_indent * c_spacesPerTab); ++i)
        {
            str << " ";
        }

        return str;
    }

    size_t getStackSize() { return _autoSectionActivationStack.size(); }

  protected:
    virtual std::ostream& GetStream() = 0;

    void Reset()
    {
        _indent = 0;
        _lineNumber = 2;
    }

  private:
    size_t _indent;
    size_t _lineNumber;

    static const size_t c_spacesPerTab = 4;

    std::list<AutoSection> _autoSectionActivationStack;
};

class RedirectableSourceWriter : public SourceWriter
{
  public:
    RedirectableSourceWriter() : _otherWriter(nullptr) {}

    // Used to temporarily redirect output ot another SourceWriter
    void BeginRedirect(SourceWriter& otherWriter)
    {
        assert(_otherWriter == nullptr);
        _otherWriter = &otherWriter;
    }

    void EndRedirect()
    {
        assert(_otherWriter != nullptr);
        _otherWriter = nullptr;
    }

  protected:
    virtual std::ostream& GetStreamImpl() = 0;

    std::ostream& GetStream() override { return _otherWriter ? _otherWriter->GetStream() : GetStreamImpl(); }

  private:
    SourceWriter* _otherWriter;
};

// Writes source to an output file
class FileSourceWriter : public RedirectableSourceWriter
{
  public:
    FileSourceWriter(const char* const fileName) { Open(fileName); }

    void Open(const char* const fileName)
    {
        if (_str.is_open())
        {
            _str.close();
        }

        OpenOutputFileStream(_str, fileName);

        Reset();
    }

  protected:
    std::ostream& GetStreamImpl() override { return _str; }

  private:
    std::ofstream _str;
};

// Writes source to a string
class StringSourceWriter : public SourceWriter
{
  public:
    StringSourceWriter() {}

    std::string GetString() const { return _str.str(); }

    void Reset() { _str = std::ostringstream(); }

  protected:
    std::ostream& GetStream() override { return _str; }

  private:
    std::ostringstream _str;
};

// Writes (a,b,c,d)
class CsvListWriter
{
  public:
    CsvListWriter() : _empty(true) {}

    std::ostream& Str()
    {
        if (!_empty)
        {
            _str << ", ";
        }

        _empty = false;

        return _str;
    }

    std::string Result() { return _str.str(); }

  private:
    std::ostringstream _str;
    bool _empty;
};

class AutoIndent
{
  public:
    AutoIndent(SourceWriter& sourceWriter) : _sourceWriter(sourceWriter) { _sourceWriter.Indent(); }

    ~AutoIndent() { _sourceWriter.Unindent(); }

  private:
    AutoIndent& operator=(const AutoIndent& rhs);

    SourceWriter& _sourceWriter;
};

// The set of soft registers allocated to a function
static const uint64_t c_invalidSoftRegister = std::numeric_limits<uint64_t>::max();

struct FunctionSoftRegisters
{
    // When this soft register is written, and new function call is enqueued
    uint64_t _writeEnable;

    // When this soft register is read, 0 is return if the input fifo is not full
    uint64_t _inputFifoFull;

    // When this soft register is read, 0 is returned if the result fifo is empty
    // For synchronous functions only
    uint64_t _outputFifoEmpty;

    // When this soft register is read, a return is dequeued
    // For synchronous functions only
    uint64_t _readEnable;

    // When this soft register is read, the function return value is returned
    // For functions that return a value only
    uint64_t _returnValue;

    // When soft register _baseParameter + i is written
    // the value of parameter i is set
    uint64_t _baseParameter;
};

bool ContainsAssert(const BasicBlock& basicBlock);

std::set<size_t> GetRefByGlobalViews(const Program& program);
std::set<size_t> GetGlobalRequireNext(const Program& program);
// Returns the set of global variables that are read by a basic block
std::set<size_t> GetGlobalsRead(const Program& program, const BasicBlock& basicBlock);
bool DetectOpsUseNextValue(const Operation& op);
std::set<size_t> GetGlobalsReadWithoutNext(const Program& program, const BasicBlock& basicBlock,
                                           const std::set<size_t>& globalsRequiringNext);
std::set<size_t> GetGlobalsReadWithNext(const Program& program, const BasicBlock& basicBlock,
                                        const std::set<size_t>& globalsRequiringNext);

std::set<size_t> GetGlobalRegisterSources(const Program& program, const OperationList& operations);

std::set<size_t> GetLocalRegisters(const Program& program, const OperationList& operations);

// Returns the set of global variables that are written by a basic block
// The returned pairs contain { registerIndex, writeIndex }
std::set<std::pair<size_t, size_t>> GetGlobalsWritten(const Program& program, const BasicBlock& basicBlock);

std::set<size_t> GetGlobalViewsRead(const Program& program, const BasicBlock& basicBlock);

std::set<size_t> GetPropagationFifos(const Program& program, const BasicBlock& basicBlock);

using PropagationFifoSlices = std::map<size_t, std::map<size_t, size_t>>;
PropagationFifoSlices GetPropagationFifoSlices(const Program& program, const BasicBlock& basicBlock);

const std::string StageFunctionName(const BasicBlock& basicBlock, const size_t stageIndex);

bool IsStageEmpty(const BasicBlock& basicBlock, const size_t stageIndex,
                  const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap);

const std::vector<std::pair<const Stage*, size_t>>
GetStagesInPipeline(const BasicBlock& basicBlock, const size_t atomicSequence,
                    const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap);

// Returns source locations that this pipeline stage stems from
const std::set<FileAndLineNumber>
GetLocationsPerPipelineStage(const BasicBlock& basicBlock, const size_t atomicSequence,
                             const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap);

// Returns local and global registers accessed by a pipeline stage within a basic block
const std::list<std::pair<size_t, RegisterAccessType>>
GetRegistersAccessedByStage(const Program& program, const BasicBlock& basicBlock, const size_t atomicSequence,
                            const BasicBlockToEmptyStagesMap& allBlocksEmptyStagesMap,
                            std::list<std::pair<size_t, RegisterAccessType>>& temporaries);

std::set<size_t> GetWrittenFifos(const BasicBlock& basicBlock);
std::set<size_t> GetBackpressureFifos(const Program& program, const BasicBlock& basicBlock);
std::set<size_t> GetWrittenAndBackpressureFifos(const Program& program, const BasicBlock& basicBlock);

struct MemoryAccessRecord
{
    size_t _memoryIndex;
    size_t _portIndex;
};

bool operator<(const MemoryAccessRecord& lhs, const MemoryAccessRecord& rhs);

std::set<MemoryAccessRecord> GetMemoriesRead(const Program& program, const BasicBlock& basicBlock);

std::set<MemoryAccessRecord> GetMemoriesWritten(const Program& program, const BasicBlock& basicBlock);

void GetMemoryStructures(const Program& program, std::map<std::string, MemoryStructure>& memoryStructures);

std::set<size_t> GetExternalClassInstanceCallsOfType(const Program& program, const BasicBlock& basicBlock,
                                                     const ExternalModuleCallType type);

std::set<size_t> GetCrossBBExternalClassInstanceCalls(const Program& program, const BasicBlock& basicBlock);

// Returns the successor BasicBlocks for a given BasicBlock and the type of the successor based on
// the Enqueue operation
std::vector<std::pair<const Operation&, const BasicBlock*>> GetSuccessorBasicBlocks(const Program& program,
                                                                                    const BasicBlock& basicBlock);

std::string GetFunctionCombinedName(const Function* const function);

// Maps entry point functions to opcode in command buffer
// Each entry point is assigned a range of opcodes (1 per instance)
typedef std::map<const Function*, size_t> FunctionOpcodeMap;

// Number of bytes in the command buffer for the opcode
const size_t c_OpcodeByteSize = 4;

FunctionOpcodeMap GetFunctionOpcodeMap(const Program& program);

// Describes how a logic memory is decomposed into physical memories
struct MemoryComposition
{
    // Number of physical memories (including replicas)
    size_t _atomCount;

    // Size of each physical memory
    size_t _atomWidth;
    size_t _atomDepth;

    // "Cost" of each physical memory
    float _atomCost;

    // Number of physical memories in width and depth per replica
    size_t _numAtomsWide;
    size_t _numAtomsDeep;

    // Percent utilization (45 means 45%)
    size_t _utilization;
};

template <typename T>
MemoryComposition GetMemoryComposition(const T& configs, const size_t width, const size_t depth,
                                       const size_t replicaCount)
{
    // Assert that configurations are present deepest to shallowest
    size_t prevDepth = std::numeric_limits<size_t>::max();

    size_t numValidConfigs = 0;

    for (const auto& c : configs)
    {
        if (c.width == 0)
            break;

        assert(c.depth < prevDepth);

        prevDepth = c.depth;

        numValidConfigs++;
    }

    MemoryComposition result = {};
    result._atomCount = std::numeric_limits<size_t>::max();

    // For each memory, brute force all configurations to find the fewest
    for (const auto& c : configs)
    {
        if (c.width == 0)
            break;
        auto numWide = (width + c.width - 1) / c.width;
        auto numDeep = (depth + c.depth - 1) / c.depth;

        const size_t newAtomCount = numWide * numDeep * replicaCount;

        // < is used here under the assumption that the deepest memory
        // is listed first in the config list.  This ensures that deep memory
        // configurations are chosen over narrow ones.  Deeper configurations use less logic
        if (newAtomCount < result._atomCount)
        {
            result._atomCount = newAtomCount;
            result._atomWidth = c.width;
            result._atomDepth = c.depth;
            result._atomCost = c.cost;
            result._numAtomsWide = numWide;
            result._numAtomsDeep = numDeep;

            if (newAtomCount > 0)
            {
                result._utilization = (width * depth * 100) / (c.width * c.depth * numWide * numDeep);
            }
            else
            {
                // For example, 0-width FIFOs.
                result._utilization = 100;
            }
        }
    }

    // std::numeric_limits<size_t>::max() should only be returned
    // if there are no configurations
    assert((0 == numValidConfigs) || (result._atomCount != std::numeric_limits<size_t>::max()));

    return result;
}

template <typename T>
std::pair<size_t, size_t> GetMemoryResourceUsage(const T& configs, const size_t width, const size_t depth,
                                                 const size_t replicaCount)
{
    const MemoryComposition mc = GetMemoryComposition(configs, width, depth, replicaCount);

    return std::pair<size_t, size_t>(mc._atomCount, mc._utilization);
}

struct FifoResourceUsage
{
    size_t _brams;
    size_t _alms;
};

FifoResourceUsage GetFifoResourceUsage(const Program& program, const size_t index);

enum class RAM_TYPE
{
    LUT,
    BLOCK,
    DEEP,
    LOGIC
};

bool ShouldUseRamType(const RegisterDescription& regDesc, const RAM_TYPE ramType);

bool ShouldFifoUseLutRam(const Program& program, const size_t registerIndex);

std::set<size_t> GetAcquiredSemaphores(const BasicBlock& basicBlock);

std::set<size_t> GetReleasedSemaphores(const BasicBlock& basicBlock);

bool DoesFunctionHaveOrderedRestrictions(const ParseTreeFunctionModifier modifiers, const size_t maxThreadCount);

bool BasicBlockNeedsCycleCounter(const BasicBlock& basicBlock);

bool ShouldPromoteUnsignedToSigned(const Operation& op);

class FifoNamer
{
  public:
    FifoNamer(const Program& program);

    std::string GetFifoName(const size_t registerIndex) const;

    size_t GetNormFifoIndex(const size_t registerIndex) const;

    // suffix added to the instance names of cross-region fifos
    // to enable these fifos to be easily found with floor planning scripts.
    static const std::string CrossRegionFifoSuffix;

  private:
    std::map<size_t, std::string> _nameMap;
    std::map<size_t, size_t> _indexMap;
    const Program& _program;
};

// Looks up an element in a map, asserts that the element exists
template <typename ResultType, typename MapType, typename IndexType>
const ResultType& LookupAndAssert(const MapType& map, const IndexType& index)
{
    const auto it = map.find(index);
    assert(it != map.end());

    return it->second;
}

size_t BitsToBytes(const size_t bits);

// Determines if an interator coressponds to the last valid one
template <typename T> bool IsLastIterator(const T& it, const T& end)
{
    T nextIt = it;
    ++nextIt;

    return nextIt == end;
}

template <typename T> std::string join(const T& v, const std::string& delim)
{
    std::ostringstream s;
    for (const auto& i : v)
    {
        const auto& firstElement = *(v.begin());

        if (&i != &firstElement)
        {
            s << delim;
        }
        s << i;
    }
    return s.str();
}

size_t GetInspectableBitWidth(const Program& program, const InspectableVariable& inspectableVariable);

std::string FixupString(const std::string& src);

std::string FixupCommas(const std::string& src);

std::string FixupClassName(const ClassType* const classType);

bool RegisterTracksRaces(const RegisterDescription& regDesc);

struct NestingRecord
{
    size_t _atomicSequence;
    size_t _numThreads;
    AccessedRegister _threadCountReachedRegister;
};

std::string GetExternalClassInstanceBaseName(const ExternalClassInstance& externModule);
std::string GetExternalClassInstanceName(const ExternalClassInstance& externModule);
std::string CombineObjectNameAndExternalClassInstanceName(const std::string& objectName,
                                                          const std::string& externModuleName);

typedef std::vector<Stage*> PipelineStage;
std::vector<PipelineStage> GetPipelineStages(BasicBlock& basicBlock);

struct BypassMaps
{
    // Maps load operation to associated bypass ID
    std::map<const Operation*, size_t> _loadMemoryOpToBypassId;

    // Maps store operation to associated bypass IDs (one per associated load)
    std::map<const Operation*, std::set<size_t>> _storeMemoryOpToBypassIds;

    // Maps stage to list of store operations that should be hoisted to that stage
    std::map<const Stage*, std::list<const Operation*>> _stageToStoreMemory;

    boost::optional<size_t> TryGetBypassObjectId(const Operation* const loadMemoryOp) const
    {
        assert(Opcode::LoadMemory == loadMemoryOp->_opcode);

        const auto it = _loadMemoryOpToBypassId.find(loadMemoryOp);
        if (it != _loadMemoryOpToBypassId.end())
        {
            return boost::optional<size_t>(it->second);
        }
        else
        {
            return boost::optional<size_t>();
        }
    }

    std::set<size_t> GetBypassObjectIds(const Operation* const storeMemoryOp) const
    {
        assert(Opcode::StoreMemory == storeMemoryOp->_opcode);

        std::set<size_t> result;

        const auto it = _storeMemoryOpToBypassIds.find(storeMemoryOp);
        if (it != _storeMemoryOpToBypassIds.end())
        {
            for (const size_t id : it->second)
            {
                result.insert(id);
            }
        }

        return result;
    }
};

// Parameters are (pipeline stage depth, unique id)
using CreateBypassFunction = std::function<void(size_t, size_t)>;

BypassMaps ComputeBypassMaps(const Program& program, const CreateBypassFunction& createBypass);

// Data that is required for debug symbols generation and for the bytecode backend
struct BasicBlockDebugMaps
{
    BasicBlockToLocalsMap _allBlocksLocalsMap;

    BasicBlockToEmptyStagesMap _allBlocksEmptyStagesMap;

    BypassMaps _allBlocksBypassMaps;

    std::vector<std::pair<size_t, size_t>> _bypassObjects;
};

BasicBlockDebugMaps ComputeBasicBlockDebugMaps(const Program& program);

// Generates comma separated lists
class CommaSeparatedOutputHelper
{
  public:
    CommaSeparatedOutputHelper();

    void Append(const std::string& str);
    const std::string Str() const;

  private:
    bool _empty;
    std::ostringstream _str;
};

bool FifoSupportsEncoding(const RegisterDescription& regDesc);
size_t GetLogicalFifoWidth(const RegisterDescription& regDesc);
size_t GetPhysicalFifoWidth(const RegisterDescription& regDesc);

std::string EscapeSpecialChars(const std::string& str);

size_t GetMemoryAtomDepth(const Program& program, const size_t registerIndex);

std::set<size_t> GetMovLatencyWires(const BasicBlock& bb, const Program& program);

class RandomStallRateThresholdGenerator
{
    std::mt19937 _rng;
    std::unique_ptr<std::normal_distribution<float>> _threshold;

  public:
    RandomStallRateThresholdGenerator();
    RandomStallRateThresholdGenerator(size_t stallRate);
    size_t Next();
};

class RandomStallRateMultiThresholdGenerator
{
    RandomStallRateThresholdGenerator generator1, generator2, generator3, generator4;

  public:
    RandomStallRateMultiThresholdGenerator()
        : generator1(RandomStallRateThresholdGenerator(1)), generator2(RandomStallRateThresholdGenerator(2)),
          generator3(RandomStallRateThresholdGenerator(3)), generator4(RandomStallRateThresholdGenerator(4))
    {
    }

    std::vector<size_t> Next()
    {
        return {0, generator1.Next(), generator2.Next(), generator3.Next(), generator4.Next()};
    }
};

// Calculates the desired intermediate width for binary ops
size_t CalculateBinaryOpDesiredSourceOperandWidths(const Program& program, const Operation& op);
size_t CalculateComparisonWidth(const Program& program, const Operation& op);

// Maps (pipeline register index, pipeline stage index) set of pipeline registers used in (pipeline stage index)
// with the same content (produced by a chain of Mov operations)
using RegisterAndStage = std::pair<size_t, size_t>;

using RegisterTreeMap = std::map<RegisterAndStage, std::set<size_t>>;

// Maps bypass group index to information associated with that group
struct BypassStoreGroupRecord
{
    // Index of the memory being read and written
    size_t _memoryIndex;

    // Which pipeline stage contains the load operation
    size_t _loadStage;

    // Which pipeline stage contains the bypass operation
    size_t _bypassStage;

    // Which pipeline stage contains the store operation
    size_t _storeStage;

    // Set of write port associated with the store operation
    size_t _writePort;

    // Maps key to load memory operation
    std::map<size_t, const Operation*> _loadMemoryMap;
};

// Maps bypass group index (relative to basic block)
// to BypassStoreGroupRecord
using BypassStoreMap = std::map<size_t, BypassStoreGroupRecord>;

BypassStoreMap ComputeBypassStoreMap(const BasicBlock& basicBlock);

const ExternalClassInstance& LookupContainingExternalClassInstance(const Program& program,
                                                                   const ExternalModuleCall& call);

bool BasicBlockUsesCycleCounter(const BasicBlock& bb, const OperationEnumerationMode mode);

struct LatencyRecord
{
    // register index that holds the delayed value
    size_t _registerIndex;

    // number of cycles of latency added by Opcode::MovLatencyPlaceholder
    size_t _extraLatency;
};

using LatencyMap = std::map<size_t, LatencyRecord>;

LatencyMap GetExtraLatencyMap(const BasicBlock& basicBlock, const OperationEnumerationMode mode,
                              const Program& program);
bool IsAStartConditionPlaceholder(const Operation& op);
bool IsASemaphorePlaceholder(const Operation& op);
bool IsAllSemaphores(const Stage& stage);

inline bool FunctionStageIsEmpty(const Stage& stage, const BypassMaps& bypassMaps)
{
    const bool functionIsEmpty =
        ((stage._operations.size() == 0) || (IsAllSemaphores(stage)) ||
         ((stage._operations.size() == 1) && (IsAStartConditionPlaceholder(stage._operations.front())))) &&
        (bypassMaps._stageToStoreMemory.find(&stage) == bypassMaps._stageToStoreMemory.end());
    return functionIsEmpty;
}

double ArrayCostFunction(const CodeGenDeviceConfig& deviceConfig, const size_t width, const size_t depth,
                         const size_t numReadPorts, const size_t numWritePorts);

double MemoryCostFunction(const CodeGenDeviceConfig& deviceConfig, const size_t width, const size_t depth,
                          const size_t numReadPorts, const size_t numWritePorts);

// Used in debug symbols
union LocalsHeader
{
    uint64_t       validBitsShort;
    struct
    {
        uint8_t    pad[sizeof(mp_int)-1];
        bool       isWaitConditionMet;
    };
    struct
    {
        uint8_t    data[sizeof(mp_int)];
    };
};

// The meaning of bits in per-basic block control state
enum ControlStateBit : uint8_t
{
        InputFifoEmpty = 0,
        OutputFifoFull = 1,
        SemaphoreCountExceeded = 2,
        WaitForConditionNotMet = 3,
        InputFifoUnderflow = 4,
        OutputFifoOverflow = 5,
        NumberOfStateBits = 6
};


static constexpr size_t c_stallLfsrBits = 11;
static constexpr size_t c_stallLfsrPeriod = (1 << c_stallLfsrBits) - 1;
