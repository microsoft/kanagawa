// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

class ControlFlowGraph;

size_t Log2RoundUp(const mp_int&);

struct AccessedRegister
{
    size_t _registerIndex;

    bool operator!=(const AccessedRegister& rhs) const { return _registerIndex != rhs._registerIndex; }

    bool operator==(const AccessedRegister& rhs) const { return _registerIndex == rhs._registerIndex; }
};

// Number of bits in a string handle
static const size_t c_stringHandleWidth = 64;

// Number of bits in the string handle that specify the allocation location
// These are the upper bits of the string handle
static const size_t c_stringHandleAllocationLocationBits = 32;

// Number of bits in a string handle that specify the string ID relative to the allocation location
// These are the lower bits of the string handle
static const size_t c_stringHandleStringIdBits = c_stringHandleWidth - c_stringHandleAllocationLocationBits;

// Number of bits in signed integer operand that defines how much to change reference counts by
static const size_t c_stringReferenceBits = 64;

// Number of bits in the counter that counts the number of race conditions that occur
static const size_t c_raceCountWidth = 32;

// Number of bits in the state that represent a basic block control decision
static const size_t c_basicBlockControlWidth = 8;

static const size_t c_maxRegisterBitWidth = 1024 * 1024;

extern const size_t c_invalidAccessedRegisterIndex;

static const size_t c_defaultMemoryReadLatency = 2;

// Maps local register index to offset into input FIFO
typedef std::map<size_t, size_t> FifoOffsetMap;

// Maps 1 register index to another
typedef std::map<size_t, size_t> RegisterIndexMap;

using RegisterSet = std::set<size_t>;

struct BasicBlock;
using RegisterSetMap = std::unordered_map<BasicBlock*, RegisterSet>;

// Maps local register index to pipeline register index
typedef RegisterIndexMap LocalToPipelineRegisterMap;

enum class FifoType
{
    Default,
    ContextSaverCaller,
    ReorderBuffer,
    Passthrough,
    PassthroughRegistered,   // writes are registered - no back pressure
    PassthroughUnregistered, // writes are not registered - no back pressure
    FixedDelay,
};

bool FifoTypeCanBackpressure(const FifoType ft);

enum class FifoImplementation
{
    Default = 0,

    // A 2-deep fifo implemented in registers - supports 1 write per cycle
    TwoRegister,

    // Default fifo followed by a two register fifo
    InternalBuffer,

    // FIFO that sits between caller and callee that may be in different floor-planned regions
    CrossRegion
};

struct Literal
{
    mp_int _value;
    size_t _width;

    bool operator!=(const Literal& rhs) const { return std::tie(_value, _width) != std::tie(rhs._value, rhs._width); }

    bool operator<(const Literal& rhs) const { return std::tie(_value, _width) < std::tie(rhs._value, rhs._width); }
};

// Describes a contiguous set of bits in logical fifo data
// that map to bits in physical fifo data
struct FifoCodeInputRange
{
    size_t _width;
    size_t _decodedOffset;
    size_t _encodedOffset;
};

// Describes a contiguous set of bits in logical fifo data.
// The bits could map to bits in the physical fifo data
// Or the bits could map to literal values
struct FifoCodeOutputRange
{
    size_t _width;
    size_t _decodedOffset;
    bool _isLiteral;

    size_t _encodedOffset;
    mp_int _literal;
};

struct FifoCode
{
    // True if the code has been initialized
    bool _initialized = false;

    // Number of bits required to hold the encoded data
    size_t _encodedWidth;

    // Describes how to encode inputs
    std::vector<FifoCodeInputRange> _inputRanges;

    // Describes how to decode outputs
    std::vector<FifoCodeOutputRange> _outputRanges;
};

struct RegisterDescription
{
    ~RegisterDescription();

    RegisterDescription() = default;
    RegisterDescription(const RegisterDescription& mE) = default;
    RegisterDescription& operator=(const RegisterDescription& mE) = default;
    RegisterDescription(RegisterDescription&& mE) noexcept = default;
    RegisterDescription& operator=(RegisterDescription&& mE) noexcept = default;

    // Bit count
    size_t _width;

    RegisterType _type;

    // Human-readable name
    std::string _name;

    // True if the register must be marked as a duplicate that should be preserved
    bool _preserve;

    // Source variable information for this, if any
    SourceVariable _sourceVariable;

    // Used to differentiate registers that stem directly from source variables
    // Registers marked with this make it to the DebugSymbols
    bool _isDirectSource;

    // True if the register name is unique within the Program
    // Set for member and global variables
    bool _uniqueName;

    RegisterSourceType _sourceType;

    // Function that contains this register (if any)
    Function* _function;

    size_t GetMemoryAddressWidth() const;
    size_t GetPow2RoundedElementCount() const;

    Literal GetInitialValueLiteral() const
    {
        const Literal result = {Global()._initialValue, _width};

        return result;
    }

    void SetRegisterSourceType(const RegisterSourceType type)
    {
        _isDirectSource = true;

        _sourceType = type;
    }

    struct GlobalDesc
    {
        GlobalDesc() = default;
        GlobalDesc(const GlobalDesc& mE) = default;
        GlobalDesc& operator=(const GlobalDesc& mE) = default;
        GlobalDesc(GlobalDesc&& mE) noexcept = default;
        GlobalDesc& operator=(GlobalDesc&& mE) noexcept = default;

        bool _hasInitialValue;

        // True if the initial value is the only value this register will have
        bool _isConstant;

        // If _hasInitialValue is true then the register is set to this value on reset
        mp_int _initialValue;

        // Number of write ports on the global variable
        size_t _writeCount;

        // Maps write port index to Literal value
        // for cases where a literal value is assigned to a global
        std::map<size_t, Literal> _literalValues;

        // Path to the CIRCT container which will contain this
        ObjectPath _containerInstancePath;
    };

    struct GlobalViewDesc
    {
        // Index into the table of global view functions
        size_t _globalViewFunctionIndex;
    };

    struct LocalDesc
    {
        // true for values that are expected to sometimes not be live-out
        // at an enqueue, but live-in at the destination
        bool _canSkipFifoWrites;
    };

    struct FifoDesc
    {
        FifoDesc() = default;
        FifoDesc(const FifoDesc& mE) = default;
        FifoDesc& operator=(const FifoDesc& mE) = default;
        FifoDesc(FifoDesc&& mE) noexcept = default;
        FifoDesc& operator=(FifoDesc&& mE) noexcept = default;

        // Clock indices for read and write operations
        size_t _writeClock;

        size_t _readClock;

        // The number slots in the FIFO
        // Can be 0 during most of compilation, in which case it will be sized at the end of compliation
        // If this is not a power of 2, the backend can choose to round the depth up to a power of 2
        size_t _depth;

        // Use DSP to implement FixedDelay FIFO
        bool _useDsp;

        // The FIFO reports that it is almost full when it contains (depth - _almostFullSlots) elements
        // If 0, then the almost full = full
        // Not set until the end of compilation
        size_t _almostFullSlots;

        // Number of extra cycles to delay writes & almost_full signals
        size_t _writeDelay;

        // When auto-pipelining is enabled, this indicates a minimum write delay, and _writeDelay represents
        // the maximum write delay to allow the EDA tool to chose.
        size_t _minWriteDelay;

        // Maximum neceessary depth value (if > 0)
        // If the fifo reaches this depth, then overflow is impossible and
        // almost_full will not be used
        size_t _maxDepth;

        FifoImplementation _implementation;

        FifoType _type;

        size_t LogDepth() const
        {
            assert(_depth != 0);
            return Log2RoundUp(_depth);
        }

        // Returns the FIFO depth to use
        // Returns a value >= _depth, approriate for the target device
        size_t RoundedDepth() const;

        // Only for _type == ContextSaverCaller
        struct
        {
            // True if the ordered version of the context saver should be used
            bool _ordered;

            // Number of bits necessary to represent the loop counter
            size_t _loopCounterWidth;

            // Caller has a transaction size modifier
            bool _hasTransactionSize;
        } _contextSaverCaller;

        // Only for _type == ReorderBuffer
        // The offset + width of the reordering slot id in the fifo
        struct
        {
            size_t _slotOffset;
            size_t _slotWidth;
        } _reorderBuffer;

        // Populated by MapMemoriesAndFifosToRams()
        bool _useLutRam;

        // Can be used to narrow the physical fifo width
        // based on duplicate or constant entries
        FifoCode _code;

        // For fifo size computation.
        // If this is true then the fifo depth
        // is the caller pipeline depth + the depth stored in the fifo register description
        //
        // If this is false, then the fifo depth
        // is max(caller pipeline depth, the depth stored in the fifo register description)
        bool _addMinimumDepth;

        // A named subset of the FIFO bits
        struct NamedRange
        {
            NamedRange(const NamedRange& mE) = default;
            NamedRange& operator=(const NamedRange& mE) = default;
            NamedRange(NamedRange&& mE) noexcept = default;

            size_t _offset;
            size_t _width;
            std::string _name;
        };

        std::vector<NamedRange> _namedRanges;

        void AddNamedRange(const size_t offset, const size_t width, const std::string& name)
        {
            // All registers in the IR should have names
            assert(!name.empty());

            // No 2 ranges should overlap
            const size_t end = offset + width;

            for (const NamedRange& other : _namedRanges)
            {
                const size_t otherEnd = other._offset + other._width;

                assert((otherEnd <= offset) || (other._offset >= end));
            }

            const NamedRange range = {offset, width, name};

            _namedRanges.push_back(range);
        }

        void SetLocation(const Location& loc, const size_t callStackIndex)
        {
            _location = loc;
            _callStackIndex = callStackIndex;
        }

        // max size of one transaction
        size_t _transactionSize;

        // bit index in data that indicates last flit in transaction
        size_t _transactionBitOffset;

        // Source code location & call stack which caused the fifo to be created
        std::optional<Location> _location;

        std::optional<size_t> _callStackIndex;
    };

    struct MemoryDesc
    {
        MemoryDesc() = default;
        MemoryDesc(const MemoryDesc& mE) = default;
        MemoryDesc& operator=(const MemoryDesc& mE) = default;
        MemoryDesc(MemoryDesc&& mE) noexcept = default;
        MemoryDesc& operator=(MemoryDesc&& mE) noexcept = default;

        size_t _elementWidth;
        size_t _elementCount;

        // If > 1, then the _replicate bit determines if
        // each read port gets a replica
        // or if the caller must arbitrate
        size_t _readPortCount;

        // On a given cycle, only 1 write port can be active
        size_t _writePortCount;

        // if true, then each read port gets its own replica
        // if false, then only 1 read port can have access on any given cycle
        bool _replicate;

        // true if the memory is used in simple-quad-port mode
        // no more than 2 read ports, no more than 2 write ports
        bool _quadPort;

        // True if ECC should be enabled
        bool _ecc;

        std::vector<mp_int> _initialValues;

        std::string _sourceDataType;

        // Populated by MapMemoriesAndFifosToRams()
        bool _useLutRam;
        bool _useBlockRam;
        bool _useLogicRam;

        // Path to the CIRCT container which will contain this
        ObjectPath _containerInstancePath;

        // Location where the memory is declared
        Location _location;

        // Returns true if the memory requires arbitration among read sites
        bool HasReadArbitration() const { return !_replicate && !_quadPort; }

        void ClearBypassMask() { _bypassMask.clear(); }

        void SetBypassMask(size_t readPort)
        {
            if (!HasReadArbitration())
            {
                if (readPort >= _bypassMask.size())
                    _bypassMask.resize(readPort + 1, false);
                _bypassMask[readPort] = true;
            }
            else
            {
                // If the memory is not replicated, then enable bypass if any read port needs it
                _bypassMask.assign(1, true);
            }
        }

        bool IsBypassReadPort(size_t port) const
        {
            assert(!HasReadArbitration() || _bypassMask.size() <= 1);
            port = HasReadArbitration() ? 0 : port;
            return port < _bypassMask.size() && _bypassMask[port];
        }

        bool IsBypassWritePort(const size_t port) const
        {
            // For quad port memories, there are 2 ports (both support 1 read
            // and 1 write) Bypass can be configured separately for each of
            // those ports
            // For non-quad-port memories, bypass is considered enabled for a
            // write location if any read location needs bypass
            return _quadPort ? IsBypassReadPort(port) : HasAnyBypassPort();
        }

        bool HasAnyBypassPort() const
        {
            return any_of(_bypassMask.begin(), _bypassMask.end(), [](bool b) { return b; });
        }

        void SetReadLatency(const size_t port, const size_t latency)
        {
            SafeInsertIdempotent(_readLatency, port, latency);
        }

        size_t GetReadLatency(const size_t port) const
        {
            const auto it = _readLatency.find(port);
            if (it != _readLatency.end())
            {
                return it->second;
            }
            else
            {
                // For memories that have no read ports, use the default latency
                return c_defaultMemoryReadLatency;
            }
        }

        size_t GetMinReadLatency() const
        {
            size_t result = c_defaultMemoryReadLatency;

            for (const auto& p : _readLatency)
            {
                const size_t latency = p.second;
                assert(latency <= c_defaultMemoryReadLatency);

                result = std::min(result, latency);
            }

            return result;
        }

        size_t ReplicaCount() const
        {
            // Inspectable memories can have no read ports
            return _replicate ? std::max<size_t>(1, _readPortCount) : 1;
        }

        bool IsROM() const { return _writePortCount == 0 && !_initialValues.empty(); }

      private:
        // Bit i is set if port "i" needs bypass
        std::vector<bool> _bypassMask;

        // Per-read port read latency
        std::map<size_t, size_t> _readLatency;
    };

    void AllocatePointer()
    {
        switch (_type)
        {
        case RegisterType::Global:
        {
            _global.Initialize();
        }
        break;
        case RegisterType::Fifo:
        {
            _fifo.Initialize();
        }
        break;
        case RegisterType::Memory:
        {
            _memory.Initialize();
        }
        break;
        default:
            break;
        }
    }

    // Asserts that optional pointers have been allocated based on the
    // register type.  This asserts both that pointers are allocated when needed
    // and that pointers are null otherwise
    void AssertPointers() const
    {
        assert((_type != RegisterType::Global) ^ _global);
        assert((_type != RegisterType::Fifo) ^ _fifo);
        assert((_type != RegisterType::Memory) ^ _memory);
    }

    GlobalDesc& Global()
    {
        assert(RegisterType::Global == _type);
        return _global;
    }

    const GlobalDesc& Global() const
    {
        assert(RegisterType::Global == _type);
        return _global;
    }

    FifoDesc& Fifo()
    {
        assert(RegisterType::Fifo == _type);
        return _fifo;
    }

    const FifoDesc& Fifo() const
    {
        assert(RegisterType::Fifo == _type);
        return _fifo;
    }

    MemoryDesc& Memory()
    {
        assert(RegisterType::Memory == _type);
        return _memory;
    }

    const MemoryDesc& Memory() const
    {
        assert(RegisterType::Memory == _type);
        return _memory;
    }

    GlobalViewDesc& GlobalView()
    {
        assert(RegisterType::GlobalView == _type);
        return _globalView;
    }

    const GlobalViewDesc& GlobalView() const
    {
        assert(RegisterType::GlobalView == _type);
        return _globalView;
    }

    LocalDesc& Local()
    {
        assert(RegisterType::Local == _type);
        return _local;
    }

    const LocalDesc& Local() const
    {
        assert(RegisterType::Local == _type);
        return _local;
    }

  private:
    // Pointers to rarely-used large structures (to reduce memory consumption)
    deep_ptr<GlobalDesc> _global;
    deep_ptr<FifoDesc> _fifo;
    deep_ptr<MemoryDesc> _memory;

    GlobalViewDesc _globalView;
    LocalDesc _local;
};

// Register descriptions, indexed by register ID
typedef std::vector<RegisterDescription> RegisterTable;

// Operation codes
enum class Opcode
{
    Mov,
    MovCrossFunction, // Mov that will not be optimized away.  Used for function arguments and return values.
    Clear,
    UnaryOp,
    BinaryOp,
    BeginAtomic,
    EndAtomic,
    Assert,
    Enqueue,
    Select,
    WriteGlobal,
    Print,
    Gather,
    StoreMemory,
    LoadMemory,
    BypassMemory,
    AcquireSemaphore,
    ReleaseSemaphore,
    CallAtomic,
    AtomicReturn,
    FanOut,
    Stage,
    StartCondition,
    CycleCounter,
    MovLatencyPlaceholder, // Move that is not optimized away.  Used for long-latency operations
    LineNumber,
    Lut,
    InlineExternalModule,
    ReadSelectedFifo,
    ExternalPlaceholder, // To make external function implementations not be optimized away
    DebugView,
    EnqueueRegisters,
    DequeueRegisters,
    PushPredicate, // Only emitted in debug mode
    PopPredicate,  // Only emitted in debug mode
    StallCheck,
    LocationRecord,
    ConditionalIgnore,
    FormatString,
    FormatEnum,
    ReferenceString,
    AssertStringEqual,
    StringCount,
    NoOp
};

enum class EnqueueType
{
    Default,

    // The destination fifo is a reorder buffer
    ReorderBuffer,

    // Synchronous function calls:

    // Jump from caller to the context saver (allocates invocation ID)
    ContextSaverCaller,

    // Jump from the caller to the callee
    FunctionCall,

    // Jump from callee back to context saver (frees invocation ID)
    ContextSaverCallee
};

struct Enqueue
{
    size_t _successorFifo;

    bool _isPipelinedCall;

    bool _isPredicated;

    bool _isReturn;

    // If true, then execute when the predicate value is true
    // If false, then execute when the predicate value is false
    // Ignored if _predicateIndex == c_invalidPredicateIndex
    bool _predicateExecutionValue;

    EnqueueType _type;

    CallModifiers _modifiers;

    // True if this enqueue operation can appear in an atomic block
    bool _allowedInAtomic;

    // True if this is a call to an asynchronous function
    bool _isAsyncCall;

    // For ReorderBuffer, the local register ID of the slot
    size_t _reorderSlotRegister;

    // for basic blocks that have multiple input fifos (loops) - which fifo to target
    size_t _whichFifo;
};

struct WriteGlobal
{
    bool _isPredicated;
};

struct StartCondition
{
    // If true, then global writes occur before the start condition and create
    // side effects even if start condition is not met. This is used for
    // `atomic do` loops.
    bool _globalsBefore;
};

struct StallCheck
{
    // global writes should be reordered after stallcheck and predicated
    bool _globalsAfter;
};

struct Fifo
{
    size_t _fifoIndex;
};

struct GatherEntry
{
    // Offsets are in bits
    size_t _sourceOffset;
    size_t _destOffset;

    size_t _numBits;

    bool operator==(const GatherEntry& rhs) const
    {
        return std::tie(_sourceOffset, _destOffset, _numBits) ==
               std::tie(rhs._sourceOffset, rhs._destOffset, rhs._numBits);
    }

    bool operator!=(const GatherEntry& rhs) const { return !(*this == rhs); }
};

struct Gather
{
    std::vector<GatherEntry>* _entries;
};

typedef std::function<BasicBlock*()> GetSuccessorBlock;

struct FifoSubset
{
    size_t _registerIndex;
    size_t _offset;
    size_t _width;

    bool operator==(const FifoSubset& rhs) const
    {
        return std::tie(_registerIndex, _offset, _width) == std::tie(rhs._registerIndex, rhs._offset, rhs._width);
    }

    bool operator!=(const FifoSubset& rhs) const { return !(*this == rhs); }

    bool operator<(const FifoSubset& rhs) const
    {
        return std::tie(_registerIndex, _offset, _width) < std::tie(rhs._registerIndex, rhs._offset, rhs._width);
    }
};

enum class SourceOperandType
{
    Literal,
    Register,
    Fifo,
    StringLiteral
};

struct SourceOperand
{
    SourceOperand() {}

    SourceOperand(const size_t val, const size_t width = 64) : _type(SourceOperandType::Literal)
    {
        _literal._value = mp_int(val);
        _literal._width = width;
    }

    SourceOperand(const mp_int& val) : _type(SourceOperandType::Literal)
    {
        _literal._value = val;
        _literal._width = GetMpWidth(val);
    }

    SourceOperand(const Literal& val) : _type(SourceOperandType::Literal), _literal(val) {}

    SourceOperand(const AccessedRegister& reg) : _type(SourceOperandType::Register), _reg(reg) {}

    SourceOperand(const FifoSubset& fifo) : _type(SourceOperandType::Fifo), _fifo(fifo) {}

    SourceOperand(const std::string& str) : _type(SourceOperandType::StringLiteral), _stringLiteral(str) {}

    const AccessedRegister& GetAccessedRegister() const
    {
        assert(_type == SourceOperandType::Register);

        return _reg;
    }

    AccessedRegister& GetAccessedRegister()
    {
        assert(_type == SourceOperandType::Register);

        return _reg;
    }

    FifoSubset& GetFifoSubset()
    {
        assert(_type == SourceOperandType::Fifo);
        return _fifo;
    }

    const FifoSubset& GetFifoSubset() const
    {
        assert(_type == SourceOperandType::Fifo);
        return _fifo;
    }

    Literal& GetLiteral()
    {
        assert(_type == SourceOperandType::Literal);
        return _literal;
    }

    const Literal& GetLiteral() const
    {
        assert(_type == SourceOperandType::Literal);
        return _literal;
    }

    const std::string GetStringLiteral() const
    {
        assert(_type == SourceOperandType::StringLiteral);
        return *_stringLiteral;
    }

    size_t Width(const Program& program) const;

    SourceOperandType Type() const { return _type; }

    bool operator!=(const SourceOperand& rhs) const
    {
        if (_type != rhs._type)
        {
            return true;
        }
        else
        {
            if (_type == SourceOperandType::Literal)
            {
                return _literal != rhs._literal;
            }
            else if (_type == SourceOperandType::Register)
            {
                return _reg != rhs._reg;
            }
            else
            {
                assert(_type == SourceOperandType::Fifo);
                return _fifo != rhs._fifo;
            }
        }
    }

    bool operator==(const SourceOperand& rhs) const { return !(*this != rhs); }

    bool operator<(const SourceOperand& rhs) const
    {
        if (_type < rhs._type)
        {
            return true;
        }
        else if (_type > rhs._type)
        {
            return false;
        }
        else
        {
            switch (_type)
            {
            case SourceOperandType::Register:
                return GetAccessedRegister()._registerIndex < rhs.GetAccessedRegister()._registerIndex;

            case SourceOperandType::Literal:
                return GetLiteral() < rhs.GetLiteral();

            case SourceOperandType::Fifo:
                return GetFifoSubset() < rhs.GetFifoSubset();

            case SourceOperandType::StringLiteral:
                return GetStringLiteral() < rhs.GetStringLiteral();

            default:
                assert(false);
                return false;
            }
        }
    }

  private:
    SourceOperandType _type;

    AccessedRegister _reg;
    Literal _literal;
    FifoSubset _fifo;
    std::optional<std::string> _stringLiteral;
};

enum class DestinationOperandType
{
    Register,
    Fifo
};

struct DestinationOperand
{
    DestinationOperand() {}

    DestinationOperand(const AccessedRegister& reg)
        : _type(DestinationOperandType::Register), _reg(reg), _writeIndex(std::numeric_limits<size_t>::max())
    {
    }

    DestinationOperand(const FifoSubset& fifo)
        : _type(DestinationOperandType::Fifo), _fifo(fifo), _writeIndex(std::numeric_limits<size_t>::max())
    {
    }

    AccessedRegister& GetAccessedRegister()
    {
        assert(_type == DestinationOperandType::Register);
        return _reg;
    }

    const AccessedRegister& GetAccessedRegister() const
    {
        assert(_type == DestinationOperandType::Register);
        return _reg;
    }

    FifoSubset& GetFifoSubset()
    {
        assert(_type == DestinationOperandType::Fifo);
        return _fifo;
    }

    const FifoSubset& GetFifoSubset() const
    {
        assert(_type == DestinationOperandType::Fifo);
        return _fifo;
    }

    size_t Width(const Program& program) const;

    DestinationOperandType Type() const { return _type; }

    size_t GetWriteIndex() const
    {
        assert(_type == DestinationOperandType::Register);

        return _writeIndex;
    }

    void SetWriteIndex(const size_t index)
    {
        assert(_type == DestinationOperandType::Register);

        _writeIndex = index;
    }

  private:
    DestinationOperandType _type;

    union
    {
        AccessedRegister _reg;
        FifoSubset _fifo;
    };

    // For writes to global variables
    // Value that is < RegisterDescription._global._writeCount
    // which uniquely identifies the write operation to a particular global variable
    size_t _writeIndex;
};

struct LoadMemory
{
    size_t _readPort;
    bool _isPredicated;

    // 1 means no output register
    // 2 means output register is enabled
    size_t _readLatency;

    // If _bypass, then _bypassGroupIndex is a unique identifier that can match this operation
    // with all store operations in the same basic block which should be connected
    // via bypass
    bool _bypass;

    size_t _bypassGroupIndex;

    // Uniquely identifies the associated this LoadMemory operation within a bypass group
    size_t _loadMemoryKey;

    // Source offset that each destination operand should start with
    std::vector<size_t>* _sourceOffsets;
};

struct StoreMemory
{
    size_t _writePort;

    bool _isPredicated;

    // Widths of all source operands
    std::vector<size_t>* _sourceWidths;

    // If _bypass, then _bypassGroupIndex is a unique identifier that can match this operation
    // with all load operations in the same basic block which should be connected
    // via bypass
    bool _bypass;

    size_t _bypassGroupIndex;
};

struct BypassMemory
{
    size_t _bypassGroupIndex;

    // Uniquely identifies the associated LoadMemory operation
    size_t _loadMemoryKey;
};

struct Assertion
{
    const char* _message;
    // assertion should use case equality "===" instead of logical equality "==" for Verilog
    bool _caseEQ;
};

struct CallAtomic
{
    // Index of the Function
    size_t _functionIndex;

    // Index that is unique among all atomic calls in the basic block that contains the inline atomic call
    size_t _indexWithinFunction;
};

struct CallInlineExternalModule
{
    size_t _externalModuleIndex;
    size_t _latency;
    bool _isLive;
    bool _isPure;
};

enum class PrintType
{
    Uint,
    Int,
    Float,
    StringLiteral,
    StringHandle
};

struct PrintEntry
{
    size_t _operandIndex;
    PrintType _type;
};

struct Print
{
    std::vector<PrintEntry>* _entries;
};

enum class FormatStringType
{
    StringLiteral,
    StringHandle,
    Integer,
    Boolean,
    Float
};

struct FormatStringEntry
{
    size_t _operandIndex;
    FormatStringType _type;
    size_t _precision;
    int64_t _alignment;
    bool _signed;
    size_t _signBitIndex;
    ParseTreeFormatSpecifier _specifier;
};

struct FormatString
{
    std::vector<FormatStringEntry>* _entries;
};

using FormatEnumEntry = std::pair<std::string, mp_int>;

struct FormatEnum
{
    std::vector<FormatEnumEntry>* _entries;
    std::string* _defaultString;
};

struct AssertStringEqual
{
    const char* _message;
};

struct DebugViewArgument
{
    std::string _name;
    std::string _typeName;
    size_t _beginSrcOperand;
    bool _isEnum;
    std::vector<size_t> _srcOperandWidths;

    size_t Width() const
    {
        size_t result = 0;

        for (const size_t w : _srcOperandWidths)
        {
            result += w;
        }

        return result;
    }
};

struct DebugView
{
    std::string _label;

    std::vector<DebugViewArgument> _arguments;
};

const size_t c_maxLutSources = 6;

struct LutEntry
{
    // How many sources here are
    // 0 is valid - it means that the output is a constant (encoded in _table)
    size_t _numSources;

    // Which source operands are inputs to the lut
    size_t _sourceIndices[c_maxLutSources];

    // Identifies which bit in each source operand applies
    size_t _sourceBit[c_maxLutSources];

    // Lookup table - the low 2^_numSources bits are valid
    uint64_t _table;

    bool IsConstantZero() const;
    bool IsConstantOne() const;
    bool IsInvert() const;
    bool IsPassthrough() const;

    size_t TableSize() const;

    uint64_t TableMask() const;

    bool GetTableEntry(const size_t rowIndex) const;
};

struct Lut
{
    // Number of output bits
    size_t _numDestinationBits;

    // 1 LutEntry per output bit
    LutEntry* _lutEntries;

    // Determine the outputs for a given set of input values
    mp_int Evaluate(const std::function<mp_int(const size_t srcOperandIndex)>& evalSrcOperand) const;
};

enum class AtomicBlockType
{
    Default,

    // For atomic_mem - the stage that does the compute
    AtomicMemCompute,

    // For atomic_mem - the stage that loads from memory
    MemoryLoad,

    // Additional stages of read latency after MemoryLoad and before AtomicMemCompute
    MemoryLoadLatency,

    // For atomic_mem - the stage that writes to memory
    MemoryStore,

    // For inline pipelined calls - the stage that creates threads
    Fork,

    // For inline pipelined calls - the stage that destroys threads
    Join
};

struct AtomicBlockDesc
{
    AtomicBlockType _type;
    bool _containsAtomic;
    size_t _updateRate;
    size_t _chainIndex;
    Location _location;
};

struct QueueRegisters
{
    size_t _fifoIndex;
    std::vector<size_t>* _offsets;
};

// A single operation
struct Operation
{
    Operation() : _signExtendSourceMask(0) { memset(&_flags, 0, sizeof(_flags)); }

    static const size_t c_maxSourceOperands = 8;

    Opcode _opcode;

    union
    {
        ParseTreeUnaryOpType _unaryOpType;
        ParseTreeBinaryOpType _binaryOpType;
        Enqueue _enqueue;
        WriteGlobal _writeGlobal;
        StartCondition _startCondition;
        StallCheck _stallCheck;
        Fifo _fifo;
        Gather _gather;
        LoadMemory _loadMemory;
        StoreMemory _storeMemory;
        BypassMemory _bypassMemory;
        Assertion _assertion;
        size_t _semaphoreIndex;
        CallAtomic _callAtomic;
        Print _print;
        FormatString _formatString;
        FormatEnum _formatEnum;
        AssertStringEqual _assertStringEqual;
        Lut _lut;
        CallInlineExternalModule _callInlineExternalModule;
        AtomicBlockDesc _atomicBlockDesc; // for begin/end atomic
        DebugView* _debugView;
        size_t _locationRecordIndex;
        QueueRegisters _queueRegisters;
    } _flags;

    uint64_t _signExtendSourceMask;

    std::set<FileAndLineNumber> _locations;

    std::vector<SourceOperand> _src;
    std::vector<DestinationOperand> _dst;

    // Only used for Enqueue operations
    GetSuccessorBlock _getSuccessorBlock;
    RegisterIndexMap _renamingTable;        // original index -> most recent renamed index
    RegisterIndexMap _reverseRenamingTable; // most recent renamed index -> original index

    // Mark if this operation is run under some predicate
    // Used later for populating predicate register in debug symbols
    bool _isPredicated = false;

    // True if it is expected that no source location
    // is associated with this operation
    bool _expectNoSourceLocation = false;

    bool ShouldSignExtend(const size_t srcOperand) const
    {
        assert(srcOperand < _src.size());

        return _signExtendSourceMask & (1ull << srcOperand);
    }

    // Push a source operand (and associated sign extension bit)
    // into this operation
    void PushOperand(const SourceOperand& srcOp, bool signExtend)
    {
        _src.push_back(srcOp);

        if (signExtend)
        {
            _signExtendSourceMask |= (1ull << (_src.size() - 1));
        }
    }

    void PushOperand(const Operation& otherOp, const size_t operandIndex)
    {
        assert(operandIndex < otherOp._src.size());
        const SourceOperand& srcOp = otherOp._src[operandIndex];

        PushOperand(srcOp, otherOp.ShouldSignExtend(operandIndex));
    }

    void InsertLocation(const Location& location);
};

// A list of operations
// The boost list implementation is used to
// ensure that list::size() runs in constant time.
using OperationList = boost_list<Operation>;

bool OperationListIsEmpty(const OperationList& operations);

// A single stage of a pipeline
struct Stage
{
    Stage() : _atomicSequence(0) {}

    // 2 consequtive stages with the same _atomicSequence value must run atomically
    size_t _atomicSequence;

    // The set of source code lines this stage stems from
    std::set<FileAndLineNumber> _fileAndLineNumbers;

    // The set of operations to perfom in this pipeline stage
    // In the non-atomic case, there is no ordering - all operations run in parallel.
    // In the atomic case, order must be preserved.
    OperationList _operations;

    // Maps local registers to pipeline registers for reads from this stage
    LocalToPipelineRegisterMap _beforeStageRegisterMap;

    // Maps pipelined gated register/wire to set of pipelined gate register/wires
    // (after this stage)
    std::map<size_t, std::set<size_t>> _clockGates;
};

// A basic block
struct BasicBlock
{
    static const size_t c_maxInputFifoCount = 2;

    BasicBlock(Function* const function, const Location& location, const size_t callStackIndex)
        : _function(function), _indexWithinFunction(std::numeric_limits<size_t>::max()),
          _inputFifoIsLoopGenerator(false), _inputFifoCount(1), _location(location), _callStackIndex(callStackIndex),
          _isOneBBLoop(false), _oneBBLoopOrdered(false), _hasAtomicDoShadowRegisters(false),
          _hasInlinedAtomicFunctions(false), _hasInlinedAtomicDoFunctions(false),
          _transitiveBackpressurePredecessor(nullptr), _transitiveBackpressureSucessor(nullptr), _isStallable(false),
          _bypassGroupCount(0)
    {
        for (size_t& index : _inputFifoIndices)
        {
            index = c_invalidAccessedRegisterIndex;
        }

        for (size_t& d : _inputFifoMinDepth)
        {
            d = 0;
        }

        for (size_t& d : _inputFifoMaxDepth)
        {
            d = 0;
        }
    }

    // Determines if no operations have been added to the basic block
    bool IsEmpty() const;

    // Note there is no IsLastInFunction - because the last basic block in a function can be optimized away
    // when an async call makes a tail call to a non-async function
    bool IsFirstInFunction() const;

    FileAndLineNumber _lastEmittedLineNumber;

    // The total set of operations in the basic block
    // Only used when the basic block is being built
    OperationList _operations;

    // The list of stages in the basic block
    // Once the backend starts, this is guarenteed to be non-empty
    std::list<Stage> _stages;

    // The function that contains this basic block
    Function* _function;

    // Set of registers that should be forced to be live-in at a basic block
    // Used for function parameters, because callers assume that all variables are present in the input FIFO
    std::list<size_t> _liveInReg;

    // If the basic block is a return site, then this is the return value registers
    // This is a subset of _liveInReg
    std::list<size_t> _returnValueReg;

    // Registers that should not be considered live-in
    // This is used for the array returned from a pipelined call
    std::list<size_t> _forcedDeadInReg;

    // Registers in _liveInReg which are not required inside of the basic block
    // These registers are only required for connections between basic blocks
    std::list<size_t> _externalOnlyLiveInReg;

    // For basic blocks that are a return site of an unordered call
    // this is the local register index that holds the invocation ID
    std::optional<size_t> _invocationInstanceRegisterIndex;

    // Uniquely identifies the basic block within the function
    size_t _indexWithinFunction;

    // The FIFOs that feed this basic block
    std::array<size_t, c_maxInputFifoCount> _inputFifoIndices;

    // Minimum depth of the associated input fifo
    //
    // Used to prevent deadlocks in these cases:
    // 1. Loops
    // 2. Synchronous functions with multiple callers
    //
    // Also influenced by [[fifo_depth(N)]] on loops
    std::array<size_t, c_maxInputFifoCount> _inputFifoMinDepth;

    void SetInputFifoMinDepth(size_t fifoIndex, size_t minDepth)
    {
        _inputFifoMinDepth[fifoIndex] = std::max(_inputFifoMinDepth[fifoIndex], minDepth);
    }

    // If non-0 indicates the maximum number of entries a FIFO could ever have
    std::array<size_t, c_maxInputFifoCount> _inputFifoMaxDepth;

    void SetInputFifoMaxDepth(size_t fifoIndex, size_t maxDepth)
    {
        assert(0 == _inputFifoMaxDepth[fifoIndex]);
        _inputFifoMaxDepth[fifoIndex] = maxDepth;
    }

    // The number of valid elements in _inputFifoIndices
    // Either 1 or 2
    size_t _inputFifoCount;

    // Wires or pipeline registers that are used in this basic block
    std::list<size_t> _intraStageRegisters;

    // Maps local register index to bit offset in the input fifo
    FifoOffsetMap _inputFifoRegisterMap;

    // True if the input FIFO is routed through a loop generator
    bool _inputFifoIsLoopGenerator;

    // File/line number where the basic block starts
    Location _location;

    // Can be used to lookup the call stack at the start of the basic block
    size_t _callStackIndex;

    // The list of objects this BasicBlock is associated with.
    // It can be more than one due to function inlining
    std::set<std::string> _objectNames;

    // True if the basic block represents a loop that contains only this basic block
    bool _isOneBBLoop;

    // True if the basic block starts with an atomic do that updates shadow copies
    // of local registers
    bool _hasAtomicDoShadowRegisters;

    // Only valid when _isOneBBLoop is set
    // True if the basic block represents an ordered loop
    bool _oneBBLoopOrdered;

    // Contains inlined functions within an atomic block
    bool _hasInlinedAtomicFunctions;

    // Contains inlined functions within an atomic do block
    bool _hasInlinedAtomicDoFunctions;

    // In some cases, the basic block that writes to a fifo
    // is not the one that checks almost_full, instead the predecessor block does
    // _transitiveBackpressurePredecessor is non-null if almost_full is checked by a predecessor
    // (_transitiveBackpressurePredecessor is the predecessor) _transitiveBackpressureSucessor is non-null if this block
    // should check almost_full for a sucessor (_transitiveBackpressureSucessor is the successor)
    const BasicBlock* _transitiveBackpressurePredecessor;
    const BasicBlock* _transitiveBackpressureSucessor;

    bool HasStartCondition() const { return !OperationListIsEmpty(_startConditionOperations); }

    size_t GetCurrAtomicSequenceNumber() const { return _stages.empty() ? 0 : _stages.back()._atomicSequence; }

    size_t GetNextAtomicSequenceNumber() const { return _stages.empty() ? 0 : (_stages.back()._atomicSequence + 1); }

    AccessedRegister GetStartConditionRegister() const;

    size_t GetThreadRate() const;

    size_t GetThreadRateForFifoSizing() const;

    ObjectPath GetObjectPath() const;

    OperationList _startConditionOperations;

    struct StallableState
    {
        bool _isStallable;
        boost::optional<size_t> _nearestPreceedingNonStallable;
        size_t _stageAdditionalFifoAlmostFullEntries;
    };

    bool IsResetBlock() const;

    std::map<size_t, StallableState> _pipelineStageStallableMap;
    bool _isStallable;

    // Maps destination pipeline register to source pipeline register
    // Only for registers that are present only to carry values through the pipeline
    std::map<size_t, size_t> _pipelineMoveMap;

    // Number of storememory->loadmemory bypass modules needed
    size_t _bypassGroupCount;
};

// Describes the connection between an external class callback
// and the concrete function it calls
struct ExternalClassInstanceCallback
{
    // Name of the callback
    std::string _callbackName;

    // Function type
    const FunctionType* _type;

    // The called function
    const FunctionNode* _calleeNode;

    // The name of the called object
    std::string _calleeName;

    const Function* GetCalledFunction() const;
};

// An external class
struct ExternalClassInstance
{
    // Exported name, will be different than the class name
    // if the [[name]] attribute is used, or if the external class is an instance of a template.
    std::string _name;
    Scope _scope;
    std::string _objectName;
    bool _isExportClass;
    const ClassType* _classType;

    std::list<ExternalClassInstanceCallback> _callbacks;

    std::map<std::string, FunctionDesc> _fixedLatencyMethods;

    // Set of all methods which are callable from the outside
    std::map<std::string, FunctionDesc> _interfaceMethods;

    std::list<TemplateArgument> _templateArguments;
};

// Describes a return from an extern to Kanagawa code
struct ExternReturnDesc
{
    size_t _callSiteIndex;
    size_t _fifoIndex;
};

// A non-inline function
struct Function
{
    // The associated node in the AST
    const FunctionNode* _functionNode;

    // Name of the object that contains this function (or the global class name for flat functions)
    std::string _objectName;

    // Type of the class that contains this function (or the global class type)
    const ClassType* _classType;

    // The namespace(s) that contain the function
    Scope _scope;

    // The name of the function
    std::string _name;

    // Unmangled name of the function
    std::string _unmangledName;

    // All basic blocks
    std::list<BasicBlock> _basicBlocks;

    // The entry-point basic block
    BasicBlock* _start;

    // The exit basic block
    BasicBlock* _end;

    // The unique function name when combing object name, function name and possible index
    std::string _uniqueName;

    // For export, synchronous functions, the index of the return FIFO register
    // c_invalidAccessedRegisterIndex otherwise
    size_t _returnFifoRegisterIndex;

    // The maximum number of threads that can be executing inside the function at one time
    // This includes threads that this function creates via {Begin,End}NestedThreads
    size_t _maxThreadCountInsideFunction;

    // The source-code-specified initiation interval of the first basic block within the function
    size_t _functionThreadRate;

    // Index which identifies the inline atomic function
    size_t _inlineAtomicIndex;

    // Semaphores owned by this function
    std::list<size_t> _semaphores;

    // Number of locations in the program that call this function
    size_t _numCallSites;

    // The external module this extern function is associated with
    ExternalClassInstance* _externClassInstance;

    // True if the function can be executed at runtime
    bool _isReachable;

    // For synchronous external function calls:
    struct
    {
        // the location in the input fifo that contains the call index
        size_t _callIndexStartOffset;
        size_t _callIndexWidth;

        // the set of fifos to return to
        std::vector<ExternReturnDesc> _returnDesc;

        // The sum of maximum thread counts in all calling functions
        // This bounds the number of oustanding calls
        size_t _maxOutstandingCalls;
        size_t GetLogDepth() const
        {
            // The fifo is sized to ensure it will never overflow
            // A minimum depth of 32 is used to avoid errors from Xilinx about a minimum depth of 8
            return std::max<size_t>(Log2RoundUp(_maxOutstandingCalls), 5);
        }

        // Populated by MapMemoriesAndFifosToRams()
        bool _useLutRam;
    } _syncExtern;

    BasicBlock* CreateBasicBlock(const Location& location, const size_t callStackIndex, Program* const program);

    bool IsExtern() const;

    bool IsExport() const;

    bool IsExportClassInterface() const;

    bool IsAsync() const;

    bool IsPipelined() const;

    bool IsUnordered() const;

    bool IsNoBackpressure() const;

    bool IsFixedLatency() const;

    bool HasEndTransactionParameter() const;

    size_t GetEndTransactionRegister() const;

    bool CallOnReset() const;

    // This method serves 2 purposes:
    // 1) avoid name collisions in generated code when there are 2 namespaces with exports that have matching names
    // 2) to avoid ":" characters in the generated code
    std::string GetBackendName() const;

    bool IsExternClassMethod() const;
};

// Describes a structure which combines N input FIFOs into 1 output FIFO
struct FIFOMerger
{
    std::vector<size_t> _sources;

    size_t _dest;

    // Non-null if all sources come from the same function
    Function* _singleSourceFunction;

    // The function that the fifo merger writes to
    Function* _dstFunction;

    // True if fifo merger should not interleave transactions
    bool _isTransactional;

    // Offset to the bit which is set to 1 at the end of each transaction
    size_t _transactionBitOffset;

    // Location of enqueue
    std::vector<const Location*> _locations;
};

// Describes a structure which generates invocations for parallel_call
struct LoopGenerator
{
    // These members are set when the function is generated
    bool _initialized;
    const Function* _function;
    size_t _counterLocalRegisterIndex;
    size_t _counterWidth;
    size_t _threadCountOneRegisterIndex;

    // Offset of count/iteration index in FIFOs
    size_t _counterOffset;

    // Offset to a 1-bit value that is set to 1 if the thread count == 1
    size_t _threadCountOneOffset;

    // Valid iff the thread count is known at compile time
    boost::optional<size_t> _literalMaxThreadId;
};

// A structure which saves live variables while waiting for a synchronous call to complete
struct ContextSaver
{
    // True if the context saver can assume that threads leave the callee in the same order they entered
    bool _isOrdered;

    // True if the callee is an external function
    bool _isExternal;

    // True if the callee is a pipelined function
    bool _isPipelined;

    // True if the call has a transaction size modifier
    bool _hasTransactionSize;

    // Number of bits required to represent the loop count
    // Within the context saver.  Note that the register that the loop count
    // is extracted from can be wider than this
    size_t _loopCounterWidth;

    // The basic block that ends with the enqueue to the called function
    const BasicBlock* _beforeCall;

    // The basic block that the call returns to
    const BasicBlock* _afterCall;

    // Identifier of the call site (for functions with multiple call sites)
    size_t _callSiteIndex;

    // The called function
    const FunctionNode* _callee;

    const FunctionNode::Instance* _calleeInstance;

    // Local register which holds the invocation instance
    // null for ordered calls
    const AllocatedLeafRegister* _invocationInstanceLocalRegister;

    // Local register which holds the loop counter - null for non-pipelined calls
    // This value is the true loop count, not the loop count - 1 passed to the function
    const AllocatedLeafRegister* _loopCountLocalRegister;

    // Local registers which hold the function return value
    // In the common case these point to the same object
    // They only differ for pipelined calls that return an array
    const AllocatedRegister* _calleeReturnValueRegisters;

    const AllocatedRegister* _callerReturnValueRegisters;

    // Filled in after pipelining

    // This is for the return array from pipelined calls
    // Array registers that hold return values
    // Maps register index to offset in the return site fifo
    FifoOffsetMap _pipelinedCallerReturnRegisters;

    // Maps register index to offset into the callee input of the context saver
    FifoOffsetMap _calleeInputMap;

    // Maps register index to offset into the fifo that is input to _afterCall
    // This is for registers that are returned from the callee
    FifoOffsetMap _fromCalleeOutputMap;

    // Maps register index to offset into the caller input of the context saver
    FifoOffsetMap _callerInputMap;

    // Maps register index to offset into the fifo that is input to _afterCall
    // This is for live registers that are saved during the function call
    FifoOffsetMap _fromCallerOutputMap;

    // The input FIFO at the return site
    size_t _destinationFifo;

    // The register (type = FIFO) that the callee writes return values into
    size_t _fromCalleeFifoIndex;

    // the register (type = ContextSaverCaller) that the caller writes live registers into
    // In the back-ends, this is the index used to identify the context saver object
    size_t _fromCallerFifoIndex;

    // Delay to add to writes into context saver fifo
    size_t _writeDelay;

    // Location of the call site
    std::optional<Location> _callSiteLocation;

    // Source code call stack for the function call
    std::optional<size_t> _callStackIndex;

    // Subset of the FIFO that the callee writes to
    // which contains the invocation instance
    FifoSubset InvocationInstanceInCalleeFifo() const;

    // Returns the width of the connection from the context saver to the context saver merge function
    // For pipelined calls that return a value, this returns the width of the array
    size_t GetCalleeOutputWidth(const Program& program) const;
};

struct ExternalModuleCall
{
    // Name of the function being called
    // for fixed-latency methods of export classes
    // this contains the object name as well
    std::string _name;

    // 1 for each source operand - the first is the predicate
    std::vector<std::string> _srcOperandNames;

    // 1 for each source operand - the width of the operand
    // this matches the width of the function parameter
    std::vector<size_t> _srcOperandWidths;

    ExternalModuleCallType _type;

    // Type information about the extern function
    FunctionDesc _functionDesc;

    // Widths of destination results (determined by the function the return type)
    std::vector<size_t> _destinationOperandWidths;

    // Only used with ExternalModuleCallType::ExternClassMethod
    boost::optional<ExternalClassInstanceName> _externClassInstanceName;

    std::string GetFullyQualifiedName() const;
};

// Defines an export function
// Each export function has 1 or more instances
// This distinction matters for export member functions
// There is 1 entry point corresponding to the function
// and 1 instance per object of the containing class
struct EntryPoint
{
    const ClassType* _classType;
    Scope _scope;
    std::string _functionName;
    std::list<Function*> _instances;
    std::string _backendName;

    // Return the function associated with the first instance
    const Function* GetFirstFunction() const
    {
        assert(!_instances.empty());
        return (*_instances.begin());
    }

    const FunctionNode* GetFunctionNode() const { return GetFirstFunction()->_functionNode; }
};

enum class CodeCoverageType
{
    BasicBlock,
    IfStatement,
    SwitchStatement,
    LoopCondition,
    MuxCondition,
    Condition,
    Expression,
    NumCodeCoverageType
};

// Information for code coverage inspectables
struct CodeCoverage
{
    CodeCoverageType _coverageType;
    std::pair<Location, Location> _statements;
    Location _condition;
    std::string _case;
    std::string _functionNameStack;
    const BasicBlock* _basicBlock;
};

enum class InspectableVariableType
{
    Default,
    RaceCount,
    BasicBlockControlState,
    CodeCoverage,
    ExternalClassInstanceInst,
    SymbolHash
};

struct InspectableVariable
{
    // The type of the variable begin inspected
    const Type* _type;

    // The fully-qualified name of the variable
    std::string _name;

    // Description specified in the code
    std::string _description;

    // Index which uniquely identifies this inspectable variable
    size_t _inspectableIndex;

    // A single variable in the source may corespond to multiple registers (structs or arrays for example)
    // Only non-empty for InspectableVariableType::Default
    std::vector<size_t> _registers;

    // To differentiate global from memory
    RegisterType _registerType;

    // Defines where the source data comes from
    InspectableVariableType _inspectionType;

    // Only for BasicBlockControlState
    const BasicBlock* _basicBlock;

    // Only for exported classes
    const ExternalClassInstance* _externClassInstance;

    // Only for CodeCoverage
    CodeCoverage _codeCoverage;

    // Code corresponding to inspected variable has been removed
    bool _removed;
};

struct StackFrame
{
    std::string _unmangledFunctionName;
    size_t _fileIndex;
    size_t _lineNumber;
    size_t _columnNumber;

    StackFrame() {}

    StackFrame(const std::string& functionName, const Location& location)
        : _unmangledFunctionName(functionName), _fileIndex(location._fileIndex), _lineNumber(location._beginLine),
          _columnNumber(location._beginColumn)
    {
    }

    StackFrame(const std::string& functionName, size_t fileIndex, size_t lineNumber, size_t columnNumber)
        : _unmangledFunctionName(functionName), _fileIndex(fileIndex), _lineNumber(lineNumber),
          _columnNumber(columnNumber)
    {
    }
};

inline bool operator==(const StackFrame& lhs, const StackFrame& rhs)
{
    return std::tie(lhs._fileIndex, lhs._unmangledFunctionName, lhs._lineNumber, lhs._columnNumber) ==
           std::tie(rhs._fileIndex, rhs._unmangledFunctionName, rhs._lineNumber, rhs._columnNumber);
}

inline bool operator<(const StackFrame& lhs, const StackFrame& rhs)
{
    return std::tie(lhs._fileIndex, lhs._unmangledFunctionName, lhs._lineNumber, lhs._columnNumber) <
           std::tie(rhs._fileIndex, rhs._unmangledFunctionName, rhs._lineNumber, rhs._columnNumber);
}

using CallStack = std::stack<StackFrame>;

// Map from register index to map from bit index to constant value
using ConstantBitMap = std::map<size_t, std::map<size_t, bool>>;

// Maps register index to set of bits
using RegisterToBitSet = std::map<size_t, std::set<size_t>>;

enum class ExportTypeBehavior
{
    VerilogOnly,
    Default
};

struct Program
{
    Program()
        : _semaphoreCount(0), _atomicFunctionCount(0), _locationRecordCount(0), _isDefaultPass(false),
          _exportClass(nullptr), _numStallers(0), _prePipelineCfgAllowed(false),
          _postPipelineCfgAllowed(false), _stringAllocationLocation(1)
    {
    }

    // Name of the generated module
    std::string _moduleName;

    // True if this is the compilation pass that compiles everything (not a particular export class)
    bool _isDefaultPass;

    // The export class that is compiled (null for default pass)
    const ClassNode* _exportClass;

    // All registers in the program
    RegisterTable _registerTable;

    // Functions which are callable from outside of the program
    std::list<EntryPoint> _entryPoints;

    // The subset of _entryPoints which are callable from the proxy (framework)/exposed at top level module (library)
    // Missing networking recv_* functions and extern module callbacks
    // Only valid in the back ends
    std::list<const EntryPoint*> _callableEntryPoints;

    // Callbacks from extern class instances into Kanagawa
    std::list<const EntryPoint*> _externClassInstanceEntryPoints;

    // Callable entry point functions, sorted by virtual placement position
    std::list<const Function*> _sortedCallableEntryPoints;

    // The set of extern functions that are called
    std::list<Function*> _externFunctions;

    // The set of extern functions that are not called
    std::list<Function*> _unreferencedExternFunctions;

    std::list<FIFOMerger> _fifoMergers;

    std::list<LoopGenerator> _loopGenerators;

    // Non-inline functions
    std::list<Function> _functions;

    std::list<ContextSaver> _contextSavers;

    std::vector<ExternalModuleCall> _externalModuleCalls;

    size_t _numStallers;

    // List of exported types - in the order they should be exported
    std::list<const Type*> _exportedTypes;

    // Set of types which should be exported to software is possible
    std::set<const Type*> _cppExportedTypes;

    std::map<std::string, const Type*> _exportedTypedefs;

    std::list<InspectableVariable> _inspectableVariables;

    std::list<InspectableVariable> _codeCoverageVariables;

    std::list<ExternalClassInstance> _externClassInstances;

    std::map<const BasicBlock*, Placement::Vec2> _basicBlockPositions;

    std::map<size_t, Placement::Vec2> _registerPositions;

    size_t _semaphoreCount;

    size_t _atomicFunctionCount;

    size_t _locationRecordCount;

    std::list<Placement::SortRecord> _sortRecords;

    std::list<size_t> _raceConditionRegisters;

    // pipeline/wire to local source variables map (does not include globals and memories)
    std::map<size_t, size_t> _pipelineToSource;

    // Maps basic block's global code coverage counter
    std::map<BasicBlock*, size_t> codeCoverageCounterMap;

    // Maps basic block's transaction size assertion counter
    std::map<BasicBlock*, size_t> transactionSizeCounterMap;

    // The source objects in the program (classes, structs, arrays)
    std::list<SourceContainer> _sourceContainers;

    // Source variable to register/wire mapping
    std::map<size_t, std::set<size_t>> _sourceToPipelineRegisterMap;

    // Object name, globals
    std::map<std::string, std::set<size_t>> _objectNameToGlobals;
    std::map<std::string, const SourceContainer*> _objectNameToContainers;

    // Maps index to function that defines a global view
    std::map<size_t, OperationList> _globalViewFunctions;

    // Call stacks, identified by index
    std::map<CallStack, size_t> _callStackToIndex;
    std::map<size_t, CallStack> _indexToCallStack;

    // Information about which bits are constant in each register/wire
    ConstantBitMap _constantBits;

    // Maps local register index to set of bits in that register which are not used
    RegisterToBitSet _unusedBits;

    // To ensure correct CFG implementations are used at correct time
    bool _prePipelineCfgAllowed;

    bool _postPipelineCfgAllowed;

    size_t _stringAllocationLocation;

    size_t GetCallStackIndex(const CallStack& callStack);

    size_t GetCallstackIndexForFunction(const Function& function, const Location& location);

    const CallStack& GetCallstackFromIndex(const size_t index) const;

    CallStack GetLimitedCallstackFromIndex(const size_t index) const;

    Placement::Vec2 GetBasicBlockPosition(const BasicBlock* const basicBlock) const
    {
        return SafeLookup(_basicBlockPositions, basicBlock);
    }

    Placement::Vec2 GetRegisterPosition(const size_t index) const
    {
        // If a variable is never referenced
        // give it a default position
        // this can happen if a variable is marked for inspection, but then never read/written
        Placement::Vec2 result(0.0f, 0.0f);

        const auto it = _registerPositions.find(index);

        if (it != _registerPositions.end())
        {
            result = it->second;
        }

        return result;
    }

    const Function& GetInlineAtomicFunction(const size_t atomicIndex) const
    {
        for (auto it = _functions.begin(); it != _functions.end(); ++it)
        {
            const Function& function = *it;
        }

        throw std::runtime_error("Failed to find inline atomic function");
    }

    bool HasExternalFunction(const std::string& name) const
    {
        for (const Function* const function : _externFunctions)
        {
            if (function->_name == name)
            {
                return true;
            }
        }

        return false;
    }

    bool HasExternalAsyncFunction(const std::string& name) const
    {
        for (const Function* const function : _externFunctions)
        {
            const FunctionNode* const functionNode = function->_functionNode;

            const bool isAsync = functionNode->GetModifiers() & ParseTreeFunctionModifierAsync;

            if (isAsync && (function->_name == name))
            {
                return true;
            }
        }

        return false;
    }

    const Function* GetEntryPoint(const std::string& name) const;

    const Function* GetExternFunction(const std::string& name) const
    {
        const Function* result = nullptr;

        for (const Function* function : _externFunctions)
        {
            if (function->_name == name)
            {
                result = function;
                break;
            }
        }

        return result;
    }

    void AddExportedType(const Type* const type, const ExportTypeBehavior behavior)
    {
        if (const auto classType = dynamic_cast<const ClassType*>(type))
        {
            // exported classes are handled separately
        }
        else
        {
            // Check to see if the type is already on the list of types to export
            const auto it = std::find(_exportedTypes.begin(), _exportedTypes.end(), type);

            if (it == _exportedTypes.end())
            {
                _exportedTypes.push_back(type);
            }
        }

        if (ExportTypeBehavior::Default == behavior)
        {
            _cppExportedTypes.insert(type);
        }
    }

    void AddExportedTypedef(const std::string& name, const Type* const type)
    {
        _exportedTypedefs[name] = type;

        _cppExportedTypes.insert(type);
    }

    std::string GetRegisterName(const AccessedRegister& r) const { return _registerTable[r._registerIndex]._name; }
};

// Map from variable name to register information
struct GenerateIRGlobalData
{
};

struct GenerateIRVariableData
{
    std::string _name;

    const AllocatedRegister* _allocatedRegisters;

    const Type* _type;

    KnownValue _value;

    DeclareNode::DeclarationScope _declarationScope;

    bool _isConst;

    const ParseTreeNode* _constantInitialValueNode;
};

typedef TypeContext<GenerateIRGlobalData, GenerateIRVariableData> GenerateIRTypeContext;

// One portion of a non-masking condition. Portions are ANDed together for the full non-masking condition
struct NonMaskingConditionSubEntry
{
    // String representing this non-masking condition portion
    std::string _str;
    // Register index for this non-masking condition portion
    size_t _registerIndex;
    // Whether this non-masking condition portion should be inverted
    bool _invert;
};

// Non-masking condition information for one instance of a leaf condition
struct NonMaskingConditionEntry
{
    // Vector of sub-entries that are ANDed together to produce the full non-masking condition
    std::vector<NonMaskingConditionSubEntry> _entries;
    // Indicates that this leaf condition instance is in inverting mode with repsect to the top-level condition
    bool _inverting;
};

// Condition coverage information for one leaf condition
struct ConditionCoverageEntry
{
    // Register for leaf condition value
    size_t _registerIndex;
    // Marks whether this condition ever appears within an XOR
    bool _withinXor;
    // Vector of non-masking conditions, one for each instance of the leaf
    // condition in the top-level condition. Each of these non-masking conditions,
    // is ORed together for the full non-masking condition.
    std::vector<NonMaskingConditionEntry> _nonMaskingConditions;
};

// Save information from leaf condition node
struct ConditionCoverageLeafConditionEntry
{
    // Register index for leaf condition value
    size_t _registerIndex;
    // String representing leaf condition
    std::string _str;
    // Whether leaf condition is in inverting or non-inverting mode with respect to the top-level condition
    bool _inverting;
    // Whether this leaf condition is a call node
    bool _callNode;
};

struct ConditionCoverageFlags
{
    // Condition coverage tracking is turned on
    bool _enabled;
    // Track if visit is within an XOR node
    bool _withinXor;
    // Track whether a deeper binary op exists
    bool _deeperBinaryOp;
    // Track if node is in inverting or non-inverting mode with respect to original condition
    bool _inverting;
    // Tracking to find leaf condition
    bool _leafConditionTracking;
};

// Struct for tracking information for generating condition coverage
struct ConditionCoverageTracker
{
    ConditionCoverageFlags _conditionCoverageFlags;

    // Condition or Expression coverage
    CodeCoverageType _codeCoverageType;

    // Top-level condition that is being broken down
    boost::optional<AccessedRegister> _topLevelCondition;

    // Location of top-level condition
    Location _topLevelConditionLocation;

    // PrettyPrint string of top-level condition
    std::string _topLevelConditionString;

    // Maps from leaf condition string to its condition coverage information
    std::map<std::string, ConditionCoverageEntry> _conditionCoverageMap;

    // Track portions of non-masking conditions during Visit
    std::vector<NonMaskingConditionSubEntry> _nonMaskingConditionStack;

    // Save leaf condition information during Visit
    boost::optional<ConditionCoverageLeafConditionEntry> _conditionCoverageLeafCondition;

    // Counters used to index multiple calls to a function in one condition
    std::map<std::string, size_t> _conditionCoverageCallCounterMap;
};

class RegisterAccessNotification
{
  public:
    virtual void NotifyWrite(const size_t registerIndex, const size_t conditionStackDepth) = 0;
};

struct CallSiteInstance
{
    CallSiteInstance(const FunctionInstance& caller, const Location& callerLocation, const FunctionInstance& callee)
        : _caller(caller), _callerLocation(callerLocation), _callee(callee)
    {
    }

    FunctionInstance _caller;
    Location _callerLocation;
    FunctionInstance _callee;
};

struct IRContext : public KnownValueContext
{
    IRContext()
        : _program(nullptr), _writtenRegisters(nullptr), _returnValueRegisters(nullptr),
          _callSiteIndexRegister(nullptr), _invocationInstanceRegister(nullptr), _function(nullptr),
          _basicBlock(nullptr), _inlineCallCount(0), _enclosingClassType(nullptr), _atomicChainIndex(0),
          _isInAtomicDo(false), _isInSimAssert(false), _atomicStackDepth(0), _reorderStackDepth(0),
          _codeCoverageCounter(0), _containsAtomic(false), _expectEmptyFunctionInstanceStack(false)
    {
    }

    BasicBlock* CreateBasicBlock(const Location& location);

    void PushPredicate(const size_t registerIndex, const Location& location);
    void PushInversePredicate(const size_t registerIndex, const Location& location);
    void PopPredicate();

    bool IsPredicated() const;
    size_t GetPredicate() const;
    void Barrier(const Location& location);

    void PredicatedIgnore(const AccessedRegister reg, const Location& location);

    void SetSymbolKnownValue(const Scope& scope, const std::string& name, const KnownValue& knownValue,
                             const Location& location) override;

    KnownValue LookupKnownValueForSymbol(const Scope& scope, const std::string& name,
                                         const Location& location) override;

    size_t AllocateAtomicChainIndex();

    std::string GetObjectName();

    size_t GetCallStackIndex();

    const std::string GetRegisterName(const AccessedRegister ar) const;

    ObjectPath GetBasicBlockContainerPath();

    std::string GenerateUniqueNameWithinPath(const ObjectPath& path, const std::string& baseName);

    bool FunctionHasOrderingRestrictions();

    size_t GetConditionStackDepth() const;

    Program* _program;
    GenerateIRTypeContext _typeContext;
    const AllocatedRegister* _writtenRegisters;
    const AllocatedRegister* _returnValueRegisters;
    const AllocatedLeafRegister* _callSiteIndexRegister;
    const AllocatedLeafRegister* _invocationInstanceRegister;
    size_t _inlineCallCount;

    // Prefix for names of compile-generated files
    std::string _baseFileName;

    std::stack<std::string> _objectNameStack;

    std::stack<ExternalClassInstance*> _externClassInstances;

    // List of objects that should be notified when a local variable is updated
    std::list<RegisterAccessNotification*> _registerAccessNotificationStack;

    // type for the class that IR is being generated for (or GetGlobalClassType)
    const ClassType* _enclosingClassType;

    // The current (non-inline) function being generated
    Function* _function;
    BasicBlock* _basicBlock;

    // The instance for the current function being generated
    // Note that when IR is being generated for an inline function
    // the instance of the inline function is on this stack
    std::list<FunctionInstance> _functionInstanceStack;

    size_t _atomicChainIndex;

    bool _isInAtomicDo;

    bool _isInSimAssert;

    size_t _atomicStackDepth;

    size_t _reorderStackDepth;

    // Set in unit tests when it is normal for _functionInstanceStack to be empty
    bool _expectEmptyFunctionInstanceStack;

    bool IsInAtomicBlock() const { return _atomicStackDepth > 0; }

    // Atomic block descriptions during IR generation
    std::stack<AtomicBlockDesc> _atomicBlockDescStack;

    // Flag used to determine if an AtomicNode contains a nested AtomicNode
    bool _containsAtomic;

    // Iterator to BeginAtomic op during AtomicNode IR generation
    OperationList::iterator _beginAtomicIt;

    // Generate name for code coverage inspectable. The same name should be used for
    // cases that are aggregated together.
    std::string GenerateCodeCoverageName(const Location& location, const CodeCoverageType codeCoverageType);

    void NewConditionCoverageTracker(CodeCoverageType codeCoverageType);
    bool HasConditionCoverageTracker() const;
    ConditionCoverageTracker& GetConditionCoverageTracker();
    void PopConditionCoverageTracker();

  private:
    struct PredicateStackEntry
    {
        // Register that holds 1 predicate value
        AccessedRegister _leafRegister;

        // Register that holds the and reduction of the entire stack
        AccessedRegister _combinedRegister;
    };

    std::list<PredicateStackEntry> _predicateStack;

    // Global counter to uniquely identify code coverage instances
    size_t _codeCoverageCounter;

    // Stack of structs used to track information for generating condition
    // coverage. A stack is used because there may be nested instances of nodes
    // for which we need to generate condition coverage.
    std::stack<ConditionCoverageTracker> _conditionCoverageTrackerStack;

    std::map<ObjectPath, std::map<std::string, size_t>> _uniqueNameMap;
};

// Sets Operation::_location
// for all operations added to an operation list between
// constructor/destructor
class SetOperationLocation
{
  public:
    SetOperationLocation(IRContext& context, const Location& location);
    SetOperationLocation(OperationList& ops, const Location& location);
    SetOperationLocation(OperationList& ops, const std::set<FileAndLineNumber>& locations);
    SetOperationLocation(OperationList& ops, const Location& location, const size_t callStackIndex);
    SetOperationLocation(OperationList& ops, const Operation& srcOp);
    ~SetOperationLocation();

  private:
    IRContext* _context;
    BasicBlock* _startBlock;
    OperationList& _ops;
    size_t _beginSize;
    std::set<FileAndLineNumber> _locations;
};

// RAII class for IRContext::_writtenRegisters
class PushPopWrittenRegister
{
  public:
    PushPopWrittenRegister(IRContext& context) : _context(context), _saved(context._writtenRegisters) {}

    ~PushPopWrittenRegister() { _context._writtenRegisters = _saved; }

  private:
    PushPopWrittenRegister* operator=(const PushPopWrittenRegister&);

    IRContext& _context;
    const AllocatedRegister* _saved;
};

// RAII class for IRContext::_atomicStackDepth
class PushPopAtomicStackDepth
{
  public:
    PushPopAtomicStackDepth(IRContext& context) : _context(context) { context._atomicStackDepth++; }

    ~PushPopAtomicStackDepth() { _context._atomicStackDepth--; }

  private:
    IRContext& _context;
};

// RAII class for IRContext::_reorderStackDepth
class PushPopReorderStackDepth
{
  public:
    PushPopReorderStackDepth(IRContext& context) : _context(context) { context._reorderStackDepth++; }

    ~PushPopReorderStackDepth() { _context._reorderStackDepth--; }

  private:
    IRContext& _context;
};

// RAII class that inserts BeginAtomic and EndAtomic operations
class AtomicBlock
{
  public:
    AtomicBlock(IRContext& context, const AtomicBlockType type, const size_t updateRate, const size_t atomicChainIndex,
                const Location location)
        : _context(context), _sol(context, location), _pushPopStackDepth(context)
    {
        memset(&_desc, 0, sizeof(_desc));
        _desc._type = type;
        _desc._updateRate = updateRate;
        _desc._chainIndex = atomicChainIndex;
        _desc._location = location;
        _desc._containsAtomic = false; // This will be updated later

        // Reset flag
        context._containsAtomic = false;

        Operation op = {};

        op._opcode = Opcode::BeginAtomic;
        op._flags._atomicBlockDesc = _desc;

        _context._basicBlock->_operations.push_back(op);

        // Save iterator to BeginAtomic operation
        _beginAtomicIt = _context._basicBlock->_operations.end();
        _beginAtomicIt--;
        assert(_beginAtomicIt->_opcode == Opcode::BeginAtomic);

        // Save description to context
        _context._atomicBlockDescStack.push(_desc);
    }

    OperationList::iterator GetBeginAtomicIt() const { return _beginAtomicIt; }

    ~AtomicBlock()
    {
        // Check flag and update description
        if (_context._containsAtomic)
        {
            _desc._containsAtomic = true;

            _beginAtomicIt->_flags._atomicBlockDesc = _desc;
        }

        Operation op = {};

        op._opcode = Opcode::EndAtomic;
        op._flags._atomicBlockDesc = _desc;

        _context._basicBlock->_operations.push_back(op);

        // Pop description from context
        assert(!_context._atomicBlockDescStack.empty());
        _context._atomicBlockDescStack.pop();

        // Set flag for upstream nodes
        _context._containsAtomic = true;
    }

  private:
    SetOperationLocation _sol;
    IRContext& _context;
    AtomicBlockDesc _desc;
    PushPopAtomicStackDepth _pushPopStackDepth;
    OperationList::iterator _beginAtomicIt;
};

static const size_t c_defaultAtomicBlockUpdateRate = 1;

// RAII class that inserts BeginAtomic and EndAtomic operations
// if the new atomic block is more restrictive than the atomic block already on the stack
class OptionalAtomicBlock
{
  public:
    OptionalAtomicBlock(IRContext& context, const size_t updateRate, const Location location)
    {
        bool generateAtomic = true;

        if (!context._atomicBlockDescStack.empty())
        {
            const size_t outerUpdateRate = context._atomicBlockDescStack.top()._updateRate;

            // Nested atomic that is less restrictive can be ignored
            if (updateRate >= outerUpdateRate)
            {
                generateAtomic = false;
            }
        }

        if (generateAtomic)
        {
            _atomicBlock = std::make_unique<AtomicBlock>(context, AtomicBlockType::Default, updateRate,
                                                         context.AllocateAtomicChainIndex(), location);
        }
    }

  private:
    std::unique_ptr<AtomicBlock> _atomicBlock;
};

// RAII class for IRContext::_objectNameStack
class PushPopObjectNameStack
{
  public:
    PushPopObjectNameStack(IRContext& context, const std::string& objectName) : _context(context)
    {
        _context._objectNameStack.push(objectName);
    }

    ~PushPopObjectNameStack() { _context._objectNameStack.pop(); }

  private:
    PushPopObjectNameStack& operator=(const PushPopObjectNameStack&);

    IRContext& _context;
};

// RAII class for IRContext::_enclosingClassType
class PushPopClassType
{
  public:
    PushPopClassType(IRContext& context, const ClassType* const classType)
        : _context(context), _savedClassType(context._enclosingClassType)
    {
        assert(classType);

        context._enclosingClassType = classType;
    }

    ~PushPopClassType() { _context._enclosingClassType = _savedClassType; }

  private:
    PushPopClassType& operator=(const PushPopClassType&);

    IRContext& _context;

    const ClassType* const _savedClassType;
};

// RAII class for IRContext::_inlineCallCount
class PushPopInlineCallCount
{
  public:
    PushPopInlineCallCount(IRContext& context, const size_t count)
        : _context(context), _savedCount(context._inlineCallCount)
    {
        context._inlineCallCount += count;
    }

    ~PushPopInlineCallCount() { _context._inlineCallCount = _savedCount; }

  private:
    PushPopInlineCallCount& operator=(const PushPopInlineCallCount&);

    IRContext& _context;

    const size_t _savedCount;
};

// RAII class for IRContext::_externClassInstances
class PushPopExternalClassInstanceStack
{
  public:
    PushPopExternalClassInstanceStack(IRContext& context, ExternalClassInstance* const externModule) : _context(context)
    {
        _context._externClassInstances.push(externModule);
    }

    ~PushPopExternalClassInstanceStack() { _context._externClassInstances.pop(); }

  private:
    PushPopExternalClassInstanceStack& operator=(const PushPopExternalClassInstanceStack&);

    IRContext& _context;
};

// RAII class for IRContext::_isInAtomicDo
class PushPopIsInAtomicDo
{
  public:
    PushPopIsInAtomicDo(IRContext& context) : _context(context)
    {
        assert(!context._isInAtomicDo);

        _context._isInAtomicDo = true;
    }

    ~PushPopIsInAtomicDo() { _context._isInAtomicDo = false; }

  private:
    PushPopIsInAtomicDo& operator=(const PushPopIsInAtomicDo&);

    IRContext& _context;
};

// RAII class for IRContext::_isInSimAssert
class PushPopIsInSimAssert
{
  public:
    PushPopIsInSimAssert(IRContext& context) : _context(context)
    {
        _prevIsInSimAssert = _context._isInSimAssert;
        _context._isInSimAssert = true;
    }

    ~PushPopIsInSimAssert() { _context._isInSimAssert = _prevIsInSimAssert; }

  private:
    PushPopIsInSimAssert& operator=(const PushPopIsInSimAssert&);

    IRContext& _context;
    bool _prevIsInSimAssert;
};

// RAII class for IRContext::_functionInstanceStack
class PushPopFunctionInstance
{
  public:
    PushPopFunctionInstance(IRContext& context, const FunctionInstance& instance) : _context(context)
    {
        _context._functionInstanceStack.push_back(instance);
    }

    ~PushPopFunctionInstance() { _context._functionInstanceStack.pop_back(); }

  private:
    PushPopFunctionInstance& operator=(const PushPopFunctionInstance&);

    IRContext& _context;
};

// RAII class for IRContext::_nonMaskingConditionStack
class PushPopNonMaskingCondition
{
  public:
    PushPopNonMaskingCondition(IRContext& context, const NonMaskingConditionSubEntry& entry, bool valid)
        : _context(context), _valid(valid)
    {
        if (_valid && _context.HasConditionCoverageTracker())
        {
            ConditionCoverageTracker& tracker = _context.GetConditionCoverageTracker();
            tracker._nonMaskingConditionStack.push_back(entry);
        }
    }

    ~PushPopNonMaskingCondition()
    {
        if (_valid && _context.HasConditionCoverageTracker())
        {
            ConditionCoverageTracker& tracker = _context.GetConditionCoverageTracker();
            tracker._nonMaskingConditionStack.pop_back();
        }
    }

  private:
    PushPopNonMaskingCondition& operator=(const PushPopNonMaskingCondition&);

    IRContext& _context;
    bool _valid;
};

// RAII class for temporarily disabling condition coverage tracking
class DisableConditionCoverage
{
  public:
    DisableConditionCoverage(IRContext& context, bool valid) : _context(context), _valid(valid)
    {
        if (_valid && _context.HasConditionCoverageTracker())
        {
            ConditionCoverageTracker& tracker = _context.GetConditionCoverageTracker();
            _previousValue = tracker._conditionCoverageFlags._enabled;
            tracker._conditionCoverageFlags._enabled = false;
        }
    }

    ~DisableConditionCoverage()
    {
        if (_valid && _context.HasConditionCoverageTracker())
        {
            ConditionCoverageTracker& tracker = _context.GetConditionCoverageTracker();
            tracker._conditionCoverageFlags._enabled = _previousValue;
        }
    }

  private:
    DisableConditionCoverage& operator=(const DisableConditionCoverage&);

    IRContext& _context;
    bool _previousValue;
    bool _valid;
};

// RAII class for tracking inverting/non-inverting mode for condition coverage
class ConditionCoverageInverting
{
  public:
    ConditionCoverageInverting(IRContext& context, bool valid) : _context(context), _valid(valid)
    {
        if (_valid && _context.HasConditionCoverageTracker())
        {
            ConditionCoverageTracker& tracker = _context.GetConditionCoverageTracker();
            tracker._conditionCoverageFlags._inverting = !tracker._conditionCoverageFlags._inverting;
        }
    }

    ~ConditionCoverageInverting()
    {
        if (_valid && _context.HasConditionCoverageTracker())
        {
            ConditionCoverageTracker& tracker = _context.GetConditionCoverageTracker();
            tracker._conditionCoverageFlags._inverting = !tracker._conditionCoverageFlags._inverting;
        }
    }

  private:
    ConditionCoverageInverting& operator=(const ConditionCoverageInverting&);

    IRContext& _context;
    bool _valid;
};

// RAII class for tracking whether we are inside an XOR node for condition coverage
class ConditionCoverageWithinXor
{
  public:
    ConditionCoverageWithinXor(IRContext& context, bool isXor) : _context(context)
    {
        if (_context.HasConditionCoverageTracker())
        {
            ConditionCoverageTracker& tracker = _context.GetConditionCoverageTracker();
            _previousValue = tracker._conditionCoverageFlags._withinXor;
            if (isXor)
            {
                tracker._conditionCoverageFlags._withinXor = true;
            }
        }
    }

    ~ConditionCoverageWithinXor()
    {
        if (_context.HasConditionCoverageTracker())
        {
            ConditionCoverageTracker& tracker = _context.GetConditionCoverageTracker();
            tracker._conditionCoverageFlags._withinXor = _previousValue;
        }
    }

  private:
    ConditionCoverageWithinXor& operator=(const ConditionCoverageWithinXor&);

    IRContext& _context;
    bool _previousValue;
};

BasicBlock* FinalizeBasicBlock(IRContext& context);

void OptimizeScheduleAndPipeline(IRContext& context);

void CheckExports(IRContext& context);

void CheckAndRewriteAtomics(IRContext& context);

void CheckRecursion(const Program& program);

void RemoveFrontEndConceptsBeforeOptimize(IRContext& context);

size_t GetCallSiteIndexWidth(const size_t callerCount);

void CheckExterns(IRContext& context);

mp_int Truncate(const mp_int& src, const size_t numBits);

mp_int Widen(const Literal& srcIn, const bool signExtend, const size_t dstWidth);

mp_int Resize(const Literal& srcIn, const bool signExtend, const size_t dstWidth);

mp_int ImplementUnaryOp(const Literal& srcIn, const bool signExtendSrc, const ParseTreeUnaryOpType type,
                        const size_t dstWidth);

mp_int ImplementBinaryOp(const Literal& src0In, const Literal& src1In, const size_t dstWidth,
                         const bool signExtendSource0, const bool signExtendSource1, const ParseTreeBinaryOpType type);

bool IsNegative(const mp_int& value, const Type* const type);

size_t Log2(const mp_int& input);

bool IsPow2(const mp_int& input);

mp_int RoundUpToPow2(const mp_int& input);

uint32_t FloatAsUint(const float input);

class AllocatedRegister
{
  public:
    typedef std::function<void(const AccessedRegister reg, const Type* const type)> UnaryVisitFunction;

    typedef std::function<void(const AccessedRegister lhs, const Type* const lhsType, const AccessedRegister rhs,
                               const Type* const rhsType)>
        BinaryVisitFunction;

    AllocatedRegister(const RegisterType registerType, const Type* const type);

    RegisterType GetRegisterType() const { return _registerType; }

    const Type* GetType() const { return _type; }

    size_t GetConditionStackDepth() const;

    virtual void SetConditionStackDepth(const size_t depth);

    virtual ~AllocatedRegister() {}

    virtual void VisitRegisters(const UnaryVisitFunction& visitor) const = 0;

    virtual void VisitRegisters(const AllocatedRegister* const rhs, const BinaryVisitFunction& visitor) const = 0;

    virtual void Initialize(IRContext& context, const ParseTreeNode* const initialValueNode) const = 0;

    virtual void Interpolate(IRContext& context, Operation& op, const size_t alignment, const size_t precision,
                             const ParseTreeFormatSpecifier format) const;

  private:
    RegisterType _registerType;
    const Type* _type;
    std::optional<size_t> _conditionStackDepth;
};

size_t AllocateRegister(Program* const program, const RegisterDescription& regDesc);

size_t AllocateRegister(Program* const program, const size_t bitWidth, const RegisterType type, const std::string& name,
                        const ObjectPath& containerPath = {}, const SourceVariable& source = {});

size_t AllocateDuplicateRegister(Program* const program, const size_t otherRegisterIndex);

enum class OperationEnumerationMode
{
    Unscheduled,
    Scheduled
};

typedef std::function<void(Operation& op)> OperationCallback;
void ForEachOperationForward(BasicBlock& basicBlock, const OperationCallback& callback,
                             const OperationEnumerationMode mode = OperationEnumerationMode::Unscheduled);
void ForEachOperationReverse(BasicBlock& basicBlock, const OperationCallback& callback,
                             const OperationEnumerationMode mode = OperationEnumerationMode::Unscheduled);

struct ForEachOperation_AtomicData
{
    bool _inAtomic;
    AtomicBlockDesc _currAtomicBlockDesc;
    OperationList::iterator _currAtomicChainStart;
};

using ForEachOperation_AtomicCallback =
    std::function<void(Operation&, OperationList::iterator, const ForEachOperation_AtomicData&)>;

void ForEachOperation_Atomic(OperationList& operations, ForEachOperation_AtomicCallback callback);

typedef std::function<bool(const Operation& op)> OperationRemoveCallback;
void RemoveOperations(BasicBlock& basicBlock, const OperationRemoveCallback& callback,
                      const OperationEnumerationMode mode = OperationEnumerationMode::Unscheduled);

void PropagateLineNumbers(BasicBlock* basicBlock);

bool IsLiveOperation(const Operation& op);
bool OpcodeAllowedInAtomicBlock(const Opcode opcode);
bool AllowMergeSubsequentAtomicDo(const Opcode opcode);
bool ShouldPreserveInputRegisters(const Opcode opcode);
bool OperationHasRegisteredInput(const Operation& op);
bool OperationHasRegisteredOutput(const Operation& op);
bool OpcodeHasExternalInputRegisters(const Opcode opcode);
bool DoesOpcodeSupportCommonSubexpressionElimination(const Opcode opcode);
bool CanOpcodeBeLoweredToLut(const Opcode opcode);
bool DoesOpcodeImplyOptimizationBarrier(const Opcode opcode);
bool DoesOpcodeRequireTypeBarrier(const Opcode opcode);
bool DoesOpcodeRequireAssertBarrier(const Opcode opcode);
bool CanOperationBackpressure(const Operation& op);
bool OpcodeCanUseBitBucketOutput(const Opcode opcode);
bool OperationReadsFromStringTable(const Operation& op);
bool OpcodeWritesToStringTable(const Opcode opcode);
const char* GetOpcodeString(const Program& program, const Operation& op);
size_t GetOpPathLength(const Program& program, const Operation& op, const size_t carryChainWidthPerLogicLevel);
size_t GetOpPathLength(const Program& program, const Operation& op);
size_t GetInputRoutingDelay(const Operation& op);
size_t GetOutputRoutingDelay(const Opcode opcode);
bool OpcodeUsesHardenedRegisters(const Opcode opcode);

bool IsRegisterTypeAllowedInKillMoves(const RegisterType registerType);
const char* GetRegisterTypeName(const RegisterType registerType);
bool CanRegisterTypeBeRenamed(const RegisterType registerType);
bool IsLocalRegisterType(const RegisterType registerType);
bool RegisterTypeRequiresPipelineStage(const RegisterType registerType);
bool RegisterTypeDisallowsReordering(const RegisterType registerType);

std::ostream& operator<<(std::ostream& str, const Program& program);
void SerializeProgram(std::ostream& str, const Program& program);
void SerializeBasicBlock(std::ostream& str, const BasicBlock& basicBlock, const Program& program);
void SerializeFunction(std::ostream& str, const Function& function, const Program& program);
std::ostream& SerializeOperation(std::ostream& str, const Operation& op, const Program& program);

bool IsSignedLeafType(const Type* const type);
bool IsUnsignedLeafType(const Type* const type);

bool IsLegalWrapperType(const Type* const inputType);

class AllocatedLeafRegister : public AllocatedRegister
{
  public:
    AllocatedLeafRegister(const RegisterType registerType, const Type* const type, const size_t registerIndex);

    ~AllocatedLeafRegister();

    AccessedRegister GetAccessedRegister() const;

    void VisitRegisters(const UnaryVisitFunction& visitor) const override;

    void VisitRegisters(const AllocatedRegister* const rhs, const BinaryVisitFunction& visitor) const override;

    void Initialize(IRContext& context, const ParseTreeNode* const initialValueNode) const override;

    void Interpolate(IRContext& context, Operation& op, const size_t alignment, const size_t precision,
                     const ParseTreeFormatSpecifier format) const override;

    // Can be changed by CompactRegisterTable
    size_t _registerIndex;

  private:
    AllocatedLeafRegister operator=(const AllocatedLeafRegister& rhs) const;
};

class AllocatedStructRegister : public AllocatedRegister
{
  public:
    typedef std::pair<std::string, const AllocatedRegister*> EntryType;
    typedef std::vector<EntryType> ContainerType;

    AllocatedStructRegister(const RegisterType registerType, const Type* const type, const ContainerType& members)
        : AllocatedRegister(registerType, type), _members(members)
    {
    }

    void VisitRegisters(const UnaryVisitFunction& visitor) const override
    {
        // It is important (for cases like cast) that enumeration is in structure order
        for (const auto& p : _members)
        {
            p.second->VisitRegisters(visitor);
        }
    }

    void VisitRegisters(const AllocatedRegister* const rhs, const BinaryVisitFunction& visitor) const override
    {
        // It is important (for cases like cast) that enumeration is in structure order
        const AllocatedStructRegister* const structRhs = dynamic_cast<const AllocatedStructRegister*>(rhs);

        assert(_members.size() == structRhs->_members.size());

        for (size_t i = 0; i < _members.size(); ++i)
        {
            const EntryType& lhsEntry = _members[i];
            const EntryType& rhsEntry = structRhs->_members[i];

            // member names must match
            assert(lhsEntry.first == rhsEntry.first);

            lhsEntry.second->VisitRegisters(rhsEntry.second, visitor);
        }
    }

    // Note that initialValueNode can be null, in which case this should be zero-initialized
    void Initialize(IRContext& context, const ParseTreeNode* const initialValueNode) const override
    {
        // Initializing a class instance with an initializer list is not yet supported
        if (dynamic_cast<const ClassType*>(GetType()))
        {
            return;
        }

        const BaseInitializerListNode* const initializerListNode =
            dynamic_cast<const BaseInitializerListNode*>(initialValueNode);

        for (size_t i = 0; i < _members.size(); i++)
        {
            const EntryType& entry = _members[i];

            bool initialized = false;

            if (initializerListNode)
            {
                const ParseTreeNode* const child = initializerListNode->TryGetField(i, entry.first);

                if (child)
                {
                    entry.second->Initialize(context, child);

                    initialized = true;
                }
            }

            if (!initialized)
            {
                entry.second->Initialize(context, nullptr);
            }
        }
    }

    void SetConditionStackDepth(const size_t depth) override
    {
        AllocatedRegister::SetConditionStackDepth(depth);

        for (const EntryType& entry : _members)
        {
            const_cast<AllocatedRegister*>(entry.second)->SetConditionStackDepth(depth);
        }
    }

    void Interpolate(IRContext& context, Operation& op, const size_t alignment, const size_t precision,
                     const ParseTreeFormatSpecifier format) const override;

    const ContainerType _members;

  private:
    AllocatedStructRegister operator=(const AllocatedStructRegister& rhs) const;
};

bool FixupLutOp(Operation& op);
bool DetectConstantLut(Operation& op);
LutEntry RemoveSourceFromLut(const LutEntry& inputLutEntry, const size_t srcIndex, const size_t knownValue);

// Number of clock specifiers
static const size_t MaxClockCount = 1;

void ComputeFifoSize(const size_t configMinDepth, const size_t configDualClockMinDepth,
                     const size_t configMinimumAlmostFullDepth, const size_t configAlmostFullBackwardLatency,
                     const size_t minWriteDelay, const size_t maxWriteDelay, const size_t modifierMinDepth,
                     const size_t modifierCallRatePerThread, const size_t readClock, const size_t writeClock,
                     const size_t stageIndex, const bool addToResultDepth, const size_t minDepth, const size_t maxDepth,
                     const size_t syncCallerMaxThreadCount, const bool contextSaverHasTransactionSize,
                     const size_t transactionSize, size_t& resultLogDepth, size_t& resultAlmostFullDepth,
                     size_t& resultWriteDelay, size_t& resultMinWriteDelay);

const FunctionNode* GetFunctionByName(const ClassType* const classType, const Scope& scope, const std::string& name,
                                      const Location& location);

struct OperationListDependencyGraph
{
    // For each operation, tracks the set of operations that must be scheduled first
    std::map<const Operation*, std::set<const Operation*>> backwardOperationDependencies;

    // For each operation, tracks the set of operations that must be scheduled later
    std::map<const Operation*, std::set<const Operation*>> forwardOperationDependencies;

    // For operations contained within an atomic block, maps the operation to a unique ID associated with the atomic
    // block
    std::map<const Operation*, std::vector<size_t>> operationToAtomicBlockIndex;

    // Maps atomic block ID to BeginAtomic operation
    std::map<size_t, const Operation*> atomicBlockIndexToBeginAtomicOp;

    // Map BeginAtomic operation to atomic block ID
    std::map<const Operation*, size_t> beginAtomicOpToAtomicBlockIndex;

    // Maps atomic block ID to EndAtomic operation
    std::map<size_t, const Operation*> atomicBlockIndexToEndAtomicOp;
};

OperationListDependencyGraph ComputeOperationListDependencyGraph(const OperationList& operations,
                                                                 const Program& program);

void AddCodeCoverageContext(CodeCoverage& codeCoverage, const IRContext& context);
OperationList CreateCodeCoverageCounter(Program* const program, const std::string name, const std::string description,
                                        const Location& location, const SourceOperand counterInput,
                                        const CodeCoverage& codeCoverage);

FileAndLineNumber LocationToFileAndLineNumber(const Location& loc);
FileAndLineNumber LocationToFileAndLineNumber(const Location& loc, const size_t callStackIndex);
Location FileAndLineNumberToLocation(const FileAndLineNumber& faln);
std::string LocationToString(const Location& loc, bool stripLeadingPath = true, const std::string& separator = ":",
                             bool appendLineNumber = true);
std::string FunctionNameStackToString(std::stack<std::string> functionNameStack);
void GenerateCallTrace(std::ostream& str, const IRContext& context);

bool RemoveUnusedLutSourceRegisters(Operation& op);

bool OperationUsesOnlyLocalSrcReg(const Program& program, const Operation& op);
bool OperationUsesOnlyLocalDstReg(const Program& program, const Operation& op);
bool OperationUsesOnlyLocalReg(const Program& program, const Operation& op);

void GetAccessedRegisters(const Operation& op, std::vector<size_t>& read, std::vector<size_t>& write);

size_t InsertStallLogic(IRContext& context);

void AssertUnscheduledOperationsHaveLocations(const Program& program, const BasicBlock& bb);
void AssertScheduledOperationsHaveLocations(const Program& program, const BasicBlock& bb);

bool SaveLeafCondition(IRContext& context, bool childIsLeaf);
std::string CodeCoverageTypeToString(const CodeCoverageType codeCoverageType);

void DetermineUnusedBitsForFunction(Program& program, ControlFlowGraph& cfg, const Function& function,
                                    RegisterToBitSet& result);

// RAII class for condition coverage
class PushPopConditionCoverageTracker
{
  public:
    PushPopConditionCoverageTracker(bool valid, IRContext& context, CodeCoverageType codeCoverageType);
    PushPopConditionCoverageTracker(bool valid, IRContext& context, CodeCoverageType codeCoverageType,
                                    const AccessedRegister& topLevelCondition,
                                    const Location& topLevelConditionLocation,
                                    const ParseTreeNode* const topLevelConditionNode);
    ~PushPopConditionCoverageTracker();

  private:
    PushPopConditionCoverageTracker& operator=(const PushPopConditionCoverageTracker&);

    bool _valid;
    IRContext& _context;

    // Top-level condition information saved from constructor
    bool _topLevelConditionValid;
    const AccessedRegister* _topLevelCondition;
    const Location* _topLevelConditionLocation;
    const ParseTreeNode* _topLevelConditionNode;
};

OperationList ReduceVecBinaryOp(Program& program, const std::vector<AccessedRegister>& srcRegs,
                                const ParseTreeBinaryOpType reduceOp, const AccessedRegister& dstReg,
                                const Location& location, const std::string& intermediateRegName,
                                const mp_int& defaultValue);

std::vector<std::string> TokenizeObjectName(const std::string& objectName, const Program& program);

// A way to save a "pointer" to a location in an operation list
// Useful for cases where a pointer or iterator could be invalidated
// between save and recall.
class OperationLocationRecord
{
  public:
    OperationLocationRecord(BasicBlock& basicBlock, Program& program);

    ~OperationLocationRecord();

    struct ListAndIterator
    {
        OperationList* _list;
        OperationList::iterator _iterator;
    };

    // Returns an iterator that is 1 past the location record
    // Typically used with std::list::insert
    // Also returns the associated list.
    // This is useful because BasicBlock contains 2 lists of operations
    // The regular operations and the start condition operations
    ListAndIterator GetInsertListAndIterator() const;

  private:
    ListAndIterator GetLocationRecordListAndIterator() const;

    BasicBlock& _basicBlock;
    size_t _recordIndex;
};

void GetLiveInToSuccessor(const Operation& op, const BasicBlock& basicBlock, RegisterSetMap& liveInMap,
                          const std::unordered_set<const BasicBlock*>& returnSites, RegisterSet& liveInToSuccessor);

void ComputeLiveInMap(Program& program, RegisterSetMap& liveInMap, std::unordered_set<const BasicBlock*>& returnSites,
                      const OperationEnumerationMode mode);

void ComputeLiveInMap(const Program& program, Function& function, ControlFlowGraph& controlFlowGraph,
                      const std::unordered_set<const BasicBlock*>& returnSites, RegisterSetMap& liveInMap,
                      const OperationEnumerationMode mode);

// A function that allocates a Lut structure given a specified width
using AllocateLutFn = std::function<Lut(const size_t)>;

enum class LowerToLutBehavior
{
    Default,
    DeepCopy
};

bool LowerToLut(const Program& program, Operation& op, const AllocateLutFn& allocateLutFn,
                const LowerToLutBehavior behavior = LowerToLutBehavior::Default);

bool DecomposeWideOps(Program& program, Function& function);

size_t GetMuxPathLength(const size_t numChoices);

Lut AllocateLut(const size_t numDstBits);

bool FixupLutOps(BasicBlock& basicBlock);

// Can be used to allocate LUTs with storage released when
// this object goes out of scope
class LutArenaAllocator
{
  public:
    AllocateLutFn LutFn();

  private:
    std::list<std::vector<LutEntry>> _allocated;
};

size_t OperationHash(const Operation& op);

bool LutOperationsProduceSameResult(const Operation& op1, const Operation& op2);

void LinkExternalClassCallbacks(IRContext& context);

void PushPredicateOrLiteralOne(IRContext& context, Operation& op);

void ValidateLocalsAreWritten(const Program& program, Function& function);
