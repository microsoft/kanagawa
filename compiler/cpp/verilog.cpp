// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

#include <circt/Support/LLVM.h>

#include <circt/Dialect/Comb/CombOps.h>
#include <circt/Dialect/HW/HWOps.h>
#include <circt/Dialect/Kanagawa/KanagawaOps.h>
#include <circt/Dialect/Pipeline/PipelineOps.h>
#include <circt/Dialect/SV/SVOps.h>
#include <circt/Dialect/Seq/SeqOps.h>
#include <circt/Support/BackedgeBuilder.h>

// Redirect assertions to a function that will throw an exception on release builds
#include "ship_assert.h"

// Maps module port name to type
using PortNameToType = std::map<std::string, mlir::Type>;

// Don't require optimizations to respect semantics related to unknown values
static const bool TwoState = true;

// Hardcoded LFSR width = 11
static const size_t LfsrWidth = 11;

// Hard-coded circt port indices which are always present

// HW module input/output ports
enum class CirctModulePort
{
    // Inputs
    Clock = 0,
    Reset, // Must be the last of the inputs

    // Outputs
    Done, // Indicates that a thread is running the last pipeline stage on the current clock cycle

    Count
};

// HW module output ports
// These must match up with CirctModulePort
enum class CirctModuleOutputPort
{
    Done,

    Count
};

enum class CirctPipelineResultValues
{
    // Pipelined version of the go signal
    Done = 0,

    Count
};

// Reduce N input values down to a single value
template <typename T>
std::string BitwiseReduce(const T& container, const std::string& operatorStr, const std::string& defaultValue)
{
    std::ostringstream result;

    if (container.empty())
    {
        result << defaultValue;
    }
    else
    {
        result << "(" << operatorStr << "{" << join(container, ", ") << "})";
    }

    return result.str();
}

bool MemoryRequiresReset(const RegisterDescription& regDesc)
{
    assert(RegisterType::Memory == regDesc._type);

    bool result = false;

    // Memory initialization logic
    if (GetCodeGenDeviceConfig()._memoryInitFileType == MemoryInitFileType::Asic &&
        !regDesc.Memory()._initialValues.empty())
    {
        result = true;
    }

    // ECC error injection logic
    if (regDesc.Memory()._ecc)
    {
        result = true;
    }

    return result;
}

void GetSFormat(const Operation& op, const std::function<void(const std::string&)>& emitStr,
                const std::function<void(size_t)>& emitSrcOp)
{
    assert(Opcode::FormatString == op._opcode);
    assert(0 == op._dst.size());

    // First compute the format string
    std::string formatString;

    for (const FormatStringEntry& formatStringEntry : *(op._flags._formatString._entries))
    {
        switch (formatStringEntry._type)
        {
        case FormatStringType::StringLiteral:
        {
            const std::string literal = op._src[formatStringEntry._operandIndex].GetStringLiteral();

            // work-around Verilator behavior of sometimes converting $sformatf("%s", "") into a string of length 1
            if (!literal.empty())
            {
                formatString += "%s";
            }
        }
        break;

        default:
            // A function will be called to convert the result to a string
            formatString += "%s";
            break;
        }
    }

    if (formatString.empty())
    {
        // Don't emit $sformatf, just emit a literal empty string
        emitStr("\"\"");
    }
    else
    {
        emitStr("$sformatf(\"" + formatString + "\"");

        // Now emit the format arguments
        for (const FormatStringEntry& formatStringEntry : *(op._flags._formatString._entries))
        {
            if ((FormatStringType::StringLiteral == formatStringEntry._type) &&
                (op._src[formatStringEntry._operandIndex].GetStringLiteral().empty()))
            {
                // Verilator work-around from above
                continue;
            }

            emitStr(", ");

            switch (formatStringEntry._type)
            {
            case FormatStringType::StringLiteral:
            {
                const std::string literal = op._src[formatStringEntry._operandIndex].GetStringLiteral();

                assert(!literal.empty());

                emitStr("\"" + EscapeSpecialChars(op._src[formatStringEntry._operandIndex].GetStringLiteral()) + "\"");
            }
            break;

            case FormatStringType::StringHandle:
            {
                emitStr("string_table.align(");
                emitStr("string_table.get(");
                emitSrcOp(formatStringEntry._operandIndex);
                emitStr(", 1'b1)");
                emitStr("," + std::to_string(formatStringEntry._alignment));
                emitStr(")");
            }
            break;

            case FormatStringType::Integer:
            {
                std::string specifier = "%d";

                switch (formatStringEntry._specifier)
                {
                case ParseTreeFormatSpecifierBin:
                    specifier = "%b";
                    break;
                case ParseTreeFormatSpecifierOct:
                    specifier = "%o";
                    break;
                case ParseTreeFormatSpecifierHex:
                    specifier = "%x";
                    break;
                case ParseTreeFormatSpecifierHexUpper:
                    specifier = "%x";
                    break;
                }

                const bool toUpperCase = formatStringEntry._specifier == ParseTreeFormatSpecifierHexUpper;

                const bool isSigned =
                    formatStringEntry._signed && ((formatStringEntry._specifier == ParseTreeFormatSpecifierDec) ||
                                                  (formatStringEntry._specifier == ParseTreeFormatSpecifierNone));

                emitStr("string_table.itoa($sformatf(\"" + specifier + "\", ");
                if (isSigned)
                {
                    emitStr("((");
                    emitSrcOp(formatStringEntry._operandIndex);
                    emitStr(" >> " + std::to_string(formatStringEntry._signBitIndex - 1) + ") & 1'b1 ? ");
                    emitStr(" " + std::to_string(formatStringEntry._signBitIndex + 1) + "'(");
                    emitStr("~(");
                    emitSrcOp(formatStringEntry._operandIndex);
                    emitStr(") + 1) : ");
                    emitSrcOp(formatStringEntry._operandIndex);
                    emitStr(")");
                }
                else
                {
                    emitSrcOp(formatStringEntry._operandIndex);
                }
                emitStr(")");
                emitStr("," + std::to_string(formatStringEntry._precision));
                emitStr("," + std::to_string(formatStringEntry._alignment));
                emitStr("," + std::to_string(toUpperCase ? 1 : 0));

                emitStr(", ");
                if (isSigned)
                {
                    // Indicate if ito should prepend a negative sign
                    emitStr("(");
                    emitSrcOp(formatStringEntry._operandIndex);
                    emitStr(" >> " + std::to_string(formatStringEntry._signBitIndex - 1) + ") & 1'b1");
                }
                else
                {
                    emitStr("1'b0"); // to not add a negative sign
                }
                emitStr(")");
            }
            break;

            case FormatStringType::Boolean:
                emitStr("string_table.align(");
                emitSrcOp(formatStringEntry._operandIndex);
                emitStr(" ? \"true\" : \"false\"");
                emitStr(", " + std::to_string(formatStringEntry._alignment) + ")");
                break;

            case FormatStringType::Float:
                emitStr("string_table.align(");
                emitStr("$sformatf(\"%f\", $bitstoshortreal(");
                emitSrcOp(formatStringEntry._operandIndex);
                emitStr(")), " + std::to_string(formatStringEntry._alignment) + ")");
                break;

            default:
                assert(false);
            }
        }

        // end the $sformatf
        emitStr(")");
    }
}

// Returns an empty string when width < 2
// Otherwise, returns "[width-1:0]"
std::string OptionalWidthDeclarationNoSpace(const size_t width)
{
    assert(width > 0);

    std::ostringstream result;

    if (width > 1)
    {
        result << "[" << width - 1 << ":0]";
    }

    return result.str();
}

// Returns an empty string when width < 2
// Otherwise, returns "[width-1:0] " (with a space at the end)
std::string OptionalWidthDeclaration(const size_t width)
{
    assert(width > 0);

    std::ostringstream result;

    if (width > 1)
    {
        result << "[" << width - 1 << ":0] ";
    }

    return result.str();
}

// return: "[offset+width-1 : Offset]"
std::string Slice(const size_t offset, const size_t width)
{
    assert(width > 0);

    std::ostringstream str;

    str << "[" << offset + width - 1 << ":" << offset << "]";

    return str.str();
}

std::string BuildNetSliceStr(const std::string& prefix, const std::string& suffix, const size_t offset,
                             const size_t width)
{
    assert(width > 0);

    std::ostringstream ostr;

    ostr << prefix << suffix << Slice(offset, width);

    return ostr.str();
}

size_t GetConfiguredClockFrequency(size_t clockNum)
{
    assert(clockNum < MaxClockCount);

    size_t freq = GetCodeGenConfig()._frequency[clockNum];
    if (freq == 0)
    {
        // Use default value
        freq = GetCodeGenDeviceConfig()._defaultFreqMhz[clockNum];
    }

    return freq;
}

static const char* const c_atomicFunctionReturnName = "atomicFunctionReturnValue_out";

static size_t GetAlmostEmptyDepth()
{
    // Intel show-ahead fifos require almost empty to be 0 or >= 4
    // Xilinx show-ahead fifos require almost empty to be 0 or >= 5
    return GetCodeGenConfig()._allowFifoStutter ? GetCodeGenDeviceConfig()._almostEmptyDepth : 0;
}

static std::string GetClockIdSuffix(const size_t clockIndex)
{
    // clock0 is just named "clk"
    // other clocks are named "clk1" etc
    std::ostringstream clockIdStr;
    if (clockIndex > 0)
    {
        clockIdStr << clockIndex;
    }

    return clockIdStr.str();
}

static std::string GetClockString(const size_t clockIndex)
{
    std::ostringstream str;
    str << "clk" << GetClockIdSuffix(clockIndex);
    return str.str();
}

static std::string GetResetString(const size_t clockIndex, const std::string& resetReplica)
{
    std::ostringstream str;

    if (0 == clockIndex)
    {
        // Use the role clock reset replica
        str << resetReplica;
    }
    else
    {
        // Use the reset specific to the clock domain that this module runs on
        str << "rst_array[" << clockIndex << "]";
    }

    return str.str();
}

static std::string GetBasicBlockControlStateName(const BasicBlock& basicBlock)
{
    std::ostringstream str;
    str << GetBasicBlockName(basicBlock) << "_control_state";

    return str.str();
}

// Computes the depth of the tree needed to track race count
RaceCounterTreeDesc GetRaceCounterWidthDepth(const size_t rawSize, const size_t reductionWidth)
{
    size_t depth = 1;

    size_t currentWidth = reductionWidth;

    while (currentWidth < rawSize)
    {
        currentWidth *= reductionWidth;

        depth++;
    }

    RaceCounterTreeDesc result = {};

    result._width = currentWidth;
    result._depth = depth;

    return result;
}

size_t GetClampedRegisterWidth(const size_t logicalWidth)
{
    // For basic blocks with no live variables, a fifo is still used (just for keep track of threads)
    return std::max<size_t>(logicalWidth, 1);
}

static std::ostream& operator<<(std::ostream& str, const Literal& literalVal)
{
    SaveIosFmt<std::ostream::char_type> saveIos(str);

    // literalVal._value can actually be wider than the width specified by literalVal._width
    // Those upper bits need to be removed before we output the hex number, otherwise
    // we will get Verilog compiler warnings about extra digits in literal.
    auto widthCorrectedValue = MpSlice(literalVal._value, 0, literalVal._width);

    const size_t displayWidth = (literalVal._width / 4) + (literalVal._width % 4 != 0);
    str << std::dec << literalVal._width << "'h" << std::hex << std::setfill('0') << std::setw(displayWidth)
        << widthCorrectedValue << std::dec;

    return str;
}

// Returns the names of all parameters for a function, concatenated
// Used for extern/export functions
std::string GetConcatFunctionParams(const Function* const function)
{
    std::ostringstream str;

    const FunctionNode* const functionNode = function->_functionNode;

    str << "{";

    for (size_t invI = 0; invI < functionNode->GetParameterCount(); invI++)
    {
        if (invI > 0)
        {
            str << ", ";
        }

        const size_t i = functionNode->GetParameterCount() - invI - 1;

        str << function->_name << "_" << functionNode->GetParameterName(i);
    }

    str << "}";

    return str.str();
}

std::string GetExternalClassInstanceCombinedName(const ExternalClassInstance* externModule)
{
    return FixupString(FlattenScopeAndAppendName(externModule->_scope, externModule->_name));
}

std::string GetExternalClassInstanceSignalPrefix(const Function* const function)
{
    std::ostringstream str;

    str << function->GetBackendName();

    return FixupString(str.str());
}

std::string GetRegisterBaseName(const Program& program, const size_t registerIndex)
{
    const RegisterDescription regDesc = program._registerTable[registerIndex];

    std::string prefix;

    switch (regDesc._type)
    {
    case RegisterType::Pipeline:
        prefix = "reg";
        break;
    case RegisterType::Wire:
        prefix = "wire";
        break;
    default:
        assert(false);
        break;
    }

    return prefix + std::to_string(registerIndex) + "_" + regDesc._name;
}

std::string GetBasicBlockInstanceName(const BasicBlock& basicBlock) { return GetBasicBlockName(basicBlock) + "Impl"; }

class VerilogCompiler;

class ModuleInstanceHelper
{
  public:
    ModuleInstanceHelper(VerilogCompiler& compiler, const mlir::Location& location);

    void SetModuleName(const std::string& name);

    void SetInstanceName(const std::string& name);

    void AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction, const mlir::Type& type,
                 const std::string& value);

    void AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction, const mlir::Type& type,
                 const mlir::Value& value);

    void AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction, const mlir::Value& value);

    void AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction, const mlir::Type& type,
                 const mlir::Value& pathToContainer, const circt::hw::InnerSymAttr& containerPortSymbol);

    void AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction, const mlir::Type& type);

    void AddStringParameter(const std::string& name, const std::string& value);

    void AddU64Parameter(const std::string& name, const uint64_t value);

    void AddBoolParameter(const std::string& name, const bool value);

    mlir::Type GetParameterizedIntegerType(const std::string& parameterName);

    void Generate(circt::OpBuilder* const opbIn = nullptr);

    mlir::Value GetPortValue(const std::string& name);

    mlir::Type ConcreteType(const mlir::Type srcType);

    mlir::Attribute ConcreteAttr(const mlir::Attribute srcAttribute);

  private:
    void AddParameter(const std::string& name, const mlir::Type type, const mlir::Attribute value);

    // .first is a PathOp
    using PathAndPortName = std::pair<mlir::Value, circt::hw::InnerSymAttr>;

    VerilogCompiler& _verilogCompiler;
    std::string _moduleName;
    std::string _instanceName;
    mlir::Location _location;

    mlir::SmallVector<circt::hw::PortInfo> _ports;

    // Used for creating the HWModuleExternOp
    // No default value is specified
    // which forces CIRCT to always emit the parameter
    // definition at the module instantiation
    mlir::SmallVector<mlir::Attribute> _parameterDeclarations;

    // Used for creating the InstanceOp
    mlir::SmallVector<mlir::Attribute> _parameterDefinitions;

    std::map<std::string, mlir::Type> _parameterNameToType;

    std::map<std::string, size_t> _parameterNameToU64Value;

    std::map<std::string, std::string> _portNameToStringValue;

    std::map<std::string, mlir::Value> _portNameToMlirValue;

    std::map<std::string, PathAndPortName> _portNameToPathAndPort;

    PortNameToType _portNameToType;

    std::map<std::string, size_t> _portNameToIndex;
};

class VerilogCompiler
{
    friend class ModuleInstanceHelper;

    // Special cases for DSP registers in hardended blocks
    enum class SourceOpToStringMode
    {
        Default,
        NextValue
    };

    enum class DestOpToStringMode
    {
        Default,
        FFValue
    };

    // Adds information about constant bits to RTL map
    // and adds assertions to generated RTL to ensure these bits are constant
    class ConstantSignalHelper
    {
      public:
        ConstantSignalHelper(VerilogCompiler& compiler, JsonValue& jsonArray, const std::string& resetName,
                             const bool popSection)
            : _compiler(compiler), _resetName(resetName), _jsonArray(jsonArray), _popSection(popSection)
        {
        }

        ~ConstantSignalHelper()
        {
        }

        // Called when the net is not a member of a struct
        void AddConstantBit(const std::string& name, const size_t offset, const bool value)
        {
            AddConstantBit("", name, offset, value);
        }

        // when container is non-empty, then field is a member of a struct
        // otherwise, field is just a net name
        void AddConstantBit(const std::string& container, const std::string& field, const size_t offset,
                            const bool value)
        {
            const size_t valueInt = value ? 1 : 0;

            {
                JsonValue constantBit = JsonValue::CreateObject();

                auto& rtlMap = _compiler._rtlMap;

                if (!container.empty())
                {
                    constantBit.AddMember("container", rtlMap.SerializeString(container));
                }

                constantBit.AddMember("name", rtlMap.SerializeString(field));
                constantBit.AddMember("offset", rtlMap.SerializeSizeT(offset));
                constantBit.AddMember("value", rtlMap.SerializeSizeT(valueInt));

                _jsonArray.PushBack(constantBit);
            }

            {
                const std::string combinedName = container.empty() ? field : container + "." + field;

                const std::string bitName = combinedName + "[" + std::to_string(offset) + "]";

                const std::string expected = "1'b" + std::to_string(valueInt);

                // RTL simulators output 'x from an addition if any input bits
                // are unknown, even if one can logically deduce that some output bits are known
                std::ostringstream str;

                str << "assert property (@(posedge clk) (!" << _resetName << " |-> ((" << bitName << " === 1'bx) || ("
                    << bitName << " == " << expected << ")))) else $error (\"%m " << bitName
                    << " was not constant as expected\");";

                _assertions.push_back(str.str());
            }
        }

        // Called when the net is not a member of a struct
        void AddFifoCodeLiterals(const std::string& name, const FifoCode& fifoCode)
        {
            AddFifoCodeLiterals("", name, fifoCode);
        }

        void AddFifoCodeLiterals(const std::string& container, const std::string& field, const FifoCode& fifoCode)
        {
            if (fifoCode._initialized)
            {
                for (const FifoCodeOutputRange& range : fifoCode._outputRanges)
                {
                    if (range._isLiteral)
                    {
                        const mp_int& value = range._literal;

                        for (size_t i = 0; i < range._width; i++)
                        {
                            AddConstantBit(container, field, i + range._decodedOffset, bit_test(value, i));
                        }
                    }
                }
            }
        }

      private:
        VerilogCompiler& _compiler;

        std::string _resetName;

        JsonValue& _jsonArray;

        bool _popSection;

        std::vector<std::string> _assertions;
    };

    // Emit json and assertions for constant bits in global fifo structs
    void EmitFifoTopLevelConstants(const size_t registerIndex, const std::string& container, const std::string& field)
    {
        const FifoCode& fifoCode = _program._registerTable[registerIndex].Fifo()._code;

        ConstantSignalHelper helper(*this, _jsonConstantSignals, "rst", false);

        helper.AddFifoCodeLiterals(container, field, fifoCode);
    }

  public:
    VerilogCompiler(const char* const svFileName, const char* const svPackageFileName, const char* const cppFileName,
                    const char* const headerFileName, const char* const memFileBase, const char* const rtlMapFileName,
                    const char* const circtAsmFileName, const Program& program)
        : _writer(svFileName), _program(program), _cppFileName(cppFileName),
          _headerFileName(headerFileName), _memFileBase(memFileBase), _resetReplicaIndex(0), _inputValidReplicaIndex(0),
          _selectTableIndex(0), _threadCountNameIndex(0), _basicBlockHasStalls(false),
          _basicBlockHasIntraPipelineStalls(false), _svPackageFileName(svPackageFileName),
          _rtlMap(rtlMapFileName), _circtAsmFileName(circtAsmFileName),
          _jsonDebugSignals(JsonValue::CreateArray()), _jsonConstantSignals(JsonValue::CreateArray()),
          _jsonGlobalVariables(JsonValue::CreateArray()), _jsonFifos(JsonValue::CreateArray()),
          _jsonLoopGenerators(JsonValue::CreateArray()), _fifoNamer(program), _coreModule(nullptr), _mlirModule(nullptr)
    {
        _compileContext = CompileContext{};
    }

    ~VerilogCompiler()
    {
        _rtlMap.AddMember("debug_signals", _jsonDebugSignals);
        _rtlMap.AddMember("constant_signals", _jsonConstantSignals);
        _rtlMap.AddMember("global_variables", _jsonGlobalVariables);
        _rtlMap.AddMember("fifos", _jsonFifos);
        _rtlMap.AddMember("loop_generators", _jsonLoopGenerators);
    }

    // RAII class to emit // coverage {off,on}
    class DisableCodeCoverage
    {
      public:
        DisableCodeCoverage(VerilogCompiler& compiler) : _compiler(compiler)
        {
            _compiler._writer.Str() << "// coverage off";
        }

        ~DisableCodeCoverage() { _compiler._writer.Str() << "// coverage on"; }

      private:
        DisableCodeCoverage& operator=(const DisableCodeCoverage&) const;

        VerilogCompiler& _compiler;
    };

    // RAII class to wrap code that should not be excluded in code coverage builds
    class DisableDynamicAssert
    {
      public:
        DisableDynamicAssert(VerilogCompiler& compiler) : _compiler(compiler)
        {
            _compiler._writer.Str() << "`ifndef NO_DYNAMIC_ASSERTS";
        }

        ~DisableDynamicAssert() { _compiler._writer.Str() << "`endif"; }

      private:
        VerilogCompiler& _compiler;
    };

    // Merge modules used by context savers
    void DeclareContextSaverMergers()
    {
        for (const ContextSaver& contextSaver : _program._contextSavers)
        {
            const RegisterDescription outputRegDesc = _program._registerTable[contextSaver._destinationFifo];
            const RegisterDescription calleeRegDesc = _program._registerTable[contextSaver._fromCalleeFifoIndex];
            const RegisterDescription callerRegDesc = _program._registerTable[contextSaver._fromCallerFifoIndex];

            if (outputRegDesc._width == 0)
            {
                // Don't declare an empty module
                continue;
            }

            // Get the normalized caller/callee FIFO indexes
            const size_t callerFifoMappedIndex = _fifoNamer.GetNormFifoIndex(contextSaver._fromCallerFifoIndex);

            // Module that combines the callee and caller data
            _writer.Str() << "module " << GetModuleNamePrefix() << "ContextSaverMerger" << callerFifoMappedIndex;
            _writer.Str() << "(";

            {
                AutoIndent autoIndent2(_writer);

                if (callerRegDesc._width > 0)
                {
                    _writer.Str() << "input wire [" << callerRegDesc._width - 1 << ":0] caller_data,";
                }

                if (contextSaver.GetCalleeOutputWidth(_program) > 0)
                {
                    _writer.Str() << "input wire [" << contextSaver.GetCalleeOutputWidth(_program) - 1
                                  << ":0] callee_data,";
                }

                assert(outputRegDesc._width > 0);
                _writer.Str() << "output logic [" << outputRegDesc._width - 1 << ":0] output_data";
            }

            {
                AutoSectionRAII moduleClose(_writer, true, ");", "endmodule");
                {
                    AutoSectionRAII alwaysCombBlock(_writer, true, "always_comb begin", "end");
                    for (const std::pair<size_t, size_t>& p : contextSaver._fromCalleeOutputMap)
                    {
                        const size_t registerIndex = p.first;
                        const size_t offset = p.second;
                        const size_t width = _program._registerTable[registerIndex]._width;

                        assert(width > 0);

                        const auto pipelinedCallerIt = contextSaver._pipelinedCallerReturnRegisters.find(registerIndex);

                        if (contextSaver._pipelinedCallerReturnRegisters.end() == pipelinedCallerIt)
                        {
                            const auto it = contextSaver._calleeInputMap.find(registerIndex);
                            assert(it != contextSaver._calleeInputMap.end());

                            _writer.Str()
                                << "output_data[" << (offset + width - 1) << ":" << offset << "] = callee_data["
                                << (it->second + width - 1) << ":" << it->second << "];";
                        }
                        else
                        {
                            _writer.Str()
                                << "output_data[" << (offset + width - 1) << ":" << offset << "] = callee_data["
                                << (pipelinedCallerIt->second + width - 1) << ":" << pipelinedCallerIt->second << "];";
                        }
                    }

                    for (const std::pair<size_t, size_t>& p : contextSaver._fromCallerOutputMap)
                    {
                        const size_t registerIndex = p.first;
                        const size_t offset = p.second;
                        const size_t width = _program._registerTable[registerIndex]._width;

                        assert(width > 0);

                        const auto it = contextSaver._callerInputMap.find(registerIndex);
                        assert(it != contextSaver._callerInputMap.end());

                        _writer.Str() << "output_data[" << (offset + width - 1) << ":" << offset << "] = caller_data["
                                      << (it->second + width - 1) << ":" << it->second << "];";
                    }
                }
            }
        }
    }

    void WriteStallRatePorts(ModuleDeclarationHelper& coreModule)
    {
        coreModule.AddPort("stall_rate_in", circt::hw::ModulePort::Direction::Input, 3);
        coreModule.AddPort("stall_rate_valid_in", circt::hw::ModulePort::Direction::Input, 1);
        coreModule.AddPort("stall_rate_supported_out", circt::hw::ModulePort::Direction::Output, 1);
    }

    void AssignStallRatePorts(ModuleDeclarationHelper& coreModule)
    {
        _writer.Str() << "assign " << coreModule.AssignPort("stall_rate_supported_out")
                      << ((_program._numStallers > 0) ? " = 1'b1" : " = 1'b0") << ";";
    }

    struct BypassAndWriteDelay
    {
        size_t _bypassSlots;
        size_t _writeDelay;
        bool _hardenedBypass;
    };

    BypassAndWriteDelay CalculateMemoryBypassAndWriteDelay(const bool requireBypass, const size_t readLatency,
                                                           const RegisterDescription& regDesc)
    {
        const bool isQuadPort = regDesc.Memory()._quadPort;
        const bool ecc = regDesc.Memory()._ecc;
        const bool memoryInBRam = ShouldUseRamType(regDesc, RAM_TYPE::BLOCK);

        // Hardened bypass does not work when memory output registers are disabled
        // The Intel forwarding logic will forward in cases where it should not
        // the work-around from Intel is to add pipeline registers to the memory inputs
        // but that defeats the purpose of disabling the memory output register
        // GetCodeGenConfig()._rmwMemoryReadDelay > 0 requires the bypass to occur at a different pipeline stage
        // than where the read occurs.
        // Also, when hardened bypass is used (only available on the S10 and Agilex), the maximum width of a simple
        // dual port BRAM mapped to an M20K is 20 bits.  For example, a 32 deep, 32-bit wide RAM
        // would need to be split into two M20Ks.  Thus, if the width of the memory is greater than 20 but the
        // depth is 512 or less, we will opt for soft bypass logic rather than increasing the M20K requirement by
        // at least 60% while dropping the utilization of each M20K below 50%.
        const bool hardenedBypass = requireBypass && memoryInBRam &&
                                    GetCodeGenDeviceConfig()._supportsRmwBram && (readLatency == 2) &&
                                    !isQuadPort && !ecc && (GetCodeGenConfig()._rmwMemoryReadDelay == 0) &&
                                    !(regDesc.Memory()._elementWidth > GetCodeGenConfig()._rmwHardenedMemoryMaxWidth &&
                                      regDesc.Memory()._elementCount <= GetCodeGenConfig()._rmwHardenedMemoryMaxHeight);

        size_t writeDelay = 0;

        if (requireBypass)
        {
            // For bypass, add user-specified cycles of write delay
            // to removing the routing delay (from the output of the computation to the memory input register)
            // the computation stage is frequently the critical path, and the routing delay is non-trivial
            // There are 2 user-specified parameters: 1 write delay for memories that have hardened bypass, and another
            // for memories that do not
            writeDelay = hardenedBypass ? GetCodeGenConfig()._rmwHardenedMemoryWriteDelay
                                        : GetCodeGenConfig()._rmwMemoryWriteDelay;
        }

        if (regDesc.Memory().HasReadArbitration())
        {
            assert(requireBypass || !regDesc.Memory().HasAnyBypassPort());
        }

        // readLatency slots are needed for bypass to work correctly
        // the generated pipeline is like this:
        // stage 0: load
        // stage 1: compute
        // stage 2: store
        // The memory is configured in "OLD_DATA" mode
        //
        // writeDelay extra bypass slots are added because the store happens on stage 2 + writeDelay

        // Determine how far back in time the bypass logic needs to look
        // to handle read-modify-write operations

        // Write delay is only added when bypass is enabled
        assert((0 == writeDelay) || requireBypass);

        // Each cycle of write delay requires 1 bypass slot
        size_t numBypassSlots = writeDelay;

        if (requireBypass)
        {
            // Each cycle of read latency requires a bypass slot
            numBypassSlots += readLatency;

            // Hardened bypass can handle up to 2 slots
            if (hardenedBypass)
            {
                numBypassSlots = numBypassSlots >= 2 ? numBypassSlots - 2 : 0;
            }
        }

        BypassAndWriteDelay result = {};

        result._bypassSlots = numBypassSlots;
        result._writeDelay = writeDelay;
        result._hardenedBypass = hardenedBypass;

        // The implementation of --rmw-memory-favor-area
        // and GetResetHoldCycles assumes this
        assert(numBypassSlots <= GetCodeGenConfig().GetMaxBypassSlots());

        return result;
    }

    void InstantiateInspectables(ModuleDeclarationHelper& mod, const std::set<size_t>& globalsRequiringNext)
    {
        assert(GetCodeGenConfig()._inspection);

        if (GetCodeGenConfig()._detectRaces)
        {
            // A counter that counts the number of cycles where a race condition occurs
            _writer.Str() << "logic [" << c_raceCountWidth - 1 << ":0] race_count;";

            if (!_program._raceConditionRegisters.empty())
            {
                const size_t rawNumInputs = _program._raceConditionRegisters.size();

                // Each level of the reduction tree peforms reductions of this width
                const size_t reductionSize = 4;

                // Round rawNumInputs to a power of reductionSize
                const RaceCounterTreeDesc treeDesc = GetRaceCounterWidthDepth(rawNumInputs, reductionSize);

                _writer.Str() << "logic [" << treeDesc._width - 1 << ":0] race_bitvector;";

                auto it = _program._raceConditionRegisters.begin();

                for (size_t i = 0; i < rawNumInputs; i++)
                {
                    const size_t registerIndex = *it;
                    ++it;

                    const RegisterDescription& regDesc = _program._registerTable[registerIndex];
                    assert(RegisterTracksRaces(regDesc));

                    std::ostringstream str;

                    {
                        circt::OpBuilder& opb = mod.OpBuilder();

                        mlir::Value hasRaceOccured;

                        switch (regDesc._type)
                        {
                        case RegisterType::Global:
                        {
                            const mlir::Value pathToInstance =
                                GetPathToGlobalContainer(mod, ObjectPath(), registerIndex, globalsRequiringNext);

                            const GlobalRegKey key = GetGlobalRegKey(registerIndex, globalsRequiringNext);

                            hasRaceOccured = ReadContainerPort(opb, GetUnknownLocation(), pathToInstance,
                                                               key.GetFieldSymbol("has_race_occured_out").getSymName(),
                                                               GetIntegerType(1));
                        }
                        break;

                        case RegisterType::Memory:
                        {
                            const mlir::Value pathToInstance =
                                GetPathToMemoryContainer(mod, ObjectPath(), registerIndex);

                            hasRaceOccured =
                                ReadContainerPort(opb, GetUnknownLocation(), pathToInstance,
                                                  GetFullyQualifiedStringAttr(regDesc.Memory()._containerInstancePath,
                                                                              "has_race_occured_out"),
                                                  GetIntegerType(1));
                        }
                        break;

                        default:
                            assert(false);
                        }

                        mod.AddVerbatimOp(
                            GetUnknownLocation(), [&](VerbatimWriter& writer)
                            { writer << "assign race_bitvector[" << i << "] = " << hasRaceOccured << ";"; });
                    }
                }

                assert(it == _program._raceConditionRegisters.end());

                // Assign padding values to 0, they do participate in the reduction
                for (size_t i = rawNumInputs; i < treeDesc._width; i++)
                {
                    _writer.Str() << "assign race_bitvector[" << i << "] = 1'b0;";
                }

                // Instantiate module that counts the # of race conditions
                _writer.Str() << "KanagawaRaceCounter";

                _writer.Str() << "#(";

                {
                    AutoIndent autoIndent2(_writer);

                    _writer.Str() << ".COUNTER_WIDTH(" << c_raceCountWidth << "),";
                    _writer.Str() << ".INPUT_WIDTH(" << treeDesc._width << "),";
                    _writer.Str() << ".REDUCTION_SIZE(" << reductionSize << "),";
                    _writer.Str() << ".DEPTH(" << (treeDesc._depth) << ")";
                }

                _writer.Str() << ")";

                _writer.Str() << "race_counter";

                _writer.Str() << "(";

                {
                    AutoIndent autoIndent2(_writer);

                    _writer.Str() << ".clk(clk),";
                    _writer.Str() << ".rst(" << GetResetReplica() << "),";
                    _writer.Str() << ".race_occured_in(race_bitvector),";
                    _writer.Str() << ".race_count_out(race_count)";

                    _writer.Str() << ");";
                }
            }
        }

        for (auto it = _program._inspectableVariables.begin(); it != _program._inspectableVariables.end(); ++it)
        {
            auto nextIt = it;
            ++nextIt;

            const InspectableVariable& inspectableVariable = *it;

            const size_t inspectableIndex = inspectableVariable._inspectableIndex;

            _writer.Str() << "KanagawaTypes::InspectableValue inspectable_" << inspectableIndex << "_value_in;";
            _writer.Str() << "KanagawaTypes::InspectableValue inspectable_" << inspectableIndex << "_value_out;";

            std::string resetReplica = GetResetReplica();

            if (inspectableVariable._inspectionType != InspectableVariableType::ExternalClassInstanceInst)
            {
                const size_t totalBitWidth = GetInspectableBitWidth(_program, inspectableVariable);

                const bool isMemory = (RegisterType::Memory == inspectableVariable._registerType);

                // Memories are contained in 1 register
                assert((inspectableVariable._registers.size() == 1) || !isMemory);

                size_t memoryAddressWidth;
                size_t memoryElementCount;
                size_t memoryIndex;

                if (isMemory)
                {
                    memoryIndex = inspectableVariable._registers[0];
                    const RegisterDescription& memoryDesc = _program._registerTable[memoryIndex];
                    memoryAddressWidth = memoryDesc.GetMemoryAddressWidth();
                    memoryElementCount = memoryDesc.Memory()._elementCount;
                }
                else
                {
                    memoryIndex = 0;
                    memoryAddressWidth = 1;
                    memoryElementCount = 1;
                }

                // must match KanagawaTypes::INSPECTABLE_VALUE_WIDTH
                const size_t bitsPerFlit = 16;
                const size_t flitCount = Align(totalBitWidth, bitsPerFlit) / bitsPerFlit;

                if (inspectableVariable._inspectionType == InspectableVariableType::BasicBlockControlState)
                {
                    _writer.Str() << "logic [" << c_basicBlockControlWidth - 1 << ":0] "
                                  << GetBasicBlockControlStateName(*inspectableVariable._basicBlock) << ";";
                }

                ModuleInstanceHelper instance(*this, GetUnknownLocation());
                instance.SetModuleName("KanagawaInspectableVariable");
                instance.AddU64Parameter("VARIABLE_WIDTH", totalBitWidth);
                instance.AddU64Parameter("FLIT_COUNT", flitCount);
                instance.AddBoolParameter("IS_MEMORY", isMemory);
                instance.AddU64Parameter("MEMORY_ADDR_WIDTH", memoryAddressWidth);
                instance.AddU64Parameter("MEMORY_LATENCY",
                                         isMemory ? 2
                                                  : 0); // memory reads take 2 cycles, output register is always enabled
                instance.AddU64Parameter(
                    "MEMORY_ELEMENT_COUNT",
                    memoryElementCount); // memory reads take 2 cycles, output register is always enabled
                instance.SetInstanceName("inspectable_" + std::to_string(inspectableIndex));
                instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
                instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), resetReplica);

                if (isMemory)
                {
                    assert(inspectableVariable._inspectionType == InspectableVariableType::Default);

                    instance.AddPort("memory_addr_out", circt::hw::ModulePort::Direction::Output,
                                     instance.GetParameterizedIntegerType("MEMORY_ADDR_WIDTH"),
                                     "memory_data_" + std::to_string(memoryIndex) + ".inspection_addr");
                    instance.AddPort("memory_en_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                     "memory_data_" + std::to_string(memoryIndex) + ".inspection_success");

                    const std::string valueField = "inspection_data";

                    instance.AddPort("value_in", circt::hw::ModulePort::Direction::Input,
                                     instance.GetParameterizedIntegerType("VARIABLE_WIDTH"),
                                     "memory_data_" + std::to_string(memoryIndex) + "." + valueField);
                }
                else
                {
                    instance.AddPort("memory_addr_out", circt::hw::ModulePort::Direction::Output,
                                     instance.GetParameterizedIntegerType("MEMORY_ADDR_WIDTH"));
                    instance.AddPort("memory_en_in", circt::hw::ModulePort::Direction::Input, GetI1Type(), "1'b0");

                    std::ostringstream str;

                    if (InspectableVariableType::Default == inspectableVariable._inspectionType)
                    {
                        assert(!inspectableVariable._registers.empty());

                        // Concat all registers
                        bool isFirst = true;

                        for (auto it = inspectableVariable._registers.rbegin();
                             it != inspectableVariable._registers.rend(); ++it)
                        {
                            const size_t registerIndex = *it;

                            if (!isFirst)
                            {
                                str << ", ";
                            }
                            str << GetGlobalStructName(registerIndex) << ".value";
                            isFirst = false;
                        }
                    }
                    else if (inspectableVariable._inspectionType == InspectableVariableType::RaceCount)
                    {
                        assert(!_program._raceConditionRegisters.empty());

                        str << "race_count";
                    }
                    else if (inspectableVariable._inspectionType == InspectableVariableType::BasicBlockControlState)
                    {
                        str << GetBasicBlockControlStateName(*inspectableVariable._basicBlock);
                    }
                    else if (inspectableVariable._inspectionType == InspectableVariableType::CodeCoverage)
                    {
                        assert(inspectableVariable._registers.size() == 1);
                        const size_t registerIndex = inspectableVariable._registers[0];
                        str << GetGlobalStructName(registerIndex) << ".value";
                    }
                    else if (inspectableVariable._inspectionType == InspectableVariableType::SymbolHash)
                    {
                        // This code used to write _symbolHash value. Calculation of that was removed
                        // with framework mode. We wanted to keep the core inspectable code around, for
                        // possible future use, so for now I am just writing 0.
                        str << "64'h" << std::hex << 0 << std::dec;
                    }
                    else
                    {
                        assert(false);
                    }
                    instance.AddPort("value_in", circt::hw::ModulePort::Direction::Input,
                                     instance.GetParameterizedIntegerType("VARIABLE_WIDTH"), "{" + str.str() + "}");
                }

                instance.AddPort("inspectable_value_in", circt::hw::ModulePort::Direction::Input,
                                 GetInspectableStructType(),
                                 "inspectable_" + std::to_string(inspectableIndex) + "_value_in");
                instance.AddPort("inspectable_value_out", circt::hw::ModulePort::Direction::Output,
                                 GetInspectableStructType(),
                                 "inspectable_" + std::to_string(inspectableIndex) + "_value_out");
                instance.Generate();
            }
        }

        // Chain inspectable modules together
        size_t prevInspectableIndex = std::numeric_limits<size_t>::max();

        if (_program._inspectableVariables.empty())
        {
            // If inspectables have been disabled, then we will just have the input and output ports, but no
            // inspectables. In this situation, just connect the inspectable output port to the input port. Register
            // inpectable output to make Lint happy
            _writer.Str() << "KanagawaTypes::InspectableValue inspection_value_ff;";
            _writer.Str() << "assign " << mod.AssignPort("inspection_value_out") << " = inspection_value_ff;";

            _writer.Str() << "always_ff @(posedge clk)";
            _writer.Str() << "  inspection_value_ff <= inspection_value_in;";
        }
        else
        {
            for (auto it = _program._inspectableVariables.begin(); it != _program._inspectableVariables.end(); ++it)
            {
                const InspectableVariable& inspectableVariable = *it;

                const size_t inspectableIndex = inspectableVariable._inspectableIndex;

                const bool isFirst = (it == _program._inspectableVariables.begin());

                auto nextIt = it;
                ++nextIt;

                const bool isLast = (nextIt == _program._inspectableVariables.end());

                if (inspectableVariable._inspectionType == InspectableVariableType::ExternalClassInstanceInst)
                {
                    // If inspectable represents a connection to the chain of inspectables within an extern class
                    // instance then save the inspectable index in a map so we can attach the appropriate nets to the
                    // ports in the Verilog module instantiation
                    assert(nullptr != inspectableVariable._externClassInstance);
                    SafeInsert(_externClassInstanceInspectableIndices, inspectableVariable._externClassInstance,
                               inspectableIndex);
                }

                if (isFirst)
                {
                    _writer.Str() << "assign inspectable_" << inspectableIndex << "_value_in = inspection_value_in;";
                }
                else
                {
                    _writer.Str() << "assign inspectable_" << inspectableIndex << "_value_in = inspectable_"
                                  << prevInspectableIndex << "_value_out;";
                }

                if (isLast)
                {
                    _writer.Str() << "assign " << mod.AssignPortOptional("inspection_value_out") << " = inspectable_"
                                  << inspectableIndex << "_value_out;";
                }

                prevInspectableIndex = inspectableIndex;
            }
        }
    }

    void InstantiateCoverpoints(ModuleDeclarationHelper& mod, const std::set<size_t>& globalsRequiringNext)
    {
        DisableTranslation disableTranslation(_writer);

        // For each codeCoverageType
        for (size_t i = 0; i < (size_t)CodeCoverageType::NumCodeCoverageType; i++)
        {
            const CodeCoverageType codeCoverageType = (CodeCoverageType)i;

            // Get code coverage registers and names for this coverage type
            std::vector<size_t> codeCoverageRegs;
            std::vector<std::string> codeCoverageVars;
            for (const auto& inspectableVariable : _program._codeCoverageVariables)
            {
                if (!inspectableVariable._removed)
                {
                    assert(inspectableVariable._inspectionType == InspectableVariableType::CodeCoverage);
                    assert(inspectableVariable._registers.size() == 1);

                    const CodeCoverage& codeCoverage = inspectableVariable._codeCoverage;

                    if (codeCoverage._coverageType == codeCoverageType)
                    {
                        const size_t registerIndex = inspectableVariable._registers[0];

                        codeCoverageRegs.push_back(registerIndex);
                        codeCoverageVars.push_back(FixupString(inspectableVariable._name + "_" + codeCoverage._case));
                    }
                }
            }

            // Continue if no code coverage of this type
            if (codeCoverageRegs.size() == 0)
            {
                continue;
            }

            // Generate assign statement to rename from global to user-readable code coverage name
            for (size_t i = 0; i < codeCoverageVars.size(); i++)
            {
                const std::string& codeCoverageVar = codeCoverageVars[i];
                const size_t codeCoverageReg = codeCoverageRegs[i];
                _writer.Str() << "logic " << codeCoverageVar << ";";

                {
                    circt::OpBuilder opb = circt::OpBuilder::atBlockEnd(_coreModule->GetBodyBlock());

                    const mlir::Value pathToInstance =
                        GetPathToGlobalContainer(mod, ObjectPath(), codeCoverageReg, globalsRequiringNext);

                    const GlobalRegKey key = GetGlobalRegKey(codeCoverageReg, globalsRequiringNext);

                    const mlir::Value value =
                        ReadContainerPort(opb, GetUnknownLocation(), pathToInstance,
                                          key.GetFieldSymbol("value_out").getSymName(), GetIntegerType(1));

                    mod.AddVerbatimOp(GetUnknownLocation(), [&](VerbatimWriter& writer)
                                      { writer << "assign " << codeCoverageVar << " = " << value << ";"; });
                }
            }

            const std::string coverGroupName = "codeCoverage" + CodeCoverageTypeToString(codeCoverageType);

            // Create covergroup
            {
                AutoSectionRAII coverGroup(_writer, true, "covergroup " + coverGroupName + " @(posedge clk);",
                                           "endgroup");
                for (const std::string& codeCoverageVar : codeCoverageVars)
                {
                    _writer.Str() << "coverpoint " << codeCoverageVar << "{ bins covered = {1}; }";
                }
            }

            // Instantiate covergroup
            {
                AutoSectionRAII initialBlock(_writer, true, "initial begin", "end");
                _writer.Str() << "static " + coverGroupName + " " + coverGroupName + "Inst = new();";
            }
        }
    }

    void AddNetToJson(const std::string& varName, const size_t varWidth, JsonValue& jsonNets)
    {
        JsonValue jsonNet = JsonValue::CreateObject();
        jsonNet.AddMember("name", _rtlMap.SerializeString(varName));
        jsonNet.AddMember("width", _rtlMap.SerializeSizeT(varWidth));
        jsonNets.PushBack(jsonNet);
    }

    // Uniquely describes the module for a single global register
    struct GlobalRegKey
    {
        size_t _width;
        size_t _writeCount;
        bool _hasNextLogic;
        bool _tracksRaces;
        std::map<size_t, Literal> _literalValues;
        std::optional<Literal> _initialValue;

        std::string Name() const
        {
            std::ostringstream str;

            str << "reg_" << _width << "_w" << _writeCount;

            for (const auto& p : _literalValues)
            {
                str << "_v_" << p.first << "_" << p.second._width << "_" << p.second._value;
            }

            if (_initialValue)
            {
                str << "_i_" << _initialValue->_width << "_" << _initialValue->_value;
            }

            if (_hasNextLogic)
            {
                str << "_n";
            }

            if (_tracksRaces)
            {
                str << "_r";
            }

            return str.str();
        }

        // Combines the key with a field name to generate
        // a unique symbol name.
        // Note that this does not use containerInstancePath because
        // multiple instances (with different paths) can share the same
        // global register container
        circt::hw::InnerSymAttr GetFieldSymbol(const std::string& fieldName) const
        {
            const ObjectPath objectPath(1, Name());

            return GetFullyQualifiedInnerSymAttr(objectPath, fieldName);
        }
    };

    GlobalRegKey GetGlobalRegKey(const size_t registerIndex, const std::set<size_t>& globalsRequiringNext)
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];

        const RegisterDescription::GlobalDesc& globalDesc = regDesc.Global();

        const GlobalRegKey key = {regDesc._width,
                                  globalDesc._writeCount,
                                  Contains(globalsRequiringNext, registerIndex),
                                  RegisterTracksRaces(regDesc) && GetCodeGenConfig()._detectRaces,
                                  globalDesc._literalValues,
                                  GlobalHasResetPort(regDesc) ? std::optional<Literal>(regDesc.GetInitialValueLiteral())
                                                              : std::optional<Literal>()};

        return key;
    }

    void DeclareAndInstantiateGlobalVariablesCIRCT(const std::set<size_t>& globalsRequiringNext)
    {
        assert(_coreModule);

        std::set<size_t> inspectableVariables;

        if (GetCodeGenConfig()._inspection)
        {
            for (const InspectableVariable& inspectableVariable : _program._inspectableVariables)
            {
                for (const size_t registerIndex : inspectableVariable._registers)
                {
                    if (RegisterType::Global == _program._registerTable[registerIndex]._type)
                    {
                        inspectableVariables.insert(registerIndex);
                    }
                }
            }
        }

        circt::kanagawa::DesignOp designOp = GetDesignOp(_coreModule->MlirModule());

        circt::OpBuilder opb(designOp.getBodyRegion());

        std::set<ObjectPath> paths;

        std::map<std::string, circt::kanagawa::ContainerOp> nameToContainer;

        for (size_t i = 0; i < _program._registerTable.size(); ++i)
        {
            const RegisterDescription& regDesc = _program._registerTable[i];

            if (regDesc._type == RegisterType::Global)
            {
                const ObjectPath& containerInstancePath = regDesc.Global()._containerInstancePath;

                // Container instance paths should be unique
                // beacuse each leaf node is a single scalar register
                assert(!containerInstancePath.empty());

                // Each path should be a path to a container
                // that only contains this variable
                // Otherwise port names between variables would conflict
                SafeInsert(paths, containerInstancePath);

                const RegisterDescription::GlobalDesc& globalDesc = regDesc.Global();

                const GlobalRegKey key = GetGlobalRegKey(i, globalsRequiringNext);

                // Generate the name of the container
                // Based on the register properties
                // All globals with the same properties
                // will use the same leaf container
                const std::string leafContainerName = key.Name();

                const mlir::StringAttr leafContainerNameAttr = StringToStringAttr(leafContainerName);

                // Only create a container if one has not already been created with these properties
                const auto containerIt = nameToContainer.find(leafContainerName);

                if (nameToContainer.end() == containerIt)
                {
                    const mlir::Type valueType = GetIntegerType(regDesc._width);

                    // The container is not associated with any one location
                    // Because many instances can be associated with it
                    const mlir::Location location = GetUnknownLocation();

                    // The container has TopLevel = false to prepend the design name to the container name
                    // to avoid collisions between RTL modules generated by different export classes
                    circt::kanagawa::ContainerOp container = opb.create<circt::kanagawa::ContainerOp>(
                        location, circt::hw::InnerSymAttr::get(leafContainerNameAttr), false);

                    circt::OpBuilder::InsertionGuard g(opb);

                    // Add ports to the container
                    opb.setInsertionPointToEnd(container.getBodyBlock());

                    circt::seq::CompRegOp valueReg;

                    circt::BackedgeBuilder beb(opb, location);
                    circt::Backedge valueBackedge = beb.get(valueType);

                    const auto addInputPort = [&](const std::string& name, const mlir::Type& type)
                    {
                        const mlir::Value port = opb.create<circt::kanagawa::InputPortOp>(
                            location, key.GetFieldSymbol(name), type, StringToStringAttr(name));

                        return opb.create<circt::kanagawa::PortReadOp>(location, port);
                    };

                    const auto addOutputPort = [&](const std::string& name, const mlir::Value& value)
                    {
                        const mlir::Value port = opb.create<circt::kanagawa::OutputPortOp>(
                            location, key.GetFieldSymbol(name), value.getType(), StringToStringAttr(name));

                        opb.create<circt::kanagawa::PortWriteOp>(location, port, value);
                    };

                    const mlir::Value untypedClock = addInputPort("clk", GetI1Type());

                    const mlir::Value clk = opb.create<circt::seq::ToClockOp>(location, untypedClock);

                    const mlir::StringAttr symbol = key.GetFieldSymbol("value").getSymName();

                    // Instantiate register
                    if (key._initialValue.has_value())
                    {
                        const mlir::Value rst = addInputPort("rst", GetI1Type());

                        valueReg = opb.create<circt::seq::CompRegOp>(location, valueBackedge, clk, rst,
                                                                     LiteralToValue(*key._initialValue, opb, location),
                                                                     symbol);
                    }
                    else
                    {
                        valueReg = opb.create<circt::seq::CompRegOp>(location, valueBackedge, clk, symbol);
                    }

                    // Ensure a deterministic register name so that
                    // SystemVerilog testbenches can reach in and inspect values
                    valueReg.setName(StringToStringAttr("value"));

                    // Compute value for the next clock cycle;
                    mlir::Value valueNext = valueReg;

                    mlir::SmallVector<mlir::Value> writeEnables;

                    for (size_t j = 0; j < key._writeCount; ++j)
                    {
                        const mlir::Value inputValid = addInputPort("input_valid_" + std::to_string(j), GetI1Type());

                        writeEnables.push_back(inputValid);

                        mlir::Value inputData;

                        const auto it = key._literalValues.find(j);

                        if (key._literalValues.end() == it)
                        {
                            inputData = addInputPort("input_" + std::to_string(j), valueType);
                        }
                        else
                        {
                            inputData = LiteralToValue(it->second, opb, location);
                        }

                        valueNext =
                            opb.create<circt::comb::MuxOp>(location, inputValid, inputData, valueNext, TwoState);
                    }

                    valueBackedge.setValue(valueNext);

                    if (key._tracksRaces)
                    {
                        // Count the number of wren ports which are set
                        const mlir::Value sum =
                            PopCount(opb, location, writeEnables,
                                     opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(GetI1Type(), 0)));

                        // Compare against 1
                        const mlir::Value one =
                            opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(sum.getType(), 1));

                        const mlir::Value compareResult = opb.create<circt::comb::ICmpOp>(
                            location, circt::comb::ICmpPredicate::ugt, sum, one, TwoState);

                        addOutputPort("has_race_occured_out", compareResult);
                    }

                    if (key._hasNextLogic)
                    {
                        addOutputPort("value_next_out", valueNext);
                    }

                    addOutputPort("value_out", valueReg);

                    SafeInsert(nameToContainer, leafContainerName, container);
                }

                // Add an instance of the leaf container to the parent container
                const ObjectPath parentPath = SubPath(containerInstancePath, 0, containerInstancePath.size() - 1);

                circt::kanagawa::ContainerOp parentContainer = PathToContainer(parentPath);

                circt::OpBuilder::InsertionGuard g(opb);

                // Add an instance
                opb.setInsertionPointToEnd(parentContainer.getBodyBlock());

                const mlir::Location location = RegDescToLocation(regDesc);

                opb.create<circt::kanagawa::ContainerInstanceOp>(
                    location, circt::hw::InnerSymAttr::get(StringToStringAttr(containerInstancePath.back())),
                    circt::hw::InnerRefAttr::get(StringToStringAttr(GetCirctDesignName()), leafContainerNameAttr));

                // Write clock and reset ports
                const auto writeInputPort =
                    [&](const std::string& name, const mlir::Type type, const std::string& verbatimValue)
                {
                    circt::OpBuilder::InsertionGuard g(opb);

                    opb.setInsertionPointToEnd(_coreModule->Container().getBodyBlock());

                    // Add a port write to the core module container
                    const llvm::SmallVector<mlir::Value> substitutions; // Empty, there are no substitutions

                    const mlir::Value value = opb.create<circt::sv::VerbatimExprOp>(
                        location, type, StringToStringAttr(verbatimValue), substitutions, nullptr);

                    _coreModule->WritePort(opb, ObjectPath(), regDesc.Global()._containerInstancePath,
                                           key.GetFieldSymbol(name).getSymName(), leafContainerName,
                                           GetCirctDesignName(), value);
                };

                const auto readOutputPort = [&](const std::string& name, const mlir::Type type)
                {
                    circt::OpBuilder::InsertionGuard g(opb);

                    opb.setInsertionPointToEnd(_coreModule->Container().getBodyBlock());

                    return _coreModule->ReadPort(opb, ObjectPath(), regDesc.Global()._containerInstancePath,
                                                 key.GetFieldSymbol(name).getSymName(), leafContainerName,
                                                 GetCirctDesignName(), type);
                };

                // Connect clk and rst ports
                writeInputPort("clk", GetI1Type(), "clk");

                if (key._initialValue.has_value())
                {
                    writeInputPort("rst", GetI1Type(), GetResetReplica());
                }

                if (inspectableVariables.end() != inspectableVariables.find(i))
                {
                    // The variable is inspectable
                    // Declare a top-level struct with the value
                    _coreModule->AddVerbatimOp(
                        GetUnknownLocation(),
                        [&](VerbatimWriter& writer)
                        {
                            std::ostringstream str;
                            str << "struct packed {";
                            str << "logic " << OptionalWidthDeclaration(regDesc._width) << " value; ";
                            str << "} " << GetGlobalStructName(i) << ";";

                            writer << str.str();

                            writer << "assign " << GetGlobalStructName(i)
                                   << ".value = " << readOutputPort("value_out", GetIntegerType(regDesc._width)) << ";";
                        });
                }
            }
        }
    }

    mlir::Value InstantiateMemoriesCIRCT()
    {
        assert(_coreModule);

        circt::kanagawa::DesignOp designOp = GetDesignOp(_mlirModule);

        circt::OpBuilder opb(designOp.getBodyRegion());

        std::set<ObjectPath> paths;

        std::map<std::string, circt::kanagawa::ContainerOp> nameToContainer;

        // Determine which memories are inspectable
        std::set<size_t> inspectableMemories;

        if (GetCodeGenConfig()._inspection)
        {
            for (const InspectableVariable& inspectableVariable : _program._inspectableVariables)
            {
                for (const size_t registerIndex : inspectableVariable._registers)
                {
                    if (RegisterType::Memory == _program._registerTable[registerIndex]._type)
                    {
                        inspectableMemories.insert(registerIndex);
                    }
                }
            }
        }

        llvm::SmallVector<mlir::Value> memInitDoneTerms;

        for (size_t i = 0; i < _program._registerTable.size(); ++i)
        {
            const RegisterDescription& regDesc = _program._registerTable[i];

            if (regDesc._type == RegisterType::Memory)
            {
                const ObjectPath& containerInstancePath = regDesc.Memory()._containerInstancePath;

                // Container instance paths should be unique
                // beacuse each leaf node is a single scalar register
                assert(!containerInstancePath.empty());

                // Each path should be a path to a container
                // that only contains this variable
                // Otherwise port names between variables would conflict
                SafeInsert(paths, containerInstancePath);

                const RegisterDescription::MemoryDesc& memoryDesc = regDesc.Memory();

                // Each memory instance corresponds to a single container
                // "__mem_container" suffix added to avoid symbol name conflicts with
                // container instance
                const std::string leafContainerName = GetMemoryContainerName(regDesc._name);

                const mlir::StringAttr leafContainerNameAttr = StringToStringAttr(leafContainerName);

                // The container is not associated with any one location
                // Because many instances can be associated with it
                const mlir::Location location = RegDescToLocation(regDesc);

                // Create a container
                const mlir::Type valueType = GetIntegerType(regDesc._width);

                // The container has TopLevel = = false to prepend the design name to the container name
                // to avoid collisions between RTL modules generated by different export classes
                circt::kanagawa::ContainerOp container = opb.create<circt::kanagawa::ContainerOp>(
                    location, circt::hw::InnerSymAttr::get(leafContainerNameAttr), false);

                circt::OpBuilder::InsertionGuard g(opb);

                // Add ports to the container
                opb.setInsertionPointToEnd(container.getBodyBlock());

                const auto addInputPort = [&](const std::string& name, const mlir::Type& type)
                {
                    const mlir::Value port = opb.create<circt::kanagawa::InputPortOp>(
                        location, GetFullyQualifiedInnerSymAttr(containerInstancePath, name), type,
                        StringToStringAttr(name));

                    return opb.create<circt::kanagawa::PortReadOp>(location, port);
                };

                const auto addOutputPort = [&](const std::string& name, const mlir::Value& value)
                {
                    const mlir::Value outputPortOp = opb.create<circt::kanagawa::OutputPortOp>(
                        location, GetFullyQualifiedInnerSymAttr(containerInstancePath, name), value.getType(),
                        StringToStringAttr(name));

                    opb.create<circt::kanagawa::PortWriteOp>(location, outputPortOp, value);
                };

                const mlir::Value untypedClock = addInputPort("clk", GetI1Type());

                const mlir::Value clk = opb.create<circt::seq::ToClockOp>(location, untypedClock);

                const mlir::Value rst = addInputPort("rst", GetI1Type());

                SafeInsert(nameToContainer, leafContainerName, container);

                // Instaniatate memory instances/init/initLUT inside container
                const bool hasInitialData = !regDesc.Memory()._initialValues.empty();

                // Determine if the memory should be implemented in BRAM or LUT RAM
                const bool memoryInLutRam = ShouldUseRamType(regDesc, RAM_TYPE::LUT);

                const bool memoryInBRam = ShouldUseRamType(regDesc, RAM_TYPE::BLOCK);

                // Determine if the memory supports true dual-port
                const bool memoryTrueDualPort = GetCodeGenDeviceConfig()._supportsTrueDualPortRam;

                const bool isInspectable = (inspectableMemories.end() != inspectableMemories.find(i));

                const bool isQuadPort = regDesc.Memory()._quadPort;

                const bool useLogicRam = regDesc.Memory()._useLogicRam;

                const size_t addrWidth = regDesc.GetMemoryAddressWidth();

                const size_t dataWidth = regDesc.Memory()._elementWidth;

                const size_t writePorts = regDesc.Memory()._writePortCount;

                const size_t readPorts = regDesc.Memory()._readPortCount;

                const bool writeArbitration = (writePorts > 1) && !isQuadPort;

                const bool ecc = regDesc.Memory()._ecc;

                // The reset pins of the resetable instances inside container hook up to container's "rst" port.
                // The container's reset port hooks up with the generated reset replica or is tied down to zero.
                const std::string resetContainer = MemoryRequiresReset(regDesc) ? GetResetReplica() : "1'b0";

                // If there are no write ports and the memory is not placed in lut ram
                // Then each RAM instance can serve 2 reads
                // The !isInspectable term is there just to simplify this code (inspecting a read-only memory is
                // strange)
                size_t readPortsPerModule = regDesc.Memory()._replicate && (0 == writePorts) && !memoryInLutRam &&
                                                    !isInspectable && memoryTrueDualPort
                                                ? 2
                                                : 1;

                if (isQuadPort)
                {
                    readPortsPerModule = 2;
                }
                else
                {
                    // Don't allow dual read ports if read port latencies don't match
                    for (size_t i = 0; i < readPorts; i++)
                    {
                        if (regDesc.Memory().GetReadLatency(0) != regDesc.Memory().GetReadLatency(i))
                        {
                            readPortsPerModule = 1;
                        }
                    }
                }

                if (useLogicRam)
                {
                    readPortsPerModule = readPorts == 0 ? 1 : readPorts;
                }

                size_t numModules = useLogicRam ? 1
                                    : regDesc.Memory()._replicate
                                        ? Align(readPorts, readPortsPerModule) / readPortsPerModule
                                        : 1;

                if (isInspectable)
                {
                    // If a memory is inspectable, but has no reads in the program
                    // ensure that a module is still instantiated
                    numModules = std::max<size_t>(numModules, 1);
                }

                if (isQuadPort)
                {
                    // quad port memories do not support more access locations
                    // than ports
                    // (to avoid having to dynamically assign reads/writes to multiple available ports)
                    assert(writePorts <= 2);
                    assert(readPorts <= 2);

                    // Quad port memories do not support implicit replicas
                    assert(numModules == 1);
                }

                std::string memContentFileName;
                std::ostringstream memInitStructName;

                mlir::Value memInitLutAddr;
                mlir::Value memInitWrAddr;
                mlir::Value memInitWrData;
                mlir::Value memInitWrEn;
                mlir::Value memInitDone =
                    opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 1));
                if (hasInitialData)
                {
                    // Initial value of the memory was specified
                    // Generate a data file with the contents
                    memContentFileName = WriteMemoryContentFile(i);
                    // instantiate the LUT for initializing ASIC memory
                    // for ASIC memory with multiple replicas,share the intializer and LUT
                    if (GetCodeGenDeviceConfig()._memoryInitFileType == MemoryInitFileType::Asic)
                    {
                        mlir::Value memoryInitStart = addInputPort("memory_init_start_in", GetI1Type());
                        // Declare per-replica variables for ASIC memory init signals
                        memInitStructName << "init_data_" << i;
                        std::string lutModuleName = memContentFileName.erase(memContentFileName.length() - 3);
                        // LUT controller
                        ModuleInstanceHelper instance1(*this, RegDescToLocation(regDesc));
                        instance1.SetModuleName("KanagawaMemInitCtrl");
                        instance1.AddU64Parameter("ADDR_WIDTH", addrWidth);
                        instance1.AddU64Parameter("LUT_LATENCY", 1);
                        instance1.SetInstanceName("ctrl_" + lutModuleName);
                        instance1.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
                        instance1.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), rst);
                        instance1.AddPort("init_start_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                          memoryInitStart);
                        instance1.AddPort("init_addr_out", circt::hw::ModulePort::Direction::Output,
                                          instance1.GetParameterizedIntegerType("ADDR_WIDTH"));
                        instance1.AddPort("init_we_out", circt::hw::ModulePort::Direction::Output, GetI1Type());
                        instance1.AddPort("init_done_out", circt::hw::ModulePort::Direction::Output, GetI1Type());
                        instance1.AddPort("lut_addr_out", circt::hw::ModulePort::Direction::Output,
                                          instance1.GetParameterizedIntegerType("ADDR_WIDTH"));
                        instance1.Generate(&opb);
                        memInitLutAddr = instance1.GetPortValue("lut_addr_out");
                        memInitWrAddr = instance1.GetPortValue("init_addr_out");
                        memInitWrEn = instance1.GetPortValue("init_we_out");
                        memInitDone = instance1.GetPortValue("init_done_out");
                        // LUT
                        ModuleInstanceHelper instance0(*this, RegDescToLocation(regDesc));
                        instance0.SetModuleName("MemInitLUT_" + lutModuleName);
                        instance0.AddU64Parameter("ADDR_WIDTH", addrWidth);
                        instance0.AddU64Parameter("DATA_WIDTH", dataWidth);
                        instance0.SetInstanceName(lutModuleName + "_" + std::to_string(i));
                        instance0.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
                        instance0.AddPort("addr_in", circt::hw::ModulePort::Direction::Input,
                                          instance0.GetParameterizedIntegerType("ADDR_WIDTH"), memInitLutAddr);
                        instance0.AddPort("data_out", circt::hw::ModulePort::Direction::Output,
                                          instance0.GetParameterizedIntegerType("DATA_WIDTH"));
                        instance0.Generate(&opb);
                        memInitWrData = instance0.GetPortValue("data_out");
                    }
                }

                const size_t writeIndexWidth = (writePorts > 1) ? Log2RoundUp(writePorts) : 1;
                const size_t readIndexWidth = (readPorts > 1) ? Log2RoundUp(readPorts) : 1;

                // Aribtration (choosing a winner) occurs for memories which the user asks to not replicate
                // or for inspectable memories (which sneak in to read values on idle cycles)
                const bool needsArbitration =
                    (isInspectable || !regDesc.Memory()._replicate) && !isQuadPort;
                // Number of arbitrations needed
                const size_t numArbitrations = 1;

                // Declare variables that connect I/O ports of memory
                // Don't declare arrays of size 0 for the case of 0 write ports
                const size_t writePortsForStruct = writePorts;
                const size_t readPortsForStruct = readPorts;

                // Memory container's non-array typed port since each basic block may access part of that array
                // Use push_back method to initialize/update the vectors
                mlir::SmallVector<mlir::Value> memoryDataWrenPorts;
                mlir::SmallVector<mlir::Value> memoryDataWriteDataPorts;
                mlir::SmallVector<mlir::Value> memoryDataWriteAddrPorts;
                mlir::SmallVector<mlir::Value> memoryDataReadAddrPorts;
                mlir::SmallVector<mlir::Value> memoryDataRdenPorts;
                mlir::SmallVector<mlir::Value> memoryDataReadDataArray;

                for (size_t port = 0; port < readPortsForStruct; port++)
                {
                    // KanagawaLogicRam doesn't have rden ports,so its container shouldn't have them either.
                    if (!useLogicRam)
                    {
                        memoryDataRdenPorts.push_back(addInputPort("rden_in_" + std::to_string(port), GetI1Type()));
                    }
                    memoryDataReadAddrPorts.push_back(
                        addInputPort("read_addr_in_" + std::to_string(port), GetIntegerType(addrWidth)));
                }

                // ROM container doesn't have explicit input ports for write and will internally hook the signals to 0
                for (size_t port = 0; port < writePortsForStruct; port++)
                {
                    memoryDataWrenPorts.push_back(addInputPort("wren_in_" + std::to_string(port), GetI1Type()));
                    memoryDataWriteAddrPorts.push_back(
                        addInputPort("write_addr_in_" + std::to_string(port), GetIntegerType(addrWidth)));
                    memoryDataWriteDataPorts.push_back(
                        addInputPort("write_data_in_" + std::to_string(port), GetIntegerType(dataWidth)));
                }

                mlir::Value memoryDataSelectedWriteIndex;
                mlir::SmallVector<mlir::Value> memoryDataFinalReadAddr(numArbitrations);
                mlir::SmallVector<mlir::Value> memoryDataFinalRden(numArbitrations);
                mlir::Value memoryDataInspectionAddr;
                mlir::Value memoryDataInspectionSuccess;
                mlir::Value memoryDataEccStatus;
                mlir::Value memoryDataHasRaceOccured =
                    opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 0));
                mlir::Value eccPortValue;

                if (isInspectable)
                {
                    memoryDataInspectionAddr = addInputPort("inspection_addr_in", GetIntegerType(addrWidth));
                }

                if ((writePorts == 0) && memContentFileName.empty())
                {
                    // no KanagawaSyncRam will be instantiated, just set the read data to a dummy value
                    for (size_t port = 0; port < readPorts; ++port)
                    {
                        memoryDataReadDataArray.push_back(opb.create<circt::hw::ConstantOp>(
                            location, opb.getIntegerAttr(opb.getIntegerType(dataWidth), 0)));
                    }

                    // Also if no memory, set the inspectable inspection_success flag so the inspectable chain doesn't
                    // lock up awaiting response from a non-existent memory
                    if (isInspectable)
                    {
                        memoryDataInspectionSuccess =
                            opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 1));
                    }

                    // The non-CIRCT version uses continue to skip this loop iteration.CIRCT version still needs to
                    // create a container although there is no KanagawaSyncRam instantiation.
                    numModules = 0;
                }

                // Determine the maximum depth of any physical memory that
                // is used to implement the the logical memory.
                // This reduces bram usage in these cases:
                // 1.  On Intel FPGAs Arria10 (and before), this enables narrow
                //     configurations like 2x8192
                // 2.  On Intel FPGAs, this enables memories with a
                //     depth that is not a power of 2, to use the minimum number
                //     of physical memories.  For example, depth = 1536 will be
                //     3 memories of depth 512, rather than 4 memories of depth 2048
                const size_t maximumDepth = GetMemoryAtomDepth(_program, i);

                // Choose the write port that wins on each cycle
                if (writeArbitration)
                {
                    // A priority mux tree with writePorts levels with higher priority on larger index values
                    mlir::SmallVector<mlir::Value> muxTree(writePorts);
                    for (size_t i = 0; i < writePorts; i++)
                    {
                        const mlir::Value iValue = opb.create<circt::hw::ConstantOp>(
                            location, opb.getIntegerAttr(opb.getIntegerType(writeIndexWidth), i));

                        if (i == 0)
                        {
                            muxTree[i] = iValue;
                        }
                        else
                        {
                            muxTree[i] = opb.create<circt::comb::MuxOp>(location, memoryDataWrenPorts[i], iValue,
                                                                        muxTree[i - 1], TwoState);
                        }
                    }
                    memoryDataSelectedWriteIndex = muxTree[writePorts - 1];
                }

                if (needsArbitration)
                {
                    for (size_t j = 0; j < numArbitrations; j++)
                    {
                        // Determine ports to arbitrate over
                        // Default is to arbitrate across all read ports
                        size_t boundStart = 0;
                        size_t boundEnd = readPorts;
                        if (regDesc.Memory()._replicate)
                        {
                            // for replicated memories, only replica 0 needs arbitration
                            boundEnd = std::min<size_t>(1, readPorts);
                        }
                        assert(boundEnd >= boundStart);
                        assert(boundEnd <= readPorts);

                        // Choose the read port that wins on each cycle
                        llvm::SmallVector<mlir::Value> rdenArray;
                        {
                            memoryDataInspectionSuccess =
                                opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 1));

                            if (isInspectable)
                            {
                                // If all read-enables are low, then read based on the inspection address
                                memoryDataFinalReadAddr[j] = memoryDataInspectionAddr;
                            }
                            else
                            {
                                memoryDataFinalReadAddr[j] = opb.create<circt::hw::ConstantOp>(
                                    location, opb.getIntegerAttr(opb.getIntegerType(addrWidth), 0));
                            }

                            for (size_t i = boundStart; i < boundEnd; i++)
                            {
                                memoryDataFinalReadAddr[j] = opb.create<circt::comb::MuxOp>(
                                    location, memoryDataRdenPorts[i], memoryDataReadAddrPorts[i],
                                    memoryDataFinalReadAddr[j], TwoState);

                                // memoryDataInspectionSuccess is 0 if any rden is 1
                                memoryDataInspectionSuccess = opb.create<circt::comb::MuxOp>(
                                    location, memoryDataRdenPorts[i],
                                    opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 0)),
                                    memoryDataInspectionSuccess, TwoState);
                            }

                            if (isInspectable)
                            {
                                // If inspectable, the memory is always readable, either by arbitration or inspection
                                memoryDataFinalRden[0] =
                                    opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 1));
                            }
                            else
                            {
                                for (size_t i = boundStart; i < boundEnd; i++)
                                {
                                    rdenArray.push_back(memoryDataRdenPorts[i]);
                                }

                                memoryDataFinalRden[j] = Reduce<circt::comb::OrOp>(
                                    rdenArray, opb.getIntegerAttr(opb.getI1Type(), 0), opb, location);
                            }
                        }
                    }
                }

                // Logic to trace when data races occur
                if (RegisterTracksRaces(regDesc) && GetCodeGenConfig()._detectRaces)
                {
                    const struct
                    {
                        bool _enable;
                        std::string _name;
                        size_t _numPorts;
                    } portDescriptions[] = {
                        {(writePorts > 1), "wren", writePorts},
                        {(readPorts > 1) && !regDesc.Memory()._replicate, "rden", readPorts},
                    };

                    llvm::SmallVector<mlir::Value> writeEnables;
                    llvm::SmallVector<mlir::Value> readEnables;
                    for (const auto& portDesc : portDescriptions)
                    {
                        if (!portDesc._enable)
                        {
                            continue;
                        }

                        for (size_t portIndex = 0; portIndex < portDesc._numPorts; portIndex++)
                        {
                            if (portDesc._name == "wren")
                            {
                                writeEnables.push_back(memoryDataWrenPorts[portIndex]);
                            }
                            else if (portDesc._name == "rden")
                            {
                                readEnables.push_back(memoryDataRdenPorts[portIndex]);
                            }
                            else
                            {
                                assert(false);
                            }
                        }
                    }
                    // Count the number of wren ports which are set
                    const mlir::Value sumWren =
                        PopCount(opb, location, writeEnables,
                                 opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(GetI1Type(), 0)));

                    // Count the number of rden ports which are set
                    const mlir::Value sumRden =
                        PopCount(opb, location, readEnables,
                                 opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(GetI1Type(), 0)));

                    // Compare against 1
                    const mlir::Value oneWren =
                        opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(sumWren.getType(), 1));

                    const mlir::Value compareWren = opb.create<circt::comb::ICmpOp>(
                        location, circt::comb::ICmpPredicate::ugt, sumWren, oneWren, TwoState);

                    // Compare against 1
                    const mlir::Value oneRden =
                        opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(sumRden.getType(), 1));

                    const mlir::Value compareRden = opb.create<circt::comb::ICmpOp>(
                        location, circt::comb::ICmpPredicate::ugt, sumRden, oneRden, TwoState);
                    // Set race flag when sumWren > 1 or sumRden > 1
                    memoryDataHasRaceOccured =
                        opb.create<circt::comb::OrOp>(location, compareWren, compareRden, TwoState);
                }

                for (size_t moduleIndex = 0; moduleIndex < numModules; ++moduleIndex)
                {
                    const size_t basePort = moduleIndex * readPortsPerModule;

                    size_t endPort =
                        std::min(basePort + readPortsPerModule, readPorts);

                    if (endPort == basePort)
                    {
                        // Handle memories with 0 read ports (only inspection)
                        assert(0 == readPorts);
                        endPort++;
                    }

                    const size_t portsForThisModule = endPort - basePort;
                    assert(portsForThisModule >= 1);

                    const size_t readLatency = regDesc.Memory().GetReadLatency(basePort);

                    // Read latency must match for all replicas
                    if (!isQuadPort)
                    {
                        for (size_t offset = 0; offset < portsForThisModule; offset++)
                        {
                            assert(readLatency == regDesc.Memory().GetReadLatency(basePort + offset));
                        }
                    }

                    mlir::Value lramDataModuleReadAddr;
                    mlir::Value lramDataModuleWriteAddr;
                    mlir::Value lramDataModuleWren;
                    mlir::Value lramDataModuleWriteData;
                    mlir::Value lramDataModuleReadData;
                    mlir::Value memoryDataModuleReadAddr;
                    mlir::Value memoryDataModuleWriteAddr;
                    mlir::Value memoryDataModuleDualPortAddr;
                    mlir::Value memoryDataModuleDualPortRden;
                    mlir::Value memoryDataModuleDualPortWren;
                    mlir::Value memoryDataModuleDualPortDataIn;
                    mlir::Value memoryDataModuleDualPortDataOut;

                    const size_t basePortWidth = (basePort > 1) ? Log2RoundUp(basePort) : 1;
                    const mlir::Value basePortValue = opb.create<circt::hw::ConstantOp>(
                        location, opb.getIntegerAttr(opb.getIntegerType(basePortWidth), basePort));

                    if (useLogicRam)
                    {
                        assert(!regDesc.Memory().HasReadArbitration());
                        // Read ports
                        llvm::SmallVector<mlir::Value> lramDataModuleReadAddrArray;
                        for (size_t i = 0; i < portsForThisModule; i++)
                        {
                            lramDataModuleReadAddrArray.push_back(
                                GetTypedZeros(opb, location, GetIntegerType(addrWidth)));
                        }

                        for (size_t portOffset = 0; portOffset < readPorts; portOffset++)
                        {
                            lramDataModuleReadAddrArray[portOffset] = memoryDataReadAddrPorts[portOffset];
                        }
                        std::reverse(lramDataModuleReadAddrArray.begin(), lramDataModuleReadAddrArray.end());
                        lramDataModuleReadAddr =
                            opb.create<circt::hw::ArrayCreateOp>(location, lramDataModuleReadAddrArray);

                        // Write port
                        lramDataModuleWren = memoryDataWrenPorts[0];
                        lramDataModuleWriteAddr = memoryDataWriteAddrPorts[0];
                        lramDataModuleWriteData = memoryDataWriteDataPorts[0];
                    }
                    else if (isQuadPort)
                    {
                        llvm::SmallVector<mlir::Value> memoryDataModuleDualPortRdenArray(2);
                        llvm::SmallVector<mlir::Value> memoryDataModuleReadAddrArray(2);
                        llvm::SmallVector<mlir::Value> memoryDataModuleDualPortWrenArray(2);
                        llvm::SmallVector<mlir::Value> memoryDataModuleWriteAddrArray(2);
                        llvm::SmallVector<mlir::Value> memoryDataModuleDualPortDataInArray(2);
                        for (size_t port = 0; port < 2; port++)
                        {
                            memoryDataModuleDualPortRdenArray[port] = GetTypedZeros(opb, location, GetI1Type());
                            memoryDataModuleReadAddrArray[port] =
                                GetTypedZeros(opb, location, GetIntegerType(addrWidth));
                            memoryDataModuleDualPortWrenArray[port] = GetTypedZeros(opb, location, GetI1Type());
                            memoryDataModuleWriteAddrArray[port] =
                                GetTypedZeros(opb, location, GetIntegerType(addrWidth));
                            memoryDataModuleDualPortDataInArray[port] =
                                GetTypedZeros(opb, location, GetIntegerType(dataWidth));
                        }
                        for (size_t port = 0; port < 2; port++)
                        {
                            const mlir::Value portValue =
                                opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), port));
                            if (port < readPorts)
                            {
                                memoryDataModuleDualPortRdenArray[port] = memoryDataRdenPorts[port];
                                memoryDataModuleReadAddrArray[port] = memoryDataReadAddrPorts[port];
                            }
                            else
                            {
                                memoryDataModuleDualPortRdenArray[port] =
                                    opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 0));
                                memoryDataModuleReadAddrArray[port] = opb.create<circt::hw::ConstantOp>(
                                    location, opb.getIntegerAttr(opb.getIntegerType(addrWidth), 0));
                            }

                            if (port < writePorts)
                            {
                                memoryDataModuleWriteAddrArray[port] = memoryDataWriteAddrPorts[port];
                                memoryDataModuleDualPortWrenArray[port] = memoryDataWrenPorts[port];
                                memoryDataModuleDualPortDataInArray[port] = memoryDataWriteDataPorts[port];
                            }
                            else
                            {
                                memoryDataModuleWriteAddrArray[port] = opb.create<circt::hw::ConstantOp>(
                                    location, opb.getIntegerAttr(opb.getIntegerType(addrWidth), 0));
                                memoryDataModuleDualPortWrenArray[port] =
                                    opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 0));
                                memoryDataModuleDualPortDataInArray[port] = opb.create<circt::hw::ConstantOp>(
                                    location, opb.getIntegerAttr(opb.getIntegerType(dataWidth), 0));
                            }
                        }
                        std::reverse(memoryDataModuleWriteAddrArray.begin(), memoryDataModuleWriteAddrArray.end());
                        std::reverse(memoryDataModuleDualPortWrenArray.begin(),
                                     memoryDataModuleDualPortWrenArray.end());
                        std::reverse(memoryDataModuleDualPortDataInArray.begin(),
                                     memoryDataModuleDualPortDataInArray.end());
                        std::reverse(memoryDataModuleDualPortRdenArray.begin(),
                                     memoryDataModuleDualPortRdenArray.end());
                        std::reverse(memoryDataModuleReadAddrArray.begin(), memoryDataModuleReadAddrArray.end());
                        memoryDataModuleWriteAddr =
                            opb.create<circt::hw::ArrayCreateOp>(location, memoryDataModuleWriteAddrArray);
                        memoryDataModuleDualPortWren =
                            opb.create<circt::hw::ArrayCreateOp>(location, memoryDataModuleDualPortWrenArray);
                        memoryDataModuleDualPortDataIn =
                            opb.create<circt::hw::ArrayCreateOp>(location, memoryDataModuleDualPortDataInArray);
                        memoryDataModuleDualPortRden =
                            opb.create<circt::hw::ArrayCreateOp>(location, memoryDataModuleDualPortRdenArray);
                        memoryDataModuleReadAddr =
                            opb.create<circt::hw::ArrayCreateOp>(location, memoryDataModuleReadAddrArray);
                    }
                    else
                    {
                        llvm::SmallVector<mlir::Value> memoryDataModuleDualPortRdenArray(2);
                        llvm::SmallVector<mlir::Value> memoryDataModuleDualPortAddrArray(2);
                        llvm::SmallVector<mlir::Value> memoryDataModuleDualPortWrenArray(2);
                        llvm::SmallVector<mlir::Value> memoryDataModuleDualPortDataInArray(2);
                        for (size_t i = 0; i < 2; i++)
                        {
                            memoryDataModuleDualPortRdenArray[i] = GetTypedZeros(opb, location, GetI1Type());
                            memoryDataModuleDualPortAddrArray[i] =
                                GetTypedZeros(opb, location, GetIntegerType(addrWidth));
                            memoryDataModuleDualPortWrenArray[i] = GetTypedZeros(opb, location, GetI1Type());
                            memoryDataModuleDualPortDataInArray[i] =
                                GetTypedZeros(opb, location, GetIntegerType(dataWidth));
                        }
                        const size_t portsForThisModuleWidth =
                            (portsForThisModule > 1) ? Log2RoundUp(portsForThisModule) : 1;
                        for (size_t portOffset = 0; portOffset < portsForThisModule; portOffset++)
                        {
                            // Port on the individual replica
                            // For simple dual port memories, portOffset is always 0
                            // and whichInnerPort = 1 (always read on port 1)
                            // For true dual port memories, whichInnerPort = { 1, 0 }, and portOffset = { 0, 1 } (just
                            // to keep this expression simple, either of the dual ports on the memory would work)
                            const size_t whichInnerPort = (1 - portOffset);

                            // Port on the logical memory
                            const size_t whichOuterPort = basePort + portOffset;

                            const mlir::Value whichInnerPortValue = opb.create<circt::hw::ConstantOp>(
                                location,
                                opb.getIntegerAttr(opb.getIntegerType(portsForThisModuleWidth), whichInnerPort));
                            const mlir::Value whichOuterPortValue = opb.create<circt::hw::ConstantOp>(
                                location,
                                opb.getIntegerAttr(opb.getIntegerType(portsForThisModuleWidth), whichOuterPort));
                            // for replicated memories (replica 0), use final_read_addr to enable inspection to sneak in
                            // on idle cycles
                            if (regDesc.Memory()._replicate && !(isInspectable && (moduleIndex == 0)))
                            {
                                memoryDataModuleDualPortRdenArray[whichInnerPort] = memoryDataRdenPorts[whichOuterPort];
                                memoryDataModuleDualPortAddrArray[whichInnerPort] =
                                    memoryDataReadAddrPorts[whichOuterPort];
                            }
                            else
                            {
                                assert(portsForThisModule == 1);

                                memoryDataModuleDualPortRdenArray[1] = memoryDataFinalRden[0];
                                memoryDataModuleDualPortAddrArray[1] = memoryDataFinalReadAddr[0];
                            }
                        }
                        // never write on port 1
                        memoryDataModuleDualPortWrenArray[1] =
                            opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 0));
                        memoryDataModuleDualPortDataInArray[1] = opb.create<circt::hw::ConstantOp>(
                            location, opb.getIntegerAttr(opb.getIntegerType(dataWidth), 0));

                        if (writePorts > 0)
                        {
                            memoryDataModuleDualPortRdenArray[0] =
                                opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 0));

                            mlir::Value writeDataStr;
                            mlir::Value writeAddrStr;
                            if (writeArbitration)
                            {
                                // Since memoryDataSelectedWriteIndex is mlir::Value, we have to convert the mlir vector
                                // into a mlir::Value and use hw::ArrayGetOp to extract the selected_write_index field
                                std::reverse(memoryDataWriteDataPorts.begin(), memoryDataWriteDataPorts.end());
                                std::reverse(memoryDataWriteAddrPorts.begin(), memoryDataWriteAddrPorts.end());
                                const mlir::Value memoryDataWriteData =
                                    opb.create<circt::hw::ArrayCreateOp>(location, memoryDataWriteDataPorts);
                                const mlir::Value memoryDataWriteAddr =
                                    opb.create<circt::hw::ArrayCreateOp>(location, memoryDataWriteAddrPorts);
                                std::reverse(memoryDataWriteDataPorts.begin(), memoryDataWriteDataPorts.end());
                                std::reverse(memoryDataWriteAddrPorts.begin(), memoryDataWriteAddrPorts.end());
                                writeDataStr = opb.create<circt::hw::ArrayGetOp>(location, memoryDataWriteData,
                                                                                 memoryDataSelectedWriteIndex);
                                writeAddrStr = opb.create<circt::hw::ArrayGetOp>(location, memoryDataWriteAddr,
                                                                                 memoryDataSelectedWriteIndex);
                            }
                            else
                            {
                                writeDataStr = memoryDataWriteDataPorts[0];
                                writeAddrStr = memoryDataWriteAddrPorts[0];
                            }

                            if (hasInitialData &&
                                GetCodeGenDeviceConfig()._memoryInitFileType == MemoryInitFileType::Asic)
                            {
                                mlir::Value muxValue;
                                muxValue = opb.create<circt::comb::MuxOp>(location, memInitWrEn, memInitWrData,
                                                                          writeDataStr, TwoState);

                                memoryDataModuleDualPortDataInArray[0] = muxValue;

                                muxValue = opb.create<circt::comb::MuxOp>(location, memInitWrEn, memInitWrAddr,
                                                                          writeAddrStr, TwoState);

                                memoryDataModuleDualPortAddrArray[0] = muxValue;
                            }
                            else
                            {
                                memoryDataModuleDualPortDataInArray[0] = writeDataStr;
                                memoryDataModuleDualPortAddrArray[0] = writeAddrStr;
                            }

                            llvm::SmallVector<mlir::Value> wrenArray;
                            for (size_t writePort = 0; writePort < writePorts; writePort++)
                            {
                                const mlir::Value selectedWren = memoryDataWrenPorts[writePort];
                                wrenArray.push_back(selectedWren);

                                if (hasInitialData &&
                                    GetCodeGenDeviceConfig()._memoryInitFileType == MemoryInitFileType::Asic)
                                {
                                    wrenArray.push_back(memInitWrEn);
                                }
                            }
                            const mlir::Value reduceWren = opb.create<circt::comb::OrOp>(location, wrenArray, TwoState);

                            memoryDataModuleDualPortWrenArray[0] = reduceWren;
                        }
                        else
                        {
                            // Read-only memory (with initial value)//ROM doesn't need to be updated.
                            if (portsForThisModule == 1)
                            {
                                // don't read on port 0 (this memory is not in true dual port mode)
                                memoryDataModuleDualPortRdenArray[0] =
                                    opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 0));

                                if (hasInitialData &&
                                    GetCodeGenDeviceConfig()._memoryInitFileType == MemoryInitFileType::Asic)
                                {
                                    memoryDataModuleDualPortAddrArray[0] = memInitWrAddr;
                                }
                                else
                                {
                                    memoryDataModuleDualPortAddrArray[0] = opb.create<circt::hw::ConstantOp>(
                                        location, opb.getIntegerAttr(opb.getIntegerType(addrWidth), 0));
                                }
                            }

                            if (hasInitialData &&
                                GetCodeGenDeviceConfig()._memoryInitFileType == MemoryInitFileType::Asic)
                            {
                                // only write during initialization
                                memoryDataModuleDualPortDataInArray[0] = memInitWrData;
                                memoryDataModuleDualPortWrenArray[0] = memInitWrEn;
                            }
                            else
                            {
                                // never write
                                memoryDataModuleDualPortDataInArray[0] = opb.create<circt::hw::ConstantOp>(
                                    location, opb.getIntegerAttr(opb.getIntegerType(dataWidth), 0));
                                memoryDataModuleDualPortWrenArray[0] =
                                    opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), 0));
                            }
                        }
                        std::reverse(memoryDataModuleDualPortRdenArray.begin(),
                                     memoryDataModuleDualPortRdenArray.end());
                        std::reverse(memoryDataModuleDualPortAddrArray.begin(),
                                     memoryDataModuleDualPortAddrArray.end());
                        std::reverse(memoryDataModuleDualPortDataInArray.begin(),
                                     memoryDataModuleDualPortDataInArray.end());
                        std::reverse(memoryDataModuleDualPortWrenArray.begin(),
                                     memoryDataModuleDualPortWrenArray.end());
                        memoryDataModuleDualPortRden =
                            opb.create<circt::hw::ArrayCreateOp>(location, memoryDataModuleDualPortRdenArray);
                        memoryDataModuleDualPortAddr =
                            opb.create<circt::hw::ArrayCreateOp>(location, memoryDataModuleDualPortAddrArray);
                        memoryDataModuleDualPortDataIn =
                            opb.create<circt::hw::ArrayCreateOp>(location, memoryDataModuleDualPortDataInArray);
                        memoryDataModuleDualPortWren =
                            opb.create<circt::hw::ArrayCreateOp>(location, memoryDataModuleDualPortWrenArray);
                    }

                    ModuleInstanceHelper instance(*this, RegDescToLocation(regDesc));

                    instance.AddU64Parameter("DATA_WIDTH", dataWidth);
                    instance.AddU64Parameter("ADDR_WIDTH", addrWidth);
                    instance.AddU64Parameter("DEPTH", regDesc.Memory()._elementCount);

                    mlir::Type arrayAddrType =
                        GetPackedArrayType(instance.GetParameterizedIntegerType("ADDR_WIDTH"), 2);
                    mlir::Type arrayDataType =
                        GetPackedArrayType(instance.GetParameterizedIntegerType("DATA_WIDTH"), 2);
                    mlir::Type arrayWrRdEnType = GetPackedArrayType(GetI1Type(), 2);
                    const size_t clog2NumReadPorts = (portsForThisModule > 1) ? Log2RoundUp(portsForThisModule) : 1;

                    if (useLogicRam)
                    {
                        instance.SetModuleName("KanagawaLogicRam");
                        instance.AddU64Parameter("NUM_READ_PORTS", readPortsPerModule);
                        mlir::Type lramAddrType = GetPackedArrayTypeParameterizedSize(
                            instance.GetParameterizedIntegerType("ADDR_WIDTH"), "NUM_READ_PORTS");
                        mlir::Type lramDataType = GetPackedArrayTypeParameterizedSize(
                            instance.GetParameterizedIntegerType("DATA_WIDTH"), "NUM_READ_PORTS");
                        instance.SetInstanceName(ValidateUniqueName(UniqueRegisterName(i)));

                        instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), clk);
                        instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), rst);
                        instance.AddPort("rdaddr_in", circt::hw::ModulePort::Direction::Input, lramAddrType,
                                         lramDataModuleReadAddr);
                        instance.AddPort("rddata_out", circt::hw::ModulePort::Direction::Output, lramDataType);
                        instance.AddPort("wren_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                         lramDataModuleWren);
                        instance.AddPort("wraddr_in", circt::hw::ModulePort::Direction::Input,
                                         instance.GetParameterizedIntegerType("ADDR_WIDTH"), lramDataModuleWriteAddr);
                        instance.AddPort("wrdata_in", circt::hw::ModulePort::Direction::Input,
                                         instance.GetParameterizedIntegerType("DATA_WIDTH"), lramDataModuleWriteData);
                    }
                    else
                    {
                        if (isQuadPort)
                        {
                            instance.SetModuleName("KanagawaHalSimpleQuadPortMemory");
                        }
                        else
                        {
                            instance.SetModuleName("KanagawaSyncRam");
                        }

                        instance.AddU64Parameter("MAX_DEPTH", maximumDepth);

                        if (isQuadPort)
                        {
                            // simple quad port memories are required to be in bram
                            assert(memoryInBRam);

                            for (size_t i = 0; i < 2; i++)
                            {
                                const size_t readLatency = regDesc.Memory().GetReadLatency(i);
                                assert(readLatency >= 1);
                                assert(readLatency <= 2);

                                instance.AddU64Parameter("READ_LATENCY_" + std::to_string(i), readLatency);

                                const BypassAndWriteDelay bawd = CalculateMemoryBypassAndWriteDelay(
                                    regDesc.Memory().IsBypassReadPort(i), readLatency, regDesc);

                                instance.AddU64Parameter("WRITE_DELAY_" + std::to_string(i), bawd._writeDelay);

                                // Not supported with quad-port
                                assert(!bawd._hardenedBypass);
                            }
                        }
                        else
                        {
                            instance.AddBoolParameter("USE_LUTRAM", memoryInLutRam);
                            instance.AddBoolParameter("USE_BRAM", memoryInBRam);

                            assert(readLatency >= 1);
                            assert(readLatency <= 2);
                            instance.AddU64Parameter("USE_OUTPUT_REG", readLatency - 1);

                            bool isBypassReadPort = false;

                            // for non-replicated memory, all bits will be set if any port requires bypass
                            isBypassReadPort = regDesc.Memory().IsBypassReadPort(basePort);

                            const BypassAndWriteDelay bawd =
                                CalculateMemoryBypassAndWriteDelay(isBypassReadPort, readLatency, regDesc);

                            if (portsForThisModule != 1)
                            {
                                // True dual-port mode does not support write-delay nor bypass
                                assert(bawd._writeDelay == 0);
                                assert(bawd._bypassSlots == 0);
                            }

                            instance.AddU64Parameter("WRITE_DELAY", bawd._writeDelay);
                            instance.AddBoolParameter("USE_HARDENED_BYPASS", bawd._hardenedBypass);
                            instance.AddBoolParameter("TRUE_DUAL_PORT", portsForThisModule == 2);

                            // Only require strictly-defined behavior (old_data)
                            // if requires on the command line
                            // If a memory has no write locations,
                            // then concurrent read/write to the same address is impossible
                            // If hardened bypass logic is enabled, then the bypass HW will ensure that read will see
                            // the new data
                            const bool wellDefinedRwCollisions = GetCodeGenConfig()._strictMemoryReadWrite &&
                                                                 (writePorts > 0) && !(bawd._hardenedBypass);

                            instance.AddBoolParameter("SUPPORTS_RW_COLLISIONS", wellDefinedRwCollisions);
                            instance.AddBoolParameter("ECC", ecc);
                        }

                        if (GetCodeGenDeviceConfig()._memoryInitFileType != MemoryInitFileType::Asic)
                        {
                            memContentFileName = memContentFileName.empty() ? "UNUSED" : memContentFileName;

                            instance.AddStringParameter("INITIAL_DATA_FILE", memContentFileName);
                        }

                        instance.AddStringParameter("DEVICE_FAMILY", GetCodeGenDeviceConfig()._halDeviceFamily);

                        instance.SetInstanceName(
                            ValidateUniqueName(UniqueRegisterName(i) + "_" + std::to_string(moduleIndex)));

                        instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
                        instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), rst);

                        if (isQuadPort)
                        {
                            instance.AddPort("read_addr_in", circt::hw::ModulePort::Direction::Input, arrayAddrType,
                                             memoryDataModuleReadAddr);
                            instance.AddPort("write_addr_in", circt::hw::ModulePort::Direction::Input, arrayAddrType,
                                             memoryDataModuleWriteAddr);
                        }
                        else
                        {
                            instance.AddPort("addr_in", circt::hw::ModulePort::Direction::Input, arrayAddrType,
                                             memoryDataModuleDualPortAddr);
                        }

                        instance.AddPort("ecc_status_out", circt::hw::ModulePort::Direction::Output, GetIntegerType(2));
                        instance.AddPort("wren_in", circt::hw::ModulePort::Direction::Input, arrayWrRdEnType,
                                         memoryDataModuleDualPortWren);
                        instance.AddPort("data_in", circt::hw::ModulePort::Direction::Input, arrayDataType,
                                         memoryDataModuleDualPortDataIn);
                        instance.AddPort("rden_in", circt::hw::ModulePort::Direction::Input, arrayWrRdEnType,
                                         memoryDataModuleDualPortRden);
                        instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output, arrayDataType);
                    }
                    // After the logics of generating SRAM instance's input ports(wren/rden/addr/wrdata),generate the
                    // SRAM instance
                    instance.Generate(&opb);
                    // The container's output port can't be generated until the SRAM instance is created.the container's
                    // read_data output logics have to be relocated after the SRAM instance's generation. (1)ECC flag
                    // generation
                    if (ecc)
                    {
                        // ECC + true-dual-port are not supported
                        assert(portsForThisModule == 1);
                        memoryDataEccStatus = instance.GetPortValue("ecc_status_out");
                    }
                    // (2) Memory read data
                    if (useLogicRam)
                    {
                        lramDataModuleReadData = instance.GetPortValue("rddata_out");
                        for (size_t i = 0; i < readPortsForStruct; i++)
                        {
                            memoryDataReadDataArray.push_back(GetTypedZeros(opb, location, GetIntegerType(dataWidth)));
                        }
                        // Read ports
                        for (size_t portOffset = 0; portOffset < portsForThisModule; portOffset++)
                        {
                            const mlir::Value portOffsetValue = opb.create<circt::hw::ConstantOp>(
                                location, opb.getIntegerAttr(GetIntegerType(clog2NumReadPorts), portOffset));
                            const mlir::Value readDataValue =
                                opb.create<circt::hw::ArrayGetOp>(location, lramDataModuleReadData, portOffsetValue);
                            memoryDataReadDataArray[portOffset] = readDataValue;
                        }
                    }
                    else if (isQuadPort)
                    {
                        memoryDataModuleDualPortDataOut = instance.GetPortValue("data_out");
                        for (size_t i = 0; i < 2; i++)
                        {
                            memoryDataReadDataArray.push_back(GetTypedZeros(opb, location, GetIntegerType(dataWidth)));
                        }
                        for (size_t port = 0; port < 2; port++)
                        {
                            const mlir::Value portValue =
                                opb.create<circt::hw::ConstantOp>(location, opb.getIntegerAttr(opb.getI1Type(), port));
                            if (port < readPorts)
                            {
                                const mlir::Value readDataValue = opb.create<circt::hw::ArrayGetOp>(
                                    location, memoryDataModuleDualPortDataOut, portValue);
                                memoryDataReadDataArray[port] = readDataValue;
                            }
                        }
                    }
                    else
                    {
                        memoryDataModuleDualPortDataOut = instance.GetPortValue("data_out");
                        for (size_t i = 0; i < 2; i++)
                        {
                            memoryDataReadDataArray.push_back(GetTypedZeros(opb, location, GetIntegerType(dataWidth)));
                        }
                        for (size_t portOffset = 0; portOffset < portsForThisModule; portOffset++)
                        {
                            // Port on the individual replica
                            // For simple dual port memories, portOffset is always 0
                            // and whichInnerPort = 1 (always read on port 1)
                            // For true dual port memories, whichInnerPort = { 1, 0 }, and portOffset = { 0, 1 } (just
                            // to keep this expression simple, either of the dual ports on the memory would work)
                            const size_t whichInnerPort = (1 - portOffset);

                            // Port on the logical memory
                            const size_t whichOuterPort = basePort + portOffset;

                            const mlir::Value whichInnerPortValue = opb.create<circt::hw::ConstantOp>(
                                location, opb.getIntegerAttr(opb.getI1Type(), whichInnerPort));
                            const mlir::Value whichOuterPortValue = opb.create<circt::hw::ConstantOp>(
                                location, opb.getIntegerAttr(opb.getI1Type(), whichOuterPort));
                            // for replicated memories (replica 0), use final_read_addr to enable inspection to sneak in
                            // on idle cycles
                            if (regDesc.Memory()._replicate && !(isInspectable && (moduleIndex == 0)))
                            {
                                const mlir::Value selectedReadData = opb.create<circt::hw::ArrayGetOp>(
                                    location, memoryDataModuleDualPortDataOut, whichInnerPortValue);
                                memoryDataReadDataArray[whichOuterPort] = selectedReadData;
                            }
                            else
                            {
                                assert(portsForThisModule == 1);

                                memoryDataReadDataArray[0] = opb.create<circt::hw::ArrayGetOp>(
                                    location, memoryDataModuleDualPortDataOut,
                                    opb.create<circt::hw::ConstantOp>(location,
                                                                      opb.getIntegerAttr(opb.getI1Type(), 1)));
                            }
                        }
                    }

                    if (regDesc.Memory().HasReadArbitration())
                    {
                        for (size_t i = 0; i < readPorts; i++)
                        {
                            memoryDataReadDataArray.push_back(GetTypedZeros(opb, location, GetIntegerType(dataWidth)));
                        }
                        // Broadcast the read result
                        for (size_t port = 1; port < readPorts; port++)
                        {
                            memoryDataReadDataArray[port] = memoryDataReadDataArray[0];
                        }
                    }
                }
                for (size_t port = 0; port < readPortsForStruct; port++)
                {
                    addOutputPort("read_data_out_" + std::to_string(port), memoryDataReadDataArray[port]);
                    if (ecc)
                    {
                        addOutputPort("ecc_status_out_" + std::to_string(port), memoryDataEccStatus);
                    }
                }
                // (3)Generate mem_init_complete for each container
                addOutputPort("init_completed", memInitDone);
                // (4)Race status
                if (RegisterTracksRaces(regDesc) && GetCodeGenConfig()._detectRaces)
                {
                    addOutputPort("has_race_occured_out", memoryDataHasRaceOccured);
                }

                if (isInspectable)
                {
                    addOutputPort("inspection_success_out", memoryDataInspectionSuccess);
                    addOutputPort("inspection_data_out", memoryDataReadDataArray[0]);
                }

                // Add an instance of the leaf container to the parent container
                {
                    const ObjectPath parentPath = SubPath(containerInstancePath, 0, containerInstancePath.size() - 1);

                    circt::kanagawa::ContainerOp parentContainer = PathToContainer(parentPath);

                    circt::OpBuilder::InsertionGuard g(opb);

                    // Add an instance
                    opb.setInsertionPointToEnd(parentContainer.getBodyBlock());

                    opb.create<circt::kanagawa::ContainerInstanceOp>(
                        location, circt::hw::InnerSymAttr::get(StringToStringAttr(containerInstancePath.back())),
                        circt::hw::InnerRefAttr::get(StringToStringAttr(GetCirctDesignName()), leafContainerNameAttr));

                    // Write clock and reset ports
                    const auto writeInputPort =
                        [&](const std::string& name, const mlir::Type type, const std::string& verbatimValue)
                    {
                        circt::OpBuilder::InsertionGuard g(opb);

                        opb.setInsertionPointToEnd(_coreModule->Container().getBodyBlock());

                        // Add a port write to the core module container
                        const llvm::SmallVector<mlir::Value> substitutions; // Empty, there are no substitutions

                        const mlir::Value value = opb.create<circt::sv::VerbatimExprOp>(
                            location, type, StringToStringAttr(verbatimValue), substitutions, nullptr);

                        _coreModule->WritePort(
                            opb, ObjectPath(), regDesc.Memory()._containerInstancePath,
                            GetFullyQualifiedStringAttr(regDesc.Memory()._containerInstancePath, name),
                            leafContainerName, GetCirctDesignName(), value);
                    };
                    // Connect clk and rst ports
                    writeInputPort("clk", GetI1Type(), "clk");
                    writeInputPort("rst", GetI1Type(), resetContainer);
                    // Connect reset completion flag
                    if (hasInitialData &&
                        GetCodeGenDeviceConfig()._memoryInitFileType == MemoryInitFileType::Asic)
                    {
                        writeInputPort("memory_init_start_in", GetI1Type(), "reset_sequence_finished_this_cycle");
                    }

                    // Read memory init status port
                    const auto readOutputPort = [&](const std::string& name, const mlir::Type type)
                    {
                        circt::OpBuilder::InsertionGuard g(opb);

                        opb.setInsertionPointToEnd(_coreModule->Container().getBodyBlock());

                        return _coreModule->ReadPort(
                            opb, ObjectPath(), regDesc.Memory()._containerInstancePath,
                            GetFullyQualifiedStringAttr(regDesc.Memory()._containerInstancePath, name),
                            leafContainerName, GetCirctDesignName(), type);
                    };
                    memInitDoneTerms.push_back(readOutputPort("init_completed", GetI1Type()));

                    if (isInspectable)
                    {
                        // Declare a top-level struct to hold inspection data
                        _coreModule->AddVerbatimOp(GetUnknownLocation(),
                                                   [&](VerbatimWriter& writer)
                                                   {
                                                       std::ostringstream str;
                                                       str << "struct packed {";
                                                       str << "logic " << OptionalWidthDeclaration(addrWidth)
                                                           << " inspection_addr; ";
                                                       str << "logic " << OptionalWidthDeclaration(dataWidth)
                                                           << " inspection_data; ";
                                                       str << "logic "
                                                           << " inspection_success; ";
                                                       str << "} memory_data_" << i << ";";

                                                       writer << str.str();
                                                   });

                        writeInputPort("inspection_addr_in", GetIntegerType(addrWidth),
                                       "memory_data_" + std::to_string(i) + ".inspection_addr");

                        _coreModule->AddVerbatimOp(
                            GetUnknownLocation(),
                            [&](VerbatimWriter& writer)
                            {
                                writer << "assign memory_data_" << i << ".inspection_success = "
                                       << readOutputPort("inspection_success_out", GetI1Type()) << ";";

                                writer << "assign memory_data_" << i << ".inspection_data = "
                                       << readOutputPort("inspection_data_out", GetIntegerType(dataWidth)) << ";";
                            });
                    }
                }
            }
        }

        circt::OpBuilder::InsertionGuard g(opb);

        opb.setInsertionPointToEnd(_coreModule->Container().getBodyBlock());

        // If there is no memory, return 1;otherwise, return redeuce-AND of memInitDoneTerms
        mlir::Value allMemoryInitialized;
        if (memInitDoneTerms.empty())
        {
            allMemoryInitialized =
                opb.create<circt::hw::ConstantOp>(GetUnknownLocation(), opb.getIntegerAttr(GetI1Type(), 1));
        }
        else
        {
            allMemoryInitialized = opb.create<circt::comb::AndOp>(GetUnknownLocation(), memInitDoneTerms, TwoState);
        }

        return allMemoryInitialized;
    }

    void InstantiateResetControlCIRCT(ModuleDeclarationHelper& mod, const size_t resetReplicas,
                                      mlir::Value hasMemInitCompleted)
    {
        circt::OpBuilder opb = circt::OpBuilder::atBlockEnd(_coreModule->Container().getBodyBlock());

        // route reset signal throughout the chip
        _writer.Str() << "logic [" << resetReplicas - 1 << ":0] reg_rst_delayed;";

        // Give wrapper code access to the delayed reset signals
        _writer.Str() << "logic combined_reset;";
        _writer.Str() << "assign combined_reset = rst;";

        // 1 on the cycle when the reset sequence is done
        _writer.Str() << "logic reset_sequence_finished_this_cycle;";

        _writer.Str() << "logic has_startup_completed_raw;";

        // 1 after memory has been initialized
        _writer.Str() << "logic has_mem_init_completed;";

        _coreModule->FlushVerbatimStrings();
        circt::sv::VerbatimExprOp verbatimOp =
            opb.create<circt::sv::VerbatimExprOp>(GetUnknownLocation(), GetI1Type(), "has_startup_completed_raw");

        const mlir::Value hasStartupCompletedRaw = static_cast<mlir::Value>(verbatimOp);
        const mlir::Value hasOthersCompleted =
            opb.create<circt::comb::AndOp>(GetUnknownLocation(), hasStartupCompletedRaw, hasMemInitCompleted, TwoState);

        {
            // Source of has_others_complete_in of KanagawaResetControl
            VerbatimWriter vw(opb, GetUnknownLocation());

            vw << "logic has_others_completed;\n";
            vw << "assign has_others_completed = " << hasOthersCompleted << ";";
        }

        // rst_and_startup_done_out is a top level port used to indicate either:
        //   (a) no reset has ever been applied, or
        //   (b) both the reset sequence and the startup functions have finished
        // In particular, one should ensure that this is 1 before:
        //   (i) raising the reset signal (which would overlap with the ongoing reset)
        //   (ii) calling any exported [[no_backpressure]] functions which are now
        //        ready to accept data
        {
            _writer.Str() << "logic rst_and_startup_done_raw;";

            std::ostringstream str;

            std::list<const Function*> externFunctionList;
            for (const Function* const function : _program._externFunctions)
            {
                externFunctionList.push_back(function);
            }

            for (const Function* const function : _program._unreferencedExternFunctions)
            {
                externFunctionList.push_back(function);
            }

            for (const ExternalClassInstance& externClassInstance : _program._externClassInstances)
            {
                if (externClassInstance._isExportClass)
                {
                    const std::string instanceName = GetExternalClassInstanceName(externClassInstance).second;

                    _writer.Str() << "logic " << instanceName << "_rst_and_startup_done_out;";

                    str << " & " << instanceName << "_rst_and_startup_done_out";
                }
            }
        }

        assert(GetCodeGenConfig()._resetFanOutCycles <= GetCodeGenConfig()._resetCycles);

        ModuleInstanceHelper instance(*this, GetUnknownLocation());
        instance.SetModuleName("KanagawaResetControl");
        instance.AddU64Parameter("WIDTH", resetReplicas);
        instance.AddU64Parameter("DELAY_CYCLES", GetCodeGenConfig()._resetCycles);
        instance.AddU64Parameter("FAN_OUT_LEVELS", GetCodeGenConfig()._resetFanOutCycles);
        instance.AddU64Parameter("HOLD_CYCLES", GetResetHoldCycles());
        instance.AddU64Parameter("ADDTIONAL_LATENCY", std::max<size_t>(1, GetCodeGenConfig()._additionalLatency));
        instance.AddU64Parameter("INIT_VAL", 1);
        instance.SetInstanceName("reset_control");
        instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
        instance.AddPort("rst_in", circt::hw::ModulePort::Direction::Input, GetI1Type(), "combined_reset");
        instance.AddPort("has_others_completed_in", circt::hw::ModulePort::Direction::Input, hasOthersCompleted);
        instance.AddPort("rst_and_startup_done_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                         mod.AssignPort("rst_and_startup_done_out"));
        instance.AddPort("rst_delayed_out", circt::hw::ModulePort::Direction::Output, GetIntegerType(resetReplicas),
                         "reg_rst_delayed");
        instance.AddPort("reset_sequence_finished_this_cycle_out", circt::hw::ModulePort::Direction::Output,
                         GetI1Type(), "reset_sequence_finished_this_cycle");
        instance.Generate();

        _writer.Str() << "logic [" << MaxClockCount - 1 << ":0] rst_array;";
        _writer.Str() << "assign rst_array[0] = combined_reset;";
        assert(_resetReplicaIndex == 0);

        // Synchronize reset signal to other clock domains
        for (size_t i = 1; i < MaxClockCount; i++)
        {
            ModuleInstanceHelper instance0(*this, GetUnknownLocation());
            instance0.SetModuleName("KanagawaHALResetSynchronizer");
            instance0.SetInstanceName("reset_sync_" + std::to_string(i));
            instance0.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetI1Type(), GetClockString(i));
            instance0.AddPort("arst", circt::hw::ModulePort::Direction::Input, GetI1Type(), GetResetReplica());
            instance0.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                              "rst_array[" + std::to_string(i) + "]");
            instance0.Generate();
        }
        assert(_resetReplicaIndex == (MaxClockCount - 1));
    }

    // Generate a file with the initial contents of a memory
    std::string WriteMemoryContentFile(const size_t registerIndex)
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];

        const MemoryInitFileType fileType = GetCodeGenDeviceConfig()._memoryInitFileType;

        const char* const extension = (fileType == MemoryInitFileType::Mif)   ? "mif"
                                      : (fileType == MemoryInitFileType::Mem) ? "mem"
                                                                              : "sv";

        const std::string uniqueRegName = UniqueRegisterName(registerIndex);

        // If the contents of fileNameStr is changed, it should also be reflected in
        // ResourceUsage.cpp::DumpSyncRamAndROM()
        std::ostringstream fileNameStr;
        fileNameStr << _memFileBase << uniqueRegName << "." << extension;

        std::string fileName = fileNameStr.str();

        std::ofstream str;
        if (fileType == MemoryInitFileType::Mif || fileType == MemoryInitFileType::Mem)
        {
            OpenOutputFileStream(str, fileName.c_str());
        }

        const size_t elementWidth = regDesc.Memory()._elementWidth;

        const size_t initialValuesSize = regDesc.Memory()._initialValues.size();
        const size_t initialValuesSizePow2 = (1ull << Log2RoundUp(initialValuesSize));

        assert(initialValuesSize == regDesc.Memory()._elementCount);

        if (fileType == MemoryInitFileType::Mif)
        {
            str << "DEPTH = " << regDesc.Memory()._elementCount << ";\n";
            str << "WIDTH = " << elementWidth << ";\n";
            str << "ADDRESS_RADIX = HEX;\n";
            str << "DATA_RADIX = HEX;\n";
            str << "CONTENT\n";
            str << "BEGIN\n";

            for (size_t i = 0; i < initialValuesSize; i++)
            {
                const mp_int value = regDesc.Memory()._initialValues[i];

                str << std::hex << i << " : " << std::hex << value << ";\n";
            }

            str << "END;";
        }
        else if (fileType == MemoryInitFileType::Mem)
        {
            assert(fileType == MemoryInitFileType::Mem);

            for (size_t i = 0; i < initialValuesSize; i++)
            {
                const mp_int value = regDesc.Memory()._initialValues[i];

                str << std::hex << value << "\n";
            }
        }
        else
        {
            assert(fileType == MemoryInitFileType::Asic);
        }

        return fileName;
    }

    // Returns the name of the internal fifo that holds call indices for return routing
    std::string GetExternReturnRouterRootName(const Function* const function)
    {
        std::ostringstream str;

        str << "extern_return_router_" << function->GetBackendName();

        return str.str();
    }

    std::pair<std::string, std::string> GetExternalClassInstanceName(const ExternalClassInstance& externClassInstance)
    {
        const std::string moduleName = FixupString(FlattenScopeAndAppendName(
            externClassInstance._isExportClass ? Scope() : externClassInstance._scope, externClassInstance._name));

        std::string instanceName = moduleName;

        if (externClassInstance._objectName != g_globalObjectName)
        {
            instanceName = externClassInstance._objectName + "::" + instanceName;
        }

        return {moduleName, FixupString(instanceName)};
    }

    // Update include file
    void DeclareMemInitLutModules()
    {
        for (size_t i = 0; i < _program._registerTable.size(); ++i)
        {
            const RegisterDescription& regDesc = _program._registerTable[i];

            if ((regDesc._type == RegisterType::Memory) && !regDesc.Memory()._initialValues.empty())
            {
                if (GetCodeGenDeviceConfig()._memoryInitFileType == MemoryInitFileType::Asic)
                {
                    AutoIndent autoIndent2(_writer);
                    std::ostringstream svModuleNameStr;
                    svModuleNameStr << _memFileBase << UniqueRegisterName(i);
                    std::string svModuleName = svModuleNameStr.str();
                    {
                        AutoSectionRAII moduleBlock(_writer, true, "module MemInitLUT_" + svModuleName + " #(",
                                                    "endmodule : MemInitLUT_" + svModuleName);
                        _writer.Str() << "parameter	ADDR_WIDTH		= 16,";
                        _writer.Str() << "parameter	DATA_WIDTH  	= 32";
                        _writer.Str() << ")(";
                        _writer.Str() << "input   wire    clk,";
                        _writer.Str() << "input	wire	[ADDR_WIDTH-1:0]	addr_in,";
                        _writer.Str() << "output  logic	[DATA_WIDTH-1:0]	data_out\n);";
                        {
                            AutoSectionRAII alwaysClockBlock(_writer, true, "always @(posedge clk) begin", "end");
                            {
                                AutoSectionRAII caseStatement(_writer, true, "case(addr_in)", "endcase");
                                for (size_t j = 0; j < regDesc.Memory()._initialValues.size(); j++)
                                {
                                    _writer.Str()
                                        << "'d" << std::dec << j << ":\t data_out <= " << regDesc.Memory()._elementWidth
                                        << "'h" << std::hex << regDesc.Memory()._initialValues[j] << ";" << std::dec;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Assigns a value to has_startup_completed_raw
    // This is based on when all [[reset]] functions complete their initial invocation
    void WaitForResetFunctions()
    {
        std::vector<std::string> resultFifos;

        for (const Function& function : _program._functions)
        {
            if (function.CallOnReset())
            {
                assert(function._returnFifoRegisterIndex != c_invalidAccessedRegisterIndex);
                const size_t fifoIndexNorm = _fifoNamer.GetNormFifoIndex(function._returnFifoRegisterIndex);
                const std::string resultStr = std::string("!fifo_data_") + std::to_string(fifoIndexNorm) + ".empty";

                // The output from the initial call will go into a FIFO and stay there.
                _writer.Str() << "assign fifo_data_" << fifoIndexNorm << ".rden = 1'b0;";

                resultFifos.push_back(resultStr);
            }
        }

        // Set has_startup_completed_raw once all function invocations have finished
        _writer.Str() << "assign has_startup_completed_raw = " << BitwiseReduce(resultFifos, "&", "1'b1") << ";";
    }

    void InstantiateExternalClassInstances(ModuleDeclarationHelper& mod)
    {
        std::list<const Function*> externFunctionList;

        for (const Function* const function : _program._externFunctions)
        {
            externFunctionList.push_back(function);
        }

        for (const Function* const function : _program._unreferencedExternFunctions)
        {
            externFunctionList.push_back(function);
        }

        // (module name) -> set<method name>
        std::map<std::string, std::set<std::string>> referencedMethodNames;

        const auto methodIsReferenced = [&](const std::string& objectName, const std::string& methodName)
        {
            const auto it1 = referencedMethodNames.find(objectName);
            if (it1 != referencedMethodNames.end())
            {
                const std::set<std::string>& referencedMethods = it1->second;

                return referencedMethods.end() != referencedMethods.find(methodName);
            }

            return false;
        };

        for (const ExternalClassInstance& externClassInstance : _program._externClassInstances)
        {
            // 1) Export class instances use:
            // valid/rdy (push) for parameters
            // and empty/rden (pull) for return values
            //
            // 2) Other extern modules (including connections to extern return router) use:
            // empty/rden (pull) for parameters
            // and valid/rdy (push) for return values
            //
            // When an extern class is instantiated, and adapter is used to map between the protocols

            struct ExternalClassInstancePort
            {
                ExternalClassInstancePort() {}

                ExternalClassInstancePort(const size_t w, const Function* const function, const std::string& portName,
                                          const std::string& externClassPortName, const bool isInput)
                    : _width(w), _shortFunctionName(FixupString(function->_name)),
                      _longFunctionName(function->GetBackendName()), _portName(FixupString(portName)),
                      _externClassPortName(FixupString(externClassPortName)), _isInput(isInput)
                {
                }

                ExternalClassInstancePort(const size_t w, const std::string& callbackName,
                                          const Function* const calledFunction, const std::string& portName,
                                          const std::string& externClassPortName, const bool isInput)
                    : _width(w), _shortFunctionName(callbackName), _longFunctionName(calledFunction->GetBackendName()),
                      _portName(FixupString(portName)), _externClassPortName(FixupString(externClassPortName)),
                      _isInput(isInput)
                {
                }

                ExternalClassInstancePort(const size_t w, const ExternalClassInstance& externClassInstance,
                                          const std::string& methodName, const std::string& portName,
                                          const bool isInput)
                    : _width(w), _shortFunctionName(FixupString(methodName)),
                      _longFunctionName(FixupString(
                          CombineObjectAndMemberName(Scope(), externClassInstance._objectName, methodName))),
                      _portName(FixupString(portName)), _externClassPortName(FixupString(portName)), _isInput(isInput)
                {
                }

                std::string GetFullName() const { return _longFunctionName + "_" + _portName; }

                // Returns variable name for variables directly connected to export class instances
                std::string GetExportClassFullName() const
                {
                    std::string result = _longFunctionName + "_" + _externClassPortName;

                    if (UniqueExportClassPortName())
                    {
                        result += "_export_class";
                    }

                    return result;
                }

                std::string GetShortName() const
                {
                    const std::string inOutString = _isInput ? "_in" : "_out";

                    return _shortFunctionName + "_" + _portName + inOutString;
                }

                std::string GetExportClassShortName() const
                {
                    const std::string inOutString = _isInput ? "_in" : "_out";

                    return _shortFunctionName + "_" + _externClassPortName + inOutString;
                }

                std::string GetRawPortName() const { return _portName; }

                std::string GetRawExternClassPortName() const { return _externClassPortName; }

                std::string GetFullFunctionName() const { return _longFunctionName; }

                size_t GetWidth() const { return _width; }

                bool IsInput() const { return _isInput; }

                void ReverseDirection() { _isInput = !_isInput; }

                bool UniqueExportClassPortName() const { return _portName != _externClassPortName; }

              private:
                size_t _width;
                std::string _shortFunctionName;
                std::string _longFunctionName;
                std::string _portName;
                std::string _externClassPortName;
                bool _isInput;
                bool _includeSuffixInFullName;
            };

            std::list<ExternalClassInstancePort> ports;

            // Extern module calling Kanagawa code
            for (const ExternalClassInstanceCallback& callback : externClassInstance._callbacks)
            {
                {
                    const FunctionNode* const functionNode = callback._calleeNode;

                    const Function* const function = callback.GetCalledFunction();

                    if (!function->IsNoBackpressure())
                    {
                        if (externClassInstance._isExportClass)
                        {
                            ports.push_back(ExternalClassInstancePort(1, function, "rdy", "rden", true));
                        }
                        else
                        {
                            ports.push_back(
                                ExternalClassInstancePort(1, callback._callbackName, function, "rdy", "rdy", true));
                        }
                    }

                    for (size_t i = 0; i < functionNode->GetParameterCount(); i++)
                    {
                        if (externClassInstance._isExportClass)
                        {
                            ports.push_back(ExternalClassInstancePort(functionNode->GetParameterWidth(i), function,
                                                                      functionNode->GetParameterName(i),
                                                                      functionNode->GetParameterName(i), false));
                        }
                        else
                        {
                            ports.push_back(ExternalClassInstancePort(
                                functionNode->GetParameterWidth(i), callback._callbackName, function,
                                functionNode->GetParameterName(i), functionNode->GetParameterName(i), false));
                        }
                    }

                    if (externClassInstance._isExportClass)
                    {
                        ports.push_back(ExternalClassInstancePort(1, function, "valid", "empty", false));
                    }
                    else
                    {
                        ports.push_back(
                            ExternalClassInstancePort(1, callback._callbackName, function, "valid", "valid", false));
                    }

                    if (!(functionNode->GetModifiers() & ParseTreeFunctionModifierAsync))
                    {
                        if (!function->IsNoBackpressure())
                        {
                            if (externClassInstance._isExportClass)
                            {
                                ports.push_back(ExternalClassInstancePort(1, function, "rden", "rdy", false));
                            }
                            else
                            {
                                ports.push_back(ExternalClassInstancePort(1, callback._callbackName, function, "rden",
                                                                          "rden", false));
                            }
                        }

                        const size_t returnWidth = functionNode->GetReturnType()->GetBitWidth();

                        if (returnWidth > 0)
                        {
                            if (externClassInstance._isExportClass)
                            {
                                ports.push_back(
                                    ExternalClassInstancePort(returnWidth, function, "result", "result", true));
                            }
                            else
                            {
                                ports.push_back(ExternalClassInstancePort(returnWidth, callback._callbackName, function,
                                                                          "result", "result", true));
                            }
                        }

                        if (!function->IsFixedLatency())
                        {
                            if (externClassInstance._isExportClass)
                            {
                                ports.push_back(ExternalClassInstancePort(1, function, "empty", "valid", true));
                            }
                            else
                            {
                                ports.push_back(ExternalClassInstancePort(1, callback._callbackName, function, "empty",
                                                                          "empty", true));
                            }
                        }
                    }
                }
            }

            // Kanagawa code calling extern module

            // Used to track the set of interface methods which were never called
            std::map<std::string, FunctionDesc> unreferencedInterfaceMethods = externClassInstance._interfaceMethods;

            for (const Function* const function : externFunctionList)
            {
                if (function->_externClassInstance == &externClassInstance)
                {
                    const FunctionNode* const functionNode = function->_functionNode;

                    referencedMethodNames[externClassInstance._objectName].insert(function->_name);

                    unreferencedInterfaceMethods.erase(function->_name);

                    // Parameters
                    if (function->IsNoBackpressure())
                    {
                        ports.push_back(ExternalClassInstancePort(1, function, "valid", "valid", true));
                    }
                    else
                    {
                        ports.push_back(ExternalClassInstancePort(1, function, "rden", "rdy", false));
                    }

                    for (size_t i = 0; i < functionNode->GetParameterCount(); i++)
                    {
                        const ExternalClassInstancePort port = ExternalClassInstancePort(
                            functionNode->GetParameterWidth(i), function, functionNode->GetParameterName(i),
                            functionNode->GetParameterName(i), true);
                        ports.push_back(port);
                    }

                    if (!function->IsNoBackpressure())
                    {
                        ports.push_back(ExternalClassInstancePort(1, function, "empty", "valid", true));
                    }

                    if (!(functionNode->GetModifiers() & ParseTreeFunctionModifierAsync))
                    {
                        ports.push_back(ExternalClassInstancePort(1, function, "valid", "empty", false));

                        const size_t returnWidth = functionNode->GetReturnType()->GetBitWidth();

                        if (returnWidth > 0)
                        {
                            const ExternalClassInstancePort port =
                                ExternalClassInstancePort(returnWidth, function, "result", "result", false);
                            ports.push_back(port);
                        }

                        ports.push_back(ExternalClassInstancePort(1, function, "rdy", "rden", true));
                    }
                }
            }

            // Fixed latency methods
            for (const auto& p : externClassInstance._fixedLatencyMethods)
            {
                const std::string& name = p.first;
                const FunctionDesc& methodDesc = p.second;

                // Input valid
                ports.push_back(ExternalClassInstancePort(1, externClassInstance, name, "valid", true));

                // Parameters
                assert(methodDesc._parameterNames.size() == methodDesc._parameterTypes.size());

                for (size_t i = 0; i < methodDesc._parameterNames.size(); i++)
                {
                    // Struct/Array parameters are not currently spuported
                    assert(1 == methodDesc._parameterTypes[i]->GetRegisterCount());

                    ports.push_back(ExternalClassInstancePort(methodDesc._parameterTypes[i]->GetBitWidth(),
                                                              externClassInstance, name, methodDesc._parameterNames[i],
                                                              true));
                }

                // Return
                if (methodDesc._returnType->GetBitWidth() > 0)
                {
                    ports.push_back(ExternalClassInstancePort(methodDesc._returnType->GetBitWidth(),
                                                              externClassInstance, name, "result", false));
                }
            }

            const std::string resetReplica = GetResetReplica();

            for (const ExternalClassInstancePort& port : ports)
            {
                _writer.Str() << "logic " << OptionalWidthDeclaration(port.GetWidth()) << port.GetFullName() << ";";

                if (port.UniqueExportClassPortName())
                {
                    _writer.Str() << "logic " << OptionalWidthDeclaration(port.GetWidth())
                                  << port.GetExportClassFullName() << ";";
                }
            }

            auto names = GetExternalClassInstanceName(externClassInstance);
            const std::string moduleName = names.first;
            const std::string instanceName = names.second;

            if (externClassInstance._isExportClass)
            {
                const auto inspectableIt = _externClassInstanceInspectableIndices.find(&externClassInstance);
                const bool hasInspectable =  GetCodeGenConfig()._inspection && inspectableIt != _externClassInstanceInspectableIndices.end();

                const size_t inspectableChainDelay =
                    (GetCodeGenConfig()._inspection && (GetCodeGenConfig()._autoPipelineCrossRegion > 0))
                        ? GetCodeGenConfig()._autoPipelineCrossRegion
                        : 0;

                const size_t clockIndex = 0;

                if (hasInspectable)
                {
                    const size_t inspectableIndex = inspectableIt->second;

                    // Net to hold the output of the KanagawaCrossRegionInspectableChain that sits on the input
                    // side of the inspectable chain
                    _writer.Str() << "KanagawaTypes::InspectableValue inspectable_" << inspectableIndex
                                  << "_input_chain_out;";

                    // Net to hold the output of the extern class inspectable chain / input to the
                    // KanagawaCrossRegionInspectableChain that sits on the output side of the inspectable chain
                    _writer.Str() << "KanagawaTypes::InspectableValue inspectable_" << inspectableIndex
                                  << "_output_chain_in;";

                    // KanagawaCrossRegionInspectableChain that sits on the input side of the inspectable chain
                    ModuleInstanceHelper instance(*this, GetUnknownLocation());
                    instance.SetModuleName("KanagawaCrossRegionInspectableChain");
                    instance.AddU64Parameter("DELAY", inspectableChainDelay);
                    instance.AddU64Parameter("MIN_DELAY", inspectableChainDelay);
                    instance.AddStringParameter("AUTO_PIPELINE_GROUP",
                                                "inspectable_" + std::to_string(inspectableIndex) + "_input_chain");
                    instance.SetInstanceName("inspectable_" + std::to_string(inspectableIndex) + "_input_chain_inst");
                    instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                     GetClockString(clockIndex));
                    instance.AddPort("inspection_value_in", circt::hw::ModulePort::Direction::Input,
                                    GetInspectableStructType(),
                                    "inspectable_" + std::to_string(inspectableIndex) + "_value_in");
                    instance.AddPort("inspection_value_out", circt::hw::ModulePort::Direction::Output,
                                    GetInspectableStructType(),
                                    "inspectable_" + std::to_string(inspectableIndex) + "_input_chain_out");
                    instance.Generate();
                    // KanagawaCrossRegionInspectableChain that sits on the output side of the inspectable chain
                    ModuleInstanceHelper instance0(*this, GetUnknownLocation());
                    instance0.SetModuleName("KanagawaCrossRegionInspectableChain");
                    instance0.AddU64Parameter("DELAY", inspectableChainDelay);
                    instance0.AddU64Parameter("MIN_DELAY", inspectableChainDelay);
                    instance0.AddStringParameter("AUTO_PIPELINE_GROUP",
                                                 "inspectable_" + std::to_string(inspectableIndex) + "_output_chain");
                    instance0.SetInstanceName("inspectable_" + std::to_string(inspectableIndex) + "_output_chain_inst");
                    instance0.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                      GetClockString(clockIndex));
                    instance0.AddPort("inspection_value_in", circt::hw::ModulePort::Direction::Input,
                                      GetInspectableStructType(),
                                      "inspectable_" + std::to_string(inspectableIndex) + "_output_chain_in");
                    instance0.AddPort("inspection_value_out", circt::hw::ModulePort::Direction::Output,
                                      GetInspectableStructType(),
                                      "inspectable_" + std::to_string(inspectableIndex) + "_value_out");
                    instance0.Generate();
                }

                // Extern class instance
                _writer.Str() << moduleName << " " << instanceName << "_inst";

                _writer.Str() << "(";

                {
                    AutoIndent autoIndent2(_writer);

                    _writer.Str() << ".clk(" << GetClockString(clockIndex) << "),";

                    _writer.Str() << ".rst(" << GetResetString(clockIndex, resetReplica) << "),";

                    _writer.Str() << ".rst_and_startup_done_out(" << instanceName << "_rst_and_startup_done_out),";

                    if (hasInspectable)
                    {
                        const size_t inspectableIndex = inspectableIt->second;

                        _writer.Str() << ".inspection_value_in(inspectable_" << inspectableIndex
                                      << "_input_chain_out),";
                        _writer.Str() << ".inspection_value_out(inspectable_" << inspectableIndex
                                      << "_output_chain_in),";
                    }
                    else if (GetCodeGenConfig()._inspection)
                    {
                        _writer.Str() << ".inspection_value_in(),";
                        _writer.Str() << ".inspection_value_out(),";
                    }

                    ConnectStallRateVariables();

                    // Connect ports for interface methods which are never called
                    for (const auto& p : unreferencedInterfaceMethods)
                    {
                        const std::string& methodName = p.first;
                        const FunctionDesc& functionDesc = p.second;

                        _writer.Str() << "." << methodName << "_rdy_out(),";
                        _writer.Str() << "." << methodName << "_valid_in(1'b0),";

                        for (const std::string& parameterName : functionDesc._parameterNames)
                        {
                            _writer.Str() << "." << methodName << "_" << parameterName << "_in('0),";
                        }

                        if (!(functionDesc._modifiers & ParseTreeFunctionModifierAsync))
                        {
                            _writer.Str() << "." << methodName << "_empty_out(),";

                            if (functionDesc._returnType->GetBitWidth() > 0)
                            {
                                _writer.Str() << "." << methodName << "_result_out(),";
                            }

                            _writer.Str() << "." << methodName << "_rden_in(1'b0),";
                        }
                    }

                    for (auto it = ports.begin(); it != ports.end(); ++it)
                    {
                        const ExternalClassInstancePort& port = *it;

                        auto nextIt = it;
                        ++nextIt;

                        const bool isLast = (nextIt == ports.end());

                        const std::string trailingComma = isLast ? "" : ",";

                        _writer.Str() << "." << port.GetExportClassShortName() << "(" << port.GetExportClassFullName()
                                      << ")" << trailingComma;
                    }
                }

                _writer.Str() << ");";

                // Convert between the protocol used by the export class
                // and the protocols used by the extern return router
                for (const ExternalClassInstancePort& port : ports)
                {
                    if (port.UniqueExportClassPortName())
                    {
                        if (port.GetRawExternClassPortName() == "rdy")
                        {
                            assert(port.GetRawPortName() == "rden");

                            _writer.Str() << "assign " << port.GetFullName() << " = " << port.GetExportClassFullName()
                                          << " & ~" << port.GetFullFunctionName() << "_empty;";
                        }
                        else if (port.GetRawExternClassPortName() == "valid")
                        {
                            assert(port.GetRawPortName() == "empty");

                            _writer.Str() << "assign " << port.GetExportClassFullName() << " = "
                                          << port.GetFullFunctionName() << "_rden;";
                        }
                        else if (port.GetRawExternClassPortName() == "empty")
                        {
                            assert(port.GetRawPortName() == "valid");

                            _writer.Str()
                                << "assign " << port.GetFullName() << " = ~" << port.GetExportClassFullName() << ";";
                        }
                        else if (port.GetRawExternClassPortName() == "rden")
                        {
                            assert(port.GetRawPortName() == "rdy");

                            _writer.Str()
                                << "assign " << port.GetExportClassFullName() << " = " << port.GetFullFunctionName()
                                << "_valid & " << port.GetFullFunctionName() << "_rdy;";
                        }
                        else
                        {
                            assert(false);
                        }
                    }
                }
            }
            else
            {
                _writer.Str() << moduleName;

                if (!externClassInstance._templateArguments.empty())
                {
                    CommaSeparatedOutputHelper csvHelper;

                    for (const auto& arg : externClassInstance._templateArguments)
                    {
                        csvHelper.Append("." + arg._name + "(" + arg._literalValue + ")");
                    }

                    _writer.Str() << "#(" << csvHelper.Str() << ")";
                    ;
                }

                _writer.Str() << instanceName << "_inst";

                _writer.Str() << "(";

                {
                    AutoIndent autoIndent2(_writer);

                    const size_t clockIndex = 0;

                    _writer.Str() << ".clk(" << GetClockString(clockIndex) << "),";

                    _writer.Str() << ".rst(" << GetResetString(clockIndex, resetReplica) << "),";

                    for (const auto& p : unreferencedInterfaceMethods)
                    {
                        const std::string& methodName = p.first;
                        const FunctionDesc& functionDesc = p.second;

                        _writer.Str() << "." << methodName << "_rden_out(),";
                        _writer.Str() << "." << methodName << "_empty_in(1'b0),";

                        for (const std::string& parameterName : functionDesc._parameterNames)
                        {
                            _writer.Str() << "." << methodName << "_" << parameterName << "_in('0),";
                        }

                        if (!(functionDesc._modifiers & ParseTreeFunctionModifierAsync))
                        {
                            _writer.Str() << "." << methodName << "_valid_out(),";

                            if (functionDesc._returnType->GetBitWidth() > 0)
                            {
                                _writer.Str() << "." << methodName << "_result_out(),";
                            }

                            _writer.Str() << "." << methodName << "_rdy_in(1'b0),";
                        }
                    }

                    for (auto it = ports.begin(); it != ports.end(); ++it)
                    {
                        const ExternalClassInstancePort& port = *it;

                        auto nextIt = it;
                        ++nextIt;

                        const bool isLast = (nextIt == ports.end());

                        const std::string trailingComma = isLast ? "" : ",";

                        _writer.Str() << "." << port.GetShortName() << "(" << port.GetFullName() << ")"
                                      << trailingComma;
                    }
                }

                _writer.Str() << ");";
            }
        }
    }

    void InstantiateFifos(ModuleDeclarationHelper& mod)
    {
        // has_others_completed is connected to the reset pin of all FIFOs that accept inputs from the outside
        // to prevent the outside from sending inputs before startup and memory initialization have completed
        // The only exception is no_backpressure fifos, where the outside world must use rst_and_startup_done_out
        // to determine when it is safe to send inputs.
        //
        // Each destination that has_others_completed connects to gets its own KanagawaFlipFlopChainNoEnable
        // to make placement easier
        size_t startupAndMemInitCompletedReplicaCount = 0;

        std::set<size_t> interfaceFifos;

        const auto getFifos = [&](const Function* const function)
        {
            // Reset functions have no FIFOs
            if ((function->_start->_inputFifoCount == 0) &&
                (function->_functionNode->GetModifiers() & ParseTreeFunctionModifierReset))
                return;

            assert(1 == function->_start->_inputFifoCount);
            const size_t fifoIndexIn = function->_start->_inputFifoIndices[0];
            const size_t fifoIndexOut = function->_returnFifoRegisterIndex;

            const size_t inputFifoIndex = fifoIndexIn;

            if (inputFifoIndex != c_invalidAccessedRegisterIndex)
            {
                switch (_program._registerTable[inputFifoIndex].Fifo()._type)
                {
                case FifoType::Default:
                case FifoType::ReorderBuffer:
                case FifoType::PassthroughRegistered:
                case FifoType::Passthrough:
                    interfaceFifos.insert(inputFifoIndex);
                    startupAndMemInitCompletedReplicaCount++;
                    break;

                // These types cannot be interface fifos
                // or they do not have backpressure
                // and the caller must use rst_and_startup_done_out
                case FifoType::ContextSaverCaller:
                case FifoType::PassthroughUnregistered:
                case FifoType::FixedDelay:
                    break;

                default:
                    assert(false);
                    break;
                }
            }
        };

        for (const EntryPoint& entryPoint : _program._entryPoints)
        {
            for (Function* const function : entryPoint._instances)
            {
                getFifos(function);
            }
        }

        for (const Function* const function : _program._externFunctions)
        {
            getFifos(function);
        }

        // For timing reasons, has_others_completed is explictly fanned-out through registers here
        // Count the number of replicas needed.
        // has_others_completed is used rather than has_startup_completed
        // to take memory initialization into account.
        for (size_t i = 0; i < startupAndMemInitCompletedReplicaCount; i++)
        {
            std::ostringstream replicaName;
            replicaName << "has_startup_completed_delayed_" << i;

            _writer.Str() << "logic " << replicaName.str() << ";";

            if (GetCodeGenConfig()._additionalLatency != 0)
            {
                ModuleInstanceHelper instance(*this, GetUnknownLocation());
                instance.SetModuleName("KanagawaFlipFlopChainNoEnable");
                instance.AddU64Parameter("WIDTH", 1);
                instance.AddU64Parameter("DEPTH", GetCodeGenConfig()._additionalLatency);
                instance.SetInstanceName(replicaName.str() + "_delay_chain");
                instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
                instance.AddPort("data_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 "has_others_completed");
                instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output, GetI1Type(), replicaName.str());
                instance.Generate();
            }
            else
            {
                _writer.Str() << "assign " << replicaName.str() << " = has_others_completed;";
            }
        }

        size_t startupAndMemInitCompletedReplicaIndex = 0;

        for (size_t i = 0; i < _program._registerTable.size(); ++i)
        {
            const RegisterDescription& regDesc = _program._registerTable[i];

            if (regDesc._type == RegisterType::Fifo)
            {
                // Returns a string that should be connected to FIFO reset signals
                // Can be a combination of a reset replica, and a signal that indicates that startup + mem
                // initialization has completed
                const auto getResetString = [&](bool needsReset)
                {
                    std::string result;

                    if (interfaceFifos.end() != interfaceFifos.find(i))
                    {
                        const std::string startupNotCompleted = std::string("!has_startup_completed_delayed_") +
                                                                std::to_string(startupAndMemInitCompletedReplicaIndex);

                        if (needsReset)
                        {
                            // Avoid lint errors related to a binary expression directly used in port assignment
                            // .rst(a | b)
                            const std::string netName = std::string("rst_or_not_startup_completed_delayed_") +
                                                        std::to_string(startupAndMemInitCompletedReplicaIndex);

                            _writer.Str() << "logic " << netName << ";";
                            _writer.Str() << "assign " << netName << " = " << std::string("(") << GetResetReplica()
                                          << " | " << startupNotCompleted << ");";

                            result = netName;
                        }
                        else
                        {
                            result = startupNotCompleted;
                        }

                        startupAndMemInitCompletedReplicaIndex++;
                    }
                    else
                    {
                        if (needsReset)
                        {
                            result = GetResetReplica();
                        }
                        else
                        {
                            result = "";
                        }
                    }

                    return result;
                };

                const std::string fifoName = _fifoNamer.GetFifoName(i);

                if (!GetCodeGenDeviceConfig()._supportsAutoPipelining)
                {
                    // If the device does not support auto-pipelining, then auto-pipelining should be disabled
                    // on the FIFO. For our FIFO, we do this by setting MIN_WRITE_DELAY == WRITE_DELAY parameters.
                    assert(regDesc.Fifo()._writeDelay == regDesc.Fifo()._minWriteDelay);
                }

                const size_t logicalFifoWidth = GetLogicalFifoWidth(regDesc);
                const size_t physicalFifoWidth = GetPhysicalFifoWidth(regDesc);

                // Names of input/data signals
                // for declaring constant bits
                std::vector<std::pair<std::string, std::string>> topLevelSignalNames;

                const size_t iNorm = _fifoNamer.GetNormFifoIndex(i);

                if ((FifoType::Default == regDesc.Fifo()._type) || (FifoType::ReorderBuffer == regDesc.Fifo()._type))
                {
                    const bool isReorderBuffer = (FifoType::ReorderBuffer == regDesc.Fifo()._type);

                    const bool isDualClock = (regDesc.Fifo()._readClock != regDesc.Fifo()._writeClock);

                    const std::string structName = std::string("fifo_data_") + std::to_string(iNorm);

                    // Struct that holds wires connected to FIFO
                    _writer.Str() << "struct packed {";

                    {
                        AutoIndent autoIndent2(_writer);

                        _writer.Str() << "logic rden;";
                        _writer.Str() << "logic empty;";

                        _writer.Str() << "logic wren;";
                        _writer.Str() << "logic almost_full;";

                        // Logical fifo data accessed by the reset of the generated code
                        if (logicalFifoWidth > 0)
                        {
                            _writer.Str() << "logic [" << logicalFifoWidth - 1 << ":0] data_in;";
                            _writer.Str() << "logic [" << logicalFifoWidth - 1 << ":0] data_out;";

                            topLevelSignalNames.emplace_back(structName, "data_in");
                            topLevelSignalNames.emplace_back(structName, "data_out");

                            if (FifoSupportsEncoding(regDesc))
                            {
                                if (physicalFifoWidth > 0)
                                {
                                    // Physical fifo data accessed by the fifo
                                    _writer.Str() << "logic [" << physicalFifoWidth - 1 << ":0] encoded_data_in;";
                                    _writer.Str() << "logic [" << physicalFifoWidth - 1 << ":0] encoded_data_out;";
                                }
                            }
                            else
                            {
                                assert(logicalFifoWidth == physicalFifoWidth);
                            }
                        }

                        _writer.Str() << "logic overflow;";
                        _writer.Str() << "logic underflow;";

                        AddGlobalDebugSignal(structName, "overflow");
                        AddGlobalDebugSignal(structName, "underflow");
                    }

                    _writer.Str() << "} " << structName << ";";

                    if (FifoSupportsEncoding(regDesc))
                    {
                        // Emit encoding and decoding logic
                        const FifoCode& fifoCode = regDesc.Fifo()._code;

                        assert(fifoCode._encodedWidth == physicalFifoWidth);

                        for (const FifoCodeInputRange& inputRange : fifoCode._inputRanges)
                        {
                            _writer.Str()
                                << "assign fifo_data_" << iNorm << ".encoded_data_in"
                                << Slice(inputRange._encodedOffset, inputRange._width) << " = fifo_data_" << iNorm
                                << ".data_in" << Slice(inputRange._decodedOffset, inputRange._width) << ";";
                        }

                        for (const FifoCodeOutputRange& outputRange : fifoCode._outputRanges)
                        {
                            std::ostringstream str;

                            str << "assign fifo_data_" << iNorm << ".data_out"
                                << Slice(outputRange._decodedOffset, outputRange._width) << " = ";

                            if (outputRange._isLiteral)
                            {
                                Literal l = {};

                                l._width = outputRange._width;
                                l._value = outputRange._literal;

                                str << l;
                            }
                            else
                            {
                                str << "fifo_data_" << iNorm << ".encoded_data_out"
                                    << Slice(outputRange._encodedOffset, outputRange._width);
                            }

                            _writer.Str() << str.str() << ";";
                        }
                    }

                    bool hasWriteDelayParam = false;
                    bool hasAlmostEmptyParam = false;
                    bool hasDebugChecks = true;
                    bool hasAutoPipelineParam = false;
                    bool hasPortWidthParam = false;
                    bool hasTransactionalParams = false;

                    size_t widthParameter = GetClampedRegisterWidth(physicalFifoWidth);

                    ModuleInstanceHelper moduleInstance(*this, GetUnknownLocation());

                    if (isReorderBuffer)
                    {
                        assert(regDesc.Fifo()._transactionSize == 0);

                        // Fifo encoded is not supported with reorder buffers
                        // Because the reorder buffer interprets some of the
                        // input data
                        assert(!FifoSupportsEncoding(regDesc));

                        moduleInstance.SetModuleName("KanagawaReorderBuffer");
                    }
                    else if (regDesc.Fifo()._implementation == FifoImplementation::TwoRegister)
                    {
                        assert(regDesc.Fifo()._transactionSize == 0);

                        // Used when almost_full is not needed - 2-deep fifo implemented in registers
                        moduleInstance.SetModuleName("KanagawaTwoRegisterFifo");
                    }
                    else if (regDesc.Fifo()._implementation == FifoImplementation::InternalBuffer)
                    {
                        moduleInstance.SetModuleName("KanagawaInternalBufferFifo");

                        hasWriteDelayParam = true;

                        hasTransactionalParams = true;
                    }
                    else if (isDualClock)
                    {
                        assert(regDesc.Fifo()._implementation == FifoImplementation::Default);
                        assert(regDesc.Fifo()._transactionSize == 0);

                        moduleInstance.SetModuleName("KanagawaHALDualClockFifo");

                        // Does not support overflow/underflow error signals
                        hasDebugChecks = false;
                    }
                    else if (regDesc.Fifo()._implementation == FifoImplementation::CrossRegion)
                    {
                        assert(!isDualClock); // Not supported yet
                        assert(regDesc.Fifo()._transactionSize == 0);

                        moduleInstance.SetModuleName("KanagawaCrossRegionFifo");

                        hasWriteDelayParam = true;

                        hasAlmostEmptyParam = true;

                        hasAutoPipelineParam = true;

                        // Allow width to be zero for this module only, which will cause it to forgo
                        // instantiating the whole FIFO macro (including its underlying 1-bit wide but
                        // unconnected RAM) in favour of inserting just the supporting control logic
                        if (0 == physicalFifoWidth)
                        {
                            assert(widthParameter == 1);
                            widthParameter = 0;
                        }

                        // Physical port width is set by the PORT_WIDTH parameter
                        hasPortWidthParam = true;
                    }
                    else
                    {
                        assert(regDesc.Fifo()._implementation == FifoImplementation::Default);

                        moduleInstance.SetModuleName("KanagawaWriteDelayFifo");

                        hasWriteDelayParam = true;

                        hasAlmostEmptyParam = true;

                        hasAutoPipelineParam = true;

                        // Physical port width is set by the PORT_WIDTH parameter
                        hasPortWidthParam = true;

                        hasTransactionalParams = true;

                        // Allow width to be zero for this module only, which will cause it to forgo
                        // instantiating the whole FIFO macro (including its underlying 1-bit wide but
                        // unconnected RAM) in favour of inserting just the supporting control logic
                        if (0 == physicalFifoWidth)
                        {
                            assert(widthParameter == 1);
                            widthParameter = 0;
                        }
                    }

                    if (hasTransactionalParams)
                    {
                        bool isTransactional = regDesc.Fifo()._transactionSize > 0;

                        // Bit which indicates end of transaction
                        size_t transactionBitOffset = isTransactional ? regDesc.Fifo()._transactionBitOffset : 0;
                        // Translate transaction offset to encoded bit
                        if (isTransactional && FifoSupportsEncoding(regDesc))
                        {
                            const FifoCode& fifoCode = regDesc.Fifo()._code;

                            for (const FifoCodeOutputRange& outputRange : fifoCode._outputRanges)
                            {
                                // Start and end of this encoding
                                const size_t rangeStart = outputRange._decodedOffset;
                                const size_t rangeEnd = rangeStart + outputRange._width - 1;

                                // If transaction bit is in this range
                                if (transactionBitOffset >= rangeStart && transactionBitOffset <= rangeEnd)
                                {
                                    assert(outputRange._width == 1);

                                    if (outputRange._isLiteral)
                                    {
                                        assert(outputRange._literal == 1);
                                        // Disable transactional if [[last]] is constant true
                                        isTransactional = false;
                                        transactionBitOffset = 0;
                                        break;
                                    }
                                    else
                                    {
                                        // Perform translation
                                        transactionBitOffset = transactionBitOffset - outputRange._decodedOffset +
                                                               outputRange._encodedOffset;
                                        break;
                                    }
                                }
                            }
                        }

                        // Parameters for store-and-forward behavior for transactions
                        moduleInstance.AddBoolParameter("IS_TRANSACTIONAL", isTransactional);
                        moduleInstance.AddU64Parameter("END_TRANSACTION_OFFSET", transactionBitOffset);
                    }

                    {
                        const size_t depth = regDesc.Fifo()._depth;

                        moduleInstance.AddU64Parameter("WIDTH", widthParameter);

                        if (hasPortWidthParam)
                        {
                            moduleInstance.AddU64Parameter("PORT_WIDTH", widthParameter == 0 ? 1 : widthParameter);
                        }
                        else
                        {
                            assert(widthParameter > 0);
                        }

                        if (hasWriteDelayParam)
                        {
                            moduleInstance.AddU64Parameter("WRITE_DELAY", regDesc.Fifo()._writeDelay);
                        }

                        if (hasAlmostEmptyParam)
                        {
                            moduleInstance.AddU64Parameter("ALMOSTEMPTY_VAL", GetAlmostEmptyDepth());
                        }

                        if (hasAutoPipelineParam)
                        {
                            moduleInstance.AddU64Parameter("MIN_WRITE_DELAY", regDesc.Fifo()._minWriteDelay);

                            moduleInstance.AddStringParameter("AUTO_PIPELINE_GROUP", fifoName);
                        }

                        if (isReorderBuffer)
                        {
                            // The reorder buffer hardware assumes that the slot ID width will be
                            // exactly 1 bit wider than the depth of the reorder buffer
                            assert(regDesc.Fifo()._reorderBuffer._slotWidth == (regDesc.Fifo().LogDepth() + 1));

                            // RTL implementation assumes depth is  power of 2
                            assert(IsPow2(regDesc.Fifo()._depth));

                            moduleInstance.AddU64Parameter("DEPTH", regDesc.Fifo()._depth);
                            moduleInstance.AddU64Parameter("SLOT_ID_OFFSET", regDesc.Fifo()._reorderBuffer._slotOffset);
                            moduleInstance.AddU64Parameter("SLOT_ID_WIDTH", regDesc.Fifo()._reorderBuffer._slotWidth);
                            moduleInstance.AddU64Parameter("USEDW_WIDTH", regDesc.Fifo().LogDepth() + 1);
                            moduleInstance.AddStringParameter("DEVICE_FAMILY", GetCodeGenDeviceConfig()._halDeviceFamily);
                        }
                        else
                        {
                            if (isDualClock)
                            {
                                moduleInstance.AddU64Parameter("USEDW_WIDTH", regDesc.Fifo().LogDepth() + 1);
                            }
                            moduleInstance.AddU64Parameter("DEPTH", regDesc.Fifo().RoundedDepth());
                            moduleInstance.AddU64Parameter("ALMOSTFULL_ENTRIES", regDesc.Fifo()._almostFullSlots);
                            moduleInstance.AddBoolParameter("USE_LUTRAM", ShouldFifoUseLutRam(_program, i));
                        }
                    }

                    const std::string portWidthParamName = hasPortWidthParam ? "PORT_WIDTH" : "WIDTH";

                    moduleInstance.SetInstanceName(fifoName);

                    // only dump FIFO signal into json when the FIFO is write-delay-FIFO
                    JsonValue jsonFifo = JsonValue::CreateObject();
                    JsonValue jsonTopLevelNets = JsonValue::CreateArray();
                    if (regDesc.Fifo()._implementation == FifoImplementation::Default && !isDualClock)
                    {
                        JsonValue jsonNets = JsonValue::CreateArray();
                        jsonFifo.AddMember("module", JsonValue("KanagawaWriteDelayFifo"));
                        jsonFifo.AddMember("instance", _rtlMap.SerializeString(fifoName));
                        AddNetToJson("wrreq", 1, jsonNets);
                        AddNetToJson("full", 1, jsonNets);
                        AddNetToJson("overflow_out", 1, jsonNets);
                        AddNetToJson("rdreq", 1, jsonNets);
                        AddNetToJson("empty", 1, jsonNets);
                        AddNetToJson("underflow_out", 1, jsonNets);
                        // FIFO module will instantiate KanagawaWriteDelayFifo when width > 0
                        // there are 2 levels of hierarchies:genblk1.fifo and genblk1.fifo.fifo_inst
                        // We only dump "data" and "q" since only these two need to be handled by exclusion generation
                        const size_t usedw_width = regDesc.Fifo().LogDepth() + 1;
                        if (0 < physicalFifoWidth)
                        {
                            AddNetToJson("data", physicalFifoWidth, jsonNets);
                            AddNetToJson("q", physicalFifoWidth, jsonNets);
                            AddNetToJson("q_internal", physicalFifoWidth, jsonNets);
                            AddNetToJson("genblk1.data_delayed", physicalFifoWidth, jsonNets);
                            AddNetToJson("genblk1.fifo.data", physicalFifoWidth, jsonNets);
                            AddNetToJson("genblk1.fifo.q", physicalFifoWidth, jsonNets);
                            AddNetToJson("genblk1.fifo.usedw", usedw_width, jsonNets);
                            AddNetToJson("genblk1.fifo.fifo_inst.data", physicalFifoWidth, jsonNets);
                            AddNetToJson("genblk1.fifo.fifo_inst.q", physicalFifoWidth, jsonNets);
                            AddNetToJson("genblk1.fifo.fifo_inst.usedw", usedw_width, jsonNets);
                            AddNetToJson("genblk1.fifo.fifo_inst.gen_with_mux.ptrs.almost_full_nxt", 1, jsonNets);
                            AddNetToJson("genblk1.fifo.fifo_inst.gen_with_mux.ptrs.full_nxt", 1, jsonNets);
                            AddNetToJson("genblk1.fifo.fifo_inst.gen_with_mux.ptrs.usedw_ff", usedw_width, jsonNets);
                            AddNetToJson("genblk1.fifo.fifo_inst.gen_with_mux.ptrs.usedw_nxt", usedw_width, jsonNets);
                            AddNetToJson("genblk1.fifo.fifo_inst.gen_with_mux.ptrs.usedw_out", usedw_width, jsonNets);
                        }
                        else
                        {
                            AddNetToJson("genblk1.fifo_ptrs.almost_full_nxt", 1, jsonNets);
                            AddNetToJson("genblk1.fifo_ptrs.full_nxt", 1, jsonNets);
                            AddNetToJson("genblk1.fifo_ptrs.usedw_ff", usedw_width, jsonNets);
                            AddNetToJson("genblk1.fifo_ptrs.usedw_nxt", usedw_width, jsonNets);
                            AddNetToJson("genblk1.fifo_ptrs.usedw_out", usedw_width, jsonNets);
                        }
                        jsonFifo.AddMember("nets", jsonNets);
                    }

                    {
                        const std::string resetReplica = getResetString(true);

                        if (isDualClock)
                        {
                            moduleInstance.AddPort("wrclk", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                                   GetClockString(regDesc.Fifo()._writeClock));
                            moduleInstance.AddPort("wrrst", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                                   GetResetString(regDesc.Fifo()._writeClock, resetReplica));

                            moduleInstance.AddPort("rdclk", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                                   GetClockString(regDesc.Fifo()._readClock));
                            moduleInstance.AddPort("rdrst", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                                   GetResetString(regDesc.Fifo()._readClock, resetReplica));
                        }
                        else
                        {
                            moduleInstance.AddPort("clock", circt::hw::ModulePort::Direction::Input, GetClockType(),
                                                   "clk");
                            moduleInstance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                                   resetReplica);
                        }

                        const auto addPortAndNetToJson = [&](const std::string& name,
                                                             const circt::hw::ModulePort::Direction direction,
                                                             const mlir::Type& type, const std::string& value)
                        {
                            moduleInstance.AddPort(name, direction, type, value);
                            AddNetToJson(value, GetMlirTypeWidth(moduleInstance.ConcreteType(type)), jsonTopLevelNets);
                        };

                        const std::string sf = "fifo_data_" + std::to_string(iNorm);
                        if (hasDebugChecks)
                        {
                            addPortAndNetToJson("overflow_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                                sf + ".overflow");

                            addPortAndNetToJson("underflow_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                                sf + ".underflow");
                        }
                        addPortAndNetToJson("empty", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                            sf + ".empty");
                        addPortAndNetToJson("rdreq", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                            sf + ".rden");
                        if (isDualClock)
                        {
                            addPortAndNetToJson("almost_full", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                                sf + ".almost_full");
                        }
                        else
                        {
                            addPortAndNetToJson("full", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                                sf + ".almost_full");
                        }
                        addPortAndNetToJson("wrreq", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                            sf + ".wren");

                        if (FifoSupportsEncoding(regDesc))
                        {
                            if (physicalFifoWidth > 0)
                            {
                                addPortAndNetToJson("data", circt::hw::ModulePort::Direction::Input,
                                                    moduleInstance.GetParameterizedIntegerType(portWidthParamName),
                                                    sf + ".encoded_data_in");
                                AddNetToJson(sf + ".data_in", physicalFifoWidth, jsonTopLevelNets);

                                addPortAndNetToJson("q", circt::hw::ModulePort::Direction::Output,
                                                    moduleInstance.GetParameterizedIntegerType(portWidthParamName),
                                                    sf + ".encoded_data_out");
                                AddNetToJson(sf + ".data_out", physicalFifoWidth, jsonTopLevelNets);
                            }
                            else
                            {
                                moduleInstance.AddPort("data", circt::hw::ModulePort::Direction::Input,
                                                       moduleInstance.GetParameterizedIntegerType(portWidthParamName),
                                                       "'0");
                                moduleInstance.AddPort("q", circt::hw::ModulePort::Direction::Output,
                                                       moduleInstance.GetParameterizedIntegerType(portWidthParamName));
                            }
                        }
                        else
                        {
                            if (logicalFifoWidth > 0)
                            {
                                addPortAndNetToJson("data", circt::hw::ModulePort::Direction::Input,
                                                    moduleInstance.GetParameterizedIntegerType(portWidthParamName),
                                                    sf + ".data_in");
                                addPortAndNetToJson("q", circt::hw::ModulePort::Direction::Output,
                                                    moduleInstance.GetParameterizedIntegerType(portWidthParamName),
                                                    sf + ".data_out");
                            }
                            else
                            {
                                moduleInstance.AddPort("data", circt::hw::ModulePort::Direction::Input,
                                                       moduleInstance.GetParameterizedIntegerType(portWidthParamName),
                                                       "'0");
                                moduleInstance.AddPort("q", circt::hw::ModulePort::Direction::Output,
                                                       moduleInstance.GetParameterizedIntegerType(portWidthParamName));
                            }
                        }

                        if (isReorderBuffer || regDesc.Fifo()._implementation == FifoImplementation::TwoRegister ||
                            isDualClock)
                        {
                            if (isDualClock)
                            {
                                moduleInstance.AddPort("full", circt::hw::ModulePort::Direction::Output, GetI1Type());
                            }
                            else
                            {
                                moduleInstance.AddPort("almost_empty", circt::hw::ModulePort::Direction::Output,
                                                       GetI1Type());
                                moduleInstance.AddPort("almost_full", circt::hw::ModulePort::Direction::Output,
                                                       GetI1Type());
                            }

                            if (isReorderBuffer || isDualClock)
                            {
                                moduleInstance.AddPort("usedw", circt::hw::ModulePort::Direction::Output,
                                                       moduleInstance.GetParameterizedIntegerType("USEDW_WIDTH"));
                            }
                        }
                    }

                    jsonFifo.AddMember("top_level_signals", jsonTopLevelNets);

                    // Write information describing ranges of bits in the fifo
                    JsonValue jsonRanges = JsonValue::CreateArray();

                    for (const RegisterDescription::FifoDesc::NamedRange& range : regDesc.Fifo()._namedRanges)
                    {
                        JsonValue jsonRange = JsonValue::CreateObject();

                        jsonRange.AddMember("name", _rtlMap.SerializeString(range._name));
                        jsonRange.AddMember("offset", _rtlMap.SerializeSizeT(range._offset));
                        jsonRange.AddMember("width", _rtlMap.SerializeSizeT(range._width));

                        jsonRanges.PushBack(jsonRange);
                    }

                    jsonFifo.AddMember("named_ranges", jsonRanges);

                    _jsonFifos.PushBack(jsonFifo);

                    if (!hasDebugChecks)
                    {
                        // DC FIFOs currently do not have overflow/underflow checks
                        // just set them to 0 always
                        _writer.Str() << "assign fifo_data_" << iNorm << ".overflow = 1'b0;";
                        _writer.Str() << "assign fifo_data_" << iNorm << ".underflow = 1'b0;";
                    }

                    moduleInstance.Generate();
                }
                else if (FifoType::Passthrough == regDesc.Fifo()._type)
                {
                    // logical fifo width and physical fifo width are assumed to
                    // match for passthrough fifos
                    assert(!FifoSupportsEncoding(regDesc));

                    const std::string structName = "passthrough_data_" + std::to_string(iNorm);

                    // This is used for the inputs to an exported function - where no fifo is needed
                    // The interface is rdy/valid rather than full/wren
                    _writer.Str() << "struct packed {";

                    {
                        AutoIndent autoIndent2(_writer);

                        _writer.Str() << "logic rdy_int;"; // internal ready
                        _writer.Str() << "logic rdy_ext;"; // internal ready combind with reset and startup completed
                        _writer.Str() << "logic valid;";

                        if (logicalFifoWidth > 0)
                        {
                            _writer.Str() << "logic [" << logicalFifoWidth - 1 << ":0] data;";

                            topLevelSignalNames.emplace_back(structName, "data");
                        }

                        _writer.Str() << "logic underflow;";
                    }

                    _writer.Str() << "} " << structName << ";";

                    // Ready/valid protocol does not allow for underflow
                    _writer.Str() << "assign passthrough_data_" << iNorm << ".underflow = 1'b0;";

                    // No need to combine reset signal into getResetString
                    // becuase the reset signal is already incorporated into rdy_int
                    const std::string resetReplica = getResetString(false);

                    const std::string rdyModifier =
                        resetReplica.empty() ? std::string("") : (std::string(" & !(") + resetReplica + ")");

                    _writer.Str() << "assign passthrough_data_" << iNorm << ".rdy_ext = "
                                  << "passthrough_data_" << iNorm << ".rdy_int" << rdyModifier << ";";

                    AddGlobalDebugSignal(structName, "underflow");
                }
                else if (FifoType::PassthroughRegistered == regDesc.Fifo()._type)
                {
                    // logical fifo width and physical fifo width are assumed to
                    // match for passthrough fifos
                    assert(!FifoSupportsEncoding(regDesc));

                    const std::string structName = "passthrough_data_" + std::to_string(iNorm);

                    const std::string resetReplica = getResetString(true);

                    // This is used for the inputs to an exported function - where no fifo is needed
                    // The interface is rdy/valid rather than full/wren
                    _writer.Str() << "struct packed {";

                    {
                        AutoIndent autoIndent2(_writer);

                        _writer.Str() << "logic valid_next;";
                        _writer.Str() << "logic valid_ff;";

                        if (logicalFifoWidth > 0)
                        {
                            _writer.Str() << "logic [" << logicalFifoWidth - 1 << ":0] data_next;";
                            _writer.Str() << "logic [" << logicalFifoWidth - 1 << ":0] data_ff;";

                            topLevelSignalNames.emplace_back(structName, "data_next");
                            topLevelSignalNames.emplace_back(structName, "data_ff");
                        }
                    }

                    _writer.Str() << "} " << structName << ";";

                    {
                        AutoSectionRAII alwaysFFBlock(_writer, true, "always_ff @(posedge clk) begin", "end");

                        _writer.Str() << "passthrough_data_" << iNorm << ".valid_ff <= " << resetReplica
                                      << " ? 1'b0 : passthrough_data_" << iNorm << ".valid_next;";

                        if (logicalFifoWidth > 0)
                        {
                            _writer.Str() << "passthrough_data_" << iNorm << ".data_ff <= passthrough_data_" << iNorm
                                          << ".data_next;";
                        }
                    }
                }
                else if (FifoType::PassthroughUnregistered == regDesc.Fifo()._type)
                {
                    // logical fifo width and physical fifo width are assumed to
                    // match for passthrough fifos
                    assert(!FifoSupportsEncoding(regDesc));

                    const std::string structName = "passthrough_data_" + std::to_string(iNorm);

                    // This is used for inputs and outputs of fixed-latency functions
                    _writer.Str() << "struct packed {";

                    {
                        AutoIndent autoIndent2(_writer);

                        _writer.Str() << "logic valid;";

                        if (logicalFifoWidth > 0)
                        {
                            _writer.Str() << "logic [" << logicalFifoWidth - 1 << ":0] data;";

                            topLevelSignalNames.emplace_back(structName, "data");
                        }
                    }

                    _writer.Str() << "} " << structName << ";";
                }
                else if (FifoType::ContextSaverCaller == regDesc.Fifo()._type)
                {
                    const std::string structName = "context_saver_data_" + std::to_string(iNorm);

                    if (logicalFifoWidth > 0)
                    {
                        topLevelSignalNames.emplace_back(structName, "caller_data");
                        topLevelSignalNames.emplace_back(structName, "output_caller_data");
                    }
                }

                for (const auto& signalName : topLevelSignalNames)
                {
                    EmitFifoTopLevelConstants(i, signalName.first, signalName.second);
                }
            }
        }

        assert(startupAndMemInitCompletedReplicaIndex == startupAndMemInitCompletedReplicaCount);

        const size_t autoPipelineCrossRegionStages = GetCodeGenConfig()._autoPipelineCrossRegion;

        for (const Function* const function : _program._externFunctions)
        {
            if (!function->IsAsync())
            {
                assert(1 == function->_start->_inputFifoCount);
                const size_t inputFifoIndex = function->_start->_inputFifoIndices[0];
                const size_t inputFifoIndexNorm = _fifoNamer.GetNormFifoIndex(inputFifoIndex);

                const RegisterDescription& inputRegDesc = _program._registerTable[inputFifoIndex];
                assert(RegisterType::Fifo == inputRegDesc._type);

                // Internal fifo that holds call index to route return values
                const size_t callIndexOffset = function->_syncExtern._callIndexStartOffset;
                // Number of bits required to represent the call site indices
                const size_t callSiteIndexWidth = function->_syncExtern._callIndexWidth;

                // Get the root name for this router
                const std::string rootExternReturnRouterName = GetExternReturnRouterRootName(function);
                // Get the number of output ports
                const size_t numOutputPorts = function->_syncExtern._returnDesc.size();
                // Get the data width of the output ports, which should match the width of the input data
                const size_t dataInputFifoIndex = function->_syncExtern._returnDesc[0]._fifoIndex;
                const RegisterDescription& dataInputRegDesc = _program._registerTable[dataInputFifoIndex];
                const size_t dataOutputFifoWidth = GetClampedRegisterWidth(dataInputRegDesc._width);
                // Get the name of the return value fifo
                const bool inExternalClassInstance = (function->_externClassInstance != nullptr);
                const std::string rootReturnValueSourceName = inExternalClassInstance
                                                                  ? GetExternalClassInstanceSignalPrefix(function)
                                                                  : GetFunctionCombinedName(function);
                // Suffixes for external modules
                const std::string inString(inExternalClassInstance ? "" : "_in");
                const std::string outString(inExternalClassInstance ? "" : "_out");

                const size_t writeClockIndex = 0;
                const size_t readClockIndex = 0;

                const std::string resetReplica = GetResetReplica();

                const FunctionNode* const functionNode = function->_functionNode;
                const size_t dataReturnWidth = functionNode->GetReturnType()->GetBitWidth();

                // Declare Verilog interfaces to the external return router
                _writer.Str() << "pd_fifo_intf #( .DATA_WIDTH(" << callSiteIndexWidth << ") ) "
                              << rootExternReturnRouterName << "_input_index_intf();";
                _writer.Str() << "pd_fifo_intf #( .DATA_WIDTH(" << dataOutputFifoWidth << ") ) "
                              << rootExternReturnRouterName << "_input_data_intf();";
                _writer.Str() << "pd_fifo_intf #( .DATA_WIDTH(" << dataOutputFifoWidth << ") ) "
                              << rootExternReturnRouterName << "_switch_output_intfs[" << numOutputPorts << "]();";

                if (function->IsExportClassInterface() && (autoPipelineCrossRegionStages > 0))
                {
                    // The called function is either a public method of an export class
                    // or is a function hooked up to an export class callback.
                    // Either way, buffer return data in a cross-region FIFO
                    // because this data cross the export class boundary
                    const std::string fifoName = rootExternReturnRouterName + FifoNamer::CrossRegionFifoSuffix;

                    // A width of 0 is acceptable for this fifo type
                    const size_t portWidth = dataReturnWidth == 0 ? 1 : dataReturnWidth;

                    _writer.Str() << "logic [" << (portWidth - 1) << ":0] " << fifoName << "_data_in;";
                    _writer.Str() << "logic " << fifoName << "_full;";

                    _writer.Str() << "logic " << fifoName << "_empty;";
                    _writer.Str() << "logic " << fifoName << "_rden;";
                    _writer.Str() << "logic [" << (portWidth - 1) << ":0] " << fifoName << "_data_out;";

                    ModuleInstanceHelper fifo(*this, LocationToCirctLocation(function->_start->_location));

                    fifo.SetModuleName("KanagawaCrossRegionFifo");

                    size_t depth = 0;
                    size_t almostFullSlots = 0;
                    size_t writeDelay = 0;
                    size_t minWriteDelay = 0;

                    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

                    ComputeFifoSize(
                        GetCodeGenDeviceConfig()._minFifoDepth, GetCodeGenDeviceConfig()._minDualClockFifoDepth,
                        GetCodeGenDeviceConfig()._minAlmostFullDepth, codeGenConfig._additionalLatency,
                        codeGenConfig.GetMinCrossRegionFifoWriteDelay(), codeGenConfig._autoPipelineCrossRegion, 0, 1,
                        0, 0, 0, false, 0, std::numeric_limits<size_t>::max(), std::numeric_limits<size_t>::max(),
                        false, 0, depth, almostFullSlots, writeDelay, minWriteDelay);

                    const size_t logDepth = Log2RoundUp(depth);

                    fifo.AddU64Parameter("WIDTH", dataReturnWidth);
                    fifo.AddU64Parameter("PORT_WIDTH", portWidth);
                    fifo.AddU64Parameter("WRITE_DELAY", autoPipelineCrossRegionStages);
                    fifo.AddU64Parameter("ALMOSTEMPTY_VAL", GetAlmostEmptyDepth());
                    fifo.AddU64Parameter("MIN_WRITE_DELAY", codeGenConfig.GetMinCrossRegionFifoWriteDelay());
                    fifo.AddStringParameter("AUTO_PIPELINE_GROUP", fifoName);
                    fifo.AddU64Parameter("DEPTH", depth);
                    fifo.AddU64Parameter("ALMOSTFULL_ENTRIES", 0);
                    fifo.AddU64Parameter("USE_LUTRAM", 1);

                    fifo.SetInstanceName(fifoName);

                    // Data is always on the output clock
                    fifo.AddPort("clock", circt::hw::ModulePort::Direction::Input, GetClockType(),
                                 GetClockString(readClockIndex));
                    fifo.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 GetResetString(readClockIndex, resetReplica));

                    fifo.AddPort("overflow_out", circt::hw::ModulePort::Direction::Output, GetI1Type());
                    fifo.AddPort("underflow_out", circt::hw::ModulePort::Direction::Output, GetI1Type());
                    fifo.AddPort("empty", circt::hw::ModulePort::Direction::Output, GetI1Type(), fifoName + "_empty");
                    fifo.AddPort("rdreq", circt::hw::ModulePort::Direction::Input, GetI1Type(), fifoName + "_rden");
                    fifo.AddPort("full", circt::hw::ModulePort::Direction::Output, GetI1Type(), fifoName + "_full");
                    fifo.AddPort("wrreq", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 rootReturnValueSourceName + "_valid" + inString + " && " + rootReturnValueSourceName +
                                     "_rdy" + outString);
                    fifo.AddPort("data", circt::hw::ModulePort::Direction::Input,
                                 fifo.GetParameterizedIntegerType("PORT_WIDTH"), fifoName + "_data_in");
                    fifo.AddPort("q", circt::hw::ModulePort::Direction::Output,
                                 fifo.GetParameterizedIntegerType("PORT_WIDTH"), fifoName + "_data_out");
                    fifo.Generate();

                    _writer.Str() << "assign " << mod.AssignPortOptional(rootReturnValueSourceName + "_rdy" + outString)
                                  << " = !" << fifoName << "_full;";
                    _writer.Str() << "assign " << fifoName << "_rden = " << rootExternReturnRouterName
                                  << "_input_data_intf.ready && !" << fifoName << "_empty;";
                    _writer.Str() << "assign " << rootExternReturnRouterName << "_input_data_intf.valid = " << fifoName
                                  << "_rden;";

                    if (dataReturnWidth > 0)
                    {
                        _writer.Str() << "assign " << fifoName << "_data_in = " << rootReturnValueSourceName
                                      << "_result" << inString << ";";

                        _writer.Str() << "assign " << rootExternReturnRouterName
                                      << "_input_data_intf.data = " << fifoName << "_data_out;";
                    }
                    else
                    {
                        _writer.Str() << "assign " << fifoName << "_data_in = '0;";
                    }
                }
                else
                {
                    // Make data input interface connections
                    _writer.Str() << "assign " << rootExternReturnRouterName
                                  << "_input_data_intf.valid = " << rootReturnValueSourceName << "_valid" << inString
                                  << ";";

                    _writer.Str() << "assign " << mod.AssignPortOptional(rootReturnValueSourceName + "_rdy" + outString)
                                  << " = " << rootExternReturnRouterName << "_input_data_intf.ready;";

                    if (dataReturnWidth > 0)
                        _writer.Str() << "assign " << rootExternReturnRouterName
                                      << "_input_data_intf.data = " << rootReturnValueSourceName << "_result"
                                      << inString << ";";
                }

                // In some cases, the external return router is used as a signaling mechanism to indicate when a DDR
                // write has been accepted, and
                //  in those cases, there is no data signal, so I'll just tie it off here.  The dataOutputFifoWidth will
                //  be set to 1 in those cases, but the dataReturnWidth will be 0.
                if (dataReturnWidth == 0)
                    _writer.Str() << "assign " << rootExternReturnRouterName << "_input_data_intf.data = 0;";

                // Make index input interface connections, the *_input_index_intf.ready signal is ignored because it is
                // assumed that the index
                //  FIFO is big enough that it will never overflow.
                _writer.Str() << "assign " << rootExternReturnRouterName << "_input_index_intf.valid = fifo_data_"
                              << inputFifoIndexNorm << ".wren;";
                _writer.Str() << "assign " << rootExternReturnRouterName << "_input_index_intf.data = fifo_data_"
                              << inputFifoIndexNorm << ".data_in[" << callIndexOffset + callSiteIndexWidth - 1 << ":"
                              << callIndexOffset << "];";

                // The data output connections are made below in ConnectExportFifos()

                _writer.Str() << "KanagawaExternReturnRouter";
                _writer.Str() << "#(";

                {
                    AutoIndent autoIndent2(_writer);

                    const bool isDualClock = (writeClockIndex != readClockIndex);
                    const size_t logDepth = function->_syncExtern.GetLogDepth();
                    const size_t depth = 1ull << logDepth;
                    // The call site indices are not necessarily contiguous numbers due to compiler optimizations.  This
                    // parameter tells
                    //  how many bits are necessary to represent all possible call site index values (4 bits for 13
                    //  values, etc).  By extension, it also indicates the width of the call site index field in the
                    //  hardware.
                    // Build the port map string, which tells the KanagawaExternReturnRouter which _callSiteIndex
                    // should be used to reference
                    //  which switch output port.  In SetupSynchronousExterns() the call return sites were sorted to be
                    //  in the same relative order as the command processors so that the place & route tools would have
                    //  an easier time routing the command processor I/O bus alongside the data call site return bus.
                    //  The function->_syncExtern._returnDesc vector has the correctly sorted return site order, and the
                    //  externReturnDesc._callSiteIndex of each entry indicates the call site index that each port
                    //  should respond too.
                    // The port map string itself is an encoded version of that mapping, to make things faster in the
                    // hardware.  Since the indices
                    //  are not in placement order, and therefore are not a linear map, a lookup table is used to
                    //  translate from the call site index to the linear map that the hardware uses, which is supposed
                    //  to be in placement order, or at least it matches the order that is used by the command
                    //  processors so that the routing paths are consistent.
                    // In addition, port map output values are decremented by 1 so that underflow can be used by the
                    // hardware for matching.
                    const unsigned int numPossibleCallSiteValues = (1UL << callSiteIndexWidth);
                    std::vector<unsigned int> portIndexMapValues(numPossibleCallSiteValues);
                    // Set all values to max so that unassigned index values won't match (unless all index positions are
                    // used)
                    portIndexMapValues.assign(portIndexMapValues.size(), numPossibleCallSiteValues - 1);
                    // It could happen that fewer bits are needed to represent the match_count within the
                    // KanagawaExternReturnRouter than are
                    //  needed to represent the call site indices because the call site indices can be optimized away by
                    //  the compiler. Add an extra bit for arithmetic underflow in the uppermost position.
                    const size_t errPortOffsetWidth = ((size_t)ceil(log2(numOutputPorts))) + 1;
                    // function->_syncExtern._returnDesc is a list that is sorted in the order that the command
                    // processors appear in the primary
                    //  I/O chain.  Each element contains the call site index that is used to reference that position.
                    // Fill in all index positions that are used with their offset in the hardware chain (minus 1 for
                    // the underflow flag)
                    int iPosition = -1;
                    for (auto externReturnDesc_it = function->_syncExtern._returnDesc.begin();
                         externReturnDesc_it != function->_syncExtern._returnDesc.end(); externReturnDesc_it++)
                    {
                        portIndexMapValues[externReturnDesc_it->_callSiteIndex] = (unsigned int)(iPosition++);
                    }
                    // Use the port map integer values to build the PORT_MAP string for hardware, using the appropriate
                    // bit width.
                    std::string portIndexMapString = "{ ";
                    unsigned int dwPortIndexMapMask = (1ul << errPortOffsetWidth) - 1;
                    for (auto portIndexMap_it = portIndexMapValues.begin(); portIndexMap_it != portIndexMapValues.end();
                         portIndexMap_it++)
                    {
                        std::stringstream hex_offset;
                        hex_offset << std::hex << ((*portIndexMap_it) & dwPortIndexMapMask);
                        portIndexMapString += std::to_string(errPortOffsetWidth) + "'h" + hex_offset.str();
                        portIndexMapString += (portIndexMap_it + 1 == portIndexMapValues.end()) ? " }" : ", ";
                    }

                    _writer.Str() << ".DUAL_CLOCK(" << (isDualClock ? 1 : 0) << "),";
                    _writer.Str() << ".LOG_DEPTH(" << logDepth << "),";
                    _writer.Str() << ".INDEX_WIDTH(" << callSiteIndexWidth << "),";
                    _writer.Str() << ".DATA_WIDTH(" << dataOutputFifoWidth << "),";
                    _writer.Str() << ".USE_LUTRAM(" << (function->_syncExtern._useLutRam ? 1 : 0) << "),";
                    _writer.Str() << ".NUM_OUTPUT_PORTS(" << numOutputPorts << "),";
                    _writer.Str() << ".PORT_INDEX_MAP_VALUE_WIDTH(" << errPortOffsetWidth << "),";
                    _writer.Str() << ".PORT_INDEX_MAP_DEPTH(" << numPossibleCallSiteValues << "),";
                    _writer.Str() << ".PORT_INDEX_MAP(" << portIndexMapString << ")";
                }

                _writer.Str() << ")";
                _writer.Str() << rootExternReturnRouterName;
                _writer.Str() << "(";

                {
                    AutoIndent autoIndent2(_writer);

                    // ExternReturnRouter for extern modules can run on 2 clocks
                    // Call index is written into the ExternReturnRouter on clock0
                    // Call index is read out on the extern module clock
                    _writer.Str() << ".input_clk(" << GetClockString(writeClockIndex) << "),";
                    _writer.Str() << ".input_rst(" << GetResetString(writeClockIndex, resetReplica) << "),";
                    _writer.Str() << ".input_index_intf(" << rootExternReturnRouterName << "_input_index_intf),";

                    _writer.Str() << ".output_clk(" << GetClockString(readClockIndex) << "),";
                    _writer.Str() << ".output_rst(" << GetResetString(readClockIndex, resetReplica) << "),";
                    _writer.Str() << ".input_data_intf(" << rootExternReturnRouterName << "_input_data_intf),";
                    _writer.Str() << ".output_switch_node_intfs(" << rootExternReturnRouterName
                                  << "_switch_output_intfs)";
                }

                _writer.Str() << ");";
            }
        }
    }

    void InstantiateFifoMergers()
    {
        for (const FIFOMerger& fifoMerger : _program._fifoMergers)
        {
            assert(fifoMerger._sources.size() > 1);

            const size_t dstFifoId = fifoMerger._dest;

            const size_t dstFifoIdNorm = _fifoNamer.GetNormFifoIndex(dstFifoId);

            const RegisterDescription& outputDesc = _program._registerTable[dstFifoId];
            assert(outputDesc._type == RegisterType::Fifo);

            const size_t fanIn = fifoMerger._sources.size();

            // will be 1 when outputDesc._width == 0
            const size_t dataWidth = GetClampedRegisterWidth(outputDesc._width);

            const bool hasData = outputDesc._width > 0;

            const std::string functionName = FixupString(fifoMerger._dstFunction->_name);

            const auto getIntermediateFifoName = [this, dstFifoId, &fifoMerger,
                                                  functionName](const size_t i) -> std::string
            {
                const size_t dstFifoIdNorm = this->_fifoNamer.GetNormFifoIndex(dstFifoId);
                return std::string("arb_") + functionName + "_" + std::to_string(dstFifoIdNorm) +
                       std::string("_intermediate_") + std::to_string(i);
            };

            // The first instance consumes 2 inputs
            // each other instance in the chain consumes 1 input
            const size_t numInstances = fanIn - 1;

            // Declare intermediate fifos that connect the links in the chain
            for (size_t i = 0; i < (numInstances - 1); i++)
            {
                const std::string intermediateFifoName = getIntermediateFifoName(i);

                _writer.Str() << "logic [" << dataWidth - 1 << ":0] " << intermediateFifoName << "_data_in;";
                _writer.Str() << "logic " << intermediateFifoName << "_wren;";
                _writer.Str() << "logic " << intermediateFifoName << "_full;";

                _writer.Str() << "logic [" << dataWidth - 1 << ":0] " << intermediateFifoName << "_data_out;";
                _writer.Str() << "logic " << intermediateFifoName << "_rden;";
                _writer.Str() << "logic " << intermediateFifoName << "_empty;";

                _writer.Str() << "KanagawaRegisterFifoSkid";
                _writer.Str() << "#(";
                {
                    AutoIndent autoIndent2(_writer);

                    _writer.Str() << ".WIDTH(" << dataWidth << ")";
                }
                _writer.Str() << ")";
                _writer.Str() << intermediateFifoName;
                _writer.Str() << "(";
                {
                    AutoIndent autoIndent2(_writer);

                    _writer.Str() << ".clock(clk),";
                    _writer.Str() << ".rst(" << GetResetReplica() << "),";

                    _writer.Str() << ".wrreq(" << intermediateFifoName << "_wren),";
                    _writer.Str() << ".data(" << intermediateFifoName << "_data_in),";
                    _writer.Str() << ".full(" << intermediateFifoName << "_full),";

                    _writer.Str() << ".rdreq(" << intermediateFifoName << "_rden),";
                    _writer.Str() << ".empty(" << intermediateFifoName << "_empty),";
                    _writer.Str() << ".q(" << intermediateFifoName << "_data_out)";
                }

                _writer.Str() << ");";
            }

            size_t baseSource = 0;
            // Each 2:1 arbiter has a reconfigurable RATIO parameter to specify its port #0's priority of 1/RATIO
            // When chaining N-1 2:1 arbiters for N requests to get each requester the same priority of 1/N
            // (1)The last level of arbiter has prority of 1/N on port #0, and its port #1 with priority of N-1/N
            // connects
            //    to the output of previous level arbiter.(RATIO = N)
            // (2)The (N-2)-th level of arbiter has priority of 1/N on port #0, and its port #1 with priority of N-2/N
            // connects
            //    to the output of (N-3)-th level of arbiter.(RATIO = N-1)
            // ....
            // (N-2)The 2nd level arbiter has 1/N priority on port #0, and its ports #1 with priority of 2/N connects to
            // the
            //    output of the 1st level arbiter.(RATIO = 3)
            // (N-1)The 1st level arbiter has the equal priority of 1/N on port #0 and #1.(RATIO = 2)
            size_t ratioOnPort0 = 2;

            for (size_t i = 0; i < numInstances; i++)
            {
                const bool isFirst = (i == 0);
                const bool isLast = (i == (numInstances - 1));

                std::string inputData[2];
                std::string inputEmpty[2];
                std::string inputRden[2];

                // DC does not allow an unpacked N-bit input or output to be assigned to two separate signals directly.
                // (e.g. .rden_out('{inputRden[1], inputRden[0]}))
                // Because of this, we have to declare additional N-bit[2] signals first, then assign them later.
                _writer.Str() << "logic [1:0][" << dataWidth - 1 << ":0] arbiter_" << functionName << "_"
                              << dstFifoIdNorm << "_data_in_" << i << ";";
                _writer.Str() << "logic [1:0] arbiter_" << functionName << "_" << dstFifoIdNorm << "_empty_in_" << i
                              << ";";
                _writer.Str() << "logic [1:0] arbiter_" << functionName << "_" << dstFifoIdNorm << "_rden_out_" << i
                              << ";";

                ModuleInstanceHelper instance(*this, GetUnknownLocation());
                instance.SetModuleName("KanagawaArbitrationChainNode");
                instance.AddU64Parameter("WIDTH", dataWidth);
                instance.AddBoolParameter("IS_TRANSACTIONAL", fifoMerger._isTransactional);
                instance.AddU64Parameter("END_TRANSACTION_OFFSET", fifoMerger._transactionBitOffset);
                instance.AddU64Parameter("RATIO", ratioOnPort0++);
                instance.SetInstanceName("arbiter_" + functionName + "_" + std::to_string(dstFifoIdNorm) + "_link_" +
                                         std::to_string(i));
                instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
                instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), GetResetReplica());

                for (size_t j = 0; j < 2; j++)
                {
                    if ((j == 0) || (isFirst))
                    {
                        const size_t indexNorm = _fifoNamer.GetNormFifoIndex(fifoMerger._sources[baseSource + j]);
                        const std::string fifoDataStr = std::string("fifo_data_") + std::to_string(indexNorm);

                        inputData[j] = fifoDataStr + ".data_out";
                        inputEmpty[j] = fifoDataStr + ".empty";
                        inputRden[j] = fifoDataStr + ".rden";
                    }
                    else
                    {
                        // Input is the output of a previous chain
                        const std::string intermediateName = getIntermediateFifoName(i - 1);

                        inputData[j] = intermediateName + "_data_out";
                        inputEmpty[j] = intermediateName + "_empty";
                        inputRden[j] = intermediateName + "_rden";
                    }
                }

                instance.AddPort("data_in", circt::hw::ModulePort::Direction::Input,
                                 GetPackedArrayType(instance.GetParameterizedIntegerType("WIDTH"), 2),
                                 "arbiter_" + functionName + "_" + std::to_string(dstFifoIdNorm) + "_data_in_" +
                                     std::to_string(i));
                instance.AddPort(
                    "empty_in", circt::hw::ModulePort::Direction::Input, GetPackedArrayType(GetI1Type(), 2),
                    "arbiter_" + functionName + "_" + std::to_string(dstFifoIdNorm) + "_empty_in_" + std::to_string(i));
                instance.AddPort(
                    "rden_out", circt::hw::ModulePort::Direction::Output, GetPackedArrayType(GetI1Type(), 2),
                    "arbiter_" + functionName + "_" + std::to_string(dstFifoIdNorm) + "_rden_out_" + std::to_string(i));

                if (isLast)
                {
                    // If the fifo has 0 width then the data input was set to '0
                    // (near the instantiation of the fifo)
                    // Don't assign to data_in again
                    const size_t logicalFifoWidth = GetLogicalFifoWidth(outputDesc);

                    switch (_program._registerTable[dstFifoId].Fifo()._type)
                    {
                    case FifoType::Default:
                        if (logicalFifoWidth > 0)
                        {
                            instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output,
                                             instance.GetParameterizedIntegerType("WIDTH"),
                                             "fifo_data_" + std::to_string(dstFifoIdNorm) + ".data_in");
                        }
                        else
                        {
                            instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output,
                                             instance.GetParameterizedIntegerType("WIDTH"));
                        }

                        instance.AddPort("wren_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                         "fifo_data_" + std::to_string(dstFifoIdNorm) + ".wren");
                        instance.AddPort("full_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                         "fifo_data_" + std::to_string(dstFifoIdNorm) + ".almost_full");
                        break;

                    case FifoType::PassthroughRegistered:
                        // Output fifo does not have backpressure
                        if (logicalFifoWidth > 0)
                        {
                            instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output,
                                             instance.GetParameterizedIntegerType("WIDTH"),
                                             "passthrough_data_" + std::to_string(dstFifoIdNorm) + ".data_next");
                        }
                        else
                        {
                            instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output,
                                             instance.GetParameterizedIntegerType("WIDTH"));
                        }

                        instance.AddPort("wren_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                         "passthrough_data_" + std::to_string(dstFifoIdNorm) + ".valid_next");
                        instance.AddPort("full_in", circt::hw::ModulePort::Direction::Input, GetI1Type(), "1'b0");
                        break;

                    default:
                        assert(false);
                    }
                }
                else
                {
                    const std::string intermediateName = getIntermediateFifoName(i);

                    instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output,
                                     instance.GetParameterizedIntegerType("WIDTH"), intermediateName + "_data_in");
                    instance.AddPort("wren_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                     intermediateName + "_wren");
                    instance.AddPort("full_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                     intermediateName + "_full");
                }
                instance.Generate();

                if (hasData)
                {
                    _writer.Str() << "assign arbiter_" << functionName << "_" << dstFifoIdNorm << "_data_in_" << i
                                  << "[0] = " << inputData[0] << ";";
                    _writer.Str() << "assign arbiter_" << functionName << "_" << dstFifoIdNorm << "_data_in_" << i
                                  << "[1] = " << inputData[1] << ";";
                }

                _writer.Str() << "assign arbiter_" << functionName << "_" << dstFifoIdNorm << "_empty_in_" << i
                              << "[0] = " << inputEmpty[0] << ";";
                _writer.Str() << "assign arbiter_" << functionName << "_" << dstFifoIdNorm << "_empty_in_" << i
                              << "[1] = " << inputEmpty[1] << ";";
                _writer.Str() << "assign " << inputRden[0] << " = arbiter_" << functionName << "_" << dstFifoIdNorm
                              << "_rden_out_" << i << "[0];";
                _writer.Str() << "assign " << inputRden[1] << " = arbiter_" << functionName << "_" << dstFifoIdNorm
                              << "_rden_out_" << i << "[1];";

                if (isFirst)
                {
                    // First instance merges calls from 2 call sites
                    baseSource += 2;
                }
                else
                {
                    // Each other instance merges 1 new call site
                    baseSource++;
                }
            }
        }
    }

    std::map<size_t, size_t> InstantiateLoopGenerators()
    {
        // Create the loop generator index map
        std::set<size_t> loopGeneratorIndexSet;
        std::map<size_t, size_t> loopGeneratorMap;
        for (const LoopGenerator& loopGenerator : _program._loopGenerators)
        {
            assert(1 == loopGenerator._function->_start->_inputFifoCount);
            const size_t fifoIndex = loopGenerator._function->_start->_inputFifoIndices[0];
            loopGeneratorIndexSet.insert(fifoIndex);
        }
        loopGeneratorMap = SetToIndexedMap(loopGeneratorIndexSet);

        for (const LoopGenerator& loopGenerator : _program._loopGenerators)
        {
            assert(1 == loopGenerator._function->_start->_inputFifoCount);
            const size_t fifoIndex = loopGenerator._function->_start->_inputFifoIndices[0];

            const RegisterDescription fifoDesc = _program._registerTable[fifoIndex];

            const size_t subFifoCount = GetSubFifoCount(fifoIndex);

            assert(1 == subFifoCount);
            assert(RegisterType::Fifo == fifoDesc._type);

            const size_t fifoMappedIndex = SafeLookup(loopGeneratorMap, fifoIndex);
            const size_t fifoIndexNorm = _fifoNamer.GetNormFifoIndex(fifoIndex);
            const std::string structName = std::string("loop_data_") + std::to_string(fifoIndexNorm);

            // Structure holds connection between loop generator and basic block
            _writer.Str() << "struct packed {";

            {
                AutoIndent autoIndent2(_writer);

                _writer.Str() << "logic [" << subFifoCount - 1 << ":0] rden;";
                _writer.Str() << "logic [" << subFifoCount - 1 << ":0] empty;";
                _writer.Str() << "logic [" << fifoDesc._width - 1 << ":0] data_out;";
                _writer.Str() << "logic underflow;";
            }

            _writer.Str() << "} " << structName << ";";

            AddGlobalDebugSignal(structName, "underflow");

            ModuleInstanceHelper instance(*this, GetUnknownLocation());
            instance.SetModuleName("KanagawaLoopGenerator");
            instance.AddU64Parameter("TOTAL_WIDTH", fifoDesc._width);
            instance.AddU64Parameter("COUNTER_WIDTH", loopGenerator._counterWidth);
            instance.AddBoolParameter("HAS_LITERAL_MAX_THREAD_ID", loopGenerator._literalMaxThreadId.is_initialized());
            instance.AddU64Parameter("LITERAL_MAX_THREAD_ID",
                                     loopGenerator._literalMaxThreadId ? *loopGenerator._literalMaxThreadId : 0);
            instance.AddU64Parameter("OFFSET", loopGenerator._counterOffset);
            instance.AddU64Parameter("ONLY_ONE_THREAD_OFFSET", loopGenerator._threadCountOneOffset);

            const std::string loopGeneratorName =
                "loop_generator_" + FixupString(loopGenerator._function->_name) + "_" + std::to_string(fifoIndexNorm);
            instance.SetInstanceName(loopGeneratorName);
            instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
            instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), GetResetReplica());

            JsonValue jsonLoopGenerator = JsonValue::CreateObject();
            JsonValue jsonNets = JsonValue::CreateArray();
            JsonValue jsonTopLevelNets = JsonValue::CreateArray();
            jsonLoopGenerator.AddMember("module", JsonValue("KanagawaLoopGenerator"));
            jsonLoopGenerator.AddMember("instance", _rtlMap.SerializeString(loopGeneratorName));

            const std::string st = "loop_data_" + std::to_string(fifoIndexNorm);
            const std::string s0 = st + ".underflow";
            instance.AddPort("underflow_out", circt::hw::ModulePort::Direction::Output, GetI1Type(), s0);
            instance.AddPort("empty_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                             "fifo_data_" + std::to_string(fifoIndexNorm) + ".empty");
            instance.AddPort("data_in", circt::hw::ModulePort::Direction::Input,
                             instance.GetParameterizedIntegerType("TOTAL_WIDTH"),
                             "fifo_data_" + std::to_string(fifoIndexNorm) + ".data_out");
            instance.AddPort("rden_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                             "fifo_data_" + std::to_string(fifoIndexNorm) + ".rden");

            const std::string s1 = st + ".data_out";
            instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output,
                             instance.GetParameterizedIntegerType("TOTAL_WIDTH"), s1);
            const std::string s2 = st + ".empty";
            instance.AddPort("empty_out", circt::hw::ModulePort::Direction::Output, GetI1Type(), s2);
            const std::string s3 = st + ".rden";
            instance.AddPort("rden_in", circt::hw::ModulePort::Direction::Input, GetI1Type(), s3);
            instance.Generate();

            // dump loop_generator's signals shown at top-level
            AddNetToJson(s0, 1, jsonTopLevelNets);
            AddNetToJson(s1, fifoDesc._width, jsonTopLevelNets);
            AddNetToJson(s2, 1, jsonTopLevelNets);
            AddNetToJson(s3, 1, jsonTopLevelNets);
            // dump loop_generator into JSON
            AddNetToJson("max_thread_id", loopGenerator._counterWidth, jsonNets);
            AddNetToJson("current_thread_id", loopGenerator._counterWidth, jsonNets);
            AddNetToJson("underflow_out", 1, jsonNets);
            AddNetToJson("data_in", fifoDesc._width, jsonNets);
            AddNetToJson("data_out", fifoDesc._width, jsonNets);
            AddNetToJson("only_one_thread", 1, jsonNets);
            // dump thread counter signals
            AddNetToJson("thread_counter.only_one_thread", 1, jsonNets);
            AddNetToJson("thread_counter.max_thread_id_in", loopGenerator._counterWidth, jsonNets);
            AddNetToJson("thread_counter.only_one_thread", 1, jsonNets);
            AddNetToJson("thread_counter.only_one_thread_in", 1, jsonNets);
            AddNetToJson("thread_counter.thread_count_data_ff.current_count", loopGenerator._counterWidth, jsonNets);
            AddNetToJson("thread_counter.thread_count_data_ff.current_count_plus_one", loopGenerator._counterWidth,
                         jsonNets);
            AddNetToJson("thread_counter.thread_count_data_ff.thread_id_minus_one_reached", 1, jsonNets);
            AddNetToJson("thread_counter.thread_count_data_next.current_count", loopGenerator._counterWidth, jsonNets);
            AddNetToJson("thread_counter.thread_count_data_next.current_count_plus_one", loopGenerator._counterWidth,
                         jsonNets);
            AddNetToJson("thread_counter.thread_count_data_next.thread_id_minus_one_reached", 1, jsonNets);
            AddNetToJson("thread_counter.thread_id_out", loopGenerator._counterWidth, jsonNets);
            jsonLoopGenerator.AddMember("nets", jsonNets);
            jsonLoopGenerator.AddMember("top_level_signals", jsonTopLevelNets);
            _jsonLoopGenerators.PushBack(jsonLoopGenerator);
        }

        return loopGeneratorMap;
    }

    void InstantiateContextSavers()
    {
        for (const ContextSaver& contextSaver : _program._contextSavers)
        {
            const RegisterDescription outputRegDesc = _program._registerTable[contextSaver._destinationFifo];
            const RegisterDescription calleeRegDesc = _program._registerTable[contextSaver._fromCalleeFifoIndex];
            const RegisterDescription callerRegDesc = _program._registerTable[contextSaver._fromCallerFifoIndex];

            const size_t loopCountWidth = contextSaver._loopCounterWidth;

            assert(RegisterType::Fifo == outputRegDesc._type);

            // Get the normalized caller/callee FIFO indexes
            const size_t callerFifoMappedIndex = _fifoNamer.GetNormFifoIndex(contextSaver._fromCallerFifoIndex);
            const size_t calleeFifoMappedIndex = _fifoNamer.GetNormFifoIndex(contextSaver._fromCalleeFifoIndex);
            const size_t destinationFifoIndexNorm = _fifoNamer.GetNormFifoIndex(contextSaver._destinationFifo);

            // Struct that holds wires connected to context saver
            _writer.Str() << "struct packed {";

            {
                AutoIndent autoIndent2(_writer);

                _writer.Str() << "logic caller_wren;";
                {
                    std::unique_ptr<DisableCodeCoverage> disableCodeCoverage;

                    if (0 == callerRegDesc._width)
                    {
                        // Disable code coverage for this variables which are created because our FIFO is actually zero
                        // width
                        disableCodeCoverage = std::make_unique<DisableCodeCoverage>(*this);
                    }

                    _writer.Str() << "logic [" << GetClampedRegisterWidth(callerRegDesc._width) - 1
                                  << ":0] caller_data;";
                }
                _writer.Str() << "logic [" << loopCountWidth - 1 << ":0] caller_loop_count;";
                _writer.Str() << "logic caller_almost_full;";

                if (!contextSaver._isOrdered)
                {
                    _writer.Str() << "logic [" << GetCodeGenConfig().GetInvocationIndexSize() - 1
                                  << ":0] caller_invocation_index;";
                }

                _writer.Str() << "logic [" << GetClampedRegisterWidth(contextSaver.GetCalleeOutputWidth(_program)) - 1
                              << ":0] output_callee_data;";
                _writer.Str() << "logic [" << GetClampedRegisterWidth(callerRegDesc._width) - 1
                              << ":0] output_caller_data;";
            }

            _writer.Str() << "} context_saver_data_" << callerFifoMappedIndex << ";";

            if (0 == callerRegDesc._width)
            {
                _writer.Str() << "assign context_saver_data_" << callerFifoMappedIndex << ".caller_data = '0;";
            }

            // Instantiate merger
            // Choose a name that people will be able to understand
            // For external calls, use the name of the extern function
            // For other calls, use the name of the called function
            const std::string readableName =
                FixupString(contextSaver._isExternal ? contextSaver._beforeCall->_function->_name
                                                     : contextSaver._callee->GetName());

            if (outputRegDesc._width > 0)
            {
                _writer.Str() << GetModuleNamePrefix() << "ContextSaverMerger" << callerFifoMappedIndex
                              << " context_saver_merger_" << readableName << "_" << callerFifoMappedIndex;
                _writer.Str() << "(";

                {
                    AutoIndent autoIndent2(_writer);

                    if (callerRegDesc._width > 0)
                    {
                        _writer.Str() << ".caller_data(context_saver_data_" << callerFifoMappedIndex
                                      << ".output_caller_data),";
                    }

                    if (contextSaver.GetCalleeOutputWidth(_program) > 0)
                    {
                        _writer.Str() << ".callee_data(context_saver_data_" << callerFifoMappedIndex
                                      << ".output_callee_data),";
                    }

                    assert(outputRegDesc._width > 0);

                    if (outputRegDesc.Fifo()._type == FifoType::Passthrough)
                    {
                        _writer.Str() << ".output_data(passthrough_data_" << destinationFifoIndexNorm << ".data)";
                    }
                    else
                    {
                        assert(outputRegDesc.Fifo()._type == FifoType::Default);

                        _writer.Str() << ".output_data(fifo_data_" << destinationFifoIndexNorm << ".data_in)";
                    }
                }

                _writer.Str() << ");";
            }

            const bool isPipelined = contextSaver._isExternal
                                         ? false
                                         : (contextSaver._callee->GetModifiers() & ParseTreeFunctionModifierPipelined);

            const char* const pipelineString = isPipelined ? "1" : "0";

            // If the callee is unreachable, then there will be no assignment
            // to the from-callee-fifo wren.  Tie it to 0.
            if (!contextSaver._calleeInstance->GetFunction()->_isReachable)
            {
                _writer.Str() << "assign fifo_data_" << calleeFifoMappedIndex << ".wren = 1'b0;";
            }

            ModuleInstanceHelper instance(*this, GetUnknownLocation());
            if (contextSaver._isOrdered)
            {
                instance.SetModuleName("ContextSaverOrdered");

                const size_t calleeReturnWidth =
                    contextSaver._calleeReturnValueRegisters
                        ? contextSaver._calleeReturnValueRegisters->GetType()->GetBitWidth()
                        : 1;
                const size_t callerReturnWidth =
                    contextSaver._callerReturnValueRegisters
                        ? contextSaver._callerReturnValueRegisters->GetType()->GetBitWidth()
                        : 1;
                const size_t returnElementCount = callerReturnWidth / calleeReturnWidth;

                const size_t bufferFifoEnable =
                    GetCodeGenDeviceConfig()._useInternalBufferFifoOptimization ? 1 : 0;

                size_t pendingCountWidth = 0;

                if (isPipelined)
                {
                    if (contextSaver._calleeReturnValueRegisters)
                    {
                        // The return array buffer size is returnElementCount + 1
                        // +2 to ensure that this buffer size can be represented
                        pendingCountWidth = Log2RoundUp(returnElementCount + 2);
                    }
                    else
                    {
                        // Ensure the pending count variable will never overflow
                        // At most each calling thread can launch 2^loopCountWidth sub-threads
                        pendingCountWidth =
                            loopCountWidth +
                            Log2RoundUp(contextSaver._afterCall->_function->_maxThreadCountInsideFunction);
                    }
                }

                instance.AddU64Parameter("DST_WIDTH", GetClampedRegisterWidth(outputRegDesc._width));
                instance.AddU64Parameter("CALLEE_IN_WIDTH", GetClampedRegisterWidth(calleeRegDesc._width));
                instance.AddU64Parameter("CALLEE_OUT_WIDTH",
                                         GetClampedRegisterWidth(contextSaver.GetCalleeOutputWidth(_program)));
                instance.AddU64Parameter("CALLER_WIDTH", GetClampedRegisterWidth(callerRegDesc._width));
                instance.AddU64Parameter("LOG_DEPTH", callerRegDesc.Fifo().LogDepth());
                instance.AddU64Parameter("DEPTH", callerRegDesc.Fifo().RoundedDepth());
                instance.AddBoolParameter("USE_LUTRAM",
                                          ShouldFifoUseLutRam(_program, contextSaver._fromCallerFifoIndex));
                instance.AddU64Parameter("ALMOSTFULL_ENTRIES", callerRegDesc.Fifo()._almostFullSlots);
                instance.AddBoolParameter("IS_PIPELINED", isPipelined);
                instance.AddU64Parameter("LOOP_COUNT_WIDTH", loopCountWidth);
                instance.AddU64Parameter("PENDING_COUNT_WIDTH", pendingCountWidth);
                instance.AddU64Parameter("RETURN_ELEMENT_WIDTH", calleeReturnWidth);
                instance.AddU64Parameter("RETURN_ELEMENT_COUNT", returnElementCount);
                instance.AddU64Parameter("ALMOSTEMPTY_VAL", GetAlmostEmptyDepth());
                instance.AddU64Parameter("WRITE_DELAY", contextSaver._writeDelay);
                instance.AddBoolParameter("BUFFER_FIFO_OPTIMIZATION", bufferFifoEnable);
                instance.AddU64Parameter("CALLEE_VOID_RETURN_TYPE", contextSaver._calleeReturnValueRegisters ? 0 : 1);

                instance.SetInstanceName(_fifoNamer.GetFifoName(contextSaver._fromCallerFifoIndex));
                instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
                instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), GetResetReplica());
                instance.AddPort("caller_wren_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".caller_wren");
                instance.AddPort("caller_data_in", circt::hw::ModulePort::Direction::Input,
                                 instance.GetParameterizedIntegerType("CALLER_WIDTH"),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".caller_data");
                instance.AddPort("caller_loop_count_in", circt::hw::ModulePort::Direction::Input,
                                 instance.GetParameterizedIntegerType("LOOP_COUNT_WIDTH"),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".caller_loop_count");
                instance.AddPort("caller_almost_full_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".caller_almost_full");
                instance.AddPort("callee_empty_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 "fifo_data_" + std::to_string(calleeFifoMappedIndex) + ".empty");

                const std::string calleeDataInValue =
                    (calleeRegDesc._width > 0) ? "fifo_data_" + std::to_string(calleeFifoMappedIndex) + ".data_out"
                                               : "\'0";

                instance.AddPort("callee_data_in", circt::hw::ModulePort::Direction::Input,
                                 instance.GetParameterizedIntegerType("CALLEE_IN_WIDTH"), calleeDataInValue);

                instance.AddPort("callee_rden_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                 "fifo_data_" + std::to_string(calleeFifoMappedIndex) + ".rden");

                const RegisterDescription& dstFifoDesc = _program._registerTable[contextSaver._destinationFifo];
                assert(RegisterType::Fifo == dstFifoDesc._type);

                if (FifoType::Default == dstFifoDesc.Fifo()._type)
                {
                    instance.AddPort("output_rdy_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                     "~fifo_data_" + std::to_string(destinationFifoIndexNorm) + ".almost_full");
                    instance.AddPort("valid_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                     "fifo_data_" + std::to_string(destinationFifoIndexNorm) + ".wren");
                }
                else
                {
                    assert(FifoType::Passthrough == dstFifoDesc.Fifo()._type);
                    instance.AddPort("output_rdy_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                     "passthrough_data_" + std::to_string(destinationFifoIndexNorm) + ".rdy_ext");
                    instance.AddPort("valid_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                     "passthrough_data_" + std::to_string(destinationFifoIndexNorm) + ".valid");
                }

                instance.AddPort("callee_data_out", circt::hw::ModulePort::Direction::Output,
                                 instance.GetParameterizedIntegerType("CALLEE_OUT_WIDTH"),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".output_callee_data");
                instance.AddPort("caller_data_out", circt::hw::ModulePort::Direction::Output,
                                 instance.GetParameterizedIntegerType("CALLER_WIDTH"),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".output_caller_data");
                instance.Generate();
            }
            else
            {
                assert(!isPipelined);

                instance.SetModuleName("KanagawaContextSaver");

                const FifoSubset invocationInstanceFifoSubset = contextSaver.InvocationInstanceInCalleeFifo();
                instance.AddU64Parameter("DST_WIDTH", GetClampedRegisterWidth(outputRegDesc._width));
                instance.AddU64Parameter("CALLEE_WIDTH", GetClampedRegisterWidth(calleeRegDesc._width));
                instance.AddU64Parameter("CALLER_WIDTH", GetClampedRegisterWidth(callerRegDesc._width));
                instance.AddU64Parameter("DEPTH", callerRegDesc.Fifo()._depth);
                instance.AddU64Parameter("INSTANCE_OFFSET", invocationInstanceFifoSubset._offset);
                instance.AddU64Parameter("INSTANCE_WIDTH", GetCodeGenConfig().GetInvocationIndexSize());
                instance.AddStringParameter("DEVICE_FAMILY", GetCodeGenDeviceConfig()._halDeviceFamily);

                instance.SetInstanceName(_fifoNamer.GetFifoName(contextSaver._fromCallerFifoIndex));

                instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
                instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), GetResetReplica());
                instance.AddPort("caller_wren_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".caller_wren");
                instance.AddPort("caller_data_in", circt::hw::ModulePort::Direction::Input,
                                 instance.GetParameterizedIntegerType("CALLER_WIDTH"),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".caller_data");
                instance.AddPort("caller_invocation_index_out", circt::hw::ModulePort::Direction::Output,
                                 GetIntegerType(GetCodeGenConfig().GetInvocationIndexSize()),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) +
                                     ".caller_invocation_index");
                instance.AddPort("callee_empty_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 "fifo_data_" + std::to_string(calleeFifoMappedIndex) + ".empty");
                instance.AddPort("callee_data_in", circt::hw::ModulePort::Direction::Input,
                                 instance.GetParameterizedIntegerType("CALLEE_WIDTH"),
                                 "fifo_data_" + std::to_string(calleeFifoMappedIndex) + ".data_out");
                instance.AddPort("callee_rden_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                 "fifo_data_" + std::to_string(calleeFifoMappedIndex) + ".rden");

                const RegisterDescription& dstFifoDesc = _program._registerTable[contextSaver._destinationFifo];
                assert(RegisterType::Fifo == dstFifoDesc._type);

                instance.AddPort("output_almost_full_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 "fifo_data_" + std::to_string(destinationFifoIndexNorm) + ".almost_full");
                instance.AddPort("wren_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                 "fifo_data_" + std::to_string(destinationFifoIndexNorm) + ".wren");
                instance.AddPort("callee_data_out", circt::hw::ModulePort::Direction::Output,
                                 instance.GetParameterizedIntegerType("CALLEE_WIDTH"),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".output_callee_data");
                instance.AddPort("caller_data_out", circt::hw::ModulePort::Direction::Output,
                                 instance.GetParameterizedIntegerType("CALLER_WIDTH"),
                                 "context_saver_data_" + std::to_string(callerFifoMappedIndex) + ".output_caller_data");
                instance.Generate();
            }
        }
    }

    std::string GetGlobalOutName(const size_t registerIndex, const size_t writeIndex) const
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];

        assert(RegisterType::Global == regDesc._type);
        assert(writeIndex < regDesc.Global()._writeCount);

        std::ostringstream str;

        str << "global_out_" << regDesc._name << "_" << registerIndex << "_" << writeIndex;

        return str.str();
    }

    std::string GetGlobalValidOutName(const size_t registerIndex, const size_t writeIndex) const
    {
        std::string result = GetGlobalOutName(registerIndex, writeIndex);

        result += "_valid";

        return result;
    }

    std::string GetGlobalInName(const size_t registerIndex) const
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];

        assert(RegisterType::Global == regDesc._type);

        std::ostringstream str;

        str << "global_in_" << regDesc._name << "_" << registerIndex;

        return str.str();
    }

    std::string GetGlobalInNextName(const size_t registerIndex) const
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];

        assert(RegisterType::Global == regDesc._type);

        std::ostringstream str;

        str << "global_in_" << regDesc._name << "_" << registerIndex << "_next";

        return str.str();
    }

    // Returns the name of the variable that holds a global view value
    std::string GetGlobalViewName(const size_t registerIndex) const
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];

        assert(RegisterType::GlobalView == regDesc._type);

        std::ostringstream str;

        str << "global_view_" << regDesc._name << "_" << registerIndex;

        return str.str();
    }

    // Returns a port name for a global view in a basic block
    std::string GetGlobalViewInName(const size_t registerIndex) const
    {
        return GetGlobalViewName(registerIndex) + "_in";
    }

    void InstantiateBasicBlocks(ModuleDeclarationHelper& mod, const std::set<size_t>& globalsRequiringNext,
                                const mlir::Value hasMemInitCompleted, const std::map<size_t, size_t>& loopGeneratorMap)
    {
        for (const Function& function : _program._functions)
        {
            if (function.IsExtern())
            {
            }
            else
            {
                for (const BasicBlock& basicBlock : function._basicBlocks)
                {
                    const std::set<size_t> readGlobals =
                        GetGlobalsReadWithoutNext(_program, basicBlock, globalsRequiringNext);
                    const std::set<size_t> readGlobalsWithNext =
                        GetGlobalsReadWithNext(_program, basicBlock, globalsRequiringNext);
                    const std::set<std::pair<size_t, size_t>> writtenGlobals = GetGlobalsWritten(_program, basicBlock);
                    const std::set<size_t> readGlobalViews = GetGlobalViewsRead(_program, basicBlock);
                    const std::set<size_t> writtenFifos = GetWrittenFifos(basicBlock);
                    const std::map<size_t, size_t> writtenFifoMap =
                        SetToIndexedMap(GetWrittenAndBackpressureFifos(_program, basicBlock));
                    const std::set<size_t> backpressureFifos = GetBackpressureFifos(_program, basicBlock);
                    const std::set<MemoryAccessRecord> writtenMemories = GetMemoriesWritten(_program, basicBlock);
                    const std::set<MemoryAccessRecord> readMemories = GetMemoriesRead(_program, basicBlock);

                    const std::set<size_t> acquiredSemaphores = GetAcquiredSemaphores(basicBlock);
                    const std::set<size_t> releasedSemaphores = GetReleasedSemaphores(basicBlock);

                    ModuleInstanceHelper instance(*this, LocationToCirctLocation(basicBlock._location));

                    instance.SetModuleName(GetModuleNamePrefix() + GetBasicBlockName(basicBlock));
                    instance.SetInstanceName(GetBasicBlockInstanceName(basicBlock));

                    instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");

                    instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), GetResetReplica());

                    // The done port is only there to prevent dead code elimination
                    // from eliminating all logic in a basic block
                    instance.AddPort("done_out", circt::hw::ModulePort::Direction::Output, GetI1Type());

                    if (basicBlock.IsResetBlock())
                    {
                        instance.AddPort("allow_initial_call_in", circt::hw::ModulePort::Direction::Input,
                                            hasMemInitCompleted);
                    }

                    for (const size_t semaphoreIndex : acquiredSemaphores)
                    {
                        // Thread count ports
                        instance.AddPort("semaphore_full_" + std::to_string(semaphoreIndex) + "_in",
                                         circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                         GetSemaphoreName(function, semaphoreIndex) + ".full");
                        instance.AddPort("incr_semaphore_thread_count_" + std::to_string(semaphoreIndex) + "_out",
                                         circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                         GetSemaphoreName(function, semaphoreIndex) + ".incr_count");
                    }

                    for (const size_t semaphoreIndex : releasedSemaphores)
                    {
                        // Thread count ports
                        instance.AddPort("decr_semaphore_thread_count_" + std::to_string(semaphoreIndex) + "_out",
                                         circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                         GetSemaphoreName(function, semaphoreIndex) + ".decr_count");
                    }

                    {
                        circt::OpBuilder opb = circt::OpBuilder::atBlockEnd(_coreModule->GetBodyBlock());

                        for (const size_t i : readGlobals)
                        {
                            const RegisterDescription& regDesc = _program._registerTable[i];

                            const mlir::Location location = RegDescToLocation(regDesc);

                            const GlobalRegKey key = GetGlobalRegKey(i, globalsRequiringNext);

                            // When reading from a global, the typical case is to read .value
                            instance.AddPort(
                                GetGlobalInName(i), circt::hw::ModulePort::Direction::Input,
                                GetIntegerType(regDesc._width),
                                GetPathToGlobalContainer(*_coreModule, ObjectPath(), i, globalsRequiringNext),
                                key.GetFieldSymbol("value_out"));
                        }

                        // When adding ports for global inputs with next
                        // also add globals read by global views in this block
                        const std::set<size_t> readGlobalsWithNextIncGlobalViewInputs =
                            GetReadGlobalsWithNextIncGlobalViewInputs(readGlobalsWithNext, readGlobalViews);

                        for (const size_t i : readGlobalsWithNextIncGlobalViewInputs)
                        {
                            // When reading from a global, the typical case is to read .value
                            // When the result is passed to a hardened pipelined register, the value_next is read
                            const RegisterDescription& regDesc = _program._registerTable[i];

                            const RegisterDescription::GlobalDesc& globalDesc = regDesc.Global();

                            const mlir::Location location = RegDescToLocation(regDesc);

                            const GlobalRegKey key = GetGlobalRegKey(i, globalsRequiringNext);

                            const mlir::Value pathToInstance =
                                GetPathToGlobalContainer(*_coreModule, ObjectPath(), i, globalsRequiringNext);

                            instance.AddPort(GetGlobalInName(i), circt::hw::ModulePort::Direction::Input,
                                             GetIntegerType(regDesc._width), pathToInstance,
                                             key.GetFieldSymbol("value_out"));

                            instance.AddPort(GetGlobalInNextName(i), circt::hw::ModulePort::Direction::Input,
                                             GetIntegerType(regDesc._width), pathToInstance,
                                             key.GetFieldSymbol("value_next_out"));
                        }

                        for (const std::pair<size_t, size_t>& p : writtenGlobals)
                        {
                            const size_t regIndex = p.first;
                            const size_t writeIndex = p.second;

                            const RegisterDescription& regDesc = _program._registerTable[regIndex];

                            const RegisterDescription::GlobalDesc& globalDesc = regDesc.Global();

                            const GlobalRegKey key = GetGlobalRegKey(regIndex, globalsRequiringNext);

                            const mlir::Value pathToInstance =
                                GetPathToGlobalContainer(*_coreModule, ObjectPath(), regIndex, globalsRequiringNext);

                            instance.AddPort(GetGlobalValidOutName(regIndex, writeIndex),
                                             circt::hw::ModulePort::Direction::Output, GetI1Type(), pathToInstance,
                                             key.GetFieldSymbol("input_valid_" + std::to_string(writeIndex)));

                            if (globalDesc._literalValues.end() == globalDesc._literalValues.find(writeIndex))
                            {
                                instance.AddPort(GetGlobalOutName(regIndex, writeIndex),
                                                 circt::hw::ModulePort::Direction::Output,
                                                 GetIntegerType(regDesc._width), pathToInstance,
                                                 key.GetFieldSymbol("input_" + std::to_string(writeIndex)));
                            }
                        }

                        for (const MemoryAccessRecord& record : readMemories)
                        {
                            const size_t registerIndex = record._memoryIndex;
                            const size_t port = record._portIndex;

                            const RegisterDescription& regDesc = _program._registerTable[registerIndex];

                            const RegisterDescription::MemoryDesc& memDesc = regDesc.Memory();

                            const size_t dataWidth = regDesc.Memory()._elementWidth;

                            const size_t addrWidth = regDesc.GetMemoryAddressWidth();

                            const mlir::Value pathToInstance =
                                GetPathToMemoryContainer(*_coreModule, ObjectPath(), registerIndex);

                            // Combine register index and port index
                            std::ostringstream str;
                            str << registerIndex << "_" << port;

                            instance.AddPort("memory_read_data_in_" + str.str(),
                                             circt::hw::ModulePort::Direction::Input, GetIntegerType(dataWidth),
                                             pathToInstance,
                                             GetFullyQualifiedInnerSymAttr(regDesc.Memory()._containerInstancePath,
                                                                           "read_data_out_" + std::to_string(port)));

                            instance.AddPort("memory_read_addr_out_" + str.str(),
                                             circt::hw::ModulePort::Direction::Output, GetIntegerType(addrWidth),
                                             pathToInstance,
                                             GetFullyQualifiedInnerSymAttr(regDesc.Memory()._containerInstancePath,
                                                                           "read_addr_in_" + std::to_string(port)));

                            if (!memDesc._useLogicRam)
                            {
                                instance.AddPort("memory_rden_out_" + str.str(),
                                                 circt::hw::ModulePort::Direction::Output, GetI1Type(), pathToInstance,
                                                 GetFullyQualifiedInnerSymAttr(regDesc.Memory()._containerInstancePath,
                                                                               "rden_in_" + std::to_string(port)));
                            }

                            if (regDesc.Memory()._ecc)
                            {
                                instance.AddPort(
                                    "memory_read_ecc_in_" + str.str(), circt::hw::ModulePort::Direction::Input,
                                    GetIntegerType(2), pathToInstance,
                                    GetFullyQualifiedInnerSymAttr(regDesc.Memory()._containerInstancePath,
                                                                  "ecc_status_out_" + std::to_string(port)));
                            }
                        }

                        for (const MemoryAccessRecord& record : writtenMemories)
                        {
                            const size_t registerIndex = record._memoryIndex;
                            const size_t port = record._portIndex;

                            const RegisterDescription& regDesc = _program._registerTable[registerIndex];

                            const RegisterDescription::MemoryDesc& memDesc = regDesc.Memory();

                            const size_t dataWidth = regDesc.Memory()._elementWidth;

                            const size_t addrWidth = regDesc.GetMemoryAddressWidth();

                            const mlir::Value pathToInstance =
                                GetPathToMemoryContainer(*_coreModule, ObjectPath(), registerIndex);

                            // Combine register index and port index
                            std::ostringstream str;
                            str << registerIndex << "_" << port;

                            instance.AddPort("memory_write_data_out_" + str.str(),
                                             circt::hw::ModulePort::Direction::Output, GetIntegerType(dataWidth),
                                             pathToInstance,
                                             GetFullyQualifiedInnerSymAttr(regDesc.Memory()._containerInstancePath,
                                                                           "write_data_in_" + std::to_string(port)));

                            instance.AddPort("memory_write_addr_out_" + str.str(),
                                             circt::hw::ModulePort::Direction::Output, GetIntegerType(addrWidth),
                                             pathToInstance,
                                             GetFullyQualifiedInnerSymAttr(regDesc.Memory()._containerInstancePath,
                                                                           "write_addr_in_" + std::to_string(port)));

                            instance.AddPort("memory_wren_" + str.str(), circt::hw::ModulePort::Direction::Output,
                                             GetI1Type(), pathToInstance,
                                             GetFullyQualifiedInnerSymAttr(regDesc.Memory()._containerInstancePath,
                                                                           "wren_in_" + std::to_string(port)));
                        }
                    }

                    // Connect fixed-latency external function ports from top-level module to basic block
                    const std::set<size_t> externalModuleCalls =
                        GetCrossBBExternalClassInstanceCalls(_program, basicBlock);

                    for (size_t externalModuleIndex : externalModuleCalls)
                    {
                        const ExternalModuleCall& externModuleCall = _program._externalModuleCalls[externalModuleIndex];

                        struct Port
                        {
                            std::string _name;
                            circt::hw::ModulePort::Direction _direction;
                            mlir::Type _type;
                        };

                        std::vector<Port> ports;

                        ports.push_back(Port{"valid", circt::hw::ModulePort::Direction::Output, GetI1Type()});

                        const FunctionDesc& functionDesc = externModuleCall._functionDesc;

                        for (size_t i = 0; i < functionDesc._parameterTypes.size(); i++)
                        {
                            const std::string& name = functionDesc._parameterNames[i];

                            const size_t width = functionDesc._parameterTypes[i]->GetBitWidth();

                            ports.push_back(
                                Port{name, circt::hw::ModulePort::Direction::Output, GetIntegerType(width)});
                        }

                        if (functionDesc._returnType->GetBitWidth() > 0)
                        {
                            ports.push_back(Port{"result", circt::hw::ModulePort::Direction::Input,
                                                 GetIntegerType(functionDesc._returnType->GetBitWidth())});
                        }

                        for (const Port& port : ports)
                        {
                            std::ostringstream nameWithPrefix;

                            if (externModuleCall._type == ExternalModuleCallType::ExternClassMethod)
                            {
                                nameWithPrefix << externModuleCall.GetFullyQualifiedName();
                            }
                            else
                            {
                                nameWithPrefix << externModuleCall._name;
                            }

                            nameWithPrefix << "_" + port._name;

                            // the net connected to the basic block
                            // has in/out suffix in the case where the function is connected to top-level ports
                            const std::string portSuffix =
                                circt::hw::ModulePort::Direction::Input == port._direction ? "_in" : "_out";

                            const std::string netSuffix =
                                (ExternalModuleCallType::ExternallyInstantiated == externModuleCall._type) ? portSuffix
                                                                                                           : "";

                            instance.AddPort(nameWithPrefix.str() + portSuffix, port._direction, port._type,
                                             mod.AssignPortOptional(nameWithPrefix.str() + netSuffix));
                        }
                    }

                    for (const size_t i : writtenFifos)
                    {
                        const RegisterDescription& regDesc = _program._registerTable[i];
                        assert(RegisterType::Fifo == regDesc._type);

                        const size_t portIndex = SafeLookup(writtenFifoMap, i);
                        const size_t iNorm = _fifoNamer.GetNormFifoIndex(i);

                        switch (regDesc.Fifo()._type)
                        {
                        case FifoType::Default:
                        case FifoType::ReorderBuffer:
                            if (regDesc._width > 0)
                            {
                                instance.AddPort("fifo_data_out_" + std::to_string(portIndex),
                                                 circt::hw::ModulePort::Direction::Output,
                                                 GetIntegerType(regDesc._width),
                                                 "fifo_data_" + std::to_string(iNorm) + ".data_in");
                            }

                            instance.AddPort("fifo_wren_" + std::to_string(portIndex),
                                             circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                             "fifo_data_" + std::to_string(iNorm) + ".wren");
                            break;

                        case FifoType::ContextSaverCaller:
                            instance.AddPort("context_saver_caller_wren_" + std::to_string(portIndex),
                                             circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                             "context_saver_data_" + std::to_string(iNorm) + ".caller_wren");

                            instance.AddPort("context_saver_caller_loop_count_" + std::to_string(portIndex),
                                             circt::hw::ModulePort::Direction::Output,
                                             GetIntegerType(regDesc.Fifo()._contextSaverCaller._loopCounterWidth),
                                             "context_saver_data_" + std::to_string(iNorm) + ".caller_loop_count");

                            if (!regDesc.Fifo()._contextSaverCaller._ordered)
                            {
                                instance.AddPort("context_saver_caller_invocation_index_" + std::to_string(portIndex),
                                                 circt::hw::ModulePort::Direction::Input,
                                                 GetIntegerType(GetCodeGenConfig().GetInvocationIndexSize()),
                                                 "context_saver_data_" + std::to_string(iNorm) +
                                                     ".caller_invocation_index");
                            }

                            if (regDesc._width > 0)
                            {
                                instance.AddPort("context_saver_caller_data_" + std::to_string(portIndex),
                                                 circt::hw::ModulePort::Direction::Output,
                                                 GetIntegerType(regDesc._width),
                                                 "context_saver_data_" + std::to_string(iNorm) + ".caller_data");
                            }
                            break;

                        case FifoType::PassthroughRegistered:
                            if (regDesc._width > 0)
                            {
                                instance.AddPort("fifo_data_out_" + std::to_string(portIndex),
                                                 circt::hw::ModulePort::Direction::Output,
                                                 GetIntegerType(regDesc._width),
                                                 "passthrough_data_" + std::to_string(iNorm) + ".data_next");
                            }

                            instance.AddPort("fifo_wren_" + std::to_string(portIndex),
                                             circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                             "passthrough_data_" + std::to_string(iNorm) + ".valid_next");
                            break;

                        case FifoType::PassthroughUnregistered:
                            if (regDesc._width > 0)
                            {
                                instance.AddPort("fifo_data_out_" + std::to_string(portIndex),
                                                 circt::hw::ModulePort::Direction::Output,
                                                 GetIntegerType(regDesc._width),
                                                 "passthrough_data_" + std::to_string(iNorm) + ".data");
                            }

                            instance.AddPort("fifo_wren_" + std::to_string(portIndex),
                                             circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                             "passthrough_data_" + std::to_string(iNorm) + ".valid");
                            break;

                        default:
                            assert(false);
                        }
                    }

                    for (const size_t i : backpressureFifos)
                    {
                        const RegisterDescription& regDesc = _program._registerTable[i];
                        assert(RegisterType::Fifo == regDesc._type);

                        const size_t portIndex = SafeLookup(writtenFifoMap, i);
                        const size_t iNorm = _fifoNamer.GetNormFifoIndex(i);
                        size_t contextSaverIndex = 0;

                        switch (regDesc.Fifo()._type)
                        {
                        case FifoType::Default:
                        case FifoType::ReorderBuffer:
                            instance.AddPort("fifo_almost_full_in_raw_" + std::to_string(portIndex),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                             "fifo_data_" + std::to_string(iNorm) + ".almost_full");

                            instance.AddPort("fifo_overflow_in_" + std::to_string(portIndex),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                             "fifo_data_" + std::to_string(iNorm) + ".overflow");
                            break;

                        case FifoType::ContextSaverCaller:
                            instance.AddPort("fifo_almost_full_in_raw_" + std::to_string(portIndex),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                             "context_saver_data_" + std::to_string(iNorm) + ".caller_almost_full");

                            instance.AddPort("fifo_overflow_in_" + std::to_string(portIndex),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(), "1'b0");
                            break;

                        default:
                            assert(false);
                        }
                    }

                    // Connect basic block inputs to fifo
                    for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
                    {
                        const size_t inputFifoIndex = basicBlock._inputFifoIndices[i];
                        const size_t inputFifoIndexNorm = _fifoNamer.GetNormFifoIndex(inputFifoIndex);

                        const RegisterDescription& inputFifoDesc = _program._registerTable[inputFifoIndex];
                        assert(inputFifoDesc._type == RegisterType::Fifo);

                        const bool isLast = (i + 1) == basicBlock._inputFifoCount;

                        if (inputFifoDesc.Fifo()._type == FifoType::Passthrough)
                        {
                            const char* inputStructName = "passthrough_data_";

                            if (inputFifoDesc._width > 0)
                            {
                                instance.AddPort("data_in_" + std::to_string(inputFifoIndexNorm),
                                                 circt::hw::ModulePort::Direction::Input,
                                                 GetIntegerType(inputFifoDesc._width),
                                                 inputStructName + std::to_string(inputFifoIndexNorm) + ".data");
                            }

                            instance.AddPort("input_fifo_underflow_" + std::to_string(i),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                             inputStructName + std::to_string(inputFifoIndexNorm) + ".underflow");

                            instance.AddPort("input_rdy_" + std::to_string(i), circt::hw::ModulePort::Direction::Output,
                                             GetI1Type(),
                                             inputStructName + std::to_string(inputFifoIndexNorm) + ".rdy_int");

                            instance.AddPort("input_valid_" + std::to_string(i),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                             inputStructName + std::to_string(inputFifoIndexNorm) + ".valid");
                        }
                        else if (inputFifoDesc.Fifo()._type == FifoType::PassthroughRegistered)
                        {
                            const char* inputStructName = "passthrough_data_";

                            if (inputFifoDesc._width > 0)
                            {
                                instance.AddPort("data_in_" + std::to_string(inputFifoIndexNorm),
                                                 circt::hw::ModulePort::Direction::Input,
                                                 GetIntegerType(inputFifoDesc._width),
                                                 inputStructName + std::to_string(inputFifoIndexNorm) + ".data_ff");
                            }

                            instance.AddPort("input_fifo_underflow_" + std::to_string(i),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(), "1'b0");

                            instance.AddPort("input_valid_" + std::to_string(i),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                             inputStructName + std::to_string(inputFifoIndexNorm) + ".valid_ff");
                        }
                        else if (inputFifoDesc.Fifo()._type == FifoType::PassthroughUnregistered)
                        {
                            const char* inputStructName = "passthrough_data_";

                            if (inputFifoDesc._width > 0)
                            {
                                instance.AddPort("data_in_" + std::to_string(inputFifoIndexNorm),
                                                 circt::hw::ModulePort::Direction::Input,
                                                 GetIntegerType(inputFifoDesc._width),
                                                 inputStructName + std::to_string(inputFifoIndexNorm) + ".data");
                            }

                            instance.AddPort("input_fifo_underflow_" + std::to_string(i),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(), "1'b0");

                            instance.AddPort("input_valid_" + std::to_string(i),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                             inputStructName + std::to_string(inputFifoIndexNorm) + ".valid");
                        }
                        else
                        {
                            const char* inputStructName =
                                basicBlock._inputFifoIsLoopGenerator ? "loop_data_" : "fifo_data_";
                            const size_t selectedInputFifoIndex = _fifoNamer.GetNormFifoIndex(inputFifoIndex);

                            if (inputFifoDesc._width > 0)
                            {
                                instance.AddPort(
                                    "data_in_" + std::to_string(selectedInputFifoIndex),
                                    circt::hw::ModulePort::Direction::Input, GetIntegerType(inputFifoDesc._width),
                                    inputStructName + std::to_string(selectedInputFifoIndex) + ".data_out");
                            }

                            instance.AddPort("input_fifo_underflow_" + std::to_string(i),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                             inputStructName + std::to_string(selectedInputFifoIndex) + ".underflow");

                            instance.AddPort("input_fifo_rden_" + std::to_string(i),
                                             circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                             inputStructName + std::to_string(selectedInputFifoIndex) + ".rden");

                            instance.AddPort("input_fifo_empty_" + std::to_string(i),
                                             circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                             inputStructName + std::to_string(selectedInputFifoIndex) + ".empty");
                        }
                    }

                    if (GetCodeGenConfig()._stall > 0)
                    {
                        instance.AddPort("threshold_sel_in", circt::hw::ModulePort::Direction::Input, GetIntegerType(3),
                                         "stall_rate_in");

                        instance.AddPort("threshold_sel_valid_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                         "stall_rate_valid_in");
                    }

                    if (GetCodeGenConfig()._controlInspection && GetCodeGenConfig()._inspection)
                    {
                        instance.AddPort("control_state_out", circt::hw::ModulePort::Direction::Output,
                                         GetIntegerType(c_basicBlockControlWidth),
                                         GetBasicBlockControlStateName(basicBlock));
                    }
                    else
                    {
                        instance.AddPort("control_state_out", circt::hw::ModulePort::Direction::Output,
                                         GetIntegerType(c_basicBlockControlWidth));
                    }

                    instance.Generate();
                }

                CompileFunctionThreadCountCheck(function);
            }
        }
    }

    const size_t GetFunctionSemaphoreWidth(const Function& function)
    {
        return Log2RoundUp(function._maxThreadCountInsideFunction) + 1;
    }

    std::string GetSemaphoreName(const Function& function, const size_t index)
    {
        std::ostringstream str;

        str << "thread_count_" << FixupString(function._objectName) << "_" << FixupString(function._name) << "_"
            << index;

        return g_compiler->ClampStringLength(str.str());
    }

    void InstantiateFunctionThreadCounts()
    {
        for (const Function& function : _program._functions)
        {
            std::set<size_t> releasedSemaphores;

            for (const BasicBlock& bb : function._basicBlocks)
            {
                Union(releasedSemaphores, GetReleasedSemaphores(bb));
            }

            for (const size_t semaphoreIndex : function._semaphores)
            {
                const size_t maxThreadCount = function._maxThreadCountInsideFunction;

                const std::string structName = GetSemaphoreName(function, semaphoreIndex);

                _writer.Str() << "struct packed {";

                {
                    AutoIndent autoIndent(_writer);

                    _writer.Str() << "logic incr_count;";
                    _writer.Str() << "logic decr_count;";
                    _writer.Str() << "logic full;";
                }

                _writer.Str() << "} " << structName << ";";

                // Check to see if the release operation was optimized out
                // (for example, the function contains an infinite loop)
                if (releasedSemaphores.end() == releasedSemaphores.find(semaphoreIndex))
                {
                    _writer.Str() << "assign " << structName << ".decr_count = 1'b0;";
                }

                ModuleInstanceHelper instance(*this, GetUnknownLocation());
                instance.SetModuleName("KanagawaSemaphore");
                instance.AddU64Parameter("MAX_VALUE", maxThreadCount);
                instance.AddU64Parameter("SEM_DELAY", GetCodeGenConfig()._semaphoreDelay);
                instance.SetInstanceName(structName + "_counter");
                instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
                instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), GetResetReplica());
                instance.AddPort("incr_count_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 structName + ".incr_count");
                instance.AddPort("decr_count_in", circt::hw::ModulePort::Direction::Input, GetI1Type(),
                                 structName + ".decr_count");
                instance.AddPort("full_out", circt::hw::ModulePort::Direction::Output, GetI1Type(),
                                 structName + ".full");
                instance.Generate();
            }
        }
    }

    size_t GetSubFifoCount(const size_t registerIndex) { return 1; }

    std::pair<size_t, size_t> GetSubFifoRange(const size_t subFifoIndex, const size_t fifoWidth,
                                              const FifoType fifoType)
    {
        assert(0 == subFifoIndex);

        return std::pair<size_t, size_t>(0, fifoWidth);
    }

    // Determines how much to fan-out the reset signal
    // If the manual fanout control is disabled by setting command-line option:--no-reset-fanout,the replica is fixed to
    // 1
    size_t CountResetReplicas()
    {
        size_t result = 0;

        if (GetCodeGenConfig()._resetFanout)
        {
            // fifo mergers
            for (const FIFOMerger& fifoMerger : _program._fifoMergers)
            {
                assert(fifoMerger._sources.size() > 1);

                const size_t numInstances = fifoMerger._sources.size() - 1;
                const size_t numFifos = numInstances - 1;

                // There is one reset per KanagawaArbitrationChainNode
                result += numInstances;

                // There is one reset per intermediate fifo
                result += numFifos;
            }

            // loop generators
            result += _program._loopGenerators.size();

            // context savers
            result += _program._contextSavers.size();

            // Functions
            for (const Function& function : _program._functions)
            {
                if (!function.IsExtern())
                {
                    // 1 for each semaphore
                    result += function._semaphores.size();

                    // Basic blocks
                    for (const BasicBlock& basicBlock : function._basicBlocks)
                    {
                        result++;
                    }
                }
            }

            for (const Function* const function : _program._externFunctions)
            {
                const FunctionNode* const functionNode = function->_functionNode;

                // Synchronous extern functions require // Internal fifo that holds call index
                if (!(functionNode->GetModifiers() & ParseTreeFunctionModifierAsync))
                {
                    result++;
                }
            }

            for (size_t i = 0; i < _program._registerTable.size(); ++i)
            {
                const RegisterDescription& regDesc = _program._registerTable[i];

                if (regDesc._type == RegisterType::Global)
                {
                    // global variables with initial values
                    if (regDesc.Global()._hasInitialValue)
                    {
                        ++result;
                    }
                }
                else if (regDesc._type == RegisterType::Fifo)
                {
                    // FIFOs
                    switch (regDesc.Fifo()._type)
                    {
                    case FifoType::Default:
                    case FifoType::ReorderBuffer:
                    case FifoType::PassthroughRegistered:
                    {
                        const size_t subFifoCount = GetSubFifoCount(i);

                        result += subFifoCount;
                    }
                    break;
                    default:
                        break;
                    }
                }
                else if ((regDesc._type == RegisterType::Memory) && MemoryRequiresReset(regDesc))
                {
                    result++;
                }
            }

            // Inspectable variables
            result += _program._inspectableVariables.size();

            // Extern modules
            result += _program._externClassInstances.size();

            // KanagawaRaceCounter
            if (!_program._raceConditionRegisters.empty())
            {
                result++;
            }

            // Resets for other clock domains
            result += (MaxClockCount - 1);
        }
        else
        {
            // Each clock domain needs a reset fanout to avoid Lint error
            result = MaxClockCount;
        }

        return result;
    }

    std::string GetResetReplica()
    {
        std::ostringstream str;

        if (_resetReplicaIndex < MaxClockCount ||
            _resetReplicaIndex >= MaxClockCount && GetCodeGenConfig()._resetFanout)
        {
            str << "reg_rst_delayed[" << _resetReplicaIndex << "]";
            ++_resetReplicaIndex;
        }
        else
        {
            assert(_resetReplicaIndex == MaxClockCount);
            str << "reg_rst_delayed[" << _resetReplicaIndex - 1 << "]";
        }

        return str.str();
    }

    // Computes the number of cycles that the delayed (and replicated) reset signal must be held high
    // This is required by reset signals are not passed to thread-valid bits in basic blocks or flip-flop chains
    // FIFOs and global variables are reset this many cycles which threads drain from basic blocks and flip-flop chains
    size_t GetResetHoldCycles()
    {
        size_t result = 0;

        // Add user specified additional reset pipeline delay
        result += GetCodeGenConfig()._resetCycles;

        // Add user-specified reset cycles
        result += GetCodeGenConfig()._additionalResetCycles;

        // Add user-specified thread count semaphore delay
        result += GetCodeGenConfig()._semaphoreDelay;

        // flip-flop chains for backward signals (almost_full, semaphore decrement)
        result = std::max<size_t>(result, GetCodeGenConfig()._additionalLatency);

        // Minimum hold of 1 cycle
        result = std::max<size_t>(result, 1);

        // basic blocks
        for (const Function& function : _program._functions)
        {
            for (const BasicBlock& basicBlock : function._basicBlocks)
            {
                size_t numStages = basicBlock._stages.back()._atomicSequence;
                result = std::max<size_t>(result, numStages);
            }
        }

        // For rams with bypass, the wren input must be 0
        // to clear all bypass slots
        result += GetCodeGenConfig().GetMaxBypassSlots();

        // * 2 for good measure
        return result * 2;
    }

    // Returns all export functions that have ports in the core object
    std::vector<const Function*> GetCoreInterfaceExportFunctions()
    {
        std::vector<const Function*> result;

        for (const EntryPoint& entryPoint : _program._entryPoints)
        {
            for (Function* const function : entryPoint._instances)
            {
                if (function->_externClassInstance)
                {
                    // Extern modules are instantiated inside the core
                    continue;
                }

                result.push_back(function);
            }
        }

        return result;
    }

    struct ExportPortDesc
    {
        std::string _name;
        size_t _width;
        bool _input;
        const Type* _type;
        EsiPortSemantics _portSemantics;
        EsiChannelSemantics _channelSemantics;
        EsiChannelName _channelName;
        std::string _fieldName;

        ExportPortDesc() {}

        ExportPortDesc(const std::string& prefix, const std::string& name, const size_t width, const bool input,
                       const Type* type, const EsiPortSemantics portSemantics,
                       const EsiChannelSemantics channelSemantics, const EsiChannelName channelName)
            : _width(width), _input(input), _type(type), _portSemantics(portSemantics),
              _channelSemantics(channelSemantics), _channelName(channelName)
        {
            _name = prefix + "_" + name;

            if (input)
            {
                _name += "_in";
            }
            else
            {
                _name += "_out";
            }

            _fieldName = name;
        }
    };

    struct ExportInterface
    {
        std::string _name;
        std::vector<ExportPortDesc> _ports;
        std::optional<std::string> _esiBundleName;
    };

    std::vector<ExportInterface> GetExportInterfaces()
    {
        std::vector<ExportInterface> result;

        const std::vector<const Function*> functions = GetCoreInterfaceExportFunctions();

        for (const Function* const function : functions)
        {
            // Get unique name (for exported members)
            const std::string combinedFunctionName = GetFunctionCombinedName(function);

            ExportInterface exportInterface = {};
            exportInterface._name = combinedFunctionName;

            const FunctionNode* const functionNode = function->_functionNode;

            const bool hasBackpressure = !functionNode->NoBackpressure();

            const bool fixedLatency = functionNode->IsFixedLatency();

            if (hasBackpressure)
            {
                exportInterface._esiBundleName = combinedFunctionName;
            }

            {
                exportInterface._ports.emplace_back(combinedFunctionName, "valid", 1, true, g_compiler->GetBoolType(),
                                                    EsiPortSemantics::Valid, EsiChannelSemantics::ToGeneratedHw,
                                                    EsiChannelName::Arguments);

                for (size_t i = 0; i < functionNode->GetParameterCount(); i++)
                {
                    exportInterface._ports.emplace_back(combinedFunctionName, functionNode->GetParameterName(i),
                                                        functionNode->GetParameterWidth(i), true,
                                                        functionNode->GetParameterType(i), EsiPortSemantics::Payload,
                                                        EsiChannelSemantics::ToGeneratedHw, EsiChannelName::Arguments);
                }

                if (hasBackpressure)
                {
                    exportInterface._ports.emplace_back(combinedFunctionName, "rdy", 1, false,
                                                        g_compiler->GetBoolType(), EsiPortSemantics::Ready,
                                                        EsiChannelSemantics::ToGeneratedHw, EsiChannelName::Arguments);
                }
            }

            if (function->_returnFifoRegisterIndex != c_invalidAccessedRegisterIndex)
            {
                const RegisterDescription& outputRegDesc = _program._registerTable[function->_returnFifoRegisterIndex];
                assert(RegisterType::Fifo == outputRegDesc._type);

                if (hasBackpressure)
                {
                    exportInterface._ports.emplace_back(combinedFunctionName, "rden", 1, true,
                                                        g_compiler->GetBoolType(), EsiPortSemantics::ReadEnable,
                                                        EsiChannelSemantics::FromGeneratedHw, EsiChannelName::Results);
                    exportInterface._ports.emplace_back(combinedFunctionName, "empty", 1, false,
                                                        g_compiler->GetBoolType(), EsiPortSemantics::Empty,
                                                        EsiChannelSemantics::FromGeneratedHw, EsiChannelName::Results);
                }
                else if (!fixedLatency)
                {
                    exportInterface._ports.emplace_back(combinedFunctionName, "valid", 1, false,
                                                        g_compiler->GetBoolType(), EsiPortSemantics::Valid,
                                                        EsiChannelSemantics::FromGeneratedHw, EsiChannelName::Results);
                }

                if (outputRegDesc._width > 0)
                {
                    exportInterface._ports.emplace_back(combinedFunctionName, "result", outputRegDesc._width, false,
                                                        functionNode->GetReturnType(), EsiPortSemantics::Payload,
                                                        EsiChannelSemantics::FromGeneratedHw, EsiChannelName::Results);
                }
            }

            result.push_back(exportInterface);
        }

        return result;
    }

    void DeclareCorePorts(ModuleDeclarationHelper& coreModule, const size_t resetReplicas)
    {
        for (size_t i = 0; i < MaxClockCount; i++)
        {
            coreModule.AddPort(GetClockString(i), circt::hw::ModulePort::Direction::Input, GetI1Type());
        }

        // Reset for clk0
        coreModule.AddPort("rst", circt::hw::ModulePort::Direction::Input, 1);

        coreModule.AddPort("rst_and_startup_done_out", circt::hw::ModulePort::Direction::Output, 1);

        // For each exported function
        const std::vector<ExportInterface> exportInterfaces = GetExportInterfaces();

        for (const ExportInterface& exportInterface : exportInterfaces)
        {
            PushPopEsiBundle pushPopEsiBundle(coreModule, exportInterface._esiBundleName);

            for (const ExportPortDesc& port : exportInterface._ports)
            {
                coreModule.AddPort(port._name,
                                    port._input ? circt::hw::ModulePort::Direction::Input
                                                : circt::hw::ModulePort::Direction::Output,
                                    ToMlirType(port._type), port._type, port._portSemantics, port._channelSemantics,
                                    port._channelName, port._fieldName);
            }
        }

        // For each external function (referenced and not)
        // It is important to include unreferenced externs in the interface
        // to allow for a consistent interface to RTL
        std::list<const Function*> externsForInterface;

        for (const Function* const function : _program._externFunctions)
        {
            // Extern modules are instantiated inside the core
            if (!function->_externClassInstance)
            {
                externsForInterface.push_back(function);
            }
        }

        for (const Function* const function : _program._unreferencedExternFunctions)
        {
            if (!function->_externClassInstance)
            {
                externsForInterface.push_back(function);
            }
        }

        for (const Function* const function : externsForInterface)
        {
            assert(!function->_externClassInstance);

            const FunctionNode* const functionNode = function->_functionNode;
            const auto prefix = GetFunctionCombinedName(function);

            const bool isAsync = functionNode->GetModifiers() & ParseTreeFunctionModifierAsync;

            const bool isNoBackpressure = functionNode->GetModifiers() & ParseTreeFunctionModifierNoBackPressure;

            std::optional<std::string> esiBundleName;

            if (!isNoBackpressure)
            {
                esiBundleName = prefix;
            }

            PushPopEsiBundle pushPopEsiBundle(coreModule, esiBundleName);

            if (isNoBackpressure)
            {
                assert(isAsync);

                coreModule.AddPort(prefix + "_valid_out", circt::hw::ModulePort::Direction::Output, 1,
                                   EsiPortSemantics::Valid, EsiChannelSemantics::FromGeneratedHw,
                                   EsiChannelName::Arguments);
            }
            else
            {
                coreModule.AddPort(prefix + "_rden_in", circt::hw::ModulePort::Direction::Input, 1,
                                   EsiPortSemantics::ReadEnable, EsiChannelSemantics::FromGeneratedHw,
                                   EsiChannelName::Arguments);
            }

            for (size_t i = 0; i < functionNode->GetParameterCount(); i++)
            {
                const Type* type = functionNode->GetParameterType(i);
                coreModule.AddPort(prefix + "_" + functionNode->GetParameterName(i) + "_out",
                                    circt::hw::ModulePort::Direction::Output, ToMlirType(type), type,
                                    EsiPortSemantics::Payload, EsiChannelSemantics::FromGeneratedHw,
                                    EsiChannelName::Arguments, functionNode->GetParameterName(i));
            }

            if (!isNoBackpressure)
            {
                coreModule.AddPort(prefix + "_empty_out", circt::hw::ModulePort::Direction::Output, 1,
                                   EsiPortSemantics::Empty, EsiChannelSemantics::FromGeneratedHw,
                                   EsiChannelName::Arguments);
            }

            if (!isAsync)
            {
                coreModule.AddPort(prefix + "_valid_in", circt::hw::ModulePort::Direction::Input, 1,
                                   EsiPortSemantics::Valid, EsiChannelSemantics::ToGeneratedHw,
                                   EsiChannelName::Results);

                const size_t returnWidth = functionNode->GetReturnType()->GetBitWidth();

                if (returnWidth > 0)
                {
                    const Type* type = functionNode->GetReturnType();
                    coreModule.AddPort(prefix + "_result_in", circt::hw::ModulePort::Direction::Input,
                                        ToMlirType(type), type, EsiPortSemantics::Payload,
                                        EsiChannelSemantics::ToGeneratedHw, EsiChannelName::Results);
                }

                if (!isNoBackpressure)
                {
                    coreModule.AddPort(prefix + "_rdy_out", circt::hw::ModulePort::Direction::Output, 1,
                                       EsiPortSemantics::Ready, EsiChannelSemantics::ToGeneratedHw,
                                       EsiChannelName::Results);
                }
            }
        }

        // Add fixed-latency external functions instantiated outside of kanagawa
        for (const ExternalModuleCall& externModuleCall : _program._externalModuleCalls)
        {
            if (externModuleCall._type == ExternalModuleCallType::ExternallyInstantiated)
            {
                const auto callback = [&](const std::string& name, const circt::hw::ModulePort::Direction direction,
                                          const size_t width) { coreModule.AddPort(name, direction, width); };

                WriteExternalModuleCallPorts(callback, externModuleCall);
            }
        }

        if (GetCodeGenConfig()._inspection)
        {
            coreModule.AddPort("inspection_value_in", circt::hw::ModulePort::Direction::Input,
                            coreModule.GetInspectableTypeAlias());
            coreModule.AddPort("inspection_value_out", circt::hw::ModulePort::Direction::Output,
                            coreModule.GetInspectableTypeAlias());
        }

        WriteStallRatePorts(coreModule);
    }

    void ConnectOneExportFunction(ModuleDeclarationHelper& mod, const Function* const function)
    {
        const bool isExternalClassCallbackCallee = (function->_externClassInstance != nullptr);

        const size_t clockIndex = 0;

        const std::string combinedFunctionName = isExternalClassCallbackCallee
                                                     ? GetExternalClassInstanceSignalPrefix(function)
                                                     : GetFunctionCombinedName(function);

        const std::string inString(isExternalClassCallbackCallee ? "" : "_in");

        const std::string outString(isExternalClassCallbackCallee ? "" : "_out");

        const FunctionNode* const functionNode = function->_functionNode;

        // Reset functions have no FIFOs
        if ((function->_start->_inputFifoCount == 0) &&
            (function->_functionNode->GetModifiers() & ParseTreeFunctionModifierReset))
            return;

        assert(1 == function->_start->_inputFifoCount);
        const size_t FifoIndexIn = function->_start->_inputFifoIndices[0];
        const size_t FifoIndexOut = function->_returnFifoRegisterIndex;

        const size_t inputFifoIndex = FifoIndexIn;
        const size_t returnFifoIndex = FifoIndexOut;

        const bool isNoBackpressure = functionNode->GetModifiers() & ParseTreeFunctionModifierNoBackPressure;

        const bool isFixedLatency = functionNode->IsFixedLatency();

        if (inputFifoIndex != c_invalidAccessedRegisterIndex)
        {
            const RegisterDescription& inputRegDesc = _program._registerTable[inputFifoIndex];
            assert(RegisterType::Fifo == inputRegDesc._type);

            const bool isPassthroughFifo = (inputRegDesc.Fifo()._type == FifoType::Passthrough);
            const size_t inputFifoIndexNorm = _fifoNamer.GetNormFifoIndex(inputFifoIndex);

            std::ostringstream validWriteEnableStr;
            std::ostringstream rdyFullStr;
            std::ostringstream dataStr;

            if (inputRegDesc.Fifo()._type == FifoType::Passthrough)
            {
                validWriteEnableStr << "passthrough_data_" << inputFifoIndexNorm << ".valid";
                rdyFullStr << "passthrough_data_" << inputFifoIndexNorm << ".rdy_ext";
                dataStr << "passthrough_data_" << inputFifoIndexNorm << ".data";
            }
            else if (inputRegDesc.Fifo()._type == FifoType::PassthroughRegistered)
            {
                validWriteEnableStr << "passthrough_data_" << inputFifoIndexNorm << ".valid_next";
                rdyFullStr << "1'b1";
                dataStr << "passthrough_data_" << inputFifoIndexNorm << ".data_next";
            }
            else if (inputRegDesc.Fifo()._type == FifoType::PassthroughUnregistered)
            {
                validWriteEnableStr << "passthrough_data_" << inputFifoIndexNorm << ".valid";
                rdyFullStr << "1'b1";
                dataStr << "passthrough_data_" << inputFifoIndexNorm << ".data";
            }
            else
            {
                validWriteEnableStr << "fifo_data_" << inputFifoIndexNorm << ".wren";
                rdyFullStr << "~fifo_data_" << inputFifoIndexNorm << ".almost_full";
                dataStr << "fifo_data_" << inputFifoIndexNorm << ".data_in";
            }

            {
                std::ostringstream str;

                // For [[no_backpressure]] functions, create a signal since it is not a port
                if (isNoBackpressure)
                {
                    _writer.Str() << "logic " << combinedFunctionName << "_rdy" << outString << ";";
                }

                str << "assign " << mod.AssignPortOptional(combinedFunctionName + "_rdy" + outString) << " = "
                    << rdyFullStr.str();

                _writer.Str() << str.str() << ";";
            }

            // Filter the valid signal going to the basic block so that the basic block only sees it after the reset
            // sequence & startup() have completed
            _writer.Str() << "assign " << mod.AssignPortOptional(validWriteEnableStr.str()) << " = "
                          << combinedFunctionName << "_rdy" << outString << " & " << combinedFunctionName << "_valid"
                          << inString << ";";

            if (functionNode->GetParameterCount() > 0)
            {
                std::ostringstream str;

                // Concat all parameters
                str << "assign " << mod.AssignPortOptional(dataStr.str()) << " = { ";

                for (size_t invI = 0; invI < functionNode->GetParameterCount(); invI++)
                {
                    if (invI > 0)
                    {
                        str << ", ";
                    }

                    const size_t i = functionNode->GetParameterCount() - invI - 1;

                    str << combinedFunctionName << "_" << functionNode->GetParameterName(i) << inString;
                }

                str << " };";

                _writer.Str() << str.str();
            }
        }

        if (returnFifoIndex != c_invalidAccessedRegisterIndex)
        {
            const RegisterDescription& outputRegDesc = _program._registerTable[returnFifoIndex];
            assert(RegisterType::Fifo == outputRegDesc._type);

            const size_t returnFifoIndexNorm = _fifoNamer.GetNormFifoIndex(returnFifoIndex);

            if (outputRegDesc.Fifo()._type == FifoType::Passthrough)
            {
                assert(!isFixedLatency);

                if (outputRegDesc._width > 0)
                {
                    const std::string resultString("_result");
                    _writer.Str() << "assign "
                                  << mod.AssignPortOptional(combinedFunctionName + resultString + outString)
                                  << " = passthrough_data_" << returnFifoIndexNorm << ".data;";
                }

                {
                    std::ostringstream str;

                    str << "assign " << mod.AssignPortOptional(combinedFunctionName + "_valid" + outString)
                        << " = passthrough_data_" << returnFifoIndexNorm << ".valid;";

                    _writer.Str() << str.str() << ";";
                }
            }
            else if (outputRegDesc.Fifo()._type == FifoType::PassthroughRegistered)
            {
                assert(!isFixedLatency);

                if (outputRegDesc._width > 0)
                {
                    const std::string resultString("_result");
                    _writer.Str() << "assign "
                                  << mod.AssignPortOptional(combinedFunctionName + resultString + outString)
                                  << " = passthrough_data_" << returnFifoIndexNorm << ".data_ff;";
                }

                {
                    std::ostringstream str;

                    str << "assign " << mod.AssignPortOptional(combinedFunctionName + "_valid" + outString)
                        << " = passthrough_data_" << returnFifoIndexNorm << ".valid_ff;";

                    _writer.Str() << str.str() << ";";
                }
            }
            else if (outputRegDesc.Fifo()._type == FifoType::PassthroughUnregistered)
            {
                assert(isFixedLatency);

                if (outputRegDesc._width > 0)
                {
                    const std::string resultString("_result");
                    _writer.Str() << "assign "
                                  << mod.AssignPortOptional(combinedFunctionName + resultString + outString)
                                  << " = passthrough_data_" << returnFifoIndexNorm << ".data;";
                }

                // There is no valid-out signal for fixed-latency functions
            }
            else
            {
                assert(!isFixedLatency);

                _writer.Str() << "assign fifo_data_" << returnFifoIndexNorm << ".rden = " << combinedFunctionName
                              << "_rden" << inString << ";";

                if (outputRegDesc._width > 0)
                {
                    const std::string resultString("_result");
                    _writer.Str() << "assign "
                                  << mod.AssignPortOptional(combinedFunctionName + resultString + outString)
                                  << " = fifo_data_" << returnFifoIndexNorm << ".data_out;";
                }

                {
                    std::ostringstream str;

                    str << "assign " << mod.AssignPortOptional(combinedFunctionName + "_empty" + outString)
                        << " = fifo_data_" << returnFifoIndexNorm << ".empty";

                    _writer.Str() << str.str() << ";";
                }
            }
        }
    }

    // detect if there is memory initialization logic
    bool HasMemoryInitialization()
    {
        bool has_memory_init = false;
        for (size_t i = 0; i < _program._registerTable.size(); ++i)
        {
            const RegisterDescription& regDesc = _program._registerTable[i];

            if ((regDesc._type == RegisterType::Memory) && !regDesc.Memory()._initialValues.empty())
            {
                if (GetCodeGenDeviceConfig()._memoryInitFileType == MemoryInitFileType::Asic)
                {
                    has_memory_init = true;
                    break;
                }
            }
        }

        return has_memory_init;
    }

    void ConnectExportFifos(ModuleDeclarationHelper& mod)
    {
        // For each exported function
        const bool memoryInitFound = HasMemoryInitialization();

        for (const EntryPoint& entryPoint : _program._entryPoints)
        {
            for (Function* const function : entryPoint._instances)
            {
                ConnectOneExportFunction(mod, function);
            }
        }

        // For each external function
        for (const Function* const function : _program._externFunctions)
        {
            const bool inExternalClassInstance = (function->_externClassInstance != nullptr);

            const size_t clockIndex = 0;

            // Functions that are a part of extern modules have the module name prepended
            // also, "_in" and "_out" are not used because the extern module is declared inside the core
            const std::string rootReturnValueSourceName = inExternalClassInstance
                                                              ? GetExternalClassInstanceSignalPrefix(function)
                                                              : GetFunctionCombinedName(function);

            const std::string inString(inExternalClassInstance ? "" : "_in");

            const std::string outString(inExternalClassInstance ? "" : "_out");

            const FunctionNode* const functionNode = function->_functionNode;

            const bool isAsync = functionNode->GetModifiers() & ParseTreeFunctionModifierAsync;

            const bool isNoBackpressure = functionNode->GetModifiers() & ParseTreeFunctionModifierNoBackPressure;

            {
                assert(1 == function->_start->_inputFifoCount);
                const size_t inputFifoIndex = function->_start->_inputFifoIndices[0];
                const size_t inputFifoIndexNorm = _fifoNamer.GetNormFifoIndex(inputFifoIndex);

                const bool isPassthroughOutput =
                    FifoType::PassthroughRegistered == _program._registerTable[inputFifoIndex].Fifo()._type;

                std::string fifoStructName;
                std::string fifoMemberName;

                if (isPassthroughOutput)
                {
                    // This path is for no_backpressure outputs - the generated code pushes the data out
                    assert(isNoBackpressure);
                    assert(isAsync);

                    _writer.Str() << "assign "
                                  << mod.AssignPortOptional(rootReturnValueSourceName + "_valid" + outString)
                                  << " = passthrough_data_" << inputFifoIndexNorm << ".valid_ff;";

                    fifoStructName = "passthrough_data_";

                    fifoMemberName = "data_ff";
                }
                else
                {
                    // Connect input fifo directly to the output port
                    _writer.Str() << "assign fifo_data_" << inputFifoIndexNorm
                                  << ".rden = " << rootReturnValueSourceName << "_rden" << inString << ";";

                    fifoStructName = "fifo_data_";

                    fifoMemberName = "data_out";
                }

                // Output params
                size_t offset = 0;

                for (size_t i = 0; i < functionNode->GetParameterCount(); i++)
                {
                    const size_t width = functionNode->GetParameterWidth(i);

                    _writer.Str() << "assign "
                                  << mod.AssignPortOptional(rootReturnValueSourceName + "_" +
                                                            functionNode->GetParameterName(i) + outString)
                                  << " = " << fifoStructName << inputFifoIndexNorm << "." << fifoMemberName << "["
                                  << offset + width - 1 << ":" << offset << "];";

                    offset += width;
                }

                if (!isPassthroughOutput)
                {
                    // Generate the name of the empty signal for the FIFO that feeds the external module
                    std::ostringstream externalModuleInputFifoEmptySignal;
                    externalModuleInputFifoEmptySignal << "fifo_data_" << inputFifoIndexNorm << ".empty";

                    if (isAsync)
                    {
                        _writer.Str() << "assign "
                                      << mod.AssignPortOptional(rootReturnValueSourceName + "_empty" + outString)
                                      << " = " << externalModuleInputFifoEmptySignal.str() << ";";
                    }
                    else
                    {
                        // Connect the external module input FIFO's empty back to the external module
                        _writer.Str() << "assign "
                                      << mod.AssignPortOptional(rootReturnValueSourceName + "_empty" + outString)
                                      << " = " << externalModuleInputFifoEmptySignal.str() << ";";

                        // Get the root name for the associated extern return router
                        const std::string rootExternReturnRouterName = GetExternReturnRouterRootName(function);

                        // Connect extern return router data output fifo interfaces to their associated FIFOs
                        // Note that the order of the port connections here is linear, which matches the order used in
                        // the instantiation of the
                        //  KanagawaExternReturnRouter above in InstantiateFifos().  It is important that they match so
                        //  that the correct _callSiteIndex mapping will be used above.
                        size_t stPortNum = 0;
                        for (const ExternReturnDesc& externReturnDesc : function->_syncExtern._returnDesc)
                        {
                            const RegisterDescription& returnFifoDesc =
                                _program._registerTable[externReturnDesc._fifoIndex];
                            assert(RegisterType::Fifo == returnFifoDesc._type);

                            const size_t fifoIndexNorm = _fifoNamer.GetNormFifoIndex(externReturnDesc._fifoIndex);

                            const std::string portValidName = rootExternReturnRouterName + "_switch_output_intfs[" +
                                                              std::to_string(stPortNum) + "].valid";
                            const std::string portReadyName = rootExternReturnRouterName + "_switch_output_intfs[" +
                                                              std::to_string(stPortNum) + "].ready";
                            const std::string portDataName = rootExternReturnRouterName + "_switch_output_intfs[" +
                                                             std::to_string(stPortNum) + "].data";
                            _writer.Str() << "assign fifo_data_" << fifoIndexNorm << ".wren = " << portValidName
                                          << " & " << portReadyName << ";";
                            _writer.Str() << "assign " << mod.AssignPortOptional(portReadyName) << " = ~fifo_data_"
                                          << fifoIndexNorm << ".almost_full;";

                            if (returnFifoDesc._width > 0)
                            {
                                _writer.Str()
                                    << "assign fifo_data_" << fifoIndexNorm << ".data_in = " << portDataName << ";";
                            }

                            stPortNum++;
                        }
                    }
                }
            }
        }

        // Hook up signals for unreferenced external functions
        for (const Function* const function : _program._unreferencedExternFunctions)
        {
            const FunctionNode* const functionNode = function->_functionNode;

            const bool inExternalClassInstance = (function->_externClassInstance != nullptr);

            const std::string outString(inExternalClassInstance ? "" : "_out");

            const std::string prefix(inExternalClassInstance ? GetExternalClassInstanceSignalPrefix(function)
                                                             : GetFunctionCombinedName(function));

            const bool isAsync = functionNode->GetModifiers() & ParseTreeFunctionModifierAsync;
            const bool isNoBackpressure = functionNode->GetModifiers() & ParseTreeFunctionModifierNoBackPressure;

            if (!isNoBackpressure)
            {
                _writer.Str() << "assign " << mod.AssignPortOptional(prefix + "_empty" + outString) << " = 1'b1;";
            }

            if (!isAsync)
            {
                _writer.Str() << "assign " << mod.AssignPortOptional(prefix + "_rdy" + outString) << " = 1'b0;";
            }
        }
    }

    void DeclareCore(const size_t resetReplicas, const std::set<size_t>& globalsRequiringNext,
                     mlir::ModuleOp* const mlirModule)
    {
        std::string moduleStr = GetModuleName();

        ModuleDeclarationHelper coreModule(_writer, moduleStr, GetUnknownLocation(), GetCirctDesignName(), mlirModule);

        assert(!_coreModule);

        _coreModule = &coreModule;

        ExitScope nullCoreModule([this]() { _coreModule = nullptr; });

        // Emit typedefs for structs referenced by ports on this module
        // The same symbol is used for all export classes
        // to ensure that the generate `ifndef _TYPESCOPE_* macros all agree
        coreModule.AddTypedefs("CoreModuleTypeScope");

        // Input and outputs of the KanagawaCore module
        DeclareCorePorts(coreModule, resetReplicas);

        coreModule.FinishPorts();

        DeclareDebugVariables();

        DeclareStringTable();

        mlir::Value hasMemInitCompleted;
        // KanagawaResetControl implements the reset strategy
        {
            circt::OpBuilder opb = circt::OpBuilder::atBlockEnd(_coreModule->GetBodyBlock());
            circt::BackedgeBuilder beb(opb, GetUnknownLocation());
            circt::Backedge bebHasMemInitCompleted = beb.get(opb.getI1Type());
            InstantiateResetControlCIRCT(coreModule, resetReplicas, bebHasMemInitCompleted);
            hasMemInitCompleted = InstantiateMemoriesCIRCT();
            bebHasMemInitCompleted.setValue(hasMemInitCompleted);
        }

        // Modules that count the number of threads in a function
        InstantiateFunctionThreadCounts();

        // Global and member variables
        DeclareAndInstantiateGlobalVariablesCIRCT(globalsRequiringNext);

        // Debug network
        if (GetCodeGenConfig()._inspection)
        {
            InstantiateInspectables(coreModule, globalsRequiringNext);
        }

        // Coverpoints for code coverage
        if (GetCodeGenConfig()._codeCoverage && _program._codeCoverageVariables.size() > 0)
        {
            InstantiateCoverpoints(coreModule, globalsRequiringNext);
        }

        InstantiateExternalClassInstances(coreModule);

        InstantiateFifos(coreModule);

        ConnectExportFifos(coreModule);

        InstantiateFifoMergers();

        const std::map<size_t, size_t> loopGeneratorMap = InstantiateLoopGenerators();

        InstantiateContextSavers();

        InstantiateBasicBlocks(coreModule, globalsRequiringNext, hasMemInitCompleted, loopGeneratorMap);

        // Generate logic to automically call functions with the [[reset]] attribute
        WaitForResetFunctions();

        AssignStallRatePorts(coreModule);

        {
            circt::OpBuilder opb = circt::OpBuilder::atBlockEnd(_coreModule->GetBodyBlock());
            _globalBatchAssignments.Flush(opb, GetUnknownLocation());
        }

        coreModule.Finish();

        coreModule.EmitEsiWrapper(GetCirctDesignName());
    }

    void InstantiateInitialAssertionsBlock()
    {
        // You can add validating assertions here - for example, that assumptions in the
        // code generator match up with values in the supporting SystemVerilog
        {
            AutoSectionRAII initialBlock(_writer, true, "initial begin", "end");
            // Put assertions here
        }
    }

    void DeclareInspectionVariables(bool declareChainEnds)
    {
        assert(GetCodeGenConfig()._inspection);

        // Both ends of the inspection value chain
        _writer.Str() << "KanagawaTypes::InspectableValue inspection_value_input_ff;";
        _writer.Str() << "KanagawaTypes::InspectableValue inspection_value_output;";
        if (declareChainEnds)
        {
            _writer.Str() << "KanagawaTypes::InspectableValue inspection_value_in;";
            _writer.Str() << "KanagawaTypes::InspectableValue inspection_value_out;";
        }
    }

    void ConnectStallRateVariables()
    {
        _writer.Str() << ".stall_rate_in(stall_rate_in),";
        _writer.Str() << ".stall_rate_valid_in(stall_rate_valid_in),";
        _writer.Str() << ".stall_rate_supported_out(),";
    }

    void DeclareDebugVariables() { _writer.Str() << "logic [31:0] dropped_commands;"; }

    void DeclareStringTable()
    {
        AutoSectionRAII translateBlock(_writer, false, "//synopsys translate_off", "//synopsys translate_on");

        _writer.Str() << "KanagawaStringTable string_table(.clk(clk));";
    }

    void GetFunctionDesc(const Function* const function, std::set<const Type*>& types)
    {
        const FunctionNode* const functionNode = function->_functionNode;

        types.insert(functionNode->GetReturnType());

        for (size_t i = 0; i < functionNode->GetParameterCount(); i++)
        {
            const DeclareNode* const parameterDeclareNode = functionNode->GetParameterDeclareNode(i);

            types.insert(parameterDeclareNode->GetDeclaredType());
        }
    }

    std::string GetPackageName()
    {
        std::ostringstream str;

        str << GetModuleName() << "Types";

        return str.str();
    }

    void WritePackage(SourceWriter& writer)
    {
        const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

        // Constants
        AutoSectionRAII packageBlock(writer, false, "package " + GetPackageName() + ";", "endpackage");

        // Write SystemVerilog versions of all exported types - in order
        for (const Type* const type : _program._exportedTypes)
        {
            type->ExportVerilog(writer);
        }

        for (const auto t : _program._exportedTypedefs)
        {
            t.second->ExportVerilogTypedef(writer, t.first);
        }
    }

    std::string GetDebugViewModuleName(const Operation& op)
    {
        assert(Opcode::DebugView == op._opcode);

        std::ostringstream str;
        str << GetModuleName() << "DebugView_" << op._flags._debugView->_label;

        // Clamp string length to avoid synthesis tool limits on module names
        return FixupString(g_compiler->ClampStringLength(str.str()));
    }

    void DeclareDebugViewModules()
    {
        for (const Function& function : _program._functions)
        {
            for (const BasicBlock& basicBlock : function._basicBlocks)
            {
                for (const Stage& stage : basicBlock._stages)
                {
                    for (const Operation& op : stage._operations)
                    {
                        if (Opcode::DebugView == op._opcode)
                        {
                            const std::string moduleName = GetDebugViewModuleName(op);

                            _writer.Str() << "module " << moduleName;

                            _writer.Str() << "(";

                            {
                                AutoIndent autoIndent(_writer);

                                _writer.Str() << "input wire clk,";

                                for (size_t i = 0; i < op._flags._debugView->_arguments.size(); i++)
                                {
                                    const DebugViewArgument& debugViewArgument = op._flags._debugView->_arguments[i];

                                    // When using circts, ports are declared as fixed-with wires, not with types from
                                    // packages to avoid simulation errors related to casting
                                    const std::string portTypeName = (std::string("wire") + OptionalWidthDeclaration(debugViewArgument.Width()));

                                    // An underscore is added to the argument name, to ensure the name does not conflict
                                    // with SystemVerilog reserved keywords
                                    _writer.Str() << "input " << portTypeName << " _" << debugViewArgument._name << ",";
                                }

                                _writer.Str() << "input wire valid,";
                                _writer.Str() << "output logic valid_out";
                            }

                            {
                                AutoSectionRAII moduleClose(_writer, true, ");", "endmodule");

                                {
                                    // Optionally print values with $display
                                    AutoSectionRAII ifDefBlock(_writer, true, "`ifdef PIPEDREAM_PRINT_DEBUG_VIEW",
                                                               "`endif // PIPEDREAM_PRINT_DEBUG_VIEW");
                                    AutoSectionRAII alwaysFFBlock(_writer, true, "always_ff @(posedge clk) begin",
                                                                  "end");

                                    std::ostringstream formatStr;
                                    std::ostringstream dataStr;

                                    formatStr << "%0t";
                                    dataStr << "$time";

                                    for (size_t i = 0; i < op._flags._debugView->_arguments.size(); i++)
                                    {
                                        const DebugViewArgument& debugViewArgument =
                                            op._flags._debugView->_arguments[i];

                                        formatStr << " " << debugViewArgument._name << ": %p";
                                        dataStr << ", _" << debugViewArgument._name;
                                    }

                                    _writer.Str() << "if (valid) $display(\"" << op._flags._debugView->_label
                                                  << " time: " << formatStr.str() << "\", " << dataStr.str() << ");";
                                }

                                // Add an unused assignment to avoid synthesis tools being confused
                                // by an empty module.
                                _writer.Str() << "assign valid_out = valid;";
                            }
                        }
                    }
                }
            }
        }
    }

    void Compile()
    {
        if (!GetCodeGenConfig()._noVerilogHeader)
        {
            _writer.Str() << "// This file was generated. Do not modify.";
            _writer.Str() << "// " << GetCodeGenConfig()._cmdArgs << std::endl;
        }

        _writer.Str() << "`default_nettype wire";

        // Write package
        FileSourceWriter writer(_svPackageFileName.c_str());

        WritePackage(writer);

        // Global functions

        assert(!_mlirModule);
        std::unique_ptr<MlirModule> mlirModule;

        {
            // Create the top-level MLIR module operation
            // which contains all IR
            mlirModule = std::make_unique<MlirModule>(GetCirctDesignName());

            _mlirModule = mlirModule->Module();
        }

        ExitScope nullMlirModule([this]() { _mlirModule = nullptr; });

        // Declare a module for each debug view
        if (!GetCodeGenConfig()._suppressDebugView)
        {
            DeclareDebugViewModules();
        }

        const std::set<size_t> globalsRequiringNext = GetGlobalRequireNext(_program);

        const size_t rawResetReplicas = CountResetReplicas();

        // If rawResetReplicas == 0, declare KanagawaResetControl with width = 1
        // because it has ports of that width
        const size_t resetReplicas = (rawResetReplicas == 0) ? 1 : rawResetReplicas;

        JsonValue jsonBasicBlocks = JsonValue::CreateArray();

        // Declare a module for each basic block
        for (const Function& function : _program._functions)
        {
            if (function.IsExtern())
            {
            }
            else
            {
                for (const BasicBlock& basicBlock : function._basicBlocks)
                {
                    CompileBasicBlock(basicBlock, globalsRequiringNext, jsonBasicBlocks);
                }
            }
        }

        _rtlMap.AddMember("basic_blocks", jsonBasicBlocks);

        {
            mlir::OpBuilder opb = mlir::OpBuilder::atBlockEnd(mlirModule->Module().getBody());
            mlir::Location loc = GetUnknownLocation();
            StringSourceWriter verbSourceWriter;
            _writer.BeginRedirect(verbSourceWriter);

            // Declare context saver modules
            DeclareContextSaverMergers();

            // Declare ASIC memory init
            DeclareMemInitLutModules();

            _writer.EndRedirect();

            VerbatimWriter vw(opb, loc);
            vw << verbSourceWriter.GetString();
        }

        // Declare a module which has interfaces for all extern/export methods
        DeclareCore(resetReplicas, globalsRequiringNext, mlirModule.get() ? &mlirModule->Module() : nullptr);

        {
            addPipelineSrcs(mlirModule->Module());

            // Write out the CIRCT IR.
            if (GetCodeGenConfig()._serializeCIRCTIR)
            {
                std::error_code ec;
                llvm::raw_fd_ostream circtIRFile(_circtAsmFileName, ec);
                if (ec)
                {
                    throw std::runtime_error("Failed to open circt.mlir for writing: " + ec.message());
                }
                mlir::OpPrintingFlags opPrintingFlag = {};
                opPrintingFlag.enableDebugInfo();
                mlirModule->Module().print(circtIRFile, opPrintingFlag);
            }

            // In-memory IR can work fine, but the IR is unable to be serialized and then deserialized
            // so check that it can be serialized/deserialized here (to avoid regressing the ability to dump valid CIRCT
            // IR)

#ifndef KANAGAWA_SKIP_CONSISTENCY_CHECKS
            mlirModule->VerifyRoundTrip();
#endif // KANAGAWA_SKIP_CONSISTENCY_CHECKS

            // Convert MLIR to a SystemVerilog string
            _writer.Str() << mlirModule->Generate();
        }

        assert(_globalBatchAssignments.Empty());

        // Verify CountResetReplicas computed the correct value
        assert(_resetReplicaIndex == rawResetReplicas);
    }

  private:
    // Returns the path from sourcePath to a particular global register container
    mlir::Value GetPathToGlobalContainer(ModuleDeclarationHelper& mod, const ObjectPath& sourcePath,
                                         const size_t registerIndex, const std::set<size_t>& globalsRequiringNext)
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];

        circt::OpBuilder& opb = mod.OpBuilder();

        return GetPathOp(opb, sourcePath, regDesc.Global()._containerInstancePath,
                         GetGlobalRegKey(registerIndex, globalsRequiringNext).Name(), GetCirctDesignName());
    }

    // Returns the path from sourcePath to a particular memory container
    mlir::Value GetPathToMemoryContainer(ModuleDeclarationHelper& mod, const ObjectPath& sourcePath,
                                         const size_t registerIndex)
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];

        circt::OpBuilder& opb = mod.OpBuilder();

        return GetPathOp(opb, sourcePath, regDesc.Memory()._containerInstancePath, regDesc._name, GetCirctDesignName());
    }

    // Returns the name of the struct that holds global input/output data
    // Unique names are used to enable scenarios such as coverage exclusions from TCL scripts
    std::string GetGlobalStructName(const size_t registerIndex) { return UniqueRegisterName(registerIndex) + "_data_"; }

    std::string GetModuleName() { return _program._moduleName; }

    void WriteExternalModuleCallPorts(
        const std::function<void(const std::string&, const circt::hw::ModulePort::Direction, const size_t)>& callback,
        const ExternalModuleCall& externalModuleCall)
    {
        assert(externalModuleCall._type != ExternalModuleCallType::InstantiateInBasicBlock);

        const std::string prefix = (externalModuleCall._type == ExternalModuleCallType::ExternClassMethod)
                                       ? externalModuleCall.GetFullyQualifiedName()
                                       : externalModuleCall._name;

        callback(prefix + "_valid_out", circt::hw::ModulePort::Direction::Output, 1);

        const FunctionDesc& functionDesc = externalModuleCall._functionDesc;

        assert(functionDesc._parameterTypes.size() == functionDesc._parameterNames.size());

        for (size_t i = 0; i < functionDesc._parameterTypes.size(); i++)
        {
            const Type* const type = functionDesc._parameterTypes[i];
            const std::string& name = functionDesc._parameterNames[i];

            callback(prefix + "_" + name + "_out", circt::hw::ModulePort::Direction::Output, type->GetBitWidth());
        }

        if (functionDesc._returnType->GetBitWidth() > 0)
        {
            callback(prefix + "_result_in", circt::hw::ModulePort::Direction::Input,
                     functionDesc._returnType->GetBitWidth());
        }
    }

    // Returns a string associated with the CIRCT design
    // All modules generated by non-top-level containers have this prefix added
    std::string GetCirctDesignName() { return GetModuleName(); }

    std::string GetModuleNamePrefix()
    {
        std::ostringstream str;

        str << GetModuleName() << "_";

        return str.str();
    }

    bool OperationRequiresInputValidReplica(const Operation& op) { return (Opcode::Enqueue == op._opcode); }

    // Adds assertions to the code to ensure that the maximum thread count
    // of a function is not exceeded.
    // This is useful to verify that the compiler optimization that removes unnecessary semaphores
    // is does not remove a necessary semaphore
    void CompileFunctionThreadCountCheck(const Function& function)
    {
        // Nothing to do for empty functions
        if (function._basicBlocks.empty())
        {
            return;
        }

        // Don't add assertions if the function is guarded by a semaphore
        // because the assertion may be overly conservative
        // For examples, functions with many return locations may release the semaphore
        // before the end of the last basic block
        if (!function._semaphores.empty())
        {
            return;
        }
    }

    inline std::string StallLfsrEnableForThreadRate(const BasicBlock& basicBlock) const
    {
        std::ostringstream ostr;

        if (basicBlock.GetThreadRate() != 1)
        {
            ostr << "fiber_index_0_ff == " << GetFiberIndexWidth(basicBlock) << "'d"
                 << (basicBlock.GetThreadRate() - 1);
        }
        else
        {
            ostr << "1'b1";
        }

        return ostr.str();
    }

    enum class WritePortCirctBehavior
    {
        Ignore,

        // Add the port to the circt-generated module
        // Do not pipeline the value through the basic block
        Add_NoPipeline,

        // Add the port to the circt-generated module
        // Do pipeline the value through the basic block
        Add_Pipeline,
    };

    void WritePortAndAddToJson(const circt::hw::ModulePort::Direction direction, const std::string& str,
                               const size_t width, const bool isLast, JsonValue& jsonPorts,
                               const bool isOptionalWidth,
                               const WritePortCirctBehavior circtBehavior = WritePortCirctBehavior::Ignore)
    {
        const std::string prefix =
            direction == circt::hw::ModulePort::Direction::Input ? "input wire " : "output logic ";

        const std::string trailingComma = isLast ? std::string("") : std::string(",");

        AddNetToJson(str, width, jsonPorts);

        if (WritePortCirctBehavior::Ignore != circtBehavior)
        {
            // CIRCT port information
            const BasicBlockPortInfo pi = {{StringToStringAttr(str), GetIntegerType(width), direction},

                                           WritePortCirctBehavior::Add_Pipeline == circtBehavior};

            _compileContext._basicBlockPorts.push_back(pi);
        }
    }

    void CompileBasicBlock(const BasicBlock& basicBlock, const std::set<size_t>& globalsRequiringNext,
                           JsonValue& jsonBasicBlocks)
    {
        const std::set<size_t> readGlobals = GetGlobalsReadWithoutNext(_program, basicBlock, globalsRequiringNext);
        const std::set<size_t> readGlobalsWithNext = GetGlobalsReadWithNext(_program, basicBlock, globalsRequiringNext);
        const std::set<std::pair<size_t, size_t>> writtenGlobals = GetGlobalsWritten(_program, basicBlock);
        const std::set<size_t> readGlobalViews = GetGlobalViewsRead(_program, basicBlock);
        const std::set<size_t> writtenFifos = GetWrittenFifos(basicBlock);
        const std::set<size_t> backpressureFifos = GetBackpressureFifos(_program, basicBlock);
        const std::set<MemoryAccessRecord> writtenMemories = GetMemoriesWritten(_program, basicBlock);
        const std::set<MemoryAccessRecord> readMemories = GetMemoriesRead(_program, basicBlock);

        const std::set<size_t> acquiredSemaphores = GetAcquiredSemaphores(basicBlock);
        const std::set<size_t> releasedSemaphores = GetReleasedSemaphores(basicBlock);

        const std::string fullModuleName = GetModuleNamePrefix() + GetBasicBlockName(basicBlock);

        JsonValue jsonBasicBlock = JsonValue::CreateObject();
        JsonValue jsonPorts = JsonValue::CreateArray();
        jsonBasicBlock.AddMember("module", _rtlMap.SerializeString(fullModuleName));
        jsonBasicBlock.AddMember("instance", _rtlMap.SerializeString(GetBasicBlockInstanceName(basicBlock)));

        assert(!_compileContext._basicBlock);
        assert(!_compileContext._basicBlockDebugSignals);
        assert(!_compileContext._basicBlockDebugInstances);
        assert(!_compileContext._basicBlockConstantSignals);
        assert(!_compileContext._basicBlockUnusedSignals);
        assert(_compileContext._basicBlockPorts.empty());
        assert(_compileContext._bypassStoreMap.empty());

        _compileContext._basicBlock = &basicBlock;

        _compileContext._bypassStoreMap = ComputeBypassStoreMap(basicBlock);

        _compileContext._writtenFifoMap = SetToIndexedMap(GetWrittenAndBackpressureFifos(_program, basicBlock));

        _compileContext._latencyMap = GetExtraLatencyMap(basicBlock, OperationEnumerationMode::Scheduled, _program);

        // Setup basic block ports which are common across all CIRCT-generated hardware modules
        {
            assert(_compileContext._basicBlockPorts.empty());

            _compileContext._basicBlockPorts.resize(static_cast<size_t>(CirctModulePort::Count));

            mlir::IntegerType i1Type = GetIntegerType(1);

            const BasicBlockPortInfo clockPort = {
                {StringToStringAttr("clk"), GetClockType(), circt::hw::ModulePort::Direction::Input}, false};

            const BasicBlockPortInfo resetPort = {
                {StringToStringAttr("rst"), i1Type, circt::hw::ModulePort::Direction::Input}, false};

            const BasicBlockPortInfo goPort = {
                {StringToStringAttr("go_in"), i1Type, circt::hw::ModulePort::Direction::Input}, true};

            const BasicBlockPortInfo donePort = {
                {StringToStringAttr("done_out"), i1Type, circt::hw::ModulePort::Direction::Output}, false};

            _compileContext._basicBlockPorts[static_cast<size_t>(CirctModulePort::Clock)] = clockPort;
            _compileContext._basicBlockPorts[static_cast<size_t>(CirctModulePort::Reset)] = resetPort;
            _compileContext._basicBlockPorts[static_cast<size_t>(CirctModulePort::Done)] = donePort;
        }

        // Debug signals declared inside the basic block
        JsonValue jsonBasicBlockDebugSignals = JsonValue::CreateArray();
        _compileContext._basicBlockDebugSignals = &jsonBasicBlockDebugSignals;

        JsonValue jsonBasicBlockDebugInstances = JsonValue::CreateArray();
        _compileContext._basicBlockDebugInstances = &jsonBasicBlockDebugInstances;

        JsonValue jsonBasicBlockConstantSignals = JsonValue::CreateArray();
        _compileContext._basicBlockConstantSignals = &jsonBasicBlockConstantSignals;

        JsonValue jsonBasicBlockUnusedSignals = JsonValue::CreateArray();
        _compileContext._basicBlockUnusedSignals = &jsonBasicBlockUnusedSignals;

        // Get the set of atomic sequence numbers associated with pipeline stages
        // Also get the auto pipeline scale for each stage, and a mapping of pipeline stage number to Stage*
        std::set<size_t> stageNumbersWithoutFirst;
        std::set<size_t> stageNumbersIncludingFirst;
        std::map<size_t, const Stage*> stageNumToStageMap;

        for (const Stage& stage : basicBlock._stages)
        {
            // There is no flip-flop for stage 0 valid bits
            if (stage._atomicSequence != 0)
            {
                stageNumbersWithoutFirst.insert(stage._atomicSequence);
            }

            stageNumbersIncludingFirst.insert(stage._atomicSequence);

            stageNumToStageMap[stage._atomicSequence] = &stage;
        }

        // Count the number of input valid replicas needed for each stage
        std::map<size_t, size_t> inputValidReplicaCountMap;

        for (const size_t stage : stageNumbersWithoutFirst)
        {
            inputValidReplicaCountMap[stage] = 0;
        }

        for (const Stage& stage : basicBlock._stages)
        {
            // Input-valid bit is not replicated for stage 0
            if (stage._atomicSequence != 0)
            {
                size_t replicaCount = 0;

                for (const Operation& op : stage._operations)
                {
                    if (OperationRequiresInputValidReplica(op))
                    {
                        ++replicaCount;
                    }
                }

                inputValidReplicaCountMap[stage._atomicSequence] += replicaCount;
            }
        }

        std::unique_ptr<ConstantSignalHelper> portConstantBitsHelper =
            std::make_unique<ConstantSignalHelper>(*this, *_compileContext._basicBlockConstantSignals, "rst", false);

        {
            AutoIndent autoIndent(_writer);

            // Get the size of the input data
            assert(!basicBlock._stages.empty());

            for (const size_t semaphoreIndex : acquiredSemaphores)
            {
                // Input/output port for thread count
                const size_t bits = GetFunctionSemaphoreWidth(*basicBlock._function);

                const std::string s = "semaphore_full_" + std::to_string(semaphoreIndex) + "_in";
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, s, 1, false, jsonPorts, true,
                                      WritePortCirctBehavior::Add_NoPipeline);
                const std::string r = "incr_semaphore_thread_count_" + std::to_string(semaphoreIndex) + "_out";
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, r, 1, false, jsonPorts, true,
                                      WritePortCirctBehavior::Add_NoPipeline);
            }

            for (const size_t releasedSemaphoreIndex : releasedSemaphores)
            {
                // Output port to decrement thread count
                const std::string s = "decr_semaphore_thread_count_" + std::to_string(releasedSemaphoreIndex) + "_out";
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, s, 1, false, jsonPorts, true,
                                      WritePortCirctBehavior::Add_NoPipeline);
            }

            // Input ports for global variables that are read
            for (const size_t i : readGlobals)
            {
                const RegisterDescription& globalRegDesc = _program._registerTable[i];
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, GetGlobalInName(i), globalRegDesc._width,
                                      false, jsonPorts, false, WritePortCirctBehavior::Add_NoPipeline);
            }

            // When using CIRCT, global views are implemented inside of the basic block logic
            // When adding ports for global inputs with next
            // also add globals read by global views in this block
            const std::set<size_t> readGlobalsWithNextIncGlobalViewInputs =
                GetReadGlobalsWithNextIncGlobalViewInputs(readGlobalsWithNext, readGlobalViews);

            // Input ports for global variables that are read
            for (const size_t i : readGlobalsWithNextIncGlobalViewInputs)
            {
                const RegisterDescription& globalRegDesc = _program._registerTable[i];

                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, GetGlobalInName(i), globalRegDesc._width,
                                      false, jsonPorts, false, WritePortCirctBehavior::Add_NoPipeline);
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, GetGlobalInNextName(i),
                                      globalRegDesc._width, false, jsonPorts, false,
                                      WritePortCirctBehavior::Add_NoPipeline);
            }

            // Output ports for global variables that are written
            for (const std::pair<size_t, size_t>& p : writtenGlobals)
            {
                const size_t registerIndex = p.first;
                const size_t writeIndex = p.second;

                const RegisterDescription& globalRegDesc = _program._registerTable[registerIndex];
                const RegisterDescription::GlobalDesc& globalDesc = globalRegDesc.Global();

                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output,
                                      GetGlobalValidOutName(registerIndex, writeIndex), 1, false, jsonPorts, true,
                                      WritePortCirctBehavior::Add_NoPipeline);

                if (globalDesc._literalValues.end() == globalDesc._literalValues.find(writeIndex))
                {
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output,
                                          GetGlobalOutName(registerIndex, writeIndex), globalRegDesc._width, false,
                                          jsonPorts, true, WritePortCirctBehavior::Add_NoPipeline);
                }
            }

            // Ports for memories that are accessed
            for (const MemoryAccessRecord& record : writtenMemories)
            {
                // Combine register index and port index
                std::ostringstream str;
                str << record._memoryIndex << "_" << record._portIndex;

                const RegisterDescription& memoryDesc = _program._registerTable[record._memoryIndex];

                const size_t addrWidth = memoryDesc.GetMemoryAddressWidth();
                const std::string s = "memory_write_data_out_" + str.str();
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, s, memoryDesc.Memory()._elementWidth,
                                      false, jsonPorts, true, WritePortCirctBehavior::Add_NoPipeline);
                const std::string r = "memory_write_addr_out_" + str.str();
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, r, addrWidth, false, jsonPorts, true,
                                      WritePortCirctBehavior::Add_NoPipeline);
                const std::string t = "memory_wren_" + str.str();
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, t, 1, false, jsonPorts, true,
                                      WritePortCirctBehavior::Add_NoPipeline);
            }

            for (const MemoryAccessRecord& MemoryAccessRecord : readMemories)
            {
                const size_t registerIndex = MemoryAccessRecord._memoryIndex;
                const size_t port = MemoryAccessRecord._portIndex;

                const RegisterDescription& memoryDesc = _program._registerTable[registerIndex];

                const size_t addrWidth = memoryDesc.GetMemoryAddressWidth();

                const std::string s =
                    "memory_read_data_in_" + std::to_string(registerIndex) + "_" + std::to_string(port);
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, s, memoryDesc.Memory()._elementWidth,
                                      false, jsonPorts, false, WritePortCirctBehavior::Add_NoPipeline);
                const std::string r =
                    "memory_read_addr_out_" + std::to_string(registerIndex) + "_" + std::to_string(port);
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, r, addrWidth, false, jsonPorts, true,
                                      WritePortCirctBehavior::Add_NoPipeline);
                // Don't create rden port for the container when memory type is KanagawaLogicRam
                if (!memoryDesc.Memory()._useLogicRam)
                {
                    const std::string t =
                        "memory_rden_out_" + std::to_string(registerIndex) + "_" + std::to_string(port);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, t, 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);
                }

                if (memoryDesc.Memory()._ecc)
                {
                    const std::string s =
                        "memory_read_ecc_in_" + std::to_string(registerIndex) + "_" + std::to_string(port);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, s, 2, false, jsonPorts, false,
                                          WritePortCirctBehavior::Add_NoPipeline);
                }
            }

            // Ports for external module calls (fixed-latency, connected to top-level ports or extern module methods)
            const std::set<size_t> externalModuleCalls = GetCrossBBExternalClassInstanceCalls(_program, basicBlock);

            for (const size_t callIndex : externalModuleCalls)
            {
                const ExternalModuleCall& externalModuleCall = _program._externalModuleCalls[callIndex];

                const auto callback =
                    [&](const std::string& name, const circt::hw::ModulePort::Direction direction, const size_t width)
                {
                    WritePortAndAddToJson(direction, name, width, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);
                };

                WriteExternalModuleCallPorts(callback, externalModuleCall);
            }

            // ports for FIFOs that are written
            for (const size_t i : writtenFifos)
            {
                const RegisterDescription& fifoDesc = _program._registerTable[i];
                assert(RegisterType::Fifo == fifoDesc._type);

                const size_t portIndex = SafeLookup(_compileContext._writtenFifoMap, i);

                std::string dataPortName;

                switch (fifoDesc.Fifo()._type)
                {
                case FifoType::Default:
                case FifoType::ReorderBuffer:
                    if (fifoDesc._width > 0)
                    {
                        dataPortName = "fifo_data_out_" + std::to_string(portIndex);
                        WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, dataPortName, fifoDesc._width,
                                              false, jsonPorts, false, WritePortCirctBehavior::Add_NoPipeline);
                    }

                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output,
                                          "fifo_wren_" + std::to_string(portIndex), 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);
                    break;

                case FifoType::ContextSaverCaller:
                    if (fifoDesc._width > 0)
                    {
                        dataPortName = "context_saver_caller_data_" + std::to_string(portIndex);
                        WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, dataPortName, fifoDesc._width,
                                              false, jsonPorts, false, WritePortCirctBehavior::Add_NoPipeline);
                    }

                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output,
                                          "context_saver_caller_wren_" + std::to_string(portIndex), 1, false, jsonPorts,
                                          true, WritePortCirctBehavior::Add_NoPipeline);

                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output,
                                          "context_saver_caller_loop_count_" + std::to_string(portIndex),
                                          fifoDesc.Fifo()._contextSaverCaller._loopCounterWidth, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);

                    if (!fifoDesc.Fifo()._contextSaverCaller._ordered)
                    {
                        WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input,
                                              "context_saver_caller_invocation_index_" + std::to_string(portIndex),
                                              GetCodeGenConfig().GetInvocationIndexSize(), false, jsonPorts, false,
                                              WritePortCirctBehavior::Add_NoPipeline);
                    }
                    break;

                case FifoType::Passthrough:
                case FifoType::PassthroughRegistered:
                case FifoType::PassthroughUnregistered:
                    if (fifoDesc._width > 0)
                    {
                        dataPortName = "fifo_data_out_" + std::to_string(portIndex);
                        WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, dataPortName, fifoDesc._width,
                                              false, jsonPorts, false, WritePortCirctBehavior::Add_NoPipeline);
                    }

                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output,
                                          "fifo_wren_" + std::to_string(portIndex), 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);
                    break;

                // No external ports
                case FifoType::FixedDelay:
                    break;

                default:
                    assert(false);
                }

                const FifoCode& fifoCode = fifoDesc.Fifo()._code;

                // FifoSupportsEncoding is not used here
                // because some fifos (like context savers) have constant bit information
                // but do not use the encoding, because the context saver RTL
                // cannot interpret encoded data.
                portConstantBitsHelper->AddFifoCodeLiterals(dataPortName, fifoCode);
            }

            for (const size_t i : backpressureFifos)
            {
                const RegisterDescription& fifoDesc = _program._registerTable[i];
                assert(RegisterType::Fifo == fifoDesc._type);
                std::string s = "";

                const size_t portIndex = SafeLookup(_compileContext._writtenFifoMap, i);

                switch (fifoDesc.Fifo()._type)
                {
                case FifoType::Default:
                case FifoType::ReorderBuffer:
                case FifoType::ContextSaverCaller:
                    s = "fifo_almost_full_in_raw_" + std::to_string(portIndex);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, s, 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);

                    s = "fifo_overflow_in_" + std::to_string(portIndex);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, s, 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);
                    AddBasicBlockDebugSignal(s);
                    break;

                default:
                    assert(false);
                }
            }

            if (basicBlock.IsResetBlock())
            {
                const std::string portName = "allow_initial_call_in";
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, portName, 1, false, jsonPorts, false,
                                      WritePortCirctBehavior::Add_NoPipeline);
            }

            for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
            {
                const size_t inputRegisterIndex = basicBlock._inputFifoIndices[i];
                const size_t inputRegisterIndexNorm = _fifoNamer.GetNormFifoIndex(inputRegisterIndex);

                const RegisterDescription inputRegDesc = _program._registerTable[inputRegisterIndex];
                assert(inputRegDesc._type == RegisterType::Fifo);

                const size_t inputWidth = inputRegDesc._width;

                if (inputWidth > 0)
                {
                    const std::string s = "data_in_" + std::to_string(inputRegisterIndexNorm);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, s, inputWidth, false, jsonPorts,
                                          false, WritePortCirctBehavior::Add_Pipeline);
                }

                const std::string underflowName = std::string("input_fifo_underflow_") + std::to_string(i);

                AddBasicBlockDebugSignal(underflowName);

                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, underflowName, 1, false, jsonPorts, true,
                                      WritePortCirctBehavior::Add_NoPipeline);

                if (inputRegDesc.Fifo()._type == FifoType::Passthrough)
                {
                    // The input has rdy/valid semantics (data is pushed into basic block)
                    const std::string s = "input_rdy_" + std::to_string(i);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, s, 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);

                    const std::string r = "input_valid_" + std::to_string(i);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, r, 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);
                }
                else if (inputRegDesc.Fifo()._type == FifoType::PassthroughRegistered)
                {
                    const std::string s = "input_valid_" + std::to_string(i);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, s, 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);
                }
                else if (inputRegDesc.Fifo()._type == FifoType::PassthroughUnregistered)
                {
                    const std::string s = "input_valid_" + std::to_string(i);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, s, 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);
                }
                else
                {
                    // The input has empty/rden semantics (basic block pulls data)
                    const std::string s = "input_fifo_rden_" + std::to_string(i);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, s, 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);

                    const std::string r = "input_fifo_empty_" + std::to_string(i);
                    WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, r, 1, false, jsonPorts, true,
                                          WritePortCirctBehavior::Add_NoPipeline);
                }
            }

            // Stall threshold for the basic blocks only when --stall is enabled
            if (GetCodeGenConfig()._stall > 0)
            {
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, "threshold_sel_in", 3, false, jsonPorts,
                                      false, WritePortCirctBehavior::Add_NoPipeline);
                WritePortAndAddToJson(circt::hw::ModulePort::Direction::Input, "threshold_sel_valid_in", 1, false,
                                      jsonPorts, false, WritePortCirctBehavior::Add_NoPipeline);
            }

            AddBasicBlockDebugSignal("control_state_out");
            WritePortAndAddToJson(circt::hw::ModulePort::Direction::Output, "control_state_out",
                                  c_basicBlockControlWidth, true, jsonPorts, true,
                                  WritePortCirctBehavior::Add_NoPipeline);

            jsonBasicBlock.AddMember("ports", jsonPorts);
        }

        {
            // Initialize CIRCT objects for the basic block
            OpenCirctBasicBlock();

            bool preStartCondition = basicBlock.HasStartCondition();

            // Per-stage operations
            for (auto it = basicBlock._stages.begin(); it != basicBlock._stages.end(); ++it)
            {
                const Stage& stage = *it;

                auto nextIt = it;
                ++nextIt;

                const Stage* const nextStage = nextIt == basicBlock._stages.end() ? nullptr : &(*nextIt);

                CompileOperationsCirct(basicBlock, stage, nextStage, preStartCondition);
            }

            for (const size_t releasedSemaphoreIndex : releasedSemaphores)
            {
                RestoreContextStage restoreContextStage(_compileContext);

                // Decrement the semaphore count when the last stage runs
                assert(!_compileContext._basicBlock->_stages.empty());

                _compileContext.SetCurrentStage(&(_compileContext._basicBlock->_stages.back()));

                _compileContext.OpBuilder().create<circt::sv::AssignOp>(
                    LocationToCirctLocation(basicBlock._location),
                    _compileContext.PortNameToValue("decr_semaphore_thread_count_" +
                                                    std::to_string(releasedSemaphoreIndex) + "_out"),
                    _compileContext.GetCurrentStageEnableSignal());
            }

            // Finialize CIRCT objects for the basic block
            CloseCirctBasicBlock();

            assert(_compileContext._batchAssignments.Empty());
        }

        jsonBasicBlock.AddMember("debug_signals", jsonBasicBlockDebugSignals);
        jsonBasicBlock.AddMember("debug_instances", jsonBasicBlockDebugInstances);
        jsonBasicBlock.AddMember("constant_signals", jsonBasicBlockConstantSignals);
        jsonBasicBlock.AddMember("unused_signals", jsonBasicBlockUnusedSignals);

        jsonBasicBlocks.PushBack(jsonBasicBlock);

        _compileContext = CompileContext{};
    }

    std::set<size_t> GetReadGlobalsWithNextIncGlobalViewInputs(const std::set<size_t>& readGlobalsWithNext,
                                                               const std::set<size_t>& readGlobalViews)
    {
        std::set<size_t> readGlobalsWithNextIncGlobalViewInputs = readGlobalsWithNext;

        for (const size_t i : readGlobalViews)
        {
            const RegisterDescription& regDesc = _program._registerTable[i];

            const OperationList& operations =
                SafeLookup(_program._globalViewFunctions, regDesc.GlobalView()._globalViewFunctionIndex);

            const std::set<size_t> globalRegisterInputs = GetGlobalRegisterSources(_program, operations);

            for (const size_t r : globalRegisterInputs)
            {
                readGlobalsWithNextIncGlobalViewInputs.insert(r);
            }
        }

        return readGlobalsWithNextIncGlobalViewInputs;
    }

    // If the underlying device requires control registers to have initial values
    // then this returns "'0".  Otherwise, this returns an empty string.
    const char* GetPowerOnString() const
    {
        const bool requirePowerOnReset = GetCodeGenDeviceConfig()._requirePowerOnReset;

        return requirePowerOnReset ? " = '0" : "";
    }

    void DeclarePropagationFifos(const BasicBlock& basicBlock)
    {
        const std::set<size_t> propagationFifos = GetPropagationFifos(_program, basicBlock);

        const PropagationFifoSlices propagationFifoSlices = GetPropagationFifoSlices(_program, basicBlock);

        const mlir::Location mlirBbLocation = LocationToCirctLocation(basicBlock._location);

        for (const size_t regIndex : propagationFifos)
        {
            const RegisterDescription regDesc = _program._registerTable[regIndex];
            const size_t regIndexNorm = _fifoNamer.GetNormFifoIndex(regIndex);
            assert(regDesc.Fifo()._type == FifoType::FixedDelay);

            const size_t width = regDesc._width;
            const size_t delay = regDesc.Fifo()._depth;

            ModuleInstanceHelper instance(*this, mlirBbLocation);

            instance.SetModuleName("KanagawaFixedDelayFifo");
            instance.SetInstanceName(_fifoNamer.GetFifoName(regIndex));

            instance.AddU64Parameter("WIDTH", width);
            instance.AddU64Parameter("DELAY", delay);
            instance.AddBoolParameter("USE_LUTRAM", ShouldFifoUseLutRam(_program, regIndex));
            instance.AddStringParameter("DEVICE_FAMILY", GetCodeGenDeviceConfig()._halDeviceFamily);
            instance.AddBoolParameter("USE_DSP", regDesc.Fifo()._useDsp);

            instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
            instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), "rst");

            {
                // Declare wires for FIFO inputs which the pipeilne will write to
                // Names are added to disable the wire read/writes from being canconicalized away
                circt::OpBuilder opb = circt::OpBuilder::atBlockBegin(_compileContext._hwModule.getBodyBlock());

                const std::map<size_t, size_t>& offsetToWidth = SafeLookup(propagationFifoSlices, regIndex);

                // Used to combine values from all wires together
                SparseConcat sparseConcat(opb, mlirBbLocation, width);

                std::map<size_t, mlir::Value>& offsetToInputWire = _compileContext._dataPropIn[regIndex];
                assert(offsetToInputWire.empty());

                for (const auto& p : offsetToWidth)
                {
                    const size_t sliceOffset = p.first;
                    const size_t sliceWidth = p.second;

                    const mlir::Type sliceType = GetIntegerType(sliceWidth);

                    const std::string inputWireName =
                        std::string("data_prop_in_") + std::to_string(regIndexNorm) + "_" + std::to_string(sliceOffset);

                    mlir::Value inputSliceWire = opb.create<circt::sv::LogicOp>(
                        mlirBbLocation, sliceType, StringToStringAttr(inputWireName),
                        GetFullyQualifiedInnerSymAttr(basicBlock.GetObjectPath(), inputWireName));

                    SafeInsert(offsetToInputWire, sliceOffset, inputSliceWire);

                    sparseConcat.Insert(sliceOffset,
                                        opb.create<circt::sv::ReadInOutOp>(mlirBbLocation, sliceType, inputSliceWire));
                }

                // Concatenate all slices into the input value
                const mlir::Value combinedInputs = sparseConcat.Flush();

                instance.AddPort("data_in", circt::hw::ModulePort::Direction::Input,
                                 instance.GetParameterizedIntegerType("WIDTH"), combinedInputs);
                instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output,
                                 instance.GetParameterizedIntegerType("WIDTH"));

                instance.Generate(&opb);

                // Slice the output into words
                // This is important because mlir CSE will combine multiple sv::ReadInOutOp in different pipeline stages
                // which read from the same wire
                std::map<size_t, mlir::Value>& offsetToOutputWire = _compileContext._dataPropOut[regIndex];
                assert(offsetToOutputWire.empty());

                for (const auto& p : offsetToWidth)
                {
                    const size_t sliceOffset = p.first;
                    const size_t sliceWidth = p.second;

                    const mlir::Value slice = opb.create<circt::comb::ExtractOp>(
                        mlirBbLocation, instance.GetPortValue("data_out"), sliceOffset, sliceWidth);

                    const mlir::Type sliceType = GetIntegerType(sliceWidth);

                    const std::string outputWireName = std::string("data_prop_out_") + std::to_string(regIndexNorm) +
                                                       "_" + std::to_string(sliceOffset);

                    mlir::Value outputSliceWire = opb.create<circt::sv::WireOp>(
                        mlirBbLocation, sliceType, StringToStringAttr(outputWireName),
                        GetFullyQualifiedInnerSymAttr(basicBlock.GetObjectPath(), outputWireName));

                    SafeInsert(offsetToOutputWire, sliceOffset, outputSliceWire);

                    opb.create<circt::sv::AssignOp>(mlirBbLocation, outputSliceWire, slice);
                }
            }
        }
    }

    bool GlobalHasResetPort(const RegisterDescription& regDesc)
    {
        assert(RegisterType::Global == regDesc._type);

        return regDesc.Global()._hasInitialValue;
    }

    size_t GetFiberIndexWidth(const BasicBlock& basicBlock) const
    {
        const size_t limit = basicBlock.GetThreadRate();

        assert(limit > 1);

        return Log2RoundUp(limit);
    }

    // generate the delayed signal for inverted almost_full/overflow
    mlir::Value GenerateDelayedInvFifoStatus(circt::OpBuilder& opb, const std::string& portName,
                                             const std::string& instName, const size_t fifoIndex)
    {
        const BasicBlock& basicBlock = *_compileContext._basicBlock;
        const mlir::Location bbLocation = LocationToCirctLocation(basicBlock._location);
        const std::string portNameIndex = portName + "_" + std::to_string(fifoIndex);
        const std::string instNameIndex = instName + "_" + std::to_string(fifoIndex);

        const mlir::Value invFifoStatus =
            circt::comb::createOrFoldNot(bbLocation, _compileContext.PortNameToValue(portNameIndex), opb, TwoState);

        mlir::Value invFifoStatusSelected;

        if (GetCodeGenConfig()._additionalLatency != 0)
        {
            ModuleInstanceHelper instance(*this, GetUnknownLocation());
            instance.SetModuleName("KanagawaFlipFlopChainNoEnable");
            instance.AddU64Parameter("WIDTH", 1);
            instance.AddU64Parameter("DEPTH", GetCodeGenConfig()._additionalLatency);
            instance.SetInstanceName(instNameIndex);
            instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
            instance.AddPort("data_in", circt::hw::ModulePort::Direction::Input, invFifoStatus);
            instance.AddPort("data_out", circt::hw::ModulePort::Direction::Output, GetI1Type());
            instance.Generate(&opb);

            const mlir::Value invFifoStatusDelayed = instance.GetPortValue("data_out");

            invFifoStatusSelected = invFifoStatusDelayed;
        }
        else
        {
            invFifoStatusSelected = invFifoStatus;
        }

        return invFifoStatusSelected;
    }

    struct StartNewThreadResult
    {
        mlir::Value _startNewThread;
        mlir::Value _startNewThreadPreStartCondition;
        mlir::Value _selectedFifoIndex;
        mlir::Value _startConditionWire;
        mlir::Value _fiberIndexRegMatch;
    };

    StartNewThreadResult CompileStartNewThreadCirct(circt::OpBuilder& opb)
    {
        const BasicBlock& basicBlock = *_compileContext._basicBlock;

        const mlir::Location bbLocation = LocationToCirctLocation(basicBlock._location);

        // Used to place all assignments in a single always_comb op
        BatchAssignments batchAssignments;

        StartNewThreadResult result;

        result._startConditionWire = opb.create<circt::sv::WireOp>(bbLocation, opb.getI1Type(), "start_condition");

        mlir::Value startConditionValue =
            opb.create<circt::sv::ReadInOutOp>(bbLocation, opb.getI1Type(), result._startConditionWire);

        // True if at least 1 input fifo is not empty
        // Only used when (basicBlock._inputFifoCount > 1)
        mlir::Value anyInputFifoNotEmpty;
        mlir::Value anyInputFifoNotEmptyPreStartCondition;

        // True if this is the first basic block of a [[reset]] function
        const bool isResetBlock = basicBlock.IsResetBlock();

        // Set on cycles where a thread executes the last stage
        mlir::Value lastStageValid;

        // This variable determines which input fifo will be read from
        // Initialized to 1-bit zero, can be overwritten later
        result._selectedFifoIndex =
            opb.create<circt::hw::ConstantOp>(bbLocation, opb.getIntegerAttr(opb.getI1Type(), 0));

        assert((basicBlock._inputFifoCount == 0) || IsPow2(basicBlock._inputFifoCount));

        const size_t selectedInputFifoWidth = (basicBlock._inputFifoCount > 1) ? Log2(basicBlock._inputFifoCount) : 0;

        std::list<size_t> backpressureLoopAsserts;

        // mapping of predicate expressions to bit indices into the control state that the CPU can inspect
        std::map<ControlStateBit, std::vector<mlir::Value>> controlStateRecords;

        // If there are 2 input fifos, and the second one is FifoType::PassthroughRegistered
        // then inputs coming from that link cannot be stopped
        // Output control signals are only checked when dequeuing from the other input fifo
        // This is important for transitive backpressure, as a downstream fifo may report it is not almost full
        // when a thread first enters a loop, but that same fifo may report almost_full when the thread loops around
        // In this case, no overflow will occur
        const bool checkControlFifo0Only =
            (basicBlock._inputFifoCount == 2) &&
            (FifoType::PassthroughRegistered == _program._registerTable[basicBlock._inputFifoIndices[1]].Fifo()._type);

        {
            // the set of conditions that all must be true to enable a new thread to start executing
            std::vector<mlir::Value> allowThreadPredicates;

            // Output fifos must not be almost full
            const std::set<size_t> backpressureFifos = GetBackpressureFifos(_program, basicBlock);

            for (const size_t fifoIndex : backpressureFifos)
            {
                const RegisterDescription& regDesc = _program._registerTable[fifoIndex];
                assert(RegisterType::Fifo == regDesc._type);

                switch (regDesc.Fifo()._type)
                {
                case FifoType::Default:
                case FifoType::ReorderBuffer:
                case FifoType::ContextSaverCaller:
                {
                    const size_t fifoPortIndex = SafeLookup(_compileContext._writtenFifoMap, fifoIndex);

                    const mlir::Value notAlmostFullSelected = GenerateDelayedInvFifoStatus(
                        opb, "fifo_almost_full_in_raw", "fifo_almostfull_ffc", fifoPortIndex);
                    allowThreadPredicates.push_back(notAlmostFullSelected);
                    controlStateRecords[ControlStateBit::OutputFifoFull].push_back(notAlmostFullSelected);

                    const mlir::Value NoOverflowSelected =
                        GenerateDelayedInvFifoStatus(opb, "fifo_overflow_in", "fifo_overflow_ffc", fifoPortIndex);
                    controlStateRecords[ControlStateBit::OutputFifoOverflow].push_back(NoOverflowSelected);
                }
                break;

                // Passthrough fifos can never backpressure
                case FifoType::PassthroughRegistered:
                    break;

                default:
                    assert(false);
                }
            }

            const size_t fifoPredicates = allowThreadPredicates.size();

            // The thread count must not exceed the maximum
            const std::set<size_t> acquiredSemaphores = GetAcquiredSemaphores(basicBlock);

            for (const size_t semaphoreIndex : acquiredSemaphores)
            {
                std::ostringstream str;

                mlir::Value notSemaphoreFull = circt::comb::createOrFoldNot(
                    bbLocation,
                    _compileContext.PortNameToValue("semaphore_full_" + std::to_string(semaphoreIndex) + "_in"), opb,
                    TwoState);

                allowThreadPredicates.push_back(notSemaphoreFull);

                controlStateRecords[ControlStateBit::SemaphoreCountExceeded].push_back(notSemaphoreFull);
            }

            const size_t adjustedThreadRate = basicBlock.GetThreadRate();
            mlir::Value fiberIndexRegMatch;
            if (adjustedThreadRate != 1)
            {
                // Backedge is needed because cycle counter output feeds back into itself
                circt::BackedgeBuilder beb(opb, bbLocation);
                circt::Backedge backedge = beb.get(opb.getIntegerType(GetFiberIndexWidth(basicBlock)));

                fiberIndexRegMatch = opb.create<circt::comb::ICmpOp>(
                    bbLocation, circt::comb::ICmpPredicate::eq, backedge,
                    opb.create<circt::hw::ConstantOp>(
                        bbLocation,
                        opb.getIntegerAttr(opb.getIntegerType(GetFiberIndexWidth(basicBlock)), adjustedThreadRate - 1)),
                    TwoState);

                const mlir::Value fiberIndexRegInc = opb.create<circt::comb::AddOp>(
                    bbLocation, backedge,
                    opb.create<circt::hw::ConstantOp>(
                        bbLocation, opb.getIntegerAttr(opb.getIntegerType(GetFiberIndexWidth(basicBlock)), 1)),
                    TwoState);

                const mlir::Value fiberIndexRegMux = opb.create<circt::comb::MuxOp>(
                    bbLocation, fiberIndexRegMatch,
                    opb.create<circt::hw::ConstantOp>(
                        bbLocation, opb.getIntegerAttr(opb.getIntegerType(GetFiberIndexWidth(basicBlock)), 0)),
                    fiberIndexRegInc, TwoState);

                circt::seq::CompRegOp fiberIndexReg = opb.create<circt::seq::CompRegOp>(
                    bbLocation, fiberIndexRegMux, _compileContext.PortNameToValue("clk"),
                    _compileContext.PortNameToValue("rst"),
                    opb.create<circt::hw::ConstantOp>(
                        bbLocation, opb.getIntegerAttr(opb.getIntegerType(GetFiberIndexWidth(basicBlock)), 0)),
                    GetFullyQualifiedStringAttr(basicBlock.GetObjectPath(), "fiber_index_0_ff"));

                result._fiberIndexRegMatch = static_cast<mlir::Value>(fiberIndexRegMatch);

                circt::seq::CompRegOp fiberIndexEqZeroReg = opb.create<circt::seq::CompRegOp>(
                    bbLocation, fiberIndexRegMatch, _compileContext.PortNameToValue("clk"),
                    _compileContext.PortNameToValue("rst"),
                    opb.create<circt::hw::ConstantOp>(bbLocation, opb.getIntegerAttr(opb.getI1Type(), 1)),
                    GetFullyQualifiedStringAttr(basicBlock.GetObjectPath(), "fiber_index_eq_0_ff"));
                backedge.setValue(fiberIndexReg);

                if (GetCodeGenConfig()._releaseAssert)
                {
                    VerbatimWriter writer(opb, bbLocation);

                    writer << "`ifndef NO_DYNAMIC_ASSERTS\n";
                    writer << "//synopsys translate_off\n";
                    writer << "assert property (@(posedge clk) (!rst && !$isunknown(";
                    writer << static_cast<mlir::Value>(fiberIndexReg);
                    writer << ")) |-> (" << static_cast<mlir::Value>(fiberIndexEqZeroReg) << " == (";
                    writer << static_cast<mlir::Value>(fiberIndexReg) << " == '0)));\n";
                    writer << "//synopsys translate_on\n";
                    writer << "`endif\n";
                }

                allowThreadPredicates.push_back(fiberIndexEqZeroReg);
            }

            const bool backpressureAllowed = !basicBlock._function->_functionNode->NoBackpressure();

            if (backpressureAllowed && _basicBlockHasIntraPipelineStalls)
            {
                const size_t threshold = _stallRateThresholdGenerator.Next();
                const mlir::Value thresholdValue =
                    opb.create<circt::hw::ConstantOp>(bbLocation, opb.getIntegerAttr(opb.getI64Type(), threshold));

                const mlir::Value oneValue = opb.create<circt::hw::ConstantOp>(
                    bbLocation, opb.getIntegerAttr(opb.getIntegerType(GetFiberIndexWidth(basicBlock)), 1));
                const mlir::Value stallEnableForThreadRate =
                    (basicBlock.GetThreadRate() != 1) ? fiberIndexRegMatch : oneValue;

                ModuleInstanceHelper instance(*this, GetUnknownLocation());
                instance.SetModuleName("KanagawaLfsr");
                instance.AddU64Parameter("WIDTH", c_stallLfsrBits);
                instance.SetInstanceName("lfsr_stall_inst");
                instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetI1Type(), "clk");
                instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), "rst");
                instance.AddPort("en", circt::hw::ModulePort::Direction::Input, GetI1Type());
                instance.AddPort("lfsr_out", circt::hw::ModulePort::Direction::Output,
                                 GetIntegerType(c_stallLfsrBits));
                instance.Generate();

                const mlir::Value stallFromLfsr =
                    opb.create<circt::comb::ICmpOp>(bbLocation, circt::comb::ICmpPredicate::ule,
                                                    instance.GetPortValue("lfsr_out"), thresholdValue, TwoState);

                batchAssignments.Append(bbLocation, instance.GetPortValue("en"), stallEnableForThreadRate);

                allowThreadPredicates.push_back(stallFromLfsr);
            }

            if (allowThreadPredicates.size() + (basicBlock.HasStartCondition() ? 1 : 0) > fifoPredicates)
            {
                // There are predicates other than fifo almost_full

                // accepting input from fifo 1 would not honor this condition
                assert(!checkControlFifo0Only);

                // Predecessor block cannot detect this condition ahead of time (when it decides to accept a new thread)
                assert(!basicBlock._transitiveBackpressurePredecessor);
            }

            mlir::Value allowNewThreadPreStartCondition;
            mlir::Value allowNewThread;

            {
                mlir::SmallVector<mlir::Value> allowThreadTerms;

                // Don't trust fifo control signals during the reset sequence
                allowThreadTerms.push_back(
                    circt::comb::createOrFoldNot(bbLocation, _compileContext.PortNameToValue("rst"), opb, TwoState));

                if (!checkControlFifo0Only)
                {
                    // Check predicates before allowing a new thread to enter the basic block
                    for (const mlir::Value& predicate : allowThreadPredicates)
                    {
                        allowThreadTerms.push_back(predicate);
                    }
                }

                allowNewThreadPreStartCondition =
                    opb.create<circt::comb::AndOp>(bbLocation, allowThreadTerms, TwoState);
            }

            if (basicBlock.HasStartCondition())
            {
                // The user-specified start condition must be met
                // The IR to generate that start condition has not been emitted yet
                // It will write to result._startConditionWire
                allowNewThread = opb.create<circt::comb::AndOp>(bbLocation, allowNewThreadPreStartCondition,
                                                                startConditionValue, TwoState);

                controlStateRecords[ControlStateBit::WaitForConditionNotMet].push_back(startConditionValue);
            }
            else
            {
                allowNewThread = allowNewThreadPreStartCondition;
            }

            // If the no_backpressure attribute is present on the function, then there should be no reason block
            // incoming requests
            assert(backpressureAllowed || allowThreadPredicates.empty());

            if (basicBlock._inputFifoCount > 1)
            {
                assert(!isResetBlock);

                // Examine all input fifos looking for a non-empty one
                anyInputFifoNotEmpty =
                    opb.create<circt::hw::ConstantOp>(bbLocation, opb.getIntegerAttr(opb.getI1Type(), 0));
                anyInputFifoNotEmptyPreStartCondition =
                    opb.create<circt::hw::ConstantOp>(bbLocation, opb.getIntegerAttr(opb.getI1Type(), 0));

                result._selectedFifoIndex = opb.create<circt::hw::ConstantOp>(
                    bbLocation, opb.getIntegerAttr(opb.getIntegerType(selectedInputFifoWidth), 0));

                for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
                {
                    const size_t inputFifoIndex = basicBlock._inputFifoIndices[i];
                    const RegisterDescription& inputFifoDesc = _program._registerTable[inputFifoIndex];
                    assert(inputFifoDesc._type == RegisterType::Fifo);

                    mlir::SmallVector<mlir::Value> writeConditions;

                    if (inputFifoDesc.Fifo()._type == FifoType::PassthroughRegistered)
                    {
                        // PassthroughRegistered fifos have no backpressure
                        // And input comes in as a valid signal
                        // This can occur with loop backward links if the compiler
                        // can be sure that no backpressure is needed on the backward link
                        // This input must be chosen, because there is no way to backpressure mechanism
                        assert(checkControlFifo0Only && (i == 1));

                        // This assert ensures that this fifo will be chosen with higher priority
                        assert(i == (basicBlock._inputFifoCount - 1));

                        writeConditions.push_back(_compileContext.PortNameToValue("input_valid_" + std::to_string(i)));

                        if (GetCodeGenConfig()._releaseAssert)
                        {
                            backpressureLoopAsserts.push_back(i);
                        }
                    }
                    else
                    {
                        std::ostringstream str;

                        writeConditions.push_back(circt::comb::createOrFoldNot(
                            bbLocation, _compileContext.PortNameToValue("input_fifo_empty_" + std::to_string(i)), opb,
                            TwoState));

                        if ((i == 0) && checkControlFifo0Only)
                        {
                            // Check predicates before dequeuing from fifo 0
                            assert(2 == basicBlock._inputFifoCount);

                            for (const mlir::Value& predicate : allowThreadPredicates)
                            {
                                writeConditions.push_back(predicate);
                            }
                        }
                    }

                    assert(!writeConditions.empty());

                    const mlir::Value writeCondition =
                        opb.create<circt::comb::AndOp>(bbLocation, writeConditions, TwoState);

                    anyInputFifoNotEmpty = opb.create<circt::comb::MuxOp>(bbLocation, writeCondition, allowNewThread,
                                                                          anyInputFifoNotEmpty, TwoState);

                    anyInputFifoNotEmptyPreStartCondition =
                        opb.create<circt::comb::MuxOp>(bbLocation, writeCondition, allowNewThreadPreStartCondition,
                                                       anyInputFifoNotEmptyPreStartCondition, TwoState);

                    result._selectedFifoIndex = opb.create<circt::comb::MuxOp>(
                        bbLocation, writeCondition,
                        opb.create<circt::hw::ConstantOp>(
                            bbLocation, opb.getIntegerAttr(opb.getIntegerType(selectedInputFifoWidth), i)),
                        result._selectedFifoIndex, TwoState);
                }

                result._startNewThread = anyInputFifoNotEmpty;

                result._startNewThreadPreStartCondition = anyInputFifoNotEmptyPreStartCondition;

                for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
                {
                    const size_t inputFifoIndex = basicBlock._inputFifoIndices[i];
                    const RegisterDescription& inputFifoDesc = _program._registerTable[inputFifoIndex];
                    assert(inputFifoDesc._type == RegisterType::Fifo);

                    // PassthroughRegistered have no read enable, data must be accepted when available
                    if (inputFifoDesc.Fifo()._type != FifoType::PassthroughRegistered)
                    {
                        const mlir::Value fifoIndexMatch = opb.create<circt::comb::ICmpOp>(
                            bbLocation, circt::comb::ICmpPredicate::eq, result._selectedFifoIndex,
                            opb.create<circt::hw::ConstantOp>(
                                bbLocation, opb.getIntegerAttr(opb.getIntegerType(selectedInputFifoWidth), i)),
                            TwoState);

                        const mlir::Value readInputFifo = opb.create<circt::comb::AndOp>(
                            bbLocation, result._startNewThread, fifoIndexMatch, TwoState);

                        batchAssignments.Append(bbLocation,
                                                _compileContext.PortNameToValue("input_fifo_rden_" + std::to_string(i)),
                                                readInputFifo);
                    }
                }

                controlStateRecords[ControlStateBit::InputFifoEmpty].push_back(anyInputFifoNotEmpty);
            }
            else
            {
                if (isResetBlock)
                {
                    // Global writes before start condition are not supported.
                    // The only case where a reset block has a start condition
                    // is when it is introduced by compiler-inserted stalls so
                    // global writes are not expected.
                    if (basicBlock.HasStartCondition())
                    {
                        bool startCondition = false;

                        for (const Stage& stage : basicBlock._stages)
                        {
                            if (stage._atomicSequence > 0)
                            {
                                break;
                            }

                            for (const Operation& op : stage._operations)
                            {
                                assert(op._opcode != Opcode::WriteGlobal);
                                if (op._opcode == Opcode::StartCondition)
                                {
                                    startCondition = true;
                                    break;
                                }
                            }

                            if (startCondition)
                            {
                                break;
                            }
                        }
                        assert(startCondition);
                    }

                    // Used to represent initialCallMadeReg before it is available
                    circt::BackedgeBuilder beb(opb, bbLocation);
                    circt::Backedge initialCallMadeRegBackedge = beb.get(opb.getI1Type());

                    // Determine the initial call should occur on the current cycle:
                    // !initial_call_made_ff && allow_new_thread && allow_initial_call_in
                    //
                    // allow_initial_call_in is set after the reset and memory initialization sequences have finished
                    mlir::SmallVector<mlir::Value> initialCallMadeThisCycleInputs;

                    initialCallMadeThisCycleInputs.push_back(
                        circt::comb::createOrFoldNot(bbLocation, initialCallMadeRegBackedge, opb, TwoState));
                    initialCallMadeThisCycleInputs.push_back(allowNewThread);
                    initialCallMadeThisCycleInputs.push_back(_compileContext.PortNameToValue("allow_initial_call_in"));

                    const mlir::Value initialCallMadeThisCycle = opb.create<circt::comb::AndOp>(
                        bbLocation, opb.getI1Type(), initialCallMadeThisCycleInputs, TwoState);

                    // initial_call_made_ff || initial_call_this_cycle
                    const mlir::Value initialCallMadeRegInput = opb.create<circt::comb::OrOp>(
                        bbLocation, initialCallMadeRegBackedge, initialCallMadeThisCycle, TwoState);

                    circt::seq::CompRegOp initialCallMadeReg = opb.create<circt::seq::CompRegOp>(
                        bbLocation, initialCallMadeRegInput, _compileContext.PortNameToValue("clk"),
                        _compileContext.PortNameToValue("rst"),
                        opb.create<circt::hw::ConstantOp>(bbLocation,
                                                          opb.getIntegerAttr(opb.getI1Type(), 0)), // reset value
                        GetFullyQualifiedStringAttr(basicBlock.GetObjectPath(), "initial_call_made_ff"));

                    initialCallMadeRegBackedge.setValue(initialCallMadeReg);

                    // Threads that enter the basic block are either:
                    // 1) automatically created threads right after reset OR
                    // 2) regular function calls
                    if (basicBlock._inputFifoCount > 0)
                    {
                        const mlir::Value inputFifoNotEmpty = circt::comb::createOrFoldNot(
                            bbLocation, _compileContext.PortNameToValue("input_fifo_empty_0"), opb, TwoState);

                        const mlir::Value fifoNotEmptyAllowNewThread =
                            opb.create<circt::comb::AndOp>(bbLocation, inputFifoNotEmpty, allowNewThread, TwoState);

                        const mlir::Value stageValid0 = opb.create<circt::comb::OrOp>(
                            bbLocation, fifoNotEmptyAllowNewThread, initialCallMadeThisCycle, TwoState);

                        const mlir::Value initCallNotThisCycle =
                            circt::comb::createOrFoldNot(bbLocation, initialCallMadeThisCycle, opb, TwoState);

                        const mlir::Value inputFifoRden0 =
                            opb.create<circt::comb::AndOp>(bbLocation, stageValid0, initCallNotThisCycle, TwoState);

                        batchAssignments.Append(bbLocation, _compileContext.PortNameToValue("input_fifo_rden_0"),
                                                inputFifoRden0);

                        result._startNewThread = stageValid0;

                        controlStateRecords[ControlStateBit::InputFifoEmpty].push_back(inputFifoNotEmpty);
                    }
                    else
                    {
                        // there are no call sites to explictly call the reset function
                        result._startNewThread = initialCallMadeThisCycle;
                    }
                }
                else
                {
                    assert(basicBlock._inputFifoCount > 0);

                    const size_t inputFifoIndex = basicBlock._inputFifoIndices[0];

                    const RegisterDescription& inputFifoDesc = _program._registerTable[inputFifoIndex];
                    assert(inputFifoDesc._type == RegisterType::Fifo);

                    if (inputFifoDesc.Fifo()._type == FifoType::Passthrough)
                    {
                        const mlir::Value inputValid0 = _compileContext.PortNameToValue("input_valid_0");

                        batchAssignments.Append(bbLocation, _compileContext.PortNameToValue("input_rdy_0"),
                                                allowNewThread);

                        result._startNewThread =
                            opb.create<circt::comb::AndOp>(bbLocation, allowNewThread, inputValid0, TwoState);
                        result._startNewThreadPreStartCondition = opb.create<circt::comb::AndOp>(
                            bbLocation, allowNewThreadPreStartCondition, inputValid0, TwoState);

                        controlStateRecords[ControlStateBit::InputFifoEmpty].push_back(inputValid0);
                    }
                    else if ((inputFifoDesc.Fifo()._type == FifoType::PassthroughRegistered) ||
                             (inputFifoDesc.Fifo()._type == FifoType::PassthroughUnregistered))
                    {
                        const mlir::Value inputValid0 = _compileContext.PortNameToValue("input_valid_0");

                        result._startNewThread =
                            opb.create<circt::comb::AndOp>(bbLocation, allowNewThread, inputValid0, TwoState);
                        result._startNewThreadPreStartCondition = opb.create<circt::comb::AndOp>(
                            bbLocation, allowNewThreadPreStartCondition, inputValid0, TwoState);

                        controlStateRecords[ControlStateBit::InputFifoEmpty].push_back(inputValid0);
                    }
                    else
                    {
                        const mlir::Value inputFifoNotEmpty = circt::comb::createOrFoldNot(
                            bbLocation, _compileContext.PortNameToValue("input_fifo_empty_0"), opb, TwoState);

                        result._startNewThread =
                            opb.create<circt::comb::AndOp>(bbLocation, allowNewThread, inputFifoNotEmpty, TwoState);
                        result._startNewThreadPreStartCondition = opb.create<circt::comb::AndOp>(
                            bbLocation, allowNewThreadPreStartCondition, inputFifoNotEmpty, TwoState);

                        batchAssignments.Append(bbLocation, _compileContext.PortNameToValue("input_fifo_rden_0"),
                                                result._startNewThread);

                        controlStateRecords[ControlStateBit::InputFifoEmpty].push_back(inputFifoNotEmpty);
                    }
                }
            }

            for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
            {
                // Inverted because a value of "0" in controlStateRecords means that everything is OK
                const mlir::Value noUnderflow = circt::comb::createOrFoldNot(
                    bbLocation, _compileContext.PortNameToValue("input_fifo_underflow_" + std::to_string(i)), opb,
                    TwoState);

                controlStateRecords[ControlStateBit::InputFifoUnderflow].push_back(noUnderflow);
            }

            // Increment the total thread count in the function - this happens after the fifo read latency
            for (const size_t semaphoreIndex : acquiredSemaphores)
            {
                batchAssignments.Append(bbLocation,
                                        _compileContext.PortNameToValue("incr_semaphore_thread_count_" +
                                                                        std::to_string(semaphoreIndex) + "_out"),
                                        result._startNewThread);
            }

            // Write output control state. Code coverage is disabled for these debug-only nets
            {
                llvm::SmallVector<mlir::Value> controlStateBits;

                for (size_t i = 0; i < c_basicBlockControlWidth; i++)
                {
                    controlStateBits.push_back(
                        opb.create<circt::hw::ConstantOp>(bbLocation, opb.getIntegerAttr(opb.getI1Type(), 0)));
                }

                for (const auto& p : controlStateRecords)
                {
                    const size_t bitIndex = static_cast<size_t>(p.first);

                    const std::vector<mlir::Value>& expressions = p.second;

                    const mlir::Value reduced = opb.create<circt::comb::AndOp>(bbLocation, expressions, TwoState);

                    // Flip bits so that 0 means everything is OK, 1 means something is causing a slow down
                    controlStateBits[bitIndex] = circt::comb::createOrFoldNot(bbLocation, reduced, opb, TwoState);
                }

                // Reverse control state bits because concat op expects most significant bit first
                std::reverse(controlStateBits.begin(), controlStateBits.end());

                const mlir::Value combined = opb.create<circt::comb::ConcatOp>(
                    bbLocation, opb.getIntegerType(c_basicBlockControlWidth), controlStateBits);

                batchAssignments.Append(bbLocation, _compileContext.PortNameToValue("control_state_out"), combined);
            }

            if (backpressureLoopAsserts.size() != 0)
            {
                VerbatimWriter writer(opb, bbLocation);

                writer << "`ifndef NO_DYNAMIC_ASSERTS\n";
                writer << "//synopsys translate_off\n";
                // Generate an assertion that will fire if input_valid_i is high
                // but that input is not accepted
                for (const size_t i : backpressureLoopAsserts)
                {
                    writer << "assert property (@(posedge clk) (!rst && ";
                    writer << static_cast<mlir::Value>(
                                  _compileContext.PortNameToValue("input_valid_" + std::to_string(i)))
                           << ") |-> (";
                    const mlir::Value selectedFifoIndexMatch = opb.create<circt::comb::ICmpOp>(
                        bbLocation, circt::comb::ICmpPredicate::eq, result._selectedFifoIndex,
                        opb.create<circt::hw::ConstantOp>(
                            bbLocation, opb.getIntegerAttr(opb.getIntegerType(selectedInputFifoWidth), i)),
                        TwoState);

                    const mlir::Value notEmpyAndFifoIndexMatch = opb.create<circt::comb::AndOp>(
                        bbLocation, anyInputFifoNotEmpty, selectedFifoIndexMatch, TwoState);
                    writer << static_cast<mlir::Value>(notEmpyAndFifoIndexMatch) << "));\n";
                }
                writer << "//synopsys translate_on\n";
                writer << "`endif\n";
            }
        }

        batchAssignments.Flush(opb, bbLocation);

        return result;
    }

    std::string GetStageValidName(const size_t atomicSequence) const
    {
        std::ostringstream str;

        if (0 == atomicSequence)
        {
            str << "stage_valid_0"; // no _ff
        }
        else
        {
            str << "stage_valid_" << atomicSequence << "_ff";
        }

        return str.str();
    }

    bool IsClockGatingEnabledForBasicBlock(const BasicBlock& basicBlock) const
    {
        return GetCodeGenConfig().PipelineClockGatingEnabled();
    }

    std::string GetStallPredicateForPipelineStage(const BasicBlock& basicBlock, const size_t atomicSequence) const
    {
        std::ostringstream str;

        if (_basicBlockHasStalls)
        {
            const auto& stallable = SafeLookup(basicBlock._pipelineStageStallableMap, atomicSequence);

            if (basicBlock._isStallable && stallable._isStallable)
            {
                str << "(stall";

                if (stallable._nearestPreceedingNonStallable)
                {
                    // There is an upstream non-stallable stage. We include that stages valid bit in
                    // the stall predicate (i.e. if that stage is valid, we can't stall)
                    str << " & ~" << GetStageValidName(stallable._nearestPreceedingNonStallable.get());
                }

                str << ")";
            }
        }

        return str.str();
    }

    std::string GetFiberIndexPreviousStage(const BasicBlock& basicBlock, const Stage& stage)
    {
        std::ostringstream str;

        if (stage._atomicSequence == basicBlock._stages.front()._atomicSequence)
        {
            throw RuntimeErrorWithTrace("Cannot locate previous stage");
        }
        else
        {
            str << "fiber_index_" << stage._atomicSequence << "_next";
        }

        return str.str();
    }

    std::string GetFiberIndex(const Stage& stage)
    {
        return std::string("fiber_index_") + std::to_string(stage._atomicSequence) + "_ff";
    }

    std::string GetIsFiber(const BasicBlock& basicBlock, const Stage& stage, const size_t fiberIndex)
    {
        const size_t fiberIndexWidth = GetFiberIndexWidth(basicBlock);

        std::ostringstream str;
        str << "(" << GetFiberIndex(stage) << " == " << fiberIndexWidth << "'d" << fiberIndex << ")";

        return str.str();
    }

    std::string GetInputValid(const BasicBlock& basicBlock, const Stage& stageIn,
                              const bool preStartCondition = false) const
    {
        // Find the first stage in the basic block with a matching atomic sequence number
        for (const Stage& stage : basicBlock._stages)
        {
            if (stage._atomicSequence == stageIn._atomicSequence)
            {
                std::ostringstream str;

                str << "(";

                if (preStartCondition && (stage._atomicSequence == 0))
                {
                    str << "stage_valid_0_pre_start_condition";
                }
                else
                {
                    str << GetStageValidName(stage._atomicSequence);
                }

                // We skip adding stall predicate for stage 0, because this is handled separately in the logic
                // for allow_new_thread. If we also add it here, it is functionally valid, but it generates
                // an extra expression that will show up as a miss in code coverage.
                if (0 != stage._atomicSequence)
                {
                    const std::string stallPredicate =
                        GetStallPredicateForPipelineStage(basicBlock, stage._atomicSequence);

                    if (!stallPredicate.empty())
                    {
                        str << " & ~" << stallPredicate;
                    }
                }

                str << ")";

                return str.str();
            }
        }

        assert(false);
        return std::string("Cannot locate stage");
    }

    std::string GetInputValidPreviousStage(const BasicBlock& basicBlock, const Stage& stage)
    {
        // Find the first stage in the basic block with a matching atomic sequence number
        std::ostringstream str;

        str << "(";

        if (stage._atomicSequence == basicBlock._stages.front()._atomicSequence)
        {
            throw RuntimeErrorWithTrace("Cannot locate previous stage");
        }
        else
        {
            str << "stage_valid_" << stage._atomicSequence << "_next";
        }

        const std::string stallPredicate = GetStallPredicateForPipelineStage(basicBlock, stage._atomicSequence - 1);

        if (!stallPredicate.empty())
        {
            str << " & ~" << stallPredicate;
        }

        str << ")";

        return str.str();
    }

    std::string GetInputValidReplica(const BasicBlock& basicBlock, const Stage& stageIn)
    {
        // Find the first stage in the basic block with a matching atomic sequence number
        for (const Stage& stage : basicBlock._stages)
        {
            if (stage._atomicSequence == stageIn._atomicSequence)
            {
                std::ostringstream str;

                str << "(";

                if (stage._atomicSequence == basicBlock._stages.front()._atomicSequence)
                {
                    return GetInputValid(basicBlock, stageIn);
                }
                else
                {
                    str << "stage_valid_" << stage._atomicSequence << "_replica_" << _inputValidReplicaIndex << "_ff";

                    ++_inputValidReplicaIndex;
                }

                const std::string stallPredicate = GetStallPredicateForPipelineStage(basicBlock, stage._atomicSequence);

                if (!stallPredicate.empty())
                {
                    str << " & ~" << stallPredicate;
                }

                str << ")";

                return str.str();
            }
        }

        assert(false);
        return std::string("Cannot locate stage");
    }

    const BasicBlock& GetCurrentBasicBlock() const
    {
        assert(_compileContext._basicBlock);
        return *_compileContext._basicBlock;
    }

    const Stage& GetCurrentStage() const
    {
        assert(_compileContext._stage);
        return *_compileContext._stage;
    }

    void CompileOperationsCirct(const BasicBlock& basicBlock, const Stage& stage, const Stage* const nextStage,
                                bool& preStartCondition)
    {
        assert(!_compileContext._stage);
        assert(_compileContext._batchAssignments.Empty());
        assert(!_compileContext._triggeredOpHelper);

        _compileContext.SetCurrentStage(&stage);

        bool hasPreStartConditionTriggeredOps = false;
        bool hasTriggeredOps = false;

        // asserts/prints are emitted separately
        // in a shared always block to ensure
        // correct ordering between them
        // Operations that access the string table
        // must run in clock-triggered block (once per clock cycle)
        const auto useTriggeredOp = [](const Operation& op)
        {
            switch (op._opcode)
            {
            case Opcode::Assert:
            case Opcode::Print:
            case Opcode::ReferenceString:
            case Opcode::FormatString:
            case Opcode::FormatEnum:
            case Opcode::AssertStringEqual:
                return true;
                break;
            default:
                return false;
                break;
            }
        };

        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::StartCondition)
            {
                assert(preStartCondition);

                preStartCondition = false;
            }

            if (!useTriggeredOp(op))
            {
                CompileOperationCirct(op, preStartCondition);
            }
            else
            {
                if (preStartCondition)
                {
                    hasPreStartConditionTriggeredOps = true;
                }
                else
                {
                    hasTriggeredOps = true;
                }
            }
        }

        const auto compileTriggeredOps = [&](const bool preStartCondition, TriggeredOpHelper& triggeredOpHelper)
        {
            bool valid;
            if (preStartCondition)
            {
                valid = true;
            }
            else
            {
                // Skip over ops before start condition
                valid = !hasPreStartConditionTriggeredOps;
            }

            for (const Operation& op : stage._operations)
            {
                if (op._opcode == Opcode::StartCondition)
                {
                    if (preStartCondition)
                    {
                        break;
                    }
                    else
                    {
                        valid = true;
                    }
                }

                if (valid && useTriggeredOp(op))
                {
                    triggeredOpHelper.AddOp(op);
                }
            }

            triggeredOpHelper.DoneAddingOps(circt::hw::EventControl::AtPosEdge, _compileContext._clock);

            DisableDynamicAssertsAndTranslateOffCirct ifdefBlock(_compileContext.OpBuilder(), GetUnknownLocation());

            _compileContext._triggeredOpHelper = &triggeredOpHelper;

            for (const Operation& op : stage._operations)
            {
                if (useTriggeredOp(op))
                {
                    CompileOperationCirct(op, preStartCondition);
                }
            }

            _compileContext._triggeredOpHelper = nullptr;
        };

        if (hasPreStartConditionTriggeredOps)
        {
            assert(_compileContext._stage->_atomicSequence == 0);
            assert(_compileContext._allowNewThreadPreStartCondition);
            TriggeredOpHelper triggeredOpHelper(_compileContext.OpBuilder(), _compileContext._circtPipeline,
                                                GetSourceOperandToMlirValue(), _program,
                                                _compileContext._allowNewThreadPreStartCondition);

            compileTriggeredOps(true, triggeredOpHelper);
        }
        if (hasTriggeredOps)
        {
            TriggeredOpHelper triggeredOpHelper(_compileContext.OpBuilder(), _compileContext._circtPipeline,
                                                GetSourceOperandToMlirValue(), _program,
                                                _compileContext.GetCurrentStageEnableSignal());
            compileTriggeredOps(false, triggeredOpHelper);
        }

        // Write out always_comb block with assignments accumulated in the current stage
        _compileContext._batchAssignments.Flush(_compileContext.OpBuilder(),
                                                LocationToCirctLocation(basicBlock._location));

        _compileContext.SetCurrentStage(nullptr);
    }

    std::string FileAndLineNumberToString(size_t fileIndex, size_t lineNumber)
    {
        return g_compiler->GetSourceFileNameWithoutLeadingPath(fileIndex) + ":" + std::to_string(lineNumber);
    }

    std::string StackFrameToString(const StackFrame& stackFrame)
    {
        return stackFrame._unmangledFunctionName + " in " +
               FileAndLineNumberToString(stackFrame._fileIndex, stackFrame._lineNumber);
    }

    // Generate CIRCT IR for the operations defining a global view
    mlir::Value CompileGlobalViewCirct(const size_t globalViewRegisterIndex)
    {
        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const RegisterDescription& regDesc = _program._registerTable[globalViewRegisterIndex];

        const OperationList& operations =
            SafeLookup(_program._globalViewFunctions, regDesc.GlobalView()._globalViewFunctionIndex);

        // Save _compileContext._regToValue, clear it
        // and restore it at function exit
        // _compileContext._regToValue will be used to track the values of variables defined and used inside of the
        // global view
        std::map<size_t, mlir::Value> prevRegToValue;

        std::swap(prevRegToValue, _compileContext._regToValue);

        ExitScope exitScope([&]() { _compileContext._regToValue = prevRegToValue; });

        _compileContext._regToValue.clear();

        // Use SourceOpToStringMode::NextValue when reading from globals
        // to ensure the value of a global view reflects updates to input global variables
        // which occured on the same cycle.
        SourceOperandToMlirValueCb sourceOperandToMlirValue =
            [this](const Operation& op, const size_t srcOperandIndex, const size_t desiredWidth)
        {
            const SourceOperand& srcOp = op._src.at(srcOperandIndex);

            SourceOpToStringMode srcOpToStrMode = SourceOpToStringMode::Default;

            if (SourceOperandType::Register == srcOp.Type())
            {
                const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                if (RegisterType::Global == _program._registerTable[registerIndex]._type)
                {
                    srcOpToStrMode = SourceOpToStringMode::NextValue;
                }
            }

            return SourceOperandToMlirValue(op, srcOperandIndex, desiredWidth, srcOpToStrMode);
        };

        for (const Operation& op : operations)
        {
            ConvertOpToCirct(op, opb, _program, sourceOperandToMlirValue, GetStoreMlirValueInDestOperand());
        }

        // The final operation writes the result
        assert(!operations.empty());
        const Operation& resultOp = operations.back();
        assert(resultOp._dst.size() == 1);

        return SafeLookup(_compileContext._regToValue, resultOp._dst[0].GetAccessedRegister()._registerIndex);
    }

    // Returns lambda of type SourceOperandToMlirValueCb
    SourceOperandToMlirValueCb GetSourceOperandToMlirValue()
    {
        return [this](const Operation& op, const size_t srcOperandIndex, const size_t desiredWidth)
        { return SourceOperandToMlirValue(op, srcOperandIndex, desiredWidth); };
    }

    // Returns lambda of type StoreMlirValueInDestOperandCb
    StoreMlirValueInDestOperandCb GetStoreMlirValueInDestOperand()
    {
        return [this](const Operation& op, const size_t dstOperandIndex, const mlir::Value& value)
        { StoreValueIntoDestOperand(op, dstOperandIndex, value); };
    }

    // Callback used by ConvertOpToCirct
    mlir::Value SourceOperandToMlirValue(const Operation& op, const size_t srcOperandIndex, const size_t desiredWidth,
                                         const SourceOpToStringMode srcOpToStrMode = SourceOpToStringMode::Default)
    {
        const SourceOperand& srcOp = op._src.at(srcOperandIndex);

        mlir::Value result;

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location location = OperationToCirctLocation(op, _program);

        switch (srcOp.Type())
        {
        case SourceOperandType::Literal:
            result = LiteralToValue(srcOp.GetLiteral(), opb, location);
            break;

        case SourceOperandType::Register:
        {
            const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

            switch (_program._registerTable[registerIndex]._type)
            {
            case RegisterType::Pipeline:
            case RegisterType::Wire:
            case RegisterType::Local: // used when defining a global view
                result = SafeLookup(_compileContext._regToValue, registerIndex);
                break;

            case RegisterType::Global:
                result = (srcOpToStrMode == SourceOpToStringMode::NextValue)
                             ? _compileContext.PortNameToValue(GetGlobalInNextName(registerIndex))
                             : _compileContext.PortNameToValue(GetGlobalInName(registerIndex));
                break;

            case RegisterType::GlobalView:
                result = CompileGlobalViewCirct(registerIndex);
                break;

            default:
                assert(false);
            }
        }
        break;

        case SourceOperandType::Fifo:
        {
            const FifoSubset& fs = srcOp.GetFifoSubset();

            const std::string portName = "data_in_" + std::to_string(_fifoNamer.GetNormFifoIndex(fs._registerIndex));

            const mlir::Value inputValue = _compileContext.PortNameToValue(portName);

            result = opb.create<circt::comb::ExtractOp>(location, inputValue, fs._offset, fs._width);
        }
        break;

        default:
            assert(false);
        }

        const bool signExtend = op.ShouldSignExtend(srcOperandIndex);

        return AdjustValueWidth(result, desiredWidth, signExtend, opb, location);
    }

    // Callback used by ConvertOpToCirct
    void StoreValueIntoDestOperand(const Operation& op, const size_t dstOperandIndex, const mlir::Value& valueIn)
    {
        const DestinationOperand& dstOp = op._dst.at(dstOperandIndex);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location location = OperationToCirctLocation(op, _program);

        // Truncation is fine, but widening is not supported because signedness is not known
        const size_t dstOpWidth = dstOp.Width(_program);

        const size_t valueWidth = GetMlirValueWidth(valueIn);

        assert(dstOpWidth <= valueWidth);

        const mlir::Value value = AdjustValueWidth(valueIn, dstOpWidth, false, opb, location);

        switch (dstOp.Type())
        {
        case DestinationOperandType::Register:
        {
            const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

            // Set the namehint attribute on the input value
            // This affects names of pipeline registers, and wires (when the ExportVerilog code decides to spill a value
            // to a wire) This will also be recursively applied to all unnamed operations which are sources of value
            AttachNameHintToValue(value, dstOp.GetAccessedRegister(), _program);

            switch (_program._registerTable[registerIndex]._type)
            {
            case RegisterType::Pipeline:
            case RegisterType::Wire:
                SafeInsert(_compileContext._regToValue, registerIndex, value);
                break;

            // used when defining a global view
            case RegisterType::Local:
                _compileContext._regToValue[registerIndex] = value;
                break;

            case RegisterType::BitBucket:
                // Drop the write
                break;

            default:
                assert(false);
            }
        }
        break;

        case DestinationOperandType::Fifo:
        {
            const FifoSubset& fs = dstOp.GetFifoSubset();

            _compileContext._fifoWrites.Accumulate(fs._registerIndex, value, fs._offset, fs._width);
        }
        break;

        default:
            assert(false);
        }
    }

    void CompileDebugViewCIRCT(const Operation& op)
    {
        assert(Opcode::DebugView == op._opcode);
        assert(0 == op._dst.size());

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        ModuleInstanceHelper instance(*this, opLocation);

        instance.SetModuleName(GetDebugViewModuleName(op));

        instance.SetInstanceName(GetDebugViewModuleName(op) + "_instance");

        instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");

        for (size_t i = 0; i < op._flags._debugView->_arguments.size(); i++)
        {
            const DebugViewArgument& debugViewArgument = op._flags._debugView->_arguments[i];

            // Concat all components of this argument
            mlir::SmallVector<mlir::Value> values;

            for (size_t j = 0; j < debugViewArgument._srcOperandWidths.size(); j++)
            {
                // Most significant argument goes first
                const size_t invJ = debugViewArgument._srcOperandWidths.size() - j - 1;

                const size_t srcOpIndex = debugViewArgument._beginSrcOperand + invJ;

                const size_t argumentWidth = debugViewArgument._srcOperandWidths[invJ];

                values.push_back(SourceOperandToMlirValue(op, srcOpIndex, argumentWidth));
            }

            mlir::Value expressionValue = opb.create<circt::comb::ConcatOp>(opLocation, values);

            // Replace with 'x on cycles when the stage enable bit is low
            mlir::SmallVector<mlir::Value> substitutions;

            substitutions.push_back(_compileContext.GetCurrentStageEnableSignal());
            substitutions.push_back(expressionValue);

            expressionValue = opb.create<circt::sv::VerbatimExprOp>(opLocation, expressionValue.getType(),
                                                                    "{{0}} ? {{1}} : 'x", substitutions);

            instance.AddPort(std::string("_") + debugViewArgument._name, circt::hw::ModulePort::Direction::Input,
                             expressionValue);
        }

        instance.AddPort("valid", circt::hw::ModulePort::Direction::Input,
                         _compileContext.GetCurrentStageEnableSignal());

        instance.AddPort("valid_out", circt::hw::ModulePort::Direction::Output, GetIntegerType(1));

        instance.Generate();
    }

    void CompileEnqueueCirct(const Operation& op)
    {
        assert(Opcode::Enqueue == op._opcode);

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const Enqueue& enqueue = op._flags._enqueue;

        const RegisterDescription& regDesc = _program._registerTable[enqueue._successorFifo];
        assert(RegisterType::Fifo == regDesc._type);

        std::string dataPortName;

        const size_t portIndex = SafeLookup(_compileContext._writtenFifoMap, enqueue._successorFifo);

        // Assign to the wren output port
        switch (regDesc.Fifo()._type)
        {
        case FifoType::Default:
        case FifoType::ReorderBuffer:
        {
            assert(0 == op._dst.size());

            const std::string wrenPortName = "fifo_wren_" + std::to_string(portIndex);

            dataPortName = "fifo_data_out_" + std::to_string(portIndex);

            // Write-enable bit
            const mlir::Value portSsaValue = _compileContext.PortNameToValue(wrenPortName);

            const mlir::Value doEnqueueBit =
                GetPredicatedStageEnable(op, 0, enqueue._isPredicated, !enqueue._predicateExecutionValue);

            _compileContext._batchAssignments.Append(opLocation, portSsaValue, doEnqueueBit);
        }
        break;

        case FifoType::ContextSaverCaller:
        {
            // Fifo encoding is not supported with context savers, because
            // context savers can interpret some of the data
            assert(!FifoSupportsEncoding(regDesc));
            assert(1 == op._src.size()); // loop counter width

            dataPortName = "context_saver_caller_data_" + std::to_string(portIndex);

            const std::string wrenPortName = "context_saver_caller_wren_" + std::to_string(portIndex);

            // Wren is connected to the stage valid bit only
            // IR contains operations to set the thread count to 0 on predication
            _compileContext._batchAssignments.Append(opLocation, _compileContext.PortNameToValue(wrenPortName),
                                                     _compileContext.GetCurrentStageEnableSignal());

            // Loop count output port
            // IR contains operations to set the thread count to 0 on predication
            const std::string loopCountName = "context_saver_caller_loop_count_" + std::to_string(portIndex);

            _compileContext._batchAssignments.Append(
                opLocation, _compileContext.PortNameToValue(loopCountName),
                SourceOperandToMlirValue(op, 0, regDesc.Fifo()._contextSaverCaller._loopCounterWidth));

            // For unordered context savers
            if (!regDesc.Fifo()._contextSaverCaller._ordered)
            {
                const std::string callerInvocationIndexName =
                    "context_saver_caller_invocation_index_" + std::to_string(portIndex);
                StoreValueIntoDestOperand(op, 0, _compileContext.PortNameToValue(callerInvocationIndexName));
            }
        };
        break;

        case FifoType::Passthrough:
        case FifoType::PassthroughRegistered:
        case FifoType::PassthroughUnregistered:
            dataPortName = "fifo_data_out_" + std::to_string(portIndex);

            _compileContext._batchAssignments.Append(
                opLocation, _compileContext.PortNameToValue("fifo_wren_" + std::to_string(portIndex)),
                GetPredicatedStageEnable(op, 0, enqueue._isPredicated, !enqueue._predicateExecutionValue));
            break;

        default:
            assert(false);
        }

        // Combine all data writes and assign them to the data output port
        if (!dataPortName.empty() && (regDesc._width > 0))
        {
            _compileContext._fifoWrites.Flush(opb, opLocation, enqueue._successorFifo,
                                              _compileContext.PortNameToValue(dataPortName),
                                              _compileContext._batchAssignments);
        }
    }

    void CompileEnqueueRegistersCirct(const Operation& op)
    {
        assert(Opcode::EnqueueRegisters == op._opcode);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        const size_t fifoIndex = op._flags._queueRegisters._fifoIndex;

        const RegisterDescription& regDesc = _program._registerTable[fifoIndex];
        assert(RegisterType::Fifo == regDesc._type);
        assert(FifoType::FixedDelay == regDesc.Fifo()._type);

        const std::map<size_t, mlir::Value>& offsetToWire = SafeLookup(_compileContext._dataPropIn, fifoIndex);

        for (size_t i = 0; i < op._src.size(); i++)
        {
            const SourceOperand& srcOp = op._src[i];

            const size_t offset = op._flags._queueRegisters._offsets->at(i);

            const size_t width = srcOp.Width(_program);

            const mlir::Value value = SourceOperandToMlirValue(op, i, width);

            const mlir::Value& wire = SafeLookup(offsetToWire, offset);

            assert(width == GetMlirTypeWidth(mlir::cast<circt::hw::InOutType>(wire.getType()).getElementType()));

            _compileContext._batchAssignments.Append(opLocation, wire, value);
        }
    }

    void CompileDequeueRegistersCirct(const Operation& op)
    {
        assert(Opcode::DequeueRegisters == op._opcode);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        const size_t fifoIndex = op._flags._queueRegisters._fifoIndex;

        const RegisterDescription& regDesc = _program._registerTable[fifoIndex];

        assert(RegisterType::Fifo == regDesc._type);
        assert(FifoType::FixedDelay == regDesc.Fifo()._type);

        // Read fixed-delay fifo output
        const std::map<size_t, mlir::Value>& offsetToWire = SafeLookup(_compileContext._dataPropOut, fifoIndex);

        // Extract subsets of the fifo values
        for (size_t i = 0; i < op._dst.size(); i++)
        {
            const DestinationOperand& dstOp = op._dst[i];

            const size_t offset = op._flags._queueRegisters._offsets->at(i);

            const size_t dstOpWidth = dstOp.Width(_program);

            const mlir::Value& srcWire = SafeLookup(offsetToWire, offset);

            const size_t wireWidth =
                GetMlirTypeWidth(mlir::cast<circt::hw::InOutType>(srcWire.getType()).getElementType());

            // It is possible for not all bits of the result to be used
            // The width of the wire is passed to ReadInOutOp
            // While StoreValueIntoDestOperand handles truncation to dstOpWidth
            assert(dstOpWidth <= wireWidth);

            const mlir::Value slice =
                opb.create<circt::sv::ReadInOutOp>(opLocation, GetIntegerType(wireWidth), srcWire);

            StoreValueIntoDestOperand(op, i, slice);
        }
    }

    void CompileAssertCirct(const Operation& op, const bool preStartCondition)
    {
        assert(Opcode::Assert == op._opcode);
        assert(op._src.size() > 1);
        assert(op._src.size() <= 3);
        assert(0 == op._dst.size());
        assert(_compileContext._triggeredOpHelper);

        TriggeredOpHelper& triggeredOpHelper = *_compileContext._triggeredOpHelper;

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        {
            VerbatimWriter writer(opb, opLocation);

            // Fire assertion when all of the following:
            //   - rst === 0 (not 1 or x)
            //   - stage valid bit === 1 (not 0 or x)
            //   - src[0] != src[1] or src[0] !== src[1] depending on caseEQ flag
            // If src[2] is present, then the following must also be true for an assertion to fire:
            //   - src[2] === 1 (not 1 or x), this is the predicate if one existed

            writer << "assert(!((rst === 1'b0) & (";
            writer << triggeredOpHelper.GetEnableSignal();
            writer << " === 1'b1)";

            if (3 == op._src.size())
            {
                // Include the predicate
                writer << " & (" << triggeredOpHelper.GetSourceOperand(op, 2, 1) << " === 1'b1)";
            }

            // Fire assertion if src[0] and src[1] do not match
            const std::string cmpString = op._flags._assertion._caseEQ ? " !== " : " != ";

            const size_t lhsWidth = op._src[0].Width(_program);
            const size_t rhsWidth = op._src[1].Width(_program);
            const size_t maxSrcWidth = std::max(lhsWidth, rhsWidth);

            const mlir::Value lhs = triggeredOpHelper.GetSourceOperand(op, 0, maxSrcWidth);
            const mlir::Value rhs = triggeredOpHelper.GetSourceOperand(op, 1, maxSrcWidth);

            // lhs and rhs are wrapped in parenthesis because they can be transformed into aribtrary expressions
            // which might have bad interactions with the precedence of !=
            writer << " & ((" << lhs << ")" << cmpString << "(" << rhs << "))";

            writer << ")) else $fatal(1, \"" << EscapeSpecialChars(op._flags._assertion._message) << "\");";
        }
    }

    void CompilePrintCirct(const Operation& op)
    {
        assert(Opcode::Print == op._opcode);
        assert(0 == op._dst.size());
        assert(_compileContext._triggeredOpHelper);

        TriggeredOpHelper& triggeredOpHelper = *_compileContext._triggeredOpHelper;

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        {
            VerbatimWriter writer(opb, opLocation);

            // source operand 0 is the predicate
            writer << "if ((" << triggeredOpHelper.GetEnableSignal() << " & "
                   << triggeredOpHelper.GetSourceOperand(op, 0) << ") === 1'b1) $write(\"";

            bool first = true;

            for (const PrintEntry& printEntry : *(op._flags._print._entries))
            {
                if (!first)
                {
                    writer << (" ");
                }

                first = false;

                switch (printEntry._type)
                {
                case PrintType::Uint:
                case PrintType::Int:
                    writer << "0x%h";
                    break;

                case PrintType::StringLiteral:
                case PrintType::StringHandle:
                    writer << "%s";
                    break;

                case PrintType::Float:
                    writer << "%f";
                    break;

                default:
                    assert(false);
                }
            }

            writer << "\"";

            for (const PrintEntry& printEntry : *(op._flags._print._entries))
            {
                switch (printEntry._type)
                {
                case PrintType::Uint:
                case PrintType::Int:
                    writer << ", " << triggeredOpHelper.GetSourceOperand(op, printEntry._operandIndex);
                    break;

                case PrintType::StringLiteral:
                    writer << ", \"" << EscapeSpecialChars(op._src[printEntry._operandIndex].GetStringLiteral())
                           << "\"";
                    break;

                case PrintType::StringHandle:
                    writer << ", string_table.get("
                           << triggeredOpHelper.GetSourceOperand(op, printEntry._operandIndex, c_stringHandleWidth)
                           << ", 1'b1)";
                    break;

                case PrintType::Float:
                    writer << ", $bitstoshortreal("
                           << triggeredOpHelper.GetSourceOperand(op, printEntry._operandIndex, 64) << ")";
                    break;

                default:
                    assert(false);
                }
            }

            writer << ");";
        }
    }

    void CompileFormatStringCirct(const Operation& op)
    {
        assert(Opcode::FormatString == op._opcode);
        assert(0 == op._dst.size());
        assert(_compileContext._triggeredOpHelper);

        TriggeredOpHelper& triggeredOpHelper = *_compileContext._triggeredOpHelper;

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        VerbatimWriter writer(opb, opLocation);

        const mlir::Value condition =
            opb.create<circt::comb::AndOp>(GetUnknownLocation(), triggeredOpHelper.GetEnableSignal(),
                                           triggeredOpHelper.GetSourceOperand(op, 1, 1), TwoState);

        writer << "if (" << condition << ") ";

        writer << "string_table.allocate(";

        const auto emitString = [&](const std::string& literal) { writer << literal; };

        const auto emitSrcOp = [&](size_t operandIdx)
        { writer << triggeredOpHelper.GetSourceOperand(op, operandIdx, op._src.at(operandIdx).Width(_program)); };

        GetSFormat(op, emitString, emitSrcOp);

        writer << ", " << triggeredOpHelper.GetSourceOperand(op, 0, c_stringHandleWidth) << ");";
    }

    void CompileFormatEnumCirct(const Operation& op)
    {
        assert(Opcode::FormatEnum == op._opcode);
        assert(0 == op._dst.size());
        assert(3 == op._src.size());
        assert(_compileContext._triggeredOpHelper);

        TriggeredOpHelper& triggeredOpHelper = *_compileContext._triggeredOpHelper;

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        VerbatimWriter writer(opb, opLocation);

        const mlir::Value condition =
            opb.create<circt::comb::AndOp>(GetUnknownLocation(), triggeredOpHelper.GetEnableSignal(),
                                           triggeredOpHelper.GetSourceOperand(op, 1, 1), TwoState);

        writer << "if (" << condition << ") begin ";

        writer << "string __enumString;";

        writer << "case (" << triggeredOpHelper.GetSourceOperand(op, 2, op._src[2].Width(_program)) << ")";

        for (const FormatEnumEntry& entry : *op._flags._formatEnum._entries)
        {
            writer << entry.second << " : __enumString = \"" << entry.first << "\";";
        }

        // - here removes leading white space
        writer << "default : __enumString = $sformatf(\"%-d %s\", "
               << triggeredOpHelper.GetSourceOperand(op, 2, op._src[2].Width(_program)) << ", \""
               << *op._flags._formatEnum._defaultString << "\");";

        writer << "endcase ";

        writer << "string_table.allocate(__enumString";

        writer << ", " << triggeredOpHelper.GetSourceOperand(op, 0, c_stringHandleWidth) << ");";

        writer << " end";
    }

    void CompileReferenceStringCirct(const Operation& op)
    {
        assert(Opcode::ReferenceString == op._opcode);
        assert(3 == op._src.size());
        assert(0 == op._dst.size());

        assert(_compileContext._triggeredOpHelper);

        TriggeredOpHelper& triggeredOpHelper = *_compileContext._triggeredOpHelper;

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        VerbatimWriter writer(opb, opLocation);

        const mlir::Value enableAndPredicate = opb.create<circt::comb::AndOp>(
            opLocation, triggeredOpHelper.GetEnableSignal(), triggeredOpHelper.GetSourceOperand(op, 0, 1), TwoState);

        writer << "string_table.reference_string(" << triggeredOpHelper.GetSourceOperand(op, 1, c_stringHandleWidth)
               << ", " << triggeredOpHelper.GetSourceOperand(op, 2, c_stringReferenceBits) << ", " << enableAndPredicate
               << ");";
    }

    void CompileAssertStringEqualCirct(const Operation& op)
    {
        assert(Opcode::AssertStringEqual == op._opcode);
        assert(3 == op._src.size());
        assert(0 == op._dst.size());

        assert(_compileContext._triggeredOpHelper);

        TriggeredOpHelper& triggeredOpHelper = *_compileContext._triggeredOpHelper;

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        VerbatimWriter writer(opb, opLocation);

        const mlir::Value enableAndPredicate = opb.create<circt::comb::AndOp>(
            opLocation, triggeredOpHelper.GetEnableSignal(), triggeredOpHelper.GetSourceOperand(op, 0, 1), TwoState);

        writer << "string_table.assert_strings_equal(" << triggeredOpHelper.GetSourceOperand(op, 1, c_stringHandleWidth)
               << ", " << triggeredOpHelper.GetSourceOperand(op, 2, c_stringHandleWidth) << ", " << enableAndPredicate
               << ", \"" << EscapeSpecialChars(op._flags._assertion._message) << "\");";
    }

    void CompileStringCountCirct(const Operation& op)
    {
        assert(Opcode::StringCount == op._opcode);
        assert(0 == op._src.size());
        assert(1 == op._dst.size());

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        // Counting the number of strings
        // in the string table is not supported, just return 0
        StoreValueIntoDestOperand(op, 0, GetTypedZeros(opb, opLocation, GetIntegerType(op._dst[0].Width(_program))));
    }

    void CompileWriteGlobalCirct(const Operation& op, const bool preStartCondition)
    {
        assert(Opcode::WriteGlobal == op._opcode);
        assert(1 == op._dst.size());

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const size_t dstReg = op._dst[0].GetAccessedRegister()._registerIndex;

        const RegisterDescription::GlobalDesc& globalDesc = _program._registerTable[dstReg].Global();

        const size_t writeIndex = op._dst[0].GetWriteIndex();

        // Write data
        // There is no write data port if the write data is known at compile time
        if (globalDesc._literalValues.end() == globalDesc._literalValues.find(writeIndex))
        {
            _compileContext._batchAssignments.Append(
                opLocation, _compileContext.PortNameToValue(GetGlobalOutName(dstReg, writeIndex)),
                SourceOperandToMlirValue(op, 0, op._dst[0].Width(_program)));
        }

        // Write enable
        _compileContext._batchAssignments.Append(
            opLocation, _compileContext.PortNameToValue(GetGlobalValidOutName(dstReg, writeIndex)),
            GetPredicatedStageEnable(op, 1, op._flags._writeGlobal._isPredicated, false, SourceOpToStringMode::Default,
                                     preStartCondition));
    }

    void CompileLoadMemoryCirct(const Operation& op)
    {
        assert(Opcode::LoadMemory == op._opcode);

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const size_t loadStage = _compileContext._stage->_atomicSequence;

        // This inserts operations at the stage before and after the load
        // Restore the current stage at the end of this function
        RestoreContextStage restoreContextStage(_compileContext);

        const size_t readPort = op._flags._loadMemory._readPort;

        const bool isPredicated = op._flags._loadMemory._isPredicated;

        assert(op._flags._loadMemory._readLatency <= 2);
        const bool useOutputRegister = op._flags._loadMemory._readLatency == 2;

        const bool useInputRegister = op._flags._loadMemory._readLatency > 0;

        const AccessedRegister& src0 = op._src[0].GetAccessedRegister();

        const RegisterDescription& memoryDesc = _program._registerTable[src0._registerIndex];
        assert(memoryDesc._type == RegisterType::Memory);

        const bool ecc = memoryDesc.Memory()._ecc;

        SourceOpToStringMode srcOpToStrMode = SourceOpToStringMode::Default;

        if (useInputRegister)
        {
            // Read enable and address are computed before the load occurs
            _compileContext.SetCurrentStage(_compileContext.GetLastStage(loadStage - 1));
            srcOpToStrMode = SourceOpToStringMode::NextValue;
        }

        // Set the index
        const std::string addrPortName =
            "memory_read_addr_out_" + std::to_string(src0._registerIndex) + "_" + std::to_string(readPort);

        _compileContext._batchAssignments.Append(
            opLocation, _compileContext.PortNameToValue(addrPortName),
            SourceOperandToMlirValue(op, 1, memoryDesc.GetMemoryAddressWidth(), srcOpToStrMode));

        // Set enable bit
        if (!memoryDesc.Memory()._useLogicRam)
        {
            const std::string rdenStr =
                "memory_rden_out_" + std::to_string(src0._registerIndex) + "_" + std::to_string(readPort);

            _compileContext._batchAssignments.Append(
                opLocation, _compileContext.PortNameToValue(rdenStr),
                GetPredicatedStageEnable(op, 2, isPredicated, false, srcOpToStrMode));
        }

        // If ECC is enabled, then the last 2 destinations hold ECC status bits
        assert(!ecc || (op._dst.size() >= 2));
        const size_t numDataElements = ecc ? op._dst.size() - 2 : op._dst.size();

        // Read the data

        if (useOutputRegister)
        {
            // Output is read on the stage after the LoadMemory operation
            _compileContext.SetCurrentStage(_compileContext.GetLastStage(loadStage + 1));
        }
        else
        {
            // Output is read on the same stage as the LoadMemory operation
            _compileContext.SetCurrentStage(_compileContext.GetLastStage(loadStage));
        }

        // Full word read from memory
        const mlir::Value readData = _compileContext.PortNameToValue(
            "memory_read_data_in_" + std::to_string(src0._registerIndex) + "_" + std::to_string(readPort));

        // Decompose into fields
        for (size_t i = 0; i < numDataElements; ++i)
        {
            const size_t width = op._dst[i].Width(_program);

            const size_t offset = op._flags._loadMemory._sourceOffsets->at(i);

            const size_t destRegisterIndex = op._dst[i].GetAccessedRegister()._registerIndex;

            // If a memory holds structure elements, and some elements are not read
            // then the destination will be a bitbucket
            // Do not emit a line for those members
            if (RegisterType::BitBucket != _program._registerTable[destRegisterIndex]._type)
            {
                circt::comb::ExtractOp extractOp =
                    opb.create<circt::comb::ExtractOp>(opLocation, readData, offset, width);

                StoreValueIntoDestOperand(op, i, extractOp);
            }
        }

        if (ecc)
        {
            const mlir::Value eccBits = _compileContext.PortNameToValue(
                "memory_read_ecc_in_" + std::to_string(src0._registerIndex) + "_" + std::to_string(readPort));

            // Connect ECC status bits
            for (size_t i = 0; i < 2; i++)
            {
                const size_t dstOpIndex = i + numDataElements;

                const DestinationOperand& dstOp = op._dst[dstOpIndex];

                // Handles the cases where ECC data is not read
                if (RegisterType::BitBucket !=
                    _program._registerTable[dstOp.GetAccessedRegister()._registerIndex]._type)
                {
                    circt::comb::ExtractOp extractOp = opb.create<circt::comb::ExtractOp>(opLocation, eccBits, i, 1);

                    StoreValueIntoDestOperand(op, dstOpIndex, extractOp);
                }
            }
        }
    }

    void CompileStoreMemoryCirct(const Operation& op)
    {
        assert(Opcode::StoreMemory == op._opcode);
        assert(1 == op._dst.size());

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        // Address, data, write-enable are computed in the previous pipeline stage
        // ensure that the current stage will be CompileContext
        RestoreContextStage restoreContextStage(_compileContext);

        _compileContext.SetCurrentStage(_compileContext.GetLastStage(_compileContext._stage->_atomicSequence - 1));

        const AccessedRegister& dst = op._dst[0].GetAccessedRegister();

        const RegisterDescription& dstDesc = _program._registerTable[dst._registerIndex];
        assert(dstDesc._type == RegisterType::Memory);

        const bool isPredicated = op._flags._storeMemory._isPredicated;

        const size_t writePort = op._flags._storeMemory._writePort;

        // Set the write-enable bit
        const std::string wrenStr = MemoryWriteEnableString(dst._registerIndex, writePort);

        // Write enable
        _compileContext._batchAssignments.Append(opLocation, _compileContext.PortNameToValue(wrenStr),
                                                 GetPredicatedStageEnable(op, 1, isPredicated));

        // Set the address
        const std::string addrPortName = MemoryWriteAddrString(dst._registerIndex, writePort);

        _compileContext._batchAssignments.Append(opLocation, _compileContext.PortNameToValue(addrPortName),
                                                 SourceOperandToMlirValue(op, 0, dstDesc.GetMemoryAddressWidth()));

        // Concatenate all write data
        mlir::SmallVector<mlir::Value> writeDataFields;

        const size_t firstSource = isPredicated ? 2 : 1;

        assert(op._src.size() >= firstSource);

        // Most significant to least significant
        for (size_t i = op._src.size() - 1; i >= firstSource; --i)
        {
            const size_t width = op._flags._storeMemory._sourceWidths->at(i - firstSource);

            writeDataFields.push_back(SourceOperandToMlirValue(op, i, width));
        }

        circt::comb::ConcatOp combinedWriteData = opb.create<circt::comb::ConcatOp>(opLocation, writeDataFields);

        _compileContext._batchAssignments.Append(
            opLocation, _compileContext.PortNameToValue(MemoryWriteDataString(dst._registerIndex, writePort)),
            combinedWriteData);
    }

    void CompileCycleCounterCirct(const Operation& op)
    {
        assert(Opcode::CycleCounter == op._opcode);
        assert(0 == op._src.size());
        assert(1 == op._dst.size());

        const BasicBlock& basicBlock = *_compileContext._basicBlock;

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        // Create a wire outside of the pipeline
        // and assign it to the cycle counter
        mlir::Value cycleCountWire;

        const auto it = _compileContext._cycleCountWireMap.find(_compileContext._stage->_atomicSequence);

        if (it == _compileContext._cycleCountWireMap.end())
        {
            circt::OpBuilder::InsertionGuard g(opb);

            opb.setInsertionPointToEnd(_compileContext._hwModule.getBodyBlock());

            // Pipeline stage index is added to symbol name
            // to ensure that CSE doesn't combine reads across different pipeline stages
            const std::string wireName =
                std::string("cycle_count_wire") + std::to_string(_compileContext._stage->_atomicSequence);

            cycleCountWire = opb.create<circt::sv::WireOp>(
                opLocation, _compileContext._cycleCountReg.getType(), StringToStringAttr(wireName),
                GetFullyQualifiedInnerSymAttr(basicBlock.GetObjectPath(), wireName));

            opb.create<circt::sv::AssignOp>(opLocation, cycleCountWire, _compileContext._cycleCountReg);

            SafeInsert(_compileContext._cycleCountWireMap, _compileContext._stage->_atomicSequence, cycleCountWire);
        }
        else
        {
            cycleCountWire = it->second;
        }

        // Read from that wire inside of the pipeline, at the current pipeline stage
        const mlir::Value cycleCount =
            opb.create<circt::sv::ReadInOutOp>(opLocation, _compileContext._cycleCountReg.getType(), cycleCountWire);

        StoreValueIntoDestOperand(op, 0, cycleCount);
    }

    void CompileReadSelectedFifoCirct(const Operation& op)
    {
        assert(Opcode::ReadSelectedFifo == op._opcode);
        assert(GetCurrentBasicBlock()._inputFifoCount == op._src.size());
        assert(1 == op._dst.size());

        const size_t numChoices = op._src.size();

        const size_t selectIndexWidth = Log2(numChoices);

        const size_t dstWidth = op._dst[0].Width(_program);

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Value selectIndex =
            AdjustValueWidth(_compileContext._selectedFifoIndex, selectIndexWidth, false, opb, opLocation);

        std::vector<mlir::Value> choices;

        for (size_t i = 0; i < op._src.size(); i++)
        {
            choices.push_back(SourceOperandToMlirValue(op, i, dstWidth));
        }

        const mlir::Value result = MuxTree(selectIndex, choices, opb, opLocation);

        StoreValueIntoDestOperand(op, 0, result);
    }

    void CompileStartConditionCirct(const Operation& op)
    {
        assert(Opcode::StartCondition == op._opcode);
        assert(1 == op._src.size());
        assert(0 == op._dst.size());

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Value condition = SourceOperandToMlirValue(op, 0, 1);

        opb.create<circt::sv::AssignOp>(opLocation, _compileContext._startConditionWire, condition);
    }

    void CompileStallCheckCirct(const Operation& op)
    {
        assert(GetCodeGenConfig()._stall > 0);
        assert(Opcode::StallCheck == op._opcode);
        assert(1 == op._src.size());

        const BasicBlock& basicBlock = *_compileContext._basicBlock;

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);
        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const std::vector<size_t> threshold = _stallRateThresholdGenerator2.Next();
        assert(threshold.size() == 5);
        std::vector<mlir::Value> thresholdValues;
        // make a 8-entry vector for 3-bit select signal
        mlir::Value iVal;
        for (size_t i = 0; i < 8; i++)
        {
            if (i < threshold.size())
            {
                iVal = opb.create<circt::hw::ConstantOp>(
                    opLocation, opb.getIntegerAttr(opb.getIntegerType(LfsrWidth), threshold[i]));
            }
            else
            {
                iVal =
                    opb.create<circt::hw::ConstantOp>(opLocation, opb.getIntegerAttr(opb.getIntegerType(LfsrWidth), 0));
            }
            thresholdValues.push_back(iVal);
        }

        const mlir::Value selectIndex =
            opb.create<circt::sv::ReadInOutOp>(opLocation, GetIntegerType(3), _compileContext._thresholdSelWire);
        const mlir::Value selectIndexAdjusted = AdjustValueWidth(selectIndex, 3, false, opb, opLocation);
        const mlir::Value thresholdFinal = MuxTree(selectIndexAdjusted, thresholdValues, opb, opLocation);

        // instantiate LFSR
        ModuleInstanceHelper instance(*this, GetUnknownLocation());
        instance.SetModuleName("KanagawaLfsr");
        instance.AddU64Parameter("WIDTH", c_stallLfsrBits);
        instance.SetInstanceName("lfsr_inst");
        instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetI1Type(), "clk");
        instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, GetI1Type(), "rst");
        instance.AddPort("en", circt::hw::ModulePort::Direction::Input,
                         opb.create<circt::hw::ConstantOp>(opLocation, opb.getIntegerAttr(opb.getI1Type(), 1)));
        instance.AddPort("lfsr_out", circt::hw::ModulePort::Direction::Output,
                         GetIntegerType(c_stallLfsrBits));
        instance.Generate(&opb);
        const mlir::Value lfsrValue = instance.GetPortValue("lfsr_out");

        // compare with LFSR output and register the result
        const mlir::Value lfsrGeThresholdFinal = opb.create<circt::comb::ICmpOp>(
            opLocation, circt::comb::ICmpPredicate::uge, lfsrValue, thresholdFinal, TwoState);

        circt::seq::CompRegOp stallCompareReg = opb.create<circt::seq::CompRegOp>(
            opLocation, lfsrGeThresholdFinal, _compileContext.PortNameToValue("clk"),
            _compileContext.PortNameToValue("rst"),
            opb.create<circt::hw::ConstantOp>(opLocation, opb.getIntegerAttr(opb.getI1Type(), 0)),
            GetFullyQualifiedStringAttr(basicBlock.GetObjectPath(), "stall_compare_ff"));
        // write back to destination operand
        StoreValueIntoDestOperand(op, 0, stallCompareReg);
    }

    void CompileBypassMemoryCirct(const Operation& op)
    {
        assert(Opcode::BypassMemory == op._opcode);
        assert(op._src.size() == (op._dst.size() + 1));

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        const size_t numOperands = op._dst.size();

        const BypassStoreGroupRecord& bsgr =
            SafeLookup(_compileContext._bypassStoreMap, op._flags._bypassMemory._bypassGroupIndex);

        const size_t writePort = bsgr._writePort;

        const Operation& loadMemoryOp = *(SafeLookup(bsgr._loadMemoryMap, op._flags._bypassMemory._loadMemoryKey));
        assert(Opcode::LoadMemory == loadMemoryOp._opcode);

        // Time from the cycle where the address is presented, until where bypass occurs
        assert(bsgr._bypassStage >= bsgr._loadStage);
        const size_t readLatencyForBypass = (bsgr._bypassStage - bsgr._loadStage) + 1;

        const RegisterDescription& memoryDesc = _program._registerTable[bsgr._memoryIndex];

        const BypassAndWriteDelay bawd = CalculateMemoryBypassAndWriteDelay(true, readLatencyForBypass, memoryDesc);

        // Check that hardened bypass for bypass op matches hardened bypass for
        // corresponding memory
        {
            const size_t readPort = loadMemoryOp._flags._loadMemory._readPort;
            const bool isBypassReadPort = memoryDesc.Memory().IsBypassReadPort(readPort);
            const size_t memoryReadLatency = memoryDesc.Memory().GetReadLatency(readPort);

            const BypassAndWriteDelay memoryBawd =
                CalculateMemoryBypassAndWriteDelay(isBypassReadPort, memoryReadLatency, memoryDesc);

            assert(memoryBawd._hardenedBypass == bawd._hardenedBypass);
        }

        const std::string wrenStr = MemoryWriteEnableString(bsgr._memoryIndex, writePort);
        const std::string writeAddrPortName = MemoryWriteAddrString(bsgr._memoryIndex, writePort);
        const std::string writeDataPortName = MemoryWriteDataString(bsgr._memoryIndex, writePort);

        const size_t readPort = loadMemoryOp._flags._loadMemory._readPort;

        bool readDataEarly = true;

        // Determine if the read address can be provided 1 cycle early
        const bool readAddressEarly = IsPipelineRegisterNextValueAvailableCirct(op, 0);

        // Determine if the memory output data can be provided 1 cycle early
        for (size_t i = 0; i < numOperands; ++i)
        {
            if (RegisterType::BitBucket !=
                _program._registerTable[op._dst[i].GetAccessedRegister()._registerIndex]._type)
            {
                if (!IsPipelineRegisterNextValueAvailableCirct(op, i + 1))
                {
                    readDataEarly = false;

                    break;
                }
            }
        }

        const size_t dataWidth = memoryDesc.Memory()._elementWidth;
        const size_t addressWidth = memoryDesc.GetMemoryAddressWidth();

        // Generate bypass input data (output of memory load)
        const auto generateInputValue = [&]()
        {
            mlir::Value inputValue;

            mlir::SmallVector<mlir::Value> inputValues;

            // Source operand 0 is the address
            // All remaining operands are the values to be modified
            assert(!op._src.empty());

            // Use SparseConcat for the situation of 1 slice and zero padding is required
            SparseConcat sparseConcat(opb, opLocation, dataWidth);
            size_t offsetVal = 0;

            for (size_t i = 0; i < op._dst.size(); i++)
            {
                const size_t srcOpIndex = i + 1;
                const size_t dstWidth = op._dst[i].Width(_program);
                sparseConcat.Insert(offsetVal, SourceOperandToMlirValue(op, srcOpIndex, dstWidth));
                offsetVal += dstWidth;
            }

            inputValue = sparseConcat.Flush();

            assert(GetMlirTypeWidth(inputValue.getType()) == dataWidth);

            return inputValue;
        };

        const auto generateInputAddress = [&]() { return SourceOperandToMlirValue(op, 0, addressWidth); };

        const mlir::Value inputValue =
            GenerateNonPipelinedValueAtStage(opLocation, readDataEarly ? 1 : 0, generateInputValue);

        const mlir::Value inputAddress =
            GenerateNonPipelinedValueAtStage(opLocation, readAddressEarly ? 1 : 0, generateInputAddress);

        ModuleInstanceHelper instance(*this, opLocation);

        instance.SetModuleName("KanagawaMemoryBypass");
        instance.SetInstanceName("memory_bypass_" + std::to_string(op._flags._bypassMemory._bypassGroupIndex) + "_" +
                                 std::to_string(readPort));

        instance.AddU64Parameter("DATA_WIDTH", dataWidth);
        instance.AddU64Parameter("ADDR_WIDTH", addressWidth);
        instance.AddU64Parameter("NUM_BYPASS_SLOTS", bawd._bypassSlots);
        instance.AddBoolParameter("READ_ADDRESS_EARLY", readAddressEarly);
        instance.AddBoolParameter("READ_DATA_EARLY", readDataEarly);

        instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, GetClockType(), "clk");
        instance.AddPort("read_addr_in", circt::hw::ModulePort::Direction::Input,
                         instance.GetParameterizedIntegerType("ADDR_WIDTH"), inputAddress);
        instance.AddPort("read_data_in", circt::hw::ModulePort::Direction::Input,
                         instance.GetParameterizedIntegerType("DATA_WIDTH"), inputValue);
        instance.AddPort("read_data_out", circt::hw::ModulePort::Direction::Output,
                         instance.GetParameterizedIntegerType("DATA_WIDTH"));

        instance.AddPort("wren_in", circt::hw::ModulePort::Direction::Input, GetI1Type(), wrenStr);
        instance.AddPort("write_addr_in", circt::hw::ModulePort::Direction::Input,
                         instance.GetParameterizedIntegerType("ADDR_WIDTH"), writeAddrPortName);
        instance.AddPort("data_in", circt::hw::ModulePort::Direction::Input,
                         instance.GetParameterizedIntegerType("DATA_WIDTH"), writeDataPortName);

        instance.Generate();

        // Store results
        for (size_t i = 0; i < numOperands; ++i)
        {
            const size_t offset = loadMemoryOp._flags._loadMemory._sourceOffsets->at(i);

            const size_t width = op._dst[i].Width(_program);

            const mlir::Value resultSubset =
                opb.create<circt::comb::ExtractOp>(opLocation, instance.GetPortValue("read_data_out"), offset, width);

            StoreValueIntoDestOperand(op, i, resultSubset);
        }
    }

    void CompileInlineExternalModuleCirct(const Operation& op)
    {
        assert(Opcode::InlineExternalModule == op._opcode);

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const size_t externalModuleIndex = op._flags._callInlineExternalModule._externalModuleIndex;

        const ExternalModuleCall& externModuleCall = _program._externalModuleCalls[externalModuleIndex];

        assert(externModuleCall._srcOperandWidths.size() == op._src.size());

        // The pipeline stage that the scheduler placed htis op in
        const size_t scheduledStage = _compileContext._stage->_atomicSequence;

        // Restore the current stage at the end of this function
        RestoreContextStage restoreContextStage(_compileContext);

        // Input operands
        // The module instance is created in the stage where inputs should be read
        const size_t readInputStage = OperationHasRegisteredInput(op) ? (scheduledStage - 1) : scheduledStage;

        _compileContext.SetCurrentStage(_compileContext.GetLastStage(readInputStage));

        llvm::SmallVector<mlir::Type> resultTypes;
        llvm::SmallVector<mlir::Value> resultValues;
        // Instantiate the module
        if (externModuleCall._type == ExternalModuleCallType::InstantiateInBasicBlock)
        {
            ModuleInstanceHelper instance(*this, opLocation);

            instance.SetModuleName(externModuleCall._name);

            instance.SetInstanceName(externModuleCall._name + "_" + std::to_string(externalModuleIndex));

            instance.AddStringParameter("DEVICE_FAMILY", GetCodeGenDeviceConfig()._halDeviceFamily);

            instance.AddPort("clk", circt::hw::ModulePort::Direction::Input, _compileContext._circtPipeline.getClock());
            instance.AddPort("rst", circt::hw::ModulePort::Direction::Input, _compileContext._circtPipeline.getReset());

            // src[0] is the predicate
            instance.AddPort("enable_in", circt::hw::ModulePort::Direction::Input,
                             GetPredicatedStageEnable(op, 0, true));

            for (size_t i = 1; i < op._src.size(); i++)
            {
                // op._src[i].Width() is may not match the width of
                // the extern function param (if copy propagation
                // removed some size-changing moves)
                const size_t width = externModuleCall._srcOperandWidths[i];

                instance.AddPort(externModuleCall._srcOperandNames[i] + "_in", circt::hw::ModulePort::Direction::Input,
                                 SourceOperandToMlirValue(op, i, width));
            }

            const auto getOutputPortName = [](const size_t i)
            {
                // Only output i for > 0, so that the non-struct case is not ugly
                std::string portName = "result_out";

                if (i > 0)
                {
                    portName += std::to_string(i);
                }

                return portName;
            };

            for (size_t i = 0; i < op._dst.size(); i++)
            {
                // op._dst[i].Width() is may not match the width of
                // the extern function param (if copy propagation
                // removed some size-changing moves)
                const size_t width = externModuleCall._destinationOperandWidths[i];

                const mlir::Type resultType = GetIntegerType(width);

                instance.AddPort(getOutputPortName(i), circt::hw::ModulePort::Direction::Output, resultType);

                resultTypes.push_back(resultType);
            }

            instance.Generate();

            for (size_t i = 0; i < op._dst.size(); i++)
            {
                resultValues.push_back(instance.GetPortValue(getOutputPortName(i)));
            }
        }
        else
        {
            const std::string prefix = (externModuleCall._type == ExternalModuleCallType::ExternClassMethod)
                                           ? externModuleCall.GetFullyQualifiedName()
                                           : externModuleCall._name;

            _compileContext._batchAssignments.Append(opLocation, _compileContext.PortNameToValue(prefix + "_valid_out"),
                                                     GetPredicatedStageEnable(op, 0, true));

            for (size_t i = 1; i < op._src.size(); i++)
            {
                const size_t width = externModuleCall._srcOperandWidths[i];
                _compileContext._batchAssignments.Append(
                    opLocation,
                    _compileContext.PortNameToValue(prefix + "_" + externModuleCall._srcOperandNames[i] + "_out"),
                    SourceOperandToMlirValue(op, i, width));
            }

            const auto getInputPortName = [](const std::string prefix)
            {
                std::string portName = prefix + "_result_in";
                return portName;
            };

            // result port may contain multiple dst operands
            if (op._dst.size() > 0)
            {
                const mlir::Value resultVal = _compileContext.PortNameToValue(getInputPortName(prefix));
                size_t offset = 0;
                for (size_t i = 0; i < op._dst.size(); i++)
                {
                    const size_t width = externModuleCall._destinationOperandWidths[i];
                    const mlir::Type resultType = GetIntegerType(width);
                    resultTypes.push_back(resultType);
                    const mlir::Value resultSlice =
                        opb.create<circt::comb::ExtractOp>(opLocation, resultVal, offset, width);
                    offset += width;
                    resultValues.push_back(resultSlice);
                }
            }
        }

        const size_t resultAvailableStage = OperationHasRegisteredOutput(op) ? (scheduledStage + 1) : scheduledStage;

        assert(resultAvailableStage >= readInputStage);
        size_t latency = resultAvailableStage - readInputStage;

        // Handle additional latency beyond input and output registers
        // 1) Increase the latency passed to LatencyOp, to prevent auto-pipelining until the approriate point
        // 2) Find chains of MoveLatencyPlaceholder operations, which start with the output of this operation
        // and store the output of this operation in the register written by the end of the chain,
        // so that the consumer of the end of the chain reads the correct value.
        assert(externModuleCall._functionDesc._fixedLatency);
        if (*externModuleCall._functionDesc._fixedLatency > 2)
        {
            latency += (*externModuleCall._functionDesc._fixedLatency - 2);
        }

        // Use a latency op to indicate that the results should be sampled on a later pipeline stage
        // Only emit latency op when the operator has a non-empty destination
        if (!op._dst.empty())
        {
            LatencyOpHelper latencyOpHelper(opLocation, opb, resultValues, latency);

            for (size_t i = 0; i < op._dst.size(); i++)
            {
                const DestinationOperand& dstOp = op._dst[i];

                const size_t dstReg = dstOp.GetAccessedRegister()._registerIndex;

                // There could be a chain of MoveLatencyPlaceholder operations
                // starting with dstReg
                // If so, then the final value should be written to the end of that chain
                // The implementation of MoveLatencyPlaceholder operations is a NOP when targeting CIRCT
                const LatencyRecord lr =
                    LookupWithDefault(_compileContext._latencyMap, dstReg, LatencyRecord{dstReg, 0});

                _compileContext.SetCurrentStage(_compileContext.GetLastStage(resultAvailableStage + lr._extraLatency));

                const size_t dstWidth = _program._registerTable[lr._registerIndex]._width;

                // Truncation may be necessary if
                // which can occur if copy propagation eliminated a narrowing move
                const mlir::Value resultValue = latencyOpHelper.GetResult(i);

                assert(GetMlirValueWidth(resultValue) >= dstWidth);

                const mlir::Value adjustedValue = AdjustValueWidth(resultValue, dstWidth, false, opb, opLocation);

                SafeInsert(_compileContext._regToValue, lr._registerIndex, adjustedValue);
            }
        }
    }

    void CompileOperationCirct(const Operation& op, const bool preStartCondition)
    {
        switch (op._opcode)
        {
            // Acquire and release are handled at the beginning and end of the basic block
            // The operations are in the basic block to signify that acquire/release should happen
            // and to prevent the basic block from being optimized away
        case Opcode::AcquireSemaphore:
        case Opcode::ReleaseSemaphore:
            break;

            // Taken care of in individual operations and _compileContext._latencyMap
        case Opcode::MovLatencyPlaceholder:
            break;

        case Opcode::DebugView:
            if (!GetCodeGenConfig()._suppressDebugView)
            {
                CompileDebugViewCIRCT(op);
            }
            break;

        case Opcode::Mov:
        case Opcode::UnaryOp:
        case Opcode::Gather:
        case Opcode::Lut:
        case Opcode::Select:
        case Opcode::Clear:
            ConvertOpToCirct(op, _compileContext.OpBuilder(), _program, GetSourceOperandToMlirValue(),
                             GetStoreMlirValueInDestOperand());
            break;

        case Opcode::BinaryOp:
            if (op._flags._binaryOpType == ParseTreeBinaryOpTypeLutMul)
            {
                const bool lhsSigned = op.ShouldSignExtend(0);
                const bool rhsSigned = op.ShouldSignExtend(1);

                const std::string lutMulInstName = "lut_mul_u" + std::to_string(_compileContext._lutMulCount++);

                ModuleInstanceHelper instance(*this, OperationToCirctLocation(op, _program));
                instance.SetModuleName("KanagawaLutMul");
                instance.AddU64Parameter("DW_X", op._src[0].Width(_program));
                instance.AddU64Parameter("DW_Y", op._src[1].Width(_program));
                instance.AddU64Parameter("X_SIGNED", lhsSigned);
                instance.AddU64Parameter("Y_SIGNED", rhsSigned);
                instance.AddU64Parameter("DW_Z", op._dst[0].Width(_program));
                instance.SetInstanceName(lutMulInstName);
                instance.AddPort("x_in", circt::hw::ModulePort::Direction::Input,
                                 instance.GetParameterizedIntegerType("DW_X"),
                                 SourceOperandToMlirValue(op, 0, op._src[0].Width(_program)));
                instance.AddPort("y_in", circt::hw::ModulePort::Direction::Input,
                                 instance.GetParameterizedIntegerType("DW_Y"),
                                 SourceOperandToMlirValue(op, 1, op._src[1].Width(_program)));
                instance.AddPort("z_out", circt::hw::ModulePort::Direction::Output,
                                 instance.GetParameterizedIntegerType("DW_Z"));
                instance.Generate(&_compileContext.OpBuilder());
                StoreValueIntoDestOperand(op, 0, instance.GetPortValue("z_out"));
            }
            else
            {
                ConvertOpToCirct(op, _compileContext.OpBuilder(), _program, GetSourceOperandToMlirValue(),
                                 GetStoreMlirValueInDestOperand());
            }
            break;

        case Opcode::Enqueue:
            CompileEnqueueCirct(op);
            break;

        case Opcode::Assert:
            CompileAssertCirct(op, preStartCondition);
            break;

        case Opcode::Print:
            CompilePrintCirct(op);
            break;

        case Opcode::WriteGlobal:
            CompileWriteGlobalCirct(op, preStartCondition);
            break;

        case Opcode::LoadMemory:
            CompileLoadMemoryCirct(op);
            break;

        case Opcode::StoreMemory:
            CompileStoreMemoryCirct(op);
            break;

        case Opcode::CycleCounter:
            CompileCycleCounterCirct(op);
            break;

        case Opcode::ReadSelectedFifo:
            CompileReadSelectedFifoCirct(op);
            break;

        case Opcode::StartCondition:
            CompileStartConditionCirct(op);
            break;

        case Opcode::BypassMemory:
            CompileBypassMemoryCirct(op);
            break;

        case Opcode::StallCheck:
            CompileStallCheckCirct(op);
            break;

        case Opcode::InlineExternalModule:
            CompileInlineExternalModuleCirct(op);
            break;

        case Opcode::EnqueueRegisters:
            CompileEnqueueRegistersCirct(op);
            break;

        case Opcode::DequeueRegisters:
            CompileDequeueRegistersCirct(op);
            break;

        case Opcode::FormatString:
            CompileFormatStringCirct(op);
            break;

        case Opcode::FormatEnum:
            CompileFormatEnumCirct(op);
            break;

        case Opcode::ReferenceString:
            CompileReferenceStringCirct(op);
            break;

        case Opcode::AssertStringEqual:
            CompileAssertStringEqualCirct(op);
            break;

        case Opcode::StringCount:
            CompileStringCountCirct(op);
            break;

        case Opcode::LineNumber:
        case Opcode::NoOp:
            // Nothing to do
            break;

        default:
            std::cout << "Unsupported opcode: " << GetOpcodeString(_program, op) << "\n";
            assert(false);
        }
    }

    std::string ToString(const AccessedRegister& reg, const RegisterAccessType accessType) const
    {
        const RegisterDescription& regDesc = _program._registerTable[reg._registerIndex];

        std::ostringstream str;

        switch (regDesc._type)
        {
        case RegisterType::Memory:
            assert(false);
            if (accessType == RegisterAccessType::Write)
            {
                str << "memory_write_data_out_" << reg._registerIndex;
            }
            else
            {
                str << "memory_read_data_in_" << reg._registerIndex;
            }
            break;

        case RegisterType::Global:
            if (accessType == RegisterAccessType::Read)
            {
                str << GetGlobalInName(reg._registerIndex);
            }
            else
            {
                str << GetGlobalInNextName(reg._registerIndex);
            }
            break;

        case RegisterType::GlobalView:
            str << GetGlobalViewInName(reg._registerIndex);
            break;

        case RegisterType::Pipeline:
            str << "reg" << reg._registerIndex << "_" << regDesc._name;

            if (accessType == RegisterAccessType::Write)
            {
                str << "_next";
            }
            else
            {
                str << "_ff";
            }
            break;

        case RegisterType::Fifo:
            switch (regDesc.Fifo()._type)
            {
            case FifoType::Default:
                if (accessType == RegisterAccessType::Write)
                {
                    str << "fifo_data_out_" << SafeLookup(_compileContext._writtenFifoMap, reg._registerIndex);
                }
                else
                {
                    str << "data_in_" << _fifoNamer.GetNormFifoIndex(reg._registerIndex);
                }
                break;

            case FifoType::ContextSaverCaller:
                assert(accessType == RegisterAccessType::Write);
                str << "context_saver_caller_data_" << SafeLookup(_compileContext._writtenFifoMap, reg._registerIndex);
                break;

            default:
                assert(false);
            }
            break;

        case RegisterType::Wire:
            str << "wire" << reg._registerIndex << "_" << regDesc._name;
            break;

        case RegisterType::BitBucket:
            assert(false);
            break;

        case RegisterType::Local:
        {
            // Only occurs for the parameter to atomic funcions
            const auto it = _localRegisterNameMap.find(reg._registerIndex);

            assert(_localRegisterNameMap.end() != it);

            str << it->second;
        }
        break;

        default:
            assert(false);
        }

        return str.str();
    }

    std::string ToString(const FifoSubset& fifo, const RegisterAccessType accessType) const
    {
        const RegisterDescription& regDesc = _program._registerTable[fifo._registerIndex];

        std::ostringstream str;

        switch (regDesc.Fifo()._type)
        {
        case FifoType::Default:
        case FifoType::ReorderBuffer:
        case FifoType::Passthrough:
        case FifoType::PassthroughRegistered:
        case FifoType::PassthroughUnregistered:
            if (accessType == RegisterAccessType::Write)
            {
                str << "fifo_data_out_" << SafeLookup(_compileContext._writtenFifoMap, fifo._registerIndex);
            }
            else
            {
                str << "data_in_" << _fifoNamer.GetNormFifoIndex(fifo._registerIndex);
            }
            break;

        case FifoType::ContextSaverCaller:
            assert(accessType == RegisterAccessType::Write);
            str << "context_saver_caller_data_" << SafeLookup(_compileContext._writtenFifoMap, fifo._registerIndex);
            break;

        case FifoType::FixedDelay:
            str << "data_propagation_in_" << _fifoNamer.GetNormFifoIndex(fifo._registerIndex);
            break;

        default:
            assert(false);
        }

        str << "[" << fifo._offset + fifo._width - 1 << ":" << fifo._offset << "]";

        return str.str();
    }

    std::string ToString(const DestinationOperand& destOp, SourceWriter& writer,
                         const DestOpToStringMode mode = DestOpToStringMode::Default)
    {
        switch (destOp.Type())
        {
        case DestinationOperandType::Register:
        {
            const size_t registerIndex = destOp.GetAccessedRegister()._registerIndex;

            const RegisterDescription& regDesc = _program._registerTable[registerIndex];

            if (RegisterType::Global == regDesc._type)
            {
                assert(mode == DestOpToStringMode::Default);

                const size_t writeIndex = destOp.GetWriteIndex();

                std::ostringstream str;

                str << GetGlobalOutName(registerIndex, writeIndex);

                // Also write the valid bit
                assert(_compileContext._basicBlock);
                assert(_compileContext._stage);
                writer.Str() << GetGlobalValidOutName(registerIndex, writeIndex) << " = "
                             << GetInputValid(GetCurrentBasicBlock(), GetCurrentStage()) << ";";

                return str.str();
            }
            else
            {
                if (mode == DestOpToStringMode::FFValue)
                {
                    // FFValue is for output pipeline registered of hardended blocks (like memories)
                    // The output of the block is wired directly to the _ff value of the pipeline register
                    assert(RegisterType::Pipeline == regDesc._type);

                    // Remember that this pipeline register is special
                    _hardenedOutputRegisters.insert(registerIndex);
                }

                return ToString(destOp.GetAccessedRegister(), (mode == DestOpToStringMode::Default)
                                                                  ? RegisterAccessType::Write
                                                                  : RegisterAccessType::Read);
            }
            break;
        }

        case DestinationOperandType::Fifo:
            return ToString(destOp.GetFifoSubset(), RegisterAccessType::Write);
            break;

        default:
            assert(false);
            return std::string("");
        }
    }

    std::string SourceOpToString(const Operation& op, const size_t operandIndex, const size_t desiredWidth,
                                 const SourceOpToStringMode modeIn = SourceOpToStringMode::Default,
                                 const bool promoteToSigned = false, bool* madeSigned = nullptr) const
    {
        std::ostringstream str;

        const SourceOperand& sourceOperand = op._src[operandIndex];

        const bool signExtend = op._signExtendSourceMask & (1ull << operandIndex);

        std::ostringstream srcStr;

        SourceOpToStringMode mode = modeIn;

        switch (sourceOperand.Type())
        {
        case SourceOperandType::Literal:
            srcStr << sourceOperand.GetLiteral();
            break;

        case SourceOperandType::Register:
        {
            if (modeIn == SourceOpToStringMode::NextValue)
            {
                const size_t registerIndex = sourceOperand.GetAccessedRegister()._registerIndex;

                // SourceOpToStringMode::NextValue should only be used in cases where the source is known to be a
                // pipeline register
                const RegisterType registerType = _program._registerTable[registerIndex]._type;

                assert((RegisterType::Pipeline == registerType) || (RegisterType::Global == registerType) ||
                       (RegisterType::GlobalView == registerType));

                // Determine if _next will be used
                const bool opSrcHasNextValue = DetectOpsUseNextValue(op);
                // Assert this is true for future-proof
                assert(opSrcHasNextValue);
            }

            // If mode == SourceOpToStringMode::NextValue, then RegisterAccessType::Write is used to to select
            // _next (snap on the previous cycle)
            srcStr << ToString(sourceOperand.GetAccessedRegister(), (mode == SourceOpToStringMode::Default)
                                                                        ? RegisterAccessType::Read
                                                                        : RegisterAccessType::Write);
        }
        break;

        case SourceOperandType::Fifo:
            assert(mode == SourceOpToStringMode::Default);
            srcStr << ToString(sourceOperand.GetFifoSubset(), RegisterAccessType::Read);
            break;

        default:
            assert(false);
        }

        bool useSignedFunc;
        switch (op._flags._binaryOpType)
        {
        case ParseTreeBinaryOpTypeEQ:
        case ParseTreeBinaryOpTypeNE:
        case ParseTreeBinaryOpTypeGT:
        case ParseTreeBinaryOpTypeGE:
        case ParseTreeBinaryOpTypeLT:
        case ParseTreeBinaryOpTypeLE:
        case ParseTreeBinaryOpTypeShr:
        case ParseTreeBinaryOpTypeShl:
        case ParseTreeBinaryOpTypeLutMul:
            useSignedFunc = true;
            break;

        default:
            useSignedFunc = false;
        }

        // Sign-extend to desiredWidth
        const size_t srcWidth = sourceOperand.Width(_program);
        const size_t numAdditionalBits = desiredWidth > srcWidth ? desiredWidth - srcWidth : 0;

        if (signExtend)
        {
            // right shift is implemented with operator >>>
            // which uses the signedness of the lhs to determine if it shift shift in 1s
            if (useSignedFunc)
            {
                str << "$signed(";
                if (madeSigned != nullptr)
                    *madeSigned = true;
            }

            if (numAdditionalBits > 0)
            {
                if (sourceOperand.Type() == SourceOperandType::Literal)
                {
                    const size_t highBit =
                        bit_test(sourceOperand.GetLiteral()._value, sourceOperand.GetLiteral()._width - 1) ? 1 : 0;

                    str << "{{" << numAdditionalBits << "{1'b" << highBit << "}}, " << srcStr.str() << "}";
                }
                else
                {
                    assert(sourceOperand.Type() == SourceOperandType::Register);

                    str << "{{" << numAdditionalBits << "{"
                        << ToString(sourceOperand.GetAccessedRegister(), RegisterAccessType::Read) << "["
                        << srcWidth - 1 << ":" << srcWidth - 1 << "]}}, " << srcStr.str() << "}";
                }
            }
            else
            {
                str << srcStr.str();
            }

            if (useSignedFunc)
            {
                str << ")"; // end $signed
            }
        }
        else if (promoteToSigned && useSignedFunc)
        {
            // This is an unsigned operand that needs to be promoted to signed
            // Append a zero as the most significant bit
            // and then used $signed
            // to ensure that comparisons of signed and unsigned operands behave correctly
            if (op._flags._binaryOpType == ParseTreeBinaryOpTypeLutMul && op._opcode == Opcode::BinaryOp)
            {
                str << "$signed({1'b0, " << srcStr.str() << "})";
            }
            else
            {
                str << "$signed(" << srcStr.str() << ")";
            }
            if (madeSigned != nullptr)
                *madeSigned = true;
        }
        else
        {
            if (numAdditionalBits > 0)
            {
                str << "{{" << numAdditionalBits << "{1'b0}}," << srcStr.str() << "}";
            }
            else
            {
                str << srcStr.str();
            }
        }

        return str.str();
    }

    // Returns bits [0,n] of a source operand
    std::string SourceOpSubsetToString(const Operation& op, const size_t operandIndex, const size_t desiredWidth) const
    {
        assert(desiredWidth > 0);

        std::ostringstream str;

        const SourceOperand& sourceOperand = op._src[operandIndex];

        switch (sourceOperand.Type())
        {
        case SourceOperandType::Literal:
        {
            // Ensure the output literal has the desired number of bits
            Literal literalVal = sourceOperand.GetLiteral();

            // Zero out any bits beyond literalVal._width
            const mp_int mask = (mp_int(1) << literalVal._width) - 1;
            literalVal._value = literalVal._value & mask;

            // Set the new desired width
            literalVal._width = desiredWidth;

            str << literalVal;
        }
        break;

            // Use [] to select
        case SourceOperandType::Register:
            str << SourceOpToString(op, operandIndex, desiredWidth);
            str << "[" << desiredWidth - 1 << ":0]";
            break;

        default:
            assert(false);
        }

        return str.str();
    }

    // Returns true if a given source operand was computed on an earlier pipeline stage
    bool IsPipelineRegisterNextValueAvailableCirct(const Operation& op, const size_t operandIndex) const
    {
        bool result = false;

        assert(operandIndex < op._src.size());

        const SourceOperand& srcOp = op._src[operandIndex];

        if (SourceOperandType::Register == srcOp.Type())
        {
            const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

            if (RegisterType::Pipeline == _program._registerTable[registerIndex]._type)
            {
                // Get the operation that produced the operand
                const mlir::Value operandValue = SafeLookup(_compileContext._regToValue, registerIndex);

                mlir::Operation* const defOp = operandValue.getDefiningOp();
                assert(defOp);

                mlir::Block* const block = defOp->getBlock();

                const auto it = _compileContext._blockToStageIndex.find(block);
                if (it != _compileContext._blockToStageIndex.end())
                {
                    const size_t stage = it->second;

                    if (stage < _compileContext._stage->_atomicSequence)
                    {
                        result = true;
                    }
                }
            }
        }

        return result;
    }

    std::string MemoryWriteEnableString(const size_t memoryIndex, const size_t writePort)
    {
        return "memory_wren_" + std::to_string(memoryIndex) + "_" + std::to_string(writePort);
    }

    std::string MemoryWriteAddrString(const size_t memoryIndex, const size_t writePort)
    {
        return "memory_write_addr_out_" + std::to_string(memoryIndex) + "_" + std::to_string(writePort);
    }

    std::string MemoryWriteDataString(const size_t memoryIndex, const size_t writePort)
    {
        return "memory_write_data_out_" + std::to_string(memoryIndex) + "_" + std::to_string(writePort);
    }

    std::string GlobalVariableModuleName(const size_t index)
    {
        std::ostringstream str;

        const RegisterDescription& regDesc = _program._registerTable[index];

        str << GetModuleNamePrefix() << "global_var_";

        if (!regDesc._name.empty())
        {
            str << regDesc._name << "_";
        }

        str << index;

        // Avoid quartus crashes with long module names
        return g_compiler->ClampStringLength(str.str());
    }

    // Generates a value at pipeline stage = generateStageIndex
    // and make that available in the current pipeline stage (without any pipelining)
    // generateStageOffset = 0 means to generate the value at the current pipeline stage
    // generateStageOffset = 1 means to generate the value at the previous pipeline stage
    mlir::Value GenerateNonPipelinedValueAtStage(const mlir::Location location, const size_t generateStageOffset,
                                                 const std::function<mlir::Value(void)>& callback)
    {
        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const size_t currStageIndex = _compileContext._stage->_atomicSequence;

        assert(currStageIndex >= generateStageOffset);

        const size_t generateStageIndex = currStageIndex - generateStageOffset;

        mlir::Value result;

        if (generateStageIndex != currStageIndex)
        {
            // Restore current pipeline stage at the end of this scope
            RestoreContextStage restoreStage(_compileContext);

            const Stage* const targetStage = _compileContext.GetLastStage(generateStageIndex);

            _compileContext.SetCurrentStage(targetStage);

            // Call the callback to generate the result
            const mlir::Value resultInPrevStage = callback();

            // Insert a LatencyOp to ensure the value is not pipelined to the current stage
            llvm::SmallVector<mlir::Type> resultTypes;
            llvm::SmallVector<mlir::Value> resultValues;

            resultTypes.push_back(resultInPrevStage.getType());
            resultValues.push_back(resultInPrevStage);

            circt::pipeline::LatencyOp latencyOp = opb.create<circt::pipeline::LatencyOp>(
                location, resultTypes, opb.getIntegerAttr(opb.getI32Type(), generateStageOffset));

            latencyOp.getBody().emplaceBlock();

            {
                circt::OpBuilder::InsertionGuard g(opb);

                opb.setInsertionPointToStart(latencyOp.getBodyBlock());

                opb.create<circt::pipeline::LatencyReturnOp>(location, resultValues);
            }

            result = latencyOp.getResult(0);
        }
        else
        {
            // Generating a value in the same pipeline stage where it will be used
            result = callback();
        }

        return result;
    }

    void AddGlobalDebugSignal(const std::string& container, const std::string& signal)
    {
        JsonValue debugSignal = JsonValue::CreateObject();

        debugSignal.AddMember("container", _rtlMap.SerializeString(container));
        debugSignal.AddMember("name", _rtlMap.SerializeString(signal));

        _jsonDebugSignals.PushBack(debugSignal);
    }

    void AddBasicBlockDebugSignal(const std::string& name)
    {
        assert(_compileContext._basicBlockDebugSignals);

        JsonValue debugSignal = JsonValue::CreateObject();

        debugSignal.AddMember("name", _rtlMap.SerializeString(name));

        _compileContext._basicBlockDebugSignals->PushBack(debugSignal);
    }

    void AddBasicBlockDebugInstance(const std::string& name)
    {
        assert(_compileContext._basicBlockDebugInstances);

        JsonValue debugInstance = JsonValue::CreateObject();

        debugInstance.AddMember("name", _rtlMap.SerializeString(name));

        _compileContext._basicBlockDebugInstances->PushBack(debugInstance);
    }

    void OpenCirctBasicBlock()
    {
        const BasicBlock& basicBlock = *_compileContext._basicBlock;

        mlir::Location mlirBbLocation = LocationToCirctLocation(basicBlock._location);

        // add in call stack
        mlirBbLocation = CallStackToCirctLocation(_program, basicBlock._callStackIndex, mlirBbLocation);

        // The module for the basic block is nested inside of the module created by _coreModule
        assert(_mlirModule);
        circt::OpBuilder opb(_mlirModule.getBodyRegion());

        llvm::SmallVector<circt::hw::PortInfo> basicBlockPorts;

        for (const BasicBlockPortInfo& pi : _compileContext._basicBlockPorts)
        {
            basicBlockPorts.push_back(pi._portInfo);
        }

        const std::string moduleName = GetModuleNamePrefix() + GetBasicBlockName(basicBlock);

        _compileContext._hwModule =
            opb.create<circt::hw::HWModuleOp>(mlirBbLocation, opb.getStringAttr(moduleName), basicBlockPorts);

        SafeInsert(_nameToHwModule, moduleName, _compileContext._hwModule);

        mlir::Block* const hwModuleBodyBlock = _compileContext._hwModule.getBodyBlock();

        // Remove the default terminator in the module
        // It will be replaced with an OutputOp
        hwModuleBodyBlock->getTerminator()->erase();

        opb.setInsertionPointToEnd(hwModuleBodyBlock);

        mlir::SmallVector<mlir::Type> pipelineResultTypes(static_cast<size_t>(CirctPipelineResultValues::Count));
        mlir::SmallVector<mlir::Attribute> outputNames(static_cast<size_t>(CirctPipelineResultValues::Count));

        pipelineResultTypes[static_cast<size_t>(CirctPipelineResultValues::Done)] = opb.getI1Type();
        outputNames[static_cast<size_t>(CirctPipelineResultValues::Done)] = StringToStringAttr("pipeline_done");

        // Connect module input ports to pipeline input ports
        // Start with the go signal, and add others
        mlir::SmallVector<mlir::Value> pipelineInputValues;
        mlir::SmallVector<mlir::Attribute> inputNames;

        // Input value 0 is the go signal
        static_assert(static_cast<size_t>(CirctModulePort::Done) == static_cast<size_t>(CirctModulePort::Reset) + 1,
                      "CirctModulePort::Done must follow CirctModulePort::Reset");

        // Add cycle count pipeline input if necessary
        if (BasicBlockUsesCycleCounter(basicBlock, OperationEnumerationMode::Scheduled))
        {
            // Backedge is needed because cycle counter output feeds back into itself
            circt::BackedgeBuilder beb(opb, mlirBbLocation);
            circt::Backedge backedge = beb.get(opb.getI64Type());

            // Add 1 each clock cycle
            const mlir::Value plusOne = opb.create<circt::comb::AddOp>(
                mlirBbLocation, backedge, opb.create<circt::hw::ConstantOp>(mlirBbLocation, opb.getI64IntegerAttr(1)),
                TwoState);

            const mlir::Value cycleCountReg = opb.create<circt::seq::CompRegOp>(
                mlirBbLocation, plusOne, hwModuleBodyBlock->getArgument(static_cast<size_t>(CirctModulePort::Clock)),
                hwModuleBodyBlock->getArgument(static_cast<size_t>(CirctModulePort::Reset)),
                opb.create<circt::hw::ConstantOp>(mlirBbLocation, opb.getI64IntegerAttr(0)), // reset value
                GetFullyQualifiedStringAttr(basicBlock.GetObjectPath(), "cycle_count_ff"));

            backedge.setValue(cycleCountReg);

            _compileContext._cycleCountReg = cycleCountReg;
        }

        size_t inputPortIndex = 0;

        for (const BasicBlockPortInfo& pi : _compileContext._basicBlockPorts)
        {
            SafeInsert(_compileContext._portNameToInfo, pi._portInfo.name.str(), pi._portInfo);

            if (pi._portInfo.dir == circt::hw::ModulePort::Direction::Output)
            {
                // Create a logicop that the will be connected to the output port
                const mlir::Value logicOp = opb.create<circt::sv::LogicOp>(
                    mlirBbLocation, pi._portInfo.type, pi._portInfo.name,
                    GetFullyQualifiedInnerSymAttr(basicBlock.GetObjectPath(), pi._portInfo.name.str()));

                SafeInsert(_compileContext._moduleOutputWireMap, pi._portInfo.name.str(), logicOp);
            }
            else
            {
                if (inputPortIndex >= static_cast<size_t>(CirctModulePort::Reset))
                {
                    assert(pi._portInfo.dir == circt::hw::ModulePort::Direction::Input);
                    if (pi._autoPipeline)
                    {
                        pipelineInputValues.push_back(hwModuleBodyBlock->getArgument(inputPortIndex));
                        inputNames.push_back(StringToStringAttr(pi._portInfo.name.str()));
                    }
                }

                SafeInsert(_compileContext._moduleInputPortMap, pi._portInfo.name.str(), inputPortIndex);

                inputPortIndex++;
            }
        }

        const StartNewThreadResult startNewThreadResult = CompileStartNewThreadCirct(opb);

        _compileContext._selectedFifoIndex = startNewThreadResult._selectedFifoIndex;

        _compileContext._startConditionWire = startNewThreadResult._startConditionWire;

        _compileContext._fiberIndexRegMatch = startNewThreadResult._fiberIndexRegMatch;

        _compileContext._allowNewThreadPreStartCondition = startNewThreadResult._startNewThreadPreStartCondition;

        // Build input port maps
        // The CIRCT pipeline will concatenate regular inputs and external inputs
        // into a single flat list (regular inputs first)
        assert(pipelineInputValues.size() == inputNames.size());

        for (size_t i = 0; i < pipelineInputValues.size(); i++)
        {
            SafeInsert(_compileContext._pipelineInputPortMap, mlir::cast<mlir::StringAttr>(inputNames[i]).str(), i);
        }

        // Add stall threshold selector if necessary
        if (GetCodeGenConfig()._stall > 0)
        {
            circt::BackedgeBuilder beb(opb, mlirBbLocation);
            circt::Backedge backedge = beb.get(opb.getIntegerType(3));

            // register threshold_sel_in when threshold_sel_valid_in is 1'b1
            const mlir::Value thresholdSelMux = opb.create<circt::comb::MuxOp>(
                mlirBbLocation, _compileContext.PortNameToValue("threshold_sel_valid_in"),
                _compileContext.PortNameToValue("threshold_sel_in"), backedge, TwoState);

            const size_t defaultRateSelector = GetCodeGenConfig()._stall;
            circt::seq::CompRegOp thresholdSelReg = opb.create<circt::seq::CompRegOp>(
                mlirBbLocation, thresholdSelMux, _compileContext.PortNameToValue("clk"),
                _compileContext.PortNameToValue("rst"),
                opb.create<circt::hw::ConstantOp>(mlirBbLocation,
                                                  opb.getIntegerAttr(opb.getIntegerType(3), defaultRateSelector)),
                GetFullyQualifiedStringAttr(basicBlock.GetObjectPath(), "threshold_sel_ff"));

            backedge.setValue(thresholdSelReg);

            const std::string wireName = "threshold_sel_wire";

            _compileContext._thresholdSelWire =
                opb.create<circt::sv::WireOp>(mlirBbLocation, thresholdSelReg.getType(), StringToStringAttr(wireName),
                                              GetFullyQualifiedInnerSymAttr(basicBlock.GetObjectPath(), wireName));

            opb.create<circt::sv::AssignOp>(mlirBbLocation, _compileContext._thresholdSelWire, thresholdSelReg);
        }

        // Declare the propagation FIFO instances
        DeclarePropagationFifos(basicBlock);

        // Setting the "name" field of ScheduledPipelineOp's build method to an empty string to remove the p0_* prefix
        // on the pipeline register.
        _compileContext._circtPipeline = opb.create<circt::pipeline::ScheduledPipelineOp>(
            mlirBbLocation, pipelineResultTypes, pipelineInputValues, opb.getArrayAttr(inputNames),
            opb.getArrayAttr(outputNames), hwModuleBodyBlock->getArgument(static_cast<size_t>(CirctModulePort::Clock)),
            startNewThreadResult._startNewThread,
            hwModuleBodyBlock->getArgument(static_cast<size_t>(CirctModulePort::Reset)));

        // Pipeline stages
        // The first pipeline stage is created with the pipeline
        mlir::Block* const firstStage = _compileContext._circtPipeline.getEntryStage();

        _compileContext._blockToStageIndex[firstStage] = 0;

        _compileContext._lastPipelineStage = firstStage;

        while (_compileContext._circtPipeline.getStages().size() <= basicBlock._stages.back()._atomicSequence)
        {
            mlir::Block* const newStage = _compileContext._circtPipeline.addStage();

            _compileContext._blockToStageIndex[newStage] = _compileContext._circtPipeline.getStages().size() - 1;

            _compileContext._lastPipelineStage = newStage;
        }

        _compileContext._clock = opb.create<circt::seq::FromClockOp>(
            mlirBbLocation, hwModuleBodyBlock->getArgument(static_cast<size_t>(CirctModulePort::Clock)));
    }

    void CloseCirctBasicBlock()
    {
        const BasicBlock& basicBlock = *_compileContext._basicBlock;

        const mlir::Location mlirBbLocation = LocationToCirctLocation(basicBlock._location);

        circt::OpBuilder opb = circt::OpBuilder::atBlockEnd(_compileContext._hwModule.getBodyBlock());

        // Terminate all stages with StageOp or ReturnOp
        mlir::Block* prevStage = nullptr;

        for (mlir::Block& stage : _compileContext._circtPipeline.getStages())
        {
            circt::OpBuilder::InsertionGuard g(opb);

            if (prevStage)
            {
                opb.setInsertionPointToEnd(prevStage);

                circt::pipeline::StageOp stageOp = opb.create<circt::pipeline::StageOp>(
                    mlirBbLocation, &stage, llvm::ArrayRef<mlir::Value>({}), // registers
                    llvm::ArrayRef<mlir::Value>({}));                        // passthroughs
            }

            if (&stage == _compileContext._lastPipelineStage)
            {
                opb.setInsertionPointToEnd(&stage);

                // Return the stage valid bit, to ensure the entire pipeline is not optimized away
                circt::pipeline::ReturnOp rop = opb.create<circt::pipeline::ReturnOp>(
                    mlirBbLocation,
                    llvm::ArrayRef<mlir::Value>({_compileContext._circtPipeline.getStageEnableSignal(&stage)}));
            }

            prevStage = &stage;
        }

        // HW module output operation
        mlir::SmallVector<mlir::Value> outputs;

        for (const BasicBlockPortInfo& port : _compileContext._basicBlockPorts)
        {
            if (circt::hw::ModulePort::Direction::Output == port._portInfo.dir)
            {
                const mlir::Value wire = SafeLookup(_compileContext._moduleOutputWireMap, port._portInfo.name.str());

                const mlir::Value value = opb.create<circt::sv::ReadInOutOp>(mlirBbLocation, port._portInfo.type, wire);

                outputs.push_back(value);
            }
        }

        // Connect pipeline unused output to hw module output port
        // to prevent dead-code-elimination from removing the pipeline
        outputs[static_cast<size_t>(CirctModuleOutputPort::Done)] =
            _compileContext._circtPipeline.getDataOutputs().front();

        opb.create<circt::hw::OutputOp>(LocationToCirctLocation(basicBlock._location), outputs);

        // Verify there are no seq.from_clock operations inside of the pipeline
        // as this causes the pipeline to register the clock signal
        _compileContext._circtPipeline.walk(
            [](mlir::Operation* op, const mlir::WalkStage&)
            {
                if (mlir::isa<circt::seq::FromClockOp>(op))
                {
                    assert(false);
                }
            });
    }

    FileSourceWriter _writer;
    const std::string _svPackageFileName;
    const Program& _program;
    FifoNamer _fifoNamer;

    const std::string _cppFileName;
    const std::string _headerFileName;
    const std::string _memFileBase;
    const std::string _circtAsmFileName;

    size_t _resetReplicaIndex;
    size_t _inputValidReplicaIndex;
    size_t _selectTableIndex;

    bool _basicBlockHasStalls;
    bool _basicBlockHasIntraPipelineStalls;

    size_t _threadCountNameIndex;

    mutable std::set<size_t> _hardenedOutputRegisters;

    std::map<size_t, std::string> _localRegisterNameMap;

    std::map<const ExternalClassInstance*, size_t> _externClassInstanceInspectableIndices;

    // Old stall threshold generator providing 1 threshold value(pipeline stall still uses it)
    RandomStallRateThresholdGenerator _stallRateThresholdGenerator;
    // New stall threshold generator providing 5 threshold values for selection
    RandomStallRateMultiThresholdGenerator _stallRateThresholdGenerator2;

    // Non-null when core module is being built
    ModuleDeclarationHelper* _coreModule;

    // Maps HwModule name to the module
    std::map<std::string, circt::hw::HWModuleOp> _nameToHwModule;

    mlir::ModuleOp _mlirModule;

    // Set of extern module declarations that have been emitted
    std::set<std::string> _circtExternalModuleNames;

    // Maps external module name to PortNameToType
    std::map<std::string, PortNameToType> _circtExternalModules;

    struct BasicBlockPortInfo
    {
        circt::hw::PortInfo _portInfo;

        // If a given input port is accessed in stage N
        // and this value is true, then the value on the port
        // will be carried through pipeline registers to stage N.
        // If this is false, then stage N will access the input port directly.
        bool _autoPipeline;
    };

    struct CompileContext
    {
        const BasicBlock* _basicBlock;
        const Stage* _stage;
        TriggeredOpHelper* _triggeredOpHelper;
        JsonValue* _basicBlockDebugSignals;
        JsonValue* _basicBlockDebugInstances;
        JsonValue* _basicBlockConstantSignals;
        JsonValue* _basicBlockUnusedSignals;

        // Assignments associated with the current pipeline stage
        // can be placed into here.  They are batched into 1 always_comb per stage.
        BatchAssignments _batchAssignments;

        // Counts number of KanagawaLutMul instances
        size_t _lutMulCount;

        // Maps fifo index to fifo port index
        std::map<size_t, size_t> _writtenFifoMap;

        BypassStoreMap _bypassStoreMap;

        LatencyMap _latencyMap;

        circt::hw::HWModuleOp _hwModule;
        mlir::SmallVector<BasicBlockPortInfo> _basicBlockPorts;

        circt::pipeline::ScheduledPipelineOp _circtPipeline;
        mlir::Block* _lastPipelineStage;

        // Maps block to pipeline stage index
        std::map<mlir::Block*, size_t> _blockToStageIndex;

        // Appends to the current pipeline stage
        std::unique_ptr<circt::OpBuilder> _opBuilder;

        // Maps register index (wire, pipeline) to value
        std::map<size_t, mlir::Value> _regToValue;

        // Maps CIRCT port name to information about the port
        std::map<std::string, circt::hw::PortInfo> _portNameToInfo;

        // Maps input port name to pipeline argument index
        std::map<std::string, size_t> _pipelineInputPortMap;

        // Maps InOut port name to hw module argument index
        std::map<std::string, size_t> _moduleInputPortMap;

        // Maps output port name to wire assigned that will be read to produce the module output
        std::map<std::string, mlir::Value> _moduleOutputWireMap;

        // Maps port name and pipeline stage index
        // to circt::hw::WireOp which is used to sample the port on a specific pipeline stage
        std::map<std::pair<std::string, size_t>, mlir::Value> _inputReadWireMap;

        // Maps pipeline stage index to wire which holds cycle count sampled at that pipeline stage
        std::map<size_t, mlir::Value> _cycleCountWireMap;

        // Accumulates writes to fifos
        AccumulateOutputPortUpdates _fifoWrites;

        // Set of extern module declarations that have been emitted
        std::set<std::string> _circtExternalModuleNames;

        // Values which are not sent as pipeline inputs (hence not pipelind)
        // but are accessed by the pipeline
        mlir::Value _cycleCountReg;

        mlir::Value _selectedFifoIndex;

        mlir::Value _startConditionWire;

        mlir::Value _fiberIndexRegMatch;

        mlir::Value _allowNewThreadPreStartCondition;

        mlir::Value _thresholdSelWire;

        mlir::Value _clock;

        // Maps fixed delay fifo index to mapping of offset to wire
        // that the pipeline should write fixed delay fifo inputs into
        std::map<size_t, std::map<size_t, mlir::Value>> _dataPropIn;

        // Maps fixed delay fifo index to mapping of offset to wire
        // that the pipeline should read fixed delay fifo inputs from
        std::map<size_t, std::map<size_t, mlir::Value>> _dataPropOut;

        // Convert a port string name to an mlir value corresponding to that name
        // For inputs, this mlir value can be used as a regular operand
        // For outputs, only use sv.assign
        mlir::Value PortNameToValue(const std::string& name)
        {
            const circt::hw::PortInfo& portInfo = SafeLookup(_portNameToInfo, name);

            if (portInfo.dir == circt::hw::ModulePort::Direction::Output)
            {
                // Return a wire that the result value can be assigned to with sv::assign
                return SafeLookup(_moduleOutputWireMap, name);
            }

            if (_stage)
            {
                // Inside of a pipeline

                // Check to see if auto-pipelining is enabled for this port
                // If so, return the pipelined version (a read in stage N will read from a chain of flops with depth
                // =N)
                const auto pipelineIt = _pipelineInputPortMap.find(name);
                if (pipelineIt != _pipelineInputPortMap.end())
                {
                    return _circtPipeline.getEntryStage()->getArgument(pipelineIt->second);
                }
                else
                {
                    // Create a dedicatde wire for reading the input port in a specific pipeline stage (if not
                    // already created) The hw module argument cannot be directly returned here. If the read in
                    // stage N (without a wire) but not used until stage N+1, then the read can actually occur in
                    // stage N+1 because the module input is just an SSA value (there is no actual "read" operation)

                    // Separate wires are needed for each pipeline stage to prevent CSE from combining accesses
                    // across pipeline stages

                    // inner_sym is provided to prevent this wire from be canonicalized away

                    const std::pair<std::string, size_t> key(portInfo.name.str(), _stage->_atomicSequence);

                    const auto it = _inputReadWireMap.find(key);

                    if (it == _inputReadWireMap.end())
                    {
                        const mlir::Location mlirBbLocation = LocationToCirctLocation(_basicBlock->_location);

                        circt::OpBuilder& opb = OpBuilder();

                        const std::string innerSymName =
                            portInfo.name.str() + "_" + std::to_string(_stage->_atomicSequence);

                        const size_t inputPortIndex = SafeLookup(_moduleInputPortMap, portInfo.name.str());

                        mlir::Value wireOp = opb.create<circt::hw::WireOp>(
                            mlirBbLocation, _hwModule.getBodyBlock()->getArgument(inputPortIndex), portInfo.name,
                            GetFullyQualifiedInnerSymAttr(_basicBlock->GetObjectPath(), innerSymName));

                        SafeInsert(_inputReadWireMap, key, wireOp);

                        return wireOp;
                    }
                    else
                    {
                        return it->second;
                    }
                }
            }
            else
            {
                return _hwModule.getBodyBlock()->getArgument(SafeLookup(_moduleInputPortMap, name));
            }
        }

        mlir::Value GetCurrentStageEnableSignal()
        {
            return _circtPipeline.getStageEnableSignal(_stage->_atomicSequence);
        }

        circt::OpBuilder& OpBuilder() { return *(_opBuilder.get()); }

        void SetCurrentStage(const Stage* const stage)
        {
            // Flush any assignments to ensure those assignments occur in the correct stage
            assert(_basicBlock);
            _batchAssignments.Flush(OpBuilder(), LocationToCirctLocation(_basicBlock->_location));

            _stage = stage;

            if (!_opBuilder)
            {
                _opBuilder = std::make_unique<circt::OpBuilder>(g_compiler->GetMlirContext());
            }

            if (stage)
            {
                // Create an OpBuilder pointing to the end of the new pipeline stage
                mlir::Block* const circtStage = _circtPipeline.getStage(stage->_atomicSequence);

                _opBuilder->setInsertionPointToEnd(circtStage);
            }
        }

        // Get the last Stage with a given sequence number
        const Stage* GetLastStage(const size_t atomicSequence)
        {
            for (auto it = _basicBlock->_stages.rbegin(); it != _basicBlock->_stages.rend(); ++it)
            {
                const Stage& stage = *it;

                if (stage._atomicSequence == atomicSequence)
                {
                    return &stage;
                }
            }

            throw std::runtime_error("Failed to locate pipeline stage");
        }
    };

    // RAII class to restore the current pipeline stage in CompileContext
    class RestoreContextStage
    {
      public:
        RestoreContextStage(CompileContext& context) : _context(context), _stage(context._stage) {}

        ~RestoreContextStage() { _context.SetCurrentStage(_stage); }

      private:
        CompileContext& _context;
        const Stage* _stage;
    };

    CompileContext _compileContext;

    JsonWriter _rtlMap;

    JsonValue _jsonDebugSignals;
    JsonValue _jsonConstantSignals;
    JsonValue _jsonGlobalVariables;
    JsonValue _jsonFifos;
    JsonValue _jsonLoopGenerators;

    using ContainerAndInstance = std::pair<circt::kanagawa::ContainerOp, circt::kanagawa::ContainerInstanceOp>;

    std::map<ObjectPath, ContainerAndInstance> _pathToContainerInstance;

    // Top-level symbol table which contains all other IR
    circt::kanagawa::DesignOp _designOp;

    // Batches all assignments in the top-level module
    // into a single always_comb block
    BatchAssignments _globalBatchAssignments;

    circt::kanagawa::ContainerOp PathToContainer(const ObjectPath& path)
    {
        assert(_coreModule);

        // The container in the core module
        // is the container for the root path
        if (path.empty())
        {
            return _coreModule->Container();
        }

        if (_pathToContainerInstance.find(path) == _pathToContainerInstance.end())
        {
            assert(!path.empty());

            // Recurse to find the parent instance and container
            const ObjectPath parentPath = SubPath(path, 0, path.size() - 1);

            circt::kanagawa::ContainerOp parentContainer = PathToContainer(parentPath);

            // Create a new container representing this full path
            const mlir::StringAttr containerNameAttr = StringToStringAttr(GenericContainerName(path));

            circt::kanagawa::DesignOp designOp = GetDesignOp(_coreModule->MlirModule());

            circt::OpBuilder opb(designOp.getBodyRegion());

            // The container has TopLevel = = false to prepend the design name to the container name
            // to avoid collisions between RTL modules generated by different export classes
            circt::kanagawa::ContainerOp container = opb.create<circt::kanagawa::ContainerOp>(
                GetUnknownLocation(), circt::hw::InnerSymAttr::get(containerNameAttr), false);

            // Verification requires all containers to contain a `this` operation
            opb.setInsertionPointToEnd(container.getBodyBlock());

            // Add an instance of the new container to the parent
            opb.setInsertionPointToEnd(parentContainer.getBodyBlock());

            circt::kanagawa::ContainerInstanceOp instance = opb.create<circt::kanagawa::ContainerInstanceOp>(
                GetUnknownLocation(), circt::hw::InnerSymAttr::get(StringToStringAttr(path.back())),
                circt::hw::InnerRefAttr::get(StringToStringAttr(GetCirctDesignName()), containerNameAttr));

            SafeInsert(_pathToContainerInstance, path, ContainerAndInstance(container, instance));
        }

        return SafeLookup(_pathToContainerInstance, path).first;
    }

    // Returns the stage enable bit, optionally ANDed with a predicate bit from the operation
    mlir::Value GetPredicatedStageEnable(const Operation& op, const size_t predicateOperandIndex,
                                         const bool isPredicated, const bool reversePredicateMeaning = false,
                                         const SourceOpToStringMode srcOpToStrMode = SourceOpToStringMode::Default,
                                         const bool preStartCondition = false)
    {
        circt::OpBuilder& opb = _compileContext.OpBuilder();

        const mlir::Location opLocation = OperationToCirctLocation(op, _program);

        assert(Implies(preStartCondition, _compileContext._stage->_atomicSequence == 0));
        const mlir::Value enable = preStartCondition ? _compileContext._allowNewThreadPreStartCondition
                                                     : _compileContext.GetCurrentStageEnableSignal();

        mlir::Value result;

        if (isPredicated)
        {
            mlir::Value predicate = SourceOperandToMlirValue(op, predicateOperandIndex, 1, srcOpToStrMode);

            if (reversePredicateMeaning)
            {
                predicate = circt::comb::createOrFoldNot(opLocation, predicate, opb, TwoState);
            }

            result = opb.create<circt::comb::AndOp>(opLocation, enable, predicate, TwoState);
        }
        else
        {
            result = enable;
        }

        return result;
    }

    // Incrementally adds names to a set of unique names
    // If any new string is a duplicate, then throws an exception
    // Used to ensure that conflicting names in the generated code
    // Uniqueness is ensured by preprending containing object names to shared variables
    std::string ValidateUniqueName(const std::string& nameIn)
    {
        const auto insertResult = _uniqueNames.insert(nameIn);

        if (!insertResult.second)
        {
            std::ostringstream err;
            err << "Name in generated code is not unique: " << nameIn;
            throw RuntimeErrorWithTrace(err.str());
        }

        return nameIn;
    }

    std::set<std::string> _uniqueNames;

    // Returns a string that can be used as an identifier for a given register
    // Used to give modules in the generated code stable names
    // so that they can be referenced by test benches
    std::string UniqueRegisterName(const size_t registerIndex)
    {
        const RegisterDescription& regDesc = _program._registerTable.at(registerIndex);

        // Only globals/memories can have stable names
        assert((RegisterType::Global == regDesc._type) || (RegisterType::Memory == regDesc._type));

        // And underscore is prepended to each name to ensure
        // that the string is not a SystemVerilog keyword
        std::ostringstream str;
        str << "_" << regDesc._name;

        // If the register name might not be unique (static locals)
        // then make it unique
        if (!regDesc._uniqueName)
        {
            str << "_" << registerIndex;
        }

        return str.str();
    }

    std::string GenericContainerName(const ObjectPath& path)
    {
        return path.empty() ? _coreModule->Name() : FixupString(SerializePath(path));
    }
};

void CompileVerilog(const char* const svFileName, const char* const svPackageFileName, const char* const cppFileName,
                    const char* const headerFileName, const char* const tclFileName,
                    const char* const hwConfigMkFileName, const char* const memFileBase,
                    const char* const rtlMapFileName, const char* const circtAsmFileName, const Program& program)
{
    VerilogCompiler compiler(svFileName, svPackageFileName, cppFileName, headerFileName, memFileBase, rtlMapFileName,
                             circtAsmFileName, program);

    compiler.Compile();
}

ModuleInstanceHelper::ModuleInstanceHelper(VerilogCompiler& compiler, const mlir::Location& location)
    : _verilogCompiler(compiler), _location(location)
{
}

void ModuleInstanceHelper::SetModuleName(const std::string& name) { _moduleName = name; }

void ModuleInstanceHelper::SetInstanceName(const std::string& name) { _instanceName = name; }

void ModuleInstanceHelper::AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction,
                                   const mlir::Type& type, const std::string& value)
{
    AddPort(name, direction, type);

    // An empty string means the output is unused
    if (!value.empty())
    {
        SafeInsert(_portNameToStringValue, name, value);
    }
}

void ModuleInstanceHelper::AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction,
                                   const mlir::Type& type, const mlir::Value& value)
{
    AddPort(name, direction, type);

    SafeInsert(_portNameToMlirValue, name, value);
}

void ModuleInstanceHelper::AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction,
                                   const mlir::Type& type, const mlir::Value& pathToContainer,
                                   const circt::hw::InnerSymAttr& containerPortSymbol)
{
    AddPort(name, direction, type);

    SafeInsert(_portNameToPathAndPort, name, PathAndPortName{pathToContainer, containerPortSymbol});
}

void ModuleInstanceHelper::AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction,
                                   const mlir::Value& value)
{
    AddPort(name, direction, value.getType(), value);
}

void ModuleInstanceHelper::AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction,
                                   const mlir::Type& type)
{
    SafeInsert(_portNameToIndex, name, _ports.size());

    const circt::hw::PortInfo portInfo = {StringToStringAttr(name), type, direction};

    _ports.push_back(portInfo);

    SafeInsert(_portNameToType, name, type);
}

void ModuleInstanceHelper::AddStringParameter(const std::string& name, const std::string& value)
{
    AddParameter(name, mlir::NoneType::get(g_compiler->GetMlirContext()), StringToStringAttr(value));
}

void ModuleInstanceHelper::AddU64Parameter(const std::string& name, const uint64_t value)
{
    // CIRCT requires ports with parameterized widths to
    // be associated with a parameter with width exactly equal to 32
    // Convert here because most compiler code operates on size_t
    assert(value <= std::numeric_limits<uint32_t>::max());

    const mlir::Type type =
        mlir::IntegerType::get(g_compiler->GetMlirContext(), 32, mlir::IntegerType::SignednessSemantics::Unsigned);

    AddParameter(name, type, mlir::IntegerAttr::get(type, value));

    SafeInsert(_parameterNameToU64Value, name, value);
}

void ModuleInstanceHelper::AddParameter(const std::string& name, const mlir::Type type, const mlir::Attribute value)
{
    _parameterDeclarations.push_back(circt::hw::ParamDeclAttr::get(name, type));

    assert(llvm::isa<mlir::TypedAttr>(value));

    _parameterDefinitions.push_back(
        circt::hw::ParamDeclAttr::get(StringToStringAttr(name), llvm::cast<mlir::TypedAttr>(value)));

    SafeInsert(_parameterNameToType, name, type);
}

void ModuleInstanceHelper::AddBoolParameter(const std::string& name, const bool value)
{
    AddU64Parameter(name, value ? 1 : 0);
}

// When adding ports with a parameterized width, use this type
mlir::Type ModuleInstanceHelper::GetParameterizedIntegerType(const std::string& parameterName)
{
    return circt::hw::IntType::get(circt::hw::ParamDeclRefAttr::get(StringToStringAttr(parameterName),
                                                                    SafeLookup(_parameterNameToType, parameterName)));
}

// Convert an attribute that may be a circt::hw::ParamDeclRefAttr
// into an attribute that is not a circt::hw::ParamDeclRefAttr
mlir::Attribute ModuleInstanceHelper::ConcreteAttr(const mlir::Attribute srcAttribute)
{
    if (llvm::isa<circt::hw::ParamDeclRefAttr>(srcAttribute))
    {
        const circt::hw::ParamDeclRefAttr paramRefAttr = llvm::cast<circt::hw::ParamDeclRefAttr>(srcAttribute);

        const std::string paramName = paramRefAttr.getName().str();

        const size_t paramValue = SafeLookup(_parameterNameToU64Value, paramName);

        return mlir::IntegerAttr::get(GetIntegerType(64), paramValue);
    }
    else
    {
        return srcAttribute;
    }
}

// Convert a type that may contain ParamDeclRefAttr
// into a type without any ParamDeclRefAttr, by converting all
// ParamDeclRefAttr into concrete integer types
mlir::Type ModuleInstanceHelper::ConcreteType(const mlir::Type srcType)
{
    if (llvm::isa<circt::hw::ArrayType>(srcType))
    {
        // Convert element type to a concrete type
        const circt::hw::ArrayType arrayType = llvm::cast<circt::hw::ArrayType>(srcType);

        return circt::hw::ArrayType::get(g_compiler->GetMlirContext(), ConcreteType(arrayType.getElementType()),
                                         ConcreteAttr(arrayType.getSizeAttr()));
    }
    else if (llvm::isa<circt::hw::IntType>(srcType))
    {
        // Dereference the parameter to a concrete type
        const circt::hw::IntType intType = llvm::cast<circt::hw::IntType>(srcType);

        assert(llvm::isa<circt::hw::ParamDeclRefAttr>(intType.getWidth()));

        const circt::hw::ParamDeclRefAttr parameterRefAttr =
            llvm::cast<circt::hw::ParamDeclRefAttr>(intType.getWidth());

        const std::string paramName = parameterRefAttr.getName().str();

        const size_t concreteWidth = SafeLookup(_parameterNameToU64Value, paramName);

        return GetIntegerType(concreteWidth);
    }
    else
    {
        return srcType;
    }
}

void ModuleInstanceHelper::Generate(circt::OpBuilder* const opbIn)
{
    assert(!_moduleName.empty());
    assert(!_instanceName.empty());

    // _verilogCompiler._compileContext._stage should only be non-null
    // when compiling a basic block, in which case _mlirModule should be non-null
    assert(Implies(_verilogCompiler._compileContext._stage, _verilogCompiler._mlirModule));

    // Generate a circt module instance if the compiler is currently compiling operations inside
    // of the top-level MlirModule. Otherwise, just emit raw strings.
    const bool generateCirctModuleInstance =
        (_verilogCompiler._compileContext._stage || _verilogCompiler._coreModule || opbIn);

    if (generateCirctModuleInstance)
    {
        // Choose between a new op builder pointing to HwModuleOp for the core
        // or the op builder for the current pipeline stage
        circt::OpBuilder* opbPtr = nullptr;

        circt::OpBuilder coreOpb(g_compiler->GetMlirContext());

        if (_verilogCompiler._coreModule)
        {
            // Ensure verbatim register/net declarations appear before instances that use them
            _verilogCompiler._coreModule->FlushVerbatimStrings();

            coreOpb = circt::OpBuilder::atBlockEnd(_verilogCompiler._coreModule->GetBodyBlock());
        }

        circt::OpBuilder& opb =
            opbIn ? *opbIn
                  : (_verilogCompiler._compileContext._stage ? _verilogCompiler._compileContext.OpBuilder() : coreOpb);

        // Declare the module as external if the name is not in the set
        // of names of HWModuleOps that have been emitted
        const auto moduleIt = _verilogCompiler._nameToHwModule.find(_moduleName);
        const bool isExternalClassInstance = _verilogCompiler._nameToHwModule.end() == moduleIt;

        if (isExternalClassInstance)
        {
            // Add a definition of the external module to the outer mlir module
            // Only do this once per module
            const auto it = _verilogCompiler._circtExternalModules.find(_moduleName);

            if (it == _verilogCompiler._circtExternalModules.end())
            {
                circt::OpBuilder::InsertionGuard g(opb);

                opb.setInsertionPointToStart(&_verilogCompiler._mlirModule.getBodyRegion().front());

                opb.create<circt::hw::HWModuleExternOp>(_location, StringToStringAttr(_moduleName), _ports,
                                                        mlir::StringRef{}, opb.getArrayAttr(_parameterDeclarations));

                SafeInsert(_verilogCompiler._circtExternalModules, _moduleName, _portNameToType);
            }
            else
            {
                // Verify that port types match
                // If they do not, it is an indicating that a port type is parameterized
                assert(_portNameToType == it->second);
            }
        }
        else
        {
            // Sort _ports match the order that ports were declared in the module
            circt::hw::HWModuleOp hwModule = moduleIt->second;

            mlir::SmallVector<circt::hw::PortInfo> sortedPorts;

            circt::hw::ModuleType moduleType = hwModule.getHWModuleType();

            for (size_t i = 0; i < moduleType.getNumPorts(); i++)
            {
                const std::string expectedName = moduleType.getPortName(i).str();

                const size_t portIndex = SafeLookup(_portNameToIndex, expectedName);

                assert(portIndex < _ports.size());

                const circt::hw::PortInfo& port = _ports[portIndex];

                assert(port.name.str() == expectedName);

                sortedPorts.push_back(port);
            }

            _ports = sortedPorts;

            _portNameToIndex.clear(); // now out of date
        }

        // Instantiate the module
        mlir::SmallVector<mlir::Type> resultTypes;
        mlir::SmallVector<mlir::Value> inputs;
        mlir::SmallVector<mlir::Attribute> argNames;
        mlir::SmallVector<mlir::Attribute> resultNames;
        mlir::StringAttr innerSym;

        size_t inputPortIndex = 0;

        for (const circt::hw::PortInfo& port : _ports)
        {
            if (circt::hw::ModulePort::Direction::Input == port.dir)
            {
                argNames.push_back(port.name);

                const auto it = _portNameToMlirValue.find(port.name.str());
                const auto it2 = _portNameToPathAndPort.find(port.name.str());

                if (it != _portNameToMlirValue.end())
                {
                    inputs.push_back(it->second);
                }
                else if (it2 != _portNameToPathAndPort.end())
                {
                    const PathAndPortName& papn = it2->second;

                    // Value comes from a container port
                    // Locate the port
                    const mlir::Value value =
                        ReadContainerPort(opb, _location, papn.first, papn.second.getSymName(), port.type);

                    inputs.push_back(value);
                }
                else
                {
                    // Construct mlir value from verbatim string
                    const std::string& value = SafeLookup(_portNameToStringValue, port.name.str());

                    const mlir::Type portType = ConcreteType(port.type);

                    // For clock ports, the verbatim op has a i1 type
                    // and then an explicit cast is added
                    const bool isClockType = llvm::isa<circt::seq::ClockType>(portType);

                    const mlir::Type verbatimType = isClockType ? GetI1Type() : portType;

                    // ConcreteType is used to convert the type of the input value
                    // to one without parameter references
                    circt::sv::VerbatimExprOp verbatimOp =
                        opb.create<circt::sv::VerbatimExprOp>(_location, verbatimType, value);

                    // Use a name hint to ensure the wire generated from this verbatim op
                    // doesn't conflict with a name of a variable declared in another verbatim op
                    verbatimOp->setAttr("sv.namehint",
                                        StringToStringAttr(_instanceName + "_" + port.name.str() + "__circt"));

                    const mlir::Value input =
                        isClockType ? opb.create<circt::seq::ToClockOp>(_location, GetClockType(),
                                                                        static_cast<mlir::Value>(verbatimOp))
                                    : static_cast<mlir::Value>(verbatimOp);

                    inputs.push_back(input);
                }

                inputPortIndex++;
            }
            else
            {
                assert(circt::hw::ModulePort::Direction::Output == port.dir);
                resultNames.push_back(port.name);
                resultTypes.push_back(ConcreteType(port.type));
            }
        }

        circt::hw::InstanceOp instanceOp = opb.create<circt::hw::InstanceOp>(
            _location, resultTypes, mlir::StringRef(_instanceName), mlir::StringRef(_moduleName), inputs,
            opb.getArrayAttr(argNames), opb.getArrayAttr(resultNames), opb.getArrayAttr(_parameterDefinitions),
            circt::hw::InnerSymAttr());

        // Connect module outputs via verbatim ops
        auto results = instanceOp->getResults();

        size_t resultIndex = 0;

        // Place all assignments in a single always_comb block
        for (const circt::hw::PortInfo& port : _ports)
        {
            if (circt::hw::ModulePort::Direction::Output == port.dir)
            {
                const mlir::Value result = results[resultIndex];

                const auto it = _portNameToStringValue.find(port.name.str());
                const auto it2 = _portNameToPathAndPort.find(port.name.str());

                if (_portNameToStringValue.end() != it)
                {
                    // Assigning an output to an empty string does not make sense
                    assert(!it->second.empty());

                    _verilogCompiler._globalBatchAssignments.AppendVerbatimDst(_location, it->second, result);
                }
                else if (it2 != _portNameToPathAndPort.end())
                {
                    const PathAndPortName& papn = it2->second;

                    // Value should be written to a container port
                    // Locate the port
                    WriteContainerPort(opb, _location, papn.first, papn.second.getSymName(), port.type, result);
                }
                else
                {
                    // Save for later retrieval
                    SafeInsert(_portNameToMlirValue, port.name.str(), result);
                }

                resultIndex++;
            }
        }
    }
}

mlir::Value ModuleInstanceHelper::GetPortValue(const std::string& name)
{
    return SafeLookup(_portNameToMlirValue, name);
}
