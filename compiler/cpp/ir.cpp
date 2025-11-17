// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

// 64-bit integer -1
static_assert(c_stringReferenceBits == 64, "c_stringReferenceNegOne width incorrect");
static const Literal c_stringReferenceNegOne = {std::numeric_limits<uint64_t>::max(), c_stringReferenceBits};

RegisterDescription::~RegisterDescription()
{
    if (!_name.empty())
    {
        // To catch cases where multiple pointers are allocated when they
        // should not be
        AssertPointers();
    }
}

const Scope g_dspScope = {"@hardware@dsp"};
const Scope g_float32OperatorScope = {"@numeric@float32@operator"};

Location SourceVariableToLocation(const SourceVariable& sv)
{
    Location result = {};

    result._beginLine = sv._declaredLocation._lineNumber;
    result._endLine = sv._declaredLocation._lineNumber;
    result._fileIndex = sv._declaredLocation._fileIndex;
    result._valid = true;

    return result;
}

void Operation::InsertLocation(const Location& location) { _locations.insert(LocationToFileAndLineNumber(location)); }

// Returns true if ContainsMemberRecursive can early searching a subtree because no match will be found
// A match can only be be possible if searchMemberName starts with the characters in parentObjectName `P`
// because the search combines parentObjectName with a possible member `M` to form the string: PM
// and then compares (equality) that against searchMemberName.
bool ContainsMemberRecursiveEarlyOut(const std::string& searchMemberName, const std::string& parentObjectName)
{
    if (searchMemberName.length() < parentObjectName.length())
    {
        return true;
    }

    for (size_t i = 0; i < parentObjectName.length(); i++)
    {
        if (searchMemberName[i] != parentObjectName[i])
        {
            return true;
        }
    }

    return false;
}

static void GenerateInlineCall(IRContext& context, const FunctionDesc& functionDesc,
                               const FunctionNode* const functionNode, const std::string& calledObjectName,
                               const FunctionNode::Instance& functionInstance,
                               const std::vector<KnownValue>& parameterKnownValues,
                               const FunctionInstance& calleeInstance, const FunctionInstance& callerInstance,
                               const Location& callerLocation)
{
    // Record the callee instance
    // This is used if the callee makes a function call to another function
    PushPopFunctionInstance pushPopFunctionInstance(context, calleeInstance);

    // Emit the statements for the body of the function
    functionNode->GenerateInlineIR(context, calledObjectName, parameterKnownValues);
}

// Decomposes a AllocatedRegister into a linear list of registers
typedef std::pair<AccessedRegister, const Type*> RegAndType;

// Exports a type and all contained type
void ExportTypeHelper(Program& program, const Type* const type, const ExportTypeBehavior behavior)
{
    // Add the type, and all contained types
    const auto callback = [&](const Type* const containedType)
    {
        if (containedType->ShouldExport())
        {
            program.AddExportedType(containedType, behavior);
        }
    };

    // Reference types are not exportable (do not traverse into referenced classes)
    type->VisitTypes(callback, VisitTypesBehavior::SkipReferences);
}

void ExportTypeHelper(Program& program, const ParseTreeNode* const node, const Type* const type,
                      const ExportTypeBehavior behavior)
{
    if (const TypeNode* typeNode = dynamic_cast<const TypeNode*>(node))
    {
        if (auto name = typeNode->GetTypedefName())
        {
            program.AddExportedTypedef(*name, type);
        }
    }

    ExportTypeHelper(program, type, behavior);
}

std::vector<RegAndType> GetFlatRegisters(const AllocatedRegister* const allocatedRegister)
{
    std::vector<RegAndType> result;

    const auto callback = [&](const AccessedRegister reg, const Type* const type)
    {
        const RegAndType rat(reg, type);

        result.push_back(rat);
    };

    allocatedRegister->VisitRegisters(callback);

    return result;
}

void ClearAllocatedRegister(IRContext& context, const AllocatedRegister* const registers, const Location& location)
{
    const auto clearCallback = [&](const AccessedRegister reg, const Type* const type)
    {
        Operation op = {};

        op._opcode = Opcode::Clear;

        op.InsertLocation(location);

        op._dst.push_back(reg);

        context._basicBlock->_operations.push_back(op);
    };

    registers->VisitRegisters(clearCallback);
}

// Extracts the string literal value of a ParseTreeNode
std::string StringFromNode(IRContext& context, const Location& location, const ParseTreeNode* const node)
{
    std::string result;

    const KnownValue knownArgValue = node->TryGetKnownValue(context, node->GetType());

    const LiteralStringNode* const literalStringNode = dynamic_cast<const LiteralStringNode*>(node);

    if (knownArgValue._type == KnownValueType::String)
    {
        result = knownArgValue._stringVal;
    }
    else if (literalStringNode)
    {
        result = literalStringNode->GetValue();
    }
    else
    {
        g_compiler->ErrorStream(location, CompileError::InvalidType)
            << "Unknown string value (should you mark it const?)";
    }

    return result;
}

// Helper class to create a DebugView operation
class DebugViewBuilder
{
  public:
    DebugViewBuilder(IRContext& context, const std::string& label, const Location& location)
        : _context(context), _location(location)
    {
        Operation nop = {};
        _op = nop;

        _op._opcode = Opcode::DebugView;

        _op.InsertLocation(location);

        _op._flags._debugView = g_compiler->Create<DebugView>();

        _op._flags._debugView->_label = label;
    }

    void AppendArgument(const AllocatedRegister* const allocatedRegister, const std::string& nameIn)
    {
        // Ensure the parameter name will only have legal characters for the SV backend to deal with
        const std::string name = FixupString(nameIn);

        // Ensure parameter names are unique (so that the backend doesn't have to handle this)
        const auto insertResult = _argumentNames.insert(name);
        if (!insertResult.second)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidDebugView)
                << "Duplicate debug view parameter name: " << name;
        }

        // DebugView is not useful to non-Verilog backends
        // Avoid exporting types to non-Verilog backends
        if (!g_compiler->IsCompilingToVerilog())
        {
            return;
        }

        DebugViewArgument debugViewArgument = {};

        const Type* const argType = allocatedRegister->GetType();

        size_t numExportedTypes = 0;

        debugViewArgument._name = name;
        debugViewArgument._typeName = argType->GetVerilogName();
        debugViewArgument._beginSrcOperand =
            _op._src.size(); // index of first SourceOperand corresponding to this argument
        debugViewArgument._isEnum = (nullptr != dynamic_cast<const EnumType*>(argType));

        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        {
            if (type->ShouldExport())
            {
                numExportedTypes++;

                _op._src.push_back(reg);

                debugViewArgument._srcOperandWidths.push_back(type->GetBitWidth());
            }
        };

        allocatedRegister->VisitRegisters(callback);

        if (numExportedTypes > 0)
        {
            // Ensure a verilog version of the type is generated
            ExportTypeHelper(*_context._program, argType, ExportTypeBehavior::VerilogOnly);

            _op._flags._debugView->_arguments.push_back(debugViewArgument);
        }
    }

    // Inserts the DebugView operation before the provided iterator
    void InsertOp(OperationList& ops, OperationList::iterator insertIt)
    {
        // DebugView is not useful to C++ callers
        // Avoid exporting types to C++
        if (g_compiler->IsCompilingToVerilog())
        {
            ops.insert(insertIt, _op);
        }
    }

  private:
    std::set<std::string> _argumentNames;
    Operation _op;
    IRContext& _context;
    Location _location;
};

struct RegisterTableEntry
{
    RegisterType _type;
    const char* _name;
    bool _allowedInKillMoves;
    bool _canBeRenamed;
    bool _isLocal;
    bool _requiresPipelineStage;
    bool _reorderBarrier;
};

const RegisterTableEntry g_registerTable[] = {
    // _type,                      _name,           _allowedInKillMoves,    _canBeRenamed,  _isLocal,
    // _requiresPipelineStage,  _reorderBarrier
    {RegisterType::Global, "global", false, false, false, false, true},
    {RegisterType::Local, "local", true, true, true, false, false},
    {RegisterType::Pipeline, "pipeline", false, false, true, false, false},
    {RegisterType::Fifo, "fifo", false, false, false, true, false},
    {RegisterType::Wire, "wire", false, false, true, false, false},
    {RegisterType::Memory, "memory", false, false, false, true, true},
    {RegisterType::GlobalView, "globalview", false, false, false, false, false},
    {RegisterType::BitBucket, "bitbucket", false, false, false, false, false}};

const RegisterTableEntry& GetRegisterTableEntry(const RegisterType registerType)
{
    const size_t index = static_cast<size_t>(registerType);

    assert(index < ARRAY_SIZE(g_registerTable));

    const RegisterTableEntry& result = g_registerTable[index];

    assert(result._type == registerType);

    return result;
}

// Returns true if a register is allowed to participate in the KillMoves optimization
// Some register (like DspOutput and MemoryOutput) are there to ensure proper pipelining
// and cannot be optimized out
bool IsRegisterTypeAllowedInKillMoves(const RegisterType registerType)
{
    return GetRegisterTableEntry(registerType)._allowedInKillMoves;
}

const char* GetRegisterTypeName(const RegisterType registerType) { return GetRegisterTableEntry(registerType)._name; }

// Returns true if the register type supports renaming.
// Only local registers do.
bool CanRegisterTypeBeRenamed(const RegisterType registerType)
{
    return GetRegisterTableEntry(registerType)._canBeRenamed;
}

// Returns true if the register type is local to a basic block
bool IsLocalRegisterType(const RegisterType registerType) { return GetRegisterTableEntry(registerType)._isLocal; }

// Returns true if the register type requires a pipeline stage (for example DSP input/output)
bool RegisterTypeRequiresPipelineStage(const RegisterType registerType)
{
    return GetRegisterTableEntry(registerType)._requiresPipelineStage;
}

// Returns true if writes to the register type should not be reordered with respect to other writes that disallow
// reordering
bool RegisterTypeDisallowsReordering(const RegisterType registerType)
{
    return GetRegisterTableEntry(registerType)._reorderBarrier;
}

const FunctionNode* GetFunctionByName(const ClassType* const classType, const Scope& scope, const std::string& name,
                                      const Location& location)
{
    const FunctionNode* functionNode = nullptr;

    const std::string flattenedName = FlattenScopeAndAppendFunctionName(classType, scope, name);

    functionNode = g_compiler->TryLookupFunctionByName(classType, flattenedName);

    if (!functionNode && (classType == g_compiler->GetGlobalClassType()) && scope.empty())
    {
        // See if the function represents a callback of the export class being compiled
        functionNode = g_compiler->TryGetExportClassCallback(name);
    }

    if (!functionNode)
    {
        g_compiler->ErrorStream(location, CompileError::UnknownFunction)
            << "Function: " << flattenedName << " not found";
        throw std::runtime_error("Function not found");
    }

    return functionNode;
}

const size_t c_invalidAccessedRegisterIndex = std::numeric_limits<size_t>::max();

bool IsLeafType(const Type* const type)
{
    const LeafType* const leafType = dynamic_cast<const LeafType*>(type);

    return leafType != nullptr;
}

bool IsSignedLeafType(const Type* const type)
{
    const LeafType* const leafType = dynamic_cast<const LeafType*>(type);

    return (leafType && (leafType->_baseType == BaseType::Int));
}

bool IsUnsignedLeafType(const Type* const type)
{
    const LeafType* const leafType = dynamic_cast<const LeafType*>(type);

    return (leafType && (leafType->_baseType == BaseType::Uint));
}

// Sets _signExtendSourceMask for a given source type
// This ensures that signed integers are sign-extended
void SignExtend(Operation& op, const Type* const sourceType, const size_t sourceIndex)
{
    if (IsSignedLeafType(sourceType))
    {
        // 64-bit mask
        if (sourceIndex >= 64)
        {
            throw RuntimeErrorWithTrace("Too many source operands");
        }

        op._signExtendSourceMask |= (1ull << sourceIndex);
    }
}

void Move(IRContext& context, const AllocatedRegister* const dest, const AllocatedRegister* const source,
          const Opcode opcode, const Location& location)
{
    // Add operations to the basic block which move the value
    const auto callback = [&](const AccessedRegister lhs, const Type* const lhsType, const AccessedRegister rhs,
                              const Type* const rhsType)
    {
        Operation op = {};

        op._opcode = opcode;

        op._src.push_back(rhs);
        op._dst.push_back(lhs);
        op.InsertLocation(location);

        SignExtend(op, rhsType, 0);

        context._basicBlock->_operations.push_back(op);
    };

    dest->VisitRegisters(source, callback);
}

size_t AllocateRegister(Program* const program, const size_t bitWidth, const RegisterType type, const std::string& name,
                        const ObjectPath& containerPath, const SourceVariable& source)
{
    // To ensure generated code is readable
    // assert that all registers have names
    assert(!name.empty());

    const size_t index = program->_registerTable.size();

    // Zero-initialize
    RegisterDescription registerDescription = {};

    registerDescription._width = bitWidth;
    registerDescription._type = type;
    registerDescription._name = name;

    // Source variable information
    registerDescription._sourceVariable._name = source._name;
    registerDescription._sourceVariable._scope = source._scope;
    registerDescription._sourceVariable._dataType = source._dataType;
    registerDescription._sourceVariable._bitSize = bitWidth;
    registerDescription._sourceVariable._declaredLocation = source._declaredLocation;
    registerDescription._sourceVariable._container = source._container;

    // Allocate internal pointers based on register type
    registerDescription.AllocatePointer();

    // Store container path
    if (RegisterType::Global == type)
    {
        assert(!containerPath.empty());
        registerDescription.Global()._containerInstancePath = containerPath;
        for (std::string& s : registerDescription.Global()._containerInstancePath)
        {
            s = FixupStringCirct(s);
        }
    }
    else if (RegisterType::Memory == type)
    {
        assert(!containerPath.empty());
        registerDescription.Memory()._containerInstancePath = containerPath;
        for (std::string& s : registerDescription.Memory()._containerInstancePath)
        {
            s = FixupStringCirct(s);
        }
    }

    program->_registerTable.push_back(std::move(registerDescription));

    return index;
}

// Allocates a register that has the same properties as an existing register
size_t AllocateDuplicateRegister(Program* const program, const size_t otherRegisterIndex)
{
    assert(otherRegisterIndex < program->_registerTable.size());

    const RegisterDescription registerDescription = program->_registerTable[otherRegisterIndex];

    return AllocateRegister(program, registerDescription);
}

// Inserts MovLatencyPlaceholder operations to match latency of externally-defined blocks (like dsps)
const AllocatedRegister* AddDelayStages(IRContext& context, const AllocatedRegister* const src,
                                        const size_t extraStages)
{
    const AllocatedRegister* result = src;

    if (extraStages > 0)
    {
        // "extraStages" stages are added where the value is just move through wires
        // Then a final stage is added where the value is placed into a register (local)
        // The final stage is needed to prevent optimizations from operating on a src wire
        for (size_t i = 0; i <= extraStages; ++i)
        {
            const bool isLastStage = (i == extraStages);

            const AllocatedRegister* const tempReg = src->GetType()->AllocateRegisters(
                *context._program, isLastStage ? RegisterType::Local : RegisterType::Wire, "LatencyReg");

            const auto callback = [&](const AccessedRegister lhs, const Type* const lhsType, const AccessedRegister rhs,
                                      const Type* const rhsType)
            {
                Operation op = {};

                op._opcode = Opcode::MovLatencyPlaceholder;

                op._src.push_back(lhs);
                op._dst.push_back(rhs);

                context._basicBlock->_operations.push_back(op);
            };

            result->VisitRegisters(tempReg, callback);

            result = tempReg;
        }
    }

    return result;
}

size_t AllocateRegister(Program* const program, const RegisterDescription& regDesc)
{
    // Assert that caller has allocated any required pointers
    regDesc.AssertPointers();

    const size_t index = program->_registerTable.size();

    program->_registerTable.push_back(std::move(regDesc));

    return index;
}

size_t AllocateRegister(Program& program, const size_t bitWidth, const RegisterType type, const std::string& name,
                        const ObjectPath& containerPath = {}, const SourceVariable& source = {})
{
    return AllocateRegister(&program, bitWidth, type, name, containerPath, source);
}

AllocatedRegister::AllocatedRegister(const RegisterType registerType, const Type* const type)
    : _registerType(registerType), _type(type)
{
    if (g_compiler->_contextPtr)
    {
        // Record the number of conditions currently on the IR context stack
        _conditionStackDepth = g_compiler->_contextPtr->GetConditionStackDepth();
    }
}

void AllocatedRegister::SetConditionStackDepth(const size_t depth)
{
    assert(RegisterType::Local == _registerType);

    // For parameters of inline functions
    // this will be called many times, with different depths
    _conditionStackDepth = depth;
}

size_t AllocatedRegister::GetConditionStackDepth() const
{
    assert(RegisterType::Local == _registerType);
    assert(_conditionStackDepth);

    return *_conditionStackDepth;
}

void AppendFormatStringEntry(
    Operation& op, const FormatStringType type, const SourceOperand& srcOp, const size_t alignment = 0,
    const size_t precision = 0,
    const ParseTreeFormatSpecifier specifier = ParseTreeFormatSpecifier::ParseTreeFormatSpecifierNone)
{
    FormatStringEntry fse = {};

    fse._type = type;
    fse._operandIndex = op._src.size();
    fse._alignment = alignment;
    fse._precision = precision;
    fse._specifier = specifier;
    op._src.push_back(srcOp);

    op._flags._formatString._entries->push_back(fse);
}

void InterpolateRegister(IRContext& context, Operation& op, const size_t alignment, const size_t precision,
                         const ParseTreeFormatSpecifier format, const AccessedRegister reg, const Type* const type)
{
    const LeafType* const leafType = dynamic_cast<const LeafType*>(type);
    const StringType* const stringType = dynamic_cast<const StringType*>(type);
    const BoolType* const boolType = dynamic_cast<const BoolType*>(type);
    const FloatType* const floatType = dynamic_cast<const FloatType*>(type);
    const EnumType* const enumType = dynamic_cast<const EnumType*>(type);
    const StructUnionType* const structUnionType = dynamic_cast<const StructUnionType*>(type);

    const bool isDecOrNoneSpecifier =
        (format == ParseTreeFormatSpecifierDec) || (format == ParseTreeFormatSpecifierNone);

    if (stringType)
    {
        AppendFormatStringEntry(op, FormatStringType::StringHandle, reg, alignment);
    }
    else if (enumType && (format == ParseTreeFormatSpecifierNone))
    {
        enumType->Interpolate(context, op, reg, alignment);
    }
    else if (boolType && (format != ParseTreeFormatSpecifierBin))
    {
        AppendFormatStringEntry(op, FormatStringType::Boolean, reg, alignment);
    }
    else if (floatType && (format == ParseTreeFormatSpecifierNone))
    {
        AppendFormatStringEntry(op, FormatStringType::Float, reg, alignment);
    }
    else
    {
        FormatStringEntry fse = {};

        fse._type = FormatStringType::Integer;
        fse._operandIndex = op._src.size();
        fse._precision = boolType ? 1 : precision;
        fse._alignment = alignment;
        fse._specifier = format;

        if (leafType && isDecOrNoneSpecifier)
        {
            // Negative decimal values are printed like (-43)
            // All other format specifiers just print the raw bits
            fse._signed = (leafType->_baseType == BaseType::Int);
            fse._signBitIndex = leafType->_width - 1;
        }
        else
        {
            fse._signed = false;
            fse._signBitIndex = 0;
        }

        op._src.push_back(reg);

        op._flags._formatString._entries->push_back(fse);
    }
}

void AllocatedRegister::Interpolate(IRContext& context, Operation& op, const size_t alignment, const size_t precision,
                                    const ParseTreeFormatSpecifier format) const
{
    VisitRegisters([&](const AccessedRegister reg, const Type* const type)
                   { InterpolateRegister(context, op, alignment, precision, format, reg, type); });
}

AllocatedLeafRegister::AllocatedLeafRegister(const RegisterType registerType, const Type* const type,
                                             const size_t registerIndex)
    : AllocatedRegister(registerType, type), _registerIndex(registerIndex)
{
    // Register this object with the compiler, so that _registerIndex can be changed by CompactRegisterTable
    g_compiler->AddAllocatedLeafRegister(this);
}

AllocatedLeafRegister::~AllocatedLeafRegister() { g_compiler->RemoveAllocatedLeafRegister(this); }

AccessedRegister AllocatedLeafRegister::GetAccessedRegister() const
{
    const AccessedRegister result = {_registerIndex};

    return result;
}

void AllocatedLeafRegister::VisitRegisters(const UnaryVisitFunction& visitor) const
{
    visitor(GetAccessedRegister(), GetType());
}

void AllocatedLeafRegister::VisitRegisters(const AllocatedRegister* const rhs, const BinaryVisitFunction& visitor) const
{
    const AllocatedLeafRegister* const leafRhs = dynamic_cast<const AllocatedLeafRegister*>(rhs);

    visitor(GetAccessedRegister(), GetType(), leafRhs->GetAccessedRegister(), rhs->GetType());
}

// Note that initialValueNode can be null, in which case this should be zero-initialized
void AllocatedLeafRegister::Initialize(IRContext& context, const ParseTreeNode* const initialValueNode) const
{
    RegisterDescription& regDesc = context._program->_registerTable[_registerIndex];

    const BaseInitializerListNode* const initializerListNode =
        dynamic_cast<const BaseInitializerListNode*>(initialValueNode);

    if (regDesc._type == RegisterType::Global)
    {
        // Strings are always initialized to the null handle before this code runs
        assert(!regDesc.Global()._hasInitialValue || (g_compiler->GetStringType() == initialValueNode->GetType()));

        KnownValue value;

        if (initializerListNode)
        {
            // This handles 2 cases:
            // 1) A leaf node assigned to an empty initializer list: int32 x = {};
            // 2) A union initialized with a designated initializer list: U u = { .y = 3 };
            value = TypeConvert(initializerListNode->GetFlatValue(context), initialValueNode->GetType(), GetType());
        }
        else if (initialValueNode)
        {
            value = initialValueNode->TryGetKnownValue(context, GetType());
        }
        else
        {
            // When initialValueNode is null, it implies zero-initialization
            value = KnownValue(mp_int(0));
        }

        if (value._type == KnownValueType::Int)
        {
            assert(regDesc._type == RegisterType::Global);
            assert(regDesc._width == GetType()->GetBitWidth());

            regDesc.Global()._hasInitialValue = true;
            regDesc.Global()._initialValue = value._intVal;
        }
        else if ((value._type == KnownValueType::String) && value._stringVal.empty())
        {
            assert(regDesc._type == RegisterType::Global);
            assert(regDesc._width == GetType()->GetBitWidth());

            // The value._stringVal.empty() check is required
            // because backends do not support non-empty initial values
            // for non-local strings
            regDesc.Global()._hasInitialValue = true;
            regDesc.Global()._initialValue = 0; // the null handle, which corresponds to the empty string
        }
        else if (regDesc.Global()._isConstant && dynamic_cast<const StringType*>(initialValueNode->GetType()))
        {
            // For non-interpolated strings, front-end constexpr evaluation will replace all
            // references to the shared variable with the literal value
            // For interpolated strings, GenerateIRVariableData::_constantInitialValueNode will be used
            // to avoid referencing the shared register
            // So the global register exists in the IR, but is never used
        }
        else
        {
            g_compiler->ErrorStream(initialValueNode->GetLocation(), CompileError::InvalidGlobalAssignment)
                << "Variable cannot be initialized to a value of type: " << initialValueNode->GetType()->GetName();
        }
    }
    else
    {
        assert(regDesc._type == RegisterType::Memory);

        assert(initializerListNode);

        regDesc.Memory()._initialValues.resize(regDesc.Memory()._elementCount);

        // Write the entire initial value into 1 wide mp_int
        const mp_int wideInitialValue = initializerListNode->GetFlatValue(context);

        // Chop it up into element-sized pieces
        const mp_int mask = (mp_int(1) << regDesc.Memory()._elementWidth) - 1;

        for (size_t i = 0; i < regDesc.Memory()._elementCount; i++)
        {
            const mp_int elementValue = (wideInitialValue >> (i * regDesc.Memory()._elementWidth)) & mask;

            regDesc.Memory()._initialValues[i] = elementValue;
        }
    }
}

void AllocatedLeafRegister::Interpolate(IRContext& context, Operation& op, const size_t alignment,
                                        const size_t precision, const ParseTreeFormatSpecifier format) const
{
    const StructUnionType* const sut = dynamic_cast<const StructUnionType*>(GetType());

    if (sut)
    {
        assert(sut->_type == ContainerType::Union);

        sut->InterpolateUnion(context, op, GetAccessedRegister());
    }
    else
    {
        AllocatedRegister::Interpolate(context, op, alignment, precision, format);
    }
}

void AllocatedStructRegister::Interpolate(IRContext& context, Operation& op, const size_t alignment,
                                          const size_t precision, const ParseTreeFormatSpecifier format) const
{
    // Format specifiers are not allowed for structs
    assert(alignment == 0);
    assert(precision == 0);
    assert(format == ParseTreeFormatSpecifierNone);

    AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand("{"));

    bool isFirst = true;

    for (const auto& member : _members)
    {
        const ReferenceType* const referenceType = dynamic_cast<const ReferenceType*>(member.second->GetType());

        if (referenceType)
        {
            // Do not emit anything for captured $this
            continue;
        }

        if (!isFirst)
        {
            AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand(", "));
        }

        AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand(member.first + ":"));

        member.second->Interpolate(context, op, 0, 0, ParseTreeFormatSpecifierNone);

        isFirst = false;
    }

    AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand("}"));
}

const AllocatedLeafRegister* AllocateLeafRegister(Program& program, const BaseType baseType, const size_t bitWidth,
                                                  const Location& location, const RegisterType registerType,
                                                  const std::string& name, const ObjectPath& containerPath = {})
{
    const Type* const leafType = g_compiler->GetLeafType(baseType, bitWidth, location);

    const size_t registerIndex = AllocateRegister(program, bitWidth, registerType, name, containerPath);

    return g_compiler->Create<AllocatedLeafRegister>(registerType, leafType, registerIndex);
}

AllocatedRegister* BoolType::AllocateRegisters(Program& program, const RegisterType registerType,
                                               const std::string& name, const ObjectPath& containerPath,
                                               const SourceVariable& source) const
{
    const size_t registerIndex = AllocateRegister(program, 1, registerType, name, containerPath, source);

    return g_compiler->Create<AllocatedLeafRegister>(registerType, this, registerIndex);
}

AllocatedRegister* FloatType::AllocateRegisters(Program& program, const RegisterType registerType,
                                                const std::string& name, const ObjectPath& containerPath,
                                                const SourceVariable& source) const
{
    const size_t registerIndex = AllocateRegister(program, GetBitWidth(), registerType, name, containerPath, source);

    return g_compiler->Create<AllocatedLeafRegister>(registerType, this, registerIndex);
}

AllocatedRegister* LeafType::AllocateRegisters(Program& program, const RegisterType registerType,
                                               const std::string& name, const ObjectPath& containerPath,
                                               const SourceVariable& source) const
{
    const size_t registerIndex = AllocateRegister(program, GetBitWidth(), registerType, name, containerPath, source);

    return g_compiler->Create<AllocatedLeafRegister>(registerType, this, registerIndex);
}

class AllocatedArrayRegister : public AllocatedRegister
{
  public:
    typedef std::vector<AllocatedRegister*> ContainerType;

    AllocatedArrayRegister(const RegisterType registerType, const Type* const type, const ContainerType& elements)
        : AllocatedRegister(registerType, type), _elements(elements)
    {
    }

    void VisitRegisters(const UnaryVisitFunction& visitor) const override
    {
        for (const AllocatedRegister* const element : _elements)
        {
            element->VisitRegisters(visitor);
        }
    }

    void VisitRegisters(const AllocatedRegister* const rhs, const BinaryVisitFunction& visitor) const override
    {
        const AllocatedArrayRegister* const arrayRhs = dynamic_cast<const AllocatedArrayRegister*>(rhs);

        assert(_elements.size() == arrayRhs->_elements.size());

        for (size_t i = 0; i < _elements.size(); ++i)
        {
            _elements[i]->VisitRegisters(arrayRhs->_elements[i], visitor);
        }
    }

    // Note that initialValueNode can be null, in which case this should be zero-initialized
    void Initialize(IRContext& context, const ParseTreeNode* const initialValueNode) const override
    {
        const InitializerListNode* const initializerListNode =
            dynamic_cast<const InitializerListNode*>(initialValueNode);

        for (size_t i = 0; i < _elements.size(); i++)
        {
            if (initializerListNode && (i < initializerListNode->Children().size()))
            {
                const ParseTreeNode* const child = initializerListNode->Children()[i];

                _elements[i]->Initialize(context, child);
            }
            else
            {
                _elements[i]->Initialize(context, nullptr);
            }
        }
    }

    void SetConditionStackDepth(const size_t depth) override
    {
        AllocatedRegister::SetConditionStackDepth(depth);

        for (AllocatedRegister* const element : _elements)
        {
            element->SetConditionStackDepth(depth);
        }
    }

    void Interpolate(IRContext& context, Operation& op, const size_t alignment, const size_t precision,
                     const ParseTreeFormatSpecifier format) const override
    {
        // Format specifiers are not allowed for structs
        assert(alignment == 0);
        assert(precision == 0);
        assert(format == ParseTreeFormatSpecifierNone);

        AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand("["));

        bool isFirst = true;

        for (const AllocatedRegister* const element : _elements)
        {
            if (!isFirst)
            {
                AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand(", "));
            }

            element->Interpolate(context, op, 0, 0, ParseTreeFormatSpecifierNone);

            isFirst = false;
        }

        AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand("]"));
    }

    const ContainerType _elements;

  private:
    AllocatedArrayRegister operator=(const AllocatedArrayRegister& rhs) const;
};

AllocatedRegister* ArrayType::AllocateRegisters(Program& program, const RegisterType registerType,
                                                const std::string& name, const ObjectPath& containerPath,
                                                const SourceVariable& source) const
{
    AllocatedArrayRegister::ContainerType elements;

    // For debug symbols
    program._sourceContainers.push_back(SourceContainer(source._dataType, source._name, source._scope));
    SourceContainer* container = &program._sourceContainers.back();
    container->_parent = source._container;

    const size_t totalCount = GetTotalCount();

    for (size_t i = 0; i < totalCount; ++i)
    {
        // Append the element index to the name
        const std::string elementName = GetArrayElementObjectName(name, i);

        std::ostringstream elementSourceName;
        elementSourceName << source._name << "[" << i << "]";

        std::ostringstream elementScope;
        elementScope << source._scope << "[" << i << "]";

        const ObjectPath elementPath = AppendToPath(containerPath, "element_" + std::to_string(i));

        SourceVariable elementSource(elementSourceName.str(), elementScope.str(), _elementType->GetName(),
                                               _elementType->GetBitWidth(), source._declaredLocation);
        elementSource._container = container;
        elementSource._container->_members.push_back(
            {elementSourceName.str(), _elementType->GetName(), 0, _elementType->GetBitWidth()});

        elements.push_back(
            _elementType->AllocateRegisters(program, registerType, elementName, elementPath, elementSource));
    }

    return g_compiler->Create<AllocatedArrayRegister>(registerType, this, elements);
}

bool ArrayTypeBase::ContainsMemberRecursive(const std::string& memberName, const std::string& parentObjectName) const
{
    assert(!parentObjectName.empty());

    if (ContainsMemberRecursiveEarlyOut(memberName, parentObjectName))
    {
        // Don't waste time searching subtrees where a match is not possible
        return false;
    }

    const size_t totalCount = GetTotalCount();

    for (size_t i = 0; i < totalCount; ++i)
    {
        const std::string combinedName = GetArrayElementObjectName(parentObjectName, i);

        if (combinedName == memberName)
        {
            return true;
        }
        else
        {
            if (_elementType->ContainsMemberRecursive(memberName, combinedName))
            {
                return true;
            }
        }
    }

    return false;
}

AllocatedRegister* MemoryType::AllocateRegisters(Program& program, const RegisterType registerType,
                                                 const std::string& name, const ObjectPath& containerPath,
                                                 const SourceVariable& source) const
{
    if (registerType != RegisterType::Global)
    {
        throw std::runtime_error("Memory objects may not be local variables");
    }

    const size_t registerIndex =
        AllocateRegister(program, GetBitWidth(), RegisterType::Memory, name, containerPath, source);

    RegisterDescription& regDesc = program._registerTable[registerIndex];

    regDesc.Memory()._elementWidth = _elementType->GetBitWidth();
    regDesc.Memory()._elementCount = GetTotalCount();
    regDesc.Memory()._readPortCount = 0;
    regDesc.Memory()._writePortCount = 0;
    regDesc.Memory().ClearBypassMask();

    regDesc.Memory()._replicate = true;
    regDesc.Memory()._quadPort = false;
    regDesc.Memory()._ecc = false;
    regDesc.Memory()._location = SourceVariableToLocation(source);

    switch (_memoryType)
    {
    case ParseTreeMemoryTypeDefault:
        break;

    case ParseTreeMemoryTypeNoReplication:
        regDesc.Memory()._replicate = false;
        break;

    case ParseTreeMemoryTypeQuadPort:
        regDesc.Memory()._replicate = false;
        regDesc.Memory()._quadPort = true;
        break;

    case ParseTreeMemoryTypeEcc:
        regDesc.Memory()._ecc = true;
        break;

    default:
        assert(false);
        break;
    }

    if (_autoInitialize)
    {
        assert(regDesc.Memory()._initialValues.empty());

        // Initialize to 0
        regDesc.Memory()._initialValues.resize(regDesc.Memory()._elementCount, 0);
    }

    regDesc.Memory()._sourceDataType = _elementType->GetName();

    return g_compiler->Create<AllocatedLeafRegister>(registerType, this, registerIndex);
}

AllocatedRegister* AllocateClassStructRegisters(Program& program, const RegisterType registerType,
                                                const std::string& name, const Compiler::MemberVariableList& members,
                                                const Type* const type, const ObjectPath& containerPath,
                                                const SourceVariable& source)
{
    AllocatedStructRegister::ContainerType memberRegisters;

    const bool isClass = (nullptr != dynamic_cast<const ClassType*>(type));

    // For debug symbols
    program._sourceContainers.push_back(SourceContainer(type->GetName(), source._name, source._scope));
    SourceContainer* container = &program._sourceContainers.back();
    container->_parent = source._container;

    program._objectNameToContainers[name] = container;

    // It is important that members stay in the same order
    // To ensure that the VisitRegisters() functions run in the correct order
    for (const auto& entry : members)
    {
        const std::string& memberName = entry.first;
        const DeclareNode* const declareNode = entry.second;

        // Call the declare node associated with the member to allocate the register
        const AllocatedRegister* const allocatedRegister =
            declareNode->AllocateRegistersInternal(program, registerType, name, "", container, containerPath);

        if (isClass)
        {
            // Associate the register pointer with the object name
            // so that the register can be looked up by member variable name
            const_cast<DeclareNode*>(declareNode)->SetAllocatedRegisterForObject(allocatedRegister, name);
        }

        const AllocatedStructRegister::EntryType member(memberName, allocatedRegister);

        memberRegisters.push_back(member);
    }

    return g_compiler->Create<AllocatedStructRegister>(registerType, type, memberRegisters);
}

AllocatedRegister* StructUnionType::AllocateRegisters(Program& program, const RegisterType registerType,
                                                      const std::string& name, const ObjectPath& containerPath,
                                                      const SourceVariable& source) const
{
    if (_type == ContainerType::Union)
    {
        // Allocate a single register of the maximum size
        size_t requiredWidth = 0;

        for (const auto& entry : _members)
        {
            const DeclareNode* const declareNode = entry.second;

            const size_t bitWidth = declareNode->GetDeclaredType()->GetBitWidth();
            assert(bitWidth > 0);

            requiredWidth = std::max<size_t>(requiredWidth, bitWidth);
        }

        if (requiredWidth == 0)
        {
            throw std::runtime_error("Empty structure");
        }

        const size_t registerIndex =
            AllocateRegister(program, requiredWidth, registerType, name, containerPath, source);

        return g_compiler->Create<AllocatedLeafRegister>(registerType, this, registerIndex);
    }
    else
    {
        return AllocateClassStructRegisters(program, registerType, name, _members, this, containerPath, source);
    }
}

size_t StructUnionType::GetRegisterCount() const
{
    size_t result = 0;

    for (const EntryType& entry : _members)
    {
        result += entry.second->GetDeclaredType()->GetRegisterCount();
    }

    return result;
}

bool StructUnionType::ShouldExport() const
{
    // Don't export empty structs as they are not supported by SystemVerilog
    bool result = false;

    for (const StructUnionType::EntryType& member : _members)
    {
        if (member.second->GetDeclaredType()->ShouldExport())
        {
            result = true;
        }
    }

    return result;
}

void StructUnionType::InterpolateUnion(IRContext& context, Operation& op, const AccessedRegister srcReg) const
{
    assert(ContainerType::Union == _type);

    // Union interpolation emits all possible interpretations of the value

    AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand("{"));

    bool isFirst = true;

    for (const auto& member : _members)
    {
        if (!isFirst)
        {
            AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand(", "));
        }

        AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand(member.first + ":"));

        // Copy data from reg into a new register of the field type
        const AllocatedRegister* const field = member.second->GetDeclaredType()->AllocateRegisters(
            *context._program, RegisterType::Local, "UnionInterpolateField");

        size_t offset = 0;

        const auto callback = [&](const AccessedRegister dstReg, const Type* const type)
        {
            const size_t dstWidth = type->GetBitWidth();

            Operation op = {};

            op._opcode = Opcode::BinaryOp;

            op._flags._binaryOpType = ParseTreeBinaryOpTypeShr;

            op._src.push_back(srcReg);
            op._src.push_back(offset);

            op._dst.push_back(dstReg);

            context._basicBlock->_operations.push_back(op);

            offset += dstWidth;
        };

        field->VisitRegisters(callback);

        field->Interpolate(context, op, 0, 0, ParseTreeFormatSpecifierNone);

        isFirst = false;
    }

    AppendFormatStringEntry(op, FormatStringType::StringLiteral, SourceOperand("}"));
}

void ClassType::Reset()
{
    _objectNames.clear();
    _perObjectRegisters.clear();
}

AllocatedRegister* ClassType::AllocateRegisters(Program& program, const RegisterType registerType,
                                                const std::string& name, const ObjectPath& containerPath,
                                                const SourceVariable& source) const
{
    AllocatedRegister* const result =
        AllocateClassStructRegisters(program, registerType, name, _memberVariables, this, containerPath, source);

    // Remember the registers associated with a particular object
    SafeInsert(_perObjectRegisters, name, result);

    return result;
}

// For types that do not contain any actual run-time data (strings, references)
class AllocatedEmptyRegister : public AllocatedRegister
{
  public:
    AllocatedEmptyRegister(const RegisterType registerType, const Type* const type)
        : AllocatedRegister(registerType, type)
    {
    }

    void VisitRegisters(const UnaryVisitFunction& visitor) const override {}

    void VisitRegisters(const AllocatedRegister* const rhs, const BinaryVisitFunction& visitor) const override {}

    void Initialize(IRContext& context, const ParseTreeNode* const initialValueNode) const override
    {
        throw RuntimeErrorWithTrace("AllocatedEmptyRegister::Initialize should not be called");
    }

  private:
    AllocatedEmptyRegister operator=(const AllocatedEmptyRegister& rhs) const;
};

AllocatedRegister* StringType::AllocateRegisters(Program& program, const RegisterType registerType,
                                                 const std::string& name, const ObjectPath& containerPath,
                                                 const SourceVariable& source) const
{
    // Ensure that all registers have a non-empty name
    const std::string registerName = name.empty() ? "empty_string" : name;

    const size_t registerIndex =
        AllocateRegister(program, c_stringHandleWidth, registerType, registerName, containerPath, source);

    return g_compiler->Create<AllocatedLeafRegister>(registerType, this, registerIndex);
}

AllocatedRegister* ReferenceType::AllocateRegisters(Program& program, const RegisterType registerType,
                                                    const std::string& name, const ObjectPath& containerPath,
                                                    const SourceVariable& source) const
{
    return g_compiler->Create<AllocatedEmptyRegister>(registerType, this);
}

AllocatedRegister* VoidType::AllocateRegisters(Program& program, const RegisterType registerType,
                                               const std::string& name, const ObjectPath& containerPath,
                                               const SourceVariable& source) const
{
    return g_compiler->Create<AllocatedEmptyRegister>(registerType, this);
}

AllocatedRegister* FunctionType::AllocateRegisters(Program& program, const RegisterType registerType,
                                                   const std::string& name, const ObjectPath& containerPath,
                                                   const SourceVariable& source) const
{
    return g_compiler->Create<AllocatedEmptyRegister>(registerType, this);
}

void ClassType::EnumerateContainedObjectNames(std::vector<std::string>& objectNames) const
{
    // Add object names of this class type
    for (const std::string& name : _objectNames)
    {
        objectNames.push_back(name);
    }

    // Child classes
    const std::vector<ClassType::EntryType> memberVariables = GetMemberVariables();

    for (const ClassType::EntryType& memberVariable : memberVariables)
    {
        memberVariable.second->GetDeclaredType()->EnumerateContainedObjectNames(objectNames);
    }
}

// Determines if a particular name refers to a member variable
// Note that "memberName" could be composite like: __obj1__obj2
bool ClassType::ContainsMemberRecursive(const std::string& memberName, const std::string& parentObjectName) const
{
    if (ContainsMemberRecursiveEarlyOut(memberName, parentObjectName))
    {
        // Don't waste time searching subtrees where a match is not possible
        return false;
    }

    for (const ClassType::EntryType& memberVariable : _memberVariables)
    {
        // Check this member name
        const std::string combinedName =
            parentObjectName.empty() ? memberVariable.first
                                     : CombineObjectAndMemberName(Scope(), parentObjectName, memberVariable.first);

        if (combinedName == memberName)
        {
            return true;
        }

        // If the member variable is an object, recursively check its members for a match
        if (memberVariable.second->GetDeclaredType()->ContainsMemberRecursive(memberName, combinedName))
        {
            return true;
        }
    }

    return false;
}

// Lookup registers for a given member variable and object
const AllocatedRegister* ClassType::GetRegistersForMember(const std::string& objectName, const std::string& memberName,
                                                          const Location& location) const
{
    for (const EntryType& et : _memberVariables)
    {
        if (et.first == memberName)
        {
            return et.second->GetRegisters(objectName);
        }
    }

    g_compiler->ErrorStream(location, CompileError::InvalidMember)
        << "Object does not contain member variable: " << memberName;
    throw std::runtime_error("Unable to find member");
}

const AllocatedRegister* ClassType::GetRegistersForObject(const std::string& objectName) const
{
    return SafeLookup(_perObjectRegisters, objectName);
}

static void AddSemaphore(Program& program, Function& function, BasicBlock& firstBlock, BasicBlock& lastBlock)
{
    // This also ensures that the basic blocks at the start and end of the function are not optimized away if they are
    // otherwise empty The location of the opcode within the basic block does not matter The back-ends will acquire and
    // release at the approriate points These opcodes only serve to inform the back-ends which basic blocks need to
    // acquire/release

    // Allocate a semaphore index
    const size_t semaphoreIndex = program._semaphoreCount;
    ++program._semaphoreCount;

    function._semaphores.push_back(semaphoreIndex);

    {
        Operation op = {};

        op._opcode = Opcode::AcquireSemaphore;
        op._flags._semaphoreIndex = semaphoreIndex;
        op.InsertLocation(function._functionNode->GetLocation());

        firstBlock._operations.push_front(op);
    }

    {
        Operation op = {};

        op._opcode = Opcode::ReleaseSemaphore;
        op._flags._semaphoreIndex = semaphoreIndex;
        op.InsertLocation(function._functionNode->GetLocation());

        lastBlock._operations.push_back(op);
    }
}

// During basicBlock construction operations are not always assigned a line number.
// Instead we set a marker (_opcode==Opcode::LineNumber) at the start, create the ops,
// then come back here to forward-propagate the line number information at one go.
void PropagateLineNumbersToOps(OperationList& operations)
{
    FileAndLineNumber fnum;
    bool fnumValid = false;

    for (auto it = operations.begin(); it != operations.end(); ++it)
    {
        Operation& op = *it;
        if (op._opcode == Opcode::LineNumber)
        {
            assert(1 == op._locations.size());
            fnum = *(op._locations.begin());
            fnumValid = true;
        }
        else if (op._locations.empty() &&
                 !(op._opcode == Opcode::ReleaseSemaphore || op._opcode == Opcode::AcquireSemaphore))
        {
            // Do not propagate line number information to AcquireSemaphore and ReleaseSemaphore
            // operations. They are removed from the backends anyway.
            // ReleaseSemaphore is at the end of a BasicBlock's chain of operations
            // and so it receives the last line number for the BasicBlock.
            // However, in optimized mode, it is scheduled
            // at the beginning of the pipeline which skews the debugger view
            // Do not propagate invalid file numbers.
            if (fnumValid)
            {
                op._locations.insert(fnum);
            }
        }
    }
}

void PropagateLineNumbers(BasicBlock* basicBlock)
{
    PropagateLineNumbersToOps(basicBlock->_operations);
    PropagateLineNumbersToOps(basicBlock->_startConditionOperations);
}

void EmitLineNumber(BasicBlock* basicBlock, const Location& location)
{
    if (basicBlock && (GetCodeGenConfig()._optimize == 0))
    {
        // Emit IR that indicates which source line this node is associated with
        // This serves as an instruction scheduling barrier,
        // to ensure that operations that stem from one source line are scheduled together,
        // and that the optimizer does not schedule multiple source lines in the same cycle.
        FileAndLineNumber fnum = LocationToFileAndLineNumber(location);

        Operation op = {};
        op._opcode = Opcode::LineNumber;
        op._locations.insert(fnum);

        // Ensures no duplicate line numbers within a BasicBlock
        // This is important for debug backend since we schedule
        // operations stemming from one source line together
        // New BasicBlocks will also receive a line number upon
        // creation
        if (basicBlock->_lastEmittedLineNumber != fnum)
        {
            basicBlock->_operations.push_back(op);

            basicBlock->_lastEmittedLineNumber = fnum;
        }
    }
}

FileAndLineNumber LocationToFileAndLineNumber(const Location& loc)
{
    FileAndLineNumber f;
    f._lineNumber = loc._beginLine;
    f._fileIndex = loc._fileIndex;
    f._columnNumber = loc._beginColumn;
    assert(!f._callStackIndex);
    return f;
}

FileAndLineNumber LocationToFileAndLineNumber(const Location& loc, const size_t callStackIndex)
{
    FileAndLineNumber f;
    f._lineNumber = loc._beginLine;
    f._fileIndex = loc._fileIndex;
    f._columnNumber = loc._beginColumn;
    f._callStackIndex = callStackIndex;
    return f;
}

Location FileAndLineNumberToLocation(const FileAndLineNumber& faln)
{
    Location l = {};

    l._beginLine = faln._lineNumber;
    l._endLine = faln._lineNumber;
    l._fileIndex = faln._fileIndex;
    l._beginColumn = faln._columnNumber;
    l._endColumn = faln._columnNumber;
    l._valid = true;

    return l;
}

void FormatString(IRContext& context, const Location location, const std::function<void(Operation&)>& cb,
                  const Opcode opcode = Opcode::FormatString)
{
    assert(context._writtenRegisters != nullptr);
    assert(context._basicBlock != nullptr);

    SetOperationLocation sol(context, location);

    // Allocate a new register for the string handle
    // This is done with a shared register.
    // The upper bits of the shared register hold the allocation location
    // the lower bits increment each time a string is allocated from this location
    const ObjectPath basicBlockPath = context.GetBasicBlockContainerPath();

    const std::string allocatorName = context.GenerateUniqueNameWithinPath(basicBlockPath, "StringAllocator");

    const ObjectPath allocatorPath = AppendToPath(basicBlockPath, allocatorName);

    const AllocatedLeafRegister* const allocatorLeafReg =
        AllocateLeafRegister(*context._program, BaseType::Uint, c_stringHandleStringIdBits, location,
                             RegisterType::Global, allocatorName, allocatorPath);

    RegisterDescription& allocatorRegDesc =
        context._program->_registerTable[allocatorLeafReg->GetAccessedRegister()._registerIndex];
    assert(RegisterType::Global == allocatorRegDesc._type);

    allocatorRegDesc.Global()._hasInitialValue = true;
    allocatorRegDesc.Global()._initialValue = 0;

    // Atomically save the allocator value into a local register and increment the allocator
    const AccessedRegister relativeStringHandle = {
        AllocateRegister(context._program, c_stringHandleStringIdBits, RegisterType::Local, "RelativeStringHandle")};

    {
        const AccessedRegister temporarySum = {
            AllocateRegister(context._program, c_stringHandleStringIdBits, RegisterType::Local, "NewStringAllocator")};

        OptionalAtomicBlock atomicBlock(context, c_defaultAtomicBlockUpdateRate, location);

        // Read the global ID value
        {
            Operation op = {};

            op._opcode = Opcode::Mov;

            op._src.push_back(SourceOperand(allocatorLeafReg->GetAccessedRegister()));
            op._dst.push_back(relativeStringHandle);

            context._basicBlock->_operations.push_back(op);
        }

        // Increment the global ID - note that wrapping is desired
        // this is broken up into 2 operations, writes to globals use the WriteGlobal opcode
        {
            Operation op = {};

            op._opcode = Opcode::BinaryOp;
            op._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

            op._src.resize(2);
            op._src[0] = SourceOperand(relativeStringHandle);
            op._src[1] = SourceOperand(1);
            op._dst.push_back(temporarySum);

            context._basicBlock->_operations.push_back(op);
        }

        {
            Operation op = {};

            op._opcode = Opcode::WriteGlobal;

            op._flags._writeGlobal._isPredicated = false;

            op._src.push_back(SourceOperand(temporarySum));
            op._dst.push_back(allocatorLeafReg->GetAccessedRegister());

            context._basicBlock->_operations.push_back(op);
        }
    }

    // Combine the snapped ID with the allocation location
    const size_t allocationLocationId = context._program->_stringAllocationLocation;
    context._program->_stringAllocationLocation++;
    assert(allocationLocationId != 0); // a location ID of 0 would allow a null handle to be allocated
    assert((allocationLocationId <
            (1ull << c_stringHandleAllocationLocationBits))); // Fires if no more locations are available

    const AllocatedLeafRegister* const stringHandleReg = AllocateLeafRegister(
        *context._program, BaseType::Uint, c_stringHandleWidth, location, RegisterType::Local, "StringHandle");

    {
        Operation op = {};

        op._opcode = Opcode::BinaryOp;
        op._flags._binaryOpType = ParseTreeBinaryOpTypeOr;

        op._src.resize(2);
        op._src[0] = SourceOperand(relativeStringHandle);
        op._src[1] = SourceOperand(allocationLocationId << c_stringHandleStringIdBits);
        op._dst.push_back(stringHandleReg->GetAccessedRegister());

        context._basicBlock->_operations.push_back(op);
    }

    SourceOperand predicateOperand;

    if (context.IsPredicated())
    {
        predicateOperand = SourceOperand(AccessedRegister{context.GetPredicate()});
    }
    else
    {
        predicateOperand = SourceOperand(1, 1);
    }

    {
        Operation op = {};

        op.InsertLocation(location);

        op._opcode = opcode;

        op._src.push_back(stringHandleReg->GetAccessedRegister());

        op._src.push_back(predicateOperand);

        if (opcode == Opcode::FormatString)
        {
            op._flags._formatString._entries = g_compiler->Create<std::vector<FormatStringEntry>>();
        }
        else
        {
            assert(opcode == Opcode::FormatEnum);
            op._flags._formatEnum._entries = g_compiler->Create<std::vector<FormatEnumEntry>>();

            op._flags._formatEnum._defaultString = g_compiler->Create<std::string>();
        }

        // Callback will fill out the rest of the operation
        cb(op);

        context._basicBlock->_operations.push_back(op);
    }

    Move(context, context._writtenRegisters, stringHandleReg, Opcode::Mov, location);

    // Release a reference on the string when leaving the current scope
    {
        IRContext* const contextPtr = &context;

        context._typeContext.OnLeaveCurrentScope(
            [stringHandleReg, location, contextPtr, predicateOperand]()
            {
                Operation op = {};

                op.InsertLocation(location);

                op._opcode = Opcode::ReferenceString;

                op._src.push_back(predicateOperand);

                op._src.push_back(stringHandleReg->GetAccessedRegister());

                op.PushOperand(c_stringReferenceNegOne, true);

                contextPtr->_basicBlock->_operations.push_back(op);
            });
    }
}

// Allocate a register, then call FormatString and store the handle in that register
AccessedRegister AllocateAndFormatString(IRContext& context, const Location location,
                                         const std::function<void(Operation&)>& cb,
                                         const Opcode opcode = Opcode::FormatString)
{
    // Save context._writtenRegisters
    PushPopWrittenRegister pushPopWrittenReg(context);

    const AllocatedRegister* const reg =
        g_compiler->GetLeafType(BaseType::Uint, c_stringHandleWidth, location)
            ->AllocateRegisters(*context._program, RegisterType::Local, "FormatString");

    context._writtenRegisters = reg;

    FormatString(context, location, cb, opcode);

    return safe_cast<const AllocatedLeafRegister*>(reg)->GetAccessedRegister();
}

// Function: CreateCodeCoverageCounter
//
// Returns a set of ops that sets a flag if counterInput is true. The flag is
// exposed as a code coverage inspectable.
OperationList CreateCodeCoverageCounter(Program* const program, const std::string name, const std::string description,
                                        const Location& location, const SourceOperand counterInput,
                                        const CodeCoverage& codeCoverage, BasicBlock& basicBlock)
{
    assert(GetCodeGenConfig()._codeCoverage);

    OperationList operationList;

    {
        SetOperationLocation sol(operationList, location);

        const ObjectPath basicBlockPath = basicBlock.GetObjectPath();
        const std::string suffixContainerPath =
            "_" + name + "_" + std::to_string(program->codeCoverageCounterMap[&basicBlock]++);
        const ObjectPath containerPath = AppendToPath(basicBlockPath, suffixContainerPath);
        const AccessedRegister counterReg = {AllocateRegister(program, 1, RegisterType::Global, name, containerPath)};

        // Make counterReg inspectable
        InspectableVariable inspectableVariable = {};
        inspectableVariable._inspectableIndex = program->_inspectableVariables.size();
        inspectableVariable._type = g_compiler->GetLeafType(BaseType::Uint, 1, location);
        inspectableVariable._name = name;
        inspectableVariable._description = description;
        inspectableVariable._registers.push_back(counterReg._registerIndex);
        inspectableVariable._registerType = RegisterType::Global;
        inspectableVariable._inspectionType = InspectableVariableType::CodeCoverage;
        inspectableVariable._codeCoverage = codeCoverage;

        // Generate code coverage as coverpoint
        program->_codeCoverageVariables.push_back(inspectableVariable);

        // Initialize inspectable
        RegisterDescription& regDesc = program->_registerTable[counterReg._registerIndex];
        regDesc.Global()._hasInitialValue = true;
        regDesc.Global()._initialValue = 0;

        const AccessedRegister cmpReg = {AllocateRegister(program, 1, RegisterType::Local, name + "_not_0")};

        // Check if input is not 0
        Operation cmpOp = {};
        cmpOp._opcode = Opcode::BinaryOp;
        cmpOp._flags._binaryOpType = ParseTreeBinaryOpTypeNE;
        cmpOp._src.push_back(counterInput);
        cmpOp._src.push_back(0);
        cmpOp._dst.push_back(cmpReg);
        operationList.push_back(cmpOp);

        // Set flag if input is not 0
        Operation writeOp = {};
        writeOp._opcode = Opcode::WriteGlobal;
        writeOp._flags._writeGlobal._isPredicated = true;
        writeOp._src.push_back(1);
        writeOp._src.push_back(cmpReg);
        writeOp._dst.push_back(counterReg);
        operationList.push_back(writeOp);
    }

    return operationList;
}

BasicBlock* IRContext::CreateBasicBlock(const Location& location)
{
    BasicBlock* basicBlock = _function->CreateBasicBlock(location, GetCallStackIndex(), _program);

    std::stack<std::string> objectNameStackCopy = this->_objectNameStack;

    do
    {
        // Add the object this BasicBlock is associated with
        // A BasicBlock can be tied to multiple objects if it
        // contains inlined member functions
        const std::string objectName = objectNameStackCopy.top();

        if (objectName != g_globalObjectName)
        {
            basicBlock->_objectNames.insert(objectName);
        }

        objectNameStackCopy.pop();

    } while (_inlineCallCount > 0 && !objectNameStackCopy.empty());

    return basicBlock;
}

const std::string IRContext::GetRegisterName(const AccessedRegister ar) const { return _program->GetRegisterName(ar); }

// Get the path to the current basic block
ObjectPath IRContext::GetBasicBlockContainerPath()
{
    assert(_function);
    return _basicBlock->GetObjectPath();
}

// Generate a string that is unique within a given path
// It will have the form "baseNameN" where N is an integer
std::string IRContext::GenerateUniqueNameWithinPath(const ObjectPath& path, const std::string& baseName)
{
    std::map<std::string, size_t>& nameToCount = _uniqueNameMap[path];

    const std::string result = baseName + std::to_string(nameToCount[baseName]++);

    return result;
}

BasicBlock* Function::CreateBasicBlock(const Location& location, const size_t callStackIndex, Program* const program)
{
    _basicBlocks.emplace_back(this, location, callStackIndex);

    BasicBlock* const result = &(_basicBlocks.back());

    EmitLineNumber(result, location);

    result->_indexWithinFunction = _basicBlocks.size() - 1;

    if (GetCodeGenConfig()._codeCoverage)
    {
        // Create code coverage counter
        std::string name = this->_name + "_BasicBlockCodeCoverage_" + std::to_string(result->_indexWithinFunction);
        std::string description = "Code coverage for basic block in function " + this->_name;

        // Extra information for code coverage
        CodeCoverage codeCoverage = {};
        codeCoverage._coverageType = CodeCoverageType::BasicBlock;
        codeCoverage._statements.first = location;
        // Currently do not have information for end of basic block
        codeCoverage._statements.second._valid = false;
        codeCoverage._basicBlock = result;

        OperationList codeCoverageOps =
            CreateCodeCoverageCounter(program, name, description, location, SourceOperand(1), codeCoverage, *result);
        result->_operations.splice(result->_operations.end(), codeCoverageOps);
    }

    return result;
}

const Function* Program::GetEntryPoint(const std::string& name) const
{
    const Function* result = nullptr;

    for (const EntryPoint& entryPoint : _entryPoints)
    {
        // This function is only ever used to get entry points in the global namespace
        if (entryPoint._scope.empty() && (entryPoint._classType == g_compiler->GetGlobalClassType()) &&
            (entryPoint._functionName == name))
        {
            assert(1 == entryPoint._instances.size());

            result = entryPoint._instances.front();
        }
    }

    return result;
}

bool Function::IsExport() const { return _functionNode->GetModifiers() & ParseTreeFunctionModifierExport; }

bool Function::IsExtern() const { return _functionNode->GetModifiers() & ParseTreeFunctionModifierExternal; }

bool Function::IsExportClassInterface() const
{
    return _functionNode->GetModifiers() & ParseTreeFunctionModifierExportClassInterface;
}

bool Function::IsAsync() const { return _functionNode->GetModifiers() & ParseTreeFunctionModifierAsync; }

bool Function::IsPipelined() const { return _functionNode->GetModifiers() & ParseTreeFunctionModifierPipelined; }

bool Function::IsUnordered() const { return _functionNode->GetModifiers() & ParseTreeFunctionModifierUnordered; }

bool Function::IsNoBackpressure() const
{
    return _functionNode->GetModifiers() & ParseTreeFunctionModifierNoBackPressure;
}

bool Function::IsFixedLatency() const { return _functionNode->IsFixedLatency(); }

bool Function::HasEndTransactionParameter() const
{
    bool result = false;

    for (size_t i = 0; i < _functionNode->GetParameterCount(); i++)
    {
        const DeclareNode* const declareNode = _functionNode->GetParameterDeclareNode(i);

        if (declareNode->_isEndTransaction)
        {
            // Only 1 parameter should have this bit
            assert(!result);

            result = true;
        }
    }

    return result;
}

size_t Function::GetEndTransactionRegister() const
{
    assert(HasEndTransactionParameter());

    size_t result = std::numeric_limits<size_t>::max();

    for (size_t i = 0; i < _functionNode->GetParameterCount(); i++)
    {
        const DeclareNode* const declareNode = _functionNode->GetParameterDeclareNode(i);

        if (declareNode->_isEndTransaction)
        {
            const FunctionNode::Instance& instance = _functionNode->GetInstance(_objectName);

            const AllocatedLeafRegister* const localReg =
                dynamic_cast<const AllocatedLeafRegister*>(instance.GetParameterRegisters(i));

            result = localReg->_registerIndex;
        }
    }

    return result;
}

bool Function::CallOnReset() const { return _functionNode->GetModifiers() & ParseTreeFunctionModifierReset; }

bool Function::IsExternClassMethod() const { return _classType->IsExternal(); }

std::string Function::GetBackendName() const
{
    std::string result = _name;

    if (_externClassInstance)
    {
        result = GetCombinedExternalClassInstanceFunctionName(_externClassInstance->_name, _name);

        if (_objectName != g_globalObjectName)
        {
            result = _objectName + "_" + result;
        }
    }

    result = FlattenScopeAndAppendFunctionName(_classType, _scope, result);

    return FixupString(result);
}

void DebuggerHintPushPredicate(OperationList& operations)
{
    if (GetCodeGenConfig()._optimize == 0)
    {
        Operation op = {};

        // In debug mode, explicit LineNumber operations will be emitted
        op._expectNoSourceLocation = true;

        op._opcode = Opcode::PushPredicate;

        operations.push_back(op);
    }
}

void DebuggerHintPopPredicate(OperationList& operations)
{
    if (GetCodeGenConfig()._optimize == 0)
    {
        Operation op = {};

        // In debug mode, explicit LineNumber operations will be emitted
        op._expectNoSourceLocation = true;

        op._opcode = Opcode::PopPredicate;

        operations.push_back(op);
    }
}

void IRContext::PushPredicate(const size_t inputPredicateRegisterIndex, const Location& location)
{
    // Assign locations to generated operations
    SetOperationLocation sol(*this, location);

    const AccessedRegister inputPredicate = {inputPredicateRegisterIndex};

    PredicateStackEntry pse = {};

    // Save a copy of the predicate register
    // In case the original predicate register is changed after this point
    {
        const AccessedRegister predicateCopy = {AllocateDuplicateRegister(_program, inputPredicateRegisterIndex)};

        Operation op = {};

        op._opcode = Opcode::Mov;
        op._src.push_back(inputPredicate);
        op._dst.push_back(predicateCopy);
        op.InsertLocation(location);

        _basicBlock->_operations.push_back(op);

        pse._leafRegister = predicateCopy;
    }

    if (_predicateStack.empty())
    {
        pse._combinedRegister = pse._leafRegister;
    }
    else
    {
        // Combine the new predicate with the currently pushed predicates
        const AccessedRegister topOfStackPredicate = _predicateStack.back()._combinedRegister;

        std::string combinedName = _program->_registerTable[topOfStackPredicate._registerIndex]._name + "_and_" +
                                   _program->_registerTable[inputPredicateRegisterIndex]._name;

        const AccessedRegister combinedPredicate = {AllocateRegister(_program, 1, RegisterType::Local, combinedName)};

        Operation op = {};

        op._opcode = Opcode::BinaryOp;
        op._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;

        op._src.push_back(inputPredicate);
        op._src.push_back(topOfStackPredicate);
        op._dst.push_back(combinedPredicate);

        _basicBlock->_operations.push_back(op);

        pse._combinedRegister = combinedPredicate;
    }

    _predicateStack.push_back(pse);

    // Debug symbols: flag the predicate register for later addition to _sourceToPipelineRegisterMap
    {
        RegisterDescription& regDesc = _program->_registerTable[pse._combinedRegister._registerIndex];

        regDesc.SetRegisterSourceType(RegisterSourceType::Predicate);

        DebuggerHintPushPredicate(_basicBlock->_operations);
    }
}

void IRContext::PushInversePredicate(const size_t inputPredicateRegisterIndex, const Location& location)
{
    // Assign locations to generated operations
    SetOperationLocation sol(*this, location);

    // Allocate a 1-bit register to hold the inverse
    const AccessedRegister invertedPredicate = {AllocateRegister(
        _program, 1, RegisterType::Local, "not_" + _program->_registerTable[inputPredicateRegisterIndex]._name)};

    const AccessedRegister predicate = {inputPredicateRegisterIndex};

    {
        // Add an operation which will invert the predicate
        Operation op = {};

        op._opcode = Opcode::UnaryOp;
        op._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;

        op._src.push_back(predicate);
        op._dst.push_back(invertedPredicate);

        _basicBlock->_operations.push_back(op);
    }

    PushPredicate(invertedPredicate._registerIndex, location);
}

void IRContext::PopPredicate()
{
    assert(!_predicateStack.empty());

    _predicateStack.pop_back();

    DebuggerHintPopPredicate(_basicBlock->_operations);
}

bool IRContext::IsPredicated() const { return !_predicateStack.empty(); }

size_t IRContext::GetPredicate() const
{
    assert(IsPredicated());

    return _predicateStack.back()._combinedRegister._registerIndex;
}

// Inserts a ConditionalIgnore operation
// that indicates that a register value can be ignored
// if the combined predicate is false
void IRContext::PredicatedIgnore(const AccessedRegister reg, const Location& location)
{
    assert(GetCodeGenConfig().ControlClockGatingEnabled());
    assert(IsPredicated());
    assert(RegisterType::Local == _program->_registerTable[reg._registerIndex]._type);

    Operation op = {};

    op.InsertLocation(location);

    op._opcode = Opcode::ConditionalIgnore;
    op._dst.push_back(reg);
    op._src.push_back(reg);

    // Push all leaf predicate registers
    // the input/output value register can be ignored
    // if any predicate register is false
    for (const PredicateStackEntry& pse : _predicateStack)
    {
        op._src.push_back(pse._leafRegister);
    }

    _basicBlock->_operations.push_back(op);
}

KnownValue IRContext::LookupKnownValueForSymbol(const Scope& scope, const std::string& name, const Location& location)
{
    const GenerateIRVariableData variableData = _typeContext.LookupSymbol(scope, name, location);

    return variableData._value;
}

void IRContext::SetSymbolKnownValue(const Scope& scope, const std::string& name, const KnownValue& knownValue,
                                    const Location& location)
{
    GenerateIRVariableData data = _typeContext.LookupSymbol(scope, name, location);

    assert(data._value._type == KnownValueType::None);

    data._value = knownValue;

    _typeContext.UpdateSymbol(scope, name, data, location);
}

void IRContext::Barrier(const Location& location)
{
    Operation op = {};

    op.InsertLocation(location);

    op._opcode = Opcode::Stage;

    _basicBlock->_operations.push_back(op);
}

size_t IRContext::AllocateAtomicChainIndex()
{
    _atomicChainIndex++;

    return _atomicChainIndex;
}

// Returns the name of the object currently being compiled
// Can different from _objectNameStack.top if a function with reference parameters is being compiled
std::string IRContext::GetObjectName()
{
    assert(!_objectNameStack.empty());
    return _functionInstanceStack.empty() ? _objectNameStack.top() : _functionInstanceStack.back()._objectName;
}

std::string CodeCoverageTypeToString(const CodeCoverageType codeCoverageType)
{
    std::string str;
    switch (codeCoverageType)
    {
    case CodeCoverageType::BasicBlock:
        str = "BasicBlock";
        break;
    case CodeCoverageType::IfStatement:
        str = "IfStatement";
        break;
    case CodeCoverageType::Condition:
        str = "Condition";
        break;
    case CodeCoverageType::LoopCondition:
        str = "LoopCondition";
        break;
    case CodeCoverageType::SwitchStatement:
        str = "SwitchStatement";
        break;
    case CodeCoverageType::MuxCondition:
        str = "MuxCondition";
        break;
    case CodeCoverageType::Expression:
        str = "Expression";
        break;
    default:
        assert(false);
        break;
    }
    return str;
}

// Generate name for code coverage inspectable. The same name should be used for
// cases that are aggregated together.
std::string IRContext::GenerateCodeCoverageName(const Location& location, const CodeCoverageType codeCoverageType)
{
    std::string str = LocationToString(location) + "_" + std::to_string(_codeCoverageCounter) + "_" +
                      CodeCoverageTypeToString(codeCoverageType) + "Coverage";

    // Advance global code coverage counter
    _codeCoverageCounter++;

    return str;
}

// Call before Visit of condition expression to set up and enable condition coverage tracking
void IRContext::NewConditionCoverageTracker(CodeCoverageType codeCoverageType)
{
    assert(codeCoverageType == CodeCoverageType::Condition || codeCoverageType == CodeCoverageType::Expression);

    // Create a new condition coverage tracker
    ConditionCoverageTracker tracker = {};
    tracker._codeCoverageType = codeCoverageType;
    // Enable tracking
    tracker._conditionCoverageFlags._enabled = true;

    _conditionCoverageTrackerStack.push(tracker);
}

bool IRContext::HasConditionCoverageTracker() const { return !_conditionCoverageTrackerStack.empty(); }

ConditionCoverageTracker& IRContext::GetConditionCoverageTracker()
{
    assert(!_conditionCoverageTrackerStack.empty());
    ConditionCoverageTracker& tracker = _conditionCoverageTrackerStack.top();
    return tracker;
}

void IRContext::PopConditionCoverageTracker()
{
    assert(!_conditionCoverageTrackerStack.empty());
    _conditionCoverageTrackerStack.pop();
}

size_t IRContext::GetCallStackIndex()
{
    // _functionInstanceStack should only be empty
    // during unit tests
    assert(_expectEmptyFunctionInstanceStack == _functionInstanceStack.empty());

    // the first entry in this list is the outermost frame
    std::list<StackFrame> stackFrames;

    if (!_functionInstanceStack.empty())
    {
        FunctionInstanceEnumerator::InstanceAndLocation instAndLocation(
            _functionInstanceStack.back(), _functionInstanceStack.back()._functionNode->GetLocation());

        while (true)
        {
            // Add one frame to the call stack
            assert(instAndLocation.first._functionNode);
            stackFrames.push_front(
                StackFrame(instAndLocation.first.GetUnmangledObjectAndFunctionName(), instAndLocation.second));

            // Move to the next frame
            const std::optional<FunctionInstanceEnumerator::InstanceAndLocation> nextInst =
                g_compiler->GetFunctionInstanceEnumerator().GetCaller(instAndLocation.first);

            if (nextInst)
            {
                instAndLocation = *nextInst;
            }
            else
            {
                break;
            }
        }
    }

    CallStack callStack;

    for (const StackFrame& frame : stackFrames)
    {
        callStack.push(frame);
    }

    return _program->GetCallStackIndex(callStack);
}

// Returns true if the non-inline function being compiled
// must not perform operations that changes thread ordering (threads leave the function in a different order in which
// they appear)
bool IRContext::FunctionHasOrderingRestrictions()
{
    // If _reorderStackDepth > 0, then a reorder node is beign compiled
    // and so thread ordering will be restored before the function exits.
    return (_reorderStackDepth == 0) && DoesFunctionHaveOrderedRestrictions(_function->_functionNode->GetModifiers(),
                                                                            _function->_maxThreadCountInsideFunction);
}

size_t IRContext::GetConditionStackDepth() const { return _registerAccessNotificationStack.size(); }

// RAII class for IRContext::_returnValueRegisters
class PushPopReturnValueRegister
{
  public:
    PushPopReturnValueRegister(IRContext& context) : _context(context), _saved(context._returnValueRegisters) {}

    ~PushPopReturnValueRegister() { _context._returnValueRegisters = _saved; }

  private:
    PushPopReturnValueRegister* operator=(const PushPopReturnValueRegister&);

    IRContext& _context;
    const AllocatedRegister* _saved;
};

size_t Program::GetCallStackIndex(const CallStack& callStack)
{
    assert(_callStackToIndex.size() == _indexToCallStack.size());

    size_t result = 0;

    const auto it = _callStackToIndex.find(callStack);
    if (it == _callStackToIndex.end())
    {
        result = _callStackToIndex.size();
        SafeInsert(_callStackToIndex, callStack, result);
        SafeInsert(_indexToCallStack, result, callStack);
    }
    else
    {
        result = it->second;
    }

    return result;
}

// Used in cases where there are no inline calls, just 1 non-inline function
size_t Program::GetCallstackIndexForFunction(const Function& function, const Location& location)
{
    CallStack stack;
    stack.push(StackFrame(function._unmangledName, location));
    return GetCallStackIndex(stack);
}

const CallStack& Program::GetCallstackFromIndex(const size_t index) const
{
    return SafeLookup(_indexToCallStack, index);
}

CallStack Program::GetLimitedCallstackFromIndex(const size_t index) const
{
    CallStack fullStack = GetCallstackFromIndex(index);

    std::list<StackFrame> frames;

    const size_t frameLimit = GetCodeGenConfig()._maxStackDepth;

    for (size_t i = 0; i < frameLimit; i++)
    {
        if (fullStack.empty())
        {
            break;
        }
        else
        {
            frames.push_front(fullStack.top());
            fullStack.pop();
        }
    }

    CallStack result;

    for (const StackFrame& frame : frames)
    {
        result.push(frame);
    }

    assert(result.size() <= frameLimit);

    return result;
}

void ImplementMux(IRContext& context, const AllocatedRegister* const destRegister,
                  const AllocatedLeafRegister* const predicateRegister,
                  const std::vector<const AllocatedRegister*>& sourceRegisters)
{
    // Start a list of operations - filing in the destination register and predicate register
    OperationList operationList;

    {
        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        {
            Operation op = {};

            op._opcode = Opcode::Select;

            op._src.push_back(predicateRegister->GetAccessedRegister());
            op._dst.push_back(reg);

            operationList.push_back(op);
        };

        destRegister->VisitRegisters(callback);
    }

    // Now fill in source operands
    // This assumes that visitRegisters will have a consistent enumeration order, because types match
    for (const AllocatedRegister* const sourceRegister : sourceRegisters)
    {
        OperationList::iterator it = operationList.begin();

        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        {
            Operation& op = *it;

            op._src.push_back(reg);

            SignExtend(op, type, op._src.size() - 1);

            ++it;
        };

        sourceRegister->VisitRegisters(callback);

        assert(it == operationList.end());
    }

    // Add the operations to the basic block
    context._basicBlock->_operations.splice(context._basicBlock->_operations.end(), operationList);
}

void EvaluateExpressionIntoRegister(IRContext& context, const AllocatedRegister* const resultRegister,
                                    const ParseTreeNode* const expression)
{
    // Save context._writtenRegisters
    PushPopWrittenRegister pushPopWrittenReg(context);

    context._writtenRegisters = resultRegister;

    // Evaluate the expression
    expression->GenerateIR(context);

    // Add ConditionalIgnore operations to indicate that the expression
    // can be ignored if the predicate is false
    if (GetCodeGenConfig().ControlClockGatingEnabled() && context.IsPredicated())
    {
        resultRegister->VisitRegisters(
            [&](const AccessedRegister reg, const Type* const type)
            {
                if (RegisterType::Local == context._program->_registerTable[reg._registerIndex]._type)
                {
                    context.PredicatedIgnore(reg, expression->GetLocation());
                }
            });
    }
}

const AllocatedRegister* EvaluateExpression(IRContext& context, const ParseTreeNode* const expression)
{
    const Type* const type = expression->GetType();

    const AllocatedRegister* const allocatedRegister =
        type->AllocateRegisters(*context._program, RegisterType::Local, expression->GetExpressionString());

    EvaluateExpressionIntoRegister(context, allocatedRegister, expression);

    return allocatedRegister;
}

// Evaluate an expression into a single uint<*> register
const AllocatedLeafRegister* EvaluateAndFlattenExpression(IRContext& context, const ParseTreeNode* const expression)
{
    // Evaluate the source into 1 or more registers
    const AllocatedRegister* const srcReg = EvaluateExpression(context, expression);

    // Create a uint<*> register of the same width
    const AllocatedLeafRegister* const resultReg =
        AllocateLeafRegister(*context._program, BaseType::Uint, srcReg->GetType()->GetBitWidth(),
                             expression->GetLocation(), RegisterType::Local, expression->GetExpressionString());

    Operation op = {};

    op.InsertLocation(expression->GetLocation());

    op._opcode = Opcode::Gather;

    op._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();

    op._dst.push_back(resultReg->GetAccessedRegister());

    size_t currOffset = 0;

    const auto callback = [&](const AccessedRegister sourceReg, const Type* const sourceLeafType)
    {
        const size_t sourceWidth = sourceLeafType->GetBitWidth();

        GatherEntry gatherEntry = {};

        gatherEntry._sourceOffset = 0;
        gatherEntry._destOffset = currOffset;
        gatherEntry._numBits = sourceWidth;

        op._flags._gather._entries->push_back(gatherEntry);

        op._src.push_back(sourceReg);

        currOffset += sourceWidth;
    };

    // Move elements from srcReg into resultReg
    srcReg->VisitRegisters(callback);

    assert(currOffset == srcReg->GetType()->GetBitWidth());

    context._basicBlock->_operations.push_back(op);

    return resultReg;
}

// used for function arguments and return values to ensure that these are not optimized away by the per-function
// optimizer
void EvaluateCrossFunctionExpressionIntoRegister(IRContext& context, const AllocatedRegister* const resultRegisters,
                                                 const ParseTreeNode* const expression)
{
    // Evaluate the expression into new registers
    const AllocatedRegister* const expressionRegisters = EvaluateExpression(context, expression);

    // MovCrossFunction is used to ensure that the call index is not optimized away (the optimizer runs per-function)
    Move(context, resultRegisters, expressionRegisters, Opcode::MovCrossFunction, expression->GetLocation());
}

// assert arg = true (unless execution is predicated off)
void AddAssert(IRContext& context, const AccessedRegister& conditionAccessedRegisterIn, const std::string& errorString,
               const Location& location)
{
    AccessedRegister conditionAccessedRegister = conditionAccessedRegisterIn;

    Operation op = {};

    op._opcode = Opcode::Assert;

    op.InsertLocation(location);

    // Allocate permanent memory to hold the string
    std::string* errorMessage = g_compiler->Create<std::string>(errorString);

    op._flags._assertion._message = errorMessage->c_str();

    // conditionAccessedRegister == 1
    op._src.push_back(conditionAccessedRegister);
    op._src.push_back(1);

    if (context.IsPredicated())
    {
        DebuggerHintPushPredicate(context._basicBlock->_operations);

        const AccessedRegister predicate = {context.GetPredicate()};

        op._src.push_back(predicate);
    }

    context._basicBlock->_operations.push_back(op);

    if (context.IsPredicated())
    {
        DebuggerHintPopPredicate(context._basicBlock->_operations);
    }
}

Stage& AppendStage(BasicBlock* const basicBlock, const Location& location, bool useEndLine = false)
{
    // add 1 final stage
    Stage newStage = {};

    FileAndLineNumber fnum = LocationToFileAndLineNumber(location);
    if (useEndLine)
    {
        fnum._lineNumber = location._endLine;
    }

    newStage._fileAndLineNumbers.insert(fnum);

    basicBlock->_stages.push_back(newStage);

    return basicBlock->_stages.back();
}

// Note that if operationList should be for an appended stage
void EnqueueAndWriteParams(Program& program, OperationList& operationList, const size_t successorfifo,
                           const AllocatedRegister** valueRegisters, const size_t valueRegisterCount,
                           const Location& location)
{
    SetOperationLocation sol(operationList, location);

    if (valueRegisterCount)
    {
        // This is a return from an export function to the caller
        // Or a call from an external wrapper to an external function
        const RegisterDescription& fifoDesc = program._registerTable[successorfifo];

        size_t currOffset = 0;

        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        {
            const FifoSubset fifoSubset = {successorfifo, currOffset, type->GetBitWidth()};

            currOffset += type->GetBitWidth();

            // move the return value
            Operation op = {};

            op._opcode = Opcode::Mov;

            op._src.push_back(reg);
            op._dst.push_back(fifoSubset);
            op.InsertLocation(location);

            operationList.push_back(op);
        };

        for (size_t i = 0; i < valueRegisterCount; ++i)
        {
            // Move from valueRegisters[i] into the result FIFO
            valueRegisters[i]->VisitRegisters(callback);
        }

        assert(currOffset == fifoDesc._width);
    }

    // Add a jump
    {
        Operation op = {};

        op._opcode = Opcode::Enqueue;

        op._flags._enqueue._type = EnqueueType::Default;

        // Each thread can enqueue
        op._flags._enqueue._modifiers._callRate = 1;

        op._flags._enqueue._successorFifo = successorfifo;

        // Identify cross region (extern class entry/exit points) so user can floor-plan
        op._flags._enqueue._isReturn = true;

        // Returns from fixed-latency functions are allowed within atomic blocks
        // The compiler writes the entire function in an atomic block
        op._flags._enqueue._allowedInAtomic = true;

        operationList.push_back(op);
    }
}

void AppendEnqueue(Program& program, BasicBlock* const basicBlock, const size_t successorfifo,
                   const AllocatedRegister** valueRegisters, const size_t valueRegisterCount, const Location& location)
{
    // Add a final stage to the basic block
    Stage& lastStage = AppendStage(basicBlock, location, true);

    EnqueueAndWriteParams(program, lastStage._operations, successorfifo, valueRegisters, valueRegisterCount, location);
}

void AppendJump(BasicBlock* const basicBlock, BasicBlock* const next, const size_t whichFifo = 0)
{
    // Add a final stage to the basic block
    Stage& lastStage = AppendStage(basicBlock, next->_location);

    // Add a jump to next
    {
        Operation op = {};

        op._opcode = Opcode::Enqueue;

        op._getSuccessorBlock = [next]() { return next; };

        op._flags._enqueue._type = EnqueueType::Default;

        // Each thread can enqueue
        op._flags._enqueue._modifiers._callRate = 1;

        // For backward links - use the secondary fifo to avoid a fifo merger
        op._flags._enqueue._whichFifo = whichFifo;

        // Say where we jumped from
        op._locations.insert(basicBlock->_lastEmittedLineNumber);

        lastStage._operations.push_back(op);
    }
}

void AppendReorderingJump(BasicBlock* const basicBlock, BasicBlock* const next, const AccessedRegister& slotRegister,
                          const Location& location)
{
    // Add a final stage to the basic block
    Stage& lastStage = AppendStage(basicBlock, location);

    // Add a jump to next
    {
        Operation op = {};

        op.InsertLocation(location);

        op._opcode = Opcode::Enqueue;

        op._getSuccessorBlock = [next]() { return next; };

        op._flags._enqueue._type = EnqueueType::ReorderBuffer;

        // Add the slot register as a source of the enqueue
        // to ensure that the register is not optimized away
        op._src.push_back(slotRegister);

        // Each thread can enqueue
        op._flags._enqueue._modifiers._callRate = 1;

        op._flags._enqueue._reorderSlotRegister = slotRegister._registerIndex;

        lastStage._operations.push_back(op);
    }
}

void AppendConditionalJump(BasicBlock* const basicBlock, const size_t conditionRegisterIndex, BasicBlock* const lhs,
                           BasicBlock* const rhs, const Location& location,
                           const size_t whichFifoLhs = 0,                                    // for loops
                           const AllocatedLeafRegister* const reorderSlotRegister = nullptr) // for reordering loops
{
    const AccessedRegister conditionRegister{conditionRegisterIndex};

    // Add a final stage to the basic block
    Stage& lastStage = AppendStage(basicBlock, location, true);

    DebuggerHintPushPredicate(lastStage._operations);

    // Add a jump to lhs (if conditionRegister == 1)
    {
        Operation op = {};

        op._opcode = Opcode::Enqueue;

        op.InsertLocation(location);

        op._src.push_back(conditionRegister);

        op._flags._enqueue._isPredicated = true;
        op._flags._enqueue._predicateExecutionValue = true;
        op._flags._enqueue._type = EnqueueType::Default;
        op._flags._enqueue._whichFifo = whichFifoLhs;

        // Each thread can enqueue
        op._flags._enqueue._modifiers._callRate = 1;

        op._getSuccessorBlock = [lhs]() { return lhs; };

        lastStage._operations.push_back(op);
    }

    // Add a jump to rhs (if conditionRegister == 0)
    {
        Operation op = {};

        op._opcode = Opcode::Enqueue;

        op.InsertLocation(location);

        op._src.push_back(conditionRegister);

        op._flags._enqueue._isPredicated = true;
        op._flags._enqueue._predicateExecutionValue = false;
        op._flags._enqueue._type = EnqueueType::Default;

        // Each thread can enqueue
        op._flags._enqueue._modifiers._callRate = 1;

        op._getSuccessorBlock = [rhs]() { return rhs; };

        if (reorderSlotRegister)
        {
            // Mark the rhs enqueue as being a reordering operation
            op._flags._enqueue._type = EnqueueType::ReorderBuffer;

            const AccessedRegister slotRegister = reorderSlotRegister->GetAccessedRegister();

            // Add the slot register as a source of the enqueue
            // to ensure that the register is not optimized away
            op._src.push_back(slotRegister);

            op._flags._enqueue._reorderSlotRegister = slotRegister._registerIndex;
        }

        lastStage._operations.push_back(op);

        DebuggerHintPopPredicate(lastStage._operations);
    }
}

void AppendCallHelper(IRContext& context, BasicBlock* const basicBlock, const GetSuccessorBlock& getSuccessorBlock,
                      const FunctionNode* const calledFunction, BasicBlock* const returnBlock, const bool isPipelined,
                      const bool isPredicated, const AccessedRegister predicateValue, const std::string& objectName,
                      const CallModifiers& callParams, const Type* const callerReturnType, const size_t callSiteIndex,
                      const AllocatedLeafRegister* const loopCountRegisterForContextSaver, const size_t loopCountWidth,
                      ContextSaver** contextSaverOut, const Location& callSiteLocation)
{
    Program& program = *context._program;

    const FunctionNode::Instance& functionInstance = calledFunction->GetInstance(objectName);

    // Create a context saver object, which holds live variables while the function is executing
    ContextSaver contextSaver = {};

    // Use optimized ordered context saver where possible
    // if the call site is in a single-threaded function, then the ordered context saver can be used even if the callee
    // is unordered
    contextSaver._isOrdered = (0 == (calledFunction->GetModifiers() & ParseTreeFunctionModifierUnordered));
    contextSaver._isExternal = false;
    contextSaver._isPipelined = isPipelined;
    contextSaver._hasTransactionSize = callParams._transactionSize > 0;
    contextSaver._loopCounterWidth = loopCountWidth;

    contextSaver._callSiteIndex = callSiteIndex;

    contextSaver._beforeCall = basicBlock;
    contextSaver._afterCall = returnBlock;
    contextSaver._callee = calledFunction;
    contextSaver._calleeInstance = &functionInstance;

    // This register holds a hidden parameter which is the ID of the invocation - not needed for ordered functions
    const AllocatedLeafRegister* invocationInstanceIdRegister =
        contextSaver._isOrdered ? nullptr : functionInstance.GetInvocationInstanceRegister();

    contextSaver._invocationInstanceLocalRegister = invocationInstanceIdRegister;

    assert(!isPipelined || (loopCountRegisterForContextSaver != nullptr));
    contextSaver._loopCountLocalRegister = loopCountRegisterForContextSaver;

    // callerReturnType can be void when calledFunction is not
    // in the case of a pipelined function with a return value (that is ignored), and thread count is not known at
    // compile time
    if ((calledFunction->GetReturnType()->GetBitWidth() > 0) && (callerReturnType->GetBitWidth() > 0))
    {
        contextSaver._calleeReturnValueRegisters = functionInstance.GetCalleeReturnRegisters();

        if (isPipelined)
        {
            // The context saver will accumulate the scalar return values into an array
            contextSaver._callerReturnValueRegisters =
                callerReturnType->AllocateRegisters(*context._program, RegisterType::Local, "PipelinedReturnArray");
        }
        else
        {
            contextSaver._callerReturnValueRegisters = contextSaver._calleeReturnValueRegisters;
        }
    }

    contextSaver._callSiteLocation = callSiteLocation;

    contextSaver._callStackIndex = context.GetCallStackIndex();

    program._contextSavers.push_back(contextSaver);

    *contextSaverOut = &(program._contextSavers.back());

    Stage& stage0 = AppendStage(basicBlock, callSiteLocation);

    const auto getReturnBlock = [returnBlock]() { return returnBlock; };

    // Add jump to the return site (the function will jump there too)
    // This is remapped to a jump to the context saver caller input
    // It returns the allocated slot ID (for non-ordered context savers)
    {
        Operation op = {};

        op.InsertLocation(callSiteLocation);

        op._opcode = Opcode::Enqueue;

        op._getSuccessorBlock = getReturnBlock;

        op._flags._enqueue._type = EnqueueType::ContextSaverCaller;

        // Each thread can enqueue
        op._flags._enqueue._modifiers._callRate = 1;

        // The jump to the return site via the context saver can be predicated, in which case the context saver is not
        // called
        if (isPredicated)
        {
            DebuggerHintPushPredicate(stage0._operations);

            // Predication is implemented by setting the thread count to 0
            assert(loopCountRegisterForContextSaver);
        }

        // Add the loop count as a source operand
        // To ensure the loop count is not optimized away
        op._src.push_back(loopCountRegisterForContextSaver->GetAccessedRegister());

        if (!contextSaver._isOrdered)
        {
            op._dst.push_back(invocationInstanceIdRegister->GetAccessedRegister());
        }

        stage0._operations.push_back(op);
    }

    Stage* finalStage = &stage0;

    if (!contextSaver._isOrdered)
    {
        // Add another stage for the actual write  of the function parameters
        // This occurs after the enqueue to the context saver
        // to ensure that the invocationInstanceId register has been computed
        Stage& lastStage = AppendStage(basicBlock, calledFunction->GetLocation());

        finalStage = &lastStage;
    }

    if (isPredicated && !contextSaver._isOrdered)
    {
        // Predicated call to an unordered function
        // Add a jump to the return site not via the context saver
        // this handles the case where the function call is predicated away
        // This executes in stage 1, to ensure that the invocationInstanceIdRegister has been initialized
        // To ensure there are no uninitialized variables in the verilog code
        //
        // For ordered functions - the predicate is handled directly by the context saver - to keep the caller ordered
        assert(!contextSaver._isOrdered);

        Operation op = {};

        op.InsertLocation(callSiteLocation);

        op._opcode = Opcode::Enqueue;

        op._getSuccessorBlock = getReturnBlock;

        op._flags._enqueue._type = EnqueueType::FunctionCall;

        // Each thread can enqueue
        op._flags._enqueue._modifiers._callRate = 1;

        // Make the Enqueue operation predicated
        op._flags._enqueue._isPredicated = true;
        op._flags._enqueue._predicateExecutionValue = false;

        op._src.push_back(predicateValue);

        finalStage->_operations.push_back(op);
    }

    {
        Operation op = {};

        op._opcode = Opcode::Enqueue;

        op.InsertLocation(callSiteLocation);

        op._getSuccessorBlock = getSuccessorBlock;

        op._flags._enqueue._type = EnqueueType::FunctionCall;

        op._flags._enqueue._isPipelinedCall = isPipelined;

        op._flags._enqueue._modifiers = callParams;

        if (isPredicated)
        {
            // Make the Enqueue operation predicated
            op._flags._enqueue._isPredicated = true;
            op._flags._enqueue._predicateExecutionValue = true;

            op._src.push_back(predicateValue);
        }

        finalStage->_operations.push_back(op);
    }

    if (isPredicated)
    {
        DebuggerHintPopPredicate(finalStage->_operations);
    }
}

void AppendCall(IRContext& context, BasicBlock* const basicBlock, const GetSuccessorBlock& getSuccessorBlock,
                const FunctionNode* const calledFunction, BasicBlock* const returnBlock, const bool isPipelined,
                const std::string& objectName, const CallModifiers& callParams, const Type* const callerReturnType,
                const size_t callSiteIndex, const AllocatedLeafRegister* const loopCountRegisterForContextSaver,
                const size_t loopCountWidth, ContextSaver** contextSaverOut, const Location& callSiteLocation)
{
    AccessedRegister predicateRegister = {c_invalidAccessedRegisterIndex};

    if (context.IsPredicated())
    {
        predicateRegister._registerIndex = context.GetPredicate();
    }

    AppendCallHelper(context, basicBlock, getSuccessorBlock, calledFunction, returnBlock, isPipelined,
                     context.IsPredicated(), predicateRegister, objectName, callParams, callerReturnType, callSiteIndex,
                     loopCountRegisterForContextSaver, loopCountWidth, contextSaverOut, callSiteLocation);
}

const Operation* TryGetLastOperation(const BasicBlock* const basicBlock)
{
    const Operation* result = nullptr;

    if (!basicBlock->_stages.empty())
    {
        const Stage& lastStage = basicBlock->_stages.back();

        result = &(lastStage._operations.back());
    }

    return result;
}

Operation& GetLastOperation(BasicBlock* const basicBlock)
{
    // The final stage should have been created via AppendOperation
    assert(!basicBlock->_stages.empty());

    Stage& lastStage = basicBlock->_stages.back();

    return lastStage._operations.back();
}

const Operation& GetLastOperation(const BasicBlock* const basicBlock)
{
    return GetLastOperation(const_cast<BasicBlock*>(basicBlock));
}

BasicBlock* FinalizeBasicBlock(IRContext& context)
{
    BasicBlock* const basicBlock = context._basicBlock;

    // Should only be called once per basic block
    assert(basicBlock->_stages.empty());

    context._basicBlock = nullptr;

    return basicBlock;
}

std::pair<BasicBlock*, BasicBlock*> GenerateBasicBlocks(const ParseTreeNode* const statements, IRContext& context,
                                                        const std::function<void()>& finalizeCallback = nullptr)
{
    // There should be no pending basic block
    assert(context._basicBlock == nullptr);
    assert(context._writtenRegisters == nullptr);

    BasicBlock* const firstBasicBlock = context.CreateBasicBlock(statements->GetLocation());

    context._basicBlock = firstBasicBlock;

    // Save pointer to first basic block of function. Set this before IR
    // generation so a basic block can know if it is the first.
    context._function->_start = firstBasicBlock;

    assert(context._writtenRegisters == nullptr);

    statements->GenerateIR(context);

    if (finalizeCallback)
    {
        // Call a callback which can be used to add operations to context._basicBlock
        finalizeCallback();
    }

    // Finalize the last basic block
    BasicBlock* const lastBasicBlock = FinalizeBasicBlock(context);

    // Save pointer to last basic block of function
    context._function->_end = lastBasicBlock;

    return std::pair<BasicBlock*, BasicBlock*>(firstBasicBlock, lastBasicBlock);
}

// Called when a node should generate IR
// If the node is an expression, the value of the expression is not used
//
// For example, in this case:
// uint32 x = 3;
// x;
//
// The statement "x;" is legal, the evaluated expression is not used
void ParseTreeNode::GenerateStatementIR(IRContext& context) const
{
    assert(!context._writtenRegisters);

    EmitLineNumber(context._basicBlock, _location);

    // Generate IR for this expression, ignoring the results
    // This allows derived classes to assume that context._writtenRegisters != null
    GenerateIROptionalResult(context);
}

// Generates IR for this node
// If this node has a type, then the result of evaluating this node is stored into a register and returned
const AllocatedRegister* ParseTreeNode::GenerateIROptionalResult(IRContext& context) const
{
    const AllocatedRegister* result = nullptr;

    if (_type && (GetType()->GetBitWidth() > 0))
    {
        result = EvaluateExpression(context, this);
    }
    else
    {
        GenerateIR(context);
    }

    return result;
}

void ParseTreeNode::GenerateIR(IRContext& context) const
{
    // Not yet implemented for this specific sub-type
    assert(false);
}

void NodeList::GenerateIR(IRContext& context) const
{
    for (const ParseTreeNode* const child : _nodes)
    {
        child->GenerateStatementIR(context);
    }
}

void NestedScopeNode::GenerateIR(IRContext& context) const
{
    // New scope variables
    GenerateIRTypeContext::PushPopScope pushScope(context._typeContext);

    _body->GenerateStatementIR(context);
}

// Add/Remove specified modifiers
// Used to generate a top-level module for a particular export class
void FunctionNode::RemoveModifiers(const ParseTreeFunctionModifier modifiersToRemove)
{
    _modifiers = static_cast<ParseTreeFunctionModifier>(_modifiers & ~modifiersToRemove);
}

void FunctionNode::AddModifiers(const ParseTreeFunctionModifier modifiersToAdd)
{
    _modifiers = static_cast<ParseTreeFunctionModifier>(_modifiers | modifiersToAdd);
}

void FunctionNode::Reset()
{
    _instances.clear();

    _statements = _originalStatements;
}

void FunctionNode::AllocateRegisters(Program& program)
{
    // Get the set of objects that contain an instance of this function that is called
    // this - will have 1 entry for flat functions
    const std::vector<std::string> objectNames =
        g_compiler->GetFunctionInstanceEnumerator().GetObjectNamesForFunction(this);

    for (const std::string& objectName : objectNames)
    {
        // Object names should be unique
        assert(_instances.end() == _instances.find(objectName));

        Instance& instance = _instances[objectName];

        // Inline atomic index should start out with this value
        assert(std::numeric_limits<size_t>::max() == instance._inlineAtomicIndex);

        instance.SetParent(this, objectName);

        const bool isExport = (_modifiers & ParseTreeFunctionModifierExport);
        const bool isExtern = (_modifiers & ParseTreeFunctionModifierExternal);
        const bool isAsync = (_modifiers & ParseTreeFunctionModifierAsync);
        const bool isUnordered = (_modifiers & ParseTreeFunctionModifierUnordered);
        const bool isInline = (_modifiers & ParseTreeFunctionModifierInline);
        const bool isPipelined = (_modifiers & ParseTreeFunctionModifierPipelined);

        if (isPipelined)
        {
            // Add a hidden 1-bit parameter which is set to 1 if the thread count = 1
            instance._threadCountOneRegister =
                AllocateLeafRegister(program, BaseType::Uint, 1, _location, RegisterType::Local, "ThreadCountOne");
        }

        if (!isExport && !isAsync && !isInline)
        {
            // Synchronous, non-export call

            // Extern functions and functions with more than 1 call site require a call site index
            //
            // Functions with the [[reset]] attribute have an implicit call site (invoked only on restart)
            // callCount == 1 means the function has no explicit call sites
            const size_t callCount = g_compiler->GetFunctionInstanceEnumerator().GetFunctionCallCount(this, objectName);

            if (isExtern || (callCount > 1))
            {
                // Add a hidden parameter which is the caller index
                instance._callSiteIndexRegister = AllocateLeafRegister(program, BaseType::Uint,
                                                                       64, // width - adjusted later
                                                                       _location, RegisterType::Local, "CallIndex");

                // Debug symbols: flag the callIndex register for later addition to _sourceToPipelineRegisterMap
                const auto callback = [&](const AccessedRegister reg, const Type* const type)
                {
                    RegisterDescription& regDesc = program._registerTable[reg._registerIndex];

                    regDesc.SetRegisterSourceType(RegisterSourceType::Hidden);
                };

                instance._callSiteIndexRegister->VisitRegisters(callback);
            }

            if (isUnordered)
            {
                // Add a hidden parameter which is the invocation instance
                instance._invocationInstanceRegister =
                    AllocateLeafRegister(program, BaseType::Uint, GetCodeGenConfig().GetInvocationIndexSize(),
                                         _location, RegisterType::Local, "InvocationInstance");
            }
        }

        // Allocate register for return value
        const Type* const returnType = GetReturnType();

        const size_t returnTypeWidth = returnType->GetBitWidth();

        // Don't allocate a register for void functions
        if (returnTypeWidth > 0)
        {
            // If there is a node corresponding to the return statement
            // Get the register/variable name from it
            const std::string returnValueName =
                _returnNode ? _returnNode->GetExpressionString() : GetFlattenedName() + "_ReturnValue";

            SourceLocation sourceLocation = {static_cast<size_t>(GetLocation()._fileIndex),
                                                       static_cast<size_t>(GetLocation()._beginLine),
                                                       static_cast<size_t>(GetLocation()._beginColumn)};
            SourceVariable source(returnValueName, GetFlattenedName(), returnType->GetName(), returnTypeWidth,
                                            sourceLocation);

            instance._calleeReturnRegisters =
                returnType->AllocateRegisters(program, RegisterType::Local, returnValueName, ObjectPath(), source);

            // Debug symbols: flag this source for later addition to _sourceToPipelineRegisterMap
            const auto callback = [&](const AccessedRegister reg, const Type* const type)
            {
                RegisterDescription& regDesc = program._registerTable[reg._registerIndex];

                regDesc.SetRegisterSourceType(RegisterSourceType::Local);
            };

            instance._calleeReturnRegisters->VisitRegisters(callback);
        }
    }
}

void FunctionNode::GenerateIR(IRContext& context) const
{
    const bool isExport = (_modifiers & ParseTreeFunctionModifierExport);
    const bool isAsync = (_modifiers & ParseTreeFunctionModifierAsync);
    const bool isPipelined = (_modifiers & ParseTreeFunctionModifierPipelined);
    const bool isInline = (_modifiers & ParseTreeFunctionModifierInline);
    const bool isExtern = (_modifiers & ParseTreeFunctionModifierExternal);
    const bool isExportClassInterface = (_modifiers & ParseTreeFunctionModifierExportClassInterface);
    const bool isReset = (_modifiers & ParseTreeFunctionModifierReset);
    const bool isExternClassMethod = context._enclosingClassType->IsExternal();
    const bool isExternFixedLatency = _modifiers & ParseTreeFunctionModifierExternalExternallyInstantiated;
    const bool isPure = _modifiers & ParseTreeFunctionModifierPure;

    // All extern class methods must have isExtern
    assert(Implies(isExternClassMethod, isExtern));

    if (isExternFixedLatency)
    {
        // This function is a class callback with the [[latency()]] attribute
        // All necessary IR is generated at the call site
        // no need to generate any IR for teh function itself
        return;
    }

    if (isPure && !isExtern)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
            << "[[pure]] is only allowed on external functions";
    }

    if (isExport && isPipelined)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
            << "[[pipelined]] is not allowed on exported functions";
    }

    // These checks are done here rather than at type-checking time
    // to account for methods/callbacks of exported classes (which will have ParseTreeFunctionModifierExport added at
    // this time)
    if (_modifiers & ParseTreeFunctionModifierNoBackPressure)
    {
        // synchronous extern [[no_backpressure]] functions are not allowed
        // because there could be backpressure from the return site
        if (((_modifiers & ParseTreeFunctionModifierExternal) == ParseTreeFunctionModifierExternal) &&
            ((_modifiers & ParseTreeFunctionModifierAsync) == 0))
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
                << "[[no_backpressure]]/[[latency]] callbacks must also have the [[async]] attribute.";
        }
    }

    if (isExtern && (_classType != g_compiler->GetGlobalClassType()) && !_classType->IsExternal() &&
        !isExportClassInterface)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidExternFunction)
            << "Methods of non-external classes must have implementations";
    }

    // Inline (non-atomic) functions generate IR at the call site
    // If a function is not reachable, don't bother generating IR for it
    if (isInline)
    {
        return;
    }

    if (isExport || isExtern)
    {
        // Export all interface types
        for (size_t i = 0; i < GetParameterCount(); i++)
        {
            ExportTypeHelper(*context._program, GetParameterDeclareNode(i)->GetDeclaredTypeNode(),
                             GetParameterDeclareNode(i)->GetDeclaredType(), ExportTypeBehavior::Default);
        }

        ExportTypeHelper(*context._program, GetReturnTypeNode(), GetReturnType(), ExportTypeBehavior::Default);
    }

    assert(!context._objectNameStack.empty());
    const std::list<FunctionInstance> instances =
        g_compiler->GetFunctionInstanceEnumerator().LookupMethodInstances(this, context._objectNameStack.top());

    const std::set<FunctionInstance>& externClassCallees =
        g_compiler->GetFunctionInstanceEnumerator().GetExternalClassInstanceCallees();

    for (const FunctionInstance thisFunctionInstance : instances)
    {
        // Separate scopes for each function instances
        std::unique_ptr<GenerateIRTypeContext::PushPopScope> pushScope =
            std::make_unique<GenerateIRTypeContext::PushPopScope>(context._typeContext);

        // Note that this object name will be different than context._objectNameStack.top()
        // if this function has reference parameters.  There is a separate instance for each unique set of reference
        // parameters
        const std::string& objectName = thisFunctionInstance._objectName;

        // 1 is passed for minimumCallCount to ensure that IR generation runs for functions
        // that have 0 call sites - to allow error checking to run and inform the user of errors
        // in functions with 0 call sites
        const bool isInlineable = g_compiler->GetFunctionInstanceEnumerator().IsInlineable(thisFunctionInstance, 1);

        if (isInlineable)
        {
            // Auto-inlined function
            // IR will be generated at the call
            continue;
        }

        // Even if a function is not reachable, function instance enumeration should still enumerate all non-inline
        // functions This is done to ensure that errors are reported for unreachable code
        assert(HasInstance(objectName));

        const bool isReachable = g_compiler->GetFunctionInstanceEnumerator().IsInstanceReachable(thisFunctionInstance);

        const bool isExternClassCallee = externClassCallees.end() != externClassCallees.find(thisFunctionInstance);

        // The only case where a function should not be reachable
        // is it if is external, and IR generation runs to generate a consistent interface
        // for the top-level module
        assert(isReachable || isExtern);

        // Record the function instance that is being generated
        // InstanceIndex is set to 0, because this function is not inline
        assert(context._functionInstanceStack.empty());
        PushPopFunctionInstance pushPopFunctionInstance(context, thisFunctionInstance);

        // AllocateRegisters should have allocated instances
        Instance& instance = const_cast<Instance&>(GetInstance(objectName));

        // Nested functions should not have parsed
        assert(!context._function);
        assert(!context._returnValueRegisters);
        assert(!context._callSiteIndexRegister);
        assert(!context._invocationInstanceRegister);
        assert(context._basicBlock == nullptr);
        assert(!context.IsPredicated());
        assert(0 == context._inlineCallCount);

        // Allocate function record
        context._program->_functions.push_back(Function());
        Function& function = context._program->_functions.back();

        context._function = &function;

        function._functionNode = this;
        function._objectName = objectName;
        function._classType = context._enclosingClassType;
        function._scope = context._typeContext.GetNamespaceScope();
        function._name = GetName();
        function._unmangledName = GetUnmangledName();
        function._start = nullptr;
        function._end = nullptr;
        function._returnFifoRegisterIndex = c_invalidAccessedRegisterIndex;
        function._maxThreadCountInsideFunction = GetFunctionMaxThreadCount(
            _modifierList, _location,
            g_compiler->GetFunctionInstanceEnumerator().GetFunctionCallCount(this, objectName));
        function._functionThreadRate = _threadRate;
        function._inlineAtomicIndex = instance._inlineAtomicIndex;
        function._numCallSites = 0;
        function._externClassInstance =
            context._externClassInstances.empty() ? nullptr : context._externClassInstances.top();

        // IR generation runs even for unreachable functions - to enable errors to be reported in unreachable code
        // Unreachable functions are removed from the IR before optimization
        function._isReachable = isReachable;

        // Create an output FIFO for synchronous export functions
        // and functions with the [[reset]] attribute (to know when the reset has completed)
        // and functions which are callable by an extern class callback
        if (((isExport || isExternClassCallee) && !isAsync) || isReset)
        {
            // 0 for void functions
            const size_t fifoWidth = GetReturnType()->GetBitWidth();

            function._returnFifoRegisterIndex =
                AllocateRegister(context._program, fifoWidth, RegisterType::Fifo, GetName() + "_ReturnValue");

            context._program->_registerTable[function._returnFifoRegisterIndex].Fifo().SetLocation(
                _location, context.GetCallStackIndex());

            // If there is a ReturnNode in the function, use the location of the ReturnNode
            const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
            {
                const ReturnNode* const returnNode = dynamic_cast<const ReturnNode*>(node);

                if (returnNode)
                {
                    context._program->_registerTable[function._returnFifoRegisterIndex].Fifo()._location =
                        returnNode->GetLocation();
                }

                recurseCallback();
            };

            VisitContext visitContext = {};
            VisitContext::PushPopScope outerScope(visitContext);
            Visit(callback, visitContext);
        }

        // Add parameters to symbol table
        AddParameterSymbols(context, nullptr);

        // Remember the call site index, so that return expressions can return to the correct caller
        // Note that this will be null if there is only 1 call site
        context._callSiteIndexRegister = instance._callSiteIndexRegister;

        // Remember the invocation instance register, so that return expressions can return with the correct live
        // variables in the caller This is null in for ordered functions
        context._invocationInstanceRegister = instance._invocationInstanceRegister;

        // Save the return value register
        instance.SetReturnValueRegister(context);

        std::unique_ptr<PushPopAtomicStackDepth> pushPopAtomicStackDepth;

        const bool isFixedLatency = IsFixedLatency();

        if (isFixedLatency)
        {
            // Increment the atomic stack depth in the context
            // Begn/End atomic operations are added later
            pushPopAtomicStackDepth = std::make_unique<PushPopAtomicStackDepth>(context);
        }

        std::pair<BasicBlock*, BasicBlock*> bodyBlocks;

        {
            // For extern functions (like when compiling a call into an export class)
            // Use an empty list for the function body
            const ParseTreeNode* statements = _statements;

            if (isExtern)
            {
                // Ensure that statements and bodyBlocks will use _location
                SetParseLocation spl(_location);

                statements = g_compiler->Create<NodeList>();
            }

            // Pop the scope while context._basicBlock is non-null
            bodyBlocks = GenerateBasicBlocks(statements, context, [&]() { pushScope.reset(); });
        }

        if (_modifiers & ParseTreeFunctionModifierNoBackPressure)
        {
            // Check the IR for things that could cause backpressure
            if (function._start->HasStartCondition() || (function._start != function._end))
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
                    << "[[no_backpressure]] cannot be used with control flow (rolled loops, calls to "
                       "non-inline functions)";
                throw std::runtime_error("Invalid function modifier");
            }

            // Check for async function calls
            const auto callback = [&](Operation& op)
            {
                if (CanOperationBackpressure(op))
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
                        << "[[no_backpressure]] cannot be used with control calls to non-inline functions";
                }
            };

            ForEachOperationForward(*bodyBlocks.first, callback);
        }

        if (!isExtern)
        {
            std::string debugViewLabel;

            if (objectName != g_globalObjectName)
            {
                debugViewLabel = objectName;
                debugViewLabel += "_";
            }

            debugViewLabel += function._name;

            // Parameter Count/Return type are checked
            // to avoid emitting empty debug view modules, which will be reordered in confusing ways
            if (GetParameterCount() > 0)
            {
                // Add debug view corresponding to function entrance
                std::stringstream str;
                str << debugViewLabel << "Entry";

                DebugViewBuilder debugViewBuilder(context, str.str(), _location);

                for (size_t i = 0; i < GetParameterCount(); ++i)
                {
                    debugViewBuilder.AppendArgument(instance.GetParameterRegisters(i), GetParameterName(i));
                }

                debugViewBuilder.InsertOp(bodyBlocks.first->_operations, bodyBlocks.first->_operations.begin());
            }

            if (GetReturnType()->GetBitWidth() > 0)
            {
                // Add debug view corresponding to function exit
                std::stringstream str;
                str << debugViewLabel << "Exit";

                DebugViewBuilder debugViewBuilder(context, str.str(), _location);
                debugViewBuilder.AppendArgument(instance.GetCalleeReturnRegisters(), "ReturnValue");

                debugViewBuilder.InsertOp(bodyBlocks.second->_operations, bodyBlocks.second->_operations.end());
            }
        }

        // Ensure the function semaphore is acquired and released
        // so that FIFOs do not overflow
        // If the NoBackpressure attribute is present (in library mode), then no thread count semaphore is needed
        // there will be no FIFOs in the function
        // Semaphores are not added to fixed latency functions because they have no control flow
        if (!isExtern && !NoBackpressure() && !IsFixedLatency())
        {
            AddSemaphore(*context._program, function, *bodyBlocks.first, *bodyBlocks.second);
        }

        if ((function._returnFifoRegisterIndex != c_invalidAccessedRegisterIndex) && !isReset)
        {
            // Export, synchronous function
            // Add an operation that will enqueue into the return FIFO
            //
            // For functions with the [[reset]] attribute, _returnFifoRegisterIndex will be valid
            // but the return will be handled conditionally like other conditional returns
            // because multiple returns are possible
            const std::pair<Location, Location> lineBounds = GetLineBounds();
            AppendEnqueue(*context._program, bodyBlocks.second, context._function->_returnFifoRegisterIndex,
                          &context._returnValueRegisters, context._returnValueRegisters ? 1 : 0, lineBounds.first);
        }

        context._function = nullptr;
        context._basicBlock = nullptr;

        if (isExport)
        {
            // Add an entrypoint to the program
            EntryPoint entryPoint = {};

            entryPoint._classType = function._classType;
            entryPoint._scope = function._scope;
            entryPoint._functionName = function._name;
            entryPoint._backendName = function.GetBackendName();

            entryPoint._instances.push_back(&function);

            context._program->_entryPoints.push_back(entryPoint);
        }

        const NodeList* const parameters = dynamic_cast<const NodeList*>(_params);

        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        { bodyBlocks.first->_liveInReg.push_back(reg._registerIndex); };

        // Record the set of registers that should be considered live-in at the start of the first basic block

        for (size_t i = 0; i < parameters->Children().size(); ++i)
        {
            const AllocatedRegister* const paramRegs = instance.GetParameterRegisters(i);

            paramRegs->VisitRegisters(callback);
        }

        // Also, the hidden call index register is live-in
        if (instance._callSiteIndexRegister)
        {
            bodyBlocks.first->_liveInReg.push_back(
                dynamic_cast<const AllocatedLeafRegister*>(instance._callSiteIndexRegister)
                    ->GetAccessedRegister()
                    ._registerIndex);
        }

        // Also, the hidden invocation instance register is live-in
        if (instance._invocationInstanceRegister)
        {
            bodyBlocks.first->_liveInReg.push_back(
                dynamic_cast<const AllocatedLeafRegister*>(instance._invocationInstanceRegister)
                    ->GetAccessedRegister()
                    ._registerIndex);
        }

        // Also, the hidden "is thread count == 1" register is live-in (used by the loop generator)
        if (instance._threadCountOneRegister)
        {
            bodyBlocks.first->_liveInReg.push_back(
                dynamic_cast<const AllocatedLeafRegister*>(instance._threadCountOneRegister)
                    ->GetAccessedRegister()
                    ._registerIndex);
        }

        instance._function = &function;

        context._returnValueRegisters = nullptr;
        context._callSiteIndexRegister = nullptr;
        context._invocationInstanceRegister = nullptr;

        // loop generation for exports occurs in the command processor
        if (isPipelined && !isExport)
        {
            const AccessedRegister counterReg = instance.GetLoopCounterRegister()->GetAccessedRegister();

            const AccessedRegister threadCountOneReg = instance.GetThreadCountOneRegister()->GetAccessedRegister();

            // Allocate loop-generator object
            LoopGenerator loopGenerator = {};

            loopGenerator._initialized = false;
            loopGenerator._function = &function;
            loopGenerator._counterLocalRegisterIndex = counterReg._registerIndex;
            loopGenerator._counterWidth = context._program->_registerTable[counterReg._registerIndex]._width;
            loopGenerator._threadCountOneRegisterIndex = threadCountOneReg._registerIndex;

            context._program->_loopGenerators.push_back(loopGenerator);
        }

        if (isExtern)
        {
            context._program->_externFunctions.push_back(&function);

            // Add an opcode to the IR to ensure that the basic block is not optimized away
            // The back-end ignores the IR and generates a specific module to handle the external call
            Operation op = {};

            op.InsertLocation(_location);

            op._opcode = Opcode::ExternalPlaceholder;

            bodyBlocks.first->_operations.push_back(op);
        }

        if (isFixedLatency)
        {
            // Implement desired latency by wrapping all operations in {Begin,End}Atomic
            assert(1 == function._basicBlocks.size());
            BasicBlock& basicBlock = function._basicBlocks.front();

            AtomicBlockDesc desc = {};

            desc._type = AtomicBlockType::Default;
            desc._updateRate =
                GetLatency() + 1; // latency(N) means that the generate code can spread over N+1 pipeline stages
            desc._chainIndex = context.AllocateAtomicChainIndex();
            desc._location = _location;

            {
                Operation op = {};

                op.InsertLocation(_location);

                op._opcode = Opcode::BeginAtomic;
                op._flags._atomicBlockDesc = desc;

                basicBlock._operations.push_front(op);
            }

            {
                Operation op = {};

                op.InsertLocation(_location);

                op._opcode = Opcode::EndAtomic;
                op._flags._atomicBlockDesc = desc;

                basicBlock._operations.push_back(op);
            }
        }

        for (BasicBlock& bb : function._basicBlocks)
        {
            // Assert that all operations with an area cost have an associated line/file
            AssertUnscheduledOperationsHaveLocations(*context._program, bb);
        }
    }
}

// Used to count threads for inline pipelined calls
class ThreadCounter
{
  public:
    ThreadCounter(IRContext& context, const mp_int& targetThreadCount, const Location& location)
        : _context(context), _targetThreadCount(targetThreadCount),
          _threadCountWidth(std::max<size_t>(1, Log2RoundUp(targetThreadCount))), _location(location)
    {
        // Allocate a global register which is used to track the number of threads which have finished
        const size_t threadCountWidth = Log2RoundUp(targetThreadCount);

        _globalCountRegister._registerIndex =
            AllocateRegister(*_context._program, _threadCountWidth, RegisterType::Global, "ForkJoinThreadCounter");

        // Ensure the thread count register is initialized to 0
        RegisterDescription& joinCountRegDesc = _context._program->_registerTable[_globalCountRegister._registerIndex];
        joinCountRegDesc.Global()._hasInitialValue = true;
        joinCountRegDesc.Global()._initialValue = 0;
    }

    void Prolog()
    {
        // Assign locations to generated operations
        SetOperationLocation sol(_context, _location);

        // Compute the thread count plus one
        const AccessedRegister threadCountPlusOneRegister = {
            AllocateRegister(*_context._program, _threadCountWidth, RegisterType::Local, "ThreadCountPlusOne")};

        {
            Operation op = {};

            op._opcode = Opcode::BinaryOp;

            op._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

            op._src.push_back(_globalCountRegister);
            op._src.push_back(SourceOperand(1));

            op._dst.push_back(threadCountPlusOneRegister);

            _context._basicBlock->_operations.push_back(op);
        }

        // Compare the thread count against the target thread count - 1
        // -1 trick is for timing purposes, this compare can run in parallel with the add above
        assert(_targetThreadCount > 0);

        _threadCountReachedReg._registerIndex =
            AllocateRegister(*_context._program, 1, RegisterType::Local, "MaxThreadCountReached");

        {
            Operation op = {};

            op._opcode = Opcode::BinaryOp;
            op._flags._binaryOpType = ParseTreeBinaryOpTypeEQ;

            op._src.push_back(_globalCountRegister);
            op._src.push_back(SourceOperand(_targetThreadCount - 1));
            op._dst.push_back(_threadCountReachedReg);

            _context._basicBlock->_operations.push_back(op);
        }

        // If the target has been reached, the join count will be reset to 0
        // Otherwise, the joint count will be incremented
        _newThreadCountRegister._registerIndex =
            AllocateRegister(*_context._program, _threadCountWidth, RegisterType::Local, "NewThreadCount");

        {
            Operation op = {};

            op._opcode = Opcode::Select;

            // Index
            op._src.push_back(_threadCountReachedReg);

            op._src.push_back(threadCountPlusOneRegister);
            op._src.push_back(SourceOperand(0));

            op._dst.push_back(_newThreadCountRegister);

            _context._basicBlock->_operations.push_back(op);
        }
    }

    void Epilog()
    {
        // Assign locations to generated operations
        SetOperationLocation sol(_context, _location);

        // Write the new join count back to the global
        {
            Operation op = {};

            op._opcode = Opcode::WriteGlobal;

            op._flags._writeGlobal._isPredicated = _context.IsPredicated();

            op._src.push_back(_newThreadCountRegister);

            op._dst.push_back(_globalCountRegister);

            if (_context.IsPredicated())
            {
                DebuggerHintPushPredicate(_context._basicBlock->_operations);

                const AccessedRegister predicateReg = {_context.GetPredicate()};

                op._src.push_back(predicateReg);
            }

            _context._basicBlock->_operations.push_back(op);

            if (_context.IsPredicated())
            {
                DebuggerHintPopPredicate(_context._basicBlock->_operations);
            }
        }
    }

    const AccessedRegister& GetThreadCountReachedRegister() { return _threadCountReachedReg; }

    const AccessedRegister& GetThreadCountRegister() { return _globalCountRegister; }

  private:
    IRContext& _context;
    const mp_int _targetThreadCount;
    const size_t _threadCountWidth;
    const Location _location;

    AccessedRegister _globalCountRegister = {};
    AccessedRegister _newThreadCountRegister = {};
    AccessedRegister _threadCountReachedReg = {};
};

void FunctionNode::GenerateInlineIR(IRContext& context, const std::string& objectName,
                                    const std::vector<KnownValue>& parameterKnownValues) const
{
    const Instance& instance = GetInstance(objectName);

    // Call site should be within a function
    assert(context._function);
    assert(context._basicBlock != nullptr);

    // Update the object this basicBlock is associated with
    if (objectName != g_globalObjectName)
    {
        context._basicBlock->_objectNames.insert(objectName);
    }

    // inline pipelined is not supported
    assert(!(_modifiers & ParseTreeFunctionModifierPipelined));

    KnownValue threadCountKnownValue;

    {
        // Save context._writtenRegister
        PushPopWrittenRegister pushPopWrittenReg(context);

        context._writtenRegisters = nullptr;

        // Push the object name onto the context stack
        // So that parameters know which object IR is being generated for
        PushPopObjectNameStack pushObjectName(context, objectName);

        // Push the class type onto the stack
        PushPopClassType pushClassType(context, _classType);

        // Push a new scope for class members
        // Note that hideAbove is set to true - to prevent the callee from accessing local variables
        // in the caller
        GenerateIRTypeContext::PushPopScope pushScope1(context._typeContext, nullptr, true);

        // If this is a member function of a class, add member variables to the symbol table
        if (_classType != g_compiler->GetGlobalClassType())
        {
            const std::vector<ClassType::EntryType>& memberVariables = _classType->GetMemberVariables();

            for (const ClassType::EntryType& memberVariable : memberVariables)
            {
                memberVariable.second->GenerateIR(context);
            }
        }

        // Push a new scope for parameters and local variables
        GenerateIRTypeContext::PushPopScope pushScope2(context._typeContext);

        // Add parameters to symbol table
        AddParameterSymbols(context, &parameterKnownValues);

        // Save and restore context._returnValueRegister
        PushPopReturnValueRegister pushReturnValue(context);

        // Save the return value register
        instance.SetReturnValueRegister(context);

        // Increment the inline call depth - this is used by ReturnNode::GenerateIR to know that it should not jump
        PushPopInlineCallCount pushPopInlineCallCount(context, 1);

        _statements->GenerateIR(context);

    } // context._writtenRegisters restored here

    if (context._writtenRegisters && (GetReturnType()->GetBitWidth() > 0))
    {
        // non-pipelined call.  Move return value to the correct location
        Move(context, context._writtenRegisters, instance.GetCalleeReturnRegisters(), Opcode::Mov, _location);
    }
}

void FunctionNode::AddParameterSymbols(IRContext& context,
                                       const std::vector<KnownValue>* const parameterKnownValues) const
{
    const NodeList* const parameters = dynamic_cast<const NodeList*>(_params);

    const std::vector<const ParseTreeNode*>& children = parameters->Children();

    if (parameterKnownValues)
    {
        assert(children.size() == parameterKnownValues->size());
    }

    for (size_t i = 0; i < children.size(); i++)
    {
        const DeclareNode* const parameter = dynamic_cast<const DeclareNode*>(children[i]);

        // GenerateIR* will add parameters to the symbol table
        if (parameterKnownValues)
        {
            parameter->GenerateIRImpl(context, parameterKnownValues->data() + i);
        }
        else
        {
            parameter->GenerateIR(context);
        }
    }
}

// Used to null dangling pointers to return blocks
void FunctionNode::HandleRemovedBasicBlocks(const std::set<const BasicBlock*>& removedBlocks) const
{
    for (auto& p : _instances)
    {
        const Instance& instance = p.second;

        instance.HandleRemovedBasicBlocks(removedBlocks);
    }
}

// Used to null dangling pointers to return blocks
void FunctionNode::Instance::HandleRemovedBasicBlocks(const std::set<const BasicBlock*>& removedBlocks) const
{
    for (auto& p : _returnBlocks)
    {
        const BasicBlock* const block = p.second;

        if (removedBlocks.end() != removedBlocks.find(block))
        {
            p.second = nullptr;
        }
    }
}

void FunctionNode::Instance::SetReturnValueRegister(IRContext& context) const
{
    context._returnValueRegisters = _calleeReturnRegisters;
}

size_t FunctionNode::Instance::AddCaller(const size_t maxCallerThreadCount)
{
    const size_t result = _callerCount;

    ++_callerCount;

    _totalCallerThreadCount += maxCallerThreadCount;

    return result;
}

void FunctionNode::Instance::SetReturnBlock(const size_t index, BasicBlock* const basicBlock)
{
    _returnBlocks[index] = basicBlock;
}

Function* FunctionNode::Instance::GetFunction() const { return _function; }

bool FunctionNode::Instance::HasCallSiteIndex() const
{
    // Async, export, inline, and calls with only 1 call site do not have a call site index
    return _callSiteIndexRegister != nullptr;
}

const AllocatedLeafRegister* FunctionNode::Instance::GetCallSiteIndexRegister() const
{
    assert(_callSiteIndexRegister);

    return dynamic_cast<const AllocatedLeafRegister*>(_callSiteIndexRegister);
}

const AllocatedLeafRegister* FunctionNode::Instance::GetInvocationInstanceRegister() const
{
    assert(_invocationInstanceRegister);

    return dynamic_cast<const AllocatedLeafRegister*>(_invocationInstanceRegister);
}

const AllocatedRegister* FunctionNode::Instance::GetCalleeReturnRegisters() const
{
    assert(_calleeReturnRegisters);

    return _calleeReturnRegisters;
}

const AllocatedLeafRegister* FunctionNode::Instance::GetLoopCounterRegister() const
{
    assert(_parent->GetModifiers() & ParseTreeFunctionModifierPipelined);

    return dynamic_cast<const AllocatedLeafRegister*>(GetParameterRegisters(0));
}

const AllocatedLeafRegister* FunctionNode::Instance::GetThreadCountOneRegister() const
{
    assert(_threadCountOneRegister);

    return dynamic_cast<const AllocatedLeafRegister*>(_threadCountOneRegister);
}

const AllocatedRegister* FunctionNode::Instance::GetParameterRegisters(const size_t index) const
{
    const std::vector<const ParseTreeNode*>& parameters = dynamic_cast<const NodeList*>(_parent->_params)->Children();

    assert(index < parameters.size());

    const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(parameters[index]);

    return declareNode->GetRegisters(_objectName);
}

// Note that this can return null for return blocks that have been optimized away
BasicBlock* FunctionNode::Instance::GetReturnBlock(const size_t index) const
{
    const auto it = _returnBlocks.find(index);
    assert(it != _returnBlocks.end());

    return it->second;
}

size_t FunctionNode::Instance::GetCallSiteCount() const { return _callerCount; }

size_t FunctionNode::Instance::GetTotalCallerThreadCount() const { return _totalCallerThreadCount; }

size_t FunctionNode::Instance::GetInlineAtomicIndex() const
{
    assert(_inlineAtomicIndex != std::numeric_limits<size_t>::max());
    return _inlineAtomicIndex;
}

void DeclareNode::Reset()
{
    _allocatedRegisterMap.clear();

    _staticInstanceId = 0;
}

// Called after type checking, but before GenerateIR is called on any node
void DeclareNode::AllocateRegisters(Program& program)
{
    if (IsStatic())
    {
        if (ContainsClassType())
        {
            assert(_allocatedRegisterMap.empty());

            // Static local objects are allocated now
            // Consult the function enumerator to determine how many objects should be allocated for this declaration
            const std::list<size_t> uniqueIds =
                g_compiler->GetFunctionInstanceEnumerator().GetAllStaticLocalIndices(this);

            const RegisterType registerType = RegisterType::Global;

            for (const size_t id : uniqueIds)
            {
                const std::string instanceName = GetStaticLocalInstanceNameWithoutObject(id);

                SetAllocatedRegisterForObject(AllocateRegistersInternal(program, registerType, g_globalObjectName,
                                                                        instanceName, nullptr, ObjectPath()),
                                              instanceName);
            }
        }
        // static local variables that are not objects are allocated in GenerateIRImpl
    }
    else
    {
        // Registers for member variables are allocated when the containing object is declared
        if (_declarationScope != DeclarationScope::Member)
        {
            assert(_allocatedRegisterMap.empty());

            // For variables that are a part of a class, get list of objects of that class
            // For variables that are not part of a class, this list will contain 1 entry
            // unless the function has referece parameters, in which case it will have 1 entry
            // per unique set of reference parameters
            assert(_classType);
            const std::vector<std::string> objectNames =
                _containingFunction
                    ? g_compiler->GetFunctionInstanceEnumerator().GetObjectNamesForFunction(_containingFunction)
                    : _classType->GetObjectNames();

            // If this is a global declaration, mark the register as not pipelined
            const RegisterType registerType =
                (_declarationAccess == DeclarationAccess::Shared) ? RegisterType::Global : RegisterType::Local;

            for (const std::string& objectName : objectNames)
            {
                const ObjectPath objectPath =
                    (registerType == RegisterType::Global) ? g_compiler->ObjectNameToPath(objectName) : ObjectPath();

                SetAllocatedRegisterForObject(
                    AllocateRegistersInternal(program, registerType, objectName, "", nullptr, objectPath), objectName);
            }
        }
    }
}

const AllocatedRegister* DeclareNode::AllocateRegistersInternal(Program& program, const RegisterType registerType,
                                                                const std::string& objectName,
                                                                const std::string& instanceNamePrefix,
                                                                SourceContainer* container,
                                                                const ObjectPath& containerPath) const
{
    const Type* const type = GetDeclaredType();

    std::string registerName;

    if ((_declarationScope == DeclarationScope::Member) || (_declarationScope == DeclarationScope::Global))
    {
        registerName = GetCombinedName(objectName);
    }
    else
    {
        // Local variables (static and not)
        // do not have object name included.
        // This improves code readability
        registerName = GetDeclaredName();
    }

    if (!instanceNamePrefix.empty())
    {
        registerName = CombineObjectAndMemberName(Scope(), instanceNamePrefix, registerName);
    }

    const std::string scopeName = (container) ? GetCombinedParseNamespaceScopeName(container->_scope)
                                              : GetCombinedParseNamespaceScopeName(objectName);

    // Get source variable information; to be added to newly created register
    SourceLocation sourceLocation = {static_cast<size_t>(GetLocation()._fileIndex),
                                               static_cast<size_t>(GetLocation()._beginLine),
                                               static_cast<size_t>(GetLocation()._beginColumn)};
    SourceVariable source(GetFlattenedName(), scopeName, GetDeclaredType()->GetName(),
                                    GetDeclaredType()->GetBitWidth(), sourceLocation);
    if (container)
    {
        container->_members.push_back(
            {GetFlattenedName(), GetDeclaredType()->GetName(), 0, GetDeclaredType()->GetBitWidth()});
        source._container = container;
    }

    ObjectPath path = AppendToPath(containerPath, GetDeclaredName());

    // For static objects, prepend the instance name to the container name
    if (!instanceNamePrefix.empty())
    {
        assert(path.size() == 1);
        path.back() = instanceNamePrefix + path.back();
    }

    // Allocate a register for this variable
    const AllocatedRegister* result = type->AllocateRegisters(program, registerType, registerName, path, source);

    if (RegisterType::Global == registerType)
    {
        // If _isConst is true, then mark all registers as constant
        // Initialize strings without a user-specified initial value to the null handle
        // to ensure they member variables can be freed
        // on first assignment
        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        {
            RegisterDescription& regDesc = program._registerTable[reg._registerIndex];

            if (_isConst)
            {
                if (regDesc._type == RegisterType::Global)
                {
                    assert(!regDesc.Global()._isConstant);
                    regDesc.Global()._isConstant = true;
                }
                else
                {
                    assert(regDesc._type == RegisterType::Memory);
                }
            }
            else if (type == g_compiler->GetStringType())
            {
                if (!regDesc.Global()._hasInitialValue)
                {
                    regDesc.Global()._hasInitialValue = true;
                    regDesc.Global()._initialValue = 0;
                }
            }
        };

        result->VisitRegisters(callback);
    }

    return result;
}

// For member variable declarations in classes, stores the AllocatedRegister* associated with a particular object
// For global variables, stores the AllocatedRegister* associated with the global variable
void DeclareNode::SetAllocatedRegisterForObject(const AllocatedRegister* allocatedRegister,
                                                const std::string& objectName)
{
    // There should be no duplicates
    assert(_allocatedRegisterMap.end() == _allocatedRegisterMap.find(objectName));

    _allocatedRegisterMap[objectName] = allocatedRegister;
}

void DeclareNode::SetKnownValue(KnownValueContext& knownValueContext,
                                const KnownValue* const inlineCallKnownValue) const
{
    KnownValue knownValue;

    if (inlineCallKnownValue && !_isModifiedParameter)
    {
        // The variable is a parameter of an inline function call
        // and the argument is known at the call site
        // and the parameter is not modified by the body of the function
        knownValue = *inlineCallKnownValue;
    }
    else if (_isConst)
    {
        if (_assignNode)
        {
            // const type name = expr;

            // The value of the variable may be known at compile time
            knownValue = _assignNode->GetRhs()->TryGetKnownValue(knownValueContext, GetDeclaredType());
        }
    }

    knownValueContext.SetSymbolKnownValue((_declarationScope == DeclarationScope::Global) ? _parseNamespaceScope
                                                                                          : Scope(),
                                          GetDeclaredName(), knownValue, _location);
}

// Note that this code does not run for declare nodes inside of structs
// It does run for declare nodes inside of classes
void DeclareNode::GenerateIR(IRContext& context) const
{
    // Generate IR without assuming that the variable has a known value from an inline function call
    GenerateIRImpl(context, nullptr);
}

bool DeclareNode::ContainsClassType() const
{
    bool containsClassType = false;

    const auto visitCallback = [&containsClassType](const Type* const type)
    {
        const ClassType* const classType = dynamic_cast<const ClassType*>(type);

        if (classType != nullptr)
        {
            containsClassType = true;
        }
    };

    GetDeclaredType()->VisitTypes(visitCallback, VisitTypesBehavior::Default);

    return containsClassType;
}

void DeclareNode::GenerateIRImpl(IRContext& context, const KnownValue* inlineCallKnownValue) const
{
    const Type* const type = GetDeclaredType();

    // Get the name of the object that this variable is a member of
    // The member variable check is needed for classes contained within functions
    // Do not use the object name associated with the function
    const std::string objectName =
        (_declarationScope == DeclarationScope::Member) ? context._objectNameStack.top() : context.GetObjectName();

    // Add the register index to the symbol table
    GenerateIRVariableData variableData = {};

    variableData._declarationScope = _declarationScope;

    variableData._name = GetDeclaredName();

    variableData._isConst = _isConst;

    // If a constant shared variable is initialized to a non-empty string
    // then the global register in the IR associated with it cannot be used
    // because backends do not support non-empty strings as initial values
    // for global registers.
    // Set GenerateIRVariableData::_constantInitialValueNode
    // which will cause accesses to this variable to simply re-evaluate
    // the initial value expression
    const auto typeContainsString = [this]()
    {
        bool result = false;

        const auto callback = [&](const Type* const containedType)
        {
            if (dynamic_cast<const StringType*>(containedType))
            {
                result = true;
            }
        };

        GetDeclaredType()->VisitTypes(callback, VisitTypesBehavior::SkipReferences);

        return result;
    };

    const auto initialValueReferencesNonConstVariable = [this]()
    {
        bool result = false;

        const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
        {
            const VariableAccessNode* const var = dynamic_cast<const VariableAccessNode*>(node);

            if (var && !var->IsKnownConstInitializer())
            {
                result = true;
            }

            recurseCallback();
        };

        VisitContext visitContext = {};
        VisitContext::PushPopScope outerScope(visitContext);
        _assignNode->GetRhs()->Visit(callback, visitContext);

        return result;
    };

    if (_isConst && (_declarationScope != DeclarationScope::Local) && _assignNode && typeContainsString())
    {
        // uint32 a = 3;
        // const string s = "{a}"
        // <later on in a function>:
        // a = 4;
        // println(s)
        //
        // In this case, the interpolation expression
        // cannot be re-evaluated when the println occurs
        // because it will evaluate to "4" rather than "3"
        if (initialValueReferencesNonConstVariable())
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidGlobalAssignment)
                << "Constant shared variables which contain strings cannot be initialized to a value which references "
                   "a mutable variable";
        }

        variableData._constantInitialValueNode = _assignNode->GetRhs();
    }

    // If true, then this is local variable that can be re-allocated each time
    // GenerateIR is called
    //
    // Parameters do not go through this path because GenerateIR is called once
    // regardless of the number of call sites to an inline function
    //
    // Loop induction variables do not go through this path because
    // GenerateIR is called once for a loop (not once per iteration)
    const bool nonStaticLocal =
        ((_declarationScope == DeclarationScope::Local) && (_declarationAccess == DeclarationAccess::Private)) &&
        !_isParameter && !_isUninitConst;

    if (IsStatic())
    {
        assert(!inlineCallKnownValue);

        // Determine if this declaration is for an object, or an array of objects
        // If so, then the registers have already been allocated - look them up
        if (ContainsClassType())
        {
            // Static local object, AllocateRegistersInternal was already called (during pre-ir-generation
            // AllocateRegisters pass)

            // Lookup a unique ID for this static local object
            const size_t uniqueId = g_compiler->GetFunctionInstanceEnumerator().LookupStaticLocalObjectIndex(
                this, context._functionInstanceStack.back());

            variableData._allocatedRegisters = GetRegisters(GetStaticLocalInstanceNameWithoutObject(uniqueId));
        }
        else
        {
            // Append function name to the global/static variable when it is not mapped to memory
            // If appending function names to memory,it may introduce long initialization file names
            // exceeding what OS supports and generate a runtime error.
            // This will make the generated RTL more readable when the same local static variable name is used in
            // multiple functions
            const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(GetDeclaredType());
            std::ostringstream functionNamePrefix;
            if (memoryType == nullptr)
            {
                std::list<FunctionInstance>::const_iterator fi;
                for (fi = context._functionInstanceStack.begin(); fi != std::prev(context._functionInstanceStack.end());
                     fi++)
                {
                    const std::string functionName = fi->_functionNode->GetName();
                    functionNamePrefix << functionName << "_";
                }
            }
            // For non-class static locals, allocate a new value each time GenerateIRImpl is called
            const std::string fullName = (objectName != g_globalObjectName)
                                             ? objectName + "_" + functionNamePrefix.str()
                                             : functionNamePrefix.str();
            variableData._allocatedRegisters = AllocateRegistersInternal(
                *context._program, RegisterType::Global, g_globalObjectName, fullName, nullptr, ObjectPath());

            // Ensure registers associated with this instance have unique names in the IR.
            // Inlining and unrolling can cause this declaration to correspond to many instances
            const std::string suffix = "_" + std::to_string(_staticInstanceId);
            _staticInstanceId++;

            const auto callback = [&](const AccessedRegister reg, const Type* const type)
            {
                RegisterDescription& regDesc = context._program->_registerTable[reg._registerIndex];

                regDesc._name += suffix;

                switch (regDesc._type)
                {
                case RegisterType::Global:
                    assert(!regDesc.Global()._containerInstancePath.empty());
                    regDesc.Global()._containerInstancePath.back() += suffix;
                    break;

                case RegisterType::Memory:
                    assert(!regDesc.Memory()._containerInstancePath.empty());
                    regDesc.Memory()._containerInstancePath.back() += suffix;
                    break;

                default:
                    assert(false);
                }
            };

            variableData._allocatedRegisters->VisitRegisters(callback);
        }
    }
    else if (nonStaticLocal)
    {
        // Allocate fresh register each time GenerateIR is called for a non-static local variable
        // This elimiates WAW and WAR hazards for local variables declared inside of unrolled_for loops
        // The code that schedules atomic blocks does not perform renaming, and
        // can allow more cases if no WAW and WAR hazards are present in the IR
        variableData._allocatedRegisters =
            AllocateRegistersInternal(*context._program, RegisterType::Local, objectName, "", nullptr, ObjectPath());
    }
    else
    {
        variableData._allocatedRegisters = GetRegisters(objectName);
    }

    if (_declarationAccess == DeclarationAccess::Private)
    {
        // Record the depth of the condition stack
        // to determine which conditional operations apply to this declaration
        // For parameters of inline functions, this changes the depth at each call
        const_cast<AllocatedRegister*>(variableData._allocatedRegisters)
            ->SetConditionStackDepth(context.GetConditionStackDepth());
    }

    variableData._type = type;

    // Initially mark the variable as not known at compile time
    variableData._value._type = KnownValueType::None;

    // If the declaration is for a non-static local variable, then prevent it from being visible
    // to inline functions called by the function that contains this declaration
    assert(!IsStatic() || (_declarationAccess == DeclarationAccess::Shared)); // static implies shared

    // Members declared in outer class to be visible in inner class
    const bool hideFromInlineCall = (_declarationScope == DeclarationScope::Local);

    context._typeContext.AddSymbol(GetDeclaredName(), variableData, _location, AddSymbolBehavior::Default,
                                   hideFromInlineCall);

    // Mark the variable as known at compile time if possible
    SetKnownValue(context, inlineCallKnownValue);

    if (_shouldInitialize)
    {
        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        {
            // memory types do not support initialization
            if (type->SupportsInitialization())
            {
                RegisterDescription& regDesc = context._program->_registerTable[reg._registerIndex];

                if (regDesc._type == RegisterType::Local)
                {
                    assert(context._basicBlock);

                    // Add an operation which will set the initial value of the variable
                    // Note that for string handles, a handle value of 0 is considered to be NULL
                    // and it is OK to pass that handle in ReferenceString (it will be a NOP)
                    Operation op = {};

                    op._opcode = Opcode::Clear;

                    op.InsertLocation(_location);

                    op._dst.push_back(reg);

                    context._basicBlock->_operations.push_back(op);
                }
                else
                {
                    // Global variable is initialized during reset
                }
            }
        };

        variableData._allocatedRegisters->VisitRegisters(callback);
    }

    // Flag sources (locals, globals, memories) for later addition to _sourceToPipelineRegisterMap
    const auto callback = [&](const AccessedRegister reg, const Type* const type)
    {
        RegisterDescription& regDesc = context._program->_registerTable[reg._registerIndex];

        regDesc.SetRegisterSourceType(RegisterSourceType::Local);

        if (_declarationAccess == DeclarationAccess::Shared)
        {
            assert(regDesc._type == RegisterType::Global || regDesc._type == RegisterType::Memory);

            context._program->_objectNameToGlobals[objectName].insert(reg._registerIndex);

            if (_declarationScope != DeclarationScope::Local)
            {
                // Shared variable (not static local)
                // The name should be unique within the program
                // because the source language requires unique names for
                // variables and all containing object names are part of the
                // register name
                regDesc._uniqueName = true;
            }
        }
    };
    variableData._allocatedRegisters->VisitRegisters(callback);

    if ((_declarationScope == DeclarationScope::Local) && (_declarationAccess == DeclarationAccess::Private))
    {
        // Local variable (for function parameter), release any contained strings when the variable goes out of scope
        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        {
            if (g_compiler->GetStringType() == type)
            {
                const AccessedRegister stringHandleReg = reg;

                const bool isPredicated = context.IsPredicated();

                const AccessedRegister predicateReg = {isPredicated ? context.GetPredicate()
                                                                    : c_invalidAccessedRegisterIndex};

                IRContext* const contextPtr = &context;

                context._typeContext.OnLeaveCurrentScope(
                    [this, stringHandleReg, contextPtr, isPredicated, predicateReg]()
                    {
                        Operation op = {};

                        op.InsertLocation(_location);

                        op._opcode = Opcode::ReferenceString;

                        if (isPredicated)
                        {
                            op._src.push_back(predicateReg);
                        }
                        else
                        {
                            // Literal 1 predicate value
                            op._src.push_back(SourceOperand(1, 1));
                        }

                        op._src.push_back(stringHandleReg);

                        op.PushOperand(c_stringReferenceNegOne, true);

                        assert(contextPtr->_basicBlock);
                        contextPtr->_basicBlock->_operations.push_back(op);
                    });
            }
        };

        variableData._allocatedRegisters->VisitRegisters(callback);
    }
}

const AllocatedRegister* DeclareNode::GetRegisters(const std::string& objectName) const
{
    const auto it = _allocatedRegisterMap.find(objectName);
    assert(it != _allocatedRegisterMap.end());

    const AllocatedRegister* const result = it->second;

    return result;
}

void AssignNode::GenerateIR(IRContext& context) const
{
    // Cannot have nested assignments
    assert(context._writtenRegisters == nullptr);
    const LValNode* const storeNode = dynamic_cast<const LValNode*>(_lhs);

    const auto isCompileTimeAssignment = [](const ParseTreeNode* const node)
    {
        const ReferenceType* const referenceType = dynamic_cast<const ReferenceType*>(node->GetType());
        const FunctionType* const functionType = dynamic_cast<const FunctionType*>(node->GetType());

        return (referenceType != nullptr) || (functionType != nullptr);
    };

    if (isCompileTimeAssignment(_lhs) && isCompileTimeAssignment(_rhs))
    {
        // Reference/callback tracking is done during function instance enumeration
        // no IR is generated to assign a reference to a reference
        return;
    }

    // (context._basicBlock == true) && (_isGlobalInit) occurs for initialization of static variables
    // these should be treated as globals
    if (context._basicBlock && !_isGlobalInit)
    {
        // Assign locations to generated operations
        SetOperationLocation sol(context, _location);

        const bool isNarrowing = _lhs->GetType()->GetBitWidth() < _rhs->GetType()->GetBitWidth();

        const AllocatedRegister* storeSrcReg = nullptr;

        if (!_isInitialAssignment && isNarrowing && GetCodeGenConfig()._assertNarrowingConversion)
        {
            // Evaluate the right-hand-side at the width of the right-hand-side
            const AllocatedRegister* const rhsReg = EvaluateExpression(context, _rhs);

            // Allocate a register of the left-hand-side type
            storeSrcReg =
                _lhs->GetType()->AllocateRegisters(*context._program, RegisterType::Local, _rhs->GetExpressionString());

            // Move data from the wide to the narrow register
            Move(context, storeSrcReg, rhsReg, Opcode::Mov, _location);

            // Move data from narrow to another wide register
            const AllocatedRegister* const roundTripReg =
                _rhs->GetType()->AllocateRegisters(*context._program, RegisterType::Local, _rhs->GetExpressionString());

            Move(context, roundTripReg, storeSrcReg, Opcode::Mov, _location);

            // Assert that round-tripped wide register contents match the original wide register contents
            const auto callback = [&](const AccessedRegister lhs, const Type* const lhsType, const AccessedRegister rhs,
                                      const Type* const rhsType)
            {
                Operation op = {};

                op._opcode = Opcode::Assert;

                std::ostringstream message;
                message << "Source value of narrowing conversion cannot be represented by the result type.\n";
                message << "Source type: " << _rhs->GetType()->GetName() << ".\n";
                message << "Result type: " << _lhs->GetType()->GetName() << ".\n";
                message << "Conversion at: " << g_compiler->GetSourceFileNameWithoutLeadingPath(_location._fileIndex)
                        << " line: " << _location._beginLine << "\n";
                GenerateCallTrace(message, context);

                std::string* errorMessage = g_compiler->Create<std::string>(message.str());

                op._flags._assertion._message = errorMessage->c_str();

                op.PushOperand(lhs, IsSignedLeafType(lhsType));
                op.PushOperand(rhs, IsSignedLeafType(rhsType));

                if (context.IsPredicated())
                {
                    const AccessedRegister predicate = {context.GetPredicate()};

                    op._src.push_back(predicate);
                }

                context._basicBlock->_operations.push_back(op);
            };

            roundTripReg->VisitRegisters(rhsReg, callback);
        }
        else
        {
            // If the left-hand-side is narrower than the right-hand-side
            // then when the right-hand-side is evaluated, it only needs to produces narrower width
            const Type* const evalType = isNarrowing ? _lhs->GetType() : _rhs->GetType();

            storeSrcReg =
                evalType->AllocateRegisters(*context._program, RegisterType::Local, _rhs->GetExpressionString());

            // Evaluate the right-hand-side into an AllocatedRegister
            EvaluateExpressionIntoRegister(context, storeSrcReg, _rhs);
        }

        const auto storeCallback = [&](IRContext& context, const AllocatedRegister* const dstReg)
        { LValNode::StoreImpl(context, dstReg, storeSrcReg, _location); };

        storeNode->Store(context, storeCallback);
    }
    else
    {
        // global variable, set the initialization value rather than emitting operations
        assert(_isGlobalInit);

        const ClassType* const rhsClassType = dynamic_cast<const ClassType*>(_rhs->GetType());

        const DesignatedInitializerListNode* const rhsDesignatedInitializerList =
            dynamic_cast<const DesignatedInitializerListNode*>(_rhs);

        // Initializing a class with an empty InitializerListNode is fine
        // The parse tree API arbitrarily chooses to use InitializerListNode for empty initializer lists
        const InitializerListNode* const rhsInitializerList = dynamic_cast<const InitializerListNode*>(_rhs);

        if (rhsClassType && !rhsDesignatedInitializerList &&
            !(rhsInitializerList && rhsInitializerList->Children().empty()))
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidGlobalAssignment)
                << "Objects can only be initialized with a designated initializer list";
        }

        storeNode->Initialize(context, _rhs);
    }
}

void LValNode::StoreRegister(IRContext& context, const AllocatedRegister* const dest,
                             const AllocatedRegister* const src, const Location& location)
{
    const bool isGlobal = dest->GetRegisterType() == RegisterType::Global;

    if (isGlobal || context.IsPredicated())
    {
        DebuggerHintPushPredicate(context._basicBlock->_operations);

        // Move from the temp registers to variableRegisters
        const auto callback = [&](const AccessedRegister destRegister, const Type* const lhsType,
                                  const AccessedRegister srcRegister, const Type* const rhsType)
        {
            if (isGlobal)
            {
                // Add an operation which updates the global variable
                Operation op = {};

                op._opcode = Opcode::WriteGlobal;

                op._src.push_back(srcRegister);
                op._dst.push_back(destRegister);

                if (context.IsPredicated())
                {
                    op._flags._writeGlobal._isPredicated = true;

                    const AccessedRegister predicateReg = {context.GetPredicate()};

                    op._src.push_back(predicateReg);
                }

                // Ensure sign-extension works correctly
                SignExtend(op, rhsType, 0);

                context._basicBlock->_operations.push_back(op);
            }
            else if (context.IsPredicated())
            {
                // Notify conditional update objects about write to a local variable
                assert(!context._registerAccessNotificationStack.empty());

                const size_t destConditionStackDepth = dest->GetConditionStackDepth();

                for (RegisterAccessNotification* const r : context._registerAccessNotificationStack)
                {
                    r->NotifyWrite(destRegister._registerIndex, destConditionStackDepth);
                }
            }
            else
            {
                assert(false);
            }
        };

        dest->VisitRegisters(src, callback);

        DebuggerHintPopPredicate(context._basicBlock->_operations);
    }

    if (!isGlobal)
    {
        Move(context, dest, src, Opcode::Mov, location);
    }
}

// Stores value into dstRegisters
void LValNode::StoreImpl(IRContext& context, const AllocatedRegister* const dstRegisters,
                         const AllocatedRegister* const srcRegisters, const Location& location)
{
    const bool isGlobal = dstRegisters->GetRegisterType() == RegisterType::Global;

    // Release the reference on the previous string, and add a reference to the new string.
    // Note that a string handle of 0 is the null handle
    // and adjusting the reference count a null handle is a nop.
    for (size_t i = 0; i < 2; i++)
    {
        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        {
            if (g_compiler->GetStringType() == type)
            {
                Operation op = {};

                op.InsertLocation(location);

                op._opcode = Opcode::ReferenceString;

                PushPredicateOrLiteralOne(context, op);

                op._src.push_back(reg);

                op.PushOperand((i == 0) ? c_stringReferenceNegOne : Literal({1, 1}), i == 0);

                context._basicBlock->_operations.push_back(op);
            }
        };

        if (i == 0)
        {
            dstRegisters->VisitRegisters(callback);
        }
        else
        {
            assert(i == 1);
            srcRegisters->VisitRegisters(callback);
        }
    }

    if (context.IsPredicated() || isGlobal)
    {
        // Predicated write to dstRegisters
        StoreRegister(context, dstRegisters, srcRegisters, location);
    }
    else
    {
        // Mov into dstRegisters
        Move(context, dstRegisters, srcRegisters, Opcode::Mov, location);
    }
}

std::string LValNode::GetObjectName(KnownValueContext& context, const std::string& containingObjectName,
                                    const ResolveReferenceFunction& resolveReferenceCallback) const
{
    static_assert(27 == static_cast<size_t>(CompileError::InvalidCall), "Invalid enum");
    throw std::runtime_error("Error 27: Invalid object reference");
}

void VariableAccessNode::CheckGlobalRestrictions(IRContext& context) const
{
    const GenerateIRVariableData variableData = context._typeContext.LookupSymbol(
        _scopedIdentifierNode->GetScope(), _scopedIdentifierNode->GetName(), _location);

    if (!variableData._isConst && (variableData._declarationScope == DeclareNode::DeclarationScope::Global) &&
        !context._program->_isDefaultPass)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidExportClass)
            << "Global variables cannot be accessed (directly or indirectly) from an exported class";

        throw std::runtime_error("Invalid variable access");
    }
}

const AllocatedRegister* VariableAccessNode::Load(IRContext& context) const
{
    CheckGlobalRestrictions(context);

    const GenerateIRVariableData variableData = context._typeContext.LookupSymbol(
        _scopedIdentifierNode->GetScope(), _scopedIdentifierNode->GetName(), _location);

    if (variableData._constantInitialValueNode)
    {
        return EvaluateExpression(context, variableData._constantInitialValueNode);
    }
    else
    {
        return variableData._allocatedRegisters;
    }
}

void VariableAccessNode::Store(IRContext& context, const StoreCallback& storeCallback) const
{
    CheckGlobalRestrictions(context);

    const GenerateIRVariableData variableData = context._typeContext.LookupSymbol(
        _scopedIdentifierNode->GetScope(), _scopedIdentifierNode->GetName(), _location);

    if (!_isKnownConstInitialSet)
    {
        if (variableData._value._type == KnownValueType::Int)
        {
            static_assert(61 == static_cast<size_t>(CompileError::AssignmentToConst), "Invalid enum");
            g_compiler->ErrorStream(_location, CompileError::AssignmentToConst)
                << "Invalid assignment. The variable `" << GetName()
                << "` is a constant and can't be modified after definition.";
            throw std::runtime_error("Error 61: Invalid assignment to a const variable");
        }
    }

    const ReferenceType* const referenceType =
        dynamic_cast<const ReferenceType*>(variableData._allocatedRegisters->GetType());

    const AllocatedRegister* containerRegisters = variableData._allocatedRegisters;

    if (referenceType)
    {
        // Called when 1 member within a reference is written
        // Dereference
        const std::string objectName = g_compiler->GetFunctionInstanceEnumerator().GetReferencedObject(
            this, context._functionInstanceStack.back());

        const ClassType* const referencedClassType = dynamic_cast<const ClassType*>(referenceType->_referencedType);
        assert(referencedClassType);

        containerRegisters = referencedClassType->GetRegistersForObject(objectName);
    }

    storeCallback(context, containerRegisters);
}

void VariableAccessNode::Initialize(IRContext& context, const ParseTreeNode* const rhs) const
{
    const GenerateIRVariableData variableData = context._typeContext.LookupSymbol(
        _scopedIdentifierNode->GetScope(), _scopedIdentifierNode->GetName(), _location);

    variableData._allocatedRegisters->Initialize(context, rhs);
}

void VariableAccessNode::GenerateIR(IRContext& context) const
{
    // Assign locations to generated operations
    SetOperationLocation sol(context, _location);

    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    const AllocatedRegister* const sourceRegisters = Load(context);

    // Add operations to the basic block which moves from sourceRegisters to context._writtenRegisters
    Move(context, context._writtenRegisters, sourceRegisters, Opcode::Mov, _location);
}

KnownValue VariableAccessNode::TryGetKnownValueImpl(KnownValueContext& context) const
{
    // The call to SetSymbolKnownValue ensures that the literal value has the
    // width of the variable type
    return context.LookupKnownValueForSymbol(_scopedIdentifierNode->GetScope(), _scopedIdentifierNode->GetName(),
                                             _location);
}

std::string VariableAccessNode::GetObjectName(KnownValueContext& context, const std::string& containingObjectName,
                                              const ResolveReferenceFunction& resolveReferenceCallback) const
{
    std::string result = _scopedIdentifierNode->GetName();

    if (dynamic_cast<const ReferenceType*>(GetType()))
    {
        // Change from reference variable name ("this") to name of referenced object
        result = resolveReferenceCallback(result);
    }

    return result;
}

struct AddrAndOffsetRegs
{
    const AllocatedLeafRegister* _partitionIndex;
    const AllocatedLeafRegister* _offsetWithinPartition;
};

void EmitStoreMemory(IRContext& context, const MemoryType* const memoryType,
                     const AllocatedLeafRegister* const memoryRegister, const AllocatedRegister* const sourceRegisters,
                     const AllocatedLeafRegister* const indexRegister)
{
    // Update write count
    RegisterDescription& regDesc =
        context._program->_registerTable[memoryRegister->GetAccessedRegister()._registerIndex];
    assert(regDesc._type == RegisterType::Memory);

    // Allocate table of source widths (to handle implicit bit-width conversion)
    std::vector<size_t>* sourceWidths = g_compiler->Create<std::vector<size_t>>();

    // An an operation that stores all of sourceRegisters into the memory
    Operation op = {};

    op._opcode = Opcode::StoreMemory;

    op._flags._storeMemory._isPredicated = context.IsPredicated();
    op._flags._storeMemory._sourceWidths = sourceWidths;

    // Add the index
    op._src.push_back(indexRegister->GetAccessedRegister());

    if (context.IsPredicated())
    {
        const AccessedRegister predicateReg = {context.GetPredicate()};

        // Add the predicate
        op._src.push_back(predicateReg);
    }

    // Add all parts of the source as sources
    {
        const auto callback = [&](const AccessedRegister reg, const Type* const type) { op._src.push_back(reg); };

        sourceRegisters->VisitRegisters(callback);
    }

    // Save widths of the elements in the memory, for implicit width conversion
    {
        const AllocatedRegister* const memoryTypeRegisters =
            memoryType->_elementType->AllocateRegisters(*context._program, RegisterType::Local, "MemoryTypeReg");

        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        { sourceWidths->push_back(type->GetBitWidth()); };

        memoryTypeRegisters->VisitRegisters(callback);
    }

    const size_t firstSource = context.IsPredicated() ? 2 : 1;

    assert(sourceWidths->size() == (op._src.size() - firstSource));

    op._dst.push_back(memoryRegister->GetAccessedRegister());

    context._basicBlock->_operations.push_back(op);
}

void ArrayAccessNode::Store(IRContext& context, const StoreCallback& storeCallbackIn) const
{
    const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(_array->GetType());

    if (memoryType)
    {
        // Evaluate the index into a register
        // It is important that EvaluateIndexRegister is not called
        // Because that could return a global register, which won't work for stores to memories in an atomic block
        const AllocatedLeafRegister* const indexRegister =
            dynamic_cast<const AllocatedLeafRegister*>(EvaluateExpression(context, _indexNode));

        const auto callback = [&](IRContext& context, const AllocatedRegister* const reg)
        {
            // Evaluate the value into a register
            const AllocatedRegister* const sourceRegisters = memoryType->_elementType->AllocateRegisters(
                *context._program, RegisterType::Local, "mem_write_data_" + GetExpressionString());

            // Ensure sourceRegisters are initialized
            ClearAllocatedRegister(context, sourceRegisters, _location);

            // Predication can be ignored when writing to sourceRegisters because sourceRegisters is a temporary
            // allocated by the compiler
            storeCallbackIn(context, sourceRegisters);

            if (context.IsPredicated())
            {
                DebuggerHintPushPredicate(context._basicBlock->_operations);
            }

            EmitStoreMemory(context, memoryType, dynamic_cast<const AllocatedLeafRegister*>(reg), sourceRegisters,
                            indexRegister);

            if (context.IsPredicated())
            {
                DebuggerHintPopPredicate(context._basicBlock->_operations);
            }

            // Release string references held by sourceRegisters
            const auto callback = [&](const AccessedRegister reg, const Type* const type)
            {
                if (g_compiler->GetStringType() == type)
                {
                    Operation op = {};

                    op.InsertLocation(_location);

                    op._opcode = Opcode::ReferenceString;

                    PushPredicateOrLiteralOne(context, op);

                    op._src.push_back(reg);

                    op.PushOperand(c_stringReferenceNegOne, true);

                    context._basicBlock->_operations.push_back(op);
                }
            };

            sourceRegisters->VisitRegisters(callback);
        };

        dynamic_cast<const LValNode*>(_array)->Store(context, callback);
    }
    else
    {
        if (IsIndexKnown(context))
        {
            // The index is known by the compiler
            const size_t index = GetKnownIndex(context);

            const auto callback = [&](IRContext& context, const AllocatedRegister* const reg)
            {
                const AllocatedArrayRegister* const containerRegisters =
                    dynamic_cast<const AllocatedArrayRegister*>(reg);

                assert(index < containerRegisters->_elements.size());

                storeCallbackIn(context, containerRegisters->_elements[index]);
            };

            dynamic_cast<const LValNode*>(_array)->Store(context, callback);
        }
        else
        {
            // The index is dynamic
            // Put the index into a register
            const AccessedRegister indexRegister = {GetIndexRegister(context, IndexAccessMode::Write)};

            const auto callback = [&](IRContext& context, const AllocatedRegister* const reg)
            {
                const AllocatedArrayRegister* const containerRegisters =
                    dynamic_cast<const AllocatedArrayRegister*>(reg);

                // Assign locations to generated operations
                SetOperationLocation sol(context, _location);

                // For each element in the result array
                for (size_t i = 0; i < containerRegisters->_elements.size(); ++i)
                {
                    const AccessedRegister conditionRegister = {AllocateRegister(
                        *context._program, 1, RegisterType::Local, "array_write_enable_" + GetExpressionString())};

                    // Check to see if i == combinedIndexRegister
                    {
                        Operation op = {};

                        op._opcode = Opcode::BinaryOp;
                        op._flags._binaryOpType = ParseTreeBinaryOpTypeEQ;

                        op._src.push_back(indexRegister);
                        op._src.push_back(SourceOperand(i));
                        op._dst.push_back(conditionRegister);

                        // Ensure index is sign-extended if necessary
                        SignExtend(op, _indexNode->GetType(), 0);

                        context._basicBlock->_operations.push_back(op);
                    }

                    // Do a predicated store to the destination
                    ConditionalLocalUpdates conditionalLocalUpdates(context, _location);

                    conditionalLocalUpdates.BeginCase(conditionRegister._registerIndex,
                                                      ConditionalLocalUpdates::PredicateMode::Normal);

                    storeCallbackIn(context, containerRegisters->_elements[i]);

                    conditionalLocalUpdates.EndCase();
                }
            };

            dynamic_cast<const LValNode*>(_array)->Store(context, callback);
        }
    }
}

void ArrayAccessNode::Initialize(IRContext& context, const ParseTreeNode* const rhs) const
{
    dynamic_cast<const LValNode*>(_array)->Initialize(context, rhs);
}

void ArrayAccessNode::GenerateIR(IRContext& context) const
{
    // Assign locations to generated operations
    SetOperationLocation sol(context, _location);

    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    // Dereference
    const AllocatedRegister* const elementRegister = Load(context);

    // Move from the array element to context._writtenRegisters
    Move(context, context._writtenRegisters, elementRegister, Opcode::Mov, _location);
}

const AllocatedRegister* ArrayAccessNode::Load(IRContext& context) const
{
    // Assign locations to generated operations
    SetOperationLocation sol(context, _location);

    const ArrayTypeBase* const arrayType = dynamic_cast<const ArrayTypeBase*>(_array->GetType());

    const ClassType* const classType = dynamic_cast<const ClassType*>(arrayType->_elementType);

    if (classType)
    {
        // Handle this case:
        // Class c;
        // c[4] ary;
        // ary[3].x = 4;
        if (!IsIndexKnown(context))
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidArrayIndexing)
                << "Accessing objects through an array requires the array index to be known at compile time";
            throw std::runtime_error("The array index needs to be known at compile time.");
        }

        return Load_Array(context);
    }
    else
    {
        const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(_array->GetType());

        if (memoryType)
        {
            return Load_Memory(context);
        }
        else
        {
            return Load_Array(context);
        }
    }
}

void EmitLoadMemory(IRContext& context, const AllocatedRegister* const destRegisters,
                    const AllocatedLeafRegister* const memoryRegister, const AllocatedLeafRegister* const indexRegister,
                    const std::array<const AllocatedLeafRegister*, 2>* const eccStatus)
{
    Operation op = {};

    op._opcode = Opcode::LoadMemory;

    std::vector<size_t>* sourceOffsets = g_compiler->Create<std::vector<size_t>>();

    op._flags._loadMemory._sourceOffsets = sourceOffsets;

    // By default enable the output register
    // This may be disabled later
    op._flags._loadMemory._readLatency = c_defaultMemoryReadLatency;

    op._flags._loadMemory._isPredicated = false;

    // Add the memory object
    op._src.push_back(memoryRegister->GetAccessedRegister());

    // Add the index
    op._src.push_back(indexRegister->GetAccessedRegister());

    const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(memoryRegister->GetType());

    // Predicated loads matter because if the predicate is false:
    //   - On non-replicated memory, an unnecessary load might unnecessarily compete with a load from another call site
    //   - On both non-replicated and replicated memory, the unnecessary load consumes power.
    if (context.IsPredicated() &&
        (GetCodeGenConfig()._conditionalMemoryReads ||
         context._program->_registerTable[memoryRegister->GetAccessedRegister()._registerIndex]
             .Memory()
             .HasReadArbitration()))
    {
        DebuggerHintPushPredicate(context._basicBlock->_operations);

        const AccessedRegister predicateRegister = {context.GetPredicate()};

        // Add the predicate
        op._src.push_back(predicateRegister);

        op._flags._loadMemory._isPredicated = true;
    }

    // Add all parts of the destination at destinations
    size_t runningOffset = 0;

    const auto callback = [&](const AccessedRegister reg, const Type* const type)
    {
        op._dst.push_back(reg);

        // Store the bit offset of this element within the memory
        op._flags._loadMemory._sourceOffsets->push_back(runningOffset);

        runningOffset += type->GetBitWidth();
    };

    destRegisters->VisitRegisters(callback);

    assert(op._flags._loadMemory._sourceOffsets->size() == op._dst.size());

    if (eccStatus)
    {
        // Add more destination registers, to hold ECC status
        for (const AllocatedLeafRegister* const eccReg : *eccStatus)
        {
            op._dst.push_back(eccReg->GetAccessedRegister());
        }
    }

    // op._flags._loadMemory._readPort will be set later
    op._flags._loadMemory._bypass = false;

    context._basicBlock->_operations.push_back(op);

    if (context.IsPredicated() && ParseTreeMemoryTypeNoReplication == memoryType->GetMemoryType())
    {
        DebuggerHintPopPredicate(context._basicBlock->_operations);
    }
}

// Returns an AllocatedLeafRegister containing the index
// If a variable name is used as the index, then the associated AllocatedLeafRegister is returned
// without an intermediate copy
// This is important for memory load within an atomic block, where the address must be computed
// before the atomic block
const AllocatedLeafRegister* ArrayAccessNode::EvaluateIndexExpression(IRContext& context) const
{
    const VariableAccessNode* const variableAccessNode = dynamic_cast<const VariableAccessNode*>(_indexNode);

    if (variableAccessNode)
    {
        return dynamic_cast<const AllocatedLeafRegister*>(variableAccessNode->Load(context));
    }
    else
    {
        return dynamic_cast<const AllocatedLeafRegister*>(EvaluateExpression(context, _indexNode));
    }
}

const AllocatedRegister* ArrayAccessNode::Load_Memory(IRContext& context) const
{
    // Get the register associated with the memory object
    const AllocatedLeafRegister* const memoryRegister =
        dynamic_cast<const AllocatedLeafRegister*>(dynamic_cast<const LValNode*>(_array)->Load(context));

    // Evaluate the index into a register
    const AllocatedLeafRegister* const indexRegister = EvaluateIndexExpression(context);

    const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(_array->GetType());

    // Allocate destination registers of the same type as the memory element
    // This handles the case where the array elements are integers, and the language allows implicit bit-width
    // conversion This also handles the fact that the data becomes available on the next cycle
    const AllocatedRegister* const destRegisters =
        memoryType->_elementType->AllocateRegisters(*context._program, RegisterType::Local, GetExpressionString());

    const bool ecc = ParseTreeMemoryTypeEcc == memoryType->GetMemoryType();

    std::array<const AllocatedLeafRegister*, 2> eccRegs;
    std::array<const AllocatedLeafRegister*, 2>* eccRegsPointer = nullptr;

    if (ecc)
    {
        // Cause EmitLoadMemory to capture ecc status into 2 more registers
        const std::array<std::string, 2> registerNames = {"EccError", "EccValid"};

        for (size_t i = 0; i < 2; i++)
        {
            eccRegs[i] = dynamic_cast<const AllocatedLeafRegister*>(
                g_compiler->GetLeafType(BaseType::Uint, 1, _location)
                    ->AllocateRegisters(*context._program, RegisterType::Local, registerNames[i]));
        }

        eccRegsPointer = &eccRegs;
    }

    EmitLoadMemory(context, destRegisters, memoryRegister, indexRegister, eccRegsPointer);

    const AllocatedRegister* resultRegisters = nullptr;

    if (ecc)
    {
        // Call a function to map memory return value and ECC status
        // to a value of type: _type
        const ResolvedCall resolvedCall =
            ResolveFlatFunctionCall(memoryType->GetEccFuncNameNode(), context._typeContext.GetNamespaceScope());

        // Type checking should verify this
        assert(resolvedCall._functionDesc._modifiers & ParseTreeFunctionModifierInline);
        assert(3 == resolvedCall._functionDesc._parameterTypes.size());

        const FunctionNode::Instance& functionInstance = resolvedCall._functionNode->GetInstance(g_globalObjectName);

        // Error
        Move(context, functionInstance.GetParameterRegisters(0), eccRegs[0], Opcode::Mov, _location);

        // Valid
        Move(context, functionInstance.GetParameterRegisters(1), eccRegs[1], Opcode::Mov, _location);

        // Value
        Move(context, functionInstance.GetParameterRegisters(2), destRegisters, Opcode::Mov, _location);

        // Memory data & ecc status are not known at compile time
        std::vector<KnownValue> knownParameterValues(resolvedCall._functionDesc._parameterTypes.size());

        // Allocate a register to hold the result of the function call
        resultRegisters = GetType()->AllocateRegisters(*context._program, RegisterType::Local, "EccLoadResult");

        // Save/restore _writtenRegisters
        PushPopWrittenRegister pushPopWrittenRegister(context);

        context._writtenRegisters = resultRegisters;

        const FunctionInstance calleeInstance = g_compiler->GetFunctionInstanceEnumerator().LookupEccMemoryCall(this);

        assert(!context._functionInstanceStack.empty());
        auto callerInstance = context._functionInstanceStack.back();

        GenerateInlineCall(context, resolvedCall._functionDesc, resolvedCall._functionNode, g_globalObjectName,
                           functionInstance, knownParameterValues, calleeInstance, callerInstance, _location);
    }
    else
    {
        resultRegisters = destRegisters;
    }

    return resultRegisters;
}

// Returns the runtime index for an array access
size_t ArrayAccessNode::GetIndexRegister(IRContext& context, const IndexAccessMode mode) const
{
    assert(!IsIndexKnown(context));

    const LeafType* const indexLeafType = dynamic_cast<const LeafType*>(_indexNode->GetType());

    const bool isSigned = indexLeafType->_baseType == BaseType::Int;

    // Determine the number of bits needed to access the array
    const ArrayTypeBase* const arrayType = dynamic_cast<const ArrayTypeBase*>(_array->GetType());

    size_t requiredIndexWidth = std::max<size_t>(Log2RoundUp(arrayType->GetTotalCount()), 1);

    if (isSigned && (IndexAccessMode::Write == mode))
    {
        // Signed integers require another bit to cover the space (because negative values don't count)
        // This only applies to writes because reads always wrap
        requiredIndexWidth++;
    }

    // Evaluate the index into a register that is wide enough to cover the array
    // The optimizer and verilog backends assume the index is wide enough
    const AllocatedLeafRegister* const indexRegister =
        AllocateLeafRegister(*(context._program), indexLeafType->_baseType, requiredIndexWidth, _location,
                             RegisterType::Local, _indexNode->GetExpressionString());

    EvaluateExpressionIntoRegister(context, indexRegister, _indexNode);

    return indexRegister->_registerIndex;
}
const AllocatedRegister* ArrayAccessNode::Load_Array(IRContext& context) const
{
    const ArrayTypeBase* const arrayType = dynamic_cast<const ArrayTypeBase*>(_array->GetType());

    const AllocatedArrayRegister* containerRegisters =
        dynamic_cast<const AllocatedArrayRegister*>(dynamic_cast<const LValNode*>(_array)->Load(context));

    const AllocatedRegister* result = nullptr;

    if (IsIndexKnown(context))
    {
        const size_t index = GetKnownIndex(context);

        assert(index < containerRegisters->_elements.size());

        result = containerRegisters->_elements[index];
    }
    else
    {
        // Allocate registers to hold the return value
        const Type* const elementType = arrayType->_elementType;

        if (!elementType->IsPod())
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidArrayIndexing)
                << "Dynamic array indexing requires a value type";
        }

        result =
            elementType->AllocateRegisters(*(context._program), RegisterType::Local, _array->GetExpressionString());

        // Decompose the result into registers
        std::vector<RegAndType> resultRegisters = GetFlatRegisters(result);

        // Decompose each array element into registers
        const size_t srcCount = containerRegisters->_elements.size();

        // If the count is not a power of 2, compute the next largest power of 2
        const size_t srcCountRounded = IsPow2(srcCount) ? srcCount : MpToSizeT(RoundUpToPow2(srcCount));

        const AccessedRegister indexRegister = {GetIndexRegister(context, IndexAccessMode::Read)};

        std::vector<std::vector<RegAndType>> elementRegisters(srcCount);

        for (size_t i = 0; i < srcCount; ++i)
        {
            elementRegisters[i] = GetFlatRegisters(containerRegisters->_elements[i]);

            // Types should be compatible
            assert(elementRegisters[i].size() == resultRegisters.size());
        }

        // Assign locations to generated operations
        SetOperationLocation sol(context, _location);

        // For each structure member
        for (size_t i = 0; i < resultRegisters.size(); ++i)
        {
            Operation op = {};

            if (srcCount > 1)
            {
                // Emit a select operation which will mux the inputs into 1 result
                op._opcode = Opcode::Select;

                // Index
                op._src.push_back(indexRegister);

                // All possible sources
                for (size_t j = 0; j < srcCount; ++j)
                {
                    op._src.push_back(elementRegisters[j][i].first);
                }

                // Add dummy sources to ensure that the number of sources is a power of 2
                for (size_t j = srcCount; j < srcCountRounded; ++j)
                {
                    op._src.push_back(SourceOperand(0));
                }
            }
            else
            {
                assert(srcCount == 1);

                // Only one source, just emit a move
                op._opcode = Opcode::Mov;
                op._src.push_back(elementRegisters[0][i].first);
                op.InsertLocation(_location);
            }

            op._dst.push_back(resultRegisters[i].first);

            context._basicBlock->_operations.push_back(op);
        }
    }

    return result;
}

// Called when an array of objects is accessed
std::string ArrayAccessNode::GetObjectName(KnownValueContext& context, const std::string& containingObjectName,
                                           const ResolveReferenceFunction& resolveReferenceCallback) const
{
    if (!IsIndexKnown(context))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidArrayIndexing)
            << "Object arrays must be referenced with compile-time-known indices";
        throw std::runtime_error("The array index needs to be known at compile time.");
    }

    // GetKnownIndex does bounds checking
    const size_t objectIndex = GetKnownIndex(context);

    // Objects inside of an array don't have names, synthesize one here
    const std::string arrayName =
        dynamic_cast<const LValNode*>(_array)->GetObjectName(context, containingObjectName, resolveReferenceCallback);

    const std::string elementName = GetArrayElementObjectName(arrayName, objectIndex);

    return elementName;
}

// Foo[4] f;
// f[i].x = 4;
// For memories, this is not supported (byte enables could allow this, but this is not supported currently)
// For arrays: if Foo is a struct type, then this is directly supported
// If Foo is a class type, then this is supported, but i must be known at compile time (verified elsewhere)
bool ArrayAccessNode::SupportsPartialWrites() const { return !IsMemory(); }

// Partial writes to memories do not make sense
bool ArrayAccessNode::IsMemory() const
{
    const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(_array->GetType());

    return memoryType != nullptr;
}

void UnaryOpNode::GenerateIR(IRContext& context) const
{
    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    // Disable condition coverage tracking if not an invert operation
    DisableConditionCoverage disableConditionCoverage(context, !IsInvertOperation());

    // Flip inverting flag for condition coverage
    ConditionCoverageInverting conditionCoverageInverting(context, IsInvertOperation());

    // Check if _expression should be saved as a leaf condition for condition coverage
    const bool saveLeafCondition = SaveLeafCondition(context, _expression->IsConditionCoverageLeaf());

    // Allocate a register to hold the result of the child expression
    const AllocatedLeafRegister* const childRegister =
        dynamic_cast<const AllocatedLeafRegister*>(EvaluateExpression(context, _expression));
    const AllocatedLeafRegister* const resultRegister =
        dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters);

    // Assign locations to generated operations
    SetOperationLocation sol(context, _location);

    if (saveLeafCondition && context.HasConditionCoverageTracker())
    {
        ConditionCoverageTracker& tracker = context.GetConditionCoverageTracker();

        // Save leaf condition information
        ConditionCoverageLeafConditionEntry entry = {};
        entry._registerIndex = childRegister->GetAccessedRegister()._registerIndex;
        entry._str = _expression->PrettyPrint();
        entry._inverting = tracker._conditionCoverageFlags._inverting;
        entry._callNode = (bool)(dynamic_cast<const CallNode*>(_expression));
        tracker._conditionCoverageLeafCondition = entry;
    }

    switch (_opType)
    {
    case ParseTreeUnaryOpTypeInvert:
    case ParseTreeUnaryOpTypeLogicalInvert:
    {
        // Add an operation to the basic block which does a invert
        // The invert writes to a register of the same width as the input (because the invert operation behaves as if
        // the input was infinitely wide)
        const AllocatedLeafRegister* const invertedReg = dynamic_cast<const AllocatedLeafRegister*>(
            _expression->GetType()->AllocateRegisters(*context._program, RegisterType::Local, GetExpressionString()));

        {
            Operation op = {};

            op._opcode = Opcode::UnaryOp;
            op._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;

            op._src.resize(1);
            op._src[0] = childRegister->GetAccessedRegister();
            op._dst.push_back(invertedReg->GetAccessedRegister());

            context._basicBlock->_operations.push_back(op);
        }

        // Move from invertedReg to the result
        Move(context, resultRegister, invertedReg, Opcode::Mov, _location);
    }
    break;

    case ParseTreeUnaryOpTypeNegate:
    {
        assert(_opType == ParseTreeUnaryOpTypeNegate);

        const FloatType* const expressionFloatType = dynamic_cast<const FloatType*>(_expression->GetType());

        if (expressionFloatType)
        {
            assert(32 == expressionFloatType->GetBitWidth());

            // Flip the sign bit
            {
                Operation op = {};

                op._opcode = Opcode::BinaryOp;
                op._flags._binaryOpType = ParseTreeBinaryOpTypeXor;

                op._src.resize(2);
                op._src[0] = childRegister->GetAccessedRegister();
                op._src[1] = SourceOperand(0x80000000);
                op._dst.push_back(resultRegister->GetAccessedRegister());

                context._basicBlock->_operations.push_back(op);
            }
        }
        else
        {
            const LeafType* const expressionType = dynamic_cast<const LeafType*>(_expression->GetType());

            const LeafType* const signedExpressionType = ToSigned(expressionType, _location);

            const AllocatedLeafRegister* invertSource = childRegister;

            if (expressionType != signedExpressionType)
            {
                // Move the source into an unsigned register that is as wide as the signed result
                const LeafType* const widenedLeafType =
                    g_compiler->GetLeafType(BaseType::Uint, signedExpressionType->GetBitWidth(), _location);

                const AllocatedLeafRegister* const widenedLeafReg =
                    dynamic_cast<const AllocatedLeafRegister*>(widenedLeafType->AllocateRegisters(
                        *context._program, RegisterType::Local, _expression->GetExpressionString()));

                Move(context, widenedLeafReg, childRegister, Opcode::Mov, _location);

                invertSource = widenedLeafReg;
            }

            // Allocate a register to hold the result of the invert
            // The result of the invert operation is signed, if if the source is unsigned
            const AccessedRegister invertRegister = {
                AllocateRegister(*context._program, signedExpressionType->GetBitWidth(), RegisterType::Local,
                                 "neg_inv_" + _expression->GetExpressionString())};

            // Add an operation to the basic block which does the invert
            {
                Operation op = {};

                op._opcode = Opcode::UnaryOp;
                op._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;

                op._src.resize(1);
                op._src[0] = invertSource->GetAccessedRegister();
                op._dst.push_back(invertRegister);

                // The source is signed.  Indicate this in the operation.
                SignExtend(op, expressionType, 0);

                context._basicBlock->_operations.push_back(op);
            }

            // Add an operation to the basic block adds which 1
            {
                Operation op = {};

                op._opcode = Opcode::BinaryOp;
                op._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

                op._src.resize(2);
                op._src[0] = invertRegister;
                op._src[1] = SourceOperand(1, 1 /* width */);
                op._dst.push_back(resultRegister->GetAccessedRegister());

                // Add flags indicating that negated value should be sign-extended on read
                op._signExtendSourceMask |= (1ull << 0);

                context._basicBlock->_operations.push_back(op);
            }
        }
    }
    break;

    default:
        assert(false);
    }
}

KnownValue UnaryOpNode::TryGetKnownValueImpl(KnownValueContext& context) const
{
    const Type* const expressionType = _expression->GetType();

    const KnownValue childValue = _expression->TryGetKnownValue(context, expressionType);

    const LeafType* const leafExpressionType = dynamic_cast<const LeafType*>(expressionType);

    const BoolType* const boolExpressionType = dynamic_cast<const BoolType*>(expressionType);

    const FloatType* const floatExpressionType = dynamic_cast<const FloatType*>(expressionType);

    KnownValue result;

    if (childValue._type == KnownValueType::Int)
    {
        // child value is known

        if (leafExpressionType || boolExpressionType)
        {
            result._type = KnownValueType::Int;

            const Literal expressionLiteral = {childValue._intVal, expressionType->GetBitWidth()};

            result._intVal = ImplementUnaryOp(expressionLiteral, IsSignedLeafType(expressionType), _opType,
                                              GetType()->GetBitWidth());
        }
        else if (floatExpressionType)
        {
            result._type = KnownValueType::Int;

            result._intVal = childValue._intVal;

            // Flip the sign bit
            bit_flip(result._intVal, 31);
        }
    }

    return result;
}

bool UnaryOpNode::IsInvertOperation() const
{
    return _opType == ParseTreeUnaryOpTypeInvert || _opType == ParseTreeUnaryOpTypeLogicalInvert;
}

void MuxOpNode::GenerateIR(IRContext& context) const
{
    // Disable previous condition coverage tracker for mux node
    DisableConditionCoverage disableConditionCoverage(context, true);

    // Do not generate branch and condition coverage if predicate is known value
    const KnownValue predicateValue = _predicate->TryGetKnownValue(context, _predicate->GetType());
    const bool knownPredicateValue = predicateValue._type == KnownValueType::Int;

    const AllocatedLeafRegister* const predicateRegister =
        dynamic_cast<const AllocatedLeafRegister*>(_predicate->GetType()->AllocateRegisters(
            *context._program, RegisterType::Local, _predicate->GetExpressionString()));
    {
        PushPopConditionCoverageTracker pushPopConditionCoverageTracker(
            !knownPredicateValue, context, CodeCoverageType::Condition, predicateRegister->GetAccessedRegister(),
            _location, _predicate);

        // Evaluate mux predicate
        EvaluateExpressionIntoRegister(context, predicateRegister, _predicate);
    }

    const std::vector<const ParseTreeNode*>& childExpressions =
        dynamic_cast<const NodeList*>(_expressionList)->Children();

    // Determine if mux predicate is an EnumType
    const EnumType* enumType = dynamic_cast<const EnumType*>(_predicate->GetType());

    const size_t numMuxCaseValues = (enumType != nullptr) ? enumType->_constants.size() : childExpressions.size();

    if (GetCodeGenConfig()._codeCoverage && !knownPredicateValue && !context._isInSimAssert &&
        (0 == GetCodeGenConfig()._codeCoverageMuxThreshold ||
         numMuxCaseValues < GetCodeGenConfig()._codeCoverageMuxThreshold))
    {
        SetOperationLocation sol(context, _location);

        const std::string name = context.GenerateCodeCoverageName(_location, CodeCoverageType::MuxCondition);
        const std::string description = "Code coverage for Mux at " + LocationToString(_location);

        CodeCoverage codeCoverage = {};
        codeCoverage._coverageType = CodeCoverageType::MuxCondition;
        codeCoverage._condition = _location;
        AddCodeCoverageContext(codeCoverage, context);

        std::vector<size_t> caseValues;
        // For enum type, only consider valid enum values
        if (enumType)
        {
            for (const auto& entry : enumType->_constants)
            {
                caseValues.push_back(MpToSizeT(entry.second));
            }
        }
        else
        {
            for (size_t caseValue = 0; caseValue < childExpressions.size(); caseValue++)
            {
                caseValues.push_back(caseValue);
            }
        }

        // For each mux case
        for (const size_t caseValue : caseValues)
        {
            codeCoverage._case = std::to_string(caseValue);

            const AccessedRegister caseReg = {
                AllocateRegister(context._program, 1, RegisterType::Local, name + "_eq_" + std::to_string(caseValue))};

            // Compare predicate to caseValue
            {
                Operation cmpOp = {};
                cmpOp._opcode = Opcode::BinaryOp;
                cmpOp._flags._binaryOpType = ParseTreeBinaryOpTypeEQ;
                cmpOp._src.push_back(predicateRegister->GetAccessedRegister());
                cmpOp._src.push_back(caseValue);
                cmpOp._dst.push_back(caseReg);
                context._basicBlock->_operations.push_back(cmpOp);
            }

            // Create code coverage for this case
            OperationList codeCoverageOps = CreateCodeCoverageCounter(context._program, name, description, _location,
                                                                      caseReg, codeCoverage, *context._basicBlock);
            context._basicBlock->_operations.splice(context._basicBlock->_operations.end(), codeCoverageOps);
        }
    }

    std::vector<const AllocatedRegister*> sourceRegisters;

    for (const ParseTreeNode* const expression : childExpressions)
    {
        // Note that this can store null into sourceRegisters if the child expressions (and thus this expression do not
        // have a type) In this case, context._writtenRegisters will be null
        const AllocatedRegister* const expressionRegister = expression->GenerateIROptionalResult(context);

        assert(expressionRegister || !context._writtenRegisters);

        sourceRegisters.push_back(expressionRegister);
    }

    if (context._writtenRegisters)
    {
        // Assign locations to generated operations
        SetOperationLocation sol(context, _location);

        ImplementMux(context, context._writtenRegisters, predicateRegister, sourceRegisters);
    }
    else
    {
        // This case occurs when in code like this:
        // mux(a, b, c); // the result of the mux is not used
        // In this case, a, b, c are all evaluated, but then the result is discarded
    }
}

KnownValue MuxOpNode::TryGetKnownValueImpl(KnownValueContext& context) const
{
    const KnownValue predicateValue = _predicate->TryGetKnownValue(context, _predicate->GetType());

    KnownValue result;

    if (predicateValue._type == KnownValueType::Int)
    {
        const size_t index = MpToSizeT(predicateValue._intVal);

        const std::vector<const ParseTreeNode*>& expressionList =
            dynamic_cast<const NodeList*>(_expressionList)->Children();

        const ParseTreeNode* const expression = expressionList[index];

        result = expression->TryGetKnownValue(context, GetType());
    }

    return result;
}

bool BinaryOpNode::IsFloatOperation() const
{
    const FloatType* const lhsFloatType = dynamic_cast<const FloatType*>(_lhs->GetType());

    const FloatType* const rhsFloatType = dynamic_cast<const FloatType*>(_rhs->GetType());

    return lhsFloatType && rhsFloatType;
}

// Helper used in multiplication - generates an adder tree
void AddRegisters(IRContext& context, const std::vector<AccessedRegister>& srcRegisters, const Location& location)
{
    const size_t resultWidth = context._writtenRegisters->GetType()->GetBitWidth();

    std::vector<AccessedRegister> intermediateRegisters = srcRegisters;

    while (intermediateRegisters.size() > 1)
    {
        const size_t numPairs = intermediateRegisters.size() / 2;

        std::vector<AccessedRegister> newIntermediateRegisters;

        for (size_t pairIndex = 0; pairIndex < numPairs; pairIndex++)
        {
            const AccessedRegister lhs = intermediateRegisters[(pairIndex * 2) + 0];
            const AccessedRegister rhs = intermediateRegisters[(pairIndex * 2) + 1];

            const std::string sumName = context.GetRegisterName(lhs) + "_plus_" + context.GetRegisterName(rhs);

            const AccessedRegister sumRegister = {
                AllocateRegister(*context._program, resultWidth, RegisterType::Local, sumName)};

            newIntermediateRegisters.push_back(sumRegister);

            Operation op = {};

            op._opcode = Opcode::BinaryOp;
            op._dst.push_back(sumRegister);

            op._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

            op._src.push_back(lhs);
            op._src.push_back(rhs);

            context._basicBlock->_operations.push_back(op);
        }

        // Get any left-overs
        for (size_t i = (numPairs * 2); i < intermediateRegisters.size(); i++)
        {
            newIntermediateRegisters.push_back(intermediateRegisters[i]);
        }

        intermediateRegisters = newIntermediateRegisters;
    }

    Operation op = {};

    op._opcode = Opcode::Mov;
    op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister());

    if (intermediateRegisters.size() == 1)
    {
        op._src.push_back(intermediateRegisters[0]);
    }
    else
    {
        // x * 0 == 0
        assert(intermediateRegisters.size() == 0);

        op._src.push_back(SourceOperand(0));
    }

    op.InsertLocation(location);

    context._basicBlock->_operations.push_back(op);
}

// Integer multiplication where 1 value is known at compile time
void GenerateIntegerMultConstant(IRContext& context, const mp_int& knownValue, const LeafType* const knownValueType,
                                 const AllocatedLeafRegister* const otherValue, const size_t resultWidth,
                                 const Location& location)
{
    const bool knownValueSigned = knownValueType->_baseType == BaseType::Int;
    const bool otherValueSigned = dynamic_cast<const LeafType*>(otherValue->GetType())->_baseType == BaseType::Int;

    const size_t otherWidth = otherValue->GetType()->GetBitWidth();

    mp_int bitMask = knownValue;

    // For each bit that is set in the known value
    size_t bitIndex = 0;

    std::vector<AccessedRegister> intermediateRegisters;

    boost::optional<AccessedRegister> valueToSubtract;

    while (bitMask != 0)
    {
        if (bit_test(bitMask, bitIndex))
        {
            // Allocate a register to hold the intermediate result
            const AccessedRegister intermediateRegister = {
                AllocateRegister(*context._program, otherWidth + bitIndex, RegisterType::Local, "MulIntermediate")};

            {
                // Shift the unknown value by the bit index
                Operation op = {};

                op._opcode = Opcode::BinaryOp;
                op._dst.push_back(intermediateRegister);

                op._flags._binaryOpType = ParseTreeBinaryOpTypeShl;

                op._src.push_back(otherValue->GetAccessedRegister());
                op._src.push_back(bitIndex);

                context._basicBlock->_operations.push_back(op);
            }

            AccessedRegister finalIntermediateRegister = intermediateRegister;

            if (otherValueSigned)
            {
                // Sign extend the intermediateRegister to the width of the result
                finalIntermediateRegister._registerIndex =
                    AllocateRegister(*context._program, context._writtenRegisters->GetType()->GetBitWidth(),
                                     RegisterType::Local, "MulIntermediateExtended");

                {
                    Operation op = {};

                    op._opcode = Opcode::Mov;

                    op._src.push_back(intermediateRegister);
                    op._dst.push_back(finalIntermediateRegister);

                    op._signExtendSourceMask = 1ull;

                    op.InsertLocation(location);

                    context._basicBlock->_operations.push_back(op);
                };
            }

            if (knownValueSigned && (bitIndex == (knownValueType->GetBitWidth() - 1)))
            {
                // If the known value is signed, the most significant bit is special
                // It is subtracted from the result (rather than added)
                valueToSubtract = finalIntermediateRegister;
            }
            else
            {
                intermediateRegisters.push_back(finalIntermediateRegister);
            }

            bit_unset(bitMask, bitIndex);
        }

        ++bitIndex;
    }

    AddRegisters(context, intermediateRegisters, location);

    if (valueToSubtract)
    {
        const AccessedRegister& outputReg =
            dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister();

        Operation op = {};

        op._opcode = Opcode::BinaryOp;
        op._flags._binaryOpType = ParseTreeBinaryOpTypeSub;

        op._src.push_back(outputReg);
        op._src.push_back(valueToSubtract.get());

        op._dst.push_back(outputReg);

        context._basicBlock->_operations.push_back(op);
    }
}

// Emits operations to divide a (potentially wide register) into pieces with width = targetSliceWidth
// Sign extends as approriate
std::vector<const AllocatedRegister*> SliceRegister(IRContext& context, const AllocatedRegister* const srcReg,
                                                    const size_t targetSliceWidth, const Location& location)
{
    Program& program = *(context._program);

    std::vector<const AllocatedRegister*> result;

    const size_t srcWidth = srcReg->GetType()->GetBitWidth();

    for (size_t offset = 0; offset < srcWidth; offset += targetSliceWidth)
    {
        const size_t endOffset = std::min(offset + targetSliceWidth, srcWidth);

        assert(endOffset > offset);

        const size_t sliceWidth = endOffset - offset;

        assert(sliceWidth > 0);
        assert(sliceWidth <= targetSliceWidth);

        const LeafType* const sliceType = g_compiler->GetLeafType(BaseType::Uint, sliceWidth, location);

        const AllocatedRegister* const sliceReg =
            sliceType->AllocateRegisters(program, RegisterType::Local, "SlicedRegister");

        // Shift the input to the right, and store in sliceReg
        {
            Operation op = {};

            op._opcode = Opcode::BinaryOp;

            op._flags._binaryOpType = ParseTreeBinaryOpTypeShr;

            op._src.push_back(dynamic_cast<const AllocatedLeafRegister*>(srcReg)->GetAccessedRegister());
            op._src.push_back(offset);

            op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(sliceReg)->GetAccessedRegister());

            context._basicBlock->_operations.push_back(op);
        }

        if (sliceReg->GetType()->GetBitWidth() == targetSliceWidth)
        {
            result.push_back(sliceReg);
        }
        else
        {
            // Sign-extend or zero-extend to produce an operand with the correct width
            const LeafType* const targetSliceType = g_compiler->GetLeafType(BaseType::Uint, targetSliceWidth, location);

            const AllocatedRegister* const wideReg =
                targetSliceType->AllocateRegisters(program, RegisterType::Local, "WideSlicedRegister");

            Operation op = {};

            op._opcode = Opcode::Mov;

            op._src.push_back(dynamic_cast<const AllocatedLeafRegister*>(sliceReg)->GetAccessedRegister());
            op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(wideReg)->GetAccessedRegister());

            if (IsSignedLeafType(srcReg->GetType()))
            {
                op._signExtendSourceMask = 1ull;
            }

            op.InsertLocation(location);

            context._basicBlock->_operations.push_back(op);

            result.push_back(wideReg);
        }
    }

    return result;
}

const AllocatedRegister* IntegerMultExtend(IRContext& context, const AllocatedRegister* const srcReg,
                                           const bool srcOperandIsSigned, const bool srcOperandIsHighWord,
                                           const size_t resultWidth, const Location& location)
{
    Program& program = *(context._program);

    // Allocate a result register that is 1 bit wider than the source
    const LeafType* const srcType = dynamic_cast<const LeafType*>(srcReg->GetType());

    const LeafType* const resultType = g_compiler->GetLeafType(srcType->_baseType, resultWidth, location);

    const AllocatedLeafRegister* const resultReg = dynamic_cast<const AllocatedLeafRegister*>(
        resultType->AllocateRegisters(program, RegisterType::Local, "MultExtendedSrcWord"));

    // Emit an operation that moves from the source to the destination
    // Sign-extend if the source operand is signed, and this is the most significant word of the source operand
    // Zero-extend otherwise
    Operation op = {};

    op._opcode = Opcode::Mov;

    op._src.push_back(dynamic_cast<const AllocatedLeafRegister*>(srcReg)->GetAccessedRegister());
    op._dst.push_back(resultReg->GetAccessedRegister());

    if (srcOperandIsSigned && srcOperandIsHighWord)
    {
        op._signExtendSourceMask = 1ull;
    }

    op.InsertLocation(location);

    context._basicBlock->_operations.push_back(op);

    return resultReg;
}

// Returns the source width of intermediate partial products used in integer multiplications
size_t GetIntegerMultMaxTargetSrcWidth(const bool eitherOperandSigned, const size_t targetIntegerMulSrcWidth)
{
    assert(targetIntegerMulSrcWidth > 0);

    // For unsigned operands: the maximum multiplication width available in the target device
    // For signed operands: one less than the maximum width available on the target device, because some factors of
    // partial products must be zero-extended
    const size_t maxTargetSrcWidth = eitherOperandSigned ? (targetIntegerMulSrcWidth - 1) : targetIntegerMulSrcWidth;

    return maxTargetSrcWidth;
}

// Implements an integer multiply with one or more primitive operations (DSP or lutmul)
// Breaks down the operation into individual operations which are supported by the target HW
//
// For unsigned * unsigned, the math is straight-forward.
// The large unsigned multiply is broken down into small unsigned multiplies.
//
// For uint8 * uint8 broken down into uint4 * uint4: A[7:0] * B[7:0]
// Result = ((A[7:4]*B[7:4])*2^8) + ((A[7:4]*B[3:0])*2^4) + ((A[3:0]*B[7:4])*2^4) + (A[3:0]*B[3:0])
//
// For signed cases, it is a bit trickier.  Some of the operands of the small multiplications are treated as signed,
// while others are unsigned In order to implement this, the small multiplications put a 0 or 1 in the high bit of the
// source operands This requires the multiplication to be broken down into 1-bit smaller pieces
//
// For example, int8 * int8 could be implemented with 4-bit multipliers, but the source operands would be broken down
// into 3-bit pieces int8 * int8: A[7:0] * B[7:0] Result = (SignExtend(A[7:6]) * SignExtend(B[7:6]) * 2^^12) +
//          (SignExtend(A[7:6]) * ZeroExtend(B[5:3]) * 2^^9) +
//          (SignExtend(A[7:6]) * ZeroExtend(B[2:0]) * 2^^6) +
//          (ZeroExtend(A[5:3]) * SignExtend(B[7:6]) * 2^^9) +
//          (ZeroExtend(A[5:3]) * ZeroExtend(B[5:3]) * 2^^6) +
//          (ZeroExtend(A[5:3]) * ZeroExtend(B[2:0]) * 2^^3) +
//          (ZeroExtend(A[2:0]) * SignExtend(B[7:6]) * 2^^6) +
//          (ZeroExtend(A[2:0]) * ZeroExtend(B[5:3]) * 2^^3) +
//          (ZeroExtend(A[2:0]) * ZeroExtend(B[2:0]))
//
void GenerateIntegerMultParameterized(
    IRContext& context, const AllocatedRegister* const lhsRegister, const AllocatedRegister* const rhsRegister,
    const size_t targetIntegerMulSrcWidth,
    const std::function<void(const AllocatedRegister* const, const std::vector<const AllocatedRegister*>&,
                             const AllocatedLeafRegister* const, const std::array<bool, 2>&)>& EmitMultCb,
    const Location& location)
{
    Program& program = *(context._program);

    const bool lhsSigned = (dynamic_cast<const LeafType*>(lhsRegister->GetType())->_baseType == BaseType::Int);
    const bool rhsSigned = (dynamic_cast<const LeafType*>(rhsRegister->GetType())->_baseType == BaseType::Int);

    const bool eitherOperandSigned = lhsSigned || rhsSigned;

    // For unsigned operands: the maximum multiplication width available in the target device
    // For signed operands: one less than the maximum width available on the target device, because some factors of
    // partial products must be zero-extended
    const size_t maxTargetSrcWidth = GetIntegerMultMaxTargetSrcWidth(eitherOperandSigned, targetIntegerMulSrcWidth);

    // Break each operand down into operands no larger than maxTargetSrcWidth
    std::vector<const AllocatedRegister*> lhsTerms = SliceRegister(context, lhsRegister, maxTargetSrcWidth, location);
    std::vector<const AllocatedRegister*> rhsTerms = SliceRegister(context, rhsRegister, maxTargetSrcWidth, location);

    std::vector<AccessedRegister> partialProducts;

    const LeafType* const partialProductType = g_compiler->GetLeafType(
        eitherOperandSigned ? BaseType::Int : BaseType::Uint, targetIntegerMulSrcWidth * 2, location);

    // Signed multiplication functions may have a 3rd parameter, which optionally negates the output
    // Create a register with the constant '0' in it
    const AllocatedLeafRegister* const falseReg = dynamic_cast<const AllocatedLeafRegister*>(
        g_compiler->GetBoolType()->AllocateRegisters(program, RegisterType::Local, "ConstFalse"));

    {
        Operation op = {};

        op._opcode = Opcode::Mov;

        op._src.push_back(0);
        op._dst.push_back(falseReg->GetAccessedRegister());
        op.InsertLocation(location);

        context._basicBlock->_operations.push_back(op);
    };

    for (size_t i = 0; i < lhsTerms.size(); i++)
    {
        for (size_t j = 0; j < rhsTerms.size(); j++)
        {
            // Allocate a register to store the partial product
            const AllocatedRegister* const partialResultReg =
                partialProductType->AllocateRegisters(program, RegisterType::Local, "MultPartialProduct");

            // Select the src operands
            std::vector<const AllocatedRegister*> srcOperands(2);
            srcOperands[0] = lhsTerms[i];
            srcOperands[1] = rhsTerms[j];

            if (eitherOperandSigned)
            {
                // If an operand is signed and this is the most significant word, then sign-extend it by 1 bit
                // Otherwise, zero extend it by 1 bit
                srcOperands[0] = IntegerMultExtend(context, srcOperands[0], lhsSigned, (i == (lhsTerms.size() - 1)),
                                                   targetIntegerMulSrcWidth, location);
                srcOperands[1] = IntegerMultExtend(context, srcOperands[1], rhsSigned, (j == (rhsTerms.size() - 1)),
                                                   targetIntegerMulSrcWidth, location);
            }

            // Emit a multiplication operation supported by the target device
            EmitMultCb(partialResultReg, srcOperands, falseReg, {lhsSigned, rhsSigned});

            const AllocatedRegister* extendedPartialResultReg = partialResultReg;

            if (eitherOperandSigned)
            {
                // Sign extend the partial product up to the width of the result
                // Note that the width of extendedPartialResultReg = width of context._writtenRegisters
                // To ensure that shift & add operations for upper bits can be optimized away if they are not used
                extendedPartialResultReg = context._writtenRegisters->GetType()->AllocateRegisters(
                    program, RegisterType::Local, "MultPartialProductExtended");

                {
                    Operation op = {};

                    op._opcode = Opcode::Mov;

                    op._src.push_back(
                        dynamic_cast<const AllocatedLeafRegister*>(partialResultReg)->GetAccessedRegister());
                    op._dst.push_back(
                        dynamic_cast<const AllocatedLeafRegister*>(extendedPartialResultReg)->GetAccessedRegister());

                    op._signExtendSourceMask = 1ull;

                    op.InsertLocation(location);

                    context._basicBlock->_operations.push_back(op);
                };
            }

            // Shift the result left by the correct amount
            const size_t shiftAmount = (i + j) * maxTargetSrcWidth;

            // Note that the width of shiftedReg = width of context._writtenRegisters
            // To ensure that shift & add operations for upper bits can be optimized away if they are not used
            const AllocatedRegister* const shiftedReg = context._writtenRegisters->GetType()->AllocateRegisters(
                program, RegisterType::Local, "MultPartialProductShifted");

            {
                Operation op = {};

                op._opcode = Opcode::BinaryOp;
                op._flags._binaryOpType = ParseTreeBinaryOpTypeShl;

                op._src.push_back(
                    dynamic_cast<const AllocatedLeafRegister*>(extendedPartialResultReg)->GetAccessedRegister());
                op._src.push_back(shiftAmount);
                op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(shiftedReg)->GetAccessedRegister());

                context._basicBlock->_operations.push_back(op);
            }

            // Store partial product
            partialProducts.push_back(dynamic_cast<const AllocatedLeafRegister*>(shiftedReg)->GetAccessedRegister());
        }
    }

    // Sum all of the partial products
    AddRegisters(context, partialProducts, location);
}

// Integer multiplication where both operands are not known at compile time
void GenerateIntegerMult(IRContext& context, const BinaryOpNode* const binaryOpNode,
                         const AllocatedRegister* const lhsRegister, const AllocatedRegister* const rhsRegister,
                         const Location& location)
{
    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

    const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();

    const bool useExternalModule = deviceConfig._integerMulSrcWidth > 0;

    // external module names should be specified if and only if _integerMulSrcWidth is > 0
    assert(deviceConfig._unsignedIntegerMulName.empty() != useExternalModule);
    assert(deviceConfig._signedIntegerMulName.empty() != useExternalModule);

    if (useExternalModule)
    {
        const auto emitMult = [&](const AllocatedRegister* const resultReg,
                                  const std::vector<const AllocatedRegister*>& srcOperandsIn,
                                  const AllocatedLeafRegister* const falseReg, const std::array<bool, 2>& operandSigned)
        {
            std::vector<const AllocatedRegister*> srcOperands = srcOperandsIn;

            const bool eitherOperandSigned = operandSigned[0] || operandSigned[1];

            const std::string mulFunctionName =
                eitherOperandSigned ? deviceConfig._signedIntegerMulName : deviceConfig._unsignedIntegerMulName;

            const FunctionDesc mulFunctionDesc =
                g_compiler->GetFunctionDesc(g_compiler->GetGlobalClassType(), g_dspScope, mulFunctionName, location);

            if (mulFunctionDesc._parameterTypes.size() == 3)
            {
                // Some signed multiplication functions have a 3rd parameter, which optionally negates the output
                srcOperands.push_back(falseReg);
            }
            else
            {
                assert(mulFunctionDesc._parameterTypes.size() == 2);
            }

            // Save context._writtenRegister
            PushPopWrittenRegister pushPopWrittenReg(context);

            context._writtenRegisters = resultReg;

            if (mulFunctionDesc._modifiers & ParseTreeFunctionModifierExternalFixedLatency)
            {
                CallNode::GenerateExternalFixedLatencyCallRegisterArgs(
                    context, srcOperands, mulFunctionDesc,
                    FixupString(FlattenScopeAndAppendName(g_dspScope, mulFunctionName)),
                    ExternalModuleCallType::InstantiateInBasicBlock, {}, location);
            }
            else
            {
                const ResolvedCall resolvedCall = ResolveFlatFunctionCall(
                    mulFunctionName, g_dspScope, context._typeContext.GetNamespaceScope(), location);

                const FunctionNode::Instance& functionInstance =
                    resolvedCall._functionNode->GetInstance(g_globalObjectName);

                std::vector<KnownValue> knownParameterValues(resolvedCall._functionDesc._parameterTypes.size());

                const FunctionInstance calleeInstance =
                    g_compiler->GetFunctionInstanceEnumerator().LookupBinaryOpCall(binaryOpNode);

                auto callerInstance = context._functionInstanceStack.back();

                // Write function parameters
                for (size_t paramIndex = 0; paramIndex < srcOperands.size(); paramIndex++)
                {
                    Move(context, functionInstance.GetParameterRegisters(paramIndex), srcOperands[paramIndex],
                         Opcode::Mov, location);
                }

                GenerateInlineCall(context, mulFunctionDesc, resolvedCall._functionNode, g_globalObjectName,
                                   functionInstance, knownParameterValues, calleeInstance, callerInstance, location);
            }

            if (mulFunctionDesc._parameterTypes.empty())
            {
                g_compiler->ErrorStream(location, CompileError::UnknownFunction)
                    << "Specify --import-dir for the standard library in order to use DSP functions.";
                return;
            }
        };

        const size_t targetIntegerMulSrcWidth = GetCodeGenDeviceConfig()._integerMulSrcWidth;

        GenerateIntegerMultParameterized(context, lhsRegister, rhsRegister, targetIntegerMulSrcWidth, emitMult,
                                         location);
    }
    else
    {
        const auto emitMult = [&](const AllocatedRegister* const resultReg,
                                  const std::vector<const AllocatedRegister*>& srcOperandsIn,
                                  const AllocatedLeafRegister* const falseReg, const std::array<bool, 2>& operandSigned)
        {
            Operation op = {};

            op._opcode = Opcode::BinaryOp;
            op._flags._binaryOpType = ParseTreeBinaryOpTypeLutMul;
            op.PushOperand(dynamic_cast<const AllocatedLeafRegister*>(srcOperandsIn[0])->GetAccessedRegister(),
                           operandSigned[0]);
            op.PushOperand(dynamic_cast<const AllocatedLeafRegister*>(srcOperandsIn[1])->GetAccessedRegister(),
                           operandSigned[1]);
            op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(resultReg)->GetAccessedRegister());

            context._basicBlock->_operations.push_back(op);
        };

        const size_t targetSrcWidth = codeGenConfig._maxMulSrcWidth;

        if ((lhsRegister->GetType()->GetBitWidth() > targetSrcWidth) ||
            (rhsRegister->GetType()->GetBitWidth() > targetSrcWidth))
        {
            // One of the operands is too wide, decompose
            GenerateIntegerMultParameterized(context, lhsRegister, rhsRegister, targetSrcWidth, emitMult, location);
        }
        else
        {
            // Both operands are narrow enough, emit 1 multiplication into the IR
            // Don't use GenerateIntegerMultParameterized because it will decompose for signed operations always
            // because it subtracts 1 from the width of internal operands
            assert(context._writtenRegisters);

            std::vector<const AllocatedRegister*> srcOperands;

            srcOperands.push_back(lhsRegister);
            srcOperands.push_back(rhsRegister);

            const bool lhsSigned = (dynamic_cast<const LeafType*>(lhsRegister->GetType())->_baseType == BaseType::Int);
            const bool rhsSigned = (dynamic_cast<const LeafType*>(rhsRegister->GetType())->_baseType == BaseType::Int);

            emitMult(context._writtenRegisters, srcOperands, nullptr, {lhsSigned, rhsSigned});
        }
    }
}

// For binary operations that are implemented as calls to inline functions
// Return the called function
std::pair<const FunctionNode*, size_t> BinaryOpNode::GetCalledInlineFunction(KnownValueContext& knownValueContext) const
{
    std::pair<const FunctionNode*, size_t> result(nullptr, 0);

    if (IsFloatOperation())
    {
        std::string name;

        switch (_opType)
        {
        case ParseTreeBinaryOpTypeAdd:
            name = "add";
            break;

        case ParseTreeBinaryOpTypeSub:
            name = "sub";
            break;

        case ParseTreeBinaryOpTypeMul:
            name = "mul";
            break;

        default:
            assert(false);
        }

        result.first = GetFunctionByName(g_compiler->GetGlobalClassType(), g_float32OperatorScope, name, _location);

        if (result.first)
        {
            assert(ParseTreeFunctionModifierInline == (result.first->GetModifiers() & ParseTreeFunctionModifierInline));

            result.second = 1;
        }
        else
        {
            g_compiler->ErrorStream(_location, CompileError::UnknownFunction)
                << "Specify --import-dir for the standard library in order to use floating point operators.";
            throw std::runtime_error("Function not found");
        }
    }
    else if (_opType == ParseTreeBinaryOpTypeMul)
    {
        const bool lhsSigned = IsSignedLeafType(_lhs->GetType());
        const bool rhsSigned = IsSignedLeafType(_rhs->GetType());

        const bool lhsKnown = _lhs->TryGetKnownValue(knownValueContext, _lhs->GetType())._type == KnownValueType::Int;
        const bool rhsKnown = _rhs->TryGetKnownValue(knownValueContext, _rhs->GetType())._type == KnownValueType::Int;

        // If either operand is known at compile time
        // then the multiplication function will not be called
        if (!lhsKnown && !rhsKnown)
        {
            const bool eitherOperandSigned = lhsSigned || rhsSigned;

            const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

            const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();

            // _integerMulSrcWidth is 0 when there is no device-specific multiplication function
            if (GetCodeGenDeviceConfig()._integerMulSrcWidth > 0)
            {
                const size_t maxTargetSrcWidth = GetIntegerMultMaxTargetSrcWidth(
                    eitherOperandSigned, GetCodeGenDeviceConfig()._integerMulSrcWidth);

                const std::string mulFunctionName =
                    eitherOperandSigned ? deviceConfig._signedIntegerMulName : deviceConfig._unsignedIntegerMulName;

                const FunctionDesc mulFunctionDesc = g_compiler->GetFunctionDesc(
                    g_compiler->GetGlobalClassType(), g_dspScope, mulFunctionName, _location);

                if (0 == (mulFunctionDesc._modifiers & ParseTreeFunctionModifierExternalFixedLatency))
                {
                    // The multiplication function is not external
                    // Function instance enumeration can enumerate multiple instances of the function
                    // This must produce the same result as GenerateIntegerMultParameterized
                    const size_t numLhsTerms =
                        (_lhs->GetType()->GetBitWidth() + maxTargetSrcWidth - 1) / maxTargetSrcWidth;
                    const size_t numRhsTerms =
                        (_rhs->GetType()->GetBitWidth() + maxTargetSrcWidth - 1) / maxTargetSrcWidth;

                    result.first =
                        GetFunctionByName(g_compiler->GetGlobalClassType(), g_dspScope, mulFunctionName, _location);

                    result.second = numLhsTerms * numRhsTerms;
                }
            }
        }
    }

    return result;
}

void BinaryOpNode::GenerateIR(IRContext& context) const
{
    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    // Evaluate both sides of the operation
    std::vector<const ParseTreeNode*> nodes;

    nodes.push_back(_lhs);
    nodes.push_back(_rhs);

    // Also, get compile-time known values for each side of the operation
    std::vector<KnownValue> knownValues(2);

    // Evaluate the child expressions
    std::vector<const AllocatedLeafRegister*> registers(2, nullptr);

    // Disable condition coverage if not an AND, OR, or XOR node
    const bool isLogicalOrBitOperation = IsLogicalOperation() || IsBitOperation();
    DisableConditionCoverage disableConditionCoverage(context, !isLogicalOrBitOperation);

    // If we are not currently tracking condition or expression coverage, then start tracking expression coverage
    bool newExpressionCoverage = false;
    if (GetCodeGenConfig()._codeCoverage && isLogicalOrBitOperation)
    {
        bool trackingCoverage = false;
        if (context.HasConditionCoverageTracker())
        {
            ConditionCoverageTracker& tracker = context.GetConditionCoverageTracker();
            if (tracker._conditionCoverageFlags._enabled)
            {
                trackingCoverage = true;
            }
        }

        newExpressionCoverage = !trackingCoverage;
    }
    PushPopConditionCoverageTracker pushPopConditionCoverageTracker(newExpressionCoverage, context,
                                                                    CodeCoverageType::Expression);

    // Handle condition coverage
    bool doConditionCoverage = false;
    ConditionCoverageTracker* tracker;
    if (context.HasConditionCoverageTracker())
    {
        tracker = &context.GetConditionCoverageTracker();
        doConditionCoverage = tracker->_conditionCoverageFlags._enabled;

        // Save the first condition that we see as the top-level condition
        if (doConditionCoverage && !tracker->_topLevelCondition)
        {
            tracker->_topLevelCondition =
                dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister();
            tracker->_topLevelConditionLocation = _location;
            tracker->_topLevelConditionString = PrettyPrint();
        }
    }

    // Track whether we are within an XOR node
    const bool isXor = _opType == ParseTreeBinaryOpTypeLogicalXor || _opType == ParseTreeBinaryOpTypeXor;
    ConditionCoverageWithinXor conditionCoverageWithinXor(context, isXor);

    // Registers for tracking non-masking conditions for condition coverage
    AccessedRegister nonMaskingReg0;
    AccessedRegister nonMaskingReg1;
    if (doConditionCoverage)
    {
        nonMaskingReg0._registerIndex = AllocateRegister(context._program, 1, RegisterType::Local, "non_masking_0");
        nonMaskingReg1._registerIndex = AllocateRegister(context._program, 1, RegisterType::Local, "non_masking_1");
    }

    for (size_t i = 0; i < 2; ++i)
    {
        const ParseTreeNode* const node = nodes[i];

        // Track non-masking conditions for condition coverage
        NonMaskingConditionSubEntry entry = {};
        if (doConditionCoverage && !isXor)
        {
            // Save the node that we are not visiting in this iteration
            const ParseTreeNode* const saveNode = nodes[i == 0 ? 1 : 0];
            std::string saveNodeString = saveNode->PrettyPrint();

            switch (_opType)
            {
            case ParseTreeBinaryOpTypeLogicalAnd:
            case ParseTreeBinaryOpTypeAnd:
                entry._invert = false;
                break;
            case ParseTreeBinaryOpTypeLogicalOr:
            case ParseTreeBinaryOpTypeOr:
                entry._invert = true;
                if (saveNodeString.at(0) == '~' || saveNodeString.at(0) == '!')
                {
                    // Cancel out existing invert
                    saveNodeString.erase(0, 1);
                }
                else
                {
                    saveNodeString = "!" + saveNodeString;
                }
                break;
            default:
                assert(false);
                break;
            }

            entry._registerIndex = (i == 0 ? nonMaskingReg1._registerIndex : nonMaskingReg0._registerIndex);
            entry._str = saveNodeString;

            // Update register name with more descriptive string
            assert(!saveNodeString.empty());
            context._program->_registerTable[entry._registerIndex]._name = saveNodeString;
        }
        PushPopNonMaskingCondition pushPopNonMaskingCondition(context, entry, doConditionCoverage && !isXor);

        knownValues[i] = node->TryGetKnownValue(context, node->GetType());

        if (doConditionCoverage)
        {
            // Reset binary op flag
            tracker->_conditionCoverageFlags._deeperBinaryOp = false;

            // If node is not a leaf condition, then enable tracking to find leaf condition
            tracker->_conditionCoverageFlags._leafConditionTracking = !node->IsConditionCoverageLeaf();
            // Reset optional node information for inverted leaf condition
            tracker->_conditionCoverageLeafCondition = {};
        }

        // Disable condition coverage tracking if node is known value
        DisableConditionCoverage disableConditionCoverageKnownValue(
            context, doConditionCoverage && knownValues[i]._type == KnownValueType::Int);

        registers[i] = dynamic_cast<const AllocatedLeafRegister*>(EvaluateExpression(context, node));

        if (doConditionCoverage)
        {
            // Copy register result into register used for condition coverage
            {
                Operation mvOp = {};
                mvOp._opcode = Opcode::Mov;
                mvOp._src.push_back(registers[i]->GetAccessedRegister());
                mvOp._dst.push_back(i == 0 ? nonMaskingReg0 : nonMaskingReg1);
                mvOp.InsertLocation(_location);
                context._basicBlock->_operations.push_back(mvOp);
            }

            // If node value is known, then skip
            if (knownValues[i]._type == KnownValueType::Int)
            {
                continue;
            }

            // If flag comes back set, then there is a deeper binary op
            const bool hasBinaryOp = tracker->_conditionCoverageFlags._deeperBinaryOp;
            // If no deeper binary op, then save condition coverage information for this node
            if (!hasBinaryOp)
            {
                std::string nodeString;
                size_t registerIndex;
                bool inverting;
                bool callNode;
                // Look for leaf condition information saved from tracking within invert operations
                if (tracker->_conditionCoverageLeafCondition)
                {
                    const ConditionCoverageLeafConditionEntry& entry = *tracker->_conditionCoverageLeafCondition;
                    nodeString = entry._str;
                    registerIndex = entry._registerIndex;
                    inverting = entry._inverting;
                    callNode = entry._callNode;
                }
                // Otherwise, directly use information from node
                else
                {
                    nodeString = node->PrettyPrint();
                    registerIndex = registers[i]->GetAccessedRegister()._registerIndex;
                    inverting = tracker->_conditionCoverageFlags._inverting;
                    callNode = (bool)(dynamic_cast<const CallNode*>(node));
                }

                const bool hasNonMaskingCondition = (tracker->_nonMaskingConditionStack.size() != 0);
                NonMaskingConditionEntry nonMaskingConditionEntry = {};
                if (hasNonMaskingCondition)
                {
                    // Copy conditionCoverageStack to nonMaskingConditionEntry
                    nonMaskingConditionEntry._entries = tracker->_nonMaskingConditionStack;
                    // Track whether this leaf condition inverting or non-inverting with respect to the original
                    // condition
                    nonMaskingConditionEntry._inverting = inverting;
                }

                // Each call to a function should be handled separately in case function is stateful
                if (callNode)
                {
                    size_t callCounter = 0;
                    // If no previous call found
                    if (tracker->_conditionCoverageCallCounterMap.end() ==
                        tracker->_conditionCoverageCallCounterMap.find(nodeString))
                    {
                        // Initialize counter
                        tracker->_conditionCoverageCallCounterMap[nodeString] = 0;
                    }
                    else
                    {
                        // Increment counter
                        callCounter = tracker->_conditionCoverageCallCounterMap[nodeString];
                        callCounter++;
                        tracker->_conditionCoverageCallCounterMap[nodeString] = callCounter;
                    }
                    nodeString += "_call" + std::to_string(callCounter);
                }

                // No existing entry, create one
                if (tracker->_conditionCoverageMap.end() == tracker->_conditionCoverageMap.find(nodeString))
                {
                    ConditionCoverageEntry conditionCoverageEntry = {};
                    conditionCoverageEntry._registerIndex = registerIndex;
                    conditionCoverageEntry._withinXor = tracker->_conditionCoverageFlags._withinXor;
                    if (hasNonMaskingCondition)
                    {
                        conditionCoverageEntry._nonMaskingConditions.push_back(nonMaskingConditionEntry);
                    }

                    tracker->_conditionCoverageMap[nodeString] = conditionCoverageEntry;
                }
                else
                // Update existing entry
                {
                    ConditionCoverageEntry& conditionCoverageEntry = tracker->_conditionCoverageMap[nodeString];

                    conditionCoverageEntry._withinXor =
                        conditionCoverageEntry._withinXor || tracker->_conditionCoverageFlags._withinXor;
                    if (hasNonMaskingCondition)
                    {
                        conditionCoverageEntry._nonMaskingConditions.push_back(nonMaskingConditionEntry);
                    }
                }
            }
        }
    }

    // Assign locations to generated operations
    SetOperationLocation sol(context, _location);

    // Check for divide by zero
    switch (_opType)
    {
    case ParseTreeBinaryOpTypeDiv:
    case ParseTreeBinaryOpTypeMod:
        if ((knownValues[1]._type == KnownValueType::Int) && (knownValues[1]._intVal == 0))
        {
            g_compiler->ErrorStream(_location, CompileError::DivideByZero) << "Divide by zero";
        }
        break;
    default:
        break;
    }

    // Try to see if the entire binary expression can be evaluated at compile time
    bool evaluatedAtCompileTime = false;

    {
        const KnownValue thisNodeKnownValue = TryGetKnownValue(context, context._writtenRegisters->GetType());

        if (thisNodeKnownValue._type == KnownValueType::Int)
        {
            Operation op = {};

            op._opcode = Opcode::Mov;

            op._src.push_back(thisNodeKnownValue._intVal);
            op._dst.push_back(
                dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister());

            context._basicBlock->_operations.push_back(op);

            evaluatedAtCompileTime = true;
        }
    }

    if (!evaluatedAtCompileTime)
    {
        if (IsFloatOperation())
        {
            const FunctionNode* const functionNode = GetCalledInlineFunction(context).first;

            auto& functionInstance = functionNode->GetInstance(g_globalObjectName);

            auto functionDesc = g_compiler->GetFunctionDesc(g_compiler->GetGlobalClassType(), g_float32OperatorScope,
                                                            functionNode->GetName(), _location);

            for (size_t i = 0; i < registers.size(); ++i)
            {
                Move(context, functionInstance.GetParameterRegisters(i), registers[i], Opcode::Mov, _location);
            }

            assert(!context._functionInstanceStack.empty());
            auto callerInstance = context._functionInstanceStack.back();

            GenerateInlineCall(context, functionDesc, functionNode, g_globalObjectName, functionInstance, knownValues,
                               g_compiler->GetFunctionInstanceEnumerator().LookupBinaryOpCall(this), callerInstance,
                               _location);
        }
        else if (ParseTreeBinaryOpTypeMul == _opType)
        {
            const LeafType* const lhsType = dynamic_cast<const LeafType*>(nodes[0]->GetType());
            const LeafType* const rhsType = dynamic_cast<const LeafType*>(nodes[1]->GetType());

            if ((knownValues[0]._type == KnownValueType::Int) || (knownValues[1]._type == KnownValueType::Int))
            {
                // At least 1 value is known at compile time, implement the multiplication with shifts and adds
                const size_t knownValueIndex = (knownValues[0]._type == KnownValueType::Int) ? 0 : 1;

                const KnownValue knownValue = knownValues[knownValueIndex];

                const LeafType* const knownValueType = dynamic_cast<const LeafType*>(nodes[knownValueIndex]->GetType());

                const AllocatedLeafRegister* const otherValue = registers[1 - knownValueIndex];

                GenerateIntegerMultConstant(context, knownValue._intVal, knownValueType, otherValue,
                                            GetType()->GetBitWidth(), _location);
            }
            else
            {
                // Implement the multiplication with a series of DSP operations
                GenerateIntegerMult(context, this, registers[0], registers[1], _location);
            }
        }
        else
        {
            Operation op = {};

            op._opcode = Opcode::BinaryOp;
            op._dst.push_back(
                dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister());

            switch (_opType)
            {
            case ParseTreeBinaryOpTypeMul:
                assert(false); // should be covered above
                break;

            case ParseTreeBinaryOpTypeDiv:
                if ((knownValues[1]._type == KnownValueType::Int) && IsPow2(knownValues[1]._intVal))
                {
                    const LeafType* const lhsLeafType = dynamic_cast<const LeafType*>(_lhs->GetType());
                    assert(lhsLeafType);

                    AccessedRegister shiftSrcReg = registers[0]->GetAccessedRegister();

                    if (BaseType::Int == lhsLeafType->_baseType && knownValues[1]._intVal > 1)
                    {
                        // lhs is signed.  If it is negative, then a constant must be added
                        // to implement rounding towards zero

                        // Move the sign bit to a 1-bit register
                        const AccessedRegister signBitReg = {
                            AllocateRegister(context._program, 1, RegisterType::Local, "ShiftSignBit")};

                        {
                            Operation shiftSignBitOp = {};

                            shiftSignBitOp._opcode = Opcode::BinaryOp;
                            shiftSignBitOp._flags._binaryOpType = ParseTreeBinaryOpTypeShr;

                            shiftSignBitOp._src.push_back(shiftSrcReg);
                            shiftSignBitOp._src.push_back(lhsLeafType->_width - 1);

                            shiftSignBitOp._dst.push_back(signBitReg);

                            // Shift in sign bit
                            SignExtend(shiftSignBitOp, _lhs->GetType(), 0);

                            context._basicBlock->_operations.push_back(shiftSignBitOp);
                        }

                        const AccessedRegister valueToAddReg = {AllocateRegister(
                            context._program, Log2(knownValues[1]._intVal), RegisterType::Local, "ShiftValuetoAdd")};

                        // Generate the value to add (0 for positive values)
                        {
                            Operation selectOp = {};

                            selectOp._opcode = Opcode::Select;

                            selectOp._src.push_back(signBitReg);
                            selectOp._src.push_back(0);
                            selectOp._src.push_back(MpToSizeT(knownValues[1]._intVal - 1));

                            selectOp._dst.push_back(valueToAddReg);

                            context._basicBlock->_operations.push_back(selectOp);
                        }

                        // Do the add
                        const AccessedRegister sumReg = {AllocateRegister(context._program, lhsLeafType->_width + 1,
                                                                          RegisterType::Local, "ShiftAddReg")};

                        {
                            Operation addOp = {};

                            addOp._opcode = Opcode::BinaryOp;
                            addOp._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

                            addOp._src.push_back(shiftSrcReg);
                            addOp._src.push_back(valueToAddReg);

                            addOp._dst.push_back(sumReg);

                            SignExtend(addOp, _lhs->GetType(), 0);

                            context._basicBlock->_operations.push_back(addOp);
                        }

                        // The shift applies to the value after the addition
                        shiftSrcReg = sumReg;
                    }

                    op._flags._binaryOpType = ParseTreeBinaryOpTypeShr;

                    op._src.push_back(shiftSrcReg);
                    op._src.push_back(Log2(knownValues[1]._intVal));

                    // If the lhs is signed, then the shift should sift in the msb
                    SignExtend(op, _lhs->GetType(), 0);
                }
                else
                {
                    std::ostringstream str;

                    str << "Division requires both sides to be known at compile time, or the right-hand side to be a "
                           "power of 2 (known at compile time). ";

                    const char* labels[2] = {"left-hand side", "right-hand side"};

                    for (size_t i = 0; i < 2; i++)
                    {
                        str << " " << labels[i] << ":";

                        if (knownValues[i]._type == KnownValueType::Int)
                        {
                            str << knownValues[1]._intVal;
                        }
                        else
                        {
                            str << "unknown";
                        }
                    }

                    g_compiler->ErrorStream(_location, CompileError::InvalidLiteral) << str.str();
                }
                break;

            case ParseTreeBinaryOpTypeMod:
            {
                if ((knownValues[1]._type == KnownValueType::Int) && IsPow2(knownValues[1]._intVal) &&
                    (knownValues[1]._intVal > 0))
                {
                    const LeafType* const lhsLeafType = dynamic_cast<const LeafType*>(_lhs->GetType());
                    assert(lhsLeafType);

                    if (BaseType::Int == lhsLeafType->_baseType)
                    {
                        // lhs is signed - Check to see if it is negative

                        // Move the sign bit to a 1-bit register
                        const AccessedRegister signBitReg = {
                            AllocateRegister(context._program, 1, RegisterType::Local, "ModNumeratorIsNegative")};

                        {
                            Operation shiftSignBitOp = {};

                            shiftSignBitOp._opcode = Opcode::BinaryOp;
                            shiftSignBitOp._flags._binaryOpType = ParseTreeBinaryOpTypeShr;

                            shiftSignBitOp._src.push_back(registers[0]->GetAccessedRegister());
                            shiftSignBitOp._src.push_back(lhsLeafType->_width - 1);

                            shiftSignBitOp._dst.push_back(signBitReg);

                            // Shift in sign bit
                            SignExtend(shiftSignBitOp, _lhs->GetType(), 0);

                            context._basicBlock->_operations.push_back(shiftSignBitOp);
                        }

                        // Compute output assuming positive input
                        const AccessedRegister positiveMod = {
                            AllocateRegister(context._program, context._writtenRegisters->GetType()->GetBitWidth(),
                                             RegisterType::Local, "PositiveMod")};

                        {
                            Operation andOp = {};

                            andOp._opcode = Opcode::BinaryOp;
                            andOp._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;

                            andOp._src.push_back(registers[0]->GetAccessedRegister());
                            andOp._src.push_back(mp_int(knownValues[1]._intVal - 1));
                            andOp._dst.push_back(positiveMod);

                            context._basicBlock->_operations.push_back(andOp);
                        }

                        // Compute output (assuming negative input)
                        // negate lhs (flip all bits and then add 1)
                        // Width is + 2 because there are 2 add operations
                        const AccessedRegister tempReg = {AllocateRegister(context._program, lhsLeafType->_width + 2,
                                                                           RegisterType::Local, "SignedModTempReg")};

                        {
                            Operation negateOp = {};

                            negateOp._opcode = Opcode::UnaryOp;
                            negateOp._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;

                            negateOp._src.push_back(registers[0]->GetAccessedRegister());
                            negateOp._dst.push_back(tempReg);

                            // Ensure this invert operation sign-extens the source operand
                            SignExtend(negateOp, _lhs->GetType(), 0);

                            context._basicBlock->_operations.push_back(negateOp);
                        }

                        {
                            Operation addOp = {};

                            addOp._opcode = Opcode::BinaryOp;
                            addOp._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

                            addOp._src.push_back(tempReg);
                            addOp._src.push_back(1);
                            addOp._dst.push_back(tempReg);

                            context._basicBlock->_operations.push_back(addOp);
                        }

                        // Do mod operation on positive numerator
                        {
                            Operation andOp = {};

                            andOp._opcode = Opcode::BinaryOp;
                            andOp._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;

                            andOp._src.push_back(tempReg);
                            andOp._src.push_back(mp_int(knownValues[1]._intVal - 1));
                            andOp._dst.push_back(tempReg);

                            context._basicBlock->_operations.push_back(andOp);
                        }

                        // Negate result (flip all bits and add 1)
                        {
                            Operation negateOp = {};

                            negateOp._opcode = Opcode::UnaryOp;
                            negateOp._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;

                            negateOp._src.push_back(tempReg);
                            negateOp._dst.push_back(tempReg);

                            context._basicBlock->_operations.push_back(negateOp);
                        }

                        {
                            Operation addOp = {};

                            addOp._opcode = Opcode::BinaryOp;
                            addOp._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

                            addOp._src.push_back(tempReg);
                            addOp._src.push_back(1);
                            addOp._dst.push_back(tempReg);

                            context._basicBlock->_operations.push_back(addOp);
                        }

                        // Choose between positive and negative results
                        op._opcode = Opcode::Select;

                        op._src.push_back(signBitReg);
                        op._src.push_back(positiveMod);
                        op._src.push_back(tempReg);

                        // Ensure negative result is sign-extended
                        SignExtend(op, _lhs->GetType(), 2);
                    }
                    else
                    {
                        op._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;

                        op._src.push_back(registers[0]->GetAccessedRegister());
                        op._src.push_back(mp_int(knownValues[1]._intVal - 1));
                    }
                }
                else
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidLiteral)
                        << "Mod operator requires both sides to be known at compile time, or the right-hand side to be "
                           "a power of 2 (known at compile time)";
                }
            }
            break;

            case ParseTreeBinaryOpTypeShl:
            case ParseTreeBinaryOpTypeShr:
            {
                op._flags._binaryOpType = _opType;

                op._src.push_back(registers[0]->GetAccessedRegister());
                op._src.push_back(registers[1]->GetAccessedRegister());

                // Do not sign-extend the shift amount
                // It is reinterpreted as an unsigned value
                SignExtend(op, _lhs->GetType(), 0);
            }
            break;

            default:
            {
                op._flags._binaryOpType = _opType;

                op._src.push_back(registers[0]->GetAccessedRegister());
                op._src.push_back(registers[1]->GetAccessedRegister());

                // Add flags indicating if sources should be sign-extended on read
                SignExtend(op, _lhs->GetType(), 0);
                SignExtend(op, _rhs->GetType(), 1);
            }
            }

            context._basicBlock->_operations.push_back(op);
        }
    }

    if (doConditionCoverage && !newExpressionCoverage)
    {
        // Set BinaryOpFlag to indicate to higher nodes that there is a binary op node deeper
        tracker->_conditionCoverageFlags._deeperBinaryOp = true;
    }
}

KnownValue BinaryOpNode::TryGetKnownValueImpl(KnownValueContext& context) const
{
    const Type* const lhsType = _lhs->GetType();
    const Type* const rhsType = _rhs->GetType();

    const size_t lhsWidth = lhsType->GetBitWidth();
    const size_t rhsWidth = rhsType->GetBitWidth();

    const KnownValue lhsValue = _lhs->TryGetKnownValue(context, lhsType);
    const KnownValue rhsValue = _rhs->TryGetKnownValue(context, rhsType);

    KnownValue result;

    if ((lhsValue._type == KnownValueType::Int) ^ (rhsValue._type == KnownValueType::Int))
    {
        // Only one value is known.  For logical OR and AND, it may still
        // be possible to determine the value of the whole expression
        const mp_int knownValue = (lhsValue._type == KnownValueType::Int) ? lhsValue._intVal : rhsValue._intVal;
        const size_t knownWidth = (lhsValue._type == KnownValueType::Int) ? lhsWidth : rhsWidth;
        const size_t unknownWidth = (lhsValue._type == KnownValueType::Int) ? rhsWidth : lhsWidth;

        if ((_opType == ParseTreeBinaryOpTypeLogicalOr) && (knownValue == 1))
        {
            // true OR anything == true
            result._type = KnownValueType::Int;
            result._intVal = 1;
        }

        if ((_opType == ParseTreeBinaryOpTypeLogicalAnd) && (knownValue == 0))
        {
            // false AND anything == false
            result._type = KnownValueType::Int;
            result._intVal = 0;
        }

        // 0 & (anything <= bit width) == 0
        if ((_opType == ParseTreeBinaryOpTypeAnd) && (knownWidth >= unknownWidth) && (knownValue == 0))
        {
            result._type = KnownValueType::Int;
            result._intVal = 0;
        }

        // all 1's | (anything <= bit width) == all 1's
        bool allOnes = (((knownValue + 1) & knownValue) == 0) && (knownValue != 0);
        if ((_opType == ParseTreeBinaryOpTypeOr) && (knownWidth >= unknownWidth) && allOnes)
        {
            result._type = KnownValueType::Int;
            result._intVal = knownValue;
        }
    }

    // Floating-point add/mul at compile time are not supported
    if (!IsFloatOperation() && (lhsValue._type == KnownValueType::Int) && (rhsValue._type == KnownValueType::Int))
    {
        // Child values are known
        result._type = KnownValueType::Int;

        const Literal lhsLiteral = {lhsValue._intVal, lhsType->GetBitWidth()};
        const Literal rhsLiteral = {rhsValue._intVal, rhsType->GetBitWidth()};

        switch (_opType)
        {
        case ParseTreeBinaryOpTypeShl:
        case ParseTreeBinaryOpTypeShr:
            // Validate the shift amount is not negative
            if (IsNegative(rhsValue._intVal, rhsType))
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidLiteral)
                    << "Negative shift amounts are not supported";
                throw std::runtime_error("Negative shift amount");
            }
            break;

        case ParseTreeBinaryOpTypeDiv:
            if (rhsValue._intVal == 0)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidLiteral) << "Compile time divide by zero";
                throw std::runtime_error("Divide by zero");
            }
            break;

        default:
            break;
        }

        result._intVal = ImplementBinaryOp(lhsLiteral, rhsLiteral, GetType()->GetBitWidth(), IsSignedLeafType(lhsType),
                                           IsSignedLeafType(rhsType), _opType);
    }

    return result;
}

void IntegerNode::GenerateIR(IRContext& context) const
{
    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    // Add an operation to the basic block reads the literal value
    Operation op = {};

    op._opcode = Opcode::Mov;

    const Literal literal = {GetTypedValue(), GetType()->GetBitWidth()};

    op.PushOperand(SourceOperand(literal), IsSignedLeafType(GetType()));
    op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister());
    op.InsertLocation(_location);

    context._basicBlock->_operations.push_back(op);
}

void FloatNode::GenerateIR(IRContext& context) const
{
    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    // Add an operation to the basic block reads the literal value
    // No need to set the sign-extension mask
    // Because integer nodes only hold unsigned values
    Operation op = {};

    op._opcode = Opcode::Mov;

    // Reinterpret the floating-point literal as an integer
    op._src.push_back(SourceOperand(FloatAsUint(_value)));
    op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister());
    op.InsertLocation(_location);

    context._basicBlock->_operations.push_back(op);
}

void BoolNode::GenerateIR(IRContext& context) const
{
    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    // Add an operation to the basic block reads the literal value
    Operation op = {};

    op._opcode = Opcode::Mov;

    op._src.push_back(SourceOperand(_value ? 1 : 0));
    op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister());
    op.InsertLocation(_location);

    context._basicBlock->_operations.push_back(op);
}

// Class that emits IR for reordering cases (reorder blocks, ordered loops)
class ReorderHelper
{
  public:
    ReorderHelper(IRContext& context, const Location location)
        : _context(context), _localSlotReg(nullptr), _location(location), _slotBits(0), _exitSlotReg(nullptr),
          _bodyValidReg(nullptr)
    {
    }

    // Called before the basic blocks are generated that will cause threads to run out of order
    void EmitPreamble()
    {
        // Assign locations to generated operations
        SetOperationLocation sol(_context, _location);

        // Allocate a local register to hold the reorder buffer slot id
        // It needs to hold enough bits for maxThreadCount + 1
        // The high bit alternates on each time through in the RTL implementation
        // GetCodeGenDeviceConfig()._minFifoDepth is used as a minimum, because the reorder buffer hardware does
        // not work correctly in degenerate cases like width = 1 The reorder buffer itself will be sized to be at least
        // GetCodeGenDeviceConfig()._minFifoDepth deep
        const size_t threadCountForSlotIdWidth =
            std::max(GetCodeGenDeviceConfig()._minFifoDepth, _context._function->_maxThreadCountInsideFunction);

        const size_t roundedMaxThreadCount = Log2RoundUp(threadCountForSlotIdWidth);

        _slotBits = roundedMaxThreadCount + 1;

        _localSlotReg = AllocateLeafRegister(*_context._program, BaseType::Uint, _slotBits, _location,
                                             RegisterType::Local, "ReorderSlotId");

        // Allocate a global register that will be used to allocate slot ids
        const ObjectPath basicBlockPath = _context.GetBasicBlockContainerPath();

        const std::string slotAllocatorName =
            _context.GenerateUniqueNameWithinPath(basicBlockPath, "ReorderSlotAllocator");

        const ObjectPath slotAllocatorPath = AppendToPath(basicBlockPath, slotAllocatorName);

        const AllocatedLeafRegister* const globalSlotReg =
            AllocateLeafRegister(*_context._program, BaseType::Uint, _slotBits, _location, RegisterType::Global,
                                 slotAllocatorName, slotAllocatorPath);

        // Setup properties of the global
        RegisterDescription& globalSlotRegDesc =
            _context._program->_registerTable[globalSlotReg->GetAccessedRegister()._registerIndex];
        assert(RegisterType::Global == globalSlotRegDesc._type);

        globalSlotRegDesc.Global()._hasInitialValue = true;
        globalSlotRegDesc.Global()._isConstant = false;
        globalSlotRegDesc.Global()._initialValue = 0;
        globalSlotRegDesc.Global()._writeCount = 0; // computed later

        // Allocate a local temporary register to hold the result of the +1 operation
        const AllocatedLeafRegister* const temporarySumReg = AllocateLeafRegister(
            *_context._program, BaseType::Uint, _slotBits, _location, RegisterType::Local, "ReorderSlotSum");

        // Emit IR that will fill in the slot id
        {
            // This will emit BeginAtomic and EndAtomic opcodes - to ensure the slot ID allocation is atomic
            AtomicBlock atomicBlock(_context, AtomicBlockType::Default, c_defaultAtomicBlockUpdateRate,
                                    _context.AllocateAtomicChainIndex(), _location);

            // Read the global ID value
            {
                Operation op = {};

                op._opcode = Opcode::Mov;

                op._src.push_back(SourceOperand(globalSlotReg->GetAccessedRegister()));
                op._dst.push_back(_localSlotReg->GetAccessedRegister());

                op.InsertLocation(_location);

                _context._basicBlock->_operations.push_back(op);
            }

            // Increment the global ID - note that wrapping is desired
            // this is broken up into 2 operations, writes to globals use the WriteGlobal opcode
            {
                Operation op = {};

                op._opcode = Opcode::BinaryOp;
                op._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

                op._src.resize(2);
                op._src[0] = SourceOperand(globalSlotReg->GetAccessedRegister());
                op._src[1] = SourceOperand(1);
                op._dst.push_back(temporarySumReg->GetAccessedRegister());

                _context._basicBlock->_operations.push_back(op);
            }

            {
                Operation op = {};

                op._opcode = Opcode::WriteGlobal;

                op._flags._writeGlobal._isPredicated = false;

                op._src.push_back(SourceOperand(temporarySumReg->GetAccessedRegister()));
                op._dst.push_back(globalSlotReg->GetAccessedRegister());

                _context._basicBlock->_operations.push_back(op);
            }
        }
    }

    void ReorderByLoopingEmitPreamble()
    {
        // Generate slot ID
        EmitPreamble();

        SetOperationLocation sol(_context, _location);

        // Define and initialize global register for tracking next thread to exit
        assert(_slotBits > 0);
        const ObjectPath basicBlockPath = _context.GetBasicBlockContainerPath();
        const std::string exitSlotIdName = _context.GenerateUniqueNameWithinPath(basicBlockPath, "ExitSlotId");
        const ObjectPath exitSlotIdPath = AppendToPath(basicBlockPath, exitSlotIdName);
        _exitSlotReg = AllocateLeafRegister(*_context._program, BaseType::Uint, _slotBits, _location,
                                            RegisterType::Global, exitSlotIdName, exitSlotIdPath);
        RegisterDescription& regDesc =
            _context._program->_registerTable[_exitSlotReg->GetAccessedRegister()._registerIndex];
        regDesc.Global()._hasInitialValue = true;
        regDesc.Global()._initialValue = 0;

        // Register indicating that loop body is valid (predicated true)
        _bodyValidReg =
            AllocateLeafRegister(*_context._program, BaseType::Uint, 1, _location, RegisterType::Local, "BodyValid");
        {
            Operation op = {};
            op._opcode = Opcode::Mov;

            if (_context.IsPredicated())
            {
                // Initialize bodyValid to the predicate value
                // If the loop is predicated off, then bodyValid is always false
                op._src.push_back(AccessedRegister{_context.GetPredicate()});
            }
            else
            {
                op._src.push_back(1);
            }

            op._dst.push_back(_bodyValidReg->GetAccessedRegister());
            _context._basicBlock->_operations.push_back(op);
        }
    }

    void ReorderByLoopingEmitPostamble(const AccessedRegister& loopConditionReg) const
    {
        SetOperationLocation sol(_context, _location);

        // bodyValid = bodyValid && loopCondition
        {
            Operation op = {};
            op._opcode = Opcode::BinaryOp;
            op._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;
            op._src.push_back(_bodyValidReg->GetAccessedRegister());
            op._src.push_back(loopConditionReg);
            op._dst.push_back(_bodyValidReg->GetAccessedRegister());
            _context._basicBlock->_operations.push_back(op);
        }

        // Atomic read-modify-write of exit slot id
        {
            AtomicBlock atomicBlock(_context, AtomicBlockType::Default, c_defaultAtomicBlockUpdateRate,
                                    _context.AllocateAtomicChainIndex(), _location);

            // Loop again if next iteration of body is valid or slot id is not the next
            // one allowed to exit
            // loopCondition = bodyValid || (slotId != exitSlotId);
            const AccessedRegister notExitSlotReg = {
                AllocateRegister(_context._program, 1, RegisterType::Local, "NotExitSlotId")};
            {
                Operation op = {};
                op._opcode = Opcode::BinaryOp;
                op._flags._binaryOpType = ParseTreeBinaryOpTypeNE;
                op._src.push_back(_localSlotReg->GetAccessedRegister());
                op._src.push_back(_exitSlotReg->GetAccessedRegister());
                op._dst.push_back(notExitSlotReg);
                _context._basicBlock->_operations.push_back(op);
            }
            {
                Operation op = {};
                op._opcode = Opcode::BinaryOp;
                op._flags._binaryOpType = ParseTreeBinaryOpTypeOr;
                op._src.push_back(_bodyValidReg->GetAccessedRegister());
                op._src.push_back(notExitSlotReg);
                op._dst.push_back(loopConditionReg);
                _context._basicBlock->_operations.push_back(op);
            }

            // Increment exit slot id if this thread will exit
            // notLoopCondition = !loopCondition;
            // exitSlotId += notLoopCondition
            const AccessedRegister notLoopConditionReg = {
                AllocateRegister(_context._program, 1, RegisterType::Local, "NotLoopCondition")};
            {
                Operation op = {};
                op._opcode = Opcode::UnaryOp;
                op._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;
                op._src.push_back(loopConditionReg);
                op._dst.push_back(notLoopConditionReg);
                _context._basicBlock->_operations.push_back(op);
            }
            assert(_slotBits > 0);
            const AccessedRegister newExitSlotReg = {
                AllocateRegister(_context._program, _slotBits, RegisterType::Local, "NewExitSlotId")};
            {
                Operation op = {};
                op._opcode = Opcode::BinaryOp;
                op._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;
                op._src.push_back(_exitSlotReg->GetAccessedRegister());
                op._src.push_back(notLoopConditionReg);
                op._dst.push_back(newExitSlotReg);
                _context._basicBlock->_operations.push_back(op);
            }
            {
                Operation op = {};
                op._opcode = Opcode::WriteGlobal;
                op._flags._writeGlobal._isPredicated = false;
                op._src.push_back(newExitSlotReg);
                op._dst.push_back(_exitSlotReg->GetAccessedRegister());
                _context._basicBlock->_operations.push_back(op);
            }
        }
    }

    const AllocatedLeafRegister* GetSlotRegister() const { return _localSlotReg; }

    const AllocatedLeafRegister* GetBodyValidRegister() const
    {
        assert(_bodyValidReg);
        return _bodyValidReg;
    }

    // Called after all unordered code is emitted, emits jump to a new basic block - threads will enter
    // that basic block in the correct order
    void EmitPostamble(BasicBlock* const afterReorderBlock)
    {
        assert(_context._basicBlock == afterReorderBlock);

        // Record that the reorder buffer must be large enough to hold the maximum thread count in the function
        // In the worst case, threads leave in the opposite order that they enter, which could cause all threads in a
        // function to enter the reorder buffer before they can leave
        assert(1 == afterReorderBlock->_inputFifoCount);
        afterReorderBlock->SetInputFifoMinDepth(0, afterReorderBlock->_function->_maxThreadCountInsideFunction);

        // Ensure the slot ID will not be optimized out
        afterReorderBlock->_liveInReg.push_back(_localSlotReg->GetAccessedRegister()._registerIndex);
    }

  private:
    IRContext& _context;

    const AllocatedLeafRegister* _localSlotReg;

    const AllocatedLeafRegister* _exitSlotReg;

    const AllocatedLeafRegister* _bodyValidReg;

    const Location _location;

    size_t _slotBits;
};

// Determines if the IR for this node can be merged with the current basic block
bool DoWhileLoopNode::MergeWithCurrentBasicBlock(IRContext& context) const
{
    // Don't merge in O0 debug mode
    if (GetCodeGenConfig()._debug && GetCodeGenConfig()._optimize == 0)
    {
        return false;
    }

    // Don't merge multiple start conditions together
    if (context._basicBlock->HasStartCondition())
    {
        return false;
    }

    // Don't merge with start of reset blocks. Global writes in the `atomic do`
    // are not supported if the basic block is merged.
    if (context._basicBlock->IsResetBlock())
    {
        return false;
    }

    // Don't merge if the previous basic block has appended stages
    if (!context._basicBlock->_stages.empty())
    {
        return false;
    }

    bool hasInlined = false;

    for (const Operation& op : context._basicBlock->_operations)
    {
        // Merging with an operation that performs actual computation
        // could increase the length of the critical path inside of the start condition
        if (GetOpPathLength(*context._program, op) > 0)
        {
            return false;
        }

        // Some operations are impossible to schedule inside of atomic-do
        // Others would change behavior (like Opcode::CycleCounter)
        if (!AllowMergeSubsequentAtomicDo(op._opcode))
        {
            return false;
        }

        if (Opcode::NoOp == op._opcode)
        {
            hasInlined = true;
        }
    }

    // We are merging, update this flag for already-generated inlines.
    context._basicBlock->_hasInlinedAtomicDoFunctions |= hasInlined;

    return true;
}

void DoWhileLoopNode::GenerateWaitFor(IRContext& context) const
{
    // Check if the atomic-do can be merged with the current basic block
    const bool mergeWithCurrentBasicBlock = MergeWithCurrentBasicBlock(context);

    BasicBlock* prevBasicBlock = nullptr;
    BasicBlock* waitBlock = nullptr;

    if (mergeWithCurrentBasicBlock)
    {
        // There should not already be a start condition in the basic block
        assert(!context._basicBlock->HasStartCondition());

        prevBasicBlock = nullptr;
        waitBlock = context._basicBlock;
    }
    else
    {
        // End the current basic block
        prevBasicBlock = FinalizeBasicBlock(context);

        // Create a new basic block for the code after the atomic-do statement
        waitBlock = context.CreateBasicBlock(_location);
        context._basicBlock = waitBlock;
    }

    // Scoped here so that SetOperationLocation can set locations in the basicBlock->_operations
    // before they are moved to _startConditionOperations
    {
        // Assign locations to generated operations
        SetOperationLocation sol(context, _location);

        const bool waitBlockEmpty = waitBlock->_operations.empty();
        // For non-empty waitBlock, waitBlockIt points to last operation of waitBlock
        auto waitBlockIt = waitBlock->_operations.end();
        if (!waitBlockEmpty)
        {
            waitBlockIt--;
        }

        // Generate loop body
        _body->GenerateIR(context);

        // Emit IR for the condition
        const AllocatedRegister* const conditionRegister = EvaluateExpression(context, _conditionStatement);

        // There should be no control flow in the condition
        if ((context._basicBlock != waitBlock) || (!waitBlock->_stages.empty()))
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidLoop)
                << "Control flow not allowed inside atomic do-while loop";
        }

        // Check for operations which are not allowed inside of atomic-do
        for (const Operation& op : waitBlock->_operations)
        {
            if (!OpcodeAllowedInAtomicBlock(op._opcode))
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidAtomic)
                    << "Unsupported operation inside of atomic do-while loop";
            }

            // memory read/write not allowed
            if (Opcode::LoadMemory == op._opcode)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidAtomic)
                    << "memory read not supported inside of atomic do-while loop";
            }

            if (Opcode::StoreMemory == op._opcode)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidAtomic)
                    << "memory write not supported inside of atomic do-while loop";
            }
        }

        std::set<size_t> readBeforeWriteRegisters;
        std::map<size_t, FileAndLineNumber> writeRegisters;

        // Update waitBlockIt to point to first new operation
        if (waitBlockEmpty)
        {
            waitBlockIt = waitBlock->_operations.begin();
        }
        else
        {
            waitBlockIt++;
        }

        for (auto it = waitBlockIt; it != waitBlock->_operations.end(); it++)
        {
            const Operation& op = *it;

            // Record local registers that are read before they are written
            for (const SourceOperand& src : op._src)
            {
                if (src.Type() == SourceOperandType::Register)
                {
                    const size_t r = src.GetAccessedRegister()._registerIndex;

                    const RegisterDescription& regDesc = context._program->_registerTable[r];

                    if (IsLocalRegisterType(regDesc._type))
                    {
                        // Check if register has been written to
                        const auto it = writeRegisters.find(r);
                        if (it == writeRegisters.end())
                        {
                            readBeforeWriteRegisters.insert(r);
                        }
                    }
                }
            }

            // Record local registers that are written
            for (const DestinationOperand& dst : op._dst)
            {
                if (dst.Type() == DestinationOperandType::Register)
                {
                    const size_t r = dst.GetAccessedRegister()._registerIndex;

                    const RegisterDescription& regDesc = context._program->_registerTable[r];

                    if (IsLocalRegisterType(regDesc._type))
                    {
                        // Save location for possible error
                        FileAndLineNumber location = {};
                        for (const FileAndLineNumber& fal : op._locations)
                        {
                            location = fal;
                            break;
                        }

                        writeRegisters[r] = location;
                    }
                }
            }
        }

        // Remove registers which are never written to
        std::vector<size_t> noWriteRegisters;
        for (const size_t r : readBeforeWriteRegisters)
        {
            if (writeRegisters.find(r) == writeRegisters.end())
            {
                noWriteRegisters.push_back(r);
            }
        }
        for (const size_t r : noWriteRegisters)
        {
            readBeforeWriteRegisters.erase(r);
        }

        size_t conditionRegIndex = c_invalidAccessedRegisterIndex;

        // Add an operation that consume the condition register
        {
            Operation op = {};

            op._opcode = Opcode::StartCondition;
            // Global writes are performed before start condition so they occur
            // on every loop iteration.
            op._flags._startCondition._globalsBefore = true;

            // loop condition is true when loop should be repeated
            const AccessedRegister loopConditionReg =
                dynamic_cast<const AllocatedLeafRegister*>(conditionRegister)->GetAccessedRegister();
            // wait for condition is true when wait for should exit - invert loop condition
            const AccessedRegister conditionReg = {AllocateRegister(
                context._program, 1, RegisterType::Local, "not_" + context.GetRegisterName(loopConditionReg))};
            {
                Operation op = {};

                op._opcode = Opcode::UnaryOp;
                op._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;

                op._src.push_back(loopConditionReg);
                op._dst.push_back(conditionReg);

                waitBlock->_operations.push_back(op);
            }

            if (context.IsPredicated())
            {
                // Transform the condition into "condition | !predicate"
                // In other words, if the predicate is false, don't bother waiting
                const AccessedRegister predicateReg = {context.GetPredicate()};

                const AccessedRegister inversePredicate = {AllocateRegister(
                    context._program, 1, RegisterType::Local, "not_" + context.GetRegisterName(predicateReg))};

                {
                    Operation op = {};

                    op._opcode = Opcode::UnaryOp;
                    op._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;

                    op._src.push_back(predicateReg);
                    op._dst.push_back(inversePredicate);

                    waitBlock->_operations.push_back(op);
                }

                const AccessedRegister combinedReg = {AllocateRegister(context._program, 1, RegisterType::Local,
                                                                       context.GetRegisterName(conditionReg) + "_or_" +
                                                                           context.GetRegisterName(inversePredicate))};

                {
                    Operation op = {};

                    op._opcode = Opcode::BinaryOp;
                    op._flags._binaryOpType = ParseTreeBinaryOpTypeOr;

                    op._src.push_back(conditionReg);
                    op._src.push_back(inversePredicate);
                    op._dst.push_back(combinedReg);

                    waitBlock->_operations.push_back(op);
                }

                op._src.push_back(combinedReg);

                conditionRegIndex = combinedReg._registerIndex;
            }
            else
            {
                op._src.push_back(conditionReg);

                conditionRegIndex = conditionReg._registerIndex;
            }

            waitBlock->_operations.push_back(op);
        }

        assert(conditionRegIndex != c_invalidAccessedRegisterIndex);

        assert(!waitBlock->_hasAtomicDoShadowRegisters);

        if (!readBeforeWriteRegisters.empty())
        {
            waitBlock->_hasAtomicDoShadowRegisters = true;

            // Get iterator to StartCondition op
            auto startConditionIt = waitBlock->_operations.end();
            {
                startConditionIt--;
                assert(startConditionIt->_opcode == Opcode::StartCondition);
            }

            const ObjectPath basicBlockPath = context._basicBlock->GetObjectPath();

            // Track first iteration of atomic do loop for each thread
            const std::string firstIterationSuffix = "_AtomicDoFirst";
            const ObjectPath firstIterationPath = AppendToPath(basicBlockPath, firstIterationSuffix);
            const AccessedRegister firstIterationReg = {
                AllocateRegister(context._program, 1, RegisterType::Global, "atomic_do_first", firstIterationPath)};
            {
                RegisterDescription& regDesc = context._program->_registerTable[firstIterationReg._registerIndex];
                regDesc.Global()._hasInitialValue = true;
                regDesc.Global()._initialValue = 1;
            }

            // Set firstIterationReg to 0 if condition is not met and to 1 if
            // condition is met (condition true indicates to exit the loop)
            {
                Operation op = {};
                op._opcode = Opcode::WriteGlobal;
                op._flags._writeGlobal._isPredicated = context.IsPredicated();
                op._src.push_back(AccessedRegister{conditionRegIndex});
                if (context.IsPredicated())
                {
                    op._src.push_back(AccessedRegister{context.GetPredicate()});
                }
                op._dst.push_back(firstIterationReg);
                waitBlock->_operations.insert(startConditionIt, op);
            }

            for (const size_t r : readBeforeWriteRegisters)
            {
                const RegisterDescription& regDesc = context._program->_registerTable[r];
                assert(regDesc._type == RegisterType::Local);

                // Create a global shadow register
                const std::string globalSuffix = "_" + regDesc._name + "_Global";
                const ObjectPath globalRegPath = AppendToPath(basicBlockPath, globalSuffix);
                const AccessedRegister globalReg = {AllocateRegister(
                    context._program, regDesc._width, RegisterType::Global, regDesc._name + "_global", globalRegPath)};

                // Read global value into local register at start of wait block if
                // this is not the first iteration
                {
                    Operation op = {};
                    op._opcode = Opcode::Select;
                    op._src.push_back(firstIterationReg);
                    op._src.push_back(globalReg);
                    op._src.push_back(AccessedRegister{r});
                    op._dst.push_back(AccessedRegister{r});
                    waitBlock->_operations.insert(waitBlockIt, op);
                }

                // Write local register value back to global register
                {
                    Operation op = {};
                    op._opcode = Opcode::WriteGlobal;
                    op._flags._writeGlobal._isPredicated = context.IsPredicated();
                    op._src.push_back(AccessedRegister{r});
                    if (context.IsPredicated())
                    {
                        op._src.push_back(AccessedRegister{context.GetPredicate()});
                    }
                    op._dst.push_back(globalReg);
                    waitBlock->_operations.insert(startConditionIt, op);
                }
            }
        }
    }

    // Move the generated operations to the special list of operations for the start condition
    waitBlock->_startConditionOperations = waitBlock->_operations;
    waitBlock->_operations.clear();

    // Remove redundant {Begin,End}Atomic
    waitBlock->_startConditionOperations.remove_if(
        [](const Operation& op) { return (op._opcode == Opcode::BeginAtomic) || (op._opcode == Opcode::EndAtomic); });

    assert(1 == waitBlock->_inputFifoCount);
    auto minFifoDepth = GetFifoDepthParam(context);
    // Use function thread count if no fifo depth specified
    if (minFifoDepth == 0)
    {
        minFifoDepth = context._function->_maxThreadCountInsideFunction;
    }
    waitBlock->SetInputFifoMinDepth(0, minFifoDepth);

    if (!mergeWithCurrentBasicBlock)
    {
        // Jump from the previous basic block to the new one
        AppendJump(prevBasicBlock, waitBlock);
    }
}

void LoopNode::GenerateIR(IRContext& context) const
{
    // Special case of atomic { do {} while() }
    // Generate wait-for
    if (context._isInAtomicDo)
    {
        GenerateWaitFor(context);
        return;
    }

    // Loop within atomic is not allowed
    if (!context._atomicBlockDescStack.empty())
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidAtomic) << "Illegal loop within an atomic block";
        throw std::runtime_error("Invalid atomic block");
    }

    // Must not be in an assignment
    assert(context._writtenRegisters == nullptr);

    assert((_mode == ParseTreeLoopModeOrdered) || (_mode == ParseTreeLoopModeUnordered) ||
           (_mode == ParseTreeLoopModeReorderByLooping));

    assert((ParseTreeLoopModeOrdered == _mode) || (ParseTreeLoopModeUnordered == _mode) ||
           (ParseTreeLoopModeReorderByLooping == _mode));

    if (context.FunctionHasOrderingRestrictions() && (ParseTreeLoopModeUnordered == _mode))
    {
        g_compiler->ErrorStream(_location, CompileError::UnorderedOperation)
            << "Unordered loops are not allowed in ordered functions";
    }

    // Allocate a register to hold the result of the loop condition
    const AllocatedLeafRegister* loopConditionRegister =
        AllocateLeafRegister(*context._program, BaseType::Uint, 1, _location, RegisterType::Local, "LoopCondition");

    GenerateBeforeLoop(context, loopConditionRegister);

    // Reorder buffer not needed in any of these cases:
    // 1. Loop has [[unordered]] attribute
    // 2. Range-for with compile-time known bound
    // 3. [[max_threads(1)]] on the containing function
    const bool needsReorderBuffer = (_mode == ParseTreeLoopModeOrdered) && CanReorderThreads(context) &&
                                    (context._function->_maxThreadCountInsideFunction > 1);

    ReorderHelper reorderHelper(context, _location);

    const bool reorderByLooping = (_mode == ParseTreeLoopModeReorderByLooping);

    if (needsReorderBuffer)
    {
        // Emit code in the current basic block that will allocate a slot in the reorder buffer
        reorderHelper.EmitPreamble();
    }

    if (reorderByLooping)
    {
        assert(!needsReorderBuffer);
        reorderHelper.ReorderByLoopingEmitPreamble();
    }

    // End the current basic block
    BasicBlock* const prevBasicBlock = FinalizeBasicBlock(context);

    assert(context._basicBlock == nullptr);
    assert(context._writtenRegisters == nullptr);

    // Generate a basic block which contains:
    // for while() and for() loops:
    //      1. the condition statement
    //      2. the statements in the body of the loop
    //      3. the increment statement
    // for do/while loops:
    //      1. the statements in the body of the loop
    //      2. the condition statement
    // the increment statement - for loops
    // the condition statement - do/while loops

    std::pair<BasicBlock*, BasicBlock*> bodyBlocks(nullptr, nullptr);

    const std::pair<Location, Location> lineBounds = GetLineBounds();

    bodyBlocks.first = context.CreateBasicBlock(lineBounds.first);
    context._basicBlock = bodyBlocks.first;

    {
        // If bodyValidReg is false, then the thread should have exited already but
        // is looping to maintain order of exiting loops. Use ConditionalLocalUpdates
        // to predicate these extra loop iterations.
        std::unique_ptr<ConditionalLocalUpdates> conditionalLocalUpdates;
        if (reorderByLooping)
        {
            conditionalLocalUpdates.reset(new ConditionalLocalUpdates(context, _location));
            conditionalLocalUpdates->BeginCase(
                reorderHelper.GetBodyValidRegister()->GetAccessedRegister()._registerIndex,
                ConditionalLocalUpdates::PredicateMode::Normal);
        }

        GenerateLoop(context, loopConditionRegister);

        if (reorderByLooping)
        {
            conditionalLocalUpdates->EndCase();
            // Emit code to decide whether thread should loop extra times to maintain ordering
            reorderHelper.ReorderByLoopingEmitPostamble(loopConditionRegister->GetAccessedRegister());
        }
    } // End of scope for conditionalLocalUpdates

    size_t loopAgainRegister = loopConditionRegister->GetAccessedRegister()._registerIndex;

    // If the loop is inside of predication - then the predicate must be taken into account when
    // deciding if the loop should be taken again.  If the predicate is false, then the loop should not be taken
    // In the case of reorderByLooping, the thread will loop until it can exit in order.
    if (context.IsPredicated() && !reorderByLooping)
    {
        // Compute loopCondition AND predicate
        context.PushPredicate(loopAgainRegister, _location);

        loopAgainRegister = context.GetPredicate();

        context.PopPredicate();
    }

    // Add code coverage
    if (GetCodeGenConfig()._codeCoverage)
    {
        Location conditionLocation = _location;

        // Code coverage for true case
        const AccessedRegister predicateReg = {loopAgainRegister};
        const std::string name = context.GenerateCodeCoverageName(conditionLocation, CodeCoverageType::LoopCondition);
        const std::string description = "Code coverage for loop condition at " + LocationToString(conditionLocation);
        CodeCoverage codeCoverage = {};
        codeCoverage._coverageType = CodeCoverageType::LoopCondition;
        codeCoverage._statements = lineBounds;
        codeCoverage._condition = conditionLocation;
        codeCoverage._case = "true";
        AddCodeCoverageContext(codeCoverage, context);

        OperationList codeCoverageOps = CreateCodeCoverageCounter(
            context._program, name, description, conditionLocation, predicateReg, codeCoverage, *context._basicBlock);
        context._basicBlock->_operations.splice(context._basicBlock->_operations.end(), codeCoverageOps);

        // Code coverage for false case
        context.PushInversePredicate(loopConditionRegister->GetAccessedRegister()._registerIndex, conditionLocation);
        const AccessedRegister invPredicateReg = {context.GetPredicate()};

        codeCoverage._case = "false";
        OperationList invCodeCoverageOps =
            CreateCodeCoverageCounter(context._program, name, description, conditionLocation, invPredicateReg,
                                      codeCoverage, *context._basicBlock);
        context._basicBlock->_operations.splice(context._basicBlock->_operations.end(), invCodeCoverageOps);

        context.PopPredicate();
    }

    // Finalize the last basic block
    bodyBlocks.second = FinalizeBasicBlock(context);

    // Record of the loop only contains 1 basic block
    if (bodyBlocks.first == bodyBlocks.second)
    {
        assert(!bodyBlocks.first->_isOneBBLoop);
        assert(!bodyBlocks.first->_oneBBLoopOrdered);

        bodyBlocks.first->_isOneBBLoop = true;
        bodyBlocks.first->_oneBBLoopOrdered = (_mode != ParseTreeLoopModeUnordered);
    }

    // Mark 1 basic block as having 2 inputs - to avoid a fifo merger
    BasicBlock* const basicBlockWith2Inputs = bodyBlocks.first;

    assert(1 == basicBlockWith2Inputs->_inputFifoCount);

    // The starting basic block of the loop will have 2 input fifos
    // to avoid a fifo merger and the extra fifo that implies
    basicBlockWith2Inputs->_inputFifoCount = 2;

    // Also mark the 2nd fifo as requiring a large thread count to avoid deadlock
    basicBlockWith2Inputs->SetInputFifoMinDepth(1, basicBlockWith2Inputs->_function->_maxThreadCountInsideFunction);

    if (!needsReorderBuffer)
    {
        // There can never be more than _maxThreadCountInsideFunction entries in the backward link FIFO
        // so no need to check for full/almost_full
        // Reorder buffers can report full right after reset (as they fill an internal memory)
        basicBlockWith2Inputs->SetInputFifoMaxDepth(1, basicBlockWith2Inputs->_function->_maxThreadCountInsideFunction);
    }

    // Create a new basic block for the code after the loop
    BasicBlock* const nextBlock = context.CreateBasicBlock(lineBounds.second);
    context._basicBlock = nextBlock;

    // Hook up the basic blocks

    // Jump from the previous basic block to the loop body
    AppendJump(prevBasicBlock, bodyBlocks.first);

    // Propagate [[fifo_depth(N)]]
    bodyBlocks.first->SetInputFifoMinDepth(0, GetFifoDepthParam(context));

    // Jump from the end of the loop to either the begining, or the block following the loop
    AppendConditionalJump(bodyBlocks.second, loopAgainRegister, bodyBlocks.first, nextBlock, lineBounds.second,
                          1, // If looping, jump to the secondary input fifo for the start basic block - avoids a fifo
                             // merger at the start of every loop
                          needsReorderBuffer ? reorderHelper.GetSlotRegister() : nullptr);

    if (needsReorderBuffer)
    {
        reorderHelper.EmitPostamble(nextBlock);
    }
}

void DoWhileLoopNode::GenerateBeforeLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const
{
}

void DoWhileLoopNode::GenerateLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const
{
    // Generate IR for the body of the loop
    _body->GenerateIR(context);

    PushPopConditionCoverageTracker createConditionCoverageTracker(
        true, context, CodeCoverageType::Condition, loopConditionRegister->GetAccessedRegister(), _location, nullptr);

    // Generate IR to evaluate the loop condition
    PushPopWrittenRegister pushPopWrittenRegister(context);

    context._writtenRegisters = loopConditionRegister;

    _conditionStatement->GenerateIR(context);
}

bool DoWhileLoopNode::CanReorderThreads(IRContext& context) const
{
    // There is no way to know at compile time
    // if all threads entering the loop will take the same number of trips
    return true;
}

void RangeForLoopNode::Reset() { _boundMinusOneRegister = nullptr; }

void RangeForLoopNode::GenerateBeforeLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const
{
    // Initialize induction variable to 0
    _declareNode->GenerateIR(context);

    const AllocatedLeafRegister* const inductionVariable =
        dynamic_cast<const AllocatedLeafRegister*>(_declareNode->GetRegisters(context.GetObjectName()));

    ClearAllocatedRegister(context, inductionVariable, _location);

    // Evaluate the loop bound
    const AllocatedRegister* boundRegister = EvaluateExpression(context, _bound);

    _boundMinusOneRegister = AllocateLeafRegister(*context._program, BaseType::Uint, _bound->GetType()->GetBitWidth(),
                                                  _location, RegisterType::Local, "BoundMinusOne");

    // Assign locations to generated operations
    SetOperationLocation sol(context, _location);

    // Compare the bound to zero
    // If the bound is zero, there will be 1 iteration through the loop with execution predicated off
    {
        Operation op = {};

        op._opcode = Opcode::BinaryOp;
        op._flags._binaryOpType = ParseTreeBinaryOpTypeNE;

        op._src.push_back(SourceOperand(0));
        op._src.push_back(dynamic_cast<const AllocatedLeafRegister*>(boundRegister)->GetAccessedRegister());
        op._dst.push_back(loopConditionRegister->GetAccessedRegister());

        context._basicBlock->_operations.push_back(op);
    }

    // Compute (bound - 1) - used in the induction variable check
    // The incremented induction variable cannot be used, because it may wrap
    {
        Operation op = {};

        op._opcode = Opcode::BinaryOp;
        op._flags._binaryOpType = ParseTreeBinaryOpTypeSub;

        op._src.push_back(dynamic_cast<const AllocatedLeafRegister*>(boundRegister)->GetAccessedRegister());
        op._src.push_back(SourceOperand(1));
        op._dst.push_back(_boundMinusOneRegister->GetAccessedRegister());

        context._basicBlock->_operations.push_back(op);
    }

    // If the bound is 0, then set _boundMinusOneRegister to 0, to cause the nop loop iteration to exit correctly
    {
        Operation op = {};

        op._opcode = Opcode::Select;

        op._src.push_back(loopConditionRegister->GetAccessedRegister());
        op._src.push_back(SourceOperand(0));
        op._src.push_back(_boundMinusOneRegister->GetAccessedRegister());

        op._dst.push_back(_boundMinusOneRegister->GetAccessedRegister());

        context._basicBlock->_operations.push_back(op);
    }
}

void RangeForLoopNode::GenerateIR(IRContext& context) const
{
    const KnownValue bound = _bound->TryGetKnownValue(context, _bound->GetType());

    if ((bound._type == KnownValueType::Int) && (bound._intVal == 0))
    {
        // the bound is known to be 0, don't emit any IR for the loop
    }
    else
    {
        LoopNode::GenerateIR(context);
    }
}

void RangeForLoopNode::GenerateLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const
{
    const boost::optional<size_t> bound = GetBound(context);

    // If the bound is known to be positive
    // then there is no need to predicate loop execution, the first trip through the loop will have execution enabled
    auto needPredicate = !bound || (*bound == 0);

    {
        PushPopConditionCoverageTracker pushPopConditionCoverageTracker(true, context, CodeCoverageType::Condition,
                                                                        loopConditionRegister->GetAccessedRegister(),
                                                                        _location, nullptr);
    }

    // Use the loop condition as a predicate - once the condition turns false there will be 1 dead trip through the loop
    // body
    std::unique_ptr<ConditionalLocalUpdates> conditionalLocalUpdates;
    if (needPredicate)
    {
        conditionalLocalUpdates.reset(new ConditionalLocalUpdates(context, _location));

        conditionalLocalUpdates->BeginCase(loopConditionRegister->GetAccessedRegister()._registerIndex,
                                           ConditionalLocalUpdates::PredicateMode::Normal);
    }

    // Generate IR for the body of the loop
    _body->GenerateIR(context);

    // Assign locations to generated operations
    SetOperationLocation sol(context, _location);

    const AllocatedLeafRegister* const inductionVariable =
        dynamic_cast<const AllocatedLeafRegister*>(_declareNode->GetRegisters(context.GetObjectName()));

    {
        // Compare the induction variable to bound-1
        Operation op = {};

        op._opcode = Opcode::BinaryOp;
        op._flags._binaryOpType = ParseTreeBinaryOpTypeNE;

        op._src.push_back(inductionVariable->GetAccessedRegister());
        op._src.push_back(_boundMinusOneRegister->GetAccessedRegister());
        op._dst.push_back(loopConditionRegister->GetAccessedRegister());

        context._basicBlock->_operations.push_back(op);
    }

    {
        // increment the induction variable
        Operation op = {};

        op._opcode = Opcode::BinaryOp;
        op._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

        op._src.push_back(inductionVariable->GetAccessedRegister());
        op._src.push_back(1);
        op._dst.push_back(inductionVariable->GetAccessedRegister());

        context._basicBlock->_operations.push_back(op);
    }

    if (needPredicate)
    {
        conditionalLocalUpdates->EndCase();
    }
}

boost::optional<size_t> RangeForLoopNode::GetBound(IRContext& context) const
{
    boost::optional<size_t> result;

    // Even if the bound is known at compile time, if it can be predicated
    // then the number of loop iterations is not truly known
    if (!context.IsPredicated())
    {
        const KnownValue bound = _bound->TryGetKnownValue(context, _bound->GetType());

        if (bound._type == KnownValueType::Int)
        {
            result = boost::optional<size_t>(MpToSizeT(bound._intVal));
        }
    }

    return result;
}

bool RangeForLoopNode::CanReorderThreads(IRContext& context) const
{
    const boost::optional<size_t> bound = GetBound(context);

    // If the bound is known at compile time
    // then all threads will take the same number of trips
    // no reorder buffer is necessary
    return !bound;
}

void UnrolledForLoopNode::GenerateIR(IRContext& context) const
{
    // Must not be in an assignment
    assert(context._writtenRegisters == nullptr);

    // Allocate register for induction variable and assign it it's initial value
    _declareNode->GenerateIR(context);

    // Determine the number of loop iterations
    const KnownValue bound = _bound->TryGetKnownValue(context, _bound->GetType());

    if (bound._type != KnownValueType::Int)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidLoop)
            << "`static for` loop bound is not known by the compiler";
    }

    const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(_declareNode);

    const std::string& inductionVariableName = declareNode->GetDeclaredName();

    for (size_t loopIteration = 0; loopIteration < bound._intVal; ++loopIteration)
    {
        // Update the symbol table with the new value of the induction variable
        // global namespace can be used because loop induction variable is local
        {
            GenerateIRVariableData variableData =
                context._typeContext.LookupSymbol(Scope(), inductionVariableName, _location);

            variableData._value = KnownValue(loopIteration);

            context._typeContext.UpdateSymbol(Scope(), inductionVariableName, variableData, _location);
        }

        // Also emit an opcode which will assign the value to the induction variable
        {
            const AllocatedLeafRegister* const allocatedLeafRegister =
                dynamic_cast<const AllocatedLeafRegister*>(declareNode->GetRegisters(context.GetObjectName()));

            Operation op = {};

            op._opcode = Opcode::Mov;

            op._src.push_back(loopIteration);
            op._dst.push_back(allocatedLeafRegister->GetAccessedRegister());

            op.InsertLocation(_location);

            context._basicBlock->_operations.push_back(op);
        }

        // Emit the body of the loop
        _body->GenerateIR(context);
    }
}

// Gets the depth of the return true for a given caller count
size_t GetReturnTreeDepth(const size_t callerCount)
{
    assert(callerCount > 0);

    if (1 == callerCount)
    {
        return 1;
    }
    else
    {
        unsigned long highestBitIndex = 0;

        _BitScanReverse64(&highestBitIndex, callerCount - 1);

        return highestBitIndex + 1;
    }
}

bool IsPow2(const mp_int& input) { return (input > 0) && (input & (input - 1)) == 0; }

size_t Log2RoundUp(const mp_int& input)
{
    size_t result = 0;

    size_t highestBitIndex = msb(input);

    if (IsPow2(input))
    {
        result = highestBitIndex;
    }
    else
    {
        result = highestBitIndex + 1;
    }

    return result;
}

mp_int RoundUpToPow2(const mp_int& input)
{
    mp_int result(0);

    bit_set(result, MpToSizeT(Log2RoundUp(input)));

    return result;
}

size_t Log2(const mp_int& input)
{
    assert(IsPow2(input));

    const size_t highestBitIndex = msb(input);

    return highestBitIndex;
}

uint32_t FloatAsUint(const float input) { return reinterpret_cast<const uint32_t*>(&input)[0]; }

size_t GetCallSiteIndexWidth(const size_t callerCount)
{
    size_t result = 0;

    if (callerCount > 0)
    {
        result = Log2RoundUp(callerCount);
    }

    return std::max<size_t>(1, result);
}
// Called after the call site count is known
// Generates IR for the enqueue operations to return to the correct call site
void ReturnNode::GenerateConditionalReturns(Program& program) const
{
    for (const DeferredReturn& deferredReturn : _deferredReturns)
    {
        BasicBlock& basicBlock = *deferredReturn._basicBlock;

        const Function* const function = basicBlock._function;

        const size_t callerCount = function->_functionNode->GetInstance(deferredReturn._objectName).GetCallSiteCount();

        // !_isSyncReturn means the thread is returning from an async function, so there is no need to enqueue the
        // thread into a new FIFO
        if (deferredReturn._isSyncReturn && (callerCount > 0))
        {
            // Add 1 return operation for each potential caller

            if (deferredReturn._callSiteIndexRegister)
            {
                // Adjust the width of the call index register
                RegisterDescription& callSiteRegDesc =
                    program._registerTable[deferredReturn._callSiteIndexRegister->GetAccessedRegister()._registerIndex];

                callSiteRegDesc._width = GetCallSiteIndexWidth(callerCount);
            }

            Stage* returnStage = nullptr;

            const auto appendStage = [&]()
            {
                Stage& newStage = AppendStage(&basicBlock, _location);
                returnStage = &newStage;
            };

            for (size_t callSiteIndex = 0; callSiteIndex < callerCount; callSiteIndex++)
            {
                const bool isResetCallReturn = (0 == callSiteIndex) && function->CallOnReset();

                if (callerCount > 1)
                {
                    // Add extra stages to make placement easier
                    for (size_t i = 0; i < GetCodeGenConfig()._returnStages; i++)
                    {
                        appendStage();
                    }
                }

                // Add a minimum of 1 pipeline stage after the basic block
                if (!returnStage)
                {
                    appendStage();
                }

                assert(returnStage);

                const AccessedRegister conditionRegister = {
                    AllocateRegister(program, 1, RegisterType::Local, "ReturnCondition")};

                {
                    // Compare dynamic call-site index with static call-site index
                    Operation op = {};

                    op.InsertLocation(_location);

                    if (deferredReturn._callSiteIndexRegister)
                    {
                        op._opcode = Opcode::BinaryOp;
                        op._flags._binaryOpType = ParseTreeBinaryOpTypeEQ;

                        op._src.resize(2);
                        op._src[0] = deferredReturn._callSiteIndexRegister->GetAccessedRegister();
                        op._src[1] = SourceOperand(callSiteIndex);
                    }
                    else
                    {
                        op._opcode = Opcode::Mov;
                        op.PushOperand(SourceOperand(1), false);
                    }

                    op._dst.push_back(conditionRegister);

                    basicBlock._operations.push_back(op);
                }

                const FunctionNode* const functionNode = function->_functionNode;

                const std::string objectName = function->_objectName;

                const GetSuccessorBlock getSuccessorCallback = [callSiteIndex, functionNode, objectName]()
                { return functionNode->GetInstance(objectName).GetReturnBlock(callSiteIndex); };

                {
                    Operation op = {};

                    op.InsertLocation(_location);

                    op._opcode = Opcode::Enqueue;

                    op._flags._enqueue._isPredicated = true;
                    op._flags._enqueue._predicateExecutionValue = true;
                    op._flags._enqueue._type = EnqueueType::ContextSaverCallee;

                    // Each thread can enqueue
                    op._flags._enqueue._modifiers._callRate = 1;

                    if (isResetCallReturn)
                    {
                        // This is the return from the initial call made after reset
                        op._flags._enqueue._successorFifo = function->_returnFifoRegisterIndex;
                    }
                    else
                    {
                        // This is a return to a regular function call
                        op._getSuccessorBlock = getSuccessorCallback;
                    }

                    DebuggerHintPushPredicate(returnStage->_operations);

                    op._src.push_back(conditionRegister);

                    // Add the register that holds the invocation instance ID - to ensure that the invocation instance
                    // is pipelined through the basic block This is null for ordered functions
                    if (deferredReturn._invocationInstanceRegister)
                    {
                        op._src.push_back(deferredReturn._invocationInstanceRegister->GetAccessedRegister());
                    }

                    if (_value && (_value->GetType()->GetBitWidth() > 0))
                    {
                        // Add the return value as a parameter to ensure that the return opcode
                        // is scheduled after the return value is computed - and to mark code the computes the return
                        // value to be marked as not-dead
                        const auto callback = [&](const AccessedRegister reg, const Type* const type)
                        { op._src.push_back(reg); };

                        deferredReturn._returnValueRegisters->VisitRegisters(callback);
                    }

                    returnStage->_operations.push_back(op);

                    DebuggerHintPopPredicate(returnStage->_operations);
                }
            }
        }
    }
}

void ReturnNode::Reset() { _deferredReturns.clear(); }

void ReturnNode::GenerateIR(IRContext& context) const
{
    if (_value && (_value->GetType()->GetBitWidth() > 0))
    {
        if (context._inlineCallCount > 0)
        {
            // Inline call, generate result into context._returnValueRegisters
            EvaluateExpressionIntoRegister(context, context._returnValueRegisters, _value);
        }
        else
        {
            // Non-inline call, use MovCrossFunction to ensure the return value is not optimized away by the
            // per-function optimizer
            EvaluateCrossFunctionExpressionIntoRegister(context, context._returnValueRegisters, _value);
        }

        // Increment reference counts on strings contained in the return value
        const auto callback = [&](const AccessedRegister reg, const Type* const type)
        {
            if (g_compiler->GetStringType() == type)
            {
                Operation op = {};

                op.InsertLocation(_location);

                op._opcode = Opcode::ReferenceString;

                PushPredicateOrLiteralOne(context, op);

                op._src.push_back(reg);

                op.PushOperand(1, false);

                context._basicBlock->_operations.push_back(op);
            }
        };

        context._returnValueRegisters->VisitRegisters(callback);
    }

    // Add jump back to caller
    // _inlineCallCount > 0 means that this is a return from an inline function
    // there is no need to jump
    if (0 == context._inlineCallCount)
    {
        // Return must be inside of a function, and not inside of an expression
        assert(context._function);
        assert(context._writtenRegisters == nullptr);

        // IR is generated later (after the call site count is known)
        DeferredReturn deferredReturn = {};

        deferredReturn._basicBlock = context._basicBlock;
        deferredReturn._objectName = context.GetObjectName();
        deferredReturn._isSyncReturn = !context._function->IsAsync();
        deferredReturn._returnValueRegisters = context._returnValueRegisters;
        deferredReturn._callSiteIndexRegister = context._callSiteIndexRegister;
        deferredReturn._invocationInstanceRegister = context._invocationInstanceRegister;

        _deferredReturns.push_back(deferredReturn);
    }
}

void SizeOfNode::GenerateIR(IRContext& context) const
{
    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    // Add an operation to the basic block reads the literal value
    const AllocatedLeafRegister* const dest = dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters);

    Operation op = {};

    op._opcode = Opcode::Mov;

    op._src.push_back(SourceOperand(_result));
    op._dst.push_back(dest->GetAccessedRegister());
    op.InsertLocation(_location);

    context._basicBlock->_operations.push_back(op);
}

std::vector<AccessedRegister> GetAccessedRegisterList(const AllocatedRegister* const reg)
{
    std::vector<AccessedRegister> result;

    const auto callback = [&](const AccessedRegister reg, const Type* const type) { result.push_back(reg); };

    reg->VisitRegisters(callback);

    return result;
}

void CastNode::GenerateIR(IRContext& context) const
{
    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    const Type* const destType = _castType->GetType();

    const Type* const sourceType = _value->GetType();

    // Check if _value should be saved as a leaf condition for condition coverage
    const bool saveLeafCondition = SaveLeafCondition(context, _value->IsConditionCoverageLeaf());
    const CallNode* const callNode = dynamic_cast<const CallNode*>(_value);

    if (IsLeafType(destType) && IsLeafType(sourceType))
    {
        // Have the child expression write the result into a register
        // and then move to the result
        // This enables sign extension to work
        const AllocatedRegister* const childReg =
            sourceType->AllocateRegisters(*context._program, RegisterType::Local, "CastSource");

        EvaluateExpressionIntoRegister(context, childReg, _value);

        Move(context, context._writtenRegisters, childReg, Opcode::Mov, _location);

        // Save leaf condition for condition coverage
        if (saveLeafCondition)
        {
            const AllocatedLeafRegister* const childLeafReg = dynamic_cast<const AllocatedLeafRegister*>(childReg);
            assert(childLeafReg);
            assert(sourceType->GetBitWidth() == 1);
            assert(destType->GetBitWidth() == 1);

            ConditionCoverageTracker& tracker = context.GetConditionCoverageTracker();

            ConditionCoverageLeafConditionEntry entry = {};
            entry._str = _value->PrettyPrint();
            entry._inverting = tracker._conditionCoverageFlags._inverting;
            entry._registerIndex = childLeafReg->GetAccessedRegister()._registerIndex;
            entry._callNode = (bool)callNode;

            tracker._conditionCoverageLeafCondition = entry;
        }
    }
    else
    {
        assert(destType->GetBitWidth() == sourceType->GetBitWidth());

        // Have the child expression write the result into a new set of registers
        const AllocatedRegister* const tempRegisters = EvaluateExpression(context, _value);

        // Save leaf condition for condition coverage
        if (saveLeafCondition)
        {
            const AllocatedLeafRegister* const tempLeafRegister =
                dynamic_cast<const AllocatedLeafRegister*>(tempRegisters);
            assert(tempLeafRegister);
            assert(sourceType->GetBitWidth() == 1);
            assert(destType->GetBitWidth() == 1);

            ConditionCoverageTracker& tracker = context.GetConditionCoverageTracker();

            ConditionCoverageLeafConditionEntry entry = {};
            entry._str = _value->PrettyPrint();
            entry._inverting = tracker._conditionCoverageFlags._inverting;
            entry._registerIndex = tempLeafRegister->GetAccessedRegister()._registerIndex;
            entry._callNode = (bool)callNode;

            tracker._conditionCoverageLeafCondition = entry;
        }

        // Gather into a register with type = destType
        const AllocatedRegister* const destRegister = destType->AllocateRegisters(
            *context._program, RegisterType::Local, "cast_" + _value->GetExpressionString());

        // For each register in the destination
        size_t destOffset = 0;

        const auto destCallback = [&](const AccessedRegister destReg, const Type* const destLeafType)
        {
            const size_t destWidth = destLeafType->GetBitWidth();

            const size_t destEnd = destOffset + destWidth;

            // Begin a gather operation, which will write to the destination
            Operation op = {};

            op.InsertLocation(_location);

            op._opcode = Opcode::Gather;

            op._dst.push_back(destReg);

            size_t sourceOffset = 0;

            // Allocate memory to hold the gather table
            op._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();

            // Find all source registers which overlap with the range [destOffset, destOffset + destWidth]
            const auto sourceCallback = [&](const AccessedRegister sourceReg, const Type* const sourceLeafType)
            {
                const size_t sourceWidth = sourceLeafType->GetBitWidth();

                const size_t sourceEnd = sourceOffset + sourceWidth;

                if ((sourceOffset < destEnd) && (sourceEnd > destOffset))
                {
                    const size_t intersectionStart = std::max<size_t>(sourceOffset, destOffset);

                    const size_t intersectionEnd = std::min<size_t>(sourceEnd, destEnd);

                    assert(intersectionEnd > intersectionStart);

                    // Add an element to the table of gathers
                    GatherEntry gatherEntry = {};

                    gatherEntry._sourceOffset = intersectionStart - sourceOffset;
                    gatherEntry._destOffset = intersectionStart - destOffset;
                    gatherEntry._numBits = intersectionEnd - intersectionStart;

                    op._flags._gather._entries->push_back(gatherEntry);

                    // Add the source to the operation
                    op._src.push_back(sourceReg);
                }

                sourceOffset += sourceWidth;
            };

            tempRegisters->VisitRegisters(sourceCallback);

            assert(sourceOffset == sourceType->GetBitWidth());

            context._basicBlock->_operations.push_back(op);

            destOffset += destWidth;
        };

        // Move from destRegister into the context._writtenRegisters
        // for sign-extension
        destRegister->VisitRegisters(destCallback);

        Move(context, context._writtenRegisters, destRegister, Opcode::Mov, _location);
    }
}

std::string LocationToString(const Location& loc, bool stripLeadingPath, const std::string& separator,
                             bool appendLineNumber)
{
    if (loc._valid)
    {
        std::string fileName = GetCodeGenConfig()._fileNames[loc._fileIndex];
        // Strip the path from the file name
        if (stripLeadingPath)
        {
            fileName = StripLeadingPath(fileName);
        }

        std::ostringstream str;
        str << fileName;
        if (appendLineNumber)
        {
            str << separator << loc._beginLine;
        }
        return str.str();
    }
    else
    {
        return "";
    }
}

// Add information from context to code coverage
void AddCodeCoverageContext(CodeCoverage& codeCoverage, const IRContext& context)
{
    // Convert instances in _context._functionInstanceStack
    // to names for code coverage stack
    std::stack<std::string> functionNameStack;

    for (const FunctionInstance& fi : context._functionInstanceStack)
    {
        const std::string functionName = fi._functionNode->GetName();

        const ClassType* const classType = fi._functionNode->GetClassType();

        const std::string name =
            classType == g_compiler->GetGlobalClassType() ? functionName : classType->GetName() + "_" + functionName;

        functionNameStack.push(name);
    }

    codeCoverage._functionNameStack = FunctionNameStackToString(functionNameStack);
    codeCoverage._basicBlock = context._basicBlock;
}

void AddIfStatementCodeCoverage(IRContext& context, const std::string& name, const size_t targetRegIndex,
                                const Location condloc, bool validStatementloc,
                                const std::pair<Location, Location> statementloc, bool invert)
{
    assert(context._program->_registerTable[targetRegIndex]._width == 1);

    std::string description = "Code coverage for If statement at " + LocationToString(condloc);

    // Create code coverage counter
    const AccessedRegister targetReg = {targetRegIndex};
    CodeCoverage codeCoverage = {};
    codeCoverage._coverageType = CodeCoverageType::IfStatement;
    if (validStatementloc)
    {
        codeCoverage._statements = statementloc;
    }
    codeCoverage._condition = condloc;
    codeCoverage._case = invert ? "false" : "true";
    AddCodeCoverageContext(codeCoverage, context);

    OperationList codeCoverageOps = CreateCodeCoverageCounter(
        context._program, name, description, condloc, SourceOperand(targetReg), codeCoverage, *context._basicBlock);
    context._basicBlock->_operations.splice(context._basicBlock->_operations.end(), codeCoverageOps);
}

bool BinaryOpNode::IsLogicalOperation() const
{
    return _opType == ParseTreeBinaryOpTypeLogicalAnd || _opType == ParseTreeBinaryOpTypeLogicalOr ||
           _opType == ParseTreeBinaryOpTypeLogicalXor;
}

bool BinaryOpNode::IsBitOperation() const
{
    bool opType = _opType == ParseTreeBinaryOpTypeAnd || _opType == ParseTreeBinaryOpTypeOr ||
                  _opType == ParseTreeBinaryOpTypeXor;
    bool bitWidth = GetType()->GetBitWidth() == 1;
    return opType && bitWidth;
}

bool SaveLeafCondition(IRContext& context, bool childIsLeaf)
{
    // If we are tracking condition coverage leaf conditions and the child is
    // a leaf, then disable tracking further and save child as leaf condition
    if (context.HasConditionCoverageTracker())
    {
        ConditionCoverageTracker& tracker = context.GetConditionCoverageTracker();

        if (tracker._conditionCoverageFlags._enabled && tracker._conditionCoverageFlags._leafConditionTracking &&
            childIsLeaf)
        {
            tracker._conditionCoverageFlags._leafConditionTracking = false;
            return true;
        }
        else
        {
            return false;
        }
    }
    else
    {
        return false;
    }
}

// Create coverage counters for (conditionReg && nonMaskingConditionReg) and
// (!conditionReg && nonMaskingConditionReg).
// originalConditionRegister indicates that conditionRegister is the top-level
// condition and is used for the naming strings.
void CreateConditionCoverageHelper(IRContext& context, const std::string name, const AccessedRegister& conditionReg,
                                   const AccessedRegister& nonMaskingConditionReg, bool hasNonMaskingCondition,
                                   const std::string& conditionString, const std::string& nonMaskingConditionString,
                                   const Location& condloc, bool originalConditionRegister,
                                   const CodeCoverageType codeCoverageType, const std::string& topLevelConditionString)
{
    OperationList operationList;

    {
        SetOperationLocation sol(operationList, condloc);

        // Invert conditionReg
        const AccessedRegister invConditionReg = {AllocateRegister(context._program, 1, RegisterType::Local, name)};
        {
            Operation invOp = {};
            invOp._opcode = Opcode::UnaryOp;
            invOp._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;
            invOp._src.push_back(conditionReg);
            invOp._dst.push_back(invConditionReg);
            operationList.push_back(invOp);
        }

        // for {!conditionReg, conditionReg}
        for (size_t i = 0; i < 2; i++)
        {
            // AND conditionReg with nonMaskingCondition
            const AccessedRegister combinedReg = {
                AllocateRegister(context._program, 1, RegisterType::Local, name + "_non_masking_" + std::to_string(i))};
            if (hasNonMaskingCondition)
            {
                Operation andOp = {};
                andOp._opcode = Opcode::BinaryOp;
                andOp._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;
                andOp._src.push_back(i == 0 ? invConditionReg : conditionReg);
                andOp._src.push_back(nonMaskingConditionReg);
                andOp._dst.push_back(combinedReg);
                operationList.push_back(andOp);
            }
            else
            {
                Operation mvOp = {};
                mvOp._opcode = Opcode::Mov;
                mvOp._src.push_back(i == 0 ? invConditionReg : conditionReg);
                mvOp._dst.push_back(combinedReg);
                operationList.push_back(mvOp);
            }

            std::string caseString = "(" + conditionString + ")";
            if (originalConditionRegister)
            {
                caseString += (i == 0 ? "->0" : "->1");
            }
            else
            {
                caseString += (i == 0 ? "_0" : "_1");
            }
            std::string description = CodeCoverageTypeToString(codeCoverageType) + " coverage for ";
            if (!topLevelConditionString.empty())
            {
                description += "top-level condition " + topLevelConditionString + " and ";
            }
            description += "sub-expression condition " + caseString;
            if (hasNonMaskingCondition)
            {
                description += " with non-masking condition " + nonMaskingConditionString;
            }
            description += " for condition at " + LocationToString(condloc);

            // Extra information for code coverage
            CodeCoverage codeCoverage = {};
            assert(codeCoverageType == CodeCoverageType::Condition || codeCoverageType == CodeCoverageType::Expression);
            codeCoverage._coverageType = codeCoverageType;
            codeCoverage._condition = condloc;
            codeCoverage._case = caseString;
            if (hasNonMaskingCondition)
            {
                codeCoverage._case += " with non-masking condition (" + nonMaskingConditionString + ")";
            }
            AddCodeCoverageContext(codeCoverage, context);

            OperationList codeCoverageOps = CreateCodeCoverageCounter(context._program, name, description, condloc,
                                                                      combinedReg, codeCoverage, *context._basicBlock);
            operationList.splice(operationList.end(), codeCoverageOps);
        }
    }
    context._basicBlock->_operations.splice(context._basicBlock->_operations.end(), operationList);
}

// Create code coverage counters for the passed conditionCoverageEntry which
// corresponds to a single leaf condition
void CreateConditionCoverage(IRContext& context, const std::string& conditionString,
                             const ConditionCoverageEntry conditionCoverageEntry,
                             const AccessedRegister& originalConditionRegister, const Location& condloc,
                             const std::string& name, const CodeCoverageType codeCoverageType,
                             const std::string& topLevelConditionString)
{
    // Operation list for combining conditions
    OperationList operationList;

    // Register with combined non-masking condition
    const AccessedRegister combinedNonMaskingReg = {
        AllocateRegister(context._program, 1, RegisterType::Local, name + "_non_masking_condition")};

    std::string nonMaskingConditionString = "";

    // Create OR of all non-masking condition entries
    bool first = true;
    bool invertingSeen = false;
    bool nonInvertingSeen = false;

    {
        SetOperationLocation sol(operationList, condloc);

        // Initialize combinedNonMaskingReg to
        {
            Operation op = {};

            op._opcode = Opcode::Clear;
            op._dst.push_back(combinedNonMaskingReg);

            operationList.push_back(op);
        }

        for (const auto& nonMaskingCondition : conditionCoverageEntry._nonMaskingConditions)
        {
            // AND together non-masking condition sub-entries
            const AccessedRegister nonMaskingConditionReg = {
                AllocateRegister(context._program, 1, RegisterType::Local, name + "_non_masking_condition")};

            std::string nonMaskingConditionStr = "";
            for (size_t i = 0; i < nonMaskingCondition._entries.size(); i++)
            {
                const NonMaskingConditionSubEntry& entry = nonMaskingCondition._entries[i];

                const AccessedRegister conditionReg = {entry._registerIndex};
                const AccessedRegister invConditionReg = {AllocateRegister(context._program, 1, RegisterType::Local,
                                                                           name + "_non_masking_" + std::to_string(i))};

                // Invert condition if needed
                const bool invert = entry._invert;
                if (invert)
                {
                    Operation invOp = {};
                    invOp._opcode = Opcode::UnaryOp;
                    invOp._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;
                    invOp._src.push_back(conditionReg);
                    invOp._dst.push_back(invConditionReg);
                    operationList.push_back(invOp);
                }

                // Move first condition
                if (i == 0)
                {
                    Operation mvOp = {};
                    mvOp._opcode = Opcode::Mov;
                    mvOp._src.push_back(invert ? invConditionReg : conditionReg);
                    mvOp._dst.push_back(nonMaskingConditionReg);
                    operationList.push_back(mvOp);
                }
                // AND with subsequent conditions
                else
                {
                    Operation andOp = {};
                    andOp._opcode = Opcode::BinaryOp;
                    andOp._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;
                    andOp._src.push_back(nonMaskingConditionReg);
                    andOp._src.push_back(invert ? invConditionReg : conditionReg);
                    andOp._dst.push_back(nonMaskingConditionReg);
                    operationList.push_back(andOp);
                }

                // Construct non-masking condition string
                if (i != 0)
                {
                    nonMaskingConditionStr += " && ";
                }
                nonMaskingConditionStr += entry._str;
            }

            // Move first condition into combined register
            if (first)
            {
                Operation mvOp = {};
                mvOp._opcode = Opcode::Mov;
                mvOp._src.push_back(nonMaskingConditionReg);
                mvOp._dst.push_back(combinedNonMaskingReg);
                operationList.push_back(mvOp);

                if (conditionCoverageEntry._nonMaskingConditions.size() == 1)
                {
                    nonMaskingConditionString = nonMaskingConditionStr;
                }
                else
                {
                    nonMaskingConditionString = "(" + nonMaskingConditionStr + ")";
                }
            }
            // OR subsequent conditions with combined register
            else
            {
                Operation orOp = {};
                orOp._opcode = Opcode::BinaryOp;
                orOp._flags._binaryOpType = ParseTreeBinaryOpTypeOr;
                orOp._src.push_back(combinedNonMaskingReg);
                orOp._src.push_back(nonMaskingConditionReg);
                orOp._dst.push_back(combinedNonMaskingReg);
                operationList.push_back(orOp);

                nonMaskingConditionString += " || (" + nonMaskingConditionStr + ")";
            }

            if (nonMaskingCondition._inverting)
            {
                invertingSeen = true;
            }
            if (!nonMaskingCondition._inverting)
            {
                nonInvertingSeen = true;
            }

            first = false;
        }

        // Apply predicate
        if (context.IsPredicated())
        {
            const AccessedRegister predicateReg = {context.GetPredicate()};

            Operation andOp = {};
            andOp._opcode = Opcode::BinaryOp;
            andOp._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;
            andOp._src.push_back(combinedNonMaskingReg);
            andOp._src.push_back(predicateReg);
            andOp._dst.push_back(combinedNonMaskingReg);
            operationList.push_back(andOp);
        }
    }

    // Add operations to basic block
    context._basicBlock->_operations.splice(context._basicBlock->_operations.end(), operationList);
    // If first flag is set, then at least one non-masking condition was seen
    const bool hasNonMaskingCondition = !first;

    // Leaf condition node
    const AccessedRegister conditionReg = {conditionCoverageEntry._registerIndex};

    // Create condition coverage for (conditionReg)_0 and (conditionReg)_0
    CreateConditionCoverageHelper(context, name, conditionReg, combinedNonMaskingReg, hasNonMaskingCondition,
                                  conditionString, nonMaskingConditionString, condloc, false, codeCoverageType,
                                  topLevelConditionString);
    // If there are inverting and non-inverting leaf conditions or a leaf
    // condition in an XOR, then track ->0 and ->1
    if (conditionCoverageEntry._withinXor || (invertingSeen && nonInvertingSeen))
    {
        CreateConditionCoverageHelper(context, name, originalConditionRegister, combinedNonMaskingReg,
                                      hasNonMaskingCondition, conditionString, nonMaskingConditionString, condloc, true,
                                      codeCoverageType, topLevelConditionString);
    }
}

void IfNode::GenerateIR(IRContext& context) const
{
    // Check to see if the condition is known at compile time
    const KnownValue conditionValue = _condition->TryGetKnownValue(context, _condition->GetType());

    if (conditionValue._type == KnownValueType::Int)
    {
        if (conditionValue._intVal == 1)
        {
            _lhs->GenerateIR(context);
        }
        else if (_rhs)
        {
            _rhs->GenerateIR(context);
        }
    }
    else
    {
        const Location& condloc = GetLocation();

        const AllocatedLeafRegister* const conditionRegister =
            dynamic_cast<const AllocatedLeafRegister*>(_condition->GetType()->AllocateRegisters(
                *context._program, RegisterType::Local, _condition->GetExpressionString()));
        {
            PushPopConditionCoverageTracker pushPopConditionCoverageTracker(true, context, CodeCoverageType::Condition,
                                                                            conditionRegister->GetAccessedRegister(),
                                                                            condloc, _condition);

            // Evaluate the if condition
            EvaluateExpressionIntoRegister(context, conditionRegister, _condition);
        }

        // Handle predicated updates to local variables
        ConditionalLocalUpdates conditionalLocalUpdates(context, _location);

        conditionalLocalUpdates.BeginCase(conditionRegister->GetAccessedRegister()._registerIndex,
                                          ConditionalLocalUpdates::PredicateMode::Normal);

        // Generate code inside of the if
        _lhs->GenerateIR(context);

        // Add code coverage for true/if case
        std::string codeCoverageName;
        if (GetCodeGenConfig()._codeCoverage)
        {
            codeCoverageName = context.GenerateCodeCoverageName(condloc, CodeCoverageType::IfStatement);
            AddIfStatementCodeCoverage(context, codeCoverageName, context.GetPredicate(), condloc, true,
                                       _lhs->GetLineBounds(), false);
        }

        // Remove the predication that was applied above
        conditionalLocalUpdates.EndCase();

        if (_rhs)
        {
            // This line number is for the "else if/else" lines
            // GenerateIR below only emits line numbers for the statements within the condition
            EmitLineNumber(context._basicBlock, _rhs->GetLocation());

            // Set the predicate to the opposite value
            conditionalLocalUpdates.BeginCase(conditionRegister->GetAccessedRegister()._registerIndex,
                                              ConditionalLocalUpdates::PredicateMode::Inverse);

            // Generate code inside of the else
            _rhs->GenerateIR(context);

            if (GetCodeGenConfig()._codeCoverage)
            {
                // Add code coverage for false/else case
                AddIfStatementCodeCoverage(context, codeCoverageName, context.GetPredicate(), GetLocation(), true,
                                           _rhs->GetLineBounds(), true);
            }

            // Remove the predication that was applied above
            conditionalLocalUpdates.EndCase();

            // Because the if/else ladder ends with an "else"
            // If a variable is written by all cases
            // then it is impossible for the value
            // of that variable before the if/else ladder
            // to affect the variable afterwards
            conditionalLocalUpdates.SetComplete();
        }
        else if (GetCodeGenConfig()._codeCoverage)
        {
            // Add code coverage for false/else case even if there are no statements associated with it
            context.PushInversePredicate(conditionRegister->GetAccessedRegister()._registerIndex, _location);
            AddIfStatementCodeCoverage(context, codeCoverageName, context.GetPredicate(), GetLocation(), false,
                                       std::pair<Location, Location>(), true);
            context.PopPredicate();
        }
    }
}

void AtomicNode::GenerateIR(IRContext& context) const
{
    BasicBlock* const initialBasicBlock = context._basicBlock;

    const size_t updateRate = _updateRate.GetUnmodifiedUpdateRate();

    bool generateAtomic = true;
    // Nested atomic
    if (!context._atomicBlockDescStack.empty())
    {
        const size_t outerUpdateRate = context._atomicBlockDescStack.top()._updateRate;

        // Nested atomic that is less restrictive can be ignored
        if (updateRate >= outerUpdateRate)
        {
            generateAtomic = false;
        }
    }

    // Look for do-while loop inside atomic
    bool hasLoopNode = false;
    const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
    {
        const DoWhileLoopNode* const loopNode = dynamic_cast<const DoWhileLoopNode*>(node);

        if (loopNode)
        {
            hasLoopNode = true;
            // Do not generate atomic, instead generate wait-for during loop generation
            generateAtomic = false;
        }

        recurseCallback();
    };

    VisitContext visitContext = {};
    VisitContext::PushPopScope outerScope(visitContext);
    _body->Visit(callback, visitContext);

    {
        // This will emit BeginAtomic and EndAtomic opcodes
        std::unique_ptr<AtomicBlock> atomicBlock;
        if (generateAtomic)
        {
            atomicBlock = std::make_unique<AtomicBlock>(context, AtomicBlockType::Default, updateRate,
                                                        context.AllocateAtomicChainIndex(), GetLocation());
        }

        std::unique_ptr<PushPopIsInAtomicDo> pushPopIsInAtomicDo;
        if (hasLoopNode)
        {
            pushPopIsInAtomicDo = std::make_unique<PushPopIsInAtomicDo>(context);
        }

        // Emit IR for the body
        _body->GenerateIR(context);

        // Save iterator to BeginAtomic op for use in HandleRmwMemory
        if (generateAtomic)
        {
            context._beginAtomicIt = atomicBlock->GetBeginAtomicIt();
        }

        // A new basic block should not have been started
        if (!hasLoopNode && initialBasicBlock != context._basicBlock)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidAtomic)
                << "Illegal control flow within an atomic block";
            throw std::runtime_error("Invalid atomic block");
        }

        // Check that atomicBlockDescStack entry exists
        assert(Implies(generateAtomic, !context._atomicBlockDescStack.empty()));
    }

    // Handle memory load/store within the atomic block
    if (generateAtomic)
    {
        HandleRmwMemory(context);
    }
}

// Reorder operations to fit into the 3 phases for atomic blocks that do a read-modify-write to memory
enum class SortAtomicOperationsPhase
{
    GenerateAddress,
    Load,
    Compute,
    Store,

    Count
};

static const size_t SortAtomicOperationsPhaseCount = static_cast<size_t>(SortAtomicOperationsPhase::Count);

std::array<OperationList, SortAtomicOperationsPhaseCount>
SortAtomicOperations(const OperationList& operations, const Location& location, const Program& program)
{
    std::array<OperationList, SortAtomicOperationsPhaseCount> result;

    // Find dependencies between operations
    OperationListDependencyGraph dependencyGraph = ComputeOperationListDependencyGraph(operations, program);

    // For each LoadMemory operation
    // Determine if the address computation can be moved into SortAtomicOperationsPhase::GenerateAddress
    // and the load moved to SortAtomicOperationsPhase::Load
    // This occurs when all operations involved in the address computation are locals
    // If this is not the case, then the LoadMemory is left in SortAtomicOperationsPhase::Compute, and scheduling will
    // fail if there not enough available stages in the schedule block

    // Determine which operations that could be moved to before the atomic block
    std::set<const Operation*> potentialHoistOperations;

    std::set<size_t> impureRegisters;

    for (const Operation& op : operations)
    {
        bool pureOperation = true;

        if (IsLiveOperation(op))
        {
            // Operations with side effects cannot be hoisted out of the atomic
            pureOperation = false;
        }

        std::vector<size_t> readSet;
        std::vector<size_t> writeSet;

        GetAccessedRegisters(op, readSet, writeSet);

        // all input registers must be pure local registers
        for (const size_t inputRegisterIndex : readSet)
        {
            if (impureRegisters.end() != impureRegisters.find(inputRegisterIndex))
            {
                pureOperation = false;
            }

            if (RegisterType::Local != program._registerTable[inputRegisterIndex]._type)
            {
                pureOperation = false;
            }
        }

        // All output registers must be locals
        for (const size_t outputRegisterIndex : writeSet)
        {
            if (RegisterType::Local != program._registerTable[outputRegisterIndex]._type)
            {
                pureOperation = false;
            }
        }

        if (pureOperation)
        {
            potentialHoistOperations.insert(&op);
        }
        else
        {
            // Mark all output registers as impure
            for (const size_t outputRegisterIndex : writeSet)
            {
                impureRegisters.insert(outputRegisterIndex);
            }
        }
    }

    // The set of operations that run in SortAtomicOperationsPhase::Load
    std::set<const Operation*> hoistedLoadOperations;

    // The set of operations that run in SortAtomicOperationsPhase::GenerateAddress
    std::set<const Operation*> generateAddressOperations;

    for (const Operation& op : operations)
    {
        if (Opcode::LoadMemory == op._opcode)
        {
            // First find direct predecessors of LoadMemory operations
            std::set<const Operation*> pendingOperations = dependencyGraph.backwardOperationDependencies[&op];

            bool canHoist = true;

            // Check to see if the address & predicates are local
            for (size_t i = 1; i < op._src.size(); i++)
            {
                if (SourceOperandType::Register == op._src[i].Type())
                {
                    const size_t regIndex = op._src[i].GetAccessedRegister()._registerIndex;

                    if (!IsLocalRegisterType(program._registerTable[regIndex]._type))
                    {
                        canHoist = false;
                        assert(op._flags._loadMemory._readLatency ==
                               1); // output register should have been disabled before this point
                    }
                }
            }

            // Move address computations to before the atomic block (if possible)
            // This is done even if the address or predicate are globals
            // (the other one could be a local)

            // Next, iteratively add predecessors of those operations
            while (!pendingOperations.empty())
            {
                std::set<const Operation*> newPendingOperations;

                for (const Operation* const pendingOp : pendingOperations)
                {
                    if (potentialHoistOperations.end() == potentialHoistOperations.find(pendingOp))
                    {
                        canHoist = false;
                    }
                    else
                    {
                        // Move this address computation op to before the atomic block
                        generateAddressOperations.insert(pendingOp);

                        // Check predecessors of this address computation op
                        const std::set<const Operation*>& predecessors =
                            dependencyGraph.backwardOperationDependencies[pendingOp];

                        for (const Operation* const predecessor : predecessors)
                        {
                            newPendingOperations.insert(predecessor);
                        }
                    }
                }

                pendingOperations.swap(newPendingOperations);
            }

            if (canHoist)
            {
                // All address computations were moved to SortAtomicOperationsPhase::GenerateAddress
                // Move the load to SortAtomicOperationsPhase::Load
                hoistedLoadOperations.insert(&op);
            }
        }
    }

    for (size_t phaseIndex = 0; phaseIndex < SortAtomicOperationsPhaseCount; phaseIndex++)
    {
        for (const Operation& op : operations)
        {
            bool operationMatchesPhase = false;

            if (generateAddressOperations.end() != generateAddressOperations.find(&op))
            {
                operationMatchesPhase = (phaseIndex == static_cast<size_t>(SortAtomicOperationsPhase::GenerateAddress));
            }
            else
            {
                switch (op._opcode)
                {
                // LoadMemory with latency 2 goes in phase 1
                // LoadMemory with latency 1 goes in phase 2
                case Opcode::LoadMemory:
                    if (op._flags._loadMemory._readLatency == 2)
                    {
                        if (hoistedLoadOperations.end() != hoistedLoadOperations.find(&op))
                        {
                            operationMatchesPhase =
                                (phaseIndex == static_cast<size_t>(SortAtomicOperationsPhase::Load));
                        }
                        else
                        {
                            // Address computation could not be hoisted
                            operationMatchesPhase =
                                (phaseIndex == static_cast<size_t>(SortAtomicOperationsPhase::Compute));
                        }
                    }
                    else
                    {
                        assert(op._flags._loadMemory._readLatency == 1);
                        assert(hoistedLoadOperations.end() == hoistedLoadOperations.find(&op));
                        operationMatchesPhase = (phaseIndex == static_cast<size_t>(SortAtomicOperationsPhase::Compute));
                    }
                    break;

                // StoreMemory goes in phase 3
                case Opcode::StoreMemory:
                    operationMatchesPhase = (phaseIndex == static_cast<size_t>(SortAtomicOperationsPhase::Store));
                    break;

                // All other operations go in phase 2
                default:
                    operationMatchesPhase = (phaseIndex == static_cast<size_t>(SortAtomicOperationsPhase::Compute));
                    break;
                }
            }

            // All operations that must preceed this operation must have already been scheduled
            if (operationMatchesPhase)
            {
                const std::set<const Operation*>& predecessors = dependencyGraph.backwardOperationDependencies[&op];
                if (!predecessors.empty())
                {
                    g_compiler->ErrorStream(location, CompileError::InvalidAtomic)
                        << "For atomic blocks that access memories, all memory indices must be computed before the "
                           "atomic block, and all memory writes must appear after memory reads";
                }

                // Add the operation to the current phase
                result[phaseIndex].push_back(op);

                // Record that successor operations have 1 fewer unmet dependency
                const std::set<const Operation*>& successors = dependencyGraph.forwardOperationDependencies[&op];

                for (const Operation* const successor : successors)
                {
                    dependencyGraph.backwardOperationDependencies[successor].erase(&op);
                }
            }
        }
    }

    return result;
}

void AtomicNode::HandleRmwMemory(IRContext& context) const
{
    OperationList& operationList = context._basicBlock->_operations;

    OperationList::iterator initialIt = context._beginAtomicIt;
    assert(initialIt->_opcode == Opcode::BeginAtomic);

    size_t atomicDepth = 0;
    bool unnestedMemoryAccess = false;
    std::string memName;
    for (OperationList::iterator it = initialIt; it != operationList.end(); it++)
    {
        const Operation& op = *it;

        if (op._opcode == Opcode::BeginAtomic)
        {
            atomicDepth++;
        }
        else if (op._opcode == Opcode::EndAtomic)
        {
            assert(atomicDepth > 0);
            atomicDepth--;
        }
        else if (op._opcode == Opcode::LoadMemory || op._opcode == Opcode::StoreMemory)
        {
            assert(atomicDepth > 0);
            if (atomicDepth == 1)
            {
                unnestedMemoryAccess = true;

                // Get memory name
                size_t regIndex;
                if (op._opcode == Opcode::LoadMemory)
                {
                    regIndex = op._src[0].GetAccessedRegister()._registerIndex;
                }
                else
                {
                    assert(op._opcode == Opcode::StoreMemory);
                    regIndex = op._dst[0].GetAccessedRegister()._registerIndex;
                }
                const RegisterDescription& memoryDesc = context._program->_registerTable[regIndex];
                assert(memoryDesc._type == RegisterType::Memory);
                memName = memoryDesc._name;
            }
        }
    }
    assert(atomicDepth == 0);

    if (initialIt->_flags._atomicBlockDesc._containsAtomic)
    {
        // Unnested memory access is not allowed when nested atomics exist
        if (unnestedMemoryAccess)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidAtomic)
                << "Memory loads and stores in non-leaf atomic block are not supported. Memory name: " << memName;
            throw std::runtime_error("Invalid atomic block");
        }

        // Only apply RMW memory rewriting to leaf atomic blocks
        return;
    }

    // search for memory load/store within the atomic block
    std::set<size_t> memoriesRead;
    std::set<size_t> memoriesWritten;

    const Operation& initialOp = *initialIt;
    assert(Opcode::BeginAtomic == initialOp._opcode);

    assert(!operationList.empty());
    const Operation& lastOp = operationList.back();
    assert(Opcode::EndAtomic == lastOp._opcode);

    for (auto it = initialIt; it != operationList.end(); ++it)
    {
        Operation& op = *it;

        if (op._opcode == Opcode::LoadMemory)
        {
            assert(2 == op._flags._loadMemory._readLatency);

            const size_t regIndex = op._src[0].GetAccessedRegister()._registerIndex;
            const RegisterDescription& memoryDesc = context._program->_registerTable[regIndex];
            assert(memoryDesc._type == RegisterType::Memory);

            // If the address or predicate are global, then set the read latency to 1
            // Because there is no other feasible way to implement reading
            // from a memory in an atomic block with the address coming from a global
            for (size_t i = 1; i < op._src.size(); i++)
            {
                if (SourceOperandType::Register == op._src[i].Type())
                {
                    const size_t srcRegIndex = op._src[i].GetAccessedRegister()._registerIndex;
                    const RegisterDescription& srcRegDesc = context._program->_registerTable[srcRegIndex];

                    if (RegisterType::Global == srcRegDesc._type)
                    {
                        op._flags._loadMemory._readLatency = 1;
                    }
                    else
                    {
                        assert(RegisterType::Local == srcRegDesc._type);
                    }
                }
            }

            memoriesRead.insert(regIndex);
        }
        else if (op._opcode == Opcode::StoreMemory)
        {
            const size_t regIndex = op._dst[0].GetAccessedRegister()._registerIndex;

            const RegisterDescription& memoryDesc = context._program->_registerTable[regIndex];
            assert(memoryDesc._type == RegisterType::Memory);

            memoriesWritten.insert(regIndex);
        }
    }

    // Determine which memories are both read and written - these will need bypass
    const std::set<size_t> rmwMemories = Intersection(memoriesRead, memoriesWritten);

    // Allocate unique bypass group indices for each memory which is both read and written
    std::map<size_t, size_t> memoryToBypassGroup;

    for (const size_t memoryIndex : rmwMemories)
    {
        SafeInsert(memoryToBypassGroup, memoryIndex, context._basicBlock->_bypassGroupCount++);
    }

    if (!memoriesRead.empty() || !memoriesWritten.empty())
    {
        const UpdateRateModifierNode::ModifiedUpdateRate modifiedUpdateRate =
            _updateRate.GetModifiedUpdateRate(!rmwMemories.empty());

        // The atomic block must be split into 3
        // The first one performs all memory loads (for memories with output register enabled)
        // The second does all general operations (and is the only one that can access global variables).
        //   the second block also does memory loads for memories that have output registered disabled
        // The last one performs all memory stores

        // Move the body of the atomic block into a new list
        OperationList bodyOperations;

        for (auto it = initialIt; it != operationList.end(); ++it)
        {
            const Operation& op = *it;

            bodyOperations.push_back(op);
        }

        // Remove the outermost Begin/EndAtomic, they will be re-added later
        assert(bodyOperations.front()._opcode == Opcode::BeginAtomic);
        bodyOperations.pop_front();

        assert(bodyOperations.back()._opcode == Opcode::EndAtomic);
        bodyOperations.pop_back();

        // Validate nesting is not allowed (this is also validated later, but will trip up the assertion about
        // op._flags._loadMemory._bypass)
        for (const Operation& op : bodyOperations)
        {
            if ((Opcode::BeginAtomic == op._opcode) || (Opcode::EndAtomic == op._opcode))
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidAtomic) << "Nested atomics are not supported";
                throw std::runtime_error("invalid atomic");
            }
        }

        // Set bypass flags and bypass group index values
        if (modifiedUpdateRate._needsBypass)
        {
            size_t loadMemoryKey = 0;

            for (Operation& op : bodyOperations)
            {
                if (Opcode::LoadMemory == op._opcode)
                {
                    assert(!op._flags._loadMemory._bypass);

                    const size_t regIndex = op._src[0].GetAccessedRegister()._registerIndex;

                    const RegisterDescription& memoryDesc = context._program->_registerTable[regIndex];
                    assert(memoryDesc._type == RegisterType::Memory);

                    // Save bypass group index in the operation
                    const auto it = memoryToBypassGroup.find(regIndex);
                    if (it != memoryToBypassGroup.end())
                    {
                        op._flags._loadMemory._bypass = true;
                        op._flags._loadMemory._bypassGroupIndex = it->second;
                        op._flags._loadMemory._loadMemoryKey = loadMemoryKey++;
                    }
                }
                else if (Opcode::StoreMemory == op._opcode)
                {
                    assert(!op._flags._storeMemory._bypass);

                    const size_t regIndex = op._dst[0].GetAccessedRegister()._registerIndex;

                    const RegisterDescription& memoryDesc = context._program->_registerTable[regIndex];
                    assert(memoryDesc._type == RegisterType::Memory);

                    // Save bypass group index in the operation
                    const auto it = memoryToBypassGroup.find(regIndex);
                    if (it != memoryToBypassGroup.end())
                    {
                        op._flags._storeMemory._bypass = true;
                        op._flags._storeMemory._bypassGroupIndex = it->second;
                    }
                }
            }
        }

        // Add memory bypass operations
        for (OperationList::iterator it = bodyOperations.begin(); it != bodyOperations.end(); ++it)
        {
            const Operation& op = *it;

            if ((Opcode::LoadMemory == op._opcode) && op._flags._loadMemory._bypass)
            {
                Operation bypassOp = {};
                bypassOp._opcode = Opcode::BypassMemory;
                bypassOp._flags._bypassMemory._bypassGroupIndex = op._flags._loadMemory._bypassGroupIndex;
                bypassOp._flags._bypassMemory._loadMemoryKey = op._flags._loadMemory._loadMemoryKey;

                // Add the address
                bypassOp.PushOperand(op, 1);

                for (size_t i = 0; i < op._dst.size(); i++)
                {
                    const AccessedRegister reg = op._dst[i].GetAccessedRegister();

                    bypassOp._src.push_back(reg);
                    bypassOp._dst.push_back(reg);
                }

                ++it;
                bodyOperations.insert(it, bypassOp);
            }
        }

        operationList.erase(initialIt, operationList.end());

        // Sort the operations into multiple phases
        std::array<OperationList, SortAtomicOperationsPhaseCount> phases =
            SortAtomicOperations(bodyOperations, _location, *context._program);

        // Address computation (before any atomic block)
        if (!phases[static_cast<size_t>(SortAtomicOperationsPhase::GenerateAddress)].empty())
        {
            operationList.splice(operationList.end(),
                                 phases[static_cast<size_t>(SortAtomicOperationsPhase::GenerateAddress)]);
        }

        // allocate a unique id that identifies this read/modify/write sequence
        const size_t atomicChainIndex = context.AllocateAtomicChainIndex();

        // Loads from memory
        if (!phases[static_cast<size_t>(SortAtomicOperationsPhase::Load)].empty())
        {
            const size_t additionalReadLatency = GetCodeGenConfig()._rmwMemoryReadDelay;

            {
                AtomicBlock atomicBlock(context, AtomicBlockType::MemoryLoad, c_defaultAtomicBlockUpdateRate,
                                        atomicChainIndex, GetLocation());

                operationList.splice(operationList.end(), phases[static_cast<size_t>(SortAtomicOperationsPhase::Load)]);
            }

            // Add additional pipeline stages to route memory load data to the compute stage
            for (size_t i = 0; i < additionalReadLatency; i++)
            {
                AtomicBlock atomicBlock(context, AtomicBlockType::MemoryLoadLatency, c_defaultAtomicBlockUpdateRate,
                                        atomicChainIndex, GetLocation());
            }
        }

        // General computation
        {
            // AtomicMemCompute is used to ensure the computation stage is not optimized away (even if it contains no
            // operations) modifiedUpdateRate._updateRate is passed to allow multiple pipeline stages, if the source
            // code indicates that this is safe
            AtomicBlock atomicBlock(context, AtomicBlockType::AtomicMemCompute, modifiedUpdateRate._updateRate,
                                    atomicChainIndex, GetLocation());

            // Remove the last line number from the body operations
            // This belongs to the storeMemory phase below
            OperationList computePhaseOps = phases[static_cast<size_t>(SortAtomicOperationsPhase::Compute)];

            // Atomic mem blocks become 3 pipeline stages
            // 1. Compute address
            // 2. Load from memory, compute on value
            // 3. Store to memory
            // The sequence of operations from the above looks something like:
            // linenumber, beginatomic, loadmemory, endatomic
            // beginatomic, linenumber, otherops, linenumber, otherops, endatomic
            // beginatomic, storememory, endatomic
            // This remove_if removes the last emitted line number from the body operations as
            // it belongs to the store phase
            // This improves visualization of how the stage breakdown occurs in the debugger.
            const auto removeMemoryStoreLineFromBody = [context](const Operation& op)
            {
                bool result = false;

                if (Opcode::LineNumber == op._opcode)
                {
                    assert(1 == op._locations.size());
                    const FileAndLineNumber& fnum = *(op._locations.begin());

                    result = context._basicBlock->_lastEmittedLineNumber == fnum;
                }

                return result;
            };

            computePhaseOps.remove_if(removeMemoryStoreLineFromBody);

            operationList.splice(operationList.end(), computePhaseOps);
        }

        // Stores to memory
        if (!phases[static_cast<size_t>(SortAtomicOperationsPhase::Store)].empty())
        {
            AtomicBlock atomicBlock(context, AtomicBlockType::MemoryStore, c_defaultAtomicBlockUpdateRate,
                                    atomicChainIndex, GetLocation());

            // Emit line number for the StoreMemory phase
            OperationList storeMemoryPhaseOps = phases[static_cast<size_t>(SortAtomicOperationsPhase::Store)];

            Operation op = {};
            op._opcode = Opcode::LineNumber;
            op._locations.insert(context._basicBlock->_lastEmittedLineNumber);

            storeMemoryPhaseOps.insert(storeMemoryPhaseOps.begin(), op);

            operationList.splice(operationList.end(), storeMemoryPhaseOps);
        }
    }
}

bool ArrayAccessNode::IsIndexKnown(KnownValueContext& context) const
{
    const KnownValue indexValue = _indexNode->TryGetKnownValue(context, _indexNode->GetType());

    return indexValue._type == KnownValueType::Int;
}

size_t ArrayAccessNode::GetKnownIndex(KnownValueContext& context) const
{
    const KnownValue indexValue = _indexNode->TryGetKnownValue(context, _indexNode->GetType());
    assert(indexValue._type == KnownValueType::Int);

    // Check for a negative index
    if (IsNegative(indexValue._intVal, _indexNode->GetType()))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidArrayIndexing) << "Negative array index";
        throw std::runtime_error("Negative array index");
    }

    const size_t index = MpToSizeT(indexValue._intVal);

    const ArrayTypeBase* const arrayType = dynamic_cast<const ArrayTypeBase*>(_array->GetType());

    if (index >= arrayType->_arraySize)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidArrayIndexing) << "Invalid array index";
        throw std::runtime_error("Invalid array index");
    }

    return index;
}

// Generate IR for a call to an external fixed-latency function
// This simply emits an operation that represents an invocation of the external module
/* static */ void CallNode::GenerateExternalFixedLatencyCall(
    IRContext& context, const std::vector<const ParseTreeNode*>& args, const FunctionDesc& functionDesc,
    const std::string& functionName, const ExternalModuleCallType callType,
    const boost::optional<ExternalClassInstanceName>& externModuleInstanceName, const Location& location)
{
    assert(args.size() == functionDesc._parameterTypes.size());
    assert(args.size() == functionDesc._parameterNames.size());

    // Evalate all arguments into registers
    std::vector<const AllocatedRegister*> argRegisters;

    for (size_t i = 0; i < args.size(); i++)
    {
        // Allocate a register for the parameter
        const AllocatedRegister* const allocatedRegister = functionDesc._parameterTypes[i]->AllocateRegisters(
            *context._program, RegisterType::Local, "ExternalModuleArg");

        // Evaluate the argument expression into the register
        EvaluateExpressionIntoRegister(context, allocatedRegister, args[i]);

        argRegisters.push_back(allocatedRegister);
    }

    GenerateExternalFixedLatencyCallRegisterArgs(context, argRegisters, functionDesc, functionName, callType,
                                                 externModuleInstanceName, location);
}

/* static */ void CallNode::GenerateExternalFixedLatencyCallRegisterArgs(
    IRContext& context, const std::vector<const AllocatedRegister*>& argRegisters, const FunctionDesc& functionDesc,
    const std::string& functionName, const ExternalModuleCallType callType,
    const boost::optional<ExternalClassInstanceName>& externModuleInstanceName, const Location& location)
{
    assert(argRegisters.size() == functionDesc._parameterTypes.size());
    assert(argRegisters.size() == functionDesc._parameterNames.size());

    SetOperationLocation sol(context, location);

    // Add information about the call to the program
    ExternalModuleCall externalModuleCall = {};

    externalModuleCall._name = functionName;
    externalModuleCall._type = callType;
    externalModuleCall._functionDesc = functionDesc;
    externalModuleCall._externClassInstanceName = externModuleInstanceName;

    // Allocate a register to hold the result
    const AllocatedRegister* const outputReg =
        functionDesc._returnType->AllocateRegisters(*context._program, RegisterType::Local, "ExternalModuleResult");

    // Emit an operation that invokes the external module
    Operation op = {};

    op._opcode = Opcode::InlineExternalModule;

    // Add the predicate
    if (context.IsPredicated())
    {
        const AccessedRegister predicateReg = {context.GetPredicate()};

        op._src.push_back(SourceOperand(predicateReg));
    }
    else
    {
        op._src.push_back(SourceOperand(1, 1));
    }

    externalModuleCall._srcOperandNames.push_back("InternalPredicateValue");
    externalModuleCall._srcOperandWidths.push_back(1);

    for (size_t i = 0; i < argRegisters.size(); i++)
    {
        const AllocatedRegister* const argReg = argRegisters[i];

        // Used to composite parameters (like structs) to differentiate the element names
        size_t subParamIndex = 0;

        const auto visitFunction = [&](const AccessedRegister reg, const Type* const type)
        {
            // determine a name for the indiviual parameter - which will be used as the parameter name in the
            // caller-defined implementation
            std::ostringstream completeName;
            completeName << functionDesc._parameterNames[i];

            // Only output subParamIndex for > 0, so that the non-struct case is not ugly
            if (subParamIndex > 0)
            {
                completeName << subParamIndex;
            }

            subParamIndex++;

            externalModuleCall._srcOperandNames.push_back(completeName.str());

            externalModuleCall._srcOperandWidths.push_back(type->GetBitWidth());

            op._src.push_back(SourceOperand(reg));
        };

        argReg->VisitRegisters(visitFunction);
    }

    assert(externalModuleCall._srcOperandNames.size() == externalModuleCall._srcOperandWidths.size());

    {
        const auto visitFunction = [&](const AccessedRegister reg, const Type* const type)
        {
            externalModuleCall._destinationOperandWidths.push_back(type->GetBitWidth());

            op._dst.push_back(DestinationOperand(reg));
        };

        outputReg->VisitRegisters(visitFunction);
    }

    CallInlineExternalModule callInlineExternalModule = {};

    // It is important that this comes after child expressions are evaluated
    // Because those evaluations could push something on to _externalModuleCalls
    callInlineExternalModule._externalModuleIndex = context._program->_externalModuleCalls.size();

    assert(functionDesc._fixedLatency);
    callInlineExternalModule._latency = *functionDesc._fixedLatency;

    // Assume that externally-instantiated fixed-latency functions can have side effects
    // Never optimize away calls to these functions
    callInlineExternalModule._isLive = callType != ExternalModuleCallType::InstantiateInBasicBlock;

    // Record if the called function is side-effect-free
    callInlineExternalModule._isPure = functionDesc._modifiers & ParseTreeFunctionModifierPure;

    op._flags._callInlineExternalModule = callInlineExternalModule;

    context._basicBlock->_operations.push_back(op);

    // For the purposes of debugging, mark if this basic block has atomic ops inside.
    // That can lead to multiple line numbers for the same stage.
    context._basicBlock->_hasInlinedAtomicFunctions |= context.IsInAtomicBlock();

    context._program->_externalModuleCalls.push_back(externalModuleCall);

    // Insert extra operations to get the correct number of cycles of latency
    // Latency >= 2: Operation is considered to have registered input and output
    // Latency == 1: Operation is considered to have registered input
    // Latency == 0: Operation is considered to be combinatorial
    const size_t extraStages = *functionDesc._fixedLatency >= 2 ? *functionDesc._fixedLatency - 2 : 0;

    const AllocatedRegister* finalOutputReg = AddDelayStages(context, outputReg, extraStages);

    // Move from the right-sized output to context._writtenRegisters
    if (context._writtenRegisters)
    {
        Move(context, context._writtenRegisters, finalOutputReg, Opcode::Mov, location);
    }
}

static void GenerateAsyncCall(IRContext& context, const FunctionNode::Instance& functionInstance,
                              const GetSuccessorBlock& getSuccessorBlock, const std::vector<const ParseTreeNode*>& args,
                              const bool isPipelined, const CallModifiers& callParams, const Location& callSiteLocation)
{
    // Async functions must be void
    assert(!context._writtenRegisters);

    // Add an operation which will enqueue a new thread into the destination function
    Operation op = {};

    op._opcode = Opcode::Enqueue;

    op.InsertLocation(callSiteLocation);

    op._flags._enqueue._type = EnqueueType::FunctionCall;

    op._flags._enqueue._modifiers = callParams;

    op._getSuccessorBlock = getSuccessorBlock;

    if (context.IsPredicated())
    {
        DebuggerHintPushPredicate(context._basicBlock->_operations);

        // Make the Enqueue operation predicated
        op._flags._enqueue._isPredicated = true;

        op._flags._enqueue._predicateExecutionValue = true;

        const AccessedRegister predicateReg = {context.GetPredicate()};

        op._src.push_back(predicateReg);
    }

    // Add all parameters as sources of the jump, to ensure that the jump is scheduled after the arguments are evaluated
    for (size_t i = 0; i < args.size(); ++i)
    {
        const AllocatedRegister* const paramRegister = functionInstance.GetParameterRegisters(i);

        const auto callback = [&](const AccessedRegister reg, const Type* const type) { op._src.push_back(reg); };

        paramRegister->VisitRegisters(callback);
    }

    // Also, add the thread count == 1 register, to ensure the jump is scheduled after it is evaluated
    if (isPipelined)
    {
        op._src.push_back(functionInstance.GetThreadCountOneRegister()->GetAccessedRegister());
    }

    op._flags._enqueue._isPipelinedCall = isPipelined;

    // Async calls are allowed within atomic blocks
    op._flags._enqueue._allowedInAtomic = true;

    op._flags._enqueue._isAsyncCall = true;

    context._basicBlock->_operations.push_back(op);

    if (context.IsPredicated())
    {
        DebuggerHintPopPredicate(context._basicBlock->_operations);
    }
}

static void GenerateSyncCall(IRContext& context, const FunctionDesc& functionDesc,
                             const FunctionNode* const functionNode, const std::string& calledObjectName,
                             CallNode::Instance& callSiteInstance, const FunctionNode::Instance& functionInstance,
                             const GetSuccessorBlock& getSuccessorBlock, const bool isPipelined, const bool isUnordered,
                             const CallModifiers& callParams, const Type* const callerReturnType,
                             const AllocatedLeafRegister* const loopCountRegisterForContextSaver,
                             const Location& afterCallLocation)
{
    // Add the hidden call index argument
    // This is not needed for async functions
    // because they do not return
    if (functionInstance.HasCallSiteIndex())
    {
        Operation op = {};

        // MovCrossFunction is used to ensure that the call index is not optimized away (the optimizer runs
        // per-function)
        op._opcode = Opcode::MovCrossFunction;

        op._src.push_back(SourceOperand(callSiteInstance.GetCallSiteIndex()));
        op._dst.push_back(functionInstance.GetCallSiteIndexRegister()->GetAccessedRegister());
        op.InsertLocation(afterCallLocation);

        context._basicBlock->_operations.push_back(op);
    }

    // End the current basic block
    BasicBlock* const prevBasicBlock = FinalizeBasicBlock(context);

    // Create a new basic block for the code after the call
    BasicBlock* const nextBlock = context.CreateBasicBlock(afterCallLocation);
    context._basicBlock = nextBlock;

    // The number of bits to represent the loop count inside the context saver
    //
    // If the function is pipelined, add 1 because the loop count
    // can represent 2^N (thread IDs are in [0, 2^N-1])
    //
    // If the function is not pipelined, then 1 bit is needed
    // to handle predication
    const size_t loopCountWidth = isPipelined ? functionDesc._parameterTypes[0]->GetBitWidth() + 1 : 1;

    ContextSaver* contextSaver = nullptr;
    AppendCall(context, prevBasicBlock, getSuccessorBlock, functionNode, nextBlock, isPipelined, calledObjectName,
               callParams, callerReturnType, callSiteInstance.GetCallSiteIndex(), loopCountRegisterForContextSaver,
               loopCountWidth, &contextSaver, afterCallLocation);

    // The context saver object needs accessed to the invocation instance
    // and the loop count (pipelined only)
    // Add these registers to the live-in set of the return site
    // to ensure that these values will be passed to the context saver input
    // This is not necessary for ordered functions, because threads return in order
    if (isUnordered)
    {
        const size_t invocationInstanceRegisterIndex =
            functionInstance.GetInvocationInstanceRegister()->GetAccessedRegister()._registerIndex;

        nextBlock->_liveInReg.push_back(invocationInstanceRegisterIndex);

        assert(!nextBlock->_invocationInstanceRegisterIndex);
        nextBlock->_invocationInstanceRegisterIndex = invocationInstanceRegisterIndex;
    }

    if (isPipelined)
    {
        // The context saver also needs access to the loop count
        const size_t loopCounterRegisterIndex =
            functionInstance.GetLoopCounterRegister()->GetAccessedRegister()._registerIndex;

        nextBlock->_liveInReg.push_back(loopCounterRegisterIndex);

        // Improve RTL coverage by noting that the loop count
        // register is not needed at the return site
        nextBlock->_externalOnlyLiveInReg.push_back(loopCounterRegisterIndex);

        // For pipelined calls that return an array, the array should not be live-in at the return site (otherwise it
        // will be live-in at the callee)
        if (contextSaver->_callerReturnValueRegisters)
        {
            const auto callback = [&](const AccessedRegister reg, const Type* const type)
            { nextBlock->_forcedDeadInReg.push_back(reg._registerIndex); };

            contextSaver->_callerReturnValueRegisters->VisitRegisters(callback);
        }
    }

    // for pipelined calls that return an array, this points to the array, not the scalar values returned by the
    // function
    const AllocatedRegister* returnValueRegister = contextSaver->_callerReturnValueRegisters;

    if (functionDesc._returnType->GetBitWidth() > 0)
    {
        if (context._writtenRegisters)
        {
            // Move the return value into the destination register
            const Type* const returnType = functionDesc._returnType;

            assert(returnType->GetBitWidth() > 0);

            // Add operations to the basic block which move the value
            // MovCrossFunction is used to ensure that the call index is not optimized away (the optimizer runs
            // per-function)
            Move(context, context._writtenRegisters, returnValueRegister, Opcode::MovCrossFunction, afterCallLocation);

            if (isPipelined)
            {
                // Set return strings handles to 0 for
                // any thread beyond the thread count
                // because the values of the handles are undefind
                // coming out of the context saver
                const AllocatedArrayRegister* const arrayReg =
                    safe_cast<const AllocatedArrayRegister*>(context._writtenRegisters);

                for (size_t i = 0; i < arrayReg->_elements.size(); i++)
                {
                    const AccessedRegister conditionReg = {
                        AllocateRegister(*context._program, 1, RegisterType::Local, "ClearStringHandle")};

                    {
                        Operation op = {};

                        op.InsertLocation(afterCallLocation);

                        op._opcode = Opcode::BinaryOp;
                        op._flags._binaryOpType = ParseTreeBinaryOpTypeGE;

                        op.PushOperand(SourceOperand(i), false);
                        op.PushOperand(loopCountRegisterForContextSaver->GetAccessedRegister(),
                                       IsSignedLeafType(loopCountRegisterForContextSaver->GetType()));
                        op._dst.push_back(conditionReg);

                        context._basicBlock->_operations.push_back(op);
                    }

                    const AllocatedRegister* const elementReg = arrayReg->_elements[i];

                    const auto callback = [&](const AccessedRegister reg, const Type* const type)
                    {
                        if (g_compiler->GetStringType() == type)
                        {
                            const Literal zeroHandle = {0, c_stringHandleWidth};

                            Operation op = {};

                            op.InsertLocation(afterCallLocation);

                            op._opcode = Opcode::Select;

                            op.PushOperand(conditionReg, false);
                            op.PushOperand(reg, IsSignedLeafType(type));
                            op.PushOperand(zeroHandle, false);

                            op._dst.push_back(reg);

                            context._basicBlock->_operations.push_back(op);
                        }
                    };

                    arrayReg->_elements[i]->VisitRegisters(callback);
                }
            }
        }

        // Add the return register to the live-in set of the return site
        // This is used during liveness analysis to recognize
        // that this variable is not live-out at the call site
        // Note that this happens even if the return value is not used
        // returnValueRegister can be null for pipelined functions that return a value that is ignored
        if (returnValueRegister != nullptr)
        {
            const auto callback = [&](const AccessedRegister reg, const Type* const type)
            {
                // Add the return register to the live-in set of the return site
                // This is used during liveness analysis to recognize
                // that this variable is not live-out at the call site
                context._basicBlock->_liveInReg.push_back(reg._registerIndex);

                context._basicBlock->_returnValueReg.push_back(reg._registerIndex);
            };

            returnValueRegister->VisitRegisters(callback);
        }
    }

    callSiteInstance.SetReturnBlock(nextBlock);
}

// For pipelined calls, predicates off execution if the loop count is zero
// So that loop generators can assume the loop count is not zero
class PushPopLoopCountZeroPredicate
{
  public:
    PushPopLoopCountZeroPredicate(IRContext& context, const AllocatedLeafRegister* const loopCountRegister,
                                  const Location& location)
        : _context(context)
    {
        const AccessedRegister& loopCountReg = loopCountRegister->GetAccessedRegister();

        const AccessedRegister loopCountNotZero = {
            AllocateRegister(*context._program, 1, RegisterType::Local, "LoopCountNotZero")};

        {
            Operation op = {};

            op._opcode = Opcode::BinaryOp;
            op._flags._binaryOpType = ParseTreeBinaryOpTypeNE;

            op._src.push_back(loopCountReg);
            op._src.push_back(0);
            op._dst.push_back(loopCountNotZero);

            context._basicBlock->_operations.push_back(op);
        }

        _context.PushPredicate(loopCountNotZero._registerIndex, location);
    }

    ~PushPopLoopCountZeroPredicate() { _context.PopPredicate(); }

  private:
    IRContext& _context;
};

void CallNode::Reset() { _instances.clear(); }

void CallNode::GenerateIR(IRContext& context) const
{
    // Get the name of the object that contains the call site
    assert(!context._objectNameStack.empty());

    // Disable condition coverage tracking at a call node
    DisableConditionCoverage disableConditionCoverage(context, true);

    // Get the instance of the caller function
    assert(!context._functionInstanceStack.empty());
    const FunctionInstance callerInstance = context._functionInstanceStack.back();

    const std::string callSiteObjectName = context.GetObjectName();

    const ResolveReferenceFunction resolveReferences = [&](const std::string& src)
    { return g_compiler->GetFunctionInstanceEnumerator().DereferenceParameter(callerInstance, this, src); };

    // Determine information about the called function
    const ResolvedCall resolvedCall =
        ResolveFunctionCallPostTypeChecking(this, context._enclosingClassType, context._typeContext.GetNamespaceScope(),
                                            callSiteObjectName, resolveReferences);

    const std::string& functionName = GetName();

    const FunctionDesc& functionDesc = resolvedCall._functionDesc;

    if (functionDesc._modifiers & ParseTreeFunctionModifierExport)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidCall) << "Exported functions cannot be called";
    }

    const ParseTreeFunctionModifier inlineIntrinsicMask =
        static_cast<ParseTreeFunctionModifier>(ParseTreeFunctionModifierInline | ParseTreeFunctionModifierIntrinsic);

    if (inlineIntrinsicMask == (functionDesc._modifiers & inlineIntrinsicMask))
    {
        // Intrinsic function that generates inline operations
        if ("assert" == functionName)
        {
            GenerateAssert(context);
        }
        else if ("__print" == functionName)
        {
            GeneratePrint(context);
        }
        else if ("__cycles" == functionName)
        {
            GenerateCycleCounter(context);
        }
        else if ("__debug_view" == functionName)
        {
            GenerateDebugView(context);
        }
        else if ("__assert_str_eq" == functionName)
        {
            GenerateAssertStringEqual(context);
        }
        else if ("__str_cnt" == functionName)
        {
            GenerateStringCount(context);
        }
        else
        {
            throw std::runtime_error("Unknown intrinsic name");
        }
    }
    else
    {
        const bool isAsync = functionDesc._modifiers & ParseTreeFunctionModifierAsync;
        const bool isPipelined = functionDesc._modifiers & ParseTreeFunctionModifierPipelined;
        const bool isInline = functionDesc._modifiers & ParseTreeFunctionModifierInline;
        const bool isUnordered = functionDesc._modifiers & ParseTreeFunctionModifierUnordered;
        const bool isExternalFixedLatency = functionDesc._modifiers & ParseTreeFunctionModifierExternalFixedLatency;
        const bool isExternallyInstantiated =
            functionDesc._modifiers & ParseTreeFunctionModifierExternalExternallyInstantiated;

        if (context._isInAtomicDo && !isInline)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidLoop)
                << "Calls to non-inline functions are not allowed inside of atomic do-while loop";
        }

        if (functionDesc._fixedLatency &&
            (0 == (functionDesc._modifiers &
                   (ParseTreeFunctionModifierExternal | ParseTreeFunctionModifierExternalFixedLatency |
                    ParseTreeFunctionModifierExternalExternallyInstantiated))))
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                << "Non-external functions with the [[latency()]] attribute are not callable from Kanagawa code";
        }

        // Don't allow calls to inline pipelined functions
        // In order to avoid FIFOs, the implementation would require that
        // calls that are predicated off still take N cycles
        // Which would give unexpected throughput.
        if (isInline && isPipelined && context.IsPredicated())
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidExternFunction)
                << "Calls to inline pipelined functions cannot be made within flat control flow (if/else/switch).";
            throw std::runtime_error("Invalid call");
        }

        // Lookup information about the called function instance
        const std::vector<const ParseTreeNode*>& args = dynamic_cast<const NodeList*>(_args)->Children();

        if (isExternalFixedLatency)
        {
            if (resolvedCall._explicitObjectNode)
            {
                if (!resolvedCall._calledClassType->GetClassNode()->IsExternal())
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                        << "[[latency]] can only be applied to methods of external or exported classes";
                    throw std::runtime_error("Invalid call");
                }

                // fixed latency method of an extern class
                const std::string calledObjectName =
                    g_compiler->GetFunctionInstanceEnumerator().GetCalledObjectName(this, callerInstance);

                ExternalClassInstanceName externModuleInstanceName = {};

                externModuleInstanceName._className =
                    resolvedCall._calledClassType->GetClassNode()->GetExternalClassName();
                externModuleInstanceName._objectName = calledObjectName;

                boost::optional<ExternalClassInstanceName> externModuleInstanceNameOpt = externModuleInstanceName;

                GenerateExternalFixedLatencyCall(context, args, functionDesc, resolvedCall._functionName,
                                                 ExternalModuleCallType::ExternClassMethod, externModuleInstanceNameOpt,
                                                 _location);
            }
            else
            {
                const std::string prefix = FlattenScope(resolvedCall._flatFunctionScope);

                const auto name =
                    prefix.empty()
                        ? resolvedCall._functionName
                        : FixupString(GetCombinedExternalClassInstanceFunctionName(prefix, resolvedCall._functionName));

                const ExternalModuleCallType callType = isExternallyInstantiated
                                                            ? ExternalModuleCallType::ExternallyInstantiated
                                                            : ExternalModuleCallType::InstantiateInBasicBlock;

                GenerateExternalFixedLatencyCall(context, args, functionDesc, name, callType, {}, _location);
            }
        }
        else
        {
            if (context.FunctionHasOrderingRestrictions() && isUnordered)
            {
                g_compiler->ErrorStream(_location, CompileError::UnorderedOperation)
                    << "Ordered functions cannot call unordered functions";
            }

            const FunctionInstanceEnumerator::CalledInstanceDesc callDesc =
                g_compiler->GetFunctionInstanceEnumerator().LookupCall(this, callerInstance);

            // Create a new instance object
            std::list<Instance>& instanceList = _instances[callSiteObjectName];
            instanceList.emplace_back();
            Instance& callSiteInstance = instanceList.back();

            // Find the function node
            const FunctionNode* const functionNode = resolvedCall._functionNode;

            // ResolveFunctionCall can return a null function node for the cases handled above
            // like inline intrinics.  Those cases should not apply in this branch.
            assert(functionNode);

            // Determine the name of the called object
            const std::string calledObjectName = callDesc._calleeInstance._objectName;

            callSiteInstance.SetCalledObjectName(calledObjectName);

            callSiteInstance.SetCallSiteFunctionInstance(callerInstance);

            // 0 is passed for minimumCallCount to enable an unreachable function
            // to call a non-inline function in an atomic block
            const bool isInlineable =
                g_compiler->GetFunctionInstanceEnumerator().IsInlineable(callDesc._calleeInstance, 0);

            // Calls to export class members should not be auto-inlined
            assert(!isInlineable || !resolvedCall._callToExportClass);

            // Determine the name of the called object
            FunctionNode::Instance& functionInstance =
                const_cast<FunctionNode*>(functionNode)->GetInstance(calledObjectName);

            // Increment the call-site count
            const size_t callSiteIndex = functionInstance.AddCaller(context._function->_maxThreadCountInsideFunction);

            // Remember the call site index
            callSiteInstance.SetCallSiteIndex(callSiteIndex);

            std::unique_ptr<PushPopLoopCountZeroPredicate> pushPopLoopCountZeroPredicate;

            // Evaluate arguments - writing results into function parameter registers
            // For inline functions, determine if any arguments are known at compile-time
            std::vector<KnownValue> parameterKnownValues(args.size());

            for (KnownValue& kv : parameterKnownValues)
            {
                kv._type = KnownValueType::None;
            }

            // Evaluate arguments into new registers in one loop
            // Do not write into functionInstance.GetParameterRegisters until
            // all argument expressions have been evaluated
            // This is important for the case where an argument expression
            // is itself a call to the same function being called here

            std::vector<const AllocatedRegister*> argumentRegisters(args.size(), nullptr);

            for (size_t i = 0; i < args.size(); ++i)
            {
                assert(argumentRegisters[i] == nullptr);

                const ParseTreeNode* arg = args[i];

                argumentRegisters[i] = EvaluateExpression(context, args[i]);
            }

            // 1 will be subtracted from this register
            const AllocatedLeafRegister* loopCountRegister = nullptr;

            // This is a copy of the original loop count
            const AllocatedLeafRegister* loopCountForContextSaver = nullptr;

            if (isPipelined)
            {
                // Save a copy of the loop count, before subtracting one
                assert(args.size() > 0);

                loopCountRegister = dynamic_cast<const AllocatedLeafRegister*>(argumentRegisters[0]);

                loopCountForContextSaver =
                    dynamic_cast<const AllocatedLeafRegister*>(loopCountRegister->GetType()->AllocateRegisters(
                        *context._program, RegisterType::Local, "LoopCount"));

                Move(context, loopCountForContextSaver, loopCountRegister, Opcode::Mov, _location);
            }

            for (size_t i = 0; i < args.size(); ++i)
            {
                const ParseTreeNode* arg = args[i];

                const AllocatedRegister* const paramRegister = functionInstance.GetParameterRegisters(i);

                // Increment reference counts of all string parameters
                // Callee will decrement the reference count
                // For pipelind calls, the increment by the loop count
                // because each callee thread will decrement
                const auto callback = [&](const AccessedRegister reg, const Type* const type)
                {
                    if (g_compiler->GetStringType() == type)
                    {
                        Operation op = {};

                        op.InsertLocation(_location);

                        op._opcode = Opcode::ReferenceString;

                        PushPredicateOrLiteralOne(context, op);

                        op._src.push_back(reg);

                        if (isPipelined)
                        {
                            op.PushOperand(loopCountForContextSaver->GetAccessedRegister(),
                                           IsSignedLeafType(loopCountForContextSaver->GetType()));
                        }
                        else
                        {
                            op.PushOperand(1, false);
                        }

                        context._basicBlock->_operations.push_back(op);
                    }
                };

                argumentRegisters[i]->VisitRegisters(callback);

                if (isInline || isInlineable)
                {
                    // Move the data into the argument parameter register
                    Move(context, paramRegister, argumentRegisters[i], Opcode::Mov, _location);

                    // Also - check to see if the value is known

                    // For inline pipelined calls, store the thread count in the
                    // argument type, because further arithmetic (thread count - 1) must occur
                    // before conversion to parametr type.
                    // For all others, convert to the parameter type.
                    const Type* const knownValueType =
                        isPipelined && (i == 0) ? arg->GetType() : paramRegister->GetType();

                    parameterKnownValues[i] = arg->TryGetKnownValue(context, knownValueType);
                }
                else
                {
                    if (isPipelined && (i == 0))
                    {
                        // Assign locations to generated operations
                        SetOperationLocation sol(context, _location);

                        // Evaluate if the thread count count == 1
                        // This enables the loop generator to know this up-front, which improves timing inside the loop
                        // generator
                        {
                            Operation op = {};

                            op._opcode = Opcode::BinaryOp;
                            op._flags._binaryOpType = ParseTreeBinaryOpTypeEQ;

                            op._src.push_back(loopCountRegister->GetAccessedRegister());
                            op._src.push_back(1);
                            op._dst.push_back(functionInstance.GetThreadCountOneRegister()->GetAccessedRegister());

                            context._basicBlock->_operations.push_back(op);
                        }

                        // Predicate off the call if the loop count is zero
                        pushPopLoopCountZeroPredicate.reset(
                            new PushPopLoopCountZeroPredicate(context, loopCountRegister, _location));

                        // Subtract one from the loop count, to convert from loop count into max thread ID (which the
                        // loop generator expects)
                        {
                            Operation op = {};

                            op._opcode = Opcode::BinaryOp;
                            op._flags._binaryOpType = ParseTreeBinaryOpTypeSub;

                            op._src.push_back(loopCountRegister->GetAccessedRegister());
                            op._src.push_back(SourceOperand(1));

                            op._dst.push_back(loopCountRegister->GetAccessedRegister());

                            context._basicBlock->_operations.push_back(op);
                        }

                        Move(context, paramRegister, loopCountRegister, Opcode::MovCrossFunction, _location);
                    }
                    else
                    {
                        // Use MovCrossFunction to avoid the argument being optimized away by the per-function optimizer
                        Move(context, paramRegister, argumentRegisters[i], Opcode::MovCrossFunction, _location);
                    }
                }
            }

            const auto getSuccessor = [functionNode, calledObjectName]()
            { return functionNode->GetInstance(calledObjectName).GetFunction()->_start; };

            // Note that there is validation here, so it should be called even in the isInlineable case
            const CallModifiers callParams = GetCallParams(context);

            if (isInline || isInlineable)
            {
                assert(!context._functionInstanceStack.empty());
                auto callerInstance = context._functionInstanceStack.back();

                // Using [[transaction_size]] with inlining is not allowed (because it doesn't implement any
                // store-and-forward behavior) Auto-inlining is disabled for
                assert(callParams._transactionSize == 0);

                // Insert a "noop" to keep the stage for the call site
                if (GetCodeGenConfig()._optimize == 0)
                {
                    Operation op = {};

                    op._opcode = Opcode::NoOp;

                    op.InsertLocation(_location);

                    context._basicBlock->_operations.push_back(op);
                }

                context._basicBlock->_hasInlinedAtomicFunctions |= context.IsInAtomicBlock();

                context._basicBlock->_hasInlinedAtomicDoFunctions |= context._isInAtomicDo;

                GenerateInlineCall(context, functionDesc, functionNode, calledObjectName, functionInstance,
                                   parameterKnownValues, callDesc._calleeInstance, callerInstance, _location);
            }
            else
            {
                // Add assertion if transaction exceeds transaction size
                if (callParams._transactionSize > 0)
                {
                    // Verify that HasAttribute (used in auto-inlining decision) agrees
                    assert(HasAttribute(ParseTreeTransactionSizeAttr));

                    SetOperationLocation sol(context, _location);

                    // Atomically read and update size of current transaction
                    AtomicBlock atomicBlock(context, AtomicBlockType::Default, 1, context.AllocateAtomicChainIndex(),
                                            _location);

                    const size_t counterWidth = std::max<size_t>(Log2RoundUp(callParams._transactionSize + 1), 1);

                    const ObjectPath basicBlockPath = context._basicBlock->GetObjectPath();
                    const std::string suffixContainerPath =
                        "_TransactionSize_" +
                        std::to_string(context._program->transactionSizeCounterMap[context._basicBlock]++);
                    const ObjectPath containerPath = AppendToPath(basicBlockPath, suffixContainerPath);

                    // Create counter for number of calls in a transaction
                    const AccessedRegister counterReg = {AllocateRegister(
                        context._program, counterWidth, RegisterType::Global, "transaction_size", containerPath)};

                    // Initialize to 0
                    RegisterDescription& regDesc = context._program->_registerTable[counterReg._registerIndex];
                    regDesc.Global()._hasInitialValue = true;
                    regDesc.Global()._initialValue = 0;

                    const AccessedRegister incrementReg = {AllocateRegister(
                        context._program, counterWidth, RegisterType::Local, "transaction_size_plus_1")};

                    // Increment
                    Operation addOp = {};
                    addOp._opcode = Opcode::BinaryOp;
                    addOp._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;
                    addOp._src.push_back(counterReg);
                    addOp._src.push_back(1);
                    addOp._dst.push_back(incrementReg);
                    context._basicBlock->_operations.push_back(addOp);

                    const AccessedRegister writeBackReg = {
                        AllocateRegister(context._program, counterWidth, RegisterType::Local, "transaction_size_new")};

                    // Reset transaction size if last
                    assert(functionDesc._isLastParameterIndex);
                    const size_t isLastParameterIndex = *functionDesc._isLastParameterIndex;
                    assert(isLastParameterIndex < argumentRegisters.size());
                    Operation selectOp = {};
                    selectOp._opcode = Opcode::Select;
                    selectOp._src.push_back(
                        safe_cast<const AllocatedLeafRegister*>(argumentRegisters[isLastParameterIndex])
                            ->GetAccessedRegister());
                    selectOp._src.push_back(incrementReg);
                    selectOp._src.push_back(0);
                    selectOp._dst.push_back(writeBackReg);
                    context._basicBlock->_operations.push_back(selectOp);

                    // Write back
                    Operation writeOp = {};
                    writeOp._opcode = Opcode::WriteGlobal;
                    writeOp._src.push_back(writeBackReg);
                    writeOp._flags._writeGlobal._isPredicated = context.IsPredicated();
                    if (context.IsPredicated())
                    {
                        const AccessedRegister predicateReg = {context.GetPredicate()};
                        writeOp._src.push_back(predicateReg);
                    }
                    writeOp._dst.push_back(counterReg);
                    context._basicBlock->_operations.push_back(writeOp);

                    const AccessedRegister cmpResultReg = {
                        AllocateRegister(context._program, 1, RegisterType::Local, "transaction_size_exceeded")};

                    // Check that transaction size is less than max
                    Operation cmpOp = {};
                    cmpOp._opcode = Opcode::BinaryOp;
                    cmpOp._flags._binaryOpType = ParseTreeBinaryOpTypeLT;
                    cmpOp._src.push_back(writeBackReg);
                    cmpOp._src.push_back(callParams._transactionSize);
                    cmpOp._dst.push_back(cmpResultReg);
                    context._basicBlock->_operations.push_back(cmpOp);
                    AddAssert(context, cmpResultReg, "Transaction exceeded transaction size", _location);
                }

                if (isAsync)
                {
                    GenerateAsyncCall(context, functionInstance, getSuccessor, args, isPipelined, callParams,
                                      _location);
                }
                else
                {
                    {
                        // Assign locations to generated operations
                        SetOperationLocation sol(context, _location);

                        if (!isPipelined)
                        {
                            assert(!loopCountForContextSaver);

                            // Store the predicate in a 1-bit "loop count"
                            loopCountForContextSaver = dynamic_cast<const AllocatedLeafRegister*>(
                                g_compiler->GetLeafType(BaseType::Uint, 1, _location)
                                    ->AllocateRegisters(*context._program, RegisterType::Local,
                                                        "PredicateInLoopCount"));

                            Operation op = {};

                            op._opcode = Opcode::Mov;
                            op._dst.push_back(loopCountForContextSaver->GetAccessedRegister());

                            if (context.IsPredicated())
                            {
                                op._src.push_back(AccessedRegister{context.GetPredicate()});
                            }
                            else
                            {
                                op._src.push_back(SourceOperand(1));
                            }

                            context._basicBlock->_operations.push_back(op);
                        }

                        if (loopCountForContextSaver && context.IsPredicated())
                        {
                            // Implement predication in the context saver by setting the thread count to 0
                            // if the predicate value is 0
                            Operation op = {};

                            op._opcode = Opcode::BinaryOp;
                            op._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;
                            op._dst.push_back(loopCountForContextSaver->GetAccessedRegister());

                            op.PushOperand(loopCountForContextSaver->GetAccessedRegister(), false);

                            // Sign-extend the predicate to clear all bits of the thread count
                            op.PushOperand(AccessedRegister{context.GetPredicate()}, true);

                            context._basicBlock->_operations.push_back(op);
                        }
                    }

                    GenerateSyncCall(context, functionDesc, functionNode, calledObjectName, callSiteInstance,
                                     functionInstance, getSuccessor, isPipelined, isUnordered, callParams, GetType(),
                                     loopCountForContextSaver, _location);
                }
            }
        }

        // Decrement reference counts of strings contained within the return value
        if (GetType() != g_compiler->GetVoidType())
        {
            assert(context._writtenRegisters);

            const auto callback = [&](const AccessedRegister reg, const Type* const type)
            {
                if (g_compiler->GetStringType() == type)
                {
                    const AccessedRegister stringHandleReg = reg;

                    const bool isPredicated = context.IsPredicated();

                    const AccessedRegister predicateReg = {isPredicated ? context.GetPredicate()
                                                                        : c_invalidAccessedRegisterIndex};

                    IRContext* const contextPtr = &context;

                    context._typeContext.OnLeaveCurrentScope(
                        [this, stringHandleReg, contextPtr, isPredicated, predicateReg]()
                        {
                            Operation op = {};

                            op.InsertLocation(_location);

                            op._opcode = Opcode::ReferenceString;

                            if (isPredicated)
                            {
                                op._src.push_back(predicateReg);
                            }
                            else
                            {
                                // Literal 1 predicate value
                                op._src.push_back(SourceOperand(1, 1));
                            }

                            op._src.push_back(stringHandleReg);

                            op.PushOperand(c_stringReferenceNegOne, true);

                            contextPtr->_basicBlock->_operations.push_back(op);
                        });
                }
            };

            context._writtenRegisters->VisitRegisters(callback);
        }
    }
}

CallModifiers CallNode::GetCallParams(IRContext& context) const
{
    const std::vector<const ParseTreeNode*>& modifiers = dynamic_cast<const NodeList*>(_modifiers)->Children();

    CallModifiers params = {};

    params._callRate = 1;
    params._transactionSize = 0;
    params._minFifoDepth = 0;

    Flags flags = {};

    for (const ParseTreeNode* const modifier : modifiers)
    {
        auto attr = dynamic_cast<const IntAttributeNode*>(modifier);

        SetModifiers(context, attr, params, flags);
    }

    return params;
}

void GenerateCallTrace(std::ostream& str, const IRContext& context)
{
    if (!context._functionInstanceStack.empty())
    {
        // Get the leaf function instance from the function instance stack
        // All other instances are determined by the function instance enumerate
        // which is able to construct stacks containing calls to non-inline functions
        g_compiler->GetFunctionInstanceEnumerator().GenerateCallTrace(str, context._functionInstanceStack.back());
    }
}

void CallNode::GenerateAssert(IRContext& context) const
{
    // Record the fact that IR generation is inside of a assert operation
    PushPopIsInSimAssert pushPopIsInSimAssert(context);

    // Evaluate the assertion expression into a register
    const std::vector<const ParseTreeNode*>& args = dynamic_cast<const NodeList*>(_args)->Children();
    assert(args.size() == 1);

    const AllocatedLeafRegister* const conditionRegister =
        dynamic_cast<const AllocatedLeafRegister*>(EvaluateExpression(context, args[0]));

    std::ostringstream message;
    message << "Assertion at: " << g_compiler->GetSourceFileNameWithoutLeadingPath(_location._fileIndex)
            << " line: " << _location._beginLine << "\n";
    GenerateCallTrace(message, context);

    AddAssert(context, conditionRegister->GetAccessedRegister(), message.str(), _location);
}

void CallNode::GenerateCycleCounter(IRContext& context) const
{
    Operation op = {};

    op.InsertLocation(_location);

    op._opcode = Opcode::CycleCounter;

    op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister());

    context._basicBlock->_operations.push_back(op);
}

void CallNode::GenerateStringCount(IRContext& context) const
{
    Operation op = {};

    op.InsertLocation(_location);

    op._opcode = Opcode::StringCount;

    op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters)->GetAccessedRegister());

    context._basicBlock->_operations.push_back(op);
}

void CallNode::GenerateAssertStringEqual(IRContext& context) const
{
    const std::vector<const ParseTreeNode*>& args = dynamic_cast<const NodeList*>(_args)->Children();
    assert(args.size() == 2);

    Operation op = {};

    op.InsertLocation(_location);

    op._opcode = Opcode::AssertStringEqual;

    std::ostringstream errorStream;
    GenerateCallTrace(errorStream, context);

    std::string* errorMessage = g_compiler->Create<std::string>(errorStream.str());

    op._flags._assertStringEqual._message = errorMessage->c_str();

    if (context.IsPredicated())
    {
        const AccessedRegister predicateReg = {context.GetPredicate()};

        op._src.push_back(predicateReg);
    }
    else
    {
        // Literal 1 predicate value
        op._src.push_back(1);
    }

    for (size_t i = 0; i < args.size(); ++i)
    {
        const AllocatedLeafRegister* const expressionRegisters =
            safe_cast<const AllocatedLeafRegister*>(EvaluateExpression(context, args[i]));

        op._src.push_back(expressionRegisters->GetAccessedRegister());
    }

    context._basicBlock->_operations.push_back(op);
}

void CallNode::GeneratePrint(IRContext& context) const
{
    const std::vector<const ParseTreeNode*>& args = dynamic_cast<const NodeList*>(_args)->Children();
    assert(args.size() >= 1);

    Operation op = {};

    op.InsertLocation(_location);

    op._opcode = Opcode::Print;

    op.InsertLocation(_location);

    // Allocate vector of entries - 1 per src operand
    op._flags._print._entries = g_compiler->Create<std::vector<PrintEntry>>();

    // The 1st src operand is always a predicate value, to simplify back-ends
    if (context.IsPredicated())
    {
        DebuggerHintPushPredicate(context._basicBlock->_operations);

        const AccessedRegister predicate = {context.GetPredicate()};

        op._src.push_back(predicate);
    }
    else
    {
        op._src.push_back(1);
    }

    const auto callback = [&](const AccessedRegister reg, const Type* const type)
    {
        const size_t opIndex = op._src.size();

        op._src.push_back(reg);

        // Add information for the backend to determine how to print the value
        PrintEntry printEntry = {};

        printEntry._operandIndex = opIndex;

        if (IsSignedLeafType(type))
        {
            printEntry._type = PrintType::Int;
        }
        else if (type == g_compiler->GetStringType())
        {
            printEntry._type = PrintType::StringHandle;
        }
        else if (type == g_compiler->GetFloatType(32))
        {
            printEntry._type = PrintType::Float;
        }
        else
        {
            printEntry._type = PrintType::Uint;
        }

        op._flags._print._entries->push_back(printEntry);
    };

    for (size_t i = 0; i < args.size(); ++i)
    {
        // Try to avoid calling args[i]->GenerateIR
        // to runtime cost of allocating and freeing string handles
        const KnownValue knownArgValue = args[i]->TryGetKnownValue(context, g_compiler->GetStringType());

        if (knownArgValue._type == KnownValueType::String)
        {
            const size_t opIndex = op._src.size();

            op._src.push_back(knownArgValue._stringVal);

            PrintEntry printEntry = {};

            printEntry._operandIndex = opIndex;
            printEntry._type = PrintType::StringLiteral;

            op._flags._print._entries->push_back(printEntry);
        }
        else
        {
            const AllocatedRegister* const expressionRegisters = EvaluateExpression(context, args[i]);

            expressionRegisters->VisitRegisters(callback);
        }
    }

    context._basicBlock->_operations.push_back(op);

    if (context.IsPredicated())
    {
        DebuggerHintPopPredicate(context._basicBlock->_operations);
    }
}

void CallNode::GenerateDebugView(IRContext& context) const
{
    const std::vector<const ParseTreeNode*>& args = dynamic_cast<const NodeList*>(_args)->Children();

    assert(!args.empty());

    // Get label from the 1st argument
    const std::string label = StringFromNode(context, _location, args[0]);

    DebugViewBuilder debugViewBuilder(context, label, _location);

    // The remaining arguments are (name, values) pairs to debug inserted into the debug view
    for (size_t i = 1; i < args.size(); i += 2)
    {
        assert((i + 1) < args.size());

        const std::string name = StringFromNode(context, _location, args[i]);
        const AllocatedRegister* const expressionRegisters = EvaluateExpression(context, args[i + 1]);

        debugViewBuilder.AppendArgument(expressionRegisters, name);
    }

    debugViewBuilder.InsertOp(context._basicBlock->_operations, context._basicBlock->_operations.end());
}

CallNode::InstanceMap& CallNode::GetInstances() { return _instances; }

CallNode::Instance::Instance() : _callSiteIndex(std::numeric_limits<size_t>::max()), _returnBlock(nullptr) {}

void CallNode::Instance::SetCallSiteFunctionInstance(const FunctionInstance& callSiteFunctionInstance)
{
    _callSiteFunctionInstance = callSiteFunctionInstance;
}

const FunctionInstance& CallNode::Instance::GetCallSiteFunctionInstance() { return _callSiteFunctionInstance; }

void CallNode::Instance::SetCalledObjectName(const std::string& name)
{
    assert(_calledObjectName.empty());

    _calledObjectName = name;
}

const std::string& CallNode::Instance::GetCalledObjectName() const { return _calledObjectName; }

void CallNode::Instance::SetCallSiteIndex(const size_t callSiteIndex)
{
    // Should only be called once
    assert(_callSiteIndex == std::numeric_limits<size_t>::max());

    _callSiteIndex = callSiteIndex;
}

size_t CallNode::Instance::GetCallSiteIndex() const
{
    // SetCallSiteIndex should have been called
    assert(_callSiteIndex != std::numeric_limits<size_t>::max());

    return _callSiteIndex;
}

void CallNode::Instance::SetReturnBlock(BasicBlock* const block)
{
    assert(!_returnBlock);

    _returnBlock = block;
}

BasicBlock* CallNode::Instance::GetReturnBlock() const { return _returnBlock; }

void StructUnionNode::GenerateIR(IRContext& context) const {}

const AllocatedRegister* MemberAccessNode::GetRegisterFromContainer(IRContext& context,
                                                                    const AllocatedRegister* const inputContainer) const
{
    const ReferenceType* const containerReferenceType = dynamic_cast<const ReferenceType*>(inputContainer->GetType());

    const AllocatedStructRegister* const containerRegisters =
        dynamic_cast<const AllocatedStructRegister*>(inputContainer);

    // Find the coresponding member
    for (const auto& p : containerRegisters->_members)
    {
        if (p.first == _containedName)
        {
            return p.second;
        }
    }

    // Failed to find member
    assert(false);
    return nullptr;
}

// If any element in the chain f.x.y.b is a union
// then partial writes are not supported
bool MemberAccessNode::SupportsPartialWrites() const
{
    const StructUnionType* const structUnionType = dynamic_cast<const StructUnionType*>(_container->GetType());

    const bool isUnion = structUnionType && (structUnionType->_type == ContainerType::Union);

    if (isUnion)
    {
        return false;
    }
    else
    {
        return dynamic_cast<const LValNode*>(_container)->SupportsPartialWrites();
    }
}

bool MemberAccessNode::ContainerIsUnion() const
{
    const StructUnionType* const structUnionType = dynamic_cast<const StructUnionType*>(_container->GetType());

    const bool isUnion = structUnionType && (structUnionType->_type == ContainerType::Union);

    return isUnion;
}

const AllocatedRegister* MemberAccessNode::Load(IRContext& context) const
{
    const bool isUnion = ContainerIsUnion();

    // Get registers for the struct/union
    const AllocatedRegister* const containerRegisters = dynamic_cast<const LValNode*>(_container)->Load(context);

    const ReferenceType* const containerReferenceType =
        dynamic_cast<const ReferenceType*>(containerRegisters->GetType());

    if (containerReferenceType)
    {
        const VariableAccessNode* const van = dynamic_cast<const VariableAccessNode*>(_container);
        assert(van);

        const std::string objectName =
            g_compiler->GetFunctionInstanceEnumerator().GetReferencedObject(van, context._functionInstanceStack.back());

        const ClassType* const referencedClassType =
            dynamic_cast<const ClassType*>(containerReferenceType->_referencedType);
        assert(referencedClassType);

        return referencedClassType->GetRegistersForMember(objectName, _containedName, _location);
    }
    else if (isUnion)
    {
        // The entire union is 1 leaf register
        const AllocatedLeafRegister* const leafReg = dynamic_cast<const AllocatedLeafRegister*>(containerRegisters);

        // Cast from the leaf type to the member type

        // Create a node wrapping leafReg
        const AllocatedRegisterNode* const allocatedRegisterNode = g_compiler->Create<AllocatedRegisterNode>(
            g_compiler->GetLeafType(BaseType::Uint, GetType()->GetBitWidth(), _location), leafReg);

        // Create a cast node that casts to the member type
        const TypeNode* const typeNode = g_compiler->Create<TypeNode>(GetType());

        const CastNode* const castNode = g_compiler->Create<CastNode>(typeNode, allocatedRegisterNode);

        return EvaluateExpression(context, castNode);
    }
    else
    {
        return GetRegisterFromContainer(context, containerRegisters);
    }
}

void MemberAccessNode::GenerateIR(IRContext& context) const
{
    // Caller must have determined where the result should go
    assert(context._writtenRegisters != nullptr);

    const AllocatedRegister* const sourceRegisters = Load(context);

    Move(context, context._writtenRegisters, sourceRegisters, Opcode::Mov, _location);
}

void MemberAccessNode::Store(IRContext& context, const StoreCallback& storeCallbackIn) const
{
    const LValNode* const container = dynamic_cast<const LValNode*>(_container);

    if (ContainerIsUnion())
    {
        // The value could be a struct, composed of multiple registers
        // This requires special handling because the union is just 1 large uint

        // Write the new member value to a register
        const AllocatedRegister* const newMemberValue = Load(context);

        storeCallbackIn(context, newMemberValue);

        // Get the width of the value to be stored in the union
        const size_t srcWidth = newMemberValue->GetType()->GetBitWidth();

        const TypeNode* const typeNode =
            g_compiler->Create<TypeNode>(g_compiler->GetLeafType(BaseType::Uint, srcWidth, _location));

        // Wrap newMemberValue in a ParseTreeNode
        const AllocatedRegisterNode* const newMemberValueNode =
            g_compiler->Create<AllocatedRegisterNode>(GetType(), newMemberValue);

        // Cast from the src value into a uint of width "srcWidth"
        const CastNode* const castNode = g_compiler->Create<CastNode>(typeNode, newMemberValueNode);

        // castResult is a uint with width = srcWidth
        const AllocatedRegister* const castResult = EvaluateExpression(context, castNode);

        const auto storeCallback = [&](IRContext& context, const AllocatedRegister* const containerReg)
        {
            // Assign the result of the cast into the union
            LValNode::StoreRegister(context, containerReg, castResult, _location);
        };

        container->Store(context, storeCallback);
    }
    else if (container->SupportsPartialWrites())
    {
        // Get the register for the element to write, and store it
        const auto storeCallback = [&](IRContext& context, const AllocatedRegister* const containerReg)
        {
            // Extract the member from the input container
            const AllocatedRegister* const memberReg = GetRegisterFromContainer(context, containerReg);

            // Store into the member
            storeCallbackIn(context, memberReg);
        };

        container->Store(context, storeCallback);
    }
    else
    {
        // Writing into an array element like so:
        // Foo[4] ary;
        // ary[i].member = 2;
        //
        // Convert this into read-modify-write:
        // Foo tmp = ary[i];
        // tmp.member = 2;
        // ary[i] = tmp

        if (container->IsMemory())
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidArrayIndexing)
                << "Modifying a member of an array of structs is not supported with memories";
        }

        // Get a copy of the object
        const AllocatedRegister* const objectCopy = EvaluateExpression(context, _container);

        // Update the member
        const AllocatedRegister* const memberCopy = GetRegisterFromContainer(context, objectCopy);

        storeCallbackIn(context, memberCopy);

        // Update the container
        const auto storeCallback = [&](IRContext& context, const AllocatedRegister* const reg)
        { LValNode::StoreRegister(context, reg, objectCopy, _location); };

        container->Store(context, storeCallback);
    }
}

void MemberAccessNode::Initialize(IRContext& context, const ParseTreeNode* const rhs) const
{
    dynamic_cast<const LValNode*>(_container)->Initialize(context, rhs);
}

std::string MemberAccessNode::GetObjectName(KnownValueContext& context, const std::string& containingObjectName,
                                            const ResolveReferenceFunction& resolveReferenceCallback) const
{
    const LValNode* const containerLVal = dynamic_cast<const LValNode*>(_container);

    const std::string containerName =
        containerLVal->GetObjectName(context, containingObjectName, resolveReferenceCallback);

    return CombineObjectAndMemberName(Scope(), containerName, _containedName);
}

void ClassNode::GenerateIR(IRContext& context) const
{
    // In the case of nested classes, ClassNode::GenerateIR can be called multiple times
    // for the same ClassNode.  Only generate IR once (for all objects)
    if (_generatedIR)
    {
        return;
    }

    const ClassType* const classType = safe_cast<const ClassType*>(GetType());

    // GenerateIR for each object of this class
    const std::vector<std::string>& objectNames = classType->GetObjectNames();

    const auto setInterfaceMethods = [this](ExternalClassInstance& irModule)
    {
        // Record the set of all interface methods (used to determine if any are unreferenced)
        const auto callback = [&](const FunctionNode* functionNode)
        { SafeInsert(irModule._interfaceMethods, functionNode->GetName(), functionNode->GetFunctionDesc()); };

        EnumeratePublicMethods(callback);
    };

    // Each object gets a separate function in the IR
    for (const std::string& objectName : objectNames)
    {
        std::unique_ptr<PushPopExternalClassInstanceStack> pushPopExternalClassInstanceStack;

        if (IsExport() && (context._program->_exportClass != this))
        {
            // This export class will be instantiated (not implemented) in this pass
            // Treat it as an extern module
            context._program->_externClassInstances.push_back(ExternalClassInstance());

            ExternalClassInstance& irModule = context._program->_externClassInstances.back();

            irModule._name = classType->GetExportedName();
            irModule._scope = context._typeContext.GetNamespaceScope();
            irModule._objectName = objectName;
            irModule._isExportClass = true;
            irModule._classType = classType;

            // Store descriptions of all interface methods
            // So that ports for unreferenced functions can be connected
            setInterfaceMethods(irModule);

            // Record the set of callbacks from the class
            EnumerateCallbacks(
                [&](const std::string& name, const FunctionType* type)
                {
                    ExternalClassInstanceCallback irCallback = {};

                    irCallback._callbackName = name;
                    irCallback._type = type;

                    const FunctionNode* const function = TryGetExportCallback(objectName, name);

                    // This check handles callbacks with default values (not exposed at the interface)
                    if (function)
                    {
                        irCallback._calleeNode = function;
                        irCallback._calleeName = objectName;

                        irModule._callbacks.push_back(irCallback);
                    }
                });

            // Nesting export classes are not supported
            assert(context._externClassInstances.empty());

            // Record irModule in context
            pushPopExternalClassInstanceStack = std::make_unique<PushPopExternalClassInstanceStack>(context, &irModule);
        }

        // Each instance gets a separate scope, so that parameters and local variables are unique per-instance
        GenerateIRTypeContext::PushPopScope pushScope(context._typeContext);

        // Push the object name onto the context stack
        // So that function and variable nodes know which object IR is being generated for
        PushPopObjectNameStack pushObjectName(context, objectName);

        // Push the class type onto the stack
        PushPopClassType pushClassType(context, dynamic_cast<const ClassType*>(GetType()));

        // Initialize function nodes associated with callbacks
        // contained within the export class being compiled
        if (classType->IsExport())
        {
            if (context._program->_exportClass == this)
            {
                // Externs for this class to call
                for (const auto& p : _externCallbacks)
                {
                    p.second->GenerateIR(context);
                }
            }
            else
            {
                // Exports that are called by the exported class
                for (const auto& p : _exportCallbacks)
                {
                    // There are separate FunctionNode for each instance of this class
                    // Each of these FunctionNodes is flat (associated with the global object)
                    if (p.first.first == objectName)
                    {
                        p.second->GenerateIR(context);
                    }
                }
            }
        }

        if (classType->IsExternal())
        {
            assert(!pushPopExternalClassInstanceStack);

            // Record that IR is being generated for an extern class
            context._program->_externClassInstances.push_back(ExternalClassInstance());

            ExternalClassInstance& irModule = context._program->_externClassInstances.back();

            irModule._name = GetExternalClassName();

            // Don't prepend the module if the [[name]] attribue was used
            irModule._scope = classType->HasExternNameAttribute() ? Scope() : context._typeContext.GetNamespaceScope();

            irModule._objectName = objectName;
            irModule._isExportClass = false;
            irModule._classType = classType;

            if (classType->IsTemplate())
            {
                irModule._templateArguments = classType->GetClassNode()->GetTemplateArguments();
            }

            // Store descriptions of all interface methods
            // So that ports for unreferenced functions can be connected
            setInterfaceMethods(irModule);

            // Record the set of callbacks from the extern class
            EnumerateCallbacks(
                [&](const std::string& name, const FunctionType* type)
                {
                    ExternalClassInstanceCallback irCallback = {};

                    irCallback._callbackName = name;
                    irCallback._type = type;

                    const FunctionInstance calledInst =
                        g_compiler->GetFunctionInstanceEnumerator().DereferenceExternalClassCallback(
                            classType, objectName, name, _location);

                    irCallback._calleeNode = calledInst._functionNode;
                    irCallback._calleeName = calledInst._objectName;

                    irModule._callbacks.push_back(irCallback);
                });

            pushPopExternalClassInstanceStack = std::make_unique<PushPopExternalClassInstanceStack>(context, &irModule);
        }

        const std::vector<const ParseTreeNode*>& memberNodes = dynamic_cast<const NodeList*>(_members)->Children();

        for (const ParseTreeNode* const member : memberNodes)
        {
            member->GenerateIR(context);
        }
    }

    _generatedIR = true;
}

// Called when this class is being compiled to a top level module
// Mark interface functions as extern/export
void ClassNode::MarkInterfaceFunctions(const ParseTreeFunctionModifier modifierToAdd)
{
    const auto callback = [&](const FunctionNode* functionNode)
    { const_cast<FunctionNode*>(functionNode)->AddModifiers(modifierToAdd); };

    EnumeratePublicMethods(callback);
}

// Used when creating synthetic functions associated with
// export class callbacks
void ClassNode::CreateCallbacksHelper(const ParseTreeFunctionModifier modifier, const NotifyFunction& notifyFunction)
{
    assert(IsExportIgnoreTarget());

    // Search for callbacks within this class
    const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
    {
        const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);

        if (declareNode && dynamic_cast<const FunctionType*>(declareNode->GetDeclaredType()))
        {
            if (declareNode->GetAssignNode())
            {
                // Callbacks with initial values are not exported
                return;
            }

            // Use the location of the callback for the location of all parse tree nodes created here
            SetParseLocation spl(declareNode->GetLocation());

            const FunctionType* const functionType = safe_cast<const FunctionType*>(declareNode->GetDeclaredType());

            // callback of the class being compiled
            // Create a FunctionNode to represent
            // calls to this extern function
            ParseTreeNodePtr parameterList = ParseBaseList(nullptr);

            for (size_t i = 0; i < functionType->GetParamCount(); i++)
            {
                const std::string paramName = functionType->GetParamName(i);

                ParseTreeNodePtr paramAttributes = nullptr;

                // If the callback has [[last]], then ensure that the external function also does
                // so that if the callback has multiple call sites, arbitration will occur correctly.
                if ((modifier & ParseTreeFunctionModifierExternal) && functionType->HasIsLastParameter() &&
                    (functionType->GetIsLastParameterIndex() == i))
                {
                    paramAttributes = ParseBaseList(ParseFlagAttribute(DECLARE_FLAG_END_TRANSACTION));
                }
                else
                {
                    paramAttributes = ParseBaseList(nullptr);
                }

                ParseTreeNode* const param =
                    ParseFunctionParam(paramAttributes, g_compiler->Create<TypeNode>(functionType->GetParamType(i)),
                                       ParseIdentifier(paramName.c_str()), nullptr);

                ParseAppendList(parameterList, param);
            }

            ParseTreeNodePtr modifiers = nullptr;

            if (functionType->GetLatency())
            {
                modifiers = ParseBaseList(ParseFunctionModifier(
                    modifier | ParseTreeFunctionModifierExternalFixedLatency |
                    ParseTreeFunctionModifierExternalExternallyInstantiated | functionType->GetModifiers()));

                const std::string latencyAsString = std::to_string(*(functionType->GetLatency()));

                modifiers = ParseAppendList(
                    modifiers, ParseIntAttribute(ParseTreeLatencyAttr, ParseDecimalLiteral(latencyAsString.c_str())));
            }
            else
            {
                modifiers = ParseBaseList(ParseFunctionModifier(modifier | functionType->GetModifiers()));
            }

            FunctionNode* const newFunctionNode =
                g_compiler->Create<FunctionNode>(modifiers, g_compiler->Create<TypeNode>(functionType->GetReturnType()),
                                                 ParseIdentifier(declareNode->GetDeclaredName().c_str()), parameterList,
                                                 ParseBaseList(nullptr), declareNode->GetDeclaredName());

            newFunctionNode->SetClassType(g_compiler->GetGlobalClassType());

            notifyFunction(declareNode->GetDeclaredName(), newFunctionNode);
        }
        else
        {
            recurseCallback();
        }
    };

    VisitContext visitContext = {};

    VisitContext::PushPopScope outerScope(visitContext);

    Visit(callback, visitContext);
}

// Called when compiling a definition an exported class
// Creates extern functions that are connected to callbacks of the export class
void ClassNode::CreateExternCallbacks()
{
    assert(_externCallbacks.empty());

    CreateCallbacksHelper(ParseTreeFunctionModifierExternal | ParseTreeFunctionModifierExportClassInterface,
                          [&](const std::string& name, FunctionNode* const externFunctionNode)
                          { SafeInsert(_externCallbacks, name, externFunctionNode); });
}

// Called when compiling a use an exported class
// Creates export functions that are connected to callbacks of the export class
void ClassNode::CreateExportCallbacks(const std::string& objectName)
{
    // ParseTreeFunctionModifierNoSrcTxWarning is to disable warnings related to calls
    // from these exported functions.  The assumption is that external code
    // which calls the callback will not make the function call until they have
    // an entire transaction ready to go
    CreateCallbacksHelper(ParseTreeFunctionModifierExport | ParseTreeFunctionModifierExportClassInterface | ParseTreeFunctionModifierNoSrcTxWarning,
                          [&](const std::string& name, FunctionNode* const exportFunctionNode)
                          {
                              const ExportCallbackKey key(objectName, name);

                              SafeInsert(_exportCallbacks, key, exportFunctionNode);
                          });
}

void ClassNode::Reset()
{
    _members = _originalMembers;

    _generatedIR = false;

    // Handle the case where a class is the top-level module being compiled in 1 pass
    // and then is not in another pass.
    _externCallbacks.clear();

    _exportCallbacks.clear();
}

void SwitchNode::GenerateIR(IRContext& context) const
{
    const std::string testExpressionString = _testExpression->GetExpressionString();

    // Evaluate the test expression into a register
    const AllocatedLeafRegister* const testExprValue =
        dynamic_cast<const AllocatedLeafRegister*>(EvaluateExpression(context, _testExpression));

    // Compare the test expression to each case statement
    const std::vector<const ParseTreeNode*>& switchBlocks = dynamic_cast<const NodeList*>(_blockList)->Children();

    std::vector<AccessedRegister> conditionRegisters;

    const size_t invalidBlockIndex = std::numeric_limits<size_t>::max();

    size_t defaultBlockIndex = invalidBlockIndex;

    // For each case Y: ... break; block
    for (size_t switchBlockIndex = 0; switchBlockIndex < switchBlocks.size(); ++switchBlockIndex)
    {
        const SwitchBlockNode* const switchBlockNode =
            dynamic_cast<const SwitchBlockNode*>(switchBlocks[switchBlockIndex]);

        std::string switchConditionString;

        const ParseTreeNode* const caseValue = switchBlockNode->GetCaseValue();

        if (caseValue)
        {
            // case Y: Y must be known at compile time
            const KnownValue switchBlockValue =
                switchBlockNode->GetCaseValue()->TryGetKnownValue(context, switchBlockNode->GetCaseValue()->GetType());

            if (switchBlockValue._type != KnownValueType::Int)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidSwitch)
                    << "Case value not known at compile time";
            }

            switchConditionString = testExpressionString + "_eq_" + std::to_string(MpToSizeT(switchBlockValue._intVal));
        }
        else
        {
            switchConditionString = testExpressionString + "_default_case";
        }

        const AccessedRegister conditionRegister = {
            AllocateRegister(*context._program, 1, RegisterType::Local, switchConditionString)};

        conditionRegisters.push_back(conditionRegister);

        if (caseValue)
        {
            // Assign locations to generated operations
            SetOperationLocation sol(context, _location);

            // case Y: Y must be known at compile time
            const KnownValue switchBlockValue =
                switchBlockNode->GetCaseValue()->TryGetKnownValue(context, switchBlockNode->GetCaseValue()->GetType());
            assert(KnownValueType::Int == switchBlockValue._type);

            {
                Operation op = {};

                op._opcode = Opcode::BinaryOp;
                op._flags._binaryOpType = ParseTreeBinaryOpTypeEQ;

                op._src.push_back(testExprValue->GetAccessedRegister());
                op._src.push_back(SourceOperand(switchBlockValue._intVal));

                op._dst.push_back(conditionRegister);

                context._basicBlock->_operations.push_back(op);
            }
        }
        else
        {
            // This is a default: block
            // There can be at most 1
            assert(defaultBlockIndex == invalidBlockIndex);

            defaultBlockIndex = switchBlockIndex;
        }
    }

    assert(conditionRegisters.size() == switchBlocks.size());

    // Given the width of the test expression, compute the maximum number of possible switch blocks
    const size_t maxSwitchBlocks = 1ull << dynamic_cast<const LeafType*>(_testExpression->GetType())->_width;

    // Evaluate the inverse condition for the default block
    // (false means that no other block was taken)
    if (defaultBlockIndex != invalidBlockIndex)
    {
        // Assign locations to generated operations
        SetOperationLocation sol(context, _location);

        // Compute the OR of all conditions
        std::vector<AccessedRegister> reduceInputRegs;

        for (size_t switchBlockIndex = 0; switchBlockIndex < switchBlocks.size(); ++switchBlockIndex)
        {
            if (switchBlockIndex != defaultBlockIndex)
            {
                reduceInputRegs.push_back(conditionRegisters[switchBlockIndex]);
            }
        }

        OperationList reduceOps = ReduceVecBinaryOp(
            *context._program, reduceInputRegs, ParseTreeBinaryOpTypeOr, conditionRegisters[defaultBlockIndex],
            switchBlocks[defaultBlockIndex]->GetLocation(), "DefaultSwitchPredicate", 0);

        context._basicBlock->_operations.splice(context._basicBlock->_operations.end(), reduceOps);
    }

    // Handles updates to local variables
    ConditionalLocalUpdates conditionalLocalUpdates(context, _location);

    const std::string codeCoverageName = context.GenerateCodeCoverageName(_location, CodeCoverageType::SwitchStatement);

    // For each case Y: ... break; block
    for (size_t switchBlockIndex = 0; switchBlockIndex < switchBlocks.size(); ++switchBlockIndex)
    {
        const SwitchBlockNode* const switchBlockNode =
            dynamic_cast<const SwitchBlockNode*>(switchBlocks[switchBlockIndex]);

        // Push a predicate for the switch condition
        const AccessedRegister conditionRegister = conditionRegisters[switchBlockIndex];

        // For the default statement, the inverse of the condition is needed (NOT any of the non-default conditions)
        conditionalLocalUpdates.BeginCase(conditionRegister._registerIndex,
                                          switchBlockNode->GetCaseValue()
                                              ? ConditionalLocalUpdates::PredicateMode::Normal
                                              : ConditionalLocalUpdates::PredicateMode::Inverse);

        // Add code coverage
        if (GetCodeGenConfig()._codeCoverage)
        {
            const AccessedRegister predicateReg = {context.GetPredicate()};

            const std::string description = "Code coverage for Switch statement at " + LocationToString(_location);
            CodeCoverage codeCoverage = {};
            codeCoverage._coverageType = CodeCoverageType::SwitchStatement;
            codeCoverage._statements = switchBlockNode->GetLineBounds();
            codeCoverage._condition = _location;
            if (switchBlockNode->GetCaseValue())
            {
                codeCoverage._case = switchBlockNode->GetCaseValue()
                                         ->TryGetKnownValue(context, switchBlockNode->GetCaseValue()->GetType())
                                         ._intVal.str();
            }
            else
            {
                codeCoverage._case = "default";
            }
            AddCodeCoverageContext(codeCoverage, context);

            OperationList codeCoverageOps = CreateCodeCoverageCounter(context._program, codeCoverageName, description,
                                                                      switchBlockNode->GetLocation(), predicateReg,
                                                                      codeCoverage, *context._basicBlock);
            context._basicBlock->_operations.splice(context._basicBlock->_operations.end(), codeCoverageOps);
        }

        // Emit IR for the body of the switch block
        switchBlockNode->GetStatementList()->GenerateIR(context);

        conditionalLocalUpdates.EndCase();
    }

    // If there is a default case, or all possible switch expressions are specified
    // and a variable is written in all switch blocks
    // then conditionalLocalUpdates can ignore the original value of that variable
    if ((defaultBlockIndex != invalidBlockIndex) || (switchBlocks.size() == maxSwitchBlocks))
    {
        conditionalLocalUpdates.SetComplete();
    }
}

void SwitchBlockNode::GenerateIR(IRContext& context) const {}

void AllocatedRegisterNode::GenerateIR(IRContext& context) const
{
    assert(context._writtenRegisters);

    Move(context, context._writtenRegisters, _src, Opcode::Mov, _location);
}

void StaticNode::GenerateIR(IRContext& context) const
{
    const KnownValue knownValue = TryGetKnownValue(context, GetType());

    if (knownValue._type != KnownValueType::Int)
    {
        g_compiler->ErrorStream(_location, CompileError::ExpressionNotStatic)
            << "Expression value is not known at compile time";
    }

    _expression->GenerateIR(context);
}

void FanOutNode::GenerateIR(IRContext& context) const
{
    // Evaluate the source expression
    const AllocatedRegister* const src = EvaluateExpression(context, _expression);

    // Determine output count
    const ArrayTypeBase* const resultType = dynamic_cast<const ArrayTypeBase*>(GetType());

    const size_t elementCount = resultType->_arraySize;

    const AllocatedArrayRegister* const dstReg = dynamic_cast<const AllocatedArrayRegister*>(context._writtenRegisters);

    // Move from src to each output element
    for (size_t i = 0; i < elementCount; ++i)
    {
        Move(context, dstReg->_elements[i], src, Opcode::FanOut, _location);
    }
}

void StageNode::GenerateIR(IRContext& context) const
{
    Operation op = {};

    op._opcode = Opcode::Stage;

    op.InsertLocation(_location);

    context._basicBlock->_operations.push_back(op);
}

void ConcatNode::GenerateIR(IRContext& context) const
{
    const std::vector<const ParseTreeNode*>& elements = dynamic_cast<const NodeList*>(_elements)->Children();

    // Allocate a register to hold the result
    const AllocatedRegister* const resultReg =
        GetType()->AllocateRegisters(*context._program, RegisterType::Local, "ConcatTemp");

    Operation op = {};

    SetOperationLocation sol(context, _location);

    op._opcode = Opcode::Gather;

    op._dst.push_back(dynamic_cast<const AllocatedLeafRegister*>(resultReg)->GetAccessedRegister());

    // Allocate memory to hold the gather table
    op._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();

    size_t currOffset = 0;

    // Traverse backwards, so that the first element is the most significant
    for (auto it = elements.rbegin(); it != elements.rend(); ++it)
    {
        const ParseTreeNode* const element = *it;

        const auto callback = [&](const AccessedRegister sourceReg, const Type* const sourceLeafType)
        {
            const size_t sourceWidth = sourceLeafType->GetBitWidth();

            GatherEntry gatherEntry = {};

            gatherEntry._sourceOffset = 0;
            gatherEntry._destOffset = currOffset;
            gatherEntry._numBits = sourceWidth;

            op._flags._gather._entries->push_back(gatherEntry);

            op._src.push_back(sourceReg);

            currOffset += sourceWidth;
        };

        // Evaluate the element into a register
        const AllocatedRegister* const elementReg = EvaluateExpression(context, element);

        // Visit leaf registers, adding 1 gather entry for each
        elementReg->VisitRegisters(callback);
    }

    assert(currOffset == GetType()->GetBitWidth());

    context._basicBlock->_operations.push_back(op);

    // Move from resultReg to the final result
    Move(context, context._writtenRegisters, resultReg, Opcode::Mov, _location);
}

void ReorderNode::GenerateIR(IRContext& context) const
{
    const std::pair<Location, Location> lineBounds = GetLineBounds();

    // Increment _reorderStackDepth
    PushPopReorderStackDepth pushPopReorderStackDepth(context);

    ReorderHelper reorderHelper(context, _location);

    reorderHelper.EmitPreamble();

    // Generate basic blocks for the body
    _body->GenerateIR(context);

    // Finish the last basic block in the body and create a new one
    BasicBlock* const lastReorderBlock = FinalizeBasicBlock(context);

    BasicBlock* const afterReorderBlock = context.CreateBasicBlock(lineBounds.second);
    context._basicBlock = afterReorderBlock;

    // Link lastReorderBlock to afterReorderBlock - the link is the reorder buffer
    AppendReorderingJump(lastReorderBlock, afterReorderBlock, reorderHelper.GetSlotRegister()->GetAccessedRegister(),
                         _location);

    reorderHelper.EmitPostamble(afterReorderBlock);
}

void InlineExternalModuleNode::GenerateIR(IRContext& context) const
{
    if (!context._enclosingClassType->IsExternal() && (context._enclosingClassType != g_compiler->GetGlobalClassType()))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidExternFunction)
            << "Methods of non-external classes must have implementations";
    }

    if (!context._externClassInstances.empty())
    {
        // Fixed latency method of an extern class instance
        ExternalClassInstance* const externModule = context._externClassInstances.top();

        FunctionDesc methodDesc = {};

        methodDesc._fixedLatency = _latency;

        methodDesc._returnType = _returnTypeNode->GetType();

        methodDesc._modifiers = GetModifiers();

        if (methodDesc._returnType->GetRegisterCount() > 1)
        {
            g_compiler->ErrorStream(_location, CompileError::ExportTypeRestriction)
                << "Fixed-latency extern methods cannot have composite return types";
        }

        const std::vector<const ParseTreeNode*>& params = dynamic_cast<const NodeList*>(_paramsNode)->Children();

        for (const ParseTreeNode* const param : params)
        {
            const DeclareNode* const paramDeclareNode = safe_cast<const DeclareNode*>(param);

            methodDesc._parameterNames.push_back(paramDeclareNode->GetDeclaredName());
            methodDesc._parameterTypes.push_back(paramDeclareNode->GetDeclaredType());

            if (paramDeclareNode->GetDeclaredType()->GetRegisterCount() != 1)
            {
                g_compiler->ErrorStream(_location, CompileError::ExportTypeRestriction)
                    << "Fixed-latency extern method parameters cannot have composite types";
            }
        }

        SafeInsert(externModule->_fixedLatencyMethods, GetName(), methodDesc);
    }
}

void StaticAssertNode::GenerateIR(IRContext& context) const
{
    const KnownValue assertionValue = _expr->TryGetKnownValue(context, _expr->GetType());

    if (assertionValue._type != KnownValueType::Int)
    {
        std::ostringstream call_trace;
        GenerateCallTrace(call_trace, context);
        g_compiler->ErrorStream(_location, CompileError::ExpressionNotStatic)
            << "static_assert value not known at compile time\n"
            << call_trace.str();
    }

    if (assertionValue._intVal == 0)
    {
        std::ostringstream call_trace;
        GenerateCallTrace(call_trace, context);
        g_compiler->ErrorStream(_location, CompileError::StaticAssert)
            << "static_assert failed at: " << g_compiler->GetSourceFileName(_location._fileIndex)
            << " line: " << _location._beginLine << "\n"
            << call_trace.str();
    }
}

void ExportTypeNode::GenerateIR(IRContext& context) const
{
    const Type* const type = _typeNode->GetType();

    // Exported classes are not added to the listed of exported types
    // Instead exported classes are handled at a higher level
    // by treating each exported class as a separate compilation unit.
    if (dynamic_cast<const ClassType*>(type) == nullptr)
    {
        ExportTypeHelper(*context._program, _typeNode, type, ExportTypeBehavior::Default);
    }
}

void ExternNode::GenerateIR(IRContext& context) const {}

void NamespaceNode::GenerateIR(IRContext& context) const
{
    const std::string& namespaceName = GetName();

    GenerateIRTypeContext::PushPopScope pushScope(context._typeContext, &namespaceName);

    _bodyNode->GenerateIR(context);
}

void InitializerListNode::GenerateIR(IRContext& context) const
{
    assert(context._writtenRegisters);

    const std::vector<const ParseTreeNode*>& children = _list->Children();

    const AllocatedArrayRegister* const destArrayRegisters =
        dynamic_cast<const AllocatedArrayRegister*>(context._writtenRegisters);

    const AllocatedStructRegister* const destStructRegisters =
        dynamic_cast<const AllocatedStructRegister*>(context._writtenRegisters);

    const AllocatedLeafRegister* const destLeafRegisters =
        dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters);

    const StructUnionType* const structUnionType = dynamic_cast<const StructUnionType*>(GetType());

    std::vector<const AllocatedRegister*> destRegisters;

    if (destArrayRegisters)
    {
        for (const auto& element : destArrayRegisters->_elements)
        {
            destRegisters.push_back(element);
        }
    }
    else if (destStructRegisters)
    {
        for (const auto& member : destStructRegisters->_members)
        {
            destRegisters.push_back(member.second);
        }
    }
    else if (destLeafRegisters)
    {
        // For initializing a union
        assert(children.size() < 2);
        destRegisters.push_back(context._writtenRegisters);
    }
    else
    {
        // Empty initializer list, zero all written registers
        assert(children.empty());
        destRegisters.push_back(context._writtenRegisters);
    }

    for (size_t i = 0; i < destRegisters.size(); i++)
    {
        if (i < children.size())
        {
            if (destLeafRegisters && structUnionType && (structUnionType->_type == ContainerType::Union))
            {
                // Only the first field of a union can be initialized
                assert(i == 0);

                // Truncate the expression to the type of the first field
                const auto& member = structUnionType->_members.at(i);

                const Type* const firstFieldType = member.second->GetDeclaredType();

                const AllocatedRegister* const firstFieldRegister =
                    firstFieldType->AllocateRegisters(*context._program, RegisterType::Local, member.first);

                EvaluateExpressionIntoRegister(context, firstFieldRegister, children[i]);

                Move(context, destRegisters[i], firstFieldRegister, Opcode::Mov, _location);
            }
            else
            {
                // Write the expression result into the destination
                EvaluateExpressionIntoRegister(context, destRegisters[i], children[i]);
            }
        }
        else
        {
            const auto clearCallback = [&](const AccessedRegister reg, const Type* const type)
            {
                // Add an operation which will set the initial value of the variable
                Operation op = {};

                op._opcode = Opcode::Clear;
                op.InsertLocation(_location);
                op._dst.push_back(reg);

                context._basicBlock->_operations.push_back(op);
            };

            destRegisters[i]->VisitRegisters(clearCallback);
        }
    }
}

// Called to determine initial value an element of a memory
// No matter how complicated the type of the initializer list is
// all bits are concatenated
mp_int BaseInitializerListNode::GetFlatValue(IRContext& context) const
{
    const StructUnionType* const structUnionType = dynamic_cast<const StructUnionType*>(GetType());
    const ArrayTypeBase* const arrayType = dynamic_cast<const ArrayTypeBase*>(GetType());

    // This covers the case of an empty initializer list
    // as well as sparse initializer lists
    mp_int result = 0;

    size_t offset = 0;

    const size_t fieldCount = structUnionType ? structUnionType->_members.size()
                              : arrayType     ? arrayType->_arraySize
                                              : 0;

    // For each field in the associated type
    for (size_t fieldIndex = 0; fieldIndex < fieldCount; fieldIndex++)
    {
        std::string fieldName;
        const Type* fieldType = nullptr;

        if (structUnionType)
        {
            const StructUnionType::EntryType& typeField = structUnionType->_members.at(fieldIndex);

            fieldName = typeField.first;
            fieldType = typeField.second->GetDeclaredType();
        }
        else
        {
            assert(arrayType);
            fieldType = arrayType->_elementType;
        }

        const ParseTreeNode* const fieldNode = TryGetField(fieldIndex, fieldName);

        if (fieldNode)
        {
            const BaseInitializerListNode* const childInitializerList =
                dynamic_cast<const BaseInitializerListNode*>(fieldNode);

            mp_int childFlatValue = 0;

            if (childInitializerList)
            {
                childFlatValue =
                    TypeConvert(childInitializerList->GetFlatValue(context), fieldNode->GetType(), fieldType);
            }
            else
            {
                const KnownValue knownValue = fieldNode->TryGetKnownValue(context, fieldType);

                if (knownValue._type == KnownValueType::Int)
                {
                    childFlatValue = knownValue._intVal;
                }
                else
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidInitializerList)
                        << "Initializer list element not known at compile time";
                }
            }

            result = result | (childFlatValue << offset);
        }

        if (!structUnionType || (ContainerType::Struct == structUnionType->_type))
        {
            offset += fieldType->GetBitWidth();
        }
        else
        {
            // Do not increment offset when processing unions
            // All fields start at offset 0
            assert(structUnionType && (ContainerType::Union == structUnionType->_type));
        }
    }

    return result;
}

void DesignatedInitializerListNode::GenerateIR(IRContext& context) const
{
    assert(context._writtenRegisters);

    const StructUnionType* const type = safe_cast<const StructUnionType*>(GetType());

    const AllocatedStructRegister* const destStructRegisters =
        dynamic_cast<const AllocatedStructRegister*>(context._writtenRegisters);

    // Used for unions
    const AllocatedLeafRegister* const destLeafRegisters =
        dynamic_cast<const AllocatedLeafRegister*>(context._writtenRegisters);

    if (destStructRegisters)
    {
        assert(type->_type == ContainerType::Struct);

        for (const auto& dstMember : destStructRegisters->_members)
        {
            const std::string& dstMemberName = dstMember.first;
            const AllocatedRegister* const dstMemberRegister = dstMember.second;

            const auto it = _children.find(dstMemberName);
            if (it != _children.end())
            {
                EvaluateExpressionIntoRegister(context, dstMemberRegister, it->second->GetValue());
            }
            else
            {
                const auto clearCallback = [&](const AccessedRegister reg, const Type* const type)
                {
                    // Add an operation which will set the initial value of the variable
                    Operation op = {};

                    op._opcode = Opcode::Clear;
                    op.InsertLocation(_location);
                    op._dst.push_back(reg);

                    context._basicBlock->_operations.push_back(op);
                };

                dstMemberRegister->VisitRegisters(clearCallback);
            }
        }
    }
    else
    {
        assert(destLeafRegisters);

        assert(type->_type == ContainerType::Union);

        // The front-end disallows initializing multiple union fields
        // If an initializer list is empty, then InitializerListNode will be used rather than
        // DesignatedInitializerListNode
        assert(1 == _children.size());

        const auto& p = *(_children.begin());

        // Evaluate the expression value a single flat register
        const AllocatedLeafRegister* const rhs = EvaluateAndFlattenExpression(context, p.second->GetValue());

        // Move into the destination
        // This will zero any bits in the union that are not covered by the particular field that is initialized
        Move(context, context._writtenRegisters, rhs, Opcode::Mov, _location);
    }
}

// All reference tracking is done during function instance enumeration
void ThisNode::GenerateIR(IRContext& context) const {}

void LiteralStringNode::GenerateIR(IRContext& context) const
{
    const auto cb = [this](Operation& op) { AppendFormatStringEntry(op, FormatStringType::StringLiteral, _value); };

    FormatString(context, _location, cb);
}

void InterpolatedStringNode::GenerateIR(IRContext& context) const
{
    const auto cb = [&](Operation& op)
    {
        for (const ParseTreeNode* const seg : _segments->Children())
        {
            const InterpolatedStringSegmentNode* const segment = safe_cast<const InterpolatedStringSegmentNode*>(seg);

            AppendFormatStringEntry(op, FormatStringType::StringLiteral, segment->GetString());

            const InterpolationExpressionNode* const expression = segment->GetExpression();

            if (expression)
            {
                expression->Interpolate(context, op);
            }
        }
    };

    FormatString(context, _location, cb);
}

void InterpolationExpressionNode::Interpolate(IRContext& context, Operation& op) const
{
    assert(_expression);

    const AllocatedRegister* const reg = EvaluateExpression(context, _expression);

    reg->Interpolate(context, op, _alignment, _precision, _format);
}

void EnumType::Interpolate(IRContext& context, Operation& outerOp, const AccessedRegister valueReg,
                           const size_t alignment) const
{
    // Assign the variable to a string handle which represents the case
    // where the register value doesn't match any members of the enumeration
    const AccessedRegister stringHandleReg = AllocateAndFormatString(
        context, _location,
        [this, valueReg](Operation& op)
        {
            assert(Opcode::FormatEnum == op._opcode);
            assert(op._flags._formatEnum._entries->empty());
            assert(op._flags._formatEnum._defaultString->empty());

            op._src.push_back(valueReg);

            *op._flags._formatEnum._entries = _constants;

            *op._flags._formatEnum._defaultString = "does not represent a valid " + _name;
        },
        Opcode::FormatEnum);

    AppendFormatStringEntry(outerOp, FormatStringType::StringHandle, stringHandleReg, alignment);
}

void CallNode::SetModifiers(IRContext& context, const IntAttributeNode* attr, CallModifiers& modifiers,
                            Flags& flags) const
{
    if (attr->_attribute == ParseTreeCallRateAttr)
    {
        assert(!flags._callRateSpecified);

        const uint64_t callRate = attr->_value;

        if (callRate == 0)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidCall) << "Call rate cannot be 0";
        }

        if ((callRate > 1) && !context.IsPredicated())
        {
            g_compiler->ErrorStream(_location, CompileError::CallRateNotPredicated)
                << "Non-trivial call rate is used, but all threads executing this path will result in a call.  Control "
                   "flow is needed to ensure that execution is predicated off for some threads.";
        }

        modifiers._callRate = static_cast<size_t>(callRate);

        flags._callRateSpecified = true;
    }
    else if (attr->_attribute == ParseTreeFifoDepthAttr)
    {
        assert(!flags._fifoDepthSpecified);

        const uint64_t fifoDepth = attr->_value;

        modifiers._minFifoDepth = static_cast<size_t>(fifoDepth);

        flags._fifoDepthSpecified = true;
    }
    else if (attr->_attribute == ParseTreeTransactionSizeAttr)
    {
        assert(!flags._transactionSizeSpecified);

        const uint64_t transactionSize = attr->_value;

        if (transactionSize == 0)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidCall) << "Transaction size cannot be 0";
        }

        modifiers._transactionSize = static_cast<size_t>(transactionSize);

        flags._transactionSizeSpecified = true;
    }
    else
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidCall) << "Invalid call attribute";
    }
}

// Calls a callback for each operation in an basic block, in forward order
// Can iterate over operations in scheduled or unscheduled basic blocks
void ForEachOperationForward(BasicBlock& basicBlock, const OperationCallback& callback,
                             const OperationEnumerationMode mode)
{
    if (mode == OperationEnumerationMode::Unscheduled)
    {
        // iterate through start condition operations
        for (Operation& op : basicBlock._startConditionOperations)
        {
            callback(op);
        }

        // iterate through unscheduled instructions
        for (auto it = basicBlock._operations.begin(); it != basicBlock._operations.end(); ++it)
        {
            Operation& op = *it;

            callback(op);
        }
    }
    else
    {
        assert(mode == OperationEnumerationMode::Scheduled);
    }

    // Next iterate through appended stages/schedule operations
    for (auto stageIt = basicBlock._stages.begin(); stageIt != basicBlock._stages.end(); ++stageIt)
    {
        Stage& stage = *stageIt;

        for (auto it = stage._operations.begin(); it != stage._operations.end(); ++it)
        {
            Operation& op = *it;

            callback(op);
        }
    }
}

// Calls a callback for each operation in an basic block, in reverse order
// Can iterate over operations in scheduled or unscheduled basic blocks
void ForEachOperationReverse(BasicBlock& basicBlock, const OperationCallback& callback,
                             const OperationEnumerationMode mode)
{
    // First iterate through appended stages/schedule operations
    for (auto stageIt = basicBlock._stages.rbegin(); stageIt != basicBlock._stages.rend(); ++stageIt)
    {
        Stage& stage = *stageIt;

        for (auto it = stage._operations.rbegin(); it != stage._operations.rend(); ++it)
        {
            Operation& op = *it;

            callback(op);
        }
    }

    if (mode == OperationEnumerationMode::Unscheduled)
    {
        // Next iterate through unscheduled instructions
        for (auto it = basicBlock._operations.rbegin(); it != basicBlock._operations.rend(); ++it)
        {
            Operation& op = *it;

            callback(op);
        }

        // iterate through start condition operations
        for (auto it = basicBlock._startConditionOperations.rbegin(); it != basicBlock._startConditionOperations.rend();
             ++it)
        {
            Operation& op = *it;

            callback(op);
        }
    }
    else
    {
        assert(mode == OperationEnumerationMode::Scheduled);
    }
}

void RemoveOperations(BasicBlock& basicBlock, const OperationRemoveCallback& callback,
                      const OperationEnumerationMode mode)
{
    if (mode == OperationEnumerationMode::Unscheduled)
    {
        // scan&remove from the start condition operations
        basicBlock._startConditionOperations.remove_if(callback);

        // scan&remove from the unscheduled instructions
        basicBlock._operations.remove_if(callback);
    }
    else
    {
        assert(mode == OperationEnumerationMode::Scheduled);
    }

    // Next iterate through appended stages/schedule operations
    for (Stage& stage : basicBlock._stages)
    {
        stage._operations.remove_if(callback);
    }
}

struct OpcodeTableEntry
{
    Opcode _opcode;
    const char* _name; // null for cases where the name depends on operation flags
    bool _isLive;
    bool _preserveInputReg;
    bool _allowedInAtomic;
    bool _registeredInput;
    bool _registeredOutput;
    bool _hasIoRoutingDelay;
    bool _commonSubexpression;
    bool _lowerToLut;
    bool _optimizationBarrier;
    bool _typeBarrier;   // true if 2 operations with the same opcode should not be reordered
    bool _assertBarrier; // true if this operation should not be moved forward past and assert (because the assert can
                         // stop simulation)
    bool _externalInputRegisters;
    bool _canCauseBackpressure;
    bool _allowBitbucket; // true if outputs should not be mapped to RegisterType::BitBucket
    bool _mergeAtomicDo;
    bool _readsStringTable;
    bool _writesStringTable;
    int _pathLength; // levels of logic used to implement.  -1 means that more than just the opcode is needed to
                     // determine the length.
};

// Keep the formatting of this table spreedsheet-like, for easier reference.
// clang-format off
const OpcodeTableEntry g_opcodeTable[] = {
    // _opcode,                      _name                   _isLive,    _preserveInputReg,  _allowedInAtomic,   _registeredInput,   _registeredOutput,   _hasIoRoutingDelay, _commonSubexpression,  _lowerToLut,    _optimizationBarrier,   _typeBarrier,  _assertBarrier,      _externalInputRegisters, _canCauseBackpressure, _allowBitbucket, _mergeAtomicDo, _readsStringTable, _writesStringTable, _pathLength
    { Opcode::Mov,                   "mov",                  false,      false,              true,               false,              false,              false,               true,                  true,           false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::MovCrossFunction,      "movcrossfunction",     true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::Clear,                 "clear",                false,      false,              true,               false,              false,              false,               false,                 true,           false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::UnaryOp,               nullptr,                false,      false,              true,               false,              false,              false,               true,                  true,           false,                  false,         false,               false,                   false,             true,                true,          false,             false,              1},
    { Opcode::BinaryOp,              nullptr,                false,      false,              true,               false,              false,              false,               true,                  true,           false,                  false,         false,               false,                   false,             true,                true,          false,             false,              -1},
    { Opcode::BeginAtomic,           "beginatomic",          true,       false,              true,               false,              false,              false,               false,                 false,          true,                   false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::EndAtomic,             "endatomic",            true,       false,              true,               false,              false,              false,               false,                 false,          true,                   false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::Assert,                "assert",               true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                false,         false,             false,              0},
    { Opcode::Enqueue,               "enqueue",              true,       true,               false,              false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   true,              true,                false,         false,             false,              1},
    { Opcode::Select,                "select",               false,      false,              true,               false,              false,              false,               true,                  true,           false,                  false,         false,               false,                   false,             true,                true,          false,             false,              -1},
    { Opcode::WriteGlobal,           "writeglobal",          true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                false,         false,             false,              1},
    { Opcode::Print,                 "print",                true,       false,              true,               false,              false,              false,               false,                 false,          false,                  true,          true,                false,                   false,             true,                false,         false,             false,              0},
    { Opcode::Gather,                "gather",               false,      false,              true,               false,              false,              false,               true,                  true,           false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::StoreMemory,           "storememory",          true,       false,              true,               true,               false,              true,                false,                 false,          false,                  false,         false,               true,                    false,             true,                false,         false,             false,              1},
    { Opcode::LoadMemory,            "loadmemory",           false,      false,              true,               true,               true,               true,                false,                 false,          false,                  false,         false,               true,                    false,             true,                false,         false,             false,              1},
    { Opcode::BypassMemory,          "bypassmemory",         false,      false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                false,         false,             false,              1},
    { Opcode::AcquireSemaphore,      "acquiresemaphore",     true,       false,              false,              false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   true,              true,                true,          false,             false,              1},
    { Opcode::ReleaseSemaphore,      "releasesemaphore",     true,       false,              false,              false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              1},
    { Opcode::CallAtomic,            "callatomic",           false,      false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              1},
    { Opcode::AtomicReturn,          "returnatomic",         true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              1},
    { Opcode::FanOut,                "fanout",               false,      false,              false,              false,              true,               false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                false,         false,             false,              1},
    { Opcode::Stage,                 "stage",                true,       false,              false,              false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                false,         false,             false,              1},
    { Opcode::StartCondition,        "startcondition",       true,       false,              false,              false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                false,         false,             false,              1},
    { Opcode::CycleCounter,          "cyclecounter",         false,      false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                false,         false,             false,              0},
    { Opcode::MovLatencyPlaceholder, "movlatency",           false,      false,              true,               true,               true,               false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::LineNumber,            "linenumber",           true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::Lut,                   "lut",                  false,      false,              true,               false,              false,              false,               true,                  true,           false,                  false,         false,               false,                   false,             true,                true,          false,             false,              -1},
    { Opcode::InlineExternalModule,  "inlineexternalmodule", false,      false,              true,               false,              false,              true,                false,                 false,          false,                  false,         false,               true,                    false,             false,               false,         false,             false,              1},
    { Opcode::ReadSelectedFifo,      "readselectedfifo",     false,      false,              false,              false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              1},
    { Opcode::ExternalPlaceholder,   "externalplaceholder",  true,       false,              false,              false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                false,         false,             false,              1},
    { Opcode::DebugView,             "debugview",            true,       false,              true,               false,              false,              false,               false,                 false,          false,                  true,          true,                false,                   false,             true,                true,          false,             false,              0},
    { Opcode::EnqueueRegisters,      "enqueueregisters",     true,       false,              false,              false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   true,              true,                false,         false,             false,              0},
    { Opcode::DequeueRegisters,      "dequeueregisters",     false,      false,              false,              false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                false,         false,             false,              0},
    { Opcode::PushPredicate,         "pushpredicate",        true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::PopPredicate,          "poppredicate",         true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::StallCheck,            "stallcheck",           false,      false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::LocationRecord,        "locationrecord",       false,      false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::ConditionalIgnore,     "conditionalignore",    false,      false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0},
    { Opcode::FormatString,          "formatstring",         true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             true,               0},
    { Opcode::FormatEnum,            "formatenum",           true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             true,               0},
    { Opcode::ReferenceString,       "referencestring",      true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             true,               0},
    { Opcode::AssertStringEqual,     "assertstringeq",       true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          true,              false,              0},
    { Opcode::StringCount,           "stringcount",          false,      false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          true,              false,              0},
    { Opcode::NoOp,                  "noop",                 true,       false,              true,               false,              false,              false,               false,                 false,          false,                  false,         false,               false,                   false,             true,                true,          false,             false,              0}
};
// clang-format on

const OpcodeTableEntry& GetOpcodeTableEntry(const Opcode opcode)
{
    const size_t index = static_cast<size_t>(opcode);

    assert(index < ARRAY_SIZE(g_opcodeTable));

    const OpcodeTableEntry& result = g_opcodeTable[index];

    assert(result._opcode == opcode);

    return result;
}

bool FifoTypeCanBackpressure(const FifoType ft)
{
    bool result = true;

    switch (ft)
    {
    case FifoType::Default:
    case FifoType::ContextSaverCaller:
    case FifoType::ReorderBuffer:
    case FifoType::Passthrough:
        result = true;
        break;

    case FifoType::PassthroughRegistered:
    case FifoType::PassthroughUnregistered:
    case FifoType::FixedDelay:
        result = false;
        break;

    default:
        assert(false);
    }

    return result;
}

// Estimate the number of levels of logic required for a mux
// Assume a 4:1 mux can be implemented in 1 level of logic
size_t GetMuxPathLength(const size_t numChoices)
{
    size_t result = 1;
    size_t width = 4;

    while (true)
    {
        if (width >= numChoices)
        {
            return result;
        }
        else
        {
            result++;
            width *= 4;
        }
    }
}

size_t GetOpPathLength(const Program& program, const Operation& op)
{
    return GetOpPathLength(program, op, GetCodeGenConfig()._carryChainWidthPerLogicLevel);
}

size_t GetOpPathLength(const Program& program, const Operation& op, const size_t carryChainWidthPerLogicLevel)
{
    size_t result = 1;

    // Special case for LoadMemory operation that is referencing a memory that has been converted to logic
    if (Opcode::LoadMemory == op._opcode && 0 == op._flags._loadMemory._readLatency)
    {
        const size_t memoryIndex = op._src[0].GetAccessedRegister()._registerIndex;
        assert(RegisterType::Memory == program._registerTable[memoryIndex]._type);

        const RegisterDescription& regDesc = program._registerTable[memoryIndex];

        const auto depth = regDesc.Memory()._elementCount;

        return GetMuxPathLength(depth);
    }

    const int pathLength = GetOpcodeTableEntry(op._opcode)._pathLength;

    const auto getAddSubPathLength = [&](const Operation& op) -> size_t
    {
        // Add/Sub are implemented with a carry chain
        // The width of the destination determines
        assert(1 == op._dst.size());

        return (op._dst[0].Width(program) + carryChainWidthPerLogicLevel - 1) / carryChainWidthPerLogicLevel;
    };

    const auto getShiftPathLength = [&](const Operation& op)
    {
        size_t result = 0;

        assert(2 == op._src.size());
        if (SourceOperandType::Literal == op._src[1].Type())
        {
            // Shift by a constant requires no logic
            result = 0;
        }
        else
        {
            // Model a shift as a daisy chain of 4:1 muxes (1 per bit in the shift amount)
            // If the shift amount is wide, then assume that the diasy chain will be cut off early
            // and a comparison will handle wide values that produce a constant output
            const size_t shiftAmountWidth = std::min(op._src[1].Width(program), Log2RoundUp(op._src[0].Width(program)));

            const size_t fourToOneMuxPathLength = GetMuxPathLength(4);

            result = fourToOneMuxPathLength * (Align(shiftAmountWidth, 2) / 2);
        }

        return result;
    };

    if (pathLength >= 0)
    {
        // Most of the time, the opcode itself is sufficient to determine the path length of an operation
        result = static_cast<size_t>(pathLength);
    }
    else if (Opcode::Select == op._opcode)
    {
        // Multiplexer path length depends on how many inputs there are
        const size_t numChoices = op._src.size() - 1;

        result = GetMuxPathLength(numChoices);
    }
    else if (Opcode::Lut == op._opcode)
    {
        // Path length depends on the contents of the lookup tables
        // Iterate over all lookup tables, taking the maximum path length
        result = 0;

        const size_t numDstBits = op._flags._lut._numDestinationBits;

        for (size_t i = 0; i < numDstBits; i++)
        {
            size_t thisLutPathLength = 1;

            const LutEntry& lutEntry = op._flags._lut._lutEntries[i];

            if (0 == lutEntry._numSources)
            {
                // Contant value, no luts are needed to implement it
                thisLutPathLength = 0;
            }
            else if (lutEntry.IsPassthrough())
            {
                // Pass input value through to output, no luts are needed to implement
                thisLutPathLength = 0;
            }
            else
            {
                // Otherwise, 1 LUT is needed to implement this operation
                thisLutPathLength = 1;
            }

            result = std::max<size_t>(result, thisLutPathLength);
        }
    }
    else
    {
        assert(Opcode::BinaryOp == op._opcode);

        switch (op._flags._binaryOpType)
        {
        case ParseTreeBinaryOpTypeAdd:
        case ParseTreeBinaryOpTypeSub:
            result = getAddSubPathLength(op);
            break;

        case ParseTreeBinaryOpTypeShl:
        case ParseTreeBinaryOpTypeShr:
            result = getShiftPathLength(op);
            break;

        case ParseTreeBinaryOpTypeLutMul:
        {
            // 3*3 can be done in 1 level of logic
            // assume 6*6 can be done in 2 levels, etc
            const size_t numSrcBits = op._src[0].Width(program) + op._src[1].Width(program);
            result = (numSrcBits + 5) / 6;
        }
        break;

        case ParseTreeBinaryOpTypeAnd:
        case ParseTreeBinaryOpTypeLogicalAnd:
        case ParseTreeBinaryOpTypeOr:
        case ParseTreeBinaryOpTypeLogicalOr:
        case ParseTreeBinaryOpTypeXor:
        case ParseTreeBinaryOpTypeLogicalXor:
            result = 1;
            break;

        case ParseTreeBinaryOpTypeEQ:
        case ParseTreeBinaryOpTypeNE:
        case ParseTreeBinaryOpTypeGT:
        case ParseTreeBinaryOpTypeGE:
        case ParseTreeBinaryOpTypeLT:
        case ParseTreeBinaryOpTypeLE:
            // These don't matter because optimizations replace them with other optimizations
            result = 1;
            break;

        default:
            assert(false);
        }
    }

    return result;
}

// Returns true if an opcode indicates that an operation should not be eliminated by dead code elimination
// {Begin,End}Atomic have no operands, but they do mark atomic ranges
// Assert has a side effect (checking the condition and notifying the user of pass/fail)
// Enqueue has a side effect (creating threads)
// MovCrossFunction ensures that function arguments and return values are not optimized out
// StoreMemory operaitons are live, because other threads could read from the memory
// StoreOffChip operations are live, because the results can be read by other threads/functions
bool IsLiveOperation(const Operation& op)
{
    if (Opcode::InlineExternalModule == op._opcode)
    {
        // Externally-instantiated externs are always considered to be live
        return op._flags._callInlineExternalModule._isLive;
    }
    else
    {
        return GetOpcodeTableEntry(op._opcode)._isLive;
    }
}

// Returns true for operations can be merged into subsequent atomic do operations
// Not allowed for cases like Opcode::CycleCounter or calls to external verilog
// where converting from 1 invocation to many can change behavior
bool AllowMergeSubsequentAtomicDo(const Opcode opcode) { return GetOpcodeTableEntry(opcode)._mergeAtomicDo; }

// Returns true for operations that should not have input register types changed during optimization
bool ShouldPreserveInputRegisters(const Opcode opcode) { return GetOpcodeTableEntry(opcode)._preserveInputReg; }

// Enqueue is not allowed in an atomic block, because control-flow is not allowed in atomics
//   because the basic block that is called will not finish in the same cycle
// DSP operations are not allowed because the DSP has internal pipeline registers
// assertions are not allowed in atomics because of a problem in the verilog generated code
//  an assertion in the middle of atomic stage fires when it should not
// Memory load/store are not allowed because memory operations happen on the next clock cycle (on-chip and off-chip)
bool OpcodeAllowedInAtomicBlock(const Opcode opcode) { return GetOpcodeTableEntry(opcode)._allowedInAtomic; }

// Returns true if an operation has input registers which was outside of the generated pipelines
bool OpcodeHasExternalInputRegisters(const Opcode opcode)
{
    return GetOpcodeTableEntry(opcode)._externalInputRegisters;
}

// Returns true if an opcode requires that it immediately follow a pipeline register
bool OperationHasRegisteredInput(const Operation& op)
{
    if (Opcode::InlineExternalModule == op._opcode)
    {
        if (op._flags._callInlineExternalModule._latency >= 1)
        {
            return true;
        }
        else
        {
            return false;
        }
    }
    else if (Opcode::LoadMemory == op._opcode && 0 == op._flags._loadMemory._readLatency)
    {
        return false;
    }
    else
    {
        return GetOpcodeTableEntry(op._opcode)._registeredInput;
    }
}

// Returns a delay (measured in levels of logic)
// That instruction scheduling should use to account for routing data to memory/dsp output registers
size_t GetInputRoutingDelay(const Operation& op)
{
    return (GetOpcodeTableEntry(op._opcode)._hasIoRoutingDelay && OperationHasRegisteredInput(op))
               ? GetCodeGenConfig()._inputOutputRegisterDelay
               : 0;
}

// Returns a delay (measured in levels of logic)
// That instruction scheduling should use to account for routing data from memory/dsp output registers
size_t GetOutputRoutingDelay(const Opcode opcode)
{
    return GetOpcodeTableEntry(opcode)._hasIoRoutingDelay ? GetCodeGenConfig()._inputOutputRegisterDelay : 0;
}

bool OpcodeUsesHardenedRegisters(const Opcode opcode) { return GetOpcodeTableEntry(opcode)._hasIoRoutingDelay; }

// Returns true if an opcode requires that it immediately precede a pipeline register
bool OperationHasRegisteredOutput(const Operation& op)
{
    // Load memory output register can be optionally enabled or disabled
    if (Opcode::LoadMemory == op._opcode)
    {
        if (2 == op._flags._loadMemory._readLatency)
        {
            return true;
        }
        else
        {
            assert(op._flags._loadMemory._readLatency < 2);
            return false;
        }
    }
    else if (Opcode::InlineExternalModule == op._opcode)
    {
        if (op._flags._callInlineExternalModule._latency >= 2)
        {
            return true;
        }
        else
        {
            return false;
        }
    }
    else
    {
        return GetOpcodeTableEntry(op._opcode)._registeredOutput;
    }
}

// Barriers and atomics prevent optimizations (like PackLuts) from occuring across them
bool DoesOpcodeImplyOptimizationBarrier(const Opcode opcode)
{
    return GetOpcodeTableEntry(opcode)._optimizationBarrier;
}

// __print() and __debug_view() should not be reordered
bool DoesOpcodeRequireTypeBarrier(const Opcode opcode)
{
    const bool result = GetOpcodeTableEntry(opcode)._typeBarrier;

    // While these operations cannot be reordered, many should still be able to fit into 1 pipeline stage
    // (to avoid many print statements causing extra pipeline stages)
    assert(!result || (0 == GetOpcodeTableEntry(opcode)._pathLength));

    return result;
}

bool DoesOpcodeRequireAssertBarrier(const Opcode opcode) { return GetOpcodeTableEntry(opcode)._assertBarrier; }

// Used to limit testing for common subexpression elimination
// Only some opcodes are supported
bool DoesOpcodeSupportCommonSubexpressionElimination(const Opcode opcode)
{
    return GetOpcodeTableEntry(opcode)._commonSubexpression;
}

bool CanOpcodeBeLoweredToLut(const Opcode opcode) { return GetOpcodeTableEntry(opcode)._lowerToLut; }

bool CanOperationBackpressure(const Operation& op)
{
    bool result = GetOpcodeTableEntry(op._opcode)._canCauseBackpressure;

    if (result)
    {
        if (op._getSuccessorBlock)
        {
            const Function* function = op._getSuccessorBlock()->_function;

            if (function->IsNoBackpressure())
            {
                // Calling a [[no_backpressure]] function does not introduce backpressure
                result = false;
            }
        }
    }

    return result;
}

bool OpcodeCanUseBitBucketOutput(const Opcode opcode) { return GetOpcodeTableEntry(opcode)._allowBitbucket; }
bool OpcodeWritesToStringTable(const Opcode opcode) { return GetOpcodeTableEntry(opcode)._writesStringTable; }

bool OperationReadsFromStringTable(const Operation& op)
{
    if (op._opcode == Opcode::Print)
    {
        // Print operations may or may not read from the string table
        // Check to see this operation references a string handle
        for (const PrintEntry& pe : *(op._flags._print._entries))
        {
            if (pe._type == PrintType::StringHandle)
            {
                return true;
            }
        }

        return false;
    }
    else
    {
        return GetOpcodeTableEntry(op._opcode)._readsStringTable;
    }
}

const char* GetUnaryOpTypeString(const ParseTreeUnaryOpType op)
{
    switch (op)
    {
    case ParseTreeUnaryOpTypeInvert:
        return "invert";
    case ParseTreeUnaryOpTypeNegate:
        return "negate";
    default:
        assert(false);
        return nullptr;
    }
}

const char* GetBinaryOpTypeString(const ParseTreeBinaryOpType op)
{
    switch (op)
    {
    case ParseTreeBinaryOpTypeAdd:
        return "add";
    case ParseTreeBinaryOpTypeSub:
        return "sub";
    case ParseTreeBinaryOpTypeMul:
        return "mul";
    case ParseTreeBinaryOpTypeLutMul:
        return "lutmul";
    case ParseTreeBinaryOpTypeDiv:
        return "div";
    case ParseTreeBinaryOpTypeMod:
        return "mod";
    case ParseTreeBinaryOpTypeLogicalAnd:
    case ParseTreeBinaryOpTypeAnd:
        return "and";
    case ParseTreeBinaryOpTypeLogicalOr:
    case ParseTreeBinaryOpTypeOr:
        return "or";
    case ParseTreeBinaryOpTypeLogicalXor:
    case ParseTreeBinaryOpTypeXor:
        return "xor";
    case ParseTreeBinaryOpTypeShl:
        return "shl";
    case ParseTreeBinaryOpTypeShr:
        return "shr";
    case ParseTreeBinaryOpTypeEQ:
        return "eq";
    case ParseTreeBinaryOpTypeNE:
        return "ne";
    case ParseTreeBinaryOpTypeGT:
        return "gt";
    case ParseTreeBinaryOpTypeGE:
        return "ge";
    case ParseTreeBinaryOpTypeLT:
        return "lt";
    case ParseTreeBinaryOpTypeLE:
        return "le";
    default:
        assert(false);
        return nullptr;
    }
}

const char* GetAtomicFunctionName(const Program& program, const size_t index)
{
    return program.GetInlineAtomicFunction(index)._name.c_str();
}

const char* GetOpcodeString(const Program& program, const Operation& op)
{
    switch (op._opcode)
    {
    case Opcode::UnaryOp:
        return GetUnaryOpTypeString(op._flags._unaryOpType);
    case Opcode::BinaryOp:
        return GetBinaryOpTypeString(op._flags._binaryOpType);
    case Opcode::CallAtomic:
        return GetAtomicFunctionName(program, op._flags._callAtomic._functionIndex);
    default:
        return GetOpcodeTableEntry(op._opcode)._name;
    }
}

std::ostream& operator<<(std::ostream& str, const AccessedRegister& reg)
{
    str << "r" << reg._registerIndex;

    return str;
}

std::ostream& operator<<(std::ostream& str, const FifoSubset& fifo)
{
    str << "fifo" << fifo._registerIndex << "[" << fifo._offset + fifo._width - 1 << "," << fifo._offset << "]";

    return str;
}

std::ostream& operator<<(std::ostream& str, const Literal& lit)
{
    str << lit._value << "[" << lit._width << "]";

    return str;
}

std::ostream& operator<<(std::ostream& str, const SourceOperand& op)
{
    switch (op.Type())
    {
    case SourceOperandType::Literal:
        str << op.GetLiteral();
        break;

    case SourceOperandType::Register:
        str << op.GetAccessedRegister();
        break;

    case SourceOperandType::Fifo:
        str << op.GetFifoSubset();
        break;

    case SourceOperandType::StringLiteral:
        str << "\"" << op.GetStringLiteral() << "\"";
        break;
    }

    return str;
}

std::ostream& operator<<(std::ostream& str, const FileAndLineNumber& loc)
{
    const std::string fullFileName = GetCodeGenConfig()._fileNames[loc._fileIndex];

    // Strip the path from the file name
    const std::string fileNameWithoutPath = StripLeadingPath(fullFileName);

    str << "// _debug source line " << loc._lineNumber << " \"" << fileNameWithoutPath << "\"";

    return str;
}

std::ostream& SerializeOperation(std::ostream& str, const Operation& op, const Program& program)
{
    str << GetOpcodeString(program, op);

    for (const DestinationOperand& destOp : op._dst)
    {
        switch (destOp.Type())
        {
        case DestinationOperandType::Register:
            str << " " << destOp.GetAccessedRegister();
            break;

        case DestinationOperandType::Fifo:
            str << " " << destOp.GetFifoSubset();
            break;

        default:
            assert(false);
        }

        str << " (w:" << destOp.Width(program) << ")";

        if (DestinationOperandType::Register == destOp.Type())
        {
            const std::string& name = program._registerTable[destOp.GetAccessedRegister()._registerIndex]._name;

            str << "(" << name << ")";
        }
    }

    for (size_t i = 0; i < op._src.size(); ++i)
    {
        const uint64_t mask = 1ull << i;

        const bool isSigned = (mask == (op._signExtendSourceMask & mask));

        const SourceOperand& srcOp = op._src[i];

        str << " ";

        if (isSigned)
        {
            str << "Signed(";
        }

        str << srcOp;

        if (isSigned)
        {
            str << ")";
        }

        str << " (w:" << srcOp.Width(program) << ")";

        if (SourceOperandType::Register == srcOp.Type())
        {
            const std::string& name = program._registerTable[srcOp.GetAccessedRegister()._registerIndex]._name;

            str << "(" << name << ")";
        }
    }

    if (Opcode::Enqueue == op._opcode || Opcode::EnqueueRegisters == op._opcode ||
        Opcode::DequeueRegisters == op._opcode)
    {
        str << " fifo: " << op._flags._enqueue._successorFifo;
    }

    if (op._getSuccessorBlock)
    {
        BasicBlock* successor = op._getSuccessorBlock();
        str << " successor: Basic Block " << std::hex << successor << std::dec;
    }

    if (Opcode::BeginAtomic == op._opcode)
    {
        str << " rate: " << op._flags._atomicBlockDesc._updateRate << " ";
    }

    if (Opcode::Lut == op._opcode)
    {
        const Lut& lut = op._flags._lut;

        str << " lut {";

        CommaSeparatedOutputHelper csv;

        for (size_t i = 0; i < lut._numDestinationBits; i++)
        {
            const LutEntry& le = lut._lutEntries[i];

            // Only interpret lut entries that map to a constant output bit
            if (le._numSources == 0)
            {
                csv.Append(std::to_string(le._table));
            }
            else
            {
                csv.Append("?");
            }
        }

        str << csv.Str() << "}";
    }

    if (!op._reverseRenamingTable.empty())
    {
        str << " renaming table: ";

        for (const auto& p : op._reverseRenamingTable)
        {
            str << "r" << p.first << "->r" << p.second << " ";
        }
    }

    for (const FileAndLineNumber& f : op._locations)
    {
        str << f << " ";
    }

    return str;
}

void SerializeBasicBlock(std::ostream& str, const BasicBlock& basicBlock, const Program& program)
{
    str << "\nBasic Block " << std::hex << &basicBlock << std::dec << "\n";

    str << LocationToString(basicBlock._location, true, ":") << "\n";

    if (!basicBlock._startConditionOperations.empty())
    {
        str << "Start condition operations:\n";

        for (const Operation& op : basicBlock._startConditionOperations)
        {
            SerializeOperation(str, op, program) << "\n";
        }
    }

    if (!basicBlock._operations.empty())
    {
        str << "Unscheduled Operations:\n";

        for (const Operation& op : basicBlock._operations)
        {
            SerializeOperation(str, op, program) << "\n";
        }
    }

    for (const Stage& stage : basicBlock._stages)
    {
        str << "Stage: " << stage._atomicSequence << "\n";

        for (const FileAndLineNumber& loc : stage._fileAndLineNumbers)
        {
            str << loc << "\n";
        }

        for (const Operation& op : stage._operations)
        {
            SerializeOperation(str, op, program) << "\n";
        }

        if (!stage._clockGates.empty())
        {
            str << "Clock gates:\n";

            for (const auto& p : stage._clockGates)
            {
                str << p.first << " gated by set: ";

                for (const size_t r : p.second)
                {
                    str << r << " ";
                }

                str << "\n";
            }
        }
    }
}

void SerializeProgram(std::ostream& str, const Program& program)
{
    for (const Function& function : program._functions)
    {
        SerializeFunction(str, function, program);
    }
}

void SerializeFunction(std::ostream& str, const Function& function, const Program& program)
{
    std::cout << "Function: " << function._name << "\n";
    for (const BasicBlock& basicBlock : function._basicBlocks)
    {
        SerializeBasicBlock(str, basicBlock, program);
    }
}

std::ostream& operator<<(std::ostream& str, const Program& program)
{
    for (const Function& function : program._functions)
    {
        SerializeFunction(str, function, program);
    }

    return str;
}

// Before optimizations run, cleans up the IR
// Transforms logical AND, OR, and XOR into bitwise versions
void RemoveFrontEndConceptsBeforeOptimize(IRContext& context)
{
    for (Function& function : context._program->_functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            for (size_t i = 0; i < 2; i++)
            {
                OperationList& operationList = i == 0 ? basicBlock._startConditionOperations : basicBlock._operations;
                for (Operation& op : operationList)
                {
                    if (Opcode::BinaryOp == op._opcode)
                    {
                        switch (op._flags._binaryOpType)
                        {
                        case ParseTreeBinaryOpTypeLogicalAnd:
                            op._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;
                            break;

                        case ParseTreeBinaryOpTypeLogicalOr:
                            op._flags._binaryOpType = ParseTreeBinaryOpTypeOr;
                            break;

                        case ParseTreeBinaryOpTypeLogicalXor:
                            op._flags._binaryOpType = ParseTreeBinaryOpTypeXor;
                            break;

                        default:
                            break;
                        }
                    }
                }
            }
        }
    }
}

FifoSubset ContextSaver::InvocationInstanceInCalleeFifo() const
{
    // External calls do not track the invocation instance
    assert(!_isExternal);

    const AccessedRegister localReg = _invocationInstanceLocalRegister->GetAccessedRegister();

    const auto it = _calleeInputMap.find(localReg._registerIndex);
    assert(it != _calleeInputMap.end());

    const FifoSubset result = {_destinationFifo, it->second, GetCodeGenConfig().GetInvocationIndexSize()};

    return result;
}

size_t ContextSaver::GetCalleeOutputWidth(const Program& program) const
{
    size_t result = 0;

    if (_isOrdered)
    {
        // For pipelined calls, this is the size of the array
        result = _callerReturnValueRegisters ? _callerReturnValueRegisters->GetType()->GetBitWidth() : 0;
    }
    else
    {
        const RegisterDescription& calleeRegDesc = program._registerTable[_fromCalleeFifoIndex];

        // No need to worry about the array, but the invocation instance may be there
        result = calleeRegDesc._width;
    }

    return result;
}

size_t SourceOperand::Width(const Program& program) const
{
    size_t result = 0;

    switch (_type)
    {
    case SourceOperandType::Literal:
        result = _literal._width;
        break;

    case SourceOperandType::Register:
        result = program._registerTable[_reg._registerIndex]._width;
        break;

    case SourceOperandType::Fifo:
        result = _fifo._width;
        break;

    case SourceOperandType::StringLiteral:
        result = 0;
        break;

    default:
        assert(false);
    }

    return result;
}

size_t DestinationOperand::Width(const Program& program) const
{
    size_t result = 0;

    switch (_type)
    {
    case DestinationOperandType::Register:
        result = program._registerTable[_reg._registerIndex]._width;
        break;

    case DestinationOperandType::Fifo:
        result = _fifo._width;
        break;

    default:
        assert(false);
    }

    return result;
}

size_t RegisterDescription::GetPow2RoundedElementCount() const { return 1ULL << Log2RoundUp(Memory()._elementCount); }

size_t RegisterDescription::GetMemoryAddressWidth() const
{
    assert(RegisterType::Memory == _type);

    const size_t result = Log2RoundUp(Memory()._elementCount);

    // Support memories of depth 1 by forcing address with = 1
    return (result == 0) ? 1 : result;
}

// Ignore all-LineNums cases
bool OperationListIsEmpty(const OperationList& operations)
{
    for (const auto& op : operations)
    {
        if (op._opcode != Opcode::LineNumber)
            return false;
    }
    return true;
}

bool BasicBlock::IsFirstInFunction() const { return this == _function->_start; }

bool BasicBlock::IsEmpty() const
{
    return (OperationListIsEmpty(_operations) && _stages.empty() && OperationListIsEmpty(_startConditionOperations));
}

AccessedRegister BasicBlock::GetStartConditionRegister() const
{
    for (const Stage& stage : _stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._opcode == Opcode::StartCondition)
            {
                return op._src[0].GetAccessedRegister();
            }
        }
    }

    assert(false);
    throw std::runtime_error("Failed to find start condition register");
}

// Returns true if this is the entry basic block
// to a function that is called automatically on reset
bool BasicBlock::IsResetBlock() const { return _function->CallOnReset() && IsFirstInFunction(); }

// Returns the rate at which threads can execute within a basic block
// A value of 1 means all pipeline stages can be occupied at the same time
// A value of 2 means that there must be a gap of 1 pipeline stage between threads
size_t BasicBlock::GetThreadRate() const { return _function->_functionThreadRate; }

// Returns the maximum thread rate to use for fifo sizing
// When targeting Stratix10NX devices, a thread rate of 1 is used
// for fifo sizing, to work-around a Quartus bug:
// Min-cost flow problem is unbounded.  There is an arc with negative cost and infinite capacity.
size_t BasicBlock::GetThreadRateForFifoSizing() const
{
    return GetCodeGenDeviceConfig()._useThreadRateInFifoSizing ? GetThreadRate() : 1;
}

ObjectPath BasicBlock::GetObjectPath() const
{
    const ObjectPath objectPath = g_compiler->ObjectNameToPath(_function->_objectName);

    return AppendToPath(AppendToPath(objectPath, _function->_name), std::to_string(_indexWithinFunction));
}

// Validate usage of externs
void CheckExterns(IRContext& context)
{
    std::set<std::string> externFixedLatencyNames;

    // Check for multiple call sites
    for (const ExternalModuleCall& externalModuleCall : context._program->_externalModuleCalls)
    {
        if (externalModuleCall._type != ExternalModuleCallType::InstantiateInBasicBlock)
        {
            const std::string name = externalModuleCall._type == ExternalModuleCallType::ExternClassMethod
                                         ? externalModuleCall.GetFullyQualifiedName()
                                         : externalModuleCall._name;

            const auto it = externFixedLatencyNames.find(name);

            if (it != externFixedLatencyNames.end())
            {
                // fixed-latency modules instantiated outside of Kanagawa cannot have multiple call sites
                // Because there is no arbitration
                std::ostringstream str;
                str << "Externally instantiated fixed-latency function has multiple call sites: " << name;

                throw std::runtime_error(str.str());
            }
            else
            {
                externFixedLatencyNames.insert(name);
            }
        }
    }
}

std::string FunctionNameStackToString(std::stack<std::string> functionNameStack)
{
    std::string s;
    if (functionNameStack.empty())
    {
        return "";
    }
    else
    {
        s = FixupString(functionNameStack.top());
        functionNameStack.pop();
    }

    while (!functionNameStack.empty())
    {
        s = FixupString(functionNameStack.top()) + "/" + s;
        functionNameStack.pop();
    }
    return s;
}

size_t InsertStallLogic(IRContext& context)
{
    auto program = context._program;

    size_t stallerIndex = 0;

    for (Function& function : program->_functions)
    {
        if (function.IsExtern())
        {
            continue;
        }

        // Can't insert stalls into no-backpressure functions
        if (function._functionNode->NoBackpressure())
        {
            continue;
        }

        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            OperationList stallOps;

            {
                SetOperationLocation sol(stallOps, basicBlock._location,
                                         program->GetCallstackIndexForFunction(function, basicBlock._location));

                // Compare with threshold
                AccessedRegister cmpReg;
                {
                    Operation op = {};
                    op._opcode = Opcode::StallCheck;
                    // By default, reorder global writes after the stall check
                    // and predicate with the stall condition.
                    op._flags._stallCheck._globalsAfter = true;
                    const mp_int this_index(stallerIndex++);
                    op._src.push_back(this_index);
                    cmpReg = {AllocateRegister(program, 1, RegisterType::Local, "stall_compare")};
                    op._dst.push_back(cmpReg);
                    stallOps.push_back(op);
                }

                bool startConditionGlobalsBefore = false;
                if (basicBlock.HasStartCondition())
                {
                    auto startConditionOp = basicBlock._startConditionOperations.back();
                    assert(startConditionOp._opcode == Opcode::StartCondition);
                    assert(startConditionOp._src.size() == 1);
                    basicBlock._startConditionOperations.pop_back();

                    // If global writes are being reordered after the start
                    // condition, then reordering and predicating after the
                    // stall check is not needed.
                    startConditionGlobalsBefore = startConditionOp._flags._startCondition._globalsBefore;
                    if (!startConditionGlobalsBefore)
                    {
                        Operation& stallCheckOp = stallOps.back();
                        assert(stallCheckOp._opcode == Opcode::StallCheck);
                        stallCheckOp._flags._stallCheck._globalsAfter = false;
                    }

                    // And with existing start condition
                    Operation op = {};
                    op._opcode = Opcode::BinaryOp;
                    op._flags._binaryOpType = ParseTreeBinaryOpTypeLogicalAnd;
                    op._src.push_back(cmpReg);
                    op._src.push_back(startConditionOp._src.front());
                    cmpReg = {AllocateRegister(program, 1, RegisterType::Local, "start_condition")};
                    op._dst.push_back(cmpReg);
                    stallOps.push_back(op);
                }

                // Use as start condition
                {
                    Operation op = {};
                    op._opcode = Opcode::StartCondition;
                    op._flags._startCondition._globalsBefore = startConditionGlobalsBefore;
                    op._src.push_back(cmpReg);
                    stallOps.push_back(op);
                }
            }

            basicBlock._startConditionOperations.splice(basicBlock._startConditionOperations.end(), stallOps);
        }
    }

    return stallerIndex;
}

// Asserts that each operation with a non-0 cost
// is associated with a source file/line
void AssertOperationsHaveLocations(const Program& program, const OperationList& ops)
{
    if (g_compiler->HadCompileError())
    {
        // Locations are not inserted by ~SetOperationLocation
        // if the source code has an error
        return;
    }

#ifndef KANAGAWA_SKIP_CONSISTENCY_CHECKS
    for (const Operation& op : ops)
    {
        if (op._expectNoSourceLocation)
        {
            continue;
        }

        if (op._locations.empty())
        {
            std::cout << "Found an operation without an associated location\n";
            SerializeOperation(std::cout, op, program);
            std::cout << "\n";
        }
        assert(!op._locations.empty());
    }
#endif // KANAGAWA_SKIP_CONSISTENCY_CHECKS
}

// AssertOperationsHaveLocations for all unscheduled operations in a basic block
void AssertUnscheduledOperationsHaveLocations(const Program& program, const BasicBlock& bb)
{
    AssertOperationsHaveLocations(program, bb._startConditionOperations);
    AssertOperationsHaveLocations(program, bb._operations);
}

// AssertOperationsHaveLocations for all scheduled operations in a basic block
void AssertScheduledOperationsHaveLocations(const Program& program, const BasicBlock& bb)
{
    for (const Stage& stage : bb._stages)
    {
        AssertOperationsHaveLocations(program, stage._operations);
    }
}

SetOperationLocation::SetOperationLocation(IRContext& context, const Location& location)
    : _context(&context), _startBlock(context._basicBlock), _ops(context._basicBlock->_operations),
      _beginSize(context._basicBlock->_operations.size())
{
    FileAndLineNumber faln = {};

    // Per-operation call stacks are only used by the Verilog backend
    if (g_compiler->IsCompilingToVerilog())
    {
        faln = LocationToFileAndLineNumber(location, context.GetCallStackIndex());
    }
    else
    {
        faln = LocationToFileAndLineNumber(location);
    }

    _locations.insert(faln);
}

SetOperationLocation::SetOperationLocation(OperationList& ops, const Location& location)
    : _context(nullptr), _startBlock(nullptr), _ops(ops), _beginSize(ops.size())
{
    _locations.insert(LocationToFileAndLineNumber(location));
}

SetOperationLocation::SetOperationLocation(OperationList& ops, const std::set<FileAndLineNumber>& locations)
    : _context(nullptr), _startBlock(nullptr), _ops(ops), _beginSize(ops.size())
{
    _locations = locations;
}

SetOperationLocation::SetOperationLocation(OperationList& ops, const Location& location, const size_t callStackIndex)
    : _context(nullptr), _startBlock(nullptr), _ops(ops), _beginSize(ops.size())
{
    _locations.insert(LocationToFileAndLineNumber(location, callStackIndex));
}

SetOperationLocation::SetOperationLocation(OperationList& ops, const Operation& srcOp)
    : _context(nullptr), _startBlock(nullptr), _ops(ops), _beginSize(ops.size()), _locations(srcOp._locations)
{
    assert(!_locations.empty());
}

SetOperationLocation::~SetOperationLocation()
{
    // If the source has a bug like calling a non-inline function
    // in an atomic, then the assumption about basic block not changing
    // won't not be true.
    if (g_compiler->HadCompileError())
    {
        return;
    }

    const auto updateOpLocations = [this](Operation& op)
    {
        // Only set a location if one is not already assigned for this operation
        // This is useful for nesting cases - where the inner most nest should win
        if (op._locations.empty())
        {
            op._locations = _locations;
        }
    };

    // Advance from the end of the list back to first operation that was inserted
    // after the constructor ran
    if (_ops.size() >= _beginSize)
    {
        const size_t amountToAdvance = _ops.size() - _beginSize;

        auto rit = _ops.rbegin();
        std::advance(rit, amountToAdvance);

        for (auto it = rit.base(); it != _ops.end(); ++it)
        {
            updateOpLocations(*it);
        }
    }

    if (_context && (_startBlock != _context->_basicBlock))
    {
        // New basic blocks have been added
        // Update locations in this
        bool beyondStartBlock = false;

        for (BasicBlock& bb : _context->_function->_basicBlocks)
        {
            if (beyondStartBlock)
            {
                for (Operation& op : bb._operations)
                {
                    updateOpLocations(op);
                }
            }

            if (&bb == _startBlock)
            {
                beyondStartBlock = true;
            }
        }

        // The start block should have been encountered
        assert(beyondStartBlock);
    }
}

void CopyLocationsToList(const Operation& srcOp, OperationList& ops)
{
    assert(!srcOp._locations.empty());

    for (Operation& op : ops)
    {
        assert(op._locations.empty());
        op._locations = srcOp._locations;
    }
}

size_t RegisterDescription::FifoDesc::RoundedDepth() const
{
    return GetCodeGenDeviceConfig()._fifoDepthPow2 ? 1ull << LogDepth() : _depth;
}

// RAII class for condition coverage
PushPopConditionCoverageTracker::PushPopConditionCoverageTracker(bool valid, IRContext& context,
                                                                 CodeCoverageType codeCoverageType)
    : _context(context), _topLevelConditionValid(false)
{
    if (GetCodeGenConfig()._codeCoverage && !_context._isInSimAssert && valid)
    {
        _valid = true;

        _context.NewConditionCoverageTracker(codeCoverageType);
    }
    else
    {
        _valid = false;
    }
}

PushPopConditionCoverageTracker::PushPopConditionCoverageTracker(bool valid, IRContext& context,
                                                                 CodeCoverageType codeCoverageType,
                                                                 const AccessedRegister& topLevelCondition,
                                                                 const Location& topLevelConditionLocation,
                                                                 const ParseTreeNode* const topLevelConditionNode)
    : _context(context), _topLevelConditionValid(true)
{
    if (GetCodeGenConfig()._codeCoverage && !_context._isInSimAssert && valid)
    {
        _valid = true;

        _topLevelCondition = &topLevelCondition;
        _topLevelConditionLocation = &topLevelConditionLocation;
        _topLevelConditionNode = topLevelConditionNode;

        _context.NewConditionCoverageTracker(codeCoverageType);
    }
    else
    {
        _valid = false;
    }
}

// Generate condition coverage on deconstruct
PushPopConditionCoverageTracker::~PushPopConditionCoverageTracker()
{
    if (_valid)
    {
        assert(GetCodeGenConfig()._codeCoverage);
        assert(!_context._isInSimAssert);

        ConditionCoverageTracker& tracker = _context.GetConditionCoverageTracker();

        assert(tracker._codeCoverageType == CodeCoverageType::Condition ||
               tracker._codeCoverageType == CodeCoverageType::Expression);
        // Check that any changes to these variables were undone
        assert(tracker._nonMaskingConditionStack.size() == 0);
        assert(!tracker._conditionCoverageFlags._inverting);

        // Assign saved top-level condition to tracker
        if (_topLevelConditionValid)
        {
            tracker._topLevelCondition = *_topLevelCondition;
            tracker._topLevelConditionLocation = *_topLevelConditionLocation;
            if (_topLevelConditionNode)
            {
                tracker._topLevelConditionString = _topLevelConditionNode->PrettyPrint();
            }
        }

        // If only one leaf condition, do not need condition coverage
        if (tracker._conditionCoverageMap.size() > 1)
        {
            assert((bool)tracker._topLevelCondition);
            const AccessedRegister& topLevelCondition = *tracker._topLevelCondition;
            const Location& condloc = tracker._topLevelConditionLocation;

            const std::string name = _context.GenerateCodeCoverageName(condloc, tracker._codeCoverageType);

            // Create condition coverage counters
            for (const auto& it : tracker._conditionCoverageMap)
            {
                CreateConditionCoverage(_context, it.first, it.second, topLevelCondition, condloc, name,
                                        tracker._codeCoverageType, tracker._topLevelConditionString);
            }
        }

        // Remove condition coverage tracker from stack
        _context.PopConditionCoverageTracker();
    }
}

OperationLocationRecord::OperationLocationRecord(BasicBlock& basicBlock, Program& program)
    : _basicBlock(basicBlock), _recordIndex(program._locationRecordCount++)
{
    // Insert an operation that can be serached for later
    Operation op = {};

    op._opcode = Opcode::LocationRecord;
    op._flags._locationRecordIndex = _recordIndex;

    _basicBlock._operations.push_back(op);
}

OperationLocationRecord::~OperationLocationRecord()
{
    // Remove the location record operation
    const ListAndIterator lai = GetLocationRecordListAndIterator();

    lai._list->erase(lai._iterator);
}

OperationLocationRecord::ListAndIterator OperationLocationRecord::GetInsertListAndIterator() const
{
    OperationLocationRecord::ListAndIterator result = GetLocationRecordListAndIterator();

    ++result._iterator;

    return result;
}

OperationLocationRecord::ListAndIterator OperationLocationRecord::GetLocationRecordListAndIterator() const
{
    const auto searchList = [this](OperationList& operations)
    {
        ListAndIterator result = {};

        // Search for the operation location record back to front
        // In typical uses cases, it will be near the end of the list
        for (OperationList::reverse_iterator it = operations.rbegin(); it != operations.rend(); ++it)
        {
            const Operation& op = *it;

            if ((op._opcode == Opcode::LocationRecord) && (op._flags._locationRecordIndex == _recordIndex))
            {
                ++it;

                result._list = &operations;
                result._iterator = it.base();

                break;
            }
        }

        return result;
    };

    OperationLocationRecord::ListAndIterator result = searchList(_basicBlock._operations);

    if (!result._list)
    {
        // Operation must have been moved into _startConditionOperations
        result = searchList(_basicBlock._startConditionOperations);
    }

    // Assert that the placeholder was found
    assert(result._list != nullptr);
    assert(result._iterator != result._list->end());
    assert(result._iterator->_opcode == Opcode::LocationRecord);
    assert(result._iterator->_flags._locationRecordIndex == _recordIndex);

    return result;
}

// Input: "a_b_c"
// Output: {"a", "b", "c"}
std::vector<std::string> TokenizeObjectName(const std::string& objectName, const Program& program)
{
    std::vector<std::string> result;

    const auto it = program._objectNameToContainers.find(objectName);

    if (it != program._objectNameToContainers.end())
    {
        const SourceContainer* container = it->second;

        for (; container != nullptr; container = container->_parent)
        {
            result.push_back(container->_instanceName);
        }

        // The loop above went from leaves to the root
        // so the order of elements must be reversed
        std::reverse(result.begin(), result.end());
    }

    if (result.empty())
    {
        // Search through _objectNameToContainers failed, just return the input string
        result.push_back(objectName);
    }

    return result;
}

bool LutEntry::IsConstantZero() const { return _table == 0; }

bool LutEntry::IsConstantOne() const
{
    bool result = false;

    if ((0 == _numSources) && (_table == 1))
    {
        result = true;
    }
    if ((1 == _numSources) && (_table == 3))
    {
        result = true;
    }

    return result;
}

bool LutEntry::IsInvert() const { return (1 == _numSources) && (_table == 1); }

bool LutEntry::IsPassthrough() const { return (1 == _numSources) && (_table == 2); }

std::string ExternalModuleCall::GetFullyQualifiedName() const
{
    assert(ExternalModuleCallType::ExternClassMethod == _type);
    return FixupString(_externClassInstanceName->_objectName + "_" + _name);
}

// Returns the number of rows in the truth table
size_t LutEntry::TableSize() const
{
    // Assert that overflow will not occur
    assert(_numSources <= c_maxLutSources);

    return 1ull << _numSources;
}

// Returns a mask indicating which bits in the lookup table are valid
uint64_t LutEntry::TableMask() const
{
    const size_t tableSize = TableSize();

    return (tableSize == 64) ? std::numeric_limits<uint64_t>::max() : (1ull << tableSize) - 1;
}

bool LutEntry::GetTableEntry(const size_t rowIndex) const
{
    assert(rowIndex < TableSize());

    return 1 == ((_table >> rowIndex) & 1);
}

mp_int Lut::Evaluate(const std::function<mp_int(const size_t srcOperandIndex)>& evalSrcOperand) const
{
    mp_int result = 0;

    for (size_t dstIndex = 0; dstIndex < _numDestinationBits; dstIndex++)
    {
        const LutEntry& le = _lutEntries[dstIndex];

        size_t tableRowIndex = 0;

        for (size_t tableColumnIndex = 0; tableColumnIndex < le._numSources; tableColumnIndex++)
        {
            const size_t srcOperandIndex = le._sourceIndices[tableColumnIndex];

            const mp_int operandValue = evalSrcOperand(srcOperandIndex);

            const size_t srcBit = le._sourceBit[tableColumnIndex];

            if (bit_test(operandValue, static_cast<uint32_t>(srcBit)))
            {
                tableRowIndex |= (1ull << tableColumnIndex);
            }
        }

        if (le.GetTableEntry(tableRowIndex))
        {
            bit_set(result, static_cast<uint32_t>(dstIndex));
        }
    }

    return result;
}

const Function* ExternalClassInstanceCallback::GetCalledFunction() const
{
    return _calleeNode->GetInstance(_calleeName).GetFunction();
}

void LinkExternalClassCallbacks(IRContext& context)
{
    Program& program = *context._program;

    for (ExternalClassInstance& externModule : program._externClassInstances)
    {
        for (const ExternalClassInstanceCallback& callback : externModule._callbacks)
        {
            Function* const calledFunction = callback._calleeNode->GetInstance(callback._calleeName).GetFunction();

            assert(!calledFunction->_externClassInstance || (calledFunction->_externClassInstance == &externModule));
            calledFunction->_externClassInstance = &externModule;
        }
    }
}

// Push a source operand which represents the predicate
// If no predicate is set, push a literal operand with value = 1
void PushPredicateOrLiteralOne(IRContext& context, Operation& op)
{
    if (context.IsPredicated())
    {
        const AccessedRegister predicateReg = {context.GetPredicate()};

        op._src.push_back(predicateReg);
    }
    else
    {
        // Literal 1 predicate value
        op._src.push_back(SourceOperand(1, 1));
    }
}

// Verify that all local registers ready by a function are either written by that function before being
// read, or will be written by the caller.
// Called before scheduling.
void ValidateLocalsAreWritten(const Program& program, Function& function)
{
    RegisterSetMap liveInMap;

    // Build a list of basic blocks that are points where a function call returns
    std::unordered_set<const BasicBlock*> returnSites;

    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        returnSites.insert(contextSaver._afterCall);
    }

    // Build the control flow graph
    ControlFlowGraph controlFlowGraph(function, program, OperationEnumerationMode::Unscheduled,
                                      ControlFlowGraphPhase::PrePipeline);

    // Compute live-in registers for basic blocks in this function
    ComputeLiveInMap(program, function, controlFlowGraph, returnSites, liveInMap,
                     OperationEnumerationMode::Unscheduled);

    // Determine the set of registers that are live in to the first basic block
    // (according to ComputeLiveInMap)
    BasicBlock* const firstBb = &(function._basicBlocks.front());

    const RegisterSet& liveInRegs = liveInMap[firstBb];

    // firstBb->_liveInReg is the set of registers that will be written by the caller
    RegisterSet callerProvided(firstBb->_liveInReg.begin(), firstBb->_liveInReg.end());

    for (const size_t liveInReg : liveInRegs)
    {
        if (!Contains(callerProvided, liveInReg))
        {
            SerializeFunction(std::cout, function, program);

            throw std::runtime_error(std::string("Local register ") + std::to_string(liveInReg) +
                                     std::string(" is uninitialized at the entry BB"));
        }
    }
}
