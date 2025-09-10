// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

const std::string g_globalClassName("__globalClass__");
const std::string g_globalObjectName("__globalObject__");

static Location g_currentTokenLocation = {};

bool operator<(const Location& lhs, const Location& rhs)
{
    return std::tie(lhs._beginLine, lhs._beginColumn, lhs._endLine, lhs._endColumn, lhs._fileIndex, lhs._valid) <
           std::tie(rhs._beginLine, rhs._beginColumn, rhs._endLine, rhs._endColumn, rhs._fileIndex, rhs._valid);
}

mp_int StringToMpInt(const std::string& inputString, const Type* const type)
{
    mp_int result(0);

    const mp_int typeWidthMask = (mp_int(1) << type->GetBitWidth()) - 1;

    std::istringstream str(inputString);

    // If inputString is negative, this will set the sign flag in the mp_int to true
    // and then store the unsigned results
    str >> result;

    // Clear all upper bits beyond the width specified by the type
    // If the sign flag in result is set, this will  negate, perform the bitwise and,
    // and the sign flag in the result will not be set
    result = result & typeWidthMask;

    return result;
}

ErrStream GetCompilerErrorStream(const Location& location, const CompileError error)
{
    return g_compiler->ErrorStream(location, error);
}

void TypeCheckArrayIndex(const ParseTreeNode* const indexNode, const Location& location)
{
    const LeafType* const indexType = dynamic_cast<const LeafType*>(indexNode->GetType());

    if (!indexType)
    {
        g_compiler->ErrorStream(location, CompileError::InvalidArrayIndexing) << "Array index must be an integer";
    }
}

// Validates that the same attribute is not specified more than once
void CheckForMultipleAttributes(const ParseTreeNode* const modifiers, const Location& location)
{
    const std::vector<const ParseTreeNode*>& modifierList = dynamic_cast<const NodeList*>(modifiers)->Children();

    std::set<unsigned> attributes;

    for (const ParseTreeNode* const modifier : modifierList)
    {
        auto baseAttr = dynamic_cast<const BaseAttributeNode*>(modifier);

        if (baseAttr)
        {
            const auto insertResult = attributes.insert(baseAttr->_attribute);
            if (!insertResult.second)
            {
                g_compiler->ErrorStream(location, CompileError::InvalidAttribute)
                    << "The same attribute cannot be specified multiple times";
            }
        }
    }
}

std::string CombineObjectAndMemberName(const Scope& scope, const std::string& objectName, const std::string& memberName)
{
    std::string result;

    if (g_globalObjectName == objectName)
    {
        result = memberName;
    }
    else
    {
        result = objectName + "_" + memberName;
    }

    return FlattenScopeAndAppendName(scope, result);
}

// Given a list of identifier nodes
// construct a Scope object
Scope ScopeFromNodeList(const ParseTreeNode* const scopeNode)
{
    Scope result;

    const NodeList* const nodeList = dynamic_cast<const NodeList*>(scopeNode);

    const std::vector<const ParseTreeNode*>& nodes = nodeList->Children();

    for (const ParseTreeNode* node : nodes)
    {
        const std::string& scopeName = dynamic_cast<const IdentifierNode*>(node)->GetValue();

        result.push_front(scopeName);
    }

    return result;
}

void GetMembers(const ParseTreeNode* const members, const ParseTreeMemberProtectionModifier initialProtectionModifier,
                Compiler::MemberVariableList& memberVariables, Compiler::MemberFunctionList& memberFunctions)
{
    const std::vector<const ParseTreeNode*>& memberNodes = dynamic_cast<const NodeList*>(members)->Children();

    ParseTreeMemberProtectionModifier memberProtectionModifier = initialProtectionModifier;

    for (const ParseTreeNode* memberNode : memberNodes)
    {
        auto declareNode = dynamic_cast<const DeclareNode*>(memberNode);

        auto memberModifierNode = dynamic_cast<const MemberModifierNode*>(memberNode);

        auto nestedScopeNode = dynamic_cast<const NestedScopeNode*>(memberNode);

        if (declareNode)
        {
            const std::string& name = declareNode->GetDeclaredName();

            memberVariables.push_back(std::pair<std::string, const DeclareNode*>(name, declareNode));

            // Note that the declare node is a member
            // so that it will not allocate registers
            const_cast<DeclareNode*>(declareNode)->MarkAsMember(memberProtectionModifier);
        }
        else if (memberModifierNode)
        {
            // Update the protection modifier associated with future member variables
            memberProtectionModifier = memberModifierNode->GetModifier();
        }
        else if (nestedScopeNode)
        {
            const ParseTreeNode* const body = nestedScopeNode->GetBody();

            const FunctionNode* const functionNode = dynamic_cast<const FunctionNode*>(body);

            const ClassNode* const classNode = dynamic_cast<const ClassNode*>(body);

            const InlineExternalModuleNode* const fixedLatencyFunction =
                dynamic_cast<const InlineExternalModuleNode*>(body);

            if (functionNode)
            {
                memberFunctions.push_back(functionNode->GetName());
            }
            else
            {
                assert(classNode || fixedLatencyFunction);
                // Ignore nested classes and fixed-latency methods
            }
        }
    }
}

void ToSigned(LeafType& input)
{
    if (input._baseType == BaseType::Uint)
    {
        // Convert to signed, and add 1 bit
        input._baseType = BaseType::Int;
        input._width++;
    }
    else
    {
        assert(input._baseType == BaseType::Int);
    }
}

size_t GetIntegerBitCount(const mp_int& input)
{
    if (input == 0)
    {
        return 1;
    }
    else
    {
        return Log2RoundUp(input + 1);
    }
}

const LeafType* ToSigned(const LeafType* const input, const Location& location)
{
    LeafType modified = *input;

    ToSigned(modified);

    return g_compiler->GetLeafType(modified._baseType, modified._width, location);
}

bool Type::TypeCheckConversion(const Type* const dst, const Type* const src, const Location& location)
{
    bool result = true;

    if ((typeid(*dst) == typeid(LeafType) || typeid(*dst) == typeid(EnumType)) &&
        ((typeid(*src)) == typeid(LeafType) || (typeid(*src) == typeid(EnumType))))
    {
        // All leaf types are convertible to all other leaf types
    }
    else if ((typeid(*dst) == typeid(FunctionType)) && (typeid(*src)) == typeid(FunctionType))
    {
        const FunctionType* const dstFunctionType = safe_cast<const FunctionType*>(dst);
        const FunctionType* const srcFunctionType = safe_cast<const FunctionType*>(src);

        // Check conversions on each parameter and each return value
        // This is needed to handle callbacks that are subtypes
        // of the function they are assigned to

        if (dstFunctionType->GetParamCount() != srcFunctionType->GetParamCount())
        {
            g_compiler->ErrorStream(location, CompileError::InvalidConversion)
                << "Can't convert value of type " << src->GetName() << " to " << dst->GetName()
                << " because parameter counts do not match";

            result = false;
        }
        else
        {
            for (size_t i = 0; i < dstFunctionType->GetParamCount(); i++)
            {
                // src and dst are swapped here
                // because for parameters, the concrete parameter type can be wider than the callback parameter type
                result = result && Type::TypeCheckConversion(srcFunctionType->GetParamType(i),
                                                             dstFunctionType->GetParamType(i), location);
            }

            result = result && Type::TypeCheckConversion(dstFunctionType->GetReturnType(),
                                                         srcFunctionType->GetReturnType(), location);
        }
    }
    else
    {
        if (dst != src)
        {
            g_compiler->ErrorStream(location, CompileError::InvalidConversion)
                << "Can't implicitly convert value of type " << src->GetName() << " to " << dst->GetName();

            result = false;
        }
    }

    return result;
}

ParseTreeNodePtr Type::CreateParseTreeNode() const { return g_compiler->Create<TypeNode>(this); }

void Type::ExportVerilog(SourceWriter& writer) const
{
    g_compiler->ErrorStream(g_currentTokenLocation, CompileError::ExportTypeRestriction)
        << "Type: " << GetName() << " does not support Verilog export";
}

void Type::WriteSymbols(SymbolWriter& symbolWriter) const { symbolWriter.Append("typeclass", "primitive"); }

void BoolType::ExportVerilog(SourceWriter& writer, const std::string& name) const
{
    writer.Str() << "typedef logic " << FixupString(name) << ";";
}

void FloatType::ExportVerilog(SourceWriter& writer, const std::string& name) const
{
    writer.Str() << "typedef logic [" << (_bitWidth - 1) << ":0] " << FixupString(name) << ";";
}

void LeafType::ExportVerilog(SourceWriter& writer, const std::string& name) const
{
    std::ostringstream str;

    str << "typedef logic ";

    if (BaseType::Uint == _baseType)
    {
        str << "unsigned ";
    }

    str << "[" << (_width - 1) << ":0] " << FixupString(name) << ";";

    writer.Str() << str.str();
}

std::string EnumType::GetVerilogName() const { return FixupString(_name); }

void EnumType::ExportVerilog(SourceWriter& writer, const std::string& name) const
{
    std::ostringstream str;

    str << "typedef enum logic " << ((BaseType::Uint == _baseType) ? "unsigned " : "");

    const std::string fixedName = FixupString(name);
    str << "[" << (_width - 1) << ":0] { ";
    for (size_t i = 0; i < _constants.size(); ++i)
    {
        str << fixedName << "_" << _constants[i].first << " = " << _width << "'b";

        for (size_t j = 0; j < _width; ++j)
        {
            const size_t bitIndex = _width - j - 1;

            if (bit_test(_constants[i].second, bitIndex))
            {
                str << "1";
            }
            else
            {
                str << "0";
            }
        }

        str << (((i + 1) < _constants.size()) ? ", " : "");
    }
    str << " } " << fixedName << ";";

    writer.Str() << str.str();
}

mp_int EnumType::GetConstantValue(size_t index) const
{
    const mp_int value = _constants[index].second;
    return (BaseType::Int == _baseType && bit_test(value, _width - 1)) ? value - (mp_int(1) << _width) : value;
}

void EnumType::WriteSymbols(SymbolWriter& symbolWriter) const
{
    symbolWriter.Append("typeclass", "enum");

    for (const auto& constant : _constants)
    {
        // Format the constant carefully.
        // It can have arbitrary size, and a user might have to keep it as a string
        // Use the same exact format that ReadInspectables uses.
        size_t width = RoundUp(_width, 8);
        std::stringstream stream;
        stream << std::hex << std::nouppercase << std::setfill('0') << std::setw(width / 4) << constant.second;

        // BUG: Boost's cpp_int does not obey std::nouppercase (fixed in some version later than the 67 we use)
        std::string hex = stream.str();
        std::transform(hex.begin(), hex.end(), hex.begin(), [](unsigned char c) { return std::tolower(c); });

        symbolWriter.Append("member_name", constant.first);
        symbolWriter.Append("enum_value", hex);
    }
}

void ArrayType::ExportVerilog(SourceWriter& writer) const
{
    // No need to export
    // The raw name will work fine
    // for example: uint32[3:0]
}

void StructUnionType::ExportVerilog(SourceWriter& writer) const
{
    if (_type == ContainerType::Struct)
    {
        writer.Str() << "typedef struct packed";

        writer.Str() << "{";

        {
            AutoIndent autoIndent(writer);

            // SystemVerilog structs are written most-significant element first
            for (auto it = _members.rbegin(); it != _members.rend(); ++it)
            {
                const StructUnionType::EntryType& entry = *it;

                if (entry.second->GetDeclaredType()->ShouldExport())
                {
                    writer.Str() << entry.second->GetDeclaredType()->GetVerilogName() << " " << FixupString(entry.first)
                                 << ";";
                }
            }
        }

        writer.Str() << "} " << GetVerilogName() << ";";
    }
    else
    {
        writer.Str() << "typedef logic [" << (GetBitWidth() - 1) << ":0] " << GetVerilogName() << ";";

        // SystemVerilog structs are written most-significant element first
        for (const EntryType& member : _members)
        {
            const std::string memberName = member.first;
            writer.Str() << "function automatic Read" << memberName << "From" << GetVerilogName() << "("
                         << GetVerilogName() << " v);";
            {
                const size_t memberWidth = member.second->GetDeclaredType()->GetBitWidth();
                AutoIndent autoIndent(writer);
                writer.Str() << member.second->GetDeclaredType()->GetVerilogName() << " " << memberName << " = v["
                             << memberWidth - 1 << ":0];";
                writer.Str() << "return " << memberName << ";";
            }
            writer.Str() << "endfunction";
        }
    }
}

void StructUnionType::ExportVerilogTypedef(SourceWriter& writer, const std::string& name) const
{
    writer.Str() << "typedef " << GetVerilogName() << " " << FixupString(name) << ";";
}

std::string StructUnionType::GetCppMemberModifier(const size_t srcOffset, const size_t width) const
{
    size_t currOffset = 0;

    std::string result;

    for (const EntryType& member : _members)
    {
        const Type* const memberType = member.second->GetDeclaredType();

        const size_t memberStart = currOffset;

        const size_t memberEnd = currOffset + memberType->GetBitWidth();

        if ((srcOffset >= memberStart) && (srcOffset < memberEnd))
        {
            result = "." + member.first + memberType->GetCppMemberModifier(srcOffset - memberStart, width);
            break;
        }

        currOffset += member.second->GetDeclaredType()->GetBitWidth();
    }

    assert(!result.empty());
    return result;
}

std::string StructUnionType::GetVerilogName() const { return FixupString(_name); }

ArrayTypeBase::ArrayTypeBase(const Type* const elementType, const size_t arraySize, const Location& location)
    : _elementType(elementType), _arraySize(arraySize)
{
}

size_t ArrayTypeBase::GetBitWidth() const { return _elementType->GetBitWidth() * GetTotalCount(); }

const Type* ArrayTypeBase::GetElementType(const size_t elementIndex) const
{
    assert(elementIndex < GetTotalCount());
    return _elementType;
}

size_t ArrayTypeBase::GetTotalCount() const { return _arraySize; }

size_t ArrayTypeBase::GetRegisterCount() const { return _arraySize * _elementType->GetRegisterCount(); }

void ArrayTypeBase::WriteSymbols(SymbolWriter& symbolWriter) const
{
    symbolWriter.Append("count", _arraySize);

    symbolWriter.Append("element_type", _elementType->GetName());
}

ArrayType::ArrayType(const Type* const elementType, const size_t arraySize, const Location& location)
    : ArrayTypeBase(elementType, arraySize, location)
{
}

const Type* ArrayType::ResolveDeferredType(const Location& location) const
{
    return g_compiler->GetArrayType(_elementType->ResolveDeferredType(location), _arraySize, ParseTreeArrayTypeDefault,
                                    ParseTreeMemoryTypeDefault, nullptr, false, location);
}

// Used for arrays of objects, to reference elements with the class
// Note this is called for non-object types too, in which case it is a nop
void ArrayType::RegisterObjects(const RegisterObjectsMode mode, const std::string& objectName,
                                const ObjectPath& objectPath, const ObjectCallback& objectCallback,
                                const DefaultInitializeCallback& defaultInitializeCallback,
                                ObjectToClassCallableFunctionMap& objectToCallableFunctions) const
{
    const size_t totalCount = GetTotalCount();

    for (size_t i = 0; i < totalCount; ++i)
    {
        const std::string elementObjectName = GetArrayElementObjectName(objectName, i);

        const ObjectPath elementPath = AppendToPath(objectPath, std::string("element_") + std::to_string(i));

        _elementType->RegisterObjects(mode, elementObjectName, elementPath, objectCallback, defaultInitializeCallback,
                                      objectToCallableFunctions);
    }
}

void ArrayType::WriteSymbols(SymbolWriter& symbolWriter) const
{
    symbolWriter.Append("typeclass", "array");

    ArrayTypeBase::WriteSymbols(symbolWriter);
}

MemoryType::MemoryType(const Type* const elementType, const size_t arraySize, const ParseTreeMemoryType memoryType,
                       const ScopedIdentifierNode* const eccFuncNameNode, const bool autoInitialize,
                       const Location& location)
    : ArrayTypeBase(elementType, arraySize, location), _memoryType(memoryType), _eccFuncNameNode(eccFuncNameNode),
      _autoInitialize(autoInitialize)
{
    elementType->VisitTypes(
        [&](const Type* t)
        {
            if (g_compiler->GetStringType() == t)
            {
                g_compiler->ErrorStream(location, CompileError::InvalidString) << "memories cannot contain strings";
            }
        },
        VisitTypesBehavior::Default);
}

const Type* MemoryType::ResolveDeferredType(const Location& location) const
{
    return g_compiler->GetArrayType(_elementType->ResolveDeferredType(location), _arraySize, ParseTreeArrayTypeMemory,
                                    _memoryType, _eccFuncNameNode, _autoInitialize, location);
}

void MemoryType::WriteSymbols(SymbolWriter& symbolWriter) const
{
    symbolWriter.Append("typeclass", "memory");

    ArrayTypeBase::WriteSymbols(symbolWriter);
}

void MemoryType::ExportVerilog(SourceWriter& writer) const
{
    // No need to export
    // The raw name will work fine
    // for example: uint32[3:0]
}

StructUnionType::StructUnionType(const std::string& name, const ContainerType type,
                                 const std::vector<EntryType>& children)
    : _name(name), _type(type), _members(children)
{
}

size_t StructUnionType::GetBitWidth() const
{
    size_t result = 0;

    switch (_type)
    {
    case ContainerType::Struct:
        for (size_t i = 0; i < _members.size(); ++i)
        {
            result += _members[i].second->GetDeclaredType()->GetBitWidth();
        }
        break;

    case ContainerType::Union:
        for (size_t i = 0; i < _members.size(); ++i)
        {
            result = std::max<size_t>(_members[i].second->GetDeclaredType()->GetBitWidth(), result);
        }
        break;

    default:
        assert(false);
    }

    return result;
}

const Type* StructUnionType::GetElementType(const size_t elementIndex) const
{
    // Doesn't make logical sense for a union
    assert(_type == ContainerType::Struct);

    assert(elementIndex < _members.size());

    return _members[elementIndex].second->GetDeclaredType();
}

void StructUnionType::VisitTypes(const TypeVisitFunction& callback, const VisitTypesBehavior behavior) const
{
    for (const EntryType& member : _members)
    {
        member.second->GetDeclaredType()->VisitTypes(callback, behavior);
    }

    Type::VisitTypes(callback, behavior);
}

void StructUnionType::WriteSymbols(SymbolWriter& symbolWriter) const
{
    if (_type == ContainerType::Struct)
    {
        symbolWriter.Append("typeclass", "struct");
    }
    else
    {
        assert(_type == ContainerType::Union);
        symbolWriter.Append("typeclass", "union");
    }

    size_t offset = 0;

    for (const EntryType& member : _members)
    {
        const Type* const memberType = member.second->GetDeclaredType();

        symbolWriter.Append("member_name", member.first);

        symbolWriter.Append("member_type", memberType->GetName());

        symbolWriter.Append("member_offset", offset);

        symbolWriter.Append("member_width", memberType->GetBitWidth());

        if (_type == ContainerType::Struct)
        {
            offset += memberType->GetBitWidth();
        }
    }
}

// Used only to resolve references (which can only be contained within structures)
// Flattens nested structures within structures
std::vector<StructUnionType::EntryType> StructUnionType::GetFlattenedMembers() const
{
    assert(_type == ContainerType::Struct);

    std::vector<EntryType> result;

    for (const auto& member : _members)
    {
        const Type* const memberType = member.second->GetDeclaredType();

        const StructUnionType* const memberStructType = dynamic_cast<const StructUnionType*>(memberType);

        if (memberStructType && (memberStructType->_type == ContainerType::Struct))
        {
            const std::vector<EntryType> recursiveResult = memberStructType->GetFlattenedMembers();

            for (const auto& recursiveMember : recursiveResult)
            {
                EntryType et = {};

                et.first = GetMemberName(member.first, recursiveMember.first);
                et.second = recursiveMember.second;

                result.push_back(et);
            }
        }
        else
        {
            result.push_back(member);
        }
    }

    return result;
}

// Returns a map from reference expressions in the right-hand-side of an assignment
// to reference expressions in the left-hand-side of an assignment
// srcName is the name of the structure variable in the left-hand-side
// dstName is the name of the structure variable in the right-hand-side
std::map<std::string, std::string>
StructUnionType::GetReferences(const std::string& srcName, const std::string& dstName,
                               const ResolveReferenceFunction& resolveReferenceCallback) const
{
    assert(_type == ContainerType::Struct);

    std::map<std::string, std::string> result;

    const std::vector<StructUnionType::EntryType> members = GetFlattenedMembers();

    for (const auto& member : members)
    {
        if (ContainsReference(member.second->GetDeclaredType()))
        {
            const std::string srcMemberName = GetMemberName(srcName, member.first);
            const std::string dstMemberName = GetMemberName(dstName, member.first);

            const std::string objectName = resolveReferenceCallback(srcMemberName);

            result[dstMemberName] = objectName;
        }
    }

    return result;
}

// Only the verilog backend fully supports exported classes
bool ClassType::IsExport() const
{
    return _exported && (g_compiler->IsCompilingToVerilog() ? true : (_instanceCount == 0));
}

// For validation that should apply regardless of the target
bool ClassType::IsExportIgnoreTarget() const { return _exported; }

const std::string& ClassType::GetExportedName() const { return _exportedName; }

bool ClassType::HasExternNameAttribute() const
{
    assert(_external);

    return _externName.has_value();
}

// Returns the name of an RTL module that will be instantiated
// for an external class
const std::string ClassType::GetExternalName() const
{
    if (_externName)
    {
        // the [[name]] attribute was specified
        return *_externName;
    }
    else if (_classNode->IsTemplateInstance())
    {
        // This class is a template instance
        // use the name of the template
        return _classNode->GetTemplateName();
    }
    else
    {
        return GetName();
    }
}

void ClassType::SetExported(const std::optional<std::string>& nameOverride) const
{
    _exported = true;

    _exportedName = nameOverride ? *nameOverride : _className;
}

bool ClassType::IsExternal() const { return _external; }

void ClassType::SetExternal(const std::optional<std::string>& nameAttr)
{
    _external = true;

    _externName = nameAttr;
}

void ClassType::VisitTypes(const TypeVisitFunction& callback, const VisitTypesBehavior behavior) const
{
    for (const EntryType& member : _memberVariables)
    {
        member.second->GetDeclaredType()->VisitTypes(callback, behavior);
    }

    Type::VisitTypes(callback, behavior);
}

const Type* ClassType::TryGetMemberType(const Scope& scope, const std::string& name) const
{
    // Members cannot be referenced with a scoped name
    if (!scope.empty())
    {
        return nullptr;
    }

    for (const EntryType& entry : _memberVariables)
    {
        if (entry.first == name)
        {
            return entry.second->GetDeclaredType();
        }
    }

    return nullptr;
}

// Returns the function type associated with a callback
// Returns nullptr if the provided name does not match any callback
const FunctionType* ClassType::GetCallbackType(const Scope& scope, const std::string& name) const
{
    return dynamic_cast<const FunctionType*>(TryGetMemberType(scope, name));
}

bool ClassType::ContainsMethod(const std::string& name) const
{
    return _memberFunctions.end() != std::find(_memberFunctions.begin(), _memberFunctions.end(), name);
}

// Should not be called on the default class type
// (which has no associated class node)
const ClassNode* ClassType::GetClassNode() const
{
    assert(_classNode);

    return _classNode;
}

void ClassType::SetTemplate(const ExternClassTemplateType* const templ)
{
    assert(!_externClassTemplate);

    _externClassTemplate = templ;
}

bool ClassType::IsTemplate() const { return _externClassTemplate != nullptr; }

// Notifies the class of an object of the class type that exists
// Called for global objects (from DeclareNode), also recursively called for member objects
void ClassType::RegisterObjects(const RegisterObjectsMode mode, const std::string& objectName,
                                const ObjectPath& objectPath, const ObjectCallback& objectCallback,
                                const DefaultInitializeCallback& defaultInitializeCallbackIn,
                                ObjectToClassCallableFunctionMap& objectToCallableFunctions) const
{
    // IsExportNoTarget ensures that default initializers do not cross export class boundaries
    const DefaultInitializeCallback defaultInitializeCallback =
        IsExportIgnoreTarget() ? DefaultDefaultInitializeCallback : defaultInitializeCallbackIn;

    if (mode == RegisterObjectsMode::RegisterObjects)
    {
        _objectNames.push_back(objectName);

        // Store information for a name->path lookup
        g_compiler->RegisterObjectPath(objectName, objectPath);
    }

    // Update the mapping of strings to callable functions
    // The mapping may already have entries if there are outer initializer lists
    ClassCallableFunctionMap& classCallableFunctionMap = objectToCallableFunctions[objectName];

    // Class methods
    // An external class cannot call it's own methods (only callbacks)
    if (!IsExternal())
    {
        for (const std::string& methodName : _memberFunctions)
        {
            ClassCallableFunction ccf = {};

            ccf._objectName = objectName;
            ccf._classType = this;
            ccf._functionNode = GetFunctionByName(this, Scope(), methodName, _classNode->GetLocation());

            SafeInsert(classCallableFunctionMap, methodName, ccf);
        }
    }

    const CompiledModule& currentCompiledModule = g_compiler->GetCurrentModuleToCompile();

    const bool isExportClassBeingCompiled = (currentCompiledModule._classNodeToCompile == _classNode) &&
                                            (currentCompiledModule._placeholderObjectName == objectName);

    // Class callbacks
    for (const EntryType& entry : _memberVariables)
    {
        const FunctionType* const functionType = dynamic_cast<const FunctionType*>(entry.second->GetDeclaredType());

        if (functionType)
        {
            // Check to see if the callback has an initial value
            // The initial value is only used if an outer initializer list
            // Does not define the value of this callback
            if (entry.second->GetAssignNode() &&
                (classCallableFunctionMap.end() == classCallableFunctionMap.find(entry.first)))
            {
                const ClassCallableFunction ccf =
                    safe_cast<const CallbackRValNode*>(entry.second->GetAssignNode()->GetRhs())
                        ->DescribeCall(objectName, classCallableFunctionMap);

                SafeInsert(classCallableFunctionMap, entry.first, ccf);
            }

            // Connect callbacks of exported classes without inital values corresponding extern callback functions
            if (isExportClassBeingCompiled && !entry.second->GetAssignNode())
            {
                const FunctionNode* const externCallback = _classNode->TryGetExternCallback(entry.first);
                assert(externCallback);

                const ClassCallableFunction ccf = {objectName, g_compiler->GetGlobalClassType(), externCallback};

                SafeInsert(classCallableFunctionMap, entry.first, ccf);
            }

            if (classCallableFunctionMap.end() == classCallableFunctionMap.find(entry.first))
            {
                // The callback is uninitialized, check for default initializers in containing object
                // That this this calls defaultInitializeCallbackIn, because it is fine for callbacks of
                // export classes to accept default initializers from above.
                defaultInitializeCallbackIn(entry.first, objectName, functionType);
            }
        }
    }

    objectCallback(objectName, _classNode, classCallableFunctionMap);

    const DesignatedInitializerListNode* defaultInitializer =
        _classNode ? _classNode->TryGetDefaultInitializer() : nullptr;

    for (const EntryType& entry : _memberVariables)
    {
        if (!entry.second->ContainsClassType())
        {
            // This loop only applies to members that contain objects
            continue;
        }

        // If this class is an exported class, but is not currently being compiled
        // then do not register member objects.  They do not affect the use of this class.
        if (IsExport() && !isExportClassBeingCompiled)
        {
            continue;
        }

        const Type* const childType = entry.second->GetDeclaredType();

        const std::string childObjectName = entry.second->GetCombinedName(objectName);

        const ObjectPath childObjectPath = AppendToPath(objectPath, entry.second->GetDeclaredName());

        // If there is an initializer list for the member object
        // Then resolve it to concrete functions now
        TryResolveInitializerListFunctions(childObjectName, objectName,
                                           entry.second->GetAssignNode() ? entry.second->GetAssignNode()->GetRhs()
                                                                         : nullptr,
                                           objectToCallableFunctions);

        // This function is called to fill in entries of ClassCallableFunctionMap
        // from default initializers.
        // It only fills in entries in ClassCallableFunctionMap which are currently uninitialized
        const DefaultInitializeCallback childDefaultInitializeCallback = [&](const std::string& callbackName,
                                                                             const std::string& objectToInitialize,
                                                                             const FunctionType* const callbackType)
        {
            bool continueSearch = true;

            // Apply the default initializer from this class
            if (defaultInitializer)
            {
                // Add functions from the default initializer to objectToCallableFunctions
                // This occurs after
                if (defaultInitializer->ResolveFunction(objectToInitialize, objectName, callbackName, callbackType,
                                                        objectToCallableFunctions))
                {
                    // No need to keep searching for a more outer default initializer for this callback
                    continueSearch = false;
                }
            }

            // Apply default initializer from the class of the containing object
            // defaultInitializeCallback is used here, to stop searching at export class boundaries
            if (continueSearch)
            {
                defaultInitializeCallback(callbackName, objectToInitialize, callbackType);
            }
        };

        childType->RegisterObjects(mode, childObjectName, childObjectPath, objectCallback,
                                   childDefaultInitializeCallback, objectToCallableFunctions);
    }
}

LeafType::LeafType(const BaseType baseType, const size_t bitCount, const Location& location)
    : _baseType(baseType), _width(bitCount)
{
    if (_width == 0)
    {
        g_compiler->ErrorStream(location, CompileError::InvalidType) << "Integer bit width must be positive";
    }
}

size_t LeafType::GetBitWidth() const { return _width; }

std::string LeafType::GetName() const
{
    std::string str;

    switch (_baseType)
    {
    case BaseType::Int:
        str = "int";
        break;

    case BaseType::Uint:
        str = "uint";
        break;

    default:
        assert(false);
    }

    str += std::to_string(_width);

    return str;
}

std::string FloatType::GetName() const
{
    std::ostringstream str;

    str << "float" << _bitWidth;

    return str.str();
}

std::string ArrayType::GetName() const
{
    std::vector<size_t> dims;

    return GetNameWithDimensions(dims);
}

size_t StringType::GetBitWidth() const { return c_stringHandleWidth; }

std::string StringType::GetVerilogName() const { return "__stringHandle"; }

void StringType::ExportVerilog(SourceWriter& writer, const std::string& name) const
{
    writer.Str() << "typedef logic [" << (c_stringHandleWidth - 1) << ":0] " << FixupString(name) << ";";
}

// uint32[z][y][x]
// is the same as:
// ((uint32[x])[y])[z]
//
// The first syntax is prefered, as it is what is allowed by the language
// The second syntax is used for memories or arrays or arrays of memories
// to ensure the output type is clear
std::string ArrayType::GetNameWithDimensions(std::vector<size_t>& dims) const
{
    const ArrayType* const elementArrayType = dynamic_cast<const ArrayType*>(_elementType);
    const MemoryType* const elementMemoryType = dynamic_cast<const MemoryType*>(_elementType);

    std::string result;

    dims.push_back(_arraySize);

    if (elementArrayType)
    {
        result = elementArrayType->GetNameWithDimensions(dims);
    }
    else if (elementMemoryType)
    {
        result += "(";
        result += elementMemoryType->GetName();
        result += ")";

        for (const size_t dim : dims)
        {
            result += "[";
            result += std::to_string(dim);
            result += "]";
        }
    }
    else
    {
        result = _elementType->GetName();

        for (const size_t dim : dims)
        {
            result += "[";
            result += std::to_string(dim);
            result += "]";
        }
    }

    return result;
}

// Quartus synthesis doesn't like types like:
// uint32[4] x
// so instead generated: uint32[3:0] x
std::string ArrayType::GetVerilogName() const
{
    std::ostringstream str;

    str << _elementType->GetVerilogName() << " [" << _arraySize - 1 << ":0]";

    return str.str();
}

std::string ArrayType::GetCppMemberModifier(const size_t srcOffset, const size_t width) const
{
    const size_t elementWidth = _elementType->GetBitWidth();

    const size_t elementIndex = srcOffset / elementWidth;
    assert(elementIndex < _arraySize);

    const size_t offsetIntoElement = srcOffset % elementWidth;

    std::ostringstream str;
    str << ".data[" << elementIndex << "]" << _elementType->GetCppMemberModifier(offsetIntoElement, width);

    return str.str();
}

// For symbols
std::string MemoryType::GetName() const
{
    const ArrayType* const elementArrayType = dynamic_cast<const ArrayType*>(_elementType);

    std::string result = "[[memory]] ";

    // Parentheses are used for memories or arrays
    // to distingush which dimension is used for memories
    // and which is used for arrays
    if (elementArrayType)
    {
        result += "(";
    }

    result += _elementType->GetName();

    if (elementArrayType)
    {
        result += ")";
    }

    result += "[";
    result += std::to_string(_arraySize);
    result += "]";

    return result;
}

size_t ReferenceType::GetBitWidth() const { return 0; }

std::string ReferenceType::GetName() const
{
    std::ostringstream str;
    str << _referencedType->GetName() << "&";
    return str.str();
}

const Type* ReferenceType::ResolveDeferredType(const Location& location) const
{
    return g_compiler->GetReferenceType(_referencedType->ResolveDeferredType(location));
}

const Type* FunctionType::ResolveDeferredType(const Location& location) const
{
    std::vector<const Type*> paramTypesResolved;

    for (const Type* const paramType : _paramTypes)
    {
        paramTypesResolved.push_back(paramType->ResolveDeferredType(location));
    }

    return g_compiler->GetFunctionType(paramTypesResolved, _paramNames, _returnType->ResolveDeferredType(location),
                                       _modifiers, _latency, _isLastParameterIndex);
}

// Returns enough information in a FunctionDesc
// for type checking
FunctionDesc FunctionType::ToFunctionDesc() const
{
    FunctionDesc result = {};

    result._returnType = _returnType;
    result._parameterTypes = _paramTypes;
    result._parameterNames = _paramNames;
    result._modifiers = _modifiers;
    result._fixedLatency = _latency;
    result._isLastParameterIndex = _isLastParameterIndex;

    return result;
}

size_t FunctionType::GetParamCount() const { return _paramTypes.size(); }

const Type* FunctionType::GetParamType(const size_t index) const { return _paramTypes.at(index); }

const std::string& FunctionType::GetParamName(const size_t index) const { return _paramNames.at(index); }

const Type* FunctionType::GetReturnType() const { return _returnType; }

// Returns a map from reference expressions in the right-hand-side of an assignment
// to reference expressions in the left-hand-side of an assignment
// srcName is the name of the reference variable in the left-hand-side
// dstName is the name of the reference variable in the right-hand-side
std::map<std::string, std::string>
ReferenceType::GetReferences(const std::string& srcName, const std::string& dstName,
                             const ResolveReferenceFunction& resolveReferenceCallback) const
{
    std::map<std::string, std::string> result;

    result[dstName] = resolveReferenceCallback(srcName);

    return result;
}

size_t g_currParseTreeSequenceNumber = 0;

_ParseTreeNode::_ParseTreeNode(const Type* const type)
    : _type(type), _frontEndType(nullptr), _location(g_currentTokenLocation),
      _sequenceNumber(g_currParseTreeSequenceNumber++)
{
    g_compiler->AddParseTreeNode(this);
}

std::pair<Location, Location> _ParseTreeNode::GetLineBounds() const
{
    std::pair<Location, Location> result;

    result.first = result.second = GetLocation();

    const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& visitChildren)
    {
        const Location location = node->GetLocation();

        if (location._fileIndex == _location._fileIndex && location._valid &&
            location._beginLine > result.second._beginLine)
        {
            result.second = location;
        }

        visitChildren();
    };

    VisitContext context;
    VisitContext::PushPopScope globalScope(context);

    Visit(callback, context);

    return result;
}

void _ParseTreeNode::ResolveDeferredTypes() const
{
    VisitContext context;
    VisitContext::PushPopScope scope(context);

    Visit(
        [](const ParseTreeNode* const node, const VisitChildrenFunction& visitChildren)
        {
            if (auto t = dynamic_cast<const TypeNode* const>(node))
            {
                t->ResolveDeferredTypes();
            }

            visitChildren();
        },
        context);
}

void _ParseTreeNode::SetType(const Type* const type, const SetTypeBehavior behavior) const
{
    _type = type;

    // If the front-end provided a type
    // use it rather than the middle-end computed type
    // because the front-end has logic for special cases like x >> 2
    // This is not done for enums, because the front-end passes an integer type
    // rather than the enum type.
    if (_frontEndType && (_frontEndType != type) && (behavior == SetTypeBehavior::Default) &&
        (dynamic_cast<const EnumType*>(type) == nullptr))
    {
        // Front-end only provides integer types
        assert(dynamic_cast<const LeafType*>(_frontEndType));

        assert(!type || dynamic_cast<const LeafType*>(type));

        _type = _frontEndType;
    }

    // type can be null in cases where the middle-end
    // doesn't calculate the type, and uses _frontEndType instead
    assert(_type);
}

void _ParseTreeNode::SetFrontEndType(const Type* const type)
{
    assert(type);

    _frontEndType = type;

    // SetFrontEndType is called before middle-end type checking
    // In rare cases that _type is already set,
    // it should match the front-end type.
    assert(!_type || dynamic_cast<const DeferredType*>(_type) || (_type == _frontEndType));
}

IntegerNode::IntegerNode(const std::string& valueString) : _valueString(valueString)
{
    std::istringstream str(valueString);

    str >> _untypedValue;
}

// Front-end notifies the middle-end of the literal type here
// So that cases like concat can use the proper width for the literal
void IntegerNode::SetFrontEndType(const Type* const type)
{
    ParseTreeNode::SetFrontEndType(type);

    _typedValue = StringToMpInt(_valueString, type);
}

// Used before type checking, to get integer attributes
const mp_int& IntegerNode::GetUntypedValue() const { return _untypedValue; }

const mp_int& IntegerNode::GetTypedValue() const
{
    assert(_typedValue);

    return *_typedValue;
}

void IntegerNode::TypeCheck(TypeCheckContext& context) const
{
    // Simply accept the type provided by the front-end
    assert(_frontEndType);
    SetType(_frontEndType);
}

const std::string& IntegerNode::GetValueString() const { return _valueString; }

void FloatNode::TypeCheck(TypeCheckContext& context) const { SetType(g_compiler->GetFloatType(32)); }

KnownValue FloatNode::TryGetKnownValueImpl(KnownValueContext& context) const
{
    // convert floating point value to uint32
    return KnownValue(reinterpret_cast<const uint32_t*>(&_value)[0]);
}

void BoolNode::TypeCheck(TypeCheckContext& context) const { SetType(g_compiler->GetBoolType()); }

TypeNode::TypeNode(const Type* const type) : ParseTreeNode(type) {}

TypeNode::TypeNode(const Scope scope, const std::string& name, const Type* const type)
    : ParseTreeNode(type), _name(name), _scope(scope)
{
}

std::optional<std::string> TypeNode::GetTypedefName() const
{
    const std::string flattendName = FlattenScopeAndAppendName(_scope, _name);

    if (g_compiler->IsTypedef(flattendName))
        return std::optional<std::string>(flattendName);
    else
        return std::optional<std::string>();
}

void TypeNode::TypeCheck(TypeCheckContext& context) const
{
    if (auto mem = dynamic_cast<const MemoryType*>(GetType()))
    {
        if (!mem->_elementType->IsPod())
        {
            g_compiler->ErrorStream(GetLocation(), CompileError::NonValueType) << "Invalid memory element type";
        }
    }
}

// Gets a known value at type checking time
KnownValue TryGetTypeCheckKnownValue(TypeCheckContext& context, const ParseTreeNode* const expression,
                                     const Location location, const Type* resultType)
{
    // Wrap the context into an object that can be passed into TryGetKnownValue
    TypeCheckKnownValueContext knownValueContext(context);

    return expression->TryGetKnownValue(knownValueContext, resultType);
}

// Gets a known value during a Visit() traversal
KnownValue TryGetVisitKnownValue(VisitContext& context, const ParseTreeNode* const expression,
                                 const Type* const resultType)
{
    VisitCheckKnownValueContext knownValueContext(context);

    return expression->TryGetKnownValue(knownValueContext, resultType);
}

bool IsUnsignedOrNonNegative(TypeCheckContext& context, const ParseTreeNode* const node, const Location& location)
{
    const LeafType* const leafType = dynamic_cast<const LeafType*>(node->GetType());

    if (!leafType)
    {
        // Must be a bool/struct/union/etc
        return false;
    }

    if (leafType->_baseType != BaseType::Uint)
    {
        const KnownValue knownValue = TryGetTypeCheckKnownValue(context, node, location, node->GetType());

        if (knownValue._type == KnownValueType::Int)
        {
            // Signed, known at compile time
            // Check the sign bit to see if the value is negative
            if (bit_test(knownValue._intVal, node->GetType()->GetBitWidth() - 1))
            {
                return false;
            }
        }
        else
        {
            // Signed, not known at compile time
            return false;
        }
    }

    return true;
}

const Type* DeferredType::GetNamedType(const Location& location) const
{
    return g_compiler->GetNamedType(_implicitScope, _explicitScope, _name, location);
}

void TypeNode::ResolveDeferredTypes() const
{
    for (;;)
    {
        auto t = GetType()->ResolveDeferredType(_location);

        // This can happen for external class templates with no instances
        if (!t)
            break;

        if (t == GetType())
            break;

        SetType(t);
    }
}

ParseTreeFunctionModifier GetFunctionModifiers(const ParseTreeNode* const modifierList)
{
    uint32_t result = ParseTreeFunctionModifierNone;

    const std::vector<const ParseTreeNode*>& modifiers = dynamic_cast<const NodeList*>(modifierList)->Children();

    for (const ParseTreeNode* modifierParseTreeNode : modifiers)
    {
        const FunctionModifierNode* const modifierNode =
            dynamic_cast<const FunctionModifierNode*>(modifierParseTreeNode);

        if (modifierNode)
        {
            const uint32_t modifier = modifierNode->_modifier;

            // Don't allow the same modifier twice
            if (result & modifier)
            {
                g_compiler->ErrorStream(g_currentTokenLocation, CompileError::InvalidFunctionModifier)
                    << "The same function modifier can be specified more than once";
            }

            result |= modifier;
        }
    }

    return static_cast<ParseTreeFunctionModifier>(result);
}

boost::optional<size_t> GetFunctionLatency(const ParseTreeNode* const modifierList)
{
    boost::optional<size_t> latency;

    if (const auto list = dynamic_cast<const NodeList*>(modifierList))
    {
        for (const auto node : list->Children())
        {
            if (const auto attr = dynamic_cast<const IntAttributeNode*>(node))
            {
                if (attr->_attribute == ParseTreeLatencyAttr)
                {
                    latency = attr->_value;
                }
            }
        }
    }

    return latency;
}

size_t GetFunctionMaxThreadCount(const ParseTreeNode* const modifierList, const Location& location,
                                 const std::optional<size_t> functionCallCount)
{
    const ParseTreeFunctionModifier modifierFlags = GetFunctionModifiers(modifierList);

    size_t result = GetCodeGenConfig()._maxThreadsDefault;

    size_t maxThreadCountNodeCount = 0;

    const std::vector<const ParseTreeNode*>& modifiers = dynamic_cast<const NodeList*>(modifierList)->Children();

    for (const ParseTreeNode* modifierParseTreeNode : modifiers)
    {
        auto intAttr = dynamic_cast<const IntAttributeNode*>(modifierParseTreeNode);

        auto modifierNode = dynamic_cast<const FunctionModifierNode*>(modifierParseTreeNode);

        if (intAttr && intAttr->_attribute == ParseTreeMaxThreadsAttr)
        {
            result = intAttr->_value;

            ++maxThreadCountNodeCount;
        }

        if (modifierNode && (modifierNode->_modifier & ParseTreeFunctionModifierReset))
        {
            // This function has the [[reset]] attribute
            if (functionCallCount)
            {
                // The function instance enumerator always assumes that [[reset]] functions
                // have 1 implicit caller
                assert(*functionCallCount > 0);

                if (*functionCallCount == 1)
                {
                    // There are no explicit callers, so there will never be more than 1 thread in the function at a
                    // time Use that as the maximum thread count, to minimize resource usage (e.g., context saver depth)
                    result = 1;
                }
            }
        }
    }

    if ((modifierFlags & ParseTreeFunctionModifierInline) && (maxThreadCountNodeCount > 0))
    {
        g_compiler->ErrorStream(location, CompileError::InvalidFunctionModifier)
            << "Function max thread count cannot be specified on an inline function";
    }

    // Don't allow the same thread count to be specified more than once
    if (maxThreadCountNodeCount > 1)
    {
        g_compiler->ErrorStream(location, CompileError::InvalidFunctionModifier)
            << "Function max thread count can only be specified once";
    }

    if (0 == result)
    {
        g_compiler->ErrorStream(location, CompileError::InvalidFunctionModifier)
            << "Function max thread count must be positive";
    }
    else if (!IsPow2(result))
    {
        g_compiler->ErrorStream(location, CompileError::InvalidFunctionModifier)
            << "Function max thread count must be a power of 2";
    }

    // Validate that --max-threads-limit=N is not exceeded, for two reasons:
    // 1) User code can access --max-threads-limit via .options
    // and assume that this is the upper bound on the number of threads
    // executing concurrently in a given function
    // 2) Invocation index bit width is determined based on this
    if (result > GetCodeGenConfig()._maxThreadsLimit)
    {
        g_compiler->ErrorStream(location, CompileError::InvalidFunctionModifier)
            << "Function max thread count exceeds the limit set by --max-threads-limit";
    }

    return result;
}

size_t GetFunctionThreadRate(const ParseTreeNode* const modifierList, const Location& location)
{
    const ParseTreeFunctionModifier modifierFlags = GetFunctionModifiers(modifierList);

    size_t result = 1;

    size_t threadRateNodeCount = 0;

    const std::vector<const ParseTreeNode*>& modifiers = dynamic_cast<const NodeList*>(modifierList)->Children();

    for (const ParseTreeNode* modifierParseTreeNode : modifiers)
    {
        if (auto intAttr = dynamic_cast<const IntAttributeNode*>(modifierParseTreeNode))
            if (intAttr->_attribute == ParseTreeThreadRateAttr)
            {
                result = intAttr->_value;

                ++threadRateNodeCount;
            }
    }

    if ((modifierFlags & ParseTreeFunctionModifierInline) && (threadRateNodeCount > 0))
    {
        g_compiler->ErrorStream(g_currentTokenLocation, CompileError::InvalidFunctionModifier)
            << "Function thread rate count cannot be specified on an inline function";
    }

    // Don't allow the same thread count to be specified more than once
    if (threadRateNodeCount > 1)
    {
        g_compiler->ErrorStream(g_currentTokenLocation, CompileError::InvalidFunctionModifier)
            << "Function thread rate count can only be specified once";
    }

    if (result == 0)
    {
        g_compiler->ErrorStream(g_currentTokenLocation, CompileError::InvalidFunctionModifier)
            << "Function thread rate must be greater than 0";
    }

    return result;
}

boost::optional<size_t> GetFunctionFixedLatency(const ParseTreeNode* const modifierList)
{
    boost::optional<size_t> result;

    const std::vector<const ParseTreeNode*>& modifiers = dynamic_cast<const NodeList*>(modifierList)->Children();

    for (const ParseTreeNode* modifierParseTreeNode : modifiers)
    {
        if (auto intAttr = dynamic_cast<const IntAttributeNode*>(modifierParseTreeNode))
            if (intAttr->_attribute == ParseTreeLatencyAttr)
            {
                result = intAttr->_value;
            }
    }

    return result;
}

FunctionNode::FunctionNode(const ParseTreeNode* const modifierList, const ParseTreeNode* const returnType,
                           const ParseTreeNode* const name, const ParseTreeNode* const params,
                           const ParseTreeNode* const statements, const std::string& unmangledName)
    : _ParseTreeNode(nullptr), _modifiers(GetFunctionModifiers(modifierList)), _modifierList(modifierList),
      _threadRate(GetFunctionThreadRate(modifierList, g_currentTokenLocation)),
      _fixedLatency(GetFunctionFixedLatency(modifierList)), _returnType(returnType), _name(name), _params(params),
      _statements(statements), _originalStatements(statements), _unmangledName(unmangledName), _returnNode(nullptr),
      _classType(nullptr), _protectionModifier(ParseTreeMemberProtectionModifierPublic)
{
    g_compiler->AddFunctionNode(this);

    if (_fixedLatency)
    {
        // Fixed-latency implies no backpressure
        _modifiers = _modifiers | ParseTreeFunctionModifierNoBackPressure;
    }
}

FunctionNode::~FunctionNode() { g_compiler->RemoveFunctionNode(this); }

bool FunctionNode::IsFixedLatency() const
{
    if (_fixedLatency)
    {
        return true;
    }
    else
    {
        return false;
    }
}

size_t FunctionNode::GetLatency() const
{
    assert(IsFixedLatency());
    return *_fixedLatency;
}

size_t FunctionNode::GetParameterCount() const
{
    const NodeList* const parametersNode = dynamic_cast<const NodeList*>(_params);

    const std::vector<const ParseTreeNode*>& parameters = parametersNode->Children();

    return parameters.size();
}

const DeclareNode* FunctionNode::GetParameterDeclareNode(const size_t index) const
{
    const NodeList* const parametersNode = dynamic_cast<const NodeList*>(_params);

    const std::vector<const ParseTreeNode*>& parameters = parametersNode->Children();

    assert(index < parameters.size());

    const DeclareNode* const paramDeclaration = dynamic_cast<const DeclareNode*>(parameters[index]);

    return paramDeclaration;
}

const std::string& FunctionNode::GetParameterName(const size_t index) const
{
    return GetParameterDeclareNode(index)->GetDeclaredName();
}

size_t FunctionNode::GetParameterWidth(const size_t index) const
{
    return GetParameterDeclareNode(index)->GetDeclaredType()->GetBitWidth();
}

// Must be called after type checking
const ClassType* FunctionNode::GetClassType() const
{
    assert(_classType);
    return _classType;
}

void FunctionNode::SetClassType(const ClassType* const classType)
{
    assert(!_classType);
    _classType = classType;

    for (size_t i = 0; i < GetParameterCount(); i++)
    {
        DeclareNode* const declareNode = const_cast<DeclareNode*>(GetParameterDeclareNode(i));

        declareNode->SetClassType(classType);

        declareNode->SetContainingFunction(this);
    }
}

FunctionNode::Instance& FunctionNode::GetInstance(const std::string& objectName)
{
    auto it = _instances.find(objectName);
    assert(it != _instances.end());
    return it->second;
}

const FunctionNode::Instance& FunctionNode::GetInstance(const std::string& objectName) const
{
    auto it = _instances.find(objectName);
    assert(it != _instances.end());
    return it->second;
}

bool FunctionNode::HasInstance(const std::string& objectName) const
{
    auto it = _instances.find(objectName);

    return it != _instances.end();
}

const Type* FunctionNode::GetParameterType(const size_t index) const
{
    return GetParameterDeclareNode(index)->GetDeclaredType();
}

ParseTreeMemberProtectionModifier FunctionNode::GetProtectionModifier() const { return _protectionModifier; }

// Determines if a type can be used as a return/parameter type for an export function (framework mode only)
bool IsLegalWrapperType(const Type* const inputType)
{
    const LeafType* const leafType = dynamic_cast<const LeafType*>(inputType);
    const FloatType* const floatType = dynamic_cast<const FloatType*>(inputType);
    const ArrayType* const arrayType = dynamic_cast<const ArrayType*>(inputType);
    const StructUnionType* const structUnionType = dynamic_cast<const StructUnionType*>(inputType);

    bool result = false;

    if (leafType || floatType)
    {
        // Leaf and float types are allowed, with specific widths
        const size_t allowedWidths[] = {8, 16, 32, 64};

        const auto end = allowedWidths + ARRAY_SIZE(allowedWidths);

        if (end != std::find(allowedWidths, end, inputType->GetBitWidth()))
        {
            result = true;
        }
    }

    if (arrayType)
    {
        // Arrays are allowed, as long as the inner type is allowed
        result = IsLegalWrapperType(arrayType->_elementType);
    }

    if (structUnionType)
    {
        // Structs/unions are allowed if all members are legal
        result = true;

        for (const StructUnionType::EntryType& entry : structUnionType->_members)
        {
            result = result && IsLegalWrapperType(entry.second->GetDeclaredType());
        }
    }

    return result;
}

bool CheckWrapperType(const Type* const inputType, const Location& location) { return IsLegalWrapperType(inputType); }

void FunctionNode::TypeCheck(TypeCheckContext& context) const
{
    // Set context._global._currentFunction
    PushPopCurrentFunction pushPopCurrentFunction(context, this);

    // Determine what class this function is a member of - the global class type will be returned for flat functions
    // This is saved as a member for AllocateRegisters()
    _classType = context._global._classStack.top();

    // Register function types with global map
    const IdentifierNode* const nameIdentifierNode = dynamic_cast<const IdentifierNode*>(_name);

    // The name to pass to RegisterFunction()
    std::string registrationName = nameIdentifierNode->GetValue();

    // For flat functions: save combined namespace + function name
    // For member functions: save member name
    // for external module functions: save combined extern module name + function name
    {
        Scope functionScope = context.GetNamespaceScope();

        if (!context._global._externClassInstanceNameStack.empty())
        {
            registrationName = GetCombinedExternalClassInstanceFunctionName(
                context._global._externClassInstanceNameStack.top(), registrationName);
        }

        _flattenedName = FlattenScopeAndAppendFunctionName(_classType, functionScope, registrationName);

        _containingScope = functionScope;
    }

    const NodeList* const parametersNode = dynamic_cast<const NodeList*>(_params);

    const std::vector<const ParseTreeNode*>& parameters = parametersNode->Children();

    _modifierList->TypeCheck(context);

    // type check return type and parameters before registering the function
    // to resolve array sizes
    _returnType->TypeCheck(context);

    _params->TypeCheck(context);

    _protectionModifier = context._global._memberProtectionModifier;

    FunctionDesc functionDesc = {};

    if ((_modifiers & ParseTreeFunctionModifierAsync) && (_modifiers & ParseTreeFunctionModifierUnordered))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
            << "Async functions cannot be marked unordered";
    }

    // Extern module functions cannot be marked export
    const uint32_t ExternalExportMask = ParseTreeFunctionModifierExternal | ParseTreeFunctionModifierExport;
    if (ExternalExportMask == (_modifiers & ExternalExportMask))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
            << "Extern module functions cannot have the export modifier";
    }

    // export void startup() is deprecated
    if ((_modifiers & ParseTreeFunctionModifierExport) && (nameIdentifierNode->GetValue() == "startup"))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
            << "\'export void startup()\' is deprecated.  Use \'[[reset]] void startup()\' instead.";
    }

    if (_modifiers & ParseTreeFunctionModifierReset)
    {
        if (0 != (_modifiers & ~ParseTreeFunctionModifierReset))
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
                << "Functions with the [[reset]] attribute cannot have other attributes or modifiers.";
        }
    }

    functionDesc._modifiers = _modifiers;

    functionDesc._returnType = _returnType->GetType();

    functionDesc._protectionModifier = _protectionModifier;

    functionDesc._fixedLatency = _fixedLatency;

    size_t numEndTransactionParameters = 0;

    for (size_t parameterIndex = 0; parameterIndex < parameters.size(); parameterIndex++)
    {
        const ParseTreeNode* const param = parameters[parameterIndex];

        const DeclareNode* const paramDeclaration = dynamic_cast<const DeclareNode*>(param);

        functionDesc._parameterNames.push_back(paramDeclaration->GetDeclaredName());

        functionDesc._parameterTypes.push_back(paramDeclaration->GetDeclaredType());

        if (paramDeclaration->_isEndTransaction)
        {
            numEndTransactionParameters++;

            const BoolType* const paramTypeBoolean = dynamic_cast<const BoolType*>(paramDeclaration->GetDeclaredType());

            if (!paramTypeBoolean)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidEndTransaction)
                    << "[[last]] attribute can only be used with boolean parameters";
            }

            functionDesc._isLastParameterIndex = parameterIndex;
        }
    }

    if (numEndTransactionParameters > 1)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidEndTransaction)
            << "[[last]] attribute can only appear once per function";
    }

    // Functions are registered in the first pass
    if (TypeCheckPass::Functions == context._global._pass)
    {
        Scope functionScope = context.GetNamespaceScope();

        if (!context._global._externClassInstanceNameStack.empty())
        {
            if (_classType != g_compiler->GetGlobalClassType())
            {
                // Don't prepend namespace scope for extern modules which are members of a class
                functionScope.clear();
            }
        }

        g_compiler->RegisterFunction(_classType, functionScope, registrationName, functionDesc, _location);
    }

    // Nesting of functions is not allowed
    if (context._global._functionCount > 0)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidNesting) << "Nested functions are not allowed";
    }

    // Record in the context if the function is ordered
    const size_t maxThreadCount = GetFunctionMaxThreadCount(_modifierList, _location);

    assert(0 == context._global._returnCount);

    const Type* const returnType = _returnType->GetType();

    // Avoid type checking a call before the function has been registered
    if (TypeCheckPass::Default == context._global._pass)
    {
        ++context._global._functionCount;

        context._global._functionReturnType = _returnType->GetType();

        _statements->TypeCheck(context);

        context._global._functionReturnType = g_compiler->GetVoidType();

        --context._global._functionCount;

        // 1 return statement max
        if (context._global._returnCount > 1)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidReturn)
                << "Functions can only have 1 return statement";
        }

        // Non-void functions must have a return statement
        // This does not apply to external functions - which have a special implementation
        if ((returnType != g_compiler->GetVoidType()) && (0 == context._global._returnCount) &&
            !(GetModifiers() & ParseTreeFunctionModifierExternal))
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidReturn)
                << "Non-void functions must have 1 return statement";
        }

        // If there is a return statement, it must the last
        if (1 == context._global._returnCount)
        {
            const std::vector<const ParseTreeNode*>& statements =
                dynamic_cast<const NodeList*>(_statements)->Children();

            const ParseTreeNode* const lastStatement = statements.back();

            const ReturnNode* const returnNode = dynamic_cast<const ReturnNode*>(lastStatement);

            if (!returnNode)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidReturn)
                    << "Return statement must be the last statement in a function";
            }
        }

        // Reset _returnCount for other functions to use
        context._global._returnCount = 0;
    }

    if ((GetModifiers() & ParseTreeFunctionModifierAsync) && (returnType != g_compiler->GetVoidType()))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidFunctionReturnType)
            << "Async function return type must be void";
    }

    if ((GetModifiers() & ParseTreeFunctionModifierPipelined) && (GetModifiers() & ParseTreeFunctionModifierUnordered))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
            << "Pipelined functions cannot be unordered";
    }

    if ((GetModifiers() & ParseTreeFunctionModifierExport) && (GetModifiers() & ParseTreeFunctionModifierUnordered))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
            << "export functions cannot be unordered";
    }

    // Inline cannot be combined with other modifiers (expect unordered)
    const uint32_t allowedInlineModifiers = ParseTreeFunctionModifierUnordered | ParseTreeFunctionModifierInline;

    if ((GetModifiers() & ParseTreeFunctionModifierInline) && (0 != (GetModifiers() & ~allowedInlineModifiers)))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
            << "Inline functions cannot have attributes except unordered";
    }

    if (GetModifiers() & ParseTreeFunctionModifierPipelined)
    {
        const std::vector<const ParseTreeNode*>& params = dynamic_cast<const NodeList*>(_params)->Children();

        if (params.empty())
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidIterationCount)
                << "Pipelined function requires unsigned integer count parameter";
        }
        else
        {
            const LeafType* const param0Type =
                dynamic_cast<const LeafType*>(dynamic_cast<const DeclareNode*>(params[0])->GetDeclaredType());

            if (!param0Type || (param0Type->_baseType != BaseType::Uint))
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidIterationCount)
                    << "Pipelined function requires unsigned integer count argument";
            }
        }

        if ((GetModifiers() & ParseTreeFunctionModifierExport))
        {
           g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
               << "[[pipelined]] is not allowed on exported functions";
        }
    }

    if (_modifiers & ParseTreeFunctionModifierNoBackPressure)
    {
        if (maxThreadCount != GetCodeGenConfig()._maxThreadsDefault)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
                << "[[no_backpressure]]/[[latency]] cannot be used with max_threads";
        }

        if (_threadRate != 1)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
                << "[[no_backpressure]]/[[latency]] cannot be used with thread_rate";
        }

        const ParseTreeFunctionModifier allowedModifiers = static_cast<ParseTreeFunctionModifier>(
            ParseTreeFunctionModifierExport | ParseTreeFunctionModifierNoBackPressure |
            ParseTreeFunctionModifierExternal | ParseTreeFunctionModifierAsync);

        if ((_modifiers & ~allowedModifiers) != 0)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidFunctionModifier)
                << "[[no_backpressure]]/[[latency]] cannot be used with any attributes/modifiers except "
                   "export, extern, and [[async]].";
        }
    }

    SetType(g_compiler->GetFunctionType(functionDesc._parameterTypes, functionDesc._parameterNames,
                                        functionDesc._returnType, functionDesc._modifiers, functionDesc._fixedLatency,
                                        functionDesc._isLastParameterIndex));
}

void FunctionNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _returnType->Visit(visitFunction, context);

        _name->Visit(visitFunction, context);

        _params->Visit(visitFunction, context);

        _statements->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

const std::string& FunctionNode::GetName() const { return dynamic_cast<const IdentifierNode*>(_name)->GetValue(); }

const std::string& FunctionNode::GetUnmangledName() const { return _unmangledName; }

const std::string FunctionNode::GetFlattenedName() const { return _flattenedName; }

const Scope FunctionNode::GetContainingScope() const { return _containingScope; }

ParseTreeFunctionModifier FunctionNode::GetModifiers() const { return _modifiers; }

const Type* FunctionNode::GetReturnType() const { return dynamic_cast<const TypeNode*>(_returnType)->GetType(); }

bool FunctionNode::IsInline() const { return (_modifiers & ParseTreeFunctionModifierInline); }

bool FunctionNode::IsAutoInline(const FunctionNode* caller, const CallNode* const callNode) const
{
    const bool isInline = (_modifiers & ParseTreeFunctionModifierInline);
    const bool isNoInline = (_modifiers & ParseTreeFunctionModifierNoInline);
    const bool isExport = (_modifiers & ParseTreeFunctionModifierExport);
    const bool isAsync = (_modifiers & ParseTreeFunctionModifierAsync);
    const bool isPipelined = (_modifiers & ParseTreeFunctionModifierPipelined);
    const bool isExtern = (_modifiers & ParseTreeFunctionModifierExternal);
    const bool isNoBackpressure = (_modifiers & ParseTreeFunctionModifierNoBackPressure);

    // IsExportIgnoreTarget means that auto-inlining will be disabled for calls to export class methods
    const bool isExportClassMember = _classType && (_classType->IsExportIgnoreTarget());

    const size_t maxCallerThreadCount = GetFunctionMaxThreadCount(caller->_modifierList, caller->_location);
    const size_t maxThreadCount = GetFunctionMaxThreadCount(_modifierList, _location);

    const size_t callerThreadRate = caller->_threadRate;

    // Ensure store-and-forward semantics are honored
    // by not inlining if [[transaction_size]] is specified
    const bool hasTransactionSize = callNode->HasAttribute(ParseTreeTransactionSizeAttr);

    return !isInline && !isNoInline && !isExport && !isAsync && !isPipelined && !isExtern && !isExportClassMember &&
           !isNoBackpressure && !hasTransactionSize && maxThreadCount >= maxCallerThreadCount &&
           (_threadRate <= callerThreadRate);
}

bool FunctionNode::NoBackpressure() const { return _modifiers & ParseTreeFunctionModifierNoBackPressure; }

bool FunctionNode::ContainsReferenceParameters() const
{
    bool result = false;

    for (size_t i = 0; i < GetParameterCount(); i++)
    {
        if (ContainsReference(GetParameterDeclareNode(i)->GetDeclaredType()))
        {
            result = true;
            break;
        }
    }

    return result;
}

void FunctionNode::SetReturnNode(const ParseTreeNode* returnNode) { _returnNode = returnNode; }

// The statement list is reset to the original
// after each module is compiled
void FunctionNode::SetStatements(const ParseTreeNode* statements) { _statements = statements; }

FunctionDesc FunctionNode::GetFunctionDesc() const
{
    FunctionDesc fd = safe_cast<const FunctionType*>(GetType())->ToFunctionDesc();

    assert(fd._parameterNames.size() == GetParameterCount());

    for (size_t i = 0; i < GetParameterCount(); i++)
    {
        assert(fd._parameterNames[i] == GetParameterName(i));
    }

    fd._protectionModifier = _protectionModifier;

    return fd;
}

DeclareNode::DeclareNode(const ParseTreeNode* const type, ParseTreeNode* const name, const Scope& namespaceScope,
                         const uint32_t flags, const AssignNode* assignNode, const ParseTreeNode* const attributeList)
    : _typeNode(type), _name(name), _parseNamespaceScope(namespaceScope), _isConst(flags & DECLARE_FLAG_CONST),
      _isUninitConst(flags & DECLARE_FLAG_UNINIT_CONST), _shouldInitialize(flags & DECLARE_FLAG_INITIALIZE),
      _isParameter(flags & DECLARE_FLAG_PARAMETER), _isEndTransaction(flags & DECLARE_FLAG_END_TRANSACTION),
      _assignNode(nullptr), _attributeList(attributeList),
      _classType(nullptr), _containingFunction(nullptr),
      _memberProtectionModifier(ParseTreeMemberProtectionModifierPublic),
      _declarationScope(DECLARE_FLAG_GLOBAL == (flags & (DECLARE_FLAG_GLOBAL | DECLARE_FLAG_STATIC))
                            ? DeclarationScope::Global
                            : DeclarationScope::Local),
      _declarationAccess(flags & (DECLARE_FLAG_GLOBAL | DECLARE_FLAG_STATIC) ? DeclarationAccess::Shared
                                                                             : DeclarationAccess::Private),
      _staticInstanceId(0), _isModifiedParameter(false)
{
    if (assignNode)
    {
        SetAssignNode(assignNode);
    }
}

// Used to tell this node that the declaration is used in an assignment like:
// uint8 x = y + w;
// This can happen during construction, or after, but only once
void DeclareNode::SetAssignNode(const AssignNode* const assignRhs)
{
    assert(!_assignNode);

    _assignNode = assignRhs;
}

const AssignNode* DeclareNode::GetAssignNode() const { return _assignNode; }

void DeclareNode::MarkAsMember(const ParseTreeMemberProtectionModifier protectionModifier)
{
    assert(_declarationScope != DeclarationScope::Member);

    _declarationScope = DeclarationScope::Member;

    // Record if the member variable is public or private
    _memberProtectionModifier = protectionModifier;
}

ParseTreeMemberProtectionModifier DeclareNode::GetProtectionModifier() const
{
    // Only makes sense for member variables
    assert(_declarationScope == DeclarationScope::Member);

    return _memberProtectionModifier;
}

void DeclareNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    // Add a symbol to the symbol table - with an unknown initial value
    VisitContextPerVariableData symbolTableEntry = {};
    symbolTableEntry._declaration = this;

    context.AddSymbol(GetDeclaredName(), symbolTableEntry, _location);

    if (context._global._trackKnownValues)
    {
        // Try to update the symbol table if possible
        VisitCheckKnownValueContext knownValueContext(context);
        SetKnownValue(knownValueContext, nullptr);
    }

    const auto callback = [&]()
    {
        _typeNode->Visit(visitFunction, context);

        _name->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

void DeclareNode::SetClassType(const ClassType* const classType)
{
    assert(!_classType);
    _classType = classType;
}

void DeclareNode::SetInferredType(const Type* const type) const { _typeNode = g_compiler->Create<TypeNode>(type); }

void DeclareNode::SetContainingFunction(const FunctionNode* const function)
{
    assert(!_containingFunction);
    _containingFunction = function;
}

void DeclareNode::TypeCheck(TypeCheckContext& context) const
{
    // Save a pointer to the containing function
    _containingFunction = context._global._currentFunction;

    _typeNode->TypeCheck(context);

    // Determine what class this variable is a member of - the global class type will be return for flat functions
    // This is saved as a member for AllocateRegisters()
    _classType = context._global._classStack.top();

    const IdentifierNode* const identifierNode = dynamic_cast<const IdentifierNode*>(_name);

    const Type* type = GetDeclaredType();

    const bool isFunction = (nullptr != dynamic_cast<const FunctionType*>(type));

    const bool isMemberObject =
        (nullptr != dynamic_cast<const ClassType*>(type)) && (_classType != g_compiler->GetGlobalClassType());

    // Check for export class instances
    // SkipReferences is used to avoid marking an exported class as instantiated
    // if a reference to it is captured
    type->VisitTypes(
        [&](const Type* t)
        {
            ClassType* ct = dynamic_cast<ClassType*>((Type*)(t));

            if ((ct != nullptr) && (ct->IsExport()))
            {
                ct->IsInstantiated();
            }
        },
        VisitTypesBehavior::SkipReferences);

    // classes and memories cannot be local variables
    // strings can be parameters to inline functions
    if ((_declarationAccess != DeclarationAccess::Shared) && !type->IsPod())
    {
        g_compiler->ErrorStream(_location, CompileError::NonValueType)
            << "Class/Memory local variables are not supported";
    }

    if (IsStatic() && ContainsClassType() && (context._global._classStack.top() != g_compiler->GetGlobalClassType()))
    {
        g_compiler->ErrorStream(GetLocation(), CompileError::InvalidStaticLocal)
            << "Static local objects cannot be defined within class methods";
    }

    // _isUninitConst is used for the induction variable range for and unrolled_for
    // The variable is marked const, but is not initialized by the front-end
    if (_isConst && !_isParameter && !_assignNode && !_isUninitConst)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidConst)
            << "const variables must be assigned during declaration";
    }

    if (context._global._pass == TypeCheckPass::Functions)
    {
        // Do not add the variable to the symbol table in the 'functions' pass
        // because callbacks with initializers may refer to functions that have not yet been type checked
    }
    else if ((_declarationScope == DeclarationScope::Global) && (context._global._pass != TypeCheckPass::Globals))
    {
        // Global (non-static) variables are type-checked and added to the symbol table
        // in the 2nd pass.  Do not repeat that in the third pass (it will cause a duplicate symbol error).
        // Global variables are not registered in the first pass, in case the global is a callback
        // which is declared and assigned to a function that is not yet registered.
    }
    else
    {
        // Add the variable name to the symbol table
        TypeCheckData symbolTableEntry = {};

        symbolTableEntry._type = type;
        symbolTableEntry._value = KnownValue(); // initially mark the variable as not known at compile time

        context.AddSymbol(identifierNode->GetValue(), symbolTableEntry, _location);

        // type check the assignment node before calling SetKnownValue
        // SetKnownValue only works if the type of the rhs is known
        //
        // Do not type check the right-hand-side of member object declarations now
        // because that declaration could initialize callbacks to refer to
        // methods of other member objects which have not yet been declared
        // ClassNode will type check member object initializations later
        if (_assignNode && !isMemberObject)
        {
            _assignNode->TypeCheckImpl(context);
        }

        // Record the value of the variable if possible
        // Wrap the context into an object that can be passed into TryGetKnownValue
        TypeCheckKnownValueContext knownValueContext(context);

        SetKnownValue(knownValueContext, nullptr);

        _name->TypeCheck(context);
    }

    if (typeid(VoidType) == typeid(*type))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType) << "Variables must not have void type";
    }

    _scope = (_declarationScope == DeclarationScope::Global) ? context.GetNamespaceScope() : Scope();

    // Export classes must be contained within other classes (either as members or static locals)
    const ClassType* const classType = dynamic_cast<const ClassType*>(GetDeclaredType());

    if (classType && classType->IsExportIgnoreTarget() && (_declarationScope == DeclarationScope::Global))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidExportClass)
            << "Export class instances must be contained within classes";
    }
    _scope = (_declarationScope == DeclarationScope::Global) ? context.GetNamespaceScope() : Scope();

    const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(GetDeclaredType());

    if (memoryType && (ParseTreeMemoryTypeEcc == memoryType->GetMemoryType()) && _assignNode)
    {
        g_compiler->ErrorStream(_location, CompileError::EccUnsupported)
            << "ECC memories with initial values are not supported";
    }

    if (!_isConst && (_declarationScope == DeclarationScope::Global) &&
        GetCodeGenConfig()._enableDeprecationWarnings)
    {
        g_compiler->WarningStream(_location, CompileWarning::NonConstGlobal)
            << "Non-const global variables are deprecated, use class member variables instead";
    }

    // Shared strings cannot be initialized to a non-empty string
    // because there is no way to add those strings to the string table
    if (!_isConst && (DeclarationAccess::Shared == _declarationAccess) && _assignNode &&
        (g_compiler->GetStringType() == GetDeclaredType()))
    {
        const KnownValue kv =
            TryGetTypeCheckKnownValue(context, _assignNode->GetRhs(), _location, g_compiler->GetStringType());

        if (kv._type == KnownValueType::String)
        {
            if (!kv._stringVal.empty())
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidString)
                    << "Shared string initialized to non-empty string: " << kv._stringVal;
            }
        }
    }
}

bool DeclareNode::IsStatic() const
{
    return ((_declarationScope == DeclarationScope::Local) && (_declarationAccess == DeclarationAccess::Shared));
}

DeclareNode::DeclarationScope DeclareNode::GetDeclarationScope() const { return _declarationScope; }

const Type* DeclareNode::GetDeclaredType() const { return dynamic_cast<const TypeNode*>(_typeNode)->GetType(); }

const std::string& DeclareNode::GetDeclaredName() const
{
    return dynamic_cast<const IdentifierNode*>(_name)->GetValue();
}

// Appends namespace name to object name
const std::string DeclareNode::GetFlattenedName() const { return FlattenScopeAndAppendName(_scope, GetDeclaredName()); }

// Combines an object name with a member variable name to get a string representing the member variable of a particular
// object
std::string DeclareNode::GetCombinedName(const std::string& objectName) const
{
    return CombineObjectAndMemberName(_scope, objectName, GetDeclaredName());
}

std::string DeclareNode::GetCombinedParseNamespaceScopeName(const std::string& objectName) const
{
    return CombineObjectAndMemberName(_parseNamespaceScope, objectName, GetDeclaredName());
}

// For global objects, notify the class of the object's existence
// Member objects are registered recursively through RegisterObjects
void DeclareNode::RegisterObjects() const
{
    if (_declarationScope == DeclarationScope::Global)
    {
        ResolveFunctionsAndRegisterObjects(RegisterObjectsMode::RegisterObjects, GetFlattenedName(),
                                           ObjectPath(1, GetFlattenedName()), g_globalObjectName,
                                           DefaultObjectCallback);
    }
}

// For global and static objects
// Resolve callbacks and register or enumerate objects
void DeclareNode::ResolveFunctionsAndRegisterObjects(const RegisterObjectsMode mode, const std::string& objectName,
                                                     const ObjectPath& objectPath,
                                                     const std::string& containingObjectName,
                                                     const ObjectCallback& objectCallback) const
{
    // Static local objects inside of class methods are not supported
    assert(g_globalObjectName == containingObjectName);

    ObjectToClassCallableFunctionMap objectToFunctions;
    TryResolveInitializerListFunctions(objectName, g_globalObjectName, _assignNode ? _assignNode->GetRhs() : nullptr,
                                       objectToFunctions);

    GetDeclaredType()->RegisterObjects(mode, objectName, objectPath, objectCallback, DefaultDefaultInitializeCallback,
                                       objectToFunctions);
}

void DeclareNode::Rename(const std::string& newName) { dynamic_cast<IdentifierNode*>(_name)->Rename(newName); }

void DeclareNode::MarkModifiedParameter()
{
    assert(_isParameter);

    _isModifiedParameter = true;
}

void IdentifierNode::TypeCheck(TypeCheckContext& context) const
{
    SetType(context.LookupSymbol(Scope(), _value, _location)._type);
}

const std::string& IdentifierNode::GetValue() const { return _value; }

void IdentifierNode::Rename(const std::string& newName) { _value = newName; }

void LiteralStringNode::TypeCheck(TypeCheckContext& context) const { SetType(g_compiler->GetStringType()); }

const std::string& LiteralStringNode::GetValue() const { return _value; }

KnownValue LiteralStringNode::TryGetKnownValueImpl(KnownValueContext& context) const { return KnownValue(_value); }

void NodeList::TypeCheck(TypeCheckContext& context) const
{
    for (const ParseTreeNode* child : _nodes)
    {
        child->TypeCheck(context);
    }
}

void NodeList::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        for (const ParseTreeNode* child : _nodes)
        {
            child->Visit(visitFunction, context);
        }
    };

    visitFunction(this, callback);
}

void NodeList::Append(const ParseTreeNode* const node) { _nodes.push_back(node); }

// Removes nodes from a node list that are part of the supplied set
void NodeList::RemoveNodes(const std::set<const ParseTreeNode*>& nodesToRemove) const
{
    std::vector<const ParseTreeNode*> remaining;

    for (const ParseTreeNode* const node : _nodes)
    {
        if (!Contains(nodesToRemove, node))
        {
            remaining.push_back(node);
        }
    }

    std::swap(_nodes, remaining);
}

std::string NodeList::PrettyPrint() const
{
    std::string s = "";

    size_t i = 0;
    for (const ParseTreeNode* n : _nodes)
    {
        s += n->PrettyPrint();
        if (i != _nodes.size() - 1)
        {
            s += ",";
        }
        i++;
    }

    return s;
}

static size_t GetFifoDepthParam(IRContext& context, const ParseTreeNode* modifiersNode, const Location& location)
{
    const std::vector<const ParseTreeNode*>& modifiers = dynamic_cast<const NodeList*>(modifiersNode)->Children();
    bool fifoDepthSpecified = false;
    size_t minFifoDepth = 0;

    for (const ParseTreeNode* const modifier : modifiers)
    {
        auto attr = dynamic_cast<const IntAttributeNode*>(modifier);
        if (attr && attr->_attribute == ParseTreeFifoDepthAttr)
        {
            assert(!fifoDepthSpecified);

            const uint64_t fifoDepth = attr->_value;

            minFifoDepth = static_cast<size_t>(fifoDepth);

            fifoDepthSpecified = true;
        }
    }

    return minFifoDepth;
}

void LoopNode::TypeCheck(TypeCheckContext& context) const
{
    _body->TypeCheck(context);

    _modifiers->TypeCheck(context);

    CheckForMultipleAttributes(_modifiers, _location);
}

void LoopNode::VisitImpl(const VisitFunction& visitFunction, VisitContext& context) const
{
    _body->Visit(visitFunction, context);
}

size_t LoopNode::GetFifoDepthParam(IRContext& context) const
{
    return ::GetFifoDepthParam(context, _modifiers, _location);
}

ParseTreeLoopMode LoopNode::GetMode(const ParseTreeNode* const modifiers)
{
    std::list<ParseTreeLoopMode> modes;

    if (const auto list = dynamic_cast<const NodeList*>(modifiers))
    {
        for (const auto node : list->Children())
        {
            if (const auto modifier = dynamic_cast<const FunctionModifierNode*>(node))
            {
                if (modifier->_modifier == ParseTreeFunctionModifierUnordered)
                {
                    modes.push_back(ParseTreeLoopModeUnordered);
                }

                if (modifier->_modifier == ParseTreeFunctionModifierReorderByLooping)
                {
                    modes.push_back(ParseTreeLoopModeReorderByLooping);
                }
            }
        }
    }

    ParseTreeLoopMode mode = ParseTreeLoopModeOrdered;

    if (modes.size() == 0)
    {
        mode = ParseTreeLoopModeOrdered;
    }
    else if (modes.size() == 1)
    {
        mode = modes.front();
    }
    else
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidLoop)
            << "[[unordered]] and [[reorder_by_looping]] loop modifiers cannot be combined";
    }

    return mode;
}

void DoWhileLoopNode::TypeCheck(TypeCheckContext& context) const
{
    _conditionStatement->TypeCheck(context);

    const BoolType* const boolConditionType = dynamic_cast<const BoolType*>(_conditionStatement->GetType());

    if (!boolConditionType)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidLoop)
            << "Loop conditional expression must be boolean (uint1)";
    }

    LoopNode::TypeCheck(context);
}

void DoWhileLoopNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _conditionStatement->Visit(visitFunction, context);

        LoopNode::VisitImpl(visitFunction, context);
    };

    visitFunction(this, callback);
}

void RangeForLoopNode::TypeCheck(TypeCheckContext& context) const
{
    _declareNode->TypeCheck(context);

    _bound->TypeCheck(context);

    const LeafType* const declarationLeafType = dynamic_cast<const LeafType*>(_declareNode->GetDeclaredType());

    if (!declarationLeafType || (declarationLeafType->_baseType != BaseType::Uint))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType)
            << "for loop induction variable must be an unsigned integer";
    }

    if (!IsUnsignedOrNonNegative(context, _bound, _location))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType)
            << "for loop bound must be unsigned, or be known at compile time and non-negative";
    }

    if (_mode == ParseTreeLoopModeReorderByLooping)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidAttribute)
            << "unexpected [[reorder_by_looping]] with for loop";
    }

    LoopNode::TypeCheck(context);
}

void RangeForLoopNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _declareNode->Visit(visitFunction, context);

        _bound->Visit(visitFunction, context);

        LoopNode::VisitImpl(visitFunction, context);
    };

    visitFunction(this, callback);
}

void UnrolledForLoopNode::TypeCheck(TypeCheckContext& context) const
{
    _bound->TypeCheck(context);

    const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(_declareNode);

    const LeafType* const boundLeafType = dynamic_cast<const LeafType*>(_bound->GetType());

    if (!boundLeafType)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType) << "`static for` loop bound must be an integer";
    }

    const KnownValue bound = TryGetTypeCheckKnownValue(context, _bound, _location, _bound->GetType());

    if (bound._type != KnownValueType::Int)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidLoop)
            << "`static for` loop bound is not known by the compiler";
    }

    if (IsNegative(bound._intVal, boundLeafType))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidLoop) << "`static for` loop bound is negative";
    }

    // If the bound is 0, treat it as 1 for type checking on the index - to avoid undefined log2(0)
    const mp_int clampedBound = (bound._intVal == 0) ? 1 : bound._intVal;

    // std::max(1) to allocate 1 bit for loop with 1 iteration
    const size_t requiredBitCount = std::max<size_t>(1, MpToSizeT(Log2RoundUp(clampedBound)));

    const LeafType* const inductionVariableType = dynamic_cast<const LeafType*>(declareNode->GetDeclaredType());

    if (BaseType::Uint != inductionVariableType->_baseType)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType)
            << "`static for` loop variable must be an unsigned integer";
    }

    if (inductionVariableType->_width < requiredBitCount)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidLoop)
            << "`static for` loop variable is not wide enough (consider using auto).  Required bit count: "
            << requiredBitCount;
    }

    // This must happen after type inference
    _declareNode->TypeCheck(context);

    _body->TypeCheck(context);
}

void UnrolledForLoopNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    // If requested - determine the bounds of the loop and visit the body that many times
    size_t numLoopIterations = 1;

    if (context._global._trackKnownValues)
    {
        const KnownValue bound = TryGetVisitKnownValue(context, _bound, _bound->GetType());
        assert(bound._type == KnownValueType::Int);

        numLoopIterations = MpToSizeT(bound._intVal);
    }

    const auto callback = [&]()
    {
        _declareNode->Visit(visitFunction, context);

        _bound->Visit(visitFunction, context);

        // Visit the body N times
        const std::string& inductionVariableName = dynamic_cast<const DeclareNode*>(_declareNode)->GetDeclaredName();

        for (size_t loopIteration = 0; loopIteration < numLoopIterations; ++loopIteration)
        {
            if (context._global._trackKnownValues)
            {
                VisitContextPerVariableData variableData =
                    context.LookupSymbol(Scope(), inductionVariableName, _location);

                variableData._knownValue = KnownValue(loopIteration);

                context.UpdateSymbol(Scope(), inductionVariableName, variableData, _location);
            }

            _body->Visit(visitFunction, context);
        }
    };

    visitFunction(this, callback);
}

// Used during type inference
const Type* AssignNode::GetRhsType(TypeCheckContext& context) const
{
    _rhs->TypeCheck(context);

    return _rhs->GetType();
}

void AssignNode::TypeCheck(TypeCheckContext& context) const
{
    // For assignments that are a part of a declaration
    // TypeCheckImpl will be called as a part of the declaration type checking.
    // This leverages the logic in DeclareNode to determine which pass to perform type checking in.
    if (!_isInitialAssignment)
    {
        TypeCheckImpl(context);
    }
}

void AssignNode::TypeCheckImpl(TypeCheckContext& context) const
{
    // TypeCheckForAssignment is used to enable
    // situations where the type of a node depends on if it used as the destination of an assignment
    _lhs->TypeCheckForAssignment(context);

    const Type* const lhsType = _lhs->GetType();

    {
        // Store lhsType in the context
        // To help the rhs of the assignment to determine its type
        PushPopExpectedType pushPopExpectedType(context, lhsType);

        _rhs->TypeCheck(context);
    }

    const Type* const rhsType = _rhs->GetType();

    if (typeid(VoidType) == typeid(*rhsType))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidConversion) << "Assigning void value";
    }

    if (!_isInitialAssignment && !rhsType->IsPod())
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidAssignment)
            << "Assignment to string/memory/objects is not supported";
    }

    Type::TypeCheckConversion(lhsType, rhsType, _location);
}

void AssignNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _lhs->Visit(visitFunction, context);

        _rhs->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

// Used when visiting the AST nodes to track reference values
// Handles assignments of reference values
void AssignNode::HandleReferenceAssignment(const ResolveReferenceFunction& resolveReferenceCallback,
                                           const std::string& containingObjectName,
                                           const FunctionNode* const containingFunction,
                                           std::map<std::string, std::string>& referenceMap) const
{
    const Type* const rhsType = GetRhs()->GetType();

    if (ContainsReference(rhsType))
    {
        // Assignments to types containing references must be via variable access node
        const VariableAccessNode* const lhsVariableAccessNode = safe_cast<const VariableAccessNode*>(GetLhs());

        // Get a mapping of composite variable names (like s.f[3].x), to referenced object names.
        // Variable names are in terms of the lhs
        // For example: y = s;
        // then the resulting variable names will be y.f[3].x
        const std::map<std::string, std::string> references =
            GetRhs()->GetReferences(resolveReferenceCallback, lhsVariableAccessNode->GetName(), containingObjectName,
                                    containingFunction->GetClassType());

        for (const auto& p : references)
        {
            SafeInsert(referenceMap, p.first, p.second);
        }
    }
}

bool AssignNode::IsInitialAssignment() const { return _isInitialAssignment; }

void MuxOpNode::TypeCheck(TypeCheckContext& context) const
{
    _predicate->TypeCheck(context);

    _expressionList->TypeCheck(context);

    const Type* const predicateType = _predicate->GetType();

    const BoolType* predicateBoolType = dynamic_cast<const BoolType*>(predicateType);

    const LeafType* predicateLeafType = dynamic_cast<const LeafType*>(predicateType);

    if (predicateBoolType || (predicateLeafType && (predicateLeafType->_baseType == BaseType::Uint)))
    {
        // Valid
    }
    else
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidMux) << "MUX first operand must have bool or uint type";
    }

    const size_t expectedCount = 1ull << predicateType->GetBitWidth();

    const std::vector<const ParseTreeNode*>& expressionList =
        dynamic_cast<const NodeList*>(_expressionList)->Children();

    if (expressionList.size() != expectedCount)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidMux)
            << "Incorrect MUX expression list size, expected: " << expectedCount << " found: " << expressionList.size();
    }
    else
    {
        // the sign-extend mask is 64-bits, which only allows muxes of up to size 32
        // this is only enforced if some inputs are signed
        bool signedInput = false;

        for (const ParseTreeNode* const expr : expressionList)
        {
            if (IsSignedLeafType(expr->GetType()))
            {
                signedInput = true;
            }
        }

        if (signedInput && (expectedCount > 32))
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidMux) << "MUX is too large";
        }

        const Type* const firstType = expressionList[0]->GetType();

        for (size_t i = 1; i < expressionList.size(); ++i)
        {
            // Type must be convertible to the first type
            Type::TypeCheckConversion(firstType, expressionList[i]->GetType(), _location);
        }

        const LeafType* const firstLeafType = dynamic_cast<const LeafType*>(firstType);

        if (firstLeafType)
        {
            // Compute the smallest type that can represent all expressions
            const LeafType* finalType = firstLeafType;

            for (const ParseTreeNode* const node : expressionList)
            {
                const Type* const expressionType = node->GetType();

                const LeafType* leafType = dynamic_cast<const LeafType*>(expressionType);

                if (leafType)
                {
                    if ((leafType->_baseType == BaseType::Int) || (finalType->_baseType == BaseType::Int))
                    {
                        // Result is signed
                        leafType = ToSigned(leafType, _location);

                        finalType = ToSigned(finalType, _location);
                    }

                    const size_t width = std::max(leafType->GetBitWidth(), finalType->GetBitWidth());

                    finalType = g_compiler->GetLeafType(finalType->_baseType, width, _location);
                }
                else
                {
                    // TypeCheckConversion will have already failed
                }
            }

            SetType(finalType);
        }
        else
        {
            SetType(firstType);
        }
    }
}

void MuxOpNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _predicate->Visit(visitFunction, context);

        _expressionList->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

void BinaryOpNode::TypeCheck(TypeCheckContext& context) const
{
    _lhs->TypeCheck(context);

    _rhs->TypeCheck(context);

    const LeafType* const lhsLeafType = dynamic_cast<const LeafType*>(_lhs->GetType());

    const LeafType* const rhsLeafType = dynamic_cast<const LeafType*>(_rhs->GetType());

    const BoolType* const lhsBoolType = dynamic_cast<const BoolType*>(_lhs->GetType());

    const BoolType* const rhsBoolType = dynamic_cast<const BoolType*>(_rhs->GetType());

    const FloatType* const lhsFloatType = dynamic_cast<const FloatType*>(_lhs->GetType());

    const FloatType* const rhsFloatType = dynamic_cast<const FloatType*>(_rhs->GetType());

    const Type* type = nullptr;

    if (lhsBoolType && rhsBoolType)
    {
        switch (_opType)
        {
        case ParseTreeBinaryOpTypeLogicalAnd:
        case ParseTreeBinaryOpTypeLogicalOr:
        case ParseTreeBinaryOpTypeLogicalXor:
        case ParseTreeBinaryOpTypeEQ:
        case ParseTreeBinaryOpTypeNE:
            type = g_compiler->GetBoolType();
            break;

        default:
            g_compiler->ErrorStream(_location, CompileError::InvalidType)
                << "Unsupported binary operation on boolean types";
        }
    }
    else if (lhsLeafType && rhsLeafType)
    {
        LeafType lhsModifiedType = *lhsLeafType;
        LeafType rhsModifiedType = *rhsLeafType;

        switch (_opType)
        {
        case ParseTreeBinaryOpTypeShl:
        case ParseTreeBinaryOpTypeShr:
            // Signed rhs is OK, it is reinterpreted as unsigned (same width)
            break;

        case ParseTreeBinaryOpTypeGT:
        case ParseTreeBinaryOpTypeGE:
        case ParseTreeBinaryOpTypeLT:
        case ParseTreeBinaryOpTypeLE:
        case ParseTreeBinaryOpTypeEQ:
        case ParseTreeBinaryOpTypeNE:
        case ParseTreeBinaryOpTypeAdd:
        case ParseTreeBinaryOpTypeSub:
        case ParseTreeBinaryOpTypeAnd:
        case ParseTreeBinaryOpTypeOr:
        case ParseTreeBinaryOpTypeXor:
            if (lhsModifiedType._baseType != rhsModifiedType._baseType)
            {
                // 1 operand is signed and the other is unsigned
                // Convert both to signed (adding bits to ensure that no data is lost)
                ToSigned(lhsModifiedType);
                ToSigned(rhsModifiedType);
            }

            assert(lhsModifiedType._baseType == rhsModifiedType._baseType);
            break;

        case ParseTreeBinaryOpTypeMul:
        case ParseTreeBinaryOpTypeLutMul:
            // The math of mul does not require unsigned operands to be converted to signed
            // For example: uint8 * int8 = int16
            break;

        case ParseTreeBinaryOpTypeDiv:
        case ParseTreeBinaryOpTypeMod:
            if (rhsLeafType->_baseType != BaseType::Uint)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidType)
                    << "Denominator of / % operations must be unsigned";
            }
            break;

        default:
            g_compiler->ErrorStream(_location, CompileError::InvalidType) << "Operation is not supported on integers";
        }

        BaseType baseType = lhsModifiedType._baseType;

        // mp_int is used here to avoid overflow in intermediate calculations
        // Overflow is checked at the end
        mp_int bitCount = 0;

        switch (_opType)
        {
        case ParseTreeBinaryOpTypeSub:
            // Sub always produces a signed value to ensure that the result of the subtraction is properly sign-extended
            // when it is used in other operations.
            // An example failing case if uint - uint produces a uint:
            // (uint<32>(4) - uint<3>(5)) + uint2(2) = this should produce 1: (4-5+2)
            // If uint<32> - uint<3> produces a uint<33> however, then this results
            // (uint<32>(4) - uint<3>(5)) + uint2(2) =
            // uint<33>(111111111111111111111111111111111)  + uint2(2) =
            // uint<34>(0111111111111111111111111111111111) + uint2(2) =
            // uint<34>(100000000000000000000000000000001)
            baseType = BaseType::Int;
        case ParseTreeBinaryOpTypeAdd:
            bitCount = mp_int(std::max(lhsModifiedType._width, rhsModifiedType._width)) + 1;
            break;

        case ParseTreeBinaryOpTypeAnd:
        case ParseTreeBinaryOpTypeOr:
        case ParseTreeBinaryOpTypeXor:
            bitCount = std::max(lhsModifiedType._width, rhsModifiedType._width);
            break;

        case ParseTreeBinaryOpTypeShl:
            bitCount = lhsModifiedType._width + ((mp_int(1) << rhsModifiedType._width) - 1);
            break;

        case ParseTreeBinaryOpTypeShr:
            bitCount = lhsModifiedType._width;
            break;

        case ParseTreeBinaryOpTypeEQ:
        case ParseTreeBinaryOpTypeNE:
        case ParseTreeBinaryOpTypeGT:
        case ParseTreeBinaryOpTypeGE:
        case ParseTreeBinaryOpTypeLT:
        case ParseTreeBinaryOpTypeLE:
            type = g_compiler->GetBoolType();
            break;

        case ParseTreeBinaryOpTypeMul:
        case ParseTreeBinaryOpTypeLutMul:
            bitCount = mp_int(lhsModifiedType._width) + mp_int(rhsModifiedType._width);

            // If either operand is signed, the result is signed
            baseType = (lhsModifiedType._baseType == BaseType::Uint) && (rhsModifiedType._baseType == BaseType::Uint)
                           ? BaseType::Uint
                           : BaseType::Int;
            break;

        case ParseTreeBinaryOpTypeDiv:
            bitCount = lhsModifiedType._width;
            break;

        case ParseTreeBinaryOpTypeMod:
            if (rhsModifiedType._width < 1)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidLiteral)
                    << "Right hand size of % operation must be >= 1";
            }

            if (lhsModifiedType._baseType == BaseType::Uint)
            {
                baseType = BaseType::Uint;
                bitCount = rhsModifiedType._width;
            }
            else
            {
                baseType = BaseType::Int;
                bitCount = rhsModifiedType._width;
            }
            break;

        default:
            g_compiler->ErrorStream(_location, CompileError::InvalidType) << "Operation is not supported on integers";
        }

        if (!type)
        {
            if (bitCount > c_maxRegisterBitWidth)
            {
                if (_frontEndType)
                {
                    // Either the front-end determiend that a narrower type could be used
                    // or the check against c_maxRegisterBitWidth at the end of this function will catch the wide type.
                    type = _frontEndType;
                }
                else
                {
                    g_compiler->ErrorStream(_location, CompileError::TypeToWide)
                        << "Maximum type width exceeded with type of width: " << GetType()->GetBitWidth() << "\n";
                    throw std::runtime_error("Type too wide");
                }
            }
        }
    }
    else if (lhsFloatType && rhsFloatType)
    {
        switch (_opType)
        {
        case ParseTreeBinaryOpTypeAdd:
        case ParseTreeBinaryOpTypeSub:
        case ParseTreeBinaryOpTypeMul:
            assert(32 == lhsFloatType->GetBitWidth());
            assert(32 == rhsFloatType->GetBitWidth());
            break;

        default:
            g_compiler->ErrorStream(_location, CompileError::InvalidType)
                << "Operation is not supported on floating point numbers";
        }

        type = g_compiler->GetFloatType(32);
    }
    else
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType)
            << "Binary operations are only supported on scalar types";
    }

    SetType(type);

    if (GetType() && (GetType()->GetBitWidth() > c_maxRegisterBitWidth))
    {
        g_compiler->ErrorStream(_location, CompileError::TypeToWide)
            << "Maximum type width exceeded with type of width: " << GetType()->GetBitWidth() << "\n";
        throw std::runtime_error("Type too wide");
    }
}

void BinaryOpNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _lhs->Visit(visitFunction, context);

        _rhs->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

std::string BinaryOpNode::PrettyPrint() const
{
    std::string str = "(" + _lhs->PrettyPrint() + " ";

    switch (_opType)
    {
    case ParseTreeBinaryOpTypeAdd:
        str += "+";
        break;
    case ParseTreeBinaryOpTypeSub:
        str += "-";
        break;
    case ParseTreeBinaryOpTypeMul:
    case ParseTreeBinaryOpTypeLutMul:
        str += "*";
        break;
    case ParseTreeBinaryOpTypeDiv:
        str += "/";
        break;
    case ParseTreeBinaryOpTypeMod:
        str += "%";
        break;
    case ParseTreeBinaryOpTypeAnd:
        str += "&";
        break;
    case ParseTreeBinaryOpTypeLogicalAnd:
        str += "&&";
        break;
    case ParseTreeBinaryOpTypeOr:
        str += "|";
        break;
    case ParseTreeBinaryOpTypeLogicalOr:
        str += "||";
        break;
    case ParseTreeBinaryOpTypeXor:
        str += "^";
        break;
    case ParseTreeBinaryOpTypeLogicalXor:
        str += "^^";
        break;
    case ParseTreeBinaryOpTypeShl:
        str += "<<";
        break;
    case ParseTreeBinaryOpTypeShr:
        str += ">>";
        break;
    case ParseTreeBinaryOpTypeEQ:
        str += "==";
        break;
    case ParseTreeBinaryOpTypeNE:
        str += "!=";
        break;
    case ParseTreeBinaryOpTypeGT:
        str += ">";
        break;
    case ParseTreeBinaryOpTypeGE:
        str += ">=";
        break;
    case ParseTreeBinaryOpTypeLT:
        str += "<";
        break;
    case ParseTreeBinaryOpTypeLE:
        str += "<=";
        break;
    default:
        assert(false);
    }

    str += " " + _rhs->PrettyPrint() + ")";

    return str;
}

void UnaryOpNode::TypeCheck(TypeCheckContext& context) const
{
    _expression->TypeCheck(context);

    const LeafType* const leafType = dynamic_cast<const LeafType*>(_expression->GetType());

    const BoolType* const boolType = dynamic_cast<const BoolType*>(_expression->GetType());

    const FloatType* const floatType = dynamic_cast<const FloatType*>(_expression->GetType());

    switch (_opType)
    {
    case ParseTreeUnaryOpTypeInvert:
        if (!leafType)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidType)
                << "The invert operator applies to integer types only";
        }

        SetType(leafType);
        break;

    case ParseTreeUnaryOpTypeNegate:
    {
        if (leafType)
        {
            // Convert the expression type to signed and add 1 bit to account for the +1 that will occur
            SetType(g_compiler->GetLeafType(BaseType::Int, leafType->_width + 1, _location));
        }
        else if (floatType)
        {
            SetType(floatType);
        }
        else
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidType)
                << "The negate operator applies to integer and float types only";
        }
    }
    break;

    case ParseTreeUnaryOpTypeLogicalInvert:
        if (!boolType)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidType)
                << "The logical invert operator applies to boolean types only";
        }

        SetType(boolType);
        break;

    default:
        assert(false);
    }
}

void UnaryOpNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _expression->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

std::string UnaryOpNode::PrettyPrint() const
{
    std::string str;

    switch (_opType)
    {
    case ParseTreeUnaryOpTypeInvert:
        str = "~";
        break;
    case ParseTreeUnaryOpTypeNegate:
        str = "-";
        break;
    case ParseTreeUnaryOpTypeLogicalInvert:
        str = "!";
        break;
    default:
        assert(false);
    }
    str += _expression->PrettyPrint();

    return str;
}

const Type* ReturnNode::GetReturnType() const
{
    assert(_returnType);

    return _returnType;
}

std::string ReturnNode::PrettyPrint() const
{
    std::ostringstream str;
    str << "return";

    if (_value)
    {
        str << " " << _value->PrettyPrint();
    }

    return str.str();
}

void ReturnNode::TypeCheck(TypeCheckContext& context) const
{
    // Record the number of return nodes in the current function
    ++context._global._returnCount;

    // _value is null for statements that return nothing
    if (_value)
    {
        {
            // Push the function return type onto the expect type stack
            // so that the value type can be context dependent
            PushPopExpectedType pushPopExpectedType(context, context._global._functionReturnType);

            _value->TypeCheck(context);
        }

        const Type* const type = _value->GetType();

        Type::TypeCheckConversion(context._global._functionReturnType, type, _location);

        _returnType = type;

        if (dynamic_cast<const VoidType*>(type) != nullptr)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidReturn) << "return void is not allowed";
        }

        // Link the containing function to the return node
        assert(context._global._currentFunction);
        const_cast<FunctionNode*>(context._global._currentFunction)->SetReturnNode(this);
    }
    else
    {
        if (dynamic_cast<const VoidType*>(context._global._functionReturnType) == nullptr)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidReturn) << "non-void function returning void";
        }

        _returnType = g_compiler->GetVoidType();
    }
}

void ReturnNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        if (_value)
        {
            _value->Visit(visitFunction, context);
        }
    };

    visitFunction(this, callback);
}

std::map<std::string, std::string> ReturnNode::GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                             const std::string& resultBaseName,
                                                             const std::string& thisObjectName,
                                                             const ClassType* const thisClassType) const
{
    std::map<std::string, std::string> result;

    if (_value)
    {
        result = _value->GetReferences(resolveReferenceCallback, resultBaseName, thisObjectName, thisClassType);
    }

    return result;
}

void CastNode::TypeCheck(TypeCheckContext& context) const
{
    _castType->TypeCheck(context);

    const Type* const dst = _castType->GetType();

    {
        // Push expected type to help determine the type of _value
        // For example, if _value is an initializer list
        PushPopExpectedType pushPopExpectedType(context, dst);

        _value->TypeCheck(context);
    }

    const Type* const src = _value->GetType();

    if (src->GetBitWidth() == dst->GetBitWidth())
    {
        // If bit widths match, then the cast will work
    }
    else
    {
        Type::TypeCheckConversion(dst, src, _location);
    }

    // assigned in constructor
    // Overwritten here for type inference
    SetType(dst);
}

void CastNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _castType->Visit(visitFunction, context);

        _value->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

KnownValue CastNode::TryGetKnownValueImpl(KnownValueContext& context) const
{
    return _value->TryGetKnownValue(context, GetType());
}

void IfNode::TypeCheck(TypeCheckContext& context) const
{
    _condition->TypeCheck(context);

    _lhs->TypeCheck(context);

    if (_rhs)
    {
        _rhs->TypeCheck(context);
    }

    const BoolType* const boolConditionType = dynamic_cast<const BoolType*>(_condition->GetType());

    if (!boolConditionType)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType) << "If conditional expression must be boolean";
    }

    // If the condition value is known at compile time, then the if is ordered
    const KnownValue conditionValue = TryGetTypeCheckKnownValue(context, _condition, _location, _condition->GetType());
}

void IfNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _condition->Visit(visitFunction, context);

        // If condition is known at compile time, don't visit the child node that will not be executed
        const KnownValue conditionValue = TryGetVisitKnownValue(context, _condition, g_compiler->GetBoolType());

        const bool visitLhs = (conditionValue._type != KnownValueType::Int) || (conditionValue._intVal == 1);
        const bool visitRhs = (conditionValue._type != KnownValueType::Int) || (conditionValue._intVal != 1);

        if (visitLhs)
        {
            _lhs->Visit(visitFunction, context);
        }

        if ((_rhs != nullptr) && visitRhs)
        {
            _rhs->Visit(visitFunction, context);
        }
    };

    visitFunction(this, callback);
}

void UpdateRateModifierNode::TypeCheck(TypeCheckContext& context) const
{
    if (_updateRate == 0)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidUpdateRate) << "[[schedule]] attribute cannot be zero";
        throw std::runtime_error("Invalid update rate");
    }
}

UpdateRateModifierNode::ModifiedUpdateRate UpdateRateModifierNode::GetModifiedUpdateRate(const bool hasMemoryRmw) const
{
    ModifiedUpdateRate result = {};

    // If the update rate is high enough, then we have an option to optimize
    // memories written within an atomic block area, by not instantiating
    // bypass HW and scheduling fewer stages, depending on the fact that
    // hazards are guaranteed to be spaced far enough apart. By default we
    // optimize for Fmax by using bypass HW and maximum number of stages.

    const auto optimizeForArea = GetCodeGenConfig()._rmwMemoryFavorArea;

    assert(GetCodeGenConfig().GetMaxBypassSlots() > 0);
    const size_t threshold = GetCodeGenConfig().GetMaxBypassSlots() - 1;

    if (hasMemoryRmw && (_updateRate > threshold) && optimizeForArea)
    {
        result._updateRate = _updateRate - threshold;
        result._needsBypass = false;
    }
    else
    {
        result._updateRate = _updateRate;
        result._needsBypass = true;
    }

    return result;
}

size_t UpdateRateModifierNode::GetUnmodifiedUpdateRate() const { return _updateRate; }

void AtomicNode::TypeCheck(TypeCheckContext& context) const
{
    // Look for do-while loop inside atomic
    bool hasLoopNode = false;
    {
        size_t atomicDepth = 0;
        const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
        {
            const DoWhileLoopNode* const loopNode = dynamic_cast<const DoWhileLoopNode*>(node);

            if (loopNode)
            {
                if (atomicDepth > 1)
                {
                    g_compiler->ErrorStream(node->GetLocation(), CompileError::InvalidAtomic)
                        << "Atomic do-while loop cannot be nested in another atomic/schedule";
                    throw std::runtime_error("Invalid atomic block");
                }

                if (hasLoopNode)
                {
                    g_compiler->ErrorStream(node->GetLocation(), CompileError::InvalidAtomic)
                        << "Multiple do-while loops inside atomic";
                    throw std::runtime_error("Invalid atomic block");
                }

                hasLoopNode = true;
            }

            const AtomicNode* const atomicNode = dynamic_cast<const AtomicNode*>(node);
            if (atomicNode)
            {
                atomicDepth++;
            }
            recurseCallback();
            if (atomicNode)
            {
                atomicDepth--;
            }
        };

        VisitContext visitContext = {};
        VisitContext::PushPopScope outerScope(visitContext);
        _body->Visit(callback, visitContext);
    }

    if (hasLoopNode)
    {
        // Update rate must be 1 for atomic do-while
        const size_t updateRate = _updateRate.GetUnmodifiedUpdateRate();
        if (updateRate != 1)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidAtomic)
                << "Do-while loop inside schedule block must have update rate of 1";
            throw std::runtime_error("Invalid atomic block");
        }

        // Check for operations outside of do-while loop
        bool inLoopNode = false;
        const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
        {
            const DoWhileLoopNode* const loopNode = dynamic_cast<const DoWhileLoopNode*>(node);

            if (loopNode)
            {
                inLoopNode = true;
            }

            // Check for invalid nodes outside of do-while
            if (!inLoopNode)
            {
                const AtomicNode* const atomicNode = dynamic_cast<const AtomicNode*>(node);
                const NestedScopeNode* const nestedScopeNode = dynamic_cast<const NestedScopeNode*>(node);
                const NodeList* const nodeList = dynamic_cast<const NodeList*>(node);

                if (!atomicNode && !nestedScopeNode && !nodeList)
                {
                    g_compiler->ErrorStream(node->GetLocation(), CompileError::InvalidAtomic)
                        << "Invalid operation in atomic outside of do-while loop";
                    throw std::runtime_error("Invalid atomic block");
                }
            }

            recurseCallback();

            if (loopNode)
            {
                inLoopNode = false;
            }
        };

        VisitContext visitContext = {};
        VisitContext::PushPopScope outerScope(visitContext);
        _body->Visit(callback, visitContext);
    }

    _body->TypeCheck(context);

    _updateRate.TypeCheck(context);
}

void AtomicNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _body->Visit(visitFunction, context);

        _updateRate.Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

static size_t GetSizeofResult(const size_t input, const ParseTreeSizeofType sizeofType, const Location& location)
{
    size_t result = 0;

    switch (sizeofType)
    {
    case ParseTreeSizeofTypeBit:
        result = input;
        break;

    case ParseTreeSizeofTypeByte:
        if (0 != (input % 8))
        {
            g_compiler->ErrorStream(location, CompileError::InvalidType)
                << "bytesizeof/byteoffsetof require a bit count that is a multiple of 8.  Actual bit count: " << input;
        }

        result = input / 8;
        break;

    default:
        assert(false);
    }

    return result;
}

void SizeOfNode::TypeCheck(TypeCheckContext& context) const
{
    _expression->TypeCheck(context);

    const Type* const expressionType = _expression->GetType();

    if (0 == expressionType->GetBitWidth())
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType)
            << "Sizeof operations are not allowed on void/object types";
    }

    _result = GetResult(context);

    SetType(g_compiler->GetLeafType(BaseType::Uint, GetIntegerBitCount(_result), _location));
}

void SizeOfNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _expression->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

size_t SizeOfNode::GetResult(TypeCheckContext& context) const
{
    const Type* const expressionType = _expression->GetType();

    // Internally, strings are treated as integers
    // For the purposes of sizeof* operators, strings have 0 width
    const size_t bitCount = g_compiler->GetStringType() == expressionType ? 0 : expressionType->GetBitWidth();

    return GetSizeofResult(bitCount, _sizeofType, _location);
}

const ParseTreeNode* NestedScopeNode::GetBody() const { return _body; }

void NestedScopeNode::TypeCheck(TypeCheckContext& context) const
{
    // New scope variables
    TypeCheckContext::PushPopScope pushScope(context);

    _body->TypeCheck(context);
}

void NestedScopeNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _body->Visit(visitFunction, context); };

    VisitContext::PushPopScope pushScope(context);

    visitFunction(this, callback);
}

// For cat.run() - returns cat()
// Returns null if there is no explict object specified
const ParseTreeNode* CallNode::GetObject() const { return _object; }

void CallNode::TypeCheck(TypeCheckContext& context) const
{
    // Save a pointer to the containing function
    if (!context._global._currentFunction)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidCall)
            << "function calls must be contained inside of functions";
    }

    _containingFunction = context._global._currentFunction;

    // Don't type check the name, that will fail when it tries to lookup the name in the symbol table
    _modifiers->TypeCheck(context);

    CheckForMultipleAttributes(_modifiers, _location);

    _calledClassType = g_compiler->GetGlobalClassType();

    if (_object)
    {
        _object->TypeCheck(context);
    }

    // Get the class that contains the function where the call site is located
    _callSiteClassType = context._global._classStack.top();

    _callSiteScope = context.GetNamespaceScope();

    // Determine information about the called function
    // No need to resolve the function node
    const ResolvedCall resolvedCall =
        ResolveFunctionCallForTypeChecking(this, _callSiteClassType, context.GetNamespaceScope());

    // _targetScope is the namespace where the flat function exists
    _targetScope = resolvedCall._flatFunctionScope;

    _calledClassType = resolvedCall._calledClassType;

    if (_object)
    {
        // Lookup the class associated with the object
        const Type* const objectType = _object->GetType();

        if (!_calledClassType)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                << "Left of . in a method call must be a class instance";
            return;
        }
    }
    const FunctionDesc& functionDesc = resolvedCall._functionDesc;

    // Allocate instances for functions
    assert(!context._global._classStack.empty());

    if (ParseTreeMemberProtectionModifierPublic != functionDesc._protectionModifier)
    {
        // Private member function, verify access is OK
        assert(ParseTreeMemberProtectionModifierPrivate == functionDesc._protectionModifier);

        if ((_calledClassType != _callSiteClassType) && !resolvedCall._callByReference)
        {
            g_compiler->ErrorStream(_location, CompileError::ProtectionModifier) << "Calling private member function";
        }
    }

    if (functionDesc._modifiers & ParseTreeFunctionModifierExport)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidCall) << "Exported functions cannot be called";
    }

    // CallNode modifiers
    const std::vector<const ParseTreeNode*>& modifiers = dynamic_cast<const NodeList*>(_modifiers)->Children();
    bool hasTransactionSize = false;
    for (const ParseTreeNode* const modifier : modifiers)
    {
        auto attr = dynamic_cast<const IntAttributeNode*>(modifier);

        if (attr->_attribute == ParseTreeTransactionSizeAttr)
        {
            if (attr->_value == 0)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidCall) << "Transaction size cannot be 0";
            }

            if (!functionDesc._isLastParameterIndex)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                    << "Transaction size specified without [[last]] argument";
            }

            hasTransactionSize = true;
        }
    }

    // Inline calls cannot have modifiers
    if (functionDesc._modifiers & ParseTreeFunctionModifierInline)
    {
        if (!modifiers.empty())
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                << "Calls to inline functions cannot have modifiers";
        }

        if (hasTransactionSize)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                << "[[transaction_size]] cannot be used when calling an inline function";
        }
    }

    // Extern functions are not supported with transaction size modifier
    if (functionDesc._modifiers & ParseTreeFunctionModifierExternalFixedLatency)
    {
        if (hasTransactionSize)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                << "Calls to fixed latency extern function cannot have [[transaction_size]] modifier";
        }
    }

    const std::vector<const ParseTreeNode*>& arguments = GetArguments();

    const std::string combinedName = FlattenScopeAndAppendName(_targetScope, _scopedName->GetName());

    if (functionDesc._hasVariableArguments)
    {
        assert(functionDesc._parameterTypes.empty());

        // Per-argument type-checking will not run in the loop below
        _args->TypeCheck(context);

        if (arguments.size() < functionDesc._parameterTypes.size())
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                << "Argument(" << arguments.size() << ")/Parameter(" << functionDesc._parameterTypes.size()
                << ") count mismatch for: " << combinedName << functionDesc.GetParameterList();
        }
    }
    else
    {
        if (arguments.size() != functionDesc._parameterTypes.size())
        {
            ErrStream errStream = g_compiler->ErrorStream(_location, CompileError::InvalidCall);
            g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                << "Argument(" << arguments.size() << ")/Parameter(" << functionDesc._parameterTypes.size()
                << ") count mismatch for: " << combinedName << functionDesc.GetParameterList();
        }
    }

    // Warn on narrowing conversions for thread count - this is a common bug
    const bool isPipelined = functionDesc._modifiers & ParseTreeFunctionModifierPipelined;

    const bool isInline = functionDesc._modifiers & ParseTreeFunctionModifierInline;

    for (size_t i = 0; i < functionDesc._parameterTypes.size(); ++i)
    {
        if (i < arguments.size())
        {
            const Type* const parameterType = functionDesc._parameterTypes[i];

            // Set the expected type of the argument, to enable the type of the argument to be inferred if necessary
            {
                PushPopExpectedType pushPopExpectedType(context, parameterType);

                arguments[i]->TypeCheck(context);
            }

            const Type* const argumentType = arguments[i]->GetType();

            Type::TypeCheckConversion(parameterType, argumentType, _location);
        }
    }

    if (functionDesc._isLastParameterIndex)
    {
        const ParseTreeNode* lastArg = arguments[*functionDesc._isLastParameterIndex];

        const KnownValue knownArgValue = TryGetTypeCheckKnownValue(context, lastArg, _location, lastArg->GetType());
        if (knownArgValue._type == KnownValueType::Int)
        {
            if (knownArgValue._intVal == 0)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                    << "[[last]] argument cannot have constant false value";
            }
        }
    }

    // Inline pipelined calls have more restrictions on thread count
    if (isPipelined && isInline)
    {
        const KnownValue threadCount =
            TryGetTypeCheckKnownValue(context, arguments[0], _location, arguments[0]->GetType());

        if (threadCount._type == KnownValueType::Int)
        {
            // Check to see if the thread count is too large to be represented by the 1st parameter
            // Note that in the unknown thread count case, this is OK.  Upper bits are ignored
            const LeafType* const threadCountType = dynamic_cast<const LeafType*>(functionDesc._parameterTypes[0]);
            assert(threadCountType->_baseType == BaseType::Uint);

            const size_t maxThreadCount = 1ull << threadCountType->_width;

            if (threadCount._intVal > maxThreadCount)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                    << "Thread count specified: " << threadCount._intVal
                    << "cannot be represented by the thread id/count parameter.  Max allowable =  " << maxThreadCount;
            }

            if (threadCount._intVal == 0)
            {
                // non-inline functions can handle thread count = 0
                // but inline ones cannot
                g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                    << "Thread count for inline pipelined call must be positive";
            }
        }
        else
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidCall)
                << "Thread count for inline pipelined call must be known at compile time";
        }
    }

    const ParseTreeFunctionModifier inlineIntrinsicMask =
        static_cast<ParseTreeFunctionModifier>(ParseTreeFunctionModifierInline | ParseTreeFunctionModifierIntrinsic);

    if (inlineIntrinsicMask == (functionDesc._modifiers & inlineIntrinsicMask))
    {
        if ("__debug_view" == GetName())
        {
            const std::vector<const ParseTreeNode*>& args = dynamic_cast<const NodeList*>(_args)->Children();

            if (args.empty())
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidDebugView)
                    << "__debug_view must be passed 1 argument (label string)";
            }
            else if (g_compiler->GetStringType() != args[0]->GetType())
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidDebugView)
                    << "First argument passed to __debug_view must be a string";
            }

            // The remaining arguments are (name, values) pairs to debug inserted into the debug view
            for (size_t i = 1; i < args.size(); i += 2)
            {
                if ((i + 1) == args.size())
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidDebugView)
                        << "__debug_view arguments must have the form (label, name1, value1, name2, value2) - value "
                           "missing";
                    break;
                }

                if (g_compiler->GetStringType() != args[i]->GetType())
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidDebugView)
                        << "__debug_view arguments must have the form (label, name1, value1, name2, value2) - expecting "
                           "a string name";
                    break;
                }

                if (g_compiler->GetStringType() == args[i + 1]->GetType())
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidDebugView)
                        << "__debug_view arguments must have the form (label, name1, value1, name2, value2) - expecting "
                           "a non-string value";
                    break;
                }
            }
        }
    }

    if (isPipelined && (functionDesc._returnType != g_compiler->GetVoidType()))
    {
        // Pipelined function with non-void return type
        const KnownValue threadCount =
            TryGetTypeCheckKnownValue(context, arguments[0], _location, arguments[0]->GetType());

        if (threadCount._type == KnownValueType::Int)
        {
            // Thread count is known at compile time, therefore the return type is known
            SetType(g_compiler->GetArrayType(functionDesc._returnType, MpToSizeT(threadCount._intVal),
                                             ParseTreeArrayTypeDefault, ParseTreeMemoryTypeDefault, nullptr, false,
                                             _location));
        }
        else
        {
            SetType(g_compiler->GetVoidType());

            if (!context._global._expectedTypeStack.empty())
            {
                // Infer the result type from the context
                const Type* const expectedType = context._global._expectedTypeStack.top();

                const ArrayType* const expectedArrayType = dynamic_cast<const ArrayType*>(expectedType);

                if (expectedArrayType)
                {
                    if (expectedArrayType->_elementType == functionDesc._returnType)
                    {
                        // Array element type matches, accept the array size from the context
                        SetType(expectedArrayType);
                    }
                }
            }

            // In the case that the call site ignores the return value, it is OK to leave with _type = void
        }
    }
    else
    {
        SetType(functionDesc._returnType);
    }
}

const std::vector<const ParseTreeNode*>& CallNode::GetArguments() const
{
    return dynamic_cast<const NodeList*>(_args)->Children();
}

void CallNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        if (_object)
        {
            _object->Visit(visitFunction, context);
        }

        _scopedName->Visit(visitFunction, context);

        _args->Visit(visitFunction, context);

        _modifiers->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

std::string CallNode::GetName() const { return _scopedName->GetName(); }

const ScopedIdentifierNode* CallNode::GetScopedName() const { return _scopedName; }

const std::string CallNode::GetFlattenedName() const
{
    // _targetScope is non-empty only for non-member function calls
    return FlattenScopeAndAppendName(_targetScope, _scopedName->GetName());
}

const FunctionNode* CallNode::GetContainingFunction() const
{
    assert(_containingFunction);
    return _containingFunction;
}

// Must be called after type checking
const ClassType* CallNode::GetCalledClassType() const
{
    assert(_calledClassType);

    return _calledClassType;
}

// Must be called after type checking
const ClassType* CallNode::GetCallSiteClassType() const
{
    assert(_callSiteClassType);

    return _callSiteClassType;
}

// Must be called after type checking
const Scope& CallNode::GetCallSiteScope() const { return _callSiteScope; }

// Translate argument names to parameter names
std::map<std::string, std::string>
CallNode::TranslateParameterReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                       const FunctionNode* const callee, const std::string& thisObjectName) const
{
    std::map<std::string, std::string> result;

    const std::vector<const ParseTreeNode*>& arguments = GetArguments();

    for (size_t i = 0; i < arguments.size(); i++)
    {
        const ParseTreeNode* const argument = arguments[i];

        const Type* const argumentType = argument->GetType();

        if (ContainsReference(argumentType))
        {
            const std::string& parameterName = callee->GetParameterName(i);

            // Find all referenced objects in the argument.
            // Translate those references to the parameter name in the callee
            // The map contains pairs of (composite expression in the callee, referenced object name)
            //
            // For example:
            // s is a variable with a struct type.  s.this is a reference to Object1
            // Caller: Foo(s)
            // Callee: void Foo(S param1)
            // The pair in the map is: (param1.this, Object1)
            const std::map<std::string, std::string> references = argument->GetReferences(
                resolveReferenceCallback, parameterName, thisObjectName, callee->GetClassType());

            for (const auto& p : references)
            {
                SafeInsert(result, p.first, p.second);
            }
        }
    }

    return result;
}

std::map<std::string, std::string> CallNode::GetReferences(const ResolveReferenceFunction& resolveReferenceCallbackIn,
                                                           const std::string& resultBaseName,
                                                           const std::string& thisObjectName,
                                                           const ClassType* const thisClassType) const
{
    // Holds information about references used within the function
    std::map<std::string, std::string> referenceMap;

    // Holds information about references in the caller function after
    // the call completes
    std::map<std::string, std::string> result;

    const ResolvedCall resolvedCall = ResolveFunctionCallPostTypeChecking(this, thisClassType, _callSiteScope,
                                                                          thisObjectName, resolveReferenceCallbackIn);

    if (resolvedCall._functionNode)
    {
        // Functions that can return references
        // are always flat functions, never methods
        if (g_compiler->GetGlobalClassType() != resolvedCall._functionNode->GetClassType())
        {
            g_compiler->ErrorStream(_location, CompileError::MethodReturningReference)
                << "Methods returning captured `this` are not supported";
            throw std::runtime_error("Unsupported");
        }

        const std::string calledObjectName = g_globalObjectName;

        referenceMap =
            TranslateParameterReferences(resolveReferenceCallbackIn, resolvedCall._functionNode, thisObjectName);

        const auto resolveReferenceCallback = [&](const std::string& variableName) -> std::string
        { return SafeLookup(referenceMap, variableName); };

        const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
        {
            const AssignNode* const assignNode = dynamic_cast<const AssignNode*>(node);
            const ReturnNode* const returnNode = dynamic_cast<const ReturnNode*>(node);

            if (assignNode)
            {
                assignNode->HandleReferenceAssignment(resolveReferenceCallback, calledObjectName,
                                                      resolvedCall._functionNode, referenceMap);
            }
            else if (returnNode)
            {
                result = returnNode->GetReferences(resolveReferenceCallback, resultBaseName, calledObjectName,
                                                   resolvedCall._functionNode->GetClassType());
            }

            recurseCallback();
        };

        {
            VisitContext visitContext;

            visitContext._global._trackKnownValues = true;

            VisitContext::PushPopScope globalScope(visitContext);

            resolvedCall._functionNode->Visit(callback, visitContext);
        }
    }

    return result;
}

bool CallNode::HasAttribute(const ParseTreeAttribute attribute) const
{
    bool result = false;

    const std::vector<const ParseTreeNode*>& modifiers = dynamic_cast<const NodeList*>(_modifiers)->Children();

    for (const ParseTreeNode* const modifier : modifiers)
    {
        auto attr = dynamic_cast<const BaseAttributeNode*>(modifier);

        if (attr->_attribute == attribute)
        {
            result = true;
        }
    }

    return result;
}

EnumNode::EnumNode(const ParseTreeNode* const name, const ParseTreeNode* const baseType,
                   const ParseTreeNode* const constants, const Scope& namespaceScope)
    : _name(name), _baseType(baseType), _constants(constants)
{
    const auto list = dynamic_cast<const NodeList*>(constants);
    assert(list);

    auto leafType = dynamic_cast<const LeafType*>(baseType->GetType());
    assert(leafType != nullptr);

    std::vector<EnumType::EntryType> constantValues;
    TypeCheckContext dummyTypeCheckContext;
    TypeCheckKnownValueContext dummyKnownValueContext(dummyTypeCheckContext);
    for (const auto node : list->Children())
    {
        const EnumConstantNode* constantNode = dynamic_cast<const EnumConstantNode*>(node);
        assert(constantNode);
        constantNode->TypeCheck(dummyTypeCheckContext);

        const KnownValue knownValue = constantNode->TryGetKnownValue(dummyKnownValueContext, leafType);

        constantValues.emplace_back(constantNode->GetIdentifier(), knownValue._intVal);
    }

    const IdentifierNode* const nameIdentifierNode = dynamic_cast<const IdentifierNode*>(name);

    SetType(
        g_compiler->RegisterEnum(namespaceScope, nameIdentifierNode->GetValue(), leafType, constantValues, _location));
}

StructUnionNode::StructUnionNode(const ContainerType type, const ParseTreeNode* const name,
                                 const ParseTreeNode* const members, const Scope& namespaceScope)
    : _containerType(type), _name(name), _members(members)
{
    // Members of strutures are public
    const IdentifierNode* const nameIdentifierNode = dynamic_cast<const IdentifierNode*>(name);

    Compiler::MemberVariableList memberVariables;
    Compiler::MemberFunctionList memberFunctions;

    GetMembers(members, ParseTreeMemberProtectionModifierPublic, memberVariables, memberFunctions);

    // Structures cannot contain functions
    assert(memberFunctions.empty());

    SetType(g_compiler->RegisterStructUnion(namespaceScope, type, nameIdentifierNode->GetValue(), memberVariables,
                                            _location));
}

void StructUnionNode::TypeCheck(TypeCheckContext& context) const
{
    const NodeList* const members = dynamic_cast<const NodeList*>(_members);

    const std::vector<const ParseTreeNode*>& children = members->Children();

    // Structures cannot contain classes
    for (const auto& p : children)
    {
        if (auto declareNode = dynamic_cast<const DeclareNode*>(p))
        {
            const Type* const declaredType = declareNode->GetDeclaredType();

            if (!declaredType->IsPod())
            {
                g_compiler->ErrorStream(_location, CompileError::NonValueType)
                    << "Struct cannot contain a class or memory; unsupported field type: " << declaredType->GetName();

                // Don't throw an exception into generated parser code
            }

            if ((ContainerType::Union == _containerType) && (declaredType == g_compiler->GetStringType()))
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidString) << "Unions cannot contain strings";
            }

            if (const auto referenceType = dynamic_cast<const ReferenceType*>(declaredType))
            {
                continue;
            }
        }
    }

    // Nested structs can have members with matching names
    TypeCheckContext::PushPopScope pushScope(context);

    for (const ParseTreeNode* child : children)
    {
        child->TypeCheck(context);
    }
}

void StructUnionNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _name->Visit(visitFunction, context);

        _members->Visit(visitFunction, context);
    };

    VisitContext::PushPopScope pushScope(context);

    visitFunction(this, callback);
}

std::string StructUnionNode::PrettyPrint() const
{
    std::ostringstream str;

    if (_containerType == ContainerType::Struct)
    {
        str << "struct";
    }
    else
    {
        assert(_containerType == ContainerType::Union);
        str << "union";
    }

    // Members are not currently printed

    return str.str();
}

// Returns the information necessary to lookup the a symbol in a symbol table
std::pair<Scope, std::string> VariableAccessNode::GetSymbolLookupKey() const
{
    return std::pair<Scope, std::string>(_scopedIdentifierNode->GetScope(), _scopedIdentifierNode->GetName());
}

void VariableAccessNode::TypeCheck(TypeCheckContext& context) const
{
    assert(!context._global._classStack.empty());

    const std::pair<Scope, std::string> symbolKey = GetSymbolLookupKey();

    const Type* type = nullptr;

    if (context.ContainsSymbol(symbolKey.first, symbolKey.second, _location))
    {
        type = context.LookupSymbol(symbolKey.first, symbolKey.second, _location)._type;
    }
    else if (nullptr != (type = context._global._classStack.top()->TryGetMemberType(symbolKey.first, symbolKey.second)))
    {
        // Identifier refers to a class member
    }
    else if (nullptr != (type = g_compiler->TryLookupFunctionType(context._global._classStack.top(), symbolKey.first,
                                                                  symbolKey.second)))
    {
        // Identifier refers to a method of the class
    }
    else if (nullptr != (type = g_compiler->TryLookupFunctionType(g_compiler->GetGlobalClassType(), symbolKey.first,
                                                                  symbolKey.second)))
    {
        // Identifier refers to a global function
    }
    else
    {
        // This will throw an exception
        type = context.LookupSymbol(symbolKey.first, symbolKey.second, _location)._type;
    }

    SetType(type);
}

void VariableAccessNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _scopedIdentifierNode->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

void VariableAccessNode::Rename(const std::string& newName) { _scopedIdentifierNode->Rename(newName); }

std::map<std::string, std::string>
VariableAccessNode::GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                  const std::string& resultBaseName, const std::string& thisObjectName,
                                  const ClassType* const thisClassType) const
{
    return GetType()->GetReferences(GetName(), resultBaseName, resolveReferenceCallback);
}

ClassCallableFunction VariableAccessNode::DescribeCall(const std::string& containingObjectName,
                                                       const ClassCallableFunctionMap& classCallableFunctionMap) const
{
    const std::string functionName = GetRootVariableName();

    const auto it = classCallableFunctionMap.find(functionName);
    if (it != classCallableFunctionMap.end())
    {
        return it->second;
    }
    else
    {
        // Global function
        ClassCallableFunction ccf = {};

        ccf._objectName = g_globalObjectName;
        ccf._classType = g_compiler->GetGlobalClassType();
        ccf._functionNode = GetFunctionByName(g_compiler->GetGlobalClassType(), _scopedIdentifierNode->GetScope(),
                                              functionName, _location);

        return ccf;
    }
}

const ParseTreeNode* ArrayAccessNode::GetIndexNode() const { return _indexNode; }

void ArrayAccessNode::TypeCheck(TypeCheckContext& context) const
{
    _array->TypeCheck(context);

    _indexNode->TypeCheck(context);

    TypeCheckArrayIndex(_indexNode, _location);

    const LeafType* const indexType = dynamic_cast<const LeafType*>(_indexNode->GetType());

    const ArrayTypeBase* const arrayType = dynamic_cast<const ArrayTypeBase*>(_array->GetType());

    if (arrayType)
    {
        const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(arrayType);

        if (memoryType && (ParseTreeMemoryTypeEcc == memoryType->GetMemoryType()))
        {
            const ScopedIdentifierNode* const eccFunctionNameNode = memoryType->GetEccFuncNameNode();
            assert(eccFunctionNameNode);

            // Loads implicitly call a function associated with the memory type
            // The type of the loaded value is equal to the return type of the function
            const ResolvedCall resolvedCall = ResolveFlatFunctionCall(eccFunctionNameNode, context.GetNamespaceScope());

            const FunctionDesc& functionDesc = resolvedCall._functionDesc;

            if (functionDesc._modifiers != ParseTreeFunctionModifierInline)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidEccFunction)
                    << "ECC function must be inline, and must have any other modifiers or attributes";
            }

            if (functionDesc._parameterTypes.size() == 3)
            {
                if (functionDesc._parameterTypes[0] != g_compiler->GetBoolType())
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidEccFunction)
                        << "ECC function first parameter type must be bool";
                }

                if (functionDesc._parameterTypes[1] != g_compiler->GetBoolType())
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidEccFunction)
                        << "ECC function second parameter type must be bool";
                }

                if (functionDesc._parameterTypes[2] != arrayType->_elementType)
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidEccFunction)
                        << "ECC function third parameter type must match the type of data stored in the memory";
                }
            }
            else
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidEccFunction)
                    << "ECC function must have exactly 3 parameters";
            }

            // Ignore the front-end specified type, because the middle-end
            // treats reads from an ecc memory as returning the same type as the return type
            // of the function specified in the `ecc` attribute.
            SetType(resolvedCall._functionDesc._returnType, SetTypeBehavior::DisallowFrontEndOverride);
        }
        else
        {
            SetType(arrayType->_elementType);
        }
    }
    else
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidArrayIndexing) << "Indexing into a non-array type";
    }
}

void ArrayAccessNode::TypeCheckForAssignment(TypeCheckContext& context) const
{
    TypeCheck(context);

    // The type of ArrayAccessNode depends on if it is used for load or store
    // In the store case, the type is simply the element type
    const ArrayTypeBase* const arrayType = dynamic_cast<const ArrayTypeBase*>(_array->GetType());

    if (arrayType)
    {
        SetType(arrayType->_elementType);
    }
}

void ArrayAccessNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _array->Visit(visitFunction, context);

        _indexNode->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

Scope ArrayAccessNode::GetObjectScope() const { return dynamic_cast<const LValNode*>(_array)->GetObjectScope(); }

bool ArrayAccessNode::IsEccMemory() const
{
    const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(_array->GetType());

    return memoryType && (ParseTreeMemoryTypeEcc == memoryType->GetMemoryType());
}

const ScopedIdentifierNode* ArrayAccessNode::GetEccFuncNameNode() const
{
    assert(IsEccMemory());

    const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(_array->GetType());

    return memoryType->GetEccFuncNameNode();
}

MemberAccessNode::MemberAccessNode(const ParseTreeNode* const container, const ParseTreeNode* const contained)
    : _container(container), _containedName(dynamic_cast<const IdentifierNode*>(contained)->GetValue())
{
}

void MemberAccessNode::TypeCheck(TypeCheckContext& context) const
{
    _container->TypeCheck(context);

    const StructUnionType* const structUnionType = dynamic_cast<const StructUnionType*>(_container->GetType());
    const ClassType* classType = dynamic_cast<const ClassType*>(_container->GetType());
    const ReferenceType* referenceType = dynamic_cast<const ReferenceType*>(_container->GetType());

    if (referenceType)
    {
        // The only kind of references that are allowed are references to classes
        assert(!structUnionType);
        assert(!classType);

        classType = dynamic_cast<const ClassType*>(referenceType->_referencedType);
        assert(classType);
    }

    if (structUnionType || classType)
    {
        const Compiler::MemberVariableList& memberVariableList =
            structUnionType ? structUnionType->_members : classType->GetMemberVariables();

        for (const auto& member : memberVariableList)
        {
            if (member.first == _containedName)
            {
                SetType(member.second->GetDeclaredType());

                // Allow private member variable access when accessed via reference
                // References can only be created via capturing [this]
                // At which point all members should be accesible
                if ((ParseTreeMemberProtectionModifierPrivate == member.second->GetProtectionModifier()) &&
                    (referenceType == nullptr))
                {
                    g_compiler->ErrorStream(_location, CompileError::ProtectionModifier)
                        << "Accessing private member variable:" << member.first;
                }
                break;
            }
        }

        if (!GetType())
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidMember)
                << "Unknown member: " << _containedName << "";
            throw std::runtime_error("Unknown member");
        }
    }

    if (!GetType())
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType)
            << "Type error, expecting struct, union, or class";
        throw std::runtime_error("Type error");
    }
}

void MemberAccessNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _container->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

Scope MemberAccessNode::GetObjectScope() const { return dynamic_cast<const LValNode*>(_container)->GetObjectScope(); }

std::map<std::string, std::string>
MemberAccessNode::GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                const std::string& resultBaseName, const std::string& thisObjectName,
                                const ClassType* const thisClassType) const
{
    std::map<std::string, std::string> result;

    const StructUnionType* const thisStructType = dynamic_cast<const StructUnionType*>(GetType());
    const ReferenceType* const thisReferenceType = dynamic_cast<const ReferenceType*>(GetType());

    const std::map<std::string, std::string> containerReferences =
        _container->GetReferences(resolveReferenceCallback, "", thisObjectName, thisClassType);

    if (thisStructType)
    {
        assert(ContainerType::Struct == thisStructType->_type);

        // The return type is a structure
        // Flatten it and find all references contained within nested structures
        const std::vector<StructUnionType::EntryType> flattenedMembers = thisStructType->GetFlattenedMembers();

        for (const auto& member : flattenedMembers)
        {
            const std::string flattenedName = GetMemberName(_containedName, member.first);

            const auto it = containerReferences.find(GetMemberName("", flattenedName));

            if (it != containerReferences.end())
            {
                result[GetMemberName(resultBaseName, member.first)] = it->second;
            }
        }
    }
    else
    {
        assert(thisReferenceType);

        // GetReferences returns all references from the container
        // Select only the one referenced by this member access node
        const std::string objectName = SafeLookup(containerReferences, GetMemberName("", _containedName));

        result[resultBaseName] = objectName;
    }

    return result;
}

// Re-arranges the ordering of members so that member variables come before
// member functions.  This enables variables to be referenced before they are declared
// the only tricky part about this is the public/private modifiers
const ParseTreeNode* ReorderMembers(const ParseTreeNode* const membersIn)
{
    const std::vector<const ParseTreeNode*>& memberNodesIn = dynamic_cast<const NodeList*>(membersIn)->Children();

    std::vector<const ParseTreeNode*> declarationLists[2];
    std::vector<const ParseTreeNode*> otherList;

    // Setup initial protection modifier state
    for (auto& declarationList : declarationLists)
    {
        declarationList.push_back(ParseMemberModifier(ParseTreeMemberProtectionModifierPrivate));
    }

    otherList.push_back(ParseMemberModifier(ParseTreeMemberProtectionModifierPrivate));

    // Modifier state in input node list
    ParseTreeMemberProtectionModifier currInputModifier = ParseTreeMemberProtectionModifierPrivate;

    // Modifier state in the output declaration list
    ParseTreeMemberProtectionModifier currDeclarationListModifier[2] = {ParseTreeMemberProtectionModifierPrivate,
                                                                        ParseTreeMemberProtectionModifierPrivate};

    // Modifier state in the output other list
    ParseTreeMemberProtectionModifier currOtherListModifier = ParseTreeMemberProtectionModifierPrivate;

    for (const ParseTreeNode* const node : memberNodesIn)
    {
        const MemberModifierNode* const modifierNode = dynamic_cast<const MemberModifierNode*>(node);

        const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);

        if (modifierNode)
        {
            currInputModifier = modifierNode->GetModifier();
        }

        if (declareNode)
        {
            const size_t declarationListIndex =
                (nullptr == dynamic_cast<const FunctionType*>(declareNode->GetDeclaredType())) ? 0 : 1;

            auto& declarationList = declarationLists[declarationListIndex];

            // Ensure that modifiers in the declaration list match expectations
            if (currDeclarationListModifier[declarationListIndex] != currInputModifier)
            {
                declarationList.push_back(ParseMemberModifier(currInputModifier));

                currDeclarationListModifier[declarationListIndex] = currInputModifier;
            }

            declarationList.push_back(node);
        }
        else
        {
            // Ensure that modifiers in the other list match expectations
            if (currOtherListModifier != currInputModifier)
            {
                otherList.push_back(ParseMemberModifier(currInputModifier));

                currOtherListModifier = currInputModifier;
            }

            otherList.push_back(node);
        }
    }

    // combine both lists
    NodeList* const result = g_compiler->Create<NodeList>();

    // Non-callback declarations first
    // Callback declarations next (which can reference declared objects)
    for (auto& declarationList : declarationLists)
    {
        for (const ParseTreeNode* const node : declarationList)
        {
            result->Append(node);
        }
    }

    // Then the remainder of nodes
    for (const ParseTreeNode* const node : otherList)
    {
        result->Append(node);
    }

    return result;
}

ClassNode::ClassNode(const ParseTreeNode* const name, const ParseTreeNode* const members,
                     const ParseTreeNode* const templateInstance, const Scope& namespaceScope,
                     const std::string& unmangledName)
    : _name(name), _members(ReorderMembers(members)), _templateInstance(templateInstance),
      _unmangledName(unmangledName), _defaultInitializerList(nullptr), _generatedIR(false)
{
    _originalMembers = _members;

    const IdentifierNode* const nameIdentifierNode = dynamic_cast<const IdentifierNode*>(name);

    Compiler::MemberVariableList memberVariables;
    Compiler::MemberFunctionList memberFunctions;

    // By default, member variables of classes are private
    GetMembers(members, ParseTreeMemberProtectionModifierPrivate, memberVariables, memberFunctions);

    SetType(g_compiler->RegisterClass(namespaceScope, nameIdentifierNode->GetValue(), memberVariables, memberFunctions,
                                      this, _location));

    if (templateInstance)
    {
        auto templateInstanceNode = safe_cast<const TemplateInstanceNode*>(templateInstance);

        auto externClassTemplateType = safe_cast<const ExternClassTemplateType*>(g_compiler->RegisterTemplateInstance(
            templateInstanceNode->GetQualifiedName()->GetScope(), templateInstanceNode->GetQualifiedName()->GetName()));

        const_cast<ExternClassTemplateType*>(externClassTemplateType)
            ->AddClassType(const_cast<ClassType*>(safe_cast<const ClassType*>(GetType())));
    }

    const std::vector<const ParseTreeNode*>& memberNodes = dynamic_cast<const NodeList*>(_members)->Children();

    for (const ParseTreeNode* const member : memberNodes)
    {
        const DefaultInitializerNode* const initNode = dynamic_cast<const DefaultInitializerNode*>(member);

        if (initNode)
        {
            if (_defaultInitializerList)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidInitializerList)
                    << "A class cannot contain multiple default initializers";
            }

            _defaultInitializerList = initNode->GetInitializerList();
        }
    }
}

void ClassNode::TypeCheck(TypeCheckContext& context) const
{
    // Flat functions and global variables are considered to be public
    // Save/Restore the member protection modifier of the containing class
    PushPopMemberProtectionModifier pushPopMemberProtectionModifier(context);

    // By default, member variables of classes are private
    context._global._memberProtectionModifier = ParseTreeMemberProtectionModifierPrivate;

    // Record that type checking has entered this class
    PushPopClassStack pushPopClassStack(context, dynamic_cast<const ClassType*>(GetType()));

    const std::vector<const ParseTreeNode*>& memberNodes = dynamic_cast<const NodeList*>(_members)->Children();

    for (const ParseTreeNode* const member : memberNodes)
    {
        member->TypeCheck(context);
    }

    if (_templateInstance)
    {
        _templateInstance->TypeCheck(context);
    }

    // Type check object initializers.
    // This is done after type checking all members
    // to ensure that members objects are in the symbol table
    // before type checking initializers which may refer
    // to methods to member objects.
    if (context._global._pass == TypeCheckPass::Default)
    {
        const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& visitChildren)
        {
            const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);
            const ClassNode* const classNode = dynamic_cast<const ClassNode*>(node);
            const DefaultInitializerNode* const defaultInitializerNode =
                dynamic_cast<const DefaultInitializerNode*>(node);

            if (declareNode && (dynamic_cast<const ClassType*>(declareNode->GetDeclaredType())) &&
                declareNode->GetAssignNode())
            {
                declareNode->GetAssignNode()->TypeCheckImpl(context);
            }

            if (defaultInitializerNode)
            {
                defaultInitializerNode->TypeCheckImpl(context);
            }

            if (classNode && (classNode != this))
            {
                // Type checking of initializers of nested classes
                // will be performed when the nested class is type checked
            }
            else
            {
                visitChildren();
            }
        };

        VisitContext visitContext;
        VisitContext::PushPopScope globalScope(visitContext);

        _members->Visit(callback, visitContext);
    }

    if (IsExportIgnoreTarget())
    {
        // Type checking for exported classes

        // strings are not allowed on the export class interfaces
        const auto typeAllowedOnExportedClass = [](const Type* const type)
        {
            bool result = true;

            if (type == g_compiler->GetStringType())
            {
                result = false;
            }

            return result;
        };

        const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& visitChildren)
        {
            const FunctionNode* const functionNode = dynamic_cast<const FunctionNode*>(node);
            const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);

            if (functionNode && (ParseTreeMemberProtectionModifierPublic == functionNode->GetProtectionModifier()))
            {
                for (size_t i = 0; i < functionNode->GetParameterCount(); i++)
                {
                    if (!typeAllowedOnExportedClass(functionNode->GetParameterType(i)))
                    {
                        g_compiler->ErrorStream(_location, CompileError::InvalidString)
                            << "String parameters not allowed on public methods of exported classes";
                    }
                }

                if (!typeAllowedOnExportedClass(functionNode->GetReturnType()))
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidString)
                        << "String return type not allowed on public methods of exported classes";
                }
            }
            else if (declareNode)
            {
                const FunctionType* const functionType =
                    dynamic_cast<const FunctionType*>(declareNode->GetDeclaredType());

                if (functionType)
                {
                    for (size_t i = 0; i < functionType->GetParamCount(); i++)
                    {
                        if (!typeAllowedOnExportedClass(functionType->GetParamType(i)))
                        {
                            g_compiler->ErrorStream(_location, CompileError::InvalidString)
                                << "String parameters not allowed for callbacks contained in exported classes";
                        }
                    }

                    if (!typeAllowedOnExportedClass(functionType->GetReturnType()))
                    {
                        g_compiler->ErrorStream(_location, CompileError::InvalidString)
                            << "String return type not allowed for callbacks contained in exported classes";
                    }
                }
            }

            visitChildren();
        };

        VisitContext visitContext;
        VisitContext::PushPopScope globalScope(visitContext);

        _members->Visit(callback, visitContext);
    }
    else if (!IsExternal() &&
             (context._global._pass ==
              TypeCheckPass::Default)) // Ensure that the exported attribute has propagated to the class before checking
    {
        // type checking for classes that are not exported
        EnumerateCallbacks(
            [this](const std::string& name, const FunctionType* fnType)
            {
                if (fnType->GetLatency())
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidCallback)
                        << "[[latency()]] attribute specified on callback: " << name
                        << " of a non-{export/extern} class";
                }

                if (fnType->GetModifiers() & ParseTreeFunctionModifierNoBackPressure)
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidCallback)
                        << "[[no_backpressure]] attribute specified on callback: " << name
                        << " of a non-{export/extern} class";
                }
            });
    }
}

void ClassNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _name->Visit(visitFunction, context);

        if (_templateInstance)
        {
            _templateInstance->Visit(visitFunction, context);
        }

        _members->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

const std::string& ClassNode::GetName() const { return dynamic_cast<const IdentifierNode*>(_name)->GetValue(); }

const std::string& ClassNode::GetUnmangledName() const { return _unmangledName; }

bool ClassNode::IsExport() const { return safe_cast<const ClassType*>(GetType())->IsExport(); }

bool ClassNode::IsExportIgnoreTarget() const { return safe_cast<const ClassType*>(GetType())->IsExportIgnoreTarget(); }

bool ClassNode::IsExternal() const { return safe_cast<const ClassType*>(GetType())->IsExternal(); }

// Used to initialize the top-level ClassCallableFunctionMap
// Map callback names to the synthesized FunctionNode associated with them
ClassCallableFunctionMap ClassNode::GetCallbackExternFunctions() const
{
    ClassCallableFunctionMap result;

    for (const auto& p : _externCallbacks)
    {
        ClassCallableFunction ccf = {};

        ccf._objectName = g_globalObjectName;
        ccf._classType = g_compiler->GetGlobalClassType();
        ccf._functionNode = p.second;

        SafeInsert(result, p.first, ccf);
    }

    return result;
}

const FunctionNode* ClassNode::TryGetExternCallback(const std::string& name) const
{
    const auto it = _externCallbacks.find(name);

    return it == _externCallbacks.end() ? nullptr : it->second;
}

const FunctionNode* ClassNode::TryGetExportCallback(const std::string& objectName,
                                                    const std::string& callbackName) const
{
    const ExportCallbackKey key(objectName, callbackName);

    const auto it = _exportCallbacks.find(key);

    return it == _exportCallbacks.end() ? nullptr : it->second;
}

const FunctionNode* ClassNode::GetExportCallback(const std::string& objectName, const std::string& callbackName) const
{
    const ExportCallbackKey key(objectName, callbackName);

    return SafeLookup(_exportCallbacks, key);
}

// Remove portions of the AST which should not affect compilation
// when compiling uses of an export/external class
void ClassNode::PruneForExportOrExternalClassUse()
{
    assert(IsExportIgnoreTarget() || IsExternal());

    // Remove the bodies of all methods
    EnumerateMethods([&](const FunctionNode* functionNode)
                     { const_cast<FunctionNode*>(functionNode)->SetStatements(ParseBaseList(nullptr)); });

    // Remove member objects (but preserve callbacks without initial values)

    // Reset() will restore the original member list
    assert(_members == _originalMembers);

    const std::vector<const ParseTreeNode*>& memberNodes = dynamic_cast<const NodeList*>(_members)->Children();

    NodeList* const prunedMembers = g_compiler->Create<NodeList>();

    for (const ParseTreeNode* const member : memberNodes)
    {
        const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(member);

        const AssignNode* const assignNode = dynamic_cast<const AssignNode*>(member);

        if (declareNode)
        {
            if (dynamic_cast<const FunctionType*>(declareNode->GetDeclaredType()))
            {
                prunedMembers->Append(member);
            }
        }
        else if (assignNode)
        {
            // Declarations with initial values will be contained in assignment nodes
            // Callbacks from export classes with initial values are not exposed on the interface
            // so they can be removed
        }
        else
        {
            prunedMembers->Append(member);
        }
    }

    _members = prunedMembers;
}

void ClassNode::EnumerateMethods(const std::function<void(const FunctionNode*)>& functionCallback,
                                 const std::optional<ParseTreeMemberProtectionModifier> modifierFilter) const
{
    const std::vector<const ParseTreeNode*>& memberNodes = dynamic_cast<const NodeList*>(_members)->Children();

    ParseTreeMemberProtectionModifier currModifier = ParseTreeMemberProtectionModifierPrivate;

    for (const ParseTreeNode* const member : memberNodes)
    {
        const MemberModifierNode* const modifierNode = dynamic_cast<const MemberModifierNode*>(member);

        if (modifierNode)
        {
            currModifier = modifierNode->GetModifier();
        }
        else
        {
            VisitContext visitContext = {};

            VisitContext::PushPopScope outerScope(visitContext);

            const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
            {
                const FunctionNode* const functionNode = dynamic_cast<const FunctionNode*>(node);

                if (functionNode)
                {
                    // Method - check the protection modifier
                    if (!modifierFilter || (*modifierFilter == currModifier))
                    {
                        functionCallback(functionNode);
                    }
                }
                else
                {
                    recurseCallback();
                }
            };

            member->Visit(callback, visitContext);
        }
    }
}

void ClassNode::EnumeratePublicMethods(const std::function<void(const FunctionNode*)>& functionCallback) const
{
    EnumerateMethods(functionCallback,
                     std::optional<ParseTreeMemberProtectionModifier>(ParseTreeMemberProtectionModifierPublic));
}

void ClassNode::EnumerateCallbacks(
    const std::function<void(const std::string&, const FunctionType*)>& outerCallback) const
{
    const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
    {
        const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);

        if (declareNode && dynamic_cast<const FunctionType*>(declareNode->GetDeclaredType()))
        {
            outerCallback(declareNode->GetDeclaredName(),
                          safe_cast<const FunctionType*>(declareNode->GetDeclaredType()));
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

const DesignatedInitializerListNode* ClassNode::TryGetDefaultInitializer() const { return _defaultInitializerList; }

std::list<TemplateArgument> ClassNode::GetTemplateArguments() const
{
    assert(_templateInstance);

    return safe_cast<const TemplateInstanceNode*>(_templateInstance)->GetTemplateArguments();
}

const std::string& ClassNode::GetTemplateName() const
{
    assert(_templateInstance);

    const QualifiedNameNode* const qualifiedName =
        safe_cast<const TemplateInstanceNode*>(_templateInstance)->GetQualifiedName();

    return qualifiedName->GetName();
}

// Returns the name of the external RTL module
// that will implement this extern class
// For template instances, return the name of the template
const std::string ClassNode::GetExternalClassName() const
{
    assert(IsExternal());

    return safe_cast<const ClassType*>(GetType())->GetExternalName();
}

bool ClassNode::IsTemplateInstance() const { return _templateInstance != nullptr; }

void MemberModifierNode::TypeCheck(TypeCheckContext& context) const
{
    context._global._memberProtectionModifier = _modifier;
}

void SwitchNode::TypeCheck(TypeCheckContext& context) const
{
    _testExpression->TypeCheck(context);

    _blockList->TypeCheck(context);

    const Type* const expressionType = _testExpression->GetType();

    const LeafType* const leafType = dynamic_cast<const LeafType*>(expressionType);

    if (!leafType || (leafType->_baseType != BaseType::Uint))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidSwitch)
            << "Switch statement value must be an unsigned integer";
    }

    const std::vector<const ParseTreeNode*>& switchBlocks = dynamic_cast<const NodeList*>(_blockList)->Children();

    size_t defaultBlockCount = 0;

    std::set<size_t> caseValues;

    for (const ParseTreeNode* const node : switchBlocks)
    {
        const SwitchBlockNode* const switchBlockNode = dynamic_cast<const SwitchBlockNode*>(node);

        const ParseTreeNode* const caseValueNode = switchBlockNode->GetCaseValue();

        if (caseValueNode)
        {
            const KnownValue caseValue =
                TryGetTypeCheckKnownValue(context, caseValueNode, _location, caseValueNode->GetType());

            if (KnownValueType::Int == caseValue._type)
            {
                const auto insertResult = caseValues.insert(MpToSizeT(caseValue._intVal));

                if (!insertResult.second)
                {
                    g_compiler->ErrorStream(_location, CompileError::InvalidSwitch) << "Case values must be unique";
                }
            }
            else
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidSwitch)
                    << "Case statements values must be integers known at compile time";
            }
        }
        else
        {
            defaultBlockCount++;
        }
    }

    if (defaultBlockCount > 1)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidSwitch)
            << "Switch statement cannot have 2 default blocks";
    }
}

void SwitchNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _testExpression->Visit(visitFunction, context);

        _blockList->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

void SwitchBlockNode::TypeCheck(TypeCheckContext& context) const
{
    if (_caseValue)
    {
        _caseValue->TypeCheck(context);
    }

    _statementList->TypeCheck(context);
}

void SwitchBlockNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        if (_caseValue)
        {
            _caseValue->Visit(visitFunction, context);
        }

        _statementList->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

void StaticNode::TypeCheck(TypeCheckContext& context) const
{
    _expression->TypeCheck(context);

    SetType(_expression->GetType());
}

void StaticNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _expression->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

void FanOutNode::TypeCheck(TypeCheckContext& context) const
{
    _count->TypeCheck(context);

    _expression->TypeCheck(context);

    if (!IsUnsignedOrNonNegative(context, _count, _location))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType)
            << "fan_out requires count to be an unsigned integer, or known to be non-negative at compile time";
    }

    // Get the count
    const KnownValue countValue = TryGetTypeCheckKnownValue(context, _count, _location, _count->GetType());

    if (countValue._type != KnownValueType::Int)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidArraySize)
            << "fan_out requires count to be known at compile time";
    }

    if (countValue._intVal == 0)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidArraySize) << "fan_out requires count to be positive";
    }

    SetType(g_compiler->GetArrayType(_expression->GetType(), MpToSizeT(countValue._intVal), ParseTreeArrayTypeDefault,
                                     ParseTreeMemoryTypeDefault, nullptr, false, _location));
}

void FanOutNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _count->Visit(visitFunction, context);

        _expression->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

void ConcatNode::TypeCheck(TypeCheckContext& context) const
{
    const std::vector<const ParseTreeNode*>& elements = dynamic_cast<const NodeList*>(_elements)->Children();

    size_t totalWidth = 0;

    for (const ParseTreeNode* const element : elements)
    {
        element->TypeCheck(context);

        const size_t elementWidth = element->GetType()->GetBitWidth();

        if (0 == elementWidth)
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidType)
                << "concat element type must have positive width";
        }

        totalWidth += elementWidth;
    }

    SetType(g_compiler->GetLeafType(BaseType::Uint, totalWidth, _location));
}

void ConcatNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _elements->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

void ReorderNode::TypeCheck(TypeCheckContext& context) const { _body->TypeCheck(context); }

void ReorderNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _body->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

// The front-end passes a nullptr for nodes which are not template instances
// This function is used to select a default name in this case
std::string GetOptionalUnmangledName(const char* const unmangledName, ParseTreeNodePtr defaultNameNode)
{
    return unmangledName ? unmangledName : safe_cast<const IdentifierNode*>(defaultNameNode)->GetValue();
}

API ParseTreeNodePtr ParseFlagAttribute(unsigned int attribute)
{
    return g_compiler->Create<FlagAttributeNode>(attribute);
}

API ParseTreeNodePtr ParseIntAttribute(unsigned int attribute, ParseTreeNodePtr value)
{
    return g_compiler->Create<IntAttributeNode>(attribute,
                                                MpToSizeT(dynamic_cast<const IntegerNode*>(value)->GetUntypedValue()));
}

API ParseTreeNodePtr ParseAttribute(unsigned int attribute, ParseTreeNodePtr value)
{
    return g_compiler->Create<AttributeNode>(attribute, value);
}

ParseTreeNodePtr ParseFunctionParam(ParseTreeNodePtr attributes, ParseTreeNodePtr type, ParseTreeNodePtr name,
                                    ParseNamespaceScopePtr namespaceScope)
{
    uint32_t flags = DECLARE_FLAG_PARAMETER;

    for (auto node : dynamic_cast<const NodeList*>(attributes)->Children())
    {
        flags |= dynamic_cast<const FlagAttributeNode*>(node)->_attribute;
    }

    return ParseDeclare(nullptr, type, name, nullptr, flags, namespaceScope);
}

ParseTreeNodePtr ParseDeclare(ParseTreeNodePtr attributeList, ParseTreeNodePtr type, ParseTreeNodePtr name,
                              ParseTreeNodePtr val, unsigned int flags, ParseNamespaceScopePtr namespaceScope)
{
    if (auto const_ = dynamic_cast<const ConstNode*>(type))
    {
        type = const_->GetTypeNode();
        flags |= DECLARE_FLAG_CONST;
    }

    if (val)
    {
        NodeList* const nodeList = g_compiler->Create<NodeList>();

        const bool isConst = flags & DECLARE_FLAG_CONST;

        const bool isGlobal = flags & DECLARE_FLAG_GLOBAL;

        // Append a null scope
        // this is OK because Parse*NamedVariable only needs a namespace name when referencing a global, not declaring
        // it
        ParseTreeNode* const scopedIdentifierNode = ParseScopedIdentifier(ParseBaseList(name));

        // If the value is const, then create the VariableAccessNode such that the Store() operation
        // does not fail because the value is known at compile-time
        const ParseTreeNode* const variableAccessNode =
            isConst ? ParseKnownConstNamedVariable(scopedIdentifierNode) : ParseNamedVariable(scopedIdentifierNode);

        const AssignNode* const assignNode = g_compiler->Create<AssignNode>(variableAccessNode, val, isGlobal, true);

        const DeclareNode* const declareNode =
            g_compiler->Create<DeclareNode>(type, name, ToScope(namespaceScope), flags, assignNode, attributeList);

        // Ensure the assign is type checked first
        nodeList->Append(declareNode);

        nodeList->Append(assignNode);

        return nodeList;
    }
    else
    {
        return g_compiler->Create<DeclareNode>(type, name, ToScope(namespaceScope), flags, nullptr /* assignNode */,
                                               attributeList);
    }
}

ParseTreeNodePtr ParseAssign(ParseTreeNodePtr lhs, ParseTreeNodePtr rhs)
{
    // 3rd parameter means the assignment may or may not be to a global
    // 4th parameter means that the assignment is not part of a variable declaration
    return g_compiler->Create<AssignNode>(lhs, rhs, false, false);
}

ParseTreeNodePtr ParseIf(ParseTreeNodePtr condition, ParseTreeNodePtr lhs, ParseTreeNodePtr rhs)
{
    return g_compiler->Create<IfNode>(condition, lhs, rhs);
}

ParseTreeNodePtr ParseDoWhile(ParseTreeNodePtr modifiers, ParseTreeNodePtr cond, ParseTreeNodePtr body)
{
    return ParseNestedScope(g_compiler->Create<DoWhileLoopNode>(modifiers, cond, body));
}

ParseTreeNodePtr ParseRangeFor(ParseTreeNodePtr modifiers, ParseTreeNodePtr var, ParseTreeNodePtr bound,
                               ParseTreeNodePtr body)
{
    return ParseNestedScope(g_compiler->Create<RangeForLoopNode>(modifiers, var, bound, body));
}

ParseTreeNodePtr ParseUnrolledFor(ParseTreeNodePtr var, ParseTreeNodePtr bound, ParseTreeNodePtr body)
{
    return ParseNestedScope(g_compiler->Create<UnrolledForLoopNode>(var, bound, body));
}

ParseTreeNodePtr ParseAnnotatedStatement(ParseTreeNodePtr attr, ParseTreeNodePtr statement)
{
    size_t maxThreads = 1;

    if (const auto list = dynamic_cast<const NodeList*>(attr))
    {
        for (const auto node : list->Children())
        {
            if (const auto attr = dynamic_cast<const IntAttributeNode*>(node))
            {
                if (attr->_attribute == ParseTreeScheduleAttr)
                {
                    maxThreads = attr->_value;
                }
            }
        }
    }

    // TODO: generalize AtomicNode to a node which can annotate contained
    // statement with various attributes
    return g_compiler->Create<AtomicNode>(statement, maxThreads);
}

ParseTreeNodePtr ParseMux(ParseTreeNodePtr predicate, ParseTreeNodePtr list)
{
    return g_compiler->Create<MuxOpNode>(predicate, list);
}

ParseTreeNodePtr ParseBinaryOp(ParseTreeBinaryOpType type, ParseTreeNodePtr lhs, ParseTreeNodePtr rhs)
{
    return g_compiler->Create<BinaryOpNode>(type, lhs, rhs);
}

ParseTreeNodePtr ParseUnaryOp(ParseTreeUnaryOpType type, ParseTreeNodePtr expression)
{
    return g_compiler->Create<UnaryOpNode>(type, expression);
}

ParseTreeNodePtr ParseSizeOf(ParseTreeSizeofType type, ParseTreeNodePtr expression)
{
    return g_compiler->Create<SizeOfNode>(type, expression);
}

ParseTreeNodePtr ParseCast(ParseTreeNodePtr type, ParseTreeNodePtr src)
{
    return g_compiler->Create<CastNode>(type, src);
}

ParseTreeNodePtr ParseFunctionSpecifier(ParseTreeNodePtr object, ParseTreeNodePtr name)
{
    return g_compiler->Create<FunctionSpecifierNode>(object, name);
}

ParseTreeNodePtr ParseFunctionCall(ParseTreeNodePtr fun, ParseTreeNodePtr arguments, ParseTreeNodePtr modifiers)
{
    const auto f = dynamic_cast<const FunctionSpecifierNode*>(fun);

    assert(f);

    return g_compiler->Create<CallNode>(f->_object, f->_name, arguments, modifiers);
}

ParseTreeNodePtr ParseBaseList(ParseTreeNodePtr listBase)
{
    NodeList* const result = g_compiler->Create<NodeList>();

    if (listBase)
    {
        ParseAppendList(result, listBase);
    }

    return result;
}

ParseTreeNodePtr ParseAppendList(ParseTreeNodePtr list, ParseTreeNodePtr newNode)
{
    NodeList* const nodeList = dynamic_cast<NodeList*>(list);

    NodeList* const newList = dynamic_cast<NodeList*>(newNode);

    if (newList)
    {
        // newNode is a list, insert the individual elements of the newNode into list
        // To enable functions to non-recursively traverse a list
        const std::vector<const ParseTreeNode*>& newChildren = newList->Children();

        for (const ParseTreeNode* const child : newChildren)
        {
            nodeList->Append(child);
        }
    }
    else if (newNode)
    {
        nodeList->Append(newNode);
    }

    return nodeList;
}

ParseTreeNodePtr ParseArrayType(ParseTreeNodePtr attributes, ParseTreeNodePtr baseType, ParseTreeNodePtr sizeList)
{
    // Multi-dimensional arrays are built in a nested fashion
    // This isn't done in the grammar, because it is tricky to get multi-dimensional arrays correct
    const std::vector<const ParseTreeNode*>& sizeElements = dynamic_cast<const NodeList*>(sizeList)->Children();

    const TypeNode* const elementTypeNode = dynamic_cast<const TypeNode*>(baseType);

    const TypeNode* mostRecentTypeNode = elementTypeNode;

    ParseTreeArrayType arrayType = ParseTreeArrayTypeDefault;

    ParseTreeMemoryType memoryType = ParseTreeMemoryTypeDefault;

    const ScopedIdentifierNode* eccFuncName = nullptr;

    bool autoInitialize = false;

    for (auto node : dynamic_cast<const NodeList*>(attributes)->Children())
    {
        auto flag = dynamic_cast<const FlagAttributeNode*>(node);

        if (flag)
        {
            const auto attr = flag->_attribute;

            if (attr == ParseTreeMemoryTypeDefault)
            {
                arrayType = ParseTreeArrayTypeMemory;
            }

            if (attr == ParseTreeMemoryTypeNoReplication)
            {
                memoryType = ParseTreeMemoryTypeNoReplication;
            }

            if (attr == ParseTreeMemoryTypeQuadPort)
            {
                memoryType = ParseTreeMemoryTypeQuadPort;
            }

            if (attr == ParseTreeMemoryTypeInitialize)
            {
                autoInitialize = true;
            }
        }

        auto attr = dynamic_cast<const AttributeNode*>(node);

        if (attr && attr->_attribute == ParseTreeMemoryTypeEcc)
        {
            memoryType = ParseTreeMemoryTypeEcc;

            // Get the name of the function to call on reads from the ecc memory
            eccFuncName = dynamic_cast<const ScopedIdentifierNode*>(attr->_value);
            assert(eccFuncName);
        }
    }

    for (auto it = sizeElements.rbegin(); it != sizeElements.rend(); ++it)
    {
        const auto sizeNode = dynamic_cast<const IntegerNode* const>(*it);

        const size_t arraySize = MpToSizeT(sizeNode->GetTypedValue());

        const auto array = g_compiler->GetArrayType(mostRecentTypeNode->GetType(), arraySize, arrayType, memoryType,
                                                    eccFuncName, autoInitialize, g_currentTokenLocation);

        mostRecentTypeNode = g_compiler->Create<TypeNode>(array);

        // For memory uint32[4][512] represents 4 memories each with 512 elements
        // Thus only the first loop iteration can have ParseTreeArrayTypeMemory
        arrayType = ParseTreeArrayTypeDefault;

        memoryType = ParseTreeMemoryTypeDefault;

        autoInitialize = false;

        eccFuncName = nullptr;
    }

    return const_cast<TypeNode*>(mostRecentTypeNode);
}

ParseTreeNodePtr ParseFunctionModifier(ParseTreeFunctionModifier modifier)
{
    return g_compiler->Create<FunctionModifierNode>(modifier);
}

ParseTreeNodePtr ParseFunctionTypeParam(ParseTreeNodePtr attr, ParseTreeNodePtr type, ParseTreeNodePtr name)
{
    return g_compiler->Create<FunctionTypeParamNode>(attr, type, name);
}

ParseTreeNodePtr ParseFunctionType(ParseTreeNodePtr modifierList, ParseTreeNodePtr params,
                                   ParseTreeNodePtr returnTypeNode)
{
    std::vector<const Type*> paramTypes;
    std::vector<std::string> paramNames;
    boost::optional<size_t> isLastParameterIndex;

    const std::vector<const ParseTreeNode*>& paramTypeNodes = safe_cast<const NodeList*>(params)->Children();

    for (size_t parameterIndex = 0; parameterIndex < paramTypeNodes.size(); parameterIndex++)
    {
        const ParseTreeNode* const node = paramTypeNodes[parameterIndex];

        const FunctionTypeParamNode* const paramTypeNode = safe_cast<const FunctionTypeParamNode*>(node);

        paramTypes.push_back(paramTypeNode->GetType());

        if (paramTypeNode->HasName())
        {
            paramNames.push_back(paramTypeNode->GetName());
        }
        else
        {
            // Generate a synthetic parameter name which hasn't already been used
            for (size_t i = 0;; i++)
            {
                const std::string paramName = std::string("_param_") + std::to_string(i);

                if (paramNames.end() == std::find(paramNames.begin(), paramNames.end(), paramName))
                {
                    paramNames.push_back(paramName);
                    break;
                }
            }
        }

        if (paramTypeNode->HasLastAttribute())
        {
            isLastParameterIndex = parameterIndex;
        }
    }

    const Type* const returnType = returnTypeNode->GetType();

    const ParseTreeFunctionModifier flags = GetFunctionModifiers(modifierList);

    const boost::optional<size_t> latency = GetFunctionLatency(modifierList);

    return g_compiler->Create<TypeNode>(
        g_compiler->GetFunctionType(paramTypes, paramNames, returnType, flags, latency, isLastParameterIndex));
}

ParseTreeNodePtr ParseFunction(ParseTreeNodePtr modifierList, ParseTreeNodePtr modifier,
                               ParseTreeNodePtr returnTypeNode, ParseTreeNodePtr name, ParseTreeNodePtr params,
                               ParseTreeNodePtr statements, const char* unmangledName)
{
    const Type* const returnType = dynamic_cast<const TypeNode*>(returnTypeNode)->GetType();

    if (modifier != nullptr)
    {
        if (auto list = dynamic_cast<NodeList*>(modifierList))
        {
            list->Append(modifier);
        }
    }

    ParseTreeNodePtr functionStatements = statements;

    // Void functions do not need a return statement
    // If one was not supplied in the code, then add one
    // This assumes that return statements must appear at the end of a function
    // and that there can only be 1
    if (returnType == g_compiler->GetVoidType())
    {
        const std::vector<const ParseTreeNode*>& statementVector =
            dynamic_cast<const NodeList*>(statements)->Children();

        bool hasReturn = false;

        for (const ParseTreeNode* const node : statementVector)
        {
            const ReturnNode* const returnNode = dynamic_cast<const ReturnNode*>(node);

            if (returnNode)
            {
                hasReturn = true;
                break;
            }
        }

        if (!hasReturn)
        {
            if (statementVector.size() > 0)
            {
                const ParseTreeNode* const lastStatement = statementVector.back();

                Location location = lastStatement->GetLocation();
                if (location._endLine > location._beginLine)
                {
                    // A block statement (i.e. atomic block or if/else block)
                    // will have a bounds location. The implicit return should only
                    // use the endLine of the block
                    location._beginLine = location._endLine;
                }

                // An implicit return does not have a location. Give it the last location in the function.
                SetLocation2(&location);
            }

            functionStatements = ParseAppendList(statements, ParseReturn());
        }
    }

    return ParseNestedScope(g_compiler->Create<FunctionNode>(
        modifierList, returnTypeNode, name, params, functionStatements, GetOptionalUnmangledName(unmangledName, name)));
}

ParseTreeNodePtr ParseReturn() { return g_compiler->Create<ReturnNode>(nullptr); }

ParseTreeNodePtr ParseReturnExpression(ParseTreeNodePtr value) { return g_compiler->Create<ReturnNode>(value); }

ParseTreeNodePtr ParseNestedScope(ParseTreeNodePtr body) { return g_compiler->Create<NestedScopeNode>(body); }

ParseTreeNodePtr ParseStruct(ParseTreeNodePtr name, ParseTreeNodePtr members, ParseNamespaceScopePtr namespaceScope,
                             const char* unmangledName)
{
    return g_compiler->Create<StructUnionNode>(ContainerType::Struct, name, members, ToScope(namespaceScope));
}

ParseTreeNodePtr ParseUnion(ParseTreeNodePtr name, ParseTreeNodePtr members, ParseNamespaceScopePtr namespaceScope,
                            const char* unmangledName)
{
    return g_compiler->Create<StructUnionNode>(ContainerType::Union, name, members, ToScope(namespaceScope));
}

ParseTreeNodePtr ParseThis(ParseNamespaceScopePtr namespaceScope)
{
    return g_compiler->Create<ThisNode>(ToScope(namespaceScope));
}

ParseTreeNodePtr ParseReference(ParseTreeNodePtr type)
{
    return g_compiler->Create<TypeNode>(g_compiler->GetReferenceType(type->GetType()));
}

ParseTreeNodePtr ParseClass(ParseTreeNodePtr name, ParseTreeNodePtr members, ParseTreeNodePtr templateInstance,
                            ParseNamespaceScopePtr namespaceScope, const char* unmangledName)
{
    return ParseNestedScope(g_compiler->Create<ClassNode>(name, members, templateInstance, ToScope(namespaceScope),
                                                          GetOptionalUnmangledName(unmangledName, name)));
}

ParseTreeNodePtr ParseQualifiedName(ParseQualifiedNamePtr nameArray)
{
    Scope qualifier;
    std::string name;

    for (size_t i = 0; nameArray[i] != nullptr; i++)
    {
        qualifier.push_front(nameArray[i]);
    }

    assert(!qualifier.empty());

    name = qualifier.front();
    qualifier.pop_front();

    return g_compiler->Create<QualifiedNameNode>(qualifier, name);
}

ParseTreeNodePtr ParseTemplateArg(const char* name, ParseTreeNodePtr arg)
{
    return g_compiler->Create<TemplateArgNode>(name, arg);
}

ParseTreeNodePtr ParseTemplateInstance(ParseTreeNodePtr templateName, ParseTreeNodePtr templateArguments)
{
    return g_compiler->Create<TemplateInstanceNode>(templateName, templateArguments);
}

ParseTreeNodePtr ParseDefaultInitialization(ParseTreeNodePtr init)
{
    return g_compiler->Create<DefaultInitializerNode>(safe_cast<const DesignatedInitializerListNode*>(init));
}

ParseTreeNodePtr ParseNamedType(ParseTreeNodePtr nameNode, ParseNamespaceScopePtr namespaceScope)
{
    auto scope = ScopeFromNodeList(nameNode);

    auto name = scope.front();

    // remove name to leave just scope
    scope.pop_front();

    // The name will be resolved to a specific type during type checking. This
    // deferred resolution is necessary to support template instantiation where
    // instances, which are created in place of template definition, may refer
    // to types defined later in the file and passed as type arguments during
    // instantiation.
    // Deferred type resolution is implemented via Type::ResolveDeferredType
    // virtual function. The type checker uses Visit function to recursively
    // resolve types in the parse tree.
    const Type* const type = g_compiler->Create<DeferredType>(ToScope(namespaceScope), scope, name);

    return g_compiler->Create<TypeNode>(scope, name, type);
}

ParseTreeNodePtr ParseNamedVariable(ParseTreeNodePtr nameNode)
{
    ScopedIdentifierNode* const scopedIdentifierNode = dynamic_cast<ScopedIdentifierNode*>(nameNode);

    return g_compiler->Create<VariableAccessNode>(scopedIdentifierNode, false /* _isKnownConstInitialSet */);
}

ParseTreeNodePtr ParseKnownConstNamedVariable(ParseTreeNodePtr nameNode)
{
    ScopedIdentifierNode* const scopedIdentifierNode = dynamic_cast<ScopedIdentifierNode*>(nameNode);

    return g_compiler->Create<VariableAccessNode>(scopedIdentifierNode, true /* _isKnownConstInitialSet */);
}

ParseTreeNodePtr ParseAccessArray(ParseTreeNodePtr ary, ParseTreeNodePtr indexList)
{
    return g_compiler->Create<ArrayAccessNode>(ary, indexList);
}

ParseTreeNodePtr ParseAccessArrayLValue(ParseTreeNodePtr ary, ParseTreeNodePtr indexList)
{
    // TODO: LValueArrayAccessNode
    return g_compiler->Create<ArrayAccessNode>(ary, indexList);
}

ParseTreeNodePtr ParseAccessMember(ParseTreeNodePtr container, ParseTreeNodePtr contained)
{
    return g_compiler->Create<MemberAccessNode>(container, contained);
}

ParseTreeNodePtr ParseEnum(ParseTreeNodePtr name, ParseTreeNodePtr baseType, ParseTreeNodePtr constants,
                           ParseNamespaceScopePtr scope)
{
    const auto list = dynamic_cast<const NodeList*>(constants);

    assert(list);
    for (const auto node : list->Children())
    {
        assert(dynamic_cast<const EnumConstantNode*>(node));
    }

    // TODO: handle enums as distinct types
    return g_compiler->Create<EnumNode>(name, baseType, constants, ToScope(scope));
}

ParseTreeNodePtr ParseEnumConstant(ParseTreeNodePtr name, ParseTreeNodePtr value, ParseNamespaceScopePtr scope)
{
    assert(dynamic_cast<const IdentifierNode*>(name));
    // IntegerNode doesn't support negative numbers so they are passed as Negate unary expression
    assert(dynamic_cast<const IntegerNode*>(value) || dynamic_cast<const UnaryOpNode*>(value));

    return g_compiler->Create<EnumConstantNode>(name, value, ToScope(scope));
}

ParseTreeNodePtr ParseEnumValue(ParseTreeNodePtr identifier, ParseTreeNodePtr value)
{
    assert(dynamic_cast<const ScopedIdentifierNode*>(identifier));

    // TODO: handle enum values as values of enum type; for now treat as underlying integer
    return value;
}

ParseTreeNodePtr ParseMemberModifier(ParseTreeMemberProtectionModifier modifier)
{
    return g_compiler->Create<MemberModifierNode>(modifier);
}

ParseTreeNodePtr ParseDesignator(ParseTreeNodePtr name, ParseTreeNodePtr init)
{
    return g_compiler->Create<DesignatorNode>(name, init);
}

ParseTreeNodePtr ParseInitializerList(ParseTreeNodePtr initializerList)
{
    const std::vector<const ParseTreeNode*>& children = safe_cast<const NodeList*>(initializerList)->Children();

    // If any element is a designator node (.x = 4), then treat the whole list as a designated initializer list
    bool isDesignatedInitializerList = false;

    for (const ParseTreeNode* const child : children)
    {
        if (dynamic_cast<const DesignatorNode*>(child))
        {
            isDesignatedInitializerList = true;
            break;
        }
    }

    if (isDesignatedInitializerList)
    {
        return g_compiler->Create<DesignatedInitializerListNode>(initializerList);
    }
    else
    {
        return g_compiler->Create<InitializerListNode>(initializerList);
    }
}

ParseTreeNodePtr ParseSwitch(ParseTreeNodePtr testExpression, ParseTreeNodePtr blockList)
{
    return g_compiler->Create<SwitchNode>(testExpression, blockList);
}

ParseTreeNodePtr ParseSwitchBlock(ParseTreeNodePtr caseValue, ParseTreeNodePtr statementList)
{
    // ParseNestedScope to allow local variables to be declared inside of case statement
    // without requiremeing curly braces
    return g_compiler->Create<SwitchBlockNode>(caseValue, ParseNestedScope(statementList));
}

ParseTreeNodePtr ParseExternalFunction(ParseTreeNodePtr modifierListIn, ParseTreeNodePtr returnTypeNode,
                                       ParseTreeNodePtr nameNode, ParseTreeNodePtr parameters)
{
    if (const auto list = dynamic_cast<const NodeList*>(modifierListIn))
    {
        for (const auto node : list->Children())
        {
            if (const auto attr = dynamic_cast<const IntAttributeNode*>(node))
            {
                if (attr->_attribute == ParseTreeLatencyAttr)
                {
                    return ParseNestedScope(g_compiler->Create<InlineExternalModuleNode>(
                        attr->_value, modifierListIn, returnTypeNode, nameNode, parameters));
                }
            }
        }
    }

    // This adds a wrapper function that calls the true external function
    // The wrapper handles returning to the correct call site, with the correct invocation instance
    ParseTreeNodePtr statements = ParseBaseList(nullptr);

    ParseTreeNode* modifierList =
        ParseBaseList(ParseFunctionModifier(static_cast<ParseTreeFunctionModifier>(ParseTreeFunctionModifierExternal)));

    // Add the user-specified modifiers
    ParseAppendList(modifierList, modifierListIn);

    return ParseNestedScope(g_compiler->Create<FunctionNode>(modifierList, returnTypeNode, nameNode, parameters,
                                                             statements,
                                                             safe_cast<const IdentifierNode*>(nameNode)->GetValue()));
}

ParseTreeNodePtr ParseConst(ParseTreeNodePtr type) { return g_compiler->Create<ConstNode>(type); }

ParseTreeNodePtr ParseStatic(ParseTreeNodePtr expression) { return g_compiler->Create<StaticNode>(expression); }

ParseTreeNodePtr ParseFanOut(ParseTreeNodePtr count, ParseTreeNodePtr expression)
{
    return g_compiler->Create<FanOutNode>(count, expression);
}

ParseTreeNodePtr ParseBarrier() { return g_compiler->Create<StageNode>(); }

ParseTreeNodePtr ParseConcat(ParseTreeNodePtr body) { return g_compiler->Create<ConcatNode>(body); }

ParseTreeNodePtr ParseReorder(ParseTreeNodePtr body) { return g_compiler->Create<ReorderNode>(body); }

ParseTreeNodePtr ParseTypedef(ParseTreeNodePtr typeNode, ParseTreeNodePtr aliasNode,
                              ParseNamespaceScopePtr namespaceScope, const char* unmangledName)
{
    const Type* const type = typeNode->GetType();
    assert(type);

    const std::string& alias = dynamic_cast<const IdentifierNode*>(aliasNode)->GetValue();
    const auto implicitScope = ToScope(namespaceScope);
    const std::string flattendAlias = FlattenScopeAndAppendName(implicitScope, alias);

    g_compiler->RegisterTypedef(flattendAlias, type);

    // Return a node which will be used to ensure the typedef is properly scoped
    return g_compiler->Create<UnregisterTypedefNode>(flattendAlias);
}

ParseTreeNodePtr ParseStaticAssert(ParseTreeNodePtr expr) { return g_compiler->Create<StaticAssertNode>(expr); }

ParseTreeNodePtr ParseExportType(ParseTreeNodePtr attr, ParseTreeNodePtr type)
{
    return g_compiler->Create<ExportTypeNode>(attr, type);
}

ParseTreeNodePtr ParseExtern(ParseTreeNodePtr attr, ParseTreeNodePtr identifier)
{
    assert(dynamic_cast<const TypeNode*>(identifier));

    return g_compiler->Create<ExternNode>(identifier, attr);
}

ParseTreeNodePtr ParseStringType() { return g_compiler->Create<TypeNode>(g_compiler->GetStringType()); }

ParseTreeNodePtr ParseStringLiteral(const char* str) { return g_compiler->Create<LiteralStringNode>(str); }

ParseTreeNodePtr ParseInterpolatedString(ParseTreeNodePtr segments)
{
    return g_compiler->Create<InterpolatedStringNode>(safe_cast<const NodeList*>(segments));
}

ParseTreeNodePtr ParseInterpolatedStringSegment(ParseTreeNodePtr str, ParseTreeNodePtr exp)
{
    return g_compiler->Create<InterpolatedStringSegmentNode>(safe_cast<const LiteralStringNode*>(str), exp);
}

ParseTreeNodePtr ParseInterpolationExpression(ParseTreeNodePtr exp, ParseTreeNodePtr alignment,
                                              ParseTreeFormatSpecifier format, unsigned int precision)
{
    return g_compiler->Create<InterpolationExpressionNode>(exp, alignment, format, precision);
}

ParseTreeNodePtr ParseVoidType() { return g_compiler->Create<TypeNode>(g_compiler->GetVoidType()); }

ParseTreeNodePtr ParseBoolType() { return g_compiler->Create<TypeNode>(g_compiler->GetBoolType()); }

ParseTreeNodePtr ParseBoolLiteral(BOOL b) { return g_compiler->Create<BoolNode>(b); }

ParseTreeNodePtr ParseFloatType() { return g_compiler->Create<TypeNode>(g_compiler->GetFloatType(32)); }

ParseTreeNodePtr ParseFloatLiteral(float f) { return g_compiler->Create<FloatNode>(f); }

ParseTreeNodePtr ParseIntType(int width)
{
    return g_compiler->Create<TypeNode>(g_compiler->GetLeafType(BaseType::Int, width, g_currentTokenLocation));
}

ParseTreeNodePtr ParseDecimalLiteral(const char* n) { return g_compiler->Create<IntegerNode>(n); }

ParseTreeNodePtr ParseUintType(int width)
{
    return g_compiler->Create<TypeNode>(g_compiler->GetLeafType(BaseType::Uint, width, g_currentTokenLocation));
}

ParseTreeNodePtr ParseIdentifier(const char* str) { return g_compiler->Create<IdentifierNode>(str); }

ParseTreeNodePtr ParseNamespace(ParseTreeNodePtr target, ParseTreeNodePtr body)
{
    return g_compiler->Create<NamespaceNode>(target, body);
}

ParseTreeNodePtr ParseScopedIdentifier(ParseTreeNodePtr identifier)
{
    return g_compiler->Create<ScopedIdentifierNode>(identifier);
}

ParseTreeNodePtr ParseScopedIdentifierFromString(const std::string& str)
{
    return ParseScopedIdentifier(ParseBaseList(ParseIdentifier(str.c_str())));
}

ParseTreeNodePtr ParseNamedVariableFromString(const std::string& str)
{
    return ParseNamedVariable(ParseScopedIdentifierFromString(str));
}

void SetNodeType(ParseTreeNodePtr node, ParseTreeNodePtr typeNode)
{
    const Type* const type = safe_cast<const TypeNode*>(typeNode)->GetType();

    node->SetFrontEndType(type);
}

void SetLocation(Location location)
{
    g_currentTokenLocation = location;
    g_currentTokenLocation._valid = true;
}

void SetLocation2(const Location* location) { SetLocation(*location); }

// Some nodes do not have a location associated with them. In these cases,
// the location of the last node is used (assigned through SetLocation) with
// the valid flag set to false.
void UnknownLocation() { g_currentTokenLocation._valid = false; }

BOOL InitCompiler(const Options* options)
{
    try
    {
        SetupCodeGenConfig(*options);
        g_compiler = new Compiler();

        g_compiler->Init();
    }
    catch (const std::exception& e)
    {
        std::cout << e.what() << std::endl;
        return false;
    }

    return true;
}

uint64_t SignExtend(const uint64_t input, const size_t bitCount)
{
    assert(bitCount <= 64);
    assert(bitCount > 0);

    const size_t shiftAmount = (64 - bitCount);

    return static_cast<uint64_t>(static_cast<int64_t>(input << shiftAmount) >> shiftAmount);
}

// Returns the absolute value of a literal - assumes it is a signed number
mp_int AbsLiteral(const Literal& src)
{
    mp_int result = src._value;

    // Check sign bit
    if (bit_test(result, src._width - 1))
    {
        result = ImplementUnaryOp(src, true, ParseTreeUnaryOpTypeNegate, src._width + 1);
    }

    return result;
}

// Converts an input mp_int & width to an output width - taking into account sign extension
mp_int Widen(const Literal& srcIn, const bool signExtend, const size_t dstWidth)
{
    mp_int result = srcIn._value;

    assert(srcIn._width > 0);

    if (signExtend && bit_test(srcIn._value, srcIn._width - 1))
    {
        for (size_t i = srcIn._width; i < dstWidth; i++)
        {
            bit_set(result, i);
        }
    }

    return result;
}

// Sets all high bits to 0
mp_int Truncate(const mp_int& src, const size_t numBits)
{
    const mp_int mask = (mp_int(1) << numBits) - 1;

    return src & mask;
}

// Truncate or Widen input Literal to an output width, taking into account sign extension
mp_int Resize(const Literal& srcIn, const bool signExtend, const size_t dstWidth)
{
    return Truncate(Widen(srcIn, signExtend, dstWidth), dstWidth);
}

// Convert a literal value from one type to another
// {Sign, Zero}-extend to widen, truncate to narrow
mp_int TypeConvert(const mp_int& srcIn, const Type* const srcType, const Type* const dstType)
{
    mp_int result = srcIn;

    if (srcType->GetBitWidth() < dstType->GetBitWidth())
    {
        const LeafType* const srcLeafType = dynamic_cast<const LeafType*>(srcType);

        // Can be null during type checking if the source code has a type error
        if (srcLeafType)
        {
            const Literal lit = {srcIn, srcType->GetBitWidth()};

            result = Widen(lit, srcLeafType->_baseType == BaseType::Int, dstType->GetBitWidth());
        }
    }
    else if (srcType->GetBitWidth() > dstType->GetBitWidth())
    {
        result = Truncate(srcIn, dstType->GetBitWidth());
    }

    return result;
}

mp_int ImplementUnaryOp(const Literal& srcIn, const bool signExtendSrc, const ParseTreeUnaryOpType type,
                        const size_t dstWidth)
{
    // Determine an intermediate width that is long enough
    // to ensure that this behaves as if the source has infinite width
    const size_t intermediateWidth = std::max<size_t>(srcIn._width, dstWidth) + 1;

    // Widen the source to the intermediate width
    const mp_int src = Widen(srcIn, signExtendSrc, intermediateWidth);

    // flip all bits
    mp_int result(0);

    for (size_t i = 0; i < intermediateWidth; ++i)
    {
        if (!bit_test(src, i))
        {
            bit_set(result, i);
        }
    }

    switch (type)
    {
    case ParseTreeUnaryOpTypeInvert:
    case ParseTreeUnaryOpTypeLogicalInvert:
        break;

    case ParseTreeUnaryOpTypeNegate:
        result = result + 1;
        break;

    default:
        assert(false);
    }

    // Ensure bits beyond dstWidth are not set
    return Truncate(result, dstWidth);
}

mp_int ImplementBinaryOp(const Literal& src0In, const Literal& src1In, const size_t dstWidth,
                         const bool signExtendSource0, const bool signExtendSource1, const ParseTreeBinaryOpType type)
{
    // Some parts of this code assume a positive width
    assert(src0In._width > 0);
    assert(src1In._width > 0);
    assert(dstWidth > 0);

    // Increase source width
    size_t requiredSourceWidth = 0;

    switch (type)
    {
    case ParseTreeBinaryOpTypeAdd:
    case ParseTreeBinaryOpTypeMul:
    case ParseTreeBinaryOpTypeLutMul:
    case ParseTreeBinaryOpTypeDiv:
    case ParseTreeBinaryOpTypeMod:
    case ParseTreeBinaryOpTypeSub:
    case ParseTreeBinaryOpTypeAnd:
    case ParseTreeBinaryOpTypeLogicalAnd:
    case ParseTreeBinaryOpTypeOr:
    case ParseTreeBinaryOpTypeLogicalOr:
    case ParseTreeBinaryOpTypeXor:
    case ParseTreeBinaryOpTypeLogicalXor:
    case ParseTreeBinaryOpTypeShl:
        requiredSourceWidth = dstWidth;
        break;

    case ParseTreeBinaryOpTypeShr:
        // Sign-extend the pre-shift value such that
        // all "dstWidth" bits will be valid after the shift
        requiredSourceWidth = dstWidth + MpToSizeT(src1In._value);
        break;

    case ParseTreeBinaryOpTypeEQ:
    case ParseTreeBinaryOpTypeNE:
    case ParseTreeBinaryOpTypeGT:
    case ParseTreeBinaryOpTypeGE:
    case ParseTreeBinaryOpTypeLT:
    case ParseTreeBinaryOpTypeLE:
        requiredSourceWidth = std::max<size_t>(src0In._width, src1In._width);
        break;

    default:
        assert(false);
    }

    const mp_int src0 = Widen(src0In, signExtendSource0, requiredSourceWidth);
    const mp_int src1 = Widen(src1In, signExtendSource1, requiredSourceWidth);

    bool src0Positive = signExtendSource0 ? !bit_test(src0In._value, src0In._width - 1) : true;
    bool src1Positive = signExtendSource1 ? !bit_test(src1In._value, src1In._width - 1) : true;
    bool src1IsZero = (src1In._value == 0);

    mp_int result = 0;

    switch (type)
    {
    case ParseTreeBinaryOpTypeAdd:
        result = src0 + src1;
        break;

    case ParseTreeBinaryOpTypeMul:
    case ParseTreeBinaryOpTypeLutMul:
        result = src0 * src1;
        break;

    case ParseTreeBinaryOpTypeDiv:
        // denominator must not be zero
        if (src1IsZero)
        {
            // Same as Boost did.
            throw std::overflow_error("Division by zero.");
        }

        if (src0Positive)
        {
            result = src0 / src1;
        }
        else
        {
            // Compute the divide operation with the absolute value of the numerator
            // src0In._width + 1 because the abs operation can add 1 bit
            const Literal negatedResult = {AbsLiteral(src0In) / src1, src0In._width + 1};

            // Negate the result
            result = ImplementUnaryOp(negatedResult, false, ParseTreeUnaryOpTypeNegate, dstWidth);
        }
        break;

    case ParseTreeBinaryOpTypeMod:
        // denominator must not be zero
        if (src1IsZero)
        {
            // Same as Boost did.
            throw std::overflow_error("Division by zero.");
        }

        if (src0Positive)
        {
            result = src0 % src1;
        }
        else
        {
            // Compute the mod operation with the absolute value of the numerator
            const Literal negatedResult = {AbsLiteral(src0In) % src1, src1In._width};

            // Negate the result
            result = ImplementUnaryOp(negatedResult, false, ParseTreeUnaryOpTypeNegate, dstWidth);
        }
        break;

    case ParseTreeBinaryOpTypeSub:
        result = src0 - src1;
        break;

    case ParseTreeBinaryOpTypeAnd:
    case ParseTreeBinaryOpTypeLogicalAnd:
        result = src0 & src1;
        break;

    case ParseTreeBinaryOpTypeOr:
    case ParseTreeBinaryOpTypeLogicalOr:
        result = src0 | src1;
        break;

    case ParseTreeBinaryOpTypeXor:
    case ParseTreeBinaryOpTypeLogicalXor:
        result = src0 ^ src1;
        break;

    case ParseTreeBinaryOpTypeShl:
        assert(src1Positive);
        result = src0 << MpToSizeT(src1);
        break;

    case ParseTreeBinaryOpTypeShr:
        assert(src1Positive);
        result = src0 >> MpToSizeT(src1);
        break;

    case ParseTreeBinaryOpTypeEQ:
        result = (src0 == src1) ? 1 : 0;
        break;

    case ParseTreeBinaryOpTypeNE:
        result = (src0 != src1) ? 1 : 0;
        break;

    case ParseTreeBinaryOpTypeGT:
        if (src0Positive && src1Positive)
        {
            result = (src0 > src1) ? 1 : 0;
        }
        else if (src0Positive && !src1Positive)
        {
            result = 1;
        }
        else if (!src0Positive && src1Positive)
        {
            result = 0;
        }
        else
        {
            assert(!src0Positive && !src1Positive);
            result = (src0 <= src1) ? 0 : 1;
        }
        break;

    case ParseTreeBinaryOpTypeGE:
        if (src0Positive && src1Positive)
        {
            result = (src0 >= src1) ? 1 : 0;
        }
        else if (src0Positive && !src1Positive)
        {
            result = 1;
        }
        else if (!src0Positive && src1Positive)
        {
            result = 0;
        }
        else
        {
            assert(!src0Positive && !src1Positive);
            result = (src0 < src1) ? 0 : 1;
        }
        break;

    case ParseTreeBinaryOpTypeLT:
        if (src0Positive && src1Positive)
        {
            result = (src0 < src1) ? 1 : 0;
        }
        else if (src0Positive && !src1Positive)
        {
            result = 0;
        }
        else if (!src0Positive && src1Positive)
        {
            result = 1;
        }
        else
        {
            assert(!src0Positive && !src1Positive);
            result = (src0 >= src1) ? 0 : 1;
        }
        break;

    case ParseTreeBinaryOpTypeLE:
        if (src0Positive && src1Positive)
        {
            result = (src0 <= src1) ? 1 : 0;
        }
        else if (src0Positive && !src1Positive)
        {
            result = 0;
        }
        else if (!src0Positive && src1Positive)
        {
            result = 1;
        }
        else
        {
            assert(!src0Positive && !src1Positive);
            result = (src0 > src1) ? 0 : 1;
        }
        break;

    default:
        assert(false);
    }

    return Truncate(result, dstWidth);
}

// Generates an object name for an array element Used for arrays of objects
std::string GetArrayElementObjectName(const std::string& baseName, const size_t elementIndex)
{
    return baseName + "_element_" + std::to_string(elementIndex);
}

std::string FunctionDesc::GetParameterList() const
{
    std::string parameters = "(";
    for (size_t i = 0; i < _parameterTypes.size(); ++i)
    {
        parameters += ((i == 0) ? "" : ",") + _parameterTypes[i]->GetName();
        parameters += (i < _parameterNames.size()) ? (" " + _parameterNames[i]) : "";
    }
    parameters += std::string(!_hasVariableArguments ? "" : ((_parameterTypes.size() == 0) ? "..." : ", ..."));
    parameters += ")";
    return parameters;
}

TypeCheckKnownValueContext::TypeCheckKnownValueContext(TypeCheckContext& context) : _context(context) {}

KnownValue TypeCheckKnownValueContext::LookupKnownValueForSymbol(const Scope& scope, const std::string& name,
                                                                 const Location& location)
{
    const TypeCheckData& typeCheckData = _context.LookupSymbol(scope, name, location);

    return typeCheckData._value;
}

void TypeCheckKnownValueContext::SetSymbolKnownValue(const Scope& scope, const std::string& name,
                                                     const KnownValue& knownValue, const Location& location)
{
    TypeCheckData symbolTableEntry = _context.LookupSymbol(scope, name, location);

    assert(symbolTableEntry._value._type == KnownValueType::None);

    symbolTableEntry._value = knownValue;

    _context.UpdateSymbol(scope, name, symbolTableEntry, location);
}

VisitCheckKnownValueContext::VisitCheckKnownValueContext(VisitContext& context) : _context(context) {}

KnownValue VisitCheckKnownValueContext::LookupKnownValueForSymbol(const Scope& scope, const std::string& name,
                                                                  const Location& location)
{
    KnownValue result = {};

    // Some tree traversals do not visit all nodes
    // If a declare node was not visited, then there will be no entry in the symbol table
    // In which case, this returns KnownValueType::None
    if (_context.ContainsSymbol(scope, name, location))
    {
        const VisitContextPerVariableData& symbolTableEntry = _context.LookupSymbol(scope, name, location);

        result = symbolTableEntry._knownValue;
    }

    return result;
}

void VisitCheckKnownValueContext::SetSymbolKnownValue(const Scope& scope, const std::string& name,
                                                      const KnownValue& knownValue, const Location& location)
{
    VisitContextPerVariableData symbolTableEntry = _context.LookupSymbol(scope, name, location);

    assert(symbolTableEntry._knownValue._type == KnownValueType::None);

    symbolTableEntry._knownValue = knownValue;

    _context.UpdateSymbol(scope, name, symbolTableEntry, location);
}

KnownValue NopCheckKnownValueContext::LookupKnownValueForSymbol(const Scope& scope, const std::string& name,
                                                                const Location& location)
{
    const KnownValue result = {};

    return result;
}

void NopCheckKnownValueContext::SetSymbolKnownValue(const Scope& scope, const std::string& name,
                                                    const KnownValue& knownValue, const Location& location)
{
}

void InlineExternalModuleNode::TypeCheck(TypeCheckContext& context) const
{
    _returnTypeNode->TypeCheck(context);

    // Don't type-check the name, it will look in the symbol table

    _paramsNode->TypeCheck(context);

    // This pass registers function names
    if (TypeCheckPass::Functions == context._global._pass)
    {
        const std::vector<const ParseTreeNode*>& params = dynamic_cast<const NodeList*>(_paramsNode)->Children();

        FunctionDesc functionDesc = {};

        functionDesc._returnType = _returnTypeNode->GetType();

        for (const ParseTreeNode* const param : params)
        {
            const DeclareNode* const paramDeclareNode = dynamic_cast<const DeclareNode*>(param);

            functionDesc._parameterTypes.push_back(paramDeclareNode->GetDeclaredType());

            functionDesc._parameterNames.push_back(paramDeclareNode->GetDeclaredName());
        }

        functionDesc._fixedLatency = _latency;
        functionDesc._modifiers = GetModifiers();
        functionDesc._hasVariableArguments = false;
        functionDesc._protectionModifier = ParseTreeMemberProtectionModifierPublic;

        const std::string registrationName = context._global._externClassInstanceNameStack.empty()
                                                 ? GetName()
                                                 : GetCombinedExternalClassInstanceFunctionName(
                                                       context._global._externClassInstanceNameStack.top(), GetName());

        g_compiler->RegisterFunction(context._global._classStack.top(), context.GetNamespaceScope(), registrationName,
                                     functionDesc, _location);
    }
}

void InlineExternalModuleNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _returnTypeNode->Visit(visitFunction, context);

        _nameNode->Visit(visitFunction, context);

        _paramsNode->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

ParseTreeFunctionModifier InlineExternalModuleNode::GetModifiers() const
{
    ParseTreeFunctionModifier modifiers =
        ParseTreeFunctionModifierExternalFixedLatency | ParseTreeFunctionModifierExternalExternallyInstantiated;

    if (const auto list = dynamic_cast<const NodeList*>(_modifierList))
    {
        for (const auto node : list->Children())
        {
            if (const auto attr = dynamic_cast<const FunctionModifierNode*>(node))
            {
                modifiers = modifiers | attr->_modifier;
            }
        }
    }

    return modifiers;
}

void StaticAssertNode::TypeCheck(TypeCheckContext& context) const
{
    // Shared variable values are not available in the `functions` pass
    if (context._global._pass == TypeCheckPass::Functions)
    {
        return;
    }

    _expr->TypeCheck(context);

    const BoolType* const exprType = dynamic_cast<const BoolType*>(_expr->GetType());

    if (exprType == nullptr)
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidType) << "static_assert parameter must have type: bool";
    }
}

void StaticAssertNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _expr->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

// Returns the user-specified name to export this class with
std::optional<std::string> ExportTypeNode::GetNameOverride() const
{
    std::optional<std::string> result;

    for (auto listEntry : dynamic_cast<const NodeList*>(_attributeList)->Children())
    {
        if (auto attributeNode = dynamic_cast<const AttributeNode*>(listEntry))
        {
            if (attributeNode->_attribute == ParseTreeNameAttr)
            {
                assert(!result);
                result = dynamic_cast<const LiteralStringNode*>(attributeNode->_value)->GetValue();
            }
        }
    }

    return result;
}

void ExportTypeNode::TypeCheck(TypeCheckContext& context) const
{
    if (const auto classType = dynamic_cast<const ClassType*>(_typeNode->GetType()))
    {
        classType->SetExported(GetNameOverride());
    }

    _typeNode->TypeCheck(context);
}

void ExportTypeNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _typeNode->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

void ExternNode::TypeCheck(TypeCheckContext& context) const
{
    // Check to see if a name attribute is attached
    std::optional<std::string> nameAttribute;

    for (auto listEntry : dynamic_cast<const NodeList*>(_attributes)->Children())
    {
        if (auto attributeNode = dynamic_cast<const AttributeNode*>(listEntry))
        {
            if (attributeNode->_attribute == ParseTreeNameAttr)
            {
                assert(!nameAttribute);
                nameAttribute = dynamic_cast<const LiteralStringNode*>(attributeNode->_value)->GetValue();
            }
        }
    }

    if (const auto typeNode = dynamic_cast<const TypeNode*>(_identifier))
    {
        if (const auto classType = dynamic_cast<const ClassType*>(typeNode->GetType()))
        {
            const_cast<ClassType*>(classType)->SetExternal(nameAttribute);
        }
        else if (const auto externClassTemplateType = dynamic_cast<const ExternClassTemplateType*>(typeNode->GetType()))
        {
            // A template is declared external
            // Mark each instance of that template as external
            const_cast<ExternClassTemplateType*>(externClassTemplateType)->SetExternal(nameAttribute);
        }

        typeNode->TypeCheck(context);
    }
}

void ExternNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _identifier->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

void NamespaceNode::TypeCheck(TypeCheckContext& context) const
{
    // Don't typecheck the identifier, it will try to find the namespace in the symbol table
    const std::string& namespaceName = GetName();

    TypeCheckContext::PushPopScope pushScope(context, &namespaceName);

    _bodyNode->TypeCheck(context);
}

void NamespaceNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _identifierNode->Visit(visitFunction, context);

        _bodyNode->Visit(visitFunction, context);
    };

    const std::string& namespaceName = GetName();

    VisitContext::PushPopScope pushScope(context, &namespaceName);

    visitFunction(this, callback);
}

const std::string& NamespaceNode::GetName() const
{
    return dynamic_cast<const IdentifierNode*>(_identifierNode)->GetValue();
}

void TryResolveInitializerListFunctions(const std::string& initializedObjectName,
                                        const std::string& initializerObjectName, const ParseTreeNode* const node,
                                        ObjectToClassCallableFunctionMap& objectToCallableFunctions)
{
    const BaseInitializerListNode* initializerList = dynamic_cast<const BaseInitializerListNode*>(node);

    if (initializerList)
    {
        initializerList->ResolveFunctions(initializedObjectName, initializerObjectName, objectToCallableFunctions);
    }
}

InitializerListNode::InitializerListNode(const ParseTreeNode* const list) : _list(safe_cast<const NodeList*>(list)) {}

void InitializerListNode::TypeCheck(TypeCheckContext& context) const
{
    const std::vector<const ParseTreeNode*>& children = _list->Children();

    // The type of an initializer list is derived from the context
    if (context._global._expectedTypeStack.empty())
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidInitializerList)
            << "Initializer list used in a context where type cannot be inferred";
    }
    else
    {
        const Type* const expectedType = context._global._expectedTypeStack.top();

        // Initializer lists can be used for arrays, memories, structs, and unions
        const ArrayTypeBase* const arrayType = dynamic_cast<const ArrayTypeBase*>(expectedType);

        const StructUnionType* const structUnionType = dynamic_cast<const StructUnionType*>(expectedType);

        if (arrayType)
        {
            PushPopExpectedType pushPopExpectedType(context, arrayType->_elementType);

            // Initializer list most not contain more elements than the array size
            if (children.size() > arrayType->_arraySize)
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidInitializerList)
                    << "Initializer list contains too many elements (" << children.size()
                    << ").  Expected no more than (" << arrayType->_arraySize << ")";
            }

            // Each element in the initializer list must have the correct type
            for (size_t i = 0; i < children.size(); i++)
            {
                children[i]->TypeCheck(context);
                Type::TypeCheckConversion(arrayType->_elementType, children[i]->GetType(), _location);
            }
        }
        else if (structUnionType)
        {
            // Assigning a initializer list with multiple elements to a union is not allowed
            if ((structUnionType->_type == ContainerType::Union) && (children.size() > 1))
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidInitializerList)
                    << "Unions cannot be initialized with a non-empty initializer list";
            }

            if (children.size() > structUnionType->_members.size())
            {
                g_compiler->ErrorStream(_location, CompileError::InvalidInitializerList)
                    << "Initializer list contains too many elements (" << children.size()
                    << ").  Expected no more than (" << structUnionType->_members.size() << ")";
            }
            else
            {
                // Each element in the initializer list must have the correct type
                for (size_t i = 0; i < children.size(); i++)
                {
                    PushPopExpectedType pushPopExpectedType(context,
                                                            structUnionType->_members[i].second->GetDeclaredType());
                    children[i]->TypeCheck(context);
                    Type::TypeCheckConversion(structUnionType->_members[i].second->GetDeclaredType(),
                                              children[i]->GetType(), _location);
                }
            }
        }
        else if (!children.empty())
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidInitializerList)
                << "Non-empty initializer list cannot be assigned to type: " << expectedType->GetName();
        }

        SetType(expectedType);
    }
}

void InitializerListNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _list->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

const ParseTreeNode* InitializerListNode::TryGetField(const size_t index, const std::string& name) const
{
    const ParseTreeNode* result = nullptr;

    const std::vector<const ParseTreeNode*>& members = Children();

    if (index < members.size())
    {
        result = members[index];
    }

    return result;
}

std::map<std::string, std::string>
InitializerListNode::GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                   const std::string& resultBaseName, const std::string& thisObjectName,
                                   const ClassType* const thisClassType) const
{
    // Currently, structures are the only container that can hold references
    const StructUnionType* const structType = dynamic_cast<const StructUnionType*>(GetType());

    assert(structType);
    assert(structType->_type == ContainerType::Struct);

    const std::vector<const ParseTreeNode*>& children = Children();

    assert(children.size() <= structType->_members.size());

    std::map<std::string, std::string> result;

    for (size_t i = 0; i < children.size(); i++)
    {
        const ParseTreeNode* const child = children[i];

        if (ContainsReference(child->GetType()))
        {
            const std::string baseName = GetMemberName(resultBaseName, structType->_members[i].first);

            const std::map<std::string, std::string> childResult =
                child->GetReferences(resolveReferenceCallback, baseName, thisObjectName, thisClassType);

            for (const auto& p : childResult)
            {
                SafeInsert(result, p.first, p.second);
            }
        }
    }

    return result;
}

void InitializerListNode::ResolveFunctions(const std::string& initializedObjectName,
                                           const std::string& initializerObjectName,
                                           ObjectToClassCallableFunctionMap& objectToCallableFunctions) const
{
    const std::vector<const ParseTreeNode*>& children = Children();

    for (size_t i = 0; i < children.size(); i++)
    {
        const std::string arrayElementObjectName = GetArrayElementObjectName(initializedObjectName, i);

        TryResolveInitializerListFunctions(arrayElementObjectName, initializerObjectName, children[i],
                                           objectToCallableFunctions);
    }
}

DesignatedInitializerListNode::DesignatedInitializerListNode(const ParseTreeNode* const list)
{
    for (const ParseTreeNode* const child : safe_cast<const NodeList*>(list)->Children())
    {
        const DesignatorNode* const designator = safe_cast<const DesignatorNode*>(child);

        const std::string& name = designator->GetName();

        _children[name] = designator;
    }
}

void DesignatedInitializerListNode::TypeCheck(TypeCheckContext& context) const
{
    // The type of an initializer list is derived from the context
    if (context._global._expectedTypeStack.empty())
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidInitializerList)
            << "Designated initializer list used in a context where type cannot be inferred";
    }
    else
    {
        const Type* const expectedType = context._global._expectedTypeStack.top();

        // Initializer lists can be used for structs, unions and classes
        const StructUnionType* const structUnionType = dynamic_cast<const StructUnionType*>(expectedType);

        const ClassType* const classType = dynamic_cast<const ClassType*>(expectedType);

        if (structUnionType)
        {
            // Each element in the initializer list must reference a valid field name and have the correct type
            for (const auto& p : _children)
            {
                const std::string& fieldName = p.first;
                const ParseTreeNode* const fieldValue = p.second->GetValue();

                // Search for this field
                for (const auto& member : structUnionType->_members)
                {
                    if (member.first == fieldName)
                    {
                        // Update _expectedTypeStack for the inner type check
                        PushPopExpectedType pushPopExpectedType(context, member.second->GetDeclaredType());

                        fieldValue->TypeCheck(context);

                        Type::TypeCheckConversion(member.second->GetDeclaredType(), fieldValue->GetType(), _location);
                    }
                }
            }
        }
        else if (classType)
        {
            for (const auto& p : _children)
            {
                // Update _expectedTypeStack for the inner type check
                for (const ClassType::EntryType& member : classType->GetMemberVariables())
                {
                    if (member.first == p.first)
                    {
                        PushPopExpectedType pushPopExpectedType(context, member.second->GetDeclaredType());

                        p.second->TypeCheck(context);
                    }
                }
            }
        }
        else
        {
            g_compiler->ErrorStream(_location, CompileError::InvalidInitializerList)
                << "Designated initializer list cannot be assigned to type: " << expectedType->GetName();
        }

        SetType(expectedType);
    }
}

void ExternClassTemplateType::AddClassType(ClassType* const classType)
{
    _classes.insert(classType);

    classType->SetTemplate(this);
}

void ExternClassTemplateType::SetExternal(const std::optional<std::string>& nameAttr)
{
    for (const ClassType* const classType : _classes)
    {
        const_cast<ClassType*>(classType)->SetExternal(nameAttr);
    }
}

// Called when this initializer list is used as a default initializer list
void DesignatedInitializerListNode::TypeCheckDefaultInitializer(TypeCheckContext& context) const
{
    for (const auto& p : _children)
    {
        p.second->TypeCheck(context);
    }
}

void DesignatedInitializerListNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        for (const auto& p : _children)
        {
            p.second->Visit(visitFunction, context);
        }
    };

    visitFunction(this, callback);
}

const ParseTreeNode* DesignatedInitializerListNode::TryGetField(const size_t index, const std::string& name) const
{
    const ParseTreeNode* result = nullptr;

    const auto it = _children.find(name);

    if (it != _children.end())
    {
        result = it->second->GetValue();
    }

    return result;
}

std::map<std::string, std::string>
DesignatedInitializerListNode::GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                             const std::string& resultBaseName, const std::string& thisObjectName,
                                             const ClassType* const thisClassType) const
{
    std::map<std::string, std::string> result;

    // Designated initializer lists cannot contain references
    for (const auto& p : _children)
    {
        const ParseTreeNode* const fieldValue = p.second->GetValue();

        assert(!ContainsReference(fieldValue->GetType()));
    }

    return result;
}

std::string DesignatedInitializerListNode::PrettyPrint() const
{
    std::vector<std::string> terms;

    for (const auto& p : _children)
    {
        std::ostringstream str;

        str << "." << p.first << " = " << p.second->GetValue()->PrettyPrint();
    }

    return std::string("{") + join(terms, ",") + std::string("}");
}

void DesignatedInitializerListNode::ResolveFunctions(const std::string& initializedObjectName,
                                                     const std::string& initializerObjectName,
                                                     ObjectToClassCallableFunctionMap& objectToCallableFunctions) const
{
    for (const auto& p : _children)
    {
        const std::string& fieldName = p.first;
        const ParseTreeNode* const fieldValue = p.second->GetValue();

        if (dynamic_cast<const FunctionType*>(fieldValue->GetType()))
        {
            // Lookup the ClassCallableFunctionMap for the object being initialized
            ClassCallableFunctionMap& initializedCcfm = objectToCallableFunctions[initializedObjectName];

            // Field is a callback
            // Check to see if the callback value is defined by an outer
            // declaration list.  If so, then that definition takes priority
            if (initializedCcfm.end() == initializedCcfm.find(fieldName))
            {
                // Lookup the ClassCallableFunctionMap for the object that contains the initializer list
                const ClassCallableFunctionMap& initializerCcfm = objectToCallableFunctions[initializerObjectName];

                // For .field = foo
                // Use the initializer object name and ccfm to resolve "foo" to a concrete function
                const ClassCallableFunction ccf = safe_cast<const CallbackRValNode*>(fieldValue)
                                                      ->DescribeCall(initializerObjectName, initializerCcfm);

                SafeInsert(initializedCcfm, fieldName, ccf);
            }
        }
        else if (dynamic_cast<const ClassType*>(fieldValue->GetType()) || dynamic_cast<const ArrayType*>(fieldValue->GetType()))
        {
            // Field is a member object
            const std::string memberName = CombineObjectAndMemberName(Scope(), initializedObjectName, fieldName);

            TryResolveInitializerListFunctions(memberName, initializerObjectName, fieldValue,
                                               objectToCallableFunctions);
        }
    }
}

bool DesignatedInitializerListNode::ResolveFunction(const std::string& initializedObjectName,
                                                    const std::string& initializerObjectName,
                                                    const std::string& callbackName,
                                                    const FunctionType* const callbackType,
                                                    ObjectToClassCallableFunctionMap& objectToCallableFunctions) const
{
    for (const auto& p : _children)
    {
        if (p.first == callbackName)
        {
            // Lookup the ClassCallableFunctionMaps for the object that contains the initializer list
            // and the object that contains the callback
            const ClassCallableFunctionMap& initializerCcfm = objectToCallableFunctions[initializerObjectName];

            ClassCallableFunctionMap& initializedCcfm = objectToCallableFunctions[initializedObjectName];

            // For .field = foo
            // Use the initializer object name and ccfm to resolve "foo" to a concrete function
            const ClassCallableFunction ccf = safe_cast<const CallbackRValNode*>(p.second->GetValue())
                                                  ->DescribeCall(initializerObjectName, initializerCcfm);

            if (!Type::TypeCheckConversion(ccf._functionNode->GetType(), callbackType, _location))
            {
                // Add additional diagnostic information (object names)
                g_compiler->ErrorStream(_location, CompileError::InvalidType)
                    << "Callback: " << callbackName << " type: " << callbackType->GetName()
                    << " is incompatible with the type of the function specified in the default initializer: "
                    << ccf._functionNode->GetType()->GetName() << ".  Outer object: " << initializerObjectName
                    << " inner object: " << initializedObjectName;
            }

            SafeInsert(initializedCcfm, callbackName, ccf);

            return true;
        }
    }

    return false;
}

void ScopedIdentifierNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _identifierNode->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

std::string ScopedIdentifierNode::GetName() const { return ScopeFromNodeList(_identifierNode).front(); }

Scope ScopedIdentifierNode::GetScope() const
{
    auto s = ScopeFromNodeList(_identifierNode);
    // remove name to leave just scope
    s.pop_front();
    return s;
}

std::string ScopedIdentifierNode::GetFlatenedName() const { return FlattenScopeAndAppendName(GetScope(), GetName()); }

void ScopedIdentifierNode::Rename(const std::string& newName)
{
    const NodeList* const nodeList = dynamic_cast<const NodeList*>(_identifierNode);
    assert(nodeList);

    // Renaming never applies to fully-qualified references
    const std::vector<const ParseTreeNode*>& children = nodeList->Children();
    assert(children.size() == 1);

    const_cast<IdentifierNode*>(dynamic_cast<const IdentifierNode*>(children[0]))->Rename(newName);
}

void ThisNode::TypeCheck(TypeCheckContext& context) const
{
    // the type of "this" is a refererence to the current class
    assert(!context._global._classStack.empty());

    const ClassType* const classType = context._global._classStack.top();

    SetType(g_compiler->GetReferenceType(classType));
}

void FunctionSpecifierNode::TypeCheck(TypeCheckContext& context) const
{
    _object->TypeCheck(context);

    _name->TypeCheck(context);

    // Disallow cross-module {.function = ModuleName::Object.Fn}
    // FunctionSpecifierNode::DescribeCall does not support this scenario
    const Scope objectScope = safe_cast<const LValNode*>(_object)->GetObjectScope();

    if (!objectScope.empty() && (objectScope != context.GetNamespaceScope()))
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidCallback)
            << "Callbacks cannot be connected to methods of module global objects";
    }

    const ScopedIdentifierNode* const nameNode = safe_cast<const ScopedIdentifierNode*>(_name);

    const ClassType* const containingClassType = context._global._classStack.top();

    const UnresolvedCall ur = {
        nameNode->GetName(),
        nameNode->GetScope(),
        _object,
        containingClassType,
        context.GetNamespaceScope(),
        "" // not needed because a FunctionNode is not required from ResolveFunctionCall
    };

    const ResolvedCall resolvedCall = ResolveFunctionCall(ur, _location, false);

    SetType(g_compiler->GetFunctionType(
        resolvedCall._functionDesc._parameterTypes, resolvedCall._functionDesc._parameterNames,
        resolvedCall._functionDesc._returnType, resolvedCall._functionDesc._modifiers,
        resolvedCall._functionDesc._fixedLatency, resolvedCall._functionDesc._isLastParameterIndex));
}

void FunctionSpecifierNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _object->Visit(visitFunction, context);

        _name->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

const ParseTreeNode* const FunctionSpecifierNode::GetObject() const { return _object; }

const ScopedIdentifierNode* const FunctionSpecifierNode::GetFunctionName() const
{
    return safe_cast<const ScopedIdentifierNode*>(_name);
}

ClassCallableFunction
FunctionSpecifierNode::DescribeCall(const std::string& containingObjectName,
                                    const ClassCallableFunctionMap& classCallableFunctionMap) const
{
    const LValNode* const objectLValNode = safe_cast<const LValNode*>(_object);

    const ResolveReferenceFunction resolveReferences = [](const std::string& str) { return str; };

    NopCheckKnownValueContext knownValueContext;

    ClassCallableFunction ccf = {};

    ccf._objectName = CombineObjectAndMemberName(
        safe_cast<const LValNode*>(_object)->GetObjectScope(), containingObjectName,
        objectLValNode->GetObjectName(knownValueContext, containingObjectName, resolveReferences));
    ccf._classType = safe_cast<const ClassType*>(_object->GetType());
    ccf._functionNode =
        GetFunctionByName(ccf._classType, GetFunctionName()->GetScope(), GetFunctionName()->GetName(), _location);

    return ccf;
}

void DesignatorNode::TypeCheck(TypeCheckContext& context) const
{
    // Don't type check the name, because it may not correspond to a symbol in the context

    _value->TypeCheck(context);
}

void DesignatorNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _name->Visit(visitFunction, context);

        _value->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

const std::string& DesignatorNode::GetName() const { return safe_cast<const IdentifierNode*>(_name)->GetValue(); }

const ParseTreeNode* DesignatorNode::GetValue() const { return _value; }

void DefaultInitializerNode::TypeCheckImpl(TypeCheckContext& context) const
{
    _initializerList->TypeCheckDefaultInitializer(context);
}

void DefaultInitializerNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _initializerList->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

void TemplateArgNode::TypeCheck(TypeCheckContext& context) const
{
    // Don't type-check arguments, front-end has done all type checking
    // integers may not have proper types (auto)
}

void TemplateArgNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        // _arg can be null for some non-external class templates
        if (_arg)
        {
            _arg->Visit(visitFunction, context);
        }
    };

    visitFunction(this, callback);
}

TemplateArgument TemplateArgNode::Serialize() const
{
    TemplateArgument result = {};

    result._name = _name;

    const IntegerNode* const integerNode = dynamic_cast<const IntegerNode*>(_arg);
    const LiteralStringNode* const stringNode = dynamic_cast<const LiteralStringNode*>(_arg);

    std::ostringstream str;

    if (integerNode)
    {
        result._type = TemplateArgumentType::IntegerLiteral;
        str << integerNode->GetUntypedValue();
    }
    else if (stringNode)
    {
        result._type = TemplateArgumentType::StringLiteral;
        str << "\"" << stringNode->GetValue() << "\"";
    }
    else
    {
        g_compiler->ErrorStream(_location, CompileError::InvalidTemplateArgument)
            << "Unsupported external class template argument.  The only supported types are integers and strings.";
    }

    result._literalValue = str.str();

    return result;
}

const QualifiedNameNode* const TemplateInstanceNode::GetQualifiedName() const
{
    return safe_cast<const QualifiedNameNode*>(_qualifiedName);
}

void TemplateInstanceNode::TypeCheck(TypeCheckContext& context) const
{
    _qualifiedName->TypeCheck(context);

    _args->TypeCheck(context);
}

void TemplateInstanceNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _qualifiedName->Visit(visitFunction, context);

        _args->Visit(visitFunction, context);
    };

    visitFunction(this, callback);
}

std::list<TemplateArgument> TemplateInstanceNode::GetTemplateArguments() const
{
    std::list<TemplateArgument> result;

    const std::vector<const ParseTreeNode*>& nodes = safe_cast<const NodeList*>(_args)->Children();

    for (const ParseTreeNode* const node : nodes)
    {
        const TemplateArgNode* const templateArg = safe_cast<const TemplateArgNode*>(node);

        result.push_back(templateArg->Serialize());
    }

    return result;
}

void InterpolationExpressionNode::TypeCheck(TypeCheckContext& context) const
{
    if (_expression)
    {
        _expression->TypeCheck(context);
    }

    if (_alignmentNode)
    {
        _alignmentNode->TypeCheck(context);

        const IntegerNode* const integerNode = safe_cast<const IntegerNode*>(_alignmentNode);

        _alignment = atoi(integerNode->GetValueString().c_str());
    }
}

void InterpolationExpressionNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _expression->Visit(visitFunction, context);

        if (_alignmentNode)
        {
            _alignmentNode->Visit(visitFunction, context);
        }
    };

    visitFunction(this, callback);
}

std::string InterpolationExpressionNode::PrettyPrint() const
{
    std::string result = "{";

    if (_expression)
    {
        result += _expression->PrettyPrint();
    }

    result += "}";

    return result;
}

InterpolatedStringSegmentNode::InterpolatedStringSegmentNode(const LiteralStringNode* const str,
                                                             const ParseTreeNode* const exp)
    : _string(str), _expression(exp ? safe_cast<const InterpolationExpressionNode*>(exp) : nullptr)
{
}

void InterpolatedStringSegmentNode::TypeCheck(TypeCheckContext& context) const
{
    _string->TypeCheck(context);

    if (_expression)
    {
        _expression->TypeCheck(context);
    }
}

void InterpolatedStringSegmentNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]()
    {
        _string->Visit(visitFunction, context);

        if (_expression)
        {
            _expression->Visit(visitFunction, context);
        }
    };

    visitFunction(this, callback);
}

std::string InterpolatedStringSegmentNode::PrettyPrint() const
{
    std::string result = _string->GetValue();

    if (_expression)
    {
        result += _expression->PrettyPrint();
    }

    return result;
}

void InterpolatedStringNode::TypeCheck(TypeCheckContext& context) const
{
    _segments->TypeCheck(context);

    SetType(g_compiler->GetStringType());
}

void InterpolatedStringNode::Visit(const VisitFunction& visitFunction, VisitContext& context) const
{
    const auto callback = [&]() { _segments->Visit(visitFunction, context); };

    visitFunction(this, callback);
}

std::string InterpolatedStringNode::PrettyPrint() const { return _segments->PrettyPrint(); }

std::map<std::string, std::string> ThisNode::GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                           const std::string& resultBaseName,
                                                           const std::string& thisObjectName,
                                                           const ClassType* const thisClassType) const
{
    std::map<std::string, std::string> result;

    result[resultBaseName] = thisObjectName;

    return result;
}

std::string FlattenScope(const Scope& scope)
{
    std::string str;

    bool isFirst = true;

    for (const std::string& s : scope)
    {
        if (!isFirst)
        {
            str += "::";
        }

        str += s;

        isFirst = false;
    }

    return str;
}

std::string FlattenScopeAndAppendName(const Scope& scope, const std::string& name)
{
    std::string str;

    if (scope.empty())
    {
        str = name;
    }
    else
    {
        str = FlattenScope(scope) + "::" + name;
    }

    return str;
}

std::string GetCombinedExternalClassInstanceFunctionName(const std::string& moduleName, const std::string& functionName)
{
    return moduleName + "::" + functionName;
}

// For flat functions, returns: scope::name
// for member functions, returns: name
// for extern modules, return scope::name
std::string FlattenScopeAndAppendFunctionName(const ClassType* const classType, const Scope& scope,
                                              const std::string& name)
{
    return (classType == g_compiler->GetGlobalClassType()) ? FlattenScopeAndAppendName(scope, name) : name;
}

Scope CombineScopes(const Scope& lhsIn, const Scope& rhsIn)
{
    Scope lhs = lhsIn;
    Scope rhs = rhsIn;

    lhs.splice(lhs.end(), rhs);

    return lhs;
}

bool ScopesEqual(const Scope& lhs, const Scope& rhs)
{
    if (lhs.size() != rhs.size())
    {
        return false;
    }

    auto lhsIt = lhs.begin();
    auto rhsIt = rhs.begin();

    while (lhsIt != lhs.end())
    {
        if (*lhsIt != *rhsIt)
        {
            return false;
        }

        ++lhsIt;
        ++rhsIt;
    }

    return true;
}

// Converts A::B::C into: (Scope(A, B), C)
ScopeAndName StringToScopeAndName(const std::string& str)
{
    std::vector<std::string> tokens;

    std::string workingString = str;

    while (!workingString.empty())
    {
        const size_t pos = workingString.find("::");

        if (pos != std::string::npos)
        {
            tokens.push_back(workingString.substr(0, pos));
            workingString = workingString.substr(pos + 2);
        }
        else
        {
            tokens.push_back(workingString);
            workingString = "";
        }
    }

    ScopeAndName result;

    if (tokens.size() > 1)
    {
        result.first = Scope(tokens.data(), tokens.data() + tokens.size() - 1);
    }

    result.second = tokens[tokens.size() - 1];

    return result;
}

// If explicit scope is specified then search in that scope.
// If explicit scope is empty then search in implicit scope and its ancestors, e.g.:
//
// namespace X
// {
//   namespace Y
//   {
//     Foo();
//   }
// }
// implicitScope = X::Y
// explicitScope = empty
//
// returns [X::Y, X]
// Callers can use this to search for the correct symbol
std::list<Scope> GetScopeSearchList(const Scope& implicitScope, const Scope& explicitScope)
{
    if (!explicitScope.empty())
    {
        return {explicitScope};
    }

    std::list<Scope> result;

    // Search through all posibilities
    // starting with implicitScope::explicitScope
    // and remove entries from implicitScope 1 by 1
    // This searches the innermost namespace first
    Scope currOuterScope = implicitScope;

    while (true)
    {
        const Scope combinedScope = CombineScopes(currOuterScope, explicitScope);

        result.push_back(combinedScope);

        if (currOuterScope.empty())
        {
            break;
        }

        currOuterScope.pop_back();
    }

    return result;
}

// Determines if an integer value is negative
// this is true if the type is signed integer, and the sign bit is set
bool IsNegative(const mp_int& value, const Type* const type)
{
    const LeafType* const indexLeafType = dynamic_cast<const LeafType*>(type);

    return ((indexLeafType->_baseType == BaseType::Int) && bit_test(value, indexLeafType->_width - 1));
}

std::string GetStaticLocalInstanceNameWithoutObject(const size_t instanceUniqueId)
{
    std::ostringstream str;
    str << "__static_instance_" << instanceUniqueId;
    return str.str();
}

std::string GetStaticLocalInstanceName(const std::string& staticVariableName, const size_t instanceUniqueId)
{
    return CombineObjectAndMemberName(Scope(), GetStaticLocalInstanceNameWithoutObject(instanceUniqueId),
                                      staticVariableName);
}

std::string GetMemberName(const std::string& containerName, const std::string& containedName)
{
    return containerName + "." + containedName;
}

// Returns true if the input type contains a reference type
bool ContainsReference(const Type* const type)
{
    bool result = false;

    const auto visitCallback = [&](const Type* const type)
    {
        const ReferenceType* const referenceType = dynamic_cast<const ReferenceType*>(type);

        if (referenceType)
        {
            result = true;
        }
    };

    type->VisitTypes(visitCallback, VisitTypesBehavior::Default);

    return result;
}

// Given a string that was output from PrettyPrint()
// Convert to a form that will be usable as an identifier in generated code
const struct
{
    std::string _original;
    std::string _modified;
} g_replacementTable[] = {
    // Longer source strings come first
    // to ensure they are replaced before substrings of the longer strings
    // for example, "&&" should be mapped to "_and_"
    // rather than "_and_and_"
    {"&&", "_and_"},
    {"||", "_or_"},
    {"^^", "_xor_"},
    {"<<", "_shl_"},
    {">>", "_shr_"},
    {"==", "_eq_"},
    {"!=", "_ne_"},
    {">=", "_ge_"},
    {"<=", "_le_"},

    {">", "_gt_"},
    {"<", "_lt_"},

    {"(", "_"},
    {")", "_"},
    {"{", "_"},
    {"}", "_"},
    {"[", "_"},
    {"]", "_"},
    {".", "_"},
    {",", "_"},
    {"+", "_plus_"},
    {"-", "_minus_"},
    {"*", "_times_"},
    {"/", "_div_"},
    {"%", "_mod_"},
    {"&", "_and_"},
    {"|", "_or_"},
    {"^", "_xor_"},
    {"~", "not_"},
    {"!", "not_"},

    {" ", "_"},

    // After all other replacements,
    // replace runs of underscore with a single underscore
    {"__", "_"},
};

std::string PrettyToIdentifier(const std::string& str)
{
    std::string result = str;

    for (const auto& replacement : g_replacementTable)
    {
        // Make all possible replacements
        while (true)
        {
            const size_t location = result.find(replacement._original);

            if (location != std::string::npos)
            {
                result.erase(location, replacement._original.size());

                result.insert(location, replacement._modified);
            }
            else
            {
                break;
            }
        }
    }

    return result;
}

bool FunctionTypeParamNode::HasLastAttribute() const
{
    if (_attr)
    {
        const std::vector<const ParseTreeNode*>& children = safe_cast<const NodeList*>(_attr)->Children();

        for (const ParseTreeNode* const child : children)
        {
            const FlagAttributeNode* const flagAttribute = safe_cast<const FlagAttributeNode*>(child);

            if (DECLARE_FLAG_END_TRANSACTION == flagAttribute->_attribute)
            {
                return true;
            }
        }
    }

    return false;
}

SetParseLocation::SetParseLocation(const Location& location) : _saved(g_currentTokenLocation)
{
    g_currentTokenLocation = location;
}

SetParseLocation::~SetParseLocation() { g_currentTokenLocation = _saved; }
