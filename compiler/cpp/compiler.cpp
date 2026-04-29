// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

Compiler* g_compiler = nullptr;

#include <circt/Support/LLVM.h>

// Redirect assertions to a function that will throw an exception on release builds
#include "ship_assert.h"

Compiler::Compiler()
    : _root(nullptr), _compileError(false), _doneParsing(false), _permanentNodeCount(0), _contextPtr(nullptr),
      _isCompilingToVerilog(false)
{
    // Initialize is in Init() to ensure that compiler singleton is available during initialization
}

Compiler::~Compiler()
{
    // Ensure that all parse tree nodes are destroyed
    // before _functionNodes destructor is called.
    // Destroying a function node will call methods on _functionNodes.
    FreeCleanupList(_permanentParseTreeCleanupList);

    FreeCleanupList(_temporaryParseTreeCleanupList);

    assert(_functionNodes.empty());
}

void Compiler::Init()
{
    _mlirContext = std::make_unique<mlir::MLIRContext>(mlir::MLIRContext::Threading::DISABLED);

    LoadDialects(*_mlirContext.get());

    _voidType = Create<VoidType>();

    _stringType = Create<StringType>();

    _boolType = Create<BoolType>();

    _floatTypes[32] = Create<FloatType>(32);

    // there is 1 global class (for global variables and flat functions)
    // and there is 1 object of that class

    // Note that member variable/function lists are not used for the global class
    MemberVariableList emptyMemberVariableList;
    MemberFunctionList emtpyMemberFunctionList;

    _globalClassType = Create<ClassType>(g_globalClassName, emptyMemberVariableList, emtpyMemberFunctionList, nullptr);

    RegisterIntrinsics();
}

mlir::MLIRContext* Compiler::GetMlirContext() { return _mlirContext.get(); }

void Compiler::RegisterIntrinsics()
{
    const ParseTreeFunctionModifier inlineIntrinsicMask =
        static_cast<ParseTreeFunctionModifier>(ParseTreeFunctionModifierInline | ParseTreeFunctionModifierIntrinsic);

    // All intrinsics are in the global namespace
    const Scope globalScope;
    Location loc = {0};

    {
        FunctionDesc desc = {};

        desc._returnType = GetVoidType();
        desc._modifiers = inlineIntrinsicMask;
        desc._hasVariableArguments = true;
        desc._protectionModifier = ParseTreeMemberProtectionModifierPublic;

        RegisterFunction(GetGlobalClassType(), globalScope, "__print", desc, loc);
    }

    {
        FunctionDesc desc = {};

        desc._returnType = GetVoidType();
        desc._modifiers = inlineIntrinsicMask;
        desc._hasVariableArguments = true;
        desc._protectionModifier = ParseTreeMemberProtectionModifierPublic;

        RegisterFunction(GetGlobalClassType(), globalScope, "__debug_view", desc, loc);
    }

    {
        FunctionDesc desc = {};

        desc._returnType = GetVoidType();
        desc._parameterTypes.push_back(GetBoolType());
        desc._modifiers = inlineIntrinsicMask;
        desc._protectionModifier = ParseTreeMemberProtectionModifierPublic;

        RegisterFunction(GetGlobalClassType(), globalScope, "assert", desc, loc);
    }

    {
        FunctionDesc desc = {};

        desc._returnType = GetLeafType(BaseType::Uint, 64, loc);
        desc._modifiers = inlineIntrinsicMask;
        desc._protectionModifier = ParseTreeMemberProtectionModifierPublic;

        RegisterFunction(GetGlobalClassType(), globalScope, "__cycles", desc, loc);
    }

    {
        FunctionDesc desc = {};

        desc._returnType = GetLeafType(BaseType::Uint, 64, loc);
        desc._modifiers = inlineIntrinsicMask;
        desc._protectionModifier = ParseTreeMemberProtectionModifierPublic;

        RegisterFunction(GetGlobalClassType(), globalScope, "__str_cnt", desc, loc);
    }

    {
        FunctionDesc desc = {};

        desc._returnType = GetVoidType();
        desc._parameterTypes.push_back(GetStringType());
        desc._parameterTypes.push_back(GetStringType());
        desc._modifiers = inlineIntrinsicMask;
        desc._protectionModifier = ParseTreeMemberProtectionModifierPublic;

        RegisterFunction(GetGlobalClassType(), globalScope, "__assert_str_eq", desc, loc);
    }
}

void Compiler::SetRoot(const ParseTreeNode* const root, const bool isCompilingToVerilog)
{
    _isCompilingToVerilog = isCompilingToVerilog;

    _root = root;
}

void Compiler::MarkPermanentNodes()
{
    assert(!_doneParsing);
    assert(_permanentNodeCount == 0);

    _doneParsing = true;

    _permanentNodeCount = _parseTreeNodes.size();
}

const ParseTreeNode* Compiler::GetRoot() const { return _root; }

std::string Compiler::GetCallTrace()
{
    std::ostringstream callTrace;

    if (_contextPtr != nullptr)
    {
        // In IR generation, use instance stack from IR context
        GenerateCallTrace(callTrace, *_contextPtr);
    }
    else if (_functionEnumerator)
    {
        // Could be in function instance enumeration
        // attempt to get a call trace from the function instance enumerator
        _functionEnumerator->GenerateCurrentEnumerateFunctionCallTrace(callTrace);
    }

    return callTrace.str();
}

ErrStream Compiler::ErrorStream(const Location& location, const CompileError error)
{
    const std::string callTrace = GetCallTrace();

    _compileError = true;
    return ErrStream(GetSourceFileName(location._fileIndex), location._beginLine, error, callTrace);
}

ErrStream Compiler::WarningStream(const Location& location, const CompileWarning warning)
{
    if (GetCodeGenConfig()._warningsAsErrors)
    {
        _compileError = true;
    }

    return ErrStream(GetSourceFileName(location._fileIndex), location._beginLine, warning);
}

ErrStream Compiler::WarningStream(const CompileWarning warning) { return ErrStream(warning); }

void Compiler::RegisterFunction(const ClassType* const classType, const Scope& scope, const std::string& name,
                                const FunctionDesc& functionDesc, const Location& location)
{
    // Global functions use GetGlobalClassType()
    assert(classType);

    if (functionDesc._modifiers & ParseTreeFunctionModifierAsync)
    {
        if (0 != functionDesc._returnType->GetBitWidth())
        {
            ErrorStream(location, CompileError::InvalidFunctionReturnType) << "Async functions cannot return a value";
        }
    }

    // combine name with scope (for flat functions only)
    const bool isExternal = (ParseTreeFunctionModifierExternal & functionDesc._modifiers);

    const bool prependScope = (classType == GetGlobalClassType());

    const std::string combinedName = prependScope ? FlattenScopeAndAppendName(scope, name) : name;

    FunctionDescMap& functionDescMap = _functionDescMaps[classType];

    const auto it = functionDescMap.find(combinedName);

    if (it != functionDescMap.end())
    {
        ErrorStream(location, CompileError::DuplicateFunction) << "Duplicate function: " << combinedName;
    }

    functionDescMap[combinedName] = functionDesc;
}

const Type* Compiler::RegisterEnum(const Scope& scope, const std::string& name, const LeafType* baseType,
                                   const std::vector<EnumType::EntryType>& constants, const Location& location)
{
    const std::string combinedName = FlattenScopeAndAppendName(scope, name);

    const auto it = _namedTypeMap.find(combinedName);

    if (it != _namedTypeMap.end())
    {
        ErrorStream(location, CompileError::DuplicateType) << "Duplicate type: " << combinedName;
    }

    const EnumType* const enumType = Create<EnumType>(combinedName, baseType, constants, location);

    _namedTypeMap[combinedName] = enumType;

    return enumType;
}

const Type* Compiler::RegisterStructUnion(const Scope& scope, const ContainerType type, const std::string& name,
                                          const MemberVariableList& members, const Location& location)
{
    const std::string combinedName = FlattenScopeAndAppendName(scope, name);

    const auto it = _namedTypeMap.find(combinedName);

    if (it != _namedTypeMap.end())
    {
        ErrorStream(location, CompileError::DuplicateType) << "Duplicate type: " << combinedName;
    }

    const StructUnionType* const structUnionType = Create<StructUnionType>(combinedName, type, members);

    _namedTypeMap[combinedName] = structUnionType;

    return structUnionType;
}

const Type* Compiler::RegisterClass(const Scope& scope, const std::string& name,
                                    const MemberVariableList& memberVariables,
                                    const MemberFunctionList& memberFunctions, const ClassNode* const classNode,
                                    const Location& location)
{
    const std::string combinedName = FlattenScopeAndAppendName(scope, name);

    const auto it = _namedTypeMap.find(combinedName);

    if (it != _namedTypeMap.end())
    {
        ErrorStream(location, CompileError::DuplicateType) << "Duplicate type: " << name;
    }

    const ClassType* const classType = Create<ClassType>(name, memberVariables, memberFunctions, classNode);

    _namedTypeMap[combinedName] = classType;

    return classType;
}

const Type* Compiler::RegisterTemplateInstance(const Scope& scope, const std::string& name)
{
    const std::string combinedName = FlattenScopeAndAppendName(scope, name);

    const auto it = _namedTypeMap.find(combinedName);

    // It is normal for multiple classes in the parse tree
    // to refer to the same template instance
    if (it == _namedTypeMap.end())
    {
        const ExternClassTemplateType* const externClassTemplateType = Create<ExternClassTemplateType>(name);

        _namedTypeMap[combinedName] = externClassTemplateType;
    }
    else
    {
        assert(safe_cast<const ExternClassTemplateType*>(it->second)->GetName() == name);
    }

    return _namedTypeMap.at(combinedName);
}

// namespace N
// {
//     Y::TypeName
// }
//
// explicitScope = Y
// currentScope = N
const Type* Compiler::GetNamedType(const Scope& implicitScope, const Scope& explicitScope, const std::string& name,
                                   const Location& location)
{
    // Search through all possible namespaces
    const std::list<Scope> searchList = GetScopeSearchList(implicitScope, explicitScope);

    for (const Scope& combinedScope : searchList)
    {
        const std::string combinedName = FlattenScopeAndAppendName(combinedScope, name);

        const auto it = _namedTypeMap.find(combinedName);

        if (it != _namedTypeMap.end())
        {
            return it->second;
        }
    }

    // This case occurs for external template classes with no instances
    // Because the named typed is referened by ExternNode
    // but no such type exists
    return nullptr;
}

// namespace X
//{
//    Y::Foo();
//}
// callSiteScope = X
// explictScope = Y
// functionName = Foo
// returns the scope where the function is defined: Y in this case
Scope Compiler::GetFlatFunctionScope(const Scope& callSiteScope, const Scope& explicitScope,
                                     const std::string& functionName, const Location& location)
{
    const FunctionDescMap& functionDescMap = _functionDescMaps[GetGlobalClassType()];

    // Search through all posibilities
    const std::list<Scope> searchList = GetScopeSearchList(callSiteScope, explicitScope);

    for (const Scope& combinedScope : searchList)
    {
        const std::string combinedName = FlattenScopeAndAppendName(combinedScope, functionName);

        if (functionDescMap.find(combinedName) != functionDescMap.end())
        {
            return combinedScope;
        }
    }

    // Check for an extern function that is generated to implement
    // callbacks from an exported class
    if (callSiteScope.empty() && explicitScope.empty() && !_currentCompiledModule._isDefaultPass &&
        _currentCompiledModule._classNodeToCompile && // null during type checking
        (_currentCompiledModule._classNodeToCompile->TryGetExternCallback(functionName)))
    {
        return Scope();
    }

    ErrorStream(location, CompileError::UnknownFunction) << "Undefined function: " << functionName;
    throw std::runtime_error("Undefined function");
}

FunctionDesc Compiler::GetFunctionDesc(const ClassType* const classType, const Scope& scope, const std::string& name,
                                       const Location& location)
{
    // Global functions use GetGlobalClassType()
    assert(classType);

    // combine name with scope
    const std::string combinedName = FlattenScopeAndAppendFunctionName(classType, scope, name);

    FunctionDescMap& functionDescMap = _functionDescMaps[classType];

    const auto it = functionDescMap.find(combinedName);

    FunctionDesc result = {};

    if (it == functionDescMap.end())
    {
        // Check to see if name represents a callback
        const Type* const callbackType = classType->GetCallbackType(scope, name);

        if (callbackType)
        {
            // Callback classType
            result = safe_cast<const FunctionType*>(callbackType)->ToFunctionDesc();
        }
        else if (scope.empty() && (classType == GetGlobalClassType()) &&
                 _currentCompiledModule._classNodeToCompile->TryGetExternCallback(name))
        {
            // Callback of the export class being compiled
            result = safe_cast<const ClassType*>(_currentCompiledModule._classNodeToCompile->GetType())
                         ->GetCallbackType(Scope(), name)
                         ->ToFunctionDesc();

            if (result._fixedLatency)
            {
                // If the callback has the [[latency()]] attribute
                // then add these modifiers to ensure that calls to the function
                // use the fixed-latency external function codepath
                result._modifiers = result._modifiers | ParseTreeFunctionModifierExternalFixedLatency;
                result._modifiers = result._modifiers | ParseTreeFunctionModifierExternalExternallyInstantiated;
            }
        }
        else
        {
            ErrorStream(location, CompileError::UnknownFunction) << "Undefined function: " << combinedName;

            result._returnType = GetVoidType();
        }
    }
    else
    {
        result = it->second;
    }

    return result;
}

const FunctionType* Compiler::TryLookupFunctionType(const ClassType* const classType, const Scope& scope,
                                                    const std::string& name)
{
    // Global functions use GetGlobalClassType()
    assert(classType);

    // combine name with scope
    const std::string combinedName = FlattenScopeAndAppendFunctionName(classType, scope, name);

    FunctionDescMap& functionDescMap = _functionDescMaps[classType];

    const auto it = functionDescMap.find(combinedName);

    if (it != functionDescMap.end())
    {
        return GetFunctionType(it->second._parameterTypes, it->second._parameterNames, it->second._returnType,
                               it->second._modifiers, it->second._fixedLatency, it->second._isLastParameterIndex);
    }
    else
    {
        return nullptr;
    }
}

const FunctionType* Compiler::GetFunctionType(const std::vector<const Type*>& paramTypes,
                                              const std::vector<std::string>& paramNames, const Type* const returnType,
                                              const ParseTreeFunctionModifier modifiers,
                                              const boost::optional<size_t> latency,
                                              const boost::optional<size_t> isLastParameterIndex)
{
    assert(paramTypes.size() == paramNames.size());

    const FunctionTypeDesc ftd = {paramTypes, paramNames, returnType, modifiers, latency, isLastParameterIndex};

    const auto it = _functionDesc.find(ftd);

    const FunctionType* result = nullptr;

    if (it == _functionDesc.end())
    {
        result = Create<FunctionType>(paramTypes, paramNames, returnType, modifiers, latency, isLastParameterIndex);

        _functionDesc[ftd] = result;
    }
    else
    {
        result = it->second;
    }

    return result;
}

const VoidType* Compiler::GetVoidType() const { return _voidType; }

const StringType* Compiler::GetStringType() const { return _stringType; }

const BoolType* Compiler::GetBoolType() const { return _boolType; }

const ClassType* Compiler::GetGlobalClassType() const { return _globalClassType; }

bool Compiler::LeafTypeDesc::operator<(const Compiler::LeafTypeDesc& rhs) const
{
    if (_baseType < rhs._baseType)
    {
        return true;
    }
    else if (_baseType > rhs._baseType)
    {
        return false;
    }
    else
    {
        return _width < rhs._width;
    }
}

const LeafType* Compiler::GetLeafType(const BaseType baseType, const size_t bitCount, const Location& location)
{
    // Left shift operations can produce types with large widths
    // The back-ends will overflow with extreme width registers
    if (bitCount > c_maxRegisterBitWidth)
    {
        ErrorStream(location, CompileError::TypeToWide)
            << "Maximum type width exceeded with type of width: " << bitCount << "\n";
    }

    const LeafTypeDesc ltd = {baseType, bitCount};

    const auto it = _leafTypes.find(ltd);

    const LeafType* result = nullptr;

    if (it == _leafTypes.end())
    {
        result = Create<LeafType>(baseType, bitCount, location);

        _leafTypes[ltd] = result;
    }
    else
    {
        result = it->second;
    }

    return result;
}

const FloatType* Compiler::GetFloatType(const size_t bitCount) const { return _floatTypes.at(bitCount); }

bool Compiler::ArrayTypeDesc::operator<(const Compiler::ArrayTypeDesc& rhs) const
{
    return std::tie(_elementType, _arraySize, _type, _memoryType, _eccFuncName, _autoInitialize) <
           std::tie(rhs._elementType, rhs._arraySize, rhs._type, rhs._memoryType, rhs._eccFuncName,
                    rhs._autoInitialize);
}

const ArrayTypeBase* Compiler::GetArrayType(const Type* const elementType, const size_t arraySize,
                                            const ParseTreeArrayType arrayType, const ParseTreeMemoryType memoryType,
                                            const ScopedIdentifierNode* const eccFuncNameNode,
                                            const bool autoInitialize, const Location& location)
{
    const std::string eccFuncName = eccFuncNameNode ? eccFuncNameNode->GetFlatenedName() : "";

    const ArrayTypeDesc arrayTypeDesc = {elementType, arraySize, arrayType, memoryType, eccFuncName, autoInitialize};

    const auto it = _arrayTypes.find(arrayTypeDesc);

    const ArrayTypeBase* result = nullptr;

    if (it == _arrayTypes.end())
    {
        if (arrayType == ParseTreeArrayTypeDefault)
        {
            assert(memoryType == ParseTreeMemoryTypeDefault);
            assert(!autoInitialize);
            result = Create<ArrayType>(elementType, arraySize, location);
        }
        else
        {
            result = Create<MemoryType>(elementType, arraySize, memoryType, eccFuncNameNode, autoInitialize, location);
        }

        _arrayTypes[arrayTypeDesc] = result;
    }
    else
    {
        result = it->second;
    }

    return result;
}

const ReferenceType* Compiler::GetReferenceType(const Type* const underlyingType)
{
    const auto it = _referenceTypes.find(underlyingType);
    const ReferenceType* result = nullptr;
    if (it == _referenceTypes.end())
    {
        result = Create<ReferenceType>(underlyingType);
        _referenceTypes[underlyingType] = result;
    }
    else
    {
        result = it->second;
    }
    return result;
}

void Compiler::RegisterObjects(const CompiledModule& moduleToCompile)
{
    const ClassType* const topLevelClass =
        moduleToCompile._isDefaultPass ? _globalClassType
                                       : dynamic_cast<const ClassType*>(moduleToCompile._classNodeToCompile->GetType());

    const std::string topLevelObjectName =
        moduleToCompile._isDefaultPass ? g_globalObjectName : moduleToCompile._placeholderObjectName;

    // No callbacks at the top level
    ObjectToClassCallableFunctionMap objectToFunctions;

    topLevelClass->RegisterObjects(RegisterObjectsMode::RegisterObjects, topLevelObjectName,
                                   ObjectPath(), // Root object starts with the empty object path
                                   DefaultObjectCallback, DefaultDefaultInitializeCallback, objectToFunctions);

    if (moduleToCompile._isDefaultPass)
    {
        assert(!moduleToCompile._classNodeToCompile);

        // Register global objects declared in the code
        for (ParseTreeNode* const node : _parseTreeNodes)
        {
            const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);

            if (declareNode)
            {
                declareNode->RegisterObjects();
            }
        }
    }
}

// Sorts exported types such that:
// If type B references type A (for example, B is a struct that has a member of type A)
// then type A must come first in the exported types list
// This is necessary so that backends can output the exported types in order
// and can be sure they will compile
void SortExportedTypes(Program& program)
{
    // The list of types that have not been added to program._exportedTypes
    std::list<const Type*> remainingTypes = program._exportedTypes;

    // The set of types that have been added to program._exportedTypes
    std::set<const Type*> addedTypes;

    program._exportedTypes.clear();

    // Used to avoid iterating into exported classes
    const auto isClassType = [](const Type* const t) { return dynamic_cast<const ClassType*>(t) != nullptr; };

    while (!remainingTypes.empty())
    {
        // See if this type is safe to add now
        std::list<const Type*>::iterator it = remainingTypes.begin();

        while (it != remainingTypes.end())
        {
            // Get the next iterator (in case this entry is to be deleted)
            auto nextIt = it;
            ++nextIt;

            const Type* const candidate = *it;
            bool canAddType = true;

            const auto callback = [&](const Type* const containedType)
            {
                // Empty structs are never exported
                if ((containedType != candidate) && containedType->ShouldExport() && !isClassType(containedType))
                {
                    // Check if containedType has been added
                    if (addedTypes.end() == addedTypes.find(containedType))
                    {
                        // The candidate type references a type that has not yet
                        // been exported
                        canAddType = false;
                    }
                }
            };

            if (!isClassType(candidate))
            {
                candidate->VisitTypes(callback, VisitTypesBehavior::SkipReferences);
            }

            if (canAddType)
            {
                program._exportedTypes.push_back(candidate);

                addedTypes.insert(candidate);

                remainingTypes.erase(it);
            }

            it = nextIt;
        }
    }
}

struct MemoryOrFifo
{
  public:
    void init(const size_t width, const size_t depth, const size_t replicaCount, const bool lutRamAllowed,
              const bool deepRamAllowed, const bool ROM)
    {
        const CodeGenConfig& config = GetCodeGenConfig();
        const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();

        auto lutRamUsage = GetMemoryResourceUsage(deviceConfig._memory._lutRamConfigs, width, depth, replicaCount);
        _lutRamsNeeded = (lutRamAllowed) ? lutRamUsage.first : 0;
        auto blockRamUsage =
            GetMemoryResourceUsage(deviceConfig._memory._blockRamConfigs, width, depth,
                                   // If ROM, can use true dual port thus halve the number of replicas (rounded up)
                                   (ROM) ? (replicaCount + 1) / 2 : replicaCount);
        _blockRamsNeeded = blockRamUsage.first;
        _blockRamUtilization = blockRamUsage.second;
        _lutRamsSavedPerBlock = _lutRamsNeeded / float(_blockRamsNeeded);
        auto deepRamUsage = GetMemoryResourceUsage(deviceConfig._memory._deepRamConfigs, width, depth, replicaCount);
        _deepRamsNeeded = (deepRamAllowed) ? deepRamUsage.first : 0;
        _deepRamUtilization = (deepRamAllowed) ? deepRamUsage.second : 0;
        _blockRamsSavedPerDeep = (deepRamAllowed) ? _blockRamsNeeded / float(_deepRamsNeeded) : 0;
    }

    RegisterDescription* const _registerDesc;
    Function* const _externReturnRouterFunction;

  public:
    MemoryOrFifo(RegisterDescription* const registerDesc)
        : _registerDesc(registerDesc), _externReturnRouterFunction(nullptr)
    {
        if (registerDesc->_type == RegisterType::Memory)
        {
            const size_t replicaCount = registerDesc->Memory().ReplicaCount();

            const bool hasInitialData = !registerDesc->Memory()._initialValues.empty();

            const bool hasOutputRegister = registerDesc->Memory().GetMinReadLatency() >= c_defaultMemoryReadLatency;

            const auto width = registerDesc->Memory()._elementWidth;
            const auto depth = registerDesc->Memory()._elementCount;

            assert(width > 0 && depth > 0 && replicaCount > 0);

            // If the output register is disabled, then bram must be used
            // (Intel FPGAs only support concurrent, well-defined, read/write the same address for BRAM)
            // ECC is not supported on LUTRAM
            const bool lutRamAllowed = hasOutputRegister && !registerDesc->Memory()._ecc;
            // Deep RAMs do not support initial content, and do not use unless it has an output register
            const bool deepRamAllowed = (!hasInitialData && hasOutputRegister);
            // ROMs can be implemented using Block RAMs with true dual port mode
            const bool ROM = (registerDesc->Memory()._writePortCount == 0);

            init(width, depth, replicaCount, lutRamAllowed, deepRamAllowed, ROM);

            if (lutRamAllowed)
            {
                assert(_lutRamsNeeded > 0);
                registerDesc->Memory()._useLutRam = true;
            }
            else
            {
                assert(_blockRamsNeeded > 0);
                registerDesc->Memory()._useBlockRam = true;

                if (deepRamAllowed)
                    assert(_deepRamsNeeded > 0);
            }
        }
        else if (registerDesc->_type == RegisterType::Fifo)
        {
            // Passthrough fifos do not consume resources
            assert(FifoType::Passthrough != registerDesc->Fifo()._type);

            // FIFOs always have an output register
            const bool isDualClock = (registerDesc->Fifo()._readClock != registerDesc->Fifo()._writeClock);

            const auto width = GetPhysicalFifoWidth(*registerDesc);
            const auto depth = registerDesc->Fifo()._depth;
            const size_t replicaCount = 1;

            assert(width > 0 && depth > 0 && replicaCount > 0);

            // FIFOs always have an output register
            // On Intel, using LUTRAM with dual-clock FIFOs causes a timing violation
            const bool lutRamAllowed = !isDualClock;
            // Deep RAMs not currently used as FIFOs
            const bool deepRamAllowed = false;
            const bool ROM = false;

            init(width, depth, replicaCount, lutRamAllowed, deepRamAllowed, ROM);

            if (lutRamAllowed)
            {
                assert(_lutRamsNeeded > 0);
                registerDesc->Fifo()._useLutRam = true;
            }
            else
            {
                assert(_blockRamsNeeded > 0);
            }
        }
        else
        {
            assert(false);
        }
    }

    MemoryOrFifo(Function* const externReturnRouterFunction)
        : _registerDesc(nullptr), _externReturnRouterFunction(externReturnRouterFunction)
    {
        assert(!externReturnRouterFunction->IsAsync());

        const size_t callSiteIndexWidth = externReturnRouterFunction->_syncExtern._callIndexWidth;

        const size_t logDepth = externReturnRouterFunction->_syncExtern.GetLogDepth();

        const size_t width = 1 /* valid */ + 1 /* ready */ + callSiteIndexWidth /* data */;
        const size_t depth = 1ull << logDepth;
        const size_t replicaCount = 1;

        // KanagawaExternReturnRouter must be placed in BRAM (Intel KanagawaHALReadyValidFifo requires it)
        const bool lutRamAllowed = false;
        // Deep RAMs not currently used here
        const bool deepRamAllowed = false;
        const bool ROM = false;

        init(width, depth, replicaCount, lutRamAllowed, deepRamAllowed, ROM);

        externReturnRouterFunction->_syncExtern._useLutRam = false;
    }

    void LutToBlockRam() const
    {
        if (_registerDesc)
        {
            if (_registerDesc->_type == RegisterType::Memory)
            {
                assert(_registerDesc->Memory()._useLutRam);
                _registerDesc->Memory()._useLutRam = false;
                _registerDesc->Memory()._useBlockRam = true;
            }
            else if (_registerDesc->_type == RegisterType::Fifo)
            {
                assert(_registerDesc->Fifo()._useLutRam);
                _registerDesc->Fifo()._useLutRam = false;
            }
            else
            {
                assert(false);
            }
        }
        else if (_externReturnRouterFunction)
        {
            assert(_externReturnRouterFunction->_syncExtern._useLutRam);
            _externReturnRouterFunction->_syncExtern._useLutRam = false;
        }
        else
        {
            assert(false);
        }
    }

    void BlockToDeepRam() const
    {
        assert(_deepRamsNeeded > 0);
        assert(_registerDesc->_type == RegisterType::Memory);

        assert(_registerDesc->Memory()._useBlockRam);
        _registerDesc->Memory()._useBlockRam = false;
    }

    struct descendingLutRamsSavedPerBlock
    {
        bool operator()(const MemoryOrFifo& lhs, const MemoryOrFifo& rhs) const
        {
            return lhs._lutRamsSavedPerBlock > rhs._lutRamsSavedPerBlock;
        }
    };

    struct descendingBlockRamsSavedPerDeep
    {
        bool operator()(const MemoryOrFifo& lhs, const MemoryOrFifo& rhs) const
        {
            return lhs._blockRamsSavedPerDeep > rhs._blockRamsSavedPerDeep;
        }
    };

    size_t _lutRamsNeeded;
    size_t _blockRamsNeeded;
    size_t _blockRamUtilization;
    size_t _deepRamsNeeded;
    size_t _deepRamUtilization;
    float _lutRamsSavedPerBlock;
    float _blockRamsSavedPerDeep;
};

void MapMemoriesAndFifosToRams(Program& program)
{
    const CodeGenConfig& config = GetCodeGenConfig();
    const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();

    const float infinity = std::numeric_limits<float>::infinity();
    const auto maxLutRam = deviceConfig._memory._numLutRam;
    const auto maxBlockRam = (config._maxBlockRam != -1) ? config._maxBlockRam : deviceConfig._memory._numBlockRam;
    const auto maxDeepRam = (config._maxDeepRam != -1) ? config._maxDeepRam : deviceConfig._memory._numDeepRam;

    assert(maxBlockRam > 0);

    const float targetLutToBlockRatio = float(maxLutRam) / maxBlockRam;
    const float targetBlockToDeepRatio = (maxDeepRam > 0) ? float(maxBlockRam) / maxDeepRam : infinity;

    // Sort by descending value for money
    std::multiset<MemoryOrFifo, MemoryOrFifo::descendingLutRamsSavedPerBlock> potentialBlockRams;

    size_t lutRamsUsed = 0;
    size_t blockRamsUsed = 0;

    // Sort by descending value for money
    std::multiset<MemoryOrFifo, MemoryOrFifo::descendingBlockRamsSavedPerDeep> potentialDeepRams;

    // Extract all memories, compute how many lut/block/deep RAMs are needed for each
    for (size_t index = 0; index < program._registerTable.size(); ++index)
    {
        RegisterDescription& regDesc = program._registerTable[index];

        if (regDesc._type == RegisterType::Memory ||
            (regDesc._type == RegisterType::Fifo && GetPhysicalFifoWidth(regDesc) > 0 &&
             // ignore passthrough FIFOs and FIFOs implemented using DSPs
             FifoType::Passthrough != regDesc.Fifo()._type && !regDesc.Fifo()._useDsp))
        {
            MemoryOrFifo i(&regDesc);

            if (i._lutRamsNeeded > 0)
            {
                lutRamsUsed += i._lutRamsNeeded;
                potentialBlockRams.insert(i);
            }
            else
            {
                // LUTRAM not allowed
                blockRamsUsed += i._blockRamsNeeded;

                if (maxDeepRam > 0 && i._deepRamsNeeded > 0)
                    potentialDeepRams.insert(i);
            }
        }
    }

    // Consider KanagawaExternReturnRouter too
    for (Function* const function : program._externFunctions)
    {
        if (!function->IsAsync())
        {
            MemoryOrFifo i(function);

            // LUTRAM not allowed
            assert(!function->_syncExtern._useLutRam);

            blockRamsUsed += i._blockRamsNeeded;
        }
    }

    float lutToBlockRatio = infinity;
    float blockToDeepRatio = infinity;
    size_t deepRamsUsed = 0;

    bool fixed_point = false;
    while (!fixed_point)
    {
        fixed_point = true;

        // Consider moving memories from LUTRAM into BlockRAM in the order of descending
        // value-for-money, continuing as long as we do not exceed the maximum number of
        // Block RAMs
        for (auto it = potentialBlockRams.begin(); it != potentialBlockRams.end() && blockRamsUsed < maxBlockRam;)
        {
            const MemoryOrFifo& cheapest = *it;

            auto newBlockRamsUsed = blockRamsUsed + cheapest._blockRamsNeeded;
            if ((newBlockRamsUsed <= maxBlockRam) && (cheapest._blockRamUtilization >= config._minBlockRamUtilization))
            {
                lutRamsUsed -= cheapest._lutRamsNeeded;
                blockRamsUsed = newBlockRamsUsed;
                lutToBlockRatio = lutRamsUsed / float(blockRamsUsed);

                cheapest.LutToBlockRam();

                if (maxDeepRam > 0 && cheapest._deepRamsNeeded > 0)
                    potentialDeepRams.insert(cheapest);

                it = potentialBlockRams.erase(it);
                fixed_point = false;
            }
            else
            {
                it++;
            }
        }

        // Consider moving memories from BlockRAM into DeepRAM in the order of descending
        // value-for-money, unless it would exceed the maximum number of Deep RAMs
        for (auto it = potentialDeepRams.begin(); it != potentialDeepRams.end() && deepRamsUsed < maxDeepRam;)
        {
            const MemoryOrFifo& cheapest = *it;

            auto newDeepRamsUsed = deepRamsUsed + cheapest._deepRamsNeeded;
            if ((newDeepRamsUsed <= maxDeepRam) && (cheapest._deepRamUtilization >= config._minDeepRamUtilization))
            {
                const MemoryOrFifo& cheapest = *it;

                blockRamsUsed -= cheapest._blockRamsNeeded;
                deepRamsUsed = newDeepRamsUsed;
                blockToDeepRatio = blockRamsUsed / float(deepRamsUsed);

                cheapest.BlockToDeepRam();

                it = potentialDeepRams.erase(it);
                fixed_point = false;

                // After converting 1 BRAM to URAM
                // Try to convert more LUTRAM to BRAM to get more candidates
                // in potentialDeepRams
                break;
            }
            else
            {
                it++;
            }
        }
    }

    if (blockRamsUsed > maxBlockRam)
    {
        // This can occur if some memories/fifos cannot be placed in LUTRAMs
        g_compiler->WarningStream(CompileWarning::TooManyRams)
            << "Unable to find mapping using fewer than " << maxBlockRam << " Block RAMs";
    }

    assert(deepRamsUsed <= maxDeepRam);
}

const Program* Compiler::GenerateIR(const CompiledModule& moduleToCompile)
{
    Program* const program = Create<Program>();

    program->_moduleName = moduleToCompile._moduleName;
    program->_isDefaultPass = moduleToCompile._isDefaultPass;
    program->_exportClass = moduleToCompile._isDefaultPass ? nullptr : moduleToCompile._classNodeToCompile;

    IRContext context;
    context._program = program;
    context._basicBlock = nullptr;
    context._baseFileName = moduleToCompile._baseFileName;

    // Allocate registers for all declared variables, function return values, and function call indices
    for (ParseTreeNode* const node : _parseTreeNodes)
    {
        node->AllocateRegisters(*program);
    }

    if (program->_exportClass)
    {
        // Allocate registers for the export class

        // The placeholder object does not correspond to any variable in the source
        SourceVariable source = {};

        const AllocatedRegister* const exportClassTopLevelReg = program->_exportClass->GetType()->AllocateRegisters(
            *program, RegisterType::Global, moduleToCompile._placeholderObjectName, ObjectPath(), source);
    }

    {
        CompileTimer ct(*this, "Generate IR");

        // Context is only valid for access during IR generation
        // For example, to set condition stack depth of registers allocation during IR generation
        SetContextPtr setContextPtr(*this, &context);

        // Push global scope
        GenerateIRTypeContext::PushPopScope globalScope(context._typeContext);

        // Push the global object name
        PushPopObjectNameStack globalOjbectName(context, g_globalObjectName);

        // Push global class type
        PushPopClassType globalClassType(context, GetGlobalClassType());

        _root->GenerateStatementIR(context);
    }

    // Generate IR for return statements
    for (ParseTreeNode* const node : _parseTreeNodes)
    {
        ReturnNode* const returnNode = dynamic_cast<ReturnNode*>(node);

        if (returnNode)
        {
            returnNode->GenerateConditionalReturns(*(context._program));
        }
    }

    if (GetCodeGenConfig()._stall > 0)
    {
        program->_numStallers = InsertStallLogic(context);
    }

    // Link call sites to target basic blocks
    for (ParseTreeNode* node : _parseTreeNodes)
    {
        CallNode* const call = dynamic_cast<CallNode*>(node);

        if (call)
        {
            CallNode::InstanceMap& instanceLists = call->GetInstances();

            for (auto& p : instanceLists)
            {
                const std::string& callSiteObjectName = p.first;

                // p.second will contain more than 1 entry
                // in case an inline function (A) calls a non-inline function (B)
                // and A is called multiple times
                for (CallNode::Instance& instance : p.second)
                {
                    const ResolveReferenceFunction resolveReferences = [&](const std::string& src)
                    {
                        const FunctionInstance& callerInstance = instance.GetCallSiteFunctionInstance();

                        return g_compiler->GetFunctionInstanceEnumerator().DereferenceParameter(callerInstance, call,
                                                                                                src);
                    };

                    const ResolvedCall resolvedCall = ResolveFunctionCallPostTypeChecking(
                        call, call->GetCallSiteClassType(), call->GetCallSiteScope(), callSiteObjectName,
                        resolveReferences);

                    // Intrinsics have no target function
                    if (resolvedCall._functionNode)
                    {
                        const std::string& functionName = resolvedCall._functionName;

                        const std::string& calledObjectName = instance.GetCalledObjectName();

                        const_cast<FunctionNode*>(resolvedCall._functionNode)
                            ->GetInstance(instance.GetCalledObjectName())
                            .SetReturnBlock(instance.GetCallSiteIndex(), instance.GetReturnBlock());
                    }
                }
            }
        }
    }

    // Link external class callbacks with concrete functions
    LinkExternalClassCallbacks(context);

    // Fail if recv* exports do not have expected modifiers
    CheckExports(context);

    // Validate calls to external functions
    CheckExterns(context);

    // Fail it atomic nesting rules are violated
    CheckAndRewriteAtomics(context);

    // Optimization, scheduling, liveness analysis, add pipeline registers
    // Some dataflow optimizations crash on invalid programs
    // so only execute if no errors have been encountered so far
    if (!HadCompileError())
    {
        // Remove concepts that optimize should not encounter
        RemoveFrontEndConceptsBeforeOptimize(context);

        OptimizeScheduleAndPipeline(context);

        CheckInspection(*program);

        CheckStartingGlobalWrites(*program);

        // Replaces :: with _ to avoid errors in generated code
        FixupNames(*program);

        // Order exported types
        SortExportedTypes(*program);

        MapMemoriesAndFifosToRams(*program);
    }

    return program;
}

void Compiler::FixupNames(Program& program)
{
    for (RegisterDescription& regDesc : program._registerTable)
    {
        regDesc._name = FixupString(regDesc._name);
    }
}

void Compiler::CheckInspection(const Program& program)
{
    Location loc = {0};

    // In HW, inspectable variables are identified via their position in the chain of inspectables.
    // To allow the inspectable hardware to be independent of this position, each inspectable node
    // decrements the incoming ID, and when the ID decrements past zero, that inspectable responds.
    // At runtime, we determine the number of inspectables in the tree by sending two inspectable
    // read requests. The first one flushes the pipeline, and utilizes an ID (position) value that
    // should not exist. For this purpose, we reserve ID (position) value 0xFFFF, therefore the
    // limit to the number of inspectables has been reduced to 0xFFFE.
    if (program._inspectableVariables.size() > 0xFFFE)
    {
        ErrorStream(loc, CompileError::TooManyInspectableVariables) << "Limit of inspectable() statements reached";
    }

    // Validate that no variable is marked for inspection more than once
    // HW backend cannot handle this for memories
    std::set<size_t> inspectableRegs;

    for (const InspectableVariable& inspectableVariable : program._inspectableVariables)
    {
        // Don't consider internally added inspection variables for data races
        if (inspectableVariable._inspectionType != InspectableVariableType::Default)
        {
            continue;
        }

        for (const size_t registerIndex : inspectableVariable._registers)
        {
            const auto it = inspectableRegs.find(registerIndex);

            if (it != inspectableRegs.end())
            {
                ErrorStream(loc, CompileError::InvalidInspectionTarget)
                    << "Variable: " << inspectableVariable._name << " marked for inspection more than once";
            }

            inspectableRegs.insert(registerIndex);

            const RegisterDescription& regDesc = program._registerTable[registerIndex];

            // INSPECTABLE_ELEMENT_INDEX_WIDTH limits memory depth
            if ((regDesc._type == RegisterType::Memory) && (regDesc.Memory()._elementCount > 0xFFFF))
            {
                ErrorStream(loc, CompileError::InspectableTooDeep)
                    << "Memory: " << inspectableVariable._name << " is too deep to support inspection";
            }
        }
    }
}

// Check placement of global writes in first stage relative to starting
// condition and stall check
void Compiler::CheckStartingGlobalWrites(const Program& program)
{
    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            if (basicBlock.HasStartCondition())
            {
                bool startCondition = false;
                bool globalsBefore;
                bool globalWriteBeforeStartCondition = false;
                bool globalWriteAfterStartCondition = false;

                for (const Stage& stage : basicBlock._stages)
                {
                    // Only look at operations in stage 0
                    if (stage._atomicSequence > 0)
                    {
                        break;
                    }

                    for (const Operation& op : stage._operations)
                    {
                        // Prints and asserts also create side effects and are
                        // treated the same as global writes in this check
                        if (op._opcode == Opcode::WriteGlobal || op._opcode == Opcode::Print ||
                            op._opcode == Opcode::Assert)
                        {
                            if (startCondition)
                            {
                                globalWriteAfterStartCondition = true;
                            }
                            else
                            {
                                globalWriteBeforeStartCondition = true;
                            }
                        }
                        else if (op._opcode == Opcode::StartCondition)
                        {
                            globalsBefore = op._flags._startCondition._globalsBefore;
                            startCondition = true;
                        }
                    }
                }

                assert(startCondition);
                if (globalsBefore)
                {
                    // All global writes should occur before start condition.
                    // Check for any writes after start condition.
                    assert(!globalWriteAfterStartCondition);
                }
                else
                {
                    // All global writes should occur after start condition.
                    // Check for any writes before start condition.
                    assert(!globalWriteBeforeStartCondition);
                }
            }
        }
    }
}

void Compiler::AddParseTreeNode(ParseTreeNode* const node) { _parseTreeNodes.push_back(node); }

void Compiler::AddFunctionNode(FunctionNode* const node) { _functionNodes.push_back(node); }

void Compiler::RemoveFunctionNode(FunctionNode* const node)
{
    const auto it = std::find(_functionNodes.begin(), _functionNodes.end(), node);
    assert(it != _functionNodes.end());

    _functionNodes.erase(it);
}

const std::list<ParseTreeNode*>& Compiler::GetParseTreeNodes() { return _parseTreeNodes; }

const std::list<AllocatedLeafRegister*>& Compiler::GetAllocatedLeafRegisters() { return _leafRegisters; }

void Compiler::AddAllocatedLeafRegister(AllocatedLeafRegister* const reg) { _leafRegisters.push_back(reg); }

void Compiler::RemoveAllocatedLeafRegister(AllocatedLeafRegister* const reg)
{
    const auto it = std::find(_leafRegisters.begin(), _leafRegisters.end(), reg);
    assert(it != _leafRegisters.end());

    _leafRegisters.erase(it);
}

void Compiler::TypeCheck()
{
    GetRoot()->ResolveDeferredTypes();

    TypeCheckContext typeCheckContext;

    // Push global scope
    TypeCheckContext::PushPopScope globalScope(typeCheckContext);

    // Push global class
    PushPopClassStack globalClass(typeCheckContext, GetGlobalClassType());

    // This happens three times
    // The first pass registers functions
    // The second pass registers global (non-static) variables
    // The third pass does the real work
    // This enables functions to be called before they are defined
    // and globals to be referenced before they are declared.
    // This also enables the declaration of a global to reference a function (for callbacks)
    // The same typeCheckContext is re-used between passes
    // to enable global variables to be referenced before they are declared
    const TypeCheckPass typeCheckPasses[] = {TypeCheckPass::Functions, TypeCheckPass::Globals, TypeCheckPass::Default};

    for (const TypeCheckPass pass : typeCheckPasses)
    {
        typeCheckContext._global._pass = pass;

        GetRoot()->TypeCheck(typeCheckContext);
        if (HadCompileError())
        {
            throw std::runtime_error("Type checking failed");
        }
    }
}

// For future-proofing, structs and unions cannot share names with functions
void Compiler::CheckTypeNames()
{
    Location loc = {0};

    for (const auto& c : _functionDescMaps)
    {
        for (const auto& p : c.second)
        {
            if (_namedTypeMap.end() != _namedTypeMap.find(p.first))
            {
                ErrorStream(loc, CompileError::DuplicateType) << "Duplicate symbol: " << p.first;
            }
        }
    }
}

void Compiler::ExtractDeviceConfig()
{
    VisitContext visitContext = {};
    VisitContext::PushPopScope globalScope(visitContext);

    CodeGenDeviceConfig deviceConfig;
    DeviceConfigVisitor deviceConfigVisitor(*this, deviceConfig);

    GetRoot()->Visit(
        [&](const ParseTreeNode* const node, const VisitChildrenFunction& visitChildren)
        {
            deviceConfigVisitor.VisitNode(node, visitContext);
            visitChildren();
        },
        visitContext);

    std::vector<std::string> missingProperties;
    bool validateSuccess = deviceConfigVisitor.Finalize(missingProperties);

    if (!validateSuccess)
    {
        std::ostringstream ss;

        if (!missingProperties.empty())
        {
            ss << "Missing device configuration properties:";

            for (const std::string& property : missingProperties)
            {
                ss << "\n  " << property;
            }

            ErrorStream(Location{0}, CompileError::MissingDeviceConfigProperty) << ss.str();

            throw std::runtime_error(ss.str());
        }
        else
        {
            ss << "Invalid device configuration";

            ErrorStream(Location{0}, CompileError::InvalidDeviceConfigProperty) << ss.str();

            throw std::runtime_error(ss.str());
        }
    }

    SetCodeGenDeviceConfig(deviceConfig);
}

void Compiler::DeviceCapabilityCheck()
{
    VisitContext visitContext = {};
    VisitContext::PushPopScope globalScope(visitContext);

    GetRoot()->Visit(
        [&](const ParseTreeNode* const node, const VisitChildrenFunction& visitChildren)
        {
            // Check for ECC memories on devices that do not support ECC
            if (!GetCodeGenDeviceConfig()._eccMemorySupport)
            {
                const auto declareNode = dynamic_cast<const DeclareNode*>(node);
                if (nullptr != declareNode)
                {
                    const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(declareNode->GetDeclaredType());

                    if (memoryType && (ParseTreeMemoryTypeEcc == memoryType->GetMemoryType()))
                    {
                        ErrorStream(declareNode->GetLocation(), CompileError::EccUnsupported)
                            << "Target device does not support ECC memories";
                        _compileError = true;
                    }
                }
            }
            if (!GetCodeGenDeviceConfig()._quadPortRamSupport)
            {
                const auto declareNode = dynamic_cast<const DeclareNode*>(node);
                if (nullptr != declareNode)
                {
                    const MemoryType* const memoryType = dynamic_cast<const MemoryType*>(declareNode->GetDeclaredType());

                    if (memoryType && (ParseTreeMemoryTypeQuadPort == memoryType->GetMemoryType()))
                    {
                        ErrorStream(declareNode->GetLocation(), CompileError::QuadPortUnsupported)
                            << "Target device does not support quad port memories";
                        _compileError = true;
                    }
                }
            }

            visitChildren();
        },
        visitContext
    );

    if (_compileError)
    {
        throw std::runtime_error("Device capability check failed");
    }
}

std::string Compiler::GetSourceFileName(const size_t fileIndex) const
{
    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

    assert(fileIndex < codeGenConfig._fileNames.size());

    return codeGenConfig._fileNames[fileIndex];
}

std::string Compiler::GetSourceFileNameWithoutLeadingPath(const size_t fileIndex) const
{
    return StripLeadingPath(GetSourceFileName(fileIndex));
}

void Compiler::RegisterTypedef(const std::string& alias, const Type* const type)
{
    Location loc = {0};

    const auto it = _namedTypeMap.find(alias);
    if (it != _namedTypeMap.end())
    {
        ErrorStream(loc, CompileError::DuplicateType) << "Duplicate typedef: " << alias;
    }

    _namedTypeMap[alias] = type;
    _typedefs.insert(alias);
}

bool Compiler::IsTypedef(const std::string& name) { return _typedefs.find(name) != _typedefs.end(); }

// Called when a named type goes out of scope
void Compiler::UnregisterNamedType(const std::string& alias)
{
    // A typedef in a nested class may be unregistered multiple times
    // so don't fail if the lookup doesn't find anything
    const auto it = _namedTypeMap.find(alias);

    if (it != _namedTypeMap.end())
    {
        _namedTypeMap.erase(it);
    }
}

bool Compiler::IsTypeName(const std::string& name) const { return _namedTypeMap.end() != _namedTypeMap.find(name); }

void Compiler::IncrementCompileStageTime(const std::string& name, const double sec)
{
    _compileStageTimeSeconds[name] += sec;
}

void Compiler::PrintCompileTime(std::ostream& str)
{
    std::cout << "Step,Time(seconds)\n";

    for (const auto& p : _compileStageTimeSeconds)
    {
        str << p.first << "," << p.second << "\n";
    }
}

FunctionInstanceEnumerator& Compiler::GetFunctionInstanceEnumerator()
{
    assert(_functionEnumerator.get());
    return *(_functionEnumerator.get());
}

// Called when assert() fails (even on release builds)
void OnAssertionFailure(const char* const fileName, const size_t lineNumber)
{
    // Assertions can be disabled at the cmd line (to work-around benign assertions).
    if (GetCodeGenConfig()._releaseAssert)
    {
        std::ostringstream str;
        str << "Internal compiler error: " << fileName << ":" << lineNumber;

        if (g_compiler)
        {
            const std::string callTrace = g_compiler->GetCallTrace();

            str << "\n\n" << callTrace;
        }

        throw RuntimeErrorWithTrace(str.str());
    }
}

CompileTimer::CompileTimer(Compiler& compiler, const std::string& name)
    : _compiler(compiler), _name(name), _beginTime(std::chrono::steady_clock::now())
{
}

CompileTimer::~CompileTimer()
{
    const double time =
        std::chrono::duration_cast<std::chrono::duration<double>>(std::chrono::steady_clock::now() - _beginTime)
            .count();

    _compiler.IncrementCompileStageTime(_name, time);
}

SetContextPtr::SetContextPtr(Compiler& compiler, IRContext* const contextPtr) : _compiler(compiler)
{
    _compiler._contextPtr = contextPtr;
}

SetContextPtr::~SetContextPtr() { _compiler._contextPtr = nullptr; }

// Function functions which modify their parameters
// This is used to disable tracking of known values of
// parameters of inline function calls
void Compiler::FindModifiedParameters()
{
    VisitContext visitContext = {};

    const auto visitCallback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& visitChildren)
    {
        const AssignNode* const assignNode = dynamic_cast<const AssignNode*>(node);

        if (assignNode && !assignNode->IsInitialAssignment())
        {
            // This assignment is modifying the value of a variable
            const VariableAccessNode* const variableAccessNode =
                dynamic_cast<const VariableAccessNode*>(assignNode->GetLhs());

            if (variableAccessNode)
            {
                const std::pair<Scope, std::string> symbolLookupKey = variableAccessNode->GetSymbolLookupKey();

                const Scope scope = symbolLookupKey.first;
                const std::string name = symbolLookupKey.second;

                if (visitContext.ContainsSymbol(scope, name, variableAccessNode->GetLocation()))
                {
                    const VisitContextPerVariableData symbolRecord =
                        visitContext.LookupSymbol(scope, name, variableAccessNode->GetLocation());

                    assert(symbolRecord._declaration);

                    if (symbolRecord._declaration->_isParameter)
                    {
                        const_cast<DeclareNode*>(symbolRecord._declaration)->MarkModifiedParameter();
                    }
                }
            }
        }

        // Recurse
        visitChildren();
    };

    VisitContext::PushPopScope globalScope(visitContext);

    _root->Visit(visitCallback, visitContext);
}

// When an external class is used
// the bodies of all methods should be ignored
// and public methods should be marked as extern
// This is achieved here by replace all method bodies
// and marking public methods as extern
void Compiler::ExternalizeClasses()
{
    for (ParseTreeNode* const node : _parseTreeNodes)
    {
        ClassNode* const classNode = dynamic_cast<ClassNode*>(node);

        if (classNode && classNode->IsExternal())
        {
            classNode->MarkInterfaceFunctions(ParseTreeFunctionModifierExternal);

            classNode->PruneForExportOrExternalClassUse();
        }
    }
}

// The call graph construction code requires that static locals have unique names
// Because they are treated like global variables during call graph construction
// There are 2 sources of collisions that could occur:
// 1) Two locals with the same name, in different functions.  This is handled by prepending the function name to the
// variable name 2) Two locals with the same name, in the same function.  This is handled by appending a unique number
// to each static local in a function
void Compiler::RenameStaticLocals()
{
    VisitContext visitContext = {};

    std::stack<std::string> functionNameStack;

    // Maps declaration to new name
    std::map<const DeclareNode*, std::string> renamingTable;

    // Maps local variable name to # of instances of that name within the current function
    std::map<std::string, size_t> duplicateMap;

    const auto visitCallback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& visitChildren)
    {
        const FunctionNode* const functionNode = dynamic_cast<const FunctionNode*>(node);
        const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);
        const VariableAccessNode* const variableAccessNode = dynamic_cast<const VariableAccessNode*>(node);

        const auto checkRename = [&](const std::string& name) -> boost::optional<std::string>
        {
            if (visitContext.ContainsSymbol(Scope(), name, node->GetLocation()))
            {
                const VisitContextPerVariableData variableData =
                    visitContext.LookupSymbol(Scope(), name, node->GetLocation());
                assert(variableData._declaration);

                const auto it = renamingTable.find(variableData._declaration);
                if (it != renamingTable.end())
                {
                    // Update the name
                    const std::string& newName = it->second;

                    return boost::optional<std::string>(newName);
                }
            }

            return boost::optional<std::string>();
        };

        if (functionNode)
        {
            // Note that this includes the namespace name
            const std::string functionName = functionNode->GetFlattenedName();

            // Nested functions are not allowed
            assert(functionNameStack.empty());
            assert(duplicateMap.empty());

            functionNameStack.push(functionName);
        }
        else if (declareNode)
        {
            if (declareNode->IsStatic())
            {
                // Static locals must be within a function
                assert(!functionNameStack.empty());

                const size_t instanceWithinFunction = duplicateMap[declareNode->GetDeclaredName()]++;

                // Found a static local, rename it
                std::ostringstream newName;
                newName << functionNameStack.top() << "__static_" << declareNode->GetDeclaredName() << "_"
                        << instanceWithinFunction;

                SafeInsert(renamingTable, declareNode, newName.str());

                const_cast<DeclareNode*>(declareNode)->Rename(newName.str());
            }
        }
        else if (variableAccessNode)
        {
            // Static local references never have a scope prepended
            if (variableAccessNode->GetObjectScope().empty())
            {
                const boost::optional<std::string> newName = checkRename(variableAccessNode->GetName());

                if (newName)
                {
                    const_cast<VariableAccessNode*>(variableAccessNode)->Rename(*newName);
                }
            }
        }

        // Recurse
        visitChildren();

        if (functionNode)
        {
            functionNameStack.pop();
            duplicateMap.clear();
        }
    };

    // Push global scope
    VisitContext::PushPopScope globalScope(visitContext);

    _root->Visit(visitCallback, visitContext);

    // Update names in all declarations
    for (const auto& p : renamingTable)
    {
        const DeclareNode* const declaration = p.first;
        const std::string& newName = p.second;

        const_cast<DeclareNode*>(declaration)->Rename(newName);
    }
}

// Reorder the parse tree to move declarations before references
void Compiler::ReorderDeclarations()
{
    VisitContext visitContext = {};

    VisitContext::PushPopScope globalScope(visitContext);

    struct NamespaceRecord
    {
        std::string _namespace;
        std::list<const DeclareNode*> _declarations;

        NamespaceRecord(const std::string& n) : _namespace(n) {}
    };

    std::list<NamespaceRecord> records;

    records.push_back(NamespaceRecord(""));

    std::set<const ParseTreeNode*> nodesToRemove;

    const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
    {
        const FunctionNode* const functionNode = dynamic_cast<const FunctionNode*>(node);
        const ClassNode* const classNode = dynamic_cast<const ClassNode*>(node);
        const NodeList* const nodeList = dynamic_cast<const NodeList*>(node);
        const NamespaceNode* const namespaceNode = dynamic_cast<const NamespaceNode*>(node);
        const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);

        if (functionNode || classNode)
        {
            // stop traversal when a function or class is reached
        }
        else if (namespaceNode)
        {
            // Nested namespaces are not supported
            assert(records.back()._namespace.empty());

            records.push_back(NamespaceRecord(namespaceNode->GetName()));

            recurseCallback();

            records.push_back(NamespaceRecord(""));
        }
        else if (declareNode)
        {
            if (declareNode->GetDeclarationScope() == DeclareNode::DeclarationScope::Global)
            {
                records.back()._declarations.push_back(declareNode);

                nodesToRemove.insert(declareNode);
            }

            recurseCallback();
        }
        else
        {
            recurseCallback();
        }
    };

    _root->Visit(callback, visitContext);

    // Remove declarations from the parse tree that will be added in another location
    const auto removeCallback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
    {
        const FunctionNode* const functionNode = dynamic_cast<const FunctionNode*>(node);
        const ClassNode* const classNode = dynamic_cast<const ClassNode*>(node);
        const NodeList* const nodeList = dynamic_cast<const NodeList*>(node);
        const NamespaceNode* const namespaceNode = dynamic_cast<const NamespaceNode*>(node);
        const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);

        if (functionNode || classNode)
        {
            // stop traversal when a function or class is reached
        }
        else
        {
            if (nodeList)
            {
                nodeList->RemoveNodes(nodesToRemove);
            }

            recurseCallback();
        }
    };

    _root->Visit(removeCallback, visitContext);

    NodeList* const newRoot = g_compiler->Create<NodeList>();

    // Build new parse tree with only namespaces & declarations
    for (const NamespaceRecord& nr : records)
    {
        if (!nr._declarations.empty())
        {
            NodeList* const listNode = g_compiler->Create<NodeList>();

            for (const DeclareNode* const declaration : nr._declarations)
            {
                listNode->Append(declaration);
            }

            const ParseTreeNode* const nodeToAppend =
                nr._namespace.empty() ? listNode : ParseNamespace(ParseIdentifier(nr._namespace.c_str()), listNode);

            newRoot->Append(nodeToAppend);
        }
    }

    newRoot->Append(_root);
    _root = newRoot;
}

std::string Compiler::ClampStringLength(const std::string& stringIn)
{
    std::string result = stringIn;

    const size_t limit = GetCodeGenConfig()._maxStringLength;

    if (result.size() > limit)
    {
        // Compute a hash code of the full string
        std::hash<std::string> hasher;

        const size_t hash = hasher(stringIn);

        // If an entry exists in _clampStringMap[hash], it must be for the same input strings
        // Hash collisions are fatal
        const auto it = _clampStringMap.find(hash);

        if (it != _clampStringMap.end())
        {
            if (stringIn != it->second)
            {
                throw std::runtime_error("String hash collision: " + stringIn + " " + it->second);
            }
        }
        else
        {
            _clampStringMap[hash] = stringIn;
        }

        // Return the first portion of the input string + the hash code
        // The hash code ensures the string is unique
        std::ostringstream str;
        str << stringIn.substr(0, limit) << "_" << hash;

        result = str.str();
    }

    return result;
}

std::list<CompiledModule> Compiler::EnumerateCompiledModules(const std::string& defaultFileName)
{
    std::list<CompiledModule> result;

    // The first compiled module is the default module
    // that compiles all extern/export functions
    {
        CompiledModule defaultModule = {};

        defaultModule._isDefaultPass = true;
        defaultModule._moduleName = GetCodeGenConfig()._moduleName;
        defaultModule._baseFileName = defaultFileName;

        result.push_back(defaultModule);
    }

    // Find export classes
    for (ParseTreeNode* const node : _parseTreeNodes)
    {
        ClassNode* const classNode = dynamic_cast<ClassNode*>(node);

        if (classNode)
        {
            if (classNode->IsExport())
            {
                const ClassType* classType = safe_cast<const ClassType*>(classNode->GetType());

                // Do not compile separately if it is instantiated somewhere (unless Verilog)
                if ((classType->InstanceCount() == 0) || _isCompilingToVerilog)
                {
                    // Export classes are not supported in framework mode
                    CompiledModule classModule = {};

                    const std::string exportedName =
                        dynamic_cast<const ClassType*>(classNode->GetType())->GetExportedName();

                    classModule._isDefaultPass = false;
                    classModule._classNodeToCompile = classNode;
                    classModule._moduleName = exportedName;
                    classModule._baseFileName = defaultFileName + FixupString(exportedName);
                    classModule._placeholderObjectName = g_globalObjectName;

                    result.push_back(classModule);
                }
            }
        }
    }

    return result;
}

const FunctionNode* Compiler::TryLookupFunctionByName(const ClassType* const containingClass,
                                                      const std::string& flattenedName)
{
    const FunctionByNameKey key(containingClass, flattenedName);

    const FunctionNode* result = nullptr;

    const auto it = _functionByName.find(key);

    if (it == _functionByName.end())
    {
        for (const FunctionNode* const fn : _functionNodes)
        {
            if ((containingClass == fn->GetClassType()) && (fn->GetFlattenedName() == flattenedName))
            {
                const FunctionByNameKey key(fn->GetClassType(), fn->GetFlattenedName());

                SafeInsert(_functionByName, key, fn);

                result = fn;
            }
        }
    }
    else
    {
        result = it->second;
    }

    return result;
}

void Compiler::Reset(const CompiledModule& moduleToCompile)
{
    assert(_doneParsing);
    assert(_parseTreeNodes.size() >= _permanentNodeCount);

    // Free parse tree nodes allocated for the previous module
    while (_parseTreeNodes.size() != _permanentNodeCount)
    {
        _parseTreeNodes.pop_back();
    }

    FreeCleanupList(_temporaryParseTreeCleanupList);

    // _functionByName must be rebuilt on each pass
    // because some function nodes it contains could be deleted
    // when _temporaryParseTreeCleanupList is cleared
    _functionByName.clear();

    _currentCompiledModule = moduleToCompile;

    // Reset IR generation state
    for (ParseTreeNode* const node : _parseTreeNodes)
    {
        node->Reset();
    }

    for (auto& p : _namedTypeMap)
    {
        const_cast<Type*>(p.second)->Reset();
    }

    _functionEnumerator.reset();

    _nameToPath.clear();

    if (!moduleToCompile._isDefaultPass)
    {
        // Compiling a specific exported class
        // Remove export/extern from all functions
        // (global functions which are marked export/extern in the source,
        // or functions which had this modifier added when compiling another exported class)
        // Do not remove modifiers from extern class public methods, as these modifiers are added once during
        // compilation.
        for (const ParseTreeNode* const node : _parseTreeNodes)
        {
            const FunctionNode* const function = dynamic_cast<const FunctionNode*>(node);

            if (function && (!function->GetClassType()->IsExternal()))
            {
                const ParseTreeFunctionModifier modifiersToRemove =
                    ParseTreeFunctionModifierExternal | ParseTreeFunctionModifierExport |
                    ParseTreeFunctionModifierExportClassInterface;

                const_cast<FunctionNode*>(function)->RemoveModifiers(modifiersToRemove);
            }
        }

        // Add export to all public members of the export class being compiled
        assert(moduleToCompile._classNodeToCompile);
        moduleToCompile._classNodeToCompile->MarkInterfaceFunctions(ParseTreeFunctionModifierExport |
                                                                    ParseTreeFunctionModifierExportClassInterface);

        // Create extern functions to call when a callback of the class being compiled is called
        moduleToCompile._classNodeToCompile->CreateExternCallbacks();
    }

    // Add extern to all public members of export classes not begin compiled
    for (const ParseTreeNode* const node : _parseTreeNodes)
    {
        ClassNode* const classNode = const_cast<ClassNode*>(dynamic_cast<const ClassNode*>(node));

        if (classNode && classNode->IsExport() && (classNode != moduleToCompile._classNodeToCompile))
        {
            classNode->MarkInterfaceFunctions(ParseTreeFunctionModifierExternal |
                                              ParseTreeFunctionModifierExportClassInterface);

            // Remove portions of the AST which should not be compiled when
            // compiling uses of this export class
            classNode->PruneForExportOrExternalClassUse();
        }
    }
}

const FunctionNode* Compiler::TryGetExportClassCallback(const std::string& name)
{
    return _currentCompiledModule._classNodeToCompile
               ? _currentCompiledModule._classNodeToCompile->TryGetExternCallback(name)
               : nullptr;
}

const CompiledModule& Compiler::GetCurrentModuleToCompile() const { return _currentCompiledModule; }

bool Compiler::IsCompilingToVerilog() const { return _isCompilingToVerilog; }

ObjectPath Compiler::ObjectNameToPath(const std::string& name) { return SafeLookup(_nameToPath, name); }

void Compiler::RegisterObjectPath(const std::string& name, const ObjectPath& path)
{
    SafeInsertIdempotent(_nameToPath, name, path);
}

void Compiler::FreeCleanupList(ParseTreeCleanupList& cleanupList)
{
    for (const DestroyFunction& fn : cleanupList)
    {
        fn();
    }

    cleanupList.clear();
}

std::string CompiledModule::ModifyFileName(const std::string& inputName) const
{
    // Modfies the name of a compiler output file to ensure all output files for unique modules have unique names
    if (_isDefaultPass || inputName.empty())
    {
        return inputName;
    }
    else
    {
        const size_t dot = inputName.find_last_of('.');
        assert(dot != std::string::npos);

        const std::string ext = inputName.substr(dot + 1, inputName.size());
        const std::string base = inputName.substr(0, dot);

        const std::string result = base + _moduleName + "." + ext;

        return result;
    }
}
