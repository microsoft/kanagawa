// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

std::regex GetFloatRegEx();

class AllocatedLeafRegister;
namespace mlir
{
class MLIRContext;
}

struct CompiledModule
{
    bool _isDefaultPass;
    ClassNode* _classNodeToCompile;
    std::string _moduleName;
    std::string _baseFileName;
    std::string _placeholderObjectName;

    std::string ModifyFileName(const std::string& inputName) const;
};

class Compiler
{
  public:
    typedef std::vector<std::pair<std::string, const DeclareNode*>> MemberVariableList;
    typedef std::vector<std::string> MemberFunctionList;

    Compiler();

    ~Compiler();

    void Init();

    // All CIRCT/MLIR objects associated with an invocation of the compiler
    // are associated with 1 MLIR context.  The MLIR context contains uniqued
    // objects (like types), so it is illegal to compare MLIR objects
    // associated with different contexts.
    mlir::MLIRContext* GetMlirContext();

    template <typename T, typename... ConstructorArgsT> T* Create(const ConstructorArgsT&... args);

    void SetRoot(const ParseTreeNode* const root, const bool isCompilingToVerilog);

    void MarkPermanentNodes();

    const ParseTreeNode* GetRoot() const;

    bool HadCompileError() const { return _compileError; }

    ErrStream ErrorStream(const Location& location, const CompileError error);

    ErrStream WarningStream(const Location& location, const CompileWarning warning);

    ErrStream WarningStream(const CompileWarning warning);

    void RegisterFunction(const ClassType* const classType, const Scope& scope, const std::string& name,
                          const FunctionDesc& functionDesc, const Location& location);

    FunctionDesc GetFunctionDesc(const ClassType* const classType, const Scope& scope, const std::string& name,
                                 const Location& location);

    const FunctionType* TryLookupFunctionType(const ClassType* const classType, const Scope& scope,
                                              const std::string& name);

    Scope GetFlatFunctionScope(const Scope& callSiteScope, const Scope& explicitScope, const std::string& functionName,
                               const Location& location);

    const Type* RegisterEnum(const Scope& scope, const std::string& name, const LeafType* baseType,
                             const std::vector<std::pair<std::string, mp_int>>& members, const Location& location);

    const Type* RegisterStructUnion(const Scope& scope, const ContainerType type, const std::string& name,
                                    const MemberVariableList& members, const Location& location);

    const Type* RegisterClass(const Scope& scope, const std::string& name, const MemberVariableList& memberVariables,
                              const MemberFunctionList& memberFunctions, const ClassNode* const classNode,
                              const Location& location);

    const Type* RegisterTemplateInstance(const Scope& scope, const std::string& name);

    const Type* GetNamedType(const Scope& implicitScope, const Scope& explicitScope, const std::string& name,
                             const Location& location);

    const LeafType* GetLeafType(const BaseType baseType, const size_t bitCount, const Location& location);

    const FloatType* GetFloatType(const size_t bitCount) const;

    const ArrayTypeBase* GetArrayType(const Type* const elementType, const size_t arraySize,
                                      const ParseTreeArrayType arrayType, const ParseTreeMemoryType memoryType,
                                      const ScopedIdentifierNode* const eccFuncName, const bool autoInitialize,
                                      const Location& location);

    const FunctionType* GetFunctionType(const std::vector<const Type*>& paramTypes,
                                        const std::vector<std::string>& paramNames, const Type* const returnType,
                                        const ParseTreeFunctionModifier modifiers,
                                        const boost::optional<size_t> latency,
                                        const boost::optional<size_t> isLastParameterIndex);

    const ReferenceType* GetReferenceType(const Type* const underlyingType);

    const VoidType* GetVoidType() const;

    const StringType* GetStringType() const;

    const BoolType* GetBoolType() const;

    const ClassType* GetGlobalClassType() const;

    const Program* GenerateIR(const CompiledModule& moduleToCompile);

    void AddParseTreeNode(ParseTreeNode* const node);

    const std::list<ParseTreeNode*>& GetParseTreeNodes();

    const std::list<AllocatedLeafRegister*>& GetAllocatedLeafRegisters();

    void AddAllocatedLeafRegister(AllocatedLeafRegister* const reg);

    void RemoveAllocatedLeafRegister(AllocatedLeafRegister* const reg);

    void TypeCheck();

    void CheckTypeNames();

    void ExtractDeviceConfig();

    void DeviceCapabilityCheck();

    void RegisterObjects(const CompiledModule& moduleToCompile);

    void ReorderDeclarations();

    bool IsTypeName(const std::string&) const;

    size_t RandomlyRoundFloat(const float input);

    std::string GetSourceFileName(const size_t fileIndex) const;

    std::string GetSourceFileNameWithoutLeadingPath(const size_t fileIndex) const;

    void RegisterTypedef(const std::string& alias, const Type* const type);

    bool IsTypedef(const std::string& name);

    void UnregisterNamedType(const std::string& alias);

    void IncrementCompileStageTime(const std::string& name, const double sec);

    void PrintCompileTime(std::ostream& str);

    FunctionInstanceEnumerator& GetFunctionInstanceEnumerator();

    void RenameStaticLocals();

    void ExternalizeClasses();

    void FindModifiedParameters();

    void EnumerateFunctionInstances(const CompiledModule& moduleToCompile);

    std::string ClampStringLength(const std::string& stringIn);

    std::list<CompiledModule> EnumerateCompiledModules(const std::string& defaultFileName);

    void Reset(const CompiledModule& moduleToCompile);

    const FunctionNode* TryGetExportClassCallback(const std::string& name);

    const CompiledModule& GetCurrentModuleToCompile() const;

    bool IsCompilingToVerilog() const;

    const FunctionNode* TryLookupFunctionByName(const ClassType* const containingClass,
                                                const std::string& flattenedName);

    ObjectPath ObjectNameToPath(const std::string& name);

    void RegisterObjectPath(const std::string& name, const ObjectPath& path);

    void AddFunctionNode(FunctionNode* const node);

    void RemoveFunctionNode(FunctionNode* const node);

    std::string GetCallTrace();

    IRContext* _contextPtr;

  private:
    std::unique_ptr<mlir::MLIRContext> _ctxt;

    void RegisterIntrinsics();

    void CheckInspection(const Program& program);

    void FixupNames(Program& program);

    void CheckStartingGlobalWrites(const Program& program);

    using DestroyFunction = std::function<void(void)>;
    using ParseTreeCleanupList = std::list<DestroyFunction>;

    void FreeCleanupList(ParseTreeCleanupList& cleanupList);

    // For nodes created by the front-end
    // reused across compiled modules
    ParseTreeCleanupList _permanentParseTreeCleanupList;

    // For nodes created by the middle-end
    // reset after each compiled module
    ParseTreeCleanupList _temporaryParseTreeCleanupList;

    std::list<ParseTreeNode*> _parseTreeNodes;

    std::list<FunctionNode*> _functionNodes;

    std::list<AllocatedLeafRegister*> _leafRegisters;

    const ParseTreeNode* _root;

    bool _compileError;

    bool _doneParsing;

    size_t _permanentNodeCount;

    typedef std::unordered_map<std::string, FunctionDesc> FunctionDescMap;

    std::unordered_map<const ClassType*, FunctionDescMap> _functionDescMaps;

    std::unordered_map<std::string, const Type*> _namedTypeMap;

    std::unordered_set<std::string> _typedefs;

    const VoidType* _voidType;

    const StringType* _stringType;

    const BoolType* _boolType;

    const ClassType* _globalClassType;

    struct LeafTypeDesc
    {
        BaseType _baseType;
        size_t _width;

        bool operator<(const LeafTypeDesc& rhs) const;
    };

    struct ArrayTypeDesc
    {
        const Type* _elementType;
        size_t _arraySize;
        ParseTreeArrayType _type;
        ParseTreeMemoryType _memoryType;
        std::string _eccFuncName;
        bool _autoInitialize;

        bool operator<(const ArrayTypeDesc& rhs) const;
    };

    struct FunctionTypeDesc
    {
        std::vector<const Type*> _paramTypes;

        std::vector<std::string> _paramNames;

        const Type* _returnType;

        ParseTreeFunctionModifier _modifiers;

        boost::optional<size_t> _latency;

        boost::optional<size_t> _isLastParameterIndex;

        bool operator<(const FunctionTypeDesc& rhs) const
        {
            return std::tie(_paramTypes, _paramNames, _returnType, _modifiers, _latency, _isLastParameterIndex) <
                   std::tie(rhs._paramTypes, rhs._paramNames, rhs._returnType, rhs._modifiers, rhs._latency,
                            rhs._isLastParameterIndex);
        }
    };

    std::map<LeafTypeDesc, const LeafType*> _leafTypes;

    std::map<size_t, const FloatType*> _floatTypes;

    std::map<ArrayTypeDesc, const ArrayTypeBase*> _arrayTypes;

    std::map<const Type*, const ReferenceType*> _referenceTypes;

    std::map<FunctionTypeDesc, const FunctionType*> _functionDesc;

    // Mapping of compilation step name to compilation time
    std::map<std::string, double> _compileStageTimeSeconds;

    // Maps hash to long string
    std::map<size_t, std::string> _clampStringMap;

    std::map<std::string, ObjectPath> _nameToPath;

    std::unique_ptr<FunctionInstanceEnumerator> _functionEnumerator;

    // Lookup function by containing class type and flattened name
    using FunctionByNameKey = std::pair<const ClassType*, std::string>;

    std::map<FunctionByNameKey, const FunctionNode*> _functionByName;

    // The MLIR context is a boundary for de-duplication.
    // 2 pointers associated with the same contxt can be compared
    // for equality to determine if they represent the same type.
    std::unique_ptr<mlir::MLIRContext> _mlirContext;

    // The module currently being compiled
    CompiledModule _currentCompiledModule = {};

    bool _isCompilingToVerilog;
};

// RAII class to count elasped time for various compilation steps
class CompileTimer
{
  public:
    CompileTimer(Compiler& compiler, const std::string& name);
    ~CompileTimer();

  private:
    Compiler& _compiler;
    std::string _name;
    std::chrono::steady_clock::time_point _beginTime;
};

extern Compiler* g_compiler;

template <typename T, typename... ConstructorArgsT> T* Compiler::Create(const ConstructorArgsT&... args)
{
    T* const result = new T(args...);

    // Remember that the object should be destroyed
    const DestroyFunction destroyFunction = [result]() { delete result; };

    // Types created during parsing are not destroyed
    if (_doneParsing && !std::is_base_of_v<Type, T>)
    {
        _temporaryParseTreeCleanupList.push_back(destroyFunction);
    }
    else
    {
        _permanentParseTreeCleanupList.push_back(destroyFunction);
    }

    return result;
}

// RAII class to set context pointer
class SetContextPtr
{
  public:
    SetContextPtr(Compiler& compiler, IRContext* const contextPtr);
    ~SetContextPtr();

  private:
    Compiler& _compiler;
};
