// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

// Code to compute the number of instances of each function
// flat non-inline functions have 1 instance
// non-inline methods have 1 instance per object of the containing class
// inline functions have 1 instance per call site
//
// static local variables (of class type) result in 1 object per instance of the containing function

struct CompiledModule;

// Describes a function call in the AST
struct UnresolvedCall
{
    // The called function name
    std::string _calledFunctionName;

    // Scope that was prepended to the function name
    Scope _explicitScope;

    // Object name that was explicitly specified in the call (can be null)
    const ParseTreeNode* _explicitObject;

    // The class that contains the call
    const ClassType* _callSiteClassType;

    // The namespace/module that contains the call
    Scope _callSiteScope;

    // The name of the object that contains the call
    // Can be empty during type checking
    std::string _callSiteObjectName;

    // Callback to resolve object references
    ResolveReferenceFunction _resolveReferenceCallback;
};

// Information about a specific call without reference to any objects or function instances
struct ResolvedCall
{
    // The name of the function that is called
    std::string _functionName;

    // The node in the AST corresponding to the called function
    // Can be nullptr if there is no corresponding node in the AST
    const FunctionNode* _functionNode;

    // Types that describe function signature
    FunctionDesc _functionDesc;

    // For flat functions, the namespaces that the function is declared in
    Scope _flatFunctionScope;

    // The type of the class that contains the called function
    // Set to the global class for flat functions
    const ClassType* _calledClassType;

    // True if the call is done via an object reference
    bool _callByReference;

    // True if this is a call to an exported class
    bool _callToExportClass;

    // Object name that was explicitly specified in the call
    // Can either be a node, or an object name string
    // or neither
    const ParseTreeNode* _explicitObjectNode;

    boost::optional<std::string> _explicitObjectName;
};

ResolvedCall ResolveFunctionCallForTypeChecking(const CallNode* const callNode,
                                                const ClassType* const callSiteClassType, const Scope& callSiteScope);

ResolvedCall ResolveFunctionCallPostTypeChecking(const CallNode* const callNode,
                                                 const ClassType* const callSiteClassType, const Scope& callSiteScope,
                                                 const std::string& callSiteObjectName,
                                                 const ResolveReferenceFunction resolveReferenceCallback);

ResolvedCall ResolveFunctionCall(const UnresolvedCall& unresolvedCall, const Location& callSiteLocation,
                                 const bool resolveFunctionNode);

ResolvedCall ResolveFlatFunctionCall(const std::string& functionName, const Scope& functionScope,
                                     const Scope& callSiteScope, const Location& location);

ResolvedCall ResolveFlatFunctionCall(const ScopedIdentifierNode* const calledIdentifier, const Scope& callSiteScope);

class FunctionInstanceEnumerator
{
  public:
    struct CalledInstanceDesc
    {
        // For member functions, this is "namespace::object"
        // For flat functions, this is "namespace"
        std::string _prefix;

        // Instance describing the called function
        FunctionInstance _calleeInstance;
    };

    using InstanceAndLocation = std::pair<FunctionInstance, Location>;

  private:
    // Current state of parse tree traversal
    struct Context
    {
        VisitContext _visitContext;
        Scope _namespaceScope;
        std::stack<const ClassType*> _classTypeStack;
    };

    // Maps function parameter name to name of the referenced object
    typedef std::map<std::string, std::string> ParameterReferenceMap;

    // For a function instance: Name of containing object + parameter references
    typedef std::pair<std::string, ParameterReferenceMap> ObjectNameAndParameterReferences;

    // Counts number of instances for inline functions
    typedef std::map<const FunctionNode*, Context> PerFunctionSymbolTable;
    std::map<const FunctionNode*, size_t> _inlineFunctionInstanceCountMap;

    // Contains non-inline instances (indexed by functionNode & object name)
    typedef std::map<ObjectNameAndParameterReferences, size_t> ObjectToInstanceMap;
    std::map<const FunctionNode*, ObjectToInstanceMap> _nonInlineInstanceMap;

    // Contains all instances (indexed by function node)
    std::map<const FunctionNode*, std::list<FunctionInstance>> _allInstanceMap;

    // Counts number of call sites for each function instance
    // Used for auto-inlining
    std::map<FunctionInstance, size_t> _instanceCallCountMap;

    // Tracks called functionNodes that do not support auto-inlining
    // based on their attributes
    std::set<const FunctionNode*> _disallowAutoInlineSet;

    // Maps caller function instance to a description of the called function
    typedef std::map<FunctionInstance, std::list<CalledInstanceDesc>> CalledInstanceDescriptionListMap;
    std::map<const CallNode*, CalledInstanceDescriptionListMap> _callMap;

    // Maps called function instance to calling function instances
    std::map<FunctionInstance, std::set<InstanceAndLocation>> _calleeToCaller;

    // For static local objects, maps (FunctionInstance, DeclareNode) to a unique identifier for the object
    typedef std::map<FunctionInstance, size_t> StaticLocalMap;
    std::map<const DeclareNode*, StaticLocalMap> _staticLocals;

    // Used to give each static local object a unique id
    size_t _staticLocalObjectCount;

    // Unique ID used to differentiate functions passed on parameter references
    size_t _referenceObjectNameSeqNumber;

    // The set of non-inline, unreachable function instances
    std::set<FunctionInstance> _reachableFunctionInstances;

    // The set of functions called by extern class callbacks
    std::set<FunctionInstance> _externClassCallbackCallees;

    // Used for binary operations that are implemented as a call to an inline library function
    // Maps binary operation node to the list of instances associated with the called function
    std::map<const BinaryOpNode*, std::list<FunctionInstance>> _binaryOperationCalls;

    // Used for array accesses into ecc memories
    // Maps array access node to the list of instances associated with the called function
    std::map<const ArrayAccessNode*, std::list<FunctionInstance>> _eccMemoryCalls;

    std::map<FunctionInstance, ParameterReferenceMap> _parameterReferences;

    // Maps (caller function instance, call node) -> parameter references for a particular call
    std::map<std::pair<FunctionInstance, const CallNode*>, ParameterReferenceMap> _parameterReferencesAtCallSites;

    // Maps VariableAccessNode (containing a reference) to the name of the referenced object
    typedef std::map<const VariableAccessNode*, std::string> VariableAccessTable;
    std::map<FunctionInstance, VariableAccessTable> _variableAccessTables;

    // Traverse the parse tree, capture the symbol table at each function
    PerFunctionSymbolTable TraverseParseTree(const ParseTreeNode* root);

    void EvaluateCallbacks(const ParseTreeNode* const root, PerFunctionSymbolTable& perFunctionSymbolTable,
                           const std::function<void(const FunctionInstance&)>& addToWorkList);

    FunctionInstance GetCalledInstance(const CallNode* const callNode, Context& context,
                                       const FunctionInstance& callSiteInstance,
                                       const ClassType* const callSiteClassType, const Scope& callSiteScope,
                                       const ResolveReferenceFunction& resolveReferenceCallback);

    size_t GetInlineInstanceIndex(const FunctionNode* const functionNode);

    void RegisterExportClassInstance(const std::string& objectName, const ClassNode* const classNode,
                                     const ClassCallableFunctionMap& classCallableFunctionMap,
                                     const std::function<void(const FunctionInstance&)>& addToWorkList,
                                     PerFunctionSymbolTable& perFunctionSymbolTable);

    void RegisterObject(const std::string& objectName, const ClassNode* const classNode,
                        const ClassCallableFunctionMap& classCallableFunctionMap,
                        PerFunctionSymbolTable& perFunctionSymbolTable,
                        const std::function<void(const FunctionInstance&)>& addToWorkList);

    size_t GetOrAllocateInstanceIndex(ObjectToInstanceMap& objectToInstanceMap,
                                      const ObjectNameAndParameterReferences& objectNameAndParameterReferences);

    // Maps function parameter index to compile-time known value & type
    // the type is the parameter (function declaration) type not the argument (call site) type
    using ParameterIndexToKnownValue = std::map<size_t, VisitContextPerVariableData>;

    // Maps function instance (explict inline functions only) to parameter values
    std::map<FunctionInstance, ParameterIndexToKnownValue> _inlineParameterValueMap;

    std::map<const ClassType*, std::set<const FunctionNode*>> _classTypeToMethods;
    // Used to dereference callbacks
    struct CallbackDesc
    {
        const ClassType* _classType;
        std::string _objectName;
        std::string _memberName;

        bool operator<(const CallbackDesc& rhs) const
        {
            return std::tie(_classType, _objectName, _memberName) <
                   std::tie(rhs._classType, rhs._objectName, rhs._memberName);
        }
    };

    // Maps object to list of callable methods or callbacks
    std::map<std::string, ClassCallableFunctionMap> _objectToCallableFunctions;

    const CompiledModule& _moduleToCompile;

    const bool _isCompilingToVerilog;

    // Maps (call node, call site function instance) to called object name
    using CallSite = std::pair<const CallNode*, FunctionInstance>;
    std::map<CallSite, std::list<std::string>> _callSiteToCalledObjectName;

    // The function instance currently being enumerated
    std::optional<FunctionInstance> _currentInstance;

  public:
    FunctionInstanceEnumerator(const CompiledModule& moduleToCompile, const bool isCompilingToVerilog);

    void FindUnreachableFunctions();

    void Enumerate(const ParseTreeNode* const root);

    std::vector<std::string> GetObjectNamesForFunction(const FunctionNode* const node) const;

    bool IsInlineable(const FunctionInstance& functionInstance, const size_t minimumCallCount) const;

    CalledInstanceDesc LookupCall(const CallNode* const callNode, const FunctionInstance& callerInstance);

    size_t LookupStaticLocalObjectIndex(const DeclareNode* const declareNode,
                                        const FunctionInstance& containingInstance) const;

    std::list<size_t> GetAllStaticLocalIndices(const DeclareNode* const declareNode) const;

    std::list<FunctionInstance> LookupMethodInstances(const FunctionNode* const functionNode,
                                                      const std::string& objectName) const;

    bool IsInstanceReachable(const FunctionInstance& instance) const;

    std::string GetReferencedObject(const VariableAccessNode* variableAccessNode,
                                    const FunctionInstance& instance) const;

    FunctionInstance LookupBinaryOpCall(const BinaryOpNode* const node);

    FunctionInstance LookupEccMemoryCall(const ArrayAccessNode* const node);

    size_t GetFunctionCallCount(const FunctionNode* const node, const std::string& objectName) const;

    UnresolvedCall DereferenceCallback(const ClassType* callSiteClassType, const std::string& callSiteObjectName,
                                       const std::string& callbackName, const Location& callSiteLocation) const;

    FunctionInstance DereferenceExternalClassCallback(const ClassType* classType,
                                                      const std::string& externClassObjectName,
                                                      const std::string& callbackName, const Location& location) const;

    std::string DereferenceParameter(const FunctionInstance& callerInstance, const CallNode* const callNode,
                                     const std::string& paramName);

    std::string GetCalledObjectName(const CallNode* const callNode, const FunctionInstance& callerInstance);

    void GenerateCallTrace(std::ostream& str, const FunctionInstance& srcInst);

    bool GenerateCurrentEnumerateFunctionCallTrace(std::ostream& str);

    const std::set<FunctionInstance> GetExternalClassInstanceCallees() const;

    std::optional<InstanceAndLocation> GetCaller(const FunctionInstance& inst) const;
};