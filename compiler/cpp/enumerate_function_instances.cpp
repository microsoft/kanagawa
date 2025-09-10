// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

static const ParseTreeFunctionModifier AlwaysReachableModifiers =
    ParseTreeFunctionModifierExport | ParseTreeFunctionModifierReset;

std::ostream& operator<<(std::ostream& str, const FunctionInstance& inst)
{
    str << "[" << inst._functionNode->GetName() << ", " << inst._objectName << ", " << inst._instanceIndex << "]";

    return str;
}

bool FunctionInstance::operator<(const FunctionInstance& rhs) const
{
    if (_functionNode->GetSequenceNumber() < rhs._functionNode->GetSequenceNumber())
    {
        return true;
    }
    else if (_functionNode->GetSequenceNumber() > rhs._functionNode->GetSequenceNumber())
    {
        return false;
    }
    else if (_instanceIndex < rhs._instanceIndex)
    {
        return true;
    }
    else if (_instanceIndex > rhs._instanceIndex)
    {
        return false;
    }
    else
    {
        return _objectName < rhs._objectName;
    }
}

bool FunctionInstance::operator==(const FunctionInstance& rhs) const { return !(*this < rhs) && !(rhs < *this); }

std::string FunctionInstance::GetObjectAndFunctionName() const
{
    std::string result;

    if (_objectName != g_globalObjectName)
    {
        result = _objectName;
        result += "_";
    }

    result += _functionNode->GetName();

    return result;
}

std::string FunctionInstance::GetUnmangledObjectAndFunctionName() const
{
    std::string result;

    if (_objectName != g_globalObjectName)
    {
        result = _objectName;
        result += "::";
    }

    result += _functionNode->GetUnmangledName();

    return result;
}

FunctionInstanceEnumerator::FunctionInstanceEnumerator(const CompiledModule& moduleToCompile,
                                                       const bool isCompilingToVerilog)
    : _staticLocalObjectCount(0), _referenceObjectNameSeqNumber(0), _moduleToCompile(moduleToCompile),
      _isCompilingToVerilog(isCompilingToVerilog)
{
}

// See if an instance for a non-inline function and object name has already been allocated
// If not, then allocate one
size_t FunctionInstanceEnumerator::GetOrAllocateInstanceIndex(
    ObjectToInstanceMap& objectToInstanceMap, const ObjectNameAndParameterReferences& objectNameAndParameterReferences)
{
    size_t result = 0;

    const auto it = objectToInstanceMap.find(objectNameAndParameterReferences);

    if (it != objectToInstanceMap.end())
    {
        result = it->second;
    }
    else
    {
        result = objectToInstanceMap.size();

        objectToInstanceMap[objectNameAndParameterReferences] = result;
    }

    return result;
}

// Called for each object in the design
void FunctionInstanceEnumerator::RegisterObject(const std::string& objectName, const ClassNode* const classNode,
                                                const ClassCallableFunctionMap& classCallableFunctionMap,
                                                PerFunctionSymbolTable& perFunctionSymbolTable,
                                                const std::function<void(const FunctionInstance&)>& addToWorkList)
{
    SafeInsert(_objectToCallableFunctions, objectName, classCallableFunctionMap);

    const ClassType* const classType = safe_cast<const ClassType*>(classNode->GetType());

    const std::set<const FunctionNode*>& methods = _classTypeToMethods[classType];

    const auto addToWorkListWrapper =
        [&](const FunctionNode* const method, const std::string& objectName, const bool validateExternClassCallees)
    {
        ObjectToInstanceMap& oim = _nonInlineInstanceMap[method];

        ObjectNameAndParameterReferences onpr;
        onpr.first = objectName;

        const size_t instanceIndex = GetOrAllocateInstanceIndex(oim, onpr);

        const FunctionInstance fi(method, instanceIndex, objectName);

        if (validateExternClassCallees)
        {
            // Arbitration is not yet supported, validate that a single function is not connected to multiple callbacks
            if (_externClassCallbackCallees.end() != _externClassCallbackCallees.find(fi))
            {
                g_compiler->ErrorStream(fi._functionNode->GetLocation(), CompileError::InvalidCallback)
                    << "A single function cannot be connected to multiple external class callbacks: "
                    << CombineObjectAndMemberName(fi._functionNode->GetContainingScope(), fi._objectName,
                                                  fi._functionNode->GetName());

                throw std::runtime_error("Invalid callback connection");
            }
        }

        addToWorkList(fi);

        return fi;
    };

    // Methods with ParseTreeFunctionModifierReset or ParseTreeFunctionModifierExport must be processed
    // through the work list, even if they do have no callers.
    for (const FunctionNode* const method : methods)
    {
        if (0 != (method->GetModifiers() & AlwaysReachableModifiers))
        {
            addToWorkListWrapper(method, objectName, false);
        }
    }

    // Methods which are callable from an external class must be must be processed
    // through the work list, even if they do have no callers.
    if (classNode->IsExternal())
    {
        for (const auto& p : classCallableFunctionMap)
        {
            const FunctionInstance fi = addToWorkListWrapper(p.second._functionNode, p.second._objectName, true);

            // Validate modifiers on the callee
            const ParseTreeFunctionModifier allowedCalleeAttributes =
                ParseTreeFunctionModifierAsync | ParseTreeFunctionModifierNoInline |
                ParseTreeFunctionModifierNoBackPressure | ParseTreeFunctionModifierExternalFixedLatency;

            if (0 != (fi._functionNode->GetModifiers() & ~allowedCalleeAttributes))
            {
                g_compiler->ErrorStream(p.second._functionNode->GetLocation(), CompileError::InvalidAttribute)
                    << "Functions connected to external class callbacks can only have noinline, [[async]], "
                       "[[no_backpressure]], and [[latency()]].  Object: "
                    << objectName;
            }

            _externClassCallbackCallees.insert(fi);
        }
    }

    if (classType->IsExport() && (classNode != _moduleToCompile._classNodeToCompile))
    {
        RegisterExportClassInstance(objectName, classNode, classCallableFunctionMap, addToWorkList,
                                    perFunctionSymbolTable);
    }
}

// Traverse the parse tree, capture the symbol table at each function
FunctionInstanceEnumerator::PerFunctionSymbolTable
FunctionInstanceEnumerator::TraverseParseTree(const ParseTreeNode* root)
{
    PerFunctionSymbolTable result;

    Context context = {};

    // Ensure that loops are unrolled when traversing the graph
    context._visitContext._global._trackKnownValues = true;

    context._classTypeStack.push(g_compiler->GetGlobalClassType());

    const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
    {
        const FunctionNode* const functionNode = dynamic_cast<const FunctionNode*>(node);
        const NamespaceNode* const namespaceNode = dynamic_cast<const NamespaceNode*>(node);
        const ClassNode* const classNode = dynamic_cast<const ClassNode*>(node);

        if (functionNode)
        {
            SafeInsert(result, functionNode, context);
            // no need to traverse into functions, nested functions are not allowed

            _classTypeToMethods[context._classTypeStack.top()].insert(functionNode);
        }
        else if (namespaceNode)
        {
            context._namespaceScope.push_back(namespaceNode->GetName());

            recurseCallback();

            context._namespaceScope.pop_back();
        }
        else if (classNode)
        {
            const ClassType* const classType = dynamic_cast<const ClassType*>(classNode->GetType());

            context._classTypeStack.push(classType);

            recurseCallback();

            context._classTypeStack.pop();
        }
        else
        {
            recurseCallback();
        }
    };

    // push global scope
    VisitContext::PushPopScope globalScope(context._visitContext);

    root->Visit(callback, context._visitContext);

    context._classTypeStack.pop(); // global class type

    assert(context._namespaceScope.empty());
    assert(context._classTypeStack.empty());

    // Add empty contexts for callbacks of the exported class being compiled
    if (!_moduleToCompile._isDefaultPass)
    {
        const ClassCallableFunctionMap ccfm = _moduleToCompile._classNodeToCompile->GetCallbackExternFunctions();

        for (const auto& p : ccfm)
        {
            Context context = {};

            VisitContext::PushPopScope globalScope(context._visitContext);

            context._classTypeStack.push(g_compiler->GetGlobalClassType());

            SafeInsert(result, p.second._functionNode, context);
        }
    }

    return result;
}

// Determines if a FunctionNode exists in the AST
// inline intrinsics do not have a corresponding FunctionNode
const FunctionNode* ResolveFunctionNode(const ClassType* const calledClassType, const Scope& calledFunctionScope,
                                        const std::string& functionNameForNodeLookup, const Location& location)
{
    const FunctionNode* result = nullptr;

    const FunctionDesc functionDesc =
        g_compiler->GetFunctionDesc(calledClassType, calledFunctionScope, functionNameForNodeLookup, location);

    const ParseTreeFunctionModifier inlineIntrinsicMask =
        static_cast<ParseTreeFunctionModifier>(ParseTreeFunctionModifierInline | ParseTreeFunctionModifierIntrinsic);

    if ((inlineIntrinsicMask == (functionDesc._modifiers & inlineIntrinsicMask)) ||
        (ParseTreeFunctionModifierExternalFixedLatency ==
         (functionDesc._modifiers & ParseTreeFunctionModifierExternalFixedLatency)))
    {
        result = nullptr;
    }
    else
    {
        result = GetFunctionByName(calledClassType, calledFunctionScope, functionNameForNodeLookup, location);
    }

    return result;
}

ResolvedCall ResolveFunctionCallForTypeChecking(const CallNode* const callNode,
                                                const ClassType* const callSiteClassType, const Scope& callSiteScope)
{
    const UnresolvedCall unresolvedCall = {
        callNode->GetName(),
        callNode->GetScopedName()->GetScope(),
        callNode->GetObject(),
        callSiteClassType,
        callSiteScope,
        "" // call site object name is not needed during type checking, because callbacks are not dereferenced
    };

    return ResolveFunctionCall(unresolvedCall, callNode->GetLocation(), false);
}

ResolvedCall ResolveFunctionCallPostTypeChecking(const CallNode* const callNode,
                                                 const ClassType* const callSiteClassType, const Scope& callSiteScope,
                                                 const std::string& callSiteObjectName,
                                                 const ResolveReferenceFunction resolveReferenceCallback)
{
    const UnresolvedCall unresolvedCall = {callNode->GetName(),     callNode->GetScopedName()->GetScope(),
                                           callNode->GetObject(),   callSiteClassType,
                                           callSiteScope,           callSiteObjectName,
                                           resolveReferenceCallback};

    return ResolveFunctionCall(unresolvedCall, callNode->GetLocation(), true);
}

ResolvedCall ResolveFunctionCall(const UnresolvedCall& unresolvedCall, const Location& callSiteLocation,
                                 const bool resolveFunctionNode)
{
    const ClassType* const callSiteClassType = unresolvedCall._callSiteClassType;
    const Scope& callSiteScope = unresolvedCall._callSiteScope;
    const std::string& callSiteObjectName = unresolvedCall._callSiteObjectName;

    assert(Implies(resolveFunctionNode, !callSiteObjectName.empty()));

    ResolvedCall result = {};

    result._functionName = unresolvedCall._calledFunctionName;
    result._callByReference = false;
    result._callToExportClass = false;

    // This represents the called object that is explicitly referenced in the source
    // null if there is no explict object
    const ParseTreeNode* const explictObject = unresolvedCall._explicitObject;

    const ClassType* calledClassType = g_compiler->GetGlobalClassType();

    // The scope containing the function (flat functions only)
    Scope calledFunctionScope = Scope();

    // Used when looking for the AST node corresponding to the called function
    // For calls into external modules, the module name is prepended
    std::string functionNameForNodeLookup = unresolvedCall._calledFunctionName;

    if (explictObject)
    {
        // This case is dog.bark();
        // explictObject represents dog
        const Type* const calledObjectType = explictObject->GetType();

        // explictObject can either be an instance of a class or it can be an external module
        calledClassType = dynamic_cast<const ClassType*>(calledObjectType);

        const ReferenceType* const calledReferenceType = dynamic_cast<const ReferenceType*>(calledObjectType);

        const FunctionType* const calledFunctionType = dynamic_cast<const FunctionType*>(calledObjectType);

        if (calledReferenceType)
        {
            assert(!calledClassType);
            calledClassType = dynamic_cast<const ClassType*>(calledReferenceType->_referencedType);
            assert(calledClassType);

            result._callByReference = true;

            // Check to see if the called function is a callback
            if (calledClassType->GetCallbackType(unresolvedCall._explicitScope, unresolvedCall._calledFunctionName) !=
                nullptr)
            {
                if (resolveFunctionNode)
                {
                    assert(unresolvedCall._resolveReferenceCallback);

                    // Convert the reference variable name to a physical object name
                    const std::string calledObjectName = unresolvedCall._resolveReferenceCallback(
                        safe_cast<const VariableAccessNode*>(explictObject)->GetName());

                    const UnresolvedCall dereferencedUnresolvedCall =
                        g_compiler->GetFunctionInstanceEnumerator().DereferenceCallback(
                            calledClassType, calledObjectName, unresolvedCall._calledFunctionName, callSiteLocation);

                    // Dereference the callback to get the actual called function
                    ResolvedCall result = ResolveFunctionCall(dereferencedUnresolvedCall, callSiteLocation, true);

                    // Return the called object name
                    // because the caller has no other way of determining it
                    assert(!result._explicitObjectNode);

                    result._explicitObjectName = dereferencedUnresolvedCall._callSiteObjectName;

                    return result;
                }
            }
        }

        // IsExportIgnoreTarget here matches the logic in FunctionNode::IsAutoInline
        if (calledClassType && (calledClassType->IsExportIgnoreTarget()))
        {
            result._callToExportClass = true;
        }

        if (calledFunctionType)
        {
            // This code path is for initializing a callback
            // to a method of an object:
            // callback = member.fn;
            //
            // Determine the type of the member
            const std::string memberName = safe_cast<const VariableAccessNode*>(explictObject)->GetName();

            for (const ClassType::EntryType& member : callSiteClassType->GetMemberVariables())
            {
                if (member.first == memberName)
                {
                    calledClassType = safe_cast<const ClassType*>(member.second->GetDeclaredType());
                    break;
                }
            }
        }

        if (!calledClassType)
        {
            // calledClassType can be for code that will fail type checking (this function is called during type
            // checking)
            g_compiler->ErrorStream(callSiteLocation, CompileError::InvalidCall)
                << "Left of . in a method call must be an object/extern module";
            throw std::runtime_error("Invalid function call");
        }
    }
    else
    {
        if (callSiteClassType->ContainsMethod(unresolvedCall._calledFunctionName))
        {
            // Even if no object was specified, the called function could be a member of the same class as the class
            // containing the call site
            calledClassType = callSiteClassType;
        }
        else if (callSiteClassType->GetCallbackType(unresolvedCall._explicitScope,
                                                    unresolvedCall._calledFunctionName) != nullptr)
        {
            // Or the called function could be a callback member of the class containing the call site
            calledClassType = callSiteClassType;

            // Members can not be referenced via an explicit module name
            assert(unresolvedCall._explicitScope.empty());

            if (resolveFunctionNode)
            {
                // For compilation phases after type checking
                // Dereference the callback to get the actual called function
                return ResolveFunctionCall(
                    g_compiler->GetFunctionInstanceEnumerator().DereferenceCallback(
                        callSiteClassType, callSiteObjectName, unresolvedCall._calledFunctionName, callSiteLocation),
                    callSiteLocation, true);
            }
        }
        else
        {
            calledFunctionScope = g_compiler->GetFlatFunctionScope(
                callSiteScope, unresolvedCall._explicitScope, unresolvedCall._calledFunctionName, callSiteLocation);
        }
    }

    const FunctionDesc functionDesc =
        g_compiler->GetFunctionDesc(calledClassType, calledFunctionScope, functionNameForNodeLookup, callSiteLocation);

    result._functionNode = resolveFunctionNode ? ResolveFunctionNode(calledClassType, calledFunctionScope,
                                                                     functionNameForNodeLookup, callSiteLocation)
                                               : nullptr;

    result._functionDesc = functionDesc;

    result._flatFunctionScope = calledFunctionScope;

    result._calledClassType = calledClassType;

    result._explicitObjectNode = explictObject;

    return result;
}

// Resolve a call to a flat function
ResolvedCall ResolveFlatFunctionCall(const std::string& functionName, const Scope& functionScope,
                                     const Scope& callSiteScope, const Location& location)
{
    ResolvedCall result = {};

    result._functionName = functionName;
    result._callByReference = false;
    result._callToExportClass = false;

    const ClassType* calledClassType = g_compiler->GetGlobalClassType();

    // Used when looking for the AST node corresponding to the called function
    // For calls into external modules, the module name is prepended
    const std::string functionNameForNodeLookup = functionName;

    const Scope calledFunctionScope =
        g_compiler->GetFlatFunctionScope(callSiteScope, functionScope, functionName, location);

    const FunctionDesc functionDesc =
        g_compiler->GetFunctionDesc(calledClassType, calledFunctionScope, functionNameForNodeLookup, location);

    result._functionNode =
        ResolveFunctionNode(calledClassType, calledFunctionScope, functionNameForNodeLookup, location);

    result._functionDesc = functionDesc;

    result._flatFunctionScope = calledFunctionScope;

    result._calledClassType = calledClassType;

    return result;
}

ResolvedCall ResolveFlatFunctionCall(const ScopedIdentifierNode* const calledIdentifier, const Scope& callSiteScope)
{
    return ResolveFlatFunctionCall(calledIdentifier->GetName(), calledIdentifier->GetScope(), callSiteScope,
                                   calledIdentifier->GetLocation());
}

size_t FunctionInstanceEnumerator::GetInlineInstanceIndex(const FunctionNode* const functionNode)
{
    assert(ParseTreeFunctionModifierInline == (functionNode->GetModifiers() & ParseTreeFunctionModifierInline));

    // Each call site to an inline function generates a new function instance
    size_t& instanceIndex = _inlineFunctionInstanceCountMap[functionNode];

    return instanceIndex++;
}

FunctionInstance FunctionInstanceEnumerator::GetCalledInstance(const CallNode* const callNode, Context& context,
                                                               const FunctionInstance& callSiteInstance,
                                                               const ClassType* const callSiteClassType,
                                                               const Scope& callSiteScope,
                                                               const ResolveReferenceFunction& resolveReferenceCallback)
{
    // The call site might be a flat function with reference parameters
    // In which case, use the global object name (not the synthesized name with reference values)
    const std::string callSiteObjectName =
        callSiteClassType == g_compiler->GetGlobalClassType() ? g_globalObjectName : callSiteInstance._objectName;

    // Get non-instance-specific information about the call
    const ResolvedCall resolvedCall = ResolveFunctionCallPostTypeChecking(callNode, callSiteClassType, callSiteScope,
                                                                          callSiteObjectName, resolveReferenceCallback);

    FunctionInstance result = {};
    CalledInstanceDesc callDesc = {};

    result._objectName = g_globalObjectName;
    result._functionNode = resolvedCall._functionNode;

    if (resolvedCall._explicitObjectNode)
    {
        const ParseTreeNode* const explictObject = resolvedCall._explicitObjectNode;

        // This case is dog.bark();
        // explictObject represents dog
        const Type* const calledObjectType = explictObject->GetType();

        assert(resolvedCall._calledClassType);

        // The called object name is in the source
        // Get the name of the object in the source
        VisitCheckKnownValueContext knownValueContext(context._visitContext);

        const LValNode* const explicitObjectLVal = dynamic_cast<const LValNode*>(explictObject);

        std::string calledObjectName =
            explicitObjectLVal->GetObjectName(knownValueContext, callSiteObjectName, resolveReferenceCallback);

        // Get the scope written in the source
        const Scope calledObjectExplicitScope = dynamic_cast<const LValNode*>(explictObject)->GetObjectScope();

        const std::vector<ClassType::EntryType>& memberVariables = callSiteClassType->GetMemberVariables();

        if (calledObjectExplicitScope.empty() && callSiteClassType->ContainsMemberRecursive(calledObjectName))
        {
            // The called object is a member variable
            // callSiteObjectName contains the namespace
            result._objectName = CombineObjectAndMemberName(Scope(), callSiteObjectName, calledObjectName);
        }
        else
        {
            // Called object is not a member variable

            // check to see if the called object is a static local
            const std::string symbolLookupName = dynamic_cast<const LValNode*>(explictObject)->GetRootVariableName();

            if (context._visitContext.ContainsSymbol(Scope(), symbolLookupName, callNode->GetLocation()))
            {
                const VisitContextPerVariableData variableData =
                    context._visitContext.LookupSymbol(Scope(), symbolLookupName, callNode->GetLocation());

                if (variableData._declaration->IsStatic())
                {
                    // Use the object name for this instance
                    const StaticLocalMap& slm = SafeLookup(_staticLocals, variableData._declaration);

                    const size_t uniqueId = SafeLookup(slm, callSiteInstance);

                    calledObjectName = GetStaticLocalInstanceName(calledObjectName, uniqueId);
                }
            }

            result._objectName = FlattenScopeAndAppendName(calledObjectExplicitScope, calledObjectName);
        }

        callDesc._prefix = result._objectName;
    }
    else if (resolvedCall._explicitObjectName)
    {
        // Used when resolve callbacks called via reference
        result._objectName = *resolvedCall._explicitObjectName;
    }
    else
    {
        // No explicit object was specified, calling a method of an object from with a method of that object
        // or a callback
        const ClassCallableFunctionMap& functionMap = _objectToCallableFunctions[callSiteObjectName];

        const auto it = functionMap.find(callNode->GetName());

        if (it != functionMap.end())
        {
            const ClassCallableFunction& callableFunction = it->second;

            result._objectName = callableFunction._objectName;

            callDesc._prefix = result._objectName;
        }
        else
        {
            // Flat function, not part of any class
            callDesc._prefix = FlattenScope(resolvedCall._flatFunctionScope);
        }
    }

    if (result._functionNode)
    {
        // Determine if any of the arguments are references to objects
        // If they are, then save that information
        ParameterReferenceMap referenceMap = callNode->TranslateParameterReferences(
            resolveReferenceCallback, result._functionNode, callSiteInstance._objectName);

        // Determine the instance index
        if (resolvedCall._functionDesc._modifiers & ParseTreeFunctionModifierInline)
        {
            // Each call site to an inline function generates a new function instance
            result._instanceIndex = GetInlineInstanceIndex(result._functionNode);
        }
        else
        {
            // Non-inline function/method
            // 1 instance per {containing object, parameter references}

            // Functions that have parameter references use synthetic object names
            // based on the parameter values. This ensures separate function instances for each set of parameter values
            if (!referenceMap.empty() && (result._objectName == g_globalObjectName))
            {
                std::ostringstream str;
                str << "__references_";
                for (const auto& p : referenceMap)
                {
                    str << "_" << p.first << "_" << p.second;
                }
                result._objectName = str.str();

                // Store information for a name->path lookup for this new object name
                g_compiler->RegisterObjectPath(result._objectName, ObjectPath());
            }

            ObjectToInstanceMap& objectToInstanceMap = _nonInlineInstanceMap[result._functionNode];

            ObjectNameAndParameterReferences objectNameAndParameterReferences;

            objectNameAndParameterReferences.first = result._objectName;
            objectNameAndParameterReferences.second = referenceMap;

            result._instanceIndex = GetOrAllocateInstanceIndex(objectToInstanceMap, objectNameAndParameterReferences);
        }

        if (!referenceMap.empty())
        {
            // If an inline function (with multiple call sites)
            // calls a function with reference parameters
            // then this code path in the compiler can run multiple times
            // validate that all references are equal on each insertion
            SafeInsertIdempotent(_parameterReferences, result, referenceMap);
        }

        // Record a description of the call, to be used during IR generation
        callDesc._calleeInstance = result;

        CalledInstanceDescriptionListMap& callDescriptionListMap = _callMap[callNode];
        callDescriptionListMap[callSiteInstance].push_back(callDesc);
    }

    // Save the called object name
    // This even applies when there is no function node (like a fixed-latency method of an extern module)
    const CallSite callSite(callNode, callSiteInstance);

    _callSiteToCalledObjectName[callSite].push_back(result._objectName);

    return result;
}

// Determines which function each callback refers to
void FunctionInstanceEnumerator::EvaluateCallbacks(const ParseTreeNode* const root,
                                                   PerFunctionSymbolTable& perFunctionSymbolTable,
                                                   const std::function<void(const FunctionInstance&)>& addToWorkList)
{
    VisitContext visitContext = {};

    // Ensure that loops are unrolled when traversing the graph
    visitContext._global._trackKnownValues = true;

    using ObjectStackEntry = std::pair<std::string, const ClassType*>;

    Scope namespaceScope;

    bool expectExportClassInstances = false;

    const auto objectCallback = [&](const std::string& objectName, const ClassNode* const classNode,
                                    const ClassCallableFunctionMap& classCallableFunctionMap)
    { RegisterObject(objectName, classNode, classCallableFunctionMap, perFunctionSymbolTable, addToWorkList); };

    if (_moduleToCompile._isDefaultPass)
    {
        // During the default pass, it is expected to find export class instances which are global variables
        // or members of global variables
        expectExportClassInstances = true;
    }
    else
    {
        // Evaluate callbacks with default initial values
        ObjectToClassCallableFunctionMap objectToFunctions;

        // Export class instances are expected to be found as children of the top-level class being compiled
        expectExportClassInstances = true;

        _moduleToCompile._classNodeToCompile->GetType()->RegisterObjects(
            RegisterObjectsMode::EnumerateOnly, _moduleToCompile._placeholderObjectName,
            g_compiler->ObjectNameToPath(_moduleToCompile._placeholderObjectName), objectCallback,
            DefaultDefaultInitializeCallback, objectToFunctions);

        // Export class instances should not be found as instances of globals in the traversal below
        expectExportClassInstances = false;
    }

    const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
    {
        const ClassNode* const classNode = dynamic_cast<const ClassNode*>(node);
        const NamespaceNode* const namespaceNode = dynamic_cast<const NamespaceNode*>(node);
        const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);

        if (classNode)
        {
            // Don't traverse into the class
        }
        else if (namespaceNode)
        {
            namespaceScope.push_back(namespaceNode->GetName());

            recurseCallback();

            namespaceScope.pop_back();
        }
        else if (declareNode)
        {
            const ReferenceType* const referenceType =
                dynamic_cast<const ReferenceType*>(declareNode->GetDeclaredType());

            // Static variables are handled while enumerating the containing function instance
            // The _moduleToCompile._isDefaultPass term ensures that globals are only registered
            // during the default pass.
            // They should not be used in other passes.
            if (declareNode->ContainsClassType() && !referenceType && !declareNode->IsStatic() &&
                _moduleToCompile._isDefaultPass)
            {
                const AssignNode* const assignNode = declareNode->GetAssignNode();

                const BaseInitializerListNode* const initializerList =
                    assignNode ? dynamic_cast<const BaseInitializerListNode*>(assignNode->GetRhs()) : nullptr;

                // Call objectCallback for each object that has already been registered
                declareNode->ResolveFunctionsAndRegisterObjects(RegisterObjectsMode::EnumerateOnly,
                                                                declareNode->GetFlattenedName(), ObjectPath(),
                                                                g_globalObjectName, objectCallback);
            }
        }
        else
        {
            recurseCallback();
        }
    };

    // push global scope
    VisitContext::PushPopScope globalScope(visitContext);

    root->Visit(callback, visitContext);

    assert(namespaceScope.empty());
}

void FunctionInstanceEnumerator::Enumerate(const ParseTreeNode* const root)
{
    // Traverse the parse tree, record the symbol table at each function
    PerFunctionSymbolTable perFunctionSymbolTable = TraverseParseTree(root);

    // Set of function instances to be processed
    std::set<FunctionInstance> workList;

    // Determine the function that each callback refers to
    EvaluateCallbacks(root, perFunctionSymbolTable, [&](const FunctionInstance& fi) { workList.insert(fi); });

    // Set of function instances that have already been processed
    std::set<FunctionInstance> visitedInstances;

    // A specialized version of FunctionInstance used only for recursion detection
    // sets _instanceIndex for inline functions
    // to enable catching recursion among inline functions (where each call generates a new instance)
    class RecursionDetectionFunctionInstance
    {
      private:
        FunctionInstance _instance;

      public:
        RecursionDetectionFunctionInstance() {}

        RecursionDetectionFunctionInstance(const FunctionInstance& inst) { *this = inst; }

        RecursionDetectionFunctionInstance& operator=(const FunctionInstance& inst)
        {
            _instance = inst;

            if (inst._functionNode->IsInline())
            {
                _instance._instanceIndex = 0;
            }

            return *this;
        }

        bool operator<(const RecursionDetectionFunctionInstance& rhs) const { return _instance < rhs._instance; }
    };

    // Maps a function instance to the set of function instances
    // topologically before it in the call graph
    // This is used to detect recursion
    std::map<RecursionDetectionFunctionInstance, std::set<RecursionDetectionFunctionInstance>> callGraphParentInstances;

    // Traverse the call graph, starting with exported functions and continuing on to functions they call
    for (const auto& p : perFunctionSymbolTable)
    {
        const FunctionNode* const functionNode = p.first;

        const ClassType* const containingClassType = p.second._classTypeStack.top();

        // If we are not compiling the default module then ignore a global Reset function
        const bool allowGlobalResetFunctions =
            _moduleToCompile._isDefaultPass || (0 == (functionNode->GetModifiers() & ParseTreeFunctionModifierReset));

        // If any of these modifiers are present, then the function is considered reachable
        // even if it does not have call sites
        if ((containingClassType == g_compiler->GetGlobalClassType()) &&
            (0 != (functionNode->GetModifiers() & AlwaysReachableModifiers)) && allowGlobalResetFunctions)
        {
            // Add each instance of this function to the list of functions to be traversed
            // Note that this only applies to flat functions.
            // Class methods with modifiers like reset are handled separately
            const std::vector<std::string>& objectNames = containingClassType->GetObjectNames();

            for (size_t i = 0; i < objectNames.size(); i++)
            {
                const std::string& objectName = objectNames[i];

                // Record the method for later lookup in _nonInlineInstanceMap
                ObjectToInstanceMap& oim = _nonInlineInstanceMap[functionNode];

                // Exported functions do not have reference parameters
                assert(!functionNode->ContainsReferenceParameters());
                ObjectNameAndParameterReferences onpr;
                onpr.first = objectName;

                SafeInsert(oim, onpr, i);

                const FunctionInstance fi(functionNode, i, objectName);

                workList.insert(fi);
                SafeInsert(callGraphParentInstances, RecursionDetectionFunctionInstance(fi),
                           std::set<RecursionDetectionFunctionInstance>());
            }
        }

        // extern/export functions are not candidates for auto-inlining
        // this code handles unreferenced/unreachable functions
        const ParseTreeFunctionModifier noInlineMask =
            ParseTreeFunctionModifierAsync | ParseTreeFunctionModifierExport | ParseTreeFunctionModifierExternal |
            ParseTreeFunctionModifierExternalFixedLatency | ParseTreeFunctionModifierPipelined |
            ParseTreeFunctionModifierReset;

        if (0 != (functionNode->GetModifiers() & noInlineMask))
        {
            _disallowAutoInlineSet.insert(functionNode);
        }
    }

    assert(!_currentInstance.has_value());

    while (!workList.empty())
    {
        bool addedNewObjects = false;

        const FunctionInstance functionInstance = *(workList.begin());
        workList.erase(workList.begin());

        // Store the instance being processed (for error reported)
        _currentInstance = std::optional<FunctionInstance>(functionInstance);
        ExitScope clearCurrentInstance([this]() { _currentInstance.reset(); });

        std::set<RecursionDetectionFunctionInstance> parentInstances = callGraphParentInstances[functionInstance];
        parentInstances.insert(functionInstance);

        _allInstanceMap[functionInstance._functionNode].push_back(functionInstance);

        // Recall the symbol table that was saved during the parse tree traversal
        Context context = SafeLookup(perFunctionSymbolTable, functionInstance._functionNode);

        // Maps variable names to objects that they reference
        // Note that the keys to this map may be composite like: "foo.x[3].this"
        std::map<std::string, std::string> referenceMap;

        // Determine if any parameters contain references to objects
        const auto prmIt = _parameterReferences.find(functionInstance);
        if (prmIt != _parameterReferences.end())
        {
            referenceMap = prmIt->second;
        }

        // Callback used to map a composite variable name (like x.f[4].this) to a referenced object name
        const auto resolveReferenceCallback = [&](const std::string& variableName) -> std::string
        { return SafeLookup(referenceMap, variableName); };

        // Maps parameter declaration to parameter known value
        std::map<const DeclareNode*, VisitContextPerVariableData> knownParameterDeclarations;

        const auto inlineParameterValueIt = _inlineParameterValueMap.find(functionInstance);

        if (inlineParameterValueIt != _inlineParameterValueMap.end())
        {
            const ParameterIndexToKnownValue& parameterIndexToKnownValue = inlineParameterValueIt->second;

            for (const auto& p : parameterIndexToKnownValue)
            {
                const size_t parameterIndex = p.first;
                const VisitContextPerVariableData& perVariableData = p.second;

                SafeInsert(knownParameterDeclarations, perVariableData._declaration, perVariableData);
            }
        }

        const auto callback = [&](const ParseTreeNode* const node, const VisitChildrenFunction& recurseCallback)
        {
            const CallNode* const callNode = dynamic_cast<const CallNode*>(node);
            const DeclareNode* const declareNode = dynamic_cast<const DeclareNode*>(node);
            const BinaryOpNode* const binaryOpNode = dynamic_cast<const BinaryOpNode*>(node);
            const AssignNode* const assignNode = dynamic_cast<const AssignNode*>(node);
            const VariableAccessNode* const variableAccessNode = dynamic_cast<const VariableAccessNode*>(node);
            const ArrayAccessNode* const arrayAccessNode = dynamic_cast<const ArrayAccessNode*>(node);

            const auto addToWorkList = [&](const FunctionInstance& fi)
            {
                workList.insert(fi);

                callGraphParentInstances[fi] = parentInstances;
            };

            if (callNode)
            {
                const FunctionInstance calledInstance =
                    GetCalledInstance(callNode, context, functionInstance, context._classTypeStack.top(),
                                      context._namespaceScope, resolveReferenceCallback);

                // Don't save call stack information for intrinsics
                // Logging code assumes non-null functions pointers
                // Errors cannot occur inside of intrinsic functions
                if (calledInstance._functionNode && functionInstance._functionNode)
                {
                    _calleeToCaller[calledInstance].insert(
                        InstanceAndLocation(functionInstance, callNode->GetLocation()));
                }

                // Record parameter references at this call site
                _parameterReferencesAtCallSites[std::pair<FunctionInstance, const CallNode*>(functionInstance,
                                                                                             callNode)] = referenceMap;

                // No processing needed for inline intrinsics
                if (calledInstance._functionNode)
                {
                    if (parentInstances.end() != parentInstances.find(calledInstance))
                    {
                        std::cout << "Error " << static_cast<uint32_t>(CompileError::RecursionNotSupported)
                                  << " recursion detected in function: "
                                  << calledInstance._functionNode->GetFlattenedName() << "\n";
                        throw std::runtime_error("Recursion is not allowed");
                    }

                    // Save parameter values (for explict inline calls only)
                    if (calledInstance._functionNode->GetModifiers() & ParseTreeFunctionModifierInline)
                    {
                        ParameterIndexToKnownValue parameterIndexToKnownValue;

                        const std::vector<const ParseTreeNode*>& arguments = callNode->GetArguments();

                        assert(arguments.size() == calledInstance._functionNode->GetParameterCount());

                        VisitCheckKnownValueContext knownValueContext(context._visitContext);

                        for (size_t i = 0; i < arguments.size(); i++)
                        {
                            const DeclareNode* const paramDeclareNode =
                                calledInstance._functionNode->GetParameterDeclareNode(i);

                            const KnownValue kv =
                                arguments[i]->TryGetKnownValue(knownValueContext, paramDeclareNode->GetDeclaredType());

                            if (kv._type != KnownValueType::None)
                            {
                                const VisitContextPerVariableData perVariableData = {kv, paramDeclareNode};

                                SafeInsert(parameterIndexToKnownValue, i, perVariableData);
                            }
                        }

                        SafeInsert(_inlineParameterValueMap, calledInstance, parameterIndexToKnownValue);
                    }

                    // Track the number of calls to each function
                    // Even if the function is not inline, this count increases
                    // This is for auto-inlining
                    _instanceCallCountMap[calledInstance]++;

                    if (!calledInstance._functionNode->IsAutoInline(functionInstance._functionNode, callNode))
                    {
                        // This caller->callee pair does not support auto-inlining
                        // remember this
                        _disallowAutoInlineSet.insert(calledInstance._functionNode);
                    }

                    // Add the instance to the work list if the instance has not been processed before
                    const auto insertResult = visitedInstances.insert(calledInstance);

                    if (insertResult.second)
                    {
                        addToWorkList(calledInstance);
                    }
                }
            }
            else if (binaryOpNode)
            {
                VisitCheckKnownValueContext knownValueContext(context._visitContext);

                // Some binary operations are implemented as calls to inline functions
                const std::pair<const FunctionNode*, size_t> calledFunction =
                    binaryOpNode->GetCalledInlineFunction(knownValueContext);

                if (calledFunction.first)
                {
                    assert(ParseTreeFunctionModifierInline ==
                           (calledFunction.first->GetModifiers() & ParseTreeFunctionModifierInline));

                    // Integer multiplications can  map to many calls to DSP functions
                    assert(calledFunction.second > 0);

                    for (size_t i = 0; i < calledFunction.second; i++)
                    {
                        const FunctionInstance calledInstance(
                            calledFunction.first, GetInlineInstanceIndex(calledFunction.first), g_globalObjectName);

                        addToWorkList(calledInstance);

                        _binaryOperationCalls[binaryOpNode].push_back(calledInstance);
                    }
                }
            }
            else if (declareNode)
            {
                if (declareNode->IsStatic() && declareNode->ContainsClassType())
                {
                    // Static local class variable
                    // There will be a unique object for each function instance
                    const size_t uniqueIndex = _staticLocalObjectCount++;

                    StaticLocalMap& slm = _staticLocals[declareNode];

                    SafeInsert(slm, functionInstance, uniqueIndex);

                    // When registering the object,
                    // Resolve callbacks contained within it
                    // and locate methods which are reset functions (add them to the work list)
                    const auto registerObjectCallback = [&](const std::string& objectName,
                                                            const ClassNode* const classNode,
                                                            const ClassCallableFunctionMap& classCallableFunctionMap)
                    {
                        RegisterObject(objectName, classNode, classCallableFunctionMap, perFunctionSymbolTable,
                                       [&](const FunctionInstance& fi) { workList.insert(fi); });
                    };

                    // Resolve identifiers in the initializer list to
                    // concrete functions
                    const std::string objectName =
                        GetStaticLocalInstanceName(declareNode->GetDeclaredName(), uniqueIndex);

                    const ObjectPath objectPath =
                        AppendToPath(g_compiler->ObjectNameToPath(functionInstance._objectName), objectName);

                    // Register a new object of this type
                    declareNode->ResolveFunctionsAndRegisterObjects(RegisterObjectsMode::RegisterObjects, objectName,
                                                                    objectPath, functionInstance._objectName,
                                                                    registerObjectCallback);

                    addedNewObjects = true;

                    // Validate that static local objects do not have exported methods
                    const auto validateCallback = [&](const Type* const type)
                    {
                        const ClassType* const classType = dynamic_cast<const ClassType*>(type);

                        if (classType)
                        {
                            const std::vector<std::string>& memberFunctions = classType->GetMemberFunctions();

                            for (const std::string& functionName : memberFunctions)
                            {
                                const FunctionDesc& functionDesc = g_compiler->GetFunctionDesc(
                                    classType, Scope(), functionName, declareNode->GetLocation());

                                if (ParseTreeFunctionModifierExport ==
                                    (functionDesc._modifiers & ParseTreeFunctionModifierExport))
                                {
                                    g_compiler->ErrorStream(declareNode->GetLocation(),
                                                            CompileError::InvalidStaticLocal)
                                        << "Static local objects cannot contain exported methods";
                                }
                            }
                        }
                    };

                    declareNode->GetDeclaredType()->VisitTypes(validateCallback, VisitTypesBehavior::Default);
                }
            }
            else if (assignNode)
            {
                assignNode->HandleReferenceAssignment(resolveReferenceCallback, functionInstance._objectName,
                                                      functionInstance._functionNode, referenceMap);
            }
            else if (variableAccessNode)
            {
                // If a variable access node refers to a reference
                // replace it with the name of the referenced object
                // Remember this for use during IR generation
                const auto it = referenceMap.find(variableAccessNode->GetName());

                if (it != referenceMap.end())
                {
                    VariableAccessTable& vat = _variableAccessTables[functionInstance];

                    const auto it2 = vat.find(variableAccessNode);
                    if (it2 == vat.end())
                    {
                        vat[variableAccessNode] = it->second;
                    }
                    else
                    {
                        assert(it2->second == it->second);
                    }
                }
            }
            else if (arrayAccessNode && arrayAccessNode->IsEccMemory())
            {
                const ResolvedCall resolvedCall =
                    ResolveFlatFunctionCall(arrayAccessNode->GetEccFuncNameNode(), context._namespaceScope);

                FunctionInstance fi(resolvedCall._functionNode, GetInlineInstanceIndex(resolvedCall._functionNode),
                                    g_globalObjectName);

                addToWorkList(fi);

                _eccMemoryCalls[arrayAccessNode].push_back(fi);
            }

            // Always call recurseCallback
            // For example, one of the parameters to a call may itself be a call
            {
                recurseCallback();
            }

            // After traversing function parameter declarations
            // Update with compile-time known values of inline calls
            if (declareNode)
            {
                const auto it = knownParameterDeclarations.find(declareNode);
                if (it != knownParameterDeclarations.end())
                {
                    const DeclareNode* const declareNode = it->first;
                    const VisitContextPerVariableData& perVariableData = it->second;

                    VisitCheckKnownValueContext knownValueContext(context._visitContext);

                    declareNode->SetKnownValue(knownValueContext, &perVariableData._knownValue);
                }
            }
        };

        // Note that if this function is a method of an exported class which is not being compiled
        // then the body of the function will be empty, so this Visit() will not do anything interesting
        functionInstance._functionNode->Visit(callback, context._visitContext);

        visitedInstances.insert(functionInstance);
    }

    // Add instances for unreferenced external functions
    // to ensure a consistent signature for generated top-level modules
    for (const auto& p : perFunctionSymbolTable)
    {
        const FunctionNode* const functionNode = p.first;

        if ((functionNode->GetModifiers() & ParseTreeFunctionModifierExternal) && _allInstanceMap[functionNode].empty())
        {
            const FunctionInstance fi(functionNode, 0,
                                      _moduleToCompile._isDefaultPass ? g_globalObjectName
                                                                      : _moduleToCompile._placeholderObjectName);

            _allInstanceMap[functionNode].push_back(fi);
        }
    }

    // Validate that functions called by external class callbacks do not have other call sites (arbitration is not
    // supported)
    for (const FunctionInstance& fi : _externClassCallbackCallees)
    {
        const size_t callSiteCount = LookupWithDefault(_instanceCallCountMap, fi, static_cast<size_t>(0));

        if (callSiteCount > 0)
        {
            g_compiler->ErrorStream(fi._functionNode->GetLocation(), CompileError::InvalidCallback)
                << "Functions which are connected to external class callbacks cannot have other call sites: "
                << CombineObjectAndMemberName(fi._functionNode->GetContainingScope(), fi._objectName,
                                              fi._functionNode->GetName());
        }
    }
}

// Create export functions that will be hooked up to
// callbacks from an exported class
void FunctionInstanceEnumerator::RegisterExportClassInstance(
    const std::string& objectName, const ClassNode* const classNode,
    const ClassCallableFunctionMap& classCallableFunctionMap,
    const std::function<void(const FunctionInstance&)>& addToWorkList, PerFunctionSymbolTable& perFunctionSymbolTable)
{
    const ClassType* const classType = safe_cast<const ClassType*>(classNode->GetType());

    const_cast<ClassNode*>(classType->GetClassNode())->CreateExportCallbacks(objectName);

    // Determine which function each callback refers to
    for (const auto& member : classType->GetMemberVariables())
    {
        if (member.second->GetAssignNode())
        {
            // Callbacks with initial values are not exported
            continue;
        }

        if (dynamic_cast<const FunctionType*>(member.second->GetDeclaredType()))
        {
            // Generate a FunctionNode for an export function
            // When the exported class calls the callback
            // that call is routed to this export function
            const std::string& callbackName = member.first;

            const auto it = classCallableFunctionMap.find(callbackName);
            if (it == classCallableFunctionMap.end())
            {
                g_compiler->ErrorStream(member.second->GetLocation(), CompileError::UnknownFunction)
                    << "Callback (" << callbackName << ") of exported class not initialized";
                throw std::runtime_error("Uninitialized callback");
            }

            const ClassCallableFunction& ccf = it->second;

            const ClassNode* const classNode = classType->GetClassNode();

            // Make the export function call the target function
            const FunctionType* const callbackType = safe_cast<const FunctionType*>(member.second->GetDeclaredType());

            const FunctionNode* const callbackExportFunction = classNode->GetExportCallback(objectName, callbackName);

            ParseTreeNodePtr calledObjectName = nullptr;

            if (ccf._classType != g_compiler->GetGlobalClassType())
            {
                // Callback is connected to a method
                calledObjectName = ParseNamedVariableFromString(ccf._objectName);

                calledObjectName->SetType(ccf._classType);
            }

            ParseTreeNodePtr scopedCalledFunctionName = ParseBaseList(nullptr);

            if (!ccf._functionNode->GetContainingScope().empty())
            {
                for (const std::string& str : ccf._functionNode->GetContainingScope())
                {
                    scopedCalledFunctionName = ParseAppendList(scopedCalledFunctionName, ParseIdentifier(str.c_str()));
                }
            }

            scopedCalledFunctionName =
                ParseAppendList(scopedCalledFunctionName, ParseIdentifier(ccf._functionNode->GetName().c_str()));

            ParseTreeNodePtr specifier =
                ParseFunctionSpecifier(calledObjectName, ParseScopedIdentifier(scopedCalledFunctionName));

            ParseTreeNodePtr arguments = ParseBaseList(nullptr);

            for (size_t i = 0; i < callbackType->GetParamCount(); i++)
            {
                ParseTreeNode* argument = ParseNamedVariableFromString(callbackType->GetParamName(i));

                argument->SetType(callbackType->GetParamType(i));

                ParseAppendList(arguments, argument);
            }

            ParseTreeNodePtr call = ParseFunctionCall(specifier, arguments, ParseBaseList(nullptr));

            call->SetType(callbackType->GetReturnType());

            safe_cast<CallNode*>(call)->SetClassTypes(ccf._classType, g_compiler->GetGlobalClassType());

            // The function body is either:
            // 1) return foo();
            // or
            // 2) foo();
            ParseTreeNodePtr functionBody =
                callbackType->GetReturnType() == g_compiler->GetVoidType() ? call : ParseReturnExpression(call);

            const_cast<FunctionNode*>(callbackExportFunction)->SetStatements(functionBody);

            // Record the method for later lookup in _nonInlineInstanceMap
            {
                ObjectToInstanceMap& oim = _nonInlineInstanceMap[callbackExportFunction];

                // Exported functions do not have reference parameters
                assert(!callbackExportFunction->ContainsReferenceParameters());
                ObjectNameAndParameterReferences onpr;

                onpr.first = objectName;

                SafeInsert(oim, onpr, static_cast<size_t>(0));
            }

            // Add the exported function to the work list
            // To add it (and the function it calls) to the call graph
            const FunctionInstance exportInstance(callbackExportFunction, 0, objectName);
            addToWorkList(exportInstance);

            // Add an empty context to perFunctionSymbolTable for the exported function
            {
                Context context = {};

                VisitContext::PushPopScope globalScope(context._visitContext);

                context._classTypeStack.push(g_compiler->GetGlobalClassType());

                SafeInsert(perFunctionSymbolTable, callbackExportFunction, context);
            }
        }
    }
}

// Finds the set of all functions that are reachable from an exported function
void FunctionInstanceEnumerator::FindUnreachableFunctions()
{
    assert(_reachableFunctionInstances.empty());

    // Include the set of instances
    // which are called by extern class callbacks
    _reachableFunctionInstances = _externClassCallbackCallees;

    // Find instances corresponding to exported functions
    for (const auto& p1 : _nonInlineInstanceMap)
    {
        const FunctionNode* const functionNode = p1.first;

        if (0 == (functionNode->GetModifiers() & AlwaysReachableModifiers))
        {
            continue;
        }

        for (const auto& p2 : p1.second)
        {
            const ObjectNameAndParameterReferences& onpr = p2.first;
            const size_t instanceIndex = p2.second;

            // export functions should have no reference parameters
            assert(onpr.second.empty());

            const std::string& objectName = onpr.first;

            FunctionInstance instance = {};

            instance._functionNode = functionNode;
            instance._instanceIndex = instanceIndex;
            instance._objectName = objectName;

            _reachableFunctionInstances.insert(instance);
        }
    }

    // Build mapping of caller function instance to callee function instance
    std::map<FunctionInstance, std::set<FunctionInstance>> callMap;

    for (const auto& p1 : _callMap)
    {
        for (const auto& p2 : p1.second)
        {
            const FunctionInstance& callerInstance = p2.first;

            std::set<FunctionInstance>& calleeSet = callMap[callerInstance];

            for (const CalledInstanceDesc& cid : p2.second)
            {
                calleeSet.insert(cid._calleeInstance);
            }
        }
    }

    std::set<FunctionInstance> workList = _reachableFunctionInstances;

    while (!workList.empty())
    {
        std::set<FunctionInstance> instances;
        instances.swap(workList);
        assert(workList.empty());

        for (const FunctionInstance& fi : instances)
        {
            // Each instance should be processed no more than once
            assert(_reachableFunctionInstances.end() != _reachableFunctionInstances.find(fi));

            // Determine which functions are called
            const std::set<FunctionInstance>& callees = callMap[fi];

            for (const FunctionInstance& callee : callees)
            {
                const auto insertResult = _reachableFunctionInstances.insert(callee);

                if (insertResult.second)
                {
                    // first time this instance was found
                    workList.insert(callee);
                }
            }
        }
    }
}

// Returns the names of all objects containing an instance of the given function
std::vector<std::string> FunctionInstanceEnumerator::GetObjectNamesForFunction(const FunctionNode* const node) const
{
    std::set<std::string> result;

    const auto it = _allInstanceMap.find(node);

    if (it != _allInstanceMap.end())
    {
        for (const FunctionInstance& instance : it->second)
        {
            result.insert(instance._objectName);
        }
    }

    return std::vector<std::string>(result.begin(), result.end());
}

bool FunctionInstanceEnumerator::IsInlineable(const FunctionInstance& functionInstance,
                                              const size_t minimumCallCount) const
{
    size_t instanceCount = 0;

    // Increment instanceCount by the number of calls to this instance
    const auto it = _instanceCallCountMap.find(functionInstance);

    if (it != _instanceCallCountMap.end())
    {
        instanceCount += it->second;
    }

    const bool disallow = _disallowAutoInlineSet.end() != _disallowAutoInlineSet.find(functionInstance._functionNode);

    // Only enable auto-inlining if:
    // There is no more than 1 call site
    // and if modifiers on the caller & callee allow auto-inlining
    // and only if optimizing
    // The 0 call site case is allowed to support auto-inlining in unreachable functions
    return (instanceCount >= minimumCallCount) && (instanceCount <= 1) && !disallow &&
           (GetCodeGenConfig()._optimize > 0);
}

// Lookup a description of a called function
// (the first call has inlineIndex = 0, the next has inlineIndex = 1, etc)
FunctionInstanceEnumerator::CalledInstanceDesc
FunctionInstanceEnumerator::LookupCall(const CallNode* const callNode, const FunctionInstance& callerInstance)
{
    CalledInstanceDescriptionListMap& callDescriptionListMap = _callMap[callNode];

    std::list<CalledInstanceDesc>& callList = callDescriptionListMap[callerInstance];

    assert(!callList.empty());

    // Use the first instance from the list
    // This assumes that IR generation and function enumeration traverse a given instance in the same order
    const CalledInstanceDesc result = callList.front();
    callList.pop_front();

    return result;
}

FunctionInstance FunctionInstanceEnumerator::LookupBinaryOpCall(const BinaryOpNode* const node)
{
    std::list<FunctionInstance>& callList = _binaryOperationCalls[node];

    assert(!callList.empty());

    const FunctionInstance result = callList.front();
    callList.pop_front();

    return result;
}

FunctionInstance FunctionInstanceEnumerator::LookupEccMemoryCall(const ArrayAccessNode* const node)
{
    std::list<FunctionInstance>& callList = _eccMemoryCalls[node];

    assert(!callList.empty());

    const FunctionInstance result = callList.front();
    callList.pop_front();

    return result;
}

// Returns a number that unique identifies a static local object for a given function instance
size_t FunctionInstanceEnumerator::LookupStaticLocalObjectIndex(const DeclareNode* const declareNode,
                                                                const FunctionInstance& containingInstance) const
{
    const StaticLocalMap& slm = SafeLookup(_staticLocals, declareNode);

    return SafeLookup(slm, containingInstance);
}

// Returns the total set of unique identifiers for a given static local object declaration
std::list<size_t> FunctionInstanceEnumerator::GetAllStaticLocalIndices(const DeclareNode* const declareNode) const
{
    std::list<size_t> result;

    const auto it = _staticLocals.find(declareNode);

    if (it != _staticLocals.end())
    {
        const StaticLocalMap& slm = it->second;

        for (const auto& p : slm)
        {
            result.push_back(p.second);
        }
    }

    return result;
}

// Lookup all function instances for a given object & non-inline method
std::list<FunctionInstance> FunctionInstanceEnumerator::LookupMethodInstances(const FunctionNode* const functionNode,
                                                                              const std::string& objectName) const
{
    // Should not be called for inline functions
    assert(0 == (functionNode->GetModifiers() & ParseTreeFunctionModifierInline));

    const bool isFlatFunction = functionNode->GetClassType() == g_compiler->GetGlobalClassType();

    std::list<FunctionInstance> result;

    const auto it = _allInstanceMap.find(functionNode);

    if (it != _allInstanceMap.end())
    {
        const std::list<FunctionInstance>& instances = it->second;

        for (const FunctionInstance& fi : instances)
        {
            if (functionNode->ContainsReferenceParameters() && isFlatFunction)
            {
                // Flat Function with reference parameter
                // Return all instances (which will have synthetic object names)
                // Member functions that that reference parameters and have
                // multiple call sites are not supported
                result.push_back(fi);
            }
            else if (fi._objectName == objectName)
            {
                // Return instances with matching object names (at most 1)
                // A non-inline methods with reference parameters and multiple
                // call sites are not supported yet.  Lookups for
                // FunctionNode::Instance and CallNode::Instance
                // need to be referenced by FunctionInstance or {ObjectName, ParameterReferences} for this to be
                // supported.
                if (!result.empty())
                {
                    throw std::runtime_error(
                        "Non-inline methods with that access `this` and have multiple call sites are not supported");
                }
                result.push_back(fi);
            }
        }
    }

    return result;
}

bool FunctionInstanceEnumerator::IsInstanceReachable(const FunctionInstance& instance) const
{
    return _reachableFunctionInstances.end() != _reachableFunctionInstances.find(instance);
}

std::string FunctionInstanceEnumerator::GetReferencedObject(const VariableAccessNode* variableAccessNode,
                                                            const FunctionInstance& instance) const
{
    const VariableAccessTable& vat = SafeLookup(_variableAccessTables, instance);

    return SafeLookup(vat, variableAccessNode);
}

// Returns the number of call sites of a given FunctionNode & object name
size_t FunctionInstanceEnumerator::GetFunctionCallCount(const FunctionNode* const node,
                                                        const std::string& objectName) const
{
    size_t result = 0;

    const auto it = _allInstanceMap.find(node);

    if (it != _allInstanceMap.end())
    {
        for (const FunctionInstance& instance : it->second)
        {
            if (instance._objectName == objectName)
            {
                result += LookupWithDefault(_instanceCallCountMap, instance, static_cast<size_t>(0));
            }
        }
    }

    // [[reset]] functions have an implicit caller
    // which is the automatic call that occurs after reset
    if (node->GetModifiers() & ParseTreeFunctionModifierReset)
    {
        result++;
    }

    return result;
}

// Used to determine the function that a callback refers to
UnresolvedCall FunctionInstanceEnumerator::DereferenceCallback(const ClassType* callSiteClassType,
                                                               const std::string& callSiteObjectName,
                                                               const std::string& callbackName,
                                                               const Location& callSiteLocation) const
{
    const ClassCallableFunctionMap& callableFunctionMap = SafeLookup(_objectToCallableFunctions, callSiteObjectName);

    const auto it = callableFunctionMap.find(callbackName);
    if (it == callableFunctionMap.end())
    {
        g_compiler->ErrorStream(callSiteLocation, CompileError::InvalidCall)
            << "Callback: " << callbackName << " not initialized";
        throw std::runtime_error("Uninitialized callback");
    }

    const ClassCallableFunction& ccf = it->second;

    UnresolvedCall ur = {};

    ur._calledFunctionName = ccf._functionNode->GetName();
    ur._callSiteClassType = ccf._classType;
    ur._callSiteObjectName = ccf._objectName;

    if (ccf._classType == g_compiler->GetGlobalClassType())
    {
        // The callback references to a scoped global function
        // Store the scope
        ur._explicitScope = ccf._functionNode->GetContainingScope();
    }

    return ur;
}

FunctionInstance FunctionInstanceEnumerator::DereferenceExternalClassCallback(const ClassType* classType,
                                                                              const std::string& externClassObjectName,
                                                                              const std::string& callbackName,
                                                                              const Location& location) const
{
    const ClassCallableFunctionMap& callableFunctionMap = SafeLookup(_objectToCallableFunctions, externClassObjectName);

    const auto it = callableFunctionMap.find(callbackName);
    if (it == callableFunctionMap.end())
    {
        g_compiler->ErrorStream(location, CompileError::InvalidCall)
            << "Callback: " << callbackName << " not initialized";
        throw std::runtime_error("Uninitialized callback");
    }

    const ClassCallableFunction& ccf = it->second;

    // Calls from external code back to generated RTL cannot be inlined
    assert(!ccf._functionNode->IsInline());

    FunctionInstance fi = {};

    fi._functionNode = ccf._functionNode;

    const ObjectToInstanceMap& objectToInstanceMap = SafeLookup(_nonInlineInstanceMap, ccf._functionNode);
    const ObjectNameAndParameterReferences onpr(ccf._objectName, {});
    fi._instanceIndex = SafeLookup(objectToInstanceMap, onpr);

    fi._objectName = ccf._objectName;

    // The called function must be reachable
    assert(IsInstanceReachable(fi));

    return fi;
}

std::string FunctionInstanceEnumerator::DereferenceParameter(const FunctionInstance& callerInstance,
                                                             const CallNode* const callNode,
                                                             const std::string& paramName)
{
    std::string result = paramName;

    const std::pair<FunctionInstance, const CallNode*> key(callerInstance, callNode);

    const auto it = _parameterReferencesAtCallSites.find(key);
    if (it != _parameterReferencesAtCallSites.end())
    {
        const ParameterReferenceMap& prm = it->second;

        result = LookupWithDefault(prm, paramName, paramName);
    }

    return result;
}

// Determines the called object name associated with a call site
// This works even for calls to fixed-latency extern methods
// for which LookupCall does not work
std::string FunctionInstanceEnumerator::GetCalledObjectName(const CallNode* const callNode,
                                                            const FunctionInstance& callerInstance)
{
    const CallSite callSite(callNode, callerInstance);

    std::list<std::string>& calledObjectNames = _callSiteToCalledObjectName[callSite];

    // Use the first instance from the list
    // This assumes that IR generation and function enumeration traverse a given instance in the same order
    assert(!calledObjectNames.empty());

    const std::string result = calledObjectNames.front();
    calledObjectNames.pop_front();

    return result;
}

// Returns the instance which calls the `inst`.  Only returns valid instance
// if there is exactly 1 calling instance.
std::optional<FunctionInstanceEnumerator::InstanceAndLocation>
FunctionInstanceEnumerator::GetCaller(const FunctionInstance& inst) const
{
    std::optional<InstanceAndLocation> result = {};

    const auto it = _calleeToCaller.find(inst);
    if (it != _calleeToCaller.end())
    {
        const std::set<InstanceAndLocation>& callers = it->second;

        if (callers.size() == 1)
        {
            result = std::optional<InstanceAndLocation>(*(callers.begin()));
        }
    }

    return result;
}

// Generates a call trace for a given function instance
void FunctionInstanceEnumerator::GenerateCallTrace(std::ostream& str, const FunctionInstance& srcInst)
{
    InstanceAndLocation instAndLocation(srcInst, srcInst._functionNode->GetLocation());

    str << "  Call trace:\n";

    while (true)
    {
        const FunctionInstance inst = instAndLocation.first;
        const Location callLoc = instAndLocation.second;

        // Emit one frame of the stack
        const std::string functionName = inst._functionNode->GetUnmangledName();

        const ClassType* const classType = inst._functionNode->GetClassType();

        const std::string name = classType == g_compiler->GetGlobalClassType()
                                     ? functionName
                                     : classType->GetClassNode()->GetUnmangledName() + "::" + functionName;

        str << "    " << name << " in file ";
        str << LocationToString(callLoc, true /*stripLeadingPath*/, " at line ") << "\n";

        // Move to the next frame
        std::optional<InstanceAndLocation> nextInst = GetCaller(inst);

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

// Generates a call trace from the function instance which is currently being enumerated
bool FunctionInstanceEnumerator::GenerateCurrentEnumerateFunctionCallTrace(std::ostream& str)
{
    bool result = false;

    if (_currentInstance.has_value())
    {
        result = true;

        GenerateCallTrace(str, *_currentInstance);
    }

    return result;
}

// Returns the set of function instances which are callable via extern class callbacks
const std::set<FunctionInstance> FunctionInstanceEnumerator::GetExternalClassInstanceCallees() const
{
    return _externClassCallbackCallees;
}

void Compiler::EnumerateFunctionInstances(const CompiledModule& moduleToCompile)
{
    assert(!_functionEnumerator);

    _functionEnumerator.reset(new FunctionInstanceEnumerator(moduleToCompile, _isCompilingToVerilog));

    _functionEnumerator->Enumerate(_root);

    _functionEnumerator->FindUnreachableFunctions();
}
