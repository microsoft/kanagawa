// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Declarations used in the interface between the parser and the compiler
#pragma once

#include <stdbool.h>

#include "options.h"

#ifdef __cplusplus
extern "C"
{
#endif

#define DECLARE_FLAG_CONST 0x00000001
#define DECLARE_FLAG_GLOBAL 0x00000002
#define DECLARE_FLAG_INITIALIZE 0x00000004
#define DECLARE_FLAG_PARAMETER 0x00000008
#define DECLARE_FLAG_PER_THREAD 0x00000010
#define DECLARE_FLAG_END_TRANSACTION 0x00000020
#define DECLARE_FLAG_STATIC 0x00000040
#define DECLARE_FLAG_CLASS 0x00000080
#define DECLARE_FLAG_UNINIT_CONST 0x00000100

#ifdef EXPORT_API
#define API __declspec(dllexport)
#endif

#ifndef API
#define API
#endif

    typedef struct _ParseTreeNode* ParseTreeNodePtr;

    typedef struct
    {
        size_t _beginLine;
        size_t _beginColumn;
        size_t _endLine;
        size_t _endColumn;
        size_t _fileIndex;
        bool _valid;
    } Location;

    typedef enum _ParseTreeBinaryOpType
    {
        ParseTreeBinaryOpTypeAdd,
        ParseTreeBinaryOpTypeSub,
        ParseTreeBinaryOpTypeMul,
        ParseTreeBinaryOpTypeLutMul,
        ParseTreeBinaryOpTypeDiv,
        ParseTreeBinaryOpTypeMod,
        ParseTreeBinaryOpTypeAnd,
        ParseTreeBinaryOpTypeLogicalAnd,
        ParseTreeBinaryOpTypeOr,
        ParseTreeBinaryOpTypeLogicalOr,
        ParseTreeBinaryOpTypeXor,
        ParseTreeBinaryOpTypeLogicalXor,
        ParseTreeBinaryOpTypeShl,
        ParseTreeBinaryOpTypeShr,
        ParseTreeBinaryOpTypeEQ,
        ParseTreeBinaryOpTypeNE,
        ParseTreeBinaryOpTypeGT,
        ParseTreeBinaryOpTypeGE,
        ParseTreeBinaryOpTypeLT,
        ParseTreeBinaryOpTypeLE,
    } ParseTreeBinaryOpType;

    typedef enum _ParseTreeUnaryOpType
    {
        ParseTreeUnaryOpTypeInvert,
        ParseTreeUnaryOpTypeNegate,
        ParseTreeUnaryOpTypeLogicalInvert
    } ParseTreeUnaryOpType;

    typedef enum _ParseTreeLoopMode
    {
        ParseTreeLoopModeOrdered,
        ParseTreeLoopModeUnordered,
        ParseTreeLoopModeReorderByLooping
    } ParseTreeLoopMode;

    typedef enum _ParseTreeSizeofType
    {
        ParseTreeSizeofTypeBit,
        ParseTreeSizeofTypeByte,
    } ParseTreeSizeofType;

    typedef enum _ParseTreeFunctionModifier
    {
        ParseTreeFunctionModifierNone = 0,
        ParseTreeFunctionModifierInline = (1 << 0),
        ParseTreeFunctionModifierAsync = (1 << 1),
        ParseTreeFunctionModifierExport = (1 << 2), // TODO: FOSS - Remove after this has been removed from front-end (PR 39888)
        ParseTreeFunctionModifierPipelined = (1 << 3),
        ParseTreeFunctionModifierIntrinsic = (1 << 4),
        ParseTreeFunctionModifierUnordered = (1 << 5),
        ParseTreeFunctionModifierExternal = (1 << 6),
        ParseTreeFunctionModifierExternalFixedLatency = (1 << 7),
        ParseTreeFunctionModifierNoBackPressure = (1 << 8),
        ParseTreeFunctionModifierReorderByLooping = (1 << 9),
        ParseTreeFunctionModifierNoInline = (1 << 10),
        ParseTreeFunctionModifierExternalExternallyInstantiated = (1 << 11),
        ParseTreeFunctionModifierReset = (1 << 13),

        // Interface method or callback as part of an export class
        // This is set both when an export class is used, and when it is defined
        ParseTreeFunctionModifierExportClassInterface = (1 << 14),

        ParseTreeFunctionModifierPure = (1 << 15),

        // Do not emit warnings about missing [[transaction_size()]] attribute
        // for calls *from* this function
        ParseTreeFunctionModifierNoSrcTxWarning = (1 << 16)
    } ParseTreeFunctionModifier;

    typedef enum _ParseTreeMemberProtectionModifier
    {
        ParseTreeMemberProtectionModifierPublic,
        ParseTreeMemberProtectionModifierPrivate
    } ParseTreeMemberProtectionModifier;

    typedef enum _ParseTreeArrayType
    {
        ParseTreeArrayTypeDefault,
        ParseTreeArrayTypeMemory
    } ParseTreeArrayType;

    typedef enum _ParseTreeMemoryType
    {
        ParseTreeMemoryTypeDefault,
        ParseTreeMemoryTypeNoReplication,
        ParseTreeMemoryTypeQuadPort,
        ParseTreeMemoryTypeEcc,
        ParseTreeMemoryTypeInitialize
    } ParseTreeMemoryType;

    typedef enum _ParseTreeAttribute
    {
        ParseTreeCallRateAttr,
        ParseTreeClockAttr,
        ParseTreeFifoDepthAttr,
        ParseTreeLatencyAttr,
        ParseTreeMaxThreadsAttr,
        ParseTreeScheduleAttr,
        ParseTreeThreadRateAttr,
        ParseTreeTransactionSizeAttr,
        ParseTreeNameAttr,
        ParseTreeMarshalAttr
    } ParseTreeAttribute;

    typedef enum _ParseTreeFormatSpecifier
    {
        ParseTreeFormatSpecifierBin,
        ParseTreeFormatSpecifierOct,
        ParseTreeFormatSpecifierDec,
        ParseTreeFormatSpecifierHex,
        ParseTreeFormatSpecifierHexUpper,
        ParseTreeFormatSpecifierNone
    } ParseTreeFormatSpecifier;

    typedef const char* const* ParseNamespaceScopePtr;
    typedef const char* const* ParseQualifiedNamePtr;

    API ParseTreeNodePtr ParseDeclare(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr,
                                      unsigned int flags, ParseNamespaceScopePtr);
    API ParseTreeNodePtr ParseFunctionParam(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr,
                                            ParseNamespaceScopePtr);
    API ParseTreeNodePtr ParseFlagAttribute(unsigned int attr);
    API ParseTreeNodePtr ParseIntAttribute(unsigned int attr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseAssign(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseIf(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseDoWhile(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseUnrolledFor(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseRangeFor(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseAnnotatedStatement(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseMux(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseBinaryOp(ParseTreeBinaryOpType, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseUnaryOp(ParseTreeUnaryOpType, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseSizeOf(ParseTreeSizeofType, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseCast(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseFunctionSpecifier(ParseTreeNodePtr object, ParseTreeNodePtr name);
    API ParseTreeNodePtr ParseFunctionCall(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseBaseList(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseAppendList(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseArrayType(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseFunctionModifier(ParseTreeFunctionModifier);
    API ParseTreeNodePtr ParseFunctionTypeParam(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseFunctionType(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseFunction(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr,
                                       ParseTreeNodePtr, ParseTreeNodePtr, const char*);
    API ParseTreeNodePtr ParseReturn();
    API ParseTreeNodePtr ParseReturnExpression(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseNestedScope(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseStruct(ParseTreeNodePtr, ParseTreeNodePtr, ParseNamespaceScopePtr, const char*);
    API ParseTreeNodePtr ParseUnion(ParseTreeNodePtr, ParseTreeNodePtr, ParseNamespaceScopePtr, const char*);
    API ParseTreeNodePtr ParseNamedType(ParseTreeNodePtr, ParseNamespaceScopePtr);
    API ParseTreeNodePtr ParseNamedVariable(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseKnownConstNamedVariable(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseAccessArray(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseAccessArrayLValue(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseAccessMember(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseMemberModifier(ParseTreeMemberProtectionModifier);
    API ParseTreeNodePtr ParseClass(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr, ParseNamespaceScopePtr,
                                    const char*);
    API ParseTreeNodePtr ParseDesignator(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseInitializerList(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseSwitch(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseSwitchBlock(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseExternalFunction(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseConst(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseStatic(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseFanOut(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseBarrier();
    API ParseTreeNodePtr ParseStages(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseConcat(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseReorder(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseAttribute(unsigned int attr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseTypedef(ParseTreeNodePtr, ParseTreeNodePtr, ParseNamespaceScopePtr, const char*);
    API ParseTreeNodePtr ParseStaticAssert(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseExportType(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseExtern(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseStringType();
    API ParseTreeNodePtr ParseStringLiteral(const char*);
    API ParseTreeNodePtr ParseInterpolatedString(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseInterpolatedStringSegment(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseInterpolationExpression(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeFormatSpecifier,
                                                      unsigned int);
    API ParseTreeNodePtr ParseVoidType();
    API ParseTreeNodePtr ParseBoolType();
    API ParseTreeNodePtr ParseBoolLiteral(BOOL);
    API ParseTreeNodePtr ParseFloatType();
    API ParseTreeNodePtr ParseFloatLiteral(float);
    API ParseTreeNodePtr ParseIntType(int);
    API ParseTreeNodePtr ParseDecimalLiteral(const char*);
    API ParseTreeNodePtr ParseUintType(int);
    API ParseTreeNodePtr ParseIdentifier(const char*);
    API ParseTreeNodePtr ParseNamespace(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseScopedIdentifier(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseEnum(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr, ParseNamespaceScopePtr);
    API ParseTreeNodePtr ParseEnumConstant(ParseTreeNodePtr, ParseTreeNodePtr, ParseNamespaceScopePtr);
    API ParseTreeNodePtr ParseEnumValue(ParseTreeNodePtr, ParseTreeNodePtr);
    API ParseTreeNodePtr ParseReference(ParseTreeNodePtr);
    API ParseTreeNodePtr ParseThis(ParseNamespaceScopePtr);
    API ParseTreeNodePtr ParseQualifiedName(ParseQualifiedNamePtr name);
    API ParseTreeNodePtr ParseTemplateArg(const char* name, ParseTreeNodePtr arg);
    API ParseTreeNodePtr ParseTemplateInstance(ParseTreeNodePtr templateName, ParseTreeNodePtr templateArguments);
    API ParseTreeNodePtr ParseDefaultInitialization(ParseTreeNodePtr init);

    API void SetNodeType(ParseTreeNodePtr, ParseTreeNodePtr);
    API void SetLocation(Location);
    API void SetLocation2(const Location*);
    API void UnknownLocation();
    API BOOL InitCompiler(const Options*);
    API BOOL Codegen(const char* target, const char* output, const char* res, const char* dmgl,
                     const char* dmglDetailed, ParseTreeNodePtr root);

#ifdef __cplusplus
};
#endif

#ifdef __cplusplus

bool operator<(const Location& lhs, const Location& rhs);

ParseTreeNodePtr ParseScopedIdentifierFromString(const std::string& str);

ParseTreeNodePtr ParseNamedVariableFromString(const std::string& str);

enum class BaseType
{
    Int,
    Uint
};

typedef struct _ParseTreeNode ParseTreeNode;

struct AccessedRegister;
struct Operation;

class FunctionNode;
class DesignatedInitializerListNode;

// A single instance of a function in the IR
// Functions can have multiple instances for 2 reasons:
// 1) Inline functions have 1 instance per call site
// 2) Methods have 1 instance per object
struct FunctionInstance
{
    const FunctionNode* _functionNode;
    size_t _instanceIndex;
    std::string _objectName;

    FunctionInstance() : _functionNode(nullptr), _instanceIndex(0) {}

    FunctionInstance(const FunctionNode* const fn, const size_t index, const std::string& objectName)
        : _functionNode(fn), _instanceIndex(index), _objectName(objectName)
    {
    }

    bool operator<(const FunctionInstance& rhs) const;

    bool operator==(const FunctionInstance& rhs) const;

    std::string GetObjectAndFunctionName() const;

    std::string GetUnmangledObjectAndFunctionName() const;
};

struct FileAndLineNumber
{
    FileAndLineNumber() : _fileIndex(0), _lineNumber(0), _columnNumber(0) {}

    size_t _fileIndex;
    size_t _lineNumber;
    size_t _columnNumber;
    boost::optional<size_t> _callStackIndex;

    friend std::ostream& operator<<(std::ostream& str, const FileAndLineNumber& loc);

    bool operator==(const FileAndLineNumber& rhs) const
    {
        return ((_fileIndex == rhs._fileIndex) && (_lineNumber == rhs._lineNumber) &&
                (_columnNumber == rhs._columnNumber) && (_callStackIndex == rhs._callStackIndex));
    }

    bool operator!=(const FileAndLineNumber& rhs) const { return !(*this == rhs); }

    bool operator<(const FileAndLineNumber& rhs) const
    {
        if (_fileIndex < rhs._fileIndex)
        {
            return true;
        }
        else if (_fileIndex > rhs._fileIndex)
        {
            return false;
        }
        else if (_lineNumber < rhs._lineNumber)
        {
            return true;
        }
        else if (_lineNumber > rhs._lineNumber)
        {
            return false;
        }
        else if (_columnNumber < rhs._columnNumber)
        {
            return true;
        }
        else if (_columnNumber > rhs._columnNumber)
        {
            return false;
        }
        else
        {
            return _callStackIndex < rhs._callStackIndex;
        }
    }
};

enum class RegisterType
{
    Global,
    Local,
    Pipeline,
    Fifo,
    Wire,
    Memory,
    GlobalView, // A function of global variables

    // Means that the operation result is not needed
    // The operation must still be executed (in case it has side-effects)
    BitBucket
};

struct Program;

enum class ContainerType
{
    Struct,
    Union
};

enum class KnownValueType
{
    None,
    Int,
    String
};

struct KnownValue
{
    KnownValue() : _type(KnownValueType::None) {}

    KnownValue(const mp_int& i) : _type(KnownValueType::Int), _intVal(i) {}

    KnownValue(const std::string& s) : _type(KnownValueType::String), _stringVal(s) {}

    KnownValueType _type;
    mp_int _intVal;
    std::string _stringVal;
};

enum class TemplateArgumentType
{
    IntegerLiteral,
    StringLiteral
};

struct TemplateArgument
{
    TemplateArgumentType _type;
    std::string _name;
    std::string _literalValue;
};

class AllocatedRegister;
class AllocatedLeafRegister;
class SourceWriter;
class Type;
class SymbolWriter;
class ScopedIdentifierNode;
class ClassType;
class FunctionType;

typedef std::list<std::string> Scope;

inline Scope ToScope(ParseNamespaceScopePtr scope)
{
    Scope _scope;

    for (; scope && *scope; ++scope)
        _scope.push_front(*scope);

    return _scope;
}

bool ScopesEqual(const Scope& lhs, const Scope& rhs);

std::string FlattenScope(const Scope& scope);
std::string FlattenScopeAndAppendName(const Scope& scope, const std::string& name);
std::string FlattenScopeAndAppendFunctionName(const ClassType* const classType, const Scope& scope,
                                              const std::string& name);
Scope CombineScopes(const Scope& lhsIn, const Scope& rhsIn);
std::list<Scope> GetScopeSearchList(const Scope& implicitScope, const Scope& explicitScope);

typedef std::pair<Scope, std::string> ScopeAndName;
ScopeAndName StringToScopeAndName(const std::string& str);

std::string GetCombinedExternalClassInstanceFunctionName(const std::string& moduleName,
                                                         const std::string& functionName);

ErrStream GetCompilerErrorStream(const Location& location, const CompileError error);

size_t GetFunctionMaxThreadCount(const ParseTreeNode* const modifierList, const Location& location,
                                 const std::optional<size_t> functionCallCount = {});

typedef std::function<void(const Type* const)> TypeVisitFunction;

class BaseInitializerListNode;
class ClassNode;
class DeclareNode;

typedef std::function<std::string(const std::string&)> ResolveReferenceFunction;

enum class VisitTypesBehavior
{
    Default,
    SkipReferences // do not traverse into references
};

enum class RegisterObjectsMode
{
    // Used when new objects are added
    RegisterObjects,

    // Used to enumerate objects that have already been added
    EnumerateOnly
};

// Describes a function that is callable from within a class
// Used to handle method calls without an explicit object, and calls of callbacks
struct ClassCallableFunction
{
    std::string _objectName;

    const ClassType* _classType;

    const FunctionNode* _functionNode;
};

// Maps identifier to concrete function to call
using ClassCallableFunctionMap = std::map<std::string, ClassCallableFunction>;

// Maps object name to ObjectToClassCallableFunctionMap
using ObjectToClassCallableFunctionMap = std::map<std::string, ClassCallableFunctionMap>;

using ObjectCallback = std::function<void(const std::string& objectName, const ClassNode* const classNode,
                                          const ClassCallableFunctionMap& classCallableFunctionMap)>;

using DefaultInitializeCallback = std::function<void(
    const std::string& callbackName, const std::string& childObjectName, const FunctionType* const callbackType)>;

inline void DefaultObjectCallback(const std::string&, const ClassNode* const, const ClassCallableFunctionMap&) {}

inline void DefaultDefaultInitializeCallback(const std::string&, const std::string&, const FunctionType* const) {}

struct IRContext;

class Type
{
  public:
    virtual ~Type(){};

    virtual const Type* ResolveDeferredType(const Location&) const { return this; }

    virtual size_t GetBitWidth() const = 0;

    virtual const Type* GetElementType(const size_t elementIndex) const
    {
        // Should not be called on the base class
        assert(false);
        return nullptr;
    }

    virtual std::string GetName() const = 0;

    virtual std::string GetVerilogName() const { return GetName(); }

    virtual std::string GetCppMemberModifier(const size_t srcOffset, const size_t width) const
    {
        assert(0 == srcOffset);
        assert(width == GetBitWidth());

        return "";
    }

    static bool TypeCheckConversion(const Type* const dst, const Type* const src, const Location& location);

    virtual ParseTreeFunctionModifier GetModifier() const { return ParseTreeFunctionModifierNone; }

    virtual AllocatedRegister* AllocateRegisters(Program& program, const RegisterType type, const std::string& name,
                                                 const ObjectPath& containerPath = {},
                                                 const SourceVariable& source = {}) const
    {
        assert(false);
        return nullptr;
    }

    virtual bool SupportsInitialization() const { return true; }

    virtual bool IsPod() const { return true; }

    virtual bool ShouldExport() const { return true; }

    virtual void RegisterObjects(const RegisterObjectsMode mode, const std::string& objectName,
                                 const ObjectPath& objectPath, const ObjectCallback& objectCallback,
                                 const DefaultInitializeCallback& defaultInitializeCallback,
                                 ObjectToClassCallableFunctionMap& objectToFunctions) const
    {
    }

    ParseTreeNodePtr CreateParseTreeNode() const;

    virtual size_t GetRegisterCount() const
    {
        assert(false);
        return 0;
    }

    virtual void EnumerateContainedObjectNames(std::vector<std::string>& objectNames) const {}

    virtual void ExportVerilog(SourceWriter& writer) const;

    virtual void ExportVerilogTypedef(SourceWriter& writer, const std::string&) const { ExportVerilog(writer); }

    virtual void VisitTypes(const TypeVisitFunction& callback, const VisitTypesBehavior behavior) const
    {
        callback(this);
    }

    virtual void WriteSymbols(SymbolWriter& symbolWriter) const;

    virtual bool ContainsMemberRecursive(const std::string& memberName, const std::string& parentObjectName = "") const
    {
        // Only used to find member objects
        return false;
    }

    virtual std::map<std::string, std::string>
    GetReferences(const std::string& srcName, const std::string& dstName,
                  const ResolveReferenceFunction& resolveReferenceCallback) const
    {
        throw std::runtime_error(std::string(typeid(*this).name()) + " doesn't implement GetReferences");
    }

    virtual void Reset() {}

  private:
    Type& operator=(const Type& rhs) const;
};

class VoidType : public Type
{
  public:
    size_t GetBitWidth() const override { return 0; }

    std::string GetName() const override { return std::string("void"); }

    bool IsPod() const override
    {
        // void cannot be local variables, array elements, or struct members
        return false;
    }

    void ExportVerilog(SourceWriter& writer) const override {}

    size_t GetRegisterCount() const override { return 0; }

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType type, const std::string& name,
                                         const ObjectPath& containerPath,
                                         const SourceVariable& source) const override;

  private:
    VoidType& operator=(const VoidType& rhs);
};

class StringType : public Type
{
  public:
    size_t GetBitWidth() const override;

    std::string GetName() const override { return std::string("string"); }

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType type, const std::string& name,
                                         const ObjectPath& containerPath,
                                         const SourceVariable& source = {}) const override;

    std::string GetVerilogName() const override;

    void ExportVerilog(SourceWriter& writer) const override { ExportVerilog(writer, GetVerilogName()); }

    void ExportVerilogTypedef(SourceWriter& writer, const std::string& name) const override
    {
        ExportVerilog(writer, name);
    }

    void ExportVerilog(SourceWriter& writer, const std::string& name) const;

  private:
    StringType& operator=(const StringType& rhs);
};

class BoolType : public Type
{
  public:
    size_t GetBitWidth() const override { return 1; }

    std::string GetName() const override { return std::string("bool"); }

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType registerType, const std::string& name,
                                         const ObjectPath& containerPath = {},
                                         const SourceVariable& source = {}) const override;

    size_t GetRegisterCount() const override { return 1; }

    void ExportVerilog(SourceWriter& writer) const override { ExportVerilog(writer, GetVerilogName()); }

    void ExportVerilogTypedef(SourceWriter& writer, const std::string& name) const override
    {
        ExportVerilog(writer, name);
    }

    void ExportVerilog(SourceWriter& writer, const std::string& name) const;

  private:
    BoolType& operator=(const BoolType& rhs);
};

class FloatType : public Type
{
  public:
    FloatType(const size_t bitWidth) : _bitWidth(bitWidth) {}

    size_t GetBitWidth() const override { return _bitWidth; }

    std::string GetName() const override;

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType registerType, const std::string& name,
                                         const ObjectPath& containerPath,
                                         const SourceVariable& source) const override;

    size_t GetRegisterCount() const override { return 1; }

    void ExportVerilog(SourceWriter& writer) const override { ExportVerilog(writer, GetVerilogName()); }

    void ExportVerilogTypedef(SourceWriter& writer, const std::string& name) const override
    {
        ExportVerilog(writer, name);
    }

    void ExportVerilog(SourceWriter& writer, const std::string& name) const;

  private:
    FloatType& operator=(const FloatType& rhs);

    const size_t _bitWidth;
};

class ArrayTypeBase : public Type
{
  public:
    ArrayTypeBase(const Type* const elementType, const size_t arraySize, const Location& location);

    size_t GetBitWidth() const override;

    const Type* GetElementType(const size_t elementIndex) const override;

    size_t GetTotalCount() const;

    const Type* const _elementType;

    const size_t _arraySize;

    bool IsPod() const override { return _elementType->IsPod(); }

    size_t GetRegisterCount() const override;

    void EnumerateContainedObjectNames(std::vector<std::string>& objectNames) const override
    {
        _elementType->EnumerateContainedObjectNames(objectNames);
    }

    void VisitTypes(const TypeVisitFunction& callback, const VisitTypesBehavior behavior) const override
    {
        _elementType->VisitTypes(callback, behavior);

        Type::VisitTypes(callback, behavior);
    }

    void WriteSymbols(SymbolWriter& symbolWriter) const override;

    bool ContainsMemberRecursive(const std::string& memberName, const std::string& parentObjectName) const override;

  private:
    ArrayTypeBase& operator=(const ArrayTypeBase& rhs);
};

class ArrayType : public ArrayTypeBase
{
  public:
    ArrayType(const Type* const elementType, const size_t arraySize, const Location& location);

    const Type* ResolveDeferredType(const Location& location) const override;

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType registerType, const std::string& name,
                                         const ObjectPath& containerPath,
                                         const SourceVariable& source) const override;

    void RegisterObjects(const RegisterObjectsMode mode, const std::string& objectName, const ObjectPath& objectPath,
                         const ObjectCallback& objectCallback,
                         const DefaultInitializeCallback& defaultInitializeCallback,
                         ObjectToClassCallableFunctionMap& objectToFunctions) const override;

    std::string GetName() const override;

    std::string GetVerilogName() const override;

    std::string GetCppMemberModifier(const size_t srcOffset, const size_t width) const override;

    void ExportVerilog(SourceWriter& writer) const override;

    void ExportVerilogTypedef(SourceWriter& writer, const std::string&) const override { ExportVerilog(writer); }

    void WriteSymbols(SymbolWriter& symbolWriter) const override;

    std::string GetNameWithDimensions(std::vector<size_t>& dims) const;

  private:
    ArrayType& operator=(const ArrayType& rhs);
};

class MemoryType : public ArrayTypeBase
{
  public:
    MemoryType(const Type* const elementType, const size_t arraySize, const ParseTreeMemoryType memoryType,
               const ScopedIdentifierNode* const eccFuncNameNode, const bool autoInitialize, const Location& location);

    const Type* ResolveDeferredType(const Location& location) const override;

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType registerType, const std::string& name,
                                         const ObjectPath& containerPath,
                                         const SourceVariable& source) const override;

    std::string GetName() const override;

    bool SupportsInitialization() const override
    {
        // The contents of memory blocks are not initialized on reset
        return false;
    }

    bool IsPod() const override
    {
        // Memories cannot be local variables, array elements, or struct members
        return false;
    }

    void ExportVerilog(SourceWriter& writer) const override;

    void ExportVerilogTypedef(SourceWriter& writer, const std::string&) const override { ExportVerilog(writer); }

    void WriteSymbols(SymbolWriter& symbolWriter) const override;

    ParseTreeMemoryType GetMemoryType() const { return _memoryType; }

    const ScopedIdentifierNode* GetEccFuncNameNode() const { return _eccFuncNameNode; }

  private:
    MemoryType& operator=(const MemoryType& rhs);

    const ParseTreeMemoryType _memoryType;

    const ScopedIdentifierNode* const _eccFuncNameNode;

    const bool _autoInitialize;
};

class StructUnionType : public Type
{
  public:
    typedef std::pair<std::string, const DeclareNode*> EntryType;

    StructUnionType(const std::string& name, ContainerType type, const std::vector<EntryType>& children);

    size_t GetBitWidth() const override;

    const Type* GetElementType(const size_t elementIndex) const override;

    std::string GetName() const override { return _name; }

    std::string GetCppMemberModifier(const size_t srcOffset, const size_t width) const override;

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType registerType, const std::string& name,
                                         const ObjectPath& containerPath,
                                         const SourceVariable& source) const override;

    const std::vector<EntryType> _members;

    const ContainerType _type;

    const std::string _name;

    size_t GetRegisterCount() const override;

    void ExportVerilog(SourceWriter& writer) const override;

    void ExportVerilogTypedef(SourceWriter& writer, const std::string& name) const override;

    void VisitTypes(const TypeVisitFunction& callback, const VisitTypesBehavior behavior) const override;

    void WriteSymbols(SymbolWriter& symbolWriter) const override;

    std::string GetVerilogName() const override;

    std::vector<EntryType> GetFlattenedMembers() const;

    std::map<std::string, std::string>
    GetReferences(const std::string& srcName, const std::string& dstName,
                  const ResolveReferenceFunction& resolveReferenceCallback) const override;

    bool ShouldExport() const override;

    void InterpolateUnion(IRContext& context, Operation& outerOp, const AccessedRegister valueReg) const;

  private:
    StructUnionType& operator=(const StructUnionType& rhs);
};

class LeafType : public Type
{
  public:
    LeafType(const BaseType baseType, const size_t bitCount, const Location& location);

    size_t GetBitWidth() const override;

    std::string GetName() const override;

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType registerType, const std::string& name,
                                         const ObjectPath& containerPath = {},
                                         const SourceVariable& source = {}) const override;

    size_t GetRegisterCount() const override { return 1; }

    void ExportVerilog(SourceWriter& writer) const override { ExportVerilog(writer, GetVerilogName()); }

    void ExportVerilogTypedef(SourceWriter& writer, const std::string& name) const override
    {
        ExportVerilog(writer, name);
    }

    virtual void ExportVerilog(SourceWriter& writer, const std::string& name) const;

    BaseType _baseType;
    size_t _width;
};

class EnumType : public LeafType
{
  public:
    typedef std::pair<std::string, mp_int> EntryType;

    EnumType(const std::string& name, const LeafType* baseType, const std::vector<EntryType>& constants,
             const Location& location)
        : LeafType(baseType->_baseType, baseType->_width, location), _name(name), _constants(constants),
          _location(location)
    {
    }

    std::string GetName() const override { return _name; }

    void ExportVerilog(SourceWriter& writer, const std::string& name) const override;

    std::string GetVerilogName() const override;

    mp_int GetConstantValue(size_t) const;

    void WriteSymbols(SymbolWriter& symbolWriter) const override;

    void Interpolate(IRContext& context, Operation& outerOp, const AccessedRegister valueReg,
                     const size_t alignment) const;

    const std::vector<EntryType> _constants;

    const std::string _name;

    const Location _location;
};

class ExternClassTemplateType : public Type
{
  public:
    ExternClassTemplateType(const std::string& name) : _unqualifiedName(name) {}

    bool IsPod() const override { return false; }

    size_t GetBitWidth() const override { return 0; }

    std::string GetName() const override { return _unqualifiedName; }

    void AddClassType(ClassType* const classType);

    void SetExternal(const std::optional<std::string>& nameAttr);

  private:
    // Does not include module name
    const std::string _unqualifiedName;

    // Each class in this list is an instance of the external class
    std::set<const ClassType*> _classes;
};

class ClassType : public Type
{
  public:
    typedef std::pair<std::string, const DeclareNode*> EntryType;

    ClassType(const std::string& className, const std::vector<EntryType>& memberVariables,
              const std::vector<std::string>& memberFunctions, const ClassNode* const classNode)
        : _className(className), _memberVariables(memberVariables), _memberFunctions(memberFunctions),
          _classNode(classNode), _exported(false), _external(false), _instanceCount(0), _externClassTemplate(nullptr)
    {
    }

    bool IsPod() const override
    {
        // classes cannot be local variables, array elements, or struct members
        return false;
    }

    size_t GetBitWidth() const override { return 0; }

    const std::vector<std::string>& GetObjectNames() const { return _objectNames; }

    std::string GetName() const override { return _className; }

    const std::vector<EntryType>& GetMemberVariables() const { return _memberVariables; }

    const std::vector<std::string>& GetMemberFunctions() const { return _memberFunctions; }

    void RegisterObjects(const RegisterObjectsMode mode, const std::string& objectName, const ObjectPath& objectPath,
                         const ObjectCallback& objectCallback,
                         const DefaultInitializeCallback& defaultInitializeCallback,
                         ObjectToClassCallableFunctionMap& objectToFunctions) const override;

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType registerType, const std::string& name,
                                         const ObjectPath& containerPath,
                                         const SourceVariable& source) const override;

    void EnumerateContainedObjectNames(std::vector<std::string>& objectNames) const override;

    void VisitTypes(const TypeVisitFunction& callback, const VisitTypesBehavior behavior) const override;

    bool ContainsMemberRecursive(const std::string& memberName,
                                 const std::string& parentObjectName = "") const override;

    const AllocatedRegister* GetRegistersForMember(const std::string& objectName, const std::string& memberName,
                                                   const Location& location) const;

    const AllocatedRegister* GetRegistersForObject(const std::string& objectName) const;

    void Reset() override;

    void SetExported(const std::optional<std::string>& nameOverride) const;

    bool IsExport() const;

    bool IsExportIgnoreTarget() const;

    const std::string& GetExportedName() const;

    const std::string GetExternalName() const;

    void SetExternal(const std::optional<std::string>& nameAttr);

    bool IsExternal() const;

    const Type* TryGetMemberType(const Scope& scope, const std::string& name) const;

    const FunctionType* GetCallbackType(const Scope& scope, const std::string& name) const;

    bool ContainsMethod(const std::string& name) const;

    const ClassNode* GetClassNode() const;

    void SetTemplate(const ExternClassTemplateType* const templ);

    bool IsTemplate() const;

    bool HasExternNameAttribute() const;

    void IsInstantiated() { _instanceCount++; }

    const size_t InstanceCount() const { return _instanceCount; }

  private:
    ClassType& operator=(const ClassType& rhs);

    const std::string& _className;

    mutable std::vector<std::string> _objectNames;

    const std::vector<EntryType> _memberVariables;

    const std::vector<std::string> _memberFunctions;

    mutable std::map<std::string, AllocatedRegister*> _perObjectRegisters;

    mutable bool _exported;

    mutable std::string _exportedName;

    mutable bool _external;

    const ExternClassTemplateType* _externClassTemplate;

    size_t _instanceCount;

    const ClassNode* const _classNode;

    std::optional<std::string> _externName;
};

class ReferenceType : public Type
{
  public:
    ReferenceType(const Type* const referencedType) : _referencedType(referencedType) {}

    size_t GetBitWidth() const override;

    std::string GetName() const override;

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType registerType, const std::string& name,
                                         const ObjectPath& containerPath,
                                         const SourceVariable& source = {}) const override;

    size_t GetRegisterCount() const override { return 0; }

    void ExportVerilog(SourceWriter& writer) const override { assert(false); }

    void ExportVerilogTypedef(SourceWriter& writer, const std::string& name) const override { assert(false); }

    void VisitTypes(const TypeVisitFunction& callback, const VisitTypesBehavior behavior) const override
    {
        if (behavior != VisitTypesBehavior::SkipReferences)
        {
            _referencedType->VisitTypes(callback, behavior);

            Type::VisitTypes(callback, behavior);
        }
    }

    bool IsPod() const override
    {
        // references can be local variables, parameters, etc
        return true;
    }

    bool ShouldExport() const override
    {
        // References are compiler-internal, and should not be exported
        return false;
    }

    const Type* ResolveDeferredType(const Location& location) const override;

    std::map<std::string, std::string>
    GetReferences(const std::string& srcName, const std::string& dstName,
                  const ResolveReferenceFunction& resolveReferenceCallback) const override;

    const Type* const _referencedType;
};

enum class AddSymbolBehavior
{
    Default,
    AllowDuplicates
};

template <typename GlobalType, typename PerTypeType> class TypeContext
{
  public:
    using LeaveScopeCallback = std::function<void()>;

    // hideAbove is used to make some symbol table entries invisible
    // If hideAbove = true, then all symbol table entries in a higher scope than this
    // that have hideWhenRequested = true, will not be returned by calls to Lookup* or Contains*
    void PushScope(const std::string* const namespaceName, const bool hideAbove = false)
    {
        TableType newTable = {};

        newTable._isNamespaceScope = namespaceName != nullptr;
        newTable._namespaceName = namespaceName ? *namespaceName : std::string("");
        newTable._hideAbove = hideAbove;

        _scopes.push_back(newTable);

        if (namespaceName)
        {
            _namespaceScope.push_back(*namespaceName);
        }
    }

    void PopScope()
    {
        assert(!_scopes.empty());

        TableType& scopeToPop = _scopes.back();

        if (scopeToPop._isNamespaceScope)
        {
            _namespaceScope.pop_back();
        }

        for (const LeaveScopeCallback& cb : scopeToPop._leaveScopeCallbacks)
        {
            cb();
        }

        _scopes.pop_back();
    }

    PerTypeType LookupSymbol(const Scope& namespaceScopeIn, const std::string& name, const Location& location) const
    {
        const EntryType* const entry = LookupInternal(namespaceScopeIn, name, location);

        return entry->_data;
    }

    bool ContainsSymbol(const Scope& namespaceScopeIn, const std::string& name, const Location& location) const
    {
        const EntryType* const entry = LookupNoException(namespaceScopeIn, name, location);

        return entry != nullptr;
    }

    // Namespace name is not specified, it is implied by contents of _namespaceScope
    void AddSymbol(const std::string& name, const PerTypeType& value, const Location& location,
                   const AddSymbolBehavior behavior = AddSymbolBehavior::Default, const bool hideWhenRequested = false)
    {
        TableType& table = FindNonNamespaceScope();

        // If the inner most scope is a namespace
        // then store the namespace with the entry so that lookups
        // outside the namespace can reference objects inside the namespace
        const Scope symbolScope = InnerMostScopeIsNamespace() ? _namespaceScope : Scope();

        const std::string combinedName = FlattenScopeAndAppendName(symbolScope, name);

        // Lookup all symbols in table that match the specified name
        if (behavior != AddSymbolBehavior::AllowDuplicates)
        {
            const auto it = table._map.find(combinedName);

            if (it != table._map.end())
            {
                GetCompilerErrorStream(location, CompileError::DuplicateIdentifier)
                    << "Duplicate symbol in the same scope: " << combinedName;
            }
        }

        const EntryType newEntry = {value, symbolScope, hideWhenRequested};

        table._map[combinedName] = newEntry;
    }

    void UpdateSymbol(const Scope& namespaceScopeIn, const std::string& name, const PerTypeType& newValue,
                      const Location& location)
    {
        EntryType* const entry = LookupInternal(namespaceScopeIn, name, location);

        entry->_data = newValue;
    }

    class PushPopScope
    {
      public:
        PushPopScope(TypeContext& context, const std::string* const namespaceName = nullptr,
                     const bool hideAbove = false)
            : _context(context)
        {
            _context.PushScope(namespaceName, hideAbove);
        }

        ~PushPopScope() { _context.PopScope(); }

      private:
        PushPopScope& operator=(const PushPopScope&);

        TypeContext& _context;
    };

    Scope GetNamespaceScope() const { return _namespaceScope; }

    void Enumerate(const std::function<void(const std::string&, const PerTypeType&)>& callback) const
    {
        for (auto it = _scopes.rbegin(); it != _scopes.rend(); ++it)
        {
            const TableType& table = *it;

            for (const auto& p : table._map)
            {
                callback(p.first, p.second._data);
            }
        }
    }

    // Registers a function to be called when the current scope is popped
    void OnLeaveCurrentScope(const LeaveScopeCallback& cb)
    {
        assert(!_scopes.empty());
        _scopes.back()._leaveScopeCallbacks.push_back(cb);
    }

    GlobalType _global;

  private:
    struct EntryType
    {
        PerTypeType _data;
        Scope _namespaceScope;
        bool _hideWhenRequested;
    };

    // A single name may map to multiple entries (each with different scopes)
    typedef std::unordered_map<std::string, EntryType> MapType;

    struct TableType
    {
        MapType _map;

        bool _isNamespaceScope;
        std::string _namespaceName;
        bool _hideAbove;

        std::list<LeaveScopeCallback> _leaveScopeCallbacks;
    };

    // Iterates through scope list (back to front), looking for the first non-namespace one
    TableType& FindNonNamespaceScope()
    {
        for (auto it = _scopes.rbegin(); it != _scopes.rend(); ++it)
        {
            TableType& table = *it;

            if (!table._isNamespaceScope)
            {
                return table;
            }
        }

        throw std::runtime_error("Scoping error");
    }

    bool InnerMostScopeIsNamespace() const
    {
        assert(!_scopes.empty());
        return _scopes.back()._isNamespaceScope;
    }

    EntryType* LookupNoException(const Scope& explicitScope, const std::string& name, const Location& location) const
    {
        Scope currImplicitScope;

        bool shouldHideVariables = false;

        for (auto it = _scopes.rbegin(); it != _scopes.rend(); ++it)
        {
            const TableType& table = *it;

            // Get the list of all possible scopes for this variable
            const std::list<Scope> scopeSearchList = GetScopeSearchList(currImplicitScope, explicitScope);

            for (const Scope& scope : scopeSearchList)
            {
                const std::string combinedName = FlattenScopeAndAppendName(scope, name);

                const auto tableIt = table._map.find(combinedName);

                if (tableIt != table._map.end())
                {
                    const EntryType& entry = tableIt->second;

                    if (!shouldHideVariables || !entry._hideWhenRequested)
                    {
                        return const_cast<EntryType*>(&entry);
                    }
                }
            }

            // When traversing out of a namespace
            // append the namespace name to currImplicitScope
            // so that objects in the namespace can be referenced without explictly naming the namespace
            if (table._isNamespaceScope)
            {
                currImplicitScope.push_front(table._namespaceName);
            }

            // For inline calls, when traversing out of the inline call
            // hide all local variables from callees
            if (table._hideAbove)
            {
                shouldHideVariables = true;
            }
        }

        return nullptr;
    }

    EntryType* LookupInternal(const Scope& explicitScope, const std::string& name, const Location& location) const
    {
        EntryType* const result = LookupNoException(explicitScope, name, location);

        if (result)
        {
            return result;
        }
        else
        {
            GetCompilerErrorStream(location, CompileError::UnknownIdentifier) << "Unknown symbol: " << name;

            throw std::runtime_error("Unknown symbol");
        }
    }

    std::list<TableType> _scopes;

    Scope _namespaceScope;
};

// To enable variables to be referenced before declaration
// and functions to be called before definition
// Type checking is divided into 3 passes.
enum class TypeCheckPass
{
    // Functions
    Functions,

    // Global (non-static) variables
    Globals,

    // Everything else
    Default
};

struct TypeCheckGlobalData
{
    TypeCheckGlobalData()
        : _functionReturnType(nullptr), _functionCount(0), _returnCount(0),
          _memberProtectionModifier(ParseTreeMemberProtectionModifierPublic), _pass(TypeCheckPass::Default),
          _currentFunction(nullptr)
    {
    }

    const Type* _functionReturnType;

    size_t _functionCount;

    size_t _returnCount;

    std::stack<const ClassType*> _classStack;

    ParseTreeMemberProtectionModifier _memberProtectionModifier;

    TypeCheckPass _pass;

    std::stack<std::string> _externClassInstanceNameStack;

    std::stack<const Type*> _expectedTypeStack;

    const FunctionNode* _currentFunction;
};

// Record stored in symbol table during type checking
struct TypeCheckData
{
    const Type* _type;
    KnownValue _value;
};

typedef TypeContext<TypeCheckGlobalData, TypeCheckData> TypeCheckContext;

// RAII class for TypeCheckContext::_global::_externClassInstanceNameStack
class PushPopExternalClassInstance
{
  public:
    PushPopExternalClassInstance(TypeCheckContext& typeContext, const std::string& name) : _typeContext(typeContext)
    {
        // Nesting of extern modules is not allowed
        assert(_typeContext._global._externClassInstanceNameStack.empty());

        _typeContext._global._externClassInstanceNameStack.push(name);
    }

    ~PushPopExternalClassInstance()
    {
        _typeContext._global._externClassInstanceNameStack.pop();

        assert(_typeContext._global._externClassInstanceNameStack.empty());
    }

  private:
    PushPopExternalClassInstance& operator=(const PushPopExternalClassInstance&);

    TypeCheckContext& _typeContext;
};

// RAII class for TypeCheckContext::_global::_expectedTypeStack
class PushPopExpectedType
{
  public:
    PushPopExpectedType(TypeCheckContext& typeContext, const Type* const type) : _typeContext(typeContext)
    {
        _typeContext._global._expectedTypeStack.push(type);
    }

    ~PushPopExpectedType() { _typeContext._global._expectedTypeStack.pop(); }

  private:
    PushPopExpectedType& operator=(const PushPopExpectedType&);

    TypeCheckContext& _typeContext;
};

// RAII class for TypeCheckContext::_global::_classStack
class PushPopClassStack
{
  public:
    PushPopClassStack(TypeCheckContext& typeContext, const ClassType* const classType) : _typeContext(typeContext)
    {
        assert(classType);

        _typeContext._global._classStack.push(classType);
    }

    ~PushPopClassStack() { _typeContext._global._classStack.pop(); }

  private:
    PushPopClassStack& operator=(const PushPopClassStack&);

    TypeCheckContext& _typeContext;
};

// RAII class for TypeCheckContext::_global::_memberProtectionModifier
class PushPopMemberProtectionModifier
{
  public:
    PushPopMemberProtectionModifier(TypeCheckContext& typeContext) : _typeContext(typeContext)
    {
        _saved = _typeContext._global._memberProtectionModifier;
    }

    ~PushPopMemberProtectionModifier() { _typeContext._global._memberProtectionModifier = _saved; }

  private:
    PushPopMemberProtectionModifier& operator=(const PushPopMemberProtectionModifier&);

    TypeCheckContext& _typeContext;

    ParseTreeMemberProtectionModifier _saved;
};

// RAII class for TypeCheckContext::_global::_currentFunction
class PushPopCurrentFunction
{
  public:
    PushPopCurrentFunction(TypeCheckContext& typeContext, const FunctionNode* const function)
        : _typeContext(typeContext)
    {
        // Nested functions are not allowed
        assert(typeContext._global._currentFunction == nullptr);
        typeContext._global._currentFunction = function;
    }

    ~PushPopCurrentFunction() { _typeContext._global._currentFunction = nullptr; }

  private:
    PushPopCurrentFunction& operator=(const PushPopCurrentFunction&);

    TypeCheckContext& _typeContext;
};

struct FunctionDesc
{
    const Type* _returnType;

    std::vector<const Type*> _parameterTypes;

    // When a function type is converted to a function desc
    // parameter names are not supplied (this vector is empty)
    std::vector<std::string> _parameterNames;

    // For fixed-latency functions - the number of cycles of latency
    boost::optional<size_t> _fixedLatency;

    // Index of parameter with [[last]] attribute
    boost::optional<size_t> _isLastParameterIndex;

    ParseTreeFunctionModifier _modifiers;

    bool _hasVariableArguments;

    ParseTreeMemberProtectionModifier _protectionModifier;

    std::string GetParameterList() const;
};

class FunctionType : public Type
{
  public:
    FunctionType(const std::vector<const Type*>& paramTypes, const std::vector<std::string>& paramNames,
                 const Type* const returnType, const ParseTreeFunctionModifier modifiers,
                 const boost::optional<size_t> latency, const boost::optional<size_t> isLastParameterIndex)
        : _paramTypes(paramTypes), _paramNames(paramNames), _returnType(returnType), _modifiers(modifiers),
          _latency(latency), _isLastParameterIndex(isLastParameterIndex)
    {
        assert(paramTypes.size() == paramNames.size());
    }

    size_t GetBitWidth() const override { return 0; }

    std::string GetName() const override
    {
        std::ostringstream str;

        str << "(";

        for (size_t i = 0; i < _paramTypes.size(); i++)
        {
            if (i > 0)
            {
                str << ", ";
            }

            str << _paramTypes[i]->GetName();
        }

        str << ") -> ";

        str << _returnType->GetName();

        return str.str();
    }

    FunctionDesc ToFunctionDesc() const;

    AllocatedRegister* AllocateRegisters(Program& program, const RegisterType type, const std::string& name,
                                         const ObjectPath& containerPath,
                                         const SourceVariable& source) const override;

    const Type* ResolveDeferredType(const Location& location) const override;

    void ExportVerilog(SourceWriter& writer) const override {}

    size_t GetParamCount() const;

    const Type* GetParamType(const size_t index) const;

    const std::string& GetParamName(const size_t index) const;

    const Type* GetReturnType() const;

    ParseTreeFunctionModifier GetModifiers() const { return _modifiers; }

    boost::optional<size_t> GetLatency() const { return _latency; }

    bool HasIsLastParameter() const { return _isLastParameterIndex.is_initialized(); }

    size_t GetIsLastParameterIndex() const { return *_isLastParameterIndex; }

  private:
    std::vector<const Type*> _paramTypes;
    std::vector<std::string> _paramNames;
    const Type* _returnType;
    ParseTreeFunctionModifier _modifiers;
    boost::optional<size_t> _latency;
    boost::optional<size_t> _isLastParameterIndex;
};

// Interface used to get known values at type-checking and IR-generation time
class KnownValueContext
{
  public:
    virtual ~KnownValueContext(){};

    virtual void SetSymbolKnownValue(const Scope& scope, const std::string& name, const KnownValue& knownValue,
                                     const Location& location) = 0;

    virtual KnownValue LookupKnownValueForSymbol(const Scope& scope, const std::string& name,
                                                 const Location& location) = 0;
};

class TypeCheckKnownValueContext : public KnownValueContext
{
  public:
    TypeCheckKnownValueContext(TypeCheckContext& context);

    void SetSymbolKnownValue(const Scope& scope, const std::string& name, const KnownValue& knownValue,
                             const Location& location) override;

    KnownValue LookupKnownValueForSymbol(const Scope& scope, const std::string& name,
                                         const Location& location) override;

  private:
    TypeCheckContext& _context;
};

// Context passed to visit functions
struct VisitContextGlobalData
{
    // True if the traversal should keep track of known variable values
    bool _trackKnownValues;

    VisitContextGlobalData() : _trackKnownValues(false) {}
};

struct VisitContextPerVariableData
{
    KnownValue _knownValue;
    const DeclareNode* _declaration;
};

typedef TypeContext<VisitContextGlobalData, VisitContextPerVariableData> VisitContext;

class VisitCheckKnownValueContext : public KnownValueContext
{
  public:
    VisitCheckKnownValueContext(VisitContext& context);

    void SetSymbolKnownValue(const Scope& scope, const std::string& name, const KnownValue& knownValue,
                             const Location& location) override;

    KnownValue LookupKnownValueForSymbol(const Scope& scope, const std::string& name,
                                         const Location& location) override;

  private:
    VisitContext& _context;
};

// Does not store any known values
class NopCheckKnownValueContext : public KnownValueContext
{
  public:
    void SetSymbolKnownValue(const Scope& scope, const std::string& name, const KnownValue& knownValue,
                             const Location& location) override;

    KnownValue LookupKnownValueForSymbol(const Scope& scope, const std::string& name,
                                         const Location& location) override;
};

// When ParseTreeNode::Visit calls a VisitFunction, it passes another callback
// which should be called to continue the traversal down the tree
typedef std::function<void(void)> VisitChildrenFunction;
typedef std::function<void(const ParseTreeNode* const, const VisitChildrenFunction&)> VisitFunction;

std::string PrettyToIdentifier(const std::string& str);

KnownValue TryGetVisitKnownValue(VisitContext& context, const ParseTreeNode* const expression,
                                 const Type* const resultType);

enum class SetTypeBehavior
{
    Default,
    DisallowFrontEndOverride
};

struct _ParseTreeNode
{
    _ParseTreeNode(const Type* const type = nullptr);

    virtual ~_ParseTreeNode() {}

    virtual void TypeCheck(TypeCheckContext& context) const = 0;

    virtual void TypeCheckForAssignment(TypeCheckContext& context) const { TypeCheck(context); }

    const Type* GetType() const
    {
        if (_type)
        {
            return _type;
        }
        else
        {
            throw std::runtime_error("Type error");
        }
    }

    void SetType(const Type* const type, const SetTypeBehavior behavior = SetTypeBehavior::Default) const;

    virtual void SetFrontEndType(const Type* const type);

    void GenerateStatementIR(IRContext& context) const;

    const AllocatedRegister* GenerateIROptionalResult(IRContext& context) const;

    virtual void GenerateIR(IRContext& context) const;

    virtual void AllocateRegisters(Program& program) {}

    // The width of the resulting integer = resultType->GetBitWidth()
    KnownValue TryGetKnownValue(KnownValueContext& context, const Type* const resultType) const
    {
        // First check the type of this node
        const LeafType* const leafType = dynamic_cast<const LeafType*>(_type);

        const BoolType* const boolType = dynamic_cast<const BoolType*>(_type);

        const FloatType* const floatType = dynamic_cast<const FloatType*>(_type);

        const StringType* const stringType = dynamic_cast<const StringType*>(_type);

        if (boolType || leafType || stringType || (floatType && (floatType->GetBitWidth() == 32)))
        {
            // Call derived class
            const KnownValue childKnownValue = TryGetKnownValueImpl(context);

            if (KnownValueType::Int == childKnownValue._type)
            {
                // Handle widening or narrowing to resultType
                return KnownValue(TypeConvert(childKnownValue._intVal, GetType(), resultType));
            }
            else
            {
                return childKnownValue;
            }
        }
        else
        {
            return KnownValue();
        }
    }

    Location GetLocation() const { return _location; }

    // For a node that is contained within 1 file
    // Returns the range of line numbers that the node spans
    std::pair<Location, Location> GetLineBounds() const;

    virtual void Visit(const VisitFunction& visitFunction, VisitContext& context) const = 0;

    void ResolveDeferredTypes() const;

    // Returns a number that uniquely identifies this node
    // Useful for sorting nodes in a way that doesn't change
    // based on pointers returned from the heap
    size_t GetSequenceNumber() const { return _sequenceNumber; }

    virtual std::map<std::string, std::string> GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                             const std::string& resultBaseName,
                                                             const std::string& thisObjectName,
                                                             const ClassType* const thisClassType) const
    {
        throw std::runtime_error(std::string(typeid(*this).name()) + " doesn't implement GetReferences");
    }

    // Reset IR generation state
    virtual void Reset(){};

    virtual std::string PrettyPrint() const
    {
        assert(false);
        throw std::runtime_error("PrettyPrint not implemented");
    }

    std::string GetExpressionString() const { return PrettyToIdentifier(PrettyPrint()); }

    // Returns true if the node is a leaf condition for condition coverage
    virtual bool IsConditionCoverageLeaf() const
    {
        // By default, nodes are considered leaves except in specific cases
        // where we can trace deeper
        return true;
    }

  protected:
    _ParseTreeNode& operator=(const _ParseTreeNode&) const;

    // Dervied classes that will always be leaf nodes in the parse tree can use this
    void VisitNoChildren(const VisitFunction& visitFunction, VisitContext& context) const
    {
        const auto callback = []() {};

        visitFunction(this, callback);
    }

    // The width of the resulting integer is = GetType()->GetBitWidth()
    virtual KnownValue TryGetKnownValueImpl(KnownValueContext& context) const { return KnownValue(); }

    mutable const Type* _frontEndType;

    const Location _location;

    const size_t _sequenceNumber;

  private:
    // Private to ensure that SetType is used
    // to allow the front-end type to override the type computed during type checking
    mutable const Type* _type;
};

class QualifiedNameNode : public ParseTreeNode
{
  public:
    QualifiedNameNode(const Scope& scope, const std::string& name) : _scope(scope), _name(name) {}

    void TypeCheck(TypeCheckContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    const Scope& GetScope() const { return _scope; }

    const std::string& GetName() const { return _name; }

  private:
    Scope _scope;

    std::string _name;
};

class TemplateArgNode : public ParseTreeNode
{
  public:
    TemplateArgNode(const std::string& name, const ParseTreeNode* const arg) : _name(name), _arg(arg) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    TemplateArgument Serialize() const;

  private:
    std::string _name;

    const ParseTreeNode* const _arg;
};

class TemplateInstanceNode : public ParseTreeNode
{
  public:
    TemplateInstanceNode(const ParseTreeNode* const qualifiedName, const ParseTreeNode* const args)
        : _qualifiedName(qualifiedName), _args(args)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const QualifiedNameNode* const GetQualifiedName() const;

    std::list<TemplateArgument> GetTemplateArguments() const;

  private:
    const ParseTreeNode* const _qualifiedName;

    const ParseTreeNode* const _args;
};

class ScopedIdentifierNode : public ParseTreeNode
{
  public:
    ScopedIdentifierNode(const ParseTreeNode* const identifier) : _identifierNode(identifier) {}

    void TypeCheck(TypeCheckContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    // Returns just the identifier
    // a::b::c -> c
    std::string GetName() const;

    // Return the namespaces
    // a::b::c -> a::b
    Scope GetScope() const;

    std::string GetFlatenedName() const;

    void Rename(const std::string& newName);

    std::string PrettyPrint() const override { return GetFlatenedName(); }

  private:
    const ParseTreeNode* const _identifierNode;

    ScopedIdentifierNode& operator=(const ScopedIdentifierNode&);
};

class IntegerNode : public ParseTreeNode
{
  public:
    IntegerNode(const std::string& valueString);

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void SetFrontEndType(const Type* const type) override;

    const mp_int& GetUntypedValue() const;

    const mp_int& GetTypedValue() const;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    std::string PrettyPrint() const override { return std::to_string(static_cast<int64_t>(GetTypedValue())); }

    const std::string& GetValueString() const;

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override { return KnownValue(GetTypedValue()); }

  private:
    IntegerNode& operator=(const IntegerNode&);
    const std::string _valueString;
    mp_int _untypedValue;
    mutable std::optional<mp_int> _typedValue;
};

class FloatNode : public ParseTreeNode
{
  public:
    FloatNode(const float value) : _value(value) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    float GetValue() const { return _value; }

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    std::string PrettyPrint() const override { return std::to_string((float)_value); }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override;

  private:
    FloatNode& operator=(const FloatNode&);
    const float _value;
};

class BoolNode : public ParseTreeNode
{
  public:
    BoolNode(const bool value) : _value(value) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    bool GetValue() const { return _value; }

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    std::string PrettyPrint() const override { return _value ? "true" : "false"; }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override { return KnownValue(_value ? 1 : 0); }

  private:
    BoolNode& operator=(const BoolNode&);
    const bool _value;
};

class IdentifierNode : public ParseTreeNode
{
  public:
    IdentifierNode(const std::string& str) : _value(str) {}

    void TypeCheck(TypeCheckContext& context) const override;

    const std::string& GetValue() const;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    void Rename(const std::string& newName);

    std::string PrettyPrint() const override { return _value; }

  private:
    IdentifierNode& operator=(const IdentifierNode&);
    std::string _value;
};

class LiteralStringNode : public ParseTreeNode
{
  public:
    LiteralStringNode(const std::string& str) : _value(str) {}

    void TypeCheck(TypeCheckContext& context) const override;

    const std::string& GetValue() const;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    std::string PrettyPrint() const override { return _value; }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override;

  private:
    LiteralStringNode& operator=(const LiteralStringNode&);
    const std::string _value;
};

class ConstNode : public ParseTreeNode
{
  public:
    ConstNode(ParseTreeNodePtr type) : _ParseTreeNode(type->GetType()), _underlyingTypeNode(type) {}

    void TypeCheck(TypeCheckContext& context) const override { assert(false); }

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        const auto callback = [&]() { _underlyingTypeNode->Visit(visitFunction, context); };

        visitFunction(this, callback);
    }

    ParseTreeNodePtr GetTypeNode() const { return _underlyingTypeNode; }

  private:
    ParseTreeNodePtr _underlyingTypeNode;
};

class ThisNode : public ParseTreeNode
{
  public:
    ThisNode(const Scope& scope) : _scope(scope) {}

    void GenerateIR(IRContext& context) const override;

    void TypeCheck(TypeCheckContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    std::map<std::string, std::string> GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                     const std::string& resultBaseName,
                                                     const std::string& thisObjectName,
                                                     const ClassType* const thisClassType) const override;

    std::string PrettyPrint() const override { return "this"; }

  private:
    Scope _scope;
};

class DeferredType : public Type
{
  public:
    DeferredType(const Scope& implicitScope, const Scope& explicitScope, const std::string& name)
        : _implicitScope(implicitScope), _explicitScope(explicitScope), _name(name)
    {
    }

    const Type* ResolveDeferredType(const Location& location) const override { return GetNamedType(location); }

    size_t GetBitWidth() const override { return 0; }

    std::string GetName() const override { return _name; }

    bool IsPod() const override { return false; }

    const Type* GetNamedType(const Location& location) const;

  private:
    Scope _implicitScope;
    Scope _explicitScope;
    std::string _name;
};

class TypeNode : public ParseTreeNode
{
  public:
    TypeNode(const Type* const type);
    TypeNode(const Scope scope, const std::string& name, const Type* const type);

    void TypeCheck(TypeCheckContext& context) const override;

    void ResolveDeferredTypes() const;

    std::optional<std::string> GetTypedefName() const;

    void ResolveUnresolvedTypes(TypeCheckContext& context) const;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    std::string PrettyPrint() const override { return GetType()->GetName(); }

  private:
    TypeNode& operator=(const TypeNode&);
    Scope _scope;
    std::string _name;
};

class CallNode;
struct BasicBlock;
struct Function;

class FunctionNode : public ParseTreeNode
{
  public:
    FunctionNode(const ParseTreeNode* modifier, const ParseTreeNode* const returnType, const ParseTreeNode* const name,
                 const ParseTreeNode* const params, const ParseTreeNode* const statements,
                 const std::string& unmangledName);

    ~FunctionNode();

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void GenerateInlineIR(IRContext& context, const std::string& objectName,
                          const std::vector<KnownValue>& parameterKnownValues) const;

    const std::string& GetName() const;

    const std::string& GetUnmangledName() const;

    const std::string GetFlattenedName() const;

    const Scope GetContainingScope() const;

    ParseTreeFunctionModifier GetModifiers() const;

    const Type* GetReturnType() const;

    const ParseTreeNode* GetReturnTypeNode() const { return _returnType; }

    void AllocateRegisters(Program& program) override;

    const ClassType* GetClassType() const;

    void SetClassType(const ClassType* const classType);

    size_t GetParameterCount() const;

    const std::string& GetParameterName(const size_t index) const;

    size_t GetParameterWidth(const size_t index) const;

    const DeclareNode* GetParameterDeclareNode(const size_t index) const;

    void HandleRemovedBasicBlocks(const std::set<const BasicBlock*>& removedBlocks) const;

    bool ContainsReferenceParameters() const;

    void SetReturnNode(const ParseTreeNode* returnNode);

    void SetStatements(const ParseTreeNode* statements);

    const Type* GetParameterType(const size_t index) const;

    ParseTreeMemberProtectionModifier GetProtectionModifier() const;

    // For member functions, there is a separate instance per object
    // For global functions, there is 1 instance
    class Instance
    {
        friend class FunctionNode;

      public:
        Instance()
            : _parent(nullptr), _callerCount(0), _totalCallerThreadCount(0), _function(nullptr),
              _calleeReturnRegisters(nullptr), _callSiteIndexRegister(nullptr), _threadCountOneRegister(nullptr),
              _invocationInstanceRegister(nullptr), _inlineAtomicIndex(std::numeric_limits<size_t>::max())
        {
        }

        void SetParent(const FunctionNode* parent, const std::string& objectName)
        {
            assert(!_parent);
            assert(_callerCount == 0);

            _parent = parent;
            _objectName = objectName;

            // Functions with the [[reset]] modifier always have 1 implicit caller
            if (parent->GetModifiers() & ParseTreeFunctionModifierReset)
            {
                _callerCount = 1;
            }
        }

        size_t AddCaller(const size_t maxCallerThreadCount);

        void SetReturnBlock(const size_t index, BasicBlock* const basicBlock);

        Function* GetFunction() const;

        const AllocatedLeafRegister* GetLoopCounterRegister() const;

        const AllocatedLeafRegister* GetThreadCountOneRegister() const;

        const AllocatedRegister* GetParameterRegisters(const size_t index) const;

        bool HasCallSiteIndex() const;

        const AllocatedLeafRegister* GetCallSiteIndexRegister() const;

        const AllocatedLeafRegister* GetInvocationInstanceRegister() const;

        const AllocatedRegister* GetCalleeReturnRegisters() const;

        BasicBlock* GetReturnBlock(const size_t index) const;

        void SetReturnValueRegister(IRContext& context) const;

        size_t GetCallSiteCount() const;

        size_t GetTotalCallerThreadCount() const;

        size_t GetInlineAtomicIndex() const;

        void HandleRemovedBasicBlocks(const std::set<const BasicBlock*>& removedBlocks) const;

      private:
        Instance& operator=(const Instance&);

        const FunctionNode* _parent;

        std::string _objectName;

        size_t _callerCount;

        size_t _totalCallerThreadCount;

        Function* _function;

        size_t _inlineAtomicIndex;

        mutable std::unordered_map<size_t, BasicBlock*> _returnBlocks;

        // This is the value returned from the function
        // In the common case, this is also the value used by the caller
        // For pipelined functions that return an array, this register is used by the context saver
        // to create the array
        const AllocatedRegister* _calleeReturnRegisters;

        const AllocatedLeafRegister* _callSiteIndexRegister;
        const AllocatedLeafRegister* _invocationInstanceRegister;
        const AllocatedLeafRegister* _threadCountOneRegister;
    };

    Instance& GetInstance(const std::string& objectName);

    const Instance& GetInstance(const std::string& objectName) const;

    bool HasInstance(const std::string& objectName) const;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    bool IsInline() const;

    bool IsAutoInline(const FunctionNode* caller, const CallNode* const callNode) const;

    bool NoBackpressure() const;

    bool IsFixedLatency() const;

    size_t GetLatency() const;

    void RemoveModifiers(const ParseTreeFunctionModifier modifiers);

    void AddModifiers(const ParseTreeFunctionModifier modifiers);

    void Reset() override;

    FunctionDesc GetFunctionDesc() const;

  private:
    void AddParameterSymbols(IRContext& context, const std::vector<KnownValue>* const parameterKnownValues) const;

    FunctionNode& operator=(const FunctionNode&);

    ParseTreeFunctionModifier _modifiers;
    size_t _threadRate;
    const ParseTreeNode* const _modifierList;
    const ParseTreeNode* const _returnType;
    const ParseTreeNode* const _name;
    const ParseTreeNode* const _params;
    const ParseTreeNode* _statements;
    const ParseTreeNode* _originalStatements;
    const ParseTreeNode* _returnNode;
    const boost::optional<size_t> _fixedLatency;
    const std::string _unmangledName;

    // Set during type checking
    mutable const ClassType* _classType;

    mutable std::string _flattenedName;

    mutable Scope _containingScope;

    // Maps object names to instances
    std::unordered_map<std::string, Instance> _instances;

    mutable ParseTreeMemberProtectionModifier _protectionModifier;
};

class AssignNode : public ParseTreeNode
{
  public:
    AssignNode(const ParseTreeNode* const lhs, const ParseTreeNode* const rhs, const bool isGlobalInit,
               const bool isInitialAssignment)
        : _lhs(lhs), _rhs(rhs), _isGlobalInit(isGlobalInit), _isInitialAssignment(isInitialAssignment)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    const ParseTreeNode* GetLhs() const { return _lhs; }

    const ParseTreeNode* GetRhs() const { return _rhs; }

    const Type* GetRhsType(TypeCheckContext& context) const;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    void TypeCheckImpl(TypeCheckContext& context) const;

    void HandleReferenceAssignment(const ResolveReferenceFunction& resolveReferenceCallback,
                                   const std::string& containingObjectName,
                                   const FunctionNode* const containingFunction,
                                   std::map<std::string, std::string>& referenceMap) const;

    bool IsInitialAssignment() const;

  private:
    AssignNode& operator=(const AssignNode&);

    const ParseTreeNode* const _lhs;
    const ParseTreeNode* const _rhs;

    // True if the assigning is definitely to a global variable during initialization
    // even when this is false, the assignment may be to a global (not during initialization)
    const bool _isGlobalInit;

    // True if this assignment is part of a variable declaration
    const bool _isInitialAssignment;
};

class DeclareNode : public ParseTreeNode
{
  public:
    // Defines the scope that a declaration appears in
    enum class DeclarationScope
    {
        // Declared in a function
        Local,

        // Declared in a class
        Member,

        // Declared outside of any function/class
        Global
    };

    // Defines which threads can access a variable
    enum class DeclarationAccess
    {
        // Private to a thread
        Private,

        // Shared by all thread
        Shared
    };

    DeclareNode(const ParseTreeNode* const type, ParseTreeNode* const name, const Scope& namespaceScope,
                const uint32_t flags, const AssignNode* assignNode, const ParseTreeNode* const attributeList);

    void TypeCheck(TypeCheckContext& context) const override;

    const Type* GetDeclaredType() const;

    const ParseTreeNode* GetDeclaredTypeNode() const { return _typeNode; }

    const std::string& GetDeclaredName() const;

    const std::string GetFlattenedName() const;

    const bool _isConst;

    const bool _isUninitConst;

    const bool _shouldInitialize;

    const bool _isParameter;

    const bool _isEndTransaction;

    bool IsStatic() const;

    DeclarationScope GetDeclarationScope() const;

    void GenerateIR(IRContext& context) const override;

    void GenerateIRImpl(IRContext& context, const KnownValue* inlineCallKnownValue) const;

    void AllocateRegisters(Program& program) override;

    const AllocatedRegister* GetRegisters(const std::string& objectName) const;

    void MarkAsMember(const ParseTreeMemberProtectionModifier protectionModifier);

    const AllocatedRegister* AllocateRegistersInternal(Program& program, const RegisterType registerType,
                                                       const std::string& objectName,
                                                       const std::string& instanceNamePrefix,
                                                       SourceContainer* container,
                                                       const ObjectPath& containerPath) const;

    void SetAllocatedRegisterForObject(const AllocatedRegister* allocatedRegister, const std::string& objectName);

    ParseTreeMemberProtectionModifier GetProtectionModifier() const;

    void RegisterObjects() const;

    void ResolveFunctionsAndRegisterObjects(const RegisterObjectsMode mode, const std::string& objectName,
                                            const ObjectPath& objectPath, const std::string& containingObjectName,
                                            const ObjectCallback& objectCallback) const;

    std::string GetCombinedName(const std::string& objectName) const;

    std::string GetCombinedParseNamespaceScopeName(const std::string& objectName) const;

    void SetInferredType(const Type* const type) const;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const ClassType* GetClassType() { return _classType; }

    void SetClassType(const ClassType* const);

    void SetContainingFunction(const FunctionNode* const);

    void SetAssignNode(const AssignNode* const assignRhs);

    const AssignNode* GetAssignNode() const;

    bool ContainsClassType() const;

    void Rename(const std::string& newName);

    void Reset() override;

    void SetKnownValue(KnownValueContext& knownValueContext, const KnownValue* const inlineCallKnownValue) const;

    void MarkModifiedParameter();

    const Scope& GetScope() const { return _scope; }

  private:
    DeclareNode& operator=(const DeclareNode&);

    mutable const ParseTreeNode* _typeNode;
    ParseTreeNode* const _name;

    const AssignNode* _assignNode;

    const ParseTreeNode* const _attributeList;

    // Maps object name to associated registers
    std::unordered_map<std::string, const AllocatedRegister*> _allocatedRegisterMap;

    // Set during type checking
    mutable const ClassType* _classType;

    // Non-null only for declarations inside of a function
    mutable const FunctionNode* _containingFunction;

    // Namespace this object is declared in
    mutable Scope _scope;

    DeclarationScope _declarationScope;

    DeclarationAccess _declarationAccess;

    ParseTreeMemberProtectionModifier _memberProtectionModifier;

    // Namespaces in scope at the time the node was parsed
    const Scope _parseNamespaceScope;

    mutable size_t _staticInstanceId;

    bool _isModifiedParameter;
};

class FunctionTypeParamNode : public ParseTreeNode
{
  public:
    FunctionTypeParamNode(const ParseTreeNode* attr, const ParseTreeNode* type, const ParseTreeNode* name)
        : _ParseTreeNode(type->GetType()), _name(name), _attr(attr)
    {
    }

    virtual void TypeCheck(TypeCheckContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    bool HasName() const { return _name != nullptr; }

    const std::string& GetName() const { return dynamic_cast<const IdentifierNode*>(_name)->GetValue(); }

    bool HasLastAttribute() const;

  private:
    const ParseTreeNode* const _name;
    const ParseTreeNode* const _attr;
};

class NodeList : public ParseTreeNode
{
  public:
    NodeList() {}

    void TypeCheck(TypeCheckContext& context) const override;

    void Append(const ParseTreeNode* const node);

    const std::vector<const ParseTreeNode*>& Children() const { return _nodes; }

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    void RemoveNodes(const std::set<const ParseTreeNode*>& nodesToRemove) const;

    std::string PrettyPrint() const override;

  private:
    NodeList& operator=(const NodeList&);

    mutable std::vector<const ParseTreeNode*> _nodes;
};

template <typename T> class ModifierNode : public ParseTreeNode
{
  public:
    ModifierNode(const T modifier) : _modifier(modifier) {}

    void TypeCheck(TypeCheckContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    const T _modifier;

  private:
    ModifierNode& operator=(const ModifierNode&);
};

using FunctionModifierNode = ModifierNode<ParseTreeFunctionModifier>;

class LoopNode : public ParseTreeNode
{
  public:
    LoopNode(const ParseTreeNode* const modifiers, const ParseTreeNode* const body)
        : _mode(GetMode(modifiers)), _body(body), _modifiers(modifiers)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

  protected:
    LoopNode& operator=(const LoopNode&);

    virtual void GenerateBeforeLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const = 0;
    virtual void GenerateLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const = 0;
    virtual bool CanReorderThreads(IRContext& context) const = 0;

    virtual void GenerateWaitFor(IRContext& context) const { assert(false); }

    const ParseTreeLoopMode _mode;

    void VisitImpl(const VisitFunction& visitFunction, VisitContext& context) const;

    const ParseTreeNode* const _body;
    const ParseTreeNode* const _modifiers;

    size_t GetFifoDepthParam(IRContext& context) const;

  private:
    ParseTreeLoopMode GetMode(const ParseTreeNode* const modifiers);
};

// do/while loops
class DoWhileLoopNode : public LoopNode
{
  public:
    DoWhileLoopNode(const ParseTreeNode* const modifiers, const ParseTreeNode* const conditionStatement,
                    const ParseTreeNode* const body)
        : LoopNode(modifiers, body), _conditionStatement(conditionStatement)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

  protected:
    void GenerateBeforeLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const override;
    void GenerateLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const override;
    bool CanReorderThreads(IRContext& context) const override;

    bool MergeWithCurrentBasicBlock(IRContext& context) const;
    void GenerateWaitFor(IRContext& context) const override;

  private:
    const ParseTreeNode* _conditionStatement;
};

class RangeForLoopNode : public LoopNode
{
  public:
    RangeForLoopNode(const ParseTreeNode* const modifiers, const ParseTreeNode* const declareNode,
                     const ParseTreeNode* const bound, const ParseTreeNode* const body)
        : LoopNode(modifiers, body), _declareNode(dynamic_cast<const DeclareNode*>(declareNode)), _bound(bound),
          _boundMinusOneRegister(nullptr)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Reset() override;

  protected:
    boost::optional<size_t> GetBound(IRContext& context) const;

    void GenerateBeforeLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const override;
    void GenerateLoop(IRContext& context, const AllocatedLeafRegister* loopConditionRegister) const override;
    bool CanReorderThreads(IRContext& context) const override;

  private:
    const DeclareNode* _declareNode;
    const ParseTreeNode* _bound;

    mutable const AllocatedLeafRegister* _boundMinusOneRegister;
};

class UnrolledForLoopNode : public ParseTreeNode
{
  public:
    UnrolledForLoopNode(const ParseTreeNode* const declareNode, const ParseTreeNode* const bound,
                        const ParseTreeNode* const body)
        : _declareNode(declareNode), _bound(bound), _body(body)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

  private:
    UnrolledForLoopNode& operator=(const UnrolledForLoopNode&);

    const ParseTreeNode* const _declareNode;
    const ParseTreeNode* const _bound;
    const ParseTreeNode* const _body;
};

class MuxOpNode : public ParseTreeNode
{
  public:
    MuxOpNode(const ParseTreeNode* const predicate, const ParseTreeNode* const expressionList)
        : _predicate(predicate), _expressionList(expressionList)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    std::string PrettyPrint() const override
    {
        return "mux(" + _predicate->PrettyPrint() + "," + _expressionList->PrettyPrint() + ")";
    }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override;

  private:
    MuxOpNode& operator=(const MuxOpNode&);

    const ParseTreeNode* const _predicate;
    const ParseTreeNode* const _expressionList;
};

class BinaryOpNode : public ParseTreeNode
{
  public:
    BinaryOpNode(const ParseTreeBinaryOpType type, const ParseTreeNode* const lhs, const ParseTreeNode* const rhs)
        : _opType(type), _lhs(lhs), _rhs(rhs)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    void ConditionCoverageVisit(IRContext& context, const Location& condloc, const AccessedRegister& originalCondition,
                                bool withinXor) const;

    bool IsFloatOperation() const;

    bool IsLogicalOperation() const;

    bool IsBitOperation() const;

    std::pair<const FunctionNode*, size_t> GetCalledInlineFunction(KnownValueContext& knownValueContext) const;

    std::string PrettyPrint() const override;

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override;

  private:
    BinaryOpNode& operator=(const BinaryOpNode&);

    const ParseTreeBinaryOpType _opType;
    const ParseTreeNode* const _lhs;
    const ParseTreeNode* const _rhs;
};

class UnaryOpNode : public ParseTreeNode
{
  public:
    UnaryOpNode(const ParseTreeUnaryOpType type, const ParseTreeNode* const expression)
        : _opType(type), _expression(expression)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    std::string PrettyPrint() const override;

    bool IsInvertOperation() const;

    // Invert operations are not considered leaf conditions for condition coverage
    bool IsConditionCoverageLeaf() const override { return !IsInvertOperation(); }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override;

  private:
    UnaryOpNode& operator=(const UnaryOpNode&);

    const ParseTreeUnaryOpType _opType;
    const ParseTreeNode* const _expression;
};

class SizeOfNode : public ParseTreeNode
{
  public:
    SizeOfNode(const ParseTreeSizeofType type, const ParseTreeNode* const expression)
        : _sizeofType(type), _expression(expression), _result(0)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    std::string PrettyPrint() const override { return "sizeof(" + _expression->PrettyPrint() + ")"; }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override { return KnownValue(_result); }

  private:
    size_t GetResult(TypeCheckContext& context) const;

    SizeOfNode& operator=(const SizeOfNode&);

    const ParseTreeSizeofType _sizeofType;
    const ParseTreeNode* const _expression;
    mutable size_t _result;
};

class ReturnNode : public ParseTreeNode
{
  public:
    ReturnNode(const ParseTreeNode* const value) : _value(value), _returnType(nullptr) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    const Type* GetReturnType() const;

    void GenerateConditionalReturns(Program& program) const;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    void Reset() override;

    std::string PrettyPrint() const override;

    std::map<std::string, std::string> GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                     const std::string& resultBaseName,
                                                     const std::string& thisObjectName,
                                                     const ClassType* const thisClassType) const override;

  private:
    mutable const Type* _returnType;

    ReturnNode& operator=(const ReturnNode&);

    // Enqueue operations are not emitted during GenerateIR
    // Because the call site count is not known
    // This structure stores information saved during GenerateIR
    // and is used to generated enqueue operations after the call site count is known
    struct DeferredReturn
    {
        BasicBlock* _basicBlock;
        std::string _objectName;
        bool _isSyncReturn;
        const AllocatedRegister* _returnValueRegisters;
        const AllocatedLeafRegister* _callSiteIndexRegister;
        const AllocatedLeafRegister* _invocationInstanceRegister;
    };

    mutable std::list<DeferredReturn> _deferredReturns;

    const ParseTreeNode* const _value;
};

class CastNode : public ParseTreeNode
{
  public:
    CastNode(const ParseTreeNode* const type, const ParseTreeNode* const value)
        : ParseTreeNode(type->GetType()), _castType(type), _value(value)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    std::map<std::string, std::string> GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                     const std::string& resultBaseName,
                                                     const std::string& thisObjectName,
                                                     const ClassType* const thisClassType) const override
    {
        return _value->GetReferences(resolveReferenceCallback, resultBaseName, thisObjectName, thisClassType);
    }

    std::string PrettyPrint() const override
    {
        return "cast<" + _castType->PrettyPrint() + ">(" + _value->PrettyPrint() + ")";
    }

    // A CastNode is not a condition coverage leaf if it casts from 1-bit to
    // 1-bit (typically bool->uint1 or uint1->bool cast)
    bool IsConditionCoverageLeaf() const override
    {
        return !(_value->GetType()->GetBitWidth() == 1 && _castType->GetType()->GetBitWidth() == 1);
    }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override;

  private:
    CastNode& operator=(const CastNode&);

    const ParseTreeNode* const _castType;
    const ParseTreeNode* const _value;
};

// RHS can be NULL if there is no else
class IfNode : public ParseTreeNode
{
  public:
    IfNode(const ParseTreeNode* const condition, const ParseTreeNode* const lhs, const ParseTreeNode* const rhs)
        : _condition(condition), _lhs(lhs), _rhs(rhs)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

  private:
    IfNode& operator=(const IfNode&);

    const ParseTreeNode* const _condition;
    const ParseTreeNode* const _lhs;
    const ParseTreeNode* const _rhs;
};

// Used to get the update rate for an atomic block
// If the atomic block does a read-modify-write to memories
// this can modify the update rate to avoid the need to enable bypass on those memories
class UpdateRateModifierNode : public ParseTreeNode
{
  public:
    struct ModifiedUpdateRate
    {
        size_t _updateRate;
        bool _needsBypass;
    };

    UpdateRateModifierNode(size_t updateRate) : _updateRate(updateRate) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    ModifiedUpdateRate GetModifiedUpdateRate(const bool hasMemoryRmw) const;

    size_t GetUnmodifiedUpdateRate() const;

  private:
    UpdateRateModifierNode& operator=(const UpdateRateModifierNode&);

    const size_t _updateRate;
};

class AtomicNode : public ParseTreeNode
{
  public:
    AtomicNode(const ParseTreeNode* const body, size_t updateRate) : _body(body), _updateRate(updateRate) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

  private:
    AtomicNode& operator=(const AtomicNode&);

    void HandleRmwMemory(IRContext& context) const;

    const ParseTreeNode* const _body;
    const UpdateRateModifierNode _updateRate;
};

class NestedScopeNode : public ParseTreeNode
{
  public:
    NestedScopeNode(const ParseTreeNode* const body) : _body(body) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const ParseTreeNode* GetBody() const;

  private:
    NestedScopeNode& operator=(const NestedScopeNode&);

    const ParseTreeNode* const _body;
};

struct CallModifiers
{
    // 1 means that each thread can enqueue
    // N means that at most 1 out of every N threads will enqueue
    size_t _callRate;

    // Max transaction size for call to function with [[last]]
    size_t _transactionSize;

    // The minimum depth of the associated FIFO - specified by the user.  This can be 0.
    size_t _minFifoDepth;
};

class IntAttributeNode;

enum class ExternalModuleCallType
{
    // Flat function only called in 1 basic block
    InstantiateInBasicBlock,

    // Hooked up to a top-level port
    ExternallyInstantiated,

    // Fixed-latency function of an extern class
    ExternClassMethod
};

struct ExternalClassInstanceName
{
    // Name of the class
    std::string _className;

    // Name of the class instance
    std::string _objectName;
};

class CallNode : public ParseTreeNode
{
  public:
    CallNode(const ParseTreeNode* const object, const ParseTreeNode* const name, const ParseTreeNode* const arguments,
             const ParseTreeNode* const modifiers)
        : _object(object), _scopedName(dynamic_cast<const ScopedIdentifierNode*>(name)), _args(arguments),
          _modifiers(modifiers), _containingFunction(nullptr), _calledClassType(nullptr), _callSiteClassType(nullptr)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    std::map<std::string, std::string> GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                     const std::string& resultBaseName,
                                                     const std::string& thisObjectName,
                                                     const ClassType* const thisClassType) const override;

    std::map<std::string, std::string>
    TranslateParameterReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                 const FunctionNode* const callee, const std::string& thisObjectName) const;

    std::string GetName() const;

    const ScopedIdentifierNode* GetScopedName() const;

    const std::string GetFlattenedName() const;

    const ClassType* GetCalledClassType() const;

    const ClassType* GetCallSiteClassType() const;

    const Scope& GetCallSiteScope() const;

    const ParseTreeNode* GetObject() const;

    bool HasAttribute(const ParseTreeAttribute attribute) const;

    const std::vector<const ParseTreeNode*>& GetArguments() const;

    class Instance
    {
      public:
        Instance();

        void SetCallSiteFunctionInstance(const FunctionInstance& callSiteFunctionInstance);

        const FunctionInstance& GetCallSiteFunctionInstance();

        void SetCalledObjectName(const std::string& name);

        const std::string& GetCalledObjectName() const;

        void SetReturnBlock(BasicBlock* const);

        BasicBlock* GetReturnBlock() const;

        size_t GetCallSiteIndex() const;

        void SetCallSiteIndex(const size_t callSiteIndex);

      private:
        size_t _callSiteIndex;
        BasicBlock* _returnBlock;
        std::string _calledObjectName;
        FunctionInstance _callSiteFunctionInstance;
    };

    // Maps name of the object that contains the call site, to the list of instances
    // The lists will have size > 1 in the case of an inline function that contains
    // a call to a non-inline function
    typedef std::unordered_map<std::string, std::list<Instance>> InstanceMap;

    InstanceMap& GetInstances();

    static void GenerateExternalFixedLatencyCall(
        IRContext& context, const std::vector<const ParseTreeNode*>& args, const FunctionDesc& functionDesc,
        const std::string& functionName, const ExternalModuleCallType callType,
        const boost::optional<ExternalClassInstanceName>& externModuleInstanceName, const Location& location);
    static void GenerateExternalFixedLatencyCallRegisterArgs(
        IRContext& context, const std::vector<const AllocatedRegister*>& argRegisters, const FunctionDesc& functionDesc,
        const std::string& functionName, const ExternalModuleCallType callType,
        const boost::optional<ExternalClassInstanceName>& externModuleInstanceName, const Location& location);

    const FunctionNode* GetContainingFunction() const;

    void Reset() override;

    std::string PrettyPrint() const override
    {
        return GetName() + "(" + dynamic_cast<const NodeList*>(_args)->PrettyPrint() + ")";
    }

    void SetClassTypes(const ClassType* const calledClassType, const ClassType* const callSiteClassType)
    {
        assert(!_calledClassType);
        assert(!_callSiteClassType);

        _calledClassType = calledClassType;
        _callSiteClassType = callSiteClassType;
    }

  private:
    struct Flags
    {
        bool _callRateSpecified;
        bool _transactionSizeSpecified;
        bool _fifoDepthSpecified;
    };

    CallNode& operator=(const CallNode&);

    CallModifiers GetCallParams(IRContext& context) const;
    void SetModifiers(IRContext& context, const IntAttributeNode* attr, CallModifiers& modifiers, Flags& flags) const;

    void GenerateInlineAtomicCall(IRContext& context) const;

    void GenerateAssert(IRContext& context) const;
    void GeneratePrint(IRContext& context) const;
    void GenerateCycleCounter(IRContext& context) const;
    void GenerateDebugView(IRContext& context) const;
    void GenerateAssertStringEqual(IRContext& context) const;
    void GenerateStringCount(IRContext& context) const;

    const ParseTreeNode* const _object;
    const ScopedIdentifierNode* const _scopedName;
    const ParseTreeNode* const _args;
    const ParseTreeNode* const _modifiers;
    mutable const FunctionNode* _containingFunction;
    mutable const ClassType* _calledClassType;
    mutable const ClassType* _callSiteClassType;
    mutable Scope _callSiteScope;

    mutable InstanceMap _instances;

    // For flat functions, the namespace where the function exists
    mutable Scope _targetScope;
};

// Interface to nodes that can describe a function that a callback is assigned to
struct UnresolvedCall;

class CallbackRValNode
{
  public:
    virtual ClassCallableFunction DescribeCall(const std::string& containingObjectName,
                                               const ClassCallableFunctionMap& classCallableFunctionMap) const = 0;
};

class FunctionSpecifierNode : public ParseTreeNode, public CallbackRValNode
{
  public:
    FunctionSpecifierNode(const ParseTreeNode* const object, const ParseTreeNode* const name)
        : _object(object), _name(name)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const ParseTreeNode* const GetObject() const;

    const ScopedIdentifierNode* const GetFunctionName() const;

    ClassCallableFunction DescribeCall(const std::string& containingObjectName,
                                       const ClassCallableFunctionMap& classCallableFunctionMap) const override;

  private:
    friend API ParseTreeNodePtr ParseFunctionCall(ParseTreeNodePtr, ParseTreeNodePtr, ParseTreeNodePtr);
    const ParseTreeNode* const _object;
    const ParseTreeNode* const _name;

    FunctionSpecifierNode& operator=(const FunctionSpecifierNode&);
};

class EnumConstantNode : public ParseTreeNode
{
  public:
    EnumConstantNode(const ParseTreeNode* const identifier, const ParseTreeNode* const value, const Scope& scope)
        : _identifier(dynamic_cast<const IdentifierNode*>(identifier)), _value(value), _scope(scope)
    {
        assert(_identifier);
    }

    void TypeCheck(TypeCheckContext& context) const override
    {
        _value->TypeCheck(context);
        SetType(_value->GetType());
    }

    void GenerateIR(IRContext& context) const override { assert(false); }

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override { assert(false); }

    const std::string& GetIdentifier() const { return _identifier->GetValue(); }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override
    {
        return _value->TryGetKnownValue(context, GetType());
    }

  private:
    EnumConstantNode& operator=(const EnumConstantNode&);

    const IdentifierNode* const _identifier;
    const ParseTreeNode* const _value;
    const Scope _scope;
};

class EnumNode : public ParseTreeNode
{
  public:
    EnumNode(const ParseTreeNode* const name, const ParseTreeNode* const baseType, const ParseTreeNode* const members,
             const Scope& namespaceScope);

    void TypeCheck(TypeCheckContext& context) const override {}

    void GenerateIR(IRContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override {}

    std::string PrettyPrint() const override
    {
        return "enum " + _name->PrettyPrint();

        // Members are not currently printed
    }

  private:
    const ParseTreeNode* const _name;
    const ParseTreeNode* const _baseType;
    const ParseTreeNode* const _constants;
};

class StructUnionNode : public ParseTreeNode
{
  public:
    StructUnionNode(const ContainerType type, const ParseTreeNode* const name, const ParseTreeNode* const members,
                    const Scope& namespaceScope);

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const ParseTreeNode* const GetObjMembers() const { return _members; }

    std::string PrettyPrint() const override;

  private:
    StructUnionNode& operator=(const StructUnionNode&);

    const ParseTreeNode* const _name;
    const ParseTreeNode* const _members;
    const ContainerType _containerType;
};

struct RegisterDescription;

typedef std::function<void(IRContext& context, const AllocatedRegister* const reg)> StoreCallback;

class LValNode
{
  public:
    virtual const AllocatedRegister* Load(IRContext& context) const = 0;

    virtual void Store(IRContext& context, const StoreCallback& storeCallback) const = 0;

    virtual void Initialize(IRContext& context, const ParseTreeNode* const rhs) const = 0;

    virtual std::string GetObjectName(KnownValueContext& context, const std::string& containingObjectName,
                                      const ResolveReferenceFunction& resolveReferenceCallback) const;

    virtual Scope GetObjectScope() const = 0;

    virtual bool SupportsPartialWrites() const { return true; }

    virtual bool IsMemory() const { return false; }

    virtual std::string GetRootVariableName() const { return ""; }

    static void StoreImpl(IRContext& context, const AllocatedRegister* const dstRegisters,
                          const AllocatedRegister* const srcRegisters, const Location& location);

    static void StoreRegister(IRContext& context, const AllocatedRegister* const dest,
                              const AllocatedRegister* const src, const Location& location);
};

class VariableAccessNode : public ParseTreeNode, public LValNode, public CallbackRValNode
{
  public:
    VariableAccessNode(ScopedIdentifierNode* const scopedName, const bool isKnownConst)
        : _scopedIdentifierNode(scopedName), _isKnownConstInitialSet(isKnownConst)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    const AllocatedRegister* Load(IRContext& context) const override;

    void Store(IRContext& context, const StoreCallback& storeCallback) const override;

    void Initialize(IRContext& context, const ParseTreeNode* const rhs) const override;

    void GenerateIR(IRContext& context) const override;

    const std::string GetName() const { return _scopedIdentifierNode->GetFlatenedName(); }

    std::string GetRootVariableName() const override { return _scopedIdentifierNode->GetName(); }

    std::string GetObjectName(KnownValueContext& context, const std::string& containingObjectName,
                              const ResolveReferenceFunction& resolveReferenceCallback) const override;

    Scope GetObjectScope() const override { return _scopedIdentifierNode->GetScope(); }

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    void Rename(const std::string& newName);

    std::map<std::string, std::string> GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                     const std::string& resultBaseName,
                                                     const std::string& thisObjectName,
                                                     const ClassType* const thisClassType) const override;

    std::string PrettyPrint() const override { return GetName(); }

    ClassCallableFunction DescribeCall(const std::string& containingObjectName,
                                       const ClassCallableFunctionMap& classCallableFunctionMap) const override;

    std::pair<Scope, std::string> GetSymbolLookupKey() const;

    bool IsKnownConstInitializer() const { return _isKnownConstInitialSet; }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override;

  private:
    VariableAccessNode& operator=(const VariableAccessNode&);

    void CheckGlobalRestrictions(IRContext& context) const;

    ScopedIdentifierNode* const _scopedIdentifierNode;

    const bool _isKnownConstInitialSet;
};

class ArrayAccessNode : public ParseTreeNode, public LValNode
{
  public:
    ArrayAccessNode(const ParseTreeNode* const ary, const ParseTreeNode* const indexNode)
        : _array(ary), _indexNode(indexNode)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void TypeCheckForAssignment(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const AllocatedRegister* Load(IRContext& context) const override;

    void Store(IRContext& context, const StoreCallback& storeCallback) const override;

    void Initialize(IRContext& context, const ParseTreeNode* const rhs) const override;

    const ParseTreeNode* GetIndexNode() const;

    std::string GetObjectName(KnownValueContext& context, const std::string& containingObjectName,
                              const ResolveReferenceFunction& resolveReferenceCallback) const override;

    Scope GetObjectScope() const override;

    bool SupportsPartialWrites() const override;

    bool IsMemory() const override;

    bool IsEccMemory() const;

    const ScopedIdentifierNode* GetEccFuncNameNode() const;

    std::string GetRootVariableName() const override
    {
        return dynamic_cast<const LValNode*>(_array)->GetRootVariableName();
    }

    std::string PrettyPrint() const override
    {
        std::ostringstream str;
        str << _array->PrettyPrint() << "[" << _indexNode->PrettyPrint() << "]";
        return str.str();
    }

  private:
    ArrayAccessNode& operator=(const ArrayAccessNode&);

    const AllocatedRegister* Load_Memory(IRContext& context) const;

    const AllocatedRegister* Load_Array(IRContext& context) const;

    bool IsIndexKnown(KnownValueContext& context) const;

    size_t GetKnownIndex(KnownValueContext& context) const;

    enum class IndexAccessMode
    {
        Read,
        Write
    };

    size_t GetIndexRegister(IRContext& context, const IndexAccessMode mode) const;

    const AllocatedLeafRegister* EvaluateIndexExpression(IRContext& context) const;

    const ParseTreeNode* const _array;
    const ParseTreeNode* const _indexNode;
};

class MemberAccessNode : public ParseTreeNode, public LValNode
{
  public:
    MemberAccessNode(const ParseTreeNode* const container, const ParseTreeNode* const contained);

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const AllocatedRegister* Load(IRContext& context) const override;

    void Store(IRContext& context, const StoreCallback& storeCallback) const override;

    void Initialize(IRContext& context, const ParseTreeNode* const rhs) const override;

    bool SupportsPartialWrites() const override;

    std::string GetObjectName(KnownValueContext& context, const std::string& containingObjectName,
                              const ResolveReferenceFunction& resolveReferenceCallback) const override;

    Scope GetObjectScope() const override;

    std::map<std::string, std::string> GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                     const std::string& resultBaseName,
                                                     const std::string& thisObjectName,
                                                     const ClassType* const thisClassType) const override;

    std::string PrettyPrint() const override { return _container->PrettyPrint() + "." + _containedName; }

  private:
    MemberAccessNode& operator=(const MemberAccessNode&);

    const AllocatedRegister* GetRegisterFromContainer(IRContext& context,
                                                      const AllocatedRegister* const inputContainer) const;

    bool ContainerIsUnion() const;

    const ParseTreeNode* const _container;
    const std::string _containedName;
};

class BaseAttributeNode : public ParseTreeNode
{
  public:
    BaseAttributeNode(const unsigned attribute) : _attribute(attribute) {}

    void TypeCheck(TypeCheckContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    const unsigned _attribute;
};

class FlagAttributeNode : public BaseAttributeNode
{
  public:
    FlagAttributeNode(const unsigned attribute) : BaseAttributeNode(attribute) {}

    void TypeCheck(TypeCheckContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

  private:
    FlagAttributeNode& operator=(const FlagAttributeNode&);
};

class IntAttributeNode : public BaseAttributeNode
{
  public:
    IntAttributeNode(const unsigned attribute, size_t value) : BaseAttributeNode(attribute), _value(value) {}

    void TypeCheck(TypeCheckContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    const size_t _value;

  private:
    IntAttributeNode& operator=(const IntAttributeNode&);
};

class AttributeNode : public BaseAttributeNode
{
  public:
    AttributeNode(const unsigned attribute, const ParseTreeNode* const value)
        : BaseAttributeNode(attribute), _value(value)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    const ParseTreeNode* const _value;

  private:
    AttributeNode& operator=(const AttributeNode&);
};

class MemberModifierNode : public ParseTreeNode
{
  public:
    MemberModifierNode(const ParseTreeMemberProtectionModifier modifier) : _modifier(modifier) {}

    ParseTreeMemberProtectionModifier GetModifier() const { return _modifier; }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

  private:
    MemberModifierNode& operator=(const MemberModifierNode&);

    const ParseTreeMemberProtectionModifier _modifier;
};

class ClassNode : public ParseTreeNode
{
  public:
    ClassNode(const ParseTreeNode* const name, const ParseTreeNode* const members,
              const ParseTreeNode* const templateInstance, const Scope& namespaceScope,
              const std::string& unmangledName);

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const std::string& GetName() const;

    const std::string& GetUnmangledName() const;

    const ParseTreeNode* const GetObjMembers() const { return _members; }

    void MarkInterfaceFunctions(const ParseTreeFunctionModifier modifierToAdd);

    void PruneForExportOrExternalClassUse();

    void CreateExportCallbacks(const std::string& objectNAme);

    void CreateExternCallbacks();

    void Reset() override;

    bool IsExport() const;

    bool IsExportIgnoreTarget() const;

    bool IsExternal() const;

    bool IsTemplateInstance() const;

    ClassCallableFunctionMap GetCallbackExternFunctions() const;

    const FunctionNode* TryGetExternCallback(const std::string& name) const;

    const FunctionNode* TryGetExportCallback(const std::string& objectName, const std::string& callbackName) const;

    const FunctionNode* GetExportCallback(const std::string& objectName, const std::string& callbackName) const;

    void EnumerateMethods(const std::function<void(const FunctionNode*)>& callback,
                          const std::optional<ParseTreeMemberProtectionModifier> modifierFilter = {}) const;

    void EnumeratePublicMethods(const std::function<void(const FunctionNode*)>& callback) const;

    void EnumerateCallbacks(const std::function<void(const std::string&, const FunctionType*)>& callback) const;

    const DesignatedInitializerListNode* TryGetDefaultInitializer() const;

    std::list<TemplateArgument> GetTemplateArguments() const;

    const std::string& GetTemplateName() const;

    const std::string GetExternalClassName() const;

  private:
    ClassNode& operator=(const ClassNode&);

    using NotifyFunction = std::function<void(const std::string&, FunctionNode* const)>;

    void CreateCallbacksHelper(const ParseTreeFunctionModifier modifier, const NotifyFunction& notifyFunction);

    const ParseTreeNode* const _name;
    const ParseTreeNode* _members;
    const ParseTreeNode* _originalMembers;
    const ParseTreeNode* _templateInstance;
    const std::string _unmangledName;
    const DesignatedInitializerListNode* _defaultInitializerList;

    mutable bool _generatedIR;

    // Callbacks of exported classes correspond to 2 FunctionNode
    // which are generated internally.

    // Extern function used when compiling the export class
    std::map<std::string, FunctionNode*> _externCallbacks;

    // (Object name, Callback name) -> Export function used when instantiating an export class
    using ExportCallbackKey = std::pair<std::string, std::string>;

    std::map<ExportCallbackKey, FunctionNode*> _exportCallbacks;
};

class SwitchNode : public ParseTreeNode
{
  public:
    SwitchNode(const ParseTreeNode* const testExpression, const ParseTreeNode* const blockList)
        : _testExpression(testExpression), _blockList(blockList)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

  private:
    SwitchNode& operator=(const SwitchNode&);

    const ParseTreeNode* const _testExpression;
    const ParseTreeNode* const _blockList;
};

class SwitchBlockNode : public ParseTreeNode
{
  public:
    SwitchBlockNode(const ParseTreeNode* const caseValue, const ParseTreeNode* const statementList)
        : _caseValue(caseValue), _statementList(statementList)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    // Returns NULL for the default case
    const ParseTreeNode* GetCaseValue() const { return _caseValue; }

    const ParseTreeNode* GetStatementList() const { return _statementList; }

  private:
    SwitchBlockNode& operator=(const SwitchBlockNode&);

    // _caseValue is null for the "default" case
    const ParseTreeNode* const _caseValue;
    const ParseTreeNode* const _statementList;
};

// Does not coresspond to anything in the input source
// Just a wrapper around AllocatedRegister
class AllocatedRegisterNode : public ParseTreeNode
{
  public:
    AllocatedRegisterNode(const Type* const type, const AllocatedRegister* const src) : _ParseTreeNode(type), _src(src)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override {}

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

    std::string PrettyPrint() const override
    {
        // This will not show up in the output
        // But must be non-empoty to avoid assertions about empty names
        return "allocated_reg";
    }

  private:
    AllocatedRegisterNode& operator=(const AllocatedRegisterNode&);

    const AllocatedRegister* const _src;
};

class StaticNode : public ParseTreeNode
{
  public:
    StaticNode(const ParseTreeNode* const expression) : _expression(expression) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    std::string PrettyPrint() const override { return "static(" + _expression->PrettyPrint() + ")"; }

  protected:
    KnownValue TryGetKnownValueImpl(KnownValueContext& context) const override
    {
        return _expression->TryGetKnownValue(context, GetType());
    }

  private:
    StaticNode& operator=(const StaticNode&);

    const ParseTreeNode* const _expression;
};

class FanOutNode : public ParseTreeNode
{
  public:
    FanOutNode(const ParseTreeNode* const count, const ParseTreeNode* const expression)
        : _count(count), _expression(expression)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    std::string PrettyPrint() const override
    {
        return "fan_out<" + _count->PrettyPrint() + ">(" + _expression->PrettyPrint() + ")";
    }

  private:
    FanOutNode& operator=(const FanOutNode&);

    const ParseTreeNode* const _count;
    const ParseTreeNode* const _expression;
};

class StageNode : public ParseTreeNode
{
  public:
    StageNode() {}

    void TypeCheck(TypeCheckContext& context) const override {}

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

  private:
    StageNode& operator=(const StageNode&);
};

class ConcatNode : public ParseTreeNode
{
  public:
    ConcatNode(const ParseTreeNode* const elements) : _elements(elements) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    std::string PrettyPrint() const override { return "concat(" + _elements->PrettyPrint() + ")"; }

  private:
    ConcatNode& operator=(const ConcatNode&);

    const ParseTreeNode* const _elements;
};

class ReorderNode : public ParseTreeNode
{
  public:
    ReorderNode(const ParseTreeNode* const body) : _body(body) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

  private:
    ReorderNode& operator=(const ReorderNode&);

    const ParseTreeNode* const _body;
};

// for fixed latency externs
class InlineExternalModuleNode : public ParseTreeNode
{
  public:
    InlineExternalModuleNode(size_t latencyNode, const ParseTreeNode* const modifierList,
                             const ParseTreeNode* const returnTypeNode, const ParseTreeNode* const nameNode,
                             const ParseTreeNode* const paramsNode)
        : _latency(latencyNode), _modifierList(modifierList), _returnTypeNode(returnTypeNode), _nameNode(nameNode),
          _paramsNode(paramsNode)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const std::string& GetName() const { return dynamic_cast<const IdentifierNode*>(_nameNode)->GetValue(); }

  private:
    InlineExternalModuleNode& operator=(const InlineExternalModuleNode&);

    ParseTreeFunctionModifier GetModifiers() const;

    size_t _latency;
    const ParseTreeNode* const _modifierList;
    const ParseTreeNode* const _returnTypeNode;
    const ParseTreeNode* const _nameNode;
    const ParseTreeNode* const _paramsNode;
};

class UnregisterTypedefNode : public ParseTreeNode
{
  public:
    UnregisterTypedefNode(const std::string& name) : _name(name) {}

    void TypeCheck(TypeCheckContext& context) const override {}

    void GenerateIR(IRContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override
    {
        VisitNoChildren(visitFunction, context);
    }

  private:
    UnregisterTypedefNode& operator=(const UnregisterTypedefNode&);

    const std::string _name;
};

class StaticAssertNode : public ParseTreeNode
{
  public:
    StaticAssertNode(const ParseTreeNode* const expr) : _expr(expr) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

  private:
    StaticAssertNode& operator=(const StaticAssertNode&);

    const ParseTreeNode* const _expr;
};

class ExportTypeNode : public ParseTreeNode
{
  public:
    ExportTypeNode(const ParseTreeNode* const attributeList, const ParseTreeNode* const type)
        : _attributeList(attributeList), _typeNode(type)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

  private:
    std::optional<std::string> GetNameOverride() const;

    ExportTypeNode& operator=(const ExportTypeNode&);

    const ParseTreeNode* const _typeNode;
    const ParseTreeNode* const _attributeList;
};

class ExternNode : public ParseTreeNode
{
  public:
    ExternNode(const ParseTreeNode* const identifier, const ParseTreeNode* const attributes)
        : _identifier(identifier), _attributes(attributes)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

  private:
    ExternNode& operator=(const ExternNode&);

    const ParseTreeNode* const _identifier;

    const ParseTreeNode* const _attributes;
};

class NamespaceNode : public ParseTreeNode
{
  public:
    NamespaceNode(const ParseTreeNode* const identifier, const ParseTreeNode* const body)
        : _identifierNode(identifier), _bodyNode(body)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const std::string& GetName() const;

  private:
    const ParseTreeNode* const _identifierNode;

    const ParseTreeNode* const _bodyNode;

    NamespaceNode& operator=(const NamespaceNode&);
};

class DesignatorNode : public ParseTreeNode
{
  public:
    DesignatorNode(const ParseTreeNode* const name, const ParseTreeNode* const value) : _name(name), _value(value) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const std::string& GetName() const;

    const ParseTreeNode* GetValue() const;

  private:
    DesignatorNode& operator=(const DesignatorNode&);

    const ParseTreeNode* const _name;

    const ParseTreeNode* const _value;
};

class BaseInitializerListNode : public ParseTreeNode
{
  public:
    BaseInitializerListNode(const Type* const type = nullptr) : ParseTreeNode(type) {}

    virtual const ParseTreeNode* TryGetField(const size_t index, const std::string& name) const = 0;

    const ParseTreeNode* TryGetFieldByIndex(const size_t index) const { return TryGetField(index, ""); }

    const ParseTreeNode* TryGetFieldByName(const std::string& name) const
    {
        return TryGetField(std::numeric_limits<size_t>::max(), name);
    }

    // Adds entries to ObjectToClassCallableFunctionMap
    virtual void ResolveFunctions(const std::string& initializedObjectName, const std::string& initializerObjectName,
                                  ObjectToClassCallableFunctionMap& objectToCallableFunctions) const = 0;

    mp_int GetFlatValue(IRContext& context) const;
};

void TryResolveInitializerListFunctions(const std::string& initializedObjectName,
                                        const std::string& initializerObjectName, const ParseTreeNode* const node,
                                        ObjectToClassCallableFunctionMap& objectToCallableFunctions);

// Initializer list of the form { 1, 3, 5 }
class InitializerListNode : public BaseInitializerListNode
{
  public:
    InitializerListNode(const ParseTreeNode* const list);

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const ParseTreeNode* TryGetField(const size_t index, const std::string& name) const override;

    const std::vector<const ParseTreeNode*>& Children() const { return _list->Children(); }

    std::map<std::string, std::string> GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                     const std::string& resultBaseName,
                                                     const std::string& thisObjectName,
                                                     const ClassType* const thisClassType) const override;

    std::string PrettyPrint() const override { return "{" + _list->PrettyPrint() + "}"; }

    void ResolveFunctions(const std::string& initializedObjectName, const std::string& initializerObjectName,
                          ObjectToClassCallableFunctionMap& objectToCallableFunctions) const override;

  private:
    InitializerListNode& operator=(const InitializerListNode&);

    const NodeList* const _list;
};

// Initializer list of the form { .x = 4, .flag = false }
class DesignatedInitializerListNode : public BaseInitializerListNode
{
  public:
    DesignatedInitializerListNode(const ParseTreeNode* const list);

    void TypeCheck(TypeCheckContext& context) const override;

    void TypeCheckDefaultInitializer(TypeCheckContext& context) const;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const ParseTreeNode* TryGetField(const size_t index, const std::string& name) const override;

    std::map<std::string, std::string> GetReferences(const ResolveReferenceFunction& resolveReferenceCallback,
                                                     const std::string& resultBaseName,
                                                     const std::string& thisObjectName,
                                                     const ClassType* const thisClassType) const override;

    std::string PrettyPrint() const override;

    void ResolveFunctions(const std::string& initializedObjectName, const std::string& initializerObjectName,
                          ObjectToClassCallableFunctionMap& objectToCallableFunctions) const override;

    bool ResolveFunction(const std::string& initializedObjectName, const std::string& initializerObjectName,
                         const std::string& callbackName, const FunctionType* const callbackType,
                         ObjectToClassCallableFunctionMap& objectToCallableFunctions) const;

  private:
    DesignatedInitializerListNode& operator=(const DesignatedInitializerListNode&);

    std::map<std::string, const DesignatorNode*> _children;
};

class DefaultInitializerNode : public ParseTreeNode
{
  public:
    DefaultInitializerNode(const DesignatedInitializerListNode* const initializerList)
        : _initializerList(initializerList)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override {}

    void TypeCheckImpl(TypeCheckContext& context) const;

    void GenerateIR(IRContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const DesignatedInitializerListNode* GetInitializerList() const { return _initializerList; }

  private:
    const DesignatedInitializerListNode* const _initializerList;
};

class InterpolationExpressionNode : public ParseTreeNode
{
  public:
    InterpolationExpressionNode(const ParseTreeNode* const expression, const ParseTreeNode* const alignment,
                                const ParseTreeFormatSpecifier format, const size_t precision)
        : _expression(expression), _alignmentNode(alignment), _format(format), _precision(precision), _alignment(0)
    {
    }

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    void Interpolate(IRContext& context, Operation& op) const;

    std::string PrettyPrint() const override;

  private:
    const ParseTreeNode* const _expression;
    const ParseTreeNode* const _alignmentNode; // can be nullptr
    const ParseTreeFormatSpecifier _format;
    const size_t _precision;

    mutable int64_t _alignment;
};

class InterpolatedStringSegmentNode : public ParseTreeNode
{
  public:
    InterpolatedStringSegmentNode(const LiteralStringNode* const str, const ParseTreeNode* const exp);

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override {}

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    const std::string& GetString() const { return _string->GetValue(); }

    const InterpolationExpressionNode* GetExpression() const { return _expression; }

    std::string PrettyPrint() const override;

  private:
    const LiteralStringNode* const _string;
    const InterpolationExpressionNode* const _expression; // can be nullptr
};

class InterpolatedStringNode : public ParseTreeNode
{
  public:
    InterpolatedStringNode(const NodeList* const segments) : _segments(segments) {}

    void TypeCheck(TypeCheckContext& context) const override;

    void GenerateIR(IRContext& context) const override;

    void Visit(const VisitFunction& visitFunction, VisitContext& context) const override;

    std::string PrettyPrint() const override;

  private:
    const NodeList* const _segments;
};

void ToSigned(LeafType& input);

const LeafType* ToSigned(const LeafType* const input, const Location& location);

size_t GetIntegerBitCount(const mp_int& input);

extern const std::string g_globalClassName;
extern const std::string g_globalObjectName;

std::string GetArrayElementObjectName(const std::string& baseName, const size_t elementIndex);

std::string CombineObjectAndMemberName(const Scope& scope, const std::string& objectName,
                                       const std::string& memberName);

std::string GetStaticLocalInstanceNameWithoutObject(const size_t instanceUniqueId);

std::string GetStaticLocalInstanceName(const std::string& staticVariableName, const size_t instanceUniqueId);

std::string GetMemberName(const std::string& containerName, const std::string& containedName);

bool ContainsReference(const Type* const type);

inline ParseTreeFunctionModifier operator|(const ParseTreeFunctionModifier lhs, const ParseTreeFunctionModifier rhs)
{
    return static_cast<ParseTreeFunctionModifier>(static_cast<size_t>(lhs) | static_cast<size_t>(rhs));
}

inline ParseTreeFunctionModifier operator&(const ParseTreeFunctionModifier lhs, const ParseTreeFunctionModifier rhs)
{
    return static_cast<ParseTreeFunctionModifier>(static_cast<size_t>(lhs) & static_cast<size_t>(rhs));
}

// RAII class for g_currentTokenLocation
class SetParseLocation
{
  public:
    SetParseLocation(const Location& location);
    ~SetParseLocation();

  private:
    Location _saved;
};

#endif
