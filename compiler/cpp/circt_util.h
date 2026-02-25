// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#include <circt/Dialect/HW/HWOps.h>
#include <circt/Dialect/Kanagawa/KanagawaOps.h>
#include <circt/Dialect/Pipeline/PipelineOps.h>
#include <circt/Dialect/Seq/SeqTypes.h>
#include <circt/Support/LLVM.h>
#include <mlir/IR/Value.h>

// Redirect assertions to a function that will throw an exception on release builds
#include "ship_assert.h"

// Describes how a HW module port
// will be used in an ESI channel
enum class EsiPortSemantics
{
    NonEsi,
    Valid,
    Ready,
    ReadEnable,
    Empty,
    Payload
};

// Describes which channel in a bundle
// a particular port will be a port of
enum class EsiChannelSemantics
{
    NonEsi,

    // The channel which sends information from generated HW to the outside world
    FromGeneratedHw,

    // The channel which receives information from the outside world
    ToGeneratedHw
};

// Name of a channel within a bundle
enum class EsiChannelName
{
    Undefined,
    Arguments,
    Results
};

void LoadDialects(mlir::MLIRContext& context);
mlir::ModuleOp CreateMlirModuleAndDesign(const mlir::Location& loc, const std::string& designName);
circt::kanagawa::DesignOp GetDesignOp(mlir::ModuleOp mlirModule);

circt::hw::StructType GetInspectableStructType();
mlir::Location LocationToCirctLocation(const Location& locationIn);
mlir::Location CallStackToCirctLocation(const Program& program, const size_t callStackIndex,
                                        const mlir::Location leafLocation);
mlir::Location FileAndLineNumberToCirctLocation(const Program& program, const FileAndLineNumber& faln);
mlir::Location OperationToCirctLocation(const Operation& op, const Program& program);
mlir::Location GetUnknownLocation();
mlir::Location RegDescToLocation(const RegisterDescription& regDesc);

std::string FixupStringCirct(const std::string& src);

mlir::Value GetPathOp(circt::OpBuilder& opb, const ObjectPath& srcPath, const ObjectPath& dstPath,
                      const std::string& finalTypeName, const std::string& circtDesignName);

mlir::APInt LiteralToApInt(const Literal& l);
mlir::Value LiteralToValue(const Literal& l, circt::OpBuilder& opb, const mlir::Location& location);

mlir::StringAttr StringToStringAttr(const std::string& str);

size_t GetMlirTypeWidth(const mlir::Type& type);

size_t GetMlirValueWidth(const mlir::Value& v);

mlir::IntegerType GetIntegerType(const size_t width,
                                 mlir::IntegerType::SignednessSemantics signedness = mlir::IntegerType::Signless);

mlir::IntegerType GetI1Type();

circt::hw::ArrayType GetPackedArrayType(const mlir::Type& elementType, const size_t elementCount);

circt::hw::ArrayType GetPackedArrayTypeParameterizedSize(const mlir::Type& elementType, const std::string& paramName);

circt::seq::ClockType GetClockType();

mlir::Type ToMlirType(const Type* typeIn, bool signedness = false);

mlir::Value GetTypedZeros(circt::OpBuilder& opb, const mlir::Location& location, const mlir::Type& typeIn);

mlir::Value PopCount(circt::OpBuilder& opb, const mlir::Location location, const mlir::ValueRange values,
                     const mlir::Value outputWhenEmpty);

mlir::Value ReadContainerPort(circt::OpBuilder& opb, const mlir::Location location, const mlir::Value containerPath,
                              const mlir::StringAttr portSymbol, const mlir::Type type, const mlir::Type dstType = {});

void WriteContainerPort(circt::OpBuilder& opb, const mlir::Location location, const mlir::Value containerPath,
                        const mlir::StringAttr portSymbol, const mlir::Type type, const mlir::Value valueToWrite);

void AssertTwoState(mlir::ModuleOp& moduleOp);

void DumpMlirOperation(mlir::Operation* const op);

void AttachNameHintToValue(const mlir::Value value, const AccessedRegister accessedRegister, const Program& program);

mlir::Value MuxTree(const mlir::Value selectIndex, const std::vector<mlir::Value>& choices, circt::OpBuilder& opb,
                    const mlir::Location& location);

using SourceOperandToMlirValueCb =
    std::function<mlir::Value(const Operation& op, const size_t srcOperandIndex, const size_t desiredWidth)>;

using StoreMlirValueInDestOperandCb =
    std::function<void(const Operation& op, const size_t dstOperandIndex, const mlir::Value& value)>;

void ConvertOpToCirct(const Operation& op, circt::OpBuilder& opb, const Program& program,
                      const SourceOperandToMlirValueCb& srcToValue, const StoreMlirValueInDestOperandCb& storeDst);

mlir::Value AdjustValueWidth(const mlir::Value& srcValue, const size_t desiredWidth, const bool signExtend,
                             circt::OpBuilder& opb, const mlir::Location& location);

void SetLoweringOperations(mlir::ModuleOp& moduleOp);

std::string GetSVTypeString(mlir::Type type, const std::string& arrayDims);

mlir::StringAttr GetFullyQualifiedStringAttr(const ObjectPath& containerPath, const std::string& fieldName);

circt::hw::InnerSymAttr GetFullyQualifiedInnerSymAttr(const ObjectPath& containerPath, const std::string& fieldName);

std::string GetMemoryContainerName(const std::string& registerName);

void addPipelineSrcs(mlir::ModuleOp module);

// Compute a single value by applying a binary operator to reduce an array of values
// If the input array of values is empty, then return a default value
template <typename OpType>
mlir::Value Reduce(const llvm::SmallVector<mlir::Value>& inputs, const mlir::IntegerAttr& defaultValue,
                   circt::OpBuilder& opb, const mlir::Location& location, const bool TwoState = true)
{
    if (inputs.empty())
    {
        return circt::hw::ConstantOp::create(opb, location, defaultValue);
    }
    else
    {
        return OpType::create(opb, location, inputs, TwoState);
    }
}

// Used to construct a single mlir::Value out of a sparse set of slices
// unspecified bits are set to 0
class SparseConcat
{
  public:
    SparseConcat(circt::OpBuilder& opb, const mlir::Location& location, const size_t width);
    void Insert(const size_t offset, const mlir::Value value);
    mlir::Value Flush();

  private:
    circt::OpBuilder& _opb;
    mlir::Location _location;
    size_t _width;

    // Offset and value
    using QueuedValue = std::pair<size_t, mlir::Value>;

    std::list<QueuedValue> _values;
};

// Groups assignments (where AssignOp) could be used
// into an AlwaysCombOp with a batch of BPAssignOp
class BatchAssignments
{
  public:
    void Append(const mlir::Location& location, const mlir::Value& dstValue, const mlir::Value& srcValue);

    void AppendVerbatimDst(const mlir::Location& location, const std::string& dstString, const mlir::Value& srcValue);

    void Flush(circt::OpBuilder& opb, const mlir::Location& location);

    bool Empty() const;

  private:
    struct AssignRecord
    {
        mlir::Location _location;
        mlir::Value _dstValue;
        mlir::Value _srcValue;
    };

    struct VerbatimAssignRecord
    {
        mlir::Location _location;
        std::string _dstString;
        mlir::Value _srcValue;
    };

    std::vector<AssignRecord> _assignments;
    std::vector<VerbatimAssignRecord> _verbatimAssignments;
};

// Each call to Accumulate writes a subset of bits in an output port
// Flush() inserts sv.assign operations to write an entire output port
class AccumulateOutputPortUpdates
{
  public:
    void Accumulate(const size_t outputRegisterIndex, const mlir::Value& valueToWrite, const size_t offset,
                    const size_t width);

    void Flush(circt::OpBuilder& opb, const mlir::Location& location, const size_t outputRegisterIndex,
               const mlir::Value portSsaValue, BatchAssignments& batchAssignments);

  private:
    struct AccumulateRecord
    {
        size_t _offset;
        size_t _width;
        mlir::Value _value;
    };

    // List of updates to apply
    using Value = std::list<AccumulateRecord>;

    std::map<size_t, Value> _updates;
};

// Used to build verbatim ops with substitutions for mlir::Value
class VerbatimWriter
{
  public:
    VerbatimWriter(circt::OpBuilder& opb, const mlir::Location& location);

    ~VerbatimWriter();

    template <typename T, typename std::enable_if<std::is_same<T, mlir::Value>::value>::type* = nullptr>
    VerbatimWriter& operator<<(const T& v)
    {
        _str << "{{" << _substitutions.size() << "}}";

        _substitutions.push_back(v);

        return *this;
    }

    template <typename T, typename std::enable_if<!std::is_same<T, mlir::Value>::value>::type* = nullptr>
    VerbatimWriter& operator<<(const T& v)
    {
        _str << v;

        return *this;
    }

    mlir::Value GetExpr(mlir::Type type);

  private:
    circt::OpBuilder& _opb;

    mlir::Location _location;

    mlir::SmallVector<mlir::Value> _substitutions;

    std::ostringstream _str;
};

// This is a RAII class for a ifdef section for CIRCT
class DisableDynamicAssertsAndTranslateOffCirct
{
  public:
    DisableDynamicAssertsAndTranslateOffCirct(circt::OpBuilder& opb, mlir::Location opLocation);

    ~DisableDynamicAssertsAndTranslateOffCirct();

  private:
    circt::OpBuilder& _opb;
    mlir::Location _location;
};

class ModuleDeclarationHelper
{
  public:
    ModuleDeclarationHelper(RedirectableSourceWriter& writer, const std::string& name, const mlir::Location& location,
                            const std::string& circtDesignName, mlir::ModuleOp* const mlirModule);

    ~ModuleDeclarationHelper();

    mlir::Block* GetBodyBlock();

    void BeginEsiBundle(const std::string& name);

    void EndEsiBundle();

    void AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction, const size_t width,
                 const EsiPortSemantics portSemantics = EsiPortSemantics::NonEsi,
                 const EsiChannelSemantics channelSemantics = EsiChannelSemantics::NonEsi,
                 const EsiChannelName channelName = EsiChannelName::Undefined, const std::string fieldName = "");

    void AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction, const size_t outerWidth,
                 const size_t innerWidth);

    void AddPortOptionalArray(const std::string& name, const circt::hw::ModulePort::Direction direction,
                              const size_t outerWidth, const size_t innerWidth);

    void AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction, const mlir::Type type,
                 const Type* origType = nullptr, const EsiPortSemantics portSemantics = EsiPortSemantics::NonEsi,
                 const EsiChannelSemantics channelSemantics = EsiChannelSemantics::NonEsi,
                 const EsiChannelName channelName = EsiChannelName::Undefined, const std::string fieldName = "");

    void FinishPorts();

    std::string AssignPort(const std::string& portName);

    std::string AssignPortOptional(const std::string& portName);

    // Returns the InOut-typed sv::LogicOp value for an output port's net.
    // AssignPort() must have been called first for this port.
    mlir::Value GetOutputNetInOutValue(const std::string& portName) const;

    void AddTypedefs(const std::string& typeScopeName);

    mlir::Type GetInspectableTypeAlias();

    mlir::ModuleOp MlirModule();

    circt::kanagawa::ContainerOp Container();

    circt::OpBuilder& OpBuilder();

    void Finish();

    void EmitEsiWrapper(const std::string& circtDesignName);

    void FlushVerbatimStrings();

    using VerbatimCallback = std::function<void(VerbatimWriter& writer)>;

    void AddVerbatimOp(const mlir::Location& location, const VerbatimCallback& callback);

    std::string Name() const;

    mlir::Value GetPort(circt::OpBuilder& opb, const ObjectPath& srcPath, const ObjectPath& dstPath,
                        const mlir::StringAttr portSymbol, const circt::kanagawa::Direction portDirection,
                        const mlir::Type portType, const std::string& finalTypeName,
                        const std::string& circtDesignName);

    void WritePort(circt::OpBuilder& opb, const ObjectPath& srcPath, const ObjectPath& dstPath,
                   const mlir::StringAttr portSymbol, const std::string& finalTypeName,
                   const std::string& circtDesignName, mlir::Value value);

    mlir::Value ReadPort(circt::OpBuilder& opb, const ObjectPath& srcPath, const ObjectPath& dstPath,
                         const mlir::StringAttr portSymbol, const std::string& finalTypeName,
                         const std::string& circtDesignName, const mlir::Type type);

  private:
    mlir::Type GetTypeAlias(const std::string& name, const mlir::Type& referencedType);

  private:
    void AssertStructsMatch(const mlir::Type& circtTypeAlias, const std::string& otherStructName);

    std::string _name;
    mlir::Location _location;
    std::string _circtDesignName;
    circt::OpBuilder _opb;
    RedirectableSourceWriter& _originalWriter;

    struct PortInfo
    {
        circt::hw::PortInfo _hwPortInfo;
        EsiPortSemantics _esiPortSemantics;
        EsiChannelSemantics _esiChannelSemantics;
        EsiChannelName _esiChannelName;
        std::string _fieldName;
        std::optional<std::string> _bundleName;
        const Type* _origType;
    };

    llvm::SmallVector<PortInfo> _ports;
    mlir::ModuleOp _mlirModule;
    circt::kanagawa::ContainerOp _container;
    StringSourceWriter _verbatimBuffer;

    std::optional<std::string> _bundleName;
    size_t _bundleStartPortIndex;

    // Maps bundle name to range of relevant ports
    std::map<std::string, std::pair<size_t, size_t>> _bundleNameToPortRange;

    std::map<std::string, size_t> _portNameToIndex;
    std::map<std::string, mlir::Value> _outputValues;

    // Maps port name to the InOut-typed sv::LogicOp value for the output net
    std::map<std::string, mlir::Value> _outputNetInOutValues;

    // Maps port name to mlir operation which represents the port
    std::map<std::string, mlir::Value> _inputPortOps;
    std::map<std::string, mlir::Value> _outputPortOps;

    circt::hw::TypeScopeOp _typeScopeOp;

    bool _finished;

    bool _exportVerilog;
};

// RAII class for {Begin, End}EsiBundle
class PushPopEsiBundle
{
  public:
    PushPopEsiBundle(ModuleDeclarationHelper& helper, const std::optional<std::string>& name) : _helper(helper)
    {
        if (name)
        {
            _pushedName = true;

            helper.BeginEsiBundle(*name);
        }
        else
        {
            _pushedName = false;
        }
    }

    ~PushPopEsiBundle()
    {
        if (_pushedName)
        {
            _helper.EndEsiBundle();
        }
    }

  private:
    ModuleDeclarationHelper& _helper;
    bool _pushedName;
};

// Helper to create a triggered region
// and feed signals into it
class TriggeredOpHelper
{
  public:
    TriggeredOpHelper(circt::OpBuilder& opb, circt::pipeline::ScheduledPipelineOp& scheduledPipelineOp,
                      const SourceOperandToMlirValueCb& sourceOperandToMlirValue, const Program& program,
                      const mlir::Value enableSignal);

    ~TriggeredOpHelper();

    void AddOp(const Operation& op);

    void DoneAddingOps(const circt::hw::EventControl triggerCondition, const mlir::Value clock);

    mlir::Value GetEnableSignal();

    mlir::Value GetSourceOperand(const Operation& op, const size_t operandIndex);

    mlir::Value GetSourceOperand(const Operation& op, const size_t operandIndex, const size_t desiredWidth);

  private:
    size_t AddSignal(const mlir::Value& value);

    circt::OpBuilder& _opb;
    circt::pipeline::ScheduledPipelineOp& _scheduledPipelineOp;
    SourceOperandToMlirValueCb _sourceOperandToMlirValue;
    const Program& _program;
    mlir::SmallVector<mlir::Value> _signals;
    circt::hw::TriggeredOp _triggeredOp;
    circt::OpBuilder::InsertPoint _insertionPoint;
    bool _doneAddingOps;
    std::vector<const Operation*> _ops;
    std::map<const Operation*, size_t> _opToStartIndex;
};

// Top-level container of MLIR
class MlirModule
{
  public:
    MlirModule(const std::string& circtDesignName);

    std::string Generate();

    mlir::ModuleOp& Module();

    void VerifyRoundTrip();

  private:
    mlir::ModuleOp _mlirModule;
};

// Wrapper around LatencyOp that handles a latency of 0
class LatencyOpHelper
{
  public:
    LatencyOpHelper(const mlir::Location location, circt::OpBuilder& opb, const llvm::SmallVector<mlir::Value>& values,
                    const size_t latency);

    mlir::Value GetResult(const size_t index);

  private:
    llvm::SmallVector<mlir::Value> _results;
};
