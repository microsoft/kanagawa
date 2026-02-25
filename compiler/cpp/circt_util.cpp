// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

#include <circt/Conversion/ExportVerilog.h>
#include <circt/Conversion/HWToSV.h>
#include <circt/Conversion/Passes.h>
#include <circt/Conversion/SeqToSV.h>
#include <circt/Dialect/Comb/CombDialect.h>
#include <circt/Dialect/Comb/CombOps.h>
#include <circt/Dialect/Comb/CombPasses.h>
#include <circt/Dialect/ESI/ESIOps.h>
#include <circt/Dialect/ESI/ESIPasses.h>
#include <circt/Dialect/ESI/ESITypes.h>
#include <circt/Dialect/HW/HWDialect.h>
#include <circt/Dialect/HW/HWOps.h>
#include <circt/Dialect/HWArith/HWArithDialect.h>
#include <circt/Dialect/Kanagawa/KanagawaDialect.h>
#include <circt/Dialect/Kanagawa/KanagawaPassPipelines.h>
#include <circt/Dialect/Kanagawa/KanagawaPasses.h>
#include <circt/Dialect/Pipeline/PipelineDialect.h>
#include <circt/Dialect/Pipeline/PipelineOps.h>
#include <circt/Dialect/Pipeline/PipelinePasses.h>
#include <circt/Dialect/SV/SVDialect.h>
#include <circt/Dialect/SV/SVOps.h>
#include <circt/Dialect/SV/SVPasses.h>
#include <circt/Dialect/Seq/SeqDialect.h>
#include <circt/Dialect/Seq/SeqOps.h>
#include <circt/Dialect/Seq/SeqPasses.h>
#include <circt/Support/LoweringOptions.h>
#include <circt/Support/Passes.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Parser/Parser.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/Passes.h>

// Redirect assertions to a function that will throw an exception on release builds
#include "ship_assert.h"

// Don't require optimizations to respect semantics related to unknown values
static const bool TwoState = true;

void LoadDialects(mlir::MLIRContext& context)
{
    context.loadDialect<circt::comb::CombDialect, circt::hw::HWDialect, circt::hwarith::HWArithDialect,
                        circt::kanagawa::KanagawaDialect, circt::pipeline::PipelineDialect, circt::seq::SeqDialect,
                        circt::sv::SVDialect, circt::esi::ESIDialect>();
}

mlir::ModuleOp CreateMlirModuleAndDesign(const mlir::Location& loc, const std::string& designName)
{
    mlir::ModuleOp mlirModule = mlir::ModuleOp::create(loc);

    // The MLIR module contains 1 child, a design op
    // The design op contains all other IR
    // Create it here
    circt::OpBuilder opb = circt::OpBuilder::atBlockEnd(&mlirModule.getBodyRegion().front());

    circt::kanagawa::DesignOp designOp = circt::kanagawa::DesignOp::create(opb, loc, StringToStringAttr(designName));

    // add 1 block to the design op
    designOp.getBodyRegion().emplaceBlock();

    return mlirModule;
}

circt::kanagawa::DesignOp GetDesignOp(mlir::ModuleOp mlirModule)
{
    auto designOps = mlirModule.getBodyRegion().front().getOps<circt::kanagawa::DesignOp>();

    // There should be exactly 1 design op
    assert(!designOps.empty());
    assert(std::next(designOps.begin()) == designOps.end());
    ;

    return *(designOps.begin());
}

mlir::Location LocationToCirctLocation(const Location& locationIn)
{
    return mlir::FileLineColLoc::get(g_compiler->GetMlirContext(),
                                     g_compiler->GetSourceFileNameWithoutLeadingPath(locationIn._fileIndex),
                                     locationIn._beginLine, locationIn._beginColumn);
}

mlir::Location CallStackToCirctLocation(const Program& program, const size_t callStackIndex,
                                        const mlir::Location leafLocation)
{
    mlir::Location result = leafLocation;

    // GetLimitedCallstackFromIndex is used to honor the --max-stack-depth command line parameter
    CallStack callStack = program.GetLimitedCallstackFromIndex(callStackIndex);

    bool skippedOneFrame = false;

    while (!callStack.empty())
    {
        // Don't include the leaf frame in the call stack
        // because leafLocation has more accurate information (line within a function vs the first/last line of the
        // function)
        if (skippedOneFrame)
        {
            const StackFrame& stackFrame = callStack.top();

            const mlir::NameLoc frame = mlir::NameLoc::get(
                StringToStringAttr(stackFrame._unmangledFunctionName),
                mlir::FileLineColLoc::get(g_compiler->GetMlirContext(),
                                          g_compiler->GetSourceFileNameWithoutLeadingPath(stackFrame._fileIndex),
                                          stackFrame._lineNumber, stackFrame._columnNumber));

            result = mlir::CallSiteLoc::get(result, frame);
        }

        skippedOneFrame = true;

        callStack.pop();
    }

    return result;
}

mlir::Location FileAndLineNumberToCirctLocation(const Program& program, const FileAndLineNumber& faln)
{
    mlir::Location result = mlir::FileLineColLoc::get(g_compiler->GetMlirContext(),
                                                      g_compiler->GetSourceFileNameWithoutLeadingPath(faln._fileIndex),
                                                      faln._lineNumber, faln._columnNumber);

    if (faln._callStackIndex)
    {
        result = CallStackToCirctLocation(program, *faln._callStackIndex, result);
    }

    return result;
}

mlir::Location OperationToCirctLocation(const Operation& op, const Program& program)
{
    // If multiple operations are combined together during optimization
    // Then the union of there locations is preserved.  op._locations has one entry
    // for each unoptimized location.  Each of these locations can optionally have an associated call stack.
    llvm::SmallVector<mlir::Location> locationArray;

    for (const FileAndLineNumber& loc : op._locations)
    {
        locationArray.push_back(FileAndLineNumberToCirctLocation(program, loc));
    }

    if (locationArray.empty())
    {
        return mlir::UnknownLoc::get(g_compiler->GetMlirContext());
    }
    else
    {
        return mlir::FusedLoc::get(g_compiler->GetMlirContext(), locationArray);
    }
}

mlir::Location GetUnknownLocation() { return mlir::UnknownLoc::get(g_compiler->GetMlirContext()); }

mlir::Location RegDescToLocation(const RegisterDescription& regDesc)
{
    // Not all registers in the IR have an associated source variable
    if (regDesc._sourceVariable._name.empty())
    {
        return GetUnknownLocation();
    }

    return mlir::FileLineColLoc::get(
        g_compiler->GetMlirContext(),
        g_compiler->GetSourceFileNameWithoutLeadingPath(regDesc._sourceVariable._declaredLocation._fileIndex),
        regDesc._sourceVariable._declaredLocation._lineNumber, regDesc._sourceVariable._declaredLocation._columnNumber);
}

// Count the number of bits which are set
mlir::Value PopCount(circt::OpBuilder& opb, const mlir::Location location, const mlir::ValueRange values,
                     const mlir::Value outputWhenEmpty)
{
    // return outputWhenEmpty when values is empty
    if (values.empty())
    {
        return outputWhenEmpty;
    }

    if (values.size() == 1)
    {
        return values[0];
    }

    // First determine how wide the sum must be
    const size_t sumWidth = Log2RoundUp(values.size() + 1);
    assert(sumWidth > 1);

    const mlir::Type sumType = GetIntegerType(sumWidth);

    // Convert sum inputs to be this width
    const mlir::Value upperBits = GetTypedZeros(opb, location, GetIntegerType(sumWidth - 1));

    mlir::SmallVector<mlir::Value> widenedInputs;

    for (const mlir::Value v : values)
    {
        const mlir::Value widened = circt::comb::ConcatOp::create(opb, location, upperBits, v);

        widenedInputs.push_back(widened);
    }

    // Add them up
    return circt::comb::AddOp::create(opb, location, sumType, widenedInputs, TwoState);
}

void addPipelineSrcs(circt::pipeline::ScheduledPipelineOp pipeline)
{
    // Find the stage block which contains an op.
    auto findStage = [&](mlir::Block* block) -> mlir::Block*
    {
        while (block)
        {
            mlir::Operation* parentOp = block->getParentOp();
            if (mlir::isa<circt::pipeline::ScheduledPipelineOp>(parentOp))
            {
                // This is the pipeline operation
                return block;
            }
            block = parentOp->getBlock();
        }
        return nullptr;
    };

    // Find all operations in the pipeline which have operands in a previous
    // stage and fix said operand by inserting a 'pipeline.src' between the
    // operand and consuming operation.
    for (mlir::Block& stage : pipeline->getRegions().front().getBlocks())
    {
        // Use the same `pipeline.src` value for the same values in the same
        // stage. For values which don't require a `pipeline.src`, cache the
        // original value so we don't do potentially expensive checks.
        llvm::DenseMap<mlir::Value, mlir::Value> operandToSrcMap;
        mlir::OpBuilder opb(&stage, stage.begin());

        stage.walk(
            [&](mlir::Operation* op)
            {
                if (mlir::isa<circt::pipeline::SourceOp>(op))
                {
                    // This is a 'pipeline.src' operation, skip it
                    return;
                }

                // Insert a 'pipeline.src' for each operand that is not in the current stage
                for (size_t i = 0; i < op->getNumOperands(); ++i)
                {
                    mlir::Value operand = op->getOperand(i);
                    auto srcIter = operandToSrcMap.find(operand);
                    if (srcIter != operandToSrcMap.end())
                    {
                        // If the operand has already been processed, replace it with the 'pipeline.src' value
                        op->setOperand(i, srcIter->second);
                        continue;
                    }

                    bool insert;
                    // If the operand is defined by an operation, find the stage block for that operation and if
                    // they're in different blocks insert a 'pipeline.src'.
                    if (operand.getDefiningOp())
                    {
                        mlir::Block* operandStageBlock = findStage(operand.getDefiningOp()->getBlock());
                        insert = (operandStageBlock != &stage);
                    }
                    else if (auto blockArg = mlir::dyn_cast<mlir::BlockArgument>(operand))
                    {
                        // If the operation which defines the block argument lies between the consuming operation
                        // and the pipeline, there is no need to insert a 'pipeline.src'.
                        mlir::Operation* currentOp = blockArg.getOwner()->getParentOp();
                        insert = true;
                        while (currentOp)
                        {
                            if (currentOp == pipeline)
                            {
                                insert = false;
                                break;
                            }
                            currentOp = currentOp->getParentOp();
                        }
                    }
                    else
                    {
                        assert(false && "Operand defined by neither an operation nor a block argument");
                    }

                    if (insert)
                    {
                        auto srcOp = circt::pipeline::SourceOp::create(opb, op->getLoc(), operand.getType(), operand);
                        op->setOperand(i, srcOp.getResult());
                        operandToSrcMap[operand] = srcOp.getResult();
                    }
                    else
                    {
                        operandToSrcMap[operand] = operand;
                    }
                }
            });
    }

    if (mlir::failed(mlir::verify(pipeline, true)))
    {
        DumpMlirOperation(pipeline);

        throw std::runtime_error("CIRCT pipeline verification failed");
    }
}
void addPipelineSrcs(mlir::ModuleOp module)
{
    module.walk([&](circt::pipeline::ScheduledPipelineOp pipeline) { addPipelineSrcs(pipeline); });
}

// This generates a valid MLIR identifier
std::string FixupStringCirct(const std::string& src)
{
    std::string result = src;

    for (size_t i = 0; i < result.size(); i++)
    {
        bool validCharacter = true;
        char& c = result.at(i);

        validCharacter = std::isalnum(c);
        // Note that if c == '_' then validCharacter = false
        // But actually underscore is valid, that is OK because an underscore is inserted
        if (!validCharacter)
        {
            result.at(i) = '_';
        }
    }

    return result;
}

// Returns a path operation that represents the shortest path from src to dst
// If src==dst, then emptyPathValue is returned
mlir::Value GetPathOp(circt::OpBuilder& opb, const ObjectPath& srcPath, const ObjectPath& dstPath,
                      const std::string& dstContainerName, const std::string& circtDesignName)
{
    // Find the common root between the 2 paths
    ObjectPath commonRoot;

    const size_t maxLength = std::max(srcPath.size(), dstPath.size());

    for (size_t i = 0; i < maxLength; i++)
    {
        if ((i < srcPath.size()) && (i < dstPath.size()) && (srcPath[i] == dstPath[i]))
        {
            commonRoot.push_back(srcPath[i]);
        }
        else
        {
            break;
        }
    }

    llvm::SmallVector<mlir::Attribute> steps;

    // Step up from srcPath to the common root
    ObjectPath currPath = srcPath;

    while (currPath != commonRoot)
    {
        const mlir::FlatSymbolRefAttr symbolRef = mlir::FlatSymbolRefAttr::get(StringToStringAttr(currPath.back()));

        const mlir::Attribute step = circt::kanagawa::PathStepAttr::get(
            g_compiler->GetMlirContext(), circt::kanagawa::PathDirection::Parent,
            circt::kanagawa::ScopeRefType::get(g_compiler->GetMlirContext()), symbolRef);

        steps.push_back(step);

        currPath.pop_back();
    }

    // Step down from common root to dstPath
    while (currPath != dstPath)
    {
        assert(currPath.size() < dstPath.size());

        const std::string stepName = dstPath[currPath.size()];

        const mlir::FlatSymbolRefAttr symbolRef = mlir::FlatSymbolRefAttr::get(StringToStringAttr(stepName));

        const mlir::Attribute step = circt::kanagawa::PathStepAttr::get(
            g_compiler->GetMlirContext(), circt::kanagawa::PathDirection::Child,
            circt::kanagawa::ScopeRefType::get(g_compiler->GetMlirContext()), symbolRef);

        steps.push_back(step);

        currPath.push_back(stepName);
    }

    assert(!steps.empty());

    {
        // Add an explicit type name to the final step
        circt::kanagawa::PathStepAttr lastStep = llvm::cast<circt::kanagawa::PathStepAttr>(steps.back());

        steps.back() = circt::kanagawa::PathStepAttr::get(
            g_compiler->GetMlirContext(), lastStep.getDirection(),
            circt::kanagawa::ScopeRefType::get(g_compiler->GetMlirContext(),
                                               circt::hw::InnerRefAttr::get(StringToStringAttr(circtDesignName),
                                                                            StringToStringAttr(dstContainerName))),
            lastStep.getChild());

        return circt::kanagawa::PathOp::create(opb, GetUnknownLocation(), opb.getArrayAttr(steps));
    }
}

mlir::APInt LiteralToApInt(const Literal& l)
{
    llvm::SmallVector<uint64_t> words;

    const size_t numWords = Align(l._width, 64) / 64;

    const mp_int mask = (mp_int(1) << 64) - 1;

    for (size_t i = 0; i < numWords; i++)
    {
        words.push_back(MpToSizeT((l._value >> (i * 64)) & mask));
    }

    return mlir::APInt(l._width, words);
}

mlir::Value LiteralToValue(const Literal& l, circt::OpBuilder& opb, const mlir::Location& location)
{
    return circt::hw::ConstantOp::create(opb, location,
                                             opb.getIntegerAttr(opb.getIntegerType(l._width), LiteralToApInt(l)));
}

mlir::StringAttr StringToStringAttr(const std::string& str)
{
    return mlir::StringAttr::get(g_compiler->GetMlirContext(), str);
}

mlir::IntegerType GetIntegerType(const size_t width, mlir::IntegerType::SignednessSemantics signedness)
{
    return mlir::IntegerType::get(g_compiler->GetMlirContext(), width, signedness);
}

mlir::Value GetTypedZeros(circt::OpBuilder& opb, const mlir::Location& location, const mlir::Type& typeIn)
{
    if (llvm::isa<circt::hw::StructType>(typeIn))
    {
        const circt::hw::StructType structType = llvm::cast<circt::hw::StructType>(typeIn);

        mlir::SmallVector<mlir::Value> fields;

        for (const circt::hw::StructType::FieldInfo& field : structType.getElements())
        {
            fields.push_back(GetTypedZeros(opb, location, field.type));
        }

        return circt::hw::StructCreateOp::create(opb, location, typeIn, fields);
    }
    else if (llvm::isa<circt::hw::ArrayType>(typeIn))
    {
        const circt::hw::ArrayType arrayType = llvm::cast<circt::hw::ArrayType>(typeIn);

        const mlir::SmallVector<mlir::Value> elements(arrayType.getNumElements(),
                                                      GetTypedZeros(opb, location, arrayType.getElementType()));

        return circt::hw::ArrayCreateOp::create(opb, location, typeIn, elements);
    }
    else if (llvm::isa<circt::hw::TypeAliasType>(typeIn))
    {
        const mlir::Value structValue =
            GetTypedZeros(opb, location, llvm::cast<circt::hw::TypeAliasType>(typeIn).getInnerType());

        return circt::hw::BitcastOp::create(opb, location, typeIn, structValue);
    }
    else
    {
        return circt::hw::ConstantOp::create(opb, location, typeIn, 0);
    }
}

mlir::IntegerType GetI1Type() { return GetIntegerType(1); }

circt::hw::ArrayType GetPackedArrayType(const mlir::Type& elementType, const size_t elementCount)
{
    return circt::hw::ArrayType::get(elementType, elementCount);
}

circt::hw::ArrayType GetPackedArrayTypeParameterizedSize(const mlir::Type& elementType, const std::string& paramName)
{
    return circt::hw::ArrayType::get(
        g_compiler->GetMlirContext(), elementType,
        circt::hw::ParamDeclRefAttr::get(StringToStringAttr(paramName), GetIntegerType(64)));
}

circt::seq::ClockType GetClockType() { return circt::seq::ClockType::get(g_compiler->GetMlirContext()); }

mlir::Type ToMlirType(const Type* typeIn, bool signedness)
{
    const BoolType* boolType = dynamic_cast<const BoolType*>(typeIn);
    const ArrayType* arrayType = dynamic_cast<const ArrayType*>(typeIn);
    const FloatType* floatType = dynamic_cast<const FloatType*>(typeIn);
    const StructUnionType* structUnionType = dynamic_cast<const StructUnionType*>(typeIn);
    const LeafType* leafType = dynamic_cast<const LeafType*>(typeIn);

    if (boolType)
    {
        return GetI1Type();
    }
    else if (arrayType)
    {
        return GetPackedArrayType(ToMlirType(arrayType->_elementType, signedness), arrayType->_arraySize);
    }
    else if (floatType)
    {
        return GetIntegerType(32);
    }
    else if (structUnionType)
    {
        if (structUnionType->_type == ContainerType::Struct)
        {
            llvm::SmallVector<circt::hw::StructType::FieldInfo> fields;

            const auto addField = [&](const std::string& name, const Type* const type)
            {
                fields.push_back(
                    circt::hw::StructType::FieldInfo{StringToStringAttr(name), ToMlirType(type, signedness)});
            };

            for (const StructUnionType::EntryType& member : structUnionType->_members)
            {
                const Type* const memberType = member.second->GetDeclaredType();
                const std::string memberName = member.first;
                addField(memberName, memberType);
            }

            std::reverse(fields.begin(), fields.end());
            return circt::hw::StructType::get(g_compiler->GetMlirContext(), fields);
        }
        else
        {
            llvm::SmallVector<circt::hw::UnionType::FieldInfo> fields;

            const auto addField = [&](const std::string& name, const Type* const type)
            {
                fields.push_back(
                    circt::hw::UnionType::FieldInfo{StringToStringAttr(name), ToMlirType(type, signedness)});
            };

            for (const StructUnionType::EntryType& member : structUnionType->_members)
            {
                const Type* const memberType = member.second->GetDeclaredType();
                const std::string memberName = member.first;
                addField(memberName, memberType);
            }

            std::reverse(fields.begin(), fields.end());
            return circt::hw::UnionType::get(g_compiler->GetMlirContext(), fields);
        }
    }
    else if (leafType)
    {
        if (signedness)
        {
            return GetIntegerType(leafType->_width, leafType->_baseType == BaseType::Int ? mlir::IntegerType::Signed
                                                                                         : mlir::IntegerType::Unsigned);
        }
        else
        {
            return GetIntegerType(leafType->_width, mlir::IntegerType::Signless);
        }
    }
    else
    {
        assert(false);
        return GetI1Type();
    }
}

// Used to avoid symbol name conflicts for elements like container ports
// returns a symbol name which will be unique provided
// that flattened container paths are unique
mlir::StringAttr GetFullyQualifiedStringAttr(const ObjectPath& containerPath, const std::string& fieldName)
{
    ObjectPath pathWithField = containerPath;
    pathWithField.push_back("__field__" + fieldName);

    return StringToStringAttr(FixupStringCirct(SerializePath(pathWithField, '_')));
}

circt::hw::InnerSymAttr GetFullyQualifiedInnerSymAttr(const ObjectPath& containerPath, const std::string& fieldName)
{
    return circt::hw::InnerSymAttr::get(GetFullyQualifiedStringAttr(containerPath, fieldName));
}

// Returns a string that can be used as a unique symbol for
// memory container names.
// The suffix-free version is reserved for container instances
std::string GetMemoryContainerName(const std::string& registerName) { return registerName + "__mem_container"; }

mlir::Value ReadContainerPort(circt::OpBuilder& opb, const mlir::Location location, const mlir::Value containerPath,
                              const mlir::StringAttr portSymbol, const mlir::Type type, const mlir::Type dstType)
{
    // circt::kanagawa::Direction::Output means that it is an output port of the container
    const mlir::Value containerPort = circt::kanagawa::GetPortOp::create(opb, location, containerPath, portSymbol, type,
                                                                             circt::kanagawa::Direction::Output);

    mlir::Value ret = circt::kanagawa::PortReadOp::create(opb, location, containerPort);
    if (dstType && (dstType != type))
    {
        assert(circt::hw::getBitWidth(dstType) == circt::hw::getBitWidth(type));
        ret = circt::hw::BitcastOp::create(opb, location, dstType, ret);
    }
    return ret;
}

void WriteContainerPort(circt::OpBuilder& opb, const mlir::Location location, const mlir::Value containerPath,
                        const mlir::StringAttr portSymbol, const mlir::Type type, const mlir::Value valueToWrite)
{
    // circt::kanagawa::Direction::Input means that it is an input port of the container
    const mlir::Value containerPort = circt::kanagawa::GetPortOp::create(opb, location, containerPath, portSymbol, type,
                                                                             circt::kanagawa::Direction::Input);
    mlir::Value value = valueToWrite;
    if (type != valueToWrite.getType())
    {
        assert(circt::hw::getBitWidth(type) == circt::hw::getBitWidth(valueToWrite.getType()));
        value = circt::hw::BitcastOp::create(opb, location, type, valueToWrite);
    }

    circt::kanagawa::PortWriteOp::create(opb, location, containerPort, value);
}

void ConvertOpToCirct(const Operation& op, circt::OpBuilder& opb, const Program& program,
                      const SourceOperandToMlirValueCb& srcToValue, const StoreMlirValueInDestOperandCb& storeDst)
{
    const mlir::Location opLocation = OperationToCirctLocation(op, program);

    switch (op._opcode)
    {
    case Opcode::Mov:
    {
        assert(1 == op._src.size());
        assert(1 == op._dst.size());

        const mlir::Value v = srcToValue(op, 0, op._dst[0].Width(program));

        storeDst(op, 0, v);
    }
    break;

    case Opcode::UnaryOp:
    {
        assert(ParseTreeUnaryOpTypeInvert == op._flags._unaryOpType);
        assert(1 == op._src.size());
        assert(1 == op._dst.size());

        const size_t srcWidth = op._src[0].Width(program);

        mlir::Value srcValue = srcToValue(op, 0, op._dst[0].Width(program));
        mlir::Value v = circt::comb::createOrFoldNot(opLocation, srcValue, opb, TwoState);

        storeDst(op, 0, v);
    }
    break;

    case Opcode::Clear:
    {
        assert(Opcode::Clear == op._opcode);
        assert(0 == op._src.size());
        assert(1 == op._dst.size());

        const size_t dstWidth = op._dst[0].Width(program);
        const mlir::Value dstValue =
            circt::hw::ConstantOp::create(opb, opLocation, opb.getIntegerAttr(opb.getIntegerType(dstWidth), 0));

        storeDst(op, 0, dstValue);
    }
    break;

    case Opcode::BinaryOp:
    {
        assert(2 == op._src.size());
        assert(1 == op._dst.size());

        const size_t dstWidth = op._dst[0].Width(program);

        const size_t intermediateWidth = CalculateBinaryOpDesiredSourceOperandWidths(program, op);

        const bool lhsSigned = op.ShouldSignExtend(0);

        llvm::SmallVector<mlir::Value, 2> srcValues;

        for (size_t i = 0; i < 2; i++)
        {
            srcValues.push_back(srcToValue(op, i, intermediateWidth));
        }

        const bool eitherSigned = op._signExtendSourceMask != 0;

        switch (op._flags._binaryOpType)
        {
        case ParseTreeBinaryOpTypeAdd:
            storeDst(op, 0, circt::comb::AddOp::create(opb, opLocation, srcValues[0], srcValues[1], TwoState));
            break;

        case ParseTreeBinaryOpTypeSub:
            storeDst(op, 0, circt::comb::SubOp::create(opb, opLocation, srcValues[0], srcValues[1], TwoState));
            break;

        case ParseTreeBinaryOpTypeShl:
            storeDst(op, 0, circt::comb::ShlOp::create(opb, opLocation, srcValues[0], srcValues[1], TwoState));
            break;

        case ParseTreeBinaryOpTypeAnd:
            storeDst(op, 0, circt::comb::AndOp::create(opb, opLocation, srcValues[0], srcValues[1], TwoState));
            break;

        case ParseTreeBinaryOpTypeLutMul:
            // LutMul case is handled by CompileOperationCirct() in Verilog.cpp
            assert(false);
            break;

        case ParseTreeBinaryOpTypeOr:
            storeDst(op, 0, circt::comb::OrOp::create(opb, opLocation, srcValues[0], srcValues[1], TwoState));
            break;

        case ParseTreeBinaryOpTypeXor:
            storeDst(op, 0, circt::comb::XorOp::create(opb, opLocation, srcValues[0], srcValues[1], TwoState));
            break;

        case ParseTreeBinaryOpTypeShr:
            if (lhsSigned)
            {
                storeDst(op, 0, circt::comb::ShrSOp::create(opb, opLocation, srcValues[0], srcValues[1], TwoState));
            }
            else
            {
                storeDst(op, 0, circt::comb::ShrUOp::create(opb, opLocation, srcValues[0], srcValues[1], TwoState));
            }
            break;

        case ParseTreeBinaryOpTypeEQ:
            storeDst(op, 0,
                     circt::comb::ICmpOp::create(opb, opLocation, circt::comb::ICmpPredicate::eq, srcValues[0],
                                                     srcValues[1], TwoState));
            break;
        case ParseTreeBinaryOpTypeNE:
            storeDst(op, 0,
                     circt::comb::ICmpOp::create(opb, opLocation, circt::comb::ICmpPredicate::ne, srcValues[0],
                                                     srcValues[1], TwoState));
            break;
        case ParseTreeBinaryOpTypeGT:
            storeDst(op, 0,
                     circt::comb::ICmpOp::create(opb, 
                         opLocation, eitherSigned ? circt::comb::ICmpPredicate::sgt : circt::comb::ICmpPredicate::ugt,
                         srcValues[0], srcValues[1], TwoState));
            break;
        case ParseTreeBinaryOpTypeGE:
            storeDst(op, 0,
                     circt::comb::ICmpOp::create(opb, 
                         opLocation, eitherSigned ? circt::comb::ICmpPredicate::sge : circt::comb::ICmpPredicate::uge,
                         srcValues[0], srcValues[1], TwoState));
            break;
        case ParseTreeBinaryOpTypeLT:
            storeDst(op, 0,
                     circt::comb::ICmpOp::create(opb, 
                         opLocation, eitherSigned ? circt::comb::ICmpPredicate::slt : circt::comb::ICmpPredicate::ult,
                         srcValues[0], srcValues[1], TwoState));
            break;
        case ParseTreeBinaryOpTypeLE:
            storeDst(op, 0,
                     circt::comb::ICmpOp::create(opb, 
                         opLocation, eitherSigned ? circt::comb::ICmpPredicate::sle : circt::comb::ICmpPredicate::ule,
                         srcValues[0], srcValues[1], TwoState));
            break;

        default:
            assert(false);
        }
    }
    break;

    case Opcode::Gather:
    {
        const std::vector<GatherEntry>& gatherEntries = *op._flags._gather._entries;
        assert(gatherEntries.size() == op._src.size());
        mlir::SmallVector<mlir::Value> values;

        // Most significant to least significant
        for (size_t reverseIndex = 0; reverseIndex < op._src.size(); ++reverseIndex)
        {
            const size_t i = op._src.size() - reverseIndex - 1;
            const GatherEntry& gatherEntry = gatherEntries[i];

            // convert src operands to MLIR, then extract bit field
            const mlir::Value v = srcToValue(op, i, gatherEntry._sourceOffset + gatherEntry._numBits);
            const mlir::Value vSlice =
                circt::comb::ExtractOp::create(opb, opLocation, v, gatherEntry._sourceOffset, gatherEntry._numBits);
            values.push_back(vSlice);
        }
        // Concatenate values to the result
        mlir::Value concatValue = circt::comb::ConcatOp::create(opb, opLocation, values);

        // Update destination operand
        storeDst(op, 0, concatValue);
    }
    break;

    case Opcode::Lut:
    {
        assert(Opcode::Lut == op._opcode);
        assert(1 == op._dst.size());
        assert(GetCodeGenDeviceConfig()._supportsLuts);
        const size_t dstWidth = op._dst[0].Width(program);
        const Lut& lut = op._flags._lut;
        // The destination may be smaller if the consumers of the destination do not need all of the bits
        assert(lut._numDestinationBits >= dstWidth);
        mlir::SmallVector<mlir::Value> lutSlices;

        // Generate LUT table
        for (size_t invDstIndex = 0; invDstIndex < dstWidth; ++invDstIndex)
        {
            const size_t dstIndex = dstWidth - invDstIndex - 1;
            const LutEntry& lutEntry = lut._lutEntries[dstIndex];
            // build a LUT
            const size_t numChoices = lutEntry.TableSize();
            llvm::SmallVector<bool> lutCirct;
            mlir::SmallVector<mlir::Value> lutIndex;
            for (size_t i = 0; i < numChoices; i++)
            {
                const uint64_t bitVal = lutEntry.GetTableEntry(i);
                lutCirct.push_back(static_cast<bool>(bitVal));
            }
            for (size_t invI = 0; invI < lutEntry._numSources; ++invI)
            {
                const size_t i = lutEntry._numSources - invI - 1;
                mlir::Value v =
                    srcToValue(op, lutEntry._sourceIndices[i], op._src[lutEntry._sourceIndices[i]].Width(program));
                const mlir::Value vSlice = circt::comb::ExtractOp::create(opb, opLocation, v, lutEntry._sourceBit[i], 1);
                lutIndex.push_back(vSlice);
            }
            const mlir::Value lutSlice = circt::comb::TruthTableOp::create(opb, opLocation, opb.getI1Type(), lutIndex,
                                                                               lutCirct);
            lutSlices.push_back(lutSlice);
        }
        // Concatenate values to the result
        mlir::Value concatValue = circt::comb::ConcatOp::create(opb, opLocation, lutSlices);
        // Update destination operand
        storeDst(op, 0, concatValue);
    }
    break;

    case Opcode::Select:
    {
        assert(1 == op._dst.size());

        const size_t dstWidth = op._dst[0].Width(program);

        const size_t numChoices = op._src.size() - 1;

        // Decompose into a tree of 2:1 muxes
        const size_t selectIndexWidth = Log2(numChoices);

        const mlir::Value selectIndex = srcToValue(op, 0, selectIndexWidth);

        std::vector<mlir::Value> srcValues;

        for (size_t i = 1; i < op._src.size(); i++)
        {
            srcValues.push_back(srcToValue(op, i, dstWidth));
        }

        const mlir::Value result = MuxTree(selectIndex, srcValues, opb, opLocation);

        storeDst(op, 0, result);
    }
    break;

    default:
        assert(false);
    }
}

mlir::Value MuxTree(const mlir::Value selectIndex, const std::vector<mlir::Value>& choices, circt::OpBuilder& opb,
                    const mlir::Location& location)
{
    // All operands should have the same type
    assert(!choices.empty());

    const mlir::Type dstType = choices[0].getType();

    for (const mlir::Value& v : choices)
    {
        assert(dstType == v.getType());
    }

    const size_t numChoices = choices.size();
    assert(IsPow2(numChoices));

    // Decompose into a tree of 2:1 muxes
    const size_t selectIndexWidth = Log2(numChoices);
    assert(GetMlirValueWidth(selectIndex) == selectIndexWidth);

    llvm::SmallVector<mlir::Value> srcValues;

    for (const mlir::Value& v : choices)
    {
        srcValues.push_back(v);
    }

    for (size_t bitIndex = 0; bitIndex < selectIndexWidth; bitIndex++)
    {
        const mlir::Value indexBit = circt::comb::ExtractOp::create(opb, location, selectIndex, bitIndex, 1);

        llvm::SmallVector<mlir::Value> newSrcValues;

        assert(0 == (srcValues.size() % 2));

        for (size_t i = 0; i < srcValues.size() / 2; i++)
        {
            newSrcValues.push_back(circt::comb::MuxOp::create(opb, location, indexBit, srcValues[i * 2 + 1],
                                                                  srcValues[i * 2 + 0], TwoState));
        }

        srcValues = newSrcValues;
    }

    assert(1 == srcValues.size());

    return srcValues[0];
}

// Extend or truncate the width of a value
mlir::Value AdjustValueWidth(const mlir::Value& srcValue, const size_t desiredWidth, const bool signExtend,
                             circt::OpBuilder& opb, const mlir::Location& location)
{
    const size_t srcValueWidth = GetMlirValueWidth(srcValue);

    mlir::Value result = srcValue;

    if (srcValueWidth < desiredWidth)
    {
        const size_t bitsToAdd = desiredWidth - srcValueWidth;

        // extend
        if (signExtend)
        {
            assert(srcValueWidth > 0);

            result = circt::comb::createOrFoldSExt(location, srcValue, opb.getIntegerType(desiredWidth), opb);
        }
        else
        {
            mlir::Value upperBits =
                circt::hw::ConstantOp::create(opb, location, opb.getIntegerAttr(opb.getIntegerType(bitsToAdd), 0));

            result = circt::comb::ConcatOp::create(opb, location, upperBits, srcValue);
        }
    }
    else if (srcValueWidth > desiredWidth)
    {
        // truncate
        result = circt::comb::ExtractOp::create(opb, location, srcValue, 0, desiredWidth);
    }

    return result;
}

SparseConcat::SparseConcat(circt::OpBuilder& opb, const mlir::Location& location, const size_t width)
    : _opb(opb), _location(location), _width(width)
{
}

void SparseConcat::Insert(const size_t offset, const mlir::Value value)
{
    _values.push_back(QueuedValue{offset, value});
}

mlir::Value SparseConcat::Flush()
{
    // Sort values by offset
    _values.sort([](const QueuedValue& lhs, const QueuedValue& rhs) { return lhs.first < rhs.first; });

    // Generate an array of MLIR values to be concatenated
    // Fill in zeros for any gaps
    mlir::SmallVector<mlir::Value> values;

    size_t offset = 0;

    for (const QueuedValue& qv : _values)
    {
        assert(qv.first >= offset);

        if (qv.first > offset)
        {
            // pad with zeros
            values.push_back(circt::hw::ConstantOp::create(_opb, _location, GetIntegerType(qv.first - offset), 0));

            offset = qv.first;
        }

        values.push_back(qv.second);

        offset += GetMlirValueWidth(qv.second);
    }

    assert(offset <= _width);

    if (offset < _width)
    {
        // pad with zeros
        values.push_back(circt::hw::ConstantOp::create(_opb, _location, GetIntegerType(_width - offset), 0));
    }

    // Reverse the elements in the array because ConcatOp expects MSB first
    std::reverse(values.begin(), values.end());

    circt::comb::ConcatOp concatOp = circt::comb::ConcatOp::create(_opb, _location, values);

    assert(GetMlirValueWidth(concatOp) == _width);

    return concatOp;
}

void BatchAssignments::Append(const mlir::Location& location, const mlir::Value& dstValue, const mlir::Value& srcValue)
{
    _assignments.push_back({location, dstValue, srcValue});
}

void BatchAssignments::AppendVerbatimDst(const mlir::Location& location, const std::string& dstString,
                                         const mlir::Value& srcValue)
{
    _verbatimAssignments.push_back({location, dstString, srcValue});
}

void BatchAssignments::Flush(circt::OpBuilder& opb, const mlir::Location& location)
{
    if (Empty())
    {
        return;
    }

    circt::sv::AlwaysCombOp alwaysComb = circt::sv::AlwaysCombOp::create(opb, location);

    circt::OpBuilder::InsertionGuard g(opb);

    opb.setInsertionPointToStart(alwaysComb.getBodyBlock());

    for (const AssignRecord& ar : _assignments)
    {
        circt::sv::BPAssignOp::create(opb, ar._location, ar._dstValue, ar._srcValue);
    }

    for (const VerbatimAssignRecord& ar : _verbatimAssignments)
    {
        mlir::SmallVector<mlir::Value> substitutions(1, ar._srcValue);

        const std::string str = ar._dstString + " = {{0}};";

        mlir::SmallVector<mlir::Attribute> attributes;

        circt::sv::VerbatimOp::create(opb, ar._location, StringToStringAttr(str), substitutions,
                                          opb.getArrayAttr(attributes));
    }

    _assignments.clear();
    _verbatimAssignments.clear();
}

bool BatchAssignments::Empty() const { return _assignments.empty() && _verbatimAssignments.empty(); }

void AccumulateOutputPortUpdates::Accumulate(const size_t outputRegisterIndex, const mlir::Value& valueToWrite,
                                             const size_t offset, const size_t width)
{
    const size_t valueWidth = GetMlirValueWidth(valueToWrite);
    assert(valueWidth == width);

    const AccumulateRecord ar = {offset, width, valueToWrite};

    _updates[outputRegisterIndex].push_back(ar);
}

void AccumulateOutputPortUpdates::Flush(circt::OpBuilder& opb, const mlir::Location& location,
                                        const size_t outputRegisterIndex, const mlir::Value portSsaValue,
                                        BatchAssignments& batchAssignments)
{
    const auto it = _updates.find(outputRegisterIndex);
    if (it == _updates.end())
    {
        return;
    }

    const size_t portWidth =
        GetMlirTypeWidth(mlir::cast<circt::hw::InOutType>(portSsaValue.getType()).getElementType());

    // Sort the accumulate records by offset so that (smallest to largest)
    Value v = it->second;

    SparseConcat sparseConcat(opb, location, portWidth);

    for (const AccumulateRecord& ar : v)
    {
        sparseConcat.Insert(ar._offset, ar._value);
    }

    mlir::Value concatValue = sparseConcat.Flush();

    assert(GetMlirValueWidth(concatValue) == portWidth);

    batchAssignments.Append(location, portSsaValue, concatValue);

    _updates.erase(it);
}

size_t GetMlirTypeWidth(const mlir::Type& type)
{
    size_t result = 0;

    if (llvm::isa<circt::hw::StructType>(type))
    {
        const circt::hw::StructType structType = llvm::cast<circt::hw::StructType>(type);

        for (const circt::hw::StructType::FieldInfo& field : structType.getElements())
        {
            result += GetMlirTypeWidth(field.type);
        }
    }
    else if (llvm::isa<circt::hw::ArrayType>(type))
    {
        const circt::hw::ArrayType arrayType = llvm::cast<circt::hw::ArrayType>(type);

        result = GetMlirTypeWidth(arrayType.getElementType()) * arrayType.getNumElements();
    }
    else if (llvm::isa<circt::hw::TypeAliasType>(type))
    {
        return GetMlirTypeWidth(llvm::cast<circt::hw::TypeAliasType>(type).getInnerType());
    }
    else
    {
        result = type.getIntOrFloatBitWidth();
    }

    return result;
}

size_t GetMlirValueWidth(const mlir::Value& v) { return GetMlirTypeWidth(v.getType()); }

void SetLoweringOperations(mlir::ModuleOp& moduleOp)
{
    circt::LoweringOptions loweringOptions;

    loweringOptions.omitVersionComment = true;
    loweringOptions.emitWireInPorts = true;
    // Removing local variables makes debugging on the generated SystemVerilog easier
    loweringOptions.disallowLocalVariables = true;
    // Make some lint tools happy
    loweringOptions.explicitBitcast = true;
    // Disable port declaration sharing
    loweringOptions.disallowPortDeclSharing = true;

    loweringOptions.setAsAttribute(moduleOp);
}

// In general, ModuleDeclarationHelper::GetInspectableTypeAlias should be prefered
// to avoid duplicating the struct definition in generated code
circt::hw::StructType GetInspectableStructType()
{
    llvm::SmallVector<circt::hw::StructType::FieldInfo> fields;

    const auto addField = [&](const std::string& name, const size_t width)
    {
        fields.push_back(circt::hw::StructType::FieldInfo{StringToStringAttr(name),
                                                          mlir::IntegerType::get(g_compiler->GetMlirContext(), width)});
    };

    addField("retry", 1);
    addField("last_element", 1);
    addField("last_flit", 1);
    addField("valid", 1);
    addField("flit_index", 12);
    addField("element_index", 16);
    addField("variable_index", 16);
    addField("value", 16);

    return circt::hw::StructType::get(g_compiler->GetMlirContext(), fields);
}

std::string GetSVTypeString(mlir::Type type, const std::string& arrayDims)
{
    if (llvm::isa<circt::hw::ArrayType>(type))
    {
        const circt::hw::ArrayType arrayType = llvm::cast<circt::hw::ArrayType>(type);

        const mlir::Type elementType = arrayType.getElementType();

        const size_t elementCount = arrayType.getNumElements();

        return GetSVTypeString(elementType, "[" + std::to_string(elementCount - 1) + ":0]" + arrayDims);
    }
    else if (llvm::isa<circt::hw::TypeAliasType>(type))
    {
        // Emit the name of the type alias
        // not the referenced type
        return llvm::cast<circt::hw::TypeAliasType>(type).getRef().getLeafReference().str() + arrayDims;
    }
    else if (llvm::isa<circt::hw::StructType>(type))
    {
        const circt::hw::StructType structType = llvm::cast<circt::hw::StructType>(type);

        std::string strStruct = "struct packed {";

        for (auto element : structType.getElements())
        {
            strStruct += GetSVTypeString(element.type, "") + " " + element.name.getValue().str() + ";";
        }

        strStruct += "}" + arrayDims;

        return strStruct;
    }
    else if (llvm::isa<circt::hw::UnionType>(type))
    {
        const circt::hw::UnionType unionType = llvm::cast<circt::hw::UnionType>(type);

        // CIRCT treats union as packed union so all elements should have the same width
        // Find the maximum element width and pad 0s for those fields less than the maximum one
        size_t maxWidth = 0;
        for (auto element : unionType.getElements())
        {
            if (maxWidth < GetMlirTypeWidth(element.type))
            {
                maxWidth = GetMlirTypeWidth(element.type);
            }
        }

        std::string strUnion = "union packed {";

        for (auto element : unionType.getElements())
        {
            const std::string elementName = element.name.getValue().str();
            const size_t elementWidth = GetMlirTypeWidth(element.type);
            if (elementWidth == maxWidth)
            {
                strUnion += GetSVTypeString(element.type, "") + " " + elementName + ";";
            }
            else
            {
                const size_t paddingWidth = maxWidth - elementWidth;
                strUnion += "struct packed {logic [" + std::to_string(paddingWidth - 1) + ":0] __post_padding_" +
                            elementName + ";" + GetSVTypeString(element.type, "") + elementName + ";} " + elementName +
                            ";";
            }
        }

        strUnion += "}" + arrayDims;

        return strUnion;
    }
    else
    {
        const size_t width = GetMlirTypeWidth(type);

        return std::string("logic ") + arrayDims + OptionalWidthDeclarationNoSpace(width);
    }
}

VerbatimWriter::VerbatimWriter(circt::OpBuilder& opb, const mlir::Location& location) : _opb(opb), _location(location)
{
}

VerbatimWriter::~VerbatimWriter()
{
    const std::string str = _str.str();

    if (!str.empty())
    {
        mlir::SmallVector<mlir::Attribute> attributes;

        circt::sv::VerbatimOp::create(_opb, _location, StringToStringAttr(str), _substitutions,
                                           _opb.getArrayAttr(attributes));
    }
}

// Convert any build up strings/values into an mlir value of a given type
// then clear internal state
mlir::Value VerbatimWriter::GetExpr(mlir::Type type)
{
    const std::string str = _str.str();
    assert(!str.empty());

    // Clear _str so that a verbatim op is not created in the destructor
    _str.str("");
    _str.clear();

    return circt::sv::VerbatimExprOp::create(_opb, _location, type, str, _substitutions);
}

DisableDynamicAssertsAndTranslateOffCirct::DisableDynamicAssertsAndTranslateOffCirct(circt::OpBuilder& opb,
                                                                                     mlir::Location opLocation)
    : _opb(opb), _location(opLocation)
{
    VerbatimWriter writer(_opb, _location);

    writer << "`ifndef NO_DYNAMIC_ASSERTS\n";
    writer << "//synopsys translate_off\n";
}

DisableDynamicAssertsAndTranslateOffCirct::~DisableDynamicAssertsAndTranslateOffCirct()
{
    VerbatimWriter writer(_opb, _location);

    writer << "//synopsys translate_on\n";
    writer << "`endif\n";
}

ModuleDeclarationHelper::ModuleDeclarationHelper(RedirectableSourceWriter& writer, const std::string& name,
                                                 const mlir::Location& location, const std::string& circtDesignName,
                                                 mlir::ModuleOp* const mlirModule)
    : _name(name), _location(location), _circtDesignName(circtDesignName), _opb(g_compiler->GetMlirContext()),
      _originalWriter(writer), _finished(false), _exportVerilog(mlirModule == nullptr), _bundleStartPortIndex(0)
{
    if (mlirModule)
    {
        _mlirModule = *mlirModule;
    }
    else
    {
        _mlirModule = CreateMlirModuleAndDesign(location, circtDesignName);
    }

    circt::kanagawa::DesignOp designOp = GetDesignOp(_mlirModule);

    _opb.setInsertionPointToEnd(&designOp.getBodyRegion().front());

    // Redirect any strings from _originalWriter into _verbatimBuffer
    _originalWriter.BeginRedirect(_verbatimBuffer);
}

ModuleDeclarationHelper::~ModuleDeclarationHelper()
{
    // EndEsiBundle should have been called
    assert(!_bundleName);
}

void ModuleDeclarationHelper::BeginEsiBundle(const std::string& name)
{
    assert(!_bundleName);

    _bundleName = name;

    _bundleStartPortIndex = _ports.size();
}

void ModuleDeclarationHelper::EndEsiBundle()
{
    assert(_bundleName);

    SafeInsert(_bundleNameToPortRange, *_bundleName, std::pair<size_t, size_t>(_bundleStartPortIndex, _ports.size()));

    _bundleName.reset();
}

mlir::Block* ModuleDeclarationHelper::GetBodyBlock()
{
    // Can only be called after FinishPorts
    assert(_container);

    return _container.getBodyBlock();
}

void ModuleDeclarationHelper::Finish()
{
    // FinishPorts should be called
    assert(_container);

    assert(!_finished);
    _finished = true;

    _originalWriter.EndRedirect();

    // Write out any text in _verbatimBuffer as a circt VerbatimOp
    FlushVerbatimStrings();

    // Write output ports
    for (const PortInfo& pi : _ports)
    {
        if (circt::hw::ModulePort::Direction::Output == pi._hwPortInfo.dir)
        {
            const auto it = _outputValues.find(pi._hwPortInfo.name.str());

            mlir::Value outputValue;

            // If no output value was provided, just fill in zeros
            if (it == _outputValues.end())
            {
                if (Verbosity() > Normal)
                {
                    std::cout << "Zeroing output port: " << pi._hwPortInfo.name.str() << "\n";
                }

                outputValue = GetTypedZeros(_opb, _location, pi._hwPortInfo.type);
            }
            else
            {
                outputValue = it->second;
            }

            circt::kanagawa::PortWriteOp::create(_opb, _location, SafeLookup(_outputPortOps, pi._hwPortInfo.name.str()),
                                                      outputValue);
        }
    }

    // Only generate Verilog if the MlirModule passed to the constructor was null
    // otherwise, the IR was added to that MlirModule, and it will eventually be converted to Verilog
    if (_exportVerilog)
    {
        // Fixup the all of the pipelines to add `pipepline.src`s.
        addPipelineSrcs(_mlirModule);

        if (mlir::failed(mlir::verify(_mlirModule, true)))
        {
            DumpMlirOperation(_mlirModule);

            throw std::runtime_error("CIRCT verification failed");
        }

        SetLoweringOperations(_mlirModule);

        // Verify that TwoState=true for all operations that have a TwoState attribute
        AssertTwoState(_mlirModule);

        mlir::PassManager pm(g_compiler->GetMlirContext());
        circt::kanagawa::loadKanagawaLowLevelPassPipeline(pm);

        std::string generatedModule;
        llvm::raw_string_ostream generatedModuleStr(generatedModule);
        pm.addPass(circt::createExportVerilogPass(generatedModuleStr));

        if (mlir::failed(pm.run(_mlirModule)))
        {
            DumpMlirOperation(_mlirModule);

            throw std::runtime_error("CIRCT pass execution failed");
        }

        _originalWriter.Str() << generatedModule << "\n";
    }
}

void ModuleDeclarationHelper::FlushVerbatimStrings()
{
    // Emit any text that have been placed into _verbatimBuffer
    // as a verbatim operation
    VerbatimWriter verbatimWriter(_opb, _location);

    verbatimWriter << _verbatimBuffer.GetString();

    _verbatimBuffer.Reset();
}

void ModuleDeclarationHelper::AddVerbatimOp(const mlir::Location& location, const VerbatimCallback& callback)
{
    // Flush any strings in _verbatimBuffer
    FlushVerbatimStrings();

    VerbatimWriter vw(_opb, location);

    callback(vw);
}

std::string ModuleDeclarationHelper::Name() const { return _name; }

void ModuleDeclarationHelper::AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction,
                                      const size_t width, const EsiPortSemantics portSemantics,
                                      const EsiChannelSemantics channelSemantics, const EsiChannelName channelName,
                                      const std::string fieldName)
{
    AddPort(name, direction, _opb.getIntegerType(width), nullptr, portSemantics, channelSemantics, channelName,
            fieldName);
}

void ModuleDeclarationHelper::AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction,
                                      const size_t outerWidth, const size_t innerWidth)
{
    AddPort(name, direction,
            circt::hw::ArrayType::get(g_compiler->GetMlirContext(), _opb.getIntegerType(innerWidth),
                                      _opb.getIntegerAttr(_opb.getIntegerType(64), outerWidth)));
}

void ModuleDeclarationHelper::AddPortOptionalArray(const std::string& name,
                                                   const circt::hw::ModulePort::Direction direction,
                                                   const size_t outerWidth, const size_t innerWidth)
{
    assert(outerWidth > 0);

    if (outerWidth > 1)
    {
        AddPort(name, direction, outerWidth, innerWidth);
    }
    else
    {
        AddPort(name, direction, innerWidth);
    }
}

void ModuleDeclarationHelper::AddPort(const std::string& name, const circt::hw::ModulePort::Direction direction,
                                      const mlir::Type type, const Type* origType, const EsiPortSemantics portSemantics,
                                      const EsiChannelSemantics channelSemantics, const EsiChannelName channelName,
                                      const std::string fieldName)
{
    PortInfo pi = {};

    pi._hwPortInfo.name = StringToStringAttr(name);
    pi._hwPortInfo.dir = direction;
    pi._hwPortInfo.type = type;

    pi._esiPortSemantics = EsiPortSemantics::NonEsi;
    pi._esiChannelSemantics = EsiChannelSemantics::NonEsi;
    pi._origType = origType;

    // Only add ESI information if this port is part of a bundle
    if (_bundleName)
    {
        pi._esiPortSemantics = portSemantics;
        pi._esiChannelSemantics = channelSemantics;
        pi._esiChannelName = channelName;
        pi._fieldName = fieldName;
        pi._bundleName = _bundleName;
    }

    _ports.push_back(pi);
}

void ModuleDeclarationHelper::FinishPorts()
{
    // Build mapping of port name to index
    assert(_portNameToIndex.empty());

    for (size_t i = 0; i < _ports.size(); i++)
    {
        const PortInfo& pi = _ports[i];

        SafeInsert(_portNameToIndex, pi._hwPortInfo.name.str(), i);
    }

    // The container has TopLevel = true to avoid prepending the design name to the container name
    _container = circt::kanagawa::ContainerOp::create(_opb, 
        _location, circt::hw::InnerSymAttr::get(_opb.getStringAttr(_name)), true);

    _opb.setInsertionPointToStart(GetBodyBlock());

    // Add ports to the container
    for (const PortInfo& pi : _ports)
    {
        if (pi._hwPortInfo.dir == circt::hw::ModulePort::Direction::Input)
        {
            SafeInsert(_inputPortOps, pi._hwPortInfo.name.str(),
                       static_cast<mlir::Value>(circt::kanagawa::InputPortOp::create(_opb, 
                           _location, GetFullyQualifiedInnerSymAttr(ObjectPath(), pi._hwPortInfo.name.str()),
                           mlir::TypeAttr::get(pi._hwPortInfo.type), pi._hwPortInfo.name)));
        }
        else
        {
            assert(pi._hwPortInfo.dir == circt::hw::ModulePort::Direction::Output);

            SafeInsert(_outputPortOps, pi._hwPortInfo.name.str(),
                       static_cast<mlir::Value>(circt::kanagawa::OutputPortOp::create(_opb, 
                           _location, GetFullyQualifiedInnerSymAttr(ObjectPath(), pi._hwPortInfo.name.str()),
                           mlir::TypeAttr::get(pi._hwPortInfo.type), pi._hwPortInfo.name)));
        }
    }
}

// Like AssignPort, but can be safely called even if portName does not represent an output port name
std::string ModuleDeclarationHelper::AssignPortOptional(const std::string& portName)
{
    const auto it = _portNameToIndex.find(portName);

    if (_portNameToIndex.end() == it)
    {
        return portName;
    }
    else
    {
        const size_t portIndex = it->second;

        const circt::hw::PortInfo portInfo = _ports[portIndex]._hwPortInfo;

        if (portInfo.dir == circt::hw::ModulePort::Direction::Output)
        {
            return AssignPort(portName);
        }
        else
        {
            // Do nothing for input ports
            return portName;
        }
    }
}

// Set the value of an output port to a verbatim string
// Returns the name of a new net that the verbatim code should assign to
std::string ModuleDeclarationHelper::AssignPort(const std::string& portName)
{
    const size_t portIndex = SafeLookup(_portNameToIndex, portName);

    const PortInfo& portInfo = _ports[portIndex];

    assert(portInfo._hwPortInfo.dir == circt::hw::ModulePort::Direction::Output);

    // A net, which verbatim code can assign to
    const std::string newNetName = portInfo._hwPortInfo.name.str() + "_net";

    // This method is idempotent
    // To enable callers to write to subsets of the net
    if (_outputValues.end() == _outputValues.find(portName))
    {
        const llvm::SmallVector<mlir::Value> substitutions; // Empty, there are no substitutions

        const mlir::SmallVector<mlir::Attribute> attributes; // Empty, there are no attributes

        // Add a verbatim op to the start of the container
        // which declares a new net that other verbatim code will use
        {
            circt::OpBuilder::InsertionGuard g(_opb);
            const std::string declaration = GetSVTypeString(portInfo._hwPortInfo.type, "") + " " + newNetName + ";";

            _opb.setInsertionPointToStart(GetBodyBlock());

            circt::sv::VerbatimOp::create(_opb, _location, StringToStringAttr(declaration), substitutions,
                                               _opb.getArrayAttr(attributes));
        }

        // Add a verbatim op which evaluates to the value of the new net
        // Record that the output port should be assigned to it
        circt::sv::VerbatimExprOp verbatimOp = circt::sv::VerbatimExprOp::create(_opb, 
            _location, portInfo._hwPortInfo.type, StringToStringAttr(newNetName), substitutions, nullptr);

        SafeInsert(_outputValues, portName, verbatimOp.getResult());
    }

    return newNetName;
}

static const std::string InspectableValueName("InspectableValueT");

void ModuleDeclarationHelper::AddTypedefs(const std::string& typeScopeName)
{
    {
        // Add a type container to the mlir module
        circt::OpBuilder::InsertionGuard g(_opb);

        _opb.setInsertionPointToStart(&_mlirModule.getBodyRegion().front());

        _typeScopeOp = circt::hw::TypeScopeOp::create(_opb, _location, StringToStringAttr(typeScopeName));
    }

    // Add the one and only block to the type container
    _typeScopeOp.getBodyRegion().emplaceBlock();

    {
        circt::OpBuilder::InsertionGuard g(_opb);

        _opb.setInsertionPointToStart(_typeScopeOp.getBodyBlock());

        if (GetCodeGenConfig()._inspection)
        {
            circt::hw::TypedeclOp::create(_opb, _location, StringToStringAttr(InspectableValueName),
                                            GetInspectableStructType(), StringToStringAttr(InspectableValueName));
        }
    }

    if (GetCodeGenConfig()._inspection)
    {
        AssertStructsMatch(GetInspectableTypeAlias(), "KanagawaTypes::InspectableValue");
    }
}

// Emit assertions to check that a struct defined in the CIRCT IR
// matches a struct from hand-written RTL
void ModuleDeclarationHelper::AssertStructsMatch(const mlir::Type& circtTypeAlias, const std::string& otherStructName)
{
    assert(llvm::isa<circt::hw::TypeAliasType>(circtTypeAlias));

    circt::hw::TypeAliasType typeAliasType = llvm::cast<circt::hw::TypeAliasType>(circtTypeAlias);

    circt::hw::StructType circtType = llvm::cast<circt::hw::StructType>(typeAliasType.getInnerType());

    const std::string circtTypeAliasName = typeAliasType.getRef().getLeafReference().str();

    const size_t structWidth = GetMlirTypeWidth(circtType);

    DisableTranslation disableTranslation(_verbatimBuffer);

    _verbatimBuffer.Str() << "initial begin";

    {
        AutoIndent autoIndent(_verbatimBuffer);

        _verbatimBuffer.Str() << circtTypeAliasName << " a;";
        _verbatimBuffer.Str() << otherStructName << " b;";

        for (const circt::hw::StructType::FieldInfo& field : circtType.getElements())
        {
            const std::string fieldName = field.name.str();

            _verbatimBuffer.Str() << "a = '0; a." << fieldName << " = '1;";

            _verbatimBuffer.Str() << "b = '0; b." << fieldName << " = '1;";

            _verbatimBuffer.Str() << "assert(" << structWidth << "'(a) == " << structWidth
                                  << "'(b)) else $fatal(1, \"Struct type mismatch: " + circtTypeAliasName + "\");";
        }
    }

    _verbatimBuffer.Str() << "end";
}

mlir::Type ModuleDeclarationHelper::GetTypeAlias(const std::string& name, const mlir::Type& referencedType)
{
    // AddTypedefs must be called first
    assert(_typeScopeOp);

    mlir::SymbolRefAttr symbolRefAttr =
        mlir::SymbolRefAttr::get(_typeScopeOp.getSymNameAttr(), mlir::FlatSymbolRefAttr::get(StringToStringAttr(name)));

    return circt::hw::TypeAliasType::get(symbolRefAttr, referencedType);
}

mlir::Type ModuleDeclarationHelper::GetInspectableTypeAlias()
{
    assert(GetCodeGenConfig()._inspection);
    return GetTypeAlias(InspectableValueName, GetInspectableStructType());
}

mlir::ModuleOp ModuleDeclarationHelper::MlirModule() { return _mlirModule; }

circt::OpBuilder& ModuleDeclarationHelper::OpBuilder() { return _opb; }

circt::kanagawa::ContainerOp ModuleDeclarationHelper::Container()
{
    assert(_container);
    return _container;
}

// Emit a container that wraps the one created by this object
// and uses ESI channels and bundles to model function calls
void ModuleDeclarationHelper::EmitEsiWrapper(const std::string& circtDesignName)
{
    // point _obp at the design op
    circt::OpBuilder::InsertionGuard g(_opb);

    circt::kanagawa::DesignOp designOp = GetDesignOp(_mlirModule);

    _opb.setInsertionPointToEnd(&designOp.getBodyRegion().front());

    const std::string wrapperName = _name + "_EsiWrapper";

    const std::string innerContainerInstance = "EsiWrapped";

    const std::string innerContainerSymbol = "EsiWrappedSymbol";

    const auto getPortSymbol = [](const std::string& portName)
    { return circt::hw::InnerSymAttr::get(StringToStringAttr(portName)); };

    // Create a container
    circt::kanagawa::ContainerOp wrapperContainer = circt::kanagawa::ContainerOp::create(_opb, 
        _location, circt::hw::InnerSymAttr::get(StringToStringAttr(wrapperName)), true);

    _opb.setInsertionPointToStart(wrapperContainer.getBodyBlock());

    // Instantiate the inner container
    circt::kanagawa::ContainerInstanceOp::create(_opb, 
        _location, circt::hw::InnerSymAttr::get(StringToStringAttr(innerContainerInstance)),
        circt::hw::InnerRefAttr::get(StringToStringAttr(circtDesignName), StringToStringAttr(_name)));

    // Get path from the wrapper container to the inner container
    llvm::SmallVector<mlir::Attribute> steps;

    steps.push_back(
        circt::kanagawa::PathStepAttr::get(g_compiler->GetMlirContext(), circt::kanagawa::PathDirection::Child,
                                           circt::kanagawa::ScopeRefType::get(g_compiler->GetMlirContext()),
                                           mlir::FlatSymbolRefAttr::get(StringToStringAttr(innerContainerInstance))));

    circt::kanagawa::PathOp pathToContainer = circt::kanagawa::PathOp::create(_opb, _location, _opb.getArrayAttr(steps));

    // Directly connect input and output ports which are not for function calls
    // and function calls which are not ESI compatible (fixed-latency, no backpressure)
    for (const PortInfo& pi : _ports)
    {
        // Port and channel semantics should agree about if they are for an ESI interface or not
        assert((pi._esiChannelSemantics == EsiChannelSemantics::NonEsi) ==
               (pi._esiPortSemantics == EsiPortSemantics::NonEsi));

        if (pi._esiPortSemantics == EsiPortSemantics::NonEsi)
        {
            if (pi._hwPortInfo.dir == circt::hw::ModulePort::Direction::Input)
            {
                circt::kanagawa::InputPortOp inputPort = circt::kanagawa::InputPortOp::create(_opb, 
                    _location, getPortSymbol(pi._hwPortInfo.name.str()), mlir::TypeAttr::get(pi._hwPortInfo.type),
                    pi._hwPortInfo.name);

                mlir::Value inputValue = circt::kanagawa::PortReadOp::create(_opb, _location, inputPort);

                WriteContainerPort(_opb, _location, pathToContainer,
                                   GetFullyQualifiedStringAttr(ObjectPath(), pi._hwPortInfo.name.str()),
                                   pi._hwPortInfo.type, inputValue);
            }
            else
            {
                assert(pi._hwPortInfo.dir == circt::hw::ModulePort::Direction::Output);

                circt::kanagawa::OutputPortOp outputPort = circt::kanagawa::OutputPortOp::create(_opb, 
                    _location, getPortSymbol(pi._hwPortInfo.name.str()), mlir::TypeAttr::get(pi._hwPortInfo.type),
                    pi._hwPortInfo.name);

                mlir::Value outputValue = ReadContainerPort(
                    _opb, _location, pathToContainer,
                    GetFullyQualifiedStringAttr(ObjectPath(), pi._hwPortInfo.name.str()), pi._hwPortInfo.type);

                circt::kanagawa::PortWriteOp::create(_opb, _location, outputPort, outputValue);
            }
        }
    }

    // Handle wrapper ports which are for function calls
    for (const auto& bundleInfo : _bundleNameToPortRange)
    {
        const std::string bundleName = bundleInfo.first;

        const size_t startPortIndex = bundleInfo.second.first;
        const size_t endPortIndex = bundleInfo.second.second;

        // Determine channel and bundle types
        std::array<circt::esi::ChannelType, 2> directionToChannelType = {};
        std::array<bool, 2> directionToChannelExists = {};
        std::array<llvm::SmallVector<mlir::Type>, 2> directionToPayloadTypes;
        std::array<llvm::SmallVector<std::string>, 2> directionToPayloadNames;
        std::array<EsiChannelName, 2> directionToChannelName;
        llvm::SmallVector<circt::esi::BundledChannel> bundleChannelDesc;

        for (size_t channelDirection = 0; channelDirection < 2; channelDirection++)
        {
            const EsiChannelSemantics channelSemantics =
                (channelDirection == 0) ? EsiChannelSemantics::FromGeneratedHw : EsiChannelSemantics::ToGeneratedHw;

            bool channelExists = false;

            circt::esi::ChannelSignaling signaling = circt::esi::ChannelSignaling::ValidReady;

            EsiChannelName channelName = EsiChannelName::Undefined;

            llvm::SmallVector<mlir::Type> bundlePayloadTypes;
            llvm::SmallVector<mlir::Type> payloadTypes;
            llvm::SmallVector<std::string> payloadNames;
            llvm::SmallVector<std::string> payloadFieldNames;

            for (size_t portIndex = startPortIndex; portIndex < endPortIndex; portIndex++)
            {
                const PortInfo& portInfo = _ports[portIndex];

                assert(portInfo._esiChannelSemantics != EsiChannelSemantics::NonEsi);

                if (portInfo._esiChannelSemantics == channelSemantics)
                {
                    channelExists = true;

                    channelName = portInfo._esiChannelName;

                    switch (portInfo._esiPortSemantics)
                    {
                    case EsiPortSemantics::Valid:
                    case EsiPortSemantics::Ready:
                        signaling = circt::esi::ChannelSignaling::ValidReady;
                        break;

                    case EsiPortSemantics::ReadEnable:
                    case EsiPortSemantics::Empty:
                        signaling = circt::esi::ChannelSignaling::FIFO;
                        break;

                    case EsiPortSemantics::Payload:
                        bundlePayloadTypes.push_back(ToMlirType(portInfo._origType, true));
                        payloadTypes.push_back(portInfo._hwPortInfo.type);
                        payloadNames.push_back(portInfo._hwPortInfo.name.str());
                        payloadFieldNames.push_back(portInfo._fieldName);
                        break;

                    default:
                        assert(false);
                    }
                }
            }

            directionToChannelExists[channelDirection] = channelExists;

            if (channelExists)
            {
                mlir::Type channelPayloadType;

                if (payloadTypes.empty())
                {
                    channelPayloadType = GetIntegerType(0);
                }
                else if (channelName == EsiChannelName::Results)
                {
                    // Functions can have at most 1 return value
                    assert(payloadTypes.size() == 1);

                    channelPayloadType = bundlePayloadTypes[0];
                }
                else
                {
                    assert(channelName == EsiChannelName::Arguments);

                    // Arguments alre always packed into a struct
                    llvm::SmallVector<circt::hw::StructType::FieldInfo> fields;

                    assert(payloadNames.size() == payloadTypes.size());
                    assert(payloadFieldNames.size() == payloadTypes.size());

                    for (size_t i = 0; i < bundlePayloadTypes.size(); i++)
                    {
                        fields.push_back(circt::hw::StructType::FieldInfo{StringToStringAttr(payloadFieldNames[i]),
                                                                          bundlePayloadTypes[i]});
                    }

                    channelPayloadType = circt::hw::StructType::get(g_compiler->GetMlirContext(), fields);
                }

                circt::esi::ChannelType channelType =
                    circt::esi::ChannelType::get(g_compiler->GetMlirContext(), channelPayloadType, signaling, 0);

                directionToChannelType[channelDirection] = channelType;
                directionToPayloadTypes[channelDirection] = payloadTypes;
                directionToPayloadNames[channelDirection] = payloadNames;
                directionToChannelName[channelDirection] = channelName;

                assert(channelName != EsiChannelName::Undefined);

                circt::esi::BundledChannel bc = {};
                bc.name = (channelName == EsiChannelName::Arguments) ? StringToStringAttr("arg")
                                                                     : StringToStringAttr("result");
                bc.direction = (channelSemantics == EsiChannelSemantics::FromGeneratedHw)
                                   ? circt::esi::ChannelDirection::from
                                   : circt::esi::ChannelDirection::to;
                bc.type = channelType;

                bundleChannelDesc.push_back(bc);
            }
        }

        assert(!bundleChannelDesc.empty());

        circt::esi::ChannelBundleType bundleType =
            circt::esi::ChannelBundleType::get(g_compiler->GetMlirContext(), bundleChannelDesc, nullptr);

        // Declare a port on the container with bundle type
        circt::kanagawa::InputPortOp inputBundlePort = circt::kanagawa::InputPortOp::create(_opb, 
            _location, getPortSymbol(bundleName), mlir::TypeAttr::get(bundleType), StringToStringAttr(bundleName));

        // PortReadOp to get the bundle
        mlir::Value inputBundle = circt::kanagawa::PortReadOp::create(_opb, _location, inputBundlePort);

        // For each channel in the bundle
        // It is important to handle the FromGeneratedHw channel first
        // UnpackBundleOp takes that channel as input
        llvm::SmallVector<mlir::Value> fromChannels;
        llvm::SmallVector<mlir::Value> toChannels;

        for (size_t channelDirection = 0; channelDirection < 2; channelDirection++)
        {
            const EsiChannelSemantics channelSemantics =
                (channelDirection == 0) ? EsiChannelSemantics::FromGeneratedHw : EsiChannelSemantics::ToGeneratedHw;

            if (channelDirection == 1)
            {
                // Unpack the bundle to get the ToGeneratedHw channel
                circt::esi::UnpackBundleOp unpackBundleOp =
                    circt::esi::UnpackBundleOp::create(_opb, _location, inputBundle, fromChannels);

                toChannels = unpackBundleOp.getToChannels();
            }

            // Collect information about the channel
            if (directionToChannelExists[channelDirection])
            {
                circt::esi::ChannelSignaling signaling = directionToChannelType[channelDirection].getSignaling();

                std::string validName;
                std::string rdenName;
                mlir::Value ready;
                mlir::Value empty;
                std::vector<mlir::Value> payload;

                for (size_t portIndex = startPortIndex; portIndex < endPortIndex; portIndex++)
                {
                    const PortInfo& portInfo = _ports[portIndex];

                    assert(portInfo._esiChannelSemantics != EsiChannelSemantics::NonEsi);

                    if (portInfo._esiChannelSemantics == channelSemantics)
                    {
                        switch (portInfo._esiPortSemantics)
                        {
                        case EsiPortSemantics::Valid:
                            validName = portInfo._hwPortInfo.name.str();
                            break;

                        case EsiPortSemantics::Ready:
                            ready = ReadContainerPort(
                                _opb, _location, pathToContainer,
                                GetFullyQualifiedStringAttr(ObjectPath(), portInfo._hwPortInfo.name.str()),
                                portInfo._hwPortInfo.type);
                            break;

                        case EsiPortSemantics::ReadEnable:
                            rdenName = portInfo._hwPortInfo.name.str();
                            break;

                        case EsiPortSemantics::Empty:
                            empty = ReadContainerPort(
                                _opb, _location, pathToContainer,
                                GetFullyQualifiedStringAttr(ObjectPath(), portInfo._hwPortInfo.name.str()),
                                portInfo._hwPortInfo.type);
                            break;

                        case EsiPortSemantics::Payload:
                            if (channelSemantics == EsiChannelSemantics::FromGeneratedHw)
                            {
                                payload.push_back(ReadContainerPort(
                                    _opb, _location, pathToContainer,
                                    GetFullyQualifiedStringAttr(ObjectPath(), portInfo._hwPortInfo.name.str()),
                                    portInfo._hwPortInfo.type, ToMlirType(portInfo._origType, true)));
                            }
                            break;

                        default:
                            assert(false);
                        }
                    }
                }

                if (channelSemantics == EsiChannelSemantics::ToGeneratedHw)
                {
                    // The UnpackBundleOp generates the channel
                    assert(1 == toChannels.size());
                    mlir::Value inputChannel = toChannels[0];

                    if (circt::esi::ChannelSignaling::ValidReady == signaling)
                    {
                        circt::esi::UnwrapValidReadyOp unwrapOp =
                            circt::esi::UnwrapValidReadyOp::create(_opb, _location, inputChannel, ready);

                        const llvm::SmallVector<mlir::Type>& payloadTypes = directionToPayloadTypes[channelDirection];
                        const llvm::SmallVector<std::string>& payloadNames = directionToPayloadNames[channelDirection];

                        // Write data (if it exists) and valid on the wrapped container
                        if (payloadTypes.size() > 0)
                        {
                            if (directionToChannelName[channelDirection] == EsiChannelName::Results)
                            {
                                assert(payloadTypes.size() == 1);

                                WriteContainerPort(_opb, _location, pathToContainer,
                                                   GetFullyQualifiedStringAttr(ObjectPath(), payloadNames[0]),
                                                   payloadTypes[0], unwrapOp.getRawOutput());
                            }
                            else
                            {
                                assert(directionToChannelName[channelDirection] == EsiChannelName::Arguments);

                                circt::hw::StructExplodeOp explodeOp =
                                    circt::hw::StructExplodeOp::create(_opb, _location, unwrapOp.getRawOutput());

                                assert(payloadNames.size() == payloadTypes.size());

                                for (size_t i = 0; i < payloadTypes.size(); i++)
                                {
                                    WriteContainerPort(_opb, _location, pathToContainer,
                                                       GetFullyQualifiedStringAttr(ObjectPath(), payloadNames[i]),
                                                       payloadTypes[i], explodeOp.getResult()[i]);
                                }
                            }
                        }

                        WriteContainerPort(_opb, _location, pathToContainer,
                                           GetFullyQualifiedStringAttr(ObjectPath(), validName), GetI1Type(),
                                           unwrapOp.getValid());
                    }
                    else
                    {
                        assert(false);
                    }
                }
                else
                {
                    assert(channelSemantics == EsiChannelSemantics::FromGeneratedHw);

                    // Declare a port on the container with channel type
                    mlir::Value outputChannel;

                    if (circt::esi::ChannelSignaling::FIFO == signaling)
                    {
                        circt::esi::ChannelType channelType = directionToChannelType[channelDirection];

                        mlir::Value wrapPayload;

                        if (payload.empty())
                        {
                            // Channel payload is i0
                            // Lowering will remove the i0 ports
                            wrapPayload = circt::hw::ConstantOp::create(_opb, _location,
                                                                             _opb.getIntegerAttr(GetIntegerType(0), 0));
                        }
                        else if (directionToChannelName[channelDirection] == EsiChannelName::Results)
                        {
                            wrapPayload = payload[0];
                        }
                        else
                        {
                            assert(directionToChannelName[channelDirection] == EsiChannelName::Arguments);

                            wrapPayload =
                                circt::hw::StructCreateOp::create(_opb, _location, channelType.getInner(), payload);
                        }

                        circt::esi::WrapFIFOOp wrapOp = circt::esi::WrapFIFOOp::create(_opb, 
                            _location, channelType, GetI1Type(), wrapPayload, empty);

                        outputChannel = wrapOp.getChanOutput();

                        // Write rden on the wrapped container
                        WriteContainerPort(_opb, _location, pathToContainer,
                                           GetFullyQualifiedStringAttr(ObjectPath(), rdenName), GetI1Type(),
                                           wrapOp.getRden());
                    }
                    else
                    {
                        assert(false);
                    }

                    fromChannels.push_back(outputChannel);
                }
            }
        }
    }
}

mlir::Value ModuleDeclarationHelper::GetPort(circt::OpBuilder& opb, const ObjectPath& srcPath,
                                             const ObjectPath& dstPath, const mlir::StringAttr portSymbol,
                                             const circt::kanagawa::Direction portDirection, const mlir::Type portType,
                                             const std::string& finalTypeName, const std::string& circtDesignName)
{
    // Find the common root between the 2 paths
    ObjectPath commonRoot;

    const size_t maxLength = std::max(srcPath.size(), dstPath.size());

    for (size_t i = 0; i < maxLength; i++)
    {
        if ((i < srcPath.size()) && (i < dstPath.size()) && (srcPath[i] == dstPath[i]))
        {
            commonRoot.push_back(srcPath[i]);
        }
        else
        {
            break;
        }
    }

    llvm::SmallVector<mlir::Attribute> steps;

    // Step up from srcPath to the common root
    ObjectPath currPath = srcPath;

    while (currPath != commonRoot)
    {
        const mlir::FlatSymbolRefAttr symbolRef = mlir::FlatSymbolRefAttr::get(StringToStringAttr(currPath.back()));

        const mlir::Attribute step = circt::kanagawa::PathStepAttr::get(
            g_compiler->GetMlirContext(), circt::kanagawa::PathDirection::Parent,
            circt::kanagawa::ScopeRefType::get(g_compiler->GetMlirContext()), symbolRef);

        steps.push_back(step);

        currPath.pop_back();
    }

    // Step down from common root to dstPath
    while (currPath != dstPath)
    {
        assert(currPath.size() < dstPath.size());

        const std::string stepName = dstPath[currPath.size()];

        const mlir::FlatSymbolRefAttr symbolRef = mlir::FlatSymbolRefAttr::get(StringToStringAttr(stepName));

        const mlir::Attribute step = circt::kanagawa::PathStepAttr::get(
            g_compiler->GetMlirContext(), circt::kanagawa::PathDirection::Child,
            circt::kanagawa::ScopeRefType::get(g_compiler->GetMlirContext()), symbolRef);

        steps.push_back(step);

        currPath.push_back(stepName);
    }

    if (steps.empty())
    {
        // srcPath and dstPath match
        // the desired port is a port of _container
        if (circt::kanagawa::Direction::Input == portDirection)
        {
            return SafeLookup(_inputPortOps, portSymbol.str());
        }
        else
        {
            assert(circt::kanagawa::Direction::Output == portDirection);
            return SafeLookup(_outputPortOps, portSymbol.str());
        }
    }
    else
    {
        // Add an explicit type name to the final step
        circt::kanagawa::PathStepAttr lastStep = llvm::cast<circt::kanagawa::PathStepAttr>(steps.back());

        steps.back() = circt::kanagawa::PathStepAttr::get(
            g_compiler->GetMlirContext(), lastStep.getDirection(),
            circt::kanagawa::ScopeRefType::get(
                g_compiler->GetMlirContext(),
                circt::hw::InnerRefAttr::get(StringToStringAttr(circtDesignName), StringToStringAttr(finalTypeName))),
            lastStep.getChild());

        circt::kanagawa::PathOp path = circt::kanagawa::PathOp::create(opb, _location, opb.getArrayAttr(steps));

        return circt::kanagawa::GetPortOp::create(opb, _location, path, portSymbol, portType, portDirection);
    }
}

void ModuleDeclarationHelper::WritePort(circt::OpBuilder& opb, const ObjectPath& srcPath, const ObjectPath& dstPath,
                                        const mlir::StringAttr portSymbol, const std::string& finalTypeName,
                                        const std::string& circtDesignName, mlir::Value value)
{
    mlir::Value port = GetPort(opb, srcPath, dstPath, portSymbol, circt::kanagawa::Direction::Input, value.getType(),
                               finalTypeName, circtDesignName);

    circt::kanagawa::PortWriteOp::create(opb, _location, port, value);
}

mlir::Value ModuleDeclarationHelper::ReadPort(circt::OpBuilder& opb, const ObjectPath& srcPath,
                                              const ObjectPath& dstPath, const mlir::StringAttr portSymbol,
                                              const std::string& finalTypeName, const std::string& circtDesignName,
                                              const mlir::Type type)
{
    mlir::Value port = GetPort(opb, srcPath, dstPath, portSymbol, circt::kanagawa::Direction::Output, type,
                               finalTypeName, circtDesignName);

    return circt::kanagawa::PortReadOp::create(opb, _location, port);
}

TriggeredOpHelper::TriggeredOpHelper(circt::OpBuilder& opb, circt::pipeline::ScheduledPipelineOp& scheduledPipelineOp,
                                     const SourceOperandToMlirValueCb& sourceOperandToMlirValue, const Program& program,
                                     const mlir::Value enableSignal)
    : _opb(opb), _scheduledPipelineOp(scheduledPipelineOp), _sourceOperandToMlirValue(sourceOperandToMlirValue),
      _program(program), _doneAddingOps(false)
{
    const size_t enableSignalIndex = AddSignal(enableSignal);
    assert(0 == enableSignalIndex);
}

TriggeredOpHelper::~TriggeredOpHelper() { _opb.restoreInsertionPoint(_insertionPoint); }

void TriggeredOpHelper::AddOp(const Operation& op) { _ops.push_back(&op); }

// Add a value which can be read from within the triggered op
// Returns an index which can be passed to GetSignal
size_t TriggeredOpHelper::AddSignal(const mlir::Value& value)
{
    assert(!_doneAddingOps);

    const size_t result = _signals.size();

    _signals.push_back(value);

    return result;
}

mlir::Value TriggeredOpHelper::GetEnableSignal() { return _triggeredOp.getBodyBlock()->getArgument(0); }

// Operations created by the operation builder after this function returns
// are placed within the triggered region
void TriggeredOpHelper::DoneAddingOps(const circt::hw::EventControl triggerCondition, const mlir::Value clock)
{
    assert(!_doneAddingOps);

    if (!_ops.empty())
    {
        // Make all source operands visible
        for (const Operation* const op : _ops)
        {
            SafeInsert(_opToStartIndex, op, _signals.size());

            for (size_t i = 0; i < op->_src.size(); i++)
            {
                if (SourceOperandType::StringLiteral == op->_src[i].Type())
                {
                    // GetSourceOperand will not be called on this value
                    // add it just to take up a slot for easier indexing
                    mlir::Value emptyValue = GetTypedZeros(_opb, GetUnknownLocation(), GetI1Type());
                    AddSignal(emptyValue);
                }
                else
                {
                    AddSignal(_sourceOperandToMlirValue(*op, i, op->_src[i].Width(_program)));
                }
            }
        }

        _triggeredOp = circt::hw::TriggeredOp::create(_opb, 
            GetUnknownLocation(), circt::hw::EventControlAttr::get(g_compiler->GetMlirContext(), triggerCondition),
            clock, _signals);

        _insertionPoint = _opb.saveInsertionPoint();

        _opb.setInsertionPointToEnd(_triggeredOp.getBodyBlock());
    }

    _doneAddingOps = true;
}

// Returns a value which can be used inside of the triggered region
// corresponding to a source operand of the operation
mlir::Value TriggeredOpHelper::GetSourceOperand(const Operation& op, const size_t operandIndex)
{
    assert(_doneAddingOps);

    const size_t startIndex = SafeLookup(_opToStartIndex, &op);

    return _triggeredOp.getBodyBlock()->getArgument(startIndex + operandIndex);
}

mlir::Value TriggeredOpHelper::GetSourceOperand(const Operation& op, const size_t operandIndex,
                                                const size_t desiredWidth)
{
    return AdjustValueWidth(GetSourceOperand(op, operandIndex), desiredWidth, op.ShouldSignExtend(operandIndex), _opb,
                            OperationToCirctLocation(op, _program));
}

void DumpMlirOperation(mlir::Operation* const op)
{
    // Flush any buffered data in std::cout
    // before calling CIRCT to print to stdout
    std::cout << std::flush;

    mlir::OpPrintingFlags opPrintingFlag = {};
    opPrintingFlag.enableDebugInfo();
    op->print(llvm::outs(), opPrintingFlag);

    // Flush CIRCT-buffered data before
    // future calls to std::cout
    llvm::outs().flush();
}

// Sets the sv.namehint attribute on the operation which defines a given value
// to match the name of a register
void AttachNameHintToValue(const mlir::Value value, const AccessedRegister accessedRegister, const Program& program)
{
    // Only proceed if the defining operation can be located
    // and it does not already have a name hint
    mlir::Operation* const definingOp = value.getDefiningOp();

    const std::string attributeName("sv.namehint");

    if (definingOp && !definingOp->hasAttr(attributeName))
    {
        const RegisterDescription& regDesc = program._registerTable[accessedRegister._registerIndex];

        assert(!regDesc._name.empty());

        definingOp->setAttr(attributeName, StringToStringAttr(regDesc._name));

        // Recursively apply the name to all unnamed sources
        // This is useful for cases where 1 input IR operation maps to multiple
        // CIRCT operations
        // If there are multiple operations with the same sv.namehint
        // then registers/wires in the generated RTL will have integer suffixes
        for (unsigned i = 0; i < definingOp->getNumOperands(); i++)
        {
            AttachNameHintToValue(definingOp->getOperand(i), accessedRegister, program);
        }
    }
}

LatencyOpHelper::LatencyOpHelper(const mlir::Location location, circt::OpBuilder& opb,
                                 const llvm::SmallVector<mlir::Value>& values, const size_t latency)
{
    if (latency > 0)
    {
        llvm::SmallVector<mlir::Type> types;

        for (const mlir::Value v : values)
        {
            types.push_back(v.getType());
        }

        circt::pipeline::LatencyOp latencyOp =
            circt::pipeline::LatencyOp::create(opb, location, types, opb.getIntegerAttr(opb.getI32Type(), latency));

        latencyOp.getBody().emplaceBlock();

        {
            circt::OpBuilder::InsertionGuard g(opb);

            opb.setInsertionPointToStart(latencyOp.getBodyBlock());

            circt::pipeline::LatencyReturnOp::create(opb, location, values);
        }

        for (size_t i = 0; i < values.size(); i++)
        {
            _results.push_back(latencyOp.getResult(i));
        }
    }
    else
    {
        for (size_t i = 0; i < values.size(); i++)
        {
            _results.push_back(values[i]);
        }
    }
}

mlir::Value LatencyOpHelper::GetResult(const size_t index) { return _results[index]; }

// Checks that all operations with the TwoState attribute
// have TwoState=true.
// This is helpful because many operation builders have a default value of TwoState=false
// so it easy miss the fact that TwoState should be specified.
void AssertTwoState(mlir::ModuleOp& moduleOp)
{
    const auto callback = [&](mlir::Operation* op, const mlir::WalkStage&)
    {
        if (mlir::isa<circt::comb::AddOp>(op) || mlir::isa<circt::comb::AndOp>(op) ||
            mlir::isa<circt::comb::DivSOp>(op) || mlir::isa<circt::comb::DivUOp>(op) ||
            mlir::isa<circt::comb::ICmpOp>(op) || mlir::isa<circt::comb::ModSOp>(op) ||
            mlir::isa<circt::comb::ModUOp>(op) || mlir::isa<circt::comb::MulOp>(op) ||
            mlir::isa<circt::comb::MuxOp>(op) || mlir::isa<circt::comb::OrOp>(op) ||
            mlir::isa<circt::comb::ParityOp>(op) || mlir::isa<circt::comb::ShlOp>(op) ||
            mlir::isa<circt::comb::ShrSOp>(op) || mlir::isa<circt::comb::ShrUOp>(op) ||
            mlir::isa<circt::comb::SubOp>(op) || mlir::isa<circt::comb::XorOp>(op))
        {
            const std::string name = "twoState";

            // The twoState attribute is a UnitAttr
            // which presence of the attribute in the dictionary
            // means twoState = true
            if (!op->getAttrDictionary().contains(name))
            {
                DumpMlirOperation(op);

                std::cout << "\n";

                throw std::runtime_error("MLIR operation with twoState = false found");
            }
        }
    };

    moduleOp.walk(callback);
}

MlirModule::MlirModule(const std::string& circtDesignName)
{
    _mlirModule = CreateMlirModuleAndDesign(GetUnknownLocation(), circtDesignName);
}

mlir::ModuleOp& MlirModule::Module() { return _mlirModule; }

// Serialize IR to a string and then deserialize
// Just to verify that this is possible
void MlirModule::VerifyRoundTrip()
{
    std::string irString;
    llvm::raw_string_ostream str(irString);

    _mlirModule->print(str, mlir::OpPrintingFlags().enableDebugInfo());

    mlir::MLIRContext context;
    LoadDialects(context);

    mlir::FallbackAsmResourceMap resourceMap;
    mlir::ParserConfig parseConfig(&context, true, &resourceMap);

    mlir::OwningOpRef<mlir::Operation*> parsedModule =
        mlir::parseSourceString<mlir::Operation*>(str.str(), parseConfig);

    if (!parsedModule)
    {
        DumpMlirOperation(_mlirModule);

        throw std::runtime_error("Failed to round-trip CIRCT IR");
    }
}

// Lower IR and export to SystemVerilog in a string
std::string MlirModule::Generate()
{
    if (mlir::failed(mlir::verify(_mlirModule, true)))
    {
        DumpMlirOperation(_mlirModule);
        throw std::runtime_error("CIRCT verification failed");
    }

    SetLoweringOperations(_mlirModule);

    // Verify that TwoState=true for all operations that have a TwoState attribute
    AssertTwoState(_mlirModule);

    mlir::PassManager pm(g_compiler->GetMlirContext());
    // pm.enableTiming();

    // Passes needed to lower basic blocks
    pm.addPass(circt::createSimpleCanonicalizerPass());
    circt::kanagawa::loadKanagawaLowLevelPassPipeline(pm);
    pm.addPass(circt::esi::createESIBundleLoweringPass());
    pm.addPass(circt::esi::createESIPortLoweringPass());
    pm.addPass(circt::esi::createESItoHWPass());
    pm.nest<circt::hw::HWModuleOp>().addPass(circt::pipeline::createExplicitRegsPass());
    // Add power-on values to the pipeline control registers
    circt::PipelineToHWOptions pipelineToHWOptions = {};
    pipelineToHWOptions.enablePowerOnValues = GetCodeGenDeviceConfig()._requirePowerOnReset;
    pm.addPass(circt::createPipelineToHWPass(pipelineToHWOptions));
    pm.addPass(circt::comb::createLowerComb());
    // set lowerToAlwaysFF to false to emit "always" for QuestaSim to compile when power-on value is enabled
    circt::LowerSeqToSVOptions lowerSeqToSVOptions = {};
    lowerSeqToSVOptions.lowerToAlwaysFF = !GetCodeGenDeviceConfig()._requirePowerOnReset;
    pm.addPass(circt::createLowerSeqToSVPass(lowerSeqToSVOptions));
    pm.nest<circt::hw::HWModuleOp>().addPass(circt::sv::createHWCleanupPass()); // this merges always blocks
    pm.addPass(mlir::createCSEPass());
    pm.addPass(circt::createSimpleCanonicalizerPass());
    pm.nest<circt::hw::HWModuleOp>().addPass(circt::createLowerHWToSVPass());
    pm.nest<circt::hw::HWModuleOp>().addPass(circt::sv::createHWLegalizeModulesPass());
    pm.nest<circt::hw::HWModuleOp>().addPass(circt::sv::createPrettifyVerilogPass());

    std::string generatedModule;
    llvm::raw_string_ostream generatedModuleStr(generatedModule);
    pm.addPass(circt::createExportVerilogPass(generatedModuleStr));

    if (mlir::failed(pm.run(_mlirModule)))
    {
        DumpMlirOperation(_mlirModule);
        throw std::runtime_error("CIRCT pass execution failed");
    }

    return generatedModule;
}
