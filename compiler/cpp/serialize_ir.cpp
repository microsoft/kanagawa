// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

class IrJsonSerializer : public JsonWriter
{
  public:
    IrJsonSerializer(const Program& program, const std::string& fileName) : JsonWriter(fileName), _program(program)
    {
        AssignIds();

        {
            JsonValue functions = JsonValue::CreateArray();

            for (const Function& function : program._functions)
            {
                functions.PushBack(SerializeFunction(function));
            }

            _document.AddMember("functions", functions);
        }

        {
            JsonValue registers = JsonValue::CreateArray();

            for (size_t i = 0; i < program._registerTable.size(); i++)
            {
                const RegisterDescription& rd = program._registerTable[i];

                registers.PushBack(SerializeRegister(rd, i));
            }

            _document.AddMember("registers", registers);
        }
    }

  private:
    JsonValue SerializeRegister(const RegisterDescription& rd, const size_t index)
    {
        JsonValue jsonRegister = JsonValue::CreateObject();

        jsonRegister.AddMember("index", SerializeSizeT(index));

        jsonRegister.AddMember("width", SerializeSizeT(rd._width));

        jsonRegister.AddMember("name", SerializeString(rd._name));

        jsonRegister.AddMember("type", SerializeString(GetRegisterTypeName(rd._type)));

        switch (rd._type)
        {
        case RegisterType::Global:
            jsonRegister.AddMember("is_const", SerializeBool(rd.Global()._isConstant));

            if (rd.Global()._hasInitialValue)
            {
                jsonRegister.AddMember("initial_value", SerializeMpInt(rd.Global()._initialValue));
            }
            break;

        case RegisterType::Memory:
            jsonRegister.AddMember("width", SerializeSizeT(rd.Memory()._elementWidth));
            jsonRegister.AddMember("depth", SerializeSizeT(rd.Memory()._elementCount));
            break;

        default:
            break;
        }

        return jsonRegister;
    }

    JsonValue SerializeFunction(const Function& function)
    {
        JsonValue jsonFunction = JsonValue::CreateObject();

        jsonFunction.AddMember("name", SerializeString(function._name));

        jsonFunction.AddMember("is_export", SerializeBool(function.IsExport()));

        JsonValue jsonBasicBlocks = JsonValue::CreateArray();

        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            jsonBasicBlocks.PushBack(SerializeBasicBlock(basicBlock));
        }

        jsonFunction.AddMember("basic_blocks", jsonBasicBlocks);

        return jsonFunction;
    }

    JsonValue SerializeBasicBlock(const BasicBlock& basicBlock)
    {
        JsonValue jsonBasicBlock = JsonValue::CreateObject();

        const size_t id = SafeLookup(_basicBlockIds, &basicBlock);

        jsonBasicBlock.AddMember("id", SerializeSizeT(id));

        if (basicBlock.HasStartCondition())
        {
            JsonValue jsonOperations = JsonValue::CreateArray();

            SerializeOperationList(basicBlock._startConditionOperations, jsonOperations);

            jsonBasicBlock.AddMember("start_condition_operations", jsonOperations);
        }

        {
            JsonValue jsonOperations = JsonValue::CreateArray();

            SerializeOperationList(basicBlock._operations, jsonOperations);

            // Appended stages
            for (const Stage& stage : basicBlock._stages)
            {
                SerializeOperationList(stage._operations, jsonOperations);
            }

            jsonBasicBlock.AddMember("operations", jsonOperations);
        }

        return jsonBasicBlock;
    }

    void SerializeOperationList(const OperationList& ops, JsonValue& resultArray)
    {
        for (OperationList::const_iterator it = ops.begin(); it != ops.end(); ++it)
        {
            resultArray.PushBack(SerializeOperation(ops, it));
        }
    }

    // it will be advanced if an atomic block is serialized
    JsonValue SerializeOperation(const OperationList& ops, OperationList::const_iterator& it)
    {
        assert(it != ops.end());
        const Operation& op = *it;

        JsonValue jsonOperation = JsonValue::CreateObject();

        if (Opcode::BeginAtomic == op._opcode)
        {
            // Atomic blocks are nested in the serialized IR
            jsonOperation.AddMember("opcode", JsonValue("atomic"));

            JsonValue jsonContainedOperations = JsonValue::CreateArray();

            for (++it; it != ops.end(); ++it)
            {
                if (it->_opcode == Opcode::EndAtomic)
                {
                    // don't advance past the EndAtomic
                    break;
                }
                else
                {
                    jsonContainedOperations.PushBack(SerializeOperation(ops, it));
                }
            }

            jsonOperation.AddMember("operations", jsonContainedOperations);
        }
        else
        {
            jsonOperation.AddMember("opcode", SerializeString(GetOpcodeString(_program, op)));

            JsonValue jsonDstOperands = JsonValue::CreateArray();

            for (const DestinationOperand& dstOp : op._dst)
            {
                JsonValue jsonDstOperand = JsonValue::CreateObject();

                if (DestinationOperandType::Register == dstOp.Type())
                {
                    jsonDstOperand.AddMember("type", SerializeString("register"));

                    jsonDstOperand.AddMember("index", SerializeSizeT(dstOp.GetAccessedRegister()._registerIndex));
                }
                else
                {
                    assert(DestinationOperandType::Fifo == dstOp.Type());

                    jsonDstOperand.AddMember("type", SerializeString("fifo"));

                    jsonDstOperand.AddMember("fifo", SerializeFifoSubset(dstOp.GetFifoSubset()));
                }

                jsonDstOperands.PushBack(jsonDstOperand);
            }

            jsonOperation.AddMember("destinations", jsonDstOperands);
        }

        {
            JsonValue jsonSrcOperands = JsonValue::CreateArray();

            for (const SourceOperand& srcOp : op._src)
            {
                JsonValue jsonSrcOperand = JsonValue::CreateObject();

                if (SourceOperandType::Register == srcOp.Type())
                {
                    jsonSrcOperand.AddMember("type", SerializeString("register"));

                    jsonSrcOperand.AddMember("index", SerializeSizeT(srcOp.GetAccessedRegister()._registerIndex));
                }
                else if (SourceOperandType::Literal == srcOp.Type())
                {
                    jsonSrcOperand.AddMember("type", SerializeString("literal"));

                    jsonSrcOperand.AddMember("value", SerializeMpInt(srcOp.GetLiteral()._value));
                }
                else if (SourceOperandType::StringLiteral == srcOp.Type())
                {
                    jsonSrcOperand.AddMember("type", SerializeString("string"));

                    jsonSrcOperand.AddMember("value", SerializeString(srcOp.GetStringLiteral()));
                }
                else
                {
                    assert(SourceOperandType::Fifo == srcOp.Type());

                    jsonSrcOperand.AddMember("type", SerializeString("fifo"));

                    jsonSrcOperand.AddMember("fifo", SerializeFifoSubset(srcOp.GetFifoSubset()));
                }

                jsonSrcOperands.PushBack(jsonSrcOperand);
            }

            jsonOperation.AddMember("sources", jsonSrcOperands);
        }

        if ((Opcode::Enqueue == op._opcode) && op._getSuccessorBlock)
        {
            const BasicBlock* const target = op._getSuccessorBlock();

            jsonOperation.AddMember("target_block", JsonValue(SafeLookup(_basicBlockIds, target)));

            const char* typeStr = "";

            switch (op._flags._enqueue._type)
            {
            case EnqueueType::Default:
                typeStr = "default";
                break;
            case EnqueueType::ReorderBuffer:
                typeStr = "reorder";
                break;
            case EnqueueType::ContextSaverCaller:
                typeStr = "context_saver_caller";
                break;
            case EnqueueType::ContextSaverCallee:
                typeStr = "context_saver_callee";
                break;
            case EnqueueType::FunctionCall:
                typeStr = "function_call";
                break;
            default:
                assert(false);
            }

            jsonOperation.AddMember("enqueue_type", SerializeString(typeStr));

            {
                JsonValue reverseRenamingTable = JsonValue::CreateArray();

                for (const auto& p : op._reverseRenamingTable)
                {
                    JsonValue renameEntry = JsonValue::CreateArray();

                    renameEntry.PushBack(SerializeSizeT(p.first));

                    renameEntry.PushBack(SerializeSizeT(p.second));

                    reverseRenamingTable.PushBack(renameEntry);
                }

                jsonOperation.AddMember("renamed_to_original", reverseRenamingTable);
            }
        }

        return jsonOperation;
    }

    JsonValue SerializeFifoSubset(const FifoSubset& fs)
    {
        JsonValue v = JsonValue::CreateObject();

        v.AddMember("index", SerializeSizeT(fs._registerIndex));
        v.AddMember("offset", SerializeSizeT(fs._offset));
        v.AddMember("width", SerializeSizeT(fs._width));

        return v;
    }

    // Assign unique IDs to various constructs in the IR
    void AssignIds()
    {
        size_t basicBlockId = 0;

        for (const Function& function : _program._functions)
        {
            for (const BasicBlock& basicBlock : function._basicBlocks)
            {
                SafeInsert(_basicBlockIds, &basicBlock, basicBlockId);

                basicBlockId++;
            }
        }
    }

    const Program& _program;

    // Unique IDs per basic block
    std::map<const BasicBlock*, size_t> _basicBlockIds;
};

void SerializeIRToJson(const Program& program, const std::string& fileName)
{
    IrJsonSerializer serializer(program, fileName);
}
