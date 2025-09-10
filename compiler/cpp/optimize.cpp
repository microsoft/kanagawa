// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

// IR optimization and related functions

// Returns true if optimizations are allowed to replace a source operand
// with a literal
bool AllowLiteralSourceOperand(const Operation& op, const size_t sourceOperandIndex)
{
    assert(sourceOperandIndex < op._src.size());

    bool result = true;

    switch (op._opcode)
    {
    // Opcode::StartCondition is special because the back ends currently assume the start condition
    // is in a register
    case Opcode::StartCondition:
        result = false;
        break;

    // Opcode::MovCrossFunction ensures that function parameters/return values stay in registers
    // (later code assumes this)
    case Opcode::MovCrossFunction:
        result = false;
        break;

    // Only change the predicate operand
    // Other operands must stay to represent live values in other functions
    case Opcode::Enqueue:
        if (op._flags._enqueue._isPredicated && (0 == sourceOperandIndex))
        {
            result = true;
        }
        else
        {
            result = false;
        }
        break;

    default:
        break;
    }

    return result;
}

// If table[originalIndex] exists, returns that
// otherwise returns originalIndex
size_t OptionallyMapIndex(const size_t originalIndex, const RegisterIndexMap& table)
{
    const auto it = table.find(originalIndex);

    return (it == table.end()) ? originalIndex : it->second;
}

// Maps original register index to renamed register index
// This is used when propagating liveness through the CFG of a function
// The input registers for a basic block are not renamed
// but inside of a pipeline (at an enqueue operation), local registers have been renamed
size_t MapOriginalToRenamedRegisterIndex(const Operation& op, const size_t originalIndex)
{
    assert(op._opcode == Opcode::Enqueue);

    return OptionallyMapIndex(originalIndex, op._renamingTable);
}

// Maps most recent renamed register index to original register index
size_t MapRenamedToOriginalRegisterIndex(const Operation& op, const size_t originalIndex)
{
    assert(op._opcode == Opcode::Enqueue);

    return OptionallyMapIndex(originalIndex, op._reverseRenamingTable);
}

bool IsGlobalReg(const Program& program, const AccessedRegister& reg)
{
    const RegisterDescription& regDesc = program._registerTable[reg._registerIndex];

    return regDesc._type == RegisterType::Global;
}

// Maps register index to known value
typedef std::map<size_t, Literal> RegisterValueMap;

bool Equal(const RegisterValueMap& src1, const RegisterValueMap& src2)
{
    if (src1.size() != src2.size())
    {
        return false;
    }

    for (const auto& p : src1)
    {
        const size_t regIndex = p.first;
        const Literal& val = p.second;

        const auto it = src2.find(regIndex);

        if (it == src2.end())
        {
            return false;
        }
        else
        {
            const Literal& otherVal = it->second;

            if (val != otherVal)
            {
                return false;
            }
        }
    }

    return true;
}

// Returns true if "larger" is a superset of "smaller"
bool IsSuperSet(const RegisterValueMap& larger, const RegisterValueMap& smaller)
{
    for (const auto& p : smaller)
    {
        const size_t regIndex = p.first;
        const Literal& val = p.second;

        const auto it = larger.find(regIndex);
        if (it == larger.end())
        {
            return false;
        }

        if (it->second != val)
        {
            return false;
        }
    }

    return true;
}

// As a part of constant propagation & literal substition
// Finds all globals with constant known values
RegisterValueMap ComputeConstantGlobals(Program& program)
{
    RegisterValueMap result = {};

    for (size_t registerIndex = 0; registerIndex < program._registerTable.size(); ++registerIndex)
    {
        const RegisterDescription& regDesc = program._registerTable[registerIndex];

        if (RegisterType::Global == regDesc._type)
        {
            // The register must be constant
            // and have a known value that fits into a literal struct
            if (regDesc.Global()._isConstant && regDesc.Global()._hasInitialValue)
            {
                result[registerIndex] = regDesc.GetInitialValueLiteral();
            }
        }
    }

    return result;
}

// Replaces references to constant globals with literals
// called before scheduling
bool SubstituteLiterals(Program& program)
{
    const RegisterValueMap globalConstants = ComputeConstantGlobals(program);

    bool madeSubstitution = false;

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            const auto operationCallback = [&](Operation& op)
            {
                for (SourceOperand& srcOp : op._src)
                {
                    if (SourceOperandType::Register == srcOp.Type())
                    {
                        const size_t index = srcOp.GetAccessedRegister()._registerIndex;

                        const auto it = globalConstants.find(index);

                        if (it != globalConstants.end())
                        {
                            // Turn the source operand into a literal
                            srcOp = SourceOperand(it->second);
                            madeSubstitution = true;
                        }
                    }
                }
            };

            ForEachOperationForward(basicBlock, operationCallback);
        }
    }

    return madeSubstitution;
}

// Called before scheduling
bool ConstantPropagation(Program& program, ControlFlowGraph& controlFlowGraph, Function& function,
                         const OperationEnumerationMode mode)
{
    bool didChangeIR = false;

    // This is set of basic blocks that need to be processed
    DataFlowWorkList workList(controlFlowGraph, function);

    // Mapping from preceeding enqueue operation, to the set of values which are constant at that predecessor
    typedef std::map<const Operation*, RegisterValueMap> PredecessorMap;

    // Tracks values at the start of each basic block
    std::unordered_map<BasicBlock*, PredecessorMap> inputValueMap;

    // Initialize PredecessorMap so that the first pass through the convergent algorithm
    // will see unknown values where it should
    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        const auto operationCallback = [&](Operation& op)
        {
            if (op._getSuccessorBlock)
            {
                // This is an enqueue operation
                // Propagate inputs to successor
                BasicBlock* const successor = op._getSuccessorBlock();

                if (successor->_function == &function)
                {
                    PredecessorMap& predecessorMap = inputValueMap[successor];

                    predecessorMap[&op] = RegisterValueMap();
                }
            }
        };

        ForEachOperationForward(basicBlock, operationCallback, mode);
    }

    // Until convergence
    while (!workList.Empty())
    {
        // Choose a basic block
        BasicBlock* const basicBlock = workList.Pop();

        // Compute the set of known values at the beginning of this basic block
        RegisterValueMap registerValues;

        // If this basic block is the start of a function
        // Assume all parameters are unknown
        // This is important for pipelined calls, where the iteration count is transformed into an iteration index
        const bool isFunctionStart = basicBlock->IsFirstInFunction();

        const PredecessorMap& predecessorMap = inputValueMap[basicBlock];

        if (!predecessorMap.empty() && !isFunctionStart)
        {
            // Compute the intersection of all predecessors

            // Get the values at the enqueue operation in one of the predecessors
            const RegisterValueMap& valuesAtPredeccessor0 = (predecessorMap.begin())->second;

            // For each known value from predecessor 0
            for (const auto& p : valuesAtPredeccessor0)
            {
                const size_t regIndex = p.first;
                const Literal value = p.second;

                // Check to see if all predecessors agree
                bool shouldInsert = true;

                for (const auto& p : predecessorMap)
                {
                    const RegisterValueMap& valuesAtPredeccessor = p.second;

                    const auto it = valuesAtPredeccessor.find(regIndex);

                    if (it == valuesAtPredeccessor.end())
                    {
                        // This predecessor does not have a known value for this register
                        shouldInsert = false;
                    }
                    else
                    {
                        // This predecessor does have a known value for this register
                        // Compare it to predecessor 0
                        const Literal otherValue = it->second;

                        if (value != otherValue)
                        {
                            // The predecessors disagree on the possible value
                            shouldInsert = false;
                        }
                    }
                }

                if (shouldInsert)
                {
                    // All predecessors agree - so the basic block under consideration can treat the value as known
                    // at the start of the basic block
                    registerValues[regIndex] = value;
                }
            }
        }

        // Iterate over each operation in the basic block
        const auto operationCallback = [&](Operation& op)
        {
            // Check to see if all source operands have known values
            bool allSourcesKnown = true;

            if (op._getSuccessorBlock)
            {
                // This is an enqueue operation
                // Propagate inputs to successor
                BasicBlock* const successor = op._getSuccessorBlock();

                if (successor->_function == &function)
                {
                    RegisterValueMap valuesForSuccessor;

                    // For each value that is known constant in the predecessor block
                    for (const auto& p : registerValues)
                    {
                        if (op._renamingTable.end() != op._renamingTable.find(p.first))
                        {
                            // this original register was written by this basic block (and thus renamed)
                            // Don't pass the value on to the successor.  The renamed value will be passed on.
                            // If p.first is not found in _renamingTable, it means the register was not written by this
                            // basic block and the value is just being passed through
                        }
                        else
                        {
                            // Map register index from renamed index to original index (if the register index was
                            // renamed)
                            const size_t registerIndex = MapRenamedToOriginalRegisterIndex(op, p.first);

                            valuesForSuccessor[registerIndex] = p.second;
                        }
                    }

                    // inputValueMap should have already been pre-initialized
                    const auto inputValueMapIt = inputValueMap.find(successor);
                    assert(inputValueMapIt != inputValueMap.end());
                    PredecessorMap& predecessorMap = inputValueMapIt->second;

                    const auto predecessorMapIt = predecessorMap.find(&op);
                    assert(predecessorMapIt != predecessorMap.end());
                    RegisterValueMap& previousValues = predecessorMapIt->second;

                    if (!Equal(valuesForSuccessor, previousValues))
                    {
                        // This algorithm must always add new constants, never remove them
                        assert(IsSuperSet(valuesForSuccessor, previousValues));

                        // The set of known values from this basic block to the successor has changed
                        // Ensure the successor is processed again
                        previousValues = valuesForSuccessor;

                        workList.AddBasicBlock(successor);
                    }
                }
            }

            // Renaming ensures that a destination register will never appear in registerValues
            for (const DestinationOperand& destOp : op._dst)
            {
                if (DestinationOperandType::Register == destOp.Type())
                {
                    assert(registerValues.end() == registerValues.find(destOp.GetAccessedRegister()._registerIndex));
                }
            }

            for (size_t sourceOperandIndex = 0; sourceOperandIndex < op._src.size(); sourceOperandIndex++)
            {
                SourceOperand& srcOp = op._src[sourceOperandIndex];

                if (srcOp.Type() == SourceOperandType::Fifo)
                {
                    allSourcesKnown = false;
                }
                else if (srcOp.Type() == SourceOperandType::StringLiteral)
                {
                    allSourcesKnown = false;
                }
                else if (srcOp.Type() == SourceOperandType::Register)
                {
                    const AccessedRegister& srcReg = srcOp.GetAccessedRegister();

                    std::optional<Literal> srcValue;

                    auto registerValuesIt = registerValues.find(srcReg._registerIndex);

                    if (registerValues.end() == registerValuesIt)
                    {
                        // Check to see if the source register is a constant global
                        const RegisterDescription& regDesc = program._registerTable[srcReg._registerIndex];

                        if ((RegisterType::Global == regDesc._type) && regDesc.Global()._isConstant &&
                            regDesc.Global()._hasInitialValue)
                        {
                            srcValue = regDesc.GetInitialValueLiteral();
                        }
                    }
                    else
                    {
                        srcValue = registerValuesIt->second;
                    }

                    if (srcValue)
                    {
                        if (AllowLiteralSourceOperand(op, sourceOperandIndex))
                        {
                            // The value is known

                            // Replace the source operand with a literal
                            // Even if this doesn't allow this operation to be removed
                            // It could cause operations that produce this operand
                            // to no longer be live
                            srcOp = SourceOperand(*srcValue);

                            didChangeIR = true;

                            // Lut operations should not contain literal source operands
                            if (Opcode::Lut == op._opcode)
                            {
                                FixupLutOp(op);
                            }
                        }
                    }
                    else
                    {
                        // Value is not known
                        allSourcesKnown = false;
                    }
                }
                else
                {
                    assert(srcOp.Type() == SourceOperandType::Literal);
                }
            }

            // Writes to global registers are not optimized
            // because another thread on a different path could modify the global
            if (allSourcesKnown && (1 == op._dst.size()) && (DestinationOperandType::Register == op._dst[0].Type()) &&
                !IsGlobalReg(program, op._dst[0].GetAccessedRegister()))
            {
                bool doUpdateIR = true;
                boost::optional<Literal> destinationValue;

                const size_t dstWidth = op._dst[0].Width(program);

                switch (op._opcode)
                {
                case Opcode::Mov:
                {
                    if (op._src[0].Type() == SourceOperandType::Literal)
                    {
                        // To prevent infinite optimization, don't consider this as a reason to continue optimization
                        // This operation will be replaced with an identical move operation (not helpful), additionally
                        // registerValues[] will be set to allow future operations to know that the destination operand
                        // is constant (helpful)
                        doUpdateIR = false;
                    }

                    destinationValue =
                        Literal{Resize(op._src[0].GetLiteral(), op.ShouldSignExtend(0), dstWidth), dstWidth};
                }
                break;

                case Opcode::Clear:
                {
                    destinationValue = Literal{0, dstWidth};
                }
                break;

                case Opcode::UnaryOp:
                {
                    assert(1 == op._dst.size());

                    const Literal src = op._src[0].GetLiteral();

                    const mp_int opResult =
                        ImplementUnaryOp(src, op.ShouldSignExtend(0), op._flags._unaryOpType, dstWidth);

                    destinationValue = Literal{opResult, dstWidth};
                }
                break;

                case Opcode::BinaryOp:
                {
                    assert(1 == op._dst.size());

                    const Literal src0 = op._src[0].GetLiteral();
                    const Literal src1 = op._src[1].GetLiteral();

                    // Sign extend flags are still passed to ImplementBinaryOp to differentiate
                    // between logical and arithmetic right shift
                    const mp_int opResult = ImplementBinaryOp(src0, src1, dstWidth, op.ShouldSignExtend(0),
                                                              op.ShouldSignExtend(1), op._flags._binaryOpType);

                    destinationValue = Literal{opResult, dstWidth};
                }
                break;

                case Opcode::Gather:
                {
                    mp_int combined = 0;

                    for (size_t i = 0; i < op._src.size(); i++)
                    {
                        // Gather operations only copy bits
                        // no sign extension is necessary
                        assert(!op.ShouldSignExtend(i));

                        const GatherEntry& gatherEntry = op._flags._gather._entries->at(i);

                        const Literal& srcLiteral = op._src[i].GetLiteral();

                        const mp_int sliced =
                            Truncate(srcLiteral._value >> gatherEntry._sourceOffset, gatherEntry._numBits);

                        combined = combined | (sliced << gatherEntry._destOffset);
                    }

                    combined = Truncate(combined, dstWidth);

                    destinationValue = Literal{combined, dstWidth};
                }
                break;

                default:
                    break;
                }

                if (destinationValue)
                {
                    const Literal newValue = *destinationValue;

                    // Replace the operation with a mov of a literal value
                    assert(1 == op._dst.size());
                    const AccessedRegister& dstReg = op._dst[0].GetAccessedRegister();

                    if (doUpdateIR)
                    {
                        // No need for sign extension on this move because
                        // the source and destination widths match
                        assert(newValue._width == dstWidth);

                        Operation newOperation = {};

                        newOperation._opcode = Opcode::Mov;
                        newOperation._locations = op._locations;
                        newOperation._src.push_back(SourceOperand(newValue));
                        newOperation._dst.push_back(dstReg);

                        op = newOperation;

                        didChangeIR = true;
                    }

                    // Update the register value map
                    registerValues[dstReg._registerIndex] = newValue;
                }
            }
        };

        ForEachOperationForward(*basicBlock, operationCallback, mode);
    }

    return didChangeIR;
}

// Called before scheduling
bool RemoveDeadOperationsImpl(Program& program, ControlFlowGraph& controlFlowGraph, Function& function,
                              const OperationEnumerationMode mode,
                              const std::function<bool(const Operation&)>& isLiveOperationCb,
                              const std::function<void(const Operation&)>& notifyLiveOpCb, const bool allowIRChanges)
{
    // This is set of basic blocks that need to be processed
    DataFlowWorkList workList(controlFlowGraph, function);

    // The set the of registers which are live at the begining of a basic block
    RegisterSetMap liveInMap;

    // This is relative to the current basic block
    RegisterSet liveRegisters;

    // This is global, once an operation is marked live, it stays that way
    std::unordered_set<const Operation*> liveOperations;

    // For EnqueueRegisters/DequeueRegisters
    // maps fifo index to set of operands that are live
    std::map<size_t, std::set<size_t>> liveLocalDataPropagationSources;

    // Maps LoadMemory operation to set of destination operand indices which are live
    std::map<const Operation*, std::set<size_t>> liveLoadMemoryDestinations;

    const auto callback = [&](const Operation& op)
    {
        // Determine if this operation is live
        bool liveOperation = false;

        if (op._getSuccessorBlock)
        {
            // This is an enqueue operation
            // Lookup the variables that are live-in at the start of the destination basic block
            BasicBlock* const successor = op._getSuccessorBlock();

            const RegisterSet& successorLiveInSet = liveInMap[successor];

            for (const size_t registerIndex : successorLiveInSet)
            {
                // If a register is live-in at the successor
                // then it is live-in at the enqueue
                liveRegisters.insert(MapOriginalToRenamedRegisterIndex(op, registerIndex));
            }

            for (const size_t registerIndex : successor->_liveInReg)
            {
                // Also include registers which are forced live-in at the successor
                liveRegisters.insert(MapOriginalToRenamedRegisterIndex(op, registerIndex));
            }
        }

        if (isLiveOperationCb(op))
        {
            // Some operations are live based on the opcode only
            liveOperation = true;
        }
        else
        {
            // If the operation writes to a live variable, then the operation is live
            for (size_t destOperandIndex = 0; destOperandIndex < op._dst.size(); destOperandIndex++)
            {
                const DestinationOperand& destOp = op._dst[destOperandIndex];

                if (DestinationOperandType::Register == destOp.Type())
                {
                    const AccessedRegister& reg = destOp.GetAccessedRegister();

                    if (Contains(liveRegisters, reg._registerIndex))
                    {
                        liveOperation = true;

                        if (Opcode::DequeueRegisters == op._opcode)
                        {
                            const size_t fifo = op._flags._queueRegisters._fifoIndex;

                            liveLocalDataPropagationSources[fifo].insert(destOperandIndex);
                        }
                        else if (Opcode::LoadMemory == op._opcode)
                        {
                            liveLoadMemoryDestinations[&op].insert(destOperandIndex);
                        }
                    }
                }
                else
                {
                    // FIFO writes are always live
                    assert(DestinationOperandType::Fifo == destOp.Type());

                    liveOperation = true;
                }
            }
        }

        // If the operation is live, then the sources must be live too
        if (liveOperation)
        {
            for (const SourceOperand& sourceOp : op._src)
            {
                if (sourceOp.Type() == SourceOperandType::Register)
                {
                    liveRegisters.insert(sourceOp.GetAccessedRegister()._registerIndex);
                }
            }

            liveOperations.insert(&op);

            notifyLiveOpCb(op);
        }
        else
        {
            // If an operation is considered live at one point, then it should always be considered live after that
            // point too
            assert(liveOperations.end() == liveOperations.find(&op));
        }
    };

    // Until convergence
    while (!workList.Empty())
    {
        // Choose a basic block
        BasicBlock* const basicBlock = workList.Pop();

        // Reset the liveRegisters container which is written by the callback
        liveRegisters.clear();

        if (OperationEnumerationMode::Scheduled == mode)
        {
            // add clock gate registers
            for (const Stage& stage : basicBlock->_stages)
            {
                for (const auto& p : stage._clockGates)
                {
                    const std::set<size_t>& gateRegisters = p.second;

                    Union(liveRegisters, gateRegisters);
                }
            }
        }

        // Iterate through all operations in the basic block (in reverse order)
        ForEachOperationReverse(*basicBlock, callback, mode);

        // If the set of live input registers for this basic block has changed since this last time it was computed
        // then add all predecessor blocks to the working set
        if (liveRegisters != liveInMap[basicBlock])
        {
            workList.AddPredecessors(basicBlock);

            // Save the new set of live input registers in the basic block
            liveInMap[basicBlock] = std::move(liveRegisters);
        }
    }

    // If IR changes are not allowed, then early-out
    if (!allowIRChanges)
    {
        return false;
    }

    bool didChangeIR = false;

    // Modify {Enqueue,Dequeue}Registers based on liveLocalDataPropagationSources
    // Modify LoadMemory destination registers based on liveLoadMemoryDestinations
    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        std::set<size_t> dequeueFifoSet;

        const auto callback = [&](Operation& op)
        {
            const size_t fifo = op._flags._queueRegisters._fifoIndex;

            const bool isEnqueue = Opcode::EnqueueRegisters == op._opcode;
            const bool isDequeue = Opcode::DequeueRegisters == op._opcode;

            if (isEnqueue || isDequeue)
            {
                assert(!isEnqueue || (op._flags._queueRegisters._offsets->size() == op._src.size()));
                assert(!isDequeue || (op._flags._queueRegisters._offsets->size() == op._dst.size()));

                const std::set<size_t>& liveOperands = liveLocalDataPropagationSources[fifo];

                size_t runningOffset = 0;

                size_t operandCount = op._flags._queueRegisters._offsets->size();

                for (size_t i = 0; i < operandCount; i++)
                {
                    if (liveOperands.end() == liveOperands.find(i))
                    {
                        VectorEraseByIndex(*op._flags._queueRegisters._offsets, runningOffset);

                        if (isEnqueue)
                        {
                            VectorEraseByIndex(op._src, runningOffset);
                        }
                        else
                        {
                            VectorEraseByIndex(op._dst, runningOffset);
                        }
                    }
                    else
                    {
                        runningOffset++;
                    }
                }

                if (op._flags._queueRegisters._offsets->empty())
                {
                    // If no operands remain, then remove the operation
                    liveOperations.erase(&op);
                }

                assert(!isEnqueue || (op._flags._queueRegisters._offsets->size() == op._src.size()));
                assert(!isDequeue || (op._flags._queueRegisters._offsets->size() == op._dst.size()));
            }
            else if (Opcode::LoadMemory == op._opcode)
            {
                const std::set<size_t>& liveOperands = liveLoadMemoryDestinations[&op];

                for (size_t i = 0; i < op._dst.size(); i++)
                {
                    if (liveOperands.end() == liveOperands.find(i))
                    {
                        if (DestinationOperandType::Register == op._dst[i].Type())
                        {
                            program._registerTable[op._dst[i].GetAccessedRegister()._registerIndex]._type =
                                RegisterType::BitBucket;
                        }
                    }
                }
            }
        };

        ForEachOperationForward(basicBlock, callback, mode);
    }

    // The computation has converged, now remove dead operations
    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        const auto predicate = [&](const Operation& op)
        {
            const bool isLive = liveOperations.end() != liveOperations.find(&op);

            didChangeIR = didChangeIR || !isLive;

            return !isLive;
        };

        if (OperationEnumerationMode::Unscheduled == mode)
        {
            basicBlock._startConditionOperations.remove_if(predicate);

            basicBlock._operations.remove_if(predicate);

            // Note that the appended stages are not modified here
            // Those are optimized away with RemoveDeadJumps
        }
        else
        {
            assert(OperationEnumerationMode::Scheduled == mode);

            for (Stage& stage : basicBlock._stages)
            {
                stage._operations.remove_if(predicate);
            }
        }
    }

    return didChangeIR;
}

bool RemoveDeadOperations(Program& program, ControlFlowGraph& controlFlowGraph, Function& function,
                          const OperationEnumerationMode mode)
{
    return RemoveDeadOperationsImpl(
        program, controlFlowGraph, function, mode, IsLiveOperation, [](const Operation&) {}, true);
}

// Removes global variables which are unused
// This includes global variables which are used only to compute a new value
// for that same global variable
bool RemoveUnusedGlobals(Program& program)
{
    // All variables which can be inspected are live
    std::set<size_t> liveGlobalVariables;

    for (const InspectableVariable& inspectableVariable : program._inspectableVariables)
    {
        for (const size_t registerIndex : inspectableVariable._registers)
        {
            if (RegisterType::Global == program._registerTable[registerIndex]._type)
            {
                liveGlobalVariables.insert(registerIndex);
            }
        }
    }

    // All variables which are used for code coverage are live
    for (const InspectableVariable& inspectableVariable : program._codeCoverageVariables)
    {
        for (const size_t registerIndex : inspectableVariable._registers)
        {
            if (RegisterType::Global == program._registerTable[registerIndex]._type)
            {
                liveGlobalVariables.insert(registerIndex);
            }
        }
    }

    // All variables referenced by a global view are live
    for (const auto& p : program._globalViewFunctions)
    {
        for (const Operation& op : p.second)
        {
            for (const SourceOperand& src : op._src)
            {
                if (SourceOperandType::Register == src.Type())
                {
                    const size_t registerIndex = src.GetAccessedRegister()._registerIndex;

                    if (RegisterType::Global == program._registerTable[registerIndex]._type)
                    {
                        liveGlobalVariables.insert(registerIndex);
                    }
                }
            }
        }
    }

    const auto isLiveOperation = [&](const Operation& op)
    {
        if (Opcode::WriteGlobal == op._opcode)
        {
            // WriteGlobal operations are considered live if and only if the global
            // is already known to be live
            assert(1 == op._dst.size());
            const size_t registerIndex = op._dst[0].GetAccessedRegister()._registerIndex;

            return liveGlobalVariables.end() != liveGlobalVariables.find(registerIndex);
        }
        else
        {
            return IsLiveOperation(op);
        }
    };

    // Run until convergence
    do
    {
        const std::set<size_t> prevLiveGlobalVariables = liveGlobalVariables;

        for (Function& function : program._functions)
        {
            ControlFlowGraph controlFlowGraph(function, program, OperationEnumerationMode::Unscheduled,
                                              ControlFlowGraphPhase::PrePipeline);

            const auto notifyLiveOp = [&](const Operation& op)
            {
                for (const SourceOperand& srcOp : op._src)
                {
                    // If any source is a global variable
                    // then mark it as live
                    if (SourceOperandType::Register == srcOp.Type())
                    {
                        const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                        const RegisterDescription& rd = program._registerTable[registerIndex];

                        if (RegisterType::Global == rd._type)
                        {
                            liveGlobalVariables.insert(registerIndex);
                        }
                    }
                }
            };

            const bool result =
                RemoveDeadOperationsImpl(program, controlFlowGraph, function, OperationEnumerationMode::Unscheduled,
                                         isLiveOperation, notifyLiveOp, false);
            assert(!result); // This should not change the IR
        }

        if (prevLiveGlobalVariables == liveGlobalVariables)
        {
            // The set of global variables has converged
            break;
        }
    } while (true);

    bool didChangeIR = false;

    // Now modify the IR
    for (Function& function : program._functions)
    {
        ControlFlowGraph controlFlowGraph(function, program, OperationEnumerationMode::Unscheduled,
                                          ControlFlowGraphPhase::PrePipeline);

        const auto notifyLiveOp = [](const Operation& op) {};

        const bool result =
            RemoveDeadOperationsImpl(program, controlFlowGraph, function, OperationEnumerationMode::Unscheduled,
                                     isLiveOperation, notifyLiveOp, true);

        didChangeIR = didChangeIR || result;
    }

    return didChangeIR;
}

// Mapping from register index to the SourceOperand that is equivalent
typedef std::map<size_t, SourceOperand> EqualValueMap;

void RenameEqualValues(const EqualValueMap& equalValues, EqualValueMap& valuesForSuccessor, const Operation& op)
{
    // For each entry in the predecessor block
    for (const auto& p : equalValues)
    {
        // If p.second has been overwritten by this basic block (found as index
        // in _renamingTable), then do not pass this equal value relation on to
        // the successor (source of move is no longer valid because it has been
        // overwritten).
        if (p.second.Type() == SourceOperandType::Register)
        {
            if (op._renamingTable.find(p.second.GetAccessedRegister()._registerIndex) != op._renamingTable.end())
            {
                continue;
            }
        }

        // There are three cases in finding p.first in the renaming tables:
        // 1. Found as index in renamingTable
        //      This means that the original p.first register was written by this basic block (and thus
        //      renamed to something different). Don't pass the equal value relation on to the successor.
        // 2. Found as index in _reverseRenamingTable (value in _renamingTable)
        //      This is the typical case. p.first is the result of this basic block writing to it.
        //      Map the equal value relation to the registers used in the successors using the rename
        //      table. Pass the relation on to the successor
        // 3. Not found in _renamingTable or _reverseRenamingTable
        //      p.first is not written by this basic block and the value is just being passed through.
        //      Map the equal value relation of p.second used in the successor (if needed) and pass
        //      the relation to the successor.

        // If p.first is not an index in the renamingTable
        if (op._renamingTable.find(p.first) == op._renamingTable.end())
        {
            // Map register index from renamed index to original index
            const size_t registerIndex = MapRenamedToOriginalRegisterIndex(op, p.first);

            // Rename source operand register
            SourceOperand second = p.second;
            if (p.second.Type() == SourceOperandType::Register)
            {
                size_t secondRegisterIndex =
                    MapRenamedToOriginalRegisterIndex(op, p.second.GetAccessedRegister()._registerIndex);
                second = SourceOperand(AccessedRegister({secondRegisterIndex}));
            }

            valuesForSuccessor[registerIndex] = second;
        }
    }
}

// Makes MOV operations dead
// For example:
// x = op(...)
// y = x
// z = op(y)
//
// Is transformed to:
// x = op(...)
// y = x
// z = op(x)
bool KillMoves(Program& program, ControlFlowGraph& controlFlowGraph, Function& function)
{
    bool didChangeIR = false;

    // Set of basic blocks that need to be processed
    DataFlowWorkList workList(controlFlowGraph, function);

    // Mapping from preceeding enqueue operation to the EqualValueMap output by that predecessor
    typedef std::map<const Operation*, EqualValueMap> PredecessorMap;

    // Track equal values for each basic block
    std::unordered_map<BasicBlock*, EqualValueMap> equalValueMaps;
    // Track new equal values at the start of each basic block
    std::map<BasicBlock*, PredecessorMap> inputValueMap;

    const bool optimizing = GetCodeGenConfig()._optimize > 0;

    // Initialize equalValueMaps and inputValueMap
    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        EqualValueMap& equalValues = equalValueMaps[&basicBlock];

        // Find equal values in this basic block
        for (Operation& op : basicBlock._operations)
        {
            if (op._opcode == Opcode::Mov)
            {
                assert(1 == op._src.size());
                assert(1 == op._dst.size());

                const SourceOperand& srcReg = op._src[0];
                const AccessedRegister& dstReg = op._dst[0].GetAccessedRegister();
                const bool srcIsLiteral = (srcReg.Type() == SourceOperandType::Literal);
                assert(srcReg.Type() == SourceOperandType::Literal || srcReg.Type() == SourceOperandType::Register);

                // This optimization only applies to locals. Global registers, DSP outputs and memory outputs must be
                // handled carefully
                if (srcIsLiteral || (IsRegisterTypeAllowedInKillMoves(
                                        program._registerTable[srcReg.GetAccessedRegister()._registerIndex]._type)))
                {
                    const size_t destWidth = op._dst[0].Width(program);
                    const bool dstHasSource = program._registerTable[dstReg._registerIndex]._isDirectSource;

                    if (!optimizing && dstHasSource)
                    {
                        // Keep this move, the debugger needs it.
                    }
                    else if (srcIsLiteral)
                    {
                        // Size-changing moves are handled here
                        // Size-extension is not yet handled
                        if (0 == op._signExtendSourceMask)
                        {
                            Literal newLiteral = {};

                            newLiteral._value = Truncate(op._src[0].GetLiteral()._value, destWidth);
                            newLiteral._width = destWidth;

                            // Insert a new record into equalValues
                            equalValues[dstReg._registerIndex] = SourceOperand(newLiteral);
                        }
                    }
                    else if (destWidth ==
                             op._src[0].Width(program)) // Size-changing moves do useful work, do not optimize them away
                    {
                        // Insert a new record into equalValues
                        equalValues[dstReg._registerIndex] = op._src[0];
                    }
                }
            }
        }

        const auto callback = [&](Operation& op)
        {
            // Propagate to successors
            // Enqueue operation - Propagate to successors
            if (op._getSuccessorBlock)
            {
                BasicBlock* const successor = op._getSuccessorBlock();

                if (successor->_function == &function)
                {
                    // Rename to register indices for successor
                    EqualValueMap valuesForSuccessor;
                    RenameEqualValues(equalValues, valuesForSuccessor, op);

                    // Update successor
                    PredecessorMap& predecessorMap = inputValueMap[successor];
                    predecessorMap[&op] = valuesForSuccessor;
                }
            }
        };

        ForEachOperationForward(basicBlock, callback);
    }

    while (!workList.Empty())
    {
        // Choose a basic block
        BasicBlock* const basicBlock = workList.Pop();

        // New equal values found for this basic block
        EqualValueMap newEqualValues;
        // Existing equal values for this basic block
        EqualValueMap& equalValues = equalValueMaps[basicBlock];
        const PredecessorMap& predecessorMap = inputValueMap[basicBlock];

        if (!predecessorMap.empty())
        {
            // Compute the intersection of all predecessors

            // Get the entries at the enqueue operation in one of the predecessors
            const EqualValueMap& valuesAtPredecessor0 = (predecessorMap.begin())->second;

            // For each entry from predecessor 0
            for (const auto& p : valuesAtPredecessor0)
            {
                const size_t regIndex = p.first;
                const SourceOperand srcOp = p.second;

                // Check to see if all predecessors agree
                bool shouldInsert = true;

                for (const auto& q : predecessorMap)
                {
                    const EqualValueMap& valuesAtPredecessor = q.second;

                    const auto it = valuesAtPredecessor.find(regIndex);

                    // Not found in this predecessor
                    if (it == valuesAtPredecessor.end())
                    {
                        shouldInsert = false;
                    }
                    else
                    {
                        // Compare operands
                        const SourceOperand& otherSrcOp = it->second;
                        if (srcOp != otherSrcOp)
                        {
                            shouldInsert = false;
                        }
                    }
                }

                if (shouldInsert)
                {
                    const auto it = equalValues.find(regIndex);
                    if (it == equalValues.end())
                    {
                        // All predecessors agree and new to equalValues - add to maps
                        equalValues[regIndex] = srcOp;
                        newEqualValues[regIndex] = srcOp;
                    }
                    else
                    {
                        assert(it->second == srcOp);
                    }
                }
            }

            // Clear predecessor maps
            for (auto& p : predecessorMap)
            {
                EqualValueMap q = p.second;
                q.clear();
            }
        }

        if (!newEqualValues.empty())
        {
            // Add to new equal values for successors
            const auto callback = [&](Operation& op)
            {
                // Enqueue operation - Propagate to successors
                if (op._getSuccessorBlock)
                {
                    BasicBlock* const successor = op._getSuccessorBlock();

                    if (successor->_function == &function)
                    {
                        // Rename to register indices for successor
                        EqualValueMap valuesForSuccessor;
                        RenameEqualValues(equalValues, valuesForSuccessor, op);

                        // Find successor in inputValueMap
                        const auto inputValueMapIt = inputValueMap.find(successor);
                        assert(inputValueMapIt != inputValueMap.end());
                        PredecessorMap& nextPredecessorMap = inputValueMapIt->second;

                        // Find this enqueue op
                        const auto predecessorMapIt = nextPredecessorMap.find(&op);
                        assert(predecessorMapIt != nextPredecessorMap.end());
                        EqualValueMap& previousValues = predecessorMapIt->second;

                        for (const auto& p : valuesForSuccessor)
                        {
                            auto it = previousValues.find(p.first);
                            if (it != previousValues.end())
                            {
                                assert(p.second == it->second);
                            }
                            else
                            {
                                previousValues[p.first] = p.second;
                            }
                        }

                        workList.AddBasicBlock(successor);
                    }
                }
            };

            ForEachOperationForward(*basicBlock, callback);
        }
    }

    // Apply optimization
    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        const EqualValueMap& equalValues = equalValueMaps[&basicBlock];

        const auto callback = [&](Operation& op)
        {
            // Optimize by replacing source operands with equivalent value
            // Don't optimize Enqueue operations - values should pass through them in registers to ensure that fifo
            // writes occur correctly
            if (!ShouldPreserveInputRegisters(op._opcode))
            {
                // Replace source operands where possible
                for (SourceOperand& srcOp : op._src)
                {
                    if (srcOp.Type() == SourceOperandType::Register)
                    {
                        AccessedRegister& reg = srcOp.GetAccessedRegister();
                        auto it = equalValues.find(reg._registerIndex);
                        if (it != equalValues.end())
                        {
                            // Get the SourceOperand that have the same value as reg._registerIndex
                            const SourceOperand& equalValue = it->second;

                            // Match - Replace the source operand
                            if (equalValue.Type() == SourceOperandType::Literal)
                            {
                                // Preserve the src width, to ensure sign-extension works
                                const size_t width = program._registerTable[reg._registerIndex]._width;

                                if (srcOp.Type() == SourceOperandType::Register)
                                {
                                    didChangeIR = true;
                                }
                                else
                                {
                                    // Only check value because original width is preserved
                                    if (srcOp.GetLiteral()._value != equalValue.GetLiteral()._value)
                                    {
                                        didChangeIR = true;
                                    }
                                }

                                srcOp = equalValue;

                                srcOp.GetLiteral()._width = width;
                            }
                            else
                            {
                                if (srcOp != equalValue)
                                {
                                    didChangeIR = true;
                                }

                                srcOp = equalValue;
                            }
                        }
                    }
                }
            }
        };

        ForEachOperationForward(basicBlock, callback);
    }

    return didChangeIR;
}

size_t CountOperations(Function& function)
{
    size_t result = 0;

    const auto callback = [&result](Operation& op) { ++result; };

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        ForEachOperationForward(basicBlock, callback);
    }

    return result;
}

// Removes Enqueue operations that target empty basic blocks
// This enables code that follows in the compiler to assume that all jumps go somewhere
// Note that this runs before scheduling
bool RemoveDeadJumps(Function& function)
{
    bool didChangeIR = false;

    const auto predicate = [&](const Operation& op)
    {
        bool shouldRemove = false;

        if (op._getSuccessorBlock)
        {
            const BasicBlock* const block = op._getSuccessorBlock();

            if (!block || (OperationListIsEmpty(block->_operations) && block->_stages.empty()))
            {
                didChangeIR = true;
                shouldRemove = true;
            }
        }

        return shouldRemove;
    };

    for (BasicBlock& block : function._basicBlocks)
    {
        // Unscheduled operations
        block._operations.remove_if(predicate);

        // Appended stages
        for (Stage& stage : block._stages)
        {
            stage._operations.remove_if(predicate);
        }
    }

    return didChangeIR;
}

// Removes IR assert operations. Should only be run when asserts are disabled.
bool RemoveAsserts(Function& function)
{
    bool didChangeIR = false;

    const auto predicate = [&](const Operation& op)
    {
        bool shouldRemove = false;

        if (op._opcode == Opcode::Assert)
        {
            didChangeIR = true;
            shouldRemove = true;
        }

        return shouldRemove;
    };

    for (BasicBlock& block : function._basicBlocks)
    {
        // Unscheduled operations
        block._operations.remove_if(predicate);

        // Appended stages
        for (Stage& stage : block._stages)
        {
            stage._operations.remove_if(predicate);
        }
    }

    return didChangeIR;
}

// Computes a hash code for an operation
size_t OperationHash(const Operation& op)
{
    size_t result = static_cast<size_t>(op._opcode);

    const auto hashOperands = [](const std::vector<SourceOperand>& srcOperands, size_t& currHashValue)
    {
        for (const SourceOperand& src : srcOperands)
        {
            switch (src.Type())
            {
            case SourceOperandType::Literal:
                boost::hash_combine(currHashValue, src.GetLiteral()._value);
                break;

            case SourceOperandType::Register:
                boost::hash_combine(currHashValue, src.GetAccessedRegister()._registerIndex);
                break;

            case SourceOperandType::Fifo:
                boost::hash_combine(currHashValue, src.GetFifoSubset()._registerIndex);
                break;

            default:
                assert(false);
            }
        }
    };

    // Lut operations are handled specially
    // because 2 seemingly different LUT operations can produce the same result
    // (for example, truth table is permuted)
    if (Opcode::Lut == op._opcode)
    {
        std::vector<SourceOperand> sorted = op._src;

        std::sort(sorted.begin(), sorted.end());

        hashOperands(sorted, result);

        // Lookup table contents are not hashed, because of the permutation problem
        // But lookup table sizes are hashed
        for (size_t i = 0; i < op._flags._lut._numDestinationBits; i++)
        {
            const LutEntry& le = op._flags._lut._lutEntries[i];

            boost::hash_combine(result, le._numSources);
        }
    }
    else
    {
        hashOperands(op._src, result);
    }

    return result;
}

bool LutOperationsProduceSameResult(const Operation& op1, const Operation& op2)
{
    assert(Opcode::Lut == op1._opcode);
    assert(Opcode::Lut == op2._opcode);

    // Sign-extension is baked into truth tables
    assert(op1._signExtendSourceMask == 0);
    assert(op2._signExtendSourceMask == 0);

    const Lut& lut1 = op1._flags._lut;
    const Lut& lut2 = op2._flags._lut;

    if (lut1._numDestinationBits != lut2._numDestinationBits)
    {
        return false;
    }

    for (size_t destIndex = 0; destIndex < lut1._numDestinationBits; destIndex++)
    {
        const LutEntry& le1 = lut1._lutEntries[destIndex];
        const LutEntry& le2 = lut2._lutEntries[destIndex];

        if (le1._numSources != le2._numSources)
        {
            return false;
        }

        const size_t tableSize = le1.TableSize();

        // Returns (register index, bit index) for a given truth table input bit
        const auto getRegAndBit = [](const Operation& op, const LutEntry& le, const size_t sourceIndex)
        {
            assert(sourceIndex < c_maxLutSources);

            return std::pair<size_t, size_t>(
                op._src.at(le._sourceIndices[sourceIndex]).GetAccessedRegister()._registerIndex,
                le._sourceBit[sourceIndex]);
        };

        // If operandMap[i] = j
        // Then bit `i` in the truth table index for op1
        // is equivalent to bit `j` in the truth table index for op2
        std::array<size_t, c_maxLutSources> operandMap = {};

        for (size_t sourceIndex1 = 0; sourceIndex1 < le1._numSources; sourceIndex1++)
        {
            const std::pair<size_t, size_t> regAndBit1 = getRegAndBit(op1, le1, sourceIndex1);

            bool foundEquivalentRegAndBit = false;

            for (size_t sourceIndex2 = 0; sourceIndex2 < le1._numSources; sourceIndex2++)
            {
                const std::pair<size_t, size_t> regAndBit2 = getRegAndBit(op2, le2, sourceIndex2);

                if (regAndBit1 == regAndBit2)
                {
                    assert(!foundEquivalentRegAndBit);
                    operandMap[sourceIndex1] = sourceIndex2;
                    foundEquivalentRegAndBit = true;

                    // No break here to allow the assert to check that a given (register, bit) pair only occurs once
                }
            }

            if (!foundEquivalentRegAndBit)
            {
                return false;
            }
        }

        // For each row in the truth table of op1
        for (size_t truthTableRow1 = 0; truthTableRow1 < tableSize; truthTableRow1++)
        {
            // Determine which row to check in the truth table of op2
            size_t truthTableRow2 = 0;

            for (size_t sourceIndex1 = 0; sourceIndex1 < le1._numSources; sourceIndex1++)
            {
                if ((truthTableRow1 >> sourceIndex1) & 1)
                {
                    // Bit `sourceIndex1` is set in truthTableRow1
                    // Set the corresponding bit in `truthTableRow2`
                    const size_t sourceIndex2 = operandMap[sourceIndex1];

                    truthTableRow2 |= (1ull << sourceIndex2);
                }
            }

            if (le1.GetTableEntry(truthTableRow1) != le2.GetTableEntry(truthTableRow2))
            {
                return false;
            }
        }
    }

    return true;
}

bool OperationsProduceSameResult(const Program& program, const Operation& op1, const Operation& op2)
{
    if (op1._opcode != op2._opcode)
    {
        return false;
    }

    if (op1._src.size() != op2._src.size())
    {
        return false;
    }

    if (op1._dst.size() != op2._dst.size())
    {
        return false;
    }

    // Destination widths must match
    for (size_t i = 0; i < op1._dst.size(); ++i)
    {
        const DestinationOperand& dst1 = op1._dst[i];
        const DestinationOperand& dst2 = op2._dst[i];

        if (dst1.Width(program) != dst2.Width(program))
        {
            return false;
        }
    }

    // LUT operations require special handling
    // because 2 truth tables can be equivalent
    // even if operands are permuted
    const bool isLut = Opcode::Lut == op1._opcode;

    if (isLut)
    {
        if (!LutOperationsProduceSameResult(op1, op2))
        {
            return false;
        }
    }
    else
    {
        if (op1._signExtendSourceMask != op2._signExtendSourceMask)
        {
            return false;
        }

        switch (op1._opcode)
        {
            // These opcodes have no flags
        case Opcode::Mov:
        case Opcode::Select:
            break;

        case Opcode::UnaryOp:
            if (op1._flags._unaryOpType != op2._flags._unaryOpType)
            {
                return false;
            }
            break;

        case Opcode::BinaryOp:
            if (op1._flags._binaryOpType != op2._flags._binaryOpType)
            {
                return false;
            }
            break;

        case Opcode::Gather:
            if (op1._flags._gather._entries->size() != op2._flags._gather._entries->size())
            {
                return false;
            }

            for (size_t i = 0; i < op1._flags._gather._entries->size(); i++)
            {
                if (op1._flags._gather._entries->at(i) != op2._flags._gather._entries->at(i))
                {
                    return false;
                }
            }
            break;

        default:
            // Other opcodes are not supported by common subexpression elimination
            assert(false);
        }

        for (size_t i = 0; i < op1._src.size(); ++i)
        {
            const SourceOperand& src1 = op1._src[i];
            const SourceOperand& src2 = op2._src[i];

            if (src1.Type() != src2.Type())
            {
                return false;
            }

            switch (src1.Type())
            {
            case SourceOperandType::Literal:
                if (src1.GetLiteral() != src2.GetLiteral())
                {
                    return false;
                }
                break;

            case SourceOperandType::Register:
                if (src1.GetAccessedRegister()._registerIndex != src2.GetAccessedRegister()._registerIndex)
                {
                    return false;
                }
                break;

            case SourceOperandType::Fifo:
                if (src1.GetFifoSubset() != src2.GetFifoSubset())
                {
                    return false;
                }
                break;

            default:
                assert(false);
            }
        }
    }

    return true;
}

// Returns the number of bits for an operand, after the promotion of unsigned values to signed if necessary
const size_t GetPromotedWidth(const size_t sizeIn, const bool operandSigned, const bool resultSigned)
{
    size_t result = sizeIn;

    if (resultSigned && !operandSigned)
    {
        // The unsigned operand will have a zero append to the msb
        result++;
    }

    return result;
}

// Decompose a comparison into a subtraction followed by a 1 bit comparison
// A future pass through the IR may decompose the subtraction even further
bool DecomposeCompareCarryChain(Program& program, BasicBlock& basicBlock, OperationList::iterator it)
{
    Operation& op = *it;

    assert(Opcode::BinaryOp == op._opcode);
    assert(2 == op._src.size());
    assert(1 == op._dst.size());

    const std::string dstName = program.GetRegisterName(op._dst[0].GetAccessedRegister());

    // Operation is converted to something like:
    // (lhsOp - rhsOp) < 0
    // The left hand side operand of the subtraction may be src[0] or src[1]
    // The comparison against 0 may be < or >= (check for sign bit being set or not set)
    size_t lhsOpIndex = 0;
    size_t rhsOpIndex = 0;
    bool invertSignBit = false;

    switch (op._flags._binaryOpType)
    {
    case ParseTreeBinaryOpTypeGT:
        // (a > b) -> (b - a) < 0
        lhsOpIndex = 1;
        rhsOpIndex = 0;
        invertSignBit = false;
        break;

    case ParseTreeBinaryOpTypeGE:
        // (a >= b) -> (a - b) >= 0
        lhsOpIndex = 0;
        rhsOpIndex = 1;
        invertSignBit = true;
        break;

    case ParseTreeBinaryOpTypeLT:
        // (a < b) -> (a - b) < 0
        lhsOpIndex = 0;
        rhsOpIndex = 1;
        invertSignBit = false;
        break;

    case ParseTreeBinaryOpTypeLE:
        // (a <= b) -> (b - a) >= 0
        lhsOpIndex = 1;
        rhsOpIndex = 0;
        invertSignBit = true;
        break;

    default:
        assert(false);
        break;
    }

    const SourceOperand& lhs = op._src[lhsOpIndex];
    const SourceOperand& rhs = op._src[rhsOpIndex];

    const bool lhsSigned = op.ShouldSignExtend(lhsOpIndex);
    const bool rhsSigned = op.ShouldSignExtend(rhsOpIndex);

    const bool eitherSigned = lhsSigned || rhsSigned;

    const size_t lhsWidth = GetPromotedWidth(lhs.Width(program), lhsSigned, eitherSigned);
    const size_t rhsWidth = GetPromotedWidth(rhs.Width(program), rhsSigned, eitherSigned);

    // Allocate register for subtraction result
    const size_t subResultWidth = std::max<size_t>(lhsWidth, rhsWidth) + 1;

    const AccessedRegister subResultRegister = {
        AllocateRegister(&program, subResultWidth, RegisterType::Local, dstName + "_diff")};

    // Generation IR operation for subtraction
    Operation subOp = {};

    subOp._opcode = Opcode::BinaryOp;
    subOp._locations = op._locations;
    subOp._flags._binaryOpType = ParseTreeBinaryOpTypeSub;

    subOp._src.push_back(lhs);
    subOp._src.push_back(rhs);

    subOp._dst.push_back(subResultRegister);

    // Ensure subtraction sources are sign extended if necessary
    if (lhsSigned)
    {
        subOp._signExtendSourceMask |= (1ull << 0);
    }

    if (rhsSigned)
    {
        subOp._signExtendSourceMask |= (1ull << 1);
    }

    // Add the subtraction right before the original comparison
    basicBlock._operations.insert(it, subOp);

    // Shift the sign bit to bit 0
    const AccessedRegister shiftResultRegister = {
        AllocateRegister(&program, 1, RegisterType::Local, dstName + "_sign_bit")};

    Operation shiftOp = {};

    shiftOp._opcode = Opcode::BinaryOp;
    shiftOp._locations = op._locations;
    shiftOp._flags._binaryOpType = ParseTreeBinaryOpTypeShr;
    shiftOp._dst.push_back(shiftResultRegister);

    shiftOp.PushOperand(subResultRegister, false);
    shiftOp.PushOperand(subResultWidth - 1, false);

    basicBlock._operations.insert(it, shiftOp);

    // Write the result (optionally inverting it)
    Operation finalOp = {};

    finalOp._locations = op._locations;
    finalOp.PushOperand(shiftResultRegister, false);
    finalOp._dst.push_back(op._dst[0]);

    if (invertSignBit)
    {
        finalOp._opcode = Opcode::UnaryOp;
        finalOp._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;
    }
    else
    {
        // Move the sign bit to the destination
        finalOp._opcode = Opcode::Mov;
    }

    // Replace the original comparison with the final operation
    op = finalOp;

    return true;
}

// Converts == and != operations into pointwise XOR
// followed by a reduction
bool DecomposeCompareEQ(Program& program, BasicBlock& basicBlock, OperationList::iterator it)
{
    Operation& op = *it;

    assert(Opcode::BinaryOp == op._opcode);
    assert(2 == op._src.size());
    assert(1 == op._dst.size());

    const std::string name = program.GetRegisterName(op._dst[0].GetAccessedRegister());

    const bool lhsSigned = op.ShouldSignExtend(0);
    const bool rhsSigned = op.ShouldSignExtend(1);

    const bool eitherSigned = lhsSigned || rhsSigned;

    const size_t lhsWidth = GetPromotedWidth(op._src[0].Width(program), lhsSigned, eitherSigned);
    const size_t rhsWidth = GetPromotedWidth(op._src[1].Width(program), rhsSigned, eitherSigned);

    const ParseTreeBinaryOpType opType = op._flags._binaryOpType;

    // A XOR B
    const size_t intermediateWidth = std::max<size_t>(lhsWidth, rhsWidth);

    const AccessedRegister xorResultRegister = {
        AllocateRegister(&program, intermediateWidth, RegisterType::Local, name + "_xor")};

    const AccessedRegister finalDestRegister = op._dst[0].GetAccessedRegister();

    OperationList::iterator nextIt = it;
    ++nextIt;

    {
        // Copy sources and sign extension masks
        Operation xorOp = op;

        xorOp._opcode = Opcode::BinaryOp;
        xorOp._locations = op._locations;
        xorOp._flags._binaryOpType = ParseTreeBinaryOpTypeXor;
        xorOp._dst[0] = xorResultRegister;

        // Replace the original comparison with XOR
        op = xorOp;
    }

    AccessedRegister reductionTreeInputRegister = {};

    // For ==, invert the result of XOR (before the reduction tree)
    ParseTreeBinaryOpType reduceOp = {};

    if (opType == ParseTreeBinaryOpTypeEQ)
    {
        const AccessedRegister invertResultRegister = {
            AllocateRegister(&program, intermediateWidth, RegisterType::Local, name + "_reduce_eq")};

        Operation invertOp = {};

        invertOp._opcode = Opcode::UnaryOp;
        invertOp._locations = op._locations;

        invertOp._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;

        invertOp._src.push_back(xorResultRegister);
        invertOp._dst.push_back(invertResultRegister);

        reductionTreeInputRegister = invertResultRegister;

        basicBlock._operations.insert(nextIt, invertOp);

        // The reduction operation is AND (the result is true iff all bits in invertResultRegister are 1)
        reduceOp = ParseTreeBinaryOpTypeAnd;
    }
    else
    {
        assert(opType == ParseTreeBinaryOpTypeNE);

        reductionTreeInputRegister = xorResultRegister;

        // The reduction operation is OR (the result is true iff any bit in invertResultRegister is 1)
        reduceOp = ParseTreeBinaryOpTypeOr;
    }

    // Reduction tree to produce final answer
    OperationList reductionTree = ReduceBinaryOp(program, reductionTreeInputRegister, reduceOp, finalDestRegister, op);

    // Insert the reduction tree operations
    basicBlock._operations.splice(nextIt, reductionTree);

    return true;
}

// Create move operation to widen source operand width (with sign extension)
const Operation WidenSourceOperand(Program& program, const SourceOperand& src, const size_t targetWidth,
                                   bool signExtend)
{
    assert(src.Width(program) <= targetWidth);

    const std::string name = (SourceOperandType::Register == src.Type())
                                 ? program.GetRegisterName(src.GetAccessedRegister())
                                 : std::string("Widened");

    const AccessedRegister dstReg{AllocateRegister(&program, targetWidth, RegisterType::Local, name)};

    Operation movOp = {};
    movOp._opcode = Opcode::Mov;
    movOp._src.push_back(src);
    movOp._dst.push_back(dstReg);
    if (signExtend)
    {
        movOp._signExtendSourceMask = 1ull;
    }

    return movOp;
}

// If selectIndex == 0, in src0 is returned
const Operation Mux21(Program& program, const size_t dstWidth, const std::string& dstRegName,
                      const SourceOperand& selectIndex, const SourceOperand& src0, const bool signExtend0,
                      const SourceOperand& src1, const bool signExtend1)
{
    const AccessedRegister dstReg{AllocateRegister(&program, dstWidth, RegisterType::Local, dstRegName)};

    Operation op = {};
    op._opcode = Opcode::Select;
    op.PushOperand(selectIndex, false);
    op.PushOperand(src0, signExtend0);
    op.PushOperand(src1, signExtend1);
    op._dst.push_back(dstReg);

    return op;
}

const Operation BinaryOp(Program& program, const ParseTreeBinaryOpType opType, const size_t dstWidth,
                         const std::string& dstRegName, const SourceOperand& src0, const bool signExtend0,
                         const SourceOperand& src1, const bool signExtend1)
{
    const AccessedRegister dstReg{AllocateRegister(&program, dstWidth, RegisterType::Local, dstRegName)};

    Operation op = {};
    op._opcode = Opcode::BinaryOp;
    op._flags._binaryOpType = opType;
    op.PushOperand(src0, signExtend0);
    op.PushOperand(src1, signExtend1);
    op._dst.push_back(dstReg);

    return op;
}

const Operation Concat(Program& program, const std::string& dstRegName, const std::vector<SourceOperand>& srcOperands)
{
    Operation op = {};
    op._opcode = Opcode::Gather;

    op._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();

    size_t dstWidth = 0;

    for (const SourceOperand& srcOp : srcOperands)
    {
        const size_t srcWidth = srcOp.Width(program);

        op._flags._gather._entries->push_back(GatherEntry{0, dstWidth, srcOp.Width(program)});

        dstWidth += srcWidth;
    }

    op._src = srcOperands;

    const AccessedRegister dstReg{AllocateRegister(&program, dstWidth, RegisterType::Local, dstRegName)};

    op._dst.push_back(dstReg);

    return op;
}

const std::string GetSourceOperandName(const Program& program, const SourceOperand& src)
{
    std::string result;

    if (SourceOperandType::Register == src.Type())
    {
        result = program.GetRegisterName(src.GetAccessedRegister());
    }
    else
    {
        assert(SourceOperandType::Literal == src.Type());

        std::ostringstream str;
        str << "literal_" << src.GetLiteral()._value;

        result = str.str();
    }

    return result;
}

const std::string GetDestOperandName(const Program& program, const DestinationOperand& dst)
{
    assert(DestinationOperandType::Register == dst.Type());

    return program.GetRegisterName(dst.GetAccessedRegister());
}

// Converts a shift operation with a dynamic shift amount
// into a series of shift operations with static shift amounts
bool DecomposeShift(Program& program, BasicBlock& basicBlock, OperationList::iterator it)
{
    const Operation& op = *it;

    assert(Opcode::BinaryOp == op._opcode);
    assert(2 == op._src.size());
    assert(1 == op._dst.size());

    // Only optimize operations that operate on locals
    if (!OperationUsesOnlyLocalReg(program, op))
    {
        return false;
    }

    const SourceOperand& operandToShift = op._src[0];
    const SourceOperand& shiftAmountOperand = op._src[1];

    // Only optimize operations with a dynamic shift amount
    if (SourceOperandType::Register != shiftAmountOperand.Type())
    {
        return false;
    }

    std::string operandToShiftName;

    if (SourceOperandType::Register == operandToShift.Type())
    {
        operandToShiftName = program._registerTable[operandToShift.GetAccessedRegister()._registerIndex]._name;
    }
    else
    {
        std::ostringstream str;

        str << "shift_literal_";
        str << operandToShift.GetLiteral()._value;

        operandToShiftName = str.str();
    }

    const size_t operandToShiftWidth = operandToShift.Width(program);

    const bool isSigned = op.ShouldSignExtend(0);

    const size_t shiftAmountRegister = shiftAmountOperand.GetAccessedRegister()._registerIndex;

    const std::string shiftAmountName = program._registerTable[shiftAmountRegister]._name;

    const size_t shiftAmountWidth = shiftAmountOperand.Width(program);

    if (shiftAmountWidth >= 64)
    {
        // would exceed size_t
        return false;
    }

    AccessedRegister currentValue = {
        AllocateRegister(&program, operandToShiftWidth, RegisterType::Local, operandToShiftName)};

    OperationList ops;

    {
        SetOperationLocation sol(ops, op);

        // Move from the operand to shift into another register=

        {
            Operation mov = {};

            mov._opcode = Opcode::Mov;
            mov._dst.push_back(currentValue);
            mov.PushOperand(op, 0);

            ops.push_back(mov);
        }

        for (size_t shiftAmountBitIndex = 0; shiftAmountBitIndex < shiftAmountWidth; shiftAmountBitIndex++)
        {
            const size_t currentWidth = program._registerTable[currentValue._registerIndex]._width;

            // Extract 1 bit from the shift amount
            const AccessedRegister shiftAmountBit = {AllocateRegister(
                &program, 1, RegisterType::Local, shiftAmountName + "_bit_" + std::to_string(shiftAmountBitIndex))};

            {
                Operation extractBit = {};

                extractBit._opcode = Opcode::BinaryOp;
                extractBit._flags._binaryOpType = ParseTreeBinaryOpTypeShr;
                extractBit.PushOperand(shiftAmountOperand, false);
                extractBit.PushOperand(SourceOperand(shiftAmountBitIndex), false);
                extractBit._dst.push_back(shiftAmountBit);

                ops.push_back(extractBit);
            }

            // Shift the current value by 1 << shiftAmountBitIndex
            assert(shiftAmountBitIndex < 64);
            const size_t staticShiftAmount = 1ull << shiftAmountBitIndex;

            size_t newWidth = 0;

            switch (op._flags._binaryOpType)
            {
            case ParseTreeBinaryOpTypeShl:
                newWidth = currentWidth + staticShiftAmount;
                break;
            case ParseTreeBinaryOpTypeShr:
                newWidth = currentWidth;
                break;
            default:
                assert(false);
                break;
            }

            const AccessedRegister shiftedCurrentValue = {
                AllocateRegister(&program, newWidth, RegisterType::Local,
                                 operandToShiftName + "_shift_choice_" + std::to_string(staticShiftAmount))};

            {
                Operation shiftOp = {};

                shiftOp._opcode = Opcode::BinaryOp;
                shiftOp._flags._binaryOpType = op._flags._binaryOpType;
                shiftOp._dst.push_back(shiftedCurrentValue);
                shiftOp.PushOperand(currentValue, isSigned);
                shiftOp.PushOperand(SourceOperand(staticShiftAmount), false);

                ops.push_back(shiftOp);
            }

            // Select between the current value and the newly shifted value
            const AccessedRegister newValue = {
                AllocateRegister(&program, newWidth, RegisterType::Local,
                                 operandToShiftName + "_shifted_" + std::to_string(staticShiftAmount))};

            {
                Operation muxOp = {};

                muxOp._opcode = Opcode::Select;
                muxOp._dst.push_back(newValue);
                muxOp.PushOperand(shiftAmountBit, false);
                muxOp.PushOperand(currentValue, isSigned);
                muxOp.PushOperand(shiftedCurrentValue, isSigned);

                ops.push_back(muxOp);
            }

            currentValue = newValue;
        }
    }

    basicBlock._operations.splice(it, ops);

    // Replace the original shift with a mov
    {
        Operation mov = {};

        mov._opcode = Opcode::Mov;
        mov._dst.push_back(op._dst[0]);
        mov.PushOperand(currentValue, isSigned);

        mov._locations = op._locations;

        *it = mov;
    }

    return true;
}

// If a select operation has more inputs than "threshold", then it will be decomposed
bool DecomposeSelect(Program& program, BasicBlock& basicBlock, OperationList::iterator it, const size_t threshold)
{
    Operation& originalOp = *it;
    assert(Opcode::Select == originalOp._opcode);
    assert(1 == originalOp._dst.size());
    assert(!originalOp._src.empty());

    if (!OperationUsesOnlyLocalReg(program, originalOp))
    {
        // Avoid decomposing global index values
        return false;
    }

    const SourceOperand& indexOp = originalOp._src[0];

    if (SourceOperandType::Register != indexOp.Type())
    {
        // Other optimizations handle literal source registers
        return false;
    }

    const size_t originalIndexRegister = indexOp.GetAccessedRegister()._registerIndex;

    const RegisterDescription originalIndexDesc = program._registerTable[originalIndexRegister];

    const size_t numChoices = originalOp._src.size() - 1;

    // Don't use the register index here, instead infer it from the number of source operands
    const size_t indexWidth = Log2(numChoices);

    // Can't get smaller than 2 inputs to select among
    assert(threshold >= 2);

    if (numChoices <= threshold)
    {
        return false;
    }

    // Ensure that there are at least "threshold" non-literal source operands
    // Muxes with many literal source operands are not decomposed
    size_t numRegisterSources = 0;

    // Don't count the index register
    for (size_t i = 1; i < originalOp._src.size(); i++)
    {
        const SourceOperand& srcOp = originalOp._src[i];

        if (SourceOperandType::Register == srcOp.Type())
        {
            numRegisterSources++;
        }
    }

    if (numRegisterSources <= threshold)
    {
        return false;
    }

    // Decompose original index into least-significant bits and the remaining bits
    const size_t numLowBits = Log2(threshold);

    assert(indexWidth > numLowBits);
    const size_t numUpperBits = indexWidth - numLowBits;

    const AccessedRegister lowerBitsReg = {
        AllocateRegister(&program, numLowBits, RegisterType::Local, originalIndexDesc._name)};
    const AccessedRegister upperBitsReg = {
        AllocateRegister(&program, indexWidth - numLowBits, RegisterType::Local, originalIndexDesc._name)};

    OperationList ops;

    {
        Operation op = {};

        op._locations = originalOp._locations;

        op._opcode = Opcode::Mov;
        op._dst.push_back(lowerBitsReg);
        op.PushOperand(originalOp, 0);

        ops.push_back(op);
    }

    {
        Operation op = {};

        op._locations = originalOp._locations;

        op._opcode = Opcode::BinaryOp;
        op._flags._binaryOpType = ParseTreeBinaryOpTypeShr;
        op._dst.push_back(upperBitsReg);
        op.PushOperand(originalOp, 0);
        op.PushOperand(SourceOperand(numLowBits), false);

        ops.push_back(op);
    }

    // threshold:1 select operations using the least-significant bit as the input

    // threshold and numChoices should both be powers of 2
    assert(0 == (numChoices % threshold));

    const size_t numIntermediateResults = numChoices / threshold;
    std::vector<AccessedRegister> intermediateResults(numIntermediateResults);

    const DestinationOperand& finalDstOperand = originalOp._dst[0];
    const size_t intermediateWidth = finalDstOperand.Width(program);

    assert(DestinationOperandType::Register == finalDstOperand.Type());

    const std::string intermediateName =
        program._registerTable[finalDstOperand.GetAccessedRegister()._registerIndex]._name;

    for (size_t i = 0; i < numIntermediateResults; i++)
    {
        const AccessedRegister intermediate = {
            AllocateRegister(&program, intermediateWidth, RegisterType::Local, intermediateName)};

        Operation op = {};

        op._locations = originalOp._locations;

        op._opcode = Opcode::Select;

        op._dst.push_back(intermediate);

        op._src.push_back(lowerBitsReg);

        // The "+ 1" is to skip over the index operand
        for (size_t j = 0; j < threshold; j++)
        {
            op.PushOperand(originalOp, (i * threshold) + 1 + j);
        }

        intermediateResults[i] = intermediate;

        ops.push_back(op);
    }

    // New operation to replace the original
    {
        Operation op = {};

        op._locations = originalOp._locations;

        op._opcode = Opcode::Select;

        op._dst.push_back(finalDstOperand);

        op._src.push_back(upperBitsReg);

        for (size_t i = 0; i < numIntermediateResults; i++)
        {
            // No sign extension necessary
            // Because intermediate width == final dst width
            op._src.push_back(intermediateResults[i]);

            assert(op._src.back().Width(program) == finalDstOperand.Width(program));
        }

        basicBlock._operations.splice(it, ops);

        originalOp = op;
    }

    return true;
}

// Gather portions of source operands and add them together
OperationList GatherAndAdd(Program& program, const SourceOperand& src0, const SourceOperand& src1,
                           const size_t src0Start, const size_t src1Start, const size_t src0Width,
                           const size_t src1Width, const AccessedRegister& dst, const uint64_t signExtendSourceMask)
{
    OperationList operationList;

    const std::string name = program.GetRegisterName(dst);

    const AccessedRegister src0_gather{AllocateRegister(&program, src0Width, RegisterType::Local, name + "_subset0")};
    const AccessedRegister src1_gather{AllocateRegister(&program, src1Width, RegisterType::Local, name + "_subset1")};

    Operation gatherOp0 = {};
    gatherOp0._opcode = Opcode::Gather;
    gatherOp0._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();
    gatherOp0._flags._gather._entries->push_back({src0Start, 0, src0Width});
    gatherOp0._src.push_back(src0);
    gatherOp0._dst.push_back(src0_gather);
    operationList.push_back(gatherOp0);

    Operation gatherOp1 = {};
    gatherOp1._opcode = Opcode::Gather;
    gatherOp1._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();
    gatherOp1._flags._gather._entries->push_back({src1Start, 0, src1Width});
    gatherOp1._src.push_back(src1);
    gatherOp1._dst.push_back(src1_gather);
    operationList.push_back(gatherOp1);

    Operation addOp = {};
    addOp._opcode = Opcode::BinaryOp;
    addOp._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;
    addOp._src.push_back(src0_gather);
    addOp._src.push_back(src1_gather);
    addOp._dst.push_back(dst);
    addOp._signExtendSourceMask = signExtendSourceMask;
    operationList.push_back(addOp);

    return operationList;
}

struct DecomposeAddSubConfig
{
    bool _carrySelect;
    size_t _internalWordWidth;
};

// Determines if add/sub operation with a specific width should be decomposed
// Also determines if carry-select should be used to reduce path lengths
boost::optional<DecomposeAddSubConfig> ComputeDecomposeAddSubConfig(const size_t dstWidth)
{
    boost::optional<DecomposeAddSubConfig> result;

    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

    const size_t carryChainWidthPerLogicLevel = codeGenConfig._carryChainWidthPerLogicLevel;

    // Reciprocal of carryChainWidthPerLogicLevel
    // Use to get more accurate estimate of the path length required for an add/sub operation
    // (rather than integer division with floor or ceiling rounding, which discards a lot of information)
    const double levelsOfLogicPerBit = 1.0 / static_cast<double>(carryChainWidthPerLogicLevel);

    const double singleMuxPathLength = static_cast<double>(GetMuxPathLength(2));

    if (codeGenConfig._carrySelect)
    {
        // Logic depth of the operation without any decomposition
        const double baselinePathLength = dstWidth * levelsOfLogicPerBit;

        // Computes the path length the carry-select decomposition
        // given an word width
        const auto carrySelectPathLength = [=](const size_t internalWordWidth)
        {
            // Determine the number of words in the result
            const size_t numWords = (dstWidth + internalWordWidth - 1) / internalWordWidth;

            // +2 to account for input and output carry bits
            const size_t maxWordWidth = internalWordWidth + 2;

            // All adders run in parallel
            const double adderPathLength = maxWordWidth * levelsOfLogicPerBit;

            // Carry/borrow bits propagate through a chain of muxes
            // 1 mux per word (except for for word 0)
            const double muxPathLength = (numWords - 1) * singleMuxPathLength;

            const double totalPathLength = adderPathLength + muxPathLength;

            return totalPathLength;
        };

        // With real numbers (ignoring ceiling associated with division), the function above is:
        //
        // [(internalWordWidth + 2) / carryChainWidthPerLogicLevel] +
        // [(dstWidth / internalWordWidth) - 1] * singleMuxPathLength
        //
        // The function above has 1 minimum
        //
        // The derivative with respect to internalWordWidth is:
        //
        // [1 / carryChainWidthPerLogicLevel] - [(dstWidth * singleMuxPathLength) / (internalWordWidth^2)]
        //
        // The derivative is zero when:
        // [1 / carryChainWidthPerLogicLevel] == [(dstWidth * singleMuxPathLength) / (internalWordWidth^2)]
        //
        // Isolating internalWordWidth the derivative is zero when:
        // internalWordWidth == sqrt((dstWidth * singleMuxPathLength * carryChainWidthPerLogicLevel)
        const double iwwReal = sqrt(static_cast<double>(dstWidth) * singleMuxPathLength *
                                    static_cast<double>(carryChainWidthPerLogicLevel));

        // uint32_t check here to ensure that +1 after the cast will never overflow
        if ((iwwReal >= 1.0) && (iwwReal < static_cast<double>(std::numeric_limits<uint32_t>::max())))
        {
            double bestCarrySelectPathLength = baselinePathLength;

            // Check floor and ceiling around iwwReal
            for (size_t i = 0; i < 2; i++)
            {
                const size_t internalWordWidth = static_cast<size_t>(floor(iwwReal)) + i;

                const double pathLength = carrySelectPathLength(internalWordWidth);

                if (pathLength < bestCarrySelectPathLength)
                {
                    bestCarrySelectPathLength = pathLength;

                    result = DecomposeAddSubConfig{true, internalWordWidth};
                }
            }
        }
    }

    if (!result)
    {
        const size_t logicRegisterRatio = codeGenConfig._logicRegisterRatio;

        const size_t maxWordWidth = carryChainWidthPerLogicLevel * logicRegisterRatio;

        if ((dstWidth > maxWordWidth) && (maxWordWidth > 2))
        {
            // Decompose to enable operation scheduling to stay underneath the register ratio
            // -2 because 2 bits of width are added for input and output carry/borrow bits of each word
            result = DecomposeAddSubConfig{false, maxWordWidth - 2};
        }
    }

    return result;
}

bool DecomposeAddSub(Program& program, BasicBlock& basicBlock, OperationList::iterator it)
{
    // this operation (add)
    Operation& op = *it;
    // next operation
    OperationList::iterator nextIt = it;
    ++nextIt;

    // New operations
    OperationList operationList;

    assert(Opcode::BinaryOp == op._opcode);
    assert((ParseTreeBinaryOpTypeAdd == op._flags._binaryOpType) ||
           (ParseTreeBinaryOpTypeSub == op._flags._binaryOpType));
    assert(2 == op._src.size());
    assert(1 == op._dst.size());

    const bool isAdd = ParseTreeBinaryOpTypeAdd == op._flags._binaryOpType;

    const std::string sumDiffStr = isAdd ? "_sum_" : "_diff_";

    // Get original src and dst
    SourceOperand src0 = op._src[0];
    SourceOperand src1 = op._src[1];
    const DestinationOperand dst = op._dst[0];

    const std::string name = program.GetRegisterName(dst.GetAccessedRegister());

    // Original operand widths
    const size_t src0Width = src0.Width(program);
    const size_t src1Width = src1.Width(program);
    const size_t dstWidth = dst.Width(program);

    // Maximum of input operands
    const size_t maxSrcWidth = std::max(src0Width, src1Width);

    // Apply if adder carry chain exceeds logicRegisterRatio
    const boost::optional<DecomposeAddSubConfig> config = ComputeDecomposeAddSubConfig(dstWidth);

    if (!config)
    {
        // Operation is already narrow enough
        return false;
    }

    const bool carrySelect = (*config)._carrySelect;

    // The loop in this function processes output words with maximum width: internalWordWidth
    // The add/sub operations to produce internalWordWidth output bits may actually produce
    // internalWordWidth + 2 bits (to support carry in and carry out).
    const size_t internalWordWidth = (*config)._internalWordWidth;

    const size_t maxWordWidth = internalWordWidth + 2;

    // Determine the number of words in the result
    const size_t numWords = (dstWidth + internalWordWidth - 1) / internalWordWidth;
    assert(numWords > 1);

    // Get sign information
    const bool dstSigned = op.ShouldSignExtend(0) || op.ShouldSignExtend(1);

    // The carry or borrow bit from the last iteration
    AccessedRegister carryBorrowBit = {};

    // This operation will concatenate all output words
    Operation finalGatherOp = {};
    finalGatherOp._opcode = Opcode::Gather;
    finalGatherOp._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();
    finalGatherOp._dst.push_back(op._dst[0]);

    // For each result word
    for (size_t wordIndex = 0; wordIndex < numWords; wordIndex++)
    {
        const size_t wordStartOffset = wordIndex * internalWordWidth;
        const size_t wordEndOffset = std::min(dstWidth, wordStartOffset + internalWordWidth);
        const size_t wordSize = wordEndOffset - wordStartOffset;

        const bool isFirstWord = (wordIndex == 0);
        const bool isLastWord = (wordIndex == (numWords - 1));

        assert(wordSize > 0);
        assert(Implies(isLastWord, wordEndOffset == dstWidth));

        // If carry-select is enabled
        // then perform the per-word add/sub twice
        // (speculating on the value of the carry/borrow bit)
        const size_t numIterations = (carrySelect && !isFirstWord) ? 2 : 1;

        std::array<AccessedRegister, 2> speculativeOutputWords = {c_invalidAccessedRegisterIndex,
                                                                  c_invalidAccessedRegisterIndex};
        std::array<AccessedRegister, 2> speculativeCarryBorrowBits = {c_invalidAccessedRegisterIndex,
                                                                      c_invalidAccessedRegisterIndex};

        for (size_t speculativeIterationIndex = 0; speculativeIterationIndex < numIterations;
             speculativeIterationIndex++)
        {
            // Extract `wordSize` bits from the correct location in the source operands
            std::array<AccessedRegister, 2> srcWords;

            for (size_t srcOperandIndex = 0; srcOperandIndex < 2; srcOperandIndex++)
            {
                const std::string srcWordName =
                    GetSourceOperandName(program, op._src[srcOperandIndex]) + "_word_" + std::to_string(wordIndex);

                const Operation extractOp =
                    BinaryOp(program, ParseTreeBinaryOpTypeShr, wordSize, srcWordName, op._src[srcOperandIndex],
                             op.ShouldSignExtend(srcOperandIndex), SourceOperand(wordStartOffset), false);

                operationList.push_back(extractOp);

                if (isFirstWord)
                {
                    srcWords[srcOperandIndex] = extractOp._dst[0].GetAccessedRegister();
                }
                else
                {
                    // Implement carry/borrow by appending a least significant bit to both operands
                    std::vector<SourceOperand> concatSourceOperands;

                    SourceOperand concatLsb = {};

                    if (srcOperandIndex == 0)
                    {
                        // The LSB of the left-hand-side of the add/sub becomes 1 (add) or 0 (sub)
                        concatLsb = SourceOperand(Literal{isAdd ? 1 : 0, 1});
                    }
                    else
                    {
                        assert(srcOperandIndex == 1);

                        // The LSB of the right-hand-side of the add/sub becomes the the carry/borrow bit
                        if (carrySelect)
                        {
                            // Carry/borrow bit is speculative
                            concatLsb = SourceOperand(Literal{speculativeIterationIndex, 1});
                        }
                        else
                        {
                            concatLsb = SourceOperand(carryBorrowBit);
                        }
                    }

                    assert(1 == concatLsb.Width(program));
                    concatSourceOperands.push_back(concatLsb);
                    concatSourceOperands.push_back(extractOp._dst[0].GetAccessedRegister());

                    const Operation gatherOp = Concat(program, srcWordName, concatSourceOperands);

                    operationList.push_back(gatherOp);

                    srcWords[srcOperandIndex] = gatherOp._dst[0].GetAccessedRegister();
                }
            }

            // Perform the add/sub operation
            // For all but the last iteration, the output of the add/sub operation is 1 bit wider than the input
            // The MSB of the output is the carry/borrow output
            assert(program._registerTable[srcWords[0]._registerIndex]._width ==
                   program._registerTable[srcWords[1]._registerIndex]._width);
            const size_t addSubOutputWidth =
                program._registerTable[srcWords[0]._registerIndex]._width + (isLastWord ? 0 : 1);

            const Operation addSubOp =
                BinaryOp(program, op._flags._binaryOpType, addSubOutputWidth,
                         GetDestOperandName(program, op._dst[0]) + sumDiffStr + std::to_string(wordIndex), srcWords[0],
                         isLastWord, // sign extend input words on the last iteration only
                         srcWords[1], isLastWord);

            operationList.push_back(addSubOp);

            AccessedRegister addSubOutput = addSubOp._dst[0].GetAccessedRegister();

            if (!isFirstWord)
            {
                // Discard the first bit of the add/subtract
                // the LSB of the inputs was only provided
                // to propagate the carry/borrow
                const Operation discardOp =
                    BinaryOp(program, ParseTreeBinaryOpTypeShr, addSubOutputWidth - 1,
                             GetDestOperandName(program, op._dst[0]) + sumDiffStr + std::to_string(wordIndex),
                             addSubOutput, false, // sign extension doesn't matter, narrowing
                             SourceOperand(1), false);

                operationList.push_back(discardOp);

                addSubOutput = discardOp._dst[0].GetAccessedRegister();
            }

            AccessedRegister outputWord = {};

            if (isLastWord)
            {
                outputWord = addSubOutput;
            }
            else
            {
                const size_t modifiedAddSubOutputWidth = program._registerTable[addSubOutput._registerIndex]._width;
                assert(modifiedAddSubOutputWidth > 0);

                // Decompose the add/sub output into output word and carry/borrow bit
                const mp_int mask = (mp_int(1) << (modifiedAddSubOutputWidth - 1)) - 1;

                const Operation andOp =
                    BinaryOp(program, ParseTreeBinaryOpTypeAnd, modifiedAddSubOutputWidth - 1,
                             GetDestOperandName(program, op._dst[0]) + "_word_" + std::to_string(wordIndex),
                             addSubOutput, false, // sign extension doesn't matter, narrowing
                             SourceOperand(mask), false);

                operationList.push_back(andOp);

                outputWord = andOp._dst[0].GetAccessedRegister();

                const Operation extractCarryBorrowOp =
                    BinaryOp(program, ParseTreeBinaryOpTypeShr, 1,
                             GetDestOperandName(program, op._dst[0]) + "_word_" + std::to_string(wordIndex) +
                                 (isAdd ? "_carry" : "_borrow"),
                             addSubOutput, false, // sign extension doesn't matter, narrowing
                             SourceOperand(modifiedAddSubOutputWidth - 1), false);

                operationList.push_back(extractCarryBorrowOp);

                speculativeCarryBorrowBits[speculativeIterationIndex] =
                    extractCarryBorrowOp._dst[0].GetAccessedRegister();
            }

            speculativeOutputWords[speculativeIterationIndex] = outputWord;
        }

        AccessedRegister chosenOutputWord;

        if (numIterations == 2)
        {
            assert(speculativeOutputWords[0]._registerIndex != c_invalidAccessedRegisterIndex);
            assert(speculativeOutputWords[1]._registerIndex != c_invalidAccessedRegisterIndex);

            // Mux to choose between the 2 words
            assert(program._registerTable[speculativeOutputWords[0]._registerIndex]._width ==
                   program._registerTable[speculativeOutputWords[1]._registerIndex]._width);

            const Operation selectWordOp = Mux21(
                program, program._registerTable[speculativeOutputWords[0]._registerIndex]._width,
                GetDestOperandName(program, op._dst[0]) + "_word_" + std::to_string(wordIndex), carryBorrowBit,
                speculativeOutputWords[0], false, // sign-extension doesn't matter, input and output width are equal
                speculativeOutputWords[1], false);

            operationList.push_back(selectWordOp);

            chosenOutputWord = selectWordOp._dst[0].GetAccessedRegister();

            // Mux to choose the output carry/borrow bit
            if (!isLastWord)
            {
                assert(speculativeCarryBorrowBits[0]._registerIndex != c_invalidAccessedRegisterIndex);
                assert(speculativeCarryBorrowBits[1]._registerIndex != c_invalidAccessedRegisterIndex);

                const Operation selectCarryOp =
                    Mux21(program, 1,
                          GetDestOperandName(program, op._dst[0]) + "_word_" + std::to_string(wordIndex) +
                              (isAdd ? "_carry" : "_borrow"),
                          carryBorrowBit, speculativeCarryBorrowBits[0],
                          false, // sign-extension doesn't matter, input and output width are equal
                          speculativeCarryBorrowBits[1], false);

                operationList.push_back(selectCarryOp);

                carryBorrowBit = selectCarryOp._dst[0].GetAccessedRegister();
            }
        }
        else
        {
            assert(numIterations == 1);

            assert(speculativeOutputWords[0]._registerIndex != c_invalidAccessedRegisterIndex);

            chosenOutputWord = speculativeOutputWords[0];
            carryBorrowBit = speculativeCarryBorrowBits[0];
        }

        finalGatherOp._src.push_back(chosenOutputWord);
        finalGatherOp._flags._gather._entries->push_back({0, wordStartOffset, wordSize});
    }

    operationList.push_back(finalGatherOp);

    // Broadcast the location of the original operation
    // to every new operation
    for (Operation& otherOp : operationList)
    {
        assert(otherOp._locations.empty());
        assert(!op._locations.empty());
        otherOp._locations = op._locations;
    }

    // Replace original op
    op = operationList.front();
    operationList.pop_front();

    // Insert other ops
    basicBlock._operations.splice(nextIt, operationList);

    return true;
}

// Optimizations enabled by constant operands
bool BinaryOpAlgebraicIdentities(Program& program, Operation& op)
{
    assert(Opcode::BinaryOp == op._opcode);
    assert(op._src.size() == 2);
    assert(op._dst.size() == 1);

    // These op types should be replaced before reaching this optimization
    assert(op._flags._binaryOpType != ParseTreeBinaryOpTypeMul);
    assert(op._flags._binaryOpType != ParseTreeBinaryOpTypeDiv);
    assert(op._flags._binaryOpType != ParseTreeBinaryOpTypeLogicalAnd);
    assert(op._flags._binaryOpType != ParseTreeBinaryOpTypeLogicalOr);
    assert(op._flags._binaryOpType != ParseTreeBinaryOpTypeLogicalXor);

    bool result = false;

    size_t srcWidth[2];
    for (size_t i = 0; i < 2; i++)
    {
        srcWidth[i] = op._src[i].Width(program);
    }
    const size_t dstWidth = op._dst[0].Width(program);

    // src op == 0
    bool srcIsZero[2] = {false, false};
    // src op == 1
    bool srcIsOne[2] = {false, false};
    // all bits in src op are 1
    bool srcIsAllOnes[2] = {false, false};
    mp_int allOnes[2];
    for (size_t i = 0; i < 2; i++)
    {
        SourceOperand& srcOp = op._src[i];
        if (SourceOperandType::Literal == srcOp.Type())
        {
            const Literal& literal = srcOp.GetLiteral();
            const mp_int value = literal._value;

            if (value == mp_int(0))
            {
                srcIsZero[i] = true;
            }
            else
            {
                if (value == mp_int(1))
                {
                    srcIsOne[i] = true;
                }

                allOnes[i] = 0;
                bit_set(allOnes[i], srcWidth[i]);
                --allOnes[i];
                if (value == allOnes[i])
                {
                    srcIsAllOnes[i] = true;
                }
            }
        }
    }

    Operation newOp = {};
    newOp._locations = op._locations;

    if (srcIsZero[1])
    {
        switch (op._flags._binaryOpType)
        {
        // op(x, 0) == x
        case ParseTreeBinaryOpTypeOr:
        case ParseTreeBinaryOpTypeXor:
        case ParseTreeBinaryOpTypeAdd:
        case ParseTreeBinaryOpTypeSub:
        case ParseTreeBinaryOpTypeShl:
        case ParseTreeBinaryOpTypeShr:
            newOp._opcode = Opcode::Mov;
            newOp.PushOperand(op, 0);
            newOp._dst.push_back(op._dst[0]);
            result = true;
            break;

        // op(x, 0) == 0
        case ParseTreeBinaryOpTypeAnd:
        case ParseTreeBinaryOpTypeLutMul:
            newOp._opcode = Opcode::Mov;
            newOp._src.push_back(0);
            newOp._dst.push_back(op._dst[0]);
            result = true;
            break;

        default:
            break;
        }
    }
    else if (srcIsZero[0])
    {
        switch (op._flags._binaryOpType)
        {
        // op(0, x) == x
        case ParseTreeBinaryOpTypeOr:
        case ParseTreeBinaryOpTypeXor:
        case ParseTreeBinaryOpTypeAdd:
            newOp._opcode = Opcode::Mov;
            newOp.PushOperand(op, 1);
            newOp._dst.push_back(op._dst[0]);
            result = true;
            break;

        // op(0, x) == 0
        case ParseTreeBinaryOpTypeAnd:
        case ParseTreeBinaryOpTypeShl:
        case ParseTreeBinaryOpTypeShr:
        case ParseTreeBinaryOpTypeLutMul:
            newOp._opcode = Opcode::Mov;
            newOp._src.push_back(0);
            newOp._dst.push_back(op._dst[0]);
            result = true;
            break;

        default:
            break;
        }
    }
    else if (srcIsOne[1] && ParseTreeBinaryOpTypeLutMul == op._flags._binaryOpType)
    {
        // op(x, 1) == x
        newOp._opcode = Opcode::Mov;
        newOp.PushOperand(op, 0);
        newOp._dst.push_back(op._dst[0]);
        result = true;
    }
    else if (srcIsOne[0] && ParseTreeBinaryOpTypeLutMul == op._flags._binaryOpType)
    {
        // op(1, x) == x
        newOp._opcode = Opcode::Mov;
        newOp.PushOperand(op, 1);
        newOp._dst.push_back(op._dst[0]);
        result = true;
    }

    if (!result)
    {
        if (srcIsAllOnes[1])
        {
            // if 1 bits cover all of src0 or dst (for sign extension, all bits will be 1)
            if (srcWidth[1] >= dstWidth || op.ShouldSignExtend(1) ||
                ((srcWidth[1] >= srcWidth[0])) && !op.ShouldSignExtend(0))
            {
                switch (op._flags._binaryOpType)
                {
                // op(x, 1s) = x
                case ParseTreeBinaryOpTypeAnd:
                    newOp._opcode = Opcode::Mov;
                    newOp.PushOperand(op, 0);
                    newOp._dst.push_back(op._dst[0]);
                    result = true;
                    break;
                // op(x, 1s) = 1s
                case ParseTreeBinaryOpTypeOr:
                    newOp._opcode = Opcode::Mov;
                    newOp.PushOperand(op, 1);
                    newOp._dst.push_back(op._dst[0]);
                    result = true;
                    break;
                // op(x, 1s) = ~x
                case ParseTreeBinaryOpTypeXor:
                    newOp._opcode = Opcode::UnaryOp;
                    newOp._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;
                    newOp.PushOperand(op, 0);
                    newOp._dst.push_back(op._dst[0]);
                    result = true;
                    break;

                default:
                    break;
                }
            }
        }
        else if (srcIsAllOnes[0])
        {
            // if 1 bits cover all of src1 or dst (for sign extension, all bits will be 1)
            if (srcWidth[0] >= dstWidth || op.ShouldSignExtend(0) ||
                ((srcWidth[0] >= srcWidth[1]) && !op.ShouldSignExtend(1)))
            {
                switch (op._flags._binaryOpType)
                {
                // op(1s, x) = x
                case ParseTreeBinaryOpTypeAnd:
                    newOp._opcode = Opcode::Mov;
                    newOp.PushOperand(op, 1);
                    newOp._dst.push_back(op._dst[0]);
                    result = true;
                    break;
                // op(1s, x) = 1s
                case ParseTreeBinaryOpTypeOr:
                    newOp._opcode = Opcode::Mov;
                    newOp.PushOperand(op, 0);
                    newOp._dst.push_back(op._dst[0]);
                    result = true;
                    break;
                // op(1s, x) = ~x
                case ParseTreeBinaryOpTypeXor:
                    newOp._opcode = Opcode::UnaryOp;
                    newOp._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;
                    newOp.PushOperand(op, 1);
                    newOp._dst.push_back(op._dst[0]);
                    result = true;
                    break;

                default:
                    break;
                }
            }
        }
    }

    if (!result)
    {
        switch (op._flags._binaryOpType)
        {
        case ParseTreeBinaryOpTypeShl:
            if (SourceOperandType::Literal == op._src[1].Type())
            {
                if (op._src[1].GetLiteral()._value >= mp_int(dstWidth))
                {
                    // y = x << i (where i is >= the width of y)
                    // ->
                    // y == 0
                    newOp._opcode = Opcode::Mov;
                    newOp._dst.push_back(op._dst[0]);
                    newOp._src.push_back(0);

                    result = true;
                }
            }
            break;

        case ParseTreeBinaryOpTypeShr:
            if (SourceOperandType::Literal == op._src[1].Type())
            {
                const size_t src0Width = op._src[0].Width(program);

                if (!op.ShouldSignExtend(0) && (op._src[1].GetLiteral()._value >= mp_int(src0Width)))
                {
                    // x >> i (where i is >= the width of x, and x is unsigned)
                    // ->
                    // x == 0
                    newOp._opcode = Opcode::Mov;
                    newOp._dst.push_back(op._dst[0]);
                    newOp._src.push_back(0);

                    result = true;
                }
            }
            break;

        default:
            break;
        }
    }

    if (result)
    {
        op = newOp;
    }

    return result;
}

bool SelectAlgebraicIdentities(Program& program, Operation& op)
{
    assert(Opcode::Select == op._opcode);
    assert(3 <= op._src.size());
    assert(1 == op._dst.size());

    bool didChangeIR = false;

    Operation newOp = {};
    newOp._locations = op._locations;

    const SourceOperand& src0 = op._src[0];
    if (SourceOperandType::Literal == src0.Type())
    {
        // The index is known at compile time
        const size_t index = MpToSizeT(src0.GetLiteral()._value);

        // op._src[0] is the index
        // op._src[1] is the first choice
        const size_t whichSrcOperand = index + 1;
        assert(whichSrcOperand < op._src.size());

        newOp._opcode = Opcode::Mov;
        newOp._dst.push_back(op._dst[0]);
        newOp._src.push_back(op._src[whichSrcOperand]);

        // Sign extend if necessary
        newOp._signExtendSourceMask = (op._signExtendSourceMask >> whichSrcOperand) & 1;

        op = newOp;

        return true;
    }

    // Check if all choices are literals or all registers
    bool allLiterals = true;
    bool allRegisters = true;
    for (size_t i = 1; i < op._src.size(); i++)
    {
        const SourceOperandType& type = op._src[i].Type();
        if (type != SourceOperandType::Literal)
        {
            allLiterals = false;
        }
        if (type != SourceOperandType::Register)
        {
            allRegisters = false;
        }
    }

    if (allLiterals)
    {
        const size_t dstWidth = op._dst[0].Width(program);
        const size_t selWidth = op._src[0].Width(program);

        // Check if all choice literals match
        bool allEqual = true;
        const mp_int firstValue = Resize(op._src[1].GetLiteral(), op.ShouldSignExtend(1), dstWidth);
        // mux(sel, 0, 1, 2, 3, ...) == sel
        bool choiceEqualSel = true;
        // mux(sel, 1, 0) == ~sel
        bool choiceEqualInvSel = true;
        for (size_t i = 1; i < op._src.size(); i++)
        {
            // Widen/truncate all choices to match dst width
            const mp_int thisValue = Resize(op._src[i].GetLiteral(), op.ShouldSignExtend(i), dstWidth);

            if (thisValue != firstValue)
            {
                allEqual = false;
            }

            // Select value for each choice
            const Literal selLiteral = {i - 1, selWidth};
            const mp_int selValue = Resize(selLiteral, op.ShouldSignExtend(0), dstWidth);

            if (thisValue != selValue)
            {
                choiceEqualSel = false;
            }

            // Invert select value
            const mp_int invSel =
                ImplementUnaryOp(selLiteral, op.ShouldSignExtend(0), ParseTreeUnaryOpTypeInvert, dstWidth);

            if (thisValue != invSel)
            {
                choiceEqualInvSel = false;
            }
        }

        // Replace with move if all choices match
        if (allEqual)
        {
            newOp._opcode = Opcode::Mov;
            newOp._dst.push_back(op._dst[0]);
            newOp.PushOperand(op, 1);

            op = newOp;
            return true;
        }

        // Replace with move if choices match select
        if (choiceEqualSel)
        {
            newOp._opcode = Opcode::Mov;
            newOp.PushOperand(op, 0);
            newOp._dst.push_back(op._dst[0]);

            op = newOp;
            return true;
        }

        // Replace with invert if choices match inverse of select
        if (choiceEqualInvSel)
        {
            newOp._opcode = Opcode::UnaryOp;
            newOp._flags._unaryOpType = ParseTreeUnaryOpTypeInvert;
            newOp.PushOperand(op, 0);
            newOp._dst.push_back(op._dst[0]);

            op = newOp;
            return true;
        }
    }

    if (allRegisters)
    {
        // All choices are same register
        bool allEqual = true;
        const bool shouldSignExtend = op.ShouldSignExtend(1);
        const size_t width = op._src[1].Width(program);
        const size_t registerIndex = op._src[1].GetAccessedRegister()._registerIndex;
        for (size_t i = 2; i < op._src.size(); i++)
        {
            // Register and sign extend must match
            if (op.ShouldSignExtend(i) != shouldSignExtend ||
                op._src[i].GetAccessedRegister()._registerIndex != registerIndex)
            {
                allEqual = false;
                break;
            }
        }

        if (allEqual)
        {
            newOp._opcode = Opcode::Mov;
            newOp.PushOperand(op, 1);
            newOp._dst.push_back(op._dst[0]);

            op = newOp;
            return true;
        }
    }

    // No optimizations found
    return false;
}

bool AlgebraicIdentities(Program& program, Function& function)
{
    bool didChangeIR = false;

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        for (Operation& op : basicBlock._operations)
        {
            switch (op._opcode)
            {
            case Opcode::BinaryOp:
                if (BinaryOpAlgebraicIdentities(program, op))
                {
                    didChangeIR = true;
                }
                break;

            case Opcode::Select:
                if (SelectAlgebraicIdentities(program, op))
                {
                    didChangeIR = true;
                }
                break;

            default:
                break;
            }
        }
    }

    return didChangeIR;
}

bool ClearPredicate(Operation& op)
{
    bool result = true;

    switch (op._opcode)
    {
    case Opcode::Assert:
        assert(op._src.size() == 3);
        op._src.resize(2);
        break;

    case Opcode::WriteGlobal:
        assert(op._flags._writeGlobal._isPredicated);
        assert(op._src.size() > 1);
        op._src.erase(op._src.begin() + 1);
        op._flags._writeGlobal._isPredicated = false;
        break;

    case Opcode::Enqueue:
        assert(op._flags._enqueue._isPredicated);
        assert(op._src.size() >= 1);
        op._src.erase(op._src.begin() + 0);
        op._flags._enqueue._isPredicated = false;
        break;

    case Opcode::StoreMemory:
        assert(op._flags._storeMemory._isPredicated);
        assert(op._src.size() > 1);
        op._src.erase(op._src.begin() + 1);
        op._flags._storeMemory._isPredicated = false;
        break;

    case Opcode::LoadMemory:
        assert(op._flags._loadMemory._isPredicated);
        assert(op._src.size() > 2);
        op._src.erase(op._src.begin() + 2);
        op._flags._loadMemory._isPredicated = false;
        break;

    case Opcode::ReferenceString:
        // ReferenceString does not support clearing the predicate
        result = false;
        break;

    default:
        break;
    }

    return result;
}

bool IsPredicatedConstant(Program& program, const Operation& op, bool& isPredicatedTrue)
{
    bool isPredicated = false;
    size_t srcIndex = 0;
    size_t predicatedValue = 1;

    switch (op._opcode)
    {
    case Opcode::Assert:
        isPredicated = (3 == op._src.size());
        srcIndex = 2;
        break;

    case Opcode::WriteGlobal:
        isPredicated = op._flags._writeGlobal._isPredicated;
        srcIndex = 1;
        break;

    case Opcode::Enqueue:
        isPredicated = op._flags._enqueue._isPredicated;
        srcIndex = 0;
        predicatedValue = (op._flags._enqueue._predicateExecutionValue) ? 1 : 0;
        break;

    case Opcode::StoreMemory:
        isPredicated = op._flags._storeMemory._isPredicated;
        srcIndex = 1;
        break;

    case Opcode::LoadMemory:
        isPredicated = op._flags._loadMemory._isPredicated;
        srcIndex = 2;
        break;

    case Opcode::ReferenceString:
        isPredicated = true;
        srcIndex = 0;
        break;

    default:
        break;
    }

    if (isPredicated)
    {
        const SourceOperand& sourceOperand = op._src[srcIndex];

        if (sourceOperand.Type() == SourceOperandType::Literal)
        {
            isPredicatedTrue = (sourceOperand.GetLiteral()._value == predicatedValue);
            return true;
        }
    }

    return false;
}

bool PredicateIdentities(Program& program, Function& function)
{
    bool didChangeIR = false;

    auto callback = [&](Operation& op)
    {
        bool isPredicatedTrue = false;

        if (IsPredicatedConstant(program, op, isPredicatedTrue) && isPredicatedTrue)
        {
            if (ClearPredicate(op))
            {
                didChangeIR = true;
            }
        }
    };

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        ForEachOperationForward(basicBlock, callback);
    }

    return didChangeIR;
}

bool PredicateFalsities(Program& program, Function& function)
{
    bool didChangeIR = false;

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        std::set<std::pair<size_t, std::set<FileAndLineNumber>>> registersToZero;

        auto callback = [&](const Operation& op) -> bool
        {
            bool isPredicatedTrue = false;

            // Remove if the two constants differ
            if (IsPredicatedConstant(program, op, isPredicatedTrue) && !isPredicatedTrue)
            {
                didChangeIR = true;

                if (Opcode::Enqueue == op._opcode)
                {
                    BasicBlock* const successor = op._getSuccessorBlock();

                    if (2 == successor->_inputFifoCount)
                    {
                        // Removing the loop entrance fifo is not supported
                        // If a loop is contained in a control flow construct
                        // and the control flow predicate is false
                        // execution still continues through the loop (predicated off)
                        assert(1 == op._flags._enqueue._whichFifo);

                        // Removing the backward link fifo
                        successor->_isOneBBLoop = false;
                        successor->_oneBBLoopOrdered = false;

                        successor->_inputFifoCount = 1;
                    }
                }

                // If the operation writes to local registers
                // then record that additional operations must be added to the IR
                // to avoid leaving these registers uninitialized
                for (const DestinationOperand& dstOp : op._dst)
                {
                    if (DestinationOperandType::Register == dstOp.Type())
                    {
                        const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

                        if (RegisterType::Local == program._registerTable[registerIndex]._type)
                        {
                            registersToZero.insert(
                                std::pair<size_t, std::set<FileAndLineNumber>>(registerIndex, op._locations));
                        }
                    }
                }

                return true;
            }
            return false;
        };

        RemoveOperations(basicBlock, callback);

        for (const auto& p : registersToZero)
        {
            Operation op = {};

            op._opcode = Opcode::Mov;
            op._src.push_back(SourceOperand(0));
            op._dst.push_back(AccessedRegister{p.first});
            op._locations = p.second;

            basicBlock._operations.push_front(op);
        }
    }

    return didChangeIR;
}

// Sets Literal::_width to a minimal value
bool NarrowLiterals(Program& program, Function& function)
{
    bool didChangeIR = false;

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        ForEachOperationForward(basicBlock,
                                [&](Operation& op)
                                {
                                    for (size_t srcOpIndex = 0; srcOpIndex < op._src.size(); srcOpIndex++)
                                    {
                                        SourceOperand& srcOp = op._src[srcOpIndex];

                                        if (SourceOperandType::Literal == srcOp.Type())
                                        {
                                            Literal& literal = srcOp.GetLiteral();

                                            const size_t originalWidth = literal._width;

                                            if (op.ShouldSignExtend(srcOpIndex))
                                            {
                                                // Remove the most significant bit as long as the
                                                // 2 most significant bits match
                                                while (literal._width >= 2)
                                                {
                                                    if (bit_test(literal._value, literal._width - 1) ==
                                                        bit_test(literal._value, literal._width - 2))
                                                    {
                                                        literal._width--;
                                                    }
                                                    else
                                                    {
                                                        break;
                                                    }
                                                }
                                            }
                                            else
                                            {
                                                // remove leading zeros
                                                if (literal._value == 0)
                                                {
                                                    literal._width = 1;
                                                }
                                                else
                                                {
                                                    const size_t highestBitSet = msb(literal._value);

                                                    const size_t tightWidth = highestBitSet + 1;

                                                    if (literal._width > tightWidth)
                                                    {
                                                        literal._width = tightWidth;
                                                    }
                                                }
                                            }

                                            assert(literal._width <= originalWidth);

                                            if (originalWidth != literal._width)
                                            {
                                                // Removing bits from literal._value beyond the specified width
                                                const mp_int mask = (mp_int(1) << literal._width) - 1;

                                                literal._value = literal._value & mask;

                                                didChangeIR = true;
                                            }
                                        }
                                    }
                                });
    }

    return didChangeIR;
}

// Converts gather operations to mov operations
// which can then be optimized by optimizations like copy propagation
bool GatherToMove(Program& program, Function& function)
{
    bool didChangeIR = false;

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        ForEachOperationForward(basicBlock,
                                [&](Operation& op)
                                {
                                    if ((Opcode::Gather == op._opcode) && (1 == op._src.size()))
                                    {
                                        assert(1 == op._dst.size());
                                        assert(1 == op._flags._gather._entries->size());

                                        const GatherEntry& ge = op._flags._gather._entries->front();

                                        // The check on the source width could potentially be loosened
                                        // by taking advantage of implicit sign-extension in the IR
                                        if ((ge._sourceOffset == 0) && (ge._destOffset == 0) &&
                                            (ge._numBits == op._src[0].Width(program)) &&
                                            (ge._numBits == op._dst[0].Width(program)))
                                        {
                                            op._opcode = Opcode::Mov;
                                            didChangeIR = true;
                                        }
                                    }
                                });
    }

    return didChangeIR;
}

// Input:
// X + Y
//
// Output:
// X | Y
//
// Only applies when the lower bits of X are 0
bool AddToOr(Program& program, Function& function)
{
    bool didChangeIR = false;

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        LocalDefUseTracker defUseTracker(program, basicBlock);

        LutArenaAllocator allocator;

        for (Operation& op : basicBlock._operations)
        {
            defUseTracker.HandleOperation(op, CanOpcodeBeLoweredToLut(op._opcode));

            if ((Opcode::BinaryOp == op._opcode) && (ParseTreeBinaryOpTypeAdd == op._flags._binaryOpType) &&
                (0 == op._signExtendSourceMask))
            {
                // X and Y can be in either operand
                // because add is commutative
                for (size_t narrowOpIdx = 0; narrowOpIdx < 2; narrowOpIdx++)
                {
                    // If the transformation occurs of narrowOpIdx==0
                    // then the iteration where narrowOpIdx==1 shoul dnot run
                    assert(ParseTreeBinaryOpTypeAdd == op._flags._binaryOpType);

                    const size_t wideOpIdx = 1 - narrowOpIdx;

                    const SourceOperand& narrowOp = op._src[narrowOpIdx];
                    const SourceOperand& wideOp = op._src[wideOpIdx];

                    const size_t narrowWidth = narrowOp.Width(program);

                    if (narrowWidth < wideOp.Width(program))
                    {
                        if (SourceOperandType::Register == wideOp.Type())
                        {
                            const Operation* const wideDef =
                                defUseTracker.GetDef(wideOp.GetAccessedRegister()._registerIndex);

                            if (wideDef)
                            {
                                assert(CanOpcodeBeLoweredToLut(wideDef->_opcode));
                                assert(1 == wideDef->_dst.size());

                                // Attempt to convert the wide operand to a lut
                                // Only for the purpose of determining which bits are constant zero
                                Operation wideDefAsLut = *wideDef;

                                if (LowerToLut(program, wideDefAsLut, allocator.LutFn()))
                                {
                                    assert(narrowWidth <= wideDefAsLut._flags._lut._numDestinationBits);

                                    // Check to see if all low bits of wideOp are known to be 0
                                    bool allLowBitsZero = true;

                                    for (size_t i = 0; i < narrowWidth; i++)
                                    {
                                        const LutEntry& lutEntry = wideDefAsLut._flags._lut._lutEntries[i];

                                        if (!lutEntry.IsConstantZero())
                                        {
                                            allLowBitsZero = false;
                                            break;
                                        }
                                    }

                                    if (allLowBitsZero)
                                    {
                                        // For each bit of the narrow op, the corresponding bit of the wide op
                                        // is 0, so the addition is equivalent to a bitwise or.
                                        op._flags._binaryOpType = ParseTreeBinaryOpTypeOr;
                                        didChangeIR = true;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return didChangeIR;
}

// Converts wide operations into smaller ones (to improve timing)
// Also converts some comparisons into subtractions followed by a 1-bit comparison (again to improve timing)
bool DecomposeWideOps(Program& program, Function& function)
{
    bool didChangeIR = false;

    const size_t maxSelectInputs = GetCodeGenConfig()._maxSelectInputs;

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        OperationList::iterator nextIt;

        size_t atomicBlockDepth = 0;

        for (OperationList::iterator it = basicBlock._operations.begin(); it != basicBlock._operations.end();
             it = nextIt)
        {
            // Get the iterator for the next iteration before any optimization has a change to insert more operations
            // (or erase *it)
            nextIt = it;
            ++nextIt;

            Operation& op = *it;

            if (Opcode::BeginAtomic == op._opcode)
            {
                atomicBlockDepth++;
            }
            else if (Opcode::EndAtomic == op._opcode)
            {
                assert(atomicBlockDepth > 0);
                atomicBlockDepth--;
            }

            // Don't decompose operations in atomic blocks
            // as more pipeline stages cannot be added
            if (atomicBlockDepth == 0)
            {
                if (Opcode::BinaryOp == op._opcode)
                {
                    assert(2 == op._src.size());
                    assert(1 == op._dst.size());

                    switch (op._flags._binaryOpType)
                    {
                    // These comparisons are decomposed regardless of width and mode
                    case ParseTreeBinaryOpTypeGT:
                    case ParseTreeBinaryOpTypeGE:
                    case ParseTreeBinaryOpTypeLT:
                    case ParseTreeBinaryOpTypeLE:
                        if (DecomposeCompareCarryChain(program, basicBlock, it))
                        {
                            didChangeIR = true;
                        }
                        break;

                    case ParseTreeBinaryOpTypeEQ:
                    case ParseTreeBinaryOpTypeNE:
                        if (DecomposeCompareEQ(program, basicBlock, it))
                        {
                            didChangeIR = true;
                        }
                        break;

                    case ParseTreeBinaryOpTypeAdd:
                    case ParseTreeBinaryOpTypeSub:
                        if (DecomposeAddSub(program, basicBlock, it))
                        {
                            didChangeIR = true;
                        }
                        break;

                    default:
                        break;
                    }
                }
                else if (Opcode::Select == op._opcode)
                {
                    if (DecomposeSelect(program, basicBlock, it, maxSelectInputs))
                    {
                        didChangeIR = true;
                    }
                }
            }
        }

        assert(atomicBlockDepth == 0);
    }

    return didChangeIR;
}

// Finds select operations where some bits of the select index are
// known to be constant.  Rewrites the select operation remove
// select sources which are impossible to select from
bool ConstSelectBits(Program& program, Function& function)
{
    bool didChangeIR = false;

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        LocalDefUseTracker defUseTracker(program, basicBlock);

        LutArenaAllocator allocator;

        for (auto it = basicBlock._operations.begin(); it != basicBlock._operations.end(); ++it)
        {
            Operation& op = *it;

            defUseTracker.HandleOperation(op, CanOpcodeBeLoweredToLut(op._opcode));

            if ((Opcode::Select == op._opcode) && !op.ShouldSignExtend(0))
            {
                const SourceOperand& selectIndexOp = op._src[0];

                const size_t selectIndexWidth = selectIndexOp.Width(program);

                if (SourceOperandType::Register == selectIndexOp.Type())
                {
                    const Operation* const selectIndexDef =
                        defUseTracker.GetDef(selectIndexOp.GetAccessedRegister()._registerIndex);

                    if (selectIndexDef)
                    {
                        // Attempt to convert the select index definition into a LUT
                        // Only for the purpose of determining which bits are constants
                        Operation selectIndexAsLut = *selectIndexDef;

                        if (LowerToLut(program, selectIndexAsLut, allocator.LutFn()))
                        {
                            std::map<size_t, size_t> constantSelectBits;
                            std::set<size_t> dynamicSelectBits;

                            for (size_t bitIndex = 0; bitIndex < selectIndexAsLut._flags._lut._numDestinationBits;
                                 bitIndex++)
                            {
                                const LutEntry& lutEntry = selectIndexAsLut._flags._lut._lutEntries[bitIndex];

                                if (0 == lutEntry._numSources)
                                {
                                    assert(lutEntry._table < 2);

                                    constantSelectBits[bitIndex] = lutEntry._table;
                                }
                                else
                                {
                                    dynamicSelectBits.insert(bitIndex);
                                }
                            }

                            // The < selectIndexWidth term is to avoid
                            // dealing with the case where all select index bits are constants
                            // a separate optimization handles that
                            if ((constantSelectBits.size() != 0) && (constantSelectBits.size() < selectIndexWidth))
                            {
                                const size_t numDynamicSelectBits = dynamicSelectBits.size();
                                assert(numDynamicSelectBits > 0);

                                const AccessedRegister compactedSelectIndex = {AllocateRegister(
                                    &program, numDynamicSelectBits, RegisterType::Local,
                                    program._registerTable[selectIndexOp.GetAccessedRegister()._registerIndex]._name)};

                                // Emit an operation which generates a new select index
                                Operation gatherOp = {};

                                gatherOp._locations = op._locations;

                                gatherOp._opcode = Opcode::Gather;

                                gatherOp._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();

                                gatherOp._dst.push_back(compactedSelectIndex);

                                size_t dstIdx = 0;

                                for (const size_t i : dynamicSelectBits)
                                {
                                    GatherEntry ge = {};
                                    ge._sourceOffset = i;
                                    ge._destOffset = dstIdx;
                                    ge._numBits = 1;

                                    gatherOp._flags._gather._entries->push_back(ge);

                                    gatherOp._src.push_back(selectIndexOp);

                                    dstIdx++;
                                }

                                assert(dstIdx == numDynamicSelectBits);

                                basicBlock._operations.insert(it, gatherOp);

                                // Modify 'op' to use the new select index
                                const Operation referenceOp = op;

                                op = {};

                                op._locations = referenceOp._locations;

                                op._dst = referenceOp._dst;

                                op._opcode = Opcode::Select;

                                op.PushOperand(compactedSelectIndex, false);

                                for (size_t i = 1; i < referenceOp._src.size(); i++)
                                {
                                    const size_t whichSource = i - 1;

                                    // For each bit `c` in constantSelectBits,
                                    // check to see if bit `c` in `whichSource`
                                    // has the same constant value
                                    bool validSource = true;

                                    for (const auto& p : constantSelectBits)
                                    {
                                        const size_t bitIndex = p.first;
                                        const size_t expectedValue = p.second;

                                        if (expectedValue != ((whichSource >> bitIndex) & 1))
                                        {
                                            validSource = false;
                                            break;
                                        }
                                    }

                                    if (validSource)
                                    {
                                        op.PushOperand(referenceOp, i);
                                    }
                                }

                                assert(op._src.size() == ((1ull << numDynamicSelectBits) + 1));

                                didChangeIR = true;
                            }
                        }
                    }
                }
            }
        }
    }

    return didChangeIR;
}

// Input:
// C = op1(A, B)
// D = op1(A, B)
//
// Output:
// C = op1(A, B)
// D = C
bool CommonSubexpressionElimination(Program& program, Function& function, const bool transformLuts)
{
    bool didChangeIR = false;

    const auto doesOpSupportCSE = [&](const Operation& op)
    {
        // Only a subset of opcodes are supported, to limit testing requirements
        if (!DoesOpcodeSupportCommonSubexpressionElimination(op._opcode))
        {
            return false;
        }

        // if the transformLuts parameter is false
        // then do not consider LUTs for optimization
        if ((Opcode::Lut == op._opcode) && !transformLuts)
        {
            return false;
        }

        if (Opcode::Mov == op._opcode)
        {
            if (op._dst[0].Width(program) == op._src[0].Width(program))
            {
                // Move operations which do not change sizes are not considered for CSE
                // as CSE would simply replace a move with another move
                // Usually these operations will be eliminated by copy propagation in another pass.
                //
                // Move operations which do change sizes are important to optimize to enable
                // optimizations which detect 2 operations with a common source operand
                return false;
            }

            if (SourceOperandType::Register != op._src[0].Type())
            {
                // Do not try to optimize moves from a register
                // There is nothing to be gained, and it causes optimization to not converge
                // For example:
                // input:
                // a = 5
                // b = 5
                //
                // output:
                // a = 5
                // b = 5
                // (no change, but CommonSubexpressionElimination() returns true)
                return false;
            }
        }

        // Don't combine operations which access shared registers
        // For example, a shared variable could be read at 2 different stages in the pipeline.
        // These reads cannot be combined, because the value of the variable can change
        // between the time the first and second reads occur.
        if (!OperationUsesOnlyLocalReg(program, op))
        {
            return false;
        }

        return true;
    };

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        // Maps hash of operation to list of operations with that hash
        std::map<size_t, std::list<Operation*>> operationMap;

        for (Operation& op : basicBlock._operations)
        {
            // This optimization only applies to operations with 1 destination
            if (1 != op._dst.size())
            {
                continue;
            }

            // The destination must be a local register
            if (DestinationOperandType::Register == op._dst[0].Type())
            {
                if (!doesOpSupportCSE(op))
                {
                    continue;
                }

                // See if there was a previous operation that matches `op` (in everything except destination index)
                const std::list<Operation*>& candidateList = operationMap[OperationHash(op)];

                auto it = std::find_if(candidateList.begin(), candidateList.end(),
                                       [&](Operation* const candidate)
                                       { return OperationsProduceSameResult(program, op, *candidate); });
                if (it != candidateList.end())
                {
                    Operation* const candidate = *it;

                    // Replace the current operation with a move of the previous result
                    Operation newOp = {};

                    newOp._locations = op._locations;

                    newOp._opcode = Opcode::Mov;

                    // Record that the candidate operation is now associated
                    // with the location of the operation that is being turned into a mov
                    Union(candidate->_locations, newOp._locations);

                    newOp._src.push_back(SourceOperand(candidate->_dst[0].GetAccessedRegister()));
                    newOp._dst.push_back(op._dst[0].GetAccessedRegister());

                    op = newOp;

                    didChangeIR = true;
                }
                else
                {
                    operationMap[OperationHash(op)].push_back(&op);
                }
            }
        }
    }

    return didChangeIR;
}

RegisterReadCountMap ComputeRegisterReadCountMap(const Program& program, const BasicBlock& basicBlock)
{
    RegisterReadCountMap result;

    for (const Operation& op : basicBlock._operations)
    {
        for (const SourceOperand& srcOp : op._src)
        {
            if (SourceOperandType::Register == srcOp.Type())
            {
                const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                result[registerIndex]++;
            }
        }
    }

    return result;
}

// Input:
// b = ConditionalIgnore(a, p)
// c = op(a)
//
// ConditionalIgnore input data register had other uses
// Remove the ConditionalIgnore, to avoid duplicate pipeline registers
// with the only difference being that one has a control gate and the other does not
//
// Output:
// b = Mov(a)
// c = op(a)
bool ParallelConditionalIgnore(const Program& program, Function& function)
{
    bool didChangeIR = false;

    for (BasicBlock& bb : function._basicBlocks)
    {
        const RegisterReadCountMap registerReads = ComputeRegisterReadCountMap(program, bb);

        ForEachOperationForward(bb,
                                [&](Operation& op)
                                {
                                    if (Opcode::ConditionalIgnore == op._opcode)
                                    {
                                        assert(!op._src.empty());

                                        const SourceOperand& srcOp = op._src[0];

                                        if (SourceOperandType::Register == srcOp.Type())
                                        {
                                            const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                                            const auto it = registerReads.find(registerIndex);

                                            const size_t numReads = (it == registerReads.end()) ? 0 : it->second;

                                            if (numReads > 1)
                                            {
                                                op._opcode = Opcode::Mov;
                                                op._src.resize(1);

                                                didChangeIR = true;
                                            }
                                        }
                                    }
                                });
    }

    return didChangeIR;
}

// Constant propagation for ConditionalIgnore
bool ConditionalIgnoreConstantProp(const Program& program, Function& function)
{
    // Early out if no clock gating is enabled
    if (!GetCodeGenConfig().ControlClockGatingEnabled())
    {
        return false;
    }

    bool didChangeIR = false;

    for (BasicBlock& bb : function._basicBlocks)
    {
        const auto constantValueCallback = [&](Operation& op)
        {
            if (Opcode::ConditionalIgnore != op._opcode)
            {
                return;
            }

            assert(0 == op._signExtendSourceMask);
            assert(op._dst.size() == 1);
            assert(op._src.size() >= 1);

            const SourceOperand& valueOperand = op._src[0];

            if (SourceOperandType::Literal == valueOperand.Type())
            {
                didChangeIR = true;

                // Value is known at compile time
                // There is no point in the ConditionalIgnore
                // (a literal cannot be clock gated)
                // Replace the operation with a mov
                op._opcode = Opcode::Mov;
                op._src.resize(1);
            }
            else if (1 == op._src.size())
            {
                didChangeIR = true;

                // All predicates have been optimized out
                // output will never be ignored
                // Replace the operation with a mov
                op._opcode = Opcode::Mov;
                op._src.resize(1);
            }
            else
            {
                // Check to see if any predicate has a literal value of 0 or 1
                bool anyPredicateZero = false;
                bool anyPredicateOne = false;

                for (size_t i = 1; i < op._src.size(); i++)
                {
                    const SourceOperand& sourceOperand = op._src[i];

                    if (SourceOperandType::Literal == sourceOperand.Type())
                    {
                        if (0 == sourceOperand.GetLiteral()._value)
                        {
                            anyPredicateZero = true;
                        }
                        else
                        {
                            assert(1 == sourceOperand.GetLiteral()._value);
                            anyPredicateOne = true;
                        }
                    }
                }

                if (anyPredicateZero)
                {
                    // A predicate is known to be zero (false)
                    // Therefore the output will always be predicated
                    // Replace the ConditionalIgnore with a move of a literal zero
                    didChangeIR = true;

                    op._opcode = Opcode::Mov;
                    op._src.resize(1);
                    op._src[0] = SourceOperand(0);
                }
                else if (anyPredicateOne)
                {
                    // If any predicate is known to be one, then it can be removed
                    didChangeIR = true;

                    std::vector<SourceOperand> newSourceOperands;

                    newSourceOperands.push_back(op._src[0]);

                    for (size_t i = 1; i < op._src.size(); i++)
                    {
                        const SourceOperand& sourceOperand = op._src[i];

                        if ((SourceOperandType::Literal == sourceOperand.Type()) &&
                            (1 == sourceOperand.GetLiteral()._value))
                        {
                            // don't include this source operand in the new list
                        }
                        else
                        {
                            newSourceOperands.push_back(sourceOperand);
                        }
                    }

                    op._src = newSourceOperands;
                }
            }
        };

        ForEachOperationForward(bb, constantValueCallback);
    }

    return didChangeIR;
}

// Finds memories that would consume less area if there were implemented as arrays instead of memories
// Converts them to arrays
// Runs during optimization
bool MemToArray(Program& program)
{
    struct MemoryInfo
    {
        size_t _readPortCount;
        size_t _writePortCount;
        size_t _baseGlobalIndex;
        bool _usesGlobalView;
        bool _convert;
    };

    const CodeGenConfig& config = GetCodeGenConfig();

    bool foundMemoryToConvert = false;

    // Maps index of memory to:
    // - Base index of the array of global registers
    // - Count of the number of read ports
    // - Count of the number of write ports
    std::map<size_t, MemoryInfo> memoriesToConvert;

    size_t numGlobalsAllocated = 0;

    const bool addClockGates = GetCodeGenConfig().PipelineClockGatingEnabled();

    // Find all memories accessed by all functions and collect the information we need to determine if we should convert
    // them
    for (Function& function : program._functions)
    {
        for (BasicBlock& bb : function._basicBlocks)
        {
            for (const Operation& op : bb._operations)
            {
                if (Opcode::LoadMemory == op._opcode)
                {
                    const size_t memoryIndex = op._src[0].GetAccessedRegister()._registerIndex;
                    assert(RegisterType::Memory == program._registerTable[memoryIndex]._type);

                    auto& memory = memoriesToConvert[memoryIndex];

                    memory._readPortCount++;

                    for (size_t i = 1; i < op._src.size(); i++)
                    {
                        const SourceOperand& srcOp = op._src[i];

                        if (SourceOperandType::Register == srcOp.Type())
                        {
                            const RegisterDescription& rd =
                                program._registerTable[srcOp.GetAccessedRegister()._registerIndex];

                            if (rd._type == RegisterType::GlobalView)
                            {
                                memory._usesGlobalView = true;
                            }
                        }
                    }
                }
                else if (Opcode::StoreMemory == op._opcode)
                {
                    const size_t memoryIndex = op._dst[0].GetAccessedRegister()._registerIndex;
                    assert(RegisterType::Memory == program._registerTable[memoryIndex]._type);

                    memoriesToConvert[memoryIndex]._writePortCount++;
                }
            };
        }
    }

    // Determine which memories are eligible for conversion
    for (auto& m : memoriesToConvert)
    {
        const size_t registerIndex = m.first;
        auto& memoryInfo = m.second;

        const RegisterDescription& rd = program._registerTable[registerIndex];

        {
            const auto& memoryDesc = rd.Memory();

            // This optimization doesn't preserve semantics of non-replicated or quad port memories
            if (!memoryDesc._replicate || memoryDesc._quadPort)
            {
                continue;
            }

            // Initial values for RAM are not supported (ROM ok) because, although we can set them on an
            // FPGA, they are only good during power-up and the RAMs are cleared to zeros during reset.
            if (!memoryDesc._initialValues.empty() && memoryInfo._writePortCount > 0)
            {
                continue;
            }

            // Memory has already been marked for conversion using a logic-based RAM module
            if (memoryDesc._useLogicRam)
            {
                continue;
            }

            // This optimization doesn't preserve semantics of multiple concurrent writes to different addresses
            if (memoryInfo._writePortCount > 1)
            {
                continue;
            }

            // This optimization doesn't work if address/predicate are a global view
            if (memoryInfo._usesGlobalView)
            {
                continue;
            }

            const double costAsArray =
                ArrayCostFunction(GetCodeGenDeviceConfig(), memoryDesc._elementWidth, memoryDesc._elementCount,
                                  memoryInfo._readPortCount, memoryInfo._writePortCount);

            const double costAsMemory =
                MemoryCostFunction(GetCodeGenDeviceConfig(), memoryDesc._elementWidth, memoryDesc._elementCount,
                                   memoryInfo._readPortCount, memoryInfo._writePortCount);

            // Convert the memory to array
            // - if requested at the command line (and the element count is not
            //   so large that it would make compilation painfully slow)
            // - or if the cost function indicates that less area would be
            //   consumed as an array
            // - If the RAM has not already been annotated as a "logic RAM"
            if (((config._forceMemToArray != 0) && (memoryDesc._elementCount <= config._forceMemToArray)) ||
                (costAsArray < costAsMemory))
            {
                // We will convert this memory into an array
                foundMemoryToConvert = true;
                memoryInfo._convert = true;

                // ROMs are converted into registers, which will later be converted into wires+mux during optimization.
                // RAMs will be converted into an instance of KanagawaHALLogicRAM.
                // So for ROMs, we need to reserve this additional register range, but not for RAMs
                if (memoryDesc.IsROM())
                {
                    // Reserve register range
                    memoryInfo._baseGlobalIndex = program._registerTable.size() + numGlobalsAllocated;
                    numGlobalsAllocated += memoryDesc._elementCount;
                }
            }
        }
    }

    if (!foundMemoryToConvert)
    {
        return false;
    }

    for (const auto& p : memoriesToConvert)
    {
        const size_t memoryIndex = p.first;
        const auto& memoryInfo = p.second;

        if (!memoryInfo._convert)
        {
            // If this memory wasn't tagged for conversion, skip it.
            continue;
        }

        // Copy RegisterDescription because AllocateRegister can
        // reference into the register table
        const RegisterDescription memory = program._registerTable[memoryIndex];

        assert(RegisterType::Memory == memory._type);
        const auto& memoryDesc = memory.Memory();

        if (!memoryDesc.IsROM())
        {
            // Set flag that will cause special Logic RAM module to be generated
            program._registerTable[memoryIndex].Memory()._useLogicRam = true;
            // No need to allocate global registers
            continue;
        }

        const bool hasInitialValues = !memoryDesc._initialValues.empty();
        const size_t numWritePorts = memoryInfo._writePortCount;

        for (size_t i = 0; i < memory.Memory()._elementCount; i++)
        {
            std::ostringstream nameStr;
            nameStr << memory._name << "_element_" << i;

            const size_t globalRegisterIndex =
                AllocateRegister(&program, memory.Memory()._elementWidth, RegisterType::Global, nameStr.str(),
                                 memory.Memory()._containerInstancePath);
            assert(globalRegisterIndex == (memoryInfo._baseGlobalIndex + i));

            RegisterDescription& global = program._registerTable[globalRegisterIndex];

            RegisterDescription::GlobalDesc gd = {};

            // For ROMs, set the initial value
            if (hasInitialValues)
            {
                assert(numWritePorts == 0);

                gd._hasInitialValue = true;
                gd._initialValue = memoryDesc._initialValues[i];
                gd._isConstant = true;
            }

            // gd._writeCount will be set later

            global.Global() = gd;
        }
    }

    // Change Opcode::StoreMemory and Opcode::LoadMemory for any operations in any functions referencing the converted
    // memories
    for (Function& f : program._functions)
    {
        for (BasicBlock& bb : f._basicBlocks)
        {
            OperationList::iterator nextIt = bb._operations.end();

            std::stack<AtomicBlockDesc> currAtomicBlockDesc;

            for (OperationList::iterator it = bb._operations.begin(); it != bb._operations.end(); it = nextIt)
            {
                Operation& op = *it;

                // Save the iterator for the next operation
                // to enable the current operation to be removed from the operation list
                nextIt = it;
                ++nextIt;

                if (Opcode::BeginAtomic == op._opcode)
                {
                    currAtomicBlockDesc.push(op._flags._atomicBlockDesc);
                }
                else if (Opcode::EndAtomic == op._opcode)
                {
                    currAtomicBlockDesc.pop();
                }
                else if (Opcode::LoadMemory == op._opcode)
                {
                    const AccessedRegister& src0 = op._src[0].GetAccessedRegister();

                    // Copy RegisterDescription because AllocateRegister can change
                    // references into the register table
                    const RegisterDescription memoryDesc = program._registerTable[src0._registerIndex];
                    assert(memoryDesc._type == RegisterType::Memory);

                    const auto memoriesToConvertIt = memoriesToConvert.find(src0._registerIndex);

                    if (memoriesToConvertIt == memoriesToConvert.end())
                    {
                        continue;
                    }

                    const auto& memoryInfo = memoriesToConvertIt->second;

                    if (!memoryInfo._convert)
                    {
                        continue;
                    }

                    OperationList ops;

                    OperationList::iterator insertIt = it;

                    {
                        SetOperationLocation sol(ops, op);

                        if (memoryDesc.Memory().IsROM())
                        {
                            const size_t baseGlobalRegIndex = memoryInfo._baseGlobalIndex;

                            const bool isSingleElement = 1 == memoryDesc.Memory()._elementCount;

                            const size_t indexWidth =
                                isSingleElement ? 1 : Log2RoundUp(memoryDesc.Memory()._elementCount);

                            // Move index into a register that is the correct width (wrapping)
                            const AccessedRegister wideLoadIndex = {
                                AllocateRegister(&program, indexWidth, RegisterType::Local, "MemToArrayLoadIndex")};

                            {
                                Operation movOp = {};

                                movOp._opcode = Opcode::Mov;
                                movOp._dst.push_back(wideLoadIndex);
                                movOp.PushOperand(op, 1);

                                ops.push_back(movOp);
                            }

                            // memory_norep is not supported with this optimization
                            assert(!memoryDesc.Memory().HasReadArbitration());
                            assert(2 == op._src.size() || 3 == op._src.size());

                            // This loop handles structures contained in memories
                            for (size_t dstIdx = 0; dstIdx < op._dst.size(); dstIdx++)
                            {
                                const size_t width = op._dst[dstIdx].Width(program);
                                const size_t offset = op._flags._loadMemory._sourceOffsets->at(dstIdx);

                                Operation muxOp = {};

                                muxOp._dst.push_back(op._dst[dstIdx]);

                                if (isSingleElement)
                                {
                                    // Only a single element to choose
                                    muxOp._opcode = Opcode::Mov;
                                }
                                else
                                {
                                    muxOp._opcode = Opcode::Select;
                                    muxOp._src.push_back(wideLoadIndex);
                                }

                                const size_t elementCountPow2 =
                                    isSingleElement ? 1 : (1ULL << memoryDesc.GetMemoryAddressWidth());

                                for (size_t i = 0; i < elementCountPow2; i++)
                                {
                                    if (i < memoryDesc.Memory()._elementCount)
                                    {
                                        // Gather to select subset of the source operand
                                        const AccessedRegister gatherSrcReg = {baseGlobalRegIndex + i};

                                        const AccessedRegister gatherDstReg = {
                                            AllocateRegister(&program, width, RegisterType::Local, "MemToArrayGather")};

                                        Operation gatherOp = {};
                                        gatherOp._opcode = Opcode::Gather;
                                        gatherOp._flags._gather._entries =
                                            g_compiler->Create<std::vector<GatherEntry>>();
                                        gatherOp._flags._gather._entries->push_back({offset, 0, width});
                                        gatherOp._src.push_back(gatherSrcReg);
                                        gatherOp._dst.push_back(gatherDstReg);
                                        ops.push_back(gatherOp);

                                        muxOp._src.push_back(gatherDstReg);
                                    }
                                    else
                                    {
                                        muxOp._src.push_back(0);
                                    }
                                }

                                ops.push_back(muxOp);
                            }
                        }
                        else
                        {
                            // RAM - just create a new LoadMemory operation
                            Operation loadMemOp = op;
                            loadMemOp._flags._loadMemory._readLatency =
                                0; // Logic-based RAM has zero read latency (just a mux)
                            loadMemOp._flags._loadMemory._bypass = false; // logic below will remove the need for bypass
                            ops.push_back(loadMemOp);
                        }

                        // Determine where to insert the new operations
                        // By default, insert them right before the LoadMemory operation
                        insertIt = it;

                        if (op._flags._loadMemory._bypass)
                        {
                            if (!currAtomicBlockDesc.empty())
                            {
                                if (currAtomicBlockDesc.top()._type == AtomicBlockType::MemoryLoad)
                                {
                                    // Move the global reads into the compute atomic block in the chain
                                    // To avoid adding forwarding hardware
                                    bool foundNextBlock = false;

                                    while (insertIt != bb._operations.end())
                                    {
                                        assert(!foundNextBlock);

                                        const Operation& currOp = *insertIt;

                                        ++insertIt;

                                        if (currOp._opcode == Opcode::BeginAtomic)
                                        {
                                            assert((currOp._flags._atomicBlockDesc._type ==
                                                    AtomicBlockType::AtomicMemCompute) ||
                                                   (currOp._flags._atomicBlockDesc._type ==
                                                    AtomicBlockType::MemoryLoadLatency));

                                            // ops will be inserted right after currOp
                                            if (currOp._flags._atomicBlockDesc._type ==
                                                AtomicBlockType::AtomicMemCompute)
                                            {
                                                foundNextBlock = true;
                                                break;
                                            }
                                        }
                                    }

                                    assert(foundNextBlock);
                                }
                                else
                                {
                                    // A load in a [[schedule(N)]] block where N is
                                    // long enough to just put the load
                                    // in the compute atomic
                                    assert(currAtomicBlockDesc.top()._type == AtomicBlockType::AtomicMemCompute);
                                }
                            }
                            else
                            {
                                // _bypass should only be set inside of a MemoryLoad atomic
                                assert(false);
                            }
                        }
                    }

                    //  Insert new operations
                    bb._operations.splice(insertIt, ops);

                    // Remove the LoadMemory
                    bb._operations.erase(it);
                }
            }

            assert(currAtomicBlockDesc.empty());
        }
    }

    return true;
}

// Finds bits of local variables which are known to be constant or unused
// Create dense versions of these variables, to reduce the number of pipeline register bits.
void SparseToDenseReg(Program& program)
{
    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

    // This optimization is controllable from the command line
    // as some synthesis tools will do this automatically
    if (!codeGenConfig._sparseRegOpt)
    {
        return;
    }

    for (Function& function : program._functions)
    {
        ControlFlowGraph cfg(function, program, OperationEnumerationMode::Scheduled,
                             ControlFlowGraphPhase::PostPipeline);

        RegisterToBitSet programUnusedBits;
        DetermineUnusedBitsForFunction(program, cfg, function, programUnusedBits);

        ConstantBitMap functionConstantBits;
        BitConstantPropagation(program, cfg, function, &functionConstantBits);

        // Treat unused bits as constant 0
        for (const auto& p : programUnusedBits)
        {
            std::map<size_t, bool>& bitToVal = functionConstantBits[p.first];

            for (const size_t bitIndex : p.second)
            {
                // If a bit is marked as constant and unused
                // Keep the constant value
                if (bitToVal.end() == bitToVal.find(bitIndex))
                {
                    SafeInsert(bitToVal, bitIndex, false);
                }
            }
        }

        for (BasicBlock& bb : function._basicBlocks)
        {
            // Sparse->Dense pack operations are placed in this stage
            Stage packStage;

            // Dense->sparse unpack operations are placed in this stage
            Stage unpackStage;

            // pack and unpack stages initialize to atomic sequence = 0
            assert(bb._stages.empty() || (bb._stages.front()._atomicSequence == 0));

            size_t previousAtomicSequence = 0;

            std::set<size_t> registerToDisallow;

            // First pass to detect hardened input of output registers
            // These will not be optimized
            {
                std::vector<size_t> readSet;
                std::vector<size_t> writeSet;

                ForEachOperationForward(
                    bb,
                    [&](Operation& op)
                    {
                        if (OpcodeUsesHardenedRegisters(op._opcode))
                        {
                            GetAccessedRegisters(op, readSet, writeSet);

                            for (const size_t r : readSet)
                            {
                                registerToDisallow.insert(r);
                            }

                            for (const size_t r : writeSet)
                            {
                                registerToDisallow.insert(r);
                            }
                        }
                    },
                    OperationEnumerationMode::Scheduled);
            }

            for (auto stageIt = bb._stages.begin(); stageIt != bb._stages.end(); ++stageIt)
            {
                Stage& stage = *stageIt;

                if (stage._atomicSequence > previousAtomicSequence)
                {
                    // Advancing to a new pipeline stage

                    // Insert pack operations at the end of the previous pipeline stage
                    if (!packStage._operations.empty())
                    {
                        packStage._atomicSequence = previousAtomicSequence;

                        bb._stages.insert(stageIt, packStage);

                        packStage = Stage();
                    }

                    if (!unpackStage._operations.empty())
                    {
                        // Insert unpack operations at the start of the current pipeline stage
                        unpackStage._atomicSequence = stage._atomicSequence;

                        bb._stages.insert(stageIt, unpackStage);

                        unpackStage = Stage();
                    }

                    previousAtomicSequence = stage._atomicSequence;
                }

                for (Operation& op : stage._operations)
                {
                    for (DestinationOperand& dstOp : op._dst)
                    {
                        if (DestinationOperandType::Register != dstOp.Type())
                        {
                            continue;
                        }

                        const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

                        const RegisterDescription rd = program._registerTable[registerIndex];

                        if (RegisterType::Pipeline != rd._type)
                        {
                            continue;
                        }

                        const auto it = functionConstantBits.find(registerIndex);
                        if (it == functionConstantBits.end())
                        {
                            continue;
                        }

                        const std::map<size_t, bool>& bitToVal = it->second;

                        if (bitToVal.empty())
                        {
                            continue;
                        }

                        if (Contains(registerToDisallow, registerIndex))
                        {
                            continue;
                        }

                        // Some bits being written are constant or will not affect top level outputs
                        assert(bitToVal.size() <= rd._width);

                        // If all bits are constant or all bits are unused
                        // then constant propagation or dead code elimination optimizations will have removed them.
                        // To simplify the code, don't optimize for the case where some bits are constant
                        // and others are unused (the sum of constant + unused bits = the register width)
                        if (bitToVal.size() == rd._width)
                        {
                            continue;
                        }

                        assert(bitToVal.size() < rd._width);

                        const size_t sparseWidth = rd._width;

                        const size_t denseWidth = sparseWidth - bitToVal.size();

                        // Allocate a new (narrower) pipeline register
                        const AccessedRegister densePipelineRegister = {
                            AllocateRegister(&program, denseWidth, RegisterType::Pipeline, rd._name + "_dense")};

                        {
                            // Change the producer operation to write to a new sparse wire
                            // Pack relevant bits from sparse output wire into the new dense pipeline register
                            const AccessedRegister sparseWire = {
                                AllocateRegister(&program, sparseWidth, RegisterType::Wire, rd._name)};

                            Operation gatherOp = {};

                            gatherOp._opcode = Opcode::Gather;

                            gatherOp._locations = op._locations;
                            gatherOp._expectNoSourceLocation = op._expectNoSourceLocation;

                            gatherOp._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();

                            gatherOp._dst.push_back(densePipelineRegister);

                            size_t offset = 0;

                            for (size_t i = 0; i < sparseWidth; i++)
                            {
                                if (bitToVal.end() == bitToVal.find(i))
                                {
                                    gatherOp._src.push_back(sparseWire);

                                    gatherOp._flags._gather._entries->push_back({i, offset, 1});

                                    offset++;
                                }
                            }

                            assert(offset == denseWidth);

                            // The gather will run in another substage
                            packStage._operations.push_back(gatherOp);

                            // Change the producer to write to a sparse wire
                            dstOp.GetAccessedRegister() = sparseWire;
                        }

                        {
                            // Change the pipeline register that `registerIndex` refers to into a sparse wire
                            // Consumers will read from this
                            program._registerTable[registerIndex]._type = RegisterType::Wire;

                            Operation gatherOp = {};

                            gatherOp._opcode = Opcode::Gather;

                            gatherOp._locations = op._locations;
                            gatherOp._expectNoSourceLocation = op._expectNoSourceLocation;

                            gatherOp._flags._gather._entries = g_compiler->Create<std::vector<GatherEntry>>();

                            gatherOp._dst.push_back(AccessedRegister{registerIndex});

                            size_t offset = 0;

                            for (size_t i = 0; i < sparseWidth; i++)
                            {
                                const auto it = bitToVal.find(i);
                                if (it != bitToVal.end())
                                {
                                    // this bit is constant or unused
                                    gatherOp._src.push_back(SourceOperand(it->second ? 1 : 0));

                                    gatherOp._flags._gather._entries->push_back({0, i, 1});
                                }
                                else
                                {
                                    // copy from the dense pipeline register
                                    gatherOp._src.push_back(densePipelineRegister);

                                    gatherOp._flags._gather._entries->push_back({offset, i, 1});

                                    offset++;
                                }
                            }

                            assert(offset == denseWidth);

                            unpackStage._operations.push_back(gatherOp);
                        }
                    }
                }
            }
        }
    }
}

// Removes FormatString/ReferenceString operations
// for strings which are never printed
bool RemoveUnusedStrings(const Program& program, Function& function)
{
    bool didChangeIR = false;

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        // Maps predicate operand to aggregated reference count adjustment
        using PredicateToCount = std::map<SourceOperand, mp_int>;

        std::map<size_t, PredicateToCount> regToPredicateToCount;

        const auto operationCallback = [&](Operation& op)
        {
            if ((Opcode::FormatString == op._opcode) || (Opcode::FormatEnum == op._opcode))
            {
                const SourceOperand& handleOp = op._src[0];
                const SourceOperand& predicateOp = op._src[1];

                if (SourceOperandType::Register == handleOp.Type())
                {
                    const size_t handleReg = handleOp.GetAccessedRegister()._registerIndex;

                    // The string handle should not be referenced before the Format{String,Enum} op
                    assert(regToPredicateToCount.end() == regToPredicateToCount.find(handleReg));

                    PredicateToCount& ptc = regToPredicateToCount[handleReg];

                    // Format{String,Enum} sets a reference count of 1 (if the predicate is true)
                    ptc[predicateOp] = 1;
                }

                // If any operand of the FormatString
                // other than the string handle being allocated
                // is a string handle, then that string handle
                // is considered to be live
                if (Opcode::FormatString == op._opcode)
                {
                    for (size_t i = 1; i < op._src.size(); i++)
                    {
                        const SourceOperand& srcOp = op._src[i];

                        if (SourceOperandType::Register == srcOp.Type())
                        {
                            regToPredicateToCount.erase(srcOp.GetAccessedRegister()._registerIndex);
                        }
                    }
                }
            }
            else if (Opcode::FormatEnum == op._opcode)
            {
                const SourceOperand& handleOp = op._src[0];
                const SourceOperand& predicateOp = op._src[1];

                if (SourceOperandType::Register == handleOp.Type())
                {
                    const size_t handleReg = handleOp.GetAccessedRegister()._registerIndex;

                    // The string handle should not be referenced before the FormatEnum op
                    assert(regToPredicateToCount.end() == regToPredicateToCount.find(handleReg));

                    PredicateToCount& ptc = regToPredicateToCount[handleReg];

                    // FormatEnum sets a reference count of 1 (if the predicate is true)
                    ptc[predicateOp] = 1;
                }
            }
            else if (Opcode::ReferenceString == op._opcode)
            {
                const SourceOperand& predicateOp = op._src[0];
                const SourceOperand& handleOp = op._src[1];
                const SourceOperand& amountOp = op._src[2];

                // All predicates must have the same width
                // otherwise 2 predicates with the same value but different widths
                // will be considered to be different
                assert(1 == predicateOp.Width(program));

                // Literal NULL handles are possible
                if (SourceOperandType::Register == handleOp.Type())
                {
                    assert(SourceOperandType::Literal == amountOp.Type());

                    const size_t handleReg = handleOp.GetAccessedRegister()._registerIndex;

                    const auto it = regToPredicateToCount.find(handleReg);
                    if (it != regToPredicateToCount.end())
                    {
                        PredicateToCount& ptc = it->second;

                        const mp_int amount =
                            Resize(amountOp.GetLiteral(), op.ShouldSignExtend(2), c_stringReferenceBits);

                        // Drop bits above c_stringReferenceBits
                        // to emulate c_stringReferenceBits-bit arithmetic
                        const mp_int mask = (mp_int(1) << c_stringReferenceBits) - 1;

                        ptc[predicateOp] = (ptc[predicateOp] + amount) & mask;
                    }
                }
            }
            else
            {
                // Check if a string handle is read
                // if so, treat it as live
                for (const SourceOperand& srcOp : op._src)
                {
                    if (SourceOperandType::Register == srcOp.Type())
                    {
                        regToPredicateToCount.erase(srcOp.GetAccessedRegister()._registerIndex);
                    }
                }
            }

            // Enqueue to another function is fine, because if a string handle is passed
            // it will appear as a source operand
            // Enqueue to another basic block in the same function will only appear at the end of a basic block
            // and there will not be string operations after that point
        };

        ForEachOperationForward(basicBlock, operationCallback);

        // The set of string handle registers which can be removed
        std::set<size_t> stringsToRemove;

        for (const auto& p1 : regToPredicateToCount)
        {
            // Check to see if the sum of reference count adjustments
            // within each predicate is zero
            bool allZero = true;

            for (const auto& p2 : p1.second)
            {
                if (p2.second != 0)
                {
                    allZero = false;
                }
            }

            if (allZero)
            {
                stringsToRemove.insert(p1.first);

                didChangeIR = true;
            }
        }

        const auto removeCallback = [&](const Operation& op)
        {
            bool shouldRemove = false;

            if ((Opcode::FormatString == op._opcode) || (Opcode::FormatEnum == op._opcode))
            {
                const SourceOperand& handleOp = op._src[0];

                if (SourceOperandType::Register == handleOp.Type())
                {
                    const size_t handleReg = handleOp.GetAccessedRegister()._registerIndex;

                    shouldRemove = stringsToRemove.end() != stringsToRemove.find(handleReg);
                }
            }
            else if (Opcode::ReferenceString == op._opcode)
            {
                const SourceOperand& handleOp = op._src[1];

                if (SourceOperandType::Register == handleOp.Type())
                {
                    const size_t handleReg = handleOp.GetAccessedRegister()._registerIndex;

                    shouldRemove = stringsToRemove.end() != stringsToRemove.find(handleReg);
                }
            }

            return shouldRemove;
        };

        RemoveOperations(basicBlock, removeCallback);
    }

    return didChangeIR;
}

// Finds 2 ReferenceString operations with matching predicates
// and string handles.  Combines them into 1 that does the combined reference.
// Removes ReferenceString operations which are will have no side effects
// Runs after scheduling
void RemoveStringOps(Program& program)
{
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            // Only optimize within a pipeline stage
            // to effectively avoid reordering reference operations
            // pats operations that store string handles in shared state
            // That reordering would prevent manual reference counting from being possible.
            for (Stage& stage : basicBlock._stages)
            {
                // Handle register and predicate operand (could be literal or register)
                using Key = std::pair<size_t, SourceOperand>;

                struct Value
                {
                    std::list<Operation*> _ops;
                    mp_int _referenceAmount;
                };

                std::map<Key, Value> regsToAmt;

                for (Operation& op : stage._operations)
                {
                    if (Opcode::ReferenceString == op._opcode)
                    {
                        const SourceOperand& predicateOp = op._src[0];
                        const SourceOperand& handleOp = op._src[1];
                        const SourceOperand& amountOp = op._src[2];

                        if (((SourceOperandType::Register == predicateOp.Type()) ||
                             (SourceOperandType::Literal == predicateOp.Type())) &&
                            (SourceOperandType::Register == handleOp.Type()) &&
                            (SourceOperandType::Literal == amountOp.Type()))
                        {
                            const size_t handleReg = handleOp.GetAccessedRegister()._registerIndex;

                            const mp_int amount =
                                Resize(amountOp.GetLiteral(), op.ShouldSignExtend(2), c_stringReferenceBits);

                            const Key key(handleReg, predicateOp);

                            Value& value = regsToAmt[key];

                            value._ops.push_back(&op);
                            value._referenceAmount += amount;
                        }
                    }
                };

                for (auto& p : regsToAmt)
                {
                    Value& v = p.second;

                    if (v._ops.size() > 1)
                    {
                        for (Operation* const op : v._ops)
                        {
                            SourceOperand& amountOp = op->_src[2];

                            assert(SourceOperandType::Literal == amountOp.Type());

                            const Literal l = {0, c_stringReferenceBits};

                            amountOp = SourceOperand(l);
                        }

                        {
                            Operation* lastOp = v._ops.back();

                            SourceOperand& amountOp = lastOp->_src[2];

                            assert(SourceOperandType::Literal == amountOp.Type());

                            const Literal l = {Truncate(v._referenceAmount, c_stringReferenceBits),
                                               c_stringReferenceBits};

                            amountOp = SourceOperand(l);

                            lastOp->_signExtendSourceMask |= (1ull << 2);
                        }
                    }
                }

                // Remove ReferenceString operations that are NOPs
                // (either a null handle, or a 0 reference amount)
                const auto predicate = [&](const Operation& op)
                {
                    bool isLive = true;

                    if (Opcode::ReferenceString == op._opcode)
                    {
                        assert(0 == op._dst.size());
                        assert(3 == op._src.size());

                        const SourceOperand& handleOp = op._src[1];
                        const SourceOperand& amountOp = op._src[2];

                        if ((SourceOperandType::Literal == handleOp.Type()) && (0 == handleOp.GetLiteral()._value))
                        {
                            isLive = false;
                        }
                        else if ((SourceOperandType::Literal == amountOp.Type()) && (0 == amountOp.GetLiteral()._value))
                        {
                            isLive = false;
                        }
                    }

                    return !isLive;
                };

                stage._operations.remove_if(predicate);
            }
        }
    }
}

// Used to verify that optimization passes do not produce invalid IR
bool ValidateIR(const Program& program, Function& function)
{
    ValidateLocalsAreWritten(program, function);

    return false; // No IR changes made
}

// Register index and literal value
using SelectIndexEnvironment = std::pair<size_t, Literal>;

using SelectIndexMemoizationKey = std::pair<const Operation*, size_t>;

using SelectIndexMemoizationTable = std::map<SelectIndexMemoizationKey, std::optional<SourceOperand>>;

std::optional<SourceOperand> GetSelectIndexOptimizedResult(const Program& program, const Operation& op,
                                                           const size_t outputWidth, const SelectIndexEnvironment& env,
                                                           const LocalDefUseTracker& defUseTracker,
                                                           const size_t maxRecursion,
                                                           SelectIndexMemoizationTable& memoizationTable)
{
    assert(1 == op._dst.size());
    assert(DestinationOperandType::Register == op._dst[0].Type());

    // To minimize compile time, the search depth is limited
    if (maxRecursion == 0)
    {
        return {};
    }

    const SelectIndexMemoizationKey key(&op, outputWidth);

    const auto it = memoizationTable.find(key);
    if (it != memoizationTable.end())
    {
        return it->second;
    }

    const auto getSpecializedSourceOperand = [&](const size_t srcOperandIndex)
    {
        assert(srcOperandIndex < op._src.size());

        std::optional<SourceOperand> result;

        const SourceOperand& srcOperand = op._src[srcOperandIndex];

        const size_t srcOperandWidth = srcOperand.Width(program);

        if (SourceOperandType::Literal == srcOperand.Type())
        {
            result = srcOperand;
        }
        else if (SourceOperandType::Register == srcOperand.Type())
        {
            const size_t registerIndex = srcOperand.GetAccessedRegister()._registerIndex;

            if (env.first == registerIndex)
            {
                const Literal& literalIndexValue = env.second;

                const Literal resized = {
                    Resize(literalIndexValue, op.ShouldSignExtend(srcOperandIndex), srcOperandWidth), srcOperandWidth};

                result = SourceOperand(resized);
            }
            else
            {
                const Operation* const definingOp = defUseTracker.GetDef(srcOperand);

                if (definingOp)
                {
                    assert(maxRecursion > 0);

                    result = GetSelectIndexOptimizedResult(program, *definingOp, srcOperandWidth, env, defUseTracker,
                                                           maxRecursion - 1, memoizationTable);
                }
            }
        }

        assert(!result || result->Width(program) == srcOperandWidth);

        return result;
    };

    const auto getLiteralSourceOperand = [&](const size_t srcOperandIndex)
    {
        const std::optional<SourceOperand> specialized = getSpecializedSourceOperand(srcOperandIndex);

        std::optional<Literal> result;

        if (specialized && (SourceOperandType::Literal == specialized->Type()))
        {
            result = specialized->GetLiteral();
        }

        assert(!result || (result->_width == op._src[srcOperandIndex].Width(program)));

        return result;
    };

    std::optional<SourceOperand> result;

    switch (op._opcode)
    {
    case Opcode::Mov:
    {
        assert(1 == op._src.size());

        const std::optional<Literal> op0 = getLiteralSourceOperand(0);

        if (op0)
        {
            const mp_int literalResult = Resize(*op0, op.ShouldSignExtend(0), outputWidth);

            result = SourceOperand(Literal{literalResult, outputWidth});
        }
    }
    break;

    case Opcode::UnaryOp:
    {
        assert(1 == op._src.size());

        const std::optional<Literal> op0 = getLiteralSourceOperand(0);

        if (op0)
        {
            const mp_int literalResult =
                ImplementUnaryOp(*op0, op.ShouldSignExtend(0), op._flags._unaryOpType, outputWidth);

            result = SourceOperand(Literal{literalResult, outputWidth});
        }
    }
    break;

    case Opcode::BinaryOp:
    {
        assert(2 == op._src.size());

        const std::optional<Literal> op0 = getLiteralSourceOperand(0);
        const std::optional<Literal> op1 = getLiteralSourceOperand(1);

        if (op0 && op1)
        {
            const mp_int literalResult = ImplementBinaryOp(*op0, *op1, outputWidth, op.ShouldSignExtend(0),
                                                           op.ShouldSignExtend(1), op._flags._binaryOpType);

            result = SourceOperand(Literal{literalResult, outputWidth});
        }
    }
    break;

    case Opcode::Select:
    {
        const size_t numChoices = op._src.size();

        const std::optional<Literal> op0 = getLiteralSourceOperand(0);

        if (op0)
        {
            // Index value is known
            // Just return the selected operand
            const size_t literalIndexValue = MpToSizeT(op0->_value);

            assert(literalIndexValue < numChoices);

            const size_t selectedOperandIndex = literalIndexValue + 1;

            // Get a specialized version of the source operand if possible
            const std::optional<SourceOperand> specializedOperand = getSpecializedSourceOperand(selectedOperandIndex);

            const SourceOperand newSourceOperand =
                specializedOperand ? *specializedOperand : op._src[selectedOperandIndex];

            assert(newSourceOperand.Width(program) == op._src[selectedOperandIndex].Width(program));

            if (SourceOperandType::Literal == newSourceOperand.Type())
            {
                const mp_int literalResult =
                    Resize(newSourceOperand.GetLiteral(), op.ShouldSignExtend(selectedOperandIndex), outputWidth);

                result = SourceOperand(Literal{literalResult, outputWidth});
            }
            else if (SourceOperandType::Register == newSourceOperand.Type())
            {
                // Sign/Zero extension to the output width is not supported
                if (newSourceOperand.Width(program) == outputWidth)
                {
                    result = newSourceOperand;
                }
            }
        }
    }
    break;
    }

    assert(!result || (result->Width(program) == outputWidth));

    SafeInsert(memoizationTable, key, result);

    return result;
}

// Input:
// a = b + 1
// c = b + 2
// d = select(b, a, c)
//
// Output:
// a = b + 1
// c = b + 2
// d = select(b, 1, 2)
bool SelectIndexPropagation(Program& program, Function& function)
{
    bool didChangeIR = false;

    const size_t maxRecursion = GetCodeGenConfig().GetMaxSelectIndexOptRecursion();

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        LocalDefUseTracker defUseTracker(program, basicBlock);

        for (Operation& op : basicBlock._operations)
        {
            // Update def-use tracker
            // Only operations which produce a single register are needed
            defUseTracker.HandleOperation(op, (op._dst.size() == 1) &&
                                                  (DestinationOperandType::Register == op._dst[0].Type()));

            if (Opcode::Select == op._opcode)
            {
                const SourceOperand& indexOperand = op._src[0];

                if (SourceOperandType::Register == indexOperand.Type())
                {
                    const size_t indexRegister = indexOperand.GetAccessedRegister()._registerIndex;

                    for (size_t i = 1; i < op._src.size(); i++)
                    {
                        const Literal selectIndexValue = {i - 1, indexOperand.Width(program)};

                        SourceOperand& originalOperand = op._src[i];

                        // Reduce compile time by skipping this optimization
                        // If the source operand value is not affected by the value of indexRegister
                        if (SourceOperandType::Register == originalOperand.Type())
                        {
                            const Operation* const definingOp = defUseTracker.GetDef(originalOperand);

                            if (definingOp)
                            {
                                const SelectIndexEnvironment env(indexRegister, selectIndexValue);

                                SelectIndexMemoizationTable memoizationTable;

                                const std::optional<SourceOperand> newSrcOp =
                                    GetSelectIndexOptimizedResult(program, *definingOp, originalOperand.Width(program),
                                                                  env, defUseTracker, maxRecursion, memoizationTable);

                                if (newSrcOp)
                                {
                                    assert(newSrcOp->Width(program) == originalOperand.Width(program));

                                    if (originalOperand != *newSrcOp)
                                    {
                                        originalOperand = *newSrcOp;
                                        didChangeIR = true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return didChangeIR;
}

// Called before scheduling
void Optimize(IRContext& context)
{
    Program& program = *context._program;

    struct OptimizationFunction
    {
        std::function<bool()> _function;
        const char* _name;
        bool _shouldRun;
    };

    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

    const size_t phaseCount = static_cast<size_t>(OptimizationPhase::Count);

    const OptimizationFunction globalOptimizations[] = {
        {[&]() { return SubstituteLiterals(program); }, "SubstituteLiterals", true},
        {[&]() { return MemToArray(program); }, "MemToArray",
         codeGenConfig._optimize > 0 && g_compiler->IsCompilingToVerilog()},
        {[&]() { return RemoveUnusedGlobals(program); }, "RemoveUnusedGlobals", codeGenConfig._optimize > 0},
    };

    for (size_t phaseIndex = 0; phaseIndex < phaseCount; phaseIndex++)
    {
        bool makingForwardProgress;

        do
        {
            makingForwardProgress = false;

            for (const auto& optimization : globalOptimizations)
            {
                if (optimization._shouldRun)
                {
                    for (Function& function : program._functions)
                    {
                        for (const BasicBlock& bb : function._basicBlocks)
                        {
                            AssertUnscheduledOperationsHaveLocations(program, bb);
                        }
                    }

                    {
                        CompileTimer ct(*g_compiler, optimization._name);

                        // If any optimization reports that it changed the IR, then continue optimizing.
                        // If one optimization reports that it changed the IR, then continue with other
                        // optimizations. (Don't short-circuit makingForwardProgress.) This ensures
                        // dead codes removed early.
                        makingForwardProgress = optimization._function() || makingForwardProgress;
                    }

                    for (Function& function : program._functions)
                    {
                        for (const BasicBlock& bb : function._basicBlocks)
                        {
                            AssertUnscheduledOperationsHaveLocations(program, bb);
                        }
                    }

                    if (codeGenConfig._dumpOpt)
                    {
                        std::cout << "\n=====\nAfter " << optimization._name << "\n=====\n";
                        SerializeProgram(std::cout, program);
                    }
                }
            }

            for (Function& function : program._functions)
            {
                bool makingForwardProgressThisFunction = false;

                // Run all optimizations on this function
                // until no optimization makes progress
                do
                {
                    makingForwardProgressThisFunction = false;

                    for (const BasicBlock& bb : function._basicBlocks)
                    {
                        AssertUnscheduledOperationsHaveLocations(program, bb);
                    }

                    ControlFlowGraph controlFlowGraph(function, program, OperationEnumerationMode::Unscheduled,
                                                      ControlFlowGraphPhase::PrePipeline);

                    // OptimizationPhase::LutCommonSub exists because PackLuts and CommonSubexpressionElimination
                    // can potentially never reach a fixed point.
                    //
                    // PackLuts input:
                    // A = LUT1
                    // B = MOV(A)
                    //
                    // PackLuts output:
                    // A = LUT1 (not dead code, referenced in another basic block)
                    // B = LUT1
                    //
                    // CommonSubexpressionElimination output:
                    // A = LUT1
                    // B = MOV(A)
                    //
                    // In this phase, common sub expression elimination runs on LUT operations, but PackLuts is skipped

                    const bool allowLutCSE = phaseIndex == static_cast<size_t>(OptimizationPhase::LutCommonSub);

                    // Keep the formatting of this table spreedsheet-like, for easier reference.
                    // clang-format off
                    const OptimizationFunction functionOptimizations[] = {

                        {[&]() { return KillMoves(program, controlFlowGraph, function); }, "KillMoves", true},
                        {[&]() { return ConstantPropagation(program, controlFlowGraph, function, OperationEnumerationMode::Unscheduled); }, "ConstantPropagation", codeGenConfig._optimize > 0},
                        {[&]() { return SelectIndexPropagation(program, function); }, "SelectIndexPropagation", codeGenConfig._optimize > 0},

                        {[&]() { return RemoveDeadJumps(function); }, "RemoveDeadJumps", codeGenConfig._optimize > 0},
                        {[&]() { return RemoveAsserts(function); }, "RemoveAsserts", !codeGenConfig._releaseAssert},
                        {[&]() { return GatherToMove(program, function); }, "GatherToMove", codeGenConfig._optimize > 0},
                        {[&]() { return AddToOr(program, function); }, "AddToOr", codeGenConfig._optimize > 0},
                        {[&]() { return RemoveUnusedStrings(program, function); }, "RemoveUnusedStrings", codeGenConfig._optimize > 0},

                        {[&]() { return FixupLutOps(program, function); }, "FixupLutOps", (codeGenConfig._optimize > 0) && allowLutCSE}, // CommonSubexpressionElimination expects LUTs in canonical form
                        {[&]() { return CommonSubexpressionElimination(program, function, allowLutCSE); }, "CommonSubexpressionElimination", codeGenConfig._optimize > 0},

                        {[&]() { return ConstSelectBits(program, function); }, "ConstSelectBits", codeGenConfig._optimize > 0},
                        {[&]() { return NarrowLiterals(program, function); }, "NarrowLiterals", codeGenConfig._optimize > 0},
                        {[&]() { return PredicateIdentities(program, function); }, "PredicateIdentities", codeGenConfig._optimize > 0},
                        {[&]() { return PredicateFalsities(program, function); }, "PredicateFalsities", codeGenConfig._optimize > 0},

                        {[&]() { return ConditionalIgnoreConstantProp(program, function); }, "ConditionalIgnoreConstantProp", codeGenConfig._optimize > 0},

                        {[&]() { return ParallelConditionalIgnore(program, function); }, "ParallelConditionalIgnore", codeGenConfig._optimize > 0},

                        // Must come after ConditionalIgnoreConstantProp (assumes no literals in ConditionalIgnore
                        // operations)
                        {[&]() { return ComputeClockGating(program, function, OperationEnumerationMode::Unscheduled, nullptr); }, "ComputeClockGating", codeGenConfig._optimize > 0},

                        // Do not run if slow-optimizations are disabled.  Do not generate luts in the initial phase
                        // Also, do not decompose wide selects before constant propagation has converged
                        {[&]() { return DecomposeWideOps(program, function); }, "DecomposeWideOps", (codeGenConfig._optimize > 1) && (phaseIndex >= static_cast<size_t>(OptimizationPhase::PackLuts1))},

                        {[&]() { return AlgebraicIdentities(program, function); }, "AlgebraicIdentities", codeGenConfig._optimize > 0},

                        {[&]() { return FixupLutOps(program, function); }, "FixupLutOps", codeGenConfig._optimize > 0}, // Must come before PackLuts, so that PackLuts only ever sees clean inputs

                        // This optimization can slow down CL.exe significantly, only run for -O2
                        {[&]() { return PackLuts(program, function, phaseIndex); }, "PackLuts", codeGenConfig._optimize > 1 && !allowLutCSE}, // must come before KillMoves, to help catch bugs
                                                                                                                                              // when KillMoves operates on Opcode::Lut

                        // Generates Opcode::Lut operations, so only runs in later phases
                        {[&]() { return BitConstantPropagation(program, controlFlowGraph, function, nullptr); }, "BitConstantPropagation", (codeGenConfig._optimize > 1) && (phaseIndex >= static_cast<size_t>(OptimizationPhase::PackLuts1))},

                        // Validate IR is valid
                        // Runs before RemoveDeadOperations to catch cases where invalid IR is produced
                        // but then accidentally fixed up by RemoveDeadOperations
                        {[&]() { return ValidateIR(program, function); }, "ValidateIR", true},

                        // Remove dead code is always run so that operations on RegisterType::BitBucket types are
                        // eliminated from codegen output Dead codegen is not written to DebugSymbols
                        {[&]() { return RemoveDeadOperations(program, controlFlowGraph, function, OperationEnumerationMode::Unscheduled); }, "RemoveDeadOperations", true}, // Must come near the end, to ensure operation count changes
                    };
                    // clang-format on

                    for (const auto& optimization : functionOptimizations)
                    {
                        if (optimization._shouldRun)
                        {
                            {
                                CompileTimer ct(*g_compiler, optimization._name);

                                // If any optimization reports that it changed the IR, then continue optimizing.
                                // If one optimization reports that it changed the IR, then continue with other
                                // optimizations. (Don't short-circuit makingForwardProgressThisFunction) .
                                // This ensures dead code is removed early.
                                makingForwardProgressThisFunction =
                                    optimization._function() || makingForwardProgressThisFunction;

                                // If any optimization returns true, then the
                                // entire program will be optimized again
                                makingForwardProgress = makingForwardProgress || makingForwardProgressThisFunction;
                            }

                            if (codeGenConfig._dumpOpt)
                            {
                                std::cout << "\n=====\nAfter " << optimization._name << "\n=====\n";
                                SerializeFunction(std::cout, function, program);
                            }

                            for (const BasicBlock& bb : function._basicBlocks)
                            {
                                AssertUnscheduledOperationsHaveLocations(program, bb);
                            }
                        }
                    }
                } while (makingForwardProgressThisFunction);
            }
        } while (makingForwardProgress);
    }
}

// Simple optimizations
// which reduce the amount of work that subsequent serialize optimizations and lowerings perform
// The complexity of this optimization is low enough that it is reasonable to optimization
// functions in parallel
void EarlyOptimization(Program& program)
{
    if (GetCodeGenConfig()._dumpOpt)
    {
        std::cout << "\n=====\nBefore Early Optimization\n=====\n";
        SerializeProgram(std::cout, program);
    }

    // Each function is optimized concurrently with every other function
    ThreadPool threadPool;

    for (auto it = program._functions.begin(); it != program._functions.end(); ++it)
    {
        // Iterator is captured by value, program is captured by reference
        // Iterator is dereferenced inside the lambda, to avoid copying the Function
        threadPool.Async(
            [it, &program]()
            {
                Function& function = *it;

                for (BasicBlock& basicBlock : function._basicBlocks)
                {
                    //
                    // Copy propagation
                    //

                    // Used to avoid optimizing if any register in involved in an operation
                    // does not have a compatible type
                    // For instance, this prevents optimizations acting on Opcode::MovLatencyPlaceholder
                    const auto checkRegs = [&](const Operation& op)
                    {
                        for (const SourceOperand& srcOp : op._src)
                        {
                            if (SourceOperandType::Register == srcOp.Type())
                            {
                                if (!IsRegisterTypeAllowedInKillMoves(
                                        program._registerTable[srcOp.GetAccessedRegister()._registerIndex]._type))
                                {
                                    return false;
                                }
                            }
                        }

                        for (const DestinationOperand& dstOp : op._dst)
                        {
                            if (DestinationOperandType::Register == dstOp.Type())
                            {
                                if (!IsRegisterTypeAllowedInKillMoves(
                                        program._registerTable[dstOp.GetAccessedRegister()._registerIndex]._type))
                                {
                                    return false;
                                }
                            }
                        }

                        return true;
                    };

                    // Forward pass through operations
                    // looking for redundant Opcode::Mov
                    using OperandAndSequenceNumber = std::pair<SourceOperand, size_t>;

                    std::map<size_t, size_t> regIndexToSequenceNumber;

                    std::map<size_t, OperandAndSequenceNumber> regIndexToEquivalentOperand;

                    const auto callback = [&](Operation& op)
                    {
                        // Update sources according to regToMove
                        // Don't optimize Enqueue operations - values should pass through them in registers to ensure
                        // that fifo writes occur correctly
                        if (checkRegs(op) && !ShouldPreserveInputRegisters(op._opcode))
                        {
                            for (size_t sourceOperandIndex = 0; sourceOperandIndex < op._src.size();
                                 sourceOperandIndex++)
                            {
                                SourceOperand& srcOp = op._src[sourceOperandIndex];

                                if (SourceOperandType::Register == srcOp.Type())
                                {
                                    const auto it =
                                        regIndexToEquivalentOperand.find(srcOp.GetAccessedRegister()._registerIndex);
                                    if (it != regIndexToEquivalentOperand.end())
                                    {
                                        // The source operand was produced by a mov operation
                                        // replace the source operand with the source of that mov
                                        // No need to update the sign extension mask as the mov did not change sizes
                                        const SourceOperand& replacementOp = it->second.first;

                                        if (replacementOp.Type() == SourceOperandType::Literal)
                                        {
                                            if (AllowLiteralSourceOperand(op, sourceOperandIndex))
                                            {
                                                // Replace srcOp with a literal
                                                srcOp = replacementOp;
                                            }
                                        }
                                        else if (replacementOp.Type() == SourceOperandType::Register)
                                        {
                                            // Check if a write has occurred to
                                            // replacementOp.GetAccessedRegister()._registerIndex after a reference to
                                            // it was placed into regIndexToEquivalentOperand
                                            if (it->second.second ==
                                                regIndexToSequenceNumber[replacementOp.GetAccessedRegister()
                                                                             ._registerIndex])
                                            {
                                                srcOp = replacementOp;
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // This runs before renaming (and helps renaming take less time, but reducing the number of
                        // operations in the IR)
                        for (const DestinationOperand& dstOp : op._dst)
                        {
                            if (DestinationOperandType::Register == dstOp.Type())
                            {
                                const size_t dstReg = dstOp.GetAccessedRegister()._registerIndex;

                                // dstReg is no longer equivalent to another operand
                                regIndexToEquivalentOperand.erase(dstReg);

                                // if any register "r" was previously equivalent to dstReg
                                // it no longer is
                                regIndexToSequenceNumber[dstReg]++;
                            }
                        }

                        if ((Opcode::Mov == op._opcode) && (DestinationOperandType::Register == op._dst[0].Type()) &&
                            checkRegs(op))
                        {
                            // Record that the destination is equivalent to the source
                            const size_t dstReg = op._dst[0].GetAccessedRegister()._registerIndex;

                            const size_t dstRegWidth = op._dst[0].Width(program);

                            if (op._src[0].Type() == SourceOperandType::Literal)
                            {
                                // Adjust the size of the literal to match the destination register
                                const Literal adjustedLiteral = {
                                    Resize(op._src[0].GetLiteral(), op.ShouldSignExtend(0), dstRegWidth), dstRegWidth};

                                regIndexToEquivalentOperand[dstReg] =
                                    OperandAndSequenceNumber(SourceOperand(adjustedLiteral), 0);
                            }
                            else if (op._src[0].Type() == SourceOperandType::Register)
                            {
                                if (dstRegWidth == op._src[0].Width(program))
                                {
                                    regIndexToEquivalentOperand[dstReg] = OperandAndSequenceNumber(
                                        op._src[0],
                                        regIndexToSequenceNumber[op._src[0].GetAccessedRegister()._registerIndex]);
                                }
                            }
                        }
                        else if ((Opcode::Clear == op._opcode) && checkRegs(op))
                        {
                            // Treat Opcode::Clear like Opcode::Mov with a literal 0 source operand
                            assert(1 == op._dst.size());

                            if (DestinationOperandType::Register == op._dst[0].Type())
                            {
                                const size_t dstReg = op._dst[0].GetAccessedRegister()._registerIndex;

                                const Literal zero = {0, op._dst[0].Width(program)};

                                regIndexToEquivalentOperand[dstReg] = OperandAndSequenceNumber(zero, 0);
                            }
                        }
                    };

                    ForEachOperationForward(basicBlock, callback, OperationEnumerationMode::Unscheduled);
                }

                //
                // Dead code elimination
                // Eliminate definitions of registers which have no uses in the function
                // Removes operations that were made useless by copy propagation
                //

                size_t numOperationsRemoved = 0;

                do
                {
                    numOperationsRemoved = 0;

                    // Determine which registers are used
                    RegisterSet usedRegisters;

                    for (BasicBlock& basicBlock : function._basicBlocks)
                    {
                        const auto callback = [&](Operation& op)
                        {
                            for (const SourceOperand& srcOp : op._src)
                            {
                                if (SourceOperandType::Register == srcOp.Type())
                                {
                                    usedRegisters.insert(srcOp.GetAccessedRegister()._registerIndex);
                                }
                            }

                            if (op._getSuccessorBlock)
                            {
                                // Include registers which are live-in to other basic blocks (including in other
                                // functions)
                                BasicBlock* const successor = op._getSuccessorBlock();

                                for (const size_t registerIndex : successor->_liveInReg)
                                {
                                    usedRegisters.insert(registerIndex);
                                }
                            }
                        };

                        ForEachOperationForward(basicBlock, callback, OperationEnumerationMode::Unscheduled);
                    }

                    for (BasicBlock& basicBlock : function._basicBlocks)
                    {
                        const auto predicate = [&](const Operation& op)
                        {
                            bool isLive = false;

                            // Some opcodes should always be considered live
                            if (IsLiveOperation(op))
                            {
                                isLive = true;
                            }

                            // If any destination is used, then consider the operation to be live
                            for (const DestinationOperand& dstOp : op._dst)
                            {
                                if (DestinationOperandType::Register == dstOp.Type())
                                {
                                    if (usedRegisters.end() !=
                                        usedRegisters.find(dstOp.GetAccessedRegister()._registerIndex))
                                    {
                                        isLive = true;
                                    }
                                }
                            }

                            if (!isLive)
                            {
                                numOperationsRemoved++;
                            }

                            return !isLive;
                        };

                        basicBlock._startConditionOperations.remove_if(predicate);

                        basicBlock._operations.remove_if(predicate);

                        // Note that the appended stages are not modified here
                        // Those are optimized away with RemoveDeadJumps
                    }
                } while (numOperationsRemoved > 0);
            });
    }

    threadPool.WaitForIdle();

    if (GetCodeGenConfig()._dumpOpt)
    {
        std::cout << "\n=====\nAfter Early Optimization\n=====\n";
        SerializeProgram(std::cout, program);
    }
}
