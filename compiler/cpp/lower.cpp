// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

// Iterates over all operations in operation list
// Find each compare operation and create a new register with width of "desiredWidth" on
// lhs and rhs before performing that op if "desiredWidth" is bigger than lhs/rhs width
// The purpose of this function is to ensure lhs and rhs have the same data width on
// the compare operations(EQ/NE/GT/GE/LT/LE) to avoid the Lint warning of STARC-2.10.3.1
void UpdateCompareConditionList(Program& program, OperationList& operations)
{
    for (OperationList::iterator it = operations.begin(); it != operations.end(); ++it)
    {
        Operation& op = *it;

        if (op._opcode == Opcode::BinaryOp &&
            (op._flags._binaryOpType == ParseTreeBinaryOpTypeEQ || op._flags._binaryOpType == ParseTreeBinaryOpTypeNE ||
             op._flags._binaryOpType == ParseTreeBinaryOpTypeGT || op._flags._binaryOpType == ParseTreeBinaryOpTypeGE ||
             op._flags._binaryOpType == ParseTreeBinaryOpTypeLT || op._flags._binaryOpType == ParseTreeBinaryOpTypeLE))
        {
            assert(2 == op._src.size());
            assert(1 == op._dst.size());

            const SourceOperand& lhsOperand = op._src[0];
            const SourceOperand& rhsOperand = op._src[1];
            // Promote unsigned to signed when only one operand is signed
            const bool lhsSigned = op.ShouldSignExtend(0);
            const bool rhsSigned = op.ShouldSignExtend(1);
            const bool promoteToSigned = lhsSigned ^ rhsSigned;
            // If an operand is promoted and it's not the maximum width, increment width by 1
            const size_t lhsWidth = op._src[0].Width(program);
            const size_t rhsWidth = op._src[1].Width(program);
            const size_t maxOpWidth = std::max<size_t>(lhsWidth, rhsWidth);
            // The new width is identical bewteen lhs and rhs
            size_t desiredCmpWidth;
            if (!promoteToSigned)
            {
                desiredCmpWidth = maxOpWidth;
            }
            else if (!lhsSigned && (lhsWidth == maxOpWidth) || !rhsSigned && (rhsWidth == maxOpWidth))
            {
                // if lhs is unsigned and its width is bigger,lhsWidth+1 will be the comprasion width;same for rhs
                desiredCmpWidth = maxOpWidth + 1;
            }
            else
            {
                desiredCmpWidth = maxOpWidth;
            }

            OperationList ops;
            {
                SetOperationLocation sol(ops, op);

                for (size_t opIdx = 0; opIdx < 2; opIdx++)
                {
                    // perform new op to update register only desired*hsWidth != *hsWidth
                    if (desiredCmpWidth != op._src[opIdx].Width(program))
                    {
                        // Get the operand name for *hs
                        std::string operandName;
                        if (SourceOperandType::Register == op._src[opIdx].Type())
                        {
                            operandName =
                                program._registerTable[op._src[opIdx].GetAccessedRegister()._registerIndex]._name;
                        }
                        else
                        {
                            std::ostringstream str;

                            str << "cmp_literal_";
                            str << op._src[opIdx].GetLiteral()._value;

                            operandName = str.str();
                        }

                        // Create a new register to store the desiredLhsWidth
                        const AccessedRegister desiredValue = {
                            AllocateRegister(&program, desiredCmpWidth, RegisterType::Local, operandName)};

                        // Move from the original lhs operand to the new lhs operand
                        {
                            Operation mov = {};

                            mov._opcode = Opcode::Mov;
                            mov._dst.push_back(desiredValue);
                            mov.PushOperand(op, opIdx);

                            ops.push_back(mov);
                        }
                        // Update the cmp operator's source operands to the new ones
                        op._src[opIdx] = SourceOperand(desiredValue);
                    }
                }
            }
            // Put the added operations into IR
            operations.splice(it, ops);
            // Update the sign-bit mask to indicate both LHS and RHS are treated as signed.
            if (op._signExtendSourceMask != 0)
            {
                op._signExtendSourceMask = 3;
            }
        }
    }
}

void UpdateCmpOpWidth(Program& program)
{
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            UpdateCompareConditionList(program, basicBlock._operations);
            UpdateCompareConditionList(program, basicBlock._startConditionOperations);
        }
    }

    for (auto& p : program._globalViewFunctions)
    {
        UpdateCompareConditionList(program, p.second);
    }
}

// Iterates over all operations in an operation list
// Calls a callback for each one
// Metadata is passed to each callback about if the operation is contained in an atomic block
void ForEachOperation_Atomic(OperationList& operations, ForEachOperation_AtomicCallback callback)
{
    std::stack<ForEachOperation_AtomicData> dataStack;

    // Map atomic chain index to BeginAtomic operation at the start of the chain
    std::map<size_t, OperationList::iterator> atomicChainMap;

    for (OperationList::iterator it = operations.begin(); it != operations.end(); ++it)
    {
        Operation& op = *it;

        ForEachOperation_AtomicData data = {};
        if (!dataStack.empty())
        {
            data = dataStack.top();
        }

        if (Opcode::BeginAtomic == op._opcode)
        {
            data._inAtomic = true;
            data._currAtomicBlockDesc = op._flags._atomicBlockDesc;

            const size_t chainIndex = op._flags._atomicBlockDesc._chainIndex;

            // If chain has not been seen before, mark this as start of chain
            const auto itChain = atomicChainMap.find(chainIndex);
            if (itChain == atomicChainMap.end())
            {
                SafeInsert(atomicChainMap, chainIndex, it);
                data._currAtomicChainStart = it;
            }
            else
            {
                data._currAtomicChainStart = itChain->second;
            }

            dataStack.push(data);
        }

        callback(op, it, data);

        if (Opcode::EndAtomic == op._opcode)
        {
            assert(data._inAtomic);
            assert(!dataStack.empty());
            dataStack.pop();
        }
    }

    assert(dataStack.empty());
}

bool IsAsyncCallEntryPoint(const BasicBlock& basicBlock)
{
    bool result = false;

    if (basicBlock.IsFirstInFunction())
    {
        if (basicBlock._function->_functionNode->GetModifiers() & ParseTreeFunctionModifierAsync)
        {
            result = true;
        }
    }

    return result;
}

bool StageHasExternalInputRegisters(const Stage& stage)
{
    bool result = false;

    for (const Operation& op : stage._operations)
    {
        result |= OpcodeHasExternalInputRegisters(op._opcode);
    }

    return result;
}

bool StageRequiresRegisteredInput(const Stage& stage)
{
    bool stageRequiresRegisters = false;

    for (const Operation& op : stage._operations)
    {
        // Check opcode
        stageRequiresRegisters |= OperationHasRegisteredInput(op);
    }

    return stageRequiresRegisters;
}

bool StageRequiresRegisteredOutput(const Program& program, const Stage& stage)
{
    bool stageRequiresRegisters = false;

    for (const Operation& op : stage._operations)
    {
        // Check opcode
        stageRequiresRegisters |= OperationHasRegisteredOutput(op);
    }

    return stageRequiresRegisters;
}

// Allocates a register that is large enough to hold all live values
size_t AllocatePipelineRegister(Program& program, const RegisterSet& liveRegisters, const RegisterType type,
                                const std::string& name)
{
    // Compute the number of bits needed
    size_t widthSum = 0;

    for (const size_t registerIndex : liveRegisters)
    {
        const size_t registerWidth = program._registerTable[registerIndex]._width;

        widthSum += registerWidth;
    }

    return AllocateRegister(&program, widthSum, type, name);
}

// Allocates a register that is large enough to hold registers in a RegisterOffsetMap
size_t AllocatePipelineRegister(Program& program, const FifoOffsetMap& offsetMap, const RegisterType type,
                                const std::string& name)
{
    // Compute the number of bits needed
    size_t widthSum = 0;

    for (const auto& p : offsetMap)
    {
        const size_t registerIndex = p.first;

        const size_t registerWidth = program._registerTable[registerIndex]._width;

        widthSum += registerWidth;
    }

    return AllocateRegister(&program, widthSum, type, name);
}

// Remaps a register access from local registers to pipeline register
void RemapRegister(size_t& registerIndex, const LocalToPipelineRegisterMap& registerMap)
{
    const auto it = registerMap.find(registerIndex);
    if (it != registerMap.end())
    {
        registerIndex = it->second;
    }
}

// Debug symbols preparation
// Maps source variables to allocated registers (note: these could be either wire
// or pipeline registers being inserted for the global/local/memory source)
void MapLocalSourceToAllocatedRegister(Program& program, const size_t localIndex, const size_t registerIndex)
{
    const auto it = program._sourceToPipelineRegisterMap.find(localIndex);

    if (it != program._sourceToPipelineRegisterMap.end())
    {
        program._sourceToPipelineRegisterMap[localIndex].insert(registerIndex);

        SafeInsert(program._pipelineToSource, registerIndex, localIndex);

        // Propagate source variable information to new registers/wires
        assert(program._registerTable[localIndex]._isDirectSource);
        program._registerTable[registerIndex]._sourceVariable = program._registerTable[localIndex]._sourceVariable;
        program._registerTable[registerIndex]._isDirectSource = program._registerTable[localIndex]._isDirectSource;
        program._registerTable[registerIndex]._sourceType = program._registerTable[localIndex]._sourceType;
    }
}

// Get registers that are live-in to the successor of the passed op and add them
// to liveInToSuccessor. If op does not have a successor, then nothing is done.
void GetLiveInToSuccessor(const Operation& op, const BasicBlock& basicBlock, RegisterSetMap& liveInMap,
                          const std::unordered_set<const BasicBlock*>& returnSites, RegisterSet& liveInToSuccessor)
{
    assert(op._getSuccessorBlock != NULL);

    BasicBlock* const successor = op._getSuccessorBlock();

    // For async calls, there is no need to compute live registers
    // The only registers are parameters, which will be made live at the call site anyways
    if (!IsAsyncCallEntryPoint(*successor))
    {
        const RegisterSet& successorLiveInSet = liveInMap[successor];

        for (const size_t registerIndex : successorLiveInSet)
        {
            liveInToSuccessor.insert(MapOriginalToRenamedRegisterIndex(op, registerIndex));
        }

        // Also include registers which are forced live-in at the successor
        // The sucessor is in a different function in this case, so liveInMap
        // will not have been populated for that basic block
        for (const size_t registerIndex : successor->_liveInReg)
        {
            liveInToSuccessor.insert(MapOriginalToRenamedRegisterIndex(op, registerIndex));
        }

        // Don't include registers that should be considered not live-in at the successor
        for (const size_t registerIndex : successor->_forcedDeadInReg)
        {
            liveInToSuccessor.erase(MapOriginalToRenamedRegisterIndex(op, registerIndex));
        }

        if ((op._flags._enqueue._type == EnqueueType::ContextSaverCaller) &&
            (successor->_invocationInstanceRegisterIndex))
        {
            // The invocation instance ID is marked live in to the return basic block
            // to ensure the callee passes it back to the context saver
            // But this register should not be considered to be live-in to the
            // enqueue operation that writes data into the context saver (that operation allocates the instance ID);
            liveInToSuccessor.erase(
                MapOriginalToRenamedRegisterIndex(op, *successor->_invocationInstanceRegisterIndex));
        }
    }

    // If the successor is a return site, the input registers
    // will come from 2 basic blocks - the caller and the callee
    // If "basicBlock" is the caller, then remove the registers
    // that will be supplied by the return site
    // If "basicBlock" is the callee, then everything works out
    // because successor will be in a different function
    // so the only live-in registers will be the ones in successor->_returnValueReg
    const auto it = returnSites.find(successor);

    if (it != returnSites.end())
    {
        if (basicBlock._function == successor->_function)
        {
            // basicBlock is the call site
            // Remove return values supplied by the callee
            for (const size_t registerIndex : successor->_returnValueReg)
            {
                liveInToSuccessor.erase(MapOriginalToRenamedRegisterIndex(op, registerIndex));
            }
        }
    }
}

RegisterSet ComputeLiveRegistersBeforeStage(const Program& program, const Stage& stage, const RegisterSet& liveAfter,
                                            const BasicBlock& srcBasicBlock, RegisterSetMap& liveInMap,
                                            const std::unordered_set<const BasicBlock*>& returnSites)
{
    RegisterSet result = liveAfter;

    // It is important to go in reverse order - to handle dependencies correctly
    for (auto it = stage._operations.rbegin(); it != stage._operations.rend(); ++it)
    {
        const Operation& op = *it;

        // A register is dead before a stage if it is overwritten by that stage
        for (const DestinationOperand& destOp : op._dst)
        {
            if (destOp.Type() == DestinationOperandType::Register)
            {
                const AccessedRegister& reg = destOp.GetAccessedRegister();

                if (IsLocalRegister(program, reg))
                {
                    result.erase(reg._registerIndex);
                }
            }
        }

        // A register is live before a stage if it is read by that stage
        // Note that for WriteIndexed, the destination is listed as a source also
        // so the array will stay live when a read-modify-write operation only updates some of it
        for (const SourceOperand& sourceOperand : op._src)
        {
            if (sourceOperand.Type() == SourceOperandType::Register)
            {
                const AccessedRegister& reg = sourceOperand.GetAccessedRegister();

                if (IsLocalRegister(program, reg))
                {
                    result.insert(reg._registerIndex);
                }
            }
        }

        // Enqueue operations can also add the set of registers which are live-in at the successor
        if (op._getSuccessorBlock)
        {
            GetLiveInToSuccessor(op, srcBasicBlock, liveInMap, returnSites, result);
        }
    }

    return result;
}

// Compute live-in map for the passed function
void ComputeLiveInMap(const Program& program, Function& function, ControlFlowGraph& controlFlowGraph,
                      const std::unordered_set<const BasicBlock*>& returnSites, RegisterSetMap& liveInMap,
                      const OperationEnumerationMode mode)
{
    // This is set of basic blocks that need to be processed
    DataFlowWorkList workList(controlFlowGraph, function);

    // While the computation has not converged
    while (!workList.Empty())
    {
        // Choose a basic block
        BasicBlock* const basicBlock = workList.Pop();

        // Initially empty
        RegisterSet liveRegisters;

        // Iterate through all operations, in reverse order
        ForEachOperationReverse(
            *basicBlock,
            [&](const Operation& op)
            {
                // A register is dead before a basic block if it is overwritten by that basic block
                for (const DestinationOperand& destOp : op._dst)
                {
                    if (destOp.Type() == DestinationOperandType::Register)
                    {
                        const AccessedRegister& reg = destOp.GetAccessedRegister();

                        if (IsLocalRegister(program, reg))
                        {
                            liveRegisters.erase(reg._registerIndex);
                        }
                    }
                }

                // A register is live before a basic block if it is read by that basic block
                // Note that for WriteIndexed, the destination is listed as a source also
                // so the array will stay live when a read-modify-write operation only updates some of it
                for (const SourceOperand& sourceOperand : op._src)
                {
                    if (sourceOperand.Type() == SourceOperandType::Register)
                    {
                        const AccessedRegister& reg = sourceOperand.GetAccessedRegister();

                        if (IsLocalRegister(program, reg))
                        {
                            liveRegisters.insert(reg._registerIndex);
                        }
                    }
                }

                // Enqueue operations can also add the set of registers which are live-in at the successor
                if (op._getSuccessorBlock)
                {
                    GetLiveInToSuccessor(op, *basicBlock, liveInMap, returnSites, liveRegisters);
                }
            },
            mode);

        // If the set of live input registers for this basic block has changed since this last time it was computed
        // then add all predecessor blocks to the working set
        if (liveRegisters != liveInMap[basicBlock])
        {
            workList.AddPredecessors(basicBlock);

            // Save the new set of live input registers in the basic block
            liveInMap[basicBlock] = liveRegisters;
        }
    }
}

// Compute live-in map for all functions in the program
void ComputeLiveInMap(Program& program, RegisterSetMap& liveInMap, std::unordered_set<const BasicBlock*>& returnSites,
                      const OperationEnumerationMode mode)
{
    assert(liveInMap.empty());
    assert(returnSites.empty());
    assert(mode == OperationEnumerationMode::Unscheduled); // the only supported mode, ControlFlowGraphPhase would have
                                                           // to change if scheduled mode was supported.

    // Build a list of basic blocks that are points where a function call returns
    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        returnSites.insert(contextSaver._afterCall);
    }

    for (Function& function : program._functions)
    {
        // Build the control flow graph
        ControlFlowGraph controlFlowGraph(function, program, OperationEnumerationMode::Unscheduled,
                                          ControlFlowGraphPhase::PrePipeline);

        // Compute live-in registers for basic blocks in this function
        ComputeLiveInMap(program, function, controlFlowGraph, returnSites, liveInMap,
                         OperationEnumerationMode::Unscheduled);
    }
}

// Called after clock gating has been computed
// Convert ConditionalIgnore to mov before the backend
void RemoveConditionalIgnore(Function& function)
{
    for (BasicBlock& bb : function._basicBlocks)
    {
        for (Stage& stage : bb._stages)
        {
            for (auto it = stage._operations.begin(); it != stage._operations.end(); ++it)
            {
                Operation& op = *it;

                if (Opcode::ConditionalIgnore == op._opcode)
                {
                    op._opcode = Opcode::Mov;
                    op._src.resize(1);
                }
            }
        }
    }
}

// Called after pipelining
// Changes fanout operations to move operations
// Marks desitnation registers are needing to be preserved
void TransformFanOutOperations(Program& program, BasicBlock& basicBlock)
{
    for (Stage& stage : basicBlock._stages)
    {
        for (Operation& op : stage._operations)
        {
            if (Opcode::FanOut == op._opcode)
            {
                assert(1 == op._src.size());
                assert(1 == op._dst.size());

                // Backends do not handle the FanOut opcode
                // Transform it to a move
                op._opcode = Opcode::Mov;

                // Mark the register so that the hint will be emitted in the RTL to indicate that this duplicate
                // register should not be removed
                const size_t dstRegister = op._dst[0].GetAccessedRegister()._registerIndex;

                RegisterDescription& regDesc = program._registerTable[dstRegister];

                assert(!regDesc._preserve);

                regDesc._preserve = true;
            }
        }
    }
}

// Decompose the flat list of Stage structures into nested
// PipelineStage/Stage
// For computations that need to differentiate between pipeline stages and stage structures
std::vector<PipelineStage> GetPipelineStages(BasicBlock& basicBlock)
{
    std::vector<PipelineStage> result;

    PipelineStage currentPipelineStage;

    for (Stage& stage : basicBlock._stages)
    {
        if (stage._atomicSequence != result.size())
        {
            // This is a new pipeline stage
            result.push_back(currentPipelineStage);
            currentPipelineStage.clear();

            assert(stage._atomicSequence == result.size());
        }

        currentPipelineStage.push_back(&stage);
    }

    // Get any stragglers
    if (!currentPipelineStage.empty())
    {
        result.push_back(currentPipelineStage);
    }

    // Assert that results are correct
    for (size_t pipelineStageIndex = 0; pipelineStageIndex < result.size(); pipelineStageIndex++)
    {
        const PipelineStage& pipelineStage = result[pipelineStageIndex];

        for (const Stage* const stage : pipelineStage)
        {
            assert(stage->_atomicSequence == pipelineStageIndex);
        }
    }

    return result;
}

// Used by AddPipelineRegisters to allocate a wire/pipeline register that will hold the value of a local
size_t AllocateIntraStageRegister(Program& program, const size_t localIndex, const RegisterType pipelineRegisterType)
{
    assert(IsLocalRegister(program, localIndex));

    // Allocate a new register to hold the result
    const size_t width = program._registerTable[localIndex]._width;
    const std::string& name = program._registerTable[localIndex]._name;

    return AllocateRegister(&program, width, pipelineRegisterType, name.c_str());
}

// Remove clock gates that don't gate enough registers to
// justify the power consumed by the gate
void CheckClockGateThreshold(const Program& program, Stage& stage)
{
    // Assert that no gate register is itself gated
    ClockGateSet clockGateRegisters;

    for (const auto& p : stage._clockGates)
    {
        Union(clockGateRegisters, p.second);
    }

    for (const size_t gateRegister : clockGateRegisters)
    {
        assert(stage._clockGates.end() == stage._clockGates.find(gateRegister));
    }

    // Remove clock gates if the threshold is not met

    // Maps set of clock gate registers to number of gated register bits
    std::map<std::set<size_t>, size_t> clockGatesToRegisterCount;

    for (const auto& p : stage._clockGates)
    {
        const size_t gatedRegister = p.first;
        const std::set<size_t>& gateRegisters = p.second;

        const size_t gatedRegisterWidth = program._registerTable[gatedRegister]._width;

        clockGatesToRegisterCount[gateRegisters] += gatedRegisterWidth;
    }

    for (auto it = stage._clockGates.begin(); it != stage._clockGates.end();)
    {
        auto nextIt = it;
        ++nextIt;

        const std::set<size_t>& gateRegisters = it->second;

        const size_t numGatedRegisters = clockGatesToRegisterCount[gateRegisters];

        if (GetCodeGenConfig().ClockGateBelowThreshold(numGatedRegisters))
        {
            stage._clockGates.erase(it);
        }

        it = nextIt;
    }
}

// Converts register accesses to pipeline register accesses
void AddPipelineRegisters(IRContext& context, BasicBlock& basicBlock, RegisterSetMap& liveInMap,
                          const std::unordered_set<const BasicBlock*>& returnSites,
                          const BasicBlockClockGateMap& clockGateMap)
{
    Program& program = *context._program;

    // Empty basic blocks have already been removed
    assert(!basicBlock._stages.empty());

    // Holds the set of registers which are live after the corresponding stage
    std::map<const Stage*, RegisterSet> liveRegisterStages;

    RegisterSet tempLiveRegisters;

    // Compute the set of registers which are live before each stage
    // A register is removed from the live set if it is written by a stage
    // A register is added to the live set if it is read by a stage
    for (auto it = basicBlock._stages.rbegin(); it != basicBlock._stages.rend(); ++it)
    {
        const Stage& stage = *it;

        SafeInsert(liveRegisterStages, &stage, tempLiveRegisters);

        const RegisterSet liveBeforeCurrentStage =
            ComputeLiveRegistersBeforeStage(program, stage, tempLiveRegisters, basicBlock, liveInMap, returnSites);

        tempLiveRegisters = liveBeforeCurrentStage;
    }

    // Get nested PipelineStage/Stage structure
    const std::vector<PipelineStage> pipelineStages = GetPipelineStages(basicBlock);

    // Save registers which are live-in to the first stage
    RegisterSet firstStageLiveRegisters = tempLiveRegisters;

    // Some registers are always considered live-in, add those here
    // Registers in _externalOnlyLiveInReg are needed for context savers/loop generators
    // but are not required by logic inside of the basic block
    const std::set<size_t> externalOnlyLiveInReg(basicBlock._externalOnlyLiveInReg.begin(),
                                                 basicBlock._externalOnlyLiveInReg.end());

    for (const size_t registerIndex : basicBlock._liveInReg)
    {
        if (externalOnlyLiveInReg.end() == externalOnlyLiveInReg.find(registerIndex))
        {
            tempLiveRegisters.insert(registerIndex);
        }
    }

    // Remove the input fifo for reset functions that have no call sites (tempLiveRegisters)
    // This is done to avoid having a FIFO in the generated RTL with nothing connected to the write port
    // that is never used.
    if (basicBlock.IsResetBlock() && tempLiveRegisters.empty() && g_compiler->IsCompilingToVerilog())
    {
        basicBlock._inputFifoCount = 0;
    }

    // Allocate FIFOs for callers to write inputs into
    for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
    {
        const std::string fifoName = GetBasicBlockName(basicBlock);

        basicBlock._inputFifoIndices[i] =
            AllocatePipelineRegister(program, tempLiveRegisters, RegisterType::Fifo, fifoName);

        program._registerTable[basicBlock._inputFifoIndices[i]].Fifo().SetLocation(basicBlock._location,
                                                                                   basicBlock._callStackIndex);
    }

    // Maps local registers to pipeline registers before the current stage
    LocalToPipelineRegisterMap localToBeforePipelineMap;

    // Construct a stage that will read from the FIFO into wires associated with each live-in register
    Stage initialStage;

    // To help timing - add a pipeline stage to just read fifo outputs wherever possible
    // HasStartCondition is needed for correctness
    // Fixed-latency exports do not insert the addition stage to avoid a lower-bound function latency
    const bool addStageForFifoRead =
        !basicBlock.HasStartCondition() && !basicBlock._function->_functionNode->IsFixedLatency();

    for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
    {
        RegisterDescription& inputFifoRegDesc = program._registerTable[basicBlock._inputFifoIndices[i]];

        // No named ranges should have been filled in yet
        // Code that writes these ranges later in the function assumes
        // that it doesn't need to check for duplicates.
        assert(inputFifoRegDesc.Fifo()._namedRanges.empty());

        const auto minFifoDepth = basicBlock._inputFifoMinDepth[i];
        if (minFifoDepth != 0)
        {
            assert(0 == inputFifoRegDesc.Fifo()._depth);

            // max(x, DeviceConfig._minFifoDepth) is used to ensure MLAB space is not wasted
            inputFifoRegDesc.Fifo()._depth =
                std::max<size_t>(minFifoDepth, GetCodeGenDeviceConfig()._minFifoDepth);
        }
        else
        {
            // Fifo size will be set later - based on the feeding basic block
        }

        const size_t maxFifoDepth = basicBlock._inputFifoMaxDepth[i];

        if (maxFifoDepth != 0)
        {
            assert(0 == inputFifoRegDesc.Fifo()._maxDepth);

            // Indicate that FIFO does not need to be deeper than maxFifoDepth
            // If the FIFO depth is maxFifoDepth, then full/almost_full values do not need to be checked
            // _minFifoDepth is used to avoid emitting a fifo with depth = 1
            inputFifoRegDesc.Fifo()._maxDepth = std::max(maxFifoDepth, GetCodeGenDeviceConfig()._minFifoDepth);
        }
    }

    AccessedRegister initialCallLocalReg = {c_invalidAccessedRegisterIndex};

    // Determine whether global writes should occur before or after Start Condition
    bool globalWritesAfterStartCondition = false;
    if (basicBlock.HasStartCondition())
    {
        bool startConditionFound = false;
        for (const Stage& stage : basicBlock._stages)
        {
            for (const Operation& op : stage._operations)
            {
                if (op._opcode == Opcode::StartCondition)
                {
                    globalWritesAfterStartCondition = !op._flags._startCondition._globalsBefore;
                    startConditionFound = true;
                    break;
                }
            }

            if (startConditionFound)
            {
                break;
            }
        }
        assert(startConditionFound);
    }

    // Create a stage for global writes if they need to occur after start condition
    Stage globalWriteStage;

    if (basicBlock.IsResetBlock() && !tempLiveRegisters.empty())
    {
        // Allocate a global variable to track when the first call is made
        // The first call is the auto-generated one.
        // Set all live registers (including the hidden call index)
        // to 0 for this call (they are uninitialized)

        // A shared variable is used to keep track of if the initial call has occurred yet
        const ObjectPath basicBlockPath = basicBlock.GetObjectPath();
        const std::string initCallMadeName = context.GenerateUniqueNameWithinPath(
            basicBlockPath, FixupString(basicBlock._function->_name) + "_initial_call_made");
        const ObjectPath initCallMadePath = AppendToPath(basicBlockPath, initCallMadeName);
        const AccessedRegister initialCallSharedReg = {
            AllocateRegister(&program, 1, RegisterType::Global, initCallMadeName, initCallMadePath)};

        // Initialize to false on reset
        {
            RegisterDescription::GlobalDesc& gd = program._registerTable[initialCallSharedReg._registerIndex].Global();
            gd._hasInitialValue = true;
            gd._initialValue = 0;
            gd._writeCount = 1;
        }

        // Copy the value of the shared variable into a local
        initialCallLocalReg = {AllocateRegister(
            &program, 1, RegisterType::Wire, FixupString(basicBlock._function->_name) + "_snapped_initial_call_made")};

        {
            Operation mov = {};
            mov._locations.insert(LocationToFileAndLineNumber(basicBlock._location));

            mov._opcode = Opcode::Mov;
            mov._src.push_back(initialCallSharedReg);
            mov._dst.push_back(initialCallLocalReg);

            initialStage._operations.push_back(mov);
        }

        // Set the shared variable to true
        {
            Operation writeGlobal = {};
            writeGlobal._locations.insert(LocationToFileAndLineNumber(basicBlock._location));

            writeGlobal._opcode = Opcode::WriteGlobal;
            writeGlobal._src.push_back(SourceOperand(1));
            writeGlobal._dst.push_back(initialCallSharedReg);

            writeGlobal._dst[0].SetWriteIndex(0);

            // If basic block has start condition, then insert global writes
            // after start condition
            if (globalWritesAfterStartCondition)
            {
                globalWriteStage._operations.push_back(writeGlobal);
            }
            else
            {
                initialStage._operations.push_back(writeGlobal);
            }
        }
    }

    size_t inputFifoOffset = 0;

    for (const size_t registerIndex : tempLiveRegisters)
    {
        // Not a reference because AllocateRegister is called here
        const RegisterDescription regDesc = program._registerTable[registerIndex];

        const size_t width = regDesc._width;

        const std::string& name = regDesc._name;

        // Record the fifo offset, so that writes to the FIFO can use the correct offset
        basicBlock._inputFifoRegisterMap[registerIndex] = inputFifoOffset;

        // Only read from the FIFO for values which are live-in to the first stage
        if (firstStageLiveRegisters.end() != firstStageLiveRegisters.find(registerIndex))
        {
            const AccessedRegister dst = {AllocateRegister(
                &program, width, addStageForFifoRead ? RegisterType::Pipeline : RegisterType::Wire, name.c_str())};

            Operation op = {};

            op._opcode = (basicBlock._inputFifoCount > 1) ? Opcode::ReadSelectedFifo : Opcode::Mov;

            // The debugger expects these fifo moves to not be associated with a location
            op._expectNoSourceLocation = true;

            for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
            {
                const FifoSubset src = {basicBlock._inputFifoIndices[i], inputFifoOffset, width};

                op._src.push_back(SourceOperand(src));
            }

            if (basicBlock.IsResetBlock())
            {
                OperationList ops;

                {
                    SetOperationLocation sol(ops, basicBlock._location);

                    // Replace the FIFO output with 0 if the local register is false
                    const AccessedRegister beforeZero = {
                        AllocateRegister(&program, width, RegisterType::Wire, name.c_str())};

                    op._dst.push_back(beforeZero);

                    ops.push_front(op);

                    {
                        Operation zeroOp = {};

                        zeroOp._opcode = Opcode::Select;

                        zeroOp._src.push_back(initialCallLocalReg);
                        zeroOp._src.push_back(SourceOperand(0));
                        zeroOp._src.push_back(beforeZero);

                        zeroOp._dst.push_back(dst);

                        ops.push_back(zeroOp);
                    }
                }

                initialStage._operations.splice(initialStage._operations.end(), ops);
            }
            else
            {
                op._dst.push_back(dst);

                initialStage._operations.push_back(op);
            }

            // Record the local->wire register mapping
            localToBeforePipelineMap[registerIndex] = dst._registerIndex;

            // Debug symbols preparation
            MapLocalSourceToAllocatedRegister(program, registerIndex, dst._registerIndex);

            // Save information in the fifo description
            // For output in the RTL map
            for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
            {
                RegisterDescription& inputFifoRegDesc = program._registerTable[basicBlock._inputFifoIndices[i]];

                inputFifoRegDesc.Fifo().AddNamedRange(inputFifoOffset, width, name);
            }
        }

        inputFifoOffset += width;
    }

    // Save the local->pipeline mapping at the start of the initial stage
    initialStage._beforeStageRegisterMap = localToBeforePipelineMap;

    for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
    {
        assert(inputFifoOffset == program._registerTable[basicBlock._inputFifoIndices[i]]._width);
    }

    // Stage for moves after global write
    Stage afterGlobalWriteStage;

    for (size_t pipelineStageIndex = 0; pipelineStageIndex < pipelineStages.size(); ++pipelineStageIndex)
    {
        const PipelineStage& pipelineStage = pipelineStages[pipelineStageIndex];

        // Determine if this is the final pipeline stage in this basic block
        const bool isFinalPipelineStage = (pipelineStageIndex + 1) == pipelineStages.size();

        assert(!pipelineStage.empty());
        Stage* const finalSubStage = pipelineStage.back();

        // The set of registers which are live after this pipeline stage
        const RegisterSet& liveAfterPipelineStage =
            SafeLookup(liveRegisterStages, static_cast<const Stage*>(finalSubStage));

        // Keep track of the set of locals which are live after this pipeline stage
        // but not written into a pipeline register by this stage
        // Extra move operations will be inserted to move values along to pipeline registers
        RegisterSet registersToMove;

        if (!isFinalPipelineStage)
        {
            registersToMove = liveAfterPipelineStage;
        }

        LocalToPipelineRegisterMap localToAfterPipelineMap;

        // Allocate wires/pipeline registers, and remap all locals to those
        for (size_t subStageIndex = 0; subStageIndex < pipelineStage.size(); ++subStageIndex)
        {
            const bool isFinalSubStage = (subStageIndex + 1) == pipelineStage.size();

            const bool isFinalSubStageOfPipeline = isFinalPipelineStage && isFinalSubStage;

            Stage& stage = *(pipelineStage[subStageIndex]);

            // Save the local->{wire, pipeline} mapping at the start
            stage._beforeStageRegisterMap = localToBeforePipelineMap;

            // Change all source operands to be from the pipeline register/wire before this stage
            for (Operation& op : stage._operations)
            {
                for (SourceOperand& sourceOperand : op._src)
                {
                    if (sourceOperand.Type() == SourceOperandType::Register)
                    {
                        RemapRegister(sourceOperand.GetAccessedRegister()._registerIndex, localToBeforePipelineMap);
                    }
                }
            }

            const RegisterSet& liveAfterSubStage = SafeLookup(liveRegisterStages, static_cast<const Stage*>(&stage));

            // Change all destination operands to target the pipeline register/wire after this stage
            for (Operation& op : stage._operations)
            {
                // It is hard for backends to handle
                // Opcode::InlineExternalModule with some outputs mapped to RegisterType::BitBucket
                // So registers are invented here for unused outputs to be placed in
                const bool allowBitbucket = OpcodeCanUseBitBucketOutput(op._opcode);

                for (DestinationOperand& destOp : op._dst)
                {
                    if (destOp.Type() == DestinationOperandType::Register)
                    {
                        AccessedRegister& originalReg = destOp.GetAccessedRegister();

                        if (IsLocalRegister(program, originalReg) && allowBitbucket &&
                            (isFinalSubStageOfPipeline ||
                             (liveAfterSubStage.end() == liveAfterSubStage.find(originalReg._registerIndex))))
                        {
                            const std::string name = program.GetRegisterName(originalReg);

                            // The destination is not live, change the destination register specifier to indicate that
                            // the result should be dropped This case happens for operations which have side effects
                            // (and thus cannot be optimize away) and produce a result (which is not used) The width is
                            // preserved to enable memory load operations to use the width to compute offsets within the
                            // returned valued (for memories of structures)
                            originalReg._registerIndex = AllocateRegister(
                                &program, static_cast<size_t>(destOp.Width(program)), RegisterType::BitBucket, name);
                        }
                        else
                        {
                            if (IsLocalRegister(program, originalReg._registerIndex))
                            {
                                const bool registeredOutput = OperationHasRegisteredOutput(op);

                                // If this is the final substage in this pipeline stage or if the operation has
                                // registered output -> then allocate a pipeline register otherwise allocate a wire
                                // Inline atomic functions do not use pipeline registers
                                // The registered output case necessary to avoid trying to add mov operations that read
                                // from the hardened output register
                                const RegisterType pipelineRegisterType = ((isFinalSubStage || registeredOutput))
                                                                              ? RegisterType::Pipeline
                                                                              : RegisterType::Wire;

                                if (RegisterType::Pipeline == pipelineRegisterType)
                                {
                                    // This value will be available to the next stage as in a pipeline register
                                    // so there is no need to add another move operation
                                    registersToMove.erase(originalReg._registerIndex);
                                }

                                // Allocate a register/wire for the destination to be written into
                                const size_t pipelineRegisterIndex = AllocateIntraStageRegister(
                                    program, originalReg._registerIndex, pipelineRegisterType);

                                localToAfterPipelineMap[originalReg._registerIndex] = pipelineRegisterIndex;
                                localToBeforePipelineMap[originalReg._registerIndex] = pipelineRegisterIndex;

                                // Debug symbols preparation
                                MapLocalSourceToAllocatedRegister(program, originalReg._registerIndex,
                                                                  pipelineRegisterIndex);
                            }

                            RemapRegister(destOp.GetAccessedRegister()._registerIndex, localToAfterPipelineMap);
                        }
                    }
                }
            }

            // Remove operations that were re-mapped to the bitbucket and have no side-effects
            const auto callback = [&program](const Operation& op)
            {
                bool result = false;

                if (op._dst.size() == 1)
                {
                    const DestinationOperand& destOp = op._dst[0];

                    if (destOp.Type() == DestinationOperandType::Register)
                    {
                        result = (program._registerTable[destOp.GetAccessedRegister()._registerIndex]._type ==
                                  RegisterType::BitBucket);
                    }
                }

                return result;
            };

            stage._operations.remove_if(callback);
        }

        // Insert the move operations (in the last substage associated with this stage)
        // This ensures that all locals that are live after this pipeline stage will be written by this pipeline stage
        for (const size_t registerIndex : registersToMove)
        {
            // Allocate a new register to hold the result
            const RegisterType pipelineRegisterType = RegisterType::Pipeline;

            const size_t pipelineRegisterIndex =
                AllocateIntraStageRegister(program, registerIndex, pipelineRegisterType);

            const AccessedRegister srcAccessedRegister = {SafeLookup(localToBeforePipelineMap, registerIndex)};
            const AccessedRegister dstAccessedRegister = {pipelineRegisterIndex};

            // Debug symbols preparation
            MapLocalSourceToAllocatedRegister(program, registerIndex, pipelineRegisterIndex);

            Operation op = {};

            op._opcode = Opcode::Mov;

            // There is no reasonable location to associated with this mov
            op._expectNoSourceLocation = true;

            op._src.push_back(srcAccessedRegister);
            op._dst.push_back(dstAccessedRegister);

            localToAfterPipelineMap[registerIndex] = pipelineRegisterIndex;

            if (!globalWriteStage._operations.empty() && finalSubStage->_atomicSequence == 0)
            {
                // Insert these ops into a stage after global writes stage to
                // ensure read dependencies are met.
                afterGlobalWriteStage._operations.push_back(op);
            }
            else
            {
                // push_front is used to ensure the MOVs operations appear before jump operations
                finalSubStage->_operations.push_front(op);
            }

            // for the back-end, remember that pipelineRegisterIndex is produced by moving srcAccessedRegister
            SafeInsert(basicBlock._pipelineMoveMap, pipelineRegisterIndex, srcAccessedRegister._registerIndex);
        }

        // Save clock gating information for outputs of this pipeline stage
        for (const auto& p : localToAfterPipelineMap)
        {
            const size_t localReg = p.first;

            // Only record clock gating information for registers that are live
            // at the end of this pipeline stage
            if (liveAfterPipelineStage.end() == liveAfterPipelineStage.find(localReg))
            {
                continue;
            }

            const auto it = clockGateMap.find(localReg);

            if (it != clockGateMap.end())
            {
                // localReg register is gated localGateRegisters
                const ClockGateSet& localGateRegisters = it->second;

                // Check to see if which gate registers are present in the pipeline registers
                // that are output of this stage
                // The gate register may not have been computed yet
                ClockGateSet pipelinedGateRegisters;

                for (const size_t localGateRegister : localGateRegisters)
                {
                    const auto it2 = localToAfterPipelineMap.find(localGateRegister);

                    if (it2 != localToAfterPipelineMap.end())
                    {
                        // Don't allow values computed on this pipeline stage to act as clock
                        // gates for the next pipeline stage (for avoid adding the computation of the clock gate values
                        // to the critical path)
                        if (registersToMove.end() != registersToMove.find(localGateRegister))
                        {
                            // Pipelined version of the gate register
                            pipelinedGateRegisters.insert(it2->second);
                        }
                    }
                }

                if (!pipelinedGateRegisters.empty())
                {
                    // Pipelined version of the gated register
                    const size_t pipelineGatedReg = p.second;

                    SafeInsert(finalSubStage->_clockGates, pipelineGatedReg, pipelinedGateRegisters);
                }
            }
        }

        CheckClockGateThreshold(program, *finalSubStage);

        localToBeforePipelineMap = localToAfterPipelineMap;
    }

    // Insert the initial stage
    if (addStageForFifoRead)
    {
        // RegisterIO is not allowed in start conditions
        // This is important because the initial stage must occur on the same cycle as the start condition
        assert(!basicBlock.HasStartCondition());
        // These stages should only occur if basic block has start condition
        assert(globalWriteStage._operations.empty());
        assert(afterGlobalWriteStage._operations.empty());

        for (Stage& stage : basicBlock._stages)
        {
            stage._atomicSequence++;
        }

        initialStage._atomicSequence = basicBlock._stages.front()._atomicSequence - 1;
    }
    else
    {
        // If the first pipeline stage has registered input, then a pipeline stage must be inserted before it
        // ScheduleOperations take care of this
        assert(!StageRequiresRegisteredInput(basicBlock._stages.front()));

        assert(0 == basicBlock._stages.front()._atomicSequence);
        initialStage._atomicSequence = 0;
        globalWriteStage._atomicSequence = 0;
        afterGlobalWriteStage._atomicSequence = 0;
    }

    // Insert initial stage
    basicBlock._stages.push_front(initialStage);

    // Insert stage with global write after start condition
    if (!globalWriteStage._operations.empty())
    {
        bool startConditionFound = false;
        for (auto it = basicBlock._stages.begin(); it != basicBlock._stages.end();)
        {
            auto nextIt = it;
            nextIt++;

            const Stage& stage = *it;
            for (const Operation& op : stage._operations)
            {
                if (op._opcode == Opcode::StartCondition)
                {
                    startConditionFound = true;
                    break;
                }
            }

            if (startConditionFound)
            {
                basicBlock._stages.insert(nextIt, globalWriteStage);
                if (!afterGlobalWriteStage._operations.empty())
                {
                    basicBlock._stages.insert(nextIt, afterGlobalWriteStage);
                }
                break;
            }

            it = nextIt;
        }
        assert(startConditionFound);
    }
}

// Note that this runs before scheduling
std::unordered_set<BasicBlock*> GetSuccessors(const BasicBlock& basicBlock)
{
    std::unordered_set<BasicBlock*> result;

    // Unscheduled operations
    for (const Operation& op : basicBlock._operations)
    {
        if (op._getSuccessorBlock)
        {
            result.insert(op._getSuccessorBlock());
        }
    }

    // Appended enqueues
    for (const Stage& stage : basicBlock._stages)
    {
        for (const Operation& op : stage._operations)
        {
            if (op._getSuccessorBlock)
            {
                result.insert(op._getSuccessorBlock());
            }
        }
    }

    return result;
}

void GetAccessedRegisters(const Operation& op, std::vector<size_t>& read, std::vector<size_t>& write)
{
    read.clear();
    write.clear();

    for (const SourceOperand& sourceOperand : op._src)
    {
        if (sourceOperand.Type() == SourceOperandType::Register)
        {
            const AccessedRegister& ar = sourceOperand.GetAccessedRegister();

            read.push_back(ar._registerIndex);
        }
    }

    for (const DestinationOperand& destOp : op._dst)
    {
        if (destOp.Type() == DestinationOperandType::Register)
        {
            const AccessedRegister& ar = destOp.GetAccessedRegister();

            write.push_back(ar._registerIndex);
        }
    }
}

// Returns the number of bits in the register, if it is local
// Returns 0 otherwise
size_t GetLocalBitCount(const Program& program, const size_t registerIndex)
{
    const RegisterDescription& regDesc = program._registerTable[registerIndex];

    return IsLocalRegisterType(regDesc._type) ? regDesc._width : 0;
}

// Returns the number of bits of destination operands which are local
size_t GetLocalDestBitCount(const Program& program, const Operation& op)
{
    size_t result = 0;

    for (const DestinationOperand& dst : op._dst)
    {
        if (DestinationOperandType::Register == dst.Type())
        {
            result += GetLocalBitCount(program, dst.GetAccessedRegister()._registerIndex);
        }
    }

    return result;
}

// Returns the number of bits of source operands which are local, and are not live after the current stage
size_t GetLocalSourceAlmostDeadBitCount(const Program& program, const Operation& op,
                                        const std::set<size_t>& liveAfterStage)
{
    size_t result = 0;

    for (const SourceOperand& src : op._src)
    {
        if (SourceOperandType::Register == src.Type())
        {
            const size_t registerIndex = src.GetAccessedRegister()._registerIndex;

            if (liveAfterStage.end() == liveAfterStage.find(registerIndex))
            {
                result += GetLocalBitCount(program, registerIndex);
            }
        }
    }

    return result;
}

// Returns (true, registerIndex) if the operation accesses a memory
// the register index coressponds to the memory object
std::pair<bool, size_t> GetMemoryReference(const Program& program, const Operation& op)
{
    std::pair<bool, size_t> result(false, 0);

    for (const SourceOperand& srcOp : op._src)
    {
        if (SourceOperandType::Register == srcOp.Type())
        {
            const size_t index = srcOp.GetAccessedRegister()._registerIndex;

            if (RegisterType::Memory == program._registerTable[index]._type)
            {
                assert(!result.first);

                result.first = true;
                result.second = index;
            }
        }
    }

    for (const DestinationOperand& dstOp : op._dst)
    {
        if (DestinationOperandType::Register == dstOp.Type())
        {
            const size_t index = dstOp.GetAccessedRegister()._registerIndex;

            if (RegisterType::Memory == program._registerTable[index]._type)
            {
                assert(!result.first);

                result.first = true;
                result.second = index;
            }
        }
    }

    return result;
}

// Detetects when src writes to a register and dst reads from the same register
bool OperationsHaveRAWHazard(const Operation& src, const Operation& dst)
{
    bool result = false;

    for (const DestinationOperand& dstOp : src._dst)
    {
        if (DestinationOperandType::Register == dstOp.Type())
        {
            const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

            for (const SourceOperand& srcOp : dst._src)
            {
                if (SourceOperandType::Register == srcOp.Type())
                {
                    if (registerIndex == srcOp.GetAccessedRegister()._registerIndex)
                    {
                        result = true;
                    }
                }
            }
        }
    }

    return result;
}

// Returns a data structure that represents the data dependencies between with a list of operations
OperationListDependencyGraph ComputeOperationListDependencyGraph(const OperationList& operations,
                                                                 const Program& program)
{
    OperationListDependencyGraph result = {};

    std::vector<size_t> readRegisters;
    std::vector<size_t> writeRegisters;

    // Some opcodes (like print) require that 2 operations with the same opcode cannot be reordered
    // This map tracks the most recently scheduled operation for a given opcode type
    std::map<Opcode, const Operation*> lastOperationByType;

    // For each register, tracks the identity of the operation that last wrote to it
    std::map<size_t, const Operation*> previousWriter;

    // For each egister, tracks the identity of the operation that last read from it
    std::map<size_t, std::set<const Operation*>> previousReader;

    // Maps atomic chain index BeginAtomic operation representing the start of the chain
    std::map<size_t, const Operation*> atomicChainMap;

    // Give each atomic block a unique ID, and remember which operations are
    // contained within atomic blocks.
    size_t atomicBlockCount = 0;
    std::list<size_t> atomicBlockCountList;

    for (const Operation& op : operations)
    {
        const Operation* const operationPointer = &op;

        if (Opcode::EndAtomic == op._opcode)
        {
            assert(!atomicBlockCountList.empty());

            SafeInsert(result.atomicBlockIndexToEndAtomicOp, atomicBlockCountList.back(), operationPointer);

            atomicBlockCountList.pop_back();
        }

        if (!atomicBlockCountList.empty())
        {
            // Save all atomic blocks that contain this op
            for (const size_t atomicBlockId : atomicBlockCountList)
            {
                result.operationToAtomicBlockIndex[operationPointer].push_back(atomicBlockId);
            }
        }

        if (Opcode::BeginAtomic == op._opcode)
        {
            // New atomic block ID
            SafeInsert(result.atomicBlockIndexToBeginAtomicOp, atomicBlockCount, operationPointer);
            SafeInsert(result.beginAtomicOpToAtomicBlockIndex, operationPointer, atomicBlockCount);
            atomicBlockCountList.push_back(atomicBlockCount);
            atomicBlockCount++;

            const size_t chainIndex = op._flags._atomicBlockDesc._chainIndex;

            const auto it = atomicChainMap.find(chainIndex);
            if (it == atomicChainMap.end())
            {
                atomicChainMap[chainIndex] = operationPointer;
            }
        }
    }

    assert(atomicBlockCountList.empty());

    for (const Operation& op : operations)
    {
        const Operation* const operationPointer = &op;

        GetAccessedRegisters(op, readRegisters, writeRegisters);

        if (Opcode::ReferenceString == op._opcode)
        {
            // Act as though ReferenceString writes to the string handle
            // to prevent a ReferenceString from being ordered backward past
            // operations which use the handle.  If that reordering could occur
            // then the string may be freed before use.
            assert(3 == op._src.size());
            assert(0 == op._dst.size());

            const SourceOperand& srcOp = op._src[1];

            if (SourceOperandType::Register == srcOp.Type())
            {
                writeRegisters.push_back(srcOp.GetAccessedRegister()._registerIndex);
            }
        }

        if (DoesOpcodeRequireTypeBarrier(op._opcode))
        {
            // Look up the most recently scheduled operation with the same opcode
            const auto it = lastOperationByType.find(op._opcode);
            if (it != lastOperationByType.end())
            {
                result.forwardOperationDependencies[it->second].insert(operationPointer);
            }

            lastOperationByType[op._opcode] = operationPointer;
        }

        // Handle RAW dependencies
        for (const size_t registerIndex : readRegisters)
        {
            const auto it = previousWriter.find(registerIndex);

            if (it != previousWriter.end())
            {
                // Record the dependency
                result.forwardOperationDependencies[it->second].insert(operationPointer);
            }
        }

        // Handle WAW dependencies
        for (const size_t registerIndex : writeRegisters)
        {
            const auto it = previousWriter.find(registerIndex);

            if (it != previousWriter.end())
            {
                // Record the dependency
                result.forwardOperationDependencies[it->second].insert(operationPointer);
            }
        }

        // Handle WAR dependencies
        for (const size_t registerIndex : writeRegisters)
        {
            const std::set<const Operation*>& previousReaders = previousReader[registerIndex];

            for (const Operation* op : previousReaders)
            {
                // Record the dependency
                result.forwardOperationDependencies[op].insert(operationPointer);
            }
        }

        // update previousReader table
        for (const size_t registerIndex : readRegisters)
        {
            previousReader[registerIndex].insert(operationPointer);
        }

        // update previousWriter table
        for (const size_t registerIndex : writeRegisters)
        {
            previousWriter[registerIndex] = operationPointer;

            previousReader[registerIndex].clear();
        }
    }

    // Add additional links for dependencies that go into or out of atomic blocks
    // This simplifies instruction scheduling by allowing the {Begin,End}Atomic operations
    // to act as placeholders for the whole block

    // New dependencies are added here to avoid modifying result.forwardOperationDependencies while iterating through it
    std::map<const Operation*, std::set<const Operation*>> newForwardOperationDependencies;

    for (const auto& p : result.forwardOperationDependencies)
    {
        const Operation* const src = p.first;

        const auto srcAtomicBlockIt = result.operationToAtomicBlockIndex.find(src);
        const bool srcInAtomicBlock = srcAtomicBlockIt != result.operationToAtomicBlockIndex.end();

        // Run one iteration if src is not in an atomic block
        size_t srcIterations = 1;
        if (srcInAtomicBlock)
        {
            srcIterations = srcAtomicBlockIt->second.size();
        }
        assert(srcIterations > 0);

        // For each src atomic block
        for (size_t srcIndex = 0; srcIndex < srcIterations; srcIndex++)
        {
            size_t srcAtomicBlockIndex;
            if (srcInAtomicBlock)
            {
                srcAtomicBlockIndex = srcAtomicBlockIt->second[srcIndex];
            }

            for (const Operation* const dst : p.second)
            {
                const auto dstAtomicBlockIt = result.operationToAtomicBlockIndex.find(dst);
                const bool dstInAtomicBlock = dstAtomicBlockIt != result.operationToAtomicBlockIndex.end();

                // Run one iteration if dst is not in an atomic block
                size_t dstIterations = 1;
                if (dstInAtomicBlock)
                {
                    dstIterations = dstAtomicBlockIt->second.size();
                }
                assert(dstIterations > 0);

                // Dst op is in src atomic block
                const bool dstInSrcAtomicBlock =
                    dstInAtomicBlock && std::find(dstAtomicBlockIt->second.begin(), dstAtomicBlockIt->second.end(),
                                                  srcAtomicBlockIndex) != dstAtomicBlockIt->second.end();

                // For each dst atomic block
                for (size_t dstIndex = 0; dstIndex < dstIterations; dstIndex++)
                {
                    size_t dstAtomicBlockIndex;
                    if (dstInAtomicBlock)
                    {
                        dstAtomicBlockIndex = dstAtomicBlockIt->second[dstIndex];
                    }

                    // Src op is in dst atomic block
                    const bool srcInDstAtomicBlock =
                        srcInAtomicBlock && std::find(srcAtomicBlockIt->second.begin(), srcAtomicBlockIt->second.end(),
                                                      dstAtomicBlockIndex) != srcAtomicBlockIt->second.end();

                    if (srcInAtomicBlock && (!dstInAtomicBlock || !dstInSrcAtomicBlock))
                    {
                        // The producer is in an atomic block, and the consumer is not in the same atomic block
                        // Add EndAtomic->Consumer
                        const Operation* const endOp =
                            SafeLookup(result.atomicBlockIndexToEndAtomicOp, srcAtomicBlockIndex);
                        assert(Opcode::EndAtomic == endOp->_opcode);

                        assert(endOp != dst);
                        newForwardOperationDependencies[endOp].insert(dst);

                        if (dstInAtomicBlock)
                        {
                            if (endOp->_flags._atomicBlockDesc._updateRate > 1)
                            {
                                // The producer atomic block has an update rate > 1
                                // Add EndAtomic->BeginAtomic dependency
                                // to ensure that the BeginAtomic is not scheduled too early
                                const Operation* const beginOp =
                                    SafeLookup(result.atomicBlockIndexToBeginAtomicOp, dstAtomicBlockIndex);
                                assert(Opcode::BeginAtomic == beginOp->_opcode);

                                // If src op is not in dst atomic block
                                // and src and dst are in different atomic chains
                                if (!srcInDstAtomicBlock && beginOp->_flags._atomicBlockDesc._chainIndex !=
                                                                endOp->_flags._atomicBlockDesc._chainIndex)
                                {
                                    newForwardOperationDependencies[endOp].insert(beginOp);
                                }
                            }
                        }
                    }

                    if (dstInAtomicBlock && (!srcInAtomicBlock || !srcInDstAtomicBlock))
                    {
                        // The consumer is in an atomic block, and the producer is not in the same atomic block
                        // Add Producer->BeginAtomic
                        const Operation* const beginOp =
                            SafeLookup(result.atomicBlockIndexToBeginAtomicOp, dstAtomicBlockIndex);
                        assert(Opcode::BeginAtomic == beginOp->_opcode);

                        assert(beginOp != src);
                        newForwardOperationDependencies[src].insert(beginOp);

                        // For atomic chains (read/modify/write) also add a dependency to the BeginAtomic operation
                        // associated with the start of the chain This ensures that the modify stage is not scheduled
                        // too early (if it has producers outside of the chain of atomics)
                        const size_t consumerChainIndex = beginOp->_flags._atomicBlockDesc._chainIndex;
                        const Operation* const chainStart = SafeLookup(atomicChainMap, consumerChainIndex);
                        assert(chainStart->_flags._atomicBlockDesc._chainIndex == consumerChainIndex);

                        bool addCrossChainDependency = true;

                        if (srcInAtomicBlock)
                        {
                            for (size_t srcAtomicBlockIndex : srcAtomicBlockIt->second)
                            {
                                const Operation* const producerChainBeginOp =
                                    SafeLookup(result.atomicBlockIndexToBeginAtomicOp, srcAtomicBlockIndex);

                                assert(Opcode::BeginAtomic == producerChainBeginOp->_opcode);

                                if (producerChainBeginOp->_flags._atomicBlockDesc._chainIndex == consumerChainIndex)
                                {
                                    // The producer and consumer are part of the same chain of atomic blocks, do not add
                                    // a dependency
                                    addCrossChainDependency = false;
                                }
                            }
                        }

                        if (addCrossChainDependency)
                        {
                            newForwardOperationDependencies[src].insert(chainStart);
                        }
                    }
                }
            }
        }
    }

    for (const auto& p : newForwardOperationDependencies)
    {
        std::set<const Operation*>& setToAddTo = result.forwardOperationDependencies[p.first];
        const std::set<const Operation*>& newSet = p.second;

        for (const Operation* const dst : newSet)
        {
            setToAddTo.insert(dst);
        }
    }

    // Construct backward dependencies
    for (const auto& p : result.forwardOperationDependencies)
    {
        const Operation* const src = p.first;

        for (const Operation* const dst : p.second)
        {
            result.backwardOperationDependencies[dst].insert(src);
        }
    }

    return result;
}

// Renames writes to local registers to convert the IR to "SSA for FPGAs" form
// The term "original registers" refers to the set of local registers in the IR before this function runs
// The term "renamed registers" refers to the set of local registers in the IR after this function runs
//
// After this function runs, each local register is written exactly once
// The equivalent of phi nodes are implemented with the help of tables stored with enqueue operations
// Optimizations that span basic blocks must map between renamed registers and original registers
// to enable information to cross basic blocks
//
// For example, before this function runs, the IR could be:
//
// Basic block 1:
// r0 = 3;
// r1 = 8;
// print(r0)
// r0 = 7;
// Enqueue (basic block 2)
//
// Basic block 2
// print(r0)
// r0 = 4;
// print(r0)
//
// After renaming the IR would be:
//
// Basic block 1:
// r2 = 3;
// r3 = 8;
// print(r2)
// r4 = 7;
// Enqueue (basic block 2) : renameTable = [r0->r4]
//
// Basic block 2
// print(r0)
// r5 = 4;
// print(r5)
void Rename(Program& program, BasicBlock& basicBlock)
{
    OperationList& operationList = basicBlock._operations;

    // Maps original register index to renamed register index
    RegisterIndexMap renameTable;

    // Walk forward through operations (including those in appended stages)
    const auto callback = [&](Operation& op)
    {
        if (Opcode::Enqueue == op._opcode)
        {
            // Enqueue operations need a copy of the renaming table
            // To enable cross-basic block liveness to be determined after renaming
            op._renamingTable = renameTable;

            assert(op._reverseRenamingTable.empty());

            // Compute the inverse of the renaming table
            for (const auto& p : renameTable)
            {
                SafeInsert(op._reverseRenamingTable, p.second, p.first);
            }
        }

        // Update source operands according to the renaming table
        for (SourceOperand& srcOp : op._src)
        {
            if (SourceOperandType::Register == srcOp.Type())
            {
                const size_t oldRegisterIndex = srcOp.GetAccessedRegister()._registerIndex;

                const auto it = renameTable.find(oldRegisterIndex);

                if (it != renameTable.end())
                {
                    const size_t newRegisterIndex = it->second;

                    srcOp.GetAccessedRegister()._registerIndex = newRegisterIndex;
                }
            }
        }

        for (size_t dstOpIdx = 0; dstOpIdx < op._dst.size(); dstOpIdx++)
        {
            DestinationOperand& destOp = op._dst[dstOpIdx];

            if (DestinationOperandType::Register == destOp.Type())
            {
                AccessedRegister& reg = destOp.GetAccessedRegister();

                const RegisterDescription& regDesc = program._registerTable[reg._registerIndex];

                // Check if this register type can be renamed
                if (CanRegisterTypeBeRenamed(regDesc._type))
                {
                    const size_t newRegisterIndex = AllocateRegister(&program, regDesc);

                    // Record the rename in the renaming table
                    renameTable[reg._registerIndex] = newRegisterIndex;

                    // Update the destination operand
                    reg._registerIndex = newRegisterIndex;
                }
                else
                {
                    // Renaming should be affected by register type & write count only
                    assert(renameTable.end() == renameTable.find(reg._registerIndex));
                }
            }
        }
    };

    ForEachOperationForward(basicBlock, callback);
}

// Used by InsertGlobalViews
// Given a local register index
// Find the function (of global variables only) that computes the value of that register
// store the function in a flat list of operations
void FlattenFunction(const size_t srcIdx, const std::map<size_t, const Operation*>& registerMap, OperationList& listOut)
{
    // Lookup the operation that wrote this register
    const Operation* const writeOp = SafeLookup(registerMap, srcIdx);

    // Recurse to all operations that wrote sources of writeOp
    for (const SourceOperand& srcOp : writeOp->_src)
    {
        if (SourceOperandType::Register == srcOp.Type())
        {
            const size_t srcRegIdx = srcOp.GetAccessedRegister()._registerIndex;

            if (registerMap.end() != registerMap.find(srcRegIdx))
            {
                FlattenFunction(srcRegIdx, registerMap, listOut);
            }
        }
    }

    listOut.push_back(*writeOp);
}

// Finds operations with latency > 0 which operands that are
// pure functions of global variables.
// Introduces new global views, which that contain the function result,
// computed at the point where the global variables are written.
// Called before scheduling (to make more schedules possible).
class InsertGlobalViewsHelper
{
  public:
    InsertGlobalViewsHelper(Program& program) : _program(program) {}

    void ComputeAddresses(BasicBlock& basicBlock)
    {
        const ForEachOperation_AtomicCallback callback =
            [&](Operation& op, OperationList::iterator it, const ForEachOperation_AtomicData& data)
        {
            if (Opcode::BeginAtomic == op._opcode)
            {
                assert(_precomputeMap.empty());

                // Memory loads can only occur in leaf atomic blocks. On a new
                // nested atomic block, discard previous information
                _registerMap.clear();
            }
            else if (Opcode::EndAtomic == op._opcode)
            {
                // Emit operations that write to new global variables
                EmitOperations(it, basicBlock);

                _registerMap.clear();
                _precomputeMap.clear();
            }
            else if (data._inAtomic)
            {
                UpdateState(op);
            }
        };

        ForEachOperation_Atomic(basicBlock._operations, callback);

        assert(_registerMap.empty());
        assert(_precomputeMap.empty());
    }

  private:
    void EmitOperations(OperationList::iterator it, BasicBlock& basicBlock)
    {
        for (const auto& p : _precomputeMap)
        {
            const size_t localRegIdx = p.first;
            const size_t globalRegIdx = p.second;

            RegisterDescription& globalRegDesc = _program._registerTable[globalRegIdx];
            assert(globalRegDesc._type == RegisterType::GlobalView);

            globalRegDesc.GlobalView()._globalViewFunctionIndex = _program._globalViewFunctions.size();
            OperationList& globalViewFunction =
                _program._globalViewFunctions[globalRegDesc.GlobalView()._globalViewFunctionIndex];

            // Find the list of operations that define
            // the function that computes the address
            FlattenFunction(localRegIdx, _registerMap, globalViewFunction);
        }
    }

    // Called for each operation in an atomic block
    void UpdateState(Operation& op)
    {
        // If an operation that computes a load address has side effects
        // then this optimization cannot apply
        bool isFunctionOfOnlyGlobals = !IsLiveOperation(op);

        for (const SourceOperand& srcOp : op._src)
        {
            if (SourceOperandType::Register == srcOp.Type())
            {
                const size_t srcRegIdx = srcOp.GetAccessedRegister()._registerIndex;

                const RegisterDescription srcRegDesc = _program._registerTable[srcRegIdx];

                if (RegisterType::Global == srcRegDesc._type)
                {
                    // The output of this operation is function of globals, because the input is a global
                }
                else if (RegisterType::Local == srcRegDesc._type)
                {
                    // Check if the source register itself is a function of globals
                    const auto it = _registerMap.find(srcRegIdx);
                    if (it == _registerMap.end())
                    {
                        isFunctionOfOnlyGlobals = false;
                    }
                }
                else
                {
                    // Other source register types disqualify the operation
                    isFunctionOfOnlyGlobals = false;
                }
            }
        }

        if (isFunctionOfOnlyGlobals)
        {
            // Record the destination registers written by this operation
            for (const DestinationOperand& dstOp : op._dst)
            {
                const size_t dstRegIdx = dstOp.GetAccessedRegister()._registerIndex;

                // For fixed-latency external calls, the register type can be wire
                // Disallow this transformation
                if (RegisterType::Local == _program._registerTable[dstRegIdx]._type)
                {
                    SafeInsert(_registerMap, dstRegIdx, static_cast<const Operation*>(&op));
                }
            }
        }

        // Only modify operations which have registered input
        // The purpose of this transformation is to allow the operation with registered input
        // to be scheduled at the very begining of a pipeline stage.
        if (OperationHasRegisteredInput(op))
        {
            // For each operand
            for (size_t i = 0; i < op._src.size(); i++)
            {
                SourceOperand& srcOp = op._src[i];

                if (SourceOperandType::Register == srcOp.Type())
                {
                    const size_t srcRegIdx = srcOp.GetAccessedRegister()._registerIndex;

                    const RegisterDescription srcRegDesc = _program._registerTable[srcRegIdx];

                    // Check if the source register was written in this atomic block
                    // and is a function of global variables only
                    const auto it = _registerMap.find(srcRegIdx);

                    if ((RegisterType::Local == srcRegDesc._type) && (it != _registerMap.end()))
                    {
                        // This source operand is written in the current atomic block
                        // And is a pure function of global variables
                        AccessedRegister globalViewReg = {c_invalidAccessedRegisterIndex};

                        // See if a global view  has already been created for this register
                        const auto it = _precomputeMap.find(srcRegIdx);
                        if (it == _precomputeMap.end())
                        {
                            // Create a new global view register (which contains the function result)
                            globalViewReg._registerIndex = AllocateRegister(&_program, srcRegDesc._width,
                                                                            RegisterType::GlobalView, srcRegDesc._name);

                            _precomputeMap[srcRegIdx] = globalViewReg._registerIndex;
                        }
                        else
                        {
                            globalViewReg._registerIndex = it->second;
                        }

                        srcOp = SourceOperand(globalViewReg);
                    }
                }
            }
        }
    }

    Program& _program;

    // Maps register index, to the of operation that computes it (in the current atomic block only)
    std::map<size_t, const Operation*> _registerMap;

    // Maps local register index (LoadMemory source operand) to global view that
    // will replace that local register index
    std::map<size_t, size_t> _precomputeMap;
};

void InsertGlobalViews(Program& program)
{
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            InsertGlobalViewsHelper helper(program);
            helper.ComputeAddresses(basicBlock);
        }
    }
}

// Maps register index to # of levels of logic before the use of that register
using RegisterDistanceMap = std::map<size_t, size_t>;

struct DefinitionMapRecord
{
    // Basic block that provides a definition for a register
    BasicBlock* _basicBlock;

    // Renamed register index in the definining basic block
    size_t _renamedRegisterIndex;

    // Number of levels of logic between the definition and the first use in the consuming basic block
    // This includes all logic in basic blocks between the definition and the use
    size_t _defToUseDistance;

    bool operator<(const DefinitionMapRecord& rhs) const
    {
        if (_basicBlock < rhs._basicBlock)
        {
            return true;
        }
        else if (_basicBlock > rhs._basicBlock)
        {
            return false;
        }
        else
        {
            return _renamedRegisterIndex < rhs._renamedRegisterIndex;
        }
    }
};

// Maps local register index to the set of basic blocks that provide a definition for that register
using DefinitionMap = std::map<size_t, std::set<DefinitionMapRecord>>;

// Maps basic block to DefinitionMap
using BasicBlockToDefinitionMap = std::map<BasicBlock*, DefinitionMap>;

struct GetDefinitionsContext
{
    RegisterDistanceMap _undefinedRegisters;
    std::set<BasicBlock*> _visitedBasicBlocks;
    std::set<std::pair<BasicBlock*, BasicBlock*>> _visitedBackLinks;
};

void GetDefinitionsRecursive(Program& program, ControlFlowGraph& controlFlowGraph, BasicBlock& basicBlock,
                             const bool searchBlockContents, GetDefinitionsContext context,
                             DefinitionMap& definitionMap, const BasicBlock& useBasicBlock,
                             const RegisterDistanceMap& origUndefinedRegisters)
{
    // Distance from the first use to the end of the basic block
    PathLengthTracker pathLengthTracker(program);

    RegisterIndexMap renamedToOriginalTable;

    if (searchBlockContents)
    {
        // See which definitions are satisfied by this basic block
        const auto callback = [&](const Operation& op)
        {
            std::vector<size_t> registersRead;
            std::vector<size_t> registersWritten;

            GetAccessedRegisters(op, registersRead, registersWritten);

            // Update path length information
            // Note that the usage of the read and write set is backwards from normal usage
            // because this is traversing the basic block backwards
            const size_t lengthToOp = pathLengthTracker.UpdateBackward(op);

            for (const size_t r : registersWritten)
            {
                // The written register index is a renamed index
                // Re-map to an unrenamed index (which is the type of register index stored in undefinedRegisters)
                const size_t mappedR = OptionallyMapIndex(r, renamedToOriginalTable);

                const auto it = context._undefinedRegisters.find(mappedR);

                if (context._undefinedRegisters.end() != it)
                {
                    // Path length from the use to the end of this basic block
                    const size_t distanceToThisBbEnd = it->second;

                    // Found a definition for this register.  Do not continue searching for definitions of this register
                    context._undefinedRegisters.erase(it);

                    DefinitionMapRecord dmr = {&basicBlock, r, distanceToThisBbEnd + lengthToOp};

                    // If a record already exists, save the longer path
                    std::set<DefinitionMapRecord>& dmrSet = definitionMap[mappedR];
                    std::set<DefinitionMapRecord>::iterator mapIt = dmrSet.find(dmr);
                    if (mapIt != dmrSet.end())
                    {
                        if (dmr._defToUseDistance > mapIt->_defToUseDistance)
                        {
                            dmrSet.erase(mapIt);
                            dmrSet.insert(dmr);
                        }
                    }
                    else
                    {
                        dmrSet.insert(dmr);
                    }
                }
            }

            if (op._getSuccessorBlock && op._getSuccessorBlock()->_function == basicBlock._function)
            {
                // Enqueuing into another basic block in this same function
                // Update the table that maps renamed register index to original register index
                for (const auto& p : op._reverseRenamingTable)
                {
                    renamedToOriginalTable[p.first] = p.second;
                }
            }
        };

        ForEachOperationReverse(basicBlock, callback);

        // For registers that are still in the set of undefined registers, add
        // the total basic block path length their current path length. This
        // should be skipped if the basic block has already been visited.
        if (context._visitedBasicBlocks.end() == context._visitedBasicBlocks.find(&basicBlock))
        {
            // If original register usage was in this basic block, then replace
            // original distance from basic block start to usage with the total
            // basic block depth
            if (&basicBlock == &useBasicBlock)
            {
                for (auto& p : context._undefinedRegisters)
                {
                    assert(p.second >= origUndefinedRegisters.at(p.first));
                    p.second -= origUndefinedRegisters.at(p.first);
                }
            }

            const size_t bbTotalLength = pathLengthTracker.GetTotalPathLength();

            for (auto& p : context._undefinedRegisters)
            {
                p.second += bbTotalLength;
            }

            // Track visited basic blocks so that their path length is not double counted
            context._visitedBasicBlocks.insert(&basicBlock);
        }

        const Function* function = basicBlock._function;

        if (&basicBlock == function->_start)
        {
            // Entry point to a function
            // All function parameters can be supplied (even though there is no instruction in the IR yet that writes
            // parameters to registers local to this bb)
            for (const size_t r : basicBlock._liveInReg)
            {
                const auto it = context._undefinedRegisters.find(r);

                if (context._undefinedRegisters.end() != it)
                {
                    // Path length from the use to the end of this basic block
                    const size_t distanceToThisBbEnd = it->second;

                    context._undefinedRegisters.erase(it);

                    DefinitionMapRecord dmr = {&basicBlock, r, distanceToThisBbEnd};

                    std::set<DefinitionMapRecord>& dmrSet = definitionMap[r];
                    auto mapIt = dmrSet.find(dmr);
                    if (mapIt != dmrSet.end())
                    {
                        // If a record already exists, save the longer path
                        if (dmr._defToUseDistance > mapIt->_defToUseDistance)
                        {
                            dmrSet.erase(mapIt);
                            dmrSet.insert(dmr);
                        }
                    }
                    else
                    {
                        dmrSet.insert(dmr);
                    }
                }
            }

            // No undefined registers should leak out of a function
            assert(context._undefinedRegisters.empty());
        }
    }

    if (!context._undefinedRegisters.empty())
    {
        // There are more undefined registers, continue searching backwards
        const std::set<BasicBlock*> predecessors = controlFlowGraph.GetPredecessors(&basicBlock);

        std::unordered_set<BasicBlock*> predecessorsToVisit;

        for (BasicBlock* const predecessor : predecessors)
        {
            std::pair<BasicBlock*, BasicBlock*> link = {predecessor, &basicBlock};
            const auto it = context._visitedBackLinks.find(link);
            bool backLink = predecessor->_indexWithinFunction >= basicBlock._indexWithinFunction;

            // Only traverse backwards links once to prevent infinite loops
            if (!backLink || it == context._visitedBackLinks.end())
            {
                // Add to list of visited back links
                if (backLink)
                {
                    assert(it == context._visitedBackLinks.end());
                    context._visitedBackLinks.insert(link);
                }

                // context is passed by value
                // This ensures that visited information is per-traversal and
                // for undefinedRegisters this ensures that multiple definitions
                // are supported.
                GetDefinitionsRecursive(program, controlFlowGraph, *predecessor, true, context, definitionMap,
                                        useBasicBlock, origUndefinedRegisters);
            }
        }
    }
}

DefinitionMap GetDefinitions(Program& program, ControlFlowGraph& controlFlowGraph, BasicBlock& basicBlock,
                             const RegisterDistanceMap& undefinedRegisters)
{
    GetDefinitionsContext context = {};
    context._undefinedRegisters = undefinedRegisters;
    DefinitionMap definitionMap = {};

    GetDefinitionsRecursive(program, controlFlowGraph, basicBlock,
                            false, // don't search for definitions within the starting basic block
                            context, definitionMap, basicBlock, undefinedRegisters);

    return definitionMap;
}

// Adds an operation to an operation list to update a history register to a given value
// Renaming is used to ensure that the basic block stays in SSA form
// The history register write is inserted immediately before the supplied iterator
void InsertDataPropagationHistoryUpdate(Program& program, BasicBlock& basicBlock, OperationList::iterator pos,
                                        const AccessedRegister originalRegister, const size_t value)
{
    // Allocate a new register to hold the result of the write
    const RegisterDescription originalDesc = program._registerTable[originalRegister._registerIndex];

    const AccessedRegister renamedReg = {
        AllocateRegister(&program, originalDesc._width, RegisterType::Local, originalDesc._name)};

    {
        Operation op = {};

        // There is no reasonable source location for this operation
        op._expectNoSourceLocation = true;

        op._opcode = Opcode::Mov;

        op._src.push_back(SourceOperand(value));
        op._dst.push_back(renamedReg);

        basicBlock._operations.insert(pos, op);
    }

    // Update renaming tables
    auto UpdateRenamingTable = [&](Operation& op)
    {
        if (Opcode::Enqueue == op._opcode)
        {
            SafeInsert(op._renamingTable, originalRegister._registerIndex, renamedReg._registerIndex);
            SafeInsert(op._reverseRenamingTable, renamedReg._registerIndex, originalRegister._registerIndex);
        }
    };

    for (OperationList::iterator it = pos; it != basicBlock._operations.end(); ++it)
    {
        Operation& op = *it;

        UpdateRenamingTable(op);
    }

    for (Stage& stage : basicBlock._stages)
    {
        for (Operation& op : stage._operations)
        {
            UpdateRenamingTable(op);
        }
    }
}

// Allocates a memory, and inserts a store operation to write into the memory
// Returns the AccessedRegister for the memory
AccessedRegister InsertDataPropagationMemoryAndStore(Program& program, std::map<BasicBlock*, size_t>& bbDataPropMap,
                                                     const size_t localThreadIdRegisterIndex,
                                                     const size_t originalRegisterIndex,
                                                     const size_t renamedRegisterIndex, BasicBlock& basicBlock)
{
    const AccessedRegister localThreadIdRegister = {localThreadIdRegisterIndex};

    const size_t threadIdWidth = program._registerTable[localThreadIdRegister._registerIndex]._width;

    const size_t elementCount = 1ull << threadIdWidth;

    const RegisterDescription rd = program._registerTable[renamedRegisterIndex];

    // make sure the memory container has a valid instance path
    const ObjectPath basicBlockPath = basicBlock.GetObjectPath();
    const std::string suffix = "_" + std::to_string(bbDataPropMap[&basicBlock]++);
    const ObjectPath objectPath = AppendToPath(basicBlockPath, suffix);
    // Create a memory to hold the value of the variable written by this basic block
    const AccessedRegister memoryReg = {
        AllocateRegister(&program, rd._width * elementCount, RegisterType::Memory, rd._name, objectPath)};

    RegisterDescription& memoryDesc = program._registerTable[memoryReg._registerIndex];

    memoryDesc._function = basicBlock._function;

    memoryDesc.Memory()._elementWidth = rd._width;
    memoryDesc.Memory()._elementCount = elementCount;
    memoryDesc.Memory()._readPortCount = 0;
    memoryDesc.Memory()._writePortCount = 0;
    memoryDesc.Memory().ClearBypassMask();
    memoryDesc.Memory()._replicate = true; // there will only be 1 read, so this property doesn't matter
    memoryDesc._name =
        "__data_prop_" +
        CombineObjectAndMemberName(basicBlock._function->_scope, basicBlock._function->_objectName,
                                   basicBlock._function->_name + "_" + std::to_string(basicBlock._indexWithinFunction) +
                                       "_" + memoryDesc._name + suffix);

    // Emit an operation in the defining basic block that will write to the memory
    std::vector<size_t>* sourceWidths = g_compiler->Create<std::vector<size_t>>();
    sourceWidths->push_back(rd._width);

    Operation op = {};

    // There is no reasonable source location to associate with this operation
    op._expectNoSourceLocation = true;

    op._opcode = Opcode::StoreMemory;

    op._flags._storeMemory._isPredicated = false;
    op._flags._storeMemory._sourceWidths = sourceWidths;

    // Index = thread ID
    op._src.push_back(localThreadIdRegister);

    // Add the data
    const AccessedRegister srcReg = {renamedRegisterIndex};
    op._src.push_back(srcReg);

    // Reference the actual memory
    op._dst.push_back(memoryReg);

    std::vector<size_t> registersRead;
    std::vector<size_t> registersWritten;

    OperationList::iterator insertLocation = basicBlock._operations.end();
    bool foundTarget = false;

    if (&basicBlock == basicBlock._function->_start)
    {
        if (basicBlock._liveInReg.end() !=
            std::find(basicBlock._liveInReg.begin(), basicBlock._liveInReg.end(), renamedRegisterIndex))
        {
            // The register is a parameter to the function
            // Allow the store to be placed at the begining of the function
            insertLocation = basicBlock._operations.begin();
            foundTarget = true;
        }
    }

    // Search in start condition operations
    for (const Operation& op : basicBlock._startConditionOperations)
    {
        GetAccessedRegisters(op, registersRead, registersWritten);

        for (const size_t r : registersWritten)
        {
            if (r == renamedRegisterIndex)
            {
                foundTarget = true;
                insertLocation = basicBlock._operations.begin();
            }
        }
    };

    // Find the operation that writes to the register
    const auto callback = [&](Operation& op, OperationList::iterator it, const ForEachOperation_AtomicData& data)
    {
        GetAccessedRegisters(op, registersRead, registersWritten);

        bool insertHere = false;

        for (const size_t r : registersRead)
        {
            if (r == originalRegisterIndex)
            {
                // The original register index is read by this operation
                // The memory store must occur after this read
                insertHere = true;
            }
        }

        for (const size_t r : registersWritten)
        {
            if (r == renamedRegisterIndex)
            {
                // the store must appear after the definition
                insertHere = true;
            }
        }

        if (insertHere)
        {
            if (data._inAtomic)
            {
                // The insert location is in an atomic block, insert the store at the end of the basic block
                // to avoid introducing scheduling constraints
                insertLocation = basicBlock._operations.end();
            }
            else
            {
                insertLocation = it;
            }

            foundTarget = true;
        }
    };

    ForEachOperation_Atomic(basicBlock._operations, callback);

    assert(foundTarget);

    if (insertLocation != basicBlock._operations.end())
    {
        // The store should follow the operation that defines the variable
        ++insertLocation;
    }

    basicBlock._operations.insert(insertLocation, op);

    return memoryReg;
}

// Inserts load/select operations into the operation list for a basic block
// The operations are inserted right before the first use
// If the first use is in an atomic block, then operations are inserted before that
void InsertDataPropagationLoadOps(Program& program, ControlFlowGraph& controlFlowGraph, BasicBlock& basicBlock,
                                  const size_t registerIndex, const std::vector<AccessedRegister>& memoryRegisters,
                                  const size_t historyWidth, const size_t historyRegisterIndex,
                                  const size_t localThreadIdRegisterIndex)
{
    const AccessedRegister originalReg = {registerIndex};

    const AccessedRegister localThreadIdRegister = {localThreadIdRegisterIndex};

    const bool multipleDefinitions = (memoryRegisters.size() > 1);

    std::vector<AccessedRegister> loadRegisters(memoryRegisters.size());

    const RegisterDescription rd = program._registerTable[registerIndex];

    OperationList loadOps;

    // Insert the operation list immediately before this iterator
    OperationList::iterator insertLocation = basicBlock._operations.begin();

    {
        SetOperationLocation sol(loadOps, basicBlock._location);

        for (size_t i = 0; i < memoryRegisters.size(); i++)
        {
            if (multipleDefinitions)
            {
                // Allocate a register to hold the results of the load
                std::ostringstream nameStr;
                nameStr << rd._name << "_load";
                loadRegisters[i]._registerIndex =
                    AllocateRegister(&program, rd._width, RegisterType::Local, nameStr.str());
            }

            // Instruction scheduling assumes the output of a memory load will be placed into a
            // local register before use
            std::ostringstream tempName;
            tempName << rd._name << "_load";

            AccessedRegister tempReg = {AllocateRegister(&program, rd._width, RegisterType::Local, tempName.str())};

            // Load
            {
                Operation op = {};

                op._opcode = Opcode::LoadMemory;

                std::vector<size_t>* sourceOffsets = g_compiler->Create<std::vector<size_t>>();
                sourceOffsets->push_back(0);

                op._flags._loadMemory._sourceOffsets = sourceOffsets;

                op._flags._loadMemory._readLatency = c_defaultMemoryReadLatency;

                op._flags._loadMemory._isPredicated = false;

                // Add the memory object
                op._src.push_back(memoryRegisters[i]);

                // Add the index
                op._src.push_back(localThreadIdRegister);

                op._dst.push_back(tempReg);

                loadOps.push_back(op);
            }

            {
                Operation op = {};

                op._opcode = Opcode::Mov;
                op._src.push_back(tempReg);
                op._dst.push_back(multipleDefinitions ? loadRegisters[i] : originalReg);

                loadOps.push_back(op);
            }
        }

        if (multipleDefinitions)
        {
            // Select 1 load result
            Operation op = {};

            op._opcode = Opcode::Select;

            const AccessedRegister historyRegister = {historyRegisterIndex};

            op._src.push_back(historyRegister);

            for (size_t i = 0; i < loadRegisters.size(); i++)
            {
                op._src.push_back(loadRegisters[i]);
            }

            // pad with zeros to the next power of 2
            const size_t srcOperandCount = 1ull << historyWidth;

            for (size_t i = loadRegisters.size(); i < srcOperandCount; i++)
            {
                op._src.push_back(SourceOperand(0));
            }

            op._dst.push_back(originalReg);

            loadOps.push_back(op);
        }

        bool foundTarget = false;

        std::vector<size_t> registersRead;
        std::vector<size_t> registersWritten;

        const auto operationReadsRegister = [&](const Operation& op)
        {
            bool result = false;

            GetAccessedRegisters(op, registersRead, registersWritten);

            for (const size_t r : registersRead)
            {
                if (r == registerIndex)
                {
                    result = true;
                }
            }

            return result;
        };

        const auto callback = [&](Operation& op, OperationList::iterator it, const ForEachOperation_AtomicData& data)
        {
            // There can be multiple reads, hence the !foundTarget check
            if (!foundTarget && operationReadsRegister(op))
            {
                if (data._inAtomic)
                {
                    // Don't add scheduling constraints, move insert the load operations before the atomic chain
                    insertLocation = data._currAtomicChainStart;
                }
                else
                {
                    insertLocation = it;
                }

                foundTarget = true;
            }
        };

        // Inserts a set of operations into the operation list for basicBlock, immediately before the first used of the
        // register index If the first use is in an atomic block, then the operations are inserted before that atomic
        // chain
        ForEachOperation_Atomic(basicBlock._operations, callback);

        if (!foundTarget)
        {
            // Search appended stages
            for (const Stage& stage : basicBlock._stages)
            {
                for (const Operation& op : stage._operations)
                {
                    if (operationReadsRegister(op))
                    {
                        // Insert the loads right before the appended stages
                        insertLocation = basicBlock._operations.end();
                        foundTarget = true;
                    }
                }
            }
        }

        assert(foundTarget);
    } // operation locations set here

    basicBlock._operations.splice(insertLocation, loadOps);
}

// Adds operations to the start of a function
// to allocate a unique ID to each thread
void InitializeDataPropagationThreadId(IRContext& context, Function& function, const size_t threadIdWidth,
                                       const AccessedRegister sharedThreadIdRegister,
                                       const AccessedRegister localThreadIdRegister)
{
    Program& program = *context._program;

    {
        RegisterDescription& rd = program._registerTable[sharedThreadIdRegister._registerIndex];

        // Assign an initial value to the shared thread ID
        // To ensure a 'x address is not used during simulation
        assert(!rd.Global()._hasInitialValue);
        assert(!rd.Global()._isConstant);

        rd.Global()._hasInitialValue = true;
        rd.Global()._initialValue = 0;
    }

    const AccessedRegister tempThreadId = {AllocateRegister(&program, threadIdWidth, RegisterType::Local, "ThreadId")};

    // Add operations to the entry point of the function to
    // increment and snap the global ID - note that wrapping is implemented for free because the max thread count is a
    // power of 2 It is important that these operations are added here, to ensure the thread id is available before any
    // stores
    OperationList ops;

    {
        // Assign a location to each operation
        SetOperationLocation sol(ops, function._functionNode->GetLocation());

        AtomicBlockDesc atb = {};
        atb._type = AtomicBlockType::Default;
        atb._updateRate = c_defaultAtomicBlockUpdateRate;
        atb._chainIndex = context.AllocateAtomicChainIndex();
        atb._location = function._functionNode->GetLocation();

        {
            Operation op = {};

            op._opcode = Opcode::BeginAtomic;
            op._flags._atomicBlockDesc = atb;

            ops.push_back(op);
        }

        {
            Operation op = {};

            op._opcode = Opcode::Mov;

            op._src.push_back(sharedThreadIdRegister);
            op._dst.push_back(tempThreadId);

            ops.push_back(op);
        }

        {
            Operation op = {};

            op._opcode = Opcode::BinaryOp;
            op._flags._binaryOpType = ParseTreeBinaryOpTypeAdd;

            op._src.push_back(tempThreadId);
            op._src.push_back(SourceOperand(1));
            op._dst.push_back(localThreadIdRegister);

            ops.push_back(op);
        }

        {
            Operation op = {};

            op._opcode = Opcode::WriteGlobal;

            op._flags._writeGlobal._isPredicated = false;

            op._src.push_back(localThreadIdRegister);
            op._dst.push_back(sharedThreadIdRegister);

            ops.push_back(op);
        }

        {
            Operation op = {};

            op._opcode = Opcode::EndAtomic;
            op._flags._atomicBlockDesc = atb;

            ops.push_back(op);
        }
    }

    function._start->_operations.splice(function._start->_operations.begin(), ops);
}

// For a given function, finds local variables that are used and defined in separate basic blocks
BasicBlockToDefinitionMap FindGlobalDataPropagationCandidates(IRContext& context, Function& function)
{
    Program& program = *context._program;

    ControlFlowGraph controlFlowGraph(function, program, OperationEnumerationMode::Unscheduled,
                                      ControlFlowGraphPhase::PrePipeline);

    // Maps a basic block that has live-in variables
    // to a map that contains information about all definitions
    BasicBlockToDefinitionMap definitionMaps;

    // Find def->use information
    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        // Distance from the first use to the end of the basic block
        PathLengthTracker pathLengthTracker(program);

        // Find the set of registers that are used before any definition in this basic block
        RegisterDistanceMap undefinedRegisters;

        const auto callback = [&](const Operation& op)
        {
            std::vector<size_t> registersRead;
            std::vector<size_t> registersWritten;

            GetAccessedRegisters(op, registersRead, registersWritten);

            // Update path length information
            // Note that the usage of the read and write set is backwards from normal usage
            // because this is traversing the basic block backwards
            const size_t pathLengthToOp = pathLengthTracker.UpdateBackward(op);

            // Each register defined by this operation satisfies all subsequent uses in the basic block
            for (const size_t r : registersWritten)
            {
                undefinedRegisters.erase(r);
            }

            // Each register read by this operation adds a new unsatisfied use
            for (const size_t r : registersRead)
            {
                if (RegisterType::Local == program._registerTable[r]._type)
                {
                    undefinedRegisters[r] = pathLengthToOp;
                }
            }
        };

        ForEachOperationReverse(basicBlock, callback);

        // Don't look for definitions of variables read in start condition
        // Because there will be no good place to insert a load
        for (const Operation& op : basicBlock._startConditionOperations)
        {
            std::vector<size_t> registersRead;
            std::vector<size_t> registersWritten;

            GetAccessedRegisters(op, registersRead, registersWritten);

            for (const size_t r : registersRead)
            {
                undefinedRegisters.erase(r);
            }
        }

        // Don't look for definitions of function return values
        // They are provided by the return mechanism
        for (const size_t r : basicBlock._returnValueReg)
        {
            undefinedRegisters.erase(r);
        }

        // All variables that are live in to the start of a function
        // are the function parameters.  This optimization only applies within 1 function, so do not consider these
        // variables to be live-in
        if (&basicBlock == function._start)
        {
            undefinedRegisters.clear();
        }

        const size_t bbTotalLength = pathLengthTracker.GetTotalPathLength();

        // undefinedRegisters currently holds distance from the first use to the end of the bb
        // change it to hold the distance from start of the bb to the first use
        for (auto& p : undefinedRegisters)
        {
            assert(p.second <= bbTotalLength);
            p.second = bbTotalLength - p.second;
        }

        // Traverse the control flow graph backwards, finding the definitions that could satisfy the uses for
        // variables that are live-in to this basic block
        definitionMaps[&basicBlock] = GetDefinitions(program, controlFlowGraph, basicBlock, undefinedRegisters);

        const DefinitionMap& dm = definitionMaps[&basicBlock];
    }

    return definitionMaps;
}

// Called before scheduling, but after optimization.  Finds def-use pairs (local variables) in separate basic blocks
// Stores the data into a memory after the def, and loads the data from memory before the use
void GlobalDataPropagation(IRContext& context, Function& function)
{
    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();
    std::map<BasicBlock*, size_t> bbDataPropMap;

    // Unordered functions do not support global data propagation
    // because the thread ID allocation mechanism assumes ordered threads
    if ((codeGenConfig._optimize == 0) || function.IsUnordered())
    {
        return;
    }

    Program& program = *context._program;

    // Determine many bits are needed to track thread IDs in the function
    // Power of 2 max thread count is required to get free wrapping of the shared counter
    assert(IsPow2(function._maxThreadCountInsideFunction));
    const size_t threadIdWidth = std::max<size_t>(Log2(function._maxThreadCountInsideFunction), 1);
    assert(threadIdWidth > 0);
    const size_t memoryDepth = 1ull << threadIdWidth;

    ControlFlowGraph controlFlowGraph(function, program, OperationEnumerationMode::Unscheduled,
                                      ControlFlowGraphPhase::PrePipeline);

    // Maps a basic block that has live-in variables
    // to a map that contains information about all definitions
    const BasicBlockToDefinitionMap definitionMaps = FindGlobalDataPropagationCandidates(context, function);

    AccessedRegister sharedThreadIdRegister = {c_invalidAccessedRegisterIndex};
    AccessedRegister localThreadIdRegister = {c_invalidAccessedRegisterIndex};
    bool initializeThreadIds = false;

    std::set<size_t> historyRegistersToInitialize;

    // For each basic block that consumes live-in variables
    for (const auto& bbAndDm : definitionMaps)
    {
        BasicBlock& basicBlock = *(bbAndDm.first);
        const DefinitionMap& dm = bbAndDm.second;

        // For each live-in variable
        for (const auto& regAndDefs : dm)
        {
            const size_t r = regAndDefs.first;
            const std::set<DefinitionMapRecord>& defBasicBlocks = regAndDefs.second;

            // Determine if global data propgation should be applied
            size_t memoryBits = 0;
            size_t registerBits = 0;

            const RegisterDescription rd = program._registerTable[r];

            for (const DefinitionMapRecord& dmr : defBasicBlocks)
            {
                // Applying this optimization will remove pipeline registers
                // The number of registers is proportional to the number of levels of logic
                // between the def and the use
                // Divide by the register ratio to convert from levels of logic to # of registers
                registerBits += (dmr._defToUseDistance / codeGenConfig._logicRegisterRatio);

                // Each definition requires a separate memory to hold the data
                memoryBits += memoryDepth;
            }

            // BRAMs have fixed alignments
            const size_t alignedWidth =
                AlignNonPow2(rd._width, GetCodeGenDeviceConfig()._globalDataPropgationRamAlignment);

            registerBits *= rd._width;
            memoryBits *= alignedWidth;

            if ((static_cast<float>(registerBits) * codeGenConfig._memToRegThreshold) <= static_cast<float>(memoryBits))
            {
                continue;
            }

            // 'r' is live-in to this basic block
            if (!initializeThreadIds)
            {
                // Create shared thread ID variable
                // Allocate a local register that tracks the thread ID (for indexing into data propagation memories)
                // Note that there is no initial value for this register, any initial value will do
                const ObjectPath basicBlockPath = basicBlock.GetObjectPath();
                const std::string sharedThreadIdName =
                    context.GenerateUniqueNameWithinPath(basicBlockPath, "SharedThreadId");
                const ObjectPath sharedThreadIdPath = AppendToPath(basicBlockPath, sharedThreadIdName);
                sharedThreadIdRegister._registerIndex = AllocateRegister(&program, threadIdWidth, RegisterType::Global,
                                                                         sharedThreadIdName, sharedThreadIdPath);
                localThreadIdRegister._registerIndex =
                    AllocateRegister(&program, threadIdWidth, RegisterType::Local, "ThreadId");

                initializeThreadIds = true;
            }

            AccessedRegister historyRegister = {c_invalidAccessedRegisterIndex};

            const bool multipleDefinitions = defBasicBlocks.size() > 1;

            const size_t historyWidth = Log2RoundUp(defBasicBlocks.size());

            if (multipleDefinitions)
            {
                // There is more than one definition
                // Allocate a local variable to track which definition should be used
                std::ostringstream nameStr;
                nameStr << rd._name << "_history";

                historyRegister._registerIndex =
                    AllocateRegister(&program, historyWidth, RegisterType::Local, nameStr.str());

                // Initialize the history register at the start of the function
                historyRegistersToInitialize.insert(historyRegister._registerIndex);
            }

            // For each basic block that can write to this variable
            std::vector<AccessedRegister> memoryRegisters;

            size_t currHistoryIndex = 0;

            for (const DefinitionMapRecord& dmr : defBasicBlocks)
            {
                memoryRegisters.push_back(
                    InsertDataPropagationMemoryAndStore(program, bbDataPropMap, localThreadIdRegister._registerIndex, r,
                                                        dmr._renamedRegisterIndex, *dmr._basicBlock));

                if (multipleDefinitions)
                {
                    // Emit an operation that updates the history register
                    // with a literal indicating that this definition is the most recent
                    InsertDataPropagationHistoryUpdate(program, *dmr._basicBlock, dmr._basicBlock->_operations.end(),
                                                       historyRegister, currHistoryIndex);

                    if (dmr._basicBlock == function._start)
                    {
                        // No need to separately initialize the history register for this basic block
                        historyRegistersToInitialize.erase(historyRegister._registerIndex);
                    }

                    currHistoryIndex++;
                }
            }

            // Emit a load operation(s) in the using basic block
            // 1 load per definition
            assert(memoryRegisters.size() == defBasicBlocks.size());

            assert(!multipleDefinitions || (currHistoryIndex == defBasicBlocks.size()));

            InsertDataPropagationLoadOps(program, controlFlowGraph, basicBlock, r, memoryRegisters, historyWidth,
                                         historyRegister._registerIndex, localThreadIdRegister._registerIndex);
        }
    }

    if (initializeThreadIds)
    {
        InitializeDataPropagationThreadId(context, function, threadIdWidth, sharedThreadIdRegister,
                                          localThreadIdRegister);
    }

    // Initialize history registers at the start of the function
    for (const size_t r : historyRegistersToInitialize)
    {
        const AccessedRegister historyRegister = {r};

        InsertDataPropagationHistoryUpdate(program, *function._start, function._start->_operations.begin(),
                                           historyRegister, 0);
    }
}

// Grows a set of functions to include all functions which are reachable from that set
void GetReachableFunctions(const Program& program, std::set<Function*>& reachableFunctions, const bool includeAsync)
{
    const auto callback = [&](Operation& op)
    {
        if (op._getSuccessorBlock)
        {
            const BasicBlock* dstBasicBlock = op._getSuccessorBlock();

            Function* const function = dstBasicBlock->_function;

            const FunctionNode::Instance& instance = function->_functionNode->GetInstance(function->_objectName);

            const bool isAsync = function->IsAsync();

            // function->_start check is to not count function returns (if an unused function calls a sync function, the
            // return should not make the unused function reachable) GetTotalCallerThreadCount() check is to ignore
            // functions that have no callers Consier this case: void Foo() {} export void Bar() { call Foo(); } void
            // Gak() { call Foo(); }
            //
            // Foo contains a return to Gak, and foo is reachable
            // But Gak is not reachable
            //
            // async checks allow some callers to not include async functions in the analysis
            if ((dstBasicBlock == function->_start) && (instance.GetTotalCallerThreadCount() > 0) &&
                (includeAsync || !isAsync))
            {
                reachableFunctions.insert(function);
            }
        }
        else if (op._opcode == Opcode::CallAtomic)
        {
            const Function& function = program.GetInlineAtomicFunction(op._flags._callAtomic._functionIndex);

            reachableFunctions.insert(const_cast<Function*>(&function));
        }
    };

    // Until convergence
    while (true)
    {
        const size_t sizeBefore = reachableFunctions.size();

        // Find all functions that are directly reachable from the reachable functions
        for (Function* const function : reachableFunctions)
        {
            for (BasicBlock& basicBlock : function->_basicBlocks)
            {
                ForEachOperationForward(basicBlock, callback);
            }
        }

        if (sizeBefore == reachableFunctions.size())
        {
            // nothing changed, all reachable functions have been found
            break;
        }
    }
}

// Finds functions in the IR which are unreachable
void ComputeUnreachableFunctions(Program& program)
{
    std::set<Function*> reachableFunctions;

    // Start with all exported functions
    for (EntryPoint& entryPoint : program._entryPoints)
    {
        for (Function* const instance : entryPoint._instances)
        {
            reachableFunctions.insert(instance);
        }
    }

    // Add functions with the [[reset]] attribute
    for (Function& function : program._functions)
    {
        if (function.CallOnReset())
        {
            reachableFunctions.insert(&function);
        }
    }

    // Add functions which are callable from external class instances
    for (const ExternalClassInstance& em : program._externClassInstances)
    {
        for (const ExternalClassInstanceCallback& callback : em._callbacks)
        {
            reachableFunctions.insert(const_cast<Function*>(callback.GetCalledFunction()));
        }
    }

    // Grow the set to include all reachable functions
    GetReachableFunctions(program, reachableFunctions, true);

    for (Function& function : program._functions)
    {
        const bool isReachableNow = reachableFunctions.end() != reachableFunctions.find(&function);

        // If function instance enumeration decided that a function was unreachable
        // then it should still be unreachable
        assert(Implies(!function._isReachable, !isReachableNow));

        function._isReachable = isReachableNow;
    }
}

// Removes all operations from basic blocks which are unreachable
// Later optimizations will remove the basic blocks completely
bool RemoveUnreachableFunctions(Program& program)
{
    bool didChangeIR = false;

    // This can be called multiple times
    // each time can find more unreachable functions
    program._unreferencedExternFunctions.clear();

    // Called for each unreferenced basic block
    const auto clearBasicBlock = [&](BasicBlock& basicBlock)
    {
        if (!basicBlock._operations.empty() || !basicBlock._stages.empty() ||
            !basicBlock._startConditionOperations.empty())
        {
            didChangeIR = true;
        }

        basicBlock._operations.clear();
        basicBlock._stages.clear();
        basicBlock._startConditionOperations.clear();
    };

    for (Function& function : program._functions)
    {
        if (function._isReachable)
        {
            // Determine if any internal basic blocks are unreachable
            std::set<BasicBlock*> reachableBasicBlocks;
            std::set<BasicBlock*> workList;

            // Depth first search starting with the entry basic block
            reachableBasicBlocks.insert(function._start);
            workList.insert(function._start);

            while (!workList.empty())
            {
                std::set<BasicBlock*> newWorkList;

                for (BasicBlock* const src : workList)
                {
                    ForEachOperationForward(
                        *src,
                        [&](Operation& op)
                        {
                            if (op._getSuccessorBlock)
                            {
                                BasicBlock* const successor = op._getSuccessorBlock();

                                if (successor->_function == &function)
                                {
                                    if (reachableBasicBlocks.end() == reachableBasicBlocks.find(successor))
                                    {
                                        reachableBasicBlocks.insert(successor);
                                        newWorkList.insert(successor);
                                    }
                                }
                            }
                        },
                        OperationEnumerationMode::Unscheduled);
                }

                workList = newWorkList;
            }

            for (BasicBlock& bb : function._basicBlocks)
            {
                if (reachableBasicBlocks.end() == reachableBasicBlocks.find(&bb))
                {
                    clearBasicBlock(bb);
                }
            }
        }
        else
        {
            // The function is not reachable
            for (BasicBlock& basicBlock : function._basicBlocks)
            {
                clearBasicBlock(basicBlock);
            }

            if (function.IsExtern())
            {
                // Add to program._unreferencedExternFunctions
                // To enable a consistent interface to RTL
                program._unreferencedExternFunctions.push_back(&function);

                // Remove from program._externFunctions
                const auto it = std::find(program._externFunctions.begin(), program._externFunctions.end(), &function);

                if (it != program._externFunctions.end())
                {
                    program._externFunctions.erase(it);
                }
            }

            function._semaphores.clear();
        }
    }

    return didChangeIR;
}

// Removes unused entries from the register table
// Called before scheduling
void CompactRegisterTable(Program& program)
{
    // Get pointers to all register indices
    std::vector<size_t*> registerIndexPointers;
    std::vector<RegisterIndexMap*> registerIndexMapPointers;

    const auto enumerateCallback = [&](Operation& op)
    {
        for (SourceOperand& srcOp : op._src)
        {
            switch (srcOp.Type())
            {
            case SourceOperandType::Register:
                registerIndexPointers.push_back(&srcOp.GetAccessedRegister()._registerIndex);
                break;

            case SourceOperandType::Fifo:
                registerIndexPointers.push_back(&srcOp.GetFifoSubset()._registerIndex);
                break;

            case SourceOperandType::Literal:
            case SourceOperandType::StringLiteral:
                break;

            default:
                assert(false);
            }
        }

        for (DestinationOperand& destOp : op._dst)
        {
            switch (destOp.Type())
            {
            case DestinationOperandType::Register:
                registerIndexPointers.push_back(&destOp.GetAccessedRegister()._registerIndex);
                break;

            case DestinationOperandType::Fifo:
                registerIndexPointers.push_back(&destOp.GetFifoSubset()._registerIndex);
                break;

            default:
                assert(false);
            }
        }

        if (op._opcode == Opcode::Enqueue)
        {
            // If _getSuccessorBlock is valid, then the successor fifo index is computed later
            if (!op._getSuccessorBlock)
            {
                registerIndexPointers.push_back(&op._flags._enqueue._successorFifo);
            }

            if (EnqueueType::ReorderBuffer == op._flags._enqueue._type)
            {
                registerIndexPointers.push_back(&op._flags._enqueue._reorderSlotRegister);
            }

            registerIndexMapPointers.push_back(&op._renamingTable);

            registerIndexMapPointers.push_back(&op._reverseRenamingTable);
        }

        if ((op._opcode == Opcode::EnqueueRegisters) || (op._opcode == Opcode::DequeueRegisters))
        {
            registerIndexPointers.push_back(&op._flags._queueRegisters._fifoIndex);
        }
    };

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            ForEachOperationForward(basicBlock, enumerateCallback);

            for (size_t& registerIndex : basicBlock._liveInReg)
            {
                registerIndexPointers.push_back(&registerIndex);
            }

            for (size_t& registerIndex : basicBlock._forcedDeadInReg)
            {
                registerIndexPointers.push_back(&registerIndex);
            }

            for (size_t& registerIndex : basicBlock._returnValueReg)
            {
                registerIndexPointers.push_back(&registerIndex);
            }

            for (size_t& registerIndex : basicBlock._externalOnlyLiveInReg)
            {
                registerIndexPointers.push_back(&registerIndex);
            }

            if (basicBlock._invocationInstanceRegisterIndex)
            {
                registerIndexPointers.push_back(&(*basicBlock._invocationInstanceRegisterIndex));
            }

            // No need to consider _intraStageRegisters
            assert(basicBlock._intraStageRegisters.empty());
        }
    }

    // Global view functions
    for (auto& p : program._globalViewFunctions)
    {
        OperationList& operationList = p.second;

        for (Operation& op : operationList)
        {
            enumerateCallback(op);
        }
    }

    // Add loop-counter registers
    for (LoopGenerator& loopGenerator : program._loopGenerators)
    {
        registerIndexPointers.push_back(&loopGenerator._counterLocalRegisterIndex);
        registerIndexPointers.push_back(&loopGenerator._threadCountOneRegisterIndex);
    }

    // Add return FIFOs for synchronous, export functions
    for (Function& function : program._functions)
    {
        if (function._returnFifoRegisterIndex != c_invalidAccessedRegisterIndex)
        {
            registerIndexPointers.push_back(const_cast<size_t*>(&function._returnFifoRegisterIndex));
        }
    }

    // Inspectable variables
    for (InspectableVariable& inspectableVariable : program._inspectableVariables)
    {
        for (size_t& registerIndex : inspectableVariable._registers)
        {
            registerIndexPointers.push_back(&registerIndex);
        }
    }

    // Code coverage variables
    for (InspectableVariable& inspectableVariable : program._codeCoverageVariables)
    {
        for (size_t& registerIndex : inspectableVariable._registers)
        {
            registerIndexPointers.push_back(&registerIndex);
        }
    }

    // Generate the set of registers which are used in the program
    std::set<size_t> usedRegisters;

    for (const size_t* const registerIndexPointer : registerIndexPointers)
    {
        usedRegisters.insert(*registerIndexPointer);
    }

    for (const RegisterIndexMap* const registerIndexMap : registerIndexMapPointers)
    {
        for (const auto& p : *registerIndexMap)
        {
            usedRegisters.insert(p.first);
            usedRegisters.insert(p.second);
        }
    }

    // Create a new register table, and a mapping of old register indices to new register indices
    std::map<size_t, size_t> registerMap;
    RegisterTable newRegisterTable;

    for (size_t oldRegIndex = 0; oldRegIndex < program._registerTable.size(); ++oldRegIndex)
    {
        if (usedRegisters.end() != usedRegisters.find(oldRegIndex))
        {
            // Allocate new index
            const size_t newRegIndex = newRegisterTable.size();

            // Copy register description
            newRegisterTable.push_back(program._registerTable[oldRegIndex]);

            // Update mapping
            registerMap[oldRegIndex] = newRegIndex;
        }
    }

    // Update all register indices in structures
    for (size_t* const registerIndexPointer : registerIndexPointers)
    {
        *registerIndexPointer = SafeLookup(registerMap, *registerIndexPointer);
    }

    // Update all register index maps
    for (RegisterIndexMap* const registerIndexMap : registerIndexMapPointers)
    {
        RegisterIndexMap newMap;

        for (const auto& p : *registerIndexMap)
        {
            SafeInsert(newMap, SafeLookup(registerMap, p.first), SafeLookup(registerMap, p.second));
        }

        *registerIndexMap = newMap;
    }

    // Update allocated leaf registers
    // Note that a index being in an allocated leaf register is not justification
    // for it to stay in the register table.  But if register does stay in the table for another reason
    // then it is remapped here
    const std::list<AllocatedLeafRegister*>& leafRegisters = g_compiler->GetAllocatedLeafRegisters();

    for (AllocatedLeafRegister* reg : leafRegisters)
    {
        const auto it = registerMap.find(reg->_registerIndex);

        if (it != registerMap.end())
        {
            reg->_registerIndex = it->second;
        }
        else
        {
            // This register index should not be used beyond this point
            reg->_registerIndex = 0xbeefcafe;
        }
    }

    // Update global register indices in _objectNameToGlobals map
    // If any are no longer used, remove them
    for (auto& objectGlobalsPair : program._objectNameToGlobals)
    {
        std::set<size_t>& globals = objectGlobalsPair.second;

        std::set<size_t> newGlobals;

        for (const auto global : globals)
        {
            const auto it = registerMap.find(global);

            if (it != registerMap.end())
            {
                assert(global == it->first);

                newGlobals.insert(it->second);
            }
            // else - this global is no longer used. Do not add to the new set
        }

        globals.swap(newGlobals);
    }

    // Replace the register table
    program._registerTable = newRegisterTable;
}

void SetupSynchronousExterns(Program& program)
{
    // Maps fifo index of a return side to the associated context saver
    std::unordered_map<size_t, const ContextSaver*> returnSites;

    for (ContextSaver& contextSaver : program._contextSavers)
    {
        returnSites[contextSaver._destinationFifo] = &contextSaver;
    }

    for (Function* function : program._externFunctions)
    {
        const FunctionNode* functionNode = function->_functionNode;

        const bool isSync = !(functionNode->GetModifiers() & ParseTreeFunctionModifierAsync);
        assert(functionNode->GetModifiers() & ParseTreeFunctionModifierExternal);

        if (isSync)
        {
            const FunctionNode::Instance& instance = functionNode->GetInstance(function->_objectName);

            const size_t callSiteIndexReg = instance.GetCallSiteIndexRegister()->GetAccessedRegister()._registerIndex;

            const FifoOffsetMap& registerMap = function->_start->_inputFifoRegisterMap;

            const auto it = registerMap.find(callSiteIndexReg);
            assert(it != registerMap.end());

            function->_syncExtern._callIndexStartOffset = it->second;
            function->_syncExtern._callIndexWidth = program._registerTable[callSiteIndexReg]._width;
            function->_syncExtern._maxOutstandingCalls = 0;
            std::map<size_t, Placement::Vec2> positionMap;
            std::map<size_t, std::string> nameMap;

            for (size_t i = 0; i < instance.GetCallSiteCount(); i++)
            {
                const BasicBlock* const returnBlock = instance.GetReturnBlock(i);

                if (!returnBlock)
                {
                    // The return block has been optimized away
                    continue;
                }

                assert(1 == returnBlock->_inputFifoCount);
                const size_t fifoIndex = returnBlock->_inputFifoIndices[0];

                // fifoIndex represents the return site
                // but the actual return needs to go through a context saver
                const auto it = returnSites.find(fifoIndex);
                assert(it != returnSites.end());

                const ContextSaver* const contextSaver = it->second;

                // Save two things for each call site for the extern return router
                ExternReturnDesc returnDesc = {};
                //  1. The call site index that is used by the compiler and run-time to specify which call site should
                //  get the return data
                returnDesc._callSiteIndex = contextSaver->_callSiteIndex;
                //  2. The index of the FIFO where the data for a call site should be deposited
                returnDesc._fifoIndex = contextSaver->_fromCalleeFifoIndex;
                function->_syncExtern._returnDesc.push_back(returnDesc);

                // Add to the number of outstanding calls for this fifo
                // Each thread in the calling function can be responsible for 1 outstanding call
                function->_syncExtern._maxOutstandingCalls += returnBlock->_function->_maxThreadCountInsideFunction;

                SafeInsert(positionMap, returnDesc._fifoIndex, program.GetBasicBlockPosition(contextSaver->_afterCall));
                SafeInsert(nameMap, returnDesc._fifoIndex, contextSaver->_afterCall->_function->_name);
            }

            // Sort the call return sites based on their position in the virtual placement, which should put
            //  them in the same routing order as the command processors on the chip.  That way place & route
            //  should have an easier time routing the command processor I/O bus and the external router return
            //  buses together.
            const auto getPositionCallback = [&](const ExternReturnDesc ERRDesc)
            { return SafeLookup(positionMap, ERRDesc._fifoIndex); };

            const auto getNameCallback = [&](const ExternReturnDesc ERRDesc)
            { return SafeLookup(nameMap, ERRDesc._fifoIndex); };

            std::list<ExternReturnDesc> CallReturnSiteList(function->_syncExtern._returnDesc.begin(),
                                                           function->_syncExtern._returnDesc.end());
            const Placement::SortRecord sortRecord =
                Placement::Sort<ExternReturnDesc>(CallReturnSiteList, getPositionCallback, getNameCallback,
                                                  GetCodeGenConfig()._placementConfig._display, "Extern Return Router");
            function->_syncExtern._returnDesc =
                std::vector<ExternReturnDesc>(CallReturnSiteList.begin(), CallReturnSiteList.end());
        }
    }
}

// Determines the minimum number of threads require for a function
// to achieve full throughput.
// This can be used to size context savers.
size_t GetMinThreadCountForFullThroughput(const Function* function, const size_t callerMaxThreadCount)
{
    // to catch overflow at the end
    mp_int result = 0;

    const size_t fifoLatency = GetCodeGenDeviceConfig()._minAlmostFullDepth;

    // Returns true if a particular call should be considered in fifo sizing
    const auto considerCall = [](const Function* const caller, const Function* const callee)
    {
        // Ignore enqueue from caller to caller (this is just the normal callsite->return site fifo)
        // Also ignore calls to async functions as they don't increase context saver requirements
        return (caller != callee) && !callee->IsAsync();
    };

    if (function->IsExtern())
    {
        // Extern function
        // The only known bound is the max thread count of the callee
        result = callerMaxThreadCount;
    }
    else
    {
        for (const BasicBlock& basicBlock : function->_basicBlocks)
        {
            // The called function contains a loop
            // There is no way to predict how many threads
            // are needed to fully utilize the loop hardware.
            // The trouble comes in situations where many calls
            // are predicated (0 trips through the loop)
            // but some calls take many trips.  The context saver
            // needs to be deep enough to hold > 1 of the large-trip-count calls
            if (basicBlock._inputFifoCount > 1)
            {
                result = callerMaxThreadCount;
                break;
            }

            const size_t threadRate = basicBlock.GetThreadRateForFifoSizing();
            assert(threadRate > 0);

            // 1 thread for each group of "threadRate" pipeline stages
            const size_t pipelineStageCount =
                basicBlock._stages.empty() ? 0 : (basicBlock._stages.back()._atomicSequence + 1);

            const size_t threadCount = (pipelineStageCount + threadRate - 1) / threadRate;

            result += threadCount;

            // Add latency for entry fifo for this basic block
            result += fifoLatency;

            if (basicBlock.HasStartCondition())
            {
                // The slots in the input FIFO matter for throughput
                // in cases where the start condition is waiting for long-latency
                // asynchronous work to complete
                // Count these slots
                assert(1 == basicBlock._inputFifoCount);

                // Determine how deep the input fifo will be
                // This is based on an optional source-code specified minimum depth
                // and the device configuration
                const size_t waitForFifoDepth =
                    std::max(basicBlock._inputFifoMinDepth[0], GetCodeGenDeviceConfig()._minFifoDepth);

                result += waitForFifoDepth;
            }
        }

        // Add latency for exit fifo from this basic block
        result += fifoLatency;

        // Add required thread count for any called functions
        for (const BasicBlock& basicBlock : function->_basicBlocks)
        {
            for (const Stage& stage : basicBlock._stages)
            {
                for (const Operation& op : stage._operations)
                {
                    if ((Opcode::Enqueue == op._opcode) && (EnqueueType::FunctionCall == op._flags._enqueue._type))
                    {
                        const BasicBlock* const dstBasicBlock = op._getSuccessorBlock();

                        const Function* const dstFunction = dstBasicBlock->_function;

                        if (considerCall(function, dstFunction))
                        {
                            result += GetMinThreadCountForFullThroughput(dstFunction,
                                                                         function->_maxThreadCountInsideFunction);
                        }
                    }
                }
            }
        }

        // Pipelined function calls are troublesome when the thread count is frequently 0.
        // In this case, enough context saver space is needed to enable
        // the caller to quickly dispatch the the calls with thread count = 0, so that the caller
        // can get to the rare calls with thread count > 0.
        // In the worst case, if the thread count width is 4, then 15/16 calls would have thread count = 0
        // and 1/16 calls would have thread count = 16.  Throughput should be one inner thread per cycle.
        // The steady state size of the context saver at full throughput must be increased by 16, to allow
        // 15 more calls with thread count = 0 to be buffered while the thread count=16 call is running
        if (function->IsPipelined())
        {
            // mp_int used here to avoid overflow
            const mp_int maxThreadCount = mp_int(1) << function->_functionNode->GetParameterWidth(0);

            result = result + maxThreadCount;
        }

        // The max thread count of the function also bounds the number of threads
        // required for full throughput
        if (result > function->_maxThreadCountInsideFunction)
        {
            result = function->_maxThreadCountInsideFunction;
        }
    }

    return MpToSizeT(result);
}

void CreateContextSavers(Program& program)
{
    for (ContextSaver& contextSaver : program._contextSavers)
    {
        assert(0 == contextSaver._writeDelay);

        const BasicBlock* const afterCall = contextSaver._afterCall;

        // Record the return site fifo index
        assert(1 == afterCall->_inputFifoCount);
        contextSaver._destinationFifo = afterCall->_inputFifoIndices[0];

        // Lookup the register offsets for the basic block after the call
        const FifoOffsetMap& afterCallRegisterMap = afterCall->_inputFifoRegisterMap;

        size_t calleeOffset = 0;

        // Invocation instance is handled specially because it is not needed at the return site, only in the context
        // saver
        if (!contextSaver._isOrdered)
        {
            const size_t registerIndex =
                contextSaver._invocationInstanceLocalRegister->GetAccessedRegister()._registerIndex;

            const size_t registerWidth = program._registerTable[registerIndex]._width;

            contextSaver._calleeInputMap[registerIndex] = calleeOffset;

            calleeOffset += registerWidth;
        }

        // Return values are handled specially because in the pipelined case the return registers at the callee and
        // return site are different
        if (contextSaver._callerReturnValueRegisters)
        {
            // Handle registers at the callee
            {
                const auto callback = [&](const AccessedRegister reg, const Type* const type)
                {
                    const size_t registerWidth = program._registerTable[reg._registerIndex]._width;

                    contextSaver._calleeInputMap[reg._registerIndex] = calleeOffset;

                    calleeOffset += registerWidth;
                };

                contextSaver._calleeReturnValueRegisters->VisitRegisters(callback);
            }

            // Handle registers at the return site
            {
                size_t returnArrayOffset = 0;

                const auto callback = [&](const AccessedRegister reg, const Type* const type)
                {
                    const size_t registerWidth = program._registerTable[reg._registerIndex]._width;

                    const auto it = afterCallRegisterMap.find(reg._registerIndex);
                    assert(it != afterCallRegisterMap.end());

                    const size_t offsetInAfterCall = it->second;

                    contextSaver._fromCalleeOutputMap[reg._registerIndex] = offsetInAfterCall;

                    if (contextSaver._isPipelined)
                    {
                        // Merge functions should not try to find this register in _calleeInputMap, because it won't be
                        // there _calleeInputMap holds the scalar, _fromCalleeOutputMap holds the array
                        contextSaver._pipelinedCallerReturnRegisters[reg._registerIndex] = returnArrayOffset;

                        returnArrayOffset += registerWidth;
                    }
                };

                contextSaver._callerReturnValueRegisters->VisitRegisters(callback);
            }
        }

        // For each register that does NOT come from the callee
        size_t callerOffset = 0;

        for (const std::pair<size_t, size_t>& p : afterCallRegisterMap)
        {
            const size_t registerIndex = p.first;
            const size_t offsetInAfterCall = p.second;

            const size_t registerWidth = program._registerTable[registerIndex]._width;

            const auto it = contextSaver._fromCalleeOutputMap.find(registerIndex);
            if (it == contextSaver._fromCalleeOutputMap.end())
            {
                contextSaver._fromCallerOutputMap[registerIndex] = offsetInAfterCall;

                contextSaver._callerInputMap[registerIndex] = callerOffset;

                callerOffset += registerWidth;
            }
        }

        // Compute log(fifo depth needed to ensure no overflow (because of max thread count))
        size_t depthToAvoidOverflow = std::max(afterCall->_function->_maxThreadCountInsideFunction,
                                               GetCodeGenDeviceConfig()._minFifoDepth);

        if (contextSaver._isOrdered && (1 == afterCall->_function->_maxThreadCountInsideFunction))
        {
            // Ordered context savers have a specialization for depth = 1
            // Ignore _minFifoDepth in this case
            // _minFifoDepth is only used to avoid wasting "free" space
            depthToAvoidOverflow = 1;
        }

        // Destination FIFO index is used to identify the context saver

        // Allocate registers that the callee and caller write into
        contextSaver._fromCalleeFifoIndex = AllocatePipelineRegister(
            program, contextSaver._calleeInputMap, RegisterType::Fifo, contextSaver._callee->GetName() + "_return");

        contextSaver._fromCallerFifoIndex =
            AllocatePipelineRegister(program, contextSaver._callerInputMap, RegisterType::Fifo,
                                     contextSaver._afterCall->_function->_name + "_locals");

        assert(contextSaver._callSiteLocation.has_value());
        assert(contextSaver._callStackIndex.has_value());

        program._registerTable[contextSaver._fromCalleeFifoIndex].Fifo().SetLocation(*contextSaver._callSiteLocation,
                                                                                     *contextSaver._callStackIndex);
        program._registerTable[contextSaver._fromCallerFifoIndex].Fifo().SetLocation(*contextSaver._callSiteLocation,
                                                                                     *contextSaver._callStackIndex);

        // If there is more than one call site, then
        // the FIFO returning from the callee must never overflow
        //  Imagine the following code:
        //
        //  Foo(); - call 1
        //  Foo(); - call 2
        //  If call 1 is prioritized over call 2
        //  then the return fifo for call 1 will fill up
        //  this will cause Foo() to stall
        //  The return fifo will not be empty until call 2 can go
        //  which is a deadlock
        //
        // If [[transaction_size]] is used at the call site, size the fifo similarly
        // to ensure that there will be no backpressure from the return fifo which
        // causes a partial transaction to stop mid-flight
        // This is important in cases where the callee forwards the call onto another function with
        // multiple call sites.
        if ((contextSaver._calleeInstance->GetCallSiteCount() > 1) || contextSaver._hasTransactionSize)
        {
            RegisterDescription& calleeDesc = program._registerTable[contextSaver._fromCalleeFifoIndex];
            assert(calleeOffset == calleeDesc._width);
            calleeDesc.Fifo()._type = FifoType::Default;
            calleeDesc.Fifo()._depth = depthToAvoidOverflow;
        }

        {
            RegisterDescription& callerDesc = program._registerTable[contextSaver._fromCallerFifoIndex];
            assert(callerOffset == callerDesc._width);
            assert(!callerDesc.Fifo()._addMinimumDepth);

            // The fifo from the caller has storage for per-thread live variables
            // This fifo must be large enough to never overflow
            assert(0 == callerDesc.Fifo()._maxDepth);

            callerDesc.Fifo()._type = FifoType::ContextSaverCaller;
            callerDesc.Fifo()._depth = depthToAvoidOverflow;
            callerDesc.Fifo()._maxDepth = depthToAvoidOverflow;
            callerDesc.Fifo()._contextSaverCaller._ordered = contextSaver._isOrdered;
            callerDesc.Fifo()._contextSaverCaller._loopCounterWidth = contextSaver._loopCounterWidth;
            callerDesc.Fifo()._contextSaverCaller._hasTransactionSize = contextSaver._hasTransactionSize;

            if (contextSaver._isOrdered)
            {
                // Determine if a smaller size can be used than depthToAvoidOverflow
                // Ordered context savers support almost_full to avoid overflow
                // So the context saver only needs to be deep enough for full
                // throughput
                const size_t requiredDepth = GetMinThreadCountForFullThroughput(
                    contextSaver._calleeInstance->GetFunction(),
                    contextSaver._beforeCall->_function->_maxThreadCountInsideFunction);

                // No need to size the fifo larger than depthToAvoidOverflow
                // as that size will ensure no overflow
                if (requiredDepth < depthToAvoidOverflow)
                {
                    callerDesc.Fifo()._depth = requiredDepth;
                    callerDesc.Fifo()._addMinimumDepth = true;
                }
                else
                {
                    assert(callerDesc.Fifo()._depth == depthToAvoidOverflow);
                    assert(!callerDesc.Fifo()._addMinimumDepth);
                }
            }

            // Assert that an invocation index is wide enough
            assert(callerDesc.Fifo().LogDepth() <= GetCodeGenConfig().GetInvocationIndexSize());
        }
    }
}

// Sets Operation::_successorFifo for all Enqueue operations
void SetSuccessorFifos(Program& program)
{
    // Maps fifo index of a return side to the associated context saver
    std::unordered_map<size_t, const ContextSaver*> returnSites;

    for (ContextSaver& contextSaver : program._contextSavers)
    {
        returnSites[contextSaver._destinationFifo] = &contextSaver;
    }

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            for (Stage& stage : basicBlock._stages)
            {
                for (Operation& op : stage._operations)
                {
                    if (op._getSuccessorBlock)
                    {
                        assert(op._opcode == Opcode::Enqueue);

                        const BasicBlock* const block = op._getSuccessorBlock();

                        // Loop start basic blocks have 2 input fifos - 1 is the backwards link
                        const size_t whichFifo = op._flags._enqueue._whichFifo;
                        assert(whichFifo < block->_inputFifoCount);

                        const size_t dstInputFifo = block->_inputFifoIndices[whichFifo];

                        const auto it = returnSites.find(dstInputFifo);

                        if (it == returnSites.end())
                        {
                            op._flags._enqueue._successorFifo = dstInputFifo;
                        }
                        else
                        {
                            const ContextSaver* const contextSaver = it->second;

                            // Set to one of the context saver input fifos if necessary
                            switch (op._flags._enqueue._type)
                            {
                                // Jump from call site to return site in case the function call is predicated away
                            case EnqueueType::FunctionCall:
                            case EnqueueType::Default:
                                op._flags._enqueue._successorFifo = dstInputFifo;
                                break;

                                // Jump from call site to context saver, with live variables to be held until the call
                                // returns
                            case EnqueueType::ContextSaverCaller:
                                op._flags._enqueue._successorFifo = contextSaver->_fromCallerFifoIndex;
                                break;

                                // Return from called function to return site, via the context saver to merge the return
                                // values with the saved live variables
                            case EnqueueType::ContextSaverCallee:
                                op._flags._enqueue._successorFifo = contextSaver->_fromCalleeFifoIndex;
                                break;

                            default:
                                assert(false);
                            }
                        }
                    }
                }
            }
        }
    }
}

// Sets up reorder buffer information in fifo register records
// called after SetSuccessorFifos
void SetupReorderBuffers(Program& program)
{
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            for (Stage& stage : basicBlock._stages)
            {
                for (Operation& op : stage._operations)
                {
                    if (op._opcode == Opcode::Enqueue)
                    {
                        if (op._flags._enqueue._type == EnqueueType::ReorderBuffer)
                        {
                            const size_t fifoIndex = op._flags._enqueue._successorFifo;

                            RegisterDescription& fifoRegDesc = program._registerTable[fifoIndex];

                            assert(RegisterType::Fifo == fifoRegDesc._type);
                            assert(FifoType::Default == fifoRegDesc.Fifo()._type);

                            fifoRegDesc.Fifo()._type = FifoType::ReorderBuffer;

                            // Map local register ID to location in fifo
                            const BasicBlock* const dstBasicBlock = op._getSuccessorBlock();

                            const FifoOffsetMap& destRegisterOffsetMap = dstBasicBlock->_inputFifoRegisterMap;

                            const size_t localSlotRegisterIndex = op._flags._enqueue._reorderSlotRegister;

                            const auto it = destRegisterOffsetMap.find(localSlotRegisterIndex);
                            assert(it != destRegisterOffsetMap.end());

                            const RegisterDescription& slotRegDesc = program._registerTable[localSlotRegisterIndex];
                            assert(RegisterType::Local == slotRegDesc._type);

                            fifoRegDesc.Fifo()._reorderBuffer._slotOffset = it->second;
                            fifoRegDesc.Fifo()._reorderBuffer._slotWidth = slotRegDesc._width;
                        }
                    }
                }
            }
        }
    }
}

LoopGenerator& FindLoopGenerator(Program& program, const BasicBlock* const basicBlock)
{
    for (LoopGenerator& loopGenerator : program._loopGenerators)
    {
        if (loopGenerator._function->_start == basicBlock)
        {
            return loopGenerator;
        }
    }

    throw RuntimeErrorWithTrace("Failed to locate loop generator");
}

void InitializeLoopGenerator(LoopGenerator& loopGenerator)
{
    assert(!loopGenerator._initialized);

    // Lookup the offset of the counter in the dest FIFO
    const BasicBlock* const dstBasicBlock = loopGenerator._function->_start;

    // RemoveDeadJumps assures this
    assert(!dstBasicBlock->_stages.empty());

    const FifoOffsetMap& destRegisterOffsetMap = dstBasicBlock->_inputFifoRegisterMap;

    loopGenerator._initialized = true;
    loopGenerator._counterOffset = SafeLookup(destRegisterOffsetMap, loopGenerator._counterLocalRegisterIndex);
    loopGenerator._threadCountOneOffset = SafeLookup(destRegisterOffsetMap, loopGenerator._threadCountOneRegisterIndex);
}

void GenerateLoops(Program& program, BasicBlock& basicBlock)
{
    for (Stage& stage : basicBlock._stages)
    {
        for (Operation& op : stage._operations)
        {
            if (op._getSuccessorBlock && op._flags._enqueue._isPipelinedCall)
            {
                // Find the associated loop generator
                LoopGenerator& loopGenerator = FindLoopGenerator(program, op._getSuccessorBlock());

                if (!loopGenerator._initialized)
                {
                    InitializeLoopGenerator(loopGenerator);
                }
            }
        }
    }

    // Also, loop generators for export functions
    for (LoopGenerator& loopGenerator : program._loopGenerators)
    {
        if (loopGenerator._function->IsExport())
        {
            InitializeLoopGenerator(loopGenerator);
        }
    }
}

void RemoveDeadOperationsFromFunction(Function& function, Program& program, OperationEnumerationMode mode,
                                      ControlFlowGraphPhase phase)
{
    ControlFlowGraph cfg(function, program, mode, phase);

    while (RemoveDeadOperations(program, cfg, function, mode))
    {
        // run until convergence
    }
}

// Detects cases where memory stores have been optimized away
// Removes bypass flags and bypass operations in those cases
// Runs after scheduling
void RemoveUnnecessaryBypass(Program& program)
{
    for (Function& function : program._functions)
    {
        bool convertedBypass = false;

        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            std::set<size_t> loadBypassGroups;
            std::set<size_t> bypassMemoryBypassGroups;
            std::set<size_t> storeBypassGroups;

            for (const Stage& stage : basicBlock._stages)
            {
                for (const Operation& op : stage._operations)
                {
                    if ((Opcode::LoadMemory == op._opcode) && op._flags._loadMemory._bypass)
                    {
                        loadBypassGroups.insert(op._flags._loadMemory._bypassGroupIndex);
                    }
                    else if (op._opcode == Opcode::BypassMemory)
                    {
                        bypassMemoryBypassGroups.insert(op._flags._bypassMemory._bypassGroupIndex);
                    }
                    else if ((op._opcode == Opcode::StoreMemory) && op._flags._storeMemory._bypass)
                    {
                        storeBypassGroups.insert(op._flags._storeMemory._bypassGroupIndex);
                    }
                }
            }

            // Cleanup any bypass group that does not contain at least one of each type of operation
            const std::set<size_t> validGroups =
                Intersection(loadBypassGroups, Intersection(bypassMemoryBypassGroups, storeBypassGroups));

            for (Stage& stage : basicBlock._stages)
            {
                auto nextIt = stage._operations.begin();

                for (auto it = stage._operations.begin(); it != stage._operations.end(); it = nextIt)
                {
                    nextIt = it;
                    ++nextIt;

                    Operation& op = *it;

                    if ((Opcode::LoadMemory == op._opcode) && op._flags._loadMemory._bypass &&
                        !Contains(validGroups, op._flags._loadMemory._bypassGroupIndex))
                    {
                        op._flags._loadMemory._bypass = false;
                    }
                    else if ((op._opcode == Opcode::BypassMemory) &&
                             !Contains(validGroups, op._flags._bypassMemory._bypassGroupIndex))
                    {
                        // Replace the bypass operation with mov operations
                        assert(op._src.size() == (op._dst.size() + 1));

                        convertedBypass = true;

                        for (size_t i = 0; i < op._dst.size(); i++)
                        {
                            Operation mov = {};

                            mov._opcode = Opcode::Mov;
                            mov.PushOperand(op, i + 1);
                            mov._dst.push_back(op._dst[i]);

                            stage._operations.insert(it, mov);
                        }

                        stage._operations.erase(it);
                    }
                    else if ((op._opcode == Opcode::StoreMemory) && op._flags._storeMemory._bypass &&
                             !Contains(validGroups, op._flags._storeMemory._bypassGroupIndex))
                    {
                        op._flags._storeMemory._bypass = false;
                    }
                }
            }
        }

        if (convertedBypass)
        {
            // If bypass operations were converted to mov
            // remove mov operations which do not affect the final output
            RemoveDeadOperationsFromFunction(function, program, OperationEnumerationMode::Scheduled,
                                             ControlFlowGraphPhase::PostPipeline);
        }
    }
}

// Detect calls to pipelined functions with constant loop counts
// Modifies the IR so that the generated RTL can be specialized based on those loop counts.
void DetectConstantLoopCounts(Program& program)
{
    // Build mapping of function to loop generator
    std::map<const Function*, LoopGenerator*> functionToLoop;

    for (LoopGenerator& loopGenerator : program._loopGenerators)
    {
        if (loopGenerator._initialized)
        {
            SafeInsert(functionToLoop, loopGenerator._function, &loopGenerator);
        }
    }

    for (const Function& function : program._functions)
    {
        if (function.IsPipelined())
        {
            assert(function._start);

            const auto it = functionToLoop.find(&function);
            if (it == functionToLoop.end())
            {
                // This occurs if there are unreferenced functions
                continue;
            }

            LoopGenerator& loopGenerator = *(it->second);

            assert(!loopGenerator._literalMaxThreadId);

            const BasicBlock& startBb = *function._start;

            assert(1 == startBb._inputFifoCount);

            const size_t fifoIndex = startBb._inputFifoIndices[0];

            const RegisterDescription& rd = program._registerTable[fifoIndex];

            const FifoCode& fifoCode = rd.Fifo()._code;

            if (!fifoCode._initialized)
            {
                // This occurs if there are multiple call sites
                continue;
            }

            for (const FifoCodeOutputRange& range : fifoCode._outputRanges)
            {
                if (range._isLiteral && (range._width == loopGenerator._counterWidth) &&
                    (range._decodedOffset == loopGenerator._counterOffset))
                {
                    // Loop counts always come first
                    assert(0 == loopGenerator._counterOffset);

                    const size_t maxThreadId = MpToSizeT(range._literal);

                    assert(!loopGenerator._literalMaxThreadId);
                    loopGenerator._literalMaxThreadId = maxThreadId;
                }
            }
        }
    }
}

// Verifies that stage atomic sequence numbers can be used in almost_full calculation
void AssertSequenceNumbers(const BasicBlock& basicBlock)
{
    assert(!basicBlock._stages.empty());

    assert(0 == basicBlock._stages.front()._atomicSequence);

    size_t prevSeqNum = basicBlock._stages.front()._atomicSequence;

    for (const Stage& stage : basicBlock._stages)
    {
        assert((stage._atomicSequence == prevSeqNum) || (stage._atomicSequence == (prevSeqNum + 1)));

        prevSeqNum = stage._atomicSequence;
    }
}

// returns true if the basic block can introduce backpressure
// (transitive backpressure from fifos written by this basic block do not count)
bool BasicBlockCanIntroduceBackpressure(const BasicBlock& bb)
{
    bool result = false;

    if (bb._function->_functionThreadRate != 1)
    {
        result = true;
    }

    if (bb.HasStartCondition())
    {
        result = true;
    }

    const std::set<size_t> acquiredSemaphores = GetAcquiredSemaphores(bb);

    if (!acquiredSemaphores.empty())
    {
        result = true;
    }

    return result;
}

// Finds basic blocks which can not introduce nor propagate back-pressure
// Removes input FIFOs for those
void InferNoBackpressure(Program& program)
{
    // Maps basic block to the set of fifos that could possibly introduce back-pressure
    std::map<BasicBlock*, std::set<size_t>> basicBlockBackpressureFifos;

    for (Function& function : program._functions)
    {
        for (BasicBlock& bb : function._basicBlocks)
        {
            SafeInsert(basicBlockBackpressureFifos, &bb, GetBackpressureFifos(program, bb));
        }
    }

    bool iterating = false;

    do
    {
        iterating = false;

        for (Function& function : program._functions)
        {
            for (BasicBlock& bb : function._basicBlocks)
            {
                if (&bb == function._start)
                {
                    // Function modifiers can cause the first basic block in a function
                    // to introduce back pressure.  Only allow a few "known good" modifiers
                    const ParseTreeFunctionModifier allowedModifiers =
                        ParseTreeFunctionModifierAsync | ParseTreeFunctionModifierNoInline;

                    const ParseTreeFunctionModifier modifiers = function._functionNode->GetModifiers();

                    if ((modifiers & ~allowedModifiers) != 0)
                    {
                        continue;
                    }
                }

                if (BasicBlockCanIntroduceBackpressure(bb))
                {
                    // This basic block can introduce back-pressure
                    // so its input FIFOs cannot be optimized.
                    continue;
                }

                if (bb._inputFifoCount != 1)
                {
                    // Looping can introduce back-pressure
                    continue;
                }

                const std::set<size_t>& outputFifos = SafeLookup(basicBlockBackpressureFifos, &bb);

                bool backPressureFromOutputFifos = false;

                for (const size_t fifoIndex : outputFifos)
                {
                    const RegisterDescription& rd = program._registerTable[fifoIndex];

                    if ((FifoImplementation::Default != rd.Fifo()._implementation) ||
                        FifoTypeCanBackpressure(rd.Fifo()._type))
                    {
                        // A downstream FIFO can exert back-pressure
                        backPressureFromOutputFifos = true;
                        break;
                    }
                }

                if (backPressureFromOutputFifos)
                {
                    continue;
                }

                const size_t inputFifoIndex = bb._inputFifoIndices[0];

                RegisterDescription& rd = program._registerTable[inputFifoIndex];

                if ((FifoImplementation::Default == rd.Fifo()._implementation) &&
                    (FifoType::Default == rd.Fifo()._type))
                {
                    rd.Fifo()._type = FifoType::PassthroughRegistered;

                    // Allow the outer loop to run again
                    // as this may allow upstream basic block
                    // to be optimized
                    iterating = true;
                }
            }
        }

    } while (iterating);
}

// Sets the single threaded flag on fifos
// called after fifo mergers are instantiated
void SelectFifoImplementation(Program& program)
{
    // Verify initial values
    for (const RegisterDescription& regDesc : program._registerTable)
    {
        if (RegisterType::Fifo == regDesc._type)
        {
            assert(regDesc.Fifo()._implementation == FifoImplementation::Default);
            assert(regDesc.Fifo()._writeClock == 0);
            assert(regDesc.Fifo()._readClock == 0);
        }
    }

    // To improve timing, use KanagawaInternalBufferFifo for basic blocks with start conditions
    // because the start condition must be evaluated, and the fifo rden bit set based off that
    // We skip this optimization for devices for which it does not provide an Fmax
    // benefit, because it comes with an additional area cost.
    if (GetCodeGenDeviceConfig()._useInternalBufferFifoOptimization)
    {
        for (Function& function : program._functions)
        {
            for (BasicBlock& basicBlock : function._basicBlocks)
            {
                if (basicBlock.HasStartCondition())
                {
                    for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
                    {
                        const size_t fifoIndex = basicBlock._inputFifoIndices[i];

                        RegisterDescription& regDesc = program._registerTable[fifoIndex];
                        assert(RegisterType::Fifo == regDesc._type);
                        assert(FifoImplementation::Default == regDesc.Fifo()._implementation);

                        if (FifoType::Default == regDesc.Fifo()._type)
                        {
                            regDesc.Fifo()._implementation = FifoImplementation::InternalBuffer;
                        }
                    }
                }
            }
        }
    }

    // Special case handling for entry FIFOs:
    //  - If enumerating entry points for export class, we want to use a special identifiable entry FIFO for
    //    callee that has optional additional write-side pipeline registers
    if (GetCodeGenConfig()._autoPipelineCrossRegion > 0)
    {
        for (const EntryPoint& entryPoint : program._entryPoints)
        {
            for (Function* const function : entryPoint._instances)
            {
                // Use cross-region FIFOs for:
                // Export class entry point parameters (!program._isDefaultPass)
                // Export class callback parameters (function->IsExportClassInterface())
                if (!program._isDefaultPass || function->IsExportClassInterface())
                {
                    const BasicBlock* const entryBasicBlock = function->_start;

                    // A Reset function will have no input FIFO
                    if (entryBasicBlock->_inputFifoCount)
                    {
                        assert(1 == entryBasicBlock->_inputFifoCount);

                        const size_t fifoIndex = entryBasicBlock->_inputFifoIndices[0];

                        RegisterDescription& regDesc = program._registerTable[fifoIndex];

                        assert(regDesc.Fifo()._type == FifoType::Default);
                        regDesc.Fifo()._implementation = FifoImplementation::CrossRegion;
                    }
                }
            }
        }
    }

    // Special case handling for entry FIFOs:
    //  Entry points do not need almost_full values, and thus can use passthrough fifos to save area
    if (program._isDefaultPass || GetCodeGenConfig()._autoPipelineCrossRegion == 0)
    {
        for (const EntryPoint& entryPoint : program._entryPoints)
        {
            for (Function* const function : entryPoint._instances)
            {
                assert(!function->_basicBlocks.empty());

                // Avoid dealing with extern modules (which could run on a different clock domain)
                // Reset functions are excluded for simplicity (because the code that generates the fifo signals for it
                // is special) If the first basic block has a start condition, then do not do this optimization to
                // simplify the logic that connects to the export function.  For example allow logic to check back
                // pressure signals from 2 export functions and only enqueue to one export if both report no back
                // pressure.
                if (!function->_externClassInstance &&
                    !function->CallOnReset() && !function->_basicBlocks.front().HasStartCondition())
                {
                    const BasicBlock* const entryBasicBlock = function->_start;

                    assert(1 == entryBasicBlock->_inputFifoCount);

                    const size_t fifoIndex = entryBasicBlock->_inputFifoIndices[0];

                    RegisterDescription& regDesc = program._registerTable[fifoIndex];
                    assert(RegisterType::Fifo == regDesc._type);
                    assert(0 == regDesc.Fifo()._almostFullSlots);

                    // Only perform this optimisation if there wasn't an input FIFO depth constraint
                    if (entryBasicBlock->_inputFifoMinDepth[0] == 0)
                    {
                        regDesc.Fifo()._type = FifoType::Passthrough;
                    }
                }
            }
        }
    }

    // Functions with no_backpressure attribute use registered passthrough fifo for input and output
    // Registered version can be use because no backpressure is allowed
    const auto SetPassthroughFifo =
        [&](const size_t fifoIndex, const FifoType newType = FifoType::PassthroughRegistered)
    {
        RegisterDescription& regDesc = program._registerTable[fifoIndex];
        assert(RegisterType::Fifo == regDesc._type);
        assert(FifoImplementation::Default == regDesc.Fifo()._implementation);
        assert((FifoType::Default == regDesc.Fifo()._type) || (FifoType::Passthrough == regDesc.Fifo()._type) ||
               (FifoType::PassthroughRegistered == regDesc.Fifo()._type));

        regDesc.Fifo()._type = newType;
    };

    for (Function& function : program._functions)
    {
        // The function._basicBlocks.empty() check is needed to handle unreferenced
        // callbacks.  These functions are empty, but placed in Program::_unreferencedExternFunctions
        // to ensure a consistent interface is generated.
        if (function._functionNode->NoBackpressure() && !function._basicBlocks.empty())
        {
            assert(1 == function._basicBlocks.size());

            BasicBlock& basicBlock = function._basicBlocks.front();
            assert(1 == basicBlock._inputFifoCount);
            // Only consider this optimisation if there wasn't an input FIFO depth constraint
            if (basicBlock._inputFifoMinDepth[0] == 0)
            {
                SetPassthroughFifo(basicBlock._inputFifoIndices[0]);
            }

            // All output fifos must not have backpressure
            const std::set<size_t> writtenFifos = GetWrittenFifos(basicBlock);

            for (const size_t fifoIndex : writtenFifos)
            {
                SetPassthroughFifo(fifoIndex);
            }
        }

        // Fixed-latency functions use unregistered passthrough fifos for input and output
        // to avoid introducing a lower-bound on function latency to register inputs and outputs
        if (function._functionNode->IsFixedLatency())
        {
            assert(1 == function._basicBlocks.size());
            const BasicBlock& bb = function._basicBlocks.front();
            assert(1 == bb._inputFifoCount);
            assert(0 == bb._inputFifoMinDepth[0]);

            SetPassthroughFifo(bb._inputFifoIndices[0], FifoType::PassthroughUnregistered);

            const std::set<size_t> writtenFifos = GetWrittenFifos(bb);

            for (const size_t fifoIndex : writtenFifos)
            {
                SetPassthroughFifo(fifoIndex, FifoType::PassthroughUnregistered);
            }
        }
    }

    // Find loops that contain only 1 basic block
    //
    // Optimization 1:
    // Change the backward link to be implemented with just 1 register
    // No backpressure is needed on the backward link because:
    // 1) The basic block will always choose a backward link thread over a new thread
    // 2) The only other fifo written by the basic block is the exit fifo, which is sized large enough to never
    // backpressure
    //
    // Optimization 2:
    // If the downstream basic block (after the loop) cannot introduce backpressure
    // and there is no reorder buffer
    // then change the fifo connection the loop to the downstream basic block to be passthrough
    // The almost_full signals from fifos written by the downstream basic block
    // will be checked by the loop instead of the downstream basic block
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            if (basicBlock._isOneBBLoop)
            {
                // Check if the basic block can write to any FIFOs outside of the containing function
                // Those fifo writes could cause back pressure
                const auto successorBlocks = GetSuccessors(basicBlock);

                bool containsCrossFunctionEnqueue = false;

                for (const BasicBlock* const successor : successorBlocks)
                {
                    if (successor->_function != basicBlock._function)
                    {
                        containsCrossFunctionEnqueue = true;
                    }
                }

                // The containsCrossFunctionEnqueue check ensure that no backpressure can come from other fifos written
                // inside of the loop BasicBlockCanIntroduceBackpressure checks other conditions (start condition,
                // thread rate)
                if (!containsCrossFunctionEnqueue && !BasicBlockCanIntroduceBackpressure(basicBlock))
                {
                    assert(2 == basicBlock._inputFifoCount);
                    const size_t backLinkIndex = basicBlock._inputFifoIndices[1];

                    // Find the downstream block

                    // If the downstream basic block cannot introduce backpressure, then change the link between the
                    // loop and downstream block to be passthrough
                    for (const Stage& stage : basicBlock._stages)
                    {
                        for (const Operation& op : stage._operations)
                        {
                            if ((op._opcode == Opcode::Enqueue) && (op._flags._enqueue._successorFifo != backLinkIndex))
                            {
                                BasicBlock* const downstreamBlock = op._getSuccessorBlock();
                                assert(downstreamBlock->_function == &function);
                                assert(1 == downstreamBlock->_inputFifoCount);

                                // Determine if the downstream block has any fifos that it writes to
                                const std::set<size_t> downStreamBlockWrittenFifos = GetWrittenFifos(*downstreamBlock);

                                // True if the downstream basic block can add more backpressure (besides just passing it
                                // on)
                                const bool downstreamIntroducesBackpressure =
                                    BasicBlockCanIntroduceBackpressure(*downstreamBlock);

                                // True if no backpressure can come from the downstream block (including transitive
                                // backpressure)
                                const bool noBackpressureFromDownstream =
                                    (downStreamBlockWrittenFifos.empty() && !downstreamIntroducesBackpressure);

                                if (basicBlock._oneBBLoopOrdered)
                                {
                                    // Change the look backward link to be a passthrough fifo
                                    // If the loop is ordered, then it is sufficient to check the downstream almost_full
                                    // value when a thread enters the loop for the first time.
                                    SetPassthroughFifo(backLinkIndex);

                                    // Change the loop exit to be passthrough if possible
                                    // This is done by making the loop basic block also check almost_full signals from
                                    // fifos written by the downstream block Again this only applies to ordered loops
                                    // for the same reason mentioned above about checking when a thread first enters the
                                    // loop
                                    const RegisterDescription linkDesc =
                                        program._registerTable[op._flags._enqueue._successorFifo];

                                    if ((FifoType::Default == linkDesc.Fifo()._type) &&
                                        !downstreamIntroducesBackpressure)
                                    {
                                        SetPassthroughFifo(op._flags._enqueue._successorFifo);

                                        assert(!downstreamBlock->_transitiveBackpressurePredecessor);
                                        assert(!basicBlock._transitiveBackpressureSucessor);

                                        downstreamBlock->_transitiveBackpressurePredecessor = &basicBlock;
                                        basicBlock._transitiveBackpressureSucessor = downstreamBlock;
                                    }
                                }
                                else if (noBackpressureFromDownstream)
                                {
                                    // For an unordered loop, an output fifo could be empty when thread N enters the
                                    // loop but then the fifo could fill output and become full when thread N is ready
                                    // to exit the loop If the loop is unordered, then only do this optimization if the
                                    // downstream block can never introduce backpressure (nor transitively pass
                                    // backpressure on)
                                    SetPassthroughFifo(backLinkIndex);

                                    // The loop exit fifo can also be made into a passthrough fifo
                                    SetPassthroughFifo(op._flags._enqueue._successorFifo);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Must run after the loop modification above (which sets _transitiveBackpressureSucessor and
    // _transitiveBackpressurePredecessor)
    InferNoBackpressure(program);

    // Ordered context savers can pass data directly to the return site - no fifo is needed
    // This is not done if the return site has a start condition with shadow copies of local registers
    // because the shadow copy transformation requires the fifo->basic block interface to be empty/rden semantics
    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        if (contextSaver._isOrdered && !contextSaver._afterCall->_hasAtomicDoShadowRegisters)
        {
            assert(1 == contextSaver._afterCall->_inputFifoCount);
            const size_t fifoIndex = contextSaver._afterCall->_inputFifoIndices[0];

            RegisterDescription& regDesc = program._registerTable[fifoIndex];
            assert(RegisterType::Fifo == regDesc._type);
            assert(0 == regDesc.Fifo()._almostFullSlots);

            regDesc.Fifo()._type = FifoType::Passthrough;
        }
    }

    // Dual-clock fifos must have default implemention and type
    for (RegisterDescription& regDesc : program._registerTable)
    {
        if (RegisterType::Fifo == regDesc._type)
        {
            // One side of the fifo should be on clk0
            assert((regDesc.Fifo()._readClock == 0) || (regDesc.Fifo()._writeClock == 0));

            if (regDesc.Fifo()._readClock != regDesc.Fifo()._writeClock)
            {
                assert(regDesc.Fifo()._type == FifoType::Default);
                regDesc.Fifo()._implementation = FifoImplementation::Default;
            }
        }
    }
}

void ComputeFifoSize(const size_t configMinDepth, const size_t configDualClockMinDepth,
                     const size_t configMinimumAlmostFullDepth, const size_t configAlmostFullBackwardLatency,
                     const size_t minWriteDelay, const size_t maxWriteDelay, const size_t modifierMinDepth,
                     const size_t callRate, const size_t readClock, const size_t writeClock, const size_t stageIndex,
                     const bool addToResultDepth, const size_t minDepth, const size_t maxDepth,
                     const size_t syncCallerMaxThreadCount, const bool contextSaverHasTransactionSize,
                     const size_t transactionSize, size_t& resultDepth, size_t& resultAlmostFullSlots,
                     size_t& resultWriteDelay, size_t& resultMinWriteDelay)
{
    const size_t minimumDepth =
        std::max(readClock == writeClock ? configMinDepth : configDualClockMinDepth, modifierMinDepth);

    if (readClock == writeClock && maxWriteDelay > 1)
    {
        resultMinWriteDelay = minWriteDelay;
        resultWriteDelay = maxWriteDelay;
    }
    else
    {
        resultWriteDelay = 0;
        resultMinWriteDelay = 0;
    }

    assert(resultMinWriteDelay <= resultWriteDelay);

    // This value is added to the stage index to determine the fifo size
    // configAlmostFullBackwardLatency is needed to avoid overflow.  The almost_full value is delayed by
    // _additionalLatency cycles sizeForDualClock is added because dual-clock fifo almost_full signals are computed from
    // usedw external port
    //   This technique requires the depth to be 1 larger than for non dual-clock fifos
    const size_t sizeForDualClock = (readClock != writeClock) ? 1 : 0;

    const size_t sizeToAdd = configAlmostFullBackwardLatency + sizeForDualClock + 2 * resultWriteDelay;

    // Apply the call rate modifier, to reduce the fifo size needed to avoid overflow
    const size_t logicalStageIndex = (stageIndex + callRate - 1) / callRate;

    const size_t computedDepth = logicalStageIndex + sizeToAdd;

    size_t newDepth = 0;

    if (addToResultDepth)
    {
        newDepth = computedDepth;
    }
    else
    {
        // For deadlock-avoidance, some fifo sizes are pre-determined (while others are 0)
        // Conservatively satisfy all constraints
        newDepth = std::max<size_t>(computedDepth, minDepth);
    }

    // Honor the configuration minimum depth and minimum depth specified by a modifier
    resultDepth = std::max(newDepth, minimumDepth);
    assert(resultDepth >= (logicalStageIndex + sizeToAdd));

    // Set almost full based on the stage index and configAlmostFullBackwardLatency (to avoid overflow)
    // sizeForDualClock and configMinimumAlmostFullDepth do not affect the almost full depth
    resultAlmostFullSlots = (logicalStageIndex + configAlmostFullBackwardLatency);

    // Adjust producer fifo depth to achieve full throughput for the situation where a consumer is coming out of a
    // backpressure situation Imagine a situation where the fifo is almost full (usedw == almost full threshold) And the
    // pipeline feeding it is empty.  Call this cycle 0. Assume that from this cycle onward, the consumer will dequeue 1
    // element every cycle On cycle 1, the fifo will no longer be almost full on cycle
    // 1+configAlmostFullBackwardLatency, the producer pipeline will start enqueuing threads these threads will reach
    // the fifo stageIndex cycles later and will not register for the consumer to consume until
    // configMinimumAlmostFullDepth cycles later
    //
    // Full throughput is achieved if the fifo never goes into the empty state during this time
    // so that the consumer always has something to consume
    const size_t minAlmostFullThreshold = configAlmostFullBackwardLatency + stageIndex + configMinimumAlmostFullDepth +
                                          (addToResultDepth ? minDepth : 0) + (2 * resultWriteDelay);

    resultDepth = std::max(resultDepth, minAlmostFullThreshold + resultAlmostFullSlots);

    // Calls with transaction size modifier
    if (transactionSize > 0)
    {
        assert(!contextSaverHasTransactionSize);
        // If this is the transactional FIFO, it needs to hold at least one
        // full transaction
        resultDepth = std::max(resultDepth, minAlmostFullThreshold + transactionSize);
    }
    else if (contextSaverHasTransactionSize)
    {
        assert(syncCallerMaxThreadCount != std::numeric_limits<size_t>::max());
        // Otherwise, for context saver, conservatively use max number of threads
        resultDepth = std::max(resultDepth, syncCallerMaxThreadCount);
    }

    assert((resultDepth - resultAlmostFullSlots) >= minAlmostFullThreshold);

    // Honor the specified max depth
    if ((maxDepth != 0) && (resultDepth >= maxDepth))
    {
        resultDepth = maxDepth;
        resultAlmostFullSlots = 0; // almost_full will not be used
    }

    // For synchronous calls, and fifos that connect basic blocks in the same function
    // the max thread count of the caller bounds the required fifo depth
    if ((syncCallerMaxThreadCount < resultDepth) && (syncCallerMaxThreadCount >= minimumDepth))
    {
        resultDepth = syncCallerMaxThreadCount;
        resultAlmostFullSlots = 0;
    }
}

// Sets fifo sizes and almost_full values
void SizeFifos(Program& program)
{
    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

    // Assert that all write delays are 0 initially
    for (size_t fifoIndex = 0; fifoIndex < program._registerTable.size(); ++fifoIndex)
    {
        const RegisterDescription& regDesc = program._registerTable[fifoIndex];

        if (RegisterType::Fifo == regDesc._type)
        {
            assert(0 == regDesc.Fifo()._writeDelay);
        }
    }

    // This is the size needed to not waste MLAB space
    const size_t minimumConfigDepth = GetCodeGenDeviceConfig()._minFifoDepth;
    const size_t minimumDualClockConfigDepth = GetCodeGenDeviceConfig()._minDualClockFifoDepth;

    // Set write delays
    // Returns from sync externs use write delay because the interface from the extern to kanagawa
    // is ready/valid which does not allow the extern to pipeline the result
    for (Function* function : program._externFunctions)
    {
        const FunctionNode* functionNode = function->_functionNode;

        const bool isSync = !(functionNode->GetModifiers() & ParseTreeFunctionModifierAsync);
        assert(functionNode->GetModifiers() & ParseTreeFunctionModifierExternal);

        if (isSync)
        {
            for (const ExternReturnDesc& returnDesc : function->_syncExtern._returnDesc)
            {
                RegisterDescription& regDesc = program._registerTable[returnDesc._fifoIndex];

                assert(RegisterType::Fifo == regDesc._type);

                regDesc.Fifo()._writeDelay = GetCodeGenConfig()._fifoWriteDelay;
                regDesc.Fifo()._minWriteDelay = GetCodeGenConfig()._fifoWriteDelay;
            }
        }
    }

    std::set<size_t> fifosForGeneratedRtl;

    // For each enqueue operation
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            // This code assumes that _atomicSequence values start at 0 and increase by at most 1
            // Assert this
            AssertSequenceNumbers(basicBlock);

            // Auto pipelining can add pipeline stages
            size_t autoPipelineAdditionalStageCount = 0;

            // The almost_full signal for a fifo written by this basic block
            // might be checked by the predecessor block
            // in which case, the predecessor block length must be added
            size_t transitiveBackpressureAdditionalStageCount = 0;

            if (basicBlock._transitiveBackpressurePredecessor)
            {
                // + 1 to account for the 1 cycle to move from the predecessor to the successor block
                assert(!basicBlock._transitiveBackpressurePredecessor->_stages.empty());
                transitiveBackpressureAdditionalStageCount =
                    basicBlock._transitiveBackpressurePredecessor->_stages.back()._atomicSequence + 1;
            }

            for (Stage& stage : basicBlock._stages)
            {
                const size_t stageIndex = stage._atomicSequence;
                const size_t stageAdditionalFifoDepth =
                    SafeLookup(basicBlock._pipelineStageStallableMap, stageIndex)._stageAdditionalFifoAlmostFullEntries;

                for (Operation& op : stage._operations)
                {
                    if (Opcode::Enqueue == op._opcode)
                    {
                        const size_t fifoIndex = op._flags._enqueue._successorFifo;

                        fifosForGeneratedRtl.insert(fifoIndex);

                        RegisterDescription& regDesc = program._registerTable[fifoIndex];
                        assert(RegisterType::Fifo == regDesc._type);
                        assert(0 == regDesc.Fifo()._almostFullSlots);
                        assert(0 == regDesc.Fifo()._writeDelay); // would need to take this into account when computing
                                                                 // almost_full depth

                        size_t maxWriteDelay = 0;
                        size_t minWriteDelay = 0;

                        if (regDesc.Fifo()._implementation == FifoImplementation::CrossRegion &&
                            codeGenConfig._autoPipelineCrossRegion)
                        {
                            // Enable write-side pipelining for cross region return value FIFOs
                            maxWriteDelay = codeGenConfig._autoPipelineCrossRegion;

                            minWriteDelay = codeGenConfig.GetMinCrossRegionFifoWriteDelay();
                        }
                        else if (codeGenConfig._autoPipelineScale >
                                 0) // If command-line option to enable additional pipelining was set (default value is
                                    // 0, which means disabled)
                        {
                            if (GetCodeGenDeviceConfig()._supportsAutoPipelining &&
                                codeGenConfig._autoPipelineScale > GetCodeGenDeviceConfig()._minAutoPipelineDepth)
                            {
                                maxWriteDelay = codeGenConfig._autoPipelineScale;
                                minWriteDelay = GetCodeGenDeviceConfig()._minAutoPipelineDepth;
                            }
                            else
                            {
                                maxWriteDelay = codeGenConfig._autoPipelineScale;
                                minWriteDelay = codeGenConfig._autoPipelineScale;
                            }
                        }

                        const size_t minDepth = regDesc.Fifo()._depth;

                        // The caller function maximum thread can bound fifo size
                        // for synchronous calls, and cases where both basic blocks are in the same function
                        size_t callerMaxThreadCount = std::numeric_limits<size_t>::max();

                        switch (op._flags._enqueue._type)
                        {
                        case EnqueueType::Default:
                            if (op._getSuccessorBlock && op._getSuccessorBlock()->_function == &function)
                            {
                                callerMaxThreadCount = function._maxThreadCountInsideFunction;
                            }
                            break;

                        case EnqueueType::FunctionCall:
                            if (!op._getSuccessorBlock()->_function->IsAsync())
                            {
                                callerMaxThreadCount = function._maxThreadCountInsideFunction;
                            }
                            break;

                        case EnqueueType::ContextSaverCaller:
                            callerMaxThreadCount = function._maxThreadCountInsideFunction;
                            break;

                        default:
                            break;
                        }

                        // Both the user-specified call rate
                        // and the basic-block thread rate
                        // throttle how often enqueue operations can occur.
                        const size_t scaleFactor =
                            op._flags._enqueue._modifiers._callRate * basicBlock.GetThreadRateForFifoSizing();

                        ComputeFifoSize(
                            GetCodeGenDeviceConfig()._minFifoDepth,
                            GetCodeGenDeviceConfig()._minDualClockFifoDepth,
                            GetCodeGenDeviceConfig()._minAlmostFullDepth, codeGenConfig._additionalLatency,
                            minWriteDelay, maxWriteDelay, op._flags._enqueue._modifiers._minFifoDepth, scaleFactor,
                            regDesc.Fifo()._readClock, regDesc.Fifo()._writeClock,
                            stageAdditionalFifoDepth + transitiveBackpressureAdditionalStageCount,
                            regDesc.Fifo()._addMinimumDepth, minDepth, regDesc.Fifo()._maxDepth, callerMaxThreadCount,
                            regDesc.Fifo()._type == FifoType::ContextSaverCaller &&
                                regDesc.Fifo()._contextSaverCaller._hasTransactionSize,
                            regDesc.Fifo()._transactionSize, regDesc.Fifo()._depth, regDesc.Fifo()._almostFullSlots,
                            regDesc.Fifo()._writeDelay, regDesc.Fifo()._minWriteDelay);
                    }
                }
            }
        }
    }

    // Find fifos which are not fed by basic blocks and have not yet been configured
    // Also, reorder buffers are handled separately
    for (size_t fifoIndex = 0; fifoIndex < program._registerTable.size(); ++fifoIndex)
    {
        RegisterDescription& regDesc = program._registerTable[fifoIndex];

        if (RegisterType::Fifo == regDesc._type)
        {
            if (FifoType::FixedDelay == regDesc.Fifo()._type)
            {
                // fixed-delay fifos delay has already been computed
                assert(regDesc.Fifo()._depth > 0);
                continue;
            }

            switch (regDesc.Fifo()._implementation)
            {
            case FifoImplementation::Default:
            case FifoImplementation::InternalBuffer:
            {
                // Dual clock FIFOs have additional write to read latency, and therefore have a different minimum depth
                const size_t minimumDepth = (regDesc.Fifo()._readClock != regDesc.Fifo()._writeClock)
                                                ? minimumDualClockConfigDepth
                                                : minimumConfigDepth;

                if (fifosForGeneratedRtl.find(fifoIndex) == fifosForGeneratedRtl.end())
                {
                    assert(0 == regDesc.Fifo()._almostFullSlots);

                    // Context saver outputs require one almost full slot (for dual clock FIFOs, this may internally be
                    // increased)
                    regDesc.Fifo()._almostFullSlots = 1;

                    // The FIFOs used here automatically adjust almost full entries internally to accomodate
                    // the specified write delay, so we just need to ensure that the depth is sufficient.
                    const size_t minimumDepthForWriteDelayAndAlmostFull =
                        1 + regDesc.Fifo()._almostFullSlots + 2 * regDesc.Fifo()._writeDelay;

                    // Calculate depth accounting for write delay and almost_full as well as platform minimum FIFO depth
                    const size_t depth =
                        std::max(std::max(minimumDepthForWriteDelayAndAlmostFull, regDesc.Fifo()._depth), minimumDepth);

                    regDesc.Fifo()._depth = depth;
                }
            }
            break;

            case FifoImplementation::CrossRegion:
            {
                size_t minWriteDelay = 0;
                size_t maxWriteDelay = 0;

                if (codeGenConfig._autoPipelineCrossRegion > 0)
                {
                    if (GetCodeGenDeviceConfig()._supportsAutoPipelining &&
                        codeGenConfig._autoPipelineCrossRegion > GetCodeGenDeviceConfig()._minAutoPipelineDepth)
                    {
                        maxWriteDelay = codeGenConfig._autoPipelineCrossRegion;
                        minWriteDelay = GetCodeGenDeviceConfig()._minAutoPipelineDepth;
                    }
                    else
                    {
                        maxWriteDelay = codeGenConfig._autoPipelineCrossRegion;
                        minWriteDelay = codeGenConfig._autoPipelineCrossRegion;
                    }
                }

                regDesc.Fifo()._minWriteDelay = minWriteDelay;
                regDesc.Fifo()._writeDelay = maxWriteDelay;

                if (regDesc.Fifo()._depth == 0)
                {
                    assert(regDesc.Fifo()._almostFullSlots == 0);

                    regDesc.Fifo()._depth = 1ull << Log2RoundUp(std::max(minimumConfigDepth, 2 * maxWriteDelay));
                    regDesc.Fifo()._almostFullSlots = 0;
                }

                assert(regDesc.Fifo()._depth >= (2 * maxWriteDelay + regDesc.Fifo()._almostFullSlots));
            }
            break;

            case FifoImplementation::TwoRegister:
                // Two register fifos have depth = 2
                regDesc.Fifo()._depth = 2;
                regDesc.Fifo()._almostFullSlots = 0;
                break;

            default:
                assert(false);
            }

            if (FifoType::ReorderBuffer == regDesc.Fifo()._type)
            {
                // Reorder buffer hardware requires the logDepth to be the slotWidth - 1
                // It is OK to enforce this here because the slot width is at least twice as large
                // as the max thread count of the containing function
                // So setting logDepth = (slotWidth - 1) will never cause an overflow
                assert(regDesc.Fifo()._reorderBuffer._slotWidth > 0);
                regDesc.Fifo()._depth = 1ull << (regDesc.Fifo()._reorderBuffer._slotWidth - 1);
                regDesc.Fifo()._almostFullSlots = 0;
            }
        }
    }
}

void PlaceNodes(Program& program)
{
    // This should run before MergeFifos
    assert(program._fifoMergers.empty());

    Placement placement;

    std::map<const BasicBlock*, Placement::Node> basicBlockPlacementNodes;

    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            const Placement::Node basicBlockNode = placement.AddNode(GetBasicBlockName(basicBlock));

            basicBlockPlacementNodes[&basicBlock] = basicBlockNode;
        }
    }

    // Links

    // Maps register index to the set of nodes that read/write the register
    typedef std::set<Placement::Node> NodeSet;

    std::map<size_t, NodeSet> registerMap;

    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            const Placement::Node srcNode = SafeLookup(basicBlockPlacementNodes, &basicBlock);

            for (const Stage& stage : basicBlock._stages)
            {
                for (const Operation& op : stage._operations)
                {
                    for (const SourceOperand& srcOperand : op._src)
                    {
                        // Register reads cause globals/memories to be linked to basic blocks
                        if (SourceOperandType::Register == srcOperand.Type())
                        {
                            const size_t registerIndex = srcOperand.GetAccessedRegister()._registerIndex;

                            registerMap[registerIndex].insert(srcNode);
                        }
                    }

                    for (const DestinationOperand& dstOperand : op._dst)
                    {
                        // Register reads cause globals/memories to be linked to basic blocks
                        if (DestinationOperandType::Register == dstOperand.Type())
                        {
                            const size_t registerIndex = dstOperand.GetAccessedRegister()._registerIndex;

                            registerMap[registerIndex].insert(srcNode);
                        }
                    }

                    if (op._getSuccessorBlock)
                    {
                        // Enqueue operations link 2 basic blocks
                        const BasicBlock* const dstBlock = op._getSuccessorBlock();

                        const Placement::Node dstNode = SafeLookup(basicBlockPlacementNodes, dstBlock);

                        placement.AddEdge(srcNode, dstNode);
                    }
                }
            }
        }
    }

    // Add edges for globals/memories that are accessed by multiple basic blocks
    std::map<size_t, Placement::Node> registerPlacementNodes;

    std::set<NodeSet> allNodeSets;

    for (const auto& p : registerMap)
    {
        const size_t registerIndex = p.first;

        const RegisterDescription& regDesc = program._registerTable[registerIndex];

        switch (regDesc._type)
        {
        case RegisterType::Global:
        case RegisterType::Memory:
        {
            const std::set<Placement::Node>& nodeSet = p.second;

            // Ignore variables that are only accessed by 1 basic block
            if (nodeSet.size() > 1)
            {
                // If multiple registers have the same node sets, only add 1 of them
                // To avoid overwhelming the placement code
                if (allNodeSets.end() == allNodeSets.find(nodeSet))
                {
                    allNodeSets.insert(nodeSet);

                    const Placement::Node registerNode = placement.AddNode(regDesc._name);

                    registerPlacementNodes[registerIndex] = registerNode;

                    for (const Placement::Node& basicBlockNode : nodeSet)
                    {
                        placement.AddEdge(registerNode, basicBlockNode);
                    }
                }
            }
        }
        break;

        default:
            break;
        }
    }

    const size_t numResets = placement.Run(GetCodeGenConfig()._placementConfig);

    // Record positions of all basic blocks
    assert(program._basicBlockPositions.empty());

    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            const Placement::Node node = SafeLookup(basicBlockPlacementNodes, &basicBlock);

            SafeInsert<const BasicBlock*, Placement::Vec2>(program._basicBlockPositions, &basicBlock,
                                                           placement.GetNodePosition(node));
        }
    }

    // Record positions of all global registers
    assert(program._registerPositions.empty());

    for (const auto& p : registerMap)
    {
        const size_t registerIndex = p.first;
        const NodeSet& nodeSet = p.second;

        // Node position is set to the average of all connected basic blocks
        assert(!nodeSet.empty());

        Placement::Vec2 averagePosition(0.0f, 0.0f);

        for (const Placement::Node node : nodeSet)
        {
            const Placement::Vec2 nodePosition = placement.GetNodePosition(node);

            averagePosition = Placement::Vec2::Add(averagePosition, nodePosition);
        }

        averagePosition = Placement::Vec2::Scale(averagePosition, 1.0f / static_cast<float>(nodeSet.size()));

        SafeInsert<size_t, Placement::Vec2>(program._registerPositions, registerIndex, averagePosition);
    }
}

void SetFifoTransactionSizes(Program& program)
{
    const bool enableTransactionSizeWarning = GetCodeGenConfig()._enableTransactionSizeWarning;
    for (Function& function : program._functions)
    {
        // `function` is added by the compiler to handle
        // calls from external class callbacks
        // Do not emit transaction size warnings for calls originating from `function`
        // under the assumption that the external class properly uses store-and-forward
        // before calling the callback
        if (function._functionNode->GetModifiers() & ParseTreeFunctionModifierNoSrcTxWarning)
        {
            continue;
        }

        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            for (Stage& stage : basicBlock._stages)
            {
                for (Operation& op : stage._operations)
                {
                    if (op._getSuccessorBlock)
                    {
                        assert(Opcode::Enqueue == op._opcode);

                        RegisterDescription& regDesc = program._registerTable[op._flags._enqueue._successorFifo];

                        assert(RegisterType::Fifo == regDesc._type);
                        assert(FifoImplementation::Default == regDesc.Fifo()._implementation);
                        assert(0 == regDesc.Fifo()._transactionSize);

                        const size_t transactionSize = op._flags._enqueue._modifiers._transactionSize;

                        const BasicBlock* const dstBlock = op._getSuccessorBlock();

                        const Function* const dstFunction = dstBlock->_function;

                        const bool isFunctionCall = (op._flags._enqueue._type == EnqueueType::FunctionCall);

                        if (transactionSize > 0)
                        {
                            assert(isFunctionCall);

                            // Determine which bit is the end_transaction bit
                            const size_t endTransactionLocalRegister = dstFunction->GetEndTransactionRegister();

                            const size_t transactionBitOffset = SafeLookup<size_t, size_t>(
                                dstFunction->_start->_inputFifoRegisterMap, endTransactionLocalRegister);

                            regDesc.Fifo()._transactionBitOffset = transactionBitOffset;
                            regDesc.Fifo()._transactionSize = transactionSize;
                        }
                        else if (enableTransactionSizeWarning && dstFunction->HasEndTransactionParameter() && isFunctionCall)
                        {
                            Location loc = {};

                            if (!op._locations.empty())
                            {
                                loc = FileAndLineNumberToLocation(*op._locations.begin());
                            }

                            g_compiler->WarningStream(loc, CompileWarning::LastWithoutTransactionSize)
                                << "Call to function with [[last]] parameter without [[transaction_size]] at the call "
                                   "site";
                        }
                    }
                }
            }
        }
    }
}

void MergeFifos(Program& program)
{
    struct FifoRecord
    {
        size_t* _fifoIndexPointer;

        Function* _srcFunction;
        Function* _dstFunction;

        size_t _transactionSize;
        Location _location;
    };

    // Get a list of FifoRecords
    std::vector<FifoRecord> fifoRecords;

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            for (Stage& stage : basicBlock._stages)
            {
                for (Operation& op : stage._operations)
                {
                    if (op._getSuccessorBlock)
                    {
                        const RegisterDescription& outputRegDesc =
                            program._registerTable[op._flags._enqueue._successorFifo];
                        assert(RegisterType::Fifo == outputRegDesc._type);

                        FifoRecord record = {};
                        record._fifoIndexPointer = &op._flags._enqueue._successorFifo;
                        record._transactionSize = op._flags._enqueue._modifiers._transactionSize;
                        // Get first location for warning reporting
                        for (const FileAndLineNumber& faln : op._locations)
                        {
                            record._location = FileAndLineNumberToLocation(faln);
                            break;
                        }

                        const BasicBlock* const dstBlock = op._getSuccessorBlock();

                        record._srcFunction = &function;
                        record._dstFunction = dstBlock->_function;

                        fifoRecords.push_back(record);

                        // If this is a call site (the destination is the start of a function)
                        // Then increment the number of call sites for the destination function (for diagnostic output)
                        if (dstBlock == dstBlock->_function->_start)
                        {
                            dstBlock->_function->_numCallSites++;
                        }
                    }
                }
            }
        }
    }

    for (ContextSaver& contextSaver : program._contextSavers)
    {
        FifoRecord record = {};

        record._fifoIndexPointer = &contextSaver._destinationFifo;
        record._srcFunction = nullptr;
        record._dstFunction = contextSaver._afterCall->_function;

        fifoRecords.push_back(record);
    }

    // Count writes & the set of functions that each fifo is called from
    std::map<size_t, size_t> writeCount;
    std::map<size_t, std::set<Function*>> functionMap;

    for (const FifoRecord& fifoRecord : fifoRecords)
    {
        const size_t fifoIndex = *(fifoRecord._fifoIndexPointer);

        const auto it = writeCount.find(fifoIndex);

        if (it == writeCount.end())
        {
            writeCount[fifoIndex] = 1;
        }
        else
        {
            ++(it->second);
        }

        functionMap[fifoIndex].insert(fifoRecord._srcFunction);
    }

    std::map<size_t, FIFOMerger> fifoMergers;

    for (auto& p : writeCount)
    {
        const size_t destFifoIndex = p.first;

        const size_t writes = p.second;

        const auto it = functionMap.find(destFifoIndex);
        assert(it != functionMap.end());

        const std::set<Function*>& functionSet = it->second;
        assert(!functionSet.empty());

        if (writes > 1)
        {
            FIFOMerger merger = {};

            merger._dest = destFifoIndex;

            if (functionSet.size() == 1)
            {
                merger._singleSourceFunction = *(functionSet.begin());
            }
            else
            {
                merger._singleSourceFunction = nullptr;
            }

            merger._isTransactional = false; // set to true later

            fifoMergers[destFifoIndex] = merger;
        }
    }

    // Re-map writes
    for (FifoRecord& fifoRecord : fifoRecords)
    {
        const size_t fifoIndex = *(fifoRecord._fifoIndexPointer);

        const auto it = fifoMergers.find(fifoIndex);

        if (it != fifoMergers.end())
        {
            const RegisterDescription prevFifoDesc = program._registerTable[fifoIndex];

            assert(prevFifoDesc._type == RegisterType::Fifo);

            const std::string srcName = prevFifoDesc._name;

            const size_t width = prevFifoDesc._width;

            const size_t newFifoIndex = AllocateRegister(&program, width, RegisterType::Fifo, srcName);

            assert(prevFifoDesc.Fifo()._location.has_value());
            assert(prevFifoDesc.Fifo()._callStackIndex.has_value());

            program._registerTable[newFifoIndex].Fifo().SetLocation(*(prevFifoDesc.Fifo()._location),
                                                                    *(prevFifoDesc.Fifo()._callStackIndex));

            *(fifoRecord._fifoIndexPointer) = newFifoIndex;

            FIFOMerger& fifoMerger = it->second;

            // Copy the dest function information, this will match for all sources
            if (fifoMerger._sources.empty())
            {
                fifoMerger._dstFunction = fifoRecord._dstFunction;
            }
            else
            {
                assert(fifoMerger._dstFunction == fifoRecord._dstFunction);
            }

            fifoMerger._sources.push_back(newFifoIndex);
            fifoMerger._locations.push_back(&fifoRecord._location);
        }
    }

    // Add all fifo mergers to the program
    for (auto& p : fifoMergers)
    {
        FIFOMerger& merger = p.second;

        assert(merger._sources.size() > 1);

        // The start basic block of a function must have 1 input fifo
        assert(merger._dstFunction->_start->_inputFifoCount == 1);

        // Determine if the fifo merger needs to not interleave transactions
        // This happens when the destination fifo is the input fifo for the first basic block
        // and the function has an end_transaction parameter
        if (merger._dstFunction->HasEndTransactionParameter() &&
            (merger._dest == merger._dstFunction->_start->_inputFifoIndices[0]))
        {
            merger._isTransactional = true;

            // Determine which bit is the end_transaction bit
            const size_t endTransactionLocalRegister = merger._dstFunction->GetEndTransactionRegister();

            const size_t transactionBitOffset = SafeLookup<size_t, size_t>(
                merger._dstFunction->_start->_inputFifoRegisterMap, endTransactionLocalRegister);

            merger._transactionBitOffset = transactionBitOffset;
        }

        program._fifoMergers.push_back(merger);
    }

    // Determine the virtual placement location & display name for all FIFOs
    // FIFO location is set equal to the position of the node that enqueues into the FIFO
    std::map<size_t, Placement::Vec2> fifoPositionMap;
    std::map<size_t, std::string> fifoNameMap;

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            const Placement::Vec2 position = program.GetBasicBlockPosition(&basicBlock);

            for (Stage& stage : basicBlock._stages)
            {
                for (Operation& op : stage._operations)
                {
                    if (op._getSuccessorBlock)
                    {
                        SafeInsert<size_t, Placement::Vec2>(fifoPositionMap, op._flags._enqueue._successorFifo,
                                                            position);

                        SafeInsert<size_t, std::string>(fifoNameMap, op._flags._enqueue._successorFifo,
                                                        GetFunctionCombinedName(&function));
                    }
                }
            }
        }
    }

    for (ContextSaver& contextSaver : program._contextSavers)
    {
        const Placement::Vec2 position = program.GetBasicBlockPosition(contextSaver._beforeCall);

        SafeInsert<size_t, Placement::Vec2>(fifoPositionMap, contextSaver._destinationFifo, position);

        SafeInsert<size_t, std::string>(fifoNameMap, contextSaver._destinationFifo,
                                        GetFunctionCombinedName(contextSaver._afterCall->_function));
    }

    // Sort the sources for each of the fifoMergers so that they are in the same order as the command processor
    //  bus chain.  This should help the place and route tools place each data source node with it's associated
    //  chunk of logic, and the merge chain can follow the command processor bus around the chip.
    for (FIFOMerger& merger : program._fifoMergers)
    {
        // Sort the sources according to placement, to group sources which will be near each other
        std::list<size_t> sortedSources(merger._sources.begin(), merger._sources.end());

        std::ostringstream displayName;
        displayName << "FIFO merger: ";
        displayName << GetFunctionCombinedName(merger._dstFunction);

        const auto getPositionCallback = [&](const size_t fifoIndex)
        { return SafeLookup<size_t, Placement::Vec2>(fifoPositionMap, fifoIndex); };

        const auto getNameCallback = [&](const size_t fifoIndex)
        { return SafeLookup<size_t, std::string>(fifoNameMap, fifoIndex); };

        const Placement::SortRecord sortRecord =
            Placement::Sort<size_t>(sortedSources, getPositionCallback, getNameCallback,
                                    GetCodeGenConfig()._placementConfig._display, displayName.str());

        program._sortRecords.push_back(sortRecord);

        merger._sources = std::vector<size_t>(sortedSources.begin(), sortedSources.end());
    }
}

// Tracks registers in a basic block that have constant or duplicate values
// Builds an encoding table that is stored in the IR along with FIFOs
// This is used to narrow the FIFO width
class FifoCodeBuilder
{
  public:
    FifoCodeBuilder(Program& program) : _program(program) {}

    void ProcessOperation(const Operation& op)
    {
        const bool optimizing = GetCodeGenConfig()._optimize > 0;

        // All registers in _registerMap should be pipeline registers or wires
        // and only have 1 write location

        // Detect moves to a local register
        // Sign extension is not supported
        const bool isMove = (Opcode::Mov == op._opcode) || (Opcode::MovCrossFunction == op._opcode);

        if (optimizing && isMove && OperationUsesOnlyLocalReg(_program, op) && (op._signExtendSourceMask == 0) &&
            (DestinationOperandType::Register == op._dst[0].Type()))
        {
            assert(1 == op._dst.size());
            assert(1 == op._src.size());

            const size_t dstReg = op._dst[0].GetAccessedRegister()._registerIndex;

            const SourceOperand& srcOp = op._src[0];

            if (SourceOperandType::Literal == srcOp.Type())
            {
                SafeInsert(_registerMap, dstReg, srcOp);
            }
            else if (SourceOperandType::Register == srcOp.Type())
            {
                const size_t srcReg = srcOp.GetAccessedRegister()._registerIndex;

                // Only optimize when source and dest widths match
                // To not worry about sign extension/truncation
                if (_program._registerTable[srcReg]._width == _program._registerTable[dstReg]._width)
                {
                    // See if the source register is already in the table
                    const auto it = _registerMap.find(srcReg);

                    if (it == _registerMap.end())
                    {
                        SafeInsert(_registerMap, dstReg, srcOp);
                    }
                    else
                    {
                        // Map to the register that is already in the table
                        SafeInsert(_registerMap, dstReg, it->second);
                    }
                }
            }
        }
    }

    void BeginEncode()
    {
        FifoCode fc = {};
        _fifoCode = fc;

        _encodeMap.clear();

        _physicalBitsEncoded = 0;

        _logicalBitsEncoded = 0;
    }

    FifoCode EndEncode()
    {
        assert(!_fifoCode._initialized);
        _fifoCode._initialized = true;

        assert(_fifoCode._encodedWidth == 0);
        _fifoCode._encodedWidth = _physicalBitsEncoded;

        // Build input ranges from output ranges
        assert(_fifoCode._inputRanges.empty());

        std::set<size_t> coveredRanges;

        for (const FifoCodeOutputRange& outputRange : _fifoCode._outputRanges)
        {
            if (!outputRange._isLiteral)
            {
                // When multiple logical ranges map to the same physical range
                // Only include one input range.  The rest are known to have the
                // same values and can be dropped
                if (coveredRanges.end() != coveredRanges.find(outputRange._encodedOffset))
                {
                    continue;
                }

                coveredRanges.insert(outputRange._encodedOffset);

                FifoCodeInputRange inputRange = {};

                inputRange._width = outputRange._width;
                inputRange._decodedOffset = outputRange._decodedOffset;
                inputRange._encodedOffset = outputRange._encodedOffset;

                _fifoCode._inputRanges.push_back(inputRange);
            }
        }

        return _fifoCode;
    }

    boost::optional<Literal> EncodeReg(BasicBlock& bb, Stage& stage, const size_t r)
    {
        boost::optional<Literal> result;

        const auto it = _registerMap.find(r);

        const size_t width = _program._registerTable[r]._width;

        FifoCodeOutputRange outputRange = {};

        outputRange._width = width;
        outputRange._decodedOffset = _logicalBitsEncoded;

        if (it == _registerMap.end())
        {
            // Nothing is known about this register
            outputRange._isLiteral = false;
            outputRange._encodedOffset = _physicalBitsEncoded;

            _physicalBitsEncoded += width;
        }
        else
        {
            const SourceOperand& srcOp = it->second;

            if (SourceOperandType::Literal == srcOp.Type())
            {
                // No need to store this value into the fifo
                // It is known to be constant
                outputRange._isLiteral = true;
                outputRange._literal = Truncate(srcOp.GetLiteral()._value, width);

                result = Literal{outputRange._literal, width};
            }
            else
            {
                assert(SourceOperandType::Register == srcOp.Type());

                // There can be many entries in _registerMap that all map to trueSourceReg
                const size_t trueSourceReg = srcOp.GetAccessedRegister()._registerIndex;

                // To replace one register with another, the widths must match
                assert(width == _program._registerTable[trueSourceReg]._width);

                // Check if something equal to trueSourceReg is already in the
                // fifo encoding
                const auto encodeIt = _encodeMap.find(trueSourceReg);
                if (encodeIt == _encodeMap.end())
                {
                    // First time a register equal to trueSourceReg is
                    // placed into the fifo
                    outputRange._isLiteral = false;
                    outputRange._encodedOffset = _physicalBitsEncoded;

                    SafeInsert(_encodeMap, trueSourceReg, std::pair<size_t, size_t>(r, _physicalBitsEncoded));

                    _physicalBitsEncoded += width;
                }
                else
                {
                    const size_t offset = encodeIt->second.second;

                    // found it, no need to store this value again
                    outputRange._isLiteral = false;
                    outputRange._encodedOffset = offset;

                    // Add an assertion to the IR that will check that the duplicate
                    // register has the same value as trueSourceReg
                    const AccessedRegister eqReg = {
                        AllocateRegister(&_program, 1, RegisterType::Wire, "FifoEncodeRegEq")};

                    Operation assertOp = {};
                    {
                        assertOp._opcode = Opcode::Assert;

                        std::string* errorMessage =
                            g_compiler->Create<std::string>("Fifo input register value doesn't match expectations");

                        // There is no reasonable location to associated with the assert
                        assertOp._expectNoSourceLocation = true;

                        assertOp._flags._assertion._message = errorMessage->c_str();
                        assertOp._flags._assertion._caseEQ = true;

                        const AccessedRegister src0Ar = {r};
                        const AccessedRegister src1Ar = {
                            encodeIt->second
                                .first}; // index of first duplicate register (the one that wrote into he fifo)

                        assertOp._src.push_back(src0Ar);
                        assertOp._src.push_back(src1Ar);
                    }

                    // push_front is used so that jump is the last operation in a stage
                    stage._operations.push_front(assertOp);
                }
            }
        }

        _fifoCode._outputRanges.push_back(outputRange);

        _logicalBitsEncoded += width;

        return result;
    }

  private:
    Program& _program;

    // Map register index to SourceOperand that it is equivalent to
    std::map<size_t, SourceOperand> _registerMap;

    FifoCode _fifoCode;

    // maps register index to (index of register that was moved into the fifo, offset in fifo)
    std::map<size_t, std::pair<size_t, size_t>> _encodeMap;

    size_t _physicalBitsEncoded;

    size_t _logicalBitsEncoded;
};

// Add move operations to a stage
// to copy registers from 1 basic block to another
void CopyToFifo(Program& program, BasicBlock& basicBlock, Stage& stage, const size_t dstFifoIndex,
                const FifoOffsetMap& destFifoOffsetMap, const LocalToPipelineRegisterMap& sourceRegisterIndexMap,
                const RegisterIndexMap& originalToRenamedMap, FifoCodeBuilder& fcb,
                const std::set<FileAndLineNumber>& locations)
{
    fcb.BeginEncode();

    // If the destination fifo does not directly feed a basic block
    // then it will not have any named ranges.
    // Add them here
    const bool addNamedRanges = program._registerTable[dstFifoIndex].Fifo()._namedRanges.empty();

    // For each local variable that is live-in at the destination
    for (const auto& p : destFifoOffsetMap)
    {
        // Convert p.first from un-renamed register index
        // to the renamed register index in the source basic block
        const size_t localRegisterIndex = OptionallyMapIndex(p.first, originalToRenamedMap);
        const size_t dstOffset = p.second;

        // Lookup the register in the source
        const auto it = sourceRegisterIndexMap.find(localRegisterIndex);

        if (it != sourceRegisterIndexMap.end())
        {
            const boost::optional<Literal> literalValue = fcb.EncodeReg(basicBlock, stage, it->second);

            const size_t bitWidth = program._registerTable[localRegisterIndex]._width;

            const FifoSubset dstFifo = {dstFifoIndex, dstOffset, bitWidth};

            Operation movOp;

            movOp._locations = locations;

            movOp._opcode = Opcode::Mov;

            if (literalValue)
            {
                // fcb determined that this value is a compile-time known const
                // ignore the register (it will be optimized away later)
                movOp._src.push_back(*literalValue);
            }
            else
            {
                const AccessedRegister srcRegister = {it->second};

                movOp._src.push_back(srcRegister);
            }

            movOp._dst.push_back(dstFifo);

            // Operations are added to the beginning of the stage so that the Jump is at the end of the stage
            stage._operations.push_front(movOp);

            if (addNamedRanges)
            {
                program._registerTable[dstFifoIndex].Fifo().AddNamedRange(
                    dstOffset, bitWidth, program._registerTable[localRegisterIndex]._name);
            }
        }
        else
        {
            // For predicated calls, there is an enqueue from the call site to the return site
            // (in case the call is predicated away)
            // in this case, there is nothing to write into the FIFO at the call site
            // This is ok because the return value is not used in this case
            const RegisterDescription& localRegDesc = program._registerTable[localRegisterIndex];

            assert(localRegDesc.Local()._canSkipFifoWrites);
        }
    }

    // Store a codebook that can be used to narrow the fifo width
    FifoCode& dstCode = program._registerTable[dstFifoIndex].Fifo()._code;

    assert(!dstCode._initialized);
    assert(dstCode._encodedWidth == 0);
    assert(dstCode._inputRanges.empty());
    assert(dstCode._outputRanges.empty());

    dstCode = fcb.EndEncode();
}

// Called after pipelining
// Adds writes to FIFOs associated with each Enqueue
void AddFifoWrites(Program& program, BasicBlock& basicBlock)
{
    // Build a list of basic blocks that are points where a function call returns
    std::unordered_map<const BasicBlock*, const ContextSaver*> returnSites;

    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        returnSites[contextSaver._afterCall] = &contextSaver;
    }

    FifoCodeBuilder fcb(program);

    for (Stage& stage : basicBlock._stages)
    {
        for (Operation& op : stage._operations)
        {
            fcb.ProcessOperation(op);

            // Note that for the return from an export function, the opcode will be enqueue, but _getSuccessorBlock will
            // be null
            if (op._getSuccessorBlock)
            {
                const BasicBlock* const dstBasicBlock = op._getSuccessorBlock();

                // RemoveDeadJumps assures this
                assert(!dstBasicBlock->_stages.empty());

                const LocalToPipelineRegisterMap& sourceRegisterIndexMap = stage._beforeStageRegisterMap;

                switch (op._flags._enqueue._type)
                {
                case EnqueueType::ContextSaverCallee:
                case EnqueueType::ContextSaverCaller:
                {
                    const auto it = returnSites.find(dstBasicBlock);
                    assert(it != returnSites.end());

                    // The destination is a return site
                    // There are 2 basic blocks that supply values for the destination
                    // via a context saver
                    const ContextSaver* contextSaver = it->second;

                    const FifoOffsetMap& registerOffsetMap =
                        (EnqueueType::ContextSaverCallee == op._flags._enqueue._type) ? contextSaver->_calleeInputMap
                                                                                      : contextSaver->_callerInputMap;

                    // Emit move operations
                    CopyToFifo(program, basicBlock, stage, op._flags._enqueue._successorFifo, registerOffsetMap,
                               sourceRegisterIndexMap, op._renamingTable, fcb, op._locations);
                }
                break;

                case EnqueueType::FunctionCall:
                case EnqueueType::Default:
                case EnqueueType::ReorderBuffer:
                {
                    const FifoOffsetMap& destRegisterOffsetMap = dstBasicBlock->_inputFifoRegisterMap;

                    const size_t dstFifoIndex = op._flags._enqueue._successorFifo;

                    // Emit move operations
                    CopyToFifo(program, basicBlock, stage, dstFifoIndex, destRegisterOffsetMap, sourceRegisterIndexMap,
                               op._renamingTable, fcb, op._locations);
                }
                break;

                default:
                    assert(false);
                }
            }
        }
    }
}

// Returns false if any source operand is non-local
bool OperationUsesOnlyLocalSrcReg(const Program& program, const Operation& op)
{
    for (const SourceOperand& srcOp : op._src)
    {
        if (SourceOperandType::Register == srcOp.Type())
        {
            const size_t srcReg = srcOp.GetAccessedRegister()._registerIndex;

            if (!IsLocalRegisterType(program._registerTable[srcReg]._type))
            {
                return false;
            }
        }
    }

    return true;
}

// Returns false if any destination operand is non-local
bool OperationUsesOnlyLocalDstReg(const Program& program, const Operation& op)
{
    for (const DestinationOperand& dstOp : op._dst)
    {
        if (DestinationOperandType::Register == dstOp.Type())
        {
            const size_t dstReg = dstOp.GetAccessedRegister()._registerIndex;

            if (!IsLocalRegisterType(program._registerTable[dstReg]._type))
            {
                return false;
            }
        }
    }

    return true;
}

bool OperationUsesOnlyLocalReg(const Program& program, const Operation& op)
{
    return OperationUsesOnlyLocalSrcReg(program, op) && OperationUsesOnlyLocalDstReg(program, op);
}

// Verifies that [[reset]] functions cannot call an external functions
void CheckExports(IRContext& context)
{
    for (Function& function : context._program->_functions)
    {
        if (function.CallOnReset())
        {
            // Get all other functions reachable from this function
            std::set<Function*> reachableFunctions;

            reachableFunctions.insert(&function);

            // 3rd parameter is false because it is OK for [[reset]] functions to call async functions
            // which then call externs
            GetReachableFunctions(*(context._program), reachableFunctions, false);

            for (const Function* const calledFunction : reachableFunctions)
            {
                if (calledFunction->_functionNode->GetModifiers() & ParseTreeFunctionModifierExternal)
                {
                    g_compiler->ErrorStream(function._functionNode->GetLocation(), CompileError::InvalidCall)
                        << "Functions with the [[reset]] attribute must not call external functions.  "
                        << function._name << " calls: " << calledFunction->_name;
                }
            }
        }
    }
}

// Rewrite all read-after-write patterns for global variables in an atomic block.
// Every global variable will be paired with a predicate and a local variable.
// The former indicate whether the global variable is written in the atomic block,
// and the later stores its value if it is written.
static void RewriteGlobalAccessesInAtomic(Program& program, BasicBlock& basicBlock, OperationList::iterator beginAtomic,
                                          OperationList::iterator endAtomic, const RegisterSet& readGlobals,
                                          const RegisterSet& writtenGlobals,
                                          const std::vector<OperationList::iterator>& readOperations,
                                          const std::vector<OperationList::iterator>& writeOperations)
{
    struct ShadowRegisters
    {
        size_t _predicate;
        size_t _value;
        size_t _firstWriteValue;

        ShadowRegisters()
            : _predicate(c_invalidAccessedRegisterIndex), _value(c_invalidAccessedRegisterIndex),
              _firstWriteValue(c_invalidAccessedRegisterIndex)
        {
        }

        ShadowRegisters(const size_t p, const size_t v, const size_t fv)
            : _predicate(p), _value(v), _firstWriteValue(fv)
        {
        }
    };

    const bool startCondition = (endAtomic->_opcode == Opcode::StartCondition);
    const bool stallCheck = (endAtomic->_opcode == Opcode::StallCheck) && endAtomic->_flags._stallCheck._globalsAfter;
    auto& operations = (startCondition || stallCheck) ? basicBlock._startConditionOperations : basicBlock._operations;
    auto insertShadowsBefore = (startCondition || stallCheck) ? beginAtomic : std::next(beginAtomic);

    std::map<size_t, ShadowRegisters> shadows; // maps a global to a predicate and a local

    // count the number of write operations to each global
    std::map<size_t, size_t> writeCountMap;

    for (OperationList::iterator writeOp : writeOperations)
    {
        assert(writeOp->_dst.size() == 1);
        const size_t g = writeOp->_dst[0].GetAccessedRegister()._registerIndex;

        writeCountMap[g]++;
    }

    const auto allocateShadow = [&](const size_t g)
    {
        const RegisterDescription gDesc = program._registerTable[g];

        const size_t pred = AllocateRegister(&program, 1, RegisterType::Local, gDesc._name);
        Operation initPred = {};
        initPred._opcode = Opcode::Mov;
        initPred._locations = beginAtomic->_locations;
        initPred._src.emplace_back(0);
        initPred._dst.emplace_back(AccessedRegister{pred});
        operations.insert(insertShadowsBefore, initPred);

        const size_t localRegister = AllocateRegister(&program, gDesc._width, RegisterType::Local, gDesc._name);
        Operation loadGlobalVar = {};
        loadGlobalVar._opcode = Opcode::Mov;
        loadGlobalVar._locations = beginAtomic->_locations;
        loadGlobalVar._src.emplace_back(AccessedRegister{g});
        loadGlobalVar._dst.emplace_back(AccessedRegister{localRegister});
        operations.insert(insertShadowsBefore, loadGlobalVar);

        size_t firstWriteValue = c_invalidAccessedRegisterIndex;

        const auto it = writeCountMap.find(g);

        if ((it != writeCountMap.end()) && (it->second == 1))
        {
            // There is only 1 write to the global variable
            // Allocate a special register that will hold the value written by the first write operation
            firstWriteValue = AllocateRegister(&program, gDesc._width, RegisterType::Local, gDesc._name);
        }

        shadows[g] = ShadowRegisters(pred, localRegister, firstWriteValue);
    };

    for (const size_t g : writtenGlobals)
    {
        allocateShadow(g);
    }

    // Create shadow registers for globals that are only read
    // Basic block scheduling assumes this
    // (it schedules the read in the first pipeline stage, which may not be feasible for operations other than mov which
    // have other source operands)
    for (const size_t g : readGlobals)
    {
        if (shadows.end() == shadows.find(g))
        {
            // This will allocate a predicate register, which will be optimized away because it is never referenced
            allocateShadow(g);
        }
    }

    for (OperationList::iterator writeOp : writeOperations)
    {
        assert(writeOp->_dst.size() == 1);
        const size_t g = writeOp->_dst[0].GetAccessedRegister()._registerIndex;

        if (writtenGlobals.count(g) > 0)
        {
            if (1 == SafeLookup(writeCountMap, g))
            {
                // This is the one and only write to the global
                // Save the value to write into a separate variable
                // The WriteGlobal operation emitted at the end of the atomic block
                // will source from this operand directly, no need to use the result of the select operation
                const size_t dstReg = shadows[g]._firstWriteValue;
                assert(dstReg != c_invalidAccessedRegisterIndex);

                Operation saveFirstWriteOp = {};

                saveFirstWriteOp._opcode = Opcode::Mov;
                saveFirstWriteOp._locations = beginAtomic->_locations;
                saveFirstWriteOp.PushOperand(*writeOp, 0);
                saveFirstWriteOp._dst.push_back(AccessedRegister{dstReg});

                operations.insert(writeOp, saveFirstWriteOp);
            }

            if (writeOp->_src.size() == 1)
            {
                // Rewrite
                //    Mov  dst          src
                // as
                //    Mov  shadow_pred  1
                //    Mov  shadow_dst   src
                Operation setPred = {};
                setPred._opcode = Opcode::Mov;
                setPred._locations = beginAtomic->_locations;
                setPred._src.assign(1, 1);
                setPred._dst.assign(1, AccessedRegister{shadows[g]._predicate});
                operations.insert(writeOp, setPred);

                writeOp->_opcode = Opcode::Mov;
                writeOp->_dst[0] = AccessedRegister{shadows[g]._value};
            }
            else
            {
                // Rewrite
                //    Mov     dst          src pred
                // as
                //    Select  shadow_pred  pred shadow_pred 1
                //    Select  shadow_dst   pred shadow_dst src
                assert(writeOp->_src.size() == 2);
                const AccessedRegister pred = writeOp->_src[1].GetAccessedRegister();
                Operation setPred = {};
                setPred._opcode = Opcode::Select;
                setPred._locations = beginAtomic->_locations;
                setPred._src.resize(3);
                setPred._src[2] = 1;
                setPred._src[1] = AccessedRegister{shadows[g]._predicate};
                setPred._src[0] = pred;
                setPred._dst.emplace_back(AccessedRegister{shadows[g]._predicate});
                operations.insert(writeOp, setPred);

                writeOp->_opcode = Opcode::Select;
                writeOp->_locations = beginAtomic->_locations;
                writeOp->_flags._writeGlobal._isPredicated = false;
                assert(writeOp->_signExtendSourceMask == 0 || writeOp->_signExtendSourceMask == 1);
                writeOp->_signExtendSourceMask <<=
                    2; // The source is moved from position 0 to position 2, so is its signed extension.
                writeOp->_src.resize(3);
                writeOp->_src[2] = writeOp->_src[0];
                writeOp->_src[1] = AccessedRegister{shadows[g]._value};
                writeOp->_src[0] = pred;
                writeOp->_dst[0] = AccessedRegister{shadows[g]._value};
            }
        }
    }

    for (OperationList::iterator readOp : readOperations)
    {
        for (SourceOperand& sourceOp : readOp->_src)
        {
            if (sourceOp.Type() == SourceOperandType::Register)
            {
                auto it = shadows.find(sourceOp.GetAccessedRegister()._registerIndex);

                if (it != shadows.end())
                {
                    sourceOp = AccessedRegister{it->second._value};
                }
            }
        }
    }

    // For StallCheck and StartCondition, insert writes afterwards
    auto insertWritesAfter = stallCheck || (startCondition && !endAtomic->_flags._startCondition._globalsBefore)
                                 ? std::next(endAtomic)
                                 : endAtomic;

    for (const size_t r : writtenGlobals)
    {
        const size_t srcReg = (1 == SafeLookup(writeCountMap, r)) ? shadows[r]._firstWriteValue : shadows[r]._value;

        // Update global write predicate with stall condition
        size_t predReg = shadows[r]._predicate;
        if (stallCheck)
        {
            const RegisterDescription& regDesc = program._registerTable[predReg];

            const size_t predWithStallCheckReg =
                AllocateRegister(&program, 1, RegisterType::Local, regDesc._name + "_and_stall_compare");
            Operation andOp = {};
            andOp._locations = beginAtomic->_locations;
            andOp._opcode = Opcode::BinaryOp;
            andOp._flags._binaryOpType = ParseTreeBinaryOpTypeAnd;
            andOp._src.push_back(AccessedRegister{predReg});
            andOp._src.push_back(endAtomic->_dst[0].GetAccessedRegister());
            andOp._dst.push_back(AccessedRegister{predWithStallCheckReg});
            operations.insert(insertWritesAfter, andOp);

            predReg = predWithStallCheckReg;
        }

        Operation writeGlobalVar = {};
        writeGlobalVar._opcode = Opcode::WriteGlobal;
        writeGlobalVar._locations = beginAtomic->_locations;
        writeGlobalVar._flags._writeGlobal._isPredicated = true;
        writeGlobalVar._src.emplace_back(AccessedRegister{srcReg});
        writeGlobalVar._src.emplace_back(AccessedRegister{predReg});
        writeGlobalVar._dst.emplace_back(AccessedRegister{r});
        operations.insert(insertWritesAfter, writeGlobalVar);
    }
}

// Verifies:
// No control-flow within an atomic
//
// Modifies:
// Adjust atomic update rate based on function thread rate
// Rewrite global reads into local shadow register at start of atomic or non-leaf atomic section
// Rewrite global writes from local shadow register at end of atomic or non-leaf atomic section
//
// Called before scheduling
void CheckAndRewriteAtomics(IRContext& context)
{
    RegisterSet readGlobals;
    RegisterSet writtenGlobals;
    std::vector<OperationList::iterator> writeOperations;
    std::vector<OperationList::iterator> readOperations;

    Program& program = *context._program;

    for (Function& function : context._program->_functions)
    {
        Location errorLocation = function._functionNode->GetLocation();

        std::stack<AtomicBlockDesc> atomicBlockDescStack;

        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            const auto walkOperationList = [&](OperationList& operations, bool startConditionOps)
            {
                // Start of region to perform global access rewrite
                OperationList::iterator rewriteStartIt = operations.begin();

                for (auto it = operations.begin(); it != operations.end(); ++it)
                {
                    const Operation& op = *it;

                    const bool stallCheck = (op._opcode == Opcode::StallCheck) && op._flags._stallCheck._globalsAfter;

                    // StartCondition or Begin/EndAtomic boundary in an atomic
                    if (op._opcode == Opcode::StartCondition || stallCheck ||
                        ((op._opcode == Opcode::BeginAtomic || op._opcode == Opcode::EndAtomic) &&
                         !atomicBlockDescStack.empty()))
                    {
                        // If globals accessed
                        if (writtenGlobals.size() + readGlobals.size() > 0)
                        {
                            // Perform rewrite
                            RewriteGlobalAccessesInAtomic(program, basicBlock, rewriteStartIt, it, readGlobals,
                                                          writtenGlobals, readOperations, writeOperations);
                        }
                    }

                    if (op._opcode == Opcode::BeginAtomic || op._opcode == Opcode::EndAtomic || stallCheck)
                    {
                        // Reset the sets of read/written global registers on atomic boundaries
                        // Reset after stallcheck rewrite
                        readGlobals.clear();
                        writtenGlobals.clear();

                        writeOperations.clear();
                        readOperations.clear();

                        // Update start of rewrite section
                        rewriteStartIt = it;
                    }

                    if (op._opcode == Opcode::BeginAtomic)
                    {
                        atomicBlockDescStack.push(op._flags._atomicBlockDesc);
                    }
                    else if (op._opcode == Opcode::EndAtomic)
                    {
                        assert(!atomicBlockDescStack.empty());
                        assert(atomicBlockDescStack.top()._type == op._flags._atomicBlockDesc._type);
                        atomicBlockDescStack.pop();
                    }
                    else if (Opcode::Enqueue == op._opcode)
                    {
                        if (!atomicBlockDescStack.empty() && !op._flags._enqueue._allowedInAtomic)
                        {
                            g_compiler->ErrorStream(errorLocation, CompileError::InvalidAtomic)
                                << "Control-flow operations within atomic blocks are not supported";

                            throw std::runtime_error("Control-flow operations within atomic blocks are not supported");
                        }
                    }
                    else if (!OpcodeAllowedInAtomicBlock(op._opcode))
                    {
                        if (!atomicBlockDescStack.empty())
                        {
                            g_compiler->ErrorStream(errorLocation, CompileError::InvalidAtomic)
                                << "Control-flow, fan_out, DSP, and memory operations within atomic blocks are not "
                                   "supported";

                            throw std::runtime_error("Control-flow, fan_out, DSP, and memory operations within atomic "
                                                     "blocks are not supported");
                        }
                    }

                    if (!atomicBlockDescStack.empty() || startConditionOps)
                    {
                        const bool containsAtomic =
                            !atomicBlockDescStack.empty() && atomicBlockDescStack.top()._containsAtomic;

                        for (const SourceOperand& sourceOp : op._src)
                        {
                            if (sourceOp.Type() == SourceOperandType::Register)
                            {
                                const AccessedRegister& reg = sourceOp.GetAccessedRegister();

                                const RegisterDescription& regDesc = program._registerTable[reg._registerIndex];

                                if (regDesc._type == RegisterType::Global)
                                {
                                    // Don't re-write source operands for LoadMemory operations
                                    // Where the source operand has not been written before the LoadMemory operation
                                    // This enables LoadMemory with global addresses to work correctly
                                    if ((op._opcode != Opcode::LoadMemory) ||
                                        (writtenGlobals.end() != writtenGlobals.find(reg._registerIndex)))
                                    {
                                        readGlobals.insert(reg._registerIndex);
                                        readOperations.push_back(it);
                                    }
                                }
                            }
                        }

                        for (const DestinationOperand& destOp : op._dst)
                        {
                            const AccessedRegister& reg = destOp.GetAccessedRegister();

                            if (program._registerTable[reg._registerIndex]._type == RegisterType::Global)
                            {
                                // Add this write to the set of written globals within this atomic block
                                writtenGlobals.insert(reg._registerIndex);
                                writeOperations.push_back(it);
                            }
                        }
                    }
                }
            };

            readGlobals.clear();
            writtenGlobals.clear();

            writeOperations.clear();
            readOperations.clear();

            walkOperationList(basicBlock._startConditionOperations, true /* startConditionOps */);
            walkOperationList(basicBlock._operations, false /* startConditionOps */);

            if (!atomicBlockDescStack.empty())
            {
                g_compiler->ErrorStream(errorLocation, CompileError::InvalidAtomic)
                    << "Basic block ended with outstanding atomics - function calls are not "
                       "allowed in atomic blocks";

                throw std::runtime_error("Basic block ended with outstanding atomics - function calls "
                                         "are not allowed in atomic blocks");
            }
        }
    }
}

void SetCanSkipFifoWrites(Program& program)
{
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            for (const size_t registerIndex : basicBlock._liveInReg)
            {
                RegisterDescription& regDesc = program._registerTable[registerIndex];

                assert(RegisterType::Local == regDesc._type);

                regDesc.Local()._canSkipFifoWrites = true;
            }
        }
    }
}

// Called after scheduling and optionally fifo sizing
// Finds per-function semaphores that will never reach their limit
// and removes them
void RemoveSemaphores(IRContext& context, const bool afterFifoSizing)
{
    Program& program = *context._program;

    for (Function& function : program._functions)
    {
        size_t maxThreadsInFunction = 0;

        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            // There can be a thread at each pipeline stage
            const std::vector<PipelineStage> pipelineStages = GetPipelineStages(const_cast<BasicBlock&>(basicBlock));

            maxThreadsInFunction += pipelineStages.size();

            for (const Stage& stage : basicBlock._stages)
            {
                for (const Operation& op : stage._operations)
                {
                    if ((Opcode::Enqueue == op._opcode) && op._getSuccessorBlock)
                    {
                        const BasicBlock* const destBb = op._getSuccessorBlock();

                        if (destBb->_function == &function)
                        {
                            // Enqueue from this function to this function

                            if (afterFifoSizing)
                            {
                                // There can be a thread at each slot in the fifo
                                const size_t fifoIndex = op._flags._enqueue._successorFifo;

                                const RegisterDescription& fifoDesc = program._registerTable[fifoIndex];
                                assert(RegisterType::Fifo == fifoDesc._type);

                                maxThreadsInFunction += fifoDesc.Fifo()._depth;
                            }
                            else
                            {
                                // FIFO sizes are not yet known, set maxThreadsInFunction such that semaphores will not
                                // be removed
                                maxThreadsInFunction = (function._maxThreadCountInsideFunction + 1);
                            }
                        }
                    }
                }
            }
        }

        if (maxThreadsInFunction <= function._maxThreadCountInsideFunction)
        {
            // The limit can never be reached - per-function semaphore is not necessary

            // The only semaphore should be the per-function semaphore
            assert(function._semaphores.size() < 2);

            for (BasicBlock& basicBlock : function._basicBlocks)
            {
                // Even though this runs after scheduling
                // {Acquire,Release}Semaphore only appear in unscheduled operations
                basicBlock._operations.remove_if(
                    [&](const Operation& op)
                    {
                        bool result = false;

                        switch (op._opcode)
                        {
                        case Opcode::AcquireSemaphore:
                        case Opcode::ReleaseSemaphore:
                            // the only semaphores acquired should be one associated with the function
                            assert(function._semaphores.end() != std::find(function._semaphores.begin(),
                                                                           function._semaphores.end(),
                                                                           op._flags._semaphoreIndex));
                            result = true;
                            break;

                        default:
                            break;
                        }

                        return result;
                    });
            }

            function._semaphores.clear();
        }
    }
}

// Called after scheduling
// Counts the number of writes to each global
// and assign a write port to each one
void AssignGlobalWritePorts(Program& program)
{
    for (RegisterDescription& regDesc : program._registerTable)
    {
        if (RegisterType::Global == regDesc._type)
        {
            assert(0 == regDesc.Global()._writeCount);
            assert(regDesc.Global()._literalValues.empty());
        }
    }

    const auto callback = [&](Operation& op)
    {
        for (DestinationOperand& dstOp : op._dst)
        {
            if (DestinationOperandType::Register == dstOp.Type())
            {
                RegisterDescription& regDesc = program._registerTable[dstOp.GetAccessedRegister()._registerIndex];

                if (RegisterType::Global == regDesc._type)
                {
                    assert(Opcode::WriteGlobal == op._opcode);

                    RegisterDescription::GlobalDesc& globalDesc = regDesc.Global();

                    const size_t writeIndex = globalDesc._writeCount;

                    ++globalDesc._writeCount;

                    dstOp.SetWriteIndex(writeIndex);

                    assert(op._src.size() > 0);
                    const SourceOperand& srcOp = op._src[0];
                    if (SourceOperandType::Literal == srcOp.Type())
                    {
                        // Handle sign extension
                        const Literal resized = {Resize(srcOp.GetLiteral(), op.ShouldSignExtend(0), regDesc._width),
                                                 regDesc._width};

                        SafeInsert(globalDesc._literalValues, writeIndex, resized);
                    }
                }
            }
        }
    };

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            for (Stage& stage : basicBlock._stages)
            {
                for (Operation& op : stage._operations)
                {
                    callback(op);
                }
            }
        }
    }
}

void AssignQuadPortMemoryPorts(Program& program)
{
    // Pass 0 handled memories that require bypass
    //   Stores and loads that need bypass are assigned to the same port
    // Pass 1 handles the remainder
    std::set<const Operation*> completedQuadPortOperations;

    for (size_t pass = 0; pass < 2; pass++)
    {
        for (Function& function : program._functions)
        {
            for (BasicBlock& basicBlock : function._basicBlocks)
            {
                // Maps memory index to port
                std::map<size_t, uint64_t> bypassMemories;

                for (Stage& stage : basicBlock._stages)
                {
                    for (Operation& op : stage._operations)
                    {
                        if (completedQuadPortOperations.end() != completedQuadPortOperations.find(&op))
                        {
                            // Already handled this operation in a previous pass
                            assert(pass == 1);
                            continue;
                        }

                        if (op._opcode == Opcode::LoadMemory)
                        {
                            const size_t memoryIndex = op._src[0].GetAccessedRegister()._registerIndex;

                            RegisterDescription& regDesc = program._registerTable[memoryIndex];
                            assert(regDesc._type == RegisterType::Memory);

                            if (regDesc.Memory()._quadPort)
                            {
                                if ((op._flags._loadMemory._bypass && (pass == 0)) || (pass == 1))
                                {
                                    // in pass 1, completedQuadPortOperations
                                    // will filter out all bypass operations
                                    assert((pass == 0) || !op._flags._loadMemory._bypass);

                                    op._flags._loadMemory._readPort = regDesc.Memory()._readPortCount;
                                    regDesc.Memory()._readPortCount++;

                                    if (op._flags._loadMemory._bypass)
                                    {
                                        assert(pass == 0);
                                        regDesc.Memory().SetBypassMask(op._flags._loadMemory._readPort);
                                    }

                                    regDesc.Memory().SetReadLatency(op._flags._loadMemory._readPort,
                                                                    op._flags._loadMemory._readLatency);

                                    if (pass == 0)
                                    {
                                        assert(op._flags._loadMemory._bypass);
                                        SafeInsert(bypassMemories, memoryIndex, static_cast<uint64_t>(op._flags._loadMemory._readPort));
                                    }

                                    completedQuadPortOperations.insert(&op);
                                }
                            }
                        }

                        if (op._opcode == Opcode::StoreMemory)
                        {
                            const size_t registerIndex = op._dst[0].GetAccessedRegister()._registerIndex;

                            RegisterDescription& regDesc = program._registerTable[registerIndex];
                            assert(RegisterType::Memory == regDesc._type);

                            if (regDesc.Memory()._quadPort)
                            {
                                const auto it = bypassMemories.find(registerIndex);

                                if (it != bypassMemories.end())
                                {
                                    assert(pass == 0);

                                    const uint64_t port = it->second;

                                    op._flags._storeMemory._writePort = port;

                                    assert(regDesc.Memory()._writePortCount == port);

                                    regDesc.Memory()._writePortCount++;

                                    bypassMemories.erase(it);

                                    completedQuadPortOperations.insert(&op);
                                }
                                else if (pass == 1)
                                {
                                    op._flags._storeMemory._writePort = regDesc.Memory()._writePortCount;

                                    regDesc.Memory()._writePortCount++;

                                    completedQuadPortOperations.insert(&op);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // quad port memories do not support more access locations
    // than ports
    // (to avoid having to dynamically assign reads/writes to multiple available ports)
    for (RegisterDescription& regDesc : program._registerTable)
    {
        if ((RegisterType::Memory == regDesc._type) && regDesc.Memory()._quadPort)
        {
            if (regDesc.Memory()._readPortCount > 2)
            {
                g_compiler->ErrorStream(regDesc.Memory()._location, CompileError::InvalidQuadPortMemory)
                    << "has more than two read locations";
            }

            if (regDesc.Memory()._writePortCount > 2)
            {
                g_compiler->ErrorStream(regDesc.Memory()._location, CompileError::InvalidQuadPortMemory)
                    << "has more than two write locations";
            }
        }
    }
}

// Verifies that each memory is written no more than once
// Determines the number of read ports required for each memory
// Called afer optimization - and after unreachable functions have been removed
// Runs after scheduler and after virtual placement
void AssignMemoryPorts(Program& program)
{
    Location loc = {0};

    // Initialize all memory read port counts to 0
    for (RegisterDescription& regDesc : program._registerTable)
    {
        if (RegisterType::Memory == regDesc._type)
        {
            regDesc.Memory()._readPortCount = 0;
            regDesc.Memory()._writePortCount = 0;
            regDesc.Memory().ClearBypassMask();
        }
    }

    // Quad port memories are handled separately to ensure bypass is setup on each port correctly
    AssignQuadPortMemoryPorts(program);

    struct ReadRecord
    {
        Operation* _op;
        Placement::Vec2 _pos;
    };

    std::map<size_t, std::list<ReadRecord>> readMap;

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            const BasicBlock* constBb = &basicBlock;

            const Placement::Vec2 basicBlockPos = SafeLookup(program._basicBlockPositions, constBb);

            for (Stage& stage : basicBlock._stages)
            {
                for (Operation& op : stage._operations)
                {
                    if (op._opcode == Opcode::StoreMemory)
                    {
                        const size_t registerIndex = op._dst[0].GetAccessedRegister()._registerIndex;

                        RegisterDescription& regDesc = program._registerTable[registerIndex];
                        assert(RegisterType::Memory == regDesc._type);

                        if (regDesc.Memory()._quadPort)
                        {
                            // quad port was handled separately
                            continue;
                        }

                        op._flags._storeMemory._writePort = regDesc.Memory()._writePortCount;

                        ++regDesc.Memory()._writePortCount;
                    }
                    else if (op._opcode == Opcode::LoadMemory)
                    {
                        const size_t memoryIndex = op._src[0].GetAccessedRegister()._registerIndex;

                        RegisterDescription& regDesc = program._registerTable[memoryIndex];
                        assert(regDesc._type == RegisterType::Memory);

                        if (regDesc.Memory()._quadPort)
                        {
                            // quad port was handled separately
                            continue;
                        }

                        // Reads are handled later
                        const ReadRecord rr = {&op, basicBlockPos};
                        readMap[memoryIndex].push_back(rr);
                    }
                }
            }
        }
    }

    // Handle reads (now that full set of read operations is known)
    for (const auto& p : readMap)
    {
        const size_t memoryIndex = p.first;
        std::list<ReadRecord> readList = p.second; // this is a copy

        RegisterDescription& regDesc = program._registerTable[memoryIndex];
        assert(regDesc._type == RegisterType::Memory);

        assert(!regDesc.Memory().HasAnyBypassPort());
        assert(0 == regDesc.Memory()._readPortCount);
        regDesc.Memory()._readPortCount = readList.size();

        if (!regDesc.Memory()._replicate && !regDesc.Memory()._quadPort)
        {
            // All ports reads must use the same latency
            // To ensure that multiple conditional reads
            // inside of the same atomic block do not conflict.
            size_t minLatency = std::numeric_limits<size_t>::max();

            for (ReadRecord& rr : readList)
            {
                Operation& op = *rr._op;

                minLatency = std::min(minLatency, op._flags._loadMemory._readLatency);
            }

            for (ReadRecord& rr : readList)
            {
                Operation& op = *rr._op;

                op._flags._loadMemory._readLatency = minLatency;
            }
        }

        // Sort the reads by virtual placement location
        // This is important for read-only memories
        // A single read only memory could have many read locations
        // spread across the chip.  If true dual-port mode is used to implement the reads
        // then 2 read operations that share the same underlying memory should be close together
        std::ostringstream displayName;
        displayName << "Read ports for memory: " << regDesc._name;

        const auto getPositionCallback = [&](const ReadRecord readRecord) { return readRecord._pos; };

        const auto getNameCallback = [&](const ReadRecord readRecord)
        {
            std::ostringstream s;
            bool first = true;
            for (const FileAndLineNumber& loc : readRecord._op->_locations)
            {
                if (!first)
                {
                    s << ", ";
                    first = false;
                }
                s << g_compiler->GetSourceFileNameWithoutLeadingPath(loc._fileIndex) << ":" << loc._lineNumber;
            }
            return s.str();
        };

        const Placement::SortRecord sortRecord =
            Placement::Sort<ReadRecord>(readList, getPositionCallback, getNameCallback,
                                        GetCodeGenConfig()._placementConfig._display, displayName.str());

        program._sortRecords.push_back(sortRecord);

        std::set<size_t> readPorts;

        size_t currReadPort = 0;
        for (ReadRecord& rr : readList)
        {
            Operation& op = *rr._op;

            op._flags._loadMemory._readPort = currReadPort;
            currReadPort++;

            const size_t readPort = op._flags._loadMemory._readPort;
            // Check that read ports are not reused
            SafeInsert(readPorts, readPort);

            assert(!regDesc.Memory()._replicate || !regDesc.Memory().IsBypassReadPort(readPort));

            if (op._flags._loadMemory._bypass)
            {
                regDesc.Memory().SetBypassMask(readPort);
            }

            regDesc.Memory().SetReadLatency(readPort, op._flags._loadMemory._readLatency);
        }

        assert(currReadPort == readList.size());
    }
}

// Removes atomic blocks with no content
// Called before scheduling
void RemoveEmptyAtomicBlocks(Program& program)
{
    // Run multiple times in the case of nested empty atomics
    bool didChangeIR;

    do
    {
        didChangeIR = false;

        for (Function& f : program._functions)
        {
            for (BasicBlock& bb : f._basicBlocks)
            {
                OperationList::iterator nextIt = bb._operations.end();

                OperationList::iterator beginAtomicIt = bb._operations.end();
                bool atomicIsEmpty = false;

                for (OperationList::iterator it = bb._operations.begin(); it != bb._operations.end(); it = nextIt)
                {
                    nextIt = it;
                    ++nextIt;

                    const Operation& op = *it;

                    if (Opcode::BeginAtomic == op._opcode)
                    {
                        beginAtomicIt = it;
                        atomicIsEmpty = true;

                        if (AtomicBlockType::MemoryLoadLatency == op._flags._atomicBlockDesc._type)
                        {
                            // MemoryLoadLatency blocks are intentionally empty
                            // they are inserted to pipeline memory load data
                            atomicIsEmpty = false;
                        }
                    }
                    else if (Opcode::EndAtomic == op._opcode)
                    {
                        if (atomicIsEmpty)
                        {
                            assert(beginAtomicIt != bb._operations.end());

                            // remove the BeginAtomic
                            bb._operations.erase(beginAtomicIt);

                            // Remove the EndAtomic
                            bb._operations.erase(it);

                            atomicIsEmpty = false;
                            didChangeIR = true;
                        }

                        beginAtomicIt = bb._operations.end();
                    }
                    else if (Opcode::LineNumber == op._opcode)
                    {
                        // LineNumber operations should not prevent an atomic block
                        // being removed
                    }
                    else
                    {
                        atomicIsEmpty = false;
                    }
                }
            }
        }
    } while (didChangeIR);
}

// Configures the width of the call index register for each synchronous extern
// This is done separately for non-extern synchronous calls
void CountSyncExternCallSites(Program& program)
{
    for (const Function* const function : program._externFunctions)
    {
        const FunctionNode* functionNode = function->_functionNode;

        const bool isSync = !(functionNode->GetModifiers() & ParseTreeFunctionModifierAsync);

        if (isSync)
        {
            const FunctionNode::Instance& instance = functionNode->GetInstance(function->_objectName);

            const size_t callSiteCount = instance.GetCallSiteCount();

            const size_t callSiteRegisterIndex =
                instance.GetCallSiteIndexRegister()->GetAccessedRegister()._registerIndex;
            RegisterDescription& callSiteRegDesc = program._registerTable[callSiteRegisterIndex];

            callSiteRegDesc._width = GetCallSiteIndexWidth(callSiteCount);
        }
    }
}

// Adds inspectable pseudo-variables to the IR which allow connection of the inspectable chain to any child external
// class instances
void AddExternalClassInstanceInspectables(Program& program)
{
    for (const ExternalClassInstance& externModule : program._externClassInstances)
    {
        if (externModule._isExportClass)
        {
            InspectableVariable inspectableVariable = {};

            inspectableVariable._inspectionType = InspectableVariableType::ExternalClassInstanceInst;
            inspectableVariable._registerType = RegisterType::Wire;
            inspectableVariable._externClassInstance = &externModule;
            inspectableVariable._inspectableIndex = program._inspectableVariables.size();
            inspectableVariable._type = g_compiler->GetVoidType(); // Dummy type to prevent various places in the code
                                                                   // from referencing a null pointer.
            program._inspectableVariables.push_back(inspectableVariable);
        }
    }
}

// Add a single inspectable that holds the hash for the module. This inspectable is placed at the start of the
// inspectable chain.
void PrependHashInspectable(Program& program)
{
    InspectableVariable inspectableVariable = {};

    // Give the inspectable a distinct type, so that the back-end can know to generate the RTL to return the hash value
    inspectableVariable._inspectionType = InspectableVariableType::SymbolHash;

    inspectableVariable._registerType = RegisterType::Global;
    inspectableVariable._inspectableIndex = program._inspectableVariables.size();

    inspectableVariable._name = "symbol hash";
    inspectableVariable._description = "Symbol hash for " + program._moduleName;

    // Get a uin64 type to use as the inspectable's type. The location structure is required for the call to
    // Compiler::GetLeafType, but it is not really relevant because the type didn't come from a declaration
    // in a source file. Since this value is only used for error handling during compilation, we just zero
    // initialize the structure.
    const Location loc = {};
    inspectableVariable._type = g_compiler->GetLeafType(BaseType::Uint, 64, loc);

    // Add the inspectable at the head of the chain
    program._inspectableVariables.push_front(inspectableVariable);
}

// Adds inspectable variables to the IR which allow inspection where threads are and why the are/are not moving
void AddControlInspectables(Program& program)
{
    Location loc = {0};

    for (const Function& function : program._functions)
    {
        if (function.IsExtern())
        {
            continue;
        }

        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            InspectableVariable inspectableVariable = {};

            inspectableVariable._type = g_compiler->GetLeafType(BaseType::Uint, c_basicBlockControlWidth, loc);

            const std::string basicBlockName = GetBasicBlockName(basicBlock);

            inspectableVariable._name = basicBlockName + "ControlState";

            inspectableVariable._description =
                "Bitmask that represents the reason why threads are not flowing through the basic block: " +
                basicBlockName;

            inspectableVariable._inspectableIndex = program._inspectableVariables.size();

            inspectableVariable._registerType = RegisterType::Global;

            inspectableVariable._inspectionType = InspectableVariableType::BasicBlockControlState;

            inspectableVariable._basicBlock = &basicBlock;

            program._inspectableVariables.push_back(inspectableVariable);
        }
    }
}

// Adds 1 InspectableVariable which is used to count race conditions
void AddRaceConditionInspectables(Program& program)
{
    Location loc = {0};

    // Build a sorted list of registers which may have race conditions
    assert(program._raceConditionRegisters.empty());

    // Early out if race detection is disabled
    if (!GetCodeGenConfig()._detectRaces)
    {
        return;
    }

    for (size_t i = 0; i < program._registerTable.size(); i++)
    {
        if (RegisterTracksRaces(program._registerTable[i]))
        {
            program._raceConditionRegisters.push_back(i);
        }
    }

    // The first register associated with an inspectable variable is used as the proxy for the callbacks
    const auto getPositionCallback = [&](const size_t registerIndex)
    { return program.GetRegisterPosition(registerIndex); };

    const auto getNameCallback = [&](const size_t registerIndex)
    { return program._registerTable[registerIndex]._name; };

    const Placement::SortRecord sortRecord =
        Placement::Sort<size_t>(program._raceConditionRegisters, getPositionCallback, getNameCallback,
                                GetCodeGenConfig()._placementConfig._display, "Race condition variables");

    program._sortRecords.push_back(sortRecord);

    // Add an InspectableVariable, which is used to count race conditions
    if (!program._raceConditionRegisters.empty())
    {
        InspectableVariable inspectableVariable = {};

        inspectableVariable._type = g_compiler->GetLeafType(BaseType::Uint, c_raceCountWidth, loc);

        inspectableVariable._name = "Race count";

        inspectableVariable._description = "The number of times an access to a shared variable was dropped because of "
                                           "unsynchronized concurrent accesses";

        inspectableVariable._inspectableIndex = program._inspectableVariables.size();

        inspectableVariable._registerType = RegisterType::Global;

        inspectableVariable._inspectionType = InspectableVariableType::RaceCount;

        program._inspectableVariables.push_back(inspectableVariable);
    }
}

// Builds _sortedCallableEntryPoints, and sorts it by virtual placement position
void SortEntryPoints(Program& program)
{
    for (const EntryPoint* const entryPoint : program._callableEntryPoints)
    {
        for (const Function* const function : entryPoint->_instances)
        {
            program._sortedCallableEntryPoints.push_back(function);
        }
    }

    const auto getFunctionPositionCallback = [&](const Function* const function)
    {
        const BasicBlock* const firstBasicBlock = function->_start;

        return program.GetBasicBlockPosition(firstBasicBlock);
    };

    const auto getFunctionNameCallback = [&](const Function* const function)
    { return GetFunctionCombinedName(function); };

    const Placement::SortRecord sortRecord = Placement::Sort<const Function*>(
        program._sortedCallableEntryPoints, getFunctionPositionCallback, getFunctionNameCallback,
        GetCodeGenConfig()._placementConfig._display, "Command Processor Chain");

    program._sortRecords.push_back(sortRecord);
}

// Sorts inspectable variables by position in virtual placement
// to make chaining easier on timing
void SortInspectables(Program& program)
{
    // The first register associated with an inspectable variable is used as the proxy for the callbacks
    const auto getPositionCallback = [&](const InspectableVariable inspectableVariable)
    {
        if (inspectableVariable._inspectionType == InspectableVariableType::Default ||
            inspectableVariable._inspectionType == InspectableVariableType::CodeCoverage)
        {
            assert(!inspectableVariable._registers.empty());

            const size_t firstRegister = inspectableVariable._registers[0];

            return program.GetRegisterPosition(firstRegister);
        }
        else
        {
            assert(inspectableVariable._inspectionType == InspectableVariableType::BasicBlockControlState);

            return program.GetBasicBlockPosition(inspectableVariable._basicBlock);
        }
    };

    const auto getNameCallback = [&](const InspectableVariable inspectableVariable)
    {
        if (inspectableVariable._inspectionType == InspectableVariableType::Default ||
            inspectableVariable._inspectionType == InspectableVariableType::CodeCoverage)
        {
            assert(!inspectableVariable._registers.empty());

            const size_t firstRegister = inspectableVariable._registers[0];

            return program._registerTable[firstRegister]._name;
        }
        else
        {
            assert(inspectableVariable._inspectionType == InspectableVariableType::BasicBlockControlState);

            return GetBasicBlockName(*inspectableVariable._basicBlock);
        }
    };

    // g_compiler->IsCompilingToVerilog() causes this to run quickly in non-Verilog scenarios
    // where the sorting doesn't matter for performance
    const Placement::SortRecord sortRecord = Placement::Sort<InspectableVariable>(
        program._inspectableVariables, getPositionCallback, getNameCallback,
        GetCodeGenConfig()._placementConfig._display, "Inspectable Variable Chain", g_compiler->IsCompilingToVerilog());

    program._sortRecords.push_back(sortRecord);
}

// Uses the width of some operands to bound the width of other operands
// Useful for reducing the amount of time spent running the synthesis tool
// which will generally do this same optimizaiton, but after a long time processing unnecessary bits
// This is especially important for left shift operations, where the result width can be very wide
// This runs after pipelining
class RegisterWidthTracker
{
  public:
    RegisterWidthTracker(Program& program) : _program(program) {}

    void BoundRegister(const size_t registerIndex, const size_t boundIn)
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];
        if ((RegisterType::Wire == regDesc._type) || (RegisterType::Pipeline == regDesc._type))
        {
            // Don't allow registers with width = 0, the backends will have trouble with this
            const size_t bound = std::min<size_t>(std::max<size_t>(boundIn, 1), regDesc._width);

            assert(bound > 0);

            auto it = _bounds.lower_bound(registerIndex);
            if (it == _bounds.end() || it->first != registerIndex)
            {
                _bounds.emplace_hint(it, registerIndex, bound);
            }
            else
            {
                it->second = std::max(it->second, bound);
            }
        }
    }

    size_t GetBound(const size_t registerIndex)
    {
        const RegisterDescription& regDesc = _program._registerTable[registerIndex];
        if ((RegisterType::Wire == regDesc._type) || (RegisterType::Pipeline == regDesc._type))
        {
            auto it = _bounds.lower_bound(registerIndex);
            if (it == _bounds.end() || it->first != registerIndex)
            {
                // Set a dead register to 1 bit.
                it = _bounds.emplace_hint(it, registerIndex, 1);
            }
            return it->second;
        }
        else
        {
            return regDesc._width;
        }
    }

    size_t GetBound(const DestinationOperand& dst)
    {
        if (dst.Type() == DestinationOperandType::Register)
        {
            return GetBound(dst.GetAccessedRegister()._registerIndex);
        }
        else
        {
            return dst.Width(_program);
        }
    }

    void Apply()
    {
        // Called at the end of the operation, reduces the sizes of wires/pipeline registers where possible

        // It is safe to do this because all instances where the wire/register are used as a source operand have been
        // considered A wire/register is only ever accessed in 1 basic block - so the algorithm can process each basic
        // block serially

        for (const auto& p : _bounds)
        {
            const size_t registerIndex = p.first;
            const size_t bound = p.second;

            assert(bound > 0);
            RegisterDescription& regDesc = _program._registerTable[registerIndex];

            assert((RegisterType::Wire == regDesc._type) || (RegisterType::Pipeline == regDesc._type));

            regDesc._width = bound;
        }
    }

  private:
    Program& _program;

    std::map<size_t, size_t> _bounds;
};

void ReduceRegisterWidth(Program& program, BasicBlock& basicBlock)
{
    RegisterWidthTracker tracker(program);

    // Walk through all stages
    for (auto stage_it = basicBlock._stages.crbegin(); stage_it != basicBlock._stages.crend(); ++stage_it)
    {
        const Stage& stage = *stage_it;
        for (auto op_it = stage._operations.crbegin(); op_it != stage._operations.crend(); ++op_it)
        {
            const Operation& op = *op_it;
            // Nothing to do if there are no source operands
            if (op._src.empty())
            {
                continue;
            }

            // Maps source operand index to bound on the width of that operand
            std::map<size_t, size_t> boundMap;

            switch (op._opcode)
            {
            case Opcode::Mov:
            case Opcode::MovCrossFunction:
            case Opcode::MovLatencyPlaceholder:
                assert(1 == op._dst.size());
                assert(1 == op._src.size());

                // Source width does not need to be any wider than destination width
                boundMap[0] = tracker.GetBound(op._dst[0]);
                break;

            case Opcode::UnaryOp:
                assert(1 == op._dst.size());
                assert(1 == op._src.size());
                assert(ParseTreeUnaryOpTypeInvert == op._flags._unaryOpType);
                // Source width does not need to be any wider than destination width
                boundMap[0] = tracker.GetBound(op._dst[0]);
                break;

            case Opcode::BinaryOp:
                assert(1 == op._dst.size());
                assert(2 == op._src.size());

                switch (op._flags._binaryOpType)
                {
                case ParseTreeBinaryOpTypeAdd:
                case ParseTreeBinaryOpTypeAnd:
                case ParseTreeBinaryOpTypeOr:
                case ParseTreeBinaryOpTypeXor:
                    // Both operands should be no wider than the destination
                    boundMap[0] = tracker.GetBound(op._dst[0]);
                    boundMap[1] = tracker.GetBound(op._dst[0]);
                    break;

                case ParseTreeBinaryOpTypeShl:
                    // left-hand-side operand should be no wider than the destination
                    boundMap[0] = tracker.GetBound(op._dst[0]);
                    break;

                case ParseTreeBinaryOpTypeSub:
                case ParseTreeBinaryOpTypeShr:
                case ParseTreeBinaryOpTypeMul:
                case ParseTreeBinaryOpTypeLutMul:
                case ParseTreeBinaryOpTypeEQ:
                case ParseTreeBinaryOpTypeNE:
                case ParseTreeBinaryOpTypeGT:
                case ParseTreeBinaryOpTypeGE:
                case ParseTreeBinaryOpTypeLT:
                case ParseTreeBinaryOpTypeLE:
                    // No bounds can be determined
                    break;

                default:
                    assert(false);
                }
                break;

            case Opcode::Select:
                assert(1 == op._dst.size());

                // All sources (except the first) do not need to be wider than the destination
                for (size_t i = 1; i < op._src.size(); i++)
                {
                    boundMap[i] = tracker.GetBound(op._dst[0]);
                }
                break;

            case Opcode::WriteGlobal:
                assert(1 == op._dst.size());

                // The source (not the predicate) does not need to be wider than the destination
                boundMap[0] = op._dst[0].Width(program);
                break;

            case Opcode::Gather:
                // Gather entries bound the size of the source
                assert(1 == op._dst.size());

                for (size_t i = 0; i < op._src.size(); i++)
                {
                    const GatherEntry& gatherEntry = op._flags._gather._entries->at(i);

                    boundMap[i] = gatherEntry._sourceOffset + gatherEntry._numBits;
                }
                break;

            case Opcode::Lut:
                // LUT entries bound the range of the source that is needed
                assert(1 == op._dst.size());

                for (size_t dstBit = 0; dstBit < op._flags._lut._numDestinationBits; dstBit++)
                {
                    const LutEntry& lutEntry = op._flags._lut._lutEntries[dstBit];

                    for (size_t whichSource = 0; whichSource < lutEntry._numSources; whichSource++)
                    {
                        const size_t srcIndex = lutEntry._sourceIndices[whichSource];

                        const size_t bound = lutEntry._sourceBit[whichSource] + 1;

                        boundMap[srcIndex] = std::max(boundMap[srcIndex], bound);
                    }
                }
                break;

            case Opcode::EnqueueRegisters:
            case Opcode::StoreMemory:
            case Opcode::InlineExternalModule:
            case Opcode::FanOut:
            case Opcode::Stage:
            case Opcode::StartCondition:
            case Opcode::CycleCounter:
            case Opcode::LoadMemory:
            case Opcode::Enqueue:
            case Opcode::ReadSelectedFifo:
            case Opcode::ExternalPlaceholder:
            case Opcode::Print:
            case Opcode::Assert:
            case Opcode::AtomicReturn:
            case Opcode::CallAtomic:
            case Opcode::DebugView:
            case Opcode::StallCheck:
            case Opcode::NoOp:
            case Opcode::BypassMemory:
            case Opcode::ReferenceString:
            case Opcode::FormatString:
            case Opcode::FormatEnum:
            case Opcode::AssertStringEqual:
            case Opcode::StringCount:
                // No bounds can be determined
                break;

            case Opcode::ConditionalIgnore:
                assert(1 == op._dst.size());
                assert(op._src.size() >= 1);

                // Source and dest widths should match
                boundMap[0] = tracker.GetBound(op._dst[0]);

                // Predicates
                for (size_t i = 1; i < op._src.size(); i++)
                {
                    assert(1 == op._src[i].Width(program));
                    boundMap[i] = 1;
                }
                break;

            default:
                assert(false);
            }

            // Write bounds of all source operands into tracker
            for (size_t srcIndex = 0; srcIndex < op._src.size(); srcIndex++)
            {
                const SourceOperand& srcOp = op._src[srcIndex];

                // Only registers are considered
                if (SourceOperandType::Register != srcOp.Type())
                {
                    continue;
                }

                const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                // Only wire/pipeline register operands are bounded
                const RegisterDescription& regDesc = program._registerTable[registerIndex];

                if ((regDesc._type != RegisterType::Wire) && (regDesc._type != RegisterType::Pipeline))
                {
                    continue;
                }

                // Lookup the bound, if it is not found, then treat the width as unbounded
                // because there is a read, then the bound is unknown
                const auto it = boundMap.find(srcIndex);

                const size_t bound = (it == boundMap.end()) ? srcOp.Width(program) : it->second;

                tracker.BoundRegister(registerIndex, bound);
            }
        }
    }

    tracker.Apply();

    // Fixup metadata related to some operations
    ForEachOperationForward(
        basicBlock,
        [&](Operation& op)
        {
            switch (op._opcode)
            {
            case Opcode::Gather:
            {
                assert(1 == op._dst.size());

                const size_t dstWidth = op._dst[0].Width(program);

                std::vector<GatherEntry>& gatherEntries = *op._flags._gather._entries;

                for (size_t i = 0; i < op._src.size(); ++i)
                {
                    GatherEntry& gatherEntry = gatherEntries[i];

                    const size_t end = gatherEntry._destOffset + gatherEntry._numBits;

                    if (end > dstWidth)
                    {
                        if (gatherEntry._destOffset < dstWidth)
                        {
                            gatherEntry._numBits = dstWidth - gatherEntry._destOffset;
                        }
                        else
                        {
                            gatherEntry._numBits = 0;
                        }
                    }
                }

                // Remove operands and GatherEntries for cases where _numBits = 0
                bool continueLooping = true;

                do
                {
                    continueLooping = false;

                    for (size_t i = 0; i < op._src.size(); ++i)
                    {
                        const GatherEntry& gatherEntry = gatherEntries[i];

                        if (gatherEntry._numBits == 0)
                        {
                            gatherEntries.erase(gatherEntries.begin() + i);
                            op._src.erase(op._src.begin() + i);

                            continueLooping = true;

                            break;
                        }
                    }
                } while (continueLooping);
            }
            break;

            case Opcode::Lut:
            {
                assert(1 == op._dst.size());

                const size_t dstWidth = op._dst[0].Width(program);

                assert(op._flags._lut._numDestinationBits >= dstWidth);

                if (op._flags._lut._numDestinationBits > dstWidth)
                {
                    op._flags._lut._numDestinationBits = dstWidth;
                }
            }
            break;

            default:
                break;
            }
        },
        OperationEnumerationMode::Scheduled);
}

// Modify debug view names to make them unique if necessary
void MakeDebugViewNamesUnique(Program& program)
{
    std::map<std::string, size_t> duplicateCount;

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            const auto operationCallback = [&](Operation& op)
            {
                if (Opcode::DebugView == op._opcode)
                {
                    std::string& label = op._flags._debugView->_label;

                    size_t& count = duplicateCount[label];

                    std::ostringstream str;
                    str << label;

                    if (count > 0)
                    {
                        // Append the count to make the label unique
                        str << count;
                    }

                    count++;

                    label = str.str();
                }
            };

            ForEachOperationForward(basicBlock, operationCallback);
        }
    }
}

void DetermineStallablePipelineStages(BasicBlock& basicBlock)
{
    bool stallablePipeline = GetCodeGenConfig()._stallablePipelines;

    assert(basicBlock._pipelineStageStallableMap.empty());
    assert(!basicBlock._isStallable);

    // Set initial stallable state. If --stallable-pipelines was not enabled, then all stages (except 0) are
    // non-stallable and there is no need to do any further calculations. If it was enabled, we initially set all
    // pipeline stages to stallable state, and then selectively clear them below. Note that we set state of first stage
    // to stallable, even if stallable pipelines is disabled (because stage 0, being at the head of the pipeline, is
    // always stallable)
    for (auto& stage : basicBlock._stages)
    {
        basicBlock._pipelineStageStallableMap[stage._atomicSequence] = {
            (stage._atomicSequence == 0) ? true : stallablePipeline};
    }

    if (stallablePipeline)
    {
        // Identify any pipeline stages that are non-stallable
        for (auto& stage : basicBlock._stages)
        {
            for (const Operation& op : stage._operations)
            {
                if (OperationHasRegisteredInput(op))
                {
                    // This pipeline stage is not stallable
                    basicBlock._pipelineStageStallableMap[stage._atomicSequence]._isStallable = false;
                }

                if (OperationHasRegisteredOutput(op))
                {
                    // The next pipeline stage is not stallable
                    basicBlock._pipelineStageStallableMap[stage._atomicSequence + 1]._isStallable = false;
                }

                if (Opcode::InlineExternalModule == op._opcode && op._flags._callInlineExternalModule._latency > 2)
                {
                    // DSP or fixed latency extern functions with latency > 2
                    // For example if this value is 5, then stages N+2, N+3, N+4 are all not stallable.
                    for (size_t offset = 2; offset < op._flags._callInlineExternalModule._latency; ++offset)
                    {
                        basicBlock._pipelineStageStallableMap[stage._atomicSequence + offset]._isStallable = false;
                    }
                }

                // First stage should always be stallable
                assert(stage._atomicSequence != 0 ||
                       basicBlock._pipelineStageStallableMap[stage._atomicSequence]._isStallable);
            }
        }
    }

    // Go through all the pipeline stages and:
    //   - For any that are stallable, calculate the nearest preceeding stallable pipeline stage, if any.
    //   - Set a flag indicating if the pipeline is stallable. A pipeline is considered stallable if
    //     any stage (other than stage 0) is stallable.
    //   - Calculate the additional number of FIFO almost full entries needed for that stage, taking
    //     into account how many previous stages were non-stallable.
    boost::optional<size_t> lastNonStallable;
    size_t stageAdditionalFifoAlmostFullEntries = 0;
    for (auto& stallable : basicBlock._pipelineStageStallableMap)
    {
        stallable.second._nearestPreceedingNonStallable = lastNonStallable;

        if (stallable.second._isStallable)
        {
            if (stallable.first != 0)
            {
                basicBlock._isStallable = true;
            }
        }
        else
        {
            lastNonStallable = stallable.first;
            stageAdditionalFifoAlmostFullEntries++;
        }

        stallable.second._stageAdditionalFifoAlmostFullEntries = stageAdditionalFifoAlmostFullEntries;
    }
}

void RemoveConditionalIgnoreInAtomics(Program& program)
{
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            OperationList::iterator nextIt = basicBlock._operations.end();
            size_t atomicDepth = 0;

            for (OperationList::iterator it = basicBlock._operations.begin(); it != basicBlock._operations.end();
                 it = nextIt)
            {
                const Operation& op = *it;

                nextIt = it;
                ++nextIt;

                if (Opcode::BeginAtomic == op._opcode)
                {
                    atomicDepth++;
                }
                else if (Opcode::EndAtomic == op._opcode)
                {
                    assert(atomicDepth > 0);
                    atomicDepth--;
                }
                else if ((Opcode::ConditionalIgnore == op._opcode) && atomicDepth > 0)
                {
                    // Operation can simply be removed, because src[0] == dst[0]
                    assert(op._src[0].GetAccessedRegister()._registerIndex ==
                           op._dst[0].GetAccessedRegister()._registerIndex);

                    basicBlock._operations.erase(it);
                }
            };

            assert(atomicDepth == 0);
        }
    }
}

// Called after AddFifoWrites
// At this point, MovCrossFunction (forced live) is unnecessary
// because explicit moves into FIFOs have been added
void RemoveMoveCrossFunction(BasicBlock& basicBlock)
{
    ForEachOperationForward(
        basicBlock,
        [](Operation& op)
        {
            if (Opcode::MovCrossFunction == op._opcode)
            {
                op._opcode = Opcode::Mov;
            }
        },
        OperationEnumerationMode::Scheduled);
}

// Ensure that register names in the IR do not exceed the maximum identifier length
void ClampRegisterNameLengths(Program& program)
{
    const size_t limit = GetCodeGenConfig()._maxStringLength;

    for (RegisterDescription& rd : program._registerTable)
    {
        if (rd._name.size() > limit)
        {
            rd._name = g_compiler->ClampStringLength(rd._name);
        }

        if (RegisterType::Global == rd._type)
        {
            for (std::string& s : rd.Global()._containerInstancePath)
            {
                s = g_compiler->ClampStringLength(s);
            }
        }
        if (RegisterType::Memory == rd._type)
        {
            for (std::string& s : rd.Memory()._containerInstancePath)
            {
                s = g_compiler->ClampStringLength(s);
            }
        }
    }
}

// Computes intra-stage wires/registers
// Runs after optimization and scheduling
void ComputeIntraStageRegisters(Program& program)
{
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            assert(basicBlock._intraStageRegisters.empty());

            RegisterSet intraStageRegisters;

            const auto insertReg = [&](const size_t registerIndex)
            {
                if (IsLocalRegisterType(program._registerTable[registerIndex]._type))
                {
                    intraStageRegisters.insert(registerIndex);
                }
            };

            for (const Stage& stage : basicBlock._stages)
            {
                // Clock gates count as consumers
                for (const auto& p : stage._clockGates)
                {
                    for (const size_t gateRegister : p.second)
                    {
                        insertReg(gateRegister);
                    }
                }

                for (const Operation& op : stage._operations)
                {
                    // Add consumed registers
                    for (const SourceOperand& srcOp : op._src)
                    {
                        if (SourceOperandType::Register == srcOp.Type())
                        {
                            insertReg(srcOp.GetAccessedRegister()._registerIndex);
                        }
                    }

                    // Add produced registers
                    for (const DestinationOperand& dstOp : op._dst)
                    {
                        if (DestinationOperandType::Register == dstOp.Type())
                        {
                            insertReg(dstOp.GetAccessedRegister()._registerIndex);
                        }
                    }
                }
            }

            basicBlock._intraStageRegisters = std::list<size_t>(intraStageRegisters.begin(), intraStageRegisters.end());
        }
    }
}

void DetermineConstantBits(Program& program)
{
    assert(program._constantBits.empty());

    for (Function& function : program._functions)
    {
        ControlFlowGraph cfg(function, program, OperationEnumerationMode::Scheduled,
                             ControlFlowGraphPhase::PostPipeline);

        BitConstantPropagation(program, cfg, function, &program._constantBits);
    }
}

void DetermineUnusedBits(Program& program)
{
    assert(program._unusedBits.empty());

    for (Function& function : program._functions)
    {
        ControlFlowGraph cfg(function, program, OperationEnumerationMode::Scheduled,
                             ControlFlowGraphPhase::PostPipeline);

        DetermineUnusedBitsForFunction(program, cfg, function, program._unusedBits);
    }
}

// Locates bits in wires/pipeline registers
// which do not have effect on the output of a function
void DetermineUnusedBitsForFunction(Program& program, ControlFlowGraph& cfg, const Function& function,
                                    RegisterToBitSet& result)
{
    const OperationEnumerationMode mode = cfg.GetEnumerationMode();

    // Function is not actually modified
    // But CFG and DFA require non-const functions
    Function& nonConstFunction = const_cast<Function&>(function);

    // The set of used (affecting function output) bits in a register
    using used_bit_set_t = std::set<size_t>;

    using dfa_t = DataFlowAnalysis<std::set<size_t>>;

    const auto joinCb = [](const used_bit_set_t& a, const used_bit_set_t& b)
    {
        used_bit_set_t result = a;

        // If either successor marked a bit as used
        // then consider it to be used
        Union(result, b);

        return result;
    };

    const auto translateCb = [](const used_bit_set_t& inputBits, ControlFlowGraph& cfg,
                                const dfa_t::Direction direction, BasicBlock* const from, BasicBlock* const to)
    {
        // BitToValue does not contain register indices, so no translation is necessary
        return inputBits;
    };

    LutArenaAllocator lutAllocator;

    const auto operationCb = [&](const Operation& inputOp, dfa_t::RegToT& regToUsedBits)
    {
        bool forceLive = false;

        // Some operations are always considered live
        if (IsLiveOperation(inputOp))
        {
            forceLive = true;
        }

        // If any destination is not a local register, force all source operands to be live
        for (const DestinationOperand& dstOp : inputOp._dst)
        {
            if (dstOp.Type() != DestinationOperandType::Register)
            {
                forceLive = true;
            }
        }

        if (!forceLive)
        {
            Operation lutOp = inputOp;

            if (CanOpcodeBeLoweredToLut(inputOp._opcode) && LowerToLut(program, lutOp, lutAllocator.LutFn()))
            {
                assert(1 == lutOp._dst.size());

                const size_t dstReg = lutOp._dst[0].GetAccessedRegister()._registerIndex;

                const auto it = regToUsedBits.find(dstReg);

                if (it != regToUsedBits.end())
                {
                    const used_bit_set_t& dstUsedBits = it->second;

                    // For each destination bit
                    for (size_t dstBit = 0; dstBit < lutOp._flags._lut._numDestinationBits; dstBit++)
                    {
                        // Check to see if this destination bit affects the output
                        if (dstUsedBits.end() != dstUsedBits.find(dstBit))
                        {
                            // Propagate to sources
                            const LutEntry& lutEntry = lutOp._flags._lut._lutEntries[dstBit];

                            for (size_t lutSrc = 0; lutSrc < lutEntry._numSources; lutSrc++)
                            {
                                const size_t srcOperandIndex = lutEntry._sourceIndices[lutSrc];

                                const size_t srcBit = lutEntry._sourceBit[lutSrc];

                                const SourceOperand& srcOperand = lutOp._src[srcOperandIndex];

                                if (SourceOperandType::Register == srcOperand.Type())
                                {
                                    const size_t srcRegisterIndex = srcOperand.GetAccessedRegister()._registerIndex;

                                    if (IsLocalRegisterType(program._registerTable[srcRegisterIndex]._type))
                                    {
                                        regToUsedBits[srcRegisterIndex].insert(srcBit);
                                    }
                                }
                            }
                        }
                    }
                }
                else
                {
                    // No destination bits are used
                    // no need to mark source bits as used
                }
            }
            else
            {
                // Could not interpret the operation as a lut operation
                // Assume all input bits are live
                forceLive = true;
            }
        }

        if (forceLive)
        {
            // Mark all inputs as used
            for (const SourceOperand& srcOp : inputOp._src)
            {
                if (SourceOperandType::Register == srcOp.Type())
                {
                    const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                    const RegisterDescription& rd = program._registerTable[registerIndex];

                    if (IsLocalRegisterType(rd._type))
                    {
                        used_bit_set_t& usedBits = regToUsedBits[registerIndex];

                        for (size_t i = 0; i < rd._width; i++)
                        {
                            usedBits.insert(i);
                        }
                    }
                }
            }
        }
    };

    dfa_t dfa(program, nonConstFunction, cfg, dfa_t::Direction::Backward, mode, translateCb, joinCb, operationCb);

    // Store final results
    for (BasicBlock& bb : nonConstFunction._basicBlocks)
    {
        const dfa_t::RegToT regToUsedBits = dfa.GetRegisterMap(&bb);

        ForEachOperationForward(
            bb,
            [&](Operation& op)
            {
                for (const DestinationOperand& dstOp : op._dst)
                {
                    if (DestinationOperandType::Register == dstOp.Type())
                    {
                        const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

                        const RegisterDescription& rd = program._registerTable[registerIndex];

                        assert(result.end() == result.find(registerIndex));

                        const auto it = regToUsedBits.find(registerIndex);
                        if (it != regToUsedBits.end())
                        {
                            assert(IsLocalRegisterType(rd._type));

                            const used_bit_set_t& usedBits = it->second;

                            // Convert from used to unused
                            std::set<size_t> unusedBits;

                            for (size_t i = 0; i < rd._width; i++)
                            {
                                if (usedBits.end() == usedBits.find(i))
                                {
                                    unusedBits.insert(i);
                                }
                            }

                            if (!unusedBits.empty())
                            {
                                SafeInsert(result, registerIndex, unusedBits);
                            }
                        }
                    }
                }
            },
            mode);
    }
}

// Work-around the CIRCT limitation that all symbols must be unique (including container instance symbols)
// by flattening all paths
void FlattenContainerPaths(Program& program)
{
    const auto flatten = [](ObjectPath& path)
    {
        ObjectPath result(1, SerializePath(path, '_'));

        path = result;
    };

    for (RegisterDescription& regDesc : program._registerTable)
    {
        if (RegisterType::Global == regDesc._type)
        {
            flatten(regDesc.Global()._containerInstancePath);
        }
        else if (RegisterType::Memory == regDesc._type)
        {
            flatten(regDesc.Memory()._containerInstancePath);
        }
    }
}

// Called after all basic blocks have been created
// This allocates pipeline registers, and remaps register accesses
// to the pipeline registers
void OptimizeScheduleAndPipeline(IRContext& context)
{
    Program& program = *context._program;

    // Record that control flow graphs in pre-pipeline mode can be constructed
    assert(!program._prePipelineCfgAllowed);
    program._prePipelineCfgAllowed = true;

    // Ensure that all debug view labels are unique
    MakeDebugViewNamesUnique(program);

    // Determine the width of call index registers for synchronous externs
    CountSyncExternCallSites(program);

    // Remove basic blocks that are unreachable from export functions
    // Reachability was determined during function instance enumeration
    RemoveUnreachableFunctions(program);

    // Mark forced live-in registers with the flag that says it is OK to not write them into desitnation FIFOs
    // if they are not actually live-in at the destination.
    // This enables 2 cases to work:
    //
    // 1. predicated calls (the link from the call site to the return site)
    //      It is OK to skip the fifo write in this case, because the return value is not used
    //      when the call is predicated away
    //
    // 2. async function with a tail call to a non-async function
    //       In this case, the return site is optmized away
    //       and thus the invocation index is not used
    SetCanSkipFifoWrites(program);

    // Optimizations
    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();

    std::ostream& str = std::cout;

    // Optimizations which run in parallel
    // and exist mostly to reduce the amount of work
    // that subsequent (serialize) code paths do
    if (codeGenConfig._optimize > 0)
    {
        EarlyOptimization(program);
    }

    // Remove ConditionalIgnore operations inside of atomics blocks
    // to help timing and to ensure InsertGlobalViews can produce
    // schedulable basic blocks
    // This must run before renaming
    RemoveConditionalIgnoreInAtomics(program);

    // Eliminate WAW and WAR hazards on locals via renaming
    // Note that this occurs even when optimizations are disabled
    // because instruction scheduling assumes that renaming has run
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            if (codeGenConfig._dumpIR)
            {
                std::cout << "\n=====Before renaming=====\n";
                SerializeBasicBlock(std::cout, basicBlock, program);
            }

            Rename(program, basicBlock);

            if (codeGenConfig._dumpIR)
            {
                std::cout << "\n=====After renaming=====\n";
                SerializeBasicBlock(std::cout, basicBlock, program);
            }
        }
    }

    // Before transforming the IR further, ensure it is valid
    for (Function& function : program._functions)
    {
        ValidateIR(program, function);
    }

    // Find and rewrite memory loads that have address/predicate operands
    // which are functions of global variables
    {
        CompileTimer ct(*g_compiler, "InsertGlobalViews");

        InsertGlobalViews(program);
    }

    // Optimize() below performs a different set of
    // optimizations based on codeGenConfig._optimze
    Optimize(context);

    // Update operand width for relational operators to be the same to elimiate the Lint warning
    UpdateCmpOpWidth(program);

    // Optimization may cause other functions to become unreachable
    // Remove them here
    // This runs until convergence, because removing a unreachable basic block
    // can cause new functions to become unreachable
    do
    {
        ComputeUnreachableFunctions(program);
    } while (RemoveUnreachableFunctions(program));

    // Remove unnecessary entries from the register table
    CompactRegisterTable(*context._program);

    if (codeGenConfig._serializeIRPostOpt)
    {
        const std::string fileName = context._baseFileName + "PostOptIr.json";

        SerializeIRToJson(*context._program, fileName);
    }

    // MemToArray can cause there to be empty atomic blocks
    // remove them
    if (codeGenConfig._optimize > 0)
    {
        CompileTimer ct(*g_compiler, "RemoveEmptyAtomicBlocks");

        RemoveEmptyAtomicBlocks(program);
    }

    // RemoveDeadJumps is needed for correctness, even if optimizations are disabled
    // It must happen after all optimizations
    for (Function& function : program._functions)
    {
        RemoveDeadJumps(function);
    }

    if (codeGenConfig._optimize)
    {
        if (codeGenConfig._dumpIR)
        {
            str << "\nAfter optimization\n";
            str << (*context._program);
        }
    }

    // Debug symbols: build _sourceToPipelineRegisterMap
    for (size_t regIndex = 0; regIndex < program._registerTable.size(); ++regIndex)
    {
        const RegisterDescription& regDesc = program._registerTable[regIndex];

        if (regDesc._isDirectSource)
        {
            // The actual pipeline registers for each source will be added during AddPipelineRegisters()
            program._sourceToPipelineRegisterMap.insert(
                std::pair<size_t, std::set<size_t>>(regIndex, std::set<size_t>()));
        }
    }

    std::set<const BasicBlock*> removedBasicBlocks;

    if (GetCodeGenConfig()._optimize > 1)
    {
        // Global data propagation must run before scheduling (for correctness)
        // and before local data propagation (for performance, so that local data propagation doesn't try to optimize
        // live-in variables)
        for (Function& function : program._functions)
        {
            CompileTimer ct(*g_compiler, "GlobalDataPropagation");

            GlobalDataPropagation(context, function);
        }
    }

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            // Schedule operations into stages
            {
                CompileTimer ct(*g_compiler, "ScheduleBasicBlock");

                AssertUnscheduledOperationsHaveLocations(program, basicBlock);

                ScheduleBasicBlock(program, basicBlock);

                AssertScheduledOperationsHaveLocations(program, basicBlock);
            }
        }

        // Remove empty basic blocks, so that the back-ends do not need to worry about them
        const auto removeBasicBlockCallback = [&removedBasicBlocks](const BasicBlock& basicBlock)
        {
            const bool shouldRemove = basicBlock._stages.empty();

            if (shouldRemove)
            {
                removedBasicBlocks.insert(&basicBlock);
            }

            return shouldRemove;
        };

        function._basicBlocks.remove_if(removeBasicBlockCallback);
    }

    // Allocate write ports for globals
    AssignGlobalWritePorts(program);

    {
        // Remove context saver objects that refer to the removed basic blocks
        const auto removeContextSaverCallback = [&removedBasicBlocks](const ContextSaver& contextSaver)
        {
            const auto it = removedBasicBlocks.find(contextSaver._afterCall);

            return it != removedBasicBlocks.end();
        };

        program._contextSavers.remove_if(removeContextSaverCallback);
    }

    // Handle return blocks to removed basic blocks
    for (Function& function : program._functions)
    {
        function._functionNode->HandleRemovedBasicBlocks(removedBasicBlocks);
    }

    // Mark removed basic blocks so code coverage knows that the pointer is invalid
    if (GetCodeGenConfig()._codeCoverage)
    {
        std::list<InspectableVariable>* codeCoverageVariables;

        codeCoverageVariables = &program._codeCoverageVariables;

        std::for_each(codeCoverageVariables->begin(), codeCoverageVariables->end(),
                      [&removedBasicBlocks](InspectableVariable& inspectableVariable)
                      {
                          if (inspectableVariable._inspectionType == InspectableVariableType::CodeCoverage)
                          {
                              const auto it = removedBasicBlocks.find(inspectableVariable._codeCoverage._basicBlock);
                              if (it != removedBasicBlocks.end())
                              {
                                  inspectableVariable._removed = true;
                              }
                          }
                      });
    }

    // Build a list of basic blocks that are points where a function call returns
    std::unordered_set<const BasicBlock*> returnSites;

    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        returnSites.insert(contextSaver._afterCall);
    }

    // Add pipeline registers
    for (Function& function : program._functions)
    {
        CompileTimer ct(*g_compiler, "Pipeline");

        // Build the control flow graph
        ControlFlowGraph controlFlowGraph(function, program, OperationEnumerationMode::Scheduled,
                                          ControlFlowGraphPhase::PrePipeline);

        // Propagate constants one more time to understand which FIFO predicates are constant
        if (GetCodeGenConfig()._optimize > 0)
        {
            ConstantPropagation(program, controlFlowGraph, function, OperationEnumerationMode::Scheduled);
        }

        // This is set of basic blocks that need to be processed
        DataFlowWorkList workList(controlFlowGraph, function);

        // Compute live-in registers for each basic block
        RegisterSetMap liveInMap;
        ComputeLiveInMap(program, function, controlFlowGraph, returnSites, liveInMap,
                         OperationEnumerationMode::Scheduled);

        FunctionClockGateMap clockGateMap;
        const bool clockGatingChangedIR =
            ComputeClockGating(program, function, OperationEnumerationMode::Scheduled, &clockGateMap);

        // IR should not be changed when OperationEnumerationMode::Scheduled is specified
        assert(!clockGatingChangedIR);

        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            // Not done at optimization levels less than 2
            // to save compile time
            if (codeGenConfig._optimize > 1)
            {
                CompileTimer ct(*g_compiler, "LocalDataPropagation");

                // Place long data dependencies in FIFOs
                LocalDataPropagation(*context._program, basicBlock, liveInMap, returnSites);
            }

            // Remap register accesses to pipeline registers
            // Intrinsic basic blocks are skipped because they are empty
            AddPipelineRegisters(context, basicBlock, liveInMap, returnSites, SafeLookup(clockGateMap, &basicBlock));

            // Change FanOut operations to Mov, mark output registers as needing to be preserved
            TransformFanOutOperations(*context._program, basicBlock);
        }

        RemoveConditionalIgnore(function);
    }

    // Record that control flow graphs in pre-pipeline mode can no longer be constructed
    // because pipeline registers are about to be added
    assert(program._prePipelineCfgAllowed);
    program._prePipelineCfgAllowed = false;

    // Remove unnecessary string operations
    RemoveStringOps(program);

    // Create FIFOs for context saver inputs
    CreateContextSavers(program);

    // Fill in successor FIFO indices
    SetSuccessorFifos(program);

    // Mark fifos as being reorder buffers
    SetupReorderBuffers(program);

    // Connect loop generators
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            GenerateLoops(*context._program, basicBlock);
        }
    }

    // Remove loop generators that for pipelined functions that are never called
    {
        const auto predicate = [](const LoopGenerator& loopGenerator) { return !loopGenerator._initialized; };

        program._loopGenerators.remove_if(predicate);
    }

    // Set _inputFifoIsLoopGenerator (after removing loop generators for unreferenced functions)
    for (const LoopGenerator& loopGenerator : program._loopGenerators)
    {
        loopGenerator._function->_start->_inputFifoIsLoopGenerator = true;
    }

    // Simulate placement to guess which nodes are close to each other
    // Must happen before fifo merging
    {
        CompileTimer ct(*g_compiler, "Virtual Placement");
        PlaceNodes(*context._program);
    }

    // Save information related to synchronous extern functions
    SetupSynchronousExterns(program);

    // If a FIFO has more than 1 write location, split it into N FIFOs with a merger at the destination
    MergeFifos(*context._program);

    // Enable store-and-forward for fifos written with the [[transaction_size]] attribute
    SetFifoTransactionSizes(*context._program);

    // Remove per-function semaphores that will never reach their limit
    // false means fifo sizing has already occurred
    RemoveSemaphores(context, false);

    // Determine which fifos do not need almost_full or can have depth = 1
    SelectFifoImplementation(*context._program);

    // Record that control flow graphs in post-pipeline mode can be constructed
    // after fifo writes have been added
    assert(!program._postPipelineCfgAllowed);
    program._postPipelineCfgAllowed = true;

    // Add conditional mov operations associated with each Enqueue operation
    // These move locals from pipeline registers into destination FIFOs
    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            AddFifoWrites(*context._program, basicBlock);

            // Don't require backends to handle MovCrossFunction
            // and enable RemoveDeadOperations to find more dead code
            RemoveMoveCrossFunction(basicBlock);
        }

        // AddFifoWrites can cause some operations to no longer be necessary
        // For example, if the source of a FIFO write is a literal
        // Remove those operations
        // Don't do this when the optimization level is set to 0
        // to preserve variables that could be useful for debugging
        if (codeGenConfig._optimize > 0)
        {
            RemoveDeadOperationsFromFunction(function, program, OperationEnumerationMode::Scheduled,
                                             ControlFlowGraphPhase::PostPipeline);
        }
    }

    // Narrow the width of registers that contain constant or unused bits
    if (codeGenConfig._optimize > 1)
    {
        CompileTimer ct(*g_compiler, "Sparse to dense pipeline registers");
        SparseToDenseReg(program);
    }

    // Reduce pipeline register/wire width based on how it is used
    // This must run after AddFifoWrites
    if (GetCodeGenConfig()._optimize)
    {
        for (Function& function : program._functions)
        {
            for (BasicBlock& basicBlock : function._basicBlocks)
            {
                ReduceRegisterWidth(*context._program, basicBlock);
            }
        }
    }

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            DetermineStallablePipelineStages(basicBlock);
        }
    }

    // For basic blocks that write into FIFOs, set the FIFO size and almost-full values
    SizeFifos(*context._program);

    // Setup _callableEntryPoints & _externClassInstanceEntryPoints
    assert(context._program->_callableEntryPoints.empty());
    assert(context._program->_externClassInstanceEntryPoints.empty());

    for (const EntryPoint& entryPoint : context._program->_entryPoints)
    {
        assert(!entryPoint._instances.empty());

        const Function* const firstInstance = entryPoint._instances.front();

        if (firstInstance->_externClassInstance)
        {
            // This path is taken for functions connected to export class callbacks
            context._program->_externClassInstanceEntryPoints.push_back(&entryPoint);
        }
        else
        {
            context._program->_callableEntryPoints.push_back(&entryPoint);
        }
    }

    // Create entry points for extern class callbacks
    const std::set<FunctionInstance>& externClassCallees =
        g_compiler->GetFunctionInstanceEnumerator().GetExternalClassInstanceCallees();

    for (const FunctionInstance fi : externClassCallees)
    {
        Function* const function = fi._functionNode->GetInstance(fi._objectName).GetFunction();

        EntryPoint ep = {};

        ep._classType = fi._functionNode->GetClassType();
        ep._scope = fi._functionNode->GetContainingScope();
        ep._functionName = fi._functionNode->GetName();
        ep._instances.push_back(function);
        ep._backendName = function->GetBackendName();

        context._program->_entryPoints.push_back(ep);

        context._program->_externClassInstanceEntryPoints.push_back(&context._program->_entryPoints.back());
    }

    if (codeGenConfig._dumpIR)
    {
        str << "\nAfter scheduling\n";
        str << *context._program;
    }

    // Count # of write and read ports for each memory
    // Runs after placement and any optimization that can remove operations from the IR
    AssignMemoryPorts(program);

    // Remove per-function semaphores that will never reach their limit
    // true means fifo sizing has already occurred
    RemoveSemaphores(context, true);

    // Loop through the program and update the function occurance counter
    std::map<std::string, size_t> nameToIndex;
    for (Function& function : program._functions)
    {
        // Add the object name (if it is not the global object name)
        const std::string objectName =
            (function._objectName == g_globalObjectName) ? std::string("") : (function._objectName + "_");
        const std::string funcName = objectName + function._name;
        const size_t nameIndex = nameToIndex[funcName]++;

        if (nameIndex > 0)
        {
            function._uniqueName = funcName + "_" + std::to_string(nameIndex);
        }
        else
        {
            function._uniqueName = funcName;
        }
    }

    // Unless inspectable generation was disabled via --no-inspection compiler option, generate inspectable variables
    if (GetCodeGenConfig()._inspection)
    {
        CompileTimer ct(*g_compiler, "Inspection");

        // Generate inspectable variables for control inspection (where threads are)
        // Must run before SortInspectables to get good sorting. We do this conditionally based
        // on whether --no-control-inspection compiler option was specified or not.
        if (GetCodeGenConfig()._controlInspection)
        {
            AddControlInspectables(program);
        }

        SortInspectables(program);

        PrependHashInspectable(program);

        // Generate inspectable variables for globals/memories that could have data races
        // Must run after SortInspectables, because the race condition inspectable has no position/registers
        AddRaceConditionInspectables(program);

        // Add placeholder for connections to inspectable chains in extern class instances
        AddExternalClassInstanceInspectables(program);
    }
    else
    {
        // Inspectables were disabled - delete all inspectable variables so we don't emit code for them during back-end
        // processing
        program._inspectableVariables.clear();
    }

    for (Function& function : program._functions)
    {
        for (BasicBlock& basicBlock : function._basicBlocks)
        {
            AssertScheduledOperationsHaveLocations(program, basicBlock);
        }
    }

    // The verilog backend emits files that contain metadata about unsued and constant bits
    // This information is not used by other backends
    if ((codeGenConfig._optimize > 0) && g_compiler->IsCompilingToVerilog())
    {
        DetermineConstantBits(program);

        DetermineUnusedBits(program);
    }

    SortEntryPoints(program);

    DetectConstantLoopCounts(program);

    RemoveUnnecessaryBypass(program);

    ClampRegisterNameLengths(program);

    ComputeIntraStageRegisters(program);

    FlattenContainerPaths(program);

    // Collect per-stage line numbers for the debug symbols, if not already
    if ((GetCodeGenConfig()._debug) && (GetCodeGenConfig()._optimize > 0))
    {
        for (Function& function : program._functions)
        {
            for (BasicBlock& basicBlock : function._basicBlocks)
            {
                CollectPerStageLineNumberInfo(program, basicBlock);
            }
        }
    }
}
