// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

ControlFlowGraph::ControlFlowGraph(Function& function, const Program& program, const OperationEnumerationMode mode,
                                   const ControlFlowGraphPhase phase)
    : _operationEnumerationMode(mode)
{
    if (phase == ControlFlowGraphPhase::PostPipeline)
    {
        assert(mode == OperationEnumerationMode::Scheduled);
        assert(!program._prePipelineCfgAllowed);
        assert(program._postPipelineCfgAllowed);

        _impl = std::unique_ptr<ControlFlowGraphImpl>(new ControlFlowGraphPostPipeline(function, program));
    }
    else
    {
        assert(phase == ControlFlowGraphPhase::PrePipeline);
        assert(program._prePipelineCfgAllowed);
        assert(!program._postPipelineCfgAllowed);

        _impl = std::unique_ptr<ControlFlowGraphImpl>(new ControlFlowGraphPrePipeline(function, program, mode));
    }

    for (BasicBlock& block : function._basicBlocks)
    {
        const auto callback = [&](const Operation& op)
        {
            if (op._getSuccessorBlock)
            {
                assert(Opcode::Enqueue == op._opcode);

                BasicBlock* const successor = op._getSuccessorBlock();

                if (successor->_function == &function)
                {
                    _successors[&block].insert(successor);

                    _predecessors[successor].insert(&block);
                }
            }
        };

        ForEachOperationForward(block, callback, mode);
    }
}

const std::set<BasicBlock*>& ControlFlowGraph::GetSuccessors(BasicBlock* predecessor)
{
    return _successors[predecessor];
}

const std::set<BasicBlock*>& ControlFlowGraph::GetPredecessors(BasicBlock* successor)
{
    return _predecessors[successor];
}

boost::optional<size_t> ControlFlowGraph::RenameLocalToPredecessor(BasicBlock* const predecessor,
                                                                   BasicBlock* const successor,
                                                                   const size_t registerIndexInSuccessor)
{
    return _impl->RenameLocalToPredecessor(predecessor, successor, registerIndexInSuccessor);
}

boost::optional<size_t> ControlFlowGraph::RenameLocalToSuccessor(BasicBlock* const predecessor,
                                                                 BasicBlock* const successor,
                                                                 const size_t registerIndexInPredecessor)
{
    return _impl->RenameLocalToSuccessor(predecessor, successor, registerIndexInPredecessor);
}

ControlFlowGraphPrePipeline::ControlFlowGraphPrePipeline(Function& function, const Program& program,
                                                         const OperationEnumerationMode mode)
{
    for (BasicBlock& block : function._basicBlocks)
    {
        // Renamed versions of all written registers
        RegisterSet renamedWrittenRegisters;

        // Original versions of all renamed registers
        RegisterSet originalWrittenRegisters;

        const auto callback = [&](const Operation& op)
        {
            for (const DestinationOperand& dstOp : op._dst)
            {
                if (DestinationOperandType::Register == dstOp.Type())
                {
                    const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

                    const RegisterType registerType = program._registerTable[registerIndex]._type;

                    // PostPipeline CFG implementation should be used if there are pipeline registers in the IR
                    assert(RegisterType::Pipeline != registerType);

                    if (IsLocalRegisterType(registerType))
                    {
                        SafeInsert(renamedWrittenRegisters, registerIndex);
                    }
                }
            }

            if (op._getSuccessorBlock)
            {
                assert(Opcode::Enqueue == op._opcode);

                BasicBlock* const successor = op._getSuccessorBlock();

                if (successor->_function == &function)
                {
                    const std::pair<BasicBlock*, BasicBlock*> key(&block, successor);

                    SafeInsert(_successorToPredecessorTable, key, op._renamingTable);

                    SafeInsert(_predecessorToSuccessorTable, key, op._reverseRenamingTable);

                    for (const size_t renamed : renamedWrittenRegisters)
                    {
                        const auto it = op._reverseRenamingTable.find(renamed);

                        if (it != op._reverseRenamingTable.end())
                        {
                            originalWrittenRegisters.insert(it->second);
                        }
                    }
                }
            }
        };

        ForEachOperationForward(block, callback, mode);

        SafeInsert(_originalWrittenRegisters, &block, originalWrittenRegisters);

        SafeInsert(_renamedWrittenRegisters, &block, renamedWrittenRegisters);
    }
}

boost::optional<size_t>
ControlFlowGraphPrePipeline::RenameLocalImpl(BasicBlock* const predecessor, BasicBlock* const successor,
                                             const size_t registerIndexInSource, const RegisterSet& registersToSkip,
                                             const std::map<RenamingTableKey, RegisterIndexMap>& bbPairToRenamingTable)
{
    boost::optional<size_t> result;

    if (registersToSkip.end() != registersToSkip.find(registerIndexInSource))
    {
        // LocalToPredecessor: This is an renamed version of a register written by successor
        // LocalToSuccessor: This is an un-renamed version of a register written by predecessor

        // Return an invalid register index
        // to avoid propagating both the renamed and un-renamed registers
    }
    else
    {
        const std::pair<BasicBlock*, BasicBlock*> key(predecessor, successor);

        const RegisterIndexMap& renamingTable = SafeLookup(bbPairToRenamingTable, key);

        const auto it = renamingTable.find(registerIndexInSource);

        if (it != renamingTable.end())
        {
            // LocalToPredecessor map original to renamed
            // LocalToSuccessor: map renamed to original
            result = it->second;
        }
        else
        {
            // pass through
            result = registerIndexInSource;
        }
    }

    return result;
}

// Given a register index used by a successor, determine the corresponding register index
// in the predecessor
boost::optional<size_t> ControlFlowGraphPrePipeline::RenameLocalToPredecessor(BasicBlock* const predecessor,
                                                                              BasicBlock* const successor,
                                                                              const size_t registerIndexInSuccessor)
{
    return RenameLocalImpl(predecessor, successor, registerIndexInSuccessor, _renamedWrittenRegisters[successor],
                           _successorToPredecessorTable);
}

// Given a variable that is output of a predecessor
// Determine the corresponding register index in the successor
boost::optional<size_t> ControlFlowGraphPrePipeline::RenameLocalToSuccessor(BasicBlock* const predecessor,
                                                                            BasicBlock* const successor,
                                                                            const size_t registerIndexInPredecessor)
{
    return RenameLocalImpl(predecessor, successor, registerIndexInPredecessor, _originalWrittenRegisters[predecessor],
                           _predecessorToSuccessorTable);
}

ControlFlowGraphPostPipeline::ControlFlowGraphPostPipeline(Function& function, const Program& program)
{
    for (BasicBlock& block : function._basicBlocks)
    {
        const auto callback = [&](const Operation& op)
        {
            if (op._getSuccessorBlock)
            {
                assert(Opcode::Enqueue == op._opcode);

                BasicBlock* const successor = op._getSuccessorBlock();

                if (successor->_function == &function)
                {
                    const BasicBlockPair key(&block, successor);

                    SafeInsert(_basicBlockPairToFifo, key, op._flags._enqueue._successorFifo);
                }
            }

            const auto recordFifoRead = [&](const size_t srcOperandIndex)
            {
                assert(srcOperandIndex < op._src.size());

                const SourceOperand& srcOp = op._src[srcOperandIndex];
                const DestinationOperand& dstOp = op._dst[0];

                const size_t readReg = dstOp.GetAccessedRegister()._registerIndex;

                const FifoSubset& fs = srcOp.GetFifoSubset();

                const FifoAndReg far(fs._registerIndex, readReg);

                SafeInsert(_fifoAndRegReadToOffset, far, fs._offset);

                const FifoAndOffset fao(fs._registerIndex, fs._offset);

                SafeInsert(_fifoAndOffsetToReadReg, fao, readReg);
            };

            switch (op._opcode)
            {
            case Opcode::Mov:
            {
                assert(1 == op._src.size());
                assert(1 == op._dst.size());

                const SourceOperand& srcOp = op._src[0];
                const DestinationOperand& dstOp = op._dst[0];

                if ((SourceOperandType::Register == srcOp.Type()) && (DestinationOperandType::Fifo == dstOp.Type()))
                {
                    // Register written into a fifo
                    const size_t writeReg = srcOp.GetAccessedRegister()._registerIndex;

                    const FifoSubset& fs = dstOp.GetFifoSubset();

                    const FifoAndReg far(fs._registerIndex, writeReg);

                    SafeInsert(_fifoAndRegWriteToOffset, far, fs._offset);

                    const FifoAndOffset fao(fs._registerIndex, fs._offset);

                    SafeInsert(_fifoAndOffsetToWriteReg, fao, writeReg);
                }
                else if ((SourceOperandType::Fifo == srcOp.Type()) &&
                         (DestinationOperandType::Register == dstOp.Type()))
                {
                    // Register read out of a fifo
                    recordFifoRead(0);
                }
            }
            break;

            case Opcode::ReadSelectedFifo:
            {
                // Register read out of a fifo
                assert(1 == op._dst.size());

                for (size_t i = 0; i < op._src.size(); i++)
                {
                    recordFifoRead(i);
                }
            }
            break;

            default:
                break;
            }
        };

        ForEachOperationForward(block, callback, OperationEnumerationMode::Scheduled);
    }
}

boost::optional<size_t> ControlFlowGraphPostPipeline::RenameLocalImpl(
    BasicBlock* const predecessor, BasicBlock* const successor, const size_t regIndex,
    const std::map<FifoAndReg, size_t>& fifoAndRegToOffset, const std::map<FifoAndOffset, size_t>& fifoAndOffsetToReg)
{
    boost::optional<size_t> result;

    // Lookup the FIFO that links the successor and predecessor
    const BasicBlockPair bbp(predecessor, successor);

    const size_t fifoIndex = SafeLookup(_basicBlockPairToFifo, bbp);

    // Lookup the bit offset in the FIFO at the source location
    const FifoAndReg far(fifoIndex, regIndex);

    const auto it = fifoAndRegToOffset.find(far);
    if (it != fifoAndRegToOffset.end())
    {
        const size_t offset = it->second;

        // Lookup the register in the destination location based on the offset
        const FifoAndOffset fao(fifoIndex, offset);

        const auto it2 = fifoAndOffsetToReg.find(fao);
        if (it2 != fifoAndOffsetToReg.end())
        {
            result = it2->second;
        }
    }

    return result;
}

boost::optional<size_t> ControlFlowGraphPostPipeline::RenameLocalToPredecessor(BasicBlock* const predecessor,
                                                                               BasicBlock* const successor,
                                                                               const size_t registerIndexInSuccessor)
{
    return RenameLocalImpl(predecessor, successor, registerIndexInSuccessor, _fifoAndRegReadToOffset,
                           _fifoAndOffsetToWriteReg);
}

boost::optional<size_t> ControlFlowGraphPostPipeline::RenameLocalToSuccessor(BasicBlock* const predecessor,
                                                                             BasicBlock* const successor,
                                                                             const size_t registerIndexInPredecessor)
{
    return RenameLocalImpl(predecessor, successor, registerIndexInPredecessor, _fifoAndRegWriteToOffset,
                           _fifoAndOffsetToReadReg);
}