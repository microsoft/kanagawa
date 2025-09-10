// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

enum class ControlFlowGraphPhase
{
    // Control flow graph operates on IR before pipeline registers have been added
    // Unscheduled and scheduled IR are both supported
    PrePipeline,

    // Control flow graph operates on scheduled IR after pipeline registers have been added.
    PostPipeline
};

class ControlFlowGraphImpl
{
  public:
    virtual ~ControlFlowGraphImpl() {}

    virtual boost::optional<size_t> RenameLocalToPredecessor(BasicBlock* const predecessor, BasicBlock* const successor,
                                                             const size_t registerIndexInSuccessor) = 0;

    virtual boost::optional<size_t> RenameLocalToSuccessor(BasicBlock* const predecessor, BasicBlock* const successor,
                                                           const size_t registerIndexInPredecessor) = 0;
};

class ControlFlowGraph
{
  public:
    ControlFlowGraph(Function& function, const Program& program, const OperationEnumerationMode mode,
                     const ControlFlowGraphPhase phase);

    const std::set<BasicBlock*>& GetSuccessors(BasicBlock* predecessor);

    const std::set<BasicBlock*>& GetPredecessors(BasicBlock* successor);

    boost::optional<size_t> RenameLocalToPredecessor(BasicBlock* const predecessor, BasicBlock* const successor,
                                                     const size_t registerIndexInSuccessor);

    boost::optional<size_t> RenameLocalToSuccessor(BasicBlock* const predecessor, BasicBlock* const successor,
                                                   const size_t registerIndexInPredecessor);

    OperationEnumerationMode GetEnumerationMode() const { return _operationEnumerationMode; }

  private:
    std::unique_ptr<ControlFlowGraphImpl> _impl;

    std::map<BasicBlock*, std::set<BasicBlock*>> _successors;

    std::map<BasicBlock*, std::set<BasicBlock*>> _predecessors;

    OperationEnumerationMode _operationEnumerationMode;
};

class ControlFlowGraphPrePipeline : public ControlFlowGraphImpl
{
  public:
    ControlFlowGraphPrePipeline(Function& function, const Program& program, const OperationEnumerationMode mode);

    boost::optional<size_t> RenameLocalToPredecessor(BasicBlock* const predecessor, BasicBlock* const successor,
                                                     const size_t registerIndexInSuccessor) override;

    boost::optional<size_t> RenameLocalToSuccessor(BasicBlock* const predecessor, BasicBlock* const successor,
                                                   const size_t registerIndexInPredecessor) override;

  private:
    // (predecessor, successor)
    using RenamingTableKey = std::pair<BasicBlock*, BasicBlock*>;

    boost::optional<size_t> RenameLocalImpl(BasicBlock* const predecessor, BasicBlock* const successor,
                                            const size_t registerIndexInSource, const RegisterSet& registersToSkip,
                                            const std::map<RenamingTableKey, RegisterIndexMap>& bbPairToRenamingTable);

    std::map<BasicBlock*, RegisterSet> _originalWrittenRegisters;

    std::map<BasicBlock*, RegisterSet> _renamedWrittenRegisters;

    // Maps original variable index in successor to variable index in predecessor
    std::map<RenamingTableKey, RegisterIndexMap> _successorToPredecessorTable;

    // Maps renamed variable index in predecessor to variable index in successor
    std::map<RenamingTableKey, RegisterIndexMap> _predecessorToSuccessorTable;
};

class ControlFlowGraphPostPipeline : public ControlFlowGraphImpl
{
  public:
    ControlFlowGraphPostPipeline(Function& function, const Program& program);

    boost::optional<size_t> RenameLocalToPredecessor(BasicBlock* const predecessor, BasicBlock* const successor,
                                                     const size_t registerIndexInSuccessor) override;

    boost::optional<size_t> RenameLocalToSuccessor(BasicBlock* const predecessor, BasicBlock* const successor,
                                                   const size_t registerIndexInPredecessor) override;

  private:
    // (predecessor, successor)
    using BasicBlockPair = std::pair<BasicBlock*, BasicBlock*>;

    // (fifo index, register index)
    using FifoAndReg = std::pair<size_t, size_t>;

    // (fifo index, bit offset)
    using FifoAndOffset = std::pair<size_t, size_t>;

    boost::optional<size_t> RenameLocalImpl(BasicBlock* const predecessor, BasicBlock* const successor,
                                            const size_t registerIndexInSuccessor,
                                            const std::map<FifoAndReg, size_t>& fifoAndRegToOffset,
                                            const std::map<FifoAndOffset, size_t>& fifoAndOffsetToReg);

    // BasicBlockPair to fifo index that links them
    std::map<BasicBlockPair, size_t> _basicBlockPairToFifo;

    // FifoAndReg to bit offset within the fifo
    std::map<FifoAndReg, size_t> _fifoAndRegReadToOffset;
    std::map<FifoAndReg, size_t> _fifoAndRegWriteToOffset;

    // FifoAndOffset to index of register written into or read out of the fifo
    std::map<FifoAndOffset, size_t> _fifoAndOffsetToReadReg;
    std::map<FifoAndOffset, size_t> _fifoAndOffsetToWriteReg;
};