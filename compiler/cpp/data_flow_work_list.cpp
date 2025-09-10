// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

DataFlowWorkList::DataFlowWorkList(ControlFlowGraph& controlFlowGraph, Function& function)
    : _controlFlowGraph(controlFlowGraph), _function(&function)
{
    // Add all basic blocks that are part of the target function to the working set
    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        _workingSet.insert(&basicBlock);
    }
}

bool DataFlowWorkList::Empty() const { return _workingSet.empty(); }

BasicBlock* DataFlowWorkList::Pop()
{
    const auto it = _workingSet.begin();
    BasicBlock* const basicBlock = *it;
    _workingSet.erase(it);

    // The working set should only ever include basic blocks for the target function
    assert(basicBlock->_function == _function);
    return basicBlock;
}

void DataFlowWorkList::PopAll() { _workingSet.clear(); }

void DataFlowWorkList::AddSuccessors(BasicBlock* basicBlock)
{
    const std::set<BasicBlock*>& successors = _controlFlowGraph.GetSuccessors(basicBlock);

    for (BasicBlock* const successor : successors)
    {
        AddBasicBlock(successor);
    }
}

void DataFlowWorkList::AddPredecessors(BasicBlock* basicBlock)
{
    const std::set<BasicBlock*>& predecessors = _controlFlowGraph.GetPredecessors(basicBlock);

    for (BasicBlock* const predecessor : predecessors)
    {
        AddBasicBlock(predecessor);
    }
}

void DataFlowWorkList::AddBasicBlock(BasicBlock* basicBlock)
{
    // The working set should only ever include basic blocks for the target function
    assert(basicBlock->_function == _function);

    _workingSet.insert(basicBlock);
}
