// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

class DataFlowWorkList
{
  public:
    DataFlowWorkList(ControlFlowGraph& controlFlowGraph, Function& function);

    bool Empty() const;

    BasicBlock* Pop();

    void AddSuccessors(BasicBlock* basicBlock);

    void AddPredecessors(BasicBlock* basicBlock);

    void AddBasicBlock(BasicBlock* basicBlock);

    void PopAll();

  private:
    DataFlowWorkList& operator=(const DataFlowWorkList&) const;

    ControlFlowGraph& _controlFlowGraph;

    std::set<BasicBlock*> _workingSet;

    const Function* const _function;
};

// Generic data-flow analysis

// T is information that is tracked per local register
template <typename T> class DataFlowAnalysis
{
  public:
    // Maps register index to tracked type
    using RegToT = std::map<size_t, T>;

    enum class Direction
    {
        // Propagate information from predecessors to successors
        Forward,

        // Propagate information from successors to predecessors
        Backward
    };

    // A function that translates registers in T
    // from bb: "from" to bb: "to"
    using TranslateT = std::function<T(const T&, ControlFlowGraph&, const Direction direction, BasicBlock* const from,
                                       BasicBlock* const to)>;

    // Computes intersection or union of 2 Ts
    using JoinT = std::function<T(const T&, const T&)>;

    // Callback that runs once per operation in a basic block
    // The operation is const because the IR should not be modified
    // until data flow analysis has converged to a fixed point
    using OpCallback = std::function<void(const Operation&, RegToT&)>;

    // Translate register index from 1 basic block to another
    // Returns an uninitialized value if the register
    // is not valid in "to"
    // For example, in the forward case if the register
    // is written by "from", but registerIndex is the original un-renamed index
    static boost::optional<size_t> TranslateRegisterIndex(const size_t registerIndex, ControlFlowGraph& cfg,
                                                          const Direction direction, BasicBlock* const from,
                                                          BasicBlock* const to)
    {
        if (Direction::Forward == direction)
        {
            return cfg.RenameLocalToSuccessor(from, to, registerIndex);
        }
        else
        {
            assert(Direction::Backward == direction);
            return cfg.RenameLocalToPredecessor(to, from, registerIndex);
        }
    }

    DataFlowAnalysis(Program& program, Function& function, ControlFlowGraph& cfg, const Direction direction,
                     const OperationEnumerationMode mode, const TranslateT& translateCb, const JoinT& joinCb,
                     const OpCallback& opCallback)
    {
        DataFlowWorkList workList(cfg, function);

        workList.PopAll();

        // Add either the first or the last basic block only
        // Others will be added necessary
        if (!function._basicBlocks.empty())
        {
            if (Direction::Forward == direction)
            {
                workList.AddBasicBlock(&(function._basicBlocks.front()));
            }
            else
            {
                workList.AddBasicBlock(&(function._basicBlocks.back()));
            }
        }

        while (!workList.Empty())
        {
            BasicBlock* const bb = workList.Pop();

            // Compute join of information propagated from predecessors or successors
            const std::set<BasicBlock*>& otherBbs =
                (direction == Direction::Forward) ? cfg.GetPredecessors(bb) : cfg.GetSuccessors(bb);

            bool isFirst = true;
            RegToT registerMap;

            for (BasicBlock* const otherBb : otherBbs)
            {
                if (_processedBasicBlocks.end() == _processedBasicBlocks.find(otherBb))
                {
                    // OtherBb has not been processed yet, so do not consider data flowing from it to bb
                    continue;
                }

                const RegToT& otherRegisterMap = _basicBlockMaps[otherBb];

                // Translate registers from otherRegisterMap to bb
                RegToT translatedOtherRegisterMap;

                for (const auto& p : otherRegisterMap)
                {
                    // Translate register index in p.first
                    const boost::optional<size_t> translatedRegister =
                        TranslateRegisterIndex(p.first, cfg, direction, otherBb, bb);

                    if (translatedRegister)
                    {
                        // Translate p.second
                        const T translatedT = translateCb(p.second, cfg, direction, otherBb, bb);

                        SafeInsert(translatedOtherRegisterMap, *translatedRegister, translatedT);
                    }
                }

                if (isFirst)
                {
                    // This is the first predecessor/successor
                    registerMap = translatedOtherRegisterMap;

                    isFirst = false;
                }
                else
                {
                    // Compute the intersection of the information currently collected
                    // with the information for "otherBb"
                    registerMap = JoinRegToT(registerMap, translatedOtherRegisterMap, joinCb);
                }
            }

            // Iterate through all operations in the basic block
            const auto callback = [&](Operation& op) { opCallback(op, registerMap); };

            if (Direction::Forward == direction)
            {
                ForEachOperationForward(*bb, callback, mode);
            }
            else
            {
                assert(Direction::Backward == direction);
                ForEachOperationReverse(*bb, callback, mode);
            }

            // Only local registers should be in the register maps
            for (const auto& p : registerMap)
            {
                assert(IsLocalRegisterType(program._registerTable[p.first]._type));
            }

            // Check to see if new information was computed for this basic block
            if (InsertAndCheckForChange(_basicBlockMaps, bb, registerMap))
            {
                // Something changed
                if (Direction::Forward == direction)
                {
                    workList.AddSuccessors(bb);
                }
                else
                {
                    assert(Direction::Backward == direction);
                    workList.AddPredecessors(bb);
                }
            }

            // Record that data flowing from this basic block to others is valid
            _processedBasicBlocks.insert(bb);
        }
    }

    const RegToT& GetRegisterMap(BasicBlock* const bb) { return _basicBlockMaps[bb]; }

  private:
    RegToT JoinRegToT(const RegToT& a, const RegToT& b, const JoinT& joinCb)
    {
        RegToT result;

        // Register values must appear in both maps
        for (const auto& p : a)
        {
            const auto it = b.find(p.first);
            if (it != b.end())
            {
                // Join "T" values
                SafeInsert(result, p.first, joinCb(p.second, it->second));
            }
        }

        return result;
    }

    std::map<BasicBlock*, RegToT> _basicBlockMaps;

    // The set of basic blocks that have been traversed at least once
    std::set<BasicBlock*> _processedBasicBlocks;
};