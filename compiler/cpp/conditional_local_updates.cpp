// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

ConditionalLocalUpdates::ConditionalLocalUpdates(IRContext& context, const Location& location)
    : _context(context), _location(location), _currentCase(0),
      _currentPredicateRegister({c_invalidAccessedRegisterIndex}), _currentPredicateMode(PredicateMode::Normal),
      _backupInsertLocation(*context._basicBlock, *context._program)
{
    // Register for notifications when a local variable is read/written
    context._registerAccessNotificationStack.push_back(this);

    _conditionStackDepth = context.GetConditionStackDepth();
}

ConditionalLocalUpdates::~ConditionalLocalUpdates()
{
    if (!_operationsToAdd.empty())
    {
        // Find the location to insert operations at
        // (the end of the basic block operation list when the constructor was called)
        const OperationLocationRecord::ListAndIterator insertLocation =
            _backupInsertLocation.GetInsertListAndIterator();

        insertLocation._list->splice(insertLocation._iterator, _operationsToAdd);
    }

    // Copy from updated value registers to the original register
    for (const auto& p : _updatedRegisters)
    {
        const AccessedRegister originalRegister = {p.first};

        const UpdatedRegister& updatedRegister = p.second;

        assert(!updatedRegister._writtenCases.empty());

        Operation op = {};

        op._opcode = Opcode::Mov;
        op._src.push_back(updatedRegister._updatedValueRegister);
        op._dst.push_back(originalRegister);
        op.InsertLocation(_location);

        _context._basicBlock->_operations.push_back(op);
    }

    // Unregister for notifications
    assert(!_context._registerAccessNotificationStack.empty());
    assert(_context._registerAccessNotificationStack.back() == this);
    _context._registerAccessNotificationStack.pop_back();
}

void ConditionalLocalUpdates::BeginCase(const size_t predicateRegisterIndex, const PredicateMode mode)
{
    // Make a copy of the predicate
    // This handles the case where the predicate is modified in the case
    _currentPredicateRegister._registerIndex = AllocateDuplicateRegister(_context._program, predicateRegisterIndex);

    {
        const AccessedRegister srcReg = {predicateRegisterIndex};

        Operation op = {};

        op._opcode = Opcode::Mov;
        op._src.push_back(srcReg);
        op._dst.push_back(_currentPredicateRegister);
        op.InsertLocation(_location);

        _context._basicBlock->_operations.push_back(op);
    }

    _currentPredicateMode = mode;

    // Combine the predicate value with previous predicate values
    // Also, save combined predicate value in the context
    // for operations with side effects (global variable writes)
    if (PredicateMode::Normal == mode)
    {
        _context.PushPredicate(_currentPredicateRegister._registerIndex, _location);
    }
    else
    {
        assert(PredicateMode::Inverse == mode);
        _context.PushInversePredicate(_currentPredicateRegister._registerIndex, _location);
    }
}

void ConditionalLocalUpdates::EndCase()
{
    const bool clockGating = GetCodeGenConfig().ControlClockGatingEnabled();

    // For each register written by this case
    for (auto& p : _updatedRegisters)
    {
        const size_t originalRegisterIndex = p.first;

        UpdatedRegister& updatedRegister = p.second;

        if (updatedRegister._writtenCases.end() == updatedRegister._writtenCases.find(_currentCase))
        {
            continue;
        }

        const AccessedRegister originalReg = {originalRegisterIndex};

        if (clockGating)
        {
            // Emit a ConditionalIgnore operation that will hint that the operand
            // can be ignored if the combined predicate is 0
            // The combined predicate is used to ensure that clocks are gated
            // if any level of the predicate stack is 0
            _context.PredicatedIgnore(originalReg, _location);
        }

        if (0 == _currentCase)
        {
            // Save location of the select the select operation
            // This operation may be optimized later
            assert(!updatedRegister._firstUpdateLocation);

            updatedRegister._firstUpdateLocation =
                std::make_unique<OperationLocationRecord>(*_context._basicBlock, *_context._program);
        }

        // Emit a select operation that will either:
        // keep _updatedValueRegister as is (if the predicate is 0)
        // or set _updatedValueRegister equal to the value computed by this case (if the predicate is 1)
        {
            Operation op = {};

            op._opcode = Opcode::Select;
            op._src.push_back(_currentPredicateRegister);

            // Predicate modes are implemented by swapping select operands
            if (PredicateMode::Normal == _currentPredicateMode)
            {
                op._src.push_back(updatedRegister._updatedValueRegister);
                op._src.push_back(originalReg);
            }
            else
            {
                assert(PredicateMode::Inverse == _currentPredicateMode);

                op._src.push_back(originalReg);
                op._src.push_back(updatedRegister._updatedValueRegister);
            }

            op._dst.push_back(updatedRegister._updatedValueRegister);

            op.InsertLocation(_location);

            _context._basicBlock->_operations.push_back(op);
        }

        // Restore the backup value of the register
        {
            Operation op = {};

            op._opcode = Opcode::Mov;
            op._dst.push_back(originalReg);
            op._src.push_back(updatedRegister._backupRegister);
            op.InsertLocation(_location);

            _context._basicBlock->_operations.push_back(op);
        }
    }

    _context.PopPredicate();

    _currentCase++;
}

// Called when IR is generated that writes to a local variable
void ConditionalLocalUpdates::NotifyWrite(const size_t registerIndex, const size_t registerConditionStackDepth)
{
    // Check to see if this register was in-scope
    // when the condition started.
    // If not, then the local variable is declared
    // inside the condition and there is no need
    // for a shadow register.
    if (registerConditionStackDepth >= _conditionStackDepth)
    {
        return;
    }

    // Allocate UpdatedRegister struct if needed
    UpdatedRegister& updatedRegister = _updatedRegisters[registerIndex];

    // Record that a write has occured in the current case
    updatedRegister._writtenCases.insert(_currentCase);

    if (!updatedRegister._initialized)
    {
        updatedRegister._initialized = true;

        // Allocate backup and updated value registers
        updatedRegister._backupRegister._registerIndex = AllocateDuplicateRegister(_context._program, registerIndex);

        updatedRegister._updatedValueRegister._registerIndex =
            AllocateDuplicateRegister(_context._program, registerIndex);

        // Record operations
        // to save the value of the register
        // and to initialize the value of updatedRegister._updatedValueRegister
        const AccessedRegister originalReg = {registerIndex};

        {
            Operation op = {};

            op._opcode = Opcode::Mov;
            op._dst.push_back(updatedRegister._backupRegister);
            op._src.push_back(originalReg);
            op.InsertLocation(_location);

            _operationsToAdd.push_back(op);
        }

        {
            Operation op = {};

            op._opcode = Opcode::Mov;
            op._dst.push_back(updatedRegister._updatedValueRegister);
            op._src.push_back(originalReg);
            op.InsertLocation(_location);

            _operationsToAdd.push_back(op);
        }
    }
}

// If called, then there will always be exactly 1 predicate bit set
// So there is no need to passthrough previous values
void ConditionalLocalUpdates::SetComplete()
{
    for (auto& p : _updatedRegisters)
    {
        const size_t originalRegisterIndex = p.first;

        UpdatedRegister& updatedRegister = p.second;

        const AccessedRegister originalReg = {originalRegisterIndex};

        // Check to see if this register is written by all cases
        if (updatedRegister._writtenCases.size() != _currentCase)
        {
            continue;
        }

        // Replace the first select operation
        // with a mov (the original value of the register cannot affect the final value)
        const OperationLocationRecord::ListAndIterator selectLocation =
            updatedRegister._firstUpdateLocation->GetInsertListAndIterator();

        Operation& op = *(selectLocation._iterator);

        assert(Opcode::Select == op._opcode);
        assert(updatedRegister._updatedValueRegister._registerIndex == op._dst[0].GetAccessedRegister()._registerIndex);

        Operation movOp = {};

        movOp._opcode = Opcode::Mov;
        movOp._dst = op._dst;
        movOp._src.push_back(originalReg);
        movOp._locations = op._locations;

        op = movOp;
    }
}

void IntersectBasicBlockClockGateMap(BasicBlockClockGateMap& bbClockGateMap, const size_t key,
                                     const ClockGateSet& toIntersect)
{
    const auto it = bbClockGateMap.find(key);

    if (it == bbClockGateMap.end())
    {
        // First use of this key so far
        // Logically intersect "toIntersect" with an infinite set
        SafeInsert(bbClockGateMap, key, toIntersect);
    }
    else
    {
        it->second = Intersection(toIntersect, it->second);
    }
}

// Set of ConditionalIgnore operations can be removed
using RedundantOperationSet = std::set<const Operation*>;

using dfa_t = DataFlowAnalysis<ClockGateSet>;

ClockGateSet TranslateClockGateSet(const ClockGateSet& setIn, ControlFlowGraph& cfg, const dfa_t::Direction direction,
                                   BasicBlock* const from, BasicBlock* const to)
{
    ClockGateSet result;

    for (const size_t gateRegister : setIn)
    {
        const boost::optional<size_t> translated =
            dfa_t::TranslateRegisterIndex(gateRegister, cfg, direction, from, to);

        if (translated)
        {
            result.insert(*translated);
        }
    }

    return result;
}

FunctionClockGateMap DfaToResult(dfa_t& dfa, Function& function)
{
    FunctionClockGateMap functionResult;

    for (BasicBlock& bb : function._basicBlocks)
    {
        functionResult[&bb] = dfa.GetRegisterMap(&bb);
    }

    return functionResult;
}

// Propagate ConditionalIgnore information forward through a function
// The local outputs of a operation can be gated if any gate in the intersection
// of the gate registers of the source operands is 0
FunctionClockGateMap ComputeClockGateMapForward(Program& program, Function& function, ControlFlowGraph& cfg,
                                                const OperationEnumerationMode mode,
                                                RedundantOperationSet& RedundantOperations)
{
    assert(RedundantOperations.empty());

    const auto operationCallback = [&](const Operation& op, dfa_t::RegToT& bbResult)
    {
        ClockGateSet gateRegisters;

        if (Opcode::ConditionalIgnore == op._opcode)
        {
            assert(1 == op._dst.size());
            assert(op._src.size() >= 1);

            ClockGateSet propgateRegisters;
            ClockGateSet explicitRegisters;

            for (size_t i = 0; i < op._src.size(); i++)
            {
                const SourceOperand& srcOp = op._src[i];

                // Other optimization passes ensure no literals
                assert(SourceOperandType::Register == srcOp.Type());

                const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;
                assert(RegisterType::Local == program._registerTable[registerIndex]._type);

                if (0 == i)
                {
                    // Forward propagate any predicates that apply to the value operand
                    assert(propgateRegisters.empty());
                    propgateRegisters = LookupWithDefault(bbResult, registerIndex, ClockGateSet());
                }
                else
                {
                    // All other operands are new predicates
                    explicitRegisters.insert(registerIndex);
                }
            }

            if (propgateRegisters == explicitRegisters)
            {
                // The explicit registers add no information
                RedundantOperations.insert(&op);
            }
            else
            {
                RedundantOperations.erase(&op);
            }

            assert(gateRegisters.empty());
            gateRegisters = explicitRegisters;
            Union(gateRegisters, propgateRegisters);
        }
        else
        {
            // Compute intersection of gates from all sources
            bool isFirst = true;

            for (const SourceOperand& srcOp : op._src)
            {
                if ((SourceOperandType::Register == srcOp.Type()) &&
                    (RegisterType::Local == program._registerTable[srcOp.GetAccessedRegister()._registerIndex]._type))
                {
                    const ClockGateSet srcOperandGateRegisters =
                        LookupWithDefault(bbResult, srcOp.GetAccessedRegister()._registerIndex, ClockGateSet());

                    if (isFirst)
                    {
                        gateRegisters = srcOperandGateRegisters;
                        isFirst = false;
                    }
                    else
                    {
                        gateRegisters = Intersection(gateRegisters, srcOperandGateRegisters);
                    }
                }
            }
        }

        // Forward propagate even if gateRegisters is empty
        // To record that those registers are written by this basic block (with no known clock gate)
        for (const DestinationOperand& dstOp : op._dst)
        {
            if ((DestinationOperandType::Register == dstOp.Type()) &&
                (RegisterType::Local == program._registerTable[dstOp.GetAccessedRegister()._registerIndex]._type))
            {
                // In the case of a loop, bbResult may already can contain an entry for this destination register
                IntersectBasicBlockClockGateMap(bbResult, dstOp.GetAccessedRegister()._registerIndex, gateRegisters);
            }
        }
    };

    dfa_t dfa(program, function, cfg, dfa_t::Direction::Forward, mode, TranslateClockGateSet, Intersection<size_t>,
              operationCallback);

    return DfaToResult(dfa, function);
}

// Propgate ConditionalIgnore information backward through a basic block
// For a particular operation, compute the intersection of all uses of all destination operands
// This set can be propgated backward to sources
FunctionClockGateMap ComputeClockGateMapBackward(Program& program, Function& function, ControlFlowGraph& cfg,
                                                 const OperationEnumerationMode mode,
                                                 RedundantOperationSet& RedundantOperations)
{
    assert(RedundantOperations.empty());

    const auto operationCallback = [&](const Operation& op, dfa_t::RegToT& bbResult)
    {
        ClockGateSet intersection;

        size_t numBackpropSources = op._src.size();

        if (Opcode::ConditionalIgnore == op._opcode)
        {
            ClockGateSet propgateRegisters;
            ClockGateSet explicitRegisters;

            assert(1 == op._dst.size());
            assert(op._src.size() >= 1);

            // Get any predicates back propagated to dst[0]
            const DestinationOperand& dstOp = op._dst[0];

            if ((DestinationOperandType::Register == dstOp.Type()) &&
                (RegisterType::Local == program._registerTable[dstOp.GetAccessedRegister()._registerIndex]._type))
            {
                assert(propgateRegisters.empty());
                propgateRegisters =
                    LookupWithDefault(bbResult, dstOp.GetAccessedRegister()._registerIndex, ClockGateSet());
            }

            // Add in new predicates specified by the other source operands
            for (size_t i = 1; i < op._src.size(); i++)
            {
                const SourceOperand& srcOp = op._src[i];

                if ((SourceOperandType::Register == srcOp.Type()) &&
                    (RegisterType::Local == program._registerTable[srcOp.GetAccessedRegister()._registerIndex]._type))
                {
                    explicitRegisters.insert(srcOp.GetAccessedRegister()._registerIndex);
                }
            }

            if (propgateRegisters == explicitRegisters)
            {
                // The explicit registers add no information
                RedundantOperations.insert(&op);
            }
            else
            {
                RedundantOperations.erase(&op);
            }

            assert(intersection.empty());
            intersection = explicitRegisters;
            Union(intersection, propgateRegisters);

            // Don't backpropagate to through the predicate register itself
            // to avoid a recording that a predicate can be predicated by itself
            numBackpropSources = 1;
        }
        else
        {
            bool isFirst = true;

            for (const DestinationOperand& dstOp : op._dst)
            {
                if ((DestinationOperandType::Register == dstOp.Type()) &&
                    (RegisterType::Local == program._registerTable[dstOp.GetAccessedRegister()._registerIndex]._type))
                {
                    const ClockGateSet fromSuccessors =
                        LookupWithDefault(bbResult, dstOp.GetAccessedRegister()._registerIndex, ClockGateSet());

                    if (isFirst)
                    {
                        intersection = fromSuccessors;
                        isFirst = false;
                    }
                    else
                    {
                        intersection = Intersection(intersection, fromSuccessors);
                    }
                }
            }

            // Backpropagate to all sources
            assert(numBackpropSources == op._src.size());
        }

        // Propgate to sources
        // This must occur even if intersection is empty
        // to signify that there is a use with no predicates
        for (size_t srcOpIndex = 0; srcOpIndex < numBackpropSources; srcOpIndex++)
        {
            const SourceOperand& srcOp = op._src[srcOpIndex];

            if ((SourceOperandType::Register == srcOp.Type()) &&
                (RegisterType::Local == program._registerTable[srcOp.GetAccessedRegister()._registerIndex]._type))
            {
                IntersectBasicBlockClockGateMap(bbResult, srcOp.GetAccessedRegister()._registerIndex, intersection);
            }
        }
    };

    dfa_t dfa(program, function, cfg, dfa_t::Direction::Backward, mode, TranslateClockGateSet, Intersection<size_t>,
              operationCallback);

    return DfaToResult(dfa, function);
}

// Determines which local registers can be clock gates for other local registers
// If mode == OperationEnumerationMode::Unscheduled, then this can transform the IR (ConditionalIgnore->Mov)
// This function returns true if such an IR modification was made
// clockGateMapOut is optional
bool ComputeClockGating(Program& program, Function& function, const OperationEnumerationMode mode,
                        FunctionClockGateMap* clockGateMapOut)
{
    FunctionClockGateMap functionResult;

    // Ensure result has 1 entry per basic block
    for (BasicBlock& bb : function._basicBlocks)
    {
        auto& value = functionResult[&bb];
    }

    // Early out if no clock gating is enabled
    if (!GetCodeGenConfig().ControlClockGatingEnabled())
    {
        if (clockGateMapOut)
        {
            *clockGateMapOut = functionResult;
        }

        return false;
    }

    ControlFlowGraph cfg(function, program, mode, ControlFlowGraphPhase::PrePipeline);

    // Propagate ConditionalIgnore information forward
    RedundantOperationSet forwardRedundantOperations;
    FunctionClockGateMap forwardMap =
        ComputeClockGateMapForward(program, function, cfg, mode, forwardRedundantOperations);

    // Propagate ConditionalIgnore information backward
    RedundantOperationSet backwardRedundantOperations;
    FunctionClockGateMap backwardMap =
        ComputeClockGateMapBackward(program, function, cfg, mode, backwardRedundantOperations);

    // Combine the two sets of information (union)
    for (BasicBlock& bb : function._basicBlocks)
    {
        BasicBlockClockGateMap& result = functionResult[&bb];
        assert(result.empty());

        result = forwardMap[&bb];

        for (const auto& p : backwardMap[&bb])
        {
            const size_t gatedRegister = p.first;

            const ClockGateSet& backwardGateRegisters = p.second;

            ClockGateSet& forwardGateRegisters = result[gatedRegister];

            Union(forwardGateRegisters, backwardGateRegisters);
        }

        // Ensure that predicate registers themselves cannot be predicated
        ClockGateSet predicateRegisters;

        for (const auto& p : result)
        {
            for (const size_t predicateRegister : p.second)
            {
                predicateRegisters.insert(predicateRegister);
            }
        }

        for (const size_t predicateRegister : predicateRegisters)
        {
            result.erase(predicateRegister);
        }
    }

    // If both passes determined that a ConditionalIgnore operation is redundant
    // then change that operation into a mov
    const RedundantOperationSet operationsToRemove =
        Intersection(forwardRedundantOperations, backwardRedundantOperations);

    for (const Operation* const constOp : operationsToRemove)
    {
        // Operations during dataflow analysis are constant because
        // they cannot be changed until the algorithm has converged
        Operation* const op = const_cast<Operation*>(constOp);

        assert(Opcode::ConditionalIgnore == op->_opcode);
        assert(op->_src.size() >= 1);

        op->_opcode = Opcode::Mov;
        op->_src.resize(1);
    }

    if (clockGateMapOut)
    {
        *clockGateMapOut = functionResult;
    }

    // return true if the IR was changed
    return !operationsToRemove.empty();
}
