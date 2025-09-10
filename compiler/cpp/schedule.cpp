// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

enum class AtomicBlockSchedulingPass
{
    // Try register ratios up to the max specified by configuration
    Default = 0,
    // Use the maximum register ratio but allow overpacking the last pipeline stage
    IgnorePathLengthExceeded = 1,
    // Schedule as early as possible, ignoring register ratio
    IgnoreRegisterRatio = 2,
    RemovePredication = 3,
    ReadLatencyOne = 4,
    Count
};

class ConstraintScheduler
{
  public:
    const static bool _enableLogging = false;

    // Allowable range of pipeline stages for a node
    // (i, i+1) means that a node can only be scheduled in stage i
    struct Range
    {
        size_t _begin;
        size_t _end;

        Range() : _begin(std::numeric_limits<size_t>::max()), _end(0) {}

        Range(const size_t begin, const size_t end) : _begin(begin), _end(end) { assert(begin < end); }

        static Range Infinite()
        {
            const Range result = {0, std::numeric_limits<size_t>::max()};

            return result;
        }

        void Intersect(const Range& rhs)
        {
            _begin = std::max(_begin, rhs._begin);
            _end = std::min(_end, rhs._end);
        }
    };

    // A node is realy just an index into a vector
    typedef size_t node_t;

    // Function that maps from a specific pipeline stage that an operation is scheduled on to
    // to a range of allowable pipeline stages
    typedef std::function<void(const size_t srcPipelineStage, Range& range)> constraint_function_t;

    /// Callback function that is called when a scheduling decision is made
    typedef std::function<void(const node_t node, const size_t pipelineStage)> schedule_function_t;

    // Maps node_t to desired delay (measured in pipeline stages) before a given node is scheduled
    typedef std::map<node_t, size_t> delay_map_t;

    struct Constraint
    {
        node_t _targetNode;
        constraint_function_t _constraintFunction;
        const char* _description;
    };

    struct Node
    {
        Node()
            : _pendingConstraints(0), _totalConstraints(0), _minPipelineStage(0), _allowedRange(Range::Infinite()),
              _operation(nullptr)
        {
        }

        void Reset()
        {
            _pendingConstraints = _totalConstraints;

            _allowedRange = Range::Infinite();

            _allowedRange._begin = _minPipelineStage;
        }

        // Number of constraints that have not been satisified
        // that target this node
        size_t _pendingConstraints;

        // Total number of constraints that target this node
        size_t _totalConstraints;

        // The first pipeline stage this node can be scheduled in
        size_t _minPipelineStage;

        // Constraints from this node to other nodes
        std::vector<Constraint> _constraints;

        // The range of pipeline stages this node can be scheduled in
        // Each constraint can potentially restrict this range
        Range _allowedRange;

        // The operation that makes up this node
        const Operation* _operation;
    };

    ConstraintScheduler(const Location& location) : _location(location) {}

    node_t AddNode(const Operation* const operation, const size_t minPipelineStage)
    {
        const size_t result = _nodes.size();

        Node n = {};

        n._operation = operation;
        n._minPipelineStage = minPipelineStage;

        if (operation->_opcode == Opcode::BeginAtomic)
        {
            _atomicBlockPassIndex[operation] = 0;
            _atomicBlockRegisterRatio[operation] = GetCodeGenConfig()._logicRegisterRatio;
        }

        _nodes.push_back(n);

        return result;
    }

    void AddConstraint(const node_t src, const node_t dst, const constraint_function_t& constraintFunction,
                       const char* description)
    {
        assert(src < _nodes.size());
        assert(dst < _nodes.size());
        assert(src != dst);

        if (_enableLogging)
        {
            std::cout << "AddConstraint(" << src << ", " << dst << ", " << description << ")\n";
        }

        Node& s = _nodes[src];
        Node& d = _nodes[dst];

        // Track that there is another constraint targeting the destination
        d._totalConstraints++;

        // Add the constraint to the source
        Constraint constraint = {};

        constraint._targetNode = dst;
        constraint._constraintFunction = constraintFunction;
        constraint._description = description;

        s._constraints.push_back(constraint);
    }

    // Note that Schedule() can be called many times, even though constraints are added only once
    bool Schedule(const schedule_function_t& scheduleCallback, const delay_map_t& delayMap,
                  const size_t adjustedThreadRate, const OperationListDependencyGraph& dependencyGraph)
    {
        if (_enableLogging)
        {
            std::cout << "Sched(" << _nodes.size() << ")\n";
        }

        // Reset all nodes - to prepare for a new schedule
        for (Node& node : _nodes)
        {
            node.Reset();
        }

        // Sort nodes into those that have constraints and those that do not
        std::list<node_t> readyNodes;

        // Find all nodes that start with no constraints
        for (node_t i = 0; i < _nodes.size(); i++)
        {
            const Node& n = _nodes[i];

            if (0 == n._pendingConstraints)
            {
                readyNodes.push_back(i);
            }
        }

        // Create a copy of atomic block scheduling settings to ensure that
        // current scheduling attempt uses these values even if maps are
        // updated during scheduling
        auto prevAtomicBlockPassIndex = _atomicBlockPassIndex;
        auto prevAtomicBlockRegisterRatio = _atomicBlockRegisterRatio;

        size_t numScheduledNodes = 0;

        // Iterate until all nodes have been scheduled
        while (!readyNodes.empty())
        {
            std::list<node_t> newReadyNodes;

            // Schedule all nodes that have no remaining constraints targeting them
            for (const node_t nodeIndex : readyNodes)
            {
                assert(nodeIndex < _nodes.size());
                const Node& node = _nodes[nodeIndex];

                assert(0 == node._pendingConstraints);

                const Range& allowedRange = node._allowedRange;
                assert(allowedRange._begin < allowedRange._end);

                // Determine which pipeline stage this node will run in
                // AllowedRange bounds the possible range
                // delayMap indicates when some nodes should not be scheduled as soon as possible
                const size_t desiredDelay = SafeLookup(delayMap, nodeIndex);

                size_t selectedPipelineStage = std::min(allowedRange._begin + desiredDelay, allowedRange._end - 1);

                assert(selectedPipelineStage >= allowedRange._begin);
                assert(selectedPipelineStage < allowedRange._end);

                if (_enableLogging)
                {
                    std::cout << "scheduling node: " << nodeIndex << " stage: " << selectedPipelineStage
                              << " delay: " << desiredDelay << "\n";
                }

                // Now that the decision has been made, call the callback to notify the client
                scheduleCallback(nodeIndex, selectedPipelineStage);

                // Track atomic blocks with failing constraints
                std::set<size_t> failingAtomicBlockIndices;

                // Process all constraints that have this node as a source
                for (const Constraint& constraint : node._constraints)
                {
                    assert(constraint._targetNode < _nodes.size());
                    Node& targetNode = _nodes[constraint._targetNode];

                    // Call a function to further constrain the allowed range of the target node
                    constraint._constraintFunction(selectedPipelineStage, targetNode._allowedRange);

                    if (_enableLogging)
                    {
                        std::cout << "constraint target: " << constraint._targetNode << " new range: ["
                                  << targetNode._allowedRange._begin << ", " << targetNode._allowedRange._end << "] "
                                  << constraint._description << "\n";
                    }

                    // The set of constraints cannot be satisfied
                    if (targetNode._allowedRange._begin >= targetNode._allowedRange._end)
                    {
                        // Look for atomic block that contains this operation
                        const auto operationToAtomicBlockIndexIt =
                            dependencyGraph.operationToAtomicBlockIndex.find(targetNode._operation);

                        if (operationToAtomicBlockIndexIt == dependencyGraph.operationToAtomicBlockIndex.end())
                        {
                            // If no atomic block contains this operation then
                            // this operation must be a BeginAtomic op.
                            assert(targetNode._operation->_opcode == Opcode::BeginAtomic);

                            const Operation* const beginAtomicOpPointer = targetNode._operation;

                            const size_t atomicBlockIndex =
                                SafeLookup(dependencyGraph.beginAtomicOpToAtomicBlockIndex, beginAtomicOpPointer);

                            failingAtomicBlockIndices.insert(atomicBlockIndex);
                        }
                        else
                        {
                            // Save all atomic blocks that contain this target node
                            for (const size_t atomicBlockIndex : operationToAtomicBlockIndexIt->second)
                            {
                                failingAtomicBlockIndices.insert(atomicBlockIndex);
                            }
                        }
                    }
                    else
                    {
                        assert(targetNode._pendingConstraints > 0);
                        targetNode._pendingConstraints--;

                        if (0 == targetNode._pendingConstraints)
                        {
                            newReadyNodes.push_back(constraint._targetNode);
                        }
                    }
                }

                if (!failingAtomicBlockIndices.empty())
                {
                    bool didChangeSchedule = false;

                    // Iterate backwards through atomic block indices as a heuristic to start with inner atomics
                    // before outer atomics (inner atomics are numbered higher than their corresponding outer atomics)
                    std::list<const Operation*> beginAtomicOpPointers;
                    for (auto it = failingAtomicBlockIndices.rbegin(); it != failingAtomicBlockIndices.rend(); it++)
                    {
                        const size_t atomicBlockIndex = *it;
                        const Operation* const beginAtomicOpPointer =
                            SafeLookup(dependencyGraph.atomicBlockIndexToBeginAtomicOp, atomicBlockIndex);
                        beginAtomicOpPointers.push_back(beginAtomicOpPointer);

                        const size_t previousPass = SafeLookup(prevAtomicBlockPassIndex, beginAtomicOpPointer);
                        const size_t previousRegisterRatio =
                            SafeLookup(prevAtomicBlockRegisterRatio, beginAtomicOpPointer);

                        // Increment register ratio if not at max
                        if (previousRegisterRatio < GetCodeGenConfig()._maxLogicRegisterRatio)
                        {
                            _atomicBlockRegisterRatio[beginAtomicOpPointer] = previousRegisterRatio + 1;

                            didChangeSchedule = true;
                            break;
                        }
                        // Increment first AtomicBlockSchedulingPass that is not at the max
                        else if (previousPass < static_cast<size_t>(AtomicBlockSchedulingPass::Count))
                        {
                            _atomicBlockPassIndex[beginAtomicOpPointer] = previousPass + 1;

                            didChangeSchedule = true;
                            break;
                        }
                    }

                    // All strategies have been tried
                    if (!didChangeSchedule)
                    {
                        for (const Operation* const beginAtomicOpPointer : beginAtomicOpPointers)
                        {
                            g_compiler->ErrorStream(beginAtomicOpPointer->_flags._atomicBlockDesc._location,
                                                    CompileError::BasicBlockNotSchedulable)
                                << "[[schedule()]] constraints are impossible to satisfy";
                        }
                        throw std::runtime_error("Cannot schedule basic block");
                    }

                    return false;
                }

                numScheduledNodes++;
            }

            readyNodes.swap(newReadyNodes);
        }

        // All nodes should have been scheduled by this point
        assert(readyNodes.empty());
        assert(numScheduledNodes == _nodes.size());

        return true;
    }

    size_t GetAtomicBlockPassIndex(const Operation* op) { return SafeLookup(_atomicBlockPassIndex, op); }

    size_t GetAtomicBlockRegisterRatio(const Operation* op) { return SafeLookup(_atomicBlockRegisterRatio, op); }

  private:
    std::vector<Node> _nodes;

    std::map<const Operation*, size_t> _atomicBlockPassIndex;

    std::map<const Operation*, size_t> _atomicBlockRegisterRatio;

    Location _location;
};

std::ostream& operator<<(std::ostream& str, const ConstraintScheduler::Range range)
{
    str << "[" << range._begin << "," << range._end << "]";
    return str;
}

void EmitPipelineStage(BasicBlock& basicBlock, const std::list<const Operation*>& operationsIn)
{
    const size_t sequenceNumber = basicBlock.GetNextAtomicSequenceNumber();

    // Make a local copy (that can be modified)
    std::list<const Operation*> operations = operationsIn;

    // Get the set of line numbers associated with this pipeline stage
    std::set<FileAndLineNumber> fileAndLineNumbers;

    bool globalsAfterStartCondition = false;

    size_t numWriteGlobals = 0;
    size_t numPrintAssertAndStringOps = 0;

    Stage printStage = {};
    printStage._atomicSequence = sequenceNumber;

    OperationList assertions;
    OperationList stringFormats;
    OperationList stringReferences;
    OperationList assertStringEquals;

    for (const Operation* const op : operations)
    {
        if (Opcode::LineNumber == op->_opcode)
        {
            for (const FileAndLineNumber& loc : op->_locations)
            {
                fileAndLineNumbers.insert(loc);
            }
        }
        else if (Opcode::StartCondition == op->_opcode && !op->_flags._startCondition._globalsBefore)
        {
            assert(sequenceNumber == 0);
            globalsAfterStartCondition = true;
        }
        else if (Opcode::WriteGlobal == op->_opcode)
        {
            numWriteGlobals++;
        }
        else if (Opcode::Print == op->_opcode)
        {
            numPrintAssertAndStringOps++;

            // Prints are delayed so that all prints appear in the same always
            // block and are thus not reordered by an RTL simulator.
            printStage._operations.push_back(*op);
        }
        else if (Opcode::Assert == op->_opcode)
        {
            numPrintAssertAndStringOps++;

            // Asserts are also delayed and are placed at the end of the print
            // stage to ensure that prints run before assertions fire (and stop
            // simulation)
            assertions.push_back(*op);
        }
        else if (Opcode::FormatString == op->_opcode)
        {
            numPrintAssertAndStringOps++;

            stringFormats.push_back(*op);
        }
        else if (Opcode::FormatEnum == op->_opcode)
        {
            numPrintAssertAndStringOps++;

            stringFormats.push_back(*op);
        }
        else if (Opcode::ReferenceString == op->_opcode)
        {
            numPrintAssertAndStringOps++;

            stringReferences.push_back(*op);
        }
        else if (Opcode::AssertStringEqual == op->_opcode)
        {
            numPrintAssertAndStringOps++;

            assertStringEquals.push_back(*op);
        }
    }

    if (fileAndLineNumbers.empty())
    {
        // A pipeline stage being scheduled here may not contain
        // a line number where it matters (i.e. if the operation sequence is within
        // an atomic_mem). Therefore, capture the line number from the previous stage
        if (!basicBlock._stages.empty())
        {
            fileAndLineNumbers = basicBlock._stages.back()._fileAndLineNumbers;
        }
    }

    printStage._fileAndLineNumbers = fileAndLineNumbers;

    // Some operations are filtered out now. They are only used during scheduling, and back ends do not need to deal
    // with them
    const auto shouldFilter = [](const Operation* const op)
    {
        bool result = false;

        switch (op->_opcode)
        {
        case Opcode::BeginAtomic:
        case Opcode::EndAtomic:
        case Opcode::Stage:
        case Opcode::LineNumber:
            result = true;
            break;

        default:
            break;
        }

        return result;
    };

    operations.remove_if(shouldFilter);

    const size_t stageCountBefore = basicBlock._stages.size();

    bool globalsAfterStartConditionNext = globalsAfterStartCondition;

    bool printStageBeforeStartCondition = false;
    std::list<Stage>::iterator startConditionStageIt;

    while (!operations.empty())
    {
        Stage stage = {};
        stage._atomicSequence = sequenceNumber;
        stage._fileAndLineNumbers = fileAndLineNumbers;

        // The set of registers accessed in the current stage
        std::set<size_t> registersWritten;

        bool startConditionStage = false;

        // Emit all operations which do not have hazards with operations in the current stage structure
        for (auto it = operations.begin(); it != operations.end(); /*nothing*/)
        {
            auto nextIt = it;
            ++nextIt;

            const Operation* const op = *it;

            // Get the set of registers accessed by this operation
            std::vector<size_t> readSet;
            std::vector<size_t> writeSet;

            GetAccessedRegisters(*op, readSet, writeSet);

            // Check for RAW hazards
            // Renaming ensures that other hazards are not possible for local registers
            // Explicit constraints added to the constraint schedule ensure that other hazards are not possible for
            // global registers
            bool emitNow = true;

            for (const size_t registerIndex : readSet)
            {
                if (registersWritten.end() != registersWritten.find(registerIndex))
                {
                    emitNow = false;
                }
            }

            for (const size_t registerIndex : writeSet)
            {
                registersWritten.insert(registerIndex);
            }

            if (op->_opcode == Opcode::StartCondition)
            {
                if (op->_flags._startCondition._globalsBefore)
                {
                    // If there are globals, prints, asserts, or string operations still to schedule, do not emit start
                    // condition yet
                    if (numWriteGlobals > 0 || numPrintAssertAndStringOps > 0)
                    {
                        emitNow = false;
                    }
                }

                // If StartCondition seen, disable globals-after-start-condition constraint
                // for all following stages
                if (emitNow)
                {
                    globalsAfterStartConditionNext = false;

                    startConditionStage = true;
                    printStageBeforeStartCondition = op->_flags._startCondition._globalsBefore;
                }
            }
            else if (op->_opcode == Opcode::WriteGlobal)
            {
                // Mask emitNow with globals-after-start-condition constraint
                emitNow = emitNow && !globalsAfterStartCondition;

                if (emitNow)
                {
                    assert(numWriteGlobals > 0);
                    numWriteGlobals--;
                }
            }
            else if (op->_opcode == Opcode::Print || op->_opcode == Opcode::Assert ||
                     op->_opcode == Opcode::FormatString || op->_opcode == Opcode::FormatEnum ||
                     op->_opcode == Opcode::ReferenceString || op->_opcode == Opcode::AssertStringEqual)
            {
                // Even though this will not be emitted now, it is important to
                // keep it in the operation list until all dependencies are met
                // so that start condition is emitted at the right time in the
                // case where these ops should be inserted before start condition.
                if (emitNow)
                {
                    // Operation has already been saved to print stage
                    operations.erase(it);

                    assert(numPrintAssertAndStringOps > 0);
                    numPrintAssertAndStringOps--;

                    emitNow = false;
                }
            }

            if (emitNow)
            {
                stage._operations.push_back(*op);
                operations.erase(it);
            }

            it = nextIt;
        }

        if (!stage._operations.empty())
        {
            basicBlock._stages.push_back(stage);

            if (startConditionStage)
            {
                // Save iterator to stage with start condition
                startConditionStageIt = basicBlock._stages.end();
                startConditionStageIt--;
            }
        }

        globalsAfterStartCondition = globalsAfterStartConditionNext;
    }

    // Add final stage with prints, string table operations, and assertions now

    // FormatString operations come first, because they add strings to the string table
    // which may be read out by print operations
    printStage._operations.splice(printStage._operations.begin(), stringFormats);

    // AssertStringEqual come after prints
    // because AssertStringEqual operations can stop simulation
    printStage._operations.splice(printStage._operations.end(), assertStringEquals);

    // String reference operations come after prints and AssertStringEqual
    // because they may cause strings to be removed from the string table
    printStage._operations.splice(printStage._operations.end(), stringReferences);

    // assertions come last to allow prints to execute before stopping simulation
    printStage._operations.splice(printStage._operations.end(), assertions);

    if (!printStage._operations.empty())
    {
        if (printStageBeforeStartCondition)
        {
            // Insert print stage before start condition stage
            basicBlock._stages.insert(startConditionStageIt, printStage);
        }
        else
        {
            // Insert print stage at the end of thee pipeline stage
            basicBlock._stages.push_back(printStage);
        }
    }

    const size_t stageCountAfter = basicBlock._stages.size();

    if (stageCountBefore == stageCountAfter)
    {
        // No stages were added, add one to ensure barriers and stages() operations are honored
        Stage stage = {};
        stage._atomicSequence = sequenceNumber;
        stage._fileAndLineNumbers = fileAndLineNumbers;

        basicBlock._stages.push_back(stage);
    }
}

// Returns true if the src operation is a WriteGlobal
// and the destination operation reads from or writes to the same global
bool IsGlobalRawWawHazard(const Program& program, const Operation& srcOp, const Operation& dstOp)
{
    if (srcOp._opcode == Opcode::WriteGlobal)
    {
        assert(1 == srcOp._dst.size());
        const size_t globalIndex = srcOp._dst[0].GetAccessedRegister()._registerIndex;
        assert(program._registerTable[globalIndex]._type == RegisterType::Global);

        std::vector<size_t> readSet;
        std::vector<size_t> writeSet;

        GetAccessedRegisters(dstOp, readSet, writeSet);

        for (const size_t r : readSet)
        {
            if (r == globalIndex)
            {
                return true;
            }
        }

        for (const size_t w : writeSet)
        {
            if (w == globalIndex)
            {
                return true;
            }
        }
    }

    return false;
}

bool IsAsyncCall(const Operation& op) { return (Opcode::Enqueue == op._opcode) && op._flags._enqueue._isAsyncCall; }

void RemoveUnnecessaryLineNumbers(OperationList& operations)
{
    // Detects sequences of Opcode::LineNumber and removes all but the last
    // This ensures that unnecessary pipeline stages are not added for line numbers with no associated code
    // In the optimized case, remove all line nums.
    auto it = operations.begin();
    auto nextIt = it;

    for (/*nothing*/; nextIt != operations.end(); it = nextIt)
    {
        // Get iterator for next loop iteration
        nextIt = it;
        nextIt++;

        const Operation& firstOp = *it;

        if (Opcode::LineNumber == firstOp._opcode)
        {
            if (GetCodeGenConfig()._optimize == 0)
            {
                if (nextIt != operations.end())
                {
                    const Operation& nextOp = *nextIt;

                    if (Opcode::LineNumber == nextOp._opcode)
                    {
                        operations.erase(it);
                    }
                }
            }
            else
            {
                operations.erase(it);
            }
        }
    }
}

void PropagateAndRemovePredicateHintOperations(OperationList& operations)
{
    bool isPredicated = false;

    for (auto it = operations.begin(); it != operations.end(); ++it)
    {
        Operation& op = *it;
        if (op._opcode == Opcode::PushPredicate)
        {
            isPredicated = true;
        }
        else if (op._opcode == Opcode::PopPredicate)
        {
            isPredicated = false;
        }
        else
        {
            op._isPredicated = isPredicated;
        }
    }

    const auto shouldRemove = [](const Operation& op)
    {
        bool result = false;

        switch (op._opcode)
        {
        case Opcode::PushPredicate:
        case Opcode::PopPredicate:
            result = true;
            break;

        default:
            break;
        }

        return result;
    };

    operations.remove_if(shouldRemove);
}

typedef std::map<size_t, std::list<const Operation*>> schedule_t;

size_t GetSchedulePipelineStageCount(const schedule_t& schedule)
{
    size_t result = 0;

    for (const auto& p : schedule)
    {
        result = std::max(result, p.first + 1);
    }

    return result;
}

size_t ScoreSchedule(const Program& program, const schedule_t& schedule)
{
    size_t result = 0;

    // Maps local register index to the last pipeline stage where it was read
    std::map<size_t, size_t> lastReadStage;

    // Maps local register index to the only pipeline stage where it was written
    std::map<size_t, size_t> writeStage;

    const size_t pipelineStageCount = GetSchedulePipelineStageCount(schedule);

    // Iterate though pipeline stages
    for (size_t stageIndex = 0; stageIndex < pipelineStageCount; stageIndex++)
    {
        const auto it = schedule.find(stageIndex);
        if (it == schedule.end())
        {
            continue;
        }

        const std::list<const Operation*>& operations = it->second;

        // Update lastReadStage & writeStage
        for (const Operation* const op : operations)
        {
            for (const SourceOperand& srcOp : op->_src)
            {
                if (SourceOperandType::Register == srcOp.Type())
                {
                    const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                    if (IsLocalRegister(program, registerIndex))
                    {
                        lastReadStage[registerIndex] = stageIndex;
                    }
                }
            }

            for (const DestinationOperand& destOp : op->_dst)
            {
                const size_t registerIndex = destOp.GetAccessedRegister()._registerIndex;

                if (IsLocalRegister(program, registerIndex))
                {
                    SafeInsert(writeStage, registerIndex, stageIndex);
                }
            }
        }
    }

    const auto CostFunction =
        [&program](const size_t registerIndex, const size_t readPipelineStage, const size_t writePipelineStage)
    {
        assert(readPipelineStage >= writePipelineStage);
        const size_t distance = readPipelineStage - writePipelineStage;

        const size_t width = program._registerTable[registerIndex]._width;

        // If the producer and consumer are in the same pipeline stage (distance == 0) - then no pipeline registers are
        // used to hold the intermediate data
        return (distance * width);
    };

    // For all registers that are read
    for (const auto& p : lastReadStage)
    {
        const size_t registerIndex = p.first;

        const size_t readPipelineStage = p.second;

        const auto it = writeStage.find(registerIndex);

        // If a register has no writes, assume it is written in the first stage (will come from input fifo)
        const size_t writePipelineStage = (it == writeStage.end()) ? 0 : it->second;

        result += CostFunction(registerIndex, readPipelineStage, writePipelineStage);
    }

    // Handle registers that are written by never read, assume they are read in the last stage (live out)
    for (const auto& p : writeStage)
    {
        const size_t registerIndex = p.first;

        const size_t writePipelineStage = p.second;

        if (lastReadStage.end() == lastReadStage.find(registerIndex))
        {
            const size_t readPipelineStage = pipelineStageCount;

            result += CostFunction(registerIndex, readPipelineStage, writePipelineStage);
        }
    }

    return result;
}

// Aggregate all of a stage's line numbers in a set.
// This is done once initially when the stage is created,
// and again after all optimizations have been performed.
// Optimizations will move operations around, making the first pass stale.
void CollectPerStageLineNumberInfo(const Program& program, BasicBlock& basicBlock)
{
    for (auto& stage : basicBlock._stages)
    {
        // Get the set of line numbers associated with this pipeline stage
        std::set<FileAndLineNumber> fileAndLineNumbers;

        for (const Operation op : stage._operations)
        {
            for (const FileAndLineNumber& loc : op._locations)
            {
                fileAndLineNumbers.insert(loc);
            }
        }

        if (!fileAndLineNumbers.empty())
        {
            stage._fileAndLineNumbers = fileAndLineNumbers;
        }
    }
}

// Schedules the operations in basicBlock._operations into specific pipeline stages
void ScheduleBasicBlock(const Program& program, BasicBlock& basicBlock)
{
    PropagateLineNumbers(&basicBlock);

    RemoveUnnecessaryLineNumbers(basicBlock._operations);

    {
        PropagateAndRemovePredicateHintOperations(basicBlock._startConditionOperations);
        PropagateAndRemovePredicateHintOperations(basicBlock._operations);

        for (auto& stage : basicBlock._stages)
        {
            PropagateAndRemovePredicateHintOperations(stage._operations);
        }
    }

    // The only stages in the basic block at this point are Enqueue stages added by AppendStage
    // Those must appear at the end of the basic block
    std::list<Stage> appendedStages = basicBlock._stages;

    basicBlock._stages.clear();

    assert(0 == basicBlock.GetNextAtomicSequenceNumber());

    if (basicBlock.HasStartCondition())
    {
        // Emit 1 pipeline stage corresponding to all start condition operations
        std::list<const Operation*> operationPointers;

        for (const Operation& op : basicBlock._startConditionOperations)
        {
            operationPointers.push_back(&op);
        }

        EmitPipelineStage(basicBlock, operationPointers);
    }

    const size_t carryChainWidthPerLogicLevel = GetCodeGenConfig()._carryChainWidthPerLogicLevel;

    // In order to speed up the scheduling process, a new set of dense register IDs are allocated
    // Path length tracking runs with these dense register ids
    std::map<size_t, size_t> originalToDenseRegisterMap;

    size_t denseRegisterCount = 0;

    struct OperationMetaData
    {
        const Operation* _op;
        size_t _pathLength;
        std::vector<size_t> _denseSrcReg;
        std::vector<size_t> _denseDstReg;
    };

    std::list<OperationMetaData> operationMetaDataList;

    const auto updateDenseRegisterMap = [&](const size_t originalRegisterIndex, std::vector<size_t>& denseVectorOut)
    {
        const auto it = originalToDenseRegisterMap.find(originalRegisterIndex);

        const size_t denseIndex = (originalToDenseRegisterMap.end() == it) ? denseRegisterCount : it->second;

        if (originalToDenseRegisterMap.end() == it)
        {
            SafeInsert(originalToDenseRegisterMap, originalRegisterIndex, denseIndex);

            denseRegisterCount++;
        }

        denseVectorOut.push_back(denseIndex);
    };

    // Maps scheduled operation to pipeline stage number
    std::map<const OperationMetaData*, size_t> opToStage;

    for (const Operation& operation : basicBlock._operations)
    {
        OperationMetaData operationMetaData = {};

        operationMetaData._op = &operation;
        operationMetaData._pathLength = GetOpPathLength(program, operation, carryChainWidthPerLogicLevel);

        for (const SourceOperand& sourceOperand : operation._src)
        {
            if (sourceOperand.Type() == SourceOperandType::Register)
            {
                updateDenseRegisterMap(sourceOperand.GetAccessedRegister()._registerIndex,
                                       operationMetaData._denseSrcReg);
            }
        }

        for (const DestinationOperand& destOperand : operation._dst)
        {
            updateDenseRegisterMap(destOperand.GetAccessedRegister()._registerIndex, operationMetaData._denseDstReg);
        }

        operationMetaDataList.push_back(operationMetaData);
    }

    assert(operationMetaDataList.size() == basicBlock._operations.size());

    // Maps pipeline stage to RegPathLengthMap that defines the path length
    // from the start of the pipeline stage to each register written by the pipeline stage
    std::map<size_t, RegPathLengthMap> allRegPathLengthMaps;

    const auto getRegPathLengthMapForStage =
        [&allRegPathLengthMaps](const size_t stage, const size_t denseRegisterCount) -> RegPathLengthMap&
    {
        RegPathLengthMap& result = allRegPathLengthMaps[stage];
        if (0 == result.size())
        {
            InitializeRegPathLengthMap(result, denseRegisterCount);
        }

        return result;
    };

    // Maps between scheduler nodes to operation pointers
    std::map<const Operation*, ConstraintScheduler::node_t> operationToNode;
    std::map<ConstraintScheduler::node_t, const OperationMetaData*> nodeToOperation;

    const size_t logicLevelsPerPipelineStage = GetCodeGenConfig()._logicRegisterRatio;

    const OperationListDependencyGraph dependencyGraph =
        ComputeOperationListDependencyGraph(basicBlock._operations, program);

    ConstraintScheduler scheduler(basicBlock._location);

    // Add nodes to the scheduler
    std::map<const Operation*, size_t> operationSequenceNumberMap;
    size_t operationCount = 0;

    for (const OperationMetaData& op : operationMetaDataList)
    {
        // Note that {Begin,End}Atomic are added to the scheduler
        {
            size_t minPipelineStage = 0;

            // For fixed-latency exports, stage 0 inputs are not registered (they come directly from input ports)
            // so it is impossible to get a net representing values on stage -1
            // Therefore, if an operation requires registered input, don't let it run in stage 1
            if (basicBlock._function->_functionNode->IsFixedLatency() && OperationHasRegisteredInput(*op._op))
            {
                minPipelineStage = 1;
            }

            const ConstraintScheduler::node_t node = scheduler.AddNode(op._op, minPipelineStage);

            if (ConstraintScheduler::_enableLogging)
            {
                std::cout << "node: " << node << " ";
                SerializeOperation(std::cout, *op._op, program);
                std::cout << "\n";
            }

            // update mappings between operations and nodes
            SafeInsert(operationToNode, op._op, node);
            SafeInsert(nodeToOperation, node, &op);

            // Give each operation a unique ID depending on where it appears in the input operation list
            operationSequenceNumberMap[op._op] = operationCount;
            operationCount++;
        }
    }

    // Add constraints to ensure a correct schedule

    // Data dependencies & type barriers
    for (const auto& p : dependencyGraph.forwardOperationDependencies)
    {
        const Operation* const srcOp = p.first;

        const ConstraintScheduler::node_t srcNode = SafeLookup(operationToNode, srcOp);

        const OperationMetaData* const srcOperationMetadata = SafeLookup(nodeToOperation, srcNode);

        for (const Operation* const dstOp : p.second)
        {
            const ConstraintScheduler::node_t dstNode = SafeLookup(operationToNode, dstOp);

            const OperationMetaData* const dstOperationMetadata = SafeLookup(nodeToOperation, dstNode);

            const size_t inputRoutingDelay = GetInputRoutingDelay(*dstOp);

            bool isLoadMemoryPredicateConstraint = false;
            bool isExternCallPredicateConstraint = false;

            // Determine if this dependency is for the load memory predicate
            if ((dstOp->_opcode == Opcode::LoadMemory) &&
                dstOp->_flags._loadMemory._isPredicated)
            {
                assert(3 == dstOp->_src.size());

                for (const DestinationOperand& dstOperand : srcOp->_dst)
                {
                    if ((DestinationOperandType::Register == dstOperand.Type()) &&
                        (SourceOperandType::Register == dstOp->_src[2].Type()) &&
                        (dstOperand.GetAccessedRegister()._registerIndex ==
                         dstOp->_src[2].GetAccessedRegister()._registerIndex))
                    {
                        isLoadMemoryPredicateConstraint = true;
                    }
                }
            }

            // Determine if this dependency is for the predicate
            // in a call to a side-effect-free, fixed-latency, external function
            if ((dstOp->_opcode == Opcode::InlineExternalModule) && dstOp->_flags._callInlineExternalModule._isPure)
            {
                assert(!dstOp->_src.empty()); // predicate is the first operand

                for (const DestinationOperand& dstOperand : srcOp->_dst)
                {
                    if ((DestinationOperandType::Register == dstOperand.Type()) &&
                        (SourceOperandType::Register == dstOp->_src[0].Type()) &&
                        (dstOperand.GetAccessedRegister()._registerIndex ==
                         dstOp->_src[0].GetAccessedRegister()._registerIndex))
                    {
                        isExternCallPredicateConstraint = true;
                    }
                }
            }

            const ConstraintScheduler::constraint_function_t constraintFunction =
                [srcOperationMetadata, dstOperationMetadata, denseRegisterCount, logicLevelsPerPipelineStage, srcOp,
                 dstOp, inputRoutingDelay, isLoadMemoryPredicateConstraint, isExternCallPredicateConstraint,
                 &getRegPathLengthMapForStage, &program, &dependencyGraph,
                 &scheduler](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
            {
                ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                // Lookup the path length map for the chosen pipeline stage
                RegPathLengthMap& selectedStagePathLengthMap =
                    getRegPathLengthMapForStage(srcPipelineStage, denseRegisterCount);

                const auto srcOperationToAtomicBlockIndexIt = dependencyGraph.operationToAtomicBlockIndex.find(srcOp);

                const bool srcOperationInAtomicBlock =
                    (srcOperationToAtomicBlockIndexIt != dependencyGraph.operationToAtomicBlockIndex.end());

                size_t srcAtomicBlockIndex = 0;
                if (srcOperationInAtomicBlock)
                {
                    srcAtomicBlockIndex = srcOperationToAtomicBlockIndexIt->second.back();
                }

                if (Opcode::LoadMemory == srcOp->_opcode)
                {
                    if (srcOperationInAtomicBlock)
                    {
                        const size_t srcAtomicBlockPass = scheduler.GetAtomicBlockPassIndex(
                            SafeLookup(dependencyGraph.atomicBlockIndexToBeginAtomicOp, srcAtomicBlockIndex));

                        if (srcAtomicBlockPass >= static_cast<size_t>(AtomicBlockSchedulingPass::ReadLatencyOne))
                        {
                            // Scheduling of this atomic block failed previous
                            // Disable the memory output register
                            // To relax constraints
                            const_cast<Operation*>(srcOp)->_flags._loadMemory._readLatency = 1;
                        }
                    }
                }

                const auto dstOperationToAtomicBlockIndexIt = dependencyGraph.operationToAtomicBlockIndex.find(dstOp);

                const bool dstOperationInAtomicBlock =
                    (dstOperationToAtomicBlockIndexIt != dependencyGraph.operationToAtomicBlockIndex.end());

                size_t relaxedLogicLevels = logicLevelsPerPipelineStage;
                bool ignoreRegisterRatio = false;
                bool ignorePathLengthExceeded = false;

                if (dstOperationInAtomicBlock)
                {
                    const size_t dstAtomicBlockIndex = dstOperationToAtomicBlockIndexIt->second.back();

                    const Operation* const atomicOp =
                        SafeLookup(dependencyGraph.atomicBlockIndexToBeginAtomicOp, dstAtomicBlockIndex);

                    // Use register ratio for atomic block if src and dst are in
                    // same atomic block
                    if (srcOperationInAtomicBlock && srcAtomicBlockIndex == dstAtomicBlockIndex)
                    {
                        relaxedLogicLevels = scheduler.GetAtomicBlockRegisterRatio(atomicOp);
                        assert(relaxedLogicLevels >= logicLevelsPerPipelineStage);
                    }

                    const size_t dstAtomicBlockPass = scheduler.GetAtomicBlockPassIndex(atomicOp);

                    // Check to see if scheduling of this atomic block failed previously
                    // and the path length or register ratio should be ignored to relax constraints
                    ignorePathLengthExceeded =
                        (dstAtomicBlockPass >=
                         static_cast<size_t>(AtomicBlockSchedulingPass::IgnorePathLengthExceeded));
                    ignoreRegisterRatio =
                        (dstAtomicBlockPass >= static_cast<size_t>(AtomicBlockSchedulingPass::IgnoreRegisterRatio));

                    if (isLoadMemoryPredicateConstraint)
                    {
                        assert(Opcode::LoadMemory == dstOp->_opcode);

                        const RegisterDescription memoryDesc =
                            program._registerTable[dstOp->_src[0].GetAccessedRegister()._registerIndex];

                        if ((dstAtomicBlockPass >= static_cast<size_t>(AtomicBlockSchedulingPass::RemovePredication)) &&
                            !memoryDesc.Memory().HasReadArbitration() && dstOp->_flags._loadMemory._isPredicated)
                        {
                            // If no read arbitration is needed, then predication is for
                            // power savings only, and does not affect correctness
                            // Remove the predication if scheduling cannot
                            // be satisified otherwise
                            const_cast<Operation*>(dstOp)->_flags._loadMemory._isPredicated = false;

                            // remove the predicate operand
                            const_cast<Operation*>(dstOp)->_src.pop_back();

                            assert(2 == dstOp->_src.size());
                        }

                        if (!dstOp->_flags._loadMemory._isPredicated)
                        {
                            // If predication was removed previously
                            // and this constraint is for the predicate, then
                            // treat the constraint as a nop
                            return;
                        }
                    }
                    else if (isExternCallPredicateConstraint)
                    {
                        assert(Opcode::InlineExternalModule == dstOp->_opcode);
                        assert(dstOp->_flags._callInlineExternalModule._isPure);

                        if (dstAtomicBlockPass >= static_cast<size_t>(AtomicBlockSchedulingPass::RemovePredication))
                        {
                            // Scheduling has failed with the predicate
                            // Remove the predicate by setting it to a hard-coded 1
                            const_cast<Operation*>(dstOp)->_src[0] = SourceOperand(1);
                        }

                        if (dstOp->_src[0] == SourceOperand(1))
                        {
                            // If predication was removed previously
                            // and this constraint is for the predicate, then
                            // treat the constraint as a nop
                            return;
                        }
                    }
                }

                // Determine if the desired logic/register ratio would be exceeded if dstOp was placed in the same stage
                // as srcOp
                const OperationPathLength operationPathLength = ComputeOperationPathLength(
                    selectedStagePathLengthMap, dstOperationMetadata->_denseSrcReg, dstOperationMetadata->_pathLength);

                const bool srcHasRegisteredOutput = OperationHasRegisteredOutput(*srcOp);
                const bool dstHasRegisteredInput = OperationHasRegisteredInput(*dstOp);

                const bool globalRawWawHazard = IsGlobalRawWawHazard(program, *srcOp, *dstOp);

                bool hasSlackForRouting = true;

                // Input routing delay of the destination operation is ignored
                // if the destination operation is in an atomic block
                // because the additional constraints from routing delay
                // may make scheduling impossible
                if ((inputRoutingDelay > 0) && !dstOperationInAtomicBlock)
                {
                    assert(dstHasRegisteredInput);

                    // There is a routing delay to get from the source operation
                    // to the destination operation.  Determine if there is
                    // enough slack in srcPipelineStage to account for the
                    // routing.  If there is enough slack, then the destination
                    // operation can run in srcPipelineStage + 1 Otherwise, the
                    // destination operation must run in srcPipelineStage + 2
                    for (const size_t srcOutputRegReg : srcOperationMetadata->_denseDstReg)
                    {
                        for (const size_t dstInputReg : dstOperationMetadata->_denseSrcReg)
                        {
                            if (srcOutputRegReg == dstInputReg)
                            {
                                if ((selectedStagePathLengthMap[srcOutputRegReg] + inputRoutingDelay) >
                                    relaxedLogicLevels)
                                {
                                    // There is not enough slack in srcPipelineStage to
                                    // schedule the memory/dsp in srcPipelineStage + 1
                                    hasSlackForRouting = false;
                                }
                            }
                        }
                    }
                }

                // True if the logic/register ratio threshold would be exceeded
                const bool pathLengthExceeded = (operationPathLength._inputPlusOperationLength > relaxedLogicLevels);

                // True if other constraints (atomic block size) allow scheduling the destination operation in another
                // pipeline stage
                const bool canMoveToNextStage = !ignoreRegisterRatio && (allowedRange._end > (srcPipelineStage + 1));

                if (!canMoveToNextStage)
                {
                    // The only case where there should be an upper bound on pipeline stage is:
                    // When the destination is in an atomic block
                    assert((dependencyGraph.operationToAtomicBlockIndex.end() != dstOperationToAtomicBlockIndexIt));
                }

                const bool isBeginAtomic = Opcode::BeginAtomic == dstOp->_opcode;

                // Data dependency requires that
                //   The dstNode must be scheduled at a pipeline stage >= the srcNode
                //   The logic/register ratio threshold must not be exceeded (unless this would cause the containing
                //   atomic block max depth to be exceeded) If a producer has registered output, the consumer must run
                //   in a pipeline stage after the producer If a consumer has registered input, it must run in a
                //   pipeline stage after the producer If the destination is a BeginAtomic, then it must run in a fresh
                //   pipeline stage If the producer writes to a global and the consumer reads from it or writes to it,
                //   then the consumer must run in a pipeline stage after the producer
                if (!hasSlackForRouting)
                {
                    // dstop is a memory/dsp with input routing delay
                    // and there is not enough slack in srcPipelineStage to route
                    newConstraint._begin = srcPipelineStage + 2;
                }
                else if ((pathLengthExceeded && canMoveToNextStage) || srcHasRegisteredOutput ||
                         dstHasRegisteredInput || globalRawWawHazard || isBeginAtomic)
                {
                    newConstraint._begin = srcPipelineStage + 1;
                }
                else if (!pathLengthExceeded || ignorePathLengthExceeded)
                {
                    newConstraint._begin = srcPipelineStage;
                }
                else
                {
                    // Set impossible constraint to indicate that scheduling failed
                    newConstraint._begin = std::numeric_limits<size_t>::max();
                    newConstraint._end = 0;
                }

                // MovLatencyPlaceholder is special because it must be scheduled immediately after the producer node
                if (dstOp->_opcode == Opcode::MovLatencyPlaceholder)
                {
                    // Registered input and output ensure this
                    assert(newConstraint._begin == (srcPipelineStage + 1));
                    newConstraint._end = newConstraint._begin + 1;
                }

                allowedRange.Intersect(newConstraint);
            };

            scheduler.AddConstraint(srcNode, dstNode, constraintFunction, "data hazard");

            const bool srcHasRegisteredOutput = OperationHasRegisteredOutput(*srcOp);
            const bool dstHasRegisteredInput = OperationHasRegisteredInput(*dstOp);

            const bool srcHasHardenedOutput = srcHasRegisteredOutput && OpcodeUsesHardenedRegisters(srcOp->_opcode);
            const bool dstHasHardenedInput = dstHasRegisteredInput && OpcodeUsesHardenedRegisters(dstOp->_opcode);

            if (srcHasHardenedOutput && dstHasHardenedInput)
            {
                // Output registers of the source and input registers
                // of the destination are contained within the modules themselves
                // The data traveling between them must flow through 1 level of pipeline registers
                // in the generated code, because there is no way to the src output and dst input registers together
                const ConstraintScheduler::constraint_function_t registerConstraintFunction =
                    [srcOp, dstOp](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                {
                    ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                    // Double-check that input/output registers are still enabled
                    // if scheduling previously failed for a containing atomic block
                    // then the operations may have been modified to disable hardened registers
                    const bool srcHasRegisteredOutput = OperationHasRegisteredOutput(*srcOp);
                    const bool dstHasRegisteredInput = OperationHasRegisteredInput(*dstOp);

                    const bool srcHasHardenedOutput =
                        srcHasRegisteredOutput && OpcodeUsesHardenedRegisters(srcOp->_opcode);
                    const bool dstHasHardenedInput =
                        dstHasRegisteredInput && OpcodeUsesHardenedRegisters(dstOp->_opcode);

                    if (srcHasHardenedOutput && dstHasHardenedInput)
                    {
                        newConstraint._begin = srcPipelineStage + 2;

                        allowedRange.Intersect(newConstraint);
                    }
                };

                // If the destination is in an atomic block with update rate = 1
                // and the source is not in the same atomic block
                // then apply the constraint to the atomic block that constains the destination
                ConstraintScheduler::node_t registerConstraintDstNode = dstNode;

                const auto dstOperationToAtomicBlockIndexIt = dependencyGraph.operationToAtomicBlockIndex.find(dstOp);

                if (dstOperationToAtomicBlockIndexIt != dependencyGraph.operationToAtomicBlockIndex.end())
                {
                    const std::vector<size_t>& dstAtomicBlockIndexVector = dstOperationToAtomicBlockIndexIt->second;
                    for (size_t di = 0; di < dstAtomicBlockIndexVector.size(); di++)
                    {
                        const size_t dstAtomicBlockIndex = dstAtomicBlockIndexVector[di];
                        const Operation* const beginOp =
                            SafeLookup(dependencyGraph.atomicBlockIndexToBeginAtomicOp, dstAtomicBlockIndex);

                        assert(beginOp->_opcode == Opcode::BeginAtomic);

                        const size_t updateRate = beginOp->_flags._atomicBlockDesc._updateRate;

                        // If the destination is in an atomic block with update
                        // rate = 1, it must be the leaf-most atomic block because
                        // equally or less restrictive nested atomics are removed
                        if (di == dstAtomicBlockIndexVector.size() - 1)
                        {
                            if (updateRate == 1)
                            {
                                // Check if source is in the same atomic block
                                bool srcInDstAtomicBlock = false;

                                const auto srcOperationToAtomicBlockIndexIt =
                                    dependencyGraph.operationToAtomicBlockIndex.find(srcOp);
                                const bool srcOperationInAtomicBlock =
                                    (dependencyGraph.operationToAtomicBlockIndex.end() !=
                                     srcOperationToAtomicBlockIndexIt);

                                if (srcOperationInAtomicBlock)
                                {
                                    for (const size_t srcAtomicBlockIndex : srcOperationToAtomicBlockIndexIt->second)
                                    {
                                        if (srcAtomicBlockIndex == dstAtomicBlockIndex)
                                        {
                                            srcInDstAtomicBlock = true;
                                            break;
                                        }
                                    }
                                }

                                // If source is not in the same atomic block
                                if (!srcInDstAtomicBlock)
                                {
                                    // Apply contraint
                                    registerConstraintDstNode = SafeLookup(operationToNode, beginOp);
                                }
                            }
                        }
                        else
                        {
                            assert(updateRate != 1);
                        }
                    }
                }

                scheduler.AddConstraint(srcNode, registerConstraintDstNode, registerConstraintFunction,
                                        "hardened output to input register");
            }
        }
    }

    // Stage barriers
    for (auto it = basicBlock._operations.begin(); it != basicBlock._operations.end(); ++it)
    {
        const Operation& op = *it;

        // Opcode::Stage is treated as a barrier when not in an atomic block
        const bool inAtomicBlock =
            dependencyGraph.operationToAtomicBlockIndex.end() != dependencyGraph.operationToAtomicBlockIndex.find(&op);

        if (Opcode::Stage == op._opcode)
        {
            // These operations are treated as barriers
            assert(!inAtomicBlock);

            // All preceding operations must run before the barrier (or at the same time)
            for (auto it2 = basicBlock._operations.begin(); it2 != it; ++it2)
            {
                const Operation& otherOp = *it2;

                const ConstraintScheduler::constraint_function_t constraintFunction =
                    [](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                {
                    ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                    newConstraint._begin = srcPipelineStage;

                    allowedRange.Intersect(newConstraint);
                };

                scheduler.AddConstraint(SafeLookup(operationToNode, &otherOp), SafeLookup(operationToNode, &op),
                                        constraintFunction, "before stage barrier");
            }

            // All following operations must run after the barrier
            auto it2 = it;
            ++it2;

            for (/*nothing*/; it2 != basicBlock._operations.end(); ++it2)
            {
                const Operation& otherOp = *it2;

                const ConstraintScheduler::constraint_function_t constraintFunction =
                    [](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                {
                    ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                    newConstraint._begin = srcPipelineStage + 1;

                    allowedRange.Intersect(newConstraint);
                };

                scheduler.AddConstraint(SafeLookup(operationToNode, &op), SafeLookup(operationToNode, &otherOp),
                                        constraintFunction, "after stage barrier");
            }
        }
    }

    // Line number barriers
    for (auto it = basicBlock._operations.begin(); it != basicBlock._operations.end(); ++it)
    {
        const Operation& op = *it;

        const Operation* nextLineOp = nullptr;

        // Opcode::LineNumber is treated as a barrier when not in an atomic block
        const bool inAtomicBlock =
            dependencyGraph.operationToAtomicBlockIndex.end() != dependencyGraph.operationToAtomicBlockIndex.find(&op);

        if ((Opcode::LineNumber == op._opcode) && !inAtomicBlock)
        {
            std::vector<const Operation*> operationsAfterLineNum;

            // These operations are treated as barriers
            assert(!inAtomicBlock);
            assert(GetCodeGenConfig()._optimize == 0);

            // Operations after the current line number (and before the next line number) should not be scheduled
            // before the current line number
            OperationList::iterator it2 = it;
            ++it2;
            for (/* nothing */; it2 != basicBlock._operations.end(); ++it2)
            {
                const Operation& otherOp = *it2;

                if (Opcode::LineNumber != otherOp._opcode)
                {
                    operationsAfterLineNum.push_back(&otherOp);
                }
                else
                {
                    // This is a line number but need to check if it is contained within an atomic block
                    // in which case it should not be treated as a barrier.
                    const bool inAtomicBlock = dependencyGraph.operationToAtomicBlockIndex.end() !=
                                               dependencyGraph.operationToAtomicBlockIndex.find(&otherOp);

                    if (!inAtomicBlock)
                    {
                        nextLineOp = &otherOp;

                        // Line number not contained within an atomic block.
                        // Treat as barrier.
                        // Add constraints for the operations in operationsAfterLineNum
                        break;
                    }
                }
            }

            for (const Operation* op2 : operationsAfterLineNum)
            {
                const ConstraintScheduler::constraint_function_t constraintFunction =
                    [](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                {
                    ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                    newConstraint._begin = srcPipelineStage;

                    allowedRange.Intersect(newConstraint);
                };

                // Operations after the current line number should not be scheduled before the current line number
                scheduler.AddConstraint(SafeLookup(operationToNode, &op), SafeLookup(operationToNode, op2),
                                        constraintFunction, "line number");

                if (nextLineOp)
                {
                    const ConstraintScheduler::constraint_function_t constraintFunction =
                        [](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                    {
                        ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                        newConstraint._begin = srcPipelineStage + 1;

                        allowedRange.Intersect(newConstraint);
                    };

                    // Operations after current line number should be scheduled before next line number
                    scheduler.AddConstraint(SafeLookup(operationToNode, op2), SafeLookup(operationToNode, nextLineOp),
                                            constraintFunction, "line number");
                }
            }

            // The next line number must run after the current line number
            if (it2 != basicBlock._operations.end())
            {
                const Operation& otherOp = *it2;

                assert(Opcode::LineNumber == otherOp._opcode);

                const ConstraintScheduler::constraint_function_t constraintFunction =
                    [](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                {
                    ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                    newConstraint._begin = srcPipelineStage + 1;

                    allowedRange.Intersect(newConstraint);
                };

                scheduler.AddConstraint(SafeLookup(operationToNode, &op), SafeLookup(operationToNode, &otherOp),
                                        constraintFunction, "line number");
            }
        }
    }

    // Assert barriers
    // to prevent prints from being reordered past assertions
    std::list<const Operation*> assertBarrierOps;

    for (const Operation& op : basicBlock._operations)
    {
        if (Opcode::Assert == op._opcode)
        {
            const auto assertAtomicBlocksIt = dependencyGraph.operationToAtomicBlockIndex.find(&op);

            const bool assertInAtomic = assertAtomicBlocksIt != dependencyGraph.operationToAtomicBlockIndex.end();

            for (const Operation* const prevOp : assertBarrierOps)
            {
                const auto prevOpAtomicBlocksIt = dependencyGraph.operationToAtomicBlockIndex.find(prevOp);

                const bool prevInAtomic = prevOpAtomicBlocksIt != dependencyGraph.operationToAtomicBlockIndex.end();

                // Avoid constraining the atomic in a way
                // that could make scheduling fail.
                bool addConstraint = false;

                if (prevInAtomic && assertInAtomic)
                {
                    if (prevOpAtomicBlocksIt->second == assertAtomicBlocksIt->second)
                    {
                        // Both operations are in the same set of atomic/schedule blocks
                        // Scheduling of those blocks will not be impacted
                        // by requiring the assert to not be moved before the previous operation
                        addConstraint = true;
                    }
                }
                else if (!assertInAtomic)
                {
                    // The assert is not in an atomic/schedule block
                    // So there is no problem constraining it to potentially
                    // run later than it otherwise would
                    addConstraint = true;
                }

                if (addConstraint)
                {
                    const ConstraintScheduler::constraint_function_t constraintFunction =
                        [](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                    {
                        ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                        newConstraint._begin = srcPipelineStage;

                        allowedRange.Intersect(newConstraint);
                    };

                    scheduler.AddConstraint(SafeLookup(operationToNode, prevOp), SafeLookup(operationToNode, &op),
                                            constraintFunction, "assert barrier");
                }
            }
        }
        else if (DoesOpcodeRequireAssertBarrier(op._opcode))
        {
            assertBarrierOps.push_back(&op);
        }
    }

    // Atomic blocks
    std::stack<const Operation*> beginAtomicStack;
    const Operation* previousEndAtomic = nullptr;

    for (const Operation& op : basicBlock._operations)
    {
        const Operation* const operationPointer = &op;

        if (Opcode::BeginAtomic == op._opcode)
        {
            if (previousEndAtomic &&
                (op._flags._atomicBlockDesc._chainIndex == previousEndAtomic->_flags._atomicBlockDesc._chainIndex))
            {
                // The (up to 3) atomic blocks for for atomic memory access must run back to back
                // The _chainIndex check above validates that the 2 atomic blocks stem from the same atomic block in the
                // source
                const ConstraintScheduler::constraint_function_t constraintFunction =
                    [previousEndAtomic, &op](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                {
                    ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                    size_t distance = 1;

                    if ((AtomicBlockType::MemoryLoad == previousEndAtomic->_flags._atomicBlockDesc._type) &&
                        (AtomicBlockType::MemoryStore == op._flags._atomicBlockDesc._type))
                    {
                        // Don't directly connect memory output register to memory input register
                        distance = 2;
                    }

                    newConstraint._begin = srcPipelineStage + distance;
                    newConstraint._end = newConstraint._begin + distance;

                    allowedRange.Intersect(newConstraint);
                };

                scheduler.AddConstraint(SafeLookup(operationToNode, previousEndAtomic),
                                        SafeLookup(operationToNode, operationPointer), constraintFunction,
                                        "atomic chaining");
            }

            beginAtomicStack.push(operationPointer);
        }

        if (Opcode::EndAtomic == op._opcode)
        {
            assert(!beginAtomicStack.empty());
            const Operation* previousBeginAtomic = beginAtomicStack.top();
            beginAtomicStack.pop();

            // Add a BeginAtomic->EndAtomic constraint to ensure
            // that the user-specified stage count is exactly honored
            const ConstraintScheduler::constraint_function_t constraintFunction =
                [previousBeginAtomic](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
            {
                ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                // For the typical case of UpdateRate == 1, the EndAtomic is scheduled in the same stage as the
                // BeginAtomic
                newConstraint._begin = srcPipelineStage + previousBeginAtomic->_flags._atomicBlockDesc._updateRate - 1;
                newConstraint._end = newConstraint._begin + 1;

                allowedRange.Intersect(newConstraint);
            };

            scheduler.AddConstraint(SafeLookup(operationToNode, previousBeginAtomic),
                                    SafeLookup(operationToNode, operationPointer), constraintFunction,
                                    "begin->end atomic");

            previousEndAtomic = &op;
        }

        const auto it = dependencyGraph.operationToAtomicBlockIndex.find(operationPointer);
        if (it != dependencyGraph.operationToAtomicBlockIndex.end())
        {
            // Async calls can be delayed until after the EndAtomic
            // This is a safe transformation because there is no guarantee about
            // the delay between when the call is made, and when callee function execution starts
            if (!IsAsyncCall(op))
            {
                // For each atomic block that this operation is contained within
                for (const size_t atomicBlockIndex : it->second)
                {
                    // This operation is contained within an atomic block
                    const Operation* const beginOp =
                        SafeLookup(dependencyGraph.atomicBlockIndexToBeginAtomicOp, atomicBlockIndex);
                    assert(beginOp->_opcode == Opcode::BeginAtomic);

                    {
                        // Add a constraint between the BeginAtomic and this operation
                        // This causes BeginAtomic to act as a stand-in for the whole block
                        // And the entire block does not start scheduling until the BeginAtomic is scheduled
                        const ConstraintScheduler::constraint_function_t constraintFunction =
                            [beginOp](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                        {
                            ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                            newConstraint._begin = srcPipelineStage;
                            newConstraint._end = srcPipelineStage + beginOp->_flags._atomicBlockDesc._updateRate;

                            allowedRange.Intersect(newConstraint);
                        };

                        scheduler.AddConstraint(SafeLookup(operationToNode, beginOp),
                                                SafeLookup(operationToNode, operationPointer), constraintFunction,
                                                "begin atomic->op");
                    }
                }

                for (const size_t atomicBlockIndex : it->second)
                {
                    {
                        // Add a constraint between this operation and EndAtomic
                        const Operation* const endOp =
                            SafeLookup(dependencyGraph.atomicBlockIndexToEndAtomicOp, atomicBlockIndex);
                        assert(endOp->_opcode == Opcode::EndAtomic);

                        const ConstraintScheduler::constraint_function_t constraintFunction =
                            [](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                        {
                            ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                            newConstraint._begin = srcPipelineStage;

                            allowedRange.Intersect(newConstraint);
                        };

                        scheduler.AddConstraint(SafeLookup(operationToNode, operationPointer),
                                                SafeLookup(operationToNode, endOp), constraintFunction,
                                                "op->end atomic");
                    }
                }
            }

            {
                const size_t atomicBlockIndex = it->second.back();

                const Operation* const beginOp =
                    SafeLookup(dependencyGraph.atomicBlockIndexToBeginAtomicOp, atomicBlockIndex);
                assert(beginOp->_opcode == Opcode::BeginAtomic);

                // Only apply to leaf atomic blocks
                if (!beginOp->_flags._atomicBlockDesc._containsAtomic)
                {
                    // Writes to globals are scheduled in the last stage of the atomic block
                    // And reads from globals are scheduled in the first stage of the atomic block
                    // to enable code to specify how much an loop can be unrolled
                    // This is safe because of the transformation applied by CheckAndRewriteAtomics
                    // It adds a mov operation to read from all globals (these can be scheduled early)
                    // and it converts all writes to write to locals, and then updates the global at the end of the
                    // block

                    // If KillMoves could optimze away moves with global sources
                    // reads from globals might potentially not have Opcode::Mov (and have other sources besides the
                    // global, which would cause unsolvable constraint graph)
                    assert(!IsRegisterTypeAllowedInKillMoves(RegisterType::Global));

                    bool forcePipelineStageForGlobalAccess = false;
                    size_t forcedPipelineStageOffset = 0;
                    bool writeConstraint = false;

                    assert(beginOp->_flags._atomicBlockDesc._updateRate > 0);
                    assert(!beginOp->_flags._atomicBlockDesc._containsAtomic);

                    if (Opcode::WriteGlobal == op._opcode)
                    {
                        forcePipelineStageForGlobalAccess = true;
                        forcedPipelineStageOffset = beginOp->_flags._atomicBlockDesc._updateRate - 1;

                        writeConstraint = true;
                    }
                    else
                    {
                        for (const SourceOperand& srcOp : op._src)
                        {
                            if (SourceOperandType::Register == srcOp.Type())
                            {
                                const RegisterDescription& regDesc =
                                    program._registerTable[srcOp.GetAccessedRegister()._registerIndex];
                                if (RegisterType::Global == regDesc._type)
                                {
                                    // CheckAndRewriteAtomics ensures a move operation to read from the global and write
                                    // into a local If this was not true, then there could be other sources of the
                                    // operation which might not be scheduled in the first pipeline stage
                                    // CheckAndRewriteAtomics does not rewrite loads
                                    // from memory where the source operand is a global
                                    // MemToArray can insert gather operations from globals
                                    assert((Opcode::Mov == op._opcode) || (Opcode::LoadMemory == op._opcode) ||
                                           (Opcode::Gather == op._opcode));

                                    forcePipelineStageForGlobalAccess = true;
                                    forcedPipelineStageOffset = 0;

                                    writeConstraint = false;
                                }
                            }
                        }
                    }

                    if (forcePipelineStageForGlobalAccess)
                    {
                        const ConstraintScheduler::constraint_function_t constraintFunction =
                            [forcedPipelineStageOffset](const size_t srcPipelineStage,
                                                        ConstraintScheduler::Range& allowedRange)
                        {
                            ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                            newConstraint._begin = srcPipelineStage + forcedPipelineStageOffset;
                            newConstraint._end = newConstraint._begin + 1;

                            allowedRange.Intersect(newConstraint);
                        };

                        scheduler.AddConstraint(SafeLookup(operationToNode, beginOp),
                                                SafeLookup(operationToNode, operationPointer), constraintFunction,
                                                writeConstraint ? "global write in final stage"
                                                                : "global read in first stage");
                    }
                }
            }
        }
    }
    assert(beginAtomicStack.empty());

    // Operations with global side effects should not be reordered - to prevent another thread from seeing out of order
    // writes The operations are
    // * Read/Write globals (registers or memories).  Reads can be reordered past other reads.
    // * Enqueue operations (the only enqeue operations in BasicBlock._operations are async function calls -
    // conservatively assume the called function may access global state
    // * Atomic blocks - this are considered separately to avoid situations where a BeginAtomic is scheduled too soon
    // (because it does not take into account these constraints within the atomic block)
    {
        // Find deepest atomic nesting
        // Use a minimum of 1 so that following algorithm will run at least one
        // iteration even when atomics do not exist.
        size_t deepestAtomicBlockDepth = 1;
        {
            size_t atomicBlockDepth = 0;
            for (const Operation& op : basicBlock._operations)
            {
                if (Opcode::BeginAtomic == op._opcode)
                {
                    atomicBlockDepth++;
                    deepestAtomicBlockDepth = std::max<size_t>(deepestAtomicBlockDepth, atomicBlockDepth);
                }
                else if (Opcode::EndAtomic == op._opcode)
                {
                    assert(atomicBlockDepth > 0);
                    atomicBlockDepth--;
                }
            }
            assert(atomicBlockDepth == 0);
        }
        assert(deepestAtomicBlockDepth > 0);

        // Apply to one nesting depth of atomic blocks at a time
        for (size_t atomicBlockDepthIteration = 0; atomicBlockDepthIteration < deepestAtomicBlockDepth;
             atomicBlockDepthIteration++)
        {
            std::list<const Operation*> previousSharedReads;
            const Operation* previousSharedWrite = nullptr;

            size_t atomicBlockDepth = 0;

            std::vector<size_t> registersRead;
            std::vector<size_t> registersWritten;

            for (const Operation& op : basicBlock._operations)
            {
                const Operation* const operationPointer = &op;

                bool shouldAddConstraint = false;
                bool treatAsRead = false;
                bool treatAsWrite = false;
                bool isAsyncCall = false;

                if (Opcode::BeginAtomic == op._opcode)
                {
                    atomicBlockDepth++;

                    if (atomicBlockDepth == atomicBlockDepthIteration + 1)
                    {
                        // Don't allow this new atomic block to be reordered before the previous global operation
                        shouldAddConstraint = true;

                        treatAsWrite = true;
                    }
                }
                else if (Opcode::EndAtomic == op._opcode)
                {
                    assert(atomicBlockDepth > 0);
                    atomicBlockDepth--;
                }
                else
                {
                    if (atomicBlockDepth == atomicBlockDepthIteration)
                    {
                        GetAccessedRegisters(op, registersRead, registersWritten);

                        for (const size_t r : registersRead)
                        {
                            if (RegisterTypeDisallowsReordering(program._registerTable[r]._type))
                            {
                                shouldAddConstraint = true;
                                treatAsRead = true;
                            }
                        }

                        for (const size_t r : registersWritten)
                        {
                            if (RegisterTypeDisallowsReordering(program._registerTable[r]._type))
                            {
                                shouldAddConstraint = true;
                                treatAsWrite = true;
                            }
                        }

                        if (Opcode::Enqueue == op._opcode)
                        {
                            if (IsAsyncCall(op))
                            {
                                shouldAddConstraint = true;
                                isAsyncCall = true;
                            }
                            else
                            {
                                shouldAddConstraint = true;
                                treatAsWrite = true;
                            }
                        }

                        // Don't allow reference string operations to be reordered backward
                        // With out this, it is impossible to implement manual reference counting when strings are
                        // placed into memories as the manual reference count -1 operations can be scheduled before a
                        // use of the string
                        if (Opcode::ReferenceString == op._opcode)
                        {
                            shouldAddConstraint = true;
                            treatAsWrite = true;
                        }
                    }
                }

                if (shouldAddConstraint)
                {
                    // All constraints should be classified as read or write or an async call
                    assert(treatAsRead || treatAsWrite || isAsyncCall);

                    const auto addConstraint = [&](const Operation* const previousOperation)
                    {
                        // Note that the two operations are allowed to run concurrently
                        // There is no way for another thread to detect the difference between concurrent execution
                        // and sequential execution.  What is disallowed is operationPointer being scheduled before
                        // previousOperation
                        const ConstraintScheduler::constraint_function_t constraintFunction =
                            [previousOperation](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
                        {
                            ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

                            newConstraint._begin = srcPipelineStage;

                            if (Opcode::BeginAtomic == previousOperation->_opcode)
                            {
                                // The (previousOperation->_flags._atomicBlockDesc._updateRate - 1) is there to handle
                                // atomic blocks with an update rate > 1 If the code contains: atomic block A (update
                                // rate = 4), that writes to a global in the last stage followed by B: some other
                                // operation that writes to a global Then B cannot run until the last stage of A,
                                // otherwise the writes that B performs might be visible before the writes that A
                                // performs
                                newConstraint._begin += (previousOperation->_flags._atomicBlockDesc._updateRate - 1);
                            }

                            allowedRange.Intersect(newConstraint);
                        };

                        scheduler.AddConstraint(SafeLookup(operationToNode, previousOperation),
                                                SafeLookup(operationToNode, operationPointer), constraintFunction,
                                                "shared resource ordering");
                    };

                    if (previousSharedWrite)
                    {
                        // schedule this operation after the preceding write
                        addConstraint(previousSharedWrite);
                    }

                    if (treatAsWrite)
                    {
                        if (!previousSharedReads.empty())
                        {
                            // schedule this write after all preceding reads
                            for (const Operation* const p : previousSharedReads)
                            {
                                addConstraint(p);
                            }

                            // no need to track previous reads of shared variables
                            // future writes and reads will have a constraint on
                            // the current operation, which transitively will add a constraint
                            // on previous shared reads
                            previousSharedReads.clear();
                        }

                        previousSharedWrite = operationPointer;
                    }
                    else if (treatAsRead)
                    {
                        previousSharedReads.push_back(operationPointer);

                        // note that previousSharedWrite is not set to null here
                        // other reads also have constraints on previousSharedWrite
                    }
                    else
                    {
                        // An async call must run after any preceding shared
                        // writes but that tis the only constraint.  If it fine
                        // to move async calls forward in the schedule past
                        // other shared resource resources and writes because
                        // there is no semantic difference between that and the
                        // async call parameters sitting in a fifo for a long
                        // time
                        assert(isAsyncCall);
                    }
                }
            }
        }
    }

    // Handle operations which access the string table
    // To ensure that the string table is treated as a resource which can be read and written
    {
        std::list<const Operation*> previousStringTableReaders;
        const Operation* previousStringTableWriter = nullptr;

        const ConstraintScheduler::constraint_function_t constraintFunction =
            [](const size_t srcPipelineStage, ConstraintScheduler::Range& allowedRange)
        {
            ConstraintScheduler::Range newConstraint = ConstraintScheduler::Range::Infinite();

            // No need for +1 here, because string operations within a pipeline stage
            // are ordered to avoid issues like use after free
            newConstraint._begin = srcPipelineStage;

            allowedRange.Intersect(newConstraint);
        };

        for (const Operation& op : basicBlock._operations)
        {
            const Operation* const operationPointer = &op;

            if (OpcodeWritesToStringTable(op._opcode))
            {
                if (previousStringTableWriter)
                {
                    scheduler.AddConstraint(SafeLookup(operationToNode, previousStringTableWriter),
                                            SafeLookup(operationToNode, operationPointer), constraintFunction,
                                            "string table WAW");
                }

                for (const Operation* const previousReader : previousStringTableReaders)
                {
                    scheduler.AddConstraint(SafeLookup(operationToNode, previousReader),
                                            SafeLookup(operationToNode, operationPointer), constraintFunction,
                                            "string table WAR");
                }

                previousStringTableWriter = operationPointer;
                previousStringTableReaders.clear();
            }
            else if (OperationReadsFromStringTable(op))
            {
                if (previousStringTableWriter)
                {
                    scheduler.AddConstraint(SafeLookup(operationToNode, previousStringTableWriter),
                                            SafeLookup(operationToNode, operationPointer), constraintFunction,
                                            "string table RAW");
                }

                previousStringTableReaders.push_back(operationPointer);
            }
        }
    }

    ConstraintScheduler::delay_map_t bestDelayMap;
    schedule_t schedule;

    // Callback that is called when an operation is assigned to a pipeline stage
    ConstraintScheduler::schedule_function_t scheduleCallback =
        [&](const ConstraintScheduler::node_t node, const size_t pipelineStage)
    {
        const OperationMetaData* const operationMetadata = SafeLookup(nodeToOperation, node);

        // Update per-stage register path length maps
        RegPathLengthMap& selectedStagePathLengthMap = getRegPathLengthMapForStage(pipelineStage, denseRegisterCount);

        const OperationPathLength operationPathLength = ComputeOperationPathLength(
            selectedStagePathLengthMap, operationMetadata->_denseSrcReg, operationMetadata->_pathLength);

        UpdateRegPathLength(selectedStagePathLengthMap, operationMetadata->_denseDstReg,
                            operationPathLength._inputPlusOperationLength);

        // Remember that this operation was scheduled at the specified pipeline stage
        schedule[pipelineStage].push_back(operationMetadata->_op);

        SafeInsert(opToStage, operationMetadata, pipelineStage);

        const size_t outputRoutingDelay = GetOutputRoutingDelay(operationMetadata->_op->_opcode);

        if (outputRoutingDelay > 0)
        {
            // The scheduled operation is a DSP/Memory with output routing delay
            // Update the RegPathLengthMap for the next pipeline stage for all
            // destinations of the operation (to account for this delay)
            // Output routing delay is ignored if output registers are
            // disabled
            if (OperationHasRegisteredOutput(*operationMetadata->_op))
            {
                RegPathLengthMap& nextStagePathLengthMap =
                    getRegPathLengthMapForStage(pipelineStage + 1, denseRegisterCount);

                for (const size_t registerIndex : operationMetadata->_denseDstReg)
                {
                    assert(nextStagePathLengthMap[registerIndex] == 0);

                    nextStagePathLengthMap[registerIndex] = static_cast<uint32_t>(outputRoutingDelay);
                }
            }
        }
    };

    // Function that schedules the basic block for a given delay map
    const auto scheduleOnce = [&](const ConstraintScheduler::delay_map_t& delayMap)
    {
        // For atomic blocks that contain operations with registered input/output
        // multiple attempts may be needed
        bool didSchedule = false;

        while (!didSchedule)
        {
            schedule.clear();
            opToStage.clear();

            // Zero each vector rather than calling allRegPathLengthMaps.clear
            // to avoid reallocation
            for (auto& p : allRegPathLengthMaps)
            {
                RegPathLengthMap& map = p.second;

                assert(map.size() == denseRegisterCount);

                std::fill(map.begin(), map.end(), 0);
            }

            didSchedule = scheduler.Schedule(scheduleCallback, delayMap, basicBlock.GetThreadRate(), dependencyGraph);
        }
    };

    // Start with delay map that schedules all operations as soon as possible
    for (const auto& p : operationToNode)
    {
        const ConstraintScheduler::node_t node = p.second;

        SafeInsert(bestDelayMap, node, static_cast<size_t>(0));
    }

    // Iterating through all of the operations once is enough to realize all of the gains from this optimization
    size_t outerLoopIterations = 1;

    // This optimization algorithm is expensive
    // Only do it for optimization levels >= 2
    if (GetCodeGenConfig()._optimize < 2)
    {
        outerLoopIterations = 0;
    }

    // Get the score of bestDelayMap (the intial score to beat)
    scheduleOnce(bestDelayMap);
    size_t baselineScore = ScoreSchedule(program, schedule);
    size_t baselineStageCount = GetSchedulePipelineStageCount(schedule);

    // Operations with more local outputs than local inputs are more likely
    // to benefit from delaying their start state.
    // Sort operations by the difference between local inputs and local outputs.
    // Then shrink the list of operations if necessary to stay within the user-specified limit.
    std::vector<const Operation*> operationsToDelay;

    for (const Operation& op : basicBlock._operations)
    {
        // Only allow delay for operations that are not in atomic blocks, to avoid affecting fmax.
        if (dependencyGraph.operationToAtomicBlockIndex.end() == dependencyGraph.operationToAtomicBlockIndex.find(&op))
        {
            operationsToDelay.push_back(&op);
        }
    }

    const auto scoreOperation = [&](const Operation& op)
    {
        int64_t result = 0;

        {
            std::set<size_t> srcRegs;

            for (const SourceOperand& srcOp : op._src)
            {
                if (SourceOperandType::Register == srcOp.Type())
                {
                    const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                    const RegisterDescription& rd = program._registerTable[registerIndex];

                    if (IsLocalRegisterType(rd._type))
                    {
                        // Don't double count a register
                        if (!Contains(srcRegs, registerIndex))
                        {
                            // Adding width here moves operations with many local inputs to the end of the list (lower
                            // priority)
                            result += static_cast<int64_t>(rd._width);

                            srcRegs.insert(registerIndex);
                        }
                    }
                }
            }
        }

        {
            std::set<size_t> dstRegs;

            for (const DestinationOperand& dstOp : op._dst)
            {
                if (DestinationOperandType::Register == dstOp.Type())
                {
                    const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

                    const RegisterDescription& rd = program._registerTable[registerIndex];

                    if (IsLocalRegisterType(rd._type))
                    {
                        // Don't double count a register
                        if (!Contains(dstRegs, registerIndex))
                        {
                            // Subtracting width here moves operations with many local outputs toward the begining of
                            // the list (higher priority)
                            result -= static_cast<int64_t>(rd._width);

                            dstRegs.insert(registerIndex);
                        }
                    }
                }
            }
        }

        return result;
    };

    std::sort(operationsToDelay.begin(), operationsToDelay.end(),
              [&](const Operation* const lhs, const Operation* const rhs)
              { return scoreOperation(*lhs) < scoreOperation(*rhs); });

    const size_t scheduleLimit = static_cast<size_t>(GetCodeGenConfig()._scheduleLimit);
    if (operationsToDelay.size() > scheduleLimit)
    {
        if (GetCodeGenConfig()._verbosity == VerbosityLevel::Verbose)
        {
            std::cout << GetBasicBlockName(basicBlock) << " contains " << operationsToDelay.size()
                      << " instructions to potentially delay, which is above the scheduling limit of: " << scheduleLimit
                      << ".  Increasing the scheduling limit may reduce register usage.";
        }

        operationsToDelay.resize(scheduleLimit);
    }
    assert(operationsToDelay.size() <= scheduleLimit);

    std::set<const Operation*> setOfOperationsToDelay(operationsToDelay.begin(), operationsToDelay.end());

    for (size_t iterationIndex = 0; iterationIndex < outerLoopIterations; ++iterationIndex)
    {
        // Delay operations in the order in which they appear in the basic block
        // This produces better results than delaying operations in the order they appear in operationsToDelay
        for (const Operation& op : basicBlock._operations)
        {
            // Only consider operations for delay if they were prioritized above
            if (!Contains(setOfOperationsToDelay, &op))
            {
                continue;
            }

            // Operations in atomic blocks are not candidates for delay
            assert(dependencyGraph.operationToAtomicBlockIndex.end() ==
                   dependencyGraph.operationToAtomicBlockIndex.find(&op));

            const ConstraintScheduler::node_t node = SafeLookup(operationToNode, &op);

            // Delay `op` by 1 cycle, then 2, then 3, until no improvement is made
            while (true)
            {
                ++bestDelayMap[node];

                scheduleOnce(bestDelayMap);
                const size_t newScore = ScoreSchedule(program, schedule);
                const size_t newStageCount = GetSchedulePipelineStageCount(schedule);

                if ((newScore < baselineScore) && (newStageCount <= baselineStageCount))
                {
                    // Found an improved schedule
                    // This is the new score to beat
                    baselineScore = newScore;
                    baselineStageCount = newStageCount;
                }
                else
                {
                    // Revert changes from this iteration
                    --bestDelayMap[node];

                    break;
                }
            }
        }
    }

    if (outerLoopIterations > 0)
    {
        scheduleOnce(bestDelayMap);
    }
    const size_t finalScore = ScoreSchedule(program, schedule);

    const size_t numStages = GetSchedulePipelineStageCount(schedule);

    for (size_t stageIndex = 0; stageIndex < numStages; stageIndex++)
    {
        std::list<const Operation*> operations = schedule[stageIndex];

        // Sort operations within this stage to be in the same order
        // that they appear in the input operation list
        // This enables consistent output run-to-run (for example, there are some maps with Operation* as the key)
        const auto compareFunc = [&](const Operation* const lhs, const Operation* const rhs)
        {
            const size_t seqLhs = SafeLookup(operationSequenceNumberMap, lhs);
            const size_t seqRhs = SafeLookup(operationSequenceNumberMap, rhs);

            return seqLhs < seqRhs;
        };

        operations.sort(compareFunc);

        // Each pipeline stage is divided into multiple Stage structures
        // Within a given Stage structure, all operations can run in parallel
        EmitPipelineStage(basicBlock, operations);
    }

    const bool isFixedLatency = basicBlock._function->_functionNode->IsFixedLatency();

    // Appended stages go at the end
    for (Stage& stage : appendedStages)
    {
        // For fixed-latency functions, appended stage operations are all merged into the last stage
        // to not introduce a lower bound on the allowable latency
        stage._atomicSequence =
            isFixedLatency ? basicBlock.GetCurrAtomicSequenceNumber() : basicBlock.GetNextAtomicSequenceNumber();

        basicBlock._stages.push_back(stage);
    }
}
