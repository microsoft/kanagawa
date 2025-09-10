// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Optimization runs in phases
// Each phase runs a set of optimization functions until convergence
enum class OptimizationPhase
{
    Start = 0,
    PackLuts1 = 1,
    PackLuts2 = 2,
    LutCommonSub = 3,
    Synthesis = 4,

    Count
};

inline bool IsLocalRegister(const Program& program, const size_t registerIndex)
{
    const RegisterType registerType = program._registerTable[registerIndex]._type;

    return (RegisterType::Local == registerType);
}

inline bool IsLocalRegister(const Program& program, const AccessedRegister& reg)
{
    return IsLocalRegister(program, reg._registerIndex);
}

bool FixupLutOps(Program& program, Function& function);
bool PackLuts(Program& program, Function& function, const size_t phaseIndex);
bool BitConstantPropagation(Program& program, ControlFlowGraph& controlFlowGraph, Function& function,
                            ConstantBitMap* const constantBitMap);

// Counts the number of reads for each  register read by a basic block
typedef std::map<size_t, size_t> RegisterReadCountMap;

RegisterReadCountMap ComputeRegisterReadCountMap(const Program& program, const BasicBlock& basicBlock);

size_t MapOriginalToRenamedRegisterIndex(const Operation& op, const size_t originalIndex);
size_t OptionallyMapIndex(const size_t originalIndex, const RegisterIndexMap& table);

// Helper class for optimizations that require mapping from uses to definitions
class LocalDefUseTracker
{
  public:
    LocalDefUseTracker(const Program& program, const BasicBlock& basicBlock)
        : _program(program), _useCountMap(ComputeRegisterReadCountMap(program, basicBlock))
    {
    }

    void HandleOperation(const Operation& operation, const bool trackDef)
    {
        // If the operation implies a barrier, then discard all def-use
        // information gathered up to this point
        if (DoesOpcodeImplyOptimizationBarrier(operation._opcode))
        {
            _definitionMap.clear();
        }

        // Only track def-use information for operations that reference only
        // local registers - to avoid introducing visible memory ordering differences
        if (trackDef && OperationUsesOnlyLocalReg(_program, operation))
        {
            for (const DestinationOperand& dstOp : operation._dst)
            {
                SafeInsert(_definitionMap, dstOp.GetAccessedRegister()._registerIndex, &operation);
            }
        }
    }

    // Attempts to lookup the operation that wrote to a particular register
    // Returns nullptr if the lookup fails
    const Operation* GetDef(const size_t registerIndex) const
    {
        const Operation* result = nullptr;

        const auto it = _definitionMap.find(registerIndex);
        if (it != _definitionMap.end())
        {
            result = it->second;

            // Only local register should be placed into _definitionMap
            assert(RegisterType::Local == _program._registerTable[registerIndex]._type);
        }

        return result;
    }

    const Operation* GetDef(const SourceOperand& srcOperand) const
    {
        if (SourceOperandType::Register == srcOperand.Type())
        {
            return GetDef(srcOperand.GetAccessedRegister()._registerIndex);
        }
        else
        {
            return nullptr;
        }
    }

    size_t GetUseCount(const size_t registerIndex) const { return SafeLookup(_useCountMap, registerIndex); }

  private:
    const Program& _program;

    // Maps local register index to operation that wrote that register
    std::map<size_t, const Operation*> _definitionMap;

    // Maps register index to the number of uses of that register
    RegisterReadCountMap _useCountMap;
};

bool ConstantPropagation(Program& program, ControlFlowGraph& controlFlowGraph, Function& function,
                         const OperationEnumerationMode mode);

OperationList ReduceBinaryOp(Program& program, const AccessedRegister& srcReg, const ParseTreeBinaryOpType reduceOp,
                             const DestinationOperand& dstOp, const Operation& srcLocationOp);

bool RemoveDeadJumps(Function& function);

void ScheduleBasicBlock(const Program& program, BasicBlock& basicBlock);

void CollectPerStageLineNumberInfo(const Program& program, BasicBlock& basicBlock);

void Optimize(IRContext& context);

bool RemoveDeadOperations(Program& program, ControlFlowGraph& controlFlowGraph, Function& function,
                          const OperationEnumerationMode mode);

bool DecomposeSelect(Program& program, BasicBlock& basicBlock, OperationList::iterator it, const size_t threshold);

bool DecomposeShift(Program& program, BasicBlock& basicBlock, OperationList::iterator it);

void SparseToDenseReg(Program& program);

void EarlyOptimization(Program& program);

void RemoveStringOps(Program& program);

bool ValidateIR(const Program& program, Function& function);
