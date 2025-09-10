// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

// Implements conditional updates to local variables via muxes
// Each ConditionalLocalUpdates represents a set of mutually exclusive
// cases.  Before any case, a backup copy is made of each register
// that will be written by one or more cases.
// After each case completes, the values of each local register
// is restored to the backup value.  This enables all cases to execute in parallel.
// Additional, a separate register is stored that holds the
// final value of each written register (the value from the selected case)
class ConditionalLocalUpdates : public RegisterAccessNotification
{
  public:
    enum class PredicateMode
    {
        Normal,

        // Apply the inverse of the predicate
        Inverse
    };

    ConditionalLocalUpdates(IRContext& context, const Location& location);

    ~ConditionalLocalUpdates();

    void BeginCase(const size_t predicateRegisterIndex, const PredicateMode mode);

    void EndCase();

    void NotifyWrite(const size_t registerIndex, const size_t registerConditionStackDepth) override;

    void SetComplete();

  private:
    IRContext& _context;

    Location _location;

    size_t _currentCase;

    size_t _conditionStackDepth;

    AccessedRegister _currentPredicateRegister;

    PredicateMode _currentPredicateMode;

    struct UpdatedRegister
    {
        bool _initialized = false;

        // Index of register that holds backup copy
        AccessedRegister _backupRegister;

        // Index of register that holds the final selected value
        AccessedRegister _updatedValueRegister;

        // which cases updated the register
        std::set<size_t> _writtenCases;

        // Location of the first select operation
        // that writes to _updatedValueRegister
        std::unique_ptr<OperationLocationRecord> _firstUpdateLocation;
    };

    // Maps original register -> UpdatedRegister
    std::map<size_t, UpdatedRegister> _updatedRegisters;

    // The location within the current operation list
    // when this object was created
    OperationLocationRecord _backupInsertLocation;

    // Operations which will be inserted at the location pointed at to by _backupInsertLocation
    OperationList _operationsToAdd;
};

// Set of clock gate registers
using ClockGateSet = std::set<size_t>;

// (local register -> local gate registers)
// if any gate register is 0, then the local register can be ignored
// If an entry is not in the map, or has an empty set
// then it cannot be ignored
using BasicBlockClockGateMap = std::map<size_t, ClockGateSet>;

// BasicBlock -> (local register->local gate register)
using FunctionClockGateMap = std::map<BasicBlock*, BasicBlockClockGateMap>;

bool ComputeClockGating(Program& program, Function& function, const OperationEnumerationMode mode,
                        FunctionClockGateMap* clockGateMapOut);