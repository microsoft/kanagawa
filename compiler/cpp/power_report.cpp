// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

// Included separately to avoid rapidsjon overriding assert definition globally
#include "report.h"

enum class ClockGatingPathElement
{
    Object,
    Function,
    BasicBlock,
    VariableName,

    Count
};

enum class ClockGatingTupleElement
{
    NonGated,
    Total,

    Count
};

void WriteClockGatingReport(const std::string& fileName, const Program& program)
{
    const size_t ColumnCount = static_cast<size_t>(ClockGatingTupleElement::Count);
    const size_t PathElementCount = static_cast<size_t>(ClockGatingPathElement::Count);

    using ReportBuilderT = ReportBuilder<ColumnCount, PathElementCount>;

    ReportBuilderT::ColumnArray columns = {};

    columns[static_cast<size_t>(ClockGatingTupleElement::NonGated)] = {"Non-Gated Registers", "Bits"};
    columns[static_cast<size_t>(ClockGatingTupleElement::Total)] = {"Total Registers", "Bits"};

    ReportBuilderT::PathElementNameArray pathElementNames = {};

    pathElementNames[static_cast<size_t>(ClockGatingPathElement::Object)] = "Object";
    pathElementNames[static_cast<size_t>(ClockGatingPathElement::Function)] = "Function";
    pathElementNames[static_cast<size_t>(ClockGatingPathElement::BasicBlock)] = "Basic block";
    pathElementNames[static_cast<size_t>(ClockGatingPathElement::VariableName)] = "Variable";

    const ReportBuilderT::ProjectionVector projections(
        {{"Default View", {0, 1, 2, 3}, {ReportBuilderT::AggregationRule::Sum, ReportBuilderT::AggregationRule::Sum}}});

    ReportBuilderT report(fileName, columns, pathElementNames, projections);

    std::vector<size_t> readSet;
    std::vector<size_t> writeSet;

    ReportBuilderT::Path path = {};

    for (const Function& function : program._functions)
    {
        // Tokenize flat object name into array of nested object names
        path[static_cast<size_t>(ClockGatingPathElement::Object)] = TokenizeObjectName(function._objectName, program);

        path[static_cast<size_t>(ClockGatingPathElement::Function)] = ReportBuilderT::SimplePathElement(function._name);

        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            path[static_cast<size_t>(ClockGatingPathElement::BasicBlock)] =
                ReportBuilderT::SimplePathElement(GetBasicBlockName(basicBlock));

            const std::vector<PipelineStage> pipelineStages = GetPipelineStages(const_cast<BasicBlock&>(basicBlock));

            // Variable name to gated/total bits
            std::map<std::string, ReportBuilderT::Tuple> nameToTuple;

            for (const PipelineStage& ps : pipelineStages)
            {
                assert(!ps.empty());

                // Track all pipeline registers written by any substage within this stage
                for (const Stage* const subStage : ps)
                {
                    for (const Operation& op : subStage->_operations)
                    {
                        GetAccessedRegisters(op, readSet, writeSet);

                        for (const size_t r : writeSet)
                        {
                            const RegisterDescription& rd = program._registerTable[r];

                            if (RegisterType::Pipeline == rd._type)
                            {
                                const size_t width = rd._width;

                                const auto it = subStage->_clockGates.find(r);

                                const bool isGated = (it != subStage->_clockGates.end()) && (!it->second.empty());

                                ReportBuilderT::Tuple tuple = {};
                                tuple[static_cast<size_t>(ClockGatingTupleElement::NonGated)] = isGated ? 0 : width;
                                tuple[static_cast<size_t>(ClockGatingTupleElement::Total)] = width;

                                const auto it2 = nameToTuple.find(rd._name);
                                if (it2 == nameToTuple.end())
                                {
                                    nameToTuple[rd._name] = tuple;
                                }
                                else
                                {
                                    for (size_t i = 0; i < ColumnCount; i++)
                                    {
                                        it2->second[i] += tuple[i];
                                    }
                                }
                            }
                        }
                    }
                }
            }

            for (const auto& p : nameToTuple)
            {
                path[static_cast<size_t>(ClockGatingPathElement::VariableName)] =
                    ReportBuilderT::SimplePathElement(p.first);

                report.AppendRow(path, basicBlock._location, p.second);
            }
        }
    }
}
