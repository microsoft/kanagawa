// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

// Included separately to avoid rapidsjon overriding assert definition globally
#include "report.h"

enum class PathLengthPathElement
{
    Object,
    Function,
    BasicBlock,
    Stage,
    OperationName,

    Count
};

enum class PathLengthTupleElement
{
    OperationLength,
    PathLength,

    Count
};

void WritePathLengthReport(const std::string& fileName, const Program& program)
{
    const size_t ColumnCount = static_cast<size_t>(PathLengthTupleElement::Count);
    const size_t PathElementCount = static_cast<size_t>(PathLengthPathElement::Count);

    using ReportBuilderT = ReportBuilder<ColumnCount, PathElementCount>;

    ReportBuilderT::ColumnArray columns = {};

    columns[static_cast<size_t>(PathLengthTupleElement::OperationLength)] = {"Op_Depth", "level"};
    columns[static_cast<size_t>(PathLengthTupleElement::PathLength)] = {"Path_Depth", "level"};

    ReportBuilderT::PathElementNameArray pathElementNames = {};

    pathElementNames[static_cast<size_t>(PathLengthPathElement::Object)] = "Object";
    pathElementNames[static_cast<size_t>(PathLengthPathElement::Function)] = "Function";
    pathElementNames[static_cast<size_t>(PathLengthPathElement::BasicBlock)] = "Basic block";
    pathElementNames[static_cast<size_t>(PathLengthPathElement::Stage)] = "Stage";
    pathElementNames[static_cast<size_t>(PathLengthPathElement::OperationName)] = "Operation";

    const ReportBuilderT::ProjectionVector projections(
        {{"Default View",
          {0, 1, 2, 3, 4},
          {ReportBuilderT::AggregationRule::Max, ReportBuilderT::AggregationRule::Max}}});

    ReportBuilderT report(fileName, columns, pathElementNames, projections);

    for (const Function& function : program._functions)
    {
        ReportBuilderT::Path path = {};

        // Tokenize flat object name into array of nested object names
        path[static_cast<size_t>(PathLengthPathElement::Object)] = TokenizeObjectName(function._objectName, program);

        path[static_cast<size_t>(PathLengthPathElement::Function)] = ReportBuilderT::SimplePathElement(function._name);

        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            path[static_cast<size_t>(PathLengthPathElement::BasicBlock)] =
                ReportBuilderT::SimplePathElement(GetBasicBlockName(basicBlock));

            const std::vector<PipelineStage> pipelineStages = GetPipelineStages(const_cast<BasicBlock&>(basicBlock));

            for (const PipelineStage& ps : pipelineStages)
            {
                // Distance from the first use to the end of the basic block
                PathLengthTracker pathLengthTracker(program);

                assert(!ps.empty());

                // Make operation names unique, the visualizer drops duplicates
                size_t opId = 0;

                // Track all pipeline registers written by any substage within this stage
                for (const Stage* const subStage : ps)
                {
                    const std::string stageName = "stage" + std::to_string(subStage->_atomicSequence);
                    path[static_cast<size_t>(PathLengthPathElement::Stage)] =
                        ReportBuilderT::SimplePathElement(stageName);

                    for (const Operation& op : subStage->_operations)
                    {
                        const size_t pathLength = pathLengthTracker.UpdateForward(op);
                        const size_t opLength = GetOpPathLength(program, op, pathLengthTracker.GetCarryChainWidth());

                        if (opLength > 0)
                        {
                            const std::string opName = std::to_string(opId) + "_" + GetOpcodeString(program, op);
                            opId++;

                            // Multiple source locations can contribute to this operation,
                            // but the visualizes only allows one file location.
                            // Pick the first location, if any.
                            Location loc = {};
                            const auto it = op._locations.cbegin();
                            if (it != op._locations.cend())
                            {
                                loc._beginLine = it->_lineNumber;
                                loc._endLine = it->_lineNumber + 1;
                                loc._fileIndex = it->_fileIndex;
                                loc._valid = true;
                            }
                            else
                            {
                                loc._valid = false;
                            }

                            ReportBuilderT::Tuple tuple = {};
                            tuple[static_cast<size_t>(PathLengthTupleElement::OperationLength)] = opLength;
                            tuple[static_cast<size_t>(PathLengthTupleElement::PathLength)] = pathLength;

                            path[static_cast<size_t>(PathLengthPathElement::OperationName)] =
                                ReportBuilderT::SimplePathElement(opName);

                            report.AppendRow(path, loc, tuple);
                        }
                    }
                }
            }
        }
    }
}
