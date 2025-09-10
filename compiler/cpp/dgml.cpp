// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

static std::string BasicBlockName(const BasicBlock& basicBlock) { return GetBasicBlockName(basicBlock); }

static std::string FifoMergerName(const FIFOMerger& fifoMerger)
{
    std::ostringstream str;

    str << "fifo_merger_";
    str << fifoMerger._sources[0];

    return str.str();
}

static std::string ContextSaverName(const ContextSaver& contextSaver)
{
    std::ostringstream str;

    str << "context_saver_";
    str << contextSaver._fromCallerFifoIndex;

    return str.str();
}

static std::string ExportFunctionReturnName(const Function& function)
{
    std::ostringstream str;

    str << "return_from_export_";
    if (function._objectName != g_globalObjectName)
    {
        str << function._objectName << "::";
    }

    str << function._name;

    return str.str();
}

std::string GetFifoLabel(const Program& program, const size_t index)
{
    const FifoResourceUsage resourceUsage = GetFifoResourceUsage(program, index);

    std::ostringstream str;

    str << "BRAMs: " << resourceUsage._brams << " ALMs: " << resourceUsage._alms;

    return str.str();
}

struct link_t
{
    size_t _src;
    size_t _dst;
    std::string _label;

    link_t(const size_t s, const size_t d, const std::string& l) : _src(s), _dst(d), _label(l) {}
};

typedef std::pair<size_t, std::string> node_t;

void WriteDgmlInternal(const char* const fileName, const std::vector<node_t>& nodes, const std::vector<link_t>& links)
{
    StringSourceWriter writer;

    std::set<size_t> validNodes;
    for (const node_t& node : nodes)
    {
        validNodes.insert(node.first);
    }

    writer.Str() << "<DirectedGraph xmlns=\"http://schemas.microsoft.com/vs/2009/dgml\">";

    writer.Str() << "<Nodes>";

    for (const node_t& node : nodes)
    {
        writer.Str() << "<Node Id=\"" << node.first << "\" Label=\"" << node.second << "\" />";
    }

    writer.Str() << "</Nodes>";

    writer.Str() << "<Links>";

    for (const link_t& link : links)
    {
        // VSCode DGML viewer extension fails if a link references a
        // non-existent node
        assert(validNodes.end() != validNodes.find(link._src));
        assert(validNodes.end() != validNodes.find(link._dst));
        writer.Str() << "<Link Source=\"" << link._src << "\" Target=\"" << link._dst << "\" Label=\"" << link._label
                     << "\" />";
    }

    writer.Str() << "</Links>";

    writer.Str() << "</DirectedGraph>";

    FileSourceWriter fileWriter(fileName);

    const std::string unescaped = writer.GetString();

    // Escape characters inside of strings
    std::string escaped;

    bool inString = false;

    for (const auto ch : unescaped)
    {
        if (ch == '\"')
        {
            inString = !inString;
        }

        if (inString)
        {
            switch (ch)
            {
            case '<':
                escaped.append("&lt;");
                break;
            case '>':
                escaped.append("&gt;");
                break;
            case '&':
                escaped.append("&amp;");
                break;
            default:
                escaped.append(1, ch);
                break;
            }
        }
        else
        {
            escaped.append(1, ch);
        }
    }

    assert(!inString);

    fileWriter.Str() << escaped;
}

// Output the program in DGML format
void WriteDgml(const char* const fileName, const Program& program)
{
    std::vector<node_t> nodes;
    std::vector<link_t> links;

    // Construct mapping of register index to node label
    std::unordered_map<size_t, size_t> registerMap;

    // Start with identity
    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        registerMap[i] = i;
    }

    // Fifo mergers are identified by port 0
    for (const FIFOMerger& fifoMerger : program._fifoMergers)
    {
        for (const size_t src : fifoMerger._sources)
        {
            registerMap[src] = fifoMerger._sources[0];
        }
    }

    // Context savers are identified by _fromCallerFifoIndex
    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        registerMap[contextSaver._fromCalleeFifoIndex] = contextSaver._fromCallerFifoIndex;
    }

    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            for (size_t i = 1; i < basicBlock._inputFifoCount; i++)
            {
                registerMap[basicBlock._inputFifoIndices[i]] = basicBlock._inputFifoIndices[0];
            }
        }
    }

    // Add nodes for returns from sync exported functions
    // and for returns from [[reset]] functions
    for (const Function& function : program._functions)
    {
        if (function._returnFifoRegisterIndex != c_invalidAccessedRegisterIndex)
        {
            nodes.push_back(node_t(function._returnFifoRegisterIndex, ExportFunctionReturnName(function)));
        }
    }

    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            // Basic block node
            nodes.push_back(node_t(basicBlock._inputFifoIndices[0], BasicBlockName(basicBlock)));

            // Links to other fifos
            for (const Stage& stage : basicBlock._stages)
            {
                for (const Operation& op : stage._operations)
                {
                    if (Opcode::Enqueue == op._opcode)
                    {
                        links.push_back(link_t(basicBlock._inputFifoIndices[0],
                                               registerMap[op._flags._enqueue._successorFifo],
                                               GetFifoLabel(program, op._flags._enqueue._successorFifo)));
                    }
                }
            }
        }
    }

    for (const FIFOMerger& fifoMerger : program._fifoMergers)
    {
        nodes.push_back(node_t(fifoMerger._sources[0], FifoMergerName(fifoMerger)));

        links.push_back(
            link_t(fifoMerger._sources[0], registerMap[fifoMerger._dest], GetFifoLabel(program, fifoMerger._dest)));
    }

    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        nodes.push_back(node_t(contextSaver._fromCallerFifoIndex, ContextSaverName(contextSaver)));

        links.push_back(link_t(contextSaver._fromCallerFifoIndex, registerMap[contextSaver._destinationFifo],
                               GetFifoLabel(program, contextSaver._destinationFifo)));
    }

    WriteDgmlInternal(fileName, nodes, links);
}

bool IsPipelineOrWire(const Program& program, const SourceOperand& srcOp)
{
    bool result = false;

    if (SourceOperandType::Register == srcOp.Type())
    {
        const RegisterDescription& regDesc = program._registerTable[srcOp.GetAccessedRegister()._registerIndex];

        switch (regDesc._type)
        {
        case RegisterType::Pipeline:
        case RegisterType::Wire:
            result = true;
            break;
        default:
            break;
        }
    }

    return result;
}

bool IsPipelineOrWire(const Program& program, const DestinationOperand& dstOp)
{
    bool result = false;

    if (DestinationOperandType::Register == dstOp.Type())
    {
        const RegisterDescription& regDesc = program._registerTable[dstOp.GetAccessedRegister()._registerIndex];

        switch (regDesc._type)
        {
        case RegisterType::Pipeline:
        case RegisterType::Wire:
            result = true;
            break;
        default:
            break;
        }
    }

    return result;
}

bool ShouldOutputOp(const Program& program, const Operation& op)
{
    bool result = true;

    switch (op._opcode)
    {
        // Moving live values betwee stages is not interesting
    case Opcode::Mov:
        if (IsPipelineOrWire(program, op._src[0]) && IsPipelineOrWire(program, op._dst[0]))
        {
            result = false;
        }
        break;

        // Semaphore operations do not have any inputs or outputs
    case Opcode::AcquireSemaphore:
    case Opcode::ReleaseSemaphore:
        result = false;
        break;

    default:
        break;
    }

    return result;
}

// Outputs a DGML file for each basic block, show all stages and operations
void WriteDetailedDgml(const char* const directoryName, const Program& program)
{
    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            size_t operationCount = 0;

            for (const Stage& stage : basicBlock._stages)
            {
                operationCount += stage._operations.size();
            }

            // Real stages with pipeline registers in-between
            const size_t realStageCount = basicBlock._stages.back()._atomicSequence + 1;

            // each file shows only 1 stage
            const size_t stagesPerFile = 1;

            for (size_t startStage = 0; startStage < realStageCount; startStage += stagesPerFile)
            {
                const size_t endStage = std::min<size_t>(startStage + stagesPerFile, realStageCount);

                std::ostringstream fileName;
                fileName << directoryName << "\\" << BasicBlockName(basicBlock) << "_" << startStage << ".dgml";

                std::vector<node_t> nodes;
                std::vector<link_t> links;

                std::map<size_t, std::set<size_t>> registerReadMap;

                // Base node id is chosen to not conflict with register IDs
                const size_t baseNodeId = program._registerTable.size();

                std::set<size_t> nonWireRegisterIds;

                std::map<const Operation*, size_t> operationToNodeIdMap;

                {
                    size_t currentNodeId = baseNodeId;

                    for (const Stage& stage : basicBlock._stages)
                    {
                        if ((stage._atomicSequence >= startStage) && (stage._atomicSequence < endStage))
                        {
                            for (const Operation& op : stage._operations)
                            {
                                if (ShouldOutputOp(program, op))
                                {
                                    const size_t operationNodeId = currentNodeId;

                                    operationToNodeIdMap[&op] = operationNodeId;

                                    ++currentNodeId;

                                    for (const SourceOperand& srcOp : op._src)
                                    {
                                        if (SourceOperandType::Register == srcOp.Type())
                                        {
                                            const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                                            const RegisterDescription& regDesc = program._registerTable[registerIndex];

                                            if (RegisterType::Wire != regDesc._type)
                                            {
                                                nonWireRegisterIds.insert(registerIndex);
                                            }

                                            registerReadMap[registerIndex].insert(operationNodeId);
                                        }
                                        else if (SourceOperandType::Literal == srcOp.Type())
                                        {
                                            const mp_int value = srcOp.GetLiteral()._value;

                                            std::ostringstream str;
                                            str << value;

                                            const size_t literalNodeId = currentNodeId;
                                            ++currentNodeId;

                                            node_t node(literalNodeId, str.str());
                                            nodes.push_back(node);

                                            const link_t link(literalNodeId, operationNodeId, "");
                                            links.push_back(link);
                                        }
                                    }

                                    for (const DestinationOperand& dstOp : op._dst)
                                    {
                                        if (DestinationOperandType::Register == dstOp.Type())
                                        {
                                            const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

                                            const RegisterDescription& regDesc = program._registerTable[registerIndex];

                                            if ((RegisterType::Wire != regDesc._type) ||
                                                RegisterTypeRequiresPipelineStage(regDesc._type))
                                            {
                                                nonWireRegisterIds.insert(registerIndex);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                for (const size_t registerIndex : nonWireRegisterIds)
                {
                    std::ostringstream str;
                    str << "Reg" << registerIndex;

                    node_t node(registerIndex, str.str());

                    nodes.push_back(node);
                }

                // Nodes & links for operations
                for (const Stage& stage : basicBlock._stages)
                {
                    if ((stage._atomicSequence >= startStage) && (stage._atomicSequence < endStage))
                    {
                        for (const Operation& op : stage._operations)
                        {
                            if (ShouldOutputOp(program, op))
                            {
                                const size_t nodeId = operationToNodeIdMap[&op];

                                node_t node(nodeId, GetOpcodeString(program, op));

                                nodes.push_back(node);

                                for (const SourceOperand& srcOp : op._src)
                                {
                                    if (SourceOperandType::Register == srcOp.Type())
                                    {
                                        const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                                        if (nonWireRegisterIds.end() != nonWireRegisterIds.find(registerIndex))
                                        {
                                            const link_t link(registerIndex, nodeId,
                                                              program._registerTable[registerIndex]._name);

                                            links.push_back(link);
                                        }
                                    }
                                }

                                for (const DestinationOperand& dstOp : op._dst)
                                {
                                    if (DestinationOperandType::Register == dstOp.Type())
                                    {
                                        const size_t registerIndex = dstOp.GetAccessedRegister()._registerIndex;

                                        if (nonWireRegisterIds.end() != nonWireRegisterIds.find(registerIndex))
                                        {
                                            const link_t link(nodeId, registerIndex,
                                                              program._registerTable[registerIndex]._name);

                                            links.push_back(link);
                                        }
                                        else
                                        {
                                            const RegisterDescription& regDesc = program._registerTable[registerIndex];

                                            if (RegisterType::Wire == regDesc._type)
                                            {
                                                const std::set<size_t> readerNodes = registerReadMap[registerIndex];

                                                for (const size_t readerNode : readerNodes)
                                                {
                                                    const link_t link(nodeId, readerNode,
                                                                      program._registerTable[registerIndex]._name);

                                                    links.push_back(link);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                WriteDgmlInternal(fileName.str().c_str(), nodes, links);
            }
        }
    }
}
