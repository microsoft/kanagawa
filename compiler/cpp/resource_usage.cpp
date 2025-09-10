// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

#include "boost/exception/to_string.hpp"
#include <numeric>

static std::ostream& operator<<(std::ostream& str, const FifoResourceUsage& fifoResourceUsage)
{
    str << fifoResourceUsage._brams << "\t" << fifoResourceUsage._alms;

    return str;
}

// Write text file that contains estimates of resource usages
void WriteResourceUsage(const char* const fileName, const Program& program)
{
    std::ofstream str;
    OpenOutputFileStream(str, fileName);

    const char* const separator = "=======================";

    // Placement sorting records
    for (const Placement::SortRecord sortRecord : program._sortRecords)
    {
        if (sortRecord._names.empty())
        {
            continue;
        }

        str << separator << "Sorting for: " << sortRecord._title << "\n";

        for (const std::string& name : sortRecord._names)
        {
            str << name << "\n";
        }

        str << "\n";
    }

    // Memories
    size_t longestName = 0;

    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];

        if (RegisterType::Memory == regDesc._type)
        {
            longestName = std::max(longestName, regDesc._name.size());
        }
    }

    std::ostringstream bramStr;
    std::ostringstream lutramStr;
    std::ostringstream uramStr;

    const CodeGenConfig& config = GetCodeGenConfig();
    const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();

    const int replicaWidth = 8;
    const int totalSizeWidth = 18;
    const int estResourcesWidth = 12;

    size_t totalLutRams = 0;
    size_t totalBlockRams = 0;
    size_t totalDeepRams = 0;

    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];

        if (RegisterType::Memory == regDesc._type)
        {
            const size_t replicaCount = regDesc.Memory().ReplicaCount();

            const auto width = regDesc.Memory()._elementWidth;
            const auto depth = regDesc.Memory()._elementCount;

            const size_t totalSize = width * depth * replicaCount;

            if (ShouldUseRamType(regDesc, RAM_TYPE::LUT))
            {
                auto lutRamUsage =
                    GetMemoryResourceUsage(deviceConfig._memory._lutRamConfigs, width, depth, replicaCount).first;
                totalLutRams += lutRamUsage;

                lutramStr << std::left << std::setw(longestName) << regDesc._name << "\t" << std::setw(replicaWidth)
                          << replicaCount << "\t" << std::right << std::setw(totalSizeWidth) << totalSize << "\t"
                          << std::right << std::setw(estResourcesWidth) << lutRamUsage << "\n";
            }
            else if (ShouldUseRamType(regDesc, RAM_TYPE::BLOCK))
            {
                // ROMs can be implemented using Block RAMs with true dual port mode
                const bool ROM = (regDesc.Memory()._writePortCount == 0);

                auto blockRamUsage =
                    GetMemoryResourceUsage(
                        deviceConfig._memory._blockRamConfigs, width, depth,
                        // If ROM, can use true dual port thus halve the number of replicas (rounded up)
                        (ROM) ? (replicaCount + 1) / 2 : replicaCount)
                        .first;
                totalBlockRams += blockRamUsage;

                bramStr << std::left << std::setw(longestName) << regDesc._name << "\t" << std::setw(replicaWidth)
                        << replicaCount << "\t" << std::right << std::setw(totalSizeWidth) << totalSize << "\t"
                        << std::right << std::setw(estResourcesWidth) << blockRamUsage << "\n";
            }
            else
            {
                auto deepRamUsage =
                    GetMemoryResourceUsage(deviceConfig._memory._deepRamConfigs, width, depth, replicaCount).first;
                totalDeepRams += deepRamUsage;

                uramStr << std::left << std::setw(longestName) << regDesc._name << "\t" << std::setw(replicaWidth)
                        << replicaCount << "\t" << std::right << std::setw(totalSizeWidth) << totalSize << "\t"
                        << std::right << std::setw(estResourcesWidth) << deepRamUsage << "\n";
            }
        }
    }

    str << separator << "\nURAM\n"
        << std::left << std::setw(longestName) << "Name"
        << "\t" << std::right << std::setw(replicaWidth) << "Replicas"
        << "\t" << std::right << std::setw(totalSizeWidth) << "Total URAM(bits)"
        << "\t" << std::right << std::setw(estResourcesWidth) << "Est. URAMs"
        << "\n";
    str << uramStr.str();
    str << "Total Est. URAMs:\t" << totalDeepRams << "\n\n";

    str << separator << "\nBRAM\n"
        << std::left << std::setw(longestName) << "Name"
        << "\t" << std::right << std::setw(replicaWidth) << "Replicas"
        << "\t" << std::right << std::setw(totalSizeWidth) << "Total BRAM(bits)"
        << "\t" << std::right << std::setw(estResourcesWidth) << "Est. BRAMs"
        << "\n";
    str << bramStr.str();
    str << "Total Est. BRAMs:\t" << totalBlockRams << "\n\n";

    str << separator << "\nLUTRAM\n"
        << std::left << std::setw(longestName) << "Name"
        << "\t" << std::right << std::setw(replicaWidth) << "Replicas"
        << "\t" << std::right << std::setw(totalSizeWidth) << "Total LUTRAM(bits)"
        << "\t" << std::right << std::setw(estResourcesWidth) << "Est. LUTRAMs"
        << "\n";
    str << lutramStr.str();
    str << "Total Est. LUTRAMs:\t" << totalLutRams << "\n\n";

    // Fifos
    FifoNamer fifoNamer(program);

    str << separator << "\nFIFOs\nName\tBRAMs\tALMs\n";

    FifoResourceUsage total = {};

    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];

        // Passthrough fifos do not consume resources
        if (RegisterType::Fifo == regDesc._type)
        {
            const FifoResourceUsage fifoResourceUsage = GetFifoResourceUsage(program, i);

            if (fifoResourceUsage._brams != 0 || fifoResourceUsage._alms != 0)
            {
                str << fifoNamer.GetFifoName(i) << "\t" << fifoResourceUsage << "\n";

                total._brams += fifoResourceUsage._brams;
                total._alms += fifoResourceUsage._alms;
            }
        }
    }

    str << "Total\t" << total << "\n\n";

    // Functions with multiple call sites
    str << separator << "\nFunctions with multiple call sites\nName\tCallSiteCount\n";

    for (const Function& function : program._functions)
    {
        if (function._numCallSites > 1)
        {
            if (function._objectName != g_globalObjectName)
            {
                str << function._objectName << "::";
            }

            str << function._name << "\t" << function._numCallSites << "\n";
        }
    }

    // Pipeline registers
    str << separator << "\nPipelineRegisters\nBasicBlock\tStage\tRegisterBits\n";

    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            const std::vector<PipelineStage> pipelineStages = GetPipelineStages(const_cast<BasicBlock&>(basicBlock));

            for (size_t pipelineStageIndex = 0; pipelineStageIndex < pipelineStages.size(); ++pipelineStageIndex)
            {
                size_t pipelineStageBits = 0;

                for (const Stage* const stage : pipelineStages[pipelineStageIndex])
                {
                    for (const Operation& op : stage->_operations)
                    {
                        for (const DestinationOperand& dst : op._dst)
                        {
                            if (DestinationOperandType::Register == dst.Type())
                            {
                                const size_t registerIndex = dst.GetAccessedRegister()._registerIndex;

                                const RegisterDescription& regDesc = program._registerTable[registerIndex];

                                if (RegisterType::Pipeline == regDesc._type)
                                {
                                    pipelineStageBits += regDesc._width;
                                }
                            }
                        }
                    }
                }

                str << GetBasicBlockName(basicBlock) << "\t" << pipelineStageIndex << "\t" << pipelineStageBits << "\n";
            }
        }
    }

    const size_t carryChainWidthPerLogicLevel = GetCodeGenConfig()._carryChainWidthPerLogicLevel;

    // Pipeline registers used for routing live variables
    // This is displayed in a few different ways
    {
        std::ostringstream byFunction;
        std::ostringstream byStage;
        std::ostringstream byName;

        byFunction << separator << "\nLiveVariablePipelineRegisters By Function\nFunction\tRegisterBits\n";
        byStage << separator << "\nLiveVariablePipelineRegisters By Stage\nBasicBlock\tStage\tRegisterBits\n";
        byName << separator
               << "\nLiveVariablePipelineRegisters By Variable Name\nFunction\tVariableName\tRegisterBits\n";

        size_t totalBits = 0;

        for (const Function& function : program._functions)
        {
            size_t functionBits = 0;

            std::map<std::string, size_t> nameToSizeMap;

            for (const BasicBlock& basicBlock : function._basicBlocks)
            {
                const std::vector<PipelineStage> pipelineStages =
                    GetPipelineStages(const_cast<BasicBlock&>(basicBlock));

                for (size_t pipelineStageIndex = 0; pipelineStageIndex < pipelineStages.size(); ++pipelineStageIndex)
                {
                    // the set of local registers that were the result of an operation that required compute (like a
                    // lookup table)
                    std::set<size_t> computeSet;

                    size_t pipelineStageBits = 0;

                    for (const Stage* const stage : pipelineStages[pipelineStageIndex])
                    {
                        for (const Operation& op : stage->_operations)
                        {
                            bool addDstToComputeSet = false;

                            if (GetOpPathLength(program, op, carryChainWidthPerLogicLevel) > 0)
                            {
                                // This operation requires logic to compute
                                addDstToComputeSet = true;
                            }
                            else
                            {
                                // If an source operand required compute in this pipeline stage, then then mark the
                                // destination as requiring compute
                                for (const SourceOperand& srcOp : op._src)
                                {
                                    if (SourceOperandType::Register == srcOp.Type())
                                    {
                                        const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                                        if (computeSet.end() != computeSet.find(registerIndex))
                                        {
                                            addDstToComputeSet = true;
                                        }
                                    }
                                }
                            }

                            if (addDstToComputeSet)
                            {
                                for (const DestinationOperand& dstOp : op._dst)
                                {
                                    if (DestinationOperandType::Register == dstOp.Type())
                                    {
                                        computeSet.insert(dstOp.GetAccessedRegister()._registerIndex);
                                    }
                                }
                            }

                            if ((Opcode::Mov == op._opcode) && (SourceOperandType::Register == op._src[0].Type()) &&
                                (DestinationOperandType::Register == op._dst[0].Type()))
                            {
                                const size_t srcRegisterIndex = op._src[0].GetAccessedRegister()._registerIndex;
                                const size_t dstRegisterIndex = op._dst[0].GetAccessedRegister()._registerIndex;

                                const RegisterDescription& dstDesc = program._registerTable[dstRegisterIndex];

                                if ((RegisterType::Pipeline == dstDesc._type) &&
                                    (computeSet.end() == computeSet.find(srcRegisterIndex)))
                                {
                                    pipelineStageBits += dstDesc._width;

                                    functionBits += dstDesc._width;

                                    const std::string name =
                                        dstDesc._name.empty() ? std::string("Unnamed") : dstDesc._name;

                                    nameToSizeMap[name] += dstDesc._width;

                                    totalBits += dstDesc._width;
                                }
                            }
                        }
                    }

                    byStage << GetBasicBlockName(basicBlock) << "\t" << pipelineStageIndex << "\t" << pipelineStageBits
                            << "\n";
                }
            }

            byFunction << function._name << "\t" << functionBits << "\n";

            for (const auto p : nameToSizeMap)
            {
                byName << function._name << "\t" << p.first << "\t" << p.second << "\n";
            }
        }

        str << "\nLiveVariablePipelineRegisters (Total)\n";
        str << totalBits << "\n";

        str << byFunction.str() << "\n";
        str << byStage.str() << "\n";
        str << byName.str() << "\n";
    }

    // Operations
    str << separator << "\nOperations\nBasicBlock\tStage\tOperations";

    for (const Function& function : program._functions)
    {
        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            typedef std::map<std::string, size_t> OperationMap;

            std::map<size_t, OperationMap> atomicSequenceMap;

            for (const Stage& stage : basicBlock._stages)
            {
                OperationMap& operationMap = atomicSequenceMap[stage._atomicSequence];

                for (const Operation& op : stage._operations)
                {
                    const std::string opcodeName = GetOpcodeString(program, op);

                    const auto it = operationMap.find(opcodeName);
                    if (it == operationMap.end())
                    {
                        operationMap[opcodeName] = 1;
                    }
                    else
                    {
                        ++(it->second);
                    }
                }
            }

            // Get the set of all operation names
            std::set<std::string> allOperationNames;

            for (const auto& p1 : atomicSequenceMap)
            {
                const OperationMap& operationMap = p1.second;

                for (const auto& p2 : operationMap)
                {
                    const std::string& name = p2.first;

                    allOperationNames.insert(name);
                }
            }

            for (const auto& p1 : atomicSequenceMap)
            {
                const size_t stageIndex = p1.first;

                str << GetBasicBlockName(basicBlock) << "\t" << stageIndex << "\t";

                const OperationMap& operationMap = p1.second;

                for (const std::string& name : allOperationNames)
                {
                    const auto it = operationMap.find(name);

                    const size_t count = (it == operationMap.end()) ? 0 : it->second;

                    str << name << "\t" << count << "\t";
                }

                str << "\n";
            }
        }
    }
}

void AddResourceUsageRecord(JsonValue& data, // list of records to append to
                            JsonValue& path, // path (object/function/basicblock/stage)
                            const size_t fileIndex, const size_t beginLineIn, const size_t endLine,
                            const uint64_t registers, const uint64_t lutRamBits, const uint64_t lutRamUsage,
                            const uint64_t bRamBits, const uint64_t bRamUsage, const uint64_t deepRamBits,
                            const uint64_t deepRamUsage)
{
    std::string fileName = g_compiler->GetSourceFileName(fileIndex);
    std::replace(fileName.begin(), fileName.end(), '\\', '/');

    const size_t beginLine = (beginLineIn == endLine) && (beginLineIn != 0) ? beginLineIn - 1 : beginLineIn;

    JsonValue record = JsonValue::CreateArray();
    record.PushBack(path.Move());
    record.PushBack(JsonValue(fileName));
    record.PushBack(JsonValue(beginLine));
    record.PushBack(JsonValue(endLine));
    record.PushBack(JsonValue(registers));
    record.PushBack(JsonValue(lutRamBits));
    record.PushBack(JsonValue(lutRamUsage));
    record.PushBack(JsonValue(bRamBits));
    record.PushBack(JsonValue(bRamUsage));
    record.PushBack(JsonValue(deepRamBits));
    record.PushBack(JsonValue(deepRamUsage));

    data.PushBack(record.Move());
}

static JsonValue GetPathNode(const char* name, const boost::optional<const char*> type)
{
    JsonValue node = JsonValue::CreateObject();
    node.AddMember("name", JsonValue(name));
    node.AddMember("type", type ? JsonValue(type.get()) : JsonValue());
    return node;
}

static JsonValue GetScopePath(const SourceContainer* container, const JsonValue& globalObject)
{
    JsonValue path = JsonValue::CreateArray();
    for (; container != nullptr; container = container->_parent)
    {
        JsonValue object = GetPathNode(container->_instanceName.c_str(), container->_containerName.c_str());
        path.PushBack(object.Move());
    }

    if (path.AsArray().size() == 0)
    {
        JsonValue object = globalObject;
        path.PushBack(object.Move());
    }
    else
    {
        std::reverse(path.AsArray().begin(), path.AsArray().end());
    }

    return path;
}

static JsonValue GetFunctionPath(const Program& program, const Function& function, const JsonValue& globalObject)
{
    auto it = program._objectNameToContainers.find(function._objectName);

    JsonValue funcPath = GetScopePath((it != program._objectNameToContainers.end()) ? it->second : nullptr, globalObject);
    funcPath.PushBack(GetPathNode(FixupString(function._name).c_str(), "function").Move());

    return funcPath;
}

static void AddFifoRecord(const Program& program, const JsonValue& path, const size_t fifoIndex,
                          const std::string& fifoName, const std::vector<std::pair<std::string, size_t>>& sources,
                          const Location& location, JsonValue& root)
{
    const RegisterDescription& regDesc = program._registerTable[fifoIndex];
    assert(RegisterType::Fifo == regDesc._type);

    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();
    const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();

    const auto width = GetPhysicalFifoWidth(regDesc);
    const size_t depth = regDesc.Fifo()._depth;
    const size_t replicaCount = 1;
    if (width == 0)
        return;

    // Passthrough fifos do not consume resources
    if (FifoType::Passthrough == regDesc.Fifo()._type)
        return;
    const bool passThroughRegistered = (FifoType::PassthroughRegistered == regDesc.Fifo()._type) ||
                                       (FifoType::PassthroughUnregistered == regDesc.Fifo()._type);
    const char* fifoType = (passThroughRegistered ? "passthrough" : "fifo");
    const bool useLutRam = ShouldFifoUseLutRam(program, fifoIndex);
    const uint64_t lutRamUsage =
        useLutRam ? GetMemoryResourceUsage(deviceConfig._memory._lutRamConfigs, width, depth, replicaCount).first : 0;
    const uint64_t blockRamUsage =
        !useLutRam ? GetMemoryResourceUsage(deviceConfig._memory._blockRamConfigs, width, depth, replicaCount).first
                   : 0;

    JsonValue fifoPath = path;
    fifoPath.PushBack(GetPathNode(fifoName.c_str(), fifoType).Move());

    std::string fileName = codeGenConfig._fileNames[location._fileIndex];
    std::replace(fileName.begin(), fileName.end(), '\\', '/');

    // If the caller doesn't provide the breakdown of the source, the FIFO is shown as a whole.
    if (sources.empty())
    {
        JsonValue record = JsonValue::CreateArray();
        record.PushBack(fifoPath.Move());
        record.PushBack(JsonValue(fileName));
        record.PushBack(JsonValue(static_cast<uint64_t>(location._beginLine)));
        record.PushBack(JsonValue(static_cast<uint64_t>(location._endLine)));

        const size_t size = width * depth * replicaCount;

        const uint64_t registers = (passThroughRegistered) ? width : 0;
        const uint64_t lutRamBits = (passThroughRegistered || !useLutRam) ? 0 : size;
        const uint64_t blockRamBits = (passThroughRegistered || useLutRam) ? 0 : size;
        const uint64_t deepRamBits = 0;
        const uint64_t deepRamUsage = 0;
        record.PushBack(JsonValue(registers));
        record.PushBack(JsonValue(lutRamBits));
        record.PushBack(JsonValue(lutRamUsage));
        record.PushBack(JsonValue(blockRamBits));
        record.PushBack(JsonValue(blockRamUsage));
        record.PushBack(JsonValue(deepRamBits));
        record.PushBack(JsonValue(deepRamUsage));

        root.PushBack(record.Move());
    }
    else
    {
        auto addWidth = [](size_t a, const std::pair<std::string, size_t>& source) { return a + source.second; };
        // assert(std::accumulate(sources.begin(), sources.end(), size_t(0), addWidth) == width);

        for (const auto& source : sources)
        {
            JsonValue sourcePath = fifoPath;
            sourcePath.PushBack(GetPathNode(source.first.c_str(), fifoType).Move());

            JsonValue record = JsonValue::CreateArray();
            record.PushBack(sourcePath.Move());
            record.PushBack(JsonValue(fileName));
            record.PushBack(JsonValue(static_cast<uint64_t>(location._beginLine)));
            record.PushBack(JsonValue(static_cast<uint64_t>(location._endLine)));

            const size_t size = source.second * depth * replicaCount;
            const uint64_t registers = (passThroughRegistered) ? source.second : 0;
            const uint64_t lutRamBits = (passThroughRegistered || !useLutRam) ? 0 : size;
            const uint64_t blockRamBits = (passThroughRegistered || useLutRam) ? 0 : size;
            const uint64_t deepRamBits = 0;
            const uint64_t deepRamUsage = 0;
            // Approximate contribution of this source to the whole FIFO
            const float fraction = source.second / float(width);
            record.PushBack(JsonValue(registers));
            record.PushBack(JsonValue(lutRamBits));
            record.PushBack(JsonValue(static_cast<uint64_t>(lutRamUsage * fraction)));
            record.PushBack(JsonValue(blockRamBits));
            record.PushBack(JsonValue(static_cast<uint64_t>(blockRamUsage * fraction)));
            record.PushBack(JsonValue(deepRamBits));
            record.PushBack(JsonValue(static_cast<uint64_t>(deepRamUsage * fraction)));

            root.PushBack(record.Move());
        }
    }
}

static std::string ToString(const uint16_t width, const size_t index)
{
    std::ostringstream oss;
    oss << std::setw(width) << std::setfill('0') << index;
    return oss.str();
}

void WriteResourceReport(const std::string& fileName, const Program& program)
{
    const CodeGenConfig& codeGenConfig = GetCodeGenConfig();
    const CodeGenDeviceConfig& deviceConfig = GetCodeGenDeviceConfig();

    // Create the JSON document structure using JsonValue instead of parsing a template string
    JsonValue document = JsonValue::CreateObject();

    // Create availableResources array
    JsonValue availableResources = JsonValue::CreateArray();

    auto addResource = [&](const std::string& name, const std::string& description, const std::string& unit) {
        JsonValue resource = JsonValue::CreateObject();
        resource.AddMember("name", JsonValue(name));
        resource.AddMember("description", JsonValue(description));
        resource.AddMember("unitOfMeasurement", JsonValue(unit));
        availableResources.PushBack(resource);
    };

    addResource("Register", "FPGA Register", "registers");
    addResource("LUTRAM bits", "Lookup table RAM", "bits");
    addResource("LUTRAM resources", "Lookup table RAM", "resources");
    addResource("BRAM bits", "Block RAM", "bits");
    addResource("BRAM resources", "Block RAM", "resources");
    addResource("DeepRAM bits", "Deep RAM", "bits");
    addResource("DeepRAM resources", "Deep RAM", "resources");

    document.AddMember("availableResources", availableResources);

    // Create projections array
    JsonValue projections = JsonValue::CreateArray();

    // First projection
    JsonValue projection1 = JsonValue::CreateObject();
    projection1.AddMember("pathRule", JsonValue("path"));
    JsonValue includeColumns1 = JsonValue::CreateArray();
    includeColumns1.PushBack(JsonValue("Register"));
    includeColumns1.PushBack(JsonValue("LUTRAM bits"));
    includeColumns1.PushBack(JsonValue("LUTRAM resources"));
    includeColumns1.PushBack(JsonValue("BRAM bits"));
    includeColumns1.PushBack(JsonValue("BRAM resources"));
    includeColumns1.PushBack(JsonValue("DeepRAM bits"));
    includeColumns1.PushBack(JsonValue("DeepRAM resources"));
    projection1.AddMember("includeColumns", includeColumns1);
    projection1.AddMember("name", JsonValue("By Variable"));
    JsonValue aggregationRules1 = JsonValue::CreateObject();
    aggregationRules1.AddMember("Register", JsonValue("sum"));
    aggregationRules1.AddMember("LUTRAM bits", JsonValue("sum"));
    aggregationRules1.AddMember("LUTRAM resources", JsonValue("sum"));
    aggregationRules1.AddMember("BRAM bits", JsonValue("sum"));
    aggregationRules1.AddMember("BRAM resources", JsonValue("sum"));
    aggregationRules1.AddMember("DeepRAM bits", JsonValue("sum"));
    aggregationRules1.AddMember("DeepRAM resources", JsonValue("sum"));
    projection1.AddMember("aggregationRules", aggregationRules1);
    projections.PushBack(projection1);

    // Second projection
    JsonValue projection2 = JsonValue::CreateObject();
    projection2.AddMember("pathRule", JsonValue("path"));
    projection2.AddMember("name", JsonValue("By Queue"));
    projections.PushBack(projection2);

    document.AddMember("projections", projections);

    // Create data array with schema header
    JsonValue data = JsonValue::CreateArray();
    JsonValue schema = JsonValue::CreateArray();
    schema.PushBack(JsonValue("path"));
    schema.PushBack(JsonValue("relativePath"));
    schema.PushBack(JsonValue("rangeStartLine"));
    schema.PushBack(JsonValue("rangeEndLine"));
    schema.PushBack(JsonValue("Register"));
    schema.PushBack(JsonValue("LUTRAM bits"));
    schema.PushBack(JsonValue("LUTRAM resources"));
    schema.PushBack(JsonValue("BRAM bits"));
    schema.PushBack(JsonValue("BRAM resources"));
    schema.PushBack(JsonValue("DeepRAM bits"));
    schema.PushBack(JsonValue("DeepRAM resources"));
    data.PushBack(schema);
    document.AddMember("data", data);

    JsonValue globalObject = GetPathNode(g_globalObjectName.c_str(), g_globalClassName.c_str());

    JsonValue& dataArray = document.AsObject().at("data");
    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];

        if (RegisterType::Memory == regDesc._type)
        {
            const bool hasInitialData = !regDesc.Memory()._initialValues.empty();

            const size_t replicaCount = regDesc.Memory().ReplicaCount();
            const uint16_t replicaIndexWidth = (replicaCount <= 1) ? 1 : uint16_t(ceil(log10(replicaCount)));

            JsonValue memoryPath;

            if (regDesc._function)
            {
                // The memory is associated with a particular function (global data propagation)
                memoryPath = GetFunctionPath(program, *regDesc._function, globalObject);
            }
            else
            {
                memoryPath = GetScopePath(regDesc._sourceVariable._container, globalObject);
            }

            const std::string memoryType = "memory";
            boost::optional<const char*> type =
                (replicaCount == 1) ? boost::optional<const char*>(memoryType.c_str()) : boost::optional<const char*>();
            memoryPath.PushBack(GetPathNode(regDesc._name.c_str(), type).Move());

            for (size_t i = 0; i < replicaCount; ++i)
            {
                const auto width = regDesc.Memory()._elementWidth;
                const auto depth = regDesc.Memory()._elementCount;
                const auto readPortCount = regDesc.Memory()._readPortCount;
                const size_t size = width * depth;
                // ROMs can be implemented using Block RAMs with true dual port mode
                const bool ROM = (regDesc.Memory()._writePortCount == 0);

                JsonValue path;
                if (replicaCount == 1)
                {
                    path = memoryPath;
                }
                else
                {
                    path = memoryPath;
                    path.PushBack(GetPathNode(ToString(replicaIndexWidth, i).c_str(), "memory").Move());
                }

                const uint64_t registerSize = ShouldUseRamType(regDesc, RAM_TYPE::LOGIC) ? size : 0;
                const uint64_t lutRamBits = ShouldUseRamType(regDesc, RAM_TYPE::LUT) ? size : 0;
                const uint64_t lutRamUsage = ShouldUseRamType(regDesc, RAM_TYPE::LUT)
                                                 ? GetMemoryResourceUsage(deviceConfig._memory._lutRamConfigs, width,
                                                                          depth, 1 /* replicaCount */)
                                                       .first
                                                 : 0;
                const uint64_t bRamBits = ShouldUseRamType(regDesc, RAM_TYPE::BLOCK) ? size : 0;
                const uint64_t bRamUsage =
                    static_cast<uint64_t>(ShouldUseRamType(regDesc, RAM_TYPE::BLOCK)
                                              ? GetMemoryResourceUsage(deviceConfig._memory._blockRamConfigs, width,
                                                                       depth, 1 /* replicaCount */)
                                                        .first /
                                                    (ROM ? 2.0 : 1.0)
                                              : 0);
                const uint64_t deepRamBits = ShouldUseRamType(regDesc, RAM_TYPE::DEEP) ? size : 0;
                const uint64_t deepRamUsage = ShouldUseRamType(regDesc, RAM_TYPE::DEEP)
                                                  ? GetMemoryResourceUsage(deviceConfig._memory._deepRamConfigs, width,
                                                                           depth, 1 /* replicaCount */)
                                                        .first
                                                  : 0;

                const SourceLocation& location = regDesc._sourceVariable._declaredLocation;

                AddResourceUsageRecord(data, path, location._fileIndex, location._lineNumber,
                                       location._lineNumber, registerSize, lutRamBits, lutRamUsage, bRamBits, bRamUsage,
                                       deepRamBits, deepRamUsage);
            }
        }
    }

    std::set<size_t> fifoMergerSources;
    for (const FIFOMerger merger : program._fifoMergers)
    {
        for (const size_t source : merger._sources)
        {
            fifoMergerSources.insert(source);
        }
    }

    for (const Function& function : program._functions)
    {
        JsonValue funcPath = GetFunctionPath(program, function, globalObject);

        for (const BasicBlock& basicBlock : function._basicBlocks)
        {
            const std::string bbName = std::to_string(basicBlock._inputFifoIndices[0]);

            JsonValue bbPath = funcPath;
            bbPath.PushBack(GetPathNode(bbName.c_str(), "basic block").Move());

            for (size_t i = 0; i < basicBlock._inputFifoCount; i++)
            {
                const size_t inputFifoIndex = basicBlock._inputFifoIndices[i];

                const RegisterDescription& regDesc = program._registerTable[inputFifoIndex];

                if (RegisterType::Fifo == regDesc._type)
                {
                    const std::string fifoName =
                        (i != 0) ? "BackwardLink" : ((&basicBlock == function._start) ? "Entry" : "Internal");
                    std::vector<std::pair<std::string, size_t>> sources;
                    AddFifoRecord(program, bbPath, inputFifoIndex, fifoName, sources, basicBlock._location, dataArray);
                }
            }

            const std::vector<PipelineStage> pipelineStages = GetPipelineStages(const_cast<BasicBlock&>(basicBlock));

            const uint16_t stageIndexWidth =
                (pipelineStages.size() <= 1) ? 1 : uint16_t(ceil(log10(pipelineStages.size())));

            for (size_t pipelineStageIndex = 0; pipelineStageIndex < pipelineStages.size(); ++pipelineStageIndex)
            {
                // Maps register name to number of pipeline register bits
                std::map<std::string, size_t> registerNameToBits;

                JsonValue stage = GetPathNode(ToString(stageIndexWidth, pipelineStageIndex).c_str(), "stage");

                JsonValue stagePath = bbPath;
                stagePath.PushBack(stage.Move());

                for (const Stage* const stage : pipelineStages[pipelineStageIndex])
                {
                    std::map<size_t, std::vector<std::pair<std::string, size_t>>> fifoSources;
                    for (const Operation& op : stage->_operations)
                    {
                        for (const DestinationOperand& dst : op._dst)
                        {
                            if (DestinationOperandType::Register == dst.Type())
                            {
                                const size_t registerIndex = dst.GetAccessedRegister()._registerIndex;

                                const RegisterDescription& regDesc = program._registerTable[registerIndex];

                                if (RegisterType::Pipeline == regDesc._type)
                                {
                                    registerNameToBits[regDesc._name] += regDesc._width;
                                }
                            }
                        }

                        if (op._opcode == Opcode::Mov && op._dst.size() == 1 &&
                            op._dst[0].Type() == DestinationOperandType::Fifo && op._src.size() == 1)
                        {
                            const FifoSubset& subset = op._dst[0].GetFifoSubset();
                            if (op._src[0].Type() == SourceOperandType::Literal)
                            {
                                fifoSources[subset._registerIndex].emplace_back(
                                    boost::to_string(op._src[0].GetLiteral()._value), subset._width);
                            }
                            else
                            {
                                assert(op._src[0].Type() == SourceOperandType::Register);
                                const RegisterDescription& regDesc =
                                    program._registerTable[op._src[0].GetAccessedRegister()._registerIndex];
                                fifoSources[subset._registerIndex].emplace_back(regDesc._name, subset._width);
                            }
                        }

                        // sources of FIFO mergers
                        decltype(fifoMergerSources)::iterator it;
                        if (op._getSuccessorBlock &&
                            (it = fifoMergerSources.find(op._flags._enqueue._successorFifo)) != fifoMergerSources.end())
                        {
                            fifoMergerSources.erase(it);
                            const size_t fifoIndex = op._flags._enqueue._successorFifo;
                            const RegisterDescription& outputRegDesc = program._registerTable[fifoIndex];
                            assert(RegisterType::Fifo == outputRegDesc._type);
                            const BasicBlock* const dstBlock = op._getSuccessorBlock();

                            const std::string fifoName = std::string("to ") + dstBlock->_function->_name + " " +
                                                         std::to_string(dstBlock->_inputFifoIndices[0]);

                            AddFifoRecord(program, stagePath, fifoIndex, fifoName, fifoSources[fifoIndex],
                                          basicBlock._location, dataArray);
                        }
                        // data propagation FIFOs
                        else if (op._opcode == Opcode::EnqueueRegisters)
                        {
                            const size_t fifoIndex = op._flags._enqueue._successorFifo;
                            const RegisterDescription& outputRegDesc = program._registerTable[fifoIndex];
                            assert(RegisterType::Fifo == outputRegDesc._type);

                            const std::string fifoName = std::string("propagation ") + " " + std::to_string(fifoIndex);

                            std::vector<std::pair<std::string, size_t>> sources(op._src.size());
                            std::transform(op._src.begin(), op._src.end(), sources.begin(),
                                           [&](const SourceOperand& source)
                                           {
                                               assert(source.Type() == SourceOperandType::Register);
                                               const RegisterDescription& regDesc =
                                                   program._registerTable[source.GetAccessedRegister()._registerIndex];
                                               return std::make_pair(regDesc._name, regDesc._width);
                                           });
                            AddFifoRecord(program, stagePath, fifoIndex, fifoName, sources, basicBlock._location,
                                          dataArray);
                        }
                    }
                }

                // Emit 1 entry per register name
                for (const auto& p : registerNameToBits)
                {
                    const std::string& regName = p.first;
                    const size_t bitCount = p.second;

                    JsonValue reg = GetPathNode(regName.c_str(), "variable");

                    JsonValue regPath = stagePath;
                    regPath.PushBack(reg);

                    AddResourceUsageRecord(dataArray, regPath, basicBlock._location._fileIndex,
                                           basicBlock._location._beginLine, basicBlock._location._endLine, bitCount, 0,
                                           0, 0, 0, 0, 0);
                }
            }
        }
    }

    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        const BasicBlock* beforeBlock = contextSaver._beforeCall;
        const BasicBlock* afterBlock = contextSaver._afterCall;
        struct
        {
            const BasicBlock* _block;
            const size_t _fifoIndex;
            const std::string _name;
        } fifos[] = {{beforeBlock, contextSaver._fromCallerFifoIndex,
                      std::string("local variables ") + std::to_string(afterBlock->_inputFifoIndices[0])},
                     {afterBlock, contextSaver._fromCalleeFifoIndex,
                      std::string("return values ") + std::to_string(beforeBlock->_inputFifoIndices[0])}};

        for (const auto fifo : fifos)
        {
            const Function* function = fifo._block->_function;

            auto it = program._objectNameToContainers.find(function->_objectName);

            JsonValue funcPath = GetScopePath((it != program._objectNameToContainers.end()) ? it->second : nullptr, globalObject);
            funcPath.PushBack(GetPathNode(FixupString(function->_name).c_str(), "function").Move());

            const std::string bbName = std::to_string(fifo._block->_inputFifoIndices[0]);
            JsonValue bbPath = funcPath;
            bbPath.PushBack(GetPathNode(bbName.c_str(), "basic block").Move());

            std::vector<std::pair<std::string, size_t>> sources;
            AddFifoRecord(program, bbPath, fifo._fifoIndex, fifo._name, sources, fifo._block->_location, dataArray);
        }
    }

    assert(fifoMergerSources.size() == 0);

    std::ofstream os;
    OpenOutputFileStream(os, fileName.c_str(), false);
    std::string jsonString = document.ToString(true);
    os.write(jsonString.c_str(), jsonString.length());
}

void DumpMemoryMetadata(const char* const memoryMetadataFileNameBase, const Program& program)
{
    std::ostringstream asrFileNameStr;
    asrFileNameStr << memoryMetadataFileNameBase << ".asr";
    std::string asrFileName = asrFileNameStr.str();
    std::ofstream asrFile;
    OpenOutputFileStream(asrFile, asrFileName.c_str());

    std::ostringstream romFileNameStr;
    romFileNameStr << memoryMetadataFileNameBase << ".rom";
    std::string romFileName = romFileNameStr.str();
    std::ofstream romFile;
    OpenOutputFileStream(romFile, romFileName.c_str());

    DumpSyncRamAndROM(&asrFile, &romFile, program, memoryMetadataFileNameBase);

    asrFile.close();
    romFile.close();

    std::ostringstream scFifoFileNameStr;
    scFifoFileNameStr << memoryMetadataFileNameBase << ".scf";
    std::string scFifoFileName = scFifoFileNameStr.str();
    std::ofstream scFifoFile;
    OpenOutputFileStream(scFifoFile, scFifoFileName.c_str());

    std::ostringstream dcFifoFileNameStr;
    dcFifoFileNameStr << memoryMetadataFileNameBase << ".dcf";
    std::string dcFifoFileName = dcFifoFileNameStr.str();
    std::ofstream dcFifoFile;
    OpenOutputFileStream(dcFifoFile, dcFifoFileName.c_str());

    DumpFifoRamInfo(&scFifoFile, &dcFifoFile, program);

    scFifoFile.close();
}

// This function writes out the metadata for all instatiated syncram (which may actually be RAM or ROM).
// To accurately get the name of the ROM initialization files, we have to match whatever is done in
// VerilogCompiler::WriteMemoryContentFile()
void DumpSyncRamAndROM(std::ofstream* asrFile, std::ofstream* romFile, const Program& program,
                       const char* const memFileBase)
{
    // ram are currently only instantiated in KanagawaHALSimpleDualPortRAM and KanagawaHALDualClockDualPortRAM
    //      KanagawaHALSimpleDualPortRAM defines KanagawaHALDualPortRAM.
    //          KanagawaHALDualPortRAM is currently only instatiated in KanagawaSyncRam.
    //              KanagawaSyncRam are directly instantiated in generated code (1) and instantiated in
    //              KanagawaSimpleDualPortRAM
    //                  KanagawaSimpleDualPortRAM are instantiated in ContextSaver(2), KanagawaReorderMemory, and
    //                  ReorderBuffer (3).
    //                      ContextSaver are directly instantiated in generated code
    //                      KanagawaReorderMemory are only instantiated in ReorderBuffer
    //                      ReorderBuffer are directly instantiated in generated code and contain both:
    //                          a) KanagawaSimpleDualPortRAM with depth=2^LOG_DEPTH and width=DATA_WIDTH and
    //                          b) KanagawaReorderMemory with depth=2^LOG_DEPTH and width=1
    //      KanagawaHALDualClockDualPortRAM are only currently instantiated in KanagawaPacketFifo.
    //          KanagawaPacketFifo are only currently instantiated in KanagawaBedrockRole.
    //          so we will ignore syncram that come from instantations of KanagawaHALDualClockDualPortRAM.

    // (1) Account for all directly instantiated KanagawaSyncRam
    // Each KanagawaSyncRam contains a KanagawaHALDualPortRAM withdepth=2^ADDR_WIDTH and width=DATA_WIDTH.
    // Some of these may actually be ROMs with initial data.
    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];

        if (regDesc._type == RegisterType::Memory)
        {
            const bool hasInitialData = !regDesc.Memory()._initialValues.empty();
            const size_t writePorts = regDesc.Memory()._writePortCount;

            const size_t depth = 1ull << regDesc.GetMemoryAddressWidth();

            const MemoryInitFileType fileType = GetCodeGenDeviceConfig()._memoryInitFileType;

            std::string memContentFileName;

            if (hasInitialData)
            {
                std::ostringstream fileNameStr;
                const char* const extension = (fileType == MemoryInitFileType::Mif) ? "mif" : "mem";

                fileNameStr << memFileBase << i << FixupString(regDesc._name) << "." << extension;

                memContentFileName = fileNameStr.str();
            }

            if ((writePorts == 0) && memContentFileName.empty())
            {
                continue;
            }

            // Don't bother with zero-width memories
            if (regDesc.Memory()._elementWidth == 0)
            {
                continue;
            }

            if (hasInitialData)
            {
                // Write out the depth, width, and initialization file of the memory
                *romFile << depth << "\t" << regDesc.Memory()._elementWidth << "\t" << memContentFileName << "\n";
            }
            else
            {
                // Write out the depth and width of the memory
                *asrFile << depth << "\t" << regDesc.Memory()._elementWidth << "\n";
            }
        }
    }

    // (2) Account for all instantiated ContextSaver.
    // Each ContextSaver contains a KanagawaSimpleDualPortRAM with depth=DEPTH and width=CALLER_WIDTH.
    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        const RegisterDescription callerRegDesc = program._registerTable[contextSaver._fromCallerFifoIndex];
        if (!(contextSaver._isOrdered) && callerRegDesc.Fifo()._depth != 0)
        {
            // Don't bother with zero-width memories
            if (GetClampedRegisterWidth(callerRegDesc._width) == 0)
            {
                continue;
            }

            const size_t depth = callerRegDesc.Fifo()._depth;
            *asrFile << depth << "\t" << GetClampedRegisterWidth(callerRegDesc._width) << "\n";
        }
    }

    // (3) Account for all instantiated ReorderBuffer
    // Each ReorderBuffer contains:
    //      a) KanagawaSimpleDualPortRAM with depth=2^LOG_DEPTH and width=WIDTH
    //      b) KanagawaReorderMemory with depth=2^LOG_DEPTH and width=1
    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];
        if (regDesc._type == RegisterType::Fifo)
        {
            const size_t physicalFifoWidth = GetPhysicalFifoWidth(regDesc);
            const size_t clampedPhysicalFifoWidth = GetClampedRegisterWidth(physicalFifoWidth);

            if (FifoType::ReorderBuffer == regDesc.Fifo()._type)
            {

                size_t widthParameter = clampedPhysicalFifoWidth;
                const size_t depth = regDesc.Fifo()._depth;

                // Don't bother with zero-width memories
                if (widthParameter == 0)
                {
                    continue;
                }

                *asrFile << depth << "\t" << widthParameter << "\n";
                *asrFile << depth << "\t1\n";
            }
        }
    }
}

// This function writes out the metadata for all instatiated scfifo and dcfifo
void DumpFifoRamInfo(std::ofstream* scfFile, std::ofstream* dcfFile, const Program& program)
{
    // dcfifo are currently only instantiated in KanagawaHALDualClockFifo and conditionally in
    // KanagawaHALReadyValidFifo (if it is dual clock)
    //      KanagawaHALDualClockFifo are directly instantiated in generated code (1), KanagawaDualClockDramFifo, and
    //      KanagawaHALReadyValidFifo are instantiated in KanagawaExternReturnRouter
    //          KanagawaExternReturnRouter are directly instantiated in generated code (2)

    // (1) Account for all directly instantiated KanagawaHALDualClockFifo
    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];
        if (regDesc._type == RegisterType::Fifo)
        {
            const size_t physicalFifoWidth = GetPhysicalFifoWidth(regDesc);
            const size_t clampedPhysicalFifoWidth = GetClampedRegisterWidth(physicalFifoWidth);

            if ((FifoType::Default == regDesc.Fifo()._type) || (FifoType::ReorderBuffer == regDesc.Fifo()._type))
            {
                const bool isReorderBuffer = (FifoType::ReorderBuffer == regDesc.Fifo()._type);
                const bool isDualClock = (regDesc.Fifo()._readClock != regDesc.Fifo()._writeClock);

                size_t widthParameter = clampedPhysicalFifoWidth;

                const size_t depth = regDesc.Fifo()._depth;

                if (isReorderBuffer)
                {
                }
                else if (regDesc.Fifo()._implementation == FifoImplementation::TwoRegister)
                {
                }
                else if (regDesc.Fifo()._implementation == FifoImplementation::InternalBuffer)
                {
                }
                else if (isDualClock)
                {
                    // (1) Each KanagawaHALDualClockFifo contains one dcfifo with depth=2^LOG_DEPTH and width=WIDTH
                    *dcfFile << depth << "\t" << widthParameter << "\n";
                }
                else if (regDesc.Fifo()._implementation == FifoImplementation::CrossRegion)
                {
                }
                else
                {
                }
            }
        }
    }

    // (1a) Testbench.sv contains N KanagawaAmmDramChannel, all of which are the same.  Each KanagawaAmmDramChannel
    // contain two KanagawaDualClockDramFifo.
    // The first has depth=2^9 (currently hardcoded) and width=$bits(dram_request_t).
    // The size of dram_request_t depends on the target board.
    // For Bedrock, CelestialPeak, and SiOS, this is (1 + 1 + 26 + 30 + 512) = 570.
    // For StormPeak, this is (1 + 1 + 27 + 30 + 512) = 571 for the boards with 8GB modules and (1 + 1 + 28 + 30 + 512)
    // = 572 for the boards with 16GB modules. The precompile.do file for ExecutionTest targets Bedrock, so we will use
    // 570 The second has depth=2^9 (currently hardcoded) and width=512. Each KanagawaDualClockDramFifo contains a
    // dcfifo.
    *dcfFile << "512\t570\n";
    *dcfFile << "512\t512\n";

    // (2 and 3 from the scfifo section below) Account for all directly instantiated KanagawaExternReturnRouter,
    // which contains a KanagawaHALReadyValidFifo.
    // Each KanagawaHALReadyValidFifo will have either an scfifo or dcfifo (depending on whether or not it is
    // DUAL_CLOCK) with width=$bits(input_index_intf.data) and depth=2**LOG_DEPTH.
    for (const Function* const function : program._externFunctions)
    {
        if (!function->IsAsync())
        {
            const size_t writeClockIndex = 0;
            const size_t readClockIndex = 0;
            const bool isDualClock = (writeClockIndex != readClockIndex);
            const size_t logDepth = function->_syncExtern.GetLogDepth();
            const size_t depth = 1ull << logDepth;
            const size_t callSiteIndexWidth = function->_syncExtern._callIndexWidth;

            if (!isDualClock)
            {
                // (3)
                *scfFile << depth << "\t" << callSiteIndexWidth << "\n";
            }
            else
            {
                // (2)
                *dcfFile << depth << "\t" << callSiteIndexWidth << "\n";
            }
        }
    }

    // scfifo are currently only instantiated conditionally in KanagawaHALReadyValidFifo (if it is not dual clock)
    // and in KanagawaHALShowAheadFifo
    //      KanagawaHALReadyValidFifo are instantiated in KanagawaExternReturnRouter
    //          KanagawaExternReturnRouter are directly instantiated in generated code (3, taken care of above)
    //      KanagawaHALShowAheadFifo are instantiated in KanagawaWriteDelayFifo
    //          KanagawaWriteDelayFifo are directly instantiated in generated code (5), and instantiated in
    //          ContextSaverOrdered, KanagawaCrossRegionFifo, KanagawaInternalBufferFifo, and conditionally in
    //              ContextSaverOrdered are directly instantiated in generated code (6)
    //              KanagawaCrossRegionFifo are directly instantiated in generated code (7)
    //                  Each KanagawaCrossRegionFifo contains a KanagawaWriteDelayFifo with depth=2^LOG_DEPTH and
    //                  width=WIDTH.
    //              KanagawaInternalBufferFifo are directly instantiated in generated code (8) and in
    //              Each KanagawaWriteDelayFifo conditionally contains a KanagawaHALShowAheadFifo (if the width > 0).
    //                  Each KanagawaHALShowAheadFifo contains one scfifo.

    // Account for all directly instantiated KanagawaWriteDelayFifo (5), KanagawaCrossRegionFifo (7),
    // and KanagawaInternalBufferFifo (8)
    for (size_t i = 0; i < program._registerTable.size(); ++i)
    {
        const RegisterDescription& regDesc = program._registerTable[i];
        if (regDesc._type == RegisterType::Fifo)
        {
            const size_t physicalFifoWidth = GetPhysicalFifoWidth(regDesc);
            const size_t clampedPhysicalFifoWidth = GetClampedRegisterWidth(physicalFifoWidth);

            if ((FifoType::Default == regDesc.Fifo()._type) || (FifoType::ReorderBuffer == regDesc.Fifo()._type))
            {
                const bool isReorderBuffer = (FifoType::ReorderBuffer == regDesc.Fifo()._type);
                const bool isDualClock = (regDesc.Fifo()._readClock != regDesc.Fifo()._writeClock);

                size_t widthParameter = clampedPhysicalFifoWidth;

                const size_t depth = regDesc.Fifo()._depth;

                if (isReorderBuffer)
                {
                }
                else if (regDesc.Fifo()._implementation == FifoImplementation::TwoRegister)
                {
                }
                else if (regDesc.Fifo()._implementation == FifoImplementation::InternalBuffer)
                {
                    // (8) Each KanagawaInternalBufferFifo contains a KanagawaWriteDelayFifo with depth=2^LOG_DEPTH
                    // and width=WIDTH. Each KanagawaWriteDelayFifo conditionally contains a KanagawaHALShowAheadFifo.
                    // Each KanagawaHALShowAheadFifo contains one scfifo.
                    if (widthParameter != 0)
                    {
                        *scfFile << depth << "\t" << widthParameter << "\n";
                    }
                }
                else if (isDualClock)
                {
                }
                else if (regDesc.Fifo()._implementation == FifoImplementation::CrossRegion)
                {
                    // (7) Each KanagawaCrossRegionFifo contains a KanagawaWriteDelayFifo with depth=2^LOG_DEPTH and
                    // width=WIDTH. Each KanagawaWriteDelayFifo conditionally contains a KanagawaHALShowAheadFifo.
                    // Each KanagawaHALShowAheadFifo contains one scfifo.
                    if (physicalFifoWidth != 0)
                    {
                        *scfFile << depth << "\t" << widthParameter << "\n";
                    }
                }
                else
                {
                    // (5) Each KanagawaWriteDelayFifo conditionally contains a KanagawaHALShowAheadFifo.
                    // Each KanagawaHALShowAheadFifo contains one scfifo.
                    if (widthParameter != 0)
                    {
                        *scfFile << depth << "\t" << widthParameter << "\n";
                    }
                }
            }
        }
    }

    // Account for all directly instantiated ContextSaverOrdered.  If the ContextSaverOrdered is not LOG_DEPTH=0,
    // it will contain a KanagawaWriteDelayFifo (6) with depth=2^LOG_DEPTH and width=CALLER_WIDTH.
    // Each KanagawaWriteDelayFifo conditionally contains a KanagawaHALShowAheadFifo.
    // Each KanagawaHALShowAheadFifo contains one scfifo.
    // Each ContextSaverOrdered also contains a KanagawaInternalBufferFifo (9) with depth=2^LOG_DEPTH and
    // width=$bits(live_variable_control_fifo_data_t) (currently LOOP_COUNT_WIDTH)
    for (const ContextSaver& contextSaver : program._contextSavers)
    {
        const RegisterDescription callerRegDesc = program._registerTable[contextSaver._fromCallerFifoIndex];
        const bool isPipelined = contextSaver._isExternal
                                     ? false
                                     : (contextSaver._callee->GetModifiers() & ParseTreeFunctionModifierPipelined);
        const size_t loopCountWidth = contextSaver._loopCounterWidth;

        if (contextSaver._isOrdered && callerRegDesc.Fifo()._depth != 0 &&
            GetClampedRegisterWidth(callerRegDesc._width) != 0)
        {
            // (6)
            const size_t depth = callerRegDesc.Fifo()._depth;
            *scfFile << depth << "\t" << GetClampedRegisterWidth(callerRegDesc._width) << "\n";

            // (9)
            *scfFile << depth << "\t" << loopCountWidth << "\n";
        }
    }
}
