// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

void WriteResourceUsage(const char* const fileName, const Program& program);

void WriteResourceReport(const std::string& fileName, const Program& program);

void DumpMemoryMetadata(const char* const memoryMetadataFileNameBase, const Program& program);

void DumpSyncRamAndROM(std::ofstream* asrFile, std::ofstream* romFile, const Program& program,
                       const char* const memFileBase);

void DumpFifoRamInfo(std::ofstream* scfFile, std::ofstream* dcfFile, const Program& program);

size_t LogicRamCostFunction(size_t width, size_t depth, size_t numReadPorts);