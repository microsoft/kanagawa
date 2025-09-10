// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

// first and last pipeline stage index
using PropagationRange = std::pair<size_t, size_t>;

// Maps PropagationRange to set of registers
// that are propagated through the pipeline
// with starting and ending at that range
using Propagations = std::map<PropagationRange, std::set<size_t>>;

// Maps register index to register width
using RegToWidth = std::function<size_t(const size_t)>;

// Anchor point and FIFO depth
using AnchorAndDepth = std::pair<size_t, size_t>;

// Maps pipeline stage index to set of registers
// that are stored into the FIFO at that stage
using DropPoints = std::map<size_t, std::set<size_t>>;

using FifoPropagations = std::map<AnchorAndDepth, DropPoints>;

void LocalDataPropagation(Program& program, BasicBlock& basicBlock, RegisterSetMap& liveInMap,
                          const std::unordered_set<const BasicBlock*>& returnSites);

FifoPropagations OptimizePropagations(const Propagations& propagations, const RegToWidth& regToWidth);