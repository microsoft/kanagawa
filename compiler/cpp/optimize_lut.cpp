// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

// LUT-related IR optimizations

Lut InitializeLut(std::vector<LutEntry>& lutEntries)
{
    Lut result = {};

    result._numDestinationBits = lutEntries.size();

    result._lutEntries = lutEntries.data();

    // Zero-initialize - just to be safe
    memset(result._lutEntries, 0, sizeof(LutEntry) * lutEntries.size());

    return result;
}

Lut AllocateLut(const size_t numDstBits)
{
    // Allocate LutEntry array - which will be freed when the compiler object is freed
    std::vector<LutEntry>* lutEntries = g_compiler->Create<std::vector<LutEntry>>(numDstBits);

    return InitializeLut(*lutEntries);
}

// Deep copy of a Lut struct
Lut DeepCopyLut(const Lut& src, const AllocateLutFn& allocateLut)
{
    Lut result = allocateLut(src._numDestinationBits);

    std::copy(src._lutEntries, src._lutEntries + src._numDestinationBits, result._lutEntries);

    return result;
}

AllocateLutFn LutArenaAllocator::LutFn()
{
    const AllocateLutFn result = [this](const size_t numDstBits)
    {
        std::vector<LutEntry> lutEntries(numDstBits);

        _allocated.push_back(lutEntries);

        return InitializeLut(_allocated.back());
    };

    return result;
}

OperationList ReduceBinaryOpLut(Program& program, const AccessedRegister& srcReg, const uint64_t twoInputLut,
                                const DestinationOperand& dstOp)
{
    OperationList operationList;

    AccessedRegister currSrcReg = srcReg;

    // _smallLutSize is used rather than c_maxLutSources
    // to enable 1 FPGA resource (ALM, CLB) to do 2 reductions of size c_maxLutSources
    // rather than 1 reduction of size c_maxLutSources
    const size_t maxLutSources = GetCodeGenDeviceConfig()._smallLutSize;

    while (true)
    {
        const size_t srcWidth = program._registerTable[currSrcReg._registerIndex]._width;

        const std::string srcName = program._registerTable[currSrcReg._registerIndex]._name;

        const size_t dstWidth = (srcWidth + maxLutSources - 1) / maxLutSources;
        assert(dstWidth > 0);

        AccessedRegister destReg = {};

        if (dstWidth > 1)
        {
            // Allocate a local register to hold the reduction output
            destReg._registerIndex = AllocateRegister(&program, dstWidth, RegisterType::Local, srcName + "_reduce");
        }
        else
        {
            // This is the final pass, write results to output register
            destReg = dstOp.GetAccessedRegister();
        }

        Lut lut = AllocateLut(dstWidth);

        // Break the source into groups of width no greather than maxLutSources
        // Reduce each group into 1 output bit
        for (size_t dstBit = 0; dstBit < dstWidth; dstBit++)
        {
            const size_t startSrcBit = (dstBit * maxLutSources);
            const size_t endSrcBit = std::min<size_t>(startSrcBit + maxLutSources, srcWidth);
            const size_t numSrcBits = endSrcBit - startSrcBit;

            assert(startSrcBit < srcWidth);
            assert(endSrcBit <= srcWidth);
            assert(numSrcBits > 0);

            LutEntry& lutEntry = lut._lutEntries[dstBit];

            lutEntry._numSources = numSrcBits;

            for (size_t i = 0; i < numSrcBits; i++)
            {
                lutEntry._sourceIndices[i] = 0;
                lutEntry._sourceBit[i] = startSrcBit + i;
            }

            // Convert the input table "twoInputLut", which defines how to combine 2 src bits
            // Into a table that defines how to combine numSrcBits bits
            lutEntry._table = 0;

            const size_t tableSize = 1ull << numSrcBits;

            for (size_t tableIndex = 0; tableIndex < tableSize; tableIndex++)
            {
                // tableIndex contains 1 row of the lookup table
                uint64_t reducedBits = tableIndex;

                for (size_t i = 0; i < (numSrcBits - 1); i++)
                {
                    // Get to 2 least significant bits form reducedBits
                    const uint64_t indexIntoLut = reducedBits & 3;

                    // Look up the reduced value for those bits
                    const uint64_t output = (twoInputLut >> indexIntoLut) & 1;

                    // Replace that pair of values in reducedBits with the reduced result
                    reducedBits = ((reducedBits >> 1) & ~1) | output;
                }

                // The least significant bit of reducedBits contains the value in the larger lookup table
                lutEntry._table |= (reducedBits & 1) << tableIndex;
            }
        }

        Operation reduceOp = {};

        reduceOp._opcode = Opcode::Lut;

        reduceOp._flags._lut = lut;

        reduceOp._src.push_back(currSrcReg);
        reduceOp._dst.push_back(destReg);

        operationList.push_back(reduceOp);

        // This dest for this iteration becomes the source for the next
        currSrcReg = destReg;

        if (dstWidth == 1)
        {
            // The reduction is complete
            break;
        }
    }

    return operationList;
}

OperationList ReduceBinaryOpNoLut(Program& program, const AccessedRegister& srcReg,
                                  const ParseTreeBinaryOpType reduceOp, const DestinationOperand& dstOp)
{
    OperationList operationList;

    AccessedRegister currSrcReg = srcReg;

    // Decompose the source register into N 1-bit wide registers
    const size_t srcWidth = program._registerTable[currSrcReg._registerIndex]._width;

    std::vector<AccessedRegister> srcRegs(srcWidth);

    for (size_t i = 0; i < srcWidth; i++)
    {
        srcRegs[i] = {AllocateRegister(&program, 1, RegisterType::Local, "ReductionBit")};

        Operation op = {};

        op._opcode = Opcode::BinaryOp;
        op._flags._binaryOpType = ParseTreeBinaryOpTypeShr;
        op._dst.push_back(srcRegs[i]);

        op.PushOperand(srcReg, false);
        op.PushOperand(i, false);

        operationList.push_back(op);
    }

    while (srcRegs.size() > 1)
    {
        std::vector<AccessedRegister> dstRegs;

        // combine pairs
        const size_t numPairs = srcRegs.size() / 2;

        for (size_t i = 0; i < numPairs; i++)
        {
            const AccessedRegister dstReg = {AllocateRegister(&program, 1, RegisterType::Local, "ReductionBit")};
            dstRegs.push_back(dstReg);

            Operation op = {};

            op._opcode = Opcode::BinaryOp;
            op._flags._binaryOpType = reduceOp;
            op._dst.push_back(dstReg);

            op.PushOperand(srcRegs.at(i * 2 + 0), false);
            op.PushOperand(srcRegs.at(i * 2 + 1), false);

            operationList.push_back(op);
        }

        // If the size is odd, then one left over result will be carried to the next iteration
        if (1 == (srcRegs.size() % 2))
        {
            dstRegs.push_back(srcRegs.back());
        }

        srcRegs = dstRegs;
    }

    // Move result into dstOp
    {
        Operation op = {};

        op._opcode = Opcode::Mov;
        op._dst.push_back(dstOp);
        op.PushOperand(srcRegs[0], false);

        operationList.push_back(op);
    }

    return operationList;
}

// Produces operations to perform a reduction tree
// The reduction input is 1 register (of arbitrary width)
// The reduction output is 1 bit wide
OperationList ReduceBinaryOp(Program& program, const AccessedRegister& srcReg, const ParseTreeBinaryOpType reduceOp,
                             const DestinationOperand& dstOp, const Operation& srcLocationOp)
{
    OperationList result;

    {
        SetOperationLocation sol(result, srcLocationOp);

        // Implementing reduction with N input LUTs is more efficient than
        // using 2:1 reduction operators that are later packed into N input LUTs on FPGA
        if (GetCodeGenDeviceConfig()._supportsLuts)
        {
            uint64_t lut = 0;

            if (ParseTreeBinaryOpTypeAnd == reduceOp)
            {
                // 8 here is the lookup table for AND ("result" interpreted as a 4-bit integer)
                // src2 src1 | result
                //    0    0 | 0
                //    0    1 | 0
                //    1    0 | 0
                //    1    1 | 1
                lut = 8;
            }
            else
            {
                assert(ParseTreeBinaryOpTypeOr == reduceOp);

                // 14 here is the lookup table for OR ("result" interpreted as a 4-bit integer)
                // src2 src1 | result
                //    0    0 | 0
                //    0    1 | 1
                //    1    0 | 1
                //    1    1 | 1
                lut = 14;
            }

            result = ReduceBinaryOpLut(program, srcReg, lut, dstOp);
        }
        else
        {
            result = ReduceBinaryOpNoLut(program, srcReg, reduceOp, dstOp);
        }
    }

    return result;
}

// Emit a reduction tree
// that reduces a vector of N M-bit wide registers
// down to 1 M-bit wide register
// If N is 0, then the default value is used
OperationList ReduceVecBinaryOp(Program& program, const std::vector<AccessedRegister>& srcRegs,
                                const ParseTreeBinaryOpType reduceOp, const AccessedRegister& dstReg,
                                const Location& location, const std::string& intermediateRegName,
                                const mp_int& defaultValue)
{
    const size_t width = program._registerTable[dstReg._registerIndex]._width;

    std::vector<AccessedRegister> regs = srcRegs;

    OperationList result;

    while (regs.size() > 1)
    {
        std::vector<AccessedRegister> newRegs;

        // pair-wise reduction
        for (size_t i = 0; (i + 1) < regs.size(); i += 2)
        {
            const AccessedRegister mergedReg = {
                AllocateRegister(&program, width, RegisterType::Local, intermediateRegName + "_Reduced")};

            newRegs.push_back(mergedReg);

            Operation op = {};

            op._opcode = Opcode::BinaryOp;
            op._flags._binaryOpType = reduceOp;

            op._dst.push_back(mergedReg);

            for (size_t j = 0; j < 2; j++)
            {
                const AccessedRegister ar = regs[i + j];

                assert(width == program._registerTable[ar._registerIndex]._width);

                op._src.push_back(ar);
            }

            op.InsertLocation(location);

            result.push_back(op);
        }

        // If regs is odd sized
        // then pass through the final register
        if (1 == (regs.size() % 2))
        {
            newRegs.push_back(regs[regs.size() - 1]);
        }

        regs = newRegs;
    }

    // Final move to dstReg
    Operation op = {};

    op._opcode = Opcode::Mov;
    op._dst.push_back(dstReg);

    if (!regs.empty())
    {
        op._src.push_back(regs[0]);
    }
    else
    {
        const Literal literal = {Truncate(defaultValue, width), width};

        op._src.push_back(literal);
    }

    result.push_back(op);

    return result;
}

// Returns 0 or 1, coressponding to the value of a literal source operand at a particular bit index
uint64_t GetLiteralBit(const Operation& op, const size_t operandIndex, const size_t bitIndex)
{
    const SourceOperand& srcOp = op._src[operandIndex];

    assert(SourceOperandType::Literal == srcOp.Type());

    const Literal& lit = srcOp.GetLiteral();

    // Sign-extension requires this
    assert(lit._width > 0);

    uint64_t result = 0;

    size_t bitIndexToSelect = 0;

    bool readFromLiteral = false;

    if (bitIndex < lit._width)
    {
        bitIndexToSelect = bitIndex;

        readFromLiteral = true;
    }
    else
    {
        if (op.ShouldSignExtend(operandIndex))
        {
            // Sign extend
            bitIndexToSelect = lit._width - 1;

            readFromLiteral = true;
        }
        else
        {
            // Zero-extend
            readFromLiteral = false;
        }
    }

    if (readFromLiteral)
    {
        result = bit_test(lit._value, bitIndexToSelect);
    }
    else
    {
        result = 0;
    }

    return result;
}

struct LutSourceDesc
{
    bool _isLiteral;
    uint64_t _literalVal;

    size_t _srcBitIndex;
};

std::vector<LutSourceDesc> GetSourceOperandDescriptions(const Program& program, const Operation& op,
                                                        const size_t srcIdx)
{
    std::vector<LutSourceDesc> result;

    for (size_t srcOperandIndex = 0; srcOperandIndex < op._src.size(); ++srcOperandIndex)
    {
        LutSourceDesc lutSourceDesc = {};

        const SourceOperand& srcOperand = op._src[srcOperandIndex];

        if (SourceOperandType::Literal == srcOperand.Type())
        {
            lutSourceDesc._isLiteral = true;
            lutSourceDesc._literalVal = GetLiteralBit(op, srcOperandIndex, srcIdx);
        }
        else
        {
            const size_t srcWidth = srcOperand.Width(program);

            // Needed for sign-extension
            assert(srcWidth > 0);

            if (srcIdx < srcWidth)
            {
                // In-bounds - src bit "srcIdx" affects the output bit
                lutSourceDesc._isLiteral = false;
                lutSourceDesc._srcBitIndex = srcIdx;
            }
            else
            {
                if (op.ShouldSignExtend(srcOperandIndex))
                {
                    // Sign extend - the most-significant bit of the source affects the output bit
                    lutSourceDesc._isLiteral = false;
                    lutSourceDesc._srcBitIndex = srcWidth - 1;
                }
                else
                {
                    // Zero-extend
                    lutSourceDesc._isLiteral = true;
                    lutSourceDesc._literalVal = 0;
                }
            }
        }

        result.push_back(lutSourceDesc);
    }

    return result;
}

// Used to convert pointwise operators to LUT (output bit "i" is affected by input bit "i")
void ConvertPointwiseOpToLut(const Program& program, Operation& op, const uint64_t inputTable,
                             const AllocateLutFn& allocateLutFn)
{
    assert(1 == op._dst.size());

    const size_t dstWidth = op._dst[0].Width(program);

    assert(op._src.size() <= c_maxLutSources);

    Lut lut = allocateLutFn(dstWidth);

    for (size_t dstIdx = 0; dstIdx < dstWidth; dstIdx++)
    {
        LutEntry& entry = lut._lutEntries[dstIdx];

        const std::vector<LutSourceDesc> sourceDescs = GetSourceOperandDescriptions(program, op, dstIdx);

        // For each source
        size_t numSources = 0;

        for (size_t srcOperandIndex = 0; srcOperandIndex < op._src.size(); ++srcOperandIndex)
        {
            const LutSourceDesc& lutSourceDesc = sourceDescs[srcOperandIndex];

            if (!lutSourceDesc._isLiteral)
            {
                entry._sourceIndices[numSources] = srcOperandIndex;
                entry._sourceBit[numSources] = lutSourceDesc._srcBitIndex;

                numSources++;
            }
        }

        assert(numSources <= c_maxLutSources);

        entry._numSources = numSources;

        // Compute the LUT for this destination bit
        const size_t lutSize = 1ull << numSources;

        uint64_t outputTable = 0;

        for (size_t outputTableIndex = 0; outputTableIndex < lutSize; outputTableIndex++)
        {
            // Convert output table index into input table index
            size_t inputTableIndex = 0;

            numSources = 0;

            for (size_t srcOperandIndex = 0; srcOperandIndex < op._src.size(); ++srcOperandIndex)
            {
                uint64_t val = std::numeric_limits<uint64_t>::max();

                const LutSourceDesc& lutSourceDesc = sourceDescs[srcOperandIndex];

                if (lutSourceDesc._isLiteral)
                {
                    val = lutSourceDesc._literalVal;
                }
                else
                {
                    // Determine the value of this bit in the output table
                    val = (outputTableIndex >> numSources) & 0x1;

                    numSources++;
                }

                assert(val < 2);
                inputTableIndex |= (val << srcOperandIndex);
            }

            // Read the bit in the input table
            const uint64_t inputVal = (inputTable >> inputTableIndex) & 0x1;

            // Write this bit into the output table
            outputTable |= (inputVal << outputTableIndex);
        }

        entry._table = outputTable;
    }

    op._opcode = Opcode::Lut;
    op._flags._lut = lut;

    op._signExtendSourceMask = 0;
}

// Converts a left or right shift, (where 1 operand is constant)
// in to a LUT operation
void ConvertShiftOpToLut(const Program& program, Operation& op, const AllocateLutFn& allocateLutFn)
{
    assert(Opcode::BinaryOp == op._opcode);

    assert(2 == op._src.size());
    assert(1 == op._dst.size());

    const bool src0IsLiteral = op._src[0].Type() == SourceOperandType::Literal;
    const bool src1IsLiteral = op._src[1].Type() == SourceOperandType::Literal;

    const bool lhsSigned = op.ShouldSignExtend(0);
    const bool rhsSigned = op.ShouldSignExtend(1);

    // Signed shift amounts are reinterpreted as unsigned
    assert(!rhsSigned);

    if (!src0IsLiteral && src1IsLiteral)
    {
        // RHS is constant
        const uint64_t shiftAmount = MpToSizeT(op._src[1].GetLiteral()._value);

        const size_t srcWidth = op._src[0].Width(program);

        const size_t dstWidth = op._dst[0].Width(program);

        Lut lut = allocateLutFn(dstWidth);

        for (size_t dstIdx = 0; dstIdx < dstWidth; dstIdx++)
        {
            LutEntry& entry = lut._lutEntries[dstIdx];

            bool outputIsConst = false;
            size_t srcBit = std::numeric_limits<size_t>::max();

            if (ParseTreeBinaryOpTypeShl == op._flags._binaryOpType)
            {
                outputIsConst = (dstIdx < shiftAmount) || (dstIdx >= (srcWidth + shiftAmount));

                if (dstIdx < shiftAmount)
                {
                    // output constant 0
                    outputIsConst = true;
                }
                else if (dstIdx >= (srcWidth + shiftAmount))
                {
                    if (lhsSigned)
                    {
                        // replicate the sign bit of the input
                        outputIsConst = false;
                        srcBit = (srcWidth - 1);
                    }
                    else
                    {
                        // output constant 0
                        outputIsConst = true;
                    }
                }
                else
                {
                    outputIsConst = false;
                    srcBit = dstIdx - shiftAmount;
                }
            }
            else
            {
                assert(ParseTreeBinaryOpTypeShr == op._flags._binaryOpType);

                if (srcWidth >= shiftAmount)
                {
                    const size_t srcWidthMinusShift = srcWidth - shiftAmount;

                    if (lhsSigned)
                    {
                        // arithmetic shift
                        outputIsConst = false;

                        srcBit = (dstIdx >= srcWidthMinusShift) ? (srcWidth - 1) : dstIdx + shiftAmount;
                    }
                    else
                    {
                        // logical shift
                        outputIsConst = (dstIdx >= srcWidthMinusShift);

                        srcBit = dstIdx + shiftAmount;
                    }
                }
                else
                {
                    // Shift amount is greater than the operand width
                    if (lhsSigned)
                    {
                        // arithmetic shift
                        // Replicate sign bit
                        outputIsConst = false;

                        srcBit = srcWidth - 1;
                    }
                    else
                    {
                        // logical shift
                        // output constant 0
                        outputIsConst = true;
                    }
                }
            }

            if (outputIsConst)
            {
                // Output is constant 0
                entry._numSources = 0;
                entry._table = 0;
            }
            else
            {
                // Output is 1 of the sources
                entry._numSources = 1;

                entry._sourceIndices[0] = 0; // src operand 0 is the variable

                assert(srcBit < srcWidth);

                entry._sourceBit[0] = srcBit;

                entry._table = 2; // pass through
            }
        }

        op._opcode = Opcode::Lut;
        op._flags._lut = lut;
        op._signExtendSourceMask = 0;
    }
}

// Converts any binary operation into a lut as long as the total number of inputs bits is small enough
void ConvertBinaryOpToLut(const Program& program, Operation& op, const AllocateLutFn& allocateLutFn)
{
    assert(Opcode::BinaryOp == op._opcode);

    assert(2 == op._src.size());
    assert(1 == op._dst.size());

    // Count the total number of input bits
    size_t inputBitCount = 0;

    for (const SourceOperand& srcOp : op._src)
    {
        if (srcOp.Type() == SourceOperandType::Register)
        {
            inputBitCount += srcOp.Width(program);
        }
    }

    const size_t dstWidth = op._dst[0].Width(program);

    if (inputBitCount <= c_maxLutSources)
    {
        // Compute all possible results
        const size_t outputTableSize = 1ull << inputBitCount;

        std::vector<mp_int> resultTable(outputTableSize, 0);

        for (size_t outputTableIndex = 0; outputTableIndex < outputTableSize; outputTableIndex++)
        {
            Literal srcVals[2] = {};

            size_t outputTableOffset = 0;

            for (size_t srcOpIdx = 0; srcOpIdx < 2; srcOpIdx++)
            {
                const SourceOperand& srcOp = op._src[srcOpIdx];

                const size_t srcWidth = srcOp.Width(program);

                if (srcOp.Type() == SourceOperandType::Literal)
                {
                    srcVals[srcOpIdx] = srcOp.GetLiteral();
                }
                else
                {
                    const size_t mask = (1ull << srcWidth) - 1;

                    srcVals[srcOpIdx]._value = (outputTableIndex >> outputTableOffset) & mask;
                    srcVals[srcOpIdx]._width = srcWidth;

                    outputTableOffset += srcWidth;
                }
            }

            assert(outputTableOffset == inputBitCount);

            resultTable[outputTableIndex] = ImplementBinaryOp(srcVals[0], srcVals[1], dstWidth, op.ShouldSignExtend(0),
                                                              op.ShouldSignExtend(1), op._flags._binaryOpType);
        }

        Lut lut = allocateLutFn(dstWidth);

        for (size_t dstIdx = 0; dstIdx < dstWidth; ++dstIdx)
        {
            LutEntry& lutEntry = lut._lutEntries[dstIdx];

            lutEntry._numSources = inputBitCount;

            size_t numSources = 0;

            // Setup _sourceIndices and _sourceBit
            for (size_t srcOpIdx = 0; srcOpIdx < 2; srcOpIdx++)
            {
                const SourceOperand& srcOp = op._src[srcOpIdx];

                if (srcOp.Type() == SourceOperandType::Register)
                {
                    const size_t srcOpWidth = srcOp.Width(program);

                    for (size_t i = 0; i < srcOpWidth; i++)
                    {
                        lutEntry._sourceIndices[numSources] = srcOpIdx;

                        lutEntry._sourceBit[numSources] = i;

                        ++numSources;
                    }
                }
            }

            assert(numSources == inputBitCount);

            // Build _table
            lutEntry._table = 0;

            // For each entry in the output table
            for (size_t outputTableIndex = 0; outputTableIndex < outputTableSize; outputTableIndex++)
            {
                const mp_int resultVal = resultTable[outputTableIndex];

                const uint64_t resultBit = bit_test(resultVal, dstIdx) ? 1 : 0;

                // Table is 64-bits
                assert(outputTableIndex < 64);

                // Select the approriate bit from the pre-computed result
                lutEntry._table |= (resultBit << outputTableIndex);
            }
        }

        op._opcode = Opcode::Lut;
        op._flags._lut = lut;
        op._signExtendSourceMask = 0;
    }
}

// Converts select operations with small indices into LUT operations
void ConvertSelectOpToLut(const Program& program, Operation& op, const AllocateLutFn& allocateLutFn)
{
    assert(Opcode::Select == op._opcode);
    assert(1 == op._dst.size());

    const SourceOperandType indexType = op._src[0].Type();

    if (indexType == SourceOperandType::Register)
    {
        const size_t numChoices = op._src.size() - 1;
        assert(IsPow2(numChoices));

        const size_t numIndexBits = Log2(numChoices);
        assert(op._src[0].Width(program) >= numIndexBits);

        const size_t dstWidth = op._dst[0].Width(program);

        Lut lut = allocateLutFn(dstWidth);

        for (size_t dstIdx = 0; dstIdx < dstWidth; ++dstIdx)
        {
            const std::vector<LutSourceDesc> sourceDescs = GetSourceOperandDescriptions(program, op, dstIdx);

            LutEntry& lutEntry = lut._lutEntries[dstIdx];

            // Operand indices and bit indices are written to std::vector first
            // then copied into only if they fit
            std::vector<size_t> sourceIndices;
            std::vector<size_t> sourceBits;

            // each bit of the index counts as 1 source
            for (size_t i = 0; i < numIndexBits; i++)
            {
                sourceIndices.push_back(0);
                sourceBits.push_back(i);
            }

            // Each source operand gets 0 or 1 bit (because select is a pointwise operator with respect to all but the
            // index)
            for (size_t i = 1; i < op._src.size(); i++)
            {
                const LutSourceDesc& lutSourceDesc = sourceDescs[i];

                if (!lutSourceDesc._isLiteral)
                {
                    sourceIndices.push_back(i);
                    sourceBits.push_back(lutSourceDesc._srcBitIndex);
                }
            }

            assert(sourceIndices.size() == sourceBits.size());

            const size_t numSources = sourceIndices.size();

            if (numSources > c_maxLutSources)
            {
                // Too many non-literal source operands
                return;
            }

            assert(numSources <= c_maxLutSources);
            lutEntry._numSources = numSources;

            for (size_t i = 0; i < numSources; i++)
            {
                lutEntry._sourceIndices[i] = sourceIndices[i];
                lutEntry._sourceBit[i] = sourceBits[i];
            }

            // Build output table
            const size_t outputTableSize = 1ull << numSources;

            uint64_t outputTable = 0;

            const uint64_t indexMask = (1 << numIndexBits) - 1;

            for (size_t outputValue = 0; outputValue < outputTableSize; outputValue++)
            {
                // Determine the index
                const uint64_t index = outputValue & indexMask;
                assert(index < numChoices);

                // Determine the value of the choices for dstBit
                size_t numSources = 0;

                uint64_t choiceBits = 0;

                for (size_t i = 1; i < op._src.size(); i++)
                {
                    const LutSourceDesc& lutSourceDesc = sourceDescs[i];

                    uint64_t choiceBit = 0;

                    if (lutSourceDesc._isLiteral)
                    {
                        choiceBit = lutSourceDesc._literalVal;
                    }
                    else
                    {
                        choiceBit = (outputValue >> (numSources + numIndexBits)) & 1;

                        numSources++;
                    }

                    choiceBits |= (choiceBit << (i - 1));
                }

                const uint64_t outputBit = (choiceBits >> index) & 1;

                outputTable |= (outputBit << outputValue);
            }

            lutEntry._table = outputTable;
        }

        op._opcode = Opcode::Lut;
        op._flags._lut = lut;
        op._signExtendSourceMask = 0;
    }
}

void ConvertGatherOpToLut(const Program& program, Operation& op, const AllocateLutFn& allocateLutFn)
{
    assert(Opcode::Gather == op._opcode);
    assert(1 == op._dst.size());

    const std::vector<GatherEntry>& gatherEntries = *op._flags._gather._entries;

    assert(gatherEntries.size() == op._src.size());

    const size_t dstWidth = op._dst[0].Width(program);

    Lut lut = allocateLutFn(dstWidth);

    for (size_t i = 0; i < op._src.size(); ++i)
    {
        const GatherEntry& gatherEntry = gatherEntries[i];

        const SourceOperand& srcOp = op._src[i];

        const bool isLiteral = (SourceOperandType::Literal == srcOp.Type());

        for (size_t j = 0; j < gatherEntry._numBits; j++)
        {
            const size_t srcBit = gatherEntry._sourceOffset + j;

            const size_t dstBit = gatherEntry._destOffset + j;

            assert(dstBit < dstWidth);

            LutEntry& lutEntry = lut._lutEntries[dstBit];

            if (isLiteral)
            {
                lutEntry._numSources = 0;

                lutEntry._table = GetLiteralBit(op, i, srcBit);
            }
            else
            {
                lutEntry._numSources = 1;

                lutEntry._sourceIndices[0] = i;

                lutEntry._sourceBit[0] = srcBit;

                lutEntry._table = 2; // pass-through
            }
        }
    }

    op._opcode = Opcode::Lut;
    op._flags._lut = lut;
    op._signExtendSourceMask = 0;
}

// Called during PackLuts
// Converts an operation to be a LUT operation
// so that it can be combined with other operations
bool LowerToLut(const Program& program, Operation& op, const AllocateLutFn& allocateLutFn,
                const LowerToLutBehavior behavior)
{
    assert(CanOpcodeBeLoweredToLut(op._opcode));

    switch (op._opcode)
    {
    case Opcode::Mov:
    {
        assert(1 == op._src.size());
        assert(1 == op._dst.size());

        // Table 2'b10 = pass through
        ConvertPointwiseOpToLut(program, op, 2, allocateLutFn);
    }
    break;

    case Opcode::Clear:
    {
        assert(0 == op._src.size());
        assert(1 == op._dst.size());

        // Table 0 = set to 0
        ConvertPointwiseOpToLut(program, op, 0, allocateLutFn);
    }
    break;

    case Opcode::UnaryOp:
    {
        assert(1 == op._src.size());
        assert(1 == op._dst.size());

        assert(ParseTreeUnaryOpTypeInvert == op._flags._unaryOpType);

        // Table 2'b01 = flip
        ConvertPointwiseOpToLut(program, op, 1, allocateLutFn);
    }
    break;

    case Opcode::BinaryOp:
    {
        assert(2 == op._src.size());
        assert(1 == op._dst.size());

        switch (op._flags._binaryOpType)
        {
        case ParseTreeBinaryOpTypeAnd:
            // Table 4'b1000 = and
            ConvertPointwiseOpToLut(program, op, 8, allocateLutFn);
            break;

        case ParseTreeBinaryOpTypeOr:
            // Table 4'b1110 = or
            ConvertPointwiseOpToLut(program, op, 14, allocateLutFn);
            break;

        case ParseTreeBinaryOpTypeXor:
            // Table 4'b0110 = or
            ConvertPointwiseOpToLut(program, op, 6, allocateLutFn);
            break;

        case ParseTreeBinaryOpTypeShl:
        case ParseTreeBinaryOpTypeShr:
            ConvertShiftOpToLut(program, op, allocateLutFn);
            break;

        default:
            break;
        }

        // ConvertShiftOpToLut will skip if the rhs is not known
        if (Opcode::BinaryOp == op._opcode)
        {
            // If the total number of input bits to the entire operator is < c_maxSourceOperands
            // Then the operator can be converted no matter what the opcode
            ConvertBinaryOpToLut(program, op, allocateLutFn);
        }
    }
    break;

    case Opcode::Select:
        ConvertSelectOpToLut(program, op, allocateLutFn);
        break;

    case Opcode::Gather:
        ConvertGatherOpToLut(program, op, allocateLutFn);
        break;

    case Opcode::Lut:
        if (behavior == LowerToLutBehavior::DeepCopy)
        {
            // Caller needs to be able to modify the returned LUTs
            // without modifying original LUTs
            op._flags._lut = DeepCopyLut(op._flags._lut, allocateLutFn);
        }
        else
        {
            assert(behavior == LowerToLutBehavior::Default);
        }
        break;

    default:
        // Catch opcode that should be lowered here but accidentally are not
        assert(false);
        break;
    }

    // Clear sign-extension mask for all LUT operations
    // The sign extension is baked into the lut metadata
    if (Opcode::Lut == op._opcode)
    {
        op._signExtendSourceMask = 0;
    }

    return Opcode::Lut == op._opcode;
}

// Detects the case where the LUT table is all 0's or all 1's
// in this case, numSources can be set to 0
void OptimizeLutOpt(Operation& op)
{
    assert(Opcode::Lut == op._opcode);

    // Remove lut resources that have no affect on the output
    // This is important, because it can cause a lut to cross below a threshold
    // of accepting the lut or not (which is checked after this call returns)
    while (RemoveUnusedLutSourceRegisters(op))
    {
    }

    for (size_t dstIdx = 0; dstIdx < op._flags._lut._numDestinationBits; ++dstIdx)
    {
        LutEntry& lutEntry = op._flags._lut._lutEntries[dstIdx];

        if (lutEntry._numSources > 0)
        {
            const size_t tableSize = 1ull << lutEntry._numSources;

            assert(tableSize <= 64);
            const size_t mask = (tableSize == 64) ? std::numeric_limits<size_t>::max() : (1ull << tableSize) - 1;

            const uint64_t maskedTable = lutEntry._table & mask;

            if ((maskedTable == mask) || (maskedTable == 0))
            {
                // The table always outputs the same result - no need to have the source values
                lutEntry._numSources = 0;

                lutEntry._table = (maskedTable == 0) ? 0 : 1;
            }
        }
    }

    // Remove op._src if possible - to allow the producing operations to be optimized out
    bool needsSrcOp = false;

    for (size_t dstIdx = 0; dstIdx < op._flags._lut._numDestinationBits; ++dstIdx)
    {
        LutEntry& lutEntry = op._flags._lut._lutEntries[dstIdx];

        if (lutEntry._numSources != 0)
        {
            needsSrcOp = true;
        }
    }

    if (!needsSrcOp)
    {
        op._src.clear();
    }
}

// Helper for PackLuts
// Attempts to combine the lut operation in prevOp with op
bool CombineLuts(const Program& program, Operation& opInOut, const Operation& prevOpIn, const size_t selectedSrcOpIdx,
                 const size_t maxLutSources)
{
    // This optimization requires local operands for the previous operation
    // The trouble with global operands is:
    //
    // y = Op1(global)
    // z = Op2(y)
    // If this is transformed to
    //
    // y = Op1(global)
    // z = OpMerged(global)
    // then the read of the global can be moved outside of a basic block

    // Callers must ensure this
    assert(OperationUsesOnlyLocalSrcReg(program, prevOpIn));

    // Make copies of both operations, and convert them into LUT operations
    Operation op = opInOut;
    Operation prevOp = prevOpIn;

    if (!LowerToLut(program, op, AllocateLut) || !LowerToLut(program, prevOp, AllocateLut))
    {
        return false;
    }

    assert(Opcode::Lut == op._opcode);
    assert(Opcode::Lut == prevOp._opcode);

    const size_t dstWidth = op._dst[0].Width(program);

    assert(dstWidth == op._flags._lut._numDestinationBits);

    Lut lut = AllocateLut(dstWidth);

    // Maps a register index into the index of a source operand in the new operation
    std::map<size_t, size_t> regIndexToSrcOpIndexMap;

    // Add all src ops from op (except selectedSrcOpIdx) to regIndexToSrcOpIndexMap
    for (size_t srcOpIdx = 0; srcOpIdx < op._src.size(); ++srcOpIdx)
    {
        // skip selectedSrcOpIdx because it is being optimized out
        if (srcOpIdx != selectedSrcOpIdx)
        {
            const SourceOperand& srcOp = op._src[srcOpIdx];

            if (SourceOperandType::Register == srcOp.Type())
            {
                const size_t srcReg = srcOp.GetAccessedRegister()._registerIndex;

                if (regIndexToSrcOpIndexMap.end() == regIndexToSrcOpIndexMap.find(srcReg))
                {
                    const size_t index = regIndexToSrcOpIndexMap.size();

                    regIndexToSrcOpIndexMap[srcReg] = index;
                }
            }
        }
    }

    for (size_t dstIdx = 0; dstIdx < dstWidth; ++dstIdx)
    {
        const LutEntry& originalEntry = op._flags._lut._lutEntries[dstIdx];

        typedef std::pair<size_t, size_t> SourceIndexAndBitIndex;

        // Defines the sources that make up the new LUT
        std::vector<SourceIndexAndBitIndex> newSources;

        // Used to ensure that duplicate entries are not added to newSourceBits
        std::set<SourceIndexAndBitIndex> sourceSet;

        // For each source bit in op
        for (size_t i = 0; i < originalEntry._numSources; i++)
        {
            if (selectedSrcOpIdx == originalEntry._sourceIndices[i])
            {
                // This is the source operand that is going to be replaced by the LUT encoded in prevOp
                const size_t bitInPrevOp = originalEntry._sourceBit[i];
                assert(bitInPrevOp < prevOp._flags._lut._numDestinationBits);

                LutEntry& prevEntry = prevOp._flags._lut._lutEntries[bitInPrevOp];

                // All sources from the prev table must be added to the new table
                for (size_t j = 0; j < prevEntry._numSources; j++)
                {
                    const SourceOperand& srcOp = prevOp._src[prevEntry._sourceIndices[j]];
                    assert(SourceOperandType::Register == srcOp.Type());

                    const size_t srcReg = srcOp.GetAccessedRegister()._registerIndex;

                    if (regIndexToSrcOpIndexMap.end() == regIndexToSrcOpIndexMap.find(srcReg))
                    {
                        const size_t index = regIndexToSrcOpIndexMap.size();

                        regIndexToSrcOpIndexMap[srcReg] = index;
                    }

                    const SourceIndexAndBitIndex sourceIndexAndBitIndex(regIndexToSrcOpIndexMap[srcReg],
                                                                        prevEntry._sourceBit[j]);

                    if (sourceSet.end() == sourceSet.find(sourceIndexAndBitIndex))
                    {
                        newSources.push_back(sourceIndexAndBitIndex);

                        sourceSet.insert(sourceIndexAndBitIndex);
                    }
                }
            }
            else
            {
                const SourceOperand& srcOp = op._src[originalEntry._sourceIndices[i]];
                assert(SourceOperandType::Register == srcOp.Type());

                const size_t srcReg = srcOp.GetAccessedRegister()._registerIndex;

                // This source operand remains unchanged
                const size_t sourceIndex = LookupAndAssert<size_t>(regIndexToSrcOpIndexMap, srcReg);

                const SourceIndexAndBitIndex sourceIndexAndBitIndex(sourceIndex, originalEntry._sourceBit[i]);

                if (sourceSet.end() == sourceSet.find(sourceIndexAndBitIndex))
                {
                    newSources.push_back(sourceIndexAndBitIndex);

                    sourceSet.insert(sourceIndexAndBitIndex);
                }
            }
        }

        const size_t newNumSources = newSources.size();

        // If any output bit would exceed c_maxLutSources, then bail out
        if (newNumSources > c_maxLutSources)
        {
            return false;
        }

        // Compute the inverse of regIndexToSrcOpIndexMap
        // Maps register index to src op index in the new table
        std::map<size_t, size_t> srcOpIndexToRegIndexMap;

        for (const auto& p : regIndexToSrcOpIndexMap)
        {
            srcOpIndexToRegIndexMap[p.second] = p.first;
        }

        // build a mapping { source reg, source bit } -> index in new table
        std::map<SourceIndexAndBitIndex, size_t> sourceToOutputIndexMap;

        for (size_t i = 0; i < newSources.size(); i++)
        {
            const SourceIndexAndBitIndex& sourceIndexAndBitIndex = newSources[i];

            const size_t srcReg = LookupAndAssert<size_t>(srcOpIndexToRegIndexMap, sourceIndexAndBitIndex.first);

            const SourceIndexAndBitIndex srcRegAndBitIndex(srcReg, sourceIndexAndBitIndex.second);

            sourceToOutputIndexMap[srcRegAndBitIndex] = i;
        }

        // Fill out new LutEntry
        LutEntry& newEntry = lut._lutEntries[dstIdx];

        newEntry._numSources = newNumSources;

        for (size_t i = 0; i < newNumSources; i++)
        {
            const SourceIndexAndBitIndex& sourceIndexAndBitIndex = newSources[i];

            newEntry._sourceIndices[i] = sourceIndexAndBitIndex.first;
            newEntry._sourceBit[i] = sourceIndexAndBitIndex.second;
        }

        // Compute the new table
        uint64_t newTable = 0;

        const size_t newTableSize = 1ull << newNumSources;

        // For all entries in the new LUT
        for (uint64_t newTableSlot = 0; newTableSlot < newTableSize; newTableSlot++)
        {
            // Generate the index in the table associated with "op" that coressponds to the index in newOp
            // "newTableSlot"
            uint64_t slotInOriginalTable = 0;

            // For each bit in the addresses of the original table
            for (size_t bitIndexInOriginalTable = 0; bitIndexInOriginalTable < originalEntry._numSources;
                 bitIndexInOriginalTable++)
            {
                const size_t srcOpIdxInOriginalTable = originalEntry._sourceIndices[bitIndexInOriginalTable];

                uint64_t val = 0;

                if (srcOpIdxInOriginalTable == selectedSrcOpIdx)
                {
                    // this bit in the original table corresponds to N bits in the previous operation
                    const size_t bitInPrevOp = originalEntry._sourceBit[bitIndexInOriginalTable];
                    assert(bitInPrevOp < prevOp._flags._lut._numDestinationBits);

                    LutEntry& prevEntry = prevOp._flags._lut._lutEntries[bitInPrevOp];

                    // Use the previous operation to lookup the value that the previous operation would produce given
                    // the values in newTableSlot
                    if (prevEntry._numSources > 0)
                    {
                        uint64_t slotInPrevTable = 0;

                        for (size_t bitIndexInPrevTable = 0; bitIndexInPrevTable < prevEntry._numSources;
                             bitIndexInPrevTable++)
                        {
                            // Lookup which entry in the new table corresponds to this operand in the prev table
                            const SourceIndexAndBitIndex srcRegAndBitIndex(
                                prevOp._src[prevEntry._sourceIndices[bitIndexInPrevTable]]
                                    .GetAccessedRegister()
                                    ._registerIndex,
                                prevEntry._sourceBit[bitIndexInPrevTable]);

                            const size_t bitIndexInNewTable =
                                LookupAndAssert<size_t>(sourceToOutputIndexMap, srcRegAndBitIndex);

                            uint64_t valInNewTable = (newTableSlot >> bitIndexInNewTable) & 0x1;

                            slotInPrevTable |= (valInNewTable << bitIndexInPrevTable);
                        }

                        val = (prevEntry._table >> slotInPrevTable) & 1;
                    }
                    else
                    {
                        val = prevEntry._table;
                    }
                }
                else
                {
                    // this bit in the original table corresponds to 1 bit in the new table

                    // Lookup which entry in the new table corresponds to this operand in the original table
                    const SourceIndexAndBitIndex srcRegAndBitIndex(
                        op._src[srcOpIdxInOriginalTable].GetAccessedRegister()._registerIndex,
                        originalEntry._sourceBit[bitIndexInOriginalTable]);

                    const size_t bitIndexInNewTable =
                        LookupAndAssert<size_t>(sourceToOutputIndexMap, srcRegAndBitIndex);

                    val = (newTableSlot >> bitIndexInNewTable) & 0x1;
                }

                slotInOriginalTable |= (val << bitIndexInOriginalTable);
            }

            // Lookup the value in the original table
            const uint64_t valInOriginalTable = (originalEntry._table >> slotInOriginalTable) & 1;

            // Store the value in the new table
            newTable |= (valInOriginalTable << newTableSlot);
        }

        newEntry._table = newTable;
    }

    // On success, opInOut is converted to a LUT operation
    // This is not done on failure to keep the generated code
    // readable, and to make it compile quickly
    Operation newOp = {};

    newOp._opcode = Opcode::Lut;

    // Combine locations from both sources
    Union(newOp._locations, opInOut._locations);
    Union(newOp._locations, prevOpIn._locations);

    newOp._flags._lut = lut;
    newOp._signExtendSourceMask = 0;

    const size_t numSrc = regIndexToSrcOpIndexMap.size();

    newOp._src.resize(numSrc);

    for (const auto& p : regIndexToSrcOpIndexMap)
    {
        const size_t regIndex = p.first;
        const size_t srcOpIndex = p.second;

        const AccessedRegister ar = {regIndex};

        newOp._src[srcOpIndex] = SourceOperand(ar);
    }

    newOp._dst.push_back(op._dst[0]);

    OptimizeLutOpt(newOp);

    // After optimizing (which can remove unneeded sources),
    // check the input count against maxLutSources
    for (size_t i = 0; i < newOp._flags._lut._numDestinationBits; i++)
    {
        const LutEntry& lutEntry = newOp._flags._lut._lutEntries[i];

        if (lutEntry._numSources > maxLutSources)
        {
            return false;
        }
    }

    opInOut = newOp;

    return true;
}

// Detects Opcode::Lut operations where all destination bits have a compile-time known value
// Converts them to a mov of a literal (to enable ConstantPropagation to do more optimizations)
bool DetectConstantLut(Operation& op)
{
    assert(Opcode::Lut == op._opcode);
    assert(1 == op._dst.size());

    bool didChangeIR = false;

    bool allDestinationBitsKnown = true;
    mp_int knownValue = 0;

    for (size_t dstIndex = 0; dstIndex < op._flags._lut._numDestinationBits; dstIndex++)
    {
        const LutEntry& lutEntry = op._flags._lut._lutEntries[dstIndex];

        if (lutEntry._numSources == 0)
        {
            if (lutEntry._table != 0)
            {
                assert(lutEntry._table == 1);
                knownValue |= (mp_int(1) << dstIndex);
            }
        }
        else
        {
            allDestinationBitsKnown = false;
        }
    }

    if (allDestinationBitsKnown)
    {
        Operation newOp = {};

        newOp._opcode = Opcode::Mov;
        newOp._locations = op._locations;
        newOp._src.push_back(SourceOperand(knownValue));
        newOp._dst.push_back(op._dst[0]);

        op = newOp;

        didChangeIR = true;
    }

    return didChangeIR;
}

using LutLiteralValueTable = std::array<boost::optional<uint64_t>, c_maxLutSources>;

// Changes to LutEntry to account for the fact that some inputs are known
void SetConstantLutSources(
    const Operation& op, LutEntry& originalLutEntry,
    const LutLiteralValueTable& literalValueTable) // maps source index (in the lut) to the known value
{
    size_t numLiteralBits = 0;

    // bit[i] is set if source "i" is known
    size_t literalSrcOperandMask = 0;

    // Maps literal index to source operand index in original table
    std::array<size_t, c_maxLutSources> literalSrcOpTable;

    // Maps literal index into the value of the literal
    std::array<uint64_t, c_maxLutSources> literalValues;

    assert(originalLutEntry._numSources <= c_maxLutSources);

    for (size_t i = 0; i < originalLutEntry._numSources; i++)
    {
        const boost::optional<uint64_t> optionalVal = literalValueTable[i];
        if (optionalVal)
        {
            literalSrcOperandMask |= (1ull << i);

            literalSrcOpTable[numLiteralBits] = i;

            literalValues[numLiteralBits] = *optionalVal;

            numLiteralBits++;
        }
    }

    assert(numLiteralBits > 0);

    LutEntry newLutEntry = {};

    // Maps source operand index in new table to source operand index in original table
    std::array<size_t, c_maxLutSources> srcOpTable;

    assert(numLiteralBits <= originalLutEntry._numSources);
    newLutEntry._numSources = originalLutEntry._numSources - numLiteralBits;

    // Generate newLutEntry._sourceIndices and newLutEntry._sourceBit
    size_t numSourcesGenerated = 0;

    for (size_t srcOp = 0; srcOp < originalLutEntry._numSources; srcOp++)
    {
        if (0 == (literalSrcOperandMask & (1ull << srcOp)))
        {
            // This operand is not known
            assert(op._src[originalLutEntry._sourceIndices[srcOp]].Type() == SourceOperandType::Register);

            srcOpTable[numSourcesGenerated] = srcOp;

            newLutEntry._sourceIndices[numSourcesGenerated] = originalLutEntry._sourceIndices[srcOp];

            newLutEntry._sourceBit[numSourcesGenerated] = originalLutEntry._sourceBit[srcOp];

            numSourcesGenerated++;
        }
    }

    assert((numSourcesGenerated + numLiteralBits) == originalLutEntry._numSources);

    // Generate newLutEntry._table
    newLutEntry._table = 0;

    const size_t newTableSize = 1ull << newLutEntry._numSources;

    for (size_t newTableSlotIndex = 0; newTableSlotIndex < newTableSize; newTableSlotIndex++)
    {
        // Generate an index into originalLutEntry._table
        size_t indexIntoOriginalTable = 0;

        // For each (non-literal) src in the new operation
        for (size_t srcOpInNewLut = 0; srcOpInNewLut < numSourcesGenerated; srcOpInNewLut++)
        {
            // This operand is set in the new table at this slot
            if (0 != (newTableSlotIndex & (1ull << srcOpInNewLut)))
            {
                // Determine which source operand this corresponds to in the original lut
                const size_t srcOpInOriginalLut = srcOpTable[srcOpInNewLut];

                indexIntoOriginalTable |= (1ull << srcOpInOriginalLut);
            }
        }

        // For each literal src in the original operation
        for (size_t literalIndex = 0; literalIndex < numLiteralBits; literalIndex++)
        {
            const uint64_t literalVal = literalValues[literalIndex];
            assert(literalVal < 2); // should be 0 or 1

            if (literalVal != 0)
            {
                // Determine which source operand this corresponds to in the original lut
                const size_t srcOpInOriginalLut = literalSrcOpTable[literalIndex];

                indexIntoOriginalTable |= (1ull << srcOpInOriginalLut);
            }
        }

        // Lookup the value in the original table
        const uint64_t valueInOriginalTable = (originalLutEntry._table >> indexIntoOriginalTable) & 1;

        // Store this value in the new table
        newLutEntry._table |= (valueInOriginalTable << newTableSlotIndex);
    }

    originalLutEntry = newLutEntry;
}

// PackLuts assumes that the source operands referenced by LutEntry::_numsources
// are register operands (not literals)
// If an optimization after PackLuts *(for example, KillMoves) turns a source operand into a literal
// then this assumption may be violated
// This "optimization" detects this case and ensures that PackLuts can assume that sources are register operands
bool FixupLutOp(Operation& op)
{
    assert(Opcode::Lut == op._opcode);

    bool didChangeIR = false;

    for (size_t dstIndex = 0; dstIndex < op._flags._lut._numDestinationBits; dstIndex++)
    {
        LutEntry& originalLutEntry = op._flags._lut._lutEntries[dstIndex];

        size_t numLiteralSrcOperands = 0;

        LutLiteralValueTable literalValueTable;

        for (size_t srcOp = 0; srcOp < originalLutEntry._numSources; srcOp++)
        {
            const size_t srcIndex = originalLutEntry._sourceIndices[srcOp];

            const SourceOperand& srcOperand = op._src[srcIndex];

            if (SourceOperandType::Literal == srcOperand.Type())
            {
                const size_t whichBit = originalLutEntry._sourceBit[srcOp];

                const uint64_t literalBit = GetLiteralBit(op, srcIndex, whichBit);

                literalValueTable[srcOp] = boost::optional<uint64_t>(literalBit);

                // Remember that this operand is a literal
                numLiteralSrcOperands++;
            }
        }

        if (numLiteralSrcOperands != 0)
        {
            SetConstantLutSources(op, originalLutEntry, literalValueTable);

            didChangeIR = true;
        }
    }

    return didChangeIR;
}

// Given an existing LutEntry
// Generate a new one with 1 particular source operand removed.
// The source operand can be removed, because the value of that operand is provided
LutEntry RemoveSourceFromLut(const LutEntry& inputLutEntry, const size_t srcIndex, const size_t knownValue)
{
    assert(srcIndex < inputLutEntry._numSources);
    assert(knownValue < 2); // binary number

    const size_t oldTableSize = (1ull << inputLutEntry._numSources);

    LutEntry newLutEntry = {};

    newLutEntry._numSources = inputLutEntry._numSources - 1;

    newLutEntry._table = 0;

    size_t numRowsCopied = 0;

    // For each row in the old table
    // Only 1/2 of the rows will be taken (the 1/2 that correspond to knownValue)
    for (size_t oldOutputIndex = 0; oldOutputIndex < oldTableSize; oldOutputIndex++)
    {
        const size_t srcValue = (oldOutputIndex >> srcIndex) & 1;

        if (srcValue == knownValue)
        {
            // Copy this row
            const uint64_t oldValue = (inputLutEntry._table >> oldOutputIndex) & 1;

            newLutEntry._table |= (oldValue << numRowsCopied);
            numRowsCopied++;
        }
    }

    assert(numRowsCopied == (oldTableSize / 2));

    // fill in _sourceIndices and _sourceBit
    size_t newSrcIndex = 0;

    for (size_t oldSrcIndex = 0; oldSrcIndex < inputLutEntry._numSources; oldSrcIndex++)
    {
        if (oldSrcIndex != srcIndex)
        {
            newLutEntry._sourceIndices[newSrcIndex] = inputLutEntry._sourceIndices[oldSrcIndex];
            newLutEntry._sourceBit[newSrcIndex] = inputLutEntry._sourceBit[oldSrcIndex];
            newSrcIndex++;
        }
    }

    return newLutEntry;
}

// Detects Opcode::Lut operations where some source registers do not affect the output
// These source registers are removed
bool RemoveUnusedLutSourceRegisters(Operation& op)
{
    assert(Opcode::Lut == op._opcode);
    assert(1 == op._dst.size());

    bool didChangeIR = false;

    for (size_t dstIndex = 0; dstIndex < op._flags._lut._numDestinationBits; dstIndex++)
    {
        const LutEntry& lutEntry = op._flags._lut._lutEntries[dstIndex];

        // Loop over each source operand
        for (size_t candidateSrcIndex = 0; candidateSrcIndex < lutEntry._numSources; candidateSrcIndex++)
        {
            // Generate 2 tables
            // 0: assuming the candidate operand has the value 0
            // 1: assuming the candidate operand has the value 1
            std::array<LutEntry, 2> tables;

            for (size_t whichTable = 0; whichTable < 2; whichTable++)
            {
                tables[whichTable] = RemoveSourceFromLut(lutEntry, candidateSrcIndex, whichTable);
            }

            if (tables[0]._table == tables[1]._table)
            {
                // Both tables are identical
                // Which indicates that this source register does not affect the output
                // Remove it
                op._flags._lut._lutEntries[dstIndex] = tables[0];

                didChangeIR = true;

                // Don't try other candidates after modifying the table
                break;
            }
        }
    }

    return didChangeIR;
}

// Find cases where entire source operands are not referenced
// Removes them so that other optimizations don't detect a false dependency
bool RemoveUnusedLutSourceOperands(Operation& op)
{
    assert(Opcode::Lut == op._opcode);
    assert(1 == op._dst.size());

    bool didChangeIR = false;

    std::set<size_t> usedSourceIndices;

    // Iterate over all LUTs, forming the set of all source operands that are referenced
    for (size_t dstIndex = 0; dstIndex < op._flags._lut._numDestinationBits; dstIndex++)
    {
        const LutEntry& lutEntry = op._flags._lut._lutEntries[dstIndex];

        for (size_t i = 0; i < lutEntry._numSources; i++)
        {
            usedSourceIndices.insert(lutEntry._sourceIndices[i]);
        }
    }

    assert(usedSourceIndices.size() <= op._src.size());

    if (usedSourceIndices.size() < op._src.size())
    {
        // At least one source operand is not referenced

        // Maps old source operands to new ones
        std::map<size_t, size_t> operandMap;

        size_t newIndex = 0;

        for (const size_t index : usedSourceIndices)
        {
            SafeInsert(operandMap, index, newIndex);
            newIndex++;
        }

        assert(newIndex == usedSourceIndices.size());

        // Generate new vector of SourceOperands - removing unreferenced operands
        std::vector<SourceOperand> newSourceOperands(operandMap.size());

        for (const auto& p : operandMap)
        {
            assert(p.first < op._src.size());
            assert(p.second < newSourceOperands.size());
            newSourceOperands[p.second] = op._src[p.first];
        }

        op._src = newSourceOperands;

        // Update LutEntry
        for (size_t dstIndex = 0; dstIndex < op._flags._lut._numDestinationBits; dstIndex++)
        {
            LutEntry& lutEntry = op._flags._lut._lutEntries[dstIndex];

            for (size_t i = 0; i < lutEntry._numSources; i++)
            {
                lutEntry._sourceIndices[i] = SafeLookup(operandMap, lutEntry._sourceIndices[i]);
            }
        }

        didChangeIR = true;
    }

    return didChangeIR;
}

// Detects LUT operations where the source register
// appears in multiple source operands
bool RemoveDuplicateLutSourceRegisters(Operation& op)
{
    assert(Opcode::Lut == op._opcode);
    assert(1 == op._dst.size());
    assert(0 == op._signExtendSourceMask); // Sign extension should be baked into the lut

    // Maps register index to new operand index
    std::map<size_t, size_t> regToOperandIndex;

    // Maps old operand index to new operand index
    std::map<size_t, size_t> operandIndexMap;

    std::vector<SourceOperand> newSrcOperands;

    size_t numUniqueOperands = 0;

    bool duplicateFound = false;

    for (size_t i = 0; i < op._src.size(); i++)
    {
        const SourceOperand& srcOp = op._src[i];

        const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

        const auto it = regToOperandIndex.find(registerIndex);

        if (it == regToOperandIndex.end())
        {
            SafeInsert(regToOperandIndex, registerIndex, numUniqueOperands);

            SafeInsert(operandIndexMap, i, numUniqueOperands);

            newSrcOperands.push_back(srcOp);

            numUniqueOperands++;
        }
        else
        {
            duplicateFound = true;

            SafeInsert(operandIndexMap, i, it->second);
        }
    }

    if (duplicateFound)
    {
        // Update LutEntry
        for (size_t dstIndex = 0; dstIndex < op._flags._lut._numDestinationBits; dstIndex++)
        {
            LutEntry& lutEntry = op._flags._lut._lutEntries[dstIndex];

            for (size_t srcIndex = 0; srcIndex < lutEntry._numSources; srcIndex++)
            {
                lutEntry._sourceIndices[srcIndex] = SafeLookup(operandIndexMap, lutEntry._sourceIndices[srcIndex]);
            }
        }

        // Update source operand array
        op._src = std::move(newSrcOperands);
    }

    return duplicateFound;
}

// Detects truth tables that have
// duplicate input columns: the same (SourceOperandIndex, bit) pair used in multiple columns
bool RemoveDuplicateLutColumns(Operation& op)
{
    assert(Opcode::Lut == op._opcode);
    assert(1 == op._dst.size());

    bool didChangeIR = false;

    std::set<size_t> usedSourceIndices;

    // Iterate over all LUTs, forming the set of all source operands that are referenced
    for (size_t dstIndex = 0; dstIndex < op._flags._lut._numDestinationBits; dstIndex++)
    {
        LutEntry& lutEntry = op._flags._lut._lutEntries[dstIndex];

        using OpIndexAndBit = std::pair<size_t, size_t>;

        using OldAndNewSourceIndex = std::pair<std::set<size_t>, size_t>;

        // Maps (operand index, bit index) to LUT source index (in original and de-duplicated LUTs)
        std::map<OpIndexAndBit, OldAndNewSourceIndex> opIndexAndBitToSourceIndex;

        for (size_t sourceIndex = 0; sourceIndex < lutEntry._numSources; sourceIndex++)
        {
            const OpIndexAndBit oiab(lutEntry._sourceIndices[sourceIndex], lutEntry._sourceBit[sourceIndex]);

            const auto it = opIndexAndBitToSourceIndex.find(oiab);

            if (it == opIndexAndBitToSourceIndex.end())
            {
                OldAndNewSourceIndex sourceIndices;

                SafeInsert(sourceIndices.first, sourceIndex);
                sourceIndices.second = opIndexAndBitToSourceIndex.size();

                SafeInsert(opIndexAndBitToSourceIndex, oiab, sourceIndices);
            }
            else
            {
                OldAndNewSourceIndex& sourceIndices = it->second;

                SafeInsert(sourceIndices.first, sourceIndex);
            }
        }

        assert(opIndexAndBitToSourceIndex.size() <= lutEntry._numSources);

        if (opIndexAndBitToSourceIndex.size() < lutEntry._numSources)
        {
            // de-duplicate
            LutEntry newLut = {};

            newLut._numSources = opIndexAndBitToSourceIndex.size();

            // For each row in the de-duplicated table
            const size_t newTableSize = newLut.TableSize();

            for (size_t newTableRow = 0; newTableRow < newTableSize; newTableRow++)
            {
                size_t originalTableIndex = 0;

                // For each column in the de-duplicated table
                for (const auto& p : opIndexAndBitToSourceIndex)
                {
                    const OpIndexAndBit& oiab = p.first;
                    const std::set<size_t>& originalColumnIndices = p.second.first;
                    const size_t newColumnIndex = p.second.second;

                    if ((newTableRow >> newColumnIndex) & 0x1)
                    {
                        // Map row in de-duplicated table to many rows in original table
                        for (const size_t originalColumnIndex : originalColumnIndices)
                        {
                            originalTableIndex |= (1ull << originalColumnIndex);
                        }
                    }
                }

                if (lutEntry.GetTableEntry(originalTableIndex))
                {
                    newLut._table |= (1ull << newTableRow);
                }
            }

            for (const auto& p : opIndexAndBitToSourceIndex)
            {
                const OpIndexAndBit& oiab = p.first;
                const size_t newColumnIndex = p.second.second;

                assert(newColumnIndex < newLut._numSources);

                newLut._sourceIndices[newColumnIndex] = oiab.first;
                newLut._sourceBit[newColumnIndex] = oiab.second;
            }

            lutEntry = newLut;

            didChangeIR = true;
        }
    }

    return didChangeIR;
}

bool FixupLutOps(BasicBlock& basicBlock)
{
    bool didChangeIR = false;

    for (Operation& op : basicBlock._operations)
    {
        if (Opcode::Lut == op._opcode)
        {
            // Run to convergence, because some optimizations that follow assume this
            do
            {
                bool makingProgress = false;

                if (FixupLutOp(op))
                {
                    makingProgress = true;
                }

                if (RemoveUnusedLutSourceRegisters(op))
                {
                    makingProgress = true;
                }

                if (RemoveUnusedLutSourceOperands(op))
                {
                    makingProgress = true;
                }

                if (RemoveDuplicateLutSourceRegisters(op))
                {
                    makingProgress = true;
                }

                if (RemoveDuplicateLutColumns(op))
                {
                    makingProgress = true;
                }

                if (DetectConstantLut(op))
                {
                    makingProgress = true;
                }

                if (makingProgress)
                {
                    didChangeIR = true;
                }

                // DetectConstantLut can transform a lut into an operation that moves a constant
                if (!makingProgress || (Opcode::Lut != op._opcode))
                {
                    break;
                }
            } while (true);
        }
    }

    return didChangeIR;
}

bool FixupLutOps(Program& program, Function& function)
{
    bool didChangeIR = false;

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        if (FixupLutOps(basicBlock))
        {
            didChangeIR = true;
        }
    }

    return didChangeIR;
}

// Bit-level constant propagation for lut operations
bool BitConstantPropagation(Program& program, ControlFlowGraph& controlFlowGraph, Function& function,
                            ConstantBitMap* const constantBitMap)
{
    // Maps bit index to known value
    using BitToValue = std::map<size_t, bool>;

    using dfa_t = DataFlowAnalysis<BitToValue>;

    // Maps bit index to a mapping of source operand to known value
    using BitToSrcLiteralValueMap = std::map<size_t, LutLiteralValueTable>;

    const OperationEnumerationMode enumerationMode = controlFlowGraph.GetEnumerationMode();

    const auto lookupBit = [](const dfa_t::RegToT& regToBitToValue, const size_t registerIndex, const size_t bitIndex)
    {
        boost::optional<bool> result;

        const auto it = regToBitToValue.find(registerIndex);
        if (it != regToBitToValue.end())
        {
            const BitToValue& btv = it->second;

            const auto it2 = btv.find(bitIndex);
            if (it2 != btv.end())
            {
                result = boost::optional<bool>(it2->second);
            }
        }

        return result;
    };

    const auto intersectCb = [](const BitToValue& a, const BitToValue& b)
    {
        BitToValue result;

        for (const auto& p : a)
        {
            const auto it = b.find(p.first);
            if ((it != b.end()) && (it->second == p.second))
            {
                SafeInsert(result, p.first, p.second);
            }
        }

        return result;
    };

    const auto translateCb = [](const BitToValue& inputBits, ControlFlowGraph& cfg, const dfa_t::Direction direction,
                                BasicBlock* const from, BasicBlock* const to)
    {
        // BitToValue does not contain register indices, so no translation is necessary
        return inputBits;
    };

    const auto evalAdd = [&](const Operation& inputOp, BitToValue& constantBits, dfa_t::RegToT& regToBitToValue)
    {
        assert(Opcode::BinaryOp == inputOp._opcode);
        assert(ParseTreeBinaryOpTypeAdd == inputOp._flags._binaryOpType);

        assert(2 == inputOp._src.size());
        assert(1 == inputOp._dst.size());

        const size_t dstWidth = inputOp._dst[0].Width(program);

        // For each source operand
        // Determine the maximum index of a bit that could be non-zero
        std::array<size_t, 2> maxNonZeroSrcBit = {};

        for (size_t srcOpIndex = 0; srcOpIndex < 2; srcOpIndex++)
        {
            size_t& result = maxNonZeroSrcBit[srcOpIndex];

            const bool signExtend = inputOp.ShouldSignExtend(srcOpIndex);

            const SourceOperand& srcOp = inputOp._src[srcOpIndex];

            if (SourceOperandType::Literal == srcOp.Type())
            {
                const Literal& literal = srcOp.GetLiteral();

                const mp_int resizedLiteral = Resize(literal, signExtend, dstWidth);

                result = (resizedLiteral == 0) ? 0 : msb(resizedLiteral);
            }
            else
            {
                assert(SourceOperandType::Register == srcOp.Type());

                const size_t registerIndex = srcOp.GetAccessedRegister()._registerIndex;

                const size_t registerWidth = srcOp.Width(program);

                assert(dstWidth > 0);

                // Check to see if the MSB of the source is known
                const boost::optional<bool> srcMsbKnownValue =
                    lookupBit(regToBitToValue, registerIndex, registerWidth - 1);

                for (size_t bitIndexPlusOne = dstWidth; bitIndexPlusOne > 0; bitIndexPlusOne--)
                {
                    const size_t bitIndex = bitIndexPlusOne - 1;

                    if (bitIndex < registerWidth)
                    {
                        // Check if this bit has a known value
                        const boost::optional<bool> knownValue = lookupBit(regToBitToValue, registerIndex, bitIndex);

                        if (knownValue)
                        {
                            if (*knownValue)
                            {
                                // Found a known constant non-zero bit
                                result = bitIndex;
                                break;
                            }
                            else
                            {
                                // Found a known zero bit
                                // Continue searching
                            }
                        }
                        else
                        {
                            // Unknown non-constant bit
                            // It could be non-zero at runtime
                            result = bitIndex;
                            break;
                        }
                    }
                    else
                    {
                        if (signExtend)
                        {
                            if (srcMsbKnownValue)
                            {
                                if (*srcMsbKnownValue)
                                {
                                    // Sign-extending a non-zero MSB
                                    result = bitIndex;
                                    break;
                                }
                                else
                                {
                                    // Sign-extending a zero MSB
                                }
                            }
                            else
                            {
                                // MSB is unknown
                                result = bitIndex;
                                break;
                            }
                        }
                        else
                        {
                            // Zero-extend, this bit is known to be zero
                        }
                    }
                }
            }
        }

        // Record which destination bits are zero
        // +1 here because the carry can result in 1 more bit being set in the sum
        // +1 again because the first zero bit is one beyond that
        const size_t firstZeroDstBit = std::max(maxNonZeroSrcBit[0], maxNonZeroSrcBit[1]) + 2;

        for (size_t i = firstZeroDstBit; i < dstWidth; i++)
        {
            SafeInsert(constantBits, i, false);
        }
    };

    const auto evaluateLut = [&](const Operation& inputOp, BitToSrcLiteralValueMap& optimizations,
                                 BitToValue& constantBits, dfa_t::RegToT& regToBitToValue)
    {
        bool result = false;

        LutArenaAllocator allocator;

        Operation tempOp = inputOp;

        // DeepCopy is used to enable this function to modify the LUTs in tempOp._flags._lut
        // without modifying the original LUTs
        if (LowerToLut(program, tempOp, allocator.LutFn(), LowerToLutBehavior::DeepCopy))
        {
            assert(Opcode::Lut == tempOp._opcode);
            assert(1 == tempOp._dst.size());

            assert(tempOp._flags._lut._numDestinationBits == tempOp._dst[0].Width(program));

            // Determine if knowledge of constant input bits is helpful
            for (size_t dstBitIndex = 0; dstBitIndex < tempOp._flags._lut._numDestinationBits; dstBitIndex++)
            {
                LutLiteralValueTable lutLiteralValueTable;

                const LutEntry& lutEntry = tempOp._flags._lut._lutEntries[dstBitIndex];

                for (size_t sourceIndex = 0; sourceIndex < lutEntry._numSources; sourceIndex++)
                {
                    const size_t sourceOperandIndex = lutEntry._sourceIndices[sourceIndex];

                    const size_t sourceBitIndex = lutEntry._sourceBit[sourceIndex];

                    if (SourceOperandType::Register == tempOp._src[sourceOperandIndex].Type())
                    {
                        const size_t sourceRegisterIndex =
                            tempOp._src[sourceOperandIndex].GetAccessedRegister()._registerIndex;

                        const boost::optional<bool> knownValue =
                            lookupBit(regToBitToValue, sourceRegisterIndex, sourceBitIndex);

                        if (knownValue)
                        {
                            lutLiteralValueTable[sourceIndex] = *knownValue ? 1ull : 0ul;
                        }
                    }
                }

                // Check to see if any element in lutLiteralValueTable is set
                bool anyOptimizationPresent = false;

                for (const boost::optional<uint64_t>& v : lutLiteralValueTable)
                {
                    anyOptimizationPresent = anyOptimizationPresent || v.is_initialized();
                }

                if (anyOptimizationPresent)
                {
                    SafeInsert(optimizations, dstBitIndex, lutLiteralValueTable);
                }
            }

            // Modify tempOp based on constant inputs
            for (const auto& p : optimizations)
            {
                assert(p.first < tempOp._flags._lut._numDestinationBits);
                LutEntry& le = tempOp._flags._lut._lutEntries[p.first];

                SetConstantLutSources(tempOp, le, p.second);
            }

            // Determine which output bits are constant
            for (size_t i = 0; i < tempOp._flags._lut._numDestinationBits; i++)
            {
                const LutEntry& lutEntry = tempOp._flags._lut._lutEntries[i];

                if (lutEntry.IsConstantZero())
                {
                    SafeInsert(constantBits, i, false);
                }
                else if (lutEntry.IsConstantOne())
                {
                    SafeInsert(constantBits, i, true);
                }
            }

            result = true;
        }

        return result;
    };

    bool didChangeIR = false;

    const auto operationCb =
        [&](const Operation& inputOp, dfa_t::RegToT& regToBitToValue, const bool isFinalPass, const bool allowNewLuts)
    {
        // Record that nothing is known about all destination registers
        // Note that it is insufficient to clear entries in RegToT
        bool hasFifoDst = false;

        for (const DestinationOperand& dstOp : inputOp._dst)
        {
            if (DestinationOperandType::Register == dstOp.Type())
            {
                if (IsLocalRegisterType(program._registerTable[dstOp.GetAccessedRegister()._registerIndex]._type))
                {
                    regToBitToValue[dstOp.GetAccessedRegister()._registerIndex] = BitToValue();
                }
            }
            else
            {
                assert(DestinationOperandType::Fifo == dstOp.Type());
                hasFifoDst = true;
            }
        }

        // Only consider operations that act on local registers
        if (!OperationUsesOnlyLocalReg(program, inputOp))
        {
            return;
        }

        // Optimization assumes all destinations are registers
        if (hasFifoDst)
        {
            return;
        }

        // Only analyze/transform operations that can be converted to Opcode::Lut
        // and additions
        const bool isAdd =
            (Opcode::BinaryOp == inputOp._opcode) && (ParseTreeBinaryOpTypeAdd == inputOp._flags._binaryOpType);

        if (!CanOpcodeBeLoweredToLut(inputOp._opcode) && !isAdd)
        {
            return;
        }

        // Determine which destination bits are constant
        BitToSrcLiteralValueMap optimizations;
        BitToValue constantBits;

        if (evaluateLut(inputOp, optimizations, constantBits, regToBitToValue))
        {
        }
        else if (isAdd)
        {
            assert(constantBits.empty());
            assert(optimizations.empty());

            evalAdd(inputOp, constantBits, regToBitToValue);
        }

        // Update regToBitToValue
        assert(1 == inputOp._dst.size());

        const size_t dstRegisterIndex = inputOp._dst[0].GetAccessedRegister()._registerIndex;

        regToBitToValue[dstRegisterIndex] = constantBits;

        if (isFinalPass && constantBitMap)
        {
            // Store information about constant output values
            SafeInsert(*constantBitMap, dstRegisterIndex, constantBits);
        }

        // If some sources are known to be constant
        // then convert the operation to a lut and
        // bake that information in
        if (isFinalPass && allowNewLuts && !optimizations.empty())
        {
            // IR modifications only occur after data flow analysis has converged
            Operation& nonConstInputOp = const_cast<Operation&>(inputOp);

            // Convert to LUT (permanently)
            const bool lowerResult = LowerToLut(program, nonConstInputOp, AllocateLut);
            assert(lowerResult);

            for (const auto& p : optimizations)
            {
                assert(p.first < nonConstInputOp._flags._lut._numDestinationBits);
                LutEntry& le = nonConstInputOp._flags._lut._lutEntries[p.first];

                SetConstantLutSources(nonConstInputOp, le, p.second);
            }

            didChangeIR = true;
        }
    };

    dfa_t dfa(program, function, controlFlowGraph, dfa_t::Direction::Forward, enumerationMode, translateCb, intersectCb,
              [&](const Operation& op, dfa_t::RegToT& regToBitToValue)
              { operationCb(op, regToBitToValue, false, false); });

    //  Now that the analysis has converged, modify the IR based on the results of the analysis
    for (BasicBlock& bb : function._basicBlocks)
    {
        // Only insert new lut operations when
        // when the optimization level is > 1
        // and the device supports LUTs
        // and when running during the optimization passes
        // This function is also called at the end of compilation
        // to determine which bits are constant.
        // Do not modify operations in that pass.
        const bool insertLuts = (GetCodeGenConfig()._optimize > 1) && GetCodeGenDeviceConfig()._supportsLuts &&
                                (OperationEnumerationMode::Unscheduled == enumerationMode);

        // This will be modified
        dfa_t::RegToT regToBitToValue = dfa.GetRegisterMap(&bb);

        ForEachOperationForward(
            bb, [&](Operation& op) { operationCb(op, regToBitToValue, true, insertLuts); }, enumerationMode);
    }

    return didChangeIR;
}

// input:
// x = lut(y, z)
// w = lut(x)
//
// output:
// x = lut(y, z)
// w = lut(y, z)
bool PackLuts(Program& program, Function& function, const size_t phaseIndex)
{
    // During the initial optimization phase, no luts are added to the IR
    // this enables one set of optimizations to converge without luts
    // Once all optimization opportunities without luts are found, then
    // additional phases add luts to the IR
    if (phaseIndex < static_cast<size_t>(OptimizationPhase::PackLuts1))
    {
        return false;
    }

    // Non-FPGA devices do not support lookup tables
    // Do not attempt to decompose higher level operations into LUTs for later merging
    if (!GetCodeGenDeviceConfig()._supportsLuts)
    {
        return false;
    }

    // During the first lut phase, only pack up to 4 inputs into 1 lut
    // During the second lut phase, allow packing up to 6 inputs
    // This is targeted at FPGAS where 1 resource can implement 2 small luts, or 1 large lut
    // In the first phase, only small luts are generated (2 luts per FPGA resource)
    // In the second phase, small luts are merged into large luts.  The merging proccess
    // is break-even in the worst case, because 2 small luts (1 resource) are replaced with 1 large lut (1 resource)
    // In the best case, the merging finds allows additional optimization opportunities
    const size_t maxLutSources = (phaseIndex == static_cast<size_t>(OptimizationPhase::PackLuts1))
                                     ? GetCodeGenDeviceConfig()._smallLutSize
                                     : c_maxLutSources;

    bool didChangeIR = false;

    // This algorithm relies on the fact that the LUT opcode is marked that it can be lower to a LUT
    assert(CanOpcodeBeLoweredToLut(Opcode::Lut));

    for (BasicBlock& basicBlock : function._basicBlocks)
    {
        // Maps register index to operation that wrote to that register (the operation must be one that can be lowered
        // to a LUT) and a token that identifies WAW hazards
        LocalDefUseTracker lutWriteMap(program, basicBlock);

        for (Operation& op : basicBlock._operations)
        {
            // Update def-use tracker
            lutWriteMap.HandleOperation(op, CanOpcodeBeLoweredToLut(op._opcode));

            if (CanOpcodeBeLoweredToLut(op._opcode))
            {
                // The do/while loop runs multiple times
                // in case multiple sources can be merged in
                bool makingProgress = false;

                std::set<const Operation*> prevOperationSet;

                do
                {
                    makingProgress = false;

                    // See if any sources were written by lut operations
                    for (size_t srcOpIdx = 0; srcOpIdx < op._src.size(); ++srcOpIdx)
                    {
                        const SourceOperand& srcOp = op._src[srcOpIdx];

                        if (SourceOperandType::Register == srcOp.Type())
                        {
                            const size_t srcReg = srcOp.GetAccessedRegister()._registerIndex;

                            const Operation* const prevOperation = lutWriteMap.GetDef(srcReg);

                            // Count the number of reads of the sources
                            // This affects merging (to avoid duplicating logic in multiple consumers)
                            const size_t numReads = lutWriteMap.GetUseCount(srcReg);
                            assert(numReads > 0);

                            // There are multiple consumers of the output of the previous operation then don't merge
                            // Merging with multiple consumers would cause the producer logic to be duplicated at each
                            // consumer
                            if ((prevOperation != nullptr) && (numReads == 1))
                            {
                                // This optimization should only apply to locals, not globals
                                assert(IsLocalRegisterType(program._registerTable[srcReg]._type));

                                // Operations should only be placed in the def-use tracker
                                // if they can be lowered to a LUT operation
                                assert(CanOpcodeBeLoweredToLut(prevOperation->_opcode));

                                // Renaming ensures that none of the sources of prevOperation have been overwritten
                                assert(OperationUsesOnlyLocalSrcReg(program, *prevOperation));

                                // Only process each unique previous operation once - to avoid processing the same
                                // prev/curr pair over and over
                                if ((prevOperationSet.end() == prevOperationSet.find(prevOperation)) &&
                                    CombineLuts(program, op, *prevOperation, srcOpIdx, maxLutSources))
                                {
                                    // If CombineLuts succeeded, then op._src was changed
                                    // so the for loop is now invalid
                                    makingProgress = true;

                                    didChangeIR = true;

                                    prevOperationSet.insert(prevOperation);

                                    break;
                                }
                            }
                        }
                    }
                } while (makingProgress);
            }
        }
    }

    return didChangeIR;
}
