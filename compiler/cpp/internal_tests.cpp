// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

void TestAssert(const bool b)
{
    if (!b)
    {
        throw RuntimeErrorWithTrace("Test failure");
    }
}

template <typename T> void TestAssertEqual(const T& expected, const T& actual)
{
    if (expected != actual)
    {
        std::cout << "expected: " << expected << " actual: " << actual << "\n";
    }

    TestAssert(expected == actual);
}

// Test for LUT simplification transforms
void FixupLutTest()
{
    const size_t numOuterIterations = 1024 * 64;

    const size_t numEvalIterations = 32;

    const size_t maxDstBits = 16;

    const size_t maxSrcOperands = 4;

    const size_t maxRegisters = 4;

    const size_t operandBitWidth = 4;

    for (size_t iteration = 0; iteration < numOuterIterations; iteration++)
    {
        std::mt19937_64 rng(iteration);

        const size_t numSrcOperands = (rng() % maxSrcOperands) + 1;

        Operation srcOp = {};

        srcOp._opcode = Opcode::Lut;

        for (size_t srcOperandIndex = 0; srcOperandIndex < numSrcOperands; srcOperandIndex++)
        {
            SourceOperand srcOperand = {};

            if (0 == (rng() % 4))
            {
                // Literal
                srcOperand = SourceOperand(rng() % (1ull << operandBitWidth), operandBitWidth);
            }
            else
            {
                // Register
                const size_t registerIndex = rng() % maxRegisters;

                srcOperand = SourceOperand(AccessedRegister{registerIndex});
            }

            srcOp.PushOperand(srcOperand, false);
        }

        const size_t numDstBits = (rng() % maxDstBits) + 1;

        Lut srcLut = {};

        std::array<LutEntry, maxDstBits> lutEntries = {};

        srcLut._numDestinationBits = numDstBits;

        for (size_t dstBit = 0; dstBit < numDstBits; dstBit++)
        {
            LutEntry& le = lutEntries[dstBit];

            le._numSources = rng() % (c_maxLutSources + 1);

            le._table = rng() & le.TableMask();

            for (size_t srcIndex = 0; srcIndex < le._numSources; srcIndex++)
            {
                le._sourceIndices[srcIndex] = rng() % numSrcOperands;

                le._sourceBit[srcIndex] = rng() % operandBitWidth;
            }
        }

        srcLut._lutEntries = lutEntries.data();

        srcOp._flags._lut = srcLut;
        srcOp._dst.push_back(AccessedRegister{});

        BasicBlock bb(nullptr, {}, 0);
        bb._operations.push_back(srcOp);

        // FixupLutOps modifies the LutEntry array directly (no deep copy)
        // Save the contents of the original operation
        Operation originalOp = srcOp;

        std::array<LutEntry, maxDstBits> originalLutEntries = lutEntries;

        originalOp._flags._lut._lutEntries = originalLutEntries.data();

        FixupLutOps(bb);

        TestAssert(bb._operations.size() == 1);

        const Operation& resultOp = bb._operations.front();

        if (Opcode::Lut != resultOp._opcode)
        {
            // Src operation was converted to a mov
            // Don't test this case
            continue;
        }

        // Verify the 2 operations are equivalent
        // Test with random inputs
        for (size_t evalIteration = 0; evalIteration < numEvalIterations; evalIteration++)
        {
            mp_int srcRegisterValues[maxRegisters];

            for (size_t i = 0; i < maxRegisters; i++)
            {
                srcRegisterValues[i] = rng() % (1ull << operandBitWidth);
            }

            const auto evalSrcOperand = [&](const Operation& op, const size_t srcOperandIndex)
            {
                const SourceOperand& srcOperand = op._src.at(srcOperandIndex);

                mp_int result;

                if (SourceOperandType::Literal == srcOperand.Type())
                {
                    result = srcOperand.GetLiteral()._value;
                }
                else
                {
                    const size_t registerIndex = srcOperand.GetAccessedRegister()._registerIndex;

                    assert(registerIndex < maxSrcOperands);

                    result = srcRegisterValues[registerIndex];
                }

                return result;
            };

            const mp_int srcValue = originalOp._flags._lut.Evaluate(
                [&](const size_t srcOperandIndex) { return evalSrcOperand(originalOp, srcOperandIndex); });

            const mp_int resultValue = resultOp._flags._lut.Evaluate(
                [&](const size_t srcOperandIndex) { return evalSrcOperand(resultOp, srcOperandIndex); });

            TestAssert(srcValue == resultValue);
        }
    }
}

// SetOperationLocation whose lifetime spans
// the creation of a basic block in the context
void OperationLocationSpanBbTest()
{
    IRContext context = {};
    Program program = {};
    Function function = {};

    context._objectNameStack.push("test-object");
    context._program = &program;
    context._function = &function;
    context._expectEmptyFunctionInstanceStack = true;

    BasicBlock* const startBb = context.CreateBasicBlock({});
    context._basicBlock = startBb;

    {
        Operation op = {};
        startBb->_operations.push_back(op);
    }

    BasicBlock* endBb = nullptr;

    {
        Location newLoc = {};
        newLoc._beginLine = 123;
        newLoc._endLine = 125;
        newLoc._fileIndex = 4;
        newLoc._valid = true;

        SetOperationLocation sol(context, newLoc);

        // Add an operation to startBb
        // It should get location == newLoc
        {
            Operation op = {};
            startBb->_operations.push_back(op);
        }

        // Create a new basic block with an operation
        // that operation should also be assigned to newLoc
        endBb = context.CreateBasicBlock({});
        context._basicBlock = endBb;

        {
            Operation op = {};
            endBb->_operations.push_back(op);
        }
    }

    TestAssert(2 == startBb->_operations.size());

    TestAssert(startBb->_operations.front()._locations.empty());

    TestAssert(!startBb->_operations.back()._locations.empty());

    TestAssert(startBb->_operations.back()._locations.begin()->_lineNumber == 123);
    TestAssert(startBb->_operations.back()._locations.begin()->_fileIndex == 4);

    TestAssert(1 == endBb->_operations.size());

    TestAssert(!endBb->_operations.front()._locations.empty());

    TestAssert(endBb->_operations.back()._locations.begin()->_lineNumber == 123);
    TestAssert(endBb->_operations.back()._locations.begin()->_fileIndex == 4);
}

void PrettyToIdentifierTest()
{
    const struct
    {
        std::string _input;
        std::string _expected;
    } testCases[] = {
        {"hel(lo())", "hel_lo_"},  {"hel[lo[]]", "hel_lo_"}, {"hel{lo{}}", "hel_lo_"},

        {"foo.bar", "foo_bar"},    {"foo,bar", "foo_bar"},

        {"a & b", "a_and_b"},      {"a &&b", "a_and_b"},     {"a | b", "a_or_b"},      {"a|| b", "a_or_b"},
        {"a ^ b", "a_xor_b"},      {"a ^^ b", "a_xor_b"},    {"a << b", "a_shl_b"},    {"a >> b", "a_shr_b"},
        {"a == b", "a_eq_b"},      {"a != b", "a_ne_b"},     {"a <  b", "a_lt_b"},     {"a <= b", "a_le_b"},
        {"a >  b", "a_gt_b"},      {"a >= b", "a_ge_b"},     {"a + b", "a_plus_b"},    {"a - b", "a_minus_b"},
        {"a *    b", "a_times_b"}, {"a / b", "a_div_b"},     {"a % b", "a_mod_b"},

        {"-a", "_minus_a"},        {"~a", "not_a"},          {"!a", "not_a"},
    };

    for (const auto testCase : testCases)
    {
        const std::string actual = PrettyToIdentifier(testCase._input);

        TestAssertEqual(testCase._expected, actual);
    }
}

// Arithmetic shift right
void ImplementBinaryOp_ShrSignedTest()
{
    std::mt19937 rng(1234);

    for (size_t srcWidth = 1; srcWidth < 5; srcWidth++)
    {
        const size_t srcBound = 1ull << srcWidth;

        for (size_t dstWidth = 1; dstWidth < 10; dstWidth++)
        {
            const mp_int dstMask = (mp_int(1) << dstWidth) - 1;

            const Literal src = {rng() % srcBound, srcWidth};

            for (size_t shiftAmount = 0; shiftAmount < 5; shiftAmount++)
            {
                const mp_int actual =
                    ImplementBinaryOp(src, Literal{shiftAmount, 32}, dstWidth, true, false, ParseTreeBinaryOpTypeShr);

                const mp_int expected = (Widen(src, true, dstWidth + shiftAmount) >> shiftAmount) & dstMask;

                TestAssertEqual(actual, expected);
            }
        }
    }
}

void ImplementBinaryOp_DivModTest(const ParseTreeBinaryOpType type)
{
    for (int32_t numerator = -100; numerator < 100; numerator++)
    {
        for (int32_t denominator = 1; denominator < 10; denominator++)
        {
            int32_t expected = 0;

            switch (type)
            {
            case ParseTreeBinaryOpTypeDiv:
                expected = numerator / denominator;
                break;

            case ParseTreeBinaryOpTypeMod:
                expected = numerator % denominator;
                break;

            default:
                TestAssert(false);
            }

            // When construting mp_ints from integers, manually convert to twos complement representation
            // to mimic how negative numbers are represented internally
            const size_t modifiedNumerator = numerator < 0 ? ~abs(numerator) + 1 : numerator;
            const size_t modifiedDenominator = denominator < 0 ? ~abs(denominator) + 1 : denominator;

            const Literal n = {modifiedNumerator, 32};
            const Literal d = {modifiedDenominator, 32};

            mp_int actual = ImplementBinaryOp(n, d, 32, true, false, type);

            if (bit_test(actual, 31))
            {
                // result is negative

                // Flip actual and expected to both be positive
                actual = (actual ^ mp_int(0xffffffff)) + 1;

                expected = -expected;
            }

            TestAssert(MpToSizeT(actual) == expected);
        }
    }
}

void ComputeFifoSizeTest()
{
    const size_t configMinDepth = 32;
    const size_t configDualClockMinDepth = 64;
    const size_t configMinimumAlmostFullDepth = 3;
    const size_t configAlmostFullBackwardLatency = 2;

    size_t resultDepth;
    size_t resultAlmostFullDepth;
    size_t resultWriteDelay;
    size_t resultMinWriteDelay;

    const size_t minFifoDepths[] = {1, 32, 64};

    const size_t modifierMinDepths[] = {0, 32, 64};

    for (size_t stageIndex = 0; stageIndex < 128; stageIndex++)
    {
        for (size_t writeClock = 0; writeClock < 2; writeClock++)
        {
            for (size_t readClock = 0; readClock < 2; readClock++)
            {
                for (const size_t minDepth : minFifoDepths)
                {
                    for (const size_t modifierMinDepth : modifierMinDepths)
                    {
                        size_t resultAlmostFullSlots = 0;

                        for (size_t modifierCallRate = 1; modifierCallRate < 3; modifierCallRate++)
                        {
                            for (size_t addToResultDepth = 0; addToResultDepth < 2; addToResultDepth++)
                            {
                                const size_t callerMaxThreadCounts[] = {1, 2, std::numeric_limits<size_t>::max()};

                                for (size_t callerMaxThreadCount : callerMaxThreadCounts)
                                {
                                    // Dual clock fifos require 1 additional almost_full entry
                                    const size_t dualClockAdjustmentSize = (readClock != writeClock) ? 1 : 0;
                                    const size_t dualClockAdjustedConfigMinDepth =
                                        (readClock == writeClock) ? configMinDepth : configDualClockMinDepth;

                                    ComputeFifoSize(
                                        configMinDepth, configDualClockMinDepth, configMinimumAlmostFullDepth,
                                        configAlmostFullBackwardLatency, 0, 0, modifierMinDepth, modifierCallRate,
                                        readClock, writeClock, stageIndex, addToResultDepth == 1, minDepth,
                                        0, // maxDepth
                                        callerMaxThreadCount,
                                        false, // contextSaverHasTransactionSize
                                        0,     // transactionSize
                                        resultDepth, resultAlmostFullSlots, resultWriteDelay, resultMinWriteDelay);

                                    const size_t callRate = modifierCallRate;

                                    const size_t logicalStageIndex = (stageIndex + callRate - 1) / callRate;

                                    size_t expectedMinDepth =
                                        logicalStageIndex + configAlmostFullBackwardLatency + dualClockAdjustmentSize;

                                    if (addToResultDepth)
                                    {
                                        expectedMinDepth += minDepth;
                                    }
                                    else
                                    {
                                        expectedMinDepth = std::max(expectedMinDepth, minDepth);
                                    }

                                    expectedMinDepth = std::max(
                                        std::max(dualClockAdjustedConfigMinDepth, modifierMinDepth), expectedMinDepth);

                                    size_t expectedAlmostFullSlots =
                                        logicalStageIndex + configAlmostFullBackwardLatency;

                                    const size_t resultAlmostFullDepth = resultDepth - resultAlmostFullSlots;

                                    if ((callerMaxThreadCount < expectedMinDepth) &&
                                        (callerMaxThreadCount >= expectedMinDepth))
                                    {
                                        expectedMinDepth = callerMaxThreadCount;

                                        expectedAlmostFullSlots = 0;
                                    }
                                    else
                                    {
                                        // Validate that configMinimumAlmostFullDepth is honored
                                        TestAssert(resultAlmostFullDepth >= configMinimumAlmostFullDepth);

                                        if (readClock != writeClock)
                                        {
                                            // ChtiDualClockFifoWithWriteDelay requires 1 extra depth slot
                                            // because almost_full is derived from wr_usedw, it is not a native output
                                            // from the fifo
                                            TestAssert(resultAlmostFullDepth > 1);
                                        }

                                        const size_t minAlmostFullThreshold =
                                            configAlmostFullBackwardLatency + stageIndex +
                                            configMinimumAlmostFullDepth + (addToResultDepth ? minDepth : 0) +
                                            (2 * resultWriteDelay);

                                        expectedMinDepth = std::max(expectedMinDepth,
                                                                    minAlmostFullThreshold + expectedAlmostFullSlots);
                                    }

                                    TestAssert(resultDepth == expectedMinDepth);

                                    TestAssert(expectedAlmostFullSlots == resultAlmostFullSlots);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Test minWriteDepth and maxWriteDepth
    resultWriteDelay = 0;
    resultMinWriteDelay = 0;
    ComputeFifoSize(configMinDepth, configDualClockMinDepth, configMinimumAlmostFullDepth,
                    configAlmostFullBackwardLatency,
                    1,                                  // minWriteDelay
                    2,                                  // maxWriteDelay
                    0,                                  // modifierMinDepth
                    1,                                  // callRate
                    0,                                  // readClock
                    0,                                  // writeClock
                    0,                                  // stageIndex
                    false,                              // addToMinLogDepth
                    0,                                  // minLogDepth
                    0,                                  // maxDepth
                    std::numeric_limits<size_t>::max(), // syncCallerMaxThreadCount
                    false,                              // contextSaverHasTransactionSize
                    0,                                  // transactionSize
                    resultDepth, resultAlmostFullDepth, resultWriteDelay, resultMinWriteDelay);
    TestAssert(resultWriteDelay == 2);
    TestAssert(resultMinWriteDelay == 1);

    resultWriteDelay = 0;
    resultMinWriteDelay = 0;
    ComputeFifoSize(configMinDepth, configDualClockMinDepth, configMinimumAlmostFullDepth,
                    configAlmostFullBackwardLatency,
                    1,                                  // minWriteDelay
                    2,                                  // maxWriteDelay
                    0,                                  // modifierMinDepth
                    1,                                  // callRate
                    0,                                  // readClock
                    1,                                  // writeClock
                    0,                                  // stageIndex
                    false,                              // addToMinLogDepth
                    0,                                  // minLogDepth
                    0,                                  // maxDepth
                    std::numeric_limits<size_t>::max(), // syncCallerMaxThreadCount
                    false,                              // contextSaverHasTransactionSize
                    0,                                  // transactionSize
                    resultDepth, resultAlmostFullDepth, resultWriteDelay, resultMinWriteDelay);
    TestAssert(resultWriteDelay == 0);
    TestAssert(resultMinWriteDelay == 0);

    resultWriteDelay = 0;
    resultMinWriteDelay = 0;
    ComputeFifoSize(configMinDepth, configDualClockMinDepth, configMinimumAlmostFullDepth,
                    configAlmostFullBackwardLatency,
                    1,                                  // minWriteDelay
                    0,                                  // maxWriteDelay
                    0,                                  // modifierMinDepth
                    1,                                  // callRate
                    0,                                  // readClock
                    0,                                  // writeClock
                    0,                                  // stageIndex
                    false,                              // addToMinLogDepth
                    0,                                  // minLogDepth
                    0,                                  // maxDepth
                    std::numeric_limits<size_t>::max(), // syncCallerMaxThreadCount
                    false,                              // contextSaverHasTransactionSize
                    0,                                  // transactionSize
                    resultDepth, resultAlmostFullDepth, resultWriteDelay, resultMinWriteDelay);
    TestAssert(resultWriteDelay == 0);
    TestAssert(resultMinWriteDelay == 0);

    // Transactional FIFO
    ComputeFifoSize(configMinDepth, configDualClockMinDepth, configMinimumAlmostFullDepth,
                    configAlmostFullBackwardLatency,
                    1,                                  // minWriteDelay
                    0,                                  // maxWriteDelay
                    0,                                  // modifierMinDepth
                    1,                                  // callRate
                    0,                                  // readClock
                    0,                                  // writeClock
                    0,                                  // stageIndex
                    false,                              // addToMinLogDepth
                    0,                                  // minLogDepth
                    0,                                  // maxDepth
                    std::numeric_limits<size_t>::max(), // syncCallerMaxThreadCount
                    false,                              // contextSaverHasTransactionSize
                    64,                                 // transactionSize
                    resultDepth, resultAlmostFullDepth, resultWriteDelay, resultMinWriteDelay);
    const size_t minAlmostFullThreshold =
        configAlmostFullBackwardLatency + configMinimumAlmostFullDepth + 2 * resultWriteDelay;
    TestAssert(resultDepth == 64 + minAlmostFullThreshold);

    // Context saver in basic block with transactional FIFO
    ComputeFifoSize(configMinDepth, configDualClockMinDepth, configMinimumAlmostFullDepth,
                    configAlmostFullBackwardLatency,
                    1,     // minWriteDelay
                    0,     // maxWriteDelay
                    0,     // modifierMinDepth
                    1,     // callRate
                    0,     // readClock
                    0,     // writeClock
                    0,     // stageIndex
                    false, // addToMinLogDepth
                    0,     // minLogDepth
                    0,     // maxDepth
                    512,   // syncCallerMaxThreadCount
                    true,  // contextSaverHasTransactionSize
                    0,     // transactionSize
                    resultDepth, resultAlmostFullDepth, resultWriteDelay, resultMinWriteDelay);
    TestAssert(resultDepth == 512);
}

void DetectConstantLutTest()
{
    const size_t numDestinationBits = 8;

    LutEntry lutEntries[numDestinationBits] = {};

    const uint64_t expected = 0xa7;

    const size_t dstRegisterIndex = 78;

    for (size_t i = 0; i < numDestinationBits; i++)
    {
        lutEntries[i]._numSources = 0;

        lutEntries[i]._table = (expected & (1ull << i)) ? 1 : 0;
    }

    Operation op = {};

    op._opcode = Opcode::Lut;

    op._flags._lut._numDestinationBits = numDestinationBits;
    op._flags._lut._lutEntries = lutEntries;

    const AccessedRegister reg = {dstRegisterIndex};
    op._dst.push_back(reg);

    DetectConstantLut(op);

    TestAssert(Opcode::Mov == op._opcode);
    TestAssert(op._src[0].GetLiteral()._value == expected);
    TestAssert(op._dst[0].GetAccessedRegister()._registerIndex == dstRegisterIndex);
}

void FixupLutOpTest()
{
    LutEntry lutEntry = {};

    Operation op = {};

    op._opcode = Opcode::Lut;

    op._flags._lut._numDestinationBits = 1;
    op._flags._lut._lutEntries = &lutEntry;

    // Original lut has 3 sources {0, 1, 2}
    lutEntry._numSources = 3;

    for (size_t i = 0; i < lutEntry._numSources; i++)
    {
        lutEntry._sourceIndices[i] = i;
        lutEntry._sourceBit[i] = 0;
    }

    // Original table
    // src2 src1 src0 output
    // 0    0    0    1
    // 0    0    1    1
    // 0    1    0    0
    // 0    1    1    1
    // 1    0    0    1
    // 1    0    1    1
    // 1    1    0    0
    // 1    1    1    0
    lutEntry._table = 0x3B;

    const AccessedRegister reg = {};

    const mp_int literal = 1;

    op._src.push_back(SourceOperand(reg));
    op._src.push_back(SourceOperand(literal));
    op._src.push_back(SourceOperand(reg));

    FixupLutOp(op);

    // Expected result table
    // src2 src0 output
    // 0    0    0
    // 0    1    1
    // 1    0    0
    // 1    1    0
    TestAssert(lutEntry._numSources == 2);
    TestAssert(lutEntry._sourceIndices[0] == 0);
    TestAssert(lutEntry._sourceIndices[1] == 2);
    TestAssert(lutEntry._table == 2);
}

void Log2RoundUpTest()
{
    const struct
    {
        size_t _input;
        size_t _output;
    } testCases[] = {
        {1, 0},  {2, 1},  {3, 2},  {4, 2},  {5, 3},  {6, 3},  {7, 3},  {8, 3},  {9, 4},
        {10, 4}, {11, 4}, {12, 4}, {13, 4}, {14, 4}, {15, 4}, {16, 4}, {17, 5},
    };

    for (const auto& testCase : testCases)
    {
        const size_t expected = testCase._output;
        const size_t actual = Log2RoundUp(testCase._input);

        TestAssert(expected == actual);
    }
}

void GetIntegerBitCountTest()
{
    const struct
    {
        size_t _input;
        size_t _output;
    } testCases[] = {
        {0, 1}, {1, 1},  {2, 2},  {3, 2},  {4, 3},  {5, 3},  {6, 3},  {7, 3},  {8, 4},
        {9, 4}, {10, 4}, {11, 4}, {12, 4}, {13, 4}, {14, 4}, {15, 4}, {16, 5}, {17, 5},
    };

    for (const auto& testCase : testCases)
    {
        const size_t expected = testCase._output;
        const size_t actual = GetIntegerBitCount(testCase._input);

        TestAssert(expected == actual);
    }
}

void Log2Test()
{
    const struct
    {
        size_t _input;
        size_t _output;
    } testCases[] = {
        {1, 0}, {2, 1}, {4, 2}, {8, 3}, {16, 4},
    };

    for (const auto& testCase : testCases)
    {
        const size_t expected = testCase._output;
        const size_t actual = Log2(testCase._input);

        TestAssert(expected == actual);
    }
}

void UnaryOpTest()
{
    const struct
    {
        size_t _input;
        size_t _inputWidth;
        size_t _output;
        size_t _outputWidth;
        bool _signedSrc;
    } testCases[] = {
        {0, 8, 0xff, 8, false},       {0, 8, 0x7f, 7, false}, // narrow output width
        {0, 8, 0xffff, 16, false},                            // wide output width
        {0x80, 8, 0xff7f, 16, false},                         // wide output width
        {0, 8, 0xffff, 16, true},                             // wide output width - signed
        {1, 8, 0xfffe, 16, true},                             // wide output width - signed
        {0x80, 8, 0x007f, 16, true},                          // wide output width - signed
    };

    for (const auto& testCase : testCases)
    {
        const size_t expected = testCase._output;

        const Literal inputLiteral = {testCase._input, testCase._inputWidth};

        const mp_int actual =
            ImplementUnaryOp(inputLiteral, testCase._signedSrc, ParseTreeUnaryOpTypeInvert, testCase._outputWidth);

        TestAssert(expected == actual);
    }
}

void BinaryOpTest()
{
    const struct
    {
        Literal _src0;
        Literal _src1;
        size_t _expected;
        size_t _outputWidth;
        bool _signed0;
        bool _signed1;
        ParseTreeBinaryOpType _op;
    } testCases[] = {
        {{1, 8}, {0, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeGT},      // 1 > 0
        {{0, 8}, {0, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeGT},      // 0 > 0
        {{2, 8}, {1, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeGT},      // 2 > 0
        {{11, 8}, {10, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeGT},    // 11 > 10
        {{0, 8}, {0xff, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeGT},   // 0 > 255
        {{0, 8}, {0xff, 8}, 1, 1, false, true, ParseTreeBinaryOpTypeGT},    // 0 > -1
        {{0xff, 8}, {0xff, 8}, 1, 1, false, true, ParseTreeBinaryOpTypeGT}, // 255 > -1
        {{0xff, 8}, {0xff, 8}, 0, 1, true, true, ParseTreeBinaryOpTypeGT},  // -1 > -1
        {{0xfe, 8}, {0xff, 8}, 0, 1, true, true, ParseTreeBinaryOpTypeGT},  // -2 > -1
        {{0xff, 8}, {0xfe, 8}, 1, 1, true, true, ParseTreeBinaryOpTypeGT},  // -1 > -2

        {{1, 8}, {0, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeGE},      // 1 >= 0
        {{0, 8}, {0, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeGE},      // 0 >= 0
        {{2, 8}, {1, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeGE},      // 2 >= 0
        {{11, 8}, {10, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeGE},    // 11 >= 10
        {{0, 8}, {0xff, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeGE},   // 0 >= 255
        {{0, 8}, {0xff, 8}, 1, 1, false, true, ParseTreeBinaryOpTypeGE},    // 0 >= -1
        {{0xff, 8}, {0xff, 8}, 1, 1, false, true, ParseTreeBinaryOpTypeGE}, // 255 >= -1
        {{0xff, 8}, {0xff, 8}, 1, 1, true, true, ParseTreeBinaryOpTypeGE},  // -1 >= -1
        {{0xfe, 8}, {0xff, 8}, 0, 1, true, true, ParseTreeBinaryOpTypeGE},  // -2 >= -1
        {{0xff, 8}, {0xfe, 8}, 1, 1, true, true, ParseTreeBinaryOpTypeGE},  // -1 >= -2

        {{1, 8}, {0, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeLT},      // 1 < 0
        {{0, 8}, {0, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeLT},      // 0 < 0
        {{2, 8}, {1, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeLT},      // 2 < 0
        {{11, 8}, {10, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeLT},    // 11 < 10
        {{0, 8}, {0xff, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeLT},   // 0 < 255
        {{0, 8}, {0xff, 8}, 0, 1, false, true, ParseTreeBinaryOpTypeLT},    // 0 < -1
        {{0xff, 8}, {0xff, 8}, 0, 1, false, true, ParseTreeBinaryOpTypeLT}, // 255 < -1
        {{0xff, 8}, {0xff, 8}, 0, 1, true, true, ParseTreeBinaryOpTypeLT},  // -1 < -1
        {{0xfe, 8}, {0xff, 8}, 1, 1, true, true, ParseTreeBinaryOpTypeLT},  // -2 < -1
        {{0xff, 8}, {0xfe, 8}, 0, 1, true, true, ParseTreeBinaryOpTypeLT},  // -1 < -2

        {{1, 8}, {0, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeLE},      // 1 <= 0
        {{0, 8}, {0, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeLE},      // 0 <= 0
        {{2, 8}, {1, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeLE},      // 2 <= 0
        {{11, 8}, {10, 8}, 0, 1, false, false, ParseTreeBinaryOpTypeLE},    // 11 <= 10
        {{0, 8}, {0xff, 8}, 1, 1, false, false, ParseTreeBinaryOpTypeLE},   // 0 <= 255
        {{0, 8}, {0xff, 8}, 0, 1, false, true, ParseTreeBinaryOpTypeLE},    // 0 <= -1
        {{0xff, 8}, {0xff, 8}, 0, 1, false, true, ParseTreeBinaryOpTypeLE}, // 255 <= -1
        {{0xff, 8}, {0xff, 8}, 1, 1, true, true, ParseTreeBinaryOpTypeLE},  // -1 <= -1
        {{0xfe, 8}, {0xff, 8}, 1, 1, true, true, ParseTreeBinaryOpTypeLE},  // -2 <= -1
        {{0xff, 8}, {0xfe, 8}, 0, 1, true, true, ParseTreeBinaryOpTypeLE},  // -1 <= -2

        {{0x10, 8}, {0, 8}, 0x10, 8, false, false, ParseTreeBinaryOpTypeShr},
        {{0x80, 8}, {2, 8}, 0x20, 8, false, false, ParseTreeBinaryOpTypeShr}, // logical >>
        {{0x80, 8}, {2, 8}, 0xe0, 8, true, false, ParseTreeBinaryOpTypeShr},  // arithmetic >> (msb set)
        {{0x80, 9}, {2, 8}, 0x20, 8, true, false, ParseTreeBinaryOpTypeShr},  // arithmetic >> (msb not set)
    };

    for (const auto& testCase : testCases)
    {
        const size_t expected = testCase._expected;

        const mp_int actual = ImplementBinaryOp(testCase._src0, testCase._src1, testCase._outputWidth,
                                                testCase._signed0, testCase._signed1, testCase._op);

        TestAssert(expected == actual);
    }
}

// Tests symbol lookup with namespaces, for example
// uint32 g_F;
//
// namespace N
// {
//     uint32 g_F;
//
//     void Foo()
//     {
//         g_F = 5; // references N::g_F
//     }
// }
//
// g_F = 7; // references g_F;
//
// N::g_F = 8; // references N::g_f

void SymbolLookupTest()
{
    struct EmptyStruct
    {
    };

    const Location location = {};

    typedef TypeContext<EmptyStruct, size_t> TypeContextT;

    const std::string stringN("n");
    const std::string stringM("m");

    Scope scopeN;
    scopeN.push_back(stringN);

    Scope scopeM;
    scopeM.push_back(stringM);

    Scope scopeNM;
    scopeNM.push_back(stringN);
    scopeNM.push_back(stringM);

    {
        TypeContextT context;

        {
            // Push global scope
            TypeContextT::PushPopScope pushPopGlobalScope(context);

            // x = 1
            context.AddSymbol("x", 1, location);

            {
                // push scope "n"
                TypeContextT::PushPopScope pushPopScopeN(context, &stringN);

                // n::x = 2
                context.AddSymbol("x", 2, location);

                // Lookup x from within namespace n
                TestAssert(2 == context.LookupSymbol(Scope(), "x", location));

                // Lookup n::x from within namespace n
                TestAssert(2 == context.LookupSymbol(scopeN, "x", location));
            }

            // Lookup x from global namespace
            TestAssert(1 == context.LookupSymbol(Scope(), "x", location));

            // Lookup n::x from global namespace
            TestAssert(2 == context.LookupSymbol(scopeN, "x", location));
        }
    }

    // nested namespaces
    {
        TypeContextT context;

        {
            // Push global scope
            TypeContextT::PushPopScope pushPopGlobalScope(context);

            // x = 1
            context.AddSymbol("x", 1, location);

            {
                // push scope "n"
                TypeContextT::PushPopScope pushPopScopeN(context, &stringN);

                // n::x = 2
                context.AddSymbol("x", 2, location);

                {
                    // push scope "m"
                    TypeContextT::PushPopScope pushPopScopeM(context, &stringM);

                    // n::m::x = 3
                    context.AddSymbol("x", 3, location);

                    // Lookup x from within namespace n::m
                    TestAssert(3 == context.LookupSymbol(Scope(), "x", location));

                    // Lookup n::x from within namespace n::m
                    TestAssert(2 == context.LookupSymbol(scopeN, "x", location));

                    // Lookup n::m::x from within namespace n::m
                    TestAssert(3 == context.LookupSymbol(scopeNM, "x", location));
                }

                // Lookup x from within namespace n
                TestAssert(2 == context.LookupSymbol(Scope(), "x", location));

                // Lookup n::x from within namespace n
                TestAssert(2 == context.LookupSymbol(Scope(), "x", location));

                // Lookup n::m::x from within namespace n
                TestAssert(3 == context.LookupSymbol(scopeNM, "x", location));
            }

            // Lookup x from global namespace
            TestAssert(1 == context.LookupSymbol(Scope(), "x", location));

            // Lookup n::x from global namespace
            TestAssert(2 == context.LookupSymbol(scopeN, "x", location));

            // Lookup n::m::x from global namespace
            TestAssert(3 == context.LookupSymbol(scopeNM, "x", location));
        }
    }
}

// Validates that wall forces by themselves should push everything toward the center
void PlacementWallTest()
{
    const size_t nodeCount = 100;

    Placement placement;

    for (size_t i = 0; i < nodeCount; i++)
    {
        placement.AddNode();
    }

    Placement::Parameters params = {};

    params._numIterations = 100;
    params._seed = 1;
    params._wallStrength = 0.001f;
    params._updateRate = 0.1f;
    params._display = false;

    const size_t numResets = placement.Run(params);

    TestAssert(0 == numResets);

    for (size_t i = 0; i < nodeCount; i++)
    {
        const Placement::Vec2& pos = placement.GetNodePosition(i);

        const float diffX = fabsf(pos._x - 0.5f);
        const float diffY = fabsf(pos._y - 0.5f);

        const float epsilon = 0.1f;

        TestAssert(diffX < epsilon);
        TestAssert(diffY < epsilon);
    }
}

// Validates that intra-node forces counter-act wall forces
void PlacementBoundingBoxTest()
{
    const size_t nodeCount = 21;

    Placement placement;

    for (size_t i = 0; i < nodeCount; i++)
    {
        placement.AddNode();
    }

    Placement::Parameters params = {};

    params._numIterations = 500;
    params._seed = 1;
    params._pushStrength = 0.001f;
    params._wallStrength = 0.001f;
    params._updateRate = 0.001f;
    params._display = false;

    const size_t numResets = placement.Run(params);

    TestAssert(0 == numResets);

    float minX = std::numeric_limits<float>::max();
    float maxX = std::numeric_limits<float>::lowest();

    float minY = std::numeric_limits<float>::max();
    float maxY = std::numeric_limits<float>::lowest();

    for (size_t i = 0; i < nodeCount; i++)
    {
        const Placement::Vec2& pos = placement.GetNodePosition(i);

        minX = std::min<float>(minX, pos._x);
        maxX = std::max<float>(maxX, pos._x);

        minY = std::min<float>(minY, pos._y);
        maxY = std::max<float>(maxY, pos._y);
    }

    const float guardBandLo = 0.35f;
    const float guardBandHi = 1.0f - guardBandLo;

    TestAssert(minX >= 0.0f);
    TestAssert(minX <= guardBandLo);

    TestAssert(maxX <= 1.0f);
    TestAssert(maxX >= guardBandHi);

    TestAssert(minY >= 0.0f);
    TestAssert(minY <= guardBandLo);

    TestAssert(maxY <= 1.0f);
    TestAssert(maxY >= guardBandHi);
}

// Validates that connections cause nodes to be close to each other
void PlacementConnectionTest()
{
    const size_t nodeCount = 30;

    Placement placement;

    for (size_t i = 0; i < nodeCount; i++)
    {
        placement.AddNode();
    }

    // Connect even nodes to node 0
    for (size_t i = 2; i < nodeCount; i += 2)
    {
        placement.AddEdge(0, i);
    }

    // Connect odd nodes to node 1
    for (size_t i = 3; i < nodeCount; i += 2)
    {
        placement.AddEdge(1, i);
    }

    Placement::Parameters params = {};

    params._numIterations = 1000;
    params._seed = 1;
    params._pullStrength = 100.0f;
    params._pushStrength = 0.001f;
    params._wallStrength = 0.001f;
    params._updateRate = 0.001f;
    params._display = false;

    const size_t numResets = placement.Run(params);

    TestAssert(0 == numResets);

    // Even nodes should be close to other even nodes
    for (size_t i = 0; i < nodeCount; i += 2)
    {
        const Placement::Vec2& posI = placement.GetNodePosition(i);

        // Compute sum of distances to other even nodes
        float sumEvenDist = 0.0f;

        for (size_t j = 0; j < nodeCount; j += 2)
        {
            if (j != i)
            {
                const Placement::Vec2& posJ = placement.GetNodePosition(j);

                const float dist = Placement::Vec2::Length(Placement::Vec2::Diff(posI, posJ));

                sumEvenDist += dist;
            }
        }

        // Compute sum of distances to odd nodes
        float sumOddDist = 0.0f;

        for (size_t j = 1; j < nodeCount; j += 2)
        {
            const Placement::Vec2& posJ = placement.GetNodePosition(j);

            const float dist = Placement::Vec2::Length(Placement::Vec2::Diff(posI, posJ));

            sumOddDist += dist;
        }

        TestAssert(sumOddDist > sumEvenDist);
    }
}

void PlacementSortTest()
{
    const Placement::Vec2 positions[] = {
        Placement::Vec2(-2.0f, 1.0f),
        Placement::Vec2(10.0f, -1.0f),
        Placement::Vec2(2.5f, 9.0f),
        Placement::Vec2(2.1f, -8.0f),
    };

    const size_t positionCount = ARRAY_SIZE(positions);

    std::list<size_t> list;

    for (size_t i = 0; i < positionCount; i++)
    {
        list.push_back(i);
    }

    const auto callback = [&](const size_t index)
    {
        TestAssert(index < positionCount);

        return positions[index];
    };

    const bool display = false;

    const auto getNameCallback = [](const size_t index)
    {
        std::ostringstream str;
        str << index;
        return str.str();
    };

    Placement::Sort<size_t>(list, callback, getNameCallback, display, "sort test");

    // For indexing
    std::vector<size_t> result(list.begin(), list.end());

    TestAssert(result[0] == 2);
    TestAssert(result[1] == 0);
    TestAssert(result[2] == 1);
    TestAssert(result[3] == 3);
}

// Tests that Placement::Sort handles items with duplicate positions
void PlacementSortPlaceholderTest()
{
    const Placement::Vec2 positions[] = {
        Placement::Vec2(-2.0f, 1.0f),  Placement::Vec2(10.0f, -1.0f), Placement::Vec2(10.0f, -1.0f),
        Placement::Vec2(10.0f, -1.0f), Placement::Vec2(2.5f, 9.0f),   Placement::Vec2(2.1f, -8.0f),
        Placement::Vec2(2.1f, -8.0f),
    };

    const size_t positionCount = ARRAY_SIZE(positions);

    std::list<size_t> list;

    for (size_t i = 0; i < positionCount; i++)
    {
        list.push_back(i);
    }

    const auto callback = [&](const size_t index)
    {
        TestAssert(index < positionCount);

        return positions[index];
    };

    const bool display = false;

    const auto getNameCallback = [](const size_t index)
    {
        std::ostringstream str;
        str << index;
        return str.str();
    };

    Placement::Sort<size_t>(list, callback, getNameCallback, display, "sort test");

    TestAssert(list.size() == 7);

    // For indexing
    std::vector<size_t> result(list.begin(), list.end());

    TestAssert(result[0] == 4);
    TestAssert(result[1] == 0);
    TestAssert(result[2] == 1);
    TestAssert(result[3] == 2);
    TestAssert(result[4] == 3);
    TestAssert(result[5] == 5);
    TestAssert(result[6] == 6);
}

void PlacementVec2Test()
{
    {
        const Placement::Vec2 a(3.0f, -1.5f);
        const Placement::Vec2 b(4.0f, 5.25f);

        const Placement::Vec2 sum = Placement::Vec2::Add(a, b);

        TestAssert(sum._x == 7.0f);
        TestAssert(sum._y == 3.75f);
    }

    {
        const Placement::Vec2 a(3.0f, -1.5f);
        const Placement::Vec2 b(4.0f, 5.25f);

        const Placement::Vec2 diff = Placement::Vec2::Diff(a, b);

        TestAssert(diff._x == -1.0f);
        TestAssert(diff._y == -6.75f);
    }

    {
        const Placement::Vec2 v(3.0f, -1.5f);

        const float length = Placement::Vec2::Length(v);

        const float expected = sqrtf(11.25);

        TestAssert(expected == length);
    }

    {
        const Placement::Vec2 v(3.0f, -1.5f);

        const Placement::Vec2 scaled = Placement::Vec2::Scale(v, -2.0f);

        TestAssert(scaled._x == -6.0f);
        TestAssert(scaled._y == 3.0f);
    }
}

void GetRaceCounterDepthTest()
{
    const size_t reductionWidth = 4;

    const struct
    {
        size_t _rawSize;
        RaceCounterTreeDesc _expected;
    } testCases[] = {
        // KanagawaRaceCounter has a minimum depth of 1
        {0, {4, 1}},

        {1, {4, 1}},      {2, {4, 1}},     {3, {4, 1}},   {4, {4, 1}},

        {5, {16, 2}},     {6, {16, 2}},    {7, {16, 2}},  {8, {16, 2}},  {9, {16, 2}},  {10, {16, 2}},
        {11, {16, 2}},    {12, {16, 2}},   {13, {16, 2}}, {14, {16, 2}}, {15, {16, 2}}, {16, {16, 2}},

        {17, {64, 3}},    {64, {64, 3}},

        {65, {256, 4}},   {256, {256, 4}},

        {257, {1024, 5}},
    };

    for (const auto& testCase : testCases)
    {
        const RaceCounterTreeDesc actual = GetRaceCounterWidthDepth(testCase._rawSize, reductionWidth);

        TestAssert(actual._width == testCase._expected._width);
        TestAssert(actual._depth == testCase._expected._depth);
    }
}

void GetOpPathLengthTest()
{
    {
        // Path length derived from opcode only
        const struct
        {
            Opcode _opcode;
            size_t _expected;
        } testCases[] = {
            {Opcode::Mov, 0},     {Opcode::MovCrossFunction, 0}, {Opcode::Clear, 0},
            {Opcode::UnaryOp, 1}, {Opcode::WriteGlobal, 1},      {Opcode::Gather, 0},
        };

        Program program;

        for (const auto& testCase : testCases)
        {
            Operation op = {};

            op._opcode = testCase._opcode;

            const size_t actual = GetOpPathLength(program, op, 0);

            TestAssert(actual == testCase._expected);
        }
    }

    {
        // Mux
        const struct
        {
            size_t _numChoices;
            size_t _expected;
        } testCases[] = {
            {1, 1},  {2, 1},  {3, 1},  {4, 1},

            {5, 2},  {6, 2},  {15, 2}, {16, 2},

            {17, 3}, {18, 3},
        };

        Program program;

        for (const auto& testCase : testCases)
        {
            Operation op = {};

            op._opcode = Opcode::Select;

            op._src.resize(testCase._numChoices + 1);

            const size_t actual = GetOpPathLength(program, op, 0);

            TestAssert(actual == testCase._expected);
        }
    }

    {
        // Add/Sub
        const ParseTreeBinaryOpType binaryOpTypeTable[] = {
            ParseTreeBinaryOpTypeAdd,
            ParseTreeBinaryOpTypeSub,
        };

        const struct
        {
            size_t _dstWidth;
            size_t _expected;
        } testCases[] = {
            {1, 1},  {2, 1},  {19, 1}, {20, 1},

            {21, 2}, {22, 2}, {39, 2}, {40, 2},

            {41, 3},
        };

        Program program;

        for (const ParseTreeBinaryOpType opcode : binaryOpTypeTable)
        {
            for (const auto& testCase : testCases)
            {
                Operation op = {};

                const AccessedRegister ar = {program._registerTable.size()};

                op._dst.push_back(ar);

                RegisterDescription regDesc = {};
                regDesc._width = testCase._dstWidth;
                regDesc._type = RegisterType::Local;

                program._registerTable.push_back(regDesc);

                op._opcode = Opcode::BinaryOp;
                op._flags._binaryOpType = opcode;

                const size_t actual = GetOpPathLength(program, op, 20);

                TestAssert(actual == testCase._expected);
            }
        }
    }

    {
        // lut
        const struct
        {
            struct
            {
                size_t _numSources;
                uint64_t _table;
            } _lutEntries[2];

            size_t _expected;
        } testCases[] = {
            {{{0, 0}, {0, 0}}, 0}, // both entries are constant
            {{{1, 2}, {0, 0}}, 0}, // one entry is passthrough
            {{{1, 1}, {0, 0}}, 1}, // one entry is invert
            {{{0, 0}, {1, 1}}, 1}, // one entry is invert
            {{{2, 0}, {0, 0}}, 1}, // more than 1 source
        };

        Program program;

        for (const auto& testCase : testCases)
        {
            LutEntry lutEntries[2] = {};

            for (size_t i = 0; i < 2; i++)
            {
                lutEntries[i]._numSources = testCase._lutEntries[i]._numSources;
                lutEntries[i]._table = testCase._lutEntries[i]._table;
            }

            Operation op = {};

            op._opcode = Opcode::Lut;

            op._flags._lut._numDestinationBits = 2;

            op._flags._lut._lutEntries = lutEntries;

            const size_t actual = GetOpPathLength(program, op, 0);

            TestAssert(actual == testCase._expected);
        }
    }

    {
        // the remaining binary operations
        const struct
        {
            ParseTreeBinaryOpType _opcode;
            size_t _srcWidth[2];
            size_t _expected;
        } testCases[] = {
            // shift left
            {ParseTreeBinaryOpTypeShl, {10, 0}, 0}, // rhs is literal

            {ParseTreeBinaryOpTypeShl, {16, 1}, 1},
            {ParseTreeBinaryOpTypeShl, {16, 2}, 1},
            {ParseTreeBinaryOpTypeShl, {16, 3}, 2},
            {ParseTreeBinaryOpTypeShl, {16, 4}, 2},

            {ParseTreeBinaryOpTypeShl, {32, 1}, 1},

            // shift right
            {ParseTreeBinaryOpTypeShr, {10, 0}, 0}, // rhs is literal

            {ParseTreeBinaryOpTypeShr, {16, 1}, 1},
            {ParseTreeBinaryOpTypeShr, {16, 2}, 1},
            {ParseTreeBinaryOpTypeShr, {16, 3}, 2},
            {ParseTreeBinaryOpTypeShr, {16, 4}, 2},

            {ParseTreeBinaryOpTypeShr, {32, 1}, 1},

            // lutmul
            {ParseTreeBinaryOpTypeLutMul, {2, 4}, 1},
            {ParseTreeBinaryOpTypeLutMul, {3, 1}, 1},
            {ParseTreeBinaryOpTypeLutMul, {3, 4}, 2},

            // logical operations
            {ParseTreeBinaryOpTypeAnd, {2, 4}, 1},
            {ParseTreeBinaryOpTypeOr, {3, 1}, 1},
            {ParseTreeBinaryOpTypeXor, {3, 4}, 1},

            // comparisons
            {ParseTreeBinaryOpTypeEQ, {2, 4}, 1},
            {ParseTreeBinaryOpTypeNE, {3, 1}, 1},
            {ParseTreeBinaryOpTypeGT, {3, 4}, 1},
            {ParseTreeBinaryOpTypeGE, {2, 4}, 1},
            {ParseTreeBinaryOpTypeLT, {3, 1}, 1},
            {ParseTreeBinaryOpTypeLE, {3, 4}, 1},
        };

        Program program;

        for (const auto& testCase : testCases)
        {
            Operation op = {};

            for (size_t i = 0; i < 2; i++)
            {
                if (0 == testCase._srcWidth[i])
                {
                    op._src.push_back(SourceOperand(300)); // literal
                }
                else
                {
                    const AccessedRegister ar = {program._registerTable.size()};

                    RegisterDescription regDesc = {};
                    regDesc._width = testCase._srcWidth[i];
                    regDesc._type = RegisterType::Local;

                    program._registerTable.push_back(regDesc);

                    op._src.push_back(SourceOperand(ar));
                }
            }

            op._opcode = Opcode::BinaryOp;
            op._flags._binaryOpType = testCase._opcode;

            const size_t actual = GetOpPathLength(program, op, 0);

            TestAssert(actual == testCase._expected);
        }
    }
}

void PathTest()
{
    TestAssert("bar.txt" == Filename("foo/bar.txt"));
    TestAssert("bar.txt" == Filename("foo\\bar.txt"));
    TestAssert("bar.txt" == Filename("bar.txt"));
    TestAssert("bar.txt" == Filename("foo/foo/bar.txt"));
    TestAssert("bar.txt" == Filename("foo\\foo\\bar.txt"));
    TestAssert("bar.txt" == Filename("foo/foo\\bar.txt"));
    TestAssert("bar.txt" == Filename("foo\\foo/bar.txt"));
    TestAssert("bar" == Filename("bar"));
    TestAssert("bar" == Filename("\\bar"));
    TestAssert("bar" == Filename("//bar"));
    TestAssert("" == Filename(""));
}

void FixupStringTest()
{
    const struct
    {
        std::string _input;
        std::string _expected;
    } testCases[] = {
        {"", ""},         {"test", "test"},       {"TEST", "TEST"},
        {"t123", "t123"}, {"bl a ke", "bl_a_ke"}, {"bl:a:ke", "bl_a_ke"},
        {"3xyz", "_xyz"},
    };

    for (const auto& testCase : testCases)
    {
        const std::string actual = FixupString(testCase._input);

        TestAssert(testCase._expected == actual);
    }
}

void RemoveSourceFromLutTest()
{
    {
        // Passthrough LUT
        LutEntry le = {};
        le._numSources = 1;
        le._table = 0x2; // output = input

        {
            // Constant 0 input should give constant 0 output
            const LutEntry result0 = RemoveSourceFromLut(le, 0, 0);
            TestAssert(result0._numSources == 0);
            TestAssert(result0._table == 0);
        }

        {
            // Constant 1 input should give constant 1 output
            const LutEntry result1 = RemoveSourceFromLut(le, 0, 1);
            TestAssert(result1._numSources == 0);
            TestAssert(result1._table == 1);
        }
    }

    {
        // Invert LUT
        LutEntry le = {};
        le._numSources = 1;
        le._table = 0x1; // output = ~input

        {
            // Constant 0 input should give constant 0 output
            const LutEntry result0 = RemoveSourceFromLut(le, 0, 0);
            TestAssert(result0._numSources == 0);
            TestAssert(result0._table == 1);
        }

        {
            // Constant 1 input should give constant 1 output
            const LutEntry result1 = RemoveSourceFromLut(le, 0, 1);
            TestAssert(result1._numSources == 0);
            TestAssert(result1._table == 0);
        }
    }

    {
        // OR LUT
        LutEntry le = {};
        le._numSources = 2;
        le._table = 0xe; // output = src0 | src1

        {
            // give src0 a constant value of 0
            // Result should be a passthrough lut
            const LutEntry result = RemoveSourceFromLut(le, 0, 0);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 2);
        }

        {
            // give src0 a constant value of 1
            // Result should be a const value of 1
            const LutEntry result = RemoveSourceFromLut(le, 0, 1);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 0x3);
        }

        {
            // give src1 a constant value of 0
            // Result should be a passthrough lut
            const LutEntry result = RemoveSourceFromLut(le, 1, 0);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 2);
        }

        {
            // give src1 a constant value of 1
            // Result should be a passthrough lut
            const LutEntry result = RemoveSourceFromLut(le, 1, 1);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 0x3);
        }
    }

    {
        // XOR LUT
        LutEntry le = {};
        le._numSources = 2;
        le._table = 0x6; // output = src0 ^ src1

        {
            // give src0 a constant value of 0
            // Result should be a passthrough lut
            const LutEntry result = RemoveSourceFromLut(le, 0, 0);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 2);
        }

        {
            // give src0 a constant value of 1
            // Result should be an invert lut
            const LutEntry result = RemoveSourceFromLut(le, 0, 1);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 1);
        }

        {
            // give src1 a constant value of 0
            // Result should be a passthrough lut
            const LutEntry result = RemoveSourceFromLut(le, 1, 0);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 2);
        }

        {
            // give src1 a constant value of 1
            // Result should be an invert lut
            const LutEntry result = RemoveSourceFromLut(le, 1, 1);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 1);
        }
    }

    {
        LutEntry le = {};
        le._numSources = 2;
        le._table = 0xB; // output = src0 | ~src1

        {
            // give src0 a constant value of 0
            // Result should be an invert lut
            const LutEntry result = RemoveSourceFromLut(le, 0, 0);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 1);
        }

        {
            // give src0 a constant value of 1
            // Result should be a constant 1
            const LutEntry result = RemoveSourceFromLut(le, 0, 1);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 0x3);
        }

        {
            // give src1 a constant value of 0
            // Result should be a constant 1
            const LutEntry result = RemoveSourceFromLut(le, 1, 0);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 0x3);
        }

        {
            // give src1 a constant value of 1
            // Result should be a passthrough lut
            const LutEntry result = RemoveSourceFromLut(le, 1, 1);
            TestAssert(result._numSources == 1);
            TestAssert(result._table == 2);
        }
    }
}

void OptimizeLocalDataPropTest()
{
    const CodeGenDeviceConfig& config = GetCodeGenDeviceConfig();

    const auto computeCost = [&](const Propagations& registerPropagations, const FifoPropagations& fifoPropagations,
                                 const RegToWidth& regToWidth)
    {
        size_t result = 0;

        // Maps register index to associated fifo depth
        std::map<size_t, size_t> registerToFifoDepth;

        for (const auto& p : fifoPropagations)
        {
            const AnchorAndDepth& anchorAndDepth = p.first;

            const DropPoints& dropPoints = p.second;

            for (const auto& p2 : dropPoints)
            {
                for (const size_t registerIndex : p2.second)
                {
                    registerToFifoDepth[registerIndex] = anchorAndDepth.second;
                }
            }
        }

        // register costs
        for (const auto& p : registerPropagations)
        {
            const PropagationRange& registerRange = p.first;

            for (const size_t registerIndex : p.second)
            {
                const size_t width = regToWidth(registerIndex);

                // Determine if this register was assigned to a fifo
                const auto it = registerToFifoDepth.find(registerIndex);

                size_t registerCount = 0;

                if (it == registerToFifoDepth.end())
                {
                    // No fifo, all register costs
                    registerCount = (registerRange.second - registerRange.first);
                }
                else
                {
                    const size_t fifoDepth = it->second;

                    const size_t registerRangeDepth = registerRange.second - registerRange.first;

                    assert(registerRangeDepth >= fifoDepth);

                    // account for registers after the fifo
                    registerCount = (registerRangeDepth - fifoDepth);
                }

                registerCount *= width;

                result += (registerCount * config._fifoBitsPerRegister);
            }
        }

        // fifo costs
        for (const auto& p : fifoPropagations)
        {
            const AnchorAndDepth& anchorAndDepth = p.first;

            const DropPoints& dropPoints = p.second;

            size_t unalignedWidth = 0;

            for (const auto& p2 : dropPoints)
            {
                for (const size_t registerIndex : p2.second)
                {
                    unalignedWidth += regToWidth(registerIndex);
                }
            }

            const size_t alignedWidth = AlignNonPow2(unalignedWidth, config._fifoWidthAlignment);

            const size_t alignedDepth = AlignNonPow2(anchorAndDepth.second, config._fifoDepthAlignment);

            result += ((alignedWidth * alignedDepth) + config._fifoFixedCost);
        }

        return result;
    };

    for (size_t testCase = 0; testCase < 10; testCase++)
    {
        std::mt19937_64 rng(testCase);

        const size_t numRanges = testCase + 2;

        // generate random input propagations
        Propagations registerPropagations;

        // Maps register index to register width
        std::map<size_t, size_t> regWidth;

        for (size_t i = 0; i < numRanges; i++)
        {
            PropagationRange range = {};

            range.first = rng() % 10;
            range.second = range.first + 1 + (rng() % 10);

            SafeInsert(regWidth, i, static_cast<size_t>(1 + (rng() % 16)));

            registerPropagations[range].insert(i);
        }

        const auto registerWidthCallback = [&](const size_t i) { return SafeLookup(regWidth, i); };

        const FifoPropagations fifoPropagations = OptimizePropagations(registerPropagations, registerWidthCallback);

        const size_t optimizedCost = computeCost(registerPropagations, fifoPropagations, registerWidthCallback);

        // optimized cost should be no greater than cost associated with leaving everything in registers
        {
            const FifoPropagations emptyPropagations;

            const size_t allRegisterCost = computeCost(registerPropagations, emptyPropagations, registerWidthCallback);

            TestAssert(optimizedCost <= allRegisterCost);
        }

        // optimized cost should be no greater than cost associated with putting everything into a unique fifo
        {
            FifoPropagations allFifoPropagations;

            for (const auto& p : registerPropagations)
            {
                const PropagationRange& range = p.first;

                const AnchorAndDepth anchorAndDepth(range.first, range.second - range.first);

                DropPoints& dropPoints = allFifoPropagations[anchorAndDepth];

                for (const size_t r : p.second)
                {
                    dropPoints[range.first].insert(r);
                }
            }

            const size_t allFifoCost = computeCost(registerPropagations, allFifoPropagations, registerWidthCallback);

            TestAssert(optimizedCost <= allFifoCost);
        }

        // Verify that OptimizePropagations returns the same result on each iteration
        // (multi-threading does not repeatIndex non-determinism)
        for (size_t repeatIndex = 0; repeatIndex < 100; repeatIndex++)
        {
            const FifoPropagations fifoPropagations2 =
                OptimizePropagations(registerPropagations, registerWidthCallback);

            TestAssert(fifoPropagations2 == fifoPropagations);
        }
    }
}

void ThreadPoolAsyncTest()
{
    for (size_t count = 1; count < 2048; count *= 2)
    {
        std::atomic<size_t> callbackCount(0);

        {
            ThreadPool tp;

            for (size_t i = 0; i < count; i++)
            {
                tp.Async([&]() { callbackCount++; });
            }
        } // threadPool goes out of scope here
          // destructor will block until all work is done

        TestAssert(callbackCount == count);
    }
}

void ThreadPoolMapToListTest()
{
    ThreadPool tp;

    for (size_t count = 1; count < 2048; count += 137)
    {
        std::list<size_t> result;

        const auto callback = [&](std::list<size_t>& outputList, const size_t i) { outputList.push_back(i + 3); };

        tp.MapToList<size_t>(result, count, callback);

        TestAssert(count == result.size());

        std::set<size_t> finalSet;

        for (const size_t i : result)
        {
            finalSet.insert(i);
        }

        for (size_t i = 0; i < count; i++)
        {
            TestAssert(finalSet.end() != finalSet.find(i + 3));
        }

        // Verify the same output on each invocation
        // (Deterministic regardless of thread scheduling)
        for (size_t repeatIndex = 0; repeatIndex < 100; repeatIndex++)
        {
            std::list<size_t> result2;

            tp.MapToList<size_t>(result2, count, callback);

            TestAssert(result == result2);
        }
    }

    // Verify that exceptions thrown by threads are propagated back
    tp.Async([]() { throw std::runtime_error("Intentional failure"); });

    std::string errorString;

    try
    {
        tp.WaitForIdle();
    }
    catch (std::exception& e)
    {
        errorString = e.what();
    }

    TestAssert(errorString == "Intentional failure");
}

void SparseBinaryBranchAndBoundTest()
{
    for (size_t testCase = 0; testCase < 100; testCase++)
    {
        std::mt19937_64 rng(testCase);

        const size_t numParameters = (rng() % 100) + 1;

        std::vector<size_t> parameterRanges(numParameters);

        std::vector<std::vector<size_t>> weightVector(numParameters);

        for (size_t i = 0; i < numParameters; i++)
        {
            const size_t range = (rng() % 8) + 1;

            parameterRanges[i] = range;

            std::vector<size_t>& weights = weightVector[i];

            weights.resize(range);

            for (size_t j = 0; j < range; j++)
            {
                // all weights are positive
                weights[j] = (rng() % 32) + 1;
            }
        }

        // Objective function value is the sum of a set of terms
        // Each term a sum involving a set of parameter values
        const size_t numTerms = (rng() % 64) + 1;

        std::vector<std::set<size_t>> terms(numTerms);

        ParameterInteractionGraph interactions;

        for (size_t termIndex = 0; termIndex < numTerms; termIndex++)
        {
            std::set<size_t>& term = terms[termIndex];

            const size_t termSize = (rng() % 4) + 1;

            for (size_t i = 0; i < termSize; i++)
            {
                term.insert(rng() % numParameters);
            }

            // Record edge in the interaction graph
            const size_t firstParameter = *(term.begin());

            interactions[firstParameter] = term;
        }

        const auto getWeightBounds = [&](const size_t parameterIndex)
        {
            const std::vector<size_t>& weights = weightVector[parameterIndex];

            assert(!weights.empty());

            std::pair<size_t, size_t> result;

            result.first = weights[0];
            result.second = weights[0];

            for (const size_t weight : weights)
            {
                result.first = std::min(result.first, weight);
                result.second = std::max(result.second, weight);
            }

            return result;
        };

        const size_t maxSearchSpace = 1ull << 10;

        boost::optional<size_t> optimalValue;

        const auto boundCallback = [&](const IntegerParameterVector& parameters, const bool isOptimalPoint)
        {
            TestAssert(parameters.GetParameterCount() == numParameters);

            ObjectiveFunctionBounds result = {};

            for (size_t termIndex = 0; termIndex < numTerms; termIndex++)
            {
                const std::set<size_t>& term = terms[termIndex];

                size_t productLowerBound = 1;
                size_t productUpperBound = 1;

                for (const size_t parameterIndex : term)
                {
                    size_t weightLowerBound = 0;
                    size_t weightUpperBound = 0;

                    if (parameters.IsParameterDefined(parameterIndex))
                    {
                        const size_t parameterValue = parameters.GetParameter(parameterIndex);

                        const size_t weight = weightVector[parameterIndex][parameterValue];

                        weightLowerBound = weight;
                        weightUpperBound = weight;
                    }
                    else
                    {
                        const std::pair<size_t, size_t> weightBounds = getWeightBounds(parameterIndex);

                        weightLowerBound = weightBounds.first;
                        weightUpperBound = weightBounds.second;
                    }

                    TestAssert(weightLowerBound > 0);
                    TestAssert(weightUpperBound > 0);
                    TestAssert(weightUpperBound >= weightLowerBound);

                    productLowerBound *= weightLowerBound;
                    productUpperBound *= weightUpperBound;
                }

                result._lowerBound += productLowerBound;
                result._upperBound += productUpperBound;
            }

            if (isOptimalPoint)
            {
                TestAssert(result._lowerBound == result._upperBound);
                TestAssert(!optimalValue);

                optimalValue = result._lowerBound;
            }

            return result;
        };

        IntegerBranchAndBoundSparse(parameterRanges, interactions, maxSearchSpace, boundCallback);

        TestAssert(!!optimalValue);

        size_t expectedOptimalValue = 0;

        for (size_t termIndex = 0; termIndex < numTerms; termIndex++)
        {
            const std::set<size_t>& term = terms[termIndex];

            size_t product = 1;

            for (const size_t parameterIndex : term)
            {
                const size_t parameterValue = getWeightBounds(parameterIndex).first;

                product = product * parameterValue;
            }

            expectedOptimalValue += product;
        }

        TestAssert(*optimalValue == expectedOptimalValue);
    }
}

void BinaryBranchAndBoundTest()
{
    for (size_t parameterCount = 1; parameterCount < 256; parameterCount *= 2)
    {
        std::mt19937_64 rng(parameterCount);

        // Each parameter is associated with a set of possible values
        // Of each these values has an associated with

        // weightMaps[i] is for parameter index i
        // it maps parameter value to weight
        using WeightMap = std::map<size_t, size_t>;

        std::vector<WeightMap> weightMaps(parameterCount);

        std::vector<size_t> parameterRanges(parameterCount);

        for (size_t parameterIndex = 0; parameterIndex < parameterCount; parameterIndex++)
        {
            WeightMap& wm = weightMaps[parameterIndex];

            const size_t numValues = (rng() % 8) + 1;

            parameterRanges[parameterIndex] = numValues;

            for (size_t i = 0; i < numValues; i++)
            {
                const size_t weight = rng() % 256;

                SafeInsert(wm, i, weight);
            }
        }

        const auto GetMinWeight = [](const WeightMap& wm)
        {
            size_t result = std::numeric_limits<size_t>::max();

            for (const auto& p : wm)
            {
                result = std::min(result, p.second);
            }

            return result;
        };

        const auto GetMaxWeight = [](const WeightMap& wm)
        {
            size_t result = 0;

            for (const auto& p : wm)
            {
                result = std::max(result, p.second);
            }

            return result;
        };

        bool finalCallMade = false;

        const auto boundCallback = [&](const IntegerParameterVector& paramVector, const bool isFinalCall)
        {
            TestAssert(parameterCount == paramVector.GetParameterCount());

            ObjectiveFunctionBounds result = {};

            for (size_t i = 0; i < parameterCount; i++)
            {
                const WeightMap& wm = weightMaps[i];

                if (paramVector.IsParameterDefined(i))
                {
                    const size_t value = paramVector.GetParameter(i);

                    const size_t weight = SafeLookup(wm, value);

                    result._lowerBound += weight;

                    result._upperBound += weight;
                }
                else
                {
                    TestAssert(!isFinalCall);

                    result._lowerBound += GetMinWeight(wm);

                    result._upperBound += GetMaxWeight(wm);
                }
            }

            if (isFinalCall)
            {
                TestAssert(!finalCallMade);

                finalCallMade = true;

                for (size_t i = 0; i < parameterCount; i++)
                {
                    const WeightMap& wm = weightMaps[i];

                    const size_t value = paramVector.GetParameter(i);

                    // The value with the minimum weight should have been chosen
                    TestAssert(SafeLookup(wm, value) == GetMinWeight(wm));
                }
            }

            return result;
        };

        IntegerBranchAndBound(parameterRanges, boundCallback);

        TestAssert(finalCallMade);
    }
}

void CompressedIntegerVectorTest()
{
    for (size_t iteration = 0; iteration < 1000; iteration++)
    {
        std::mt19937_64 rng(iteration);

        const size_t numParams = (rng() % 100) + 1;

        std::vector<size_t> ranges(numParams);

        for (size_t& range : ranges)
        {
            range = (1ull << (rng() % 10)) + (rng() % 32) + 1;
        }

        // There is special handling for parameters with range = 1 (no storage needed)
        ranges[rng() % numParams] = 1;

        const CompressedIntegerVector::CodeBook codeBook = CompressedIntegerVector::CreateCodeBook(ranges);

        CompressedIntegerVector civ(codeBook);

        std::vector<size_t> reference(numParams);

        // initialize all values
        for (size_t i = 0; i < numParams; i++)
        {
            const size_t value = rng() % ranges[i];

            reference[i] = value;
            civ.Set(i, value);
        }

        for (size_t innerIteration = 0; innerIteration < 10; innerIteration++)
        {
            const size_t paramIndex = rng() % numParams;

            const size_t value = rng() % ranges[paramIndex];

            reference[paramIndex] = value;
            civ.Set(paramIndex, value);
        }

        for (size_t i = 0; i < numParams; i++)
        {
            const size_t expected = reference[i];
            const size_t actual = civ.Get(i);

            TestAssert(expected == actual);
        }
    }
}

void CheckReleaseAsserts()
{
#ifdef NDEBUG
    bool caughtException = false;

    try
    {
        assert(false);
    }
    catch (std::exception)
    {
        caughtException = true;
    }

    if (!caughtException)
    {
        throw std::runtime_error("assert(false) did not result in an exception");
    }
#endif
}

void LutOperationsProduceSameResultTest()
{
    // 2 operations are identical
    {
        Operation op = {};
        op._opcode = Opcode::Lut;

        op._flags._lut._numDestinationBits = 2;

        LutEntry lutEntries[2] = {
            {0, {}, {}, 1},          // constant 1
            {2, {0, 1}, {3, 2}, 0xb} // 2 source operands
        };

        op._flags._lut._lutEntries = lutEntries;

        op._src.push_back(SourceOperand(AccessedRegister{32}));
        op._src.push_back(SourceOperand(AccessedRegister{22}));

        TestAssert(LutOperationsProduceSameResult(op, op));
        TestAssert(OperationHash(op) == OperationHash(op));
    }

    // Permuted operand order inside of LutEntry
    {
        Operation op1 = {};
        op1._opcode = Opcode::Lut;

        op1._flags._lut._numDestinationBits = 2;

        LutEntry lutEntries1[2] = {
            {0, {}, {}, 0},          // constant 0
            {2, {0, 1}, {3, 2}, 0xb} // 2 source operands
        };

        op1._flags._lut._lutEntries = lutEntries1;

        op1._src.push_back(SourceOperand(AccessedRegister{32}));
        op1._src.push_back(SourceOperand(AccessedRegister{22}));

        Operation op2 = {};
        op2._opcode = Opcode::Lut;

        op2._flags._lut._numDestinationBits = 2;

        LutEntry lutEntries2[2] = {
            {0, {}, {}, 0},          // constant 0
            {2, {1, 0}, {2, 3}, 0xd} // source operands are flipped
        };

        op2._flags._lut._lutEntries = lutEntries2;

        op2._src.push_back(SourceOperand(AccessedRegister{32}));
        op2._src.push_back(SourceOperand(AccessedRegister{22}));

        TestAssert(LutOperationsProduceSameResult(op1, op2));
        TestAssert(OperationHash(op1) == OperationHash(op2));
    }

    // Permuted source operand order
    {
        Operation op1 = {};
        op1._opcode = Opcode::Lut;

        op1._flags._lut._numDestinationBits = 2;

        LutEntry lutEntries1[2] = {
            {0, {}, {}, 0},          // constant 0
            {2, {0, 1}, {3, 2}, 0xb} // 2 source operands
        };

        op1._flags._lut._lutEntries = lutEntries1;

        op1._src.push_back(SourceOperand(AccessedRegister{32}));
        op1._src.push_back(SourceOperand(AccessedRegister{22}));

        Operation op2 = {};
        op2._opcode = Opcode::Lut;

        op2._flags._lut._numDestinationBits = 2;

        LutEntry lutEntries2[2] = {
            {0, {}, {}, 0},          // constant 0
            {2, {1, 0}, {3, 2}, 0xb} // source operands are flipped
        };

        op2._flags._lut._lutEntries = lutEntries2;

        op2._src.push_back(SourceOperand(AccessedRegister{22}));
        op2._src.push_back(SourceOperand(AccessedRegister{32}));

        TestAssert(LutOperationsProduceSameResult(op1, op2));
        TestAssert(OperationHash(op1) == OperationHash(op2));
    }

    // Permuted operand order inside of LutEntry - but tables are different
    {
        Operation op1 = {};
        op1._opcode = Opcode::Lut;

        op1._flags._lut._numDestinationBits = 2;

        LutEntry lutEntries1[2] = {
            {0, {}, {}, 0},          // constant 0
            {2, {0, 1}, {3, 2}, 0xb} // 2 source operands
        };

        op1._flags._lut._lutEntries = lutEntries1;

        op1._src.push_back(SourceOperand(AccessedRegister{32}));
        op1._src.push_back(SourceOperand(AccessedRegister{22}));

        Operation op2 = {};
        op2._opcode = Opcode::Lut;

        op2._flags._lut._numDestinationBits = 2;

        LutEntry lutEntries2[2] = {
            {0, {}, {}, 0},          // constant 0
            {2, {1, 0}, {2, 3}, 0xf} // source operands are flipped
        };

        op2._flags._lut._lutEntries = lutEntries2;

        op2._src.push_back(SourceOperand(AccessedRegister{32}));
        op2._src.push_back(SourceOperand(AccessedRegister{22}));

        TestAssert(!LutOperationsProduceSameResult(op1, op2));
    }

    // Permuted operand order inside of LutEntry - but bit indices are different
    {
        Operation op1 = {};
        op1._opcode = Opcode::Lut;

        op1._flags._lut._numDestinationBits = 2;

        LutEntry lutEntries1[2] = {
            {0, {}, {}, 0},          // constant 0
            {2, {0, 1}, {3, 2}, 0xb} // 2 source operands
        };

        op1._flags._lut._lutEntries = lutEntries1;

        op1._src.push_back(SourceOperand(AccessedRegister{32}));
        op1._src.push_back(SourceOperand(AccessedRegister{22}));

        Operation op2 = {};
        op2._opcode = Opcode::Lut;

        op2._flags._lut._numDestinationBits = 2;

        LutEntry lutEntries2[2] = {
            {0, {}, {}, 0},          // constant 0
            {2, {1, 0}, {2, 4}, 0xd} // source operands are flipped
        };

        op2._flags._lut._lutEntries = lutEntries2;

        op2._src.push_back(SourceOperand(AccessedRegister{32}));
        op2._src.push_back(SourceOperand(AccessedRegister{22}));

        TestAssert(!LutOperationsProduceSameResult(op1, op2));
    }
}

void ArrayTypeStringTest()
{
    Location loc = {};

    const Type* const u8Type = g_compiler->GetLeafType(BaseType::Uint, 8, loc);

    // uint8[4]
    const Type* const u8_4 =
        g_compiler->GetArrayType(u8Type, 4, ParseTreeArrayTypeDefault, ParseTreeMemoryTypeDefault, nullptr, false, loc);

    TestAssert("uint8[4]" == u8_4->GetName());

    // memory<uint8, 4>
    const Type* const mem_u8_4 =
        g_compiler->GetArrayType(u8Type, 4, ParseTreeArrayTypeMemory, ParseTreeMemoryTypeDefault, nullptr, false, loc);

    TestAssert("[[memory]] uint8[4]" == mem_u8_4->GetName());

    // uint8[16][4]
    const Type* const u8_16_4 =
        g_compiler->GetArrayType(u8_4, 16, ParseTreeArrayTypeDefault, ParseTreeMemoryTypeDefault, nullptr, false, loc);

    TestAssert("uint8[16][4]" == u8_16_4->GetName());

    // memory<uint8[4], 16>
    const Type* const mem_u8_16_4 =
        g_compiler->GetArrayType(u8_4, 16, ParseTreeArrayTypeMemory, ParseTreeMemoryTypeDefault, nullptr, false, loc);

    TestAssert("[[memory]] (uint8[4])[16]" == mem_u8_16_4->GetName());

    // memory<uint8, 4>[32]
    const Type* const mem_u8_32_4 = g_compiler->GetArrayType(mem_u8_4, 32, ParseTreeArrayTypeDefault,
                                                             ParseTreeMemoryTypeDefault, nullptr, false, loc);

    TestAssert("([[memory]] uint8[4])[32]" == mem_u8_32_4->GetName());

    // memory<uint8, 4>[64][32]
    const Type* const mem_u8_64_32_4 = g_compiler->GetArrayType(mem_u8_32_4, 64, ParseTreeArrayTypeDefault,
                                                                ParseTreeMemoryTypeDefault, nullptr, false, loc);

    TestAssert("([[memory]] uint8[4])[64][32]" == mem_u8_64_32_4->GetName());
}

int InternalTests()
{
    int result = -1;

    // Don't throw exception out of 'extern "C"' function
    // because by default the compiler assumes that this cannot happen
    // The compiler can optimize the caller code and to remove statements
    // that only execute if an exception is thrown
    try
    {
        std::cout << "Start internal tests\n";

        const char* fileNames[] = {"file1.k", nullptr};

        Options o = {};
        o._cmdArgs = "";
        o._deviceName = "InternalTestDevice";
        o._fileNames = fileNames;
        o._codegenOptions._releaseAssert = true;
        o._codegenOptions._logicRegisterRatio = 1;
        o._codegenOptions._maxLogicRegisterRatio = 1;
        o._codegenOptions._carryChainWidthPerLogicLevel = 10;
        o._codegenOptions._fifoMergeDistance = 4;
        o._codegenOptions._logWorkListSize = 10;
        o._codegenOptions._maxSelectInputs = 2;
        o._codegenOptions._maxThreadsDefault = 512;
        o._codegenOptions._maxThreadsLimit = 512;

        SetupCodeGenConfig(o);

        SetTestCodeGenDeviceConfig();

        InitCompiler(&o);

        ArrayTypeStringTest();

        FixupLutTest();

        LutOperationsProduceSameResultTest();

        OperationLocationSpanBbTest();

        PrettyToIdentifierTest();

        CompressedIntegerVectorTest();

        BinaryBranchAndBoundTest();

        SparseBinaryBranchAndBoundTest();

        ThreadPoolAsyncTest();

        ThreadPoolMapToListTest();

        OptimizeLocalDataPropTest();

        // Validate that assert() causes an exception to be throw on release builds
        CheckReleaseAsserts();

        ImplementBinaryOp_DivModTest(ParseTreeBinaryOpTypeDiv);

        ImplementBinaryOp_DivModTest(ParseTreeBinaryOpTypeMod);

        ImplementBinaryOp_ShrSignedTest();

        RemoveSourceFromLutTest();

        FixupStringTest();

        ComputeFifoSizeTest();

        GetOpPathLengthTest();

        DetectConstantLutTest();

        FixupLutOpTest();

        GetRaceCounterDepthTest();

        PlacementSortPlaceholderTest();

        PlacementSortTest();

        PlacementVec2Test();

        PlacementConnectionTest();

        PlacementBoundingBoxTest();

        PlacementWallTest();

        SymbolLookupTest();

        BinaryOpTest();

        UnaryOpTest();

        Log2RoundUpTest();

        GetIntegerBitCountTest();

        Log2Test();

        PathTest();

        std::cout << "Finished internal tests\n";

        result = 0;
    }
    catch (std::exception& e)
    {
        std::cout << "Exception: " << e.what() << "\n";
    }

    return result;
}
