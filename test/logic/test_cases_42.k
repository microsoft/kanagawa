// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// array read with signed index - sign extension
class Test726
{
private:
    uint32 Helper726(int2 idx)
    {
        uint32[7] data;

        static for (const auto i : 7)
        {
            data[i] = i + 1;
        }

        return data[idx];
    }

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 0 == Helper726(-1)); // should be the same as 7
        unit::assert(tag, 7 == Helper726(-2)); // should be the same as 6
    }
}

// array write with signed index - sign extension
class Test727
{
private:
    uint32[8] g_data727;

public:
    void Helper727(int2 idx, uint32 data)
    {
        g_data727[idx] = data;
    }

    void run(unit::tag_t tag)
    {
        static for (const auto i : 7)
        {
            g_data727[i] = i;
        }

        Helper727(-1, 123); // this write should be dropped
        Helper727(-2, 456); // this write should be dropped

        static for (const auto i : 7)
        {
            unit::assert(tag, g_data727[i] == i);
        }
    }
}

class Test728
{
private:
    // Shift with signed right-hand side
    int32 LogicalShiftRight(uint32 a, int5 b)
    {
        return a >> b;
    }

    int32 ArithmeticShiftRight(int32 a, int5 b)
    {
        return a >> b;
    }

    uint64 ShiftLeft(uint32 a, int5 b)
    {
        return a << b;
    }

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, LogicalShiftRight(8, 1) == 4);
        unit::assert(tag, LogicalShiftRight(0xdeadbeef, -10) == 0x37A); // -10 should be interpreted as 22

        unit::assert(tag, ArithmeticShiftRight(8, 1) == 4);
        unit::assert(tag, cast<uint32>(ArithmeticShiftRight(0xdeadbeef, -21)) == 0xFFFBD5B7); // should be interpreted as 11

        unit::assert(tag, ShiftLeft(8, 1) == 16);
        unit::assert(tag, ShiftLeft(8, -1) == 0x400000000); // -1 should be interpreted as 31
    }
}

// clog2 with signed integer
inline void TestCase729(unit::tag_t tag)
{
    unit::assert(tag, 3 == clog2(16-8));
}

// static for signed integer
inline void TestCase730(unit::tag_t tag)
{
    uint32 x = 0;

    static for (const auto i : (4-2))
    {
        x++;
    }

    unit::assert(tag, x == 2);
}

// Tests result type from: uint<a> - uint<b>
// Both operands are first converted to signed: int<a+1> - int<b+1>
// Then the subtraction occurs, resulting in: int<max(a,b)+1)>
// This is important to uint<a> - uint<b> operations where the result can be negative
// If the result is left as an unsinged integer: uint<max(a,b)+1> then it will be improperly
// sign extended when used in other operations
inline void Helper731(uint32 stage_num, uint32 logNB, uint32 log2n, unit::tag_t tag)
{
    const auto resultA = stage_num + logNB - log2n;
    const auto resultB = stage_num - log2n + logNB;

    print("{resultA} {resultB}\n");

    unit::assert(tag, resultA == resultB);
}

inline void Test731(unit::tag_t tag)
{
    Helper731(4, 2, 5, tag);
}


inline void test_main()
{
    unit::test<726>(unit::fixture<Test726>());
    unit::test<727>(unit::fixture<Test727>());
    unit::test<728>(unit::fixture<Test728>());
    unit::test<729>(TestCase729);
    unit::test<730>(TestCase730);
    unit::test<731>(Test731);
}
