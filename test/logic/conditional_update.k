// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Tests implementation of writes to local variables under predicated control flow
inline void If(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 result = tid;

        if (0 == (tid % 2))
        {
            result = tid + 4;
        }

        uint32[8] expected_values = { 4, 1, 6, 3, 8, 5, 10, 7 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

inline void IfElse(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 result = tid;

        if (0 == (tid % 2))
        {
            result = tid + 4;
        }
        else
        {
            result = tid + 2;
        }

        uint32[8] expected_values = { 4, 3, 6, 5, 8, 7, 10, 9 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

// if-else ladder without final else
inline void DanglingIfElseLadder(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 result = tid;

        if (0 == (tid % 2))
        {
            result = tid + 4;
        }
        else if (3 == tid)
        {
            result = 101;
        }
        else if (5 == tid)
        {
            result = 102;
        }

        uint32[8] expected_values = { 4, 1, 6, 101, 8, 102, 10, 7 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

// if-else ladder with final else
inline void CompleteIfElseLadder(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 result = tid;

        if (0 == (tid % 2))
        {
            result = tid + 4;
        }
        else if (3 == tid)
        {
            result = 101;
        }
        else if (5 == tid)
        {
            result = 102;
        }
        else
        {
            result = tid + 1;
        }

        uint32[8] expected_values = { 4, 2, 6, 101, 8, 102, 10, 8 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

// if-else ladder with a constexpr condition in the middle
inline void ConstExprIfElseLadder(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        const bool b = true;

        uint32 result = tid;

        if (0 == (tid % 2))
        {
            result = tid + 4;
        }
        else if (3 == tid)
        {
            result = 101;
        }
        else if (b)
        {
            result = 15;
        }
        else
        {
            result = tid + 1;
        }

        uint32[8] expected_values = { 4, 15, 6, 101, 8, 15, 10, 15 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

// nesting
inline void NestedIfElse(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 result = tid;

        if (0 == (tid % 2))
        {
            if (tid == 2)
            {
                result = tid + 10;
            }
            else
            {
                result = tid + 6;
            }
        }
        else
        {
            if (tid == 1)
            {
                result = 101;
            }
            else
            {
                result = 104;
            }
        }

        uint32[8] expected_values = { 6, 101, 12, 104, 10, 104, 12, 104 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

inline void IncompleteSwitch(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 result = tid;

        switch (tid)
        {
        case 0:
            result = tid + 33;
            break;

        case 1:
            result = 5;
            break;

        case 5:
            result = 1;
            break;
        }

        uint32[8] expected_values = { 33, 5, 2, 3, 4, 1, 6, 7 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

inline void SwitchWithDefault(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 result = tid;

        switch (tid)
        {
        case 0:
            result = 4;
            break;

        case 5:
            result = 7;
            break;

        default:
            result = tid + 10;
            break;
        }

        uint32[8] expected_values = { 4, 11, 12, 13, 14, 7, 16, 17 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

inline void CompleteSwitch(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint3 tid)
    {
        uint32 result = tid;

        switch (tid)
        {
        case 0: result = 4; break;
        case 1: result = 1; break;
        case 2: result = 7; break;
        case 3: result = 2; break;
        case 4: result = 6; break;
        case 5: result = 8; break;
        case 6: result = 9; break;
        case 7: result = 11; break;
        }

        uint32[8] expected_values = { 4, 1, 7, 2, 6, 8, 9, 11 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

inline void SwitchInsideOfIf(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint3 tid)
    {
        uint32 result = tid;

        if (tid < 4)
        {
            switch (tid)
            {
            case 0: result = 21; break;
            case 1: result = 1; break;
            case 2: result = 7; break;
            case 3: result = 2; break;
            }
        }

        uint32[8] expected_values = { 21, 1, 7, 2, 4, 5, 6, 7 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

inline void DynamicArrayWrite(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint3 tid)
    {
        uint32[8] ary = { 0, 1, 2, 3, 4, 5, 6, 7 };

        ary[tid] = tid + 100;

        static for (const auto i : 8)
        {
            uint32 result = ary[i];

            uint32 expected = (i == tid) ? (tid + 100) : i;

            unit::assert(tag, result == expected);
        }
    });
}

inline void LoopPredicate(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint3 tid)
    {
        uint32 result = 7;

        // The case where tid == 0 requires predication
        // A trip will be taken through the loop
        // but all execution will be predicated
        for (const auto i : tid)
        {
            result = i;
        }

        uint32 expected = static_cast((tid == 0) ? 7 : (tid - 1));

        unit::assert(tag, result == expected);
    });
}

inline void ConditionInAndOutOfAtomic(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint3 tid)
    {
        uint32 result = 101;

        if (0 == (tid % 2))
        {
            atomic
            {
                static uint32 _count = 0;

                result = _count;

                _count++;
            }
        }

        uint32[8] expected_values = { 0, 101, 1, 101, 2, 101, 3, 101 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

// Increment of loop induction variable
// should be properly predicated
inline void NestedForLoop(unit::tag_t tag)
{
    uint32 result = 0;

    for (const uint32 i : 5)
    {
        for (const uint32 j : 4)
        {
            result += 2;
        }
    }

    unit::assert(tag, result == 40);
}


struct accumulator
{
    uint4[4]  nibbles;
}
struct acc_utf_32
{
    uint16  w0;
}

union test_union
{
    accumulator             acc;
    acc_utf_32              utf_32;
}

// Ensures that predicate is implemented for dynamic array writes to unions
inline void DynamicWriteToArrayOfUnions(unit::tag_t tag)
{
    pipelined_for(4, [tag](uint2 tid)
    {
        static test_union _test_union = {};

        test_union tu;

        atomic
        {
            tu = _test_union;

            tu.acc.nibbles[tid] = tid + 1;

            _test_union = tu;
        }

        const uint16[4] expected_values = { 0x1, 0x21, 0x321, 0x4321 };

        const uint16 expected = expected_values[tid];

        const uint16 actual = tu.utf_32.w0;

        unit::assert(tag, actual == expected);
    });
}

// Optimization that detects when
// ConditionalIgnore can not be inserted
// because all source operands are conditionally ignored
inline void TransitiveConditionalIgnore(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 result = tid;

        if (0 == (tid % 2))
        {
            result = ((tid * 9) ^ tid) + tid;
        }

        uint32[8] expected_values = { 0, 1, 0x12, 3, 0x24, 5, 0x36, 7 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

// Memory read in condition
// Memory will be converted to array by the compiler
inline void ConditionalMem2ArrayRead(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        static memory<uint8, 32> _mem;

        _mem[tid] = tid + 100;

        uint8 result = 0;

        if (0 == (tid % 2))
        {
            result = _mem[tid];
        }

        uint8[8] expected_values = { 100, 0, 102, 0, 104, 0, 106, 0 };
        uint8 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

// atomic do inside of control flow
// operations before the atomic do
// are moved into the start condition operations
class WaitForInControlCase
{
private:
    bool g_waitForFlag = false;
    uint32 g_waitForResult = 0;

public:
    void run(unit::tag_t tag)
    {
        bool do_wait = true;

        async_exec([do_wait]()
        {
            if (do_wait)
            {
                uint32 result = 4;

                atomic do; while(!g_waitForFlag);

                g_waitForResult = result;
            }
        });


        g_waitForFlag = true;

        atomic do; while(g_waitForResult != 4);
    }
}

// predicate computed in a different basic block than the select that uses it
inline void PredicateComputedEarly(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        static bool _flag = false;

        _flag = true;

        uint32 result = tid;

        uint32 tid_plus_four = tid + 4;

        if (0 == (tid % 2))
        {
            atomic do; while(!_flag);
            result = tid_plus_four;
        }

        uint32[8] expected_values = { 4, 1, 6, 3, 8, 5, 10, 7 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

inline void Mem2ArrayRead(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        static memory<uint8, 32> _mem;

        _mem[tid] = tid + 100;

        uint8 result = _mem[tid];

        uint8[8] expected_values = { 100, 101, 102, 103, 104, 105, 106, 107 };
        uint8 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}

// Copy propgation with control gating
inline void DuplicateVariable(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        uint32 result = tid;

        if (0 == (tid % 2))
        {
            // result_copy is gated, and live for 4 stages
            // result is also live for those same stages
            // so result_copy is just consuming area without saving power
            auto result_copy = result;
            stages<4>();

            result = result_copy + 4;
        }

        uint32[8] expected_values = { 4, 1, 6, 3, 8, 5, 10, 7 };
        uint32 expected = expected_values[tid];

        unit::assert(tag, result == expected);
    });
}


inline void test_main()
{
    print("If\n");
    unit::test<1>(If);

    print("IfElse\n");
    unit::test<2>(IfElse);

    print("DanglingIfElseLadder\n");
    unit::test<3>(DanglingIfElseLadder);

    print("CompleteIfElseLadder\n");
    unit::test<4>(CompleteIfElseLadder);

    print("ConstExprIfElseLadder\n");
    unit::test<5>(ConstExprIfElseLadder);

    print("NestedIfElse\n");
    unit::test<6>(NestedIfElse);

    print("IncompleteSwitch\n");
    unit::test<7>(IncompleteSwitch);

    print("SwitchWithDefault\n");
    unit::test<8>(SwitchWithDefault);

    print("CompleteSwitch\n");
    unit::test<9>(CompleteSwitch);

    print("SwitchInsideOfIf\n");
    unit::test<10>(SwitchInsideOfIf);

    print("DynamicArrayWrite\n");
    unit::test<11>(DynamicArrayWrite);

    print("LoopPredicate\n");
    unit::test<12>(LoopPredicate);

    print("ConditionInAndOutOfAtomic\n");
    unit::test<13>(ConditionInAndOutOfAtomic);

    print("NestedForLoop\n");
    unit::test<14>(NestedForLoop);

    print("DynamicWriteToArrayOfUnions\n");
    unit::test<15>(DynamicWriteToArrayOfUnions);

    print("TransitiveConditionalIgnore\n");
    unit::test<16>(TransitiveConditionalIgnore);

    print("ConditionalMem2ArrayRead\n");
    unit::test<17>(ConditionalMem2ArrayRead);

    print("WaitForInControlCase\n");
    unit::test<18>(unit::fixture<WaitForInControlCase>());

    print("PredicateComputedEarly\n");
    unit::test<19>(PredicateComputedEarly);

    print("Mem2ArrayRead\n");
    unit::test<20>(Mem2ArrayRead);

    print("DuplicateVariable\n");
    unit::test<21>(DuplicateVariable);
}
