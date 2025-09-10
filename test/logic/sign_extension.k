// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

class TestGlobalAssignment
{
private:
    int32 _x;

public:
    void run(unit::tag_t tag)
    {
        int16 y = -3;
        _x = y;

        unit::assert(tag, _x == -3);
    }
}

// Sign-extension on wide data(int64, int130)
inline void CompareOpsInt64Int130(int64 a, int130 b, bool expectedLT, bool expectedLE, bool expectedEQ, bool expectedNE, bool expectedGT, bool expectedGE, unit::tag_t tag)
{
    bool lt = (a < b);
    bool le = (a <= b);

    bool eq = (a == b);
    bool ne = (a != b);

    bool gt = (a > b);
    bool ge = (a >= b);

    unit::assert_equal(tag, expectedLT, lt);
    unit::assert_equal(tag, expectedLE, le);

    unit::assert_equal(tag, expectedEQ, eq);
    unit::assert_equal(tag, expectedNE, ne);

    unit::assert_equal(tag, expectedGT, gt);
    unit::assert_equal(tag, expectedGE, ge);
}

inline void TestFunctionCompareOpsInt64Int130(unit::tag_t tag)
{
    CompareOpsInt64Int130(0, 0, false, true, true, false, false, true, tag);

    CompareOpsInt64Int130(1, 0, false, false, false, true, true, true, tag);

    CompareOpsInt64Int130(7, -15, false, false, false, true, true, true, tag);

    const int64 a = -(1 << 63);
    CompareOpsInt64Int130(a,  1, true, true, false, true, false, false, tag);

    const int130 b = (1 << 129) - 1;
    CompareOpsInt64Int130(-8, b, true, true, false, true, false, false, tag);

    CompareOpsInt64Int130(-1, 1, true, true, false, true, false, false, tag);
}

// Sign-extension on wide data(uint130, int64)
inline void CompareOpsUint130Int64(uint130 a, int64 b, bool expectedLT, bool expectedLE, bool expectedEQ, bool expectedNE, bool expectedGT, bool expectedGE, unit::tag_t tag)
{
    bool lt = (a < b);
    bool le = (a <= b);

    bool eq = (a == b);
    bool ne = (a != b);

    bool gt = (a > b);
    bool ge = (a >= b);

    unit::assert_equal(tag, expectedLT, lt);
    unit::assert_equal(tag, expectedLE, le);

    unit::assert_equal(tag, expectedEQ, eq);
    unit::assert_equal(tag, expectedNE, ne);

    unit::assert_equal(tag, expectedGT, gt);
    unit::assert_equal(tag, expectedGE, ge);
}

inline void TestFunctionCompareOpsUint130Int64(unit::tag_t tag)
{
    CompareOpsUint130Int64(0, 0, false, true, true, false, false, true, tag);

    CompareOpsUint130Int64(1, 0, false, false, false, true, true, true, tag);

    const uint130 a = (1 << 130) - 1;
    CompareOpsUint130Int64(a, -15, false, false, false, true, true, true, tag);

    const int64 b = -(1 << 63);
    CompareOpsUint130Int64(a,  b, false, false, false, true, true, true, tag);

    CompareOpsUint130Int64(0, b, false, false, false, true, true, true, tag);
}


inline void test_main()
{
    print("Test CompareOpsInt64Int130\n");
    unit::test<1>(TestFunctionCompareOpsInt64Int130);

    print("Test CompareOpsUint130Int64\n");
    unit::test<2>(TestFunctionCompareOpsUint130Int64);

    print("Test GlobalAssignment\n");
    unit::test<3>(unit::fixture<TestGlobalAssignment>());
}
