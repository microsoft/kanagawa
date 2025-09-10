// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// 2-dimensional array
inline void TestCase182(unit::tag_t tag)
{
    // 4 rows, 2 columns
    uint32[4][2] a;

    a[0][0] = 1;
    a[0][1] = 2;

    a[1][0] = 3;
    a[1][1] = 4;

    a[2][0] = 5;
    a[2][1] = 6;

    a[3][0] = 7;
    a[3][1] = 8;

    unit::assert(tag, a[0][0] == 1);
    unit::assert(tag, a[0][1] == 2);

    unit::assert(tag, a[1][0] == 3);
    unit::assert(tag, a[1][1] == 4);

    unit::assert(tag, a[2][0] == 5);
    unit::assert(tag, a[2][1] == 6);

    unit::assert(tag, a[3][0] == 7);
    unit::assert(tag, a[3][1] == 8);
}

// logical not operator
inline void TestCase183(unit::tag_t tag)
{
    bool b = true;

    bool c = !b;

    unit::assert(tag, c == false);
}

// logical AND operator
inline void TestCase184(unit::tag_t tag)
{
    bool b = true;

    bool c = false;

    unit::assert(tag, (b && c) == false);

    c = true;

    unit::assert(tag, (b && c) == true);
}

// logical OR operator
inline void TestCase185(unit::tag_t tag)
{
    bool b = false;

    bool c = false;

    unit::assert(tag, (b || c) == false);

    c = true;

    unit::assert(tag, (b || c) == true);
}

// logical XOR operator
inline void TestCase186(unit::tag_t tag)
{
    bool b = false;

    bool c = false;

    unit::assert(tag, (b ^^ c) == false);

    c = true;

    unit::assert(tag, (b ^^ c) == true);

    b = true;

    unit::assert(tag, (b ^^ c) == false);
}

// Logical XOR/OR precedence
inline void TestCase187(unit::tag_t tag)
{
    unit::assert(tag, (true || false ^^ true) == true);
}

// Logical XOR/AND precedence
inline void TestCase188(unit::tag_t tag)
{
    unit::assert(tag, (false && true ^^ true) == true);
}

// Logical OR/NOT precedence
inline void TestCase189(unit::tag_t tag)
{
    unit::assert(tag, (!false || true) == true);
}

inline void test_main()
{
    unit::test<182>(TestCase182);
    unit::test<183>(TestCase183);
    unit::test<184>(TestCase184);
    unit::test<185>(TestCase185);
    unit::test<186>(TestCase186);
    unit::test<187>(TestCase187);
    unit::test<188>(TestCase188);
    unit::test<189>(TestCase189);
}
