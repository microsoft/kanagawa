// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// div/mod where the lhs is negative and not known at compile time
inline int32 DivHelper(int32 numerator)
{
    return numerator / 4;
}

inline int32 ModHelper(int32 numerator)
{
    return numerator % 4;
}

inline void Test732(unit::tag_t tag)
{
    unit::assert(tag, 0 == DivHelper(0));
    unit::assert(tag, 0 == DivHelper(1));
    unit::assert(tag, 0 == DivHelper(2));
    unit::assert(tag, 0 == DivHelper(3));
    unit::assert(tag, 1 == DivHelper(4));
    unit::assert(tag, 1 == DivHelper(5));
    unit::assert(tag, 1 == DivHelper(6));
    unit::assert(tag, 1 == DivHelper(7));

    unit::assert(tag, 0 == DivHelper(-1));
    unit::assert(tag, 0 == DivHelper(-2));
    unit::assert(tag, 0 == DivHelper(-3));
    unit::assert(tag, -1 == DivHelper(-4));
    unit::assert(tag, -1 == DivHelper(-5));
    unit::assert(tag, -1 == DivHelper(-6));
    unit::assert(tag, -1 == DivHelper(-7));

    unit::assert(tag, 0 == ModHelper(0));
    unit::assert(tag, 1 == ModHelper(1));
    unit::assert(tag, 2 == ModHelper(2));
    unit::assert(tag, 3 == ModHelper(3));
    unit::assert(tag, 0 == ModHelper(4));
    unit::assert(tag, 1 == ModHelper(5));
    unit::assert(tag, 2 == ModHelper(6));
    unit::assert(tag, 3 == ModHelper(7));

    unit::assert(tag, -1 == ModHelper(-1));
    unit::assert(tag, -2 == ModHelper(-2));
    unit::assert(tag, -3 == ModHelper(-3));
    unit::assert(tag, 0  == ModHelper(-4));
    unit::assert(tag, -1 == ModHelper(-5));
    unit::assert(tag, -2 == ModHelper(-6));
    unit::assert(tag, -3 == ModHelper(-7));
}

// div/mod where the lhs is negative and is known at compile time
inline void TestCase733(unit::tag_t tag)
{
    unit::assert(tag, 0  == (-1 / 2));
    unit::assert(tag, -1 == (-2 / 2));
    unit::assert(tag, -1 == (-3 / 2));
    unit::assert(tag, -2 == (-4 / 2));
    unit::assert(tag, -2 == (-5 / 2));

    unit::assert(tag, -1 == (-1 % 3));
    unit::assert(tag, -2 == (-2 % 3));
    unit::assert(tag, 0  == (-3 % 3));
    unit::assert(tag, -1 == (-4 % 3));
    unit::assert(tag, -2 == (-5 % 3));
    unit::assert(tag, 0  == (-6 % 3));
}


inline void test_main()
{
    unit::test<732>(Test732);
    unit::test<733>(TestCase733);
}
