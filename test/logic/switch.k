// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

inline void SwitchWithOnlyDefault(unit::tag_t tag)
{
    uint32 x = 3;

    switch (x)
    {
    default:
        x = 4;
        break;
    }

    unit::assert_equal(tag, 4, x);
}

inline void EmptySwitch(unit::tag_t tag)
{
    uint32 x = 3;

    switch (x)
    {
    }

    unit::assert_equal(tag, 3, x);
}

inline void test_main()
{
    unit::test<1>(SwitchWithOnlyDefault);
    unit::test<2>(EmptySwitch);
}
