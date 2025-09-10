// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.callbacks
    { CallbackTester
    , EqualTo3
    }

import test.unit as unit

inline bool EqualTo3(uint32 x)
{
    return x == 3;
}

class CallbackTester
{
    (uint32)->bool callback = GreaterThan5;

    inline bool GreaterThan5(uint32 x)
    {
        return x > 5;
    }

public:
    void Test(unit::tag_t tag)
    {
        unit::assert(tag, !callback(4));
        unit::assert(tag, !callback(5));
        unit::assert(tag, callback(6));
    }

    inline void TestValue(unit::tag_t tag, uint32 value, bool expected)
    {
        unit::assert_equal(tag, expected, callback(value));
    }
}