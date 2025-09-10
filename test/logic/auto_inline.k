// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

class AutoInlineConstParamTest
{
private:
    void AutoInlineConstParamInner(unit::tag_t tag, uint32 x)
    {
        unit::assert_equal(tag, 4, x);
    }

    // There are multiple call sites of this function
    // however, only 1 call site has b == true
    // So the call to AutoInlineConstParamInner can be inlined
    inline void AutoInlineConstParamMiddle(unit::tag_t tag, const bool b)
    {
        // This will be evaluated at compile time
        // resulting in only 1 call to AutoInlineConstParamInner
        if (b)
        {
            AutoInlineConstParamInner(tag, 4);
        }
    }

    // Outer layer to ensure that known parameter values can
    // be passed through a call stack
    inline void AutoInlineConstParamOuter(unit::tag_t tag, const bool b)
    {
        AutoInlineConstParamMiddle(tag, b);
    }

public:
    void run(unit::tag_t tag)
    {
        AutoInlineConstParamOuter(tag, true);
        AutoInlineConstParamOuter(tag, false);
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<AutoInlineConstParamTest>());
}
