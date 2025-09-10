// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.counter.saturating
import numeric.int.limits
import test.runner
import test.unit as unit

class SaturatingTest
{
private:
    saturating_counter<8> _ctr;

    bool TestInitiallyZero()
    {
        bool result = true;

        if (_ctr.get() != 0)
        {
            print("Error: counter initial value was not zero\n");
            result = false;
        }

        return result;
    }

    bool TestIncBy1()
    {
        bool result = true;

        uint8 before = _ctr.get();
        uint8 inc_ret = _ctr.increment(1, false);
        uint8 after = _ctr.get();

        if (inc_ret != before)
        {
            print("Error: increment did not return the value before the increment\n");
            result = false;
        }
        else if (after != before + 1)
        {
            print("Error: counter value was not incremented by 1\n");
            result = false;
        }

        return result;
    }

    bool TestIncBy2()
    {
        bool result = true;

        uint8 before = _ctr.get();
        uint8 inc_ret = _ctr.increment(2, false);
        uint8 after = _ctr.get();

        if (inc_ret != before)
        {
            print("Error: increment did not return the value before the increment\n");
            result = false;
        }
        else if (after != before + 2)
        {
            print("Error: counter value was not incremented by 2\n");
            result = false;
        }

        return result;
    }

    bool TestSaturate()
    {
        bool result = true;

        uint8 before = _ctr.get();
        uint8 inc_amount = checked_cast(limits<uint8>::max - before + 1);
        _ctr.increment(inc_amount, false);
        uint8 after = _ctr.get();

        if (after != limits<uint8>::max)
        {
            print("Error: counter value was not saturated\n");
            result = false;
        }

        return result;
    }

    bool TestIncrementWithClear()
    {
        bool result = true;

        uint8 before = _ctr.get();
        uint8 inc_ret = _ctr.increment(3, true);
        uint8 after = _ctr.get();

        if (inc_ret != before)
        {
            print("Error: when calling increment with clear enabled, counter did not return value before clear\n");
            result = false;
        }
        else if (after != 3)
        {
            print("Error: increment with clear enabled did not correctly clear the value to inc_amount\n");
            result = false;
        }

        return result;
    }

public:
    inline void Run()
    {
        unit::check<1>(TestInitiallyZero);
        unit::check<2>(TestIncBy1);
        unit::check<3>(TestIncBy2);
        unit::check<4>(TestSaturate);
        unit::check<5>(TestIncrementWithClear);
    }
}

inline void test_main()
{
    static SaturatingTest _test;

    _test.Run();
}
