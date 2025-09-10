// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.counter
import test.runner
import test.unit as unit

class CounterTests
{
private:
    counter<16, 10> _counter;

public:
    void run(unit::tag_t tag)
    {
        unit::assert_equal(tag, 10, _counter.count());

        _counter.decrement();
        unit::assert_equal(tag, 9, _counter.count());

        unit::assert(tag, !_counter.is_zero());

        // Subtracting 10 and adding 1 should bring the count to zero.
        _counter.subtract(10);
        _counter.increment();
        unit::assert_equal(tag, 0, _counter.count());
        unit::assert(tag, _counter.is_zero());

        atomic
        {
            // Using an atomic block here just to ensure that they can be called from an
            // atomic block.
            _counter.add(2);
            unit::assert_equal(tag, 2, _counter.count());
        }

        _counter.reset();
        unit::assert_equal(tag, 10, _counter.count());
    }
}

// Function: Main test entry point for unit tests of CHL counter class.
inline void test_main()
{
    unit::test<1>(unit::fixture<CounterTests>());
}
