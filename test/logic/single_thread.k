// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import helper.sleep
import data.function
import test.unit as unit
import test.runner

// Synchronous call inside of single-threaded function
// Should use register fifos
class TestCase447
{
    [[max_threads(1)]] void Go()
    {
        sleep<512>();
    }

public:
    void run(unit::tag_t tag)
    {
        Go();
    }
}

class Test449
{
public:
    // Backpressure when a single-threaded function calls another function
    uint32 _ctr449 = 0;

    [[async]] void Bar449()
    {
        [[unordered]] for (const uint32 i : 100)
        {
            atomic
            {
                _ctr449++;
            }
        }
    }

    // Single-threaded function will use unregistered output from memory
    [[max_threads(1), pipelined]] void Foo449(uint32 idx)
    {
        Bar449();
    }
}

inline void TestCase449()
{
    static Test449 test;
    
    test.Foo449(64);

    atomic do; while(test._ctr449 != 6400);

    assert(test._ctr449 == 6400);
}

inline void test_main()
{
    unit::test<447>(unit::fixture<TestCase447>());
    unit::test<449>(uncurry1(constant(TestCase449)));
}
