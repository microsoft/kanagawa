// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import helper.sleep
import data.function
import test.unit as unit
import test.runner

// Context saver checking output FIFO full value
class TestCase283
{
public:
    uint32 Bar283(uint32 index)
    {
        return index;
    }

    [[pipelined]] void Foo283(uint32 index)
    {
        assert(index == Bar283(index));

        // Do something slow that will cause the output FIFO of Bar283 to fill up
        sleep<512>();
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        Foo283(2048);

        // To make the assertion count code happy
        // This test is primarily to look for deadlock
        assert(1 == 1);
    }
}

// Calling a function before it is declared
inline void TestCase285()
{
    assert(7 == Bar285());
}

inline uint32 Bar285()
{
    return 7;
}

class Test291
{
public:
    // Memory read on the first cycle, when output FIFO is full - repro of a dnntrainer bug
    [[memory]] uint32[32] _mem291;

    uint32 _result291;

    [[max_threads(1), async]] void SlowFunction291()
    {
        // something slow
        sleep<512>();
    }

    [[pipelined]] void Foo291(uint32 idx)
    {
        uint32 value = _mem291[idx];

        atomic
        {
            _result291 = _result291 + value;
        }

        // Ensure output FIFOs fill up
        SlowFunction291();
    }
}

inline void TestCase291()
{
    static Test291 test;

    // Initialize memory
    [[unordered]] for (const uint32 x : 32)
    {
        test._mem291[x] = x;
    }

    test._result291 = 0;

    test.Foo291(32);

    assert(test._result291 == 496);

}

inline void test_main()
{
    unit::test<283>(unit::fixture<TestCase283>());
    unit::test<285>(uncurry1(constant(TestCase285)));
    unit::test<291>(uncurry1(constant(TestCase291)));
}
