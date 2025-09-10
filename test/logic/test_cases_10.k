// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import helper.sleep
import test.unit as unit
import test.runner

// Initializing member arrays
class Foo225
{
public:
    uint32[4] _x = {};
}

class TestCase225
{
private:
    Foo225 g_foo225;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_foo225._x[0] == 0);
        unit::assert(tag, g_foo225._x[1] == 0);
        unit::assert(tag, g_foo225._x[2] == 0);
        unit::assert(tag, g_foo225._x[3] == 0);
    }
}

// Inline function that calls a non-inline function
// The inline function has 2 callers
class TestCase227
{
    uint32 Foo227()
    {
        return 7;
    }

    inline uint32 Bar227()
    {
        return Foo227();
    }

public:    
    void run(unit::tag_t tag)
    {
        assert(7 == Bar227());
        assert(7 == Bar227());
    }
}

// Predicated function call
// Ensure that liveness analysis does not get confused
// by the fact that the return value is not live-out at the call site
class TestCase226
{
private:
    uint32 Foo226()
    {
        return 7;
    }

    bool g_b226 = false;

public:
    [[unordered]] void run(unit::tag_t tag)
    {
        uint32 result;

        if (g_b226)
        {
            result = Foo226();
        }

        unit::assert(tag, result == 0);
    }
}

// Regular switch statement
inline void TestCase228(unit::tag_t tag)
{
    uint32 x = 2;

    uint32 result;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    case 1:
        result = 8;
        break;

    case 2:
        result = 9;
        break;
    }

    unit::assert(tag, result == 9);
}

// switch statement with the case values being a global constant
const uint32 zero229 = 0;
const uint32 one229 = 1;
const uint32 two229 = 2;

inline void TestCase229(unit::tag_t tag)
{
    uint32 x = 2;

    uint32 result;

    switch (x)
    {
    case zero229:
        result = 7;
        break;

    case one229:
        result = 8;
        break;

    case two229:
        result = 9;
        break;
    }

    unit::assert(tag, result == 9);
}

// switch with default - taken
inline void TestCase230(unit::tag_t tag)
{
    uint32 x = 5;

    uint32 result;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    case 1:
        result = 8;
        break;

    case 2:
        result = 9;
        break;

    default:
        result = 10;
        break;
    }

    unit::assert(tag, result == 10);
}

// switch with default - not taken
inline void TestCase231(unit::tag_t tag)
{
    uint32 x = 1;

    uint32 result;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    case 1:
        result = 8;
        break;

    case 2:
        result = 9;
        break;

    default:
        result = 10;
        break;
    }

    unit::assert(tag, result == 8);
}

// switch with no default
inline void TestCase232(unit::tag_t tag)
{
    uint32 x = 3;

    uint32 result = 11;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    case 1:
        result = 8;
        break;

    case 2:
        result = 9;
        break;
    }

    unit::assert(tag, result == 11);
}

// switch with no default - not taken
inline void TestCase233(unit::tag_t tag)
{
    uint32 x = 1;

    uint32 result = 11;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    case 1:
        result = 8;
        break;

    case 2:
        result = 9;
        break;
    }

    unit::assert(tag, result == 8);
}

// switch with all cases covered and no default
inline void TestCase234(unit::tag_t tag)
{
    uint2 x = 1;

    uint32 result = 11;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    case 1:
        result = 8;
        break;

    case 2:
        result = 9;
        break;

    case 3:
        result = 10;
        break;
    }

    unit::assert(tag, result == 8);
}

// switch with default not at the end - taken
inline void TestCase235(unit::tag_t tag)
{
    uint2 x = 1;

    uint32 result = 11;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    default:
        result = 8;
        break;

    case 2:
        result = 9;
        break;

    case 3:
        result = 10;
        break;
    }

    unit::assert(tag, result == 8);
}

// Ensures that max_threads is implemented
class TestCase236
{
private:
    bool g_b236 = false;

    [[pipelined, max_threads(1)]] void Foo236(uint32 idx, unit::tag_t tag)
    {
        if (idx == 0)
        {
            [[unordered]] for (const uint32 i : 1000)
            {
                unit::assert(tag, !g_b236);

                g_b236 = true;

                unit::assert(tag, g_b236);

                g_b236 = false;
            }
        }
        else
        {
            unit::assert(tag, idx == 1);

            [[unordered]] for (const uint32 i : 1000)
            {
                unit::assert(tag, !g_b236);

                g_b236 = true;

                unit::assert(tag, g_b236);

                g_b236 = false;
            }
        }
    }

public:
    void run(unit::tag_t tag)
    {
        Foo236(2, tag);
    }
}

class TestCase237
{
    // ensure the input FIFO to a basic block has the correct size (no overflow) when called from 2 functions with different max_threads values
    uint32 g_count237 = 0;
    uint32 g_dummy237 = 0;

    void Foo237()
    {
        // Do something that will cause the queue to fill up
        sleep<512>();

        atomic
        {
            g_count237++;
        }
    }

    [[pipelined, max_threads(256)]] void SmallThreadCount237(uint32 idx)
    {
        Foo237();
    }

    [[pipelined, max_threads(512)]] void LargeThreadCount237(uint32 idx)
    {
        Foo237();
    }

    [[async]] void Thread1_237()
    {
        SmallThreadCount237(2048);

        // Working around a Kanagawa compiler bug (tail call in an async function)
        ++g_dummy237;
    }

    [[async]] void Thread2_237()
    {
        LargeThreadCount237(2048);

        // Working around a Kanagawa compiler bug (tail call in an async function)
        ++g_dummy237;
    }

public:    
    [[unordered]] void run(unit::tag_t tag)
    {
        Thread1_237();
        Thread2_237();

        [[unordered]] do; while (g_count237 != 4096);
    }
}

// FIFO overflow when writing to input FIFO is async function
class TestCase238
{
private:
    uint32 g_count238 = 0;
    uint32 g_dummy238 = 0;

    [[async]] void Foo238()
    {
        // Do something that will cause the queue to fill up
        [[unordered]] for (const uint32 i : 10)
        {
            g_dummy238++;
        }

        atomic
        {
            g_count238++;
        }
    }

    [[async, pipelined]] void Bar238(uint32 idx)
    {
        Foo238();
    }

public:
    [[unordered]] void run(unit::tag_t tag)
    {
        Bar238(4096);

        [[unordered]] do; while (g_count238 != 4096);
    }
}

inline void test_main()
{
    unit::test<225>(unit::fixture<TestCase225>());
    unit::test<226>(unit::fixture<TestCase226>());
    unit::test<227>(unit::fixture<TestCase227>());
    unit::test<228>(TestCase228);
    unit::test<229>(TestCase229);
    unit::test<230>(TestCase230);
    unit::test<231>(TestCase231);
    unit::test<232>(TestCase232);
    unit::test<233>(TestCase233);
    unit::test<234>(TestCase234);
    unit::test<235>(TestCase235);
    unit::test<236>(unit::fixture<TestCase236>());
    unit::test<237>(unit::fixture<TestCase237>());
    unit::test<238>(unit::fixture<TestCase238>());
}
