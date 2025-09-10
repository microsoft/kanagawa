// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Async function that makes a tail call to a non-async function
class TestCase240
{
private:
    uint32 g_240 = 0;

    void Bar240(uint32 x)
    {
        g_240 = x;
    }

    [[async]] void Foo240(uint32 x)
    {
        Bar240(x);
    }

public:
    [[unordered]] void run(unit::tag_t tag)
    {
        unit::assert(tag, g_240 == 0);

        Foo240(7);

        [[unordered]] do; while (g_240 == 0);

        unit::assert(tag, g_240 == 7);
    }
}

// single-threaded FIFO merger
class TestCase247
{
private:
    bool b247 = true;

public:
    [[max_threads(1)]] void run(unit::tag_t tag)
    {
        uint32 x = 3;

        if (b247)
        {
            x = 5;
        }

        unit::assert(tag, x == 5);
    }
}

// single-threaded FIFO merger
class TestCase248
{
private:
    bool b248 = false;

public:
    [[max_threads(1)]] void run(unit::tag_t tag)
    {
        uint32 x = 3;

        if (b248)
        {
            x = 5;
        }

        unit::assert(tag, x == 3);
    }
}

// Ordered context saver for pipelined call
class TestCase249
{
private:
    uint32 g_count249 = 0;

    [[pipelined]] void Bar249(uint32 idx)
    {
        atomic
        {
            g_count249 = g_count249 + idx;
        }
    }

public:
    [[max_threads(1)]] void run(unit::tag_t tag)
    {
        g_count249 = 0;

        Bar249(4);

        uint32 x = 4;

        [[unordered]] do; while (g_count249 != 6);

        unit::assert(tag, g_count249 == 6);
        unit::assert(tag, x == 4); // live variable should have been saved
    }
}

inline void test_main()
{
    unit::test<240>(unit::fixture<TestCase240>());
    unit::test<247>(unit::fixture<TestCase247>());
    unit::test<248>(unit::fixture<TestCase248>());
    unit::test<249>(unit::fixture<TestCase249>());
}
