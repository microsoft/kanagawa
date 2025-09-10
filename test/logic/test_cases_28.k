// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Call to pipelined function with threadcount = 0
class TestCase441
{
private:
    uint32 g_x441;

    [[pipelined]] void Foo441(uint32 threadId)
    {
        atomic
        {
            g_x441++;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        g_x441 = 0;

        Foo441(0);
        unit::assert(tag, 0 == g_x441);

        Foo441(4);
        unit::assert(tag, 4 == g_x441);

        Foo441(0);
        unit::assert(tag, 4 == g_x441);
    }
}

// Basic single-threaded test
class TestCase442
{
private:
    [[max_threads(1)]] uint32 Foo442(uint32 x)
    {
        uint32 result = x + 1;
        return result;
    }

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 5 == Foo442(4));
    }
}

// single-threaded test if
class TestCase443
{
private:
    [[max_threads(1)]] uint32 Foo443(uint32 x)
    {
        uint32 result = x;

        if (x > 5)
        {
            result = x + 1;
        }

        return result;
    }

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 7 == Foo443(6));
        unit::assert(tag, 5 == Foo443(5));
    }
}

// single-threaded function accessing a memory
class TestCase444
{
private:
    [[memory]] uint32[512] g_mem444;

    [[max_threads(1)]] void Write444(uint32 idx, uint32 val)
    {
        g_mem444[idx] = val;
    }

    [[max_threads(1)]] uint32 Read444(uint32 idx)
    {
        return g_mem444[idx] + 1;
    }

public:
    void run(unit::tag_t tag)
    {
        Write444(7, 25);
        Write444(8, 1);

        unit::assert(tag, 26 == Read444(7));
        unit::assert(tag, 2 == Read444(8));
    }
}

// Global variable _next value used to read from a memory in a single-threaded function
class TestCase445
{
private:
    [[memory]] uint32[1024] g_mem445;

    [[max_threads(1)]] uint32 Foo445(uint64 addrIn)
    {
        uint10 addr = addrIn;

        return g_mem445[addr];
    }

    [[pipelined]] void Bar445(uint32 addr, unit::tag_t tag)
    {
        uint32 expected = (~addr) + 1;

        g_mem445[addr] = expected;

        unit::assert(tag, expected == Foo445(addr));
    }

public:
    void run(unit::tag_t tag)
    {
        Bar445(1024, tag);
    }
}

// Single-threaded function that writes to a parameter
// testing remaping in PromoteParameters()
class TestCase446
{
private:
    [[max_threads(1)]] uint32 Foo446(uint32 in)
    {
        in += 7;

        return in;
    }

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 15 == Foo446(8));
    }
}

// Memory read inside a single-threaded function
class TestCase448
{
private:
    [[memory]] uint32[32] g_mem448;

    // Single-threaded function will use unregistered output from memory
    [[max_threads(1), pipelined]] void Foo448(uint32 idx, unit::tag_t tag)
    {
        uint32 expected = idx + 7;

        uint32 val = g_mem448[idx];

        unit::assert(tag, val == expected);
    }

    // Multi-threaded function will use registered output from memory
    [[pipelined]] void Bar448(uint32 idx, unit::tag_t tag)
    {
        uint32 expected = idx + 7;

        uint32 val = g_mem448[idx];

        unit::assert(tag, val == expected);
    }

    [[pipelined]] void Write448(uint32 idx)
    {
        g_mem448[idx] = idx + 7;
    }

public:
    void run(unit::tag_t tag)
    {
        Write448(32);

        Foo448(32, tag);

        Bar448(32, tag);
    }
}

// Verifies that single-threaded function semaphore not decremented 1 cycle too early
class TestCase450
{
private:
    uint32 g_ctr450 = 0;

    // Single-threaded function will use unregistered output from memory
    [[max_threads(1), pipelined]] void Foo450(uint32 idx)
    {
        g_ctr450 = (g_ctr450 + idx) * 2;
    }

public:
    void run(unit::tag_t tag)
    {
        Foo450(20);

        println(g_ctr450);

        unit::assert(tag, g_ctr450 == 2097110);
    }
}

// 2 writes to the same memory
class TestCase451
{
private:
    [[memory]] uint32[512] g_mem451;

public:
    void run(unit::tag_t tag)
    {
        g_mem451[4] = 3;

        g_mem451[7] = 1234;

        unit::assert(tag, g_mem451[4] == 3);
        unit::assert(tag, g_mem451[7] == 1234);
    }
}

// invert converted to a lut
inline uint8 Foo452(uint8 x)
{
    return ~x;
}

inline void TestCase452(unit::tag_t tag)
{
    unit::assert(tag, 0xFA == Foo452(5));
}

// AND converted to a lut, with zero-extension
inline uint8 Foo453(uint8 x, uint6 y)
{
    return x & y;
}

inline void TestCase453(unit::tag_t tag)
{
    unit::assert(tag, 0x22 == Foo453(0xAA, 0x33));
}

// AND converted to a lut, with sign-extension
inline uint8 Foo454(uint8 x, int6 y)
{
    return x & y;
}

inline void TestCase454(unit::tag_t tag)
{
    unit::assert(tag, 0xA2 == Foo454(0xAA, 0x33));
}

// AND converted to a lut, with sign-extension
inline uint8 Foo455(uint8 x, int6 y)
{
    return x & y;
}

inline void TestCase455(unit::tag_t tag)
{
    unit::assert(tag, 0x2 == Foo455(0xAA, 0x13));
}

// OR converted to a lut
inline uint8 Foo456(uint8 x, uint8 y)
{
    return x | y;
}

inline void TestCase456(unit::tag_t tag)
{
    unit::assert(tag, 0xBF == Foo456(0xAA, 0x15));
}

inline void test_main()
{
    unit::test<441>(unit::fixture<TestCase441>());
    unit::test<442>(unit::fixture<TestCase442>());
    unit::test<443>(unit::fixture<TestCase443>());
    unit::test<444>(unit::fixture<TestCase444>());
    unit::test<445>(unit::fixture<TestCase445>());
    unit::test<446>(unit::fixture<TestCase446>());
    unit::test<448>(unit::fixture<TestCase448>());
    unit::test<450>(unit::fixture<TestCase450>());
    unit::test<451>(unit::fixture<TestCase451>());
    unit::test<452>(TestCase452);
    unit::test<453>(TestCase453);
    unit::test<454>(TestCase454);
    unit::test<455>(TestCase455);
    unit::test<456>(TestCase456);
}
