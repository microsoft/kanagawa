// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// memory that is broken into may parts
class Test384
{
private:
    [[memory]] uint8[8192] g_mem384;

public:
    [[pipelined]] void Write384(uint32 idx)
    {
        g_mem384[idx] = static_cast(idx + 4);
    }

    [[pipelined]] void Verify384(uint32 idx, unit::tag_t tag)
    {
        uint8 expected = static_cast(idx + 4);
        uint8 actual = g_mem384[idx];

        unit::assert(tag, expected == actual);
    }

    void run(unit::tag_t tag)
    {
        Write384(8192);

        Verify384(8192, tag);
    }
}

// cycles() with truncated output
class Test389
{
private:
    uint1 g_389;

public:
    void run(unit::tag_t tag)
    {
        uint1 c = static_cast(cycles());

        g_389 += c;
    }
}

// memory registered i/o
class Test393
{
private:
    [[memory]] uint32[512] g_mem393;

public:
    [[pipelined]] void Write393(uint32 idx)
    {
        g_mem393[idx] = idx;
    }

    [[pipelined]] void Read393(uint32 idx, unit::tag_t tag)
    {
        unit::assert(tag, idx == g_mem393[idx]);
    }

    void run(unit::tag_t tag)
    {
        Write393(512);

        Read393(512, tag);
    }
}

// do/while loop
[[unordered]] inline void TestCase394(unit::tag_t tag)
{
    uint32 i = 0;
    uint32 x = 1;

    [[unordered]] do
    {
        x += i;

        i++;

    } while(i < 10);

    unit::assert(tag, x == 46);
}

// signed/unsigned comparison
inline bool Compare395(int8 x, uint4 y)
{
    return x < y;
}

inline void TestCase395(unit::tag_t tag)
{
    bool b = Compare395(1, 15);
    unit::assert(tag, b);
}

// unrolled for with 1 iteration
class Test396
{
private:
    uint32 g_i396 = 0;

public:
    void run(unit::tag_t tag)
    {
        const auto iterations = 1;

        g_i396 = 0;

        static for (const auto i : iterations)
        {
            g_i396++;
        }

        unit::assert(tag, g_i396 == 1);
    }
}

// Print types
inline void TestCase397(unit::tag_t tag)
{
    float32 f = -1.2;

    uint32 i = 3;

    int32 j = -5;

    print("{f} blake {i} {j}\n");
}

// atomic do at the begining of a basic block - which is detected and optimized
class Test398
{
private:
    uint32 g_x;

public:
    [[async]] void Bar398(unit::tag_t tag)
    {
        unit::assert(tag, g_x == 0);

        g_x = 1;
    }

    [[async]] void Foo398()
    {
        atomic do; while (g_x != 1);

        g_x = 2;
    }

    void run(unit::tag_t tag)
    {
        g_x = 0;

        Bar398(tag);

        Foo398();

        atomic do; while (g_x != 2);
        unit::assert(tag, g_x == 2);
    }
}

inline void test_main()
{
    unit::test<384>(unit::fixture<Test384>());
    unit::test<389>(unit::fixture<Test389>());
    unit::test<393>(unit::fixture<Test393>());
    unit::test<394>(TestCase394);
    unit::test<395>(TestCase395);
    unit::test<396>(unit::fixture<Test396>());
    unit::test<397>(TestCase397);
    unit::test<398>(unit::fixture<Test398>());
}
