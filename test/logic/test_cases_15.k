// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// array of memories (static index)
class Test326
{
private:
    memory<uint32, 512>[4] g_mem326;

public:
    void run(unit::tag_t tag)
    {
        static for (const auto x : 4)
        {
            g_mem326[x][511 - x] = x;
        }

        static for (const auto x : 4)
        {
            unit::assert(tag, x == g_mem326[x][511 - x]);
        }
    }
}

// common subexpression elimination
inline void TestCase332(unit::tag_t tag)
{
    uint4 x = 2;
    uint5 y = 31;

    uint32 result1 = x + y;
    uint32 result2 = x + y;

    unit::assert(tag, result1 == result2);
}

// basic atomic function
inline uint8 Foo333(uint32 x, uint32 y)
{
    return x | y;
}

inline void TestCase333(unit::tag_t tag)
{
    unit::assert(tag, 9 == Foo333(8, 1));
}

// if predicate removal optimization
class Test334
{
private:
    bool g_b334 = false;
    uint32 g_x334 = 8;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 0;

        if (g_b334)
        {
            uint32 y = 7 + g_x334;

            x = y;
        }
        else
        {
            uint32 z = 10 + g_x334;

            x = z;
        }

        unit::assert(tag, x == 18);
    }
}

// parmaeter vonersion on atomic function call
inline uint32 Foo335(uint2 x, uint2 y)
{
    return x + y;
}

inline void TestCase335(unit::tag_t tag)
{
    unit::assert(tag, 5 == Foo335(15, 2));
}

// atomic call within atomic block
inline uint32 Foo336(uint2 x, uint2 y)
{
    return x + y;
}

inline void TestCase336(unit::tag_t tag)
{
    uint32 result = 0;

    atomic
    {
        result = Foo336(15, 2);
    }

    unit::assert(tag, 5 == result);
}

// nested atomic calls
inline uint32 Foo337(uint32 x, uint32 y)
{
    return x + y;
}

inline uint32 Bar337(uint32 z)
{
    return Foo337(z, z + 1);
}

inline void TestCase337(unit::tag_t tag)
{
    unit::assert(tag, 7 == Bar337(3));
}

// inline with bool param/return
inline bool LogicalAnd(bool a, bool b)
{
    return a && b;
}

inline void TestCase338(unit::tag_t tag)
{
    unit::assert(tag, false == LogicalAnd(false, false));
    unit::assert(tag, false == LogicalAnd(false, true));
    unit::assert(tag, false == LogicalAnd(true, false));
    unit::assert(tag, true  == LogicalAnd(true, true));
}

// memory of structs
class Test339
{
private:
    struct Foo339
    {
        uint8 x;
        uint7 y;
    }

    [[memory]] Foo339[512] g_mem339;

public:
    void run(unit::tag_t tag)
    {
        Foo339 in;

        in.x = 3;
        in.y = 2;

        g_mem339[2] = in;

        Foo339 out = g_mem339[2];

        unit::assert(tag, out.x == 3);
        unit::assert(tag, out.y == 2);
    }
}

// FIFO merger with 3 sources
inline uint32 Foo340(uint32 x)
{
    return x + 1;
}

inline void TestCase340(unit::tag_t tag)
{
    uint32 result = Foo340(2) + Foo340(4) + Foo340(5);

    unit::assert(tag, result == 14);
}

// FIFO merger with 4 sources
inline uint32 Foo341(uint32 x)
{
    return x + 1;
}

inline void TestCase341(unit::tag_t tag)
{
    uint32 result = Foo341(2) + Foo341(4) + Foo341(5) + Foo341(6);

    unit::assert(tag, result == 21);
}

// Predicated async call inside of an ordered function
class Test343
{
private:
    uint32 g_343 = 0;

public:
    [[async]] void Foo343(uint32 x)
    {
        g_343 = x;
    }

    void Bar343(bool b)
    {
        if (b)
        {
            Foo343(7);
        }
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        unit::assert(tag, 0 == g_343);

        Bar343(true);

        [[unordered]] do; while (g_343 != 7);

        unit::assert(tag, g_343 == 7);
    }
}

// PushForward checking input and output bit counts

inline uint1 BinaryCmp344(uint32 i)
{
    return (i < 4) ? 0 : 1;
}

[[unordered]] inline void TestCase344(unit::tag_t tag)
{
    uint32[8] a1;

    // Initialize
    [[unordered]] for (const uint32 i : 8)
    {
        a1[i] = i;
    }

    // Convert binary
    uint1[8] a2;

    static for (const auto i : 8)
    {
        a2[i] = BinaryCmp344(a1[i] + 1);
    }

    // Sum
    uint32 sum = 0;

    static for (const auto i : 8)
    {
        sum += a2[i];
    }

    unit::assert(tag, sum == 5);
}

// PushForward checking input and output bit counts
inline uint32 OneHot345(uint4 i)
{
    return 1 << i;
}

[[unordered]] inline void TestCase345(unit::tag_t tag)
{
    uint4[8] a1;

    // Initialize
    [[unordered]] for (const uint32 i : 8)
    {
        a1[i] = i;
    }

    // Convert to one-hot
    uint32[8] a2;

    static for (const auto i : 8)
    {
        a2[i] = OneHot345(a1[i] + 1);
    }

    // Sum
    uint32 sum = 0;

    static for (const auto i : 8)
    {
        sum += a2[i];
    }

    unit::assert(tag, sum == 510);
}

// fan_out
inline uint32[8] Scale346(uint32 s, uint32[8] a)
{
    uint32[4] sf = fan_out<4>(s);

    uint32[8] result;

    static for (const auto i : 8)
    {
        result[i] = a[i] << cast<uint8>(sf[i / 2]);
    }

    return result;
}

inline void TestCase346(unit::tag_t tag)
{
    uint32[8] input;

    static for (const auto i : 8)
    {
        input[i] = i;
    }

    uint32[8] result = Scale346(2, input);

    static for (const auto i : 8)
    {
        uint32 expected = input[i] * 4;
        uint32 actual = result[i];

        unit::assert(tag, expected == actual);
    }
}

// sub FIFO
[[unordered]] inline uint32 Sum347(uint32[32] data)
{
    uint32 result = 0;

    [[unordered]] for (const uint32 i : 32)
    {
        result = result + data[i];
    }

    return result;
}

[[unordered]] inline void TestCase347(unit::tag_t tag)
{
    uint32[32] input;

    static for (const auto i : 32)
    {
        input[i] = i;
    }

    uint32 result = Sum347(input);

    unit::assert(tag, result == 496);
}

// Partitioning of large memories
class Test348
{
private:
    [[memory]] uint32[65536] g_mem348;

public:
    [[pipelined]] void Write348(uint32 idx, unit::tag_t tag)
    {
        g_mem348[(idx << 12) + idx] = idx;
    }

    [[pipelined]] void Read348(uint32 idx, unit::tag_t tag)
    {
        uint32 val = g_mem348[(idx << 12) + idx];

        unit::assert(tag, val == idx);
    }

    void run(unit::tag_t tag)
    {
        Write348(10, tag);

        Read348(10, tag);
    }
}

// decomposing large muxes
inline void Helper349(uint32[64] a, uint32 index, uint32 expected, unit::tag_t tag)
{
    unit::assert(tag, a[index] == expected);
}

inline void TestCase349(unit::tag_t tag)
{
    uint32[64] a;

    static for (const auto x : 64)
    {
        a[x] = x + 1;
    }

    Helper349(a, 7, 8, tag);
    Helper349(a, 8, 9, tag);
    Helper349(a, 47, 48, tag);
    Helper349(a, 63, 64, tag);
}

// synchronous void functions should not return too soon
class Test350
{
private:
    uint32 g_350 = 0;

public:
    void Helper350()
    {
        static for (const auto i : 100)
        {
            g_350 = i;
        }
    }

    void run(unit::tag_t tag)
    {
        Helper350();

        unit::assert(tag, g_350 == 99);
    }
}


inline void test_main()
{
    unit::test<326>(unit::fixture<Test326>());
    unit::test<332>(TestCase332);
    unit::test<333>(TestCase333);
    unit::test<334>(unit::fixture<Test334>());
    unit::test<335>(TestCase335);
    unit::test<336>(TestCase336);
    unit::test<337>(TestCase337);
    unit::test<338>(TestCase338);
    unit::test<339>(unit::fixture<Test339>());
    unit::test<340>(TestCase340);
    unit::test<341>(TestCase341);
    unit::test<343>(unit::fixture<Test343>());
    unit::test<344>(TestCase344);
    unit::test<345>(TestCase345);
    unit::test<346>(TestCase346);
    unit::test<347>(TestCase347);
    unit::test<348>(unit::fixture<Test348>());
    unit::test<349>(TestCase349);
    unit::test<350>(unit::fixture<Test350>());
}
