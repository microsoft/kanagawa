// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// signed integer * negative constant (both directions)
inline void Helper568(int64 a, int96 expected, unit::tag_t tag)
{
    int96 actual1 = a * -13;
    int96 actual2 = -13 * a;

    unit::assert(tag, actual1 == expected);
    unit::assert(tag, actual2 == expected);
}

inline void TestCase568(unit::tag_t tag)
{
    Helper568(0, 0, tag);
    Helper568(1, -13, tag);
    Helper568(2, -26, tag);
    Helper568(-9223372036854775808, 119903836479112085504, tag);
    Helper568(-2793956154089394880, 36321430003162133440, tag);
    Helper568(3823711255420530967, -49708246320466902571, tag);
}


// unsigned = signed * signed
inline void Helper569(int64 a, int64 b, uint128 expected, unit::tag_t tag)
{
    uint128 actual = a * b;

    unit::assert(tag, actual == expected);
}

inline void TestCase569(unit::tag_t tag)
{
    Helper569(-7, -8, 56, tag);
    Helper569(-9223372036854775808, -9223372036854775808, 85070591730234615865843651857942052864, tag);
    Helper569(-9223372036854775807, -9223372036854775808, 85070591730234615856620279821087277056, tag);
    Helper569(-9223372036854775807, -9223372036854775807, 85070591730234615847396907784232501249, tag);
    Helper569(-2793956154089394880, -4391774912661651384, 12270426544606435595433815942074513920, tag);
    Helper569(-3823711255420530967, -2503674716406219498, 9573329193034267396046808240308194566, tag);
}

// lutmul(unsigned, signed)
inline void Helper570(uint8 a, int8 b, int16 expected, unit::tag_t tag)
{
    int16 actual = a * b;

    unit::assert(tag, actual == expected);
}

inline void TestCase570(unit::tag_t tag)
{
    Helper570(3, 3, 9, tag);
    Helper570(3, -3, -9, tag);
    Helper570(255, -128, -32640, tag);
    Helper570(255, 127, 32385, tag);
    Helper570(17, -55, -935, tag);
}

// Test for optimization: y = x + 0 -> y = x
// This must handle sign extension properly
inline void Helper571(int16 x, int32 expected, unit::tag_t tag)
{
    int32 y1 = x + 0;
    unit::assert(tag, y1 == expected);

    int32 y2 = 0 + x;
    unit::assert(tag, y2 == expected);
}

inline void TestCase571(unit::tag_t tag)
{
    Helper571(-3, -3, tag);
}

class TestCase572
{
private:
    // assert in atomic
    [[pipelined]] void Helper572(uint32 tid, unit::tag_t tag)
    {
        static uint32 count572 = 0;
        atomic
        {
            unit::assert(tag, count572 == tid);
            count572++;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        Helper572(64, tag);
    }
}

// static for, zero bound (inferred type index)
inline void TestCase573(unit::tag_t tag)
{
    uint32 x = 3;

    static for(const auto i : 0)
    {
        x++;
    }

    unit::assert(tag, x == 3);
}

// static for, zero bound (explict index type)
inline void TestCase574(unit::tag_t tag)
{
    uint32 x = 3;

    static for(const uint1 i : 0)
    {
        x++;
    }

    unit::assert(tag, x == 3);
}

// Test waitfor() that calls inline function
// which calls an inline function with control flow
class Test575
{
private:
    uint32 g_global575 = 0;

public:
    inline void Check575(bool isReset)
    {
        if (isReset)
        {
            print("Hello\n");
            g_global575 = 1;
        }
        else
        {
        print("World\n");
        }
    }

    inline bool Helper575(uint32 x)
    {
        bool retVal = false;

        if (x % 2 == 0)
        {
            retVal = true;

        }
        else
        {
            retVal = false;
        }

        Check575(retVal);

        return retVal;
    }

    void run(unit::tag_t tag)
    {
        atomic do; while(!Helper575(36));
        unit::assert(tag, g_global575 == 1);
    }
}

// Testing 256-deep memories (which have a special implementation on Stratix 10)
class Test576
{
private:
    [[memory]] uint32[256] g_mem576;

public:
    [[pipelined]] void Write576(uint32 tid)
    {
        g_mem576[tid] = tid + 9;
    }

    [[pipelined]] void Read576(uint32 tid, unit::tag_t tag)
    {
        uint32 actual = g_mem576[tid];
        unit::assert(tag, actual == (tid + 9));
    }

    void run(unit::tag_t tag)
    {
        Write576(256);
        Read576(256, tag);
    }
}

template <typename T>
class Test577
{
public:
    using S=T;
    S x;
};

    // Two classes with typedef member of the same name
    class Foo
    {
        using x=int32;
    }

    class Bar
    {
        using x=int32;
    }

    // Two instances of template with typdef member
class TestCase577
{
private:
    Test577<int32> test577_int32;
    Test577<int16> test577_int16;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, bitsizeof(test577_int32.x) == bitsizeof(int32));
        unit::assert(tag, bitsizeof(test577_int16.x) == bitsizeof(int16));
    }
}

// *offsetof at compile time
class Test581
{
private:
    struct S581
    {
        uint32 x;
        uint16 y;
    }

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, static(0 == bitoffsetof(S581, x)));
        unit::assert(tag, static(0 == byteoffsetof(S581, x)));

        unit::assert(tag, static(32 == bitoffsetof(S581, y)));
        unit::assert(tag, static(4 == byteoffsetof(S581, y)));

        unit::assert(tag, static(5 == (byteoffsetof(S581, y) + 1)));
    }
}

// Repro test case for a bug where wide memory accessed in an atomic block
// crashed the simulator
using dram_flit_582_t=uint8[64];

class Helper582
{
private:
    bool _done = false;

    (uint32)->dram_flit_582_t read_mem;

    (uint32, dram_flit_582_t)->void write_mem;

    [[async, pipelined]] void Helper(uint10 i)
    {
        atomic
        {
            dram_flit_582_t flit = read_mem(i);

            static for (const auto j : 64)
            {
                flit[j]++;
            }

            write_mem(i, flit);
        }

        if (i == 511)
        {
            _done = true;
        }
    }

public:
    inline void Start(unit::tag_t tag)
    {
        unit::assert(tag, !_done);
        Helper(512);
    }

    inline void Stop()
    {
        atomic do; while(!_done);
        _done = false;
    }
}

class Test582
{
    [[memory]] dram_flit_582_t[512] _mem582;

    inline dram_flit_582_t ReadMemory(uint32 addr)
    {
        return _mem582[addr];
    }

    inline void WriteMemory(uint32 addr, dram_flit_582_t data)
    {
        _mem582[addr] = data;
    }

    Helper582[8] g_helpers582 =
    {
        {.read_mem = ReadMemory, .write_mem = WriteMemory},
        {.read_mem = ReadMemory, .write_mem = WriteMemory},
        {.read_mem = ReadMemory, .write_mem = WriteMemory},
        {.read_mem = ReadMemory, .write_mem = WriteMemory},
        {.read_mem = ReadMemory, .write_mem = WriteMemory},
        {.read_mem = ReadMemory, .write_mem = WriteMemory},
        {.read_mem = ReadMemory, .write_mem = WriteMemory},
        {.read_mem = ReadMemory, .write_mem = WriteMemory}
    };

public:
    void run(unit::tag_t tag)
    {
        for (const uint32 iter : 10)
        {
            println(iter);
            static for (const auto i : 8)
            {
                g_helpers582[i].Start(tag);
            }

            static for (const auto i : 8)
            {
                g_helpers582[i].Stop();
            }
        }

        println(_mem582[0]); // ensure things are not optimized out
    }
}

// divide/mod known at compile time
inline void TestCase583(unit::tag_t tag)
{
    unit::assert(tag, 0 == (0/5));
    unit::assert(tag, 0 == (1/5));
    unit::assert(tag, 0 == (2/5));
    unit::assert(tag, 0 == (3/5));
    unit::assert(tag, 0 == (4/5));

    unit::assert(tag, 1 == (5/5));
    unit::assert(tag, 1 == (6/5));
    unit::assert(tag, 1 == (7/5));
    unit::assert(tag, 1 == (8/5));
    unit::assert(tag, 1 == (9/5));

    unit::assert(tag, 2 == (10/5));

    unit::assert(tag, 0 == (0%5));
    unit::assert(tag, 1 == (1%5));
    unit::assert(tag, 2 == (2%5));
    unit::assert(tag, 3 == (3%5));
    unit::assert(tag, 4 == (4%5));

    unit::assert(tag, 0 == (5%5));
    unit::assert(tag, 1 == (6%5));
    unit::assert(tag, 2 == (7%5));
    unit::assert(tag, 3 == (8%5));
    unit::assert(tag, 4 == (9%5));

    unit::assert(tag, 0 == (10%5));
}

// array initialization
inline void TestCase584(unit::tag_t tag)
{
    const uint32[4] a = { 3 + 4, 10 - 1, 2, 20 };

    unit::assert(tag, a[0] == 7);
    unit::assert(tag, a[1] == 9);
    unit::assert(tag, a[2] == 2);
    unit::assert(tag, a[3] == 20);

    // Initializer list shorter than array
    int32[3] b = { -3 + 1, 8 };

    unit::assert(tag, b[0] == -2);
    unit::assert(tag, b[1] == 8);
    unit::assert(tag, b[2] == 0);
}

// global array initialization
class Test585
{
private:
    const uint32[4] g_a585 = { 2, 3, 7 };

    uint32[4] g_b585 = { 21, 33, 71 };

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_a585[0] == 2);
        unit::assert(tag, g_a585[1] == 3);
        unit::assert(tag, g_a585[2] == 7);
        unit::assert(tag, g_a585[3] == 0);

        unit::assert(tag, g_b585[0] == 21);
        unit::assert(tag, g_b585[1] == 33);
        unit::assert(tag, g_b585[2] == 71);
        unit::assert(tag, g_b585[3] == 0);
    }
}

// local struct initialization
class Test586
{
private:
    struct S586
    {
        uint32 x;
        int16 y;
        bool b;
    };

public:
    void run(unit::tag_t tag)
    {
        S586 s1 = { 2 + 5, -3, true };

        unit::assert(tag, s1.x == 7);
        unit::assert(tag, s1.y == -3);
        unit::assert(tag, s1.b == true);

        // Not all elements specified
        const S586 s2 = { 3 - 1 };

        unit::assert(tag, s2.x == 2);
        unit::assert(tag, s2.y == 0);
        unit::assert(tag, s2.b == false);
    }
}

inline void test_main()
{
    unit::test<568>(TestCase568);
    unit::test<569>(TestCase569);
    unit::test<570>(TestCase570);
    unit::test<571>(TestCase571);
    unit::test<572>(unit::fixture<TestCase572>());
    unit::test<573>(TestCase573);
    unit::test<574>(TestCase574);
    unit::test<575>(unit::fixture<Test575>());
    unit::test<576>(unit::fixture<Test576>());
    unit::test<577>(unit::fixture<TestCase577>());
    unit::test<581>(unit::fixture<Test581>());
    unit::test<582>(unit::fixture<Test582>());
    unit::test<583>(TestCase583);
    unit::test<584>(TestCase584);
    unit::test<585>(unit::fixture<Test585>());
    unit::test<586>(unit::fixture<Test586>());
}
