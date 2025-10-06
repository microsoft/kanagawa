// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// atomic read-modify-write to memory to build a histogram
class Test514
{
private:
    [[memory]] uint32[512] g_histogramSrc514;
    [[memory]] uint32[512] g_histogramDst514;

public:
    [[pipelined]] void HistogramHelper514(uint16 threadId)
    {
        uint32 src = g_histogramSrc514[threadId];

        atomic
        {
            uint32 val = g_histogramDst514[src];
            val++;
            g_histogramDst514[src] = val;
        }
    }

    [[pipelined]] void InitializeHistogramMemories514(uint16 threadId)
    {
        g_histogramSrc514[threadId] = threadId & 1;
        g_histogramDst514[threadId] = 0;
    }

    [[pipelined]] void ValidateHistogram514(uint16 threadId, unit::tag_t tag)
    {
        uint32 actual = g_histogramDst514[threadId];

        uint32 expected = (threadId < 2) ? 256 : 0;

        unit::assert(tag, actual == expected);
    }

    void run(unit::tag_t tag)
    {
        InitializeHistogramMemories514(512);

        HistogramHelper514(512);

        ValidateHistogram514(512, tag);
    }
}


// dynamic indexing into an array of size 1
class Test515
{
private:
    uint32[1] g_ary515;

public:
    uint32 Foo515(uint32 idx)
    {
        return g_ary515[idx];
    }

    void run(unit::tag_t tag)
    {
        g_ary515[0] = 7;

        unit::assert(tag, Foo515(0) == 7);
    }
}

// dynamic indexing (load) into an array where the index type is too narrow
inline void TestCase516(unit::tag_t tag)
{
    uint32[17] data;

    static for (const auto i : 17)
    {
        data[i] = i;
    }

    for (const uint2 i : 3)
    {
        unit::assert(tag, data[i] == i);
    }
}

// dynamic indexing (load) into an array where the index type is too narrow
inline void TestCase517(unit::tag_t tag)
{
    uint32[17] data;

    for (const uint2 i : 3)
    {
        data[i] = i;
    }

    static for (const auto i : 3)
    {
        unit::assert(tag, data[i] == i);
    }
}

// large base-10 literal
inline uint128 Foo518(uint32 x)
{
    return x + 1234567898765432154456489786125;
}

inline void TestCase518(unit::tag_t tag)
{
    unit::assert(tag, Foo518(3) == 1234567898765432154456489786128);
}

// large base-16 literal
inline uint128 Foo519(uint32 x)
{
    return x + 0xdeadbeef1234567890fa;
}

inline void TestCase519(unit::tag_t tag)
{
    unit::assert(tag, Foo519(3) == 0xdeadbeef1234567890fd);
}

// global initialized to large base-16 literal
class Test520
{
private:
    uint128 g_global520 = 0xdeadbeef1234567890fa;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, (g_global520 + 3) == 0xdeadbeef1234567890fd);
    }
}

// Multiply by non-pow2 constant
inline void Foo521(uint32 x, unit::tag_t tag)
{
    unit::assert(tag, 0 == (x * 0));
    unit::assert(tag, x == (x * 1));
    unit::assert(tag, (x << 1) == (x * 2));
    unit::assert(tag, (x + (x << 1)) == (x * 3));
    unit::assert(tag, (x + (x << 1)) == (3 * x));
    unit::assert(tag, (x << 2) == (x * 4));
    unit::assert(tag, (x + (x << 2)) == (x * 5));
    unit::assert(tag, ((x << 1) + (x << 2)) == (x * 6));
    unit::assert(tag, (x + (x << 1) + (x << 2)) == (x * 7));
}

inline void TestCase521(unit::tag_t tag)
{
    Foo521(0, tag);
    Foo521(1, tag);
    Foo521(2, tag);
    Foo521(3, tag);
    Foo521(4, tag);
}

// Using an uninitialized variable ('x in verilog simulator) as an index into a LUT operation
class Test523
{
private:
    uint32 g_stuff523;

    bool g_a523;
    bool g_b523;
    bool g_c523 = true;

public:
    void Foo523()
    {
        // Something so that this function cannot be optimized away
        g_stuff523++;
    }

    bool Helper523(bool a, bool b, bool c)
    {
        // a and b are 'x
        // but predicate should be true because c is true
        return (a && b) || c;
    }

    void run(unit::tag_t tag)
    {
        bool predicate = Helper523(g_a523, g_b523, g_c523);

        print("Predicate value {predicate}\n");

        if (predicate)
        {
            // The bug would cause the predicate value to be 'x
            // which causes the thread to disappear because the fifo wren
            // value is 'x
            Foo523();
        }
    }
}

// Literal bool template parameters
template<bool predicate>
class Foo524
{
public:
    bool GetResult()
    {
        return predicate;
    }
}

class Test524
{
private:
    Foo524<false> g_foo524_1;
    Foo524<true> g_foo524_2;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, !g_foo524_1.GetResult());
        unit::assert(tag, g_foo524_2.GetResult());
    }
}

// Common subexpression elimination needs to look at destination widths
inline uint32 Foo526(uint32 x)
{
    uint2 y = static_cast(x + 1);

    uint32 z = static_cast(x + 1);

    return y + z;
}

inline void TestCase526(unit::tag_t tag)
{
    unit::assert(tag, Foo526(4) == 6);
}

// compile-time known values being narrowed through a cast
const uint8 u529 = cast<uint8>(0x5678ab12);

inline void TestCase529(unit::tag_t tag)
{
    unit::assert(tag, u529 == 0x12);
}

// parsing of floats
inline void TestCase530(unit::tag_t tag)
{
    print("{0.01} {cast<uint32>(0.01)}\n");

    barrier;

    unit::assert(tag, 0x3c23d70a == cast<uint32>(0.01));
    unit::assert(tag, 0xbe4ccccd == cast<uint32>(-0.2));
}

// signed integer comparisons
inline void TestCase531(unit::tag_t tag)
{
    unit::assert(tag, -2 < -1);
    unit::assert(tag, -2 <= -1);
    unit::assert(tag, -1 <= -1);

    unit::assert(tag, -2 > -3);
    unit::assert(tag, -2 >= -3);
    unit::assert(tag, -3 >= -3);
}

inline void test_main()
{
    unit::test<514>(unit::fixture<Test514>());
    unit::test<515>(unit::fixture<Test515>());
    unit::test<516>(TestCase516);
    unit::test<517>(TestCase517);
    unit::test<518>(TestCase518);
    unit::test<519>(TestCase519);
    unit::test<520>(unit::fixture<Test520>());
    unit::test<521>(TestCase521);
    unit::test<523>(unit::fixture<Test523>());
    unit::test<524>(unit::fixture<Test524>());
    unit::test<526>(TestCase526);
    unit::test<529>(TestCase529);
    unit::test<530>(TestCase530);
    unit::test<531>(TestCase531);
}
