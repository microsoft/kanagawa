// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// atomic on array of memories
class TestCase600
{
private:
    memory<uint32, 512>[4] g_mem600;

    [[pipelined]] void Helper600(uint32 tid)
    {
        uint16[4] indices;

        indices[0] = 0;
        indices[1] = tid % 2;
        indices[2] = tid % 4;
        indices[3] = tid % 8;

        atomic
        {
            uint32[4] values;
            static for(const auto i : 4)
            {
                values[i] = g_mem600[i][indices[i]];
            }
            static for(const auto i : 4)
            {
                values[i] = values[i] + 1;
            }
            static for(const auto i : 4)
            {
                g_mem600[i][indices[i]] = values[i];
            }
        }
    }

    [[pipelined]] void Clear600(uint32 tid)
    {
        static for(const auto i : 4)
        {
            g_mem600[i][tid] = 0;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        // Set all elements to 0
        Clear600(512);

        // do some atomic operations
        Helper600(16);

        unit::assert(tag, g_mem600[0][0] == 16);

        unit::assert(tag, g_mem600[1][0] == 8);
        unit::assert(tag, g_mem600[1][1] == 8);

        unit::assert(tag, g_mem600[2][0] == 4);
        unit::assert(tag, g_mem600[2][1] == 4);
        unit::assert(tag, g_mem600[2][2] == 4);
        unit::assert(tag, g_mem600[2][3] == 4);

        unit::assert(tag, g_mem600[3][0] == 2);
        unit::assert(tag, g_mem600[3][1] == 2);
        unit::assert(tag, g_mem600[3][2] == 2);
        unit::assert(tag, g_mem600[3][3] == 2);
        unit::assert(tag, g_mem600[3][4] == 2);
        unit::assert(tag, g_mem600[3][5] == 2);
        unit::assert(tag, g_mem600[3][6] == 2);
        unit::assert(tag, g_mem600[3][7] == 2);
    }
}

// memory uint<N>[]
class TestCase601
{
private:
    const auto BitWidth601 = 12;
    const auto Depth601 = 512;

    [[memory]] uint<BitWidth601>[Depth601] _scheduleable;

public:
    void run(unit::tag_t tag)
    {
    _scheduleable[0] = 3;
    unit::assert(tag, 3 == _scheduleable[0]);
    }
}

// static local (initialized)
class TestCase620
{
private:
    [[pipelined]] void Helper620(uint32 tid, unit::tag_t tag)
    {
        static uint32 sharedVal = 3;

        uint32 snapped;

        atomic
        {
            snapped = sharedVal;

            sharedVal++;
        }

        unit::assert(tag, snapped == (3 + tid));
    }

public:
    void run(unit::tag_t tag)
    {
        Helper620(100, tag);
    }
}

// static local (not initialized)
class TestCase621
{
private:
    [[pipelined]] void Helper621(uint32 tid, unit::tag_t tag)
    {
        static uint32 sharedVal;

        uint32 snapped;

        atomic
        {
            snapped = (tid == 0) ? 7 : sharedVal;

            sharedVal = snapped + 1;
        }

        unit::assert(tag, snapped == (7 + tid));
    }

public:
    void run(unit::tag_t tag)
    {
        Helper621(100, tag);
    }
}

// auto static local
class TestCase623
{
private:
    [[pipelined]] void Helper623(uint32 tid, unit::tag_t tag)
    {
        static auto sharedVal = 300;

        uint32 snapped;

        atomic
        {
            snapped = sharedVal;

            sharedVal--;
        }

        unit::assert(tag, snapped == (300 - tid));
    }

public:
    void run(unit::tag_t tag)
    {
        Helper623(100, tag);
    }
}

// passing a string to an inline function
inline void Helper624(uint32 x, string s)
{
    print("{x} {s}\n");
}

inline void TestCase624(unit::tag_t tag)
{
   Helper624(3, "blake");
}

// passing a string to an inline function (nested)
inline void Helper625(uint32 x, const string s)
{
    print("{x} {s}\n");
}

inline void Outer625(uint32 a, const string b)
{
    Helper625(a, b);
}

inline void TestCase625(unit::tag_t tag)
{
   Outer625(3, "blake");
}

// template instantiation with parenthesis
template<uint32 x>
class Helper629
{
public:
    void Test(uint32 in, unit::tag_t tag)
    {
        unit::assert(tag, x == in);
    }
}

class TestCase629
{
private:
    Helper629<bitsizeof(uint32)> g_helper629;

public:
    void run(unit::tag_t tag)
    {
        g_helper629.Test(32, tag);
    }
}

// bitsizeof integer
inline void TestCase633(unit::tag_t tag)
{
    unit::assert(tag, 1 == bitsizeof(1));
    unit::assert(tag, 2 == bitsizeof(2));
    unit::assert(tag, 2 == bitsizeof(3));
    unit::assert(tag, 3 == bitsizeof(4));
}

// clog2
inline void TestCase636(unit::tag_t tag)
{
    unit::assert(tag, 0 == clog2(1));
    unit::assert(tag, 1 == clog2(2));
    unit::assert(tag, 2 == clog2(3));
    unit::assert(tag, 2 == clog2(4));
    unit::assert(tag, 3 == clog2(5));
    unit::assert(tag, 3 == clog2(6));
    unit::assert(tag, 3 == clog2(7));
    unit::assert(tag, 3 == clog2(8));

    // Verify that value is known at compile time
    static assert(4 == clog2(9));
}

// clog2 - result size
inline void TestCase637(unit::tag_t tag)
{
    unit::assert(tag, 1 == bitsizeof(clog2(1)));
    unit::assert(tag, 1 == bitsizeof(clog2(2)));
    unit::assert(tag, 2 == bitsizeof(clog2(3)));
    unit::assert(tag, 2 == bitsizeof(clog2(4)));
    unit::assert(tag, 2 == bitsizeof(clog2(5)));
    unit::assert(tag, 2 == bitsizeof(clog2(6)));
    unit::assert(tag, 2 == bitsizeof(clog2(7)));
    unit::assert(tag, 2 == bitsizeof(clog2(8)));
    unit::assert(tag, 3 == bitsizeof(clog2(9)));
}

inline void test_main()
{
    unit::test<600>(unit::fixture<TestCase600>());
    unit::test<601>(unit::fixture<TestCase601>());
    unit::test<620>(unit::fixture<TestCase620>());
    unit::test<621>(unit::fixture<TestCase621>());
    unit::test<623>(unit::fixture<TestCase623>());
    unit::test<624>(TestCase624);
    unit::test<625>(TestCase625);
    unit::test<629>(unit::fixture<TestCase629>());
    unit::test<633>(TestCase633);
    unit::test<636>(TestCase636);
    unit::test<637>(TestCase637);
}
