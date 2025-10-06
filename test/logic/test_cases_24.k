// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// auto-pipelining test for a memory load as early as possible
class Test700
{
private:
    [[memory]] uint32[512] g_mem700;

public:
    [[pipelined]] void Helper700(uint32 addr, uint32 base, unit::tag_t tag)
    {
        uint32 val = g_mem700[addr];

        uint32 expected = static_cast((addr + 1));

        unit::assert(tag, val == expected);
    }

    void run(unit::tag_t tag)
    {
        // Initialize memory
        for (const uint32 i : 512)
        {
            g_mem700[i] = i + 1;
        }

        // Check ranges of memory for validity
        // Loop is written this way to allow simulation of variable-latency chain
        // to switch latencies
        for (const uint32 i : 500)
        {
            Helper700(10, i, tag);
        }
    }
}

// auto-pipelining test for fifo sizing
class Test701
{
private:
    uint32 g_innerSum701 = 0;
    uint32 g_outerSum701 = 0;

public:
    [[pipelined]] void Inner701(uint32 tid)
    {
        atomic
        {
            g_innerSum701++;
        }
    }

    [[pipelined]] void Outer701(uint32 tid)
    {
        atomic
        {
            g_outerSum701++;
        }

        Inner701(10);
    }

    void run(unit::tag_t tag)
    {
        Outer701(100);

        unit::assert(tag, 100 == g_outerSum701);
        unit::assert(tag, 1000 == g_innerSum701);
    }
}

template <typename T>
struct S
{
    T[2] x;
}

class CTest702
{
public:
    template <typename T, T N>
    T Test702()
    {
        static T x = N - 1;
        x = x + 1;
        return x;
    }
}

    using DWORD=uint32;

    class B
    {
    public:
        using DWORD = uint32;
    }

    template <typename T, T N>
    class C
    {
    private:
        CTest702 g_test702;

    public:
        template <typename T, T N>
        T Test702(unit::tag_t tag)
        {
            static T x = N - 1;
            x = x + 1;

            unit::assert(tag, x == g_test702.Test702<T, N>());

            return x;
        }

        T Test(unit::tag_t tag)
        {
            return Test702<T, N>(tag);
        }
    }

    const uint32 g_x = 10;
    const uint4 g_y = cast<uint4>(26);

template <uint8 N>
using UInt = uint<N>;

template <typename T>
using Param = T;

class Test702
{
private:
    C<uint2, 1> ct702;

public:
    void run(unit::tag_t tag)
    {
        const uint4 z = cast<uint4>(5 * 10 + 8);
        const auto s = bitsizeof S<uint6>;

        unit::assert(tag, 10 == ct702.Test702<uint32, 10>(tag));
        unit::assert(tag, 11 == ct702.Test702<uint<32>, 10>(tag));
        unit::assert(tag, 12 == ct702.Test702<DWORD, 5+5>(tag));
        unit::assert(tag, 13 == ct702.Test702<DWORD, bitsizeof int<10>>(tag));
        unit::assert(tag, 14 == ct702.Test702<UInt<32>, bitsizeof UInt<10>>(tag));
        unit::assert(tag, 15 == ct702.Test702<Param<UInt<32>>, bitsizeof uint10>(tag));
        unit::assert(tag, 16 == ct702.Test702<B::DWORD, bitsizeof DWORD - 22>(tag));
        unit::assert(tag, 17 == ct702.Test702<DWORD, bitsizeof S<uint6> - 2>(tag));
        unit::assert(tag, 18 == ct702.Test702<DWORD, g_x>(tag));
        unit::assert(tag, 19 == ct702.Test702<DWORD, g_y>(tag));
        unit::assert(tag, 20 == ct702.Test702<DWORD, z>(tag));
        unit::assert(tag, 21 == ct702.Test702<DWORD, bitsizeof 1000>(tag));
        unit::assert(tag, 22 == ct702.Test702<DWORD, bitsizeof DWORD == 32 ? 10 : 12>(tag));
        unit::assert(tag, 23 == ct702.Test702<DWORD, s - 2>(tag));
        unit::assert(tag, 10 == ct702.Test702<UInt<16>, bitsizeof int<10>>(tag));
        unit::assert(tag, 30 == ct702.Test702<DWORD, bitsizeof int<30>>(tag));
    }
}


    template <uint32 N>
    inline uint32 Fn703()
    {
        return N;
    }

    const uint2 x703 = cast<uint2>(10);
    const uint4 y703 = 10;

inline void TestCase703(unit::tag_t tag)
{
    unit::assert(tag, 2 == Fn703<x703>());
    unit::assert(tag, 10 == Fn703<y703>());
}

// mux(known index, a, b, c, d) optimization
inline uint32 Helper707(uint32[4] data)
{
    return mux(3, data[0], data[1], data[2], data[3]);
}

inline void TestCase707(unit::tag_t tag)
{
    uint32[4] data;

    static for (const auto i : 4)
    {
        data[i] = i;
    }

    unit::assert(tag, 3 == Helper707(data));
}

// mux(known index, a, b, c, d) optimization - sign extension
inline int32 Helper708(int8[4] data)
{
    return mux(3, data[0], data[1], data[2], data[3]);
}

inline void TestCase708(unit::tag_t tag)
{
    int8[4] data;

    data[0] = 0;
    data[1] = 1;
    data[2] = 2;
    data[3] = -128;

    int32 result = Helper708(data);

    unit::assert(tag, -128 == result);
}

inline void test_main()
{
    unit::test<700>(unit::fixture<Test700>());
    unit::test<701>(unit::fixture<Test701>());
    unit::test<702>(unit::fixture<Test702>());
    unit::test<703>(TestCase703);
    unit::test<707>(TestCase707);
    unit::test<708>(TestCase708);
}
