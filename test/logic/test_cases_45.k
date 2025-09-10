// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Zero-initializing leaf nodes
const uint64 g_u64739 = {};
class Test739
{
private:
    uint32 g_u32739 = {};

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_u32739 == 0);
        unit::assert(tag, g_u64739 == 0);

        uint32 i = {};
        unit::assert(tag, i == 0);
    }
}

// Zero-initializing a union
union u740
{
    uint32 x;
    uint16 y;
}

const u740 g_u1740 = {};
class Test740
{
private:
    u740 g_u2740 = {};

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_u1740.x == 0);
        unit::assert(tag, g_u1740.y == 0);

        unit::assert(tag, g_u2740.x == 0);
        unit::assert(tag, g_u2740.y == 0);

        u740 u3740 = {};
        unit::assert(tag, u3740.x == 0);
        unit::assert(tag, u3740.y == 0);
    }
}

// Memory of structs
struct s741
{
    uint32 x;
    uint16 y;
}

class Test741
{
private:
    [[memory]] s741[512] g_mem741 = { {3, 2}, { 2, 4} };

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_mem741[0].x == 3);
        unit::assert(tag, g_mem741[0].y == 2);

        unit::assert(tag, g_mem741[1].x == 2);
        unit::assert(tag, g_mem741[1].y == 4);
    }
}

// Memory of structs - literals are wider than underlying struct type
struct s742
{
    uint3 x;
    uint2 y;
}

class Test742
{
private:
    [[memory]] s742[512] g_mem742 = { {15, 16}, { 17, 18} };

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_mem742[0].x == 7);
        unit::assert(tag, g_mem742[0].y == 0);

        unit::assert(tag, g_mem742[1].x == 1);
        unit::assert(tag, g_mem742[1].y == 2);
    }
}

template <typename T, (T, uint32) -> uint32 Fn>
inline uint32 Go743(T capture, uint32 x)
{
    return Fn(capture, x);
}

inline void Test743(unit::tag_t tag)
{
    struct Capture
    {
        uint32 x;
        bool y;
    }

    uint32 a = 10;

    inline uint32 Lambda(Capture capture, uint32 x)
    {
        return capture.y ? x + capture.x : x - capture.x;
    }

    unit::assert(tag, Go743<Capture, Lambda>({a, false}, 12) == 2);
    unit::assert(tag, Go743<Capture, Lambda>({a , true}, 12) == 22);
}


class Test744
{
private:
    [[pipelined]] uint32 F1(uint32 i)
    {
        return i;
    }

public:
    void run(unit::tag_t tag)
    {
        uint32[5] x1 = F1(5);
        static for(const auto i : 5)
            unit::assert(tag, x1[i] == i);

        inline uint32 F2(uint32 x)
        {
            return x;
        }

        unit::assert(tag, F2(10) == 10);
    }
}

inline void Test745(unit::tag_t tag)
{
    uint32 a;
    bool b;

    b = true;
    a = 10;

    const auto fn = [a, b](uint32 x) -> uint32
        {
            return b ? x - a : x + a;
        };

    unit::assert(tag, fn(12) == 2);
}

inline void Test746(unit::tag_t tag)
{
    const auto fn = [](uint32 x) -> uint32
        {
            return x;
        };

    unit::assert(tag, fn(12) == 12);
}

template <typename T>
inline T Do747(T a, T b)
{
    const auto fn1 = [a](T x) -> T
        {
            return x - a;
        };

    const auto fn2 = [b](T x) -> T
        {
            return x - b;
        };

    return fn1(b) + fn2(a);
}

inline void Test747(unit::tag_t tag)
{
    unit::assert(tag, Do747<uint33>(10, 12) == 0);
    unit::assert(tag, Do747<uint34>(10, 10) == 0);
    unit::assert(tag, Do747<uint35>(12, 10) == 0);
}

inline uint32 Go748(uint32 x, (uint32, uint32) -> uint32 Callback1, uint32 y, (uint32, uint32) -> uint32 Callback2)
{
    return Callback1(x, Callback2(y, x));
}

inline void Test748(unit::tag_t tag)
{
    uint32 a = 10;
    bool b;

    const auto fn1 = [a](uint32 x, uint32 y) -> uint32
    {
        return a * (x + y);
    };

    b = true;

    const auto fn2 = [b](uint32 x, uint32 y) -> uint32
    {
        return b ? x : y;
    };

    unit::assert(tag, Go748(10, fn1, 12, fn2) == 220);
}

inline uint32 Go750(uint32 x, (uint32, uint32) -> uint32 Callback1, uint32 y, (uint32, uint32) -> uint32 Callback2)
{
    return Callback1(x, Callback2(y, x));
}

inline void Test750(unit::tag_t tag)
{
    uint32 a = 10;
    bool b;

    b = true;

    unit::assert(tag, Go750(10, [a](uint32 x, uint32 y) -> uint32 { return a * (x + y); }, 12, [b](uint32 x, uint32 y) -> uint32 { return b ? x : y; }) == 220);
}

inline uint32 Go752(uint32 x, (uint32, uint32) -> uint32 Callback1, uint32 y, (uint32, uint32) -> uint32 Callback2)
{
    return Callback1(x, Callback2(y, x));
}

inline uint32 Do752(uint32 x, uint32 y) { return 10 * (x + y); }

inline void Test752(unit::tag_t tag)
{
    bool b;

    b = true;

    unit::assert(tag, Go752(10, Do752, 12, [b](uint32 x, uint32 y) -> uint32 { return b ? x : y; }) == 220);
}

class Test749
{
private:
    bool g_cond749 = true;
    uint32 g_sum749;

public:
    template<uint8 stageCount>
    [[pipelined]] void AccumulateAfterStages(uint32 index)
    {
        uint8 local = index;
        static for (const auto i: stageCount)
        {
            atomic
            {
                local = g_cond749 ? ~local : local;
            }
        }

        atomic
        {
            g_sum749 += (local == index) ? local : index;
        }
    }

    void run(unit::tag_t tag)
    {
        // manually unroll a for loop until it works well with template
        uint32 i = 29;
        g_sum749 = 0;
        AccumulateAfterStages<29>(i + 1);
        unit::assert(tag, g_sum749 == i * (i + 1) / 2);

        i = 30;
        g_sum749 = 0;
        AccumulateAfterStages<30>(i + 1);
        unit::assert(tag, g_sum749 == i * (i + 1) / 2);

        i = 31;
        g_sum749 = 0;
        AccumulateAfterStages<31>(i + 1);
        unit::assert(tag, g_sum749 == i * (i + 1) / 2);

        i = 32;
        g_sum749 = 0;
        AccumulateAfterStages<32>(i + 1);
        unit::assert(tag, g_sum749 == i * (i + 1) / 2);

        i = 33;
        g_sum749 = 0;
        AccumulateAfterStages<33>(i + 1);
        unit::assert(tag, g_sum749 == i * (i + 1) / 2);

        i = 34;
        g_sum749 = 0;
        AccumulateAfterStages<34>(i + 1);
        unit::assert(tag, g_sum749 == i * (i + 1) / 2);

        i = 35;
        g_sum749 = 0;
        AccumulateAfterStages<35>(i + 1);
        unit::assert(tag, g_sum749 == i * (i + 1) / 2);
    }
}

inline void Test751(unit::tag_t tag)
{
    // Test for compiler consistency (future-proofing)
    uint32 x = 50;
    {
        uint32 x = x + 10;

        unit::assert(tag, x == 10);
    }
    unit::assert(tag, x == 50);
}

// Calling an inline function, 1 parameter is set to the result of a cast,
// and the cast result type doesn't match the function type
inline uint32 Foo753(uint16 x)
{
    return x + 1;
}

inline void Test753(unit::tag_t tag)
{
    uint33 y = 234;

    uint32 result = Foo753(cast<uint32>(y));

    unit::assert(tag, result == 235);
}


inline void test_main()
{
    unit::test<739>(unit::fixture<Test739>());
    unit::test<740>(unit::fixture<Test740>());
    unit::test<741>(unit::fixture<Test741>());
    unit::test<742>(unit::fixture<Test742>());
    unit::test<743>(Test743);
    unit::test<744>(unit::fixture<Test744>());
    unit::test<745>(Test745);
    unit::test<746>(Test746);
    unit::test<747>(Test747);
    unit::test<748>(Test748);
    unit::test<749>(unit::fixture<Test749>());
    unit::test<750>(Test750);
    unit::test<751>(Test751);
    unit::test<752>(Test752);
    unit::test<753>(Test753);
}
