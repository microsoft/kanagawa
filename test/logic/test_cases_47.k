// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

class TestCase765
{
private:
    [[pipelined]] uint32 Test765_Helper(uint32 tid)
    {
        uint32 result = tid;

        static for(const auto i : 32)
        {
            result += tid;
        }

        return result;
    }

    [[pipelined]] void Test765(uint32 arg, unit::tag_t tag)
    {
        Test765_Helper(4);

        uint32 expected;

        atomic
        {
            static uint32 _expected = 0;
            expected = _expected;
            _expected++;
        }

        unit::assert(tag, arg == expected);
    }

public:
    void run(unit::tag_t tag)
    {
        Test765(32, tag);
    }
}

inline void Test766(int8 x, unit::tag_t tag)
{
    int8 y = x / 1;
    unit::assert(tag, x == y);
}

inline void TestCase766(unit::tag_t tag)
{
    Test766(-7, tag);
}

const uint32 Test767_N = 10;

inline void Test767(unit::tag_t tag)
{
   static for(const auto i: Test767_N) {}
}

inline uint32 Go768(uint32 x, (uint32) -> uint32 Do, unit::tag_t tag)
{
    using ClosureDo = decltype(Do);

    struct Foo
    {
        ClosureDo[10] a;
    }

    static Foo[10] foo;
    static uint32 i = 0;

    foo[i].a[i] = Do;

    uint32 result;

    if (i > 0)
    {
        auto f = foo[i-1].a[i-1];

        result = f(x);

        unit::assert(tag, result == i - 1 + x);
    }
    else
    {
        result = 0;
    }

    i += 1;

    return result;
}

inline void Foo768(uint32 i, unit::tag_t tag)
{
    auto x = Go768(static_cast(2*i), [i](uint32 x) -> uint32 {return i + x;}, tag);

    if (i == 0)
        unit::assert(tag, x == 0);
    else
        unit::assert(tag, x == (i - 1) + 2 * i);
}

inline void Test768(unit::tag_t tag)
{
    class Helper
    {
    public:
        void Test(uint32 i, unit::tag_t tag)
        {
            Foo768(i, tag);
        }
    }

    static Helper _helper;

    static for (const auto i : 10)
        _helper.Test(i, tag);
}

// static local object - non-inline method - will be auto-inline
class Helper769
{
public:
    uint32 Foo(uint32 x)
    {
        return x + 1;
    }
}

inline void Test769(unit::tag_t tag)
{
    static Helper769 h;

    uint32 result = h.Foo(4);

    unit::assert(tag, result == 5);
}

// static local declared in an inline function with multiple call sites
class Helper770
{
private:
    uint32 _val = 7;

public:
    uint32 GetAndInc()
    {
        uint32 result = _val;

        _val++;

        return result;
    }
}

inline void Inner770(unit::tag_t tag)
{
    static Helper770 h;

    uint32 result = h.GetAndInc();
    unit::assert(tag, result == 7);

    result = h.GetAndInc();
    unit::assert(tag, result == 8);
}

inline void Test770(unit::tag_t tag)
{
    Inner770(tag);
    Inner770(tag);
}

// static local declared in an inline function with multiple call sites
// static local method is declared inline
class Helper771
{
private:
    uint32 _val = 7;

public:
    inline uint32 GetAndInc()
    {
        uint32 result = _val;

        _val++;

        return result;
    }
}

inline void Inner771(unit::tag_t tag)
{
    static Helper771 h;

    uint32 result = h.GetAndInc();
    unit::assert(tag, result == 7);

    result = h.GetAndInc();
    unit::assert(tag, result == 8);
}

inline void Test771(unit::tag_t tag)
{
    Inner771(tag);
    Inner771(tag);
}

// nested inline functions with static local in the innermost one
class Helper772
{
private:
    uint32 _val = 7;

public:
    inline uint32 GetAndInc()
    {
        uint32 result = _val;

        _val++;

        return result;
    }
}

inline void Inner772(unit::tag_t tag)
{
    static Helper772 h;

    uint32 result = h.GetAndInc();
    unit::assert(tag, result == 7);

    result = h.GetAndInc();
    unit::assert(tag, result == 8);
}

inline void Mid772(unit::tag_t tag)
{
    Inner772(tag);
    Inner772(tag);
}

inline void Test772(unit::tag_t tag)
{
    Mid772(tag);
    Mid772(tag);
}


inline void test_main()
{
    unit::test<765>(unit::fixture<TestCase765>());
    unit::test<765>(TestCase766);
    unit::test<767>(Test767);
    unit::test<768>(Test768);
    unit::test<769>(Test769);
    unit::test<770>(Test770);
    unit::test<771>(Test771);
    unit::test<772>(Test772);
}
