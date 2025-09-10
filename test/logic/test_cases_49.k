// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner
import helper.static_locals

// signed integer * positive constant (both directions)
inline void Helper567(int64 a, int96 expected, unit::tag_t tag)
{
    int96 actual1 = a * 13;
    int96 actual2 = 13 * a;

    unit::assert(tag, actual1 == expected);
    unit::assert(tag, actual2 == expected);
}

inline void TestCase567(unit::tag_t tag)
{
    Helper567(0, 0, tag);
    Helper567(1, 13, tag);
    Helper567(2, 26, tag);
    Helper567(-9223372036854775808, -119903836479112085504, tag);
    Helper567(-2793956154089394880, -36321430003162133440, tag);
    Helper567(3823711255420530967, 49708246320466902571, tag);
}

// global struct initialization
struct S587
{
    uint32 x;
    int16 y;
    bool b;
};

// Not all elements specified
const S587 g_s2587 = { 3 - 1 };

class Test587
{
private:
    S587 g_s1587 = { 2 + 5, -3, true };

public:
    void run(unit::tag_t tag)
    {
        println(g_s1587);
        println(g_s2587);

        unit::assert(tag, g_s1587.x == 7);
        unit::assert(tag, g_s1587.y == -3);
        unit::assert(tag, g_s1587.b == true);

        unit::assert(tag, g_s2587.x == 2);
        unit::assert(tag, g_s2587.y == 0);
        unit::assert(tag, g_s2587.b == false);
    }
}

// leaf-node intiializer list
class Test588
{
private:
    int32 g_i588 = {};
    bool g_b588 = {};

public:
    void run(unit::tag_t tag)
    {
        uint8 i = {};
        bool b = {};

        unit::assert(tag, g_i588 == 0);
        unit::assert(tag, i == 0);

        unit::assert(tag, g_b588 == false);
        unit::assert(tag, b == false);
    }
}

// Sign-extension of global initial value
const int32 g_i1589 = -3;

class Test589
{
private:
    int32 g_i2589 = -4 + 2;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_i1589 == -3);
        unit::assert(tag, g_i2589 == -2);
    }
}

// calling into a member of a member (inside an array)
class Inner784
{
private:
    uint32 val = 4;
public:
    uint32 GetAndInc()
    {
        val++;
        return val;
    }
}

class Mid784
{
public:
    Inner784[2] _inner;
}

class Outer784
{
private:
    Mid784 _mid;

public:
    void Test(unit::tag_t tag)
    {
        uint32 result = _mid._inner[0].GetAndInc();
        unit::assert(tag, result == 5);

        result = _mid._inner[1].GetAndInc();
        unit::assert(tag, result == 5);
    }
}

class Test784
{
private:
    Outer784 g_outer784;

public:
void run(unit::tag_t tag)
    {
        g_outer784.Test(tag);
    }
}

// inline method with a parameter name matching a member name
class Inner785
{
private:
    uint32 val = 4;

public:
    inline uint32 GetAndInc(uint32 val)
    {
        val++;
        return val;
    }
}

inline void Test785(unit::tag_t tag)
{
    static Inner785 obj;

    uint32 result = obj.GetAndInc(10);
    unit::assert(tag, 11 == result);
}

// non-inline method with a parameter name matching a member name
inline void Test786(unit::tag_t tag)
{
    class Inner786
    {
    private:
        uint32 val = 4;

    public:
        uint32 GetAndInc(uint32 val)
        {
            val++;
            return val;
        }
    }

    static Inner786 obj;

    // Two calls, to ensure the function is not auto-inlined
    uint32 result = obj.GetAndInc(10);
    unit::assert(tag, 11 == result);

    result = obj.GetAndInc(10);
    unit::assert(tag, 11 == result);
}

class Test787
{
private:
    uint32 g_787 = 0;

public:
    void Test787_helper()
    {
        inline void Do()
        {
            g_787++;
        }

        return Do();
    }

    void run(unit::tag_t tag)
    {
        Test787_helper();
        unit::assert(tag, g_787 == 1);
    }
}

// Test for a function call site that contains
// calls to the same function as parameters
inline uint32 Helper790(uint32 a, uint32 b)
{
    return a + b;
}

// static local used inside an inline function in a module
inline void Test789(unit::tag_t tag)
{
    TestStaticLocalWithinModule_Inlined(3, 5, tag);
    TestStaticLocalWithinModule_Inlined(3, 7, tag);
}

inline void Test790(unit::tag_t tag)
{
    uint32 result = Helper790(
        Helper790(3, 4),
        Helper790(5, 6));

    unit::assert(tag, result == 18);
}

class Test791
{
private:
    // Tests for KillMoves bug that caused multiple cycles() values to be aliased
    noinline void Test791Helper0() {}
    noinline void Test791Helper1() {}

public:
    void run(unit::tag_t tag)
    {
        uint64 time0;
        uint64 time1;
        uint64 time2;

        time0 = cycles();

        Test791Helper0();

        time1 = cycles();

        Test791Helper1();

        time2 = cycles();

        unit::assert(tag, time0 < time1);
        unit::assert(tag, time1 < time2);
    }
}

inline void test_main()
{
    unit::test<567>(TestCase567);
    unit::test<587>(unit::fixture<Test587>());
    unit::test<588>(unit::fixture<Test588>());
    unit::test<589>(unit::fixture<Test589>());
    unit::test<784>(unit::fixture<Test784>());
    unit::test<785>(Test785);
    unit::test<786>(Test786);
    unit::test<787>(unit::fixture<Test787>());
    unit::test<789>(Test789);
    unit::test<790>(Test790);
    unit::test<791>(unit::fixture<Test791>());
}
