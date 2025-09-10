// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// array of objects, that contain objects
class Boo351
{
public:
    uint32 Func()
    {
        return 7;
    }
}

class Foo351
{
private:
    Boo351[2] _boo;

public:
    uint32 Entry()
    {
        return _boo[1].Func();
    }
}

class Test351
{
private:
    Foo351 g_foo351;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 7 == g_foo351.Entry());
    }
}

// array of objects, that contain objects
class Boo352
{
public:
    uint32 Func()
    {
        return 7;
    }
}

class Foo352
{
private:
    Boo352[2] _boo;

public:
    uint32 Entry()
    {
        return _boo[1].Func();
    }
}

class Test352
{
private:
    Foo352[2] g_foo352;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 7 == g_foo352[1].Entry());
    }
}

// Barrier
class Test353
{
private:
    uint32 g_a353 = 0;
    uint32 g_b353 = 0;

public:
    [[async]] void Foo353(uint32 i)
    {
        // Without the barrier, the write to b would happen first because of the long dependency chain to compute the value to write to a
        uint32 amtToAddtoA = 0;

        static for (const auto x : 100);
        {
            amtToAddtoA += i;
        }

        g_a353 += amtToAddtoA;

        barrier;

        g_b353 = 2;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        Foo353(1);

        [[unordered]] do; while (g_b353 == 0);

        unit::assert(tag, g_a353 != 0);
    }
}

// static for
inline void TestCase354(unit::tag_t tag)
{
    uint32 x = 0;

    static for (const uint32 y : 10)
    {
        x += y;
    }

    unit::assert(tag, x == 45);
}

// static for with type inference
inline void TestCase355(unit::tag_t tag)
{
    uint32 x = 0;

    static for (const auto y : 10)
    {
        x += y;

        unit::assert(tag, 4 == bitsizeof(y));
    }

    unit::assert(tag, x == 45);
}

// new static for syntax inside inline member function
class Foo357
{
public:
    uint32 Func(uint32 x)
    {
        uint32 result = x;

        static for (const auto i : 3)
        {
            result++;
        }

        return result;
    }
}

class Test357
{
private:
    Foo357 g_Foo357;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_Foo357.Func(1) == 4);
    }
}

// calling a member object
class Bar359
{
public:
    uint32 Func(uint32 x)
    {
        return x + 1;
    }
}

class Foo359
{
public:
    Bar359 _bar;
}

class Test359
{
private:
    Foo359 g_foo359;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 5 == g_foo359._bar.Func(4));
    }
}

// allow atomic function to be called before it is declared
inline void TestCase360(unit::tag_t tag)
{
    unit::assert(tag, 8 == Foo360(4));
}

inline uint32 Foo360(uint32 x)
{
    return x * 2;
}

// type-checking time constant propagation

const auto x362 = 3;
const auto y362 = x362 + 1;

inline void TestCase362(unit::tag_t tag)
{
    uint32[y362] myArray;

    myArray[3] = 1;

    unit::assert(tag, myArray[3] == 1);
}

// Size computation of a large literal
// repro of a bug that happened in DnnTrainer
inline int64 Foo366(bool b)
{
    return b ? 0xFFFFFFFFFFFFFFFF : 2;
}

inline void TestCase366(unit::tag_t tag)
{
    int64 a = Foo366(true);

    int64 b = a + 1;

    unit::assert(tag, b == 0);
}

// if with known value
inline void TestCase370(unit::tag_t tag)
{
    uint32 x = 3;

    if (true)
    {
        x = 4;
    }

    unit::assert(tag, x == 4);
}

// if with known value
inline void TestCase371(unit::tag_t tag)
{
    uint32 x = 3;

    if (false)
    {
        x = 4;
    }

    unit::assert(tag, x == 3);
}

// if with known value
inline void TestCase372(unit::tag_t tag)
{
    uint32 x = 3;

    if (true)
    {
        x = 4;
    }
    else
    {
        x = 5;
    }

    unit::assert(tag, x == 4);
}

// if with known value
inline void TestCase373(unit::tag_t tag)
{
    uint32 x = 3;

    if (false)
    {
        x = 4;
    }
    else
    {
        x = 5;
    }

    unit::assert(tag, x == 5);
}

// nested arays of objects
class Bar374
{
public:
    uint32 Func()
    {
        return 7;
    }
}

class Foo374
{
    Bar374[2] _member;

public:
    uint32 Frob()
    {
        return _member[0].Func();
    }
}


class Test374
{
private:    
    Foo374 g_foo374;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = g_foo374.Frob();

        unit::assert(tag, x == 7);
    }
}

// inline that returns struct
struct S375
{
    uint32 x;
    bool b;
}

inline S375 Foo375(uint32 input)
{
    S375 s;

    s.x = input + 1;
    s.b = input > 10;

    return s;
}

inline void TestCase375(unit::tag_t tag)
{
    S375 result = Foo375(11);

    unit::assert(tag, result.x == 12);
    unit::assert(tag, result.b == true);
}

// cycle counter
inline void TestCase376(unit::tag_t tag)
{
    uint64 cycleCountOld = cycles();

    barrier;

    uint64 cycleCountNew = cycles();

    uint64 diff = cycleCountNew - cycleCountOld;

    unit::assert(tag, diff > 0);
}

// concat
inline void TestCase377(unit::tag_t tag)
{
    uint3 a = 5;
    bool b = false;
    bool c = true;
    int2 d = -1;

    uint7 result = concat(d, c, b, a);

    unit::assert(tag, result == 117);
}

// float->uint cast
inline void TestCase378(unit::tag_t tag)
{
    float32 f = 3.25;

    uint32 i = cast<uint32>(f);

    unit::assert(tag, i == 0x40500000);
}

// negative float literal
inline void TestCase379(unit::tag_t tag)
{
    float32 f = -7.125;

    uint32 i = cast<uint32>(f);

    unit::assert(tag, i == 0xc0e40000);
}

inline void test_main()
{
    unit::test<351>(unit::fixture<Test351>());
    unit::test<352>(unit::fixture<Test352>());
    unit::test<353>(unit::fixture<Test353>());
    unit::test<354>(TestCase354);
    unit::test<355>(TestCase355);
    unit::test<357>(unit::fixture<Test357>());
    unit::test<359>(unit::fixture<Test359>());
    unit::test<360>(TestCase360);
    unit::test<362>(TestCase362);
    unit::test<366>(TestCase366);
    unit::test<370>(TestCase370);
    unit::test<371>(TestCase371);
    unit::test<372>(TestCase372);
    unit::test<373>(TestCase373);
    unit::test<374>(unit::fixture<Test374>());
    unit::test<375>(TestCase375);
    unit::test<376>(TestCase376);
    unit::test<377>(TestCase377);
    unit::test<378>(TestCase378);
    unit::test<379>(TestCase379);
}
