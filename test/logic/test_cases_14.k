// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// basic union test
union Foo300
{
    uint32 a;
    uint32 b;
};

inline void TestCase300(unit::tag_t tag)
{
    Foo300 f;

    f.a = 3;

    unit::assert(tag, f.b == 3);

    f.b = 4;

    unit::assert(tag, f.a == 4);
}

// Struct that contains a union
union Foo301
{
    uint32 a;
    uint32 b;
};

struct Bar301
{
    uint32 x;
    Foo301 f;
}

inline void TestCase301(unit::tag_t tag)
{
    Bar301 b;

    b.f.a = 4;
    b.x = 2;

    unit::assert(tag, b.x == 2);
    unit::assert(tag, b.f.a == 4);
    unit::assert(tag, b.f.b == 4);
}

// Union that contains a struct
struct Struct1_302
{
    uint8 a;
    uint8 b;
}

struct Struct2_302
{
    uint32 x;
    uint32 y;
}

union Foo302
{
    Struct1_302 s1;
    Struct2_302 s2;
}

inline void TestCase302(unit::tag_t tag)
{
    Foo302 f;

    Struct1_302 s1;
    Struct2_302 s2;

    s1.a = 8;
    f.s1 = s1;
    s2 = f.s2;

    unit::assert(tag, s2.x == 8);
    unit::assert(tag, s2.y == 0);

    s2.x = 256;
    f.s2 = s2;
    s1 = f.s1;

    unit::assert(tag, s1.a == 0);
    unit::assert(tag, s1.b == 1);
}

// Array of unions
union Foo303
{
    uint16 x;
    uint8 y;
}

inline void TestCase303(unit::tag_t tag)
{
    Foo303[4] a;

    Foo303 f;
    f.x = 1;

    a[1] = f;

    f = a[1];
    unit::assert(tag, f.y == 1);
}

// partial update to union
struct Boo304
{
    uint32 x;
}

union Foo304
{
    Boo304 b;
}

inline void TestCase304(unit::tag_t tag)
{
    Foo304 f;

    f.b.x = 3;

    unit::assert(tag, f.b.x == 3);
}

// modifying member variables in an array of objects
class Foo305
{
public:
    uint32 _x;
}

class Test305
{
private:
    Foo305[8] g_objects305;

public:
    void run(unit::tag_t tag)
    {
        static for (const auto i : 8)
        {
            g_objects305[i]._x = i + 1;
        }

        static for (const auto i : 8)
        {
            unit::assert(tag, g_objects305[i]._x == (i + 1));
        }
    }
}

// Printing booleans
inline void TestCase306(unit::tag_t tag)
{
    bool b = true;

    print("Should be true:  {b}\n");
}

// Printing structs
struct Foo307
{
    bool b;
    uint32 x;
}

inline void TestCase307(unit::tag_t tag)
{
    Foo307 f;

    f.b = false;
    f.x = 3;

    print("Struct  {f}\n");
}

// sizeof (memory)
class Test308
{
private:
    [[memory]] uint32[8] g_mem308;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, bitsizeof(g_mem308) == 256);
    }
}

// +=
inline void TestCase309(unit::tag_t tag)
{
    uint32 x = 7;
    x += 4;
    unit::assert(tag, x == 11);
}

// -=
inline void TestCase310(unit::tag_t tag)
{
    uint32 x = 7;
    x -= 4;
    unit::assert(tag, x == 3);
}

// *=
inline void TestCase311(unit::tag_t tag)
{
    uint32 x = 7;
    x *= 4;
    unit::assert(tag, x == 28);
}

// /=
inline void TestCase312(unit::tag_t tag)
{
    uint32 x = 8;
    x /= 4;
    unit::assert(tag, x == 2);
}

// %=
inline void TestCase313(unit::tag_t tag)
{
    uint32 x = 5;
    x %= 4;
    unit::assert(tag, x == 1);
}

// &=
inline void TestCase314(unit::tag_t tag)
{
    uint32 x = 14;
    x &= 7;
    unit::assert(tag, x == 6);
}

// |=
inline void TestCase315(unit::tag_t tag)
{
    uint32 x = 6;
    x |= 1;
    unit::assert(tag, x == 7);
}

// ^=
inline void TestCase316(unit::tag_t tag)
{
    uint32 x = 6;
    x ^= 4;
    unit::assert(tag, x == 2);
}

// &&=
inline void TestCase317(unit::tag_t tag)
{
    bool x = true;

    x &&= true;
    unit::assert(tag, x);

    x &&= false;
    unit::assert(tag, !x);
}

// ||=
inline void TestCase318(unit::tag_t tag)
{
    bool x = false;

    x ||= false;
    unit::assert(tag, !x);

    x ||= true;
    unit::assert(tag, x);
}

// ^^=
inline void TestCase319(unit::tag_t tag)
{
    bool x = false;

    x ^^= false;
    unit::assert(tag, !x);

    x ^^= true;
    unit::assert(tag, x);

    x ^^= true;
    unit::assert(tag, !x);
}

// member objects
class TileInputMemory320
{
private:
    [[memory]] uint512[1024] _mem;

public:
    uint512 Load(uint10 addr)
    {
        return _mem[addr];
    }

    void Store(uint10 addr, uint512 data)
    {
        _mem[addr] = data;
    }
}

class TileInputMemoryWithTranspose320
{
private:
    TileInputMemory320 _tileMem;

public:
    // only has 1 call site
    uint512 Read(uint10 addr)
    {
        return _tileMem.Load(addr);
    }

    void Write(uint10 addr, uint512 data)
    {
        _tileMem.Store(addr, data);
    }
}

class Test320
{
private:
    TileInputMemoryWithTranspose320 g_tileInputMemory320_1;
    TileInputMemoryWithTranspose320 g_tileInputMemory320_2;

public:
    void run(unit::tag_t tag)
    {
        g_tileInputMemory320_1.Write(1, 2);
        unit::assert(tag, 2 == g_tileInputMemory320_1.Read(1));

        g_tileInputMemory320_2.Write(4, 5);
        unit::assert(tag, 5 == g_tileInputMemory320_2.Read(4));
    }
}

// 2d array of objects
class Foo321
{
private:
    uint8 _v;

public:
    void Set(uint8 in)
    {
        _v = in;
    }

    uint8 Get()
    {
        return _v;
    }
}

// 4 rows, 2 columns
class Test321
{
private:
    Foo321[4][2] g_f321;

public:
    void run(unit::tag_t tag)
    {
        static for (const auto y : 2)
        {
            static for (const auto x : 4)
            {
                uint8 v = y * 4 + x;

                g_f321[x][y].Set(v);
            }
        }

        static for (const auto y : 2)
        {
            static for (const auto x : 4)
            {
                uint8 expected = y * 4 + x;

                uint8 actual = g_f321[x][y].Get();

                unit::assert(tag, expected == actual);
            }
        }
    }
}

// member object
class Bar322
{
public:
    uint32 GetSeven()
    {
        return 7;
    }
}

class Foo322
{
private:
    Bar322 _bar;

public:
    uint32 Get()
    {
        return _bar.GetSeven();
    }
}

class Test322
{
private:
    Foo322 g_f322;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 7 == g_f322.Get());
    }
}

// reading a 1D slice of a 2D array
[[unordered]] inline void TestCase324(unit::tag_t tag)
{
    uint32[4][2] ary;

    [[unordered]] for (const uint32 y : 3)
    {
        [[unordered]] for (const uint32 x : 2)
        {
            ary[y][x] = x + (y * 4);
        }
    }

    uint32[2] slice = ary[1];

    unit::assert(tag, slice[0] == 4);
    unit::assert(tag, slice[1] == 5);
}

// writing a 1D slice of a 2D array
inline void TestCase325(unit::tag_t tag)
{
    uint32[4][2] ary;

    uint32[2] slice;

    slice[0] = 7;
    slice[1] = 10;

    ary[2] = slice;

    unit::assert(tag, ary[2][0] == 7);
    unit::assert(tag, ary[2][1] == 10);
}

inline void test_main()
{
    unit::test<300>(TestCase300);
    unit::test<301>(TestCase301);
    unit::test<302>(TestCase302);
    unit::test<303>(TestCase303);
    unit::test<304>(TestCase304);
    unit::test<305>(unit::fixture<Test305>());
    unit::test<306>(TestCase306);
    unit::test<307>(TestCase307);
    unit::test<308>(unit::fixture<Test308>());
    unit::test<309>(TestCase309);
    unit::test<310>(TestCase310);
    unit::test<311>(TestCase311);
    unit::test<312>(TestCase312);
    unit::test<313>(TestCase313);
    unit::test<314>(TestCase314);
    unit::test<315>(TestCase315);
    unit::test<316>(TestCase316);
    unit::test<317>(TestCase317);
    unit::test<318>(TestCase318);
    unit::test<319>(TestCase319);
    unit::test<320>(unit::fixture<Test320>());
    unit::test<321>(unit::fixture<Test321>());
    unit::test<322>(unit::fixture<Test322>());
    unit::test<324>(TestCase324);
    unit::test<325>(TestCase325);
}
