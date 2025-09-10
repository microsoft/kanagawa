// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// pre-increment
inline void TestCase204(unit::tag_t tag)
{
    uint32 i = 0;

    ++i;

    unit::assert(tag, i == 1);
}

// pre-decrement
inline void TestCase205(unit::tag_t tag)
{
    uint32 i = 3;

    --i;

    unit::assert(tag, i == 2);
}

// signed <
inline void TestCase206(unit::tag_t tag)
{
    int8 x = -5;
    int8 y = 6;

    unit::assert(tag, x < y);
}

// unsigned comparisons greater than 64-bits
inline void TestCase207(unit::tag_t tag)
{
    uint96 x = 0xffffffff << 36;
    uint96 y = 0xffffffff << 37;

    unit::assert(tag, x < y);
    unit::assert(tag, x <= y);
    unit::assert(tag, y > x);
    unit::assert(tag, y >= x);
}

// signed comparisons greater than 64-bits
inline void TestCase208(unit::tag_t tag)
{
    int96 x = 0xffffffff << 36;
    int96 y = 0xffffffff << 37;

    unit::assert(tag, x < y);
    unit::assert(tag, x <= y);
    unit::assert(tag, y > x);
    unit::assert(tag, y >= x);
}

// signed comparisons greater than 64-bits
inline void TestCase209(unit::tag_t tag)
{
    int96 x = -1;
    int96 y = 0xffffffff << 37;

    unit::assert(tag, x < y);
    unit::assert(tag, x <= y);
    unit::assert(tag, y > x);
    unit::assert(tag, y >= x);
}

// addition greater than 64-bits
inline void TestCase210(unit::tag_t tag)
{
    int96 x = -1;
    int96 y = 0xffffffff << 37;

    int96 sum = y + x;

    unit::assert(tag, sum < y);
    unit::assert(tag, (sum + 1) == y);
}

// reading an array with a dynamic index
class TestCase212
{
private:
    uint32 g_index212 = 2;

public:
    void run(unit::tag_t tag)
    {
        uint32[4] ary;
    
        ary[0] = 1;
        ary[1] = 2;
        ary[2] = 3;
        ary[3] = 4;
    
        uint32 result = ary[g_index212];
    
        unit::assert(tag, result == 3);
    }
}

// writing to an array with a dynamic index
class TestCase213
{
private:
    uint32 g_index213 = 1;

public:
    void run(unit::tag_t tag)
    {
        uint32[4] ary;
    
        ary[0] = 1;
        ary[1] = 2;
        ary[2] = 3;
        ary[3] = 4;
    
        ary[g_index213 + 1] = 7;
    
        unit::assert(tag, ary[0] == 1);
        unit::assert(tag, ary[1] == 2);
        unit::assert(tag, ary[2] == 7);
        unit::assert(tag, ary[3] == 4);
    }
}

// prediated dynamic array write
class TestCase214
{
private:
    uint32 g_index214 = 1;
    bool g_b214 = false;

public:
    void run(unit::tag_t tag)
    {
        uint32[4] ary;
    
        ary[0] = 1;
        ary[1] = 2;
        ary[2] = 3;
        ary[3] = 4;
    
        if (g_b214)
        {
            ary[g_index214 + 1] = 7;
        }
    
        unit::assert(tag, ary[0] == 1);
        unit::assert(tag, ary[1] == 2);
        unit::assert(tag, ary[2] == 3);
        unit::assert(tag, ary[3] == 4);
    
        g_b214 = true;
    
        if (g_b214)
        {
            ary[g_index214 + 1] = 7;
        }
    
        unit::assert(tag, ary[0] == 1);
        unit::assert(tag, ary[1] == 2);
        unit::assert(tag, ary[2] == 7);
        unit::assert(tag, ary[3] == 4);
    }
}

// dynamic array write (global)
class TestCase215
{
private:
    uint32 g_index215 = 1;
    uint32[4] g_ary215;

public:
    void run(unit::tag_t tag)
    {
        g_ary215[0] = 1;
        g_ary215[1] = 2;
        g_ary215[2] = 3;
        g_ary215[3] = 4;
    
        g_ary215[g_index215 + 1] = 7;
    
        unit::assert(tag, g_ary215[0] == 1);
        unit::assert(tag, g_ary215[1] == 2);
        unit::assert(tag, g_ary215[2] == 7);
        unit::assert(tag, g_ary215[3] == 4);
    }
}

// prediated dynamic array write (global)
class TestCase216
{
private:
    uint32 g_index216 = 1;
    bool g_b216 = false;
    uint32[4] g_ary216;

public:
    void run(unit::tag_t tag)
    {
        g_ary216[0] = 1;
        g_ary216[1] = 2;
        g_ary216[2] = 3;
        g_ary216[3] = 4;
    
        if (g_b216)
        {
            g_ary216[g_index216 + 1] = 7;
        }
    
        unit::assert(tag, g_ary216[0] == 1);
        unit::assert(tag, g_ary216[1] == 2);
        unit::assert(tag, g_ary216[2] == 3);
        unit::assert(tag, g_ary216[3] == 4);
    
        g_b216 = true;
    
        if (g_b216)
        {
            g_ary216[g_index216 + 1] = 7;
        }
    
        unit::assert(tag, g_ary216[0] == 1);
        unit::assert(tag, g_ary216[1] == 2);
        unit::assert(tag, g_ary216[2] == 7);
        unit::assert(tag, g_ary216[3] == 4);
    }
}

// 2-dimensional array - dynamic read
class TestCase217
{
private:
    uint32 g_index217_1 = 3;
    uint32 g_index217_2 = 1;

public:
    void run(unit::tag_t tag)
    {
        // 4 rows, 2 columns
        uint32[4][2] a;
    
        a[0][0] = 1;
        a[0][1] = 2;
    
        a[1][0] = 3;
        a[1][1] = 4;
    
        a[2][0] = 5;
        a[2][1] = 6;
    
        a[3][0] = 7;
        a[3][1] = 8;
    
        unit::assert(tag, a[g_index217_1][g_index217_2] == 8);
    }
}

// 2-dimensional array - dynamic write
class TestCase218
{
private:
    uint32 g_index218_1 = 3;
    uint32 g_index218_2 = 1;    

public:
    void run(unit::tag_t tag)
    {
        // 4 rows, 2 columns
        uint32[4][2] a;
    
        a[0][0] = 1;
        a[0][1] = 2;
    
        a[1][0] = 3;
        a[1][1] = 4;
    
        a[2][0] = 5;
        a[2][1] = 6;
    
        a[3][0] = 7;
        a[3][1] = 8;
    
        a[g_index218_1][g_index218_2] = 11;
    
        unit::assert(tag, a[3][1] == 11);
    }
}

// reading an array with a dynamic index - struct
class TestCase219
{
private:
    uint32 g_index219 = 2;
    struct Foo219
    {
        uint32 x;
        uint32 y;
    }

public:
    void run(unit::tag_t tag)
    {
        Foo219[4] ary;
    
        Foo219 foo;
    
        foo.x = 1;
        foo.y = 2;
        ary[0] = foo;
    
        foo.x = 3;
        foo.y = 4;
        ary[1] = foo;
    
        foo.x = 5;
        foo.y = 6;
        ary[2] = foo;
    
        foo.x = 7;
        foo.y = 8;
        ary[3] = foo;
    
        foo = ary[g_index219];
    
        unit::assert(tag, foo.x == 5);
        unit::assert(tag, foo.y == 6);
    }
}

// writing an array with a dynamic index - struct
class TestCase220
{
private:
    uint32 g_index220 = 2;
    struct Foo220
    {
        uint32 x;
        uint32 y;
    }

public:
    void run(unit::tag_t tag)
    {
        Foo220[4] ary;
    
        Foo220 foo;
    
        foo.x = 1;
        foo.y = 2;
        ary[0] = foo;
    
        foo.x = 3;
        foo.y = 4;
        ary[1] = foo;
    
        foo.x = 5;
        foo.y = 6;
        ary[2] = foo;
    
        foo.x = 7;
        foo.y = 8;
        ary[3] = foo;
    
        foo.x = 9;
        foo.y = 10;
        ary[g_index220] = foo;
    
        unit::assert(tag, ary[2].x == 9);
        unit::assert(tag, ary[2].y == 10);
    
        unit::assert(tag, ary[0].x == 1);
        unit::assert(tag, ary[0].y == 2);
    
        unit::assert(tag, ary[1].x == 3);
        unit::assert(tag, ary[1].y == 4);
    
        unit::assert(tag, ary[3].x == 7);
        unit::assert(tag, ary[3].y == 8);
    }
}

// out of bounds dynamic array read
class TestCase221
{
private:
    uint32 g_index221 = 5;

public:
    void run(unit::tag_t tag)
    {
        uint32[4] ary;
    
        ary[0] = 2;
        ary[1] = 3;
        ary[2] = 4;
        ary[3] = 5;
    
        // The read should wrap
        unit::assert(tag, ary[g_index221] == 3);
    }
}

// out of bounds dynamic array read - 2D
class TestCase222
{
private:
    uint32 g_index222_1 = 3;
    uint32 g_index222_2 = 0;

public:
    void run(unit::tag_t tag)
    {
        // 2 rows, 4 columns
        uint32[2][4] ary;
    
        ary[0][0] = 1;
        ary[0][1] = 2;
        ary[0][2] = 3;
        ary[0][3] = 4;
    
        ary[1][0] = 5;
        ary[1][1] = 6;
        ary[1][2] = 7;
        ary[1][3] = 8;
    
        // The array is linearized, and then the read wraps
        unit::assert(tag, ary[g_index222_1][g_index222_2] == 5);
    }
}

// Inline member function returning a class variable
class Foo223
{
private:
    uint32 _x = 2;

public:
    inline uint32 GetX()
    {
        return _x;
    }
}

class TestCase223
{
private:
    Foo223 g_foo223;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_foo223.GetX() == 2);
    }
}

// Inline member function calling another inline member function
class Foo224
{
private:
    uint32 _x = 2;

public:
    inline uint32 GetX()
    {
        return _x;
    }

    inline uint32 GetXPlus2()
    {
        return GetX() + 2;
    }
}

class TestCase224
{
private:
    Foo224 g_foo224;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_foo224.GetXPlus2() == 4);
    }
}

inline void test_main()
{
    unit::test<204>(TestCase204);
    unit::test<205>(TestCase205);
    unit::test<206>(TestCase206);
    unit::test<207>(TestCase207);
    unit::test<208>(TestCase208);
    unit::test<209>(TestCase209);
    unit::test<210>(TestCase210);
    unit::test<212>(unit::fixture<TestCase212>());
    unit::test<213>(unit::fixture<TestCase213>());
    unit::test<214>(unit::fixture<TestCase214>());
    unit::test<215>(unit::fixture<TestCase215>());
    unit::test<216>(unit::fixture<TestCase216>());
    unit::test<217>(unit::fixture<TestCase217>());
    unit::test<218>(unit::fixture<TestCase218>());
    unit::test<219>(unit::fixture<TestCase219>());
    unit::test<220>(unit::fixture<TestCase220>());
    unit::test<221>(unit::fixture<TestCase221>());
    unit::test<222>(unit::fixture<TestCase222>());
    unit::test<222>(unit::fixture<TestCase223>());
    unit::test<222>(unit::fixture<TestCase224>());
}
