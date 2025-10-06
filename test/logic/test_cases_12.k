// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Predicated assert
class TestCase250
{
private:
    bool g_b250 = true;

public:
    void run(unit::tag_t tag)
    {
        if(g_b250)
        {
            unit::assert(tag, true);
        }
    }
}

// Predicated assert
class TestCase251
{
private:
    bool g_b251 = false;

public:
    void run(unit::tag_t tag)
    {
        if(g_b251)
        {
            unit::assert(tag, false);
        }
    }
}

// Predicated assert
class TestCase252
{
private:
    bool g_b252 = false;

public:
    void run(unit::tag_t tag)
    {
        if(g_b252)
        {
            unit::assert(tag, true);
        }
    }
}

// Predicated print
class TestCase253
{
private:
    bool g_b253 = true;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 4;

        if(g_b253)
        {
            print("Print inside of flat_if {x}\n");
        }
    }
}

// multiply by power of 2 (rhs)
inline void TestCase256(unit::tag_t tag)
{
   uint32 x = 5;

   uint32 y = static_cast(x * 4);

   unit::assert(tag, y == 20);
}

// multiply by power of 2 (lhs)
inline void TestCase257(unit::tag_t tag)
{
   uint32 x = 5;

   uint32 y = static_cast(4 * x);

   unit::assert(tag, y == 20);
}

// divide by power of 2
[[unordered]] inline void TestCase258(unit::tag_t tag)
{
   [[unordered]] for (const uint32 x : 100)
   {
        uint32 y = x / 2;

        unit::assert(tag, y == (x >> 1));
   }
}

// divide by power of 2
[[unordered]] inline void TestCase259(unit::tag_t tag)
{
   [[unordered]] for (const uint32 x : 100)
   {
        uint32 y = x / 4;

        unit::assert(tag, y == (x >> 2));
   }
}

// % by power of 2
[[unordered]] inline void TestCase260(unit::tag_t tag)
{
   [[unordered]] for (const uint32 x : 100)
   {
        uint32 y = x % 4;

        unit::assert(tag, y == (x & 3));
   }
}

// % by power of 2 (constant)
[[unordered]] inline void TestCase261(unit::tag_t tag)
{
   const uint32 divisor = 8;

   [[unordered]] for (const uint32 x : 100)
   {
        uint32 y = x % divisor;

        unit::assert(tag, y == (x & 7));
   }
}

// * precedence
inline void TestCase262(unit::tag_t tag)
{
    unit::assert(tag, (4 * 3 + 2) == 14);
}

// * precedence
inline void TestCase263(unit::tag_t tag)
{
    unit::assert(tag, (4 * 3 - 2) == 10);
}

// / precedence
inline void TestCase264(unit::tag_t tag)
{
    unit::assert(tag, (4 / 2 + 2) == 4);
}

// % precedence
inline void TestCase265(unit::tag_t tag)
{
    unit::assert(tag, (1 % 2 + 2) == 3);
}

// left-to-right associativity
inline void TestCase266(unit::tag_t tag)
{
    unit::assert(tag, (4 * 3 % 8 / 2) == 2);
}

// Type inference
inline void TestCase267(unit::tag_t tag)
{
    uint32 y = 5;

    auto z = y + 1;

    unit::assert(tag, z == 6);
    unit::assert(tag, bitsizeof(z) == 33);
}

// const auto
inline void TestCase268(unit::tag_t tag)
{
    uint32 y = 5;

    const auto z = y + 1;

    unit::assert(tag, z == 6);
    unit::assert(tag, bitsizeof(z) == 33);
}

// type inference in a loop
[[unordered]] inline void TestCase269(unit::tag_t tag)
{
    uint32 sum = 0;

    [[unordered]] for (const auto x : 5)
    {
        unit::assert(tag, bitsizeof(x) == 3);
        sum = sum + x;
    }

    unit::assert(tag, sum == 10);
}

// Auto global variable
const uint32 g_x271 = 4;
const auto g_y271 = g_x271;

inline void TestCase271(unit::tag_t tag)
{
    unit::assert(tag, bitsizeof(g_y271) == 32);
    unit::assert(tag, g_y271 == 4);
}

// auto struct
struct Foo272
{
    uint3 x;
    uint4 y;
}

inline void TestCase272(unit::tag_t tag)
{
    Foo272 f;

    f.x = 1;
    f.y = 2;

    auto g = f;

    unit::assert(tag, bitsizeof(g) == 7);
    unit::assert(tag, g.x == 1);
    unit::assert(tag, g.y == 2);

}

// auto array
inline void TestCase273(unit::tag_t tag)
{
    uint32[2] f;

    f[0] = 1;
    f[1] = 3;

    auto g = f;

    unit::assert(tag, bitsizeof(g) == 64);
    unit::assert(tag, g[0] == 1);
    unit::assert(tag, g[1] == 3);
}

// Array size is not a literal
const uint32 g_size274 = 4;

inline void TestCase274(unit::tag_t tag)
{
    uint8[g_size274] ary;

    static for(const auto i : g_size274)
    {
        ary[i] = i;
    }

    unit::assert(tag, ary[0] == 0);
    unit::assert(tag, ary[1] == 1);
    unit::assert(tag, ary[2] == 2);
    unit::assert(tag, ary[3] == 3);

    unit::assert(tag, bytesizeof(ary) == 4);
}

// Array size is not a literal
inline void TestCase275(unit::tag_t tag)
{
    const uint32 size275 = 4;

    uint8[size275] ary;

    static for(const auto i : size275)
    {
        ary[i] = i;
    }

    unit::assert(tag, ary[0] == 0);
    unit::assert(tag, ary[1] == 1);
    unit::assert(tag, ary[2] == 2);
    unit::assert(tag, ary[3] == 3);

    unit::assert(tag, bytesizeof(ary) == 4);
}

// Memory size is not a literal
class Test276
{
private:
    const uint32 g_size276 = 4;

    [[memory]] uint8[g_size276] g_ary276;

public:
    [[unordered]] void run(unit::tag_t tag)
    {
        [[unordered]] for(const uint32 i : g_size276)
        {
            g_ary276[i] = i;
        }

        unit::assert(tag, g_ary276[0] == 0);
        unit::assert(tag, g_ary276[1] == 1);
        unit::assert(tag, g_ary276[2] == 2);
        unit::assert(tag, g_ary276[3] == 3);
    }
}

inline void test_main()
{
    unit::test<250>(unit::fixture<TestCase250>());
    unit::test<251>(unit::fixture<TestCase251>());
    unit::test<252>(unit::fixture<TestCase252>());
    unit::test<253>(unit::fixture<TestCase253>());
    unit::test<256>(TestCase256);
    unit::test<257>(TestCase257);
    unit::test<258>(TestCase258);
    unit::test<259>(TestCase259);
    unit::test<260>(TestCase260);
    unit::test<261>(TestCase261);
    unit::test<262>(TestCase262);
    unit::test<263>(TestCase263);
    unit::test<264>(TestCase264);
    unit::test<265>(TestCase265);
    unit::test<266>(TestCase266);
    unit::test<267>(TestCase267);
    unit::test<268>(TestCase268);
    unit::test<269>(TestCase269);
    unit::test<271>(TestCase271);
    unit::test<272>(TestCase272);
    unit::test<273>(TestCase273);
    unit::test<274>(TestCase274);
    unit::test<275>(TestCase275);
    unit::test<276>(unit::fixture<Test276>());
}
