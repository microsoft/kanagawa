// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

inline void TestCase50(unit::tag_t tag)
{
    uint3 x = 4;
    uint4 y = -x;
    unit::assert(tag, y == 12);
}


inline void TestCase51(unit::tag_t tag)
{
    uint3 x = 7;
    int4 y = -x;
    unit::assert(tag, y == -7);
}


inline void TestCase52(unit::tag_t tag)
{
    uint3 x = 1;
    int3 y = -x;
    unit::assert(tag, y == -1);
}


inline void TestCase53(unit::tag_t tag)
{
    int3 x = -1;
    int3 y = ~x;
    unit::assert(tag, y == 0);
}


inline void TestCase54(unit::tag_t tag)
{
    int3 x = -1;
    uint3 y = ~x;
    unit::assert(tag, y == 0);
}


inline void TestCase55(unit::tag_t tag)
{
    int64 x = -1;
    int64 y = x >> 63;
    unit::assert(tag, y == -1);
}


inline void TestCase56(unit::tag_t tag)
{
    uint3 x;
    uint3 y;

    atomic
    {
        x = 3;
        y = x + 2;
    }

    unit::assert(tag, y == 5);
}


inline void TestCase57(unit::tag_t tag)
{
    uint32[4] array;

    array[0] = 3;
    array[1] = 4;
    array[2] = 5;
    array[3] = 6;

    const uint1 index = 1;
    uint32 y = array[index];

    unit::assert(tag, y == 4);
}



inline void TestCase58(unit::tag_t tag)
{
    uint32[4] array;

    array[0] = 3;
    array[1] = 4;
    array[2] = 5;
    array[3] = 6;

    const uint3 index = 2;
    uint32 y = array[index];

    unit::assert(tag, y == 5);
}


struct Foo59
{
    uint3 x;
    uint7 y;
}

inline void TestCase59(unit::tag_t tag)
{
    Foo59 foo;

    unit::assert(tag, bitsizeof(foo) == 10);
}


inline void TestCase61(unit::tag_t tag)
{
    uint3[5] x;

    unit::assert(tag, bitsizeof(x) == 15);
}


inline void TestCase62(unit::tag_t tag)
{
    uint3[16] x;

    unit::assert(tag, bytesizeof(x) == 6);
}


const uint3 x = 2;

inline void TestCase63(unit::tag_t tag)
{
    uint3 y = x + 1;

    unit::assert(tag, y == 3);
}


class TestCase64
{
private:
    uint3 x64 = 2;

public:
    void run(unit::tag_t tag)
    {
        uint3 y = x64 + 1;

        unit::assert(tag, y == 3);
    }
}


class TestCase65
{
private:
    uint3 x65 = 2;

public:
    void run(unit::tag_t tag)
    {
        x65 = x65 + 1;

        uint3 y = x65 + 1;

        unit::assert(tag, y == 4);
    }
}


inline void TestCase66(unit::tag_t tag)
{
    uint32 b;

    if (3 < 4)
    {
        b = 1;
    }
    else
    {
        b = 2;
    }

    unit::assert(tag, b == 1);
}


inline void TestCase67(unit::tag_t tag)
{
    uint32 b;

    if (5 < 4)
    {
        b = 1;
    }
    else
    {
        b = 2;
    }

    unit::assert(tag, b == 2);
}


[[unordered]] inline void TestCase68(unit::tag_t tag)
{
    uint32 sum = 0;

    [[unordered]] for (const auto i : 10)
    {
        sum = sum + i;
    }

    unit::assert(tag, sum == 45);
}


inline uint32 Foo69()
{
    return 4;
}

inline void TestCase69(unit::tag_t tag)
{
    uint32 x = Foo69();

    unit::assert(tag, x == 4);
}


// 1 function called from 2 call sites
inline uint32 Foo70()
{
    return 4;
}

inline void TestCase70(unit::tag_t tag)
{
    uint32 x = Foo70() + Foo70();

    unit::assert(tag, x == 8);
}


inline uint32 Foo71(uint32 x)
{
    return x + 4;
}

inline void TestCase71(unit::tag_t tag)
{
    uint32 x = Foo71(8);

    unit::assert(tag, x == 12);
}


// sign extension for parameter
inline int32 Foo72(int32 x)
{
    return x - 4;
}

inline void TestCase72(unit::tag_t tag)
{
    int4 x = -3;

    int32 y = Foo72(x);

    unit::assert(tag, y == -7);
}


inline uint32 Foo73(uint8 x)
{
    return x + 1;
}

inline void TestCase73(unit::tag_t tag)
{
    int4 x = -3;

    // -3 is sign extended to 8 bits (11111101)
    // 1 is added to it by Foo to yeild 11111110 (254)
    uint32 y = Foo73(x);

    unit::assert(tag, y == 254);
}


// sign extension for return value
inline int3 Foo74()
{
    return -3;
}

inline void TestCase74(unit::tag_t tag)
{
    int8 x = Foo74();

    unit::assert(tag, x == -3);
}

inline void test_main()
{
    unit::test<50>(TestCase50);
    unit::test<51>(TestCase51);
    unit::test<52>(TestCase52);
    unit::test<53>(TestCase53);
    unit::test<54>(TestCase54);
    unit::test<55>(TestCase55);
    unit::test<56>(TestCase56);
    unit::test<57>(TestCase57);
    unit::test<58>(TestCase58);
    unit::test<59>(TestCase59);
    unit::test<61>(TestCase61);
    unit::test<62>(TestCase62);
    unit::test<63>(TestCase63);

    unit::test<64>(unit::fixture<TestCase64>());
    unit::test<65>(unit::fixture<TestCase65>());
    
    unit::test<66>(TestCase66);
    unit::test<67>(TestCase67);
    unit::test<68>(TestCase68);
    unit::test<69>(TestCase69);
    unit::test<70>(TestCase70);
    unit::test<71>(TestCase71);
    unit::test<72>(TestCase72);
    unit::test<73>(TestCase73);
    unit::test<74>(TestCase74);
}
