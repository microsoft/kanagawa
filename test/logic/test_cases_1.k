// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

inline void TestCase1(unit::tag_t tag)
{
    uint2 index = 1;
    unit::assert(tag, 6 == mux(index, 7, 6, 5, 4));
}


inline void TestCase2(unit::tag_t tag)
{
    uint2 index = 2;
    unit::assert(tag, 5 == mux(index, 7, 6, 5, 4));
}


inline void TestCase3(unit::tag_t tag)
{
    uint2 index = 3;
    unit::assert(tag, 4 == mux(index, 7, 6, 5, 4));
}


// signed comparison
inline void TestCase5(unit::tag_t tag)
{
    int32 x = -5;
    int32 y = 1;

    unit::assert(tag, y > x);
}


// signed comparison
inline void TestCase6(unit::tag_t tag)
{
    int32 x = -5;
    int32 y = 1;

    unit::assert(tag, y >= x);
}


// signed comparison
inline void TestCase7(unit::tag_t tag)
{
    int32 x = -5;
    int32 y = 1;

    unit::assert(tag, x < y);
}


// signed comparison
inline void TestCase8(unit::tag_t tag)
{
    int32 x = -5;
    int32 y = 1;

    unit::assert(tag, x <= y);
}


inline void TestCase9(unit::tag_t tag)
{
    uint8 x = 12;
    print("my value  {x}\n");
    unit::assert(tag, 1 == 1);
}

inline void TestCase13(unit::tag_t tag)
{
    unit::assert(tag, 3 == (1 + 2));
}


inline void TestCase14(unit::tag_t tag)
{
    unit::assert(tag, 3 == (7 - 4));
}


inline void TestCase15(unit::tag_t tag)
{
    unit::assert(tag, 3 == (7 & 3));
}


inline void TestCase16(unit::tag_t tag)
{
    unit::assert(tag, 7 == (5 | 3));
}


inline void TestCase17(unit::tag_t tag)
{
    unit::assert(tag, 6 == (5 ^ 3));
}


inline void TestCase18(unit::tag_t tag)
{
    unit::assert(tag, 16 == (2 << 3));
}


inline void TestCase19(unit::tag_t tag)
{
    unit::assert(tag, 1 == (5 >> 2));
}


inline void TestCase20(unit::tag_t tag)
{
    unit::assert(tag, 1 != 2);
}


inline void TestCase21(unit::tag_t tag)
{
    unit::assert(tag, 2 > 1);
}


inline void TestCase22(unit::tag_t tag)
{
    unit::assert(tag, 2 >= 1);
}


inline void TestCase23(unit::tag_t tag)
{
    unit::assert(tag, 2 >= 2);
}


inline void TestCase24(unit::tag_t tag)
{
    unit::assert(tag, 1 < 2);
}


inline void TestCase25(unit::tag_t tag)
{
    unit::assert(tag, 1 <= 2);
}


inline void TestCase26(unit::tag_t tag)
{
    unit::assert(tag, 1 <= 1);
}


inline void TestCase27(unit::tag_t tag)
{
    unit::assert(tag, 1 == (-2 + 3));
}


inline void TestCase28(unit::tag_t tag)
{
    unit::assert(tag, -5 == (-2 + -3));
}


inline void TestCase29(unit::tag_t tag)
{
    unit::assert(tag, 8 == (2 << 2));
}


inline void TestCase30(unit::tag_t tag)
{
    unit::assert(tag, 2 == (8 >> 2));
}


inline void TestCase31(unit::tag_t tag)
{
    unit::assert(tag, -2 == (-4 >> 1));
}


inline void TestCase32(unit::tag_t tag)
{
    unit::assert(tag, -8 == (-4 << 1));
}


inline void TestCase33(unit::tag_t tag)
{
    uint16 x;
    unit::assert(tag, 2 == bytesizeof(x));
}


inline void TestCase34(unit::tag_t tag)
{
    uint16 x;
    unit::assert(tag, 16 == bitsizeof(x));
}


inline void TestCase35(unit::tag_t tag)
{
    uint2 x = 3;
    int2 y = cast<int2>(x);

    unit::assert(tag, -1 == y);
}


struct Foo36
{
    uint2 x;
    uint3 y;
}

inline void TestCase36(unit::tag_t tag)
{
    Foo36 foo;

    foo.x = 3;
    foo.y = 1;

    unit::assert(tag, foo.x == 3);
    unit::assert(tag, foo.y == 1);
}


inline void TestCase38(unit::tag_t tag)
{
    uint2 x = 3;
    uint4 y = x;
    unit::assert(tag, y == 3);
}



inline void TestCase39(unit::tag_t tag)
{
    uint3 x = 6;
    uint2 y = static_cast(x);
    unit::assert(tag, y == 2);
}


inline void TestCase40(unit::tag_t tag)
{
    int3 x = static_cast(6);
    uint3 y = static_cast(x);
    unit::assert(tag, y == 6);
    unit::assert(tag, x == -2);
}


inline void TestCase41(unit::tag_t tag)
{
    int3 x = -3;
    uint3 y = static_cast(x);
    unit::assert(tag, y == 5);
}


inline void TestCase42(unit::tag_t tag)
{
    int10 x = -3;
    unit::assert(tag, x == -3);
}


inline void TestCase43(unit::tag_t tag)
{
    int10 x = -3;
    int20 y = x;
    unit::assert(tag, y == -3);
}


struct Foo44
{
    int5 x;
}

inline void TestCase44(unit::tag_t tag)
{
    Foo44 foo;
    foo.x = -3;

    int20 y = foo.x;
    unit::assert(tag, y == -3);
}


struct Foo45
{
    int20 x;
}

inline void TestCase45(unit::tag_t tag)
{
    int5 x = -3;

    Foo45 foo;
    foo.x = x;

    unit::assert(tag, foo.x == -3);
}


inline void TestCase46(unit::tag_t tag)
{
    int10[2] x;
    x[1] = -3;

    int20 y = x[1];
    unit::assert(tag, y == -3);
}


inline void TestCase47(unit::tag_t tag)
{
    uint2 x = 3;
    uint4 y = cast<uint4>(x);
    unit::assert(tag, y == 3);
}


inline void TestCase48(unit::tag_t tag)
{
    int10 x = -3;
    int20 y = cast<int20>(x);
    unit::assert(tag, y == -3);
}


inline void TestCase49(unit::tag_t tag)
{
    uint3 x = 4;
    int3 y = static_cast(-x);
    unit::assert(tag, y == -4);
}

inline void test_main()
{
    unit::test<1>(TestCase1);
    unit::test<2>(TestCase2);
    unit::test<3>(TestCase3);
    unit::test<5>(TestCase5);
    unit::test<6>(TestCase6);
    unit::test<7>(TestCase7);
    unit::test<8>(TestCase8);
    unit::test<9>(TestCase9);
    unit::test<13>(TestCase13);
    unit::test<14>(TestCase14);
    unit::test<15>(TestCase15);
    unit::test<16>(TestCase16);
    unit::test<17>(TestCase17);
    unit::test<18>(TestCase18);
    unit::test<19>(TestCase19);
    unit::test<20>(TestCase20);
    unit::test<21>(TestCase21);
    unit::test<22>(TestCase22);
    unit::test<23>(TestCase23);
    unit::test<24>(TestCase24);
    unit::test<25>(TestCase25);
    unit::test<26>(TestCase26);
    unit::test<27>(TestCase27);
    unit::test<28>(TestCase28);
    unit::test<29>(TestCase29);
    unit::test<30>(TestCase30);
    unit::test<31>(TestCase31);
    unit::test<32>(TestCase32);
    unit::test<33>(TestCase33);
    unit::test<34>(TestCase34);
    unit::test<35>(TestCase35);
    unit::test<36>(TestCase36);
    unit::test<38>(TestCase38);
    unit::test<39>(TestCase39);
    unit::test<40>(TestCase40);
    unit::test<41>(TestCase41);
    unit::test<42>(TestCase42);
    unit::test<43>(TestCase43);
    unit::test<44>(TestCase44);
    unit::test<45>(TestCase45);
    unit::test<46>(TestCase46);
    unit::test<47>(TestCase47);
    unit::test<48>(TestCase48);
    unit::test<49>(TestCase49);
 }
