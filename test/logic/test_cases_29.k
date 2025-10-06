// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// XOR converted to a lut
inline uint8 Foo457(uint8 x, uint8 y)
{
    return x ^ y;
}

inline void TestCase457(unit::tag_t tag)
{
    unit::assert(tag, 0xEA == Foo457(0xFF, 0x15));
}

// x << const converted to a lut
inline uint8 Foo458(uint8 x)
{
    return x << 4;
}

inline void TestCase458(unit::tag_t tag)
{
    unit::assert(tag, 240 == Foo458(15));
}

// x >> const (logical) converted to a lut
inline uint8 Foo459(uint8 x)
{
    return x >> 3;
}

inline void TestCase459(unit::tag_t tag)
{
    unit::assert(tag, 30 == Foo459(240));
}

// x >> const (arithmetic) converted to a lut
inline uint8 Foo460(int8 x)
{
    return x >> 3;
}

inline void TestCase460(unit::tag_t tag)
{
    unit::assert(tag, 0xF5 == Foo460(static_cast(0xAD)));

    unit::assert(tag, 9 == Foo460(0x48));
}

// a == b (small) converted into a lut
inline bool Foo461(uint2 a, uint4 b)
{
    return a == b;
}

inline void TestCase461(unit::tag_t tag)
{
    unit::assert(tag, true == Foo461(3, 3));
}

// a + b (small) converted into a lut - with sign extension
inline int32 Foo462(int2 a, uint4 b)
{
    return a + b;
}

inline void TestCase462(unit::tag_t tag)
{
    unit::assert(tag, 1 == Foo462(-2, 3));

    unit::assert(tag, -1 == Foo462(-2, 1));
}

// a (large literal) + b (small) converted into a lut
inline int32 Foo463(uint6 b)
{
    return 12345 + b;
}

inline void TestCase463(unit::tag_t tag)
{
    unit::assert(tag, 12348 == Foo463(3));
}

// 4:1 mux converted to luts (non-literal operands)
inline uint8 Foo464(uint2 i, uint8 a, uint8 b, uint8 c, uint8 d)
{
    return mux(i, a, b, c, d);
}

inline void TestCase464(unit::tag_t tag)
{
    unit::assert(tag, 11 == Foo464(0, 11, 12, 13, 14));
    unit::assert(tag, 12 == Foo464(1, 11, 12, 13, 14));
    unit::assert(tag, 13 == Foo464(2, 11, 12, 13, 14));
    unit::assert(tag, 14 == Foo464(3, 11, 12, 13, 14));
}

// 4:1 mux converted to luts (non-literal operands) - sign and zero extension
inline int8 Foo465(uint2 i, uint3 a, int5 b, uint2 c, int4 d)
{
    return mux(i, a, b, c, d);
}

inline void TestCase465(unit::tag_t tag)
{
    unit::assert(tag, 7  == Foo465(0, 7, -2, 1, -6));
    unit::assert(tag, -2 == Foo465(1, 7, -2, 1, -6));
    unit::assert(tag, 1  == Foo465(2, 7, -2, 1, -6));
    unit::assert(tag, -6 == Foo465(3, 7, -2, 1, -6));
}

// 4:1 mux converted to luts (some literal operands)
inline uint8 Foo466(uint2 i, uint8 a,  uint8 d)
{
    return mux(i, a, 124, 57, d);
}

inline void TestCase466(unit::tag_t tag)
{
    unit::assert(tag, 7 == Foo466(0, 7, 45));
    unit::assert(tag, 124 == Foo466(1, 7, 45));
    unit::assert(tag, 57 == Foo466(2, 7, 45));
    unit::assert(tag, 45 == Foo466(3, 7, 45));
}

// gather converted to LUT
inline uint8[4] Foo467(uint32 x)
{
    return cast<uint8[4]>(x);
}

inline void TestCase467(unit::tag_t tag)
{
    uint8[4] result = Foo467(0xdeadbeef);

    unit::assert(tag, 0xef == result[0]);
    unit::assert(tag, 0xbe == result[1]);
    unit::assert(tag, 0xad == result[2]);
    unit::assert(tag, 0xde == result[3]);
}

// combine luts - combine 2 1-input operations
inline uint1 Foo468(uint1 x, uint1 y, uint1 z)
{
    return (x & y) | z;
}

inline void TestCase468(unit::tag_t tag)
{
    unit::assert(tag, 0 == Foo468(0, 0, 0));
    unit::assert(tag, 1 == Foo468(0, 0, 1));
    unit::assert(tag, 0 == Foo468(0, 1, 0));
    unit::assert(tag, 1 == Foo468(0, 1, 1));
    unit::assert(tag, 0 == Foo468(1, 0, 0));
    unit::assert(tag, 1 == Foo468(1, 0, 1));
    unit::assert(tag, 1 == Foo468(1, 1, 0));
    unit::assert(tag, 1 == Foo468(1, 1, 1));
}

inline void test_main()
{
    unit::test<457>(TestCase457);
    unit::test<458>(TestCase458);
    unit::test<459>(TestCase459);
    unit::test<460>(TestCase460);
    unit::test<461>(TestCase461);
    unit::test<462>(TestCase462);
    unit::test<463>(TestCase463);
    unit::test<464>(TestCase464);
    unit::test<465>(TestCase465);
    unit::test<466>(TestCase466);
    unit::test<467>(TestCase467);
    unit::test<468>(TestCase468);
}
