// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner
import type.cstdint

// dynamic write to non-pow2 array
class Test400
{
private:
    uint32[7] g_ary400;

public:
    [[pipelined]] void Foo400(uint32 idx)
    {
        g_ary400[idx] = idx + 1;
    }

    void run(unit::tag_t tag)
    {
        Foo400(7);

        unit::assert(tag, g_ary400[0] == 1);
        unit::assert(tag, g_ary400[1] == 2);
        unit::assert(tag, g_ary400[2] == 3);
        unit::assert(tag, g_ary400[3] == 4);
        unit::assert(tag, g_ary400[4] == 5);
        unit::assert(tag, g_ary400[5] == 6);
        unit::assert(tag, g_ary400[6] == 7);
    }
}

// dynamic read from non-pow2 array
class Test401
{
private:
    uint32[7] g_ary401;

public:
    [[pipelined]] void Foo401(uint32 idx, unit::tag_t tag)
    {
        unit::assert(tag, (idx + 1) == g_ary401[idx]);
    }

    void run(unit::tag_t tag)
    {
        g_ary401[0] = 1;
        g_ary401[1] = 2;
        g_ary401[2] = 3;
        g_ary401[3] = 4;
        g_ary401[4] = 5;
        g_ary401[5] = 6;
        g_ary401[6] = 7;

        Foo401(7, tag);
    }
}

// initializing global float
class Test402
{
private:
    float32 g_f402 = 2.3;

public:
    void run(unit::tag_t tag)
    {
        float32 x = 2.3;

        unit::assert(tag, cast<uint32>(x) == cast<uint32>(g_f402));
    }
}

// initializing member float
class C403
{
private:
    float32 _f = 3.4;

public:
    void Compare(float32 in, unit::tag_t tag)
    {
        unit::assert(tag, cast<uint32>(_f) == cast<uint32>(in));
    }
}

class Test403
{
private:
    C403 g_c403;

public:
    void run(unit::tag_t tag)
    {
        g_c403.Compare(3.4, tag);
    }
}

class TestCase404
{
private:
    // pipelined function returning an array
    [[pipelined]] uint32 Foo404(uint32 idx)
    {
        return idx + 1;
    }

public:
    void run(unit::tag_t tag)
    {
        uint32[4] result = Foo404(4);

        unit::assert(tag, result[0] == 1);
        unit::assert(tag, result[1] == 2);
        unit::assert(tag, result[2] == 3);
        unit::assert(tag, result[3] == 4);
    }
}

class TestCase405
{
private:
    // pipelined function returning composite type
    [[pipelined]] uint32[4] Foo405(uint32 idx)
    {
        uint32[4] result;

        static for (const auto i : 4)
        {
            result[i] = idx + i;
        }

        return result;
    }

public:
    void run(unit::tag_t tag)
    {
        uint32[2][4] result = Foo405(2);

        unit::assert(tag, result[0][0] == 0);
        unit::assert(tag, result[0][1] == 1);
        unit::assert(tag, result[0][2] == 2);
        unit::assert(tag, result[0][3] == 3);

        unit::assert(tag, result[1][0] == 1);
        unit::assert(tag, result[1][1] == 2);
        unit::assert(tag, result[1][2] == 3);
        unit::assert(tag, result[1][3] == 4);
    }
}

// offsetof
struct Foo406
{
    uint40 _x;
    float32 _y;
}

inline void TestCase406(unit::tag_t tag)
{
    unit::assert(tag, 0 == bitoffsetof(Foo406, _x));
    unit::assert(tag, 40 == bitoffsetof(Foo406, _y));

    unit::assert(tag, 0 == byteoffsetof(Foo406, _x));
    unit::assert(tag, 5 == byteoffsetof(Foo406, _y));
}

// inline that consumes a struct
struct Foo407
{
    uint40 _x;
    uint16 _y;
}

inline uint32 Func407(Foo407 input)
{
    return input._x + input._y;
}

inline void TestCase407(unit::tag_t tag)
{
    Foo407 f;

    f._x = 2;
    f._y = 4;

    unit::assert(tag, 6 == Func407(f));
}

// float32 `*` with denorm output
inline void TestCase408(unit::tag_t tag)
{
    // The smallest normalized number
    float32 smallestNorm = cast<float32>(cast<uint32>(0x00800000));

    float32 f = smallestNorm * 0.75;

    unit::assert(tag, cast<uint32>(f) == 0);
}

// built-in integer typedefs
inline void TestCase409(unit::tag_t tag)
{
    uint32_t u;
    unit::assert(tag, bitsizeof(u) == 32);

    size_t s;
    unit::assert(tag, bitsizeof(s) == 64);

    int8_t i8;
    unit::assert(tag, bitsizeof(i8) == 8);

    int16_t i16;
    unit::assert(tag, bitsizeof(i16) == 16);

    int32_t i32;
    unit::assert(tag, bitsizeof(i32) == 32);

    int64_t i64;
    unit::assert(tag, bitsizeof(i64) == 64);

    uint8_t u8;
    unit::assert(tag, bitsizeof(u8) == 8);

    uint16_t u16;
    unit::assert(tag, bitsizeof(u16) == 16);

    uint32_t u32;
    unit::assert(tag, bitsizeof(u32) == 32);

    uint64_t u64;
    unit::assert(tag, bitsizeof(u64) == 64);
}

// if alias
inline void TestCase410(unit::tag_t tag)
{
    uint32 x = 0;
    uint32 y = 1;

    if (x < y)
    {
        unit::assert(tag, true);
    }
    else
    {
        unit::assert(tag, false);
    }
}

// bytesizeof(float32)
inline void TestCase411(unit::tag_t tag)
{
    unit::assert(tag, 4 == bytesizeof(float32));
}

// predicated write to memory
class Test412
{
private:
    [[memory]] uint32[8] g_mem412;

public:
    void Foo412(uint32 idx, uint32 val, bool b)
    {
        if (b)
        {
            g_mem412[idx] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        Foo412(3, 5, true);
        Foo412(3, 6, false);

        unit::assert(tag, g_mem412[3] == 5);
    }
}

class Test413
{
private:
    bool g_b413 = false;

    // Loop count assertions while predicated
    [[pipelined]] void Foo413(uint32 idx)
    {
    }

public:
    void run(unit::tag_t tag)
    {
        if (g_b413)
        {
            Foo413(0);
        }
    }
}

// Memory with depth 1
class Test414
{
private:
    [[memory]] uint32[1] g_mem;

public:
    void run(unit::tag_t tag)
    {
        g_mem[0] = 1;

        unit::assert(tag, g_mem[0] == 1);
    }
}

// inline that consumes 2 structs
struct Foo415_1
{
    uint40 _x;
    uint16 _y;
}

struct Foo415_2
{
    uint8  _z;
    uint12 _w;
}

inline uint32 Func415(Foo415_1 input1, Foo415_2 input2)
{
    return input1._x + input1._y + input2._z + input2._w;
}

inline void TestCase415(unit::tag_t tag)
{
    Foo415_1 f;

    f._x = 2;
    f._y = 4;

    Foo415_2 g;

    g._z = 6;
    g._w = 7;

    unit::assert(tag, 19 == Func415(f, g));
}

// float32 `*` operator
inline void TestCase416(unit::tag_t tag)
{
    float32 f = -7.125;

    float32 g = 2.0;

    float32 product = f * g;

    float32 expected = -14.25;

    unit::assert(tag, cast<uint32>(product) == cast<uint32>(expected));
}

inline void test_main()
{
    unit::test<400>(unit::fixture<Test400>());
    unit::test<401>(unit::fixture<Test401>());
    unit::test<402>(unit::fixture<Test402>());
    unit::test<403>(unit::fixture<Test403>());
    unit::test<404>(unit::fixture<TestCase404>());
    unit::test<405>(unit::fixture<TestCase405>());
    unit::test<406>(TestCase406);
    unit::test<407>(TestCase407);
    unit::test<408>(TestCase408);
    unit::test<409>(TestCase409);
    unit::test<410>(TestCase410);
    unit::test<411>(TestCase411);
    unit::test<412>(unit::fixture<Test412>());
    unit::test<413>(unit::fixture<Test413>());
    unit::test<414>(unit::fixture<Test414>());
    unit::test<415>(TestCase415);
    unit::test<416>(TestCase416);
}
