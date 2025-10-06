// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// function that returns an array with non literal size
class TestCase277
{
private:
    const uint32 g_size277 = 4;

    uint8[g_size277] Foo277()
    {
        uint8[g_size277] result;

        static for (const auto i : g_size277)
        {
            result[i] = i + 1;
        }

        return result;
    }

public:
    void run(unit::tag_t tag)
    {
        uint8[g_size277] a = Foo277();

        unit::assert(tag, a[0] == 1);
        unit::assert(tag, a[1] == 2);
        unit::assert(tag, a[2] == 3);
        unit::assert(tag, a[3] == 4);
    }
}

// function parameter that is an array with non literal size
class TestCase278
{
private:
    const uint32 g_size278 = 4;

    uint32 Foo278(uint8[g_size278] a)
    {
        uint32 result = 0;

        static for (const auto i : g_size278)
        {
            result = result + a[i];
        }

        return result;
    }

public:
    void run(unit::tag_t tag)
    {
        uint8[g_size278] a;

        a[0] = 1;
        a[1] = 3;
        a[2] = 8;
        a[3] = 2;

        uint32 result = Foo278(a);

        unit::assert(tag, result == 14);
    }
}

// casing to array with non literal size
inline void TestCase279(unit::tag_t tag)
{
    const uint32 size = 4;

    uint32 source = 0xdeadbeef;

    uint8[size] dest = cast<uint8[size]>(source);

    unit::assert(tag, dest[0] == 0xef);
    unit::assert(tag, dest[1] == 0xbe);
    unit::assert(tag, dest[2] == 0xad);
    unit::assert(tag, dest[3] == 0xde);
}

// struct with an array
struct Foo280
{
    uint8[4] data;
};

inline void TestCase280(unit::tag_t tag)
{
    Foo280 f;

    f.data[1] = 3;

    unit::assert(tag, f.data[1] == 3);
}

// precedence of == and ?
struct FP16_281
{
    uint10 _mantissa;
    uint5 _exponent;
    bool _sign;
}

inline int5 DecodeExp281(FP16_281 a)
{
    return (a._exponent == 0 ? -14 : (a._exponent - 15));
}

inline void TestCase281(unit::tag_t tag)
{
   FP16_281 f;

   f._exponent = 0;

   int5 result = DecodeExp281(f);

   unit::assert(tag, result == -14);
}

// Repro of a CopyToFIFO bug
struct FP16_282
{
    uint10 _mantissa;
    uint5 _exponent;
    bool _sign;
}

inline uint24 ShiftRightRoundVariable24_282(uint24 input, uint6 shiftAmount, uint64 inputRandomValue)
{
    uint24 resultX = input;

    return resultX;
}

class TestCase282
{
private:
    bool b282 = true;

    FP16_282 FixedToFloat16_282(int64 fixed, uint64 randomValue)
    {
        FP16_282 result;

        if (b282)
        {
            // Increase the exponent, and shift right to make the highest bit set == 10, then remove that bit (implied 1)
            result._mantissa = ShiftRightRoundVariable24_282(10, 4, randomValue);
        }

        return result;
    }

public:
    void run(unit::tag_t tag)
    {
        FP16_282 a = FixedToFloat16_282(1234, 3);

        unit::assert(tag, a._exponent == 0);
    }
}

// Comparing a negative number with 0
inline void TestCase284(unit::tag_t tag)
{
    int32 i = -1234;

    bool b = i < 0;

    unit::assert(tag, b == true);
}

// Verifies that memory reads/writes do not get combined into the same cycle
class TestCase288
{
private:
    [[memory]] uint32[4] g_mem288;

    [[unordered]] void Write288()
    {
        [[unordered]] for (const uint32 i : 4)
        {
            g_mem288[i] = i + 2;
        }
    }

    [[pipelined]] void Foo288(uint32 input, unit::tag_t tag)
    {
        uint32 i = static_cast(input + 1);

        uint32 x = static_cast(g_mem288[i] + 1);

        unit::assert(tag, x == (i + 2 + 1));
    }

public:
    [[unordered]] void run(unit::tag_t tag)
    {
        Write288();

        Foo288(3, tag);
    }
}

// Array of objects
class Foo290
{
private:
    uint32 _x;

public:
    void Set(uint32 x)
    {
        _x = x;
    }

    uint32 Get()
    {
        return _x;
    }
}

class TestCase290
{
private:
    Foo290[4] g_foo290;

public:
    void run(unit::tag_t tag)
    {
        static for(const auto x : 4)
        {
            g_foo290[x].Set(x + 1);
        }

        static for(const auto x : 4)
        {
            unit::assert(tag, (x + 1) == g_foo290[x].Get());
        }
    }
}

// Difference of 2 unsigned integers gives a signed integer (handled with care in cpp backend)
inline void TestCase292(unit::tag_t tag)
{
    uint32 x = 4;
    uint32 y = 7;

    uint32 result = static_cast(x - y);

    unit::assert(tag, result == 0xfffffffd);
}

// zero-initializing a local struct
struct Foo293
{
    uint8 a;
    uint8 b;
}

inline void TestCase293(unit::tag_t tag)
{
    Foo293 f = {};

    unit::assert(tag, f.a == 0);
    unit::assert(tag, f.b == 0);
}

// zero-initializing a const local struct
struct Foo294
{
    uint8 a;
    uint8 b;
}

inline void TestCase294(unit::tag_t tag)
{
    const Foo294 f = {};

    unit::assert(tag, f.a == 0);
    unit::assert(tag, f.b == 0);
}

// Partial updates to array
struct Command295
{
    uint32 _data;
}

inline void TestCase295(unit::tag_t tag)
{
    Command295[2] commands;

    commands[0]._data = 1;
    commands[1]._data = 2;

    unit::assert(tag, commands[0]._data == 1);
    unit::assert(tag, commands[1]._data == 2);
}

// Partial updates to array (dynamic)
struct Command296
{
    uint32 _data;
}

class TestCase296
{
private:
    uint32 g_i296 = 1;

public:
    void run(unit::tag_t tag)
    {
        Command296[2] commands;

        commands[g_i296]._data = 7;

        unit::assert(tag, commands[1]._data == 7);
    }
}

// switch statement with local variables
inline void TestCase297(unit::tag_t tag)
{
    uint32 x = 3;

    switch (x)
    {
    case 3:
        uint32 y = 8;
        unit::assert(tag, y == 8);
        break;

    default:
        break;
    }

    unit::assert(tag, x == 3);
}

inline void test_main()
{
    unit::test<277>(unit::fixture<TestCase277>());
    unit::test<278>(unit::fixture<TestCase278>());
    unit::test<279>(TestCase279);
    unit::test<280>(TestCase280);
    unit::test<281>(TestCase281);
    unit::test<282>(unit::fixture<TestCase282>());
    unit::test<284>(TestCase284);
    unit::test<288>(unit::fixture<TestCase288>());
    unit::test<290>(unit::fixture<TestCase290>());
    unit::test<292>(TestCase292);
    unit::test<293>(TestCase293);
    unit::test<294>(TestCase294);
    unit::test<295>(TestCase295);
    unit::test<296>(unit::fixture<TestCase296>());
    unit::test<297>(TestCase297);
}
