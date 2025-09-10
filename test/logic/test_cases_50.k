// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// float32 `+` operator
inline void TestCase417(unit::tag_t tag)
{
    float32 f = -7.125;

    float32 g = 2.0;

    float32 product = f + g;

    float32 expected = -5.125;

    unit::assert(tag, cast<uint32>(product) == cast<uint32>(expected));
}

// float32 `*` with denorm input
inline void TestCase420(unit::tag_t tag)
{
    float32 f = cast<float32>(cast<uint32>(1)) * 1.0;

    unit::assert(tag, cast<uint32>(f) == 0);
}

// float32 `-` operator
inline void TestCase421(unit::tag_t tag)
{
    float f = 3.5;

    float g = 1.25;

    float result = f - g;

    unit::assert(tag, cast<uint32>(result) == cast<uint32>(2.25));
}

// writing to array of memories with dynamic index
class Test418
{
private:
    memory<uint32, 512>[4] g_mem418;

public:
    void Foo418(uint32 whichMem, uint32 slot, uint32 val)
    {
        g_mem418[whichMem][slot] = val;
    }

    void run(unit::tag_t tag)
    {
        Foo418(0, 7, 8);
        Foo418(1, 7, 9);
        Foo418(2, 7, 10);
        Foo418(3, 7, 11);

        unit::assert(tag, g_mem418[0][7] == 8);
        unit::assert(tag, g_mem418[1][7] == 9);
        unit::assert(tag, g_mem418[2][7] == 10);
        unit::assert(tag, g_mem418[3][7] == 11);
    }
}

// floating point unary minus
inline void TestCase419(unit::tag_t tag)
{
    float f = 3.4;

    float g = -f;

    unit::assert(tag, cast<uint32>(g) == cast<uint32>(-3.4));
}

// Regular switch statement
inline void TestCase422(unit::tag_t tag)
{
    uint32 x = 2;

    uint32 result;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    case 1:
        result = 8;
        break;

    case 2:
        result = 9;
        break;
    }

    unit::assert(tag, result == 9);
}

// switch statement with the case values being a global constant
const uint32 zero423 = 0;
const uint32 one423 = 1;
const uint32 two423 = 2;

inline void TestCase423(unit::tag_t tag)
{
    uint32 x = 2;

    uint32 result;

    switch (x)
    {
    case zero423:
        result = 7;
        break;

    case one423:
        result = 8;
        break;

    case two423:
        result = 9;
        break;
    }

    unit::assert(tag, result == 9);
}

// switch with default - taken
inline void TestCase424(unit::tag_t tag)
{
    uint32 x = 5;

    uint32 result;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    case 1:
        result = 8;
        break;

    case 2:
        result = 9;
        break;

    default:
        result = 10;
        break;
    }

    unit::assert(tag, result == 10);
}

inline void test_main()
{
    unit::test<417>(TestCase417);
    unit::test<418>(unit::fixture<Test418>());
    unit::test<419>(TestCase419);
    unit::test<420>(TestCase420);
    unit::test<421>(TestCase421);
    unit::test<422>(TestCase422);
    unit::test<423>(TestCase423);
    unit::test<424>(TestCase424);
}
