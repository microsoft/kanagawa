// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// 2 variables with the same name in different scopes
inline void TestCase127(unit::tag_t tag)
{
    uint32 x = 5;

    {
        uint32 x;

        x = 7;

        unit::assert(tag, x == 7);
    }

    unit::assert(tag, x == 5);
}


// XOR/OR precedence
inline void TestCase128(unit::tag_t tag)
{
    unit::assert(tag, (3 | 7 ^ 3) == (3 | (7 ^ 3)));
}


// XOR/AND precedence
inline void TestCase129(unit::tag_t tag)
{
    unit::assert(tag, (3 & 7 ^ 3) == (3 & (7 ^ 3)));
}


// Equal/Left shift precedence
inline void TestCase130(unit::tag_t tag)
{
    unit::assert(tag, 1 << 2 == 4);
}


// Equal/Right shift precedence
inline void TestCase131(unit::tag_t tag)
{
    unit::assert(tag, 11 >> 1 == 5);
}


// NotEqual/Left shift precedence
inline void TestCase132(unit::tag_t tag)
{
    unit::assert(tag, 1 << 2 != 3);
}


// NotEqual/Right shift precedence
inline void TestCase133(unit::tag_t tag)
{
    unit::assert(tag, 11 << 1 != 5);
}


// EQ/NE left-to-right associativity
inline void TestCase134(unit::tag_t tag)
{
    unit::assert(tag, (true == true) != false);
}


// EQ/NE left-to-right associativity
inline void TestCase135(unit::tag_t tag)
{
    unit::assert(tag, (true != true) == false);
}


// EQ/EQ left-to-right associativity
inline void TestCase136(unit::tag_t tag)
{
    unit::assert(tag, (true == true) == true);
}


// Addition/Right Shift precedence
inline void TestCase137(unit::tag_t tag)
{
    unit::assert(tag, (1 + 4 >> 3) == ((1 + 4) >> 3));
}


// Subtraction/Left Shift precedence
inline void TestCase138(unit::tag_t tag)
{
    unit::assert(tag, (10 - 4 << 3) == ((10 - 4) << 3));
}


// Shift left-to-right associativity
inline void TestCase139(unit::tag_t tag)
{
    unit::assert(tag, (1 << 2 << 3) == ((1 << 2) << 3));
}


// Shift left-to-right associativity
inline void TestCase140(unit::tag_t tag)
{
    unit::assert(tag, (1 << 4 >> 1) == ((1 << 4) >> 1));
}


// Addition/Invert precedence
inline void TestCase141(unit::tag_t tag)
{
    unit::assert(tag, (~1 + 2) == ((~1) + 2));
}


// Subtraction/Invert precedence
inline void TestCase142(unit::tag_t tag)
{
    unit::assert(tag, (~1 - 2) == ((~1) - 2));
}


// Invert/Negate associativity
inline void TestCase143(unit::tag_t tag)
{
    unit::assert(tag, (~-3) == (~(-3)));
}


// Hex literal
inline void TestCase144(unit::tag_t tag)
{
    uint32 x = 15;
    uint32 y = 0xf;

    unit::assert(tag, x == y);
}


// Predicated write to a global
class TestCase145
{
private:
    uint32 g145 = 3;
    bool b145 = true;

public:
    void run(unit::tag_t tag)
    {
        if (b145)
        {
            g145 = 4;
        }
    
        unit::assert(tag, g145 == 4);
    }
}


// Predicated write to a global
class TestCase146
{
private:
    uint32 g146 = 3;
    bool b146 = false;

public:
    void run(unit::tag_t tag)
    {
        if (b146)
        {
            g146 = 4;
        }
    
        unit::assert(tag, g146 == 3);
    }
}


// sign-extension when writing to a global
class TestCase147
{
private:
    uint32 g147 = 3;

public:
    void run(unit::tag_t tag)
    {
        int4 i = -1;
    
        g147 = i;
    
        unit::assert(tag, g147 == 0xffffffff);
    }
}

// Synchronous, pipelined call
class TestCase149
{
private:
    uint32 g149 = 0;

public:
    [[pipelined]] void Foo149(uint32 index)
    {
        atomic
        {
            g149 = g149 + 1;
        }
    }
    
    void run(unit::tag_t tag)
    {
        Foo149(6);
    
        unit::assert(tag, g149 == 6);
    }
}

inline void test_main()
{
    unit::test<127>(TestCase127);
    unit::test<128>(TestCase128);
    unit::test<129>(TestCase129);
    unit::test<130>(TestCase130);
    unit::test<131>(TestCase131);
    unit::test<132>(TestCase132);
    unit::test<133>(TestCase133);
    unit::test<134>(TestCase134);
    unit::test<135>(TestCase135);
    unit::test<136>(TestCase136);
    unit::test<137>(TestCase137);
    unit::test<138>(TestCase138);
    unit::test<139>(TestCase139);
    unit::test<140>(TestCase140);
    unit::test<141>(TestCase141);
    unit::test<142>(TestCase142);
    unit::test<143>(TestCase143);
    unit::test<144>(TestCase144);
    unit::test<145>(unit::fixture<TestCase145>());
    unit::test<146>(unit::fixture<TestCase146>());
    unit::test<147>(unit::fixture<TestCase147>());
    unit::test<149>(unit::fixture<TestCase149>());
}
