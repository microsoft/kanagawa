// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// loop inside of flat if/else
class Test100
{
private:
    bool b100 = false;

public:
    [[unordered]] void run(unit::tag_t tag)
    {
        if (b100)
        {
            [[unordered]] do; while (true);
        }

        unit::assert(tag, true);
    }
}

// function call inside of if
class Test101
{
private:
    bool b101 = false;
    uint32 x101 = 0;

public:
    void Foo101()
    {
        x101 = 1;
    }

    void run(unit::tag_t tag)
    {
        if (b101)
        {
            Foo101();
        }

        unit::assert(tag, x101 == 0);
    }
}


// function call inside of if
class Test102
{
private:
    bool b102 = true;
    uint32 x102 = 0;

public:
    void Foo102()
    {
        x102 = 1;
    }

    void run(unit::tag_t tag)
    {
        if (b102)
        {
            Foo102();
        }

        unit::assert(tag, x102 == 1);
    }
}


// async function call inside of if
class Test103
{
private:
    bool b103 = true;
    uint32 x103 = 0;

public:
    [[async]] void Foo103()
    {
        x103 = 1;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        if (b103)
        {
            Foo103();
        }

        [[unordered]] do; while (x103 != 1);

        unit::assert(tag, x103 == 1);
    }
}


// async function call inside of if
class Test104
{
private:
    bool b104 = false;
    uint32 x104 = 0;

public:
    [[async]] void Foo104()
    {
        x104 = 1;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        if (b104)
        {
            Foo104();
        }

        // Wait long enough for the async function to execute if it is going to
        [[unordered]] for (const uint32 i : 100)
        {
        }

        unit::assert(tag, x104 == 0);
    }
}


// pipelined async function call inside of if
class Test105
{
private:
    bool b105 = true;
    uint32 x105 = 0;

public:
    [[pipelined, async]] void Foo105(uint32 iter)
    {
        x105 = iter;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        if (b105)
        {
            Foo105(3);
        }

        [[unordered]] do; while (x105 != 2);

        unit::assert(tag, x105 == 2);
    }
}


// pipelined async function call inside of if
class Test106
{
private:
    bool b106 = false;
    uint32 x106 = 0;

public:
    [[pipelined, async]] void Foo106(uint32 iter)
    {
        x106 = iter;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        if (b106)
        {
            Foo106(3);
        }

        // Wait long enough for the function to execute if it is going to
        [[unordered]] for (const uint32 i : 100)
        {
        }

        unit::assert(tag, x106 == 0);
    }
}


// nested if
class Test107
{
private:
    bool b1107 = false;
    bool b2107 = false;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 0;

        if (b1107)
        {
            if (b2107)
            {
                x = 1;
            }
            else
            {
                x = 2;
            }
        }
        else
        {
            if (b2107)
            {
                x = 3;
            }
            else
            {
                x = 4;
            }
        }

        unit::assert(tag, x == 4);
    }
}


// nested if
class Test108
{
private:
    bool b1108 = true;
    bool b2108 = false;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 0;

        if (b1108)
        {
            if (b2108)
            {
                x = 1;
            }
            else
            {
                x = 2;
            }
        }
        else
        {
            if (b2108)
            {
                x = 3;
            }
            else
            {
                x = 4;
            }
        }

        unit::assert(tag, x == 2);
    }
}


// nested if
class Test109
{
private:
    bool b1109 = false;
    bool b2109 = true;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 0;

        if (b1109)
        {
            if (b2109)
            {
                x = 1;
            }
            else
            {
                x = 2;
            }
        }
        else
        {
            if (b2109)
            {
                x = 3;
            }
            else
            {
                x = 4;
            }
        }

        unit::assert(tag, x == 3);
    }
}


// nested if
class Test110
{
private:
    bool b1110 = true;
    bool b2110 = true;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 0;

        if (b1110)
        {
            if (b2110)
            {
                x = 1;
            }
            else
            {
                x = 2;
            }
        }
        else
        {
            if (b2110)
            {
                x = 3;
            }
            else
            {
                x = 4;
            }
        }

        unit::assert(tag, x == 1);
    }
}


// nesting flat and regular if
class Test111
{
private:
    bool b1111 = true;
    bool b2111 = true;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 0;

        if (b1111)
        {
            if (b2111)
            {
                x = 1;
            }
            else
            {
                x = 2;
            }
        }
        else
        {
            if (b2111)
            {
                x = 3;
            }
            else
            {
                x = 4;
            }
        }

        unit::assert(tag, x == 1);
    }
}


// nesting flat and regular if
class Test112
{
private:
    bool b1112 = true;
    bool b2112 = false;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 0;

        if (b1112)
        {
            if (b2112)
            {
                x = 1;
            }
            else
            {
                x = 2;
            }
        }
        else
        {
            if (b2112)
            {
                x = 3;
            }
            else
            {
                x = 4;
            }
        }

        unit::assert(tag, x == 2);
    }
}


// nesting flat and regular if
class Test113
{
private:
    bool b1113 = false;
    bool b2113 = true;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 0;

        if (b1113)
        {
            if (b2113)
            {
                x = 1;
            }
            else
            {
                x = 2;
            }
        }
        else
        {
            if (b2113)
            {
                x = 3;
            }
            else
            {
                x = 4;
            }
        }

        unit::assert(tag, x == 3);
    }
}


// nesting flat and regular if
class Test114
{
private:
    bool b1114 = false;
    bool b2114 = false;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 0;

        if (b1114)
        {
            if (b2114)
            {
                x = 1;
            }
            else
            {
                x = 2;
            }
        }
        else
        {
            if (b2114)
            {
                x = 3;
            }
            else
            {
                x = 4;
            }
        }

        unit::assert(tag, x == 4);
    }
}


// Inline function call
inline uint32 Foo115(uint32 in)
{
    return in + 3;
}

inline void TestCase115(unit::tag_t tag)
{
    uint32 result = Foo115(4);

    unit::assert(tag, result == 7);
}


// Inline function with parameter name matching local variable in caller
inline uint32 Foo116(uint32 myvar)
{
    return myvar + 3;
}

inline void TestCase116(unit::tag_t tag)
{
    uint32 myvar = 10;

    uint32 result = Foo116(myvar);

    unit::assert(tag, result == 13);
}


// basic unrolled loop
inline void TestCase117(unit::tag_t tag)
{
    uint32 x = 0;

    static for(const auto i : 5)
    {
        x = x + i;
    }

    unit::assert(tag, x == 10);
}


// FIFO merger/splitter tree (2)
inline uint32 Foo118(uint32 x)
{
    return x + 1;
}

inline void TestCase118(unit::tag_t tag)
{
    uint32 y = static_cast(Foo118(1) + Foo118(2));

    unit::assert(tag, y == 5);
}


// FIFO merger/splitter tree (3)
inline uint32 Foo119(uint32 x)
{
    return x + 1;
}

inline void TestCase119(unit::tag_t tag)
{
    uint32 y = static_cast(Foo119(1) + Foo119(2) + Foo119(3));

    unit::assert(tag, y == 9);
}


// FIFO merger/splitter tree (4)
inline uint32 Foo120(uint32 x)
{
    return x + 1;
}

inline void TestCase120(unit::tag_t tag)
{
    uint32 y = static_cast(Foo120(1) + Foo120(2) + Foo120(3) + Foo120(4));

    unit::assert(tag, y == 14);
}


// FIFO merger/splitter tree (5)
inline uint32 Foo121(uint32 x)
{
    return x + 1;
}

inline void TestCase121(unit::tag_t tag)
{
    uint32 y = static_cast(Foo121(1) + Foo121(2) + Foo121(3) + Foo121(4) + Foo121(5));

    unit::assert(tag, y == 20);
}


// FIFO merger/splitter tree (6)
inline uint32 Foo122(uint32 x)
{
    return x + 1;
}

inline void TestCase122(unit::tag_t tag)
{
    uint32 y = static_cast(Foo122(1) + Foo122(2) + Foo122(3) + Foo122(4) + Foo122(5) + Foo122(6));

    unit::assert(tag, y == 27);
}


// global initialized with expression
const uint32 g123 = 3 + 4;

inline void TestCase123(unit::tag_t tag)
{
    unit::assert(tag, g123 == 7);
}


// ++ and --
inline void TestCase124(unit::tag_t tag)
{
    uint32 x = 2;

    x++;

    unit::assert(tag, x == 3);

    x--;

    unit::assert(tag, x == 2);
}

inline void test_main()
{
    unit::test<100>(unit::fixture<Test100>());
    unit::test<101>(unit::fixture<Test101>());
    unit::test<102>(unit::fixture<Test102>());
    unit::test<103>(unit::fixture<Test103>());
    unit::test<104>(unit::fixture<Test104>());
    unit::test<105>(unit::fixture<Test105>());
    unit::test<106>(unit::fixture<Test106>());
    unit::test<107>(unit::fixture<Test107>());
    unit::test<108>(unit::fixture<Test108>());
    unit::test<109>(unit::fixture<Test109>());
    unit::test<110>(unit::fixture<Test110>());
    unit::test<111>(unit::fixture<Test111>());
    unit::test<112>(unit::fixture<Test112>());
    unit::test<113>(unit::fixture<Test113>());
    unit::test<114>(unit::fixture<Test114>());
    unit::test<115>(TestCase115);
    unit::test<116>(TestCase116);
    unit::test<117>(TestCase117);
    unit::test<118>(TestCase118);
    unit::test<119>(TestCase119);
    unit::test<120>(TestCase120);
    unit::test<121>(TestCase121);
    unit::test<122>(TestCase122);
    unit::test<123>(TestCase123);
    unit::test<124>(TestCase124);
}
