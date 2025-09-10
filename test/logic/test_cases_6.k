// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import helper.sleep
import test.unit as unit
import test.runner

// Synchronous, pipelined call, loop count > 0, predicated on
class TestCase150
{
private:
    uint32 g150 = 0;
    bool b150 = true;

    [[pipelined]] void Foo150(uint32 index)
    {
        atomic
        {
            g150 = g150 + 1;
        }
    }

public:
    [[unordered]] void run(unit::tag_t tag)
    {
        if (b150)
        {
            Foo150(6);
        }

        unit::assert(tag, g150 == 6);
    }
}

// Synchronous, pipelined call, loop count > 0, predicated off
class TestCase151
{
private:
    uint32 g151 = 0;
    bool b151 = false;

    [[pipelined]] void Foo151(uint32 index)
    {
        atomic
        {
            g151 = g151 + 1;
        }
    }

public:
    [[unordered]] void run(unit::tag_t tag)
    {
        if (b151)
        {
            Foo151(6);
        }

        unit::assert(tag, g151 == 0);
    }
}

// Context saver filling up
class TestCase155
{
    void Bar155(uint32 index)
    {
        // something slow
        sleep<512>();
    }

    [[pipelined]] void Foo155(uint32 index)
    {
        Bar155(index);
    }

public:    
    [[unordered]] void run(unit::tag_t tag)
    {
        // deeper than the FIFO depth
        Foo155(2048);

        // To make the assertion count code happy
        // This test is primarily to look for deadlock
        assert(1 == 1);
    }
}

// Pipelined function called from 2 call sites
class TestCase156
{
private:
    uint32 g156 = 0;

    [[pipelined]] void Foo156(uint32 index)
    {
        atomic
        {
            g156 = g156 + 1;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        Foo156(1024);
        Foo156(1024);

        unit::assert(tag, g156 == 2048);
    }
}

// Global variable that is not initialized
class TestCase157
{
private:
    uint32 g157;

public:
    void run(unit::tag_t tag)
    {
        g157 = 7;

        unit::assert(tag, g157 == 7);
    }
}

// cast
struct Foo158
{
    uint8 a;
    uint8 b;
    uint8 c;
    uint8 d;
}

inline void TestCase158(unit::tag_t tag)
{
    uint32 x = 0xdeadbeef;

    Foo158 foo = cast<Foo158>(x);

    unit::assert(tag, foo.a == 0xef);
    unit::assert(tag, foo.b == 0xbe);
    unit::assert(tag, foo.c == 0xad);
    unit::assert(tag, foo.d == 0xde);

    uint32 y = cast<uint32>(x);

    unit::assert(tag, y == 0xdeadbeef);
}

// cast
inline void TestCase159(unit::tag_t tag)
{
    uint8[4] x;

    x[0] = 1;
    x[1] = 3;
    x[2] = 6;
    x[3] = 9;

    uint32 y = cast<uint32>(x);

    unit::assert(tag, y == 0x09060301);
}

// Class
class Foo160
{
public:
    uint32 Method(uint32 x)
    {
        return x << 1;
    }
}

class TestCase160
{
private:
    Foo160 g_foo160;

public:
    void run(unit::tag_t tag)
    {
        uint32 y = g_foo160.Method(4);

        unit::assert(tag, y == 8);
    }
}

// Class with member variable
class Foo161
{
private:
    uint32 _y = 5;

public:
    uint32 Method(uint32 x)
    {
        return x + _y;
    }
}

class TestCase161
{
private:
    Foo161 g_foo161;

public:
    void run(unit::tag_t tag)
    {
        uint32 y = g_foo161.Method(4);

        unit::assert(tag, y == 9);
    }
}

// Each object of a class gets separate member variables
class Foo162
{
private:
    uint32 _y;

public:
    void Set(uint32 x)
    {
        _y = x;
    }

    uint32 Get()
    {
        return _y;
    }
}

class TestCase162
{
private:
    Foo162 g_foo162_a;
    Foo162 g_foo162_b;

public:
    void run(unit::tag_t tag)
    {
        g_foo162_a.Set(2);
        g_foo162_b.Set(3);

        unit::assert(tag, 2 == g_foo162_a.Get());
        unit::assert(tag, 3 == g_foo162_b.Get());
    }
}

// Public member variables
class Foo163
{
public:
    uint32 _y = 21;
}

class TestCase163
{
private:
    Foo163 g_foo163;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 21 == g_foo163._y);
    }
}

// Calling a method from inside a class
class Foo164
{
private:
    uint32 Bar()
    {
        return 3;
    }

public:
    uint32 Foo()
    {
        return Bar();
    }
}

class TestCase164
{
private:
    Foo164 g_foo164;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 3 == g_foo164.Foo());
    }
}

// Accessing private variables inside a class
class Foo165
{
private:
    uint32 _x = 7;

public:
    uint32 Foo()
    {
        return _x;
    }
}

class TestCase165
{
private:
    Foo165 g_foo165;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 7 == g_foo165.Foo());
    }
}

inline void test_main()
{
    unit::test<150>(unit::fixture<TestCase150>());
    unit::test<151>(unit::fixture<TestCase151>());
    unit::test<155>(unit::fixture<TestCase155>());
    unit::test<156>(unit::fixture<TestCase156>());
    unit::test<157>(unit::fixture<TestCase157>());
    unit::test<158>(TestCase158);
    unit::test<159>(TestCase159);
    unit::test<160>(unit::fixture<TestCase160>());
    unit::test<161>(unit::fixture<TestCase161>());
    unit::test<162>(unit::fixture<TestCase162>());
    unit::test<163>(unit::fixture<TestCase163>());
    unit::test<164>(unit::fixture<TestCase164>());
    unit::test<165>(unit::fixture<TestCase165>());
}
