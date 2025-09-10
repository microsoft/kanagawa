// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// void function with return statement
inline void Foo75()
{
    return;
}

inline void TestCase75(unit::tag_t tag)
{
    Foo75();

    unit::assert(tag, 3 == 3);
}


inline void TestCase76(unit::tag_t tag)
{
    uint32 x = 1;

    if (x > 0)
    {
        x = 4;

        if (x > 5)
        {
            x = 10;
        }
        else
        {
            x = 3;
        }
    }

    unit::assert(tag, x == 3);
}


// Test for pipelining of partial struct updates
struct Foo76
{
    uint8 x;
    uint8 y;
    uint8 z;
}

inline void TestCase77(unit::tag_t tag)
{
    Foo76 a;
    Foo76 b;

    a.x = 1;
    a.y = 2;
    a.z = 3;

    // Write b in 1 operation
    b = a;

    // Overwrite b.y
    b.y = 10;

    // Read all of b
    Foo76 c;
    c = b;

    // Verify that everything worked correctly
    unit::assert(tag, c.x == 1);
    unit::assert(tag, c.y == 10);
    unit::assert(tag, c.z == 3);
}


// Test for pipelining of partial struct updates
struct Foo77
{
    uint8 x;
    uint8 y;
    uint8 z;
}

inline void TestCase78(unit::tag_t tag)
{
    Foo77 a;
    Foo77 b;

    a.x = 1;
    a.y = 2;
    a.z = 3;

    // Write b in 1 operation
    b = a;

    // Overwrite b.y
    b.y = 10;

    // Verify that everything worked correctly
    unit::assert(tag, b.x == 1);
    unit::assert(tag, b.y == 10);
    unit::assert(tag, b.z == 3);
}

// Async function call
class Test79
{
private:
    uint32 g79 = 0;

public:
    [[async]] void Foo79()
    {
        g79 = 4;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        // Create async thread
        Foo79();

        // Wait for the global variable to change
        [[unordered]] do; while (g79 == 0);

        unit::assert(tag, g79 == 4);
    }
}

// Async function call with parameters
class Test80
{
private:
    uint32 g80 = 0;

public:
    [[async]] void Foo80(uint32 x, uint32 y)
    {
        g80 = x + y;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        // Create async thread
        Foo80(4, 5);

        // Wait for the global variable to change
        [[unordered]] do; while (g80 == 0);

        unit::assert(tag, g80 == 9);
    }
}

// pipelined async_call
class Test81
{
private:
    uint32 g81 = 0;

public:
    [[pipelined, async]] void Foo81(uint32 iteration)
    {
        g81 = iteration;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        // Create async thread
        Foo81(4);

        // Wait for the global variable to change
        [[unordered]] do; while (g81 != 3);

        unit::assert(tag, g81 == 3);
    }
}

// pipelined async_call with parameters
class Test83
{
private:
    uint32 g83 = 0;

public:
    [[pipelined, async]] void Foo83(uint32 iteration, uint32 scale)
    {
        g83 = iteration + scale;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        // Create async thread
        Foo83(4, 2);

        // Wait for the global variable to change
        [[unordered]] do; while (g83 != 5);

        unit::assert(tag, g83 == 5);
    }
}

// pipelined async_call and FIFO merger
class Test84
{
private:
    uint32 g84 = 0;

public:
    [[pipelined, async]] void Foo84(uint32 iteration)
    {
        g84 = iteration;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        // Create async thread
        Foo84(4);

        // Wait for the global variable to change
        [[unordered]] do; while (g84 != 3);

        unit::assert(tag, g84 == 3);

        // Create async thread
        Foo84(10);

        // Wait for the global variable to change
        [[unordered]] do; while (g84 != 9);

        unit::assert(tag, g84 == 9);
    }
}

// pipelined async_call bit width test
class Test85
{
private:
    uint32 g85 = 0;

public:
    [[pipelined, async]] void Foo85(uint2 iteration)
    {
        g85 = iteration;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        // Create async thread
        Foo85(3);

        // Wait for the global variable to change
        [[unordered]] do; while (g85 != 2);

        unit::assert(tag, g85 == 2);
    }
}

// pipelined async_call count arg/parameter width difference
class Test86
{
private:
    uint32 g86 = 0;

public:
    [[pipelined, async]] void Foo86(uint2 iteration)
    {
        g86 = iteration;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        uint8 count = 3;

        // Create async thread
        Foo86(count);

        // Wait for the global variable to change
        [[unordered]] do; while (g86 != 2);

        unit::assert(tag, g86 == 2);
    }
}

// pipelined async_call count arg/parameter width difference
class Test87
{
private:
    uint32 g87 = 0;

public:
    [[pipelined, async]] void Foo87(uint32 iteration)
    {
        g87 = iteration;
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        uint8 count = 3;

        // Create async thread
        Foo87(count);

        // Wait for the global variable to change
        [[unordered]] do; while (g87 != 2);

        unit::assert(tag, g87 == 2);
    }
}

// pipelined async call where callee does not read iteration index
class Test88
{
private:
    uint32 g88 = 0;

public:
    [[pipelined, async]] void Foo88(uint32 iteration, uint32 p, unit::tag_t tag)
    {
        g88 = p;
        unit::assert(tag, p == 3);
    }

    void run(unit::tag_t tag)
    {
        // Create async threads
        Foo88(2, 3, tag);
    }
}


// 2 invocations of the same pipelined async_call -test thread ordering
class Test89
{
private:
    uint32 maxParentIteration = 0;

public:
    [[pipelined, async]] void Inner89(uint32 iteration, uint32 parentIteration, unit::tag_t tag)
    {
        unit::assert(tag, maxParentIteration <= parentIteration);

        atomic
        {
            maxParentIteration = parentIteration;
        }
    }

    [[pipelined, async]] void Outer89(uint32 iteration, unit::tag_t tag)
    {
        // Create async threads
        Inner89(3, iteration, tag);
    }

    void run(unit::tag_t tag)
    {
        // Create async threads
        Outer89(2, tag);
    }
}

// 2 calls to 1 async function in the same stage
// They should go to separate FIFOs
class Test91
{
private:
    uint32 g91 = 0;

public:
    [[async]] void Foo91(uint32 param)
    {
        atomic
        {
            g91 = g91 + param;
        }
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        Foo91(1);
        Foo91(3);

        // Wait for the global variable to change
        [[unordered]] do; while (g91 != 4);

        unit::assert(tag, g91 == 4);
    }
}


// Function call saving variables
class Test92
{
private:
    uint32 g92 = 0;

public:
    void Foo92(uint32 x)
    {
        g92 = g92 + x;
    }

    void run(unit::tag_t tag)
    {
        uint32 a = g92 + 10;

        unit::assert(tag, a == 10);

        Foo92(1);

        unit::assert(tag, a == 10);
    }
}


// Deadlock prevention in loops
class Test93
{
private:
    uint32 threadCount93 = 0;
    uint32 bla93 = 0;

public:
    [[pipelined, async]] void Foo93(uint32 x)
    {
        [[unordered]] for (const uint32 i : 10)
        {
            // Multi-cycle operation
            uint32 y = ((i + x) << 3) + 7;

            // Ensure y is not optimized away
            bla93 = y;
        }

        atomic
        {
            threadCount93 = threadCount93 + 1;
        }
    }

    [[unordered]] void run(unit::tag_t tag)
    {
        Foo93(100);

        // Wait for the global variable to change
        [[unordered]] do; while (threadCount93 != 0);

        unit::assert(tag, threadCount93 != 100);
    }
}


// flat if
class Test94
{
private:
    bool b94 = false;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 5;

        if (b94)
        {
            x = 6;
        }

        unit::assert(tag, x == 5);
    }
}


// flat if/else
class Test95
{
private:    
    bool b95 = false;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 5;

        if (b95)
        {
            x = 6;
        }
        else
        {
            x = 7;
        }

        unit::assert(tag, x == 7);
    }
}


// member assignment inside flat if/else
class Test96
{
private:   
    bool b96 = false;
    struct Foo96
    {
        uint2 x;
        uint3 y;
    }

public:
    void run(unit::tag_t tag)
    {
        Foo96 foo;
        foo.x = 1;
        foo.y = 2;

        if (b96)
        {
            foo.y = 1;
        }
        else
        {
            foo.y = 3;
        }

        unit::assert(tag, foo.x == 1);
        unit::assert(tag, foo.y == 3);
    }
}


// member assignment inside flat if/else
class Test97
{
private:   
    bool b97 = true;
    struct Foo97
    {
        uint2 x;
        uint3 y;
    }

public:
    void run(unit::tag_t tag)
    {
        Foo97 foo;
        foo.x = 1;
        foo.y = 2;

        if (b97)
        {
            foo.y = 1;
        }
        else
        {
            foo.y = 3;
        }

        unit::assert(tag, foo.x == 1);
        unit::assert(tag, foo.y == 1);
    }
}


// array assignment inside flat if/else
class Test98
{
private:   
    bool b98 = false;

public:
    void run(unit::tag_t tag)
    {
        uint32[4] array;

        array[0] = 1;
        array[1] = 2;
        array[2] = 3;
        array[3] = 4;

        if (b98)
        {
            array[1] = 7;
        }
        else
        {
            array[1] = 8;
        }

        unit::assert(tag, array[1] == 8);

        unit::assert(tag, array[0] == 1);
        unit::assert(tag, array[2] == 3);
        unit::assert(tag, array[3] == 4);
    }
}


// array assignment inside flat if/else
class Test99
{
private:   
    bool b99 = true;

public:
    void run(unit::tag_t tag)
    {
        uint32[4] array;

        array[0] = 1;
        array[1] = 2;
        array[2] = 3;
        array[3] = 4;

        if (b99)
        {
            array[1] = 7;
        }
        else
        {
            array[1] = 8;
        }

        unit::assert(tag, array[1] == 7);

        unit::assert(tag, array[0] == 1);
        unit::assert(tag, array[2] == 3);
        unit::assert(tag, array[3] == 4);
    }
}

inline void test_main()
{
    unit::test<75>(TestCase75);
    unit::test<76>(TestCase76);
    unit::test<77>(TestCase77);
    unit::test<78>(TestCase78);
    unit::test<79>(unit::fixture<Test79>());
    unit::test<80>(unit::fixture<Test80>());
    unit::test<81>(unit::fixture<Test81>());
    unit::test<83>(unit::fixture<Test83>());
    unit::test<84>(unit::fixture<Test84>());
    unit::test<85>(unit::fixture<Test85>());
    unit::test<86>(unit::fixture<Test86>());
    unit::test<87>(unit::fixture<Test87>());
    unit::test<88>(unit::fixture<Test88>());
    unit::test<89>(unit::fixture<Test89>());
    unit::test<91>(unit::fixture<Test91>());
    unit::test<92>(unit::fixture<Test92>());
    unit::test<93>(unit::fixture<Test93>());
    unit::test<94>(unit::fixture<Test94>());
    unit::test<95>(unit::fixture<Test95>());
    unit::test<96>(unit::fixture<Test96>());
    unit::test<97>(unit::fixture<Test97>());
    unit::test<98>(unit::fixture<Test98>());
    unit::test<99>(unit::fixture<Test99>());
}
