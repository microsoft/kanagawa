// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// switch with default - not taken
inline void TestCase425(unit::tag_t tag)
{
    uint32 x = 1;

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

    unit::assert(tag, result == 8);
}

// switch with no default
inline void TestCase426(unit::tag_t tag)
{
    uint32 x = 3;

    uint32 result = 11;

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

    unit::assert(tag, result == 11);
}

// switch with no default - not taken
inline void TestCase427(unit::tag_t tag)
{
    uint32 x = 1;

    uint32 result = 11;

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

    unit::assert(tag, result == 8);
}

// switch with all cases covered and no default
inline void TestCase428(unit::tag_t tag)
{
    uint2 x = 1;

    uint32 result = 11;

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

    case 3:
        result = 10;
        break;
    }

    unit::assert(tag, result == 8);
}

// switch with default not at the end - taken
inline void TestCase429(unit::tag_t tag)
{
    uint2 x = 1;

    uint32 result = 11;

    switch (x)
    {
    case 0:
        result = 7;
        break;

    default:
        result = 8;
        break;

    case 2:
        result = 9;
        break;

    case 3:
        result = 10;
        break;
    }

    unit::assert(tag, result == 8);
}

// switch alias
inline void TestCase430(unit::tag_t tag)
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

// Basic reorder buffer test
class TestCase431
{
private:
    uint32 g_x431 = 0;
    uint32 g_idx431 = 0;
    uint32 g_dummy431 = 0;

    [[pipelined]] void Foo431(uint32 idx, unit::tag_t tag)
    {
        // Create a live variable that will be used after the reording
        uint32 idxTimesTwo = static_cast(idx * 2);
        uint32 idxTimesFour;

        reorder
        {
            if (0 == (idx % 2))
            {
                g_x431 += 1;
            }
            else
            {
                g_x431 += 2;

                // Delay for a while, to ensure threads leave in a different order than they enter
                [[unordered]] for (const uint32 i : 10)
                {
                    g_dummy431 = i;
                }
            }

            // Create another live variable that will be used after the reording
            idxTimesFour = idx * 4;
        }

        // Validate live variables
        unit::assert(tag, idxTimesTwo == (idx * 2));
        unit::assert(tag, idxTimesFour == (idx * 4));

        // Validate global ordering
        uint32 snappedIndexValue;

        atomic
        {
            snappedIndexValue = g_idx431;

            g_idx431++;
        }

        unit::assert(tag, snappedIndexValue == idx);
    }

public:
    void run(unit::tag_t tag)
    {
        Foo431(4096, tag);
    }
}

// Reorder buffer filling up
class TestCase432
{
private:
    uint32 g_idx432 = 0;
    uint32 g_dummy432 = 0;

    [[pipelined, max_threads(512)]] void Foo432(uint32 idx, unit::tag_t tag)
    {
        // Create a live variable that will be used after the reording
        uint32 idxTimesTwo = static_cast(idx * 2);
        uint32 idxTimesFour;

        reorder
        {
            // thread 0 will loop for 512 iterations
            // thread 1 will loop for 511 iterations
            // This ensures that threads leave in the opposite order that the enter
            // and at some point, the buffer will be full
            uint32 loopIterations = static_cast(512 - (idx % 512));

            // Delay for a while, to ensure threads leave in a different order than they enter
            [[unordered]] for (const uint32 i : loopIterations)
            {
                g_dummy432 = i;
            }

            // Create another live variable that will be used after the reording
            idxTimesFour = idx * 4;
        }

        println(idx);

        // Validate live variables
        unit::assert(tag, idxTimesTwo == (idx * 2));
        unit::assert(tag, idxTimesFour == (idx * 4));

        // Validate global ordering
        uint32 snappedIndexValue;

        atomic
        {
            snappedIndexValue = g_idx432;

            g_idx432++;
        }

        unit::assert(tag, snappedIndexValue == idx);
    }

public:
    void run(unit::tag_t tag)
    {
        //Foo432(4096, tag);
    }
}

// switch in an ordered function
inline uint32 Foo433(uint32 x)
{
    uint32 result;

    switch (x)
    {
    case 1:
        result = 4;
        break;

    case 2:
        result = 8;
        break;

    default:
        result = 10;
        break;
    }

    return result;
}

inline void TestCase433(unit::tag_t tag)
{
    unit::assert(tag, 4 == Foo433(1));
    unit::assert(tag, 8 == Foo433(2));
    unit::assert(tag, 10 == Foo433(3));
    unit::assert(tag, 10 == Foo433(4));
}

// X * bytesizeof(some type that is a power of 2)

// 16 bytes
struct S435
{
    uint64 a;
    uint64 b;
}

inline void Foo435(uint32 x, uint32 expected, unit::tag_t tag)
{
    unit::assert(tag, (x * bytesizeof(S435)) == expected);
}

inline void TestCase435(unit::tag_t tag)
{
    Foo435(7, 112, tag);
    Foo435(15, 240, tag);
}

struct BearerCounters437
{
    uint16 _byteCount;
    uint4 _packetCount;
}

// memory of struct - repro of a bug hit by EPC code
class TestCase437
{
private:
    [[memory]] BearerCounters437[1024] _memory437;

public:
    void run(unit::tag_t tag)
    {
        BearerCounters437 ctr;

        ctr._byteCount = 4;
        ctr._packetCount = 3;

        _memory437[1] = ctr;

        BearerCounters437 read = _memory437[1];

        unit::assert(tag, read._byteCount == 4);
        //unit::assert(tag, read._packetCount == 3); // the bug occurs when 1 field is not used
    }
}

// Ordered, predicated synchronous call
class TestCase439
{
private:
    uint32 Foo439(uint32 x)
    {
        return x + 1;
    }

    uint32 g_threadId439;

    [[pipelined]] void Bar439(uint32 threadId, unit::tag_t tag)
    {
        if (0 == threadId)
        {
            unit::assert(tag, 5 == Foo439(4));
        }

        if (1 == threadId)
        {
            unit::assert(tag, 10 == Foo439(9));
        }

        if (2 == threadId)
        {
            unit::assert(tag, 11 == Foo439(10));
        }

        // Verify thread ordering has been maintained
        uint32 snappedThreadId;

        atomic
        {
            snappedThreadId = g_threadId439;

            g_threadId439++;
        }

        unit::assert(tag, threadId == snappedThreadId);
    }

public:
    void run(unit::tag_t tag)
    {
        g_threadId439 = 0;

        Bar439(3, tag);
    }
}

// predicated synchronous call to pipelined function
class TestCase440
{
private:
    [[pipelined]] uint32 Foo440(uint32 threadId, uint32 x)
    {
        return threadId + x;
    }

    uint32 g_threadId440;

    [[pipelined]] void Bar440(uint32 threadId, unit::tag_t tag)
    {
        if (0 == threadId)
        {
            uint32[4] data = Foo440(4, 7);

            static for(const auto i : 4)
            {
                unit::assert(tag, data[i] == (7 + i));
            }
        }

        if (1 == threadId)
        {
            uint32[4] data = Foo440(4, 12);

            static for(const auto i : 4)
            {
                unit::assert(tag, data[i] == (12 + i));
            }
        }

        if (2 == threadId)
        {
            uint32[4] data = Foo440(4, 3);

            static for(const auto i : 4)
            {
                unit::assert(tag, data[i] == (3 + i));
            }
        }

        // Verify thread ordering has been maintained
        uint32 snappedThreadId;

        atomic
        {
            snappedThreadId = g_threadId440;

            g_threadId440++;
        }

        unit::assert(tag, threadId == snappedThreadId);
    }

public:
    void run(unit::tag_t tag)
    {
        g_threadId440 = 0;

        Bar440(3, tag);
    }
}

// test for reorder buffer checking output fifo full
class TestCase434
{
    uint32 g_x434 = 0;
    uint32 g_y434 = 0;
    uint32 g_completed434 = 0;
    bool g_gate434 = false;

    [[async, pipelined]] void Foo434(uint32 x)
    {
        uint32 iterationCount = (x % 16);

        reorder
        {
             [[unordered]] for (const uint32 i : iterationCount)
            {
                atomic
                {
                    g_x434++;
                }
            }
        }

        atomic do; while(!(g_gate434));

        atomic
        {
            g_completed434++;
        }
    }

public:
    [[unordered]] void run(unit::tag_t tag)
    {
        Foo434(4096);

        // Stall things for a while
        [[unordered]] for (const uint32 i : 1000)
        {
            atomic
            {
                g_y434++;
            }
        }

        g_gate434 = true;

        atomic do; while(!(g_completed434 == 4096));
    }
}

inline void test_main()
{
    unit::test<425>(TestCase425);
    unit::test<426>(TestCase426);
    unit::test<427>(TestCase427);
    unit::test<428>(TestCase428);
    unit::test<429>(TestCase429);
    unit::test<430>(TestCase430);
    unit::test<431>(unit::fixture<TestCase431>());
    unit::test<432>(unit::fixture<TestCase432>());
    unit::test<433>(TestCase433);
    unit::test<434>(unit::fixture<TestCase434>());
    unit::test<435>(TestCase435);
    unit::test<437>(unit::fixture<TestCase437>());
    unit::test<439>(unit::fixture<TestCase439>());
    unit::test<440>(unit::fixture<TestCase440>());
}
