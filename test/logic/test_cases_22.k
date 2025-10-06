// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Initialized memory
class Test590
{
private:
    [[memory]] uint32[8] g_mem590 = { 4, 5+6, 1, 2, 3, 4 };

public:
    void run(unit::tag_t tag)
    {
        for (const uint32 i : 8)
        {
            uint32 expected = mux(cast<uint3>(i), 4, 11, 1, 2, 3, 4, 0, 0);

            uint32 actual = g_mem590[i];

            unit::assert(tag, expected == actual);
        }
    }
}

// Initialized memory - deep enough to require hex addresses in mif file
class Test591
{
private:
    [[memory]] uint32[16] g_mem591 = { 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115 };

public:
    void run(unit::tag_t tag)
    {
        for (const uint32 i : 16)
        {
            uint32 expected = static_cast(100 + i);

            uint32 actual = g_mem591[i];

            unit::assert(tag, expected == actual);
        }
    }
}

// Initialized memory - empty initializer list
class Test592
{
private:
    [[memory]] uint32[32] g_mem592 = { };

public:
    void run(unit::tag_t tag)
    {
        for (const uint32 i : 32)
        {
            uint32 expected = 0;

            uint32 actual = g_mem592[i];

            unit::assert(tag, expected == actual);
        }
    }
}

// Initialized memory - replicated
class Test593
{
private:
    [[memory]] uint32[8] g_mem593 = { 4, 5+6, 1, 2, 3, 4 };

public:
    void run(unit::tag_t tag)
    {
        for (const uint32 i : 8)
        {
            uint32 expected = mux(cast<uint3>(i), 4, 11, 1, 2, 3, 4, 0, 0);

            uint32 actual = g_mem593[i];

            unit::assert(tag, expected == actual);
        }

        for (const uint32 i : 8)
        {
            uint32 expected = mux(cast<uint3>(i), 4, 11, 1, 2, 3, 4, 0, 0);

            uint32 actual = g_mem593[i];

            unit::assert(tag, expected == actual);
        }
    }
}

// Initialized memory - verify writes work
class Test594
{
private:
    [[memory]] uint32[8] g_mem594 = { 4, 5+6, 1, 2, 3, 4 };

public:
    void run(unit::tag_t tag)
    {
        for (const uint32 i : 8)
        {
            uint32 expected = mux(cast<uint3>(i), 4, 11, 1, 2, 3, 4, 0, 0);

            uint32 actual = g_mem594[i];

            unit::assert(tag, expected == actual);
        }

        g_mem594[1] = 100;

        for (const uint32 i : 8)
        {
            uint32 expected = mux(cast<uint3>(i), 4, 100, 1, 2, 3, 4, 0, 0);

            uint32 actual = g_mem594[i];

            unit::assert(tag, expected == actual);
        }
    }
}

// Load from a memory in atomic block - load is predicated
class Test595
{
private:
    [[memory]] uint32[512] g_mem595;

public:
    [[pipelined]] void Helper595(uint9 tid, bool predicate, unit::tag_t tag)
    {
        uint32 loadedValue = tid;

        atomic
        {
            if (predicate)
            {
                loadedValue = g_mem595[tid];

                g_mem595[tid] = tid + 10;
            }
        }

        uint32 expected = predicate ? tid + 20 : tid;

        unit::assert(tag, loadedValue == expected);
    }

    void run(unit::tag_t tag)
    {
        // Fill the memory
        for (const auto i : 512)
        {
            g_mem595[i] = i + 20;
        }

        Helper595(512, false, tag);

        Helper595(512, true, tag);
    }
}

// Load from a memory in atomic block - load address comes from an array
class Test596
{
private:
    [[memory]] uint32[512] g_mem596;

public:
    [[pipelined]] void Helper596(uint9 tid, uint1 whichIndex)
    {
        uint32[2] indexArray;

        indexArray[0] = tid;
        indexArray[1] = tid / 2;

        atomic
        {
            g_mem596[indexArray[whichIndex]]++;
        }
    }

    void run(unit::tag_t tag)
    {
        // Fill the memory
        for (const auto i : 512)
        {
            g_mem596[i] = i;
        }

        Helper596(512, 0);

        Helper596(512, 1);

        for (const auto i : 512)
        {
            uint32 expected = (i < 256) ? (i + 3) : i + 1;

            uint32 actual = g_mem596[i];

            unit::assert(tag, expected == actual);
        }
    }
}

// Repro of a bug that occured with reorder buffer + max_threads + a deep pipeline
// The HW reorder buffer slot id width was not wide enough
class Test597
{
private:
    uint32 g_shared597 = 0;

public:
    [[pipelined, max_threads(8)]] void Helper597(uint32 tid)
    {
        for (const uint32 i : 32)
        {
            // Force a deep pipeline here
            uint32 toAdd = 0;

            static for (const auto i : 128)
            {
                toAdd += tid;
            }

            atomic
            {
                g_shared597 += toAdd;
            }
        }
    }

    void run(unit::tag_t tag)
    {
        Helper597(100);

        unit::assert(tag, g_shared597 == 0x01356000);
    }
}

class Test598
{
private:
    struct S1598
    {
        uint32 x;
        int16 y;
        bool b;
    };

public:
    S1598 Init1598()
    {
        return { 2 + 5, -3, true };
    }

    S1598 Init2598()
    {
        return { 3 - 1 };
    }

    void run(unit::tag_t tag)
    {
        S1598 s1 = Init1598();

        unit::assert(tag, s1.x == 7);
        unit::assert(tag, s1.y == -3);
        unit::assert(tag, s1.b == true);

        // Not all elements specified
        const S1598 s2 = Init2598();

        unit::assert(tag, s2.x == 2);
        unit::assert(tag, s2.y == 0);
        unit::assert(tag, s2.b == false);
    }
}

class Test599
{
private:
    struct S2599
    {
        uint32 x;
        int16 y;
        bool b;
    };

public:
    void Init1599(S2599 s1, unit::tag_t tag)
    {
        unit::assert(tag, s1.x == 7);
        unit::assert(tag, s1.y == -3);
        unit::assert(tag, s1.b == true);
    }

    void Init2599(S2599 s2, unit::tag_t tag)
    {
        unit::assert(tag, s2.x == 2);
        unit::assert(tag, s2.y == 0);
        unit::assert(tag, s2.b == false);
    }

    void run(unit::tag_t tag)
    {
        Init1599({ 2 + 5, -3, true }, tag);
        // Not all elements specified
        Init2599({ 3 - 1 }, tag);
    }
}

inline void test_main()
{
    unit::test<590>(unit::fixture<Test590>());
    unit::test<591>(unit::fixture<Test591>());
    unit::test<592>(unit::fixture<Test592>());
    unit::test<593>(unit::fixture<Test593>());
    unit::test<594>(unit::fixture<Test594>());
    unit::test<595>(unit::fixture<Test595>());
    unit::test<596>(unit::fixture<Test596>());
    unit::test<597>(unit::fixture<Test597>());
    unit::test<598>(unit::fixture<Test598>());
    unit::test<599>(unit::fixture<Test599>());
}
