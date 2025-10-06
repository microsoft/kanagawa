// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// nested template classes - parent template params visible within child class
template<typename myType, auto myConst>
class Foo500
{
private:
    class Bar
    {
    public:
        myType Transform(myType input)
        {
            return input + myConst;
        }
    }

    Bar _bar;

public:
    myType Entry(unit::tag_t tag)
    {
        myType transformed = _bar.Transform(9);

        unit::assert(tag, transformed == 11);

        return transformed << myConst;
    }
}

class Test500
{
private:
    Foo500<uint32, 2> g_foo500;

public:
    void run(unit::tag_t tag)
    {
        uint32 result = g_foo500.Entry(tag);

        unit::assert(tag, result == 44);
    }
}

// nested template classes - child declared outside of parent
template<typename t, t c>
class Bar501
{
public:
    t Transform(t input)
    {
        return input + c;
    }
}

template<typename myType, auto myConst>
class Foo501
{
private:
    Bar501<uint3, 1> _bar;

public:
    myType Entry(unit::tag_t tag)
    {
        myType transformed = _bar.Transform(static_cast(9));

        unit::assert(tag, transformed == 2);

        return transformed << myConst;
    }
}

class Test501
{
private:
    Foo501<uint32, 2> g_foo501;

public:
    void run(unit::tag_t tag)
    {
        uint32 result = g_foo501.Entry(tag);

        unit::assert(tag, result == 8);
    }
}

// Single-threaded function with multiple call sites
class Test502
{
private:
    [[memory]] uint32[32] g_mem502;

public:
    [[max_threads(1)]] uint32 Foo502(uint32 idx, uint32 val, bool accumulate)
    {
        uint32 prev = g_mem502[idx];

        uint32 new = static_cast(accumulate ? prev + val : val);

        g_mem502[idx] = new;

        return new;
    }

    void run(unit::tag_t tag)
    {
        // mem[3] = 4
        unit::assert(tag, 4 == Foo502(3, 4, false));

        // mem[3] += 7
        unit::assert(tag, 11 == Foo502(3, 7, true));

        // mem[2] = 5
        unit::assert(tag, 5 == Foo502(2, 5, false));

        // mem[2] += 1
        unit::assert(tag, 6 == Foo502(2, 1, true));
    }
}

// nested classes with function name collision with parent
class Foo504
{
public:
    class Bar
    {
    public:
        uint32 Entry()
        {
            return 5;
        }
    }

    Bar _bar;

    uint32 Entry()
    {
        return _bar.Entry() + 1;
    }
}

class Test504
{
private:
    Foo504 g_foo504;

public:
    void run(unit::tag_t tag)
    {
        uint32 result = g_foo504.Entry();

        unit::assert(tag, result == 6);
    }
}

// nested classes with name collision with parent
class Foo505
{
public:
    class Bar
    {
    private:
        uint32 _x;

    public:
        void Set(uint32 x)
        {
            _x = x;
        }

        uint32 Entry()
        {
            return _x + 1;
        }
    }; // testing semi-colon here

    Bar _bar;
    uint32 _x;

    uint32 Entry()
    {
        _bar.Set(3);

        _x = 8;

        return _bar.Entry();
    }
}

class Test505
{
private:
    Foo505 g_foo505;

public:
    void run(unit::tag_t tag)
    {
        uint32 result = g_foo505.Entry();

        unit::assert(tag, result == 4);
    }
}

// Repro of an assertion in the compiler with double casting
struct Foo506
{
    uint32 x;
}

inline void TestCase506(unit::tag_t tag)
{
    Foo506 f;
    f.x = 7;

    uint8 u = cast<uint8>(cast<uint32>(f));
    unit::assert(tag, u == 7);
}

// Repro of PackLuts bug
class Test507
{
private:
    struct L1CacheTagSet
    {
        uint39[2] _tags;
        bool[2] _valid;
        uint1 _next;
    };

    [[memory]] L1CacheTagSet[1024] L1CacheTags;

public:
    // Lookup : looks up flow table entry that matches a packet
    void Lookup507(
        uint10 packetId,
        uint64 generationId,
        uint8 gftFlags,
        unit::tag_t tag
    )
    {
        uint49 l2HashRaw = 0x10933671AF854;

        print("rawhash {l2HashRaw}\n");

        uint10 setIndex = static_cast(l2HashRaw);
        uint39 tagIn = (l2HashRaw >> 10) & 0x7fffffffff;

        print("SetIndex tagIn {setIndex} {tagIn}\n");

        bool hit = false;

        // Grab the L1 cache tag lock
        atomic do; while(false);

        // Read the L1 Cache set
        L1CacheTagSet cTags = L1CacheTags[setIndex];

        uint2 emptyIndex = 2;
        uint2 lineIndex = 0;
        // Search for the tag in the set
        static for(const auto i : 2)
        {
            print("loop  {i} {cTags._valid[i]}\n");

            if (!hit && cTags._tags[i] == tagIn && cTags._valid[i])
            {
                hit = true;
                lineIndex = i;
                barrier;
                print("PD::Lookup : found entry in hash! {i} {lineIndex}\n");
                barrier;
                unit::assert(tag, i == lineIndex);
            }
            else if (cTags._valid[i] == false)
            {
                emptyIndex = i;
            }
        }

        bool evict = false;

        if (!hit)
        {
            if (emptyIndex < 2)
            {
                lineIndex = emptyIndex;
            }
            else
            {
                lineIndex = cTags._next;
                cTags._next = cTags._next + 1;
                evict = true;
            }

            cTags._tags[lineIndex] = tagIn;
            cTags._valid[lineIndex] = true;
        }

        L1CacheTags[setIndex] = cTags;
    }

    void run(unit::tag_t tag)
    {
        Lookup507(0, 0, 0, tag);
        Lookup507(0, 0, 0, tag);
    }
}

// atomic read-modify-write to memory
class Test508
{
private:
    [[memory]] uint32[512] g_mem508;

public:
    [[pipelined]] void Foo508(uint32 threadId)
    {
        uint9 slotIndex = cast<uint4>(threadId);
        bool isFirst = threadId < 16;

        atomic
        {
            uint32 val = g_mem508[slotIndex];
            val = isFirst ? (slotIndex + 1) : (val + 1);
            print("threadId {threadId} slotIndex {slotIndex} val {val}\n");
            g_mem508[slotIndex] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        const auto NumIterations = 16;

        Foo508(NumIterations * 16);

        for (const uint32 i : 16)
        {
            uint32 val = g_mem508[i];

            uint32 expected = static_cast(i + NumIterations);

            unit::assert(tag, val == expected);
        }
    }
}

// atomic read-modify-write to memory - uses bypass
class Test509
{
private:
    [[memory]] uint32[512] g_mem509;

    struct ThreadId509
    {
        uint4 IterationIndex;
        uint9 SlotIndex;
        uint19 Padding;
    }

public:
    [[pipelined]] void Foo509(uint32 threadId)
    {
        ThreadId509 decomposedThreadId = cast<ThreadId509>(threadId);

        uint9 slotIndex = decomposedThreadId.SlotIndex;
        bool isFirst = (decomposedThreadId.IterationIndex == 0);

        atomic
        {
            auto val = g_mem509[slotIndex];
            val = isFirst ? (slotIndex + 1) : (val + 1);
            g_mem509[slotIndex] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        const auto NumIterations = 16;

        Foo509(NumIterations * 512);

        for (const uint32 i : 512)
        {
            uint32 val = g_mem509[i];

            uint32 expected = static_cast(i + NumIterations);

            unit::assert(tag, val == expected);
        }
    }
}

// atomic read-modify-write to memory - uses bypass - 1/2 frequency, 2 slots
class Test510
{
private:
    [[memory]] uint32[512] g_mem510;

public:
    [[pipelined]] void Foo510(uint32 threadId)
    {
        uint9 slotIndex = threadId & 1;
        bool isFirst = threadId < 2;

        atomic
        {
            uint32 val = g_mem510[slotIndex];
            val = isFirst ? (slotIndex + 1) : (val + 1);
            g_mem510[slotIndex] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        Foo510(512);

        for (const uint32 i : 2)
        {
            uint32 val = g_mem510[i];

            uint32 expected = static_cast(i + 256);

            unit::assert(tag, val == expected);
        }
    }
}

// atomic read-modify-write to memory - uses bypass - 1/3 frequency, 3 slots
class Test511
{
private:
    [[memory]] uint32[512] g_mem511;

    uint9 g_slotIndex511 = 0;

public:
    [[pipelined]] void Foo511(uint32 threadId)
    {
        uint9 slotIndex;

        atomic
        {
            slotIndex = g_slotIndex511;

            g_slotIndex511 = (g_slotIndex511 == 2) ? 0 : (g_slotIndex511 + 1);
        }

        bool isFirst = threadId < 3;

        atomic
        {
            auto val = g_mem511[slotIndex];
            val = isFirst ? (slotIndex + 1) : (val + 1);
            g_mem511[slotIndex] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        Foo511(512);

        for (const uint32 i : 3)
        {
            uint32 val = g_mem511[i];

            uint32 expected = static_cast(i + mux(cast<uint2>(i), 171, 171, 170, 0));

            unit::assert(tag, val == expected);
        }
    }
}

// atomic read-modify-write to memory - uses bypass - 1/4 frequency, 4 slots
class Test512
{
private:
    [[memory]] uint32[512] g_mem512;

    uint9 g_slotIndex512 = 0;

public:
    [[pipelined]] void Foo512(uint32 threadId)
    {
        uint9 slotIndex;

        atomic
        {
            slotIndex = g_slotIndex512;

            g_slotIndex512 = (g_slotIndex512 == 3) ? 0 : (g_slotIndex512 + 1);
        }

        bool isFirst = threadId < 4;

        atomic
        {
            uint32 val = g_mem512[slotIndex];
            val = isFirst ? (slotIndex + 1) : (val + 1);
            g_mem512[slotIndex] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        Foo512(512);

        for (const uint32 i : 4)
        {
            uint32 val = g_mem512[i];

            uint32 expected = static_cast(i + 128);

            unit::assert(tag, val == expected);
        }
    }
}

// atomic read-modify-write to memory - in a predicate
class Test513
{
private:
    [[memory]] uint32[512] g_mem513;

    struct ThreadId513
    {
        uint4 IterationIndex;
        uint9 SlotIndex;
        uint19 Padding;
    }

public:
    [[pipelined]] void Foo513(uint32 threadId)
    {
        ThreadId513 decomposedThreadId = cast<ThreadId513>(threadId);

        uint9 slotIndex = decomposedThreadId.SlotIndex;
        bool isFirst = (decomposedThreadId.IterationIndex == 0);

        bool predicate = (0 == (decomposedThreadId.IterationIndex & 1));

        if (predicate)
        {
            atomic
            {
                auto val = g_mem513[slotIndex];
                val = isFirst ? (slotIndex + 1) : (val + 1);
                g_mem513[slotIndex] = val;
            }
        }
    }

    void run(unit::tag_t tag)
    {
        const auto NumIterations = 16;

        Foo513(NumIterations * 512);

        for (const uint32 i : 512)
        {
            uint32 val = g_mem513[i];

            uint32 expected = static_cast(i + 8);

            unit::assert(tag, val == expected);
        }
    }
}

inline void test_main()
{
    unit::test<500>(unit::fixture<Test500>());
    unit::test<501>(unit::fixture<Test501>());
    unit::test<502>(unit::fixture<Test502>());
    unit::test<504>(unit::fixture<Test504>());
    unit::test<505>(unit::fixture<Test505>());
    unit::test<506>(TestCase506);
    unit::test<507>(unit::fixture<Test507>());
    unit::test<508>(unit::fixture<Test508>());
    unit::test<509>(unit::fixture<Test509>());
    unit::test<510>(unit::fixture<Test510>());
    unit::test<511>(unit::fixture<Test511>());
    unit::test<512>(unit::fixture<Test512>());
    unit::test<513>(unit::fixture<Test513>());
}
