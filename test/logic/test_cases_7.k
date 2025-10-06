// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.fifo
import test.unit as unit
import test.runner

// Class aggregation
class Bar167
{
    const uint32 _var = 4;

public:
    uint32 Method()
    {
        return _var;
    }
}

class Foo167
{
private:
    Bar167 _bar;

public:
    uint32 Foo()
    {
        return _bar.Method();
    }
}

class Test167
{
private:
    Foo167 g_foo167;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 4 == g_foo167.Foo());
    }
}

// inline member function
class Foo168
{
public:
    inline uint32 Method(uint32 x)
    {
        return x << 1;
    }
}

class Test168
{
private:
    Foo168 g_foo168;

public:
    void run(unit::tag_t tag)
    {
        uint32 y = g_foo168.Method(4);

        unit::assert(tag, y == 8);
    }
}

// Calls to private methods are directly correctly
class Foo169
{
    uint32 _x;

    uint32 InternalMethod()
    {
        return _x << 1;
    }

public:
    void Set(uint32 x)
    {
        _x = x;
    }

    uint32 Method()
    {
        return InternalMethod();
    }
}

class Test169
{
private:
    Foo169 g_foo169_a;
    Foo169 g_foo169_b;

public:
    void run(unit::tag_t tag)
    {
        g_foo169_a.Set(2);
        g_foo169_b.Set(3);

        unit::assert(tag, g_foo169_a.Method() == 4);
        unit::assert(tag, g_foo169_b.Method() == 6);
    }
}

// Memories
class Test170
{
private:
    [[memory]] uint32[32] g_m170;

    uint32 g_index170;

public:
    void run(unit::tag_t tag)
    {
        g_index170 = 2;

        g_m170[g_index170] = 2;

        unit::assert(tag, g_m170[g_index170] == 2);
    }
}

// 2 read ports
class Test171
{
private:
    [[memory]] uint32[32] g_m171;

public:
    [[pipelined]] void Foo171(uint32 index)
    {
        g_m171[index] = index;
    }

    void run(unit::tag_t tag)
    {
        // Initialize all slots
        Foo171(32);

        unit::assert(tag, g_m171[7] == 7);
        unit::assert(tag, g_m171[31] == 31);
    }
}

// 3 read ports
class Test172
{
private:
    [[memory]] uint32[32] g_m172;

public:
    [[pipelined]] void Foo172(uint32 index)
    {
        g_m172[index] = index;
    }

    void run(unit::tag_t tag)
    {
        // Initialize all slots
        Foo172(32);

        unit::assert(tag, g_m172[1] == 1);
        unit::assert(tag, g_m172[7] == 7);
        unit::assert(tag, g_m172[31] == 31);
    }
}

// 4 read ports
class Test173
{
private:
    [[memory]] uint32[32] g_m173;

public:
    [[pipelined]] void Foo173(uint32 index)
    {
        g_m173[index] = index + 10;
    }

    void run(unit::tag_t tag)
    {
        // Initialize all slots
        Foo173(32);

        unit::assert(tag, g_m173[0] == 10);
        unit::assert(tag, g_m173[1] == 11);
        unit::assert(tag, g_m173[7] == 17);
        unit::assert(tag, g_m173[31] == 41);
    }
}

// bounds checking on read
class Test174
{
private:
    [[memory]] uint32[32] g_m174;

public:
    [[pipelined]] void Foo174(uint32 index)
    {
        g_m174[index] = index + 10;
    }

    void run(unit::tag_t tag)
    {
        // Initialize all slots
        Foo174(32);

        unit::assert(tag, g_m174[32] == 10);
        unit::assert(tag, g_m174[33] == 11);
        unit::assert(tag, g_m174[39] == 17);
        unit::assert(tag, g_m174[95] == 41);
    }
}

// bounds checking on write
class Test175
{
private:
    [[memory]] uint32[32] g_m175;

public:
    [[pipelined]] void Foo175(uint32 index)
    {
        g_m175[index + 64] = index + 10;
    }

    void run(unit::tag_t tag)
    {
        // Initialize all slots
        Foo175(32);

        unit::assert(tag, g_m175[0] == 10);
        unit::assert(tag, g_m175[1] == 11);
        unit::assert(tag, g_m175[7] == 17);
        unit::assert(tag, g_m175[31] == 41);
    }
}

// predicated write to memory
class Test176
{
private:
    [[memory]] uint32[32] g_m176;

public:
    [[pipelined]] void Foo176(uint32 index, uint32 base, bool enable)
    {
        if(enable)
        {
            g_m176[index] = index + base;
        }
    }

    void run(unit::tag_t tag)
    {
        // Initialize all slots
        Foo176(32, 10, true);

        // This will do nothing
        Foo176(32, 20, false);

        unit::assert(tag, g_m176[7] == 17);
    }
}

// memory of struct
struct Bar77
{
    uint11 _a;
    uint6  _b;
    uint1  _c;
}

class Test177
{
private:
    [[memory]] Bar77[32] g_m177;

public:
    [[pipelined]] void Foo177(uint32 index)
    {
        Bar77 b;

        b._a = index + 1;
        b._b = index + 2;
        b._c = static_cast(index);

        g_m177[index] = b;
    }

    void run(unit::tag_t tag)
    {
        Foo177(32);

        Bar77 b = g_m177[7];

        unit::assert(tag, b._a == 8);
        unit::assert(tag, b._b == 9);
        unit::assert(tag, b._c == 1);
    }
}

// memory member variable
class Foo179
{
private:
    [[memory]] uint32[16] _m;

public:
    void Set(uint32 idx, uint32 val)
    {
        _m[idx] = val;
    }

    uint32 Get(uint32 idx)
    {
        return _m[idx];
    }
}

class Test179
{
private:
    Foo179 g_foo179;

public:
    [[max_threads(1)]] void run(unit::tag_t tag)
    {
    [[unordered]] for (const uint32 i : 16)
        {
            g_foo179.Set(i, static_cast(i+2));
        }

        unit::assert(tag, 3 == g_foo179.Get(1));
    }
}

// Ensuring the backends do not flip element width & element count
class Test180
{
private:
    [[memory]] uint8[16] g_mem180;

public:
    [[pipelined]] void Foo180(uint32 index)
    {
        g_mem180[index] = 255 - index;
    }

    void run(unit::tag_t tag)
    {
        // initialize
        Foo180(16);

        unit::assert(tag, 255 == g_mem180[0]);
        unit::assert(tag, 254 == g_mem180[1]);
        unit::assert(tag, 253 == g_mem180[2]);
        unit::assert(tag, 252 == g_mem180[3]);
        unit::assert(tag, 251 == g_mem180[4]);
        unit::assert(tag, 250 == g_mem180[5]);
        unit::assert(tag, 249 == g_mem180[6]);
        unit::assert(tag, 248 == g_mem180[7]);
        unit::assert(tag, 247 == g_mem180[8]);
        unit::assert(tag, 246 == g_mem180[9]);
        unit::assert(tag, 245 == g_mem180[10]);
        unit::assert(tag, 244 == g_mem180[11]);
        unit::assert(tag, 243 == g_mem180[12]);
        unit::assert(tag, 242 == g_mem180[13]);
        unit::assert(tag, 241 == g_mem180[14]);
        unit::assert(tag, 240 == g_mem180[15]);
    }
}

// global array being accessed by many threads concurrently
class Test181
{
private:
    uint32[8] g_mem181;
    uint1 g_foo181Done = 0;
    uint1 g_bar181Done = 0;

public:
    [[async]] void Foo181(unit::tag_t tag)
    {
        g_mem181[1] = 3;
        unit::assert(tag, g_mem181[1] == 3);
        g_foo181Done = 1;
    }

    [[async]] void Bar181(unit::tag_t tag)
    {
        g_mem181[2] = 4;
        unit::assert(tag, g_mem181[2] == 4);
        g_bar181Done = 1;
    }

    [[max_threads(1)]] void run(unit::tag_t tag)
    {
        Foo181(tag);
        Bar181(tag);

        [[unordered]] do; while ((g_foo181Done + g_bar181Done) != 2);

        unit::assert(tag, g_mem181[1] == 3);
        unit::assert(tag, g_mem181[2] == 4);
    }
}

// Test for context savers inside of `[[max_threads(1)]]` functions
class ContextSaverDepth1
{
private:
    uint32 times_seven(uint32 x)
    {
        return x * 7;
    }

    [[max_threads(1)]] void single_threaded(unit::tag_t tag, uint32 x)
    {
        uint32 z = static_cast(x * 3); // z will be saved in a context saver
        uint32 y = times_seven(x);
        uint32 sum = static_cast(y + z);

        unit::assert_equal(tag, x * 10, sum);;
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for (256, [tag](uint32 x)
        {
            single_threaded(tag, x);
        });
    }
}

class Test178
{
private:
    struct S
    {
        uint8 packet_length;
        bool[8] match_array;
    }

    FIFO<uint8, 64, true, true> processing_buffer;

public:
    void run(unit::tag_t tag)
    {
        processing_buffer.enqueue(10);
        processing_buffer.enqueue(11);
        S s = {2, {}};

        pipelined_for(s.packet_length, [s](uint8 tid){
            auto output_flit = processing_buffer.dequeue();
        });
    }

}

inline void test_main()
{
    unit::test<167>(unit::fixture<Test167>());
    unit::test<168>(unit::fixture<Test168>());
    unit::test<169>(unit::fixture<Test169>());
    unit::test<170>(unit::fixture<Test170>());
    unit::test<171>(unit::fixture<Test171>());
    unit::test<172>(unit::fixture<Test172>());
    unit::test<173>(unit::fixture<Test173>());
    unit::test<174>(unit::fixture<Test174>());
    unit::test<175>(unit::fixture<Test175>());
    unit::test<176>(unit::fixture<Test176>());
    unit::test<177>(unit::fixture<Test177>());
    unit::test<178>(unit::fixture<Test178>());
    unit::test<179>(unit::fixture<Test179>());
    unit::test<180>(unit::fixture<Test180>());
    unit::test<181>(unit::fixture<Test181>());
    unit::test<182>(unit::fixture<ContextSaverDepth1>());
}
