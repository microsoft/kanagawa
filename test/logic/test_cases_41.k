// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

template <typename T, (T) -> bool F>
inline T ZeroIf(T x)
{
    return F(x) ? 0 : x;
}

template <typename T, (T) -> bool F1, (T) -> bool F2>
inline bool Or(T x)
{
    return F1(x) || F2(x);
}

template <uint32 N>
inline bool Is(uint32 x)
{
    return x == N;
}

inline void Run709(unit::tag_t tag)
{
    unit::assert(tag, 0 == ZeroIf<uint32, Is<13>>(13));
    unit::assert(tag, 42 == ZeroIf<uint32, Is<13>>(42));

    unit::assert(tag, 0 == ZeroIf<uint32, Or<uint32, Is<13>, Is<42>>>(13));
    unit::assert(tag, 0 == ZeroIf<uint32, Or<uint32, Is<13>, Is<42>>>(42));
    unit::assert(tag, 1 == ZeroIf<uint32, Or<uint32, Is<13>, Is<42>>>(1));
}

inline void Run710(unit::tag_t tag)
{
    uint16 x = 0;

    for (const uint5 i : 20)
    {
        x++;
    }

    unit::assert(tag, x == 20);

    x = 0;

    for (const uint5 i : 20)
    {
        x += i;
    }

    unit::assert(tag, x == 20 * (20 - 1) / 2);
}

template <auto N>
inline void Go711(unit::tag_t tag)
{
    auto x = N;
    auto count = N;

    for (const auto i : count)
    {
        static assert(bitsizeof i == bitsizeof x);
        x--;
    }

    unit::assert(tag, x == 0);
}

inline void Run711(unit::tag_t tag)
{
    Go711<1>(tag);
    Go711<7>(tag);
    Go711<8>(tag);
    Go711<0xFFF>(tag);
}

// async function calls should not be reordered past global writes
class Test713
{
private:
    uint32 g_713_val1 = 0;
    uint32 g_713_val2 = 0;
    uint32 g_validationThreadsDone713 = 0;

public:
    [[async]] void UpdateVal2_713(uint32 tid)
    {
        g_713_val2 = tid;
    }

    [[pipelined, async]] void Helper713(uint32 tid)
    {
        uint32 sum = 1;

        static for (const auto i : 16)
        {
            sum += (tid + 1);
        }

        // This write has a long depdency chain in front of it
        g_713_val1 = (sum << 16) | tid;

        // This async call does not
        UpdateVal2_713(tid);
    }

    [[pipelined, async]] void Validator713(uint32 tid, unit::tag_t tag)
    {
        uint32 val1_snapped;
        uint32 val2_snapped;

        atomic
        {
            val1_snapped = g_713_val1 & 0xffff; // only take the tid part
            val2_snapped = g_713_val2;
            g_validationThreadsDone713++;
        }

        unit::assert(tag, val1_snapped >= val2_snapped);
    }

    void run(unit::tag_t tag)
    {
        Validator713(4096, tag);
        Helper713(256);

        atomic do; while(g_713_val2 != 255);
        atomic do; while(g_validationThreadsDone713 != 4096);
    }
}

// Memory RAW hazard within 1 basic block
class Test714
{
private:
    [[memory]] uint32[512] g_mem714;

public:
    [[pipelined]] void Helper714(uint32 tid, unit::tag_t tag)
    {
        uint32 data = tid + 1;

        g_mem714[tid] = data;

        uint32 result = g_mem714[tid];

        unit::assert(tag, data == result);
    }

    void run(unit::tag_t tag)
    {
        Helper714(512, tag);
    }
}

// no part of an atomic block should be scheduled
// until all parts are done
class TestCase715
{
private:
    [[pipelined]] void Helper715(uint32 tid, unit::tag_t tag)
    {
        uint32 snapped = 0;

        uint32 mask = ~tid;

        uint32 sum = 0;

        static for (const auto i : 9)
        {
            sum += tid;
        }

        atomic
        {
            static uint32 sharedval = 0;

            snapped = sharedval;

            snapped += sum;
            snapped &= mask;

            sharedval = snapped;
        }

        uint32 expected = mux(cast<uint2>(tid), 0, 0x8, 0x18, 0x30);

        unit::assert(tag, expected == snapped);
    }

public:
    void run(unit::tag_t tag)
    {
        Helper715(4, tag);
    }
}

// atomic blocks should not be reordered past each other
class Test716
{
private:
    uint32 g_716_val1 = 0;
    uint32 g_716_val2 = 0;    
    uint32 g_validationThreadsDone716 = 0;

public:
    [[pipelined, async]] void Helper716(uint32 tid)
    {
        uint32 sum = 1;

        static for (const auto i : 16)
        {
            sum += (tid + 1);
        }

        // This atomic block has a long depdency chain in front of it
        atomic
        {
            if (sum > 0) // always true, but the compiler doesn't know this
            {
                g_716_val1++;
            }
        }

        // This atomic block does not
        atomic
        {
            g_716_val2++;
        }
    }

    [[pipelined, async]] void Validator716(uint32 tid, unit::tag_t tag)
    {
        uint32 val1_snapped;
        uint32 val2_snapped;

        atomic
        {
            val1_snapped = g_716_val1;
            val2_snapped = g_716_val2;
            g_validationThreadsDone716++;
        }

        print("{val1_snapped} {val2_snapped}\n");
        unit::assert(tag, val1_snapped >= val2_snapped);
    }

    void run(unit::tag_t tag)
    {
        Validator716(4096, tag);
        Helper716(256);

        atomic do; while(g_716_val2 != 256);
        atomic do; while(g_validationThreadsDone716 != 4096);
    }
}

// writes to globals should not be reordered past each other
class Test717
{
private:
    uint32 g_717_val1 = 0;
    uint32 g_717_val2 = 0;
    uint32 g_validationThreadsDone717 = 0;

public:
    [[pipelined, async]] void Helper717(uint32 tid)
    {
        uint32 sum = 1;

        static for (const auto i : 16)
        {
            sum += (tid + 1);
        }

        // This write has a long depdency chain in front of it
        g_717_val1 = (sum << 16) | tid;

        // This atomic block does not
        g_717_val2 = tid;
    }

    [[pipelined, async]] void Validator717(uint32 tid, unit::tag_t tag)
    {
        uint32 val1_snapped;
        uint32 val2_snapped;

        atomic
        {
            val1_snapped = g_717_val1 & 0xffff; // only take the tid part
            val2_snapped = g_717_val2;
            g_validationThreadsDone717++;
        }

        unit::assert(tag, val1_snapped >= val2_snapped);
    }

    void run(unit::tag_t tag)
    {
        Validator717(4096, tag);
        Helper717(256);

        atomic do; while(g_717_val2 != 255);
        atomic do; while(g_validationThreadsDone717 != 4096);
    }
}

// values are sign extended in predicated updates
class Test718
{
private:
    bool g_true = true;

public:
    void run(unit::tag_t tag)
    {
        int2 negative_two = -2;
        int4 negative_seven = -7;

        int8[2] values;
        values[0] = 0;
        values[1] = 1;
        if (g_true) {
            values[0] = negative_two;
            values[1] = negative_seven;
        }

        unit::assert(tag, values[0] == negative_two);
        unit::assert(tag, values[1] == negative_seven);
    }
}

class Test719
{
private:
    [[memory]] int8[8] memory_ints;

public:
    void run(unit::tag_t tag)
    {
        memory_ints[0] = 0;

        int16 s = 0;
        static for (const auto i: 65)
            s += memory_ints[i];
    }
}

inline void TestCase720(unit::tag_t tag)
{
    uint256 a = 0xFEDCBA98765432100123456789ABCDEF0123456789ABCDEFFEDCBA9876543210;
    uint32[8] b;
    static for(const auto i: 8)
    {
        b[i] = a >> (i * 32);
    }

    unit::assert(tag, b[0] == 0x76543210);
    unit::assert(tag, b[1] == 0xFEDCBA98);
    unit::assert(tag, b[2] == 0x89ABCDEF);
    unit::assert(tag, b[3] == 0x01234567);
    unit::assert(tag, b[4] == 0x89ABCDEF);
    unit::assert(tag, b[5] == 0x01234567);
    unit::assert(tag, b[6] == 0x76543210);
    unit::assert(tag, b[7] == 0xFEDCBA98);
}

class Test721
{
private:
    uint8 global_int;
    bool cond;

public:
    void run(unit::tag_t tag)
    {
        cond = (cycles() % 2 == 0) ? false : true;
        uint8 local_int;
        atomic {
            global_int = 2;
            local_int = global_int;
        }
        unit::assert(tag, local_int == 2);

        atomic {
            global_int = cond ? 3 : 5;
            local_int = global_int;
        }
        unit::assert(tag, !cond || local_int == 3);
        unit::assert(tag, cond || local_int == 5);

        global_int = 9;
        atomic {
            unit::assert(tag, global_int == 9);
            global_int = cond ? 3 : 5;
            unit::assert(tag, global_int <= 5);
            global_int += cond ? 7 : 5;
            local_int = global_int;
        }
        unit::assert(tag, global_int == 10);

        local_int = global_int;
        atomic {
            if (cond)
                if (!cond)
                    global_int = local_int + 1;
        }
        unit::assert(tag, local_int == global_int);
    }
}

inline void empty_function()   {}

inline void unreachable_function_with_startConditionOperation()
{
    empty_function();
    atomic do; while(cycles() < 11);
}

// This test is intentionally not invoked.
inline void TestCase722(unit::tag_t tag)
{
    unreachable_function_with_startConditionOperation();
}

// array write with signed index
class Test723
{
private:
    uint32[7] g_data723;

public:
    void Helper723(int32 idx, uint32 data)
    {
        g_data723[idx] = data;
    }

    void run(unit::tag_t tag)
    {
        static for (const auto i : 7)
        {
            g_data723[i] = i;
        }

        for (const auto i : 9)
        {
            int32 idx = -1 - i;
            Helper723(idx, 100); // all of these writes should be dropped

            static for (const auto i : 7)
            {
                unit::assert(tag, g_data723[i] == i);
            }
        }

        for (const uint32 idx : 7)
        {
            Helper723(idx, idx + 3);
        }

        static for (const auto i : 7)
        {
            unit::assert(tag, g_data723[i] == (i + 3));
        }
    }
}

// memory read with signed index
class Test724
{
private:
    [[memory]] uint32[512] g_mem724;

public:
    uint32 Helper724(int32 idx)
    {
        return g_mem724[idx];
    }

    void run(unit::tag_t tag)
    {
        for (const auto i : 512)
        {
            g_mem724[i] = i;
        }

        for (const auto i : 512)
        {
            int32 idx = -i;

            uint32 actual = Helper724(idx);
            uint32 expected = (512 - i) % 512;

            print("{i} {idx} {actual} {expected}\n");
            unit::assert(tag, actual == expected);
        }
    }
}

// memory write with signed index
class Test725
{
private:
    [[memory]] uint32[512] g_mem725;

public:
    void Helper725(int32 idx, uint32 val)
    {
        g_mem725[idx] = val;
    }

    void run(unit::tag_t tag)
    {
        for (const auto i : 512)
        {
            g_mem725[i] = i;
        }

        Helper725(-1, 24);

        unit::assert(tag, g_mem725[511] == 24);
    }
}

inline void test_main()
{
    unit::test<709>(Run709);
    unit::test<710>(Run710);
    unit::test<711>(Run711);
    unit::test<713>(unit::fixture<Test713>());
    unit::test<714>(unit::fixture<Test714>());
    unit::test<715>(unit::fixture<TestCase715>());
    unit::test<716>(unit::fixture<Test716>());
    unit::test<717>(unit::fixture<Test717>());
    unit::test<718>(unit::fixture<Test718>());
    unit::test<719>(unit::fixture<Test719>());
    unit::test<720>(TestCase720);
    unit::test<721>(unit::fixture<Test721>());
    unit::test<723>(unit::fixture<Test723>());
    unit::test<724>(unit::fixture<Test724>());
    unit::test<725>(unit::fixture<Test725>());
}
