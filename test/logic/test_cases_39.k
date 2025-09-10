// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Read-modify-write memory inside of atomic
class Test689
{
private:
    [[memory]] uint32[8] g_mem689;

public:
    [[pipelined]] void Helper689(uint32 tid)
    {
        uint32 slot = tid % 8;

        atomic
        {
            uint32 val = g_mem689[slot];

            val++;

            g_mem689[slot] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        for (const uint32 i : 8)
        {
            g_mem689[i] = i;
        }

        Helper689(128);

        for (const uint32 i : 8)
        {
            uint32 expected = (i + 16);
            uint32 actual = g_mem689[i];

            unit::assert(tag, actual == expected);
        }
    }
}

// loads and stores inside atomic blocks (without read-modify write)
class Test690
{
private:
    [[memory]] uint32[512] g_mem690;

public:
    [[pipelined]] void Write690(uint32 tid)
    {
        atomic
        {
            g_mem690[tid] = tid + 3;
        }
    }

    [[pipelined]] void Read690(uint32 tid, unit::tag_t tag)
    {
        atomic
        {
            uint32 actual = g_mem690[tid];
            uint32 expected = tid + 3;
            unit::assert(tag, actual == expected);
        }
    }

    void run(unit::tag_t tag)
    {
        Write690(512);

        Read690(512, tag);
    }
}

// atomic block that does a read-modify-write to 1 memory
// and just a write to another
class Test691
{
private:
    [[memory]] uint32[8] g_rmwMem691;
    [[memory]] uint32[128] g_writeMem691;

public:
    [[pipelined]] void Helper691(uint32 tid)
    {
        uint32 slot = tid % 8;

        atomic
        {
            uint32 val = g_rmwMem691[slot];

            val++;

            uint32 val2 = tid + 2;

            g_rmwMem691[slot] = val;

            g_writeMem691[tid] = val2;
        }
    }

    void run(unit::tag_t tag)
    {
        for (const uint32 i : 8)
        {
            g_rmwMem691[i] = i;
        }

        Helper691(128);

        for (const uint32 i : 8)
        {
            uint32 expected = (i + 16);
            uint32 actual = g_rmwMem691[i];
            unit::assert(tag, actual == expected);
        }

        for (const uint32 i : 128)
        {
            uint32 expected = (i + 2);
            uint32 actual = g_writeMem691[i];
            unit::assert(tag, actual == expected);
        }
    }
}

// atomic block that does a read-modify-write to 1 memory
// store address is computed inside of the atomic block
class Test692
{
private:
    [[memory]] uint32[8] g_rmwMem692;

public:
    [[pipelined]] void Helper692(uint32 tid)
    {
        uint32 readSlot = tid % 8;

        atomic
        {
            uint32 val = g_rmwMem692[readSlot];

            val++;

            uint32 writeSlot = (tid + 1) % 8;

            g_rmwMem692[writeSlot] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        for (const uint32 i : 8)
        {
            g_rmwMem692[i] = 0;
        }

        Helper692(128);

        for (const uint32 i : 8)
        {
            uint32 expected = (i == 0) ? 128 : (120 + i);
            uint32 actual = g_rmwMem692[i];
            unit::assert(tag, actual == expected);
        }
    }
}

inline void Helper693(uint16 rhs, unit::tag_t tag)
{
    int16 imm16_signed = rhs;
    int32 imm16_sign_extended_old = cast<int32>(imm16_signed);
    int32 imm16_sign_extended_new = cast<int32>(cast<int16>(rhs));
    print("i_type_instr.imm16 = {rhs}\n");
    print("imm16_sign_extended_old = {imm16_sign_extended_old}\n");
    print("imm16_sign_extended_new = {imm16_sign_extended_new}\n");
    unit::assert(tag, imm16_sign_extended_old == imm16_sign_extended_new);
}

inline void TestCase693(unit::tag_t tag)
{
    Helper693(0xfffc, tag);
}

inline void test_main()
{
    unit::test<689>(unit::fixture<Test689>());
    unit::test<690>(unit::fixture<Test690>());
    unit::test<691>(unit::fixture<Test691>());
    unit::test<692>(unit::fixture<Test692>());
    unit::test<693>(TestCase693);
}
