// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

class Test757
{
private:
    [[memory]] uint32[2] g_mem757 = {0, 1};

public:
    [[pipelined]] void Helper757(uint32 tid)
    {
        uint32 idx = tid % 2;

        atomic
        {
            // This mem will have bypass logic
            // This test checks that initialized memory via
            // {initialization list} is correctly read from before
            // the first writes to the mem
            uint32 val = g_mem757[idx];
            val++;
            g_mem757[idx] = val;
        }
    }

    [[pipelined]] void Validate757(uint32 tid, uint32 totalThreadCount, unit::tag_t tag)
    {
        uint32 actual = g_mem757[tid];
        uint32 expected = tid + totalThreadCount;
        unit::assert(tag, actual == expected);
    }

    void run(unit::tag_t tag)
    {
        Helper757(4);

        Validate757(2, 4/2, tag);
    }
}

// Memory read in atomic block
// with address coming from a global
class Test758
{
private:
    [[memory]] uint32[512] Mem758 = { 5, 6, 7, 8 };
    uint9 Addr758 = 0;

public:
    [[pipelined]] void Helper758(uint32 tid, unit::tag_t tag)
    {
        uint32 val;

        atomic
        {
            val = Mem758[Addr758];

            Addr758++;
        }

        unit::assert(tag, val == (tid + 5));
    }

    void run(unit::tag_t tag)
    {
        Helper758(4, tag);
    }
}

// Memory write in atomic block
// with address coming from a global
class Test759
{
private:
    [[memory]] uint32[512] Mem759;
    uint9 Addr759 = 0;

public:
    [[pipelined]] void Helper759(uint32 tid, unit::tag_t tag)
    {
        atomic
        {
            Mem759[Addr759] = tid + 2;

            Addr759++;
        }

        uint32 actual = Mem759[tid];

        unit::assert(tag, actual == (tid + 2));
    }

    void run(unit::tag_t tag)
    {
        Helper759(16, tag);
    }
}

// Memory read and write in atomic block
// with address coming from a global
class Test760
{
private:
    [[memory]] uint32[512] Mem760;

public:
    [[pipelined]] void Helper760(uint32 tid, unit::tag_t tag)
    {
        bool push = tid < 16;
        uint32 actual = 0;

        atomic
        {
            static uint9 ReadAddr = -1;
            static uint9 WriteAddr = 0;

            actual = Mem760[ReadAddr];

            Mem760[WriteAddr] = push ? tid : 0;

            ReadAddr = push ? ReadAddr + 1 : ReadAddr - 1;
            WriteAddr = push ? WriteAddr + 1 : WriteAddr - 1;
        }

        if (!push)
        {
            uint32 expected = 31 - tid;
            unit::assert(tag, actual == expected);
        }
    }

    void run(unit::tag_t tag)
    {
        Helper760(32, tag);
    }
}

// Memory write and write in atomic block
// with address coming from a global
// and with a local predicate
class Test761
{
private:
    [[memory]] uint32[512] Mem761;

public:
    [[pipelined]] void Helper761(uint32 tid, unit::tag_t tag)
    {
        bool push = tid < 16;
        uint32 actual = 0;

        atomic
        {
            static uint9 ReadAddr = -1;
            static uint9 WriteAddr = 0;

            actual = Mem761[ReadAddr];

            if (push)
            {
                Mem761[WriteAddr] = tid;
            }

            ReadAddr = push ? ReadAddr + 1 : ReadAddr - 1;
            WriteAddr = push ? WriteAddr + 1 : WriteAddr - 1;
        }

        if (!push)
        {
            uint32 expected = 31 - tid;
            unit::assert(tag, actual == expected);
        }
    }

    void run(unit::tag_t tag)
    {
        Helper761(32, tag);
    }
}

// Memory write and write in atomic block
// with address coming from a global
// and with a local predicate (for read and write)
class Test762
{
private:
    [[memory, non_replicated]] uint32[512] Mem762;

public:
    [[pipelined]] void Helper762(uint32 tid, unit::tag_t tag)
    {
        bool push = tid < 16;
        uint32 actual = 0;

        atomic
        {
            static uint9 ReadAddr = -1;
            static uint9 WriteAddr = 0;

            if (!push)
            {
                actual = Mem762[ReadAddr];
            }

            if (push)
            {
                Mem762[WriteAddr] = tid;
            }

            ReadAddr = push ? ReadAddr + 1 : ReadAddr - 1;
            WriteAddr = push ? WriteAddr + 1 : WriteAddr - 1;
        }

        if (!push)
        {
            uint32 expected = 31 - tid;
            unit::assert(tag, actual == expected);
        }
    }

    void run(unit::tag_t tag)
    {
        Helper762(32, tag);
    }
}

// Memory write and write in atomic block
// with address coming from a global
// and with a global predicate (for read and write)
class Test763
{
private:
    [[memory]] uint32[512] Mem763;
    bool Predicate763 = false;

public:
    [[pipelined]] void Helper763(uint32 tid, unit::tag_t tag)
    {
        bool push = tid < 16;
        uint32 actual = 0;

        atomic
        {
            static uint9 ReadAddr = -1;
            static uint9 WriteAddr = 0;

            if (!Predicate763)
            {
                actual = Mem763[ReadAddr];
            }

            if (Predicate763)
            {
                Mem763[WriteAddr] = tid;
            }

            ReadAddr = push ? ReadAddr + 1 : ReadAddr - 1;
            WriteAddr = push ? WriteAddr + 1 : WriteAddr - 1;

            Predicate763 = !Predicate763;
        }

        if (!push)
        {
            if (0 == (tid % 2))
            {
                uint32 expected = 31 - tid;
                unit::assert(tag, actual == expected);
            }
        }
    }

    void run(unit::tag_t tag)
    {
        Helper763(32, tag);
    }
}

// Add two numbers and check the sum
template <typename Tin, typename Tout>
inline void Helper764a(Tin a, Tin b, Tout expectedSum, unit::tag_t tag)
{
    Tout sum = a + b;
    unit::assert(tag, sum == expectedSum);
}

// Increment - test using literals
template <typename Tin, typename Tout, bool pre, bool dec>
inline void Helper764b(Tin a, Tout expectedSum, unit::tag_t tag)
{
    Tout sum;
    if (pre)
    {
        if (dec)
        {
            sum = -1 + a;
        }
        else
        {
            sum = 1 + a;
        }
    }
    else
    {
        if (dec)
        {
            sum = a - 1;
        }
        else
        {
            sum = a + 1;
        }
    }
    unit::assert(tag, sum == expectedSum);
}

template <typename Tin, typename Tout>
inline void Helper764c(Tin a, Tout expectedProd, unit::tag_t tag)
{
    Tout prod = 0xffffffff * a;
    unit::assert(tag, prod == expectedProd);
}

// Test DecomposeAdd
inline void Test764(unit::tag_t tag)
{
    // Unsigned tests - test odd and even bit widths, test multiple passes of decompose
    // Small add - no decompose
    Helper764a<uint2, uint3>(0x1, 0x1, 0x2, tag);
    Helper764a<uint2, uint3>(0x2, 0x2, 0x4, tag);
    // No carry
    Helper764a<uint64, uint65>(0x1234567822222222, 0x1111111133333333, 0x2345678955555555, tag);
    // Carry
    Helper764a<uint64, uint65>(0xffffffffffffffff, 0x0000000000000001, 0x10000000000000000, tag);
    // Odd bit widths
    Helper764a<uint127, uint128>(0x11111111222222223333333344444444, 0x111111111222222223333333344444444, 0x22222222444444446666666688888888, tag);
    Helper764a<uint127, uint128>(0x7fffffffffffffffffffffffffffffff, 0x1, 0x80000000000000000000000000000000, tag);

    // Signed tests
    Helper764a<int64, int65>(0x4000000000000000, -1, 0x3fffffffffffffff, tag);
    Helper764a<int64, int65>(-1, 0x4000000000000000, 0x3fffffffffffffff, tag);

    // Test literals
    Helper764b<uint64, uint65, false, false>(1, 2, tag);
    Helper764b<uint64, uint65, false, false>(0xffffffffffffffff, 0x10000000000000000, tag);
    Helper764b<uint64, uint65, true, false>(1, 2, tag);
    Helper764b<uint64, uint65, true, false>(0xffffffffffffffff, 0x10000000000000000, tag);
    Helper764b<uint64, uint65, false, true>(2, 1, tag);
    Helper764b<uint64, uint65, false, true>(0x4000000000000000, 0x3fffffffffffffff, tag);
    Helper764b<uint64, uint65, true, true>(2, 1, tag);
    Helper764b<uint64, uint65, true, true>(0x4000000000000000, 0x3fffffffffffffff, tag);

    // Test multiplying by a constant which creates an adder tree where output bit width != input bit width + 1
    Helper764c<uint2, uint34>(1, 0xffffffff, tag);
    Helper764c<uint2, uint34>(3, 0x2fffffffd, tag);
}

inline void test_main()
{
    unit::test<757>(unit::fixture<Test757>());
    unit::test<758>(unit::fixture<Test758>());
    unit::test<759>(unit::fixture<Test759>());
    unit::test<760>(unit::fixture<Test760>());
    unit::test<761>(unit::fixture<Test761>());
    unit::test<762>(unit::fixture<Test762>());
    unit::test<763>(unit::fixture<Test763>());
    unit::test<764>(Test764);
}
