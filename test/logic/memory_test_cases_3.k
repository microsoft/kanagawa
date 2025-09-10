// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.array
import data.optional
import data.random.lfsr

import test.unit as unit
import test.runner

class Test513
{
private:
    // atomic read-modify-write to memory - in a predicate
    [[memory]] uint32[32] mem513;

    struct ThreadId513
    {
        uint4 IterationIndex;
        uint5 SlotIndex;
        uint23 Padding;
    }

public:
    [[pipelined]] void Foo513(uint32 threadId)
    {
        ThreadId513 decomposedThreadId = cast<ThreadId513>(threadId);

        uint5 slotIndex = decomposedThreadId.SlotIndex;
        bool isFirst = (decomposedThreadId.IterationIndex == 0);

        bool predicate = (0 == (decomposedThreadId.IterationIndex & 1));

        if (predicate)
        {
            atomic
            {
                auto val = mem513[slotIndex];
                val = isFirst ? (slotIndex + 1) : (val + 1);
                mem513[slotIndex] = val;
            }
        }
    }

    void run(unit::tag_t tag)
    {
        const auto NumIterations = 16;

        Foo513(NumIterations * 32);

        for (const uint32 i : 32)
        {
            uint32 val = mem513[i];

            uint32 expected = (i + 8);

            unit::assert(tag, val == expected);
        }
    }
}



// Two memories read in an atomic block
// The same address is used for both loads
// and that address is a function of shared variables
// A single global view is created that is used twice
class GlobalViewDuplicateTestHelper
{
private:
    memory<uint32, 32> _memA;
    memory<uint32, 32> _memB;
    memory<uint32, 32> _memC;

public:
    void test(unit::tag_t tag)
    {
        pipelined_for(32, [](uint32 id)
        {
            _memA[id] = id + 4;
            _memB[id] = id + 7;
            _memC[id] = id + 9;
        });

        // Code that will cause a global view for _mem to be created
        pipelined_for(30, [](uint32 id)
        {
            atomic
            {
                static uint32 _addr = 0;

                // A global view for the expression: "_addr + 2"
                // will be created
                // The buggy compiler would have have caused the address
                // used to compute resultB to use the global view for resultC
                uint32 resultC = _memC[_addr + 2];

                _addr++;
            }
        });

        pipelined_for(30, [tag](uint32 id)
        {
            atomic
            {
                static uint32 _addr = 0;

                uint5 addr = _addr + 1;

                uint32 resultA = _memA[addr];
                uint32 resultB = _memB[addr];

                unit::assert(tag, resultA == (id + 5));
                unit::assert(tag, resultB == (id + 8));

                _addr++;
            }
        });
    }
}

inline void GlobalViewDuplicateTest(unit::tag_t tag)
{
    static GlobalViewDuplicateTestHelper helper;
    helper.test(tag);
}

template<auto depth>
class NonPowerOfTwoMemoryHelper
{
private:
    memory<uint32, depth> _mem;
    const auto depthPowTwo = (1 << bitsizeof(depth));

public:
    void TestWriteReadVerify(unit::tag_t tag)
    {
        pipelined_for(depthPowTwo, [](index_t<depthPowTwo> tid)
        {
            _mem[tid] = tid;
        });

        pipelined_for(depth, [tag](index_t<depth> tid)
        {
            uint32 value = _mem[tid];

            unit::assert(tag, value == tid);
        });
    }
}

inline void NonPowerOfTwoMemoryTest(unit::tag_t tag)
{
    static NonPowerOfTwoMemoryHelper<24> helper;

    helper.TestWriteReadVerify(tag);
}

// Memory load, address is a pure function of shared variables
// predicate is a function of shared variables + local variables
inline void SharedAddressLocalPredicate(unit::tag_t tag)
{
    class Helper
    {
    private:
        memory<uint32, 32> _mem;

    public:
        void test(unit::tag_t tag)
        {
            pipelined_for(32, [](uint32 id)
            {
                _mem[id] = id + 4;
            });

            // Code that will cause a global view for _mem to be created
            pipelined_for(30, [tag](uint32 id)
            {
                uint32 result = 0;

                atomic
                {
                    static bool _b = true;
                    static uint32 _addr = 0;

                    if (_b && ((0 == (id % 2))))
                    {
                        result = _mem[_addr + 2];
                    }

                    _b = !_b;
                    _addr++;
                }

                uint32 expected = (0 == (id % 2)) ? (id + 6) : 0;

                unit::assert(tag, result == expected);
            });
        }
    }

    static Helper helper;
    helper.test(tag);
}

class EccTest
{
private:
    static if (ECC_SUPPORT)
    {
        memory_ecc<uint64, 512> _eccMem;
    }

public:
    void run(unit::tag_t tag)
    {
        static if (ECC_SUPPORT)
        {
            pipelined_for(512, [](index_t<512> i)
            {
                _eccMem[i] = i + 5;
            });

            pipelined_for(512, [tag](index_t<512> i)
            {
                ecc<uint64> e = _eccMem[i];

                unit::assert(tag, !e.error);
                unit::assert(tag, e.data.is_valid);
                unit::assert(tag, e.data.value == (i + 5));
            });
        }
        else
        {
            print("EccTest skipping because underlying platform doesn't support it\n");
        }
    }
}

// This test relies on a SystemVerilog assert that fire if writes to initialized memories
// could be written to between power-up and reset
inline void InitialValuePowerOnTest(unit::tag_t tag)
{
    {
        static memory<uint32, 4> _mem1 = { 1, 2, 3, 4 };

        for (const auto i : 4)
        {
            const uint32 value = _mem1[i];
            unit::assert(tag, value == (i + 1));

            // Ensure the memory is not converted to a rom
            _mem1[i] = i + 4;
        }
    }

    {
        static memory<uint32, 4> _mem2 = { 1, 2, 3, 4 };

        for (const auto i : 4)
        {
            // atomic read-modify-write to ensure write-delay is added
            // the write-delay registers must have power-on values
            atomic
            {
                const uint32 value = _mem2[i];
                unit::assert(tag, value == (i + 1));

                _mem2[i] = i + 4;
            }
        }
    }
}

// Shared variable is written twice an atomic block
// 1) assigned to a local variable
// 2) output of a memory read (where the shared variable is also the address)
template<
    template <typename, auto> typename Container
    >
inline void PointerChase(unit::tag_t tag)
{
    class Helper
    {
    private:
        Container<uint32, 8> _mem = { 3, 2, 0, 5, 1, 6, 7, 0 };

    public:
        void Test(unit::tag_t tag)
        {
            pipelined_for(8, [tag](index_t<8> tid)
            {
                bool is_first = (tid == 0);

                uint32 snapped;

                atomic
                {
                    static index_t<8> _addr;

                    uint32 next_addr;

                    if (is_first)
                    {
                        snapped = 1234;
                        next_addr = _mem[4];
                    }
                    else
                    {
                        snapped = _addr;
                        next_addr = _mem[_addr];
                    }

                    _addr = next_addr;
                }

                uint32 expected = mux(tid, 1234, 1, 2, 0, 3, 5, 6, 7);

                unit::assert(tag, snapped == expected);
            });
        }
    }

    static Helper _helper;
    _helper.Test(tag);
}

inline void PointerChase_array(unit::tag_t tag)        { PointerChase<array>(tag); }
inline void PointerChase_memory(unit::tag_t tag)       { PointerChase<memory>(tag); }
inline void PointerChase_memory_norep(unit::tag_t tag) { PointerChase<memory_norep>(tag); }


// Concurrent read/write of the same memory address
// should never result in a 'x in RTL simulation
inline void ConcurrentReadWriteSameAddress(unit::tag_t tag)
{
    class Helper
    {
    private:
        memory<uint32, 512> _mem = {};

        const auto NumIterations = 1024;

        bool[2] _finished = {};

    public:
        void Test(unit::tag_t tag)
        {
            // writer
            async_exec([]()
            {
                pipelined_for (NumIterations, [](index_t<NumIterations> tid)
                {
                    static lfsr_random<32> rng;

                    // Only use a few low bits of the random number
                    // to ensure frequeny collisions
                    index_t<8> addr = cast<index_t<8>>(rng.next(make_optional<uint32>(tid == 0, 3)));

                    _mem[addr] = 0;
                });

                _finished[0] = true;
            });

            // reader
            async_exec([tag]()
            {
                pipelined_for (NumIterations, [tag](index_t<NumIterations> tid)
                {
                    static lfsr_random<32> rng;

                    // Initialize with a different seed than the writer
                    index_t<8> addr = cast<index_t<8>>(rng.next(make_optional<uint32>(tid == 0, 2)));

                    uint32 result = _mem[addr];

                    unit::assert(tag, result == 0);

                    // Work-around the compiler transformation the == operation
                    // in a way that cause (a == 'x) to evaluate to true
                    bool b = reinterpret_cast<bool>(result);
                    unit::assert(tag, !b);
                });

                _finished[1] = true;
            });

            atomic do; while(!(and(_finished)));
        }
    }

    static Helper _helper;
    _helper.Test(tag);
}

// Comparison inside of logic that compute a global view
class GlobalViewComparisonHelper
{
private:
    memory<uint32, 32> _mem;

public:
    void test(unit::tag_t tag)
    {
        pipelined_for(32, [](index_t<32> id)
        {
            _mem[id] = id + 4;
        });

        pipelined_for(32, [tag](index_t<32> id)
        {
            uint32 result;

            atomic
            {
                static uint32 _addr = 0;

                // It is important that the comparison operands have different widths
                // to verify a compiler transformation that handles different widths
                result = _mem[_addr + cast<uint1>(_addr > 10)];

                _addr++;
            }

            uint32 expected = mux(id, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x4);

            unit::assert(tag, result == expected);
        });
    }
}

inline void GlobalViewComparison(unit::tag_t tag)
{
    static GlobalViewComparisonHelper helper;
    helper.test(tag);
}

inline void test_main()
{
    print("Test 513\n");
    unit::test<1>(unit::fixture<Test513>());

    print("GlobalViewDuplicateTest\n");
    unit::test<2>(GlobalViewDuplicateTest);

    print("NonPowerOfTwoMemoryTest\n");
    unit::test<3>(NonPowerOfTwoMemoryTest);

    print("SharedAddressLocalPredicate\n");
    unit::test<4>(SharedAddressLocalPredicate);

    print("EccTest\n");
    unit::test<5>(unit::fixture<EccTest>());

    print("InitialValuePowerOnTest\n");
    unit::test<6>(InitialValuePowerOnTest);

    print("PointerChase array\n");
    unit::test<7>(PointerChase_array);

    print("PointerChase memory\n");
    unit::test<8>(PointerChase_memory);

    print("PointerChase memory_norep\n");
    unit::test<9>(PointerChase_memory_norep);

    print("ConcurrentReadWriteSameAddress\n");
    unit::test<10>(ConcurrentReadWriteSameAddress);

    print("GlobalViewComparison\n");
    unit::test<11>(GlobalViewComparison);
}
