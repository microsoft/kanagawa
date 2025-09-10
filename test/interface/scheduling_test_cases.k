// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.memory.byte_addressable
import test.unit as unit
import test.runner

// Unrelated atomic chains - there should be no constraints between unrelated atomic blocks
class UnrelatedAtomicChainTest
{
private:
    memory<uint<32>, 512>[2] m_a;
    memory<uint<32>, 512>[2] m_c;

public:
    void UnrelatedAtomicChainHelper(uint8 addr)
    {
        static for(const auto i : 2)
        {
            uint<33> c_ = m_a[i][addr] + 1;

            atomic
            {
                m_c[i][addr] = c_;
            }
        }
    }

    void run(unit::tag_t tag)
    {
        for (const uint8 i : 10)
        {
            UnrelatedAtomicChainHelper(i);
        }
    }
}

// Global written in an atomic block once (no predication)
// Read before write
inline void RewriteGlobalAtomic_OneWrite_NoPred(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint5 idx)
    {
        uint32 snapped;

        atomic
        {
            static uint32 _shared = 0;

            snapped = _shared;

            _shared = idx;
        }

        uint32 expected = (idx == 0) ? 0 : idx - 1;

        unit::assert_equal(tag, expected, snapped);
    });
}

// Global written in an atomic block once (predicated)
// Read before write
inline void RewriteGlobalAtomic_OneWrite_Pred(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint5 idx)
    {
        uint32 snapped;

        bool pred = (0 == (idx % 2));

        atomic
        {
            static uint32 _shared = 0;

            snapped = _shared;

            if (pred)
            {
                _shared = idx;
            }
        }

        uint32 expected = (idx == 0) ? 0 : (idx - 1) & 0xffffffe;

        unit::assert_equal(tag, expected, snapped);
    });
}

// Global written in an atomic block once (predicated)
// Read after write
inline void RewriteGlobalAtomic_OneWrite_Pred_RAW(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint5 idx)
    {
        uint32 snapped;

        bool pred = (0 == (idx % 2));

        atomic
        {
            static uint32 _shared = 0;

            if (pred)
            {
                _shared = idx;
            }

            snapped = _shared;
        }

        uint32 expected = idx & 0xffffffe;

        unit::assert_equal(tag, expected, snapped);
    });
}

// Global written in an atomic block once (no predication)
// Read before write
inline void RewriteGlobalAtomic_TwoWrite_NoPred(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint5 idx)
    {
        uint32 snapped;

        atomic
        {
            static uint32 _shared = 0;

            snapped = _shared;

            _shared = idx;

            _shared = idx + 1;
        }

        uint32 expected = (idx == 0) ? 0 : idx;

        unit::assert_equal(tag, expected, snapped);
    });
}

// Global written in an atomic block twice (predicated)
// Read after write
inline void RewriteGlobalAtomic_TwoWrite_Pred_RAW(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint5 idx)
    {
        uint32 snapped;

        bool pred = (0 == (idx % 2));

        atomic
        {
            static uint32 _shared = 0;

            _shared = idx;

            if (pred)
            {
                _shared = idx + 1;
            }

            snapped = _shared;
        }

        uint32 expected = idx | 1;

        unit::assert_equal(tag, expected, snapped);
    });
}

// Memory address computed in a [[schedule()]] block
inline void MemAddrInScheduleBlock(unit::tag_t tag)
{
    class Helper
    {
    private:
        memory<uint32, 512> _mem1;
        memory<uint32, 512> _mem2;

    public:
        template<auto num_stages, auto wasted_iterations>
        void Test(unit::tag_t tag, uint32 offset)
        {
            // Offset is always 0 (but the compiler doesn't know that)
            pipelined_for(32, [](index_t<32> x)
            {
                _mem1[x] = x + 2;
                _mem2[x] = x + 5;
            });

            pipelined_for(512, [offset](index_t<512> x)
            {
                [[schedule(num_stages)]]
                {
                    index_t<32> addr = _mem1[x % 32] - 2;

                    // Used to test that register ratio can be ignored if it makes scheduling impossible
                    static for (const auto i : wasted_iterations)
                    {
                        addr += offset;
                    }

                    uint32 value = _mem2[addr];

                    value++;

                    _mem2[addr] = value;
                }
            });

            pipelined_for(32, [tag](index_t<32> x)
            {
                uint32 expected = x + 16 + 5;
                uint32 actual = _mem2[x];

                unit::assert_equal(tag, expected, actual);
            });
        }
    }

    static Helper helper;

    // Note that .Test<3, ...> cases are not covered
    // Because the compiler transforms schedule(3) blocks with bypass
    // into schedule(1) blocks without bypass
    helper.Test<2, 0>(tag, 0);
    helper.Test<4, 0>(tag, 0);
    helper.Test<5, 0>(tag, 0);

    // Testing that register-ratio can be ignored
    helper.Test<2, 32>(tag, 0);
    helper.Test<4, 32>(tag, 0);
    helper.Test<5, 32>(tag, 0);
}

inline void DspInAtomic(unit::tag_t tag)
{
    pipelined_for(128, [tag](index_t<128> tid)
    {
        uint16 x = tid + 4;
        uint16 y = tid + 2;

        uint32 expected = x * y;
        uint32 actual;

        [[schedule(6)]]
        {
            actual = x * y;
        }

        unit::assert_equal(tag, expected, actual);
    });
}

// Memory load address (in atomic block) is a function of global variables
// Compiler will compute that function where the global variables are written
inline void AddressFunctionOfGlobal(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static uint9 _idx;
        static memory<uint32, 512> _mem = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        uint32 actual;

        atomic
        {
            actual = _mem[_idx + 2];
            _idx = id;
        }

        uint32 expected = id + 1;

        if (id > 0)
        {
            // The value read for the first thread is undefined
            unit::assert_equal(tag, expected, actual);
        }
    });
}

// Memory load predicate (in atomic block) is a function of global variables
// Compiler will compute that function where the global variables are written
inline void MemoryPredicateFunctionOfGlobal(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static bool _skip_load;
        static memory_norep<uint32, 512> _mem = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        uint32 actual = 55;

        atomic
        {
            if (!_skip_load)
            {
                actual = _mem[id];
            }

            _skip_load = (0 == (id % 2));
        }

        uint32 expected = (0 == ((id - 1) % 2)) ? 55 : id;

        if (id > 0)
        {
            // The value read for the first thread is undefined
            unit::assert_equal(tag, expected, actual);
        }
    });
}

inline void RmwTwoMemoriesOneWithBypassOneWithout(unit::tag_t tag)
{
    class Helper
    {
    private:
        memory<uint32, 32> _mem1;
        memory<uint32, 32> _mem2;

    public:
        void Test(unit::tag_t tag)
        {
            pipelined_for(32, [](index_t<32> x)
            {
                _mem1[x] = x;
                _mem2[x] = x + 1;
            });

            pipelined_for(32, [](index_t<32> x)
            {
                atomic
                {
                    auto val1 = _mem1[x];
                    auto val2 = _mem2[x];

                    _mem1[x] = val1 + val2;
                }
            });

            pipelined_for(32, [tag](index_t<32> x)
            {
                uint32 expected = 2 * x + 1;
                uint32 actual = _mem1[x];

                unit::assert_equal(tag, expected, actual);
            });
        }
    }

    static Helper helper;

    helper.Test(tag);
}


// Memory load address (in atomic block) is a function of multiple global variables
inline void AddressFunctionOfMulipleGlobals(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static uint9 _idx0;
        static uint9 _idx1;

        static memory<uint32, 512> _mem = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19};
        uint32 actual;

        atomic
        {
            actual = _mem[_idx0 + _idx1];
            _idx0 = id + 2;
            _idx1 = id + 1;
        }

        // +3 for the 2 adds in the atomic, +1 because all memory contents are offset by 1
        uint32 expected = (id - 1) * 2 + 3 + 1;

        if (id > 0)
        {
            // The value read for the first thread is undefined
            unit::assert_equal(tag, expected, actual);
        }
    });
}


// Memory load address (in atomic block) is a function of a global variable
// The writes to that global are predicated
inline void AddressFunctionOfGlobalWithPredicatedGlobal(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static uint9 _idx;
        static memory<uint32, 512> _mem = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19};
        uint32 actual;
        bool impossible = id > 8;

        atomic
        {
            actual = _mem[_idx + 1];

            if (0 == id)
            {
                _idx = 0;
            }
            else if (0 == (id % 2))
            {
                _idx++;
            }
            else if (impossible)
            {
                // This case will never occur, but compiler doesn't know that
                // To ensure that the updates to the synthetic global variable that contains the address
                // is never updated with this impossible value
                _idx--;
            }
        }

        if (id > 0)
        {
            uint32 expected = 1 + ((id + 1) / 2);

            unit::assert_equal(tag, expected, actual);
        }
    });
}

// Memory load address (in atomic block) is a function of a global variable
// that has an initial value
inline void AddressFunctionOfGlobalWithInitialValue(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static uint9 _idx = 2;
        static memory<uint32, 512> _mem = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
        uint32 actual;

        atomic
        {
            actual = _mem[_idx + 2] + 1;
            _idx++;
        }

        uint32 expected = id + 5;

        unit::assert_equal(tag, expected, actual);
    });
}

// Memory load address (in atomic block) is a function of a global variable
// that has an initial value
inline void AddressFunctionOfGlobalUpdatedInSeperateAtomic(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static uint9 _idx = 1;
        static memory<uint32, 512> _mem = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
        uint32 actual;

        uint32 snappedIndex;

        atomic
        {
            snappedIndex = _idx;
            actual = _mem[_idx + 2] + 1;
        }

        // ensure _idx is updated in a separate stage
        barrier;

        atomic
        {
            _idx++;
        }

        uint32 expected = snappedIndex + 3;

        unit::assert_equal(tag, expected, actual);
    });
}

// byte_addressable memory load address (in atomic block) is a function of a global variable
inline void ByteAddressableMemStaticLoadAddr(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static byte_addressable_memory<uint32, 512> _mem;
        uint32 actual;

        atomic
        {
            static uint32 _load_addr;

            auto word = _mem.read(_load_addr);
            actual = word;

            word = id * 2;
            _mem.write(id, word, 4);

            _load_addr = (id / 2) % 4;
        }

        if (id > 0)
        {
            const uint32[8] expected_array =
            {
                0x00000000,
                0x00000200,
                0x00000402,
                0x00060402,
                0x00080604,
                0x0a080604,
                0x0c0a0806
            };

            uint32 expected = expected_array[id - 1];

            unit::assert_equal(tag, expected, actual);
        }
    });
}

// Test reordering of accesses to globals works correctly for write-after-read
// No atomic blocks
// This case is interesting for a case like a fifo
// which has 2 operations:
// 1 read data from memory
// 2 increment semaphore allowing that data to be overwritten
// The read and write must not be reordered
class TestGlobalWarHazardHelper
{
public:
    uint32 _x = 0;
    uint32 _y = 0;

public:
    void WriteAfterRead(unit::tag_t tag)
    {
        pipelined_for(1024, [tag](uint32 tid)
        {
            // read
            uint32 snappedX = _x;

            // write
            _y = tid;

            // Incentivize the compiler to delay the read of _x
            // By adding a long pipeline unrelated to snappedX
            uint32 temp = tid;

            static for (const auto i : 64)
            {
                temp = ~temp + 1;
            }

            unit::assert_equal(tag, tid, temp);

            // Ensure the assertion related to snappedX is long after the read
            // of _x
            barrier;

            // _x should always be less than or equal to tid
            // because the background thread only ever write a value N to _x
            // after _y is >= N
            unit::assert(tag, snappedX <= tid);
        });
    }

    void BackgroundThread()
    {
        async_exec([]()
        {
            pipelined_for(1024, [](uint32 tid)
            {
                atomic do; while(_y < tid);

                atomic
                {
                    _x = tid;
                }
            });
        });
    }
}

inline void TestGlobalWarHazard(unit::tag_t tag)
{
    static TestGlobalWarHazardHelper _helper;

    // Kick off a background thread
    _helper.BackgroundThread();

    // Detect the reordering
    _helper.WriteAfterRead(tag);
}

// Check that writeglobal operations are shadowed so that startcondition can still be
// computed, and still doesn't take effect if condition is false
class WriteGlobalShadowedIfStartConditionFalseTestTest
{
private:
    uint32 g_WriteGlobalShadowedIfStartConditionFalseTest = 0;

public:
    void run(unit::tag_t tag)
    {
        uint64 start = cycles();
        inline bool test(uint64 start)
        {
            auto result = (cycles() - start) > 10;
            g_WriteGlobalShadowedIfStartConditionFalseTest += cast<uint1>(result);
            return g_WriteGlobalShadowedIfStartConditionFalseTest != 0;
        }
        atomic do; while(!test(start));
        unit::assert_equal(tag, 1, g_WriteGlobalShadowedIfStartConditionFalseTest);
    }
}

// The combination of [[thread_rate(2)]] and atomic do can give rise to
// a situation where, on even cycles the input FIFO is dequeued but the atomic do
// condition is false (thus thread does not advance), followed by an odd cycle
// where the atomic do condition is true causing the thread to advance.
// This is illegal since we have a thread advancing in an odd cycle, and is a
// problem since in the immediately-following cycle (and even one) another new
// thread could advance resulting in just 1 cycle separation between the two.
class ThreadRateWaitForTest
{
private:
    [[pipelined, thread_rate(2)]] void RunThreadRateWaitForTest(uint32 tid, unit::tag_t tag)
    {
        atomic do; while(cycles() % 16 <= 0);

        // Check that all threads are spaced at least 2 cycles apart
        atomic
        {
            static uint64 _last_cycles;
            uint64 current_cycles = cycles();
            if (tid != 0)
            {
                unit::assert(tag, current_cycles - _last_cycles >= 2);
            }
            _last_cycles = current_cycles;
        }
    }
public:
    void run(unit::tag_t tag)
    {
        RunThreadRateWaitForTest(128, tag);
    }
}

// Call to an external function with latency=1
// in an atomic block.  Operands to the external function
// are pure functions of global variables, and hence
// can be precomputed via a global view
class RegisteredIncrement
{
public:
    [[pure]] [[latency(1)]] uint32 increment(uint32 x);
}

extern RegisteredIncrement;

inline void ExternalCallFunctionOfGlobal(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static uint32 _idx = -1;
        static RegisteredIncrement _obj;
        uint32 actual;

        atomic
        {
            actual = _obj.increment(_idx + 1);
            _idx = id;
        }

        uint32 expected = id + 1;

        unit::assert_equal(tag, expected, actual);
    });
}

// Call to a fixed-latency extern function in an atomic block.
// Predication must be removed to allow scheduling to succeed.
inline void RemovePurePredication(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint32 id)
    {
        static RegisteredIncrement _obj;
        uint32 actual = 0;

        bool predicate;

        atomic
        {
            static uint32 _idx = -1;

            predicate = (0 != (_idx % 4)) || (0 != (id % 2));

            if (predicate)
            {
                actual = _obj.increment(_idx + 1);
            }

            _idx = id;
        }

        uint32 expected = predicate ? id + 1 : 0;

        unit::assert_equal(tag, expected, actual);
    });
}

inline void test_main()
{
    unit::test<1>(AddressFunctionOfMulipleGlobals);
    unit::test<2>(AddressFunctionOfGlobalWithPredicatedGlobal);
    unit::test<3>(AddressFunctionOfGlobalWithInitialValue);
    unit::test<4>(ByteAddressableMemStaticLoadAddr);
    unit::test<5>(AddressFunctionOfGlobalUpdatedInSeperateAtomic);
    unit::test<6>(TestGlobalWarHazard);
    unit::test<8>(unit::fixture<WriteGlobalShadowedIfStartConditionFalseTestTest>());
    unit::test<9>(unit::fixture<ThreadRateWaitForTest>());
    unit::test<10>(unit::fixture<UnrelatedAtomicChainTest>());
    unit::test<11>(RewriteGlobalAtomic_OneWrite_NoPred);
    unit::test<12>(RewriteGlobalAtomic_OneWrite_Pred);
    unit::test<13>(RewriteGlobalAtomic_OneWrite_Pred_RAW);
    unit::test<14>(RewriteGlobalAtomic_TwoWrite_NoPred);
    unit::test<15>(RewriteGlobalAtomic_TwoWrite_Pred_RAW);
    unit::test<16>(MemAddrInScheduleBlock);
    unit::test<17>(DspInAtomic);
    unit::test<18>(AddressFunctionOfGlobal);
    unit::test<19>(MemoryPredicateFunctionOfGlobal);
    unit::test<20>(RmwTwoMemoriesOneWithBypassOneWithout);
    unit::test<21>(ExternalCallFunctionOfGlobal);
    unit::test<22>(RemovePurePredication);
}
