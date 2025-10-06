// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner
import data.array
import data.random.lfsr

// Atomic memory read-modify-write where the write is optimzied out
inline void MultipleRmwPredicateStore(unit::tag_t tag)
{
    class Helper
    {
        memory<uint32, 32> _mem;
        uint32 _counter = 0;

    public:
        void test(unit::tag_t tag)
        {
            pipelined_for(32, [tag](index_t<32> x)
            {
                _mem[x] = x;
            });

            pipelined_for(32, [tag](index_t<32> x)
            {
                bool doWrite = (x >= 32);

                atomic
                {
                    uint32 val = _mem[x];

                    // To prevent the load from being optimized away
                    _counter += val;

                    val++;

                    if (doWrite)
                    {
                        _mem[x] = val;
                    }
                }
            });

            unit::assert_equal(tag, _counter, 0x1f0);

            pipelined_for(32, [tag](index_t<32> x)
            {
                uint32 actual = _mem[x];

                unit::assert_equal(tag, x, actual);
            });
        }
    }

    static Helper _helper;

    _helper.test(tag);
}

inline void BypassTwoLoadsOneStore(unit::tag_t tag)
{
    // A single atomic block with 2 memory loads from the same memory
    // and a single store
    class Helper
    {
    private:
        memory<uint32, 8> _mem;

    public:
        void test(unit::tag_t tag)
        {
            pipelined_for(8, [](index_t<8> x)
            {
                _mem[x] = x;
            });

            pipelined_for(7, [](index_t<8> x)
            {
                atomic
                {
                    auto val1 = _mem[x];
                    auto val2 = _mem[x + 1];

                    _mem[x + 1] = val1 + val2;
                }
            });

            pipelined_for(8, [tag](index_t<8> x)
            {
                uint32 expected = mux(x, 0x0, 0x1, 0x3, 0x6, 0xa ,0xf, 0x15, 0x1c);
                uint32 actual = _mem[x];

                unit::assert_equal(tag, expected, actual);
            });
        }
    }

    static Helper _helper;

    _helper.test(tag);
}

// Bypass on a memory of arrays
inline void BypassArray(unit::tag_t tag)
{
    class Helper
    {
    private:
        using word_t = uint32[4];

        memory<word_t, 8> _mem;
        array<word_t, 8> _array;

    public:
        void test(unit::tag_t tag)
        {
            pipelined_for(8, [](index_t<8> x)
            {
                word_t w = { x, x + 1, x + 2, x + 3 };

                _mem[x] = w;
                _array[x] = w;
            });

            pipelined_for(64, [](index_t<64> tid)
            {
                static lfsr_random<8> _lfsr;
                index_t<8> x = static_cast(_lfsr.next({}));

                bool[4] enable = { x > 2, x > 4, x < 3, true };

                word_t new_data = { x * 2, x * 3, x * 4, x * 5 };

                // The same read-modify-write sequence is applied to the memory and the array
                atomic
                {
                    word_t prev = _mem[x];

                    word_t masked;

                    static for (const auto i : 4)
                    {
                        masked[i] = enable[i] ? prev[i] + new_data[i] : prev[i];
                    }

                    _mem[x] = masked;
                }

                atomic
                {
                    word_t prev = _array[x];

                    word_t masked;

                    static for (const auto i : 4)
                    {
                        masked[i] = enable[i] ? prev[i] + new_data[i] : prev[i];
                    }

                    _array[x] = masked;
                }
            });

            pipelined_for(8, [tag](index_t<8> x)
            {
                word_t expected = _array[x];
                word_t actual = _mem[x];

                unit::assert(tag, equal(expected, actual));
            });
        }
    }

    static Helper _helper;

    _helper.test(tag);
}

// memory_norep accessed in 2 different read-modify-write blocks
// with different update rates
// to test the case where bypass is disabled on one but not the other
inline void BypassNoRepMixRates(unit::tag_t tag)
{
    class Helper
    {
    private:
        memory_norep<uint32, 8> _mem;
        array<uint32, 8> _array;

    public:
        void test(unit::tag_t tag)
        {
            pipelined_for(8, [](index_t<8> x)
            {
                _mem[x] = x;
                _array[x] = x;
            });

            pipelined_for(64, [](index_t<64> tid)
            {
                static lfsr_random<8> _lfsr;

                // ensure hazards are spaced by at least 4 threads
                index_t<8> x = static_cast((_lfsr.next({}) % 2) + (tid % 4) * 2);

                // The same read-modify-write sequence is applied to the memory and the array
                [[schedule(4)]]
                {
                    auto prev = _mem[x];

                    _mem[x] = prev + 1;
                }

                atomic
                {
                    auto prev = _array[x];

                    _array[x] = prev + 1;
                }
            });

            pipelined_for(8, [tag](index_t<8> x)
            {
                uint32 expected = _array[x];
                uint32 actual = _mem[x];

                unit::assert_equal(tag, expected, actual);
            });

            pipelined_for(8, [tag](index_t<8> x)
            {
                // update memory with an update rate of 1
                atomic
                {
                    auto prev = _mem[x];

                    _mem[x] = prev + 1;
                }
            });

            pipelined_for(8, [tag](index_t<8> x)
            {
                uint32 expected = static_cast(_array[x] + 1);
                uint32 actual = _mem[x];

                unit::assert_equal(tag, expected, actual);
            });
        }
    }

    static Helper _helper;

    _helper.test(tag);
}

// Shallow memory (converted to an array internally)
// with an atomic access that only reads some fields
inline void MemToArrayUnusedField(unit::tag_t tag)
{
    class Helper
    {
    private:
        using record = pair<uint8, uint8>;

        memory<record, 8> _mem;

    public:
        void test(unit::tag_t tag)
        {
            pipelined_for(16, [](index_t<16> x)
            {
                index_t<8> addr = static_cast(x);
                bool init = x < 8;

                atomic
                {
                    auto r = _mem[x];
                    r.first = init ? x : r.first + 1;
                    r.second = x; // ignore what was previous in the memory
                    _mem[x] = r;
                }
            });

            pipelined_for(8, [](index_t<8> x)
            {
                auto r = _mem[x];
                assert(r.first == (x + 1));
            });
        }
    }

    static Helper _helper;

    _helper.test(tag);
}

// Predicated load from a memory, where the predicate is known to always be false
inline void LoadPredicateAlwaysFalse(unit::tag_t tag)
{
    // norep to ensure the load is predicated
    static memory_norep<uint32, 8> _mem;

    _mem[3] = 4;

    bool p = false;

    uint32 x = 4;

    if (p)
    {
        x = _mem[3];
    }

    unit::assert_equal(tag, 4, x);
}

inline void InitializedMemory(unit::tag_t tag)
{
    pipelined_for(512, [tag](index_t<512> i)
    {
        static [[memory, initialize]] uint32[512] _mem;
        unit::assert_equal(tag, 0, _mem[i]);
    });

    pipelined_for(4, [tag](index_t<4> i)
    {
        // Test that initialize list overrides the default initial value of 0
        static [[memory, initialize]] uint32[4] _mem = {0, 1, 2, 3};
        unit::assert_equal(tag, i, _mem[i]);
    });
}

// Atomic memory read-modify-write
// nested inside of another atomic.
// Memory depth is low, which triggers memory->array optimization
class MemToArrayNestedAtomic
{
    memory<uint32, 1> _mem;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(32, [tag](index_t<32> i)
        {
            uint32 prev;

            [[schedule(8)]]
            {
                bool is_first = (i == 0);

                atomic
                {
                    prev = _mem[i % 1];

                    _mem[i % 1] = is_first ? i : prev + 1;
                }
            }

            if (i > 0)
            {
                unit::assert_equal(tag, i - 1, prev);
            }
        });
    }
}

inline void test_main()
{
    unit::test<1>(MultipleRmwPredicateStore);
    unit::test<2>(BypassTwoLoadsOneStore);
    unit::test<3>(BypassArray);
    unit::test<4>(BypassNoRepMixRates);
    unit::test<5>(MemToArrayUnusedField);
    unit::test<6>(LoadPredicateAlwaysFalse);
    unit::test<7>(InitializedMemory);
    unit::test<8>(unit::fixture<MemToArrayNestedAtomic>());
}
