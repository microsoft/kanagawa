// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.hash_table
import data.random.lfsr
import numeric.int.operator.unsigned as unsigned
import sync.atomic
import test.unit as unit
import .options as opt
import test.runner

template<typename T, typename R>
inline R HashFunction(T data)
{
    return cast<R>(data);
}

template<auto Depth, auto MaxIterations, auto MaxFailures, auto MaxCycles, auto Associativity, auto HistorySize>
class ReferenceTestImpl
{
private:
    const auto KeyWidth = 6;
    using key_t = uint<KeyWidth>;
    using value_t = uint64;
    const auto GenerationBits = 3;
    const auto NumTestIterations = 16;
    const auto NumLookups = 256;

    using table_t = hash_table<key_t, value_t, Depth, Associativity, HashFunction<key_t, index_t<Depth / Associativity>>, GenerationBits, MaxIterations, 1, HistorySize>;
    table_t _table;

    // Sparse table that should compute the same result as _table
    value_t[1 << KeyWidth] _reference_table;

    bool _initial_tag_error = false;
    uint32 _num_lookup_failures = 0;

public:
    void Test(unit::tag_t tag)
    {
        uint64 start = cycles();

        // Hash table is reset between each outer loop iteration
        pipelined_for (NumTestIterations, [tag](index_t<NumTestIterations> outer)
        {
            pipelined_for (NumLookups, [tag, outer](index_t<NumLookups> inner)
            {
                // Random key
                static lfsr_random<KeyWidth> key_lfsr;
                key_t key = key_lfsr.next({});

                // Generate a unique sequence number for each request
                uint64 sequence_number = first(atomically([](uint64 prev)
                {
                    return cast<uint64>(prev + 1);
                }));

                const auto allocate_fn = [tag, sequence_number] -> value_t
                {
                    // Sequence numbers should always increment
                    // This tests to ensure the hash table calls allocate_fn in order
                    uint64 prev_sequence_number = first(atomically([sequence_number](uint64 p)
                    {
                        return sequence_number;
                    }));

                    unit::assert_string(tag, prev_sequence_number <= sequence_number, "decreasing sequence number detected");

                    return 17;
                };

                const auto access_fn = [tag](value_t prev, bool inserted) -> value_t
                {
                    value_t result = prev;

                    if (inserted)
                    {
                        // Test that the value from `allocate_fn` is passed here
                        if (prev != 17)
                        {
                            _initial_tag_error = true;
                        }

                        result = 0;
                    }

                    return result + 1;
                };

                auto result = _table.insert_or_update(
                    key,
                    inner == 0,
                    true,
                    allocate_fn,
                    access_fn);

                if (result.is_valid)
                {
                    value_t before_increment;
                    value_t after_increment;

                    // Apply the same operation to the reference table
                    atomic
                    {
                        before_increment = result.value.inserted ? 17 : _reference_table[key];

                        after_increment = result.value.inserted ? 1 : before_increment + 1;

                        _reference_table[key] = after_increment;
                    }

                    unit::assert_equal(tag, before_increment, result.value.value.first);
                    unit::assert_equal(tag, after_increment, result.value.value.second);
                }
                else
                {
                    atomic
                    {
                        _num_lookup_failures++;
                    }
                }
            });
        });

        uint64 elapsed_cycles = unsigned::sub(cycles(), start);

        print("lookup failures {_num_lookup_failures} cycles {elapsed_cycles}\n");

        if (opt::stall == 0)
        {
            unit::assert_string(tag, _num_lookup_failures <= MaxFailures, "Too many failures encountered");
            unit::assert_string(tag, elapsed_cycles <= MaxCycles, "Cycle count too high");
        }

        unit::assert(tag, !_initial_tag_error);
    }
};

template<auto Depth, auto MaxIterations, auto MaxFailures, auto MaxCycles, auto Associativity = 4>
inline void ReferenceTest(unit::tag_t tag)
{
    {
        // Test with history disabled
        static ReferenceTestImpl<Depth, MaxIterations, MaxFailures, MaxCycles, Associativity, 0> _test_history_0;
        _test_history_0.Test(tag);
    }

    {
        // Test with history enabled
        static ReferenceTestImpl<Depth, MaxIterations, MaxFailures, MaxCycles, Associativity, 1> _test_history_1;
        _test_history_1.Test(tag);
    }
}

// Use a hash table to count the number of unique items in a stream of items
template<auto TotalUniqueItems, auto RepetitionFactor>
class CountUniqueTestImpl
{
private:
    const auto TotalItems = TotalUniqueItems * RepetitionFactor;
    const auto Depth = TotalUniqueItems; // to ensure enough room

    using key_t = uint32;
    using value_t = uint16;
    const auto Associativity = 4;

    using hash_t = hash_table<key_t, value_t, Depth, Associativity>;
    hash_t _table;

public:
    void Test(unit::tag_t tag)
    {
        pipelined_for(RepetitionFactor, [tag](index_t<RepetitionFactor> r)
        {
            pipelined_for(TotalUniqueItems, [tag, r](index_t<TotalUniqueItems> key)
            {
                auto result = _table.insert_or_update(
                    key,
                    (r == 0) && (key == 0),
                    true,
                    [] { return cast<value_t>(0); },
                    [](value_t v, bool inserted) {return cast<value_t>(v + 1); }
                    );

                unit::assert(tag, result.is_valid);

                unit::assert_equal(tag, r + 1, result.value.value.second);
            });
        });
    }
}

template<auto TotalUniqueItems, auto RepetitionFactor>
inline void CountUniqueTest(unit::tag_t tag)
{
    static CountUniqueTestImpl<TotalUniqueItems, RepetitionFactor> _test;
    _test.Test(tag);
}

// Use a hash table to count the number of unique items in a stream of items
class InsertOnMissTestImpl
{
private:
    const auto Depth = 4096;
    using key_t = uint32;
    using value_t = uint64;
    const auto Associativity = 4;

    using hash_t = hash_table<key_t, value_t, Depth, Associativity, HashFunction<key_t, index_t<Depth / Associativity>>>;
    hash_t _table;

    memory<bool, Depth> _reference;

public:
    void Test(unit::tag_t tag)
    {
        // Initialize reference memory
        pipelined_for(Depth, [](index_t<Depth> i)
        {
            _reference[i] = false;
        });

        const auto Iterations = 1024 * 2;

        pipelined_for(Iterations, [tag](index_t<Iterations> i)
        {
            static lfsr_random<bitsizeof key_t> _key_lfsr;
            key_t key = _key_lfsr.next({});

            static lfsr_random<4> _insert_lfsr;
            bool insert_on_miss = 0 == _insert_lfsr.next({});

            auto result = _table.insert_or_update(
                    key,
                    i == 0,
                    insert_on_miss,
                    [key] { return cast<value_t>(key); },
                    [insert_on_miss, tag](value_t v, bool inserted)
                    {
                        return cast<value_t>(0);
                    }
                    );

            // Apply the same operation to the reference
            bool already_present = false;
            bool inserted_reference = false;

            atomic
            {
                already_present = _reference[key];

                if (!already_present && insert_on_miss)
                {
                    inserted_reference = true;
                    _reference[key] = true;
                }
            }

            if (insert_on_miss)
            {
                // If insert_on_miss is true, then insert_or_update should always succeed
                unit::assert_string(tag, result.is_valid, "insert_or_update failed even though insert_on_miss is true");
            }
            else
            {
                // insert_or_update should succeed if and only if the key was already present
                unit::assert_equal(tag, already_present, result.is_valid);
            }

            if (result.is_valid)
            {
                // If insert_or_update suceeded, then the reference inserted should match the returned inserted
                unit::assert_equal(tag, inserted_reference, result.value.inserted);
            }
        });
    }
}

inline void InsertOnMissTest(unit::tag_t tag)
{
    static InsertOnMissTestImpl _test;
    _test.Test(tag);
}

// Test for hazard resolution
template<auto HistorySize, bool RandomKey, auto Depth, auto Associativity>
inline void HazardTest(unit::tag_t tag)
{
    // Number of times the table is reset
    const auto OuterIterations = 4;

    pipelined_for (OuterIterations, [tag](index_t<OuterIterations> outer_tid)
    {
        // Number of elements inserted into the table
        const auto Inserts = 16;

        pipelined_for (Inserts, [tag](index_t<Inserts> inner_tid)
        {
            static hash_table<uint32, uint32, Depth, Associativity, HashFunction<uint32, index_t<Depth / Associativity>>, 4, 1, 1, HistorySize> _hash_table;

            uint32 key;

            static if(RandomKey)
            {
                static lfsr_random<32> _lfsr;

                key = _lfsr.next({});
            }
            else
            {
                key = inner_tid;
            }

            key = key % Depth;

            assert(key < Depth);

            bool reset = inner_tid == 0;

            // Compute expected value
            uint32[Depth] expected = second(atomically([key, reset](uint32[Depth] prev)
            {
                assert(key < Depth);

                uint32[Depth] result = prev;

                if (reset)
                {
                    result = {};
                }

                result[key]++;

                return result;
            }));

            const auto allocate_fn = []->uint32
            {
                return 0;
            };

            const auto access_fn = [](uint32 value, bool inserted)->uint32
            {
                return value + 1;
            };

            auto result = _hash_table.insert_or_update(key, reset, true, allocate_fn, access_fn);

            unit::assert(tag, result.is_valid);
            unit::assert_equal(tag, expected[key], result.value.value.second);
        });
    });
}

// Test reset of history data
// If history data is not reset when generation IDs wrap
// then a thread following the generation ID wrap could see
// history data from a previous thread with the same generation ID
// The current thread should not use that history data
// because it became stale when the tags were reset
template<auto HistorySize = 16, auto Depth = 8>
inline void ResetHazardTest(unit::tag_t tag)
{
    const auto Iterations = 256;

    pipelined_for (Iterations, [tag](index_t<Iterations> tid)
    {
        const auto Associativity = 1;

        const auto GenerationBits = 2;

        static hash_table<uint32, uint32, Depth, Associativity, HashFunction<uint32, index_t<Depth / Associativity>>, GenerationBits, 1, 1, HistorySize> _hash_table;

        static lfsr_random<32> _lfsr;

        uint32 key = _lfsr.next({}) % 8;

        assert(key < Depth);

        // Reset on each insert operation
        bool reset = true;

        const auto allocate_fn = []->uint32
        {
            return 0;
        };

        const auto access_fn = [](uint32 value, bool inserted)->uint32
        {
            return value + 1;
        };

        auto result = _hash_table.insert_or_update(key, reset, true, allocate_fn, access_fn);

        unit::assert(tag, result.is_valid);
        unit::assert_equal(tag, 1, result.value.value.second);
    });
}

// Hash tables with different history sizes should produce the same results
template<auto HistorySize1, auto HistorySize2, auto Depth, auto Associativity>
inline void HistorySizeCompareTest(unit::tag_t tag)
{
    const auto Iterations = 4096;

    pipelined_for (Iterations, [tag](index_t<Iterations> tid)
    {
        static hash_table<uint32, uint32, Depth, Associativity, HashFunction<uint32, index_t<Depth / Associativity>>, 2, 1, 1, HistorySize1> _hash_table1;
        static hash_table<uint32, uint32, Depth, Associativity, HashFunction<uint32, index_t<Depth / Associativity>>, 2, 1, 1, HistorySize2> _hash_table2;

        static lfsr_random<32> _key_lfsr;
        uint32 key = _key_lfsr.next({}) % (Depth * 4); // *4 to allow for some collisions, but also to limit the range of keys

        static lfsr_random<32> _reset_lfsr;
        bool reset = (tid == 0) || ((_reset_lfsr.next({}) % 64) == 0);

        static lfsr_random<32> _insert_on_miss_lfsr;
        bool insert_on_miss = (_insert_on_miss_lfsr.next({}) % 32) > 10;

        const auto allocate_fn = []->uint32
        {
            return 0;
        };

        const auto access_fn = [](uint32 value, bool inserted)->uint32
        {
            return value + 1;
        };

        auto result1 = _hash_table1.insert_or_update(key, reset, insert_on_miss, allocate_fn, access_fn);
        auto result2 = _hash_table2.insert_or_update(key, reset, insert_on_miss, allocate_fn, access_fn);

        unit::assert_equal(tag, result1.is_valid, result2.is_valid);

        if (result1.is_valid)
        {
            unit::assert_equal(tag, result1.value.inserted,     result2.value.inserted);
            unit::assert_equal(tag, result1.value.value.first,  result2.value.value.first);
            unit::assert_equal(tag, result1.value.value.second, result2.value.value.second);
        }
    });
}


inline void test_main()
{
    // Plenty of space and time.  There should be no failures
    unit::test<1>(ReferenceTest<512, 512, 0, 0x1800>);

    // Not enough space, there will be failures
    unit::test<2>(ReferenceTest<32, 512, 0x800, 0x5A00>);

    // Not enough loop iterations, there will be failures
    unit::test<3>(ReferenceTest<16, 1, 0xC00, 0x1200>);

    unit::test<4>(CountUniqueTest<1024, 7>);

    unit::test<5>(InsertOnMissTest);

    // non-pow-2 associativity
    unit::test<6>(ReferenceTest<384, 384, 0, 0x1800, 12>);

    // Testing data address computation with non-power of 2 associativity
    // ensure the table is filled up
    unit::test<6>(ReferenceTest<24, 2048, 0x1000, 0x4000, 6>);

    // Hazard handling
    unit::test<7>(HazardTest<1, false, 8, 1>);

    unit::test<8>(HazardTest<1, true, 8, 1>);

    unit::test<9>(HazardTest<2, false, 8, 1>);

    unit::test<10>(HazardTest<2, true, 8, 1>);

    unit::test<11>(HazardTest<3, false, 8, 1>);

    unit::test<12>(HazardTest<3, true, 8, 1>);

    unit::test<13>(HazardTest<3, true, 8, 2>);

    unit::test<14>(HazardTest<3, true, 32, 4>);

    unit::test<15>(HazardTest<3, true, 32, 2>);

    unit::test<16>(ResetHazardTest);

    unit::test<17>(HistorySizeCompareTest<1, 3, 64, 4>);

    unit::test<17>(HistorySizeCompareTest<0, 1, 64, 4>);
}
