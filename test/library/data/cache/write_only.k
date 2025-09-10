// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.array
import data.cache.write_only
import data.optional
import data.random.lfsr
import test.unit as unit
import test.runner

// a cache line is 4 bytes
using word_t = uint8;

const auto WordCount = 4;

using cache_line_t = word_t[WordCount];

const auto Depth = 128;

const auto Associativity = 4;

const auto Banks = 2;

const auto KeyWidth = 10;
using key_t = uint<KeyWidth>;

const auto MemoryDepth = 1 << KeyWidth;

class ReferenceTest
{
private:
    memory<cache_line_t, MemoryDepth> _reference_memory;

    memory<cache_line_t, MemoryDepth> _backing_store;

    cache<key_t, word_t, WordCount, uint2, Associativity, Depth, Banks> _cache = {.store = WriteCallback};

    void WriteCallback(key_t key, optional<word_t>[WordCount] words)
    {
        atomic
        {
            _backing_store[key] = zip_with(from_optional<word_t>, _backing_store[key], words);
        }
    }

public:
    void run(unit::tag_t tag)
    {
        // Fill reference memory and backing store
        pipelined_for(MemoryDepth, [](index_t<MemoryDepth> tid)
        {
            cache_line_t cache_line;

            static for (const auto i : 4)
            {
                cache_line[i] = cast<word_t>((tid * 4) + 1);
            }

            _reference_memory[tid] = cache_line;
            _backing_store[tid] = cache_line;
        });

        // Multiple outer loops to ensure the cache can be re-initialized
        for (const auto outerIteration : 4)
        {
            _cache.initialize();

            const auto NumIterations = 1024 * 4;

            pipelined_for(NumIterations, [](index_t<NumIterations> tid)
            {
                // Generate random key, data, enable
                static lfsr_random<KeyWidth> key_lfsr;
                key_t key = key_lfsr.next({});

                static lfsr_random<bitsizeof(cache_line_t)> data_lfsr;
                cache_line_t cache_line = cast<cache_line_t>(data_lfsr.next({}));

                static lfsr_random<WordCount> enable_lfsr;
                bool[WordCount] enable = cast<bool[WordCount]>(enable_lfsr.next({}));

                _cache.write(key, static_cast(key), zip_with(make_optional<word_t>, enable, cache_line));

                // Also write to _reference_memory
                atomic
                {
                    cache_line_t reference_cache_line = _reference_memory[key];

                    static for (const auto i : 4)
                    {
                        if (enable[i])
                        {
                            reference_cache_line[i] = cache_line[i];
                        }
                    }

                    _reference_memory[key] = reference_cache_line;
                }
            });

            // Flush unwritten data out of the cache
            _cache.flush();

            // Compare _reference_memory with _backing_store
            pipelined_for(MemoryDepth, [tag](index_t<MemoryDepth> tid)
            {
                cache_line_t expected = _reference_memory[tid];

                cache_line_t actual = _backing_store[tid];

                unit::assert_equal(tag, expected, actual);
            });
        }
    }
}

inline void test_main()
{
    print("data.cache.write_only test start\n");

    unit::test<1>(unit::fixture<ReferenceTest>());

    print("data.cache.write_only test complete\n");
}
