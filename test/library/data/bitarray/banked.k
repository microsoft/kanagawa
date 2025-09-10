// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.bitarray.banked
import data.random.lfsr
import data.vector as vec
import test.runner
import test.unit as unit

// tests the scalar read_{bit, word} and write_{bit, word} wrappers
class scalar_test
{
private:
    const auto Size = 1024;
    const auto Banks = 4;
    const auto WordWidth = 8;
    const auto WordCount = Size / WordWidth;

    using bitarray_t = bitarray<Size, WordWidth, Banks>;

    bitarray_t _bitarray;

    memory<bool, Size> _reference;

public:
    void run(unit::tag_t tag)
    {
        // Initialize _bitarray and _reference
        pipelined_for(Size, [](index_t<Size> i)
        {
            _bitarray.write_bit(i, false);

            _reference[i] = false;
        });

        // Randomly write bits
        const auto WriteIterations = 4096;

        pipelined_for(WriteIterations, [](index_t<WriteIterations> iteration)
        {
            static lfsr_random<32> _lfsr;

            uint32 rand = _lfsr.next({});

            bitarray_t::bit_addr_t addr = static_cast(rand);

            bool val = reinterpret_cast<bool>(rand / Size);

            _bitarray.write_bit(addr, val);

            _reference[addr] = val;
        });

        // Randomly write words
        const auto WriteWordIterations = 32;

        pipelined_for(WriteWordIterations, [](index_t<WriteWordIterations> iteration)
        {
            static lfsr_random<32> _lfsr;

            uint32 rand = _lfsr.next({});

            bitarray_t::word_addr_t addr = static_cast(rand);

            bitarray_t::word_t val = reinterpret_cast<bitarray_t::word_t>(rand / WordCount);

            _bitarray.write_word(addr, val);

            pipelined_for(WordWidth, [addr, val](index_t<WordWidth> i)
            {
                _reference[addr * WordWidth + i] = reinterpret_cast<bool>(val >> i);
            });
        });

        // Read all bits and compare values
        pipelined_for(Size, [tag](index_t<Size> i)
        {
            bool actual = _bitarray.read_bit(i);

            bool expected = _reference[i];

            unit::assert_equal(tag, expected, actual);
        });

        // Read all words and compare values
        pipelined_for(WordCount, [tag](index_t<WordCount> i)
        {
            bitarray_t::word_t actual = _bitarray.read_word(i);

            bitarray_t::word_t expected = static_cast(pipelined_map<WordWidth>(WordWidth, [i](index_t<WordWidth> bit_index)
            {
                return _reference[i * WordWidth + bit_index];
            }));

            unit::assert_equal(tag, expected, actual);
        });
    }
}

class vector_test
{
private:
    const auto Size = 1024;
    const auto Banks = 4;
    const auto WordWidth = 8;
    const auto WordCount = Size / WordWidth;
    const auto MaxBitsAccessed = 8;
    const auto MaxWordsAccessed = 4;

    using bitarray_t = bitarray<Size, WordWidth, Banks>;

    bitarray_t _bitarray;

    memory<bool, Size> _reference;

public:
    void run(unit::tag_t tag)
    {
        // Initialize _bitarray and _reference
        pipelined_for(Size, [](index_t<Size> i)
        {
            _bitarray.write_bit(i, false);

            _reference[i] = false;
        });

        // Randomly write bits
        const auto WriteIterations = 4096;

        pipelined_for(WriteIterations, [](index_t<WriteIterations> iteration)
        {
            static lfsr_random<32> _lfsr;

            uint32 rand = _lfsr.next({});

            vec::vector<pair<bitarray_t::bit_addr_t, bool>, MaxBitsAccessed> addresses_and_values;

            addresses_and_values.size = (rand % MaxBitsAccessed) + 1;
            rand = rand / MaxBitsAccessed;

            static for (const auto i : MaxBitsAccessed)
            {
                addresses_and_values.data[i].first = rand % Size;
                rand = rand >> 1; // to ensure that all lanes can have non-zero addresses

                addresses_and_values.data[i].second = reinterpret_cast<bool>(rand % 2);
                rand = rand / 2;
            }

            _bitarray.write_bits(addresses_and_values);

            pipelined_for(addresses_and_values.size, [addresses_and_values](index_t<MaxBitsAccessed> i)
            {
                bitarray_t::bit_addr_t addr = addresses_and_values.data[i].first;
                bool val = addresses_and_values.data[i].second;

                _reference[addr] = val;
            });
        });

        // Randomly write words
        const auto WriteWordIterations = 16;

        pipelined_for(WriteWordIterations, [](index_t<WriteWordIterations> iteration)
        {
            static lfsr_random<32> _lfsr;

            uint32 rand = _lfsr.next({});

            vec::vector<pair<bitarray_t::word_addr_t, bitarray_t::word_t>, MaxWordsAccessed> addresses_and_values;

            addresses_and_values.size = (rand % MaxWordsAccessed) + 1;
            rand = rand / MaxWordsAccessed;

            static for (const auto i : MaxWordsAccessed)
            {
                addresses_and_values.data[i].first = rand % WordCount;
                rand = rand >> 1; // to ensure that all lanes can have non-zero addresses and values

                addresses_and_values.data[i].second = cast<bitarray_t::word_t>(rand % WordWidth);
                rand = rand >> 1; // to ensure that all lanes can have non-zero addresses and values
            }

            _bitarray.write_words(addresses_and_values);

            pipelined_for(addresses_and_values.size, [addresses_and_values](index_t<MaxWordsAccessed> i)
            {
                bitarray_t::word_addr_t addr = addresses_and_values.data[i].first;
                bitarray_t::word_t val = addresses_and_values.data[i].second;

                pipelined_for(WordWidth, [addr, val](index_t<WordWidth> i)
                {
                    _reference[addr * WordWidth + i] = reinterpret_cast<bool>(val >> i);
                });
            });
        });

        // Read all bits and compare values
        pipelined_for(Size, [tag](index_t<Size> i)
        {
            bool actual = _bitarray.read_bit(i);

            bool expected = _reference[i];

            unit::assert_equal(tag, expected, actual);
        });

        // Randomly read collections of bits
        const auto ReadIterations = 4096;

        pipelined_for(ReadIterations, [tag](index_t<ReadIterations> iteration)
        {
            static lfsr_random<32> _lfsr;

            uint32 rand = _lfsr.next({});

            vec::vector<bitarray_t::bit_addr_t, MaxBitsAccessed> addresses;

            addresses.size = (rand % MaxBitsAccessed) + 1;
            rand = rand / MaxBitsAccessed;

            static for (const auto i : MaxBitsAccessed)
            {
                addresses.data[i] = rand % Size;
                rand = rand >> 1; // to ensure that all lanes can have non-zero addresses
            }

            vec::vector<bool, MaxBitsAccessed> values = _bitarray.read_bits(addresses);

            unit::assert_equal(tag, addresses.size, values.size);

            pipelined_for(addresses.size, [tag, addresses, values](index_t<MaxBitsAccessed> i)
            {
                bitarray_t::bit_addr_t addr = addresses.data[i];

                bool actual = values.data[i];

                bool expected = _reference[addr];

                unit::assert_equal(tag, expected, actual);
            });
        });

        // Randomly read words
        const auto ReadWordIterations = 128;

        pipelined_for(ReadWordIterations, [tag](index_t<ReadIterations> ReadWordIterations)
        {
            static lfsr_random<32> _lfsr;

            uint32 rand = _lfsr.next({});

            vec::vector<bitarray_t::word_addr_t, MaxWordsAccessed> addresses;

            addresses.size = (rand % MaxWordsAccessed) + 1;
            rand = rand / MaxWordsAccessed;

            static for (const auto i : MaxWordsAccessed)
            {
                addresses.data[i] = rand % WordCount;
                rand = rand >> 1; // to ensure that all lanes can have non-zero addresses
            }

            vec::vector<bitarray_t::word_t, MaxWordsAccessed> values = _bitarray.read_words(addresses);

            unit::assert_equal(tag, addresses.size, values.size);

            pipelined_for(addresses.size, [tag, addresses, values](index_t<MaxWordsAccessed> i)
            {
                bitarray_t::word_addr_t addr = addresses.data[i];

                bitarray_t::word_t actual = values.data[i];

                bitarray_t::word_t expected = static_cast(pipelined_map<WordWidth>(WordWidth, [addr](index_t<WordWidth> bit_index)
                {
                    return _reference[addr * WordWidth + bit_index];
                }));

                unit::assert_equal(tag, expected, actual);
            });
        });
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<scalar_test>());
    unit::test<2>(unit::fixture<vector_test>());
}

