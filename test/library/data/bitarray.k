// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.bitarray
import data.random.lfsr
import test.runner
import test.unit as unit

class bitarray_test
{
private:
    const auto Size = 256;
    const auto WordWidth = 8;
    const auto WordCount = Size / WordWidth;

    using bitarray_t = bitarray<Size, WordWidth>;

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

        // Randomly write some whole words
        const auto WriteWordIterations = 8;

        pipelined_for(WriteWordIterations, [](index_t<WriteWordIterations> i)
        {
            static lfsr_random<31> _lfsr;

            uint16 rand = static_cast(_lfsr.next({}));

            index_t<WordCount> word_idx = cast<index_t<WordCount>>(rand);

            bitarray_t::word_t val = static_cast(~rand);

            _bitarray.write_word(word_idx, val);

            pipelined_for(WordWidth, [word_idx, val](index_t<WordWidth> bit_index)
            {
                _reference[word_idx * WordWidth + bit_index] = reinterpret_cast<bool>(val >> bit_index);
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

inline void test_main()
{
    unit::test<1>(unit::fixture<bitarray_test>());
}

