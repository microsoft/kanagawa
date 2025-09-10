// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
module data.bitarray
    { bitarray
    }

import data.array
import numeric.int.operator

//| A fixed sized array of bits, stored internally as a memory of words.
// Provides methods to read or write at bit and word granularity.
template
    < auto Size                                             //< Number of bits in the bit vector.
    , auto WordWidth                                        //< Number of bits in each word.
    , template <typename, auto> typename Memory = memory    //< Memory implementation.
    >
class bitarray
{
private:
    //| Size must be a multiple of word width
    static_assert(0 == (Size % WordWidth));

    const auto WordCount = Size / WordWidth;

public:
    //| Represents the address of a single bit.
    using bit_addr_t = index_t<Size>;

    //| Represents the address of a single word.
    using word_addr_t = index_t<WordCount>;

    //| A single word which can be read or written.
    using word_t = uint<WordWidth>;

    //| Get the value of a single bit from the bitarray.
    inline bool read_bit
        ( bit_addr_t addr   //< Address of bit to read.
        )
    {
        assert(addr < Size);

        auto decomposed_addr = div_mod(addr, WordWidth);

        bool[WordWidth] word = cast<bool[WordWidth]>(_mem[decomposed_addr.first]);

        return word[decomposed_addr.second];
    }

    //| Set the value of a single bit in the bitarray.
    // Note that this is implemented with a read-modify write.
    // If `Memory` is `memory_norep` then concurrent reads and writes of the bitarray
    // are not allowed.
    // If `Memory` is `memory`,
    // then each call site of `read_bit`, `write_bit`, and `read_word` add another replica.
    inline void write_bit
        ( bit_addr_t addr   //< Address of the bit to write.
        , bool val          //< Value to write.
        )
    {
        assert(addr < Size);

        auto decomposed_addr = div_mod(addr, WordWidth);

        // The read-modify-write is implemented with word-width bitwise and followed by bitwise or
        // The bitwise and is used to clear bits
        // the bitwise or is used to set bits
        bool[WordWidth] and_mask = repeat(true);
        bool[WordWidth] or_mask = repeat(false);

        and_mask[decomposed_addr.second] = val;
        or_mask[decomposed_addr.second] = val;

        atomic
        {
            word_t prev = _mem[decomposed_addr.first];

            _mem[decomposed_addr.first] = (prev & cast<word_t>(and_mask)) | cast<word_t>(or_mask);
        }
    }

    //| Get the value of a word.
    inline word_t read_word
        ( word_addr_t addr  //< Address of the word to read.  Note that this is in terms of words, not bits.
        )
    {
        assert(addr < WordCount);

        return _mem[addr];
    }

    //| Set the value of a word.
    inline void write_word
        ( word_addr_t addr  //< Address of the word to read.  Note that this is in terms of words, not bits.
        , word_t word       //< Word to write.
        )
    {
        assert(addr < WordCount);

        _mem[addr] = word;
    }

private:
    Memory<word_t, WordCount> _mem;
}