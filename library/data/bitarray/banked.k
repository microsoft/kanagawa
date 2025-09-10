// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
module data.bitarray.banked
    { bitarray
    }

import data.bitarray as ba
import data.vector as vec

//| A fixed sized array of bits, stored internally as a set of banks,
// where each bank is an array of words.
// Provides methods to read or write at multiple bits or words
template
    < auto Size                                             //< Number of bits in the bit vector.
    , auto WordWidth                                        //< Number of bits in each word.
    , auto Banks                                            //< Number of banks.  Higher values increase throughput and resource usage.
    , template <typename, auto> typename Memory = memory    //< Memory implementation.
    >
class bitarray
{
private:
    //| Size must be a multiple of word width
    static_assert(0 == (Size % WordWidth));

    const auto WordCount = Size / WordWidth;

    //| Word count must be a multiple of bank count
    static_assert(0 == (WordCount % Banks));

    const auto WordsPerBank = WordCount / Banks;

    const auto BitsPerBank = Size / Banks;

    using bank_t = ba::bitarray<BitsPerBank, WordWidth, Memory>;

    // Aligned ranges of `WordWidth` bits are all stored
    // in the same bank
    // Adjacent words are spread across banks (round-robin)
    struct decomposed_addr
    {
        index_t<WordWidth> bit_index_in_word;
        index_t<Banks> bank_index;
        index_t<WordsPerBank> word_index_in_bank;
    }

    struct decomposed_addr_in_bank
    {
        index_t<WordWidth> bit_index_in_word;
        index_t<WordsPerBank> word_index_in_bank;
    }

    inline index_t<Banks> get_bit_bank_index(bit_addr_t addr)
    {
        decomposed_addr da = cast<decomposed_addr>(addr);

        return da.bank_index;
    }

    // Converts the address of a bit within this object to the address of a bit within a bank
    inline bank_t::bit_addr_t to_bank_bit_addr(bit_addr_t addr)
    {
        decomposed_addr da = cast<decomposed_addr>(addr);
        decomposed_addr_in_bank da_in_bank = {.bit_index_in_word = da.bit_index_in_word, .word_index_in_bank = da.word_index_in_bank};

        return cast<bank_t::bit_addr_t>(da_in_bank);
    }

    // Returns true if a given word is assigned to a given bank
    inline index_t<Banks> get_word_bank_index(word_addr_t addr)
    {
        return addr % Banks;
    }

    // Converts the address of a word within this object to the address of a word within a bank
    inline bank_t::word_addr_t to_bank_word_addr(word_addr_t addr)
    {
        return cast<bank_t::word_addr_t>(addr / Banks);
    }

public:
    //| Represents the address of a single bit.
    using bit_addr_t = index_t<Size>;

    //| Represents the address of a single word.
    using word_addr_t = index_t<WordCount>;

    //| A single word which can be read or written.
    using word_t = uint<WordWidth>;

    //| Get the value of multiple bits.
    template<auto MaxCount>
    inline vec::vector<bool, MaxCount> read_bits
        ( vec::vector<bit_addr_t, MaxCount> addresses //< Addresses of bits to read.
        )
    {
        return vec::sharded_map<Banks>(
            [](index_t<Banks> bank_index, bit_addr_t addr)
            {
                return _banks[bank_index].read_bit(to_bank_bit_addr(addr));
            },
            get_bit_bank_index,
            addresses);
    }

    //| Get the value of one bit.
    inline bool read_bit
        ( bit_addr_t address //< Address of bit to read.
        )
    {
        vec::vector<bool, 1> result = read_bits<1>({{address}, 1});

        assert(result.size == 1);

        return result.data[0];
    }

    //| Get the value of multiple words.
    template<auto MaxCount>
    inline vec::vector<word_t, MaxCount> read_words
        ( vec::vector<word_addr_t, MaxCount> addresses //< Addresses of words to read.
        )
    {
        return vec::sharded_map<Banks>(
            [](index_t<Banks> bank_index, word_addr_t addr)
            {
                return _banks[bank_index].read_word(to_bank_word_addr(addr));
            },
            get_word_bank_index,
            addresses);
    }

    //| Get the value of a single multiple word.
    inline word_t read_word
        ( word_addr_t address //< Addresses of the word to read.
        )
    {
        vec::vector<word_t, 1> result = read_words<1>({{address}, 1});
        assert(result.size == 1);

        return result.data[0];
    }

    //| Set the values of multiple bits.
    template<auto MaxCount>
    inline void write_bits
        ( vec::vector<pair<bit_addr_t, bool>, MaxCount> addresses_and_values //< Addresses and values of bits to write.
        )
    {
        vec::sharded_for_each<Banks>(
            [](index_t<Banks> bank_index, pair<bit_addr_t, bool> addr_and_value)
            {
                _banks[bank_index].write_bit(to_bank_bit_addr(addr_and_value.first), addr_and_value.second);
            },
            [](pair<bit_addr_t, bool> addr_vand_value)
            {
                return get_bit_bank_index(addr_vand_value.first);
            },
            addresses_and_values);
    }

    //| Set the value of one bit.
    inline void write_bit
        ( bit_addr_t address //< Address of bit to write.
        , bool value         //< Value to write.
        )
    {
        write_bits<1>({{{address, value}}, 1});
    }

    //| Set the values of multiple words.
    template<auto MaxCount>
    inline void write_words
        ( vec::vector<pair<word_addr_t, word_t>, MaxCount> addresses_and_values //< Addresses and values of words to write.
        )
    {
        vec::sharded_for_each<Banks>(
            [](index_t<Banks> bank_index, pair<word_addr_t, word_t> addr_and_value)
            {
                _banks[bank_index].write_word(to_bank_word_addr(addr_and_value.first), addr_and_value.second);
            },
            [](pair<word_addr_t, word_t> addr_vand_value)
            {
                return get_word_bank_index(addr_vand_value.first);
            },
            addresses_and_values);
    }

     //| Set the value of one word.
    inline void write_word
        ( word_addr_t address //< Address of word to write.
        , word_t value        //< Value to write.
        )
    {
        write_words<1>({{{address, value}}, 1});
    }

private:
    bank_t[Banks] _banks;
}