// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Memory supporting partial word access and unaligned addressing.
*/
module data.memory.unaligned
    { unaligned_read_unaligned_write_memory
    , unaligned_read_aligned_write_memory
    }

import data.array
import data.bits
import data.memory
import data.optional
import type.coerce
import type.stdtype

//| Memory supporting reads/writes of up to `WordSize` elements per cycle at
// aligned or unaligned address. The class internally instantiates `WordSize`
// logical memories that are `bitsizeof T` wide and `TotalSize / WordSize` deep.
template
    < typename T                                            //< Type of the memory element
    , auto WordSize                                         //< Number of elements per word
    , auto TotalSize                                        //< Total number of elements in memory
    , template <typename, auto> typename Memory = memory    //< Memory implementation
    >
class unaligned_read_unaligned_write_memory
{
private:
    using element_index_t = index_t<WordSize>;

public:
    using addr_t = index_t<TotalSize>;
    using element_count_t = count_t<WordSize>;
    using word_t = T[WordSize];

    //| Read a word from a word aligned address
    inline word_t read_aligned(addr_t addr)
    {
        assert((addr & (WordSize - 1)) == 0);

        word_t value;

        static for(const auto i : WordSize)
        {
            value[i] = mem[i][addr / WordSize];
        }

        return value;
    }

    //| Read a word from an address aligned at `N` element boundary
    template <auto N = 1>
    inline word_t read(addr_t addr)
    {
        static assert((N & (N - 1)) == 0);
        static assert((WordSize / N) * N == WordSize);
        assert((addr & (N - 1)) == 0);

        word_t per_bank_value;

        // read per-channel data into local array
        static for(const auto i : WordSize)
        {
            const auto offset = checked_cast<element_index_t>((WordSize - i) - 1);

            per_bank_value[i] = mem[i][(addr + offset) / WordSize];
        }

        // rotate
        const auto rotate_amount = cast<index_t<WordSize / N>>(addr / N);
        return cast<word_t>(rotate_array_right<WordSize / N>(cast<T[WordSize / N][N]>(per_bank_value), rotate_amount));
    }

    //| Write a word to a word aligned address
    inline void write_aligned(addr_t addr, word_t value)
    {
        assert((addr & (WordSize - 1)) == 0);

        static for(const auto i : WordSize)
        {
            mem[i][addr / WordSize] = value[i];
        }
    }

    //| Write specified number of elements to an address aligned at `N` element boundary.
    // The `size` must be a multiple of `N` and no greater than `WordSize`.
    template <auto N = 1>
    inline void write(addr_t addr, word_t value, element_count_t size)
    {
        static assert((N & (N - 1)) == 0);
        static assert((WordSize / N)*N == WordSize);
        assert((addr & (N - 1)) == 0);
        assert((size & (N - 1)) == 0);

        // Attach write mask to data elements
        auto mask = mask_less_than<WordSize / N>(size / N);
        auto data_with_mask = zip_with(make_optional<T[N]>, mask, cast<T[WordSize / N][N]>(value));

        // Rotate both mask and data
        const auto rotate_amount = cast<index_t<WordSize / N>>(addr / N);
        const auto per_bank_data_with_mask = rotate_array_left<WordSize / N>(data_with_mask, rotate_amount);

        static for(const auto i : WordSize / N)
        {
            if (per_bank_data_with_mask[i].is_valid)
            {
                const auto offset = checked_cast<element_index_t>((WordSize - i * N) - 1);

                static for(const auto j : N)
                {
                    mem[i * N + j][(addr + offset) / WordSize] = per_bank_data_with_mask[i].value[j];
                }
            }
        }
    }

private:
    Memory<T, TotalSize/WordSize>[WordSize] mem;
}

//| Memory capable of aligned or unaligned reads of `ReadWordSize` elements per
// cycle and `WordSize` element writes aligned to `WordSize`.
template
    < typename T        //< Type of the memory element.
    , auto WordSize     //< Number of elements per word.
    , auto ReadWordSize //< Number of elements to be returned per read.
                        // Must be power-of-2 fraction or integer multiple of `WordSize`.
    , auto TotalSize    //< Total number of elements in memory.
                        // Memory width is `WordSize * bitsizeof(T)` bits.
                        // Memory depth is `TotalSize / WordSize` words.
    , template <typename, auto> typename Memory = memory
                        //< Memory implementation.
    >
class unaligned_read_aligned_write_memory
{
    // ReadWordSize <= WordSize / 2 :: 2 vertical banks, 1 horizontal bank
    // ReadWordSize <= WordSize     :: WordSize vertical banks, 1 horizontal bank
    // ReadWordSize > WordSize      :: 1 vertical bank, 1 << clog2(1 + ReadWordSize / WordSize) horizontal banks
    const auto NumVerticalBanks = (ReadWordSize <= WordSize / 2) ? 2 :
                                  (ReadWordSize <= WordSize) ? WordSize :
                                  1;
    const auto NumHorizontalBanks = (ReadWordSize <= WordSize) ? 1 :
                                    1 << clog2(1 + ReadWordSize / WordSize);
    const auto BankWordSize = WordSize / NumVerticalBanks;

    using element_index_t = index_t<WordSize>;

public:
    using addr_t = index_t<TotalSize>;
    using element_count_t = count_t<WordSize>;
    using word_t = T[WordSize];
    using read_word_t = T[ReadWordSize];

    //| Read a word from a word aligned address.
    inline read_word_t read_aligned(addr_t addr)
    {
        assert((addr & (WordSize - 1)) == 0);

        T[NumHorizontalBanks][NumVerticalBanks][BankWordSize] value;

        static for(const auto h : NumHorizontalBanks)
        {
            static for(const auto v : NumVerticalBanks)
            {
                value[h][v] = mem[h][v][addr / NumVerticalBanks];
            }
        }

        return reinterpret_cast<read_word_t>(value[addr / WordSize]);
    }

    //| Read a word from an unaligned address.
    inline read_word_t read(addr_t addr)
    {
        T[NumHorizontalBanks][NumVerticalBanks][BankWordSize] per_bank_value;

        static for(const auto h : NumHorizontalBanks)
        {
            const auto hoffset = (NumHorizontalBanks - h - 1) * WordSize;

            auto bank_addr = (addr + hoffset) / (NumHorizontalBanks * BankWordSize);

            // read per-bank data into local array
            static for(const auto v : NumVerticalBanks)
            {
                const auto voffset = (NumVerticalBanks - v) - 1;

                per_bank_value[h][v] = mem[h][v][(bank_addr + voffset) / NumVerticalBanks];
            }
        }

        // rotate
        const auto AllBanksWordSize = NumHorizontalBanks * WordSize;
        read_word_t value;
        if (AllBanksWordSize > 1)
        {
            auto per_bank_value_flattened = cast<T[AllBanksWordSize]>(per_bank_value);
            index_t<AllBanksWordSize> rotate_amount = static_cast(addr);
            value = rotate_array_right<ReadWordSize>(per_bank_value_flattened, rotate_amount);
        }
        else
        {
            value = reinterpret_cast<read_word_t>(per_bank_value);
        }
        return value;
    }

    //| Write a word to a word aligned address.
    inline void write_aligned(addr_t addr, word_t value)
    {
        assert((addr & (WordSize - 1)) == 0);

        auto per_bank_value = cast<T[NumVerticalBanks][BankWordSize]>(value);

        auto word_addr = addr / WordSize;
        auto mask = reinterpret_cast<bool[NumHorizontalBanks]>(1 << cast<index_t<NumHorizontalBanks>>(word_addr));

        static for(const auto h : NumHorizontalBanks)
        {
            if (NumHorizontalBanks == 1 || mask[h])
            {
                static for(const auto v : NumVerticalBanks)
                {
                    mem[h][v][word_addr / NumHorizontalBanks] = per_bank_value[v];
                }
            }
        }
    }

private:
    const auto BankDepth = TotalSize / NumHorizontalBanks / WordSize;
    Memory<T[BankWordSize], BankDepth>[NumHorizontalBanks][NumVerticalBanks] mem;
}
