// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Memory supporting byte level access and addressing.
*/

module data.memory.byte_addressable
    { byte_addressable_memory
    }

import data.memory.unaligned

//| Memory that logically stores words of type `T` but allows addressing and
// read and write access at byte granularity.
template
    < typename T                                            //< Type of memory word
    , auto DepthInBytes                                     //< Depth of memory in bytes
    , template <typename, auto> typename Memory = memory    //< Memory implementation
    >
class byte_addressable_memory
{
    const auto BytesPerWord = bytesizeof T;
    using mem_t = unaligned_read_unaligned_write_memory<uint8, BytesPerWord, DepthInBytes, Memory>;

public:
    using addr_t = mem_t::addr_t;
    using byte_count_t = mem_t::element_count_t;
    using word_t = T;

    //| Read a word from a word aligned address
    inline word_t read_aligned(addr_t addr)
    {
        return cast<word_t>(mem.read_aligned(addr));
    }

    //| Read a word from N-byte aligned address
    template <auto N = 1>
    inline word_t read(addr_t addr)
    {
        return cast<word_t>(mem.read<N>(addr));
    }

    //| Write a word to a word aligned address
    inline void write_aligned(addr_t addr, word_t word)
    {
        mem.write_aligned(addr, cast<uint8[BytesPerWord]>(word));
    }

    //| Write specified number of bytes to an N-byte unaligned address.
    // The `size` must be a multiple of `N` and no greater than `bytesizeof T`.
    template <auto N = 1>
    inline void write(addr_t addr, word_t value, byte_count_t size)
    {
        mem.write<N>(addr, cast<uint8[BytesPerWord]>(value), size);
    }

private:
    mem_t mem;
}
