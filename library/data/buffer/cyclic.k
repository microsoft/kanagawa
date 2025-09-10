// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.buffer.cyclic
    { cyclic_buffer
    , unaligned_cyclic_buffer
    }

import data.counter
import data.memory.unaligned
import numeric.int.operator.unsigned as unsigned

//| A cyclic buffer with the following functionality:
//
// - One data word can be written at a time until the buffer is full
// - Words can be freed to create new write space in a FIFO manner
// - Any valid element (written and not freed) can be read, even if not aligned to a
//   word boundary
template
    < typename T        //< Type of each element of the buffer.
    , auto WordSize     //< Number of elements per data word.
    , auto ReadWordSize //< Number of elements to be returned per read.
                        // Must be <= to WordSize.
    , auto TotalSize    //< Total size of memory in elements.
    , typename AddrType = uint64
                        //< Type to accumulate number of written and freed words.
                        // __Ensure this will not overflow between calls to reset__.
                        // Additionally, used to access and free buffer elements.
    >
class unaligned_cyclic_buffer
{
private:
    using unaligned_memory_t = unaligned_read_aligned_write_memory<T, WordSize, ReadWordSize, TotalSize>;
    unaligned_memory_t _cyclicBufferMem;
    const auto Depth = TotalSize / WordSize;
public:
    using addr_t = AddrType;
private:
    // Next entry address (in words) that will be written into cyclic buffer.
    addr_t _writeAddr = 0;
    // Next entry address (in words) to free/remove from cyclic buffer.
    addr_t _freeAddr = 0;

    // Number of free words available for writing
    counter<Depth, Depth> _freeCounter;

public:
    using word_t = unaligned_memory_t::word_t;
    using read_word_t = unaligned_memory_t::read_word_t;

    //| Reset cyclic buffer. This is not thread-safe and should not be called
    // while any other functions are running.
    void reset()
    {
        // Free any entries in the buffer
        atomic
        {
            auto freeCount = _freeCounter.count();
            _freeCounter.add(unsigned::sub(Depth, freeCount));
            // Ensure _freeCounter is full after reset
            assert(_freeCounter.count() == Depth);
        }

        _writeAddr = 0;
        _freeAddr = 0;
    }

    //| Write one word. Maybe block until there is space in the buffer.
    void write(T[WordSize] word)
    {
        // Wait for a free entry
        inline bool entry_available()
        {
            bool result = (_freeCounter.count() != 0);
            if (result)
            {
                _freeCounter.subtract(1);
            }
            return result;
        }
        atomic do; while(!entry_available());

        // Increment write pointer
        addr_t localWriteAddr;
        atomic
        {
            localWriteAddr = _writeAddr;
            _writeAddr++;
        }
        // Write to memory
        _cyclicBufferMem.write_aligned((localWriteAddr % Depth) * WordSize, word);
    }

    //| Free words up to, but not including, the specified address. Caller
    // should ensure that this address is valid.
    void free(addr_t addr /*< Element address.*/)
    {
        auto wordAddr = addr / WordSize;

        count_t<Depth> count;
        atomic
        {
            // Ensure not freeing past written entries
            assert(wordAddr <= _writeAddr);
            // Ensure free pointer is not already past requested address
            assert(wordAddr >= _freeAddr);

            // Determine number of entries to free
            count = checked_cast(wordAddr - _freeAddr);
            _freeAddr += count;
        }
        _freeCounter.add(count);
    }

    //| Block until a fully valid word has been written to the passed address.
    void wait(addr_t addr /*< Element address.*/)
    {
        auto wordAddr = addr / WordSize;
        if (WordSize > 1)
        {
            // Wait for the next word too if it spills over
            wordAddr += cast<uint1>(cast<index_t<WordSize>>(addr) > WordSize - ReadWordSize);
        }

        atomic do; while(!(_writeAddr > wordAddr));
    }

    //| Wait for the entry at `addr` to be written and then read the value.
    // The caller should ensure that this is not called for a free entry.
    read_word_t read(addr_t addr /*< Element address.*/)
    {
        // Block if entry has not been written
        wait(addr);

        return nonblocking_read(addr);
    }

    //| Read the value at the specified address. This version of read does not
    // block to wait for the entry to be written. It is also inline to reduce
    // the cost of multiple callsites. The caller should ensure that the
    // address is valid (written and not freed).
    inline read_word_t nonblocking_read(addr_t addr /*< Element address.*/)
    {
        auto wordAddr = addr / WordSize;

        // Check that read address has not been freed
        assert(wordAddr >= _freeAddr);
        // Check that the read address has been written
        // (Note: this assertion only checks the word from the first byte;
        //  unaligned accesses can spill into the next word which may not have
        //  been written)
        assert(_writeAddr > wordAddr);

        // Read value from memory
        return _cyclicBufferMem.read(addr % TotalSize);
    }
}

//| This class is a wrapper around a `unaligned_cyclic_buffer` with
// `WordSize == 1.`
template
    < typename T //< Type of each entry in the buffer.
    , auto Size  //< Maximum number of entries that can be stored.
    , typename AddrType = uint64
                 //< Type to accumulate number of written and freed words.
                 // __Ensure this will not overflow between calls to reset__.
                 // Additionally, used to access and free buffer elements.
    >
class cyclic_buffer
{
private:
    using unaligned_cyclic_buffer_t = unaligned_cyclic_buffer<T, 1, 1, Size, AddrType>;
    unaligned_cyclic_buffer_t _cyclicBuffer;
public:
    using addr_t = unaligned_cyclic_buffer_t::addr_t;

    //| Reset cyclic buffer. This is not thread-safe and should not be called
    // while any other functions are running.
    void reset()
    {
        _cyclicBuffer.reset();
    }

    //| Wait for a free entry and write one entry.
    void write(T val)
    {
        _cyclicBuffer.write({val});
    }

    //| Free entries up to the specified address. Caller should ensure that
    // this address is valid.
    void free(addr_t addr /*< Element address.*/)
    {
        _cyclicBuffer.free(addr);
    }

    //| Block until the passed address has been written to.
    void wait(addr_t addr /*< Element address.*/)
    {
        _cyclicBuffer.wait(addr);
    }

    //| Wait for the entry at `addr` to be written and then read the value.
    // The caller should ensure that this is not called for a free entry.
    T read(addr_t addr /*< Element address.*/)
    {
        auto value = _cyclicBuffer.read(addr);
        return value[0];
    }

    //| Read the value at the specified address. This version of read does not
    // block to wait for the entry to be written. It is also inline to reduce
    // the cost of multiple callsites. The caller should ensure that the
    // address is valid (written and not freed).
    inline T nonblocking_read(addr_t addr /*< Element address.*/)
    {
        auto value = _cyclicBuffer.nonblocking_read(addr);
        return value[0];
    }
}