// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.buffer.cyclic
import test.unit as unit
import test.runner

class DataBufferCyclicTest
{
private:
    const auto Size = 128;
    using T = uint64;
    using cyclic_buffer_t = cyclic_buffer<T, Size>;

    cyclic_buffer_t cyclicBuffer;

    cyclic_buffer_t::addr_t writeAddr = 0;
    cyclic_buffer_t::addr_t readStart = 0;

    unaligned_cyclic_buffer<uint8, 4, 2, Size> unalignedCyclicBuffer;
    bool unalignedCyclicBufferReadStarted;
    bool unalignedCyclicBufferReadReturned;

    [[pipelined]] void write(uint16 count)
    {
        cyclic_buffer_t::addr_t localWriteAddr;
        atomic
        {
            localWriteAddr = writeAddr;
            writeAddr++;
        }
        cyclicBuffer.write(localWriteAddr);
    }

    [[pipelined]] void read_and_check(uint16 tid, cyclic_buffer_t::addr_t startAddr)
    {
        T val = cyclicBuffer.read(checked_cast(startAddr + tid));
        assert(val == startAddr + tid);
    }

    [[pipelined]] void nonblocking_read_and_check(uint16 tid, cyclic_buffer_t::addr_t startAddr)
    {
        T val = cyclicBuffer.nonblocking_read(checked_cast(startAddr + tid));
        assert(val == startAddr + tid);
    }

    void free(uint16 count)
    {
        readStart += count;
        cyclicBuffer.free(readStart);
    }

    [[async]] void async_write(uint16 count)
    {
        write(count);
    }

    bool check_unalignedCyclicBufferReadReturned()
    {
        // Spin a while to make sure it is and stays blocked
        pipelined_for(100,
            [](index_t<100> tid)
            {
                assert(!unalignedCyclicBufferReadReturned);
            }
        );
        return unalignedCyclicBufferReadReturned;
    }

public:
    void run(unit::tag_t tag)
    {
        uint32 result = 0;

        cyclicBuffer.reset();
        // Write 1
        write(1);
        // Read 1
        read_and_check(1, readStart);

        // Write multiple
        write(15);
        read_and_check(16, readStart);

        // Write full
        write(Size - 16);
        read_and_check(Size, readStart);
        // Read from middle
        read_and_check(Size - Size / 2, Size / 2);
        // Free entire buffer
        free(Size);

        // Fill buffer again
        write(Size);
        read_and_check(Size, readStart);
        free(Size);

        // async write a large amount
        async_write(8 * Size);
        // Alternate read and free entire buffer
        for (const auto i : 8)
        {
            read_and_check(Size, readStart);
            free(Size);
        }

        // async write a large amount
        async_write(8 * Size);
        // Alternate read and free half of buffer
        for (const auto i : 16)
        {
            read_and_check(Size / 2, readStart);
            free(Size / 2);
        }

        // async write a large amount
        async_write(8 * Size);
        // Read using wait and nonblocking reads
        for (const auto i : 8)
        {
            cyclicBuffer.wait(checked_cast(readStart + Size - 1));
            nonblocking_read_and_check(Size, readStart);
            free(Size);
        }

        // Check unaligned reads not blocked on any writes
        {
            uint32[2] input = { 0x7654321, 0xFEDCBA98 };
            auto input64 = cast<uint64>(input);
            unalignedCyclicBuffer.write(cast<uint8[4]>(input[0]));
            unalignedCyclicBuffer.write(cast<uint8[4]>(input[1]));
            for (const auto i : 5)
            {
                auto lhs = cast<uint16>(unalignedCyclicBuffer.read(i));
                auto rhs = cast<uint16>(input64 >> (i * 8));
                assert(lhs == rhs);
                result += cast<uint1>(lhs != rhs);
            }
        }

        // Check unaligned reads blocked on one write
        {
            unalignedCyclicBufferReadStarted = false;
            async_exec(
                []
                {
                    unalignedCyclicBufferReadReturned = false;
                    unalignedCyclicBufferReadStarted = true;
                    auto lhs = cast<uint16>(unalignedCyclicBuffer.read(7));
                    assert(lhs == 0xEFFE);
                    lhs = cast<uint16>(unalignedCyclicBuffer.read(9));
                    assert(lhs == 0xABCD);
                    unalignedCyclicBufferReadReturned = true;
                }
            );
            wait([](){return unalignedCyclicBufferReadStarted;});
            result += cast<uint1>(check_unalignedCyclicBufferReadReturned());
            unalignedCyclicBuffer.write(reinterpret_cast<uint8[4]>(0x89ABCDEF));
            wait([](){return unalignedCyclicBufferReadReturned;});
        }

        // Check unaligned reads blocked on two writes
        {
            unalignedCyclicBufferReadStarted = false;
            async_exec(
                []
                {
                    unalignedCyclicBufferReadReturned = false;
                    unalignedCyclicBufferReadStarted = true;
                    auto lhs = cast<uint16>(unalignedCyclicBuffer.read(14));
                    assert(lhs == 0xAAAA);
                    lhs = cast<uint16>(unalignedCyclicBuffer.read(16));
                    assert(lhs == 0xBBBB);
                    unalignedCyclicBufferReadReturned = true;
                }
            );
            wait([](){return unalignedCyclicBufferReadStarted;});
            result += cast<uint1>(check_unalignedCyclicBufferReadReturned());
            unalignedCyclicBuffer.write(reinterpret_cast<uint8[4]>(0xAAAAAAAA));
            result += cast<uint1>(check_unalignedCyclicBufferReadReturned());
            unalignedCyclicBuffer.write(reinterpret_cast<uint8[4]>(0xBBBBBBBB));
            wait([](){return unalignedCyclicBufferReadReturned;});
        }

        unit::assert_equal(tag, true, result == 0);
    }
}

inline void test_main()
{
    print("data.buffer.cyclic test start\n");

    unit::test<1>(unit::fixture<DataBufferCyclicTest>());

    print("data.buffer.cyclic test complete\n");
}
