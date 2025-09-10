// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.buffer.burst_write
import test.unit as unit
import test.runner

class BurstWriteBufferTests
{
private:
    const auto small_buffer_size = 4;
    const auto large_buffer_size = 8;
    const auto burst_size = 4;

    const auto MaxValue = 64;
    bool[MaxValue] _written;
    bool[MaxValue] _is_last;

    // Testing with a buffer that is/isn't the same size as burst_size to catch errors with type
    // sizes inside the buffer code.
    burst_write_buffer<uint32, small_buffer_size, burst_size> _buffer;
    burst_write_buffer<uint32, large_buffer_size, burst_size> _large_buffer;

    void WriteCallback(uint32 value, uint3 index, bool is_last)
    {
        if (is_last)
        {
            _is_last[value] = true;
        }

        _written[value] = true;
    }
    // If expected is true, this function will block, perhaps forever, until
    // the given value is written back.
    void CheckWriteStatus(unit::tag_t tag, uint<clog2(MaxValue)> idx, bool expected)
    {
        if (expected)
        {
            // If idx never gets written, the test will hang here:
            wait([idx]()
            {
                return _written[idx];
            });
        }
        else
        {
            if (_written[idx])
            {
                print("Error: value {idx}  was written, but should not have been.\n");
                unit::assert(tag, false);
            }
        }
    }

    // If expected is true, this function will block, perhaps forever, until
    // the given value is written back and marked as the last value.
    void CheckIsLast(unit::tag_t tag, uint<clog2(MaxValue)> idx, bool expected)
    {
        if (expected)
        {
            // If idx never gets written and marked as last, the test will hang here:
            wait([idx]()
            {
                return _is_last[idx];
            });
        }
        else
        {
            if (_is_last[idx])
            {
                print("Error: value {idx}  was marked as last, but should not have been.\n");
                unit::assert(tag, false);
            }
        }
    }

    inline void write_helper(uint32 thread_index)
    {
        pipelined_for(thread_index, [](uint32 tid)
        {
            // Adding 32 to skip past values used by other tests.
            _buffer.write(checked_cast(tid + 32), false, WriteCallback);
        });
    }

public:
    void BasicTest(unit::tag_t tag)
    {
        _written = {};
        _is_last = {};

        // Write 5 values to a buffer with a burst size of 4.
        range_for (7, 12, 1, [tag](uint8 i)
        {
            _buffer.write(i, false, WriteCallback);
        });

        // Values 7 - 10 (inclusive) should have been written back in a batch of 4,
        // with 10 (and only 10) marked as last.
        range_for (7, 11, 1, [tag](auto i)
        {
            CheckWriteStatus(tag, i, true);
            CheckIsLast(tag, i, i == 10);
        });

        // 11 should not have been written back yet, as it was the 5th write.
        CheckWriteStatus(tag, 11, false);

        auto count = _buffer.count();
        if (count != 1)
        {
            print("Buffer count should be 1 now, actual {count}\n");
            unit::assert(tag, false);
        }

        // Write 3 more values to clear out the buffer.
        range_for (12, 15, 1, [tag](uint8 i)
        {
            _buffer.write(i, false, WriteCallback);
        });

        count = _buffer.count();
        if (count != 0)
        {
            print("Buffer count should be 0 now, actual {count}\n");
            unit::assert(tag, false);
        }

        range_for (7, 15, 1, [tag](auto i)
        {
            CheckWriteStatus(tag, i, true);

            // The values 10 and 14 were the last values in their respective bursts.
            bool expect_last = (i == 10 || i == 14);

            CheckIsLast(tag, i, expect_last);
        });
    }

    void EarlyFlushTest(unit::tag_t tag)
    {
        _written = {};
        _is_last = {};

        // Write 3 values, flush on the third one - before reaching burst size.
        _large_buffer.write(20, false, WriteCallback);
        _large_buffer.write(21, false, WriteCallback);
        _large_buffer.write(22, true, WriteCallback);

        // Values 20 - 22 (inclusive) should have been written back in a batch of 3,
        // with 22 (and only 22) marked as last.
        range_for (20, 23, 1, [tag](auto i)
        {
            CheckWriteStatus(tag, i, true);
            CheckIsLast(tag, i, i == 22);
        });

        // The buffer should be empty now, since we just flushed.
        auto count = _large_buffer.count();
        if (count != 0)
        {
            print("Buffer count should be 0 now, actual {count}\n");
            unit::assert(tag, false);
        }

        // Write 4 more values to ensure the manual flush didn't interfere with normal
        // callbacks when we reach the burst size.
        range_for (23, 27, 1, [tag](auto i)
        {
            _large_buffer.write(i, false, WriteCallback);
        });

        range_for (20, 27, 1, [tag](auto i)
        {
            CheckWriteStatus(tag, i, true);

            // The values 22 and 26 were the last values in their respective bursts.
            bool expect_last = (i == 22 || i == 26);

            CheckIsLast(tag, i, expect_last);
        });

        // The burst write buffer code cycles around its 8 element buffer as writes and callbacks
        // occur.  We are now in an interesting state - we flushed after 3 writes, then 4 writes,
        // so the next batch of 4 will wrap around the end of the 8 element internal buffer.  Do
        // 4 more writes here to validate that the math inside burst write buffer handles the
        // wraparound correctly.

        range_for (27, 31, 1, [tag](auto i)
        {
            _large_buffer.write(i, false, WriteCallback);
        });

        range_for (20, 31, 1, [tag](auto i)
        {
            CheckWriteStatus(tag, i, true);

            // The values 22, 26, and 30 were the last values in their respective bursts.
            bool expect_last = (i == 22 || i == 26 || i == 30);

            CheckIsLast(tag, i, expect_last);
        });
    }

    void ConcurrentTest(unit::tag_t tag)
    {
        _written = {};
        _is_last = {};

        write_helper(8);

        range_for (32, 40, 1, [tag](auto i)
        {
            CheckWriteStatus(tag, i, true);

            // The values 35 and 39 were the last values in their respective bursts.
            bool expect_last = (i == 35 || i == 39);

            CheckIsLast(tag, i, expect_last);
        });
    }

    void Run()
    {
        unit::test<1>(BasicTest);
        unit::test<2>(EarlyFlushTest);
        unit::test<3>(ConcurrentTest);
    }
}

inline void test_main()
{
    static BurstWriteBufferTests _tests;

    _tests.Run();
}
