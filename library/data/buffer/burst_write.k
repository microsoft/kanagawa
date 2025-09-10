// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.buffer.burst_write
    { burst_write_buffer
    }

import sync.lock

//| Buffer that accumulates a given number of entries, and then invokes
// a callback one or more times to write those values to e.g. DRAM in a burst.
template
    < typename T                  //< Type of each entry of the buffer
    , auto BufferSize             //< Number of elements to buffer.  Must be a
                                  // power of 2.
    , auto BurstSize = BufferSize //< Number of elements to store before
                                  // invoking the `write_callbck`.  Must be a
                                  // power of 2 and `BurstSize` <= `BufferSize`.
    >
class burst_write_buffer
{
public:
    static assert(0 == (BufferSize & (BufferSize - 1))); //< `BufferSize` must be a power of 2.
    static assert(0 == (BurstSize & (BurstSize - 1)));   //< `BurstSize` must be a power of 2.
    static assert(BurstSize <= BufferSize);

    using buffer_count_t = count_t<BufferSize>;
    using burst_count_t = count_t<BurstSize>;

private:
    using pointer_t = index_t<BufferSize>;
    pointer_t _producer_index = 0;
    pointer_t _consumer_index = 0;

    // Memory that holds the values waiting to be written back
    memory<T, BufferSize> _data;

    // Used to block writes while the buffer is full
    semaphore<BufferSize, BufferSize, true> _write_semaphore;

    // Function: invoke_callback
    //
    // Invokes the write callback once and posts to the write semaphore.
    // thread_index   - Which thread this is, in the range 1 through (callback_count - 1).
    // data_offset    - The offset within the _data memory for the first value being passed
    //                  to the callback in this burst.
    // callback_count - How many callbacks will be invoked in this burst.  Used to determine
    //                  whether pass is_last to the callback.
    // write_callback - The function to call when BurstSize entries have been written.
    //                  See write function for parameter descriptions and usage.
    inline void invoke_callback(burst_count_t thread_index, pointer_t data_offset, burst_count_t callback_count, (T, burst_count_t, bool) -> void write_callback)
    {
        pipelined_for(callback_count, [data_offset, callback_count, write_callback](index_t<BurstSize> thread_index)
        {
            pointer_t data_index = static_cast(data_offset + thread_index);

            write_callback(_data[data_index], thread_index, thread_index == (callback_count - 1));
            _write_semaphore.post();
        });
    }

public:
    //| Return the number of elements that have been written and not yet sent to the callback.
    // Note that this can be out of date the instant it is read due to other threads writing
    // to the buffer.
    inline buffer_count_t count()
    {
        buffer_count_t result;
        atomic
        {
            result = _producer_index - _consumer_index;
        }

        return result;
    }

    //| Write one entry to the buffer.  May invoke the `write_callback` function.
    inline void write
        (T value                   //< The value to write.
        , bool flush_now           //< If true, the callback will be invoked
                                   // for any buffered values, even if there
                                   // are fewer than `BurstSize` values buffered.
        , ( T value                //< The value to write in the callback.
          , burst_count_t index    //< The index of this callback within the burst.
                                   // Between 0 and `BurstSize` - 1 inclusive.
          , bool is_last           //< True if this is the last value within this
                                   // burst, false otherwise.
          ) -> void write_callback //< The function to call when `BurstSize`
                                   // entires have been written.
        )
    {
        // Block until the buffer is no longer full.
        _write_semaphore.wait();

        pointer_t snapped_producer_index;
        pointer_t snapped_consumer_index;
        count_t<BurstSize> callback_count = 0;

        // Get the values of the read and write pointers, increment the write pointer.
        atomic
        {
            snapped_consumer_index = _consumer_index;
            snapped_producer_index = _producer_index;
            _producer_index = static_cast(_producer_index + 1);

            // Note that entries_before_this is deliberately too small to hold a count.  When
            // the producer index wraps around 0 before the consumer index, we rely on this
            // calculation overflowing to produce the correct result.
            pointer_t entries_before_this = static_cast(snapped_producer_index - snapped_consumer_index);

            if (flush_now || (entries_before_this == (BurstSize - 1)))
            {
                // Adding 1 to reflect what the count will be after writing the current value.
                callback_count = entries_before_this + 1;

                // By setting the consumer index to the producer index, this thread is
                // "reserving" these values to pass to the callback.  Note that the space
                // is not available in the buffer yet, because the code below has not yet
                // passed the value(s) to the callback.  The _write_semaphore tracks how
                // much room is available in the buffer, and it will be updated after the
                // callback is invoked.
                _consumer_index = _producer_index;
            }
        }

        // Store the value into the memory.
        _data[snapped_producer_index] = value;

        // Invoke the callback if necessary.
        invoke_callback(callback_count, snapped_consumer_index, callback_count, write_callback);
    }
}