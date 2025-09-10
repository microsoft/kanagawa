// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Wide to narrow fifo
*/

module data.fifo.wide_to_narrow
    { fifo
    }

import numeric.int.operator
import sync.lock

//| A FIFO that accepts arrays of inputs, and returns single values
template
    < typename T                    //< Datatype for each entry of the FIFO.
    , auto ArraySize                //< Number of elements in input arrays, must be a power of 2.
    , auto ArrayDepth               //< Maximum number of arrays that can be stored, must be a power of 2.
    , bool EnqueueBlocking = true   //< Block on enqueue if the FIFO is full until an entry
                                    //  frees up. By default this is true. Otherwise,
                                    //  the caller must ensure that the FIFO is not full.
    , bool DequeueBlocking = true   //< Block on dequeue if the FIFO is empty until an entry
                                    //  arrives. By default, this is true. Otherwise,
                                    //  the caller must ensure that the FIFO is not empty.
    >
class fifo
{
public:
    //| ArraySize must be a power of 2
    static assert(0 == (ArraySize & (ArraySize - 1)));

    //| ArrayDepth must be a power of 2
    static assert(0 == (ArrayDepth & (ArrayDepth - 1)));

    using array_t = T[ArraySize];

private:
    //| The number of scalar elements the FIFO can hold
    const auto ScalarDepth = ArrayDepth * ArraySize;

    using array_pointer_t = index_t<ArrayDepth>;
    using scalar_pointer_t = index_t<ScalarDepth>;

    // Memory that holds the raw data
    memory<array_t, ArrayDepth> _data_mem;

    // Used to block reads while fifo is empty
    semaphore<ScalarDepth, 0, DequeueBlocking> _read_semaphore;

    // Used to block writes while the fifo is full
    semaphore<ScalarDepth, ScalarDepth, EnqueueBlocking> _write_semaphore;

public:
    //| Read one entry from FIFO and discard `discard_count` subsequent entries.
    //  Block if FIFO is empty and `DequeueBlocking` is true.
    T dequeue_and_discard(index_t<ArraySize> discard_count)
    {
        count_t<ArraySize> total_count = discard_count + 1;

        // Block the calling thread until an entry is available
        _read_semaphore.wait_multiple(total_count);

        // Get the value of the read pointer, and increment the read pointer
        scalar_pointer_t snapped_consumer_index;

        atomic
        {
            static scalar_pointer_t _consumer_index = 0;
            snapped_consumer_index = _consumer_index;
            _consumer_index = static_cast(_consumer_index + total_count);
        }

        // Decompose snapped_consumer_index into
        // 1) index into _data_mem
        // 2) index into resulting array
        auto div_mod_result = div_mod(snapped_consumer_index, ArraySize);

        // Read an array from the memory
        array_t results = _data_mem[div_mod_result.first];

        // Select 1 element from the array
        T result = results[div_mod_result.second];

        // Now that the read has occured, allow another thread to overwrite the data
        _write_semaphore.post_multiple(total_count);

        return result;
    }

    //| Read one entry from FIFO. Block if FIFO is empty and `DequeueBlocking` is true.
    T dequeue()
    {
        return dequeue_and_discard(0);
    }

    //| Write one entry to the FIFO. Block if FIFO is full and `EnqueueBlocking` is true.
    void enqueue(array_t values)
    {
        // Block until there is room for an entire array
        _write_semaphore.wait_multiple(ArraySize);

        // Get the value of the write pointer, and increment the write pointer
        array_pointer_t snapped_producer_index;

        atomic
        {
            static array_pointer_t _producer_index = 0;
            snapped_producer_index = _producer_index;
            _producer_index = static_cast(_producer_index + 1);
        }

        // Store the values into the memory
        _data_mem[snapped_producer_index] = values;

        // Allow ArraySize more calls to dequeue
        _read_semaphore.post_multiple(ArraySize);
    }
}
