// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Transposing fifo
*/

module data.fifo.transpose
    { fifo
    }

import sync.lock
import sync.atomic
import data.array
import numeric.int.operator

//| A FIFO that accepts rows of type `T[ArraySize]`,
//  transposes each square group of `ArraySize x ArraySize` elements,
//  and returns columns of type `T[ArraySize]`
template
    < typename T                    //< Datatype for each array element
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

    //| Input and output array type
    using array_t = T[ArraySize];

private:
    using array_pointer_t = index_t<ArrayDepth>;

    // Memories that hold raw data
    memory<T, ArrayDepth>[ArraySize] _data_memories;

    // Used to block reads while fifo is empty
    semaphore<ArrayDepth, 0, DequeueBlocking> _read_semaphore;

    // Used to block writes while the fifo is full
    semaphore<ArrayDepth, ArrayDepth, EnqueueBlocking> _write_semaphore;

    // Index of a row/column within the containing square
    using index_within_square_t = index_t<ArraySize>;

    struct decomposed_addr
    {
        // Raw count of number of rows/columns seen so far
        array_pointer_t array_index;

        // Base address of the containing square within the memories
        array_pointer_t square_base;

        // Index of the row/column within the containing square
        index_within_square_t index_within_square;
    }

    inline decomposed_addr increment_address(count_t<ArraySize> array_count)
    {
        // Add to the counter
        auto snapped_index = first(atomically([array_count](array_pointer_t prev)
        {
            return cast<array_pointer_t>(prev + array_count);
        }));

        // Decompose
        decomposed_addr result;

        auto div_mod_result = div_mod(snapped_index, ArraySize);

        result.array_index = snapped_index;
        result.square_base = div_mod_result.first * ArraySize;
        result.index_within_square = div_mod_result.second;

        return result;
    }

public:
    //| Read one column from FIFO and discard `discard_column_count` subsequent columns from the same square.
    //  Block if FIFO is empty and `DequeueBlocking` is true.
    array_t dequeue_and_discard(index_t<ArraySize> discard_column_count)
    {
        count_t<ArraySize> total_column_count = discard_column_count + 1;

        // Block the calling thread until the containing square is available
        _read_semaphore.wait_multiple(total_column_count);

        // Get the value of the read pointer, and increment the read pointer
        decomposed_addr consumer_index = increment_address(total_column_count);

        // Read one element from each memory
        array_t result;

        static for (const auto i : ArraySize)
        {
           result[i] = _data_memories[i][consumer_index.array_index];
        }

        // The memory addressing and muxing that occurs during a write gets the data
        // in almost the desired form.  Each element of the output array is in 1 memory
        // but output element 'i' might be in memory 'j'.  A rotation addresses this.
        result = rotate_array_right<ArraySize>(result, consumer_index.index_within_square);

        auto array_end = consumer_index.index_within_square + total_column_count;

        // discard_count can only be used to discard values within the same square
        assert(array_end <= ArraySize);

        if (array_end == ArraySize)
        {
            // An entire square has been dequeued
            _write_semaphore.post_multiple(ArraySize);
        }

        return result;
    }

    //| Read one column from FIFO. Block if FIFO is empty and `DequeueBlocking` is true.
    array_t dequeue()
    {
        return dequeue_and_discard(0);
    }

    //| Write `1 + discard_row_count` rows into the FIFO.  Block if FIFO is full and `EnqueueBlocking` is true.
    void enqueue_and_discard
        ( array_t values                        //< The value of the first row to write into the FIFO.
        , index_t<ArraySize> discard_row_count  //< The number of additional rows (with undefined content) to write into the FIFO.
        )
    {
        count_t<ArraySize> total_row_count = 1 + discard_row_count;

        // Block until there is room for `total_row_count` more arrays
        _write_semaphore.wait_multiple(total_row_count);

        // Get the value of the write pointer, and increment the write pointer
        decomposed_addr producer_index = increment_address(total_row_count);

        // Store the values into the memories
        static for (const auto i : ArraySize)
        {
            index_t<ArraySize> select = static_cast(i - producer_index.index_within_square);

            _data_memories[i][producer_index.square_base + select] = values[select];
        }

        auto new_row_count = producer_index.index_within_square + total_row_count;
        assert(new_row_count <= ArraySize);

        if (new_row_count == ArraySize)
        {
            // Allow an entire square to be dequeued
            _read_semaphore.post_multiple(ArraySize);
        }
    }

    //| Write one row into the FIFO. Block if FIFO is full and `EnqueueBlocking` is true.
    void enqueue(array_t values)
    {
        enqueue_and_discard(values, 0);
    }
}
