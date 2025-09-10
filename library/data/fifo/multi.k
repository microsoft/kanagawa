// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.fifo.multi
    { multi_fifo
    }

import sync.lock.multi
import numeric.int.operator.modular as modular

//| This class implements a set of FIFOs that use a single, statically partitioned block of memory.
// Note that when using the blocking version, a call for one of the FIFO instances that blocks will also
// block subsequent calls for other FIFO instances, even if those calls would otherwise not block.
// In other words, there is head of line blocking across FIFOs.
template
    < typename T                  //< Type of each entry in the FIFO.
    , auto Size                   //< Maximum number of entries that can be stored in each FIFO.
    , auto N                      //< Number of FIFOs.
    , bool EnqueueBlocking = true //< Block on enqueue if the FIFO is full until an entry
                                  // frees up. By default this is true. Otherwise,
                                  // the caller must ensure that the FIFO is not full.
    , bool DequeueBlocking = true //< Block on dequeue if the FIFO is empty until an entry
                                  // arrives. By default, this is true. Otherwise,
                                  // the caller must ensure that the FIFO is not empty.
    >
class multi_fifo
{
public:
    using pointer_t = index_t<Size>;
    using fifo_idx_t = index_t<N>;

private:
    pointer_t[N] _producerIndex = {};
    pointer_t[N] _consumerIndex = {};

    // Memory that holds the raw data
    memory<T, N*Size> _dataMem;

    // Used to block reads while fifo is empty
    multi_semaphore<N, Size, 0, DequeueBlocking> _readSemaphore;

    // Used to block writes while the fifo is full
    multi_semaphore<N, Size, Size, EnqueueBlocking> _writeSemaphore;

public:
    //| Return the number of elements that have been written and not read.
    // Note that this can be out of date the instant it is read due to other
    // threads reading or writing to the FIFO. This cannot be used for checking
    // empty or full for non-blocking versions of the FIFO.
    inline count_t<Size> count(fifo_idx_t which)
    {
        return _readSemaphore.count(which);
    }

    //| Read one entry from a FIFO. Block if the FIFO is empty and `DequeueBlocking` is true.
    // If `pop` is false then the entry is left on the FIFO, equivalent to `front`.
    T dequeue(fifo_idx_t which, bool pop)
    {
        // Block the calling thread until an entry is available
        _readSemaphore.wait(which, pop);

        // Get the value of the read pointer, and optionally increment the read pointer
        pointer_t snappedConsumerIndex;

        atomic
        {
            snappedConsumerIndex = _consumerIndex[which];

            _consumerIndex[which] = modular::increment_if(snappedConsumerIndex, pop);
        }

        // Read the value from the data memory
        T result = _dataMem[Size * which + snappedConsumerIndex];

        // Now that the read has occured, allow another thread to overwrite the data
        if (pop)
        {
            _writeSemaphore.post(which);
        }

        return result;
    }

    //| Write one entry to a FIFO. Block if the FIFO is full and `EnqueueBlocking` is true.
    void enqueue(fifo_idx_t which, T value)
    {
        // Block until the FIFO is no longer full
        _writeSemaphore.wait(which, true);

        // Get the value of the write pointer, and increment the write pointer
        pointer_t snappedProducerIndex;

        atomic
        {
            snappedProducerIndex = _producerIndex[which];
            _producerIndex[which] = static_cast(snappedProducerIndex + 1);
        }

        // Store the value into the memory
        _dataMem[Size * which + snappedProducerIndex] = value;

        // Allow 1 more thread to get a value out
        _readSemaphore.post(which);
    }
}