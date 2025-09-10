// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.fifo
    { FIFO
    , FIFO_store_and_forward
    , fifo_nb
    }

import data.counter
import data.memory
import numeric.int.operator
import sync.atomic
import sync.lock
import type.coerce
import type.stdtype

//| Basic FIFO.
template
    < typename T                  //< Type for each entry of the FIFO.
    , auto Size                   //< Maximum number of entries that can be stored.
    , bool EnqueueBlocking = true //< Block on enqueue if the FIFO is full until an entry
                                  // frees up. By default this is true. Otherwise,
                                  // the caller must ensure that the FIFO is not full.
    , bool DequeueBlocking = true //< Block on dequeue if the FIFO is empty until an entry
                                  // arrives. By default, this is true. Otherwise,
                                  // the caller must ensure that the FIFO is not empty.
    >
class FIFO
{
public:
    //| `Size` must be a power of 2.
    static assert(0 == (Size & (Size - 1)));

private:
    using pointer_t = index_t<Size>;

    // Memory that holds the raw data
    memory<T, Size> _dataMem;

    // Used to block reads while fifo is empty
    semaphore<Size, 0, DequeueBlocking> _readSemaphore;

    // Used to block writes while the fifo is full
    semaphore<Size, Size, EnqueueBlocking> _writeSemaphore;

public:
    //| Return the number of elements that have been written and not read.
    // Note that this can be out of date the instant its read due to other
    // threads reading/writing to the FIFO. This cannot be used for checking
    // empty/full for non-blocking versions of the FIFO.
    inline count_t<Size> count()
    {
        return _readSemaphore.count();
    }

    //| Read one entry from FIFO. Block if FIFO is empty and `DequeueBlocking` is true.
    T dequeue()
    {
        // Block the calling thread until an entry is available
        _readSemaphore.wait();

        // Get the value of the read pointer then increment the read pointer
        auto consumer_index = first(atomically<pointer_t>(increment));

        // Read the value from the data memory
        T result = _dataMem[consumer_index];

        // Now that the read has occured, allow another thread to overwrite the data
        _writeSemaphore.post();

        return result;
    }

    //| Write one entry to the FIFO. Block if FIFO is full and `EnqueueBlocking` is true.
    void enqueue(T value)
    {
        // Block until the FIFO is no longer full
        _writeSemaphore.wait();

        // Get the value of the write pointer then increment the write pointer
        auto producer_index = first(atomically<pointer_t>(increment));

        // Store the value into the memory
        _dataMem[producer_index] = value;

        // Allow 1 more thread to get a value out
        _readSemaphore.post();
    }
}

//| Store and forward FIFO.
template
    < typename T                  //< Type for each entry of the FIFO.
    , auto Size                   //< Maximum number of entries that can be stored.
    , bool DequeueBlocking = true //< Block on dequeue if the FIFO is empty until an entry
                                  // arrives. By default, this is true. Otherwise,
                                  // the caller must ensure that the FIFO is not empty.
    >
class FIFO_store_and_forward
{
public:
    //| `Size` must be a power of 2.
    static assert(0 == (Size & (Size - 1)));

private:
    using pointer_t = index_t<Size>;

    // Stores how many potential dequeue operations are currently being deferred until
    // the next enqueue(..., true) call.
    count_t<Size> _deferredDequeueCount = 0;

    // Memory that holds the raw data
    memory<T, Size> _dataMem;

    // Used to block reads while fifo is empty
    semaphore<Size, 0, DequeueBlocking> _readSemaphore;

    // Used to block writes while the fifo is full
    semaphore<Size, Size, true> _writeSemaphore;

public:
    //| Return the number of elements that have been written and not read.
    // Note that this can be out of date the instant its read due to other
    // threads reading/writing to the FIFO. This cannot be used for checking
    // empty/full for non-blocking versions of the FIFO.
    inline count_t<Size> count()
    {
        return _readSemaphore.count();
    }

    //| Read one entry from FIFO. Block if FIFO is empty and `DequeueBlocking` is true.
    T dequeue()
    {
        // Block the calling thread until an entry is available
        _readSemaphore.wait();

        // Get the value of the read pointer then increment the read pointer
        auto consumer_index = first(atomically<pointer_t>(increment));

        // Read the value from the data memory
        T result = _dataMem[consumer_index];

        // Now that the read has occured, allow another thread to overwrite the data
        _writeSemaphore.post();

        return result;
    }

    //| Write one entry to the FIFO. Block if FIFO is full.
    // Return the number of dequeue calls that were released.
    count_t<Size> enqueue(T value, bool releaseDeferredDequeues)
    {
        // Block until the FIFO is no longer full
        _writeSemaphore.wait();

        // Get the value of the write pointer then increment the write pointer
        auto producer_index = first(atomically<pointer_t>(increment));

        // Store the value into the memory
        _dataMem[producer_index] = value;

        count_t<Size> snappedDeferredDequeueCount;
        atomic
        {
            if (releaseDeferredDequeues)
            {
                // Add one to the deferred dequeue count for the value being enqueued now.
                snappedDeferredDequeueCount = _deferredDequeueCount + 1;
                _deferredDequeueCount = 0;
            }
            else
            {
                // Ensure that the increment of _deferredDequeueCount below won't overflow.
                assert(_deferredDequeueCount < (1 << bitsizeof(count_t<Size>)) - 1);

                snappedDeferredDequeueCount = 0;
                _deferredDequeueCount++;
            }
        }

        // Allow 0 or more threads to to get a value out
        _readSemaphore.post_multiple(snappedDeferredDequeueCount);

        return snappedDeferredDequeueCount;
    }
}

//| A FIFO that enables polling read/write operations
// and conditionally reading an element based on the value of that element.
template
    < typename T //< Type for each entry of the FIFO.
    , auto Size  //< Maximum number of entries that can be stored.
    >
class fifo_nb
{
public:
    //| `Size` must be a power of 2.
    static assert(0 == (Size & (Size - 1)));

private:
    using pointer_t = index_t<Size>;
    pointer_t _consumerIndex = 0;

    // Memory that holds the raw data
    memory<T, Size> _dataMem;

    // Tracks number of elements in the queue
    // Number elements in queue + num reserved elements
    counter<Size, 0> _earlyCounter;

    // Number of elements in the queue
    counter<Size, 0> _lateCounter;

public:
    // Producer interface

    //| Return true if no more entries can be enqueued.
    // Typical usage is to call `full` and `reserve_enqueue` in the same atomic block.
    inline bool full()
    {
        return _earlyCounter.count() == Size;
    }

    //| Reserve one slot, so that data can be safely written into the FIFO at a future point in time.
    // Typical usage is to call `full` and `reserve_enqueue` in the same atomic block.
    // Each call to `reserve_enqueue` must be paired with 1 call to `enqueue`.
    // The `enqueue` call does not need to be in an atomic block with the call to `reserve_enqueue`.
    inline void reserve_enqueue()
    {
        assert(!full());
        _earlyCounter.add(1);
    }

    //| Helper function that checks fifo state and conditionally reserves a slot.
    // If the fifo is not full, then this reserves one slot and returns true.
    // Return false otherwise.
    inline bool check_and_reserve()
    {
        bool result = false;

        if (!full())
        {
            reserve_enqueue();
            result = true;
        }

        return result;
    }

    //| Write one data item to the queue. `reserve_enqueue` must have been called previously.
    void enqueue(T value)
    {
        // Get the value of the write pointer then increment the write pointer
        auto producer_index = first(atomically<pointer_t>(increment));

        // Store the value into the memory
        _dataMem[producer_index] = value;

        // Allow 1 more value to be read out
        // Check for overflow (late)
        assert(_lateCounter.count() < Size);
        _lateCounter.increment();
    }

    //| Helper function that blocks the current thread until the output is not full
    // and then enqueues one item.
    void enqueue_blocking(T value)
    {
        // Wait for an output slot to be available
        atomic do; while(!check_and_reserve());

        // Write the data
        enqueue(value);
    }

    // Consumer interface

    //| Return true if there is no more data in the queue.
    // Typical usage is to call `empty` in the same atomic block as `front` and `pop`.
    inline bool empty()
    {
        return _lateCounter.count() == 0;
    }

    //| Return the value at the head of the queue.
    // Typical usage is to call `empty` in the same atomic block as `front` and `pop`.
    inline T front()
    {
        return _dataMem[_consumerIndex];
    }

    //| Remove 1 item from the queue.
    // Typical usage is to call `empty` in the same atomic block as `front` and `pop`.
    inline void pop()
    {
        // Check for underflow
        assert(!empty());

        _consumerIndex = static_cast(_consumerIndex + 1);

        _earlyCounter.subtract(1);
        _lateCounter.subtract(1);
    }
}