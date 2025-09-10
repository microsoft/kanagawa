// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import sync.lock
import test.unit as unit
import test.runner

// Class: Unit tests for sync.lock mutex class.
class MutexTests
{
private:

    const auto NUM_ITERATIONS = 1000;
    bool _odd = false;
    bool _even = false;
    bool _lock_error = false;
    mutex _m;


    inline uint8 Lfsr8(uint8 seed)
    {
        uint1 [8] bits = cast<uint1[8]>(seed);
        uint8 next_val = concat( cast<uint7>(seed), bits[7] ^ bits[5] ^ bits[4] ^ bits[3] );

        if(next_val == 0)
        {
            next_val = 1;
        }

        return next_val;
    }

    [[pipelined]] void TestLocks(uint<bitsizeof(NUM_ITERATIONS)+1> tid)
    {
        static uint8 rand_delay = 1;

        _m.lock();
        if (0 != (tid & 0x1))
        {
            _odd = true;
        }
        else
        {
            _even = true;
        }

        // Stall for some random amount of time
        atomic
        {
            rand_delay = Lfsr8(rand_delay);
        }

        uint1 bogus;
        for (const uint8 delay : rand_delay)
        {
            atomic
            {
                // do something
                bogus = ~bogus;
            }
        }

        if (_odd == _even)
        {
            _lock_error = true;
            assert(false);
        }

        if (0 != (tid & 0x1))
        {
            _odd = false;
        }
        else
        {
            _even = false;
        }
        _m.unlock();
    }

public:
    bool run()
    {
        _lock_error = false;

        print("mutex test start\n");

        TestLocks(NUM_ITERATIONS);

        print("mutex test complete\n");

        return !_lock_error;
    }
}

// Class: Unit tests for sync.lock rwlock class.
class RwlockTests
{
private:
    const auto MaxThreads = 32;
    rwlock<MaxThreads> _lock;
    bool _error = false;
    uint32 _read_locks = 0;
    uint32 _write_locks = 0;

    [[async]] void ReadLock()
    {
        _lock.read_lock();
        _read_locks++;
    }

    [[async]] void WriteLock()
    {
        _lock.write_lock();
        _write_locks++;
    }

    void DelayFor(uint16 num_iterations)
    {
        uint1 bogus;
        for (const uint8 delay : num_iterations)
        {
            atomic
            {
                // do something
                bogus = ~bogus;
            }
        }
    }

    inline void Delay()
    {
        DelayFor(100);
    }

public:
    void TestReadLocks()
    {
        _read_locks = 0;
        _write_locks = 0;

        // Test acquiring some read locks
        const auto num_read_locks_to_request = MaxThreads/2;
        for (const uint9 i : num_read_locks_to_request)
        {
            ReadLock();
        }

        Delay();

        if (_read_locks != num_read_locks_to_request)
        {
            assert(false);
            _error = true;
        }

        for (const uint9 i : num_read_locks_to_request)
        {
            _lock.read_unlock();
        }
    }

    void TestWriteLocks()
    {
        _read_locks = 0;
        _write_locks = 0;

        WriteLock();
        Delay();
        if (_write_locks != 1)
        {
            assert(false);
            _error = true;
        }

        WriteLock();
        Delay();
        if (_write_locks != 1)
        {
            assert(false);
            _error = true;
        }

        _lock.write_unlock();
        Delay();
        if (_write_locks != 2)
        {
            assert(false);
            _error = true;
        }
        _lock.write_unlock();
    }

    void TestReadLocksBlockWriteLock()
    {
        _read_locks = 0;
        _write_locks = 0;

        ReadLock();
        Delay();
        if (_read_locks != 1)
        {
            assert(false);
            _error = true;
        }

        WriteLock();
        Delay();
        if (_write_locks != 0)
        {
            assert(false);
            _error = true;
        }

        _lock.read_unlock();
        Delay();
        if (_write_locks != 1)
        {
            assert(false);
            _error = true;
        }

        _lock.write_unlock();
    }

    void TestWriteLockBlocksReadLocks()
    {
        _read_locks = 0;
        _write_locks = 0;

        WriteLock();
        Delay();
        if (_write_locks != 1)
        {
            assert(false);
            _error = true;
        }

        ReadLock();
        Delay();
        if (_read_locks != 0)
        {
            assert(false);
            _error = true;
        }

        _lock.write_unlock();
        Delay();
        if (_read_locks != 1)
        {
            assert(false);
            _error = true;
        }

        _lock.read_unlock();
    }

    void TestMoreThanMaxThreadsReadLocks()
    {
        _read_locks = 0;
        _write_locks = 0;

        // Test acquiring some read locks
        for (const uint9 i : MaxThreads)
        {
            ReadLock();
        }

        Delay();

        if (_read_locks != MaxThreads)
        {
            assert(false);
            _error = true;
        }

        ReadLock();
        Delay();

        if (_read_locks != MaxThreads)
        {
            assert(false);
            _error = true;
        }

        _lock.read_unlock();
        Delay();

        if (_read_locks != (MaxThreads+1))
        {
            assert(false);
            _error = true;
        }

        for (const uint9 i : MaxThreads)
        {
            _lock.read_unlock();
        }
    }

    bool run()
    {
        _error = false;

        print("rwlock test start\n");

        if (!_error) TestReadLocks();
        if (!_error) TestWriteLocks();
        if (!_error) TestReadLocksBlockWriteLock();
        if (!_error) TestWriteLockBlocksReadLocks();
        if (!_error) TestMoreThanMaxThreadsReadLocks();

        print("rwlock test complete\n");

        return !_error;
    }
}

// Class: Unit tests for sync.lock semaphore class.
class SemaphoreTests
{
private:
    const auto NUM_ITERATIONS = 1000;
    const auto NUM_THREADS = 7;

    using ThreadId_t=index_t<NUM_THREADS>;
    using ThreadCount_t=count_t<NUM_THREADS>;

    bool _error = false;
    semaphore<NUM_THREADS,0,true> _sem;
    uint1[NUM_THREADS] _thread_bits = {};


    ThreadCount_t CountThreadBits()
    {
        ThreadCount_t result;

        atomic
        {
            static for (const auto i : NUM_THREADS)
            {
                if (_thread_bits[i] != 0)
                {
                    ++result;
                }
            }
        }

        return result;
    }

    [[async]] void SetBit(ThreadId_t tid)
    {
        _sem.wait();
        atomic
        {
            _thread_bits[tid] = 1;
        }
    }

    [[async]] void ClrBit(ThreadId_t tid)
    {
        _sem.wait();
        atomic
        {
            _thread_bits[tid] = 0;
        }
    }

    [[pipelined]] void SetBits(ThreadId_t tid)
    {
        SetBit(tid);
    }

    [[pipelined]] void ClrBits(ThreadId_t tid)
    {
        ClrBit(tid);
    }

    [[pipelined]] void SetBitsMultipleHelper(ThreadId_t tid)
    {
        atomic
        {
            _thread_bits[tid] = 1;
        }
    }

    // Set bits using semaphore's wait_multiple
    [[async]] void SetBitsMultiple(ThreadId_t tid)
    {
        _sem.wait_multiple(tid);
        SetBitsMultipleHelper(tid);
    }

    [[pipelined]] void ClrBitsMultipleHelper(ThreadId_t tid)
    {
        atomic
        {
            _thread_bits[tid] = 0;
        }
    }

    // Clear bits using semaphore's wait_multiple
    [[async]] void ClrBitsMultiple(ThreadId_t tid)
    {
        _sem.wait_multiple(tid);
        ClrBitsMultipleHelper(tid);
    }

    void Delay(uint16 num_iterations)
    {
        uint1 bogus;
        for (const uint8 delay : num_iterations)
        {
            atomic
            {
                // do something
                bogus = ~bogus;
            }
        }
    }

public:
    bool run()
    {
        ThreadCount_t remaining = NUM_THREADS;
        ThreadCount_t expected = 0;

        _error = false;

        print("semaphore test start\n");

        // Start
        SetBits(NUM_THREADS);
        Delay(100);
        // Validate that no bits are set (all threads blocked on Semaphore)
        if (CountThreadBits() != 0)
        {
            _error = true;
            assert(false);
        }

        // Release 1 thread
        _sem.post();
        Delay(100);
        remaining -= 1;
        expected += 1;
        // Validate that one bit is set (remaining threads blocked on Semaphore)
        if (expected != CountThreadBits())
        {
            _error = true;
            assert(false);
        }

        // Release 2 more threads
        _sem.post_multiple(2);
        Delay(100);
        remaining -= 2;
        expected += 2;
        // Validate that three bits are set (remaining threads blocked on Semaphore)
        if (expected != CountThreadBits())
        {
            _error = true;
            assert(false);
        }

        // Release remaining threads
        _sem.post_multiple(remaining);
        Delay(100);
        expected += remaining;
        remaining = 0;
        // Validate that all bits are set (no threads blocked on Semaphore)
        if (expected != CountThreadBits())
        {
            _error = true;
            assert(false);
        }

        // Clear all threads
        ClrBits(NUM_THREADS);
        Delay(100);
        // Validate that all bits are set (all threads blocked on Semaphore)
        if (expected != CountThreadBits())
        {
            _error = true;
            assert(false);
        }

        // Validate that no bits are set (no threads blocked on Semaphore)
        _sem.post_multiple(expected);
        expected = 0;
        Delay(100);
        if (expected != CountThreadBits())
        {
            _error = true;
            assert(false);
        }

        // Start with wait_multiple
        SetBitsMultiple(NUM_THREADS);
        Delay(100);
        // Validate that no bits are set (all threads blocked on Semaphore)
        if (CountThreadBits() != 0)
        {
            _error = true;
            assert(false);
        }

        // Release all threads
        _sem.post_multiple(NUM_THREADS);
        Delay(100);
        // Validate that all bits are set (no threads blocked on Semaphore)
        if (CountThreadBits() != NUM_THREADS)
        {
            _error = true;
            assert(false);
        }

        // Clear all threads with wait_multiple
        ClrBitsMultiple(NUM_THREADS);
        Delay(100);
        // Validate that all bits are set (all threads blocked on Semaphore)
        if (CountThreadBits() != NUM_THREADS)
        {
            _error = true;
            assert(false);
        }

        // Validate that no bits are set (no threads blocked on Semaphore)
        _sem.post_multiple(NUM_THREADS);
        Delay(100);
        if (CountThreadBits() != 0)
        {
            _error = true;
            assert(false);
        }

        print("semaphore test complete\n");

        return !_error;
    }
}

// Function: Main test entry point for unit tests of sync.lock mutex, rwlock, and semaphore classes.
inline void test_main()
{
    unit::check<1>(unit::fixture<MutexTests>());
    unit::check<2>(unit::fixture<RwlockTests>());
    unit::check<3>(unit::fixture<SemaphoreTests>());
}

