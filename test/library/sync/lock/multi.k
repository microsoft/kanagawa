// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.counter
import data.random.lfsr
import sync.lock.multi
import test.unit as unit
import test.runner

// Class: Unit tests for sync.lock.multi multi_mutex class.
class MultiMutexTests
{
private:

    const auto NUM_MUTEXES = 4;
    const auto NUM_THREADS = 1024;
    bool[NUM_MUTEXES] _isLocked = {};
    multi_mutex<NUM_MUTEXES> _m;
    lfsr_random<clog2(NUM_MUTEXES*16)> _randomMutexIndex;
    bool _error;


    [[pipelined]] void TestLocks(uint<clog2(NUM_THREADS)+1> tid)
    {
        uint<clog2(NUM_MUTEXES)> mutexIndex = cast<uint<clog2(NUM_MUTEXES)>>(_randomMutexIndex.next({}));

        //print("[ {tid} ] locking  {mutexIndex}\n");
        _m.lock(mutexIndex);
        //print("[ {tid} ]   locked  {mutexIndex}\n");

        if (_isLocked[mutexIndex])
        {
            _error = true;
            assert(false);
        }
        _isLocked[mutexIndex] = true;

        if (!_isLocked[mutexIndex])
        {
            _error = true;
            assert(false);
        }
        _isLocked[mutexIndex] = false;

        //print("[ {tid} ] unlocking  {mutexIndex}\n");
        _m.unlock(mutexIndex);
        //print("[ {tid} ]   unlocked  {mutexIndex}\n");
    }

public:
    bool run()
    {
        print("MultiMutexTests start\n");

        _error = false;

        _m.init();
        TestLocks(NUM_THREADS);

        print("MultiMutexTests complete\n");

        return !_error;
    }
}

// Class: Unit tests for sync.lock.multi multi_rwlock class.
class MultiRwlockTests
{
private:

    const auto NUM_RWLOCKS = 4;
    const auto NUM_THREADS = 4096;
    bool[NUM_RWLOCKS] _isWriteLocked = {};
    multi_rwlock<NUM_THREADS, NUM_RWLOCKS> _rwlocks;
    lfsr_random<clog2(NUM_RWLOCKS*16)> _randomLockIndex;
    lfsr_random<9> _randomLockType;
    bool _error;

    inline uint<clog2(NUM_RWLOCKS)> RandomLockIndex()
    {
        return cast<uint<clog2(NUM_RWLOCKS)>>(_randomLockIndex.next({}));
    }

    inline bool IsRandomLockTypeReader()
    {
        return _randomLockType.next({}) != 7;
    }

    bool timeout_active = false;

    [[async]] void StartTimeoutCheck(uint<16> timeout_cycles)
    {
        assert(!timeout_active);

        timeout_active = true;
        uint64 start = cycles();

        wait([start, timeout_cycles]()
        {
            return !timeout_active || (cycles() - start > timeout_cycles);
        });

        if (cycles() - start > timeout_cycles)
        {
            print("Timed out waiting for some lock\n");
            _error = true;
            assert(false);
        }
    }

    void StopTimeoutCheck()
    {
        timeout_active = false;
    }

    void TestSimultaneousReadLocks()
    {
        print("Testing simultaneous read locks\n");
        StartTimeoutCheck(1000);
        _rwlocks.read_lock(0);
        _rwlocks.read_lock(0);
        _rwlocks.read_unlock(0);
        _rwlocks.read_unlock(0);
        StopTimeoutCheck();
    }

    void TestIndependentWriteLocks()
    {
        print("Testing independent write locks\n");
        StartTimeoutCheck(1000);
        _rwlocks.write_lock(0);
        _rwlocks.write_lock(1);
        _rwlocks.write_unlock(1);
        _rwlocks.write_unlock(0);
        StopTimeoutCheck();
    }

    [[pipelined]] void RandomLocksPipelined(uint<clog2(NUM_THREADS)+1> tid)
    {
        // Pick a random lock index
        auto index = RandomLockIndex();

        // Determine whether we do a writer or a reader lock
        bool is_reader = IsRandomLockTypeReader();

        _rwlocks.lock(index, is_reader);

        if (_isWriteLocked[index])
        {
            assert(false);
            _error = true;
        }

        if (!is_reader)
        {
            _isWriteLocked[index] = true;
        }

        if (is_reader)
        {
            if (_isWriteLocked[index])
            {
                assert(false);
                _error = true;
            }
        }
        else
        {
            _isWriteLocked[index] = false;
        }

        _rwlocks.unlock(index, is_reader);
    }

    void TestRandomLocks()
    {
        RandomLocksPipelined(NUM_THREADS);
    }

public:
    bool run()
    {
        print("MultiRwlockTest start\n");

        _error = false;

        _rwlocks.init();

        TestSimultaneousReadLocks();
        TestIndependentWriteLocks();
        TestRandomLocks();

        print("MultiRwlockTest complete\n");

        return !_error;
    }
}

// Class: Unit tests for sync.lock.multi semaphore class.
class MultiSemaphoreTests
{
private:
    const auto NUM_SEMAPHORES = 2;
    const auto NUM_THREADS = 1000;
    const auto INITIAL_SEMAPHORE_COUNT = 10;

    using ThreadId_t = index_t<NUM_THREADS>;
    using ThreadCount_t = count_t<NUM_THREADS>;
    using SemaphoreId_t = index_t<NUM_SEMAPHORES>;

    bool _error = false;

    multi_semaphore<NUM_SEMAPHORES,NUM_THREADS,INITIAL_SEMAPHORE_COUNT> _sem;
    counter<NUM_THREADS, 0>[NUM_SEMAPHORES] _counter;
    lfsr_random<2> _random;

    [[pipelined]] void RunThread(ThreadId_t tid)
    {
        // Choose a random semaphore
        SemaphoreId_t which_sem = _random.next({}) % NUM_SEMAPHORES;

        _sem.wait(which_sem, true);

        ThreadCount_t count;
        static for(const auto i : NUM_SEMAPHORES)
        {
            if (i == which_sem)
            {
                _counter[i].increment();
                count = _counter[i].count();
            }
        }

        if (count > INITIAL_SEMAPHORE_COUNT)
        {
            _error = true;
            assert(false);
        }

        static for(const auto i : NUM_SEMAPHORES)
        {
            if (i == which_sem)
            {
                _counter[i].decrement();
            }
        }

        _sem.post(which_sem);
    }

public:
    bool run()
    {
        print("MultiSemaphoreTest start\n");

        RunThread(NUM_THREADS);

        print("MultiSemaphoreTest complete\n");

        return !_error;
    }
}

// Function: Main test entry point for unit tests of sync.lock.multi mutex, rwlock, and semaphore classes.
inline void test_main()
{
    unit::check<1>(unit::fixture<MultiMutexTests>());
    unit::check<2>(unit::fixture<MultiRwlockTests>());
    unit::check<3>(unit::fixture<MultiSemaphoreTests>());
}
