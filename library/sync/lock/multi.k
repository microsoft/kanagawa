// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module sync.lock.multi
    { multi_mutex
    , multi_rwlock
    , multi_semaphore
    }

import data.counter

//| An array of independent mutually exclusive locks, or mutexes.
// Call `lock` to acquire a mutex and `unlock` to release it.
template<auto NumMutexes>
class multi_mutex
{
private:
    memory<uint1, NumMutexes> _w;
    memory<uint1, NumMutexes> _r;
    bool is_initialized = false;

public:
    void init()
    {
        pipelined_for(NumMutexes, [](index_t<NumMutexes> i)
        {
            _r[i] = 0;
            _w[i] = 0;
        });

        is_initialized = true;
    }

    //| Atomically acquire the lock with the specified index if it is not already acquired otherwise block waiting on the lock to be released.
    void lock(index_t<NumMutexes> index)
    {
        assert(is_initialized);

        wait_until([index]
        {
            bool result = false;

            uint1 r = _r[index];
            uint1 w =  _w[index];

            if (r == w)
            {
                w = static_cast(w + 1);
                result = true;
            }
            _w[index] = w;

            return result;
        });
    }

    //| Release the lock with the specified index. You must never unlock a mutex that is not locked,
    // otherwise you will create a deadlock when a thread tries to lock this same mutex.
    void unlock(index_t<NumMutexes> index)
    {
        assert(is_initialized);

        uint1 r = _r[index];
        r = static_cast(r + 1);
        _r[index] = r;
    }
}

// An array of independent reader/writer locks.
template
    < auto MaxThreads //< The maximum number of threads that might be manipulating the lock. This determines
                      // the maximum number of read-locks that can be outstanding for a single lock. Typically 512 is
                      // a sensible value to use here.
    , auto NumLocks   //< The number of independent rwlocks to create. If we think of this class as implementing a
                      // vector of rwlock, then this value represents the vector size.
    >
class multi_rwlock
{
private:
    struct rwlock_state
    {
        uint1 num_writers;
        count_t<MaxThreads> num_readers;
    };

    using rwlock_index_t=index_t<NumLocks>;
    memory<rwlock_state, NumLocks> _w;
    memory<rwlock_state, NumLocks> _r;
    bool _is_initialized = false;

public:
    void init()
    {
        pipelined_for(NumLocks, [](index_t<NumLocks> i)
        {
            rwlock_state initial_state = {};

            _r[i] = initial_state;
            _w[i] = initial_state;
        });

        atomic
        {
            // The "atomic" is to prevent re-ordering of things such that
            // this gets set before the above writes are all completed
            _is_initialized = true;
        }
    }

    //| Attempt to acquire either a reader or a writer lock for the lock with the specified index.
    void lock(rwlock_index_t index, bool is_reader)
    {
        assert(_is_initialized);

        wait_until([index, is_reader]
        {
            bool result = false;

            auto r = _r[index];
            auto w = _w[index];

            bool no_write_locks = r.num_writers == w.num_writers;
            count_t<MaxThreads> reader_count = static_cast(w.num_readers - r.num_readers);

            if (is_reader)
            {
                // If not write locked and read locks available
                if (no_write_locks && (reader_count < MaxThreads))
                {
                    w.num_readers = static_cast(w.num_readers + 1);

                    result = true;
                }
            }
            else
            {
                // If not write locked and no read locks
                if (no_write_locks && (reader_count == 0))
                {
                    w.num_writers = static_cast(w.num_writers + 1);

                    result = true;
                }
            }

            _w[index] = w;

            return result;
        });
    }

    //| Release a reader or writer lock at the specified index. You must never unlock without having previously acquired
    // the same type of lock, otherwise you will create a deadlock situation.
    void unlock(rwlock_index_t index, bool is_reader)
    {
        assert(_is_initialized);

        atomic
        {
            auto r =  _r[index];

            if (is_reader)
            {
                // Decrement read lock ptr (by incrementing unlock side ptr) and leave write-lock ptr alone
                r.num_readers = static_cast(r.num_readers + 1);
            }
            else
            {
                // Decrement write lock ptr (by incrementing unlock side ptr) and leave read-lock ptr alone
                r.num_writers = static_cast(r.num_writers + 1);
            }

            _r[index] = r;
        }
    }

    //| Acquire a reader lock on the lock at the specified index.
    inline void read_lock(rwlock_index_t index)
    {
        lock(index, true);
    }

    //| Acquire a writer lock on the lock at the specified index.
    inline void write_lock(rwlock_index_t index)
    {
        lock(index, false);
    }

    //| Release a reader lock on the lock at the specified index.
    inline void read_unlock(rwlock_index_t index)
    {
        unlock(index, true);
    }

    //| Release a writer lock on the lock at the specified index.
    inline void write_unlock(rwlock_index_t index)
    {
        unlock(index, false);
    }

}

//| Multiple instance semaphore.
template
    < auto N               //< Number of semaphore instances.
    , auto M               //< Maximum semaphore value.
    , auto I               //< Initial value for the semaphore count. Must be less than or equal to `M`.
    , auto Blocking = true //< By default, this is true and causes the semaphore to
                           // block when wait is called. Setting this to false should only
                           // be used when the caller can ensure that the semaphore count is
                           // greater than 0 when wait is called. If it is not a diagnostics
                           // assert is raised.
    >
class multi_semaphore
{
public:
    using sem_ctr_t = count_t<M>;
    using sem_idx_t = index_t<N>;

    static assert(I <= M);

private:

    counter<M, I>[N] _counter;

    bool wait_helper(sem_idx_t which, sem_ctr_t val, bool decrement_count)
    {
        bool result;

        if (Blocking)
        {
            atomic do; while(!test_and_decrement(which, val, decrement_count));
            result = true;
        }
        else
        {
            atomic
            {
                result = test_and_decrement(which, val, decrement_count);
            }

            assert(result || !decrement_count);
        }

        return result;
    }

public:
    //| Check if the current count is greater than or equal to value. If `decrement_count` flag is set,
    // and the current count is sufficient, this method will also decrement the count.
    // This function is not inherently threadsafe, and must be called from inside an `atomic`.
    inline bool test_and_decrement(sem_idx_t which, sem_ctr_t val, bool decrement_count)
    {
        bool result = false;

        sem_ctr_t[N] counts;

        static for(const auto i : N)
        {
            counts[i] = _counter[i].count();
        }

        result = counts[which] >= val;

        if (result && decrement_count)
        {
            static for(const auto i : N)
            {
                if (i == which)
                {
                    _counter[i].subtract(val);
                }
            }
        }

        return result;
    }

    //| If blocking, wait for the semaphore count to be non-zero and conditionally decrement the count.
    // Otherwise check if the semaphore count is non-zero, conditionally decrement the count,
    // and assert if `decrement_count` is true and the count is zero.
    inline bool wait(sem_idx_t which, bool decrement_count)
    {
        return wait_helper(which, 1, decrement_count);
    }

    //| If blocking, wait for the semaphore count to be greater than or equal to the supplied value and conditionally decrement the count.
    // Otherwise check if the semaphore count is greater than or equal to `val`, conditionally decrement the count,
    // and assert if `decrement_count` is true and the count is insufficient.
    inline bool wait_multiple(sem_idx_t which, sem_ctr_t val, bool decrement_count)
    {
        return wait_helper(which, val, decrement_count);
    }

    //| Increment the semaphore count by 1, potentially waking a thread.
    inline void post(sem_idx_t which)
    {
        // static for needed due to compiler limitation (Task #6239)
        static for(const auto i : N)
        {
            if (i == which)
            {
                _counter[i].increment();
            }
        }
    }

    //| Return current value of semaphore. Provided primarily for debug/diagnostic purposes.
    inline sem_ctr_t count(sem_idx_t which)
    {
        sem_ctr_t result;

        // static for needed due to compiler limitation (Task #6239)
        static for(const auto i : N)
        {
            if (i == which)
            {
                result = _counter[i].count();
            }
        }

        return result;
    }

    //| Increment the semaphore count by the specified `amount`, potentially waking one or more threads.
    void post_multiple(sem_idx_t which, sem_ctr_t amount)
    {
        // static for needed due to compiler limitation (Task #6239)
        static for(const auto i : N)
        {
            if (i == which)
            {
                assert((cast<sem_ctr_t>(_counter[i].count()) + amount) <= M);
                _counter[i].add(amount);
            }
        }
    }
}