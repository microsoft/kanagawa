// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module sync.lock
    { mutex
    , rwlock
    , semaphore
    }

import type.stdtype
import data.counter

//| Mutex: mutually exclusive lock. Call `lock` to acquire the mutex and `unlock` to release it.
// You would typically use this class in instances where you have code that implements a read-modify-write
// pattern on some variable(s) which takes multiple cycles to complete. In cases where this
// operation could be completed within a single clock cycle, you may consider instead putting the code in
// a function which internally uses the `atomic` keyword to encapsulate the read-modify-write.
class mutex
{
private:
    bool _locked = false; // true means locked

    inline bool test_set()
    {
        bool result = false;

        if (!_locked)
        {
            _locked = true;
            result = true;
        }

        return result;
    }

public:
    //| Atomically acquire the lock if it is not already acquired, or block waiting on the lock to be released.
    void lock()
    {
        atomic do; while(!test_set());
    }

    //| Release the lock.
    void unlock()
    {
        assert(_locked);
        atomic
        {
            _locked = false;
        }
    }
}

//| Reader-Writer lock is a synchronization primitive that allows multiple
// "threads" to acquire a read-lock, but only one thread may have (any) lock if that thread
// has the write lock.
template
    < auto MaxThreads //< The maximum number of threads that might be manipulating the lock.
                      // This determines the maximum number of read-locks that can be outstanding.
                      // Typically 512 is a reasonable value to use here.
    >
class rwlock
{
private:
    counter<MaxThreads, 0> _read_lock_counter;
    bool _write_locked = false;

    inline bool check_lock(bool is_reader)
    {
        bool result = false;
        auto rlocks_used = _read_lock_counter.count();

        if (is_reader)
        {
            result = !_write_locked && (rlocks_used < MaxThreads);
        }
        else
        {
            result = !_write_locked && (rlocks_used == 0);
        }

        // Update lock state
        if (result)
        {
            _read_lock_counter.add(cast<uint1>(is_reader));
            _write_locked = !is_reader;
        }

        return result;
    }

public:

    void lock(bool is_reader)
    {
        atomic do; while(!check_lock(is_reader));
    }

    inline void read_lock()
    {
        lock(true);
    }

    inline void write_lock()
    {
        lock(false);
    }

    inline void unlock(bool is_reader)
    {
        if (is_reader)
        {
            read_unlock();
        }
        else
        {
            write_unlock();
        }
    }

    void read_unlock()
    {
        _read_lock_counter.subtract(1);
    }

    void write_unlock()
    {
        atomic
        {
            _write_locked = false;
        }
    }
}


//| [Semaphore](https://en.wikipedia.org/wiki/Semaphore_(programming))
template
    < auto M               //< Maximum semaphore value.
    , auto I               //< Initial value for the semaphore count. Must be less than or equal to `M`.
    , auto Blocking = true //< When true, this parameter causes the semaphore to block when wait is called.
                           // Setting to false should only be used when the caller can ensure that the
                           // semaphore count is greater than 0 when wait is called otherwise a `assert`
                           // is raised.
    >
class semaphore
{
public:
    using sem_ctr_t = count_t<M>;

    static assert(I <= M);

private:

    counter<M, I> _counter;

public:
    //| Decrement the counter if the current count is greater or equal than `val`. This function is not
    // inherently threadsafe and must be called from inside an `atomic`.
    inline bool test_and_decrement(sem_ctr_t val)
    {
        bool result = (_counter.count() >= val);
        if (result)
        {
            _counter.subtract(val);
        }

        return result;
    }

    //| Decrement the semaphore count by 1 if it is nonzero otherwise block.
    void wait()
    {
        wait_multiple(1);
    }

    //| Decrement the semaphore count by `val` if it is nonzero otherwise block.
    void wait_multiple(sem_ctr_t val)
    {
        if (Blocking)
        {
            atomic do; while(!test_and_decrement(val));
        }
        else
        {
            bool result;
            atomic
            {
                result = test_and_decrement(val);
            }
            // Check for underflow
            assert(result);
        }
    }

    //| Increment the semaphore count by 1, potentially waking a thread.
    inline void post()
    {
        _counter.increment();
    }

    //| Return current value of semaphore. Provided primarily for debug/diagnostic purposes.
    inline sem_ctr_t count()
    {
        return _counter.count();
    }

    //| Increment the semaphore count by the specified `amount`, potentially waking one or more threads.
    void post_multiple(sem_ctr_t amount)
    {
        assert((cast<sem_ctr_t>(_counter.count()) + amount) <= M);
        _counter.add(amount);
    }
}