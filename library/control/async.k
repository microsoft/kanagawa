// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module control.async
    { async_then
    , async_then_void
    , async_exec
    , async_await
    , fork_join
    }

import data.array
import data.counter
import data.fifo


//| Execute `task` asynchronously and call `then` with the result once the
// task completes.
//
// #### __Example__
//
//     async_then(
//         []() -> uint32
//         {
//             // ...
//         },
//         [](uint32 result)
//         {
//             // ...
//         });
template <typename T>
inline void async_then(() -> T task, (T) -> void then)
{
    using task_t = decltype(task);
    using then_t = decltype(then);

    class async
    {
    public:
        [[async]] void exec(task_t task, then_t then)
        {
            then(task());
        }
    }

    static async _async;

    _async.exec(task, then);
}


//| Like `async_then` but for tasks that don't produce a result.
inline void async_then_void(() -> void task, () -> void then)
{
    using task_t = decltype(task);
    using then_t = decltype(then);

    class async
    {
    public:
        [[async]] void exec(task_t task, then_t then)
        {
            task();
            then();
        }
    }

    static async _async;

    _async.exec(task, then);
}

//| Execute a task asynchronously.
//
// #### __Hardware__
//
// A call to `async_exec` is implemented as a dataflow fork. For the common
// case of passing a lambda to `async_exec`, the lambda entry FIFO holds the
// value of each variable captured by the lambda. If the lambda pipeline does
// not introduce backpressure, the compiler can optimize away the entry FIFO.
//
//     void F(uint32 a)
//     {
//         uint32 b = a + 1;
//
//         async_exec
//             (   [b]()
//                 {
//                 }
//             );
//     }
//
// @@
// +----------------+
// | "F() Pipeline" |
// +-------+--------+
//         |
//  +------+------+
//  | Call Lambda |
//  +------+------+
//         |
//         +---------+
//         |         |
//     +---+---+  +--+--+
//     | "..." |  |     | Lambda Entry FIFO
//     +---+---+  +-----+
//                |  b  |
//                +-----+
//                |     |
//                +--+--+
//                   |
//          +--------+--------+
//          | Lambda Pipeline |
//          +-----------------+
// @@
inline void async_exec(() -> void task)
{
    async_then_void(task, []{});
}


//| A class faciliating the async/await pattern. `async` spawns a new asynchronous thread
// in which the provided lambda will be executed. Upon completion, the lambda's return
// value will be stored into an internal FIFO.
// In parallel, a call to `await` will block the calling thread until a result from
// `async` is available.
// The standard thread ordering guarantee ensures that no threads can overtake another
// and results will be returned by `await` in the same order as they entered `async`.
template
    < typename T          //< Type returned by lambda argument to `async`.
    , auto ReturnDepth    //< Depth of return value FIFO (maximum number of `fork<i>` results that can
                          //  be buffered ahead of a corresponding `async` call). A recommended value
                          //  would be 32 allowing it to be implemented in LUTRAM.
    , auto WaitForDepth   //< Depth of the wait-for FIFO (maximum number of threads that can queue
                          //  inside `await`). A conservative value would be 32 allowing it to be
                          //  implemented in LUTRAM, however, for lambdas that typically take more
                          //  than 32 cycles to return, this should be increased.
    >
class async_await
{
    FIFO<T, ReturnDepth, true /* EnqueueBlocking */, false /* DequeueBlocking */> _returnFifo;
    // Use a separate counter so that it can be atomically check-and-decremented independently
    // of the FIFO dequeue operation.
    // This counter cannot overflow since it is only incremented after a successful blocking
    // _returnFifo.enqueue() call
    counter<ReturnDepth, 0> _returnCount;

public:
    //| Spawn a new thread that calls the provided lambda.
    // Out-of-order behaviour can occur if multiple calls (causing multiple
    // inlines) to the same method on the same instance exists and is thus
    // not recommended.
    inline void async(() -> T task)
    {
        async_then<T>(task,
            [](T value)
            {
                _returnFifo.enqueue(value);
                _returnCount.increment();
            }
        );
    }

    //| Check to see if the async function has returned.
    // It is exposed as a public function to provide the option for a consumer
    // to wait on multiple `async_await` instances simultaneously (e.g. `fork_join`).
    inline bool check()
    {
        auto count = _returnCount.count();
        return count != 0;
    }

    //| Decrement the return counter if argument is true.
    inline void decrement(bool value)
    {
        assert(!value || _returnCount.count() != 0);
        _returnCount.subtract(cast<uint1>(value));
    }

    //| Wait for task started by `async` to complete and return its result.
    T await()
    {
        inline bool check_and_decrement()
        {
            auto result = check();
            decrement(result);
            return result;
        }
        atomic [[fifo_depth(WaitForDepth)]] do; while(!check_and_decrement());
        return dequeue();
    }

    //| Pop the value returned by the lambda function to `async`.
    // This function is called by `await`.
    // It is exposed as a public function so that use can wait for `check_and_decrement()`
    // and then `dequeue`.
    inline T dequeue()
    {
        return _returnFifo.dequeue();
    }
};


//| A wrapper around an array of `async_await` objects. This allows zero, one, or more threads
// (forks) to be spawned and their lambdas to execute in parallel.
// A call to `join` will block until all the provided forks (if any) have returned a result.
template
    < typename T            //< Type returned by lambda argument to all `fork` calls.
    , auto N                //< Maximum number of parallel forks supported.
    , auto ReturnDepth      //< Depth of return value FIFO (maximum number of `fork<i>` results that can
                            //  be buffered ahead of a corresponding `join` call). A recommended value
                            //  would be 32 allowing it to be implemented in LUTRAM.
    , auto WaitForDepth     //< Depth of the wait-for FIFO (maximum number of threads that can queue
                            //  inside `join`). A conservative value would be 32 allowing it to be
                            //  implemented in LUTRAM, however, for lambdas that typically take more
                            //  than 32 cycles to return, this should be increased.
    >
class fork_join
{
    static assert(N >= 2);

    async_await<T, ReturnDepth, WaitForDepth>[N] _forks;

    inline bool check(bool[N] which)
    {
        bool[N] result;
        static for(const auto i : N)
            result[i] = !which[i] || _forks[i].check();

        auto andResult = and(result);

        static for(const auto i : N)
            _forks[i].decrement(which[i] && andResult);

        return andResult;
    }

public:
    //| Create a single fork which runs the lambda function asynchronously.
    // Out-of-order behaviour can occur if multiple calls (causing multiple
    // inlines) to the same method, with the same template argument, on the
    // same instance exist and is thus not recommended.
    template<auto i>
    inline void fork(() -> T task)
    {
        static assert(i < N);

        _forks[i].async(task);
    }

    //| Block until all forks flagged in the argument have returned, and return
    // their values.
    T[N] join(bool[N] which)
    {
        atomic [[fifo_depth(WaitForDepth)]] do; while(!check(which));

        T[N] ret;
        static for(const auto i : N)
        {
            if (which[i])
                ret[i] = _forks[i].dequeue();
        }
        return ret;
    }
};