// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Looping control flow constructs.
*/
module control.loop
    { pipelined_for
    , pipelined_for_each
    , pipelined_do
    , pipelined_map
    , pipelined_last
    , range_for
    , while_do
    , parallel_for
    , parallel_for_each
    , parallel_map
    }

import .options as opt
import control.async
import data.array
import data.counter
import data.fifo
import type.stdtype

//| Execute body for a range of values starting from `begin` and ending before
// `end`, incrementing by `step` on each iteration.
template <typename T>
inline void range_for (T begin, T end, auto step, (T) -> void body)
{
    assert (step > 0);

    if (begin < end) do
    {
        body(begin);
        begin += step;
    }
    while (begin < end);
}

//| Keep calling `body` while the `condition` returns true.
inline void while_do (() -> bool condition, () -> void body)
{
    bool proceed;

    do
    {
        proceed = condition();

        if (proceed) body();
    }
    while (proceed);
}

template <typename I, typename Closure>
class pipelined
{
public:
    [[pipelined]] auto go(I tid, Closure closure)
    {
        return closure(tid);
    }
}

//| Spawn `count` threads executing `body`, which can be a lambda or a function
// taking one argument specifying the thread index from the range [0, `count`).
//
// #### __Examples__
//
//     pipelined_for(x, [](uint4 i)
//     {
//         // ...
//     });
//
//     void foo(uint8 i)
//     {
//     }
//
//     pipelined_for(256, foo);
//
// #### __Hardware__
//
// Each call to `pipelined_for` inserts a record into a FIFO. This FIFO holds
// the thread count and the values of captured variables. A finite state
// machine translates each FIFO record into `thread_count` calls to the inner
// function. A separate finite state machine unblocks the calling thread after
// `thread_count` threads have completed. Unblocking is achieved with a
// zero-width fifo.
//
//     void F(uint32 x, uint32 y)
//     {
//         uint32 thread_count = x + 1;
//
//         pipelined_for(thread_count, [y](uint32 tid)
//         {
//         });
//     }
//
// @@
//        Entry FIFO
//   +-------------------+
//   |                   |
//   +-------------------+
//   | "thread_count, y" |
//   +-------------------+
//   |                   |
//   +---------+---------+
//             |
// +-----------+-----------+
// | Thread Generation FSM |
// +-----------+-----------+
//             |
//    +--------+--------+
//    |  body Pipeline  |
//    +--------+--------+
//             |
// +-----------+-----------+
// | Thread Collection FSM |
// +-----------+-----------+
//             |
//      +------+------+
//      |             |
//      +-------------+
//      | "width = 0" |
//      +-------------+
//      |             |
//      +-------------+
//         Exit FIFO
// @@
template <typename I>
inline void pipelined_for(auto count, (I) -> void body)
{
    assert(count <= (1 << bitsizeof I));

    static pipelined<I, decltype(body)> loop;

    loop.go(count, body);
}

//| Spawn a thread for each element of array `arr` calling `body` which can be
// a lambda or a function taking two arguments, the thread index and the element
// value.
//
// #### __Example__
//
//     uint32[10] a;
//     pipelined_for_each(a,  [](uint4 i, uint32 x)
//     {
//         ...
//     });
template <typename T, auto N>
inline void pipelined_for_each(T[N] arr, (index_t<N>, T) -> void body)
{
    pipelined_for(N, [arr, body](index_t<N> i)
    {
        body(i, arr[i]);
    });
}

//| Spawn threads that will execute the `body` closure until it returns `false`.
// If `body` has an argument, then the number of threads used is
// `2^bitsizeof(I)`.  This should be larger than the depth of the `body`
// pipeline. If `body` does not have an argument, then the number of threads
// used is .options::max_threads_limit. The function returns after `body`
// returns `false` and all in-flight threads "drain" from the loop. When an
// instance of `pipelined_do` is called multiple times, the threads for
// subsequent calls will not start until all threads for earlier calls have
// started (although not necessarily exited).
//
// #### __Example__
//
//     pipelined_do([](index_t<32> i)
//     {
//         // ...
//         return !done;
//     });
//
//     pipelined_do([]()
//     {
//         // ...
//         return !done;
//     });
template <typename I>
inline void pipelined_do((I) -> bool body)
{
    static if (I == void)
    {
        pipelined_for(opt::max_threads_limit, [body](index_t<opt::max_threads_limit> tid)
        {
            do ; while (body());
        });
    }
    else
    {
        pipelined_for(1 << bitsizeof I, [body](I tid)
        {
            do ; while (body(tid));
        });
    }
}

//| Spawn `count` threads executing `body`, which can be a lambda or a
// function taking one argument specifying the thread index from the range
// [0, `count`]. The function returns an array `T[N]` of values produced
// by `body`. `N` must not be greater than `count`.
//
// #### __Example__
//
//     auto a = pipelined_map<10>(x, [](uint4 i) -> uint32
//     {
//         // ...
//     });
//
// #### __Hardware__
//
// The hardware generated for `pipelined_map` is similar to the hardware
// generated for `pipelined_for`. The thread collection finite state machine
// concatenates the return values from each inner thread into an array and
// returns that array to the caller.
//
//     uint32[4] F(uint32 x, uint32 y)
//     {
//         uint32 thread_count = x + 1;
//
//         uint32[4] result = pipelined_map
//             ( thread_count,
//                   [y](uint32 tid) -> uint32
//                   {
//                       return y + tid;
//                   }
//             );
//
//         return result;
//     }
//
// @@
//        Entry FIFO
//   +-------------------+
//   |                   |
//   +-------------------+
//   | "thread_count, y" |
//   +-------------------+
//   |                   |
//   +---------+---------+
//             |
// +-----------+-----------+
// | Thread Generation FSM |
// +-----------+-----------+
//             |
//    +--------+--------+
//    |  body Pipeline  |
//    +--------+--------+
//             |
// +-----------+-----------+
// | Thread Collection FSM |
// +-----------+-----------+
//             |
//    +--------+--------+
//    |                 |
//    +-----------------+
//    | "[y+0,y+1,...]" |
//    +-----------------+
//    |                 |
//    +-----------------+
//         Exit FIFO
// @@
template <auto N, typename I, typename T>
inline T[N] pipelined_map(auto count, (I) -> T body)
{
    assert(count <= N);
    assert(count <= (1 << bitsizeof I));

    const auto fn = [body](I x) -> T
    {
        return body(x);
    };

    static pipelined<I, decltype(fn)> loop;

    return loop.go(count, fn);
}

//| Spawn `count` threads executing `body`, which can be a lambda or a
// function taking one argument specifying the thread index from the range
// [0, `count`]. The function returns result produced by last call to `body`.
//
// #### __Example__
//
//     auto a = pipelined_last(x, [](uint4 i)
//     {
//         // ...
//     });
//
// #### __Hardware__
//
// The hardware generated for `pipelined_last` is similar to the hardware
// generated for `pipelined_for`. The thread collection finite state machine
// ignores the return value from all inner threads except the last one, where
// `tid == (thread_count - 1)`. This value is returned to the caller. If the
// thread count is equal to 0, then `default_value` is returned.
//
//     uint32 F(uint32 x, uint32 y)
//     {
//         uint32 thread_count = x + 1;
//
//         uint32 result = pipelined_last
//             (thread_count,
//                 [y](uint32 tid)
//                 {
//                     return y + tid;
//                 }
//             );
//
//         return result;
//     }
//
// @@
//        Entry FIFO
//   +-------------------+
//   |                   |
//   +-------------------+
//   | "thread_count, y" |
//   +-------------------+
//   |                   |
//   +---------+---------+
//             |
// +-----------+-----------+
// | Thread Generation FSM |
// +-----------+-----------+
//             |
//    +--------+--------+
//    |  body Pipeline  |
//    +--------+--------+
//             |
// +-----------+-----------+
// | Thread Collection FSM |
// +-----------+-----------+
//             |
//   +---------+----------+
//   |                    |
//   +--------------------+
//   | "y+thread_count-1" |
//   +--------------------+
//   |                    |
//   +--------------------+
//        Exit FIFO
// @@
template <typename I, typename T, T default_value = {}>
inline T pipelined_last(auto count, (I) -> T body)
{
    using body_t = decltype(body);

    class Helper
    {
    public:
        [[pipelined]] void loop(I tid, I last, body_t body)
        {
            const auto x = body(tid);

            if (tid == last)
            {
                _result.enqueue(x);
            }
        }

        inline T result()
        {
            return _result.dequeue();
        }

    private:
        FIFO<T, 32, true, false> _result;
    }

    static Helper helper;

    assert(count <= (1 << bitsizeof I));

    T result = default_value;

    if (count > 0)
    {
        helper.loop(count, cast<I>(count - 1), body);
        result = helper.result();
    }

    return result;
}

template
    < auto N
    , auto MaxThreads
    , bool HasResult
    , typename ResultType
    >
class parallel
{
private:
    // These count the number of calls to `body` which have finished execution
    // but have not resulting in a return from `go` yet.  For example
    // if something downstream of `go` is stalling (which will eventually backpressure `go`)
    // then `go` may not immediately acknowledge the completion of a set of calls to `body`.
    counter<MaxThreads, 0>[N] _complete_count;

    static if (HasResult)
    {
        // EnqueueBlocking = false because there can not be more than
        // MaxThreads concurrent calls
        //
        // DequeueBlock = false because test_and_dec tracks completion
        FIFO<ResultType, MaxThreads, false, false>[N] _result_fifos;
    }

    inline bool test_and_dec(bool[N] mask)
    {
        bool[N] ready;

        static for (const auto i : N)
        {
            ready[i] = _complete_count[i].count() >= cast<uint1>(mask[i]);
        }

        bool result = and(ready);

        static for (const auto i : N)
        {
            _complete_count[i].subtract(cast<uint1>(result && mask[i]));
        }

        return result;
    }

    template<auto Index>
    inline void launch((index_t<N>) -> ResultType body)
    {
        async_exec([body]
        {
            static if (HasResult)
            {
                _result_fifos[Index].enqueue(body(Index));
            }
            else
            {
                body(Index);
            }

            _complete_count[Index].increment();
        });
    }

    inline bool[N] impl(count_t<N> count, (index_t<N>) -> ResultType body)
    {
        bool[N] mask = {};

        static for (const auto i : N)
        {
            if (i < count)
            {
                mask[i] = true;
                launch<i>(body);
            }
        }

        atomic [[fifo_depth(MaxThreads)]] do; while(!test_and_dec(mask));

        return mask;
    }

public:
    static if (HasResult)
    {
        inline ResultType[N] go(count_t<N> count, (index_t<N>) -> ResultType body)
        {
            auto mask = impl(count, body);

            ResultType[N] result = {};

            static for (const auto i : N)
            {
                if (mask[i])
                {
                    result[i] = _result_fifos[i].dequeue();
                }
            }

            return result;
        }
    }
    else
    {
        inline void go(count_t<N> count, (index_t<N>) -> void body)
        {
            impl(count, body);
        }
    }
}

//| Spawn `count` threads executing `body`, which can be a lambda or a function
// taking one argument specifying the thread index from the range [0, `count`).
//
// Threads execute across `N` instances of the function `body`.
// There are no ordering guarantees among threads spawned by one call to `parallel_for`.
// If there are two calls (`A` and `B`) to a single `parallel_for` call site, resulting in threads
// `A0`, `A1`, `B0`, `B1` executing `body`, then thread `A0` will begin executing `body` ahead of `B0`
// and `A1` will begin executing `body` ahead of `B1`.
//
// `count` must be less than `N`.
//
// #### __Examples__
//
//     parallel_for(x, [](uint4 i)
//     {
//         // ...
//     });
//
//     void foo(uint8 i)
//     {
//     }
//
//     parallel_for(256, foo);
//
// #### __Hardware__
//
// Each call to `parallel_for` broadcasts captured variables to `N` FIFOs.
// Each of these FIFOs is associated with a pipeline which is an instance of `body`.
// After executing `body`, a counter is incremented, which is used to block the calling thread
// until all calls to `body` have completed.
//
//     void F(uint32 x, uint32 y)
//     {
//         parallel_for(2, [x, y](index_t<2> tid)
//         {
//         });
//     }
//
// @@
//
//          +-------------+
//          |      F      |
//          | before call |
//          +-------------+
//              /    \
//             /      \
//            /        \
//           /          \
//          /            \
//        FIFO            FIFO
//   +-----------+    +-----------+
//   |           |    |           |
//   +-----------+    +-----------+
//   |   x, y    |    |   x, y    |
//   +-----------+    +-----------+
//   |           |    |           |
//   +-----------+    +-----------+
//         |                |
//   +-----------+    +-----------+
//   |   body    |    |   body    |
//   | pipeline  |    | pipeline  |
//   +-----------+    +-----------+
//         |               |
//   +-----------+    +-----------+
//   | increment |    | increment |
//   |  counter  |    |  counter  |
//   +-----------+    +-----------+
//             \        /
//          +------------+
//          | wait for   |
//          | counters   |
//          +------------+
//                |
//          +------------+
//          |      F     |
//          | after call |
//          +------------+
// @@
template
    < auto N                                            //< Number of replicas to `body` to instantiate.
    , auto MaxCallerThreads = opt::max_threads_limit    //< Maximum number of threads concurrently executing inside of `parallel_for`.
                                                        // Caller must ensure this limit is not exceeded.
    >
inline void parallel_for
    ( count_t<N> count              //< Number of times that `body` will be invoked.  Must be no greater than `N`.
    , (index_t<N>) -> void body     //< Function to invoke.
    )
{
    assert(count <= N);

    static parallel<N, MaxCallerThreads, false, void> loop;

    loop.go(count, body);
}

//| Spawn a thread for each element of array `arr` calling `body` which can be
// a lambda or a function taking two arguments, the thread index and the element
// value.
//
// Threads execute across `N` instances of the function `body`.
// There are no ordering guarantees among threads spawned by one call to `parallel_for_each`.
// If there are two calls (`A` and `B`) to a single `parallel_for_each` call site, resulting in threads
// `A0`, `A1`, `B0`, `B1` executing `body`, then thread `A0` will begin executing `body` ahead of `B0`
// and `A1` will begin executing `body` ahead of `B1`.
//
// #### __Example__
//
//     uint32[10] a;
//     parallel_for_each(a,  [](uint4 i, uint32 x)
//     {
//         ...
//     });
template
    < typename T                                        //< Type of each input array element.
    , auto N                                            //< Number of replicas to `body` to instantiate.
    , auto MaxCallerThreads = opt::max_threads_limit    //< Maximum number of threads concurrently executing inside of `parallel_for_each`.
                                                        // Caller must ensure this limit is not exceeded.
    >
inline void parallel_for_each
    ( T[N] arr                      //< Input array to be processed (each element is processed by a separate call to `body`).
    , (index_t<N>, T) -> void body  //< Function which processes one input array element on each call.
    )
{
    parallel_for<N, MaxCallerThreads>(N, [arr, body](index_t<N> i)
    {
        body(i, arr[i]);
    });
}

//| Spawn `count` threads executing `body`, which can be a lambda or a
// function taking one argument specifying the thread index from the range
// [0, `count`]. The function returns an array `T[N]` of values produced
// by `body`. `N` must not be greater than `count`.
//
// Threads execute across `N` instances of the function `body`.
// There are no ordering guarantees among threads spawned by one call to `parallel_map`.
// If there are two calls (`A` and `B`) to a single `parallel_map` call site, resulting in threads
// `A0`, `A1`, `B0`, `B1` executing `body`, then thread `A0` will begin executing `body` ahead of `B0`
// and `A1` will begin executing `body` ahead of `B1`.
//
// #### __Example__
//
//     auto a = parallel_map<4>(x, [](uint2 i) -> uint32
//     {
//         // ...
//     });
//
// #### __Hardware__
//
// Each call to `parallel_map` broadcasts captured variables to `N` FIFOs.
// Each of these FIFOs is associated with a pipeline which is an instance of `body`.
// After executing `body`, the return result is placed into a FIFO.
// Results from all FIFOs are dequeued and returned.
//
//     void F(uint32 x, uint32 y)
//     {
//         parallel_map<2>(2, [x, y](index_t<2> tid) -> uint32
//         {
//              return x + y;
//         });
//     }
//
// @@
//
//          +-------------+
//          |      F      |
//          | before call |
//          +-------------+
//              /    \
//             /      \
//            /        \
//           /          \
//          /            \
//        FIFO            FIFO
//   +-----------+    +-----------+
//   |           |    |           |
//   +-----------+    +-----------+
//   |   x, y    |    |   x, y    |
//   +-----------+    +-----------+
//   |           |    |           |
//   +-----------+    +-----------+
//         |                |
//   +-----------+    +-----------+
//   |   body    |    |   body    |
//   | pipeline  |    | pipeline  |
//   +-----------+    +-----------+
//         |               |
//       FIFO             FIFO
//   +-----------+    +-----------+
//   |           |    |           |
//   +-----------+    +-----------+
//   |   x + y   |    |   x + y   |
//   +-----------+    +-----------+
//   |           |    |           |
//   +-----------+    +-----------+
//             \        /
//          +------------+
//          |  combine   |
//          |  results   |
//          +------------+
//                |
//          +------------+
//          |      F     |
//          | after call |
//          +------------+
// @@
template
    < auto N                                            //< Number of replicas to `body` to instantiate.
    , typename T                                        //< Type of each output array element.
    , auto MaxCallerThreads = opt::max_threads_limit    //< Maximum number of threads concurrently executing inside of `parallel_map`.
                                                        // Caller must ensure this limit is not exceeded.
    >
inline T[N] parallel_map
    ( auto count                //< Number of times that `body` will be invoked.  Must be no greater than `N`.
    , (index_t<N>) -> T body    //< Function which returns one array element on each call.
    )
{
    assert(count <= N);

    static parallel<N, MaxCallerThreads, true, T> loop;

    return loop.go(count, body);
}
