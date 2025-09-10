// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Rendezvous with a fixed number of threads.
*/
module control.async.rendezvous.strict
    { rendezvous
    }

import .options as opt
import data.array
import data.counter
import data.fifo
import data.fifo.array
import sync.atomic.init

//| Join exactly `N` threads into a group.
//  Each thread contributes one request to an array of requests before that thread blocks.
//  A single function processes the combined array of requests to generate an array of responses.
//  One response is sent to each thread and each thread in the group is unblocked.
template
< typename Request                              //< Request type.
, typename Response                             //< Response type.
, auto N                                        //< Number of requests processed in parallel.
, auto FifoDepth = 32                           //< Depth of internal FIFOs.
                                                //  Larger values are needed to achieve full throughput if `Process` is a high latency.
>
class rendezvous
{
private:
    init_once _once;

    counter<FifoDepth>[N] _request_count;

    // DequeueBlocking is false because _request_count is checked before calling dequeue
    template<typename T, auto Depth>
    using request_fifo_t = FIFO<T, Depth, true, false>;

    fifo_array<Request, FifoDepth, N, request_fifo_t> _requests;

    counter<FifoDepth>[N] _response_count;

    // EnqueueBlocking is false because _response_count is checked before calling enqueue
    template<typename T, auto Depth>
    using response_fifo_t = FIFO<T, Depth, false, true>;

    fifo_array<Response, FifoDepth, N, response_fifo_t> _responses;

public:
    //| Must be called before any requests will be processed.
    //  All calls to `start` after the first are ignored.
    inline void start
        ( (Request[N])->Response[N] process_fn   //< Function that processes requests to generate responses.
        )
    {
        if (!_once.check())
        {
            async_exec([process_fn]{process(process_fn);});
        }
    }

    //| Submits a requests, blocks until the corresponding response is generated, and returns the response.
    template
        < auto Index //< Index of the request in the request array passed to `Process`.
        >
    Response join(Request req)
    {
        static assert(Index < N);

        // Save request into request FIFO
        _requests.enqueue_one<Index>(req);

        // Notify processing thread that another request is ready
        _request_count[Index].increment();

        // Wait for response
        Response result = _responses.dequeue_one<Index>();

        // Notify processing thread that response has been dequeued
        _response_count[Index].decrement();

        return result;
    }

private:
    inline void process((Request[N])->Response[N] process_fn)
    {
        const auto ProcessThreadCount = opt::max_threads_default;

        pipelined_do([process_fn]()
        {
            // Check to see if N requests are available
            bool all_valid;

            atomic
            {
                bool[N] valids;

                static for (const auto i : N)
                {
                    valids[i] = _request_count[i].count() > 0;
                }

                all_valid = and(valids);

                static for (const auto i : N)
                {
                    _request_count[i].subtract(cast<uint1>(all_valid));
                }
            }

            if (all_valid)
            {
                Request[N] requests = _requests.dequeue_all();

                // process the array of requests
                Response[N] responses = process_fn(requests);

                // Wait for space in output queues
                atomic do; while(!response_space_available());

                // Send responses
                _responses.enqueue_all(responses);
            }

            return true;
        });
    }

    inline bool response_space_available()
    {
        bool result = false;

        bool[N] has_space = {};

        static for (const auto i : N)
        {
            has_space[i] = (_response_count[i].count() < FifoDepth);
        }

        result = and(has_space);

        static for (const auto i : N)
        {
            _response_count[i].add(cast<uint1>(result));
        }

        return result;
    }
}

