// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.random.lfsr
import sync.atomic
import test.unit as unit
import test.runner
import .options as opt

class SingleThreadedFifo
{
public:
    // Test for sizing of fifos within max_threads(1) functions
    [[max_threads(1)]] void run(unit::tag_t tag)
    {
        uint32[4] a = pipelined_map<4>(4, [](index_t<4> tid) -> uint32
        {
            return tid + 1;
        });

        static for (const auto i : 4)
        {
            unit::assert_equal(tag, i + 1, a[i]);
        }
    }
}

// Throughput test for deep pipelines - to ensure FIFOs are sized deep enough
// to give good throughput.
template<auto Depth, auto Threads=1024>
class DeepPipelineThroughput
{
public:
    void run(unit::tag_t tag)
    {
        pipelined_for(Threads, [tag](index_t<Threads> i)
        {
            bool is_first = (i == 0);
            bool is_last = (i == (Threads - 1));
            uint32 result = i + 1;

            static for (const auto d : Depth)
            {
                result += (d * i);
            }

            // Add variablility to the inner loop
            const auto N = 4;

            static lfsr_random<32> _lfsr;
            index_t<N> inner_iteration_count = _lfsr.next({}) % N;

            uint32 total_inner_iteration_count = second(atomically([inner_iteration_count](uint32 prev)
            {
                return cast<uint32>(prev + inner_iteration_count);
            }));

            pipelined_for(inner_iteration_count, [result](index_t<N> inner_i)
            {
                // Something to avoid the loop from being optimized away
                uint32 inner_sum = second(atomically([result](uint32 prev)
                {
                    return cast<uint32>(prev + result);
                }));

                assert(inner_sum > 0);
            });

            uint64 first_cycle = second(atomically([is_first](uint64 prev)
            {
                return is_first ? cycles() : prev;
            }));

            if (is_last && (opt::stall == 0))
            {
                uint64 total_cycles = cycles() - first_cycle;

                if (total_cycles > total_inner_iteration_count)
                {
                    uint64 diff = total_cycles - total_inner_iteration_count;

                    // The total execution time should be close to the total number of executions of the inner pipelined_for
                    unit::assert(tag, diff < 20);
                }
            }
        });
    }
}

// Case where the compiler will infer that fifos can be removed
// because there is no backpressure
class InferNoBackpressure
{
public:
    void run(unit::tag_t tag)
    {
        pipelined_for(512, [tag](index_t<512> i)
        {
            async_exec([tag, i]()
            {
                async_exec([i, tag]()
                {
                    uint32 actual = first(atomically([](uint32 prev)
                    {
                        return cast<uint32>(prev + 1);
                    }));

                    unit::assert_equal(tag, i, actual);
                });
            });
        });
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<SingleThreadedFifo>());
    unit::test<2>(unit::fixture<InferNoBackpressure>());

    static for (const auto depth : 8)
    {
        // enough to get pipeline depths close to the natural fifo size
        // for example, pipeline depth 30 feeding a fifo of depth 32 would cause poor performance
        unit::test<depth + 2>(unit::fixture<DeepPipelineThroughput<depth * 4>>());
    }
}
