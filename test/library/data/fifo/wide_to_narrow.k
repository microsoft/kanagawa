// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.fifo.wide_to_narrow
import data.random.lfsr
import test.runner
import test.unit as unit

template<bool TestDiscard>
class wide_to_narow_fifo_test
{
private:
    const auto ArraySize = 4;
    const auto ArrayDepth = 32;
    const auto NumArrayIterations = 1024;
    const auto NumScalarIterations = NumArrayIterations * ArraySize;

    bool _success = true;

    fifo<uint32, ArraySize, ArrayDepth, true, true> _fifo;

public:
    bool run()
    {
        print("data.fifo.wide_to_narrow test start TestDiscard {TestDiscard}\n");

        // Producer
        async_exec([]
        {
            pipelined_for(NumArrayIterations, [](index_t<NumArrayIterations> tid)
            {
                // Get 4 random values
                uint32[ArraySize] input_array = pipelined_map<ArraySize>(ArraySize, [](index_t<ArraySize> tid)
                {
                    static lfsr_random<32> lfsr;

                    return lfsr.next({});
                });

                _fifo.enqueue(input_array);
            });
        });

        // Consumer

        // When testing discard, limit the number of iterations to prevent underflow
        // because each iteration could discard up to ArraySize-1 elemenets
        const auto test_iterations = TestDiscard ? NumScalarIterations / 4 : NumScalarIterations;

        pipelined_for(test_iterations, [](index_t<NumScalarIterations> tid)
        {
            if (TestDiscard)
            {
                // Randomly determine how many elements to discard
                static lfsr_random<clog2(ArraySize)> count_lfsr;

                index_t<ArraySize> discard_count = count_lfsr.next({});

                count_t<ArraySize> total_count = discard_count + 1;

                uint32 actual = _fifo.dequeue_and_discard(discard_count);

                pipelined_for(total_count, [total_count, actual](index_t<ArraySize> i)
                {
                    static lfsr_random<32> lfsr;

                    uint32 expected = lfsr.next({});

                    // Only check output for i==0
                    // other outputs were discarded
                    if ((i == 0) && (actual != expected))
                    {
                        print("mismatch (discard path) actual:  {actual} expected:  {expected}\n");
                        _success = false;
                    }
                });
            }
            else
            {
                uint32 actual = _fifo.dequeue();

                static lfsr_random<32> lfsr;

                uint32 expected = lfsr.next({});

                if (actual != expected)
                {
                    print("mismatch actual:  {actual} expected:  {expected}\n");
                    _success = false;
                }
            }
        });


        print("data.fifo.wide_to_narrow test end\n");

        return _success;
    }
}

inline void test_main()
{
    unit::check<1>(unit::fixture<wide_to_narow_fifo_test<false>>());
    unit::check<2>(unit::fixture<wide_to_narow_fifo_test<true>>());
}
