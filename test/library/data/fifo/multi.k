// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.fifo.multi
import test.unit as unit
import test.runner

class MultiFifoTest
{
private:
    const auto NUM_FIFOS = 2;
    const auto DEPTH = 4;

    using fifo_idx_t = index_t<NUM_FIFOS>;
    using fifo_cnt_t = count_t<NUM_FIFOS>;
    using data_t = uint8;

    multi_fifo<data_t, DEPTH, NUM_FIFOS, true, true> _fifo;
    bool _error = false;


private:

    void Fill(fifo_idx_t which, data_t seed)
    {
        if (_fifo.count(which) != DEPTH) do
        {
            _fifo.enqueue(which, seed);
            seed++;
        }
        while (_fifo.count(which) != DEPTH);
    }

    void DrainAndValidate(fifo_idx_t which, data_t expected)
    {
        if (_fifo.count(which) != 0) do
        {
            // First check value w/o pop
            data_t actual = _fifo.dequeue(which, false);
            if (actual != expected)
            {
                _error = true;
                assert(false);
            }

            // Now check value with pop
            actual = _fifo.dequeue(which, true);
            if (actual != expected)
            {
                _error = true;
                assert(false);
            }

            expected++;
        }
        while (_fifo.count(which) != 0);
    }

public:
    bool run()
    {
        print("data.fifo.multi test start\n");

        _error = false;

        // Fill both FIFOs
        static for(const auto i : NUM_FIFOS)
        {
            Fill(i, i*DEPTH);
        }

        for (const auto i : NUM_FIFOS)
        {
            if (_fifo.count(i) != DEPTH)
            {
                _error = true;
                assert(false);
            }
        }

        // Drain and validate both FIFOs
        static for(const auto i : NUM_FIFOS)
        {
            DrainAndValidate(i, i*DEPTH);
        }

        // Now validate that blocking calls work
        Fill(0, 0);

        // Spawn a background thread to dequeue the head element
        async_exec([]{
            uint1 bogus = 0;

            for (const auto i : 1000)
            {
                bogus = ~bogus;
            }

            _fifo.dequeue(0, true);
        });

        // Add one more element. Should block until async task above dequeues the element
        _fifo.enqueue(0, DEPTH);

        // Validate correct values are in FIFO
        DrainAndValidate(0, 1);

        print("data.fifo.multi test complete\n");

        return !_error;
    }

}

// Function: Main test entry point for unit tests of data.fifo.multi fifo class.
inline void test_main()
{
    unit::check<1>(unit::fixture<MultiFifoTest>());
}
