// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.fifo.transpose
import data.random.lfsr
import test.unit as unit
import test.runner

template<auto ArraySize>
inline index_t<ArraySize> get_num_to_discard()
{
    static lfsr_random<32> lfsr;
    return static_cast(lfsr.next({}));
}

template<bool TestEnqueueDiscard, bool TestDequeueDiscard>
class tranpose_test
{
private:
    const auto ArraySize = 4;
    const auto ArrayDepth = 32;
    const auto NumArrayIterations = 1024;
    const auto NumSquareIterations = NumArrayIterations / ArraySize;

    fifo<uint32, ArraySize, ArrayDepth, true, true> _fifo;

public:
    void Run(unit::tag_t tag)
    {
        print("data.fifo.tranpose test start TestEnqueueDiscard {TestEnqueueDiscard} TestDequeueDiscard {TestDequeueDiscard}\n");

        // Producer
        async_exec([]
        {
            // For each square
            pipelined_for (NumSquareIterations, [](index_t<NumSquareIterations> square_index)
            {
                if (TestEnqueueDiscard)
                {
                    // This call will return the same sequence as the call in the dequeue code path
                    // which is important because the dequeue path should only validate the content of valid rows
                    index_t<ArraySize> rows_to_discard = get_num_to_discard<ArraySize>();
                    count_t<ArraySize> enqueue_calls = static_cast(ArraySize - rows_to_discard);

                    // For each row
                    pipelined_for (enqueue_calls, [square_index, rows_to_discard, enqueue_calls](index_t<ArraySize> row_index)
                    {
                        bool is_last_row_in_square = (row_index == (enqueue_calls - 1));

                        // Get 4 random values
                        uint32[ArraySize] input_array;

                        static for (const auto i : ArraySize)
                        {
                            input_array[i] = square_index * ArraySize * ArraySize + row_index * ArraySize + i;
                        }

                        _fifo.enqueue_and_discard(input_array, is_last_row_in_square ? rows_to_discard : 0);
                    });
                }
                else
                {
                    // For each row
                    pipelined_for (ArraySize, [square_index](index_t<ArraySize> row_index)
                    {
                        // Get 4 random values
                        uint32[ArraySize] input_array;

                        static for (const auto i : ArraySize)
                        {
                            input_array[i] = square_index * ArraySize * ArraySize + row_index * ArraySize + i;
                        }

                        _fifo.enqueue(input_array);
                    });
                }
            });
        });

        // Consumer

        // For each square
        pipelined_for (NumSquareIterations, [tag](index_t<NumSquareIterations> square_index)
        {
            if (TestDequeueDiscard)
            {
                index_t<ArraySize> columns_to_discard = get_num_to_discard<ArraySize>();
                count_t<ArraySize> dequeue_calls = checked_cast(ArraySize - columns_to_discard);

                assert(dequeue_calls > 0);

                // For some prefix of all columns in the square (last call to dequeue will discard the remainder)
                pipelined_for (dequeue_calls, [tag, square_index, dequeue_calls, columns_to_discard](index_t<ArraySize> column_index)
                {
                    bool is_last_column_in_square = (column_index == (dequeue_calls - 1));

                    uint32[ArraySize] actual = _fifo.dequeue_and_discard(is_last_column_in_square ? columns_to_discard : 0);

                    static for (const auto i : ArraySize)
                    {
                        uint32 expected = column_index + (i * ArraySize) + (square_index * ArraySize * ArraySize);

                        unit::assert_equal(tag, actual[i], expected);
                    }
                });
            }
            else
            {
                count_t<ArraySize> valid_rows = ArraySize;

                if (TestEnqueueDiscard)
                {
                    index_t<ArraySize> rows_to_discard = get_num_to_discard<ArraySize>();
                    valid_rows = checked_cast(ArraySize - rows_to_discard);
                }

                // For each column
                pipelined_for (ArraySize, [tag, square_index, valid_rows](index_t<ArraySize> column_index)
                {
                    uint32[ArraySize] actual = _fifo.dequeue();

                    static for (const auto i : ArraySize)
                    {
                        if (i < valid_rows)
                        {
                            uint32 expected = column_index + (i * ArraySize) + (square_index * ArraySize * ArraySize);

                            unit::assert_equal(tag, actual[i], expected);
                        }
                    }
                });
            }
        });

        print("data.fifo.tranpose test end\n");
    }
}

template<bool TestEnqueueDiscard, bool TestDequeueDiscard>
inline void TestTranspose(unit::tag_t tag)
{
    static tranpose_test<TestEnqueueDiscard, TestDequeueDiscard> _test;
    _test.Run(tag);
}

inline void test_main()
{
    unit::test<1>(TestTranspose<false, false>);
    unit::test<2>(TestTranspose<true, false>);
    unit::test<3>(TestTranspose<false, true>);
}
