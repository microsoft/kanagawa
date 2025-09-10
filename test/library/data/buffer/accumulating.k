// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.array
import data.buffer.accumulating
import data.random.lfsr
import data.vector as vec
import sync.atomic
import test.unit as unit
import test.runner

class AccumulatingBufferTests
{
private:
    const auto accumulate_count = 4;
    accumulating_buffer<uint32, accumulate_count> _buffer;

    // A buffer which can hold the same amount of data as _buffer
    // but can only accept 2 elements/call as input
    const auto small_input_count = 2;
    accumulating_buffer<uint32, accumulate_count, small_input_count> _small_input_buffer;

    bool BasicTest()
    {
        bool result = true;

        uint32[accumulate_count] values;

        // Enqueue values 10, 11, 12, but don't release yet.
        values[0] = 10;
        values[1] = 11;
        values[2] = 12;
        auto q_result = _buffer.enqueue(values, 3, false);

        if (q_result.value_count != 0)
        {
            print("Enqueue expected to return 0 values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != false)
        {
            print("Enqueue expected to return with 'empty' member not set\n");
            result = false;
        }

        // Now enqueue 13 (the 4th value enqueued) and don't explicitly request a flush - a flush
        // should happen automatically.
        values[0] = 13;
        q_result = _buffer.enqueue(values, 1, false);

        if (q_result.value_count != accumulate_count)
        {
            print("Enqueue expected to return {accumulate_count} values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != true)
        {
            print("Enqueue expected to return with 'empty' member set\n");
            result = false;
        }

        for (const auto i : accumulate_count)
        {
            uint32 expected = i + 10;
            if (q_result.values[i] != expected)
            {
                print("Enqueue expected value {expected} at index {i} actual {q_result.values[i]}\n");
                result = false;
            }
        }

        return result;
    }

    bool MixedSizesTest()
    {
        bool result = true;
        uint32[accumulate_count] values;

        // Enqueue 2 values: 20 and 21.
        values[0] = 20;
        auto q_result = _buffer.enqueue(values, 1, false);

        if (q_result.value_count != 0)
        {
            print("Enqueue expected to return 0 values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != false)
        {
            print("Enqueue expected to return with 'empty' member not set\n");
            result = false;
        }

        values[0] = 21;
        q_result = _buffer.enqueue(values, 1, false);

        if (q_result.value_count != 0)
        {
            print("Enqueue expected to return 0 values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != false)
        {
            print("Enqueue expected to return with 'empty' member not set\n");
            result = false;
        }

        // Now enqueue 4 values: 22, 23, 24, 25. A flush should happen automatically with values
        // 20 - 23 inclusive.
        for (const auto i : accumulate_count)
        {
            values[i] = i + 22;
        }

        q_result = _buffer.enqueue(values, accumulate_count, false);

        if (q_result.value_count != accumulate_count)
        {
            print("Enqueue expected to return {accumulate_count} values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != false)
        {
            print("Enqueue expected to return with 'empty' member not set\n");
            result = false;
        }

        for (const auto i : accumulate_count)
        {
            uint32 expected = i + 20;
            if (q_result.values[i] != expected)
            {
                print("Enqueue expected value {expected} at index {i} actual {q_result.values[i]}\n");
                result = false;
            }
        }

        // Enqueue 2 more values.  This also leaves the buffer in a clean state for other tests.
        values[0] = 26;
        values[1] = 27;
        q_result = _buffer.enqueue(values, 2, false);

        if (q_result.value_count != accumulate_count)
        {
            print("Enqueue expected to return {accumulate_count} values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != true)
        {
            print("Enqueue expected to return with 'empty' member set\n");
            result = false;
        }

        for (const auto i : accumulate_count)
        {
            uint32 expected = i + 24;
            if (q_result.values[i] != expected)
            {
                print("Enqueue expected value {expected} at index {i} actual {q_result.values[i]}\n");
                result = false;
            }
        }

        return result;
    }

    bool EarlyFlushTest()
    {
        bool result = true;
        uint32[accumulate_count] values;

        // Enqueue 2 values: 40 and 41.
        values[0] = 40;
        values[1] = 41;
        auto q_result = _buffer.enqueue(values, 2, false);

        if (q_result.value_count != 0)
        {
            print("Enqueue expected to return 0 values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != false)
        {
            print("Enqueue expected to return with 'empty' member not set\n");
            result = false;
        }

        // Enqueue a 3rd value and request an early flush.
        values[0] = 42;
        q_result = _buffer.enqueue(values, 1, true);

        if (q_result.value_count != 3)
        {
            print("Enqueue expected to return {3} values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != true)
        {
            print("Enqueue expected to return with 'empty' member set\n");
            result = false;
        }

        for (const uint8 i : q_result.value_count)
        {
            uint32 expected = i + 40;
            if (q_result.values[i] != expected)
            {
                print("Enqueue expected value {expected} at index {i} actual {q_result.values[i]}\n");
                result = false;
            }
        }

        return result;
    }

    bool EmptyFlushTest()
    {
        bool result = true;
        uint32[accumulate_count] values;

        // Enqueue 2 values: 50 and 51.
        values[0] = 50;
        values[1] = 51;
        auto q_result = _buffer.enqueue(values, 2, false);

        if (q_result.value_count != 0)
        {
            print("Enqueue expected to return 0 values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != false)
        {
            print("Enqueue expected to return with 'empty' member not set\n");
            result = false;
        }

        // Call enqueue with no valid values and request an early flush.
        // Should return the 2 values enqueued before, 50 and 51.
        values[0] = 98;
        values[1] = 99;
        q_result = _buffer.enqueue(values, 0, true);

        if (q_result.value_count != 2)
        {
            print("Enqueue expected to return {2} values, actually returned {q_result.value_count}\n");
            result = false;
        }
        if (q_result.empty != true)
        {
            print("Enqueue expected to return with 'empty' member set\n");
            result = false;
        }

        for (const uint8 i : q_result.value_count)
        {
            uint32 expected = i + 50;
            if (q_result.values[i] != expected)
            {
                print("Enqueue expected value {expected} at index {i} actual {q_result.values[i]}\n");
                result = false;
            }
        }

        return result;
    }

    // Pass the same data through both buffers
    // and compare the results
    void SmallInputTest(unit::tag_t tag)
    {
        pipelined_for (1024, [tag](uint32 tid)
        {
            static lfsr_random<small_input_count> lfsr;

            count_t<small_input_count> input_count = (lfsr.next({}) % small_input_count) + 1;
            assert(input_count > 0);
            assert(input_count <= small_input_count);

            uint32[small_input_count] values;
            static for (const auto i : small_input_count)
            {
                values[i] = tid * small_input_count + i;
            }

            bool flush = (0 == (tid % 32));

            auto expected = _buffer.enqueue(reinterpret_cast<uint32[accumulate_count]>(values), input_count, flush);
            auto actual   = _small_input_buffer.enqueue(values, input_count, flush);

            unit::assert_equal(tag, expected.empty, actual.empty);

            if (!expected.empty)
            {
                unit::assert_equal(tag, expected.value_count, actual.value_count);

                static for (const auto i : accumulate_count)
                {
                    if (i < expected.value_count)
                    {
                        unit::assert_equal(tag, expected.values[i], actual.values[i]);
                    }
                }
            }
        });
    }

    // Input for each stream is 0, 1, 2, 3, ...
    // Buffer index is encoded in upper bits
    // This returns up to accumulate_count elements of input/expected outputs
    const auto buffer_count = 4;

    inline uint32[accumulate_count] GetMultiBufferData(index_t<buffer_count> buffer_index, count_t<accumulate_count> valid_count)
    {
        auto base_array = first(atomically([buffer_index, valid_count](uint32[buffer_count] prev)
        {
            uint32[buffer_count] result = prev;

            result[buffer_index] += valid_count;

            return result;
        }));

        uint32[accumulate_count] result;

        static for (const auto i : accumulate_count)
        {
            if (i < valid_count)
            {
                result[i] = (base_array[buffer_index] + i) | (buffer_index << 24);
            }
        }

        return result;
    }

    void MultiBufferTest(unit::tag_t tag)
    {
        pipelined_for(1024, [tag](uint32 tid)
        {
            static multi_accumulating_buffer<uint32, accumulate_count, buffer_count, accumulate_count, memory, false> _multi_buffer;

            static lfsr_random<32> _lfsr;

            uint32 r = _lfsr.next({});

            index_t<buffer_count> buffer_index = static_cast(r);
            r = r / buffer_count;

            count_t<accumulate_count> valid_count = r % accumulate_count;
            r = r / accumulate_count;

            bool flush = 0 == (r % 2);

            uint32[accumulate_count] input_data = GetMultiBufferData(buffer_index, valid_count);

            auto actual = _multi_buffer.enqueue(buffer_index, input_data, valid_count, flush);

            uint32[accumulate_count] expected_data = GetMultiBufferData(buffer_index, actual.value_count);

            static for (const auto i : accumulate_count)
            {
                if (i < actual.value_count)
                {
                    unit::assert_equal(tag, expected_data[i], actual.values[i]);
                }
            }
        });
    }

public:
    inline void Run()
    {
        unit::check<1>(BasicTest);
        unit::check<2>(MixedSizesTest);
        unit::check<3>(EarlyFlushTest);
        unit::check<4>(EmptyFlushTest);
        unit::test<5>(SmallInputTest);
        unit::test<6>(MultiBufferTest);
    }
}

template
        < auto FlitCount
        , auto Size
        , auto InputSize
        >
inline void EnqueueWithCallbackTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        using buffer_t = accumulating_buffer<uint32, Size, InputSize, FlitCount>;

        buffer_t _buffer;

        uint32 _buffer_elements_appended = 0;

        uint32 _buffer_elements_received = 0;

        lfsr_random<16> _input_count_lfsr;

        const auto ThreadCount = 1024;

        const auto MaxBufferedElements = Size * FlitCount;

    public:
        void test(unit::tag_t tag)
        {
            // Each iteration of the outer loop will flush each bucket
            pipelined_for(8, [tag](uint32 flush_tid)
            {
                pipelined_for(ThreadCount, [tag](uint32 outer_tid)
                {
                    // +1 to ensure that InputSize case is tested
                    count_t<InputSize> input_element_count = (_input_count_lfsr.next({}) % InputSize) + 1;

                    bool flush = outer_tid == (ThreadCount - 1);

                    uint32 num_elements_buffered = first(atomically([flush, input_element_count](uint32 prev)
                    {
                        uint32 result = 0;

                        if (flush)
                        {
                            result = 0;
                        }
                        else
                        {
                            result = prev + input_element_count;

                            if (result >= MaxBufferedElements)
                            {
                                result -= MaxBufferedElements;
                            }
                        }

                        return result;
                    }));

                    if (flush)
                    {
                        // When flush is true
                        // ensure that new data will not spill onto the next burst of flits
                        if ((num_elements_buffered + input_element_count) > MaxBufferedElements)
                        {
                            input_element_count = 0;
                        }
                    }

                    vec::vector<uint32, InputSize> input_vec = {.size = input_element_count};

                    atomic
                    {
                        input_vec.data[0]  = _buffer_elements_appended;

                        _buffer_elements_appended += input_element_count;
                    }

                    input_vec.data = map_indices([input_vec](index_t<InputSize> i) -> uint32
                    {
                        return input_vec.data[0] + i;
                    });

                    _buffer.enqueue_with_callback(
                        input_vec,
                        flush,
                        [tag](index_t<FlitCount> flit_index, count_t<FlitCount> flit_count, vec::vector<uint32, Size> output_vec)
                        {
                            unit::assert(tag, output_vec.size > 0);

                            uint32 expected_value_0;

                            atomic
                            {
                                expected_value_0 = _buffer_elements_received;

                                _buffer_elements_received = expected_value_0 + output_vec.size;
                            }

                            static for (const auto i : Size)
                            {
                                if (i < output_vec.size)
                                {
                                    unit::assert_equal(tag, expected_value_0 + i, output_vec.data[i]);
                                }
                            }
                        });
                });
            });

            unit::assert_equal(tag, _buffer_elements_appended, _buffer_elements_received);
        }
    }

    static Helper _helper;
    _helper.test(tag);
}

template
        < auto FlitCount
        , auto BufferCount
        , auto InputSize
        , auto OutputSize
        , template <typename, auto> typename Container
        , bool ResetWithInitialValues
        >
inline void MultiEnqueueWithCallbackTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        using buffer_t = multi_accumulating_buffer<uint32, OutputSize, BufferCount, InputSize, Container, ResetWithInitialValues, FlitCount>;

        buffer_t _buffer;

        memory<uint32, BufferCount> _buffer_elements_appended = {};

        memory<uint32, BufferCount> _buffer_elements_received = {};

        memory<uint32, BufferCount> _buffered_element_count = {};

        lfsr_random<16> _input_count_lfsr;

        lfsr_random<32> _buffer_index_lfsr;

        const auto ThreadCount = 4096;

        const auto MaxBufferedElements = FlitCount * OutputSize;

    public:
        void test(unit::tag_t tag)
        {
            // Each iteration of the outer loop will flush each bucket
            pipelined_for(64, [tag](uint32 flush_tid)
            {
                pipelined_for(ThreadCount + BufferCount, [tag](uint32 outer_tid)
                {
                    // +1 to ensure that InputSize case is tested
                    count_t<InputSize> input_element_count = (_input_count_lfsr.next({}) % InputSize) + 1;

                    index_t<BufferCount> buffer_index = _buffer_index_lfsr.next({}) % BufferCount;

                    bool flush = outer_tid >= ThreadCount;

                    if (flush)
                    {
                        buffer_index = outer_tid - ThreadCount;
                    }

                    uint32 num_elements_buffered;

                    atomic
                    {
                        num_elements_buffered = _buffered_element_count[buffer_index];

                        uint32 new_num_elements_buffered = 0;

                        if (flush)
                        {
                            new_num_elements_buffered = 0;
                        }
                        else
                        {
                            new_num_elements_buffered = num_elements_buffered + input_element_count;

                            if (new_num_elements_buffered >= MaxBufferedElements)
                            {
                                new_num_elements_buffered -= MaxBufferedElements;
                            }
                        }

                        _buffered_element_count[buffer_index] = new_num_elements_buffered;
                    }

                    if (flush)
                    {
                        // When flush is true
                        // ensure that new data will not spill onto the next burst of flits
                        if ((num_elements_buffered + input_element_count) > MaxBufferedElements)
                        {
                            input_element_count = 0;
                        }
                    }

                    buffer_t::input_vec_t input_vec = {.size = input_element_count};

                    atomic
                    {
                        // buffer_index << 24 to generate a different stream of inputs for each buffer
                        input_vec.data[0]  = (buffer_index << 24) | _buffer_elements_appended[buffer_index];

                        _buffer_elements_appended[buffer_index] += input_element_count;
                    }

                    input_vec.data = map_indices([input_vec](index_t<InputSize> i) -> uint32
                    {
                        return input_vec.data[0] + i;
                    });

                    _buffer.enqueue_with_callback(
                        buffer_index,
                        input_vec,
                        flush,
                        [buffer_index, tag](index_t<FlitCount> flit_index, count_t<FlitCount> flit_count, vec::vector<uint32, OutputSize> output_vec)
                        {
                            unit::assert(tag, output_vec.size > 0);

                            uint32 expected_value_0;

                            atomic
                            {
                                expected_value_0 = _buffer_elements_received[buffer_index];

                                _buffer_elements_received[buffer_index] = expected_value_0 + output_vec.size;
                            }

                            expected_value_0 |= (buffer_index << 24);

                            static for (const auto i : OutputSize)
                            {
                                if (i < output_vec.size)
                                {
                                    unit::assert_equal(tag, expected_value_0 + i, output_vec.data[i]);
                                }
                            }
                        });
                });
            });

            pipelined_for(BufferCount, [tag](index_t<BufferCount> buffer_index)
            {
                unit::assert_equal(tag, _buffer_elements_appended[buffer_index], _buffer_elements_received[buffer_index]);
            });
        }
    }

    static Helper _helper;
    _helper.test(tag);
}

inline void test_main()
{
    print("data.buffer.accumulating test start\n");

    static AccumulatingBufferTests _accumulating_buffer_tests;
    _accumulating_buffer_tests.Run();

    unit::test<7>(EnqueueWithCallbackTest<1, 4, 2>);
    unit::test<8>(EnqueueWithCallbackTest<2, 4, 2>);
    unit::test<9>(MultiEnqueueWithCallbackTest<4, 32, 2, 4, memory, true>);
    unit::test<10>(MultiEnqueueWithCallbackTest<2, 4, 4, 8, array, false>);

    print("data.buffer.accumulating test complete\n");
}
