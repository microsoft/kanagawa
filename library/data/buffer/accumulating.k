// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.buffer.accumulating
    { accumulating_buffer
    , multi_accumulating_buffer
    }

import data.array
import data.bits
import data.optional
import data.vector as vec

//| Wrapper around array/memory with a configurable way to initial container contents.
template
    < typename T                                    //< Element type.
    , auto Size                                     //< Number of elements.
    , template <typename, auto> typename Container  //< Template used to store elements.
    , bool ResetWithInitialValues                   //< Reset the container with initial values.
                                                    // If false, then the container will be reset with a `[[reset]]` function.
    >
class ContainerWrapper
{
private:
    static if (ResetWithInitialValues)
    {
        Container<T, Size> _container = {};
    }
    else
    {
        Container<T, Size> _container;

        [[reset]] void reset()
        {
            pipelined_for (Size, [](index_t<Size> i)
            {
                _container[i] = {};
            });
        }
    }

public:
    inline T read(index_t<Size> addr)
    {
        return _container[addr];
    }

    inline void write(index_t<Size> addr, T data)
    {
        _container[addr] = data;
    }
}

//| An array of buffers, each of which accumulates a given number of entries and returns them once a
// specified number are enqueued or when a call to enqueue explicitly requests
// that the buffered values be flushed.
template
    < typename T                                            //< Type of values stored in the accumulating buffer.
    , auto Size                                             //< Maximum number of elements that can be stored in each buffer.
    , auto BufferCount                                      //< The number of buffers.
    , auto InputSize = Size                                 //< Maximum number of elements that can be appended to a buffer in one call.
    , template <typename, auto> typename Container = memory //< Template used to store buffer metadata and data.
    , bool ResetWithInitialValues = false                   //< Reset the buffer metadata with initial values.
                                                            // If false, then the metadata will be reset with a `[[reset]]` function.
    , auto FlitCount = 1                                    //< Number of calls to the output callback for each `Size` elements.
    >
class multi_accumulating_buffer
{
public:
    using buffer_index_t = index_t<BufferCount>;

    // A vector of input elements.
    using input_vec_t = vec::vector<T, InputSize>;

private:
    // Total number of elements that can be buffered across all flits
    const auto MultiFlitSize = FlitCount * Size;

    using element_count_t = count_t<MultiFlitSize>;

    ContainerWrapper<index_t<Size>, BufferCount, Container, ResetWithInitialValues> _context_value_count;
    ContainerWrapper<element_count_t, BufferCount, Container, ResetWithInitialValues> _element_count;

public:
    //| A single call cannot appened more than `Size` elements.
    static assert(InputSize <= Size);

    //| Return type for `enqueue`.
    struct output
    {
        //| The number of valid elements in the output.
        count_t<Size> value_count;

        //| Output values.
        T[Size] values;

        //| Flag to indicate whether accumulating_buffer is now empty.
        bool empty;
    };

    //| Write up to `InputSize` values to a buffer.
    //  If there is now a full flit of accumulated data associated with the specified buffer
    //  then the return structure will contain that flit.  A caller can
    //  also explicitly retrieve a partial flit by setting the `flush`
    //  parameter to true.
    //  Returns an `output` in which up to Size values are valid (at most a single flit).
    output enqueue
        ( buffer_index_t buffer_index
        , T[InputSize] values            //< Values to enqueue.
        , count_t<InputSize> value_count //< The number of entries in the values array that are valid.
                                         // Must be between 0 and `InputSize`, inclusive.
        , bool flush                     //< If true, return up to Size elements, even if the buffer is not yet full.
        )
    {
        assert(buffer_index < BufferCount);

        output result;

        // Compute the new counts
        index_t<Size> snapped_context_value_count;
        bool buffer_full;
        atomic
        {
            snapped_context_value_count = _context_value_count.read(buffer_index);

            index_t<Size> new_context_value_count = snapped_context_value_count;

            auto total_value_count = snapped_context_value_count + value_count;
            buffer_full = total_value_count >= Size;
            // Buffer fills
            if (buffer_full)
            {
                // Output is full
                result.value_count = Size;
                // Remaining values are stored in context
                new_context_value_count = total_value_count - Size;
            }
            // Flush (but less than a full buffer)
            else if (flush)
            {
                // Output all values
                assert(total_value_count < Size);
                result.value_count = total_value_count;
                // Context is empty after flush
                new_context_value_count = 0;
            }
            // Buffer is not full and no flush
            else
            {
                // No output
                result.value_count = 0;
                // All values in context
                assert(total_value_count < Size);
                new_context_value_count = total_value_count;
            }

            // Context is empty after this enqueue
            result.empty = new_context_value_count == 0;

            _context_value_count.write(buffer_index, new_context_value_count);
        }

        // Expand input to width = Size
        // Pad with zeros to enable the compiler to optimize parts of the rotation
        T[Size] padded_values = map([values](index_t<Size> i) -> T { return (i < InputSize) ? values[i] : {}; }, indices<Size>());

        // Shift new values to align with context values
        T[Size] shifted_values = rotate_array_left<Size>(padded_values, snapped_context_value_count);

        // Mask where existing buffer has valid data
        auto context_mask = mask_less_than<Size>(snapped_context_value_count);

        // Compute the new context values
        T[Size] snapped_context_values;
        atomic
        {
            static Container<T[Size], BufferCount> context_values;
            snapped_context_values = context_values[buffer_index];
            T[Size] new_context_values = snapped_context_values;

            static for (const auto i : Size)
            {
                // If no full buffer for output, append new data.
                // Otherwise (full buffer for output), overwrite buffer with (rotated) new data. Only the first total_value_count - Size are actually valid.
                new_context_values[i] = (context_mask[i] && !buffer_full) ? new_context_values[i] : shifted_values[i];
            }

            context_values[buffer_index] = new_context_values;
        }

        // Append data to context values to generate output values
        static for (const auto i : Size)
        {
            result.values[i] = context_mask[i] ? snapped_context_values[i] : shifted_values[i];
        }

        return result;
    }

    //| Write up to `InputSize` values to a buffer.  Once `FlitCount` flits are accumulated for a given buffer
    // then `output_callback` is called once per accumulated flit.
    inline void enqueue_with_callback
        ( buffer_index_t buffer_index                   //< Which buffer to append to.
        , input_vec_t values                            //< Values to enqueue.
        , bool flush                                    //< If true, then call the callback for all buffer values associated with the buffer identified by `buffer_index`.
                                                        // The caller must ensure that the number of `buffered elements + values.size <= FlitCount * Size`
                                                        // when true.
        , (index_t<FlitCount>,
           count_t<FlitCount>,
           vec::vector<T, Size>)->void output_callback //< Function which is called once per output flit.
        )
    {
        assert(values.size <= InputSize);

        // This many output flits are stored per buffer
        // -1 because one flit is stored in _element_accumulating_buffer
        const auto OutputFlitsStoredPerBuffer = FlitCount - 1;

        const auto FlitBufferDepth = FlitCount == 1 ? 1 : OutputFlitsStoredPerBuffer * BufferCount;

        // A array without associated size
        using raw_output_flit_t = T[Size];

        // Type that represents maximum number of elements buffered + number of elements
        // appended by one call to `append`.
        using before_flush_size_t = count_t<MultiFlitSize + InputSize>;

        // Track the number of elements queued in the buffer identified by `buffer_index`

        // The number of elements buffered before this call
        element_count_t element_count_before;

        // The number of elements buffered after this call
        element_count_t element_count_after;

        // Number of elements to pass to `output_callback`
        before_flush_size_t num_elements_to_flush = 0;

        atomic
        {
            element_count_before = _element_count.read(buffer_index);

            before_flush_size_t incremented_element_count = checked_cast<before_flush_size_t>(element_count_before + values.size);

            if (flush)
            {
                // When adding elements and flushing
                // the caller must ensure that (buffered element count + values.size) <= MultiFlitSize
                assert(incremented_element_count <= MultiFlitSize);

                // All values will be flushed, nothing will remain
                element_count_after = 0;

                num_elements_to_flush = incremented_element_count;
            }
            else if (incremented_element_count >= MultiFlitSize)
            {
                // Logically, `values.size` elements will be apended
                // and then `MultiFlitSize` elements will be written out
                element_count_after = incremented_element_count - MultiFlitSize;

                // After writing new data
                // at most 1 flit will contain buffered data
                assert(element_count_after <= Size);

                num_elements_to_flush = MultiFlitSize;
            }
            else
            {
                element_count_after = checked_cast<element_count_t>(incremented_element_count);
            }

            _element_count.write(buffer_index, element_count_after);
        }

        // Determine which flit (relative to the buffer indicated by `buffer_index`) will be written into _flit_buffer
        index_t<FlitCount> write_flit_idx = checked_cast(element_count_before / Size);

        assert(write_flit_idx < FlitCount);

        // MultiFlitSize + InputSize to account for the maximum number of elements
        // that can be stored per before plus maximum that can be appended by one call
        // The remaining terms to account for the fact that each output thread
        // writes `Size` elements
        const auto MaxThreadCount = (MultiFlitSize + InputSize + Size - 1) / Size;

        // If `output_callback` is not going to be called, then only 1 thread is needed here
        // it will accumulate into _element_accumulating_buffer, and then potentially write one flit into _flit_buffer
        // If `output_callback` is going to be called, then one thread is used for each call to `output_callback`
        count_t<MaxThreadCount> thread_count = (num_elements_to_flush == 0)
            ? 1 :
            checked_cast<count_t<MaxThreadCount>>((num_elements_to_flush + Size - 1) / Size);

        assert(thread_count <= MaxThreadCount);

        // If flush is true
        // and the input data splits 2 flits
        // (i.e., some parts of the input land at the end of flit N while the rest lands on flit N+1)
        // then the last 2 threads inside of the pipelined_for will call enqueue()
        // the first will pass: size = values.size and flush = false, and will pass flit N to the output callback
        // the second will pass: size = 0 and flush = true, and will pass flit N+1 to the output callback
        index_t<Size> elements_in_enqueue_buffer_before = cast<index_t<Size>>(element_count_before);

        bool has_double_enqueue =
            flush &&
            ((elements_in_enqueue_buffer_before + values.size) > Size);

        pipelined_for(thread_count,
            [buffer_index, values, flush, num_elements_to_flush, write_flit_idx, output_callback, thread_count, has_double_enqueue]
            (index_t<MaxThreadCount> tid)
        {
            // Accumulate full flits.
            // This only occurs on the last thread.  When thread_count > 1
            // it means that some flits already exist in _flit_buffer, all but the last thread
            // simply output those.
            optional<output> enqueue_result = {};

            // When has_double_enqueue is true, there must be at least 2 threads
            assert(!has_double_enqueue || (thread_count >= 2));

            index_t<MaxThreadCount> first_enqueue_thread = checked_cast(thread_count - (has_double_enqueue ? 2 : 1));

            if (tid >= first_enqueue_thread)
            {
                auto enqueue_size = values.size;
                bool enqueue_flush = flush;

                if (has_double_enqueue)
                {
                    if (tid == first_enqueue_thread)
                    {
                        enqueue_flush = false;
                    }
                    else
                    {
                        enqueue_size = 0;
                        enqueue_flush = true;
                    }
                }

                enqueue_result = just(enqueue(buffer_index, values.data, enqueue_size, enqueue_flush));

                assert(!has_double_enqueue || (enqueue_result.value.value_count > 0));
            }

            // auto used here because the last flit is of each buffer is not stored in the _flit_buffer
            // so mem_addr could represent an out-of-bounds address
            // asserts is used when using read results, or writing to the buffer
            auto mem_addr = (buffer_index * OutputFlitsStoredPerBuffer) + tid;

            if (num_elements_to_flush == 0)
            {
                // In case only 1 thread is used to append a flit
                // `tid` will be 0
                // and an offset must be applied to write to the correct flit
                assert(tid == 0);

                mem_addr += write_flit_idx;
            }

            // True if the final flit is being processed
            // There is no storage for this flit in _flit_buffer
            bool is_unstored_flit = (tid == (FlitCount - 1));

            // If _element_accumulating_buffer produced data, then write it to _flit_buffer
            // unless this is the final flit in the buffer (which is not actually stored)
            bool write_enable = enqueue_result.is_valid && (enqueue_result.value.value_count > 0) && !is_unstored_flit;

            raw_output_flit_t prev_flit;

            atomic
            {
                static Container<raw_output_flit_t, FlitBufferDepth> _flit_buffer;

                // Read previously stored flit
                prev_flit = _flit_buffer[mem_addr];

                // Write new flit
                if (write_enable)
                {
                    assert(!is_unstored_flit);
                    assert(enqueue_result.is_valid);

                    // Assert that the read was in-bounds
                    assert(mem_addr < FlitBufferDepth);

                    _flit_buffer[mem_addr] = enqueue_result.value.values;
                }
            }

            if (num_elements_to_flush > 0)
            {
                vec::vector<T, Size> output_flit = {.data = prev_flit, .size = Size};

                // If _element_accumulating_buffer returned a value
                // then output it here
                // This is important because the last flit in a buffer is not stored
                // in `_flit_buffer` (to save resources).
                if (enqueue_result.is_valid && (enqueue_result.value.value_count > 0))
                {
                    output_flit.data = enqueue_result.value.values;

                    output_flit.size = enqueue_result.value.value_count;
                }
                else
                {
                    // Assert that the read was in-bounds
                    assert(mem_addr < FlitBufferDepth);
                }

                assert(output_flit.size > 0);

                output_callback(checked_cast(tid), checked_cast(thread_count), output_flit);
            }
        });
    }
}

//| A buffer that accumulates elements into flits (fixed-sized arrays of elements).
template
    < typename T            //< Type of values stored in the accumulating buffer.
    , auto Size             //< Maximum number of elements that can be stored in a flit.
    , auto InputSize = Size //< Maximum number of elements that can be appended to the buffer in one call.
    , auto FlitCount = 1    //< Maximum number of flits that can be stored.  The output callback is called once for each `Size` elements.
    >
class accumulating_buffer
{
private:
    using buffer_t = multi_accumulating_buffer<T, Size, 1, InputSize, array, true, FlitCount>;

    buffer_t _buffer;

public:
    //| A single call cannot appened more than `Size` elements.
    static assert(InputSize <= Size);

    //| Return type for `enqueue`.
    using output = buffer_t::output;

    // A vector of input elements.
    using input_vec_t = vec::vector<T, InputSize>;

    //| Write up to `InputSize` values to the buffer.  If there are now full flit of buffered data
    //  then the return structure will contain that flit.  A caller can
    //  also explicitly retrieve a partial flit by setting the `flush`
    //  parameter to true.
    //  Returns an `output` in which up to Size values are valid (at most a single flit).
    output enqueue
        ( T[InputSize] values            //< An array of values to enqueue.
        , count_t<InputSize> value_count //< The number of entries in the values array that are valid.
                                         // Must be between 0 and `InputSize`, inclusive.
        , bool flush                     //< If true, return up to Size elements, even if the buffer is not yet full.
        )
    {
        return _buffer.enqueue(0, values, value_count, flush);
    }

    //| Write up to `InputSize` values to the buffer.  Once `FlitCount` flits are accumulated
    // then `output_callback` is called once per accumulated flit.
    inline void enqueue_with_callback
        ( input_vec_t values                            //< Values to enqueue.
        , bool flush                                    //< If true, then call the callback for all buffer values associated with the buffer identified by `buffer_index`.
        , (index_t<FlitCount>,
           count_t<FlitCount>,
           vec::vector<T, Size>)->void output_callback //< Function which is called once per output flit.
        )
    {
        return _buffer.enqueue_with_callback(0, values, flush, output_callback);
    }
}
