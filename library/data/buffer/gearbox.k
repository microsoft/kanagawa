// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.buffer.gearbox
    { gearbox
    }

import sync.atomic

template <typename From, typename To>
using input_count_t = uint<clog2((bitsizeof(From) > bitsizeof(To) ? 1 : bitsizeof(To) / bitsizeof(From)) + 1)>;

template <typename From, typename To>
using output_count_t = uint<clog2((bitsizeof(To) > bitsizeof(From) ? 1 : bitsizeof(From) / bitsizeof(To)) + 1)>;


//| Used by the `gearbox_context` struct to contain the input and output
// array values during conversion.
//
// Users should use only the `output_buff` member of this union.
template
    < typename From //< Type of the value to be converted.
    , typename To   //< Type of the result of the conversion.
    >
union gearbox_in_out
{
    //| An array of `To` values containing the result of the conversion.
    To[bitsizeof(To) > bitsizeof(From) ? 1 : bitsizeof(From) / bitsizeof(To)] output_buff;
    //| An array of `From` values. This is used internally by the
    // `gearbox_with_context` function.
    From[bitsizeof(From) > bitsizeof(To) ? 1 : bitsizeof(To) / bitsizeof(From)] input_buff;
}

//| Provides context during and after conversion by `gearbox_with_context`.
template
    < typename From //< Type of the value to be converted.
    , typename To   //< Type of the result of the conversion.
    >
struct gearbox_context
{
    //| A union of the input and output buffers.
    gearbox_in_out<From, To> u;
    //| Number of elements currently used in the input buffer.
    input_count_t<From, To> num_input_elements;
    //| Number of elements currently used in the output buffer.
    output_count_t<From, To> num_output_elements;
    //| Number of converted bytes in the output.
    count_t<bytesizeof(To)> num_bytes;
    //| Indicates whether the output buffer is valid.
    bool valid;
}

//| Convert between values of differing bit sizes. Return a `gearbox_context`
// containing the current state of the conversion.
template <typename From, typename To>
inline gearbox_context<From, To> gearbox_with_context
    ( gearbox_context<From, To> context //< Current state of the conversion.
    , From input                        //< The value to be converted.
    , bool start                        //< When `true` indicates that this call
                                        // is the first of possibly several
                                        // calls performing the conversion.
    , bool last                         //< When `true` indicates that this call
                                        // is processing the last portion of
                                        // the input.
    )
{
    static assert((bitsizeof To % 8 == 0) && (bitsizeof From % 8 == 0));

    // Ensure a new context on first call
    gearbox_context<From, To> empty_context = {};
    context = start ? empty_context : context;

    // Ensure we're not being called too many times.
    const auto input_buffer_width = bitsizeof(context.u.input_buff) / bitsizeof(From);
    assert(context.num_input_elements < input_buffer_width);

    if (bitsizeof From > bitsizeof To)
    {
        // gearbox_with_context is called a single time,
        context.u.input_buff[0] = input;
        context.num_input_elements = 1;
        context.num_output_elements = bitsizeof(From) / bitsizeof(To);
        context.num_bytes = bytesizeof(To);
        context.valid = true;
    }
    else if (bitsizeof To > bitsizeof From)
    {
        // gearbox_with_context is called multiple times,
        // once for each of the smaller values
        context.u.input_buff[context.num_input_elements] = input;
        ++context.num_input_elements;
        context.num_bytes += bytesizeof(From);
        if (context.num_input_elements == input_buffer_width || last == true)
        {
            context.num_output_elements = 1;
            context.valid = true;
        }
    }
    else
    {
        // Pass-through
        // gearbox_with_context is called a single time
        context.u.output_buff[0] = input;
        context.num_input_elements = 1;
        context.num_output_elements = 1;
        context.num_bytes = bytesizeof(To);
        context.valid = true;
    }

    return context;
}

//| A mechanism for converting values among differing types.
template
    < typename From                                        //< Type of the value to be
                                                           // converted.
    , typename To                                          //< Type of the result of the
                                                           // conversion.
    >
inline void gearbox(From input, bool start, bool last, (To, bool, count_t<bytesizeof(To)>) -> void callback)
{
    const auto output_array_length = bitsizeof(To) > bitsizeof(From) ? 1 : bitsizeof(From) / bitsizeof(To);

    gearbox_context<From, To> local_context = second(atomically([input, start, last](gearbox_context<From, To> context)
    {
        return gearbox_with_context<From, To>(context, input, start, last);
    }));

    if (local_context.valid)
    {
        pipelined_for(output_array_length, [local_context, callback](index_t<output_array_length> tid)
        {
            bool last = false;
            auto bytes = bytesizeof(To);
    
            if (tid == local_context.num_output_elements - 1)
            {
                bytes -= (bytesizeof(To)) - local_context.num_bytes;
                last = true;
            }
    
            callback(local_context.u.output_buff[tid], last, bytes);
        });
    }
}