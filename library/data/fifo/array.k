// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
An array of fifos
*/

module data.fifo.array
    { fifo_array
    }

import data.fifo
    

//| An array of FIFOs.  
template
    < typename T                                             //< Datatype stored in each FIFO.
    , auto Depth                                             //< Number of elements stored in each FIFO.
    , auto ArraySize                                         //< The number of FIFOs.
    , template <typename, auto> typename FifoType = FIFO     //< Type of FIFO to use.
    >
class fifo_array
{
public:
    //| Index that identifies one FIFO.
    using fifo_index_t = index_t<ArraySize>;

private:
    FifoType<T, Depth>[ArraySize] _fifos;

public:
    //| Enqueue a value into specified FIFO.
    template
        < fifo_index_t FifoIndex //< The index of the FIFO to store into.
        >
    void enqueue_one
        ( T value //< Value to store into the selected FIFO.
        )
    {
        // Validate the index (in case of a non-power of two ArraySize)
        static assert(FifoIndex < ArraySize);

        _fifos[FifoIndex].enqueue(value);
    }

    //| Enqueue zero or one values into each FIFO.
    void enqueue_many
        ( optional<T>[ArraySize] values //< Array of values to store.  `is_valid` indicates if value should be stored.
        )
    {
        static for (const auto i : ArraySize)
        {
            if (values[i].is_valid)
            {
                _fifos[i].enqueue(values[i].value);
            }
        }
    }

    //| Enqueue one value into each FIFO.
    void enqueue_all
        ( T[ArraySize] values //< Array of values to store.
        )
    {
        static for (const auto i : ArraySize)
        {
            _fifos[i].enqueue(values[i]);
        }
    }

    //| Dequeue a value from specified FIFO.
    template
        < fifo_index_t FifoIndex //< The index of the FIFO to store into.
        >
    T dequeue_one()
    {
        // Validate the index (in case of a non-power of two ArraySize)
        static assert(FifoIndex < ArraySize);

        return _fifos[FifoIndex].dequeue();
    }

    //| Dequeue zero or one values from each FIFO.
    optional<T>[ArraySize] dequeue_many
        ( bool[ArraySize] mask //< An array of boolean values, one per FIFO.
                               // If `mask[i] == true`, then an element will be dequeued from FIFO `i`
                               // and stored in the result array at index `i`.
                               // If `mask[i] == false`, then element `i` in the result is undefined.
        )
    {
        optional<T>[ArraySize] result = {};

        static for (const auto i : ArraySize)
        {
            if (mask[i])
            {
                result[i] = make_optional(true, _fifos[i].dequeue());
            }
        }

        return result;
    }

    //| Dequeue one values from each FIFO.
    T[ArraySize] dequeue_all()
    {
        T[ArraySize] result = {};

        static for (const auto i : ArraySize)
        {
            result[i] = _fifos[i].dequeue();
        }

        return result;
    }
}
