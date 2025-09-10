// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.representation
    { binary_to_one_hot
    , onehot_mux
    , endian_change
    }

import data.array
import data.bits

//| Generate a onehot of the specified width.  A onehot is an array of booleans
// with a single value set to true.  Calling with `OutputWidth` 8 and `index` 1
// returns an array of 8 bools with only bool 1 set to true.  Or in binary,
// 00000010.
template<auto OutputWidth, typename T>
inline bool[OutputWidth] binary_to_one_hot(T index)
{
    // Make sure x isn't too large for the output array.
    // For example, it's not possible to set the 5th boolean in a 3 element array.
    assert(index < OutputWidth);

    bool[OutputWidth] result = {};
    result[index] = true;
    return result;
}

//| Similar to `mux`, but takes a `onehot` rather than an index as the first parameter.
// Also takes an array rather than a variable number of arguments for the selection list.
// The least significant bit in the onehot corresponds to index 0 in the data array.
//
// #### __Examples__
//
//     >>> onehot_mux<uint2, 4>(1, {0, 1, 2, 3});
//     0
//
//     >>> onehot_mux<uint2, 4>(2, {0, 1, 2, 3});
//     1
//
//     >>> onehot_mux<uint2, 4>(4, {0, 1, 2, 3});
//     2
//
// The `onehot` must be valid which means it has exactly one bit set.
// Like `mux`, `onehot_mux` requires that `Count` is a power of two.
template<typename T, auto Count>
inline auto onehot_mux(uint<Count> onehot, T[Count] data)
{
    uint<bitsizeof(T)>[Count] map;
    bool[Count] bool_onehot = cast<bool[Count]>(onehot);

    // Can't use map here because that doesn't pass the index to the map function.
    static for(const auto i : Count)
    {
        if (bool_onehot[i])
        {
            map[i] = cast<uint<bitsizeof(T)>>(data[i]);
        }
    }

    uint<bitsizeof(T)> result = reduce(bitwise_or<uint<bitsizeof(T)>>, map);

    return cast<T>(result);
}

//| Change a value from big endian to little endian, or vice versa.
// The input type must be of a size that's a multiple of 8 bits.
//
// #### __Example__
//
//     >>> endian_change(0x12345678)
//     0x78563412
template<typename T>
inline T endian_change(T input)
{
    uint8[bytesizeof(T)] inputAsArray = cast<uint8[bytesizeof(T)]>(input);
    uint8[bytesizeof(T)] result;

    static for (const auto i : bytesizeof(T))
    {
        result[i] = inputAsArray[static(bytesizeof(T) - i - 1)];
    }

    return cast<T>(result);
}