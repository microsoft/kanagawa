// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Bit manipulation functions for unsigned integers.
*/
module data.bits
    { rotl
    , rotr
    , highest_one
    , lowest_one
    , pop_count
    , divide_and_roundup
    , roundup_to_pow2
    , reverse
    , reverse_bytes
    , //| = Bitmask
      mask_greater_equal
    , mask_less_than
    , mask_greater_than
    , mask_less_equal

    , bitcount_t
    , bitindex_t
    , optional
    , reduction
    , reduction_and
    , reduction_or
    , reduction_xor
    , reduction_xnor
    , reduction_nor
    , reduction_nand
    , bitwise_and
    , bitwise_or
    , bitwise_xor
    , bitwise_nand
    , bitwise_nor
    , bitwise_xnor
    }

import data.array
import data.optional
import type.coerce
import type.stdtype

//| Rotate the specified value left by the specified amount.
//
// #### __Examples__
//
//     >>> rotl(0x9, 0);
//     0x9
//
//     >>> rotl(0x9, 1);
//     0x3
//
//     >>> rotl(0x9, 2);
//     0x6
//
//     >>> rotl(0x9, 3);
//     0xC
template <typename T>
inline T rotl(T value, bitindex_t<T> amount)
{
    const auto Width = bitsizeof(T);
    assert(amount <= Width);

    return cast<T>(value << amount) | cast<T>(value >> (Width - amount));
}

//| Rotate the specified value right by the specified amount.
//
// #### __Examples__
//
//     >>> rotr(0x9, 0);
//     0x9
//
//     >>> rotr(0x9, 1);
//     0xC
//
//     >>> rotr(0x9, 2);
//     0x6
//
//     >>> rotr(0x9, 3);
//     0x3
template <typename T>
inline T rotr(T value, bitindex_t<T> amount)
{
    const auto Width = bitsizeof(T);
    assert(amount <= Width);

    return cast<T>(value >> amount) | cast<T>(value << (Width - amount));
}

//| Find the index of the highest 1 bit in the value. The result is an
// `optional<bitindex_t<T>>` with the `is_valid` field indicating if there are
// any 1 bits in the value. When `is_valid` is true then `value` is the index
// of the highest 1 bit.
//
// #### __Examples__
//
//     >>> highest_one(0);
//     {0x0, 0x0}
//
//     >>> highest_one(1);
//     {0x1, 0x0}
//
//     >>> highest_one(0x32);
//     {0x1, 0x5}
template <typename T>
inline optional<bitindex_t<T>> highest_one(T value)
{
    const auto Width = bitsizeof(T);
    uint1[Width] value_as_bits = cast<uint1[Width]>(value);

    optional<bitindex_t<T>>[Width] in;
    static for (const auto i : Width)
    {
        in[i].is_valid = value_as_bits[i] == 1;
        in[i].value = i;
    }
    return last_valid<bitindex_t<T>, Width>(in);
}

//| Find the index of the lowest 1 bit in the value. The result is an
// `optional<bitindex_t<T>>` with the `is_valid` field indicating if there are
// any 1 bits in the value. When `is_valid` is true then `value` is the index
// of the lowest 1 bit.
//
// #### __Examples__
//
//     >>> lowest_one(0);
//     {0x0, 0x0}
//
//     >>> lowest_one(1);
//     {0x1, 0x0}
//
//     >>> lowest_one(0x34);
//     {0x1, 0x2}
template <typename T>
inline optional<bitindex_t<T>> lowest_one(T value)
{
    const auto Width = bitsizeof(T);
    uint1[Width] value_as_bits = cast<uint1[Width]>(value);

    optional<bitindex_t<T>>[Width] in;
    static for (const auto i : Width)
    {
        in[i].is_valid = value_as_bits[i] == 1;
        in[i].value = i;
    }
    return first_valid<bitindex_t<T>, Width>(in);
}

//| Return a count of the number of bits set in the value.
//
// #### __Examples__
//
//     >>> pop_count(0);
//     0x0
//
//     >>> pop_count(1);
//     0x1
//
//     >>> pop_count(0x34);
//     0x3
template <typename T>
inline bitcount_t<T> pop_count(T value)
{
    // Reduction tree to improve area of synthesized structure
    const auto Width = bitsizeof(T);
    bitcount_t<T>[Width] cts;

    uint1[Width] value_as_bits = cast<uint1[Width]>(value);

    // Populate cts array from initial bit vector
    static for (const auto i : Width)
    {
        cts[i] = value_as_bits[i];
    }

    return sum<bitcount_t<T>>(cts);
}

//| Divide dividend by divisor and round the result up.
// Divisor must be an unsigned power of 2, known at compile time.
// Calling with (x, 32) returns the equivalent of (x + 31) / 32.
//
// #### __Examples__
//
//     >>> divide_and_roundup(0, 0x20);
//     0x0
//
//     >>> divide_and_roundup(1, 0x20);
//     0x1
//
//     >>> divide_and_roundup(0x20, 0x20);
//     0x1
//
//     >>> divide_and_roundup(0x81f, 0x40);
//     0x21
template<typename DividendType, typename DivisorType>
inline DividendType divide_and_roundup(DividendType dividend, const DivisorType divisor)
{
    return (dividend + (divisor - 1)) / divisor;
}

//| Round up a value to the next larger power of two. For example,
// all values from 33 through 64 (inclusive) round up to 64.
// OutputType must be at least one bit larger than InputType.
//
// #### __Examples__
//
//     >>> roundup_to_pow2<uint15, uint16>(0);
//     {0x0, 0x0}
//
//     >>> roundup_to_pow2<int7, int8>(0x1f);
//     {0x1, 0x20}
//
//     >>> roundup_to_pow2<uint7, uint8>(0x20);
//     {0x1, 0x20}
//
//     >>> roundup_to_pow2<uint15, uint16>(0x21);
//     {0x1, 0x40}
template<typename InputType, typename OutputType>
inline optional<OutputType> roundup_to_pow2(InputType input)
{
    assert(bitsizeof(OutputType) > bitsizeof(InputType));

    optional<OutputType> result;
    if (pop_count(input) == 1)
    {
        // Exactly 1 bit is set -> input is already a power of 2.
        result = make_optional<OutputType>(true, input);
    }
    else
    {
        auto highest = highest_one(input);
        if (highest.is_valid)
        {
            uint1[bitsizeof(OutputType)] result_bits = {};

            assert((highest.value + 1) < bitsizeof(OutputType));

            result_bits[highest.value + 1] = 1;

            result = just(cast<OutputType>(result_bits));
        }
        else
        {
            // No bits set, input must have been 0.  Rounding up 0
            // is nonsensical and is treated as an error.
            result = make_optional<OutputType>(false, 0);
        }
    }

    return result;
}

//| Reverse all bits and return the same input data type.
//
// #### __Examples__
//
//     >>> reverse(0xC);
//     0x3
//
//     >>> reverse<optional<uint1>>({false, 0x1});
//     {0x1, 0x0}
//
//     >>> reverse<uint2[4]>({0, 1, 2, 3});
//     {0x3, 0x1, 0x2, 0x0}
template<typename T>
inline T reverse(T data)
{
    uint1[bitsizeof(T)] result;
    uint1[bitsizeof(T)] tmpData = cast<uint1[bitsizeof(T)]>(data);
    static for(const auto i: bitsizeof(T))
    {
        result[i] = tmpData[static(bitsizeof(T) - i - 1)];
    }
    return cast<T>(result);
}

//| Reverse input byte-by-byte and return as the same data type.
//
// #### __Examples__
//
//     >>> reverse_bytes<uint8>(0xC);
//     0xC
//
//     >>> reverse_bytes<uint32>(0xB01DFACE);
//     0xCEFA1DB0
//
//     >>> reverse_bytes<uint8[4]>({0, 1, 2, 3});
//     {0x3, 0x2, 0x1, 0x0}
template<typename T>
inline T reverse_bytes(T data)
{
    const auto bytesize = bytesizeof(T);
    using bytes_t = uint8[bytesize];

    bytes_t data_bytes = cast<bytes_t>(data);
    bytes_t result;
    static for (const auto i : bytesize)
    {
        result[i] = data_bytes[bytesize - 1 - i];
    }
    return cast<T>(result);
}

//| Return array `bool[N]` with element at i the result of i >= arg.
//
// #### __Example__
//
//     >>> mask_greater_equal<4>(2);
//     {0x0, 0x0, 0x1, 0x1}
template <auto N, typename T>
inline bool[N] mask_greater_equal(T arg)
{
    assert(arg <= N);
    return reinterpret_cast<bool[N]>(static((1 << N) - 1) << arg);
}

//| Return array `bool[N]` with element at i the result of i < arg.
//
// #### __Example__
//
//     >>> mask_less_than<4>(2);
//     {0x1, 0x1, 0x0, 0x0}
template <auto N, typename T>
inline bool[N] mask_less_than(T arg)
{
    assert(arg <= N);
    auto ret = mask_greater_equal<N>(arg);
    static for(const auto i : N)
        ret[i] = !ret[i];
    return ret;
}

//| Return array `bool[N]` with element at i the result of i > arg.
//
// #### __Example__
//
//     >>> mask_greater_than<4>(2);
//     {0x0, 0x0, 0x0, 0x1}
template <auto N, typename T>
inline bool[N] mask_greater_than(T arg)
{
    assert(arg <= N);
    return reinterpret_cast<bool[N]>(static((1 << N) - 2) << arg);
}

//| Return array `bool[N]` with element at i the result of i <= arg.
//
// #### __Example__
//
//     >>> mask_less_equal<4>(2);
//     {0x1, 0x1, 0x1, 0x0}
template <auto N, typename T>
inline bool[N] mask_less_equal(T arg)
{
    assert(arg <= N);
    auto ret = mask_greater_than<N>(arg);
    static for(const auto i : N)
        ret[i] = !ret[i];
    return ret;
}

// Equivalent of Verilog style reduction operators

//| Casts the specified scalar type to an array of booleans and applies the reduction operator to it
template<typename T>
inline bool reduction((bool, bool) -> bool f, T x)
{
    return reduce(f, cast<bool[bitsizeof(T)]>(x));
}

//| Verilog style AND reduction operator
template<typename T>
inline bool reduction_and(T x)
{
    return reduction([](bool a, bool b){ return a && b; }, x);
}

//| Verilog style OR reduction operator
template<typename T>
inline bool reduction_or(T x)
{
    return reduction([](bool a, bool b){ return a || b; }, x);
}

//| Verilog style XOR reduction operator
template<typename T>
inline bool reduction_xor(T x)
{
    return reduction([](bool a, bool b){ return a ^^ b; }, x);
}

//| Verilog style XNOR reduction operator
template<typename T>
inline bool reduction_xnor(T x)
{
    return !reduction_xor(x);
}

//| Verilog style NAND reduction operator
template<typename T>
inline bool reduction_nand(T x)
{
    return !reduction_and(x);
}

//| Verilog style NOR reduction operator
template<typename T>
inline bool reduction_nor(T x)
{
    return !reduction_or(x);
}

template<typename T>
inline auto binary_op((uint<bitsizeof T>, uint<bitsizeof T>)->uint<bitsizeof T> f, T x, T y)
{
    return cast<T>(f(cast<uint<bitsizeof T>>(x), cast<uint<bitsizeof T>>(y)));
}

//| AND operator applied to the bits of an arbitrary type.
template<typename T>
inline T bitwise_and(T x, T y)
{
    return binary_op([](auto a, auto b){return a & b; }, x, y);
}

//| OR operator applied to the bits of an arbitrary type.
template<typename T>
inline T bitwise_or(T x, T y)
{
    return binary_op([](auto a, auto b){return a | b; }, x, y);
}

//| XOR operator applied to the bits of an arbitrary type.
template<typename T>
inline T bitwise_xor(T x, T y)
{
    return binary_op([](auto a, auto b){return a ^ b; }, x, y);
}

//| NAND operator applied to the bits of an arbitrary type.
template<typename T>
inline T bitwise_nand(T x, T y)
{
    return binary_op([](auto a, auto b){return ~(a & b); }, x, y);
}

//| NOR operator applied to the bits of an arbitrary type.
template<typename T>
inline T bitwise_nor(T x, T y)
{
    return binary_op([](auto a, auto b){return ~(a | b); }, x, y);
}

//| XNOR operator applied to the bits of an arbitrary type.
template<typename T>
inline T bitwise_xnor(T x, T y)
{
    return binary_op([](auto a, auto b){return ~(a ^ b); }, x, y);
}
