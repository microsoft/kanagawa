// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Common utilities for floating-point types. This module is internal
// and may change at any time. See `numeric.float32` and `numeric.float64`
// for the public interface.
//
// The following table displays the required parameter
// values depending on the floating-point type.
//
// +-------------------+-----------+-----------+
// | Parameter \\ Type | `float32` | `float64` |
// +===================+===========+===========+
// | `MantissaWidth`   | 23        | 52        |
// +-------------------+-----------+-----------+
// | `ExponentWidth`   | 8         | 11        |
// +-------------------+-----------+-----------+
// | `Bias`            | 127       | 1023      |
// +-------------------+-----------+-----------+
module numeric.float.internal
    { float64
    , NativeFloatSupport
    , Denorm
    , floatPackedFormat
    , specialCaseFlags
    , getSpecialCases_internal
    , get_limits
    , int_to_float
    , float_to_int
    , ceil_internal
    , floor_internal
    , round_internal
    , trunc_internal
    }

import data.bits
import data.optional
import data.tuple
import numeric.int.operator.unsigned as unsigned
import type.coerce

using float64 = uint64;

enum NativeFloatSupport : uint1
{
    On,
    Off
}

//| Denormalization flag.
enum Denorm : uint1
{
    On,
    Off
}

template<auto MantissaWidth, auto ExponentWidth>
struct floatPackedFormat
{
    uint<MantissaWidth> mantissa;
    uint<ExponentWidth> exponent;
    uint1 sign;
}

struct specialCaseFlags
{
    bool nan;
    bool zero;
    bool inf;
    bool finite;
}

//| Check if nan, zero, inf, or finite.
template <auto MantissaWidth, auto ExponentWidth, Denorm denorm_mode>
inline specialCaseFlags getSpecialCases_internal(floatPackedFormat<MantissaWidth, ExponentWidth> a)
{
    specialCaseFlags flags;
    auto nonfinite_value = static(cast<uint<ExponentWidth>>((1 << ExponentWidth) - 1));
    flags.nan = a.exponent == nonfinite_value && a.mantissa != 0;
    flags.inf = a.exponent == nonfinite_value && a.mantissa == 0;
    flags.finite = a.exponent != nonfinite_value;
    if (denorm_mode == Denorm::On)
    {
        flags.zero = a.exponent == 0 && a.mantissa == 0;
    }
    else
    {
        flags.zero = a.exponent == 0;
    }
    return flags;
}

//| Convert an integer value to a floating-point value. This generic function
// handles conversion both to `float32` and to `float64`.
//
// This function is not meant to be called by the general public. It is a worker
// function called by the `from_int` functions in the `float32` and `float64` modules.
// Those functions are responsible for passing the proper template parameters.
template
    < typename From      //< Type of the integer to convert.
    , typename To        //< Type to be converted to. This must be either `float32` or `float64`.
    , auto MantissaWidth
    , auto ExponentWidth
    , auto Bias
    >
inline To int_to_float(From input)
{
    const auto result_width = bitsizeof(To);
    const auto infinity_exponent = (1 << ExponentWidth) - 1;

    using result_width_t = uint<result_width>;
    using integer_width_t = uint<bitsizeof(From)>;

    uint1 sign = 0;
    uint<MantissaWidth> mantissa;
    uint<ExponentWidth> exponent;

    integer_width_t integer_value = static_cast(input);

    result_width_t result = 0;

    if (input < 0)
    {
        integer_value = -input;
        sign = 1;
    }

    auto ndx_high_bit_opt = highest_one<integer_width_t>(integer_value);
    if (!ndx_high_bit_opt.is_valid)
    {
        result = 0;
    }
    else
    {
        if (ndx_high_bit_opt.value > MantissaWidth)
        {
            using extended_mantissa_width_t = uint<MantissaWidth + 2>;
            // Max shift value is width of integer
            using shift_width_t = bitindex_t<integer_width_t>;

            shift_width_t shift = unsigned::sub(ndx_high_bit_opt.value, MantissaWidth);

            extended_mantissa_width_t computed_mantissa = static_cast(integer_value >> shift);
            auto discarded_bits_mask = (1 << shift) - 1;
            auto exactly_one_half = 1 << (shift - 1);
            auto bits_to_remove = integer_value & discarded_bits_mask;
            extended_mantissa_width_t mantissa_overflowed = 1 << (MantissaWidth + 1);

            if (bits_to_remove > exactly_one_half)
            {
                ++computed_mantissa;
            }
            else if (bits_to_remove == exactly_one_half)
            {
                // round to nearest even
                if ((computed_mantissa & 1) == 1)
                {
                    ++computed_mantissa;
                }
            }

            exponent = Bias + MantissaWidth + shift;
            if (computed_mantissa >= mantissa_overflowed)
            {
                ++exponent;
            }

            // check for infinity case; adjust mantissa accordingly
            if (exponent == infinity_exponent)
            {
                computed_mantissa = 0;
            }

            mantissa = cast<uint<MantissaWidth>>(computed_mantissa);
        }
        else
        {
            // Max shift value is MantissaWidth
            using shift_width_t = uint<bitsizeof(MantissaWidth)>;
            shift_width_t shift = unsigned::sub(MantissaWidth, ndx_high_bit_opt.value);
            mantissa = cast<uint<MantissaWidth>>(integer_value << shift);
            exponent = Bias + MantissaWidth - shift;
        }

        result = cast<result_width_t>(sign) << (MantissaWidth + ExponentWidth);
        result |= cast<result_width_t>(exponent) << MantissaWidth;
        result |= mantissa;
    }

    // This structure acts as a work-around for the fact that we can't cast a uint
    // directly into a float. We can, however cast a struct. This struct is used to
    // build up the int_to_float function's return value.
    struct ReturnStruct
    {
        result_width_t ret;
    }

    ReturnStruct rs = {result};

    return cast<To>(rs);
}

//| Convert a floating-point value to an integer value. This generic function
// handles conversion both from `float32` and from `float64`.
//
// This function is not meant to be called by the general public. It is a worker
// function called by the `to_int` functions in the `float32` and `float64` modules.
// Those functions are responsible for passing the proper template parameters.
template
    < typename From      //< Type of the float to convert. This must be either `float32` or `float64`.
    , typename To        //< Integer type to be converted to. This may be signed or unsigned.
    , auto MantissaWidth
    , auto ExponentWidth
    , auto Bias
    >
inline optional<To> float_to_int(From input)
{
    const auto int_width = bitsizeof(To);
    const auto float_width = bitsizeof(From);

    using float_as_uint_t = uint<float_width>;
    using exponent_t = uint<ExponentWidth>;
    using mantissa_t = uint<MantissaWidth>;

    struct FromStruct
    {
        From from;
    }

    FromStruct f = {input};

    float_as_uint_t float_as_uint = cast<float_as_uint_t>(f);

    bool to_is_signed = (~cast<To>(0)) < 0;
    uint1 sign = float_as_uint >> float_width - 1;
    exponent_t exponent = (float_as_uint >> MantissaWidth) & ~cast<exponent_t>(0);
    mantissa_t mantissa = float_as_uint & ~cast<mantissa_t>(0);

    To result;
    bool valid = true;
    if (to_is_signed == false && sign == 1)
    {
        valid = false;
    }
    else if (exponent == ~cast<exponent_t>(0))
    {
        // input is NaN or infinity
        valid = false;
        result = 0;
    }
    else if (exponent == 0)
    {
        // Here either the mantissa is zero meaning the float value is zero,
        // or the mantissa is non-zero meaning the float is denormalized
        // in which case truncating the fraction results in zero.
        result = 0;
    }
    else
    {
        int<bitsizeof(exponent_t)> unbiased_exponent = static_cast(exponent - Bias);
        uint<MantissaWidth + 1> full_mantissa = mantissa;
        full_mantissa |= 1 << MantissaWidth;

        if (unbiased_exponent >= int_width)
        {
            valid = false;
        }
        else
        {
            if (!(unbiased_exponent < (int_width - 1)))
            {
                if (to_is_signed)
                {
                    if (unbiased_exponent == (int_width - 1))
                    {
                        if (!(mantissa == 0 && sign == 1))
                            valid = false;
                    }
                }
            }
        }

        if (unbiased_exponent > MantissaWidth)
        {
            auto shift = unbiased_exponent - MantissaWidth;
            result = full_mantissa << shift;

            if (result < 0)
            {
                result = ~result;
            }
        }
        else
        {
            auto shift = MantissaWidth - unbiased_exponent;
            result = cast<decltype(result)>(full_mantissa >> shift);
        }
        if (sign == 1)
            result = -result;
    }

    return make_optional<To>(valid, result);
}

template <typename T>
inline pair<T, T> get_limits()
{
    T min;
    T max;
    if (~cast<T>(0) < 0)
    {
        min = 1 << (bitsizeof(T) - 1);
        max = ~min;
    }
    else
    {
        min = 0;
        max = ~cast<T>(0);
    }

    pair<T, T> result = {min, max};

    return result;
}

//| Return the smallest integer not less than x.
template <typename Type, auto MantissaWidth, auto ExponentWidth, auto Bias, Denorm DenormMode>
inline Type ceil_internal(Type x)
{
    auto binary = cast<floatPackedFormat<MantissaWidth, ExponentWidth>>(x);
    auto flags = getSpecialCases_internal<MantissaWidth, ExponentWidth, DenormMode>(binary);

    const auto exponent = binary.exponent - Bias;
    const auto fractionBit = MantissaWidth - exponent;

    if (flags.finite && !flags.zero && fractionBit >= 0)
    {
        const auto mantissa = binary.mantissa | 1 << MantissaWidth;
        const auto fraction = mantissa & ((1 << fractionBit) - 1);
        const auto integral = (mantissa >> fractionBit) + ((binary.sign == 0 && fraction != 0) ? 1 : 0);

        if (integral == 0)
        {
            binary.exponent = 0;
            binary.mantissa = 0;
        }
        else if (integral == 1)
        {
            binary.exponent = Bias;
            binary.mantissa = 0;
        }
        else
        {
            binary.exponent = binary.exponent + (((integral << fractionBit) >> (MantissaWidth + 1)) & 1);
            binary.mantissa = cast<decltype(binary.mantissa)>(integral << fractionBit);
        }
    }

    return cast<Type>(binary);
}

//| Return the largest integer not greater than x.
template <typename Type, auto MantissaWidth, auto ExponentWidth, auto Bias, Denorm DenormMode>
inline Type floor_internal(Type x)
{
    auto binary = cast<floatPackedFormat<MantissaWidth, ExponentWidth>>(x);
    auto flags = getSpecialCases_internal<MantissaWidth, ExponentWidth, DenormMode>(binary);

    const auto exponent = binary.exponent - Bias;
    const auto fractionBit = MantissaWidth - exponent;

    if (flags.finite && !flags.zero && fractionBit >= 0)
    {
        const auto mantissa = binary.mantissa | 1 << MantissaWidth;
        const auto fraction = mantissa & ((1 << fractionBit) - 1);
        const auto integral = (mantissa >> fractionBit) + ((binary.sign != 0 && fraction != 0) ? 1 : 0);

        if (integral == 0)
        {
            binary.exponent = 0;
            binary.mantissa = 0;
        }
        else if (integral == 1)
        {
            binary.exponent = Bias;
            binary.mantissa = 0;
        }
        else
        {
            binary.exponent = binary.exponent + (((integral << fractionBit) >> (MantissaWidth + 1)) & 1);
            binary.mantissa = cast<decltype(binary.mantissa)>(integral << fractionBit);
        }
    }

    return cast<Type>(binary);
}

//| Return the nearest integer to x, rounding halfway away from zero.
template <typename Type, auto MantissaWidth, auto ExponentWidth, auto Bias, Denorm DenormMode>
inline Type round_internal(Type x)
{
    auto binary = cast<floatPackedFormat<MantissaWidth, ExponentWidth>>(x);
    auto flags = getSpecialCases_internal<MantissaWidth, ExponentWidth, DenormMode>(binary);

    const auto exponent = binary.exponent - Bias;
    const auto fractionBit = MantissaWidth - exponent;

    if (flags.finite && fractionBit >= 0)
    {
        const auto mantissa = binary.mantissa | 1 << MantissaWidth;
        const auto fraction = mantissa & ((1 << fractionBit) - 1);
        const auto integral = (mantissa >> fractionBit);

        const auto roundedIntegral = integral + ((mantissa >> (fractionBit - 1)) & 1);

        if (roundedIntegral == 0)
        {
            binary.exponent = 0;
            binary.mantissa = 0;
        }
        else if (roundedIntegral == 1)
        {
            binary.exponent = Bias;
            binary.mantissa = 0;
        }
        else
        {
            binary.exponent = binary.exponent + (((roundedIntegral << fractionBit) >> (MantissaWidth + 1)) & 1);
            binary.mantissa = cast<decltype(binary.mantissa)>(roundedIntegral << fractionBit);
        }
    }

    return cast<Type>(binary);
}

//| Return the nearest integer not greater in magnitude than x.
template <typename Type, auto MantissaWidth, auto ExponentWidth, auto Bias, Denorm DenormMode>
inline Type trunc_internal(Type x)
{
    auto binary = cast<floatPackedFormat<MantissaWidth, ExponentWidth>>(x);
    const auto sign = binary.sign;

    binary.sign = 0;
    x = floor_internal<Type, MantissaWidth, ExponentWidth, Bias, DenormMode>(cast<Type>(binary));

    binary = cast<floatPackedFormat<MantissaWidth, ExponentWidth>>(x);
    binary.sign = sign;

    return cast<Type>(binary);
}