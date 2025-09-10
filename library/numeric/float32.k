// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// [Single-precision floating-point format](https://en.wikipedia.org/wiki/Single-precision_floating-point_format)
module numeric.float32
    { NativeFloatSupport
    , Denorm
    , isnan
    , isinf
    , isfinite
    , isnormal
    , signbit
    , add
    , mul
    , eq
    , lt
    , gt
    , neg
    , sub
    , rcp
    , exp2
    , exp
    , expm1
    , log2
    , log10
    , log
    , log1p
    , logb
    , ilogb
    , pow
    , atan
    , min
    , max
    , ceil
    , floor
    , round
    , trunc
    , dim
    , abs
    , sin
    , cos
    , from_int
    , from_uint
    , to_int
    , to_uint
    }

import data.bits
import data.optional
import data.tuple
import numeric.fixed.internal as fixed_internal
import numeric.float32.internal
import numeric.float32.internal.operator
import numeric.float.internal

enum float32_constants: uint32
{
    POS_MAX       = 0x7F7FFFFF,
    NEG_MAX       = 0xFF7FFFFF,
    POS_ZERO      = 0x00000000,
    NEG_ZERO      = 0x80000000,
    POS_ONE       = 0x3F800000,
    NEG_ONE       = 0xBF800000,
    POS_INFINITY  = 0x7F800000,
    NEG_INFINITY  = 0xFF800000,
    DEFAULT_NAN   = 0x7FC00000
}

template<auto I, auto F>
struct float32ExtendedFormat
{
    uint<I + F> mantissa;
    int9 exponent;
    uint1 sign;
}

struct stickyMantissaFormat
{
    uint1 sticky;
    uint26 mantissaGuardRound;
}

inline int8 decodeExponent(uint8 encoded)
{
    return cast<int8>(encoded - 127);
}

inline float32ExpandedFormat normalizeInput(float32ExpandedFormat in)
{
    float32ExpandedFormat out;

    uint5 shiftAmount = priorityOneEncoderCountDown24(in.mantissa);
    out.mantissa = in.mantissa << shiftAmount;
    out.exponent = in.exponent - cast <int10>(shiftAmount);
    out.sign = in.sign;
    return(out);
}

inline uint5 priorityOneEncoderCountDown24(uint24 in)
{
    auto highest = highest_one<uint24>(in);
    return highest.is_valid ? 23 - highest.value : 24;
}

// Shift right to denorm small exponent values. Exponent must be less
// than or equal to -126.
inline float32StickyFormat normalizeOutput2(float32StickyFormat input)
{
    float32StickyFormat out;

    uint8 shiftRightAmount = static_cast(-126 - input.exponent);
    uint27 mantissaGuardRoundSticky = concat(input.mantissaGuardRound, input.sticky);
    uint27 outMantissaGuardRoundSticky;
    outMantissaGuardRoundSticky = shiftRightSticky(mantissaGuardRoundSticky, shiftRightAmount);

    out.sticky = outMantissaGuardRoundSticky & 1;
    out.mantissaGuardRound = outMantissaGuardRoundSticky >> 1;
    out.exponent = input.exponent + shiftRightAmount;
    out.sign = input.sign;

    return(out);
}

//| Check `x` is not a number.
inline bool isnan(float32 x)
{
    const auto flags = getSpecialCases<Denorm::On>(cast<float32PackedFormat>(x));
    return flags.nan;
}

//| Check `x` is positive or negative infinity.
inline bool isinf(float32 x)
{
    const auto flags = getSpecialCases<Denorm::On>(cast<float32PackedFormat>(x));
    return flags.inf;
}

inline bool isfinite(float32 x)
{
    const auto flags = getSpecialCases<Denorm::On>(cast<float32PackedFormat>(x));
    return flags.finite;
}

inline bool isnormal(float32 x)
{
    const auto s = cast<float32PackedFormat>(x);
    return s.exponent != 0xFF && s.exponent != 0;
}

//| Return sign of `x`.
inline bool signbit(float32 x)
{
    const auto s = cast<float32PackedFormat>(x);
    return s.sign == 1;
}

// Function: float32_add
//
// Add two float32 values and return the float32 result
inline float32 float32_add(float32 in1, float32 in2)
{
    // Unpack input
    float32PackedFormat a;
    float32PackedFormat b;
    a = cast<float32PackedFormat> (in1);
    b = cast<float32PackedFormat> (in2);
    float32ExpandedFormat a_PostDecode = unpackFloat32<Denorm::On>(a);
    float32ExpandedFormat b_PostDecode = unpackFloat32<Denorm::On>(b);

    float32StickyFormat a_PostAlign;
    float32StickyFormat b_PostAlign;

    uint8 alignShiftAmountA;
    uint8 alignShiftAmountB;
    if (a_PostDecode.exponent > b_PostDecode.exponent)
    {
        alignShiftAmountA = 0;
        alignShiftAmountB = a_PostDecode.exponent - b_PostDecode.exponent;
    }
    else
    {
        alignShiftAmountA = b_PostDecode.exponent - a_PostDecode.exponent;
        alignShiftAmountB = 0;
    }

    a_PostAlign = align(a_PostDecode, alignShiftAmountA);
    b_PostAlign = align(b_PostDecode, alignShiftAmountB);

    //Addition
    float32StickyFormat z_postAdd;
    uint27 aTemp = a_PostAlign.mantissaGuardRound << 1 | a_PostAlign.sticky;
    uint27 bTemp = b_PostAlign.mantissaGuardRound << 1 | b_PostAlign.sticky;

    uint28 sumTemp = static_cast(a_PostAlign.sign == b_PostAlign.sign ? aTemp + bTemp :
                        (aTemp >= bTemp ? aTemp - bTemp : bTemp - aTemp));
    z_postAdd.sign =  a_PostAlign.sign == b_PostAlign.sign ? a_PostAlign.sign :
                        (aTemp >= bTemp ? a_PostAlign.sign : b_PostAlign.sign);

    bool shiftSum = sumTemp >> 27 == 1;
    z_postAdd.exponent = a_PostAlign.exponent + cast<uint1>(shiftSum);
    z_postAdd.mantissaGuardRound = shiftSum ? cast <uint26> (sumTemp >> 2) : cast <uint26> (sumTemp >> 1);
    z_postAdd.sticky = shiftSum ? cast <uint1> (sumTemp >> 1) | cast <uint1> (sumTemp) : cast <uint1> (sumTemp);

    //Shift left to normalize small mantissa values
    float32StickyFormat z_postNormalize = addNormalizeOutput1(z_postAdd);

    //Rounding
    float32ExpandedFormat z_postRounding;

    bool guard = ((z_postNormalize.mantissaGuardRound >> 1) & 1) == 1;
    bool round = (z_postNormalize.mantissaGuardRound & 1) == 1;
    bool m0 = ((z_postNormalize.mantissaGuardRound >> 2) & 1) == 1;
    bool sticky = z_postNormalize.sticky == 1;

    bool roundUp = guard && (round || sticky || m0);
    z_postRounding.mantissa = cast<decltype(z_postRounding.mantissa)>((z_postNormalize.mantissaGuardRound >> 2) + cast<uint1>(roundUp));
    uint1 exp_inc = (roundUp && z_postNormalize.mantissaGuardRound >> 2 == 0xffffff) ? 1 : 0;
    z_postRounding.exponent = z_postNormalize.exponent + exp_inc;
    z_postRounding.sign = z_postNormalize.sign;

    //Packing
    float32PackedFormat zOutput;

    //Handle special cases
    specialCaseFlags a_flags = getSpecialCases<Denorm::On>(a);
    specialCaseFlags b_flags = getSpecialCases<Denorm::On>(b);
    bool justAInf = a_flags.inf && !b_flags.inf;
    bool justBInf = !a_flags.inf && b_flags.inf;
    bool AandBInf = a_flags.inf && b_flags.inf;
    bool specialCaseSignsEqual = a_PostDecode.sign == b_PostDecode.sign;

    bool specialCaseNaN = a_flags.nan || b_flags.nan || (AandBInf && !specialCaseSignsEqual);
    bool specialCaseZero = a_flags.zero && b_flags.zero;
    bool specialCaseInf = justAInf || justBInf || (AandBInf && specialCaseSignsEqual);
    uint1 specialCaseSign = justAInf ? a_PostDecode.sign :
                            justBInf ? b_PostDecode.sign :
                            a_PostDecode.sign & b_PostDecode.sign;

    bool positiveZero = z_postRounding.exponent == -126 && z_postRounding.mantissa == 0;
    bool zeroExponent = z_postRounding.exponent == -126 && ((z_postRounding.mantissa >> 23) & 1) == 0;
    bool overflow = z_postRounding.exponent > 127;

    zOutput.mantissa = specialCaseNaN ? 1 << 22 :
                       (specialCaseInf || overflow) ? 0 :
                       cast<decltype(zOutput.mantissa)>(z_postRounding.mantissa);
    zOutput.exponent = (specialCaseNaN || specialCaseInf || overflow) ? 255 :
                       zeroExponent ? 0 :
                       z_postRounding.exponent + 127;
    zOutput.sign = (specialCaseInf || specialCaseZero) ? specialCaseSign :
                   positiveZero ? 0 :
                   z_postRounding.sign;

    return cast<float32> (zOutput);
}

// Function: float32_mul
//
// Multiply two float32 values and return the float32 result
inline float32 float32_mul(float32 in1, float32 in2)
{
    // Unpack input
    float32PackedFormat a = cast<float32PackedFormat> (in1);
    float32PackedFormat b = cast<float32PackedFormat> (in2);
    float32ExpandedFormat a_PostDecode = unpackFloat32<Denorm::On>(a);
    float32ExpandedFormat b_PostDecode = unpackFloat32<Denorm::On>(b);
    float32ExpandedFormat a_PostDenorm = normalizeInput(a_PostDecode);
    float32ExpandedFormat b_PostDenorm = normalizeInput(b_PostDecode);

    //Multiplication
    float32StickyFormat z_postMultiply;
    z_postMultiply.sign = a_PostDenorm.sign ^ b_PostDenorm.sign;
    z_postMultiply.exponent = a_PostDenorm.exponent + b_PostDenorm.exponent + 1;

    uint50 productTemp = (a_PostDenorm.mantissa * b_PostDenorm.mantissa) << 2;
    productFormat product;
    product = cast<productFormat> (productTemp);

    z_postMultiply.mantissaGuardRound = (product.product << 2) | (product.guard << 1) | product.round;
    z_postMultiply.sticky = cast<uint1> (product.sticky != 0);

    // Output Normalization
    // If the exponent is already in the denorm range, then we shift
    // right and don't need to worry about leading ones.
    // If the exponent is in the normalized range, then we might have to
    // shift left to adjust for leading one. This can be at most a shift
    // by 1, so it will never lead to an unusable exponent.
    float32StickyFormat z_postNormalize2 = z_postMultiply.exponent <= -126 ? normalizeOutput2(z_postMultiply) : multNormalizeOutput1(z_postMultiply);

    //Rounding
    float32ExpandedFormat z_postRounding;

    bool guard = ((z_postNormalize2.mantissaGuardRound >> 1) & 1) == 1;
    bool round = (z_postNormalize2.mantissaGuardRound & 1) == 1;
    bool m0 = ((z_postNormalize2.mantissaGuardRound >> 2) & 1) == 1;
    bool sticky = z_postNormalize2.sticky == 1;

    bool roundUp = guard && (round || sticky || m0);
    z_postRounding.mantissa = (z_postNormalize2.mantissaGuardRound >> 2) + cast<uint1>(roundUp);
    uint1 exp_inc = roundUp && (z_postNormalize2.mantissaGuardRound >> 2 == 0xffffff) ? 1 : 0;
    z_postRounding.exponent = z_postNormalize2.exponent + exp_inc;
    z_postRounding.sign = z_postNormalize2.sign;

    //Packing
    float32PackedFormat zOutput;

    //Handle special cases
    specialCaseFlags a_flags = getSpecialCases<Denorm::On>(a);
    specialCaseFlags b_flags = getSpecialCases<Denorm::On>(b);

    bool specialCaseNaN = a_flags.nan || b_flags.nan || (a_flags.inf && b_flags.zero) || (a_flags.zero && b_flags.inf);
    bool specialCaseZero = a_flags.zero || b_flags.zero;
    bool specialCaseInf = (a_flags.inf && !b_flags.zero) || (!a_flags.zero && b_flags.inf);

    zOutput.mantissa = specialCaseNaN ? 1 << 22 :
                        (specialCaseZero || specialCaseInf || (z_postRounding.exponent > 127) ? 0 : z_postRounding.mantissa);
    zOutput.exponent = (specialCaseNaN || specialCaseInf || (z_postRounding.exponent > 127)) ? 255 :
                        (specialCaseZero || (z_postRounding.exponent == -126 && ((z_postRounding.mantissa >> 23) & 1) == 0) ? 0 :
                        z_postRounding.exponent + 127);
    zOutput.sign = z_postRounding.sign;

    return cast<float32> (zOutput);
}

//| Addition.
template <Denorm denorm_mode>
inline float32 add(float32 in1, float32 in2)
{
    float32 result;
    if (denorm_mode == Denorm::On)
    {
        result = float32_add(in1, in2);
    }
    else
    {
        result = float32_add_denormoff(in1, in2);
    }
    return result;
}

//| Multiplication.
template <Denorm denorm_mode>
inline float32 mul(float32 in1, float32 in2)
{
    float32 result;
    if (denorm_mode == Denorm::On)
    {
        result = float32_mul(in1, in2);
    }
    else
    {
        result = float32_mul_denormoff(in1, in2);
    }
    return result;
}

//| Compare two float32 values for equality.
template <Denorm denorm_mode>
inline bool eq(float32 in1, float32 in2)
{
    // Unpack input
    float32PackedFormat a;
    float32PackedFormat b;
    a = cast<float32PackedFormat>(in1);
    b = cast<float32PackedFormat>(in2);

    // Get zero, nan, or inf
    specialCaseFlags a_flags = getSpecialCases<denorm_mode>(a);
    specialCaseFlags b_flags = getSpecialCases<denorm_mode>(b);

    bool result;
    // Negative and positive zero are equal
    if (a_flags.zero && b_flags.zero)
    {
        result = true;
    }
    // NaNs are not equal
    else if (a_flags.nan || b_flags.nan)
    {
        result = false;
    }
    else
    {
        result = cast<uint32>(in1) == cast<uint32>(in2);
    }

    return result;
}

//| Check `in1` less than `in2`.
template <Denorm denorm_mode>
inline bool lt(float32 in1, float32 in2)
{
    // Unpack input
    float32PackedFormat a;
    float32PackedFormat b;
    a = cast<float32PackedFormat>(in1);
    b = cast<float32PackedFormat>(in2);

    // Get zero, nan, or inf
    specialCaseFlags a_flags = getSpecialCases<denorm_mode>(a);
    specialCaseFlags b_flags = getSpecialCases<denorm_mode>(b);

    bool result;
    // Check equal
    if (eq<denorm_mode>(in1, in2))
    {
        result = false;
    }
    // NaN
    else if (a_flags.nan || b_flags.nan)
    {
        result = false;
    }
    // a negative, b positive or zero
    else if (a.sign == 1 && (b_flags.zero || b.sign == 0))
    {
        result = true;
    }
    // b negative, a positive or zero
    else if (b.sign == 1 && (a_flags.zero || a.sign == 0))
    {
        result = false;
    }
    // same sign
    else
    {
        result = concat(a.exponent, a.mantissa) < concat(b.exponent, b.mantissa);
        // Reverse if negative
        if (a.sign == 1)
        {
            result = !result;
        }
    }
    return result;
}

//| Check `in1` greater than `in2`.
template <Denorm denorm_mode>
inline bool gt(float32 in1, float32 in2)
{
    // Unpack input
    float32PackedFormat a;
    float32PackedFormat b;
    a = cast<float32PackedFormat>(in1);
    b = cast<float32PackedFormat>(in2);

    // Get zero, nan, or inf
    specialCaseFlags a_flags = getSpecialCases<denorm_mode>(a);
    specialCaseFlags b_flags = getSpecialCases<denorm_mode>(b);

    bool result;
    if (a_flags.nan || b_flags.nan)
    {
        result = false;
    }
    else
    {
        result = !eq<denorm_mode>(in1, in2) && !lt<denorm_mode>(in1, in2);
    }
    return result;
}

//| Subtract `in2` from `in1`.
template <Denorm denorm_mode>
inline float32 sub(float32 in1, float32 in2)
{
    return add<denorm_mode>(in1, neg(in2));
}

//| Reciprocal `1 / x`.
inline float32 rcp(float32 x)
{
    auto binary32 = cast<float32PackedFormat>(x);
    uint32 value = cast<uint32>(x);

    if (binary32.exponent == 0)
    {
        value = (binary32.sign == 0)
            ? float32_constants::POS_INFINITY
            : float32_constants::NEG_INFINITY;
    }
    else if (value == float32_constants::POS_INFINITY)
    {
        value = float32_constants::POS_ZERO;
    }
    else if (value == float32_constants::NEG_INFINITY)
    {
        value = float32_constants::NEG_ZERO;
    }
    else if (binary32.exponent != 0xFF)
    {
        const auto significand = (1 << 23) | binary32.mantissa;

        // approximate
        const auto approxFbitCount = 8;
        const uint<1 + approxFbitCount> approxValue = fixed_internal::rcp_lookup<23, approxFbitCount>(static_cast(significand));

        // 1st refinement
        const auto refinedFbitCount_0 = 16;
        const uint<1 + refinedFbitCount_0> refinedValue_0 =
            fixed_internal::rcp_refine<23, approxFbitCount, refinedFbitCount_0>(significand, approxValue);

        // 2nd refinement
        const auto refinedFbitCount_1 = 25;
        uint<1 + refinedFbitCount_1> refinedValue_1 =
            fixed_internal::rcp_refine<23, refinedFbitCount_0, refinedFbitCount_1>(significand, refinedValue_0);

        // Adjust the significand from ~(0.5, 1.0] to [1.0, 2.0).
        bool is_one = (refinedValue_1 >> refinedFbitCount_1) == 1;
        refinedValue_1 = is_one ? (refinedValue_1 >> 1) : refinedValue_1;
        binary32.exponent = 254 - binary32.exponent;
        binary32.exponent = cast<decltype(binary32.exponent)>(binary32.exponent - (is_one ? 0 : 1));

        // round to 23 fraction bits
        refinedValue_1 = (refinedValue_1 + (refinedValue_1 & 1)) >> 1;

        if (binary32.exponent == 0 || binary32.exponent == 0xFF)
        {
            binary32.exponent = 0;
            binary32.mantissa = 0;
        }
        else
        {
            binary32.mantissa = cast<decltype(binary32.mantissa)>(refinedValue_1);
        }

        value = cast<uint32>(binary32);
    }

    return cast<float32>(value);
}

template<typename T, auto I, auto F1, auto F2>
inline T exp2_internal(float32ExtendedFormat<I, F1> x)
{
    const auto fixedValue = x.mantissa >> (F1 - x.exponent - (F2 + 1));
    const auto roundedValue = (fixedValue >> 1) + (fixedValue & 1);

    // separate integral and fraction
    T result;
    result.exponent = cast<decltype(result.exponent)>(roundedValue >> F2);
    uint<F2> fraction = static_cast(roundedValue);

    // adjust exponent and fraction for negative x
    const bool is_zero = fraction == 0;
    result.exponent = x.sign ? cast<decltype(result.exponent)>(-result.exponent - (is_zero ? 0 : 1)) : result.exponent;
    fraction = cast<decltype(fraction)>((1 << F2) + (x.sign ? -fraction : fraction));

    const auto fraction24 = exp2_lookup<F2, 24>(fraction);
    const auto fraction23 = (fraction24 >> 1) + (fraction24 & 1);
    result.mantissa = fraction23;
    result.exponent = cast<decltype(result.exponent)>(result.exponent + ((fraction23 >> 23) + 127));
    result.sign = 0;

    return result;
}

//| $2^x$
inline float32 exp2(float32 x)
{
    const auto structValue = cast<float32PackedFormat>(x);
    const uint32 binaryValue = cast<uint32>(x);

    const uint32 binaryPositive128 = 0x43000000;
    const uint32 binaryNegative126 = 0xC2FC0000;

    const specialCaseFlags flags = getSpecialCases<Denorm::Off>(structValue);

    const bool isNegative = signbit(x);
    const bool isNegInf = flags.inf && isNegative;
    const bool isPosInf = flags.inf && !isNegative;
    const bool isOverflow = structValue.sign == 0 && structValue.exponent != 0xFF && binaryValue >= binaryPositive128;
    const bool isUnderflow = structValue.sign == 1 && structValue.exponent != 0xFF && binaryValue > binaryNegative126;

    const int8 exponent = decodeExponent(structValue.exponent);
    const auto valueFbit = 23 + 7;
    const auto valueMantissa = ((1 << 23) | structValue.mantissa) << 7;

    const auto structResult =
        exp2_internal<float32PackedFormat, 1, valueFbit, 23>( {valueMantissa, exponent, structValue.sign} );

    // output
    float32PackedFormat result;
    result.sign = 0;
    result.exponent = (isNegInf || isUnderflow) ? 0 :
                        (isPosInf || isOverflow || flags.nan) ? 0xFF :
                        flags.zero ? 127 : structResult.exponent;
    result.mantissa = (isNegInf || isUnderflow || isPosInf || isOverflow || flags.zero) ? 0 :
                        flags.nan ? 1 : structResult.mantissa;

    return cast<float32>(result);
}

// Input: [1, 2)
// Output: (1, 3)
template<auto F1, auto F2>
inline uint<2 + F2> exp2_translate(uint<1 + F1> fraction)
{
    const auto log2eFbit = 52;
    const auto log2e = 0x171547652B82FE;

    const auto valueFbit = log2eFbit + F1;
    auto value = log2e * fraction;

    static assert(valueFbit >= F2);
    value = value >> (valueFbit - F2);
    return value;
}

//| $e^x$
inline float32 exp(float32 x)
{
    const auto structValue = cast<float32PackedFormat>(x);
    const uint32 binaryValue = cast<uint32>(x);

    const uint32 MAX_EXP_INPUT = 0x42B17218;    // max input 88.7228393555
    const uint32 MIN_EXP_INPUT = 0xC2AEAC50;    // min input -87.3365478516

    const specialCaseFlags flags = getSpecialCases<Denorm::Off>(structValue);

    const bool isNegative = signbit(x);
    const bool isNegInf = flags.inf && isNegative;
    const bool isPosInf = flags.inf && !isNegative;
    const bool isOverflow = structValue.sign == 0 && structValue.exponent != 0xFF && binaryValue >= MAX_EXP_INPUT;
    const bool isUnderflow = structValue.sign == 1 && structValue.exponent != 0xFF && binaryValue >= MIN_EXP_INPUT;

    // translate exp to exp2
    const int8 exponent = decodeExponent(structValue.exponent);
    const auto significand = (1 << 23) | structValue.mantissa;
    const auto translatedMantissaFbit = 75;
    const auto translatedMantissa = exp2_translate<23, translatedMantissaFbit>(significand);

    const auto structResult =
        exp2_internal<float32PackedFormat, 2, translatedMantissaFbit, 23>( {translatedMantissa, exponent, structValue.sign} );

    // output
    float32PackedFormat result;
    result.sign = 0;
    result.exponent = (isNegInf || isUnderflow) ? 0 :
                        (isPosInf || isOverflow || flags.nan) ? 0xFF :
                        flags.zero ? 127 : structResult.exponent;
    result.mantissa = (isNegInf || isUnderflow || isPosInf || isOverflow || flags.zero) ? 0 :
                        flags.nan ? 1 : structResult.mantissa;

    return cast<float32>(result);
}

const auto EXP2M1_TABLE_LG_SIZE = 9;
const auto EXP2M1_C2_FBIT = 14;
const auto EXP2M1_C1_FBIT = 21;
const auto EXP2M1_C0_FBIT = 44;

struct exp2m1_coefficients
{
    int15 c2;
    int23 c1;
    int45 c0;
}

struct exp2m1_row
{
    exp2m1_coefficients coeff_pos;
    exp2m1_coefficients coeff_neg;
}

template<auto F1, auto F2>
struct exp2m1_paramPacked
{
    uint<F1 - (F2 + 5)> residual;
    uint5 index;
    uint<F2> msbits;
}

template<auto F1, auto F2>
struct exp2m1_residual_and_index
{
    uint<F1 - (F2 + 5)> residual;
    uint5 index;
}

struct exp2m1_seperated4
{
    uint4 bit_3_0;
    uint4 bit_7_4;
    uint4 bit_11_8;
    uint4 bit_15_12;
}

template<auto F1, auto F2>
inline int<1 + F2> exp2m1_lookup(bool sign, uint<F1> fraction)
{
    const auto packedFraction_bit15 = cast<exp2m1_paramPacked<F1, 15>>(fraction);
    const auto packedFraction_bit14 = cast<exp2m1_paramPacked<F1, 14>>(fraction);
    const auto packedFraction_bit13 = cast<exp2m1_paramPacked<F1, 13>>(fraction);
    const auto packedFraction_bit12 = cast<exp2m1_paramPacked<F1, 12>>(fraction);
    const auto packedFraction_bit11 = cast<exp2m1_paramPacked<F1, 11>>(fraction);
    const auto packedFraction_bit10 = cast<exp2m1_paramPacked<F1, 10>>(fraction);
    const auto packedFraction_bit9 = cast<exp2m1_paramPacked<F1, 9>>(fraction);
    const auto packedFraction_bit8 = cast<exp2m1_paramPacked<F1, 8>>(fraction);
    const auto packedFraction_bit7 = cast<exp2m1_paramPacked<F1, 7>>(fraction);
    const auto packedFraction_bit6 = cast<exp2m1_paramPacked<F1, 6>>(fraction);
    const auto packedFraction_bit5 = cast<exp2m1_paramPacked<F1, 5>>(fraction);
    const auto packedFraction_bit4 = cast<exp2m1_paramPacked<F1, 4>>(fraction);
    const auto packedFraction_bit3 = cast<exp2m1_paramPacked<F1, 3>>(fraction);
    const auto packedFraction_bit2 = cast<exp2m1_paramPacked<F1, 2>>(fraction);
    const auto packedFraction_bit1 = cast<exp2m1_paramPacked<F1, 1>>(fraction);

    // extract the power order of 2 of first 16-bit of fraction
    const auto temp16 = cast<exp2m1_seperated4>(cast<uint16>(fraction >> (F1 - 16)));
    const uint2 powerOrderOf16 = (temp16.bit_15_12 != 0) ? 3 :
                                 (temp16.bit_11_8 != 0) ? 2 :
                                 (temp16.bit_7_4 != 0) ? 1 : 0;

    const auto temp4 = mux(powerOrderOf16, temp16.bit_3_0, temp16.bit_7_4, temp16.bit_11_8, temp16.bit_15_12);

    const uint2 powerOrderOf2 = ((temp4 >> 3) == 1) ? 3 :
                                ((temp4 >> 2) == 1) ? 2 :
                                ((temp4 >> 1) == 1) ? 1 : 0;

    const uint4 totalPowerOrderOf2 = (powerOrderOf16 << 2) | powerOrderOf2;

    // extract index and residual
    const exp2m1_residual_and_index<F1, 1> ir = mux(totalPowerOrderOf2,
        { packedFraction_bit15.residual, packedFraction_bit15.index },
        { packedFraction_bit15.residual, packedFraction_bit15.index },
        { packedFraction_bit14.residual, packedFraction_bit14.index },
        { packedFraction_bit13.residual, packedFraction_bit13.index },
        { packedFraction_bit12.residual, packedFraction_bit12.index },
        { packedFraction_bit11.residual, packedFraction_bit11.index },
        { packedFraction_bit10.residual, packedFraction_bit10.index },
        { packedFraction_bit9.residual, packedFraction_bit9.index },
        { packedFraction_bit8.residual, packedFraction_bit8.index },
        { packedFraction_bit7.residual, packedFraction_bit7.index },
        { packedFraction_bit6.residual, packedFraction_bit6.index },
        { packedFraction_bit5.residual, packedFraction_bit5.index },
        { packedFraction_bit4.residual, packedFraction_bit4.index },
        { packedFraction_bit3.residual, packedFraction_bit3.index },
        { packedFraction_bit2.residual, packedFraction_bit2.index },
        { packedFraction_bit1.residual, packedFraction_bit1.index });

    const uint9 index = (totalPowerOrderOf2 << 5) | ir.index;
    const auto residual = ir.residual;

    // extract coefficients
    const auto row = exp2m1_coefficient_table[index];
    const auto c2 = sign ? row.coeff_neg.c2 : row.coeff_pos.c2;
    const auto c1 = sign ? row.coeff_neg.c1 : row.coeff_pos.c1;
    const auto c0 = sign ? row.coeff_neg.c0 : row.coeff_pos.c0;

    // compute exp2
    const auto value2Fbit = F1 + EXP2M1_C2_FBIT;
    const auto value2 = residual * c2;

    const auto value1Fbit = F1 + value2Fbit;
    static assert(value2Fbit >= EXP2M1_C1_FBIT);
    const auto value1 = (value2 + (c1 << (value2Fbit - EXP2M1_C1_FBIT))) * residual;

    static assert(F2 >= EXP2M1_C0_FBIT);
    static assert(value1Fbit >= (F2 + 1));
    const auto value0 = (value1 + (c0 << (value1Fbit - EXP2M1_C0_FBIT))) >> (value1Fbit - (F2 + 1));

    return (value0 >> 1) + (value0 & 1);
}

//| Calculate $e^x - 1$. Denorm inputs and outputs are supported.
// For smaller values of `x`, `expm1(x)` may be more accurate than `exp(x)-1`.
inline float32 expm1(float32 x)
{
    const auto structX = cast<float32PackedFormat>(x);
    const auto binaryX = cast<uint32>(x);
    const auto flags = getSpecialCases<Denorm::On>(structX);

    float32PackedFormat result;
    const int8 exponent = decodeExponent(structX.exponent);
    const bool isSmallValue = (exponent < -23) && !flags.inf;

    if(isSmallValue)
    {
        result = structX;
    }
    else
    {
        const bool isNegativeX = structX.sign == 1;
        const bool isOutputInf = !isNegativeX && structX.exponent != 0xFF && binaryX >= 0x42B17218;
        const bool isOutputMinus1 = isNegativeX && structX.exponent != 0xFF && binaryX >= 0xC18AA123;
        const bool isPosInfinity = flags.inf && !isNegativeX;
        const bool isNegInfinity = flags.inf && isNegativeX;

        // change base to exp2
        const auto Fbit = 47;
        const auto exp2ValueSignificand = exp2_translate<23, Fbit>((1 << 23) | structX.mantissa);

        // The exponent of max input is 6 of max input(0x42B17218).
        const uint55 exp2Value = (exp2ValueSignificand << 6) >> (6 - exponent);
        const auto temp = cast<tuple2<uint<Fbit>, uint<8>>>(exp2Value);
        const auto integer = temp.second;
        const auto fraction = temp.first;

        // Compute exp2m1
        const auto exp2m1Fbit = 47;
        const auto exp2Fraction = exp2m1_lookup<Fbit, exp2m1Fbit>(isNegativeX, fraction);

        int<1 + exp2m1Fbit> exp2m1;
        if(integer == 0)
        {
            exp2m1 = exp2Fraction;
        }
        else
        {
            if(isNegativeX)
            {
                // Add 1 to exp2Fraction and divide by 2^integer, then subtract 1.
                // if exp2Fraction is negtive, int<1 + F>(exp2Fraction) + (1 << F) = uint<F>(exp2Fraction).
                const auto plus1 = cast<uint<exp2m1Fbit>>(exp2Fraction);
                exp2m1 = (plus1 >> integer) - (1 << exp2m1Fbit);
            }
            else
            {
                // Multiply exp2Fraction by 2^integer and subtract 1.
                exp2m1 = cast<decltype(exp2m1)>(exp2Fraction - (1 << (exp2m1Fbit - integer)));
            }
        }

        // convert to float
        const bool isNegativeExp2m1 = exp2m1 < 0;
        const auto absValue = isNegativeExp2m1 ? -exp2m1 : exp2m1;
        const auto leadingOneIndex = highest_one<uint24>(static_cast(absValue >> (exp2m1Fbit - 24)));

        int8 exponentResult;
        uint<exp2m1Fbit> mantissaResult;
        if(integer > 0 && !isNegativeX)
        {
            exponentResult = cast<decltype(exponentResult)>(isNegativeExp2m1 ? (integer - 1) : integer);
            mantissaResult = cast<decltype(mantissaResult)>(isNegativeExp2m1 ? (exp2m1 << 1) : exp2m1);
        }
        else
        {
            exponentResult = leadingOneIndex.value - 24;
            mantissaResult = cast<decltype(mantissaResult)>(absValue << (-exponentResult));
        }

        const auto truncatedMantissa = mantissaResult >> (exp2m1Fbit - 24);
        const auto roundedMantissa = (truncatedMantissa >> 1) + (truncatedMantissa & 1);
        exponentResult = cast<decltype(exponentResult)>(exponentResult + (cast<uint1>(roundedMantissa >> 23) + 127));

        // output
        result.sign = cast<uint1>(isNegativeX);
        result.exponent = (isNegInfinity || isOutputMinus1) ? 127 :
                        (isOutputInf || isPosInfinity || flags.nan) ? 0xFF : cast<decltype(result.exponent)>(exponentResult);
        result.mantissa = flags.nan ? 0x1 :
                        (isOutputInf || isOutputMinus1 || flags.inf) ? 0x0 : cast<decltype(result.mantissa)>(roundedMantissa);
    }
    return cast<float32>(result);
}

const auto LOG2_23_TABLE_LG_SIZE = 5;
const auto LOG2_23_C2_FBIT = 12;
const auto LOG2_23_C1_FBIT = 23;
const auto LOG2_23_C0_FBIT = 34;
const auto LOG2_23_LAST2_C2_FBIT = 12;
const auto LOG2_23_LAST2_C1_FBIT = 24;
const auto LOG2_23_LAST2_C0_FBIT = 36;

struct log2_coefficients
{
    uint37 c0;
    uint25 c1;
    int13 c2;
    uint24 c1_base;
    uint12 c2_base;
}

template<auto C2, auto C1, auto C0>
struct log2_23_section_coefficients
{
    uint<C2> c2;
    uint<C1> c1;
    uint<C0> c0;
}

struct log2_row
{
    log2_23_section_coefficients<6, 17, 28> coeff_0000000;
    log2_23_section_coefficients<6, 17, 29> coeff_0000001;
    log2_23_section_coefficients<7, 18, 30> coeff_000001;
    log2_23_section_coefficients<8, 19, 31> coeff_00001;
    log2_23_section_coefficients<9, 20, 32> coeff_0001;
    log2_23_section_coefficients<9, 20, 33> coeff_001;
    log2_23_section_coefficients<10, 21, 34> coeff_01;
    log2_23_section_coefficients<9, 21, 34> coeff_10;
    log2_23_section_coefficients<7, 19, 34> coeff_110;
    log2_23_section_coefficients<7, 19, 34> coeff_111;
}

template<auto F1, auto F2>
struct log2_23_paramPacked
{
    uint<F1 - F2 - LOG2_23_TABLE_LG_SIZE> residual;
    uint<LOG2_23_TABLE_LG_SIZE> index;
    uint<F2> msbits;
}

template<auto F1, auto F2>
struct log2_residual_and_index
{
    uint<F1 - (F2 + LOG2_23_TABLE_LG_SIZE)> residual;
    uint<LOG2_23_TABLE_LG_SIZE> index;
}

// input: [1, 2) without the implicit leading one
// output: [0, 1]
// This implementation only provides precision up to 24-bit mantissa, less
// precise than the requirement in pow.
template<auto F1, auto F2>
inline uint<1 + F2> log2_23_lookup(uint<F1> fraction)
{
    const auto packedFraction_bit2 = cast<log2_23_paramPacked<F1, 2>>(fraction);
    const auto packedFraction_bit3 = cast<log2_23_paramPacked<F1, 3>>(fraction);
    const auto packedFraction_bit4 = cast<log2_23_paramPacked<F1, 4>>(fraction);
    const auto packedFraction_bit5 = cast<log2_23_paramPacked<F1, 5>>(fraction);
    const auto packedFraction_bit6 = cast<log2_23_paramPacked<F1, 6>>(fraction);
    const auto packedFraction_bit7 = cast<log2_23_paramPacked<F1, 7>>(fraction);
    const auto packedFraction_bit8 = cast<log2_23_paramPacked<F1, 8>>(fraction);
    const auto packedFraction_bit10 = cast<log2_23_paramPacked<F1, 10>>(fraction);
    const uint2 fraction_msbit_0_1 = static_cast(packedFraction_bit2.msbits);
    const uint1 fraction_msbit_2 = static_cast(packedFraction_bit3.msbits);
    const uint1 fraction_msbit_3 = static_cast(packedFraction_bit4.msbits);
    const uint1 fraction_msbit_4 = static_cast(packedFraction_bit5.msbits);
    const uint1 fraction_msbit_5 = static_cast(packedFraction_bit6.msbits);
    const uint1 fraction_msbit_6 = static_cast(packedFraction_bit7.msbits);

    // In the range 1.11xxxx, the precision requirement is much higher when the mantissa is close to 2.
    // The selector is used to identify these sub-ranges.
    const uint2 near_two_selector =
        (packedFraction_bit8.msbits == 0xFF) ? mux(cast<uint2>(packedFraction_bit10.msbits), 0, 0, 2, 3) : 0;

    const bool isLast2Interval = near_two_selector != 0;

    // extract the residual and the table index
    const log2_residual_and_index<F1, 7> ir_000000x = { packedFraction_bit7.residual, packedFraction_bit7.index};

    const log2_residual_and_index<F1, 6> ir_00000xx = mux(fraction_msbit_5,
        { ir_000000x.residual, ir_000000x.index},
        { packedFraction_bit6.residual, packedFraction_bit6.index});

    const log2_residual_and_index<F1, 5> ir_0000xxx = mux(fraction_msbit_4,
        { ir_00000xx.residual, ir_00000xx.index},
        { packedFraction_bit5.residual, packedFraction_bit5.index});

    const log2_residual_and_index<F1, 4> ir_000xxxx = mux(fraction_msbit_3,
        { ir_0000xxx.residual, ir_0000xxx.index},
        { packedFraction_bit4.residual, packedFraction_bit4.index});

    const log2_residual_and_index<F1, 3> ir_00xxxxx = mux(fraction_msbit_2,
        { ir_000xxxx.residual, ir_000xxxx.index},
        { packedFraction_bit3.residual, packedFraction_bit3.index});

    const log2_residual_and_index<F1, 3> ir_111xxxx = mux(isLast2Interval ? 1 : 0,
        { packedFraction_bit3.residual, packedFraction_bit3.index},
        { packedFraction_bit5.residual, 0});

    const log2_residual_and_index<F1, 3> ir_11xxxxx = mux(fraction_msbit_2,
        { packedFraction_bit3.residual, packedFraction_bit3.index},
        { ir_111xxxx.residual, ir_111xxxx.index });

    const log2_residual_and_index<F1, 2> ir = mux(fraction_msbit_0_1,
        { ir_00xxxxx.residual, ir_00xxxxx.index },
        { packedFraction_bit2.residual, packedFraction_bit2.index},
        { packedFraction_bit2.residual, packedFraction_bit2.index},
        { ir_11xxxxx.residual, ir_11xxxxx.index }
    );

    const auto index = ir.index;

    // For the section close to 2, a negative residual is used to reduce the width.
    const int<F1 - LOG2_23_TABLE_LG_SIZE - 1> residual = static_cast((near_two_selector == 3) ? fraction : ir.residual);

    // extract coefficient
    const auto row = log2_23_coefficient_table[index];

    const log2_coefficients coefficient_000000 = {
        mux(fraction_msbit_6, row.coeff_0000000.c0, row.coeff_0000001.c0),
        mux(fraction_msbit_6, row.coeff_0000000.c1, row.coeff_0000001.c1),
        mux(fraction_msbit_6, row.coeff_0000000.c2, row.coeff_0000001.c2),
        mux(fraction_msbit_6, 12011297, 11918880),
        mux(fraction_msbit_6, 2909, 2865) };

    const log2_coefficients coefficient_00000x = {
        mux(fraction_msbit_5, coefficient_000000.c0, row.coeff_000001.c0),
        mux(fraction_msbit_5, coefficient_000000.c1, row.coeff_000001.c1),
        mux(fraction_msbit_5, coefficient_000000.c2, row.coeff_000001.c2),
        mux(fraction_msbit_5, coefficient_000000.c1_base, 11741028),
        mux(fraction_msbit_5, coefficient_000000.c2_base, 2779) };

    const log2_coefficients coefficient_0000xx= {
        mux(fraction_msbit_4, coefficient_00000x.c0, row.coeff_00001.c0),
        mux(fraction_msbit_4, coefficient_00000x.c1, row.coeff_00001.c1),
        mux(fraction_msbit_4, coefficient_00000x.c2, row.coeff_00001.c2),
        mux(fraction_msbit_4, coefficient_00000x.c1_base, 11400786),
        mux(fraction_msbit_4, coefficient_00000x.c2_base, 2620) };

    const log2_coefficients coefficient_000xxx = {
        mux(fraction_msbit_3, coefficient_0000xx.c0, row.coeff_0001.c0),
        mux(fraction_msbit_3, coefficient_0000xx.c1, row.coeff_0001.c1),
        mux(fraction_msbit_3, coefficient_0000xx.c2, row.coeff_0001.c2),
        mux(fraction_msbit_3, coefficient_0000xx.c1_base, 10776217),
        mux(fraction_msbit_3, coefficient_0000xx.c2_base, 2339) };

    const log2_coefficients coefficient_00xxxx = {
        mux(fraction_msbit_2, coefficient_000xxx.c0, row.coeff_001.c0),
        mux(fraction_msbit_2, coefficient_000xxx.c1, row.coeff_001.c1),
        mux(fraction_msbit_2, coefficient_000xxx.c2, row.coeff_001.c2),
        mux(fraction_msbit_2, coefficient_000xxx.c1_base, 9712095),
        mux(fraction_msbit_2, coefficient_000xxx.c2_base, 1897) };

    const log2_coefficients coefficient_111xxx = {
        mux(near_two_selector, row.coeff_111.c0, row.coeff_111.c0, 68622611811, 68719476736),
        mux(near_two_selector, row.coeff_111.c1, row.coeff_111.c1, 12114032, 12102203),
        mux(near_two_selector, row.coeff_111.c2, row.coeff_111.c2, 740, 739),
        mux(near_two_selector, 6062942, 6062942, 0, 0),
        mux(near_two_selector, 741, 741, 0, 0) };

    const log2_coefficients coefficient_11xxxx = {
        mux(fraction_msbit_2, row.coeff_110.c0, coefficient_111xxx.c0),
        mux(fraction_msbit_2, row.coeff_110.c1, coefficient_111xxx.c1),
        mux(fraction_msbit_2, row.coeff_110.c2, coefficient_111xxx.c2),
        mux(fraction_msbit_2, 6467978, coefficient_111xxx.c1_base),
        mux(fraction_msbit_2, 842, coefficient_111xxx.c2_base) };

    const log2_coefficients coefficient = {
        mux(fraction_msbit_0_1, coefficient_00xxxx.c0, row.coeff_01.c0, row.coeff_10.c0, coefficient_11xxxx.c0),
        mux(fraction_msbit_0_1, coefficient_00xxxx.c1, row.coeff_01.c1, row.coeff_10.c1, coefficient_11xxxx.c1),
        mux(fraction_msbit_0_1, coefficient_00xxxx.c2, row.coeff_01.c2, row.coeff_10.c2, coefficient_11xxxx.c2),
        mux(fraction_msbit_0_1, coefficient_00xxxx.c1_base, 8110336, 6946530, coefficient_11xxxx.c1_base),
        mux(fraction_msbit_0_1, coefficient_00xxxx.c2_base, 1320, 969, coefficient_11xxxx.c2_base) };

    const auto alignC2Fbit = isLast2Interval ? 0 : (LOG2_23_LAST2_C2_FBIT - LOG2_23_C2_FBIT);
    const auto alignC1Fbit = isLast2Interval ? 0 : (LOG2_23_LAST2_C1_FBIT - LOG2_23_C1_FBIT);
    const auto alignC0Fbit = isLast2Interval ? 0 : (LOG2_23_LAST2_C0_FBIT - LOG2_23_C0_FBIT);

    const auto c0 = coefficient.c0 << alignC0Fbit;
    const auto c1 = (coefficient.c1 + coefficient.c1_base) << alignC1Fbit;
    const auto c2 = -(coefficient.c2 + coefficient.c2_base) << alignC2Fbit;

    // compute look-up result
    const auto value2Fbit = F1 + LOG2_23_LAST2_C2_FBIT;
    const int<1 + value2Fbit> value2 = residual * c2;

    const auto value1Fbit = F1 + value2Fbit;
    const auto value1 = (value2 + (c1 << (value2Fbit - LOG2_23_LAST2_C1_FBIT))) * residual;

    static assert(value1Fbit >= (F2 + 1));
    const auto value0 = (value1 + (c0 << (value1Fbit - LOG2_23_LAST2_C0_FBIT))) >> (value1Fbit - (F2 + 1));

    return (value0 >> 1) + (value0 & 1);
}

template<auto Base, auto BaseFbit>
inline float32 log2_internal(float32 x)
{
    auto binary32 = cast<float32PackedFormat>(x);
    specialCaseFlags flags = getSpecialCases<Denorm::Off>(binary32);

    const bool isNegative = signbit(x);
    const bool isPosOne = cast<uint32>(x) == float32_constants::POS_ONE;
    const bool isNegInf = flags.inf && isNegative;
    const bool isPosInf = flags.inf && !isNegative;

    // calculate
    const int8 signedExponent = decodeExponent(binary32.exponent);
    const auto fractionFbit = 48;
    const auto fraction = log2_23_lookup<23, fractionFbit>(binary32.mantissa);
    const int<8 + fractionFbit> log2Value = static_cast((signedExponent << fractionFbit) + fraction);

    const auto valueChangedBaseFBit = fractionFbit + BaseFbit;
    const auto valueChangedBase = log2Value * Base;

    binary32.sign = valueChangedBase < 0 ? 1 : 0;
    const auto absValue = binary32.sign ? -valueChangedBase : valueChangedBase;

    const auto leadingOneIndex = highest_one<uint<8 + valueChangedBaseFBit - 24>>(static_cast(absValue >> 24));
    const auto shift = leadingOneIndex.value + 24;
    binary32.exponent = shift - valueChangedBaseFBit + 127;

    assert(shift >= 24);
    const auto truncatedValue = absValue >> (shift - 24);
    auto roundedValue = (truncatedValue >> 1) + (truncatedValue & 1);

    if((roundedValue >> 23) >= 2)
    {
        binary32.exponent += 1;
        roundedValue = roundedValue >> 1;
    }

    binary32.mantissa = cast<decltype(binary32.mantissa)>(roundedValue);

    // output
    float32PackedFormat result;
    result.sign = (isPosOne || isPosInf) ? 0 :
                    flags.zero ? 1 : binary32.sign;
    result.exponent = isPosOne ? 0 :
                        (flags.zero || flags.inf || flags.nan || isNegative) ? 0xFF : binary32.exponent;
    result.mantissa = (isPosOne || isPosInf || flags.zero) ? 0 :
                        (isNegInf || flags.nan || isNegative) ? 1 : binary32.mantissa;

    return cast<float32>(result);
}

//| $log_2(x)$
inline float32 log2(float32 x)
{
    return log2_internal<1, 0>(x);
}

//| $ln(x)$
inline float32 log(float32 x)
{
    return log2_internal<0x2C5C860, 26>(x);
}

//| $log_{10}(x)$
inline float32 log10(float32 x)
{
    return log2_internal<0x1344135, 26>(x);
}

const auto LOG21P_TABLE_LG_SIZE = 9;
const auto LOG21P_C2_FBIT = 13;
const auto LOG21P_C1_FBIT = 23;
const auto LOG21P_C0_FBIT = 43;

template<auto C2, auto C1, auto C0>
struct log21p_coefficients
{
    int<C2> c2;
    int<C1> c1;
    int<C0> c0;
}

struct log21p_row
{
    log21p_coefficients<14, 25, 44> coeff_sections;
    log21p_coefficients<16, 26, 45> coeff_0p5_1;
}

template<auto F1, auto F2>
struct log21p_paramPacked
{
    uint<F1 - (F2 + 5)> residual;
    uint5 index;
    uint<F2> msbits;
}

template<auto F1, auto F2>
struct log21p_residual_and_index
{
    uint<F1 - (F2 + 5)> residual;
    uint5 index;
}

struct log21p_seperated4
{
    uint4 bit_3_0;
    uint4 bit_7_4;
    uint4 bit_11_8;
    uint4 bit_15_12;
}

template<auto F1>
inline int<1 + F1> log21p_lookup(float32PackedFormat value)
{
    const auto Fbit = 46;
    const uint<Fbit> fraction = static_cast(((1 << 23) | value.mantissa) << ((Fbit - 23) + (value.exponent - 127)));

    const auto packedFraction_bit15 = cast<log21p_paramPacked<Fbit, 15>>(fraction);
    const auto packedFraction_bit14 = cast<log21p_paramPacked<Fbit, 14>>(fraction);
    const auto packedFraction_bit13 = cast<log21p_paramPacked<Fbit, 13>>(fraction);
    const auto packedFraction_bit12 = cast<log21p_paramPacked<Fbit, 12>>(fraction);
    const auto packedFraction_bit11 = cast<log21p_paramPacked<Fbit, 11>>(fraction);
    const auto packedFraction_bit10 = cast<log21p_paramPacked<Fbit, 10>>(fraction);
    const auto packedFraction_bit9 = cast<log21p_paramPacked<Fbit, 9>>(fraction);
    const auto packedFraction_bit8 = cast<log21p_paramPacked<Fbit, 8>>(fraction);
    const auto packedFraction_bit7 = cast<log21p_paramPacked<Fbit, 7>>(fraction);
    const auto packedFraction_bit6 = cast<log21p_paramPacked<Fbit, 6>>(fraction);
    const auto packedFraction_bit5 = cast<log21p_paramPacked<Fbit, 5>>(fraction);
    const auto packedFraction_bit4 = cast<log21p_paramPacked<Fbit, 4>>(fraction);
    const auto packedFraction_bit3 = cast<log21p_paramPacked<Fbit, 3>>(fraction);
    const auto packedFraction_bit2 = cast<log21p_paramPacked<Fbit, 2>>(fraction);
    const auto packedFraction_bit1 = cast<log21p_paramPacked<Fbit, 1>>(fraction);

    // extract the power order of 2 of first 16-bit of fraction
    const auto temp16 = cast<log21p_seperated4>(cast<uint16>(fraction >> (Fbit - 16)));
    const uint2 powerOrderOf16 = (temp16.bit_15_12 != 0) ? 3 :
                                 (temp16.bit_11_8 != 0) ? 2 :
                                 (temp16.bit_7_4 != 0) ? 1 : 0;

    const auto temp4 = mux(powerOrderOf16, temp16.bit_3_0, temp16.bit_7_4, temp16.bit_11_8, temp16.bit_15_12);

    const uint2 powerOrderOf2 = ((temp4 >> 3) == 1) ? 3 :
                                ((temp4 >> 2) == 1) ? 2 :
                                ((temp4 >> 1) == 1) ? 1 : 0;

    const uint4 totalPowerOrderOf2 = (powerOrderOf16 << 2) | powerOrderOf2;

    // extract index and residual
    const log21p_residual_and_index<Fbit, 1> ir = mux(totalPowerOrderOf2,
        { packedFraction_bit15.residual, packedFraction_bit15.index },
        { packedFraction_bit15.residual, packedFraction_bit15.index },
        { packedFraction_bit14.residual, packedFraction_bit14.index },
        { packedFraction_bit13.residual, packedFraction_bit13.index },
        { packedFraction_bit12.residual, packedFraction_bit12.index },
        { packedFraction_bit11.residual, packedFraction_bit11.index },
        { packedFraction_bit10.residual, packedFraction_bit10.index },
        { packedFraction_bit9.residual, packedFraction_bit9.index },
        { packedFraction_bit8.residual, packedFraction_bit8.index },
        { packedFraction_bit7.residual, packedFraction_bit7.index },
        { packedFraction_bit6.residual, packedFraction_bit6.index },
        { packedFraction_bit5.residual, packedFraction_bit5.index },
        { packedFraction_bit4.residual, packedFraction_bit4.index },
        { packedFraction_bit3.residual, packedFraction_bit3.index },
        { packedFraction_bit2.residual, packedFraction_bit2.index },
        { packedFraction_bit1.residual, packedFraction_bit1.index });

    const uint9 index = (totalPowerOrderOf2 << 5) | ir.index;
    const auto residual = ir.residual;

    // extract coefficients
    const auto row = log21p_coefficient_table[index];
    const auto c2 = (value.sign == 1) ? row.coeff_0p5_1.c2 : row.coeff_sections.c2;
    const auto c1 = (value.sign == 1) ? row.coeff_0p5_1.c1 : row.coeff_sections.c1;
    const auto c0 = (value.sign == 1) ? row.coeff_0p5_1.c0 : row.coeff_sections.c0;

    // compute log2
    const auto value2Fbit = Fbit + LOG21P_C2_FBIT;
    const auto value2 = residual * c2;

    const auto value1Fbit = Fbit + value2Fbit;
    static assert(value2Fbit >= LOG21P_C1_FBIT);
    const auto value1 = (value2 + (c1 << (value2Fbit - LOG21P_C1_FBIT))) * residual;

    static assert(F1 >= LOG21P_C0_FBIT);
    static assert(value1Fbit >= (F1 + 1));
    const auto value0 = (value1 + (c0 << (value1Fbit - LOG21P_C0_FBIT))) >> (value1Fbit - (F1 + 1));

    return (value0 >> 1) + (value0 & 1);
}

//| Calculate $ln(x+1)$. Denorm inputs and outputs are supported.
// For small values of `x`, `log1p` may be more accurate than `log(1+x)`.
inline float32 log1p(float32 value)
{
    const auto structValueOriginal = cast<float32PackedFormat>(value);
    const auto structValuePlusOne = cast<float32PackedFormat>(value + 1.0);

    const auto binaryValue = cast<uint32>(value);
    const specialCaseFlags flags = getSpecialCases<Denorm::On>(structValueOriginal);

    const bool isUnderflow = structValueOriginal.sign == 1 && structValueOriginal.exponent != 0xFF && binaryValue > float32_constants::NEG_ONE;
    const bool isNegative = signbit(value);
    const bool isPosInfinity = flags.inf && !isNegative;
    const bool isNegInfinity = flags.inf && isNegative;
    const bool isPosZero = flags.zero && !isNegative;
    const bool isNegZero = flags.zero && isNegative;
    const bool isNegOne = binaryValue == float32_constants::NEG_ONE;
    const bool isSmallValue = structValueOriginal.exponent < (127 - 23);

    // calculate log2
    // structValuePlusOne = [0.5, 2) use original precise fraction (before adding 1).
    // Others use fixed 23-bit resolution fraction after adding 1.
    const float32PackedFormat lookupFraction =
        (structValuePlusOne.exponent == 126 || structValuePlusOne.exponent == 127) ? structValueOriginal : {structValuePlusOne.mantissa, 127, 0};

    const auto fractionFbit = 46;
    const auto log21pFraction = log21p_lookup<fractionFbit>(lookupFraction);

    // When the exponent of structValuePlusOne equals to 126, The log21pValue doesn't need to be recovered by exponent.
    // Because the log21pFraction is lookuped by the range structValuePlusOne = [0.5, 2).
    const int8 exponent = static_cast((structValuePlusOne.exponent == 126 || structValuePlusOne.exponent == 127) ? 0 : (structValuePlusOne.exponent - 127));
    const int<8 + fractionFbit> log21pValue = static_cast((exponent << fractionFbit) + log21pFraction);

    // change base to log
    const auto valueChangedBaseFBit = fractionFbit + 30;
    const auto valueChangedBase = log21pValue * 0x2C5C85FE;

    // convert to float
    const auto sign = valueChangedBase < 0 ? 1 : 0;
    const auto absValueFbit = valueChangedBaseFBit;
    const auto absValue = sign ? -valueChangedBase : valueChangedBase;

    const auto leadingOneIndex = highest_one<uint<(8 + 24)>>(static_cast(absValue >> (absValueFbit - 24)));
    const auto shift = leadingOneIndex.value - 24;
    int8 exponentResult = static_cast(shift + 127);

    assert(absValueFbit + shift >= 24);
    const auto truncatedValue = absValue >> (absValueFbit + shift - 24);
    auto mantissaResult = (truncatedValue >> 1) + (truncatedValue & 1);

    if ((mantissaResult >> 23) >= 2)
    {
        exponentResult += 1;
        mantissaResult = mantissaResult >> 1;
    }

    float32PackedFormat result;
    result.sign = (isPosInfinity || isPosZero) ? 0 :
                  (isNegInfinity || isNegZero) ? 1 :
                  isSmallValue ? structValueOriginal.sign : sign;
    result.exponent = (isNegOne || flags.inf || flags.nan || isUnderflow) ? 0xFF :
                      flags.zero ? 0x0 :
                      isSmallValue ? structValueOriginal.exponent : exponentResult;
    result.mantissa = (flags.nan || isNegInfinity || isUnderflow) ? 0x1 :
                      (isNegOne || isPosInfinity || flags.zero) ? 0x0 :
                      isSmallValue ? structValueOriginal.mantissa : cast<decltype(result.mantissa)>(mantissaResult);
    return cast<float32>(result);
}

//| Calculate the floating-point value of the unbiased exponent part of `log(x)`.
// If `x` is a subnormal value, the result of `log2(mantissa part of x)-126` is returned.
inline float32 logb(float32 x)
{
    const auto s = cast<float32PackedFormat>(x);
    const specialCaseFlags flags = getSpecialCases<Denorm::On>(s);
    const bool isDenorm = (s.exponent == 0) && !flags.zero;
    const auto leadingOne = highest_one<uint23>(s.mantissa);
    const int9 value = isDenorm ? (leadingOne.value - 149) : (s.exponent - 127);

    float32 result;
    if(flags.zero)
    {
        result = cast<float32>(float32_constants::NEG_INFINITY);
    }
    else if(flags.inf)
    {
        result = cast<float32>(cast<uint32>(float32_constants::POS_INFINITY));
    }
    else if(flags.nan)
    {
        result = x;
    }
    else
    {
        result = int_to_float<int9, float32, _mantissa_width, _exponent_width, _exponent_bias>(value);
    }
    return result;
}

//| Return the signed integral value of the unbiased exponent part of `log(x)`.
// If `x` is a subnormal value, the result of `log2(mantissa part of x)-126` is returned.
inline int32 ilogb(float32 x)
{
    const auto s = cast<float32PackedFormat>(x);
    const specialCaseFlags flags = getSpecialCases<Denorm::On>(s);
    const bool isDenorm = (s.exponent == 0) && !flags.zero;
    const auto leadingOne = highest_one<uint23>(s.mantissa);

    const auto intMax = 0x7FFFFFFF;
    const auto intMin = 0x80000000;
    int32 result;
    if (flags.inf)
    {
        result = intMax;
    }
    else if (flags.zero || flags.nan)
    {
        result = intMin;
    }
    else if (isDenorm)
    {
        result = leadingOne.value - 149;
    }
    else
    {
        result = s.exponent - 127;
    }
    return result;
}

const auto ATAN_C0_FBIT = 26;
const auto ATAN_C1_FBIT = 26;
const auto ATAN_C2_FBIT = 27;
const auto ATAN_C3_FBIT = 28;
struct atan_row
{
    uint26 c0;
    uint22 c1;
    int16 c2;
    int11 c3;
    int5 exponent;
}

//| Return the principal values of arctangent. Denorm inputs and outputs are supported.
inline float32 atan(float32 x)
{
    const auto structX = cast<float32PackedFormat>(x);
    const auto flags = getSpecialCases<Denorm::Off>(structX);

    uint32 binaryResult;
    if (isnan(x))
    {
        binaryResult = float32_constants::DEFAULT_NAN;
    }
    else if (flags.zero || structX.exponent <= 115)
    {
        binaryResult = cast<uint32>(x);
    }
    else if (structX.exponent >= 150)
    {
        binaryResult = 0x3FC90FDB | (structX.sign << 31);
    }
    else
    {
        const uint6 relativeExponent = static_cast(structX.exponent - 116);
        const uint23 fraction = structX.mantissa;

        const uint7 index_m11 = static_cast((relativeExponent << 3)        | (fraction >> 20));
        const uint9 index_m3  = static_cast((((relativeExponent - 8) << 5)  | (fraction >> 18)) + 64);
        const uint9 index_5   = static_cast(((relativeExponent << 4)        | (fraction >> 19)) + 64);
        const uint9 index_9   = static_cast((((relativeExponent - 20) << 3) | (fraction >> 20)) + 384);
        const uint9 index = (relativeExponent < 16) ?
            ((relativeExponent < 8)  ? index_m11 : index_m3) :
            ((relativeExponent < 20) ? index_5 : index_9);

        const auto RESIDUAL_FBIT = 20;
        const uint<RESIDUAL_FBIT> residual = static_cast((relativeExponent < 16) ?
            ((relativeExponent < 8)  ? fraction        : (fraction << 2)) :
            ((relativeExponent < 20) ? (fraction << 1) : fraction));

        const auto row = atan_table[index];

        const auto c0 = row.c0;
        const auto c1 = row.c1;
        const auto c2 = row.c2;
        const auto c3 = row.c3;
        const auto exponentBase = row.exponent;

        const auto product_3 = (residual * c3) >> (RESIDUAL_FBIT + ATAN_C3_FBIT - ATAN_C2_FBIT);
        const auto product_2 = (residual * (c2 + product_3)) >> (RESIDUAL_FBIT + ATAN_C2_FBIT - ATAN_C1_FBIT);
        const auto product_1 = (residual * (c1 + product_2)) >> (RESIDUAL_FBIT + ATAN_C1_FBIT - ATAN_C0_FBIT);
        const uint26 mantissa26 = static_cast((c0 + product_1) >> (ATAN_C0_FBIT - 26));      // [0.25, 1) with 26 fraction bits

        const uint25 mantissa24 = static_cast(mux(cast<uint1>(mantissa26 >> 25), mantissa26, mantissa26 >> 1));
        const auto   mantExpo24 = mux(cast<uint1>(mantissa26 >> 25), -1, 0);    // the initial -1 has been counted in exponentBase

        const uint25 mantissa23 = (mantissa24 >> 1) + (mantissa24 & 1);
        const auto   mantExpo23 = mantExpo24;

        const uint24 mantissa = static_cast(mux(cast<uint1>(mantissa23 >> 24), mantissa23, mantissa23 >> 1));
        const auto   mantExpo = mantExpo23 + mux(cast<uint1>(mantissa23 >> 24), 0, 1);

        const float32PackedFormat structResult = { static_cast<uint23>(mantissa), static_cast<uint8>(127 + exponentBase + mantExpo), structX.sign};

        binaryResult = cast<uint32>(structResult);
    }

    return cast<float32>(binaryResult);
}

inline tuple2<bool, bool> check_integer(float32 x)
{
    const auto s = cast<float32PackedFormat>(x);
    uint24 m = (1 << 23) | s.mantissa;
    m = cast<decltype(m)>(m << (s.exponent - 127));
    tuple2<bool, bool> flags;
    flags.first = s.exponent >= 127 && (m & 0x7FFFFF) == 0;
    flags.second = ((m >> 23) & 1) == 1;
    return flags;
}

//| ${base}^{expo}$
inline float32 pow(float32 base, float32 expo)
{
    const auto binaryBase = cast<uint32>(base);
    const auto binaryExpo = cast<uint32>(expo);
    const auto structBase = cast<float32PackedFormat>(base);
    const auto structExpo = cast<float32PackedFormat>(expo);

    const specialCaseFlags flagsBase = getSpecialCases<Denorm::Off>(structBase);
    const specialCaseFlags flagsExpo = getSpecialCases<Denorm::Off>(structExpo);
    const bool isNegativeBase = signbit(base);
    const bool isNegativeExpo = signbit(expo);

    const bool isExpoPosInf = !isNegativeExpo && flagsExpo.inf;
    const bool isExpoNegInf = isNegativeExpo && flagsExpo.inf;

    const auto expoIntFlags = check_integer(expo);
    const bool isExpoInteger = expoIntFlags.first;
    const bool isExpoOddInteger = isExpoInteger && expoIntFlags.second;
    const bool isExpoEvenInteger = isExpoInteger && !expoIntFlags.second;

    const uint32 binaryNan = 0x7FC00000;

    uint32 result;
    if (binaryBase == float32_constants::POS_ONE || flagsExpo.zero)
    {
        result = float32_constants::POS_ONE;
    }
    else if (flagsBase.nan || flagsExpo.nan)
    {
        result = binaryNan;
    }
    else if (flagsBase.zero)
    {
        if (!isNegativeBase)
        {
            result = !isNegativeExpo ? float32_constants::POS_ZERO : float32_constants::POS_INFINITY;
        }
        else
        {
            if (!isNegativeExpo)
            {
                result = isExpoOddInteger ? float32_constants::NEG_ZERO : float32_constants::POS_ZERO;
            }
            else
            {
                result = isExpoOddInteger ? float32_constants::NEG_INFINITY : float32_constants::POS_INFINITY;
            }
        }
    }
    else if (binaryBase == float32_constants::NEG_ONE && structExpo.exponent == 0xFF)
    {
        result = float32_constants::POS_ONE;
    }
    else if ((structBase.exponent < 127 && isExpoNegInf) || (structBase.exponent >= 127 && isExpoPosInf))
    {
        result = float32_constants::POS_INFINITY;
    }
    else if ((structBase.exponent < 127 && isExpoPosInf) || (structBase.exponent >= 127 && isExpoNegInf))
    {
        result = float32_constants::POS_ZERO;
    }
    else if (flagsBase.inf)
    {
        if (!isNegativeBase)
        {
            result = isNegativeExpo ? float32_constants::POS_ZERO : float32_constants::POS_INFINITY;
        }
        else
        {
            if (isNegativeExpo)
            {
                result = isExpoOddInteger ? float32_constants::NEG_ZERO : float32_constants::POS_ZERO;
            }
            else
            {
                result = isExpoOddInteger ? float32_constants::NEG_INFINITY : float32_constants::POS_INFINITY;
            }
        }
    }
    else if (isNegativeBase && !isExpoInteger)
    {
        result = binaryNan;
    }
    else if (binaryBase == float32_constants::NEG_ONE)
    {
        result = isExpoEvenInteger ? float32_constants::POS_ONE : float32_constants::NEG_ONE;
    }
    else
    {
        // log2(base)
        const int8 baseExponent = decodeExponent(structBase.exponent);
        const uint62 log2Fraction = static_cast(log2_31_lookup<23, 62>(static_cast((1 << 23) | structBase.mantissa)));
        const int70 signedLog2Value = static_cast((baseExponent << 62) + log2Fraction);
        const uint70 unsignedLog2Value = static_cast((baseExponent >= 0) ? signedLog2Value : -signedLog2Value);
        const auto leadingOneIndex = highest_one<uint<8 + 24>>(unsignedLog2Value >> (62 - 24));

        const auto log2MantissaFbit = 34;
        const auto log2Mantissa = unsignedLog2Value >> (leadingOneIndex.value + (62 - 24) - log2MantissaFbit);
        const auto log2Exponent = leadingOneIndex.value - 24;
        const auto log2Sign = (signedLog2Value < 0) ? 1 : 0;

        // log2(base) * expo
        const uint1 productSign = static_cast(((signedLog2Value < 0) ? 1 : 0) + structExpo.sign);
        int9 productExponent = static_cast(structExpo.exponent - 127 + log2Exponent);
        const auto productMantissaFbit = 23 + log2MantissaFbit;
        auto productMantissa = log2Mantissa * ((1 << 23) | structExpo.mantissa);    // [1, 4)
        if ((productMantissa >> productMantissaFbit) >= 2)
        {
            productExponent += 1;
            productMantissa = productMantissa >> 1;
        }
        assert((productMantissa >> productMantissaFbit) == 1);

        const auto structValue =
            exp2_internal<float32ExpandedFormat, 8, productMantissaFbit, log2MantissaFbit>( {static_cast<uint<8+productMantissaFbit>>(productMantissa), productExponent, productSign} );

        // output
        const bool isResultZero = (productSign == 1 && productExponent >= 7) || (productExponent == 6 && structValue.exponent <= 0);
        const bool isResultInf = (productSign == 0 && productExponent >= 7) || (productExponent == 6 && structValue.exponent == 255);
        const bool isResultPostiveInf = structBase.sign == 0 || isExpoEvenInteger;

        float32PackedFormat structResult;
        structResult.sign = (isResultZero || (!isResultInf && !isExpoOddInteger) || (isResultInf && isResultPostiveInf)) ? 0 :
                            (isResultInf && !isResultPostiveInf) ? 1 : structBase.sign;
        structResult.exponent = isResultZero ? 0 :
                                isResultInf ? 0xFF : cast<decltype(structResult.exponent)>(structValue.exponent);
        structResult.mantissa = (isResultZero || isResultInf) ? 0 : cast<decltype(structResult.mantissa)>(structValue.mantissa);

        result = cast<uint32>(structResult);
    }

    return cast<float32>(result);
}

const auto SIN_TABLE_LG_SIZE = 5;
const auto SIN_C2_FBIT = 14;
const auto SIN_C1_FBIT = 18;
const auto SIN_C0_FBIT = 36;

struct sin_coefficients
{
    uint36 c0;
    int20 c1;
    int15 c2;
    uint14 c2_base;
}

template<auto C2, auto C1, auto C0>
struct sin_section_coefficients
{
    uint<C2> c2;
    uint<C1> c1;
    uint<C0> c0;
}

struct sin_row
{
    sin_section_coefficients<9, 1, 26> coeff_00000000000;
    sin_section_coefficients<10, 1, 27> coeff_00000000001;
    sin_section_coefficients<10, 2, 28> coeff_0000000001;
    sin_section_coefficients<9, 4, 29> coeff_000000001;
    sin_section_coefficients<9, 5, 30> coeff_00000001;
    sin_section_coefficients<8, 7, 31> coeff_0000001;
    sin_section_coefficients<9, 9, 32> coeff_000001;
    sin_section_coefficients<9, 11, 33> coeff_00001;
    sin_section_coefficients<10, 13, 34> coeff_0001;
    sin_section_coefficients<11, 15, 35> coeff_001;
    sin_section_coefficients<11, 17, 36> coeff_010;
    sin_section_coefficients<11, 17, 36> coeff_011;
    sin_section_coefficients<11, 18, 36> coeff_10;
    sin_section_coefficients<10, 19, 36> coeff_11;
}

template<auto F1, auto F2>
struct sin_paramPacked
{
    uint<F1 - F2 - SIN_TABLE_LG_SIZE> residual;
    uint<SIN_TABLE_LG_SIZE> index;
    uint<F2> msbits;
}

template<auto F1, auto F2>
struct sin_residual_and_index
{
    uint<F1 - (F2 + SIN_TABLE_LG_SIZE)> residual;
    uint<SIN_TABLE_LG_SIZE> index;
}

template<auto F1>
inline uint<1 + F1> sin_lookup(uint<1 + F1> significand)
{
    const auto packedFraction_bit2 = cast<sin_paramPacked<(1 + F1), 2>>(significand);
    const auto packedFraction_bit3 = cast<sin_paramPacked<(1 + F1), 3>>(significand);
    const auto packedFraction_bit4 = cast<sin_paramPacked<(1 + F1), 4>>(significand);
    const auto packedFraction_bit5 = cast<sin_paramPacked<(1 + F1), 5>>(significand);
    const auto packedFraction_bit6 = cast<sin_paramPacked<(1 + F1), 6>>(significand);
    const auto packedFraction_bit7 = cast<sin_paramPacked<(1 + F1), 7>>(significand);
    const auto packedFraction_bit8 = cast<sin_paramPacked<(1 + F1), 8>>(significand);
    const auto packedFraction_bit9 = cast<sin_paramPacked<(1 + F1), 9>>(significand);
    const auto packedFraction_bit10 = cast<sin_paramPacked<(1 + F1), 10>>(significand);
    const auto packedFraction_bit11 = cast<sin_paramPacked<(1 + F1), 11>>(significand);
    const auto packedFraction_bit12 = cast<sin_paramPacked<(1 + F1), 12>>(significand);
    const uint2 fraction_msbit_0_1 = static_cast(packedFraction_bit2.msbits);
    const uint1 fraction_msbit_2 = static_cast(packedFraction_bit3.msbits);
    const uint1 fraction_msbit_3 = static_cast(packedFraction_bit4.msbits);
    const uint1 fraction_msbit_4 = static_cast(packedFraction_bit5.msbits);
    const uint1 fraction_msbit_5 = static_cast(packedFraction_bit6.msbits);
    const uint1 fraction_msbit_6 = static_cast(packedFraction_bit7.msbits);
    const uint1 fraction_msbit_7 = static_cast(packedFraction_bit8.msbits);
    const uint1 fraction_msbit_8 = static_cast(packedFraction_bit9.msbits);
    const uint1 fraction_msbit_9 = static_cast(packedFraction_bit10.msbits);
    const uint1 fraction_msbit_10 = static_cast(packedFraction_bit11.msbits);

    // extract the residual and the table index
    const sin_residual_and_index<(1 + F1), 11> ir_0000000000x = mux(fraction_msbit_10,
        { packedFraction_bit12.residual, packedFraction_bit12.index},
        { packedFraction_bit11.residual, packedFraction_bit11.index});

    const sin_residual_and_index<(1 + F1), 10> ir_000000000xx = mux(fraction_msbit_9,
        { ir_0000000000x.residual, ir_0000000000x.index},
        { packedFraction_bit10.residual, packedFraction_bit10.index});

    const sin_residual_and_index<(1 + F1), 9> ir_00000000xxx = mux(fraction_msbit_8,
        { ir_000000000xx.residual, ir_000000000xx.index},
        { packedFraction_bit9.residual, packedFraction_bit9.index});

    const sin_residual_and_index<(1 + F1), 8> ir_0000000xxxx = mux(fraction_msbit_7,
        { ir_00000000xxx.residual, ir_00000000xxx.index},
        { packedFraction_bit8.residual, packedFraction_bit8.index});

    const sin_residual_and_index<(1 + F1), 7> ir_000000xxxxx = mux(fraction_msbit_6,
        { ir_0000000xxxx.residual, ir_0000000xxxx.index},
        { packedFraction_bit7.residual, packedFraction_bit7.index});

    const sin_residual_and_index<(1 + F1), 6> ir_00000xxxxxx = mux(fraction_msbit_5,
        { ir_000000xxxxx.residual, ir_000000xxxxx.index},
        { packedFraction_bit6.residual, packedFraction_bit6.index});

    const sin_residual_and_index<(1 + F1), 5> ir_0000xxxxxxx = mux(fraction_msbit_4,
        { ir_00000xxxxxx.residual, ir_00000xxxxxx.index},
        { packedFraction_bit5.residual, packedFraction_bit5.index});

    const sin_residual_and_index<(1 + F1), 4> ir_000xxxxxxxx = mux(fraction_msbit_3,
        { ir_0000xxxxxxx.residual, ir_0000xxxxxxx.index},
        { packedFraction_bit4.residual, packedFraction_bit4.index});

    const sin_residual_and_index<(1 + F1), 3> ir_00xxxxxxxxx = mux(fraction_msbit_2,
        { ir_000xxxxxxxx.residual, ir_000xxxxxxxx.index},
        { packedFraction_bit3.residual, packedFraction_bit3.index});

    const sin_residual_and_index<(1 + F1), 2> ir = mux(fraction_msbit_0_1,
        { ir_00xxxxxxxxx.residual, ir_00xxxxxxxxx.index},
        { packedFraction_bit3.residual, packedFraction_bit3.index},
        { packedFraction_bit2.residual, packedFraction_bit2.index},
        { packedFraction_bit2.residual, packedFraction_bit2.index});

    const auto index = ir.index;
    const uint<(1 + F1) - (2 + SIN_TABLE_LG_SIZE)> residual = ir.residual;

    // extract coefficient
    const auto row = sin_coefficient_table[index];

    const sin_coefficients coefficient_0000000000x = {
        mux(fraction_msbit_10, row.coeff_00000000000.c0, row.coeff_00000000001.c0),
        mux(fraction_msbit_10, row.coeff_00000000000.c1, row.coeff_00000000001.c1),
        mux(fraction_msbit_10, row.coeff_00000000000.c2, row.coeff_00000000001.c2),
        mux(fraction_msbit_10, 504, 1008) };

    const sin_coefficients coefficient_000000000xx = {
        mux(fraction_msbit_9, coefficient_0000000000x.c0, row.coeff_0000000001.c0),
        mux(fraction_msbit_9, coefficient_0000000000x.c1, row.coeff_0000000001.c1),
        mux(fraction_msbit_9, coefficient_0000000000x.c2, row.coeff_0000000001.c2),
        mux(fraction_msbit_9, coefficient_0000000000x.c2_base, 528) };

    const sin_coefficients coefficient_00000000xxx = {
        mux(fraction_msbit_8, coefficient_000000000xx.c0, row.coeff_000000001.c0),
        mux(fraction_msbit_8, coefficient_000000000xx.c1, row.coeff_000000001.c1),
        mux(fraction_msbit_8, coefficient_000000000xx.c2, row.coeff_000000001.c2),
        mux(fraction_msbit_8, coefficient_000000000xx.c2_base, 304) };

    const sin_coefficients coefficient_0000000xxxx = {
        mux(fraction_msbit_7, coefficient_00000000xxx.c0, row.coeff_00000001.c0),
        mux(fraction_msbit_7, coefficient_00000000xxx.c1, row.coeff_00000001.c1),
        mux(fraction_msbit_7, coefficient_00000000xxx.c2, row.coeff_00000001.c2),
        mux(fraction_msbit_7, coefficient_00000000xxx.c2_base, 241) };

    const sin_coefficients coefficient_000000xxxxx = {
        mux(fraction_msbit_6, coefficient_0000000xxxx.c0, row.coeff_0000001.c0),
        mux(fraction_msbit_6, coefficient_0000000xxxx.c1, row.coeff_0000001.c1),
        mux(fraction_msbit_6, coefficient_0000000xxxx.c2, row.coeff_0000001.c2),
        mux(fraction_msbit_6, coefficient_0000000xxxx.c2_base, 304) };

    const sin_coefficients coefficient_00000xxxxxx = {
        mux(fraction_msbit_5, coefficient_000000xxxxx.c0, row.coeff_000001.c0),
        mux(fraction_msbit_5, coefficient_000000xxxxx.c1, row.coeff_000001.c1),
        mux(fraction_msbit_5, coefficient_000000xxxxx.c2, row.coeff_000001.c2),
        mux(fraction_msbit_5, coefficient_000000xxxxx.c2_base, 521) };

    const sin_coefficients coefficient_0000xxxxxxx = {
        mux(fraction_msbit_4, coefficient_00000xxxxxx.c0, row.coeff_00001.c0),
        mux(fraction_msbit_4, coefficient_00000xxxxxx.c1, row.coeff_00001.c1),
        mux(fraction_msbit_4, coefficient_00000xxxxxx.c2, row.coeff_00001.c2),
        mux(fraction_msbit_4, coefficient_00000xxxxxx.c2_base, 1010) };

    const sin_coefficients coefficient_000xxxxxxxx = {
        mux(fraction_msbit_3, coefficient_0000xxxxxxx.c0, row.coeff_0001.c0),
        mux(fraction_msbit_3, coefficient_0000xxxxxxx.c1, row.coeff_0001.c1),
        mux(fraction_msbit_3, coefficient_0000xxxxxxx.c2, row.coeff_0001.c2),
        mux(fraction_msbit_3, coefficient_0000xxxxxxx.c2_base, 2006) };

    const sin_coefficients coefficient_00xxxxxxxxx = {
        mux(fraction_msbit_2, coefficient_000xxxxxxxx.c0, row.coeff_001.c0),
        mux(fraction_msbit_2, coefficient_000xxxxxxxx.c1, row.coeff_001.c1),
        mux(fraction_msbit_2, coefficient_000xxxxxxxx.c2, row.coeff_001.c2),
        mux(fraction_msbit_2, coefficient_000xxxxxxxx.c2_base, 3898) };

    const sin_coefficients coefficient_01xxxxxxxxx = {
        mux(fraction_msbit_2, row.coeff_010.c0, row.coeff_011.c0),
        mux(fraction_msbit_2, row.coeff_010.c1, row.coeff_011.c1),
        mux(fraction_msbit_2, row.coeff_010.c2, row.coeff_011.c2),
        mux(fraction_msbit_2, 5560, 6877) };

    const sin_coefficients coefficient = {
        mux(fraction_msbit_0_1, coefficient_00xxxxxxxxx.c0, coefficient_01xxxxxxxxx.c0, row.coeff_10.c0, row.coeff_11.c0),
        mux(fraction_msbit_0_1, coefficient_00xxxxxxxxx.c1, coefficient_01xxxxxxxxx.c1, row.coeff_10.c1, row.coeff_11.c1),
        mux(fraction_msbit_0_1, coefficient_00xxxxxxxxx.c2, coefficient_01xxxxxxxxx.c2, row.coeff_10.c2, row.coeff_11.c2),
        mux(fraction_msbit_0_1, coefficient_00xxxxxxxxx.c2_base, coefficient_01xxxxxxxxx.c2_base, 8166, 8193) };

    const uint36 c0 = coefficient.c0;
    const int20 c1 = static_cast((1 << SIN_C1_FBIT) - coefficient.c1);
    const int15 c2 = static_cast(coefficient.c2 - coefficient.c2_base);

    // compute look-up result
    const auto value2Fbit = SIN_C2_FBIT + F1;
    const auto value2 = c2 * residual;

    static assert(value2Fbit >= SIN_C1_FBIT);
    const auto value1Fbit = value2Fbit + F1;
    const auto value1 = (value2 + (c1 << (value2Fbit - SIN_C1_FBIT))) * residual;

    static assert(value1Fbit >= SIN_C0_FBIT);
    static assert(value1Fbit >= F1);
    const auto value0 = (value1 + (c0 << (value1Fbit - SIN_C0_FBIT))) >> (value1Fbit - F1);

    return value0;
}

const auto COS_TABLE_LG_SIZE = 5;
const auto COS_C2_FBIT = 16;
const auto COS_C1_FBIT = 24;
const auto COS_C0_FBIT = 38;

struct cos_coefficients
{
    uint39 c0;
    int26 c1;
    int17 c2;
    uint34 c0_base;
    uint23 c1_base;
    uint15 c2_base;
    uint6 c0_fbit;
    uint5 c1_fbit;
    uint5 c2_fbit;
}

template<auto C2, auto C1, auto C0>
struct cos_section_coefficients
{
    uint<C2> c2;
    uint<C1> c1;
    uint<C0> c0;
}

struct cos_row
{
    cos_section_coefficients<11, 17, 23> coeff_00;
    cos_section_coefficients<14, 23, 33> coeff_01;
    cos_section_coefficients<12, 15, 28> coeff_100;
    cos_section_coefficients<6, 14, 26> coeff_1010;
    cos_section_coefficients<8, 15, 26> coeff_1011;
    cos_section_coefficients<8, 12, 30> coeff_11000;
    cos_section_coefficients<3, 8, 28> coeff_11001000;
    cos_section_coefficients<1, 1, 25> coeff_1100100100000;
    cos_section_coefficients<1, 1, 24> coeff_11001001000010;
    cos_section_coefficients<1, 1, 25> coeff_110010010000110;
}

template<auto F1, auto F2>
struct cos_paramPacked
{
    uint<F1 - F2 - COS_TABLE_LG_SIZE> residual;
    uint<COS_TABLE_LG_SIZE> index;
    uint<F2> msbits;
}

template<auto F1, auto F2>
struct cos_residual_and_index
{
    uint<F1 - (F2 + COS_TABLE_LG_SIZE)> residual;
    uint<COS_TABLE_LG_SIZE> index;
}

template<auto F1>
inline uint<1 + F1> cos_lookup(uint<1 + F1> significand)
{
    const auto packedFraction_bit2 = cast<sin_paramPacked<(1 + F1), 2>>(significand);
    const auto packedFraction_bit3 = cast<sin_paramPacked<(1 + F1), 3>>(significand);
    const auto packedFraction_bit4 = cast<sin_paramPacked<(1 + F1), 4>>(significand);
    const auto packedFraction_bit5 = cast<sin_paramPacked<(1 + F1), 5>>(significand);
    const auto packedFraction_bit8 = cast<sin_paramPacked<(1 + F1), 8>>(significand);
    const auto packedFraction_bit13 = cast<sin_paramPacked<(1 + F1), 13>>(significand);
    const auto packedFraction_bit14 = cast<sin_paramPacked<(1 + F1), 14>>(significand);
    const auto packedFraction_bit15 = cast<sin_paramPacked<(1 + F1), 15>>(significand);
    const uint2 fraction_msbit_0_1 = static_cast(packedFraction_bit2.msbits);
    const uint1 fraction_msbit_2 = static_cast(packedFraction_bit3.msbits);
    const uint1 fraction_msbit_3 = static_cast(packedFraction_bit4.msbits);
    const uint1 fraction_msbit_4 = static_cast(packedFraction_bit5.msbits);
    const uint1 fraction_msbit_7 = static_cast(packedFraction_bit8.msbits);
    const uint1 fraction_msbit_12 = static_cast(packedFraction_bit13.msbits);
    const uint1 fraction_msbit_13 = static_cast(packedFraction_bit14.msbits);

    // extract the residual and the table index
    const cos_residual_and_index<(1 + F1), 14> ir_1100100100001xxx = mux(fraction_msbit_13,
        { packedFraction_bit14.residual, packedFraction_bit14.index},
        { packedFraction_bit15.residual, packedFraction_bit15.index});

    const cos_residual_and_index<(1 + F1), 13> ir_110010010000xxxx = mux(fraction_msbit_12,
        { packedFraction_bit13.residual, packedFraction_bit13.index},
        { ir_1100100100001xxx.residual, ir_1100100100001xxx.index});

    const cos_residual_and_index<(1 + F1), 8> ir_1100100xxxxxxxxx = mux(fraction_msbit_7,
        { packedFraction_bit8.residual, packedFraction_bit8.index},
        { ir_110010010000xxxx.residual, ir_110010010000xxxx.index});

    const cos_residual_and_index<(1 + F1), 5> ir_1100xxxxxxxxxxxx = mux(fraction_msbit_4,
        { packedFraction_bit5.residual, packedFraction_bit5.index},
        { ir_1100100xxxxxxxxx.residual, ir_1100100xxxxxxxxx.index});

    const cos_residual_and_index<(1 + F1), 3> ir_10xxxxxxxxxxxxxx = mux(fraction_msbit_2,
        { packedFraction_bit3.residual, packedFraction_bit3.index},
        { packedFraction_bit4.residual, packedFraction_bit4.index});

    const cos_residual_and_index<(1 + F1), 2> ir = mux(fraction_msbit_0_1,
        { packedFraction_bit2.residual, packedFraction_bit2.index},
        { packedFraction_bit2.residual, packedFraction_bit2.index},
        { ir_10xxxxxxxxxxxxxx.residual, ir_10xxxxxxxxxxxxxx.index},
        { ir_1100xxxxxxxxxxxx.residual, ir_1100xxxxxxxxxxxx.index});

    const auto index = ir.index;
    const uint<(1 + F1) - (2 + COS_TABLE_LG_SIZE)> residual = ir.residual;

    // extract coefficient
    const auto row = cos_coefficient_table[index];

    const cos_coefficients coefficient_1100100100001xxx = {
        mux(fraction_msbit_13, row.coeff_11001001000010.c0, row.coeff_110010010000110.c0),
        mux(fraction_msbit_13, row.coeff_11001001000010.c1, row.coeff_110010010000110.c1),
        mux(fraction_msbit_13, row.coeff_11001001000010.c2, row.coeff_110010010000110.c2),
        mux(fraction_msbit_13, 0, 0),
        mux(fraction_msbit_13, 0, 0),
        mux(fraction_msbit_13, 0, 0),
        mux(fraction_msbit_13, 36, 38),
        mux(fraction_msbit_13, 0, 0),
        mux(fraction_msbit_13, 0, 0)};

    const cos_coefficients coefficient_110010010000xxxx = {
        mux(fraction_msbit_12, row.coeff_1100100100000.c0, coefficient_1100100100001xxx.c0),
        mux(fraction_msbit_12, row.coeff_1100100100000.c1, coefficient_1100100100001xxx.c1),
        mux(fraction_msbit_12, row.coeff_1100100100000.c2, coefficient_1100100100001xxx.c2),
        mux(fraction_msbit_12, 0, coefficient_1100100100001xxx.c0_base),
        mux(fraction_msbit_12, 0, coefficient_1100100100001xxx.c1_base),
        mux(fraction_msbit_12, 0, coefficient_1100100100001xxx.c2_base),
        mux(fraction_msbit_12, 36, coefficient_1100100100001xxx.c0_fbit),
        mux(fraction_msbit_12, 0, coefficient_1100100100001xxx.c1_fbit),
        mux(fraction_msbit_12, 0, coefficient_1100100100001xxx.c2_fbit)};

    const cos_coefficients coefficient_1100100xxxxxxxxx = {
        mux(fraction_msbit_7, row.coeff_11001000.c0, coefficient_110010010000xxxx.c0),
        mux(fraction_msbit_7, row.coeff_11001000.c1, coefficient_110010010000xxxx.c1),
        mux(fraction_msbit_7, row.coeff_11001000.c2, coefficient_110010010000xxxx.c2),
        mux(fraction_msbit_7, 25012768, coefficient_110010010000xxxx.c0_base),
        mux(fraction_msbit_7, 4194160, coefficient_110010010000xxxx.c1_base),
        mux(fraction_msbit_7, 1, coefficient_110010010000xxxx.c2_base),
        mux(fraction_msbit_7, 35, coefficient_110010010000xxxx.c0_fbit),
        mux(fraction_msbit_7, 22, coefficient_110010010000xxxx.c1_fbit),
        mux(fraction_msbit_7, 11, coefficient_110010010000xxxx.c2_fbit)};

    const cos_coefficients coefficient_1100xxxxxxxxxxxx = {
        mux(fraction_msbit_4, row.coeff_11000.c0, coefficient_1100100xxxxxxxxx.c0),
        mux(fraction_msbit_4, row.coeff_11000.c1, coefficient_1100100xxxxxxxxx.c1),
        mux(fraction_msbit_4, row.coeff_11000.c2, coefficient_1100100xxxxxxxxx.c2),
        mux(fraction_msbit_4, 176081154, coefficient_1100100xxxxxxxxx.c0_base),
        mux(fraction_msbit_4, 1045950, coefficient_1100100xxxxxxxxx.c1_base),
        mux(fraction_msbit_4, 38, coefficient_1100100xxxxxxxxx.c2_base),
        mux(fraction_msbit_4, 34, coefficient_1100100xxxxxxxxx.c0_fbit),
        mux(fraction_msbit_4, 20, coefficient_1100100xxxxxxxxx.c1_fbit),
        mux(fraction_msbit_4, 13, coefficient_1100100xxxxxxxxx.c2_fbit)};

    const cos_coefficients coefficient_101xxxxxxxxxxxxx = {
        mux(fraction_msbit_3, row.coeff_1010.c0, row.coeff_1011.c0),
        mux(fraction_msbit_3, row.coeff_1010.c1, row.coeff_1011.c1),
        mux(fraction_msbit_3, row.coeff_1010.c2, row.coeff_1011.c2),
        mux(fraction_msbit_3, 106503287, 40068350),
        mux(fraction_msbit_3, 497542, 1028542),
        mux(fraction_msbit_3, 101, 149),
        mux(fraction_msbit_3, 29, 29),
        mux(fraction_msbit_3, 19, 20),
        mux(fraction_msbit_3, 10, 12)};

    const cos_coefficients coefficient_10xxxxxxxxxxxxxx = {
        mux(fraction_msbit_2, row.coeff_100.c0, coefficient_101xxxxxxxxxxxxx.c0),
        mux(fraction_msbit_2, row.coeff_100.c1, coefficient_101xxxxxxxxxxxxx.c1),
        mux(fraction_msbit_2, row.coeff_100.c2, coefficient_101xxxxxxxxxxxxx.c2),
        mux(fraction_msbit_2, 346525063, coefficient_101xxxxxxxxxxxxx.c0_base),
        mux(fraction_msbit_2, 220588, coefficient_101xxxxxxxxxxxxx.c1_base),
        mux(fraction_msbit_2, 5227, coefficient_101xxxxxxxxxxxxx.c2_base),
        mux(fraction_msbit_2, 30, coefficient_101xxxxxxxxxxxxx.c0_fbit),
        mux(fraction_msbit_2, 18, coefficient_101xxxxxxxxxxxxx.c1_fbit),
        mux(fraction_msbit_2, 15, coefficient_101xxxxxxxxxxxxx.c2_fbit)};

    const cos_coefficients coefficient = {
        mux(fraction_msbit_0_1, row.coeff_00.c0, row.coeff_01.c0, coefficient_10xxxxxxxxxxxxxx.c0, coefficient_1100xxxxxxxxxxxx.c0),
        mux(fraction_msbit_0_1, row.coeff_00.c1, row.coeff_01.c1, coefficient_10xxxxxxxxxxxxxx.c1, coefficient_1100xxxxxxxxxxxx.c1),
        mux(fraction_msbit_0_1, row.coeff_00.c2, row.coeff_01.c2, coefficient_10xxxxxxxxxxxxxx.c2, coefficient_1100xxxxxxxxxxxx.c2),
        mux(fraction_msbit_0_1, 59389075, 9507061610, coefficient_10xxxxxxxxxxxxxx.c0_base, coefficient_1100xxxxxxxxxxxx.c0_base),
        mux(fraction_msbit_0_1, 0, 8043612, coefficient_10xxxxxxxxxxxxxx.c1_base, coefficient_1100xxxxxxxxxxxx.c1_base),
        mux(fraction_msbit_0_1, 14439, 17919, coefficient_10xxxxxxxxxxxxxx.c2_base, coefficient_1100xxxxxxxxxxxx.c2_base),
        mux(fraction_msbit_0_1, 26, 34, coefficient_10xxxxxxxxxxxxxx.c0_fbit, coefficient_1100xxxxxxxxxxxx.c0_fbit),
        mux(fraction_msbit_0_1, 18, 24, coefficient_10xxxxxxxxxxxxxx.c1_fbit, coefficient_1100xxxxxxxxxxxx.c1_fbit),
        mux(fraction_msbit_0_1, 15, 16, coefficient_10xxxxxxxxxxxxxx.c2_fbit, coefficient_1100xxxxxxxxxxxx.c2_fbit)};

    const uint39 c0 = static_cast((coefficient.c0 + coefficient.c0_base) << (COS_C0_FBIT - coefficient.c0_fbit));
    const int26 c1 = static_cast(-(coefficient.c1 + coefficient.c1_base) << (COS_C1_FBIT - coefficient.c1_fbit));
    const int17 c2 = static_cast(-(coefficient.c2 + coefficient.c2_base) << (COS_C2_FBIT - coefficient.c2_fbit));

    // compute look-up result
    const auto value2Fbit = COS_C2_FBIT + F1;
    const int<1 + value2Fbit> value2 = c2 * residual;

    static assert(value2Fbit >= COS_C1_FBIT);
    const auto value1Fbit = value2Fbit + F1;
    const int<1 + value1Fbit> value1 = (value2 + (c1 << (value2Fbit - COS_C1_FBIT))) * residual;

    static assert(value1Fbit >= COS_C0_FBIT);
    static assert(value1Fbit >= F1);
    const auto value0 = (value1 + (c0 << (value1Fbit - COS_C0_FBIT))) >> (value1Fbit - F1);

    return value0;
}

template<auto F1, bool Cosine>
inline pair<uint1, pair<bool, uint<1 + F1>>> to_piOver2(float32PackedFormat x)
{
    const auto invPiOver2Fbit = F1;
    static assert(invPiOver2Fbit <= 64);
    const auto invPiOver2 = 0xA2F9836E4E44152A >> (64 - invPiOver2Fbit);
    const auto piOver2Fbit = F1;
    static assert(piOver2Fbit <= 64);
    const auto piOver2 = 0x1921FB54442D18469 >> (64 - piOver2Fbit);

    int8 exponent = decodeExponent(x.exponent);

    const auto Fbit = 23 + invPiOver2Fbit - exponent;
    const auto temp = ((1 << 23) | x.mantissa) * invPiOver2;

    const uint2 part = static_cast(temp >> Fbit);
    const auto valueInPiOver2 = temp & ((1 << Fbit) - 1);

    assert(Fbit + piOver2Fbit >= (F1 + 1));
    auto value = (valueInPiOver2 * piOver2) >> (Fbit + piOver2Fbit - (F1 + 1));
    const auto roundedValue = (value >> 1) + (value & 1);

    const auto complementValue = piOver2 - roundedValue;
    auto result = (part == 1 || part == 3) ? complementValue : roundedValue;

    uint1 sign;
    bool isSmallValue;
    if(Cosine)
    {
        sign = (part == 1 || part == 2) ? 1 : 0;

        // result will output directly when significand >= 1.10010010000111xxxx (close to pi/2)
        isSmallValue = (result >> (F1 - 14)) == 0x6487;
        if(isSmallValue)
        {
            result = ((part == 1 || part == 3) ? roundedValue : complementValue);
        }
    }
    else
    {
        sign = (part == 2 || part == 3) ? 1 : 0;

        // result will output directly when significand < 2^12.
        isSmallValue = (result >> (F1 - 11)) == 0;
    }

    return {sign, {isSmallValue, result}};
}

//| $sin(x)$ with `x` in radians.
inline float32 sin(float32 x)
{
    const auto structValue = cast<float32PackedFormat>(x);
    float32PackedFormat result;

    const specialCaseFlags flags = getSpecialCases<Denorm::On>(structValue);

    // x pass to result directly when input <= 2^-12
    const bool isSmallValue = structValue.exponent <= 115;

    const auto Fbit = 54;
    const auto valueInPiOver2 = to_piOver2<Fbit, false>(structValue);

    // table lookup
    const auto isInternalSmallValue = valueInPiOver2.second.first;
    const auto sineValue = isInternalSmallValue ? valueInPiOver2.second.second : sin_lookup<Fbit>(valueInPiOver2.second.second);

    // output
    result.sign = valueInPiOver2.first ^ structValue.sign;

    const auto leadingOne = highest_one<uint<1 + 27>>(sineValue >> (Fbit - 27));
    int8 exponent = leadingOne.value - 27;

    assert(Fbit + exponent >= 24);
    const auto truncatedSineValue = sineValue >> (Fbit + exponent - 24);
    const auto roundedSineValue = (truncatedSineValue >> 1) + (truncatedSineValue & 1);
    exponent = exponent + (roundedSineValue >> 24);

    result.exponent = (flags.nan || flags.inf) ? 0xFF : (exponent + 127);
    result.mantissa = (flags.nan || flags.inf) ? 0x1 : cast<decltype(result.mantissa)>(roundedSineValue);

    return isSmallValue ? x : cast<float32>(result);
}

//| $cos(x)$ with `x` in radians.
inline float32 cos(float32 x)
{
    const auto structValue = cast<float32PackedFormat>(x);
    float32PackedFormat result;

    const specialCaseFlags flags = getSpecialCases<Denorm::On>(structValue);

    const auto Fbit = 54;
    const auto valueInPiOver2 = to_piOver2<Fbit, true>(structValue);

    // table lookup
    const auto isInternalSmallValue = valueInPiOver2.second.first;
    const auto cosineValue = isInternalSmallValue ? valueInPiOver2.second.second : cos_lookup<Fbit>(valueInPiOver2.second.second);

    // output
    result.sign = valueInPiOver2.first;

    const auto leadingOne = highest_one<uint<1 + 27>>(cosineValue >> (Fbit - 27));
    int8 exponent = leadingOne.value - 27;

    assert(Fbit + exponent >= 24);
    const auto truncatedCosineValue = cosineValue >> (Fbit + exponent - 24);
    const auto roundedCosineValue = (truncatedCosineValue >> 1) + (truncatedCosineValue & 1);
    exponent = exponent + (roundedCosineValue >> 24);

    result.exponent = (flags.nan || flags.inf) ? 0xFF : (exponent + 127);
    result.mantissa = (flags.nan || flags.inf) ? 0x1 : cast<decltype(result.mantissa)>(roundedCosineValue);

    return cast<float32>(result);
}

//| Return the smaller of `x` and `y`. A NaN is treated as missing data, and the other value will be returned.
template<Denorm denorm_mode>
inline float32 min(float32 x, float32 y)
{
    return (isnan(y) || lt<denorm_mode>(x, y)) ? x : y;
}

//| Return the larger of `x` and `y`. A NaN is treated as missing data, and the other value will be returned.
template<Denorm denorm_mode>
inline float32 max(float32 x, float32 y)
{
    return (isnan(y) || gt<denorm_mode>(x, y)) ? x : y;
}

//| Return the smallest integer not less than `x`: $\lceil x \rceil$
template<Denorm denorm_mode>
inline float32 ceil(float32 x)
{
    return ceil_internal<float32, _mantissa_width, _exponent_width, _exponent_bias, denorm_mode>(x);
}

//| Return the largest integer not greater than `x`: $\lfloor x \rfloor$
template<Denorm denorm_mode>
inline float32 floor(float32 x)
{
    return floor_internal<float32, _mantissa_width, _exponent_width, _exponent_bias, denorm_mode>(x);
}

//| Return the nearest integer to `x`, rounding halfway away from zero.
template<Denorm denorm_mode>
inline float32 round(float32 x)
{
    return round_internal<float32, _mantissa_width, _exponent_width, _exponent_bias, denorm_mode>(x);
}

//| Return the nearest integer not greater in magnitude than `x`.
template<Denorm denorm_mode>
inline float32 trunc(float32 x)
{
    return trunc_internal<float32, _mantissa_width, _exponent_width, _exponent_bias, denorm_mode>(x);
}

//| The positive difference between `x` and `y`: `max(x - y, 0)`.
// Return NaN if `x` or `y` is NaN.
template<Denorm denorm_mode>
inline float32 dim(float32 x, float32 y)
{
    float32 result;

    if (isnan(x) || isnan(y))
    {
        result = cast<float32>(cast<uint32>(float32_constants::DEFAULT_NAN));
    }
    else
    {
        result = gt<denorm_mode>(x, y) ? sub<denorm_mode>(x, y) : 0.0;
    }

    return result;
}

//| Return the absolute value of `x`: $|x|$
template<Denorm denorm_mode>
inline float32 abs(float32 x)
{
    auto structValue = cast<float32PackedFormat>(x);
    structValue.sign = 0;
    return cast<float32>(structValue);
}

//| Convert a signed integer to a float32.
template <auto N /*< Width of `value`.*/>
inline float32 from_int(int<N> value)
{
    return int_to_float<int<N>, float32, _mantissa_width, _exponent_width, _exponent_bias>(value);
}

//| Convert an unsigned integer to a float32.
template <auto N /*< Width of `value`.*/>
inline float32 from_uint(uint<N> value)
{
    return int_to_float<uint<N>, float32, _mantissa_width, _exponent_width, _exponent_bias>(value);
}

//| Convert a float32 to a signed integer.
template <auto N /*< Width of the signed integer.*/>
inline optional<int<N>> to_int(float32 value)
{
    auto result = float_to_int<float32, int<N+1>, _mantissa_width, _exponent_width, _exponent_bias>(value);
    if (result.is_valid)
    {
        auto limits = get_limits<int<N>>();
        if ((result.value < limits.first) || (result.value > limits.second))
            result.is_valid = false;
    }

    return make_optional<int<N>>(result.is_valid, cast<int<N>>(result.value));
}

//| Convert a float32 to an unsigned integer
template <auto N /*< Width of the unsigned integer.*/>
inline optional<uint<N>> to_uint(float32 value)
{
    auto result = float_to_int<float32, uint<N+1>, _mantissa_width, _exponent_width, _exponent_bias>(value);
    if (result.is_valid)
    {
        auto limits = get_limits<uint<N>>();
        if ((result.value < limits.first) || (result.value > limits.second))
            result.is_valid = false;
    }

    return make_optional<uint<N>>(result.is_valid, cast<uint<N>>(result.value));
}

const memory<exp2m1_row, (1 << EXP2M1_TABLE_LG_SIZE)> exp2m1_coefficient_table =
{
    {{ 3903, 1453635, 0 }, { 3969, -1453635, 0 }},
    {{ 3583, 1453636, 11629084 }, { 3648, -1453634, -11629076 }},
    {{ 3263, 1453637, 23258175 }, { 3328, -1453633, -23258145 }},
    {{ 2942, 1453638, 34887274 }, { 3008, -1453632, -34887205 }},
    {{ 2622, 1453639, 46516381 }, { 2688, -1453631, -46516259 }},
    {{ 2302, 1453640, 58145496 }, { 2367, -1453630, -58145304 }},
    {{ 1982, 1453641, 69774618 }, { 2047, -1453629, -69774342 }},
    {{ 1661, 1453642, 81403748 }, { 1727, -1453628, -81403372 }},
    {{ 1341, 1453643, 93032885 }, { 1406, -1453627, -93032394 }},
    {{ 1021, 1453644, 104662031 }, { 1086, -1453626, -104661409 }},
    {{ 701, 1453645, 116291184 }, { 766, -1453625, -116290416 }},
    {{ 381, 1453646, 127920344 }, { 445, -1453624, -127919415 }},
    {{ 60, 1453647, 139549513 }, { 125, -1453623, -139548407 }},
    {{ 7932, 1453647, 151178690 }, { 7997, -1453623, -151177390 }},
    {{ 7612, 1453648, 162807873 }, { 7676, -1453622, -162806366 }},
    {{ 7292, 1453649, 174437065 }, { 7356, -1453621, -174435334 }},
    {{ 6972, 1453650, 186066264 }, { 7036, -1453620, -186064295 }},
    {{ 6652, 1453651, 197695471 }, { 6715, -1453619, -197693248 }},
    {{ 6331, 1453652, 209324685 }, { 6395, -1453618, -209322194 }},
    {{ 6011, 1453653, 220953907 }, { 6075, -1453617, -220951132 }},
    {{ 5691, 1453654, 232583137 }, { 5754, -1453616, -232580062 }},
    {{ 5371, 1453655, 244212375 }, { 5434, -1453615, -244208984 }},
    {{ 5051, 1453656, 255841620 }, { 5114, -1453614, -255837899 }},
    {{ 4731, 1453657, 267470873 }, { 4793, -1453613, -267466806 }},
    {{ 4411, 1453658, 279100133 }, { 4473, -1453612, -279095705 }},
    {{ 4090, 1453659, 290729401 }, { 4152, -1453611, -290724597 }},
    {{ 3770, 1453660, 302358677 }, { 3832, -1453610, -302353481 }},
    {{ 3450, 1453661, 313987961 }, { 3512, -1453609, -313982357 }},
    {{ 3130, 1453662, 325617252 }, { 3191, -1453608, -325611226 }},
    {{ 2810, 1453663, 337246551 }, { 2871, -1453607, -337240087 }},
    {{ 2490, 1453664, 348875858 }, { 2550, -1453606, -348868940 }},
    {{ 2170, 1453665, 360505173 }, { 2230, -1453605, -360497786 }},
    {{ 1850, 1453666, 372134495 }, { 1910, -1453604, -372126623 }},
    {{ 1530, 1453667, 383763824 }, { 1589, -1453603, -383755454 }},
    {{ 1209, 1453668, 395393162 }, { 1269, -1453602, -395384276 }},
    {{ 889, 1453669, 407022507 }, { 948, -1453601, -407013091 }},
    {{ 569, 1453670, 418651860 }, { 628, -1453600, -418641898 }},
    {{ 249, 1453671, 430281220 }, { 307, -1453599, -430270697 }},
    {{ -71, 1453672, 441910589 }, { -13, -1453598, -441899489 }},
    {{ 7801, 1453672, 453539965 }, { 7858, -1453598, -453528272 }},
    {{ 7481, 1453673, 465169349 }, { 7538, -1453597, -465157049 }},
    {{ 7161, 1453674, 476798740 }, { 7218, -1453596, -476785817 }},
    {{ 6841, 1453675, 488428139 }, { 6897, -1453595, -488414578 }},
    {{ 6521, 1453676, 500057546 }, { 6577, -1453594, -500043332 }},
    {{ 6201, 1453677, 511686960 }, { 6256, -1453593, -511672077 }},
    {{ 5881, 1453678, 523316382 }, { 5936, -1453592, -523300815 }},
    {{ 5561, 1453679, 534945812 }, { 5615, -1453591, -534929545 }},
    {{ 5241, 1453680, 546575249 }, { 5295, -1453590, -546558268 }},
    {{ 4921, 1453681, 558204694 }, { 4974, -1453589, -558186983 }},
    {{ 4601, 1453682, 569834147 }, { 4654, -1453588, -569815690 }},
    {{ 4281, 1453683, 581463608 }, { 4333, -1453587, -581444389 }},
    {{ 3961, 1453684, 593093076 }, { 4013, -1453586, -593073081 }},
    {{ 3641, 1453685, 604722552 }, { 3692, -1453585, -604701765 }},
    {{ 3321, 1453686, 616352035 }, { 3371, -1453584, -616330442 }},
    {{ 3001, 1453687, 627981526 }, { 3051, -1453583, -627959111 }},
    {{ 2681, 1453688, 639611025 }, { 2730, -1453582, -639587772 }},
    {{ 2361, 1453689, 651240532 }, { 2410, -1453581, -651216425 }},
    {{ 2041, 1453690, 662870046 }, { 2089, -1453580, -662845071 }},
    {{ 1721, 1453691, 674499568 }, { 1769, -1453579, -674473709 }},
    {{ 1401, 1453692, 686129098 }, { 1448, -1453578, -686102339 }},
    {{ 1081, 1453693, 697758635 }, { 1128, -1453577, -697730962 }},
    {{ 761, 1453694, 709388180 }, { 807, -1453576, -709359576 }},
    {{ 441, 1453695, 721017733 }, { 486, -1453575, -720988184 }},
    {{ 121, 1453696, 732647293 }, { 166, -1453574, -732616783 }},
    {{ 5965, 1453696, 744276863 }, { 1890, -1453573, -744245376 }},
    {{ 5645, 1453698, 767536022 }, { 5666, -1453572, -767502534 }},
    {{ 5325, 1453700, 790795212 }, { 5345, -1453570, -790759664 }},
    {{ 5005, 1453702, 814054432 }, { 5025, -1453568, -814016764 }},
    {{ 4685, 1453704, 837313684 }, { 4704, -1453566, -837273832 }},
    {{ 4365, 1453706, 860572966 }, { 4383, -1453564, -860530870 }},
    {{ 4045, 1453708, 883832279 }, { 4063, -1453562, -883787877 }},
    {{ 3726, 1453710, 907091622 }, { 3742, -1453560, -907044853 }},
    {{ 3406, 1453712, 930350997 }, { 3421, -1453558, -930301799 }},
    {{ 3086, 1453714, 953610402 }, { 3101, -1453556, -953558714 }},
    {{ 2766, 1453716, 976869838 }, { 2780, -1453554, -976815598 }},
    {{ 2446, 1453718, 1000129305 }, { 2459, -1453552, -1000072451 }},
    {{ 2126, 1453720, 1023388802 }, { 2139, -1453550, -1023329274 }},
    {{ 5903, 1453721, 1046648332 }, { 5914, -1453549, -1046586063 }},
    {{ 5583, 1453723, 1069907891 }, { 5593, -1453547, -1069842824 }},
    {{ 5263, 1453725, 1093167481 }, { 5272, -1453545, -1093099555 }},
    {{ 4943, 1453727, 1116427101 }, { 4952, -1453543, -1116356254 }},
    {{ 4624, 1453729, 1139686752 }, { 4631, -1453541, -1139612923 }},
    {{ 4304, 1453731, 1162946434 }, { 4310, -1453539, -1162869561 }},
    {{ 3984, 1453733, 1186206147 }, { 3989, -1453537, -1186126169 }},
    {{ 3664, 1453735, 1209465890 }, { 3668, -1453535, -1209382745 }},
    {{ 3345, 1453737, 1232725664 }, { 3348, -1453533, -1232639291 }},
    {{ 3025, 1453739, 1255985469 }, { 3027, -1453531, -1255895806 }},
    {{ 2705, 1453741, 1279245305 }, { 2706, -1453529, -1279152290 }},
    {{ 2386, 1453743, 1302505171 }, { 2385, -1453527, -1302408744 }},
    {{ 2066, 1453745, 1325765068 }, { 2064, -1453525, -1325665167 }},
    {{ 5842, 1453746, 1349024998 }, { 5839, -1453524, -1348921557 }},
    {{ 5523, 1453748, 1372284957 }, { 5519, -1453522, -1372177918 }},
    {{ 5203, 1453750, 1395544946 }, { 5198, -1453520, -1395434249 }},
    {{ 4883, 1453752, 1418804967 }, { 4877, -1453518, -1418690549 }},
    {{ 4564, 1453754, 1442065018 }, { 4556, -1453516, -1441946818 }},
    {{ 4244, 1453756, 1465325099 }, { 4235, -1453514, -1465203056 }},
    {{ 3930, 1453758, 1488585212 }, { 3925, -1453512, -1488459264 }},
    {{ 3611, 1453762, 1535105528 }, { 3604, -1453508, -1534971587 }},
    {{ 3291, 1453766, 1581625968 }, { 3283, -1453504, -1581483787 }},
    {{ 2972, 1453770, 1628146531 }, { 2962, -1453500, -1627995865 }},
    {{ 4700, 1453773, 1674667221 }, { 4689, -1453497, -1674507815 }},
    {{ 4381, 1453777, 1721188030 }, { 4368, -1453493, -1721019646 }},
    {{ 4061, 1453781, 1767708961 }, { 4047, -1453489, -1767531355 }},
    {{ 3742, 1453785, 1814230016 }, { 3726, -1453485, -1814042940 }},
    {{ 3422, 1453789, 1860751194 }, { 3405, -1453481, -1860554403 }},
    {{ 3103, 1453793, 1907272495 }, { 3084, -1453477, -1907065742 }},
    {{ 4832, 1453796, 1953793923 }, { 4810, -1453474, -1953576954 }},
    {{ 4512, 1453800, 2000315470 }, { 4489, -1453470, -2000088048 }},
    {{ 4193, 1453804, 2046837140 }, { 4168, -1453466, -2046599018 }},
    {{ 3874, 1453808, 2093358932 }, { 3847, -1453462, -2093109866 }},
    {{ 3554, 1453812, 2139880848 }, { 3526, -1453458, -2139620590 }},
    {{ 3235, 1453816, 2186402887 }, { 3204, -1453454, -2186131192 }},
    {{ 2916, 1453820, 2232925049 }, { 4931, -1453451, -2232641667 }},
    {{ 4645, 1453823, 2279447338 }, { 4610, -1453447, -2279152022 }},
    {{ 4325, 1453827, 2325969746 }, { 4289, -1453443, -2325662255 }},
    {{ 4006, 1453831, 2372492277 }, { 3967, -1453439, -2372172364 }},
    {{ 3687, 1453835, 2419014931 }, { 3646, -1453435, -2418682351 }},
    {{ 3368, 1453839, 2465537708 }, { 3325, -1453431, -2465192215 }},
    {{ 3049, 1453843, 2512060608 }, { 3003, -1453427, -2511701955 }},
    {{ 4778, 1453846, 2558583635 }, { 4730, -1453424, -2558211569 }},
    {{ 4458, 1453850, 2605106781 }, { 4408, -1453420, -2604721064 }},
    {{ 4139, 1453854, 2651630050 }, { 4087, -1453416, -2651230436 }},
    {{ 3820, 1453858, 2698153442 }, { 3765, -1453412, -2697739684 }},
    {{ 3501, 1453862, 2744676957 }, { 3444, -1453408, -2744248810 }},
    {{ 3182, 1453866, 2791200595 }, { 3122, -1453404, -2790757813 }},
    {{ 4911, 1453869, 2837724360 }, { 4849, -1453401, -2837266689 }},
    {{ 4592, 1453873, 2884248245 }, { 4527, -1453397, -2883775446 }},
    {{ 4273, 1453877, 2930772252 }, { 4206, -1453393, -2930284079 }},
    {{ 3946, 1453881, 2977296382 }, { 3910, -1453389, -2976792590 }},
    {{ 3627, 1453889, 3070345010 }, { 3588, -1453381, -3069809245 }},
    {{ 4332, 1453896, 3163394138 }, { 4290, -1453374, -3162825399 }},
    {{ 4013, 1453904, 3256443750 }, { 3969, -1453366, -3255841069 }},
    {{ 3694, 1453912, 3349493855 }, { 3647, -1453358, -3348856248 }},
    {{ 4400, 1453919, 3442544459 }, { 4349, -1453351, -3441870926 }},
    {{ 4081, 1453927, 3535595548 }, { 4027, -1453343, -3534885121 }},
    {{ 3762, 1453935, 3628647128 }, { 3705, -1453335, -3627898824 }},
    {{ 3444, 1453943, 3721699201 }, { 4408, -1453328, -3720912027 }},
    {{ 4149, 1453950, 3814751774 }, { 4086, -1453320, -3813924746 }},
    {{ 3831, 1453958, 3907804830 }, { 3764, -1453312, -3906936973 }},
    {{ 3512, 1453966, 4000858379 }, { 3441, -1453304, -3999948708 }},
    {{ 4218, 1453973, 4093912428 }, { 4143, -1453297, -4092959944 }},
    {{ 3899, 1453981, 4186966961 }, { 3821, -1453289, -4185970695 }},
    {{ 3581, 1453989, 4280021987 }, { 3499, -1453281, -4278980955 }},
    {{ 4287, 1453996, 4373077512 }, { 4201, -1453274, -4371990715 }},
    {{ 3968, 1454004, 4466133521 }, { 3879, -1453266, -4464999991 }},
    {{ 3650, 1454012, 4559190023 }, { 3556, -1453258, -4558008775 }},
    {{ 4356, 1454019, 4652247025 }, { 4258, -1453251, -4651017059 }},
    {{ 4038, 1454027, 4745304510 }, { 3936, -1453243, -4744024859 }},
    {{ 3720, 1454035, 4838362488 }, { 3613, -1453235, -4837032168 }},
    {{ 4426, 1454042, 4931420966 }, { 4315, -1453228, -4930038977 }},
    {{ 4108, 1454050, 5024479928 }, { 3992, -1453220, -5023045302 }},
    {{ 3790, 1454058, 5117539383 }, { 3670, -1453212, -5116051135 }},
    {{ 3472, 1454066, 5210599329 }, { 4371, -1453205, -5209056468 }},
    {{ 4178, 1454073, 5303659775 }, { 4048, -1453197, -5302061317 }},
    {{ 3860, 1454081, 5396720706 }, { 3726, -1453189, -5395065675 }},
    {{ 3542, 1454089, 5489782129 }, { 4427, -1453182, -5488069532 }},
    {{ 4249, 1454096, 5582844052 }, { 4104, -1453174, -5581072906 }},
    {{ 3931, 1454104, 5675906459 }, { 3782, -1453166, -5674075788 }},
    {{ 3613, 1454112, 5768969358 }, { 3459, -1453158, -5767078178 }},
    {{ 4319, 1454119, 5862032757 }, { 4160, -1453151, -5860080069 }},
    {{ 3970, 1454127, 5955096641 }, { 3886, -1453143, -5953081476 }},
    {{ 4164, 1454142, 6141225896 }, { 4075, -1453128, -6139082803 }},
    {{ 3847, 1454158, 6327357103 }, { 3752, -1453112, -6325082178 }},
    {{ 4041, 1454173, 6513490295 }, { 3940, -1453097, -6511079570 }},
    {{ 3724, 1454189, 6699625439 }, { 4129, -1453082, -6697074995 }},
    {{ 3919, 1454204, 6885762568 }, { 3806, -1453066, -6883068469 }},
    {{ 4114, 1454219, 7071901666 }, { 3994, -1453051, -7069059960 }},
    {{ 3797, 1454235, 7258042716 }, { 4183, -1453036, -7255049483 }},
    {{ 3992, 1454250, 7444185752 }, { 3859, -1453020, -7441037055 }},
    {{ 4187, 1454265, 7630330756 }, { 4048, -1453005, -7627022645 }},
    {{ 3870, 1454281, 7816477713 }, { 3724, -1452989, -7813006283 }},
    {{ 4066, 1454296, 8002626654 }, { 3912, -1452974, -7998987938 }},
    {{ 3749, 1454312, 8188777549 }, { 4100, -1452959, -8184967626 }},
    {{ 3944, 1454327, 8374930428 }, { 3776, -1452943, -8370945363 }},
    {{ 4140, 1454342, 8561085276 }, { 3964, -1452928, -8556921117 }},
    {{ 3824, 1454358, 8747242077 }, { 4152, -1452913, -8742894904 }},
    {{ 4019, 1454373, 8933400863 }, { 3828, -1452897, -8928866741 }},
    {{ 3703, 1454389, 9119561602 }, { 4015, -1452882, -9114836594 }},
    {{ 3899, 1454404, 9305724326 }, { 3691, -1452866, -9300804496 }},
    {{ 4095, 1454419, 9491889019 }, { 3878, -1452851, -9486770416 }},
    {{ 3779, 1454435, 9678055665 }, { 4066, -1452836, -9672734368 }},
    {{ 3975, 1454450, 9864224296 }, { 3741, -1452820, -9858696370 }},
    {{ 4172, 1454465, 10050394896 }, { 3928, -1452805, -10044656389 }},
    {{ 3856, 1454481, 10236567449 }, { 4116, -1452790, -10230614441 }},
    {{ 4052, 1454496, 10422741987 }, { 3791, -1452774, -10416570542 }},
    {{ 3737, 1454512, 10608918478 }, { 3978, -1452759, -10602524661 }},
    {{ 3933, 1454527, 10795096955 }, { 4165, -1452744, -10788476812 }},
    {{ 4130, 1454542, 10981277400 }, { 3839, -1452728, -10974427013 }},
    {{ 3815, 1454558, 11167459799 }, { 4026, -1452713, -11160375232 }},
    {{ 4012, 1454573, 11353644183 }, { 3701, -1452697, -11346321499 }},
    {{ 3696, 1454589, 11539830520 }, { 3887, -1452682, -11532265784 }},
    {{ 3893, 1454604, 11726018842 }, { 4074, -1452667, -11718208103 }},
    {{ 4015, 1454619, 11912209138 }, { 3841, -1452651, -11904148476 }},
    {{ 3956, 1454650, 12284595609 }, { 4027, -1452621, -12276023268 }},
    {{ 3897, 1454681, 12656989958 }, { 3957, -1452590, -12647890226 }},
    {{ 3839, 1454712, 13029392184 }, { 3887, -1452559, -13019749318 }},
    {{ 4037, 1454742, 13401802319 }, { 3817, -1452528, -13391600543 }},
    {{ 3978, 1454773, 13774220301 }, { 4002, -1452498, -13763443871 }},
    {{ 3920, 1454804, 14146646160 }, { 3932, -1452467, -14135279366 }},
    {{ 3863, 1454835, 14519079897 }, { 3861, -1452436, -14507106995 }},
    {{ 4061, 1454865, 14891521545 }, { 4046, -1452406, -14878926727 }},
    {{ 4004, 1454896, 15263971038 }, { 3975, -1452375, -15250738626 }},
    {{ 3946, 1454927, 15636428411 }, { 3904, -1452344, -15622542660 }},
    {{ 3889, 1454958, 16008893663 }, { 3832, -1452313, -15994338829 }},
    {{ 3832, 1454989, 16381366793 }, { 4017, -1452283, -16366127102 }},
    {{ 4032, 1455019, 16753847834 }, { 3945, -1452252, -16737907542 }},
    {{ 3975, 1455050, 17126336723 }, { 3873, -1452221, -17109680118 }},
    {{ 3919, 1455081, 17498833491 }, { 4057, -1452191, -17481444798 }},
    {{ 3862, 1455112, 17871338138 }, { 3985, -1452160, -17853201646 }},
    {{ 4062, 1455142, 18243850697 }, { 3912, -1452129, -18224950630 }},
    {{ 4006, 1455173, 18616371105 }, { 3840, -1452098, -18596691751 }},
    {{ 3951, 1455204, 18988899392 }, { 4023, -1452068, -18968424976 }},
    {{ 3895, 1455235, 19361435560 }, { 3950, -1452037, -19340150370 }},
    {{ 3840, 1455266, 19733979608 }, { 3877, -1452006, -19711867901 }},
    {{ 4040, 1455296, 20106531569 }, { 3804, -1451975, -20083577569 }},
    {{ 3985, 1455327, 20479091378 }, { 3986, -1451945, -20455279343 }},
    {{ 3930, 1455358, 20851659069 }, { 3913, -1451914, -20826973286 }},
    {{ 3876, 1455389, 21224234640 }, { 3839, -1451883, -21198659366 }},
    {{ 3821, 1455420, 21596818093 }, { 4021, -1451853, -21570337552 }},
    {{ 4023, 1455450, 21969409458 }, { 3947, -1451822, -21942007908 }},
    {{ 3968, 1455481, 22342008674 }, { 3873, -1451791, -22313670403 }},
    {{ 3914, 1455512, 22714615772 }, { 4054, -1451761, -22685325003 }},
    {{ 3860, 1455543, 23087230751 }, { 3980, -1451730, -23056971774 }},
    {{ 4062, 1455573, 23459853645 }, { 3905, -1451699, -23428610683 }},
    {{ 3975, 1455604, 23832484397 }, { 3880, -1451668, -23800241743 }},
    {{ 3922, 1455666, 24577769519 }, { 3933, -1451607, -24543480212 }},
    {{ 3997, 1455727, 25323086235 }, { 3986, -1451546, -25286687239 }},
    {{ 3944, 1455789, 26068434421 }, { 3910, -1451484, -26029862886 }},
    {{ 3892, 1455851, 26813814140 }, { 3961, -1451423, -26773007029 }},
    {{ 3968, 1455912, 27559225458 }, { 3885, -1451361, -27516119798 }},
    {{ 3917, 1455974, 28304668248 }, { 3936, -1451300, -28259201066 }},
    {{ 3993, 1456035, 29050142641 }, { 3987, -1451239, -29002250897 }},
    {{ 3942, 1456097, 29795648509 }, { 3909, -1451177, -29745269356 }},
    {{ 3892, 1456159, 30541185916 }, { 3959, -1451116, -30488256319 }},
    {{ 3969, 1456220, 31286754930 }, { 3881, -1451054, -31231211913 }},
    {{ 3920, 1456282, 32032355423 }, { 3930, -1450993, -31974136012 }},
    {{ 3998, 1456343, 32777987524 }, { 3979, -1450932, -32717028682 }},
    {{ 3949, 1456405, 33523651107 }, { 3900, -1450870, -33459889987 }},
    {{ 3900, 1456467, 34269346238 }, { 3949, -1450809, -34202719803 }},
    {{ 3979, 1456528, 35015072980 }, { 3869, -1450747, -34945518256 }},
    {{ 3931, 1456590, 35760831209 }, { 3917, -1450686, -35688285220 }},
    {{ 3883, 1456652, 36506620989 }, { 3964, -1450625, -36431020762 }},
    {{ 3963, 1456713, 37252442385 }, { 3883, -1450563, -37173724946 }},
    {{ 3916, 1456775, 37998295271 }, { 3930, -1450502, -37916397647 }},
    {{ 3997, 1456836, 38744179776 }, { 3976, -1450441, -38659038927 }},
    {{ 3951, 1456898, 39490095773 }, { 3894, -1450379, -39401648854 }},
    {{ 3904, 1456960, 40236043330 }, { 3940, -1450318, -40144227302 }},
    {{ 3986, 1457021, 40982022508 }, { 3986, -1450257, -40886774335 }},
    {{ 3941, 1457083, 41728033183 }, { 3903, -1450195, -41629290017 }},
    {{ 3896, 1457145, 42474075420 }, { 3948, -1450134, -42371774224 }},
    {{ 3979, 1457206, 43220149283 }, { 3864, -1450072, -43114227082 }},
    {{ 3934, 1457268, 43966254649 }, { 3908, -1450011, -43856648468 }},
    {{ 3890, 1457330, 44712391579 }, { 3952, -1449950, -44599038446 }},
    {{ 3974, 1457391, 45458560140 }, { 3867, -1449888, -45341397079 }},
    {{ 3930, 1457453, 46204760208 }, { 3911, -1449827, -46083724245 }},
    {{ 3887, 1457515, 46950991843 }, { 3953, -1449766, -46826020005 }},
    {{ 3959, 1457576, 47697255127 }, { 3896, -1449704, -47568284451 }},
    {{ 3917, 1457700, 49189876218 }, { 3938, -1449582, -49052718924 }},
    {{ 3940, 1457823, 50682623736 }, { 3915, -1449459, -50537027923 }},
    {{ 3963, 1457946, 52175497566 }, { 3956, -1449337, -52021211217 }},
    {{ 3922, 1458070, 53668497599 }, { 3932, -1449214, -53505269062 }},
    {{ 3947, 1458193, 55161624083 }, { 3907, -1449091, -54989201338 }},
    {{ 3972, 1458316, 56654876915 }, { 3945, -1448969, -56473007936 }},
    {{ 3934, 1458440, 58148255974 }, { 3919, -1448846, -57956689120 }},
    {{ 3960, 1458563, 59641761527 }, { 3892, -1448723, -59440244772 }},
    {{ 3923, 1458687, 61135393330 }, { 3929, -1448601, -60923674780 }},
    {{ 3951, 1458810, 62629151643 }, { 3900, -1448478, -62406979394 }},
    {{ 3979, 1458933, 64123036357 }, { 3936, -1448356, -63890158396 }},
    {{ 3945, 1459057, 65617047349 }, { 3906, -1448233, -65373212023 }},
    {{ 3974, 1459180, 67111184891 }, { 3940, -1448111, -66856140051 }},
    {{ 3941, 1459304, 68605448732 }, { 3909, -1447988, -68338942735 }},
    {{ 3972, 1459427, 70099839142 }, { 3942, -1447866, -69821619841 }},
    {{ 3940, 1459551, 71594355876 }, { 3909, -1447743, -71304171615 }},
    {{ 3972, 1459674, 73088999200 }, { 3941, -1447621, -72786597841 }},
    {{ 3941, 1459798, 74583768870 }, { 3907, -1447498, -74268898756 }},
    {{ 3975, 1459921, 76078665147 }, { 3937, -1447376, -75751074136 }},
    {{ 3945, 1460045, 77573687796 }, { 3902, -1447253, -77233124235 }},
    {{ 3981, 1460168, 79068837066 }, { 3931, -1447131, -78715048820 }},
    {{ 3952, 1460292, 80564112738 }, { 3895, -1447008, -80196848143 }},
    {{ 3925, 1460416, 82059514925 }, { 3922, -1446886, -81678521969 }},
    {{ 3962, 1460539, 83555043780 }, { 3948, -1446764, -83160070428 }},
    {{ 3936, 1460663, 85050699055 }, { 3910, -1446641, -84641493667 }},
    {{ 3974, 1460786, 86546481017 }, { 3935, -1446519, -86122791439 }},
    {{ 3949, 1460910, 88042389422 }, { 3896, -1446396, -87603964008 }},
    {{ 3925, 1461034, 89538424401 }, { 3920, -1446274, -89085011132 }},
    {{ 3966, 1461157, 91034586096 }, { 3943, -1446152, -90565932943 }},
    {{ 3943, 1461281, 92530874271 }, { 3902, -1446029, -92046729586 }},
    {{ 3985, 1461404, 94027289181 }, { 3923, -1445907, -93527400812 }},
    {{ 3960, 1461528, 95523830604 }, { 3929, -1445785, -95007946698 }},
    {{ 3972, 1461775, 98517293498 }, { 3917, -1445540, -97968662984 }},
    {{ 3953, 1462023, 101511262774 }, { 3904, -1445295, -100928878298 }},
    {{ 3968, 1462270, 104505738984 }, { 3921, -1445051, -103888592459 }},
    {{ 3952, 1462518, 107500721755 }, { 3905, -1444806, -106847806069 }},
    {{ 3969, 1462765, 110496211657 }, { 3920, -1444562, -109806518722 }},
    {{ 3955, 1463013, 113492208292 }, { 3901, -1444317, -112764730967 }},
    {{ 3975, 1463260, 116488712201 }, { 3913, -1444073, -115722442417 }},
    {{ 3964, 1463508, 119485723010 }, { 3924, -1443829, -118679653413 }},
    {{ 3955, 1463756, 122483241035 }, { 3901, -1443584, -121636364254 }},
    {{ 3979, 1464003, 125481266627 }, { 3909, -1443340, -124592574558 }},
    {{ 3972, 1464251, 128479799365 }, { 3916, -1443096, -127548284660 }},
    {{ 3967, 1464499, 131478839570 }, { 3921, -1442852, -130503494607 }},
    {{ 3963, 1464747, 134478387361 }, { 3893, -1442607, -133458204779 }},
    {{ 3960, 1464995, 137478442801 }, { 3896, -1442363, -136412414739 }},
    {{ 3958, 1465243, 140479005997 }, { 3897, -1442119, -139366124817 }},
    {{ 3958, 1465491, 143480076996 }, { 3897, -1441875, -142319335109 }},
    {{ 3960, 1465739, 146481655890 }, { 3896, -1441631, -145272045703 }},
    {{ 3962, 1465987, 149483742815 }, { 3893, -1441387, -148224256659 }},
    {{ 3966, 1466235, 152486337807 }, { 3889, -1441143, -151175968086 }},
    {{ 3972, 1466483, 155489440944 }, { 3916, -1440900, -154127179824 }},
    {{ 3978, 1466731, 158493052376 }, { 3910, -1440656, -157077892453 }},
    {{ 3986, 1466979, 161497172133 }, { 3902, -1440412, -160028105777 }},
    {{ 3964, 1467228, 164501800058 }, { 3893, -1440168, -162977819903 }},
    {{ 3974, 1467476, 167506936764 }, { 3914, -1439925, -165927034661 }},
    {{ 3986, 1467724, 170512582058 }, { 3902, -1439681, -168875750648 }},
    {{ 3968, 1467973, 173518735781 }, { 3889, -1439437, -171823967690 }},
    {{ 3982, 1468221, 176525398540 }, { 3907, -1439194, -174771685636 }},
    {{ 3967, 1468470, 179532569888 }, { 3891, -1438950, -177718905031 }},
    {{ 3984, 1468718, 182540250446 }, { 3906, -1438707, -180665625499 }},
    {{ 3971, 1468967, 185548439774 }, { 3887, -1438463, -183611847585 }},
    {{ 3991, 1469215, 188557138472 }, { 3900, -1438220, -186557570944 }},
    {{ 3980, 1469464, 191566346110 }, { 3886, -1437976, -189502796198 }},
    {{ 3988, 1469961, 197586289735 }, { 3894, -1437490, -195391750838 }},
    {{ 3983, 1470459, 203608270701 }, { 3884, -1437003, -201278713312 }},
    {{ 3980, 1470957, 209632290341 }, { 3887, -1436517, -207163683237 }},
    {{ 3981, 1471455, 215658349088 }, { 3887, -1436031, -213046661783 }},
    {{ 3984, 1471953, 221686447863 }, { 3885, -1435545, -218927649701 }},
    {{ 3989, 1472451, 227716587311 }, { 3880, -1435059, -224806647580 }},
    {{ 3982, 1472950, 233748767541 }, { 3889, -1434574, -230683655707 }},
    {{ 3993, 1473448, 239782990285 }, { 3879, -1434088, -236558675565 }},
    {{ 3990, 1473947, 245819255395 }, { 3882, -1433603, -242431706995 }},
    {{ 3991, 1474446, 251857563742 }, { 3882, -1433118, -248302751038 }},
    {{ 3994, 1474945, 257897916256 }, { 3880, -1432633, -254171808494 }},
    {{ 4000, 1475444, 263940313531 }, { 3876, -1432148, -260038880010 }},
    {{ 3992, 1475944, 269984755895 }, { 3869, -1431663, -265903966214 }},
    {{ 4004, 1476443, 276031244752 }, { 3875, -1431179, -271767067307 }},
    {{ 4001, 1476943, 282079780170 }, { 3878, -1430695, -277628184362 }},
    {{ 4002, 1477443, 288130363051 }, { 3879, -1430211, -283487318196 }},
    {{ 4006, 1477943, 294182994132 }, { 3877, -1429727, -289344469407 }},
    {{ 4012, 1478443, 300237674249 }, { 3873, -1429243, -295199638702 }},
    {{ 4004, 1478944, 306294403607 }, { 3866, -1428759, -301052826709 }},
    {{ 4000, 1479445, 312353183114 }, { 3872, -1428276, -306904033607 }},
    {{ 4014, 1479945, 318414014239 }, { 3860, -1427792, -312753261073 }},
    {{ 4015, 1480446, 324476896571 }, { 3861, -1427309, -318600508769 }},
    {{ 4003, 1480948, 330541830805 }, { 3860, -1426826, -324445777971 }},
    {{ 4009, 1481449, 336608818702 }, { 3856, -1426343, -330289069194 }},
    {{ 4018, 1481950, 342677860373 }, { 3865, -1425861, -336130382640 }},
    {{ 4014, 1482452, 348748956044 }, { 3856, -1425378, -341969720037 }},
    {{ 4012, 1482954, 354822107000 }, { 3860, -1424896, -347807081006 }},
    {{ 4014, 1483456, 360897313691 }, { 3862, -1424414, -353642466794 }},
    {{ 4018, 1483958, 366974577052 }, { 3861, -1423932, -359475877966 }},
    {{ 4024, 1484460, 373053897741 }, { 3857, -1423450, -365307315171 }},
    {{ 4018, 1484963, 379135275854 }, { 3851, -1422968, -371136779176 }},
    {{ 4027, 1485465, 385218713153 }, { 3854, -1422487, -376964269895 }},
    {{ 4028, 1486471, 397391765791 }, { 3848, -1421524, -388613337537 }},
    {{ 4027, 1487478, 409573060704 }, { 3844, -1420562, -400254521781 }},
    {{ 4032, 1488485, 421762604264 }, { 3844, -1419601, -411887829076 }},
    {{ 4034, 1489493, 433960401726 }, { 3838, -1418640, -423513264632 }},
    {{ 4041, 1490501, 446166459466 }, { 3835, -1417680, -435130833339 }},
    {{ 4038, 1491511, 458380780832 }, { 3835, -1416721, -446740540508 }},
    {{ 4040, 1492521, 470603373713 }, { 3830, -1415762, -458342392324 }},
    {{ 4048, 1493531, 482834243353 }, { 3827, -1414804, -469936392989 }},
    {{ 4045, 1494543, 495073393828 }, { 3827, -1413847, -481522547978 }},
    {{ 4048, 1495555, 507320832225 }, { 3823, -1412890, -493100863878 }},
    {{ 4056, 1496567, 519576564592 }, { 3820, -1411934, -504671344180 }},
    {{ 4054, 1497581, 531840594278 }, { 3821, -1410979, -516233995197 }},
    {{ 4057, 1498595, 544112929128 }, { 3817, -1410024, -527788822747 }},
    {{ 4058, 1499610, 556393573479 }, { 3815, -1409070, -539335830702 }},
    {{ 4064, 1500625, 568682534234 }, { 3808, -1408116, -550875025774 }},
    {{ 4068, 1501641, 580979815740 }, { 3813, -1407164, -562406411972 }},
    {{ 4069, 1502658, 593285424070 }, { 3803, -1406211, -573929995736 }},
    {{ 4076, 1503675, 605599365280 }, { 3805, -1405260, -585445781208 }},
    {{ 4072, 1504694, 617921643692 }, { 3802, -1404309, -596953774867 }},
    {{ 4082, 1505712, 630252267219 }, { 3801, -1403359, -608453980700 }},
    {{ 4081, 1506732, 642591239243 }, { 3796, -1402409, -619946405686 }},
    {{ 4086, 1507752, 654938566746 }, { 3793, -1401460, -631431053369 }},
    {{ 4088, 1508773, 667294255008 }, { 3793, -1400512, -642907929496 }},
    {{ 4088, 1509795, 679658309155 }, { 3788, -1399564, -654377040147 }},
    {{ 4093, 1510817, 692030736182 }, { 3786, -1398617, -665838389764 }},
    {{ 4096, 1511840, 704411540576 }, { 3786, -1397671, -677291983053 }},
    {{ 4097, 1512864, 716800727980 }, { 3782, -1396725, -688737827012 }},
    {{ 4103, 1513888, 729198305214 }, { 3780, -1395780, -700175925284 }},
    {{ 4106, 1514913, 741604277200 }, { 3774, -1394835, -711606284589 }},
    {{ 4107, 1515939, 754018649159 }, { 3770, -1393891, -723028908781 }},
    {{ 4106, 1516966, 766441426953 }, { 3769, -1392948, -734443803237 }},
    {{ 4112, 1517993, 778872616292 }, { 3765, -1392005, -745850974644 }},
    {{ 4119, 1520049, 803760256364 }, { 3761, -1390122, -768642164102 }},
    {{ 4125, 1522108, 828681611516 }, { 3754, -1388241, -791402519526 }},
    {{ 4131, 1524170, 853636727181 }, { 3750, -1386363, -814132083264 }},
    {{ 4136, 1526235, 878625649833 }, { 3747, -1384488, -836830894916 }},
    {{ 4140, 1528303, 903648425605 }, { 3742, -1382615, -859498998510 }},
    {{ 4147, 1530373, 928705101265 }, { 3735, -1380744, -882136436075 }},
    {{ 4154, 1532446, 953795720887 }, { 3730, -1378876, -904743247002 }},
    {{ 4156, 1534523, 978920329091 }, { 3727, -1377011, -927319472684 }},
    {{ 4165, 1536601, 1004078978012 }, { 3722, -1375148, -949865156706 }},
    {{ 4169, 1538683, 1029271707928 }, { 3714, -1373287, -972380337730 }},
    {{ 4173, 1540768, 1054498565610 }, { 3709, -1371429, -994865059339 }},
    {{ 4180, 1542855, 1079759600550 }, { 3705, -1369574, -1017319359453 }},
    {{ 4186, 1544945, 1105054857316 }, { 3700, -1367721, -1039743283866 }},
    {{ 4191, 1547038, 1130384382552 }, { 3696, -1365871, -1062136868980 }},
    {{ 4196, 1549134, 1155748220554 }, { 3691, -1364023, -1084500160606 }},
    {{ 4204, 1551232, 1181146420921 }, { 3687, -1362178, -1106833194522 }},
    {{ 4208, 1553334, 1206579026920 }, { 3682, -1360335, -1129136017023 }},
    {{ 4215, 1555438, 1232046088306 }, { 3674, -1358494, -1151408665212 }},
    {{ 4222, 1557545, 1257547649545 }, { 3669, -1356656, -1173651181933 }},
    {{ 4227, 1559655, 1283083759795 }, { 3666, -1354821, -1195863605998 }},
    {{ 4233, 1561768, 1308654462494 }, { 3660, -1352988, -1218045978158 }},
    {{ 4237, 1563884, 1334259807950 }, { 3657, -1351158, -1240198340621 }},
    {{ 4242, 1566003, 1359899838597 }, { 3652, -1349330, -1262320734208 }},
    {{ 4249, 1568124, 1385574608310 }, { 3645, -1347504, -1284413199559 }},
    {{ 4253, 1570249, 1411284155925 }, { 3640, -1345681, -1306475774855 }},
    {{ 4259, 1572376, 1437028537167 }, { 3638, -1343861, -1328508502896 }},
    {{ 4266, 1574506, 1462807791300 }, { 3633, -1342043, -1350511421238 }},
    {{ 4272, 1576639, 1488621970266 }, { 3627, -1340227, -1372484575042 }},
    {{ 4277, 1578775, 1514471120871 }, { 3622, -1338414, -1394427998219 }},
    {{ 4282, 1580914, 1540355288984 }, { 3616, -1336603, -1416341736737 }},
    {{ 4291, 1583055, 1566274523901 }, { 3613, -1334795, -1438225829226 }},
    {{ 4298, 1585199, 1592228874580 }, { 3604, -1332988, -1460080316014 }},
    {{ 4310, 1589497, 1644243096973 }, { 3595, -1329384, -1503700625909 }},
    {{ 4320, 1593807, 1696398352802 }, { 3584, -1325789, -1547202986889 }},
    {{ 4333, 1598128, 1748695004171 }, { 3574, -1322204, -1590587721660 }},
    {{ 4344, 1602461, 1801133457769 }, { 3565, -1318629, -1633855145797 }},
    {{ 4356, 1606806, 1853714073474 }, { 3556, -1315064, -1677005574189 }},
    {{ 4368, 1611162, 1906437264894 }, { 3546, -1311508, -1720039329717 }},
    {{ 4381, 1615530, 1959303393651 }, { 3537, -1307962, -1762956723518 }},
    {{ 4393, 1619910, 2012312860233 }, { 3527, -1304425, -1805758074795 }},
    {{ 4405, 1624302, 2065466049024 }, { 3518, -1300898, -1848443693525 }},
    {{ 4417, 1628706, 2118763349544 }, { 3507, -1297380, -1891013887639 }},
    {{ 4428, 1633122, 2172205157740 }, { 3498, -1293872, -1933468977113 }},
    {{ 4440, 1637550, 2225791855993 }, { 3489, -1290374, -1975809264687 }},
    {{ 4452, 1641990, 2279523843216 }, { 3480, -1286885, -2018035072472 }},
    {{ 4464, 1646442, 2333401512955 }, { 3470, -1283405, -2060146705474 }},
    {{ 4476, 1650906, 2387425261228 }, { 3461, -1279935, -2102144466637 }},
    {{ 4488, 1655382, 2441595484535 }, { 3452, -1276474, -2144028675647 }},
    {{ 4501, 1659870, 2495912575070 }, { 3441, -1273022, -2185799623742 }},
    {{ 4513, 1664370, 2550376941694 }, { 3432, -1269580, -2227457627387 }},
    {{ 4525, 1668883, 2604988965598 }, { 3423, -1266147, -2269002994019 }},
    {{ 4537, 1673408, 2659749060488 }, { 3415, -1262724, -2310436021828 }},
    {{ 4549, 1677945, 2714657630595 }, { 3404, -1259309, -2351757016023 }},
    {{ 4562, 1682494, 2769715069804 }, { 3395, -1255904, -2392966280914 }},
    {{ 4574, 1687056, 2824921782261 }, { 3386, -1252508, -2434064119997 }},
    {{ 4587, 1691630, 2880278175101 }, { 3378, -1249122, -2475050827725 }},
    {{ 4600, 1696216, 2935784660834 }, { 3368, -1245744, -2515926716159 }},
    {{ 4612, 1700815, 2991441635548 }, { 3360, -1242376, -2556692076553 }},
    {{ 4623, 1705427, 3047249513819 }, { 3349, -1239016, -2597347207421 }},
    {{ 4636, 1710051, 3103208693670 }, { 3341, -1235666, -2637892412824 }},
    {{ 4649, 1714687, 3159319605509 }, { 3332, -1232325, -2678327981959 }},
    {{ 4662, 1719336, 3215582642739 }, { 3323, -1228993, -2718654215441 }},
    {{ 4674, 1723998, 3271998223727 }, { 3315, -1225670, -2758871416974 }},
    {{ 4694, 1728668, 3328566780024 }, { 3301, -1222353, -2798979888206 }},
    {{ 4719, 1738055, 3442164393096 }, { 3283, -1215752, -2878871709551 }},
    {{ 4745, 1747492, 3556378843214 }, { 3265, -1209186, -2958332091494 }},
    {{ 4770, 1756981, 3671213466968 }, { 3248, -1202656, -3037363348059 }},
    {{ 4796, 1766521, 3786671627960 }, { 3230, -1196161, -3115967765843 }},
    {{ 4822, 1776113, 3902756713494 }, { 3213, -1189701, -3194147705791 }},
    {{ 4849, 1785757, 4019472113263 }, { 3196, -1183276, -3271905423294 }},
    {{ 4874, 1795454, 4136821297444 }, { 3178, -1176885, -3349243190843 }},
    {{ 4901, 1805203, 4254807645771 }, { 3160, -1170529, -3426163278951 }},
    {{ 4928, 1815005, 4373434655210 }, { 3144, -1164208, -3502667978218 }},
    {{ 4955, 1824860, 4492705808202 }, { 3126, -1157920, -3578759493010 }},
    {{ 4982, 1834769, 4612624578376 }, { 3110, -1151667, -3654440082042 }},
    {{ 5008, 1844732, 4733194518037 }, { 3093, -1145447, -3729711949839 }},
    {{ 5035, 1854749, 4854419122130 }, { 3076, -1139261, -3804577293805 }},
    {{ 5063, 1864820, 4976301953382 }, { 3060, -1133109, -3879038320866 }},
    {{ 5091, 1874945, 5098846626649 }, { 3043, -1126989, -3953097232771 }},
    {{ 5118, 1885126, 5222056696024 }, { 3027, -1120903, -4026756172381 }},
    {{ 5146, 1895362, 5345935773058 }, { 3010, -1114849, -4100017315605 }},
    {{ 5174, 1905654, 5470487494971 }, { 2994, -1108828, -4172882814441 }},
    {{ 5202, 1916001, 5595715548470 }, { 2978, -1102840, -4245354783707 }},
    {{ 5230, 1926405, 5721623559268 }, { 2962, -1096884, -4317435370822 }},
    {{ 5259, 1936865, 5848215232638 }, { 2946, -1090960, -4389126683814 }},
    {{ 5287, 1947382, 5975494305093 }, { 2930, -1085068, -4460430819324 }},
    {{ 5316, 1957956, 6103464472354 }, { 2914, -1079208, -4531349862671 }},
    {{ 5344, 1968588, 6232129523559 }, { 2898, -1073380, -4601885892776 }},
    {{ 5374, 1979277, 6361493181332 }, { 2883, -1067583, -4672041023439 }},
    {{ 5403, 1990024, 6491559301373 }, { 2868, -1061818, -4741817255430 }},
    {{ 5432, 2000830, 6622331656099 }, { 2851, -1056083, -4811216632053 }},
    {{ 5462, 2011694, 6753814089658 }, { 2837, -1050380, -4880241258627 }},
    {{ 5491, 2022618, 6886010454599 }, { 2821, -1044707, -4948893078100 }},
    {{ 5521, 2033600, 7018924658619 }, { 2806, -1039065, -5017174156227 }},
    {{ 5552, 2044642, 7152560541079 }, { 2791, -1033454, -5085086453836 }},
    {{ 5596, 2055728, 7286922241124 }, { 2768, -1027864, -5152632067612 }},
    {{ 5657, 2078113, 7557837986270 }, { 2738, -1016792, -5286630754215 }},
    {{ 5719, 2100742, 7831703751559 }, { 2709, -1005839, -5419186095650 }},
    {{ 5781, 2123618, 8108551768683 }, { 2680, -995004, -5550313521827 }},
    {{ 5844, 2146743, 8388414423373 }, { 2651, -984286, -5680028385441 }},
    {{ 5908, 2170119, 8671324616416 }, { 2622, -973683, -5808345958309 }},
    {{ 5972, 2193750, 8957315529871 }, { 2594, -963195, -5935281315632 }},
    {{ 6037, 2217639, 9246420605736 }, { 2566, -952819, -6060849367850 }},
    {{ 6103, 2241787, 9538673882319 }, { 2538, -942555, -6185064758837 }},
    {{ 6170, 2266198, 9834109561663 }, { 2511, -932402, -6307942135631 }},
    {{ 6236, 2290876, 10132762413252 }, { 2484, -922358, -6429495880146 }},
    {{ 6304, 2315822, 10434667295905 }, { 2457, -912423, -6549740166384 }},
    {{ 6373, 2341039, 10739859715258 }, { 2431, -902594, -6668689326603 }},
    {{ 6442, 2366532, 11048375468486 }, { 2404, -892871, -6786357024034 }},
    {{ 6513, 2392301, 11360250699653 }, { 2378, -883253, -6902757262477 }},
    {{ 6583, 2418352, 11675522129109 }, { 2353, -873739, -7017903675929 }},
    {{ 6655, 2444686, 11994226542889 }, { 2328, -864327, -7131809757841 }},
    {{ 6728, 2471307, 12316401391908 }, { 2303, -855017, -7244488769288 }},
    {{ 6801, 2498218, 12642084562529 }, { 2278, -845806, -7355954052969 }},
    {{ 6875, 2525422, 12971314176113 }, { 2253, -836695, -7466218559226 }},
    {{ 6950, 2552922, 13304128864289 }, { 2229, -827682, -7575295379972 }},
    {{ 7026, 2580721, 13640567681256 }, { 2205, -818767, -7683197142270 }},
    {{ 7102, 2608823, 13980670128262 }, { 2181, -809947, -7789936635633 }},
    {{ 7180, 2637231, 14324475948209 }, { 2158, -801222, -7895526409401 }},
    {{ 7258, 2665949, 14672025597165 }, { 2134, -792591, -7999978637941 }},
    {{ 7337, 2694979, 15023359848350 }, { 2112, -784054, -8103305826590 }},
    {{ 7416, 2724326, 15378519914374 }, { 2089, -775608, -8205519898229 }},
    {{ 7497, 2753992, 15737547328909 }, { 2066, -767253, -8306632898079 }},
    {{ 7579, 2783981, 16100484288498 }, { 2044, -758988, -8406656787511 }},
    {{ 7662, 2814296, 16467373411503 }, { 2022, -750812, -8505603202360 }},
    {{ 7745, 2844942, 16838257711956 }, { 2000, -742725, -8603483683577 }},
    {{ 7829, 2875921, 17213180724122 }, { 1979, -734724, -8700309955104 }}
};

const memory<log2_row, (1 << LOG2_23_TABLE_LG_SIZE)> log2_23_coefficient_table =
{
    {{ 45, 90906, 0 }, { 44, 89508, 192882779 }, { 84, 174987, 384276102 }, { 156, 334682, 762685164 }, { 273, 614085, 1502600169 }, { 429, 1045395, 2919289296 }, { 559, 1571356, 5530682603 }, { 337, 1121565, 10049579242 }, { 121, 447560, 13870251962 }, { 98, 391561, 15580261810 }},
    {{ 43, 87952, 6050363 }, { 42, 86599, 198886246 }, { 81, 169261, 396189253 }, { 150, 323579, 786144996 }, { 264, 593185, 1548119584 }, { 413, 1008172, 3005200343 }, { 536, 1511222, 5685108738 }, { 324, 1079761, 10178334450 }, { 116, 432158, 13925514644 }, { 94, 378142, 15631844149 }},
    {{ 43, 85000, 12099249 }, { 40, 83692, 204888259 }, { 78, 163540, 408096682 }, { 145, 312498, 809582645 }, { 254, 572362, 1593555546 }, { 397, 971205, 3090814632 }, { 513, 1451831, 5838578631 }, { 311, 1038389, 10306424234 }, { 112, 416825, 13980654408 }, { 91, 364779, 15683319386 }},
    {{ 41, 82049, 18146660 }, { 39, 80787, 210888819 }, { 76, 157825, 419998393 }, { 140, 301437, 832998152 }, { 245, 551615, 1638908375 }, { 382, 934493, 3176134224 }, { 491, 1393168, 5991104143 }, { 298, 997441, 10433855432 }, { 108, 401559, 14035671779 }, { 87, 351471, 15734687911 }},
    {{ 39, 79099, 24192595 }, { 38, 77883, 216887926 }, { 73, 152115, 431894392 }, { 135, 290397, 856391559 }, { 235, 530944, 1684178360 }, { 366, 898032, 3261161097 }, { 469, 1335221, 6142696728 }, { 285, 956911, 10560634780 }, { 104, 386361, 14090567294 }, { 84, 338218, 15785950221 }},
    {{ 38, 76151, 30237056 }, { 36, 74980, 222885582 }, { 70, 146411, 443784683 }, { 130, 279378, 879762907 }, { 226, 510348, 1729365817 }, { 351, 861821, 3345897295 }, { 447, 1277976, 6293367742 }, { 272, 916793, 10686768910 }, { 100, 371230, 14145341492 }, { 81, 325020, 15837106726 }},
    {{ 36, 73204, 36280044 }, { 35, 72079, 228881787 }, { 68, 140713, 455669273 }, { 125, 268380, 903112237 }, { 217, 489827, 1774471040 }, { 336, 825855, 3430344786 }, { 426, 1221420, 6443128410 }, { 260, 877079, 10812264491 }, {  95, 356166, 14199994877 }, { 77, 311876, 15888157831 }},
    {{ 35, 70259, 42321558 }, { 33, 69179, 234876541 }, { 64, 135019, 467548167 }, { 119, 257402, 926439590 }, { 207, 469380, 1819494322 }, { 320, 790133, 3514505497 }, { 405, 1165542, 6591989571 }, { 247, 837765, 10937127708 }, {  91, 341168, 14254528042 }, { 74, 298786, 15939104030 }},
    {{ 33, 67315, 48361600 }, { 32, 66280, 240869846 }, { 62, 129332, 479421370 }, { 114, 246445, 949745010 }, { 198, 449008, 1864435970 }, { 306, 754653, 3598381444 }, { 385, 1110329, 6739962057 }, { 235, 798844, 11061365129 }, {  87, 326235, 14308941487 }, { 70, 285750, 15989945693 }},
    {{ 32, 64373, 54400170 }, { 30, 63383, 246861702 }, { 59, 123649, 491288889 }, { 109, 235509, 973028537 }, { 189, 428709, 1909296277 }, { 291, 719412, 3681974473 }, { 365, 1055770, 6887056332 }, { 223, 760310, 11184982895 }, {  83, 311368, 14363235732 }, { 67, 272767, 16040683309 }},
    {{ 31, 61432, 60437270 }, { 30, 60488, 252852110 }, { 56, 117972, 503150728 }, { 104, 224593, 996290211 }, { 180, 408484, 1954075534 }, { 276, 684407, 3765286515 }, { 345, 1001852, 7033282763 }, { 212, 722158, 11307987258 }, {  79, 296566, 14417411300 }, { 64, 259837, 16091317271 }},
    {{ 29, 58492, 66472899 }, { 27, 57593, 258841070 }, { 54, 112301, 515006893 }, { 99, 213698, 1019530074 }, { 171, 388331, 1998774036 }, { 262, 649636, 3848319478 }, { 326, 948566, 7178651605 }, { 200, 684381, 11430384060 }, {  75, 281829, 14471468706 }, { 61, 246959, 16141848008 }},
    {{ 28, 55554, 72507059 }, { 27, 54701, 264828583 }, { 51, 106635, 526857389 }, { 94, 202823, 1042748167 }, { 162, 368251, 2043392072 }, { 247, 615097, 3931075171 }, { 307, 895898, 7323172792 }, { 189, 646975, 11552179500 }, {  71, 267155, 14525408473 }, { 57, 234134, 16192275902 }},
    {{ 26, 52617, 78539751 }, { 25, 51809, 270814651 }, { 48, 100974, 538702222 }, { 89, 191968, 1065944530 }, { 153, 348243, 2087929931 }, { 233, 580787, 4013555494 }, { 288, 843840, 7466856128 }, { 177, 609934, 11673379226 }, {  67, 252545, 14579231104 }, { 54, 221361, 16242601432 }},
    {{ 25, 49682, 84570974 }, { 24, 48919, 276799273 }, { 46, 95319, 550541397 }, { 84, 181134, 1089119204 }, { 144, 328307, 2132387901 }, { 219, 546705, 4095762243 }, { 270, 792380, 7609711386 }, { 166, 573253, 11793989257 }, {  63, 237999, 14632937105 }, { 51, 208640, 16292824984 }},
    {{ 23, 46748, 90600730 }, { 22, 46030, 282782450 }, { 43, 89669, 562374919 }, { 79, 170320, 1112272230 }, { 135, 308442, 2176766268 }, { 205, 512848, 4177697231 }, { 252, 741508, 7751747955 }, { 155, 536925, 11914015215 }, {  59, 223515, 14686526988 }, { 48, 195970, 16342946974 }},
    {{ 22, 43816, 96629019 }, { 21, 43143, 288764184 }, { 41, 84025, 574202794 }, { 74, 159526, 1135403647 }, { 126, 288648, 2221065317 }, { 191, 479213, 4259362251 }, { 234, 691214, 7892975155 }, { 145, 500948, 12033462810 }, {  55, 209094, 14740001248 }, { 45, 183352, 16392967805 }},
    {{ 21, 40885, 102655843 }, { 20, 40257, 294744474 }, { 38, 78386, 586025028 }, { 69, 148752, 1158513497 }, { 118, 268924, 2265285339 }, { 178, 445799, 4340759096 }, { 216, 641488, 8033402182 }, { 134, 465314, 12152337389 }, {  52, 194735, 14793360414 }, { 41, 170784, 16442887861 }},
    {{ 19, 37955, 108681202 }, { 18, 37373, 300723321 }, { 35, 72752, 597841625 }, { 63, 137998, 1181601818 }, { 109, 249271, 2309426599 }, { 164, 412604, 4421889471 }, { 199, 592321, 8173038127 }, { 124, 430020, 12270644648 }, {  48, 180437, 14846604925 }, { 38, 158266, 16492707607 }},
    {{ 18, 35027, 114705096 }, { 18, 34490, 306700727 }, { 32, 67124, 609652591 }, { 58, 127264, 1204668653 }, { 100, 229688, 2353489384 }, { 151, 379625, 4502755169 }, { 182, 543703, 8311891770 }, { 113, 395060, 12388389743 }, {  44, 166200, 14899735300 }, { 35, 145799, 16542427409 }},
    {{ 16, 32100, 120727526 }, { 15, 31608, 312676691 }, { 30, 61501, 621457932 }, { 53, 116550, 1227714040 }, { 92, 210174, 2397473983 }, { 138, 346859, 4583357892 }, { 166, 495626, 8449971914 }, { 103, 360430, 12505578212 }, {  40, 152025, 14952752022 }, { 32, 133381, 16592047674 }},
    {{ 15, 29175, 126748493 }, { 15, 28728, 318651215 }, { 27, 55883, 633257652 }, { 49, 105857, 1250738020 }, { 83, 190729, 2441380656 }, { 124, 314306, 4663699307 }, { 149, 448079, 8587286955 }, {  93, 326126, 12622215186 }, {  36, 137910, 15005655582 }, { 29, 121013, 16641568797 }},
    {{ 13, 26251, 132767998 }, { 13, 25849, 324624299 }, { 24, 50271, 645051757 }, { 43, 95182, 1273740631 }, { 74, 171353, 2485209686 }, { 111, 281963, 4743781160 }, { 133, 401056, 8723845492 }, {  83, 292141, 12738305850 }, {  33, 123855, 15058446490 }, { 26, 108695, 16690991170 }},
    {{ 12, 23329, 138786042 }, { 11, 22971, 330595944 }, { 21, 44664, 656840253 }, { 39, 84528, 1296721915 }, { 66, 152045, 2528961355 }, { 99, 249828, 4823605123 }, { 118, 354546, 8859655845 }, {  73, 258474, 12853855267 }, {  29, 109859, 15111125168 }, { 23, 96425, 16740315193 }},
    {{ 11, 20408, 144802625 }, { 10, 20095, 336566150 }, { 19, 39063, 668623145 }, { 34, 73894, 1319681909 }, { 58, 132805, 2572635929 }, { 86, 217899, 4903172798 }, { 102, 308542, 8994725944 }, {  64, 225117, 12968868591 }, {  25, 95923, 15163692120 }, { 20, 84204, 16789541252 }},
    {{ 9, 17488, 150817747 }, { 8, 17220, 342534919 }, { 17, 33467, 680400438 }, { 29, 63279, 1342620654 }, { 49, 113633, 2616233670 }, { 73, 186174, 4982485854 }, { 87, 263035, 9129064039 }, {  54, 192068, 13083350552 }, {  22, 82046, 15216147844 }, { 17, 72032, 16838669734 }},
    {{ 8, 14570, 156831410 }, { 8, 14347, 348502251 }, { 14, 27876, 692172137 }, { 24, 52684, 1365538188 }, { 41, 94528, 2659754863 }, { 61, 154651, 5061545942 }, { 72, 218017, 9262677913 }, {  45, 159323, 13197306237 }, {  18, 68228, 15268492754 }, { 14, 59907, 16887701032 }},
    {{ 6, 11653, 162843614 }, { 6, 11475, 354468146 }, { 11, 22290, 703938248 }, { 19, 42108, 1388434552 }, { 32, 75490, 2703199764 }, { 48, 123328, 5140354616 }, { 57, 173481, 9395575320 }, {  36, 126876, 13310740388 }, {  14, 54467, 15320727353 }, { 11, 47831, 16936635520 }},
    {{ 5, 8738, 168854361 }, { 4, 8604, 360432606 }, { 9, 16710, 715698776 }, { 14, 31552, 1411309784 }, { 24, 56518, 2746568653 }, { 36, 92203, 5218913523 }, { 42, 129418, 9527763931 }, {  27, 94725, 13423657730 }, {  11, 40765, 15372852123 }, { 8, 35802, 16985473588 }},
    {{ 4, 5824, 174863650 }, { 3, 5735, 366395631 }, { 6, 11135, 727453726 }, { 9, 21015, 1434163923 }, { 16, 37613, 2789861786 }, { 24, 61275, 5297224214 }, { 28, 85822, 9659251334 }, {  18, 62864, 13536062965 }, {   7, 27119, 15424867480 }, { 5, 23820, 17034215612 }},
    {{ 2, 2911, 180871482 }, { 2, 2867, 372357221 }, { 3, 5565, 739203103 }, { 5, 10498, 1456997009 }, { 8, 18773, 2833079432 }, { 12, 30541, 5375288256 }, { 14, 42685, 9790044851 }, {   9, 31290, 13647960710 }, {   4, 13531, 15476773926 }, { 2, 11885, 17082861970 }},
    {{ 0, 0, 186877858 }, { 0, 0, 378317378 }, { 0, 0, 750946914 }, { 0, 0, 1479809079 }, { 0, 0, 2876221848 }, { 0, 0, 5453107193 }, { 0, 0, 9920151759 }, {   0, 0, 13759355525 }, {   0, 0, 15528571866 }, { 0, 0, 17131413038 }}
};

const memory<log21p_row, (1 << LOG21P_TABLE_LG_SIZE)> log21p_coefficient_table =
{
    {{ -5744, 12102203, 0 }, { -6075, -12102203, 0 }},
    {{ -6298, 12102192, 12102197 }, { -5605, -12102215, -12102209 }},
    {{ -5829, 12102180, 24204383 }, { -6160, -12102226, -24204429 }},
    {{ -6383, 12102169, 36306557 }, { -5691, -12102238, -36306661 }},
    {{ -5914, 12102157, 48408720 }, { -6245, -12102249, -48408905 }},
    {{ -5444, 12102145, 60510872 }, { -5776, -12102261, -60511160 }},
    {{ -5999, 12102134, 72613011 }, { -6331, -12102272, -72613427 }},
    {{ -5529, 12102122, 84715139 }, { -5861, -12102284, -84715705 }},
    {{ -6083, 12102111, 96817256 }, { -6416, -12102295, -96817995 }},
    {{ -5614, 12102099, 108919361 }, { -5947, -12102307, -108920296 }},
    {{ -6168, 12102088, 121021455 }, { -5478, -12102319, -121022609 }},
    {{ -5698, 12102076, 133123537 }, { -6032, -12102330, -133124933 }},
    {{ -6253, 12102065, 145225607 }, { -5563, -12102342, -145227269 }},
    {{ -5783, 12102053, 157327666 }, { -6118, -12102353, -157329616 }},
    {{ -6337, 12102042, 169429713 }, { -5649, -12102365, -169431975 }},
    {{ -5867, 12102030, 181531749 }, { -6204, -12102376, -181534346 }},
    {{ -5398, 12102018, 193633773 }, { -5735, -12102388, -193636728 }},
    {{ -5952, 12102007, 205735786 }, { -6290, -12102399, -205739122 }},
    {{ -5482, 12101995, 217837787 }, { -5821, -12102411, -217841527 }},
    {{ -6036, 12101984, 229939777 }, { -6376, -12102422, -229943943 }},
    {{ -5566, 12101972, 242041755 }, { -5907, -12102434, -242046372 }},
    {{ -6120, 12101961, 254143721 }, { -5438, -12102446, -254148811 }},
    {{ -5650, 12101949, 266245677 }, { -5993, -12102457, -266251263 }},
    {{ -6204, 12101938, 278347620 }, { -5524, -12102469, -278353725 }},
    {{ -5734, 12101926, 290449552 }, { -6079, -12102480, -290456200 }},
    {{ -6288, 12101915, 302551472 }, { -5610, -12102492, -302558686 }},
    {{ -5818, 12101903, 314653381 }, { -6165, -12102503, -314661183 }},
    {{ -6372, 12101892, 326755278 }, { -5696, -12102515, -326763692 }},
    {{ -5902, 12101880, 338857164 }, { -6251, -12102526, -338866213 }},
    {{ -5432, 12101868, 350959039 }, { -5783, -12102538, -350968745 }},
    {{ -5986, 12101857, 363060901 }, { -6338, -12102549, -363071289 }},
    {{ -5516, 12101845, 375162752 }, { -5869, -12102561, -375173844 }},
    {{ -6070, 12101834, 387264592 }, { -5400, -12102573, -387276411 }},
    {{ -5600, 12101822, 399366420 }, { -5956, -12102584, -399378989 }},
    {{ -6153, 12101811, 411468237 }, { -5487, -12102596, -411481579 }},
    {{ -5683, 12101799, 423570042 }, { -6042, -12102607, -423584180 }},
    {{ -6237, 12101788, 435671835 }, { -5574, -12102619, -435686793 }},
    {{ -5767, 12101776, 447773617 }, { -6129, -12102630, -447789417 }},
    {{ -6320, 12101765, 459875387 }, { -5661, -12102642, -459892053 }},
    {{ -5850, 12101753, 471977146 }, { -6216, -12102653, -471994701 }},
    {{ -6404, 12101742, 484078893 }, { -5748, -12102665, -484097360 }},
    {{ -5933, 12101730, 496180629 }, { -6303, -12102676, -496200031 }},
    {{ -5463, 12101718, 508282353 }, { -5835, -12102688, -508302713 }},
    {{ -6016, 12101707, 520384066 }, { -6390, -12102699, -520405406 }},
    {{ -5546, 12101695, 532485767 }, { -5922, -12102711, -532508112 }},
    {{ -6100, 12101684, 544587457 }, { -5453, -12102723, -544610828 }},
    {{ -5629, 12101672, 556689135 }, { -6009, -12102734, -556713557 }},
    {{ -6183, 12101661, 568790801 }, { -5540, -12102746, -568816297 }},
    {{ -5712, 12101649, 580892456 }, { -6096, -12102757, -580919048 }},
    {{ -6266, 12101638, 592994100 }, { -5628, -12102769, -593021811 }},
    {{ -5795, 12101626, 605095732 }, { -6183, -12102780, -605124586 }},
    {{ -6348, 12101615, 617197352 }, { -5715, -12102792, -617227372 }},
    {{ -5878, 12101603, 629298961 }, { -6271, -12102803, -629330169 }},
    {{ -5407, 12101591, 641400558 }, { -5803, -12102815, -641432978 }},
    {{ -5960, 12101580, 653502144 }, { -6358, -12102826, -653535799 }},
    {{ -5490, 12101568, 665603718 }, { -5890, -12102838, -665638631 }},
    {{ -6043, 12101557, 677705281 }, { -5422, -12102850, -677741475 }},
    {{ -5572, 12101545, 689806832 }, { -5978, -12102861, -689844330 }},
    {{ -6126, 12101534, 701908371 }, { -5510, -12102873, -701947197 }},
    {{ -5655, 12101522, 714009899 }, { -6066, -12102884, -714050075 }},
    {{ -6208, 12101511, 726111416 }, { -5597, -12102896, -726152965 }},
    {{ -5737, 12101499, 738212921 }, { -6153, -12102907, -738255867 }},
    {{ -6290, 12101488, 750314414 }, { -5685, -12102919, -750358780 }},
    {{ -5820, 12101476, 762415896 }, { -6241, -12102930, -762461704 }},
    {{ -6141, 12101465, 774517366 }, { -5842, -12102942, -774564640 }},
    {{ -5670, 12101441, 798720272 }, { -5886, -12102965, -798770547 }},
    {{ -5711, 12101418, 822923132 }, { -5930, -12102988, -822976500 }},
    {{ -5752, 12101395, 847125946 }, { -5974, -12103011, -847182499 }},
    {{ -5793, 12101372, 871328713 }, { -6018, -12103034, -871388545 }},
    {{ -5833, 12101349, 895531435 }, { -6062, -12103057, -895594636 }},
    {{ -5874, 12101326, 919734110 }, { -6107, -12103080, -919800774 }},
    {{ -5915, 12101303, 943936739 }, { -6151, -12103103, -944006958 }},
    {{ -5956, 12101280, 968139322 }, { -5683, -12103127, -968213188 }},
    {{ -5997, 12101257, 992341859 }, { -5728, -12103150, -992419464 }},
    {{ -6037, 12101234, 1016544349 }, { -5772, -12103173, -1016625786 }},
    {{ -6078, 12101211, 1040746793 }, { -5817, -12103196, -1040832155 }},
    {{ -6118, 12101188, 1064949192 }, { -5861, -12103219, -1065038570 }},
    {{ -6159, 12101165, 1089151544 }, { -5906, -12103242, -1089245031 }},
    {{ -5687, 12101141, 1113353850 }, { -5950, -12103265, -1113451538 }},
    {{ -5728, 12101118, 1137556110 }, { -5995, -12103288, -1137658091 }},
    {{ -5768, 12101095, 1161758323 }, { -6040, -12103311, -1161864690 }},
    {{ -5809, 12101072, 1185960491 }, { -6085, -12103334, -1186071336 }},
    {{ -5849, 12101049, 1210162612 }, { -6129, -12103357, -1210278028 }},
    {{ -5889, 12101026, 1234364687 }, { -5662, -12103381, -1234484765 }},
    {{ -5929, 12101003, 1258566716 }, { -5707, -12103404, -1258691550 }},
    {{ -5970, 12100980, 1282768699 }, { -5752, -12103427, -1282898380 }},
    {{ -6010, 12100957, 1306970636 }, { -5797, -12103450, -1307105256 }},
    {{ -6050, 12100934, 1331172526 }, { -5842, -12103473, -1331312179 }},
    {{ -6090, 12100911, 1355374370 }, { -5887, -12103496, -1355519148 }},
    {{ -6130, 12100888, 1379576169 }, { -5932, -12103519, -1379726163 }},
    {{ -5658, 12100864, 1403777921 }, { -5978, -12103542, -1403933224 }},
    {{ -5698, 12100841, 1427979627 }, { -6023, -12103565, -1428140331 }},
    {{ -5738, 12100818, 1452181287 }, { -6068, -12103588, -1452347485 }},
    {{ -5777, 12100795, 1476382900 }, { -6113, -12103611, -1476554685 }},
    {{ -5817, 12100772, 1500584468 }, { -6159, -12103634, -1500761931 }},
    {{ -5857, 12100749, 1524785989 }, { -5692, -12103658, -1524969222 }},
    {{ -5902, 12100726, 1548987464 }, { -5824, -12103681, -1549176561 }},
    {{ -5942, 12100680, 1597390276 }, { -5870, -12103727, -1597591376 }},
    {{ -5981, 12100634, 1645792903 }, { -5915, -12103773, -1646006376 }},
    {{ -6021, 12100588, 1694195345 }, { -5961, -12103819, -1694421560 }},
    {{ -5804, 12100541, 1742597604 }, { -6007, -12103865, -1742836929 }},
    {{ -5843, 12100495, 1790999677 }, { -5797, -12103912, -1791252483 }},
    {{ -5882, 12100449, 1839401565 }, { -5843, -12103958, -1839668221 }},
    {{ -5921, 12100403, 1887803269 }, { -5889, -12104004, -1888084145 }},
    {{ -5960, 12100357, 1936204789 }, { -5935, -12104050, -1936500253 }},
    {{ -5999, 12100311, 1984606124 }, { -5981, -12104096, -1984916546 }},
    {{ -5782, 12100264, 2033007274 }, { -6028, -12104142, -2033333023 }},
    {{ -5821, 12100218, 2081408240 }, { -5818, -12104189, -2081749685 }},
    {{ -5859, 12100172, 2129809021 }, { -5864, -12104235, -2130166532 }},
    {{ -5898, 12100126, 2178209617 }, { -5911, -12104281, -2178583564 }},
    {{ -5936, 12100080, 2226610029 }, { -5958, -12104327, -2227000780 }},
    {{ -5974, 12100034, 2275010256 }, { -6005, -12104373, -2275418181 }},
    {{ -6013, 12099988, 2323410299 }, { -5795, -12104420, -2323835767 }},
    {{ -5795, 12099941, 2371810157 }, { -5842, -12104466, -2372253537 }},
    {{ -5833, 12099895, 2420209831 }, { -5890, -12104512, -2420671493 }},
    {{ -5871, 12099849, 2468609319 }, { -5937, -12104558, -2469089633 }},
    {{ -5909, 12099803, 2517008624 }, { -5984, -12104604, -2517507958 }},
    {{ -5946, 12099757, 2565407743 }, { -6031, -12104650, -2565926467 }},
    {{ -5984, 12099711, 2613806678 }, { -5823, -12104697, -2614345161 }},
    {{ -6022, 12099665, 2662205429 }, { -5870, -12104743, -2662764040 }},
    {{ -5803, 12099618, 2710603995 }, { -5918, -12104789, -2711183104 }},
    {{ -5841, 12099572, 2759002376 }, { -5966, -12104835, -2759602353 }},
    {{ -5878, 12099526, 2807400573 }, { -6013, -12104881, -2808021786 }},
    {{ -5915, 12099480, 2855798585 }, { -5805, -12104928, -2856441404 }},
    {{ -5952, 12099434, 2904196412 }, { -5853, -12104974, -2904861206 }},
    {{ -5989, 12099388, 2952594055 }, { -5901, -12105020, -2953281194 }},
    {{ -6026, 12099342, 3000991514 }, { -5949, -12105066, -3001701366 }},
    {{ -5807, 12099295, 3049388788 }, { -5998, -12105112, -3050121723 }},
    {{ -5875, 12099249, 3097785877 }, { -5851, -12105159, -3098542264 }},
    {{ -5912, 12099157, 3194579502 }, { -5900, -12105251, -3195383902 }},
    {{ -5948, 12099065, 3291372388 }, { -5948, -12105343, -3292226279 }},
    {{ -5857, 12098972, 3388164537 }, { -5869, -12105436, -3389069395 }},
    {{ -5893, 12098880, 3484955947 }, { -5918, -12105528, -3485913250 }},
    {{ -5928, 12098788, 3581746618 }, { -5967, -12105620, -3582757844 }},
    {{ -5964, 12098696, 3678536551 }, { -5889, -12105713, -3679603176 }},
    {{ -5872, 12098603, 3775325747 }, { -5938, -12105805, -3776449249 }},
    {{ -5907, 12098511, 3872114204 }, { -5860, -12105898, -3873296059 }},
    {{ -5942, 12098419, 3968901923 }, { -5910, -12105990, -3970143610 }},
    {{ -5849, 12098326, 4065688904 }, { -5960, -12106082, -4066991900 }},
    {{ -5884, 12098234, 4162475146 }, { -5883, -12106175, -4163840928 }},
    {{ -5919, 12098142, 4259260650 }, { -5933, -12106267, -4260690696 }},
    {{ -5953, 12098050, 4356045416 }, { -5856, -12106360, -4357541202 }},
    {{ -5859, 12097957, 4452829444 }, { -5907, -12106452, -4454392448 }},
    {{ -5893, 12097865, 4549612734 }, { -5958, -12106544, -4551244434 }},
    {{ -5927, 12097773, 4646395285 }, { -5881, -12106637, -4648097158 }},
    {{ -5961, 12097681, 4743177099 }, { -5932, -12106729, -4744950622 }},
    {{ -5867, 12097588, 4839958175 }, { -5856, -12106822, -4841804824 }},
    {{ -5900, 12097496, 4936738512 }, { -5908, -12106914, -4938659766 }},
    {{ -5933, 12097404, 5033518111 }, { -5960, -12107006, -5035515448 }},
    {{ -5966, 12097312, 5130296972 }, { -5884, -12107099, -5132371868 }},
    {{ -5871, 12097219, 5227075096 }, { -5936, -12107191, -5229229028 }},
    {{ -5904, 12097127, 5323852480 }, { -5861, -12107284, -5326086926 }},
    {{ -5936, 12097035, 5420629127 }, { -5913, -12107376, -5422945565 }},
    {{ -5840, 12096942, 5517405037 }, { -5966, -12107468, -5519804943 }},
    {{ -5873, 12096850, 5614180208 }, { -5891, -12107561, -5616665059 }},
    {{ -5904, 12096758, 5710954640 }, { -5944, -12107653, -5713525916 }},
    {{ -5936, 12096666, 5807728335 }, { -5870, -12107746, -5810387511 }},
    {{ -5840, 12096573, 5904501293 }, { -5923, -12107838, -5907249846 }},
    {{ -5871, 12096481, 6001273512 }, { -5977, -12107930, -6004112921 }},
    {{ -5902, 12096389, 6098044992 }, { -5903, -12108023, -6100976734 }},
    {{ -5918, 12096297, 6194815735 }, { -5936, -12108115, -6197841287 }},
    {{ -5885, 12096112, 6388355009 }, { -5927, -12108300, -6391572611 }},
    {{ -5915, 12095928, 6581891328 }, { -5917, -12108485, -6585306892 }},
    {{ -5881, 12095743, 6775424699 }, { -5909, -12108670, -6779044131 }},
    {{ -5911, 12095559, 6968955115 }, { -5900, -12108855, -6972784328 }},
    {{ -5876, 12095374, 7162482583 }, { -5892, -12109040, -7166527483 }},
    {{ -5905, 12095190, 7356007097 }, { -5884, -12109225, -7360273596 }},
    {{ -5933, 12095006, 7549528660 }, { -5941, -12109409, -7554022669 }},
    {{ -5898, 12094821, 7743047273 }, { -5934, -12109594, -7747774698 }},
    {{ -5925, 12094637, 7936562934 }, { -5927, -12109779, -7941529685 }},
    {{ -5889, 12094452, 8130075646 }, { -5921, -12109964, -8135287631 }},
    {{ -5916, 12094268, 8323585405 }, { -5915, -12110149, -8329048535 }},
    {{ -5879, 12094083, 8517092215 }, { -5910, -12110334, -8522812397 }},
    {{ -5905, 12093899, 8710596073 }, { -5904, -12110519, -8716579219 }},
    {{ -5931, 12093715, 8904096979 }, { -5899, -12110704, -8910348998 }},
    {{ -5893, 12093530, 9097594938 }, { -5895, -12110889, -9104121737 }},
    {{ -5918, 12093346, 9291089944 }, { -5891, -12111074, -9297897435 }},
    {{ -5879, 12093161, 9484582002 }, { -5887, -12111259, -9491676091 }},
    {{ -5903, 12092977, 9678071107 }, { -5948, -12111443, -9685457709 }},
    {{ -5927, 12092793, 9871557263 }, { -5945, -12111628, -9879242284 }},
    {{ -5887, 12092608, 10065040470 }, { -5942, -12111813, -10073029818 }},
    {{ -5911, 12092424, 10258520726 }, { -5940, -12111998, -10266820311 }},
    {{ -5870, 12092239, 10451998033 }, { -5938, -12112183, -10460613764 }},
    {{ -5893, 12092055, 10645472389 }, { -5936, -12112368, -10654410177 }},
    {{ -5915, 12091871, 10838943795 }, { -5935, -12112553, -10848209549 }},
    {{ -5873, 12091686, 11032412254 }, { -5934, -12112738, -11042011880 }},
    {{ -5895, 12091502, 11225877761 }, { -5934, -12112923, -11235817172 }},
    {{ -5916, 12091318, 11419340318 }, { -5934, -12113108, -11429625423 }},
    {{ -5873, 12091133, 11612799929 }, { -5934, -12113293, -11623436635 }},
    {{ -5894, 12090949, 11806256588 }, { -5934, -12113478, -11817250806 }},
    {{ -5914, 12090765, 11999710298 }, { -5935, -12113663, -12011067938 }},
    {{ -5870, 12090580, 12193161061 }, { -5937, -12113848, -12204888030 }},
    {{ -5893, 12090396, 12386608873 }, { -5930, -12114033, -12398711082 }},
    {{ -5912, 12090028, 12773495649 }, { -5932, -12114403, -12786366068 }},
    {{ -5898, 12089659, 13160370635 }, { -5935, -12114773, -13174032897 }},
    {{ -5884, 12089290, 13547233826 }, { -5907, -12115144, -13561711564 }},
    {{ -5900, 12088922, 13934085221 }, { -5912, -12115514, -13949402080 }},
    {{ -5884, 12088553, 14320924826 }, { -5917, -12115884, -14337104440 }},
    {{ -5899, 12088185, 14707752636 }, { -5923, -12116254, -14724818646 }},
    {{ -5882, 12087816, 15094568659 }, { -5930, -12116624, -15112544697 }},
    {{ -5896, 12087448, 15481372888 }, { -5937, -12116994, -15500282596 }},
    {{ -5909, 12087080, 15868165327 }, { -5913, -12117365, -15888032338 }},
    {{ -5889, 12086711, 16254945980 }, { -5922, -12117735, -16275793933 }},
    {{ -5900, 12086343, 16641714842 }, { -5932, -12118105, -16663567376 }},
    {{ -5879, 12085974, 17028471920 }, { -5910, -12118476, -17051352665 }},
    {{ -5890, 12085606, 17415217208 }, { -5921, -12118846, -17439149809 }},
    {{ -5899, 12085238, 17801950709 }, { -5933, -12119216, -17826958803 }},
    {{ -5908, 12084870, 18188672425 }, { -5914, -12119587, -18214779646 }},
    {{ -5884, 12084501, 18575382360 }, { -5927, -12119957, -18602612345 }},
    {{ -5891, 12084133, 18962080507 }, { -5941, -12120327, -18990456898 }},
    {{ -5898, 12083765, 19348766872 }, { -5924, -12120698, -19378313300 }},
    {{ -5903, 12083397, 19735441453 }, { -5939, -12121068, -19766181562 }},
    {{ -5877, 12083028, 20122104257 }, { -5923, -12121439, -20154061675 }},
    {{ -5881, 12082660, 20508755276 }, { -5940, -12121809, -20541953648 }},
    {{ -5885, 12082292, 20895394514 }, { -5926, -12122180, -20929857474 }},
    {{ -5888, 12081924, 21282021973 }, { -5944, -12122550, -21317773162 }},
    {{ -5890, 12081556, 21668637653 }, { -5931, -12122921, -21705700705 }},
    {{ -5891, 12081188, 22055241555 }, { -5919, -12123292, -22093640106 }},
    {{ -5892, 12080820, 22441833679 }, { -5939, -12123662, -22481591372 }},
    {{ -5892, 12080452, 22828414027 }, { -5929, -12124033, -22869554493 }},
    {{ -5892, 12080084, 23214982599 }, { -5919, -12124404, -23257529477 }},
    {{ -5891, 12079716, 23601539395 }, { -5941, -12124774, -23645516326 }},
    {{ -5888, 12079348, 23988084416 }, { -5933, -12125145, -24033515034 }},
    {{ -5886, 12078980, 24374617664 }, { -5925, -12125516, -24421525606 }},
    {{ -5884, 12078612, 24761139139 }, { -5925, -12125887, -24809548040 }},
    {{ -5879, 12077876, 25534146773 }, { -5936, -12126628, -25585628512 }},
    {{ -5889, 12077141, 26307107316 }, { -5931, -12127370, -26361756446 }},
    {{ -5881, 12076405, 27080020788 }, { -5928, -12128112, -27137931850 }},
    {{ -5888, 12075670, 27852887180 }, { -5943, -12128853, -27914154737 }},
    {{ -5877, 12074934, 28625706513 }, { -5943, -12129595, -28690425102 }},
    {{ -5881, 12074199, 29398478778 }, { -5945, -12130337, -29466742955 }},
    {{ -5884, 12073464, 30171203989 }, { -5932, -12131080, -30243108296 }},
    {{ -5885, 12072729, 30943882150 }, { -5936, -12131822, -31019521146 }},
    {{ -5884, 12071994, 31716513266 }, { -5942, -12132564, -31795981501 }},
    {{ -5883, 12071259, 32489097347 }, { -5934, -12133307, -32572489362 }},
    {{ -5879, 12070524, 33261634393 }, { -5942, -12134049, -33349044749 }},
    {{ -5875, 12069789, 34034124414 }, { -5937, -12134792, -34125647651 }},
    {{ -5885, 12069055, 34806567407 }, { -5949, -12135534, -34902298091 }},
    {{ -5877, 12068320, 35578963391 }, { -5946, -12136277, -35678996060 }},
    {{ -5868, 12067585, 36351312368 }, { -5944, -12137020, -36455741571 }},
    {{ -5874, 12066851, 37123614333 }, { -5945, -12137763, -37232534627 }},
    {{ -5878, 12066117, 37895869300 }, { -5946, -12138506, -38009375238 }},
    {{ -5881, 12065383, 38668077276 }, { -5949, -12139249, -38786263408 }},
    {{ -5866, 12064648, 39440238271 }, { -5954, -12139992, -39563199141 }},
    {{ -5866, 12063914, 40212352279 }, { -5944, -12140736, -40340182439 }},
    {{ -5864, 12063180, 40984419312 }, { -5951, -12141479, -41117213321 }},
    {{ -5877, 12062447, 41756439369 }, { -5944, -12142223, -41894291776 }},
    {{ -5873, 12061713, 42528412470 }, { -5954, -12142966, -42671417828 }},
    {{ -5867, 12060979, 43300338612 }, { -5950, -12143710, -43448591466 }},
    {{ -5876, 12060246, 44072217796 }, { -5947, -12144454, -44225812704 }},
    {{ -5867, 12059512, 44844050040 }, { -5946, -12145198, -45003081547 }},
    {{ -5873, 12058779, 45615835336 }, { -5946, -12145942, -45780398002 }},
    {{ -5862, 12058045, 46387573706 }, { -5947, -12146686, -46557762075 }},
    {{ -5865, 12057312, 47159265138 }, { -5951, -12147430, -47335173769 }},
    {{ -5866, 12056579, 47930909646 }, { -5955, -12148174, -48112633093 }},
    {{ -5866, 12055846, 48702507237 }, { -5961, -12148918, -48890140052 }},
    {{ -5864, 12055113, 49474057916 }, { -5954, -12149663, -49667694646 }},
    {{ -5860, 12053647, 51017018562 }, { -5957, -12151152, -51222946778 }},
    {{ -5861, 12052182, 52559791617 }, { -5962, -12152641, -52778389551 }},
    {{ -5859, 12050717, 54102377139 }, { -5962, -12154131, -54334022991 }},
    {{ -5854, 12049252, 55644775172 }, { -5965, -12155621, -55889847159 }},
    {{ -5855, 12047788, 57186985757 }, { -5963, -12157112, -57445862090 }},
    {{ -5853, 12046324, 58729008944 }, { -5964, -12158603, -59002067840 }},
    {{ -5855, 12044861, 60270844764 }, { -5968, -12160094, -60558464456 }},
    {{ -5847, 12043397, 61812493295 }, { -5967, -12161586, -62115051976 }},
    {{ -5853, 12041935, 63353954547 }, { -5969, -12163078, -63671830458 }},
    {{ -5847, 12040472, 64895228590 }, { -5974, -12164570, -65228799947 }},
    {{ -5846, 12039010, 66436315451 }, { -5973, -12166063, -66785960485 }},
    {{ -5843, 12037548, 67977215197 }, { -5976, -12167556, -68343312119 }},
    {{ -5845, 12036087, 69517927850 }, { -5973, -12169050, -69900854895 }},
    {{ -5844, 12034626, 71058453470 }, { -5974, -12170544, -71458588862 }},
    {{ -5840, 12033165, 72598792102 }, { -5977, -12172038, -73016514076 }},
    {{ -5841, 12031705, 74138943780 }, { -5983, -12173532, -74574630582 }},
    {{ -5839, 12030245, 75678908561 }, { -5984, -12175027, -76132938410 }},
    {{ -5835, 12028785, 77218686497 }, { -5981, -12176523, -77691437605 }},
    {{ -5836, 12027326, 78758277614 }, { -5988, -12178018, -79250128249 }},
    {{ -5834, 12025867, 80297681969 }, { -5990, -12179514, -80809010358 }},
    {{ -5829, 12024408, 81836899611 }, { -5987, -12181011, -82368083979 }},
    {{ -5829, 12022950, 83375930569 }, { -5987, -12182508, -83927349172 }},
    {{ -5827, 12021492, 84914774907 }, { -5990, -12184005, -85486805984 }},
    {{ -5829, 12020035, 86453432648 }, { -5995, -12185502, -87046454469 }},
    {{ -5829, 12018578, 87991903859 }, { -5996, -12187000, -88606294649 }},
    {{ -5826, 12017121, 89530188582 }, { -5992, -12188499, -90166326574 }},
    {{ -5828, 12015665, 91068286847 }, { -5998, -12189997, -91726550327 }},
    {{ -5819, 12014208, 92606198728 }, { -6000, -12191496, -93286965918 }},
    {{ -5824, 12012753, 94143924234 }, { -5997, -12192996, -94847573396 }},
    {{ -5818, 12011297, 95681463446 }, { -6004, -12194495, -96408372844 }},
    {{ -5816, 12009842, 97218816375 }, { -5999, -12195996, -97969364254 }},
    {{ -5819, 12008388, 98755983089 }, { -6004, -12197496, -99530547730 }},
    {{ -5813, 12005479, 101829758036 }, { -6007, -12200498, -102653490978 }},
    {{ -5810, 12002572, 104902788636 }, { -6009, -12203502, -105777202911 }},
    {{ -5809, 11999667, 107975075234 }, { -6012, -12206507, -108901683980 }},
    {{ -5806, 11996763, 111046618205 }, { -6017, -12209513, -112026934543 }},
    {{ -5802, 11993860, 114117417934 }, { -6020, -12212521, -115152954951 }},
    {{ -5801, 11990959, 117187474766 }, { -6021, -12215531, -118279745584 }},
    {{ -5797, 11988059, 120256789016 }, { -6024, -12218542, -121407306850 }},
    {{ -5792, 11985160, 123325361110 }, { -6028, -12221554, -124535639149 }},
    {{ -5790, 11982263, 126393191373 }, { -6031, -12224568, -127664742790 }},
    {{ -5790, 11979368, 129460280150 }, { -6032, -12227584, -130794618175 }},
    {{ -5784, 11976473, 132526627832 }, { -6039, -12230600, -133925265743 }},
    {{ -5785, 11973581, 135592234740 }, { -6039, -12233619, -137056685821 }},
    {{ -5780, 11970689, 138657101271 }, { -6042, -12236639, -140188878782 }},
    {{ -5777, 11967799, 141721227744 }, { -6046, -12239660, -143321845067 }},
    {{ -5773, 11964910, 144784614561 }, { -6049, -12242683, -146455584984 }},
    {{ -5771, 11962023, 147847262024 }, { -6053, -12245707, -149590098981 }},
    {{ -5768, 11959137, 150909170550 }, { -6056, -12248733, -152725387383 }},
    {{ -5767, 11956253, 153970340440 }, { -6056, -12251761, -155861450607 }},
    {{ -5765, 11953370, 157030772094 }, { -6059, -12254790, -158998289010 }},
    {{ -5761, 11950488, 160090465854 }, { -6063, -12257820, -162135903042 }},
    {{ -5760, 11947608, 163149422069 }, { -6066, -12260852, -165274292992 }},
    {{ -5757, 11944729, 166207641096 }, { -6070, -12263885, -168413459330 }},
    {{ -5752, 11941851, 169265123288 }, { -6073, -12266920, -171553402366 }},
    {{ -5750, 11938975, 172321869003 }, { -6077, -12269956, -174694122559 }},
    {{ -5746, 11936100, 175377878593 }, { -6080, -12272994, -177835620212 }},
    {{ -5745, 11933227, 178433152418 }, { -6080, -12276034, -180977895770 }},
    {{ -5743, 11930355, 181487690853 }, { -6087, -12279074, -184120949613 }},
    {{ -5738, 11927484, 184541494193 }, { -6087, -12282117, -187264782105 }},
    {{ -5737, 11924615, 187594562861 }, { -6090, -12285161, -190409393624 }},
    {{ -5733, 11921747, 190646897139 }, { -6094, -12288206, -193554784615 }},
    {{ -5729, 11918880, 193698497466 }, { -6097, -12291253, -196700955386 }},
    {{ -5726, 11916015, 196749364083 }, { -6102, -12294301, -199847906335 }},
    {{ -5721, 11910289, 202848897887 }, { -6108, -12300402, -206144150556 }},
    {{ -5715, 11904568, 208945501357 }, { -6113, -12306510, -212443520143 }},
    {{ -5709, 11898853, 215039177164 }, { -6120, -12312623, -218746018316 }},
    {{ -5703, 11893143, 221129928368 }, { -6126, -12318743, -225051648070 }},
    {{ -5698, 11887439, 227217757599 }, { -6131, -12324869, -231360412733 }},
    {{ -5694, 11881741, 233302667686 }, { -6139, -12331000, -237672315270 }},
    {{ -5687, 11876047, 239384661495 }, { -6145, -12337138, -243987358811 }},
    {{ -5683, 11870360, 245463741711 }, { -6151, -12343282, -250305546550 }},
    {{ -5676, 11864677, 251539911245 }, { -6157, -12349432, -256626881590 }},
    {{ -5670, 11859000, 257613172763 }, { -6162, -12355589, -262951366964 }},
    {{ -5666, 11853329, 263683529181 }, { -6169, -12361751, -269279006001 }},
    {{ -5661, 11847663, 269750983218 }, { -6174, -12367920, -275609801723 }},
    {{ -5654, 11842002, 275815537517 }, { -6181, -12374094, -281943757420 }},
    {{ -5649, 11836347, 281877195028 }, { -6187, -12380275, -288280876033 }},
    {{ -5644, 11830697, 287935958545 }, { -6194, -12386462, -294621160775 }},
    {{ -5639, 11825053, 293991830608 }, { -6200, -12392655, -300964614994 }},
    {{ -5634, 11819414, 300044814200 }, { -6205, -12398855, -307311241681 }},
    {{ -5628, 11813780, 306094911953 }, { -6211, -12405061, -313661043957 }},
    {{ -5623, 11808152, 312142126614 }, { -6219, -12411272, -320014025264 }},
    {{ -5617, 11802529, 318186460939 }, { -6225, -12417490, -326370188592 }},
    {{ -5611, 11796911, 324227917793 }, { -6230, -12423715, -332729537116 }},
    {{ -5606, 11791299, 330266499739 }, { -6237, -12429945, -339092074173 }},
    {{ -5601, 11785692, 336302209667 }, { -6243, -12436182, -345457802798 }},
    {{ -5597, 11780091, 342335050203 }, { -6250, -12442425, -351826726229 }},
    {{ -5592, 11774495, 348365024040 }, { -6257, -12448674, -358198847780 }},
    {{ -5586, 11768904, 354392133936 }, { -6262, -12454930, -364574170617 }},
    {{ -5580, 11763318, 360416382728 }, { -6268, -12461192, -370952697924 }},
    {{ -5576, 11757738, 366437773081 }, { -6275, -12467460, -377334432908 }},
    {{ -5570, 11752163, 372456307491 }, { -6280, -12473735, -383719378870 }},
    {{ -5565, 11746593, 378471989048 }, { -6286, -12480016, -390107538965 }},
    {{ -5558, 11741028, 384484820089 }, { -6293, -12486303, -396498916426 }},
    {{ -5552, 11735468, 390494803854 }, { -6303, -12492594, -402893514965 }},
    {{ -5541, 11724365, 402506238464 }, { -6316, -12505200, -415692386116 }},
    {{ -5531, 11713284, 414506314714 }, { -6329, -12517832, -428504178713 }},
    {{ -5520, 11702223, 426495054041 }, { -6341, -12530489, -441328919724 }},
    {{ -5510, 11691183, 438472478341 }, { -6354, -12543172, -454166634029 }},
    {{ -5499, 11680164, 450438607907 }, { -6367, -12555880, -467017349002 }},
    {{ -5489, 11669166, 462393464899 }, { -6380, -12568614, -479881090422 }},
    {{ -5478, 11658188, 474337070263 }, { -6393, -12581374, -492757884800 }},
    {{ -5468, 11647231, 486269445291 }, { -6406, -12594160, -505647758695 }},
    {{ -5458, 11636295, 498190610748 }, { -6419, -12606972, -518550738744 }},
    {{ -5448, 11625379, 510100588161 }, { -6432, -12619810, -531466851669 }},
    {{ -5438, 11614484, 521999397912 }, { -6446, -12632674, -544396123882 }},
    {{ -5428, 11603609, 533887061451 }, { -6458, -12645565, -557338582918 }},
    {{ -5417, 11592754, 545763599098 }, { -6471, -12658482, -570294255093 }},
    {{ -5407, 11581920, 557629032066 }, { -6485, -12671425, -583263167355 }},
    {{ -5397, 11571106, 569483381133 }, { -6498, -12684395, -596245347248 }},
    {{ -5387, 11560312, 581326666956 }, { -6511, -12697391, -609240822141 }},
    {{ -5377, 11549538, 593158910134 }, { -6525, -12710414, -622249618230 }},
    {{ -5367, 11538784, 604980131239 }, { -6538, -12723464, -635271763823 }},
    {{ -5357, 11528050, 616790350710 }, { -6552, -12736540, -648307286150 }},
    {{ -5346, 11517336, 628589588620 }, { -6565, -12749644, -661356212399 }},
    {{ -5337, 11506643, 640377865973 }, { -6579, -12762774, -674418570568 }},
    {{ -5328, 11495969, 652155203459 }, { -6592, -12775932, -687494388025 }},
    {{ -5317, 11485314, 663921620258 }, { -6606, -12789116, -700583692924 }},
    {{ -5308, 11474680, 675677137439 }, { -6620, -12802328, -713686512492 }},
    {{ -5298, 11464065, 687421774787 }, { -6634, -12815567, -726802875197 }},
    {{ -5288, 11453470, 699155552321 }, { -6647, -12828834, -739932808961 }},
    {{ -5278, 11442894, 710878490674 }, { -6661, -12842128, -753076341755 }},
    {{ -5269, 11432338, 722590609708 }, { -6674, -12855450, -766233502107 }},
    {{ -5259, 11421801, 734291928910 }, { -6689, -12868799, -779404317489 }},
    {{ -5249, 11411284, 745982468134 }, { -6703, -12882176, -792588817188 }},
    {{ -5240, 11400786, 757662248219 }, { -6717, -12895581, -805787029247 }},
    {{ -5225, 11390302, 769331288216 }, { -6738, -12909006, -818998982947 }},
    {{ -5206, 11369402, 792637227135 }, { -6766, -12935956, -845464226835 }},
    {{ -5187, 11348579, 815900441326 }, { -6794, -12963019, -871984779507 }},
    {{ -5168, 11327832, 839121087782 }, { -6822, -12990195, -898560873786 }},
    {{ -5149, 11307161, 862299321582 }, { -6851, -13017485, -925192740131 }},
    {{ -5130, 11286565, 885435298158 }, { -6880, -13044890, -951880614925 }},
    {{ -5112, 11266044, 908529172345 }, { -6909, -13072411, -978624733965 }},
    {{ -5093, 11245597, 931581094244 }, { -6939, -13100048, -1005425333824 }},
    {{ -5075, 11225225, 954591218421 }, { -6968, -13127803, -1032282655453 }},
    {{ -5056, 11204926, 977559694032 }, { -6998, -13155675, -1059196938365 }},
    {{ -5038, 11184701, 1000486673483 }, { -7028, -13183665, -1086168425783 }},
    {{ -5020, 11164548, 1023372306525 }, { -7057, -13211776, -1113197360804 }},
    {{ -5002, 11144468, 1046216740693 }, { -7088, -13240006, -1140283986298 }},
    {{ -4984, 11124460, 1069020124618 }, { -7118, -13268357, -1167428553200 }},
    {{ -4966, 11104524, 1091782605142 }, { -7149, -13296830, -1194631306144 }},
    {{ -4949, 11084659, 1114504331069 }, { -7180, -13325425, -1221892497736 }},
    {{ -4931, 11064865, 1137185444109 }, { -7210, -13354144, -1249212380007 }},
    {{ -4913, 11045141, 1159826091900 }, { -7242, -13382986, -1276591202633 }},
    {{ -4896, 11025488, 1182426418470 }, { -7273, -13411954, -1304029223874 }},
    {{ -4879, 11005905, 1204986566532 }, { -7305, -13441047, -1331526698372 }},
    {{ -4861, 10986391, 1227506677221 }, { -7337, -13470266, -1359083886130 }},
    {{ -4844, 10966946, 1249986895617 }, { -7369, -13499613, -1386701045621 }},
    {{ -4827, 10947570, 1272427360749 }, { -7401, -13529088, -1414378439333 }},
    {{ -4810, 10928262, 1294828213623 }, { -7434, -13558692, -1442116329035 }},
    {{ -4793, 10909022, 1317189593477 }, { -7466, -13588426, -1469914982456 }},
    {{ -4776, 10889850, 1339511638793 }, { -7499, -13618291, -1497774663255 }},
    {{ -4760, 10870745, 1361794490067 }, { -7532, -13648287, -1525695642553 }},
    {{ -4743, 10851707, 1384038280659 }, { -7565, -13678416, -1553678189530 }},
    {{ -4726, 10832735, 1406243150256 }, { -7599, -13708678, -1581722575129 }},
    {{ -4710, 10813830, 1428409234550 }, { -7633, -13739074, -1609829075457 }},
    {{ -4693, 10794990, 1450536667534 }, { -7667, -13769605, -1637997965798 }},
    {{ -4677, 10776217, 1472625584325 }, { -7701, -13800272, -1666229523681 }},
    {{ -4653, 10757490, 1494676125140 }, { -7753, -13831037, -1694524037584 }},
    {{ -4621, 10720267, 1538662581606 }, { -7823, -13893059, -1751303012362 }},
    {{ -4589, 10683301, 1582497095361 }, { -7894, -13955640, -1808337173605 }},
    {{ -4558, 10646588, 1626180722565 }, { -7965, -14018788, -1865628830698 }},
    {{ -4527, 10610128, 1669714485944 }, { -8038, -14082509, -1923180310237 }},
    {{ -4496, 10573916, 1713099414857 }, { -8112, -14146812, -1980993984772 }},
    {{ -4465, 10537950, 1756336523231 }, { -8187, -14211705, -2039072253648 }},
    {{ -4435, 10502228, 1799426819727 }, { -8262, -14277196, -2097417556451 }},
    {{ -4405, 10466748, 1842371291933 }, { -8339, -14343294, -2156032346072 }},
    {{ -4376, 10431507, 1885170930039 }, { -8417, -14410006, -2214919133446 }},
    {{ -4346, 10396502, 1927826695867 }, { -8496, -14477342, -2274080452011 }},
    {{ -4317, 10361731, 1970339566028 }, { -8576, -14545310, -2333518875274 }},
    {{ -4289, 10327192, 2012710493907 }, { -8658, -14613920, -2393237002432 }},
    {{ -4260, 10292883, 2054940410748 }, { -8740, -14683180, -2453237493371 }},
    {{ -4232, 10258800, 2097030268343 }, { -8824, -14753099, -2513523020328 }},
    {{ -4204, 10224943, 2138980982020 }, { -8909, -14823687, -2574096308684 }},
    {{ -4177, 10191308, 2180793478302 }, { -8995, -14894954, -2634960118169 }},
    {{ -4149, 10157894, 2222468650288 }, { -9082, -14966910, -2696117248766 }},
    {{ -4122, 10124699, 2264007408936 }, { -9171, -15039564, -2757570538394 }},
    {{ -4096, 10091720, 2305410646589 }, { -9260, -15112927, -2819322879562 }},
    {{ -4069, 10058954, 2346679233242 }, { -9352, -15187010, -2881377176730 }},
    {{ -4043, 10026401, 2387814050977 }, { -9444, -15261822, -2943736420930 }},
    {{ -4017, 9994058, 2428815959676 }, { -9538, -15337375, -3006403608689 }},
    {{ -3991, 9961923, 2469685815382 }, { -9634, -15413679, -3069381799146 }},
    {{ -3966, 9929994, 2510424472541 }, { -9730, -15490747, -3132674103836 }},
    {{ -3940, 9898269, 2551032757363 }, { -9829, -15568589, -3196283655294 }},
    {{ -3915, 9866746, 2591511515073 }, { -9928, -15647218, -3260213668101 }},
    {{ -3891, 9835423, 2631861569738 }, { -10030, -15726644, -3324467370693 }},
    {{ -3866, 9804298, 2672083723533 }, { -10133, -15806882, -3389048066684 }},
    {{ -3842, 9773370, 2712178797474 }, { -10237, -15887942, -3453959105466 }},
    {{ -3818, 9742636, 2752147586897 }, { -10343, -15969837, -3519203877893 }},
    {{ -3794, 9712095, 2791990882941 }, { -10451, -16052582, -3584785828746 }},
    {{ -3759, 9681693, 2831709515539 }, { -10616, -16135938, -3650708601326 }},
    {{ -3712, 9621559, 2910775670539 }, { -10842, -16305783, -3783590233457 }},
    {{ -3667, 9562167, 2989352285146 }, { -11075, -16479241, -3917878054619 }},
    {{ -3622, 9503504, 3067445321329 }, { -11316, -16656430, -4053602124377 }},
    {{ -3578, 9445557, 3145060724793 }, { -11564, -16837470, -4190793536069 }},
    {{ -3535, 9388312, 3222204307380 }, { -11821, -17022488, -4329484320860 }},
    {{ -3493, 9331757, 3298881768820 }, { -12087, -17211618, -4469707618077 }},
    {{ -3451, 9275879, 3375098678963 }, { -12362, -17404998, -4611497692890 }},
    {{ -3410, 9220666, 3450860569968 }, { -12646, -17602772, -4754889962130 }},
    {{ -3370, 9166106, 3526172842139 }, { -12940, -17805093, -4899921030693 }},
    {{ -3331, 9112189, 3601040797253 }, { -13244, -18012118, -5046628811906 }},
    {{ -3292, 9058902, 3675469621823 }, { -13560, -18224014, -5195052479425 }},
    {{ -3254, 9006234, 3749464469672 }, { -13886, -18440955, -5345232719021 }},
    {{ -3216, 8954176, 3823030337471 }, { -14225, -18663123, -5497211536589 }},
    {{ -3180, 8902716, 3896172229860 }, { -14576, -18890709, -5651032585842 }},
    {{ -3143, 8851844, 3968894925350 }, { -14941, -19123914, -5806741032488 }},
    {{ -3108, 8801550, 4041203279479 }, { -15319, -19362949, -5964383815693 }},
    {{ -3073, 8751824, 4113101939223 }, { -15712, -19608034, -6124009573905 }},
    {{ -3039, 8702657, 4184595546087 }, { -16120, -19859404, -6285668837055 }},
    {{ -3005, 8654039, 4255688609375 }, { -16544, -20117301, -6449414093500 }},
    {{ -2972, 8605962, 4326385619953 }, { -16985, -20381985, -6615299868250 }},
    {{ -2939, 8558416, 4396690939070 }, { -17444, -20653727, -6783382864070 }},
    {{ -2907, 8511392, 4466608914985 }, { -17922, -20932812, -6953722073312 }},
    {{ -2875, 8464882, 4536143763741 }, { -18420, -21219542, -7126378892969 }},
    {{ -2844, 8418878, 4605299683297 }, { -18939, -21514236, -7301417262933 }},
    {{ -2814, 8373371, 4674080787923 }, { -19480, -21817230, -7478903809383 }},
    {{ -2784, 8328353, 4742491091348 }, { -20045, -22128881, -7658907965000 }},
    {{ -2754, 8283817, 4810534563958 }, { -20634, -22449563, -7841502224933 }},
    {{ -2725, 8239755, 4878215152611 }, { -21250, -22779677, -8026762167889 }},
    {{ -2696, 8196158, 4945536683080 }, { -21894, -23119643, -8214766806616 }},
    {{ -2668, 8153021, 5012502963459 }, { -22568, -23469910, -8405598682259 }},
    {{ -2640, 8110336, 5079117700389 }, { -23273, -23830953, -8599344147698 }},
    {{ -2599, 8067974, 5145384693887 }, { -24397, -24199795, 8796094017856 }},
    {{ -2546, 7984803, 5276889281495 }, { 0, 0, 0 }},
    {{ -2495, 7903328, 5407045132171 }, { 0, 0, 0 }},
    {{ -2445, 7823499, 5535879521806 }, { 0, 0, 0 }},
    {{ -2396, 7745267, 5663419027744 }, { 0, 0, 0 }},
    {{ -2349, 7668584, 5789689515045 }, { 0, 0, 0 }},
    {{ -2304, 7593405, 5914715992411 }, { 0, 0, 0 }},
    {{ -2260, 7519685, 6038522668516 }, { 0, 0, 0 }},
    {{ -2217, 7447383, 6161133089047 }, { 0, 0, 0 }},
    {{ -2175, 7376458, 6282570158874 }, { 0, 0, 0 }},
    {{ -2134, 7306871, 6402856130781 }, { 0, 0, 0 }},
    {{ -2095, 7238584, 6522012752010 }, { 0, 0, 0 }},
    {{ -2056, 7171562, 6640060787308 }, { 0, 0, 0 }},
    {{ -2019, 7105770, 6757020916800 }, { 0, 0, 0 }},
    {{ -1982, 7041174, 6872912794526 }, { 0, 0, 0 }},
    {{ -1947, 6977742, 6987755961429 }, { 0, 0, 0 }},
    {{ -1913, 6915443, 7101569152747 }, { 0, 0, 0 }},
    {{ -1879, 6854246, 7214370545365 }, { 0, 0, 0 }},
    {{ -1846, 6794123, 7326178104754 }, { 0, 0, 0 }},
    {{ -1814, 6735045, 7437009186116 }, { 0, 0, 0 }},
    {{ -1783, 6676986, 7546880691101 }, { 0, 0, 0 }},
    {{ -1753, 6619919, 7655809103287 }, { 0, 0, 0 }},
    {{ -1724, 6563820, 7763810479701 }, { 0, 0, 0 }},
    {{ -1695, 6508663, 7870900361159 }, { 0, 0, 0 }},
    {{ -1667, 6454425, 7977094121730 }, { 0, 0, 0 }},
    {{ -1640, 6401084, 8082406632260 }, { 0, 0, 0 }},
    {{ -1613, 6348618, 8186852271930 }, { 0, 0, 0 }},
    {{ -1587, 6297004, 8290445339703 }, { 0, 0, 0 }},
    {{ -1562, 6246223, 8393199632333 }, { 0, 0, 0 }},
    {{ -1537, 6196255, 8495128497724 }, { 0, 0, 0 }},
    {{ -1513, 6147079, 8596245235232 }, { 0, 0, 0 }},
    {{ -1489, 6098678, 8696562543979 }, { 0, 0, 0 }}
};

const memory<sin_row, (1 << SIN_TABLE_LG_SIZE)> sin_coefficient_table =
{
    {{ 372, 0, 33554431 }, { 744, 0, 67108853 }, { 0, 0, 134217642 }, { 272, 2, 268434773 }, { 176, 8, 536865451 }, { 175, 32, 1073698134 }, { 263, 128, 2147134141 }, { 499, 512, 4292171649 }, { 969, 2045, 8567582434 }, { 1841, 8148, 17001470399 }, { 1607, 32090, 32945872069 }, { 1269, 70335, 46841858780 }, { 1239, 120504, 57825445266 }, { 16, 243600, 68547333329 }},
    {{ 364, 0, 34603007 }, { 727, 0, 69206004 }, { 991, 1, 138411939 }, { 206, 2, 276823315 }, { 300, 9, 553642142 }, { 167, 34, 1107248340 }, { 247, 136, 2214209201 }, { 467, 544, 4426119022 }, { 943, 2175, 8833857464 }, { 1775, 8662, 17521127142 }, { 1552, 33079, 33416010437 }, { 1224, 71737, 47233247779 }, { 1172, 123968, 58398508267 }, { 9, 247688, 68614916502 }},
    {{ 355, 0, 35651582 }, { 710, 0, 71303155 }, { 957, 1, 142606235 }, { 138, 2, 285211852 }, { 164, 9, 570418793 }, { 151, 36, 1140798281 }, { 215, 144, 2281282142 }, { 468, 578, 4560049568 }, { 902, 2308, 9099997581 }, { 1714, 9192, 18039714526 }, { 1490, 34081, 33884109034 }, { 1174, 73150, 47621753804 }, { 1104, 127465, 58957313693 }, { 6, 251780, 68665748401 }},
    {{ 346, 0, 36700158 }, { 693, 0, 73400306 }, { 922, 1, 146800529 }, { 68, 2, 293600385 }, { 280, 10, 587195418 }, { 127, 38, 1174347950 }, { 232, 153, 2348352930 }, { 437, 612, 4693962656 }, { 877, 2446, 9365998950 }, { 1658, 9738, 18557200937 }, { 1436, 35098, 34350139767 }, { 1128, 74575, 48007353328 }, { 1042, 130996, 59501725989 }, { 3, 255874, 68699816270 }},
    {{ 337, 0, 37748734 }, { 675, 0, 75497457 }, { 886, 1, 150994823 }, { 508, 3, 301988918 }, { 136, 10, 603971999 }, { 95, 40, 1207897337 }, { 232, 162, 2415421471 }, { 438, 648, 4827857900 }, { 836, 2587, 9631857338 }, { 1590, 10298, 19073554498 }, { 1384, 36129, 34814073881 }, { 1087, 76012, 48390022889 }, { 977, 134558, 60031611672 }, { 0, 259969, 68717111903 }},
    {{ 328, 0, 38797310 }, { 656, 0, 77594607 }, { 849, 1, 155189117 }, { 434, 3, 310377442 }, { 244, 11, 620748552 }, { 183, 43, 1241446451 }, { 216, 171, 2482487702 }, { 407, 684, 4961734667 }, { 812, 2733, 9897568867 }, { 1536, 10875, 19588744212 }, { 1325, 37173, 35275883139 }, { 1044, 77460, 48769738746 }, { 918, 138152, 60546841713 }, { 1, 264065, 68717631250 }},
    {{ 319, 0, 39845886 }, { 637, 0, 79691758 }, { 811, 1, 159383410 }, { 358, 3, 318765962 }, { 92, 11, 637525061 }, { 135, 45, 1274995253 }, { 184, 180, 2549551556 }, { 408, 722, 5095592568 }, { 773, 2882, 10163129234 }, { 1471, 11466, 20102738056 }, { 1269, 38231, 35735539302 }, { 998, 78919, 49146478116 }, { 860, 141776, 61047289719 }, { 6, 268161, 68701374352 }},
    {{ 309, 0, 40894462 }, { 618, 0, 81788909 }, { 772, 1, 163577702 }, { 280, 3, 327154476 }, { 192, 12, 654301538 }, { 207, 48, 1308543765 }, { 200, 190, 2616613003 }, { 377, 760, 5229430969 }, { 751, 3036, 10428534652 }, { 1412, 12073, 20615505069 }, { 1216, 39303, 36193014421 }, { 952, 80389, 49520217595 }, { 801, 145429, 61532834208 }, { 10, 272255, 68668345136 }},
    {{ 299, 0, 41943037 }, { 598, 0, 83886059 }, { 732, 1, 167771994 }, { 200, 3, 335542986 }, { 32, 12, 671077969 }, { 143, 50, 1342091948 }, { 200, 200, 2683671947 }, { 379, 800, 5363249478 }, { 713, 3193, 10693780843 }, { 1351, 12695, 21127013753 }, { 1167, 40389, 36648280497 }, { 905, 81870, 49890934711 }, { 747, 149111, 62003356292 }, { 19, 276347, 68618551299 }},
    {{ 289, 0, 42991613 }, { 577, 0, 85983209 }, { 691, 1, 171966285 }, { 118, 3, 343931491 }, { 124, 13, 687854368 }, { 199, 53, 1375639827 }, { 184, 210, 2750728324 }, { 348, 840, 5497047475 }, { 677, 3354, 10958863858 }, { 1288, 13332, 21637232935 }, { 1113, 41488, 37101309806 }, { 866, 83363, 50258607003 }, { 696, 152821, 62458741221 }, { 27, 280435, 68552005375 }},
    {{ 278, 0, 44040189 }, { 556, 0, 88080360 }, { 649, 1, 176160575 }, { 34, 3, 352319991 }, { 212, 14, 704630726 }, { 119, 55, 1409187360 }, { 153, 220, 2817782068 }, { 318, 881, 5630824498 }, { 657, 3520, 11223779763 }, { 1224, 13984, 22146131409 }, { 1056, 42600, 37552074484 }, { 821, 84866, 50623211409 }, { 644, 156557, 62898877599 }, { 35, 284518, 68468722998 }},
    {{ 267, 0, 45088765 }, { 535, 0, 90177510 }, { 606, 1, 180354865 }, { 460, 4, 360708489 }, { 40, 14, 721407033 }, { 159, 58, 1442734573 }, { 169, 231, 2884833150 }, { 319, 924, 5764580107 }, { 622, 3689, 11488524311 }, { 1167, 14652, 22653678419 }, { 1004, 43726, 38000547261 }, { 777, 86380, 50984726184 }, { 595, 160319, 63323658005 }, { 51, 288597, 68368725806 }},
    {{ 256, 0, 46137341 }, { 513, 0, 92274660 }, { 562, 1, 184549154 }, { 372, 4, 369096978 }, { 120, 15, 738183306 }, { 63, 60, 1476281424 }, { 169, 242, 2951881472 }, { 289, 967, 5898313660 }, { 587, 3862, 11753093589 }, { 1110, 15335, 23159842622 }, { 950, 44865, 38446700624 }, { 736, 87905, 51343129169 }, { 544, 164105, 63732979005 }, { 66, 292669, 68252037090 }},
    {{ 245, 0, 47185916 }, { 490, 0, 94371810 }, { 517, 1, 188743443 }, { 282, 4, 377485462 }, { 196, 16, 754959534 }, { 87, 63, 1509827939 }, { 153, 253, 3018926972 }, { 291, 1012, 6032024773 }, { 554, 4039, 12017483515 }, { 1044, 16032, 23664593223 }, { 901, 46018, 38890507669 }, { 690, 89440, 51698398248 }, { 499, 167916, 64126740646 }, { 82, 296733, 68118685276 }},
    {{ 234, 0, 48234492 }, { 467, 0, 96468960 }, { 471, 1, 192937730 }, { 190, 4, 385873940 }, { 268, 17, 771735717 }, { 103, 66, 1543374092 }, { 121, 264, 3085969585 }, { 261, 1057, 6165712820 }, { 522, 4220, 12281690075 }, { 985, 16745, 24167899600 }, { 844, 47183, 39331940627 }, { 649, 90986, 52050512043 }, { 456, 171750, 64504846747 }, { 101, 300789, 67968703520 }},
    {{ 222, 0, 49283068 }, { 444, 0, 98566110 }, { 424, 1, 197132017 }, { 96, 4, 394262412 }, { 80, 17, 788511846 }, { 111, 69, 1576919877 }, { 138, 276, 3153009277 }, { 263, 1104, 6299377404 }, { 491, 4405, 12545709239 }, { 928, 17473, 24669730840 }, { 794, 48362, 39770973170 }, { 612, 92543, 52399449014 }, { 411, 175605, 64867204334 }, { 120, 304835, 67802128090 }},
    {{ 210, 0, 50331643 }, { 420, 0, 100663260 }, { 376, 1, 201326304 }, { 0, 4, 402650878 }, { 144, 18, 805287936 }, { 111, 72, 1610465284 }, { 138, 288, 3220045956 }, { 233, 1151, 6433017900 }, { 461, 4594, 12809536979 }, { 871, 18216, 25170056427 }, { 744, 49554, 40207578292 }, { 572, 94110, 52745187892 }, { 371, 179482, 65213725938 }, { 143, 308871, 67618999483 }},
    {{ 198, 0, 51380219 }, { 395, 0, 102760409 }, { 327, 1, 205520589 }, { 414, 5, 411039342 }, { 204, 19, 822063978 }, { 103, 75, 1644010307 }, { 122, 300, 3287079556 }, { 204, 1199, 6566633854 }, { 432, 4787, 13073169269 }, { 807, 18973, 25668845616 }, { 686, 50758, 40641729258 }, { 531, 95687, 53087707295 }, { 332, 183379, 65544326478 }, { 169, 312896, 67419363191 }},
    {{ 185, 0, 52428795 }, { 370, 0, 104857559 }, { 277, 1, 209714874 }, { 314, 5, 419427796 }, { 260, 20, 838839971 }, { 87, 78, 1677554937 }, { 91, 312, 3354110012 }, { 206, 1249, 6700224821 }, { 404, 4984, 13336602100 }, { 752, 19746, 26166068401 }, { 637, 51976, 41073399824 }, { 489, 97274, 53426986474 }, { 298, 187296, 65858925875 }, { 195, 316908, 67203267010 }},
    {{ 172, 0, 53477371 }, { 345, 0, 106954709 }, { 226, 1, 213909158 }, { 212, 5, 427816244 }, { 56, 20, 855615905 }, { 63, 81, 1711099165 }, { 107, 325, 3421137294 }, { 177, 1299, 6833790167 }, { 362, 5184, 13599831299 }, { 692, 20533, 26661693773 }, { 590, 53207, 41502563569 }, { 447, 98871, 53763004684 }, { 262, 191230, 66157445586 }, { 224, 320907, 66970764449 }},
    {{ 159, 0, 54525946 }, { 319, 0, 109051858 }, { 174, 1, 218103441 }, { 108, 5, 436204686 }, { 104, 21, 872391797 }, { 31, 84, 1744642985 }, { 107, 338, 3488161306 }, { 180, 1351, 6967329502 }, { 337, 5389, 13862853117 }, { 633, 21335, 27155692131 }, { 538, 54450, 41929193869 }, { 406, 100478, 54095741519 }, { 229, 195182, 66439814810 }, { 257, 324892, 66721911874 }},
    {{ 146, 0, 55574522 }, { 292, 0, 111149007 }, { 121, 1, 222297724 }, { 2, 5, 444593120 }, { 148, 22, 889167636 }, { 119, 88, 1778186404 }, { 92, 351, 3555181982 }, { 151, 1403, 7100842206 }, { 313, 5598, 14125663411 }, { 570, 22151, 27648032715 }, { 488, 55706, 42353265267 }, { 366, 102095, 54425176668 }, { 198, 199150, 66705963216 }, { 288, 328861, 66456770048 }},
    {{ 133, 0, 56623098 }, { 265, 0, 113246156 }, { 67, 1, 226492005 }, { 406, 6, 452981553 }, { 188, 23, 905943422 }, { 71, 91, 1811729382 }, { 60, 364, 3622199261 }, { 154, 1457, 7234327879 }, { 274, 5810, 14388258077 }, { 517, 22983, 28138686151 }, { 434, 56974, 42774751480 }, { 328, 103722, 54751290039 }, { 171, 203134, 66955826410 }, { 323, 332814, 66175403630 }},
    {{ 119, 0, 57671673 }, { 238, 0, 115343305 }, { 12, 1, 230686286 }, { 296, 6, 461369974 }, { 224, 24, 922719154 }, { 144, 95, 1845271942 }, { 77, 378, 3689213108 }, { 125, 1511, 7367785899 }, { 238, 6026, 14650633185 }, { 460, 23829, 28627621976 }, { 384, 58255, 43193627029 }, { 294, 105359, 55074061681 }, { 144, 207132, 67189343466 }, { 357, 336749, 65877881285 }},
    {{ 105, 0, 58720249 }, { 210, 0, 117440454 }, { 980, 2, 234880568 }, { 184, 6, 469758389 }, { 0, 24, 939494823 }, { 80, 98, 1878814046 }, { 77, 392, 3756223430 }, { 97, 1566, 7501215807 }, { 218, 6247, 14912784877 }, { 398, 24689, 29114810547 }, { 338, 59549, 43609866423 }, { 255, 107005, 55393471867 }, { 117, 211143, 67406457209 }, { 398, 340667, 65564276144 }},
    {{ 91, 0, 59768824 }, { 181, 0, 119537603 }, { 923, 2, 239074847 }, { 70, 6, 478146796 }, { 28, 25, 956270444 }, { 136, 102, 1912355717 }, { 61, 406, 3823230162 }, { 101, 1623, 7634617160 }, { 184, 6471, 15174708917 }, { 341, 25564, 29600222101 }, { 290, 60855, 44023443983 }, { 221, 108661, 55709501168 }, { 94, 215167, 67607114711 }, { 437, 344565, 65234664147 }},
    {{ 76, 0, 60817400 }, { 152, 0, 121634752 }, { 865, 2, 243269125 }, { 466, 7, 486535201 }, { 52, 26, 973046007 }, { 56, 105, 1945896915 }, { 30, 420, 3890233239 }, { 73, 1680, 7767989337 }, { 152, 6699, 15436401403 }, { 280, 26453, 30083826966 }, { 240, 62173, 44434334524 }, { 176, 110325, 56022130009 }, { 76, 219203, 67791266978 }, { 478, 348443, 64889126420 }},
    {{ 61, 0, 61865976 }, { 123, 0, 123731901 }, { 806, 2, 247463402 }, { 348, 7, 494923594 }, { 72, 27, 989821513 }, { 96, 109, 1979437664 }, { 46, 435, 3957232631 }, { 45, 1738, 7901331877 }, { 121, 6931, 15697858391 }, { 225, 27357, 30565595768 }, { 189, 63503, 44842512909 }, { 146, 112000, 56331339847 }, { 54, 223248, 67958868381 }, { 522, 352300, 64527746426 }},
    {{ 46, 0, 62914551 }, { 93, 0, 125829049 }, { 746, 2, 251657678 }, { 228, 7, 503311980 }, { 89, 28, 1006596959 }, { 0, 112, 2012977925 }, { 47, 450, 4024228240 }, { 49, 1798, 8034644340 }, { 92, 7167, 15959075847 }, { 167, 28275, 31045498943 }, { 145, 64846, 45247954612 }, { 107, 113683, 56637111300 }, { 39, 227304, 68109879449 }, { 567, 356135, 64150613621 }},
    {{ 31, 0, 63963127 }, { 62, 0, 127926198 }, { 685, 2, 255851953 }, { 106, 7, 511700358 }, { 101, 29, 1023372346 }, { 24, 116, 2046517720 }, { 32, 465, 4091220003 }, { 22, 1858, 8167926089 }, { 49, 7406, 16220049677 }, { 115, 29208, 31523507495 }, { 93, 66200, 45650634332 }, { 75, 115376, 56939426190 }, { 25, 231368, 68244261839 }, { 615, 359947, 63757818296 }},
    {{ 16, 0, 65011702 }, { 31, 0, 130023346 }, { 623, 2, 260046228 }, { 494, 8, 520088733 }, { 109, 30, 1040147671 }, { 40, 120, 2080057026 }, { 0, 480, 4158207858 }, { 27, 1920, 8301176739 }, { 24, 7650, 16480776142 }, { 54, 30154, 31999591587 }, { 50, 67567, 46050528021 }, { 37, 117077, 57238265413 }, { 10, 235439, 68361983625 }, { 667, 363736, 63349458650 }},
    {{ 0, 0, 66060278 }, { 0, 0, 132120494 }, { 560, 2, 264240501 }, { 368, 8, 528477096 }, { 113, 31, 1056922934 }, { 48, 124, 2113595836 }, { 17, 496, 4225191770 }, { 0, 1982, 8434395666 }, { 0, 7898, 16741251156 }, { 0, 31115, 32473722829 }, { 0, 68945, 46447610855 }, { 0, 118787, 57533611291 }, { 0, 239517, 68463015520 }, { 717, 367499, 62925632263 }}
};

const memory<cos_row, (1 << COS_TABLE_LG_SIZE)> cos_coefficient_table =
{
    {{ 1944, 0, 7719789 }, { 10714, 0, 5569692173 }, { 3571, 0, 233620125 }, { 59, 0, 62784116 }, { 246, 0, 64378656 }, { 248, 0, 1039174724 }, { 0, 0, 260043580 }, { 0, 1, 33248323 }, { 0, 1, 16471108 }, { 0, 1, 32330001 }},
    {{ 1940, 4096, 7711597 }, { 10461, 229068, 5439162208 }, { 3463, 1100, 226543725 }, { 58, 642, 60792668 }, { 238, 789, 62320782 }, { 240, 143, 1005702052 }, { 0, 8, 251655252 }, { 0, 1, 32724035 }, { 0, 1, 16208964 }, { 0, 1, 31805713 }},
    {{ 1932, 8191, 7687024 }, { 10202, 456115, 5304983349 }, { 3354, 2186, 219432341 }, { 56, 1276, 58798662 }, { 230, 1563, 60261347 }, { 232, 281, 972224855 }, { 0, 16, 243266908 }, { 0, 1, 32199747 }, { 0, 1, 15946820 }, { 0, 1, 31281425 }},
    {{ 1920, 12284, 7646075 }, { 9936, 681087, 5167188342 }, { 3245, 3259, 212286423 }, { 54, 1903, 56802136 }, { 222, 2320, 58200379 }, { 224, 416, 938743293 }, { 0, 24, 234878548 }, { 0, 1, 31675459 }, { 0, 1, 15684676 }, { 0, 1, 30757137 }},
    {{ 1904, 16374, 7588760 }, { 9663, 903930, 5025810829 }, { 3135, 4318, 205106392 }, { 52, 2522, 54803117 }, { 214, 3062, 56137913 }, { 216, 547, 905257479 }, { 7, 32, 226490172 }, { 0, 1, 31151171 }, { 0, 1, 15422532 }, { 0, 1, 30232849 }},
    {{ 1884, 20460, 7515094 }, { 9383, 1124587, 4880885302 }, { 3025, 5363, 197892689 }, { 50, 3134, 52801638 }, { 206, 3788, 54073979 }, { 208, 674, 871767540 }, { 7, 39, 218101781 }, { 0, 1, 30626883 }, { 0, 1, 15160388 }, { 0, 1, 29708561 }},
    {{ 1860, 24541, 7425094 }, { 9096, 1343007, 4732447168 }, { 2914, 6394, 190645754 }, { 48, 3738, 50797727 }, { 198, 4499, 52008609 }, { 200, 797, 838273603 }, { 7, 46, 209713376 }, { 0, 1, 30102595 }, { 0, 1, 14898244 }, { 0, 1, 29184273 }},
    {{ 1833, 28616, 7318782 }, { 8803, 1559135, 4580532673 }, { 2803, 7412, 183366046 }, { 46, 4334, 48791416 }, { 190, 5193, 49941832 }, { 192, 916, 804775798 }, { 7, 53, 201324957 }, { 0, 1, 29578307 }, { 0, 1, 14636100 }, { 0, 1, 28659985 }},
    {{ 1801, 32684, 7196185 }, { 8504, 1772919, 4425178911 }, { 2691, 8416, 176053995 }, { 44, 4922, 46782734 }, { 183, 5872, 47873684 }, { 184, 1031, 771274250 }, { 6, 60, 192936525 }, { 0, 1, 29054019 }, { 0, 1, 14373956 }, { 0, 1, 28135697 }},
    {{ 1765, 36744, 7057331 }, { 8198, 1984306, 4266423775 }, { 2579, 9405, 168710032 }, { 42, 5503, 44771714 }, { 175, 6535, 45804193 }, { 176, 1142, 737769088 }, { 6, 66, 184548079 }, { 0, 1, 28529731 }, { 0, 1, 14111812 }, { 0, 1, 27611409 }},
    {{ 1725, 40795, 6902256 }, { 7885, 2193245, 4104306025 }, { 2466, 10381, 161334639 }, { 40, 6077, 42758388 }, { 167, 7182, 43733391 }, { 168, 1249, 704260440 }, { 6, 72, 176159621 }, { 0, 1, 28005443 }, { 0, 1, 13849668 }, { 0, 1, 27087121 }},
    {{ 1681, 44836, 6730996 }, { 7567, 2399684, 3938865282 }, { 2353, 11343, 153928250 }, { 38, 6642, 40742782 }, { 159, 7814, 41661312 }, { 160, 1352, 670748434 }, { 6, 78, 167771151 }, { 0, 1, 27481155 }, { 0, 1, 13587524 }, { 0, 1, 26562833 }},
    {{ 1633, 48866, 6543594 }, { 7242, 2603574, 3770141893 }, { 2239, 12290, 146491302 }, { 37, 7200, 38724934 }, { 151, 8430, 39587985 }, { 152, 1451, 637233197 }, { 5, 84, 159382669 }, { 0, 1, 26956868 }, { 0, 1, 13325380 }, { 0, 1, 26038545 }},
    {{ 1582, 52884, 6340095 }, { 6911, 2804865, 3598177072 }, { 2125, 13224, 139024283 }, { 35, 7750, 36704865 }, { 143, 9029, 37513442 }, { 144, 1546, 603714857 }, { 5, 89, 150994176 }, { 0, 1, 26432580 }, { 0, 1, 13063236 }, { 0, 1, 25514257 }},
    {{ 1526, 56889, 6120550 }, { 6573, 3003507, 3423012767 }, { 2010, 14142, 131527600 }, { 33, 8293, 34682613 }, { 135, 9613, 35437715 }, { 136, 1637, 570193543 }, { 5, 94, 142605673 }, { 0, 1, 25908292 }, { 0, 1, 12801092 }, { 0, 1, 24989969 }},
    {{ 1467, 60880, 5885010 }, { 6230, 3199453, 3244691802 }, { 1895, 15047, 124001760 }, { 31, 8827, 32658204 }, { 127, 10181, 33360837 }, { 128, 1724, 536669382 }, { 5, 99, 134217160 }, { 0, 1, 25384004 }, { 0, 1, 12538948 }, { 0, 1, 24465681 }},
    {{ 1403, 64857, 5633537 }, { 5881, 3392653, 3063257668 }, { 1780, 15937, 116447193 }, { 29, 9354, 30631673 }, { 119, 10734, 31282838 }, { 120, 1807, 503142502 }, { 4, 104, 125828638 }, { 0, 1, 24859716 }, { 0, 1, 12276804 }, { 0, 1, 23941393 }},
    {{ 1336, 68818, 5366189 }, { 5527, 3583062, 2878754700 }, { 1664, 16813, 108864375 }, { 27, 9874, 28603051 }, { 111, 11270, 29203750 }, { 112, 1886, 469613031 }, { 4, 108, 117440106 }, { 0, 1, 24335428 }, { 0, 1, 12014660 }, { 0, 1, 23417105 }},
    {{ 1265, 72762, 5083032 }, { 5166, 3770632, 2691227873 }, { 1547, 17675, 101253769 }, { 25, 10385, 26572364 }, { 103, 11791, 27123606 }, { 104, 1960, 436081080 }, { 4, 112, 109051566 }, { 0, 1, 23811140 }, { 0, 1, 11752516 }, { 0, 1, 22892817 }},
    {{ 1190, 76688, 4784134 }, { 4800, 3955317, 2500723022 }, { 1430, 18521, 93615811 }, { 23, 10889, 24539649 }, { 95, 12295, 25042435 }, { 96, 2031, 402546811 }, { 4, 116, 100663018 }, { 0, 1, 23286852 }, { 0, 1, 11490372 }, { 0, 1, 22368529 }},
    {{ 1112, 80595, 4469569 }, { 4428, 4137074, 2307286641 }, { 1313, 19354, 85951014 }, { 21, 11385, 22504934 }, { 87, 12784, 22960272 }, { 88, 2098, 369010334 }, { 3, 120, 92274463 }, { 0, 1, 22762564 }, { 0, 1, 11228228 }, { 0, 1, 21844241 }},
    {{ 1029, 84483, 4139415 }, { 4051, 4315856, 2110965958 }, { 1196, 20171, 78259800 }, { 19, 11873, 20468250 }, { 80, 13257, 20877148 }, { 80, 2161, 335471778 }, { 3, 123, 83885900 }, { 0, 1, 22238276 }, { 0, 1, 10966084 }, { 0, 1, 21319953 }},
    {{ 943, 88350, 3793751 }, { 3668, 4491621, 1911808889 }, { 1078, 20974, 70542670 }, { 17, 12354, 18429630 }, { 72, 13714, 18793093 }, { 72, 2220, 301931270 }, { 3, 126, 75497331 }, { 0, 1, 21713988 }, { 0, 1, 10703940 }, { 0, 1, 20795665 }},
    {{ 853, 92195, 3432661 }, { 3281, 4664326, 1709864100 }, { 959, 21762, 62800080 }, { 15, 12826, 16389101 }, { 64, 14155, 16708139 }, { 64, 2275, 268388939 }, { 3, 129, 67108756 }, { 0, 1, 21189700 }, { 0, 1, 10441796 }, { 0, 1, 20271377 }},
    {{ 759, 96018, 3056234 }, { 2888, 4833929, 1505180842 }, { 841, 22535, 55032504 }, { 13, 13291, 14346698 }, { 56, 14581, 14622320 }, { 56, 2326, 234844912 }, { 2, 132, 58720175 }, { 0, 1, 20665412 }, { 0, 1, 10179652 }, { 0, 1, 19747089 }},
    {{ 661, 99818, 2664563 }, { 2490, 5000387, 1297809096 }, { 722, 23294, 47240432 }, { 11, 13748, 12302451 }, { 48, 14990, 12535666 }, { 48, 2373, 201299318 }, { 2, 134, 50331590 }, { 0, 1, 20141124 }, { 0, 1, 9917508 }, { 0, 1, 19222801 }},
    {{ 560, 103593, 2257742 }, { 2086, 5163661, 1087799473 }, { 602, 24038, 39424324 }, { 9, 14198, 10256393 }, { 40, 15383, 10448209 }, { 40, 2416, 167752284 }, { 2, 136, 41943000 }, { 0, 1, 19616836 }, { 0, 1, 9655364 }, { 0, 1, 18698513 }},
    {{ 456, 107343, 1835871 }, { 1679, 5323711, 875203318 }, { 482, 24766, 31584644 }, { 7, 14639, 8208551 }, { 32, 15761, 8359981 }, { 32, 2455, 134203939 }, { 2, 138, 33554406 }, { 0, 1, 19092548 }, { 0, 1, 9393220 }, { 0, 1, 18174225 }},
    {{ 347, 111067, 1399053 }, { 1266, 5480497, 660072445 }, { 362, 25480, 23721901 }, { 5, 15073, 6158960 }, { 24, 16122, 6271014 }, { 24, 2490, 100654409 }, { 1, 140, 25165809 }, { 0, 1, 18568260 }, { 0, 1, 9131076 }, { 0, 1, 17649937 }},
    {{ 235, 114763, 947393 }, { 848, 5633982, 442459412 }, { 242, 26179, 15836561 }, { 4, 15499, 4107654 }, { 16, 16468, 4181341 }, { 16, 2521, 67103824  }, { 1, 141, 16777208 }, { 0, 1, 18043972 }, { 0, 1, 8868932 }, { 0, 1, 17125649 }},
    {{ 119, 118432, 481005 }, { 426, 5784127, 222417354 }, { 121, 26862, 7929088 }, { 2, 15917, 2054655 }, { 8, 16797, 2090992 }, { 8, 2548, 33552312  }, { 1, 142, 8388605 }, { 0, 1, 17519684 }, { 0, 1, 8606788 }, { 0, 1, 16601361 }},
    {{ 0, 122072, 0 }, { 0, 5930897, 0 }, { 0, 27531, 0 }, { 0, 16327, 0 }, { 0, 17111, 0 }, { 0, 2571, 0 }, { 1, 142, 0 }, { 0, 1, 16995396 }, { 0, 1, 8344644 }, { 0, 1, 16077073 }}
};

const memory<atan_row, 512> atan_table =
{
    { 33554429, 4194303,      0,    0, -11 },
    { 37748732, 4194303,      0,    0, -11 },
    { 41943035, 4194302,      0,    0, -11 },
    { 46137337, 4194302,      0,    0, -11 },
    { 50331639, 4194302,      0,    0, -11 },
    { 54525941, 4194301,      0,    0, -11 },
    { 58720242, 4194301,      0,    0, -11 },
    { 62914542, 4194300,      0,    0, -11 },
    { 33554421, 4194300,      0,    0, -10 },
    { 37748721, 4194299,     -1,    0, -10 },
    { 41943019, 4194298,     -1,    0, -10 },
    { 46137316, 4194296,     -1,    0, -10 },
    { 50331612, 4194295,     -1,    0, -10 },
    { 54525906, 4194293,     -1,    0, -10 },
    { 58720199, 4194292,     -1,    0, -10 },
    { 62914490, 4194290,     -1,    0, -10 },
    { 33554389, 4194288,     -3,    0,  -9 },
    { 37748675, 4194284,     -4,    0,  -9 },
    { 41942957, 4194279,     -4,    0,  -9 },
    { 46137233, 4194274,     -5,    0,  -9 },
    { 50331504, 4194268,     -5,    0,  -9 },
    { 54525769, 4194262,     -6,    0,  -9 },
    { 58720027, 4194255,     -6,    0,  -9 },
    { 62914279, 4194248,     -7,    0,  -9 },
    { 33554261, 4194240,    -15,   -1,  -8 },
    { 37748493, 4194223,    -17,   -1,  -8 },
    { 41942707, 4194204,    -19,   -1,  -8 },
    { 46136900, 4194183,    -21,   -1,  -8 },
    { 50331072, 4194160,    -23,   -1,  -8 },
    { 54525220, 4194135,    -25,   -1,  -8 },
    { 58719341, 4194108,    -27,   -1,  -8 },
    { 62913435, 4194079,    -29,   -1,  -8 },
    { 33553749, 4194048,    -63,   -5,  -7 },
    { 37747764, 4193980,    -71,   -5,  -7 },
    { 41941707, 4193904,    -79,   -5,  -7 },
    { 46135569, 4193820,    -87,   -5,  -7 },
    { 50329344, 4193728,    -95,   -5,  -7 },
    { 54523023, 4193628,   -103,   -5,  -7 },
    { 58716598, 4193520,   -111,   -5,  -7 },
    { 62910061, 4193404,   -119,   -5,  -7 },
    { 33551702, 4193280,   -255,  -21,  -6 },
    { 37744849, 4193008,   -287,  -21,  -6 },
    { 41937708, 4192705,   -319,  -21,  -6 },
    { 46130247, 4192369,   -351,  -21,  -6 },
    { 50322435, 4192001,   -383,  -21,  -6 },
    { 54514239, 4191602,   -415,  -21,  -6 },
    { 58705628, 4191170,   -447,  -21,  -6 },
    { 62896569, 4190707,   -479,  -21,  -6 },
    { 33543516, 4190212,  -1022,  -84,  -5 },
    { 37733196, 4189126,  -1149,  -84,  -5 },
    { 41921726, 4187914,  -1276,  -84,  -5 },
    { 46108981, 4186574,  -1402,  -84,  -5 },
    { 50294833, 4185108,  -1529,  -84,  -5 },
    { 54479155, 4183516,  -1655,  -83,  -5 },
    { 58661822, 4181797,  -1781,  -83,  -5 },
    { 62842708, 4179953,  -1906,  -83,  -5 },
    { 33510843, 4177984,  -4064, -332,  -4 },
    { 37686712, 4173670,  -4563, -330,  -4 },
    { 41858018, 4168859,  -5058, -327,  -4 },
    { 46024266, 4163555,  -5549, -325,  -4 },
    { 50184965, 4157761,  -6037, -322,  -4 },
    { 54339626, 4151482,  -6521, -319,  -4 },
    { 58487768, 4144721,  -7000, -315,  -4 },
    { 62628910, 4137484,  -7474, -312,  -4 },
    { 33381290, 1032444,   -992,  -19,  -3 },
    { 34413233, 1031437,  -1021,  -19,  -3 },
    { 35444154, 1030401,  -1050,  -19,  -3 },
    { 36474024, 1029336,  -1079,  -19,  -3 },
    { 37502815, 1028242,  -1107,  -18,  -3 },
    { 38530499, 1027120,  -1136,  -18,  -3 },
    { 39557046, 1025970,  -1164,  -18,  -3 },
    { 40582430, 1024792,  -1192,  -18,  -3 },
    { 41606621, 1023586,  -1219,  -18,  -3 },
    { 42629593, 1022353,  -1247,  -18,  -3 },
    { 43651317, 1021092,  -1274,  -18,  -3 },
    { 44671767, 1019804,  -1301,  -17,  -3 },
    { 45690916, 1018489,  -1328,  -17,  -3 },
    { 46708736, 1017147,  -1354,  -17,  -3 },
    { 47725201, 1015779,  -1381,  -17,  -3 },
    { 48740285, 1014384,  -1407,  -17,  -3 },
    { 49753961, 1012964,  -1433,  -17,  -3 },
    { 50766204, 1011518,  -1459,  -16,  -3 },
    { 51776988, 1010046,  -1484,  -16,  -3 },
    { 52786288, 1008549,  -1509,  -16,  -3 },
    { 53794077, 1007026,  -1534,  -16,  -3 },
    { 54800332, 1005479,  -1559,  -16,  -3 },
    { 55805027, 1003908,  -1583,  -16,  -3 },
    { 56808139, 1002311,  -1608,  -15,  -3 },
    { 57809642, 1000691,  -1632,  -15,  -3 },
    { 58809514,  999047,  -1655,  -15,  -3 },
    { 59807729,  997380,  -1679,  -15,  -3 },
    { 60804266,  995689,  -1702,  -15,  -3 },
    { 61799100,  993975,  -1725,  -15,  -3 },
    { 62792209,  992239,  -1747,  -14,  -3 },
    { 63783570,  990480,  -1770,  -14,  -3 },
    { 64773161,  988698,  -1792,  -14,  -3 },
    { 32880480,  986895,  -3628,  -56,  -2 },
    { 33865546,  983224,  -3714,  -55,  -2 },
    { 34846899,  979468,  -3797,  -54,  -2 },
    { 35824455,  975630,  -3878,  -52,  -2 },
    { 36798132,  971712,  -3957,  -50,  -2 },
    { 37767853,  967716,  -4034,  -49,  -2 },
    { 38733540,  963645,  -4108,  -48,  -2 },
    { 39695119,  959501,  -4180,  -46,  -2 },
    { 40652519,  955286,  -4249,  -45,  -2 },
    { 41605669,  951003,  -4316,  -43,  -2 },
    { 42554503,  946654,  -4381,  -41,  -2 },
    { 43498955,  942240,  -4444,  -40,  -2 },
    { 44438963,  937766,  -4505,  -38,  -2 },
    { 45374467,  933232,  -4562,  -37,  -2 },
    { 46305408,  928642,  -4618,  -35,  -2 },
    { 47231732,  923997,  -4672,  -33,  -2 },
    { 48153384,  919300,  -4722,  -32,  -2 },
    { 49070314,  914553,  -4771,  -31,  -2 },
    { 49982473,  909758,  -4817,  -29,  -2 },
    { 50889815,  904918,  -4862,  -27,  -2 },
    { 51792295,  900035,  -4904,  -26,  -2 },
    { 52689872,  895111,  -4943,  -25,  -2 },
    { 53582505,  890149,  -4981,  -23,  -2 },
    { 54470157,  885150,  -5017,  -21,  -2 },
    { 55352792,  880116,  -5050,  -20,  -2 },
    { 56230378,  875051,  -5081,  -18,  -2 },
    { 57102884,  869955,  -5110,  -18,  -2 },
    { 57970279,  864831,  -5137,  -16,  -2 },
    { 58832537,  859681,  -5162,  -15,  -2 },
    { 59689633,  854507,  -5185,  -14,  -2 },
    { 60541544,  849311,  -5206,  -12,  -2 },
    { 61388249,  844095,  -5225,  -11,  -2 },
    { 62229729, 1677722, -20975,  -77,  -2 },
    { 63896944, 1656690, -21092,  -57,  -2 },
    { 32771537,  817778, -10591,  -18,  -1 },
    { 33584014,  807173, -10619,  -12,  -1 },
    { 34385875,  796544, -10637,   -4,  -1 },
    { 35177099,  785904, -10642,    4,  -1 },
    { 35957683,  775265, -10636,   11,  -1 },
    { 36727633,  764637, -10619,   18,  -1 },
    { 37486965,  754033, -10592,   26,  -1 },
    { 38235708,  743460, -10553,   31,  -1 },
    { 38973899,  732930, -10506,   37,  -1 },
    { 39701584,  722451, -10452,   43,  -1 },
    { 40418820,  712030, -10387,   46,  -1 },
    { 41125668,  701677, -10317,   51,  -1 },
    { 41822199,  691399, -10240,   56,  -1 },
    { 42508491,  681200, -10155,   59,  -1 },
    { 43184628,  671089, -10067,   64,  -1 },
    { 43850699,  661069,  -9971,   66,  -1 },
    { 44506799,  651147,  -9872,   70,  -1 },
    { 45153028,  641327,  -9768,   72,  -1 },
    { 45789489,  631613,  -9660,   74,  -1 },
    { 46416290,  622008,  -9548,   76,  -1 },
    { 47033543,  612517,  -9434,   78,  -1 },
    { 47641362,  603141,  -9317,   80,  -1 },
    { 48239865,  593884,  -9197,   81,  -1 },
    { 48829170,  584747,  -9075,   82,  -1 },
    { 49409400,  575733,  -8952,   83,  -1 },
    { 49980677,  566843,  -8827,   84,  -1 },
    { 50543127,  558078,  -8701,   85,  -1 },
    { 51096875,  549439,  -8575,   85,  -1 },
    { 51642048,  540928,  -8447,   85,  -1 },
    { 52178773,  532544,  -8319,   85,  -1 },
    { 52707179, 1048576, -32767,  682,  -1 },
    { 53739541, 1016319, -31742,  677,  -1 },
    { 54740158,  985084, -30725,  671,  -1 },
    { 55710047,  954861, -29718,  663,  -1 },
    { 56650215,  925639, -28723,  650,  -1 },
    { 57561655,  897402, -27747,  637,  -1 },
    { 58445342,  870131, -26793,  624,  -1 },
    { 59302232,  843804, -25857,  606,  -1 },
    { 60133259,  818401, -24950,  594,  -1 },
    { 60939333,  793893, -24062,  572,  -1 },
    { 61721338,  770259, -23205,  556,  -1 },
    { 62480134,  747470, -22371,  537,  -1 },
    { 63216553,  725501, -21566,  519,  -1 },
    { 63931400,  704324, -20788,  503,  -1 },
    { 64625456,  683912, -20035,  484,  -1 },
    { 65299471,  664238, -19309,  465,  -1 },
    { 65954170,  645277, -18609,  447,  -1 },
    { 33295127,  313501,  -8968,  216,   0 },
    { 33604198,  304694,  -8645,  208,   0 },
    { 33904621,  296204,  -8333,  200,   0 },
    { 34196709,  288021,  -8034,  193,   0 },
    { 34480761,  280131,  -7746,  185,   0 },
    { 34757065,  272523,  -7469,  177,   0 },
    { 35025898,  265186,  -7202,  169,   0 },
    { 35287526,  258111,  -6948,  164,   0 },
    { 35542203,  251285,  -6702,  157,   0 },
    { 35790176,  244699,  -6467,  151,   0 },
    { 36031679,  238344,  -6242,  146,   0 },
    { 36266939,  232210,  -6024,  138,   0 },
    { 36496171,  226289,  -5817,  133,   0 },
    { 36719585,  220571,  -5617,  128,   0 },
    { 36937379,  215049,  -5425,  122,   0 },
    { 37149746,  419430, -20964,  931,   0 },
    { 37558926,  399160, -19579,  855,   0 },
    { 37948511,  380219, -18299,  783,   0 },
    { 38319775,  362505, -17128,  732,   0 },
    { 38673899,  345920, -16036,  666,   0 },
    { 39011967,  330381, -15034,  615,   0 },
    { 39334984,  315805, -14109,  569,   0 },
    { 39643877,  302120, -13249,  522,   0 },
    { 39939503,  289261, -12459,  487,   0 },
    { 40222656,  277166, -11728,  456,   0 },
    { 40494071,  265777, -11046,  421,   0 },
    { 40754430,  255045, -10414,  391,   0 },
    { 41004366,  244921,  -9824,  359,   0 },
    { 41244465,  235366,  -9285,  344,   0 },
    { 41475274,  226336,  -8775,  316,   0 },
    { 41697302,  217797,  -8302,  295,   0 },
    { 41911022,  209714,  -7859,  273,   0 },
    { 42116874,  202058,  -7448,  255,   0 },
    { 42315272,  194801,  -7066,  241,   0 },
    { 42506600,  187914,  -6707,  226,   0 },
    { 42691217,  181374,  -6367,  207,   0 },
    { 42869459,  175161,  -6055,  196,   0 },
    { 43041642,  169253,  -5760,  184,   0 },
    { 43208060,  163630,  -5484,  174,   0 },
    { 43368992,  158275,  -5222,  161,   0 },
    { 43524695,  153173,  -4981,  155,   0 },
    { 43675416,  148306,  -4749,  142,   0 },
    { 43821383,  143663,  -4534,  135,   0 },
    { 43962813,  139230,  -4331,  128,   0 },
    { 44099909,  134994,  -4139,  120,   0 },
    { 44232863,  130944,  -3959,  115,   0 },
    { 44361856,  127070,  -3787,  106,   0 },
    { 44487059,  246721, -14495,  787,   0 },
    { 44726730,  232809, -13298,  693,   0 },
    { 44953063,  220025, -12242,  625,   0 },
    { 45167123,  208249, -11296,  570,   0 },
    { 45369867,  197377, -10436,  512,   0 },
    { 45562153,  187323,  -9663,  465,   0 },
    { 45744761,  178007,  -8965,  427,   0 },
    { 45918392,  169357,  -8322,  379,   0 },
    { 46083683,  161319,  -7753,  356,   0 },
    { 46241214,  153830,  -7222,  319,   0 },
    { 46391513,  146846,  -6743,  292,   0 },
    { 46535060,  140322,  -6307,  274,   0 },
    { 46672297,  134217,  -5900,  245,   0 },
    { 46803624,  128499,  -5531,  225,   0 },
    { 46929414,  123134,  -5189,  204,   0 },
    { 47050004,  118097,  -4881,  193,   0 },
    { 47165708,  113359,  -4593,  178,   0 },
    { 47276815,  108899,  -4328,  165,   0 },
    { 47383591,  104694,  -4081,  152,   0 },
    { 47486282,  100726,  -3854,  143,   0 },
    { 47585117,   96978,  -3642,  132,   0 },
    { 47680307,   93434,  -3445,  121,   0 },
    { 47772048,   90079,  -3263,  114,   0 },
    { 47860523,   86900,  -3093,  106,   0 },
    { 47945903,   83886,  -2935,   99,   0 },
    { 48028347,   81025,  -2787,   94,   0 },
    { 48108001,   78307,  -2649,   87,   0 },
    { 48185005,   75722,  -2519,   81,   0 },
    { 48259487,   73263,  -2397,   75,   0 },
    { 48331570,   70921,  -2285,   71,   0 },
    { 48401366,   68689,  -2179,   67,   0 },
    { 48468981,   66560,  -2078,   63,   0 },
    { 48534517,  129055,  -7934,  461,   0 },
    { 48659720,  121461,  -7239,  399,   0 },
    { 48777661,  114519,  -6634,  360,   0 },
    { 48888952,  108152,  -6094,  325,   0 },
    { 48994138,  102300,  -5612,  296,   0 },
    { 49093706,   96908,  -5175,  264,   0 },
    { 49188093,   91928,  -4775,  230,   0 },
    { 49277690,   87323,  -4423,  209,   0 },
    { 49362854,   83055,  -4107,  192,   0 },
    { 49443903,   79090,  -3815,  172,   0 },
    { 49521128,   75403,  -3555,  160,   0 },
    { 49594793,   71966,  -3316,  146,   0 },
    { 49665138,   68759,  -3098,  134,   0 },
    { 49732381,   65760,  -2897,  122,   0 },
    { 49796723,   62953,  -2714,  111,   0 },
    { 49858347,   60322,  -2546,  102,   0 },
    { 49917422,   57852,  -2392,   95,   0 },
    { 49974101,   55531,  -2250,   88,   0 },
    { 50028529,   53345,  -2117,   80,   0 },
    { 50080835,   51287,  -1997,   75,   0 },
    { 50131141,   49345,  -1885,   69,   0 },
    { 50179560,   47511,  -1782,   65,   0 },
    { 50226196,   45777,  -1685,   60,   0 },
    { 50271145,   44136,  -1595,   56,   0 },
    { 50314497,   42582,  -1512,   51,   0 },
    { 50356335,   41108,  -1434,   48,   0 },
    { 50396738,   39709,  -1362,   45,   0 },
    { 50435778,   38381,  -1294,   42,   0 },
    { 50473522,   37118,  -1231,   40,   0 },
    { 50510033,   35916,  -1172,   37,   0 },
    { 50545373,   34771,  -1116,   35,   0 },
    { 50579594,   33681,  -1064,   32,   0 },
    { 50612751,   65279,  -4055,  233,   0 },
    { 50676061,   61398,  -3701,  208,   0 },
    { 50735660,   57852,  -3389,  191,   0 },
    { 50791865,   54604,  -3107,  169,   0 },
    { 50844958,   51621,  -2854,  147,   0 },
    { 50895189,   48877,  -2632,  135,   0 },
    { 50942784,   46346,  -2430,  121,   0 },
    { 50987944,   44006,  -2250,  113,   0 },
    { 51030853,   41838,  -2083,   97,   0 },
    { 51071673,   39827,  -1936,   89,   0 },
    { 51110555,   37957,  -1801,   80,   0 },
    { 51147631,   36216,  -1679,   74,   0 },
    { 51183026,   34592,  -1568,   69,   0 },
    { 51216851,   33075,  -1466,   62,   0 },
    { 51249208,   31655,  -1372,   57,   0 },
    { 51280191,   30325,  -1287,   53,   0 },
    { 51309885,   29077,  -1208,   48,   0 },
    { 51338370,   27904,  -1136,   45,   0 },
    { 51365716,   26801,  -1069,   41,   0 },
    { 51391992,   25762,  -1008,   38,   0 },
    { 51417259,   24782,   -951,   35,   0 },
    { 51441574,   23857,   -898,   33,   0 },
    { 51464990,   22982,   -849,   31,   0 },
    { 51487555,   22155,   -804,   28,   0 },
    { 51509315,   21372,   -761,   26,   0 },
    { 51530313,   20630,   -722,   25,   0 },
    { 51550587,   19925,   -685,   23,   0 },
    { 51570176,   19256,   -651,   21,   0 },
    { 51589111,   18621,   -619,   20,   0 },
    { 51607427,   18016,   -589,   19,   0 },
    { 51625153,   17440,   -561,   18,   0 },
    { 51642316,   16891,   -535,   17,   0 },
    { 51658944,   65467,  -8133,  895,   0 },
    { 51720568,   57999,  -6790,  715,   0 },
    { 51775351,   51740,  -5730,  584,   0 },
    { 51824371,   46439,  -4865,  459,   0 },
    { 51868493,   41915,  -4177,  382,   0 },
    { 51908415,   38019,  -3601,  306,   0 },
    { 51944710,   34642,  -3131,  253,   0 },
    { 51977849,   31700,  -2753,  227,   0 },
    { 52008229,   29113,  -2418,  184,   0 },
    { 52036180,   26831,  -2137,  154,   0 },
    { 52061981,   24808,  -1903,  135,   0 },
    { 52085871,   23006,  -1700,  117,   0 },
    { 52108056,   21392,  -1522,   99,   0 },
    { 52128711,   19943,  -1371,   87,   0 },
    { 52147990,   18636,  -1239,   77,   0 },
    { 52166025,   17454,  -1125,   69,   0 },
    { 52182933,   32757,  -4073,  452,   0 },
    { 52213767,   29016,  -3391,  348,   0 },
    { 52241175,   25884,  -2865,  289,   0 },
    { 52265698,   23233,  -2441,  239,   0 },
    { 52287770,   20967,  -2089,  190,   0 },
    { 52307740,   19019,  -1807,  160,   0 },
    { 52325895,   17329,  -1570,  130,   0 },
    { 52342471,   15855,  -1377,  113,   0 },
    { 52357666,   14562,  -1210,   93,   0 },
    { 52371645,   13420,  -1071,   80,   0 },
    { 52384550,   12408,   -953,   70,   0 },
    { 52396498,   11506,   -851,   60,   0 },
    { 52407593,   10699,   -762,   50,   0 },
    { 52417923,    9974,   -686,   44,   0 },
    { 52427565,    9320,   -621,   41,   0 },
    { 52436584,    8728,   -561,   33,   0 },
    { 52445040,   16381,  -2035,  223,   0 },
    { 52460459,   14512,  -1702,  182,   0 },
    { 52474165,   12945,  -1435,  149,   0 },
    { 52486429,   11617,  -1217,  115,   0 },
    { 52497466,   10485,  -1044,   94,   0 },
    { 52507452,    9510,   -903,   78,   0 },
    { 52516530,    8665,   -785,   65,   0 },
    { 52524819,    7928,   -687,   55,   0 },
    { 52532417,    7282,   -606,   48,   0 },
    { 52539408,    6711,   -536,   40,   0 },
    { 52545860,    6204,   -476,   34,   0 },
    { 52551835,    5753,   -425,   29,   0 },
    { 52557383,    5350,   -381,   25,   0 },
    { 52562548,    4987,   -343,   22,   0 },
    { 52567369,    4660,   -310,   19,   0 },
    { 52571879,    4364,   -281,   17,   0 },
    { 52576107,    8192,  -1021,  116,   0 },
    { 52583817,    7255,   -846,   86,   0 },
    { 52590671,    6472,   -715,   71,   0 },
    { 52596803,    5809,   -609,   59,   0 },
    { 52602321,    5243,   -523,   49,   0 },
    { 52607314,    4755,   -451,   39,   0 },
    { 52611854,    4333,   -393,   33,   0 },
    { 52615998,    3964,   -344,   28,   0 },
    { 52619797,    3641,   -302,   23,   0 },
    { 52623293,    3355,   -267,   20,   0 },
    { 52626519,    3102,   -237,   16,   0 },
    { 52629506,    2877,   -212,   15,   0 },
    { 52632280,    2675,   -190,   13,   0 },
    { 52634863,    2494,   -171,   11,   0 },
    { 52637274,    2330,   -155,   10,   0 },
    { 52639529,    2182,   -140,    8,   0 },
    { 52641643,    8190,  -2025,  418,   0 },
    { 52648924,    6471,  -1422,  262,   0 },
    { 52654750,    5241,  -1037,  174,   0 },
    { 52659516,    4332,   -779,  118,   0 },
    { 52663489,    3637,   -592,   77,   0 },
    { 52666849,    3101,   -469,   59,   0 },
    { 52669729,    2675,   -380,   49,   0 },
    { 52672226,    2330,   -309,   37,   0 },
    { 52674411,    4094,  -1006,  201,   0 },
    { 52678052,    3235,   -708,  127,   0 },
    { 52680964,    2621,   -520,   89,   0 },
    { 52683347,    2165,   -387,   57,   0 },
    { 52685333,    1820,   -298,   41,   0 },
    { 52687014,    1551,   -236,   31,   0 },
    { 52688454,    1337,   -190,   24,   0 },
    { 52689702,    1165,   -154,   18,   0 },
    { 52690795,    2043,   -490,   88,   0 },
    { 52692615,    1617,   -351,   60,   0 },
    { 52694071,    1310,   -258,   42,   0 },
    { 52695263,    1083,   -193,   29,   0 },
    { 52696256,     910,   -149,   21,   0 },
    { 52697096,     776,   -118,   16,   0 },
    { 52697816,     669,    -94,   12,   0 },
    { 52698440,     582,    -76,    8,   0 },
    { 52698987,    1023,   -250,   49,   0 },
    { 52699897,     809,   -176,   31,   0 },
    { 52700625,     655,   -129,   22,   0 },
    { 52701221,     541,    -97,   15,   0 },
    { 52701717,     455,    -75,   11,   0 },
    { 52702137,     388,    -59,    8,   0 },
    { 52702497,     334,    -47,    6,   0 },
    { 52702809,     291,    -38,    5,   0 },
    { 52703083,     512,   -126,   26,   0 },
    { 52703538,     404,    -88,   16,   0 },
    { 52703902,     328,    -64,   11,   0 },
    { 52704200,     271,    -48,    7,   0 },
    { 52704448,     228,    -37,    5,   0 },
    { 52704658,     194,    -29,    4,   0 },
    { 52704838,     167,    -23,    3,   0 },
    { 52704994,     146,    -19,    2,   0 },
    { 52705131,     256,    -62,   13,   0 },
    { 52705358,     202,    -44,    8,   0 },
    { 52705540,     164,    -32,    5,   0 },
    { 52705689,     135,    -24,    4,   0 },
    { 52705813,     114,    -18,    2,   0 },
    { 52705918,      97,    -14,    2,   0 },
    { 52706008,      84,    -11,    2,   0 },
    { 52706086,      73,     -9,    1,   0 },
    { 52706155,     128,    -31,    7,   0 },
    { 52706268,     101,    -22,    4,   0 },
    { 52706359,      82,    -16,    3,   0 },
    { 52706434,      68,    -12,    2,   0 },
    { 52706496,      57,     -9,    1,   0 },
    { 52706548,      48,     -7,    1,   0 },
    { 52706593,      42,     -5,    1,   0 },
    { 52706632,      36,     -4,    1,   0 },
    { 52706667,      64,    -15,    3,   0 },
    { 52706723,      51,    -11,    2,   0 },
    { 52706769,      41,     -8,    1,   0 },
    { 52706806,      34,     -6,    1,   0 },
    { 52706837,      28,     -4,    1,   0 },
    { 52706863,      24,     -3,    1,   0 },
    { 52706886,      21,     -2,    0,   0 },
    { 52706905,      18,     -2,    0,   0 },
    { 52706923,      32,     -7,    1,   0 },
    { 52706951,      25,     -5,    1,   0 },
    { 52706974,      20,     -4,    1,   0 },
    { 52706992,      17,     -3,    0,   0 },
    { 52707008,      14,     -2,    0,   0 },
    { 52707021,      12,     -1,    0,   0 },
    { 52707032,      10,     -1,    0,   0 },
    { 52707042,       9,     -1,    0,   0 },
    { 52707051,      16,     -3,    1,   0 },
    { 52707065,      13,     -2,    1,   0 },
    { 52707076,      10,     -1,    0,   0 },
    { 52707085,       8,     -1,    0,   0 },
    { 52707093,       7,     -1,    0,   0 },
    { 52707100,       6,      0,    0,   0 },
    { 52707105,       5,      0,    0,   0 },
    { 52707110,       5,      0,    0,   0 },
    { 52707115,       8,     -1,    0,   0 },
    { 52707122,       6,     -1,    0,   0 },
    { 52707127,       5,     -1,    0,   0 },
    { 52707132,       4,      0,    0,   0 },
    { 52707136,       4,      0,    0,   0 },
    { 52707139,       3,      0,    0,   0 },
    { 52707142,       3,      0,    0,   0 },
    { 52707144,       2,      0,    0,   0 },
    { 52707147,       4,      0,    0,   0 },
    { 52707150,       3,      0,    0,   0 },
    { 52707153,       3,      0,    0,   0 },
    { 52707155,       2,      0,    0,   0 },
    { 52707157,       2,      0,    0,   0 },
    { 52707159,       2,      0,    0,   0 },
    { 52707160,       1,      0,    0,   0 },
    { 52707161,       1,      0,    0,   0 },
    { 52707163,       2,      0,    0,   0 },
    { 52707164,       2,      0,    0,   0 },
    { 52707166,       1,      0,    0,   0 },
    { 52707167,       1,      0,    0,   0 },
    { 52707168,       1,      0,    0,   0 },
    { 52707169,       1,      0,    0,   0 },
    { 52707169,       1,      0,    0,   0 },
    { 52707170,       1,      0,    0,   0 },
    { 52707171,       1,      0,    0,   0 },
    { 52707171,       1,      0,    0,   0 },
    { 52707172,       1,      0,    0,   0 },
    { 52707173,       1,      0,    0,   0 },
    { 52707173,       0,      0,    0,   0 },
    { 52707174,       0,      0,    0,   0 },
    { 52707174,       0,      0,    0,   0 },
    { 52707174,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 },
    {        0,       0,      0,    0,   0 }
};