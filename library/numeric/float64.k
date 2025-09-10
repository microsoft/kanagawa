// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// [Double-precision floating-point format](https://en.wikipedia.org/wiki/Double-precision_floating-point_format)
module numeric.float64
    { from_int
    , from_uint
    , from_float32
    , to_int
    , to_uint
    , add
    , mul
    , eq
    , lt
    , gt
    , neg
    , sub
    , Denorm
    , float64
    , isnan
    , isinf
    , isfinite
    , isnormal
    , signbit
    , min
    , max
    , ceil
    , floor
    , round
    , trunc
    , dim
    , abs
    }

import data.bits
import numeric.float.internal

const auto _mantissa_width = 52;
const auto _exponent_width = 11;
const auto _exponent_bias = 1023;

using float32PackedFormat = floatPackedFormat<23, 8>;
using float64PackedFormat = floatPackedFormat<52, 11>;

enum float64_constants: uint64
{
    POS_MAX       = 0x7FEFFFFFFFFFFFFF,
    NEG_MAX       = 0xFFEFFFFFFFFFFFFF,
    POS_ZERO      = 0x0000000000000000,
    NEG_ZERO      = 0x8000000000000000,
    POS_ONE       = 0x3FF0000000000000,
    NEG_ONE       = 0xBFF0000000000000,
    POS_INFINITY  = 0x7FF0000000000000,
    NEG_INFINITY  = 0xFFF0000000000000,
    DEFAULT_NAN   = 0x7FF8000000000000
}

struct float64ExpandedFormat
{
    uint53 mantissa;
    int12 exponent;
    uint1 sign;
}

struct productFormat
{
    uint53 sticky;
    uint1 round;
    uint1 guard;
    uint53 product;
}

struct float64StickyFormat
{
    uint1 sticky;
    uint55 mantissaGuardRound;
    int13 exponent;
    uint1 sign;
}

struct stickyMantissaFormat
{
    uint1 sticky;
    uint55 mantissaGuardRound;
}

inline float64ExpandedFormat normalizeInput(float64ExpandedFormat in)
{
    float64ExpandedFormat out;

    uint6 shiftAmount = priorityOneEncoderCountDown53(in.mantissa);
    out.mantissa = in.mantissa << shiftAmount;
    out.exponent = in.exponent - cast <int13>(shiftAmount);
    out.sign = in.sign;
    return(out);
}

inline uint6 priorityOneEncoderCountDown53(uint53 in)
{
    auto highest = highest_one<uint53>(in);
    return highest.is_valid ? 52 - highest.value : 53;
}

// Shift left to normalize small mantissa values. There is guaranteed to
// be a 1 in either the MSB or the second-most significant bit because
// mantissas are normalized before taking the product.
inline float64StickyFormat multNormalizeOutput1(float64StickyFormat in)
{
    float64StickyFormat out;

    uint1 shiftAmount = ~cast<uint1>(in.mantissaGuardRound >> 54);

    out.sticky = in.sticky;
    out.mantissaGuardRound = in.mantissaGuardRound << shiftAmount;
    out.exponent = in.exponent - shiftAmount;
    out.sign = in.sign;

    return(out);
}

inline uint6 priorityOneEncoderCountDown55(uint55 in)
{
    auto highest = highest_one<uint55>(in);
    return highest.is_valid ? 54 - highest.value : 55;
}

// Shift right to denorm small exponent values. Exponent must be less
// than or equal to -1022.
inline float64StickyFormat normalizeOutput2(float64StickyFormat input)
{
    float64StickyFormat out;

    uint11 shiftRightAmount = static_cast(-1022 - input.exponent);
    uint56 mantissaGuardRoundSticky = concat(input.mantissaGuardRound, input.sticky);
    uint56 outMantissaGuardRoundSticky;
    outMantissaGuardRoundSticky = shiftRightSticky(mantissaGuardRoundSticky, shiftRightAmount);

    out.sticky = outMantissaGuardRoundSticky & 1;
    out.mantissaGuardRound = outMantissaGuardRoundSticky >> 1;
    out.exponent = input.exponent + shiftRightAmount;
    out.sign = input.sign;

    return(out);
}

inline uint56 shiftRightSticky(uint56 inMantissaGuardRoundSticky, uint11 shiftRightAmount)
{
    uint56 [11] tempValues;
    uint1 [11] tempSticky;

    static for(const uint4 i : 11)
    {
        //Add a loop variable that avoids negative indices
        const uint4 i_minus_one = static_cast((i == 0) ? 0 : i - 1);
        //Calculate the new sticky bit (OR together the new round and sticky bit)
        tempSticky[i] = (((i == 0) ? inMantissaGuardRoundSticky : tempValues[i_minus_one]) & ((1 << ((1 << i) + 1)) - 1)) == 0 ? 0 : 1;
        //Shift the value by the appropriate amount and insert the new sticky bit
        tempValues[i] = (shiftRightAmount & (1 << i)) == 0 ? ((i == 0) ? inMantissaGuardRoundSticky : tempValues[i_minus_one]) : concat(((i == 0) ? inMantissaGuardRoundSticky : tempValues[i_minus_one]) >> ((1 << i) + 1), tempSticky[i]);
    }

    return tempValues[10];
}

inline float64StickyFormat align(float64ExpandedFormat input, uint11 shiftRightAmount)
{
    float64StickyFormat out;

    uint56 mantissaGuardRoundSticky = concat(input.mantissa, cast <uint3> (0));
    uint56 outMantissaGuardRoundSticky;
    outMantissaGuardRoundSticky = shiftRightSticky(mantissaGuardRoundSticky, shiftRightAmount);

    out.sticky = outMantissaGuardRoundSticky & 1;
    out.mantissaGuardRound = outMantissaGuardRoundSticky >> 1;
    out.exponent = input.exponent + shiftRightAmount;
    out.sign = input.sign;

    return(out);
}

// Shift left to normalize small mantissa values - there is no guaranteed the input isn't all zeros
inline float64StickyFormat addNormalizeOutput1(float64StickyFormat in)
{
    float64StickyFormat out;

    //The priority encoder will always find a shift amount between 0 and 55
    //Thus, we can get into a few situations:
    //	1) The exponent is already too small to shift at all (exp < -1022).
    //		Solution is to shift by 0.
    //	2) The priority encoder finds a shift about that would make the exponent too small (exp - PE shift amount < -1022).
    //		Solution is to cap the shift amount to (exp + 1022).
    //	3) Otherwise, use the priority encoder shift amount.
    uint6 priorityEncoderShiftAmount = priorityOneEncoderCountDown55(in.mantissaGuardRound);
    uint6 leftShiftAmount = static_cast(in.exponent - priorityEncoderShiftAmount < -1022 ? in.exponent + 1022 : priorityEncoderShiftAmount);
    out.sticky = in.sticky;
    out.mantissaGuardRound = in.mantissaGuardRound << leftShiftAmount;

    //The priority encoder might have found a shift amount that was actually too small (maxes out at 55).
    //If so, make the exponent -1022.  The mantissa is already zeroed out, so there is nothing to fix.
    out.exponent = priorityEncoderShiftAmount == 55 ? -1022 : in.exponent - leftShiftAmount;
    out.sign = in.sign;

    return(out);
}

// Set denorm value to 0.
inline float64StickyFormat normalizeOutput2Zero(float64StickyFormat input)
{
    float64StickyFormat out;
    out.sticky = 0;
    out.mantissaGuardRound = 0;
    out.exponent = -1023;
    out.sign = input.sign;

    return(out);
}

// Apply exponent bias and add mantissa implied one
template<Denorm denorm_mode>
inline float64ExpandedFormat unpackFloat64(float64PackedFormat a)
{
    float64ExpandedFormat a_PostUnpack;
    a_PostUnpack.sign = a.sign;
    if (denorm_mode == Denorm::Off)
    {
        a_PostUnpack.exponent = cast<int13>(a.exponent) - 1023;
        a_PostUnpack.mantissa = a.mantissa | (1 << 52);
    }
    else
    {
        a_PostUnpack.exponent = a.exponent == 0 ? -1022 : cast<int13>(a.exponent) - 1023;
        a_PostUnpack.mantissa = a.exponent == 0 ? a.mantissa : a.mantissa | (1 << 52);
    }
    return a_PostUnpack;
}

// Check if nan, zero, inf, or finite
template<Denorm denorm_mode>
inline specialCaseFlags getSpecialCases(float64PackedFormat a)
{
    return getSpecialCases_internal<_mantissa_width, _exponent_width, denorm_mode>(a);
}

// Add two float64 values and return the float64 result
inline uint64 float64_add(uint64 in1, uint64 in2)
{
    // Unpack input
    float64PackedFormat a = cast<float64PackedFormat>(in1);
    float64PackedFormat b = cast<float64PackedFormat>(in2);
    float64ExpandedFormat a_PostDecode = unpackFloat64<Denorm::On>(a);
    float64ExpandedFormat b_PostDecode = unpackFloat64<Denorm::On>(b);

    float64StickyFormat a_PostAlign;
    float64StickyFormat b_PostAlign;

    uint11 alignShiftAmountA;
    uint11 alignShiftAmountB;
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
    float64StickyFormat z_postAdd;
    uint56 aTemp = a_PostAlign.mantissaGuardRound << 1 | a_PostAlign.sticky;
    uint56 bTemp = b_PostAlign.mantissaGuardRound << 1 | b_PostAlign.sticky;

    uint57 sumTemp = static_cast(a_PostAlign.sign == b_PostAlign.sign ? aTemp + bTemp :
                        (aTemp >= bTemp ? aTemp - bTemp : bTemp - aTemp));
    z_postAdd.sign =  a_PostAlign.sign == b_PostAlign.sign ? a_PostAlign.sign :
                        (aTemp >= bTemp ? a_PostAlign.sign : b_PostAlign.sign);

    bool shiftSum = sumTemp >> 56 == 1;
    z_postAdd.exponent = a_PostAlign.exponent + cast<uint1>(shiftSum);
    z_postAdd.mantissaGuardRound = shiftSum ? cast <uint55> (sumTemp >> 2) : cast <uint55> (sumTemp >> 1);
    z_postAdd.sticky = shiftSum ? cast <uint1> (sumTemp >> 1) | cast <uint1> (sumTemp) : cast <uint1> (sumTemp);

    //Output Normalization
    float64StickyFormat z_postNormalize = addNormalizeOutput1(z_postAdd);

    //Rounding
    float64ExpandedFormat z_postRounding;

    bool guard = ((z_postNormalize.mantissaGuardRound >> 1) & 1) == 1;
    bool round = (z_postNormalize.mantissaGuardRound & 1) == 1;
    bool m0 = ((z_postNormalize.mantissaGuardRound >> 2) & 1) == 1;
    bool sticky = z_postNormalize.sticky == 1;

    bool roundUp = guard && (round || sticky || m0);
    z_postRounding.mantissa = cast<decltype(z_postRounding.mantissa)>((z_postNormalize.mantissaGuardRound >> 2) + cast<uint1>(roundUp));
    uint1 exp_inc = roundUp && (z_postNormalize.mantissaGuardRound >> 2 == 0x1fffffffffffff) ? 1 : 0;
    z_postRounding.exponent = z_postNormalize.exponent + exp_inc;
    z_postRounding.sign = z_postNormalize.sign;

    //Packing
    float64PackedFormat zOutput;

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
                            (justBInf || (AandBInf && specialCaseSignsEqual) ? b_PostDecode.sign :
                            a_PostDecode.sign & b_PostDecode.sign);

    bool positiveZero = z_postRounding.exponent == -1022 && z_postRounding.mantissa == 0;
    bool zeroExponent = z_postRounding.exponent == -1022 && ((z_postRounding.mantissa >> 52) & 1) == 0;
    bool overflow = z_postRounding.exponent > 1023;

    zOutput.mantissa = specialCaseNaN ? 1 << 51 :
                        (overflow || specialCaseInf) ? 0 :
                        cast<decltype(zOutput.mantissa)>(z_postRounding.mantissa);
    zOutput.exponent = (specialCaseNaN || specialCaseInf || overflow) ? 2047 :
                        zeroExponent ? 0 :
                        cast<decltype(zOutput.exponent)>(z_postRounding.exponent + 1023);
    zOutput.sign = (specialCaseInf || specialCaseZero) ? specialCaseSign :
                    positiveZero ? 0 :
                    z_postRounding.sign;

    return cast<uint64> (zOutput);
}

// Add two float64 values with denorm off and return the float64 result
inline uint64 float64_add_denormoff(uint64 in1, uint64 in2)
{
    // Unpack input
    float64PackedFormat a = cast<float64PackedFormat> (in1);
    float64PackedFormat b = cast<float64PackedFormat> (in2);

    //Unpack
    float64ExpandedFormat a_PostUnpack = unpackFloat64<Denorm::Off>(a);
    float64ExpandedFormat b_PostUnpack = unpackFloat64<Denorm::Off>(b);

    uint11 alignShiftAmountA;
    uint11 alignShiftAmountB;
    if (a_PostUnpack.exponent > b_PostUnpack.exponent)
    {
        alignShiftAmountA = 0;
        alignShiftAmountB = a_PostUnpack.exponent - b_PostUnpack.exponent;
    }
    else
    {
        alignShiftAmountA = b_PostUnpack.exponent - a_PostUnpack.exponent;
        alignShiftAmountB = 0;
    }

    float64StickyFormat a_PostAlign = align(a_PostUnpack, alignShiftAmountA);
    float64StickyFormat b_PostAlign = align(b_PostUnpack, alignShiftAmountB);

    //Addition
    float64StickyFormat z_postAdd;
    uint56 aTemp = a_PostAlign.mantissaGuardRound << 1 | a_PostAlign.sticky;
    uint56 bTemp = b_PostAlign.mantissaGuardRound << 1 | b_PostAlign.sticky;

    uint57 sumTemp = static_cast(a_PostAlign.sign == b_PostAlign.sign ? aTemp + bTemp :
                        (aTemp >= bTemp ? aTemp - bTemp : bTemp - aTemp));
    z_postAdd.sign =  a_PostAlign.sign == b_PostAlign.sign ? a_PostAlign.sign :
                        (aTemp >= bTemp ? a_PostAlign.sign : b_PostAlign.sign);

    bool shiftSum = sumTemp >> 56 == 1;
    z_postAdd.exponent = a_PostAlign.exponent + cast<uint1>(shiftSum);
    z_postAdd.mantissaGuardRound = shiftSum ? cast <uint55> (sumTemp >> 2) : cast <uint55> (sumTemp >> 1);
    z_postAdd.sticky = shiftSum ? cast <uint1> (sumTemp >> 1) | cast <uint1> (sumTemp) : cast <uint1> (sumTemp);

    //Output Normalization
    float64StickyFormat z_postNormalize = addNormalizeOutput1(z_postAdd);

    //Rounding
    float64ExpandedFormat z_postRounding;

    bool guard = ((z_postNormalize.mantissaGuardRound >> 1) & 1) == 1;
    bool round = (z_postNormalize.mantissaGuardRound & 1) == 1;
    bool m0 = ((z_postNormalize.mantissaGuardRound >> 2) & 1) == 1;
    bool sticky = z_postNormalize.sticky == 1;

    bool roundUp = guard && (round || sticky || m0);
    z_postRounding.mantissa = cast<decltype(z_postRounding.mantissa)>((z_postNormalize.mantissaGuardRound >> 2) + cast<uint1>(roundUp));
    uint1 exp_inc = roundUp && (z_postNormalize.mantissaGuardRound >> 2 == 0x1fffffffffffff) ? 1 : 0;
    z_postRounding.exponent = z_postNormalize.exponent + exp_inc;
    z_postRounding.sign = z_postNormalize.sign;

    //Packing
    float64PackedFormat zOutput;

    //Handle special cases
    specialCaseFlags a_flags = getSpecialCases<Denorm::Off>(a);
    specialCaseFlags b_flags = getSpecialCases<Denorm::Off>(b);
    bool justAInf = a_flags.inf && !b_flags.inf;
    bool justBInf = !a_flags.inf && b_flags.inf;
    bool AandBInf = a_flags.inf && b_flags.inf;
    bool specialCaseSignsEqual = a.sign == b.sign;

    bool specialCaseNaN = a_flags.nan || b_flags.nan || (AandBInf && !specialCaseSignsEqual);
    bool specialCaseZero = (a_flags.zero && b_flags.zero);
    bool specialCaseInf = justAInf || justBInf || (AandBInf && specialCaseSignsEqual);
    //Careful that you check for specialCaseZero first!
    bool specialCaseReturnA = b_flags.zero;
    bool specialCaseReturnB = a_flags.zero;
    uint1 specialCaseSign = justAInf ? a_PostUnpack.sign :
                            justBInf ? b_PostUnpack.sign :
                            a_PostUnpack.sign & b_PostUnpack.sign;

    bool positiveZero = z_postRounding.exponent == -1022 && z_postRounding.mantissa == 0;
    bool zeroOut = z_postRounding.exponent == -1022 && ((z_postRounding.mantissa >> 52) & 1) == 0;
    bool overflow = z_postRounding.exponent > 1023;

    zOutput.mantissa = specialCaseNaN ? 1 << 51 :
                        (specialCaseZero || specialCaseInf || overflow) ? 0 :
                        specialCaseReturnA ? a.mantissa :
                        specialCaseReturnB ? b.mantissa:
                        zeroOut ? 0 :
                        cast<decltype(zOutput.mantissa)>(z_postRounding.mantissa);
    zOutput.exponent = (specialCaseNaN || specialCaseInf || overflow) ? 2047 :
                        specialCaseZero ? 0 :
                        specialCaseReturnA ? a.exponent :
                        specialCaseReturnB ? b.exponent :
                        (positiveZero || zeroOut) ? 0 :
                        z_postRounding.exponent + 1023;
    zOutput.sign = (specialCaseInf || specialCaseZero) ? specialCaseSign :
                    specialCaseReturnA ? a.sign :
                    specialCaseReturnB ? b.sign :
                    positiveZero ? 0 : z_postRounding.sign;

    return cast<uint64> (zOutput);
}

//| Addition.
template <Denorm denorm_mode>
inline uint64 add(uint64 in1, uint64 in2)
{
    uint64 result;
    if (denorm_mode == Denorm::On)
    {
        result = float64_add(in1, in2);
    }
    else
    {
        result = float64_add_denormoff(in1, in2);
    }
    return result;
}

// Multiply two float64 values and return the float64 result
inline uint64 float64_mul(uint64 in1, uint64 in2)
{
    // Unpack input
    float64PackedFormat a = cast<float64PackedFormat> (in1);
    float64PackedFormat b = cast<float64PackedFormat> (in2);
    float64ExpandedFormat a_PostDecode = unpackFloat64<Denorm::On>(a);
    float64ExpandedFormat b_PostDecode = unpackFloat64<Denorm::On>(b);

    float64ExpandedFormat a_PostDenorm = normalizeInput(a_PostDecode);
    float64ExpandedFormat b_PostDenorm = normalizeInput(b_PostDecode);

    //Multiplication
    float64StickyFormat z_postMultiply;
    z_postMultiply.sign = a_PostDenorm.sign ^ b_PostDenorm.sign;
    z_postMultiply.exponent = a_PostDenorm.exponent + b_PostDenorm.exponent + 1;

    uint108 productTemp = (a_PostDenorm.mantissa * b_PostDenorm.mantissa) << 2;
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
    float64StickyFormat z_postNormalize2 = z_postMultiply.exponent <= -1022 ?
                                           normalizeOutput2(z_postMultiply) :
                                           multNormalizeOutput1(z_postMultiply);

    //Rounding
    float64ExpandedFormat z_postRounding;

    bool guard = ((z_postNormalize2.mantissaGuardRound >> 1) & 1) == 1;
    bool round = (z_postNormalize2.mantissaGuardRound & 1) == 1;
    bool m0 = ((z_postNormalize2.mantissaGuardRound >> 2) & 1) == 1;
    bool sticky = z_postNormalize2.sticky == 1;

    bool roundUp = guard && (round || sticky || m0);
    z_postRounding.mantissa = (z_postNormalize2.mantissaGuardRound >> 2) + cast<uint1>(roundUp);
    uint1 exp_inc = roundUp && (z_postNormalize2.mantissaGuardRound >> 2 == 0x1fffffffffffff) ? 1 : 0;
    z_postRounding.exponent = z_postNormalize2.exponent + exp_inc;
    z_postRounding.sign = z_postNormalize2.sign;

    //Packing
    float64PackedFormat zOutput;

    //Handle special cases
    specialCaseFlags a_flags = getSpecialCases<Denorm::On>(a);
    specialCaseFlags b_flags = getSpecialCases<Denorm::On>(b);

    bool specialCaseNaN = (a_flags.nan || b_flags.nan || (a_flags.inf && b_flags.zero) || (a_flags.zero && b_flags.inf));
    bool specialCaseZero = a_flags.zero || b_flags.zero;
    bool specialCaseInf = ((a_flags.inf && !b_flags.zero) || (!a_flags.zero && b_flags.inf));

    zOutput.mantissa = specialCaseNaN ? 1 << 51 :
                        (specialCaseZero || specialCaseInf || (z_postRounding.exponent > 1023) ? 0 : z_postRounding.mantissa);
    zOutput.exponent = (specialCaseNaN || specialCaseInf || (z_postRounding.exponent > 1023)) ? 2047 :
                        (specialCaseZero || (z_postRounding.exponent == -1022 && ((z_postRounding.mantissa >> 52) & 1) == 0) ? 0 :
                        z_postRounding.exponent + 1023);
    zOutput.sign = z_postRounding.sign;

    return cast<uint64> (zOutput);
}

// Multiply two float64 values with denorm off and return the float64 result
inline uint64 float64_mul_denormoff(uint64 in1, uint64 in2)
{
    float64PackedFormat a = cast<float64PackedFormat> (in1);
    float64PackedFormat b = cast<float64PackedFormat> (in2);

    //Unpack
    float64ExpandedFormat a_PostUnpack = unpackFloat64<Denorm::Off>(a);
    float64ExpandedFormat b_PostUnpack = unpackFloat64<Denorm::Off>(b);

    //Multiplication
    float64StickyFormat z_postMultiply;
    z_postMultiply.sign = a_PostUnpack.sign ^ b_PostUnpack.sign;
    z_postMultiply.exponent = a_PostUnpack.exponent + b_PostUnpack.exponent + 1;

    uint108 productTemp = (a_PostUnpack.mantissa * b_PostUnpack.mantissa) << 2;
    productFormat product;
    product = cast<productFormat> (productTemp);

    z_postMultiply.mantissaGuardRound = cast<uint55> (product.product << 2);
    z_postMultiply.mantissaGuardRound |= product.guard << 1;
    z_postMultiply.mantissaGuardRound |= product.round;
    z_postMultiply.sticky = cast<uint1> (product.sticky != 0);

    //Output Normalization
    bool z_denorm = z_postMultiply.exponent < -1022 ||
                    (z_postMultiply.exponent == -1022 && ((z_postMultiply.mantissaGuardRound >> 54) & 1) == 0);
    float64StickyFormat z_postNormalize2 = z_denorm ?
                                           normalizeOutput2Zero(z_postMultiply) :
                                           multNormalizeOutput1(z_postMultiply);

    //Rounding
    float64ExpandedFormat z_postRounding;

    bool guard = ((z_postNormalize2.mantissaGuardRound >> 1) & 1) == 1;
    bool round = (z_postNormalize2.mantissaGuardRound & 1) == 1;
    bool m0 = ((z_postNormalize2.mantissaGuardRound >> 2) & 1) == 1;
    bool sticky = z_postNormalize2.sticky == 1;

    bool roundUp = guard && (round || sticky || m0);
    z_postRounding.mantissa = (z_postNormalize2.mantissaGuardRound >> 2) + cast<uint1>(roundUp);
    uint1 exp_inc = roundUp && ((z_postNormalize2.mantissaGuardRound >> 2) == 0x1fffffffffffff) ? 1 : 0;
    z_postRounding.exponent = z_postNormalize2.exponent + exp_inc;
    z_postRounding.sign = z_postNormalize2.sign;

    //Packing
    float64PackedFormat zOutput;
    //Handle special cases
    specialCaseFlags a_flags = getSpecialCases<Denorm::Off>(a);
    specialCaseFlags b_flags = getSpecialCases<Denorm::Off>(b);

    bool specialCaseNaN = (a_flags.nan || b_flags.nan || (a_flags.inf && b_flags.zero) || (a_flags.zero && b_flags.inf));
    bool specialCaseZero = (a_flags.zero || b_flags.zero);
    bool specialCaseInf = ((a_flags.inf && !b_flags.zero) || (!a_flags.zero && b_flags.inf));

    zOutput.mantissa = specialCaseNaN ? 1 << 51 :
                        (specialCaseZero || specialCaseInf || (z_postRounding.exponent > 1023) ? 0 : z_postRounding.mantissa);
    zOutput.exponent = (specialCaseNaN || specialCaseInf || (z_postRounding.exponent > 1023)) ? 2047 :
                        specialCaseZero ? 0 :
                        z_postRounding.exponent + 1023;
    zOutput.sign = z_postRounding.sign;

    return cast<uint64> (zOutput);
}

//| Multiplication.
template <Denorm denorm_mode>
inline uint64 mul(uint64 in1, uint64 in2)
{
    uint64 result;
    if (denorm_mode == Denorm::On)
    {
        result = float64_mul(in1, in2);
    }
    else
    {
        result = float64_mul_denormoff(in1, in2);
    }
    return result;
}

//| Compare two float64 values for equality.
template <Denorm denorm_mode>
inline bool eq(uint64 in1, uint64 in2)
{
    // Unpack input
    float64PackedFormat a = cast<float64PackedFormat>(in1);
    float64PackedFormat b = cast<float64PackedFormat>(in2);
    // Get zero, nan, or inf
    specialCaseFlags a_flags = getSpecialCases<denorm_mode>(a);
    specialCaseFlags b_flags = getSpecialCases<denorm_mode>(b);

    bool result;
    // Negative and positive zero are equal
    if (a_flags.zero && b_flags.zero)
    {
        result = true;
    }
    // NaNs are never equal
    else if (a_flags.nan || b_flags.nan)
    {
        result = false;
    }
    else
    {
        result = in1 == in2;
    }
    return result;
}

//| Check `in1` less than `in2`.
template <Denorm denorm_mode>
inline bool lt(uint64 in1, uint64 in2)
{
    // Unpack input
    float64PackedFormat a;
    float64PackedFormat b;
    a = cast<float64PackedFormat>(in1);
    b = cast<float64PackedFormat>(in2);

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
inline bool gt(uint64 in1, uint64 in2)
{
    // Unpack input
    float64PackedFormat a;
    float64PackedFormat b;
    a = cast<float64PackedFormat>(in1);
    b = cast<float64PackedFormat>(in2);

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

//| Negate.
inline uint64 neg(uint64 x)
{
    float64PackedFormat binary = cast<float64PackedFormat>(x);
    binary.sign = ~binary.sign;
    return cast<uint64>(binary);
}

//| Subtract `in2` from `in1`.
template <Denorm denorm_mode>
inline uint64 sub(uint64 in1, uint64 in2)
{
    return add<denorm_mode>(in1, neg(in2));
}

//| Convert a signed integer to a float64.
template <auto N /*< Width of `value`.*/>
inline uint64 from_int(int<N> value)
{
    return int_to_float<int<N>, uint64, _mantissa_width, _exponent_width, _exponent_bias>(value);
}

//| Convert an unsigned integer to a float64.
template <auto N /*< Width of `value`.*/>
inline uint64 from_uint(uint<N> value)
{
    return int_to_float<uint<N>, uint64, _mantissa_width, _exponent_width, _exponent_bias>(value);
}

//| Convert a float64 to a signed integer
template <auto N /*< Width of the signed integer.*/>
inline optional<int<N>> to_int(uint64 value)
{
    auto result = float_to_int<uint64, int<N+1>, _mantissa_width, _exponent_width, _exponent_bias>(value);
    if (result.is_valid)
    {
        auto limits = get_limits<int<N>>();
        if ((result.value < limits.first) || (result.value > limits.second))
            result.is_valid = false;
    }

    return make_optional<int<N>>(result.is_valid, cast<int<N>>(result.value));
}

//| Convert a float64 to an unsigned integer
template <auto N /*< Width of the unsigned integer.*/>
inline optional<uint<N>> to_uint(uint64 value)
{
    auto result = float_to_int<uint64, uint<N+1>, _mantissa_width, _exponent_width, _exponent_bias>(value);
    if (result.is_valid)
    {
        auto limits = get_limits<uint<N>>();
        if ((result.value < limits.first) || (result.value > limits.second))
            result.is_valid = false;
    }

    return make_optional<uint<N>>(result.is_valid, cast<uint<N>>(result.value));
}

//| Convert a float32 to a float64.
inline float64 from_float32(float32 value)
{
    const auto float32_mantissa_width = 23;
    const auto float64_mantissa_width = _mantissa_width;
    const auto float32_exponent_bias = 127;
    const auto float64_exponent_bias = _exponent_bias;
    const auto float32_exponent_all_bits = 0xFF;
    const auto float64_exponent_all_bits = 0x7FF;

    const auto float_packed = cast<float32PackedFormat>(value);
    float64PackedFormat double_packed;

    if (float_packed.exponent == 0 && float_packed.mantissa == 0)
    {
        // Zero is a bit special
        double_packed.exponent = 0;
        double_packed.mantissa = 0;
    }
    else if (float_packed.exponent == float32_exponent_all_bits)
    {
        // Special values infinity and Nan
        double_packed.exponent = float64_exponent_all_bits;
        double_packed.mantissa = float_packed.mantissa;
        double_packed.mantissa = double_packed.mantissa << (float64_mantissa_width - float32_mantissa_width);
    }
    else
    {
        if (float_packed.exponent == 0)
        {
            // Denormalized floats
            const auto highest = highest_one(float_packed.mantissa);
            double_packed.exponent = (-(float32_mantissa_width - highest.value)) + (-(float32_exponent_bias - 1)) + float64_exponent_bias;
            double_packed.mantissa = float_packed.mantissa;
            double_packed.mantissa = cast<decltype(double_packed.mantissa)>(double_packed.mantissa << (float64_mantissa_width - highest.value));
        }
        else
        {
            // Normalized floats
            int23 float_exp = float_packed.exponent;
            float_exp -= float32_exponent_bias;
            double_packed.exponent = float_exp;
            double_packed.exponent += float64_exponent_bias;
            double_packed.mantissa = float_packed.mantissa;
            double_packed.mantissa = double_packed.mantissa << (float64_mantissa_width - float32_mantissa_width);
        }
    }
    double_packed.sign = float_packed.sign;

    return cast<float64>(double_packed);
}

//| Check `x` is not a number.
inline bool isnan(uint64 x)
{
    const auto flags = getSpecialCases<Denorm::On>(cast<float64PackedFormat>(x));
    return flags.nan;
}

//| Check `x` is positive or negative infinity.
inline bool isinf(uint64 x)
{
    const auto flags = getSpecialCases<Denorm::On>(cast<float64PackedFormat>(x));
    return flags.inf;
}

inline bool isfinite(uint64 x)
{
    const auto flags = getSpecialCases<Denorm::On>(cast<float64PackedFormat>(x));
    return flags.finite;
}

inline bool isnormal(uint64 x)
{
    const auto s = cast<float64PackedFormat>(x);
    return s.exponent != 0x7FF && s.exponent != 0;
}

//| Return sign of `x`.
inline bool signbit(uint64 x)
{
    const auto s = cast<float64PackedFormat>(x);
    return s.sign == 1;
}

//| Return the smaller of `x` and `y`. A NaN is treated as missing data, and
// the other value will be returned.
template<Denorm denorm_mode>
inline uint64 min(uint64 x, uint64 y)
{
    return (isnan(y) || lt<denorm_mode>(x, y)) ? x : y;
}

//| Return the larger of `x` and `y`. A NaN is treated as missing data, and
// the other value will be returned.
template<Denorm denorm_mode>
inline uint64 max(uint64 x, uint64 y)
{
    return (isnan(y) || gt<denorm_mode>(x, y)) ? x : y;
}

//| Return the smallest integer not less than `x`: \(\lceil x \rceil\)
template<Denorm denorm_mode>
inline uint64 ceil(uint64 x)
{
    return ceil_internal<uint64, _mantissa_width, _exponent_width, _exponent_bias, denorm_mode>(x);
}

//| Return the largest integer not greater than `x`: \(\lfloor x \rfloor\)
template<Denorm denorm_mode>
inline uint64 floor(uint64 x)
{
    return floor_internal<uint64, _mantissa_width, _exponent_width, _exponent_bias, denorm_mode>(x);
}

//| Return the nearest integer to `x`, rounding halfway away from zero.
template<Denorm denorm_mode>
inline uint64 round(uint64 x)
{
    return round_internal<uint64, _mantissa_width, _exponent_width, _exponent_bias, denorm_mode>(x);
}

//| Return the nearest integer not greater in magnitude than `x`.
template<Denorm denorm_mode>
inline uint64 trunc(uint64 x)
{
    return trunc_internal<uint64, _mantissa_width, _exponent_width, _exponent_bias, denorm_mode>(x);
}

//| Return the positive difference between `x` and `y`, i.e. `max(x - y, 0)`.
template<Denorm denorm_mode>
inline uint64 dim(uint64 x, uint64 y)
{
    uint64 result;

    if (isnan(x) || isnan(y))
    {
        result = float64_constants::DEFAULT_NAN;
    }
    else
    {
        result = gt<denorm_mode>(x, y) ? sub<denorm_mode>(x, y) : float64_constants::POS_ZERO;
    }

    return result;
}

//| Return the absolute value of `x`: $|x|$
template<Denorm denorm_mode>
inline uint64 abs(uint64 x)
{
    auto structValue = cast<float64PackedFormat>(x);
    structValue.sign = 0;
    return cast<uint64>(structValue);
}