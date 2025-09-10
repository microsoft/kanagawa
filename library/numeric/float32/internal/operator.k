// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Note: The contents of this module should be kept to the minimal set of float32 operators
// necessary to support the compiler's built-in float32 operators on platforms that do not
// directly support this with hardened logic (for example, Xilinx U250).
module numeric.float32.internal.operator
    { _mantissa_width
    , _exponent_width
    , _exponent_bias
    , float32PackedFormat
    , float32ExpandedFormat
    , float32StickyFormat
    , productFormat
    , unpackFloat32
    , normalizeOutput2Zero
    , multNormalizeOutput1
    , getSpecialCases
    , shiftRightSticky
    , align
    , addNormalizeOutput1
    , priorityOneEncoderCountDown26
    , float32_mul_denormoff
    , float32_add_denormoff
    , neg
    , float32_sub_denormoff
    }

import numeric.float.internal
import data.bits

const auto _mantissa_width = 23;
const auto _exponent_width = 8;
const auto _exponent_bias = 127;

using float32PackedFormat = floatPackedFormat<23, 8>;

struct float32ExpandedFormat
{
    uint24 mantissa;
    int9 exponent;
    uint1 sign;
}

struct float32StickyFormat
{
    uint1 sticky;
    uint26 mantissaGuardRound;
    int10 exponent;
    uint1 sign;
}

struct productFormat
{
    uint24 sticky;
    uint1 round;
    uint1 guard;
    uint24 product;
}


//| Apply exponent bias and add mantissa implied one.
template<Denorm denorm_mode>
inline float32ExpandedFormat unpackFloat32(float32PackedFormat a)
{
    float32ExpandedFormat a_PostUnpack;
    a_PostUnpack.sign = a.sign;

    if (denorm_mode == Denorm::Off)
    {
        a_PostUnpack.exponent = cast<int9>(a.exponent) - 127;
        a_PostUnpack.mantissa = a.mantissa | 1 << 23;
    }
    else
    {
        a_PostUnpack.exponent = (a.exponent == 0) ? -126 : cast<int9>(a.exponent) - 127;
        a_PostUnpack.mantissa = (a.exponent == 0) ? a.mantissa : a.mantissa | (1 << 23);
    }

    return a_PostUnpack;
}

//| Set denorm values to 0.
inline float32StickyFormat normalizeOutput2Zero(float32StickyFormat input)
{
    float32StickyFormat out;
    out.sticky = 0;
    out.mantissaGuardRound = 0;
    out.exponent = -127;
    out.sign = input.sign;

    return(out);
}

//| Shift left to normalize small mantissa values. There is guaranteed to
// be a 1 in either the MSB or the second-most significant bit because
// mantissas are normalized before taking the product.
inline float32StickyFormat multNormalizeOutput1(float32StickyFormat in)
{
    float32StickyFormat out;

    uint1 shiftAmount = ~cast<uint1>(in.mantissaGuardRound >> 25);

    out.sticky = in.sticky;
    out.mantissaGuardRound = in.mantissaGuardRound << shiftAmount;
    out.exponent = in.exponent - shiftAmount;
    out.sign = in.sign;

    return(out);
}

//| Check if nan, zero, inf, or finite.
template<Denorm denorm_mode>
inline specialCaseFlags getSpecialCases(float32PackedFormat a)
{
    return getSpecialCases_internal<_mantissa_width, _exponent_width, denorm_mode>(a);
}

inline uint27 shiftRightSticky(uint27 inMantissaGuardRoundSticky, uint8 shiftRightAmount)
{
    uint27 [8] tempValues;
    uint1 [8] tempSticky;

    static for(const uint3 i : 8)
    {
        //Add a loop variable that avoids negative indices
        const uint3 i_minus_one = cast<uint3>((i == 0) ? 0 : i - 1);
        //Calculate the new sticky bit (OR together the new round and sticky bit)
        tempSticky[i] = (((i == 0) ? inMantissaGuardRoundSticky : tempValues[i_minus_one]) & ((1 << ((1 << i) + 1)) - 1)) == 0 ? 0 : 1;
        //Shift the value by the appropriate amount and insert the new sticky bit
        tempValues[i] = (shiftRightAmount & (1 << i)) == 0 ? ((i == 0) ? inMantissaGuardRoundSticky : tempValues[i_minus_one]) : concat(((i == 0) ? inMantissaGuardRoundSticky : tempValues[i_minus_one]) >> ((1 << i) + 1), tempSticky[i]);
    }

    return tempValues[7];
}

inline float32StickyFormat align(float32ExpandedFormat input, uint8 shiftRightAmount)
{
    float32StickyFormat out;

    uint27 mantissaGuardRoundSticky = concat(input.mantissa, cast <uint3> (0));
    uint27 outMantissaGuardRoundSticky = shiftRightSticky(mantissaGuardRoundSticky, shiftRightAmount);

    out.sticky = outMantissaGuardRoundSticky & 1;
    out.mantissaGuardRound = outMantissaGuardRoundSticky >> 1;
    out.exponent = input.exponent + shiftRightAmount;
    out.sign = input.sign;

    return(out);
}


inline uint5 priorityOneEncoderCountDown26(uint26 in)
{
    auto highest = highest_one<uint26>(in);
    return highest.is_valid ? 25 - highest.value : 26;
}

//| Shift left to normalize small mantissa values. There is no guarantee the input isn't all zeros.
inline float32StickyFormat addNormalizeOutput1(float32StickyFormat in)
{
    float32StickyFormat out;

    //The priority encoder will always find a shift amount between 0 and 26
    //Thus, we can get into a few situations:
    //  1) The exponent is already too small to shift at all (exp < -126).
    //      Solution is to shift by 0.
    //  2) The priority encoder finds a shift about that would make the exponent too small (exp - PE shift amount < -126).
    //      Solution is to cap the shift amount to (exp + 126).
    //  3) Otherwise, use the priority encoder shift amount.
    uint5 priorityEncoderShiftAmount = priorityOneEncoderCountDown26(in.mantissaGuardRound);
    uint5 leftShiftAmount = cast<uint5>(in.exponent - priorityEncoderShiftAmount < -126 ? in.exponent + 126 : priorityEncoderShiftAmount);
    out.sticky = in.sticky;
    out.mantissaGuardRound = in.mantissaGuardRound << leftShiftAmount;

    //The priority encoder might have found a shift amount that was actually too small (maxes out at 26).
    //If so, make the exponent -126.  The mantissa is already zeroed out, so there is nothing to fix.
    out.exponent = priorityEncoderShiftAmount == 26 ? -126 : in.exponent - leftShiftAmount;
    out.sign = in.sign;

    return(out);
}


//| Multiply two float32 values with denorm off and return the float32 result.
inline float32 float32_mul_denormoff(float32 in1, float32 in2)
{
    // Unpack input
    float32PackedFormat a;
    float32PackedFormat b;
    a = cast<float32PackedFormat> (in1);
    b = cast<float32PackedFormat> (in2);
    float32ExpandedFormat a_PostUnpack = unpackFloat32<Denorm::Off>(a);
    float32ExpandedFormat b_PostUnpack = unpackFloat32<Denorm::Off>(b);

    //Multiplication
    float32StickyFormat z_postMultiply;
    z_postMultiply.sign = a_PostUnpack.sign ^ b_PostUnpack.sign;
    z_postMultiply.exponent = a_PostUnpack.exponent + b_PostUnpack.exponent + 1;

    uint50 productTemp = cast<uint50>((a_PostUnpack.mantissa * b_PostUnpack.mantissa) << 2);
    productFormat product;
    product = cast<productFormat> (productTemp);

    z_postMultiply.mantissaGuardRound = cast<uint26> (product.product << 2);
    z_postMultiply.mantissaGuardRound |= product.guard << 1;
    z_postMultiply.mantissaGuardRound |= product.round;
    z_postMultiply.sticky = cast<uint1> (product.sticky != 0);

    //Output Normalization
    bool z_denorm = z_postMultiply.exponent < -126 ||
                    (z_postMultiply.exponent == -126 && ((z_postMultiply.mantissaGuardRound >> 25) & 1) == 0);
    float32StickyFormat z_postNormalize2 = z_denorm ? normalizeOutput2Zero(z_postMultiply) : multNormalizeOutput1(z_postMultiply);

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
    specialCaseFlags a_flags = getSpecialCases<Denorm::Off>(a);
    specialCaseFlags b_flags = getSpecialCases<Denorm::Off>(b);

    bool specialCaseNaN = a_flags.nan || b_flags.nan || (a_flags.inf && b_flags.zero) || (a_flags.zero && b_flags.inf);
    bool specialCaseZero = a_flags.zero || b_flags.zero;
    bool specialCaseInf = (a_flags.inf && !b_flags.zero) || (!a_flags.zero && b_flags.inf);

    zOutput.mantissa = specialCaseNaN ? 1 << 22 :
                        (specialCaseZero || specialCaseInf || (z_postRounding.exponent > 127) ? 0 : z_postRounding.mantissa);
    zOutput.exponent = (specialCaseNaN || specialCaseInf || (z_postRounding.exponent > 127)) ? 255 :
                        (specialCaseZero) ? 0 :
                        z_postRounding.exponent + 127;
    zOutput.sign = z_postRounding.sign;

    return cast<float32> (zOutput);
}

//| Add two float32 values with denorm off and return the float32 result.
inline float32 float32_add_denormoff(float32 in1, float32 in2)
{
    // Unpack input
    float32PackedFormat a;
    float32PackedFormat b;
    a = cast<float32PackedFormat> (in1);
    b = cast<float32PackedFormat> (in2);
    float32ExpandedFormat a_PostUnpack = unpackFloat32<Denorm::Off>(a);
    float32ExpandedFormat b_PostUnpack = unpackFloat32<Denorm::Off>(b);

    float32StickyFormat a_PostAlign;
    float32StickyFormat b_PostAlign;

    uint8 alignShiftAmountA;
    uint8 alignShiftAmountB;
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
    a_PostAlign = align(a_PostUnpack, alignShiftAmountA);
    b_PostAlign = align(b_PostUnpack, alignShiftAmountB);

    //Addition
    float32StickyFormat z_postAdd;
    uint27 aTemp = cast<uint27>(a_PostAlign.mantissaGuardRound << 1 | a_PostAlign.sticky);
    uint27 bTemp = cast<uint27>(b_PostAlign.mantissaGuardRound << 1 | b_PostAlign.sticky);

    uint28 sumTemp = cast<uint28>(a_PostAlign.sign == b_PostAlign.sign ? aTemp + bTemp :
                        (aTemp >= bTemp ? aTemp - bTemp : bTemp - aTemp));
    z_postAdd.sign =  a_PostAlign.sign == b_PostAlign.sign ? a_PostAlign.sign :
                        (aTemp >= bTemp ? a_PostAlign.sign : b_PostAlign.sign);

    bool shiftSum = sumTemp >> 27 == 1;
    z_postAdd.exponent = a_PostAlign.exponent + cast<uint1>(shiftSum);
    z_postAdd.mantissaGuardRound = shiftSum ? cast <uint26> (sumTemp >> 2) : cast <uint26> (sumTemp >> 1);
    z_postAdd.sticky = shiftSum ? cast <uint1> (sumTemp >> 1) | cast <uint1> (sumTemp) : cast <uint1> (sumTemp);

    // Shift left to normalize small mantissa values
    float32StickyFormat z_postNormalize = addNormalizeOutput1(z_postAdd);

    //Rounding
    float32ExpandedFormat z_postRounding;

    bool guard = ((z_postNormalize.mantissaGuardRound >> 1) & 1) == 1;
    bool round = (z_postNormalize.mantissaGuardRound & 1) == 1;
    bool m0 = ((z_postNormalize.mantissaGuardRound >> 2) & 1) == 1;
    bool sticky = z_postNormalize.sticky == 1;

    bool roundUp = guard && (round || sticky || m0);
    z_postRounding.mantissa = cast<decltype(z_postRounding.mantissa)>((z_postNormalize.mantissaGuardRound >> 2) + cast<uint1>(roundUp));
    uint1 exp_inc = roundUp && (z_postNormalize.mantissaGuardRound >> 2 == 0xffffff) ? 1 : 0;
    z_postRounding.exponent = z_postNormalize.exponent + exp_inc;
    z_postRounding.sign = z_postNormalize.sign;

    //Packing
    float32PackedFormat zOutput;

    //Handle special cases
    specialCaseFlags a_flags = getSpecialCases<Denorm::Off>(a);
    specialCaseFlags b_flags = getSpecialCases<Denorm::Off>(b);
    bool justAInf = a_flags.inf && !b_flags.inf;
    bool justBInf = !a_flags.inf && b_flags.inf;
    bool AandBInf = a_flags.inf && b_flags.inf;
    bool specialCaseSignsEqual = a.sign == b.sign;

    bool specialCaseNaN = a_flags.nan || b_flags.nan || (AandBInf && !specialCaseSignsEqual);
    bool specialCaseZero = a_flags.zero && b_flags.zero;
    bool specialCaseInf = justAInf || justBInf || (AandBInf && specialCaseSignsEqual);
    //Careful that you check for specialCaseZero first!
    bool specialCaseReturnA = b_flags.zero;
    bool specialCaseReturnB = a_flags.zero;
    uint1 specialCaseSign = justAInf ? a_PostUnpack.sign :
                            justBInf ? b_PostUnpack.sign :
                            a_PostUnpack.sign & b_PostUnpack.sign;

    bool positiveZero = z_postRounding.exponent == -126 && z_postRounding.mantissa == 0;
    bool zeroOut = z_postRounding.exponent == -126 && ((z_postRounding.mantissa >> 23) & 1) == 0;
    bool overflow = z_postRounding.exponent > 127;

    zOutput.mantissa = specialCaseNaN ? 1 << 22 :
                       (specialCaseZero || specialCaseInf) ? 0 :
                       specialCaseReturnA ? a.mantissa :
                       specialCaseReturnB ? b.mantissa:
                       (overflow || zeroOut) ? 0 :
                       cast<decltype(zOutput.mantissa)>(z_postRounding.mantissa);
    zOutput.exponent = (specialCaseNaN || specialCaseInf) ? 255 :
                       specialCaseZero ? 0 :
                       specialCaseReturnA ? a.exponent :
                       specialCaseReturnB ? b.exponent :
                       (positiveZero || zeroOut) ? 0 :
                       overflow ? 255 :
                       cast<decltype(zOutput.exponent)>(z_postRounding.exponent + 127);
    zOutput.sign = (specialCaseInf || specialCaseZero) ? specialCaseSign :
                   specialCaseReturnA ? a.sign :
                   specialCaseReturnB ? b.sign :
                   positiveZero ? 0 : z_postRounding.sign;

    return cast<float32> (zOutput);
}

//| Negate input.
inline float32 neg(float32 x)
{
    float32PackedFormat binary = cast<float32PackedFormat>(x);
    binary.sign = ~binary.sign;
    return cast<float32>(binary);
}

//| Subtract `in2` from `in1`.
inline float32 float32_sub_denormoff(float32 in1, float32 in2)
{
    return float32_add_denormoff(in1, neg(in2));
}