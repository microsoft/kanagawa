// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// [Fixed-point arithmetic](https://en.wikipedia.org/wiki/Fixed-point_arithmetic).
module numeric.fixed
    { fixed
    , round
    , ceil
    , floor
    , trunc
    , abs
    , min
    , max
    , dim
    , reciprocal
    , log2
    , log10
    , log
    , sin
    , cos
    , exp2
    , exp
    , sqrt
    , invsqrt
    , pow
    }

import data.bits
import data.optional
import numeric.fixed.internal as internal
import numeric.int.operator as intop

//| Fixed-point representation.
template<auto I /*< Integer bit-width.*/, auto F /*< Fractional bit-width.*/>
struct fixed
{
    int<I + F> value;
}

//| Internal function which conditionally performs a `checked_cast` on valid source values.
// Valid values must be representable by the type: fixed<I, F>.
template<auto I, auto F>
inline optional<fixed<I, F>> checked_cast_opt_fixed(auto src, bool is_valid)
{
    const auto fn = lift_optional(checked_cast<int<I + F>>);

    auto opt_result = fn(make_optional(is_valid, src));

    return make_optional(opt_result.is_valid, cast<fixed<I, F>>(opt_result.value));
}

//| Round half up.
//
// #### __Examples__
//
//     >>> round(-23.5)
//     -23
//
//     >>> round(23.5)
//     24
template<auto I, auto F>
inline optional<fixed<I, F>> round(fixed<I, F> x)
{
    auto value = x.value;
    optional<fixed<I, F>> result;

    result.value.value = cast<decltype(result.value.value)>((((value >> (F-1)) + 1) >> 1) << F); // error checking below
    result.is_valid = (value < 0) || (result.value.value >= 0);
    return result;
}

//| Ceiling.
template<auto I, auto F>
inline optional<fixed<I, F>> ceil(fixed<I, F> x)
{
    auto value = x.value;
    optional<fixed<I, F>> result;
    int<I> integ = value >> F;
    uint<F> frac = static_cast(value);

    result.value.value = cast<decltype(result.value.value)>((frac == 0 ? integ : integ + 1) << F); // error checking below
    result.is_valid = (value < 0) || (result.value.value >= 0);
    return result;
}

template<auto I, auto F>
inline fixed<I, F> floor(fixed<I, F> x)
{
    return { (x.value >> F) << F };
}

//| Truncate.
template<auto I, auto F>
inline fixed<I, F> trunc(fixed<I, F> x)
{
    const auto value = x.value;
    const int<I> i = value >> F;
    const uint<F> f = static_cast(value);
    return { (i + ((i < 0 && f > 0) ? 1 : 0)) << F };
}

//| Return the absolute value of x.
template<auto I, auto F>
inline optional<fixed<I, F>> abs(fixed<I, F> x)
{
    optional<fixed<I, F>> result;

    result.value.value = cast<decltype(result.value.value)>(x.value < 0 ? -x.value : x.value);
    result.is_valid = result.value.value >= 0;
    return result;
}

//| Return the smaller of x and y.
template<auto I, auto F>
inline fixed<I, F> min(fixed<I, F> x, fixed<I, F> y)
{
    return y.value < x.value ? y : x;
}

//| Return the larger of x and y.
template<auto I, auto F>
inline fixed<I, F> max(fixed<I, F> x, fixed<I, F> y)
{
    return y.value > x.value ? y : x;
}

//| Return the positive difference between x and y, i.e. `max(x - y, 0)`.
template<auto I, auto F>
inline optional<fixed<I, F>> dim(fixed<I, F> x, fixed<I, F> y)
{
    optional<fixed<I, F>> result;

    result.value.value = cast<decltype(result.value.value)>(y.value < x.value ? x.value - y.value : 0);
    result.is_valid = result.value.value >= 0;
    return result;
}

template<auto I, auto F>
inline optional<fixed<I, F>> reciprocal(fixed<I, F> x)
{
    auto value = x.value;
    optional<fixed<I, F>> result;

    if (value == 0)
    {
        result.is_valid = false;
    }
    else
    {
        const bool is_negative = value < 0;
        const uint<I + F> absValue = intop::abs(value);

        // convert to floating point
        const auto leadingOneIndex = highest_one<uint<I + F>>(absValue);
        const auto shift = leadingOneIndex.value - F;
        const uint<1 + F> significand = static_cast((absValue << (I + F - leadingOneIndex.value)) >> I);

        // approximate by table lookup
        const auto approxFbitCount = 10;
        const uint<1 + approxFbitCount> approxValue = internal::rcp_lookup<F, approxFbitCount>(static_cast(significand));

        // 1st refinement
        const auto refinedFbitCount_0 = 20;
        const uint<1 + refinedFbitCount_0> refinedValue_0 =
            internal::rcp_refine<F, approxFbitCount, refinedFbitCount_0>(significand, approxValue);

        // 2nd refinement
        const auto refinedFbitCount_1 = I + F + 1;
        const uint<1 + refinedFbitCount_1> refinedValue_1 =
            internal::rcp_refine<F, refinedFbitCount_0, refinedFbitCount_1>(significand, refinedValue_0);

        // convert to fixed point
        const auto shiftedFbitCount = F + 1;
        assert(refinedFbitCount_1 - shiftedFbitCount + shift >= 0);
        const auto shiftedValue = refinedValue_1 >> (refinedFbitCount_1 - shiftedFbitCount + shift);

        // round
        const auto roundedValue = (shiftedValue + (shiftedValue & 1)) >> 1;

        const auto signedValue = is_negative ? -roundedValue : roundedValue;

        const int<I + F> minValue = -(1 << (I + F - 1));
        const int<I + F> maxValue = (1 << (I + F - 1)) - 1;

        result = checked_cast_opt_fixed<I, F>(signedValue, (minValue <= signedValue) && (signedValue <= maxValue));
    }

    return result;
}

const auto LOG2_TABLE_LG_SIZE = 9;
const auto LOG2_C2_FBIT = 8;
const auto LOG2_C1_FBIT = 12;
const auto LOG2_C0_FBIT = 20;

struct log2_coefficients
{
    int9 c2;
    int14 c1;
    int21 c0;
}

template<auto I, auto F>
inline fixed<I, F> log2_calc(fixed<I, F> x)
{
    auto value = x.value;

    // convert to floating point
    const auto leadingOneIndex = highest_one(value);
    // shift is the integer part of value
    // If shift > 0, value > 1
    // If shift < 0, value < 1
    auto shift = leadingOneIndex.value - F;

    // significand is the fraction part of value
    // significand is fixed<1, F + 4> = fixed<1, 18 + 4> = fixed<1, 22>
    uint<1 + F + 4> significand = static_cast((value << (I - shift)) >> (I - 4));

    // Index for lookup table of Least Square (LS) coefficients with base modified
    uint9 index = static_cast(((significand - (1 << (F + 4))) * (1 << LOG2_TABLE_LG_SIZE)) >> (F + 4));
    // xb is fixed<1, F + 4> = fixed<1, 22>
    auto xb = (1 << (F + 4)) + ((1 << (F + 4)) >> LOG2_TABLE_LG_SIZE) * index;

    const auto differenceFbit = F + 4;
    // difference is fixed<1, F + 4> = fixed<1, 22>
    auto difference = significand - xb;

    // logarithm 2-based coefficient by Least Square (LS) method
    const auto coefficients = log2_coefficient_table[index];
    // // c2 is fixed<1, 8>
    const auto c2 = coefficients.c2;
    // // c1 is fixed<2, 10>
    const auto c1 = coefficients.c1;
    // // c0 is fixed<1, 19>
    const auto c0 = coefficients.c0;

    // calculate the fractional part of result
    const auto calcFbit = LOG2_C2_FBIT + differenceFbit;
    // calc1 and calc2 is fixed<1 + 1, 8 + 22> = fixed<2, 30>
    int<2 + calcFbit> calc1 = static_cast(c2 * difference);
    int<2 + calcFbit> calc2 = c1 << (calcFbit - LOG2_C1_FBIT);
    // calc is fixed<2, 30>
    uint<I + calcFbit> calc = static_cast(calc1 + calc2);
    const auto resultFracFbit = calcFbit + differenceFbit;
    // resultFrac is fixed<2 + 1, 30 + 22> = fixed<3, 52>
    auto resultFrac = calc * difference + (c0 << (resultFracFbit - LOG2_C0_FBIT));

    // summation of integer part and fraction part
    auto resultTmp1 = (shift << resultFracFbit) + resultFrac;

    // get result with rounding
    int<I + F> result = static_cast(((resultTmp1 >> (resultFracFbit - F - 1)) + 1) >> 1);
    return cast<fixed<I, F>>(result);
}

template<auto M, auto I, auto F>
inline fixed<I, F> logbase_calc(fixed<I, F> x)
{
    // calculate log2(x)
    auto log2ofxFixed = log2_calc(x);
    auto log2ofxValue = cast<int<I + F>>(log2ofxFixed.value);

    const auto mulvalueFbit = 22;
    const auto resultTmpFbit = F + mulvalueFbit;
    // get result value with rounding
    int<I + F> resultTmp = static_cast((((log2ofxValue * M) >> (resultTmpFbit - F - 1)) + 1) >> 1);
    return cast<fixed<I, F>>(resultTmp);
}

//| Base-2 logarithm.
template<auto I, auto F>
inline optional<fixed<I, F>> log2(fixed<I, F> x)
{
    return make_optional(x.value > 0, log2_calc(x));
}

//| Base-10 logarithm.
template<auto I, auto F>
inline optional<fixed<I, F>> log10(fixed<I, F> x)
{
    return make_optional(x.value > 0, logbase_calc<1262611>(x));
}

//| Base-e logarithm.
template<auto I, auto F>
inline optional<fixed<I, F>> log(fixed<I, F> x)
{
    return make_optional(x.value > 0, logbase_calc<2907270>(x));
}

//| Convert from radian to pi-radian
template<auto I, auto F1, auto F2>
inline int<I + F2> to_pirad(int<I + F1> value)
{
    // invPi = 1/Pi
    const auto oriInvPiFbit = 41;
    const auto oriInvPi = 0xa2f9836e4e;

    const auto invPiFbit = I + F2;
    const auto invPiShiftRight = oriInvPiFbit - I - F2;
    static assert(invPiShiftRight > 0);
    const uint<I + F2> invPi =  (oriInvPi >> invPiShiftRight) + ((oriInvPi >> (invPiShiftRight - 1)) & 1);

    const auto resultTmpFbit = F1 + invPiFbit;
    auto resultTmp = value * invPi;
    int<I + F2> result = static_cast((resultTmp >> (resultTmpFbit - F2)) + ((resultTmp >> (resultTmpFbit - F2 - 1)) & 1));
    return result;
}

template<auto I, auto F>
inline fixed<I, F> sin(fixed<I, F> x)
{
    const auto roundFbit = 4;

    int<I + F + roundFbit> value = to_pirad<I, F, F + roundFbit>(x.value);
    int<I + F> result = internal::sin_pirad<F + roundFbit, F>(static_cast(value));
    return cast<fixed<I, F>>(result);
}

template<auto I, auto F>
inline fixed<I, F> cos(fixed<I, F> x)
{
    const auto roundFbit = 4;
    int<I + F + roundFbit> value = to_pirad<I, F, F + roundFbit>(x.value);
    int<I + F> result = internal::cos_pirad<F + roundFbit, F>(static_cast(value));
    return cast<fixed<I, F>>(result);
}

const auto EXP2_S0_TABLE_LG_SIZE = 5;
const auto EXP2_S1_TABLE_LG_SIZE = 5;
const auto EXP2_S2_TABLE_LG_SIZE = 5;
const auto EXP2_S0_FBIT = 40;
const auto EXP2_S1_FBIT = 40;
const auto EXP2_C0_FBIT = 40;
const auto EXP2_C1_FBIT = 23;
const auto EXP2_C2_FBIT = 10;

struct exp2_coefficients
{
    uint12 c1;
    uint41 c0;
}

template<auto F1, auto F2>
inline uint<1 + F2> exp2_lookup(uint<F1> fraction)
{
    const auto totalIndexBit = EXP2_S0_TABLE_LG_SIZE + EXP2_S1_TABLE_LG_SIZE + EXP2_S2_TABLE_LG_SIZE;

    static assert(F1 >= totalIndexBit);
    uint<totalIndexBit> indexRef = fraction >> (F1 - totalIndexBit);
    const uint<F1 - totalIndexBit> residual = static_cast(fraction);

    const uint<EXP2_S0_TABLE_LG_SIZE> indexSection0 = indexRef >> (EXP2_S1_TABLE_LG_SIZE + EXP2_S2_TABLE_LG_SIZE);
    const uint<EXP2_S1_TABLE_LG_SIZE> indexSection1 = static_cast(indexRef >> EXP2_S2_TABLE_LG_SIZE);
    const uint<EXP2_S2_TABLE_LG_SIZE> indexSection2 = static_cast(indexRef);

    const auto section0 = (1 << EXP2_S0_FBIT) | exp2_section0_coefficient_table[indexSection0];
    const auto section1 = (1 << EXP2_S1_FBIT) | exp2_section1_coefficient_table[indexSection1];

    const auto section2Coefficients = exp2_section2_coefficient_table[indexSection2];

    const auto value2Fbit = EXP2_C2_FBIT + F1;
    const auto c2 = 0xF6;
    const auto value2 = c2 * residual;

    const auto value1Fbit = EXP2_C0_FBIT;
    static assert(value2Fbit >= EXP2_C1_FBIT);
    static assert(value2Fbit + F1 >= value1Fbit);
    const auto c1 = 0x58B90C + section2Coefficients.c1;
    const auto value1 = ((value2 + (c1 << (value2Fbit - EXP2_C1_FBIT))) * residual) >> (value2Fbit + F1 - value1Fbit);

    const auto section2Fbit = value1Fbit;
    static assert(EXP2_C0_FBIT >= section2Fbit);
    const auto section2 = value1 + (section2Coefficients.c0 >> (EXP2_C0_FBIT - section2Fbit));

    const auto approxFbit = 38;
    static assert(EXP2_S0_FBIT + EXP2_S1_FBIT >= approxFbit);
    const auto approx = (section0 * section1) >> (EXP2_S0_FBIT + EXP2_S1_FBIT - approxFbit);

    static assert((approxFbit + section2Fbit) >= F2);
    return (approx * section2) >> (approxFbit + section2Fbit - F2);
}

template<auto I, auto F1, auto F2>
inline optional<fixed<I, F2>> to_fixed(int<I> exponent, uint<1 + F1> fraction)
{
    const auto fixedValueFbit = F2 + 1;
    assert(F1 >= fixedValueFbit + exponent);
    const auto fixedValue = fraction >> (F1 - fixedValueFbit - exponent);

    const auto roundedValueFbit = F2;
    const auto roundedValue = (fixedValue >> 1) + (fixedValue & 1);

    static assert(I + F2 >= 1);
    return checked_cast_opt_fixed<I, F2>(roundedValue, (roundedValue >> (I + F2 - 1)) == 0);
}

//| Base-2 exponent.
template<auto I, auto F>
inline optional<fixed<I, F>> exp2(fixed<I, F> x)
{
    const auto value = x.value;

    const int<I> exponent = value >> F;
    const uint<F> fraction = static_cast(value);

    const auto fractionExp2Bit = I + F;
    const uint<1 + fractionExp2Bit> fractionExp2 = exp2_lookup<F, fractionExp2Bit>(fraction);

    return to_fixed<I, fractionExp2Bit, F>(exponent, fractionExp2);
}

//| Base-e exponent.
template<auto I, auto F>
inline optional<fixed<I, F>> exp(fixed<I, F> x)
{
    const auto value = x.value;

    const auto log2eFbit = 38;
    const auto log2e = 0x5C551D94AE;

    static assert(F + log2eFbit > 19);
    const auto expFbit = F + log2eFbit - 19;
    const auto exp = (value * log2e) >> 19;

    const int<I> exponent = static_cast(exp >> expFbit);
    const uint<expFbit> fraction = static_cast(exp);

    const auto fractionExpBit = I + F;
    const uint<1 + fractionExpBit> fractionExp = exp2_lookup<expFbit, fractionExpBit>(fraction);

    return to_fixed<I, fractionExpBit, F>(exponent, fractionExp);
}

const auto INVSQRT_TABLE_LG_SIZE = 5;
const auto INVSQRT_C0_FBIT = 11;
const auto INVSQRT_C1_FBIT = 6;

struct invsqrt_coefficients
{
    int8 c1;
    uint12 c0;
}

// input: [1, 2) without the implicit leading one
// output: ~(0.5, 1]
template<auto F1, auto F2>
inline uint<1 + F2> invsqrt_lookup(int1 exponent, uint<F1> fraction)
{
    static assert(F1 >= F2);
    static assert(F2 >= INVSQRT_TABLE_LG_SIZE);
    const uint<INVSQRT_TABLE_LG_SIZE> index = fraction >> (F1 - INVSQRT_TABLE_LG_SIZE);
    uint<F2 - INVSQRT_TABLE_LG_SIZE> delta = static_cast(fraction >> (F1 - F2));

    delta = delta >> cast<uint1>(exponent);

    const auto coefficients = (exponent == 0) ? invsqrt_exp_zero_table[index] : invsqrt_exp_m1_table[index];

    const auto resultFbit = INVSQRT_C1_FBIT + F2;
    static assert(resultFbit >= INVSQRT_C0_FBIT);
    const auto result = (coefficients.c0 << (resultFbit - INVSQRT_C0_FBIT)) +  coefficients.c1 * delta;

    static assert(resultFbit >= F2);
    return result >> (resultFbit - F2);
}

template<auto F1, auto F2, auto F3>
inline uint<1 + F3> invsqrt_refine(uint<1 + F1> significand, uint<1 + F2> approximation)
{
    static assert(F3 >= 2);
    const auto truncatedSignificandFbit = F3 - 2;
    static assert(F1 >= truncatedSignificandFbit);
    const auto truncatedSignificand = significand >> (F1 - truncatedSignificandFbit);

    const auto temp0FbitCount = truncatedSignificandFbit + F2;
    const auto temp0 =  truncatedSignificand * approximation;

    const auto temp1FbitCount = temp0FbitCount + F2;
    const auto temp1 = temp0 * approximation;
    const auto temp2FbitCount = temp1FbitCount;
    const auto temp2 = ((3 << temp1FbitCount) - temp1) >> 1;

    const auto refinedFbitCount = F2 + temp2FbitCount;
    const auto refinedValue = approximation * temp2;

    static assert(refinedFbitCount >= F3);
    return refinedValue >> (refinedFbitCount - F3);
}

// input: [1, 2), output: ~(0.5, 1]
// input: 0, output: 0
template<auto F1, auto F2>
inline uint<1 + F2> invsqrt_internal(uint1 exponent, uint<1 + F1> significand)
{
    // approximate by table lookup
    const auto approxFbitCount = 9;
    const uint<1 + approxFbitCount> approxValue = invsqrt_lookup<F1, approxFbitCount>(static_cast(exponent), static_cast(significand));

    const auto fixedValue = significand >> cast<uint1>(exponent);

    // 1st refinement
    const auto refinedFbitCount_0 = 17;
    const uint<1 + refinedFbitCount_0> refinedValue_0 = invsqrt_refine<F1, approxFbitCount, refinedFbitCount_0>(fixedValue, approxValue);

    // 2nd refinement
    const uint<1 + F2> refinedValue_1 = invsqrt_refine<F1, refinedFbitCount_0, F2>(fixedValue, refinedValue_0);

    return refinedValue_1;
}

//| Return the reciprocal of the square root of x.
template<auto I, auto F>
inline optional<fixed<I, F>> invsqrt(fixed<I, F> x)
{
    const auto value = x.value;
    optional<fixed<I, F>> result;
    result.is_valid = value > 0;

    const auto leadingOneIndex = highest_one(value);
    int6 exponent = static_cast(leadingOneIndex.value - F);
    const auto significandFbit = I + F - 1;
    const uint<I + F> significand = static_cast(value << (I + F - 1 - leadingOneIndex.value));

    // calculate 1/sqrt(x)
    const auto invsqrtValueFbit = I / 2 + F + 1;
    const uint<1 + invsqrtValueFbit> invsqrtValue = invsqrt_internal<significandFbit, invsqrtValueFbit>(static_cast(exponent), significand);

    const auto shiftedValueFbit = F + 1;
    exponent = (exponent >> 1) + (exponent & 1);
    assert(invsqrtValueFbit - shiftedValueFbit + exponent >= 0);
    const auto shiftedValue = invsqrtValue >> (invsqrtValueFbit - shiftedValueFbit + exponent);

    result.value.value = (shiftedValue >> 1) + (shiftedValue & 1);
    return result;
}

//| Return the square root of x. This function provides better accuracy than
// calling `invsqrt` and then multiplying by x.
template<auto I, auto F>
inline optional<fixed<I, F>> sqrt(fixed<I, F> x)
{
    const auto value = x.value;
    optional<fixed<I, F>> result;
    result.is_valid = value >= 0;

    const auto leadingOneIndex = highest_one(value);
    int6 exponent = static_cast(leadingOneIndex.value - F);
    const auto significandFbit = I + F - 1;
    const uint<I + F> significand = static_cast(value << (I + F - 1 - leadingOneIndex.value));

    // compute 1/sqrt(x)
    const auto invsqrtValueFbit = I + F;
    const uint<1 + invsqrtValueFbit> invsqrtValue = invsqrt_internal<significandFbit, invsqrtValueFbit>(static_cast(exponent), significand);

    // sqrt(x) = value * 1/sqrt(x)
    const auto sqrtFbit = invsqrtValueFbit + F;
    const auto sqrt = value * invsqrtValue;

    const auto shiftedValueFbit = F + 1;
    const auto halfExponent = (exponent >> 1) + (exponent & 1);
    assert(sqrtFbit - shiftedValueFbit + halfExponent >= 0);
    const auto shiftedValue = sqrt >> (sqrtFbit - shiftedValueFbit + halfExponent);

    result.value.value = (shiftedValue >> 1) + (shiftedValue & 1);
    return result;
}

const auto LOG2_40_TABLE_LG_SIZE = 5;
const auto LOG2_40_C3_FBIT = 25;
const auto LOG2_40_C2_FBIT = 35;
const auto LOG2_40_C1_FBIT = 45;
const auto LOG2_40_C0_FBIT = 55;

struct log2_40_coefficients
{
    uint56 c0;
    uint46 c1;
    int36 c2;
    uint25 c3;
    uint46 c1_base;
    uint35 c2_base;
    uint24 c3_base;
}

template<auto C3, auto C2, auto C1, auto C0>
struct log2_40_section_coefficients
{
    uint<C3> c3;
    uint<C2> c2;
    uint<C1> c1;
    uint<C0> c0;
}

struct log2_40_row
{
    log2_40_section_coefficients<20, 30, 40, 50> coeff_000000;
    log2_40_section_coefficients<21, 31, 41, 52> coeff_000001;
    log2_40_section_coefficients<21, 32, 42, 53> coeff_0001;
    log2_40_section_coefficients<21, 31, 42, 53> coeff_0010;
    log2_40_section_coefficients<21, 31, 41, 54> coeff_0011;
    log2_40_section_coefficients<21, 32, 42, 54> coeff_010;
    log2_40_section_coefficients<21, 31, 42, 55> coeff_011;
    log2_40_section_coefficients<20, 31, 42, 55> coeff_100;
    log2_40_section_coefficients<20, 31, 41, 55> coeff_101;
    log2_40_section_coefficients<20, 30, 41, 55> coeff_110;
    log2_40_section_coefficients<19, 30, 41, 55> coeff_1110;
    log2_40_section_coefficients<17, 28, 39, 56> coeff_11111;
}

template<auto F1, auto F2, auto IndexWidth>
struct log2_40_paramPacked
{
    uint<F1 - F2 - IndexWidth> residual;
    uint<IndexWidth> index;
    uint<F2> msbits;
}

template<auto F1, auto F2, auto IndexWidth>
struct log2_40_residual_and_index
{
    uint<F1 - (F2 + IndexWidth)> residual;
    uint<LOG2_40_TABLE_LG_SIZE> index;
}

// input: [1, 2) without the implicit leading one
// output: [0, 1]
template<auto F1, auto F2>
inline uint<F2> log2_40_lookup(uint<F1> fraction)
{
    const auto indexWidth = LOG2_40_TABLE_LG_SIZE;
    const auto packedFraction_bit3_32index = cast<log2_40_paramPacked<F1, 3, indexWidth>>(fraction);
    const auto packedFraction_bit4_32index = cast<log2_40_paramPacked<F1, 4, indexWidth>>(fraction);
    const auto packedFraction_bit5_32index = cast<log2_40_paramPacked<F1, 5, indexWidth>>(fraction);
    const auto packedFraction_bit6_32index = cast<log2_40_paramPacked<F1, 6, indexWidth>>(fraction);
    const auto packedFraction_bit10_32index = cast<log2_40_paramPacked<F1, 10, indexWidth>>(fraction);
    const auto packedFraction_bit4_16index = cast<log2_40_paramPacked<F1, 4, (indexWidth - 1)>>(fraction);
    const auto packedFraction_bit5_16index = cast<log2_40_paramPacked<F1, 5, (indexWidth - 1)>>(fraction);
    const auto packedFraction_bit6_16index = cast<log2_40_paramPacked<F1, 6, (indexWidth - 1)>>(fraction);

    const uint3 fraction_msbit_0_2 = packedFraction_bit3_32index.msbits;
    const uint1 fraction_msbit_3 = static_cast(packedFraction_bit4_32index.msbits);
    const uint2 fraction_msbit_3_4 = static_cast(packedFraction_bit5_32index.msbits);
    const uint1 fraction_msbit_4 =  static_cast(packedFraction_bit5_32index.msbits);
    const uint2 fraction_msbit_4_5 =  static_cast(packedFraction_bit6_32index.msbits);

    // extract the residual and the table index
    const log2_40_residual_and_index<F1, 5, (indexWidth - 1)> ir_bbb = mux(fraction_msbit_4,
        { packedFraction_bit6_16index.residual, ((0 << 4) | packedFraction_bit6_16index.index)},
        { packedFraction_bit5_16index.residual, ((1 << 4) | packedFraction_bit5_16index.index)});

    const auto sel_coefficient_0000 = mux(fraction_msbit_4_5, 0, 1, 1, 1);
    const log2_40_residual_and_index<F1, 5, (indexWidth - 1)> ir_0000x = mux(sel_coefficient_0000,
        { packedFraction_bit6_32index.residual, packedFraction_bit6_32index.index},
        { ir_bbb.residual, ir_bbb.index});

    const log2_40_residual_and_index<F1, 4, indexWidth> ir_000xx = mux(fraction_msbit_3,
        { ir_0000x.residual, ir_0000x.index},
        { packedFraction_bit4_32index.residual, packedFraction_bit4_32index.index});

    const log2_40_residual_and_index<F1, 4, (indexWidth - 1)> ir_aaa = mux(fraction_msbit_3,
        { packedFraction_bit4_16index.residual, ((0 << 4) | packedFraction_bit4_16index.index)},
        { packedFraction_bit5_16index.residual, ((1 << 4) | packedFraction_bit5_16index.index)});

    const auto sel_coefficient_111 = mux(fraction_msbit_3_4, 0, 0, 0, 1);
    const log2_40_residual_and_index<F1, 4, (indexWidth - 1)> ir_111xx = mux(sel_coefficient_111,
        { ir_aaa.residual, ir_aaa.index},
        { packedFraction_bit5_32index.residual, packedFraction_bit5_32index.index});

    const auto sel_coefficient = mux(fraction_msbit_0_2, 0, 1, 2, 2, 2, 2, 2, 3);
    const log2_40_residual_and_index<F1, 3, indexWidth> ir = mux(sel_coefficient,
        { ir_000xx.residual, ir_000xx.index},
        { packedFraction_bit4_32index.residual, packedFraction_bit4_32index.index},
        { packedFraction_bit3_32index.residual, packedFraction_bit3_32index.index},
        { ir_111xx.residual, ir_111xx.index});

    const auto index = ir.index;

    // in the last one interval, the base of residual would be changed from 0 to (1 << F1).
    // residual_new = (residual - (1 << F1)), and it's equal to the input (fraction) because of 2's complement feature.
    const int<1 + F1 - (3 + LOG2_40_TABLE_LG_SIZE)> residual = static_cast((packedFraction_bit10_32index.msbits == 0x3FF) ? fraction : ir.residual);

    // extract coefficient
    const auto row = log2_40_coefficient_table[index];

    const log2_40_coefficients coefficient_0000x = {
        mux(sel_coefficient_0000, row.coeff_000000.c0, row.coeff_000001.c0),
        mux(sel_coefficient_0000, row.coeff_000000.c1, row.coeff_000001.c1),
        mux(sel_coefficient_0000, row.coeff_000000.c2, row.coeff_000001.c2),
        mux(sel_coefficient_0000, row.coeff_000000.c3, row.coeff_000001.c3),
        mux(sel_coefficient_0000, 50003431253580, 47862400339231),
        mux(sel_coefficient_0000, 24051671026, 22036049567),
        mux(sel_coefficient_0000, 15414055, 13490159) };

    const log2_40_coefficients coefficient_000xx = {
        mux(fraction_msbit_3, coefficient_0000x.c0, row.coeff_0001.c0),
        mux(fraction_msbit_3, coefficient_0000x.c1, row.coeff_0001.c1),
        mux(fraction_msbit_3, coefficient_0000x.c2, row.coeff_0001.c2),
        mux(fraction_msbit_3, coefficient_0000x.c3, row.coeff_0001.c3),
        mux(fraction_msbit_3, coefficient_0000x.c1_base, 45198753714340),
        mux(fraction_msbit_3, coefficient_0000x.c2_base, 19651595091),
        mux(fraction_msbit_3, coefficient_0000x.c3_base, 11362618) };

    const log2_40_coefficients coefficient_001xx = {
        mux(fraction_msbit_3, row.coeff_0010.c0, row.coeff_0011.c0),
        mux(fraction_msbit_3, row.coeff_0010.c1, row.coeff_0011.c1),
        mux(fraction_msbit_3, row.coeff_0010.c2, row.coeff_0011.c2),
        mux(fraction_msbit_3, row.coeff_0010.c3, row.coeff_0011.c3),
        mux(fraction_msbit_3, 42815952861623, 40671804989095),
        mux(fraction_msbit_3, 17634217684, 15912262504),
        mux(fraction_msbit_3, 9659955, 8281178) };

    const log2_40_coefficients coefficient_111xx = {
        mux(sel_coefficient_111, row.coeff_1110.c0, row.coeff_11111.c0),
        mux(sel_coefficient_111, row.coeff_1110.c1, row.coeff_11111.c1),
        mux(sel_coefficient_111, row.coeff_1110.c2, row.coeff_11111.c2),
        mux(sel_coefficient_111, row.coeff_1110.c3, row.coeff_11111.c3),
        mux(sel_coefficient_111, 25808623031420, 25380159564698),
        mux(sel_coefficient_111, 6407300684, 6196327603),
        mux(sel_coefficient_111, 2117766, 2018142) };

    const log2_40_coefficients coefficient = {
        mux(fraction_msbit_0_2, coefficient_000xx.c0, coefficient_001xx.c0, row.coeff_010.c0, row.coeff_011.c0, row.coeff_100.c0, row.coeff_101.c0, row.coeff_110.c0, coefficient_111xx.c0),
        mux(fraction_msbit_0_2, coefficient_000xx.c1, coefficient_001xx.c1, row.coeff_010.c1, row.coeff_011.c1, row.coeff_100.c1, row.coeff_101.c1, row.coeff_110.c1, coefficient_111xx.c1),
        mux(fraction_msbit_0_2, coefficient_000xx.c2, coefficient_001xx.c2, row.coeff_010.c2, row.coeff_011.c2, row.coeff_100.c2, row.coeff_101.c2, row.coeff_110.c2, coefficient_111xx.c2),
        mux(fraction_msbit_0_2, coefficient_000xx.c3, coefficient_001xx.c3, row.coeff_010.c3, row.coeff_011.c3, row.coeff_100.c3, row.coeff_101.c3, row.coeff_110.c3, coefficient_111xx.c3),
        mux(fraction_msbit_0_2, coefficient_000xx.c1_base, coefficient_001xx.c1_base, 37021771163748, 33928568362275, 31312389604374, 29070786774199, 27128688287596, coefficient_111xx.c1_base),
        mux(fraction_msbit_0_2, coefficient_000xx.c2_base, coefficient_001xx.c2_base, 13184324960, 11073245616, 9431408587, 8129389327, 7079492349, coefficient_111xx.c2_base),
        mux(fraction_msbit_0_2, coefficient_000xx.c3_base, coefficient_001xx.c3_base, 6233743, 4799857, 3774079, 3020959, 2455602, coefficient_111xx.c3_base) };

    const uint56 c0 = coefficient.c0;
    const uint46 c1 = static_cast(coefficient.c1 + coefficient.c1_base);
    const int36 c2 = static_cast(-(coefficient.c2 + coefficient.c2_base));
    const uint25 c3 = static_cast(coefficient.c3 + coefficient.c3_base);

    // compute look-up result
    const auto value3Fbit = F1 + LOG2_40_C3_FBIT;
    const auto value3 = residual * c3;

    const auto value2Fbit = F1 + value3Fbit;
    const auto value2 = (value3 + (c2 << (value3Fbit - LOG2_40_C2_FBIT))) * residual;

    const auto value1Fbit = F2;
    static assert(value2Fbit + F1 >= F2);
    const auto value1 = ((value2 + (c1 << (value2Fbit - LOG2_40_C1_FBIT))) * residual) >> (value2Fbit + F1 - F2);

    static assert(F2 >= LOG2_40_C0_FBIT);
    const auto value0 = value1 + (c0 << (F2 - LOG2_40_C0_FBIT));

    return value0;
}

//| Raise `base` to the power of `expo`.
template<auto I, auto F>
inline optional<fixed<I, F>> pow(fixed<I, F> base, fixed<I, F> expo)
{
    const auto baseValue = base.value;
    const auto expoValue = expo.value;
    optional<fixed<I, F>> result;

    // detect invalid input pattern
    bool sign = baseValue < 0;
    const uint<F> expoFraction = static_cast(expoValue);
    const bool isValid = !sign || (sign && expoFraction == 0);

    // normalize base value
    const auto baseAbsValue = sign ? -baseValue : baseValue;
    const auto leadingOneIndex = highest_one(baseAbsValue);
    const int6 exponent = static_cast(leadingOneIndex.value - F);

    const auto significandFbit = I + F - 1;
    const uint<1 + significandFbit> significand = static_cast(baseAbsValue << (I + F - 1 - leadingOneIndex.value));

    // log2(base)
    const auto log2Fbit = 55;
    const auto log2Fraction = log2_40_lookup<significandFbit, (log2Fbit + 1)>(static_cast(significand));
    const auto roundLog2Frcation = (log2Fraction >> 1) + (log2Fraction & 1);
    const int<6 + log2Fbit> log2Value = static_cast((exponent << log2Fbit) + roundLog2Frcation);

    // expo * log2(base)
    const auto productFbit = F + log2Fbit;
    const auto product = log2Value * expoValue;
    const int<6 + (I - 1)> productExponent = static_cast(product >> productFbit);

    const auto productFractionBit = 38;
    const uint<productFractionBit + 1> productFraction = static_cast(product >> (productFbit - (productFractionBit + 1)));
    const auto roundProductFraction = (productFraction >> 1) + (productFraction & 1);

    const auto fractionExp2Bit = I + F + 1;
    const auto fractionExp2 = exp2_lookup<productFractionBit, fractionExp2Bit>(static_cast(roundProductFraction));

    // convert to fixed point
    const auto truncatedFbitCount = F + 1;
    const auto shiftedValue = (fractionExp2 << truncatedFbitCount) >> (truncatedFbitCount - productExponent);
    const auto truncatedValue = shiftedValue >> (fractionExp2Bit - truncatedFbitCount);

    const uint<I + 1 + F> roundedValue = static_cast((truncatedValue >> 1) + (truncatedValue & 1));
    const bool isExpoEvenInteger = ((expoValue >> F) & 1) == 0;
    sign = isExpoEvenInteger ? false : sign;

    const int<I + 2 + F> signedValue = sign ? -roundedValue : roundedValue;
    result.value.value = signedValue;

    const int<I + F> minValue = -(1 << (I + F - 1));
    const int<I + F> maxValue = (1 << (I + F - 1)) - 1;
    result.is_valid = (minValue <= signedValue) && (signedValue <= maxValue) && isValid;

    return result;
}

const memory<log2_40_row, (1 << LOG2_40_TABLE_LG_SIZE)> log2_40_coefficient_table =
{
    {{ 713356, 733639388, 756887875724, 0 }, { 1890652, 1992490128, 2116990800788, 805885395360409 }, { 2053349, 2303514213, 2575664271423, 3151180954111479 }, { 1643636, 1949202059, 2304330794260, 6122193385413145 }, { 1331230, 1663995685, 2073726897485, 8932530057276141 }, { 1989473, 2678178621, 3586484063012, 11598681985408084 }, { 1380983, 2036275188, 2988027315660, 16552768531983802 }, { 988425, 1584239212, 2527823111415, 21075495202192744 }, { 726001, 1256730611, 2166332663052, 25236000428929116 }, { 545195, 1013645678, 1877208337688, 29088026609077388 }, { 322536, 642724586, 1263547155672, 32674177187588548 }, { 94898, 198269436, 402859675408, 35210219994487132 }},
    {{ 687318, 709451049, 732114659692, 25373965252955 }, { 1846386, 1946347979, 2068979858296, 855840773341539 }, { 1979700, 2223019083, 2488004788976, 3246642076973482 }, { 1585018, 1881380868, 2226132729074, 6212355709474644 }, { 1283995, 1606321470, 2003537190011, 9017950892915798 }, { 1912977, 2579501356, 3459978595722, 11760861732543422 }, { 1328677, 1962105856, 2883447724946, 16700225557936320 }, { 951459, 1527089623, 2439926455297, 21210680107491296 }, { 699141, 1211767388, 2091423503923, 25360798968564528 }, { 525212, 977636320, 1812607231752, 29203920897421048 }, { 307363, 613441103, 1207264057664, 32782353224111916 }, { 91758, 191930307, 390076820059, 35235996621243240 }},
    {{ 663565, 685299600, 707365612878, 50735549941741 }, { 1802290, 1900338613, 2021061067325, 905748186478059 }, { 1906589, 2142965828, 2400666403534, 3341928202261923 }, { 1526803, 1813911385, 2148205245427, 6302361908221596 }, { 1237068, 1548930665, 1933577612726, 9103231579459486 }, { 1837428, 2481742009, 3334258876668, 11922537031750084 }, { 1276960, 1888564185, 2779458979372, 16847265448887678 }, { 914874, 1470383624, 2352485222265, 21345514338192972 }, { 672538, 1167126479, 2016872761821, 25485298589255936 }, { 505406, 941866757, 1748293241840, 29319557356211772 }, { 292316, 584339691, 1151214499481, 32890304595807900 }, { 88623, 185600600, 377306633546, 35261760471479984 }},
    {{ 639979, 661183557, 682640699719, 76084766142014 }, { 1758363, 1854461523, 1973234162818, 955607726788879 }, { 1834012, 2063351219, 2313647354053, 3437039970407611 }, { 1468990, 1746791182, 2070546941343, 6392212521413468 }, { 1190448, 1491821418, 1863847035693, 9188372576036410 }, { 1762810, 2384889231, 3209317607878, 12083711011394946 }, { 1225822, 1815643113, 2676056085881, 16993890558213612 }, { 878666, 1414116636, 2265495881907, 21479999708917064 }, { 646187, 1122804811, 1942677870514, 25609500719530872 }, { 485775, 906334865, 1684264458091, 29434937130088276 }, { 277393, 555418847, 1095397030566, 32998032233930728 }, { 85495, 179280296, 364549097043, 35287511557856792 }},
    {{ 616478, 637102792, 657939884921, 101421625911771 }, { 1714605, 1808716200, 1925498880733, 1005419486028356 }, { 1761963, 1984172058, 2226945892343, 3531978018332157 }, { 1411575, 1680017852, 1993156424515, 6481908086015477 }, { 1144132, 1434991888, 1794344336362, 9273374339523298 }, { 1689109, 2288931846, 3085147581480, 12244386770833540 }, { 1175257, 1743335678, 2573234107517, 17140103219429918 }, { 842830, 1358284139, 2178954940207, 21614138020234216 }, { 620086, 1078799349, 1868836288213, 25733406777700824 }, { 466317, 871038541, 1620518987544, 29550061356083680 }, { 262593, 526677077, 1039810212347, 33105537063956320 }, { 82373, 172969376, 351804191766, 35313249893014272 }},
    {{ 593041, 613057220, 633263133267, 126746141291382 }, { 1671014, 1763102141, 1877854958041, 1055183555687313 }, { 1690438, 1905425176, 2140560282952, 3626742979473540 }, { 1354554, 1613589009, 1916032312219, 6571449136217828 }, { 1098117, 1378440252, 1725068399509, 9358237324559116 }, { 1616310, 2193858851, 2961741678313, 12404567380767880 }, { 1125256, 1671635012, 2470988162641, 17285905746415564 }, { 807361, 1302881671, 2092858939080, 21747931058811004 }, { 594233, 1035107091, 1795345497274, 25857018171958500 }, { 447030, 835975709, 1557054953951, 29664931163692784 }, { 247914, 498112908, 984452618121, 33212820005630168 }, { 79257, 166667822, 339071898962, 35338975489574272 }},
    {{ 569658, 589046765, 608610409610, 152058324303630 }, { 1627591, 1717618845, 1830302132715, 1104900026994046 }, { 1619432, 1827107430, 2054488803050, 3721335483811465 }, { 1297924, 1547502288, 1839173231236, 6660836203454791 }, { 1052402, 1322164701, 1656018117172, 9442961983559648 }, { 1544401, 2099659411, 2839092866568, 12564255883598338 }, { 1075812, 1600534347, 2369313424156, 17431300433632274 }, { 772254, 1247904827, 2007204455904, 21881380597552772 }, { 568623, 991725071, 1722203003921, 25980336300473960 }, { 427913, 801144314, 1493870497598, 29779547674938472 }, { 233356, 469724879, 929322832923, 33319881973014384 }, { 76147, 160375615, 326352199920, 35364688360139904 }},
    {{ 546326, 565071359, 583981678872, 177358186953740 }, { 1584333, 1672265811, 1782840143732, 1154568990915326 }, { 1548939, 1749215709, 1968729742312, 3815756157892529 }, { 1241681, 1481755344, 1762577817769, 6750069816423635 }, { 1006983, 1266163442, 1587192388599, 9527548766731990 }, { 1473368, 2006322859, 2717194200454, 12723455293770668 }, { 1026916, 1530027006, 2268205118750, 17576289556341264 }, { 737505, 1193349257, 1921988103074, 22014488395744488 }, { 543255, 948650360, 1649406337959, 26103362551489300 }, { 408963, 766542323, 1430963775118, 29893912004437584 }, { 218917, 441511544, 874419453414, 33426723874534560 }, { 73044, 154092737, 313645075963, 35390388517295592 }},
    {{ 523043, 541130930, 559376906044, 202645741229417 }, { 1541240, 1627042542, 1735468731064, 1204190538157398 }, { 1478957, 1671746927, 1883281402808, 3910005624855053 }, { 1185823, 1416345850, 1686244717361, 6839150501103376 }, { 961858, 1210434694, 1518590120183, 9611998122088872 }, { 1403197, 1913838685, 2596038818881, 12882168598117246 }, { 978561, 1460106406, 2167658526148, 17720875370816748 }, { 703109, 1139210670, 1837206527552, 22147256199190080 }, { 518126, 905880062, 1576953052494, 26226098303412592 }, { 390179, 732167726, 1368332959314, 30008025259465848 }, { 204597, 413471474, 819741087754, 33533346613025944 }, { 69946, 147819169, 300950508453, 35416075973607100 }},
    {{ 499808, 517225410, 534796056184, 227920999100877 }, { 1498311, 1581948545, 1688187635674, 1253764759166979 }, { 1409479, 1594698030, 1798142098889, 4004084504453843 }, { 1130345, 1351271502, 1610172584817, 6928078780773416 }, { 917025, 1154976692, 1450210225408, 9696310495462916 }, { 1333875, 1822196544, 2475619944179, 13040398756193388 }, { 930739, 1390766055, 2067668978375, 17865060114556742 }, { 669062, 1085484827, 1752856410426, 22279685740349644 }, { 493232, 863411316, 1504840723665, 26348544924910540 }, { 371559, 698018534, 1305976238983, 30121888540022268 }, { 190393, 385603252, 765286355491, 33639751085779304 }, { 66855, 141554893, 288268478788, 35441750741621568 }},
    {{ 476619, 493354730, 510239094420, 253183972520882 }, { 1455546, 1536983326, 1640996599514, 1303291744132239 }, { 1340502, 1518065987, 1713310157078, 4097993413084559 }, { 1075246, 1286530014, 1534360084125, 7016855176031931 }, { 872482, 1099787685, 1382051624791, 9780486330520752 }, { 1265390, 1731386244, 2355930880832, 13198148700608358 }, { 883444, 1321999550, 1968231859039, 18008846006490656 }, { 635359, 1032167542, 1668934466482, 22411778738475196 }, { 468571, 821241292, 1433066950369, 26470703774999888 }, { 353101, 664092783, 1243891818740, 30235502938892744 }, { 176304, 357905477, 711053887441, 33745938184586124 }, { 63770, 135299890, 275598968401, 35467412833867556 }},
    {{ 453477, 469518823, 485705985946, 278434673424776 }, { 1412944, 1492146397, 1593895365516, 1352771582983792 }, { 1272020, 1441847797, 1628783915957, 4191732963808011 }, { 1020520, 1222119120, 1458805888375, 7105480204814227 }, { 828225, 1044865937, 1314113245824, 9864526068777040 }, { 1197728, 1641397748, 2236965014235, 13355421337351724 }, { 836668, 1253800576, 1869342602616, 18152235247184412 }, { 601997, 979254683, 1585437443770, 22543536899744520 }, { 444140, 779367197, 1361629353995, 26592576203138148 }, { 334803, 630388525, 1182077918845, 30348869541712964 }, { 162330, 330376762, 657042325573, 33851908795783460 }, { 60690, 129054143, 262941958765, 35493062262855076 }},
    {{ 430381, 445717621, 461196696023, 303673113730518 }, { 1370503, 1447437268, 1546883677590, 1402204365395674 }, { 1204030, 1366040486, 1544561726064, 4285303766374126 }, { 966167, 1158036573, 1383508679685, 7193954382410788 }, { 784254, 990209724, 1246394022917, 9948430149608332 }, { 1130878, 1552221170, 2118715809478, 13512219546114284 }, { 790404, 1186162906, 1770996693750, 18295230019042384 }, { 568970, 926742171, 1502362123190, 22674961917393564 }, { 419937, 737786267, 1290525578164, 26714163549312844 }, { 316664, 596903838, 1120532775036, 30461989427030660 }, { 148469, 303015734, 603250322899, 33957663800298440 }, { 57617, 122817632, 250297431387, 35518699041075624 }},
    {{ 407330, 421951055, 436711189983, 328899305338712 }, { 1328224, 1402855456, 1499961280620, 1451590180786319 }, { 1136527, 1290641104, 1460641949777, 4378706427245777 }, { 912181, 1094280145, 1308467149122, 7282278221485337 }, { 740565, 935817337, 1178892897346, 10032199010266890 }, { 1064828, 1463846772, 2001176810148, 13668546180604644 }, { 744645, 1119080397, 1673189666573, 18437832486506708 }, { 536274, 874625977, 1419705318077, 22806055471846776 }, { 395959, 696495773, 1219753288465, 26835467144130088 }, { 298682, 563636819, 1059254638358, 30574863666367208 }, { 134721, 275821033, 549676543359, 34063204073692052 }, { 54550, 116590341, 237665367811, 35544323181002232 }},
    {{ 384324, 398219060, 412249433220, 354113260132646 }, { 1286105, 1358400477, 1453127920456, 1500929118319526 }, { 1069506, 1215646732, 1377022961219, 4471941549622363 }, { 858561, 1030847626, 1233679996629, 7370452232092581 }, { 697156, 881687082, 1111608817195, 10115833085894346 }, { 999565, 1376264961, 1884341637154, 13824404068860850 }, { 699383, 1052546990, 1575917104022, 18580044796253936 }, { 503906, 822902123, 1337463873791, 22936819230846112 }, { 372203, 655493015, 1149310172206, 26956488308901832 }, { 280855, 530585585, 998241774997, 30687493324278492 }, { 121083, 248791315, 496319661711, 34168530486202844 }, { 51489, 110372249, 225045749617, 35569934695089492 }},
    {{ 361364, 374521568, 387811391201, 379314989978321 }, { 1244145, 1314071850, 1406383343913, 1550221266905430 }, { 1002964, 1141054473, 1293703146144, 4565009733463152 }, { 805302, 967736826, 1159145930946, 7458476921695956 }, { 654026, 827817276, 1044540737300, 10199332809535246 }, { 935077, 1289466288, 1768203987574, 13979796013557270 }, { 654614, 986556707, 1479174637175, 18721869077388664 }, { 471862, 771566680, 1255634667319, 23067254849578376 }, { 348666, 614775327, 1079193938155, 27077228355732308 }, { 263182, 497748273, 937492466121, 30799879458415132 }, { 107555, 221925247, 443178363423, 34273643902789720 }, { 48434, 104163341, 212438558424, 35595533595773584 }},
    {{ 338448, 350858512, 363397029454, 404504506724491 }, { 1181629, 1269830451, 1359727280870, 1599466715202632 }, { 936896, 1066861458, 1210680901836, 4657911575510494 }, { 752402, 904945574, 1084863669538, 7546352795185097 }, { 611171, 774206251, 977687619200, 10282698612150540 }, { 871355, 1203441444, 1652757633521, 14134724792306874 }, { 610328, 921103655, 1382957944601, 18863307441635348 }, { 440136, 720615770, 1174214606877, 23197363970801036 }, { 325347, 574340073, 1009402316297, 27197688587603236 }, { 245660, 465123041, 877005007713, 30912023119582172 }, { 97479, 195234039, 390251356248, 34378545183173168 }, { 45385, 97963596, 199843775883, 35621119895472340 }},
    {{ 315578, 327229825, 339006313579, 429681822202686 }, { 1098650, 1181800933, 1266679780739, 1697817864296980 }, { 871298, 993064842, 1127954637008, 4750647669312736 }, { 699859, 842471715, 1010831938522, 7634080354893226 }, { 568590, 720852353, 911048431078, 10365930922630890 }, { 808385, 1118181257, 1537996421028, 14289193157959558 }, { 566521, 856182016, 1287262751711, 19004361983526264 }, { 408726, 670045561, 1093200631521, 23327148224966020 }, { 302243, 534184645, 939933057587, 27317870298458356 }, { 228288, 432708068, 816777710415, 31023925351797916 }, { 90796, 181942587, 363867796951, 34430916539531572 }, { 42342, 91772998, 187261383684, 35646693606585248 }},
    {{ 292751, 303635441, 314639209239, 454846948227256 }, { 1016297, 1094269227, 1173983403251, 1795983269845312 }, { 806165, 919661806, 1045522771699, 4843218605247002 }, { 647668, 780313115, 937049472594, 7721660100614417 }, { 526280, 667753940, 844622147709, 10449030167809944 }, { 746157, 1033676692, 1423914268958, 14443203838895186 }, { 523186, 791786055, 1192084830131, 19145034780588180 }, { 377627, 619852270, 1012589710768, 23456609230342680 }, { 279350, 494306468, 870783933708, 27437774773286848 }, { 211066, 400501550, 756808899371, 31135587192352216 }, { 84140, 168691229, 337537323285, 34483235181883728 }, { 39304, 85591529, 174691363552, 35672254741493512 }},
    {{ 269969, 280075293, 290295682165, 479999896595399 }, { 934563, 1007231585, 1081636164663, 1893963632106671 }, { 741494, 846649558, 963383737172, 4935624970541734 }, { 595827, 718467655, 863515014954, 7809092529620627 }, { 484240, 614909383, 778407750412, 10531996772477430 }, { 684660, 949918846, 1310505167926, 14596759539312984 }, { 480316, 727910110, 1097419997073, 19285327893525700 }, { 346836, 570032161, 932378844209, 23585748593138636 }, { 256668, 454702995, 801952736834, 27557403288205496 }, { 193990, 368501704, 697096914070, 31247009671864404 }, { 77511, 155479806, 311259775195, 34535501216240736 }, { 36273, 79419169, 162133697248, 35697803312560076 }},
    {{ 247232, 256549315, 265975698153, 505140679087195 }, { 853443, 920684297, 989636096145, 1991759647387556 }, { 677281, 774025327, 881535975816, 5027867349299033 }, { 544334, 656933236, 790227317238, 7896378136678666 }, { 442467, 562317067, 712404226991, 10614831159392228 }, { 623883, 866898948, 1197763179247, 14749862939516412 }, { 437905, 664548598, 1003264114727, 19425243366402428 }, { 316348, 520581543, 852565061146, 23714567907619488 }, { 234193, 415371710, 733437279394, 27676757110540548 }, { 177060, 336706766, 637640108191, 31358193814340068 }, { 70909, 142308155, 285034993266, 34587714748294228 }, { 33247, 73255903, 149588366569, 35723339332129668 }},
    {{ 224538, 233057441, 241679223068, 530269307465637 }, { 772931, 834623685, 897981243644, 2089372008071600 }, { 613521, 701786369, 799977941049, 5119946322516838 }, { 493184, 595707775, 717185139448, 7983517414067007 }, { 400958, 509975388, 646610571688, 10697533749295268 }, { 563816, 784608353, 1085682433897, 14902516696193780 }, { 395947, 601696009, 909613089655, 19564783226819656 }, { 286160, 471496772, 773145420219, 23843068756226768 }, { 211923, 376310124, 665235393840, 27795837498908004 }, { 160273, 305114988, 578436849459, 31469140637227720 }, { 64333, 129176119, 258862818722, 34639875883417640 }, { 30228, 67101710, 137055353348, 35748862812528820 }},
    {{ 201888, 209599605, 217406222838, 555385793476669 }, { 693020, 749046106, 806669667746, 2186801402648995 }, { 550210, 629929964, 718708097217, 5211862468110864 }, { 442376, 534789208, 644387249877, 8070510851592411 }, { 359713, 457882757, 581025785133, 10780104960922346 }, { 504449, 703038544, 974257131497, 15054723442694952 }, { 354436, 539346909, 816462872200, 19703949486092712 }, { 256269, 422774248, 694117009049, 23971252709694644 }, { 189855, 337515775, 597344932420, 27914645703293360 }, { 143629, 273724645, 519485519486, 31579851151474536 }, { 57784, 116083538, 232743093426, 34691984726667504 }, { 27214, 60956575, 124534639453, 35774373766065920 }},
    {{ 179282, 186175741, 193156663458, 580490148849213 }, { 613706, 663947954, 715699443533, 2284048515745626 }, { 487345, 558453415, 637724919500, 5303616360936356 }, { 391906, 474175487, 571832425046, 8157358936606479 }, { 318728, 406037596, 515648874287, 10862545211016878 }, { 445771, 622181129, 863481539308, 15206485789303982 }, { 313366, 477495933, 723809455901, 19842744139424844 }, { 226670, 374410414, 615476943877, 24099121327165204 }, { 167988, 298986233, 529763766950, 28033182965130276 }, { 127126, 242534027, 460784513631, 31690326361581740 }, { 51260, 103030254, 206675659873, 34744041382784708 }, { 24205, 54820478, 112026206787, 35799872205031252 }},
    {{ 156720, 162785784, 168930510989, 605582385295207 }, { 534982, 579325653, 625068660455, 2381114028151934 }, { 424921, 487354050, 557026893817, 5395208572809693 }, { 341772, 413864581, 499519449630, 8244062154022009 }, { 278003, 354438341, 450478852399, 10944854914342480 }, { 387772, 542027835, 753349991255, 15357806323507516 }, { 272731, 416137789, 631648876920, 19981169166078924 }, { 197361, 326401759, 537222369218, 24226676156302072 }, { 146319, 260719093, 462489788595, 28151450517378496 }, { 110763, 211541444, 402332240851, 31800567265659188 }, { 44763, 90016111, 180660361186, 34796045956195752 }, { 21203, 48693402, 99530037291, 35825358141697004 }},
    {{ 134202, 139429669, 144727731557, 630662514509635 }, { 456844, 495175661, 534775422194, 2477998616851497 }, { 362936, 416629220, 476612516729, 5486639672529730 }, { 291971, 353854477, 427447116395, 8330620986329258 }, { 237534, 303083438, 385514738953, 11027034483695526 }, { 330443, 462570512, 643856886952, 15508687610259618 }, { 232525, 355267256, 539977213474, 20119226529546776 }, { 168337, 278744814, 459350457514, 24353918733403012 }, { 124846, 222711977, 395520907646, 28269449584600724 }, { 94537, 180745223, 344127123557, 31910574855479616 }, { 38292, 77040951, 154697041119, 34847998551014012 }, { 18205, 42575330, 87046112938, 35850831588317332 }},
    {{ 111727, 116107329, 120548291352, 655730548170562 }, { 379285, 411494471, 444817846528, 2574702955049405 }, { 301384, 346276298, 396480295351, 5577910225899030 }, { 242500, 294143177, 355614226127, 8417035913612043 }, { 197319, 251971347, 320755559619, 11109084329917542 }, { 273774, 383801123, 534996690761, 15659132192242602 }, { 192744, 294879180, 448790585281, 20256918177716160 }, { 139595, 231436151, 381858408792, 24480850583510732 }, { 103567, 184962535, 328855053306, 28387181383038728 }, { 78448, 150143708, 286167597475, 32020350116531940 }, { 31846, 64104620, 128785544045, 34899899271041012 }, { 15213, 36466242, 74574415740, 35876292557128384 }},
    {{ 89295, 92818701, 96392156632, 680786497939162 }, { 302301, 328278606, 355194065206, 2671227712200269 }, { 240262, 276292681, 316628747251, 5669020795744846 }, { 193356, 234728701, 284019587567, 8503307413563726 }, { 157358, 201100540, 256200346203, 11191004861907534 }, { 217755, 305711752, 426763930854, 15809142590124244 }, { 153380, 234968477, 358085153011, 20394246043036032 }, { 111132, 184472384, 304743450330, 24607473220522692 }, { 82479, 147468443, 262490173477, 28504647120688588 }, { 62494, 119735262, 228452111501, 32129894028074476 }, { 25426, 51206963, 102925714962, 34951748219767620 }, { 12226, 30366121, 62114927739, 35901741060348332 }},
    {{ 66907, 69563718, 72259293718, 705830375459754 }, { 225885, 245524623, 265902223814, 2767573554036082 }, { 179566, 206675790, 237056400368, 5759971941939959 }, { 144537, 175609085, 212662017347, 8589435961503038 }, { 117647, 150469500, 191848136603, 11272796486634234 }, { 162377, 228294593, 319153198298, 15958721302811002 }, { 114430, 175530127, 267857117751, 20531212042678744 }, { 82945, 137850170, 228002836324, 24733788147299404 }, { 61580, 110227402, 196424234546, 28621847997374980 }, { 46673, 89518263, 170979127569, 32239207563187216 }, { 19032, 38347825, 77117399482, 35003545500375332 }, { 9242, 24274948, 49667631018, 35927177110177416 }},
    {{ 44561, 46342317, 48149668996, 730862192359838 }, { 150033, 163229109, 176940481651, 2863741142593717 }, { 119293, 137423066, 157761792916, 5850764221423303 }, { 96039, 116782382, 141540339921, 8675422030389829 }, { 78185, 100076723, 127697974756, 11354459609148206 }, { 107632, 151541952, 212159146151, 16107870807697858 }, { 75886, 116559179, 178102720467, 20667818078701244 }, { 55029, 91566204, 151633847565, 24859796855771552 }, { 40869, 73237142, 130655221182, 28738785204824780 }, { 30985, 59491108, 113747120508, 32348291688823664 }, { 12663, 25527053, 51360443834, 35055291215737480 }, { 6259, 18192701, 37232507689, 35952600718797964 }},
    {{ 22259, 23154432, 24063248919, 755881960250116 }, { 74740, 81388685, 88307011599, 2959731136242266 }, { 59439, 68531974, 78743473293, 5941398188220457 }, { 47862, 58246658, 70653387502, 8761266090840590 }, { 38970, 49920717, 63748910595, 11435994632593894 }, { 53509, 75446246, 105776488584, 16256593560914702 }, { 37745, 58050745, 88818241491, 20804066038202960 }, { 27382, 45617223, 75633791118, 24985500827045508 }, { 20343, 36495415, 65181136129, 28855459926739760 }, { 15428, 29652211, 56754577911, 32457147365862300 }, { 6319, 12744495, 25654694858, 35106985468420468 }, { 3267, 12119347, 24809539896, 35978011898374452 }},
    {{ 0, 0, 0, 780889690724540 }, { 0, 0, 0, 3055544189710061 }, { 0, 0, 0, 6031874393463902 }, { 0, 0, 0, 8846968611143999 }, { 0, 0, 0, 11517401958221562 }, { 0, 0, 0, 16404891997568734 }, { 0, 0, 0, 20939957793482708 }, { 0, 0, 0, 25110901531507984 }, { 0, 0, 0, 28971873338868492 }, { 0, 0, 0, 32565775549157248 }, { 0, 0, 0, 35158628360684988 }, { 0, 0, 0, 36028797018963968 }}
};

const memory<log2_coefficients, (1 << LOG2_TABLE_LG_SIZE)> log2_coefficient_table =
{
    { -184, 5909, 0 },
    { -184, 5898, 2952 },
    { -183, 5886, 5898 },
    { -182, 5875, 8838 },
    { -181, 5863, 11773 },
    { -181, 5852, 14702 },
    { -180, 5841, 17625 },
    { -179, 5830, 20542 },
    { -179, 5818, 23454 },
    { -178, 5807, 26361 },
    { -177, 5796, 29262 },
    { -177, 5785, 32157 },
    { -176, 5774, 35047 },
    { -175, 5763, 37931 },
    { -175, 5752, 40810 },
    { -174, 5741, 43683 },
    { -173, 5730, 46551 },
    { -173, 5719, 49413 },
    { -172, 5709, 52270 },
    { -171, 5698, 55122 },
    { -171, 5687, 57968 },
    { -170, 5676, 60809 },
    { -169, 5666, 63644 },
    { -169, 5655, 66475 },
    { -168, 5645, 69300 },
    { -168, 5634, 72119 },
    { -167, 5624, 74934 },
    { -166, 5613, 77743 },
    { -166, 5603, 80547 },
    { -165, 5593, 83346 },
    { -164, 5582, 86140 },
    { -164, 5572, 88928 },
    { -163, 5562, 91711 },
    { -163, 5551, 94490 },
    { -162, 5541, 97263 },
    { -161, 5531, 100031 },
    { -161, 5521, 102794 },
    { -160, 5511, 105552 },
    { -160, 5501, 108305 },
    { -159, 5491, 111053 },
    { -159, 5481, 113796 },
    { -158, 5471, 116534 },
    { -157, 5461, 119267 },
    { -157, 5451, 121995 },
    { -156, 5442, 124719 },
    { -156, 5432, 127437 },
    { -155, 5422, 130151 },
    { -155, 5412, 132859 },
    { -154, 5403, 135563 },
    { -154, 5393, 138262 },
    { -153, 5384, 140956 },
    { -152, 5374, 143646 },
    { -152, 5364, 146330 },
    { -151, 5355, 149010 },
    { -151, 5345, 151685 },
    { -150, 5336, 154356 },
    { -150, 5327, 157021 },
    { -149, 5317, 159682 },
    { -149, 5308, 162339 },
    { -148, 5299, 164990 },
    { -148, 5289, 167637 },
    { -147, 5280, 170280 },
    { -147, 5271, 172917 },
    { -146, 5262, 175551 },
    { -146, 5253, 178179 },
    { -145, 5244, 180803 },
    { -145, 5235, 183423 },
    { -144, 5225, 186038 },
    { -144, 5216, 188648 },
    { -143, 5207, 191254 },
    { -143, 5199, 193856 },
    { -142, 5190, 196453 },
    { -142, 5181, 199045 },
    { -141, 5172, 201634 },
    { -141, 5163, 204217 },
    { -140, 5154, 206797 },
    { -140, 5145, 209372 },
    { -139, 5137, 211942 },
    { -139, 5128, 214508 },
    { -138, 5119, 217070 },
    { -138, 5111, 219628 },
    { -137, 5102, 222181 },
    { -137, 5094, 224730 },
    { -137, 5085, 227275 },
    { -136, 5076, 229815 },
    { -136, 5068, 232351 },
    { -135, 5059, 234883 },
    { -135, 5051, 237410 },
    { -134, 5043, 239934 },
    { -134, 5034, 242453 },
    { -133, 5026, 244968 },
    { -133, 5017, 247479 },
    { -132, 5009, 249985 },
    { -132, 5001, 252488 },
    { -132, 4993, 254986 },
    { -131, 4984, 257481 },
    { -131, 4976, 259971 },
    { -130, 4968, 262457 },
    { -130, 4960, 264939 },
    { -129, 4952, 267417 },
    { -129, 4944, 269891 },
    { -129, 4936, 272361 },
    { -128, 4928, 274826 },
    { -128, 4920, 277288 },
    { -127, 4912, 279746 },
    { -127, 4904, 282200 },
    { -127, 4896, 284650 },
    { -126, 4888, 287095 },
    { -126, 4880, 289537 },
    { -125, 4872, 291975 },
    { -125, 4864, 294409 },
    { -125, 4856, 296840 },
    { -124, 4849, 299266 },
    { -124, 4841, 301688 },
    { -123, 4833, 304107 },
    { -123, 4825, 306521 },
    { -123, 4818, 308932 },
    { -122, 4810, 311339 },
    { -122, 4802, 313742 },
    { -121, 4795, 316142 },
    { -121, 4787, 318537 },
    { -121, 4780, 320929 },
    { -120, 4772, 323317 },
    { -120, 4765, 325701 },
    { -119, 4757, 328082 },
    { -119, 4750, 330458 },
    { -119, 4742, 332831 },
    { -118, 4735, 335201 },
    { -118, 4727, 337566 },
    { -118, 4720, 339928 },
    { -117, 4713, 342286 },
    { -117, 4705, 344641 },
    { -117, 4698, 346991 },
    { -116, 4691, 349339 },
    { -116, 4684, 351682 },
    { -115, 4676, 354022 },
    { -115, 4669, 356359 },
    { -115, 4662, 358691 },
    { -114, 4655, 361020 },
    { -114, 4648, 363346 },
    { -114, 4640, 365668 },
    { -113, 4633, 367986 },
    { -113, 4626, 370301 },
    { -113, 4619, 372613 },
    { -112, 4612, 374920 },
    { -112, 4605, 377225 },
    { -112, 4598, 379526 },
    { -111, 4591, 381823 },
    { -111, 4584, 384117 },
    { -111, 4577, 386407 },
    { -110, 4570, 388694 },
    { -110, 4563, 390977 },
    { -110, 4557, 393257 },
    { -109, 4550, 395534 },
    { -109, 4543, 397807 },
    { -109, 4536, 400077 },
    { -108, 4529, 402343 },
    { -108, 4522, 404606 },
    { -108, 4516, 406866 },
    { -107, 4509, 409122 },
    { -107, 4502, 411375 },
    { -107, 4496, 413624 },
    { -106, 4489, 415870 },
    { -106, 4482, 418113 },
    { -106, 4476, 420353 },
    { -105, 4469, 422589 },
    { -105, 4462, 424822 },
    { -105, 4456, 427051 },
    { -105, 4449, 429278 },
    { -104, 4443, 431501 },
    { -104, 4436, 433720 },
    { -104, 4430, 435937 },
    { -103, 4423, 438150 },
    { -103, 4417, 440360 },
    { -103, 4410, 442567 },
    { -102, 4404, 444771 },
    { -102, 4398, 446971 },
    { -102, 4391, 449168 },
    { -102, 4385, 451362 },
    { -101, 4379, 453553 },
    { -101, 4372, 455741 },
    { -101, 4366, 457925 },
    { -100, 4360, 460107 },
    { -100, 4353, 462285 },
    { -100, 4347, 464460 },
    { -100, 4341, 466632 },
    { -99, 4335, 468801 },
    { -99, 4328, 470966 },
    { -99, 4322, 473129 },
    { -98, 4316, 475289 },
    { -98, 4310, 477445 },
    { -98, 4304, 479599 },
    { -98, 4298, 481749 },
    { -97, 4292, 483896 },
    { -97, 4285, 486041 },
    { -97, 4279, 488182 },
    { -96, 4273, 490320 },
    { -96, 4267, 492455 },
    { -96, 4261, 494587 },
    { -96, 4255, 496716 },
    { -95, 4249, 498843 },
    { -95, 4243, 500966 },
    { -95, 4237, 503086 },
    { -95, 4232, 505203 },
    { -94, 4226, 507318 },
    { -94, 4220, 509429 },
    { -94, 4214, 511537 },
    { -94, 4208, 513643 },
    { -93, 4202, 515745 },
    { -93, 4196, 517845 },
    { -93, 4191, 519942 },
    { -92, 4185, 522035 },
    { -92, 4179, 524126 },
    { -92, 4173, 526214 },
    { -92, 4167, 528300 },
    { -91, 4162, 530382 },
    { -91, 4156, 532461 },
    { -91, 4150, 534538 },
    { -91, 4145, 536612 },
    { -90, 4139, 538682 },
    { -90, 4133, 540750 },
    { -90, 4128, 542816 },
    { -90, 4122, 544878 },
    { -89, 4116, 546938 },
    { -89, 4111, 548995 },
    { -89, 4105, 551049 },
    { -89, 4100, 553100 },
    { -89, 4094, 555148 },
    { -88, 4089, 557194 },
    { -88, 4083, 559237 },
    { -88, 4078, 561277 },
    { -88, 4072, 563314 },
    { -87, 4067, 565349 },
    { -87, 4061, 567381 },
    { -87, 4056, 569410 },
    { -87, 4050, 571437 },
    { -86, 4045, 573460 },
    { -86, 4039, 575481 },
    { -86, 4034, 577500 },
    { -86, 4029, 579516 },
    { -85, 4023, 581529 },
    { -85, 4018, 583539 },
    { -85, 4013, 585547 },
    { -85, 4007, 587552 },
    { -85, 4002, 589554 },
    { -84, 3997, 591554 },
    { -84, 3991, 593551 },
    { -84, 3986, 595545 },
    { -84, 3981, 597537 },
    { -83, 3976, 599526 },
    { -83, 3971, 601513 },
    { -83, 3965, 603497 },
    { -83, 3960, 605478 },
    { -83, 3955, 607457 },
    { -82, 3950, 609433 },
    { -82, 3945, 611407 },
    { -82, 3940, 613378 },
    { -82, 3934, 615346 },
    { -82, 3929, 617312 },
    { -81, 3924, 619275 },
    { -81, 3919, 621236 },
    { -81, 3914, 623195 },
    { -81, 3909, 625150 },
    { -80, 3904, 627103 },
    { -80, 3899, 629054 },
    { -80, 3894, 631002 },
    { -80, 3889, 632948 },
    { -80, 3884, 634891 },
    { -79, 3879, 636832 },
    { -79, 3874, 638770 },
    { -79, 3869, 640706 },
    { -79, 3864, 642639 },
    { -79, 3859, 644570 },
    { -78, 3854, 646498 },
    { -78, 3849, 648424 },
    { -78, 3844, 650348 },
    { -78, 3840, 652269 },
    { -78, 3835, 654187 },
    { -77, 3830, 656103 },
    { -77, 3825, 658017 },
    { -77, 3820, 659928 },
    { -77, 3815, 661837 },
    { -77, 3811, 663744 },
    { -76, 3806, 665648 },
    { -76, 3801, 667549 },
    { -76, 3796, 669449 },
    { -76, 3791, 671345 },
    { -76, 3787, 673240 },
    { -76, 3782, 675132 },
    { -75, 3777, 677022 },
    { -75, 3773, 678909 },
    { -75, 3768, 680794 },
    { -75, 3763, 682677 },
    { -75, 3758, 684558 },
    { -74, 3754, 686436 },
    { -74, 3749, 688311 },
    { -74, 3744, 690185 },
    { -74, 3740, 692056 },
    { -74, 3735, 693925 },
    { -74, 3731, 695791 },
    { -73, 3726, 697655 },
    { -73, 3721, 699517 },
    { -73, 3717, 701377 },
    { -73, 3712, 703234 },
    { -73, 3708, 705089 },
    { -72, 3703, 706942 },
    { -72, 3699, 708792 },
    { -72, 3694, 710641 },
    { -72, 3690, 712487 },
    { -72, 3685, 714330 },
    { -72, 3681, 716172 },
    { -71, 3676, 718011 },
    { -71, 3672, 719848 },
    { -71, 3667, 721683 },
    { -71, 3663, 723515 },
    { -71, 3658, 725346 },
    { -71, 3654, 727174 },
    { -70, 3650, 729000 },
    { -70, 3645, 730823 },
    { -70, 3641, 732645 },
    { -70, 3636, 734464 },
    { -70, 3632, 736281 },
    { -70, 3628, 738096 },
    { -69, 3623, 739909 },
    { -69, 3619, 741720 },
    { -69, 3615, 743528 },
    { -69, 3610, 745335 },
    { -69, 3606, 747139 },
    { -69, 3602, 748941 },
    { -68, 3598, 750741 },
    { -68, 3593, 752538 },
    { -68, 3589, 754334 },
    { -68, 3585, 756127 },
    { -68, 3581, 757919 },
    { -68, 3576, 759708 },
    { -67, 3572, 761495 },
    { -67, 3568, 763280 },
    { -67, 3564, 765063 },
    { -67, 3559, 766844 },
    { -67, 3555, 768622 },
    { -67, 3551, 770399 },
    { -66, 3547, 772173 },
    { -66, 3543, 773946 },
    { -66, 3539, 775716 },
    { -66, 3535, 777484 },
    { -66, 3530, 779251 },
    { -66, 3526, 781015 },
    { -66, 3522, 782777 },
    { -65, 3518, 784537 },
    { -65, 3514, 786295 },
    { -65, 3510, 788051 },
    { -65, 3506, 789805 },
    { -65, 3502, 791557 },
    { -65, 3498, 793307 },
    { -64, 3494, 795055 },
    { -64, 3490, 796801 },
    { -64, 3486, 798544 },
    { -64, 3482, 800286 },
    { -64, 3478, 802026 },
    { -64, 3474, 803764 },
    { -64, 3470, 805500 },
    { -63, 3466, 807233 },
    { -63, 3462, 808965 },
    { -63, 3458, 810695 },
    { -63, 3454, 812423 },
    { -63, 3450, 814149 },
    { -63, 3446, 815873 },
    { -63, 3442, 817595 },
    { -62, 3438, 819315 },
    { -62, 3434, 821033 },
    { -62, 3430, 822749 },
    { -62, 3426, 824463 },
    { -62, 3423, 826176 },
    { -62, 3419, 827886 },
    { -62, 3415, 829594 },
    { -61, 3411, 831301 },
    { -61, 3407, 833005 },
    { -61, 3403, 834708 },
    { -61, 3399, 836409 },
    { -61, 3396, 838108 },
    { -61, 3392, 839804 },
    { -61, 3388, 841499 },
    { -61, 3384, 843192 },
    { -60, 3381, 844884 },
    { -60, 3377, 846573 },
    { -60, 3373, 848260 },
    { -60, 3369, 849946 },
    { -60, 3365, 851630 },
    { -60, 3362, 853311 },
    { -60, 3358, 854991 },
    { -59, 3354, 856669 },
    { -59, 3351, 858346 },
    { -59, 3347, 860020 },
    { -59, 3343, 861692 },
    { -59, 3339, 863363 },
    { -59, 3336, 865032 },
    { -59, 3332, 866699 },
    { -59, 3328, 868364 },
    { -58, 3325, 870027 },
    { -58, 3321, 871689 },
    { -58, 3317, 873348 },
    { -58, 3314, 875006 },
    { -58, 3310, 876662 },
    { -58, 3307, 878317 },
    { -58, 3303, 879969 },
    { -58, 3299, 881620 },
    { -57, 3296, 883268 },
    { -57, 3292, 884915 },
    { -57, 3289, 886561 },
    { -57, 3285, 888204 },
    { -57, 3282, 889846 },
    { -57, 3278, 891486 },
    { -57, 3274, 893124 },
    { -57, 3271, 894760 },
    { -56, 3267, 896394 },
    { -56, 3264, 898027 },
    { -56, 3260, 899658 },
    { -56, 3257, 901288 },
    { -56, 3253, 902915 },
    { -56, 3250, 904541 },
    { -56, 3246, 906165 },
    { -56, 3243, 907787 },
    { -55, 3239, 909408 },
    { -55, 3236, 911026 },
    { -55, 3232, 912644 },
    { -55, 3229, 914259 },
    { -55, 3226, 915873 },
    { -55, 3222, 917484 },
    { -55, 3219, 919095 },
    { -55, 3215, 920703 },
    { -54, 3212, 922310 },
    { -54, 3208, 923915 },
    { -54, 3205, 925518 },
    { -54, 3202, 927120 },
    { -54, 3198, 928720 },
    { -54, 3195, 930318 },
    { -54, 3192, 931915 },
    { -54, 3188, 933510 },
    { -54, 3185, 935103 },
    { -53, 3181, 936695 },
    { -53, 3178, 938284 },
    { -53, 3175, 939873 },
    { -53, 3171, 941459 },
    { -53, 3168, 943044 },
    { -53, 3165, 944627 },
    { -53, 3161, 946209 },
    { -53, 3158, 947789 },
    { -53, 3155, 949367 },
    { -52, 3152, 950944 },
    { -52, 3148, 952519 },
    { -52, 3145, 954092 },
    { -52, 3142, 955664 },
    { -52, 3139, 957234 },
    { -52, 3135, 958802 },
    { -52, 3132, 960369 },
    { -52, 3129, 961934 },
    { -52, 3126, 963498 },
    { -52, 3122, 965060 },
    { -51, 3119, 966620 },
    { -51, 3116, 968179 },
    { -51, 3113, 969736 },
    { -51, 3110, 971292 },
    { -51, 3106, 972846 },
    { -51, 3103, 974398 },
    { -51, 3100, 975949 },
    { -51, 3097, 977498 },
    { -51, 3094, 979046 },
    { -50, 3090, 980592 },
    { -50, 3087, 982136 },
    { -50, 3084, 983679 },
    { -50, 3081, 985220 },
    { -50, 3078, 986760 },
    { -50, 3075, 988298 },
    { -50, 3072, 989835 },
    { -50, 3069, 991370 },
    { -50, 3065, 992903 },
    { -50, 3062, 994435 },
    { -49, 3059, 995965 },
    { -49, 3056, 997494 },
    { -49, 3053, 999022 },
    { -49, 3050, 1000547 },
    { -49, 3047, 1002072 },
    { -49, 3044, 1003594 },
    { -49, 3041, 1005115 },
    { -49, 3038, 1006635 },
    { -49, 3035, 1008153 },
    { -49, 3032, 1009670 },
    { -48, 3029, 1011185 },
    { -48, 3026, 1012698 },
    { -48, 3023, 1014210 },
    { -48, 3020, 1015721 },
    { -48, 3017, 1017230 },
    { -48, 3013, 1018737 },
    { -48, 3010, 1020243 },
    { -48, 3008, 1021748 },
    { -48, 3005, 1023251 },
    { -48, 3002, 1024752 },
    { -48, 2999, 1026252 },
    { -47, 2996, 1027751 },
    { -47, 2993, 1029248 },
    { -47, 2990, 1030743 },
    { -47, 2987, 1032238 },
    { -47, 2984, 1033730 },
    { -47, 2981, 1035221 },
    { -47, 2978, 1036711 },
    { -47, 2975, 1038199 },
    { -47, 2972, 1039686 },
    { -47, 2969, 1041171 },
    { -46, 2966, 1042655 },
    { -46, 2963, 1044138 },
    { -46, 2960, 1045618 },
    { -46, 2958, 1047098 }
};

const memory<uint40, (1 << EXP2_S0_TABLE_LG_SIZE)> exp2_section0_coefficient_table =
{
    0,
    24076169560,
    48679538585,
    73821651238,
    99514304469,
    125769553548,
    152599717718,
    180017385982,
    208035423003,
    236666975147,
    265925476643,
    295824655897,
    326378541923,
    357601470933,
    389508093061,
    422113379237,
    455432628212,
    489481473734,
    524275891888,
    559832208589,
    596167107242,
    633297636572,
    671241218623,
    710015656930,
    749639144877,
    790130274230,
    831508043861,
    873791868665,
    917001588666,
    961157478330,
    1006280256073,
    1052391093988
};

const memory<uint35, (1 << EXP2_S1_TABLE_LG_SIZE)> exp2_section1_coefficient_table =
{
    0,
    744513071,
    1489530274,
    2235051951,
    2981078444,
    3727610094,
    4474647244,
    5222190235,
    5970239411,
    6718795114,
    7467857687,
    8217427473,
    8967504816,
    9718090059,
    10469183547,
    11220785623,
    11972896632,
    12725516918,
    13478646827,
    14232286703,
    14986436892,
    15741097739,
    16496269590,
    17251952791,
    18008147688,
    18764854628,
    19522073958,
    20279806024,
    21038051173,
    21796809754,
    22556082113,
    23315868600
};

const memory<exp2_coefficients, (1 << EXP2_S2_TABLE_LG_SIZE)> exp2_section2_coefficient_table =
{
    { 0, 1099511627776 },
    { 123, 1099534886182 },
    { 246, 1099558145080 },
    { 369, 1099581404470 },
    { 492, 1099604664352 },
    { 615, 1099627924726 },
    { 738, 1099651185592 },
    { 861, 1099674446950 },
    { 984, 1099697708800 },
    { 1107, 1099720971142 },
    { 1230, 1099744233976 },
    { 1353, 1099767497303 },
    { 1476, 1099790761121 },
    { 1599, 1099814025432 },
    { 1722, 1099837290234 },
    { 1845, 1099860555529 },
    { 1968, 1099883821316 },
    { 2091, 1099907087595 },
    { 2214, 1099930354366 },
    { 2337, 1099953621630 },
    { 2460, 1099976889385 },
    { 2583, 1100000157633 },
    { 2707, 1100023426373 },
    { 2830, 1100046695605 },
    { 2953, 1100069965330 },
    { 3076, 1100093235546 },
    { 3199, 1100116506255 },
    { 3322, 1100139777457 },
    { 3445, 1100163049150 },
    { 3568, 1100186321336 },
    { 3691, 1100209594014 },
    { 3814, 1100232867184 }
};

const memory<invsqrt_coefficients, (1 << INVSQRT_TABLE_LG_SIZE)> invsqrt_exp_zero_table =
{
    {-32, 2048},
    {-31, 2017},
    {-29, 1987},
    {-28, 1958},
    {-27, 1931},
    {-26, 1905},
    {-25, 1879},
    {-24, 1855},
    {-23, 1832},
    {-22, 1809},
    {-21, 1788},
    {-21, 1767},
    {-20, 1747},
    {-19, 1727},
    {-19, 1708},
    {-18, 1690},
    {-17, 1672},
    {-17, 1655},
    {-16, 1638},
    {-16, 1622},
    {-15, 1607},
    {-15, 1591},
    {-15, 1577},
    {-14, 1562},
    {-14, 1548},
    {-13, 1535},
    {-13, 1521},
    {-13, 1508},
    {-12, 1496},
    {-12, 1483},
    {-12, 1471},
    {-12, 1460}
};

const memory<invsqrt_coefficients, (1 << INVSQRT_TABLE_LG_SIZE)> invsqrt_exp_m1_table =
{
    {-91, 2896},
    {-86, 2852},
    {-83, 2810},
    {-79, 2769},
    {-76, 2731},
    {-73, 2694},
    {-70, 2658},
    {-67, 2624},
    {-65, 2591},
    {-62, 2559},
    {-60, 2528},
    {-58, 2499},
    {-56, 2470},
    {-54, 2442},
    {-53, 2416},
    {-51, 2390},
    {-49, 2365},
    {-48, 2341},
    {-46, 2317},
    {-45, 2294},
    {-44, 2272},
    {-42, 2251},
    {-41, 2230},
    {-40, 2209},
    {-39, 2189},
    {-38, 2170},
    {-37, 2151},
    {-36, 2133},
    {-35, 2115},
    {-34, 2098},
    {-34, 2081},
    {-33, 2064}
};