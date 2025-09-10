// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module numeric.int.divider.signed
    { divide
    }

import numeric.int.divider.unsigned as U
import numeric.int.operator

//| Calculate the quotient and remainder by dividing signed integers.
// The return value is an array with the first element being the quotient and the
// second element being the remainder.
//
// Balance throughput and area by adjusting the `UnrollingFactor` parameter.
template
    < typename NumeratorType
    , typename DenominatorType
    , auto UnrollingFactor     //< Chunks of numerator bits of this size are processed in parallel,
                               // meaning the outer loop only has to execute
                               // `bitsizeof(NumeratorType)/UnrollingFactor` iterations.
    >
inline NumeratorType[2] divide(NumeratorType numerator, DenominatorType denominator)
{
    assert(denominator != 0);

    const auto NumeratorWidth = bitsizeof(NumeratorType);
    const auto DenominatorWidth = bitsizeof(NumeratorType);

    bool num_is_neg = numerator < 0;
    bool denom_is_neg = denominator < 0;

    uint<NumeratorWidth> num_abs = abs(numerator);
    uint<DenominatorWidth> denom_abs = abs(denominator);

    NumeratorType[2] result = static_cast(U::divide<uint<NumeratorWidth>, uint<DenominatorWidth>, UnrollingFactor>(num_abs, denom_abs));

    if (num_is_neg != denom_is_neg)
    {
        result[0] = cast<NumeratorType>(-result[0]);
    }

    if (num_is_neg)
    {
        result[1] = cast<NumeratorType>(-result[1]);
    }

    return result;
}