// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module numeric.int.divider.unsigned
    { divide
    }

//| Calculate the quotient and remainder from dividing unsigned integers.
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
    const auto DenominatorWidth = bitsizeof(DenominatorType);

    uint<DenominatorWidth+1> remainder;

    NumeratorType quotient;
    uint1[NumeratorWidth] numerator_as_bits = cast<uint1[NumeratorWidth]>(numerator);

    static assert(NumeratorWidth == (NumeratorWidth / UnrollingFactor) * UnrollingFactor);

    uint<NumeratorWidth+1> i = 0;

    do
    {
        static for(const auto j : UnrollingFactor)
        {
            remainder = (remainder << 1) | numerator_as_bits[NumeratorWidth - (i+j) - 1];

            if (remainder < denominator) // underflow to upper bit => remainder < denominator
            {
                quotient = (quotient << 1);
            }
            else
            {
                quotient = (quotient << 1) | 1;
                remainder -= denominator;
            }
        }

        i += UnrollingFactor;
    }
    while (i < NumeratorWidth);

    return {quotient, remainder};
}