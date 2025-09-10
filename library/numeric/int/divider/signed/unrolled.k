// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module numeric.int.divider.signed.unrolled
    { divide
    }

import numeric.int.divider.signed as DS

//| Fully unrolled divider for signed integers. This type of divider will
// consume the most area, but has the lowest latency.
template <typename NumeratorType, typename DenominatorType>
inline NumeratorType[2] divide(NumeratorType numerator, DenominatorType denominator)
{
    return DS::divide<NumeratorType, DenominatorType, bitsizeof(NumeratorType)>(numerator, denominator);
}