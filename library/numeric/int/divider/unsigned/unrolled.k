// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module numeric.int.divider.unsigned.unrolled
    { divide
    }

import numeric.int.divider.unsigned as DU

//| Fully unrolled divider for unsigned integers. This type of divider will
// consume the most area, but has the lowest latency.
template <typename NumeratorType, typename DenominatorType>
inline NumeratorType[2] divide(NumeratorType numerator, DenominatorType denominator)
{
    return DU::divide<NumeratorType, DenominatorType, bitsizeof(NumeratorType)>(numerator, denominator);
}