// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module numeric.int.divider.signed.iterative
    { divide
    }

import numeric.int.divider.signed as DS

//| Fully iterative divider for signed integers. This type of divider will
// consume the lowest area, but has the highest latency. Latency increases
// linearly with the number of bits in the Numerator.
template <typename NumeratorType, typename DenominatorType>
inline NumeratorType[2] divide(NumeratorType numerator, DenominatorType denominator)
{
    return DS::divide<NumeratorType, DenominatorType, 1>(numerator, denominator);
}