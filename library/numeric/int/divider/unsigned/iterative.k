// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module numeric.int.divider.unsigned.iterative
    { divide
    }

import numeric.int.divider.unsigned as DU

//| Fully iterative divider for unsigned integers. This type of divider will
// consume the lowest area, but has the highest latency. Latency increases
// linearly with the number of bits in the Numerator.
template <typename NumeratorType, typename DenominatorType>
inline NumeratorType[2] divide(NumeratorType numerator, DenominatorType denominator)
{
    return DU::divide<NumeratorType, DenominatorType, 1>(numerator, denominator);
}