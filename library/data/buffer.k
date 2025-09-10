// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
*/
module data.buffer
    { serial_in_parallel_out
    }

import data.tuple

//| This function implements a shift register that accepts one data value
// per cycle to be shifted in at the most-significant end and returns the
// contents of the entire register prior to shifting and after shifting.
// If needed, the number of valid entries in the shift register must be tracked
// by the user manually.
// Since this is an inline function, each call-site will have its own private
// shift register instance; sharing the same instance can be achieved by
// calling this function from a non-inline shared outer function.
template
    < auto N                  //< Length of shift register
    , typename T              //< Type of each buffer entry
    , bool Initialize = true  //< true if the state values should be initialized to {}
    >
inline pair<T[N], T[N]> serial_in_parallel_out(T data)
{
    pair<T[N], T[N]> result;

    atomic
    {
        static if (Initialize)
        {
            static T[N] reg = {};
        }
        else
        {
            static T[N] reg;
        }

        const auto snappedReg = reg;

        static for(const auto i : N)
        {
            if (i != N - 1)
                reg[i] = snappedReg[i+1];
            else
                reg[i] = data;
        }

        result = {snappedReg, reg};
    }

    return result;
}
