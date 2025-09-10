// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright: (c) Microsoft Corporation. All rights reserved.
module data.buffer.pipelined
    { pipelined_variable
    , pipelined_variable_array
    }

import sync.atomic

//| Pass a variable between consecutive threads X and X+1.
template<typename T /*< Variable type.*/>
class pipelined_variable
{
public:
    //| Feed in a new value and push the old value down. This should typically be called exactly once per thread.
    inline T next(T input)
    {
        return first(atomically([input](T x){return input;}));
    }
}

//| Pass a variable between N consecutive threads X to X + N.
template<typename T /*< Variable type.*/, auto N /*< Number of threads.*/>
class pipelined_variable_array
{
public:
    //| Feed in a new value and push the old values down. This should typically be called exactly once per thread.
    inline T[N] next(T input)
    {
        T[N] previous_values;
        atomic
        {
            static T [N] _previous_val;
            static for (const auto i: N)
            {
                previous_values[i] = _previous_val[i];
                if(i == 0)
                {
                    _previous_val[i] = input;
                }
                else
                {
                    _previous_val[i] = previous_values[i - 1];
                }
            }
        }
        return(previous_values);
    }
}