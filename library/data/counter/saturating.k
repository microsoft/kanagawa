// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.counter.saturating
    { saturating_counter
    }

import numeric.int.limits
import data.order

//| Increment-only counter that will saturate rather than overflow.
template<auto Width>
class saturating_counter
{
public:
    using count_t=uint<Width>;

private:
    count_t _count = 0;

public:
    //| Increment the counter with an optional clear before increment.
    // Return the value _before_ the increment/clear. Returning the
    // value before clear is useful when offloading the value to some
    // external store, for example periodic sampling of performance counters.
    count_t increment(count_t inc_amount, bool clear_before_inc)
    {
        uint<Width+1> new_value;
        count_t result;

        atomic
        {
            result = _count;
            if (clear_before_inc)
            {
                new_value = inc_amount;
            }
            else
            {
                new_value = min(_count + inc_amount, limits<uint<Width>>::max);
            }

            _count = cast<count_t>(new_value);
        }

        return result;
    }

    //| Get the current value of the counter.
    inline count_t get()
    {
        return _count;
    }
}